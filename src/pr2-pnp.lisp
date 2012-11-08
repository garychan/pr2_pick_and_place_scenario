;;; Copyright (c) 2012, Jan Winkler <winkler@cs.uni-bremen.de>
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of Willow Garage, Inc. nor the names of its
;;;       contributors may be used to endorse or promote products derived from
;;;       this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :pr2-pick-and-place-scenario)

(defun prepare-scenario ()
  (fill-object-list)
  (simple-knowledge::spawn-objects))

(defun reset ()
  (top-level
    (par
      (pr2-manip-pm::open-gripper :left :position 0.04)
      (pr2-manip-pm::open-gripper :right :position 0.04)))
  (simple-knowledge::reposition-objects)
  (setf simple-belief::*attached-objects* nil))

(defgeneric start-scenario (object-detail)
  (:documentation "Starts the PR2 pick and place scenario with either
  an object name or an object type symbol, defining what is to be
  picked up."))

(defmethod start-scenario :before (object-detail)
  "Prepares the scenario, i.e. loads the knowledge base and spawns
necessary objects in the Gazebo environment. Also, resets the belief
state concerning objects in grippers."
  ;; Prepare the scenario
  (prepare-scenario)
  ;; Clear the attached objects
  (setf simple-belief::*attached-objects* nil))

(defmethod start-scenario ((object-name string))
  "Starts the scenario and takes the unique object identifier as
reference to the Gazebo object instance. This will result in a
behaviour like 'get this instance and no other, fail otherwise'."
  ;; Create an object designator from the object name and call the
  ;; actual scenario plan
  (let ((object-desig (desig:make-designator
                       'desig:object
                       `((name ,object-name)))))
    (pick-and-place-scenario object-desig)))

(defmethod start-scenario (object-type)
  "Starts the scenario and takes the object category type as reference
to possibly multiple Gazebo object instances. This will result in a
behaviour like 'try all object instances of this type until either one
succeeds, fail otherwiese'."
  ;; Create an object designator from the object type and call the
  ;; actual scenario plan
  (let ((object-desig (desig:make-designator
                       'desig:object
                       `((type ,object-type)))))
    (pick-and-place-scenario object-desig)))

(def-top-level-cram-function pick-and-place-scenario (object-desig)
  (advertise-publishers)
  (with-process-modules
    ;; First, lift the spine. This way, we can access more parts of
    ;; the environment.
    (let ((spine-lift-trajectory (roslisp:make-msg
                                  "trajectory_msgs/JointTrajectory"
                                  (stamp header)
                                  (roslisp:ros-time)
                                  joint_names #("torso_lift_joint")
                                  points (vector
                                          (roslisp:make-message
                                           "trajectory_msgs/JointTrajectoryPoint"
                                           positions #(0.2)
                                           velocities #(0)
                                           accelerations #(0)
                                           time_from_start 5.0)))))
      (roslisp:ros-info (pick-and-place-scenario) "Moving up spine")
      (pr2-manip-pm::execute-torso-command spine-lift-trajectory)
      (roslisp:ros-info (pick-and-place-scenario) "Moving spine complete")
      ;; NOTE(winkler): We are retrieving all perceived objects here
      ;; that match the description given by `object-desig' and are
      ;; going through them until we grasped one of them
      ;; successfully. This is done with the help of failure handling
      ;; capabilities of CRAM (with-failure-handling). Basically, when
      ;; we say, we want one of the mugs by the name `mug1', there is
      ;; only one mug to get (since names are marking unique
      ;; instances). The loop will therefore terminate as soon as this
      ;; one either was grasped or the grasp failed. When we say, we
      ;; want one of the objects of type `mug', there are multiple
      ;; instances to try. Total failure is only signalled when no
      ;; instance could be grasped. This should reflect common sense.
      (cram-designators:with-designators
          ((put-down-location (location `((desig-props:on Cupboard)
                                          (desig-props:name Fronttable)))))
        (let* ((perceived-objects (cram-plan-library:perceive-object
                                   'cram-plan-library:all
                                   object-desig))
               (obj-in-hand nil)
               (former-obj-loc nil)
               (perceived-object nil))
          (cond ((not perceived-objects)
                 (ros-warn (pr2-pick-and-place-scenario)
                           "Found no objects of that description."))
                (t
                 (cram-language:with-failure-handling
                     ((cram-plan-failures:manipulation-pose-unreachable (f)
                        (declare (ignore f))
                        (roslisp:ros-warn
                         (pr2-pick-and-place-scenario)
                         "Failed to grasp object of that description.")
                        (setf perceived-objects (rest perceived-objects))
                        (when perceived-objects
                          (roslisp:ros-info (pr2-pick-and-place-scenario)
                                            "Trying the next.")
                          (retry))))
                   (setf perceived-object (first perceived-objects))
                   (setf former-obj-loc (desig-prop-value perceived-object 'at))
                   (setf obj-in-hand (cram-designators:current-desig
                                      (achieve
                                       `(cram-plan-knowledge:object-in-hand
                                         ,perceived-object)))))
                 (cond
                   (obj-in-hand
                    (roslisp:ros-info
                     (pr2-pick-and-place-scenario)
                     "Successfully grasped object. Now placing it.")
                    (let ((obj-placed (achieve
                                       `(cram-plan-knowledge:object-placed-at
                                         ,obj-in-hand
                                         ;,former-obj-loc
                                         ,put-down-location))))
                      (roslisp:ros-info
                       (pr2-pick-and-place-scenario)
                       "Designator of placed object: ~a~%" obj-placed)))
                   (t
                    (roslisp:ros-warn
                     (pr2-pick-and-place-scenario)
                     "No object of that description could be grasped."))))))))))