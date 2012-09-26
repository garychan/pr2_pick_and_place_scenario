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

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (pr2-manip-pm:pr2-manipulation-process-module
        pr2-navigation-process-module:pr2-navigation-process-module
        gazebo-perception-pm:gazebo-perception-process-module
        point-head-process-module:point-head-process-module)
     ,@body))

(defun prepare-scenario ()
  (fill-object-list)
  (simple-knowledge::spawn-objects))

(def-top-level-plan pick-and-place-scenario (object-name)
  ;; NOTE(winkler): This initializes a pose publisher used for
  ;; debugging.
  (setf cram-plan-library::*pose-publisher*
        (roslisp:advertise "/foo"
                           "geometry_msgs/PoseStamped"
                           :latch t))
  (with-process-modules
    ;; First, lift the spine. This way, we can access more parts of
    ;; the environment.
    (let ((spine-lift-trajectory (roslisp:make-msg
                                  "trajectory_msgs/JointTrajectory"
                                  (stamp header)
                                  (roslisp:ros-time)
                                  joint_names #("torso_lift_joint")
                                  points (vector (roslisp:make-message
                                                  "trajectory_msgs/JointTrajectoryPoint"
                                                  positions #(0.2)
                                                  velocities #(0)
                                                  accelerations #(0)
                                                  time_from_start 10.0)))))
      (pr2-manip-pm::execute-torso-command spine-lift-trajectory)
      (let* ((perceived-object (perceive-named-object object-name)))
        (achieve `(cram-plan-knowledge:object-in-hand ,perceived-object))))))

(def-plan perceive-named-object (object-name)
  (with-designators ((obj-desig (object `((desig-props:name ,object-name)))))
    (cram-plan-library:perceive-object 'cram-plan-library:a obj-desig)))
