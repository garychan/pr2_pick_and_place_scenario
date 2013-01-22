;;; Copyright (c) 2013, Andreas Stolpmann <andisto@cs.uni-bremen.de>
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
;;;     * Neither the name of Universitaet Bremen nor the names of its
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

(defun get-current-spine-pos ()
  (roslisp:with-fields (name position) (pr2-manip-pm::get-robot-state)
    (let 
     ((idx (position "torso_lift_joint" name :test #'equal)))
      (unless idx (error 'simple-error :format-control "Invalid robot state. Couldn't find torso lift joint"))
      (elt position idx))))

(defun get-spine-lift-trajectory (position)   
  (roslisp:make-msg
   "trajectory_msgs/JointTrajectory"
   (stamp header)
   (roslisp:ros-time)
   joint_names #("torso_lift_joint")
   points (vector
           (roslisp:make-message
            "trajectory_msgs/JointTrajectoryPoint"
            positions (vector position)
            velocities #(0)
            accelerations #(0)
            time_from_start 0.1))))

(defun spine-position-reached (position)
  (< (abs (- (get-current-spine-pos) position)) 0.001))

(defun move-spine-to (position)
  (roslisp:ros-info (pick-and-place-scenario) "Moving spine")
  (loop
    (pr2-manip-pm::execute-torso-command (get-spine-lift-trajectory position))
    (when (or (spine-position-reached position)
              (or  (and (<= position 0.013)
                        (<= (get-current-spine-pos) 0.013)) ;Spine can't reach 0
                   (and (>= position 0.32) ;Maximum is ~32.5
                        (>= (get-current-spine-pos) 0.032))))
      (roslisp:ros-info (pick-and-place-scenario) "Moving spine complete") 
      (return))))