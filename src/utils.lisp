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

(defvar *pose-publisher* nil)

(defun model-path (name)
  (physics-utils:parse-uri
   (concatenate
    'string
    "package://pr2_pick_and_place_scenario/models/"
    name)))

(defun register-publishers (&key (pose-topic "/pr2pnp_pose_publisher"))
  (setf *pose-publisher* (roslisp:advertise
                          pose-topic
                          "geometry_msgs/PoseStamped")))

(defun pub-pose (pose-stamped)
  (roslisp:publish *pose-publisher*
                   (tf:pose-stamped->msg pose-stamped)))

(defun get-latest-exectrace ()
  (cet:with-episode-knowledge cet:*last-episode-knowledge*
    (cram-utilities:force-ll (crs:prolog `(and (task ?task))))))

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (pr2-manip-pm:pr2-manipulation-process-module
        pr2-navigation-process-module:pr2-navigation-process-module
        gazebo-perception-pm:gazebo-perception-process-module
        point-head-process-module:point-head-process-module)
     ,@body))

(defun get-object-instance (name)
  (top-level
    (with-process-modules
      (cram-plan-library:perceive-object
       'cram-plan-library:a
       (cram-designators:make-designator 'object
                                         `((name ,name)))))))

(defun get-object-type-instances (type)
  (top-level
    (with-process-modules
      (cram-plan-library:perceive-object
       'cram-plan-library:all
       (cram-designators:make-designator 'object
                                         `((type ,type)))))))
