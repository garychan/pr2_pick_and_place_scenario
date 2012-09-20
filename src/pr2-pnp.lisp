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

(in-package :pr2-pnp)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (pr2-manip-pm:pr2-manipulation-process-module
        pr2-navigation-process-module:pr2-navigation-process-module
        gazebo-perception-pm:gazebo-perception-process-module
        point-head-process-module:point-head-process-module)
     ,@body))

(def-top-level-plan pick-and-place-scenario (object-name)
  ;; NOTE(winkler): Function changed. Now, the scenario takes an object identifier as it's only parameter. Atm, this only works with "mug", since later on, a mug designator is generated. This has nothing to do with the pose of the object, but with the handles on the object (this must be defined per-object). To be moved into knowledge base later.
  (setf cram-plan-library::*pose-publisher* (roslisp:advertise "/foo" "geometry_msgs/PoseStamped" :latch t))
  (with-process-modules
    (let* ((perceived-object (perceive-named-object object-name))
           (object-to-grab (gazebo-perception-process-module::make-handled-object-designator
                            :name object-name
                            :object-type :mug
                            :object-pose (desig:reference (desig-prop-value perceived-object 'at))
                            :handles `((,(tf:make-pose (tf:make-3d-vector 0.13 0 0.06) (tf:euler->quaternion :ax (/ pi 2))) 0.01)))))
        (achieve `(cram-plan-knowledge:object-in-hand ,object-to-grab)))))

(def-plan perceive-named-object (object-name)
  (with-designators ((mug (object `((desig-props:name ,object-name)))))
    (cram-plan-library:perceive-object 'cram-plan-library:a mug)))

(defun generate-mug-designator (&key (pose-stamped (gazebo-perception-pm::get-model-pose "mug")) name)
  (gazebo-perception-pm::make-handled-object-designator
   :name name
   :object-type :mug
   :object-pose pose-stamped
   :handles `((,(tf:make-pose (tf:make-3d-vector 0.13 0 0.06) (tf:euler->quaternion :ax (/ pi 2))) 0.01))))

;(defmethod cram-plan-knowledge:holds ((occasion t) &optional time)
;  (declare (ignore time))
;  nil)

(defun get-latest-exectrace ()
  (cet:with-episode-knowledge cet:*last-episode-knowledge*
    (cram-utilities:force-ll (crs:prolog `(and (task ?task))))))