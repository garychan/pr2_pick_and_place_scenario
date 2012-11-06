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

(defun fill-object-list ()
  (simple-knowledge::clear-object-list)
  (simple-knowledge::add-object-to-spawn
   :name "mug1"
   :type :mug
   :handles `((,(tf:make-pose
                 (tf:make-3d-vector 0.135 0 0.07)
                 (tf:euler->quaternion :ax (/ pi 2)))
               0.01))
   :collision-parts `((,(tf:make-pose
                         (tf:make-3d-vector 0.0 0.0 0.06)
                         (tf:make-identity-rotation))
                       :cylinder   ;; type
                       0.05        ;; radius
                       0.12))      ;; length
   :pose (tf:make-pose-stamped
          "map"
          0.0
          (tf:make-3d-vector -1.0 -1.2 0.6)
          (tf:euler->quaternion :az (/ pi 4)))
   :file (model-path "mug.urdf")
   :min-handles 1)
  (simple-knowledge::add-object-to-spawn
   :name "mug2"
   :type :mug
   :handles `((,(tf:make-pose
                 (tf:make-3d-vector 0.135 0 0.07)
                 (tf:euler->quaternion :ax (/ pi 2)))
               0.01))
   :collision-parts `((,(tf:make-pose
                         (tf:make-3d-vector 0.0 0.0 0.06)
                         (tf:make-identity-rotation))
                       :cylinder   ;; type
                       0.05        ;; radius
                       0.12))      ;; length
   :pose (tf:make-pose-stamped
          "map"
          0.0
          (tf:make-3d-vector -1.3 -1.1 0.6)
          (tf:euler->quaternion :az (/ pi 4)))
   :file (model-path "mug.urdf")
   :min-handles 1)
  (simple-knowledge::add-object-to-spawn
   :name "cooking_pot"
   :type :pot
   :handles `((,(tf:make-pose
                 (tf:make-3d-vector 0.23 0.0 0.12)
                 (tf:make-identity-rotation))
               0.01)
              (,(tf:make-pose
                 (tf:make-3d-vector -0.23 0.0 0.12)
                 (tf:euler->quaternion :az pi))
               0.01))
   :pose (tf:make-pose-stamped
          "map"
          0.0
          (tf:make-3d-vector -0.4 -1.5 0.6)
          (tf:euler->quaternion :az -0.4))
   :file (model-path "Cookingpot_2.urdf")
   :min-handles 2))
  ;; (simple-knowledge::add-object-to-spawn
  ;;  :name "green_bottle"
  ;;  :type :bottle
  ;;  :handles `((,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az 0.0))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi 4)))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi 2)))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (* pi 0.75)))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az pi))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (* pi -0.75)))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi -2)))
  ;;              0.08)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi -4)))
  ;;              0.08))
  ;;  :pose (tf:make-pose-stamped
  ;;         "map"
  ;;         0.0
  ;;         (tf:make-3d-vector 1.55 0.0 1.0)
  ;;         (tf:euler->quaternion :az 1.57))
  ;;  :file (model-path "Green_Bottle.urdf")))

  ;; (simple-knowledge::add-object-to-spawn
  ;;  :name "cooking_pot"
  ;;  :type :pot
  ;;  :handles `((,(tf:make-pose
  ;;                (tf:make-3d-vector 0.175 0.0 0.12)
  ;;                (tf:make-identity-rotation))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.175 0.0 0.12)
  ;;                (tf:euler->quaternion :az pi))
  ;;              0.01))
  ;;  :pose (tf:make-pose-stamped
  ;;         "map"
  ;;         0.0
  ;;         (tf:make-3d-vector -1.0 -1.46 0.6)
  ;;         (tf:make-identity-rotation))
  ;;  :file (model-path "Cookingpot_2.urdf"))
  ;; (simple-knowledge::add-object-to-spawn
  ;;  :name "iron"
  ;;  :type :iron
  ;;  :handles `((,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.06 0.14)
  ;;                (tf:euler->quaternion :ay (/ pi -2) :az pi))
  ;;              0.01))
  ;;  :pose (tf:make-pose-stamped
  ;;         "map"
  ;;         0.0
  ;;         (tf:make-3d-vector 1.8 0.2 1.0)
  ;;         (tf:euler->quaternion :az 1.57))
  ;;  :file (model-path "iron_2.urdf"))
  ;; (simple-knowledge::add-object-to-spawn
  ;;  :name "green_bottle"
  ;;  :type :bottle
  ;;  :handles `((,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az 0.0))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi 4)))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi 2)))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (* pi 0.75)))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az pi))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (* pi -0.75)))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi -2)))
  ;;              0.01)
  ;;             (,(tf:make-pose
  ;;                (tf:make-3d-vector 0.0 0.0 0.075)
  ;;                (tf:euler->quaternion :ax (/ pi 2) :ay 0.0 :az (/ pi -4)))
  ;;              0.01))
  ;;  :pose (tf:make-pose-stamped
  ;;         "map"
  ;;         0.0
  ;;         (tf:make-3d-vector 1.8 0.6 1.0)
  ;;         (tf:euler->quaternion :az 1.57))
  ;;  :file (model-path "Green_Bottle.urdf")))
