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

(in-package :location-costmap)

(crs:def-fact-group costmap-metadata ()
  (crs:<- (costmap-size 25 25))
  (crs:<- (costmap-origin -12.5 -12.5))
  (crs:<- (costmap-resolution 0.05))

  (crs:<- (costmap-padding 0.35))
  (crs:<- (costmap-manipulation-padding 0.35))
  (crs:<- (costmap-in-reach-distance 1.0))
  (crs:<- (costmap-reach-minimal-distance 0.2)))

(crs:def-fact-group process-modules (matching-process-module available-process-module)

  (crs:<- (matching-process-module ?designator gazebo-perception-process-module)
    (desig-prop ?designator (to perceive))))

;;(def-fact-group location-costmap-desigs (desig-costmap)

  ;; (crs:<- (desig-costmap ?desig ?cm)
  ;;   (desig-prop ?desig (to grasp))
  ;;   (costmap ?cm)
  ;;   (lisp-fun 2d-pose-covariance ?poses 0.5 (?mean ?covariance))
  ;;   (costmap-add-function pose-distribution (make-gauss-cost-function ?mean ?covariance) ?cm))

  ;; (crs:<- (merged-desig-costmap ?desig ?cm)
  ;;   ;; bagof collects all true solutions for c into costmaps
  ;;   (bagof ?c (desig-costmap ?desig ?c) ?costmaps)
  ;;   (lisp-fun merge-costmaps ?costmaps ?cm)))


  ;; (crs:<- (matching-process-module ?designator pr2-manipulation-process-module)
  ;;   (trajectory-desig? ?designator)
  ;;   (not
  ;;    (or (desig-prop ?designator (to see))
  ;;        (desig-prop ?designator (to follow))))))

  ;; (<- (matching-process-module ?designator projection-manipulation)
  ;;   (trajectory-desig? ?designator)
  ;;   (not
  ;;    (or (desig-prop ?designator (to see))
  ;;        (desig-prop ?designator (to follow)))))
  
  ;; (<- (matching-process-module ?designator projection-navigation)
  ;;   (desig-prop ?designator (type navigation)))

  ;; (<- (available-process-module projection-ptu)
  ;;   (symbol-value *projection-environment* pr2-bullet-projection-environment))

  ;; (<- (available-process-module projection-perception)
  ;;   (symbol-value *projection-environment* pr2-bullet-projection-environment))

  ;; (<- (available-process-module projection-manipulation)
  ;;   (symbol-value *projection-environment* pr2-bullet-projection-environment))

  ;; (<- (available-process-module projection-navigation)
  ;;   (symbol-value *projection-environment* pr2-bullet-projection-environment)))
