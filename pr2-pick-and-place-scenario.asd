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

(defsystem pr2-pick-and-place-scenario
  :author "Jan Winkler <winkler@cs.uni-bremen.de>"
  :license "BSD"
  :description "PR2 Pick and Place Scenario"

  :depends-on (cram-language
               roslisp
               cram-pr2-knowledge
               pr2-manipulation-knowledge
               pr2-reachability-costmap
               cram-plan-library
               cram-plan-knowledge
               pr2-manipulation-process-module
               cram-reasoning
               location-costmap
               point-head-process-module
               pr2-navigation-process-module
               gazebo-perception-process-module
               occupancy-grid-costmap
               simple-belief
               simple-knowledge
               physics-utils
               cram-gazebo-utilities
               semantic-map-costmap)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "designator-config" :depends-on ("package"))
     (:file "facts" :depends-on ("package"))
     (:file "utils" :depends-on ("package" "facts"))
     (:file "object-database" :depends-on ("package" "utils" "facts"))
     (:file "pr2-pnp"
      :depends-on ("package"
                   "designator-config"
                   "object-database"
                   "utils"
                   "facts"))))))
