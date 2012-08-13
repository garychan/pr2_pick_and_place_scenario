(desig-props:def-desig-package
 pr2-pick-and-place-scenario
 (:nicknames :pr2-pnp)
 (:use
  #:location-costmap
;;  #:cram-reasoning
  #:cpl
  #:roslisp
  #:pr2-manipulation-process-module
;;   #:cram-designators
  #:cram-utilities
;;   #:cram-process-modules
  #:cram-roslisp-common
;;  #:cram-plan-knowledge
;;   #:cram-plan-failures
  ))
;;  #:alexandria))
;; (:export #:handled-object-in-hand))