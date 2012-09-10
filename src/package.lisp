(in-package :cl-user)

(desig-props:def-desig-package pr2-pick-and-place-scenario
 (:nicknames :pr2-pnp)
 (:use #:location-costmap #:cl #:common-lisp #:crs #:cpl
       #:roslisp #:cram-utilities
       #:cram-roslisp-common #:cram-plan-knowledge)
 (:desig-props #:grasp))
