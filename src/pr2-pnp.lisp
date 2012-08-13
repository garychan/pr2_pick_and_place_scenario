(in-package :pr2-pnp)

(def-top-level-plan pick-and-place-scenario (obj)
  (with-process-modules
    (cram-plan-knowledge:achieve `(cram-plan-knowledge:object-in-hand ,obj))))

(def-top-level-plan perceive-named-object (id)
  (with-process-modules
    (format t "Perceiving named object `~a'~%" id)
    (let ((obj (gazebo-perception-pm::make-named-object-designator id)))
      (cram-plan-library:perceive-object 'cram-plan-library:a obj))))

(defun generate-mug-designator ()
  (perception-pm::make-handled-object-designator
   :object-type :mug
   :object-pose (gazebo-perception-pm::get-model-pose "mug")
   :handles `((,(tf:make-pose (tf:make-3d-vector 0.13 0 0.06) (tf:euler->quaternion :ax (/ pi 2))) 0.01))))

;;(def-goal (achieve (handled-object-in-hand ?obj ?side))
  ;;(format t "Test~%"))
  ;; (with-designators ((grasp-action-desig
  ;;                     (cram-designators:action
  ;;                      `((desig-props:to desig-props:grasp)
  ;;                        (desig-props:obj ?obj)
  ;;                        (desig-props:side ?side)
  ;;                        (desig-props:type desig-props:trajectory)
  ;;                        (obstacles nil)))))
  ;;                   (cpm::pm-execute :manipulation grasp-action)))

(defmethod cram-plan-knowledge:holds ((occasion t) &optional time) nil)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       ((:manipulation pr2-manip-pm:pr2-manipulation-process-module)
        (:navigation pr2-navigation-process-module:pr2-navigation-process-module)
        (:perception gazebo-perception-pm:gazebo-perception-process-module)
;;        (:perception perception-pm:perception)
        (:ptu point-head-process-module:point-head-process-module))
     ,@body))