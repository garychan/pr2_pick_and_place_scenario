(in-package :pr2-pnp)

(def-top-level-plan pick-and-place-scenario (object-name)
  ;; NOTE(winkler): Function changed. Now, the scenario takes an object identifier as it's only parameter. Atm, this only works with "mug", since later on, a mug designator is generated. This has nothing to do with the pose of the object, but wuth the handles on the object (this must be defined per-object). To be moved into knowledge base later.
  ;; NOTE(winkler): When started, (pick-and-place-scenario "mug") successfully perceives the mug. But: UNKNOWN-PROCESS-MODULE comes up with varying process modules being missing. I suspect a race condition.
  (with-process-modules
    (let ((obj-desig (generate-mug-designator :pose-stamped (desig:designator-pose (perceive-named-object object-name)))))
      (format t "Perceived object: ~a~%" obj-desig)
      (achieve `(cram-plan-knowledge:object-in-hand ,obj-desig)))
    ))

(def-plan perceive-named-object (id)
  (with-process-modules
    (format t "Perceiving named object `~a'~%" id)
    (let ((obj (gazebo-perception-pm::make-named-object-designator id)))
      (cram-plan-library:perceive-object 'cram-plan-library:a obj))))

(defun generate-mug-designator (&key (pose-stamped (gazebo-perception-pm::get-model-pose "mug")))
  (perception-pm::make-handled-object-designator
   :object-type :mug
   :object-pose pose-stamped
   :handles `((,(tf:make-pose (tf:make-3d-vector 0.13 0 0.06) (tf:euler->quaternion :ax (/ pi 2))) 0.01))))

(defmethod cram-plan-knowledge:holds ((occasion t) &optional time) nil)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       ((:manipulation pr2-manip-pm:pr2-manipulation-process-module)
        (:navigation pr2-navigation-process-module:pr2-navigation-process-module)
        (:perception gazebo-perception-pm:gazebo-perception-process-module)
        (:ptu point-head-process-module:point-head-process-module))
     ,@body))