(defsystem pr2-pick-and-place-scenario
  :depends-on (cram-language
	       roslisp
               cram-plan-library
               cram-plan-knowledge
	       pr2-manipulation-process-module
               cram-reasoning
               location-costmap
               point-head-process-module
               pr2-navigation-process-module
               gazebo-perception-process-module
	       occupancy-grid-costmap
               perception-process-module)
  :components
  ((:module "src"
	    :components
	    ((:file "package")
             (:file "designator-config" :depends-on ("package"))
	     (:file "pr2-pnp" :depends-on ("package"))))))