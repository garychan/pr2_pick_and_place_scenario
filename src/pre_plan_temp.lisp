;; TODO(winkler): Add lists for knowledge-base and semantic-map and
;; add code to `load-semantic-map' and `load-object-knowledge' in
;; order to put objects and locations into these lists. Also, add code
;; to `choose-random-object-from-knowledge-base' and
;; `choose-random-putdown-location' in order to select random elements
;; from those lists and return them. These objects should be object
;; designators and location designators, respectively.

;; NOTE(winkler): The function calls here have not been executed
;; yet. This is a rudimentary, `pseudo-like' implementation. It is
;; intended to serve as a conceptual example for developing the
;; `pr2-pick-and-place-scenario'. It is structured in a very general
;; way to allow it to serve as a runnable implementation, later
;; on. Extend it at will.

(in-package :pr2-pnp)

(defvar *knowledge-base*)
(defvar *semantic-map*)

(def-top-level-plan pnp-scenario ()
  "This is the entry point for the pick and place scenario."
  (with-process-modules
   (load-semantic-map)
   (load-object-knowledge)
   (dotimes (i 10)
     (format t "Executing pnp-action no. ~a~%" i)
     (pnp-action)
     (save-latest-execution-trace))))

(def-plan pnp-action ()
  "This plan actually executes the pick and place actions. Meaning, it
chooses an object to look for and pick up as well as choosing a
put-down location and placing the object there."
  (let ((object-to-pick (choose-random-object-from-knowledge-base))
        (putdown-location (choose-random-putdown-location)))
    (achieve `(object-in-hand ,object-to-pick))
    (achieve `(object-at-location ,object-to-pick ,putdown-location))))

(defun load-semantic-map ()
  "Loads a semantic map for the environment. This should also include
  5-10 putdown locations (depending on the number of objects we use
  for the scenario). This way, we have a more `lightweight' version of
  the location finding and don'r run into location determination
  problems. Use the global variable *semantic-map* for that. This
  could be extended later on."
  (setf *semantic-map* nil))

(defun load-object-knowledge ()
  "For starters, we should load a list of maybe 5 objects that are
  picked and placed all over the semantic map. When done like that,
  this mechanism provides us with a simple `fake' knowledge base and
  let's us develop and test all the other components. Use the global
  variable *knowledge-base* for that. Later on, a connection to
  knowrob would be pretty cool."
  ;; TODO(winkler): The objects loaded here should be object
  ;; designators of handled objects. These handled objects should
  ;; reflect the objects we have in simulation (and later on, even the
  ;; ones we have in the real environment).
  (setf *knowledge-base* nil))

(defun save-latest-execution-trace ()
  "This one is simple, I just didn't copy the code over."
  )

(defun choose-random-object-from-knowledge-base ()
  "Pick a random location from the simple object list from the `fake'
  knowledge base. Use the global variable *knowledge-base* for that."
  nil)

(defun choose-random-putdown-location ()
  "From the list of possible put-down locations in the simple
  case (just a list of location designators in the semantic map), just
  pick a random element. Use the global variable *semantic-map* for
  that."
  nil)