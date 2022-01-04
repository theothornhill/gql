(in-package #:gql-tests)

;; These are types which correspond to the schema.
;; They represent the shape of the data visited during field resolution.

(defclass swcharacter (gql-object)
  ;; type-name covered in gql-object
  ((id :initarg :id :accessor id)
   (name :initarg :name :accessor name)
   (friends :initarg :friends :accessor friends)
   (appears-in :initarg :appears-in :accessor appears-in)))

(defclass human (swcharacter)
  ((home-planet :initarg :home-planet :accessor home-planet)))

(defclass droid (swcharacter)
  ((primary-function :initarg :primary-function :accessor primary-function)))

(defparameter *episode-enum*
  (enum
   :name "Episode"
   :description "One of the films in the Star Wars Trilogy"
   :enum-values `(,(enum-val :enum-value (make-name "NEWHOPE"))
                  ,(enum-val :enum-value (make-name "EMPIRE"))
                  ,(enum-val :enum-value (make-name "JEDI")))))

(defparameter *character-interface*
  (interface
   :name "Character"
   :description "A character in the Star Wars Trilogy"
   :fields `(,(field
               :name "id"
               :type (! *string*)
               :description "The id of the character.")
             ,(field
               :name "name"
               :type *string*
               :description "The name of the character.")
             ,(field
               :name "friends"
               :type ([] (named "Character"))
               :description "The friends of the character, or an empty list if they have none.")
             ,(field
               :name "appearsIn"
               :type ([] (named "Episode"))
               :description "Which movies they appear in."))))

(defparameter *human-type*
  (object
   :name "Human"
   :description "A humanoid creature in the Star Wars universe."
   :fields `(,(field
               :name "id"
               :type (! *string*)
               :description "The id of the human."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (id object-value))))
             ,(field
               :name "name"
               :type *string*
               :description "The name of the human."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (name object-value))))
             ,(field
               :name "friends"
               :type ([] (named "Character"))
               :description "The friends of the human, or an empty list if they have none."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (get-friends object-value))))
             ,(field
               :name "appearsIn"
               :type ([] (named "Episode"))
               :description "Which movies they appear in."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (appears-in object-value))))
             ,(field
               :name "homePlanet"
               :type *string*
               :description "Which movies they appear in."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (home-planet object-value)))))
   :interfaces (list *character-interface*)))

(defparameter *droid-type*
  (object
   :name "Droid"
   :description "A mechanical creature in the Star Wars universe."
   :fields `(,(field
               :name "id"
               :type (! *string*)
               :description "The id of the droid."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (id object-value))))
             ,(field
               :name "name"
               :type *string*
               :description "The name of the droid."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (name object-value))))
             ,(field
               :name "friends"
               :type ([] (named "Character"))
               :description "The friends of the droid, or an empty list if they have none."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (get-friends object-value))))
             ,(field
               :name "appearsIn"
               :type ([] (named "Episode"))
               :description "Which movies they appear in."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (appears-in object-value))))
             ,(field
               :name "primaryFunction"
               :type *string*
               :description "The primary function of the droid."
               :resolver (lambda ()
                           (with-slots (object-value) (execution-context *context*)
                             (primary-function object-value)))))
   :interfaces (list *character-interface*)))

(defparameter *query*
  (object
   :name "Query"
   :fields `(,(field
               :name "hero"
               :type (named "Character")
               :args `(,(arg
                         :name "episode"
                         :description "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode."
                         :type (named "Episode")))
               :resolver (lambda ()
                           (with-slots (arg-values) (execution-context *context*)
                             (get-hero (gethash "episode" arg-values)))))
             ,(field
               :name "human"
               :type (named "Human")
               :args `(,(arg
                         :name "id"
                         :description "id of the human"
                         :type (! *string*)))
               :resolver (lambda ()
                           (with-slots (arg-values) (execution-context *context*)
                             (get-human (gethash "id" arg-values)))))
             ,(field
               :name "droid"
               :type (named "Droid")
               :args `(,(arg
                         :name "id"
                         :description "id of the droid"
                         :type (! *string*)))
               :resolver (lambda ()
                           (with-slots (arg-values) (execution-context *context*)
                             (get-droid (gethash "id" arg-values))))))))

(defparameter *schema*
  (make-schema :query *query* :types (list *character-interface*
                                           *human-type*
                                           *droid-type*
                                           *episode-enum*)))

(defmacro comment (&body body)
  (declare (ignore body)))

(comment
  (with-context (:schema *schema*
                 :document (build-document
                            "query {
                               hero(episode: \"NEWHOPE\") {
                                 id
                                 primaryFunction
                                 friends {
                                   id
                                   name
                                 }
                               }
                               human(id: \"1002\") {
                                 id
                                 friends {
                                   name
                                 }
                               }
                               droid(id: \"2000\") {
                                 id
                                 friends {
                                   name
                                   friends {
                                     name
                                   }
                                 }
                               }
                             }"))
    (let* ((res (gql::execute)))
      (format t "~%~a" (cl-json:encode-json-to-string res)))))


;; This defines a basic set of data for our Star Wars Schema.
;;
;; This data is hard coded for the sake of the demo, but you could imagine
;; fetching this data from a backend service rather than from hardcoded
;; JSON objects in a more complex demo.
(defvar *luke*
  (make-instance
   'human
   :type-name "Human"
   :id "1000"
   :name "Luke Skywalker"
   :friends '("1002" "1003" "2000" "2001")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")
   :home-planet "Tatooine"))

(defvar *vader*
  (make-instance
   'human
   :type-name "Human"
   :id "1001"
   :name "Darth Vader"
   :friends '("1004")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")
   :home-planet "Tatooine"))

(defvar *han*
  (make-instance
   'human
   :type-name "Human"
   :id "1002"
   :name "Han Solo"
   :friends '("1000" "1003" "2001")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")))

(defvar *leia*
  (make-instance
   'human
   :type-name "Human"
   :id "1003"
   :name "Leia Organa"
   :friends '("1004")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")
   :home-planet "Tatooine"))

(defvar *tarkin*
  (make-instance
   'human
   :type-name "Human"
   :id "1004"
   :name "Wilhuff Tarkin"
   :friends '("1001")
   :appears-in '("NEWHOPE")))

(defvar *human-data*
  (let ((data (make-hash-table :test #'equal)))
    (setf (gethash (id *luke*)   data) *luke*)
    (setf (gethash (id *vader*)  data) *vader*)
    (setf (gethash (id *han*)    data) *han*)
    (setf (gethash (id *leia*)   data) *leia*)
    (setf (gethash (id *tarkin*) data) *tarkin*)
    data))

(defvar *threepio*
  (make-instance
   'droid
   :type-name "Droid"
   :id "2000"
   :name "C-3PO"
   :friends '("1000" "1002" "1003" "2001")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")
   :primary-function "Protocol"))

(defvar *artoo*
  (make-instance
   'droid
   :type-name "Droid"
   :id "2001"
   :name "R2-D2"
   :friends '("1000" "1002" "1003" "2001")
   :appears-in '("NEWHOPE" "EMPIRE" "JEDI")
   :primary-function "Astromech"))

(defvar *droid-data*
  (let ((data (make-hash-table :test #'equal)))
    (setf (gethash (id *threepio*) data) *threepio*)
    (setf (gethash (id *artoo*)    data) *artoo*)
    data))

(defun get-character (id)
  (or (gethash id *human-data*)
      (gethash id *droid-data*)))

(defun get-friends (character)
  (check-type character swcharacter)
  (mapcar 'get-character (friends character)))

(defun get-hero (episode)
  (if (string= episode "EMPIRE") *luke* *artoo*))

(defun get-human (id)
  (gethash id *human-data*))

(defun get-droid (id)
  (gethash id *droid-data*))
