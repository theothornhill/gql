(in-package :gql)


(defgeneric validate (node &key &allow-other-keys)
  (:documentation "TODO"))

(defmacro defvalidator (node keys &body body)
  "Convenience macro to define new validator methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod validate ((node ,node) &key ,@keys &allow-other-keys)
     ,@body))

(defvalidator document ()
  (let ((definitions (definitions node)))
    (every-definition-executable-p definitions)
    (operation-name-unique-p definitions)
    (anonymous-operation-definition-p definitions)
    (subscription-operation-valid-p)
    (values *data* *errors*)))
