;; CFN has intrinsic functions and pseudo parameters -- they are closest to builtins in the language
(defconst cloudformation-intrinsic-functions-re "\\(\"\\(Fn::Base64\\|Fn::FindInMap\\|Fn::GetAtt\\|Fn::GetAZs\\|Fn::Join\\|Fn::Select\\|Ref\\)\"[ ]*:\\)")
(defconst cloudformation-pseudo-parameters-re "\\(\"\\(AWS::NotificationARNs\\|AWS::Region\\|AWS::StackId\\|AWS::StackName\\)\"\\)")

;; Most things will be quoted keys or strings.
(defconst cloudformation-quoted-key-re "\\(\"[^\"]+?\"[ ]*:\\)")
(defconst cloudformation-quoted-string-re "\\(\".*?\"\\)")

;; Regular, unquoted numbers
(defconst cloudformation-int-re "\\([^\"][0-9]+[^\"]\\)")

;; In CFN, sometimes numbers are quoted (Yeah, I don't understand that either)
;; Also they can be negative
(defconst cloudformation-quoted-int-re "\\(\"\\([0-9]+\\|\-[0-9]+\\)\"\\)")

;; It is not uncommon to see a few CIDR blocks
(defconst cloudformation-cidr-re "\\(\"[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\/[0-9]+\"\\)")

;; I don't think null exists in CFN, but it does in JSON
(defconst cloudformation-keyword-re "\\(true\\|false\\|null\\)")

;; Color up references
(defconst cloudformation-ref-re "\"Ref\"[ ]*:[ ]*\"\\([^\"]+\\)\"")

;; Color up AWS resource types
(defconst cloudformation-aws-type-re "\"Type\"[ ]*:[ ]*\\(\"AWS::[^\"]+?\"\\)")

(defconst cloudformation-font-lock-keywords-1
  (list
   (list cloudformation-intrinsic-functions-re 1 font-lock-reference-face)
   (list cloudformation-ref-re 1 font-lock-preprocessor-face)
   (list cloudformation-pseudo-parameters-re 1 font-lock-keyword-face)
   (list cloudformation-quoted-key-re 1 font-lock-keyword-face)
   (list cloudformation-quoted-int-re 1 font-lock-constant-face)
   (list cloudformation-cidr-re 1 font-lock-constant-face)
   (list cloudformation-aws-type-re 1 font-lock-type-face)
   (list cloudformation-quoted-string-re 1 font-lock-string-face)
   (list cloudformation-keyword-re 1 font-lock-constant-face)
   (list cloudformation-int-re 1 font-lock-constant-face)
   )
  "Level one font lock.")

(define-derived-mode cloudformation-mode js-mode "CloudFormation"
  (set (make-local-variable 'font-lock-defaults) '(cloudformation-font-lock-keywords-1 t)))


(provide 'cloudformation-mode)
