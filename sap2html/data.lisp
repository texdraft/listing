(defstruct assembly
  (symbols (make-hash-table :test #'equal))
  (head #\Space)
  index
  lines
  base-name
  title
  introducer
  namer
  output-directory
  output-name
  previous
  next)

(defstruct sap-symbol
  name
  home
  pretty-name
  link-name
  value
  remarks
  (type nil :type (member :equ
                          :syn
                          :oct
                          :dec
                          :address
                          :bcd
                          :bss
                          :bes
                          nil))
  text
  definition
  references)

(defstruct operation
  (mnemonic nil :type string)
  pseudop
  (help nil :type string))

(defstruct (line (:print-object))
  assembly
  remarksp
  generatedp
  flags
  address
  assembled
  number
  location
  operation
  indirectp
  variable
  variable-length
  remarks
  sequence)

(defmethod print-object ((object line) stream)
  (format stream "#<line #~A~%  ~
                         remarks? ~A~%  ~
                         generated? ~A~%  ~
                         flags ~W~%  ~
                         address ~W~%  ~
                         location ~W~%  ~
                         operation ~A~%  ~
                         variable ~W length ~A~%  ~
                         remarks ~A~%  ~
                         sequence ~W>"
                  (line-number object)
                  (line-remarksp object)
                  (line-generatedp object)
                  (line-flags object)
                  (line-address object)
                  (line-location object)
                  (line-operation object)
                  (line-variable object)
                  (line-variable-length object)
                  (line-remarks object)
                  (line-sequence object)))

(defstruct token
  type
  data)

;; horrid hack for external symbols
(defvar *library* (list))
(defvar *undefined* (list))