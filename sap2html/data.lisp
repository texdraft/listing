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

(defstruct line
  assembly
  remarksp
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

(defstruct token
  type
  data)

;; horrid hack for external symbols
(defvar *library* (list))
(defvar *undefined* (list))