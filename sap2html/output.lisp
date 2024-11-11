(defstruct html
  output
  assembly
  tags)

(defun html-preamble (output title body-class assembly)
  (format output "<!DOCTYPE html>~%~
                  <html>~%~
                    <head>~%~
                      <title>~A</title>~
                      <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">~
                      <script src=\"code.js\" defer></script>
                    </head>~%~
                    <body class=\"~A\">"
                 title
                 body-class))

(defun html-postamble (h)
  (write-string "</body></html>" (html-output h)))

(defun begin-element (h tag &rest attributes)
  (format (html-output h) "<~(~A~)" tag)
  (push tag (html-tags h))
  (loop for (name value) on attributes by #'cddr do
    (add-attribute h name value))
  (write-string ">" (html-output h)))

(defun add-attribute (h name text)
  (format (html-output h) " ~(~A~)=\"~A\"" name text))

(defun end-element (h &optional tag)
  (when tag
    (assert (eq (first (html-tags h)) tag)))
  (format (html-output h) "</~(~A~)>" (pop (html-tags h))))

(defun format-html (h &rest arguments)
  (apply #'format (html-output h) arguments))

(defun html-line (line h externalp)
 ;; externalp = t when line links go to xxx.html#line- rather than to #line-
 (let* ((id (format nil "line-~D" (line-number line)))
        (link (if externalp
                  (format nil "~A.html#~A" (assembly-base-name (line-assembly line))
                                           id)
                  (format nil "#~A" id))))
    (cond ((line-remarksp line)
           (if externalp
               (begin-element h :tr :class "remarks-line")
               (begin-element h :tr :class "remarks-line" :id id))
           (begin-element h :td :colspan "3")
           (end-element h :td)
           (begin-element h :td :class "number")
           (begin-element h :a :href link)
           (format-html h "~D" (line-number line))
           (end-element h :a)
           (end-element h :td)
           (begin-element h :td :colspan "2")
           (end-element h :td)
           (begin-element h :td)
           (format-html h "~A" (line-remarks line))
           (end-element h :td)
           (begin-element h :td :class "sequence")
           (format-html h "~A" (line-sequence line))
           (end-element h :td)
           (end-element h :tr))
          (t
           (cond (externalp
                  (begin-element h :tr))
                 (t
                  (begin-element h :tr :id id)))
           (output-flags line h)
           (begin-element h :td :class "address"
                                :title (format nil "= ~D₁₀" (line-address line)))
           (when (line-address line)
             (format-html h "~5,'0O" (line-address line)))
           (end-element h :td)
           (output-assembled line h)
           (begin-element h :td :class "number")
           (begin-element h :a :href link)
           (format-html h "~D" (line-number line))
           (end-element h :a)
           (end-element h :td)
           (output-location line h)
           (output-operation line h)
           (output-variable line h)
           (begin-element h :td :class "sequence")
           (format-html h "~A" (line-sequence line))
           (end-element h :td)
           (end-element h :tr))))
  (terpri (html-output h)))

(defun output-flags (line h)
  (begin-element h :td :class "flags")
  (loop for c across (line-flags line) do
    (unless (char= c #\Space)
      (begin-element h :abbr :title (ecase c
                                     ((#\T)
                                      "Missing tag field")
                                     ((#\A)
                                      "Missing address field")
                                     ((#\D)
                                      "Missing decrement field")
                                     ((#\M)
                                      "Multiply-defined symbol")
                                     ((#\U)
                                      "Undefined symbol")))
      (format-html h "~C" c)
      (end-element h :abbr)))
  (end-element h))

(defun output-assembled (line h)
  (begin-element h :td :class "assembled")
  (let ((count 0))
    (loop for c across (line-assembled line) do
      (if (char= c #\-)
          (format-html h "&minus;")
          (format-html h "~C" c))
      (incf count))
    (loop repeat (- 20 count) do (format-html h " ")))
  (end-element h :td))

(defun output-location (line h)
  (begin-element h :td :class "location")
  (when (line-location line)
    (let* ((location (line-location line))
           (symbol (car location))
           (text (cdr location)))
      (cond ((eq symbol :hed)
             (format-html h "~6A" text))
            (t
             (begin-element h :a :href (format nil "~A-symbols.html#~A"
                                                   (assembly-base-name (html-assembly h))
                                                   (sap-symbol-link-name symbol))
                                 :id (format nil "~A-definition" (sap-symbol-link-name symbol))
                                 :title (symbol-title symbol t))
             (output-symbol symbol text h)
             (loop repeat (- 6 (length text)) do
               (format-html h " "))
             (end-element h)))))
  (end-element h))

(defun output-symbol (symbol text h)
  (format-html h "~A" text))

(defun output-operation (line h)
  (begin-element h :td :class "operation")
  (when (line-operation line)
    (let ((operation (line-operation line)))
      (begin-element h :span :class "mnemonic" :title (operation-help operation))
      (format-html h "~A" (operation-mnemonic operation))
      (end-element h))
    (when (line-indirectp line)
      (begin-element h :span :class "indirect" :title "(indirect addressing)")
      (format-html h "*")
      (end-element h)))
  (end-element h))

(defun variable-text (line)
  (with-output-to-string (s)
    (loop for token in (line-variable line) do
      (case (token-type token)
        ((:bcd)
         (loop for c across (cdr (token-data token)) do
           (if (char= c #\Space)
               (write-string "␣" s)
               (format s "~C" c))))
        ((:symbol)
         (format s "~A" (cdr (token-data token))))
        ((:number)
         (format s "~A" (token-data token)))
        ((:comma)
         (write-string "," s))
        ((:plus)
         (write-string "+" s))
        ((:minus)
         (write-string "&minus;" s))
        ((:slash)
         (write-string "/" s))
        ((:asterisk)
         (write-string "*" s))))))

(defun symbol-title (symbol definingp)
  (let ((meaning (string-left-trim '(#\Space) (title-text symbol)))
        (remarks (string-trim '(#\Space) (line-remarks (sap-symbol-definition symbol)))))
    (if (and (not definingp) (> (length remarks) 0))
        (concatenate 'string meaning " | " remarks)
        meaning)))

(defun title-text (symbol)
  (case (sap-symbol-type symbol)
    ((:equ :syn :oct)
     (format nil "~A = ~A = ~O₈ = ~:*~D₁₀" (sap-symbol-type symbol)
                                          (variable-text (sap-symbol-definition symbol))
                                          (sap-symbol-value symbol)))
    ((:dec)
     (format nil "DEC = ~A = ~:*~D₁₀" (variable-text (sap-symbol-definition symbol))
                                     (sap-symbol-value symbol)))
    ((:address)
     (format nil "= ~O₈ = ~:*~D₁₀" (sap-symbol-value symbol)))
    ((:bcd)
     (format nil "BCD ~A = ~O₈ = ~:*~D₁₀" (variable-text (sap-symbol-definition symbol))
                                          (sap-symbol-value symbol)))
    ((:bss :bes)
     (format nil "~A ~A = ~O₈ = ~:*~D₁₀" (sap-symbol-type symbol)
                                         (variable-text (sap-symbol-definition symbol))
                                         (sap-symbol-value symbol)))
    (otherwise
     (format nil "~A = ~O₈ = ~:*~D₁₀" (variable-text (sap-symbol-definition symbol))
                                      (sap-symbol-value symbol)))))

(defun output-variable (line h)
  (begin-element h :td :class "variable-remarks")
  (when (line-variable line)
    (let ((count 0))
      (loop for token in (line-variable line) do
        (case (token-type token)
          ((:bcd)
           (begin-element h :span :class "bcd-count")
           (format-html h "~A" (let ((count (car (token-data token))))
                                 (if (= count 10)
                                     " "
                                     count)))
           (incf count)
           (end-element h)
           (loop for c across (cdr (token-data token)) do
             (if (char= c #\Space)
                 (format-html h "␣")
                 (format-html h "~C" c))
             (incf count)))
          ((:bci)
           (format-html h "~D," (car (token-data token)))
           (loop for c across (cdr (token-data token)) do
             (if (char= c #\Space)
                 (format-html h "␣")
                 (format-html h "~C" c))))
          ((:symbol)
           (begin-element h :span :class "symbol")
           (let ((symbol (car (token-data token))))
             (cond (symbol
                    (begin-element h :a
                                     :href (format nil "~A#~A-definition"
                                                       (if (eq (sap-symbol-home symbol) (line-assembly line))
                                                           ""
                                                           (concatenate 'string
                                                                        (assembly-base-name (sap-symbol-home symbol))
                                                                        ".html"))
                                                       (sap-symbol-link-name symbol))
                                     :title (symbol-title symbol nil)))
                   (t
                    (format t "~A undefined~%" (cdr (token-data token)))
                    (pushnew (cdr (token-data token)) *undefined*
                             :test #'string=))))
           (format-html h "~A" (cdr (token-data token)))
           (incf count (length (cdr (token-data token))))
           (when (car (token-data token))
             (end-element h))
           (end-element h))
          ((:text)
           (incf count (length (token-data token)))
           (format-html h "~A" (token-data token)))
          ((:number)
           (incf count (length (token-data token)))
           (format-html h "~A" (token-data token)))
          ((:comma)
           (incf count)
           (format-html h ","))
          ((:plus)
           (incf count)
           (format-html h "+"))
          ((:minus)
           (incf count)
           (format-html h "&minus;"))
          ((:slash)
           (incf count)
           (format-html h "/"))
          ((:asterisk)
           (incf count)
           (format-html h "*"))))
      (loop repeat (- (line-variable-length line) count) do
        (format-html h " "))))
  (if (line-remarks line)
      (format-html h "~V@A" (- 60 (line-variable-length line)) (line-remarks line))
      (when (< (line-variable-length line))
        (loop repeat (- 60 (line-variable-length line)) do
          (format-html h " "))))
  (end-element h))

(defun output-navigation (h
                          endp
                          previous-text
                          previous-title
                          previous
                          up-text
                          up-title
                          up
                          next-text
                          next-title
                          next
                          auxiliary-text
                          auxiliary-title
                          auxiliary)
  (labels ((navigation (test text title link)
             (begin-element h :div :class "navigation-button")
             (when test
               (begin-element h :a :href link :title title)
               (format-html h text)
               (end-element h))
             (end-element h))
           (auxiliary ()
             (begin-element h :div :class "auxiliary-navigation")
             (begin-element h :div :class "navigation-button")
             (begin-element h :a :href auxiliary :title auxiliary-title)
             (format-html h auxiliary-text)
             (end-element h)
             (end-element h)
             (end-element h)))
    (let ((assembly (html-assembly h)))
      (begin-element h :nav :class "main-navigation")
      (when endp
        (auxiliary))
      (navigation (assembly-previous assembly) previous-text previous-title previous)
      (navigation t up-text up-title up)
      (navigation (assembly-next assembly) next-text next-title next)
      (unless endp
        (auxiliary))
      (end-element h))))

(defun main-navigation (h endp)
  (let ((assembly (html-assembly h)))
    (output-navigation h endp
                         "Previous"
                         "Go to previous assembly"
                         (when (assembly-previous assembly)
                           (format nil "~A.html" (assembly-base-name (assembly-previous assembly))))
                         "Home"
                         "Return to landing page"
                         (format nil "~A.html" (assembly-output-name assembly))
                         "Next"
                         "Go to next assembly"
                         (when (assembly-next assembly)
                           (format nil "~A.html" (assembly-base-name (assembly-next assembly))))
                         "Symbols"
                         "View alphabetic index of symbols"
                         (format nil "~A-symbols.html" (assembly-base-name assembly)))))

(defun output-assembly (assembly)
  (with-open-file (output (make-pathname :directory (assembly-output-directory assembly)
                                         :name (assembly-base-name assembly)
                                         :type "html")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((h (make-html :output output
                        :assembly assembly)))
      (html-preamble output (funcall (assembly-namer assembly)
                                     (assembly-index assembly))
                            "assembly"
                            assembly)
      (main-navigation h nil)
      (begin-element h :table :class "listing")
      (begin-element h :tbody)
      (loop for line across (assembly-lines assembly) do
        (html-line line h nil))
      (end-element h)
      (end-element h)
      (when (> (length (assembly-lines assembly)) 10)
        (main-navigation h t))
      (html-postamble h))))

(defun index-navigation (h endp)
  (let ((assembly (html-assembly h)))
    (output-navigation h endp
                         "Previous"
                         "Go to index for previous assembly"
                         (when (assembly-previous assembly)
                           (format nil "~A-symbols.html" (assembly-base-name (assembly-previous assembly))))
                         "Home"
                         "Return to landing page"
                         (format nil "~A.html" (assembly-output-name assembly))
                         "Next"
                         "Go to index for next assembly"
                         (when (assembly-next assembly)
                           (format nil "~A-symbols.html" (assembly-base-name (assembly-next assembly))))
                         "Assembly"
                         "Return to listing"
                         (format nil "~A.html" (assembly-base-name assembly)))))

(defun index-one (h symbol)
  (begin-element h :details :id (sap-symbol-link-name symbol))
  (begin-element h :summary)
  (begin-element h :span :class "index-symbol-name")
  (loop for c across (sap-symbol-name symbol) do
    (format-html h "~A" (if (char= c #\Space)
                            "␣"
                            c)))
  (end-element h :span)
  (format-html h " = ~A" (sap-symbol-value symbol))
  (end-element h :summary)
  (begin-element h :div :class "symbol-details")
  (labels ((li (text)
             (begin-element h :li)
             (begin-element h :span :class "heading")
             (format-html h text)
             (end-element h :span))
           (listing ()
             (begin-element h :table :class "listing")
             (begin-element h :tbody))
           (end-listing ()
             (end-element h :tbody)
             (end-element h :table)))
    (begin-element h :ol)
    (li "Definition")
    (listing)
    (loop for line in (sap-symbol-remarks symbol) do
      (html-line line h t))
    (html-line (sap-symbol-definition symbol) h t)
    (end-listing)
    (end-element h :li)
    (unless (null (sap-symbol-references symbol))
      (let ((locals (list))
            (externals (list)))
        (loop for line in (sap-symbol-references symbol) do
          (if (eq (line-assembly line) (sap-symbol-home symbol))
              (push line locals)
              (push line externals)))
        (unless (null locals)
          (li (let ((length (length locals)))
                (if (= length 1)
                    (if (null externals)
                        "Reference"
                        "Local reference")
                    (if (null externals)
                        (format nil "References (~D)" length)
                        (format nil "Local references (~D)" length)))))
          (listing)
          (loop for local in locals do
            (html-line local h t))
          (end-listing)
          (end-element h :li))
        (unless (null externals)
          (li (let ((length (length externals)))
                (if (= length 1)
                    "External reference"
                    (format nil "External references (~D)" length))))
          (listing)
          (loop for external in externals do
            (html-line external h t))
          (end-listing)
          (end-element h :li))))
    (end-element h :ol)
    (end-element h :div)
    (end-element h :details)))

(defun output-index (assembly)
  (with-open-file (output (make-pathname :directory (assembly-output-directory assembly)
                                         :name (format nil "~A-symbols" (assembly-base-name assembly))
                                         :type "html")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((h (make-html :output output
                        :assembly assembly)))
      (html-preamble output "Symbols" "symbol-index" assembly)
      (index-navigation h nil)
      (begin-element h :div :class "symbol-list")
      (let ((symbols (sort (loop for v being the hash-values of (assembly-symbols assembly)
                                 collect v)
                           #'string<
                           :key #'sap-symbol-name)))
        (loop for symbol in symbols do
          (index-one h symbol))
        (end-element h :div)
        (when (> (hash-table-count (assembly-symbols assembly)) 10)
          (index-navigation h t)))
      (html-postamble h))))

(defun output-landing (assemblies)
  (let ((first (first assemblies))) ; get info from here
    (with-open-file (output (make-pathname :directory (assembly-output-directory first)
                                           :name (assembly-output-name first)
                                           :type "html")
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (let ((h (make-html :output output
                          :assembly nil)))
        (html-preamble output (assembly-title first) "landing-page" nil)
        (funcall (assembly-introducer first) h)
        (begin-element h :table :class "assemblies")
        (begin-element h :tbody)
        (loop for assembly in assemblies as i from 1 do
          (begin-element h :tr)
          (begin-element h :td)
          (format-html h "~A" (funcall (assembly-namer first) i))
          (end-element h :td)
          (begin-element h :td)
          (begin-element h :a :href (format nil "~A.html" (assembly-base-name assembly)))
          (format-html h "Assembly")
          (end-element h :a)
          (end-element h :td)
          (begin-element h :td)
          (begin-element h :a :href (format nil "~A-symbols.html" (assembly-base-name assembly)))
          (format-html h "Symbols")
          (end-element h :a)
          (end-element h :td)
          (end-element h :tr))
        (end-element h :tbody)
        (end-element h :table)
        (html-postamble h)))))