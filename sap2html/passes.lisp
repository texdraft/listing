(defun remove-indexes (assembly)
  (let ((end 0))
    (loop for line across (assembly-lines assembly) as i from 0 do
      (when (and (every #'digit-char-p (subseq line 24 29))
                 (string= (subseq line 37 40) "END"))
        (setf end i)
        (return)))
    (setf (assembly-lines assembly) (subseq (assembly-lines assembly) 0 end))))

(defun deconstruct-lines (assembly)
  (let ((deconstructed (make-array (length (assembly-lines assembly))
                                   :adjustable t
                                   :fill-pointer 0)))
    (loop for line across (assembly-lines assembly) as i from 1 do
      (vector-push (deconstruct-line assembly line i) deconstructed))
    (setf (assembly-lines assembly) deconstructed)))

(defun lookup-symbol (name assembly &optional debug)
  (or (gethash name (assembly-symbols assembly))
      (loop for table in *library*
            thereis (gethash name table))))

(defun every-space (string)
  (every (lambda (c) (char= c #\Space)) string))

(defun deconstruct-line (assembly line number)
  (let ((sequence (subseq line 102 110)))
    (if (or (char= (char line 0) #\1)
            (every-space (subseq line 0 41)))
        (make-line :assembly assembly
                   :remarksp t
                   :remarks (subseq line 41 102)
                   :number number
                   :sequence sequence)
        (deconstruct-code-line assembly (if (char= (char line 0) #\1)
                                            (concatenate 'string " " (subseq line 1))
                                            line)
                                        sequence
                                        number))))

(defun deconstruct-code-line (assembly line sequence number)
  (let ((flags (subseq line 0 7))
        (address (subseq line 7 12))
        (assembled (subseq line 13 29))
        (location (subseq line 30 37))
        (generatedp (every-space (subseq line 27)))
        (i 37)
        (operation-end 37))
    (loop while (and (< i 43)
                     (char= (char line i) #\Space))
          do (incf i))
    (if (= i 37)
        (loop until (char= (char line operation-end) #\Space)
              while (< operation-end 43)
              do (incf operation-end))
        (setf operation-end (- i 1)))
    (let* ((operation-text (if (= i 37)
                               (subseq line i operation-end)
                               ""))
           (operation (gethash operation-text *operations*)))
      (if (and (or (not operation)
                   (and operation
                        (not (string= operation-text "BCD"))))
               (every-space (subseq line operation-end 47)))
          (make-line :assembly assembly
                     :flags flags
                     :generatedp generatedp
                     :assembled assembled
                     :number number
                     :location location
                     :operation operation
                     :indirectp (char= (char line operation-end) #\*)
                     :variable " "
                     :variable-length 0
                     :remarks (subseq line (1+ operation-end) 102)
                     :sequence sequence)
          (make-line :assembly assembly
                     :flags flags
                     :address address
                     :generatedp generatedp
                     :assembled assembled
                     :number number
                     :location location
                     :operation operation
                     :indirectp (char= (char line operation-end) #\*)
                     :variable (subseq line operation-end 102)
                     :variable-length 0
                     :sequence sequence)))))

(defun separate-variable-fields (assembly)
  (loop for line across (assembly-lines assembly) do
    (cond ((line-remarksp line))
          ((and (line-operation line)
                (string= (operation-mnemonic (line-operation line)) "BCD"))
           (let ((count (or (digit-char-p (char (line-variable line) 0))
                            10)))
             (unless (> count 1)
               (setf (line-remarks line) (subseq (line-variable line) 7)
                     (line-variable line) (subseq (line-variable line) 0 7)))
             (setf (line-variable-length line) (* count 6))))
          ((line-remarks line))
          (t
           (let ((variable (string-left-trim '(#\Space) (line-variable line)))
                 (variable-end 0))
             (when (> (length variable) 0)
               (loop for i from 0 below 61
                     until (char= (char variable i) #\Space)
                     do (incf variable-end)))
             (cond ((not (line-operation line))
                    (setf (line-variable line) variable))
                   ((not (line-remarks line))
                    (setf (line-remarks line) (format nil "~A" (subseq variable variable-end))
                          (line-variable line) (subseq variable 0 variable-end)
                          (line-variable-length line) variable-end))))))))


(defun symbol-character-p (c)
  (or (alphanumericp c)
      (member c '(#\$ #\( #\) #\. #\=))))

(defun tokenize-fields (assembly)
  (loop for line across (assembly-lines assembly) do
    (unless (line-remarksp line)
      (if (some #'digit-char-p (line-address line))
          (setf (line-address line) (parse-integer (line-address line) :radix 8))
          (setf (line-address line) nil))
      (cond ((and (line-operation line)
                  (string= (operation-mnemonic (line-operation line)) "HED"))
             (setf (assembly-head assembly) (let ((c (char (line-location line) 0)))
                                              (if (char= c #\0)
                                                  #\Space
                                                  c))
                   (line-location line) (cons :hed (assembly-head assembly))))
            ((some #'symbol-character-p (line-location line))
             (let* ((unheaded (string-trim '(#\Space) (line-location line)))
                    (name (if (< (length unheaded) 6)
                              (format nil "~C~5@A" (assembly-head assembly) unheaded)
                              unheaded))
                    (got (lookup-symbol name assembly)))
               (unless got
                 (setf got (make-sap-symbol :name name
                                            :home assembly
                                            :pretty-name unheaded)
                       (gethash name (assembly-symbols assembly)) got))
               (setf (line-location line) (cons got (line-location line)))))
           ((string= (line-location line) "       ")
            (setf (line-location line) nil)))
      (tokenize-variable line (line-variable line) (line-operation line) assembly))))

(defun tokenize-variable (line variable operation assembly)
  (let ((tokens nil)
        (i 0)
        (variable (concatenate 'string variable " "))
        (mnemonic (if operation
                      (intern (operation-mnemonic operation) :keyword)
                      nil)))
    (cond ((eq mnemonic :bcd)
           (let ((count (or (digit-char-p (char variable 0))
                             10)))
             (setf (line-variable line) (list (make-token :type :bcd
                                                           :data (cons count (subseq variable 1 (1+ (* count 6)))))))))
          (t
           (loop while (< i (length variable)) do
             (let ((c (char variable i)))
               (cond ((char= c #\,)
                      (push (make-token :type :comma) tokens)
                      (incf i))
                     ((char= c #\+)
                      (push (make-token :type :plus) tokens)
                      (incf i))
                     ((char= c #\-)
                      (push (make-token :type :minus) tokens)
                      (incf i))
                     ((char= c #\*)
                      (push (make-token :type :asterisk) tokens)
                      (incf i))
                     ((char= c #\/)
                      (push (make-token :type :slash) tokens)
                      (incf i))
                     ((or (symbol-character-p c)
                          (and (eq mnemonic :dec)
                               (char= c #\.)))
                      (let* ((text (with-output-to-string (s)
                                     (loop do (write-char c s)
                                              (incf i)
                                              (setf c (char variable i))
                                           while (or (symbol-character-p c)
                                                     (and (eq mnemonic :dec)
                                                          (char= c #\.))))))
                             (dollar (find #\$ text)))
                        (cond ((every (lambda (d)
                                        (or (digit-char-p d)
                                            (and (eq mnemonic :dec)
                                                 (member d '(#\B #\E #\.)))))
                                      text)
                               (push (make-token :type :number :data text)
                                     tokens))
                             (t
                              (let* ((headed (cond ((char= (char text 0) #\$)
                                                    (format nil "~6@A" (subseq text 1)))
                                                   ((position #\$ text)
                                                    (format nil "~C~5@A" (char text 0) (subseq text 2)))
                                                   ((= (length text) 6)
                                                    text)
                                                   (t
                                                    (format nil "~C~5@A" (assembly-head assembly) text))))
                                     (got (lookup-symbol headed assembly)))
                                (push (make-token :type :symbol
                                                  :data (cons (or got headed) text))
                                      tokens))))))
                     ((char= c #\Space)
                      (return))
                     (t
                      (error "weird character ~S" c)))))
           (setf (line-variable line) (nreverse tokens))))))

(defun gather-library (assembly)
  (loop for line across (assembly-lines assembly) do
    (unless (line-remarksp line)
      (let ((operation (line-operation line)))
        (when (and operation
                   (string= (operation-mnemonic operation) "WST"))
          (push (assembly-symbols assembly) *library*))))))

(defun define-symbols (assembly)
  (loop for line across (assembly-lines assembly) as i from 0 do
    (unless (line-remarksp line)
      (let ((location (line-location line)))
        (when (and location
                   (not (eq (car location) :hed)))
          (let ((symbol (car location)))
            (when (> i 0)
              (let ((remarks (loop for j downfrom (- i 1)
                                   while (> j 0)
                                   as other-line := (aref (assembly-lines assembly) j)
                                   while (line-remarksp other-line)
                                   collect other-line)))
                (setf (sap-symbol-remarks symbol) (nreverse remarks))))
            (setf (sap-symbol-definition symbol) line)
            (if (line-address line)
                (setf (sap-symbol-value symbol) (line-address line))
                (let ((segment (parse-integer (subseq (line-assembled line) 11)
                                              :radix 8
                                              :junk-allowed t)))
                  (setf (sap-symbol-value symbol) segment)))
            (when (line-operation line)
              (let* ((mnemonic (intern (operation-mnemonic (line-operation line))
                                      :keyword))
                     (type (if (member mnemonic
                                       '(:equ :syn :oct :dec :bcd :bss :bes))
                               mnemonic
                               :address)))
                (setf (sap-symbol-type symbol) type 
                      (sap-symbol-text symbol) (if (eq type :address)
                                                   nil
                                                   (line-variable line)))))))))))

(defun resolve-symbols (assembly)
  (loop for line across (assembly-lines assembly) do
    (when (line-variable line)
      (loop for token in (line-variable line) do
        (when (and (eq (token-type token) :symbol)
                   (stringp (car (token-data token))))
          (setf (car (token-data token)) (lookup-symbol (car (token-data token)) assembly t)))))))

(defun cross-reference (assembly)
  (loop for line across (assembly-lines assembly) do
    (when (line-variable line)
      (loop for token in (line-variable line) do
        (when (eq (token-type token) :symbol)
          (let ((symbol (car (token-data token))))
            (when symbol
              (pushnew line (sap-symbol-references symbol)
                       :test #'eq))))))))

(defun symbol-link-names (assembly)
  (maphash (lambda (key value)
             (cond ((= (length (sap-symbol-pretty-name value)) 6)
                    (setf (sap-symbol-link-name value) (format nil "$~A" (sap-symbol-pretty-name value))))
                   ((char/= (char (sap-symbol-name value) 0) #\Space)
                    (setf (sap-symbol-link-name value)
                          (format nil "~C$~A"
                                      (char (sap-symbol-name value) 0)
                                      (string-left-trim '(#\Space) (subseq (sap-symbol-name value) 1)))))
                   (t
                    (setf (sap-symbol-link-name value) (string-left-trim '(#\Space) (sap-symbol-name value))))))
           (assembly-symbols assembly)))