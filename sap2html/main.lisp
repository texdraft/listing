(defun read-file (input title introducer namer output-directory output-name)
  (let ((assemblies nil)
        (count 0)
        (lines (make-array 10 :adjustable t :fill-pointer 0)))
    (read-line input nil)
    (loop for line := (read-line input nil) while line do
      (cond ((char= (char line 0) #\0)
             (incf count)
             (loop repeat 8 do (read-line input nil))
             (push (make-assembly :lines (copy-seq lines)
                                  :previous (first assemblies)
                                  :index count
                                  :base-name (format nil "~A-~D" output-name count)
                                  :title title
                                  :introducer introducer
                                  :namer namer
                                  :output-directory output-directory
                                  :output-name output-name)
                   assemblies)
             (setf (fill-pointer lines) 0)
             (when (second assemblies)
               (setf (assembly-next (second assemblies)) (first assemblies))))
            (t
             (vector-push-extend (format nil "~115A" line) lines))))
    (nreverse assemblies)))

(defun process-file (input title introducer namer output-directory output-name)
  (let ((assemblies (with-open-file (in input)
                      (read-file in title introducer namer output-directory output-name))))
    (print (length assemblies))
    (loop for pass in (list #'remove-indexes
                            #'deconstruct-lines
                            #'separate-variable-fields
                            #'tokenize-fields
                            #'gather-library
                            #'define-symbols
                            #'resolve-symbols
                            #'symbol-link-names
                            #'cross-reference
                            #'output-assembly
                            #'output-index)
          do (mapc pass assemblies))
    (output-landing assemblies)
    (print *undefined*)))

(defun fortran-name (i)
  (string-upcase (case i
                   (1 "1-cs")
                   (2 "card to tape")
                   (3 "section 6 cit to sap conversion")
                   (4 "section 6 on-line print")
                   (5 "tape 3,7 to 2,6")
                   (6 "successful compilation")
                   (7 "source program error")
                   (8 "batch monitor")
                   (9 "machine error")
                   (10 "section 1 4k version")
                   (11 "section 1 diagnostic")
                   (12 "section 1 prime")
                   (13 "section 1 double prime")
                   (14 "section 2 block 1")
                   (15 "section 2 block 2")
                   (16 "section 2 block 3")
                   (17 "section 2 block 4")
                   (18 "section 2 block 5")
                   (19 "section 2 block 6")
                   (20 "open subroutines")
                   (21 "part 1 of merge")
                   (22 "part 2 of merge")
                   (23 "part 3 of merge")
                   (24 "section 4")
                   (25 "section 5")
                   (26 "section 5 prime")
                   (27 "section 6 record a")
                   (28 "section 6 record b")
                   (29 "section 6 record c")
                   (30 "section 6 record d")
                   (31 "section 6 record e")
                   (32 "section 6 record f")
                   (33 "section 6 record g")
                   (34 "section 6 record h")
                   (35 "section 6 record i")
                   (36 "section 6 record j")
                   (37 "section 6 record k")
                   (38 "section 6 record l")
                   (39 "section 6 record m")
                   (40 "section 6 record n")
                   (41 "section 6 record p")
                   (42 "section 1 8k version")
                   (43 "section 5 8k version")
                   (44 "dbc (permanent library)")
                   (45 "csh (permanent library)")
                   (46 "tsh (permanent library)")
                   (47 "bdc (permanent library)")
                   (48 "sch (permanent library)")
                   (49 "sph (permanent library)")
                   (50 "sth (permanent library)")
                   (51 "lrt (permanent library)")
                   (52 "exp 1 (permanent library)")
                   (53 "exp 2 (permanent library)")
                   (54 "exp 3 (permanent library)")
                   (55 "log (general library)")
                   (56 "sin/cos (general library)")
                   (57 "exp (general library)")
                   (58 "sqrt (general library)")
                   (59 "atan (general library)")
                   (60 "tanh (general library)")
                   (61 "edt (editor program)")
                   (62 "plib (permanent librarian)")
                   (63 "glib (general librarian)")
                   (64 "tcvp (tape copy and verify program)")
                   (65 "bss loader (binary symbolic subroutine loader)")
                   (66 "diagnostic editor")
                   (67 "diagnostic call-in example")
                   (68 "diagnostic read-in")
                   (69 "main diagnostic record")
                   (otherwise "diagnostic error comment"))))

(defun copy-file (h file)
  (with-open-file (i file)
    (format-html h (with-open-file (i "intro.html")
                     (apply #'concatenate 'string (loop for line := (read-line i nil nil)
                                                        while line
                                                        collect line))))))

(defun do-fortran (out)
  (process-file "fort2.lst"
                "FORTRAN II"
                (lambda (h) (copy-file h "intro.html"))
                #'fortran-name
                out
                "fortran"))

(defun do-sap (out)
  (process-file "uasap.lst"
                "Symbolic Assembly Program (SAP)"
                (lambda (h) (copy-file h "intro.html"))
                (lambda (n)
                  (if (= n 1)
                      "Pass 1"
                      "Pass 2"))
                out
                "sap"))

(defun do-texdraft ()
  (do-fortran (uiop:native-namestring "~/texdraft.github.io/fortran/"))
  (do-sap (uiop:native-namestring "~/texdraft.github.io/fortran/")))
