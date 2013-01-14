(ql:quickload 'cl-ppcre)

(defun tokenize (code)
  "Break code string into a list of string tokens"
  (declare (string code))
  (cl-ppcre:all-matches-as-strings
    "-?[\\d.]+|\".*?[^\\\\]\""
    code))
