;;;; -*- Mode: Lisp -*-

;;;; parser.lisp

;;;; Mervic Francesco 816871
;;;; Vitali Tommaso   816218

(defun json-parse (string)
      (json-parse-clean (clean-blank string)))

;; funzione che elimina tutti gli spazi (Space, Newline, Backspace, Tab, Return)
;; se questi non si trovano all'interno di una stringa (delimitata con " o ')
;; se gli spazi spezzano un numero restituisce un errore
(defun clean-blank (string &optional (pos 0) (l nil))
  (cond ((>= pos (length string)) ;; la stringa è finita
         string)
        ((or (eq (char string pos) #\")
             (eq (char string pos) #\'))
         (cond ((eq l nil)
                (clean-blank string (+ pos 1) (cons (char string pos) l)))
               ((eq (car l) (char string pos))
                (clean-blank string (+ pos 1) (cdr l)))
               (T (clean-blank string (+ pos 1) l))
               ))
        ((and (eq l nil)
              (or (eq (char string pos) #\Space) ;; sono su un carattere da eliminare
                  (eq (char string pos) #\Newline)
                  (eq (char string pos) #\Backspace)
                  (eq (char string pos) #\Tab)
                  (eq (char string pos) #\Return)))
         (if (or (eq pos 0)
                 (eq pos (- (length string) 1)))
             (clean-blank (concatenate 'string 
                                       (subseq string 0 pos) 
                                       (subseq string (+ pos 1))) pos l) ;; elimino il carattere su cui mi trovo e vado avanti
           (if (or (and (not (eq nil (digit-char-p (char string (+ pos 1))))) ;; posso eliminarlo solo se non si trova in mezzo ad un numero
                        (not (eq nil (digit-char-p (char string (- pos 1))))))
                   (and (not (eq nil (digit-char-p (char string (+ pos 1)))))
                        (eq #\- (char string (- pos 1))))
                   (and (not (eq nil (digit-char-p (char string (- pos 1)))))
                        (eq #\. (char string (+ pos 1))))
                   (and (not (eq nil (digit-char-p (char string (+ pos 1)))))
                        (eq #\. (char string (- pos 1)))))
               (error "Syntax error.")
             (clean-blank (concatenate 'string ;; elimino il carattere su cui mi trovo e vado avanti
                                       (subseq string 0 pos) 
                                       (subseq string (+ pos 1)))
                          pos l))))
        (T (clean-blank string (+ pos 1) l))
        ))

(defun json-parse-clean (string)
  (cond ((string= string "{}")
         (list 'json-obj))
        ((string= string "[]")
         (list 'json-array))
        ((and (string= string "{" :end1 1)
              (string= string "}" :start1 (- (length string) 1)))
         (cons 'json-obj 
               (parse-members (subseq string 1 (- (length string) 1))))) ;; inizia con { e finisce con }
        ((and (string= string "[" :end1 1)
              (string= string "]" :start1 (- (length string) 1)))
         (cons 'json-array 
               (parse-elements (subseq string 1 (- (length string) 1))))) ;; inizia con [ e finisce con ]
        (T (error "~S is not a valid JSON string." string)) ;; errore: la stringa non è valida
        ))

(defun parse-members (string)
  (cond ((not (or (eq #\" (char string 0))
                  (eq #\' (char string 0))))
         (error "~S is not a sequence of pair." string))
        ((eq (position (char string 0) (subseq string 1)) nil)
         (error "~S doesn't conclude the first string." string))
        ((eq (next-pair string) nil)
         (cons (list (subseq string 1 (+ 1 
                                         (position (char string 0) 
                                                   (subseq string 1 
                                                           (length string)))))
                     (parse-value (subseq string
                                          (+ 3 
                                             (position (char string 0) 
                                                       (subseq string 1 
                                                               (length string))))
                                          (next-pair string))))
               nil))
        (T (cons (list (subseq string 1 (+ 1 
                                           (position (char string 0) 
                                                     (subseq string 1 
                                                             (length string)))))
                       (parse-value (subseq string 
                                            (+ 3 
                                               (position (char string 0) 
                                                         (subseq string 1 
                                                                 (length string))))
                                            (next-pair string))))
                 (parse-members (subseq string (+ 1 (next-pair string)))))) ;; parse-member ricorsivo
        ))

;; funzione che fa il parsing del valore contenuto all'interno della coppia
;; oppure all'interno di un'array
;; ci possono essere: stringhe, numeri, oggetti o array
(defun parse-value (string)
  (cond ((or (not (eq nil (digit-char-p (char string 0)))) ;; se il primo carattere è un numero oppure un meno
             (eq #\- (char string 0)))
         (parse-int string))
        ((or (eq #\" (char string 0)) ;; se il primo carattere è un " o ' è una stringa
             (eq #\' (char string 0)))
         (parse-string string))
        ((eq #\{ (char string 0)) ;; se il primo carattere è un { è un oggetto -> chiamo la json-parse
         (if (eq #\} (char string (- (length string) 1)))
             (json-parse-clean (subseq string 0 (+ 1 (find-next string #\}))))
           (error "Not an object.")))
        ((string= "[]" string :end2 2)
         '(json-array))
        ((eq #\[ (char string 0)) ;; se il primo carattere è un [ è un array
         (if (eq #\] (char string (- (length string) 1)))
             (cons 'json-array 
                   (parse-elements (subseq string 1 (find-next string #\]))))
           (error "Not an array.")))
        (T (error "Syntax error."))
        ))

;; parse degli elementi di un array
(defun parse-elements (string)
  (if (eq (next-pair string) nil)
      (cons (parse-value (subseq string 0 (next-pair string)))
            nil)
    (cons (parse-value (subseq string 0 (next-pair string)))
          (parse-elements (subseq string (+ 1 (next-pair string)))))
    ))

;; funzione che fa il parse di un numero intero,
;; nel caso trovi un . chiama la parse-decimal e aggiunge la parte decimale
(defun parse-int (string &optional (pos 0) (val 0))
  (cond ((>= pos (length string))
         val)
        ((not (eq nil (digit-char-p (char string pos))))
         (parse-int string
                    (+ pos 1)
                    (+ (* val 10) (- (char-code (char string pos)) 48))))
        ((eq #\. (char string pos))
         (if (< pos (- (length string) 1))
             (+ val (parse-decimal string
                                   (+ pos 1)))
           (error "~S does not contain a valid number." string)))
        ((eq #\- (char string pos))
         (if (and (eq val 0)
                 (< pos (- (length string) 1)))
             (* -1 (parse-int string (+ pos 1)))
           (error "~S does not contain a valid number." string)))
        (T (error "~S does not contain a valid number." string))
        ))

;; funzione che fa il parse della parte decimale di un numero
(defun parse-decimal (string pos &optional (exp 0.1) (val 0))
  (cond ((>= pos (length string))
         val)
        ((not (eq nil (digit-char-p (char string pos))))
         (parse-decimal string
                        (+ pos 1)
                        (/ exp 10)
                        (+ val (* exp (- (char-code (char string pos)) 48)))))
        (T (error "~S does not contain a valid number." string))
        ))

;; funzione che fa il parse di una stringa (inizia e finisce con " oppure ')
(defun parse-string (string)
  (if (eq (char string 0)
          (char string (- (length string) 1)))
      (subseq string 1 (- (length string) 1))
    (error "~S does not contain a valid string." string)))

(defun find-next (string char-to-find &optional (pos 1) (l nil))
  (cond ((>= pos (length string)) ;; fuori lunghezza massima
         nil)
        ((and (eq l nil)
              (eq (char string pos) char-to-find)) ;; se non sono all'interno di niente ed incontro il carattere che cerco
         pos)
        ((or (eq (car l) #\")
             (eq (car l) #\')) ;; se sono all'interno di una stringa
         (if (eq (car l) (char string pos)) ;; se mi trovo alla chiusura della stringa la chiudo
             (find-next string char-to-find (+ pos 1) (cdr l))
           (find-next string char-to-find (+ pos 1) l)))
        ((or (eq (char string pos) #\")
             (eq (char string pos) #\')) ;; se inizia la stringa la apro
         (find-next string char-to-find (+ pos 1) (cons (char string pos) l)))
        ((eq #\{ (char string pos)) ;; {
         (find-next string char-to-find (+ pos 1) (cons #\{ l)))
        ((eq #\[ (char string pos)) ;; [
         (find-next string char-to-find (+ pos 1) (cons #\[ l)))
        ((and (eq #\} (char string pos)) ;; }
              (eq #\{ (car l)))
         (find-next string char-to-find (+ pos 1) (cdr l)))
        ((and (eq #\] (char string pos)) ;; ]
              (eq #\[ (car l)))
         (find-next string char-to-find (+ pos 1) (cdr l)))
        (T (find-next string char-to-find (+ pos 1) l))
        ))

;; funzione che trova la posizione della prossima coppia
(defun next-pair (string &optional (pos 0) (l nil))
  (cond ((>= pos (length string)) ;; fuori lunghezza massima
         nil)
        ((and (eq l nil)
              (eq (char string pos) #\,)) ;; se non sono all'interno di niente ed incontro una ,
         pos)
        ((or (eq (car l) #\")
             (eq (car l) #\')) ;; se sono all'interno di una stringa
         (if (eq (car l) (char string pos)) ;; se mi trovo alla chiusura della stringa la chiudo
             (next-pair string (+ pos 1) (cdr l))
           (next-pair string (+ pos 1) l)))
        ((or (eq (char string pos) #\")
             (eq (char string pos) #\')) ;; se inizia la stringa la apro
         (next-pair string (+ pos 1) (cons (char string pos) l)))
        ((eq #\{ (char string pos)) ;; {
         (next-pair string (+ pos 1) (cons #\{ l)))
        ((eq #\[ (char string pos)) ;; [
         (next-pair string (+ pos 1) (cons #\[ l)))
        ((and (eq #\} (char string pos)) ;; }
              (eq #\{ (car l)))
         (next-pair string (+ pos 1) (cdr l)))
        ((and (eq #\] (char string pos)) ;; ]
              (eq #\[ (car l)))
         (next-pair string (+ pos 1) (cdr l)))
        (T (next-pair string (+ pos 1) l))
        ))

;; implementazione di json-get
(defun json-get (obj &rest fields)
  (json-get-list obj fields))

(defun json-get-list (obj fields)
  (cond ((eq fields nil)
         obj)
        ((and (typep (car fields) 'integer) ;; il campo è un numero, cerco all'interno di un array
              (eq (car obj) 'json-array))
         (if (eq (cdr fields) nil)
             (search-array (cdr obj) (car fields))
           (json-get-list (search-array (cdr obj) (car fields)) (cdr fields))))
        ((and (typep (car fields) 'string) ;; il campo è una stringa, lo cerco come attribute
              (eq (car obj) 'json-obj))
         (if (eq (cdr fields) nil)
             (search-obj (cdr obj) (car fields))
           (json-get-list (search-obj (cdr obj) (car fields)) (cdr fields))))
        (T (error "Fields must be integer for json-array or string for json-obj"))
        ))

(defun search-array (list n)
  (cond ((or (< n 0)
             (>= n (length list)))
         (error "~S not valid." n))
        ((eq n 0)
         (car list))
        (T (search-array (cdr list) (- n 1)))
        ))

(defun search-obj (list string)
  (cond ((eq list nil)
         (error "~S not found." string))
        ((string= (car (car list)) string)
         (second (car list)))
        (T (search-obj (cdr list) string))
        ))

;; implementazione di json-load
(defun json-load (filename)
  (json-parse (with-open-file (in filename
                                  :direction :input
                                  :if-does-not-exist :error)
                (read-string-from in))))

(defun read-string-from (input-stream)
  (let ((e (read-line input-stream nil ’eof)))
    (unless (eq e ’eof)
      (concatenate 'string e '(#\Newline) (read-string-from input-stream)))))

;; implementazione di json-write
(defun json-write (obj filename)
  (if (eq nil
          (with-open-file (out filename
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :error)
            (format out (obj-to-string obj))))
      filename))

;; funzione che trasforma un oggetto JSON in stringa
(defun obj-to-string (obj)
  (cond ((equal obj '(json-obj))
         "{}")
        ((equal obj '(json-array))
         "[]")
        ((eq (car obj) 'json-obj)
         (concatenate 'string
                      '(#\{ #\Newline)
                      (json-obj-to-string (cdr obj)) ;; lo chiamo solo se non è vuoto...
                      '(#\Newline #\})
                      ))
        ((eq (car obj) 'json-array)
         (concatenate 'string
                      '(#\[)
                      (json-array-to-string (cdr obj)) ;; lo chiamo solo se non è vuoto...
                      '(#\])
                      ))
        (T (error "~S is not a valid JSON object." obj))
        ))

(defun json-array-to-string (obj)
  (if (eq nil (second obj))
      (value-to-string (car obj))
    (concatenate 'string 
                 (value-to-string (car obj))
                 ", "
                 (json-array-to-string (cdr obj)))))

(defun json-obj-to-string (obj)
  (cond ((not (typep (car (car obj)) 'string))
         (error "Attribute ~S is not a string." (car (car obj))))
        ((eq nil (second obj))
         (concatenate 'string
                      "\""
                      (car (car obj))
                      "\" : "
                      (value-to-string (second (car obj)))
                      ))
        (T (concatenate 'string
                        "\""
                        (car (car obj))
                        "\" : "
                        (value-to-string (second (car obj)))
                        '(#\, #\Newline)
                        (json-obj-to-string (cdr obj))))
        ))

;; potrebbe essere un numero, una stringa, un oggetto o un array 
;; negli ultimi due casi posso chiamare obj-to-string
(defun value-to-string (obj)
  (cond ((or (typep obj 'integer)
             (typep obj 'float))
         (write-to-string obj))
        ((typep obj 'string)
         (concatenate 'string
                      "\""
                      obj
                      "\""))
        (T (obj-to-string obj))
        ))

;;;; end of file -- parser.lisp