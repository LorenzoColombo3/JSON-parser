;;Santin Simone 886116
;;Colombo Lorenzo 885895

(defun jsonparse (jsonstring) 
    (if (not (stringp jsonstring)) (error "Non Ã¨ una stringa.")
        (splitList (removespaces (string_list jsonstring)))))

(defun splitList (JSONList)
    (cond ((and (eq (car JSONList) #\{) (eq (car (last JSONList)) #\}) 
               (cons 'jsonobj (splitMembers (removeExtern JSONList) nil 0 0))))
          ((and (eq (car JSONList) #\[) (eq (car (last JSONList)) #\]) 
               (cons 'jsonarray (splitArray (removeExtern JSONList) nil 0 0))))
          (T (error "no match braces"))))


(defun splitMembers (JSONList accumulator bracketsO bracketsC)
    (cond 
        ((and (null JSONList) (null accumulator)) nil)
        ((eq (car (last JSONList)) #\,) (error "Missing argument!")) 
        ((null JSONList) (list (splitPair accumulator nil 0)))
        ((and (eq (car JSONList) #\,) (= bracketsO bracketsC)) 
            (cons (splitPair accumulator nil 0) 
                  (splitMembers (cdr JSONList) nil 0 0)))   
        ((or (eq (car JSONList) #\{) (eq (car JSONList) #\[))
            (splitMembers (cdr JSONList) 
                (append accumulator (list (car JSONList))) 
                ( + 1 bracketsO) bracketsC))
        ((or (eq (car JSONList) #\]) (eq (car JSONList) #\}))
            (splitMembers (cdr JSONList) 
                (append accumulator (list (car JSONList))) 
                bracketsO ( + 1 bracketsC)))           
        (T  (splitMembers (cdr JSONList) 
            (append accumulator (list (car JSONList))) bracketsO bracketsC))))

(defun splitArray(JSONList accumulator bracketsO bracketsC)
    (cond   
        ((eq (car (last JSONList)) #\,) (error "Missing argument!"))         
        ((and (null JSONList) (null accumulator)) nil)       
        ((null JSONList)  (list (scanValue accumulator)))
        ((and (eq (car JSONList) #\,) (= bracketsO bracketsC)) 
            (cons (scanValue accumulator) 
                (splitArray (cdr JSONList) nil 0 0)))     
        ((or (eq (car JSONList) #\{) (eq (car JSONList) #\[))
            (splitArray (cdr JSONList) 
                (append accumulator (list (car JSONList))) 
                ( + 1 bracketsO) bracketsC))
        ((or (eq (car JSONList) #\]) (eq (car JSONList) #\}))
            (splitArray (cdr JSONList) 
                (append accumulator (list (car JSONList)))
                bracketsO ( + 1 bracketsC)))      
        ((or (null accumulator) 
        (= bracketsO bracketsC) (not (= bracketsO bracketsC)))
            (splitArray (cdr JSONList) 
                (append accumulator (list (car JSONList))) 
                 bracketsO bracketsC))           
        ((T (error "error syntax array")))))

(defun splitPair (pair accumulator nQuotes)
    (cond ((null pair) (error "syntax error"))
          ((eq (car pair) #\") 
                (splitPair (cdr pair) 
                    (append accumulator (list (car pair))) ( + 1 nQuotes)))
          ((and (eq (car pair) #\:) (= (mod nQuotes 2) 0)) 
                (list (parseString accumulator) (scanValue (cdr pair))))
          (T (splitPair (cdr pair) 
                (append accumulator (list (car pair))) nQuotes))))

(defun scanValue (value)
    (cond ((and (eq (car value) #\") (eq (car (last value)) #\") 
                (eq (is-string value) 2)) 
                    (parseString value))
          ((or (and (eq (car value) #\{) (eq (car (last value)) #\}))
               (and (eq (car value) #\[) (eq (car (last value)) #\])))
                 (splitList value))
          ((string= (list_string value) "null") 'null)
          ((string= (list_string value) "true") 'TRUE)
          ((string= (list_string value) "false") 'FALSE)
          ((numberp (list_number value)) (list_number value))
          (T (error "Not valid value"))))

;converte num (una lista) in un numero
(defun list_number (num)
    (if (eq (is-number num) 0)
        (if (null (find #\. num)) (parse-integer (list_string num)) 
                                  (parse-float (list_string num)))))

;controlla che value sia una stringa
(defun is-string (value)
    (cond ((null value) 0)
          ((eq (car value) #\") ( + (is-string (cdr value)) 1))
          (T (is-string(cdr value)))))

;verifica che num sia un numero
(defun is-number (num)
  (cond  ((eq nil (car num)) 0)
         ((or (and (> (char-int (car num)) 47) (< (char-int (car num)) 58)) 
            (eq (car num) #\.) 
            (eq (car num) #\-)
            (eq (car num) #\+)) (is-number(cdr num)))
         (T "")))
         
;rimuove il primo e ultimo carattere della lista
(defun removeExtern(charlist)
    (if (null charlist) nil
        (reverse (cdr (reverse (cdr charlist))))))

;rimuove gli spazi dalla lista
(defun removespaces (charlist)
    (remove #\Space
        (remove #\Tab
            (remove #\NewLine
                (remove #\Return charlist)))))

; converte da stringa a lista
(defun string_list (jsonstring)
    (if (= (length jsonstring) 0) nil
        (cons (char jsonstring 0) (string_list (subseq jsonstring 1)))))
    
;converte da lista a stringa
(defun list_string (charlistjson)
    (if (null charlistjson) ""
        (concatenate 'string
            (string (car charlistjson))
            (list_string (cdr charlistjson)))))

;controlla se la lista stringL è una stringa verificando che abbia i doppi apici
(defun parseString (stringL) 
        (if (not (and (eq (car stringL) #\") (eq (car (last stringL)) #\")))
            (error "The list has not a string!")
            (concatenate 'string
                (string (car (removeExtern stringL)))
                (list_string (cdr (removeExtern stringL))))))

;restituisce la lunghezza della lista
(defun lunghezza (lista)
       (cond ((null lista) 0)
             (t ( + 1 (lunghezza (cdr lista))))))


(defun jsonaccess(JSONString &optional attribute &rest index)
    (cond ((null JSONString) nil)
        ((and (null (car index)) (null attribute))  JSONString)
        ((and (or (not (null (car index))) (numberp attribute)) 
              (not (listp JSONString)))  
                (error "too many index"))
        ((and (eq 'jsonarray (car JSONString)) (numberp attribute)) 
            (if (and (listp (car index)) (not (null index))) 
                (jsonaccess (car (find-array (cdr JSONstring)  attribute))  
                    (car (car index)) (cdr (car index)))
                (jsonaccess (car (find-array (cdr JSONstring) attribute)) 
                    (car index) (cdr index))))   
        ((and (eq 'jsonobj (car JSONString)) (not (numberp attribute)))   
            (if (and (listp (car index)) (not (null index))) 
                (jsonaccess (find-obj (cdr JSONString) attribute index) 
                    (car (car index)) (cdr (car index)))
                (jsonaccess (find-obj (cdr JSONString) attribute index) 
                    (car index) (cdr index))))   
        ((and (eq 'jsonarray (car JSONString)) 
              (not (numberp attribute)) (> (lunghezza index) 0)) 
                (if (and (listp (car index)) (not (null index))) 
                    (jsonaccess (car (find-array (cdr JSONstring) 
                        (car (car index)))) attribute (cdr (car index)))
                    (jsonaccess (car (find-array (cdr JSONstring) 
                        (car index))) attribute (cdr index))))    
        (T (error "jsonaccess error"))))

(defun find-obj (JSONobj attribute index)
     (cond ((null JSONobj) (error "valore non trovato"))
        ((string= attribute (car (car JSONobj))) (car (cdr (car JSONobj))))
        (T (find-obj (cdr JSONobj) attribute index))))

(defun find-array (JSONarray index)
     (cond ((and (null JSONarray) (= index 0)) nil)
        ((not (numberp index)) (error "index is not a number"))
        ((and (< (lunghezza JSONarray) 2) (> index 0)) 
            (error "index bigger than JSONarray"))
	    ((= 0 index)  JSONarray)
	    (T (find-array (cdr JSONarray) ( - index 1)))))


(defun jsonread (filename)
  (with-open-file (streamin filename 
    :if-does-not-exist :error 
    :direction :input)
  (jsonparse (createstring streamin))))

(defun createstring (streamin)
  (let ((json (read-char streamin nil 'eof)))
    (if (eq json 'eof) ""
      (string-append json (createstring streamin)))))


(defun jsondump (LISPObj filename)
  (with-open-file (out filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (format out (parseLisp LISPObj)) filename))

(defun parseLisp (LISPObj)
  (cond
   ((eq (car LISPObj) 'jsonobj) 
        (concatenate 'string "{" (removeComma (writeObj (cdr LISPObj))) "}"))
   ((eq (car LISPObj) 'jsonarray) 
        (concatenate 'string "[" (removeComma(writeArray (cdr LISPObj)))"]"))
   (T (error "Error writing"))))

(defun writeObj (Obj)
  (cond
   ((null Obj) "")
   ((listp (car Obj)) 
        (concatenate 'string 
            (scanPair (car Obj))
            (writeObj (cdr Obj)) ))))

(defun writeArray (array)
  (cond
   ((null array) "")
   (T (concatenate 'string 
        (writeValue (car array))","(writeArray (cdr array))))))

(defun scanPair (Obj)
  (concatenate 'string "\""(car Obj)"\"" ":" (writeValue (car (cdr Obj))) ","))

(defun writeValue (value)
    (cond
    ((numberp value) 
        (write-to-string value))
    ((stringp value) 
        (concatenate 'string "\"" value "\""))
    ((eq value 'null) "null")
    ((eq value 'true) "true")
    ((eq value 'false) "false")
    (T (parseLisp value))))

(defun removeComma (obj)
  (cond
    ((string= "" obj) obj)
    (T (subseq obj 0 (- (length obj) 1)))))