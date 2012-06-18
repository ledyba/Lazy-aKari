(use srfi-13)

(define (eval-lazy obj)
	(display obj))

(define (starts-with str index prefix)
	(let (
			(str-len (- (string-length str) index))
			(pre-len (string-length prefix)))
		(cond
			((< str-len pre-len) #f)
			((string= str prefix index (+ index pre-len) 0 pre-len) #t)
			(else #f))))

(define (parse-string str)
	(letrec
		((loop (lambda (src acc idx total)
				(cond 
					((>= idx total)
						acc)
					((starts-with src idx "わぁい") ;`
						(loop src (cons #\' acc) (+ 3 idx) total))
					((starts-with src idx "うすしお") ;s
						(loop src (cons #\s acc) (+ 4 idx) total))
					((starts-with src idx "あかり") ;k
						(loop src (cons #\k acc) (+ 3 idx) total))
					((starts-with src idx "だいすき") ;i
						(loop src (cons #\i acc) (+ 4 idx) total))
					(else
						(loop src acc (+ 1 idx) total))))))
		(reverse (loop str '() 0 (string-length str)))))

(define (parse-from-file filename)
	(let*
		((port (open-input-file filename)))
		(parse-string (port->string port))))

(define (main args)
	(cond
		((not (and (pair? args) (pair? (cdr args))))
			(display "please input filename\n"))
		(else
			(let ((obj (parse-from-file (cadr args))))
				(eval-lazy obj)))))

