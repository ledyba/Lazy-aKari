(use srfi-13)

(define (S x) (lambda (y) (lambda (z) ((x z) (y z)))))
(define (K x) (lambda (y) x))
(define (I x) x)

(define (num->church num)
	(cond
		((= num 0) (K I))
		(else 
			(let ((succ (S ((S (K S)) K))))
				(succ (num->church (- num 1)))))))

(define (church->num church)
	((church (lambda (x) (+ x 1))) 0))

(define (string->church-list txt)
	(letrec ((loop (lambda (left acc)
			(cond
				((pair? left)
					(loop (cdr left) (cons (num->church (car left)) acc)))
				(else acc)))))
		(reverse (loop (append (map char->integer (string->list txt)) '(256)) '()))))

(define (church-list->expr lst)
	(letrec ((loop (lambda (left)
		(cond
			((pair? left)
				((S ((S I) (K (car left)))) (K (loop (cdr left)))))
			(else I)))))
		(loop lst)))

(define (expr->string expr)
	(letrec ((loop (lambda (expr)
		(let* (
				(_car-int (church->num (expr K)))
				(_cdr (expr (K I))))
			(cond
				((and (< _car-int 256))
					(cons (integer->char _car-int) (loop _cdr)))
				(else
					'()))))))
		(list->string (loop expr))))

(define (eval-lazy obj)
	(let* (
			(expr (to-expr obj))
			(input-string (port->string (standard-input-port)))
			(input-church (church-list->expr (string->church-list input-string))))
			(let ((result (expr input-church)))
				(print (expr->string result)))))

;リストを展開し関数適用もしてしまう
(define (to-expr _obj)
	(letrec
		((loop (lambda (obj)
			(cond
				((pair? obj)
					(let ((elem (car obj)))
						(cond
							((eq? elem '@)
								(let* (
										(first (loop (cdr obj)))
										(second (loop (cdr first)))
										(expr ((car first) (car second))))
									(cons expr (cdr second))))
						(else (cons elem (cdr obj))))))
				(else '())))))
		(car (loop _obj))))

;インデックスの最初から文字がマッチするか返す
(define-syntax starts-with
  (syntax-rules ()
    ((_ str index prefix)
	(let (
			(str-len (- (string-length str) index))
			(pre-len (string-length prefix)))
		(cond
			((< str-len pre-len) #f)
			((string= str prefix index (+ index pre-len) 0 pre-len) #t)
			(else #f))))))

;あかり形式から扱いやすいリストに変換する
(define (parse-string str)
	(letrec
		((loop (lambda (src acc idx total)
				(cond 
					((>= idx total)
						acc)
					((starts-with src idx "わぁい") ;`
						(loop src (cons '@ acc) (+ 3 idx) total))
					((starts-with src idx "うすしお") ;s
						(loop src (cons S acc) (+ 4 idx) total))
					((starts-with src idx "あかり") ;k
						(loop src (cons K acc) (+ 3 idx) total))
					((starts-with src idx "だいすき") ;i
						(loop src (cons I acc) (+ 4 idx) total))
					(else
						(loop src acc (+ 1 idx) total))))))
		(reverse (loop str '() 0 (string-length str)))))

;ファイルから読み取って返す
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

