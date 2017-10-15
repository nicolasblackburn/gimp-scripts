(define (item-is-group item)
	(= (car (gimp-item-is-group item)) TRUE))

(define (item-is-layer item)
	(= (car (gimp-item-is-layer item)) TRUE))

(define (item-get-parent item)
	(car (gimp-item-get-parent item)))

(define (item-has-parent item)
	(> (car (gimp-item-get-parent item)) -1))

(define (image-get-active-layer image)
	(car (gimp-image-get-active-layer image)))

(define (image-get-layers image)
	(cadr (gimp-image-get-layers image)))

(define (image-get-layers-count image)
	(car (gimp-image-get-layers image)))

(define (item-get-children item)
	(cadr (gimp-item-get-children item)))

(define (item-get-children-count item)
	(car (gimp-item-get-children item)))

(define (item-get-siblings item)
	(if (item-has-parent item)
		(item-get-children (item-get-parent item))
		(if (item-is-layer item) ;top-level layer
			(image-get-layers 1))))

(define (vector-for-each fn vect)
	(let*
			((len 		(vector-length vect))
			 (n			0))
		(while (< n len)
			(fn (vector-ref vect n))
			(set! n (+ n 1)))))

(define (vector-next vect item)
	(let*
			((len 		(vector-length vect))
			 (n			0)
			 (next		0))
		(while (< n len)
			(if (= item (vector-ref vect n))
				(set! next (+ n 1)))
			(set! n (+ n 1)))
		(if (< next len)
			(vector-ref vect next)
			(vector-ref vect 0)
		)))

(define (vector-prev vect item)
	(let*
			((len 		(vector-length vect))
			 (n			0)
			 (next		0))
		(while (< n len)
			(if (= item (vector-ref vect n))
				(set! next (- n 1)))
			(set! n (+ n 1)))
		(if (<= 0 next)
			(vector-ref vect next)
			(vector-ref vect (if (> len 0) (- len 1) 0))
		)))

(define (layer-hide-leafs item)
	(if (item-is-group item)
		(let*
				((layers-count 	(car (gimp-item-get-children item)))
				 (layers 		(cadr (gimp-item-get-children item)))
				 (n 0))
			(gimp-item-set-visible item TRUE)
			(while (< n layers-count)
				(layer-hide-leafs (vector-ref layers n))
				(set! n (+ n 1))))
		(gimp-item-set-visible item FALSE)))

(define (image-hide-layers image)
	(vector-for-each
		(lambda (layer) (layer-hide-leafs layer))
		(image-get-layers image)))

(define (only-current-layer-visible)
	(image-hide-layers 1)
	(gimp-item-set-visible (image-get-active-layer 1) TRUE)
	(gimp-displays-flush))

(define (only-prev-layer-visible)
	(image-hide-layers 1)
	(let* ((active-layer (image-get-active-layer 1)))
		(gimp-image-set-active-layer 1 (vector-prev (item-get-siblings active-layer) active-layer))
		(gimp-item-set-visible (image-get-active-layer 1) TRUE))
	(gimp-displays-flush))

(define (only-next-layer-visible)
	(image-hide-layers 1)
	(let* ((active-layer (image-get-active-layer 1)))
		(gimp-image-set-active-layer 1 (vector-next (item-get-siblings active-layer) active-layer))
		(gimp-item-set-visible (image-get-active-layer 1) TRUE))
	(gimp-displays-flush))

(script-fu-register
	"only-current-layer-visible"
	_"<Image>/Script-Fu/Only Current Layer Visible"
	"Only Current Layer Visible"
	"Nicolas Blackburn <nicolas@nblackburn.ca>"
	"Nicolas Blackburn"
	"2017"
	"RGBA GRAYA")

(script-fu-register
	"only-prev-layer-visible"
	_"<Image>/Script-Fu/Only Next Layer Visible"
	"Only Previous Layer Visible"
	"Nicolas Blackburn <nicolas@nblackburn.ca>"
	"Nicolas Blackburn"
	"2017"
	"RGBA GRAYA")

(script-fu-register
	"only-next-layer-visible"
	_"<Image>/Script-Fu/Only Previous Layer Visible"
	"Only Next Layer Visible"
	"Nicolas Blackburn <nicolas@nblackburn.ca>"
	"Nicolas Blackburn"
	"2017"
	"RGBA GRAYA")
