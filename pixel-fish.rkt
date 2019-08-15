#lang racket/base

(module pim racket/base
  (provide (struct-out img))

  (struct img (width height pixels)))
  

(module pim-lang racket/base
  (require (for-syntax racket/base)
           syntax/parse/define
           (submod ".." pim))
  (provide (rename-out [mb #%module-begin])
           (except-out (all-from-out racket/base)
                       #%module-begin))
  
  (define-syntax-parser mb
    [(_ width:nat height:nat pixels:nat ...)
     #:fail-unless
     (= (length (syntax->list #'(pixels ...)))
        (* (syntax-e #'width) (syntax-e #'height)))
     "expected proper number of pixels for height and width"
     #'(#%module-begin        
        (define image (img width height (vector pixels ...)))
        (provide image))])
  )

(module fish (submod ".." pim-lang)
  22 16
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  00 00 02 02 00 00 00 00 00 00 02 02 02 02 02 00 00 00 00 00 00 00 
  00 02 03 03 02 02 00 00 02 02 03 03 03 03 03 02 02 00 00 00 00 00 
  00 02 03 03 03 03 02 02 03 03 03 03 03 03 03 03 03 02 02 00 00 00 
  00 02 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 02 00 00 
  00 02 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 02 00 
  00 02 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 02 00 
  00 02 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 03 02 00 00 
  00 02 03 03 03 02 02 03 03 03 03 03 03 03 03 03 03 02 02 00 00 00 
  00 02 03 03 02 00 00 02 03 02 02 03 03 03 03 02 02 00 00 00 00 00 
  00 00 02 02 00 00 00 00 02 00 00 02 02 02 02 00 00 00 00 00 00 00 
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 
  )

(require racket/class
         racket/draw
         
         (only-in racket/port
                  call-with-output-bytes)
         (only-in racket/vector
                  vector-copy)
         
         
         (submod "." pim)
         (prefix-in fish: (submod "." fish)))

;; http://androidarts.com/palette/16pal.htm
(define palette
  (vector (bytes #x00 #x00 #x00 #x00)  ; void
          (bytes #x00 #x9D #x9D #x9D)  ; gray
          (bytes #x00 #xFF #xFF #xFF)  ; white  
          (bytes #x00 #xBE #x26 #x33)  ; red
          (bytes #x00 #xE0 #x6F #x8B)  ; meat
          (bytes #x00 #x49 #x3C #x2B)  ; darkbrown
          (bytes #x00 #xA4 #x64 #x22)  ; brown
          (bytes #x00 #xEB #x89 #x31)  ; orange
          (bytes #x00 #xF7 #xE2 #x6B)  ; yellow
          (bytes #x00 #x2F #x48 #x4E)  ; darkgreen
          (bytes #x00 #x44 #x89 #x1A)  ; green
          (bytes #x00 #xA3 #xCE #x27)  ; slimegreen
          (bytes #x00 #x1B #x26 #x32)  ; nightblue
          (bytes #x00 #x00 #x57 #x84)  ; seablue
          (bytes #x00 #x31 #xA2 #xF2)  ; skyblue
          (bytes #x00 #xB2 #xDC #xEF)  ; cloudblue
          ))

(define (remap-palette pal m)
  (define new-pal (vector-copy pal))
  (for ([old+new (in-list m)])
    (vector-set! new-pal (cdr old+new) (vector-ref pal (car old+new))))
  new-pal)

(define (img-index img x y)
  (+ x (* (img-width img) y)))

(define (img-ref img x y)
  (vector-ref (img-pixels img) (img-index img x y)))

(define (img->bitmap img #:palette cmap #:scale [scale 1])
  (define bw (* scale (img-width img)))
  (define bh (* scale (img-height img)))
  (define (down v)
    (inexact->exact (floor (/ v scale))))  
  (define px-bytes
    (call-with-output-bytes
     (lambda (out)
       (for* ([y (in-range bh)] [x (in-range bw)])
         (write-bytes
          (vector-ref cmap (img-ref img (down x) (down y)))
          out)))))
  (let ([bitmap (make-bitmap bw bh #f)])
    (send bitmap set-argb-pixels 0 0 bw bh px-bytes)
    bitmap))

(module* main #f
  (require (only-in pict bitmap blank cc-superimpose table))
  (define pal (remap-palette palette '([0 . 2] [2 . 0])))
  (define fishes
    (for*/list ([wc (in-list '(0 13 14 15 4))]
                [fc (in-range 3 16)])
      (bitmap
       (img->bitmap fish:image
                    #:scale 4
                    #:palette
                    (remap-palette pal `([,wc . 0]
                                         [,fc . 3]))))))

  (define ncols (inexact->exact (ceiling (sqrt (length fishes)))))
  (define fill-fishes
    (append fishes
            (let ([n (- (* ncols ncols) (length fishes))])
              (for/list ([_x n]) (blank)))))

  (table ncols
         fill-fishes
         cc-superimpose
         cc-superimpose
         10
         10))