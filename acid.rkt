#lang racket
;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
(provide acid)
(define null-stack '())
(struct closure (env xs))
(struct prim (n f))
(define (EVAL env stack xs)
  (if (null? xs)
      stack
      (let ([x (car xs)] [xs (cdr xs)])
        (cond
          [(symbol? x)
           (cond
             [(eq? x '&)
              (let ([x (car xs)] [xs (cdr xs)])
                (EVAL env (cons (closure env x) stack) xs))]
             [(eq? x 'λ)
              (let ([x (car xs)] [xs (cdr xs)] [v (car stack)] [stack (cdr stack)])
                (EVAL (hash-set env x v) stack xs))]
             [(eq? x '!)
              (let ([f (car stack)] [stack (cdr stack)])
                (EVAL env (APPLY f stack) xs))]
             [(eq? x 'quote)
              (let ([x (car xs)] [xs (cdr xs)])
                (EVAL env (cons x stack) xs))]
             [(eq? x 'letrec)
              (let ([ps (car xs)] [xs (cdr xs)])
                (letrec
                    ([defs (map (λ (p)
                                  (cons (car p) (delay (EVAL1 newenv (cdr p))))) ps)]
                     [newenv (hash-append env defs)])
                  (EVAL newenv stack xs)))]
             [else (EVAL env (cons (force (hash-ref env x)) stack) xs)])]
          [else (EVAL env (cons x stack) xs)]))))
(define (EVAL1 env xs)
  (let ([x (EVAL env null-stack xs)])
    (if (and (pair? x) (null? (cdr x)))
        (car x)
        (error "type error"))))
(define (hash-append h ps)
  (foldl (λ (p h) (hash-set h (car p) (cdr p))) h ps))
(define (APPLY f stack)
  (cond
    [(prim? f) (let-values ([(args stack) (split-at stack (prim-n f))])
                 (append (apply (prim-f f) args) stack))]
    [(closure? f) (EVAL (closure-env f) stack (closure-xs f))]
    [else (error "type error")]))
(define (FORCE x)
  (if (closure? x)
      (EVAL (closure-env x) null-stack (closure-xs x))
      x))
(define (prim11 f) (prim 1 (λ (x) (list (f x)))))
(define genv
  (hash
   'if (prim 3 (λ (y x b) (list (if b (FORCE x) (FORCE y)))))
   'car (prim11 car)
   'cdr (prim11 cdr)
   'cons (prim 2 (λ (d a) (list (cons a d))))
   'pair? (prim11 pair?)
   'list? (prim11 list?)
   'null? (prim11 null?)
   ))
(define (acid xs) (EVAL genv null-stack xs))
