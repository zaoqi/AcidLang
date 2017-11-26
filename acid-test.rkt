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
(require rackunit)
(require "acid.rkt")
(define-syntax-rule (e x y ...) (check-equal? (acid (quote x)) (list y ...)))
(e (1 1 + !) 2)
(e (2 & (1 cons !) !) '(2 . 1))
(e (2 1) 1 2)
(e (letrec ([map & (λ f λ xs xs null? ! () & (xs car ! f !
                                                 xs cdr ! f map !
                                                 cons !) if !)]) quote (1 2 3) & (1 + !) map !) '(2 3 4))
