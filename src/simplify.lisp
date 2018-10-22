; Given a list of reductions, this function will return the reduction whose pattern matches the term.
; If no reduction matches the term, it will return nil
(defun find-matching-reduction
  (term reductions)
  (if 
    (null reductions)
    nil
    (let*
      (
       (current-reduction (car reductions))
       (next-reduction (cdr reductions))
       (reduction-pattern (car current-reduction))
       (match-result (match term reduction-pattern)))
      (if 
        (null match-result)
        (find-matching-reduction term next-reduction)
        current-reduction))))

; Applies a reduction to a term
(defun apply-reduction
  (term reduction)
  (let
    (
      (reduction-result (cdr reduction)))
    (if
      (null (cdr reduction))
      (car reduction)
      reduction-result)))

(defun not-equal
  (l r)
  (not (equal l r)))

; Reductions should also be a list
(defun simplify-term
  (term reductions)
  (if
    (null term)
    nil
    (let* 
      (
        (matched-reduction (find-matching-reduction term reductions)))
      (if
        (null matched-reduction)
        (if
          (atom term)
          term
          (let*
              (
                (curr-car (car term))
                (curr-cdr (cdr term))
                (new-car (simplify-term curr-car reductions))
                (new-cdr (simplify-term curr-cdr reductions))
                (new-term (cons new-car new-cdr))) 
              (if
                (or
                  (not-equal new-car curr-car)
                  (not-equal new-cdr curr-cdr))
                (simplify-term new-term reductions)
                new-term))) 
        (let*
          (
            (term-after-reduction (apply-reduction term matched-reduction))
            (new-term (simplify-term term-after-reduction reductions)))
          (if
            (equal term-after-reduction new-term)
            new-term
            (simplify-term new-term reductions)))))))

(defun simplify
  (term reductions)
  (simplify-term term reductions))
