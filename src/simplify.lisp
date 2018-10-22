; simplify
; Jerred Shepherd

; match     = ((X . A))
; reduction = ((+ X 0) (X))
; term      = (+ A 0)

; Given a list of reductions, this function will return the reduction whose pattern matches the term.
; If no reduction matches the term, it will return nil
; Term:       (+ A 0)
; Reductions: (((+ X 0) X))
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
        (cons current-reduction match-result)))))

; Applies a match to a term
; Term:  (X)
; Match: (X . A)
(defun apply-match
  (term match)
  (if
    (null term)
    nil
    (let
      (
        (curr-reduction-term (car term))
        (next-reduction-term (cdr term))
        (match-key (car match))
        (match-value (cdr match)))
      (if
        (equal curr-reduction-term match-key)
        (cons match-value (apply-match next-reduction-term match))
        (cons curr-reduction-term (apply-match next-reduction-term match))))))

; Applies matches to a term
; Term:    (X)
; Matches: ((X . A))
(defun apply-matches
  (term matches)
  (let
    (
      (curr-match (car matches))
      (next-match (cdr matches)))
    (if
      (null next-match)
      (apply-match term curr-match)
      (apply-match (apply-matches term next-match) curr-match))))

; Applies a reduction to a term, and then applies any matches to the reduction
; Reduction: ((+ X 0) X)
; Matches:   ((X . A))
(defun apply-reduction
  (reduction matches)
  (let
    (
      (reduction-result (cdr reduction)))
    (list-to-atom (apply-matches reduction-result matches))))

; Converts a list of a single atom to an atom
(defun list-to-atom
  (term)
  (if
    (null (cdr term))
    (car term)
    term))

(defun not-equal
  (l r)
  (not (equal l r)))

; Reductions should also be a list
; Term:       (+ A 0)
; Reductions: (((+ X 0) X))
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
            (term-after-reduction (apply-reduction (car matched-reduction) (cdr matched-reduction)))
            (new-term (simplify-term term-after-reduction reductions)))
          (if
            (equal term-after-reduction new-term)
            new-term
              (simplify-term new-term reductions)))))))

(defun simplify
  (term reductions)
  (simplify-term term reductions))
