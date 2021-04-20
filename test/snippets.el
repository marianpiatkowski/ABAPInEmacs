;;========================================================================
;; test for comparing position in source code
;; given by two lists (linno colno)
;;========================================================================

(abaplib-util-compare-source-code  (list 6 2) (list 4 6));  => nil
(abaplib-util-compare-source-code  (list 4 6) (list 6 6));  => t
(abaplib-util-compare-source-code  (list 4 6) (list 6 4));  => t
(abaplib-util-compare-source-code  (list 6 2) (list 4 1));  => nil
(abaplib-util-compare-source-code  (list 6 6) (list 6 6));  => t

;; sort list of source code positions by first appearance
(-sort 'abaplib-util-compare-source-code  (list (list 6 2) (list 4 6) (list 6 6) (list 6 4) (list 4 1)));
(--sort (abaplib-util-compare-source-code it other)  (list (list 6 2) (list 4 6) (list 6 6) (list 6 4) (list 4 1)));
;; => ((4 1) (4 6) (6 2) (6 4) (6 6))

;;========================================================================
;; append to (end of) list
;;========================================================================

(let ((list1 (list (list 1 2))))
  (-snoc list1 (list 3 4)));

(let ((list1))
  (setq list1 (-snoc list1 (list 1 2)))
  (setq list1 (-snoc list1 (list 3 4)))
  list1);
