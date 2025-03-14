;;; Civil 3D Label Overlap Detector and Mover - Revised (AeccApplication)
;;; This script detects overlapping AECC_PIPE_LABEL and AECC_STRUCTURE_LABEL labels and moves them.
;;; Usage: Load the script and run the command C3D-LABEL-MOVE

(defun c:C3D-LABEL-MOVE (/ doc acad civilApp civilDoc pipeLabels structLabels allLabels overlapList moveDist)
  (vl-load-com)

  ;; Get AutoCAD application object
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-activedocument acad))

  ;; Get Civil 3D application object
  (setq civilApp
    (vl-catch-all-apply
      '(lambda ()
         (vlax-get-or-create-object "AeccXUiLand.AeccApplication.15.0")
       )
    )
  )

  (if (or (not civilApp) (eq (type civilApp) 'ERROR))
    (progn
      (princ "\nCivil 3D Application not found. Exiting.")
      (princ)
      (exit)
    )
  )

  ;; Get Civil 3D document
  (setq civilDoc
    (vl-catch-all-apply
      '(lambda ()
         (vla-get-ActiveDocument civilApp)
       )
    )
  )

  (if (or (not civilDoc) (eq (type civilDoc) 'ERROR))
    (progn
      (princ "\nCivil 3D Document not found. Exiting.")
      (princ)
      (exit)
    )
  )

  ;; Start a transaction
  (vla-StartUndoMark doc)

  ;; Initialize lists
  (setq pipeLabels '())
  (setq structLabels '())
  (setq allLabels '())
  (setq overlapList '())

  ;; Get pipe labels
  (setq ss (ssget "_X" '((0 . "AECC_PIPE_LABEL"))))
  (if ss
    (repeat (setq count (sslength ss))
      (setq ent (ssname ss (setq count (1- count))))
      (setq obj (vlax-ename->vla-object ent))
      (setq pipeLabels (cons obj pipeLabels))
      (setq allLabels (cons obj allLabels))
    )
  )

  ;; Get structure labels
  (setq ss (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL"))))
  (if ss
    (repeat (setq count (sslength ss))
      (setq ent (ssname ss (setq count (1- count))))
      (setq obj (vlax-ename->vla-object ent))
      (setq structLabels (cons obj structLabels))
      (setq allLabels (cons obj allLabels))
    )
  )

  ;; Function to get label extents
  (defun get-label-extents (labelObj / ext minPt maxPt)
    (vl-catch-all-apply
      '(lambda ()
         (setq ext (vla-get-GeometricExtents labelObj))
         (setq minPt (vlax-safearray->list (vlax-variant-value (vla-get-MinPoint ext))))
         (setq maxPt (vlax-safearray->list (vlax-variant-value (vla-get-MaxPoint ext))))
         (list minPt maxPt)
      )
    )
  )

  ;; Function to check if two labels overlap
  (defun check-overlap (label1 label2 / ext1 ext2 minPt1 maxPt1 minPt2 maxPt2)
    (setq ext1 (get-label-extents label1))
    (setq ext2 (get-label-extents label2))
    (setq minPt1 (car ext1))
    (setq maxPt1 (cadr ext1))
    (setq minPt2 (car ext2))
    (setq maxPt2 (cadr ext2))

    (not (or (< (nth 0 maxPt1) (nth 0 minPt2))
             (< (nth 0 maxPt2) (nth 0 minPt1))
             (< (nth 1 maxPt1) (nth 1 minPt2))
             (< (nth 1 maxPt2) (nth 1 minPt1))
        )
    )
  )

  ;; Check for overlaps
  (foreach label1 allLabels
    (foreach label2 allLabels
      (if (and (not (eq label1 label2))
               (check-overlap label1 label2))
        (progn
          (setq overlapList (cons label1 overlapList))
          (setq overlapList (cons label2 overlapList))
        )
      )
    )
  )

  ;; Function to remove duplicates from a list (by Lee Mac)
  (defun LM:remove-duplicates (lst / rtn)
    (foreach itm lst (if (not (member itm rtn)) (setq rtn (cons itm rtn))))
    (reverse rtn)
  )

  ;; Remove duplicates from overlap list
  (setq overlapList (LM:remove-duplicates overlapList))

  ;; Move overlapping labels
  (setq moveDist 10.0) ; Adjust this value as needed

  (print (strcat "\nMoving " (itoa (length overlapList)) " overlapping labels..."))

  (foreach labelObj overlapList
    (vl-catch-all-apply
      '(lambda ()
         (setq insPt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint labelObj))))
         (vla-put-InsertionPoint labelObj (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble '(0 . 2)) (list (+ (nth 0 insPt) moveDist) (nth 1 insPt) (nth 2 insPt)))))
      )
    )
  )

  (print (strcat "\nCompleted. Moved " (itoa (length overlapList)) " overlapping labels."))

  ;; End the transaction
  (vla-EndUndoMark doc)
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Move) loaded. Type C3D-LABEL-MOVE to run.")
(princ)