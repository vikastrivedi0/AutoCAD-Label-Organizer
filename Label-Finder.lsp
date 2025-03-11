;;; AutoCAD Label Overlap Detector (MTEXT) - Delete Overlapping Labels (Textbox Approach)
;;; This script detects overlapping MTEXT labels and deletes them.
;;; Usage: Load the script and run the command ACAD-MTEXT-DELETE-OVERLAP

(defun c:ACAD-MTEXT-DELETE-OVERLAP (/ ss ent obj mtextData overlapList)
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq overlapList '())

      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (setq obj (vlax-ename->vla-object ent))
        (setq tb (textbox ent))
        (setq mtextData (cons (list ent (car tb) (cadr tb)) mtextData))
      )

      ;; Function to check if two bounding boxes overlap
      (defun check-overlap (data1 data2 / minPt1 maxPt1 minPt2 maxPt2)
        (setq minPt1 (cadr data1)
              maxPt1 (caddr data1)
              minPt2 (cadr data2)
              maxPt2 (caddr data2))

        (not (or (< (car maxPt1) (car minPt2))
                 (< (car maxPt2) (car minPt1))
                 (< (cadr maxPt1) (cadr minPt2))
                 (< (cadr maxPt2) (cadr minPt1))
            )
        )
      )

      ;; Check for overlaps
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))
                   (check-overlap data1 data2))
            (progn
              (setq overlapList (cons (car data1) overlapList))
              (setq overlapList (cons (car data2) overlapList))
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

      ;; Delete overlapping labels
      (print (strcat "\nDeleting " (itoa (length overlapList)) " overlapping MTEXT labels..."))

      (foreach ent overlapList
        (entdel ent)
      )

      (print (strcat "\nCompleted. Deleted " (itoa (length overlapList)) " overlapping MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Label Overlap Detector (Delete) loaded. Type ACAD-MTEXT-DELETE-OVERLAP to run.")
(princ)