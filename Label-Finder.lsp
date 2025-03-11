;;; AutoCAD Label Overlap Detector (MTEXT) - Delete Overlapping Labels (Textbox Approach)
;;; This script detects overlapping MTEXT labels and deletes them.
;;; Usage: Load the script and run the command ACAD-MTEXT-DELETE-OVERLAP

;; Load ActiveX support
(vl-load-com)

(defun c:ACAD-MTEXT-DELETE-OVERLAP (/ ss ent obj mtextData overlapList count)
  ;; Initialize ActiveX
  (vl-load-com)
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq overlapList '())

      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)) (entget ent))  ; Validate entity
          (progn
            ;; Try to get textbox directly if ActiveX is not available
            (setq tb (vl-catch-all-apply 'textbox (list ent)))
            (if (and tb (not (vl-catch-all-error-p tb)))
              (setq mtextData (cons (list ent (car tb) (cadr tb)) mtextData))
              ;; If textbox fails, try to get bounding box from entity data
              (progn
                (setq entdata (entget ent))
                (if entdata
                  (progn
                    (setq insertion (cdr (assoc 10 entdata)))  ; Insertion point
                    (setq width (cdr (assoc 41 entdata)))      ; Width
                    (setq height (cdr (assoc 43 entdata)))     ; Height
                    (if (and insertion width height)
                      (setq mtextData 
                        (cons 
                          (list 
                            ent 
                            insertion  ; Min point
                            (list     ; Max point
                              (+ (car insertion) width)
                              (+ (cadr insertion) height)
                              (caddr insertion)
                            )
                          )
                          mtextData
                        )
                      )
                      (princ (strcat "\nWarning: Could not get dimensions for entity " (vl-princ-to-string ent)))
                    )
                  )
                  (princ (strcat "\nWarning: Could not get entity data for " (vl-princ-to-string ent)))
                )
              )
            )
          )
          (princ (strcat "\nWarning: Invalid entity encountered at index " (vl-princ-to-string count)))
        )
      )

      ;; Function to check if two bounding boxes overlap
      (defun check-overlap (data1 data2 / minPt1 maxPt1 minPt2 maxPt2)
        (if (and data1 data2 (cadr data1) (caddr data1) (cadr data2) (caddr data2))  ; Validate data
          (progn
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
          nil  ; Return nil if data is invalid
        )
      )

      ;; Check for overlaps with error handling
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))
                   (check-overlap data1 data2)
                   (entget (car data1))  ; Verify entities still exist
                   (entget (car data2)))
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

      ;; Delete overlapping labels with validation
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping MTEXT labels."))
      
      (setq deleted-count 0)
      (foreach ent overlapList
        (if (and ent (entget ent))  ; Verify entity still exists
          (progn
            (entdel ent)
            (setq deleted-count (1+ deleted-count))
          )
        )
      )

      (print (strcat "\nCompleted. Successfully deleted " (itoa deleted-count) " overlapping MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Label Overlap Detector (Delete) loaded. Type ACAD-MTEXT-DELETE-OVERLAP to run.")
(princ)