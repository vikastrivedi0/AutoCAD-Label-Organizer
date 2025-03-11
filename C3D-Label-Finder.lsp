;;; Civil 3D Label Overlap Detector - Delete Overlapping Pipe and Structure Labels
;;; This script detects overlapping Civil 3D Pipe and Structure labels and deletes them.
;;; Usage: Load the script and run the command C3D-LABEL-DELETE-OVERLAP

;; Load ActiveX support
(vl-load-com)

(defun c:C3D-LABEL-DELETE-OVERLAP (/ ss1 ss2 ent obj mtextData overlapList count)
  ;; Initialize ActiveX
  (vl-load-com)
  
  ;; Get both Pipe Labels and Structure Labels using the correct entity types
  (setq ss1 (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss2 (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL"))))
  
  (if (or ss1 ss2)
    (progn
      (setq mtextData '())
      (setq overlapList '())

      ;; Process Pipe Labels
      (if ss1
        (progn
          (setq count (sslength ss1))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss1 count))
            (process-label ent mtextData)
          )
        )
      )

      ;; Process Structure Labels
      (if ss2
        (progn
          (setq count (sslength ss2))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss2 count))
            (process-label ent mtextData)
          )
        )
      )

      ;; Function to process a single label
      (defun process-label (ent mtextData / obj bbox)
        (if (and ent (not (null ent)) (entget ent))  ; Validate entity
          (progn
            (setq obj (vlax-ename->vla-object ent))
            (if (and obj (vlax-object-p obj))
              (progn
                ;; Try to get the label's bounding box using GetBoundingBox method
                (setq bbox (vl-catch-all-apply 
                           '(lambda () 
                              (vlax-safearray->list 
                                (vlax-variant-value 
                                  (vlax-invoke-method obj 'GetBoundingBox 'minPoint 'maxPoint))))))
                (if (and bbox (not (vl-catch-all-error-p bbox)))
                  (setq mtextData 
                    (cons 
                      (list 
                        ent 
                        (car bbox)   ; Min point
                        (cadr bbox)  ; Max point
                      )
                      mtextData
                    )
                  )
                  ;; Fallback to entity data if GetBoundingBox fails
                  (progn
                    (setq entdata (entget ent))
                    (if entdata
                      (progn
                        (setq insertion (cdr (assoc 10 entdata)))  ; Insertion point
                        ;; Estimate a default size if we can't get actual dimensions
                        (setq width 10.0)  ; Default width in drawing units
                        (setq height 5.0)  ; Default height in drawing units
                        (if insertion
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
                          (princ (strcat "\nWarning: Could not get insertion point for label " (vl-princ-to-string ent)))
                        )
                      )
                      (princ (strcat "\nWarning: Could not get entity data for " (vl-princ-to-string ent)))
                    )
                  )
                )
              )
              (princ (strcat "\nWarning: Could not create object for label " (vl-princ-to-string ent)))
            )
          )
          (princ (strcat "\nWarning: Invalid entity encountered"))
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
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping Civil 3D labels."))
      
      (setq deleted-count 0)
      (foreach ent overlapList
        (if (and ent (entget ent))  ; Verify entity still exists
          (progn
            (entdel ent)
            (setq deleted-count (1+ deleted-count))
          )
        )
      )

      (print (strcat "\nCompleted. Successfully deleted " (itoa deleted-count) " overlapping Civil 3D labels."))
    )
    (print "\nNo Civil 3D Pipe or Structure labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Delete) loaded. Type C3D-LABEL-DELETE-OVERLAP to run.")
(princ) 