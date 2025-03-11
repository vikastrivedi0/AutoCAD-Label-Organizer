;;; Civil 3D Label Overlap Detector - Delete Overlapping Pipe and Structure Labels
;;; This script detects overlapping Civil 3D Pipe and Structure labels and deletes them.
;;; Usage: Load the script and run the command C3D-LABEL-DELETE-OVERLAP

;; Function to get label insertion point
(defun get-label-insertion (ent / entdata)
  (setq entdata (entget ent))
  (cdr (assoc 10 entdata))  ; Get insertion point (group code 10)
)

;; Function to calculate distance between two points
(defun calc-distance (pt1 pt2)
  (sqrt (+ (expt (- (car pt2) (car pt1)) 2)
           (expt (- (cadr pt2) (cadr pt1)) 2)))
)

;; Function to check if two labels are too close
(defun check-overlap (data1 data2 / pt1 pt2 dist threshold)
  (if (and data1 data2)
    (progn
      (setq pt1 (cadr data1)
            pt2 (cadr data2))
      
      ;; Set threshold to 5 drawing units
      (setq threshold 5.0)
      
      ;; Calculate distance between labels
      (setq dist (calc-distance pt1 pt2))
      
      ;; Debug output
      (princ (strcat "\n\nComparing labels:"))
      (princ (strcat "\nLabel 1 position: " (vl-princ-to-string pt1)))
      (princ (strcat "\nLabel 2 position: " (vl-princ-to-string pt2)))
      (princ (strcat "\nDistance: " (rtos dist)))
      (princ (strcat "\nThreshold: " (rtos threshold)))
      
      ;; Return true if distance is less than threshold
      (if (< dist threshold)
        (progn
          (princ "\nOverlap detected!")
          T
        )
        (progn
          (princ "\nNo overlap.")
          nil
        )
      )
    )
    nil
  )
)

;; Function to process a single label
(defun process-label (ent mtextData / insertion-pt)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq insertion-pt (get-label-insertion ent))
      (if insertion-pt
        (progn
          (princ (strcat "\nProcessed label at position: " (vl-princ-to-string insertion-pt)))
          (setq mtextData 
            (cons 
              (list 
                ent 
                insertion-pt
              )
              mtextData
            )
          )
        )
        (princ (strcat "\nWarning: Could not get insertion point for entity " (vl-princ-to-string ent)))
      )
    )
    (princ (strcat "\nWarning: Invalid entity encountered"))
  )
  mtextData  ; Return the updated mtextData
)

;; Function to remove duplicates from a list (by Lee Mac)
(defun LM:remove-duplicates (lst / rtn)
  (foreach itm lst (if (not (member itm rtn)) (setq rtn (cons itm rtn))))
  (reverse rtn)
)

(defun c:C3D-LABEL-DELETE-OVERLAP (/ ss1 ss2 ent mtextData overlapList count)
  (princ "\nSearching for Civil 3D labels...")
  
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
          (princ (strcat "\nProcessing " (itoa (sslength ss1)) " Pipe Labels..."))
          (setq count (sslength ss1))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss1 count))
            (setq mtextData (process-label ent mtextData))
          )
        )
      )

      ;; Process Structure Labels
      (if ss2
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss2)) " Structure Labels..."))
          (setq count (sslength ss2))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss2 count))
            (setq mtextData (process-label ent mtextData))
          )
        )
      )

      (princ "\nChecking for overlaps...")
      
      ;; Check for overlaps with error handling
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))  ; Not the same label
                   (check-overlap data1 data2)          ; Labels are too close
                   (entget (car data1))                 ; Verify entities still exist
                   (entget (car data2)))
            (progn
              ;; Only add the second label to the overlap list if it's not already there
              (if (not (member (car data2) overlapList))
                (progn
                  (princ (strcat "\nFound overlapping labels"))
                  (setq overlapList (cons (car data2) overlapList))
                )
              )
            )
          )
        )
      )

      ;; Remove duplicates from overlap list
      (setq overlapList (LM:remove-duplicates overlapList))

      ;; Delete overlapping labels with validation
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping Civil 3D labels."))
      
      (if (> (length overlapList) 0)
        (progn
          (princ "\nPress Enter to delete overlapping labels, or Esc to cancel...")
          (setq user-input (getstring))
          (if (= user-input "")  ; Check for empty string (Enter key)
            (progn
              (setq deleted-count 0)
              (foreach ent overlapList
                (if (and ent (entget ent))  ; Verify entity still exist
                  (progn
                    (entdel ent)
                    (setq deleted-count (1+ deleted-count))
                  )
                )
              )
              (print (strcat "\nCompleted. Successfully deleted " (itoa deleted-count) " overlapping Civil 3D labels."))
            )
            (print "\nOperation cancelled by user.")
          )
        )
        (print "\nNo overlapping labels found.")
      )
    )
    (print "\nNo Civil 3D Pipe or Structure labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Delete) loaded. Type C3D-LABEL-DELETE-OVERLAP to run.")
(princ) 