;;; Civil 3D Label Overlap Detector - Block Reference Method
;;; This script detects overlapping Civil 3D labels by treating them as block references.
;;; Usage: Load the script and run the command C3D-BLOCK-OVERLAP

;; Function to get block reference data
(defun get-block-data (ent / entdata insertion-pt rotation scale)
  (setq entdata (entget ent))
  (setq insertion-pt (cdr (assoc 10 entdata)))  ; Insertion point
  (setq rotation (cdr (assoc 50 entdata)))      ; Rotation angle
  (setq scale-x (cdr (assoc 41 entdata)))       ; X scale factor
  (setq scale-y (cdr (assoc 42 entdata)))       ; Y scale factor
  
  (if (not rotation) (setq rotation 0.0))
  (if (not scale-x) (setq scale-x 1.0))
  (if (not scale-y) (setq scale-y 1.0))
  
  (list insertion-pt rotation scale-x scale-y)
)

;; Function to get block extents
(defun get-block-extents (ent / block-data min-pt max-pt)
  (command "._zoom" "_object" ent "")
  (setq block-data (get-block-data ent))
  (setq min-pt (getvar "viewctr"))
  (setq max-pt (list (+ (car min-pt) (getvar "viewsize"))
                     (+ (cadr min-pt) (getvar "viewsize"))
                     0.0))
  (command "._zoom" "_previous")
  (list min-pt max-pt (car block-data))  ; Return min point, max point, and insertion point
)

;; Function to check if two blocks overlap
(defun check-block-overlap (data1 data2 / min1 max1 min2 max2 center1 center2 dist threshold)
  ;; Debug output for input data
  (princ "\nData1: ")
  (princ (vl-princ-to-string data1))
  (princ "\nData2: ")
  (princ (vl-princ-to-string data2))

  (if (and data1 data2)
    (progn
      (setq min1 (car data1)
            max1 (cadr data1)
            center1 (caddr data1)
            min2 (car data2)
            max2 (cadr data2)
            center2 (caddr data2))
      
      ;; Validate center points
      (if (and center1 center2 
               (listp center1) (listp center2)
               (>= (length center1) 2) (>= (length center2) 2))
        (progn
          ;; Calculate distance between block centers
          (setq dist (sqrt (+ (expt (- (car center2) (car center1)) 2)
                             (expt (- (cadr center2) (cadr center1)) 2))))
          
          ;; Calculate average block size for threshold
          (setq size1 (sqrt (+ (expt (- (car max1) (car min1)) 2)
                              (expt (- (cadr max1) (cadr min1)) 2))))
          (setq size2 (sqrt (+ (expt (- (car max2) (car min2)) 2)
                              (expt (- (cadr max2) (cadr min2)) 2))))
          
          ;; Set threshold to half of average block size
          (setq threshold (* 0.5 (/ (+ size1 size2) 2)))
          
          ;; Debug output
          (princ (strcat "\nComparing blocks:"))
          (princ (strcat "\nBlock 1 center: " (vl-princ-to-string center1)))
          (princ (strcat "\nBlock 2 center: " (vl-princ-to-string center2)))
          (princ (strcat "\nDistance: " (rtos dist)))
          (princ (strcat "\nThreshold: " (rtos threshold)))
          
          ;; Return true if distance is less than threshold
          (< dist threshold)
        )
        nil  ; Return nil if center points are invalid
      )
    )
    nil  ; Return nil if input data is invalid
  )
)

;; Function to process a single label
(defun process-label (ent mtextData / block-data)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq block-data (get-block-extents ent))
      (if block-data
        (progn
          (princ (strcat "\nProcessed label extents: "))
          (princ (vl-princ-to-string (car block-data)))
          (princ " to ")
          (princ (vl-princ-to-string (cadr block-data)))
          (setq mtextData 
            (cons 
              (list 
                ent 
                block-data
              )
              mtextData
            )
          )
        )
        (princ (strcat "\nWarning: Could not get block data for entity " (vl-princ-to-string ent)))
      )
    )
    (princ (strcat "\nWarning: Invalid entity encountered"))
  )
  mtextData
)

;; Function to remove duplicates from a list (by Lee Mac)
(defun LM:remove-duplicates (lst / rtn)
  (foreach itm lst (if (not (member itm rtn)) (setq rtn (cons itm rtn))))
  (reverse rtn)
)

(defun c:C3D-BLOCK-OVERLAP (/ ss1 ss2 ent mtextData overlapList count)
  (princ "\nSearching for Civil 3D labels...")
  
  ;; Get both Pipe Labels and Structure Labels
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
      
      ;; Check for overlaps
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))  ; Not the same label
                   (check-block-overlap (cadr (cadr data1)) (cadr (cadr data2)))  ; Check blocks overlap
                   (entget (car data1))                 ; Verify entities still exist
                   (entget (car data2)))
            (progn
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
(princ "\nCivil 3D Label Overlap Detector (Block Reference Method) loaded. Type C3D-BLOCK-OVERLAP to run.")
(princ) 