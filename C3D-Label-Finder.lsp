;;; Civil 3D Label Overlap Detector - Delete Overlapping Pipe and Structure Labels
;;; This script detects overlapping Civil 3D Pipe and Structure labels and deletes them.
;;; Usage: Load the script and run the command C3D-LABEL-DELETE-OVERLAP

;; Function to get label extents
(defun get-label-extents (ent / entdata pt1 pt2 pt3 pt4 minx miny maxx maxy)
  (setq entdata (entget ent))
  (setq minx 1e99 miny 1e99 maxx -1e99 maxy -1e99)
  
  ;; Debug output for entity data
  (princ (strcat "\nProcessing entity: " (vl-princ-to-string ent)))
  
  ;; Iterate through all points in the entity data
  (while entdata
    (if (= 10 (caar entdata))  ; Look for any point data (group code 10)
      (progn
        (setq pt (cdar entdata))
        (princ (strcat "\nFound point: " (vl-princ-to-string pt)))
        (if (< (car pt) minx) (setq minx (car pt)))
        (if (< (cadr pt) miny) (setq miny (cadr pt)))
        (if (> (car pt) maxx) (setq maxx (car pt)))
        (if (> (cadr pt) maxy) (setq maxy (cadr pt)))
      )
    )
    (setq entdata (cdr entdata))
  )
  
  ;; If we found any points, return the extents
  (if (and (/= minx 1e99) (/= miny 1e99) (/= maxx -1e99) (/= maxy -1e99))
    (progn
      (princ (strcat "\nExtents found - Min: (" (rtos minx) "," (rtos miny) ") Max: (" (rtos maxx) "," (rtos maxy) ")"))
      (list (list minx miny 0.0) (list maxx maxy 0.0))
    )
    (progn
      (princ "\nNo points found in entity data, trying fallback method...")
      nil
    )
  )
)

;; Function to calculate area of a bounding box
(defun calc-area (minPt maxPt)
  (setq width (abs (- (car maxPt) (car minPt))))
  (setq height (abs (- (cadr maxPt) (cadr minPt))))
  (* width height)
)

;; Function to calculate overlap area
(defun calc-overlap-area (minPt1 maxPt1 minPt2 maxPt2)
  (setq overlap-minx (max (car minPt1) (car minPt2)))
  (setq overlap-miny (max (cadr minPt1) (cadr minPt2)))
  (setq overlap-maxx (min (car maxPt1) (car maxPt2)))
  (setq overlap-maxy (min (cadr maxPt1) (cadr maxPt2)))
  
  (if (and (< overlap-minx overlap-maxx)
           (< overlap-miny overlap-maxy))
    (* (abs (- overlap-maxx overlap-minx))
       (abs (- overlap-maxy overlap-miny)))
    0.0
  )
)

;; Function to check if two bounding boxes overlap significantly
(defun check-overlap (data1 data2 / minPt1 maxPt1 minPt2 maxPt2 area1 area2 overlap-area min-area overlap-threshold)
  (if (and data1 data2 (cadr data1) (caddr data1) (cadr data2) (caddr data2))  ; Validate data
    (progn
      (setq minPt1 (cadr data1)
            maxPt1 (caddr data1)
            minPt2 (cadr data2)
            maxPt2 (caddr data2))
      
      ;; Calculate areas
      (setq area1 (calc-area minPt1 maxPt1))
      (setq area2 (calc-area minPt2 maxPt2))
      
      ;; Only proceed if both areas are valid (non-zero)
      (if (and (> area1 0.0) (> area2 0.0))
        (progn
          (setq overlap-area (calc-overlap-area minPt1 maxPt1 minPt2 maxPt2))
          (setq min-area (min area1 area2))
          (setq overlap-threshold (* min-area 0.25))  ; Reduced threshold to 25%
          
          ;; Debug output
          (princ (strcat "\n\nComparing labels:"))
          (princ (strcat "\nLabel 1 - Min: " (vl-princ-to-string minPt1) " Max: " (vl-princ-to-string maxPt1)))
          (princ (strcat "\nLabel 2 - Min: " (vl-princ-to-string minPt2) " Max: " (vl-princ-to-string maxPt2)))
          (princ (strcat "\nAreas: " (rtos area1) " and " (rtos area2)))
          (princ (strcat "\nOverlap area: " (rtos overlap-area)))
          (princ (strcat "\nThreshold: " (rtos overlap-threshold)))
          
          (if (> overlap-area overlap-threshold)
            (progn
              (princ "\nOverlap detected!")
              T
            )
            (progn
              (princ "\nNo significant overlap.")
              nil
            )
          )
        )
        nil  ; Return nil if either area is zero
      )
    )
    nil  ; Return nil if data is invalid
  )
)

;; Function to process a single label
(defun process-label (ent mtextData / extents)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq extents (get-label-extents ent))
      (if extents
        (progn
          (princ (strcat "\nProcessed label with extents: "))
          (princ (car extents))
          (princ " to ")
          (princ (cadr extents))
          (setq mtextData 
            (cons 
              (list 
                ent 
                (car extents)    ; Min point
                (cadr extents)   ; Max point
              )
              mtextData
            )
          )
        )
        (progn
          ;; Fallback to using entity bounding box
          (princ "\nUsing fallback method to get extents...")
          (command "._zoom" "_object" ent "")
          (setq pt1 (getvar "viewctr"))
          (setq size (getvar "viewsize"))
          (setq min-pt (list (- (car pt1) (/ size 4)) (- (cadr pt1) (/ size 4)) 0.0))
          (setq max-pt (list (+ (car pt1) (/ size 4)) (+ (cadr pt1) (/ size 4)) 0.0))
          (princ (strcat "\nFallback extents - Min: " (vl-princ-to-string min-pt) " Max: " (vl-princ-to-string max-pt)))
          (setq mtextData 
            (cons 
              (list 
                ent 
                min-pt  ; Min point
                max-pt  ; Max point
              )
              mtextData
            )
          )
          (command "._zoom" "_previous")
        )
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
                   (check-overlap data1 data2)          ; Significant overlap
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