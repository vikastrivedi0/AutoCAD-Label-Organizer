;;; Civil 3D Label Overlap Detector - Delete Overlapping Pipe and Structure Labels
;;; This script detects overlapping Civil 3D Pipe and Structure labels and deletes them.
;;; Usage: Load the script and run the command C3D-LABEL-DELETE-OVERLAP

;; Function to get label extents
(defun get-label-extents (ent / entdata pt1 pt2 pt3 pt4 minx miny maxx maxy)
  (setq entdata (entget ent))
  (setq minx 1e99 miny 1e99 maxx -1e99 maxy -1e99)
  
  ;; Iterate through all points in the entity data
  (while entdata
    (if (= 10 (caar entdata))  ; Look for any point data (group code 10)
      (progn
        (setq pt (cdar entdata))
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
    (list (list minx miny 0.0) (list maxx maxy 0.0))
    nil
  )
)

;; Function to process a single label
(defun process-label (ent mtextData / extents)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq extents (get-label-extents ent))
      (if extents
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
        (progn
          ;; Fallback to using entity bounding box
          (command "._zoom" "_object" ent "")
          (setq pt1 (getvar "viewctr"))
          (setq size (getvar "viewsize"))
          (setq mtextData 
            (cons 
              (list 
                ent 
                (list (- (car pt1) (/ size 2)) (- (cadr pt1) (/ size 2)) 0.0)  ; Min point
                (list (+ (car pt1) (/ size 2)) (+ (cadr pt1) (/ size 2)) 0.0)  ; Max point
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

      ;; Remove duplicates from overlap list
      (setq overlapList (LM:remove-duplicates overlapList))

      ;; Delete overlapping labels with validation
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping Civil 3D labels."))
      
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
    (print "\nNo Civil 3D Pipe or Structure labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Delete) loaded. Type C3D-LABEL-DELETE-OVERLAP to run.")
(princ) 