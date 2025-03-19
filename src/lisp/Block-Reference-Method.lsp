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

;; Function to get block extents with all four corners
(defun get-block-extents (ent / block-data min-pt max-pt width height entdata insertion-pt)
  (command "._zoom" "_object" ent "")
  (setq min-pt (getvar "viewctr"))
  (setq width (getvar "viewsize"))
  (setq height (getvar "viewsize"))
  (setq max-pt (list (+ (car min-pt) width)
                     (+ (cadr min-pt) height)
                     0.0))
  
  ;; Calculate all four corners
  (setq bottom-left min-pt
        bottom-right (list (car max-pt) (cadr min-pt) 0.0)
        top-right max-pt
        top-left (list (car min-pt) (cadr max-pt) 0.0))
  
  ;; Get insertion point from entity data instead of ActiveX
  (setq entdata (entget ent))
  (if entdata
    (setq insertion-pt (cdr (assoc 10 entdata)))  ; Get insertion point from group code 10
    (setq insertion-pt (list 0 0 0))  ; Default if not found
  )
  
  (command "._zoom" "_previous")
  (list bottom-left bottom-right top-right top-left insertion-pt)
)

;; Function to check if line segments intersect
(defun lines-intersect (p1 p2 p3 p4 / denominator ua ub)
  (setq denominator (- (* (- (cadr p4) (cadr p3)) (- (car p2) (car p1)))
                      (* (- (car p4) (car p3)) (- (cadr p2) (cadr p1)))))
  
  (if (not (equal denominator 0.0 1e-10))
    (progn
      (setq ua (/ (- (* (- (car p4) (car p3)) (- (cadr p1) (cadr p3)))
                    (* (- (cadr p4) (cadr p3)) (- (car p1) (car p3))))
                 denominator))
      (setq ub (/ (- (* (- (car p2) (car p1)) (- (cadr p1) (cadr p3)))
                    (* (- (cadr p2) (cadr p1)) (- (car p1) (car p3))))
                 denominator))
      (and (>= ua 0.0) (<= ua 1.0) (>= ub 0.0) (<= ub 1.0))
    )
    nil
  )
)

;; Function to check if polygons overlap
(defun polygons-overlap (corners1 corners2 / i j)
  ;; First check if any line segments intersect
  (setq i 0)
  (while (< i 4)
    (setq j 0)
    (while (< j 4)
      (if (lines-intersect (nth i corners1)
                          (nth (rem (1+ i) 4) corners1)
                          (nth j corners2)
                          (nth (rem (1+ j) 4) corners2))
        (progn
          (princ "\nIntersection found between segments:")
          (princ (strcat "\n  Segment 1: (" 
                        (rtos (car (nth i corners1)) 2 2) ", "
                        (rtos (cadr (nth i corners1)) 2 2) ") to ("
                        (rtos (car (nth (rem (1+ i) 4) corners1)) 2 2) ", "
                        (rtos (cadr (nth (rem (1+ i) 4) corners1)) 2 2) ")"))
          (princ (strcat "\n  Segment 2: (" 
                        (rtos (car (nth j corners2)) 2 2) ", "
                        (rtos (cadr (nth j corners2)) 2 2) ") to ("
                        (rtos (car (nth (rem (1+ j) 4) corners2)) 2 2) ", "
                        (rtos (cadr (nth (rem (1+ j) 4) corners2)) 2 2) ")"))
          T
        )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  
  ;; Then check if one polygon is completely inside the other
  (or (point-in-polygon (nth 0 corners1) corners2)
      (point-in-polygon (nth 0 corners2) corners1))
)

;; Function to get first n elements of a list
(defun get-first-n (lst n / result count)
  (setq result '()
        count 0)
  (while (and lst (< count n))
    (setq result (append result (list (car lst))))
    (setq lst (cdr lst))
    (setq count (1+ count))
  )
  result
)

;; Function to check if two blocks overlap
(defun check-block-overlap (data1 data2 / corners1 corners2)
  ;; Debug output
  (princ "\n=== Debug Information ===")
  (princ "\nChecking overlap between two labels:")
  (princ "\nLabel 1 corners:")
  (foreach pt (car data1)
    (princ (strcat "\n  Point: (" 
                   (rtos (car pt) 2 2) ", "
                   (rtos (cadr pt) 2 2) ", "
                   (rtos (caddr pt) 2 2) ")")))
  
  (princ "\nLabel 2 corners:")
  (foreach pt (car data2)
    (princ (strcat "\n  Point: (" 
                   (rtos (car pt) 2 2) ", "
                   (rtos (cadr pt) 2 2) ", "
                   (rtos (caddr pt) 2 2) ")")))
  
  ;; Get corners (first 4 elements, excluding insertion point which is last)
  (setq corners1 (car data1)
        corners2 (car data2))
  
  ;; Check for overlap
  (setq result (polygons-overlap corners1 corners2))
  
  (princ (strcat "\nOverlap detected: " (if result "Yes" "No")))
  (princ "\n=====================")
  
  result
)

;; Function to process a single label
(defun process-label (ent mtextData / block-data)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq block-data (get-block-extents ent))
      (if block-data
        (progn
          (princ "\nProcessed label corners:")
          (foreach pt (get-first-n block-data 4)
            (princ (strcat "\n  (" 
                          (rtos (car pt) 2 2) ", "
                          (rtos (cadr pt) 2 2) ", "
                          (rtos (caddr pt) 2 2) ")")))
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

;; Function to write data to file
(defun write-to-file (filename data / file)
  (setq file (open filename "w"))
  (foreach item data
    (write-line item file))
  (close file))

;; Function to format point as string
(defun point-to-string (pt)
  (strcat (rtos (car pt) 2 8) "," (rtos (cadr pt) 2 8) "," (rtos (caddr pt) 2 8)))

(defun c:C3D-EXPORT-LABELS ( / ss1 ss2 ent label-data csv-data)
  (princ "\nExporting Civil 3D label data...")
  
  ;; Get both Pipe Labels and Structure Labels
  (setq ss1 (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss2 (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL")))
        label-data '()
        csv-data '())
  
  ;; Add CSV header
  (setq csv-data (cons "id,type,bl_x,bl_y,bl_z,br_x,br_y,br_z,tr_x,tr_y,tr_z,tl_x,tl_y,tl_z,ins_x,ins_y,ins_z" csv-data))
  
  (if (or ss1 ss2)
    (progn
      ;; Process Pipe Labels
      (if ss1
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss1)) " Pipe Labels..."))
          (setq count (sslength ss1))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss1 count))
            (if ent
              (progn
                (vl-catch-all-apply
                  '(lambda ()
                    (setq corners (get-block-extents ent))
                    (if corners
                      (progn
                        ;; Get handle using entget
                        (setq handle (cdr (assoc 5 (entget ent))))
                        (setq csv-line (strcat
                          handle ","                        ; ID
                          "PIPE,"                          ; Type
                          (point-to-string (nth 0 corners)) "," ; Bottom-left
                          (point-to-string (nth 1 corners)) "," ; Bottom-right
                          (point-to-string (nth 2 corners)) "," ; Top-right
                          (point-to-string (nth 3 corners)) "," ; Top-left
                          (point-to-string (nth 4 corners))     ; Insertion point
                        ))
                        (setq csv-data (cons csv-line csv-data))
                        (princ (strcat "\nProcessed PIPE label with handle: " handle))
                      )
                    )
                  )
                )
              )
            )
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
            (if ent
              (progn
                (vl-catch-all-apply
                  '(lambda ()
                    (setq corners (get-block-extents ent))
                    (if corners
                      (progn
                        ;; Get handle using entget
                        (setq handle (cdr (assoc 5 (entget ent))))
                        (setq csv-line (strcat
                          handle ","                        ; ID
                          "STRUCTURE,"                      ; Type
                          (point-to-string (nth 0 corners)) "," ; Bottom-left
                          (point-to-string (nth 1 corners)) "," ; Bottom-right
                          (point-to-string (nth 2 corners)) "," ; Top-right
                          (point-to-string (nth 3 corners)) "," ; Top-left
                          (point-to-string (nth 4 corners))     ; Insertion point
                        ))
                        (setq csv-data (cons csv-line csv-data))
                        (princ (strcat "\nProcessed STRUCTURE label with handle: " handle))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; Write data to CSV file
      (write-to-file "label_data.csv" (reverse csv-data))
      (princ "\nLabel data exported to label_data.csv")
    )
    (princ "\nNo Civil 3D labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Block Reference Method) loaded. Type C3D-BLOCK-OVERLAP to run.")
(princ) 