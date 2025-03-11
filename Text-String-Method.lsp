;;; Civil 3D Label Overlap Detector - Text String Method
;;; This script detects overlapping Civil 3D labels by analyzing text positions and dimensions.
;;; Usage: Load the script and run the command C3D-TEXT-OVERLAP

;; Function to get text properties from a label
(defun get-text-properties (ent / entdata text-pos text-height width-factor rotation)
  (setq entdata (entget ent))
  (setq text-pos (cdr (assoc 10 entdata)))      ; Text position
  (setq text-height (cdr (assoc 40 entdata)))   ; Text height
  (setq width-factor (cdr (assoc 41 entdata)))  ; Width factor
  (setq rotation (cdr (assoc 50 entdata)))      ; Rotation angle
  
  (if (and text-pos text-height)
    (list text-pos text-height (if width-factor width-factor 1.0) (if rotation rotation 0.0))
    nil
  )
)

;; Function to calculate text bounds
(defun calc-text-bounds (text-props / pos height width-factor rotation width bounds)
  (setq pos (car text-props)
        height (cadr text-props)
        width-factor (caddr text-props)
        rotation (cadddr text-props)
        width (* height width-factor 0.75))  ; Approximate width based on height
  
  ;; Calculate rotated corners
  (setq angle (+ rotation 0.0))  ; Convert to radians if needed
  (setq cos-ang (cos angle)
        sin-ang (sin angle))
  
  ;; Calculate four corners of the text box
  (setq p1 (list (car pos) (cadr pos)))  ; Bottom-left
  (setq p2 (list (+ (car pos) (* width cos-ang))     ; Bottom-right
                 (+ (cadr pos) (* width sin-ang))))
  (setq p3 (list (- (car p2) (* height sin-ang))     ; Top-right
                 (+ (cadr p2) (* height cos-ang))))
  (setq p4 (list (- (car pos) (* height sin-ang))    ; Top-left
                 (+ (cadr pos) (* height cos-ang))))
  
  (list p1 p2 p3 p4)
)

;; Function to check if two text boxes overlap
(defun check-text-overlap (bounds1 bounds2 / i j)
  ;; Use crossing number algorithm to check if any point from one box is inside the other
  (setq overlap nil)
  (foreach pt bounds1
    (if (point-in-polygon pt bounds2)
      (setq overlap T)
    )
  )
  (if (not overlap)
    (foreach pt bounds2
      (if (point-in-polygon pt bounds1)
        (setq overlap T)
      )
    )
  )
  overlap
)

;; Function to check if a point is inside a polygon
(defun point-in-polygon (pt polygon / i j n inside x1 y1 x2 y2)
  (setq n (length polygon)
        inside nil
        j (1- n))
  
  (setq x (car pt)
        y (cadr pt))
  
  (repeat n
    (setq x1 (car (nth i polygon))
          y1 (cadr (nth i polygon))
          x2 (car (nth j polygon))
          y2 (cadr (nth j polygon)))
    
    (if (and (or (> y1 y) (> y2 y))
             (or (< y1 y) (< y2 y))
             (< x (+ x1 (/ (* (- x2 x1) (- y y1)) (- y2 y1)))))
      (setq inside (not inside))
    )
    (setq j i
          i (1+ i))
  )
  inside
)

;; Function to process a single label
(defun process-label (ent mtextData / text-props bounds)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq text-props (get-text-properties ent))
      (if text-props
        (progn
          (setq bounds (calc-text-bounds text-props))
          (princ (strcat "\nProcessed label text bounds: " (vl-princ-to-string bounds)))
          (setq mtextData 
            (cons 
              (list 
                ent 
                bounds
              )
              mtextData
            )
          )
        )
        (princ (strcat "\nWarning: Could not get text properties for entity " (vl-princ-to-string ent)))
      )
    )
    (princ (strcat "\nWarning: Invalid entity encountered"))
  )
  mtextData
)

(defun c:C3D-TEXT-OVERLAP (/ ss1 ss2 ent mtextData overlapList count)
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
                   (check-text-overlap (cadr data1) (cadr data2))  ; Check text bounds overlap
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
(princ "\nCivil 3D Label Overlap Detector (Text String Method) loaded. Type C3D-TEXT-OVERLAP to run.")
(princ) 