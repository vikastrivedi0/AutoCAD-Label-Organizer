;;; AutoCAD Label Overlap Detector (MTEXT) - Move Overlapping Labels with Precise Overlap Calculation
;;; This script detects overlapping MTEXT labels and moves them by the exact overlap distance.
;;; Usage: Load the script and run the command ACAD-MTEXT-MOVE-OVERLAP

;; Load ActiveX support
(vl-load-com)

;; Function to calculate overlap distance and direction between two bounding boxes
(defun calculate-overlap-distance (data1 data2 / minPt1 maxPt1 minPt2 maxPt2)
  (if (and data1 data2 (cadr data1) (caddr data1) (cadr data2) (caddr data2))
    (progn
      (setq minPt1 (cadr data1)
            maxPt1 (caddr data1)
            minPt2 (cadr data2)
            maxPt2 (caddr data2))
      
      ;; Calculate overlap in X direction
      (setq x-overlap (min (- (car maxPt1) (car minPt2))
                          (- (car maxPt2) (car minPt1))))
      
      ;; Calculate overlap in Y direction
      (setq y-overlap (min (- (cadr maxPt1) (cadr minPt2))
                          (- (cadr maxPt2) (cadr minPt1))))
      
      ;; Determine direction based on relative positions
      (setq direction 1.0)  ; Default to moving right
      (if (< (car minPt1) (car minPt2))
        (setq direction -1.0)  ; Move left if this label is to the left of the other
      )
      
      ;; Return list of (distance direction)
      (list (+ (max x-overlap y-overlap) 1.0) direction)
    )
    (list 0.0 1.0)  ; Default to no overlap and moving right
  )
)

(defun c:ACAD-MTEXT-MOVE-OVERLAP (/ ss ent obj mtextData overlapList count)
  ;; Initialize ActiveX
  (vl-load-com)
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq overlapList '())
      (setq overlap-pairs '())  ; List to store pairs of overlapping labels and their overlap distances

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

      ;; Check for overlaps and calculate overlap distances
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))
                   (check-overlap data1 data2)
                   (entget (car data1))  ; Verify entities still exist
                   (entget (car data2)))
            (progn
              ;; Calculate overlap distance and direction
              (setq overlap-info (calculate-overlap-distance data1 data2))
              (setq overlap-dist (car overlap-info))
              (setq direction (cadr overlap-info))
              
              ;; Store the pair and their overlap distance and direction
              (setq overlap-pairs 
                (cons 
                  (list (car data1) (car data2) overlap-dist direction)
                  overlap-pairs
                )
              )
              
              ;; Add to overlap list if not already there
              (if (not (member (car data1) overlapList))
                (setq overlapList (cons (car data1) overlapList))
              )
              (if (not (member (car data2) overlapList))
                (setq overlapList (cons (car data2) overlapList))
              )
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

      ;; Move overlapping labels with validation
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping MTEXT labels."))
      
      (setq moved-count 0)
      (foreach ent overlapList
        (if (and ent (entget ent))  ; Verify entity still exists
          (progn
            ;; Get current position
            (setq entdata (entget ent))
            (setq current-pos (cdr (assoc 10 entdata)))
            
            ;; Find the maximum overlap distance and direction for this label
            (setq max-overlap 0.0)
            (setq move-direction 1.0)  ; Default to moving right
            (foreach pair overlap-pairs
              (if (or (eq (car pair) ent) (eq (cadr pair) ent))
                (progn
                  (if (> (caddr pair) max-overlap)
                    (progn
                      (setq max-overlap (caddr pair))
                      (setq move-direction (cadddr pair))
                    )
                  )
                )
              )
            )
            
            ;; Calculate new position (offset by maximum overlap distance in the appropriate direction)
            (setq new-pos (list (+ (car current-pos) (* max-overlap move-direction))
                               (cadr current-pos)
                               (caddr current-pos)))
            
            ;; Move the label
            (command "._move" ent "" current-pos new-pos)
            (setq moved-count (1+ moved-count))
            
            ;; Print debug information
            (princ (strcat "\nMoved label by " (rtos max-overlap) " units " 
                          (if (< move-direction 0) "left" "right")))
          )
        )
      )

      (print (strcat "\nCompleted. Successfully moved " (itoa moved-count) " overlapping MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Label Overlap Detector (Move) loaded. Type ACAD-MTEXT-MOVE-OVERLAP to run.")
(princ) 