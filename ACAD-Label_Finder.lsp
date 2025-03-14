;;; AutoCAD Label Overlap Detector (MTEXT) - Move Overlapping Labels with Precise Overlap Calculation
;;; This script detects overlapping MTEXT labels and moves them by the exact overlap distance.
;;; Usage: Load the script and run the command ACAD-MTEXT-MOVE-OVERLAP

;; Load ActiveX support
(vl-load-com)

;; Function to calculate expanded bounding box for an MTEXT entity
(defun get-expanded-bbox (entdata / insertion width height)
  (if entdata
    (progn
      (setq insertion (cdr (assoc 10 entdata)))  ; Insertion point
      (setq width (cdr (assoc 41 entdata)))      ; Width
      (setq height (cdr (assoc 43 entdata)))     ; Height
      (if (and insertion width height)
        (list
          ;; Min point (expanded)
          (list (- (car insertion) width)  ; Left
                (- (cadr insertion) height) ; Bottom
                (caddr insertion))          ; Z
          ;; Max point (expanded)
          (list (+ (car insertion) (* 2 width))   ; Right
                (+ (cadr insertion) (* 2 height))  ; Top
                (caddr insertion))                 ; Z
        )
        nil
      )
    )
    nil
  )
)

;; Function to check if a point is inside a bounding box
(defun point-in-bbox (point bbox / minPt maxPt)
  (if (and point bbox (cadr bbox) (caddr bbox))
    (progn
      (setq minPt (cadr bbox)
            maxPt (caddr bbox))
      (and
        (>= (car point) (car minPt))
        (<= (car point) (car maxPt))
        (>= (cadr point) (cadr minPt))
        (<= (cadr point) (cadr maxPt))
      )
    )
    nil
  )
)

;; Function to count overlaps in each direction
(defun count-directional-overlaps (ent bbox mtextData / count-left count-right count-up count-down)
  (setq count-left 0
        count-right 0
        count-up 0
        count-down 0)
  
  (foreach data mtextData
    (if (and (not (eq (car data) ent))
             (point-in-bbox (cadr data) bbox))
      (progn
        ;; Count overlaps in each direction
        (if (< (car (cadr data)) (car (cadr bbox)))
          (setq count-left (1+ count-left)))
        (if (> (car (cadr data)) (car (cadr bbox)))
          (setq count-right (1+ count-right)))
        (if (< (cadr (cadr data)) (cadr (cadr bbox)))
          (setq count-down (1+ count-down)))
        (if (> (cadr (cadr data)) (cadr (cadr bbox)))
          (setq count-up (1+ count-up)))
      )
    )
  )
  
  ;; Return list of (direction count) pairs
  (list
    (list "left" count-left)
    (list "right" count-right)
    (list "up" count-up)
    (list "down" count-down)
  )
)

(defun c:ACAD-MTEXT-MOVE-OVERLAP-SMART (/ ss ent obj mtextData count)
  ;; Initialize ActiveX
  (vl-load-com)
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq moved-count 0)

      ;; Collect all MTEXT data
      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)))
          (progn
            (setq entdata (entget ent))
            (if entdata
              (setq mtextData (cons (list ent entdata) mtextData))
            )
          )
        )
      )

      (print (strcat "\nProcessing " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Process each label
      (foreach label-data mtextData
        (setq ent (car label-data)
              entdata (cadr label-data))
        
        ;; Get expanded bounding box
        (setq expanded-bbox (get-expanded-bbox entdata))
        (if expanded-bbox
          (progn
            ;; Count overlaps in each direction
            (setq overlap-counts (count-directional-overlaps ent expanded-bbox mtextData))
            
            ;; Find direction with least overlaps
            (setq best-direction nil
                  min-overlaps most-positive-fixnum)
            (foreach dir-count overlap-counts
              (if (< (cadr dir-count) min-overlaps)
                (progn
                  (setq min-overlaps (cadr dir-count))
                  (setq best-direction (car dir-count))
                )
              )
            )
            
            ;; Calculate movement distance (use width or height as base)
            (setq width (cdr (assoc 41 entdata))
                  height (cdr (assoc 43 entdata))
                  move-distance (max width height))
            
            ;; Calculate new position based on best direction
            (setq current-pos (cdr (assoc 10 entdata))
                  new-pos current-pos)
            
            (cond
              ((= best-direction "left")
               (setq new-pos (list (- (car current-pos) move-distance)
                                 (cadr current-pos)
                                 (caddr current-pos))))
              ((= best-direction "right")
               (setq new-pos (list (+ (car current-pos) move-distance)
                                 (cadr current-pos)
                                 (caddr current-pos))))
              ((= best-direction "up")
               (setq new-pos (list (car current-pos)
                                 (+ (cadr current-pos) move-distance)
                                 (caddr current-pos))))
              ((= best-direction "down")
               (setq new-pos (list (car current-pos)
                                 (- (cadr current-pos) move-distance)
                                 (caddr current-pos))))
            )
            
            ;; Move the label
            (command "._move" ent "" current-pos new-pos)
            (setq moved-count (1+ moved-count))
            
            ;; Print debug information
            (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                          " " best-direction " by " 
                          (rtos move-distance) " units"))
          )
        )
      )

      (print (strcat "\nCompleted. Successfully moved " (itoa moved-count) " MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Label Overlap Detector (Smart Move) loaded. Type ACAD-MTEXT-MOVE-OVERLAP-SMART to run.")
(princ) 