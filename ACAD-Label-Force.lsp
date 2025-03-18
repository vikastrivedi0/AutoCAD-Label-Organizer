;;; AutoCAD Label Force-Directed Placement
;;; This script uses physics-based forces to resolve label overlaps
;;; Usage: Load the script and run the command ACAD-MTEXT-FORCE-PLACE

;; Load ActiveX support
(vl-load-com)

;; Function to calculate distance between two points
(defun point-distance (p1 p2)
  (sqrt (+ (expt (- (car p1) (car p2)) 2)
           (expt (- (cadr p1) (cadr p2)) 2)))
)

;; Function to get bounding box from polyline
(defun get-bbox-from-polyline (ent / entdata points min-x min-y max-x max-y)
  (setq entdata (entget ent))
  (setq points '())
  
  ;; Extract points from polyline
  (while entdata
    (if (= (caar entdata) 10)  ; Vertex point
      (setq points (cons (cdar entdata) points)))
    (setq entdata (cdr entdata)))
  
  ;; Find min and max coordinates
  (setq min-x (car (car points))
        min-y (cadr (car points))
        max-x min-x
        max-y min-y)
  
  (foreach point points
    (setq min-x (min min-x (car point))
          min-y (min min-y (cadr point))
          max-x (max max-x (car point))
          max-y (max max-y (cadr point))))
  
  ;; Return bounding box as (min-point max-point)
  (list (list min-x min-y 0.0)
        (list max-x max-y 0.0))
)

;; Function to check if point is inside bounding box
(defun point-in-bbox (point bbox / min-pt max-pt)
  (setq min-pt (car bbox)
        max-pt (cadr bbox))
  (and
    (>= (car point) (car min-pt))
    (<= (car point) (car max-pt))
    (>= (cadr point) (cadr min-pt))
    (<= (cadr point) (cadr max-pt))
  )
)

;; Function to constrain point to bounding box
(defun constrain-to-bbox (point bbox / min-pt max-pt)
  (setq min-pt (car bbox)
        max-pt (cadr bbox))
  (list
    (max (car min-pt) (min (car point) (car max-pt)))
    (max (cadr min-pt) (min (cadr point) (cadr max-pt)))
    (caddr point)
  )
)

;; Function to calculate safe movement distance
(defun calculate-safe-move (current-pos new-pos bbox / min-pt max-pt)
  (setq min-pt (car bbox)
        max-pt (cadr bbox))
  
  ;; Calculate distances to boundaries
  (setq dist-to-left (- (car current-pos) (car min-pt))
        dist-to-right (- (car max-pt) (car current-pos))
        dist-to-bottom (- (cadr current-pos) (cadr min-pt))
        dist-to-top (- (cadr max-pt) (cadr current-pos)))
  
  ;; Calculate proposed movement
  (setq dx (- (car new-pos) (car current-pos))
        dy (- (cadr new-pos) (cadr current-pos)))
  
  ;; Calculate safe movement factors
  (setq x-factor 1.0
        y-factor 1.0)
  
  ;; Check X movement
  (if (> dx 0)  ; Moving right
    (setq x-factor (min 1.0 (/ dist-to-right dx)))
    (if (< dx 0)  ; Moving left
      (setq x-factor (min 1.0 (/ dist-to-left (- dx))))))
  
  ;; Check Y movement
  (if (> dy 0)  ; Moving up
    (setq y-factor (min 1.0 (/ dist-to-top dy)))
    (if (< dy 0)  ; Moving down
      (setq y-factor (min 1.0 (/ dist-to-bottom (- dy))))))
  
  ;; Return safe movement
  (list (* dx x-factor)
        (* dy y-factor))
)

;; Function to calculate repulsive force between two labels
(defun calculate-repulsion (p1 p2 min-distance repulsion-strength)
  (setq dist (point-distance p1 p2))
  (if (< dist min-distance)
    (setq dist min-distance))
  (setq force (/ repulsion-strength (* dist dist)))
  (list (* force (/ (- (car p1) (car p2)) dist))
        (* force (/ (- (cadr p1) (cadr p2)) dist)))
)

;; Function to calculate attractive force to original position
(defun calculate-attraction (current original attraction-strength)
  (list (* attraction-strength (- (car original) (car current)))
        (* attraction-strength (- (cadr original) (cadr current))))
)

;; Function to get direction of force
(defun get-force-direction (force)
  (setq x-force (abs (car force))
        y-force (abs (cadr force)))
  (cond
    ((> x-force y-force)  ; Stronger horizontal force
     (if (> (car force) 0) "right" "left"))
    ((> y-force x-force)  ; Stronger vertical force
     (if (> (cadr force) 0) "up" "down"))
    (t "none")  ; Equal forces or very small
  )
)

;; Function to apply force-directed placement
(defun c:ACAD-MTEXT-FORCE-PLACE (/ ss ent obj mtextData count iterations bbox-ent bbox)
  ;; Initialize parameters
  (setq iterations 10
        repulsion-strength 5.0
        attraction-strength 3.0
        damping 0.9
        min-distance 5.0)
  
  ;; Initialize ActiveX
  (vl-load-com)
  
  ;; Get bounding box from user
  (princ "\nSelect the bounding box polyline: ")
  (if (setq bbox-ent (entsel))
    (progn
      (setq bbox-ent (car bbox-ent))
      (if (= (cdr (assoc 0 (entget bbox-ent))) "LWPOLYLINE")
        (setq bbox (get-bbox-from-polyline bbox-ent))
        (progn
          (princ "\nError: Please select a polyline.")
          (exit)
        )
      )
    )
    (progn
      (princ "\nNo bounding box selected.")
      (exit)
    )
  )
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq count 0)

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
      
      ;; Store original positions
      (setq original-positions '())
      (foreach label-data mtextData
        (setq entdata (cadr label-data))
        (setq original-positions (cons (cdr (assoc 10 entdata)) original-positions))
      )
      
      ;; Main iteration loop
      (repeat iterations
        (setq forces '())
        
        ;; Calculate forces for each label
        (foreach label-data mtextData
          (setq entdata (cadr label-data)
                current-pos (cdr (assoc 10 entdata))
                net-force '(0.0 0.0))
          
          ;; Calculate repulsive forces from other labels
          (foreach other-data mtextData
            (if (not (eq (car label-data) (car other-data)))
              (progn
                (setq other-pos (cdr (assoc 10 (cadr other-data))))
                (setq repulsion (calculate-repulsion current-pos other-pos min-distance repulsion-strength))
                (setq net-force (list (+ (car net-force) (car repulsion))
                                    (+ (cadr net-force) (cadr repulsion))))
              )
            )
          )
          
          ;; Calculate attractive force to original position
          (setq original-pos (nth (vl-position label-data mtextData) original-positions))
          (setq attraction (calculate-attraction current-pos original-pos attraction-strength))
          (setq net-force (list (+ (car net-force) (car attraction))
                               (+ (cadr net-force) (cadr attraction))))
          
          ;; Apply damping
          (setq net-force (list (* (car net-force) damping)
                               (* (cadr net-force) damping)))
          
          (setq forces (cons net-force forces))
        )
        
        ;; Update positions
        (setq new-mtextData '())
        (foreach label-data mtextData
          (setq ent (car label-data)
                entdata (cadr label-data)
                current-pos (cdr (assoc 10 entdata))
                force (nth (vl-position label-data mtextData) forces))
          
          ;; Get direction of strongest force
          (setq direction (get-force-direction force))
          
          ;; Calculate movement distance based on force magnitude
          (setq force-magnitude (sqrt (+ (expt (car force) 2) (expt (cadr force) 2))))
          (setq move-distance (min force-magnitude 5.0))  ; Cap movement at 5 units
          
          ;; Calculate new position based on direction
          (setq new-pos current-pos)
          (cond
            ((= direction "left")
             (setq new-pos (list (- (car current-pos) move-distance)
                               (cadr current-pos)
                               (caddr current-pos))))
            ((= direction "right")
             (setq new-pos (list (+ (car current-pos) move-distance)
                               (cadr current-pos)
                               (caddr current-pos))))
            ((= direction "up")
             (setq new-pos (list (car current-pos)
                               (+ (cadr current-pos) move-distance)
                               (caddr current-pos))))
            ((= direction "down")
             (setq new-pos (list (car current-pos)
                               (- (cadr current-pos) move-distance)
                               (caddr current-pos))))
          )
          
          ;; Calculate safe movement that stays within bounds
          (setq safe-move (calculate-safe-move current-pos new-pos bbox))
          
          ;; Apply safe movement
          (setq new-pos (list (+ (car current-pos) (car safe-move))
                             (+ (cadr current-pos) (cadr safe-move))
                             (caddr current-pos)))
          
          ;; Only move if there's a significant force and new position is different
          (if (and (not (= direction "none")) 
                   (> move-distance 0.1)
                   (not (equal current-pos new-pos 0.001)))
            (progn
              ;; Move the label
              (command "._move" ent "" current-pos new-pos)
              
              ;; Print debug information
              (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                            " " direction " by " 
                            (rtos (sqrt (+ (expt (car safe-move) 2) (expt (cadr safe-move) 2))) 2 2)
                            " units"))
            )
          )
          
          ;; Update entity data
          (setq new-entdata (subst (cons 10 new-pos) (assoc 10 entdata) entdata))
          (setq new-mtextData (cons (list ent new-entdata) new-mtextData))
        )
        
        (setq mtextData new-mtextData)
      )

      (print "\nForce-directed placement completed.")
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Force-Directed Label Placement loaded. Type ACAD-MTEXT-FORCE-PLACE to run.")
(princ)

;;; AutoCAD Label Overlap Detector (MTEXT) - Greedy Label Placement
;;; This script places labels using a greedy algorithm to minimize overlaps
;;; Usage: Load the script and run the command ACAD-MTEXT-GREEDY-PLACE

;; Function to calculate label size (width * height)
(defun get-label-size (entdata)
  (setq width (cdr (assoc 41 entdata))
        height (cdr (assoc 43 entdata)))
  (* width height)
)

;; Function to count overlaps for a label
(defun count-label-overlaps (label-data mtextData / count)
  (setq count 0)
  (foreach other-data mtextData
    (if (and (not (eq (car label-data) (car other-data)))
             (setq bbox1 (get-expanded-bbox (cadr label-data)))
             (setq bbox2 (get-expanded-bbox (cadr other-data)))
             (bbox-overlap bbox1 bbox2))
      (setq count (1+ count))))
  count
)

;; Function to find nearest non-overlapping position
(defun find-nearest-position (label-data mtextData bbox / current-pos original-pos width height step-size)
  (setq current-pos (cdr (assoc 10 (cadr label-data)))
        original-pos current-pos
        width (cdr (assoc 41 (cadr label-data)))
        height (cdr (assoc 43 (cadr label-data)))
        step-size (min width height))  ; Use smaller dimension as step size
  
  ;; Try positions in expanding squares around original position
  (setq found nil
        max-steps 10)  ; Limit search radius
  
  (repeat max-steps
    (setq step-count 0)
    (while (and (not found) (< step-count 8))
      (setq test-pos current-pos)
      
      ;; Try different directions
      (cond
        ((= step-count 0)  ; Right
         (setq test-pos (list (+ (car current-pos) step-size)
                             (cadr current-pos)
                             (caddr current-pos))))
        ((= step-count 1)  ; Up
         (setq test-pos (list (car current-pos)
                             (+ (cadr current-pos) step-size)
                             (caddr current-pos))))
        ((= step-count 2)  ; Left
         (setq test-pos (list (- (car current-pos) step-size)
                             (cadr current-pos)
                             (caddr current-pos))))
        ((= step-count 3)  ; Down
         (setq test-pos (list (car current-pos)
                             (- (cadr current-pos) step-size)
                             (caddr current-pos))))
        ((= step-count 4)  ; Up-Right
         (setq test-pos (list (+ (car current-pos) step-size)
                             (+ (cadr current-pos) step-size)
                             (caddr current-pos))))
        ((= step-count 5)  ; Up-Left
         (setq test-pos (list (- (car current-pos) step-size)
                             (+ (cadr current-pos) step-size)
                             (caddr current-pos))))
        ((= step-count 6)  ; Down-Left
         (setq test-pos (list (- (car current-pos) step-size)
                             (- (cadr current-pos) step-size)
                             (caddr current-pos))))
        ((= step-count 7)  ; Down-Right
         (setq test-pos (list (+ (car current-pos) step-size)
                             (- (cadr current-pos) step-size)
                             (caddr current-pos))))
      )
      
      ;; Check if position is valid
      (if (and (point-in-bbox test-pos bbox)
               (not (has-overlaps test-pos label-data mtextData)))
        (setq found test-pos))
      
      (setq step-count (1+ step-count))
    )
    
    (if found
      (setq max-steps 0)  ; Exit loop if position found
      (setq step-size (* step-size 1.5)))  ; Increase step size for next iteration
  )
  
  found
)

;; Function to check if a position has overlaps
(defun has-overlaps (pos label-data mtextData / test-bbox has-overlap)
  (setq has-overlap nil
        test-bbox (get-expanded-bbox (subst (cons 10 pos) (assoc 10 (cadr label-data)) (cadr label-data))))
  (foreach other-data mtextData
    (if (and (not (eq (car label-data) (car other-data)))
             (setq other-bbox (get-expanded-bbox (cadr other-data)))
             (bbox-overlap test-bbox other-bbox))
      (setq has-overlap t)))
  has-overlap
)

;; Function to apply greedy placement
(defun c:ACAD-MTEXT-GREEDY-PLACE (/ ss ent obj mtextData count bbox-ent bbox)
  ;; Initialize ActiveX
  (vl-load-com)
  
  ;; Get bounding box from user
  (princ "\nSelect the bounding box polyline: ")
  (if (setq bbox-ent (entsel))
    (progn
      (setq bbox-ent (car bbox-ent))
      (if (= (cdr (assoc 0 (entget bbox-ent))) "LWPOLYLINE")
        (setq bbox (get-bbox-from-polyline bbox-ent))
        (progn
          (princ "\nError: Please select a polyline.")
          (exit)
        )
      )
    )
    (progn
      (princ "\nNo bounding box selected.")
      (exit)
    )
  )
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq count 0)

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
      
      ;; Sort labels by size (largest first) and number of overlaps
      (setq sorted-labels (sort-labels-by-priority mtextData))
      
      ;; Process each label in priority order
      (setq processed-count 0)
      (foreach label-data sorted-labels
        (setq ent (car label-data)
              entdata (cadr label-data)
              current-pos (cdr (assoc 10 entdata)))
        
        ;; Count overlaps for this label
        (setq overlap-count (count-label-overlaps label-data mtextData))
        
        ;; If label has overlaps, try to find a better position
        (if (> overlap-count 0)
          (progn
            (setq new-pos (find-nearest-position label-data mtextData bbox))
            (if new-pos
              (progn
                ;; Move the label
                (command "._move" ent "" current-pos new-pos)
                (setq processed-count (1+ processed-count))
                
                ;; Print debug information
                (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                              " to reduce " (itoa overlap-count) " overlaps"))
              )
              (princ (strcat "\nCould not find non-overlapping position for label " 
                            (vl-princ-to-string ent)))
            )
          )
          (princ (strcat "\nLabel " (vl-princ-to-string ent) " has no overlaps"))
        )
        
        ;; Update entity data
        (setq new-entdata (subst (cons 10 new-pos) (assoc 10 entdata) entdata))
        (setq mtextData (subst (list ent new-entdata) label-data mtextData))
      )

      (print (strcat "\nGreedy placement completed. Processed " 
                     (itoa processed-count) " labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Function to sort labels by priority (size and overlaps)
(defun sort-labels-by-priority (mtextData / label-scores)
  (setq label-scores '())
  
  ;; Calculate priority score for each label
  (foreach label-data mtextData
    (setq size (get-label-size (cadr label-data))
          overlaps (count-label-overlaps label-data mtextData)
          score (+ (* size 0.7) (* overlaps 0.3)))  ; Weight size more than overlaps
    (setq label-scores (cons (list label-data score) label-scores)))
  
  ;; Sort by score (highest first)
  (setq sorted-scores (vl-sort label-scores 
                              '(lambda (a b) (> (cadr a) (cadr b)))))
  
  ;; Return sorted labels
  (mapcar 'car sorted-scores)
)

;; Function to get expanded bounding box for an MTEXT entity
(defun get-expanded-bbox (entdata / insert-pt width height)
  (setq insert-pt (cdr (assoc 10 entdata))
        width (cdr (assoc 41 entdata))
        height (cdr (assoc 43 entdata)))
  
  ;; Calculate corners of bounding box
  (list
    ;; Min point (bottom-left)
    (list (- (car insert-pt) (/ width 2))
          (- (cadr insert-pt) (/ height 2))
          (caddr insert-pt))
    ;; Max point (top-right)
    (list (+ (car insert-pt) (/ width 2))
          (+ (cadr insert-pt) (/ height 2))
          (caddr insert-pt))
  )
)

;; Function to check if two bounding boxes overlap
(defun bbox-overlap (bbox1 bbox2 / min1 max1 min2 max2)
  (setq min1 (car bbox1)
        max1 (cadr bbox1)
        min2 (car bbox2)
        max2 (cadr bbox2))
  
  ;; Check for overlap in both X and Y directions
  (and
    ;; X overlap
    (<= (car min1) (car max2))
    (>= (car max1) (car min2))
    ;; Y overlap
    (<= (cadr min1) (cadr max2))
    (>= (cadr max1) (cadr min2))
  )
)

;; Load the function
(princ "\nAutoCAD MTEXT Greedy Label Placement loaded. Type ACAD-MTEXT-GREEDY-PLACE to run.")
(princ) 