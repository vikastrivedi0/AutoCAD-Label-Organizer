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

;; Function to create a leader line between two points
(defun create-leader (start-pt end-pt / leader)
  (princ (strcat "\nCreating leader from " (vl-princ-to-string start-pt) " to " (vl-princ-to-string end-pt)))
  
  ;; Check if points are different
  (if (equal start-pt end-pt 0.001)
    (progn
      (princ "\nPoints are the same, skipping leader creation")
      (return nil)
    )
  )
  
  ;; Check if DASHED linetype exists
  (if (not (tblsearch "LTYPE" "DASHED"))
    (progn
      (princ "\nDASHED linetype not found, using CONTINUOUS")
      (setq leader (entmakex (list
        (cons 0 "LINE")
        (cons 8 "0")  ; Layer
        (cons 10 start-pt)
        (cons 11 end-pt)
        (cons 62 1)   ; Color (red)
        (cons 6 "CONTINUOUS")  ; Fallback to continuous
        (cons 48 0.5) ; Linetype scale
      )))
    )
    (setq leader (entmakex (list
      (cons 0 "LINE")
      (cons 8 "0")  ; Layer
      (cons 10 start-pt)
      (cons 11 end-pt)
      (cons 62 1)   ; Color (red)
      (cons 6 "DASHED")  ; Linetype
      (cons 48 0.5) ; Linetype scale
    )))
  )
  
  ;; Verify leader was created
  (if leader
    (princ "\nLeader created successfully")
    (princ "\nFailed to create leader")
  )
  
  leader
)

;; Main function for force-directed placement
(defun c:ACAD-MTEXT-FORCE-PLACE ()
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  ;; Get bounding box from user
  (setq bbox-result (entsel "\nSelect the bounding box polyline: "))
  (if (not bbox-result)
    (progn
      (princ "\nNo polyline selected.")
      (exit)
    )
  )
  
  (setq bbox-ent (car bbox-result))
  (if (not (= "LWPOLYLINE" (cdr (assoc 0 (entget bbox-ent)))))
    (progn
      (princ "\nPlease select a polyline.")
      (exit)
    )
  )
  
  ;; Get bounding box coordinates
  (setq bbox-coords (get-bbox-from-polyline bbox-ent))
  (setq bbox-min (car bbox-coords))
  (setq bbox-max (cadr bbox-coords))
  
  ;; Initialize parameters
  (setq iterations 100)
  (setq repulsion-strength 100.0)
  (setq attraction-strength 10.0)
  (setq damping 0.9)
  (setq min-distance 5.0)
  
  ;; Collect all MTEXT entities
  (setq mtext-list nil)
  (setq ss (ssget "X" '((0 . "MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq ent-data (entget ent))
        (setq mtext-list (cons (list
          ent
          (cdr (assoc 10 ent-data))  ; Insertion point
          (cdr (assoc 42 ent-data))  ; Width
          (cdr (assoc 43 ent-data))  ; Height
          (list 0.0 0.0)  ; Velocity
          (cdr (assoc 10 ent-data))  ; Original position
        ) mtext-list))
        (setq i (1+ i))
      )
    )
  )
  
  (princ (strcat "\nProcessing " (itoa (length mtext-list)) " MTEXT labels."))
  
  ;; Main iteration loop
  (repeat iterations
    ;; Calculate forces for each label
    (foreach label mtext-list
      (setq forces (list 0.0 0.0))
      (setq pos (cadr label))
      (setq vel (nth 4 label))
      
      ;; Calculate repulsive forces from other labels
      (foreach other-label mtext-list
        (if (/= (car label) (car other-label))
          (progn
            (setq other-pos (cadr other-label))
            (setq dist (point-distance pos other-pos))
            (if (< dist min-distance)
              (progn
                (setq repulsion (calculate-repulsion pos other-pos dist))
                (setq forces (list
                  (+ (car forces) (car repulsion))
                  (+ (cadr forces) (cadr repulsion))
                ))
              )
            )
          )
        )
      )
      
      ;; Calculate attractive force to original position
      (setq orig-pos (nth 5 label))
      (setq attraction (calculate-attraction pos orig-pos attraction-strength))
      (setq forces (list
        (+ (car forces) (car attraction))
        (+ (cadr forces) (cadr attraction))
      ))
      
      ;; Update velocity and position
      (setq new-vel (list
        (* damping (+ (car vel) (car forces)))
        (* damping (+ (cadr vel) (cadr forces)))
      ))
      (setq new-pos (list
        (+ (car pos) (car new-vel))
        (+ (cadr pos) (cadr new-vel))
      ))
      
      ;; Constrain to bounding box
      (setq new-pos (constrain-to-bbox new-pos bbox-min bbox-max))
      
      ;; Update label data
      (setq label (list
        (car label)
        new-pos
        (caddr label)
        (cadddr label)
        new-vel
        (nth 5 label)
      ))
    )
  )
  
  ;; Move labels to their final positions and create leaders
  (setq leader-count 0)
  (foreach label mtext-list
    (setq ent (car label))
    (setq new-pos (cadr label))
    (setq orig-pos (nth 5 label))
    
    ;; Check if position actually changed
    (if (not (equal new-pos orig-pos 0.001))
      (progn
        ;; Move the label
        (setq ent-data (entget ent))
        (setq ent-data (subst (cons 10 new-pos) (assoc 10 ent-data) ent-data))
        (entmod ent-data)
        
        ;; Create leader from original position to new position
        (if (create-leader orig-pos new-pos)
          (setq leader-count (1+ leader-count))
        )
      )
    )
  )
  
  (princ (strcat "\nForce-directed placement completed. Created " 
                 (itoa leader-count) " leader lines."))
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" 1)
  (princ)
)

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
        step-size (min width height 8.0))  ; Increased initial step size
  
  ;; Try positions in expanding squares around original position
  (setq found nil
        max-steps 50)  ; Significantly increased search radius
  
  (repeat max-steps
    (setq step-count 0)
    (while (and (not found) (< step-count 32))  ; More directions to try
      (setq test-pos current-pos
            angle (* step-count (/ (* 2 pi) 32.0)))  ; Divide circle into 32 parts
      
      ;; Calculate position using angle and current step size
      (setq test-pos (list
        (+ (car current-pos) (* step-size (cos angle)))
        (+ (cadr current-pos) (* step-size (sin angle)))
        (caddr current-pos)
      ))
      
      ;; Check if position is valid and has minimum distance from other labels
      (if (and (point-in-bbox test-pos bbox)
               (not (has-overlaps test-pos label-data mtextData))
               (has-minimum-distance test-pos label-data mtextData))
        (setq found test-pos))
      
      (setq step-count (1+ step-count))
    )
    
    (if found
      (setq max-steps 0)  ; Exit loop if position found
      (setq step-size (* step-size 1.2)))  ; Increase step size for next iteration
  )
  
  found
)

;; Function to check if a position has minimum distance from other labels
(defun has-minimum-distance (pos label-data mtextData / min-dist)
  (setq min-dist 8.0)  ; Increased minimum distance between labels
  
  (foreach other-data mtextData
    (if (and (not (eq (car label-data) (car other-data)))
             (setq other-pos (cdr (assoc 10 (cadr other-data))))
             (< (point-distance pos other-pos) min-dist))
      (return nil)))
  t
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

      ;; Collect all MTEXT data and store original positions
      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)))
          (progn
            (setq entdata (entget ent))
            (if entdata
              (setq mtextData (cons (list 
                ent 
                entdata 
                (cdr (assoc 10 entdata)))  ; Store original position
                mtextData))
            )
          )
        )
      )

      (print (strcat "\nProcessing " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Sort labels by priority and process multiple passes
      (setq max-passes 3)  ; Number of passes to try resolving conflicts
      (repeat max-passes
        (setq sorted-labels (sort-labels-by-priority mtextData))
        (setq processed-count 0)
        
        ;; Process each label in priority order
        (foreach label-data sorted-labels
          (setq ent (car label-data)
                entdata (cadr label-data)
                current-pos (cdr (assoc 10 entdata))
                original-pos (caddr label-data))
          
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
                  
                  ;; Create leader from original position to new position
                  (create-leader original-pos new-pos)
                  
                  ;; Print debug information
                  (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                                " to reduce " (itoa overlap-count) " overlaps"))
                )
                (princ (strcat "\nCould not find non-overlapping position for label " 
                              (vl-princ-to-string ent)))
              )
            )
          )
          
          ;; Update entity data
          (setq new-entdata (subst (cons 10 new-pos) (assoc 10 entdata) entdata))
          (setq mtextData (subst (list ent new-entdata original-pos) label-data mtextData))
        )
        
        ;; Check if all overlaps are resolved
        (setq total-overlaps 0)
        (foreach label-data mtextData
          (setq total-overlaps (+ total-overlaps (count-label-overlaps label-data mtextData)))
        )
        (if (= total-overlaps 0)
          (setq max-passes 0)  ; Exit if no more overlaps
        )
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
          score (+ (* size 0.1) (* overlaps 0.9)))  ; Weight overlaps even more heavily
    (setq label-scores (cons (list label-data score) label-scores)))
  
  ;; Sort by score (highest first)
  (setq sorted-scores (vl-sort label-scores 
                              '(lambda (a b) (> (cadr a) (cadr b)))))
  
  ;; Return sorted labels
  (mapcar 'car sorted-scores)
)

;; Function to get expanded bounding box for an MTEXT entity
(defun get-expanded-bbox (entdata / insert-pt width height margin)
  (setq insert-pt (cdr (assoc 10 entdata))
        width (cdr (assoc 41 entdata))
        height (cdr (assoc 43 entdata))
        margin 4.0)  ; Increased margin around text for better overlap detection
  
  ;; Calculate corners of bounding box with margin
  (list
    ;; Min point (bottom-left)
    (list (- (car insert-pt) (/ (+ width margin) 2))
          (- (cadr insert-pt) (/ (+ height margin) 2))
          (caddr insert-pt))
    ;; Max point (top-right)
    (list (+ (car insert-pt) (/ (+ width margin) 2))
          (+ (cadr insert-pt) (/ (+ height margin) 2))
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