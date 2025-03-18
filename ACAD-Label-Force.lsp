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
  (setq iterations 100
        repulsion-strength 15.0
        attraction-strength 5.0
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
          
          ;; Constrain new position to bounding box
          (setq new-pos (constrain-to-bbox new-pos bbox))
          
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
                            (rtos move-distance) " units"))
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