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

;; Function to apply force-directed placement
(defun c:ACAD-MTEXT-FORCE-PLACE (/ ss ent obj mtextData count iterations)
  ;; Initialize parameters
  (setq iterations 100
        repulsion-strength 15.0
        attraction-strength 5.0
        damping 0.9
        min-distance 5.0)
  
  ;; Initialize ActiveX
  (vl-load-com)
  
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
          
          ;; Calculate new position
          (setq new-pos (list (+ (car current-pos) (car force))
                             (+ (cadr current-pos) (cadr force))
                             (caddr current-pos)))
          
          ;; Move the label
          (command "._move" ent "" current-pos new-pos)
          
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