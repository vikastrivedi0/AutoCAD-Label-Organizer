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

;; Function to check if two bounding boxes overlap
(defun bbox-overlap (bbox1 bbox2 / min1 max1 min2 max2)
  (if (and bbox1 bbox2 (cadr bbox1) (caddr bbox1) (cadr bbox2) (caddr bbox2))
    (progn
      (setq min1 (cadr bbox1)
            max1 (caddr bbox1)
            min2 (cadr bbox2)
            max2 (caddr bbox2))
      (and
        ;; Check X overlap
        (<= (car min1) (car max2))
        (>= (car max1) (car min2))
        ;; Check Y overlap
        (<= (cadr min1) (cadr max2))
        (>= (cadr max1) (cadr min2))
      )
    )
    nil
  )
)

;; Function to get insertion point from entity data
(defun get-insertion-point (entdata)
  (if entdata
    (cdr (assoc 10 entdata))
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
             (setq other-bbox (get-expanded-bbox (cadr data)))
             (bbox-overlap bbox other-bbox))
      (progn
        ;; Get centers of both bounding boxes
        (setq center1 (list (/ (+ (car (cadr bbox)) (car (caddr bbox))) 2.0)
              center2 (list (/ (+ (car (cadr other-bbox)) (car (caddr other-bbox))) 2.0)
                           (/ (+ (cadr (cadr other-bbox)) (cadr (caddr other-bbox))) 2.0)))
        
        ;; Count overlaps in each direction based on relative positions
        (if (< (car center2) (car center1))
          (setq count-left (1+ count-left)))
        (if (> (car center2) (car center1))
          (setq count-right (1+ count-right)))
        (if (< (cadr center2) (cadr center1))
          (setq count-down (1+ count-down)))
        (if (> (cadr center2) (cadr center1))
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
            
            ;; Print debug information about overlaps
            (princ (strcat "\nLabel " (vl-princ-to-string ent) " overlaps:"))
            (foreach dir-count overlap-counts
              (princ (strcat "\n  " (car dir-count) ": " (itoa (cadr dir-count))))
            )
            
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
            
            ;; Only move if we found a valid direction and there are overlaps
            (if (and best-direction (> min-overlaps 0))
              (progn
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
              (princ (strcat "\nNo overlaps found for label " (vl-princ-to-string ent)))
            )
          )
          (princ (strcat "\nCould not get bounding box for label " (vl-princ-to-string ent)))
        )
      )

      (print (strcat "\nCompleted. Successfully moved " (itoa moved-count) " MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Function to write data to CSV file
(defun write-csv-line (file-handle data)
  (write-line (vl-string-subst "," "\"" (vl-princ-to-string data)) file-handle)
)

;; Function to export MTEXT positions to CSV
(defun c:ACAD-MTEXT-EXPORT-POSITIONS (/ ss ent obj mtextData count file-handle)
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

      (print (strcat "\nFound " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Create CSV file
      (setq file-handle (open "mtext_positions.csv" "w"))
      (if file-handle
        (progn
          ;; Write CSV header
          (write-line "Entity Name,Handle,Insertion X,Insertion Y,Insertion Z,Width,Height,Text Content" file-handle)
          
          ;; Process each label
          (foreach label-data mtextData
            (setq ent (car label-data)
                  entdata (cadr label-data))
            
            ;; Extract data
            (setq insertion (cdr (assoc 10 entdata))  ; Insertion point
                  width (cdr (assoc 41 entdata))      ; Width
                  height (cdr (assoc 43 entdata))     ; Height
                  handle (cdr (assoc 5 entdata))      ; Handle
                  text (cdr (assoc 1 entdata)))       ; Text content
            
            ;; Write data to CSV
            (write-csv-line file-handle (strcat (vl-princ-to-string ent) "," handle))
            (write-csv-line file-handle (strcat (rtos (car insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos (cadr insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos (caddr insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos width 2 6) ","))
            (write-csv-line file-handle (strcat (rtos height 2 6) ","))
            (write-csv-line file-handle (strcat "\"" (vl-string-subst "\"\"" "\"" text) "\""))
            (write-line "" file-handle)
          )
          
          (close file-handle)
          (print "\nPosition data exported to mtext_positions.csv")
        )
        (print "\nError: Could not create CSV file")
      )
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;;; AutoCAD Label Overlap Detector (MTEXT) - Force-Directed Label Placement
;;; This script uses physics-based forces to resolve label overlaps
;;; Usage: Load the script and run the command ACAD-MTEXT-FORCE-PLACE

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
        repulsion-strength 100.0
        attraction-strength 10.0
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