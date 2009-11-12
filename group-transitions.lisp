(in-package :stumpwm)

(defmethod update-instance-for-different-class
    ((prev tile-group) (cur float-group) &rest initargs)
  (dolist (window (group-windows prev))
    (let ((frame (window-frame window)))
      (setf (window-x window) (frame-x frame)
            (window-y window) (frame-y frame)))
    (change-class window 'float-window)
    (float-window-align window))
  (setf (float-group-current-window cur) (group-current-window prev)))

(defmethod update-instance-for-different-class
    ((prev group) (cur tile-group) &rest initargs)
  (let ((heads (copy-heads (group-screen prev))))  ; can probably use shared-initialize for this bit
    (setf (tile-group-frame-tree cur) heads
          (tile-group-current-frame cur) (first heads))
    (setf (frame-window (first heads)) (first (group-windows prev)))
    (dolist (window (group-windows prev))
      (change-class window 'tile-window)
      (setf (window-frame window) (first heads)))
    (sync-all-frame-windows cur)))


;; quick hacks - to be replaced with a proper UI
(defcommand to-float () ()
  (change-class (current-group) 'float-group))
(defcommand to-tile () ()
  (change-class (current-group) 'tile-group))
