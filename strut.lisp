;; Copyright (C) 2010 Ben Spencer
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; NetWM strut (panel/dock) support
;;
;; Code:


; Notes:
;
; Add strut:
;   when processing existing windows
;   when a strut window gets a map request
;   when we see a property notify changing struts (if not already a strut)
;
; Remove strut:
;   when a strut window is destroyed
;   when a we see a property notify removing strut properties
;
; Apply struts:
;   after initially generating heads / processing existing windows
;   when regenerating heads after a configure notify
;   after a strut window is mapped
;   when seeing a property notify changing struts
;   when a strut window is destroyed

(in-package :stumpwm)

(defun xwin-strut-p (win)
  (or (xlib:get-property win :_NET_WM_STRUT_PARTIAL)
      (xlib:get-property win :_NET_WM_STRUT)))

(defun find-strut-screen (xwin)
  (find-if (lambda (screen) (find xwin (screen-struts screen))) *screen-list*))

(defun add-strut (screen xwin)
  (pushnew xwin (screen-struts screen))
  (xlib:map-window xwin))

(defun remove-strut (screen xwin)
  (setf (screen-struts screen)
        (remove xwin (screen-struts screen))))

(defun apply-struts-to-heads (screen)
  (flet ((overlaps (s1 e1 s2 e2)
           (> (- (min s1 s2)
                 (max e1 e2) 0))))
    (dolist (oh (screen-heads screen))
      (let* ((xs (head-x oh))
             (xe (+ xs (head-width oh)))
             (ys (head-y oh))
             (ye (+ ys (head-height oh)))
             (left xs)
             (right (- (screen-width screen) xe))
             (top ys)
             (bottom (- (screen-height screen) ye))
             (nh (copy-head oh)))
        (dolist (strut (screen-struts screen))
          (multiple-value-bind
                (lo ro to bo ls le rs re ts te bs be)
              (xwin-strut screen strut)
            (when (overlaps ls le ys ye) (setf left (max left lo)))
            (when (overlaps rs re ys ye) (setf right (max right ro)))
            (when (overlaps ts te xs xe) (setf top (max top to)))
            (when (overlaps bs be xs xe) (setf bottom (max bottom bo)))
            (setf (head-x nh) left)
            (setf (head-width nh) (- (screen-width screen) left right))
            (setf (head-y nh) top)
            (setf (head-height nh) (- (screen-height screen) top bottom))
            (unless (equalp oh nh)
              (scale-head screen oh nh))))))
    (mapc 'group-add-head (screen-groups screen))))
