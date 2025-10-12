;;; machineconst.el -- Contains machine related constants. -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains constants that should be globally referenced throughout
;; config. files for machine specifics.
;;; Code:
;; Machine ID constants. The value they hold shouldn't really matter as long as
;; they are referenced.
(defvar machineconst-id-asus nil
  "Machine - Asus.")
(defvar machineconst-id-macbook nil
  "Machine - Macbook.")

(provide 'machineconst)
;;; machineconst.el ends here
