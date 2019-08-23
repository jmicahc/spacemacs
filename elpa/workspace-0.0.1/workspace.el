;;; workspace.el --- Self Automation Utilities                    -*- lexical-binding: t; -*-

;; Copyright (C) 2019 John Collins

;; Author: John Collins <collins@braincorporation.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Self automation stuff.

;;; Code:

(provide 'workspace)


(defun org-open-work ()
  (interactive)
  (find-file "~/org/work.org"))


(defun org-open-life ()
  (interactive)
  (find-file "~/org/personal.org"))


(defun org-open-practice ()
  (interactive)
  (find-file "~/org/practice.org"))


;;; workspace.el ends here
