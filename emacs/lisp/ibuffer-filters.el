;;; ibuffer-filters.el --- extra filter group definitions

;; Copyright (C) 2017 Josh Elsasser

;; Author: Josh Elsasser <josh@elsasser.ca>
;; Maintainer: Josh Elsasser <josh@elsasser.ca>
;; Created: 7 May 2017
;; Keywords: ibuffer
;; Homepage: http://github.com/jaelsasser/dotfiles

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ibuffer)

(defun ibuffer-set-filter-groups-dynamic (arg &optional silent)
  (setq ibuffer-filter-groups
        (append
         ;(ibuffer-vc-generate-filter-groups-by-vc-root)
         (ibuffer-tramp-generate-filter-groups-by-tramp-connection)
         '(("erc" (mode . erc-mode))
           ("emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Backtrace\\*$")))))))

(advice-add 'ibuffer-update :before #'my-ibuffer-generate-filter-groups)qqq

(defvar ibuffer-generate-dynamic-filter-groups 'nil)

(define-minor-mode ibuffer-dynamic-groups-mode
  "Dynamically set ibuffer-filter-groups based on the open buffers"
  :group 'ibuffer)

(provide 'ibuffer-filters)
;;; ibuffer-filters.el ends here
