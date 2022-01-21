;;; org-graph-edna.el --- Link things in orgmode          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lytex

;; Author: lytex <julianlpc15@gmail.com>
;; Version: 0.1
;; Package-Requires: (org org-ql)
;; URL: https://github.com/lytex/org-graph-edna

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Create org-edna dependencies graphs to show task dependencies using plantuml

;;; Code:
;;;

(defcustom org-graph-edna-node-properties '("TODO" . "") "asdfadf")

(defun org-graph-edna-export (filename)

    (let ((queries-result (org-ql-query :select
              '(concat "([["
                           (concat "org-protocol://roam-node?node="
                                   (cdar
                                    (org-entry-properties nil "ID")))
                       " "
                       (substring-no-properties
                        (org-get-heading :no-tags :no-todo :no-priority :no-comment))
                       "]]) --> ([["
                        (concat "org-protocol://roam-node?node="
                        (string-trim
                        (cdar
                          (org-entry-properties nil "BLOCKER"))
                        "ids(\"id:" "\")"))
                       " "
                        (save-excursion (org-link-open-from-string
                            (string-trim
                            (cdar
                              (org-entry-properties nil "BLOCKER"))
                            "ids(\"" "\")"))
                            (substring-no-properties
                              (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
                        "]]) #blue\n"
)
              :from
              (org-agenda-files)
              :where
              '(and (regexp ":BLOCKER:  ids") t)
              )))

        (org-babel-execute:plantuml
         (concat "@startuml\n" (apply 'concat queries-result ) "\n@enduml")
         (list (cons :file filename))
         ))
    )

(provide 'org-graph-edna)
;;; org-graph-edna.el ends here
