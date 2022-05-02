;;; test-todoist-org-export.el --- Unit test of todoist-org-export.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Masaki Waga

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: HEAD
;; Author: Masaki Waga
;; Keywords: todoist org-mode
;; URL: https://github.com/MasWag/todoist-org-export
;; License: GNU General Public License >= 3

;;; Code:

(require 'ert)
(load-file "./todoist-org-export.el")

(defvar example-projects
  '(((id . 937158224) (color . 48) (name . "Inbox") (comment_count . 0) (shared . :json-false) (favorite . :json-false) (sync_id . 0) (inbox_project . t)) ((id . 937158360) (order . 1) (color . 35) (name . "Research") (comment_count . 0) (shared . :json-false) (favorite . :json-false) (sync_id . 0)) ((id . 2203704045) (parent . 937158360) (parent_id . 937158360) (order . 8) (color . 35) (name . "Symbolic Monitoring") (comment_count . 3) (shared . :json-false) (favorite . t) (sync_id . 0))))

(ert-deftest test-todoist-org-export--find-todoist-project-id--example-inbox ()
  (should (= 937158224
             (todoist-org-export--find-todoist-project-id example-projects "Inbox"))))

(ert-deftest test-todoist-org-export--find-todoist-project-id--example-research ()
  (should (= 937158360
             (todoist-org-export--find-todoist-project-id example-projects "Research"))))

(ert-deftest test-todoist-org-export--find-todoist-project-id--example-symbolic-monitoring ()
  (should (= 2203704045
             (todoist-org-export--find-todoist-project-id example-projects "Symbolic Monitoring"))))

;;; test-todoist-org-export.el ends here
