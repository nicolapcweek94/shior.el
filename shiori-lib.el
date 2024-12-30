;;; shiori-lib.el --- Library for accessing Shiori API -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Nicola Zangrandi <wasp@wasp.dev>
;; Version: 0.1-pre
;; Keywords: bookmarks, shiori
;; Package-Requires: ((emacs "25.1") (request "0.3.0") (dash "2.13.0"))
;; URL: https://github.com/nicolapcweek94/shior.el

;; This file is NOT part of GNU Emacs.

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

;; This library provides functions for interacting with a Shiori server's API.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'dash)

;;;; Debug helpers

(defvar shiori-lib--debug-buffer "*shiori-debug*"
  "Buffer name for debug output.")

(defun shiori-lib--log-curl-command (method url headers data)
  "Log equivalent curl command for debugging.
METHOD is HTTP method, URL is target, HEADERS and DATA are request params."
  (with-current-buffer (get-buffer-create shiori-lib--debug-buffer)
    (goto-char (point-max))
    (let* ((header-args (mapconcat (lambda (h) 
                                    (format "-H '%s: %s'" (car h) (cdr h)))
                                  headers
                                  " "))
           (data-arg (when data
                      (format "-d '%s'" data)))
           (curl-cmd (string-trim
                     (format "curl -X %s %s %s '%s'\n\n"
                             method
                             header-args
                             (or data-arg "")
                             url))))
      (insert (format "--- %s ---\n%s" (format-time-string "%Y-%m-%d %H:%M:%S") curl-cmd)))))

;;;; Variables

(defgroup shiori-lib nil
  "Library for accessing Shiori API."
  :group 'external)

(defcustom shiori-lib-url "https://shiori.wasp.ovh"
  "URL to Shiori server."
  :type 'string)

(defvar shiori-lib--session-id nil
  "Session ID for current Shiori session.")

;;;; Functions

;;;;; Authentication

(cl-defun shiori-lib-login (username password &key (remember t) (owner t))
  "Log in to Shiori with USERNAME and PASSWORD.
If REMEMBER is non-nil, request a persistent session.
If OWNER is non-nil, request owner privileges."
  (let* ((url (concat shiori-lib-url "/api/v1/auth/login"))
         (headers '(("Content-Type" . "application/json")))
         (data (json-encode
                `(("username" . ,username)
                  ("password" . ,password)
                  ("remember_me" . ,remember))))
         (_ (shiori-lib--log-curl-command "POST" url headers data))
         (response (request
                    url
                    :type "POST"
                    :headers headers
                    :data data
                    :parser 'json-read
                    :sync t))
         (data (request-response-data response)))
    (when data
      (setq shiori-lib--session-id (alist-get 'session (alist-get 'message data)))
      data)))

(defun shiori-lib-logout ()
  "Log out of current Shiori session."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/logout"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)))
           (_ (shiori-lib--log-curl-command "POST" url headers nil))
           (response (request
                      url
                      :type "POST"
                      :headers headers
                      :parser 'json-read
                      :sync t)))
      (setq shiori-lib--session-id nil)
      (request-response-status-code response))))

;;;;; Bookmarks

(cl-defun shiori-lib-get-bookmarks (&key page)
  "Get bookmarks from Shiori.
If PAGE is non-nil, get that page of results."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/bookmarks"
                        (when page
                          (concat "?page=" (number-to-string page)))))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)))
           (_ (shiori-lib--log-curl-command "GET" url headers nil))
           (response (request
                      url
                      :type "GET"
                      :headers headers
                      :parser 'json-read
                      :sync t)))
      (when (= 200 (request-response-status-code response))
        (request-response-data response)))))

(cl-defun shiori-lib-add-bookmark (url &key title excerpt tags create-archive (public 0))
  "Add bookmark for URL to Shiori.
TITLE and EXCERPT are strings.
TAGS should be a list of strings.
If CREATE-ARCHIVE is non-nil, save an archive of the page.
If PUBLIC is non-nil, make the bookmark public."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/bookmarks"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)
                     ("Content-Type" . "application/json")))
           (data (json-encode
                             `(("url" . ,url)
                               ("title" . ,title)
                               ("excerpt" . ,excerpt)
                               ("tags" . ,(mapcar (lambda (tag)
                                                  `(("name" . ,tag)))
                                                tags))
                               ("createArchive" . ,create-archive)
                               ("public" . ,public)))
                      :parser 'json-read
                      :sync t))
      (request-response-data response))))

(cl-defun shiori-lib-edit-bookmark (id &key url title excerpt tags create-archive (public 0))
  "Edit bookmark ID in Shiori.
URL, TITLE, and EXCERPT are strings.
TAGS should be a list of strings.
If CREATE-ARCHIVE is non-nil, save an archive of the page.
If PUBLIC is non-nil, make the bookmark public."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/bookmarks"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)
                     ("Content-Type" . "application/json")))
           (data (json-encode
                             (remove nil
                                    `(("id" . ,id)
                                      ,@(when url
                                          `(("url" . ,url)))
                                      ,@(when title
                                          `(("title" . ,title)))
                                      ,@(when excerpt
                                          `(("excerpt" . ,excerpt)))
                                      ,@(when tags
                                          `(("tags" . ,(mapcar (lambda (tag)
                                                               `(("name" . ,tag)))
                                                             tags))))
                                      ,@(when create-archive
                                          `(("createArchive" . ,create-archive)))
                                      ("public" . ,public))))
                      :parser 'json-read
                      :sync t))
      (request-response-data response))))

(defun shiori-lib-delete-bookmarks (&rest ids)
  "Delete bookmarks by IDS from Shiori."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/bookmarks"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)
                     ("Content-Type" . "application/json")))
           (data (json-encode ids))
           (_ (shiori-lib--log-curl-command "DELETE" url headers data))
           (response (request
                      url
                      :type "DELETE"
                      :headers headers
                      :data data
                      :sync t)))
      (= 200 (request-response-status-code response)))))

;;;;; Tags

(defun shiori-lib-get-tags ()
  "Get list of tags from Shiori."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/tags"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)))
           (_ (shiori-lib--log-curl-command "GET" url headers nil))
           (response (request
                      url
                      :type "GET"
                      :headers headers
                      :parser 'json-read
                      :sync t)))
      (request-response-data response))))

(defun shiori-lib-rename-tag (id new-name)
  "Rename tag ID to NEW-NAME in Shiori."
  (when shiori-lib--session-id
    (let* ((url (concat shiori-lib-url "/api/tags"))
           (headers `(("X-Session-Id" . ,shiori-lib--session-id)
                     ("Content-Type" . "application/json")))
           (data (json-encode
                  `(("id" . ,id)
                    ("name" . ,new-name))))
           (_ (shiori-lib--log-curl-command "PUT" url headers data))
           (response (request
                      url
                      :type "PUT"
                      :headers headers
                      :data data
                      :parser 'json-read
                      :sync t)))
      (request-response-data response))))

(provide 'shiori-lib)

;;; shiori-lib.el ends here
