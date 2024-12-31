;;; shiorel.el --- Client for Shiori bookmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Nicola Zangrandi <wasp@wasp.dev>
;; Created: 2024-12-30
;; Version: 0.1-pre
;; Keywords: bookmarks, shiori
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (kv "0.0.19") (peg "1.0.1") (s "1.10") (ov "1.0.6") (org-web-tools "0.1") (ht "2.2") (request "0.3.0"))
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

;; This is a client for Shiori (github.com/go-shiori/shiori). It allows you to
;; manage your bookmarks: add, remove, delete, tag, view, etc. Doing so in Emacs
;; with the keyboard is fast and efficient. Links can be opened in Emacs with any
;; function, or in external browsers, and specific sites/URLs can be opened with
;; specific browser functions. Views can be sorted by date, title, domain, tags,
;; etc, and "limited" mutt-style. Items can be searched for using keywords, tags,
;; etc.
;;
;; These keys can be used in the shiorel buffer:
;;
;; "RET" shiorel-open-url
;; "TAB" shiorel-pop-to-url
;; "a" shiorel-toggle-archived
;; "b" shiorel-open-in-external-browser
;; "c" shiorel-copy-url
;; "d" shiorel (return to default view)
;; "D" shiorel-delete
;; "e" shiorel-excerpt
;; "E" shiorel-excerpt-all
;; "*" shiorel-toggle-favorite
;; "f" shiorel-toggle-favorite
;; "F" shiorel-show-unread-favorites
;; "g" shiorel-resort
;; "G" shiorel-refresh
;; "s" shiorel-search
;; "m" shiorel-toggle-mark
;; "M" shiorel-mark-all
;; "U" shiorel-unmark-all
;; "o" shiorel-more
;; "l" shiorel-limit
;; "R" shiorel-random-item
;; "ta" shiorel-add-tags
;; "tr" shiorel-remove-tags
;; "tt" shiorel-set-tags
;; "ts" shiorel-tag-search
;;
;; In eww, Org, w3m, and some other major modes,
;; `shiorel-add-link' can be used to add a link at point to
;; Shiori.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'url-parse)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

(require 'dash)
(require 'kv)
(require 'ht)
(require 'ov)
(require 'peg)
(require 's)

(require 'org-web-tools)
(require 'shiori-lib)

;;;; Variables

(defvar shiorel-mode-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" shiorel-open-url
                    "TAB" shiorel-pop-to-url
                    "a" shiorel-archive-and-save
                    "b" shiorel-open-in-external-browser
                    "c" shiorel-copy-url
                    "d" shiorel ; Return to default view
                    "D" shiorel-delete
                    "e" shiorel-excerpt
                    "E" shiorel-excerpt-all
                    "g" shiorel-resort
                    "G" shiorel-refresh
                    "s" shiorel-search
                    "m" shiorel-toggle-mark
                    "M" shiorel-mark-all
                    "u" shiorel-unmark-all
                    "o" shiorel-more
                    "l" shiorel-limit
                    "R" shiorel-random-item
                    "ta" shiorel-add-tags
                    "tr" shiorel-remove-tags
                    "tt" shiorel-set-tags
                    "ts" shiorel-tag-search
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map))


(defvar shiorel-items (ht-create)
  "Items to be shown.
This is stored in a var so we can fetch the items and calculate
settings for ‘tabulated-list-mode’ based on it.  NOTE: This may
become out-of-sync with `tabulated-list-entries', so it should
not be used outside of functions that already use it.")

(defvar shiorel-offset 0
  "The current offset.")

(defvar shiorel-mark-overlays nil
  "List of overlays used to mark items.
Each item in the list is a cons cell whose first element is the
item ID and second is the overlay used to mark it.")

(defconst shiorel-url-priorities
  '(url resolved_url amp_url)
  "List of URL keys in order of preference.
When displaying or using a bookmark's URL, the first one found in
this list will be used.")

;;;;; Customization

(defgroup shiorel nil
  "Library for accessing Shiori bookmarks."
  :group 'external)

(defcustom shiorel-open-url-default-function
  #'org-web-tools-read-url-as-org
  "Default function to open items."
  :type 'function)

(defcustom shiorel-pop-to-url-default-function
  (lambda (url)
    (funcall #'org-web-tools-read-url-as-org url :show-buffer-fn #'pop-to-buffer))
  "Default function to pop-to items."
  :type 'function)

(defcustom shiorel-archive-on-open nil
  "Create archive of page when opened.
This will save a local copy of the page content in Shiori."
  :type 'boolean)

(defcustom shiorel-color-site t
  "Colorize site names uniquely."
  :type 'boolean)

(defcustom shiorel-color-title t
  "Colorize titles according to site."
  :type 'boolean)

(defcustom shiorel-show-count 50
  "Show this many items in the list."
  :type 'integer)

(defcustom shiorel-site-column-max-width 22
  "Maximum width of the site column."
  :type 'integer)

(defcustom shiorel-url-open-fn-map
  '((eww-browse-url "news.ycombinator.com"))
  ;; FIXME: This is supposed to be an alist, but the default value
  ;; isn't one.
  "List mapping URL-matching regexps to functions used to open the URL.
Regexps are anchored after the protocol (i.e. \"https://\" is not
matched against).

This is useful when certain sites should be opened in an external
browser.  The list is backward in the sense that the functions
are listed first, followed by the regexps, in this format: (FN
REGEXP REGEXP ...)."
  :type '(alist :key-type function
                :value-type (repeat string)))

(defcustom shiorel-domain-url-type-map
  '((resolved_url "reddit.com"))
  "A list mapping URL types from `shiorel-url-priorities' to domains.

This is useful when certain sites should have certain URL types
preferred (e.g. if you prefer not to load AMP URLs for Reddit)."
  :type '(alist :key-type symbol
                :value-type (repeat string)))

(defcustom shiorel-finalize-hook
  '(shiorel--apply-faces
    shiorel--add-spacers)
  "Functions run after printing items into the buffer."
  :type 'hook
  :options '(shiorel--apply-faces
             shiorel--add-spacers))

(defcustom shiorel-auth-credentials nil
  "Credentials for Shiori authentication.
Should be a cons cell of (USERNAME . PASSWORD)."
  :type '(cons string string))

(defun shiorel--ensure-authenticated ()
  "Ensure we're authenticated with Shiori."
  (unless shiori-lib--session-id
    (unless shiorel-auth-credentials
      (user-error "Please set shiorel-auth-credentials"))
    (let ((username (car shiorel-auth-credentials))
          (password (cdr shiorel-auth-credentials)))
      (shiori-lib-login username password))))

(defcustom shiorel-added-column-sort-function #'shiorel--added-fancy<
  "Function to sort the \"Added\" column."
  :type '(radio (function-item :tag "Default (by date, then favorite, then tags, then domain)" shiorel--added-fancy<)
                (function-item :tag "By date only" shiorel--added<)
                (function :tag "Custom function")))

;;;;;; Faces

(defface shiorel-marked `((default :inverse-video t)) "Face for marked items")
(defface shiorel-unread `((default :weight bold)) "Face for unread items")
(defface shiorel-archived `((default :weight normal)) "Face for archived items")

;;;; Macros

(defmacro shiorel--with-shiorel-buffer (&rest body)
  "Run BODY in ‘shiorel’ buffer and read-only inhibited."
  (declare (indent defun))
  `(with-current-buffer "*shiorel*"
     (let ((inhibit-read-only t))
       ,@body)))

(cl-defmacro shiorel--keywords-in-list (list &rest keywords)
  "Destructively remove KEYWORDS from LIST and return the last keyword found."
  (declare (debug nil))
  `(car (last (cl-loop for keyword in ',keywords
                       when (member keyword ,list)
                       do (setq ,list (delete keyword ,list))
                       and collect (s-replace (rx ":") "" keyword)))))

(cl-defmacro shiorel--regexp-in-list (list regexp &optional (prefix ":"))
  "Return last match of REGEXP in LIST, without PREFIX.
Also destructively removes matching strings from LIST."
  `(car (last (cl-loop for string in ,list
                       when (string-match ,regexp string)
                       do (setq ,list (delete string ,list))
                       and collect (replace-regexp-in-string (rx-to-string `(seq bos (regexp ,,prefix))) "" string)))))

(defmacro shiorel--at-item (id-or-item &rest body)
  "Eval BODY with point at item ID-OR-ITEM.
ID-OR-ITEM should be an integer or an alist.  If it's an alist,
get the `item-id' from it."
  (declare (indent defun) (debug (symbolp body)))
  `(shiorel--with-shiorel-buffer
     (let ((id (cl-typecase ,id-or-item
                 (integer ,id-or-item)
                 (list (alist-get 'item_id ,id-or-item)))))
       (save-excursion
         (goto-char (point-min))
         (cl-loop while (not (eobp))
                  when (equal (tabulated-list-get-id) id)
                  return (progn
                           ,@body)
                  do (forward-line 1)
                  finally do (error "Item ID not found: %s" id))))))

(defmacro shiorel--at-marked-or-current-items (&rest body)
  "Execute BODY at each marked item, or current item if none are marked."
  (declare (indent defun))
  `(if shiorel-mark-overlays
       ;; Marked items
       (cl-loop for (id . ov) in shiorel-mark-overlays
                do (shiorel--at-item id
                     ,@body))
     ;; Current item
     ,@body))

;;;; Mode

(define-derived-mode shiorel-mode tabulated-list-mode
  "Shiori Reader"
  :group 'shiorel

  ;; FIXME: Unfortunately I can't get (local 'symbol) to work with
  ;; `advice-add', and I can't get `add-function' to work either, so I
  ;; have to use `advice-add', test the buffer each time the advice is
  ;; called, and delete the advice manually when the buffer is killed.
  (advice-add 'tabulated-list--sort-by-column-name :after 'shiorel--finalize)
  (add-hook 'kill-buffer-hook (lambda ()
                                (advice-remove 'tabulated-list--sort-by-column-name 'shiorel--finalize))
            'append 'local)

  ;; Initialize format for tabulated list mode
  (setq-local tabulated-list-format [("Added" 10 shiorel-added-column-sort-function)
                                    ("Title" 40 t)
                                    ("Site" 20 t)
                                    ("Tags" 10 t)]
              tabulated-list-sort-key '("Added" . nil))
  (shiorel-refresh)
  (unless (cdr tabulated-list-sort-key)
    ;; Invert initial sort order, putting most recent items on top
    (tabulated-list-sort 0)))

;;;; Functions

;;;;; Commands

;;;###autoload
(defun shiorel ()
  "Show Shiori reading list."
  (interactive)
  (switch-to-buffer (get-buffer-create "*shiorel*"))
  (shiorel-mode))


(defun shiorel-refresh ()
  "Refresh list of bookmarks."
  (interactive)
  (let ((first-line-visible-p (pos-visible-in-window-p (point-min))))
    (goto-char (point-min))
    (custom-reevaluate-setting 'shiorel-show-count)
    (shiorel-unmark-all)
    (setq shiorel-offset 0
          shiorel-items (ht-create))
    (when-let ((items (shiori-lib-get-bookmarks :page 1)))
      (shiorel--add-items items))
    (when first-line-visible-p
      (let ((pos (point)))
        (goto-char (point-min))
        (redisplay)
        (goto-char pos)))))


(defun shiorel-more (count)
  "Fetch and show next page of items."
  (interactive "p")
  (cl-incf shiorel-offset)
  (when-let ((items (shiori-lib-get-bookmarks :page (1+ shiorel-offset))))
    (shiorel--add-items items)))

(defun shiorel-limit (query)
  "Limit display to items matching QUERY."
  ;; MAYBE: Search hidden properties so e.g. the URL can be matched against.
  (interactive (list (read-from-minibuffer "Query: ")))
  (if (s-present? query)
      (save-excursion
        (shiorel-unmark-all)
        (goto-char (point-min))
        (while (not (eobp))
          (unless (re-search-forward query (line-end-position) t)
            (ov (line-beginning-position) (1+ (line-end-position)) 'display ""))
          (forward-line 1)))
    ;; No query; show all entries
    (ov-clear 'display "")))

(defun shiorel-random-item (prefix)
  "Open a random item from the current list.
With universal prefix, read a key and call the command bound to
that keystroke on a random item."
  (interactive "p")
  (let ((fn (or (and (> prefix 1)
                     (alist-get (read-key "Key: ") shiorel-mode-map))
                #'shiorel-open-url)))
    (shiorel--with-shiorel-buffer
      (cl-loop do (progn
                    (goto-char (random (buffer-size)))
                    (beginning-of-line))
               while (not (shiorel--item-visible-p))
               finally do (funcall fn)))))

(defun shiorel--column-beginning (column)
  "Return the position of the beginning of the column named COLUMN, in the current line.

Return nil if not found."
  (save-excursion
    (beginning-of-line)
    (let ((prop 'tabulated-list-column-name)
          (end (line-end-position)))
      (while (and (< (point) end)
                  (not (equal (get-text-property (point) prop) column)))
        (goto-char (next-single-property-change (point) prop nil end)))
      (and (< (point) end) (point)))))

(defun shiorel-excerpt ()
  "Show excerpt for marked or current items."
  (interactive)
  (shiorel--at-marked-or-current-items
    (let ((excerpt (shiorel--get-property 'excerpt)))
      (unless (s-blank-str? excerpt)
        (let* ((start-col (save-excursion
                            (goto-char (shiorel--column-beginning "Title"))
                            (current-column)))
               (prefix (s-repeat start-col " "))
               (width (- (window-text-width) start-col))
               (left-margin start-col)
               (string (concat prefix (s-trim (propertize (shiorel--wrap-string excerpt width)
                                                          'face 'default)) "\n")))
          ;; Hide or show excerpt
          (unless (cl-loop for ov in (ov-forwards)
                           when (equal string (ov-val ov 'before-string))
                           do (ov-reset ov)
                           and return t)
            ;; Excerpt not found; show it
            (ov (1+ (line-end-position)) (1+ (line-end-position))
                'before-string string)))))))

(defun shiorel-excerpt-all ()
  "Show all excerpts."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((first-excerpt (cl-loop while (not (eobp))
                                  for excerpt = (shiorel--get-property 'excerpt)
                                  when excerpt
                                  return excerpt
                                  do (forward-line 1)
                                  finally do (error "No excerpts found"))))
      ;; Search for overlay showing this excerpt
      (if (cl-loop for ov in (ov-forwards)
                   thereis (equal (ov-val ov 'before-string) first-excerpt))
          ;; Already shown; hide all excerpts
          (cl-loop initially do (goto-char (point-min))
                   for ov in (ov-forwards)
                   when (not (equal (ov-val ov 'before-string) "\n"))
                   do (ov-reset ov))
        ;; Not shown; show all excerpts
        (goto-char (point-min))
        (while (not (eobp))
          (shiorel-excerpt)
          (forward-line 1))))))

;;;;;; Marking

(defun shiorel-toggle-mark ()
  "Toggle mark on current item."
  (interactive)
  ;; Make sure item is visible
  (unless (shiorel--item-visible-p)
    (error "toggle-mark called on invisible item: %s" (tabulated-list-get-id)))
  (if (shiorel--item-marked-p)
      ;; Marked; unmark
      (shiorel--unmark-item (tabulated-list-get-id))
    ;; Unmarked; mark
    (shiorel--mark-current-item))
  (forward-line 1))

(defun shiorel-mark-all ()
  "Mark all visible items."
  (interactive)
  (shiorel--with-shiorel-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               when (shiorel--item-visible-p)
               do (shiorel--mark-current-item)
               do (forward-line 1)))))

(defun shiorel-unmark-all ()
  "Unmark all items."
  (interactive)
  (cl-loop for (id . ov) in shiorel-mark-overlays
           do (shiorel--unmark-item id)))

;;;;;; Tags

(defun shiorel-tag-search (tag)
  "Search for items with TAG.
This is a plain, simple tag search, not intended to be used with
other special keywords."
  ;; MAYBE: Maybe add support for special keywords, but that might
  ;; make it more complicated to use than it is worth, because it
  ;; would mean making every plain word an implied tag keyword.
  (interactive (list (completing-read "Tag: " (cons "_untagged_" (shiorel--all-tags)))))
  (let ((query (concat ":t:" tag)))
    (shiorel-search query)))

(defun shiorel-add-tags (tags)
  "Add TAGS to current item."
  (interactive (list (completing-read "Tags: " (shiorel--all-tags))))
  (let* ((new-tags (s-split (rx (or space ",")) tags 'omit-nulls)))
    (shiorel--ensure-authenticated)
    (shiorel--at-marked-or-current-items
      (let* ((id (alist-get 'item_id (shiorel--current-item)))
             (current-tags (shiorel--get-property 'tags))
             (all-tags (append current-tags new-tags)))
        (when (shiori-lib-edit-bookmark id :tags all-tags)
          (shiorel--set-tags all-tags))))))

(defun shiorel-remove-tags (tags)
  "Remove TAGS from current item."
  (interactive (list (completing-read "Tags: " (let (tags)
                                                (shiorel--at-marked-or-current-items
                                                  (setq tags (append (shiorel--get-property 'tags) tags)))
                                                (-sort #'string< (-uniq tags))))))
  (let* ((tags-to-remove (s-split (rx (or space ",")) tags 'omit-nulls)))
    (shiorel--ensure-authenticated)
    (shiorel--at-marked-or-current-items
      (let* ((id (alist-get 'item_id (shiorel--current-item)))
             (current-tags (shiorel--get-property 'tags))
             (remaining-tags (seq-difference current-tags tags-to-remove #'string=)))
        (when (shiori-lib-edit-bookmark id :tags remaining-tags)
          (shiorel--set-tags remaining-tags))))))

(defun shiorel-set-tags (tags)
  "Set TAGS of current item."
  (interactive (list (completing-read "Tags: " (shiorel--all-tags))))
  (let* ((new-tags (s-split (rx (or space ",")) tags 'omit-nulls)))
    (shiorel--ensure-authenticated)
    (shiorel--at-marked-or-current-items
      (let* ((id (alist-get 'item_id (shiorel--current-item))))
        (when (shiori-lib-edit-bookmark id :tags new-tags)
          (shiorel--set-tags new-tags))))))

;;;;;; URL-opening

(cl-defun shiorel-open-url (&optional &key fn)
  "Open URL of current item with default function."
  (interactive)
  (shiorel--at-marked-or-current-items
    (let* ((id (tabulated-list-get-id))
           (item (ht-get shiorel-items id))
           (url (shiorel--get-url item))
           (fn (or fn (shiorel--map-url-open-fn url))))
      (when (funcall fn url)
        ;; Item opened successfully
        (when shiorel-archive-on-open
          (shiorel--with-shiorel-buffer
            (shiorel--archive-items (shiorel--current-item))))))))

(defun shiorel-pop-to-url ()
  "Open URL of current item with default pop-to function."
  (interactive)
  (shiorel-open-url :fn shiorel-pop-to-url-default-function))

(defun shiorel-open-in-external-browser ()
  "Open marked or current items in external browser.
The `browse-url-default-browser' function is used."
  (interactive)
  (shiorel-open-url
   :fn (lambda (&rest args)
         (apply #'browse-url-default-browser args)
         ;; Return t because the browsing function may not return non-nil
         ;; when it succeeds, preventing the item from being archived
         t)))

(defun shiorel-copy-url ()
  "Copy URL of current item to kill-ring/clipboard."
  (interactive)
  (when-let ((id (tabulated-list-get-id))
             (item (ht-get shiorel-items id))
             (url (shiorel--get-url item)))
    (kill-new url)
    (message url)))

;;;;;; Other

(defun shiorel-delete ()
  "Delete current or marked items (with confirmation)."
  (interactive)
  (when (yes-or-no-p "Delete item(s)?")
    (apply #'shiorel--delete-items (shiorel--marked-or-current-items))))

(defun shiorel-resort ()
  "Re-sort list."
  (interactive)
  (tabulated-list-sort 0)
  (tabulated-list-sort 0))

(defun shiorel-toggle-favorite ()
  "Toggle current or marked items' favorite status."
  (interactive)
  (cl-loop for item in (shiorel--marked-or-current-items)
           if (shiorel--at-item item
                (shiorel--is-favorite))
           collect item into unfavorites
           else collect item into favorites
           finally do (when favorites
                        (apply #'shiorel--favorite-items favorites))
           finally do (when unfavorites
                        (apply #'shiorel--unfavorite-items unfavorites))))

(defcustom shiorel-archive-file "~/org/bookmarks-archive.org"
  "File to save archived bookmarks to."
  :type 'string)

(defun shiorel-archive-and-save ()
  "Archive current or marked items by saving to org file and deleting from Shiori."
  (interactive)
  (cl-loop for item in (shiorel--marked-or-current-items)
           do (let* ((url (shiorel--get-url item))
                    (org-content (org-web-tools--html-to-org-with-pandoc url)))
             (with-temp-buffer
               (insert org-content)
               (write-region (point-min) (point-max) 
                            shiorel-archive-file 'append))
             (shiori-lib-delete-bookmarks (alist-get 'item_id item)))))

;;;;; Helpers

(defun shiorel--get-url (item &optional &key first)
  "Return URL for ITEM.
If FIRST is non-nil, return the first URL found, not the best
one.  ITEM should be a hash-table with the appropriate keys, one
of which is chosen as configured by
`shiorel-url-priorities'."
  (or (when-let ((prioritized-url
                  (cl-loop for key in shiorel-url-priorities
                           for url = (ht-get item key) ; Gets the URL
                           when (s-present? url)
                           return url)))
        (if first
            prioritized-url
          (if-let ((domain (shiorel--url-domain prioritized-url))
                   (key (cl-loop for (key . vals) in shiorel-domain-url-type-map
                                 when (member domain vals)
                                 return key))
                   (domain-preferred-url (ht-get item key)))
              domain-preferred-url
            prioritized-url)))
      (progn
        (display-warning 'shiorel (format "No URLs found for item: %S." item))
        "https://example.com/?error=item-had-no-URL")))

(defun shiorel--item-visible-p ()
  "Return non-nil if current item is visible (i.e. not hidden by an overlay)."
  (cl-loop for ov in (overlays-at (line-beginning-position))
           never (string= "" (ov-val ov 'display))))

(defun shiorel--add-items (items)
  "Add ITEMS to `shiorel-items' and update display."
  (--each (append (alist-get 'bookmarks (append items nil)) nil) ;; append to convert vector to list
    (let* ((item (ht<-alist it #'eq))
           (id (ht-get item 'id))
           (domain (shiorel--url-domain (shiorel--get-url item))))
      (ht-set item 'domain domain)
      (ht-set shiorel-items id item)))

  (shiorel--set-tabulated-list-format)

  ;; Use a copy of the list.  Otherwise, when the tabulated list is sorted, `shiorel-items'
  ;; gets rearranged when `tabulated-list-entries' gets sorted, and that somehow causes the apparent
  ;; length of `shiorel-items' to change, and that causes items to disappear from the list
  ;; when `shiorel-more' is called.  This is a very strange bug, but it's basically caused by
  ;; `sort' modifying lists by side effects.  Making `tabulated-list-entries' a copy avoids this
  ;; problem while allowing them to share the underlying items, which aren't changed.
  (setq tabulated-list-entries (shiorel--items-to-tabulated-list-entries shiorel-items))

  (tabulated-list-init-header)
  (tabulated-list-print 'remember-pos)
  (shiorel--finalize))

(defun shiorel--items-to-tabulated-list-entries (items)
  "Convert ITEMS to a list of vectors of lists.
Suitable for `tabulated-list-entries'."
  ;; NOTE: From Emacs docs:

  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.
  (cl-loop for it being the hash-values of items
           collect (let ((id (ht-get it 'id))
                         (added (shiorel--format-timestamp (ht-get it 'createdAt)))
                         (title (shiorel--not-empty-string (shiorel--or-string-not-blank
                                                                  (ht-get it 'resolved_title)
                                                                  (ht-get it 'title)
                                                                  "[untitled]")))
                         (domain (shiorel--url-domain
                                  ;; Don't use --get-url here, because, e.g. we don't want an "amp." to be shown in the list
                                  (shiorel--or-string-not-blank (ht-get it 'url)
                                                                      (ht-get it 'url))))
                         (tags (shiorel--not-empty-string (s-join "," (ht-get it 'tags)))))
                     (list id (vector added title domain tags)))))

(defun shiorel--delete-items (&rest items)
  "Delete ITEMS.
Items should be a list of items as returned by
`shiorel--marked-or-current-items'."
  (shiorel--ensure-authenticated)
  (when (apply #'shiori-lib-delete-bookmarks
               (mapcar (lambda (item) (alist-get 'item_id item)) items))
    (cl-loop for item in items
             for id = (alist-get 'item_id item)
             do (progn
                  (ht-remove! shiorel-items id)
                  (shiorel--unmark-item id)
                  (shiorel--at-item id
                    (tabulated-list-delete-entry))))
    (setq tabulated-list-entries (shiorel--items-to-tabulated-list-entries shiorel-items))))

(defun shiorel--finalize (&rest _)
  "Finalize the buffer after adding or sorting items."
  ;; Because we have to add this function as advice to
  ;; `tabulated-list--sort-by-column-name', causing it to run in every
  ;; tabulated-list buffer, we must make sure it's the shiorel
  ;; buffer.
  (when (string= "*shiorel*" (buffer-name))
    (run-hooks 'shiorel-finalize-hook)))

(defun shiorel--set-tabulated-list-format ()
  "Set `tabulated-list-format'.
Sets according to the maximum width of items about to be
displayed."
  (when-let ((domain-width (cl-loop for item being the hash-values of shiorel-items
                                    maximizing (length (ht-get item 'domain))))
             (title-width (- (window-text-width) 11 2 domain-width 10 1)))
    (when (> domain-width shiorel-site-column-max-width)
      (setq domain-width shiorel-site-column-max-width))
    (setq tabulated-list-format (vector (list "Added" 10 shiorel-added-column-sort-function)
                                       (list "Title" title-width t)
                                       (list "Site" domain-width t)
                                       (list "Tags" 10 t)))))

(defun shiorel--map-url-open-fn (url)
  "Return function to use to open URL.
Checks `shiorel-url-open-fn-map' for a function to use.  If
none is found, returns `shiorel-open-url-default-function'."
  (or (car (cl-rassoc url shiorel-url-open-fn-map
                      :test (lambda (url regexp)
                              (string-match (rx-to-string `(seq "http" (optional "s") "://"
                                                                (regexp ,(car regexp))
                                                                (or "/" eos)))
                                            url))))
      shiorel-open-url-default-function))

(defun shiorel--current-item ()
  "Return list containing cons of current item's ID.
Suitable for passing to pocket-lib."
  (let* ((id (tabulated-list-get-id)))
    (list (cons 'item_id id))))

(defun shiorel--get-property (property)
  "Return value of PROPERTY for current item."
  (let ((id (tabulated-list-get-id)))
    (ht-get* shiorel-items id property)))

(defun shiorel--set-property (property value)
  "Set current item's PROPERTY to VALUE."
  (shiorel--with-shiorel-buffer
    (let* ((id (tabulated-list-get-id))
           (item (ht-get shiorel-items id)))
      (ht-set! item property value))))

(defun shiorel--url-domain (url)
  "Return domain for URL.
Common prefixes like www are removed."
  (replace-regexp-in-string (rx bos (and (or "www" "amp") ".")) ""
                            (url-host (url-generic-parse-url url))))


(defun shiorel--format-timestamp (date-string)
"Convert date string from YYYY-MM-DD hh:mm:ss to YYYY-MM-DD using regexp."
  (message date-string)
  (if (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*" date-string)
      (match-string 1 date-string)
    date-string))

(cl-defun shiorel--add-spacers (&key (min-group-size 2))
"Insert overlay spacers where the current sort column's values change.
For example, if sorted by date, a spacer will be inserted where
the date changes.  If no group has at least MIN-GROUP-SIZE items,
no spacers will be inserted."
  ;; TODO: Use column-specific functions so that, e.g. date column could be grouped by month/year
  (let ((sort-column (seq-position tabulated-list-format tabulated-list-sort-key
                                   (lambda (seq elt)
                                     (string= (car seq) (car elt))))))
    ;; Clear existing spacers
    (ov-clear)
    (save-excursion
      (goto-char (point-min))
      (cl-loop with largest-group-size = 1
               with prev-data = (elt (tabulated-list-get-entry) sort-column)
               while (not (eobp))
               do (forward-line 1)
               for current-data = (elt (tabulated-list-get-entry) sort-column)
               if (not (equal current-data prev-data))
               do (progn
                    (ov (line-beginning-position) (line-beginning-position) 'before-string "\n")
                    (setq prev-data current-data))
               else do (cl-incf largest-group-size)
               finally do (when (< largest-group-size min-group-size)
                            (ov-clear))))))

;;;;;; Sorting

(defun shiorel--added-fancy< (a b)
  "Return non-nil if A should be sorted before B.
Items are compared by date, then favorite status, then tags, then
domain.  Suitable for sorting `tabulated-list-entries'."
  (cl-flet ((day (it) (let* ((id (car it))
                             (added-string (ht-get* shiorel-items id 'createdAt)) )
                        (time-to-days (string-to-number added-string)))))
    (let* ((a-day (day a))
           (b-day (day b)))
      (if (= a-day b-day)
          ;; Same day: compare tags, then domain
          (cl-case (shiorel--compare-tags a b)
            ('< nil)
            ('> t)
            ('=
             ;; Same tags; compare domain (invert since the default order is descending)
             (not (shiorel--domain< a b))))
        ;; Different day: compare day
        (< a-day b-day)))))

(defun shiorel--added< (a b)
  "Return non-nil if A's `createdAt' timestamp is before B's.
Suitable for sorting `tabulated-list-entries'."
  (cl-flet ((added (it) (let ((id (car it)))
                          (string-to-number (ht-get* shiorel-items id 'createdAt)))))
    (let ((a-added (added a))
          (b-added (added b)))
      (< a-added b-added))))

(defun shiorel--domain< (a b)
  "Return non-nil if A's domain is alphabetically before B's."
  (cl-flet ((domain (it) (let ((id (car it)))
                           (shiorel--url-domain (shiorel--get-url (ht-get shiorel-items id)
                                                                              :first t)))))
    (string< (domain a) (domain b))))


(defun shiorel--compare-tags (a b)
  "Compare A's and B's lists of tags.
If they are the same, return `='.  If they have different numbers
of tags, return `<' if A has more, or `>' if B has more.  If they
have the same number of tags, join each list into a single string
and compare them with `string='."
  (cl-flet ((tags (it) (let ((id (car it)))
                         (ht-get* shiorel-items id 'tags))))
    (let ((a-tags (tags a))
          (b-tags (tags b)))
      (if (not (or a-tags b-tags))
          ;; No tags
          '=
        ;; Some tags
        (if (not (and a-tags b-tags))
            ;; One item has no tags
            (if a-tags
                '<
              '>)
          ;; Both items have tags
          (let ((a-length (length a-tags))
                (b-length (length b-tags)))
            (if (/= a-length b-length)
                ;; Different number of tags: sort by number of tags
                (if (< a-length b-length)
                    ;; Entries with more tags should be sorted earlier
                    '>
                  '<)
              ;; Same number of tags: sort string of tags alphabetically
              (let ((a-string (s-join "" a-tags))
                    (b-string (s-join "" b-tags)))
                (cond ((string= a-string b-string) '=)
                      ((string< a-string b-string) '<)
                      (t '>))))))))))

;;;;;; Strings


(defun shiorel--wrap-string (string length)
  "Wrap STRING to LENGTH."
  (if (<= (length string) length)
      string
    (s-trim (with-temp-buffer
              (insert string)
              (let ((fill-column length))
                (fill-region-as-paragraph (point-min) (point-max))
                (buffer-string))))))

(defun shiorel--not-empty-string (s)
  "If S is non-empty, return it; otherwise return \" \"."
  ;; No column may be actually empty, because `tabulated-list-set-col' doesn't work on
  ;; nil columns, because it uses `next-single-property-change' to find the place to
  ;; modify.  So we use an empty string instead of nil.
  (if (string-empty-p s)
      " "
    s))

(defun shiorel--or-string-not-blank (&rest strings)
  "Return first non-empty string in STRINGS."
  (cl-loop for string in strings
           when (and string (not (s-blank-str? string)))
           return string))

;;;;;; Faces

(defun shiorel--apply-faces ()
  "Apply faces to buffer."
  ;; TODO: Maybe we should use a custom print function but this is simpler
  (shiorel--with-shiorel-buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (shiorel--apply-faces-to-line)
        (forward-line 1))
      (goto-char (point-min)))))

(defun shiorel--apply-faces-to-line ()
  "Apply faces to current line."
  (shiorel--with-shiorel-buffer
    (add-text-properties (line-beginning-position) (line-end-position)
                         (list 'face (pcase (shiorel--get-property 'status)
                                       ("0" 'shiorel-unread)
                                       ("1" 'shiorel-archived)) ))
    (when (or shiorel-color-site
              shiorel-color-title)
      (shiorel--set-site-face))))

(defun shiorel--set-site-face ()
  "Apply colored face to site column for current entry."
  (let* ((column (tabulated-list--column-number "Site"))
         (site (elt (tabulated-list-get-entry) column))
         (face (list :foreground (shiorel--prism-color site))))
    (when shiorel-color-site
      (shiorel--set-column-face "Site" face))
    (when shiorel-color-title
      (shiorel--set-column-face "Title" face))))

(defun shiorel--set-column-face (column face)
  "Apply FACE to COLUMN on current line.
COLUMN may be the column name or number."
  (shiorel--with-shiorel-buffer
    (-when-let* ((start (shiorel--column-beginning column))
                 (end (next-single-char-property-change start
                                                        'tabulated-list-column-name
                                                        nil
                                                        (line-end-position))))
      (add-face-text-property start end face t))))

;;;;; URL-adding helpers

;;;###autoload
(defun shiorel-add-link ()
  "Add link at point to Shiori.
This function tries to work in multiple major modes, such as w3m,
eww, elfeed, and Org."
  (interactive)
  (cl-case major-mode
    ('eww-mode (shiorel-eww-add-link))
    ('org-mode (shiorel-org-add-link))
    ('w3m-mode (shiorel-w3m-add-link))
    ('shr-mode (shiorel-shr-add-link))
    ('elfeed-search-mode (shiorel-elfeed-search-add-link))
    ('elfeed-show-mode (shiorel-elfeed-entry-add-link))
    (t (shiorel-generic-add-link))))

;;;###autoload
(defun shiorel-eww-add-link ()
  "Add link at point to Shiori in eww buffers."
  (interactive)
  ;; `eww-links-at-point' returns a list of links, but we only use the
  ;; first one.  I think this is the right thing to do in most, if not
  ;; all, cases.
  (when-let ((url (car (eww-links-at-point))))
    (shiorel--ensure-authenticated)
    (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
      (message "Added: %s" url))))

;;;###autoload
(defun shiorel-org-add-link ()
  "Add link at point to Shiori in Org buffers."
  (interactive)
  (when-let ((url (when (org-in-regexp org-bracket-link-regexp 1)
                    (org-link-unescape (match-string-no-properties 1)))))
    (shiorel--ensure-authenticated)
    (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
      (message "Added: %s" url))))

(declare-function 'w3m-with-lnum 'w3m-lnum)
(declare-function 'w3m-lnum-read-interactive 'w3m-lnum)
(declare-function 'w3m-lnum-get-anchor-info 'w3m-lnum)
(defvar last-index)
(defvar w3m-current-url)
;;;###autoload
(with-eval-after-load 'w3m-lnum
  (cl-defun shiorel-w3m-lnum-add-link (&key (type 1))
    "Add link to Pocket with lnum in w3m buffers."
    (interactive)
    (w3m-with-lnum
     type ""
     (when-let ((num (car (w3m-lnum-read-interactive
                           "Anchor number: "
                           'w3m-lnum-highlight-anchor
                           type last-index w3m-current-url)))
                (info (w3m-lnum-get-anchor-info num))
                (url (car info)))
       (shiorel--ensure-authenticated)
       (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
         (message "Added: %s" url))))))

;;;###autoload
(with-eval-after-load 'w3m
  (defun shiorel-w3m-add-link ()
    "Add link at point to Shiori in w3m buffers."
    (interactive)
    (if-let ((url (or (get-text-property (point) 'w3m-href-anchor)
                      (unless (bolp)
                        (save-excursion
                          (get-text-property (1- (point)) 'w3m-href-anchor)))
                      (unless (eolp)
                        (save-excursion
                          (get-text-property (1+ (point)) 'w3m-href-anchor)))
                      (thing-at-point-url-at-point))))
        (progn
          (shiorel--ensure-authenticated)
          (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
            (message "Added: %s" url)))
      (if (member 'w3m-lnum-mode minor-mode-list)
          ;; No URL found around point: use lnum if loaded
          (shiorel-w3m-lnum-add-link)
        ;; We tried.
        (message "No URL found around point.")))))

;;;###autoload
(defun shiorel-shr-add-link ()
  "Add link at point in `shr-mode' buffer to Shiori."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (progn
        (shiorel--ensure-authenticated)
        (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
          (message "Added: %s" url)))
    (message "No URL found at point.")))

(defvar elfeed-show-entry)
;;;###autoload
(with-eval-after-load 'elfeed
  (defun shiorel-elfeed-search-add-link ()
    "Add links for selected entries in Elfeed search-mode buffer to Shiori.
This is only for the elfeed-search buffer, not for entry buffers."
    (interactive)
    (when-let ((entries (elfeed-search-selected))
               (links (mapcar #'elfeed-entry-link entries)))
      (shiorel--ensure-authenticated)
      (when (cl-every (lambda (link)
                        (shiori-lib-add-bookmark link :create-archive shiorel-archive-on-open))
                      links)
        (message "Added: %s" (s-join ", " links))
        (elfeed-search-untag-all-unread))))

  (defun shiorel-elfeed-entry-add-link ()
    "Add links for selected entries in elfeed-show-mode buffer to Shiori.
This is only for the elfeed-entry buffer, not for search buffers."
    (interactive)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (shiorel--ensure-authenticated)
      (when (shiori-lib-add-bookmark link :create-archive shiorel-archive-on-open)
        (message "Added: %s" link)))))

;;;###autoload
(defun shiorel-generic-add-link ()
  "Try to add URL at point to Shiori using `thing-at-pt'."
  (interactive)
  (if-let ((url (or (thing-at-point-url-at-point)
                    (with-temp-buffer
                      (insert (current-kill 0))
                      (thing-at-point-url-at-point)))))
      (progn
        (shiorel--ensure-authenticated)
        (when (shiori-lib-add-bookmark url :create-archive shiorel-archive-on-open)
          (message "Added: %s" url)))
    (user-error "No URL found at point or in clipboard")))

(defcustom shiorel-prism-minimum-contrast 6
  "Attempt to enforce this minimum contrast ratio for user faces.
This should be a reasonable number from, e.g. 0-7 or so."
  ;; Prot would almost approve of this default.  :) I would go all the way
  ;; to 7, but 6 already significantly dilutes the colors in some cases.
  :type 'number)

(cl-defun shiorel--prism-color
    (string &key (contrast-with (face-background 'default nil 'default)))
  "Return a computed color for STRING.
The color is adjusted to have sufficient contrast with the color
CONTRAST-WITH (by default, the default face's background)."
  ;; Copied from `ement--prism-color'.
  (cl-labels ((relative-luminance (rgb)
                ;; Copy of `modus-themes-wcag-formula', an elegant
                ;; implementation by Protesilaos Stavrou.  Also see
                ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
                ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
                (cl-loop for k in '(0.2126 0.7152 0.0722)
                         for x in rgb
                         sum (* k (if (<= x 0.03928)
                                      (/ x 12.92)
                                    (expt (/ (+ x 0.055) 1.055) 2.4)))))
              (contrast-ratio (a b)
                ;; Copy of `modus-themes-contrast'; see above.
                (let ((ct (/ (+ (relative-luminance a) 0.05)
                             (+ (relative-luminance b) 0.05))))
                  (max ct (/ ct))))
              (increase-contrast (color against target toward)
                (let ((gradient (cdr (color-gradient color toward 20)))
                      new-color)
                  (cl-loop do (setf new-color (pop gradient))
                           while new-color
                           until (>= (contrast-ratio new-color against) target)
                           ;; Avoid infinite loop in case of weirdness
                           ;; by returning color as a fallback.
                           finally return (or new-color color)))))
    (let* ((id string)
           (id-hash (float (abs (sxhash id))))
           (ratio (/ id-hash (float most-positive-fixnum)))
           (color-num (round (* (* 255 255 255) ratio)))
           (color-rgb (list (/ (float (logand color-num 255)) 255)
                            (/ (float (ash (logand color-num 65280) -8)) 255)
                            (/ (float (ash (logand color-num 16711680) -16)) 255)))
           (contrast-with-rgb (color-name-to-rgb contrast-with)))
      (when (< (contrast-ratio color-rgb contrast-with-rgb) shiorel-prism-minimum-contrast)
        (setf color-rgb (increase-contrast
                         color-rgb contrast-with-rgb shiorel-prism-minimum-contrast
                         (color-name-to-rgb
                          ;; Ideally we would use the foreground color,
                          ;; but in some themes, like Solarized Dark,
                          ;; the foreground color's contrast is too low
                          ;; to be effective as the value to increase
                          ;; contrast against, so we use white or black.
                          (pcase contrast-with
                            ((or `nil "unspecified-bg")
                             ;; The `contrast-with' color (i.e. the
                             ;; default background color) is nil.  This
                             ;; probably means that we're displaying on
                             ;; a TTY.
                             (if (fboundp 'frame--current-backround-mode)
                                 ;; This function can tell us whether
                                 ;; the background color is dark or
                                 ;; light, but it was added in Emacs
                                 ;; 28.1.
                                 (pcase (frame--current-backround-mode (selected-frame))
                                   ('dark "white")
                                   ('light "black"))
                               ;; Pre-28.1: Since faces' colors may be
                               ;; "unspecified" on TTY frames, in which
                               ;; case we have nothing to compare with, we
                               ;; assume that the background color of such
                               ;; a frame is black and increase contrast
                               ;; toward white.
                               "white"))
                            (_
                             ;; The `contrast-with` color is usable: test it.
                             (if (shiorel--color-dark-p (color-name-to-rgb contrast-with))
                                 "white" "black")))))))
      (apply #'color-rgb-to-hex (append color-rgb (list 2))))))

;;;;; Emacs 28 color features.

;; Copied from Emacs 28.  See <https://github.com/alphapapa/ement.el/issues/99>.

;; TODO(future): Remove these workarounds when dropping support for Emacs <28.

(eval-and-compile
  (unless (boundp 'color-luminance-dark-limit)
    (defconst shiorel--color-luminance-dark-limit 0.325
      "The relative luminance below which a color is considered \"dark.\"
A \"dark\" color in this sense provides better contrast with
white than with black; see `color-dark-p'.  This value was
determined experimentally.")))

(defalias 'shiorel--color-dark-p
  (if (fboundp 'color-dark-p)
      'color-dark-p
    (with-suppressed-warnings ((free-vars shiorel--color-luminance-dark-limit))
      (lambda (rgb)
        "Whether RGB is more readable against white than black.
RGB is a 3-element list (R G B), each component in the range [0,1].
This predicate can be used both for determining a suitable (black or white)
contrast colour with RGB as background and as foreground."
        (unless (<= 0 (apply #'min rgb) (apply #'max rgb) 1)
          (error "RGB components %S not in [0,1]" rgb))
        ;; Compute the relative luminance after gamma-correcting (assuming sRGB),
        ;; and compare to a cut-off value determined experimentally.
        ;; See https://en.wikipedia.org/wiki/Relative_luminance for details.
        (let* ((sr (nth 0 rgb))
               (sg (nth 1 rgb))
               (sb (nth 2 rgb))
               ;; Gamma-correct the RGB components to linear values.
               ;; Use the power 2.2 as an approximation to sRGB gamma;
               ;; it should be good enough for the purpose of this function.
               (r (expt sr 2.2))
               (g (expt sg 2.2))
               (b (expt sb 2.2))
               (y (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))))
          (< y shiorel--color-luminance-dark-limit))))))

;;;; Footer

(provide 'shiorel)

;;; shiorel.el ends here
