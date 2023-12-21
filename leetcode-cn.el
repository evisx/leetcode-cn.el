;;; leetcode-cn.el --- An leetcode client           -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "26.1") (dash "2.16.0") (graphql "0.1.1") (spinner "1.7.3") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.27

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

;; leetcode-cn.el is an unofficial LeetCode client.
;;
;; Now it implements several API:
;; - Check problems list
;; - Try testcase
;; - Submit code
;;
;; Since most HTTP requests works asynchronously, it won't block Emacs.
;;
;;; Code:
(eval-when-compile
  (require 'let-alist))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)                      ; Some requests of LeetCode use GraphQL
(require 'aio)
(require 'spinner)
(require 'log4e)

(log4e:deflogger "leetcode-cn" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-leetcode-cn "*leetcode-cn-log*")

;;;###autoload
(defun leetcode-cn-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (leetcode-cn--log-debugging-p)
      (progn
        (leetcode-cn--log-set-level 'info)
        (leetcode-cn--log-disable-debugging)
        (message "leetcode disable debug"))
    (progn
      (leetcode-cn--log-set-level 'debug)
      (leetcode-cn--log-enable-debugging)
      (message "leetcode enable debug"))))

(defun leetcode-cn--install-my-cookie ()
  "Install leetcode dependencies."
  (let ((async-shell-command-display-buffer t))
    (async-shell-command
     "pip3 install my_cookies"
     (get-buffer-create "*leetcode-cn-install*"))))

(defun leetcode-cn--check-deps ()
  "Check if all dependencies installed."
  (if (executable-find "my_cookies")
      t
    (leetcode-cn--install-my-cookie)
    nil))

(defgroup leetcode-cn nil
  "A leetcode-cn client."
  :prefix 'leetcode-cn-
  :group 'tools)

(defcustom leetcode-cn-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :group 'leetcode-cn
  :type 'boolean)

(defcustom leetcode-cn-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :group 'leetcode-cn
  :type 'string)

(defcustom leetcode-cn-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql."
  :group 'leetcode-cn
  :type 'string)

(defcustom leetcode-cn-directory "~/leetcode"
  "Directory to save solutions."
  :group 'leetcode-cn
  :type 'string)

(defcustom leetcode-cn-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-cn-directory'."
  :group 'leetcode-cn
  :type 'boolean)

(defcustom leetcode-cn-focus t
  "When execute `leetcode', always delete other windows."
  :group 'leetcode-cn
  :type 'boolean)

(cl-defstruct leetcode-cn-user
  "A LeetCode User.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number"
  username solved easy medium hard)

(cl-defstruct leetcode-cn-problem
  "A single LeetCode problem.
:status     String
:id         Number
:backend-id Number
:title      String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}
:tags       List"
  status id backend-id title acceptance
  difficulty paid-only tags)

(cl-defstruct leetcode-cn-problems
  "All LeetCode problems, the problems can filtered by tag.
:num      Number
:tag      String
:problems List[leetcode-cn--problems]"
  num tag problems)

(defvar leetcode-cn--user (make-leetcode-cn-user)
  "A User object.")

(defvar leetcode-cn--problems (make-leetcode-cn-problems)
  "Problems object with a list of `leetcode-cn-problem'.")

(defvar leetcode-cn--all-tags nil
  "All problems tags.")

(defvar leetcode-cn--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar leetcode-cn--display-tags leetcode-cn-prefer-tag-display
  "(Internal) Whether tags are displayed the *leetcode* buffer.")

(defvar leetcode-cn--display-paid nil
  "(Internal) Whether paid problems are displayed the *leetcode* buffer.")

(defvar leetcode-cn--lang leetcode-cn-prefer-language
  "LeetCode programming language or sql for current problem internally.
Default is programming language.")

(defvar leetcode-cn--description-window nil
  "(Internal) Holds the reference to description window.")

(defvar leetcode-cn--testcase-window nil
  "(Internal) Holds the reference to testcase window.")

(defvar leetcode-cn--result-window nil
  "(Internal) Holds the reference to result window.")

(defconst leetcode-cn--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("dart" . ".dart") ("elixir" . ".ex") ("erlang" . ".erl")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py") ("python3" . ".py")
    ("racket" . ".rkt") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift") ("typescript" . ".ts")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "LeetCode programming language suffixes.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defvar leetcode-cn--filter-regex nil "Filter rows by regex.")
(defvar leetcode-cn--filter-tag nil "Filter rows by tag.")
(defvar leetcode-cn--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst leetcode-cn--all-difficulties '("easy" "medium" "hard"))
(defconst leetcode-cn--paid "•" "Paid mark.")
(defconst leetcode-cn--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode-cn--buffer-name             "*leetcode-cn*")

(defconst leetcode-cn--retry-times 20 "`leetcode-cn-try' or `leetcode-cn-submit' retry times.")

(defface leetcode-cn-paid-face
  '((t (:foreground "gold")))
  "Face for `leetcode-cn--paid'."
  :group 'leetcode-cn)

(defface leetcode-cn-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode-cn--checkmark'."
  :group 'leetcode-cn)

(defface leetcode-cn-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems."
  :group 'leetcode-cn)

(defface leetcode-cn-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems."
  :group 'leetcode-cn)

(defface leetcode-cn-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems."
  :group 'leetcode-cn)

(defface leetcode-cn-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted."
  :group 'leetcode-cn)

(defface leetcode-cn-error-face
  '((t (:foreground "#dc143c")))
  "Face for submission compile error, runtime error and TLE."
  :group 'leetcode-cn)

;;; Login
;; URL
(defconst leetcode-cn--domain    "leetcode.cn")
(defconst leetcode-cn--url-base  "https://leetcode.cn")
(defconst leetcode-cn--url-login (concat leetcode-cn--url-base "/accounts/login"))

;; Cookie key name
(defconst leetcode-cn--cookie-csrftoken "csrftoken")
(defconst leetcode-cn--cookie-session "LEETCODE_SESSION")

;; Header
(defconst leetcode-cn--User-Agent       '("User-Agent" .
                                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode-cn--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode-cn--X-CSRFToken      "X-CSRFToken")
(defconst leetcode-cn--Content-Type     '("Content-Type" . "application/json"))

;; API URL
(defconst leetcode-cn--url-api                 (concat leetcode-cn--url-base "/api"))
(defconst leetcode-cn--url-graphql             (concat leetcode-cn--url-base "/graphql"))
(defconst leetcode-cn--url-all-problems        (concat leetcode-cn--url-api "/problems/all/"))
(defconst leetcode-cn--url-all-tags            (concat leetcode-cn--url-base "/problems/api/tags"))
(defconst leetcode-cn--url-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))
;; submit
(defconst leetcode-cn--url-submit              (concat leetcode-cn--url-base "/problems/%s/submit/"))
(defconst leetcode-cn--url-problems-submission (concat leetcode-cn--url-base "/problems/%s/submissions/"))
(defconst leetcode-cn--url-check-submission    (concat leetcode-cn--url-base "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode-cn--url-try                 (concat leetcode-cn--url-base "/problems/%s/interpret_solution/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode-cn--to-list (vec)
  "Convert VEC to list."
  (append vec '()))

(defun leetcode-cn--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode-cn--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1)
                               leetcode-cn--cookie-csrftoken))
                    (url-cookie-retrieve leetcode-cn--domain "/" t))))
      (aref cookie 2)))

(aio-defun leetcode-cn--csrf-token ()
  "Return csrf token."
  (unless (leetcode-cn--maybe-csrf-token)
    (aio-await (aio-url-retrieve leetcode-cn--url-login)))
  (leetcode-cn--maybe-csrf-token))

(defun leetcode-cn--login-p ()
  "Whether user is login."
  (let ((username (leetcode-cn-user-username leetcode-cn--user)))
    (and username
         (not (string-empty-p username))
         (seq-find
          (lambda (item)
            (string= (aref item 1)
                     leetcode-cn--cookie-session))
          (url-cookie-retrieve leetcode-cn--domain "/" t)))))

(defun leetcode-cn--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'. 'Pow(x, n)' will be 'powx-n'"
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),]" "" str1)))
    res))

(defun leetcode-cn--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun leetcode-cn--problem-link (title)
  "Generate problem link from TITLE."
  (concat leetcode-cn--url-base "/problems/" (leetcode-cn--slugify-title title)))

(defun leetcode-cn--stringify-difficulty (difficulty)
  "Stringify DIFFICULTY level (number) to 'easy', 'medium' or 'hard'."
  (let ((easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard"))
    (cond
     ((eq 1 difficulty)
      (leetcode-cn--add-font-lock easy-tag 'leetcode-cn-easy-face))
     ((eq 2 difficulty)
      (leetcode-cn--add-font-lock medium-tag 'leetcode-cn-medium-face))
     ((eq 3 difficulty)
      (leetcode-cn--add-font-lock hard-tag 'leetcode-cn-hard-face)))))

(defun leetcode-cn--add-font-lock (str face)
  (prog1 str
    (put-text-property
     0 (length str)
     'font-lock-face face str)))

(defun leetcode-cn--detail-buffer-name (problem-id)
  "Detail buffer name."
  (format "*leetcode-cn-detail-%s*" problem-id))

(defun leetcode-cn--testcase-buffer-name (problem-id)
  "Testcase buffer name."
  (format "*leetcode-cn-testcase-%s*" problem-id))

(defun leetcode-cn--result-buffer-name (problem-id)
  "Result buffer name."
  (format "*leetcode-cn-result-%s*" problem-id))

(defun leetcode-cn--maybe-focus ()
  (if leetcode-cn-focus (delete-other-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LeetCode API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(aio-defun leetcode-cn--login ()
  "Steal LeetCode login session from local browser.
It also cleans LeetCode cookies in `url-cookie-file'."
  (leetcode-cn--loading-mode t)
  (ignore-errors (url-cookie-delete-cookies leetcode-cn--domain))
  (aio-await (leetcode-cn--csrf-token))    ;knock knock, whisper me the mysterious information
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string (concat (shell-quote-argument my-cookies) " cn")))
         (cookies-list (seq-filter
                        (lambda (s) (not (string-empty-p s)))
                        (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map
                         (lambda (s) (split-string s))
                         cookies-list))
         (leetcode-cn-session (cadr (assoc leetcode-cn--cookie-session cookies-pairs)))
         (leetcode-cn-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (leetcode-cn--debug "login session: '%s'" leetcode-cn-session)
    (leetcode-cn--debug "login csrftoken: '%s'" leetcode-cn-csrftoken)
    (url-cookie-store leetcode-cn--cookie-session leetcode-cn-session nil leetcode-cn--domain "/" t)
    (url-cookie-store "csrftoken" leetcode-cn-csrftoken nil leetcode-cn--domain "/" t))
  (leetcode-cn--loading-mode -1))

(aio-defun leetcode-cn--api-fetch-all-tags ()
  "Fetch all problems' tags."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent
            ,leetcode-cn--X-Requested-With
            ,(leetcode-cn--referer leetcode-cn--url-login)))
         (result (aio-await (aio-url-retrieve leetcode-cn--url-all-tags))))
    (with-current-buffer (cdr result)
      (goto-char url-http-end-of-headers)
      (json-read))))

(aio-defun leetcode-cn--api-fetch-user-and-problems ()
  "Fetch user and problems info."
  (if leetcode-cn--loading-mode
      (message "LeetCode has been refreshing...")
    (leetcode-cn--loading-mode t)
    (let ((url-request-method "GET")
          (url-request-extra-headers
           `(,leetcode-cn--User-Agent
             ,leetcode-cn--X-Requested-With
             ,(leetcode-cn--referer leetcode-cn--url-login)))
          (result (aio-await (aio-url-retrieve leetcode-cn--url-all-problems))))
      (leetcode-cn--loading-mode -1)
      (if-let ((error-info (plist-get (car result) :error)))
          (progn
            (switch-to-buffer (cdr result))
            (leetcode-cn--warn "LeetCode fetch user and problems failed: %S" error-info))
        (with-current-buffer (cdr result)
          (goto-char url-http-end-of-headers)
          (json-read))))))

(defun leetcode-cn--problem-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are LeetCode GraphQL parameters."
  (list
   (cons "operationName" operation)
   (cons "query"
         (graphql-query
          questionData
          (:arguments
           (($titleSlug . String!))
           (question
            :arguments
            ((titleSlug . ($ titleSlug)))
            likes
            dislikes
            content
            sampleTestCase
            (topicTags slug)
            (codeSnippets langSlug code)))))
   (if vars (cons "variables" vars))))

(aio-defun leetcode-cn--api-fetch-problem (title)
  "Fetch single problem.
TITLE is a problem's title.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (let* ((slug-title (leetcode-cn--slugify-title title))
         (url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent ,leetcode-cn--Content-Type))
         (url-request-data
          (json-encode (leetcode-cn--problem-graphql-params
                        "questionData"
                        (list (cons "titleSlug" slug-title)))))
         (result (aio-await (aio-url-retrieve leetcode-cn--url-graphql))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (leetcode-cn--warn "LeetCode fetch problem ERROR: %S" error-info))
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (alist-get 'question (alist-get 'data (json-read)))))))

(aio-defun leetcode-cn--api-try (problem-id slug-title code testcase)
  "Test CODE for problem which has PROBLEM-ID and SLUG-TITLE with TESTCASE."
  (leetcode-cn--debug "leetcode try slug-title: %s, problem-id: %s" slug-title problem-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent
            ,leetcode-cn--Content-Type
            ,(leetcode-cn--referer (format
                                 leetcode-cn--url-problems-submission
                                 slug-title))
            ,(cons leetcode-cn--X-CSRFToken (aio-await (leetcode-cn--csrf-token)))))
         (url-request-data
          (json-encode
           `((data_input  . ,testcase)
             (judge_type  . "small")
             (lang        . ,leetcode-cn--lang)
             (question_id . ,problem-id)
             (typed_code  . ,code)))))
    (aio-await (aio-url-retrieve (format leetcode-cn--url-try slug-title)))))

(aio-defun leetcode-cn--api-submit (problem-id slug-title code)
  "Submit CODE for problem which has PROBLEM-ID and SLUG-TITLE."
  (leetcode-cn--debug "leetcode submit slug-title: %s, problem-id: %s" slug-title problem-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent
            ,(leetcode-cn--referer (format
                                 leetcode-cn--url-problems-submission
                                 slug-title))
            ,leetcode-cn--Content-Type
            ,(cons leetcode-cn--X-CSRFToken (aio-await (leetcode-cn--csrf-token)))))
         (url-request-data
          (json-encode `((lang . ,leetcode-cn--lang)
                         (question_id . ,problem-id)
                         (typed_code . ,code)))))
    (aio-await (aio-url-retrieve (format leetcode-cn--url-submit slug-title)))))

(aio-defun leetcode-cn--api-check-submission (submission-id slug-title)
  "Polling to check submission detail.
After each submission, either try testcase or submit, LeetCode
returns a SUBMISSION-ID. With the SUBMISSION-ID, client will poll
for the submission detail. SLUG-TITLE is a slugified problem
title. Return response data if submission success, otherwise
nil."
  (leetcode-cn--loading-mode t)
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent
            ,(leetcode-cn--referer (format leetcode-cn--url-problems-submission slug-title))
            ,(cons leetcode-cn--X-CSRFToken (aio-await (leetcode-cn--csrf-token)))))
         (result (aio-await (aio-url-retrieve (format leetcode-cn--url-check-submission submission-id)))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (leetcode-cn--loading-mode -1)
          (switch-to-buffer (cdr result))
          (leetcode-cn--warn "LeetCode check submission failed: %S" error-info))
      (with-current-buffer (cdr result)
        (let ((submission-res
               (progn (goto-char url-http-end-of-headers)
                      (json-read))))
          (if (equal (alist-get 'state submission-res) "SUCCESS")
              submission-res))))))

(defun leetcode-cn--set-user-and-problems (user-and-problems)
  "Set `leetcode-cn--user' and `leetcode-cn--problems'.
If user isn't login, only `leetcode-cn--problems' will be set.
USER-AND-PROBLEMS is an alist comes from
`leetcode-cn--url-all-problems'."
  ;; user
  (let-alist user-and-problems
    (setf (leetcode-cn-user-username leetcode-cn--user) .user_name
          (leetcode-cn-user-solved leetcode-cn--user) .num_solved
          (leetcode-cn-user-easy leetcode-cn--user) .ac_easy
          (leetcode-cn-user-medium leetcode-cn--user) .ac_medium
          (leetcode-cn-user-hard leetcode-cn--user) .ac_hard)
    (leetcode-cn--debug "set user: %s, solved %s in %s problems" .user_name .num_solved .num_total)
    ;; problem list
    (setf (leetcode-cn-problems-num leetcode-cn--problems) .num_total
          (leetcode-cn-problems-tag leetcode-cn--problems) "all")
    (setf (leetcode-cn-problems-problems leetcode-cn--problems)
          (let* ((len .num_total)
                 (problems nil))
            (dotimes (i len problems)
              (let-alist (aref .stat_status_pairs i)
                (leetcode-cn--debug "frontend_question_id: %s, question_id: %s, title: %s"
                                 .stat.frontend_question_id .stat.question_id .stat.question__title)
                (push (make-leetcode-cn-problem
                       :status .status
                       :id .stat.question_id
                       :backend-id .stat.question_id
                       :title .stat.question__title
                       :acceptance (format
                                    "%.1f%%"
                                    (* 100
                                       (/ (float .stat.total_acs)
                                          .stat.total_submitted)))
                       :difficulty .difficulty.level
                       :paid-only (eq .paid_only t))
                      problems)))))))

(defun leetcode-cn--set-tags (all-tags)
  "Set `leetcode-cn--all-tags' and `leetcode-cn--problems' with ALL-TAGS."
  (let ((tags-table (make-hash-table :size 2000)))
    (let-alist all-tags
      (dolist (topic (leetcode-cn--to-list .topics))
        (let-alist topic
          ;; set leetcode-cn--all-tags
          (unless (member .slug leetcode-cn--all-tags)
            (push .slug leetcode-cn--all-tags))
          ;; tags-table cache with backend-id
          (dolist (id (leetcode-cn--to-list .questions))
            (let ((tags (gethash id tags-table)))
              (setf (gethash id tags-table) (cons .slug tags)))))))
    ;; set problems tags with tags-table
    (dolist (problem (leetcode-cn-problems-problems leetcode-cn--problems))
      (let ((backend-id (leetcode-cn-problem-backend-id problem)))
        (setf (leetcode-cn-problem-tags problem) (gethash backend-id tags-table))))))

(defun leetcode-cn--problems-rows ()
  "Generate tabulated list rows from `leetcode-cn--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (leetcode-cn-problems-problems leetcode-cn--problems))
        (easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard")
        rows)
    (dolist (p problems (reverse rows))
      (if (or leetcode-cn--display-paid
              (not (leetcode-cn-problem-paid-only p)))
          (setq rows
                (cons
                 (vector
                  ;; status
                  (if (equal (leetcode-cn-problem-status p) "ac")
                      (leetcode-cn--add-font-lock leetcode-cn--checkmark 'leetcode-cn-checkmark-face)
                    " ")
                  ;; id
                  (number-to-string (leetcode-cn-problem-id p))
                  ;; title
                  (concat
                   (leetcode-cn-problem-title p)
                   " "
                   (if (eq (leetcode-cn-problem-paid-only p) t)
                       (leetcode-cn--add-font-lock leetcode-cn--paid 'leetcode-cn-paid-face)
                     " "))
                  ;; acceptance
                  (leetcode-cn-problem-acceptance p)
                  ;; difficulty
                  (leetcode-cn--stringify-difficulty (leetcode-cn-problem-difficulty p))
                  ;; tags
                  (if leetcode-cn--display-tags (string-join (leetcode-cn-problem-tags p) ", ") ""))
                 rows))))))

(defun leetcode-cn--row-tags (row)
  "Get tags from ROW."
  (aref row 5))

(defun leetcode-cn--row-difficulty (row)
  "Get difficulty from ROW."
  (aref row 4))

(defun leetcode-cn--filter (rows)
  "Filter ROWS by `leetcode-cn--filter-regex', `leetcode-cn--filter-tag' and `leetcode-cn--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and
      (if leetcode-cn--filter-regex
          (let ((title (aref row 2)))
            (string-match-p leetcode-cn--filter-regex title))
        t)
      (if leetcode-cn--filter-tag
          (let ((tags (split-string (leetcode-cn--row-tags row) ", ")))
            (member leetcode-cn--filter-tag tags))
        t)
      (if leetcode-cn--filter-difficulty
          (let ((difficulty (leetcode-cn--row-difficulty row)))
            (string= difficulty leetcode-cn--filter-difficulty))
        t)))
   rows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode-cn-reset-filter ()
  "Reset filter."
  (interactive)
  (setq leetcode-cn--filter-regex nil)
  (setq leetcode-cn--filter-tag nil)
  (setq leetcode-cn--filter-difficulty nil)
  (leetcode-cn-refresh))

(defun leetcode-cn-set-filter-regex (regex)
  "Set `leetcode-cn--filter-regex' as REGEX and refresh."
  (interactive "sSearch: ")
  (setq leetcode-cn--filter-regex regex)
  (leetcode-cn-refresh))

(defun leetcode-cn-set-filter-tag ()
  "Set `leetcode-cn--filter-tag' from `leetcode-cn--all-tags' and refresh."
  (interactive)
  (setq leetcode-cn--filter-tag
        (completing-read "Tags: " leetcode-cn--all-tags))
  (leetcode-cn-refresh))

(defun leetcode-cn-set-prefer-language ()
  "Set `leetcode-cn-prefer-language' from `leetcode-cn--lang-suffixes' and refresh."
  (interactive)
  (setq leetcode-cn-prefer-language
        (completing-read "Language: " leetcode-cn--lang-suffixes))
  (leetcode-cn-refresh))

(defun leetcode-cn-set-filter-difficulty ()
  "Set `leetcode-cn--filter-difficulty' from `leetcode-cn--all-difficulties' and refresh."
  (interactive)
  (setq leetcode-cn--filter-difficulty
        (completing-read "Difficulty: " leetcode-cn--all-difficulties))
  (leetcode-cn-refresh))

(defun leetcode-cn-toggle-tag-display ()
  "Toggle `leetcode-cn--display-tags` and refresh"
  (interactive)
  (setq leetcode-cn--display-tags (not leetcode-cn--display-tags))
  (leetcode-cn-refresh))

(defun leetcode-cn-toggle-paid-display ()
  "Toggle `leetcode-cn--display-paid` and refresh"
  (interactive)
  (setq leetcode-cn--display-paid (not leetcode-cn--display-paid))
  (leetcode-cn-refresh))

(defun leetcode-cn--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (seq-reduce
          (lambda (acc row)
            (cl-mapcar
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          rows
          (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun leetcode-cn-refresh ()
  "Make `tabulated-list-entries'."
  (interactive)
  (let* ((header-names (append '(" " "#" "Problem" "Acceptance" "Difficulty")
                               (if leetcode-cn--display-tags '("Tags"))))
         (rows (leetcode-cn--filter (leetcode-cn--problems-rows)))
         (headers (leetcode-cn--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create leetcode-cn--buffer-name)
      (leetcode-cn--problems-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (cl-mapcar
             (lambda (i x) (list i x))
             (number-sequence 0 (1- (length rows)))
             rows))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (leetcode-cn--loading-mode -1))))

(aio-defun leetcode-cn-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (if-let ((users-and-problems (aio-await (leetcode-cn--api-fetch-user-and-problems)))
           (all-tags (aio-await (leetcode-cn--api-fetch-all-tags))))
      (progn
        (leetcode-cn--set-user-and-problems users-and-problems)
        (leetcode-cn--set-tags all-tags))
    (leetcode-cn--warn "LeetCode parse user and problems failed"))
  (setq leetcode-cn--display-tags leetcode-cn-prefer-tag-display)
  (leetcode-cn-reset-filter)
  (leetcode-cn-refresh))

(aio-defun leetcode-cn--async ()
  "Show leetcode problems buffer."
  (if (get-buffer leetcode-cn--buffer-name)
      (switch-to-buffer leetcode-cn--buffer-name)
    (unless (leetcode-cn--login-p)
      (aio-await (leetcode-cn--login)))
    (aio-await (leetcode-cn-refresh-fetch))
    (switch-to-buffer leetcode-cn--buffer-name))
  (leetcode-cn--maybe-focus))

;;;###autoload
(defun leetcode-cn ()
  "A wrapper for `leetcode-cn--async', because emacs-aio can not be autoloaded.
see: https://github.com/skeeto/emacs-aio/issues/3."
  (interactive)
  (if (leetcode-cn--check-deps)
      (leetcode-cn--async)
    (message "installing leetcode dependencies...")))

;;;###autoload(autoload 'leetcode-cn-daily "leetcode" nil t)
(aio-defun leetcode-cn-daily ()
  "Open the daily challenge."
  (interactive)
  (unless (leetcode-cn--login-p)
    (aio-await (leetcode)))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode-cn--User-Agent
            ,leetcode-cn--Content-Type
            ,(leetcode-cn--referer leetcode-cn--url-login)
            ,(cons leetcode-cn--X-CSRFToken (leetcode-cn--maybe-csrf-token))))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,leetcode-cn--url-daily-challenge)))))
    (with-current-buffer (url-retrieve-synchronously leetcode-cn--url-graphql)
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
          (leetcode-cn-show-problem (string-to-number qid)))))))

(defun leetcode-cn--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode-cn--get-slug-title (code-buf)
  "Get slug title before try or submit with CODE-BUF.
LeetCode require slug-title as the request parameters."
  (with-current-buffer code-buf
    (if leetcode-cn-save-solutions
        (file-name-base (cadr (split-string (buffer-name) "_")))
      (file-name-base (buffer-name)))))

(aio-defun leetcode-cn-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (leetcode-cn--loading-mode t)
  (let* ((code-buf (current-buffer))
         (slug-title (leetcode-cn--get-slug-title code-buf))
         (problem (leetcode-cn--get-problem slug-title))
         (problem-id (leetcode-cn-problem-id problem))
         (backend-id (leetcode-cn-problem-backend-id problem))
         (testcase-buf (get-buffer (leetcode-cn--testcase-buffer-name problem-id)))
         (result (aio-await (leetcode-cn--api-try backend-id slug-title
                                               (leetcode-cn--buffer-content code-buf)
                                               (leetcode-cn--buffer-content testcase-buf)))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (leetcode-cn--warn "LeetCode try failed: %S" error-info))
      (let ((data (with-current-buffer (cdr result)
                    (goto-char url-http-end-of-headers)
                    (json-read)))
            (result-buf (get-buffer (leetcode-cn--result-buffer-name problem-id))))
        (let-alist data
          (with-current-buffer result-buf
            (erase-buffer)
            (insert (concat "Your input:\n" .test_case "\n\n")))
          ;; poll interpreted
          (let ((actual_res (aio-await (leetcode-cn--api-check-submission .interpret_id slug-title)))
                (retry-times 0))
            (while (and (not actual_res) (< retry-times leetcode-cn--retry-times))
              (aio-await (aio-sleep 0.5))
              (setq actual_res (aio-await (leetcode-cn--api-check-submission .interpret_id slug-title)))
              (setq retry-times (1+ retry-times)))
            (if (< retry-times leetcode-cn--retry-times)
                (let-alist actual_res
                  (with-current-buffer result-buf
                    (goto-char (point-max))
                    (cond
                     ((eq .status_code 10)
                      (insert "Output:\n")
                      (dotimes (i (length .code_answer))
                        (insert (aref .code_answer i))
                        (insert "\n"))
                      (insert "\n")
                      (insert "Expected:\n")
                      (dotimes (i (length .expected_code_answer))
                        (insert (aref .expected_code_answer i))
                        (insert "\n"))
                      (insert "\n"))
                     ((eq .status_code 14)
                      (insert .status_msg))
                     ((eq .status_code 15)
                      (insert (leetcode-cn--add-font-lock .status_msg 'leetcode-cn-error-face))
                      (insert "\n\n")
                      (insert .full_runtime_error))
                     ((eq .status_code 20)
                      (insert (leetcode-cn--add-font-lock .status_msg 'leetcode-cn-error-face))
                      (insert "\n\n")
                      (insert .full_compile_error)))
                    (when (> (length .code_output) 0)
                      (insert "\n\n")
                      (insert "Code output:\n")
                      (dolist (item (append .code_output nil))
                        (insert (concat item "\n"))))
                    (insert "\n\n")))
              (leetcode-cn--warn "LeetCode try timeout.")))
          (leetcode-cn--loading-mode -1))))))

(defun leetcode-cn--solving-window-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |     Detail     |
|               |                |
|               +----------------+
|     Code      |   Customize    |
|               |   Testcases    |
|               +----------------+
|               |Submit/Testcases|
|               |    Result      |
+---------------+----------------+"
  (delete-other-windows)
  (setq leetcode-cn--description-window (split-window-horizontally))
  (other-window 1)
  (setq leetcode-cn--testcase-window (split-window-below))
  (other-window 1)
  (setq leetcode-cn--result-window (split-window-below))
  (other-window -1)
  (other-window -1))

(defun leetcode-cn--display-result (buffer &optional alist)
  "Display function for LeetCode result.
BUFFER is used to show LeetCode result. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode-cn--display-testcase (buffer &optional alist)
  "Display function for LeetCode testcase.
BUFFER is used to show LeetCode testcase. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode-cn--display-detail (buffer &optional alist)
  "Display function for LeetCode detail.
BUFFER is used to show LeetCode detail. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-top-child
                 (window-next-sibling
                  (window-left-child
                   (frame-root-window))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode-cn--display-code (buffer &optional alist)
  "Display function for LeetCode code.
BUFFER is the one to show LeetCode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode-cn--show-submission-result (problem-id submission-detail)
  "Show error info in `leetcode-cn--result-buffer-name' based on status code.
Error info comes from SUBMISSION-DETAIL.

STATUS_CODE has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (let-alist submission-detail
    (with-current-buffer (get-buffer-create (leetcode-cn--result-buffer-name problem-id))
      (erase-buffer)
      (font-lock-mode +1)
      (cond
       ((eq .status_code 10)
        (insert (format "Status: %s\n\n"
                        (leetcode-cn--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-cn-accepted-face)))
        (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                        .status_runtime .runtime_percentile .pretty_lang))
        (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                        .status_memory .memory_percentile .pretty_lang)))
       ((eq .status_code 11)
        (insert (format "Status: %s\n\n"
                        (leetcode-cn--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-cn-error-face)))
        (insert (format "Test Case: \n%s\n\n" .input))
        (insert (format "Answer: %s\n\n" .code_output))
        (insert (format "Expected Answer: %s\n\n" .expected_output))
        (unless (string-empty-p .std_output)
          (insert (format "Stdout: \n%s\n" .std_output))))
       ((eq .status_code 14)
        (insert (format "Status: %s" (leetcode-cn--add-font-lock .status_msg 'leetcode-cn-error-face)))
        (insert "\n"))
       ((eq .status_code 15)
        (insert (format "Status: %s" (leetcode-cn--add-font-lock .status_msg 'leetcode-cn-error-face)))
        (insert "\n\n")
        (insert (format .full_runtime_error)))
       ((eq .status_code 20)
        (insert (format "Status: %s" (leetcode-cn--add-font-lock .status_msg 'leetcode-cn-error-face)))
        (insert "\n\n")
        (insert (format .full_compile_error))))
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         leetcode-cn--display-result)
                        (reusable-frames . visible))))))

(aio-defun leetcode-cn-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (leetcode-cn--loading-mode t)
  (let* ((code-buf (current-buffer))
         (code (leetcode-cn--buffer-content code-buf))
         (slug-title (leetcode-cn--get-slug-title code-buf))
         (problem (leetcode-cn--get-problem slug-title))
         (problem-id (leetcode-cn-problem-id problem))
         (backend-id (leetcode-cn-problem-backend-id problem))
         (result (aio-await (leetcode-cn--api-submit backend-id slug-title code))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (leetcode-cn--loading-mode -1)
          (switch-to-buffer (cdr result))
          (leetcode-cn--warn "LeetCode check submit failed: %S" error-info))
      (let* ((resp
              (with-current-buffer (cdr result)
                (progn (goto-char url-http-end-of-headers)
                       (json-read))))
             (submission-id (alist-get 'submission_id resp))
             (submission-res (aio-await (leetcode-cn--api-check-submission submission-id slug-title)))
             (retry-times 0))
        ;; poll submission result
        (while (and (not submission-res) (< retry-times leetcode-cn--retry-times))
          (aio-await (aio-sleep 0.5))
          (setq submission-res (aio-await (leetcode-cn--api-check-submission submission-id slug-title)))
          (setq retry-times (1+ retry-times)))
        (if (< retry-times leetcode-cn--retry-times)
            (leetcode-cn--show-submission-result problem-id submission-res)
          (leetcode-cn--warn "LeetCode submit timeout."))
        (leetcode-cn--loading-mode -1)))))

(defun leetcode-cn--show-problem (problem problem-info)
  "Show the detail of PROBLEM, whose meta data is PROBLEM-INFO.
Use `shr-render-buffer' to render problem detail. This action
will show the detail in other window and jump to it."
  (let* ((problem-id (leetcode-cn-problem-id problem-info))
         (title (leetcode-cn-problem-title problem-info))
         (difficulty-level (leetcode-cn-problem-difficulty problem-info))
         (difficulty (leetcode-cn--stringify-difficulty difficulty-level))
         (buf-name (leetcode-cn--detail-buffer-name problem-id))
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (leetcode-cn--debug "select title: %s" title)
    (leetcode-cn--maybe-focus)
    (let-alist problem
      (when (get-buffer buf-name)
        (kill-buffer buf-name))
      (with-temp-buffer
        (insert (concat "<h1>" (number-to-string problem-id) ". " title "</h1>"))
        (insert (concat (capitalize difficulty) html-margin
                        "likes: " (number-to-string .likes) html-margin
                        "dislikes: " (number-to-string .dislikes)))
        ;; Sometimes LeetCode don't have a '<p>' at the outermost...
        (insert "<p>" .content "</p>")
        (setq shr-current-font t)
        (leetcode-cn--replace-in-buffer "" "")
        ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
        (leetcode-cn--replace-in-buffer "https" "http")
        (shr-render-buffer (current-buffer)))
      (with-current-buffer "*html*"
        (save-match-data
          (re-search-forward "dislikes: .*" nil t)
          (insert (make-string 4 ?\s))
          (insert-text-button "Solve it"
                              'action (lambda (btn)
                                        (leetcode-cn--start-coding problem problem-info))
                              'help-echo "Solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Chinese Link"
                              'action (lambda (btn)
                                        (browse-url (leetcode-cn--problem-link title)))
                              'help-echo "Open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "English Link"
                              'action (lambda (btn)
                                        (browse-url (leetcode--problem-link title)))
                              'help-echo "Open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Chinese Solution"
                              'action (lambda (btn)
                                        (browse-url (concat (leetcode-cn--problem-link title) "/solution")))
                              'help-echo "Open the problem solution page in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "English Solution"
                              'action (lambda (btn)
                                        (browse-url (concat (leetcode--problem-link title) "/solution")))
                              'help-echo "Open the problem solution page in browser."))
        (rename-buffer buf-name)
        (leetcode-cn--problem-detail-mode)
        (switch-to-buffer (current-buffer))
        (search-backward "Solve it")))))

(aio-defun leetcode-cn-show-problem (problem-id)
  "Show the detail of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
detail. This action will show the detail in other window and jump
to it."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode-cn--get-current-problem-id))))
  (let* ((problem-info (leetcode-cn--get-problem-by-id problem-id))
         (title (leetcode-cn-problem-title problem-info))
         (problem (aio-await (leetcode-cn--api-fetch-problem title))))
    (leetcode-cn--show-problem problem problem-info)))

(defun leetcode-cn-show-problem-by-slug (slug-title)
  "Show the detail of problem with slug title.
This function will work after first run M-x leetcode. Get problem
by id and use `shr-render-buffer' to render problem detail. This
action will show the detail in other window and jump to it.

It can be used in org-link elisp:(leetcode-cn-show-problem-by-slug \"3sum\")."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode-cn--get-current-problem-id))))
  (let* ((problem (seq-find (lambda (p)
                              (equal slug-title
                                     (leetcode-cn--slugify-title
                                      (leetcode-cn-problem-title p))))
                            (leetcode-cn-problems-problems leetcode-cn--problems)))
         (problem-id (leetcode-cn-problem-id problem))
         (problem-info (leetcode-cn--get-problem-by-id problem-id))
         (title (leetcode-cn-problem-title problem-info))
         (problem  (leetcode-cn--api-fetch-problem title)))
    (leetcode-cn-show-problem problem-id)))

(defun leetcode-cn-show-current-problem ()
  "Show current problem's detail.
Call `leetcode-cn-show-problem' on the current problem id. This
action will show the detail in other window and jump to it."
  (interactive)
  (leetcode-cn-show-problem (leetcode-cn--get-current-problem-id)))

(aio-defun leetcode-cn-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `leetcode-cn-show-problem', but instead of jumping to
the detail window, this action will jump back in `LC Problems'."
  (interactive (list (read-number "View problem by problem id: "
                                  (leetcode-cn--get-current-problem-id))))
  (aio-await (leetcode-cn-show-problem problem-id))
  (leetcode-cn--jump-to-window-by-buffer-name leetcode-cn--buffer-name))

(defun leetcode-cn-view-current-problem ()
  "View current problem while staying in `LC Problems' window.
Similar with `leetcode-cn-show-current-problem', but instead of
jumping to the detail window, this action will jump back in `LC
Problems'."
  (interactive)
  (leetcode-cn-view-problem (leetcode-cn--get-current-problem-id)))

(defun leetcode-cn-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (read-number "Show in browser by problem id: "
                                  (leetcode-cn--get-current-problem-id))))
  (let* ((problem (leetcode-cn--get-problem-by-id problem-id))
         (title (leetcode-cn-problem-title problem))
         (link (leetcode-cn--problem-link title)))
    (leetcode-cn--debug "open in browser: %s" link)
    (browse-url link)))

(defun leetcode-cn-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `leetcode-cn-show-problem-in-browser' on the current problem id."
  (interactive)
  (leetcode-cn-show-problem-in-browser (leetcode-cn--get-current-problem-id)))

(aio-defun leetcode-cn-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-number "Solve the problem with id: "
                                  (leetcode-cn--get-current-problem-id))))
  (let* ((problem-info (leetcode-cn--get-problem-by-id problem-id))
         (title (leetcode-cn-problem-title problem-info))
         (problem (aio-await (leetcode-cn--api-fetch-problem title))))
    (leetcode-cn--show-problem problem problem-info)
    (leetcode-cn--start-coding problem problem-info)))

(defun leetcode-cn-solve-current-problem ()
  "Start coding the current problem.
Call `leetcode-cn-solve-problem' on the current problem id."
  (interactive)
  (leetcode-cn-solve-problem (leetcode-cn--get-current-problem-id)))

(defun leetcode-cn--jump-to-window-by-buffer-name (buffer-name)
  "Jump to window by BUFFER-NAME."
  (select-window (get-buffer-window buffer-name)))

(defun leetcode-cn--kill-buff-and-delete-window (buf)
  "Kill BUF and delete its window."
  (delete-windows-on buf t)
  (kill-buffer buf))

(defun leetcode-cn-quit ()
  "Close and delete leetcode related buffers and windows."
  (interactive)
  (leetcode-cn--kill-buff-and-delete-window (get-buffer leetcode-cn--buffer-name))
  (mapc (lambda (title)
          (leetcode-cn--kill-buff-and-delete-window
           (get-buffer (leetcode-cn--get-code-buffer-name title)))
          (let* ((slug-title (leetcode-cn--slugify-title title))
                 (problem (leetcode-cn--get-problem slug-title))
                 (problem-id (leetcode-cn-problem-id problem)))
            (leetcode-cn--kill-buff-and-delete-window (get-buffer (leetcode-cn--detail-buffer-name problem-id)))
            (leetcode-cn--kill-buff-and-delete-window (get-buffer (leetcode-cn--result-buffer-name problem-id)))
            (leetcode-cn--kill-buff-and-delete-window (get-buffer (leetcode-cn--testcase-buffer-name problem-id)))))
        leetcode-cn--problem-titles))

(defun leetcode-cn--set-lang (snippets)
  "Set `leetcode-cn--lang' based on langSlug in SNIPPETS."
  (setq leetcode-cn--lang
        ;; if there is a mysql snippet, we use mysql as our prefer language.
        (if (seq-find (lambda (s)
                        (equal (alist-get 'langSlug s)
                               leetcode-cn-prefer-sql))
                      snippets)
            leetcode-cn-prefer-sql
          leetcode-cn-prefer-language)))

(defun leetcode-cn--get-code-buffer-name (title)
  "Get code buffer name by TITLE and `leetcode-cn-prefer-language'."
  (let* ((suffix (assoc-default
                  leetcode-cn--lang
                  leetcode-cn--lang-suffixes))
         (slug-title (leetcode-cn--slugify-title title))
         (title-with-suffix (concat slug-title suffix)))
    (if leetcode-cn-save-solutions
        (format "%04d_%s" (leetcode-cn--get-problem-id slug-title) title-with-suffix)
      title-with-suffix)))

(defun leetcode-cn--get-code-buffer (buf-name)
  "Get code buffer by BUF-NAME."
  (if (not leetcode-cn-save-solutions)
      (get-buffer-create buf-name)
    (unless (file-directory-p leetcode-cn-directory)
      (make-directory leetcode-cn-directory))
    (find-file-noselect
     (concat (file-name-as-directory leetcode-cn-directory)
             buf-name))))

(defun leetcode-cn--get-problem (slug-title)
  "Get problem from `leetcode-cn--problems' by SLUG-TITLE."
  (seq-find (lambda (p)
              (equal slug-title
                     (leetcode-cn--slugify-title
                      (leetcode-cn-problem-title p))))
            (leetcode-cn-problems-problems leetcode-cn--problems)))

(defun leetcode-cn--get-problem-by-id (id)
  "Get problem from `leetcode-cn--problems' by ID."
  (seq-find (lambda (p)
              (equal id (leetcode-cn-problem-id p)))
            (leetcode-cn-problems-problems leetcode-cn--problems)))

(defun leetcode-cn--get-problem-id (slug-title)
  "Get problem id by SLUG-TITLE."
  (let ((problem (leetcode-cn--get-problem slug-title)))
    (leetcode-cn-problem-id problem)))

(defun leetcode-cn--get-current-problem-id ()
  "Get id of the current problem."
  (string-to-number (aref (tabulated-list-get-entry) 1)))

(defun leetcode-cn--start-coding (problem problem-info)
  "Create a buffer for coding PROBLEM with meta-data PROBLEM-INFO.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-cn-prefer-language'and `auto-mode-alist'."
  (let-alist problem
    (let* ((title (leetcode-cn-problem-title problem-info))
           (problem-id (leetcode-cn-problem-id problem-info))
           (testcase-buf-name (leetcode-cn--testcase-buffer-name problem-id))
           (result-buf-name (leetcode-cn--result-buffer-name problem-id))
           (snippets (append .codeSnippets nil))
           (testcase .sampleTestCase))
      (add-to-list 'leetcode-cn--problem-titles title)
      (leetcode-cn--solving-window-layout)
      (leetcode-cn--set-lang snippets)
      (let* ((slug-title (leetcode-cn--slugify-title title))
             (code-buf-name (leetcode-cn--get-code-buffer-name title))
             (code-buf (leetcode-cn--get-code-buffer code-buf-name))
             (suffix (assoc-default
                      leetcode-cn--lang
                      leetcode-cn--lang-suffixes)))
        (if (= (buffer-size code-buf) 0)
            (with-current-buffer code-buf
              (setq code-buf (current-buffer))
              (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
              (leetcode-cn-solution-mode t)
              (let* ((snippet (seq-find (lambda (s)
                                          (equal (alist-get 'langSlug s)
                                                 leetcode-cn--lang))
                                        snippets))
                     (template-code (alist-get 'code snippet)))
                (unless (save-mark-and-excursion
                          (goto-char (point-min))
                          (search-forward (string-trim template-code) nil t))
                  (insert template-code))
                (leetcode-cn--replace-in-buffer "" "")))
          (with-current-buffer code-buf
            (leetcode-cn-solution-mode t)))

        (display-buffer code-buf
                        '((display-buffer-reuse-window
                           leetcode-cn--display-code)
                          (reusable-frames . visible))))
      (with-current-buffer (get-buffer-create testcase-buf-name)
        (erase-buffer)
        (insert testcase)
        (set-window-buffer leetcode-cn--testcase-window (current-buffer)))
      (with-current-buffer (get-buffer-create result-buf-name)
        (erase-buffer)
        (set-window-buffer leetcode-cn--result-window (current-buffer))))))

(aio-defun leetcode-cn-restore-layout ()
  "This command should be run in LeetCode code buffer.
It will restore the layout based on current buffer's name."
  (interactive)
  (let* ((slug-title (file-name-sans-extension (buffer-name)))
         (problem (leetcode-cn--get-problem slug-title))
         (problem-id (leetcode-cn-problem-id problem))
         (desc-buf (get-buffer (leetcode-cn--detail-buffer-name problem-id)))
         (testcase-buf (get-buffer-create (leetcode-cn--testcase-buffer-name problem-id)))
         (result-buf (get-buffer-create (leetcode-cn--result-buffer-name problem-id))))
    (leetcode-cn--solving-window-layout)
    (unless desc-buf
      (aio-await (leetcode-cn-show-problem problem-id)))
    (display-buffer desc-buf
                    '((display-buffer-reuse-window
                       leetcode-cn--display-detail)
                      (reusable-frames . visible)))
    (display-buffer testcase-buf
                    '((display-buffer-reuse-window
                       leetcode-cn--display-testcase)
                      (reusable-frames . visible)))
    (display-buffer result-buf
                    '((display-buffer-reuse-window
                       leetcode-cn--display-result)
                      (reusable-frames . visible)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problems Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode-cn--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-cn-show-current-problem)
      (define-key map (kbd "TAB") #'leetcode-cn-view-current-problem)
      (define-key map "o" #'leetcode-cn-show-current-problem)
      (define-key map "O" #'leetcode-cn-show-problem)
      (define-key map "v" #'leetcode-cn-view-current-problem)
      (define-key map "V" #'leetcode-cn-view-problem)
      (define-key map "b" #'leetcode-cn-show-current-problem-in-browser)
      (define-key map "B" #'leetcode-cn-show-problem-in-browser)
      (define-key map "c" #'leetcode-cn-solve-current-problem)
      (define-key map "C" #'leetcode-cn-solve-problem)
      (define-key map "s" #'leetcode-cn-set-filter-regex)
      (define-key map "L" #'leetcode-cn-set-prefer-language)
      (define-key map "t" #'leetcode-cn-set-filter-tag)
      (define-key map "T" #'leetcode-cn-toggle-tag-display)
      (define-key map "P" #'leetcode-cn-toggle-paid-display)
      (define-key map "d" #'leetcode-cn-set-filter-difficulty)
      (define-key map "g" #'leetcode-cn-refresh)
      (define-key map "G" #'leetcode-cn-refresh-fetch)
      (define-key map "r" #'leetcode-cn-reset-filter)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode-cn--problems-mode'.")

(define-derived-mode leetcode-cn--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-cn-refresh nil t)
  :group 'leetcode-cn
  :keymap leetcode-cn--problems-mode-map)

(defun leetcode-cn--set-evil-local-map (map)
  "Set `evil-normal-state-local-map' to MAP."
  (when (featurep 'evil)
    (define-key map "h" nil)
    (define-key map "v" nil)
    (define-key map "V" nil)
    (define-key map "b" nil)
    (define-key map "B" nil)
    (define-key map "g" nil)
    (define-key map "G" nil)
    (define-key map "z" #'leetcode-cn-refresh)
    (define-key map "Z" #'leetcode-cn-refresh-fetch)
    (setq evil-normal-state-local-map map)))

(add-hook 'leetcode-cn--problems-mode-hook #'hl-line-mode)
(add-hook 'leetcode-cn--problems-mode-hook
          (lambda () (leetcode-cn--set-evil-local-map leetcode-cn--problems-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Detail Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode-cn--problem-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode-cn--problem-detail-mode'.")

(define-derived-mode leetcode-cn--problem-detail-mode
  special-mode "LC Detail"
  "Major mode for display problem detail."
  :group 'leetcode-cn
  :keymap leetcode-cn--problem-detail-mode-map)

(add-hook 'leetcode-cn--problem-detail-mode-hook
          (lambda () (leetcode-cn--set-evil-local-map leetcode-cn--problem-detail-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Loading Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Use spinner.el to show progress indicator
(defvar leetcode-cn--spinner (spinner-create 'progress-bar-filled)
  "Progress indicator to show request progress.")
(defconst leetcode-cn--loading-lighter
  '(" [LeetCode" (:eval (spinner-print leetcode-cn--spinner)) "]"))

(define-minor-mode leetcode-cn--loading-mode
  "Minor mode to showing leetcode loading status."
  :require 'leetcode
  :lighter leetcode-cn--loading-lighter
  :group 'leetcode-cn
  (if leetcode-cn--loading-mode
      (spinner-start leetcode-cn--spinner)
    (spinner-stop leetcode-cn--spinner)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solution Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode-cn-solution-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-t") #'leetcode-cn-try)
      (define-key map (kbd "C-c C-s") #'leetcode-cn-submit)
      (define-key map (kbd "C-c C-r") #'leetcode-cn-restore-layout)))
  "Keymap for `leetcode-cn-solution-mode'.")

(define-minor-mode leetcode-cn-solution-mode
  "Minor mode to provide shortcut and hooks."
  :require 'leetcode
  :lighter " LC-Solution"
  :group 'leetcode-cn
  :keymap leetcode-cn-solution-mode-map)

(provide 'leetcode-cn)
;;; leetcode-cn.el ends here
