;;; defassoclist.el --- Provide object-oriented functions for assoc lists
;; Author: Neil Smithline
;; Maintainer: 
;; Copyright (C) 2012, Neil Smithline, all rights reserved.
;; Created: Sat Apr 28 00:01:21 2012 (-0400)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: internal, lisp, oop, assoc, alist
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `defhook', `cl-macs'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; `defassoclist' creates assoc list helper functions that allow assoc
;; lists to be accessed in an object-oriented fashion.
;;
;; A typical use of `defassoclist' is:
;;      (defassoclist SYMBOL)
;;
;; After this declaration, `defassoclist' will have created several
;; helper functions for the assoc list SYMBOL. `defassoclist' will
;; preserve the value, if any, of SYMBOL.
;;
;; `defassoclist' defines SYMBOL using `defvar' with the optional
;; keyword arguments INITVALUE and DOCSTRING. (See `defvar' for more
;; information.)
;;
;; See note about idempotency at the end of the commentary.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINED FUNCTION DOCUMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `defassoclist' defines the following functions for accessing SYMBOL
;; in a more object-oriented fashion:
;;
;; Accessor Functions:
;;     SYMBOL-assoc        Search SYMBOL for key.
;;
;;     SYMBOL-rassoc       Search SYMBOL for value.
;;
;;     SYMBOL-get          An alias for SYMBOL-assoc that facilitates
;;                         the use of the assoc list via the object-oriented
;;                         paradigm of using `set' and `get' functions.
;;
;;     The generated accessor functions have an argument list of:
;;          (KEY)
;;
;; Deletion Functions:
;;     SYMBOL-remove       Non-destructively remove an entry from SYMBOL.
;;
;;     SYMBOL-delete       Destructively remove an entry from SYMBOL.
;;
;;     The generated accessor deletion have an argument list of:
;;          (KEY)
;;
;; Assignment Functions:
;;     SYMBOL-set          Set an entry in SYMBOL. When this call completes, 
;;                         the entry will be the first in the list.
;;
;;     SYMBOL-append       Set an entry in SYMBOL. When this call completes,
;;                         the entry will be the last in the list.
;;
;;     SYMBOL-add          Like SYMBOL-set but an error occurs if key
;;                         _already_ exists in SYMBOL.
;;
;;     SYMBOL-replace      Like SYMBOL-set but an error occurs if key
;;                         _does not_ exists in SYMBOL.
;;
;;     The generated assignment functions have an argument list of:
;;          (KEY &REST VALUES)
;;
;;     The Info node `(cl)Sequence Basics' provides additional
;;     information about the handling of assoc lists.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYWORD DOCUMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; :TEST, :TEST-NOT, :KEY
;;
;;      These keywords are used during the standard lookup of items
;;      via `SYMBOL-get' and `SYMBOL-assoc'. With the exception that
;;      :TEST defaults to `equal' rather than `eql', these keywords
;;      correspond to the keywords in `assoc*'.
;;
;;      Further information about these keywords can be found in the
;;      Info node `(cl)Association Lists'.
;;
;; :RTEST, :RTEST-NOT, :KEY
;;
;;      These keywords are used during the reverse lookup of items via
;;      `SYMBOL-rassoc'. They correspond to the :TEST, :TEST-NOT, and
;;      :KEY, respectively.
;;
;;
;; :DOCSTRING
;;
;;      An optional string that will be passed to `defvar' during the
;;      creation of the assoc list as the documentation for SYMBOL. If
;;      :DOCSTRING is null the documentation for SYMBOL will be
;;      unchanged.
;;
;; :INITVALUE
;;
;;      An optional value that will be passed to `defvar' during the
;;      creation of the assoc list. See `defvar' and the :FORCEINIT
;;      keyword for details about how :INITVALUE behaves.
;;
;; :FORCEINIT
;;
;;      When :FORCEINIT is non-null, after calling `defvar' to define
;;      SYMBOL, `defassoclist' will use `setq' to set SYMOBL to
;;      :INITVALUE, erasing any existing value of SYMBOL.
;;
;; :CONSP
;;
;;      In Emacs there are two types of assoc lists. The first type is
;;      the `list' type. Each item in the `list' type of assoc lists
;;      is itself a list. The variable `minor-mode-alist' is an
;;      example of the `list' type.
;;
;;      The second type of assoc lists are the `cons' type. In these
;;      assoc lists, each item of the assoc list is a cons cell. The
;;      variable `auto-mode-alist' is an example of the `cons' type.
;;
;;      If SYMBOL is a `list' type of assoc list then the value of the
;;      :CONSP keyword should be null, its default value. If SYMBOL is
;;      a `cons'-type assoc list, then :CONSP should be non-null.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A DETAILED EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A detailed example using `defassoclist' on `minor-mode-alist':
;;     You should skim the documentation for `minor-mode-alist' if
;;     you are unfamiliar with it.
;;
;;     To use `defassoclist' on `minor-mode-alist', the declaration is:
;;
;;         (defassoclist minor-mode-alist :test #'eq)
;;
;;     A detailed explanation of the argument values is included below.
;;
;;     Once declared, some example uses of the `minor-mode-alist'
;;     assoc list are:
;;
;;         Find the first entry for `flyspell-minor-mode':
;;             (minor-mode-alist-assoc 'flyspell-minor-mode)
;;
;;         Add a new entry for `my-minor-mode':
;;             (minor-mode-alist-add 'my-minor-mode \" My\")
;;         This will generate an error if `my-minor-mode' is in the
;;         assoc list when called.
;;
;;         Change an existing value for `my-minor-mode':
;;             (minor-mode-alist-replace 'my-minor-mode \" Your\")
;;         This will generate an error if `my-minor-mode' is not in
;;         the assoc list when called. 
;;
;;         Set a value for `my-minor-mode', not caring if one already exists:
;;             (minor-mode-alist-set 'my-minor-mode)
;;
;;         Remove all entries for `my-minor-mode':
;;             (minor-mode-alist-remove 'my-minor-mode)
;;
;;         See which minor mode puts \" Fill\" in the mode line:
;;             (minor-mode-alist-rassoc \" Fill\")
;;
;; Detailed explanation of the argument values for
;;     `defassoclist' when used on `minor-mode-alist':
;;
;;      SYMBOL          The assoc list's symbol: minor-mode-alist
;;
;;      TEST            The default value of `equal' will work but, being
;;                     that the `car's of the items in the list are all symbols,
;;                     it is more efficient to use `eq'.
;;
;;      TEST-NOT        The default value of null is almost always correct.
;;
;;      KEY             The default value of null is almost always correct.
;;
;;      RTEST           Being that the second element of `minor-mode-alist'
;;                     can be strings as well as other types, the
;;                     default value of `equal' is appropriate.
;;
;;      RTEST-NOT       The default value of null is almost always correct.
;;
;;      RKEY            The default value of null is almost always correct.
;;
;;      INITVALUE       As `minor-mode-alist' is already defined, this
;;                     will have no effect since `defvar' does not
;;                     redefine a variable. Except when defining a new
;;                     assoc list, or :FORCEINIT is non-null. As
;;                     `minor-mode-alist' likely has a pre-existing
;;                     value that we want to keep, :INITVALUE won't be
;;                     used leaving.
;;
;;      FORCEINIT       In order to retain the existing value of
;;                     `minor-mode-alist', we will leave this as null.
;;
;;
;;      DOCSTRING       As `minor-mode-alist' is already documented and we
;;                     do not wish to change the document string, we use the
;;                     default value of null.
;;
;;      CONSP           As `minor-mode-alist' expects its entries to be lists,
;;                     (ie: it is a `list' type of assoc list) the default
;;                     value of null is appropriate. 
;; 
;; It should be noted that it is safe to use `defassoclist' to declare
;; the same SYMBOL as an assoc list multiple times. `defassoclist' is
;; idempotent provided you do not use the :DOCSTRING, :INITVALUE, and
;; :FORCEINIT keywords.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDEMPOTENCY (ie: repeated uses of `defassoclist' on the same symbol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using `defassoclist' multiple times on SYMBOL is always safe if you
;; provide no keys to the `defassoclist' calls. Including non-default
;; values for the keys can affect the assoc list and its functions as
;; follows:
;;
;;     TEST, TEST-NOT, KEY, RTEST, RTEST-NOT, RKEY
;;
;;          Changing the value of any of these keys will dramatically
;;          affect the behavior of the generated functions. In
;;          general, just don't.
;;
;;     CONSP
;;
;;          In almost all circumstances, change the value of CONSP in
;;          `defassoclist' will lead to problems. The most obvious
;;          execption is if the `defassoclist' passes a non-null value
;;          for FORCEINIT.
;;
;;      DOCSTRING
;;
;;          When there are multiple `defassoclist' declarations for
;;          the same SYMBOL, the documentation for SYMBOL will be the
;;          value of the most recent non-null DOCSTRING value. That
;;          is, different DOCSTRING values will change SYMBOL's
;;          documentation.
;;
;;      INITVALUE
;;
;;          Being that INITVALUE will not change the value of SYMBOL
;;          if it has one, multiple `defassoclist' declarations with
;;          one or more INITVALUE values works. The only caveat is
;;          that if you are passing different values to the INITVALUE
;;          keyword, only the first will have an affect (see `defvar'
;;          for details).
;;
;;      FORCEINIT
;;
;;          When `defassoclist' is passed a non-null FORCEINIT value,
;;          `defassoclist' behaves as a `setq' has been called on
;;          SYMBOL, assigning it INITVALUE. As long as you remember
;;          that a non-null FORCEINIT is equivalent to a `setq', using
;;          this is safe.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'defhook)

(defmacro* defassoclist (symbol &key
                                (test #'equal) test-not key
                                (rtest #'equal) rtest-not rkey
                                initvalue forceinit docstring
                                (consp nil))
  "Create assoc list helper functions for SYMBOL.
SYMBOL will be defined with `defvar' and passed the values of the
keywords INITVALUE and DOCSTRING. See `defvar' for more
information.

SYMBOL is an assoc list.

Use the \\[finder-commentary] command with `defassoclist' as the
argument for more details."
  ;; Ensure single evaluation of arguments
  (let* ((my-symname            (symbol-name symbol))
         (my-symbol             (intern my-symname))
         (my-test               test)
         (my-test-not           test-not)
         (my-key                key)
         (my-rtest              rtest)
         (my-rtest-not          rtest-not)
         (my-rkey               rkey)
         (my-docstring          docstring)
         (my-initvalue          initvalue)
         (my-forceinit          forceinit)
         (commentary-doc        (concat
                                 "\n\nUse the \\[finder-commentary] command "
                                 "with `defassoclist' as the\n"
                                 "argument for more details."))
         (assoc-sym             (intern (concat my-symname "-assoc")))
         (get-sym               (intern (concat my-symname "-get")))
         (rassoc-sym            (intern (concat my-symname "-rassoc")))
         (set-sym               (intern (concat my-symname "-set")))
         (append-sym            (intern (concat my-symname "-append")))
         (replace-sym           (intern (concat my-symname "-replace")))
         (add-sym               (intern (concat my-symname "-add")))
         (remove-sym            (intern (concat my-symname "-remove")))
         (delete-sym            (intern (concat my-symname "-delete")))
         (value-getter-sym      (if (not consp) 'identity
                                  (lambda (l)
                                    (if (and l (cdr l))
                                        (let ((ll (last l 2))) (setcdr ll (cadr ll)) l)
                                      (car l))))))

    `(progn
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Create the list with defvar
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defvar ,my-symbol ,my-initvalue ,my-docstring)
       (when ,my-forceinit (setq ,my-symbol ,my-initvalue))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Accessors
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,assoc-sym (key)                         
         ,(format "Execute `assoc*' on `%s' with KEY.%s" 
                  my-symname commentary-doc)
         (assoc* key ,my-symbol
                 :key #',my-key :test #',my-test :test-not #',my-test-not))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defalias ',get-sym #',assoc-sym)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,rassoc-sym (value)
         ,(format "Execute `rassoc*' on `%s' with VALUE.%s"
                  my-symname commentary-doc)
         (rassoc* value ,my-symbol
                  :key #',my-rkey :test #',my-rtest :test-not #',my-rtest-not))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Removers
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,remove-sym (key)
         ,(format "Non-destructively remove all entries matching KEY in `%s'.%s"
                  my-symname commentary-doc)
         ;; See comment for delete-sym below.
         (let ((match))
           (while (setq match (,assoc-sym key))
             (setq ,my-symbol (remove match ,my-symbol))))
         ,my-symbol)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,delete-sym (key)
         ,(format "Destructively delete all entries matching KEY in `%s'.%s"
                  my-symname commentary-doc)
         ;; I'm sure that an implementation similar to
         ;; `assq-delete-all' would be more efficient but I'd rather
         ;; not monkey around with the extra code complexity.
         ;; `assq-delete-all's walks the list exactly once.
         ;;
         ;; Assuming that there are no duplicates in the list, this
         ;; implementation will walk the list once to find the entry
         ;; to delete, once to find the previous entry when deleting
         ;; it, and one more time to confirm that there are no
         ;; duplicates.
         ;;
         ;; That makes worst case of my implementation only 3x that of
         ;; the `assq-delete-all' implementation. I don't think that a
         ;; small, constant multiple increase is a big deal in the
         ;; general case. If you are implementing a performance
         ;; intensive assoc list, perhaps you should not use
         ;; `defassoclist' or even better, you can fork this on
         ;; Github, fix this up, and send me a pull request.
         (let ((match))
           (while (setq match (,assoc-sym key))
             (setq ,my-symbol (delete match ,my-symbol))))
         ,my-symbol)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Mutators
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,set-sym (key &rest values)
         ,(format
           (concat
            "Set KEY to VALUES in assoc list `%s', avoiding duplicates.\n"
            "After executing, KEY will be the first item in the assoc list.\n"
            "`%s' will be used to remove any duplicates.%s")
           my-symname
           (symbol-name remove-sym)
           commentary-doc)
         (setq ,my-symbol (cons (cons key (,value-getter-sym values)) (,remove-sym key))))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,append-sym (key &rest values)
         ,(format
           (concat
            "Set KEY to VALUES in assoc list `%s', avoiding duplicates.\n"
            "After executing, KEY will be the last item in the assoc list.\n"
            "`%s' will be used to remove any duplicates.%s")
           my-symname
           (symbol-name remove-sym)
           commentary-doc)
         (,remove-sym key)
         (setq ,my-symbol (append ,my-symbol (list (cons key (,value-getter-sym values))))))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,add-sym (key &rest values)
         ,(format
           (concat
            "Set KEY to VALUES in assoc list `%s', not allowing replacement.\n"
            "After executing, KEY will be the first item in the assoc list.\n"
            "\n"
            "An error will be generated if an entry for KEY already\n"
            "exists in `%s'.%s")
           my-symname my-symname commentary-doc)
         (when (,assoc-sym key)
           (error "An entry for \"%s\" already exists in `%s'."
                  key
                  ,my-symname))
         (setq ,my-symbol (cons (cons key (,value-getter-sym values)) (,remove-sym key))))
       
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,replace-sym (key &rest values)
         ,(format
           (concat
            "Replace an existing entry for KEY in `%s' with VALUES.\n"
            "After executing, KEY will be the first item in the assoc list.\n"
            "\n"
            "An error will be generated if an entry for KEY does not\n"
            "previously exist in `%s'.%s")
           my-symname my-symname commentary-doc)
         (unless (,assoc-sym key)
           (error "No entry for \"%s\" exists in `%s'."
                  key
                  ,my-symname))
         (setq ,my-symbol (cons (cons key (,value-getter-sym values)) (,remove-sym key))))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Return the symbol, not the value, of the assoc list. 
       ',my-symbol)))

(defconst defassoclist-font-lock-keywords 
  '(("(\\(defassoclist\\)[[:space:]]+\\([-[:word:]]+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))
  "`font-lock-mode' regexp for `defassoclist' in `emacs-lisp-mode'.")

(let ((defassoclist-user-prefix "defassoclist"))
  (defhook add-defassoclist-keywords (emacs-lisp-mode-hook)
    "Add `defassoclist' keywords to `emacs-lisp-mode's `font-lock-keywords'."
    (font-lock-add-keywords nil defassoclist-font-lock-keywords)))

(provide 'defassoclist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defassoclist.el ends here
