;;; defassoclist.el --- 
;; 
;; Filename: defassoclist.el
;; Description: 
;; Author: Neil Smithline
;; Maintainer: 
;; Copyright (C) 2012, Neil Smithline, all rights reserved.;; Created: Sat Apr 28 00:01:21 2012 (-0400)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'defhook)

(defmacro* defassoclist (symbol &key
                                (test #'equal) test-not key
                                (rtest #'equal) rtest-not rkey
                                initvalue docstring cons-cell)
  "Create assoc list helper functions for SYMBOL.
SYMBOL will be defined with `defvar' and passed the values of the
keywords INITVALUE and DOCSTRING. See `defvar' for more
information.

SYMBOL is an assoc list whose entries are either a list or,
when :CONS-CELL is non-nil, a `cons' cell.

`defassoclist' defines the following functions:
Accessor Functions:
    SYMBOL-assoc        Search SYMBOL for key.
                        Passes :TEST, :TEST-NOT, and :KEY to `assoc*'.

    SYMBOL-rassoc       Search SYMBOL for value.
                        Passes :RTEST, :RTEST-NOT, and :RKEY to `rassoc*'
                        as :TEST, :TEST-NOT, and :KEY.

    As the remaining functions use these access functions when
    examining the contents of SYMBOL, the behavior of all
    generated functions will be based on the values of these
    keywords.

    :TEST and :RTEST default to `equal' to maintain compatibility
    with `assoc' and `rassoc'. The remaning keywords default to
    null. These defaults will be sufficient in most
    circumstances. A common optimization is, when appropriate,
    setting :TEST to `eq' to provide better performance.

    Further information about these keywords can be found in the
    Info node `(cl)Sequence Basics' and the Info
    node `(cl)Association Lists'.

Deletion Functions:
    SYMBOL-remove       Non-destructively remove an entry from SYMBOL.

    SYMBOL-delete       Destructively remove an entry from SYMBOL.

Assignment Functions:
    SYMBOL-set          Set an entry in SYMBOL.

    SYMBOL-add          Add an entry to SYMBOL.
                        Generate an error if key already exists in SYMBOL.

    SYMBOL-replace      Replace an entry in SYMBOL.
                        Generate an error if is not already in SYMBOL.

    When :CONS-CELL is nil, its default value, the assignment
    functions will have an argument list defined as:
        (KEY &REST VALUE)
    and the entries in the assoc list SYMBOL will be list items.

    When :CONS-CELL is t, the assignment functions will have an
    argument list defined as:
        (KEY VALUE)
    and the entries in the assoc list SYMBOL will be a `cons' cell.

Detailed example Using `defassoclist' on `minor-mode-alist':
    You should skim the documentation for `minor-mode-alist' if
    you are unfamiliar with it.

    To use `defassoclist' on `minor-mode-alist', the declaration is:

        (defassoclist minor-mode-alist :test #'eq)

    A detailed explanation of the argument values is included below.

    Once declared, some example uses of the `minor-mode-alist'
    assoc list are:

        Find the first entry for `flyspell-minor-mode':
            (minor-mode-alist-assoc 'flyspell-minor-mode)

        Add a new entry for `my-minor-mode':
            (minor-mode-alist-add 'my-minor-mode \" My\")
        This will generate an error if `my-minor-mode' is in the
        assoc list when called.

        Change an existing value for `my-minor-mode':
            (minor-mode-alist-replace 'my-minor-mode \" Your\")
        This will generate an error if `my-minor-mode' is not in
        the assoc list when called. 

        Set a value for `my-minor-mode', not caring if one already exists:
            (minor-mode-alist-set 'my-minor-mode)
        
        Remove all entries for `my-minor-mode':
            (minor-mode-alist-remove 'my-minor-mode)

        See which minor mode puts \" Fill\" in the mode line:
            (minor-mode-alist-rassoc \" Fill\")


    Detailed explanation of the argument values for
    `defassoclist' when used on `minor-mode-alist':

    SYMBOL          The assoc list's symbol: minor-mode-alist

    TEST            The default value of `equal' will work but, being
                    that the `car's of the items in the list are all symbols,
                    it is more efficient to use `eq'.

    TEST-NOT        The default value of null is almost always correct.

    KEY             The default value of null is almost always correct.

    RTEST           Being that the second element of `minor-mode-alist'
                    can be strings as well as other types, the
                    default value of `equal' is appropriate.

    RTEST-NOT       The default value of null is almost always correct.

    RKEY            The default value of null is almost always correct.

    INITVALUE       As `minor-mode-alist' is already defined, this
                    will have no effect since `defvar' does not redefine
                    a variable. Except when defining a new assoc list,
                    the default value of null is likely correct.

    DOCSTRING       As `minor-mode-alist' is already documented and we
                    do not wish to change the document string, we use the
                    default value of null.

    CONS-CELL       Use the default value of null since `minor-mode-alist'
                    entries are lists of two items."
  
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
         (my-cons-cell          cons-cell)
         (assoc-sym             (intern (concat my-symname "-assoc")))
         (rassoc-sym            (intern (concat my-symname "-rassoc")))
         (set-sym               (intern (concat my-symname "-set")))
         (replace-sym           (intern (concat my-symname "-replace")))
         (add-sym               (intern (concat my-symname "-add")))
         (remove-sym            (intern (concat my-symname "-remove")))
         (delete-sym            (intern (concat my-symname "-delete"))))

    `(progn
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defvar ,my-symbol ,my-initvalue ,my-docstring)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,assoc-sym (key)                         
         ,(format "Execute `assoc*' on %s with KEY." 
                  my-symname)
         (assoc* key ,my-symbol
                 :key #',my-key :test #',my-test :test-not #',my-test-not))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,rassoc-sym (value)
         ,(format "Execute `rassoc*' on %s with VALUE."
                  my-symname)
         (rassoc* value ,my-symbol
                  :key #',my-rkey :test #',my-rtest :test-not #',my-rtest-not))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,remove-sym (key)
         ,(format "Non-destructively remove all entries matching KEY in %s."
                  my-symname)
         ;; See comment for delete-sym below.
         (let ((match))
           (while (setq match (,assoc-sym key))
             (setq ,my-symbol (remove match ,my-symbol))))
         ,my-symbol)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,delete-sym (key)
         ,(format "Destructively delete all entries matching KEY in %s."
                  my-symname)
         ;; I'm sure that an implementation similar to
         ;; `assq-delete-all' would be more efficient but I'd rather
         ;; not monkey around with the code complexity.
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
         ;; small, linear increase is a big deal in the general case.
         ;; If you are implementing a performance intensive function,
         ;; perhaps you should not use `defassoclist'.
         (let ((match))
           (while (setq match (,assoc-sym key))
             (setq ,my-symbol (delete match ,my-symbol))))
         ,my-symbol)

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,set-sym (key 
                        ,@(if my-cons-cell '(value) '(&rest value)))
         ,(format
           (concat
            "Set KEY to VALUE in assoc list `%s', avoiding duplicates.\n"
            "`%s' will be used to remove any duplicates.")
           my-symname
           (symbol-name ',remove-sym))
         (,remove-sym key)
         (setq ,my-symbol (cons (cons key value) (,remove-sym key))))

       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,add-sym (key
                        ,@(if my-cons-cell '(value) '(&rest value)))
         ,(format
           (concat
            "Set KEY to VALUE in assoc list `%s', not allowing replacement.\n"
            "An error will be generated if an entry for KEY already\n"
            "exists in `%s'.")
           my-symname
           my-symname)
         (when (,assoc-sym key)
           (error "An entry for \"%s\" already exists in `%s'."
                  key
                  ,my-symname))
         (setq ,my-symbol (cons (cons key value) (,remove-sym key))))

       
       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (defun ,replace-sym (key
                            ,@(if my-cons-cell '(value) '(&rest value))
                            &key:test ,my-test :test-not ,my-test-not
                            :key ,my-key)
         ,(format
           (concat
            "Replace an existing entry for KEY in `%s' with VALUE.\n"
            "An error will be generated if an entry for KEY does not\n"
            "previously exist in `%s'.")
           my-symname
           my-symname)
         (unless (,assoc-sym key)
           (error "No entry for \"%s\" exists in `%s'."
                  key
                  ,my-symname))
         (setq ,my-symbol (cons (cons key value) (,remove-sym key))))

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
