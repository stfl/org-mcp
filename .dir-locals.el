;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;;; org-mcp.el carries a narrower `fill-column' of its own, the width
;;; scripts/format-elisp.el lays it out for.

((emacs-lisp-mode . ((fill-column . 100)
                     (indent-tabs-mode . nil)
                     (elisp-lint-indent-specs . ((org-mcp--modify-and-save . 3)
                                                 (org-mcp--with-uri-prefix-dispatch . 1)
                                                 (org-mcp--with-org-file . 1)
                                                 (org-mcp--with-private-kill-ring . 0)
                                                 (org-mcp--logging-note . 1)
                                                 (org-mcp--repeat-catching-up . 0)
                                                 (org-mcp-test--assert-error-and-file . 1)
                                                 (org-mcp-test--with-enabled . defun)
                                                 (org-mcp-test--with-config . 1)
                                                 (org-mcp-test--with-temp-org-file . 2)
                                                 (org-mcp-test--with-add-todo-setup . 2)
                                                 (org-mcp-test--with-id-tracking . 2)
                                                 (org-mcp-test--with-id-setup . 2)
                                                 (org-mcp-test--with-verbs-file . 1)
                                                 (org-mcp-test--with-verbs-files . 2)
                                                 (org-mcp-test--with-dirty-buffer . 2)
                                                 (org-mcp-test--get-tag-config-and-check . defun)
                                                 (mcp-server-lib-ert-with-server . defun))))))
