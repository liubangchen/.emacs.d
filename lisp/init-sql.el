;;; -*- lexical-binding: t; -*-
(use-package sql-indent)
(add-hook 'sql-mode-hook (lambda() (sqlind-minor-mode t)))

(setq sql-connection-alist
      '((test
         (sql-product 'mysql)
         (sql-server "127.0.0.1")
         (sql-user "root")
         (sql-password "123")
         (sql-database "")
         (sql-port 3306))
        ))

(defun mysql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(defun mysql-localhost ()
  (interactive)
  (mysql-connect 'mysql 'test))


(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (company-mode t)
            (set-face-attribute 'region nil :background "#99CC00" :foreground "#ffffff")
            ))

(use-package docker-compose-mode)


(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END.
Dependency:
npm i -g sql-formatter-cli"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun sql-beautify-buffer ()
  "Beautify SQL in buffer."
  (interactive)
  (sql-beautify-region (point-min) (point-max)))

(add-hook 'sql-mode-hook '(lambda ()
                            ;; beautify region or buffer
                            (local-set-key (kbd "C-c t") 'sql-beautify-region)))


(provide 'init-sql)
