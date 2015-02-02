(provide 'logmode)
(require 'f)

(defcustom simulation-log-folder "C:/ProgramData/WcmSim/users/e.hughes/logs/"
  "Location of the simulation logs"
  :type 'directory)
(defun logmode/list-log-items () (mapcar #'logmode/transform-pair (directory-files simulation-log-folder t "^[0-9]+T[0-9]+.*?\.log$")))

(defun logmode/join (&rest l)
  (mapconcat #'identity l " "))

(defun logmode/split (cand sym)
  (let ((split (split-string cand sym)))
    (lambda (n) (elt split n))))

((lambda (n) (+ 1 n)) 2)

(defun logmode/transform (cand) (let
                                    ((split (split-string cand "_")))
                                  (let ((date (elt (split-string (elt split 0) "T") 0))
                                        (time (elt (split-string (elt split 0) "T") 1))
                                        (server (elt split 2))
                                        (port (elt split 3))
                                        (pid (elt (split-string (elt split 4) "\.") 1)))
                                    (logmode/join date time server port))))

(defun logmode/transform-pair (cand)
  (cons (logmode/transform (f-filename cand)) cand))

(logmode/transform-pair  "20141113T12503_Remote_WINT-APP-VM120_4313_2132.log")

(logmode/list-log-items)

(defun logmode/helm ()
  (interactive)
  (helm :sources  '((name . "Simulation Logs")
                    (candidates . logmode/list-log-items)
                    (action ("Open" . (lambda (candidate) (find-file-read-only candidate)))
                            ("Delete" . (lambda (candidate) (delete-file candidate)))))))
