;;; Code:

(with-eval-after-load 'dashboard
  (defun dashboard-due-date-for-agenda ()
    (let ((my-dashboard-agenda-days 90))
      (if dashboard-week-agenda
          (time-add (current-time) (* 86400 my-dashboard-agenda-days))
        (time-add (current-time) 86400))
      )
    )
  )
