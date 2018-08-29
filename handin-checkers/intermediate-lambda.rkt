(module checker handin-server/checker

  (require handin-server/private/reloadable)

  (check: :language '(special intermediate-lambda)
          :require '(2htdp/image 2htdp/universe spd/tags)
          :create-text? #t
          :textualize? #t
          :eval? #t
          :coverage? #f

          (let ([the-gradefn (auto-reload-procedure `(file "../../gradefn.rkt") 'gradefn)])
	    (call-with-output-file* "grading/errlog"
				    (lambda (errlog)
				      (reraise-exn-as-submission-problem
				       (lambda ()
					 (message (the-gradefn "handin" ;dom is built from here, so has to be read-syntax able
							       ""
							       (submission-eval)
							       add-header-line!
							       (lambda (msg) (writeln msg errlog) (flush-output errlog)))
						  '(ok)))))
				    #:exists 'replace))))
