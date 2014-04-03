(require 'prodigy)

(prodigy-define-service
  :name "Bone"
  :command "bundle"
  :args '("exec" "rails" "server")
  :cwd "~/_dev/adaptly/adaptly-bone"
  :tags '(work rails))

(prodigy-define-service
  :name "CG AMQP"
  :command "bundle"
  :args '("exec" "ruby" "script/amqp.rb" "run")
  :cwd "~/_dev/adaptly/campaign-grouper"
  :tags '(work)
  :on-output (lambda (service output)
	       (when (s-matches? "myapp_daemon: process with pid [0-9]+ started." output)
		 (prodigy-set-status service 'ready))))

(prodigy-define-tag
  :name 'thin
  :on-output (lambda (service output)
	       (when (s-matches? "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop" output)
		 (prodigy-set-status service 'ready))))

(prodigy-define-tag
  :name 'webrick
  :on-output (lambda (service output)
	       (when (s-matches? "WEBrick::HTTPServer#start: pid=[0-9]+ port=[0-9]+" output)
		 (prodigy-set-status service 'ready))))

(prodigy-define-tag
  :name 'mongrel
  :on-output (lambda (service output)
	       (when (s-matches? "Ctrl-C to shutdown server" output)
		 (prodigy-set-status service 'ready))))

(prodigy-define-tag
  :name 'rails
  :tags '(thin mongrel webrick))

(prodigy-define-service
  :name "Readuction"
  :command "bundle"
  :args '("exec" "rails" "server")
  :cwd "~/_dev/_projects/adaptive-reader"
  :tags '(work rails))
