; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         mud-telnet-wsk.el
; RCS:          $Header: /repo/public.cvs/app/lambda/src/contrib/mud-telnet-wsk.el,v 1.1 2004/11/13 21:42:43 bruce Exp $
; Description:  Combo of all mud.el's that I found
; Author:       William S. Kaster
; Created:      Fri Aug 26 11:35:01 1994
; Modified:     Sat Oct 22 14:14:26 1994 (William S. Kaster) wsk@purple.mayfield.hp.com
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; (C) Copyright 1994, SGML Skunkworks, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Major Mode for talking to MUDs
;;; by James Aspnes (asp@cs.cmu.edu), Stewart Clamen (clamen@cs.cmu.edu)
;;; and Pavel Curtis (pavel@parc.xerox.com)
;;; 1989, 1990, 1991, 1992
;;;
;;; MODIFICATION HISTORY
;;; 
;;; May/June 1990 - Generalized to handle SMUG and LPMUD
;;; 
;;; January  1991 - Added Pavel Curtis' MOO support and assorted bug fixes, 
;;;                   also changed process-status call to run under 18.56.
;;;
;;; February 1991 - Added input-history browsing support (M-p/M-n);
;;;                   commands for sending the latest item on the kill
;;;                   ring (C-c C-y), optionally with each line bracketed by a
;;;                   given prefix and suffix (C-u C-c C-y); and a command to
;;;                   clear the current input line (C-c C-u).
;;;                 Added support for adding/overriding server definitions
;;;                   and the default server in one's .mud file.
;;;                 Fixed some bugs.
;;;                 Added support for people who prefer that the type-in
;;;                   point for a MUD buffer stay glued to the bottom of the
;;;                   window (see the 'mud-use-entire-window' option).
;;;
;;; October  1992 - Added support for server-initiated creation of local
;;;		      editing windows, manipulating text to be stored back on
;;;		      the server.  At present, this is done as part of the
;;;		      MOO-specific support; at some point I'll pull it out to
;;;		      make it generic across MUD types.
;;;
;;;      May 1994 - Merge all these (mud.el's) I can find, added moo-code.el
;;;                 support, well... the (require... line.
;  Merged in highlight code from afo.
  ;;; February 1994 - Added support for emacs V19 and hilit19
  ;;;                 (Aaron Oppenheimer afo@wal.hp.com)
  ;;;                 To use hilit19 support, put this in your .emacs *after*
  ;;;                 you load hilit19.el:
  ;;;                    (hilit-translate mud-player-name         'cyan)
  ;;;                    (hilit-translate mud-page                'yellow)
  ;;;                    (hilit-set-mode-patterns
  ;;;                       'mud-interactive-mode
  ;;;                       '((".*page.*" nil mud-page))
  ;;;                       nil
  ;;;                       t)

;;;  wsk  May 1995 - Merged in Pavel's latest changes from ftp.parc.

;;;  wsk  June 1995 - Fix for hilighting bug encountered when ^U M-x mud
;;;                   was used without having a character.
(if window-system
    (require 'hilit19)
)

;; This is the default set of 'server' lines in MUD entry files (e.g., ~/.mud)
(defconst mud-default-servers
  '(

;;;
;;; SERVER NAME		MUD	SITE				PORT
;;;
    ;; On Brigadoon days
    ("TinyMUD Classic"	TinyMUD	"fuzine.mt.cs.cmu.edu"		2323)
    ("fuzine"		TinyMUD	"fuzine.mt.cs.cmu.edu"		4201)
    ("TinyHELL" 	TinyMUD	"uokmax.ecn.uoknor.edu"		6250)

    ("AmosMUD"		TinyMUD	"amos.ucsd.edu" 		4201)
    ("Atlantis"		TinyMUD	"nyquist.bellcore.com"		4201)
    ("Auroris" 		TinyMUD	"quiche.cs.mcgill.ca"		4200)
    ("CITMUD" 		TinyMUD	"chrome.caltech.edu"		4201)
    ("ClubMUD"		TinyMUD "milton.u.washington.edu"	1984)
    ("dragonMUD"        TinyMUD "satan.ucsd.edu"                4201)
    ("Eden"		TinyMUD "unicorn.cc.wwu.edu"		4201)
    ("EVILMud" 		TinyMUD "convx1.ccit.arizona.edu"	4201)
    ("FantaMUD"		TinyMUD "sage.cc.purdue.edu"		5950)
    ("FurryMUCK"	TinyMUD "hobbes.catt.ncsu.edu"		4242)
    ("GenericMUD"	TinyMUD "apex.yorku.ca"			4201)
    ("Islandia" 	TinyMUD	"planck.physics.purdue.edu"	2323)
    ("MoonMUD" 		TinyMUD	"granite.cs.rochester.edu"	4201)
    ("MumbleMUD" 	TinyMUD	"berlin.rtp.dg.com"		4201)
    ("PoohMUD" 		TinyMUD	"eeyore.caltech.edu"		4201)
    ("QuartzPARADISE"   TinyMUD "quartz.rutgers.edu"		9999)
    ("StoMUD" 		TinyMUD	"dagon.acc.stolaf.edu"		8888)
    ("SunMUD"		TinyMUD "einstein.mpccl.ksu.edu"	4201)
    ("TinyCWRU"		TinyMUD	"solarium.scl.cwru.edu.edu"	4201)
    ("TinyHOLLAND" 	TinyMUD	"fysae.fys.ruu.nl"		4201)
    ("TinyHORNS" 	TinyMUD	"bashful.cc.utexas.edu"		4201)
    ("TinyMUD"	 	TinyMUD	"planck.physics.purdue.edu"	2323)
    ("TinyMush"		TinyMUD	"sigh.berkeley.edu"		4201)
    ("TinySWAT" 	TinyMUD	"masada.cs.swarthmore.edu"	4201)
    ("TinyTIM" 		TinyMUD	"grape.ecs.clarkson.edu"	6250)
    ("TinyUSC" 		TinyMUD	"coyote.usc.edu"		4201)
    ("Tinywonk"		TinyMUD "ux.acs.umn.edu"		4200)
    ("TinyWORLD"	TinyMUD "rillonia.ssc.gov"		6250)

;   ("SMUG" 		SMUG	"lancelot"			4201)

    ("Anarchy!"		TinyMUCK "galjoen.cs.vu.nl"		4201)
    ("Brigadoon"	TinyMUCK "dante.cs.uiuc.edu"		4201)    
    ("Chaos!"	 	TinyMUCK "uokmax.ecn.uoknor.edu"	6250)
;   ("MbongoMUCK"	TinyMUCK "mbongo.ucsd.edu"		4201)
    ("MbongoMUCK"	TinyMUCK "watpc13.ucr.edu"		4201)
    ("Pegasus"	 	TinyMUCK "l_cae05.icaen.uiowa.edu"	2001)
    ("TigerMUCK"	TinyMUCK "Sun1.forestry.auburn.edu"	6250)
    ("TroyMUCK"		TinyMUCK "pawl24.pawl.rpi.edu"		4201)

    ("TinyMUSH"		TinyMUSH "manray.CSUFresno.edu"		4201)
    ("ToonMUSH"		TinyMUSH "uokmax.ecn.uoknor.edu"	4835)
    ("Pern"             TinyMUSH "199.166.230.69"               4201)

    ("MaineMud"		LPMUD	"chevette.umcs.maine.edu"	2000)
    ("Darker Realms"	LPMUD	"worf.tamu.edu"			2000)
    ("Sanctuary"	LPMUD	"j.ms.uky.edu"			2000)
    ("Warhammer"	LPMUD	"issunbane.engrg.uwo.ca"	2112)
    ("The PIT"		LPMUD	"obie.cis.pitt.edu"		2000)
    ("Theive's World"   LPMUD   "uokmax.ecn.uoknor.edu"		2000)
    ("Avalon"		LPMUD	"el.ecn.purdue.edu"		2000)
    ("Boiling MUD"	LPMUD	"frey.nu.oz.au"			2000)
    ("Phoenix"		LPMUD	"galjas.cs.vu.nl"		2000)

    ("PMC"              MOO     "hero.village.virginia.edu"     7777) 
    ("DiversityMOO"     MOO     "erau.db.erau.edu"              8888)
    ("AlphaMOO"		MOO	"belch.berkeley.edu"		7777)
    ("LambdaMOO"	MOO	"lambda.parc.xerox.com"		8888)
    ("MOO2000"          MOO     "sunlab.npac.syr.edu"           2000)
    ("ChibaSprawl"	MOO	"sequoia.picosof.com"		7777)
    ("WorldMOO"  	MOO	"world.sensemedia.net"          1234)
    ("ChaosMOO"         MOO     "cub.math.oxy.edu"              8888)
    ("cybermoo" 	MOO	"umbio.med.miami.edu"		7777)
    ("MediaMOO"         MOO     "purple-crayon.media.mit.edu"   8888)
    ("JaysHouseMOO"     MOO     "theory.cs.mankato.msus.edu"    1709)
    ("PARC"		Jupiter	"osprey.parc.xerox.com"		7777)
;JHM  at  jhm.ccs.neu.edu 1709
;LambdaMOO  at  lambda.xerox.com 8888
;Foothills  at  128.197.10.75 2010
;Harper's_Tale  at  netman.widener.edu 8888
;BayMOO  at  moo.crl.com 8888
;Rivendel  at  128.8.11.201 5000
;WorldMOO  at  chiba.picosof.com 1234
;Sprawl  at  chiba.picosof.com 7777
;SyrinxMOO  at  cimsun.aidt.edu 2112
;Eon  at  mcmuse.mc.maricopa.edu 8888
;Entropy  at  monsoon.weather.brockport.edu 7777
;Dhalgren  at  actinic.princeton.edu 7777
;DreamMOO  at  Feenix.metronet.com 8888
;Dfire-Dragonsfire  at  moo.eskimo.com 7777
;DivursityU  at  erau.db.erau.edu 8888
;MarlDOOM  at  seabass.st.usm.edu 7777
;MOOsaico  at  moo.di.uminho.pt 7777
;MuMoo  at  chestnut.enmu.edu 7777
;TNGMOO  at  tng-moo.dungeon.com 1701
;trekmoo  at  trekmoo.microserve.com 2499
;PtMOOt  at  128.83.194.17 8888
;PMC  at  hero.village.virginia.edu 7777

    ))


(defvar mud-default-default-server "LambdaMOO"
  "Default 'default-server' name.")

(defvar muds nil "List of all defined MUD types")

(defmacro defmud (mud prompt connect-filter connect-command
		      filters command-filters sentinels
		      startup-hook page-regexp)
  (list 'progn
	(list 'defvar mud nil)
	(list 'setplist (list 'quote mud) nil)
	(list 'put (list 'quote mud) ''prompt prompt)
	(list 'put (list 'quote mud) ''connect-filter connect-filter)
	(list 'put (list 'quote mud) ''connect-command connect-command)
	(list 'put (list 'quote mud) ''filters filters)
	(list 'put (list 'quote mud) ''command-filters command-filters)
	(list 'put (list 'quote mud) ''sentinels sentinels)
	(list 'put (list 'quote mud) ''startup-hook startup-hook)
	(list 'put (list 'quote mud) ''page-regexp page-regexp)
	(list 'if (list 'not (list 'memq   (list 'quote mud) 'muds))
	      (list  'setq 'muds (list 'cons  (list 'quote mud) 'muds)))
	(list 'quote mud)))


(defun mud-prompt () (get mud 'prompt))
(defun mud-connect-filter () (get mud 'connect-filter))
(defun mud-connect-command () (get mud 'connect-command))
(defun mud-filters () (get mud 'filters))
(defun mud-sentinels () (get mud 'sentinels))
(defun mud-command-filters () (get mud 'command-filters))
(defun mud-startup-hook () (get mud 'startup-hook))
(defun mud-page-regexp () (get mud 'page-regexp))

;;; Equivalent mud types
(defmacro eqmud (mud2 mud1)
  (list 'progn
	(list
	 'setplist (list 'quote mud2) (list 'symbol-plist (list 'quote mud1)))
	(list 'if (list 'not (list 'memq   (list 'quote mud2) 'muds))
	      (list 'setq 'muds (list 'cons  (list 'quote mud2) 'muds)))))


(defmud TinyMUD
  ?>					; prompt char
  'tinymud-connect-filter
  "connect"
  'tinymud-filter-hook
  'nil
  'nil
  'tinymud-mode-hook
  "\\(You sense that [^ ]* is looking for you in \\|\\w+ pages: \\)"
 )

(eqmud TinyMUCK TinyMUD)
(eqmud TinyMUSH TinyMUD)
(eqmud TeenyMUD TinyMUD)

(defmud SMUG
  ?=
  'nil
  ""
  'smug-filter-hook
  'smug-macro-command-filter-hook
  'nil
  'smug-mode-hook
  "You sense that [^ ]* is looking for you in "
 )

(defmud LPMUD
  ?\ 					; prompt char
  nil
  ""
  nil
  nil
  nil
  'tinymud-mode-hook
  "You sense that [^ ]* is looking for you in "
 )


(defmud MOO
  ?>
  'tinymud-connect-filter
  "connect"
  'moo-filter-hook
  'nil
  'nil
  'moo-mode-hook
  ".* pages?\\(, \"\\|: \\)\\|-> .*\\|.* responds?, \".*\\|(from .*).*"
  )
;  "[^ ]+ [^ ]* *pages[,:] \""

(defmud ZENMOO
  ?>
  'tinymud-connect-filter
  "connect"
  'zenmoo-filter-hook
  'nil
  'nil
  'moo-mode-hook
  "[^ ]+ pages?[,:] \""
  )


(defmud Jupiter
  ?>
  'tinymud-connect-filter
  "connect"
  'jupiter-filter-hook
  'nil
  'jupiter-sentinel-hook
  'moo-mode-hook
  "\\(You sense that [^ ]* is looking for you in \\)"
  )

(eqmud JupiterX Jupiter)

(defvar mud-jupiter-client-prog "/project/jupiter/bin/jswitch"
  "*File name of the special client program for Jupiter servers.")

(defvar mud-show-page nil
  "*If non-nil, pop up MUD buffer whenever a page arrives.")

(defvar mud-reconnect-regexp
  "#### Please reconnect to \\([^@]*\\)@\\([^ @]*\\) *\\(\\|([^ @]*)\\) port \\([0-9]+\\) ####.*$"
  "Regular expression for detecting reconnect signals.")

(defconst mud-new-connectionp nil
  "Flag to identify hail for new connection")

(defvar mud-accept-reconnects nil
  "*If nil, reject reconnect signals. If non-nil, accept reconnect signals 
by breaking existing connection and establishing new connection.  If an
integer, spawn <n> connections before breaking any.")

(defun mud-check-reconnect ()
  "Look for reconnect signal and open new connection if non to that
site already exists."
  (goto-char (point-min))
  (while (not (eobp))
    (if (and mud-accept-reconnects (looking-at mud-reconnect-regexp))
	(let ((mud-name (buffer-substring (match-beginning 1)
					  (match-end 1)))
	      (mud-server-addr (buffer-substring (match-beginning 2)
						 (match-end 2)))
	      (mud-server (and (not (eq (match-beginning 3)
					(match-end 3)))
			       (buffer-substring (1+ (match-beginning 3))
						 (1- (match-end 3)))))
	      (mud-port (string-to-int
			 (buffer-substring (match-beginning 4)
					   (match-end 4)))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (let* ((mud-sys (assoc mud-name (mud-servers)))
		 (mud-buffer-name (concat "*" mud-name "*"))
		 (mud-buffer-process
		  (mud-find-existing-process mud-name)))

	    (cond
	     (mud-buffer-process	; Existing connection to that site...
	      (message "Connection to that site had already been established.")
	      (pop-to-buffer (process-buffer mud-buffer-process)))
	     ((not mud-server)
	      (message "GNU Emacs cannot handle nonsymbolic names.  Sorry."))
	     ((zerop mud-port)
	      (message "Illformed portal signal. Inform Builder."))
	     (t
	      (save-excursion
		(setq mud-new-connectionp mud-buffer-name)
		(open-mud mud-sys t)))))))
    (beginning-of-line 2)))


(defun mud-find-existing-process (name)
  "Find process of established Mud connection, if it exists"
  (let ((processes (process-list))
	(result nil))
    (while (and processes (not result))
      (if (string-equal (upcase (process-name (car processes)))
			(upcase name))
	  (setq result (car processes))
	(setq processes (cdr processes))))
    result))



(defun mud-check-page ()
  "Look for page message, and pop-up buffer if specified."
  (goto-char (point-min))
  (while (not (eobp))
    (if (and mud-show-page (looking-at (mud-page-regexp)))
	(progn
	  (make-frame-visible)
	  (display-buffer (current-buffer))
	  (ding)
	  (message "You are being paged in %s"
		   (buffer-name (current-buffer)))))
    (beginning-of-line 2))
)

; line wrap?
(defvar mud-break-lines t
  "*Non-nil if lines of output from the MUD should be broken as necessary at word boundaries.  (buffer-local)")

(make-variable-buffer-local 'mud-break-lines)

(defun mud-fill-lines ()
  "Fill buffer line by line."
  (if mud-break-lines
      (save-restriction
	(goto-char (point-max))
	(beginning-of-line)
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((break (move-to-column (1+ fill-column))))
	    (if (<= break fill-column)
		(beginning-of-line 2)
	      ;; else fill
	      (skip-chars-backward "^ \n")
	      (if (bolp)
		  ;; can't fill, we lose
		  (beginning-of-line 2)
		(delete-horizontal-space)
		(insert ?\n))))))))


(defun mud-filter (proc string)
  "Filter for input from MUD process.  Calls MUD-specific filters as well. 
Also, if recently established new connection automatically, check to see 
if number of active connections exceeded connection limit and delete 
current process if so." 
  (let ((mud-select-buffer nil))
    (save-excursion
      ;; Occasionally-useful debugging code.
      '(progn
	 (set-buffer (get-buffer-create "*MUD Packets*"))
	 (goto-char (point-max))
	 (insert "\n\n<<")
	 (insert string)
	 (insert ">>"))
      (set-buffer (process-buffer proc))
      (goto-char (marker-position (process-mark proc)))
      (let ((start (point)))
	(insert-before-markers string)
	(let ((end (point)))
	  (if (featurep 'hilit19)
	      (hilit-highlight-region start end 'nil t))
	  (goto-char start)
	  (beginning-of-line nil)
	  (save-restriction
	    (narrow-to-region (point) end)
	    (while (search-forward "\^m" nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (run-hooks (mud-filters))))))
    (if (and (= scroll-step 1)
	     (eq (current-buffer) (process-buffer proc))
	     (= (point) (point-max)))
	(recenter -1))
    (if (and mud-select-buffer
	     (eq (current-buffer) (process-buffer proc)))
	(pop-to-buffer mud-select-buffer)))
  (cond (mud-new-connectionp
	 (if (or			; Do we close current connection?
	      (not (numberp mud-accept-reconnects))
	      (let ((c mud-accept-reconnects)
		    (l (process-list)))
		(while l
		  (if (and (eq (process-filter (car l)) 'mud-filter)
			   (memq (process-status (car l)) '(open run)))
		      (setq c (1- c)))
		  (setq l (cdr l)))
		(< c 0)))
	     (progn
	       (delete-process (get-buffer-process (current-buffer)))
	       (kill-buffer (current-buffer))))
	 (pop-to-buffer mud-new-connectionp)
	 (if (> baud-rate search-slow-speed) (recenter))
	 (setq mud-new-connectionp nil))))

(defun mud-sentinel (proc change)
  "Called on state changes so hooks can get run."
  (run-hooks (mud-sentinels)))

(defun mud-send ()
  "Send current line of input to a MUD."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (cond ((or (null proc)
	       (not (memq (process-status proc) '(open run))))
	   (message "Not connected--- nothing sent.")
	   (insert ?\n))
	  (t
	   ;; process exists, send line
	   (let ((start (mud-find-input)))
	     (send-region proc start (point))
	     (send-string proc "\n")
	     (mud-remember-input (buffer-substring start (point)))
	     (goto-char (point-max))
	     (insert ?\n)
	     (move-marker (process-mark proc) (point))
	     (insert (mud-prompt))
	     (if (= scroll-step 1)
		 (recenter -1))
	     )))))

(defun mud-realign-and-send ()
  (interactive)
  (recenter 0)
  (mud-send))

(defun mud-cancel-input ()
  (interactive)
  (let ((start (mud-find-input)))
    (delete-region start (point))))

(defun mud-send-kill (arg)
  (interactive "P")
  (if arg
      (call-interactively 'mud-send-kill-prefix)
    (let ((proc (get-buffer-process (current-buffer))))
      (mud-send-string (car kill-ring) proc))))

(defun mud-send-kill-prefix (prefix suffix)
  (interactive "sPrefix: \nsSuffix: ")
  (let ((buf (current-buffer))
	(temp (generate-new-buffer " *MUD temp*")))
    (save-excursion
      (set-buffer temp)
      (yank)
      (let ((proc (get-buffer-process buf))
	    (case-replace nil))
	(goto-char (point-min))
	(untabify (point-min) (point-max))
	(while (re-search-forward "^\\(.*\\)$" nil t)
	  (replace-match (concat prefix "\\1" suffix) t))
	(send-region proc (point-min) (point-max))
	(send-string proc "\n")		;; Flush remaining input
	(kill-buffer temp)))))

(defun mud-quit ()
  "Quit MUD process."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to quit this MUD session?")
      (delete-process (get-buffer-process (current-buffer)))))

(defconst mud nil
  "Variable representing type of MUD active in current buffer")
(make-variable-buffer-local 'mud)

(defvar mud-use-entire-window nil
  "*Try to keep the type-in point for a MUD buffer at the bottom of the window.")

(defvar mud-mode-syntax-table nil
  "Syntax table used while in MUD mode.")

(defvar mud-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'mud-realign-and-send)
    (define-key map "\r" 'mud-send)
    (define-key map "\^c\^c" 'mud-quit)
    (define-key map "\^c " 'mud-make-empty-buffer)
    (define-key map "\^c\^m" 'mud-macro-command)
    (define-key map "\^c\^u" 'mud-cancel-input)
    (define-key map "\^c\^y" 'mud-send-kill)
    (define-key map "\ep" 'mud-previous-command)
    (define-key map "\en" 'mud-next-command)
    map)
  "Keymap for MUD interactive mode.")

(defun mud-interactive-mode (mud-type)
  "Major Mode for talking to inferior MUD processes.

Commands: 
\\{mud-interactive-mode-map}
Global Variables: [default in brackets]

 mud-show-page					[nil]
    If non-nil, pop up MUD buffer whenever a page arrives.
 mud-accept-reconnects				[nil]
    If nil, reject reconnect signals. If non-nil, accept reconnect
    signals by breaking existing connection and establishing new
    connection.  If an integer, spawn that many connections before
    breaking any.
 mud-entry-file					[\"~/.mud\"]
    Pathname to location of MUD address/character/password file.
 mud-use-entire-window				[nil]
    Try to keep the type-in point for the MUD buffer at the bottom
    of the window, so as not to have a half-window of blank space.

defmud parameters:

 prompt
    Character to identify MUD command input.
 connect-filters
    Initial filter hooks (before login)
 filters
    List of hooks to call before displaying output from MUD
    process to MUD buffer.  [Default hooks support line-filling,
    page checking, and reconnect detection.]
 startup-hook
    Hook to run at startup.  Users wishing to use macros may want to
    bind it to the following in their .emacs file:

     (setq tinymud-mode-hook
           '(lambda ()
       	       (mud-load-macro-commands tinymud-macro-commands-file)))

"
  (interactive)
  (kill-all-local-variables)
  (setq mud mud-type)
  (setq mode-name (symbol-name mud-type))
  (setq major-mode 'mud-interactive-mode)
  (setq fill-column (1- (window-width)))
  (setq indent-tabs-mode nil)
  (if (null mud-mode-syntax-table)
      (progn
	(setq mud-mode-syntax-table (make-syntax-table))
	(set-syntax-table mud-mode-syntax-table)
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?\[ "(]")
	(modify-syntax-entry ?\] ")["))
    (set-syntax-table mud-mode-syntax-table))
  (use-local-map (copy-keymap mud-interactive-mode-map))
  (make-local-variable 'mode-line-process)
  (let* ((s (and (concat "@" mud-server)))
	 (ss (cond ((not mud-accept-reconnects) "")
		   (t (if (> (length s) 20) (substring s 0 20) s)))))
    (setq mode-line-process (list (concat ss ":%s"))))
  (run-hooks (mud-startup-hook)))

(defun mud-start ()
  (interactive)
  (let* (
    (server (mud-default-server))
    (choice (assoc server (mud-servers)))
    (mud-name (car choice))
    (mud-sys (car (cdr choice)))
    (mud-server (car (cdr (cdr choice))))
    (mud-port (car (cdr (cdr (cdr choice))))))
    (open-mud mud-sys t)
  )
)

(defun mud (&optional server autoconnect)
  "Connect to MUD, asking for site to connect to.

With optional argument, look in mud-entry-file 
for name to connect with and attempt connect."
  (interactive (list (let* ((completion-ignore-case t)
			    (default (mud-default-server))
			    (name (completing-read
				   (format "Server (default %s): "
					   default)
				   (mud-servers)
				   nil t)))
		       (if (equal name "")
			   default
			 name))
		     current-prefix-arg))
  (let* ((choice (assoc server (mud-servers)))
	 (mud-name (car choice))
	 (mud-sys (car (cdr choice)))
	 (mud-server (car (cdr (cdr choice))))
	 (mud-port (car (cdr (cdr (cdr choice))))))
    (open-mud mud-sys autoconnect)))

(defvar mud-use-telnet t
  "*t if telnetting, nil otherwise.")

(defun open-mud (mud-sys autoconnect)
  (let ((index 0)
	(buf-name-root (concat "*" mud-name "*"))
	(buf-name nil))
    (while (and (get-buffer (setq buf-name
				  (if (= index 0)
				      buf-name-root
				    (format "%s<%d>" buf-name-root index))))
		(get-buffer-process buf-name)
		(process-status (get-buffer-process buf-name)))
      (setq index (+ index 1)))
  (let* ((buf (get-buffer-create buf-name))
	 ;;; name buffer host service/port
	 (proc (if (string= mud-sys "Jupiter")
		 (let ((process-connection-type nil))
		   (start-process "MUD" buf mud-jupiter-client-prog
				  "-n" "-t" (symbol-name mud-sys)
				  mud-server (int-to-string mud-port)))
		 (if mud-use-telnet
		     (start-process "MUD" buf "telnet" mud-server (int-to-string mud-port))
		   (open-network-stream "MUD" buf mud-server mud-port)))))
    (condition-case nil
	;; Despite how it looks, the following line ensures that Emacs *not*
	;; kill our network connection on exit without asking us first.  Some
	;; earlier versions of Emacs do not allow the second argument, so this
	;; call is wrapped in a CONDITION-CASE; in such versions, we'll just do
	;; nothing.
	(process-kill-without-query proc t)
      (error nil))
    (if autoconnect
	(let ((entry (mud-login-for-server mud-name))
	      (filter (or (mud-connect-filter)
			  'mud-filter)))
	  (set-process-filter proc filter)
	  (mud-send-string
	   (mapconcat '(lambda (x) x) 
		      (cons
		       (let ((mud mud-sys)) (mud-connect-command))
		       entry)
		      " ")
	   proc)
          (if (and entry (featurep 'hilit19))
              (let*
                  ((name (car entry))
                   (list (cons name '(nil mud-player-name)))
                   (args (cons list nil))
                   (current 
                    (assoc 'mud-interactive-mode hilit-patterns-alist)))
                (if current
                    (let*
                        ((now (cdr (cdr current)))
                         (new (cons list now)))
                      (hilit-set-mode-patterns
                       'mud-interactive-mode
                       new
                       nil
                       t))
                (hilit-set-mode-patterns
                 'mud-interactive-mode
                 args)
                 )))
	  ))
    (set-process-filter proc 'mud-filter)	    
    (set-process-sentinel proc 'mud-sentinel)
    (switch-to-buffer buf)
    (newline)
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (mud-interactive-mode mud-sys)
    (insert (mud-prompt))
    (cond (mud-use-entire-window
	   (make-local-variable 'scroll-step)
	   (setq scroll-step 1))
	  (t
	   (recenter '(4))))
    (mud-initialize-input-history))))
			   
;;; Input History Maintenance

(defun mud-make-history (size)
  ;; (head tail . vector)
  ;; head is the index of the most recent item in the history.
  ;; tail is the index one past the oldest item
  ;; if head == tail, the history is empty
  ;; all index arithmetic is mod the size of the vector
  (cons 0 (cons 0 (make-vector (+ size 1) nil))))

(defun mud-decr-mod (n m)
  (if (= n 0)
      (1- m)
    (1- n)))

(defun mud-history-insert (history element)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (length vec))
	 (new-head (mud-decr-mod head size)))
    (aset vec new-head element)
    (setcar history new-head)
    (if (= new-head tail)  ; history is full, so forget oldest element
	(setcar (cdr history) (mud-decr-mod tail size)))))

(defun mud-history-empty-p (history)
  (= (car history) (car (cdr history))))

(defun mud-history-ref (history index)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (if (<= head tail)
		   (- tail head)
		 (+ tail (- (length vec) head)))))
    (if (= size 0)
	(error "Ref of an empty history")
      (let ((i (% index size)))
	(if (< i 0)
	    (setq i (+ i size)))
	(aref vec (% (+ head i) (length vec)))))))

(defvar mud-input-history-size 20
  "The number of past input commands remembered for possible reuse")

(defvar mud-input-history nil)

(defvar mud-input-index 0)

(defun mud-initialize-input-history ()
  (make-local-variable 'mud-input-history)
  (make-local-variable 'mud-input-index)
  (setq mud-input-history (mud-make-history mud-input-history-size))
  (setq mud-input-index 0))

(defun mud-remember-input (string)
  (mud-history-insert mud-input-history string))

(defun mud-previous-command ()
  (interactive)
  (mud-browse-input-history 1))

(defun mud-next-command ()
  (interactive)
  (mud-browse-input-history -1))

(defun mud-browse-input-history (delta)
  (cond ((mud-history-empty-p mud-input-history)
	 (error "You haven't typed any commands yet!"))
	((eq last-command 'mud-browse-input-history)
	 (setq mud-input-index (+ mud-input-index delta)))
	(t
	 (setq mud-input-index 0)))
  (setq this-command 'mud-browse-input-history)
  (let ((start (mud-find-input)))
    (delete-region start (point))
    (insert (mud-history-ref mud-input-history mud-input-index))))

(defun mud-find-input ()
  (beginning-of-line 1)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (start (max (process-mark proc) (point))))
    (if (equal (char-after start) (mud-prompt))
	(setq start (1+ start)))
    (goto-char start)
    (end-of-line 1)
    start))

;;; Macro Commands

(defvar mud-current-process nil "Current MUD process")
(defvar mud-current-macro-commands-alist nil "Current MUD macro command alist")

(defvar mud-macro-commands-alist (list (cons "nil" ""))
  "*Alist of macros (keyed by strings)")
(make-variable-buffer-local 'mud-macro-commands-alist)


(defvar mud-macro-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\^c\^c" 'mud-macro-send-and-destroy)
    (define-key map "\^c\^s" 'mud-macro-send)
    (define-key map "\^cs"   'mud-macro-send)
    (define-key map "\^c\^]" 'mud-macro-abort)
    (define-key map "\^c\^t" 'mud-macro-define)
    map)
  "Keymap for mud-macro-expansion-mode.")

(defun mud-macro-expansion-mode ()
  "Major Mode for mucking with MUD macro expansion.
Commands:
\\{mud-macro-expansion-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "MUD-Macro-Expansion")
  (setq major-mode 'mud-macro-expansion-mode)
  (setq indent-tabs-mode nil)
  (set-syntax-table mud-mode-syntax-table)
  (use-local-map mud-macro-expansion-mode-map)
  (make-local-variable 'mud-expansion-macro-name)
  (message "Use ^C^S to send, ^C^C to send and destroy, ^C^] to abort..."))

(defun mud-macro-define (name)
  "Define buffer as mud-macro."
  (interactive (list (completing-read "MUD Macro: "
				      mud-current-macro-commands-alist
				      nil nil
				      mud-expansion-macro-name)))
  (let ((oldval (assoc name mud-current-macro-commands-alist)))
    (if oldval
	(setcdr oldval (buffer-string))
      (setq 
       mud-current-macro-commands-alist
       (cons
	(cons name (buffer-string))
	mud-current-macro-commands-alist))))
  (if (y-or-n-p "Save to file? ")
      (mud-store-macro-commands
       (expand-file-name
	(read-file-name (concat "File to save to (default "
				mud-macro-commands-file
				"): ")
			"~/"
			mud-macro-commands-file)))))



;;; Reading from entry file
;;;
;;; FORMAT:
;;; server  <server-name>  <mud-type>  <host-name>  <port>
;;; default-server  <server-name>
;;; include  <file-name>
;;; <server-name>  <character-name>  <password>
;;; default  <character-name>  <password>
;;;

(defvar mud-entry-file "~/.mud"
  "*Pathname to location of MUD address/character/password file.")

(defvar mud-servers nil)
(defvar mud-default-server nil)
(defvar mud-logins nil)
(defvar mud-default-login nil)

(defvar mud-entry-file-dates nil)

(defun mud-match-field (i)
  (buffer-substring (match-beginning i) (match-end i)))

(defun mud-report-syntax-error ()
  (let ((start (point)))
    (end-of-line)
    (error (concat "Syntax error in MUD entry file " file ": "
		   (buffer-substring start (point))))))

(defun mud-file-directory (name)
  (let ((i (1- (length name))))
    (while (not (= (aref name i) ?/))
      (setq i (1- i)))
    (substring name 0 (1+ i))))

(defun mud-file-write-date (file)
  (nth 5 (file-attributes file)))

(defun mud-entry-pattern (keyword nargs)
  (let ((pattern "?$"))
    (while (> nargs 0)
      (setq pattern (concat "\\([^ \n]*\\) " pattern)
	    nargs (1- nargs)))
    (if (null keyword)
	pattern
      (concat keyword " " pattern))))

(defun mud-parse-entry-file (name)
  (let ((file (expand-file-name name))
	(old-buffer (current-buffer))
	(buffer (generate-new-buffer " *MUD temp*")))
    (if (not (file-exists-p file))
	(error (concat "Can't find MUD entry file " file)))
    (setq mud-entry-file-dates (cons (cons file (mud-file-write-date file))
				     mud-entry-file-dates))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (buffer-flush-undo buffer)
	  (insert-file-contents file)
	  ;; Don't lose if no final newline.
	  (goto-char (point-max))
	  (or (eq (preceding-char) ?\n)
	      (newline))
	  (goto-char (point-min))
	  ;; handle "\\\n" continuation lines
	  (while (not (eobp))
	    (end-of-line)
	    (cond ((= (preceding-char) ?\\)
		   (delete-char -1)
		   (delete-char 1)
		   (insert ?\ )))
	    (forward-char 1))
	  ;; simplify whitespace handling
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]+" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "[ \t]+" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (cond ((or (eolp) (looking-at "#")))
		  ((looking-at "server ")
		   (let (port
			 type)
		     (if (or (not (looking-at (mud-entry-pattern "server" 4)))
			     (= (setq port (string-to-int
					    (mud-match-field 4)))
				0)
			     (not (memq (setq type
					      (intern (mud-match-field 2)))
					muds)))
			 (mud-report-syntax-error))
		     (setq mud-servers
			   (cons (list (mud-match-field 1)
				       type
				       (mud-match-field 3)
				       port)
				 mud-servers))))
		  ((looking-at "default-server ")
		   (if (not (looking-at (mud-entry-pattern "default-server"
							   1)))
		       (mud-report-syntax-error))
		   (if (null mud-default-server)
		       (setq mud-default-server (mud-match-field 1))))
		  ((looking-at "include ")
		   (if (not (looking-at (mud-entry-pattern "include" 1)))
		       (mud-report-syntax-error))
		   (mud-parse-entry-file (concat (mud-file-directory file)
						 (mud-match-field 1))))
		  ((looking-at "default ")
		   (if (not (looking-at (mud-entry-pattern "default" 2)))
		       (mud-report-syntax-error))
		   (if (null mud-default-login)
		       (setq mud-default-login (list (mud-match-field 1)
						     (mud-match-field 2)))))
		  ((looking-at (mud-entry-pattern nil 3))
		   (setq mud-logins (cons (list (mud-match-field 1)
						(mud-match-field 2)
						(mud-match-field 3))
					  mud-logins)))
		  (t (mud-report-syntax-error)))
	    (beginning-of-line 2)))
      (kill-buffer buffer)
      (set-buffer old-buffer))))

(defun mud-check-entry-file ()
  (if (or (null mud-entry-file-dates)
	  (let ((dates mud-entry-file-dates))
	    (while (and dates
			(equal (cdr (car dates))
			       (mud-file-write-date (car (car dates)))))
	      (setq dates (cdr dates)))
	    (not (null dates))))
      (progn
	(setq mud-servers nil
	      mud-default-server nil
	      mud-logins nil
	      mud-default-login nil
	      mud-entry-file-dates nil)
	(if (file-exists-p mud-entry-file)
	    (mud-parse-entry-file mud-entry-file))
	(setq mud-servers (append (reverse mud-servers) mud-default-servers))
	(if (null mud-default-server)
	    (setq mud-default-server mud-default-default-server)))))

(defun mud-servers ()
  (mud-check-entry-file)
  mud-servers)

(defun mud-default-server ()
  (mud-check-entry-file)
  mud-default-server)

(defun mud-login-for-server (server)
  (mud-check-entry-file)
  (or (cdr (assoc server mud-logins))
      mud-default-login))


;;; TinyMUD

(defvar tinymud-filter-hook
  '(mud-check-reconnect mud-check-page mud-fill-lines)
  "*List of functions to call on each line of tinymud output.  The
function is called with no arguments and the buffer narrowed to just
the line.") 

(defvar tinymud-connection-error-string
  "Either that player does not exist, or has a different password.")

(defvar tinymud-macro-commands-file "~/.tinymud_macros"
  "*Pathname of tinymud macros.")

(setq tinymud-output-filter nil)

(defun tinymud-connect-filter (proc string)
  "Filter for connecting to a TinyMUD server.  Replaced with tinymud-filter
once successful."
  (if (not (string-equal string tinymud-connection-error-string))
      (set-process-filter proc 'tinymud-filter)))



;;; SMUG (TinyMUD 2)

(defvar smug-filter-hook
  '(mud-convert-tabs-to-newlines mud-fill-lines)
  "*List of functions to call on each line of Smug output.  The
function is called with no arguments and the buffer narrowed to just
the line.")

(setq smug-macro-command-filter-hook
      '(mud-convert-newlines-to-tabs-in-strings))

(defun mud-convert-tabs-to-newlines ()
  "Replace all TABs to NEWLINEs in displaying of Smug output, since 
they represent new statements in the embedded programming language."
  (subst-char-in-region (point-min) (point-max) ?\t ?\n t))

(defun mud-convert-newlines-to-tabs-in-strings ()
  "Replace all NEWLINEs present inside top-level strings with TABs, 
as they are likely code objects."
  (goto-char (point-min))
  (if (re-search-forward "[\\[\"]" (point-max) t)
      (progn 
	(forward-char -1)
	(subst-char-in-region (point)
			      (save-excursion (forward-sexp 1) (point))
			      ?\n ?\t t))))

(defvar smug-macro-commands-file "~/.smug_macros"
  "*Pathname of SMUG macros.")


;;; MOO

(defvar moo-mode-hook '(define-moo-mode-commands))

(defun define-moo-mode-commands ()
  (define-key (current-local-map) "\^c\^d" 'moo-get-description)
  (define-key (current-local-map) "\^cd" 'moo-get-detail)
  (define-key (current-local-map) "\^ch" 'moo-get-help)
  (define-key (current-local-map) "\^c\^f" 'moo-get-field)
  (define-key (current-local-map) "\^c\^v" 'moo-get-verb-listing))

;; AstroVR fetch requests:
;;     #$# fetch host: <name> directory: <dir> filename: <name> type: <type>
(defun moo-check-fetch ()
  "Look for page message, and pop-up buffer if specified."
  (goto-char (point-min))
  (while (not (eobp))
    (if (looking-at (concat "#\\$# fetch "
			    "host: \\(.*\\) "
			    "directory: \\(.*\\) "
			    "file: \\(.*\\) "
			    "type: \\(.*\\) "
			    "destination: \\(.*\\)$"))
	(let ((host (mud-match-field 1))
	      (dir (mud-match-field 2))
	      (file (mud-match-field 3))
	      (type (mud-match-field 4))
	      (dest (mud-match-field 5)))
	  (delete-region (point) (save-excursion (beginning-of-line 2)
						 (point)))
	  (call-process "fetch-file" nil 0 nil
			host dir file type dest)))
    (beginning-of-line 2)))

(defvar *moo-gopher-buffer* nil)

(defun moo-check-gopher ()
  "look for a gopher request";
  (goto-char (point-min))
  (while (not (eobp))
    (if (looking-at (concat "#\\$# gopher"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)$"))
	(let ((here (current-buffer))
	      (changed nil)
	      (host (mud-match-field 1))
	      (port (mud-match-field 2))
	      (descr (mud-match-field 3))
	      (path (mud-match-field 4)))
	  (delete-region (point) (save-excursion (beginning-of-line 2)
						 (point)))
	  (if (not (fboundp 'gopher-set-object-host))
	      (load-library "gopher"))
	  (setq port (car (read-from-string port)))
	  (save-excursion
	    (gopher-dispatch-object
	     (vector (aref descr 0)
		     (substring descr 1)
		     path
		     host
		     port)
	     *moo-gopher-buffer*)
	    (cond( (not (equal here (current-buffer)))
		   (setq *moo-gopher-buffer* (current-buffer))
		   (setq changed t)
		   ))
	    )
	  (cond (changed
		 (display-buffer *moo-gopher-buffer* t)))
	  )
      (beginning-of-line 2)
      )))

(defun moo-explode-message ()
  "Convert a list of strings into more readable/editable text."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "{\"" nil t)
    (replace-match "{\n"))
  (goto-char (point-min))
  (while (search-forward "\", \"" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (search-forward "\"}" nil t)
    (replace-match "\n}"))
  (goto-char (1- (point-max)))
  (if (looking-at "\n")
      (delete-char 1)))

(defun moo-implode-message ()
  "Convert readable/editable text into a list of strings."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "{\n" nil t)
    (replace-match "{\""))
  (goto-char (point-min))
  (while (search-forward "\n}" nil t)
    (replace-match "\"}"))
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\", \"")))

(defun moo-get-help ()
  "Fetch a given help text and explode it."
  (interactive)
  (moo-do-fetch "Edit which help text: "
		"%s"
		"@gethelp %s"
		'moo-null-fixer))

(defun moo-null-fixer ()
  ;; Nothing required.
  )

(defun moo-get-field ()
  "Fetch the value of some field."
  (interactive)
  (moo-do-fetch "Edit what field: "
		"%s"
		"@show %s"
		'moo-fix-field))

(defun moo-get-detail ()
  "Fetch the value of some detail of the current room."
  (interactive)
  (moo-do-fetch "Edit what detail: "
		"%s"
		"dump_detail %s"
		'moo-null-fixer))

(defun moo-get-description ()
  "Fetch the description of some object."
  (interactive)
  (moo-do-fetch "Edit description of what object: "
		"%s"
		"@show %s.description"
		'moo-fix-field))

(defun moo-fix-field ()
  (define-key (current-local-map) "\^c\^e" 'moo-explode-message)
  (define-key (current-local-map) "\^c\^i" 'moo-implode-message)
  (insert "; !(")
  (search-forward ".")
  (insert "(\"")
  (end-of-line)
  (insert "\") = ")
  (let ((start (point)))
    (re-search-forward "Value: *")
    (delete-region start (point)))
  (save-excursion
    (end-of-line)
    (insert ")")))

(defun moo-get-verb-listing ()
  "Fetch the MOO code for a particular verb."
  (interactive)
  (moo-do-fetch "Program what verb: "
		"%s"
		"@list %s without numbers"
		'moo-fix-listing))

(defun moo-fix-listing ()
  (moo-code-mode)
  (cond ((looking-at "That object")
	 (let ((message (substring (buffer-string) 0 -1)))
	   (erase-buffer)
	   (error message)))
	((looking-at "That verb")
	 (let ((start (point)))
	   (end-of-line)
	   (delete-region start (point)))))
  (insert (concat "@program " moo-object "\n"))
  (if (looking-at "#")				; Kill the header line.
      (let ((start (point)))
	(beginning-of-line 2)
	(delete-region start (point))))
  (goto-char (point-max))
  (insert ".\n")
  (goto-char (point-min))
  (beginning-of-line 2))

(defun moo-do-fetch (prompt object-fmt command-fmt fixer)
  (setq moo-object (format object-fmt (read-string prompt))
	moo-state 'waiting
	moo-fixer fixer
	mud-current-process (get-buffer-process (current-buffer))
	moo-buffer (get-buffer-create moo-object))
  (moo-set-delimiter moo-suffix)
  (pop-to-buffer moo-buffer)
  (erase-buffer)
  (mud-send-string (concat "PREFIX " moo-prefix
			   "\nSUFFIX " moo-suffix
			   "\n"
			   (format command-fmt moo-object)
			   "\nPREFIX\nSUFFIX\n")
		   mud-current-process))

(defun moo-set-delimiter (str)
  (setq moo-delim-string str)
  (setq moo-delim-regexp (concat (regexp-quote str) "$")))

(defvar moo-prefix "===MOO-Prefix===")
(defvar moo-suffix "===MOO-Suffix===")
(defvar moo-upload-command nil)

;(defvar moo-edit-regexp (concat "#\\$# edit "
;				"name: \\(.*\\) "
;				"upload: \\(.*\\)$"))
; Hmm.  From the dec-94 file.
(defvar moo-edit-regexp (concat "#\\$# edit "
				"name: \\(.*\\) "
				"upload: \\(.*\\)\n"))
(defvar moo-buffer nil)
(defvar moo-delim-string nil)
(defvar moo-delim-regexp nil)
(defvar moo-state 'idle)
(defvar moo-object nil)
(defvar moo-fixer nil)
(defvar moo-filter-hook
  '(moo-filter moo-check-fetch moo-check-gopher mud-check-page mud-check-reconnect
	       astrovr-mosaic-hook mud-fill-lines))
(defvar zenmoo-filter-hook
  '(moo-filter zen-alert mud-check-page mud-check-reconnect mud-fill-lines))

(defun moo-quote-dots ()
  "Double any initial dot on every line of the current (narrowed) buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\." nil t)
      (replace-match ".."))))

(defun moo-unquote-dots ()
  "Un-double any initial dots on every line of the current (narrowed) buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\.\\." nil t)
      (replace-match "."))))

(make-variable-buffer-local 'moo-upload-command)

(defun moo-filter ()
  (goto-char (point-min))
  (while (not (eobp))
    (let ((start (point)))
      (cond ((and (eq moo-state 'waiting)
		  (looking-at (concat moo-prefix "\n")))
	     (beginning-of-line 2)
	     (delete-region start (point))
	     (setq moo-state 'copying
		   moo-upload-command nil))
	    ((and (eq moo-state 'idle)
		  '(let ((string (buffer-substring (point) (point-max))))
		     (save-excursion
		       (set-buffer (get-buffer-create "*MUD Packets*"))
		       (goto-char (point-max))
		       (insert "\n\n[[")
		       (insert string)
		       (insert "]]")
		       t))
		  (looking-at moo-edit-regexp))
	     (let ((name (mud-match-field 1))
		   (upload (mud-match-field 2)))
	       (beginning-of-line 2)
	       (delete-region start (point))
	       (setq moo-state 'copying
		     moo-buffer (get-buffer-create name)
		     mud-current-process (get-buffer-process (current-buffer))
		     moo-fixer 'moo-unquote-dots)
	       (moo-set-delimiter ".")
	       (let ((buff (current-buffer)))
		 (set-buffer moo-buffer)
		 (erase-buffer)
		 (setq moo-upload-command upload)
		 (set-buffer buff))))
	    ((eq moo-state 'copying)
	     (cond ((looking-at moo-delim-regexp)
		    (setq moo-state 'idle)
		    (beginning-of-line 2)
		    (delete-region start (point))
		    (let ((buff (current-buffer)))
		      (set-buffer moo-buffer)
		      (goto-char (point-min))
		      (let ((upload moo-upload-command))
			(if (string-match ":" (buffer-name))
			    ;; Assume verb edit, since there is a ":"
			    ;; (Can't be bad, if .prop has a ":"
			    ;; who cares if we have some extra verbing keys?)
			    (moo-code-mode)
			  (mud-macro-expansion-mode))
			(if upload
			    (setq moo-upload-command upload)))
		      (and moo-fixer (funcall moo-fixer))
		      (setq mud-select-buffer moo-buffer)
		      (set-buffer buff)))
		   (t
		    (beginning-of-line 2)
		    (let* ((buff (current-buffer))
			   (str (buffer-substring start (point)))
			   (len (length str)))
		      (if (or (> len (length moo-delim-string))
			      (not (equal (substring moo-delim-string 0 len)
					  str)))
			  (progn
			    (delete-region start (point))
			    (set-buffer moo-buffer)
			    (goto-char (point-max))
			    (insert str)
			    (set-buffer buff)))))))
	    (t
	     (beginning-of-line 2))))))

(defun zen-alert ()
  (ding t)
  (message "ZenMOO beckons.")
)
			  


;;; Jupiter

(defvar jupiter-filter-hook
  '(jupiter-filter moo-filter mud-check-page mud-check-reconnect
		   mud-fill-lines))
(defvar jupiter-sentinel-hook
  '(jupiter-sentinel))
(defvar jupiter-process nil
  "Process variable for mooaudio program.")
(make-variable-buffer-local 'jupiter-process)
(defconst jupiter-mooaudio "/project/jupiter/etc/mooaudio")

(defun jupiter-filter ()
  "Filter room change strings."
  (goto-char (point-min))
  (if (re-search-forward "^@@#\\([0-9]*\\)\n" (point-max) t)
      (let ((room (buffer-substring (match-beginning 1) (match-end 1))))
	(jupiter-set-room room)
	(delete-region (match-beginning 0) (match-end 0))))
  (goto-char (point-min))
  (if (re-search-forward "^#\\$# This server supports fancy clients.\n"
			 (point-max) t)
      (progn
	(send-string (get-buffer-process (current-buffer)) "@client emacs\n")
	(delete-region (match-beginning 0) (match-end 0))))
  (goto-char (point-min))
  (if (re-search-forward "^#\\$#channel \\([\.0-9]*\\)\n" (point-max) t)
      (let ((channel (buffer-substring (match-beginning 1) (match-end 1))))
	(jupiter-set-channel channel)
	(delete-region (match-beginning 0) (match-end 0)))))

(defun jupiter-set-room (room)
  (jupiter-set-channel (concat "224.4." room)))

(defun jupiter-set-channel (channel)
  (if (or (null jupiter-process)
	  (not (eq (process-status jupiter-process) 'run)))
      (setq jupiter-process
	    (start-process "jupiter-audio" nil
			   jupiter-mooaudio channel))
      (send-string jupiter-process (concat "g " channel "\n"))))


(defun jupiter-sentinel ()
  (if (and (not (eq (process-status proc) 'run))
	   (not (null jupiter-process)))
      (process-send-eof jupiter-process)))


;;; Generic stuff.

(defun mud-macro-abort ()
  "Abort macro expansion buffer."
  (interactive)
  (kill-buffer (current-buffer))
  ;(delete-window) ; this annoyingly destroys previously existing windows. -wsk
  )

(defun mud-macro-send ()
  "Send contents of macro expansion buffer."
  (interactive)
  (let ((str (buffer-string)))
    (mud-macro-send-2 str)))

(defun mud-macro-send-and-destroy ()
  "Send contents of macro expansion buffer and then kill the buffer."
  (interactive)
  (let ((str (buffer-string)))
    (mud-macro-send-2 str)
    (mud-macro-abort)))

(defun mud-macro-send-2 (str)
  (save-excursion
    (let ((proc mud-current-process)
	  (upload moo-upload-command))
      (set-buffer (process-buffer proc))
      (setq mud-macro-commands-alist mud-current-macro-commands-alist)
      (mud-send-string (let ((start (point)))
			 (insert str)
			 (save-restriction
			   (narrow-to-region start (point))
			   (if upload
			       (progn (moo-quote-dots)
				      (goto-char (point-min))
				      (insert (concat upload "\n"))
				      (goto-char (point-max))
				      (if (not (bolp))
					  (insert "\n"))
				      (insert ".\n"))
			     (run-hooks (mud-command-filters)))
			   (prog1
			       (buffer-string)
			     (delete-region (point-min) (point-max)))))
		       proc))))


(defun mud-send-string (string proc)
  "Send STRING as input to PROC"
  (if (not (= (aref string (- (length string) 1)) ?\n))
      (setq string (concat string "\n")))
  (send-string proc string))


(defun mud-load-macro-commands (filename)
  "Load file of mud-macros"
  (setq mud-macro-commands-alist
	(if (file-exists-p filename)
	    (progn
	      (setq mud-macro-commands-file filename)
	      (let ((tempbuf (get-buffer-create " *MUD Macros*"))
		    (buf (current-buffer)))
		(set-buffer tempbuf)
		(erase-buffer)
		(insert-file filename)
		(prog1 (car (read-from-string (buffer-string)))
		  (set-buffer buf))))
	  '("nil" . ""))))

(defun mud-store-macro-commands (filename)
  "Store MUD macros in filename"
  (interactive "FFile to save to: ")
  (setq mud-macro-commands-file filename)
  (save-excursion
    (let ((tmp (get-buffer-create " *Macros to write*")))
      (set-buffer tmp)
      (erase-buffer)
      (insert (prin1-to-string mud-current-macro-commands-alist))
      (write-file filename))))





(defun mud-macro-command (arg)
  "Insert into stream one of the commands in mud-macro-commands-alist.
Without command argument, opens buffer for editting.  With argument
sends alist entry directly to process."
  (interactive "P")
  (let ((macro
	 (assoc
	  (or (if (stringp arg) arg)
	      (completing-read "MUD Macro: "
			       mud-macro-commands-alist nil t nil))
	  mud-macro-commands-alist)))
    (let ((match (car macro))
	  (stuff (cdr macro)))
      (if (stringp stuff)
	  (let ((buff (get-buffer-create "*Expansion*"))
		(proc (get-buffer-process (current-buffer)))
		(alist mud-macro-commands-alist))
	    (if (not arg)
		(progn
		  (pop-to-buffer buff)
		  (erase-buffer)
		  (insert stuff)
		  (goto-char (point-min))
		  (mud-macro-expansion-mode)
		  (setq mud-expansion-macro-name match)
		  (setq mud-current-process proc)
		  (setq mud-current-macro-commands-alist alist)
		  )
	      (mud-send-string stuff proc)))))))

(defun mud-make-empty-buffer ()
  "Create a new empty buffer in MUD Macro Expansion mode, for local composition of a set of input lines to be sent to the MUD process."
  (interactive)
  (let ((buff (generate-new-buffer "*MUD Input*"))
	(proc (get-buffer-process (current-buffer))))
    (pop-to-buffer buff)
    (mud-macro-expansion-mode)
    (setq mud-current-process proc)))



;;; Utilities

(defun mud-cleanup-extra-processes ()
  (interactive)
  (mapcar '(lambda (p) (if (not (buffer-name (process-buffer (get-process p))))
			   (delete-process p)))
	  (process-list)))



;;; astrovr connection for Mosaic.
;;;;>read #73619  (on Lambda)
;;;;There appears to be some writing on the note ...
;;;;
;;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;;;
;;; NCSA Mosaic hooks for mud.el from AstroVR. 
;;; ==========================================
;;;
;;;                                 - D. Van Buren, dave@ipac.caltech.edu
;;;
;;; These mods to mud.el allow the connected server to run NCSA Mosaic
;;; on your cpu and display.  You need to have mosaic in your path and have
;;; write permissions on /tmp.  This code was written to run on a Sparc 1
;;; running SunOS 4.1.2, and will probably run on many UNIX boxes with at
;;; most minor changes.
;;;
;;;
;;;  1.  Add this line to your *-filter-hook routine in mud.el, for
;;;      example in your moo-filter-hook or a filter package called 
;;;      by that routine:
;;;
;;;         (astrovr-mosaic-hook)
;;;
;;;      This should come *before* the symbol mud-fill-lines appears.
;;;
;;;
;;;  2.  Substitute the local name of your NCSA Mosaic program in the line
;;;
;;;         (start-process "mosaic" "*mosaic*" "xmosaic" "-home"
;;;                                             ~~~~~~~
;;;      where it currently has the string `xmosaic'.
;;;
;;;
;;;
;;;  3.  Run the `kill -l' program on your unix system and
;;;      note the place of the SIGUSR1 entry, for example on
;;;      my machine it is the 30th entry.  In the line
;;;
;;;         (start-process "kill" "*kill*" "kill" "-30" (format "%d"
;;;                                                 ~~
;;;      change the `30' to the place number you found.
;;;
;;;                          - OR -
;;;
;;;      Check that your system supports kill -USR1 and change the "-30" 
;;;      to "-USR1".  To do this check, see if typing `kill -USR1' gives 
;;;      an error from the Unix prompt.
;;;
;;;
;;;  4.  Include this code in your mud.el.
;;;
;;;
;;;
;;;  5.  When your client sees a line that looks like this from the server
;;;
;;;         #$#display-url [*] command: <goto|newwin> url: <url> upload: [*]
;;;
;;;      it will instruct mosaic to display the document.  Here [*] just 
;;;      means any string including the empty string.  The first one is a
;;;      placeholder for an authentication key which is used to control
;;;      access to your mosaic process (not implemented) and the other is
;;;      a placeholder for a string to be sent back to the server upon
;;;      completion of the local display task (also not implemented).
;;;      Note that the blank spaces are required, if you omit the [*]
;;;      entries, you still need the blanks, for example:
;;;         
;;;         #$#display-url  command: goto url: http://foo/ upload: 
;;;                       ^^                                      ^
;;;                    2 blanks                                1 blank
;;;
;;;
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun astrovr-del-request-line ()
 "Deletes a line from the current buffer, typically the mud process buffer (from mud.el)."
 (delete-region (point) (save-excursion (beginning-of-line 2) (point)))
)

(defun astrovr-mosaic-hook ()
  "service a mosaic request."
  (goto-char (point-min))
  (while (not (eobp))
    (if (looking-at (concat "#\\$#display-url \\(.*\\) "
                            "command: \\(.*\\) "
			    "url: \\(.*\\) "
			    "upload: \\(.*\\)$" ))
        (let ((astrovr-mosaic-url (mud-match-field 3))
              (astrovr-mosaic-command (mud-match-field 2))
              (astrovr-mosaic-upload (mud-match-field 4)))
        (astrovr-del-request-line)
        (if (not(process-status "mosaic"))
            (start-process "mosaic" "*mosaic*" "Mosaic" "-home"
             astrovr-mosaic-url)
        (save-excursion
          (set-buffer (generate-new-buffer "*mosaic-cmd*"))
          (insert astrovr-mosaic-command)
          (insert "\n")
          (insert astrovr-mosaic-url)
          (setq mosaic-cmd-file (format "/tmp/Mosaic.%d"
             (process-id (get-process "mosaic"))))
          (write-region (point-min) (point-max) mosaic-cmd-file nil 'foo)
          (start-process "kill" "*kill*" "kill" "-USR1" (format "%d"
             (process-id (get-process "mosaic"))))
          (kill-buffer (current-buffer))
          ))))
    (beginning-of-line 2)))


(require 'moo-code)
