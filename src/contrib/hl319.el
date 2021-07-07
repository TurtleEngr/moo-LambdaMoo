;; hl319.el (Exp 1.31) -- customizable highlighting for Emacs19.
;; Copyright (c) 1993 Free Software Foundation, Inc.
;;
;; Author:   Jonathan Stigelman <Stig@netcom.com>
;; Keywords: faces
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:

;;{{{ TODO LIST

;; TODO LIST:
;;
;; doc strings for all the new code
;;
;; add mechanism for two-part searches to pattern builder
;;
;; add mechanism for recursion with a differrent pattern to pattern builder
;;
;; phase out the "highlighting #N: pattern" message and phase in a %done msg
;;
;; allow differrent maxout values for differrent faces.  In C, types would
;; go first...then keywords...and so on.
;;
;; set property on overlays to prevent forward stickiness?
;; eliminate priority property (for speed)?
;;
;; implement :if: construct to permit conditional coloring
;; Ex:
;;      (:pair: (:if: fixme urgent-error error)
;;		(:start: "/\\*" (:label: fixme " FIXME") ??)
;;		(:end: "\\*/"))
;;
;; change semantics of auto-rehighlight.  should be number of chars instead
;; of Nlines and it should also be relative to window start and window
;; end...not (point)

;;}}}
;;{{{ overview, installation, bugs, history

;; Hl319.el is a customizable highlighting package for Emacs19.  It supports
;; not only source code highlighting, but also Info, RMAIL, VM, gnus...
;; Hl319 knows (or thinks it knows) how to highlight emacs buffers in
;; about 25 different modes.
;;
;; WHERE TO GET THE LATEST VERSIONS OF HL319.EL (beta and release),
;; PLUS LOTS OF OTHER *WAY COOL* STUFF VIA ANONYMOUS FTP:
;;
;;      netcom.com:/pub/stig/src/{Beta,Release}/hl319.el.gz
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TO SUBMIT BUG REPORTS (or feedback of any sort)...
;;
;;    M-x hilit-submit-feedback RET
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hl319.el,v 1.31 1993/10/13 07:45:59 stig Exp
;;
;; LCD Archive Entry:
;; hl319|Jonathan Stigelman|Stig@netcom.com|
;; Comprehensive (and comparatively fast) regex-based highlighting for Emacs 19|
;; 1993/10/13 07:45:59|Exp 1.31|~/packages/hl319.el.Z|
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL OVERVIEW
;;
;;      This package installs numerous hooks to colorfully highlight your
;;      source code buffers as well as mail and news buffers.  Most
;;      programming languages have predefined highlighting patterns.
;;	Just load hl319 and files will be automatically highlighted as
;;      they're loaded.
;;
;;      Rehighlight a buffer by typing C-S-l (control-shift-lowercase-L).
;;
;;      If, when you edit the buffer, the coloring gets messed up, just
;;      redraw and the coloring will be adjusted.  If automatic highlighting
;;      in the current buffer has been turned off, then typing C-u C-S-l will
;;	force a rehighlight of the entire buffer.
;;
;;      Hl319 can build faces by examining the names that you give to them
;;	For example, green/black-bold-italic-underline would be created as
;;	a face with a green foreground, and a black background, using a
;;	bold-italic font...with underlining for good measure.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- In your .emacs:
;;
;; ***** NEW FEATURE: hilit-user-face-table *****
;;
;; (cond (window-system
;;        (setq hilit-mode-enable-list  '(not text-mode)
;;              hilit-background-mode   'light
;;              hilit-inhibit-hooks     nil
;;              hilit-inhibit-rebinding nil)
;;        (setq hilit-user-face-table
;;              ;; face         light           dark            mono
;;              '((type         nil             nil             nil)
;;                (jargon-entry defun           green           bold)
;;                (comment      firebrick       DeepPink        italic)
;;                (string       grey            pink            underline)))
;;
;;        (require 'hl319)
;;        ))
;;
;; If you like font-lock-mode and want to use both packages, then you can
;; disable hilit for the modes in which you want to use font-lock by listing
;; said modes in hilit-mode-enable-list.  To get a complete list of the
;; modes that hilit19 supports, evaluate this expression in your *scratch*
;; buffer:  (mapcar 'car hilit-patterns-alist)
;;
;; ***** THIS IS BEING PHASED OUT ******
;;
;;      (hilit-translate type     'RoyalBlue   ; enable highlighting in C/C++
;;			 string	  nil)         ; disable string highlighting
;;
;; To get 100% of the utility of hl319, you may also have to apply the
;; patches below for vm5.33L_19/vm-summary.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- Are you using the right font for Emacs?
;;
;; Emacs cannot properly find bold and italic fonts unless you specify a
;; verbose X11 font name.  Here's a good font menu:
;;
;; (setq
;;  x-fixed-font-alist
;;  '("Font Menu"
;;    ("Misc"
;;     ("6x12" "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-*-1")
;;     ("6x13" "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*-1")
;;     ("lucida 13"
;;      "-b&h-lucidatypewriter-medium-r-normal-sans-0-0-0-0-m-0-*-1")
;;     ("7x13" "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-1")
;;     ("7x14" "-misc-fixed-medium-r-normal--14-130-75-75-c-70-*-1")
;;     ("9x15" "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
;;     ("")
;;     ("clean 8x8" "-schumacher-clean-medium-r-normal--*-80-*-*-c-*-*-1")
;;     ("clean 8x14" "-schumacher-clean-medium-r-normal--*-140-*-*-c-*-*-1")
;;     ("clean 8x10" "-schumacher-clean-medium-r-normal--*-100-*-*-c-*-*-1")
;;     ("clean 8x16" "-schumacher-clean-medium-r-normal--*-160-*-*-c-*-*-1")
;;     ("")
;;     ("sony 8x16" "-sony-fixed-medium-r-normal--16-120-100-100-c-80-*-1")
;;     ("")
;;     ("-- Courier --")
;;     ("Courier 10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-*-1")
;;     ("Courier 12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-*-1")
;;     ("Courier 14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-*-1")
;;     ("Courier 18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-*-1")
;;     ("Courier 18-b" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-*-1")
;;     )))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KNOWN BUGS/TO DO LIST/HELP WANTED/APPLY WITHIN
;;
;; * When more than one size of font is used in different frames, only one
;;   font size can have bold & italic properties.
;;
;; * When identifiers such as remove_switch_entry, ar highlighted in C/C++,
;;   imbedded keywords--"switch" in this case--are highlighted.  I don't
;;   personally see this problem because I modify the syntax for C/C++ so that
;;   ?_ is a word character "w".  This also means that forward-word skips over
;;   entire variables.  This will be fixed when I generalize the highlighting
;;   patterns.
;;
;; * unbalanced, unescaped double quote characters can confuse hl319.
;;   This will be fixed, so don't bug me about it.
;;
;; * ALTHOUGH HL319 IS FASTER THAN FONT-LOCK-MODE...
;;   For various reasons, the speed of the package could still stand to be
;;   improved.  If you care to do a little profiling and make things tighter...
;;
;; * hilit-toggle-highlight is flaky when auto-rehighlight is neither t nor nil.
;;   Does anyone actually USE this?  I think I might just remove it.
;;
;; PROJECTS THAT YOU CAN TAKE OVER BECAUSE I DON'T MUCH CARE ABOUT THEM...
;;
;; * Moved hilit-wysiwyg-replace here from my version of man.el, this is not
;;   a bug.  The bug is that I don't have a reverse operation yet...just a
;;   stub Wysiwyg-anything really belongs in a package of it's own.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Thanks to the following people for their input:
;;    ebert@enpc.enpc.fr (Rolf EBERT), ada, LaTeX & bibtex highlights
;;    Olivier Lecarme <ol@aiguemarine.unice.fr>, Pascal & Icon patterns
;;    Vivek Khera <khera@cs.duke.edu>, gnus hooks, advice, patches, beta tester
;;    matt@physics.Berkeley.EDU (Matt Austern), fortran patterns, beta tester
;;    Chris.Moore@src.bae.co.uk, advice, minor patches, beta tester
;;    derway@ndc.com (Don Erway), advice, minor patches, beta tester
;;    brian@athe.WUstl.EDU (Brian Dunford-Shore), prolog highlights
;;    John Ladwig <jladwig@soils.umn.edu>, 1st pass nroff highlights
;;    campo@sunthpi3.difi.unipi.it (Massimo Campostrini), rmail summary ptrns
;;    Yoshio Turner <yoshio@CS.UCLA.EDU>, modula 2 highlights
;;    Fritz Knabe <knabe@ecrc.de>, advice & patches
;;    dana@thumper.bellcore.com (Dana A. Chee), working on the multi-frame bug
;;    Alon Albert <alon@milcse.rtsg.mot.com>, advice & patches
;;    ts@mystix.enpc.fr (Thierry SALSET), ml patterns
;;
;; With suggestions and minor regex patches from numerous others...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HISTORY: hl319.el,v
;; Revision 1.31  1993/10/13  07:45:59  stig
;; fixed minor bug in mail highlighting.
;; some pattern twiddling.
;; added (put delete-selection...) for hilit-yank
;;
;; Revision 1.30  1993/09/15  09:11:40  stig
;; removed outdated patch for VM from code
;;
;; removed some outdated variables that had already been commented out
;;
;; backed out some extra code in hilit-unhighlight-region that had been put
;; there before a more elegant fix for the hanging unhighlight problem was
;; found
;;
;; hilit-rehighlight-region now stretches the region so that it extends
;; backward to the beginning of a line and forward to the end of a line
;;
;; changes to the hilit-rehighlight-changed-lines hook so that it works better
;; for on-the-fly highlighting...
;;
;; fixed problematic msg-separator pattern so that highlighting doesn't hang in
;; gnus.
;;
;; Revision 1.29  1993/09/12  15:58:09  stig
;; 'finished' :funcall: pattern...still subject to revision, but it now works.
;; emacs-lisp can now be a mongo regex like perl and fortran....
;;
;; added 'global' patterns for folding-mode.  folding-mode must be set up
;; before the patterns are compiled or the global patterns don't kick in.  The
;; folding-mode parse function is an example of where it's useful to have more
;; than one face passed to a parse function so that the parse function can
;; choose the face.
;;
;; added documentation for several of the pattern building functions.
;;
;; partially implemented local patterns.  *** Please give me feedback on how
;; you would like to see these implemented. ***
;;
;; added label highlighting in C/C++
;;
;; Revision 1.28  1993/09/11  19:17:02  stig
;; some color fiddling
;;
;; new function hilit-respin-translations so that generation of
;;   hilit-face-translation-table from the user and default tables would be
;;   simpler.
;;
;; returned the condition-case to the pattern matcher to avoid fatal barfs.
;;
;; modified c-identifier syntax to match destructor functions.
;;
;; use mail-header-separator in mail patterns.
;;
;; twiddled vm-summary patterns to work with the standard summary format.
;;
;; simplified comment pattern in makefiles.
;;
;; Revision 1.27  1993/09/08  18:38:39  stig
;; installed patch for elusive bug in hilit-rehighlight-region that caused
;; hilit-unhighlight-region to hang in an infinite loop.
;;
;; Revision 1.26  1993/09/07  20:18:44  stig
;; new fortran patterns again.
;;
;; reshuffled credits.
;;
;; Revision 1.25  1993/09/07  19:06:31  stig
;; added missing hilit-rehighlight-message-quietly (this should fix rmail, gnus)
;;
;; hilit-submit-feedback now no longer references defunct variables....it also
;; no longer asks about inserting frame configurations.
;;
;; fixed bug in hilit-opt-regexp which caused lisp patterns to be built
;; incorrectly.
;;
;; new fortran patterns.
;;
;; slight tweak to Info patterns to support bfd.info's heading style.
;;
;; Revision 1.24  1993/09/05  10:25:38  stig
;; unmunged type patterns in C/C++
;;
;; hacked perl patterns to include #define & #include
;;
;; Revision 1.23  1993/09/03  02:49:55  stig
;; new fortran patterns
;;
;; minor patch to make site-init preloading work properly.
;;
;; perl pattern now catches highlights single-quote delimited strings.
;;
;; Revision 1.22  1993/08/31  05:48:33  stig
;; control-= now does redisplay
;;
;; replaced hilit-replace-functions and hilit-inhibit-hooks with
;; hilit-hook-list
;;
;; hilit-new-style-patterns now defaults to nil, which is faster...but somewhat
;; of a step "backwards"
;;
;; new virtual faces for future comint support: prompt, input, output
;;
;; fixed bug in hilit-string-find-fn that caused character constants to hang.
;;
;; fixed bug in initialize that caused initialization to barf when colors for
;; predefined faces could not be allocated.
;;
;; Revision 1.21  1993/08/28  21:08:00  stig
;; new function: hilit-initialize.  hl319 should now be dumpable.  You should
;; be able to load it in site-init, then dump a new emacs.  SOMEBODY TELL ME IF
;; THIS WORKS.
;;
;; the hilit-rehighlight-changed-lines hook can now almost meet the needs of
;; people who want highlighting to happen on the fly.  I don't know that I want
;; to mess with this, so this ball is in the court of the people who want
;; dynamic highlighting.
;;
;; dinked around with default colors a bit more.
;;
;; Revision 1.20  1993/08/24  05:17:10  stig
;; now use after change hook in dired/vm/gnus summary buffers.
;;
;; Revision 1.19  1993/08/23  22:29:41  stig
;; fixed perl string pattern
;;
;; use forward-sexp in string matcher.
;;
;; Revision 1.18  1993/08/23  21:16:32  stig
;; further improved the face-table merging code
;;
;; began phasing out extra-wide naming strategy...  trying to get faces to have
;; the same significance across different windows.  dired-* names phased out.
;;
;; dired patterns now match all sorts of differrent files.
;;
;; Revision 1.17  1993/08/23  06:02:24  stig
;; the new automatic recoloring of predefined faces wasn't quite right.
;; fixed bug in that...
;;
;; also fixed a bug in the definition of hilit-face-translation-table.
;;
;; Revision 1.16  1993/08/23  05:33:30  stig
;; fixed minor bug in repaint-command, larger bug in hilit-get-compiled-patterns.
;;
;; fixed broken makefile patterns.
;;
;; On initialization, hilit19 can now be used to set up the predefined faces.
;; In hilit-user-face-table, just specify translations for them.  This is a
;; winner for shrinking your init file.
;;
;; Revision 1.15  1993/08/22  05:11:26  stig
;; added hilit-user-fact-table (parallels the default-face-table) and will be
;; moving away from hilit-translate as a means of changeing color defaults.
;;
;; some support for precompiled patterns (Note that precompiling will also grab
;; colors early...this is probably a feature for most people).
;;
;; added :funcall: to pattern builder.  There seems to be a speed problem here.
;;
;; changed the top-level grouping method for :buildme:s from concatenation to
;; an unenclosed OR:  (:buildme: "foo" "bar") --> "foo\\|bar" not "foobar"
;;
;; new variable hilit-new-style-patterns generates longer list of patterns to
;; apply instead of a shorter pattern list that has a single complex regular
;; expression.  This may change what gets highlighted or it may not.
;;
;; ^U^U^U C-S-l will toggle hilit-new-style-patterns for a single pattern build
;; so that you can contrast the performance of the two.
;;
;; hilit-auto-rehighlight can now be 'defun (similar to 'visible).
;;
;; replaced hilit-inhibit-rebinding with hilit-replace-functions which is
;; '(yank yank-pop) by default.  This gives finer granularity for controlling
;; what gets rebound.
;;
; Revision 1.14  1993/08/20  14:19:10  stig
; new interactive function: hilit-translate-face
;
; fixed another bug in the pattern builder ... this one was causing extra
; overlays to be created with the face 't' ... just more markers and more
; function calls.  Sludge factor is much reduced now.
;
; Also, hilit-local-ctypes defaults to nil now.  I figure that speed should be
; the default...  still seem to be speed problems with C patterns...and type
; patterns are slightly broken...
;
; Revision 1.13  1993/08/20  09:46:36  stig
; fixed bug in pattern builder that generated expression like \(foo\|\|bar\)
; when faces were translated to nil...which caused the expression matcher to
; do flips.
;
; Now ^U ^U C-S-l (or ^U^U^L) cause a pattern recompilation AND a rehighlight.
;
; added case-insensitive to fortran, pascal, modula2.
;
; Revision 1.12  1993/08/19  15:31:06  stig
; added calendar, icon, pascal from hilit19
;
; Revision 1.11  1993/08/19  02:01:50  stig
; Added pattern compilation.  The first time that a buffer is highlighted, the
; patterns for the buffer's mode are compiled and stored in a buffer-local
; variable.  When a pattern is compiled, hilit-lookup-face-create is called to
; determine if the face has been translated to nil.  If so, the entire
; subexpression is eliminated from the compiled pattern.
;
; This delayed pattern compilation means that patterns can be constructed to
; take advantage of buffer-local patterns specified in a file's local
; variables.
;
; Precompilation should be added.
;
; Unfortunately, this will adversely affect font recreation.  But that's fine
; with me because it was STUPID that hilit19 had to do flips to keep fonts
; bold and italic.  make-face-bold and make-face-italic need to be changed so
; that faces get maintained just like the standard faces bold, italic, and
; bold-italic.
;
; Revision 1.10  1993/08/17  14:15:04  stig
; added formula face defaults
;
; Revision 1.9  1993/08/17  12:09:31  stig
; *** empty log message ***
;
; Revision 1.8  1993/08/17  09:58:24  stig
; success??  emacs-lisp highlights with JUST ONE PATTERN.  No comments
; highlighted in strings, no overlapping overlays at all.  this means fewer
; markers and faster editing.  No speed improvement in highlighting right now
; because of slow regex matching for strings...  Also, the regex matcher
; overflows on long strings.  I've had one core dump:
;
;     	sendsig: bad signal stack pid=6360, sig=11
;     	sigsp = 0xf7ebc5f8, action = 0xcd260, upc = 0xb277c
;
; Revision 1.7  1993/08/16  23:10:55  stig
; changed most of the hl3s to hilits for "compatibility" w/ the mainline.
; package still identified as hl319 to differrentiate.  both hilit and hl3 are
; provided.
;
; incorporated case-fold patch, but didn't actually set the appropriate modes
; to be folded.
;
; Revision 1.6  1993/08/16  12:55:44  stig
; deleted some crufty code.
;
; added urg-comment virtual face & use it for FIXME comments in C
;
; converted makefile expressions...with little performance improvement because
; the variable reference expression is so slow.
;
; Revision 1.5  1993/08/16  08:24:30  stig
; fixed bug in pattern matcher stemming from careless loop optimization.
;
; comments & slight changes to pattern builder
;
; Revision 1.4  1993/08/16  00:22:46  stig
; imported some of the hilit changes...  still need to do case-insensitivity.
;
; Revision 1.3  1993/08/15  16:00:42  stig
; ported C/C++ patterns, lisp patterns.  others?
;
; seems to be *significantly* faster for C/C++ and somewhat faster for elisp.
; In elisp, the big time sinks a seem to be strings & keywords.
;
; Revision 1.2  1993/08/15  09:07:52  stig
; cleaned up regex builder so that it's okay to use subexpressions in your
; strings.  This makes it simpler to define complex patterns...like, say,
; types in C/C++.
;
; Added "optimizing" keyword pattern builder.  (or :opt: ...) or (face :opt: ...)
;
; did patterns for perl, C, news, & mail
;
; Revision 1.1  1993/07/29  13:16:55  stig
; Initial revision
;
;; Revision 2.5  1993/07/28  05:02:56  stig
;; improvements to makefile regular expressions
;; removed about 130 lines just by compacting the big defconst for
;;   hilit-face-translation-table into a mapcar and defining a separate table
;;   of default faces.
;;
;; Revision 2.4  1993/07/27  14:09:05  stig
;; documented another "known problem" to "head off gripe mail at the pass."
;;
;; Revision 2.3  1993/07/27  02:15:49  stig
;; (hilit-lookup-face-create) incorporated patch which improves it's behavior
;; with more than one frame...  Still can't have bold on the same face in two
;; differrent fonts sizes at the same time...
;;
;; Revision 2.2  1993/07/27  02:02:59  stig
;; vastly improved the makefile patterns
;; added hook for mh-show-mode
;;
;; Revision 2.1  1993/07/24  17:46:21  stig
;; Phasing out Info-select-hook...  Version 19.18 will use Info-selection-hook.
;;
;; Revision 2.0  1993/07/24  13:50:10  stig
;; better documentation and added the function hilit-submit-feedback.
;; C-S-l (control shift l) repaints the buffer.  Other bindings are optional.
;; multi-line highlights no longer cause problems when
;;   hilit-auto-rehighlight is 'visible
;; added hilit-predefined-face-list...
;; changed name of hilit-mode-alist to hilit-patterns-alist
;; added hilit-message-quietly to mail-setup-hook
;; added hilit-parser-alist which can be used to apply different patterns to
;;   different parts of a buffer.  This could be integrated in a far more
;;   elegant manner, but it presently serves the purpose of not applying
;;   message header patterns to message bodies in mail-mode and it's kin.
;; hilit-set-mode-patterns now takes a list of modes and an optional parse-fn
;;

;;}}}

;;{{{ stable stuff

;;; Code:

;;{{{ user options & variables

;; User Options:

(defvar hilit-quietly nil
  "* If non-nil, this inhibits progress indicators during highlighting")

(defvar hilit-auto-highlight t
  "* T if we should highlight all buffers as we find 'em, nil to disable
  automatic highlighting by the find-file hook.")

(defvar hilit-auto-highlight-maxout 60000
  "* auto-highlight is disabled in buffers larger than this")

(defvar hilit-auto-rehighlight t
  "* If this is non-nil, then hilit-redraw and hilit-recenter will also
  rehighlight part or all of the current buffer.  T will rehighlight the
  whole buffer, a NUMBER will rehighlight that many lines before and after
  the cursor, and the symbol 'visible' will rehighlight only the visible
  portion of the current buffer.  This variable is buffer-local.")

(make-variable-buffer-local 'hilit-auto-rehighlight)

(defvar hilit-auto-rehighlight-fallback '(20000 . defun)
  "* Cons of the form (THRESHOLD . FALLBACK), where FALLBACK is assigned to
  hilit-auto-rehighlight if the size of a newly opened buffer is larger than
  THRESHOLD.")

(defvar hilit-face-check t
  "* T slows down highlighting but permits the user to change fonts without
  losing bold and italic faces...  T causes hilit-lookup-face-create to dig
  through the frame parameters for the current window every time it's called.
  If you never change fonts in emacs, set this to NIL.")

;; Variables which must be set before loading hl319.

(defvar hilit-user-face-table nil
  "This is an association list of the user's face preferences for hilit19.
See the variable for hilit-background-mode.  Each entry takes the form:
    (virtual-face light-background-face dark-background-face mono-face)
THIS VARIABLE MUST BE SET BEFORE HILIT19 IS LOADED.")

(defvar hilit-hook-list '(yank yank-pop find-file info mail gnus dired)
  "list of hilit19 hooks to install.  Valid values are:
    yank yank-pop recenter find-file info mail gnus dired")

(defvar hilit-background-mode 'light
  "'mono inhibits color, 'dark or 'light indicate the background brightness.")

(defvar hilit-mode-enable-list nil
  "If a list of modes to exclusively enable or specifically disable.
The sense of the list is negated if it begins with the symbol 'not'.
Set this variable before you load hl319.

Ex:  (perl-mode jargon-mode c-mode)	; just perl, C, and jargon modes
     (not text-mode)			; all modes except text mode")

(defvar hilit-new-style-patterns nil
  "* make BIG regexps for \"perfect\" highlighting...except the regex matcher is slooow.")

;; Variables that are not generally modified directly

(defvar hilit-parser-alist nil
  "alist of major-mode values and parsers called by hilit-rehighlight-buffer.

Parsers for a given mode are IGNORED for partial rehighlights...maybe you'd
like to make this more universal?")

(defvar hilit-patterns-alist nil
  "alist of major-mode values and default highlighting patterns

A highlighting pattern is a list of the form (start end face), where
start is a regex, end is a regex (or nil if it's not needed) and face
is the name of an entry in hilit-face-translation-table, the name of a face,
or nil (which disables the pattern).

See the hilit-lookup-face-create documentation for valid face names.")

(defvar hilit-predefined-face-list nil
  "List of faces with which hilit-lookup-face-create will NOT tamper.
This variable is initialized by the function hilit-initialize.")

;;}}}
;;{{{ hilit-default-face-table, hilit-face-translation-table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These faces are either a valid face name, or nil
;; if you want to change them, you must do so AFTER hl319 is loaded

(defconst hilit-default-face-table
  '(
    ;; used for C/C++ and elisp and perl
    (comment	firebrick-italic    orange             italic)
    (include	purple		    Plum1	       bold-italic)
    ;(define	ForestGreen-bold    green	       bold)
    (defun	blue-bold	    cyan-bold	       bold-italic)
    (decl	RoyalBlue	    yellow	       bold)
    (type	DarkGoldenrod	    Goldenrod-underline nil)
    (keyword	RoyalBlue	    yellow	       bold-italic)
    (label	red-bold	    red-bold	       underline)
    (string	grey40		    moccasin	       underline)
    (operator	black-bold	    yellow	       underline)

    ;(fold-open	  blue/grey	     green/dimgrey     reverse-default)
    ;(fold-folded  blue/grey-bold-underline
    ;		                     green/dimgrey-bold-underline
    ;				                       reverse-default-bold)
    (active-error default/pink-bold  black/DeepPink-bold default-underline)

    (error	  red-bold           yellow	       bold)
    (urgent	  red-bold-italic    red-bold	       bold-italic)
    ;(warning	  blue-italic	     green	       italic)
    (marked	  purple	     Plum1	       bold)
    (ignored	  ForestGreen	     moccasin	       bold)
    (deleted	  red-bold-italic    orange-bold       bold-italic)

    (crossref	  DarkGoldenrod	     Goldenrod	       underline)
    (directory	  blue-bold	     cyan	       bold)
    ;(link	  firebrick-italic   green	       italic)
    ;(socket	  green		     yellow	       italic)
    (special	  Navy		     blue	       nil)

    (prompt	  firebrick	     red	       underline)
    ;(input	  blue-bold	     green-bold	       bold)
    (output	  Navy		     tan	       nil)

    (formula	  Goldenrod	    DarkGoldenrod      underline)
    (rule	  blue-bold-underline cyan-underline   default-bold-underline)

    ;; some further faces for Ada
    (struct	  black-bold        white-bold	       bold)
    (glob-struct  magenta	    Plum1	       default-bold-underline)
    (named-param  DarkGoldenrod	    Goldenrod	       underline)

    ;; VM, GNUS and Text mode
    (msg-subject    blue-bold       yellow             bold)
    ;(msg-from	    purple-bold	    green	       bold)
    (msg-header	    firebrick-bold  cyan	       italic)
    (msg-separator  black/tan-bold  black/lightblue    nil)
    (msg-quote	    ForestGreen	    pink	       italic)

    (summary-seen     grey40	    white	       nil)
    ;(summary-Xed      OliveDrab2    green	       nil) ; ?? what's this
    (summary-killed   grey50	    red		       nil)
    (summary-deleted  firebrick	    red		       italic)
    (summary-unread   RoyalBlue	    yellow	       bold)
    (summary-new      blue-bold	    yellow-bold	       bold-italic)
    ;(summary-current  default/skyblue-bold green/dimgrey-bold reverse-default)

    (gnus-group-unsubscribed grey50		white	    nil)
    (gnus-group-empty	     nil		nil	    nil)
    ;(gnus-group-full	     ForestGreen	green	    italic)
    (gnus-group-overflowing  firebrick		red	    bold-italic)

    ;; Info-mode, and jargon-mode.el and prep.ai.mit.edu:/pub/gnu/jargon*
    (jargon-entry    blue-bold	       cyan            bold)
    (jargon-xref     purple-bold       Plum1	       italic)
    (jargon-keyword  firebrick-underline yellow	       underline)
    )
  "alist of default faces (face . (light-default dark-default mono-default))
For user preferences, set the variable hilit-user-face-table before loading.")

;; (hilit-respin-translations)

(defvar hilit-face-translation-table nil
  "An association list that maps \"virtual\" faces to real face names.
This is derived from hilit-default-face-table and hilit-user-face-table.

DO NOT MODIFY THIS VARIABLE.  Use hilit-user-face-table, or hilit-translate")

;;}}}
;;{{{ hilit-associate, hilit-translate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To translate one face to another...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-associate (alist key val)
  "creates, or destructively replaces, the pair (key . val) in alist"
  (let ((oldentry (assq key (eval alist))))
    (if oldentry
	(setcdr oldentry val)
      (set alist (cons (cons key val) (eval alist))))))

(defun hilit-translate-face (from to)
  "Interactive version of hilit-translate."
  (interactive "SName of face to translate: \nSTranslate to: ")
  (hilit-associate 'hilit-face-translation-table from to))

(defmacro hilit-translate (&rest args)
  "(hilit-translate FROM TO FROM TO ...): translate each face FROM to the
value of its TO face.  This is like setq for faces.

The function hilit-lookup-face-create will repeatedly translate until no more
translations for the face exist in the translation table.

See the documentation for hilit-lookup-face-create for names of valid faces."
  (or (zerop (% (length args) 2))
      (error "wrong number of args"))
  (let (cmdl from to)
    (while args
      (setq from (car args) to (nth 1 args) args (nthcdr 2 args)
	    cmdl (cons (list 'hilit-associate ''hilit-face-translation-table
			     (list 'quote from) to)
		       cmdl)))
    (cons 'progn cmdl)))

;;}}}
;;{{{ hilit-lookup-face-create

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function actually translates and then creates the faces...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-lookup-face-create (face &optional force)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the following naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]
Example: (hilit-lookup-face-create 'comment-face) might create and return 'red

Each color is either the name of an X color (see .../X11/lib/X11/rgb.txt),
a hexadecimal specification of the form \"hex-[0-9A-Fa-f]+\", or \"default\".

An optional argument, FORCE, will cause the face to be recopied from the
default...which is probably of use only if you've changed fonts.

See the documentation for hilit-translate and hilit-face-translation-table."

;; translate the face ...
  (let ((trec t) visited)
    (while trec
      (cond ((memq face visited) (error "face translation loop: %S" visited))
	    (t (setq visited (cons face visited)
		     trec (assq face hilit-face-translation-table))
	       (and trec (setq face (cdr trec)))))))

  ;; make the face if we need to...
  (let* ((fn (symbol-name face))
	 (frame (selected-frame))
	 (basefont (cdr (assq 'font (frame-parameters frame))))
	 error fgcolor bgcolor)
    (cond
     ((or (null face)
	  (memq face hilit-predefined-face-list))
      ;; do nothing if the face is nil or if it's predefined.
      )
     ((or force
	  (not (memq face (face-list)))
	  (and hilit-face-check
	       (not (string= (get face 'basefont) basefont))))
      (copy-face 'default 'scratch-face)
      (if (string-match "^reverse-?" fn)
	  (progn (invert-face 'scratch-face)
		 (setq fn (substring fn (match-end 0)))))

      ;; parse foreground color
      (if (string-match "^\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq fgcolor (concat
			 (if (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0)))
	(error "bad face name %S" face))

      ;; parse background color
      (if (string-match "^/\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq bgcolor (concat
			 (and (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0))))

      (and (string= "default" fgcolor) (setq fgcolor nil))
      (and (string= "default" bgcolor) (setq bgcolor nil))

      ;; catch errors if we can't allocate the color(s)
      (condition-case nil
	  (progn (set-face-foreground 'scratch-face fgcolor)
		 (set-face-background 'scratch-face bgcolor)
		 (copy-face 'scratch-face face)
		 (put face 'basefont basefont))
	(error (message "couldn't allocate color for '%s'"
			(symbol-name face))
	       (setq face nil error t)))
      (or error
	  ;; don't bother w/ bold or italic if we didn't get the color
	  ;; we wanted, but ignore errors making the face bold or italic
	  ;; if the font isn't available, there's nothing to do about it...
	  (progn
;;	    (set-face-font face nil frame)
	    (set-face-font face "-outline-Courier New-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1" frame)
	    (set-face-underline-p face (string-match "underline" fn))
	    (if (string-match ".*bold" fn)
		(progn
		  ;; first, fix up this frame's face
		  (make-face-bold face frame  'noerr)
		  ;; now, fix up the face from the global list
		  (set-face-font face (face-font face frame) t)))
	    (if (string-match ".*italic" fn)
		(progn
		  ;; first, fix up this frame's face
		  (make-face-italic face frame 'noerr)
		  ;; now, fix up the face from the global list
		  (set-face-font face (face-font face frame) t)))
	    ))
      )))
  face)

;;}}}
;;{{{ Region Highlight/Unhighlight code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region Highlight/Unhighlight functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst hilit-region-set-face (start end face-name &optional prio prop)
  "Highlight region from START to END using FACE and, optionally, PRIO.
The optional 5th arg, PROP is a property to set instead of 'hilit."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face-name)
    (overlay-put overlay (or prop 'hilit) t)
    (and prio (overlay-put overlay 'priority prio))
    ))

(defun hilit-unhighlight-region (start end &optional quietly)
  "Unhighlights the region from START to END, optionally in a QUIET way"
  (interactive "r")
  (or quietly hilit-quietly (message "Unhighlighting"))
  (while (and (< start end))		; FIXME - start
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr 'hilit) (delete-overlay ovr))))
	    (overlays-at start))
    (setq start (next-overlay-change start)))
  (or quietly hilit-quietly (message "Done unhighlighting")))

;;}}}
;;{{{ more convenient functions for (un|re)highlighting

(defun hilit-rehighlight-region (start end &optional quietly)
  "Re-highlights the region, optionally in a QUIET way"
  (interactive "r")
  (save-restriction
    (widen)
    (setq start (save-excursion (goto-char start) (beginning-of-line) (point))
	  start (apply 'min start (mapcar 'overlay-start (overlays-at start)))
	  end (save-excursion (goto-char end) (end-of-line) (point))
	  end (apply 'max end (mapcar 'overlay-end (overlays-at end))))
    (hilit-unhighlight-region start end quietly)
    (hilit-highlight-region   start end nil quietly)))

(defun hilit-rehighlight-buffer (&optional quietly)
  "Re-highlights the buffer, optionally in a QUIET way"
  (interactive "")
  (if (/= (point-min) (point-max))
      (let ((parse-fn (cdr (assq major-mode hilit-parser-alist))))
	(if parse-fn
	    (funcall parse-fn quietly)
	  (hilit-rehighlight-region (point-min) (point-max) quietly))))
  nil)

(defun hilit-rehighlight-buffer-quietly ()
  (hilit-rehighlight-buffer t))

(defun hilit-rehighlight-message (quietly)
  "Highlight a buffer containing a news article or mail message."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^\\("
			       (regexp-quote mail-header-separator)
			       "\\)?$")
		       nil 'noerr)
    (hilit-unhighlight-region (point-min) (point-max) quietly)
    (hilit-highlight-region (point-min) (point) 'msg-header quietly)
    (hilit-highlight-region (point) (point-max) 'msg-body quietly)))

(defun hilit-rehighlight-message-quietly ()
  (hilit-rehighlight-message t))

(defalias 'hilit-highlight-buffer 'hilit-rehighlight-buffer)

(defun hilit-gnus-message (quietly)
  "Highlight a buffer containing a news article (using gnus-hide if provided)."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$" nil 'noerr)
    (hilit-unhighlight-region (point-min) (point-max) quietly)
    (hilit-highlight-region (point-min) (point) 'msg-header quietly)

    (if (featurep 'gnus-hide)
	(let ((pat (gnus-identify-quote-prefix nil)))
	  (if pat
	      (hilit-highlight-region
	       (point) (point-max)
	       (list (list (concat "^" (regexp-quote pat) ".*$")
			   nil 'msg-quote))
	       quietly)))
      (hilit-highlight-region (point) (point-max) 'msg-body quietly))))

;;}}}

;;; Interface Code:

;;{{{ hilit-submit-feedback

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use this to report bugs:

(defun hilit-submit-feedback ()
  "Submit feedback on hl319 to the author: Stig@netcom.com"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on hl319? ")
       (reporter-submit-bug-report
	"Jonathan Stigelman <Stig@netcom.com>"
	"hl319.el (Exp 1.31)"
	(and (y-or-n-p "Do you need to include a dump hilit19 variables? ")
	     (append
	      '(
		hilit-quietly    		hilit-hook-list
		hilit-background-mode		hilit-mode-enable-list
		hilit-auto-highlight		hilit-auto-highlight-maxout
		hilit-auto-rehighlight		hilit-auto-rehighlight-fallback
		hilit-face-check
		)
	      (and (y-or-n-p "Have you modified the standard patterns? ")
		   (yes-or-no-p "Are your patterns *REALLY* relevant? ")
		   '(hilit-parser-alist
		     hilit-patterns-alist
		     hilit-predefined-face-list
		     ))))
	;; (function
	;;  (lambda ()
	;;    (and (y-or-n-p "Is this a problem with font display? ")
	;;         (insert "\nFrame Configuration:\n====================\n"
	;;                 (prin1-to-string (frame-configuration-to-register ?F))
	;;                 "\n"
	;;                 ))))
	nil ;;; in place of the frame config question above....
	nil
	"
   Y     Y   OOOO   !!!   This is Mega-Beta Code...(You might call it ALPHA.)  
    Y  	Y   O	 O  !!!	                                                       
     Y Y    O	 O  !!!	  DO NOT report pattern-related 'bugs' unless you have 
      Y	    O	 O  !!!   been invited to test it out.  Truly good ideas and   
      Y	    O	 O  !!!	  well-considered advice, however, are *ENCOURAGED*.   
      Y	    O	 O        
      Y	     OOOO   !!!	                                    Thanks...  Stig.

	Please use 'diff -uw oldver newver' for patches.
")))

;;}}}
;;{{{ hilit-find-file-hook, plus hilit-* versions of standard commands.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-find-file-hook ()
  "Find-file hook for hilit package.  See the variable hilit-auto-highlight."
  (cond ((and hilit-auto-highlight
	      (assq major-mode hilit-patterns-alist))
	 (if (> buffer-saved-size (car hilit-auto-rehighlight-fallback))
	     (setq hilit-auto-rehighlight
		   (cdr hilit-auto-rehighlight-fallback)))
	 (if (> buffer-saved-size hilit-auto-highlight-maxout) nil
	   (hilit-rehighlight-buffer)
	   (set-buffer-modified-p nil)))))

(defun hilit-repaint-command (arg)
  "Rehighlights according to the value of hilit-auto-rehighlight, or the
prefix argument if that is specified.
\t\\[hilit-repaint-command]\t\trepaint according to hilit-auto-rehighlight
\t^U \\[hilit-repaint-command]\trepaint entire buffer
\t^U ^U \\[hilit-repaint-command]\trecompile patterns, then repaint buffer
\t^U - \\[hilit-repaint-command]\trepaint current 'defun' around cursor
\t^U 0 \\[hilit-repaint-command]\trepaint visible portion of buffer
\t^U n \\[hilit-repaint-command]\trepaint n lines to either side of point"
  (interactive "P")
  (let (st en quietly)
    (or arg (setq arg hilit-auto-rehighlight))
    (cond ((or (eq  arg 'visible) (eq arg 0))
	   (setq st (window-start) en (window-end) quietly t))
	  ((or (eq  arg 'defun) (eq arg '-))
	   (setq st (save-excursion (beginning-of-defun) (point))
		 en (save-excursion (end-of-defun) (point))
		 quietly t))
	  ((numberp arg)
	   (setq st (save-excursion (forward-line (- arg)) (point))
		 en (save-excursion (forward-line arg) (point))))
	  ((consp arg)
	   (cond ((eq (car arg) 64)
		  (setq hilit-buffer-compiled-patterns nil)
		  (let ((hilit-new-style-patterns
			 (not hilit-new-style-patterns)))
		    (hilit-rehighlight-buffer)))
		 (t
		  (and (eq (car arg) 16)
		       (setq hilit-buffer-compiled-patterns nil))
		  (hilit-rehighlight-buffer))))
	  (t (hilit-rehighlight-buffer)))
    (if st
	(hilit-rehighlight-region st en quietly))))

(defun hilit-recenter (arg)
  "Recenter, then rehighlight according to hilit-auto-rehighlight.  If called
with an unspecified prefix argument (^U but no number), then a rehighlight of
the entire buffer is forced."
  (interactive "P")
  (recenter arg)
  ;; force display update
  (sit-for 0)
  (hilit-repaint-command (and (consp arg) arg)))

(defun hilit-yank (arg)
  "Yank with rehighlighting"
  (interactive "*P")
  (let ((transient-mark-mode nil))
    (yank arg)
    (and hilit-auto-rehighlight
	 (hilit-rehighlight-region (region-beginning) (region-end) t))
    (setq this-command 'yank)))

(put 'hilit-yank 'delete-selection t)

(defun hilit-yank-pop (arg)
  "Yank-pop with rehighlighting"
  (interactive "*p")
  (let ((transient-mark-mode nil))
    (yank-pop arg)
    (and hilit-auto-rehighlight
	 (hilit-rehighlight-region (region-beginning) (region-end) t))
    (setq this-command 'yank)))

;;{{{ EXPERIMENTAL: dabbling with after-change-function

;; (defvar hilit-max-change-rehighlight 3000
;;   "maximum size to rehighlight with an after change hook")

(defun hilit-rehighlight-changed-lines (st en len)
  "Quietly rehighlight just this line.
Useful as an after change hook in VM/gnus summary buffers and dired buffers."
  (save-match-data
    ;; (> (- en st) hilit-max-change-rehighlight)
    (hilit-rehighlight-region st en 'quietly)))

(defun hilit-install-line-hooks ()
  "Enable on-the-fly highlighting in the current buffer only.
Before-change-hook gets trashed, presently."
  (interactive) 
  (set (make-local-variable 'after-change-function)
       'hilit-rehighlight-changed-lines))

;; (hilit-install-line-hooks)

;;}}}

;;}}}

;;}}}
;;{{{ Initialization.  hooks, key substitutions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hilit-hook-install (hookname &rest forms)
  "if HOOKNAME is in hilit-hook-list, then execute FORMS."
  (` (cond ((memq '(, hookname) hilit-hook-list)
	    (,@ forms)))))
(put 'hilit-hook-install 'lisp-indent-hook 'defun)

(defun hilit-respin-translations ()
  "build hilit-face-translation-table from hilit-user-face-table and
hilit-default-face-table"
  (setq hilit-face-translation-table
	(let ((index (or (and (x-display-color-p)
			      (cdr (assq hilit-background-mode
					 '((light . 1) (dark . 2)))))
			 3))
	      (wide-table (copy-alist hilit-user-face-table)))
	  ;; use defaults for all faces that the user has not specified
	  (mapcar (function
		   (lambda (pp)
		     (or (assq (car pp) wide-table)
			 (setq wide-table (cons pp wide-table)))))
		  hilit-default-face-table)
	  ;; now make the face translation table
	  (nreverse (mapcar (function
			     (lambda (x)
			       (cons (car x) (nth index x))))
			    wide-table)))))

(defun hilit-initialize ()
  "Install hooks, bind keys, translate faces, someday...precompile patterns"
  (if (not window-system)
      nil
    ;; 
    ;; key bindings
    ;;     
    (global-set-key [?\C-\S-l] 'hilit-repaint-command)
    (global-set-key [?\C-=] 'hilit-repaint-command)

    ;;
    ;; keymap substitutions
    ;;
    (hilit-hook-install yank
      (substitute-key-definition 'yank 'hilit-yank (current-global-map)))
    (hilit-hook-install yank-pop
      (substitute-key-definition 'yank-pop 'hilit-yank-pop (current-global-map)))
    (hilit-hook-install recenter
      (substitute-key-definition 'recenter 'hilit-recenter (current-global-map)))

    ;;
    ;; find-file hook
    ;;
    (hilit-hook-install find-file
      (add-hook 'find-file-hooks 'hilit-find-file-hook t))

    (hilit-hook-install info
      (add-hook 'Info-selection-hook 'hilit-rehighlight-buffer-quietly))

    (hilit-hook-install mail
      (mapcar (function (lambda (hook)
			  (add-hook hook 'hilit-rehighlight-buffer-quietly)))
	      '(vm-preview-message-hook        vm-show-message-hook
		rmail-show-message-hook	       mail-setup-hook
		mh-show-mode-hook))
      (add-hook 'vm-summary-mode-hooks 'hilit-install-line-hooks)
      (add-hook 'rmail-summary-mode-hook 'hilit-rehighlight-message-quietly))

    (hilit-hook-install gnus
      (add-hook 'gnus-article-prepare-hook 'hilit-rehighlight-buffer-quietly)
      (add-hook 'gnus-summary-prepare-hook 'hilit-rehighlight-buffer-quietly)
      (add-hook 'gnus-group-prepare-hook 'hilit-rehighlight-buffer-quietly)
      (add-hook 'gnus-summary-prepare-hook 'hilit-install-line-hooks)
      (add-hook 'gnus-group-prepare-hook 'hilit-install-line-hooks))

    (hilit-hook-install dired
      (add-hook 'dired-after-readin-hook 'hilit-rehighlight-buffer-quietly)
      (add-hook 'dired-after-readin-hook 'hilit-install-line-hooks))

    ;;
    ;; merge hilit-user-face-table and hilit-default-face-table and create
    ;; hilit-face-translation-table
    ;;
    ;; list of predefined faces
    (or hilit-predefined-face-list
	(setq hilit-predefined-face-list (face-list)))

    (hilit-respin-translations)
    ;;
    ;; Apply translations to predefined faces from hilit-user-face-table
    ;;
    (mapcar (function
	     (lambda (ff)
	       (let ((trans (assq ff hilit-face-translation-table)) nface)
		 (and trans (cdr trans)
		      (setq nface (hilit-lookup-face-create ff))
		      (copy-face nface ff)))))
	    hilit-predefined-face-list)
    ))

(if noninteractive
    (add-hook 'after-init-hook 'hilit-initialize)
  (hilit-initialize))

;;}}}
;; ------------------
;;{{{ GRIND GRIND GRIND!!! (performance meters)

(defun grind-patterns (mode buf &optional NN PP)
  (let ((pp (cdr (assq mode hilit-patterns-alist)))
	tin tout secs msecs N exp)
    (or NN (setq NN 100))
    (setq pp (cdr (hilit-compile-patterns (or PP pp))))
    (save-excursion
      (set-buffer buf)
      (mapcar (function
	       (lambda (xx)
		 (setq exp (car xx)
		       tin (current-time)
		       N NN)
		 (goto-char (point-min))
		 (while (>= (setq N (1- N)) 0)
		   (if (re-search-forward exp nil t)
		       nil
		     (princ " loop")
		     (goto-char (point-min))))
		 (setq tout (current-time))
		 (setq tin (+ (* 1000 (nth 1 tin)) (/ (nth 2 tin) 1000))
		       tout (+ (* 1000 (nth 1 tout)) (/ (nth 2 tout) 1000))
		       tin (- tout tin)
		       secs (/ tin 1000)
		       msecs (% tin 1000))
		 (princ (format
			 "\n %d.%03d secs for %d searches in %s (%d%%) for %S"
			 secs msecs NN buf
			 (/ (* 100 (- (- (point-max) (point-min)) (point)))
			    (- (point-max) (point-min)))
			 exp))
		 ))
	      pp)
      nil
      )))

;;}}}
;;{{{ hilit-highlight-region

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Application code and user functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-highlight-region (start end &optional patterns quietly)
  "Highlights the area of the buffer between START and END (the region when
interactive).  Without the optional PATTERNS argument, the pattern for
major-mode is used.  If PATTERNS is a symbol, then the patterns associated
with that symbol are used.  QUIETLY suppresses progress messages if
non-nil."
  (interactive "r")
  (setq patterns (hilit-get-compiled-patterns patterns))
  (let ((prio (1- (length patterns)))	; subtract case-fold arg from length
	(case-fold-search (car patterns))
	p pstart pend face region faces applyme mstart matchno ff)
    (setq patterns (cdr patterns))	; remove case-fold from head of pattern
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while patterns
	  (setq p (car patterns))
	  (setq pstart (car p)
		pend (nth 1 p)
		applyme nil
		p (nthcdr 2 p))

	  ;; pick face & decide if this pattern should be ignored
	  (if (consp (car p))
	      (setq faces p applyme p)
	    (setq face (car p) applyme face))

	  (if (not applyme)		; skipped if nil
	      nil
	    (or quietly hilit-quietly
		(message "highlighting %d: %s%s" prio pstart
			 (if pend (concat " ... " pend) "")))
	    (and (featurep 'profile) (profile-push-timer))
	    (goto-char (point-min))
	    (condition-case cc
		(cond
		 ((consp (car p))

		  ;;
		  ;; inner loop -- one regex, many faces
		  ;;
		  (setq oldspot (point))  
		  (while (and (not (eobp)) (re-search-forward pstart nil t nil))

		    ;; would this inner loop be faster?
		    ;;
		    ;; (let ((md (match-data))
		    ;;       (ff (cdr (cdr pp))))
		    ;;   (while ff
		    ;;     (and (car ff)
		    ;;          (car md)
		    ;;          (hilit-region-set-face (car md) (car (cdr md)) (car ff) prio))
		    ;;     (setq ff (cdr ff)
		    ;;           md (cdr (cdr md)))
		    ;;     ))

		    (setq ff faces)
		    (while ff
		      (and (setq matchno (car (car ff))
				 mstart (match-beginning matchno))
			   (setq face (cdr (car ff)))
			   (cond ((symbolp face)
				  (hilit-region-set-face
				   mstart (goto-char (match-end matchno))
				   (cdr (car ff)) prio)
				  ;; fixme -- won't highlight 2 subexps
				  (setq ff nil))
				 (t
				  ;; funcall -- ignore all other submatches
				  (setq region (apply (car face) matchno
						      (cdr face))
					;; (apply (car face) matchno (cdr face))
					ff nil)
				  (and (car region)
				       (hilit-region-set-face (nth 1 region)
							      (nth 2 region)
							      (car region) prio)))
				 ))
		      (setq ff (cdr ff)))
		    (and
		     (= oldspot (point))
		     (not (eobp))
		     (forward-char 1))
		    (setq oldspot (point))))


		 ((symbolp pstart)
		  ;;
		  ;; inner loop -- special function to find pattern
		  ;;
		  (let (region)
		    (while (setq region (funcall pstart pend))
		      (hilit-region-set-face (car region) (cdr region)
					     face prio))))
		 ((stringp pend)
		  ;;
		  ;; inner loop -- regex-start ... regex-end
		  ;;
		  (while (re-search-forward pstart nil t nil)
		    (goto-char (setq mstart (match-beginning 0)))
		    (if (re-search-forward pend nil t nil)
			(hilit-region-set-face mstart (match-end 0)
					       face prio)
		      (forward-char 1))))
		 (t
		  (error "Gack! -- I don't know how to make this pattern")))

	      (error (message "Aack! Barfed on '%s'" pstart)
		     (ding) (sit-for 2)))

	    ;; fixme -- shouldn't need this test
	    ;; (or (numberp pend) (setq pend 0))
	    ;; inner loop -- just one regex to match whole pattern
	    ;; (while (re-search-forward pstart nil t nil)
	    ;;   (hilit-region-set-face  (match-beginning pend)
	    ;;			   (match-end pend) face prio))))

	    ;; (and (featurep 'profile)
	    ;;      (with-output-to-debug-buffer "*hilit-profile*"
	    ;;        (princ (format "%d msecs on %s for %s"
	    ;;                       (/ (profile-pop-time) 1000)
	    ;;                       (if buffer-file-name
	    ;;                           (file-name-nondirectory buffer-file-name)
	    ;;                         "")
	    ;;                       (if (stringp pstart)
	    ;;                           (save-excursion
	    ;;                             (set-buffer (get-buffer-create " *foo*"))
	    ;;                             (erase-buffer) (insert pstart)
	    ;;                             (subst-char-in-region (point-min)
	    ;;                                                   (point-max)  ?\n ?\r t)
	    ;;                             (buffer-string))
	    ;;                         (prin1-to-string pstart))))))

            )

	  (setq prio (1- prio)
		patterns (cdr patterns)))
	))
    (or quietly hilit-quietly (message "")) ; "Done highlighting"
    ;; txt prop: (set-buffer-modified-p bm)) ; unwind protection
    ))

;;}}}
;;{{{ Pattern compiler

(defvar hilit-buffer-compiled-patterns nil
  "buffer-local default highlighting patterns")

(make-variable-buffer-local 'hilit-buffer-compiled-patterns)

(defun hilit-get-compiled-patterns (patterns)
  "Return the compiled pattern corresponding to PATTERNS.

PATTERNS may be a list of hl319 patterns, a symbol used to extract a pattern
list from hilit-patterns-alist, or nil.

Nil indicates that the value of hilit-buffer-compiled-patterns should first
be checked, and if that's nil then the pattern should be extracted from
hilit-patterns-alist using the value of major-mode (and then compiled and
cached in hilit-buffer-compiled-patterns)."
  (cond
   ;; no mode specified.  Use major-mode and compile it for next time
   ((null patterns)
    (or hilit-buffer-compiled-patterns
	(setq hilit-buffer-compiled-patterns
	      (hilit-get-compiled-patterns major-mode)))
    hilit-buffer-compiled-patterns)
   ;; mode has been specified
   ((symbolp patterns)
    (let ((mpats (cdr (assq patterns hilit-patterns-alist))))
      (and mpats (hilit-get-compiled-patterns mpats))))
   ;; precompiled patterns
   ((and (consp patterns) (eq (car patterns) 'compiled))
    (cdr patterns))
   ;; pattern list
   (t
    (hilit-compile-patterns
     patterns
     (and (boundp 'fold-top-mark) fold-top-mark
	  (` (("^[ \t]*" (:funcall: hilit-fold-mark-fn ((fold-open fold-folded))
				 ((, (regexp-quote fold-top-mark)) "[^\r\n]*")
				 (, (regexp-quote fold-bottom-mark))
				 ) ".*"))
	     )))
    )))

;; (hilit-get-compiled-patterns nil)

(defvar lcp nil) ; fixme - last compiled pattern.  just here for debugging

(defun hilit-compile-patterns (pats &optional local-pats)
  "Compiles patterns into a form that's useful to hilit-highlight-region.

If the variable hilit-new-style-patterns is non-nil, then this function
gloms all the patterns in each :buildme: together into ONE BIG REGULAR
EXPRESSION...  This gives more correct highlighting, but it also may have a
tendency to thrash emacs (given the current inefficiency of FSF 19's regular
expression matcher).

Subexpressions whose faces have been mapped to nil are 'optimized' out of
the compiled pattern."

  ;(or hilit-quietly (message "Compiling patterns"))
  (let (rv pp)
    (while pats
      (setq pp (car pats)
	    pats (cdr pats))
      (cond ((symbolp pp) (setq rv (cons pp rv)))
	    ((not (consp pp)) (error "bad pattern %S" pp))
	    ((eq ':buildme: (car pp))
	     (setq pp (cdr pp))
	     (if local-pats (setq pp (append local-pats pp)
				  local-pats nil))
	     (if hilit-new-style-patterns
		 (setq rv (cons (hilit-build-pat pp) rv))
	       ;; make separate regular expressions for each one
	       (while pp
		 (setq rv (cons (hilit-build-pat (list (car pp))) rv)
		       pp (cdr pp)))))
	    (t
	     ;; old style patterns -- convert
	     (let ((face (hilit-lookup-face-create (car (cdr (cdr pp))))))
	       (setq rv (cons (if (symbolp (car pp))
				  (list (car pp) (car (cdr pp)) face) 
				(cons (car pp)
				      (if (stringp (car (cdr pp)))
					  (list (car (cdr pp)) face) 
					(append
					 '(nil)
					 (and face
					      (list (cons (or (car (cdr pp)) 0) face))))
					)))
			      rv))
	       ))))
    (setq rv (nreverse rv))
    (or (symbolp (car rv)) (setq rv (cons nil rv)))
    ;(or hilit-quietly (message ""))
    (setq lcp rv)
    ))

(defvar hilit-build-pat-flist nil)
(defvar hilit-build-pat-num nil)

;;{{{ "Pseudo Optimal" or builder ... merges keywords to avoid backtracking

(defun hilit-opt-regexp (words)
  (let (rwords cchars cwords thisword w2)
    (setq words (sort words 'string-lessp))
    (while words
      (setq cchars 0
	    thisword (car words)
	    words (cdr words)
	    w2 (car words))
      (cond ((and w2
		  (not (string= thisword ""))
		  (or (while (and (< cchars (min (length thisword) (length w2)))
				  (not (memq (aref thisword cchars) '(?\\ ?\[)))
				  (eq (aref thisword cchars) (aref w2 cchars)))
			(setq cchars (1+ cchars)))
		      (not (zerop cchars))))
	     (setq thisword (concat (substring thisword 0 cchars)
				    "\\(" (substring thisword cchars))
		   hilit-build-pat-num (1+ hilit-build-pat-num))
	     (while (and words
			 (< cchars (length (car words)))
			 (equal (substring thisword 0 cchars)
				(substring (car words) 0 cchars)))
	       (setq thisword (concat thisword "\\|"
				      (substring (car words) cchars))
		     words (cdr words)))
	     (setq thisword (concat thisword "\\)"))))
      (setq rwords (cons thisword rwords)))
    (concat "\\(" (mapconcat 'identity (nreverse rwords) "\\|") "\\)")))

;;}}}
;;{{{ hilit-build-pat-subex, hilit-build-pat-con

(defun hilit-build-or (argl &optional fn)
  (let ((rv "") (jj ""))
    (mapcar (function (lambda (ss)
			(setq ss (hilit-build-pat-elem ss))
			(or (string= ss "")
			    (setq rv (concat rv jj ss)
				  jj "\\|")) ))
	    argl)
    rv))

(defun hilit-build-pat-subex (argl &optional face)
  "Part of the hl319 pattern builder -- see hilit-build-pat.

ARGL is a list of expressions to be expanded and assembled thusly:
       \"\\( <arg1> [\\| <argn> ...] \\)\"

FACE, if specified, is either a symbol (in which case it's a virtual face),
or a cons of the form
       (function <face | face-list> [other-args]).

If a face or a parsing function is given, then all specified faces are
translated and if they all translate to nil, then the expressions in ARGL
are not expanded and \"\" is returned.

SIDE EFFECTS: hilit-build-pat-num is incremented,
	      hilit-build-pat-flist is prepended with (subexno . FACE)"

  ;;FIXME - use local vars ... more readable
  ;; translate face and if it's nil, then ignore this whole subexpression
  (setq face (cond ((and face (symbolp face)) (hilit-lookup-face-create face))
		   ((consp face)
		    (let* ((flist (car (cdr face)))
			   farg compileme)
		      (cond ((consp flist)
			     (setq farg (mapcar (function
						 (lambda (ff)
						   (let ((rv (hilit-lookup-face-create ff)))
						     (and rv (setq compileme t))
						     rv)))
						flist)))
			    (t
			     (setq farg (hilit-lookup-face-create flist)
				   compileme farg)))
		      (and compileme 
			   (cons (car face) (cons farg (cdr (cdr face)))))))
		   (t t)))
  (cond (face
	 (setq hilit-build-pat-num (1+ hilit-build-pat-num))
	 (or (eq face t)
	     (setq hilit-build-pat-flist (cons (cons hilit-build-pat-num face)
					       hilit-build-pat-flist)))
	 (if (eq (car argl) ':opt:)
	     (hilit-opt-regexp (mapcar (function hilit-build-pat-elem) (cdr argl)))
	   (concat "\\(" (hilit-build-or argl) "\\)")))
	(t "")))

(defun hilit-build-pat-con (arg)
  (apply 'concat (mapcar (function hilit-build-pat-elem) arg)))

;;}}}
;;{{{ hilit-var-expand -- inline expand variables

(defun hilit-var-expand (ll)
  (let (e rv)
    (while ll
      (setq e (car ll)
	    ll (cdr ll)
	    rv (append rv (if (and (symbolp e) (boundp e))
			      (progn (setq e (symbol-value e))
				     (if (listp e) e (list e)))
			    (list e)))))
    rv))

;;}}}

(defun hilit-count-subexps (str)
  (let ((pos 0))
    (while (setq pos (string-match "\\\\(" str pos))
      (setq hilit-build-pat-num (1+ hilit-build-pat-num)
	    pos (1+ pos))))
  str)

(defun hilit-build-pat-elem (e)		;FIXME - &optional enclosed-already
					; to avoid extra \\( \\) pairs
  (and (symbolp e) (boundp e) (setq e (symbol-value e))) ; expand vars
  (cond ((stringp e) (hilit-count-subexps e))
	((eq e '*) "*")
	((eq e '+) "+")
	((eq e ??) "?")
	((consp e)
	 (setq e (hilit-var-expand e)) ; expand vars, appending them into the list
	 (cond ((eq (car e) 'and)
		(error "defunct %S" e)
		(hilit-build-pat-con (cdr e)))
	       ((eq (car e) 'or)
		(error "defunct %S" e)
		(hilit-build-pat-subex (cdr e)))

	       ((eq (car e) ':or:)
		(hilit-build-pat-subex (cdr e)))
	       ((eq (car e) ':opt:)
		(hilit-build-pat-subex e))
	       ((eq (car e) ':and:)
		(hilit-build-pat-con (cdr e)))

	       ((eq (car e) ':label:)
		;; (:label: face/action ex [ex...])
		(or (> (length e) 3) (error ":label: malformed %S" e))

		)

	       ((eq (car e) ':funcall:)
		;; (:funcall: fun arglist ex [ex...])
		;;
		;; the first element of arglist must be either a face or a
		;; list of faces
		;;
		;; Makes the call (apply fun matchno arglist) and expects
		;;     * the return value (face start end)
		;;     * that (point) is set to wherever the search for the
		;;	 next pattern should begin.
		;;
		;; Example:
		;;     (:funcall: parsefun ((f1 f2) arg2) "foo" "bar")
		;;
		;;     (defun parsefun (matchno faces arg2) ...)
		;;
		;; Parsefn will be called whenever \"\\(foo\\|bar\\)\" is
		;; encountered in the highlighting search.
		;;
		(or (and (> (length e) 3) (symbolp (car (cdr e))))
		    (error ":funcall: malformed %S" e))
		(hilit-build-pat-subex (nthcdr 3 e)
				       (cons (nth 1 e) (nth 2 e))))
	       ((eq (car e) ':pair:)
		;; (:pair: face/action start-elm end-elm)
		;;
		;; start-elm must label the subexpression :start:
		;; end-elm must label the subexpression :end:
		;;
		(or (eq (length e) 4) (error ":pair: malformed %S" e))

		(hilit-build-pat-subex (cdr e)))

	       ((symbolp (car e))	; fixme - should be implicit :and:
		;; (face ex [ex...])

		;; put a face on this subexpression.  should be :and:
		;; instead of :or:

		;; (error "FIXME -- pattern syntax needs to change")
		(hilit-build-pat-subex (cdr e) (car e)))
	       (t
		;; implicit :or:, should be :and:
		;;(error "FIXME -- %S" e)
		(hilit-build-pat-con e))))
	(t
	 (error (format "hilit-build-pat-expr barfed on: %S" e)))))

(defun hilit-build-pat (elist &optional masterface)
  (setq hilit-build-pat-num 0
	hilit-build-pat-flist (and masterface (list (cons 0 masterface))))
  (let ((str (hilit-build-or elist)))
    (nconc (list str nil) (nreverse hilit-build-pat-flist))))

;;}}}

;;{{{ hilit-set-mode-patterns

;;; do I need this?  I changed the defconst to a defvar because defconst is
;;; inappropriate, but I don't know why I wanted hilit-patterns-alist to be
;;; reset on every reload...

(setq hilit-patterns-alist nil)

(defun hilit-set-mode-patterns (modelist patterns
					 &optional parse-fn case-fold)
  "Sets the default highlighting patterns for MODE to PATTERNS.
See the variables hilit-patterns-alist, hilit-parser-alist, and
hilit-mode-enable-list.

Takes optional arguments PARSE-FN and CASE-FOLD."
  (setq patterns (cons case-fold patterns))
  (or (consp modelist) (setq modelist (list modelist)))
  (let (ok (flip (eq (car hilit-mode-enable-list) 'not)))
    (mapcar (function
	     (lambda (m)
	       (setq ok (or (null hilit-mode-enable-list)
			    (memq m hilit-mode-enable-list)))
	       (and flip (setq ok (not ok)))
	       (and ok
		   (progn
		     (and parse-fn
			  (hilit-associate 'hilit-parser-alist m parse-fn))
		     (hilit-associate 'hilit-patterns-alist m patterns)))))
	    modelist)))

;;{{{ add-pattern

;; (defun hilit-add-pattern (pstart pend face &optional mode first)
;;   "Highlight pstart with face for the current major-mode.
;; Optionally, place the new pattern first in the pattern list"
;;   (interactive "sPattern start regex: \nsPattern end regex (default none): \nxFace: ")
;;
;;   (and (equal pstart "") (error "Must specify starting regex"))
;;   (cond ((equal pend "") (setq pend 0))
;;         ((string-match "^[0-9]+$" pend) (setq pend (string-to-int pend))))
;;   (or mode (setq mode major-mode))
;;   (let ((old-patterns (cdr (assq mode hilit-patterns-alist)))
;;         (new-pat (list pstart pend face)))
;;     (cond ((not old-patterns)
;;            (hilit-set-mode-patterns mode (list new-pat)))
;;           (first
;;            (setcdr old-patterns (cons new-pat (cdr old-patterns))))
;;           (t
;;            (nconc old-patterns (list new-pat)))))
;;   (and (interactive-p) (hilit-rehighlight-buffer)))

;;}}}

;;}}}
;;{{{ hilit-string-find

(defun hilit-string-find (qchar)
  "looks for a string and returns (start . end) or NIL.  The argument QCHAR
is the character that would precede a character constant double quote.
Finds strings delimited by double quotes.  The first double quote may not be
preceded by QCHAR and the closing double quote may not be preceded by an odd
number of backslashes."
  (let (st en)
    (while (and (search-forward "\"" nil t)
		(eq qchar (char-after (1- (setq st (match-beginning 0)))))))
    (while (and (search-forward "\"" nil t)
		(save-excursion
		  (setq en (point))
		  (forward-char -1)
		  (skip-chars-backward "\\\\")
		  (forward-char 1)
		  (not (zerop (% (- en (point)) 2))))))
    (and en (cons st en))))

(defun hilit-fold-mark-fn (matchno faces)
  "Hl319 parse function for folding-mode fold marks.

MATCHNO is the number of subexpression that was used to match the fold-mark
to be highlighted.

FACES is a list of the form (open-face folded-face)."
  (goto-char (match-end matchno))
  (let ((st (match-beginning matchno))
	(en (point))
	(nchar (following-char)))
    (forward-line 1)
    (or (eobp) (beginning-of-line))
    (list (if (eq nchar ?\r) (car (cdr faces)) (car faces)) st en)))

(defun hilit-string-find-fn (matchno face)
  "start pattern must be [^QCHAR]\"."
  ;;   "looks for a string and returns (start . end) or NIL.  The argument QCHAR
  ;; is the character that would precede a character constant double quote.
  ;; Finds strings delimited by double quotes.  The first double quote may not be
  ;; preceded by QCHAR and the closing double quote may not be preceded by an odd
  ;; number of backslashes."
  (forward-char -1)
  (let ((st (point)))
    (condition-case nil
	(if (eq (preceding-char) ?\\)
	    (forward-char 1)
	  ;; (progn
	  ;; (while (and (search-forward "\"" nil t)
	  ;;             (save-excursion
	  ;;               (setq en (point))
	  ;;               (forward-char -1)
	  ;;               (skip-chars-backward "\\\\")
	  ;;               (forward-char 1)
	  ;;               (not (zerop (% (- en (point)) 2))))))
	  ;; en
	  (list face st (progn (forward-sexp 1) (point))))
      (error (forward-char 1)))))

;;}}}
;;{{{ PATTERNS: C, C++

;; return types on same line...
;; ("^[a-zA-z].*\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)

;; On another note, a working pattern for grabbing function definitions for C is
;;
;; ("^[a-zA-Z_]+.*[;{]$" nil ForestGreen)  ; global defns ( start at col 1 )
;; ("^[a-zA-Z_]+.*(" ")" defun)
;; ; defuns assumed to start at col 1, not with # or {
;;
;; this will make external declarations/definitions green, and function
;; definitions the defun face.  Hmmm - seems to work for me anyway.

(defvar hilit-local-ctypes nil		; '("\\w+_[tT]")
  "set this to a list of regular expressions that match the typedefs that
you commonly use in your C/C++ sourcefiles.
	Ex: '(\"\\\\w+_[Tt]\") is one pattern that's quite useful... but it's slow.")

(let ((c-comments     '(("/\\* \\(FIXME\\|fixme\\)" "\\*/" urgent) ; this winds up being 2 overlays
			("/\\*" "\\*/" comment)))
      (c-strings      '((hilit-string-find ?' string)))
      (c-identifier   "\\<[A-Za-z0-9_$]+\\>")
      (c-preprocessor-exps
       '((define ("^#[ \t]*" (:or: "undef" "define\\(.*\\\\\n\\)*") ".*"))
	 (include "^#[ \t]*[^ud].*")))
      (c-declwords '("typedef"))
      (c-complextypes '("struct" "union" "enum"))
      (c-keywords '("return" "goto" "if" "else" "case" "default" "switch"
		    "break" "continue" "while" "do" "for"))
      (c-typemods '("const" "register" "volatile" "unsigned" "extern" "static"))
      (c++-opname '((:and: "operator[ \t]*" (:opt: "[+-*/%^&|~!=<>,]=?"
						   "new" "delete" "()" "\\[\\]"
						   "<<" ">>" "<<=" ">>=" "&&" "||"
						   "\\+\\+" "--" "->" "->\\*"))))
      (c-types '("float" "double" "void" "char" "short" "unsigned"
		 "int" "long" "FILE" hilit-local-ctypes)))

  (hilit-set-mode-patterns
   '(c-mode c++-c-mode elec-c-mode)
   (` ((,@ c-comments)
       (,@ c-strings)
       (:buildme:
	(,@ c-preprocessor-exps)
	("^[ \t]*" (label "[a-zA-Z0-9_]+:"))
	;; FIXME - "Give up (and stop debugging it)? " not defun
	(defun ("^[A-Za-z0-9_$][A-Za-z0-9_$ \t\n*]+([^)]*)+"))
	;;         (defun ("^\\(" (, c-identifier) "[* \t]+\\)*"
	;;                 (, c-identifier) "[ \t]*" "([^)]*)+"))
	(decl ("^" (:opt: (,@ c-declwords) (,@ c-complextypes))
	       ".*$"))
	("[ \t\n({,]" (type ("\\(" (:opt: (,@ c-typemods)) "\\s +\\)*"
			   (:opt: (,@ c-types)
				 (:and: (:or: (,@ c-complextypes))
					"[ \t]+" (, c-identifier)))
			   "[* \t]+")))
	(:and: "\\S_\\<" (keyword :opt: (,@ c-keywords)) "\\>\\S_"))
       )))

  (setq c-comments (append c-comments '(("[/\n]/.*$" nil comment)))
	c-keywords (nconc '("delete" "new" "public" "protected" "private")
			  c-keywords)
	c-identifier (concat c-identifier "\\(::~?" c-identifier "\\)?")
	c-complextypes (cons "class" c-complextypes)
	c-declwords (nconc '("template" "public" "private" "protected")
			   c-declwords))

  (hilit-set-mode-patterns
   'c++-mode
   (` ((,@ c-comments)
       (,@ c-strings)
       (:buildme:
	(,@ c-preprocessor-exps)
	;;	(defun ("^[A-Za-z0-9_$][A-Za-z0-9_$ \t\n*]+([^)]*)+"))
	("^[ \t]*" (label "[a-zA-Z0-9_]+:"))
	(defun ("^\\(" (, c-identifier) "[* \t]+\\)*"
		(:or: (,@ c++-opname) (, c-identifier))
		"[ \t]*" "([^)]*)+"))
	(decl ("^" (:opt: (,@ c-declwords) (,@ c-complextypes))
	       ".*$"))
	("[ \n\t({,]"
	 (type ("\\(" (:opt: (,@ c-typemods)) "\\s +\\)*"
		(:opt: (,@ c-types)
		      (:and: (:or: (,@ c-complextypes))
			     "[ \t]+" (, c-identifier)))
		"[*& \t]+")))
	("\\S_\\<" (keyword :opt: (,@ c-keywords)) "\\>\\S_"))
       )))
  )

;;}}}
;;{{{ PATTERNS: Perl, ml

(hilit-set-mode-patterns
 'perl-mode
 '((:buildme:
    (comment "[ \t]#.*$")
    (string "\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\""
	    "'[^']*'")
    (define ("^#[ \t]*" (:or: "undef" "define\\(.*\\\\\n\\)*") ".*"))
    (keyword "&\\(\\w\\|\\s_\\)+"
	     ("\\b" (:opt: "do" "if" "unless" "while" "until" "else"
			   "elsif" "for" "foreach" "continue" "next" "redo"
			   "last" "goto" "return" "die" "exit") "\\b"))
    ("^" (:or: (include "require.*$" "#[ \t]*include.*")
	       (comment ";?#.*$")
	       (label "\\(__....?__\\|\\s *\\w+:\\)")
	       (decl    "package.*$")
	       (defun "\\s *sub\\s +\\(\\w\\|[_']\\)+")))
    )))

(hilit-set-mode-patterns
 'ml-mode
 '(
   ("(\\*" "\\*)" comment)
   (hilit-string-find ?\\ string)
   ("\\([ \n\t();]\\|\\`\\)\\(abstype\\|and\\|andalso\\|as\\|case\\|datatype\\|do\\|else\\|end\\|eqtype\\|exception\\|fn\\|fun\\|functor\\|handle\\|if\\|in\\|include\\|infix\\|infixr\\|let\\|local\\|nonfix\\|of\\|op\\|open\\|orelse\\|raise\\|rec\\|sharing\\|sig\\|signature\\|struct\\|structure\\|then\\|type\\|val\\|while\\|with\\|withtype\\)\\([ \n\t();]\\|\\'\\)" 2 struct)
   ("\\(=>\\|->\\||\\)" nil struct)
   ))

;;}}}
;;{{{ PATTERNS: emacs lisp, lisp

(let ((elisp-keywords '("let\\*?" "cond" "if" "or" "and"
			"mapcar" "mapconcat" "prog[n12]"
			"while" "lambda" "function" "set" "setq" "fset"
			"setcar" "setcdr" "nconc" "nreverse" "condition-case"
			"unwind-protect" "catch" "throw" "error"))
      (elisp-decls '("defvar"))
      (elisp-consts '("defconst"))
      (elisp-includes '("load" "autoload" "require" "provide"
			"eval-when-compile" "eval-when-load"))
      (elisp-defuns '("defun" "defmacro" "defsubst" "defalias" "defadvice"))
      (elisp-defun-args '(:and: "\\s +\\S +\\s +" (:or: "nil" "([^()]*)")))
      )

  ;; match multi-line comments w/just one pattern...
  ;; but more importantly, just one overlay and just two markers.

  ;; This almost works...but I think I'll stick with the parser function for now
  ;; IF, however, this gets fixed, then the string and comment patterns can
  ;; interlock and avoid overlap...faster and more correct.
  ;;
  ;; (list 're-search-forward
  ;;       (car (hilit-build-pat '((and "[^?]"
  ;;      (string "\"\"" (:and: "\"" (:or: "[^\\\"]+" "\\([\\].\\)+") * "\"")))))))
  ;;
  ;;("[^?]\\(\"\\(\"\\||\\([^\"]+\\|[\\]\\([\\][\\]\\)*\"\\)*\"\\)\\)" 1 string)

  ;; this WORKS just fine in version 18, but there's a BUG (no shit?) in the
  ;; version 19 regex library that makes it slow as hell on strings w/ '-' in them
  ;;     ("\"\\([^\\\"]\\|[\\].\\|[\\]\n\\)*\"" nil string)
  ;;     (hilit-string-find ?\\ string)

  (hilit-set-mode-patterns
   '(emacs-lisp-mode lisp-interaction-mode)
   (` (
       (:buildme: 
	;; (comment ";.*\\(\n+;.*\\)*")
	(urgent ";+[ \t]*\\(FIXME\\|fixme\\).*")
	(comment ";.*$")
	(:funcall: hilit-string-find-fn (string) "\"")
	("^\\s *"
	 (:or: (defun (:and: "(" (:opt: (,@ elisp-defuns)) (, elisp-defun-args)))
	       (decl  (:and: "(" (:opt: (,@ elisp-decls)) "\\s +\\S +"))
	       (define (:and: "(" (:opt: (,@ elisp-consts)) "\\s +\\S +"))
	       (include (:and: "(" (:opt: (,@ elisp-includes)) ".*)"))))
	("\\s " (keyword :opt: "&rest" "&optional") "\\s ")
	("(" (keyword :opt: (,@ elisp-keywords)) "[ \t\n]")
	)
       )))

  ;;{{{ lisp

  (setq elisp-defuns (cons "method" elisp-defuns)
	;; THIS PATTERN IS *MASSIVELY* SLUGGISH
	;; elisp-defun-args '((:and: "\\s +\\S +\\s +"
	;;                           (:or: "nil" (:and: "(" (:or: "([^()]*)" "[^()]+")
	;;                                              "*)"))))
	elisp-decls (nconc '("deftype" "defparameter" "declare")
			   elisp-decls)
	elisp-consts (nconc '("defconstant" "defclass" "defstruct")
			    elisp-consts)
	elisp-keywords
	(nconc '("locally" "if*" "mapcon" "mapcan" "prog[*v]" "when"
		 "unless" "do\\*" "dolist" "dotimes" "values" "setf"
		 "rplac[ad]" "block" "go" "return" "return-from"
		 "multiple-value-\\(bind\\|setq\\|list\\|call\\|prog1\\)"
		 "[ec]?\\(type\\)?case" "handler-case" "eval-when")
	       elisp-keywords))

  (hilit-set-mode-patterns
   '(lisp-mode ilisp-mode)
   (` (
       (";.*" nil comment)
       ("#|" "|#" comment)
       (hilit-string-find ?\\ string)
       (:buildme:
	"^\\s *" (:or: (defun (:and: "(" (:opt: (,@ elisp-defuns)) (, elisp-defun-args)))
		       (decl  (:and: "(" (:opt: (,@ elisp-decls)) "\\s +\\S +"))
		       (define (:and: "(" (:opt: (,@ elisp-consts)) "\\s +\\S +"))
		       (include (:and: "(" (:opt: (,@ elisp-includes)) ".*)"))))
       (:buildme:
	(:or: (:and: "\\s " (keyword :opt: "&key" "&aux" "&rest" "&optional") "\\s ")
	      (:and: "(" (keyword :opt: (,@ elisp-keywords)) "[ \t\n]")))
       )))

  ;;}}}
  )

;; (` (global-set-key [f10] (function
;;                         (lambda () (interactive)
;;                           (re-search-forward
;;                            (, (car (nth 2 (assq 'emacs-lisp-mode
;;                                                 hilit-patterns-alist))))
;;                            )))))

;;}}}

;;{{{ Initialization.  default patterns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default patterns for various modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ PATTERNS: mail, gnus

(let* ((header-patterns (` ((:buildme:
			   (msg-subject "^Subject:.*")
			   (msg-from "^From:.*")
			   (msg-separator ("^" (, mail-header-separator) "$"))
			   (msg-header "^[A-Za-z][A-Za-z0-9-]+:")))))
       (body-patterns '(("^\\(In article\\|[ \t]*\\w*[]<>}|]\\).*$"
			 nil msg-quote)))
       (message-patterns (append header-patterns body-patterns)))
  (hilit-set-mode-patterns 'msg-header header-patterns)
  (hilit-set-mode-patterns 'msg-body body-patterns)
  (hilit-set-mode-patterns 'gnus-article-mode body-patterns 'hilit-gnus-message)
  (hilit-set-mode-patterns '(vm-mode text-mode mail-mode rmail-mode
				     news-reply-mode mh-show-mode)
			   message-patterns
			   'hilit-rehighlight-message))

(hilit-set-mode-patterns
 'gnus-group-mode
 '((:buildme:
    (gnus-group-unsubscribed "^U.*$")
    (gnus-group-empty	     "^ +[01]?[0-9]:.*$")
    (gnus-group-full	     "^ +[2-9][0-9]:.*$")
    (gnus-group-overflowing  "^ +[0-9][0-9][0-9]+:.*$")
    )))

(hilit-set-mode-patterns
 'gnus-summary-mode
 '(
   (:buildme:
    (summary-current "^. +[0-9]+:\\+\\[.*$")
    (summary-seen    "^D +[0-9]+: \\[.*$")
    (summary-killed  "^K +[0-9]+: \\[.*$")
    (summary-Xed     "^X +[0-9]+: \\[.*$")
    (summary-unread  "^- +[0-9]+: \\[.*$")
    (summary-new     "^  +[0-9]+: \\[.*$")
    )))

;; vm-summary patterns work with the default format and this one:
;; (setq vm-summary-format "%a%3.3n %-20.20F %3.3m %2.2d %5.5h %4.4l:  %s\n")

(hilit-set-mode-patterns
 'vm-summary-mode
 '((:buildme:
    (summary-current "^->.*$" )
    (summary-seen    "^\\(   ?[0-9]+\\)?   .*$")
    (summary-deleted "^\\(   ?[0-9]+\\)?  D.*$")
    (summary-unread  "^\\(   ?[0-9]+\\)?  U.*$")
    (summary-new     "^\\(   ?[0-9]+\\)?  N.*$")
    )))

(hilit-set-mode-patterns
 'rmail-summary-mode
 '(("[0-9]*-\\(JAN\\|FEB\\|MAR\\|APR\\|MAY\\|JUN\\|JUL\\|AUG\\|SEP\\|OCT\\|NOV\\|DEC\\)" nil decl) ; dates
   ("^ *[0-9]+[A-Z]+" nil summary-current) ; marks
   ("{" "}" define)			; labels
   ("to:" nil defun)
   (" [^ ]*@[^ ]* " nil type)		; addresses
   ("\\[" "\\]" include)		; replies
   ("re: .*$" nil include)		; replies
   )
 nil 'case-insensitive)

;;}}}

;;{{{ PATTERNS: compilation, makefile

(hilit-set-mode-patterns
 'compilation-mode
 '(
   ("^[-_./\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+:.*$" nil error)
   ("^[-_./\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+: warning:.*$" nil warning)
   ))

(hilit-set-mode-patterns
 'makefile-mode
 '((:buildme: (comment "#.*")
	      ;; (comment "[^$]#.*")
	      (include "^include .*")
	      (rule (:and: "^[^ \t\n]*%[^ \t\n]*[ \t]*"
			   "::?"
			   "[ \t]*[^ \t\n]*"
			   "[ \t]*\\(#.*\\)?"))
	      (rule "^[.][A-Za-z][A-Za-z]?\..*")
	      ;; variable definition
	      (define "^[_A-Za-z0-9]+[ \t]*\+?=")
	      ;; dependency
	      (defun "^[A-Za-z0-9.,/_-]+[ \t]*:.*")
	      ;; variable references
	      (keyword "$\\([^ \t\n{(]\\|[{(]@?[_A-Za-z0-9:.,%/=]+[)}]\\)")
	      ;; rules
	      (define "\\( \\|:=\\)[_A-Za-z0-9]+[ \t]*\\+=")
	      )))

;;}}}

;;{{{ PATTERNS: dired, jargon, Info

(hilit-set-mode-patterns
 'dired-mode
 (` ((:buildme: (deleted   "^D.*")
		(marked    "^\\*.*")
		(ignored ("^  .*\\("
			  (:or: "#.*#"
				(,@ (mapcar
				     'regexp-quote
				     completion-ignored-extensions)))
			  "\\)$"))
		(include   "^  d.*")
		(crossref  "^  l.*$")
		(socket	   "^  [sp].*")
		(special   "^  [cb].*")
		(urgent    "^  .\\(..[sS]\\|.....[sS]\\).*")
		(defun	   "^  -\\(..x\\|.....x\\|........x\\).*")
		))))

(hilit-set-mode-patterns
 'jargon-mode
 '(("^:[^:]*:" nil jargon-entry)
   ("{[^}]*}+" nil jargon-xref)))

(hilit-set-mode-patterns
 'Info-mode
 '((:buildme:
    (jargon-entry "^\\* [^:]+:+")
    (jargon-xref "\\*[Nn]ote\\b[^:]+:+")
    (jargon-xref "  \\(Next\\|Prev\\|Up\\):")
    ;; keywords for the emacs & lisp manuals
    (jargon-keyword "^[ \t]*[*][A-Za-z][^*]+[*]" ; bfd.info (output of 'chew')
		    (:and: "- " (:or: "Variable" "Function" "Macro" "Command"
				      "Special Form" "User Option") ":.*$"))
    )))

;;}}}

;;{{{ PATTERNS: ada, fortran, prolog, modula-2

(hilit-set-mode-patterns
 'ada-mode
 '(;; comments
   ("--.*$" nil comment)
   ;; main structure
   ("[ \t\n]procedure[ \t]" "\\([ \t]\\(is\\|renames\\)\\|);\\)" glob-struct)
   ("[ \t\n]task[ \t]" "[ \t]is" glob-struct)
   ("[ \t\n]function[ \t]" "return[ \t]+[A-Za-z_0-9]+[ \t]*\\(is\\|;\\|renames\\)" glob-struct)
   ("[ \t\n]package[ \t]" "[ \t]\\(is\\|renames\\)" glob-struct)
   ;; if there is nothing before "private", it is part of the structure
   ("^[ \t]*private[ \t\n]" nil glob-struct)
   ;; if there is no indentation before the "end", then it is most
   ;; probably the end of the package
   ("^end.*$" ";" glob-struct)
   ;; program structure -- "null", "delay" and "terminate" omitted
   ("[ \n\t]\\(in\\|out\\|select\\|if\\|else\\|case\\|when\\|and\\|or\\|not\\|accept\\|loop\\|do\\|then\\|elsif\\|else\\|for\\|while\\|exit\\)[ \n\t;]" nil struct)
   ;; block structure
   ("[ \n\t]\\(begin\\|end\\|declare\\|exception\\|generic\\|raise\\|return\\|package\\|body\\)[ \n\t;]" nil struct)
   ;; type declaration
   ("^[ \t]*\\(type\\|subtype\\).*$" ";" decl)
   ("[ \t]+is record.*$" "end record;" decl)
   ;; "pragma", "with", and "use" are close to C cpp directives
   ("^[ \t]*\\(with\\|pragma\\|use\\)" ";" include)
   ;; nice for named parameters, but not so beautiful in case statements
   ("[A-Za-z_0-9.]+[ \t]*=>"   nil named-param)
   ;; string constants probably not everybody likes this one
   ("\"" ".*\"" string)))

(hilit-set-mode-patterns
 'fortran-mode
 '((:buildme:
    (comment "^[*c].*$"                 ; Full-line comments.
             "! [^\n]*")                ; VAX Fortran-style comments.
    (string "'[^'\n]*'")        
    ;; All of these patterns except the last must appear at the beginning
    ;;  of the line.  (i.e., they can't have statement numbers.)
    ("^[ \t]+"
     (defun "program" "subroutine" "function" "entry" "block[ \t]*data")
     "\\>")
    ("^[ \t]+" (keyword "end\\([ \t]*\\(if\\|do\\)\\)?" "else") "\\>")
    ;; Variable declarations, and IMPLICIT and DIMENSION statements.
    ("^[ \t]+" (type "implicit[ \t]*none"
                     "dimension"
                     (:and: "\\(implicit[ \t]+\\)?"
                            (:or: "real" "integer" "character" "logical"
                                  "complex"
                                  (:and: "double[ \t]*"
                                         (:opt: "precision" "complex")))
                            "\\(\\*[0-9]+\\)?")))
    ("^[ \t]+" (include "include[ \t]*'[^'\n]+'"))
    ("^[ \t]+" (label "[0-9]+"))        ; Statement numbers.
    ("^     " (label "[^ \t\n]"))       ; Continuation characters.
    ;; Other declarations.
    ("^[ \t]+" (decl "external" "intrinsic" "equivalence" "data" "save" 
                     "common[ \t]*/[^/]*/" "parameter[ \t]*([^)]*)"))
    (operator (:and: "\\."		; Operations on LOGICAL variables.
		     (:opt: "true" "false" "or" "and" "not"
			    "eq" "ne" "gt" "lt" "ge" "le")
		     "\\."))
    ("\\<" (keyword                     ; Keywords that can appear anywhere,
            "else[ \t]*if"              ; e.g., after a statement number.
            "go[ \t]*to[ \t]+[0-9]+"
            "do\\([ \t]*\\(while\\|[0-9]+\\)\\)?"
            (:opt: "if" "call" "return" "stop" "continue" "then" "format"
                   "read" "write" "print" "open" "close" "rewind"))
     "\\>")
    ))
 nil 'case-insensitive)

(hilit-set-mode-patterns
 '(m2-mode modula-2-mode)
 '(("(\\*" "\\*)" comment)
   (hilit-string-find ?\\ string)
   ("^[ \t]*PROCEDURE[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(RECORD\\|ARRAY\\|OF\\|POINTER\\|TO\\|BEGIN\\|END\\|FOR\\|IF\\|THEN\\|ELSE\\|ELSIF\\|CASE\\|WHILE\\|DO\\|MODULE\\|FROM\\|RETURN\\|IMPORT\\|EXPORT\\|VAR\\|LOOP\\|UNTIL\\|\\DEFINITION\\|IMPLEMENTATION\\|AND\\|OR\\|NOT\\|CONST\\|TYPE\\|QUALIFIED\\)\\>" nil keyword)
   )
  nil 'case-insensitive)

(hilit-set-mode-patterns 'prolog-mode
 '(("/\\*" "\\*/" comment)
   ("%.*$" nil comment)
   (":-" nil defun)
   ("!" nil label)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("\\b\\(is\\|mod\\)\\b" nil keyword)
   ("\\(->\\|-->\\|;\\|==\\|\\\\==\\|=<\\|>=\\|<\\|>\\|=\\|\\\\=\\|=:=\\|=\\\.\\\.\\|\\\\\\\+\\)" nil decl)
   ("\\(\\\[\\||\\|\\\]\\)" nil include)))

;;}}}

;;{{{ PATTERNS: latex, bibtex

(hilit-set-mode-patterns
 '(
   LaTeX-mode japanese-LaTeX-mode SliTeX-mode
   japanese-SliTeX-mode FoilTeX-mode latex-mode
   )
 '(
   ;; comments
   ("[^\\]%.*$" nil comment)

   ;; the following two match \foo[xx]{xx} or \foo*{xx} or \foo{xx}
   ("\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" "}"
    keyword)
   ("\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" "}" keyword)
   ("\\\\footnote\\(mark\\|text\\)?{" "}" keyword)
   ("\\\\[a-z]+box" nil keyword)
   ("\\\\\\(v\\|h\\)space\\(\*\\)?{" "}" keyword)

   ;; (re-)define new commands/environments/counters
   ("\\\\\\(re\\)?new\\(environment\\|command\\){" "}" defun)
   ("\\\\new\\(length\\|theorem\\|counter\\){" "}" defun)

   ;; various declarations/definitions
   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
   ("\\\\\\(\\|title\\|author\\|date\\|thanks\\){" "}" define)

   ("\\\\documentstyle\\(\\[.*\\]\\)?{" "}" decl)
   ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" "}" decl)
   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
    decl)
   ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
    nil decl)
   ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" nil decl)

   ;; label-like things
   ("\\\\item\\(\\[[^]]*\\]\\)?" nil label)
   ("\\\\caption\\(\\[[^]]*\\]\\)?{" "}" label)

   ;; formulas
   ("\\\\("  "\\\\)" formula)                   ; \( \)
   ("\\\\\\[" "\\\\\\]" formula)                ; \[ \]
   ;; '$...$' or '$$...$$'
   ("[^$\\]\\($\\($[^$]*[^\\]\\$\\|[^$]*[^\\]\\)\\$\\)" 1 formula)

   ;; things that bring in external files
   ("\\\\\\(include\\|input\\|bibliography\\){" "}" include)

   ;; "wysiwyg" emphasis -- these don't work with nested expressions
   ;; ("{\\\\\\(em\\|it\\|sl\\)" "}" italic)
   ;; ("{\\\\bf" "}" bold)

   ("``" "''" string)

   ;; things that do some sort of cross-reference
   ("\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|label\\|index\\|glossary\\)\\*?{" "}" crossref)
   ))

(hilit-set-mode-patterns
 'bibtex-mode
 '(;;(";.*$"			nil	comment)
   ("%.*$"			nil	comment)
   ("@[a-zA-Z]+"		nil	keyword)
   ("{[ \t]*[-a-z:_A-Z0-9]+,"	nil	label) ; is wrong sometimes
   ("[ \t]*[a-zA-Z]+[ \t]*="	nil	define)))

;;}}}

;;{{{ PATTERNS: plain-tex, texinfo, nroff

(hilit-set-mode-patterns
 '(plain-tex-mode plain-TeX-mode)
 '(("^%%.*$" nil comment)
   ("{\\\\em\\([^}]+\\)}" nil comment)
   ("\\\\\\w+" nil keyword)
   ("{\\\\bf[^}]+}" nil keyword)
   ("^[ \t\n]*\\\\def[\\\\@]\\w+" nil defun)
   ("\\\\\\(begin\\|end\\){[A-Za-z0-9*]+}" nil defun)
   ;; '$...$' or '$$...$$'
   ("[^$\\]\\($\\($[^$]*[^\\]\\$\\|[^$]*[^\\]\\)\\$\\)" 1 formula)
   ))

(hilit-set-mode-patterns
 'texinfo-mode
 '(("^\\(@c\\|@comment\\)\\>.*$" nil comment)
   ("@\\(emph\\|strong\\|b\\|i\\){[^}]+}" nil comment)
   ("[^\\]\\(\\$[^$]*\\$\\)" 1 string)
   ("@\\(file\\|kbd\\|key\\){[^}]+}" nil string)
   ("^\\*.*$" nil defun)
   ("@\\(if\\w+\\|format\\|item\\)\\b.*$" nil defun)
   ("@end +[A-Za-z0-9]+[ \t]*$" nil defun)
   ("@\\(samp\\|code\\|var\\){[^}]+}" nil defun)
   ("@\\w+\\({[^}]+}\\)?" nil keyword)
   ))

;; Reasonable extensions would include smarter parameter handling for such
;; things as the .IX and .I macros, which alternate the handling of following
;; arguments.

(hilit-set-mode-patterns
 'nroff-mode
 '(("^\\.[\\\][\\\"].*$" nil comment)
   ("^\\.so .*$" nil include)
   ("^\\.[ST]H.*$" nil defun)
;;   ("^[^\\.].*\"[^\\\"]*\\(\\\\\\(.\\)[^\\\"]*\\)*\"" nil string)
   ("\"" "[^\\]\"" string)
   ("^\\.[A-Za-z12\\\\].*$" nil define)
   ("\\([\\\][^ ]*\\)" nil keyword)
   ("^\\.[a-zA-Z].*$" nil keyword))
  nil 'case-insensitive)

;;}}}

;;{{{ PATTERNS: calendar, icon, & pascal

(hilit-set-mode-patterns
 'calendar-mode
 '(("[A-Z][a-z]+ [0-9]+" nil define)	; month and year
   ("S  M Tu  W Th  F  S" nil label)	; week days
   ("[0-9]+\\*" nil defun)		; holidays
   ("[0-9]+\\+" nil comment)		; diary days
   ))

(hilit-set-mode-patterns
 'pascal-mode
 '(("(\\*" "\\*)" comment)
   ("{" "}" comment)
   ;; Doesn't work when there are strings in comments....
   ;; ("'[^']*'" nil string)
   ("^#.*$" nil include)
   ("^[ \t]*\\(procedure\\|function\\)[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(program\\|begin\\|end\\)\\>" nil defun)
   ("\\<\\(external\\|forward\\)\\>" nil include)
   ("\\<\\(label\\|const\\|type\\|var\\)\\>" nil define)
   ("\\<\\(record\\|array\\|file\\)\\>" nil type)
   (:buildme:
    ("\\S_\\<" (keyword "of" "to" "for" "if" "then" "else" "case" "while" "do"
		     "until" "and" "or" "not" "with" "repeat") "\\>\\S_"))
   )
 nil 'case-insensitive)

(hilit-set-mode-patterns
 'icon-mode
 '(("#.*$" nil comment)
   ("\"[^\\\"]*\\(\\\\.[^\\\"]*\\)*\"" nil string)
   ;; charsets: these do not work because of a conflict with strings
   ;; ("'[^\\']*\\(\\\\.[^\\']*\\)*'" nil string)
   ("^[ \t]*procedure[ \t]+\\w+[ \t]*(" ")" defun)
   ("^[ \t]*record.*(" ")" include)
   ("^[ \t]*\\(global\\|link\\)[ \t\n]+[A-Za-z_0-9]+\\([ \t\n]*,[ \t\n]*[A-Za-z_0-9]+\\)*" nil include)
   ("^[ \t]*\\(local\\|static\\)[ \t\n]+[A-Za-z_0-9]+\\([ \t\n]*,[ \t\n]*[A-Za-z_0-9]+\\)*" nil decl)
   ("\\<\\(initial\\|end\\)\\>" nil glob-struct)
   ("\\<\\(while\\|until\\|return\\|every\\|if\\|then\\|else\\|to\\|case\\|of\\|suspend\\|create\\|do\\|repeat\\|break\\)\\>" nil keyword)
   ))

;; as you can see, I had two similar problems for Pascal and Icon. In
;; Pascal, strings are delimited with ' and an embedded quote is doubled,
;; thus string syntax would be extremely simple. However, if a string
;; occurs within a comment, the following text is considered a string.
;;
;; In Icon, strings are similar to C ones, but there are also charsets,
;; delimited with simple quotes. I could not manage to use both regexps at
;; the same time.

;; The problem I have with my patterns for Icon is that this language has a
;; string similar constant to the C one (but a string can be cut on several
;; lines, if terminated by a dash and continued with initial blanks, like
;; this:
;;         "This is a somewhat long -
;;          string, written on three -
;;          succesive lines"
;; in order to insert a double quote in a string, you have to escape it
;; with a \), bu also a character set constant (named a charset), which
;; uses single quotes instead of double ones. It would seem intuitive to
;; highlight both constants in the same way.

;;}}}

;;}}}

(provide 'hilit19)
(provide 'hl319)

;;; hl319 ends here.

;; Local Variables:
;; byte-optimize: t
;; hilit-auto-highlight: nil
;; hilit-auto-rehighlight: defun
;; End:


