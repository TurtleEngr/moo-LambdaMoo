Manifest:
.emacs-mudstuff    -- stuff for .emacs
coldc-mode.elc     -- analog of moo-code.el, but for coldc
hl319.el           -- hili't'ing
hl319.elc          -- and compiled
moo-code.el        -- helper code for editing verbs
mud-telnet-wsk.el  -- main MOO client
mud                -- add your servers and player, then put this in ~/.mud

stick .emacs_mudstuff into your .emacs
stick .el/.elc into your load-path (.../lisp/)

Really messy face setup in those lines... probably want to at least
set your name highlighting and buddy and page highlighting.  I put
some help text in there above where you'd set up the faces.

Barely caught the require's for moo-code.el and coldc-mode.elc.
(Which means I probably have more bugs in this extraction :) :) )

I seem to have misplaced coldc-mode.el.   Just comment out the
require line at the end of mud-telnet-wsk.el if it is problem.

Byte compile the files, especially hl319.el. Meh, I'll just stick
that in.

I get adequate performance with an un-byte-compiled mud-telnet-wsk.el
and moo-code.el. Wouldn't hurt anything.
