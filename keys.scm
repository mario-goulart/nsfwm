;; Mappings between key names (symbols) and xlib constants (numbers).

(define (mod->xkey mod)
  (case mod
    ((Alt) MOD1MASK)
    ((Control) CONTROLMASK)
    ((Shift) SHIFTMASK)
    ((Super) MOD4MASK)
    (else (error 'mod->xkey "Invalid modifier" mod))))

(define (key->xkey key)
  ;; Return either key<number> or a pair (modifier<symbol> . key<number>).
  ;; In case of the latter, modifier<symbol> can be decoded to
  ;; <number> by mod->xkey.
  (case key
    ((0) XK_0)
    ((1) XK_1)
    ((2) XK_2)
    ((3) XK_3)
    ((4) XK_4)
    ((5) XK_5)
    ((6) XK_6)
    ((7) XK_7)
    ((8) XK_8)
    ((9) XK_9)
    ((A) (cons 'Shift XK_LCA))
    ;; XK_AACUTE
    ;; XK_ACIRCUMFLEX
    ;; XK_ACUTE
    ;; XK_ADIAERESIS
    ;; XK_AE
    ;; XK_AGRAVE
    ((Alt-left) XK_ALT_L)
    ((Alt-right) XK_ALT_R)
    ((&) XK_AMPERSAND)
    ;; XK_ARING
    ;; XK_ASCIICIRCUM
    ((~) XK_ASCIITILDE)
    ((*) XK_ASTERISK)
    ;; XK_AT
    ;; XK_ATILDE
    ((B) (cons 'Shift XK_LCB))
    ;; XK_BACKSLASH
    ((Backspace) XK_BACKSPACE)
    ;; XK_BAR
    ;; XK_BEGIN
    ;; XK_BRACELEFT
    ;; XK_BRACERIGHT
    ;; XK_BRACKETLEFT
    ;; XK_BRACKETRIGHT
    ;; XK_BREAK
    ;; XK_BROKENBAR
    ((C) (cons 'Shift XK_LCC))
    ;; XK_CANCEL
    ;; XK_CAPS_LOCK
    ;; XK_CCEDILLA
    ;; XK_CEDILLA
    ;; XK_CENT
    ;; XK_CLEAR
    ((:) XK_COLON)
    ((|,|) XK_COMMA)
    ((Control-left) XK_CONTROL_L)
    ((Control-right) XK_CONTROL_R)
    ;; XK_COPYRIGHT
    ;; XK_CURRENCY
    ((D) (cons 'Shift XK_LCD))
    ;; XK_DEGREE
    ((Delete) XK_DELETE)
    ;; XK_DIAERESIS
    ((/) XK_DIVISION)
    ;; XK_DOLLAR
    ((Down) XK_DOWN)
    ((E) (cons 'Shift XK_LCE))
    ;; XK_EACUTE
    ;; XK_ECIRCUMFLEX
    ;; XK_EDIAERESIS
    ;; XK_EGRAVE
    ((End) XK_END)
    ((=) XK_EQUAL)
    ((Escape) XK_ESCAPE)
    ;; XK_ETH
    ((!) XK_EXCLAM)
    ;; XK_EXCLAMDOWN
    ;; XK_EXECUTE
    ((F) (cons 'Shift XK_LCF))
    ((F1) XK_F1)
    ((F10) XK_F10)
    ((F11) XK_F11)
    ((F12) XK_F12)
    ;; XK_F13
    ;; XK_F14
    ;; XK_F15
    ;; XK_F16
    ;; XK_F17
    ;; XK_F18
    ;; XK_F19
    ((F2) XK_F2)
    ;; XK_F20
    ;; XK_F21
    ;; XK_F22
    ;; XK_F23
    ;; XK_F24
    ;; XK_F25
    ;; XK_F26
    ;; XK_F27
    ;; XK_F28
    ;; XK_F29
    ((F3) XK_F3)
    ;; XK_F30
    ;; XK_F31
    ;; XK_F32
    ;; XK_F33
    ;; XK_F34
    ;; XK_F35
    ((F4) XK_F4)
    ((F5) XK_F5)
    ((F6) XK_F6)
    ((F7) XK_F7)
    ((F8) XK_F8)
    ((F9) XK_F9)
    ;; XK_FIND
    ((G) (cons 'Shift XK_LCG))
    ((>) XK_GREATER)
    ;; XK_GUILLEMOTLEFT
    ;; XK_GUILLEMOTRIGHT
    ((H) (cons 'Shift XK_LCH))
    ;; XK_HELP
    ((Home) XK_HOME)
    ((Hyper-left) XK_HYPER_L)
    ((Hyper-right) XK_HYPER_R)
    ((-) XK_HYPHEN)
    ((I) (cons 'Shift XK_LCI))
    ;; XK_IACUTE
    ;; XK_ICIRCUMFLEX
    ;; XK_IDIAERESIS
    ;; XK_IGRAVE
    ((Insert) XK_INSERT)
    ((J) (cons 'Shift XK_LCJ))
    ((K) (cons 'Shift XK_LCK))
    ;; XK_KANJI
    ((KP-0) XK_KP_0)
    ((KP-1) XK_KP_1)
    ((KP-2) XK_KP_2)
    ((KP-3) XK_KP_3)
    ((KP-4) XK_KP_4)
    ((KP-5) XK_KP_5)
    ((KP-6) XK_KP_6)
    ((KP-7) XK_KP_7)
    ((KP-8) XK_KP_8)
    ((KP-9) XK_KP_9)
    ((KP-+) XK_KP_ADD)
    ;; XK_KP_DECIMAL
    ((KP-/) XK_KP_DIVIDE)
    ((KP-enter) XK_KP_ENTER)
    ((KP-=) XK_KP_EQUAL)
    ((KP-F1) XK_KP_F1)
    ((KP-F2) XK_KP_F2)
    ((KP-F3) XK_KP_F3)
    ((KP-F4) XK_KP_F4)
    ((KP-*) XK_KP_MULTIPLY)
    ;; XK_KP_SEPARATOR
    ((KP-space) XK_KP_SPACE)
    ((KP--) XK_KP_SUBTRACT)
    ((KP-tab) XK_KP_TAB)
    ((L) (cons 'Shift XK_LCL))
    ;; XK_L1
    ;; XK_L10
    ;; XK_L2
    ;; XK_L3
    ;; XK_L4
    ;; XK_L5
    ;; XK_L6
    ;; XK_L7
    ;; XK_L8
    ;; XK_L9
    ((a) XK_LCA)
    ;; XK_LCAACUTE
    ;; XK_LCACIRCUMFLEX
    ;; XK_LCADIAERESIS
    ;; XK_LCAE
    ;; XK_LCAGRAVE
    ;; XK_LCARING
    ;; XK_LCATILDE
    ((b) XK_LCB)
    ((c) XK_LCC)
    ;; XK_LCCCEDILLA
    ((d) XK_LCD)
    ((e) XK_LCE)
    ;; XK_LCEACUTE
    ;; XK_LCECIRCUMFLEX
    ;; XK_LCEDIAERESIS
    ;; XK_LCEGRAVE
    ;; XK_LCETH
    ((f) XK_LCF)
    ((g) XK_LCG)
    ((h) XK_LCH)
    ((i) XK_LCI)
    ;; XK_LCIACUTE
    ;; XK_LCICIRCUMFLEX
    ;; XK_LCIDIAERESIS
    ;; XK_LCIGRAVE
    ((j) XK_LCJ)
    ((k) XK_LCK)
    ((l) XK_LCL)
    ((m) XK_LCM)
    ((n) XK_LCN)
    ;; XK_LCNTILDE
    ((o) XK_LCO)
    ;; XK_LCOACUTE
    ;; XK_LCOCIRCUMFLEX
    ;; XK_LCODIAERESIS
    ;; XK_LCOGRAVE
    ;; XK_LCOTILDE
    ((p) XK_LCP)
    ((q) XK_LCQ)
    ((r) XK_LCR)
    ((s) XK_LCS)
    ((t) XK_LCT)
    ;; XK_LCTHORN
    ((u) XK_LCU)
    ;; XK_LCUACUTE
    ;; XK_LCUCIRCUMFLEX
    ;; XK_LCUDIAERESIS
    ;; XK_LCUGRAVE
    ((v) XK_LCV)
    ((w) XK_LCW)
    ((x) XK_LCX)
    ((y) XK_LCY)
    ;; XK_LCYACUTE
    ((z) XK_LCZ)
    ((Left) XK_LEFT)
    ((<) XK_LESS)
    ;; XK_LINEFEED
    ((M) (cons 'Shift XK_LCM))
    ;; XK_MACRON
    ;; XK_MASCULINE
    ((Menu) XK_MENU)
    ((Meta-left) XK_META_L)
    ((Meta-right) XK_META_R)
    ((-) XK_MINUS)
    ;; XK_MODE_SWITCH
    ;; XK_MU
    ((*) XK_MULTIPLY) ;; KP_ASTERISC?
    ;; XK_MULTI_KEY
    ((N) (cons 'Shift XK_LCN))
    ((Next) XK_NEXT)
    ;; XK_NOBREAKSPACE
    ;; XK_NOTSIGN
    ;; XK_NTILDE
    ;; XK_NUMBERSIGN
    ((Numlock) XK_NUM_LOCK)
    ((O) (cons 'Shift XK_LCO))
    ;; XK_OACUTE
    ;; XK_OCIRCUMFLEX
    ;; XK_ODIAERESIS
    ;; XK_OGRAVE
    ;; XK_ONEHALF
    ;; XK_ONEQUARTER
    ;; XK_ONESUPERIOR
    ;; XK_OOBLIQUE
    ;; XK_ORDFEMININE
    ;; XK_OSLASH
    ;; XK_OTILDE
    ((P) (cons 'Shift XK_LCP))
    ;; XK_PARAGRAPH
    ((|)|) XK_PARENLEFT)
    ((|(|) XK_PARENRIGHT)
    ;; XK_PAUSE
    ((%) XK_PERCENT)
    ((|.|) XK_PERIOD)
    ;; XK_PERIODCENTERED
    ((+) XK_PLUS)
    ((-) XK_PLUSMINUS)
    ((Print) XK_PRINT)
    ((Prior) XK_PRIOR)
    ((Q) (cons 'Shift XK_LCQ))
    ((?) XK_QUESTION)
    ;; XK_QUESTIONDOWN
    ;; XK_QUOTEDBL
    ;; XK_QUOTELEFT
    ;; XK_QUOTERIGHT
    ((R) (cons 'Shift XK_LCR))
    ;; XK_R1
    ;; XK_R10
    ;; XK_R11
    ;; XK_R12
    ;; XK_R13
    ;; XK_R14
    ;; XK_R15
    ;; XK_R2
    ;; XK_R3
    ;; XK_R4
    ;; XK_R5
    ;; XK_R6
    ;; XK_R7
    ;; XK_R8
    ;; XK_R9
    ;; XK_REDO
    ;; XK_REGISTERED
    ((Return) XK_RETURN)
    ((Right) XK_RIGHT)
    ((S) (cons 'Shift XK_LCS))
    ;; XK_SCRIPT_SWITCH
    ;; XK_SECTION
    ;; XK_SELECT
    ((|;|) XK_SEMICOLON)
    ((Shift-left) XK_SHIFT_L)
    ;; XK_SHIFT_LOCK
    ((Shift-right) XK_SHIFT_R)
    ((/) XK_SLASH)
    ((Space) XK_SPACE)
    ((|#|) XK_SSHARP)
    ;; XK_STERLING
    ((Super-left) XK_SUPER_L)
    ((Super-right) XK_SUPER_R)
    ((T) (cons 'Shift XK_LCT))
    ((Tab) XK_TAB)
    ;; XK_THORN
    ;; XK_THREEQUARTERS
    ;; XK_THREESUPERIOR
    ;; XK_TWOSUPERIOR
    ((U) (cons 'Shift XK_LCU))
    ;; XK_UACUTE
    ;; XK_UCIRCUMFLEX
    ;; XK_UDIAERESIS
    ;; XK_UGRAVE
    ((_) XK_UNDERSCORE)
    ;; XK_UNDO
    ((Up) XK_UP)
    ((V) (cons 'Shift XK_LCV))
    ((W) (cons 'Shift XK_LCW))
    ((X) (cons 'Shift XK_LCX))
    ((Y) (cons 'Shift XK_LCY))
    ;; XK_YACUTE
    ;; XK_YDIAERESIS
    ;; XK_YEN
    ((Z) (cons 'Shift XK_LCZ))
    ;; DXK_ACUTE_ACCENT
    ;; DXK_CEDILLA_ACCENT
    ;; DXK_CIRCUMFLEX_ACCENT
    ;; DXK_DIAERESIS
    ;; DXK_GRAVE_ACCENT
    ;; DXK_REMOVE
    ;; DXK_RING_ACCENT
    ;; DXK_TILDE
    (else (error 'key->xkey "Invalid key" key))))
