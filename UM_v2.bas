'Universal Menu v2

DIM SHARED restart
GOTO restart

errorhandler:
COLOR colour&("fg"), colour&("bg")
IF ERR <> 5 AND ERR <> 6 AND ERR <> 7 AND ERR <> 9 AND ERR <> 14 AND ERR <> 17 AND ERR <> 19 AND ERR <> 51 AND ERR <> 70 AND ERR <> 71 AND ERR <> 72 AND ERR <> 75 THEN
    PRINT "[ Error"; ERR; "in line"; _ERRORLINE; ", will be ignored. ]"
    _DELAY 0.2
    RESUME NEXT
ELSEIF ERR = 61 THEN
    PRINT "[ Error"; ERR; "in line"; _ERRORLINE; ", closing program. ]"
    PRINT "[ The drive is full. Please delete data. ]"
    RESUME NEXT
ELSE
    PRINT "[ Error"; ERR; "in line"; _ERRORLINE; ", closing program. ]"
    PRINT "[ Please report this error to your IT department. ]"
    SLEEP
    SYSTEM
END IF

restart:
restart = 0
CLS
CLOSE
CLEAR

DIM SHARED datapath$
datapath$ = "test\"
DIM SHARED settingspath$
settingspath$ = _DIR$("local application data") + "\Universal Menu\"

DIM SHARED swidth 'screenbreite / desktopbreite
DIM SHARED sheight 'screenhohe / desktophohe
swidth = _DESKTOPWIDTH
sheight = _DESKTOPHEIGHT

'einstellungs-variablen
DIM SHARED darkmode
DIM SHARED bigwindow
DIM SHARED rfontheight

PRINT "Lade Einstellungen..."
IF _DIREXISTS(settingspath$) = 0 THEN MKDIR settingspath$
IF _FILEEXISTS(settingspath$ + "settings.dat") THEN
    OPEN settingspath$ + "settings.dat" FOR INPUT AS #1
    INPUT #1, darkmode$
    darkmode = VAL(_INFLATE$(darkmode$))
    INPUT #1, bigwindow$
    bigwindow = VAL(_INFLATE$(bigwindow$))
    INPUT #1, scale$
    rfontheight = VAL(_INFLATE$(scale$))
    CLOSE #1
ELSE
    OPEN settingspath$ + "settings.dat" FOR OUTPUT AS #1
    WRITE #1, _DEFLATE$("1"): WRITE #1, _DEFLATE$("0"): WRITE #1, _DEFLATE$("16")
    CLOSE #1
    darkmode = 1
    bigwindow = 0
    rfontheight = 16
END IF

DIM SHARED canvas&
DIM SHARED maxx 'fensterbreite
DIM SHARED maxy 'fensterhohe
IF bigwindow = 0 THEN
    SCREEN _NEWIMAGE(_DESKTOPWIDTH / 1.5, _DESKTOPHEIGHT / 2.2, 32)
    canvas& = _NEWIMAGE(_DESKTOPWIDTH / 1.5, _DESKTOPHEIGHT / 2.2, 32)
    maxx = _DESKTOPWIDTH / 1.5
    maxy = _DESKTOPHEIGHT / 2.2
    DO: LOOP UNTIL _SCREENEXISTS
    _SCREENMOVE (_DESKTOPWIDTH / 2) - (_DESKTOPWIDTH / 1.5 / 2), (_DESKTOPHEIGHT / 2) - (_DESKTOPHEIGHT / 2. / 2)
ELSE
    SCREEN _NEWIMAGE(_DESKTOPWIDTH, _DESKTOPHEIGHT, 32)
    canvas& = _NEWIMAGE(_DESKTOPWIDTH, _DESKTOPHEIGHT, 32)
    maxx = swidth
    maxy = sheight
    DO: LOOP UNTIL _SCREENEXISTS
    _SCREENMOVE 0, 0
END IF
DIM SHARED framerate
framerate = 60
_TITLE "Universal Menu"

DIM SHARED r&
DIM SHARED h&
DIM SHARED b&
DIM SHARED fontheight
DIM SHARED fontwidth
fontpath$ = datapath$ + "fonts\"
fontfileh$ = fontpath$ + "akira_expanded.otf" 'font-datei fur uberschrift
fontheight = 32
h& = _LOADFONT(fontfileh$, fontheight)
fontfiler$ = fontpath$ + "consola.ttf" 'font-datei fur regular font
fontheight = rfontheight
r& = _LOADFONT(fontfiler$, fontheight, "MONOSPACE")
fontfileb$ = fontpath$ + "consolab.ttf" 'font-datei fur info feld rechts
b& = _LOADFONT(fontfileb$, fontheight, "MONOSPACE")
IF r& <> 0 THEN _FONT r&
fontwidth = _FONTWIDTH(r&)
DIM SHARED maxrows 'maximale anzahl an spalten
DIM SHARED maxlines 'maximale anzahl an zeilen
IF bigwindow = 0 THEN
    maxrows = INT((swidth / 1.5) / fontwidth)
    maxlines = INT((sheight / 2.2) / fontheight) - 4
ELSE
    maxrows = INT(swidth / fontwidth)
    maxlines = INT(sheight / fontheight) - 4
END IF
DIM SHARED firstline 'grund-spalte
DIM SHARED firstchar 'grund-zeile
firstline = INT((75 + fontheight) / fontheight)
firstchar = 6

DIM SHARED inputcount 'amount of input elements
DIM SHARED endparameter$ 'used to know which element caused the menu to end, use to redirect elsewhere or do something
DIM SHARED menulimit
menulimit = 50 '                                 maximum amount of menu items
DIM SHARED interactable '                        =1 if the menu contains an interactable item, otherwise =0
DIM SHARED allsel(menulimit) '                   if written text in input element is selected (ctrl + a)
DIM SHARED mtype$(menulimit) '                    type of menu item (toggle, text, input, menitem, button, selector, date, time, slider)
DIM SHARED text$(menulimit) '                    unchangeable text of menu item
DIM SHARED yoffset(menulimit) '                  row to place menu item m on, pixel = yoffset(m) / fontheight
DIM SHARED xoffset(menulimit) '                  column to place menu item m on, pixel = xoffset(m) / fontwidth
DIM SHARED basex(menulimit) '                    left border of menu item
DIM SHARED basey(menulimit) '                    upper border of menu item
DIM SHARED endx(menulimit) '                     right border of menu item
DIM SHARED endy(menulimit) '                     lower border of menu item
DIM SHARED destination$(menulimit) '             ziel von m bei interaktion                        /
DIM SHARED setting$(menulimit) '                 zu verandernde einstellung bei interaktion          /
DIM SHARED firstprint(menulimit) '               wenn 1: wurde mindestens einmal auf den bildschirm geschrieben, wenn 0: wird m bei nachster gelegenheit neu anzeigen
DIM SHARED state(menulimit) '                    status von m, vor allem relevant fur toggle / function
DIM SHARED selected(menulimit) '                 speichert die aktuell ausgewahlte instanz von selector m
DIM SHARED arraydata$(menulimit, 10000)
DIM SHARED maxad(menulimit)
DIM SHARED array(menulimit)
DIM SHARED minute(menulimit)
DIM SHARED hour(menulimit)
DIM SHARED day(menulimit)
DIM SHARED month(menulimit)
DIM SHARED year(menulimit)
DIM SHARED change(0 TO menulimit)
DIM SHARED changevalxl(menulimit)
DIM SHARED changevalxr(menulimit)
DIM SHARED increasevalyu(menulimit)
DIM SHARED increasevalyb(menulimit)
DIM SHARED decreasevalyu(menulimit)
DIM SHARED decreasevalyb(menulimit)
DIM SHARED melementxl(menulimit, 10000)
DIM SHARED melementxr(menulimit, 10000)
DIM SHARED number(menulimit)
DIM SHARED m 'aktuelles menuitem
DIM SHARED maxm
DIM SHARED clr&(menulimit)
DIM SHARED weight$(menulimit)
'slider
DIM SHARED maxval(menulimit) '                   maximum value of slider element
DIM SHARED minval(menulimit) '                   minimum value of slider element
DIM SHARED value(menulimit) '                    current value of slider element
'triangles :D
DIM SHARED mtriscale
mtriscale = 0.8
DIM SHARED p1x(menulimit)
DIM SHARED p1y(menulimit)
DIM SHARED p2x(menulimit)
DIM SHARED p2y(menulimit)
DIM SHARED p3x(menulimit)
DIM SHARED p3y(menulimit)
'start der input-variablen
DIM SHARED alch '                               maximum amount of accepted characters for input
DIM SHARED char$(menulimit, 500) '              saves character array for input
DIM SHARED g(menulimit) 'aktuelle anzahl an zeichen des bearbeiteten textes
DIM SHARED gbf(menulimit) 'g vom vorigen loop-durchlauf
DIM SHARED ac$(500) 'akzeptierte input-zeichen
DIM SHARED UserInput$(menulimit) 'speichert den input von m
'Background
DIM SHARED mtitle$ 'uberschrift fur alles
'copy/paste
DIM SHARED maxcliparray
DIM SHARED cliparray$(menulimit)
'tempsave
DIM SHARED maxtemparray(0 TO 12)
DIM SHARED temparray$(12, menulimit)
'status
statuslimit = 20
DIM SHARED sttext$(statuslimit)
DIM SHARED color$(statuslimit)
DIM SHARED st
DIM SHARED maxst
DIM SHARED shading
shading = 1
DIM SHARED UMround 'used for rounded shapes
UMround = 4
DIM SHARED detail
detail = 2

'------------------------------------------------------------------------------------------------------------------------------------------------- your program code here

loadall

NewInput 1, 0, "test: ", "yeah", 0
RunMenu 1, 0, "Yeah"
SLEEP

'-------------------------------------------------------------------------------------------------------------------------------------------------

SUB loadall
    IF _FILEEXISTS(datapath$ + "AC.tcc") = 0 THEN
        OPEN datapath$ + "AC.tcc" FOR OUTPUT AS #1
        ac = 31: DO: ac = ac + 1
            WRITE #1, _DEFLATE$(CHR$(ac))
        LOOP UNTIL ac = 122
        CLOSE #1
    END IF
    OPEN datapath$ + "AC.tcc" FOR INPUT AS #1
    ac = 0: DO: ac = ac + 1
        INPUT #1, ac$(ac)
        ac$(ac) = _INFLATE$(ac$(ac))
    LOOP UNTIL EOF(1) = -1
    alch = ac: CLOSE #1
END SUB

SUB EmptyMenu
    maxm = 0: m = 0: interactable = 0
END SUB

FUNCTION colour& (color$)
    SELECT CASE color$
        CASE "bg"
            IF darkmode = 1 THEN colour& = colour&("black") ELSE colour& = colour&("white")
        CASE "fg"
            IF darkmode = 1 THEN colour& = colour&("white") ELSE colour& = colour&("black")
        CASE "offfocus"
            IF darkmode = 1 THEN colour& = colour&("light grey") ELSE colour& = colour&("dark grey")
        CASE "white"
            colour& = _RGBA(220, 220, 220, 255)
        CASE "black"
            colour& = _RGBA(15, 15, 15, 255)
        CASE "red"
            colour& = _RGBA(255, 30, 30, 255)
        CASE "yellow"
            colour& = _RGBA(249, 194, 0, 255)
        CASE "green"
            colour& = _RGBA(94, 233, 61, 255)
        CASE "dark grey"
            colour& = _RGBA(50, 50, 50, 255)
        CASE "light grey"
            colour& = _RGBA(170, 170, 170, 255)
        CASE "transparent"
            colour& = _RGBA(0, 0, 0, 0)
    END SELECT
END SUB

SUB RunMenu (selectedm, layout, mtitle$)
    _DEST mcanvas&
    IF m > 0 THEN
        endmenu = 0
        maxm = m
        firstprint = 1
        Background layout, mtitle$, 1
        IF maxtemparray(listID) > 0 AND listID <> 0 THEN
            m2 = 0: DO: m2 = m2 + 1
                SELECT CASE mtype$(m2)
                    CASE "input"
                        IF LEN(temparray$(listID, m2)) > 0 THEN
                            p = 0: DO: p = p + 1
                                char$(m2, p) = MID$(temparray$(listID, m2), p, 1)
                            LOOP UNTIL p = LEN(temparray$(listID, m2))
                            g(m2) = p
                            gbf(m2) = p
                        END IF
                    CASE "selector"
                        ad = 0: DO: ad = ad + 1
                            IF LEN(temparray$(listID, m2)) < LEN(arraydata$(array(m2), ad)) THEN
                                p = 0: DO: p = p + 1
                                    IF MID$(arraydata$(array(m2), ad), p, LEN(temparray$(listID, m2))) = temparray$(listID, m2) THEN
                                        selected(m2) = ad: ad = maxad(array(m2))
                                    END IF
                                LOOP UNTIL p >= LEN(arraydata$(array(m2), ad)) - LEN(temparray$(listID, m2)) + 1
                            ELSEIF temparray$(listID, m2) = arraydata$(array(m2), ad) THEN
                                selected(m2) = ad: ad = maxad(array(m2)): change(m2) = 1
                            END IF
                        LOOP UNTIL ad = maxad(array(m2))
                    CASE "date"
                        IF LEN(temparray$(listID, m2)) > 10 THEN
                            year(m2) = VAL(MID$(temparray$(listID, m2), 1, 4))
                            month(m2) = VAL(MID$(temparray$(listID, m2), 6, 2))
                            day(m2) = VAL(MID$(temparray$(listID, m2), 9, 2))
                        END IF
                    CASE "time"
                        IF LEN(temparray$(listID, m2)) >= 5 THEN
                            hour(m2) = VAL(MID$(temparray$(listID, m2), 1, 2))
                            minute(m2) = VAL(MID$(temparray$(listID, m2), 4, 2))
                        END IF
                END SELECT
            LOOP UNTIL m2 = maxm OR m2 = maxtemparray(listID)
            maxtemparray(listID) = 0
            clearStatus
            newStatus "Daten aus vorheriger Sitzung eingef" + CHR$(129) + "gt.", "green"
        END IF
        DO
            m = 0
            DO
                IF fps = 0 THEN
                    framestart = TIMER
                END IF
                m = m + 1
                prevm = selectedm

                'file drop check
                IF _TOTALDROPPEDFILES = 1 THEN
                    df$ = _DROPPEDFILE
                    IF _FILEEXISTS(df$) THEN
                        dropmtype$ = "file"
                    ELSE
                        IF _DIREXISTS(a$) THEN
                            dropmtype$ = "folder"
                        ELSE
                            dropmtype$ = "empty"
                        END IF
                    END IF
                    _FINISHDROP
                    SELECT CASE dropmtype$
                        CASE "file"
                            p = 0: DO: p = p + 1
                                IF MID$(df$, p, 1) = "\" THEN
                                    u = p
                                END IF
                            LOOP UNTIL p = LEN(df$)
                            IF MID$(df$, LEN(df$) - 6, 7) = ".bremer" THEN
                                endparameter$ = MID$(df$, u + 1, LEN(df$) - 7 - u)
                                endmenu = 1
                            END IF
                            IF MID$(df$, LEN(df$) - 5, 6) = ".event" THEN
                                'fill temparray with data from file, set the listID to the proper number
                                endmenu = 1
                            END IF
                    END SELECT
                END IF

                'Mouse selection
                IF _MOUSEINPUT = -1 AND mousetrigger = 0 THEN
                    IF _MOUSEX > changevalxl(selectedm) AND _MOUSEX < changevalxr(selectedm) THEN
                        IF _MOUSEY > increasevalyu(selectedm) AND _MOUSEY < increasevalyb(selectedm) AND _MOUSEBUTTON(1) = -1 THEN
                            mousetrigger = 1
                            GOTO increase
                        ELSEIF _MOUSEY > decreasevalyu(selectedm) AND _MOUSEY < decreasevalyb(selectedm) AND _MOUSEBUTTON(1) = -1 THEN
                            mousetrigger = 1
                            GOTO decrease
                        END IF
                    END IF

                    IF _MOUSEX > (firstchsar + xoffset(m) - 1) * fontwidth - (fontwidth / 2) AND _MOUSEX < endx(m) THEN
                        IF _MOUSEY > basey(m) AND _MOUSEY < endy(m) THEN
                            change(selectedm) = 1: selectedm = m: change(selectedm) = 1
                            IF maxad(m) > 0 THEN
                                e = 0: DO
                                    e = e + 1
                                    IF _MOUSEX > melementxl(m, e) AND _MOUSEX < melementxr(m, e) THEN
                                        selected(m) = e
                                        change(m) = 1
                                    END IF
                                LOOP UNTIL e = maxad(m)
                            END IF
                            IF _MOUSEBUTTON(1) = -1 THEN
                                GOTO engageonm
                            ELSE
                                m = selectedm
                                GOTO commitchange
                            END IF
                        END IF
                    END IF

                    mousewheel = _MOUSEWHEEL
                    IF mousewheel <> 0 THEN
                        IF _MOUSEX > melementxl(selectedm, selected(selectedm)) AND _MOUSEX < melementxr(selectedm, selected(selectedm)) THEN
                            IF mousewheel = -1 THEN
                                GOTO increase
                            ELSE
                                GOTO decrease
                            END IF
                        END IF
                    END IF
                ELSEIF _MOUSEBUTTON(1) = 0 THEN
                    mousetrigger = 0
                END IF

                'Key selection
                IF _KEYDOWN(100306) = -1 OR _KEYDOWN(100305) = -1 THEN 'ctrl
                    IF mtype$(selectedm) = "input" THEN showpassword = 1: change(selectedm) = 1
                    hitk = _KEYHIT
                    IF hitk = 13 THEN 'ctrl + enter
                        IF mtype$(selectedm) = "selector" THEN
                            endparameter$ = destination$(selectedm): endmenu = 1
                        END IF
                    END IF
                    IF hitk = 97 OR hitk = 65 THEN 'ctrl + a
                        IF mtype$(selectedm) = "input" THEN
                            DO: LOOP UNTIL _KEYDOWN(hitk) = 0 'prevents multiple triggers
                            IF allsel(selectedm) = 1 THEN allsel(selectedm) = 0 ELSE allsel(selectedm) = 1
                        END IF
                    END IF
                    IF hitk = 99 OR hitk = 67 THEN 'ctrl + c
                        maxcliparray = 0
                        SELECT CASE mtype$(selectedm)
                            CASE "text"
                                _CLIPBOARD$ = cutcontent$(text$(m2))
                            CASE "input"
                                UIn$ = ""
                                IF g(selectedm) > 0 THEN
                                    g = 0: DO: g = g + 1
                                        UIn$ = UIn$ + char$(selectedm, g)
                                    LOOP UNTIL g = g(selectedm)
                                END IF
                                _CLIPBOARD$ = UIn$
                        END SELECT
                        clearStatus
                        newStatus "Element Kopiert.", "green"
                    END IF
                    IF hitk = 118 OR hitk = 86 THEN 'ctrl + v
                        paste:
                        IF maxcliparray > 0 THEN
                            m2 = 0: DO: m2 = m2 + 1
                                SELECT CASE mtype$(m2)
                                    CASE "input"
                                        IF LEN(cliparray$(m2)) > 0 THEN
                                            p = 0: DO: p = p + 1
                                                char$(m2, p) = MID$(cliparray$(m2), p, 1)
                                            LOOP UNTIL p = LEN(cliparray$(m2))
                                            g(m2) = p
                                            gbf(m2) = p
                                        END IF
                                    CASE "selector"
                                        ad = 0: DO: ad = ad + 1
                                            IF LEN(cliparray$(m2)) < LEN(arraydata$(array(m2), ad)) THEN
                                                p = 0: DO: p = p + 1
                                                    IF MID$(arraydata$(array(m2), ad), p, LEN(cliparray$(m2))) = cliparray$(m2) THEN
                                                        selected(m2) = ad: ad = maxad(array(m2))
                                                    END IF
                                                LOOP UNTIL p >= LEN(arraydata$(array(m2), ad)) - LEN(cliparray$(m2)) + 1
                                            ELSEIF cliparray$(m2) = arraydata$(array(m2), ad) THEN
                                                selected(m2) = ad: ad = maxad(array(m2)): change(m2) = 1
                                            END IF
                                        LOOP UNTIL ad = maxad(array(m2))
                                    CASE "date"
                                        IF LEN(cliparray$(m2)) > 10 THEN
                                            year(m2) = VAL(MID$(cliparray$(m2), 1, 4))
                                            month(m2) = VAL(MID$(cliparray$(m2), 6, 2))
                                            day(m2) = VAL(MID$(cliparray$(m2), 9, 2))
                                        END IF
                                    CASE "time"
                                        IF LEN(cliparray$(m2)) >= 5 THEN
                                            hour(m2) = VAL(MID$(cliparray$(m2), 1, 2))
                                            minute(m2) = VAL(MID$(cliparray$(m2), 4, 2))
                                        END IF
                                END SELECT
                                change(m2) = 1
                            LOOP UNTIL m2 = maxm OR m2 = maxcliparray
                            maxcliparray = 0
                            clearStatus
                            newStatus "Eingef" + CHR$(129) + "gt.", "green"
                        END IF
                    END IF
                ELSE
                    IF mtype$(selectedm) = "input" THEN
                        showpassword = 0: change(selectedm) = 1
                    END IF
                END IF
                IF _KEYDOWN(100308) = -1 OR _KEYDOWN(100307) = -1 THEN 'alt
                    hitk = _KEYHIT
                    IF hitk = 97 OR hitk = 65 THEN 'alt + a
                        IF mtype$(selectedm) = "input" THEN
                            DO: LOOP UNTIL _KEYDOWN(hitk) = 0 'prevents multiple triggers
                            IF allsel(selectedm) = 1 THEN allsel(selectedm) = 0 ELSE allsel(selectedm) = 1
                        END IF
                    END IF
                    IF hitk = 67 OR hitk = 99 THEN 'alt + c
                        copy:
                        _CLIPBOARD$ = ""
                        m2 = 0: DO: m2 = m2 + 1
                            SELECT CASE mtype$(m2)
                                CASE "text"
                                    IF LEN(text$(m2)) > 0 THEN
                                        cliparray$(m2) = cutcontent$(text$(m2))
                                        IF LEN(cliparray$(m2)) = 50 THEN
                                            cliparray$(m2) = ""
                                        END IF
                                    ELSE
                                        cliparray$(m2) = ""
                                    END IF
                                CASE "input"
                                    UIn$ = ""
                                    IF g(m2) > 0 THEN
                                        g = 0: DO: g = g + 1
                                            UIn$ = UIn$ + char$(m2, g)
                                        LOOP UNTIL g = g(m2)
                                    END IF
                                    cliparray$(m2) = UIn$
                            END SELECT
                            _CLIPBOARD$ = _CLIPBOARD$ + CHR$(13) + cliparray$(m2)
                        LOOP UNTIL m2 = maxm
                        maxcliparray = maxm
                        clearStatus
                        newStatus "Alles Kopiert.", "green"
                    END IF
                END IF
                IF _KEYHIT = 15360 THEN
                    _CLIPBOARDIMAGE = mcanvas&
                    newStatus "Screenshot kopiert.", "green"
                END IF
                Taste$ = INKEY$
                IF Taste$ <> "" THEN
                    SELECT CASE Taste$
                        CASE CHR$(0) + CHR$(80) 'arrow down
                            decrease:
                            IF mtype$(selectedm) = "date" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF year(selectedm) > VAL(MID$(DATE$, 7, 4)) THEN
                                            year(selectedm) = year(selectedm) - 1
                                            IF day(selectedm) = maxday(year(selectedm) + 1, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            year(selectedm) = VAL(MID$(DATE$, 7, 4)) + 5
                                            IF day(selectedm) = maxday(VAL(MID$(DATE$, 7, 4)), month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 2
                                        IF month(selectedm) > 1 THEN
                                            month(selectedm) = month(selectedm) - 1
                                            IF day(selectedm) = maxday(year(selectedm), month(selectedm) + 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            month(selectedm) = 12
                                            IF day(selectedm) = maxday(year(selectedm), 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 3
                                        IF day(selectedm) > 1 THEN
                                            day(selectedm) = day(selectedm) - 1
                                        ELSE
                                            day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                END SELECT
                                change(selectedm) = 1
                            ELSEIF mtype$(selectedm) = "time" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF hour(selectedm) > 0 THEN hour(selectedm) = hour(selectedm) - 1 ELSE hour(selectedm) = 23
                                    CASE 2
                                        IF minute(selectedm) > 0 THEN minute(selectedm) = minute(selectedm) - 5 ELSE minute(selectedm) = 55
                                END SELECT
                                change(selectedm) = 1
                            ELSE
                                IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                            END IF
                        CASE CHR$(0) + CHR$(72) 'arrow up
                            increase:
                            IF mtype$(selectedm) = "date" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF year(selectedm) < VAL(MID$(DATE$, 7, 4)) + 10 THEN
                                            year(selectedm) = year(selectedm) + 1
                                            IF day(selectedm) = maxday(year(selectedm) - 1, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            year(selectedm) = VAL(MID$(DATE$, 7, 4))
                                            IF day(selectedm) = maxday(VAL(MID$(DATE$, 7, 4)) + 10, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 2
                                        IF month(selectedm) < 12 THEN
                                            month(selectedm) = month(selectedm) + 1
                                            IF day(selectedm) = maxday(year(selectedm), month(selectedm) - 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            month(selectedm) = 1
                                            IF day(selectedm) = maxday(year(selectedm), 12) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 3
                                        IF day(selectedm) < maxday(year(selectedm), month(selectedm)) THEN
                                            day(selectedm) = day(selectedm) + 1
                                        ELSE
                                            day(selectedm) = 1
                                        END IF
                                END SELECT
                                change(selectedm) = 1
                            ELSEIF mtype$(selectedm) = "time" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF hour(selectedm) < 23 THEN hour(selectedm) = hour(selectedm) + 1 ELSE hour(selectedm) = 0
                                    CASE 2
                                        IF minute(selectedm) < 54 THEN minute(selectedm) = minute(selectedm) + 5 ELSE minute(selectedm) = 0
                                END SELECT
                                change(selectedm) = 1
                            ELSE
                                IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                            END IF
                        CASE CHR$(0) + CHR$(77) 'arrow right
                            IF mtype$(selectedm) = "selector" THEN
                                IF selected(selectedm) < maxad(array(selectedm)) THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSEIF mtype$(selectedm) = "date" THEN
                                IF selected(selectedm) < 3 THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSEIF mtype$(selectedm) = "time" THEN
                                IF selected(selectedm) < 2 THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSE
                                IF selectedm < maxm THEN selectedm = selectedm + 1: change(selectedm - 1) = 1 ELSE selectedm = 1
                                change(1) = 1
                            END IF
                        CASE CHR$(0) + CHR$(75) 'arrow left
                            IF mtype$(selectedm) = "selector" OR mtype$(selectedm) = "date" OR mtype$(selectedm) = "time" THEN
                                IF selected(selectedm) > 1 THEN
                                    selected(selectedm) = selected(selectedm) - 1
                                    change(selectedm) = 1
                                ELSE
                                    IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                                END IF
                            ELSE
                                IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                            END IF
                        CASE CHR$(9)
                            IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                        CASE CHR$(13)
                            IF mtype$(selectedm) = "input" AND selectedm < maxm AND mtype$(selectedm + 1) = "input" THEN
                                LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) + 2
                                PRINT "   "
                                selectedm = selectedm + 1
                                'GOTO commitchange
                            ELSEIF mtype$(selectedm) = "input" AND selectedm = inputcount THEN
                                LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) + 2
                                PRINT "   "
                                selectedm = selectedm + 1
                                GOTO commitchange
                            END IF

                            engageonm: 'wird getriggert, wenn auf m geklickt wird oder enter gedruckt wird
                            SELECT CASE mtype$(selectedm)
                                CASE "slider"
                                    IF _MOUSEBUTTON(1) = -1 THEN
                                        DO
                                            IF _MOUSEINPUT = -1 AND _MOUSEX > basex(selectedm) AND _MOUSEX < endx(selectedm) THEN
                                                value(m) = _MOUSEX - (basex(selectedm))
                                                value(m) = INT(value(selectedm) / (maxx / 4) * 100)
                                                printslider:
                                                'cleanup
                                                LINE (basex(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + 1)-(endx(selectedm), (firstline + yoffset(selectedm)) * fontheight - 1), colour&("black"), BF

                                                'horizontal
                                                LINE (basex(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + (fontheight / 2))-(endx(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + (fontheight / 2) + 1), colour&("white"), BF
                                                'vertical @ value
                                                LINE (basex(selectedm) + (value(selectedm) / maxval(selectedm) * (maxx / 4)), (firstline + yoffset(selectedm) - 1) * fontheight + 1)-(basex(selectedm) + 4 + (value(selectedm) / maxval(selectedm) * (maxx / 4)), (firstline + yoffset(selectedm)) * fontheight - 1), colour&("white"), BF
                                                xvalue = firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + ((maxx / 4) / fontwidth) + 2
                                                LOCATE firstline + yoffset(selectedm), xvalue
                                                COLOR colour&("fg"), colour&("bg")
                                                PRINT value(selectedm); "   "
                                            ELSEIF _MOUSEINPUT = -1 AND _MOUSEX < basex(selectedm) OR _MOUSEX > basex(selectedm) + 4 + (maxx / 4) THEN
                                                IF _MOUSEX < basex(selectedm) THEN
                                                    value(selectedm) = minval(selectedm)
                                                ELSE
                                                    value(selectedm) = maxval(selectedm)
                                                END IF
                                                GOTO printslider
                                            END IF
                                        LOOP UNTIL _MOUSEBUTTON(1) = 0
                                    END IF
                                CASE "selector"
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                CASE "date"
                                    IF selected(selectedm) < maxad(array(selectedm)) THEN
                                        selected(selectedm) = selected(selectedm) + 1: change(selectedm) = 1
                                    ELSE
                                        IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                    END IF
                                CASE "time"
                                    IF selected(selectedm) < maxad(array(selectedm)) THEN
                                        selected(selectedm) = selected(selectedm) + 1: change(selectedm) = 1
                                    ELSE
                                        IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                    END IF
                                CASE "menu"
                                    IF destination$(selectedm) = "system" THEN
                                        SYSTEM
                                    ELSE
                                        SELECT CASE destination$(selectedm)
                                            CASE "copy": change(m) = 1: GOTO copy
                                            CASE "paste": change(m) = 1: GOTO paste
                                        END SELECT
                                        endparameter$ = destination$(selectedm): endmenu = 1
                                    END IF
                                CASE "toggle"
                                    endmenu = 0
                                    IF state(selectedm) = 1 THEN state(selectedm) = 0 ELSE state(selectedm) = 1
                                    SELECT CASE setting$(selectedm)
                                        CASE "darkmode"
                                            IF darkmode = 1 THEN darkmode = 0 ELSE darkmode = 1
                                        CASE "size"
                                            IF bigwindow = 1 THEN bigwindow = 0 ELSE bigwindow = 1
                                    END SELECT: change(selectedm) = 1
                            END SELECT
                        CASE CHR$(27)
                            endmenu = 1: endparameter$ = "up"
                    END SELECT
                END IF
                commitchange:
                IF selectedm <> prevm THEN
                    change(selectedm) = 1: change(prevm) = 1 ': Background layout, mtitle$, 0
                END IF
                'if change is there, print the menu again
                IF change = 1 OR firstprint = 1 OR change(m) = 1 THEN
                    IF change(m) = 1 THEN
                        LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("bg"), BF
                    END IF
                    IF runm = m + 1 THEN
                        change = 0
                    END IF
                    SELECT CASE mtype$(m)
                        CASE "slider"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("bg")
                            PRINT text$(m)
                            'horizontal
                            IF selectedm = m THEN
                                LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(basex(m) + (maxx / 4), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), colour&("fg"), BF
                            ELSE
                                LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(basex(m) + (maxx / 4), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), colour&("offfocus"), BF
                            END IF
                            'vertical
                            rectangle basex(m) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m) - 1) * fontheight + 1, basex(m) + (UMround * 2) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m)) * fontheight - 1, UMround, colour&("red"), "BF"
                            xvalue = firstchar + xoffset(m) + LEN(text$(m)) + ((maxx / 4) / fontwidth) + 2
                            LOCATE firstline + yoffset(m), xvalue
                            PRINT value(m); "   "
                        CASE "date"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("transparent")
                            PRINT text$(m) + " ";
                            'counting elements
                            melementxl(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)))
                            melementxr(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN((text$(m)) + convyear$(year(m))))
                            melementxl(m, 2) = melementxr(m, 1) + fontwidth
                            melementxr(m, 2) = melementxl(m, 2) + (fontwidth * LEN(convmonth$(month(m))))
                            melementxl(m, 3) = melementxr(m, 2) + fontwidth
                            melementxr(m, 3) = melementxl(m, 3) + (fontwidth * LEN(convday$(day(m))))
                            LINE (melementxl(m, 1) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, 3) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("bg"), BF
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("transparent")
                                LINE (melementxl(m, selected(m)) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, selected(m)) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("red"), BF
                            ELSE
                                COLOR colour&("offfocus"), colour&("transparent")
                            END IF
                            PRINT convyear$(year(m)) + " "; convmonth$(month(m)) + " "; convday$(day(m)) + "   "
                            'triangles
                            drawTriangle changevalxl(m), increasevalyu(m), mtriscale, -1
                            drawTriangle changevalxl(m), decreasevalyu(m), mtriscale, 1
                        CASE "time"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("transparent")
                            PRINT text$(m) + " ";
                            'counting elements
                            melementxl(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)))
                            melementxr(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN((text$(m)) + convhour$(hour(m))))
                            melementxl(m, 2) = melementxr(m, 1) + fontwidth
                            melementxr(m, 2) = melementxl(m, 2) + (fontwidth * LEN(convminute$(minute(m))))
                            LINE (melementxl(m, 1) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, 2) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("bg"), BF
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("transparent")
                                IF selectedm = m THEN LINE (melementxl(m, selected(m)) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, selected(m)) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("red"), BF
                            ELSE
                                COLOR colour&("offfocus"), colour&("transparent")
                            END IF
                            PRINT convhour$(hour(m)); ":"; convminute$(minute(m)); " Uhr   "
                            'triangles
                            drawTriangle changevalxl(m), increasevalyu(m), mtriscale, -1
                            drawTriangle changevalxl(m), decreasevalyu(m), mtriscale, 1
                        CASE "selector"
                            'zeigt nicht editierbaren text an
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("bg")
                            PRINT text$(m);
                            IF selectedm <> m THEN
                                COLOR colour&("offfocus"), colour&("bg")
                            ELSE
                                COLOR colour&("red"), colour&("bg")
                            END IF
                            PRINT " < "; _TRIM$(arraydata$(array(m), selected(m))); " >  ";
                            COLOR colour&("offfocus")
                            suche$ = ""
                            IF g(m) > 0 AND m = selectedm THEN
                                gbf(m) = 0: DO: gbf(m) = gbf(m) + 1: suche$ = suche$ + char$(m, gbf(m)): LOOP UNTIL gbf(m) = g(m)
                            END IF
                            PRINT "-" + suche$ + SPC(50 - LEN(arraydata$(array(m), selected(m))) - LEN(suche$))
                            rectangle (firstchar + xoffset(m) + LEN(text$(m)) - 0.5) * fontwidth, (firstline + yoffset(m) - 1) * fontheight - 6, (firstchar + xoffset(m) + LEN(arraydata$(array(m), selected(m))) + LEN(text$(m)) + 5) * fontwidth, (firstline + yoffset(m)) * fontheight + 4, UMround, colour&("fg"), "B"
                            endx(m) = basex(m) + (LEN(text$(m)) + LEN(arraydata$(array(m), selected(m))) + 11) * fontwidth + (fontwidth / 2)
                        CASE "toggle"
                            LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("bg"), BF
                            IF selectedm = m THEN
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("red"), B
                                COLOR colour&("red"), colour&("bg")
                            ELSE
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("offfocus"), B
                                COLOR colour&("fg"), colour&("bg")
                            END IF
                            IF state(m) = 0 THEN
                                IF selectedm = m THEN
                                    rectangle basex(m) + 2, basey(m) + 2, endx(m) - (endx(m) - basex(m)) / 2 - 2, endy(m) - 2, UMround, colour&("fg"), "BF"
                                ELSE
                                    rectangle basex(m) + 2, basey(m) + 2, endx(m) - (endx(m) - basex(m)) / 2 - 2, endy(m) - 2, UMround, colour&("offfocus"), "BF"
                                END IF
                            ELSE
                                rectangle basex(m) + (endx(m) - basex(m)) / 2 + 2, basey(m) + 2, endx(m) - 2, endy(m) - 2, UMround, colour&("red"), "BF"
                            END IF
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            PRINT text$(m)
                        CASE "input"
                            'zeigt nicht editierbaren text an
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF selectedm <> m THEN
                                COLOR colour&("fg"), colour&("transparent")
                            ELSE
                                COLOR colour&("red"), colour&("transparent")
                            END IF
                            PRINT text$(m)

                            'zeigt editierbaren text an
                            IF allsel(selectedm) = 0 THEN
                                IF selectedm = m THEN
                                    COLOR colour&("red"), colour&("bg")
                                ELSE
                                    COLOR colour&("offfocus"), colour&("bg")
                                END IF
                            ELSE
                                IF selectedm = m THEN
                                    IF darkmode = 1 THEN COLOR colour&("red"), colour&("dark grey"): ELSE COLOR colour&("red"), colour&("light grey")
                                ELSE
                                    IF darkmode = 1 THEN COLOR colour&("white"), colour&("dark grey"): ELSE COLOR colour&("black"), colour&("light grey")
                                END IF
                            END IF
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m) + LEN(text$(m)) + 2
                            IF g(m) > 0 THEN
                                gbf(m) = 0
                                UserInput$(m) = ""
                                DO
                                    gbf(m) = gbf(m) + 1
                                    IF MID$(text$(m), 1, 8) = "Passwort" AND showpassword = 0 THEN
                                        PRINT "*";
                                    ELSE
                                        PRINT char$(m, gbf(m));
                                    END IF
                                LOOP UNTIL gbf(m) = g(m)
                            END IF
                            PRINT " "
                            endx(m) = basex(m) + (LEN(text$(m)) + g(m) + 2) * fontwidth + (fontwidth / 2)
                        CASE "menu"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF m <> selectedm THEN
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("fg"), "B"
                                COLOR colour&("fg"), colour&("transparent")
                            ELSE
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("red"), "BF"
                                COLOR colour&("white"), colour&("transparent")
                            END IF
                            PRINT text$(m)
                        CASE "text"
                            SELECT CASE weight$(m)
                                CASE "r"
                                    _FONT r&
                                CASE "b"
                                    _FONT b&
                            END SELECT
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("red")
                            ELSE
                                COLOR clr&(m), colour&("transparent")
                            END IF
                            PRINT text$(m)
                            _FONT r&
                    END SELECT
                    IF mtype$(m) = "input" OR mtype$(m) = "selector" OR mtype$(m) = "date" OR mtype$(m) = "time" THEN 'thick line below elements
                        LINE ((firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m)) * fontheight + 4), colour&("bg"), BF
                        LINE ((firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 1)-(endx(m) + (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 4), colour&("fg"), BF
                    END IF
                END IF
                change(m) = 0
                IF mtype$(selectedm) = "input" THEN
                    IF Taste$ <> "" THEN
                        IF Taste$ = CHR$(8) AND g(selectedm) > 0 AND allsel(selectedm) = 0 THEN
                            g(selectedm) = g(selectedm) - 1: char$(selectedm, g(selectedm) + 1) = ""
                        ELSEIF Taste$ = CHR$(8) AND g(selectedm) > 0 AND allsel(selectedm) = 1 THEN
                            g(selectedm) = 0: DO: g(selectedm) = g(selectedm) + 1: char$(selectedm, g(selectedm)) = "": LOOP UNTIL g(selectedm) = gbf(selectedm): g(selectedm) = 0: PRINT " "
                            allsel(selectedm) = 0
                            LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm))
                            DO: g(selectedm) = g(selectedm) + 1
                                PRINT " ";
                            LOOP UNTIL g(selectedm) = gbf(selectedm) + 3
                            g(selectedm) = 0
                        END IF
                        ac = 0
                        DO
                            ac = ac + 1
                            IF number(selectedm) = 1 THEN
                                IF ac$(ac) = Taste$ AND ASC(Taste$) > 47 AND ASC(Taste$) < 58 THEN
                                    g(selectedm) = g(selectedm) + 1
                                    char$(selectedm, g(selectedm)) = Taste$
                                END IF
                            ELSE
                                IF ac$(ac) = Taste$ THEN
                                    g(selectedm) = g(selectedm) + 1
                                    char$(selectedm, g(selectedm)) = Taste$
                                END IF
                            END IF
                        LOOP UNTIL ac = alch
                        IF replace$(Taste$) <> "" THEN
                            g(selectedm) = g(selectedm) + 1
                            char$(selectedm, g(selectedm)) = replace$(Taste$)
                        END IF
                    END IF
                    IF gbf(selectedm) <> g(selectedm) THEN
                        LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) + 2
                        PRINT "   "
                        change(selectedm) = 1
                        gbf(selectedm) = g(selectedm)
                    END IF
                    IF TIMER MOD 2 = 0 AND g(selectedm) = gbf(selectedm) THEN
                        LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + g(selectedm) + 2
                        COLOR colour&("red"), colour&("bg")
                        PRINT "|  "
                    ELSE
                        LOCATE firstline + yoffset(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + g(selectedm) + 2
                        COLOR colour&("bg"), colour&("bg")
                        PRINT "   "
                    END IF
                    IF change(m) = 1 AND selectedm <> m THEN
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m) + LEN(text$(m)) + g(m) + 2
                        PRINT "   "
                    END IF
                ELSEIF mtype$(selectedm) = "selector" THEN
                    IF Taste$ <> "" THEN
                        IF Taste$ = CHR$(8) AND g(selectedm) > 0 THEN
                            g(selectedm) = g(selectedm) - 1: char$(selectedm, g(selectedm) + 1) = "": change(selectedm) = 1
                        END IF
                        ac = 0
                        DO
                            ac = ac + 1
                            IF ac$(ac) = Taste$ THEN
                                g(selectedm) = g(selectedm) + 1
                                char$(selectedm, g(selectedm)) = Taste$
                            END IF
                        LOOP UNTIL ac = alch
                        IF replace$(Taste$) <> "" THEN
                            g(selectedm) = g(selectedm) + 1
                            char$(selectedm, g(selectedm)) = replace$(Taste$)
                        END IF
                        IF gbf(selectedm) <> g(selectedm) AND g(selectedm) > 0 THEN
                            change(selectedm) = 1
                            suche$ = ""
                            gbf(selectedm) = 0: DO: gbf(selectedm) = gbf(selectedm) + 1: suche$ = suche$ + char$(selectedm, gbf(selectedm)): LOOP UNTIL gbf(selectedm) = g(selectedm)
                            ad = 0: DO: ad = ad + 1
                                IF LEN(suche$) < LEN(arraydata$(array(selectedm), ad)) THEN
                                    p = 0: DO: p = p + 1
                                        IF MID$(arraydata$(array(selectedm), ad), p, LEN(suche$)) = suche$ THEN
                                            selected(selectedm) = ad: ad = maxad(array(selectedm))
                                            'p = LEN(arraydata$(array(selectedm), ad)) - LEN(suche$) + 1
                                        END IF
                                    LOOP UNTIL p >= LEN(arraydata$(array(selectedm), ad)) - LEN(suche$) + 1
                                ELSEIF suche$ = arraydata$(array(selectedm), ad) THEN
                                    selected(selectedm) = ad: ad = maxad(array(selectedm)): change(selectedm) = 1
                                END IF
                            LOOP UNTIL ad = maxad(array(selectedm))
                        ELSEIF g(selectedm) = 0 THEN
                            gbf(selectedm) = 0
                        END IF
                    END IF
                END IF
            LOOP UNTIL m = maxm
            firstprint = 0
            frameend = TIMER
            frametime = frameend - framestart
            fps = fps + 1
            IF frametime >= 1 THEN
                LOCATE maxlines - 1, maxrows - 4
                COLOR colour&("fg"), colour&("bg")
                PRINT fps
                IF fps < (framerate * 2) THEN 'removes rounding in case the framerate drops below half, potentially saves some time
                    UMround = 4
                    IF st > 0 THEN printStatus
                ELSE
                    UMround = 0
                END IF
                fps = 0
            END IF
            _DEST 0
            _PUTIMAGE (0, 0), mcanvas&
            _DISPLAY
            _DEST mcanvas&
            _LIMIT framerate
        LOOP UNTIL endmenu = 1 OR interactable = 0
        m = 0: DO: m = m + 1
            SELECT CASE mtype$(m)
                CASE "input"
                    g(m) = 0
                    IF gbf(m) > 0 THEN
                        DO
                            g(m) = g(m) + 1
                            UserInput$(m) = UserInput$(m) + char$(m, g(m))
                            char$(m, g(m)) = ""
                        LOOP UNTIL g(m) = gbf(m)
                    END IF
                CASE "selector"
                    UserInput$(m) = arraydata$(array(m), selected(m))
                    IF g(m) > 0 THEN
                        g = 0: DO: g = g + 1: char$(m, g) = "": LOOP UNTIL g = g(m)
                    END IF
                    g(m) = 0
                    gbf(m) = 0
                CASE "date"
                    year$ = convyear$(year(m))
                    month$ = convmonth$(month(m))
                    day$ = convday$(day(m))
                    UserInput$(m) = year$ + "-" + month$ + "-" + day$
                CASE "time"
                    hour$ = convhour$(hour(m))
                    minute$ = convminute$(minute(m))
                    UserInput$(m) = hour$ + ":" + minute$
            END SELECT
        LOOP UNTIL m = maxm

        'tempsave
        IF endparameter$ <> "save" AND listID <> 0 THEN
            m2 = 0: DO: m2 = m2 + 1
                temparray$(listID, m2) = UserInput$(m2)
                IF LEN(temparray$(listID, m2)) = 50 THEN
                    temparray$(listID, m2) = ""
                END IF
            LOOP UNTIL m2 = maxm
            maxtemparray(listID) = maxm
        END IF
    END IF
    _AUTODISPLAY
    _DEST 0
    clearStatus
    'writeTemp templist$
    EmptyMenu
END SUB

FUNCTION replace$ (chor$)
    SELECT CASE chor$
        CASE CHR$(228) 'ae
            replace$ = CHR$(132)
        CASE CHR$(196) 'AE
            replace$ = CHR$(142)
        CASE CHR$(252) 'ue
            replace$ = CHR$(129)
        CASE CHR$(220) 'UE
            replace$ = CHR$(154)
        CASE CHR$(246) 'oe
            replace$ = CHR$(148)
        CASE CHR$(214) 'OE
            replace$ = CHR$(153)
        CASE CHR$(223) 'ss
            replace$ = CHR$(225)
    END SELECT
END FUNCTION

SUB printStatus
    maxst = st
    _FONT b&
    st = 0: DO: st = st + 1
        COLOR colour&(color$(st)), colour&("bg")
        LOCATE firstline + st, maxrows - LEN(sttext$(st)) - 10
        PRINT SPC(5) + sttext$(st)
    LOOP UNTIL st = maxst
    _FONT r&
END SUB

SUB newStatus (sttext$, color$)
    st = st + 1
    sttext$(st) = sttext$
    color$(st) = color$
END SUB

SUB clearStatus
    st = 0: maxst = 0
END SUB

FUNCTION convyear$ (year)
    IF year > 999 THEN
        convyear$ = LST$(year)
    ELSEIF year > 99 THEN
        convyear$ = "0" + LST$(year)
    ELSEIF year(m) > 9 THEN
        convyear$ = "00" + LST$(year)
    ELSEIF year > 0 THEN
        convyear$ = "000" + LST$(year)
    ELSE: convyear$ = "0000": END IF
END FUNCTION

FUNCTION convmonth$ (month)
    IF month > 9 THEN
        convmonth$ = LST$(month)
    ELSEIF month > 0 THEN
        convmonth$ = "0" + LST$(month)
    ELSE: convmonth$ = "00": END IF
END FUNCTION

FUNCTION convday$ (day)
    IF day > 9 THEN
        convday$ = LST$(day)
    ELSEIF day > 0 THEN
        convday$ = "0" + LST$(day)
    ELSE: convday$ = "00": END IF
END FUNCTION

FUNCTION convhour$ (hour)
    IF hour > 9 THEN
        convhour$ = LST$(hour)
    ELSEIF hour > 0 THEN
        convhour$ = "0" + LST$(hour)
    ELSE: convhour$ = "00": END IF
END FUNCTION

FUNCTION convminute$ (minute)
    IF minute > 9 THEN
        convminute$ = LST$(minute)
    ELSEIF minute > 0 THEN
        convminute$ = "0" + LST$(minute)
    ELSE: convminute$ = "00": END IF
END FUNCTION

SUB NewSlider (yoffset, xoffset, minval, maxval, text$, destination$, standard)
    m = m + 1
    interactable = 1
    mtype$(m) = "slider"
    yoffset(m) = yoffset * 2 - 1
    minval(m) = minval
    maxval(m) = maxval
    xoffset(m) = xoffset
    text$(m) = text$
    destination$(m) = destination$
    basex(m) = (firstchar + xoffset(m) + LEN(text$(m))) * fontwidth + (fontwidth / 2) 'change this to fit your setup
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 10) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    value(m) = standard
    endx(m) = basex(m) + (maxx / 4) + 4
END SUB

SUB NewTime (yoffset, xoffset, text$, standard$)
    m = m + 1
    mtype$(m) = "time"
    selected(m) = 1
    hour(m) = VAL(MID$(standard$, 1, 2))
    minute(m) = VAL(MID$(standard$, 4, 2))
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 12) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    changevalxl(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 10)
    changevalxr(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 10 + mtriscale)
    increasevalyu(m) = fontheight * (firstline + yoffset(m) - 1)
    increasevalyb(m) = increasevalyu(m) + ((fontwidth * mtriscale) * 2 / 3)
    decreasevalyu(m) = fontheight * (firstline + yoffset(m) - 0.4)
    decreasevalyb(m) = decreasevalyu(m) + ((fontwidth * mtriscale) * 2 / 3)
END SUB

SUB NewDate (yoffset, xoffset, text$, standard$)
    m = m + 1
    mtype$(m) = "date"
    selected(m) = 1
    g(m) = 0: gbf(m) = 0
    month(m) = VAL(MID$(standard$, 1, 2))
    day(m) = VAL(MID$(standard$, 4, 2))
    year(m) = VAL(MID$(standard$, 7, 4))
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 14) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    changevalxl(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 11)
    changevalxr(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 11 + mtriscale)
    increasevalyu(m) = fontheight * (firstline + yoffset(m) - 1)
    increasevalyb(m) = increasevalyu(m) + ((fontwidth * mtriscale) * 2 / 3)
    decreasevalyu(m) = fontheight * (firstline + yoffset(m) - 0.4)
    decreasevalyb(m) = decreasevalyu(m) + ((fontwidth * mtriscale) * 2 / 3)
END SUB

SUB NewSelector (yoffset, xoffset, text$, array, destination$, standard)
    m = m + 1
    mtype$(m) = "selector"
    destination$(m) = destination$
    array(m) = array
    selected(m) = standard
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 1) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewToggle (yoffset, xoffset, text$, setting$)
    m = m + 1
    mtype$(m) = "toggle"
    setting$(m) = setting$
    SELECT CASE setting$(m)
        CASE "darkmode"
            state(m) = darkmode
        CASE "size"
            state(m) = bigwindow
    END SELECT
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    interactable = 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + (LEN(text$(m))) + xoffset(m) + 1) * fontwidth
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + 2 * fontheight
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewText (yoffset, xoffset, text$, clr&, weight$)
    m = m + 1
    mtype$(m) = "text"
    text$(m) = text$
    weight$(m) = weight$
    clr&(m) = clr&
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$(m)) + 1) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewInput (yoffset, xoffset, text$, placeholder$, number)
    m = m + 1
    g(m) = 0
    IF LEN(placeholder$) > 0 THEN
        gbf(m) = 0
        DO
            g(m) = g(m) + 1
            char$(m, g(m)) = MID$(placeholder$, g(m), 1)
        LOOP UNTIL g(m) = LEN(placeholder$)
    END IF
    gbf(m) = g(m)
    number(m) = number
    inputcount = m
    mtype$(m) = "input"
    text$(m) = text$
    interactable = 1
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$(m)) + g(m) + 2) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewMenItem (yoffset, xoffset, text$, destination$)
    m = m + 1
    mtype$(m) = "menu"
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 2) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 3) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    destination$(m) = destination$
END SUB

SUB Background (layout, mtitle$, maintrigger)
    IF maintrigger = 1 THEN
        COLOR colour&("fg"), colour&("bg")
        CLS
        SELECT CASE layout
            CASE 0
        END SELECT
    END IF
    SetTitle mtitle$
END SUB

SUB SetTitle (mtitle$)
    IF h& <> 0 THEN _FONT h&
    LOCATE 2
    COLOR colour&("fg"), colour&("transparent")
    PRINT SPC(17) + mtitle$
    IF r& <> 0 THEN _FONT r&
END SUB

SUB progressBar (position, progress, maxprogress)
    SELECT CASE position
        CASE 0 'center
            LINE (maxx / 2 - 200, maxy / 2 - 50)-(maxx / 2 + 200, maxy / 2 - 45), colour&("fg"), B
            LINE (maxx / 2 - 200, maxy / 2 - 50)-(maxx / 2 - 200 + ((progress / maxprogress) * 400), maxy / 2 - 45), colour&("fg"), BF
    END SELECT
END SUB

SUB rectangle (lx, ly, ux, uy, round, clr&, outline$)
    SELECT CASE outline$
        CASE "BF"
            rectangleoutline lx, ly, ux, uy, round, clr&
            PAINT (lx + ((ux - lx) / 2), ly + ((uy - ly) / 2)), clr&, clr&
            IF shading = 1 THEN
                IF clr& = colour&("white") THEN
                    shader& = _RGBA(0, 0, 0, 80)
                ELSE
                    IF colour&("bg") <> colour&("white") THEN
                        shader& = colour&("white")
                    ELSE
                        shader& = _RGBA(0, 0, 0, 80)
                    END IF
                END IF
                'top right
                x = -0.25 * _PI
                DO: x = x + ((0.25 * _PI) / round / detail)
                    PSET ((lx + round) + (SIN(x) * round), (ly + round) - (COS(x) * round)), shader&
                LOOP UNTIL x >= 0
                x = -0.5 * _PI
                DO: x = x + ((0.5 * _PI) / round / detail)
                    PSET ((ux - round) - (SIN(x) * round), (ly + round) - (COS(x) * round)), shader&
                LOOP UNTIL x >= 0
                x = -0.5 * _PI
                DO: x = x + ((0.25 * _PI) / round / detail)
                    PSET ((ux - round) - (SIN(x) * round), (uy - round) + (COS(x) * round)), shader&
                LOOP UNTIL x >= -0.25
                LINE (lx + round, ly)-(ux - round, ly), shader&
                LINE (ux, ly + round)-(ux, uy - round), shader&
            END IF
        CASE "B"
            rectangleoutline lx, ly, ux, uy, round, clr&
    END SELECT
END SUB

SUB rectangleoutline (lx, ly, ux, uy, round, clr&)
    IF round > 0 THEN
        '           corners:
        'lx-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'lx-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        '           lines:
        LINE (lx + round, ly)-(ux - round, ly), clr&
        LINE (lx + round, uy)-(ux - round, uy), clr&
        LINE (lx, ly + round)-(lx, uy - round), clr&
        LINE (ux, ly + round)-(ux, uy - round), clr&
    ELSE
        LINE (lx, ly)-(ux, uy), clr&, B
    END IF
END SUB

SUB drawTriangle (x, y, trianglescale, direction)
    'variables for dropdown triangle (right side, you know?)
    p1x = x
    IF direction = 1 THEN p1y = y ELSE p1y = y + (((fontwidth * trianglescale) * 2 / 3))
    p2x = p1x + (fontwidth * trianglescale)
    p2y = p1y
    p3x = p1x + ((p2x - p1x) / 2)
    p3y = p1y + (((fontwidth * trianglescale) * 2 / 3) * direction)

    tricolor& = colour&("fg")
    'triangle
    LINE (p1x, p1y)-(p2x, p2y), tricolor&: LINE (p1x, p1y)-(p3x, p3y), tricolor&: LINE (p2x, p2y)-(p3x, p3y), tricolor&
    PAINT (p1x + ((p2x - p1x) / 2), p1y + ((p3y - p1y) / 2)), tricolor&, tricolor&
END SUB

SUB defChar
    RESTORE KB1031toCP437_Cp437
    RESTORE KB1031toCP437_Umlaut
    EXIT SUB
    KB1031toCP437_Umlaut:
    DATA 8,65,142,79,153,85,154,97,132,101,137,105,139,111,148,117,129
    KB1031toCP437_Cp437:
    DATA 199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197
    DATA 201,230,198,244,246,242,251,249,255,214,220,162,163,165,8359,402
    DATA 225,237,243,250,241,209,170,186,191,8976,172,189,188,161,171,187
    DATA 9617,9618,9619,9474,9508,9569,9570,9558,9557,9571,9553,9559,9565,9564,9563,9488
    DATA 9492,9524,9516,9500,9472,9532,9566,9567,9562,9556,9577,9574,9568,9552,9580,9575
    DATA 9576,9572,9573,9561,9560,9554,9555,9579,9578,9496,9484,9608,9604,9612,9616,9600
    DATA 945,223,915,960,931,963,181,964,934,920,937,948,8734,966,949,8745
    DATA 8801,177,8805,8804,8992,8993,247,8776,176,8729,183,8730,8319,178,9632,32
END SUB

FUNCTION longchar$ (char$, length) 'returns a string consisting of one character repeated (length) times
    temp$ = ""
    l = 0: DO: l = l + 1
        temp$ = temp$ + char$
    LOOP UNTIL l = length
    longchar$ = temp$
END FUNCTION

FUNCTION LST$ (number) 'converts number into string
    LST$ = LTRIM$(STR$(number))
END FUNCTION

FUNCTION isLeap (year)
    IF year MOD 4 <> 0 THEN
        isLeap = 0
    ELSEIF year MOD 100 <> 0 THEN
        isLeap = 1
    ELSEIF year MOD 400 <> 0 THEN
        isLeap = 0
    ELSE
        isLeap = 1
    END IF
END FUNCTION

FUNCTION maxday (year, month)
    SELECT CASE month
        CASE 2
            IF isLeap(year) = 1 THEN
                maxday = 29
            ELSE
                maxday = 28
            END IF
        CASE 1: maxday = 31
        CASE 3: maxday = 31
        CASE 5: maxday = 31
        CASE 7: maxday = 31
        CASE 8: maxday = 31
        CASE 10: maxday = 31
        CASE 12: maxday = 31
        CASE 4: maxday = 30
        CASE 6: maxday = 30
        CASE 9: maxday = 30
        CASE 11: maxday = 30
    END SELECT
END FUNCTION

FUNCTION cutcontent$ (text$)
    p = 0: DO: p = p + 1
        IF MID$(text$, p, 1) = ":" THEN
            cutcontent$ = LTRIM$(MID$(text$, p + 1, LEN(text$) - p + 1))
            EXIT FUNCTION
        END IF
    LOOP UNTIL p = LEN(text$)
    cutcontent$ = ""
END FUNCTION

