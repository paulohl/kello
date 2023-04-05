      *>*****************************************************
      *>                                                    *
      *> COBOL Vocabulary for Dialog Language               *
      *>                                                    *
      *> (c) Micro Focus Limited  1994                      *
      *>                                                    *
      *>                                                    *
      *>                                                    *
      *>----------------------------------------------------*
      *>                                                    *
      *> This is an Early Release feature.                  *
      *>                                                    *
      *> Please use and evaluate Early Release features     *
      *> and tell Micro Focus what you think. However,      *
      *> do not rely on these features being supported      *
      *> in future products from Micro Focus.               *
      *>                                                    *
      *>----------------------------------------------------*
      *>                                                    *
      *>                                                    *
      *> Including this copyfile will allow selected        *
      *> Dialog System functions to be embedded in          *
      *> your COBOL program.                                *
      *>                                                    *
      *> The syntax, and number of parameters for each      *
      *> function is the same as in Dialog system.          *
      *> However the types of parameters allowed is not     *
      *> as extensive.                                      *
      *>                                                    *
      *> e.g. any function which returns an integer         *
      *>      value, i.e. GET-LIST-ITEM-STATE,              *
      *>      expects the return variable to be a           *
      *>      PIC 9(8) COMP-X data item.                    *
      *>                                                    *
      *>                                                    *
      *> Steps to use this Vocabulary                       *
      *> ----------------------------                       *
      *> 1)  From the Dialog System definition software     *
      *>     select 'File->Generate->Vocabulary Copybook'.  *
      *>                                                    *
      *>     This will generate a copyfile containing a     *
      *>     list of the ids for each named dialog          *
      *>     object.                                        *
      *>                                                    *
      *> 2)  Add the generated copyfile to your COBOL       *
      *>     program, in the WORKING-STORAGE SECTION.       *
      *>                                                    *
      *> 3)  Add this file, 'dslang.cpy', before the        *
      *>     IDENTIFICATION DIVISION of your COBOL          *
      *>     program.                                       *
      *>                                                    *
      *> 4)  Now add the Dialog Functions you require,      *
      *>     to your programs PROCEDURE DIVISION.           *
      *>                                                    *
      *>     e.g.  multiply rate    by total                *
      *>           REFRESH-OBJECT total-entry-field         *
      *>           SET-FOCUS total-entry-field              *
      *>                                                    *
      *> 5)  Compile your program including the directives  *
      *>     'MFOO EARLY-RELEASE'                           *
      *>                                                    *
      *>*****************************************************

       *>
       *> MOVE-WINDOW window direction distance
       *>
       PROGRAM-ID. DSMovWin IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Direction                    PIC 9(8) COMP-X.
       01  Distance                     PIC 9(8) COMP-X.

      *> PROCEDURE DIVISION USING Window-ID Direction Distance
      *>  INVOKE AS ==MOVE-WINDOW <Window-ID> <Direction> <Distance> ==.
      *> END PROGRAM DSMovWin.

       *>
       *> CREATE-WINDOW window
       *>
       PROGRAM-ID. DSCreatWin IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Window-ID
        INVOKE AS ==CREATE-WINDOW <Window-ID> ==.
       END PROGRAM DSCreatWin.

       *>
       *> SHOW-WINDOW window
       *>
       PROGRAM-ID. DSShowWin IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Window-ID
        INVOKE AS ==SHOW-WINDOW <Window-ID> ==.
       END PROGRAM DSShowWin.

       *>
       *> SET-FOCUS object
       *>
       PROGRAM-ID. DSSetFocus IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Obj-ID                      PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID
        INVOKE AS ==SET-FOCUS <Obj-ID> ==.
       END PROGRAM DSSetFocus.

       *>
       *> UNSHOW-WINDOW window focus-target-object
       *>
       PROGRAM-ID. DSUnshowWin IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Focus-Target-ID              PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Window-ID Focus-Target-ID
        INVOKE AS ==UNSHOW-WINDOW <Window-ID> <Focus-Target-ID> ==.
       END PROGRAM DSUnshowWin.

       *>
       *> DELETE-WINDOW window focus-target-object
       *>
       PROGRAM-ID. DSDelWin IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Window-ID                    PIC 9(8) COMP-X.
       01  Focus-Target-ID              PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Window-ID Focus-Target-ID
        INVOKE AS ==DELETE-WINDOW <Window-ID> <Focus-Target-ID> ==.
       END PROGRAM DSDelWin.

       *>
       *> REFRESH-OBJECT object
       *>
       PROGRAM-ID. DSRefreshObj IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Obj-ID                      PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID
        INVOKE AS ==REFRESH-OBJECT <Obj-ID> ==.
       END PROGRAM DSRefreshObj.

       *>
       *> CLEAR-OBJECT object
       *>
       PROGRAM-ID. DSClearObj IS EXTERNAL.
       LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID   
        INVOKE AS ==CLEAR-OBJECT <Obj-ID   > ==.
       end program DSClearObj.


       *>
       *> VALIDATE object
       *>
       PROGRAM-ID. DSValidate IS EXTERNAL.
       LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID   
        INVOKE AS ==VALIDATE <Obj-ID   > ==.
       END PROGRAM DSValidate.

       *>
       *> GET-BUTTON-STATE button state
       *>
       PROGRAM-ID. DSGetButStat IS EXTERNAL.
       LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
       01  State-Return                 PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Button-ID State-Return
        INVOKE AS ==GET-BUTTON-STATE <Button-ID> <State-Return> ==.
       END PROGRAM DSGetButStat.

       *>
       *> SET-BUTTON-STATE button state
       *>
       PROGRAM-ID. DSSetButStat IS EXTERNAL.
       LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
       01  State-Flag                   PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Button-ID State-Flag
        INVOKE AS ==SET-BUTTON-STATE <Button-ID> <State-Flag> ==.
       END PROGRAM DSSetButStat.

       *>
       *> TOGGLE-BUTTON-STATE button
       *>
       PROGRAM-ID. DSTogButStat IS EXTERNAL.
       LINKAGE SECTION.
       01  Button-ID                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Button-ID
        INVOKE AS ==TOGGLE-BUTTON-STATE <Button-ID> ==.
       END PROGRAM DSTogButStat.

       *>
       *> DISABLE-OBJECT object
       *>
       PROGRAM-ID. DSDisObj IS EXTERNAL.
       LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID   
        INVOKE AS ==DISABLE-OBJECT <Obj-ID   > ==.
       END PROGRAM DSDisObj.

       *>
       *> ENABLE-OBJECT object
       *>
       PROGRAM-ID. DSEnaObj IS EXTERNAL.
       LINKAGE SECTION.
       01  Obj-ID                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Obj-ID   
        INVOKE AS ==ENABLE-OBJECT <Obj-ID   > ==.
       END PROGRAM DSEnaObj.

       *>
       *> DELETE-LIST-ITEM list-or-selection-box position count
       *>
       PROGRAM-ID. DSDelLstItm IS EXTERNAL.
       LINKAGE SECTION.
       01  Box-ID                       PIC 9(8) COMP-X.
       01  Pos                          PIC 9(8) COMP-X.
       01  Cnt                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Box-ID Pos Cnt
        INVOKE AS ==DELETE-LIST-ITEM <Box-ID> <Pos> <Cnt> ==.
       END PROGRAM DSDelLstItm.

       *>
       *> SET-SLIDER-RANGE scroll-bar min max
       *>
       PROGRAM-ID. DSSetSldrRang IS EXTERNAL.
       LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Min-R                        PIC 9(8) COMP-X.
       01  Max-R                        PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Scroll-ID Min-R Max-R
        INVOKE AS ==SET-SLIDER-RANGE <Scroll-ID> <Min-R> <Max-R> ==.
       end program DSSetSldrRang.

       *>
       *> SET-PAGE-VALUE scroll-bar page-size
       *>
       PROGRAM-ID. DSSetPagVal IS EXTERNAL.
       LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Page-Value                   PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Scroll-ID Page-Value
        INVOKE AS ==SET-PAGE-VALUE <Scroll-ID> <Page-Value> ==.
       END PROGRAM DSSetPagVal.

       *>
       *> SET-LINE-VALUE scroll-bar line-size
       *>
       PROGRAM-ID. DSSetLinVal IS EXTERNAL.
       LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Line-Size                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Scroll-ID Line-SIze
        INVOKE AS ==SET-LINE-VALUE <Scroll-ID> <Line-Size> ==.
       END PROGRAM DSSetLinVal.

       *>
       *> SET-SLIDER-POSITION scroll-bar position
       *>
       PROGRAM-ID. DSSetSldrPos IS EXTERNAL.
       LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Pos                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Scroll-ID Pos
        INVOKE AS ==SET-SLIDER-POSITION <Scroll-ID> <Pos> ==.
       END PROGRAM DSSetSldrPos.

       *>
       *> SET-SLIDER-SIZE scroll-bar size
       *>
       PROGRAM-ID. DSSetSldrSiz IS EXTERNAL.
       LINKAGE SECTION.
       01  Scroll-ID                    PIC 9(8) COMP-X.
       01  Siz                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING Scroll-ID Siz
        INVOKE AS ==SET-SLIDER-SIZE <Scroll-ID> <Siz> ==.
       END PROGRAM DSSetSldrSiz.


       *>
       *> REPEAT-EVENT
       *>
       PROGRAM-ID. DSRepEvnt IS EXTERNAL.
       PROCEDURE DIVISION
        INVOKE AS ==REPEAT-EVENT ==.
       END PROGRAM DSRepEvnt.

       *>
       *> SET-TOP-LIST-ITEM list-box position
       *>
       PROGRAM-ID. DSSetTopLstItm IS EXTERNAL.
       LINKAGE SECTION.
       01 listbox-id                   PIC 9(8) COMP-X.
       01 listbox-pos                  PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING listbox-id listbox-pos
        INVOKE AS ==SET-TOP-LIST-ITEM <listbox-id> <listbox-pos> ==.
       END PROGRAM DSSetTopLstItm.

       *>
       *> SET-LIST-ITEM-STATE list-box state position
       *>
       PROGRAM-ID. DSSetLstItmStat IS EXTERNAL.
       LINKAGE SECTION.
       01 listbox-id                   PIC 9(8) COMP-X.
       01 state                        PIC 9(8) COMP-X.
       01 listbox-pos                  PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING listbox-id state listbox-pos
        INVOKE AS ==SET-LIST-ITEM-STATE <listbox-id> <state> 
           <listbox-pos> ==.
       END PROGRAM DSSetLstItmStat.

       *>
       *> SET-AUTOSWIPE-STATE entry-field state
       *>
       PROGRAM-ID. DSSetAutoSwStat IS EXTERNAL.
       LINKAGE SECTION.
       01 entry-field                  PIC 9(8) COMP-X.
       01 state                        PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING entry-field state
        INVOKE AS ==SET-AUTOSWIPE-STATE <entry-field> <state> ==.
       END PROGRAM DSSetAutoSwStat.

       *>
       *> SET-FIELD-LIMIT entry-field len
       *>
       PROGRAM-ID. DSSetFldLim IS EXTERNAL.
       LINKAGE SECTION.
       01 entry-field                  PIC 9(8) COMP-X.
       01 len                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING entry-field len
        INVOKE AS ==SET-FIELD-LIMIT <entry-field> <len> ==.
       END PROGRAM DSSetFldLim.

       *>
       *> SET-PRIVILEGE privilege-level
       *>
       PROGRAM-ID. DSSetPriv IS EXTERNAL.
       LINKAGE SECTION.
       01 privilege-level              PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING privilege-level
        INVOKE AS ==SET-PRIVILEGE <privilege-level> ==.
       END PROGRAM DSSetPriv.

       *>
       *> SET-OCCURRENCE object occurrence
       *>
       PROGRAM-ID. DSSetOccur IS EXTERNAL.
       LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
       01 occurence                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING obj-id occurence
        INVOKE AS ==SET-OCCURRENCE <obj-id> <occurence> ==.
       END PROGRAM DSSetOccur.

       *>
       *> COPY-PAGE page position occurence
       *>
       PROGRAM-ID. DSCopPag IS EXTERNAL.
       LINKAGE SECTION.
       01 page-id                      PIC 9(8) COMP-X.
       01 pos                          PIC 9(8) COMP-X.
       01 occurence                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING page-id pos occurence
        INVOKE AS ==COPY-PAGE <page-id> <pos> <occurence> ==.
       END PROGRAM DSCopPag.

       *>
       *> DELETE-PAGE page
       *>
       PROGRAM-ID. DSDelPag IS EXTERNAL.
       LINKAGE SECTION.
       01 page-id                      PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING page-id
        INVOKE AS ==DELETE-PAGE <page-id> ==.
       END PROGRAM DSDelPag.

       *>
       *> HIDE-OBJECT object
       *>
       PROGRAM-ID. DSHideObj IS EXTERNAL.
       LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING obj-id
        INVOKE AS ==HIDE-OBJECT <obj-id> ==.
       END PROGRAM DSHideObj.

       *>
       *> SHOW-OBJECT object
       *>
       PROGRAM-ID. DSShowObj IS EXTERNAL.
       LINKAGE SECTION.
       01 obj-id                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING obj-id
        INVOKE AS ==SHOW-OBJECT <obj-id> ==.
       END PROGRAM DSShowObj.

       *>
       *> REFRESH-MENU window
       *>
       PROGRAM-ID. DSRefMenu IS EXTERNAL.
       LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING window-id
        INVOKE AS ==REFRESH-MENU <window-id> ==.
       END PROGRAM DSRefMenu.

       *>
       *> SET-FIRST-WINDOW window
       *>
       PROGRAM-ID. DSSetFirstWin IS EXTERNAL.
       LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING window-id
        INVOKE AS ==SET-FIRST-WINDOW <window-id> ==.
       END PROGRAM DSSetFirstWin.

       *>
       *> MAXIMIZE-WINDOW window
       *>
       PROGRAM-ID. DSMaxWin IS EXTERNAL.
       LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING window-id
        INVOKE AS ==MAXIMIZE-WINDOW <window-id> ==.
       END PROGRAM DSMaxWin.

       *>
       *> MINIMIZE-WINDOW
       *>
       PROGRAM-ID. DSMinWin IS EXTERNAL.
       LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING window-id
        INVOKE AS ==MINIMIZE-WINDOW <window-id> ==.
       END PROGRAM DSMinWin.

       *>
       *> RESTORE-WINDOW
       *>
       PROGRAM-ID. DSRestWin IS EXTERNAL.
       LINKAGE SECTION.
       01 window-id                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING window-id
        INVOKE AS ==RESTORE-WINDOW <window-id> ==.
       END PROGRAM DSRestWin.


       *>
       *> GET-LIST-ITEM-STATE listbox state position
       *>
       PROGRAM-ID. DSGetLstItmStat IS EXTERNAL.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 ObjState                     PIC 9(8) COMP-X.
       01 ObjPos                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid objstate objpos
       INVOKE AS ==GET-LIST-ITEM-STATE <objid> <objstate> <objpos> ==.
       END PROGRAM DSGetLstItmStat.


       *>
       *> GET-OCCURRENCE page occurence
       *>
       PROGRAM-ID. DSGetOccur IS EXTERNAL.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 Occurence                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid occurence
       INVOKE AS ==GET-OCCURRENCE <objid> <occurence> ==.
       END PROGRAM DSGetOccur.


       *>
       *> GET-DESKTOP-WINDOW object-id
       *>
       PROGRAM-ID. DSGetDesktopWin IS EXTERNAL.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid
       INVOKE AS ==GET-DESKTOP-WINDOW <objid> ==.
       END PROGRAM DSGetDesktopWin.


       *>
       *> GET-SELECTED-LIST-ITEM listbox-id found-position start-position
       *>
       PROGRAM-ID. DSGetSelLstItm IS EXTERNAL.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 found-pos                    PIC 9(8) COMP-X.
       01 start-pos                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid found-pos start-pos
       INVOKE AS ==GET-SELECTED-LIST-ITEM 
      - <objid> <found-pos> <start-pos> ==.
       END PROGRAM DSGetSelLstItm.


       *>
       *> GET-TOP-LIST-ITEM listbox position
       *>
       PROGRAM-ID. DSGetTopLstItm IS EXTERNAL.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 Pos                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid pos
       INVOKE AS ==GET-TOP-LIST-ITEM <objid> <pos> ==.
       END PROGRAM DSGetTopLstItm.


       *>
       *> INSERT-LIST-ITEM listbox text position
       *>
       PROGRAM-ID. DSInsLstItm IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01 ObjId                        PIC 9(8) COMP-X.
       01 DataItem                     PIC x(255).
       01 Pos                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING objid dataitem DELIMITED pos
        INVOKE AS ==INSERT-LIST-ITEM <objid> <dataitem> <pos> ==.
       END PROGRAM DSInsLstItm.

       *>
       *> INVOKE-MESAGE-BOX message-box text return-value
       *>
       PROGRAM-ID. DSInvokeMsgBox1 IS EXTERNAL.
       DATA DIVISION.
       LINKAGE SECTION.
       01 mbox                         PIC 9(8) COMP-X.
       01 mtext                        PIC x(255).
       01 ret-value                    PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING mbox mtext DELIMITED ret-value
        INVOKE AS ==INVOKE-MESSAGE-BOX <mbox> <mtext> <ret-value> ==.
       END PROGRAM DSInvokeMsgBox1.


       *>
       *> MOVE-OBJECT-HANDLE object returned-handle
       *>
       PROGRAM-ID. DSMovObjHand IS EXTERNAL.
       LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 handle                       PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING obj handle
       INVOKE AS ==MOVE-OBJECT-HANDLE <obj> <handle> ==.
       END PROGRAM DSMovObjHand.


       *>
       *> SET-COLOR object foreground-color background-color
       *>
       PROGRAM-ID. DSSetColor IS EXTERNAL.
       LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 fground                      PIC x(255).
       01 bground                      PIC x(255).
       PROCEDURE DIVISION USING obj fground DELIMITED bground DELIMITED
       INVOKE AS ==SET-COLOR <obj> <fground> <bground> ==.
       END PROGRAM DSSetColor.


       *>
       *> SET-MOUSE-SHAPE object mouse-shape
       *>
       PROGRAM-ID. DSSetMouShape IS EXTERNAL.
       LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 shape                        PIC x(255).
       PROCEDURE DIVISION USING obj shape DELIMITED
       INVOKE AS ==SET-MOUSE-SHAPE <obj> <shape> ==.
       END PROGRAM DSSetMouShape.


       *>
       *> SET-OBJECT-LABEL object label-text
       *>
       PROGRAM-ID. DSSetObjLabel IS EXTERNAL.
       LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 otext                        PIC x(255).
       PROCEDURE DIVISION USING obj otext DELIMITED
       INVOKE AS ==SET-OBJECT-LABEL <obj> <otext> ==.
       END PROGRAM DSSetObjLabel.


       *>
       *> UPDATE-LIST-ITEM listbox item-text position
       *>
       PROGRAM-ID. DSUpdLstItm IS EXTERNAL.
       LINKAGE SECTION.
       01 obj                          PIC 9(8) COMP-X.
       01 itext                        PIC x(255).
       01 pos                          PIC 9(8) COMP-X.
       PROCEDURE DIVISION USING obj itext DELIMITED pos
       INVOKE AS ==UPDATE-LIST-ITEM <obj> <itext> <pos> ==.
       END PROGRAM DSUpdLstItm.

