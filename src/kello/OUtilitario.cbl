      $set mfoo
      $set ooctrl(+P)
       copy "windows.cpy".
       class-id. OUtilitario inherits from Base.

       object section.
       class-control.
            Icondata       is class "icondata"
            SaveDialog     is class "savedlg"
            font           is class "font"
            mouseCursor    is class "mousec"
            CharacterArray is class "chararry"
            openDialog     is class "opendlg"
            colordialog    is class "colrdlg"
            fontdialog     is class "fontdlg"
            OUtilitario    is class "outilitario"
            Window         is class "wclass"
            DialogBox      is class "dilogbox"
            ProgressBar    is class "progress"
            EFclass        is class "txtentry"
            MSExcel        is class "$OLE$Excel.Application".

       configuration section.
       special-names.   decimal-point  is    comma
                        call-convention 66 is wapi
                        call-convention 2 is dsdll
                        console        is      crt.

       working-storage section.
       object.
       object-storage section.

      *>--------------------------------------------------------------
       Method-id. "AnoMesDia".
       linkage section.
       01 lnk-data1.
          05 lnk-dia1          pic 9(02).
          05 lnk-mes1          pic 9(02).
          05 lnk-ano1          pic 9(04).

       01 lnk-data2.
          05 lnk-ano2          pic 9(04).
          05 lnk-mes2          pic 9(02).
          05 lnk-dia2          pic 9(02).

       procedure division using lnk-data1 returning lnk-data2.
           move lnk-dia1       to lnk-dia2
           move lnk-mes1       to lnk-mes2
           move lnk-ano1       to lnk-ano2
       exit method
       end method "AnoMesDia".

      *>--------------------------------------------------------------
       Method-id. "DiaMesAno".
       linkage section.
       01 lnk-data1.
          05 lnk-ano1          pic 9(04).
          05 lnk-mes1          pic 9(02).
          05 lnk-dia1          pic 9(02).

       01 lnk-data2.
          05 lnk-dia2          pic 9(02).
          05 lnk-mes2          pic 9(02).
          05 lnk-ano2          pic 9(04).

       procedure division using lnk-data1 returning lnk-data2.
           move lnk-dia1       to lnk-dia2
           move lnk-mes1       to lnk-mes2
           move lnk-ano1       to lnk-ano2
       exit method
       end method "DiaMesAno".

      *>--------------------------------------------------------------
       Method-id. "NumeroDias".
       linkage section.
       01 lnk-data.
           03 lnk-data1.
               05 lnk-dia1          pic 9(02).
               05 lnk-mes1          pic 9(02).
               05 lnk-ano1          pic 9(04).
           03 lnk-data2.
               05 lnk-dia2          pic 9(02).
               05 lnk-mes2          pic 9(02).
               05 lnk-ano2          pic 9(04).

       01  linka-difdias.
           05  link-databa         pic 9(08).
           05  link-databa-r       redefines link-databa.
               10 link-diaba       pic 9(02).
               10 link-mesba       pic 9(02).
               10 link-anoba       pic 9(04).
           05  link-datape         pic 9(08).
           05  link-datape-r       redefines link-datape.
               10 link-diape       pic 9(02).
               10 link-mespe       pic 9(02).
               10 link-anope       pic 9(04).
           05  link-diasca         pic 9(04).
           05  link-flag           pic x(01).


       01 numero-dias               pic 9(04).

       procedure division using lnk-data returning numero-dias.
           move lnk-data1   to link-databa
           move lnk-data2   to link-datape
           move zeros       to link-diasca

           call   "difdias" using linka-difdias
           cancel "difdias"

           move link-diasca to numero-dias

       exit method
       end method "NumeroDias".

      *>--------------------------------------------------------------

       Method-id. "fontDialog".
       local-storage section.
       77 lsFontBox object reference.
       77 lsFont object reference.
       linkage section.
       77 lnkWindow    object reference value null.
       77 lnkFont      object reference value null.
       77 lnkFontColor object reference value null.
       procedure division using lnkWindow lnkFont lnkFontColor.

          invoke fontdialog "new" using lnkWindow
                               returning lsFontBox
          invoke lsFontBox "create"

          invoke lsFontBox "getFont" returning lnkFont
          invoke lsFontBox "getColor" returning lnkFontColor

          invoke lsFontBox "finalize" returning lsFontBox

       exit method
       end method "fontDialog".

      *>--------------------------------------------------------------

       Method-id. "colorDialog".
       local-storage section.
       77 lsColorBox object reference.
       linkage section.
       77 lnkWindow object reference value null.
       01 lnkColor object reference value null.
       procedure division using lnkWindow returning lnkColor.

          invoke colordialog "new" using lnkWindow
                               returning lsColorBox

          invoke lsColorBox "create"
          invoke lsColorBox "getcolor" returning lnkColor
          invoke lsColorBox "finalize" returning lsColorBox

       exit method
       end method "colorDialog".

      *>--------------------------------------------------------------

       Method-id. "openDialog".
       local-storage section.
       77 lsOpenDialogBox object reference value null.
       77 lsFilter object reference value null.
       77 lsExtension object reference value null.
       77 lnkFileName object reference value null.
       linkage section.
       77 lnkWindow object reference value null.
       77 lnkString pic x(255).
       procedure division using lnkWindow returning lnkString.

          invoke opendialog "new" using lnkWindow
                               returning lsOpenDialogBox

          invoke lsOpenDialogBox "setTitleZ"
                 using z"Info Solutions... Desenvolvimento de Softwares"

          invoke CharacterArray "withValue" using z"Arquivo JPG (*.jpg)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.jpg"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using z"Arquivo BMP (*.bmp)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.bmp"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using z"Arquivo GIF (*.gif)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.gif"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using x"0000"
                 returning lsFilter
          invoke CharacterArray "withValue" using x"0000"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke lsOpenDialogBox "show"

          invoke lsOpenDialogBox "getFile" returning lnkFileName

          if lnkFileName not = null
             initialize lnkString
             invoke lnkFileName "getValue" returning lnkString
          end-if

          invoke lsOpenDialogBox "finalize" returning lsOpenDialogBox

          invoke lnkFileName "finalize" returning lnkFileName

       exit method
       end method "openDialog".

      *>--------------------------------------------------------------
       Method-id. "openDialogGeral".
       local-storage section.
       77 lsOpenDialogBox object reference value null.
       77 lsFilter        object reference value null.
       77 lsExtension     object reference value null.
       77 lnkFileName     object reference value null.
       linkage section.
       77 lnkWindow       object reference value null.
       77 lnkString       pic x(255).
       procedure division using lnkWindow returning lnkString.

          invoke opendialog "new" using lnkWindow
                               returning lsOpenDialogBox

          invoke lsOpenDialogBox "setTitleZ"
                 using z"Info Solutions... Desenvolvimento de Softwares"

          invoke CharacterArray "withValue" using z"Todos Arquivos *.*)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.*"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using x"0000"
                 returning lsFilter
          invoke CharacterArray "withValue" using x"0000"
                 returning lsExtension
          invoke lsOpenDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke lsOpenDialogBox "show"

          invoke lsOpenDialogBox "getFile" returning lnkFileName

          if lnkFileName not = null
             initialize lnkString
             invoke lnkFileName "getValue" returning lnkString
          end-if

          invoke lsOpenDialogBox "finalize" returning lsOpenDialogBox

          invoke lnkFileName "finalize" returning lnkFileName

       exit method
       end method "openDialogGeral".

      *>--------------------------------------------------------------

       Method-id. "progressBar".
       local-storage section.
       77 bHandle pic 9(09) comp-5 value zeros.
       77 wsMilisegundos pic 9(09) comp-5 value zeros.
       78 PBM-SETMARQUEE value 1034.
       78 PBS-MARQUEE value h"08".
       77 lsBegin pic 9(09) comp-5 value zeros.
       77 lsEnd pic 9(09) comp-5 value zeros.
       77 lsPositionY pic s9(09) comp-5 value zeros.
       77 lsPositionX pic s9(09) comp-5 value zeros.
       77 lswidthWindow pic s9(09) comp-5 value zeros.
       77 lsHeightWindow pic s9(09) comp-5 value zeros.
       77 lsWidth  pic s9(09) comp-5 value zeros.
       77 lsHeight pic s9(09) comp-5 value zeros.
       77 lsDialogBox object reference value null.
       77 resultado lresult.
       01 wsRemoveFlags.
          03 wsDwFlags   pic x(4) comp-5.
          03 wsDwExFlags pic x(4) comp-5.
       01 aFont                      object reference value null.
       linkage section.
       77 lnkWindow object reference value null.
       01 det-objetos.
          03 lnkProgressBar object reference value null.
          03 aEntryField object reference value null.
       procedure division using lnkWindow returning det-objetos.

           invoke DialogBox "new" using lnkWindow
                              returning lsDialogBox

           invoke EFclass "new" using lsDialogBox
                            returning aEntryField

           move 180 to lsPositionY
           move 140 to lsPositionX
           invoke aEntryField "setXY" using lsPositionX lsPositionY
           move 1400 to lsWidth
           move 70   to lsHeight
           invoke aEntryField "setWidthHeight" using lsWidth lsHeight
           invoke aEntryField "UpperCase"
           invoke aEntryField "readOnly"

           invoke self "fontBold" returning aFont

           invoke lsDialogBox "create"
           move 1800 to lsWidth
           move 300 to lsHeight
           invoke lsDialogBox "setWidthHeight" using lsWidth lsHeight
           invoke lsDialogBox "setTitleZ" using z"Por Favor Aguarde..."

           invoke lnkWindow "GetWidthHeight"
                  using lswidthWindow lsHeightWindow

           compute lsPositionX =
                   (lswidthWindow / 2) - (lsWidth / 2)

           compute lsPositionY =
                   (lsHeightWindow / 2) - (lsHeight / 2)

           invoke lsDialogBox "setXY" using lsPositionX lsPositionY

           invoke lsDialogBox "show"

           invoke ProgressBar "new" using lsDialogBox
                                returning lnkProgressBar

           invoke lnkProgressBar "Create"

           move 50 to lsPositionY
           invoke lnkProgressBar "setY" using lsPositionY
           move 140 to lsPositionY
           invoke lnkProgressBar "setX" using lsPositionY



           move 74 to lsHeight
           move 1500 to lsWidth
           invoke lnkProgressBar "setWidthHeight"
                  using lsWidth lsHeight

           invoke lnkProgressBar "show"

            invoke lnkProgressBar "AddBasicApiFlags"
                   using by value PBS-MARQUEE

           move zeros to wsDwFlags
           move WS-EX-CLIENTEDGE to wsDwExFlags
           invoke aEntryField "removeApiFlags" using wsRemoveFlags
           invoke aEntryField "setFont" using aFont
           invoke aEntryField "show"


            invoke lnkProgressBar "getSystemHandle" returning bHandle
            move 30 to wsMilisegundos
            call WAPI "SendMessageA" using by value bHandle
                                           by value PBM-SETMARQUEE
                                           by value 1
                                           by value wsMilisegundos
                                     returning resultado

       exit method
       end method "progressBar".

      *>--------------------------------------------------------------

       Method-id. "stopProgressBar".
       local-storage section.
       77 bHandle pic 9(09) comp-5 value zeros.
       77 wsMilisegundos pic 9(09) comp-5 value zeros.
       78 PBM-SETMARQUEE value 1034.
       77 resultado lresult.
       77 aWindow object reference value null.
       linkage section.
       01 det-objetos.
          03 lnkProgressBar object reference value null.
          03 aEntryField object reference value null.
       procedure division using det-objetos.

           invoke lnkProgressBar "getSystemHandle" returning bHandle
           move 0 to wsMilisegundos
           call WAPI "SendMessageA" using by value bHandle
                                          by value PBM-SETMARQUEE
                                          by value 0
                                          by value wsMilisegundos
                                returning resultado

           invoke lnkProgressBar "getAncestor" returning aWindow

           invoke lnkProgressBar "finalize" returning lnkProgressBar

           invoke aEntryField "finalize" returning aEntryField

           invoke aWindow "finalize" returning aWindow


       exit method
       end method "stopProgressBar".

      *>--------------------------------------------------------------

       Method-id. "cursorHand".
       local-storage section.
       77 lsIndex pic s9(09) comp-5 value zeros.
       78 IDC-HAND value 32649.
       linkage section.
       77 lnkMouseCursor object reference value null.
       procedure division returning lnkMouseCursor.

           invoke MouseCursor "new" returning lnkMouseCursor
           move IDC-HAND to lsIndex
           invoke lnkMouseCursor "initialize" using lsIndex

       exit method
       end method "cursorHand".

      *>--------------------------------------------------------------

       Method-id. "fontBold".
       local-storage section.
       77 font-size pic 9(9) comp-5 value zeros.
       77 theFontName  object reference value null.
       77 font-styles  PIC 9(9) COMP-5 value 1.
       77 lnkBool PIC 99 COMP-5.
       linkage section.
       77 lnkFont object reference value null.
       procedure division returning lnkFont.


          invoke CharacterArray "withValue" using z"Arial"
                 returning thefontname

           move "08" to font-size
           initialize font-styles

           invoke font "new" using thefontname
                                   font-size
                                   font-styles
                         returning lnkFont
          move 1 to lnkBool
          invoke lnkFont "bold" using lnkBool

       exit method
       end method "fontBold".

      *>--------------------------------------------------------------

       Method-id. "fontBoldName".
       local-storage section.
       77 font-size pic 9(9) comp-5 value zeros.
       77 theFontName  object reference value null.
       77 font-styles  PIC 9(9) COMP-5 value 1.
       77 lnkBool PIC 99 COMP-5.
       linkage section.
       77 lnkFontName pic x(255).
       77 lnkFont object reference value null.
       procedure division using lnkFontName returning lnkFont.


          string lnkFontName X"00" delimited by "  " into lnkFontName

          invoke CharacterArray "withValue" using lnkFontName
                 returning thefontname

           move "08" to font-size
           initialize font-styles

           invoke font "new" using thefontname
                                   font-size
                                   font-styles
                         returning lnkFont
          move 1 to lnkBool
          invoke lnkFont "bold" using lnkBool

       exit method
       end method "fontBoldName".

      *>--------------------------------------------------------------

       Method-id. "fontName".
       local-storage section.
       77 font-size pic 9(9) comp-5 value zeros.
       77 theFontName  object reference value null.
       77 font-styles  PIC 9(9) COMP-5 value 1.
       77 lnkBool PIC 99 COMP-5.
       linkage section.
       77 lnkFontName pic x(255).
       77 lnkFont object reference value null.
       procedure division using lnkFontName returning lnkFont.


          string lnkFontName X"00" delimited by "  " into lnkFontName

          invoke CharacterArray "withValue" using lnkFontName
                 returning thefontname

           move "08" to font-size
           initialize font-styles

           invoke font "new" using thefontname
                                   font-size
                                   font-styles
                         returning lnkFont
          move 1 to lnkBool

       exit method
       end method "fontName".

      *>--------------------------------------------------------------

       Method-id. "saveDialog".
       local-storage section.
       77 lsSaveDialogBox object reference value null.
       77 lsFilter object reference value null.
       77 lsExtension object reference value null.
       77 lnkFileName object reference value null.
       linkage section.
       77 lnkWindow object reference value null.
       77 lnkString pic x(255).
       procedure division using lnkWindow returning lnkString.

          invoke savedialog "new" using lnkWindow
                               returning lsSaveDialogBox

          invoke lsSaveDialogBox "setTitleZ"
                 using z"Info Solutions... Desenvolvimento de Softwares"

          invoke CharacterArray "withValue" using z"Arquivo JPG (*.jpg)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.jpg"
                 returning lsExtension
          invoke lsSaveDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using z"Arquivo BMP (*.bmp)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.bmp"
                 returning lsExtension
          invoke lsSaveDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using z"Arquivo GIF (*.gif)"
                 returning lsFilter
          invoke CharacterArray "withValue" using z"*.gif"
                 returning lsExtension
          invoke lsSaveDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke CharacterArray "withValue" using x"0000"
                 returning lsFilter
          invoke CharacterArray "withValue" using x"0000"
                 returning lsExtension
          invoke lsSaveDialogBox "AddFilter" using lsFilter lsExtension
          invoke lsFilter "finalize" returning lsFilter
          invoke lsExtension "finalize" returning lsExtension

          invoke lsSaveDialogBox "show"

          invoke lsSaveDialogBox "getFile" returning lnkFileName

          if lnkFileName not = null
             initialize lnkString
             invoke lnkFileName "getValue" returning lnkString
          end-if

          invoke lsSaveDialogBox "finalize" returning lsSaveDialogBox

          invoke lnkFileName "finalize" returning lnkFileName

       exit method
       end method "saveDialog".


      *>--------------------------------------------------------------


       Method-id. "exportarParaExcel".
       local-storage section.
       77 NumeroDeColunas  pic 9(9) comp-5.
       77 lsContador       pic 9(09).
       77 lsContadorEx     pic 9(09) comp-5.
       77 lsColunaString   pic x(60).
       77 lsTexto          pic x(60).
       77 lsLetra          pic x(01).
       77 lsLetraNulo      pic x(02).
       77 lsSizeLitView    pic 9(09) comp-5 value zeros.
       77 lsItem           pic 9(09) comp-5 value zeros.
       77 umItem           object reference value null.
       77 lsIndice         pic 9(09) comp-5 value zeros.
       77 lsIndiceColuna   pic 9(09) comp-5 value zeros.
       77 lsObjTexto       object reference value null.
       77 lsLinhaTexto     pic x(100) value spaces.
      ******************************************************************
      *            VARIAVEIS PARA O EXCEL
      ******************************************************************
       01 TituloRelatorio      pic x(50).
       01 indiceLinha          pic 9(06) value zeros.
       01 ExcelObject          object reference.
       01 WorkBooksCollection  object reference.
       01 WorkBook             object reference.
       01 Cell                 object reference.
       01 CellRange            object reference.
       01 ChartsCollection     object reference.
       01 Chart                object reference.
       01 WorkSheet            object reference.
       01 ActiveWindow         object reference.
       01 SelectedSheets       object reference.
       01 HPageBreaks          object reference.
       01 ws-arq-excel     pic x(50).
       01 status-code      pic x(02) comp-5.
       01 LoopCount        pic xx comp-5.

       01 status-call  pic 9(04) comp-x.
       01 filename1    pic x(255).
       01 filename2    pic x(255).

       01  ws-hora-sys                 pic 9(08).
       01  filler redefines ws-hora-sys.
           03 ws-ho-sys                pic 9(02).
           03 ws-mi-sys                pic 9(02).
           03 ws-se-sys                pic 9(02).
           03 ws-ms-sys                pic 9(02).

       01  WS-DATA-SYS.
           05 WS-DATA-CPU.
              10 WS-ANO-CPU            PIC 9(04).
              10 WS-MES-CPU            PIC 9(02).
              10 WS-DIA-CPU            PIC 9(02).
           05 FILLER                   PIC X(13).

       linkage section.
       77 lnkLista object reference value null.
       01 lnkArrayColunas.
          02 osNomeColuna pic x(060) occurs 100 times.
       01 lnkusu.
          copy "usuario.cpy".
       procedure division using lnkLista lnkArrayColunas lnkusu.

           ACCEPT WS-HORA-SYS FROM TIME
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           INITIALIZE FILENAME1 FILENAME2 STATUS-CALL
           MOVE "C:\PROGRAMA\EXPORTACAO" TO FILENAME2
           CALL "CBL_CREATE_DIR" USING FILENAME2
                             RETURNING STATUS-CALL

           MOVE SPACES TO WS-ARQ-EXCEL

           STRING "C:\PROGRAMA\EXPORTACAO\EXPORTACAO-"
                   WS-DIA-CPU"-"WS-MES-CPU"-"WS-ANO-CPU"-"
                   ws-hora-sys ".XLS"INTO WS-ARQ-EXCEL

           invoke lnkLista "numberOfColumns"
                  returning NumeroDeColunas

           invoke MSExcel "new" returning ExcelObject
           invoke ExcelObject "setVisible" using by value 1
           invoke ExcelObject "getWorkBooks"
                  returning WorkBooksCollection

           invoke WorkBooksCollection "Add" returning WorkBook

           initialize lsContador indiceLinha lsContadorEx
           perform numeroDeColunas times
             move 1    to indiceLinha
              add 1    to lsContador lsContadorEx
             move osNomeColuna(lsContador) to lsColunaString

             invoke ExcelObject "getCells" using by value indiceLinha
                                                 by value lsContadorEx
                                       returning Cell
             string lsColunaString x"00"
                    delimited by "  " into lsTexto
             invoke Cell "SetValue" using lsTexto
             invoke Cell "finalize" returning Cell

           end-perform

           initialize lsIndice lsSizeLitView lsIndiceColuna
           invoke lnkLista "size" returning lsSizeLitView

           perform until lsIndice = lsSizeLitView
               add 1 to lsIndice
               invoke lnkLista "itemAtIndex" using lsIndice
                                          returning umitem

               add 1 to indiceLinha
               initialize lsIndiceColuna
               perform until lsIndiceColuna = numeroDeColunas
                  add 1 to lsIndiceColuna
                  invoke umitem "getColumnValue" using lsIndiceColuna
                                                returning lsObjTexto
                  initialize lsTexto lsLinhaTexto
                  invoke lsObjTexto "getValue" returning lsTexto
                  string lsTexto x"00" delimited by "             "
                    into lsLinhaTexto

                  invoke ExcelObject "getCells" using
                                      by value indiceLinha
                                      by value lsIndiceColuna
                                      returning Cell

                  invoke Cell "SetValue" using lsLinhaTexto
                  invoke Cell "finalize" returning Cell

               end-perform
           end-perform

           initialize lsContador
           perform numeroDeColunas times
              add 1 to lsContador
              invoke self "TrazerLetra" using lsContador
                                    returning lsLetra
              string lsLetra x"00" into lsLetraNulo

              invoke ExcelObject "getColumns" using lsLetraNulo
                                          returning cellRange
              invoke cellRange "Select"
              invoke cellRange "AutoFit"
              invoke cellRange "finalize" returning cellRange

           end-perform

           invoke workBook "saveas" using ws-arq-excel

       exit method
       end method "exportarParaExcel".

      *>--------------------------------------------------------------

       method-id. "TrazerLetra".

       linkage section.
       01 lnkNumero pic 9(09).
       01 lnkLetra  pic x.
       procedure division using lnkNumero returning lnkLetra.

           evaluate lnkNumero
               when 1  move "A" to lnkLetra
               when 2  move "B" to lnkLetra
               when 3  move "C" to lnkLetra
               when 4  move "D" to lnkLetra
               when 5  move "E" to lnkLetra
               when 6  move "F" to lnkLetra
               when 7  move "G" to lnkLetra
               when 8  move "H" to lnkLetra
               when 9  move "I" to lnkLetra
               when 10 move "J" to lnkLetra
               when 11 move "K" to lnkLetra
               when 12 move "L" to lnkLetra
               when 13 move "M" to lnkLetra
               when 14 move "N" to lnkLetra
               when 15 move "O" to lnkLetra
               when 16 move "P" to lnkLetra
               when 17 move "Q" to lnkLetra
               when 18 move "R" to lnkLetra
               when 19 move "S" to lnkLetra
               when 20 move "T" to lnkLetra
               when 21 move "U" to lnkLetra
               when 22 move "V" to lnkLetra
               when 23 move "W" to lnkLetra
               when 24 move "X" to lnkLetra
               when 25 move "Y" to lnkLetra
               when 26 move "Z" to lnkLetra
           end-evaluate

       exit method
       end method "TrazerLetra".

      *>--------------------------------------------------------------

       Method-id. "statusBar".
       local-storage section.
       01 aStatusBar object reference value null.
       77 auxUsuario pic x(20).
       77 diaDaSemana  pic 9.
       77 extensoDiaDaSemana pic x(13).
       77 dataSemana pic x(27) .
       77 hBarra pic 9(09) comp-5 value zeros.
       77 hWindow pic 9(09) comp-5 value zeros.
       77 wWindow pic 9(09) comp-5 value zeros.
       77 wsSecao pic 9(09) comp-5 value zeros.
       77 wsLargura pic 9(09) comp-5 value zeros.
       77 wsTotal pic 9(09) comp-5 value zeros.
       77 umTexto object reference value null.
       77 sHandle pic 9(09) comp-5 value zeros.
       77 umIcone object reference value null.
       77 iHandle pic 9(09) comp-5 value zeros.
       78 SB-SETICON value 1039.

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu             pic 9(04).
             10 ws-mes-cpu             pic 9(02).
             10 ws-dia-cpu             pic 9(02).
          05 filler                    pic x(13).

       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       01 aWindow object reference value null.
       procedure division using lnkusu aWindow.

          invoke aWindow "addStatusbar" returning aStatusBar
          invoke aStatusBar "create"

          invoke aWindow "getHeight" returning hWindow
          invoke aStatusbar "getHeight" returning hBarra
          add 100 to hbarra
          invoke aStatusbar "setHeight" using hBarra
          invoke aWindow "setHeight" using hWindow

          move 3 to wsTotal
          invoke aStatusBar "setNumberOfParts" using wsTotal

          invoke aWindow "getWidth" returning wWindow

          move 1 to wsSecao
          compute wsLargura = wWindow * (50,0 / 100)
          invoke aStatusBar "setSectionWidth" using wsSecao wsLargura

          move 2 to wsSecao
          compute wsLargura = wWindow * (24,0 / 100)
          invoke aStatusBar "setSectionWidth" using wsSecao wsLargura

          move 3 to wsSecao
          compute wsLargura = wWindow * (26,0 / 100)
          invoke aStatusBar "setSectionWidth" using wsSecao wsLargura
      *>------------------------------------------------- 1º Secao

          invoke CharacterArray "withValue"
                 using z' Info Solutions - (43) 3025 2595'
          returning umTexto
          move 1 to wsSecao
          invoke aStatusbar "setSectionText" using wsSecao umTexto
          invoke umTexto "finalize" returning umTexto

          invoke IconData "fromFileZ" using z"info.ico"
          returning umIcone
          invoke umIcone "getID" returning iHandle

          move 0 to wsSecao
          invoke aStatusbar "getID" returning sHandle
          call WAPI "SendMessageA" using by value sHandle
                                         by value SB-SETICON size 4
                                         by value wsSecao
                                         by value iHandle

      *>------------------------------------------------- 2º Secao

          invoke IconData "fromFileZ" using z"usu.ico"
          returning umIcone
          invoke umIcone "getID" returning iHandle

          move 1 to wsSecao
          invoke aStatusbar "getID" returning sHandle
          call WAPI "SendMessageA" using by value sHandle
                                         by value SB-SETICON size 4
                                         by value wsSecao
                                         by value iHandle

          string lnk-usuario X"00" delimited by "  " into auxUsuario

          invoke CharacterArray "withValue" using auxUsuario
                                            returning umTexto
          move 2 to wsSecao
          invoke aStatusbar "setSectionText" using wsSecao umTexto
          invoke umTexto "finalize" returning umTexto

      *>------------------------------------------------- 3º Secao

          accept diaDaSemana from day-of-week

          evaluate diaDaSemana
             when 1 move "Segunda-Feira" to extensoDiaDaSemana
             when 2 move "Terça-Feira"   to extensoDiaDaSemana
             when 3 move "Quarta-Feira"  to extensoDiaDaSemana
             when 4 move "Quinta-Feira"  to extensoDiaDaSemana
             when 5 move "Sexta-Feira"   to extensoDiaDaSemana
             when 6 move "Sabado"        to extensoDiaDaSemana
             when 7 move "Domingo"       to extensoDiaDaSemana
          end-evaluate

          move function current-date to ws-data-sys

          String ws-dia-cpu "/" ws-mes-cpu"/" ws-ano-cpu " - "
             extensoDiaDaSemana x"00"
             into dataSemana

          invoke CharacterArray "withValue" using dataSemana
                                            returning umTexto
          move 3 to wsSecao
          invoke aStatusbar "setSectionText" using wsSecao umTexto
          invoke umTexto "finalize" returning umTexto


       exit method
       end method "statusBar".
      *>--------------------------------------------------------------
       Method-id. "ExecutarArquivo".
       local-storage section.

       77 lsHand               pic x(04) comp-x.
       77 lsOperacao           PIC X(255).
       77 lsParametros         PIC X(255).
       77 lsDiretorio          PIC X(255).
       77 lsShow               PIC 9(09) comp-5.

       linkage section.
       77 lnkRetorno           PIC s9(09) comp-5.
       77 lnkArquivo           pic x(255).
       procedure division using lnkArquivo returning lnkRetorno.
          CALL "SHELL32"
          MOVE 0                  TO lsHand
          MOVE z"Open"            TO lsOperacao
          MOVE x"00"              TO lsParametros lsDiretorio
          MOVE 1                  TO lsShow

          string lnkArquivo x"00" delimited by "   " into lnkArquivo

           call wapi ShellExecute using by value lsHand
                                  by reference lsOperacao
                                  by reference lnkArquivo
                                  by reference LsParametros
                                  by reference LsDiretorio
                                  by value     lsShow
                                  returning lnkRetorno

       exit method
       end method "ExecutarArquivo".

      *>--------------------------------------------------------------
       Method-id. "ExecutarPrograma".
       local-storage section.

       01 command-line2        pic x(494).
       01 command-line2-len    pic x(4) comp-5.
       01 run-unit-id          pic x(8) comp-5.
       01 stack-size           pic x(4) comp-5.
       01 flags                pic x(4) comp-5.
       01 tty-cmd              pic x(8).
       01 tty-cmd-len          pic x(4) comp-5.

       linkage section.
       77 lnkPrograma          PIC x(8).
       01 lnkusu.
           copy "usuario.cpy".
       01 status-code          pic 9(5) comp-5.
       procedure division using lnkPrograma lnkusu
                      returning status-code.

           move lnkPrograma         to command-line2
           move lnkusu              to command-line2(10: 484)
           move 494                 to command-line2-len

           call "CBL_EXEC_RUN_UNIT" using        command-line2
                                    by value     command-line2-len
                                    by reference run-unit-id
                                    by value     stack-size
                                                 flags
                                    by reference tty-cmd
                                    by value     tty-cmd-len
                                    returning    status-code
           end-call

       exit method
       end method "ExecutarPrograma".

       end object.
       end class OUtilitario.
