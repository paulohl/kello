       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP060.
      *DATA: 12/01/2001
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELAÇÃO DE ESTOQUES DISPONÍVEIS
       ENVIRONMENT DIVISION.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CAPX004.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX023.
           COPY MTPX060.
           COPY CAPX010.
           COPY COPX001.
           COPY COPX040.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS NR-ALBUM-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = CLIENTE-WK
                      WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = CIDADE-WK
                     VISITA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = UF-WK
                     VISITA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = MESANO-WK
                     CIDADE-WK NR-ALBUM-WK
                  ALTERNATE RECORD KEY IS ALT5-WK = VISITA-WK
                     CIDADE-WK WITH DUPLICATES.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CONTRATO-WK2.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW004.
       COPY MTPW002.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW023.
       COPY MTPW060.
       COPY CAPW010.
       COPY COPW001.
       COPY COPW040.
           COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  NR-ALBUM-WK         PIC 9(8).
           05  ESTOJO-WK           PIC 9(2).
           05  ENCADERN-WK         PIC 9(2).
           05  FOLHA-WK            PIC 9(4).
           05  FOTO-WK             PIC 9(4).
           05  POSTER-WK           PIC 9(2).
           05  FITA-WK             PIC 9(2).
           05  DVD-WK              PIC 9(2).
           05  PFITA-WK            PIC 9(2).
           05  PDVD-WK             PIC 9(2).
           05  FOTO-CD-WK          PIC 9(2).
           05  MOLDURA-WK          PIC 9(2).
           05  CLIENTE-WK          PIC X(30).
           05  CIDADE-WK           PIC X(13).
           05  FONE-WK             PIC 9(8).
           05  UF-WK               PIC XX.
           05  MESANO-WK           PIC 9(6).
           05  VISITA-WK           PIC 9.
           05  POSSE-WK            PIC 9.
           05  DESC-POSSE-WK       PIC X(6).
           05  BOOK-WK             PIC 9(2).

       FD  WORK2.
       01  REG-WORK2.
           05  CONTRATO-WK2        PIC 9(4).
           05  ESTOJO-WK2          PIC 9(4).
           05  ENCADERNACAO-WK2    PIC 9(4).
           05  FOTOS-WK2           PIC 9(8).
           05  FITAS-WK2           PIC 9(6).
           05  DVD-WK2             PIC 9(6).
           05  FOTO-CD-WK2         PIC 9(6).
           05  POSTER-WK2          PIC 9(4).
           05  DATA-MOV-WK2        PIC 9(8).
           05  BOOK-WK2            PIC 9(4).
           05  NR-FORMANDOS-WK2    PIC 9(5).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP060.CPB".
           COPY "MTP060.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD023             PIC XX       VALUE SPACES.
           05  ST-MTD060             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 9999     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  QTDE-E6               PIC ZZ.ZZ9.
           05  QTDE-E9               PIC Z.ZZZ.ZZ9.
           05  QTDE-E10              PIC ZZ.ZZZ.ZZ9.
           05  QT-ALBUM              PIC 9(5)     VALUE ZEROS.
           05  DATA-INI              PIC 9(08)    VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)    VALUE ZEROS.
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 9(2).
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
      *    OPCAO INDIVIDUAL
           05  GRAVAR-OPCAO          PIC 9        VALUE ZEROS.
           05  CODIGO-W              PIC 9(2)     VALUE ZEROS.
           05  UF-W                  PIC XX       VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-ESTOJO          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ENCADERNACAO    PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FOLHAS          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTOS           PIC 9(8)     VALUE ZEROS.
           05  TOTAL-POSTER          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FITAS           PIC 9(7)     VALUE ZEROS.
           05  TOTAL-DVD             PIC 9(7)     VALUE ZEROS.
           05  TOTAL-PFITAS          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-PDVD            PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTO-CD         PIC 9(7)     VALUE ZEROS.
           05  TOTAL-MOLDURA         PIC 9(7)     VALUE ZEROS.
           05  TOTAL-BOOK            PIC 9(7)     VALUE ZEROS.
           05  QTDE-E                PIC Z.ZZZ.ZZZ.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  VARIA-W2              PIC 9(08)    VALUE ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 lnkusu.
          copy usuario.cpy.

       01 WSSIZE                       pic 9(009) comp-5 value zeros.
       01 WSINDICE                     pic 9(009) comp-5 value zeros.
       01 UMITEM                       object reference value null.
       01 UMOBJETO                     object reference value null.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       01 indice              pic 9(02).
       01 lnktabela.
          02 lnkobjetoscol  object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela2.
          02 lnkobjetoscol2  object reference occurs 99 times.
       01 lnktabelaCol2.
          02 lnkcolunas2   pic 9(09) comp-5 value zeros occurs 99 times.


       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(70)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(08)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC zzz9    VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "RELACAO DE ESTOQUES DISPONIVEIS    -ORDEM: ".
           05  ORDEM-REL           PIC X(24)   VALUE SPACES.
           05  FILLER              PIC X(03)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.MES/ANO...: ".
           05  MESANO-INI-REL        PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL        PIC 99/9999.
           05  FILLER              PIC X(02).
           05  DETALHE-REL         PIC X(20).
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(113)  VALUE
           "NR-ALBUM ES EN FOLH FOTO PO FI PF DV PD FC MD BK CLIENTE
      -    "          CIDADE        FONE     UF MES/ANO V L POSSE".
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(114)  VALUE
           "QT.CLI ESTOJO ENCADERN. TOT-FOLHAS TOT-FOTOS POSTER TOT-FITA
      -    "S PORTA-FITA TOT-DVD TOT-PDV  TOT-FC TOT-MOLD TOT-BOOK".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(150)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINTOT-REL          PIC X(150)  VALUE SPACES.



       01  CAB01-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL-a       PIC X(47)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL-a       PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL-a          PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(08)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL-a            PIC zzZ9    VALUE ZEROS.
       01  CAB02-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "RELACAO DE ESTOQUES DISPONIVEIS    -ORDEM: ".
           05  ORDEM-REL-a         PIC X(24)   VALUE SPACES.
           05  FILLER              PIC X(03)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.MES/ANO...: ".
           05  MESANO-INI-REL-a    PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL-a    PIC 99/9999.
       01  CAB02-b.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(16)
               VALUE "INT.ROMANEIO..: ".
           05  ROMANEIO-INI        PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  ROMANEIO-FIM        PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X(03).
           05  DET-TIPO            PIC X(10).
           05  CIDADE-REL          PIC X(30).
           05  FILLER              PIC X(03).
       01  CAB02-c.
           05  FILLER              PIC X(09)
               VALUE "LOCAL..: ".
           05  LOCAL-REL           PIC X(40).
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  DETALHES-REL        PIC X(100)  VALUE SPACES.

       01  CAB03-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05 FILLER              PIC X(107)  VALUE ALL "=".

       01  CAB03-ar.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05 FILLER              PIC X(107)  VALUE ALL "=".

       01  CAB04-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(107)  VALUE
           "CONT QCLI ESTOJO ENCADERN QT.FOTOS QT.FITAS QT.POSTER QT.DVD
      -    " QT.FTCD QT.BK MEDIA F/A MEDIA F/CLI DATA-MONT".

       01  CAB04-ar.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05 FILLER               PIC X(107)  VALUE
           "CONT QCLI ESTOJO ENCADERN QT.FOTOS QT.FITAS QT.POSTER QT.DVD
      -    " QT.FTCD QT.BK MEDIA F/A MEDIA F/CLI DATA-MONT".

       01  DET-01-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05 DET-CONTRATO-a       PIC 9(04).
           05 FILLER               PIC X(01).
           05 DET-NRCLIENTE-a      PIC ZZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTESTOJO-a       PIC ZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTENCARD-a       PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFOTOS-a        PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFITAS-a        PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTPOSTER-a       PIC Z.ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTDVD-a          PIC ZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFTCD-a         PIC ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTBK-a           PIC Z.ZZ9.
           05 FILLER               PIC X(02).
           05 DET-MEDIA-ALB-a      PIC Z.ZZZ,99.
           05 FILLER               PIC X(04).
           05 DET-MEDIA-CLI-a      PIC Z.ZZZ,99.
           05 FILLER               PIC X(01).
           05 DET-DATA-MONT-a      PIC 99/99/9999.

       01  LINDET-a.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL-a       PIC X(107)   VALUE SPACES.

       01  CAB05a-a.
           05  FILLER             PIC X(05).
           05  FILLER             PIC X(13)
               VALUE " T. ESTOJOS".
           05  FILLER             PIC X(13)
               VALUE "  T. ENCARD".
           05  FILLER             PIC X(13)
               VALUE "   T. FOTOS".
           05  FILLER             PIC X(13)
               VALUE "   T. FITAS".
           05  FILLER             PIC X(13)
               VALUE "  T. POSTER".
           05  FILLER             PIC X(13)
               VALUE "T. CONTRATO".

       01  CAB05b-a.
           05  FILLER             PIC X(05).
           05  FILLER             PIC X(13)
               VALUE "     T. DVD".
           05  FILLER             PIC X(13)
               VALUE " T. FOTO CD".
           05  FILLER             PIC X(12)
               VALUE "   T. BOOKS".
           05  FILLER             PIC X(12)
               VALUE " T.FORMANDOS".
           05  FILLER             PIC X(13)
               VALUE "    MEDIA F/A".
           05  FILLER             PIC X(13)
               VALUE " MEDIA CLI".

       01  LINTOTa-a.
           05  FILLER             PIC X(05).
           05  LINTOT-ESTOJO-a     PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-ENCARD-a     PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FOTOS-a      PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FITAS-a      PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-POSTER-a     PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-CONTRATO-a   PIC ZZZ.ZZZ.ZZ9.

       01  LINTOTb-a.
           05 FILLER               PIC X(05).
           05 LINTOT-DVD-a         PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-FOTOCD-a      PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-BOOKS-a       PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(05).
           05 LINTOT-FORMANDOS-a   PIC ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-MEDIA-a       PIC ZZZ.ZZ9,99.
           05 FILLER               PIC X(02).
           05 LINTOT-MEDIA-CLI-a   PIC ZZZ.ZZ9,99.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".

           MOVE HORA-REL       TO HORA-REL-A
           MOVE EMISSAO-REL    TO EMISSAO-REL-a


           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL-A

           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD023"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD023.
           MOVE "MTD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD060.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN INPUT CAD010 COD040 MTD019 MTD020 MTD002 CGD001 MTD023
                      COD001.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN I-O   MTD060
           CLOSE      MTD060
           OPEN INPUT MTD060.

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD002 <> "00"
              MOVE "ERRO ABERTURA MTD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD023 <> "00"
              MOVE "ERRO ABERTURA MTD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD060 <> "00"
              MOVE "ERRO ABERTURA MTD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP060"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM VERIFICAR-SENHA-STATUS
                    PERFORM CRIAR-LISTVIEW
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    IF GS-SOMENTE-FOGO = 1
                       MOVE 1 TO GS-FOGO
                    END-IF
                    PERFORM GRAVA-WORK
                    IF GS-SOMENTE-FOGO = 1
                       MOVE 0 TO GS-FOGO
                    END-IF
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-INDIVIDUAL-TRUE
                    PERFORM LER-CODIGOS
               WHEN GS-OPCAO-POP-UP-TRUE
                    PERFORM LER-POPUP
               WHEN GS-VERIFICA-POSSE-TRUE
                    PERFORM VERIFICA-POSSE
               WHEN GS-LER-POSSE-TRUE
                    PERFORM LER-POSSE
               WHEN GS-POPUP-POSSE-TRUE
                    PERFORM POPUP-POSSE
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CRIAR-LISTVIEW SECTION.
           PERFORM CRIAR-LISTVIEW-ANALIT
           PERFORM CRIAR-LISTVIEW-SINTET
       CRIAR-LISTVIEW-FIM.
           EXIT.

       CRIAR-LISTVIEW-ANALIT SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-acp-listview-analit "adicionarColunaZ"
            using z"Descrição" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)


       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB13
           NOT INVALID KEY
               ENABLE-OBJECT PB13.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    MTD060
           OPEN I-O MTD060

           INITIALIZE REG-MTD060
           START MTD060 KEY IS NOT LESS CODIGO-MTD060 INVALID KEY
                MOVE "10" TO ST-MTD060.
           PERFORM UNTIL ST-MTD060 = "10"
                READ MTD060 NEXT AT END
                     MOVE "10" TO ST-MTD060
                NOT AT END
                     DELETE MTD060 INVALID KEY
                         MOVE "Erro de Exclusão...MTD060" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-MTD060
               WRITE REG-MTD060
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      MTD060
           OPEN INPUT MTD060.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-MTD060
           START MTD060 KEY IS NOT LESS CODIGO-MTD060 INVALID KEY
               MOVE "10" TO ST-MTD060.

           PERFORM UNTIL ST-MTD060 = "10"
               READ MTD060 NEXT AT END
                    MOVE "10" TO ST-MTD060
               NOT AT END
                    MOVE CODIGO-MTD060 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM


           IF ACHEI = "N"
              CLOSE      MTD060
              OPEN I-O   MTD060
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-MTD060
                        WRITE REG-MTD060

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      MTD060
              OPEN INPUT MTD060.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       VERIFICA-POSSE SECTION.
           MOVE GS-POSSE(1: 1)  TO GS-POSSE-W.
       LER-POSSE SECTION.
           IF GS-POSSE-W = 1
              MOVE GS-CODIGO-POSSE   TO CODIGO-MT02
              READ MTD002 INVALID KEY MOVE SPACES TO NOME-MT02
              END-READ
              MOVE NOME-MT02         TO GS-DESC-POSSE
            ELSE
              MOVE GS-CODIGO-POSSE   TO CODIGO-CG01
              READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01         TO GS-DESC-POSSE
           END-IF.

       LER-CODIGOS SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE GS-CODIGO TO CIDADE
                    READ CAD010 INVALID KEY
                         MOVE SPACES TO NOME-COMPL-CID
                    END-READ
                    MOVE NOME-COMPL-CID TO GS-DESCRICAO
             WHEN 1 CONTINUE
             WHEN 2 CONTINUE
             WHEN 3 MOVE GS-CODIGO  TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY
                         MOVE SPACES TO IDENTIFICACAO-CO40
                    END-READ
                    MOVE IDENTIFICACAO-CO40 TO GS-DESCRICAO
             WHEN 5 MOVE GS-CODIGO TO CIDADE
                    READ CAD010 INVALID KEY
                         MOVE SPACES TO NOME-COMPL-CID
                    END-READ
                    MOVE NOME-COMPL-CID TO GS-DESCRICAO
           END-EVALUATE.
       POPUP-POSSE SECTION.
           IF GS-POSSE-W = 1
              CALL   "MTP002T" USING PARAMETROS-W PASSAR-STRING-1
              CANCEL "MTP002T"
              MOVE PASSAR-STRING-1(16: 2) TO GS-CODIGO-POSSE
              MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-POSSE
           ELSE
              CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
              CANCEL "CGP001T"
              MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO-POSSE
              MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-POSSE
           END-IF.

       LER-POPUP SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP010T"
                    MOVE PASSAR-STRING-1(1: 30)   TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(35: 4)   TO GS-CODIGO

             WHEN 1 CONTINUE
             WHEN 2 CONTINUE
             WHEN 3 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 11)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(52: 4)   TO GS-CODIGO
             WHEN 5 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP010T"
                    MOVE PASSAR-STRING-1(1: 30)   TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(35: 4)   TO GS-CODIGO
           END-EVALUATE.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE WORK   DELETE FILE WORK
              CLOSE WORK2  DELETE FILE WORK2.

           ACCEPT VARIA-W  FROM TIME.

           ACCEPT VARIA-W2 FROM TIME.
           ADD 100 TO VARIA-W2

           OPEN OUTPUT WORK WORK2.
           CLOSE       WORK WORK2.
           OPEN I-O    WORK WORK2.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-INI  TO MESANO-INI-REL
                                  MESANO-INI-REL-A
                                  MESANO-W
           MOVE MES-WW           TO MES-II
           MOVE ANO-WW           TO ANO-II
           MOVE MESANO-I         TO MESANO-INI

           MOVE GS-MESANO-FIM    TO MESANO-W
                                    MESANO-FIM-REL
                                    MESANO-FIM-REL-A
           MOVE MES-WW           TO MES-II
           MOVE ANO-WW           TO ANO-II
           MOVE MESANO-I         TO MESANO-FIM
           MOVE ZEROS            TO QT-ALBUM.

           MOVE GS-DATA-INI      TO ROMANEIO-INI
           MOVE GS-DATA-FIM      TO ROMANEIO-FIM

           IF GS-OPCAO-INDIVIDUAL = 3
              MOVE   "CONTRATO: " TO DET-TIPO
           ELSE
              MOVE   "CIDADE..: " TO DET-TIPO
           END-IF

           MOVE SPACES           TO CIDADE-REL
           STRING GS-CODIGO "-" GS-DESCRICAO " "
                  GS-UF DELIMITED BY "   " INTO CIDADE-REL

           MOVE SPACES           TO LOCAL-REL
           STRING GS-POSSE " " GS-CODIGO-POSSE "-" GS-DESC-POSSE
                DELIMITED BY "  " INTO LOCAL-REL

           MOVE ";" TO DETALHES-REL
           IF GS-VISITA1 = 1
              STRING "1.VISITA, ;" INTO DETALHES-REL
           END-IF
           IF GS-VISITA2 = 1
              STRING "2.VISITA, ;" INTO DETALHES-REL
           END-IF
           IF GS-FOGO = 1
              STRING "CONSIDERAR FOGO, ;" INTO DETALHES-REL
           END-IF
           IF GS-SOMENTE-FOGO = 1
              STRING "SOMENTE FOGO, ;" INTO DETALHES-REL
           END-IF

           IF DETALHES-REL = ";"
              MOVE SPACES TO DETALHE-REL
           END-IF

      *    VERIFICA OPCAO DO RELATORIO - INDIV OU GERAL

           INITIALIZE REG-MTD020

           IF GS-OPCAO-INDIVIDUAL = 5
              PERFORM MESANO-CONTRATO
           ELSE
              IF GS-OPCAO-INDIVIDUAL = 4
                 MOVE GS-DATA-INI TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV    TO DATA-INI

                 MOVE GS-DATA-FIM TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV    TO DATA-FIM

                 MOVE DATA-INI         TO DATAROMANEIO-MTG
                 MOVE ZEROS            TO ALBUM-MTG
      *          DISPLAY "DATA INICIAL = " DATA-INI STOP "  "
      *          DISPLAY "DATA FINAL   = " DATA-FIM STOP "  "
                 START MTD020 KEY IS NOT LESS ALT-MTG INVALID KEY
                       MOVE "10" TO ST-MTD020
                 END-START
              ELSE
                 IF GS-OPCAO-INDIVIDUAL < 3
                    MOVE MESANO-INI      TO ANOMES-VISITA-MTG
                    START MTD020 KEY IS NOT < ANOMES-VISITA-MTG
                          INVALID KEY MOVE "10" TO ST-MTD020
                    END-START
                 ELSE
                    MOVE GS-CODIGO   TO ALBUM-MTG(1: 4)
                    MOVE ZEROS       TO ALBUM-MTG(5: 4)
                    START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                          MOVE "10"  TO ST-MTD020
                    END-START
                 END-IF
              END-IF

              PERFORM UNTIL ST-MTD020 = "10"
                READ MTD020 NEXT RECORD AT END
                     MOVE "10" TO ST-MTD020
                 NOT AT END
                  IF GS-OPCAO-INDIVIDUAL = 4

      *              DISPLAY "DATAROMANEIO-MTG = " DATAROMANEIO-MTG
      *              STOP "  "

                     IF DATAROMANEIO-MTG > DATA-FIM
      *                 DISPLAY "SAI" STOP " 1"
                        MOVE "10" TO ST-MTD020
                     ELSE
                        IF NAO-GEROU-ALBUM-MTG <> 1
      *                    DISPLAY "ENTREI " STOP " "
                           PERFORM MOVER-DADOS-WORK
                        END-IF
                     END-IF
                  ELSE
                     IF GS-OPCAO-INDIVIDUAL < 3
                        IF ANOMES-VISITA-MTG > MESANO-FIM
                           MOVE "10" TO ST-MTD020
                        ELSE
                           IF NAO-GEROU-ALBUM-MTG <> 1
                              PERFORM MOVER-DADOS-WORK
                           END-IF
                        END-IF
                     ELSE
                        IF CONTRATO-MTG <> GS-CODIGO
                           MOVE "10" TO ST-MTD020
                        ELSE
                           IF NAO-GEROU-ALBUM-MTG <> 1
                              PERFORM MOVER-DADOS-WORK
                           END-IF
                        END-IF
                     END-IF
                  END-IF
                END-READ
              END-PERFORM
              MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       MESANO-CONTRATO SECTION.
           STRING GS-MESANO-INI(3:4) GS-MESANO-INI(1:2) INTO MESANO-INI
           STRING GS-MESANO-FIM(3:4) GS-MESANO-FIM(1:2) INTO MESANO-FIM

           INITIALIZE REG-COD040
           MOVE MESANO-INI      TO MESANO-PREV-CO40
           START COD040 KEY IS NOT < ALT1-CO40
                 INVALID KEY MOVE "10" TO ST-COD040
           END-START
           PERFORM UNTIL ST-COD040 = "10"
               READ COD040 NEXT AT END
                   MOVE "10" TO ST-COD040
               NOT AT END
                   IF MESANO-PREV-CO40 > MESANO-FIM
                      MOVE "10" TO ST-COD040
                   ELSE
                      MOVE CIDADE-CO40 TO CIDADE
                      READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                      END-READ
                      IF GS-CODIGO = 0 OR CIDADE-CO40
                         IF GS-UF = SPACES OR UF-CID
                            PERFORM PESQUISAR-STATUS
                            IF ACHEI = "S"
                               INITIALIZE REG-MTD020
                               MOVE NR-CONTRATO-CO40 TO CONTRATO-MTG
                               START MTD020 KEY IS NOT LESS ALBUM-MTG
                                                             INVALID KEY
                                     MOVE "10" TO ST-MTD020
                               END-START
                               PERFORM UNTIL ST-MTD020 = "10"
                                     READ MTD020 NEXT AT END
                                          MOVE "10" TO ST-MTD020
                                     NOT AT END
                                          IF NR-CONTRATO-CO40 <>
                                             CONTRATO-MTG
                                             MOVE "10" TO ST-MTD020
                                          ELSE
                                             IF NAO-GEROU-ALBUM-MTG <> 1
                                                PERFORM MOVER-DADOS-WORK
                                             END-IF
                                          END-IF
                                     END-READ
                               END-PERFORM
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


       MOVER-DADOS-WORK SECTION.
           MOVE ALBUM-MTG          TO ALBUM-MT19 GS-EXIBE-VENCTO.
           MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           READ MTD019 INVALID KEY
                INITIALIZE REG-MTD019.

           PERFORM VERIFICA-OPCAO
      *    SE GS-POSSE-W = 0(GERAL) CONTINUAR, SENAO VERIFICAR SE
      *    POSSE IGUAL A SOLICITADA

           IF GS-POSSE-W > 0 AND POSSE-MTG <> GS-POSSE-W
              MOVE 0 TO GRAVAR-OPCAO.

      *    SE GS-POSSE-W <>0(GERAL) E GS-CODIGO-POSSE <>0(GERAL)
      *    ENTAO CODIGO-POSSE-MTG TEM QUE SER IGUAL AO SOLICITADO
           IF GS-POSSE-W <> 0 AND GS-CODIGO-POSSE <> 0
              IF CODIGO-POSSE-MTG <> GS-CODIGO-POSSE
                 MOVE 0 TO GRAVAR-OPCAO.


      *    DISPLAY "GRAVAR-OPCAO 4 = " GRAVAR-OPCAO STOP " "

           IF GS-SOMENTE-FOGO = 0
              IF GS-VISITA1 = 0 AND GS-VISITA2 = 0
                 MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 5 = " GRAVAR-OPCAO STOP " "


           IF GS-SOMENTE-FOGO = 0
              IF GS-VISITA1 = 1 AND GS-VISITA2 = 1
                 CONTINUE
              ELSE
                 MOVE ALBUM-MTG TO ALBUM-MTG3
                 IF GS-VISITA1 = 1
                    READ MTD023 NOT INVALID KEY
                         IF DATA-MTG3 > 0
                            MOVE 0 TO GRAVAR-OPCAO
                         END-IF
                    END-READ
                 ELSE
                    READ MTD023 INVALID KEY
                         MOVE 0 TO GRAVAR-OPCAO
                    NOT INVALID KEY
                         IF DATA-MTG3 = 0
                            MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 6 = " GRAVAR-OPCAO STOP " "

           IF GS-FOGO = 1 AND (FOGO-MTG = 1 or 7 or 8)
              MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 7 = " GRAVAR-OPCAO STOP " "

           IF GS-FOGO = 0 AND FOGO-MTG <> 0
              MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 8 = " GRAVAR-OPCAO STOP " "
      *    DISPLAY "FOGO-MTG = " FOGO-MTG STOP " "

           IF GS-SOMENTE-FOGO = 1
              IF FOGO-MTG <> 9
                 MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 9 = " GRAVAR-OPCAO STOP " "

           IF GS-OPCAO-INDIVIDUAL <> 5
              MOVE CONTRATO-MTG         TO NR-CONTRATO-CO40
              READ COD040 INVALID KEY
                   INITIALIZE REG-COD040
              END-READ

              PERFORM PESQUISAR-STATUS
              IF ACHEI = "N"
                 MOVE 0 TO GRAVAR-OPCAO.

      *    DISPLAY "GRAVAR-OPCAO 10 = " GRAVAR-OPCAO STOP " "

           IF GRAVAR-OPCAO = 1
              INITIALIZE REG-WORK
              MOVE ALBUM-MTG         TO NR-ALBUM-WK
              MOVE QT-ESTOJO-MTG     TO ESTOJO-WK
              MOVE QT-ENCADER-MTG    TO ENCADERN-WK
              MOVE QT-FOLHAS-MTG     TO FOLHA-WK
              MOVE QT-FOTOS-MTG      TO FOTO-WK
              MOVE QT-POSTER-MTG     TO POSTER-WK
              MOVE QT-FITAS-MTG      TO FITA-WK
              MOVE QT-DVD-MTG        TO DVD-WK
              MOVE QT-PORTA-FITA-MTG TO PFITA-WK
              MOVE QT-PORTA-DVD-MTG  TO PDVD-WK
              MOVE QT-FOTO-CD-MTG    TO FOTO-CD-WK
              MOVE QT-MOLDURA-MTG    TO MOLDURA-WK
              MOVE QT-BOOK-MTG       TO BOOK-WK

              MOVE POSSE-MTG         TO POSSE-WK
              EVALUATE POSSE-MTG
                WHEN 1 MOVE CODIGO-POSSE-MTG TO CODIGO-MT02
                       READ MTD002 INVALID KEY
                            MOVE SPACES TO NOME-MT02
                       END-READ
                       MOVE NOME-MT02        TO DESC-POSSE-WK
                WHEN 2 MOVE CODIGO-POSSE-MTG TO CODIGO-CG01
                       READ CGD001 INVALID KEY
                            MOVE SPACES TO NOME-MT02
                       END-READ
                       MOVE NOME-CG01        TO DESC-POSSE-WK
                WHEN 3 MOVE SPACES           TO DESC-POSSE-WK
              END-EVALUATE
              MOVE NOME-FORM-MT19    TO CLIENTE-WK
              MOVE CIDADE-MT19       TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE SPACES TO NOME-COMPL-CID
              END-READ
              MOVE NOME-CID          TO CIDADE-WK
              MOVE FONE-MT19         TO FONE-WK
              MOVE UF-MT19           TO UF-WK
              MOVE ANOMES-VISITA-MTG TO MESANO-WK
              MOVE VISITA-MTG        TO VISITA-WK
              IF NRALBUM-MTG <> ZEROS
                 ADD 1 TO QT-ALBUM
              END-IF
              WRITE REG-WORK
              END-WRITE

      ***********Dados Sintético

              INITIALIZE REG-WORK2
              MOVE CONTRATO-MTG         TO CONTRATO-WK2
              READ WORK2 INVALID KEY
                   PERFORM MOVER-DADOS-WORK2
              NOT INVALID KEY
                   PERFORM SOMAR-DADOS-WORK2
              END-READ
           END-IF.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       MOVER-DADOS-WORK2 SECTION.
           MOVE 1                  TO NR-FORMANDOS-WK2
           MOVE QT-ESTOJO-MTG      TO ESTOJO-WK2
           MOVE QT-ENCADER-MTG     TO ENCADERNACAO-WK2
           MOVE QT-FOTOS-MTG       TO FOTOS-WK2
           MOVE QT-FITAS-MTG       TO FITAS-WK2
           MOVE QT-DVD-MTG         TO DVD-WK2
           MOVE QT-BOOK-MTG        TO BOOK-WK2
           MOVE QT-FOTO-CD-MTG     TO FOTO-CD-WK2
           MOVE QT-POSTER-MTG      TO POSTER-WK2
           MOVE DATAMOV-MTG        TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-MOV-WK2.
           WRITE REG-WORK2.

       SOMAR-DADOS-WORK2 SECTION.
           ADD  1                    TO NR-FORMANDOS-WK2
           ADD  QT-ESTOJO-MTG        TO ESTOJO-WK2
           ADD  QT-ENCADER-MTG       TO ENCADERNACAO-WK2
           ADD  QT-FOTOS-MTG         TO FOTOS-WK2
           ADD  QT-FITAS-MTG         TO FITAS-WK2
           ADD  QT-DVD-MTG           TO DVD-WK2
           ADD  QT-POSTER-MTG        TO POSTER-WK2
           ADD  QT-FOTO-CD-MTG       TO FOTO-CD-WK2
           ADD  QT-BOOK-MTG          TO BOOK-WK2
           REWRITE REG-WORK2.

       VERIFICA-OPCAO SECTION.
           MOVE GS-CODIGO TO CODIGO-W
           MOVE 0 TO GRAVAR-OPCAO
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 IF GS-CODIGO = CIDADE-MT19 OR 0
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 1 IF GS-UF = UF-MT19 OR SPACES
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 2 MOVE 1 TO GRAVAR-OPCAO
             WHEN 3 IF GS-CODIGO = CONTRATO-MTG
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 4 MOVE 1 TO GRAVAR-OPCAO
             WHEN 5 MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.
      *-----------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.

           PERFORM TOTALIZA

      *Dados Sintetico
           MOVE CAB04-A(6:107) TO GS-LINDET
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03-A(6:107) TO GS-LINDET
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLOSE      WORK2
           OPEN INPUT WORK2

           PERFORM UNTIL ST-WORK2 = "10"
               READ WORK2 NEXT RECORD AT END
                    MOVE "10" TO ST-WORK2
               NOT AT END
                    PERFORM MOVER-DADOS-LINDET2
                    ADD NR-FORMANDOS-WK2     TO GS-TOT-FORMANDOS
                    ADD ESTOJO-WK2           TO GS-TOT-ESTOJO
                    ADD ENCADERNACAO-WK2     TO GS-TOT-ENCADERNACAO
                    ADD FOTOS-WK2            TO GS-TOT-FOTOS
                    ADD FITAS-WK2            TO GS-TOT-FITAS
                    ADD DVD-WK2              TO GS-TOT-DVD
                    ADD FOTO-CD-WK2          TO GS-TOT-FOTO-CD
                    ADD POSTER-WK2           TO GS-TOT-POSTER
                    ADD BOOK-WK2             TO GS-TOT-BOOK
                    ADD 1                    TO GS-TOT-CONTRATO
                    MOVE "INSERE-LIST2"      TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           PERFORM TOTALIZA2.
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET2 SECTION.
           MOVE CONTRATO-WK2        TO DET-CONTRATO-A
           MOVE NR-FORMANDOS-WK2    TO DET-NRCLIENTE-A
           MOVE ESTOJO-WK2          TO DET-QTESTOJO-A
           MOVE ENCADERNACAO-WK2    TO DET-QTENCARD-A
           MOVE FOTOS-WK2           TO DET-QTFOTOS-A
           MOVE FITAS-WK2           TO DET-QTFITAS-A
           MOVE POSTER-WK2          TO DET-QTPOSTER-A
           MOVE DVD-WK2             TO DET-QTDVD-A
           MOVE FOTO-CD-WK2         TO DET-QTFTCD-A
           MOVE BOOK-WK2            TO DET-QTBK-A
           COMPUTE DET-MEDIA-CLI-A = FOTOS-WK2 / NR-FORMANDOS-WK2
           COMPUTE DET-MEDIA-ALB-A = FOTOS-WK2 / ENCADERNACAO-WK2
           MOVE DATA-MOV-WK2        TO DET-DATA-MONT-A

           MOVE DET-01-A(6:200)     TO GS-LINDET.

       TOTALIZA2 SECTION.
           COMPUTE  GS-MEDIA-W-CLI = GS-TOT-FOTOS / GS-TOT-FORMANDOS.
           COMPUTE  GS-MEDIA-W-FA  = GS-TOT-FOTOS / GS-TOT-ENCADERNACAO.

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "ALBUM         " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NR-ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE       " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE/VISITA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "ESTADO/VISITA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "MESANO/CIDADE " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "VISITA/CIDADE " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT5-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE NR-ALBUM-WK            TO GS-LINDET(1: 9)
           MOVE ESTOJO-WK              TO GS-LINDET(10: 3)
           ADD ESTOJO-WK               TO TOTAL-ESTOJO
           MOVE ENCADERN-WK            TO GS-LINDET(13: 3)
           ADD ENCADERN-WK             TO TOTAL-ENCADERNACAO
           MOVE FOLHA-WK               TO GS-LINDET(16: 5)
           ADD FOLHA-WK                TO TOTAL-FOLHAS
           MOVE FOTO-WK                TO GS-LINDET(21: 5)
           ADD FOTO-WK                 TO TOTAL-FOTOS
           MOVE POSTER-WK              TO GS-LINDET(26: 3)
           ADD POSTER-WK               TO TOTAL-POSTER
           MOVE FITA-WK                TO GS-LINDET(29: 3)
           ADD FITA-WK                 TO TOTAL-FITAS
           MOVE PFITA-WK               TO GS-LINDET(32: 3)
           ADD PFITA-WK                TO TOTAL-PFITAS
           MOVE DVD-WK                 TO GS-LINDET(35: 3)
           ADD  DVD-WK                 TO TOTAL-DVD
           MOVE PDVD-WK                TO GS-LINDET(38:3)
           ADD  PDVD-WK                TO TOTAL-PDVD
           MOVE FOTO-CD-WK             TO GS-LINDET(41:3)
           ADD  FOTO-CD-WK             TO TOTAL-FOTO-CD
           MOVE MOLDURA-WK             TO GS-LINDET(44:3)
           ADD  MOLDURA-WK             TO TOTAL-MOLDURA
           MOVE BOOK-WK                TO GS-LINDET(47:3)
           ADD  BOOK-WK                TO TOTAL-BOOK

           MOVE CLIENTE-WK             TO GS-LINDET(50: 21)
           MOVE CIDADE-WK              TO GS-LINDET(72: 14)
           MOVE FONE-WK                TO GS-LINDET(86: 9)
           MOVE UF-WK                  TO GS-LINDET(95: 3)
           MOVE MESANO-WK              TO MESANO-I
           MOVE ANO-II                 TO ANO-WW
           MOVE MES-II                 TO MES-WW
           MOVE MESANO-W               TO MESANO-E
           MOVE MESANO-E               TO GS-LINDET(98: 8)
           MOVE VISITA-WK              TO GS-LINDET(106: 2).
           MOVE POSSE-WK               TO GS-LINDET(108: 2)
           MOVE DESC-POSSE-WK          TO GS-LINDET(110: 6).
       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-ESTOJO TOTAL-ENCADERNACAO TOTAL-FOTOS
                         TOTAL-FITAS TOTAL-POSTER TOTAL-PFITAS
                         TOTAL-FOLHAS TOTAL-DVD TOTAL-PDVD TOTAL-FOTO-CD
                         TOTAL-MOLDURA TOTAL-BOOK

           MOVE ZEROS TO GS-TOT-ESTOJO GS-TOT-ENCADERNACAO GS-TOT-FOTOS
                         GS-TOT-FITAS GS-TOT-POSTER GS-TOT-DVD
                         GS-TOT-FOTO-CD GS-TOT-BOOK GS-TOT-FORMANDOS
                         GS-TOT-CONTRATO.
       TOTALIZA SECTION.
           MOVE SPACES                 TO GS-LINTOT
           MOVE QT-ALBUM               TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(1: 7)
           MOVE TOTAL-ESTOJO           TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(8: 7)
           MOVE TOTAL-ENCADERNACAO     TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(15: 10)
           MOVE TOTAL-FOLHAS           TO QTDE-E10
           MOVE QTDE-E10               TO GS-LINTOT(25: 11)
           MOVE TOTAL-FOTOS            TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(36: 10)
           MOVE TOTAL-POSTER           TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(46: 7)
           MOVE TOTAL-FITAS            TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(53: 10)
           MOVE TOTAL-PFITAS           TO QTDE-E10
           MOVE QTDE-E10               TO GS-LINTOT(63: 10)
           MOVE TOTAL-DVD              TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(76: 07)
           MOVE TOTAL-PDVD             TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(83:07)
           MOVE TOTAL-FOTO-CD          TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(91:07)
           MOVE TOTAL-MOLDURA          TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(100:07)
           MOVE TOTAL-BOOK             TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(108:07)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP060" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           IF GS-SOMENTE-FOGO = 1
              MOVE "SOMENTE FOGO" TO DETALHE-REL
           ELSE
              MOVE SPACES TO DETALHE-REL.

           COPY CONDENSA.

           EVALUATE GS-QUAL-TAB
               WHEN 1 PERFORM IMPRIMIR-ANALITICO
               WHEN 2 PERFORM IMPRIMIR-SINTETICO
           END-EVALUATE

           COPY DESCONDENSA.

       IMPRIMIR-ANALITICO SECTION.
           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
                READ WORK NEXT RECORD AT END
                     MOVE "10" TO ST-WORK
                NOT AT END
                     PERFORM MOVER-DADOS-RELATORIO
                END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           PERFORM TOTALIZA-REL.

       IMPRIMIR-SINTETICO SECTION.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO2.

           INITIALIZE REG-WORK
                      GS-TOT-FORMANDOS
                      GS-TOT-ESTOJO
                      GS-TOT-ENCADERNACAO
                      GS-TOT-FOTOS
                      GS-TOT-FITAS
                      GS-TOT-DVD
                      GS-TOT-FOTO-CD
                      GS-TOT-POSTER
                      GS-TOT-BOOK
                      GS-TOT-CONTRATO




           MOVE ZEROS TO CONTRATO-WK2.
           START WORK2 KEY IS NOT < CONTRATO-WK2 INVALID KEY
                 MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
                 READ WORK2 NEXT RECORD AT END
                    MOVE "10" TO ST-WORK2
                 NOT AT END
                    PERFORM MOVER-DADOS-RELATORIO2
                 END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL2.

       CABECALHO2 SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL-A.

           IF PAG-W NOT < GS-PAGINA
              IF LIN = 1
                 WRITE REG-RELAT FROM CAB01-A AFTER 0
              ELSE
                 WRITE REG-RELAT FROM CAB01-A AFTER PAGE
              END-IF
              WRITE REG-RELAT FROM CAB02-A AFTER 2
              WRITE REG-RELAT FROM CAB02-B
              WRITE REG-RELAT FROM CAB02-C
              WRITE REG-RELAT FROM CAB03-Ar
              MOVE SPACES  TO REG-RELAT
              MOVE CAB04-A TO REG-RELAT(6:130)
              WRITE REG-RELAT FROM CAB04-Ar
              WRITE REG-RELAT FROM CAB03-Ar.

           MOVE 8 TO LIN.

       MOVER-DADOS-RELATORIO2 SECTION.
           PERFORM MOVER-DADOS-LINDET2.
           ADD NR-FORMANDOS-WK2     TO GS-TOT-FORMANDOS
           ADD ESTOJO-WK2           TO GS-TOT-ESTOJO
           ADD ENCADERNACAO-WK2     TO GS-TOT-ENCADERNACAO
           ADD FOTOS-WK2            TO GS-TOT-FOTOS
           ADD FITAS-WK2            TO GS-TOT-FITAS
           ADD DVD-WK2              TO GS-TOT-DVD
           ADD FOTO-CD-WK2          TO GS-TOT-FOTO-CD
           ADD POSTER-WK2           TO GS-TOT-POSTER
           ADD BOOK-WK2             TO GS-TOT-BOOK
           ADD 1                    TO GS-TOT-CONTRATO
           MOVE GS-LINDET           TO LINDET-REL.


           IF PAG-W NOT < GS-PAGINA
              WRITE REG-RELAT FROM LINDET.


           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO2.

       TOTALIZA-REL2 SECTION.

           IF PAG-W NOT < GS-PAGINA
              WRITE REG-RELAT FROM CAB05a-A AFTER 2.

           MOVE GS-TOT-ESTOJO       TO LINTOT-ESTOJO-A
           MOVE GS-TOT-ENCADERNACAO TO LINTOT-ENCARD-A
           MOVE GS-TOT-FOTOS        TO LINTOT-FOTOS-A
           MOVE GS-TOT-FITAS        TO LINTOT-FITAS-A
           MOVE GS-TOT-POSTER       TO LINTOT-POSTER-A
           MOVE GS-TOT-CONTRATO     TO LINTOT-CONTRATO-A
           MOVE GS-TOT-DVD          TO LINTOT-DVD-A
           MOVE GS-TOT-FOTO-CD      TO LINTOT-FOTOCD-A
           MOVE GS-TOT-BOOK         TO LINTOT-BOOKS-A
           MOVE GS-TOT-FORMANDOS    TO LINTOT-FORMANDOS-A
           MOVE GS-MEDIA-W-FA       TO LINTOT-MEDIA-A
           MOVE GS-MEDIA-W-CLI      TO LINTOT-MEDIA-CLI-A

           IF PAG-W NOT < GS-PAGINA
              WRITE REG-RELAT FROM LINTOTa-A
              WRITE REG-RELAT FROM CAB05b-A
              WRITE REG-RELAT FROM LINTOTb-A.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

      *    WRITE REG-RELAT FROM LINDET.

           IF PAG-W NOT < GS-PAGINA
              WRITE REG-RELAT FROM LINDET.

           ADD 1 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES                 TO LINTOT-REL
           MOVE QT-ALBUM               TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(1: 7)
           MOVE TOTAL-ESTOJO           TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(8: 7)
           MOVE TOTAL-ENCADERNACAO     TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(15: 10)
           MOVE TOTAL-FOLHAS           TO QTDE-E10
           MOVE QTDE-E10               TO LINTOT-REL(25: 11)
           MOVE TOTAL-FOTOS            TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(36: 10)
           MOVE TOTAL-POSTER           TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(46: 7)
           MOVE TOTAL-FITAS            TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(53: 10)
           MOVE TOTAL-PFITAS           TO QTDE-E10
           MOVE QTDE-E10               TO LINTOT-REL(63: 10).
           MOVE TOTAL-DVD              TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(76: 07)
           MOVE TOTAL-PDVD             TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(83:07)
           MOVE TOTAL-FOTO-CD          TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(91:07)
           MOVE TOTAL-MOLDURA          TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(100:07)
           MOVE TOTAL-BOOK             TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(108:07)

           IF PAG-W NOT < GS-PAGINA
              WRITE REG-RELAT FROM LINTOT.

           ADD 1 TO LIN.

           IF PAG-W NOT < GS-PAGINA
              MOVE SPACES       TO LINTOT-REL
              WRITE REG-RELAT FROM LINTOT.

           ADD 1 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.

      *    display "PAG-W = " PAG-W " GS-PAGINA = " GS-PAGINA stop " "

           IF PAG-W NOT < GS-PAGINA
      *    display "entrei" stop " "
              IF LIN = 1
                 WRITE REG-RELAT FROM CAB01 AFTER 0
              ELSE
                 WRITE REG-RELAT FROM CAB01 AFTER PAGE
              END-IF

              WRITE REG-RELAT FROM CAB02 AFTER 2
              WRITE REG-RELAT FROM CAB02-B
              WRITE REG-RELAT FROM CAB02-C
              WRITE REG-RELAT FROM CAB03
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB03.

           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 MTD019
                 MTD020 MTD002 CGD001
                 MTD023 COD001 MTD060


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP060"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE WORK   DELETE FILE WORK
           CLOSE WORK2  DELETE FILE WORK2

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
