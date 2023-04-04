       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP202.
      *DATA: 21/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: ACERTO DE DEVOLUÇÃO DE VENDAS DE VENDEDOR
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX001.
           COPY MTPX020.
           COPY RCPX100.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = VENDEDOR-WK ALBUM-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CGPW001.
       COPY MTPW020.
       COPY RCPW100.

       FD  WORK.
       01  REG-WORK.
           05  VENDEDOR-WK         PIC 9(6).
           05  ALBUM-WK            PIC 9(8).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(155).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP202.CPB".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  VENDEDOR-ANT          PIC 9(6)     VALUE ZEROS.
           05  FOTOS-W               PIC 9(6)     VALUE ZEROS.
           05  ALBUM-E               PIC 9999.9999.
           05  QTDE-E11              PIC ZZ-.
           05  QTDE-E1               PIC ZZZZ-.
           05  QTDE-E2               PIC ZZZZZ-.
           05  QTDE-E22              PIC ZZZZ-.
           05  QTDE-E55              PIC ZZZZ-.
           05  QTDE-E551             PIC ZZZ.
           05  QTDE-E333             PIC ZZ-.
           05  QTDE-E33              PIC ZZZ-.
           05  QTDE-E3               PIC ZZZZ-.
           05  QTDE-E4               PIC ZZZZZZ-.
           05  QTDE-E5               PIC ZZZZZZZ-.
           05  ALBUMW.
               10 CONT-W             PIC 9(4).
               10 ALB-W              PIC 9(4).
           05  ALBUM-W REDEFINES ALBUMW PIC 9(8).

           05  SALDO-W               PIC S9(7)     VALUE ZEROS.
           05  TOT-EST-M             PIC 9(3)     VALUE ZEROS.
           05  TOT-ENC-M             PIC 9(3)     VALUE ZEROS.
           05  TOT-FOLHA-M           PIC 9(5)     VALUE ZEROS.
           05  TOT-FOTO-M            PIC 9(5)     VALUE ZEROS.
           05  TOT-FITA-M            PIC 9(4)     VALUE ZEROS.
           05  TOT-DVD-M             PIC 9(4)     VALUE ZEROS.
           05  TOT-PFITA-M           PIC 9(3)     VALUE ZEROS.
           05  TOT-POSTER-M          PIC 9(3)     VALUE ZEROS.
           05  TOT-PORTA-DVD-M       PIC 9(3)     VALUE ZEROS.
           05  TOT-FOTO-CD-M         PIC 9(3)     VALUE ZEROS.
           05  TOT-MOLDURA-M         PIC 9(3)     VALUE ZEROS.
           05  TOT-EST-V             PIC 9(5)     VALUE ZEROS.
           05  TOT-ENC-V             PIC 9(5)     VALUE ZEROS.
           05  TOT-FOLHA-V           PIC 9(7)     VALUE ZEROS.
           05  TOT-FOTO-V            PIC 9(7)     VALUE ZEROS.
           05  TOT-FOTO-COM-V        PIC 9(7)     VALUE ZEROS.
           05  TOT-FITA-V            PIC 9(4)     VALUE ZEROS.
           05  TOT-DVD-V             PIC 9(4)     VALUE ZEROS.
           05  TOT-PFITA-V           PIC 9(6)     VALUE ZEROS.
           05  TOT-POSTER-V          PIC 9(4)     VALUE ZEROS.
           05  TOT-PORTA-DVD-V       PIC 9(4)     VALUE ZEROS.
           05  TOT-FOTO-CD-V         PIC 9(4)     VALUE ZEROS.
           05  TOT-MOLDURA-V         PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-EST-M         PIC 9(5)     VALUE ZEROS.
           05  TOT-GER-ENC-M         PIC 9(5)     VALUE ZEROS.
           05  TOT-GER-FOLHA-M       PIC 9(7)     VALUE ZEROS.
           05  TOT-GER-FOTO-M        PIC 9(7)     VALUE ZEROS.
           05  TOT-GER-FITA-M        PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-DVD-M         PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-PFITA-M       PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-POSTER-M      PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-PORTA-DVD-M   PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-FOTO-CD-M     PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-MOLDURA-M     PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-EST-V         PIC 9(5)     VALUE ZEROS.
           05  TOT-GER-ENC-V         PIC 9(5)     VALUE ZEROS.
           05  TOT-GER-FOLHA-V       PIC 9(7)     VALUE ZEROS.
           05  TOT-GER-FOTO-V        PIC 9(7)     VALUE ZEROS.
           05  TOT-GER-FOTO-COM-V    PIC 9(7)     VALUE ZEROS.
           05  TOT-GER-FITA-V        PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-DVD-V         PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-PFITA-V       PIC 9(6)     VALUE ZEROS.
           05  TOT-GER-POSTER-V      PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-PORTA-DVD-V   PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-FOTO-CD-V     PIC 9(4)     VALUE ZEROS.
           05  TOT-GER-MOLDURA-V     PIC 9(4)     VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(40).
           05  LIN                   PIC 9(02)    VALUE ZEROS.

       01  TIPO-MSG                  PIC X(01).
       01  RESP-MSG                  PIC X(01).
       01  MENSAGEM                  PIC X(200).

           COPY "LNKPRINT".

           COPY "PARAMETR".
       01  CAB01.
           05  EMPRESA-REL         PIC X(66)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(77)   VALUE
           "ACERTO DE DEVOLUCAO DE VENDAS DE VENDEDOR".
           05  FILLER              PIC X(11)   VALUE "INT.MOVTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(155) VALUE
           "I---------I-------------------------------------------I-----
      -    "------------------------------------------I-----------------
      -    "-------------------------I".

       01  CAB04.
           05  FILLER              PIC X(155) VALUE
           "I         I                 MONTAGEM                  I
      -    "               VENDAS                     I
      -    " SALDO                   I".

       01  CAB05.
           05  FILLER              PIC X(155) VALUE
           "INR-ALBUM IEST ENC FOL FOTOS POS FIT PFI DV PDV FC MD IEST E
      -    "NC FOL FOTOS COM POS FIT PFI DV PDV FC MD IEST ENC FOL FOTOS
      -    " POS FIT PFI DV PDV FC MDI".

       01 CAB05d.
          05 filler                pic x(01) value "I".
          05 det-contrato          pic 9(08) BLANK WHEN ZEROS.
          05 filler                pic x(02) value " I".
          05 det-est-mtg           pic zzz.
          05 filler                pic x(01).
          05 det-encard-mtg        pic zzz.
          05 filler                pic x(01).
          05 det-folha-mtg         pic zzz.
          05 filler                pic x(01).
          05 det-fotos-mtg         pic zzzzz.
          05 filler                pic x(01).
          05 det-pos-mtg           pic zzz.
          05 filler                pic x(01).
          05 det-fita-mtg          pic zzz.
          05 filler                pic x(01).
          05 det-pfita-mtg         pic zzz.
          05 filler                pic x(01).
          05 det-dvd-mtg           pic zz.
          05 filler                pic x(01).
          05 det-pdvd-mtg          pic zzz.
          05 filler                pic x(01).
          05 det-fc-mtg            pic zz.
          05 filler                pic x(01).
          05 det-md-mtg            pic zz.
          05 filler                pic x(02) value " I".
          05 det-est-ven           pic zzz.
          05 filler                pic x(01).
          05 det-encard-ven        pic zzz.
          05 filler                pic x(01).
          05 det-folha-ven         pic zzz.
          05 filler                pic x(01).
          05 det-fotos-ven         pic zzzzz.
          05 filler                pic x(01).
          05 det-com-ven           pic zzz.
          05 filler                pic x(01).
          05 det-pos-ven           pic zzz.
          05 filler                pic x(01).
          05 det-fita-ven          pic zzz.
          05 filler                pic x(01).
          05 det-pfita-ven         pic zzz.
          05 filler                pic x(01).
          05 det-dvd-ven           pic zz.
          05 filler                pic x(01).
          05 det-pdvd-ven          pic zzz.
          05 filler                pic x(01).
          05 det-fc-ven            pic zz.
          05 filler                pic x(01).
          05 det-md-ven            pic zz.
          05 filler                pic x(02) value " I".
          05 det-est-sld           pic zzz.
          05 filler                pic x(01).
          05 det-encard-sld        pic zzz.
          05 filler                pic x(01).
          05 det-folha-sld         pic zzz.
          05 filler                pic x(01).
          05 det-fotos-sld         pic zzzzz.
          05 filler                pic x(01).
          05 det-pos-sld           pic zzz.
          05 filler                pic x(01).
          05 det-fita-sld          pic zzz.
          05 filler                pic x(01).
          05 det-pfita-sld         pic zzz.
          05 filler                pic x(01).
          05 det-dvd-sld           pic zz.
          05 filler                pic x(01).
          05 det-pdvd-sld          pic zzz.
          05 filler                pic x(01).
          05 det-fc-sld            pic zz.
          05 filler                pic x(01).
          05 det-md-sld            pic zz.
          05 filler                pic x(01) value "I".

       01  LINDET.
           05  LINDET-REL          PIC X(155) VALUE SPACES.
       01  CAB06.
           05  FILLER              PIC X(62)  VALUE
           "-------------------------- MONTAGEM ------------------------
      -    "--".
       01  CAB07.
           05  FILLER              PIC X(66)  VALUE
           "---------------------------- VENDAS ------------------------
      -    "------".
       01  CAB08.
           05  FILLER              PIC X(62)  VALUE
           "---------------------------- SALDO -------------------------
      -    "--".
       01  CAB10.
           05  FILLER              PIC X(62)  VALUE
           "ESTOJ ENCAD  FOLHAS   FOTOS POS FITAS PFIT DVD PDVD FTCD MOL
      -    "D".
       01  CAB11.
           05  FILLER              PIC X(62)  VALUE
           "============================================================
      -    "==".
       01  CAB11A.
           05  FILLER              PIC X(64)  VALUE
           "============================================================
      -    "====".

       01  CAB10D.
           05 DET-TOT-EST           PIC ZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-ENCARD        PIC ZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FOLHAS        PIC ZZZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FOTOS         PIC ZZZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-POSTER        PIC ZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FITAS         PIC ZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-PFITA         PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-DVD           PIC ZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-PDVD          PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FTCD          PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-MOLD          PIC ZZZZ.

       01  CAB10A.
           05  FILLER              PIC X(66)  VALUE
           "ESTOJ ENCAD  FOLHAS   FOTOS COM POS  FITAS PFIT DVD PDVD FTC
      -    "D MOLD".

       01  CAB10D2.
           05 DET-TOT-EST2          PIC ZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-ENCARD2       PIC ZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FOLHAS2       PIC ZZZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FOTOS2        PIC ZZZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-COMIS2        PIC ZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-POSTER2       PIC ZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FITAS2        PIC ZZZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-PFITA2        PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-DVD2          PIC ZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-PDVD2         PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-FTCD2         PIC ZZZZ.
           05 FILLER                PIC X(01).
           05 DET-TOT-MOLD2         PIC ZZZZ.

       01  LINTOT1.
           05  LINTOT1-REL          PIC X(80)  VALUE SPACES.
       01  LINTOT2.
           05  LINTOT2-REL          PIC X(80)  VALUE SPACES.
       01  LINTOT3.
           05  LINTOT3-REL          PIC X(80)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE NOME-EMP           TO EMPRESA-REL
           MOVE EMPRESA            TO EMP-REC
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001 RCD100 MTD020.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR    TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01      TO GS-NOME-VENDEDOR.
       POPUP-VENDEDOR SECTION.
           CALL "CGP001T" USING PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6)  TO GS-VENDEDOR
           MOVE PASSAR-STRING-1(39: 30) TO GS-NOME-VENDEDOR.

      *--------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.

           MOVE ZEROS TO SALDO-W
                         TOT-EST-M
                         TOT-ENC-M
                         TOT-FOLHA-M
                         TOT-FOTO-M
                         TOT-FITA-M
                         TOT-DVD-M
                         TOT-PFITA-M
                         TOT-POSTER-M
                         TOT-PORTA-DVD-M
                         TOT-FOTO-CD-M
                         TOT-MOLDURA-M
                         TOT-EST-V
                         TOT-ENC-V
                         TOT-FOLHA-V
                         TOT-FOTO-V
                         TOT-FOTO-COM-V
                         TOT-FITA-V
                         TOT-DVD-V
                         TOT-PFITA-V
                         TOT-POSTER-V
                         TOT-PORTA-DVD-V
                         TOT-FOTO-CD-V
                         TOT-MOLDURA-V
                         TOT-GER-EST-M
                         TOT-GER-ENC-M
                         TOT-GER-FOLHA-M
                         TOT-GER-FOTO-M
                         TOT-GER-FITA-M
                         TOT-GER-DVD-M
                         TOT-GER-PFITA-M
                         TOT-GER-POSTER-M
                         TOT-GER-PORTA-DVD-M
                         TOT-GER-FOTO-CD-M
                         TOT-GER-MOLDURA-M
                         TOT-GER-EST-V
                         TOT-GER-ENC-V
                         TOT-GER-FOLHA-V
                         TOT-GER-FOTO-V
                         TOT-GER-FOTO-COM-V
                         TOT-GER-FITA-V
                         TOT-GER-DVD-V
                         TOT-GER-PFITA-V
                         TOT-GER-POSTER-V
                         TOT-GER-PORTA-DVD-V
                         TOT-GER-FOTO-CD-V
                         TOT-GER-MOLDURA-V
                         VENDEDOR-ANT

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.

           MOVE VECTO-INI    TO DATA-MOVTO-REC.
           MOVE ZEROS        TO ALBUM-REC.
           START RCD100 KEY IS NOT < ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
            READ RCD100 NEXT RECORD AT END MOVE "10" TO ST-RCD100
              NOT AT END
                 IF DATA-MOVTO-REC > VECTO-FIM MOVE "10" TO ST-RCD100
                 ELSE
                  MOVE DATA-MOVTO-REC TO GS-EXIBE-MOVTO
                  MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  IF GS-VENDEDOR <> ZEROS
                     IF VENDEDOR-REC <> GS-VENDEDOR
                        CONTINUE
                     ELSE PERFORM GRAVA-DADOS-WORK
                     END-IF
                  ELSE PERFORM GRAVA-DADOS-WORK
                  END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-DADOS-WORK SECTION.
           MOVE ALBUM-REC        TO ALBUM-WK.
           MOVE VENDEDOR-REC     TO VENDEDOR-WK.
           WRITE REG-WORK.
      *-------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO SALDO-W
                         TOT-EST-M
                         TOT-ENC-M
                         TOT-FOLHA-M
                         TOT-FOTO-M
                         TOT-FITA-M
                         TOT-DVD-M
                         TOT-PFITA-M
                         TOT-POSTER-M
                         TOT-PORTA-DVD-M
                         TOT-FOTO-CD-M
                         TOT-MOLDURA-M
                         TOT-EST-V
                         TOT-ENC-V
                         TOT-FOLHA-V
                         TOT-FOTO-V
                         TOT-FOTO-COM-V
                         TOT-FITA-V
                         TOT-DVD-V
                         TOT-PFITA-V
                         TOT-POSTER-V
                         TOT-PORTA-DVD-V
                         TOT-FOTO-CD-V
                         TOT-MOLDURA-V
                         TOT-GER-EST-M
                         TOT-GER-ENC-M
                         TOT-GER-FOLHA-M
                         TOT-GER-FOTO-M
                         TOT-GER-FITA-M
                         TOT-GER-DVD-M
                         TOT-GER-PFITA-M
                         TOT-GER-POSTER-M
                         TOT-GER-PORTA-DVD-M
                         TOT-GER-FOTO-CD-M
                         TOT-GER-MOLDURA-M
                         TOT-GER-EST-V
                         TOT-GER-ENC-V
                         TOT-GER-FOLHA-V
                         TOT-GER-FOTO-V
                         TOT-GER-FOTO-COM-V
                         TOT-GER-FITA-V
                         TOT-GER-DVD-V
                         TOT-GER-PFITA-V
                         TOT-GER-POSTER-V
                         TOT-GER-PORTA-DVD-V
                         TOT-GER-FOTO-CD-V
                         TOT-GER-MOLDURA-V
                         VENDEDOR-ANT
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.


           MOVE CAB04 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB05 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

      *    MOVE "         ---------------- MONTAGEM -------------- -----
      *    "------------- VENDAS ------------------ ---------------- SAL
      *    "DO -----------------    " TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *
      *    MOVE "NR-ALBUM EST ENC FOL FOTOS POS FIT PF DV PD FC MD EST E
      *    "NC FOL FOTOS COM POS FIT PF DV PD FC MD EST ENC FOL FOTOS PO
      *    "S FIT PF DV PD FC MD" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ALBUM-WK VENDEDOR-WK VENDEDOR-ANT.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
              MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF VENDEDOR-ANT = ZEROS
                        MOVE VENDEDOR-WK TO VENDEDOR-ANT
                        PERFORM CABECALHO-VENDEDOR
                ELSE IF VENDEDOR-WK <> VENDEDOR-ANT
                        PERFORM TOTALIZA-VENDEDOR
                        PERFORM CABECALHO-VENDEDOR
                     END-IF
                END-IF

                PERFORM MOVER-DADOS-LINDET

                MOVE CAB05D        TO GS-LINDET
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-VENDEDOR.
           PERFORM TOTALIZA-GER.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET.
           MOVE ALBUM-WK          TO ALBUM-E ALBUM-MTG ALBUM-REC
                                     ALBUM-W.

           MOVE ALBUM-E            TO DET-CONTRATO
           READ MTD020 INVALID KEY INITIALIZE REG-MTD020.
           READ RCD100 INVALID KEY INITIALIZE REG-RCD100.
      *    MONTAGEM
           MOVE QT-ESTOJO-MTG      TO DET-EST-MTG
           MOVE QT-ENCADER-MTG     TO DET-ENCARD-MTG
           MOVE QT-FOLHAS-MTG      TO DET-FOLHA-MTG
           MOVE QT-FOTOS-MTG       TO DET-FOTOS-MTG
           MOVE QT-POSTER-MTG      TO DET-POS-MTG
           MOVE QT-FITAS-MTG       TO DET-FITA-MTG
           MOVE QT-PORTA-FITA-MTG  TO DET-PFITA-MTG
           MOVE QT-DVD-MTG         TO DET-DVD-MTG
           MOVE QT-PORTA-DVD-MTG   TO DET-PDVD-MTG
           MOVE QT-FOTO-CD-MTG     TO DET-FC-MTG
           MOVE QT-MOLDURA-MTG     TO DET-MD-MTG

      *    VENDAS
           MOVE QESTOJO-REC        TO DET-EST-VEN
           MOVE QENCADER-REC       TO DET-ENCARD-VEN
           MOVE QFOLHAS-REC        TO DET-FOLHA-VEN
           COMPUTE FOTOS-W = QAVULSAS-REC + QFOTOS-REC + QABERTURA-REC
           MOVE FOTOS-W            TO DET-FOTOS-VEN
           MOVE QCOMISSAO-REC      TO DET-COM-VEN
           MOVE QPOSTER-REC        TO DET-POS-VEN
           MOVE QFITAS-REC         TO DET-FITA-VEN
           MOVE QPFITA-REC         TO DET-PFITA-VEN
           MOVE QDVD-REC           TO DET-DVD-VEN
           MOVE QPORTA-DVD-REC     TO DET-PDVD-VEN
           MOVE QFOTO-CD-REC       TO DET-FC-VEN
           MOVE QMOLDURA-REC       TO DET-MD-VEN

           COMPUTE SALDO-W = QT-ESTOJO-MTG - QESTOJO-REC
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = QT-ENCADER-MTG - QENCADER-REC
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = QT-FOLHAS-MTG - QFOLHAS-REC
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = QT-FOTOS-MTG - (FOTOS-W + QCOMISSAO-REC)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = QT-POSTER-MTG - QPOSTER-REC
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = QT-FITAS-MTG - QFITAS-REC
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = QT-PORTA-FITA-MTG - QPFITA-REC
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = QT-DVD-MTG - QDVD-REC
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = QT-PORTA-DVD-MTG - QPORTA-DVD-REC
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = QT-FOTO-CD-MTG - QFOTO-CD-REC
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = QT-MOLDURA-MTG - QMOLDURA-REC
           MOVE SALDO-W                TO DET-MD-SLD

           ADD QT-ESTOJO-MTG       TO TOT-EST-M
           ADD QT-ENCADER-MTG      TO TOT-ENC-M
           ADD QT-FOLHAS-MTG       TO TOT-FOLHA-M
           ADD QT-FOTOS-MTG        TO TOT-FOTO-M
           ADD QT-POSTER-MTG       TO TOT-POSTER-M
           ADD QT-FITAS-MTG        TO TOT-FITA-M
           ADD QT-DVD-MTG          TO TOT-DVD-M
           ADD QT-PORTA-DVD-MTG    TO TOT-PORTA-DVD-M
           ADD QT-FOTO-CD-MTG      TO TOT-FOTO-CD-M
           ADD QT-MOLDURA-MTG      TO TOT-MOLDURA-M
           ADD QT-PORTA-FITA-MTG   TO TOT-PFITA-M
           ADD QESTOJO-REC         TO TOT-EST-V
           ADD QENCADER-REC        TO TOT-ENC-V
           ADD QFOLHAS-REC         TO TOT-FOLHA-V
      *    TOT-FOTO-VENDIDAS = QFOTOS-REC + QCOMISSAO-REC + QABERTURA-RE
           ADD QAVULSAS-REC        TO TOT-FOTO-V
           ADD QFOTOS-REC          TO TOT-FOTO-V
           ADD QABERTURA-REC       TO TOT-FOTO-V
           ADD QCOMISSAO-REC       TO TOT-FOTO-COM-V
           ADD QPOSTER-REC         TO TOT-POSTER-V
           ADD QFITAS-REC          TO TOT-FITA-V
           ADD QDVD-REC            TO TOT-DVD-V
           ADD QPFITA-REC          TO TOT-PFITA-V
           ADD QPORTA-DVD-REC      TO TOT-PORTA-DVD-V
           ADD QFOTO-CD-REC        TO TOT-FOTO-CD-V
           ADD QMOLDURA-REC        TO TOT-MOLDURA-V.

       CABECALHO-VENDEDOR SECTION.
           MOVE SPACES               TO GS-LINDET.
           MOVE VENDEDOR-WK          TO CODIGO-CG01 GS-LINDET(1: 8)
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO GS-LINDET(9: 30)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE VENDEDOR-WK            TO VENDEDOR-ANT.
       TOTALIZA-VENDEDOR SECTION.
           MOVE SPACES             TO GS-LINDET

           MOVE ZEROS              TO DET-CONTRATO

           MOVE TOT-EST-M          TO DET-EST-MTG
           MOVE TOT-ENC-M          TO DET-ENCARD-MTG
           MOVE TOT-FOLHA-M        TO DET-FOLHA-MTG
           MOVE TOT-FOTO-M         TO DET-FOTOS-MTG
           MOVE TOT-POSTER-M       TO DET-POS-MTG
           MOVE TOT-FITA-M         TO DET-FITA-MTG
           MOVE TOT-PFITA-M        TO DET-PFITA-MTG
           MOVE TOT-DVD-M          TO DET-DVD-MTG
           MOVE TOT-PORTA-DVD-M    TO DET-PDVD-MTG
           MOVE TOT-FOTO-CD-M      TO DET-FC-MTG
           MOVE TOT-MOLDURA-M      TO DET-MD-MTG

      *    VENDAS
           MOVE TOT-EST-V          TO DET-EST-VEN
           MOVE TOT-ENC-V          TO DET-ENCARD-VEN
           MOVE TOT-FOLHA-V        TO DET-FOLHA-VEN
           MOVE TOT-FOTO-V         TO DET-FOTOS-VEN
           MOVE TOT-FOTO-COM-V     TO DET-COM-VEN
           MOVE TOT-POSTER-V       TO DET-POS-VEN
           MOVE TOT-FITA-V         TO DET-FITA-VEN
           MOVE TOT-PFITA-V        TO DET-PFITA-VEN
           MOVE TOT-DVD-V          TO DET-DVD-VEN
           MOVE TOT-PORTA-DVD-V    TO DET-PDVD-VEN
           MOVE TOT-FOTO-CD-V      TO DET-FC-VEN
           MOVE TOT-MOLDURA-V      TO DET-MD-VEN


           COMPUTE SALDO-W = TOT-EST-M - TOT-EST-V
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = TOT-ENC-M - TOT-ENC-V
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = TOT-FOLHA-M - TOT-FOLHA-V
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = TOT-FOTO-M - (TOT-FOTO-V + TOT-FOTO-COM-V)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = TOT-POSTER-M - TOT-POSTER-V
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = TOT-FITA-M - TOT-FITA-V
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = TOT-PFITA-M - TOT-PFITA-V
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = TOT-DVD-M - TOT-DVD-V
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = TOT-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = TOT-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = TOT-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-MD-SLD


           MOVE CAB03                  TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB05D                 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03                  TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           ADD TOT-EST-M               TO TOT-GER-EST-M
           ADD TOT-ENC-M               TO TOT-GER-ENC-M
           ADD TOT-FOLHA-M             TO TOT-GER-FOLHA-M
           ADD TOT-FOTO-M              TO TOT-GER-FOTO-M
           ADD TOT-POSTER-M            TO TOT-GER-POSTER-M
           ADD TOT-FITA-M              TO TOT-GER-FITA-M
           ADD TOT-DVD-M               TO TOT-GER-DVD-M
           ADD TOT-PORTA-DVD-M         TO TOT-GER-PORTA-DVD-M
           ADD TOT-FOTO-CD-M           TO TOT-GER-FOTO-CD-M
           ADD TOT-MOLDURA-M           TO TOT-GER-MOLDURA-M
           ADD TOT-PFITA-M             TO TOT-GER-PFITA-M
           ADD TOT-EST-V               TO TOT-GER-EST-V
           ADD TOT-ENC-V               TO TOT-GER-ENC-V
           ADD TOT-FOLHA-V             TO TOT-GER-FOLHA-V
           ADD TOT-FOTO-V              TO TOT-GER-FOTO-V
           ADD TOT-FOTO-COM-V          TO TOT-GER-FOTO-COM-V
           ADD TOT-POSTER-V            TO TOT-GER-POSTER-V
           ADD TOT-FITA-V              TO TOT-GER-FITA-V
           ADD TOT-DVD-V               TO TOT-GER-DVD-V
           ADD TOT-PFITA-V             TO TOT-GER-PFITA-V
           ADD TOT-PORTA-DVD-V         TO TOT-GER-PORTA-DVD-V
           ADD TOT-FOTO-CD-V           TO TOT-GER-FOTO-CD-V
           ADD TOT-MOLDURA-V           TO TOT-GER-MOLDURA-V

           MOVE ZEROS TO TOT-EST-M TOT-ENC-M TOT-FOLHA-M TOT-PFITA-M
                         TOT-FOTO-M TOT-FITA-M TOT-POSTER-M TOT-DVD-M
                         TOT-PORTA-DVD-M TOT-FOTO-CD-M TOT-MOLDURA-M
                         TOT-EST-V TOT-ENC-V TOT-FOLHA-V TOT-PFITA-V
                         TOT-FOTO-V TOT-FITA-V TOT-POSTER-V TOT-DVD-V
                         TOT-FOTO-COM-V TOT-PORTA-DVD-V TOT-FOTO-CD-V
                         TOT-MOLDURA-V.

       TOTALIZA-GER SECTION.
           MOVE SPACES TO GS-LINTOT1 GS-LINTOT2 GS-LINTOT3.

           MOVE ZEROS                  TO DET-CONTRATO

           MOVE TOT-GER-EST-M          TO DET-TOT-EST
           MOVE TOT-GER-ENC-M          TO DET-TOT-ENCARD
           MOVE TOT-GER-FOLHA-M        TO DET-TOT-FOLHAS
           MOVE TOT-GER-FOTO-M         TO DET-TOT-FOTOS
           MOVE TOT-GER-POSTER-M       TO DET-TOT-POSTER
           MOVE TOT-GER-FITA-M         TO DET-TOT-FITAS
           MOVE TOT-GER-PFITA-M        TO DET-TOT-PFITA
           MOVE TOT-GER-DVD-M          TO DET-TOT-DVD
           MOVE TOT-GER-PORTA-DVD-M    TO DET-TOT-PDVD
           MOVE TOT-GER-FOTO-CD-M      TO DET-TOT-FTCD
           MOVE TOT-GER-MOLDURA-M      TO DET-TOT-MOLD

           MOVE CAB10                  TO GS-DESC-LINHA1
           MOVE CAB10D                 TO GS-LINTOT1

      *    VENDAS

           MOVE TOT-GER-EST-V          TO DET-TOT-EST2
           MOVE TOT-GER-ENC-V          TO DET-TOT-ENCARD2
           MOVE TOT-GER-FOLHA-V        TO DET-TOT-FOLHAS2
           MOVE TOT-GER-FOTO-V         TO DET-TOT-FOTOS2
           MOVE TOT-GER-FOTO-COM-V     TO DET-TOT-COMIS2
           MOVE TOT-GER-POSTER-V       TO DET-TOT-POSTER2
           MOVE TOT-GER-FITA-V         TO DET-TOT-FITAS2
           MOVE TOT-GER-PFITA-V        TO DET-TOT-PFITA2
           MOVE TOT-GER-DVD-V          TO DET-TOT-DVD2
           MOVE TOT-GER-PORTA-DVD-V    TO DET-TOT-PDVD2
           MOVE TOT-GER-FOTO-CD-V      TO DET-TOT-FTCD2
           MOVE TOT-GER-MOLDURA-V      TO DET-TOT-MOLD2

           MOVE CAB10A                 TO GS-DESC-LINHA2
           MOVE CAB10D2                TO GS-LINTOT2


           COMPUTE SALDO-W = TOT-GER-EST-M - TOT-GER-EST-V
           MOVE SALDO-W                TO DET-TOT-EST
           COMPUTE SALDO-W = TOT-GER-ENC-M - TOT-GER-ENC-V
           MOVE SALDO-W                TO DET-TOT-ENCARD
           COMPUTE SALDO-W = TOT-GER-FOLHA-M - TOT-GER-FOLHA-V
           MOVE SALDO-W                TO DET-TOT-FOLHAS
           COMPUTE SALDO-W = TOT-GER-FOTO-M - (TOT-GER-FOTO-V +
                                               TOT-GER-FOTO-COM-V)
           MOVE SALDO-W                TO DET-TOT-FOTOS
           COMPUTE SALDO-W = TOT-GER-POSTER-M - TOT-GER-POSTER-V
           MOVE SALDO-W                TO DET-TOT-POSTER
           COMPUTE SALDO-W = TOT-GER-FITA-M - TOT-GER-FITA-V
           MOVE SALDO-W                TO DET-TOT-FITAS
           COMPUTE SALDO-W = TOT-GER-PFITA-M - TOT-GER-PFITA-V
           MOVE SALDO-W                TO DET-TOT-PFITA
           COMPUTE SALDO-W = TOT-GER-DVD-M - TOT-GER-DVD-V
           MOVE SALDO-W                TO DET-TOT-DVD
           COMPUTE SALDO-W = TOT-GER-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-TOT-PDVD
           COMPUTE SALDO-W = TOT-GER-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-TOT-FTCD
           COMPUTE SALDO-W = TOT-GER-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-TOT-MOLD

           MOVE CAB10D                 TO GS-LINTOT3.
           MOVE "REFRESH-DATA"         TO DS-PROCEDURE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP202" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO SALDO-W
                         TOT-EST-M
                         TOT-ENC-M
                         TOT-FOLHA-M
                         TOT-FOTO-M
                         TOT-FITA-M
                         TOT-DVD-M
                         TOT-PFITA-M
                         TOT-POSTER-M
                         TOT-PORTA-DVD-M
                         TOT-FOTO-CD-M
                         TOT-MOLDURA-M
                         TOT-EST-V
                         TOT-ENC-V
                         TOT-FOLHA-V
                         TOT-FOTO-V
                         TOT-FOTO-COM-V
                         TOT-FITA-V
                         TOT-DVD-V
                         TOT-PFITA-V
                         TOT-POSTER-V
                         TOT-PORTA-DVD-V
                         TOT-FOTO-CD-V
                         TOT-MOLDURA-V
                         TOT-GER-EST-M
                         TOT-GER-ENC-M
                         TOT-GER-FOLHA-M
                         TOT-GER-FOTO-M
                         TOT-GER-FITA-M
                         TOT-GER-DVD-M
                         TOT-GER-PFITA-M
                         TOT-GER-POSTER-M
                         TOT-GER-PORTA-DVD-M
                         TOT-GER-FOTO-CD-M
                         TOT-GER-MOLDURA-M
                         TOT-GER-EST-V
                         TOT-GER-ENC-V
                         TOT-GER-FOLHA-V
                         TOT-GER-FOTO-V
                         TOT-GER-FOTO-COM-V
                         TOT-GER-FITA-V
                         TOT-GER-DVD-V
                         TOT-GER-PFITA-V
                         TOT-GER-POSTER-V
                         TOT-GER-PORTA-DVD-V
                         TOT-GER-FOTO-CD-V
                         TOT-GER-MOLDURA-V
                         VENDEDOR-ANT
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.

           MOVE ZEROS TO VENDEDOR-WK ALBUM-WK
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    IF VENDEDOR-ANT = ZEROS
                       MOVE VENDEDOR-WK TO VENDEDOR-ANT
                       PERFORM CABECALHO-VENDEDOR-REL
                    ELSE
                       IF VENDEDOR-WK <> VENDEDOR-ANT
                          PERFORM TOTALIZA-VENDEDOR-REL
                          PERFORM CABECALHO
                          MOVE VENDEDOR-WK TO VENDEDOR-ANT
                          PERFORM CABECALHO-VENDEDOR-REL
                       END-IF
                    END-IF
                    PERFORM MOVER-DADOS-RELATORIO
               END-READ
           END-PERFORM.

           PERFORM TOTALIZA-VENDEDOR-REL.

           IF GS-VENDEDOR = ZEROS
              PERFORM TOTALIZA-GER-REL.


           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-RELATORIO SECTION.
           MOVE ALBUM-WK          TO ALBUM-E
                                     ALBUM-MTG
                                     ALBUM-REC
                                     ALBUM-W

           MOVE ALBUM-E            TO DET-CONTRATO
           READ MTD020 INVALID KEY INITIALIZE REG-MTD020.
           READ RCD100 INVALID KEY INITIALIZE REG-RCD100.
      *    MONTAGEM
           MOVE QT-ESTOJO-MTG      TO DET-EST-MTG
           MOVE QT-ENCADER-MTG     TO DET-ENCARD-MTG
           MOVE QT-FOLHAS-MTG      TO DET-FOLHA-MTG
           MOVE QT-FOTOS-MTG       TO DET-FOTOS-MTG
           MOVE QT-POSTER-MTG      TO DET-POS-MTG
           MOVE QT-FITAS-MTG       TO DET-FITA-MTG
           MOVE QT-PORTA-FITA-MTG  TO DET-PFITA-MTG
           MOVE QT-DVD-MTG         TO DET-DVD-MTG
           MOVE QT-PORTA-DVD-MTG   TO DET-PDVD-MTG
           MOVE QT-FOTO-CD-MTG     TO DET-FC-MTG
           MOVE QT-MOLDURA-MTG     TO DET-MD-MTG

      *    VENDAS
           MOVE QESTOJO-REC        TO DET-EST-VEN
           MOVE QENCADER-REC       TO DET-ENCARD-VEN
           MOVE QFOLHAS-REC        TO DET-FOLHA-VEN
           COMPUTE FOTOS-W = QAVULSAS-REC + QFOTOS-REC + QABERTURA-REC
           MOVE FOTOS-W            TO DET-FOTOS-VEN
           MOVE QCOMISSAO-REC      TO DET-COM-VEN
           MOVE QPOSTER-REC        TO DET-POS-VEN
           MOVE QFITAS-REC         TO DET-FITA-VEN
           MOVE QPFITA-REC         TO DET-PFITA-VEN
           MOVE QDVD-REC           TO DET-DVD-VEN
           MOVE QPORTA-DVD-REC     TO DET-PDVD-VEN
           MOVE QFOTO-CD-REC       TO DET-FC-VEN
           MOVE QMOLDURA-REC       TO DET-MD-VEN

           COMPUTE SALDO-W = QT-ESTOJO-MTG - QESTOJO-REC
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = QT-ENCADER-MTG - QENCADER-REC
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = QT-FOLHAS-MTG - QFOLHAS-REC
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = QT-FOTOS-MTG - (FOTOS-W + QCOMISSAO-REC)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = QT-POSTER-MTG - QPOSTER-REC
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = QT-FITAS-MTG - QFITAS-REC
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = QT-PORTA-FITA-MTG - QPFITA-REC
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = QT-DVD-MTG - QDVD-REC
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = QT-PORTA-DVD-MTG - QPORTA-DVD-REC
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = QT-FOTO-CD-MTG - QFOTO-CD-REC
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = QT-MOLDURA-MTG - QMOLDURA-REC
           MOVE SALDO-W                TO DET-MD-SLD

           WRITE REG-RELAT FROM CAB05d
           ADD 1 TO LIN

           IF LIN > 56
              PERFORM CABECALHO
              PERFORM CABECALHO-VENDEDOR-REL.

           ADD QT-ESTOJO-MTG       TO TOT-EST-M
           ADD QT-ENCADER-MTG      TO TOT-ENC-M
           ADD QT-FOLHAS-MTG       TO TOT-FOLHA-M
           ADD QT-FOTOS-MTG        TO TOT-FOTO-M
           ADD QT-POSTER-MTG       TO TOT-POSTER-M
           ADD QT-FITAS-MTG        TO TOT-FITA-M
           ADD QT-DVD-MTG          TO TOT-DVD-M
           ADD QT-PORTA-FITA-MTG   TO TOT-PFITA-M
           ADD QT-PORTA-DVD-MTG    TO TOT-PORTA-DVD-M
           ADD QT-FOTO-CD-MTG      TO TOT-FOTO-CD-M
           ADD QT-MOLDURA-MTG      TO TOT-MOLDURA-M
           ADD QESTOJO-REC         TO TOT-EST-V
           ADD QENCADER-REC        TO TOT-ENC-V
           ADD QFOLHAS-REC         TO TOT-FOLHA-V
           ADD QFOTOS-REC          TO TOT-FOTO-V
           ADD QAVULSAS-REC        TO TOT-FOTO-V
           ADD QABERTURA-REC       TO TOT-FOTO-V
           ADD QCOMISSAO-REC       TO TOT-FOTO-COM-V
           ADD QPOSTER-REC         TO TOT-POSTER-V
           ADD QFITAS-REC          TO TOT-FITA-V
           ADD QDVD-REC            TO TOT-DVD-V
           ADD QPORTA-DVD-REC      TO TOT-PORTA-DVD-V
           ADD QFOTO-CD-REC        TO TOT-FOTO-CD-V
           ADD QMOLDURA-REC        TO TOT-MOLDURA-V
           ADD QPFITA-REC          TO TOT-PFITA-V.

       MOVER-DADOS-RELATORIO-P SECTION.
           MOVE ALBUM-WK          TO ALBUM-E
                                     ALBUM-MTG
                                     ALBUM-REC
                                     ALBUM-W

           MOVE ALBUM-E            TO DET-CONTRATO
           READ MTD020 INVALID KEY
                INITIALIZE REG-MTD020.
           READ RCD100 INVALID KEY
                INITIALIZE REG-RCD100.

      *    MONTAGEM
           MOVE QT-ESTOJO-MTG      TO DET-EST-MTG
           MOVE QT-ENCADER-MTG     TO DET-ENCARD-MTG
           MOVE QT-FOLHAS-MTG      TO DET-FOLHA-MTG
           MOVE QT-FOTOS-MTG       TO DET-FOTOS-MTG
           MOVE QT-POSTER-MTG      TO DET-POS-MTG
           MOVE QT-FITAS-MTG       TO DET-FITA-MTG
           MOVE QT-PORTA-FITA-MTG  TO DET-PFITA-MTG
           MOVE QT-DVD-MTG         TO DET-DVD-MTG
           MOVE QT-PORTA-DVD-MTG   TO DET-PDVD-MTG
           MOVE QT-FOTO-CD-MTG     TO DET-FC-MTG
           MOVE QT-MOLDURA-MTG     TO DET-MD-MTG

      *    VENDAS
           MOVE QESTOJO-REC        TO DET-EST-VEN
           MOVE QENCADER-REC       TO DET-ENCARD-VEN
           MOVE QFOLHAS-REC        TO DET-FOLHA-VEN
           COMPUTE FOTOS-W = QAVULSAS-REC + QFOTOS-REC + QABERTURA-REC
           MOVE FOTOS-W            TO DET-FOTOS-VEN
           MOVE QCOMISSAO-REC      TO DET-COM-VEN
           MOVE QPOSTER-REC        TO DET-POS-VEN
           MOVE QFITAS-REC         TO DET-FITA-VEN
           MOVE QPFITA-REC         TO DET-PFITA-VEN
           MOVE QDVD-REC           TO DET-DVD-VEN
           MOVE QPORTA-DVD-REC     TO DET-PDVD-VEN
           MOVE QFOTO-CD-REC       TO DET-FC-VEN
           MOVE QMOLDURA-REC       TO DET-MD-VEN

           COMPUTE SALDO-W = QT-ESTOJO-MTG - QESTOJO-REC
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = QT-ENCADER-MTG - QENCADER-REC
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = QT-FOLHAS-MTG - QFOLHAS-REC
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = QT-FOTOS-MTG - (FOTOS-W + QCOMISSAO-REC)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = QT-POSTER-MTG - QPOSTER-REC
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = QT-FITAS-MTG - QFITAS-REC
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = QT-PORTA-FITA-MTG - QPFITA-REC
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = QT-DVD-MTG - QDVD-REC
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = QT-PORTA-DVD-MTG - QPORTA-DVD-REC
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = QT-FOTO-CD-MTG - QFOTO-CD-REC
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = QT-MOLDURA-MTG - QMOLDURA-REC
           MOVE SALDO-W                TO DET-MD-SLD

           MOVE CAB05d                 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           ADD QT-ESTOJO-MTG       TO TOT-EST-M
           ADD QT-ENCADER-MTG      TO TOT-ENC-M
           ADD QT-FOLHAS-MTG       TO TOT-FOLHA-M
           ADD QT-FOTOS-MTG        TO TOT-FOTO-M
           ADD QT-POSTER-MTG       TO TOT-POSTER-M
           ADD QT-FITAS-MTG        TO TOT-FITA-M
           ADD QT-DVD-MTG          TO TOT-DVD-M
           ADD QT-PORTA-FITA-MTG   TO TOT-PFITA-M
           ADD QT-PORTA-DVD-MTG    TO TOT-PORTA-DVD-M
           ADD QT-FOTO-CD-MTG      TO TOT-FOTO-CD-M
           ADD QT-MOLDURA-MTG      TO TOT-MOLDURA-M
           ADD QESTOJO-REC         TO TOT-EST-V
           ADD QENCADER-REC        TO TOT-ENC-V
           ADD QFOLHAS-REC         TO TOT-FOLHA-V
           ADD QFOTOS-REC          TO TOT-FOTO-V
           ADD QAVULSAS-REC        TO TOT-FOTO-V
           ADD QABERTURA-REC       TO TOT-FOTO-V
           ADD QCOMISSAO-REC       TO TOT-FOTO-COM-V
           ADD QPOSTER-REC         TO TOT-POSTER-V
           ADD QFITAS-REC          TO TOT-FITA-V
           ADD QDVD-REC            TO TOT-DVD-V
           ADD QPORTA-DVD-REC      TO TOT-PORTA-DVD-V
           ADD QFOTO-CD-REC        TO TOT-FOTO-CD-V
           ADD QMOLDURA-REC        TO TOT-MOLDURA-V
           ADD QPFITA-REC          TO TOT-PFITA-V.


       CABECALHO-VENDEDOR-REL SECTION.
      *    IF LIN > 51 PERFORM CABECALHO.
           MOVE SPACES               TO LINDET-REL.
           MOVE VENDEDOR-ANT         TO CODIGO-CG01 LINDET-REL(1: 8)
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO LINDET-REL(9: 30)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           MOVE VENDEDOR-WK            TO VENDEDOR-ANT.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           ADD 7 TO LIN.

       CABECALHO-VENDEDOR-REL-P SECTION.
           MOVE SPACES               TO LNK-DETALHE
           MOVE VENDEDOR-ANT         TO CODIGO-CG01 LNK-DETALHE(1: 8)
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO LNK-DETALHE(9: 30)
           PERFORM IMPRIMIR-LINHA
           MOVE SPACES               TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA
           MOVE VENDEDOR-WK          TO VENDEDOR-ANT

           MOVE CAB03                TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA
           MOVE CAB04                TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA
           MOVE CAB03                TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA
           MOVE CAB05                TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA
           MOVE CAB03                TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA.

       TOTALIZA-VENDEDOR-REL SECTION.
           MOVE SPACES             TO LINDET-REL
           WRITE REG-RELAT FROM CAB03.
           ADD 1 TO LIN.

           MOVE ZEROS              TO DET-CONTRATO

           MOVE TOT-EST-M          TO DET-EST-MTG
           MOVE TOT-ENC-M          TO DET-ENCARD-MTG
           MOVE TOT-FOLHA-M        TO DET-FOLHA-MTG
           MOVE TOT-FOTO-M         TO DET-FOTOS-MTG
           MOVE TOT-POSTER-M       TO DET-POS-MTG
           MOVE TOT-FITA-M         TO DET-FITA-MTG
           MOVE TOT-PFITA-M        TO DET-PFITA-MTG
           MOVE TOT-DVD-M          TO DET-DVD-MTG
           MOVE TOT-PORTA-DVD-M    TO DET-PDVD-MTG
           MOVE TOT-FOTO-CD-M      TO DET-FC-MTG
           MOVE TOT-MOLDURA-M      TO DET-MD-MTG

      *    VENDAS
           MOVE TOT-EST-V          TO DET-EST-VEN
           MOVE TOT-ENC-V          TO DET-ENCARD-VEN
           MOVE TOT-FOLHA-V        TO DET-FOLHA-VEN
           MOVE TOT-FOTO-V         TO DET-FOTOS-VEN
           MOVE TOT-FOTO-COM-V     TO DET-COM-VEN
           MOVE TOT-POSTER-V       TO DET-POS-VEN
           MOVE TOT-FITA-V         TO DET-FITA-VEN
           MOVE TOT-PFITA-V        TO DET-PFITA-VEN
           MOVE TOT-DVD-V          TO DET-DVD-VEN
           MOVE TOT-PORTA-DVD-V    TO DET-PDVD-VEN
           MOVE TOT-FOTO-CD-V      TO DET-FC-VEN
           MOVE TOT-MOLDURA-V      TO DET-MD-VEN


           COMPUTE SALDO-W = TOT-EST-M - TOT-EST-V
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = TOT-ENC-M - TOT-ENC-V
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = TOT-FOLHA-M - TOT-FOLHA-V
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = TOT-FOTO-M - (TOT-FOTO-V + TOT-FOTO-COM-V)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = TOT-POSTER-M - TOT-POSTER-V
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = TOT-FITA-M - TOT-FITA-V
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = TOT-PFITA-M - TOT-PFITA-V
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = TOT-DVD-M - TOT-DVD-V
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = TOT-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = TOT-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = TOT-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-MD-SLD

           WRITE REG-RELAT FROM CAB05d

           WRITE REG-RELAT FROM CAB03.
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 3 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.

           ADD TOT-EST-M               TO TOT-GER-EST-M
           ADD TOT-ENC-M               TO TOT-GER-ENC-M
           ADD TOT-FOLHA-M             TO TOT-GER-FOLHA-M
           ADD TOT-FOTO-M              TO TOT-GER-FOTO-M
           ADD TOT-POSTER-M            TO TOT-GER-POSTER-M
           ADD TOT-FITA-M              TO TOT-GER-FITA-M
           ADD TOT-DVD-M               TO TOT-GER-DVD-M
           ADD TOT-PORTA-DVD-M         TO TOT-GER-PORTA-DVD-M
           ADD TOT-FOTO-CD-M           TO TOT-GER-FOTO-CD-M
           ADD TOT-MOLDURA-M           TO TOT-GER-MOLDURA-M
           ADD TOT-PFITA-M             TO TOT-GER-PFITA-M
           ADD TOT-EST-V               TO TOT-GER-EST-V
           ADD TOT-ENC-V               TO TOT-GER-ENC-V
           ADD TOT-FOLHA-V             TO TOT-GER-FOLHA-V
           ADD TOT-FOTO-V              TO TOT-GER-FOTO-V
           ADD TOT-FOTO-COM-V          TO TOT-GER-FOTO-COM-V
           ADD TOT-POSTER-V            TO TOT-GER-POSTER-V
           ADD TOT-FITA-V              TO TOT-GER-FITA-V
           ADD TOT-DVD-V               TO TOT-GER-DVD-V
           ADD TOT-PFITA-V             TO TOT-GER-PFITA-V
           ADD TOT-PORTA-DVD-V         TO TOT-GER-PORTA-DVD-V
           ADD TOT-FOTO-CD-V           TO TOT-GER-FOTO-CD-V
           ADD TOT-MOLDURA-V           TO TOT-GER-MOLDURA-V


           MOVE ZEROS TO TOT-EST-M TOT-ENC-M TOT-FOLHA-M TOT-PFITA-M
                         TOT-FOTO-M TOT-FITA-M TOT-POSTER-M  TOT-DVD-M
                         TOT-PORTA-DVD-M TOT-FOTO-CD-M TOT-MOLDURA-M
                         TOT-EST-V TOT-ENC-V TOT-FOLHA-V TOT-PFITA-V
                         TOT-FOTO-V TOT-FITA-V TOT-POSTER-V
                         TOT-FOTO-COM-V TOT-DVD-V TOT-PORTA-DVD-V
                         TOT-FOTO-CD-V TOT-MOLDURA-V.

       TOTALIZA-VENDEDOR-REL-P SECTION.
           MOVE CAB03              TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE ZEROS              TO DET-CONTRATO

           MOVE TOT-EST-M          TO DET-EST-MTG
           MOVE TOT-ENC-M          TO DET-ENCARD-MTG
           MOVE TOT-FOLHA-M        TO DET-FOLHA-MTG
           MOVE TOT-FOTO-M         TO DET-FOTOS-MTG
           MOVE TOT-POSTER-M       TO DET-POS-MTG
           MOVE TOT-FITA-M         TO DET-FITA-MTG
           MOVE TOT-PFITA-M        TO DET-PFITA-MTG
           MOVE TOT-DVD-M          TO DET-DVD-MTG
           MOVE TOT-PORTA-DVD-M    TO DET-PDVD-MTG
           MOVE TOT-FOTO-CD-M      TO DET-FC-MTG
           MOVE TOT-MOLDURA-M      TO DET-MD-MTG

      *    VENDAS
           MOVE TOT-EST-V          TO DET-EST-VEN
           MOVE TOT-ENC-V          TO DET-ENCARD-VEN
           MOVE TOT-FOLHA-V        TO DET-FOLHA-VEN
           MOVE TOT-FOTO-V         TO DET-FOTOS-VEN
           MOVE TOT-FOTO-COM-V     TO DET-COM-VEN
           MOVE TOT-POSTER-V       TO DET-POS-VEN
           MOVE TOT-FITA-V         TO DET-FITA-VEN
           MOVE TOT-PFITA-V        TO DET-PFITA-VEN
           MOVE TOT-DVD-V          TO DET-DVD-VEN
           MOVE TOT-PORTA-DVD-V    TO DET-PDVD-VEN
           MOVE TOT-FOTO-CD-V      TO DET-FC-VEN
           MOVE TOT-MOLDURA-V      TO DET-MD-VEN


           COMPUTE SALDO-W = TOT-EST-M - TOT-EST-V
           MOVE SALDO-W                TO DET-EST-SLD
           COMPUTE SALDO-W = TOT-ENC-M - TOT-ENC-V
           MOVE SALDO-W                TO DET-ENCARD-SLD
           COMPUTE SALDO-W = TOT-FOLHA-M - TOT-FOLHA-V
           MOVE SALDO-W                TO DET-FOLHA-SLD
           COMPUTE SALDO-W = TOT-FOTO-M - (TOT-FOTO-V + TOT-FOTO-COM-V)
           MOVE SALDO-W                TO DET-FOTOS-SLD
           COMPUTE SALDO-W = TOT-POSTER-M - TOT-POSTER-V
           MOVE SALDO-W                TO DET-POS-SLD
           COMPUTE SALDO-W = TOT-FITA-M - TOT-FITA-V
           MOVE SALDO-W                TO DET-FITA-SLD
           COMPUTE SALDO-W = TOT-PFITA-M - TOT-PFITA-V
           MOVE SALDO-W                TO DET-PFITA-SLD
           COMPUTE SALDO-W = TOT-DVD-M - TOT-DVD-V
           MOVE SALDO-W                TO DET-DVD-SLD
           COMPUTE SALDO-W = TOT-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-PDVD-SLD
           COMPUTE SALDO-W = TOT-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-FC-SLD
           COMPUTE SALDO-W = TOT-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-MD-SLD

           MOVE CAB05d                 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB03                  TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE SPACES                 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           ADD TOT-EST-M               TO TOT-GER-EST-M
           ADD TOT-ENC-M               TO TOT-GER-ENC-M
           ADD TOT-FOLHA-M             TO TOT-GER-FOLHA-M
           ADD TOT-FOTO-M              TO TOT-GER-FOTO-M
           ADD TOT-POSTER-M            TO TOT-GER-POSTER-M
           ADD TOT-FITA-M              TO TOT-GER-FITA-M
           ADD TOT-DVD-M               TO TOT-GER-DVD-M
           ADD TOT-PORTA-DVD-M         TO TOT-GER-PORTA-DVD-M
           ADD TOT-FOTO-CD-M           TO TOT-GER-FOTO-CD-M
           ADD TOT-MOLDURA-M           TO TOT-GER-MOLDURA-M
           ADD TOT-PFITA-M             TO TOT-GER-PFITA-M
           ADD TOT-EST-V               TO TOT-GER-EST-V
           ADD TOT-ENC-V               TO TOT-GER-ENC-V
           ADD TOT-FOLHA-V             TO TOT-GER-FOLHA-V
           ADD TOT-FOTO-V              TO TOT-GER-FOTO-V
           ADD TOT-FOTO-COM-V          TO TOT-GER-FOTO-COM-V
           ADD TOT-POSTER-V            TO TOT-GER-POSTER-V
           ADD TOT-FITA-V              TO TOT-GER-FITA-V
           ADD TOT-DVD-V               TO TOT-GER-DVD-V
           ADD TOT-PFITA-V             TO TOT-GER-PFITA-V
           ADD TOT-PORTA-DVD-V         TO TOT-GER-PORTA-DVD-V
           ADD TOT-FOTO-CD-V           TO TOT-GER-FOTO-CD-V
           ADD TOT-MOLDURA-V           TO TOT-GER-MOLDURA-V


           MOVE ZEROS TO TOT-EST-M TOT-ENC-M TOT-FOLHA-M TOT-PFITA-M
                         TOT-FOTO-M TOT-FITA-M TOT-POSTER-M  TOT-DVD-M
                         TOT-PORTA-DVD-M TOT-FOTO-CD-M TOT-MOLDURA-M
                         TOT-EST-V TOT-ENC-V TOT-FOLHA-V TOT-PFITA-V
                         TOT-FOTO-V TOT-FITA-V TOT-POSTER-V
                         TOT-FOTO-COM-V TOT-DVD-V TOT-PORTA-DVD-V
                         TOT-FOTO-CD-V TOT-MOLDURA-V.

       TOTALIZA-GER-REL SECTION.
           MOVE SPACES TO LINTOT1-REL LINTOT2-REL LINTOT3-REL

           MOVE ZEROS                  TO DET-CONTRATO

           MOVE TOT-GER-EST-M          TO DET-TOT-EST
           MOVE TOT-GER-ENC-M          TO DET-TOT-ENCARD
           MOVE TOT-GER-FOLHA-M        TO DET-TOT-FOLHAS
           MOVE TOT-GER-FOTO-M         TO DET-TOT-FOTOS
           MOVE TOT-GER-POSTER-M       TO DET-TOT-POSTER
           MOVE TOT-GER-FITA-M         TO DET-TOT-FITAS
           MOVE TOT-GER-PFITA-M        TO DET-TOT-PFITA
           MOVE TOT-GER-DVD-M          TO DET-TOT-DVD
           MOVE TOT-GER-PORTA-DVD-M    TO DET-TOT-PDVD
           MOVE TOT-GER-FOTO-CD-M      TO DET-TOT-FTCD
           MOVE TOT-GER-MOLDURA-M      TO DET-TOT-MOLD

           MOVE CAB10                  TO GS-DESC-LINHA1
           MOVE CAB10D                 TO LINTOT1-REL

      *    VENDAS

           MOVE TOT-GER-EST-V          TO DET-TOT-EST2
           MOVE TOT-GER-ENC-V          TO DET-TOT-ENCARD2
           MOVE TOT-GER-FOLHA-V        TO DET-TOT-FOLHAS2
           MOVE TOT-GER-FOTO-V         TO DET-TOT-FOTOS2
           MOVE TOT-GER-FOTO-COM-V     TO DET-TOT-COMIS2
           MOVE TOT-GER-POSTER-V       TO DET-TOT-POSTER2
           MOVE TOT-GER-FITA-V         TO DET-TOT-FITAS2
           MOVE TOT-GER-PFITA-V        TO DET-TOT-PFITA2
           MOVE TOT-GER-DVD-V          TO DET-TOT-DVD2
           MOVE TOT-GER-PORTA-DVD-V    TO DET-TOT-PDVD2
           MOVE TOT-GER-FOTO-CD-V      TO DET-TOT-FTCD2
           MOVE TOT-GER-MOLDURA-V      TO DET-TOT-MOLD2

           MOVE CAB10A                 TO GS-DESC-LINHA2
           MOVE CAB10D2                TO LINTOT2-REL


           COMPUTE SALDO-W = TOT-GER-EST-M - TOT-GER-EST-V
           MOVE SALDO-W                TO DET-TOT-EST
           COMPUTE SALDO-W = TOT-GER-ENC-M - TOT-GER-ENC-V
           MOVE SALDO-W                TO DET-TOT-ENCARD
           COMPUTE SALDO-W = TOT-GER-FOLHA-M - TOT-GER-FOLHA-V
           MOVE SALDO-W                TO DET-TOT-FOLHAS
           COMPUTE SALDO-W = TOT-GER-FOTO-M - (TOT-GER-FOTO-V +
                                               TOT-GER-FOTO-COM-V)
           MOVE SALDO-W                TO DET-TOT-FOTOS
           COMPUTE SALDO-W = TOT-GER-POSTER-M - TOT-GER-POSTER-V
           MOVE SALDO-W                TO DET-TOT-POSTER
           COMPUTE SALDO-W = TOT-GER-FITA-M - TOT-GER-FITA-V
           MOVE SALDO-W                TO DET-TOT-FITAS
           COMPUTE SALDO-W = TOT-GER-PFITA-M - TOT-GER-PFITA-V
           MOVE SALDO-W                TO DET-TOT-PFITA
           COMPUTE SALDO-W = TOT-GER-DVD-M - TOT-GER-DVD-V
           MOVE SALDO-W                TO DET-TOT-DVD
           COMPUTE SALDO-W = TOT-GER-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-TOT-PDVD
           COMPUTE SALDO-W = TOT-GER-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-TOT-FTCD
           COMPUTE SALDO-W = TOT-GER-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-TOT-MOLD

           MOVE CAB10D                 TO LINTOT3-REL .

           ADD 15 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.

           WRITE REG-RELAT FROM CAB06 AFTER 2.
           WRITE REG-RELAT FROM CAB10.
           WRITE REG-RELAT FROM CAB11.
           WRITE REG-RELAT FROM LINTOT1.
           WRITE REG-RELAT FROM CAB07 AFTER 2.
           WRITE REG-RELAT FROM CAB10A.
           WRITE REG-RELAT FROM CAB11A.
           WRITE REG-RELAT FROM LINTOT2.
           WRITE REG-RELAT FROM CAB08 AFTER 2.
           WRITE REG-RELAT FROM CAB10.
           WRITE REG-RELAT FROM CAB11.
           WRITE REG-RELAT FROM LINTOT3.

       TOTALIZA-GER-REL-P SECTION.
           MOVE SPACES TO LINTOT1-REL LINTOT2-REL LINTOT3-REL

           MOVE ZEROS                  TO DET-CONTRATO

           MOVE TOT-GER-EST-M          TO DET-TOT-EST
           MOVE TOT-GER-ENC-M          TO DET-TOT-ENCARD
           MOVE TOT-GER-FOLHA-M        TO DET-TOT-FOLHAS
           MOVE TOT-GER-FOTO-M         TO DET-TOT-FOTOS
           MOVE TOT-GER-POSTER-M       TO DET-TOT-POSTER
           MOVE TOT-GER-FITA-M         TO DET-TOT-FITAS
           MOVE TOT-GER-PFITA-M        TO DET-TOT-PFITA
           MOVE TOT-GER-DVD-M          TO DET-TOT-DVD
           MOVE TOT-GER-PORTA-DVD-M    TO DET-TOT-PDVD
           MOVE TOT-GER-FOTO-CD-M      TO DET-TOT-FTCD
           MOVE TOT-GER-MOLDURA-M      TO DET-TOT-MOLD

           MOVE CAB10                  TO GS-DESC-LINHA1
           MOVE CAB10D                 TO LINTOT1-REL

      *    VENDAS

           MOVE TOT-GER-EST-V          TO DET-TOT-EST2
           MOVE TOT-GER-ENC-V          TO DET-TOT-ENCARD2
           MOVE TOT-GER-FOLHA-V        TO DET-TOT-FOLHAS2
           MOVE TOT-GER-FOTO-V         TO DET-TOT-FOTOS2
           MOVE TOT-GER-FOTO-COM-V     TO DET-TOT-COMIS2
           MOVE TOT-GER-POSTER-V       TO DET-TOT-POSTER2
           MOVE TOT-GER-FITA-V         TO DET-TOT-FITAS2
           MOVE TOT-GER-PFITA-V        TO DET-TOT-PFITA2
           MOVE TOT-GER-DVD-V          TO DET-TOT-DVD2
           MOVE TOT-GER-PORTA-DVD-V    TO DET-TOT-PDVD2
           MOVE TOT-GER-FOTO-CD-V      TO DET-TOT-FTCD2
           MOVE TOT-GER-MOLDURA-V      TO DET-TOT-MOLD2

           MOVE CAB10A                 TO GS-DESC-LINHA2
           MOVE CAB10D2                TO LINTOT2-REL


           COMPUTE SALDO-W = TOT-GER-EST-M - TOT-GER-EST-V
           MOVE SALDO-W                TO DET-TOT-EST
           COMPUTE SALDO-W = TOT-GER-ENC-M - TOT-GER-ENC-V
           MOVE SALDO-W                TO DET-TOT-ENCARD
           COMPUTE SALDO-W = TOT-GER-FOLHA-M - TOT-GER-FOLHA-V
           MOVE SALDO-W                TO DET-TOT-FOLHAS
           COMPUTE SALDO-W = TOT-GER-FOTO-M - (TOT-GER-FOTO-V +
                                               TOT-GER-FOTO-COM-V)
           MOVE SALDO-W                TO DET-TOT-FOTOS
           COMPUTE SALDO-W = TOT-GER-POSTER-M - TOT-GER-POSTER-V
           MOVE SALDO-W                TO DET-TOT-POSTER
           COMPUTE SALDO-W = TOT-GER-FITA-M - TOT-GER-FITA-V
           MOVE SALDO-W                TO DET-TOT-FITAS
           COMPUTE SALDO-W = TOT-GER-PFITA-M - TOT-GER-PFITA-V
           MOVE SALDO-W                TO DET-TOT-PFITA
           COMPUTE SALDO-W = TOT-GER-DVD-M - TOT-GER-DVD-V
           MOVE SALDO-W                TO DET-TOT-DVD
           COMPUTE SALDO-W = TOT-GER-PORTA-DVD-M - TOT-PORTA-DVD-V
           MOVE SALDO-W                TO DET-TOT-PDVD
           COMPUTE SALDO-W = TOT-GER-FOTO-CD-M - TOT-FOTO-CD-V
           MOVE SALDO-W                TO DET-TOT-FTCD
           COMPUTE SALDO-W = TOT-GER-MOLDURA-M - TOT-MOLDURA-V
           MOVE SALDO-W                TO DET-TOT-MOLD

           MOVE CAB10D                 TO LINTOT3-REL .

           MOVE SPACES TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB06  TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB10  TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB11  TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE LINTOT1 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB10A TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB11A TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE LINTOT2 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB08   TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB10   TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE CAB11   TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA

           MOVE LINTOT3 TO LNK-DETALHE
           PERFORM IMPRIMIR-LINHA.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02.
           MOVE SPACES       TO REG-RELAT.
           WRITE REG-RELAT.
           MOVE 3 TO LIN.

       EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM".
       EXIBIR-MENSAGEM-FIM.
           EXIT.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 RCD100 MTD020 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
