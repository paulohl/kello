       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP205.
      *DATA: 22/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE COMISSÃO DE VENDEDOR
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY MTPX020.
           COPY RCPX100.
           COPY RCPX101.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = COD-VENDEDOR-WK VENCTO-WK.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


           SELECT ALBUNS ASSIGN TO "ALBUNS-RCP205.TXT"
                         ORGANIZATION IS LINE SEQUENTIAL
                         ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY MTPW020.
       COPY RCPW100.
       COPY RCPW101.

       FD  WORK.
       01  REG-WORK.
           05  COD-VENDEDOR-WK       PIC 9(6).
           05  VENDEDOR-WK           PIC X(09).
           05  VENCTO-WK             PIC 9(6).
           05  VLR-VENDA-WK          PIC 9(8)V99.
           05  VLR-VENDA-DEF-WK      PIC 9(8)V99.
           05  VLR-COMISSAO-WK       PIC 9(8)V99.
           05  VLR-COMISSAO-DEF-WK   PIC 9(8)V99.
           05  VALOR-PM-WK           PIC 9(10)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).

       FD ALBUNS.
       01 REG-ALBUNS.
          05 ALBUM                 PIC 9(08).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP205.CPB".
           COPY "RCP205.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 9(2).
               10  DIA-I             PIC 9(2).
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  MESANOW.
               10  MES-WW            PIC 9(2).
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 9(2).
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANO-E              PIC ZZ/ZZZZ       BLANK WHEN ZEROS.
           05  DATA-E                PIC 99/99/9999    BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  VENDEDOR-ANT          PIC 9(6)     VALUE ZEROS.
           05  VLR-DEFLACAO          PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-COMISSAO        PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-COMISSAO-DEF    PIC 9(8)V99  VALUE ZEROS.
           05  PM-W                  PIC 9(3)V9.
           05  PM-E                  PIC ZZZ,Z.
           05  ALBUMW.
               10  CONTRATO-W        PIC 9(4).
               10  SEQ-W             PIC 9(4).
           05  ALBUM-W REDEFINES ALBUMW PIC 9(8).
           05  TOT-COMISSAO1         PIC 9(8)V99  VALUE ZEROS.
           05  TOTG-COMISSAO1        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VENDA             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VENDA-DEF         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-COMISSAO          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-COMISSAO-DEF      PIC 9(8)V99  VALUE ZEROS.
           05  TOTG-VENDA            PIC 9(8)V99  VALUE ZEROS.
           05  TOTG-VENDA-DEF        PIC 9(8)V99  VALUE ZEROS.
           05  TOTG-COMISSAO         PIC 9(8)V99  VALUE ZEROS.
           05  TOTG-COMISSAO-DEF     PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  AUX-DATA2             PIC 9(08).
           05  AUX-DATA              PIC 9(08).
           05  AUX-DATA-R REDEFINES AUX-DATA.
               10 AUX-MESANO         PIC 9(06).
               10 AUX-DIA            PIC 9(02).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO DE COMISSAO DE VENDEDOR -".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(03)   VALUE SPACES.
           05  ORDEM-REL2          PIC X(16)   VALUE SPACES.
       01  CAB02A.
           05  DET-DESCRICAO       PIC X(14).
           05  DATA-MOVTO-INI-REL  PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC XXX     VALUE " a ".
           05  DATA-MOVTO-FIM-REL  PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC X(6)    VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "INT.DTA.VCTO: ".
           05  DATA-VECTO-INI-REL  PIC ZZ/ZZ/ZZZZ.
           05  FILLER              PIC XXX     VALUE " a ".
           05  DATA-VECTO-FIM-REL  PIC ZZ/ZZ/ZZZZ.
       01  CAB02B.
           05  FILLER              PIC X(10) VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9999.

       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "CODIGO NOME-VENDEDOR                  DATA-VECTO  VLR-COMISS
      -    "AO".
       01  CAB05.
           05  FILLER              PIC X(80)  VALUE
           "CODIGO NOME-VEND  P.M. DT-VCTO     VLR-VENDA     VENDA-DEF
      -    "VLR-COMIS COMIS-DEFL".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.
       01  LINDET1.
           05  LINDET1-REL         PIC X(80)  VALUE SPACES.
       01  CAB06.
           05  FILLER               PIC X(80)    VALUE
           "__________________   __________________   __________________
      -    "  __________________".
       01  CAB07.
           05  FILLER               PIC X(80)    VALUE
           "    EMITENTE              DIRETOR                CPD
      -     "     FINANCEIRO".
       01  CAB08.
           05  FILLER               PIC X(80)    VALUE
           "_____/_____/______   _____/_____/______   _____/_____/______
      -     "  _____/_____/______".


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-MOVTO-W TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001 MTD020 RCD100 RCD101.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
      *             EVALUATE GS-OPCAO
      *                WHEN 1 MOVE "INT.DTA-VCTO: " TO DESCRICAO-REL
      *                WHEN 2 MOVE "INT.DTA-VEND: " TO DESCRICAO-REL
      *             END-EVALUATE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTA1
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01    TO GS-DESC-VENDEDOR.
       CHAMAR-POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           PERFORM LE-VENDEDOR.
      *----------------------------------------------------------
       INVERTE-DATA SECTION.
           MOVE GS-VECTO-INI TO DATA-INV DATA-VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV DATA-VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.
           MOVE GS-MOVTO-INI TO DATA-INV DATA-MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-INI.
           MOVE GS-MOVTO-FIM TO DATA-INV DATA-MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-FIM
           MOVE GS-DESCRICAO TO DET-DESCRICAO

           MOVE GS-CONTRATO  TO CONTRATO-REL.

       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK ALBUNS
           CLOSE       WORK
           OPEN I-O    WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM INVERTE-DATA.

           INITIALIZE REG-RCD100

           IF GS-CONTRATO > 0
              PERFORM POR-CONTRATO
           ELSE
              EVALUATE GS-OPCAO
                 WHEN 1 PERFORM POR-DTMOVTO
                 WHEN 2 PERFORM POR-DTVECTO
              END-EVALUATE
           END-IF

           CLOSE ALBUNS

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       POR-CONTRATO SECTION.
           STRING GS-CONTRATO "0000"       INTO ALBUM-REC
           START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF GS-CONTRATO <> ALBUM-REC(1:4)
                      MOVE "10" TO ST-RCD100
                   ELSE
                      IF GS-OPCAO = 1
                         IF DATA-MOVTO-REC NOT < MOVTO-INI AND
                            DATA-MOVTO-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM VERIFICA-RCD101
                            END-IF
                         END-IF
                      ELSE
                         IF DATAVEN-REC NOT < MOVTO-INI AND
                            DATAVEN-REC NOT > MOVTO-FIM OR
                            MOVTO-INI = 0
                            MOVE DATAVEN-REC     TO GS-EXIBE-MOVTO
                            MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                               VENDEDOR-REC
                               PERFORM VERIFICA-RCD101
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTMOVTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATA-MOVTO-REC
           START RCD100 KEY IS NOT LESS ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATA-MOVTO-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM VERIFICA-RCD101
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       POR-DTVECTO SECTION.
           INITIALIZE REG-RCD100
           MOVE MOVTO-INI      TO DATAVEN-REC
           START RCD100 KEY IS NOT LESS DATAVEN-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATAVEN-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE DATAVEN-REC  TO GS-EXIBE-MOVTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                         VENDEDOR-REC
                         PERFORM VERIFICA-RCD101
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       VERIFICA-RCD101 SECTION.
           MOVE ZEROS TO ALBUM
           INITIALIZE REG-RCD101.
           MOVE ALBUM-REC     TO ALBUM-REC1
           MOVE VECTO-INI     TO VENCTO-REC1
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT RECORD AT END
                    MOVE "10" TO ST-RCD101
               NOT AT END
                  IF ALBUM-REC1 <> ALBUM-REC OR VENCTO-REC1 > VECTO-FIM
                     MOVE "10" TO ST-RCD101
                  ELSE
                     IF DTA-BAIXA-REC1 NOT > 0
                        IF ALBUM-REC <> ALBUM
                           MOVE ALBUM-REC TO ALBUM
                           WRITE REG-ALBUNS
                        END-IF
                        IF (DATA-MOVTO-REC > 20050510 AND
                            TIPO-REC1 = 3) OR TIPO-REC1 <> 3
                            *> VENDA ANTECIPADA
                            MOVE ALBUM-REC1 TO ALBUM-MTG
                            READ MTD020 INVALID KEY
                                 MOVE ZEROS  TO VISITA-MTG
                                                FOGO-MTG
                            END-READ
                            IF GS-SO-FOGO = 1
                               IF FOGO-MTG = 8
                                  PERFORM MOVER-DADOS-WORK
                               END-IF
                            ELSE
                               IF GS-INCLUIR-FOGO = 1
      *                           IF FOGO-MTG = 8
                                     PERFORM MOVER-DADOS-WORK
      *                           END-IF
                               ELSE
                                  IF FOGO-MTG <> 8
                                     PERFORM MOVER-DADOS-WORK
                                  END-IF
                               END-IF
                            END-IF
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

       MOVER-DADOS-WORK SECTION.
             IF GS-TIPO-REL = 1
                PERFORM DEFLACIONA-VALOR
                COMPUTE VALOR-COMISSAO-DEF =
                        COMIS-PARC-REC1 * VLR-DEFLACAO
             ELSE
                MOVE ZEROS TO VALOR-COMISSAO-DEF VLR-DEFLACAO
             END-IF

             COMPUTE VALOR-COMISSAO =
                        VALOR-REC1 * COMIS-PARC-REC1

             MOVE VENCTO-REC1          TO DATA-I
             MOVE DATA-I(1: 6)         TO VENCTO-WK
             MOVE VENDEDOR-REC         TO COD-VENDEDOR-WK

      *      DISPLAY "VOU MOSTRAR OS DADOS" STOP " ".

      *      DISPLAY DATA-MOVTO-REC STOP " DATA-MOVTO-REC"
      *      DISPLAY DATAVEN-REC STOP " DATAVEN-REC"
      *      DISPLAY VENCTO-REC1 STOP " VENCTO-REC1"
      *      DISPLAY VENCTO-REC1 STOP " VENCTO-REC1"
      *      DISPLAY ALBUM-REC STOP " ALBUM-REC"
      *      DISPLAY ALBUM-REC STOP " ALBUM-REC"

             READ WORK INVALID KEY
                 MOVE VENDEDOR-REC    TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01          TO VENDEDOR-WK
                 MOVE VALOR-COMISSAO     TO VLR-COMISSAO-WK
                 MOVE VALOR-COMISSAO-DEF TO VLR-COMISSAO-DEF-WK
                 MOVE VALOR-REC1         TO VLR-VENDA-WK
                 MOVE VLR-DEFLACAO       TO VLR-VENDA-DEF-WK
                 COMPUTE VALOR-PM-WK = GRTIME-DAYS-FINAL * VALOR-REC1

                 WRITE REG-WORK
                 END-WRITE
               NOT INVALID KEY
                 ADD VALOR-COMISSAO     TO VLR-COMISSAO-WK
                 ADD VALOR-COMISSAO-DEF TO VLR-COMISSAO-DEF-WK
                 ADD VALOR-REC1         TO VLR-VENDA-WK
                 ADD VLR-DEFLACAO       TO VLR-VENDA-DEF-WK
                 COMPUTE VALOR-PM-WK = VALOR-PM-WK +
                     (GRTIME-DAYS-FINAL * VALOR-REC1)

                 REWRITE REG-WORK
                 END-REWRITE
             END-READ.

       DEFLACIONA-VALOR SECTION.
           MOVE 2 TO GRTIME-TYPE.
           MOVE 3 TO GRTIME-FUNCTION.
           MOVE DATA-MOVTO-I TO GRTIME-DATE.
           MOVE VENCTO-REC1 TO DATA-I.
           MOVE 10 TO DIA-I.
           IF MES-I = 12 MOVE 1 TO MES-I
                         ADD 1 TO ANO-I
           ELSE ADD 1 TO MES-I.
           MOVE DATA-I       TO GRTIME-DATE-FINAL.
           IF GRTIME-DATE > GRTIME-DATE-FINAL
              MOVE VALOR-REC1 TO VLR-DEFLACAO
           ELSE CALL "GRTIME" USING PARAMETROS-GRTIME
                COMPUTE VLR-DEFLACAO = (VALOR-REC1 /
                ((GRTIME-DAYS-FINAL * (GS-TAXA / 30) / 100) + 1)).

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-VENDA TOT-VENDA-DEF TOT-COMISSAO1
                         TOT-COMISSAO TOT-COMISSAO-DEF
                         TOTG-VENDA TOTG-VENDA-DEF TOTG-COMISSAO1
                         TOTG-COMISSAO TOTG-COMISSAO-DEF.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                           PERFORM TOTALIZA-VENDEDOR
                           MOVE "10" TO ST-WORK
              NOT AT END
                 IF VENDEDOR-ANT <> ZEROS AND
                  COD-VENDEDOR-WK <> VENDEDOR-ANT
                     PERFORM TOTALIZA-VENDEDOR
                 END-IF
                 MOVE COD-VENDEDOR-WK   TO VENDEDOR-ANT
                 PERFORM MOVER-DADOS-LINDET
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET.
           MOVE COD-VENDEDOR-WK   TO GS-LINDET(1: 7)
           MOVE VENDEDOR-WK       TO GS-LINDET(8: 31)
           MOVE VENCTO-WK         TO MESANO-I
           MOVE MES-II TO MES-WW
           MOVE ANO-II TO ANO-WW
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(39: 8)
           EVALUATE GS-TIPO-REL
             WHEN 1 MOVE VLR-COMISSAO-DEF-WK  TO VALOR-E
                    MOVE VALOR-E              TO GS-LINDET(47: 13)
                    ADD VLR-COMISSAO-DEF-WK   TO TOT-COMISSAO1
             WHEN 2 MOVE VLR-COMISSAO-WK      TO VALOR-E
                    MOVE VALOR-E              TO GS-LINDET(47: 13)
                    ADD VLR-COMISSAO-WK       TO TOT-COMISSAO1
           END-EVALUATE.


       CARREGA-LISTA1 SECTION.
           MOVE ZEROS TO VENDEDOR-ANT.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-CONT.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                         PERFORM TOTALIZA1-VENDEDOR
                        MOVE "10" TO ST-WORK
              NOT AT END
                 IF VENDEDOR-ANT <> ZEROS AND
                  COD-VENDEDOR-WK <> VENDEDOR-ANT
                     PERFORM TOTALIZA1-VENDEDOR
                 END-IF
                 MOVE COD-VENDEDOR-WK   TO VENDEDOR-ANT
                 PERFORM MOVER-DADOS-LINDET1
                 MOVE "INSERE-LIST1" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA1.

       MOVER-DADOS-LINDET1 SECTION.
           MOVE SPACES              TO GS-LINDET1
           MOVE COD-VENDEDOR-WK     TO GS-LINDET1(1: 7)
           MOVE VENDEDOR-WK         TO GS-LINDET1(8: 10)
           MOVE VENCTO-WK           TO MESANO-I
           MOVE MES-II              TO MES-WW
           MOVE ANO-II              TO ANO-WW
           MOVE MESANO-W            TO MESANO-E
           COMPUTE PM-W = VALOR-PM-WK / VLR-VENDA-WK
           MOVE PM-W                TO PM-E
           MOVE PM-E                TO GS-LINDET1(18: 6)
           MOVE MESANO-E            TO GS-LINDET1(24: 8)
           MOVE VLR-VENDA-WK        TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET1(32: 14)
           ADD VLR-VENDA-WK         TO TOT-VENDA
           MOVE VLR-VENDA-DEF-WK    TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET1(46: 14)
           ADD VLR-VENDA-DEF-WK     TO TOT-VENDA-DEF
           MOVE VLR-COMISSAO-WK     TO VALOR-E1
           MOVE VALOR-E1            TO GS-LINDET1(60: 11)
           ADD VLR-COMISSAO-WK      TO TOT-COMISSAO
           MOVE VLR-COMISSAO-DEF-WK TO VALOR-E1
           MOVE VALOR-E1            TO GS-LINDET1(71: 10)
           ADD VLR-COMISSAO-DEF-WK  TO TOT-COMISSAO-DEF.

       TOTALIZA-VENDEDOR SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "Total do Vendedor..." to GS-LINDET
           MOVE TOT-COMISSAO1        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(47: 13)
           ADD TOT-COMISSAO1         TO TOTG-COMISSAO1
           MOVE ZEROS TO TOT-COMISSAO1.
           MOVE "INSERE-LIST"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA1-VENDEDOR SECTION.
           MOVE SPACES TO GS-LINDET1.
           MOVE "Total do Vendedor..." to GS-LINDET1
           MOVE TOT-VENDA            TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET1(32: 14)
           MOVE TOT-VENDA-DEF        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET1(46: 14)
           MOVE TOT-COMISSAO         TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET1(60: 11)
           MOVE TOT-COMISSAO-DEF     TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET1(71: 10)

           ADD TOT-VENDA             TO TOTG-VENDA
           ADD TOT-VENDA-DEF         TO TOTG-VENDA-DEF
           ADD TOT-COMISSAO          TO TOTG-COMISSAO
           ADD TOT-COMISSAO-DEF      TO TOTG-COMISSAO-DEF
           MOVE ZEROS TO TOT-VENDA TOT-VENDA-DEF TOT-COMISSAO
                         TOT-COMISSAO-DEF.
           MOVE "INSERE-LIST1"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "Total Geral......." TO GS-LINDET
           MOVE TOTG-COMISSAO1       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(47: 13)
           MOVE ZEROS TO TOT-COMISSAO1.
           MOVE "INSERE-LIST"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       TOTALIZA1 SECTION.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "Total Geral......." TO GS-LINDET1
           MOVE TOTG-VENDA           TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET1(32: 14)
           MOVE TOTG-VENDA-DEF       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET1(46: 14)
           MOVE TOTG-COMISSAO        TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET1(60: 11)
           MOVE TOTG-COMISSAO-DEF    TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET1(71: 10)
           MOVE ZEROS TO TOTG-VENDA TOTG-VENDA-DEF TOTG-COMISSAO
                         TOTG-COMISSAO-DEF.
           MOVE "INSERE-LIST1"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP205" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO VENDEDOR-ANT.
           MOVE ZEROS TO TOT-VENDA TOT-VENDA-DEF TOT-COMISSAO1
                         TOT-COMISSAO TOT-COMISSAO-DEF
                         TOTG-VENDA TOTG-VENDA-DEF TOTG-COMISSAO1
                         TOTG-COMISSAO TOTG-COMISSAO-DEF.

           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           IF GS-IMPR-RELATORIO-TRUE PERFORM MOVER-WORK-REL.
           IF GS-IMPR-RESUMO-TRUE  PERFORM IMPRIME-RESUMO.

           COPY DESCONDENSA.

       MOVER-WORK-REL SECTION.
           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                           PERFORM TOTALIZA-VENDEDOR-REL
                           MOVE "10" TO ST-WORK
              NOT AT END
                 IF VENDEDOR-ANT <> ZEROS AND
                  COD-VENDEDOR-WK <> VENDEDOR-ANT
                     PERFORM TOTALIZA-VENDEDOR-REL
                 END-IF
                 MOVE COD-VENDEDOR-WK   TO VENDEDOR-ANT

                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL

           MOVE COD-VENDEDOR-WK   TO LINDET-REL(1: 7)
           MOVE VENDEDOR-WK       TO LINDET-REL(8: 31)
           MOVE VENCTO-WK         TO MESANO-I
           MOVE MES-II            TO MES-WW
           MOVE ANO-II            TO ANO-WW
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(39: 8)
           EVALUATE GS-TIPO-REL
             WHEN 1 MOVE VLR-COMISSAO-DEF-WK  TO VALOR-E
                    MOVE VALOR-E              TO LINDET-REL(47: 13)
                    ADD VLR-COMISSAO-DEF-WK   TO TOT-COMISSAO1
             WHEN 2 MOVE VLR-COMISSAO-WK      TO VALOR-E
                    MOVE VALOR-E              TO LINDET-REL(47: 13)
                    ADD VLR-COMISSAO-WK       TO TOT-COMISSAO1
           END-EVALUATE.

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       IMPRIME-RESUMO SECTION.
           PERFORM CABECALHO1.
           MOVE SPACES TO LINDET1-REL.
           INITIALIZE REG-WORK.
           MOVE ZEROS TO REG-WORK
           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                         PERFORM TOTALIZA1-VENDEDOR-REL
                         MOVE "10" TO ST-WORK
              NOT AT END
                 IF VENDEDOR-ANT <> ZEROS AND
                  COD-VENDEDOR-WK <> VENDEDOR-ANT
                     PERFORM TOTALIZA1-VENDEDOR-REL
                 END-IF
                 MOVE COD-VENDEDOR-WK   TO VENDEDOR-ANT

                 PERFORM MOVER-DADOS-RELAT1-REL
                 WRITE REG-RELAT FROM LINDET1
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO1
                 END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA1-REL.

       MOVER-DADOS-RELAT1-REL SECTION.
           MOVE SPACES              TO LINDET1-REL
           MOVE COD-VENDEDOR-WK     TO LINDET1-REL(1: 7)
           MOVE VENDEDOR-WK(1: 15)  TO LINDET1-REL(8: 10)
           COMPUTE PM-W = VALOR-PM-WK / VLR-VENDA-WK
           MOVE PM-W                TO PM-E
           MOVE PM-E                TO LINDET1-REL(18: 6)
           MOVE VENCTO-WK           TO MESANO-I
           MOVE MES-II              TO MES-WW
           MOVE ANO-II              TO ANO-WW
           MOVE MESANO-W            TO MESANO-E
           MOVE MESANO-E            TO LINDET1-REL(24: 8)
           MOVE VLR-VENDA-WK        TO VALOR-E
           MOVE VALOR-E             TO LINDET1-REL(32: 14)
           ADD VLR-VENDA-WK         TO TOT-VENDA
           MOVE VLR-VENDA-DEF-WK    TO VALOR-E
           MOVE VALOR-E             TO LINDET1-REL(46: 14)
           ADD VLR-VENDA-DEF-WK     TO TOT-VENDA-DEF
           MOVE VLR-COMISSAO-WK     TO VALOR-E1
           MOVE VALOR-E1            TO LINDET1-REL(60: 11)
           ADD VLR-COMISSAO-WK      TO TOT-COMISSAO
           MOVE VLR-COMISSAO-DEF-WK TO VALOR-E1
           MOVE VALOR-E1            TO LINDET1-REL(71: 10).
           ADD VLR-COMISSAO-DEF-WK  TO TOT-COMISSAO-DEF.
       TOTALIZA-VENDEDOR-REL SECTION.
           MOVE SPACES TO LINDET-REL
           MOVE "Total do Vendedor..." TO LINDET-REL
           MOVE TOT-COMISSAO1        TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(47: 13)
           ADD TOT-COMISSAO1         TO TOTG-COMISSAO1
           MOVE ZEROS TO TOT-COMISSAO1.
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 2 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA1-VENDEDOR-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           MOVE "Total do Vendedor..." TO LINDET1-REL
           MOVE TOT-VENDA              TO VALOR-E
           MOVE VALOR-E              TO LINDET1-REL(32: 14)
           MOVE TOT-VENDA-DEF        TO VALOR-E
           MOVE VALOR-E              TO LINDET1-REL(46: 14)
           MOVE TOT-COMISSAO         TO VALOR-E1
           MOVE VALOR-E1             TO LINDET1-REL(60: 11)
           MOVE TOT-COMISSAO-DEF     TO VALOR-E1
           MOVE VALOR-E1             TO LINDET1-REL(71: 10)

           ADD TOT-VENDA             TO TOTG-VENDA
           ADD TOT-VENDA-DEF         TO TOTG-VENDA-DEF
           ADD TOT-COMISSAO          TO TOTG-COMISSAO
           ADD TOT-COMISSAO-DEF      TO TOTG-COMISSAO-DEF
           MOVE ZEROS TO TOT-VENDA TOT-VENDA-DEF TOT-COMISSAO
                         TOT-COMISSAO-DEF.
           WRITE REG-RELAT FROM LINDET1.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT FROM LINDET.
           ADD 2 TO LIN.
           IF LIN > 56 PERFORM CABECALHO1.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINDET-REL
           MOVE "Total Geral......." TO LINDET-REL
           MOVE TOTG-COMISSAO1       TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(47: 13)
           WRITE REG-RELAT FROM LINDET AFTER 2.
           ADD 2 TO LIN.
       TOTALIZA1-REL SECTION.
           MOVE SPACES TO REG-RELAT.
           MOVE "Total Geral......." TO LINDET1-REL
           MOVE TOTG-VENDA           TO VALOR-E
           MOVE VALOR-E              TO LINDET1-REL(32: 14)
           MOVE TOTG-VENDA-DEF       TO VALOR-E
           MOVE VALOR-E              TO LINDET1-REL(46: 14)
           MOVE TOTG-COMISSAO        TO VALOR-E1
           MOVE VALOR-E1             TO LINDET1-REL(60: 11)
           MOVE TOTG-COMISSAO-DEF    TO VALOR-E1
           MOVE VALOR-E1             TO LINDET1-REL(71: 10)
           MOVE ZEROS TO TOTG-VENDA TOTG-VENDA-DEF TOTG-COMISSAO
                         TOTG-COMISSAO-DEF.
           WRITE REG-RELAT FROM LINDET1 AFTER 2.
           ADD 2 TO LIN.
           IF LIN > 50 PERFORM CABECALHO1.
           WRITE REG-RELAT FROM CAB06 AFTER 5
           WRITE REG-RELAT FROM CAB07.
           WRITE REG-RELAT FROM CAB08 AFTER 2.
       CABECALHO SECTION.
           MOVE GS-DESCRICAO TO ORDEM-REL2
           IF GS-TIPO-REL = 1 MOVE "DEFLACIONADO" TO ORDEM-REL
           ELSE MOVE "NORMAL" TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02A.
           IF GS-CONTRATO > 0
              WRITE REG-RELAT FROM CAB02B.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           IF GS-CONTRATO > 0
              MOVE 7 TO LIN
           ELSE
              MOVE 6 TO LIN.
       CABECALHO1 SECTION.
           MOVE GS-DESCRICAO TO ORDEM-REL2
           IF GS-TIPO-REL = 1 MOVE "DEFLACIONADO" TO ORDEM-REL
           ELSE MOVE "NORMAL" TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02A.
           IF GS-CONTRATO > 0
              WRITE REG-RELAT FROM CAB02B.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           IF GS-CONTRATO > 0
              MOVE 7 TO LIN
           ELSE
              MOVE 6 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 MTD020 RCD100 RCD101 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
