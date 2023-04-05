       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP502.
       DATE-WRITTEN. 15/03/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: DEFINICAO QUANTITATIVA DE EQUIPE NECESSÁRIA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY REPX502.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = CIDADE-WK CONTRATO-WK
                             WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK = CIDADE-WK
                    DATA-WK HORA-WK WITH DUPLICATES.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY REPW502.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(3).
           05  DATA-WK             PIC 9(8).
           05  HORA-WK             PIC X(5).
           05  CONTRATO-WK         PIC 9(4).
           05  CIDADE-WK           PIC 9(4).
           05  FORM-WK             PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  LOCAL-WK            PIC X(15).
           05  EVENTO-WK           PIC X(15).
           05  TELAO-WK            PIC 9.
           05  STUDIO-WK           PIC 9.
           05  CLIP-WK             PIC X.
           05  BECA-WK             PIC X.
           05  VIDEO-WK            PIC 9.
           05  EVENTO-PRE-WK       PIC 9.
      *    1-EVENTO  2-PRE-EVENTO

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(230).
       WORKING-STORAGE SECTION.
           COPY "REP502.CPB".
           COPY "REP502.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-RED502             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  GRAVA-EQUIPE-W        PIC 9        VALUE ZEROS.

           05  CIDADE-SELEC          PIC 9(4)     VALUE ZEROS.

           05  DATA-E                 PIC ZZ/ZZ/ZZZZ.
           05  QT-EQ-E               PIC ZZZ      BLANK WHEN ZEROS.

           05  DATA-SEG               PIC 9(8)     VALUE ZEROS.
           05  DATA-DOM               PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(38)  VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE
           "DEFINICAO QUANTITATIVA DE EQUIPE".
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE ALL "=".
       01  TRACO.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE ALL "-".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  CABECALHO-REL       PIC X(78)   VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(78)   VALUE
           "CIDADE        COO FOT CIN AUX TEL STU VEI".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(78)   VALUE SPACES.

           copy impressora.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "RED502"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED502.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.

           OPEN INPUT CAD010 COD003 COD040 COD060.

           OPEN I-O RED502.

           IF ST-RED502 = "35"
              CLOSE RED502  OPEN OUTPUT RED502
              CLOSE RED502  OPEN I-O RED502.

           IF ST-RED502 <> "00"
              MOVE "ERRO ABERTURA RED502: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED502 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W IMPRESSORA-W
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
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM GRAVA-RED502
                    PERFORM CARREGA-LISTA
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-DATA
               WHEN GS-CIDADE-SELECION-TRUE
                    PERFORM CIDADE-SELECIONADA
               WHEN GS-GRAVA-EQUIPE-TRUE
                    PERFORM GRAVA-EQUIPE
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
      *-----------------------------------------------------------
       VERIFICA-DATA SECTION.
           MOVE GS-DATA   TO GRTIME-DATE.
           MOVE 1         TO GRTIME-TYPE.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA.
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GS-DATA   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO DATA-SEG

           MOVE DATA-SEG   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-DOM.

       CHAMA-GRTIME-ADAY SECTION.
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 6         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".

      *------------------------------------------------------------
       GRAVA-WORK SECTION.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO SEQ-WK.
           MOVE DATA-SEG TO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
               NOT AT END
                 MOVE DATAREALIZA-CO60 TO GS-EXIBE-VENCTO
                 MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF DATAREALIZA-CO60 > DATA-DOM MOVE "10" TO ST-COD060
                 ELSE
                    MOVE DATAREALIZA-CO60 TO DATA-WK
                    MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO40
                                             CONTRATO-WK
                    READ COD040 INVALID KEY MOVE ZEROS TO CIDADE-CO40
                    END-READ
                    MOVE CIDADE-CO40      TO CIDADE-WK
                    MOVE QT-PARTICIPANTE-CO60 TO FORM-WK
                    MOVE PADRAO-CO40      TO PADRAO-WK
                    IF VIDEO-CO60 = 1
                         MOVE 1           TO VIDEO-WK
                    ELSE MOVE ZEROS       TO VIDEO-WK
                    END-IF
                    MOVE QT-TELAO-CO60    TO TELAO-WK
                    MOVE ZEROS            TO STUDIO-WK
                    MOVE BECA-CO60        TO BECA-WK
                    MOVE CLIP-CO60        TO CLIP-WK

                    MOVE CODEVENTO-CO60   TO CODIGO-CO03
                    READ COD003 INVALID KEY MOVE 1 TO EVENTO-PRE-CO03
                    END-READ
                    MOVE NOME-CO03        TO EVENTO-WK
                    MOVE EVENTO-PRE-CO03  TO EVENTO-PRE-WK
                    MOVE LOCAL-CO60       TO LOCAL-WK
                    MOVE HORARIO-CO60     TO HORA-WK(1: 2)
                    MOVE ":"              TO HORA-WK(3: 1)
                    MOVE HORARIO-CO60     TO HORA-WK(4: 2)
                    ADD 1 TO SEQ-WK
                    WRITE REG-WORK
                    END-WRITE
                 END-IF
             END-READ
           END-PERFORM.


       GRAVA-RED502 SECTION.
           MOVE ZEROS TO CIDADE-WK CONTRATO-WK.
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                MOVE CIDADE-WK TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                PERFORM CONT-GRAVA-RED502
             END-READ
           END-PERFORM.
       CONT-GRAVA-RED502 SECTION.
           MOVE CIDADE-WK  TO CIDADE CIDADE-RE502
           MOVE DATA-SEG   TO DATA-RE502.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID   TO NOME-CIDADE-RE502
           READ RED502 INVALID KEY
                MOVE ZEROS TO COORD-RE502 FOTOG-RE502 CINEG-RE502
                              AUXIL-RE502 TELAO-RE502 STUDIO-RE502
                              VEICULO-RE502
                WRITE REG-RED502
                END-WRITE
           END-READ.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
      *  LISTA DO CRONOGRAMA
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES   TO NOME-CIDADE-RE502
           MOVE DATA-SEG TO DATA-RE502
           START RED502 KEY IS NOT < ALT1-RE502 INVALID KEY
                 MOVE "10" TO ST-RED502.
           PERFORM UNTIL ST-RED502 = "10"
             READ RED502 NEXT RECORD AT END MOVE "10" TO ST-RED502
               NOT AT END
                IF DATA-RE502 <> DATA-SEG MOVE "10" TO ST-RED502
                ELSE
                 MOVE NOME-CIDADE-RE502     TO GS-LINDET(1: 15)
                 MOVE COORD-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(16: 4)
                 MOVE FOTOG-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(20: 4)
                 MOVE CINEG-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(24: 4)
                 MOVE AUXIL-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(28: 4)
                 MOVE TELAO-RE502         TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(32: 4)
                 MOVE STUDIO-RE502        TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(36: 4)
                 MOVE VEICULO-RE502       TO QT-EQ-E
                 MOVE QT-EQ-E             TO GS-LINDET(40: 4)
                 MOVE CIDADE-RE502        TO GS-LINDET(50: 4)

                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CIDADE-SELECIONADA SECTION.
           MOVE "CLEAR-LIST-EVENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-LINDET(50: 4)    TO CIDADE-SELEC CIDADE-WK
           MOVE ZEROS               TO DATA-WK CIDADE-WK
           START WORK KEY IS NOT < ALT1-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                IF CIDADE-WK <> CIDADE-SELEC CONTINUE
                ELSE
                 MOVE SEQ-WK        TO GS-LINDET1(1: 4)
                 MOVE DATA-WK       TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV      TO DATA-E
                 MOVE DATA-E        TO GS-LINDET1(5: 11)
                 MOVE HORA-WK       TO GS-LINDET1(16: 6)
                 MOVE EVENTO-WK     TO GS-LINDET1(22: 16)
                 MOVE FORM-WK       TO GS-LINDET1(38: 5)
                 MOVE PADRAO-WK     TO GS-LINDET1(43: 2)
                 MOVE CONTRATO-WK   TO GS-LINDET1(45: 5)
                 MOVE LOCAL-WK      TO GS-LINDET1(50: 16)
                 MOVE TELAO-WK      TO GS-LINDET1(66: 2)
                 MOVE STUDIO-WK     TO GS-LINDET1(68: 2)
                 MOVE BECA-WK       TO GS-LINDET1(70: 2)
                 MOVE CLIP-WK       TO GS-LINDET1(72: 2)
                 MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                END-IF
             END-READ
           END-PERFORM.

           MOVE CIDADE-SELEC     TO CIDADE-RE502.
           MOVE DATA-SEG         TO DATA-RE502.
           READ RED502 INVALID KEY
             MOVE ZEROS TO GS-COORD GS-FOTOG GS-CINEG GS-AUXIL
                           GS-TELAO GS-STUDIO GS-VEICULO
             NOT INVALID KEY
               MOVE COORD-RE502        TO GS-COORD
               MOVE CINEG-RE502        TO GS-CINEG
               MOVE FOTOG-RE502        TO GS-FOTOG
               MOVE AUXIL-RE502        TO GS-AUXIL
               MOVE TELAO-RE502        TO GS-TELAO
               MOVE STUDIO-RE502       TO GS-STUDIO
               MOVE VEICULO-RE502      TO GS-VEICULO
           END-READ
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-EQUIPE SECTION.
           IF CIDADE-SELEC = ZEROS CONTINUE
           ELSE
              MOVE DATA-SEG       TO DATA-RE502
              MOVE CIDADE-SELEC   TO CIDADE-RE502 CIDADE
              READ RED502 INVALID KEY MOVE 0 TO GRAVA-EQUIPE-W
                NOT INVALID KEY MOVE 1 TO GRAVA-EQUIPE-W
              END-READ
              READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID       TO NOME-CIDADE-RE502
              MOVE GS-COORD       TO COORD-RE502 GS-LINDET(16: 4)
              MOVE GS-CINEG       TO CINEG-RE502 GS-LINDET(24: 4)
              MOVE GS-FOTOG       TO FOTOG-RE502 GS-LINDET(20: 4)
              MOVE GS-AUXIL       TO AUXIL-RE502 GS-LINDET(28: 4)
              MOVE GS-TELAO       TO TELAO-RE502 GS-LINDET(32: 4)
              MOVE GS-STUDIO      TO STUDIO-RE502 GS-LINDET(36: 4)
              MOVE GS-VEICULO     TO VEICULO-RE502 GS-LINDET(40: 4)

              IF GRAVA-EQUIPE-W = 0
                 WRITE REG-RED502
                 END-WRITE
              ELSE REWRITE REG-RED502
                   END-REWRITE
              END-IF
           END-IF.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP502" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.

           MOVE ZEROS TO PAG-W.

            copy condensa.

            PERFORM IMPRIME-CRONOGRAMA.

            copy descondensa.

       IMPRIME-CRONOGRAMA SECTION.
           MOVE ZEROS TO PAG-W LIN.
           PERFORM CABECALHO.
           MOVE "CRONOGRAMA DA CIDADE         "  TO CABECALHO-REL.
           WRITE REG-RELAT FROM CAB04 AFTER 2.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           ADD 5 TO LIN.
           MOVE SPACES TO LINDET-REL.


           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 WORK RED502.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
