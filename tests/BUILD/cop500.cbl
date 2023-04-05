       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP500.
      *DATA: 31/08/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Relatório de anotações agendadas - CONTRATO
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX500.
           COPY COPX501.
           COPY COPX040.
           COPY CAPX010.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY COPW500.
       COPY COPW501.
       COPY COPW040.
       COPY CAPW010.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP500.CPB".
           COPY "COP500.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-COD500             PIC XX       VALUE SPACES.
           05  ST-COD501             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  TAXA-E                PIC ZZ,Z     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  LINHA-W               PIC X(10)    VALUE SPACES.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(57)   VALUE
           "RELACAO-ANOTACOES AGENDADAS - CONTRATO ".
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "DATA AGENDADA: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
           MOVE DATA-INV TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD500"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD500.
           MOVE "COD501"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD501.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.

           OPEN INPUT COD501 COD040 CAD010.
           OPEN I-O   COD500.
           IF ST-COD500 = "35"
              CLOSE COD500  OPEN I-O COD500   CLOSE COD500
              OPEN I-O COD500.
           IF ST-COD500 <> "00"
              MOVE "ERRO ABERTURA COD500: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD500 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD501 <> "00"
              MOVE "ERRO ABERTURA COD501: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD501 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-LISTA-ANOTACAO-TRUE
                    PERFORM CARREGA-LISTA-ANOTACAO
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-DATA
               WHEN GS-PEND-CHECK-TRUE
                    PERFORM VERIFICA-SITUACAO
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
       VERIFICA-SITUACAO SECTION.
      *    MOVE SPACES TO LINHA-W
      *    MOVE GS-LINDET(1: 2) TO LINHA-W(1: 2)
           IF GS-LINDET(98: 10) <> SPACES
                MOVE GS-LINDET(88: 3)  TO ITEM-CO500
                MOVE GS-LINDET(16: 04) TO NR-CONTRATO-CO500

                READ COD500 INVALID KEY
                     CONTINUE
                NOT INVALID KEY
                    IF SITUACAO-CO500 = 0
                       MOVE 1 TO SITUACAO-CO500
                       REWRITE REG-COD500
                       END-REWRITE
                       MOVE "CHECADO" TO GS-LINDET(98: 10)
                    ELSE
                       MOVE 0 TO SITUACAO-CO500
                       REWRITE REG-COD500
                       END-REWRITE
                       MOVE "PENDENTE" TO GS-LINDET(98: 10)
                    END-IF
                    MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                END-READ
           END-IF.
       VERIFICA-DATA SECTION.
      *    funcao p/ verificar o dia da semana. Caso for segunda deixar
      *    o intervalo de sabado a segunda, caso contrario o intervalo
      *    é a data do dia
           MOVE DATA-MOVTO-W TO GRTIME-DATE.
           MOVE 1            TO GRTIME-TYPE.
           MOVE 8            TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    SE FOR = A SEGUNDA SUBTRAIR 2 DIAS P/ QUE FIQUE NO SABADO
           IF GRTIME-WEEK-NUM = 2
              MOVE DATA-MOVTO-W TO GRTIME-DATE
              MOVE 1            TO GRTIME-TYPE
              MOVE 5            TO GRTIME-FUNCTION
              MOVE 2            TO GRTIME-DAYS
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DATE-FINAL TO GS-VECTO-INI
              MOVE DATA-MOVTO-W      TO GS-VECTO-FIM
           ELSE MOVE DATA-MOVTO-W TO GS-VECTO-INI GS-VECTO-FIM.
       CARREGA-LISTA-ANOTACAO SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           CLEAR-OBJECT LB1
           REFRESH-OBJECT LB1

           MOVE GS-VECTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO VECTO-FIM.
           MOVE VECTO-INI TO DATA-AGENDA-CO500.
           MOVE USUARIO-W  TO USUARIO-CO500.
           START COD500 KEY IS NOT < ALT2-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
                 READ COD500 NEXT RECORD AT END
                      MOVE "10" TO ST-COD500
                 NOT AT END
                      MOVE SPACES TO GS-LINDET
                      IF DATA-AGENDA-CO500 > VECTO-FIM OR
                         USUARIO-W <> USUARIO-CO500
                         MOVE "10" TO ST-COD500
                      ELSE
                         IF GS-PENDENTE = 1 AND
                            GS-CHECADO  = 1
                            PERFORM INSERIR-LISTA
                         ELSE
                            IF (GS-PENDENTE    = 1 AND
                                SITUACAO-CO500 = 0) OR
                               (GS-CHECADO     = 1 AND
                                SITUACAO-CO500 = 1)
                               PERFORM INSERIR-LISTA
                            ELSE
                               CONTINUE
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       INSERIR-LISTA SECTION.
           MOVE DATA-AGENDA-CO500  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV TO DATA-E
           MOVE DATA-E             TO GS-LINDET(1: 15)
           MOVE NR-CONTRATO-CO500  TO GS-LINDET(16: 04)

           MOVE NR-CONTRATO-CO500  TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
               MOVE "---"          TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-LINDET(21: 19)

           MOVE CIDADE-CO40        TO CIDADE
           READ CAD010 INVALID KEY
                MOVE "********"    TO NOME-CID
                MOVE "**"          TO UF-CID
           END-READ

           MOVE SPACES             TO GS-LINDET(41:16)
           STRING NOME-CID "-" UF-CID INTO GS-LINDET(41:16)

           MOVE DATA-CO500 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET(58: 14)
           MOVE HORA-CO500(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-CO500(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E           TO GS-LINDET(72: 08)
           MOVE USUARIO-CO500    TO GS-LINDET(80: 08)
           MOVE ITEM-CO500       TO GS-LINDET(88: 03)
           IF SITUACAO-CO500 = 0
              MOVE "PENDENTE" TO GS-LINDET(98: 10)
           ELSE
              MOVE "CHECADO"  TO GS-LINDET(98: 10).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM CARREGA-COD501.

       CARREGA-COD501 SECTION.
           MOVE NR-CONTRATO-CO500  TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE ZEROS            TO SUBITEM-CO501.
           START COD501 KEY IS NOT < CHAVE-CO501 INVALID KEY
                 MOVE "10" TO ST-COD501.
           PERFORM UNTIL ST-COD501 = "10"
              READ COD501 NEXT RECORD AT END MOVE "10" TO ST-COD501
                   NOT AT END
                     IF NR-CONTRATO-CO501 <> NR-CONTRATO-CO500 OR
                        ITEM-CO501 <> ITEM-CO500
                          MOVE "10" TO ST-COD501
                     ELSE
                        MOVE SPACES TO GS-LINDET
                        MOVE ANOTACAO-CO501 TO GS-LINDET(16: 80)
                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP500" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE VECTO-INI TO DATA-AGENDA-CO500.
           MOVE USUARIO-W  TO USUARIO-CO500.
           START COD500 KEY IS NOT < ALT2-CO500 INVALID KEY
                 MOVE "10" TO ST-COD500.
           PERFORM UNTIL ST-COD500 = "10"
              READ COD500 NEXT RECORD AT END MOVE "10" TO ST-COD500
                NOT AT END
                  MOVE SPACES TO GS-LINDET
                  IF DATA-AGENDA-CO500 > VECTO-FIM
                          MOVE "10" TO ST-COD500
                  ELSE
                    IF GS-PENDENTE = 1 AND GS-CHECADO = 1
                       PERFORM INSERIR-LISTA-REL
                    ELSE
                     IF GS-PENDENTE = 1 AND
                        SITUACAO-CO500 = 0 OR
                        GS-CHECADO = 1 AND SITUACAO-CO500 = 1
                        PERFORM INSERIR-LISTA-REL
                     ELSE CONTINUE
                     END-IF
                    END-IF
                  END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.
       INSERIR-LISTA-REL SECTION.
           MOVE DATA-AGENDA-CO500 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV TO DATA-E
           MOVE DATA-E           TO LINDET-REL(1: 15)
           MOVE NR-CONTRATO-CO500 TO LINDET-REL(16: 11)
           MOVE DATA-CO500 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET(58: 14)
           MOVE HORA-CO500(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-CO500(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E           TO LINDET-REL(72: 8)
           MOVE USUARIO-CO500    TO LINDET-REL(80: 8)
           MOVE ITEM-CO500        TO LINDET-REL(88: 3)
           IF SITUACAO-CO500 = 0
              MOVE "PENDENTE" TO LINDET-REL(98: 10)
           ELSE MOVE "CHECADO" TO LINDET-REL(98: 10).
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           PERFORM CARREGA-COD501-IMPR.
       CARREGA-COD501-IMPR SECTION.
           MOVE NR-CONTRATO-CO500  TO NR-CONTRATO-CO501.
           MOVE ITEM-CO500        TO ITEM-CO501.
           MOVE ZEROS            TO SUBITEM-CO501.
           START COD501 KEY IS NOT < CHAVE-CO501 INVALID KEY
                 MOVE "10" TO ST-COD501.
           PERFORM UNTIL ST-COD501 = "10"
              READ COD501 NEXT RECORD AT END MOVE "10" TO ST-COD501
                   NOT AT END
                     IF NR-CONTRATO-CO501 <> NR-CONTRATO-CO500 OR
                        ITEM-CO501 <> ITEM-CO500
                          MOVE "10" TO ST-COD501
                     ELSE
                        MOVE SPACES TO LINDET-REL
                        MOVE ANOTACAO-CO501 TO LINDET-REL(16: 80)
                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 56 PERFORM CABECALHO
                        END-IF
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           MOVE 3 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD500 COD501 COD040 CAD010.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
