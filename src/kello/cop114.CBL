       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP114.
      *DATA: 28/04/2007
      *AUTOR:  ALFREDO SAVIOLLI NETO
      *FUNÇÃO: DEFINIR NOME

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX010.

           COPY COPX040.

           COPY COPX060.

           COPY COPX061.

           COPY COPX003.

           COPY IEPX011.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW010.

           COPY COPW040.

           COPY COPW060.

           COPY COPW061.

           COPY COPW003.

           COPY IEPW011.

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-CONTRATO        PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-ITEM            PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-UF              PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CODCID          PIC X(15).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CIDADE          PIC X(30).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CODEVEN         PIC X(15).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-EVENTO          PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-REALIZACAO      PIC X(20).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-CURSO           PIC X(12).
          05 EXCEL-PULA            PIC X(02).


       WORKING-STORAGE SECTION.
           COPY "COP114.CPB".
           COPY "COP114.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  NR-CURSOS             PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 DATA-INI.
          05 ANO-INI                 PIC 9(04).
          05 MES-INI                 PIC 9(02).
          05 DIA-INI                 PIC 9(02).

       01 DATA-FIM.
          05 ANO-FIM                 PIC 9(04).
          05 MES-FIM                 PIC 9(02).
          05 DIA-FIM                 PIC 9(02).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           ACCEPT VARIA-W FROM TIME.

           OPEN INPUT CAD010 COD003 COD040 COD060 COD061 IED011
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
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-EXCEL-FLG-TRUE
                   PERFORM GERAR-EXCEL
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL

           STRING "\ARQUIVOS\EXC-" GS-REALIZACAO-INI "-"
                                   GS-REALIZACAO-FIM
             INTO ARQUIVO-EXCEL

           OPEN OUTPUT EXCEL

           MOVE "CONTRATO"         TO EXCEL-CONTRATO
           MOVE "ITEM"             TO EXCEL-ITEM
           MOVE "UF"               TO EXCEL-UF
           MOVE "COD.CIDADE"       TO EXCEL-CODCID
           MOVE "CIDADE"           TO EXCEL-CIDADE
           MOVE "COD.EVENTO"       TO EXCEL-CODEVEN
           MOVE "EVENTO"           TO EXCEL-EVENTO
           MOVE "DT.REALIZAÇÃO"    TO EXCEL-REALIZACAO
           MOVE "CURSO"            TO EXCEL-CURSO
           MOVE X"0DA0"            TO EXCEL-PULA
           WRITE REG-EXCEL

           MOVE GS-REALIZACAO-INI(1:2)  TO DIA-INI
           MOVE GS-REALIZACAO-INI(3:2)  TO MES-INI
           MOVE GS-REALIZACAO-INI(5:4)  TO ANO-INI

           MOVE GS-REALIZACAO-FIM(1:2)  TO DIA-FIM
           MOVE GS-REALIZACAO-FIM(3:2)  TO MES-FIM
           MOVE GS-REALIZACAO-FIM(5:4)  TO ANO-FIM

           INITIALIZE REG-COD060

           MOVE DATA-INI                TO DATAREALIZA-CO60
           START COD060 KEY IS NOT LESS DATAREALIZA-CO60 INVALID KEY
               MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
               READ COD060 NEXT AT END
                   MOVE "10" TO ST-COD060
               NOT AT END
                   IF DATAREALIZA-CO60 > DATA-FIM
                      MOVE "10" TO ST-COD060
                   ELSE
                      MOVE NR-CONTRATO-CO60   TO EXCEL-CONTRATO
                      MOVE ITEM-CO60          TO EXCEL-ITEM

                      MOVE CODEVENTO-CO60     TO EXCEL-CODEVEN
                                                 CODIGO-CO03
                      READ COD003 INVALID KEY
                           MOVE "******"      TO NOME-CO03
                      END-READ
                      MOVE NOME-CO03          TO EXCEL-EVENTO
                      MOVE SPACES TO EXCEL-REALIZACAO
                      STRING DATAREALIZA-CO60(7:2) "/"
                             DATAREALIZA-CO60(5:2) "/"
                             DATAREALIZA-CO60(1:4)
                             INTO EXCEL-REALIZACAO

                      MOVE NR-CONTRATO-CO60   TO NR-CONTRATO-CO40
                      READ COD040 INVALID KEY
                           INITIALIZE REG-COD040
                      END-READ

                      MOVE CIDADE-CO40        TO CIDADE
                      READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                      END-READ

                      MOVE CIDADE-CO40        TO EXCEL-CODCID
                      MOVE UF-CID             TO EXCEL-UF
                      MOVE NOME-CID           TO EXCEL-CIDADE

                      INITIALIZE REG-COD061 NR-CURSOS
                      MOVE NR-CONTRATO-CO60   TO NR-CONTRATO-CO61
                      MOVE ITEM-CO60          TO ITEM-CO61
                      START COD061 KEY IS NOT LESS CHAVE-CO61 INVALID
                                                                    KEY
                          MOVE "10" TO ST-COD061
                      END-START
                      PERFORM UNTIL ST-COD061 = "10"
                          READ COD061 NEXT AT END
                               MOVE "10" TO ST-COD061
                          NOT AT END
                               IF NR-CONTRATO-CO61 <> NR-CONTRATO-CO60
                                  OR ITEM-CO61 <> ITEM-CO60
                                  MOVE "10" TO ST-COD061
                               ELSE
                                  ADD 1 TO NR-CURSOS
                                  MOVE CURSO-CO61  TO CODIGO-IE11
                                  READ IED011 INVALID KEY
                                       MOVE "****" TO NOME-IE11
                                  END-READ
                                  MOVE NOME-IE11   TO EXCEL-CURSO
                               END-IF
                          END-READ
                      END-PERFORM

                      IF NR-CURSOS = 0
                         MOVE "NENHUM" TO EXCEL-CURSO
                      ELSE
                         IF NR-CURSOS > 1
                            MOVE "VÁRIOS" TO EXCEL-CURSO
                         END-IF
                      END-IF

                      MOVE X"0DA0"            TO EXCEL-PULA
                      WRITE REG-EXCEL
                   END-IF
               END-READ
           END-PERFORM

           CLOSE EXCEL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP114" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 COD061 IED011
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

