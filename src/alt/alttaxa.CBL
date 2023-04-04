       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTTAXA.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 28-04-2005
      *DESCRIÇÃO: ALTERA TAXA ADMINISTRATIVA DOS CARTÕES
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CGPX020.
           COPY RCPX100.
           COPY RCPX101.
           COPY CRPX020.


       DATA DIVISION.
       FILE SECTION.

       COPY CGPW020.
       COPY RCPW100.
       COPY RCPW101.
       COPY CRPW020.



       WORKING-STORAGE SECTION.
           COPY "ALTTAXA.CPB".
           COPY "ALTTAXA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  GRAVA-W              PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 DATA-INI.
          05 INI-ANO                 PIC 9(04).
          05 INI-MES                 PIC 9(02).
          05 INI-DIA                 PIC 9(02).

       01 DATA-FIM.
          05 FIM-ANO                 PIC 9(04).
          05 FIM-MES                 PIC 9(02).
          05 FIM-DIA                 PIC 9(02).

       LINKAGE SECTION.
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
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.

           MOVE EMPRESA-W          TO EMP-REC

           MOVE "CGD020" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020
           MOVE "RCD100" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "RCD101" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101
           MOVE "CRD020" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020
           OPEN INPUT CGD020 RCD100
           OPEN I-O   RCD101 CRD020

           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.


           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-LE-CARTAO-TRUE
                   PERFORM LER-CARTAO
               WHEN GS-POP-UP-TRUE
                   PERFORM POP-UP
               WHEN GS-ALTERA-FLG-TRUE
                   PERFORM ALTERA-LAYOUT
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       LER-CARTAO SECTION.
           MOVE GS-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020.
           MOVE CODIGO-CG20              TO GS-CARTAO
           MOVE NOME-CG20                TO GS-DESC-CARTAO
           MOVE TAXA-CREDITO-CG20        TO GS-TAXA.

       POP-UP SECTION.
           CALL "CGP020T" USING PASSAR-STRING-1
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-CARTAO
           PERFORM LER-CARTAO.

       ALTERA-LAYOUT SECTION.
           CLOSE RCD101 CRD020
           OPEN I-O RCD101 CRD020
           INITIALIZE REG-RCD100
           STRING GS-DATA-MOVTO-I(1:2) INTO INI-DIA
           STRING GS-DATA-MOVTO-I(3:4) INTO INI-MES
           STRING GS-DATA-MOVTO-I(5:8) INTO INI-ANO

           STRING GS-DATA-MOVTO-F(1:2) INTO FIM-DIA
           STRING GS-DATA-MOVTO-F(3:4) INTO FIM-MES
           STRING GS-DATA-MOVTO-F(5:8) INTO FIM-ANO

           MOVE "RCD100" TO GS-ARQUIVO1
           MOVE ZEROS    TO GS-ALTERADOS1 GS-ALTERADOS2

           MOVE DATA-INI TO DATA-MOVTO-REC

           START RCD100 KEY IS NOT < ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
             READ RCD100 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD100
             NOT AT END
                  IF DATA-MOVTO-REC > DATA-FIM
                     MOVE "10" TO ST-RCD100
                  ELSE
                     MOVE DATA-MOVTO-REC        TO GS-DATA-MOVTO1
                     MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     INITIALIZE REG-RCD101
                     MOVE ALBUM-REC             TO ALBUM-REC1
                     START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                           MOVE "10" TO ST-RCD101
                     END-START
                     PERFORM UNTIL ST-RCD101 = "10"
                          READ RCD101 NEXT AT END
                              MOVE "10" TO ST-RCD101
                          NOT AT END
                              IF ALBUM-REC1 <> ALBUM-REC
                                 MOVE "10" TO ST-RCD101
                              ELSE
                                 IF TIPO-REC1 = 6
                                    IF CARTAO-CRED-REC1 = GS-CARTAO
                                       ADD 1 TO GS-ALTERADOS1
                                       MOVE GS-TAXA TO
                                            TAXA-ADMINIST-CREDITO-REC1
                                       REWRITE REG-RCD101
                                    END-IF
                                 END-IF
                              END-IF
                          END-READ
                     END-PERFORM
             END-READ
           END-PERFORM

           CLOSE RCD101

           MOVE "CRD020" TO GS-ARQUIVO2

           MOVE DATA-INI TO DATA-MOVTO-CR20

           START CRD020 KEY IS NOT < DATA-MOVTO-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
             READ CRD020 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD020
             NOT AT END
                  IF DATA-MOVTO-CR20 > DATA-FIM
                     MOVE "10" TO ST-CRD020
                  ELSE
                     MOVE DATA-MOVTO-CR20       TO GS-DATA-MOVTO2
                     MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     IF TIPO-DOCTO-CR20 = 4
                        IF CARTAO-CRED-CR20 = GS-CARTAO
                           ADD 1 TO GS-ALTERADOS2
                           MOVE GS-TAXA TO TAXA-ADMINIST-CREDITO-CR20
                           REWRITE REG-CRD020
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM

           CLOSE CRD020.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "ALTTAXA" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD020 RCD100
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
