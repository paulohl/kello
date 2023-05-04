       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHONRFORM.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 13-05-2010
      *DESCRIÇÃO: ALTERA O NR ATUAL DE FORMANDOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY MTPX019.

           SELECT LOG ASSIGN TO ARQUIVO-LOG
                      ORGANIZATION IS LINE SEQUENTIAL
                      ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW040.
           COPY MTPW019.

       FD LOG.
       01 REG-LOG.
          05 LOG-TEXTO1             PIC X(20).
          05 LOG-TEXTO2             PIC X(20).
          05 LOG-TEXTO3             PIC X(20).


       WORKING-STORAGE SECTION.
           COPY "GALHONRFORM.CPB".
           COPY "GALHONRFORM.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  GRAVA-W              PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  parametros-w          pic x(40)    value spaces.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  WS-DATA-NUM           PIC 9(08)    VALUE ZEROS.
           05  WS-DATA.
               10  WS-ANO                PIC 9(04).
               10  WS-MES                PIC 9(02).
               10  WS-DIA                PIC 9(02).
           05 MESANO-FIM             PIC 9(06).
           05 QTDE-FORMANDOS         PIC 9(06) VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.
       01 WSTEXTO                      PIC X(255) VALUE SPACES.
       77 wsRetorno                    pic s9(08) comp-5 value zeros.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.

           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-VALIDA-CAMINHO-TRUE
                   PERFORM VALIDA-CAMINHO
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

       VALIDA-CAMINHO SECTION.
           MOVE SPACES TO PATH-COD040 PATH-MTD019

           STRING GS-CAMINHO-ARQUIVO "\COD040" DELIMITED BY "  "
             INTO PATH-COD040

           OPEN I-O COD040
           CLOSE    COD040
           OPEN I-O COD040

           STRING GS-CAMINHO-ARQUIVO "\MTD019" DELIMITED BY "  "
             INTO PATH-MTD019

           OPEN I-O MTD019
           CLOSE    MTD019
           OPEN I-O MTD019

           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE GS-ARQUIVO-LOG TO ARQUIVO-LOG
           OPEN EXTEND LOG.

           MOVE "NR-CONTRATO-CO40"  TO LOG-TEXTO1
           MOVE "QTDE-FORM-CO40"    TO LOG-TEXTO2
           MOVE "QTDE-FORMANDOS"    TO LOG-TEXTO3

           WRITE REG-LOG.

       ALTERA-LAYOUT SECTION.
           INITIALIZE REG-COD040
           STRING GS-MESANO-INI(3:4) GS-MESANO-INI(1:2) INTO
                                                        MESANO-PREV-CO40
           STRING GS-MESANO-FIM(3:4) GS-MESANO-FIM(1:2) INTO
                                                        MESANO-FIM
           START COD040 KEY IS NOT LESS ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      IF MESANO-PREV-CO40 > MESANO-FIM
                         MOVE "10" TO ST-COD040
                      ELSE
                         INITIALIZE REG-MTD019
                                    QTDE-FORMANDOS
                         MOVE NR-CONTRATO-CO40 TO CONTRATO-MT19
                         START MTD019 KEY IS NOT LESS ALBUM-MT19
                                                             INVALID KEY
                              MOVE "10" TO ST-MTD019
                         END-START
                         PERFORM UNTIL ST-MTD019 = "10"
                              READ MTD019 NEXT AT END
                                   MOVE "10" TO ST-MTD019
                              NOT AT END
                                   IF NR-CONTRATO-CO40 <> CONTRATO-MT19
                                      MOVE "10" TO ST-MTD019
                                   ELSE
                                      MOVE ALBUMMT19 TO GS-ALBUM
                                      REFRESH-OBJECT PRINCIPAL
                                      ADD 1 TO QTDE-FORMANDOS
                                   END-IF
                              END-READ
                         END-PERFORM
                         IF QTDE-FORMANDOS > 0
                            PERFORM ATUALIZAR-COD040
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM
           CLOSE COD040 MTD019 LOG

           MOVE "Acabei" TO MENSAGEM
           MOVE "C"      TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           move spaces to wstexto
           string arquivo-log x"00" delimited by "  " into wstexto
           INVOKE UTILITARIO "ExecutarArquivo" using wstexto
                                           returning wsRetorno.

           move ds-quit-set to ds-control
           perform call-dialog-system.
           stop run.

       ATUALIZAR-COD040 SECTION.
           MOVE NR-CONTRATO-CO40   TO LOG-TEXTO1
           MOVE QTDE-FORM-CO40     TO LOG-TEXTO2
           MOVE QTDE-FORMANDOS     TO LOG-TEXTO3

           WRITE REG-LOG

           MOVE QTDE-FORMANDOS  TO QTDE-FORM-CO40
           REWRITE REG-COD040.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".

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
           MOVE ds-push-set   TO DS-CONTROL
           MOVE "GALHONRFORM" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           move spaces to wstexto
           string arquivo-log x"00" delimited by "  " into wstexto
           INVOKE UTILITARIO "ExecutarArquivo" using wstexto
                                           returning wsRetorno.

           move ds-quit-set to ds-control
           perform call-dialog-system.
           stop run.
