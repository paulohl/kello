       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOBXVDLOT.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-05-2010
      *DESCRIÇÃO: Baixa venda Lote e atualiza os campos
      *                fogo-mtg = 7
      *           data-fogo-mtg = data informada pelo usuário

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY CAPX010.
           COPY MTPX020.
           COPY CGPX001.

           SELECT LOG ASSIGN TO ARQUIVO-LOG
                      ORGANIZATION IS LINE SEQUENTIAL
                      ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW040.
           COPY CAPW010.
           COPY MTPW020.
           COPY CGPW001.

       FD LOG.
       01 REG-LOG.
          05 LOG-TEXTO1             PIC X(20).
          05 LOG-TEXTO2             PIC X(20).
          05 LOG-TEXTO3             PIC X(20).
          05 LOG-TEXTO4             PIC X(20).
          05 LOG-TEXTO5             PIC X(20).
          05 LOG-TEXTO6             PIC X(20).


       WORKING-STORAGE SECTION.
           COPY "GALHOBXVDLOT.CPB".
           COPY "GALHOBXVDLOT.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
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
           05  AUX-MES               PIC 9(06) VALUE ZEROS.

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.
       01 WSTEXTO                      PIC X(255) VALUE SPACES.
       77 wsRetorno                    pic s9(08) comp-5 value zeros.

       01  PARAMETROS-W.
           10  USUARIO-W         PIC X(5).
           10  EMPRESA-W         PIC 9(3).
           10  IMPRESSORA-W      PIC 99.
           10  COD-USUARIO-W     PIC 9(3).
           10  SENHA-W           PIC 9(4).
           10  NOME-EMPRESA-W    PIC X(60).

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    accept parametros-w from command-line.
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
               WHEN GS-SUGESTAO-TRUE
                   PERFORM SUGESTAO-CODIGO
               WHEN GS-VALIDA-CODIGO-TRUE
                   PERFORM VALIDA-CODIGO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       SUGESTAO-CODIGO SECTION.
           MOVE FUNCTION NUMVAL(GS-CAMINHO-ARQUIVO) TO EMPRESA-W
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-CODIGO
           MOVE PASSAR-STRING-1(33: 6) TO GS-ACP-CODIGO.

       VALIDA-CODIGO SECTION.
           MOVE GS-ACP-CODIGO TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "Codigo Nao Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE NOME-CG01 TO GS-DESC-CODIGO
                REFRESH-OBJECT PRINCIPAL.


       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDA-CAMINHO SECTION.
           MOVE SPACES TO PATH-COD040 PATH-MTD020

           STRING GS-CAMINHO-ARQUIVO "\COD040" DELIMITED BY "  "
             INTO PATH-COD040

           OPEN I-O COD040
           CLOSE    COD040
           OPEN I-O COD040

           STRING GS-CAMINHO-ARQUIVO "\MTD020" DELIMITED BY "  "
             INTO PATH-MTD020

           OPEN I-O MTD020
           CLOSE    MTD020
           OPEN I-O MTD020

           STRING GS-CAMINHO-ARQUIVO "\CAD010" DELIMITED BY "  "
             INTO PATH-CAD010

           OPEN I-O CAD010
           CLOSE    CAD010
           OPEN I-O CAD010

           STRING GS-CAMINHO-ARQUIVO "\CGD001" DELIMITED BY "  "
             INTO PATH-CGD001

           OPEN I-O CGD001
           CLOSE    CGD001
           OPEN I-O CGD001


           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE GS-ARQUIVO-LOG TO ARQUIVO-LOG
           OPEN EXTEND LOG.

           MOVE "NR-CONTRATO-CO40"  TO LOG-TEXTO1
           MOVE "ALBUM-MTG"         TO LOG-TEXTO2
           MOVE "FOGO-MTG-ANTIGO"   TO LOG-TEXTO3
           MOVE "DATA-FOGO-MTG-ANT" TO LOG-TEXTO4
           MOVE "FOGO-MTG-ATUAL"    TO LOG-TEXTO5
           MOVE "DATA-FOGO-MTG-ATU" TO LOG-TEXTO6

           WRITE REG-LOG

           MOVE SPACES              TO LOG-TEXTO1
                                       LOG-TEXTO2
                                       LOG-TEXTO3
                                       LOG-TEXTO4
                                       LOG-TEXTO5
                                       LOG-TEXTO6.

       ALTERA-LAYOUT SECTION.
           EVALUATE GS-OPCAO
               WHEN 1 PERFORM POR-ALBUM
               WHEN 2 PERFORM POR-MESANO
           END-EVALUATE
           CLOSE CAD010 COD040 MTD020 LOG CGD001

           MOVE "Acabei" TO MENSAGEM
           MOVE "C"      TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           move spaces to wstexto
           string arquivo-log x"00" delimited by "  " into wstexto
           INVOKE UTILITARIO "ExecutarArquivo" using wstexto
                                           returning wsRetorno.

      *    move ds-quit-set to ds-control
      *    perform call-dialog-system.
      *    stop run.

       POR-ALBUM SECTION.
           INITIALIZE REG-COD040
           MOVE GS-NR-CONTRATO TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT LESS NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      IF GS-NR-CONTRATO <> NR-CONTRATO-CO40
                         MOVE "10" TO ST-COD040
                      ELSE
                         MOVE CIDADE-CO40        TO CIDADE
                         READ CAD010 INVALID KEY
                              INITIALIZE REG-CAD010
                         END-READ
                         IF GS-UF = SPACES OR UF-CID
                            INITIALIZE REG-MTD020
                            MOVE NR-CONTRATO-CO40 TO CONTRATO-MTG
                            MOVE GS-ALBUM-INI     TO NRALBUM-MTG
                            START MTD020 KEY IS NOT LESS ALBUM-MTG
                                                             INVALID KEY
                                  MOVE "10" TO ST-MTD020
                            END-START
                            PERFORM UNTIL ST-MTD020 = "10"
                                  READ MTD020 NEXT AT END
                                       MOVE "10" TO ST-MTD020
                                  NOT AT END
                                     IF NR-CONTRATO-CO40 <> CONTRATO-MTG
                                        MOVE "10" TO ST-MTD020
                                     ELSE
                                        IF GS-ALBUM-FIM = 0 OR
                                           NRALBUM-MTG <  GS-ALBUM-FIM
                                           MOVE ALBUM-MTG TO GS-ALBUM
                                           IF FOGO-MTG <> 1 AND 8
                                             IF NAO-GEROU-ALBUM-MTG <> 1
                                                PERFORM ATUALIZAR-MTD020
                                                REFRESH-OBJECT PRINCIPAL
                                             END-IF
                                           END-IF
                                        END-IF
                                     END-IF
                                  END-READ
                            END-PERFORM
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       POR-MESANO SECTION.
           INITIALIZE REG-COD040
           STRING GS-MES-INI(3:4) GS-MES-INI(1:2)
             INTO MESANO-PREV-CO40

           STRING GS-MES-FIM(3:4) GS-MES-FIM(1:2)
             INTO AUX-MES

           START COD040 KEY IS NOT LESS ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      IF MESANO-PREV-CO40 > AUX-MES
                         MOVE "10" TO ST-COD040
                      ELSE
                         MOVE CIDADE-CO40        TO CIDADE
                         READ CAD010 INVALID KEY
                              INITIALIZE REG-CAD010
                         END-READ
                         IF GS-UF = SPACES OR UF-CID
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
                                   IF NR-CONTRATO-CO40 <> CONTRATO-MTG
                                      MOVE "10" TO ST-MTD020
                                   ELSE
                                      MOVE ALBUM-MTG TO GS-ALBUM
                                      IF FOGO-MTG <> 1 AND 8
                                         IF NAO-GEROU-ALBUM-MTG <> 1
                                            PERFORM ATUALIZAR-MTD020
                                            REFRESH-OBJECT PRINCIPAL
                                         END-IF
                                      END-IF
                                   END-IF
                              END-READ
                            END-PERFORM
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       ATUALIZAR-MTD020 SECTION.
           MOVE NR-CONTRATO-CO40   TO LOG-TEXTO1
           MOVE NRALBUM-MTG        TO LOG-TEXTO2
           MOVE SPACES             TO LOG-TEXTO3
           STRING FOGO-MTG " - " POSSE-MTG " - " CODIGO-POSSE-MTG
             INTO LOG-TEXTO3
      *    MOVE FOGO-MTG           TO LOG-TEXTO3
           STRING DATA-FOGO-MTG(7:2) "/"
                  DATA-FOGO-MTG(5:2) "/"
                  DATA-FOGO-MTG(1:4)
             INTO LOG-TEXTO4

           MOVE SPACES TO LOG-TEXTO5
           STRING "7 - 4 - " GS-ACP-CODIGO INTO LOG-TEXTO5
      *    MOVE  7                 TO LOG-TEXTO5
           STRING GS-DATA-VENDA(1:2) "/"
                  GS-DATA-VENDA(3:2) "/"
                  GS-DATA-VENDA(5:4)
             INTO LOG-TEXTO6

           WRITE REG-LOG

           MOVE 4                      TO POSSE-MTG
           MOVE 7                      TO FOGO-MTG
           MOVE GS-ACP-CODIGO          TO CODIGO-POSSE-MTG
           STRING GS-DATA-VENDA(5:4)
                  GS-DATA-VENDA(3:2)
                  GS-DATA-VENDA(1:2) INTO DATA-FOGO-MTG
           REWRITE REG-MTD020.

       EXIBIR-MENSAGEM SECTION.
           MOVE 1         TO GS-FLAG-CRITICA
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
           MOVE "GALHOBXVDLOT" TO DS-SET-NAME
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
