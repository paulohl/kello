       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP102.
      *DATA: 24/01/2001
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: GERA CREDITO DE VENDEDOR - CONTA CORRENTE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
      *    COPY MTPX020.

           SELECT  MTD020 ASSIGN       TO     PATH-MTD020
                          ORGANIZATION IS          INDEXED
                          ACCESS MODE  IS          DYNAMIC
                          STATUS       IS       ST-MTD020
                          LOCK MODE    IS        AUTOMATIC
                          WITH LOCK    ON           RECORD
                          RECORD KEY   IS       ALBUM-MTG
                          ALTERNATE    RECORD       KEY IS
                                              CHAVE-MTG =
                                              DATAMOV-MTG,
                                                ALBUM-MTG
                          ALTERNATE    RECORD       KEY IS
                                        ANOMES-VISITA-MTG
                                           WITH DUPLICATES
                          ALTERNATE    RECORD       KEY IS
                                                ALT-MTG =
                                         DATAROMANEIO-MTG,
                                                ALBUM-MTG.

           COPY RCPX100.
           COPY RCPX101.
           COPY CCPX120.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS COD-VENDEDOR-WK.

           SELECT ALBUNS ASSIGN TO "ALBUNS-RCP102.TXT"
                         ORGANIZATION IS LINE SEQUENTIAL
                         ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
      *COPY MTPW020.

      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  MTD020.
       01  REG-MTD020.
           05  ALBUM-MTG.
               10  CONTRATO-MTG    PIC 9(4).
               10  NRALBUM-MTG     PIC 9(4).
           05  DATAMOV-MTG         PIC 9(8).
           05  QT-ESTOJO-MTG       PIC 9.
           05  QT-ENCADER-MTG      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG       PIC 9999.
           05  QT-FOTOS-MTG        PIC 9999.
           05  QT-FITAS-MTG        PIC 9.
           05  QT-POSTER-MTG       PIC 9.
           05  QT-PORTA-FITA-MTG   PIC 9.
           05  QT-FOTO-CD-MTG      PIC 99.
           05  QT-MOLDURA-MTG      PIC 99.
           05  QT-PORTA-DVD-MTG    PIC 99.
           05  FOGO-MTG            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG   PIC 9(6).
           05  VISITA-MTG          PIC 999.
           05  POSSE-MTG           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem  4-revendido
           05  CODIGO-POSSE-MTG    PIC 9(6).
           05  QT-DVD-MTG          PIC 9(1).
           05  NAO-GEROU-ALBUM-MTG PIC 9(1).
           05  DATAROMANEIO-MTG    PIC 9(8).
           05  QT-BOOK-MTG         PIC 9(2).
           05  FILLER              PIC X(38).

       COPY RCPW100.
       COPY RCPW101.
       COPY CCPW120.

       FD  WORK.
       01  REG-WORK.
           05  COD-VENDEDOR-WK       PIC 9(6).
           05  VLR-COMISSAO-WK       PIC 9(8)V99.
           05  VLR-COMISSAO-DEF-WK   PIC 9(8)V99.

       FD ALBUNS.
       01 REG-ALBUNS.
          05 ALBUM                 PIC 9(08).
          05 FILLER                PIC X(02).
          05 COD-VENDEDOR          PIC 9(06).


       WORKING-STORAGE SECTION.
           COPY "RCP102.CPB".
           COPY "RCP102.CPY".
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
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-DATA              PIC 9(08).
           05  AUX-DATA-R REDEFINES AUX-DATA.
               10 AUX-MESANO         PIC 9(06).
               10 AUX-DIA            PIC 9(02).
           05  ALBUMW.
               10  CONTRATO-W        PIC 9(4).
               10  SEQ-W             PIC 9(4).
           05  ALBUM-W REDEFINES ALBUMW PIC 9(8).
           05  PASSAR-STRING-1       PIC X(65).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.

           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CGD001 MTD020 RCD100 RCD101.
           OPEN I-O   CCD120
           CLOSE      CCD120
           OPEN INPUT CCD120.
           IF ST-CCD120 = "35" CLOSE CCD120  OPEN OUTPUT CCD120
                               CLOSE CCD120  OPEN I-O CCD120.

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
           IF ST-CCD120 <> "00"
              MOVE "ERRO ABERTURA CCD120: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD120 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
               WHEN GS-LIMPAR-FLG-TRUE
                   IF USUARIO-W <> "ANDER"
                      move "Somente o usuário ANDER pode excluir" to
                      mensagem
                      move "C" to tipo-msg
                      perform exibir-mensagem
                   else
                      move "Deseja realmente excluir os dados do CCD120?
      -                    "" to mensagem
                      move "Q" to tipo-msg
                      perform exibir-mensagem
                      if resp-msg = "S"
                         perform limpar-ccd120
                      end-if
                   END-IF
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       limpar-ccd120 section.
           close      ccd120
           open i-o   ccd120

           move spaces to mensagem
           initialize reg-ccd120
           move gs-mesano(1: 2)   to mesano-i(5: 2)
           move gs-mesano(3: 4)   to mesano-i(1: 4)
           move mesano-i          to mesano-base-cc120
           start ccd120 key is not less chave-cc120 invalid key
                move "10" to st-ccd120.

           perform until st-ccd120 = "10"
                read ccd120 next at end
                     move "10" to st-ccd120
                not at end
                     if mesano-i <> mesano-base-cc120
                        move "10" to st-ccd120
                     else
                        if atualizado-cc-cc120 <> 1
                           delete ccd120 invalid key
                               move "Erro ao excluir o CCD120"
                                        to mensagem
                               move "C" to tipo-msg
                               perform exibir-mensagem
                           end-delete
                        end-if
                     end-if
                end-read
           end-perform

           if mensagem equal spaces
              move "Dados limpos com sucesso" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.

           close      ccd120
           open input ccd120.


       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

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
           MOVE GS-VECTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.
           MOVE GS-MOVTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-INI.
           MOVE GS-MOVTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-FIM.

       GRAVA-WORK SECTION.
           CLOSE WORK
           OPEN OUTPUT WORK ALBUNS

           CLOSE WORK
           OPEN I-O WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM INVERTE-DATA.

           INITIALIZE REG-RCD100

           EVALUATE GS-OPCAO
              WHEN 1 PERFORM POR-DTMOVTO
              WHEN 2 PERFORM POR-DTVECTO
           END-EVALUATE

           CLOSE ALBUNS

           PERFORM GERA-CCD120.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

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
           INITIALIZE REG-RCD101.
           MOVE ZEROS TO ALBUM
           MOVE ALBUM-REC     TO ALBUM-REC1
           MOVE VECTO-INI     TO VENCTO-REC1
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT RECORD AT END
                    MOVE "10" TO ST-RCD101
               NOT AT END
                  IF ALBUM-REC1 <> ALBUM-REC OR
                     VENCTO-REC1 > VECTO-FIM
                     MOVE "10" TO ST-RCD101
                  ELSE
                     IF DTA-BAIXA-REC1 NOT > 0
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
           MOVE VENDEDOR-REC         TO COD-VENDEDOR-WK
           READ WORK INVALID KEY
               MOVE VALOR-COMISSAO     TO VLR-COMISSAO-WK
               MOVE VALOR-COMISSAO-DEF TO VLR-COMISSAO-DEF-WK
               WRITE REG-WORK
               END-WRITE
           NOT INVALID KEY
               ADD VALOR-COMISSAO     TO VLR-COMISSAO-WK
               ADD VALOR-COMISSAO-DEF TO VLR-COMISSAO-DEF-WK
               REWRITE REG-WORK
               END-REWRITE.

           IF ALBUM-REC <> ALBUM
              MOVE ALBUM-REC TO ALBUM
              MOVE VENDEDOR-REC TO COD-VENDEDOR
              WRITE REG-ALBUNS
           END-IF.



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
           ELSE
             CALL "GRTIME" USING PARAMETROS-GRTIME
             COMPUTE VLR-DEFLACAO = (VALOR-REC1 / ((GRTIME-DAYS-FINAL *
                               (GS-TAXA / 30) / 100) + 1)).


       GERA-CCD120 SECTION.
           CLOSE        CCD120
           OPEN I-O     CCD120
           MOVE ZEROS TO COD-VENDEDOR-WK.
           START WORK KEY IS NOT < COD-VENDEDOR-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                  MOVE "10" TO ST-CCD120
             NOT AT END
                 MOVE COD-VENDEDOR-WK   TO CODIGO-CC120
                 MOVE GS-MESANO(1: 2)   TO MESANO-I(5: 2)
                 MOVE GS-MESANO(3: 4)   TO MESANO-I(1: 4)
                 MOVE MESANO-I          TO MESANO-BASE-CC120
                 READ CCD120 INVALID KEY
                      CONTINUE
                 NOT INVALID KEY
                     DELETE CCD120
                     MOVE COD-VENDEDOR-WK   TO CODIGO-CC120
                     MOVE MESANO-I          TO MESANO-BASE-CC120
                 END-READ
                 MOVE COD-VENDEDOR-WK   TO CODIGO-CC120
                 MOVE DATA-MOVTO-I      TO DATA-MOVTO-CC120
                 MOVE SPACES            TO DOCTO-CC120
                 MOVE ZEROS             TO VALOR-LIBERADO-CC120
                                           ATUALIZADO-CC-CC120
                 MOVE USUARIO-W         TO DIGITADOR-CC120
                 IF GS-TIPO-REL <> 1
                    MOVE VLR-COMISSAO-WK     TO VALOR-CREDITO-CC120
                 ELSE
                    MOVE VLR-COMISSAO-DEF-WK TO VALOR-CREDITO-CC120
                 END-IF
                 WRITE REG-CCD120
                 END-WRITE
             END-READ
           END-PERFORM
           CLOSE        CCD120
           OPEN INPUT   CCD120.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP102" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 MTD020 RCD100 RCD101 CCD120 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
