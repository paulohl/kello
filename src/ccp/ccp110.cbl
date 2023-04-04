       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP110.
      *DATA: 18/02/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: GERA ARQUIVO CCD110 - ATRAVÉS DO ARQUIVO DE REPORTAGEM
      *FUNCAO:   Pode ser gerado + de uma vez, pois primeiro o sistema
      *          vai percorrer o arquivo ccd110, e eliminar todos os
      *          que tiverem mesano = o solicitado e lcto-cta-corr = 0
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX005.
           COPY CCPX110.
           COPY REPX100.
           COPY REPX103.
           SELECT WORK ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW005.
       COPY CCPW110.
       COPY REPW100.
       COPY REPW103.
       FD  WORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS VARIA-W.
       01  REG-WORK.
           05 CHAVE-WK.
              10 CODIGO-WK        PIC 9(6).
              10 DOCTO-WK         PIC X(10).
           05 TOT-VLR-REPORT-WK   PIC 9(8)V99.

       WORKING-STORAGE SECTION.
           COPY "CCP110.CPB".
           COPY "CCP110.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  CONT                  PIC 9(03)    VALUE ZEROS.
           05  IND                   PIC 9(03)    VALUE ZEROS.

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
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9999.
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
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
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
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
           MOVE "CGD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE "CCD110"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD110.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED103.

           OPEN INPUT CGD005 RED100 RED103.
           OPEN I-O   CCD110.

           IF ST-CCD110 = "35"
              CLOSE    CCD110    OPEN OUTPUT CCD110  CLOSE CCD110
              OPEN I-O CCD110.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED103 <> "00"
              MOVE "ERRO ABERTURA RED103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED103 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM LEITURA-RED300
                    PERFORM INICIO-WORK
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      ***********************************************
       LEITURA-RED300 SECTION.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-MESANO-BASE TO MESANO-W
           MOVE MES-WW         TO MES-II
           MOVE ANO-WW         TO ANO-II
           MOVE MESANO-I       TO ANOMES-R100.
           ACCEPT VARIA-W FROM TIME.

           MOVE ZEROS TO CONT
                         IND

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           START RED100 KEY IS NOT < ANOMES-R100 INVALID KEY
                 MOVE "10" TO ST-RED100.

           PERFORM UNTIL ST-RED100 = "10"
              READ RED100 NEXT RECORD AT END
                   MOVE "10" TO ST-RED100
              NOT AT END
                   IF ANOMES-R100 <> MESANO-I
                      MOVE "10" TO ST-RED100
                   ELSE
                      IF LCTO-CTA-CORR-R100 <> 1
                         PERFORM LEITURA-RED103
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.

       LEITURA-RED103 SECTION.
           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-RED103

           MOVE DOCTO-R100 TO DOCTO-R103
                              GS-EXIBE-CODIGO
                              DOCTO-WK

           MOVE ZEROS      TO SEQ-R103.
           START RED103 KEY IS NOT < ALT1-R103 INVALID KEY
                MOVE "10" TO ST-RED103.

           PERFORM UNTIL ST-RED103 = "10"
              READ RED103 NEXT RECORD AT END
                   MOVE "10" TO ST-RED103
              NOT AT END
                  IF DOCTO-R103 <> DOCTO-R100
                     MOVE "10" TO ST-RED103
                  ELSE
                     MOVE CODIGO-R103        TO CODIGO-CG05
                                                CODIGO-WK
                     READ CGD005 INVALID KEY
                          MOVE ZEROS         TO EMPRESA-CG05
                     END-READ
                     IF EMPRESA-CG05 <> ZEROS
                        MOVE EMPRESA-CG05    TO CODIGO-WK
                     ELSE
                        MOVE CODIGO-R103     TO CODIGO-WK
                     END-IF
      *              MOVE REPRES-R303 TO CODIGO-R030 CODIGO-WK
      *              READ RED030 INVALID KEY
      *                   MOVE ZEROS TO EMPRESA-R030
      *              END-READ
      *              IF EMPRESA-R030 NOT = ZEROS
      *                 MOVE EMPRESA-R030 TO CODIGO-R090
      *                 READ RED090 INVALID KEY
      *                      CONTINUE
      *                 NOT INVALID KEY
      *                      MOVE CODIGO-REP-R090 TO CODIGO-WK
      *                 END-READ
      *              END-IF
                     READ WORK INVALID KEY
                        PERFORM GRAVA-WORK
                     NOT INVALID KEY
                        ADD VLR-REPORT-R103   TO TOT-VLR-REPORT-WK
                        REWRITE REG-WORK
                     END-READ
                  END-IF
              END-READ
           END-PERFORM.
       GRAVA-WORK SECTION.
           MOVE VLR-REPORT-R103  TO TOT-VLR-REPORT-WK
           WRITE REG-WORK
           END-WRITE.
       INICIO-WORK SECTION.
      *    1o. eliminar todos os registros do ccd110 cujo
      *    mesano = solicitado

           CLOSE    CCD110
           OPEN I-O CCD110

           INITIALIZE REG-CCD110
           MOVE MESANO-I TO MESANO-BASE-CC110.
           MOVE ZEROS    TO CODIGO-CC110.
           START CCD110 KEY IS NOT < CHAVE-CC110 INVALID KEY
                 MOVE "10" TO ST-CCD110.

           PERFORM UNTIL ST-CCD110 = "10"
              READ CCD110 NEXT RECORD AT END
                   MOVE "10" TO ST-CCD110
              NOT AT END
                   IF MESANO-BASE-CC110 <> MESANO-I
                      MOVE "10" TO ST-CCD110
                   ELSE
                      DELETE CCD110
                   END-IF
              END-READ
           END-PERFORM.

           CLOSE      WORK.
           OPEN INPUT WORK.
           MOVE ZEROS TO CHAVE-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                 MOVE CODIGO-WK TO GS-EXIBE-CODIGO
                 MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 IF TOT-VLR-REPORT-WK > ZEROS
                    MOVE CODIGO-WK         TO CODIGO-CC110
                    MOVE MESANO-I          TO MESANO-BASE-CC110
                    MOVE DATA-DIA-I        TO DATA-MOVTO-CC110
                    MOVE DOCTO-WK          TO DOCTO-CC110
                    MOVE USUARIO-W         TO DIGITADOR-CC110
                    MOVE TOT-VLR-REPORT-WK TO VALOR-CREDITO-CC110
                    MOVE ZEROS             TO VALOR-LIBERADO-CC110
                    MOVE ZEROS             TO ATUALIZADO-CC-CC110
                    WRITE REG-CCD110 INVALID KEY
                          MOVE "CCD110"    TO GS-MENSAGEM-ERRO(15: 7)
                          MOVE "ST-CCD110" TO GS-MENSAGEM-ERRO(23: 2)
                          PERFORM ERRO-GRAVACAO
                    END-WRITE
                 END-IF
              END-READ
           END-PERFORM.

           CLOSE       WORK.
           DELETE FILE WORK.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP110" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RED100 RED103 CCD110 CGD005.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
