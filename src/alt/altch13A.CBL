       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTCH13A.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 06/03/2003
      *DESCRIÇÃO: ALTERA LAYOUT CHD013 -
      * INCLUI CAMPO DATA-ENTRADA-D  COM FIXO 01/03/2003
      *   E CASO A DATA-COMPRA ESTIVER EM BRANCO PREENCHER COM A DATAREC
      *    (RCD100) -
      *   INVERTE DATA-REAPRES DATA-COMPRA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           SELECT CHD013 ASSIGN TO PATH-CHD013
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CHD013
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH13 = DATA-MOVTO-CH13
                                              SEQ-CH13
                  ALTERNATE RECORD KEY IS DATA-RECTO-CH13
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-CH13
                            WITH DUPLICATES.

           SELECT CHD013A ASSIGN TO PATH-CHD013A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CHD013A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH13A = DATA-MOVTO-CH13A SEQ-CH13A
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-CH13A
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-CH13A
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-CH13A
                            WITH DUPLICATES.

           COPY RCPX100.
           COPY CHPX010.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
      *    arquivo de cheques devolvidos(COMPLEMENTO CHD010)
       FD  CHD013.
       01  REG-CHD013.
           05  DATA-MOVTO-CH13            PIC 9(8).
           05  SEQ-CH13                    PIC 9(4).
           05  ALINEA-CH13                 PIC 99.
           05  DATA-COMPRA-CH13           PIC 9(08)     COMP-3.
           05  DATA-APRES-CH13            PIC 9(08)     COMP-3.
           05  DATA-REAPRES-CH13           PIC 9(08)     COMP-3.
           05  DATA-RECTO-CH13            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CH13             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CH13             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CH13          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CH13           PIC X(10).

      *    arquivo de cheques devolvidos(COMPLEMENTO CHD010)
       FD  CHD013A.
       01  REG-CHD013A.
           05  DATA-MOVTO-CH13A            PIC 9(8).
           05  SEQ-CH13A                   PIC 9(4).
           05  ALINEA-CH13A                PIC 99.
      *    DATA-ENTRADA INVERTIDA AAAAMMDD
           05  DATA-ENTRADA-CH13A          PIC 9(8)      COMP-3.
           05  DATA-COMPRA-CH13A           PIC 9(8)      COMP-3.
      *    DATA-APRES-CH13A INVERTIDA AAAAMMDD
           05  DATA-APRES-CH13A            PIC 9(8)      COMP-3.
           05  DATA-REAPRES-CH13A          PIC 9(08)     COMP-3.
      *    DATA-RECTO-CH13A INVERTIDA AAAAMMDD
           05  DATA-RECTO-CH13A            PIC 9(8)      COMP-3.
           05  VLR-JUROS-CH13A             PIC 9(6)V99   COMP-3.
           05  VLR-MULTA-CH13A             PIC 9(6)V99   COMP-3.
           05  VLR-DESCONTO-CH13A          PIC 9(6)V99   COMP-3.
           05  FORMA-PAGTO-CH13A           PIC X(10).
       COPY RCPW100.
       COPY CHPW010.

       WORKING-STORAGE SECTION.
           COPY "ALTCH13A.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD013A            PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  GRAVA-W              PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  parametros-w          pic x(40)    value spaces.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  WS-DATA-NUM           PIC 9(08)    VALUE ZEROS.
           05  WS-DATA.
               10  WS-ANO                PIC 9(04).
               10  WS-MES                PIC 9(02).
               10  WS-DIA                PIC 9(02).

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
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CHD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CHD013A" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013A.
           MOVE "RCD100" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CHD010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           OPEN INPUT CHD013 RCD100 CHD010.
           OPEN OUTPUT CHD013A.
           CLOSE CONTROLE.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-ALTERA-FLG-TRUE
                   PERFORM ALTERA-LAYOUT
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       ALTERA-LAYOUT SECTION.
           MOVE ZEROS TO DATA-MOVTO-CH13 SEQ-CH13.
           START CHD013 KEY IS NOT < CHAVE-CH13 INVALID KEY
                 MOVE "10" TO ST-CHD013.
           PERFORM UNTIL ST-CHD013 = "10"
             READ CHD013 NEXT RECORD AT END MOVE "10" TO ST-CHD013
               NOT AT END
                 MOVE DATA-MOVTO-CH13       TO GS-DATA-MOVTO
                 MOVE "EXIBE-PROCESSAMENTO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE DATA-MOVTO-CH13             TO
                      DATA-MOVTO-CH13A
                 MOVE SEQ-CH13                    TO SEQ-CH13A
                 MOVE ALINEA-CH13                 TO ALINEA-CH13A
                 MOVE DATA-RECTO-CH13             TO DATA-RECTO-CH13A
                 MOVE DATA-RECTO-CH13A            TO WS-DATA-NUM
                 MOVE WS-DATA-NUM                 TO WS-DATA
                 IF WS-MES > 00 AND < 13 CONTINUE
                 ELSE
                    MOVE DATA-RECTO-CH13A         TO DATA-INV
                    CALL "GRIDAT2" USING DATA-INV
                    MOVE DATA-INV                 TO DATA-RECTO-CH13A
                 END-IF

                 MOVE VLR-JUROS-CH13              TO VLR-JUROS-CH13A
                 MOVE VLR-MULTA-CH13              TO VLR-MULTA-CH13A
                 MOVE VLR-DESCONTO-CH13           TO VLR-DESCONTO-CH13A
                 MOVE 20040301                    TO DATA-ENTRADA-CH13A
                 MOVE DATA-APRES-CH13             TO DATA-APRES-CH13A
                 MOVE DATA-APRES-CH13A            TO WS-DATA-NUM
                 MOVE WS-DATA-NUM                 TO WS-DATA
                 IF WS-MES > 00 AND < 13 CONTINUE
                 ELSE
                    MOVE DATA-APRES-CH13A         TO DATA-INV
                    CALL "GRIDAT2" USING DATA-INV
                    MOVE DATA-INV                 TO DATA-APRES-CH13A
                 END-IF

                 MOVE DATA-REAPRES-CH13           TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV                    TO DATA-REAPRES-CH13A
                 IF DATA-COMPRA-CH13 NOT EQUAL ZEROS
                    MOVE DATA-COMPRA-CH13         TO DATA-INV
                    CALL "GRIDAT2" USING DATA-INV
                    MOVE DATA-INV                 TO DATA-COMPRA-CH13
                 ELSE
                    MOVE DATA-MOVTO-CH13          TO DATA-MOVTO-CH10
                    MOVE SEQ-CH13                 TO SEQ-CH10
                    READ CHD010 INVALID KEY
                                MOVE ZEROS        TO DATA-COMPRA-CH13A
                         NOT INVALID KEY
                           MOVE CLIENTE-CH10      TO ALBUM-REC
                           READ RCD100 INVALID KEY
                                MOVE ZEROS        TO DATA-COMPRA-CH13A
                              NOT INVALID KEY
                                MOVE DATAVEN-REC  TO DATA-COMPRA-CH13A
                           END-READ
                    END-READ
                 END-IF
                 MOVE FORMA-PAGTO-CH13            TO FORMA-PAGTO-CH13A

                 WRITE REG-CHD013A
                 END-WRITE
             END-READ
           END-PERFORM.


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
           MOVE "ALTCH13A" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CHD013 CHD013A RCD100 CHD010.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.
