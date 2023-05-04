         IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHORCP100.
      *AUTOR: ALFREDO SAVIOLLI
      *DATA: 06/04/2010
      *Atualiza o RCP100
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY RCPX100.
           COPY RCPX101.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY RCPW100.
           COPY RCPW101.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9(01)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  TOTAL-VENDA           PIC 9(15)V99 VALUE ZEROS.
           05  TOTAL-PM              PIC 9(15)V99 VALUE ZEROS.
           05  CONT-W                PIC 9(04)         VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "CPTIME.CPY".
           COPY "PARAMETR".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "RCD100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "RCD101"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101

           MOVE ZEROS TO ERRO-W.
           OPEN I-O   RCD100
           OPEN INPUT RCD101
           CLOSE CONTROLE

           IF ST-RCD100 = "35"
              CLOSE RCD100      OPEN OUTPUT RCD100
              CLOSE RCD100      OPEN I-O    RCD100
           END-IF
           MOVE 0         TO ERRO-W

           IF ST-RCD100 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA RCD100: " ST-RCD100
                INTO MENSAGEM
              MOVE   "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           display "Vou comecar" stop " "

           IF ERRO-W = 0
              INITIALIZE REG-RCD100
              START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                    MOVE "10" TO ST-RCD100
              END-START
              PERFORM UNTIL ST-RCD100 = "10"
                    READ RCD100 NEXT AT END
                         MOVE "10" TO ST-RCD100
                    NOT AT END
                         DISPLAY "REG-RCD100 = " REG-RCD100

                         INITIALIZE REG-RCD101
                                    TOTAL-VENDA
                                    TOTAL-PM
                         MOVE ALBUM-REC        TO ALBUM-REC1
                         START RCD101 KEY IS NOT LESS CHAVE-REC1
                                                             INVALID KEY
                              MOVE "10" TO ST-RCD101
                         END-START

                         PERFORM UNTIL ST-RCD101 = "10"
                              READ RCD101 NEXT AT END
                                   MOVE "10" TO ST-RCD101
                              NOT AT END
                                   IF ALBUM-REC <> ALBUM-REC1
                                      MOVE "10" TO ST-RCD101
                                   ELSE
                                      PERFORM CALCULA-DIAS
                                      ADD VALOR-REC1  TO TOTAL-VENDA
                                      COMPUTE TOTAL-PM = TOTAL-PM +
                                             (VALOR-REC1 * CONT-W)
                                   END-IF
                              END-READ
                         END-PERFORM
                         COMPUTE PM-REC ROUNDED = TOTAL-PM / TOTAL-VENDA
                         DISPLAY REG-RCD100
                         REWRITE REG-RCD100
                    END-READ
              END-PERFORM
           ELSE
              MOVE "Não Atualizei nada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           END-IF

           MOVE "Acabei!" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           CLOSE RCD100 RCD101
           EXIT PROGRAM
           STOP RUN.

       CALCULA-DIAS SECTION.
           MOVE 2               TO GRTIME-TYPE
           MOVE 3               TO GRTIME-FUNCTION
           MOVE VENCTO-REC1     TO GRTIME-DATE-FINAL
           MOVE DATAVEN-REC     TO GRTIME-DATE
           CALL "GRTIME" USING PARAMETROS-GRTIME
           MOVE GRTIME-DAYS-FINAL TO CONT-W.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to erro-w
           move spaces to mensagem.
