         IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD020.
      *AUTOR: ALFREDO SAVIOLLI
      *DATA: 22/10/2009
      *Atualiza o CRD020 (Arquivo do Contas a Receber)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CRPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CRPW020.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
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
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020

           MOVE ZEROS TO ERRO-W.
           OPEN I-O   CRD020
           CLOSE CONTROLE

           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O    CRD020
           END-IF
           MOVE 0         TO ERRO-W

           IF ST-CRD020 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA CRD020: " ST-CRD020
                INTO MENSAGEM
              MOVE   "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           display "Vou comecar" stop " "

           IF ERRO-W = 0
              INITIALIZE REG-CRD020
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                    MOVE "10" TO ST-CRD020
              END-START
              PERFORM UNTIL ST-CRD020 = "10"
                    READ CRD020 NEXT AT END
                         MOVE "10" TO ST-CRD020
                    NOT AT END
                         DISPLAY "REG-CRD020 = " REG-CRD020

      *                  IF FORMA-PAGTO-CR20 = "4-Receb.Bco."
                            IF SITUACAO-CR20 = 2
      *                        IF VALOR-SALDO-CR20 > 0
                                  MOVE ZEROS TO VALOR-SALDO-CR20
                                  REWRITE REG-CRD020 INVALID KEY
                                      MOVE "Erro de Regravação..CRD020"
                                        TO MENSAGEM
                                      MOVE "C" TO TIPO-MSG
                                      PERFORM EXIBIR-MENSAGEM
                                  END-REWRITE
      *                        END-IF
                            END-IF
      *                  END-IF
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

           CLOSE CRD020
           EXIT PROGRAM
           STOP RUN.


       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to erro-w
           move spaces to mensagem.
