         IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCGD001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/07/1998
      *exclui conta fixa 3325 e 3326
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX001.
           COPY CGPX010.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CGPW001.
       COPY CGPW010.

       WORKING-STORAGE SECTION.
           COPY "GALHO10.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
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

           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010

           MOVE ZEROS TO ERRO-W.
           OPEN I-O   CGD010
           OPEN INPUT CGD001
           CLOSE CONTROLE

           IF ST-CGD001 = "35"
              CLOSE CGD001      OPEN OUTPUT CGD001
              CLOSE CGD001      OPEN I-O CGD001
           END-IF
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O    CGD010
           END-IF
           MOVE 0         TO ERRO-W

           IF ST-CGD001 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA CGD001: " ST-CGD001
                INTO MENSAGEM
              MOVE   "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD010 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA CGD010: " ST-CGD010
                INTO MENSAGEM
              MOVE   "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           display "Vou comecar" stop " "

           IF ERRO-W = 0
              INITIALIZE REG-CGD001
              START CGD001 KEY IS NOT LESS CODIGO-CG01 INVALID KEY
                    MOVE "10" TO ST-CGD001
              END-START
              PERFORM UNTIL ST-CGD001 = "10"
                    READ CGD001 NEXT AT END
                         MOVE "10" TO ST-CGD001
                    NOT AT END
                         INITIALIZE REG-CGD010

                         MOVE 9           TO CLASSIF-CG10
                         MOVE CODIGO-CG01 TO CODIGO-CG10
                         READ CGD010 INVALID KEY
                              MOVE NOME-CG01   TO COMPRADOR-CG10
                              display "reg-cgd010 = " reg-cgd010
                              WRITE REG-CGD010 INVALID KEY
                                   MOVE "Erro de Gravação...CGD010" TO
                                                  MENSAGEM
                                   MOVE "C"    TO TIPO-MSG
                                   PERFORM EXIBIR-MENSAGEM
                              END-WRITE
                         NOT INVALID KEY
                              MOVE NOME-CG01   TO COMPRADOR-CG10
                              display "reg-cgd010 = " reg-cgd010
                              REWRITE REG-CGD010 INVALID KEY
                                   MOVE "Erro de Regravação...CGD010" TO
                                                  MENSAGEM
                                   MOVE "C"    TO TIPO-MSG
                                   PERFORM EXIBIR-MENSAGEM
                              END-REWRITE
                         END-READ
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

           CLOSE CGD001 CGD010
           EXIT PROGRAM
           STOP RUN.


       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to erro-w
           move spaces to mensagem.
