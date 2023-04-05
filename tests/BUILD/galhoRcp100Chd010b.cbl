       IDENTIFICATION DIVISION.
       PROGRAM-ID. galhoRcp100Chd010b.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 15-02-2011.
      *FUNÇÃO: GERA O CHD010B Apartir do RCD100.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY RCPX100.
           COPY RCPX101.
           COPY CHPX010B.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY RCPW100.
           COPY RCPW101.
           COPY CHPW010B.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           COPY "PARAMETR".



       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           display erase at 0101.

           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.


           initialize reg-cad001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
                      DISPLAY "CODIGO-CA001 = " CODIGO-CA001 STOP " "

                      perform abrir-arquivos
                      perform converter-arquivo
                      perform fechar-arquivos

                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RCD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RCD100

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "RCD101"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-RCD101

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD010B"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD010B

           OPEN I-O    RCD100 RCD101 CHD010B
           CLOSE       RCD100 RCD101 CHD010B
           OPEN INPUT  RCD100 RCD101
           OPEN I-O    CHD010B.

       converter-arquivo section.

           INITIALIZE REG-RCD100
           START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
                READ RCD100 NEXT AT END
                     MOVE "10" TO ST-RCD100
                NOT AT END
                     DISPLAY "REG-RCD100 = " REG-RCD100

                     INITIALIZE REG-RCD101
                     MOVE ALBUM-REC            TO ALBUM-REC1
                     START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                           MOVE "10" TO ST-RCD101
                     END-START
                     PERFORM UNTIL ST-RCD101 = "10"
                           READ RCD101 NEXT AT END
                                MOVE "10" TO ST-RCD101
                           NOT AT END
                                IF ALBUM-REC <> ALBUM-REC1
                                   MOVE "10" TO ST-RCD101
                                ELSE
                                   IF TIPO-REC1 = 1
                                      PERFORM VERIFICAR-CHD010B
                                   END-IF
                                END-IF
                           END-READ
                     END-PERFORM
                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close rcd100 rcd101 chd010b.

       verificar-chd010b section.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.

