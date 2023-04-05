       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTD019.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/10/2004.
      * FUNCAO: GERA ARQUIVO AUXILIAR DO MTD019 / COM CHAVE PELO NOME /
      * FOGO


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX022.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW022.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
           05  DEVOLVE-HISTORICO   PIC X(30)  VALUE SPACES.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD022             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  VARIA-W               PIC 9(8)   VALUE ZEROS.
           05  CONTRATO-TRANSF       PIC 9(4)   VALUE ZEROS.
           05  CONTRATO-W            PIC X(4)   VALUE SPACES.
           05  ALBUM-W               PIC X(4)   VALUE SPACES.

       01 MENSAGEM                   PIC X(200).
       01 TIPO-MSG                   PIC X(01).
       01 RESP-MSG                   PIC X(01).

       LINKAGE SECTION.
       01  STRING-1               PIC X(80) VALUE SPACES.
       PROCEDURE DIVISION USING STRING-1.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE.
           ACCEPT VARIA-W FROM TIME.
           READ CONTROLE
           MOVE EMPRESA   TO EMP-REC
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD022"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD022.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           CLOSE CONTROLE.
           OPEN OUTPUT MTD022
           CLOSE       MTD022
           OPEN I-O    MTD022
           OPEN INPUT  MTD019 MTD020

           IF ST-MTD019 <> "00"
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              EXIT PROGRAM
              STOP RUN.

           IF ST-MTD020 <> "00"
              STRING "ERRO ABERTURA MTD020: " ST-MTD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              EXIT PROGRAM
              STOP RUN.

           IF ST-MTD022 <> "00"
              STRING "ERRO ABERTURA MTD022: " ST-MTD022 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              EXIT PROGRAM
              STOP RUN.

           INITIALIZE REG-MTD020

           START MTD020 KEY IS NOT LESS ALBUM-MTG INVALID KEY
               MOVE "10" TO ST-MTD020.

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
               NOT AT END
                   IF FOGO-MTG = 0 OR 9
                      MOVE ALBUM-MTG      TO ALBUMMT19
                      READ MTD019 NOT INVALID KEY
                           MOVE CONTRATO-MT19  TO CONTRATO-MT22
                           MOVE SEQ-MT19       TO SEQ-MT22
                           MOVE NOME-FORM-MT19 TO NOME-FORM-MT22
                           WRITE REG-MTD022
                           END-WRITE
                      END-READ
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE MTD019 MTD020 MTD022.
           EXIT PROGRAM.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".


