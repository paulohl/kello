       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO82.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/07/1998
      *exclui conta fixa 3325 e 3326
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CRPX200.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CRPW200.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
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

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".

       01 AUX-DATA                   PIC 9(08).
       01 FILLER REDEFINES AUX-DATA.
          05 AUX-DIA                 PIC 9(02).
          05 AUX-MES                 PIC 9(02).
          05 AUX-ANO                 PIC 9(04).

       01 WS-CONFIRMA                PIC X(01) VALUE SPACES.


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           DISPLAY ERASE AT 0101
           DISPLAY "INFORMAR A EMPRESA DESEJADA: " AT 0101
           ACCEPT EMP-REC                          AT 0130


           OPEN INPUT CAD001



           MOVE EMP-REC                            TO CODIGO-CA001
           READ CAD001 INVALID KEY
                DISPLAY "EMPRESA NAO IDENTIFICADA" AT 0201
                GO TO MAIN-PROCESS
           NOT INVALID KEY
                DISPLAY NOME-EMP-CA001             AT 0135
                DISPLAY "CONFIRMA A EMPRESA ? "    AT 0301
                MOVE "S"                           TO WS-CONFIRMA
                ACCEPT  WS-CONFIRMA                AT 0322
                IF WS-CONFIRMA <> "S"
                   GO TO MAIN-PROCESS.

           DISPLAY ERASE AT 0101

           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE ZEROS    TO ERRO-W.

           DISPLAY "PATH-CRD200 = " PATH-CRD200 STOP " "

           DISPLAY ERASE AT 0101
           OPEN I-O CRD200

           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O    CRD200
           END-IF.

           DISPLAY "VOU ATUALIZAR AS DATAS ERRADAS" STOP " ".

           display erase at 0101

           INITIALIZE REG-CRD200
           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.

           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                      DISPLAY COD-COMPL-CR200   AT 0101
                      DISPLAY SEQ-CR200         AT 0111


                      IF DATA-MOVTO-CR200 < 19000000
                         DISPLAY DATA-MOVTO-CR200 AT 1001
                         MOVE DATA-MOVTO-CR200    TO AUX-DATA
                         STRING AUX-ANO AUX-MES AUX-DIA INTO
                              DATA-MOVTO-CR200

                         DISPLAY DATA-MOVTO-CR200 AT 1030

                         REWRITE REG-CRD200 INVALID KEY
                             DISPLAY "ERRO" STOP " "
                         END-REWRITE
                      END-IF
                 END-READ
           END-PERFORM

           CLOSE CRD200 CAD001
           STOP RUN.
