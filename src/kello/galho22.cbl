       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO22.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 21/12/2004.
      *FUNÇÃO: GERA UM NOVO ARQUIVO RED999
      * RED999 => IGUAL O RED103 APARTIR DO DIA 19/11/2004

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           SELECT RED103 ASSIGN TO PATH-RED103
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-RED103.

           SELECT RED999 ASSIGN TO PATH-RED999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-R999 = DOCTO-R999 CODIGO-R999
                         FUNCAO-R999
                  ALTERNATE RECORD KEY IS ALT-R999 = CODIGO-R999
                            FUNCAO-R999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-R999 = DOCTO-R999
                            SEQ-R999.

           SELECT RED300 ASSIGN TO PATH-RED300
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RED300
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-R300 = DOCTO-R300 CODIGO-R300
                         FUNCAO-R300
                  ALTERNATE RECORD KEY IS ALT-R300 = CODIGO-R300
                            FUNCAO-R300 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-R300 = DOCTO-R300
                            SEQ-R300.



       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
       FD  RED103.
       01  REG-RED103.
           05  ARROBA-R103          PIC X(01).
           05  FILLER-R103          PIC X(01).
           05  REGISTRO-R103        PIC X(34).

      *ARQUIVO DE MONTAGEM NO ALBUM (NOVO)
       FD  RED999.
       01  REG-RED999.
           05  DOCTO-R999           PIC 9(6).
           05  SEQ-R999             PIC 9(3).
           05  CODIGO-R999          PIC 9(6).
           05  FUNCAO-R999          PIC 9(2).
           05  FITA-FILME-R999      PIC 9.
      *    BOOLEAN - CONTROLA 0-NENHUM  1-FILME  2-FITA   3-KM
           05  TIPO-REPORT-R999     PIC 9.
      *    BOOLENA - 1-REPORT   2-DESPESA
           05  QT-FILMES-R999       PIC 999.
      *    PODE SER QTDE DE FILMES OU FITAS OU KILOMETROS DEPENDE
      *    DO TIPO DO CAMPO FITA-FILME-R103
      *    IDENTIFICA QTDE PRODUZIDA
           05  QT-REPORT-R999       PIC 9V9.
           05  VLR-REPORT-R999      PIC 9(8)V99.

      *ARQUIVO DE MONTAGEM NO ALBUM (BACKUP)
       FD  RED300.
       01  REG-RED300.
           05  DOCTO-R300           PIC 9(6).
           05  SEQ-R300             PIC 9(3).
           05  CODIGO-R300          PIC 9(6).
           05  FUNCAO-R300          PIC 9(2).
           05  FITA-FILME-R300      PIC 9.
      *    BOOLEAN - CONTROLA 0-NENHUM  1-FILME  2-FITA   3-KM
           05  TIPO-REPORT-R300     PIC 9.
      *    BOOLENA - 1-REPORT   2-DESPESA
           05  QT-FILMES-R300       PIC 999.
      *    PODE SER QTDE DE FILMES OU FITAS OU KILOMETROS DEPENDE
      *    DO TIPO DO CAMPO FITA-FILME-R103
      *    IDENTIFICA QTDE PRODUZIDA
           05  QT-REPORT-R300       PIC 9V9.
           05  VLR-REPORT-R300      PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RED103             PIC XX       VALUE SPACES.
           05  ST-RED999             PIC XX       VALUE SPACES.
           05  ST-RED300             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "RED103" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED103.
           MOVE "RED999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED999.
           MOVE "RED300" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED300.
           OPEN I-O RED999 RED300
           OPEN INPUT RED103
           CLOSE CONTROLE.
           IF ST-RED103 = "35"
              CLOSE RED103      OPEN OUTPUT RED103
              CLOSE RED103      OPEN I-O RED103
           END-IF.
           IF ST-RED999 = "35"
              CLOSE RED999      OPEN OUTPUT RED999
              CLOSE RED999      OPEN I-O RED999
           END-IF.
           IF ST-RED300 = "35"
              CLOSE RED300      OPEN OUTPUT RED300
              CLOSE RED300      OPEN I-O RED300
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO22" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO22" STOP " ".

       START-RED103.
           INITIALIZE REG-RED103.

       READ-RED103.
             READ RED103 NEXT RECORD AT END
                  GO TO CONTINUAR1.


             IF ST-RED103 <> "00" AND "02"
                GO TO READ-RED103.

             IF ARROBA-R103 = "@"
                MOVE REGISTRO-R103 TO REG-RED999
                DISPLAY REG-RED999
                WRITE REG-RED999 INVALID KEY
                      DISPLAY "ERRO " REG-RED999 STOP " "
                END-WRITE
             END-IF

             GO TO READ-RED103.

       CONTINUAR1.
           INITIALIZE REG-RED300

           START RED300 KEY IS NOT LESS CHAVE-R300 INVALID KEY
               GO TO CONTINUAR2.

           IF ST-RED300 <> "00" AND "02"
               GO TO CONTINUAR1.

       READ-RED300.
           READ RED300 NEXT RECORD AT END
               GO TO CONTINUAR2.

           IF ST-RED300 <> "00" AND "02"
              GO TO READ-RED300.

           display reg-red300 "RED300"
           MOVE REG-RED300 TO REG-RED999
           WRITE REG-RED999
           END-WRITE

           GO TO READ-RED300.

       CONTINUAR2.

           DISPLAY "ACABOU DE EXECUTAR O GALHO22" STOP " ".

           CLOSE RED103 RED999 RED300
           EXIT PROGRAM
           STOP RUN.
