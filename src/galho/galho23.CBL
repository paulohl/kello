       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO23.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 06/01/2005.
      *FUNÇÃO: ADICIONA DADOS DOS CONTRATOS DE 1997 PRA TRAS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY MTPX020.

           SELECT MTD998 ASSIGN TO PATH-MTD998
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD998
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG998
                  ALTERNATE RECORD KEY IS CHAVE-MTG998 =
                            DATAMOV-MTG998, ALBUM-MTG998
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG998
                            WITH DUPLICATES.


           SELECT MTD999 ASSIGN TO PATH-MTD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD999
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG999
                  ALTERNATE RECORD KEY IS CHAVE-MTG999 =
                            DATAMOV-MTG999, ALBUM-MTG999
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG999
                            WITH DUPLICATES.



       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW020.

      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  MTD998.
       01  REG-MTD998.
           05  ALBUM-MTG998.
               10  CONTRATO-MTG998    PIC 9(4).
               10  SEQ-MTG998         PIC 9(4).
           05  DATAMOV-MTG998         PIC 9(8).
           05  QT-ESTOJO-MTG998       PIC 9.
           05  QT-ENCADER-MTG998      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG998       PIC 9999.
           05  QT-FOTOS-MTG998        PIC 9999.
           05  QT-FITAS-MTG998        PIC 9.
           05  QT-POSTER-MTG998       PIC 9.
           05  QT-PORTA-FITA-MTG998   PIC 9.
           05  FOGO-MTG998            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG998       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG998   PIC 9(6).
           05  VISITA-MTG998          PIC 9.
           05  POSSE-MTG998           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem
           05  CODIGO-POSSE-MTG998    PIC 9(6).


      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  MTD999.
       01  REG-MTD999.
           05  ALBUM-MTG999.
               10  CONTRATO-MTG999    PIC 9(4).
               10  SEQ-MTG999         PIC 9(4).
           05  DATAMOV-MTG999         PIC 9(8).
           05  QT-ESTOJO-MTG999       PIC 9.
           05  QT-ENCADER-MTG999      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG999       PIC 9999.
           05  QT-FOTOS-MTG999        PIC 9999.
           05  QT-FITAS-MTG999        PIC 9.
           05  QT-POSTER-MTG999       PIC 9.
           05  QT-PORTA-FITA-MTG999   PIC 9.
           05  FOGO-MTG999            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG999       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG999   PIC 9(6).
           05  VISITA-MTG999          PIC 9.
           05  POSSE-MTG999           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem
           05  CODIGO-POSSE-MTG999    PIC 9(6).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD998             PIC XX       VALUE SPACES.
           05  ST-MTD999             PIC XX       VALUE SPACES.
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

           05  CONTADOR-TOTAL        PIC 9(09).
           05  CONTADOR-ERRO         PIC 9(09).

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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD998" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD998.
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999.

           OPEN I-O MTD998

           OPEN INPUT MTD999 MTD020

           CLOSE CONTROLE.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD998 = "35"
              CLOSE MTD998      OPEN OUTPUT MTD998
              CLOSE MTD998      OPEN I-O MTD998
           END-IF.

           IF ST-MTD999 = "35"
              CLOSE MTD999      OPEN OUTPUT MTD999
              CLOSE MTD999      OPEN I-O MTD999
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO23" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO23" STOP " ".

       START-MTD020.
           INITIALIZE REG-MTD020
                      CONTADOR-TOTAL
                      CONTADOR-ERRO.

           START MTD020 KEY IS NOT LESS CHAVE-MTG INVALID KEY
               GO TO CONTINUAR1.

           IF ST-MTD020 <> "00" AND "02"
              GO TO START-MTD020.

       READ-MTD020.
             READ MTD020 NEXT RECORD AT END
                  GO TO CONTINUAR1.


             IF ST-MTD020 <> "00" AND "02"
                GO TO READ-MTD020.

             IF DATAMOV-MTG < 19970701
                GO TO READ-MTD020.

             ADD 1 TO CONTADOR-TOTAL

             MOVE REG-MTD020    TO REG-MTD998

             DISPLAY REG-MTD998

             WRITE REG-MTD998 INVALID KEY
                   ADD 1 TO  CONTADOR-ERRO.

             GO TO READ-MTD020.

       CONTINUAR1.
           INITIALIZE REG-MTD999

           START MTD999 KEY IS NOT LESS CHAVE-MTG999 INVALID KEY
               GO TO CONTINUAR2.

           IF ST-MTD999 <> "00" AND "02"
              GO TO CONTINUAR1.

       READ-MTD999.
             READ MTD999 NEXT RECORD AT END
                  GO TO CONTINUAR2.


             IF ST-MTD999 <> "00" AND "02"
                GO TO READ-MTD999.

             IF DATAMOV-MTG999 > 19970630
                GO TO CONTINUAR2.

             ADD 1 TO CONTADOR-TOTAL

             MOVE REG-MTD999    TO REG-MTD998

             IF ANOMES-VISITA-MTG999 = 0
                MOVE DATAMOV-MTG999(1:6) TO ANOMES-VISITA-MTG998
             END-IF

             DISPLAY REG-MTD998

             WRITE REG-MTD998 INVALID KEY
                ADD 1 TO  CONTADOR-ERRO
                REWRITE REG-MTD998.

             GO TO READ-MTD999.

       CONTINUAR2.

           DISPLAY "ACABOU DE EXECUTAR O GALHO22" STOP " ".

           DISPLAY "CONTADOR-TOTAL = " CONTADOR-TOTAL STOP " ".
           DISPLAY "CONTADOR-ERRO = " CONTADOR-ERRO STOP " ".

           CLOSE MTD020 MTD998 MTD999
           EXIT PROGRAM
           STOP RUN.
