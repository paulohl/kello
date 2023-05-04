       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO17.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/12/2004.
      *FUNÇÃO: GERA UM NOVO ARQUIVO MTD999
      * MTD999 => IGUAL O MTD020 APARTIR DO DIA 8/11/2004

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY MTPX020.

           SELECT MTD999 ASSIGN TO PATH-MTD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG999
                  ALTERNATE RECORD KEY IS CHAVE-MTG999 =
                            DATAMOV-MTG999, ALBUM-MTG999
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG999
                            WITH DUPLICATES.

           SELECT MTD300 ASSIGN TO PATH-MTD300
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD300
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG300
                  ALTERNATE RECORD KEY IS CHAVE-MTG300 =
                            DATAMOV-MTG300, ALBUM-MTG300
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG300
                            WITH DUPLICATES.



       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY MTPW020.

      *ARQUIVO DE MONTAGEM NO ALBUM (NOVO)
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
      *    1-EM ESTOQUE    2-COM VENDEDOR
           05  CODIGO-POSSE-MTG999    PIC 9(6).

      *ARQUIVO DE MONTAGEM NO ALBUM (BACKUP)
       FD  MTD300.
       01  REG-MTD300.
           05  ALBUM-MTG300.
               10  CONTRATO-MTG300    PIC 9(4).
               10  SEQ-MTG300         PIC 9(4).
           05  DATAMOV-MTG300         PIC 9(8).
           05  QT-ESTOJO-MTG300       PIC 9.
           05  QT-ENCADER-MTG300      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG300       PIC 9999.
           05  QT-FOTOS-MTG300        PIC 9999.
           05  QT-FITAS-MTG300        PIC 9.
           05  QT-POSTER-MTG300       PIC 9.
           05  QT-PORTA-FITA-MTG300   PIC 9.
           05  FOGO-MTG300            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG300       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG300   PIC 9(6).
           05  VISITA-MTG300          PIC 9.
           05  POSSE-MTG300           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR
           05  CODIGO-POSSE-MTG300    PIC 9(6).




       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD999             PIC XX       VALUE SPACES.
           05  ST-MTD300             PIC XX       VALUE SPACES.
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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999.
           MOVE "MTD300" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD300.
           OPEN I-O MTD999 MTD300
           OPEN INPUT MTD020
           CLOSE CONTROLE.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD999 = "35"
              CLOSE MTD999      OPEN OUTPUT MTD999
              CLOSE MTD999      OPEN I-O MTD999
           END-IF.
           IF ST-MTD300 = "35"
              CLOSE MTD300      OPEN OUTPUT MTD300
              CLOSE MTD300      OPEN I-O MTD300
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO17" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO17" STOP " ".

           INITIALIZE REG-MTD020

           MOVE 20041204       TO  DATAMOV-MTG
           MOVE ZEROS          TO  ALBUM-MTG

           START MTD020 KEY IS NOT LESS CHAVE-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
              DISPLAY ST-MTD020 STOP "ST-MTD020 1"

           PERFORM UNTIL ST-MTD020 = "10"
              DISPLAY ST-MTD020 STOP "ST-MTD020 2"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
                      display reg-mtd020 "MTD020"
                      MOVE REG-MTD020 TO REG-MTD999
                      WRITE REG-MTD999
                      END-WRITE
              END-READ
           END-PERFORM.

           INITIALIZE REG-MTD300

           MOVE ZEROS          TO  DATAMOV-MTG300
           MOVE ZEROS          TO  ALBUM-MTG300

           START MTD300 KEY IS NOT LESS CHAVE-MTG300 INVALID KEY
                 MOVE "10" TO ST-MTD300.

           PERFORM UNTIL ST-MTD300 = "10"
             READ MTD300 NEXT RECORD AT END MOVE "10" TO ST-MTD300
              NOT AT END
                      display reg-mtd300 "MTD300"
                      MOVE REG-MTD300 TO REG-MTD999
                      WRITE REG-MTD999
                      END-WRITE
              END-READ
           END-PERFORM.





           DISPLAY "ACABOU DE EXECUTAR O GALHO17" STOP " ".

           CLOSE MTD020 MTD999 MTD300
           EXIT PROGRAM
           STOP RUN.
