       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO32.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 25/01/2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO MTD020 C/ O CAMPO DVD

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

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

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY MTPW020.

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
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           OPEN I-O   MTD020.
           OPEN INPUT MTD999.
           CLOSE CONTROLE.
           IF ST-MTD999 = "35"
              CLOSE MTD999      OPEN OUTPUT MTD999
              CLOSE MTD999      OPEN I-O MTD999
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.

           INITIALIZE REG-MTD999

           MOVE ZEROS TO PAG-W.

           START MTD999 KEY IS NOT LESS CHAVE-MTG999 INVALID KEY
                 MOVE "10" TO ST-MTD999.


           PERFORM UNTIL ST-MTD999 = "10"
             READ MTD999 NEXT RECORD AT END MOVE "10" TO ST-MTD999
              NOT AT END
                   INITIALIZE REG-MTD020
                   MOVE CONTRATO-MTG999        TO CONTRATO-MTG
                   MOVE SEQ-MTG999             TO SEQ-MTG
                   MOVE DATAMOV-MTG999         TO DATAMOV-MTG
                   MOVE QT-ESTOJO-MTG999       TO QT-ESTOJO-MTG
                   MOVE QT-ENCADER-MTG999      TO QT-ENCADER-MTG
                   MOVE QT-FOLHAS-MTG999       TO QT-FOLHAS-MTG
                   MOVE QT-FOTOS-MTG999        TO QT-FOTOS-MTG
                   MOVE QT-FITAS-MTG999        TO QT-FITAS-MTG
                   MOVE QT-POSTER-MTG999       TO QT-POSTER-MTG
                   MOVE QT-PORTA-FITA-MTG999   TO QT-PORTA-FITA-MTG
                   MOVE FOGO-MTG999            TO FOGO-MTG
                   MOVE DATA-FOGO-MTG999       TO DATA-FOGO-MTG
                   MOVE ANOMES-VISITA-MTG999   TO ANOMES-VISITA-MTG
                   MOVE VISITA-MTG999          TO VISITA-MTG
                   MOVE POSSE-MTG999           TO POSSE-MTG
                   MOVE CODIGO-POSSE-MTG999    TO CODIGO-POSSE-MTG
                   MOVE 0                      TO QT-DVD-MTG
                   WRITE REG-MTD020
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE MTD999
           CLOSE MTD020
           EXIT PROGRAM.
