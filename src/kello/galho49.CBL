       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO48.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09/06/2005.
      *FUNÇÃO: ALTERA LAYOUT DO COP041

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
                  RECORD KEY IS ALBUM-MTG99
                  ALTERNATE RECORD KEY IS CHAVE-MTG99 =
                            DATAMOV-MTG99, ALBUM-MTG99
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG99
                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW020.

       FD  MTD999.
       01  REG-MTD999.
           05  ALBUM-MTG99.
               10  CONTRATO-MTG99    PIC 9(4).
               10  SEQ-MTG99         PIC 9(4).
           05  DATAMOV-MTG99         PIC 9(8).
           05  QT-ESTOJO-MTG99       PIC 9.
           05  QT-ENCADER-MTG99      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG99       PIC 9999.
           05  QT-FOTOS-MTG99        PIC 9999.
           05  QT-FITAS-MTG99        PIC 9.
           05  QT-POSTER-MTG99       PIC 9.
           05  QT-PORTA-FITA-MTG99   PIC 9.
           05  QT-FOTO-CD-MTG99      PIC 99.
           05  QT-MOLDURA-MTG99      PIC 99.
           05  QT-PORTA-DVD-MTG99    PIC 99.
           05  FOGO-MTG99            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG99       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG99   PIC 9(6).
           05  VISITA-MTG99          PIC 999.
           05  POSSE-MTG99           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem
           05  CODIGO-POSSE-MTG99    PIC 9(6).
           05  QT-DVD-MTG99          PIC 9(1).
           05  NAO-GEROU-ALBUM-MTG99 PIC 9(1).
           05  FILLER                PIC X(40).


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
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.

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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999
           OPEN I-O   MTD020
           CLOSE      MTD020
           OPEN I-O   MTD020
           OPEN INPUT MTD999

           CLOSE CONTROLE.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.

           display "Vou comecar a atualizar o MTD020" stop " ".
           display "Vou comecar a atualizar o MTD020" stop " ".

           IF ST-MTD020 <> "00" OR ST-MTD999 <> "00"
              CLOSE MTD020
                    MTD999
              stop run.

           INITIALIZE REG-MTD999

           START MTD999 KEY IS NOT LESS ALBUM-MTG99 INVALID KEY
                 MOVE "10" TO ST-MTD999.


           PERFORM UNTIL ST-MTD999 = "10"
             READ MTD999 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD999
             NOT AT END
                  MOVE ALBUM-MTG99            TO ALBUM-MTG
                  MOVE DATAMOV-MTG99          TO DATAMOV-MTG
                  MOVE QT-ESTOJO-MTG99        TO QT-ESTOJO-MTG
                  MOVE QT-ENCADER-MTG99       TO QT-ENCADER-MTG
                  MOVE QT-FOLHAS-MTG99        TO QT-FOLHAS-MTG
                  MOVE QT-FOTOS-MTG99         TO QT-FOTOS-MTG
                  MOVE QT-FITAS-MTG99         TO QT-FITAS-MTG
                  MOVE QT-POSTER-MTG99        TO QT-POSTER-MTG
                  MOVE QT-PORTA-FITA-MTG99    TO QT-PORTA-FITA-MTG
                  MOVE QT-FOTO-CD-MTG99       TO QT-FOTO-CD-MTG
                  MOVE QT-MOLDURA-MTG99       TO QT-MOLDURA-MTG
                  MOVE QT-PORTA-DVD-MTG99     TO QT-PORTA-DVD-MTG
                  MOVE FOGO-MTG99             TO FOGO-MTG
                  MOVE DATA-FOGO-MTG99        TO DATA-FOGO-MTG
                  MOVE ANOMES-VISITA-MTG99    TO ANOMES-VISITA-MTG
                  MOVE VISITA-MTG99           TO VISITA-MTG
                  MOVE POSSE-MTG99            TO POSSE-MTG
                  MOVE CODIGO-POSSE-MTG99     TO CODIGO-POSSE-MTG
                  MOVE QT-DVD-MTG99           TO QT-DVD-MTG
                  MOVE NAO-GEROU-ALBUM-MTG99  TO NAO-GEROU-ALBUM-MTG
                  MOVE ZEROS                  TO DATAROMANEIO-MTG
                  DISPLAY REG-MTD020
                  WRITE REG-MTD020
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE MTD999
           CLOSE MTD020
           EXIT PROGRAM.
