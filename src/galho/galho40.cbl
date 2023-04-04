       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO39.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 22-03-2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO RCD001

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY RCPX001.

           COPY RCPX101.

           COPY MTPX020.

           SELECT RCD901 ASSIGN TO PATH-RCD901
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-RCD901
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-REC9 = ALBUM-REC9 VENCTO-REC9
                         BANCO-REC9 NUMERO-REC9
                  ALTERNATE RECORD KEY IS ALBUM-REC9 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-REC9 = DTA-BAIXA-REC9
                            ALBUM-REC9 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY RCPW001.

           COPY RCPW101.

           COPY MTPW020.

       FD  RCD901.
       01  REG-RCD901.
           05  ALBUM-REC9       PIC 9(8)     COMP-3.
           05  VENCTO-REC9      PIC 9(8)     COMP-3.
           05  VALOR-REC9       PIC 9(8)V99  COMP-3.
           05  NUMERO-REC9      PIC 9(6)     COMP-3.
           05  BANCO-REC9       PIC 9(3).
           05  TIPO-REC9        PIC 9.
      *    1-CHEQUE  2-MOEDA  3-ANTECIPADO  4-DUPL/PROMIS
      *    5-DEB.AUTOM. 6-CARTAO CRED.        .
           05  DTA-BAIXA-REC9   PIC 9(8)     COMP-3.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD901             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-RCD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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
           MOVE "RCD901" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD901
           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101
           MOVE "RCD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           OPEN I-O   RCD101
           OPEN INPUT RCD901 RCD001 MTD020

           CLOSE CONTROLE.
           IF ST-RCD101 = "35"
              CLOSE RCD101      OPEN OUTPUT RCD101
              CLOSE RCD101      OPEN I-O RCD101
           END-IF.

           display "Vou comecar a atualizar o RCD101" stop " ".
           display "Vou comecar a atualizar o RCD101" stop " ".

           IF ST-RCD901 <> "00" OR ST-RCD101 <> "00" OR ST-RCD001 <>
              "00" OR ST-MTD020 <> "00"
              CLOSE RCD901
                    RCD101
                    RCD001
              stop run.

           INITIALIZE REG-RCD901

           MOVE ZEROS TO PAG-W.

           START RCD901 KEY IS NOT LESS CHAVE-REC9 INVALID KEY
                 MOVE "10" TO ST-RCD901.


           PERFORM UNTIL ST-RCD901 = "10"
             READ RCD901 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD901
             NOT AT END
                  INITIALIZE REG-RCD101
                  MOVE ALBUM-REC9        TO AUX-ALBUM
                  MOVE AUX-ALBUM         TO ALBUM-MTG
                  READ MTD020 INVALID KEY
                       INITIALIZE  REG-MTD020
                  END-READ
                  MOVE VISITA-MTG TO CODIGO-COMIS-RC01
                  READ RCD001 INVALID KEY
                       INITIALIZE REG-RCD001
                  END-READ
                  MOVE ALBUM-REC9        TO ALBUM-REC1
                  MOVE VENCTO-REC9       TO VENCTO-REC1
                  MOVE VALOR-REC9        TO VALOR-REC1
                  MOVE NUMERO-REC9       TO NUMERO-REC1
                  MOVE ZEROS             TO PARCELA-REC1
                  MOVE BANCO-REC9        TO BANCO-REC1
                  MOVE TIPO-REC9         TO TIPO-REC1
                  MOVE DTA-BAIXA-REC9    TO DTA-BAIXA-REC1
                  MOVE COMIS-CHEQUE-RC01 TO COMIS-PARC-REC1
                  MOVE ZEROS             TO CARTAO-CRED-REC1
                                            TAXA-ADMINISTRATIVA-REC1
                                            QT-PARCELA-REC1
                  display reg-rcd101
                  WRITE REG-RCD101
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE RCD001
           CLOSE RCD901
           CLOSE RCD101
           EXIT PROGRAM.
