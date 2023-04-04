       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO19.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09/12/2004.
      *FUNÇÃO: LIMPA O ARQUIVO RCD101

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY RCPX101.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = ALBUM-WK
                                           NUMERO-WK.


       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY RCPW101.

       FD  WORK.
       01  REG-WORK.
           05  ALBUM-WK         PIC 9(8).
           05  NUMERO-WK        PIC 9(6).



       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  CONTADOR              PIC 9(09)    VALUE ZEROS.

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

           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "RCD999" TO ARQ-REC.  MOVE EMPRESA-REF TO VARIA-W.

           DISPLAY PATH-RCD101 STOP "PATH-RCD101".
           DISPLAY PATH-RCD101 STOP "PATH-RCD101".
           DISPLAY PATH-RCD101 STOP "PATH-RCD101".
           OPEN I-O RCD101 WORK
           CLOSE CONTROLE.
           IF ST-RCD101 = "35"
              CLOSE RCD101      OPEN OUTPUT RCD101
              CLOSE RCD101      OPEN I-O RCD101
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO19" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO19" STOP " ".

           INITIALIZE REG-RCD101

           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101.

           DISPLAY ST-RCD101 STOP "ST-RCD101 1"

           MOVE 0 TO CONTADOR

           PERFORM UNTIL ST-RCD101 = "10"
             READ RCD101 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD101
             NOT AT END
                IF DTA-BAIXA-REC1 > 0
                   ADD 1 TO CONTADOR
                   MOVE ALBUM-REC1 TO ALBUM-WK
                   MOVE NUMERO-REC1 TO NUMERO-WK
                   WRITE REG-WORK
                   END-WRITE
                END-IF
             END-READ
           END-PERFORM.

           DISPLAY "POSSUI " CONTADOR " COM DATA DE BAIXA" STOP " "

           INITIALIZE REG-WORK

           START WORK KEY IS GREATER THAN CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE 0 TO CONTADOR

           DISPLAY ST-WORK STOP "ST-WORK 1"

           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                  MOVE "10" TO ST-WORK
             NOT AT END
                 INITIALIZE REG-RCD101

                 MOVE ALBUM-WK    TO ALBUM-REC1
                 COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1

                 MOVE ALL "9"     TO NUMERO-REC1

                 START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                   MOVE "10" TO ST-RCD101
                 END-START

                 PERFORM UNTIL ST-RCD101 = "10"
                     READ RCD101 NEXT RECORD AT END
                        MOVE "10" TO ST-RCD101
                     NOT AT END
                        IF ALBUM-WK  <> ALBUM-REC1
                           MOVE "10" TO ST-RCD101
                        ELSE
                           IF DTA-BAIXA-REC1 = 0 AND NUMERO-REC1 =
                                                     NUMERO-WK
                              ADD 1 TO CONTADOR
                              DISPLAY "VOU APAGAR ... " CONTADOR
                               STOP " "
                              DELETE RCD101 INVALID KEY
                                  DISPLAY "ERRO DE EXCLUSAO ... "
                                  REG-RCD101 STOP " "
                              END-DELETE
                           END-IF
                        END-IF
                     END-READ
                 END-PERFORM
             END-READ
           END-PERFORM.

           DISPLAY "APAGUEI ... " CONTADOR " REGISTROS" STOP " "

           DISPLAY "ACABOU DE EXECUTAR O GALHO19" STOP " ".

           CLOSE RCD101 WORK
           EXIT PROGRAM
           STOP RUN.
