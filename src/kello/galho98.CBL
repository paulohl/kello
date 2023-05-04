       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO98.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 08-09-2009.
      *FUNÇÃO: MOVE OS DADOS DO PORTA FITA P/ BOOK

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY MTPX020.

           COPY RCPX100.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW020.

           COPY RCPW100.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC x.
           05  RESP-MSG              PIC x.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(8).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(13).
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
           05  AUX-CONT              PIC 9(04)    VALUE ZEROS.
           05  AUX-CONT2             PIC 9(04)    VALUE ZEROS.

           COPY "PARAMETR".

       01  LINDET.
           05  CXP100-LINDET         PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ       CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "RCD100"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-RCD100
           MOVE "MTD020"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-MTD020

           OPEN I-O   RCD100 MTD020

           DISPLAY "VOU COMECAR " STOP " "

           INITIALIZE REG-RCD100
           START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
                 READ RCD100 NEXT AT END
                      MOVE "10" TO ST-RCD100
                 NOT AT END
                      DISPLAY "REG-RCD100 = " REG-RCD100
                      IF QPFITA-REC > 0
                         MOVE QPFITA-REC TO QBOOK-REC
                         MOVE 0          TO QPFITA-REC
                         REWRITE REG-RCD100
                         END-REWRITE
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-MTD020
           START MTD020 KEY IS NOT LESS ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.

           PERFORM UNTIL ST-MTD020 = "10"
                 READ MTD020 NEXT AT END
                      MOVE "10" TO ST-MTD020
                 NOT AT END
                      DISPLAY "REG-MTD020 = " REG-MTD020
                      IF QT-PORTA-FITA-MTG > 0
                         MOVE QT-PORTA-FITA-MTG TO QT-BOOK-MTG
                         MOVE 0                 TO QT-PORTA-FITA-MTG
                         REWRITE REG-MTD020
                         END-REWRITE
                      END-IF
                 END-READ
           END-PERFORM


           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE RCD100 MTD020
           EXIT PROGRAM
           STOP RUN.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

