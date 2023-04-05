       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO92.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 25/07/2005.
      *FUNÇÃO: ALTERA LAYOUT DO CGD001 CDC010

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CGPX006.

           SELECT CGD906 ASSIGN TO PATH-CGD906
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CG96
                  STATUS IS ST-CGD906.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CGPW006.

       FD  CGD906.
       01  REG-CGD906.
           05  CHAVE-CG96.
               10  CODIGO-CG96        PIC 9(6).
               10  BANCO-CG96         PIC 9(4).
               10  AGENCIA-CG96       PIC X(9).
               10  NR-CONTA-CG96      PIC X(15).
           05  USUARIO-CG96           PIC 9(02).
           05  DESC-USUARIO-CG96      PIC X(04).
           05  TITULAR-CONTA-CG96     PIC X(40).
           05  CPF-TITULAR-CG96       PIC 9(11).
           05  TIPO-DE-CONTA-CG96     PIC 9(01).
           05  COMPLEMENTO-CONTA-CG96 PIC X(11).
           05  PREFERENCIAL-CG96      PIC 9(01).
           05  FILLER                 PIC X(37).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD006             PIC XX       VALUE SPACES.
           05  ST-CGD906             PIC XX       VALUE SPACES.
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
           MOVE "CGD006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD006
           MOVE "CGD906" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD906
           OPEN I-O   CGD006
           CLOSE      CGD006
           OPEN I-O   CGD006
           OPEN INPUT CGD906

           CLOSE CONTROLE.
           IF ST-CGD006 = "35"
              CLOSE CGD006      OPEN OUTPUT CGD006
              CLOSE CGD006      OPEN I-O    CGD006
           END-IF.

           display "Vou comecar a atualizar o CGD006"
           stop " ".

           IF ST-CGD006 <> "00" OR ST-CGD906 <> "00"
              CLOSE CGD006
                    CGD906
              stop run.


           INITIALIZE REG-CGD906

           START CGD906 KEY IS NOT LESS CHAVE-CG96 INVALID KEY
                 MOVE "10" TO ST-CGD906.


           PERFORM UNTIL ST-CGD906 = "10"
             READ CGD906 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD906
             NOT AT END
                  MOVE CODIGO-CG96            TO CODIGO-CG06
                  MOVE BANCO-CG96             TO BANCO-CG06
                  MOVE AGENCIA-CG96           TO AGENCIA-CG06
                  MOVE NR-CONTA-CG96          TO NR-CONTA-CG06
                  MOVE USUARIO-CG96           TO USUARIO-CG06
                  MOVE DESC-USUARIO-CG96      TO DESC-USUARIO-CG06
                  MOVE TITULAR-CONTA-CG96     TO TITULAR-CONTA-CG06
                  MOVE CPF-TITULAR-CG96       TO CPF-TITULAR-CG06
                  MOVE TIPO-DE-CONTA-CG96     TO TIPO-DE-CONTA-CG06
                  MOVE COMPLEMENTO-CONTA-CG96 TO COMPLEMENTO-CONTA-CG06
                  MOVE PREFERENCIAL-CG96      TO PREFERENCIAL-CG06

                  DISPLAY REG-CGD006
                  WRITE REG-CGD006
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CGD906
           CLOSE CGD006
           EXIT PROGRAM
           STOP RUN.
