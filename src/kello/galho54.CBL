       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO54.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09-08-2005.
      *FUNÇÃO: GERA UM NOVO LAYOUT DO CHD040 E CHD041

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           SELECT CHD040 ASSIGN TO PATH-CHD040
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-40 = NOME-40 SEQ-40
                  ALTERNATE RECORD KEY IS ALT-40 =
                           NOME-40 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-40 = VENC-40
                           BANCO-40 NR-CHEQUE-40 WITH DUPLICATES
                  STATUS IS ST-CHD040.

           SELECT CHD940 ASSIGN TO PATH-CHD940
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-940 = NOME-940 SEQ-940
                  ALTERNATE RECORD KEY IS ALT-940 =
                           NOME-940 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-940 = VENC-940
                           BANCO-940 NR-CHEQUE-940 WITH DUPLICATES
                  STATUS IS ST-CHD940.

           SELECT CHD041 ASSIGN TO PATH-CHD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS NOME-41
                  ALTERNATE RECORD KEY IS DATA-BASE-41
                  WITH DUPLICATES
                  STATUS IS ST-CHD041.

           SELECT CHD941 ASSIGN TO PATH-CHD941
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS NOME-941
                  ALTERNATE RECORD KEY IS DATA-BASE-941
                  WITH DUPLICATES
                  STATUS IS ST-CHD941.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.

       FD  CHD040.
       01  REG-CHD040.
           05  VENC-40            PIC 9(8).
           05  BANCO-40           PIC 9(4).
           05  NR-CHEQUE-40       PIC X(10).
           05  NOME-40            PIC X(16).
           05  SEQ-40             PIC 9(3).
           05  VALOR-40           PIC 9(8)V99.
           05  VENC1-40           PIC 9(8).
           05  DIAS-40            PIC 9(3).
           05  DATA-MOVTO-40      PIC 9(8).
           05  SEQ-CHEQUE-40      PIC 9(4).

       FD  CHD940.
       01  REG-CHD940.
           05  VENC-940            PIC 9(8).
           05  BANCO-940           PIC 9(4).
           05  NR-CHEQUE-940       PIC X(10).
           05  NOME-940            PIC X(06).
           05  SEQ-940             PIC 9(3).
           05  VALOR-940           PIC 9(8)V99.
           05  VENC1-940           PIC 9(8).
           05  DIAS-940            PIC 9(3).
           05  DATA-MOVTO-940      PIC 9(8).
           05  SEQ-CHEQUE-940      PIC 9(4).

       FD  CHD041.
       01  REG-CHD041.
           05  NOME-41             PIC X(16).
           05  CARTEIRA-41 PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-41        PIC 9(8).
           05  TAXA-JUROS-41       PIC 99V99.
           05  DATA-INI-41         PIC 9(8).
           05  DATA-FIM-41         PIC 9(8).
           05  DIAS-INI-41         PIC 9(3).
           05  DIAS-FIM-41         PIC 9(3).
           05  FERIADOS-41 PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-41     PIC 9(4).
           05  VLR-BRUTO-41        PIC 9(10)V99.
           05  PM-41               PIC 9(3)V99.
           05  DIAS-30-41          PIC 9(3)V99.
           05  TAXA-30-41          PIC 9V999999.
           05  JURO-30-41          PIC 9(10)V99.
           05  SALDO-30-41         PIC 9(10)V99.
           05  DIAS-60-41          PIC 9(3)V99.
           05  TAXA-60-41          PIC 9V999999.
           05  JURO-60-41          PIC 9(10)V99.
           05  SALDO-60-41         PIC 9(10)V99.
           05  DIAS-90-41          PIC 9(3)V99.
           05  TAXA-90-41          PIC 9V999999.
           05  JURO-90-41          PIC 9(10)V99.
           05  SALDO-90-41         PIC 9(10)V99.
           05  DIAS-120-41         PIC 9(3)V99.
           05  TAXA-120-41         PIC 9V999999.
           05  JURO-120-41         PIC 9(10)V99.
           05  SALDO-120-41        PIC 9(10)V99.
           05  DIAS-150-41         PIC 9(3)V99.
           05  TAXA-150-41         PIC 9V999999.
           05  JURO-150-41         PIC 9(10)V99.
           05  SALDO-150-41        PIC 9(10)V99.
           05  DIAS-180-41         PIC 9(3)V99.
           05  TAXA-180-41         PIC 9V999999.
           05  JURO-180-41         PIC 9(10)V99.
           05  SALDO-180-41        PIC 9(10)V99.
           05  PORTADOR-DESTINO-41 PIC 9(03).

       FD  CHD941.
       01  REG-CHD941.
           05  NOME-941            PIC X(06).
           05  CARTEIRA-941 PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-941       PIC 9(8).
           05  TAXA-JUROS-941      PIC 99V99.
           05  DATA-INI-941        PIC 9(8).
           05  DATA-FIM-941        PIC 9(8).
           05  DIAS-INI-941        PIC 9(3).
           05  DIAS-FIM-941        PIC 9(3).
           05  FERIADOS-941 PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-941    PIC 9(4).
           05  VLR-BRUTO-941       PIC 9(10)V99.
           05  PM-941              PIC 9(3)V99.
           05  DIAS-30-941         PIC 9(3)V99.
           05  TAXA-30-941         PIC 9V999999.
           05  JURO-30-941         PIC 9(10)V99.
           05  SALDO-30-941        PIC 9(10)V99.
           05  DIAS-60-941         PIC 9(3)V99.
           05  TAXA-60-941         PIC 9V999999.
           05  JURO-60-941         PIC 9(10)V99.
           05  SALDO-60-941        PIC 9(10)V99.
           05  DIAS-90-941         PIC 9(3)V99.
           05  TAXA-90-941         PIC 9V999999.
           05  JURO-90-941         PIC 9(10)V99.
           05  SALDO-90-941        PIC 9(10)V99.
           05  DIAS-120-941        PIC 9(3)V99.
           05  TAXA-120-941        PIC 9V999999.
           05  JURO-120-941        PIC 9(10)V99.
           05  SALDO-120-941       PIC 9(10)V99.
           05  DIAS-150-941        PIC 9(3)V99.
           05  TAXA-150-941        PIC 9V999999.
           05  JURO-150-941        PIC 9(10)V99.
           05  SALDO-150-941       PIC 9(10)V99.
           05  DIAS-180-941        PIC 9(3)V99.
           05  TAXA-180-941        PIC 9V999999.
           05  JURO-180-941        PIC 9(10)V99.
           05  SALDO-180-941       PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD040             PIC XX       VALUE SPACES.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CHD940             PIC XX       VALUE SPACES.
           05  ST-CHD941             PIC XX       VALUE SPACES.
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


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CHD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD040
           MOVE "CHD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD041
           MOVE "CHD940" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD940
           MOVE "CHD941" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD941
           OPEN I-O   CHD040 CHD041
           CLOSE      CHD040 CHD041
           OPEN I-O   CHD040 CHD041
           OPEN INPUT CHD940 CHD941
           CLOSE CONTROLE.
           IF ST-CHD040 = "35"
              CLOSE CHD040      OPEN OUTPUT CHD040
              CLOSE CHD040      OPEN I-O    CHD040
           END-IF.
           IF ST-CHD041 = "35"
              CLOSE CHD041      OPEN OUTPUT CHD041
              CLOSE CHD041      OPEN I-O    CHD041
           END-IF.

           DISPLAY "VOU COMECAR" STOP " "

           INITIALIZE REG-CHD940

           MOVE "00" TO ST-CHD940

           PERFORM UNTIL ST-CHD940 = "10"
               READ CHD940 NEXT AT END
                   MOVE "10" TO ST-CHD940
               NOT AT END
                   MOVE VENC-940        TO VENC-40
                   MOVE BANCO-940       TO BANCO-40
                   MOVE NR-CHEQUE-940   TO NR-CHEQUE-40
                   MOVE NOME-940        TO NOME-40
                   MOVE SEQ-940         TO SEQ-40
                   MOVE VALOR-940       TO VALOR-40
                   MOVE VENC1-940       TO VENC1-40
                   MOVE DIAS-940        TO DIAS-40
                   MOVE DATA-MOVTO-940  TO DATA-MOVTO-40
                   MOVE SEQ-CHEQUE-940  TO SEQ-CHEQUE-40
                   DISPLAY REG-CHD040
                   WRITE REG-CHD040 INVALID KEY
                        DISPLAY "ERRO DE GRAVACAO" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CHD941

           MOVE "00" TO ST-CHD941

           PERFORM UNTIL ST-CHD941 = "10"
               READ CHD941 NEXT AT END
                   MOVE "10" TO ST-CHD941
               NOT AT END
                   MOVE NOME-941            TO NOME-41
                   MOVE CARTEIRA-941(1)     TO CARTEIRA-41(1)
                   MOVE CARTEIRA-941(2)     TO CARTEIRA-41(2)
                   MOVE CARTEIRA-941(3)     TO CARTEIRA-41(3)
                   MOVE DATA-BASE-941       TO DATA-BASE-41
                   MOVE TAXA-JUROS-941      TO TAXA-JUROS-41
                   MOVE DATA-INI-941        TO DATA-INI-41
                   MOVE DATA-FIM-941        TO DATA-FIM-41
                   MOVE DIAS-INI-941        TO DIAS-INI-41
                   MOVE DIAS-FIM-941        TO DIAS-FIM-41
                   MOVE FERIADOS-941(1)     TO FERIADOS-41(1)
                   MOVE FERIADOS-941(2)     TO FERIADOS-41(2)
                   MOVE FERIADOS-941(3)     TO FERIADOS-41(3)
                   MOVE FERIADOS-941(4)     TO FERIADOS-41(4)
                   MOVE FERIADOS-941(5)     TO FERIADOS-41(5)
                   MOVE FERIADOS-941(6)     TO FERIADOS-41(6)
                   MOVE FERIADOS-941(7)     TO FERIADOS-41(7)
                   MOVE FERIADOS-941(8)     TO FERIADOS-41(8)
                   MOVE FERIADOS-941(9)     TO FERIADOS-41(9)
                   MOVE FERIADOS-941(10)    TO FERIADOS-41(10)
                   MOVE QTDE-CHEQUES-941    TO QTDE-CHEQUES-41
                   MOVE VLR-BRUTO-941       TO VLR-BRUTO-41
                   MOVE PM-941              TO PM-41
                   MOVE DIAS-30-941         TO DIAS-30-41
                   MOVE TAXA-30-941         TO TAXA-30-41
                   MOVE JURO-30-941         TO JURO-30-41
                   MOVE SALDO-30-941        TO SALDO-30-41
                   MOVE DIAS-60-941         TO DIAS-60-41
                   MOVE TAXA-60-941         TO TAXA-60-41
                   MOVE JURO-60-941         TO JURO-60-41
                   MOVE SALDO-60-941        TO SALDO-60-41
                   MOVE DIAS-90-941         TO DIAS-90-41
                   MOVE TAXA-90-941         TO TAXA-90-41
                   MOVE JURO-90-941         TO JURO-90-41
                   MOVE SALDO-90-941        TO SALDO-90-41
                   MOVE DIAS-120-941        TO DIAS-120-41
                   MOVE TAXA-120-941        TO TAXA-120-41
                   MOVE JURO-120-941        TO JURO-120-41
                   MOVE SALDO-120-941       TO SALDO-120-41
                   MOVE DIAS-150-941        TO DIAS-150-41
                   MOVE TAXA-150-941        TO TAXA-150-41
                   MOVE JURO-150-941        TO JURO-150-41
                   MOVE SALDO-150-941       TO SALDO-150-41
                   MOVE DIAS-180-941        TO DIAS-180-41
                   MOVE TAXA-180-941        TO TAXA-180-41
                   MOVE JURO-180-941        TO JURO-180-41
                   MOVE SALDO-180-941       TO SALDO-180-41
                   MOVE ZEROS               TO PORTADOR-DESTINO-41
                   DISPLAY REG-CHD041
                   WRITE REG-CHD041 INVALID KEY
                        DISPLAY "ERRO DE GRAVACAO" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM


           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD040 CHD041 CHD940 CHD941
           EXIT PROGRAM
           STOP RUN.
