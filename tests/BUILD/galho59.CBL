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
                  RECORD KEY IS NOME-91
                  ALTERNATE RECORD KEY IS DATA-BASE-91
                  WITH DUPLICATES
                  STATUS IS ST-CHD941.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

       FD  CHD041.
       01  REG-CHD041.
           05  NOME-41             PIC X(16).
           05  CARTEIRA-41 PIC 9(2) OCCURS 6 TIMES.
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
           05  NOME-91             PIC X(16).
           05  CARTEIRA-91 PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-91        PIC 9(8).
           05  TAXA-JUROS-91       PIC 99V99.
           05  DATA-INI-91         PIC 9(8).
           05  DATA-FIM-91         PIC 9(8).
           05  DIAS-INI-91         PIC 9(3).
           05  DIAS-FIM-91         PIC 9(3).
           05  FERIADOS-91 PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-91     PIC 9(4).
           05  VLR-BRUTO-91        PIC 9(10)V99.
           05  PM-91               PIC 9(3)V99.
           05  DIAS-30-91          PIC 9(3)V99.
           05  TAXA-30-91          PIC 9V999999.
           05  JURO-30-91          PIC 9(10)V99.
           05  SALDO-30-91         PIC 9(10)V99.
           05  DIAS-60-91          PIC 9(3)V99.
           05  TAXA-60-91          PIC 9V999999.
           05  JURO-60-91          PIC 9(10)V99.
           05  SALDO-60-91         PIC 9(10)V99.
           05  DIAS-90-91          PIC 9(3)V99.
           05  TAXA-90-91          PIC 9V999999.
           05  JURO-90-91          PIC 9(10)V99.
           05  SALDO-90-91         PIC 9(10)V99.
           05  DIAS-120-91         PIC 9(3)V99.
           05  TAXA-120-91         PIC 9V999999.
           05  JURO-120-91         PIC 9(10)V99.
           05  SALDO-120-91        PIC 9(10)V99.
           05  DIAS-150-91         PIC 9(3)V99.
           05  TAXA-150-91         PIC 9V999999.
           05  JURO-150-91         PIC 9(10)V99.
           05  SALDO-150-91        PIC 9(10)V99.
           05  DIAS-180-91         PIC 9(3)V99.
           05  TAXA-180-91         PIC 9V999999.
           05  JURO-180-91         PIC 9(10)V99.
           05  SALDO-180-91        PIC 9(10)V99.
           05  PORTADOR-DESTINO-91 PIC 9(03).




       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD041             PIC XX       VALUE SPACES.
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
           MOVE "CHD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD041
           MOVE "CHD941" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD941
           OPEN I-O   CHD041
           CLOSE      CHD041
           OPEN I-O   CHD041
           OPEN INPUT CHD941

           CLOSE CONTROLE.
           IF ST-CHD041 = "35"
              CLOSE CHD041      OPEN OUTPUT CHD041
              CLOSE CHD041      OPEN I-O CHD041
           END-IF.

           display "Vou comecar a atualizar o CHD041" stop " ".
           display "Vou comecar a atualizar o CHD041" stop " ".

           IF ST-CHD041 <> "00" OR ST-CHD941 <> "00"
              CLOSE CHD041
                    CHD941
              stop run.

           INITIALIZE REG-CHD941

           START CHD941 KEY IS NOT LESS NOME-91 INVALID KEY
                 MOVE "10" TO ST-CHD941.


           PERFORM UNTIL ST-CHD941 = "10"
             READ CHD941 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD941
             NOT AT END
                  INITIALIZE REG-CHD041
                  MOVE NOME-91             TO NOME-41
                  MOVE CARTEIRA-91(1)      TO CARTEIRA-41(1)
                  MOVE CARTEIRA-91(2)      TO CARTEIRA-41(2)
                  MOVE CARTEIRA-91(3)      TO CARTEIRA-41(3)
                  MOVE ZEROS               TO CARTEIRA-41(4)
                  MOVE ZEROS               TO CARTEIRA-41(5)
                  MOVE ZEROS               TO CARTEIRA-41(6)
                  MOVE DATA-BASE-91        TO DATA-BASE-41
                  MOVE TAXA-JUROS-91       TO TAXA-JUROS-41
                  MOVE DATA-INI-91         TO DATA-INI-41
                  MOVE DATA-FIM-91         TO DATA-FIM-41
                  MOVE DIAS-INI-91         TO DIAS-INI-41
                  MOVE DIAS-FIM-91         TO DIAS-FIM-41
                  MOVE FERIADOS-91(1)      TO FERIADOS-41(1)
                  MOVE FERIADOS-91(2)      TO FERIADOS-41(2)
                  MOVE FERIADOS-91(3)      TO FERIADOS-41(3)
                  MOVE FERIADOS-91(4)      TO FERIADOS-41(4)
                  MOVE FERIADOS-91(5)      TO FERIADOS-41(5)
                  MOVE FERIADOS-91(6)      TO FERIADOS-41(6)
                  MOVE FERIADOS-91(7)      TO FERIADOS-41(7)
                  MOVE FERIADOS-91(8)      TO FERIADOS-41(8)
                  MOVE FERIADOS-91(9)      TO FERIADOS-41(9)
                  MOVE FERIADOS-91(10)     TO FERIADOS-41(10)
                  MOVE QTDE-CHEQUES-91     TO QTDE-CHEQUES-41
                  MOVE VLR-BRUTO-91        TO VLR-BRUTO-41
                  MOVE PM-91               TO PM-41
                  MOVE DIAS-30-91          TO DIAS-30-41
                  MOVE TAXA-30-91          TO TAXA-30-41
                  MOVE JURO-30-91          TO JURO-30-41
                  MOVE SALDO-30-91         TO SALDO-30-41
                  MOVE DIAS-60-91          TO DIAS-60-41
                  MOVE TAXA-60-91          TO TAXA-60-41
                  MOVE JURO-60-91          TO JURO-60-41
                  MOVE SALDO-60-91         TO SALDO-60-41
                  MOVE DIAS-90-91          TO DIAS-90-41
                  MOVE TAXA-90-91          TO TAXA-90-41
                  MOVE JURO-90-91          TO JURO-90-41
                  MOVE SALDO-90-91         TO SALDO-90-41
                  MOVE DIAS-120-91         TO DIAS-120-41
                  MOVE TAXA-120-91         TO TAXA-120-41
                  MOVE JURO-120-91         TO JURO-120-41
                  MOVE SALDO-120-91        TO SALDO-120-41
                  MOVE DIAS-150-91         TO DIAS-150-41
                  MOVE TAXA-150-91         TO TAXA-150-41
                  MOVE JURO-150-91         TO JURO-150-41
                  MOVE SALDO-150-91        TO SALDO-150-41
                  MOVE DIAS-180-91         TO DIAS-180-41
                  MOVE TAXA-180-91         TO TAXA-180-41
                  MOVE JURO-180-91         TO JURO-180-41
                  MOVE SALDO-180-91        TO SALDO-180-41
                  MOVE PORTADOR-DESTINO-91 TO PORTADOR-DESTINO-41

                  DISPLAY REG-CHD041
                  WRITE REG-CHD041
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD941
           CLOSE CHD041
           EXIT PROGRAM.
