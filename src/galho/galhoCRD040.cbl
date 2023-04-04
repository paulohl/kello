       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD040.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 30-04-2010.
      *FUNÇÃO: AUMENTAR O CAMPO NOME-CR40

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           SELECT CRD040  ASSIGN TO PATH-CRD040
                          ORGANIZATION IS INDEXED
                          ACCESS MODE DYNAMIC
                          RECORD KEY IS CHAVE-40 = NOME-40 SEQ-40
                          ALTERNATE RECORD KEY IS ALT-40 =
                                   NOME-40 WITH DUPLICATES
                          ALTERNATE RECORD KEY IS ALT1-40 = VENC-40
                                   BANCO-40 NR-DUPLICATA-40
                                           WITH DUPLICATES
                          STATUS IS ST-CRD040.

           SELECT CRD040A ASSIGN TO PATH-CRD040A
                          ORGANIZATION IS INDEXED
                          ACCESS MODE DYNAMIC
                          RECORD KEY IS CHAVE-40A = NOME-40A SEQ-40A
                          ALTERNATE RECORD KEY IS ALT-40A =
                                   NOME-40A WITH DUPLICATES
                          ALTERNATE RECORD KEY IS ALT1-40A = VENC-40A
                                   BANCO-40A NR-DUPLICATA-40A
                          WITH DUPLICATES
                          STATUS IS ST-CRD040A.

           SELECT CRD041  ASSIGN TO PATH-CRD041
                          ORGANIZATION IS INDEXED
                          ACCESS MODE DYNAMIC
                          RECORD KEY IS NOME-41
                          ALTERNATE RECORD KEY IS DATA-BASE-41
                          WITH DUPLICATES
                          STATUS IS ST-CRD041.

           SELECT CRD041A ASSIGN TO PATH-CRD041A
                          ORGANIZATION IS INDEXED
                          ACCESS MODE DYNAMIC
                          RECORD KEY IS NOME-41A
                          ALTERNATE RECORD KEY IS DATA-BASE-41A
                          WITH DUPLICATES
                          STATUS IS ST-CRD041A.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.

       FD  CRD040.
       01  REG-CRD040.
           05  VENC-40            PIC 9(8).
           05  BANCO-40           PIC 9(4).
           05  NR-DUPLICATA-40    PIC X(10).
           05  NOME-40            PIC X(16).
           05  SEQ-40             PIC 9(3).
           05  VALOR-40           PIC 9(8)V99.
           05  VENC1-40           PIC 9(8).
           05  DIAS-40            PIC 9(3).
           05  DATA-MOVTO-40      PIC 9(8).
           05  SEQ-DUPLI-40       PIC 9(4).
           05  COD-COMPL-40       PIC 9(9).

       FD  CRD040A.
       01  REG-CRD040A.
           05  VENC-40A           PIC 9(8).
           05  BANCO-40A          PIC 9(4).
           05  NR-DUPLICATA-40A   PIC X(10).
           05  NOME-40A           PIC X(06).
           05  SEQ-40A            PIC 9(3).
           05  VALOR-40A          PIC 9(8)V99.
           05  VENC1-40A          PIC 9(8).
           05  DIAS-40A           PIC 9(3).
           05  DATA-MOVTO-40A     PIC 9(8).
           05  SEQ-DUPLI-40A      PIC 9(4).
           05  COD-COMPL-40A      PIC 9(9).

       FD  CRD041.
       01  REG-CRD041.
           05  NOME-41             PIC X(16).
           05  CARTEIRA-41         PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-41        PIC 9(8).
           05  TAXA-JUROS-41       PIC 99V99.
           05  DATA-INI-41         PIC 9(8).
           05  DATA-FIM-41         PIC 9(8).
           05  DIAS-INI-41         PIC 9(3).
           05  DIAS-FIM-41         PIC 9(3).
           05  FERIADOS-41         PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-DUPLI-41       PIC 9(4).
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
           05  PORTADOR-DESTINO-41 PIC 9(04).

       FD  CRD041A.
       01  REG-CRD041A.
           05  NOME-41A            PIC X(06).
           05  CARTEIRA-41A        PIC 9(2) OCCURS 3 TIMES.
           05  DATA-BASE-41A       PIC 9(8).
           05  TAXA-JUROS-41A      PIC 99V99.
           05  DATA-INI-41A        PIC 9(8).
           05  DATA-FIM-41A        PIC 9(8).
           05  DIAS-INI-41A        PIC 9(3).
           05  DIAS-FIM-41A        PIC 9(3).
           05  FERIADOS-41A        PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-DUPLI-41A      PIC 9(4).
           05  VLR-BRUTO-41A       PIC 9(10)V99.
           05  PM-41A              PIC 9(3)V99.
           05  DIAS-30-41A         PIC 9(3)V99.
           05  TAXA-30-41A         PIC 9V999999.
           05  JURO-30-41A         PIC 9(10)V99.
           05  SALDO-30-41A        PIC 9(10)V99.
           05  DIAS-60-41A         PIC 9(3)V99.
           05  TAXA-60-41A         PIC 9V999999.
           05  JURO-60-41A         PIC 9(10)V99.
           05  SALDO-60-41A        PIC 9(10)V99.
           05  DIAS-90-41A         PIC 9(3)V99.
           05  TAXA-90-41A         PIC 9V999999.
           05  JURO-90-41A         PIC 9(10)V99.
           05  SALDO-90-41A        PIC 9(10)V99.
           05  DIAS-120-41A        PIC 9(3)V99.
           05  TAXA-120-41A        PIC 9V999999.
           05  JURO-120-41A        PIC 9(10)V99.
           05  SALDO-120-41A       PIC 9(10)V99.
           05  DIAS-150-41A        PIC 9(3)V99.
           05  TAXA-150-41A        PIC 9V999999.
           05  JURO-150-41A        PIC 9(10)V99.
           05  SALDO-150-41A       PIC 9(10)V99.
           05  DIAS-180-41A        PIC 9(3)V99.
           05  TAXA-180-41A        PIC 9V999999.
           05  JURO-180-41A        PIC 9(10)V99.
           05  SALDO-180-41A       PIC 9(10)V99.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD040             PIC XX       VALUE SPACES.
           05  ST-CRD040A            PIC XX       VALUE SPACES.
           05  ST-CRD041             PIC XX       VALUE SPACES.
           05  ST-CRD041A            PIC XX       VALUE SPACES.
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
           MOVE "CRD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD040
           MOVE "CRD040A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD040A
           MOVE "CRD041"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD041
           MOVE "CRD041A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD041A

           OPEN I-O CRD040 CRD041 CRD040A CRD041A
           CLOSE    CRD040 CRD041 CRD040A CRD041A
           OPEN I-O CRD040 CRD041 CRD040A CRD041A

           CLOSE    CONTROLE
           IF ST-CRD040 = "35"
              CLOSE CRD040      OPEN OUTPUT  CRD040
              CLOSE CRD040      OPEN I-O     CRD040
           END-IF.

           IF ST-CRD040A = "35"
              CLOSE CRD040A     OPEN OUTPUT  CRD040A
              CLOSE CRD040A     OPEN I-O     CRD040A
           END-IF.

           IF ST-CRD041 = "35"
              CLOSE CRD041      OPEN OUTPUT  CRD041
              CLOSE CRD041      OPEN I-O     CRD041
           END-IF.

           IF ST-CRD041A = "35"
              CLOSE CRD041A     OPEN OUTPUT  CRD041A
              CLOSE CRD041A     OPEN I-O     CRD041A
           END-IF.

           display "vou comecar" stop " "

           INITIALIZE REG-CRD040A
           START CRD040A KEY IS NOT LESS CHAVE-40A INVALID KEY
                 MOVE "10" TO ST-CRD040A.

           PERFORM UNTIL ST-CRD040A = "10"
                 READ CRD040A NEXT RECORD AT END
                      MOVE "10" TO ST-CRD040A
                 NOT AT END
                      INITIALIZE REG-CRD040

                      MOVE VENC-40A          TO VENC-40
                      MOVE BANCO-40A         TO BANCO-40
                      MOVE NR-DUPLICATA-40A  TO NR-DUPLICATA-40
                      MOVE NOME-40A          TO NOME-40
                      MOVE SEQ-40A           TO SEQ-40
                      MOVE VALOR-40A         TO VALOR-40
                      MOVE VENC1-40A         TO VENC1-40
                      MOVE DIAS-40A          TO DIAS-40
                      MOVE DATA-MOVTO-40A    TO DATA-MOVTO-40
                      MOVE SEQ-DUPLI-40A     TO SEQ-DUPLI-40
                      MOVE COD-COMPL-40A     TO COD-COMPL-40


                      DISPLAY REG-CRD040
                      WRITE REG-CRD040
                      END-WRITE

                 END-READ
           END-PERFORM.

           INITIALIZE REG-CRD041A
           START CRD041A KEY IS NOT LESS NOME-41A INVALID KEY
                 MOVE "10" TO ST-CRD041A.

           PERFORM UNTIL ST-CRD041A = "10"
                 READ CRD041A NEXT RECORD AT END
                      MOVE "10" TO ST-CRD041A
                 NOT AT END
                      INITIALIZE REG-CRD041

                      MOVE NOME-41A         TO NOME-41
                      MOVE CARTEIRA-41A(1)  TO CARTEIRA-41(1)
                      MOVE CARTEIRA-41A(2)  TO CARTEIRA-41(2)
                      MOVE CARTEIRA-41A(3)  TO CARTEIRA-41(3)
                      MOVE DATA-BASE-41A    TO DATA-BASE-41
                      MOVE TAXA-JUROS-41A   TO TAXA-JUROS-41
                      MOVE DATA-INI-41A     TO DATA-INI-41
                      MOVE DATA-FIM-41A     TO DATA-FIM-41
                      MOVE DIAS-INI-41A     TO DIAS-INI-41
                      MOVE DIAS-FIM-41A     TO DIAS-FIM-41
                      MOVE FERIADOS-41A(1)  TO FERIADOS-41(1)
                      MOVE FERIADOS-41A(2)  TO FERIADOS-41(2)
                      MOVE FERIADOS-41A(3)  TO FERIADOS-41(3)
                      MOVE FERIADOS-41A(4)  TO FERIADOS-41(4)
                      MOVE FERIADOS-41A(5)  TO FERIADOS-41(5)
                      MOVE FERIADOS-41A(6)  TO FERIADOS-41(6)
                      MOVE FERIADOS-41A(7)  TO FERIADOS-41(7)
                      MOVE FERIADOS-41A(8)  TO FERIADOS-41(8)
                      MOVE FERIADOS-41A(9)  TO FERIADOS-41(9)
                      MOVE FERIADOS-41A(10) TO FERIADOS-41(10)
                      MOVE QTDE-DUPLI-41A   TO QTDE-DUPLI-41
                      MOVE VLR-BRUTO-41A    TO VLR-BRUTO-41
                      MOVE PM-41A           TO PM-41
                      MOVE DIAS-30-41A      TO DIAS-30-41
                      MOVE TAXA-30-41A      TO TAXA-30-41
                      MOVE JURO-30-41A      TO JURO-30-41
                      MOVE SALDO-30-41A     TO SALDO-30-41
                      MOVE DIAS-60-41A      TO DIAS-60-41
                      MOVE TAXA-60-41A      TO TAXA-60-41
                      MOVE JURO-60-41A      TO JURO-60-41
                      MOVE SALDO-60-41A     TO SALDO-60-41
                      MOVE DIAS-90-41A      TO DIAS-90-41
                      MOVE TAXA-90-41A      TO TAXA-90-41
                      MOVE JURO-90-41A      TO JURO-90-41
                      MOVE SALDO-90-41A     TO SALDO-90-41
                      MOVE DIAS-120-41A     TO DIAS-120-41
                      MOVE TAXA-120-41A     TO TAXA-120-41
                      MOVE JURO-120-41A     TO JURO-120-41
                      MOVE SALDO-120-41A    TO SALDO-120-41
                      MOVE DIAS-150-41A     TO DIAS-150-41
                      MOVE TAXA-150-41A     TO TAXA-150-41
                      MOVE JURO-150-41A     TO JURO-150-41
                      MOVE SALDO-150-41A    TO SALDO-150-41
                      MOVE DIAS-180-41A     TO DIAS-180-41
                      MOVE TAXA-180-41A     TO TAXA-180-41
                      MOVE JURO-180-41A     TO JURO-180-41
                      MOVE SALDO-180-41A    TO SALDO-180-41
                      MOVE ZEROS            TO PORTADOR-DESTINO-41

                      DISPLAY REG-CRD041
                      WRITE REG-CRD041
                      END-WRITE

                 END-READ
           END-PERFORM.


           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CRD040 CRD040A CRD041 CRD041A
           EXIT PROGRAM
           STOP RUN.
