       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO61.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 28-03-2006.
      *FUNÇÃO: PROGRAMA Q ARRUMA OS VALORES DOS CHEQUES CHD010

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CHPX010.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CHPW010.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
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


       01 TAB-VALOR                      PIC 9(8)V99.
       01 TAB-VALOR-R REDEFINES TAB-VALOR.
          05 TAB-VALOR1                  PIC 9(01).
          05 TAB-VALOR2                  PIC 9(01).
          05 TAB-VALOR3                  PIC 9(01).
          05 TAB-VALOR4                  PIC 9(01).
          05 TAB-VALOR5                  PIC 9(01).
          05 TAB-VALOR6                  PIC 9(01).
          05 TAB-VALOR7                  PIC 9(01).
          05 TAB-VALOR8                  PIC 9(01).
          05 TAB-VALOR9                  PIC 9(01).
          05 TAB-VALOR10                 PIC 9(01).


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           OPEN I-O   CHD010

           CLOSE CONTROLE.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O    CHD010
           END-IF.

           display "Vou comecar a atualizar o CHD010" stop " ".
           display "Vou comecar a atualizar o CHD010" stop " ".

           INITIALIZE REG-CHD010

           START CHD010 KEY IS NOT LESS CHAVE-CH10 INVALID KEY
                 MOVE "10" TO ST-CHD010.


           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD010
             NOT AT END
                  DISPLAY REG-CHD010

                  MOVE VALOR-CH10  TO TAB-VALOR

                  IF TAB-VALOR1 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR1
                  END-IF
                  IF TAB-VALOR2 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR2
                  END-IF
                  IF TAB-VALOR3 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR3
                  END-IF
                  IF TAB-VALOR4 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR4
                  END-IF
                  IF TAB-VALOR5 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR5
                  END-IF
                  IF TAB-VALOR6 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR6
                  END-IF
                  IF TAB-VALOR7 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR7
                  END-IF
                  IF TAB-VALOR8 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR8
                  END-IF
                  IF TAB-VALOR9 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR9
                  END-IF
                  IF TAB-VALOR10 IS NOT NUMERIC
                     MOVE 0 TO TAB-VALOR10
                  END-IF

                  MOVE TAB-VALOR TO VALOR-CH10


                  REWRITE REG-CHD010
                  END-REWRITE
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD010
           EXIT PROGRAM.
