       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO15.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 10/11/2004.
      *FUNÇÃO: ATUALIZAR O CAMPO QTDE-FORM-INI-CO40 QUE ESTA COM O VALOR
      *ZERO COM O VALOR DO CAMPO QTDE-FORM-CO40

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY COPX040.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY COPW040.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD040             PIC XX       VALUE SPACES.
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
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           OPEN I-O COD040.
           CLOSE CONTROLE.
           IF ST-COD040 = "35"
              CLOSE COD040      OPEN OUTPUT COD040
              CLOSE COD040      OPEN I-O COD040
           END-IF.

           INITIALIZE REG-COD040

           MOVE ZEROS TO PAG-W.

           MOVE ZEROS          TO NR-CONTRATO-CO40

           START COD040 KEY IS NOT LESS NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
             READ COD040 NEXT RECORD AT END MOVE "10" TO ST-COD040
              NOT AT END
                   DISPLAY NR-CONTRATO-CO40
                   IF QTDE-FORM-INI-CO40 = 0
                      MOVE QTDE-FORM-CO40 TO QTDE-FORM-INI-CO40
                      REWRITE REG-COD040
                      END-REWRITE
                   END-IF
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE COD040
           EXIT PROGRAM
           STOP RUN.
