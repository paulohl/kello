       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECALC.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 07-12-2007
      *DESCRIÇÃO: Recalcula o saldo do extrato bancário

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CBPX101.
           COPY CBPX102.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CBPW101.
       COPY CBPW102.

       WORKING-STORAGE SECTION.
           COPY "CBP101.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPADAY1".
           COPY "CPDIAS1".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD101             PIC XX       VALUE SPACES.
           05  ST-CBD102             PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.

           05  ULTIMA-SEQ            PIC 9(3)     VALUE ZEROS.
      *    Ult-SEQUENCIA-Será utilizado p/ encontrar a última sequencia
      *    utilizada do movto diario
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W = 0 (não ocorreu erro abertura) erro-w=1 (houve erro)
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-EXTRATO-I        PIC 9(8)     VALUE ZEROS.
           05  DATA-EXTRATO-HEAD     PIC 9(8)     VALUE ZEROS.
           05  BANCO-HEAD            PIC 9(6)     VALUE ZEROS.
           05  DESC-BANCO-HEAD       PIC X(30)    VALUE SPACES.
           05  DATA-ULT-EXTRATO-HEAD PIC 9(8)     VALUE ZEROS.
           05  SALDO-INICIAL-HEAD    PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL-HEAD      PIC S9(8)V99 VALUE ZEROS.
           05  DATA-ANT              PIC 9(8)     VALUE ZEROS.
           05  BANCO-ANT             PIC 9(6)     VALUE ZEROS.
           05  ULTIMA-DATA           PIC 9(8)     VALUE ZEROS.
           05  ULTIMO-SALDO          PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-E                PIC 99/99/9999.
           05  MENSAGEM              PIC X(200)   VALUE SPACES.
           05  TIPO-MSG              PIC X(01)    VALUE SPACES.
           05  RESP-MSG              PIC X(01)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           COPY "PARAMETR".

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(46)   VALUE
           "CONFER.MOVIMENTO DE EXTRATO BANCARIO DO DIA: ".
           05  DATA-MOV-REL        PIC 99/99/9999.
           05  FILLER              PIC X(7)    VALUE "   HR: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "SEQ.  HISTORICO                         NR-DOCTO   VALOR-CRE
      -    "DITO  VALOR-DEBITO".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ       CONTROLE

           MOVE NOME-EMP      TO EMPRESA-REL
           MOVE EMPRESA       TO EMP-REC
           MOVE "CBD101"      TO ARQ-REC
           MOVE EMPRESA-REF   TO PATH-CBD101
           MOVE "CBD102"      TO ARQ-REC
           MOVE EMPRESA-REF   TO PATH-CBD102

           OPEN I-O    CBD101

           OPEN OUTPUT CBD102
           CLOSE       CBD102
           OPEN I-O    CBD102

           CLOSE      CONTROLE
           IF ST-CBD101 = "35"
              CLOSE CBD101      OPEN OUTPUT CBD101
              CLOSE CBD101      OPEN I-O CBD101
           END-IF
           IF ST-CBD102 = "35"
              CLOSE CBD102      OPEN OUTPUT CBD102
              CLOSE CBD102      OPEN I-O CBD102
           END-IF

           IF ST-CBD101 <> "00"
              DISPLAY "ERRO ABERTURA CBD101 " ST-CBD101 STOP " ".

           IF ST-CBD102 <> "00"
              DISPLAY "ERRO ABERTURA CBD102 " ST-CBD102 STOP " ".


           move "Vou recalcular os Saldos" to mensagem
           move "C" to tipo-msg
           perform 140-exibir-mensagem


           INITIALIZE REG-CBD101
           START CBD101 KEY IS NOT LESS CHAVE-CB101 INVALID KEY
               MOVE "10" TO ST-CBD101.

           PERFORM UNTIL ST-CBD101 = "10"
               READ CBD101 NEXT AT END
                   MOVE "10" TO ST-CBD101
               NOT AT END
                   MOVE DATA-EXTRATO-CB101 TO DATA-EXTRATO-CB102
                   MOVE CONTA-BANCO-CB101  TO CONTA-BANCO-CB102
                   READ CBD102 INVALID KEY
                        MOVE ZEROS TO SALDO-CB102
                        IF HISTORICO-CB101 < 50
                           SUBTRACT VALOR-CB101 FROM SALDO-CB102
                        ELSE
                           ADD VALOR-CB101  TO SALDO-CB102
                        END-IF
                        WRITE REG-CBD102
                        END-WRITE
                   NOT INVALID KEY
                        IF HISTORICO-CB101 < 50
                           SUBTRACT VALOR-CB101 FROM SALDO-CB102
                        ELSE
                           ADD VALOR-CB101 TO SALDO-CB102
                        END-IF
                        REWRITE REG-CBD102
                        END-REWRITE
                   END-READ
               END-READ
           END-PERFORM

           move "Acabei de recalcular os Saldos" to mensagem
           move "C" to tipo-msg
           perform 140-exibir-mensagem

           CLOSE CBD101 CBD102.
           STOP RUN.

       140-EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

