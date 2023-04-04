       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO67.
      *DATA: 05-10-2006.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LE LOG DO CAIXA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY LOGCAIXA.SEL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY LOGCAIXA.FD.


       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  FS-LOGCAIXA           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".
       01  CAB01.
           05  EMPRESA-REL         PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(10)   VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RELACAO DE CREDITOS DE REPORTAGEM".
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-REL          PIC 99/9999.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "FUNCIONARIO                     TOTAL-CREDITOS".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "LOGCAIX" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGCAIXA
           OPEN I-O LOGCAIXA
           CLOSE CONTROLE.
           IF FS-LOGCAIXA = "35"
              CLOSE LOGCAIXA    OPEN OUTPUT LOGCAIXA
              CLOSE LOGCAIXA    OPEN I-O    LOGCAIXA
           END-IF.

           INITIALIZE REG-LOGCAIXA
      *    MOVE 20060929 TO DATA-MOV-LOGCAIXA
      *    MOVE 3        TO SEQ-LOGCAIXA


           DISPLAY "VOU PROCURAR " STOP " "
           DISPLAY "VOU PROCURAR " STOP " "

           START LOGCAIXA KEY IS NOT LESS CHAVE-CX100-LOGCAIXA
               INVALID KEY
                 MOVE "10" TO FS-LOGCAIXA.

           PERFORM UNTIL FS-LOGCAIXA = "10"
             READ LOGCAIXA NEXT RECORD AT END
                  MOVE "10" TO FS-LOGCAIXA
             NOT AT END
      *           IF DATA-MOV-LOGCAIXA <> 20060929
      *           OR SEQ-LOGCAIXA      <> 3
      *              MOVE "10" TO FS-LOGCAIXA
      *           ELSE
                  IF OPERACAO-LOGCAIXA <> "I"
                     DISPLAY "DATA MOVTO.: "     AT 0101
                              DATA-MOV-LOGCAIXA  AT 0113
                     DISPLAY "SEQUENCIA..: "     AT 0201
                              SEQ-LOGCAIXA       AT 0213
                     DISPLAY "TIPO LCTO..: "     AT 0301
                              TIPO-LCTO-LOGCAIXA AT 0313
                     DISPLAY "HISTORICO..: "     AT 0401
                              HISTORICO-LOGCAIXA AT 0413
                     DISPLAY "DOCUMENTO..: "     AT 0501
                              DOCUMENTO-LOGCAIXA AT 0513
                     DISPLAY "VALOR......: "     AT 0601
                              VALOR-LOGCAIXA     AT 0613
                     DISPLAY "CT.PART....: "     AT 0701
                              CONTAPART-LOGCAIXA AT 0713
                     DISPLAY "CT.REDUZ...: "     AT 0801
                              CONTA-REDUZ-LOGCAIXA AT 0813
                     DISPLAY "CONTABIL...: "     AT 0901
                              CONTABIL-LOGCAIXA  AT 0913
                     DISPLAY "RESPONSAVEL: "     AT 1001
                              RESPONSAVEL-LOGCAIXA AT 1013
                     DISPLAY "SEQ.DESM...: "     AT 1101
                              SEQ-DESM-LOGCAIXA   AT 1113

                     EVALUATE OPERACAO-LOGCAIXA
                         WHEN "I" DISPLAY "OPERACAO...: INCLUSAO"AT 1201
                         WHEN "T" DISPLAY "OPERACAO...: EXCL. PROB DATA"
                                  AT 1201
                         WHEN "E" DISPLAY "OPERACAO...: EXCLUSAO" AT
                                  1201
                     END-EVALUATE


                     DISPLAY "USUARIO....: "   AT 1301
                              USUARIO-LOGCAIXA      AT 1313
                     STOP " "


                  END-IF
             END-READ
           END-PERFORM

           DISPLAY "ACABOU DE EXECUTAR O GALHO67" STOP " ".

           CLOSE LOGCAIXA
           EXIT PROGRAM
           STOP RUN.

