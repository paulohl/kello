       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO65.
      *DATA: 11-09-2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LE CHEQUES EXCLUIDOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CHPX099.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW099.


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
           05  ST-CHD099             PIC XX       VALUE SPACES.
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
           MOVE "CHD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           OPEN I-O CHD099
           CLOSE CONTROLE.
           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O CHD099
           END-IF.

           INITIALIZE REG-CHD099
           MOVE 20060929 TO DATA-MOVTO-CH99
      *    MOVE 3        TO SEQ-CH99


           START CHD099 KEY IS NOT LESS CHAVE-CH99 INVALID KEY
                 MOVE "10" TO ST-CHD099.

           PERFORM UNTIL ST-CHD099 = "10"
             READ CHD099 NEXT RECORD AT END
                  MOVE "10" TO ST-CHD099
             NOT AT END
                  IF DATA-MOVTO-CH99 <> 20060929
      /           OR
      *              SEQ-CH99        <> 3
                     MOVE "10" TO ST-CHD099
                  ELSE
                     DISPLAY "DATA MOVTO.: "   AT 0101
                              DATA-MOVTO-CH99  AT 0113
                     DISPLAY "SEQUENCIA..: "   AT 0201
                              SEQ-CH99         AT 0213
                     DISPLAY "CONTRATO...: "   AT 0301
                              CLIENTE-CH99     AT 0313
                     DISPLAY "NOME-CH99..: "   AT 0401
                              NOME-CH99        AT 0413
                     DISPLAY "CIDADE.....: "   AT 0501
                              CIDADE-CH99      AT 0513
                     DISPLAY "CONTA C/C..: "   AT 0601
                              CONTA-CORR-CH99  AT 0613
                     DISPLAY "BANCO......: "   AT 0701
                              BANCO-CH99       AT 0713
                     DISPLAY "AGENCIA....: "   AT 0801
                              AGENCIA-CH99     AT 0813
                     DISPLAY "NR-CHEQUE..: "   AT 0901
                              NR-CHEQUE-CH99   AT 0913
                     DISPLAY "CPF........: "   AT 1001
                              CPF-CH99         AT 1013
                     DISPLAY "DATA VENCTO: "   AT 1101
                              DATA-VENCTO-CH99 AT 1113
                     DISPLAY "DIGITADOR..: "   AT 1201
                              DIGITADOR-CH99   AT 1213

                     DISPLAY "USU.EXCL...: "   AT 1301
                              USUARIO-EXCLUSAO-CH99 AT 1313

                     DISPLAY "DATA.EXCL..: "   AT 1401
                              DATA-EXCLUSAO-CH99 AT 1413

                     DISPLAY "HORA.EXCL..: "   AT 1501
                              HORA-EXCLUSAO-CH99 AT 1513

                     STOP " "
                  END-IF
             END-READ
           END-PERFORM

           DISPLAY "ACABOU DE EXECUTAR O GALHO62" STOP " ".

           CLOSE CHD099
           EXIT PROGRAM
           STOP RUN.

