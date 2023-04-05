       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO62.
      *DATA: 12-05-2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: Atualiza a flag ATUALIZADO-CC-CC120 p/ 1
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CCPX120.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CCPW120.
       WORKING-STORAGE SECTION.
           COPY "CCP121.CPB".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD120             PIC XX       VALUE SPACES.
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
           MOVE "CCD120" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD120.
           OPEN I-O CCD120
           CLOSE CONTROLE.
           IF ST-CCD120 = "35"
              CLOSE CCD120      OPEN OUTPUT CCD120
              CLOSE CCD120      OPEN I-O CCD120
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO62" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO62" STOP " ".

           INITIALIZE REG-CCD120
           MOVE 200605 TO MESANO-BASE-CC120
           MOVE 318    TO CODIGO-CC120


      *    2478
      *    318
      *    1452
      *    4185


           START CCD120 KEY IS NOT LESS CHAVE-CC120 INVALID KEY
                 MOVE "10" TO ST-CCD120.

           PERFORM UNTIL ST-CCD120 = "10"
             READ CCD120 NEXT RECORD AT END
                  MOVE "10" TO ST-CCD120
             NOT AT END
                  IF MESANO-BASE-CC120 <> 200605
                     MOVE "10" TO MESANO-BASE-CC120
                  ELSE
                     IF CODIGO-CC120 = 318 OR 1452 OR 2478 OR 4185

                        MOVE 1 TO ATUALIZADO-CC-CC120
                        DISPLAY "VOU REGRAVAR " STOP " "
                        DISPLAY "MESANO-BASE-CC120 = " MESANO-BASE-CC120
                        DISPLAY "VENDEDOR = " CODIGO-CC120
                        DISPLAY "VALOR = " VALOR-CREDITO-CC120 STOP " "
                        REWRITE REG-CCD120 INVALID KEY
                               DISPLAY "ERRO DE REGRAVACAO" STOP " "
                        END-REWRITE
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

           DISPLAY "ACABOU DE EXECUTAR O GALHO62" STOP " ".

           CLOSE CCD120
           EXIT PROGRAM
           STOP RUN.

