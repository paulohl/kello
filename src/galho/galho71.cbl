       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO71.
      *DATA: 05-10-2006.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: ALTERAR LAYOUT DO CGD001.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY CGPX001.

           COPY CGPX006.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CGPW001.

           COPY CGPW006.

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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD006             PIC XX       VALUE SPACES.
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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001
           MOVE "CGD006"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD006

           OPEN I-O   CGD001 CGD006

           CLOSE CONTROLE.
           IF ST-CGD001 = "35"
              CLOSE CGD001    OPEN OUTPUT CGD001
              CLOSE CGD001    OPEN I-O    CGD001
           END-IF.

           IF ST-CGD006 = "35"
              CLOSE CGD006    OPEN OUTPUT CGD006
              CLOSE CGD006    OPEN I-O    CGD006
           END-IF.


           display "vou comecar" stop " ".

           INITIALIZE REG-CGD001

           START CGD001 KEY IS NOT LESS CODIGO-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.

           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD001
             NOT AT END
                  MOVE 0 TO T-TERCEIRIZADO-CG01
                  MOVE 0 TO T-FRANQUIA-CG01
                  display reg-cgd001
                  REWRITE REG-CGD001 INVALID KEY
                       DISPLAY "Erro de Regravação " reg-cgd001 stop " "
                  END-REWRITE
             END-READ
           END-PERFORM

           INITIALIZE REG-CGD006

           START CGD006 KEY IS NOT LESS CODIGO-CG06 INVALID KEY
                 MOVE "10" TO ST-CGD006.

           PERFORM UNTIL ST-CGD006 = "10"
             READ CGD006 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD006
             NOT AT END
                  MOVE 0 TO PREFERENCIAL-CG06
                  display reg-cgd006
                  display reg-cgd006
                  REWRITE REG-CGD006 INVALID KEY
                       DISPLAY "Erro de Regravação " reg-cgd006 stop " "
                  END-REWRITE
             END-READ
           END-PERFORM

           DISPLAY "ACABOU DE EXECUTAR O GALHO71" STOP " ".

           CLOSE CGD001 CGD006
           EXIT PROGRAM
           STOP RUN.

