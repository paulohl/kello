       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO68.
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


           SELECT LOGCAIXA2 ASSIGN       TO ARQUIVO-LOGCAIXA2
                            ORGANIZATION IS          INDEXED
                            ACCESS MODE  IS          DYNAMIC
                            RECORD KEY   IS   CHAVE-LOGCAIXA2
                            ALTERNATE    RECORD          KEY
                            IS CHAVE-CX100-LOGCAIXA2 =
                               DATA-MOV-LOGCAIXA2
                               SEQ-LOGCAIXA2
                               DATA-LANCAMENTO-LOGCAIXA2
                               SEQUENCIA-LANCAMENTO-LOGCAIXA2
                            LOCK   MODE  IS        AUTOMATIC
                            WITH   LOCK  ON           RECORD
                            FILE STATUS  IS      FS-LOGCAIXA2.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY LOGCAIXA.FD.


       FD LOGCAIXA2.
       01 REG-LOGCAIXA2.
          05 CHAVE-LOGCAIXA2.
             10 DATA-LANCAMENTO-LOGCAIXA2        PIC 9(08).
             10 HORA-LANCAMENTO-LOGCAIXA2        PIC 9(04).
             10 SEQUENCIA-LANCAMENTO-LOGCAIXA2   PIC 9(04).
          05 REGISTRO2-CX100.
             10 SEQ-LOGCAIXA2                    PIC 9(04).
             10 DATA-MOV-LOGCAIXA2               PIC 9(08).
             10 TIPO-LCTO-LOGCAIXA2              PIC 99.
      * Tipo-lcto < 50 - saídas     e tipo-lcto >= 50 - entradas
             10 HISTORICO-LOGCAIXA2              PIC X(30).
             10 DOCUMENTO-LOGCAIXA2              PIC X(10).
             10 VALOR-LOGCAIXA2                  PIC 9(10)V99.
             10 CONTAPART-LOGCAIXA2              PIC 9(06).
             10 CONTA-REDUZ-LOGCAIXA2            PIC 9(03).
             10 CONTABIL-LOGCAIXA2               PIC 9.
      * será usado no futuro - para informar se o lancto é contábil
             10 RESPONSAVEL-LOGCAIXA2            PIC X(5).
             10 SEQ-DESM-LOGCAIXA2               PIC 9(4).
      * operacao-logcaixa =
      *               "I" = Inclusão
      *               "T" = Exclusão de Registro c/ Problema na data
      *               "E" = Exclusão
          05  USUARIO-LOGCAIXA2                  PIC X(05).
          05  OPERACAO-LOGCAIXA2                 PIC X(01).

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
           05  FS-LOGCAIXA2          PIC XX       VALUE SPACES.
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
           MOVE "LOGCAI2" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGCAIXA2
           MOVE "LOGCAIX" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGCAIXA
           OPEN I-O   LOGCAIXA
           OPEN INPUT LOGCAIXA2

           CLOSE CONTROLE.
           IF FS-LOGCAIXA = "35"
              CLOSE LOGCAIXA    OPEN OUTPUT LOGCAIXA
              CLOSE LOGCAIXA    OPEN I-O    LOGCAIXA
           END-IF.
           IF FS-LOGCAIXA2 = "35"
              CLOSE LOGCAIXA2    OPEN OUTPUT LOGCAIXA2
              CLOSE LOGCAIXA2    OPEN I-O    LOGCAIXA2
           END-IF.

           display "vou comecar" stop " ".

           INITIALIZE REG-LOGCAIXA2

           START LOGCAIXA2 KEY IS NOT LESS CHAVE-CX100-LOGCAIXA2
               INVALID KEY
                 MOVE "10" TO FS-LOGCAIXA2.

           PERFORM UNTIL FS-LOGCAIXA2 = "10"
             READ LOGCAIXA2 NEXT RECORD AT END
                  MOVE "10" TO FS-LOGCAIXA2
             NOT AT END
                  MOVE REG-LOGCAIXA2 TO REG-LOGCAIXA
                  display reg-logcaixa
                  WRITE REG-LOGCAIXA INVALID KEY
                       DISPLAY "Erro de Gravação " reg-logcaixa stop " "
                  END-WRITE
             END-READ
           END-PERFORM

           DISPLAY "ACABOU DE EXECUTAR O GALHO68" STOP " ".

           CLOSE LOGCAIXA
           EXIT PROGRAM
           STOP RUN.

