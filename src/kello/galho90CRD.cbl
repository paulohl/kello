       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO90CRD.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 07/12/2004.
      *FUNÇÃO: LOCALIZA UMA PROGRAMAÇÃO FINANCEIRA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.
           COPY CRPX099.
           COPY LOGX003.

       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.
           COPY CRPW099.
           COPY LOGW003.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
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
           05  CONTADOR              PIC 9(09)    VALUE ZEROS.
           05  LISTEI                PIC X(01)    VALUE SPACES.
           05  ACP-PROGRAMACAO       PIC 9(09)    VALUE ZEROS.
           05  MASC-PROGRAMACAO      PIC ZZZ.ZZZ.ZZ9.
           05  MASC-VALOR            PIC ZZZ.ZZ9,99 VALUE ZEROS.
           05  ACP-DATA              PIC 9(08).
           05  AUX-DATA              PIC 9(08).
           05  MASC-DT               PIC 99/99/9999 BLANK WHEN ZEROS.
           05  ACP-VALOR             PIC 9(06)V99 VALUE ZEROS.
           05  RESPOSTA              PIC X(01)    VALUE SPACES.
           05  ACP-FORNECEDOR        PIC 9(06)    VALUE ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.
           05  ACP-DATA-EMISSAO      PIC 9(08)    VALUE ZEROS.
           05  WS-DATA               PIC 9(08)    VALUE ZEROS.
           05  WS-EMISSAO            PIC 9(08)    VALUE ZEROS.
           05  ACP-CONTRATO          PIC 9(08)    VALUE ZEROS.

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".

       01 MASC-DATA                PIC X(10) VALUE SPACES.


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.

           DISPLAY "Informar a Empresa: " AT 0101
           ACCEPT   EMP-REC               AT 0121

           IF EMP-REC = SPACES
              GO TO MAIN-PROCESS.


           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD099
           MOVE "LOG003"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003
           OPEN INPUT     CRD020 CRD099 LOG003
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O    CRD020
           END-IF.
           IF ST-CRD099 = "35"
              CLOSE CRD099      OPEN OUTPUT CRD099
              CLOSE CRD099      OPEN I-O    CRD099
           END-IF.
           IF ST-LOG003 = "35"
              CLOSE LOG003       OPEN OUTPUT LOG003
              CLOSE LOG003       OPEN I-O    LOG003
           END-IF.

       0010-PROCURAR-PROGRAMACAO.

           DISPLAY "Informar o Codigo do Contrato: " at 0101
           ACCEPT ACP-CONTRATO                       at 0132

           DISPLAY ERASE AT 0101

           INITIALIZE REG-LOG003
           MOVE "CRD020"               TO LOG3-ARQUIVO
           START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO INVALID KEY
                MOVE "10" TO ST-LOG003.

           PERFORM UNTIL ST-LOG003 = "10"
                READ LOG003 NEXT AT END
                     MOVE "10" TO ST-LOG003
                NOT AT END
                     IF LOG3-ARQUIVO <> "CRD020"
                        MOVE "10" TO ST-LOG003
                     ELSE
                        MOVE LOG3-REGISTRO TO REG-CRD099
                        READ CRD099 INVALID KEY
                             INITIALIZE REG-CRD099
                             MOVE REG-CRD099 TO REG-CRD020
                             READ CRD020 INVALID KEY
                                  INITIALIZE REG-CRD020
                             NOT INVALID KEY
                                  MOVE REG-CRD020 TO REG-CRD099
                             END-READ
                        END-READ
                        IF ACP-CONTRATO = CLIENTE-CR99
                           PERFORM LISTAR-LOG
      *                 ELSE
      *                    DISPLAY "CLIENTE-CR99 = " AT 0201
      *                    DISPLAY  CLIENTE-CR99     AT 0215
      *                    STOP " "
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           DISPLAY "ACABEI" STOP " "

           CLOSE CRD020 CRD099 LOG003
           EXIT PROGRAM
           STOP RUN.


       LISTAR-LOG SECTION.
           DISPLAY ERASE AT 0101

           DISPLAY "ACHEI O LOG     " AT 0520

           DISPLAY "DATA VENCTO = "   AT 0801
           STRING DATA-VENCTO-CR99(7:2) "/" DATA-VENCTO-CR99(5:2)
                  "/" DATA-VENCTO-CR99(1:4)

             INTO MASC-DATA
           DISPLAY DATA-VENCTO-CR99   AT 0815


           DISPLAY "VALOR DOCTO = "   AT 0901
           MOVE   VALOR-TOT-CR99      TO MASC-VALOR
           DISPLAY VALOR-TOT-CR99     AT 0915

           DISPLAY "CLIENTE     = "   AT 1001
           DISPLAY CLIENTE-CR99       AT 1015


           DISPLAY "OPERACAO = "      AT 1401
           DISPLAY LOG3-OPERACAO      AT 1412

           STRING LOG3-DIA "/" LOG3-MES "/" LOG3-ANO INTO MASC-DATA

           DISPLAY "DATA.... = "      AT 1430
           DISPLAY MASC-DATA          AT 1441

           MOVE SPACES                TO MASC-DATA
           STRING LOG3-HORA ":" LOG3-MINU INTO MASC-DATA

           DISPLAY "HORAS... = "      AT 1501
           DISPLAY MASC-DATA          AT 1512

           DISPLAY "USUARIO. = "      AT 1530
           DISPLAY LOG3-USUARIO       AT 1541

           DISPLAY "PROGRAMA = "      AT 1601
           DISPLAY LOG3-PROGRAMA      AT 1612

           STOP "  ".

