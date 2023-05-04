       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO96.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 06/06/2009.
      *FUNÇÃO: ALTERACAO DO LOG003

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY LOGX003.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY LOGW003.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC x.
           05  RESP-MSG              PIC x.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(8).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(13).
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


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ       CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "LOG003"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-LOG003

           OPEN I-O   LOG003

           DISPLAY "VOU COMECAR " STOP " "

           INITIALIZE REG-LOG003
           MOVE "CAD099"           TO LOG3-ARQUIVO
           START LOG003 KEY IS NOT LESS LOG3-CH-ARQUIVO INVALID KEY
                MOVE "10" TO ST-LOG003.

           PERFORM UNTIL ST-LOG003 = "10"
                READ LOG003 NEXT AT END
                     MOVE "10" TO ST-LOG003
                NOT AT END
                     IF LOG3-ARQUIVO <> "CAD099"
                        MOVE "10" TO ST-LOG003
                     ELSE
                        MOVE "CPD099" TO LOG3-ARQUIVO
                        DISPLAY "REG-LOGCAIXA = " REG-LOG003
                        REWRITE REG-LOG003 INVALID KEY
                          MOVE "Erro de Regravação...LOG003" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
                        END-REWRITE
                     END-IF
                END-READ
           END-PERFORM

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE LOG003
           EXIT PROGRAM
           STOP RUN.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

