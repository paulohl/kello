       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO74.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 23-01-2007.
      *FUNÇÃO: ALTERA CODIGO REDUZIDO CXD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CXPX100.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CXPW100.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD100             PIC XX       VALUE SPACES.
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
           05  ACP-DATA-MOVTO        PIC 9(08)    VALUE ZEROS.
           05  ACP-SEQUENCIA         PIC 9(04)    VALUE ZEROS.
           05  ACP-CODRED            PIC 9(03)    VALUE ZEROS.
           05  CONTINUAR             PIC X(01)    VALUE SPACES.
           05  MASC-DATA             PIC 99/99/9999 VALUE ZEROS.
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
           MOVE "CXD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD100
           OPEN I-O   CXD100
           CLOSE      CXD100
           OPEN I-O   CXD100

           CLOSE CONTROLE.
           IF ST-CXD100 = "35"
              CLOSE CXD100      OPEN OUTPUT CXD100
              CLOSE CXD100      OPEN I-O    CXD100
           END-IF.

           IF ST-CXD100 <> "00"
              CLOSE CXD100
              stop run.

       PROXIMO.
           display "Data Movimento..: " at 0101
           display "                         " at 0119
           accept acp-data-movto        at 0119
           move acp-data-movto          to masc-data
           display masc-data            at 0119

           if acp-data-movto = 0
              go to proximo.

       sequencia.
           display "SEQUENCIA.......: " at 0201
           display "                         " at 0219
           accept acp-sequencia         at 0219

           if acp-sequencia = 0
              go to sequencia.


           string acp-data-movto(5:4) acp-data-movto(3:2)
                  acp-data-movto(1:2) into data-mov-cx100

           move   acp-sequencia         to seq-cx100

           read cxd100 invalid key
               display "Erro de Leitura...CXD100" at 1001
           not invalid key

               move conta-reduz-cx100       to acp-codred

               display "Codigo Red......: " at 0501
               accept acp-codred            at 0519

               move acp-codred              to conta-reduz-cx100

               rewrite reg-cxd100 invalid key
                   display "Erro de Gravacao...CXD100" at 1001
               end-rewrite
           end-read.

       continua.
           display "Deseja Continuar (S/N) : " at 1001
           accept continuar                    at 1026

           if continuar <> "S" and "N" and "s" and "n"
              go to continua.

           if continuar = "S" or "s"
              go to proximo.

           CLOSE CXD100
           EXIT PROGRAM
           STOP RUN.
