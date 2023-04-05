       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO80.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 21-03-2007.
      *FUNÇÃO: gera um novo arquivo de contas a pagar

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY MTPX020.

           COPY RCPX100.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW020.

           COPY RCPW100.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
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
           05  nao-gravado           pic 9(06)    VALUE ZEROS.
           05  erro                  pic 9(06)    VALUE ZEROS.
           05  sucesso               pic 9(06)    VALUE ZEROS.
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
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           OPEN I-O   MTD020 RCD100
           CLOSE      MTD020 RCD100
           OPEN I-O   MTD020 RCD100

           CLOSE CONTROLE.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O    MTD020
           END-IF.

           IF ST-RCD100 = "35"
              CLOSE RCD100      OPEN OUTPUT RCD100
              CLOSE RCD100      OPEN I-O    RCD100
           END-IF.


           IF ST-MTD020 <> "00" OR ST-RCD100
              CLOSE MTD020 RCD100
              stop run.


           display "VOU EXECUTAR O GALHO81" STOP " ".

           initialize reg-rcd100
           start rcd100 key is not less album-rec invalid key
               move "10" to st-rcd100.
           perform until st-rcd100 = "10"
               read rcd100 next at end
                   move "10" to st-rcd100
               not at end
                   display reg-rcd100
                   move album-rec          to album-mtg
                   read mtd020 not invalid key
                       move 1 to fogo-mtg
                       rewrite reg-mtd020 invalid key
                           display "Erro Gravacao...MTD020" stop " "
                       end-rewrite
                   end-read
               end-read
           end-perform

           CLOSE MTD020 RCD100

           display all spaces


           display "ACABEI" STOP " ".
           display "ACABEI" STOP " ".
           EXIT PROGRAM
           STOP RUN.
