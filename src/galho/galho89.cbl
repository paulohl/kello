       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO89.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09-01-2008.
      *FUNÇÃO: ALTERA O ARQUIVO CXD004 LAYOUT

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY CXPX004.

           SELECT CXD004a ASSIGN TO PATH-CXD004a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-CXD004a
                  RECORD KEY IS CHAVE-CX004a = COD-USUARIO-CX004a
                      PROGRAMA-CX004a.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CXPW004.

       FD  CXD004a.
       01  REG-CXD004a.
           05  COD-USUARIO-CX004a   PIC 9(3).
           05  PROGRAMA-CX004a      PIC 9(3).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD004             PIC XX       VALUE SPACES.
           05  ST-CXD004A            PIC XX       VALUE SPACES.
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

           05  CONTADOR-TOTAL        PIC 9(09).
           05  CONTADOR-ERRO         PIC 9(09).

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
           MOVE "CXD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD004
           MOVE "CXD004A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD004A

           OPEN I-O   CXD004

           OPEN INPUT CXD004A

           CLOSE CONTROLE.
           IF ST-CXD004A = "35"
              CLOSE CXD004A      OPEN OUTPUT CXD004A
              CLOSE CXD004A      OPEN I-O    CXD004A
           END-IF.
           IF ST-CXD004 = "35"
              CLOSE CXD004      OPEN OUTPUT CXD004
              CLOSE CXD004      OPEN I-O    CXD004
           END-IF.

           DISPLAY "VOU COMEÇAR O GALHO89" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO89" STOP " ".


           INITIALIZE REG-CXD004A

           START CXD004A KEY IS NOT LESS CHAVE-CX004a INVALID KEY
               MOVE "10" TO ST-CXD004A.

           PERFORM UNTIL ST-CXD004A = "10"
               READ CXD004A NEXT AT END
                   MOVE "10" TO ST-CXD004A
               NOT AT END
                   MOVE COD-USUARIO-CX004a TO COD-USUARIO-CX004
                   MOVE PROGRAMA-CX004a    TO PROGRAMA-CX004

                   DISPLAY REG-CXD004
                   WRITE REG-CXD004

               END-READ
           END-PERFORM.

           DISPLAY "ACABOU DE EXECUTAR O GALHO89" STOP " ".

           CLOSE CXD004 CXD004A
           EXIT PROGRAM
           STOP RUN.
