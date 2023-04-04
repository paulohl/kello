       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO48.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09/06/2005.
      *FUNÇÃO: ALTERA LAYOUT DO COP041

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY COPX041.

           SELECT COD999 ASSIGN TO PATH-COD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CO99 = NR-CONTRATO-CO99
                                             CURSO-CO99 TURMA-CO99
                  STATUS IS ST-COD999.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW041.

       FD  COD999.
       01  REG-COD999.
           05  NR-CONTRATO-CO99  PIC 9(4).
           05  CURSO-CO99        PIC 9(3).
           05  TURMA-CO99        PIC XX.
           05  NR-PREV-FORM-CO99 PIC 9(4).
           05  LISTA-ALUNOS-CO99 PIC 9.
      *    0-Não está OK    1-Lista completa
           05  RESPONSAVEL-CO99  PIC 9(4).
      *    Nr-contrato + responsavel = nome presidente/responsavel
           05  FOTO-TURMA-CO99   PIC 9.
           05  MEDIDA-BECA-CO99  PIC 9.
      *    Foto-turma e Medida-beca = 0-não  1-sim
           05  COR-FAIXA-CO99    PIC X(10).
           05  COR-CANUDO-CO99   PIC X(10).
           05  TIPO-ALBUM-CO99   PIC 9(5).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD999             PIC XX       VALUE SPACES.
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
           MOVE "COD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041
           MOVE "COD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD999
           OPEN I-O   COD041
           CLOSE      COD041
           OPEN I-O   COD041
           OPEN INPUT COD999

           CLOSE CONTROLE.
           IF ST-COD041 = "35"
              CLOSE COD041      OPEN OUTPUT COD041
              CLOSE COD041      OPEN I-O COD041
           END-IF.

           display "Vou comecar a atualizar o COD041" stop " ".
           display "Vou comecar a atualizar o COD041" stop " ".

           IF ST-COD041 <> "00" OR ST-COD999 <> "00"
              CLOSE COD041
                    COD999
              stop run.

           INITIALIZE REG-COD999

           START COD999 KEY IS NOT LESS CHAVE-CO99 INVALID KEY
                 MOVE "10" TO ST-COD999.


           PERFORM UNTIL ST-COD999 = "10"
             READ COD999 NEXT RECORD AT END
                  MOVE "10" TO ST-COD999
             NOT AT END
                  MOVE NR-CONTRATO-CO99   TO NR-CONTRATO-CO41
                  MOVE CURSO-CO99         TO CURSO-CO41
                  MOVE TURMA-CO99         TO TURMA-CO41
                  MOVE NR-PREV-FORM-CO99  TO NR-PREV-FORM-CO41
                  MOVE LISTA-ALUNOS-CO99  TO LISTA-ALUNOS-CO41
                  MOVE RESPONSAVEL-CO99   TO RESPONSAVEL-CO41
                  MOVE FOTO-TURMA-CO99    TO FOTO-TURMA-CO41
                  MOVE MEDIDA-BECA-CO99   TO MEDIDA-BECA-CO41
                  MOVE COR-FAIXA-CO99     TO COR-FAIXA-CO41
                  MOVE COR-CANUDO-CO99    TO COR-CANUDO-CO41
                  MOVE TIPO-ALBUM-CO99    TO TIPO-ALBUM-CO41
                  MOVE SPACES             TO TURNO-CO41
                  display reg-cod041
                  WRITE REG-COD041
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE COD999
           CLOSE COD041
           EXIT PROGRAM.
