       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO60.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 28-01-2006.
      *FUNÇÃO: PROGRAMA QUE JOGA OS DADOS DO MTD019 QDO O MESMO NÃO
      *        EXISTIR NO CGD010

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY MTPX019.
           COPY CGPX010.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY MTPW019.
           COPY CGPW010.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
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
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010
           OPEN INPUT MTD019
           OPEN I-O   CGD010

           CLOSE CONTROLE.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.

           display "Vou comecar a atualizar o CGD010" stop " ".
           display "Vou comecar a atualizar o CGD010" stop " ".

           IF ST-MTD019 <> "00" OR ST-CGD010 <> "00"
              CLOSE MTD019
                    CGD010
              stop run.

           INITIALIZE REG-MTD019

           START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.


           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD019
             NOT AT END
                  DISPLAY REG-MTD019

                  MOVE ZEROS      TO CLASSIF-CG10
                  MOVE ALBUM-MT19 TO CODIGO-CG10
                  READ CGD010 INVALID KEY
                       MOVE ZEROS      TO CLASSIF-CG10
                       MOVE ALBUM-MT19 TO CODIGO-CG10
                       MOVE NOME-FORM-MT19 TO COMPRADOR-CG10
                       WRITE REG-CGD010
                       END-WRITE
                  END-READ
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE MTD019
           CLOSE CGD010
           EXIT PROGRAM.
