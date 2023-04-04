       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO25.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 31/01/2004.
      *FUNÇÃO: GERA O ARQUIVO MTD022

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY MTPX019.

           SELECT MTD999 ASSIGN TO PATH-MTD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MT99
                  ALTERNATE RECORD KEY IS ALT-MT99 =
                            DATAMOV-MT99, ALBUM-MT99
                  ALTERNATE RECORD KEY IS CIDADE-MT99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-MT99 = CONTRATO-MT99
                            CURSO-MT99 NOME-FORM-MT99 WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY MTPW019.

       FD  MTD999.
       01  REG-MTD999.
           05  ALBUMMT99.
               10  CONTRATO-MT99   PIC 9(4).
               10  SEQ-MT99        PIC 9(4).
           05  ALBUM-MT99 REDEFINES ALBUMMT99 PIC 9(8).
           05  DATAMOV-MT99        PIC 9(8).
           05  NOME-FORM-MT99      PIC X(30).
           05  CIDADE-MT99         PIC 9(04).
           05  CURSO-MT99          PIC 9(03).
           05  UF-MT99             PIC XX.
           05  FONE-MT99           PIC 9(8).
           05  IDENTIFICADO-MT99   PIC 9.
           05  TURMA-MT99          PIC X(03).
           05  TURNO-MT99          PIC X(10).
           05  FILLER              PIC X(30).
      * identificado = 0(nao)  1(sim)

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD999             PIC XX       VALUE SPACES.
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
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD999.
           OPEN I-O   MTD019
           CLOSE      MTD019

           OPEN I-O   MTD019
           OPEN INPUT MTD999

           CLOSE CONTROLE.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.

           INITIALIZE REG-MTD999

           START MTD999 KEY IS NOT LESS ALBUM-MT99 INVALID KEY
                 MOVE "10" TO ST-MTD999.


           PERFORM UNTIL ST-MTD999 = "10"
             READ MTD999 NEXT RECORD AT END MOVE "10" TO ST-MTD999
              NOT AT END
                   MOVE REG-MTD999 TO REG-MTD019
                   DISPLAY REG-MTD019
                   WRITE REG-MTD019
                   END-WRITE
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE MTD019
           CLOSE MTD999
           EXIT PROGRAM.
