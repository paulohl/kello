       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOVIP100-2.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 03/11/2009.
      *FUNÇÃO: MUDAR O CAMPO NR-FITA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY VIPX100.

           SELECT VID100A ASSIGN TO PATH-VID100A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID100A
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVEA-V100 = DATA-MOVTO-V100A SEQ-V100A
                  ALTERNATE RECORD KEY IS ALT-V100A = NR-FITAS-V100A
                           DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-V100A = CONTRATO-V100A
                            DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-V100A =
                                                      CINEGRAFISTA-V100A
                            DATA-EVENTO-V100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-V100A = DATA-EVENTO-V100A
                            DATA-MOVTO-V100A SEQ-V100A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY VIPW100.

      *MOVIMENTO E RECEBIMENTO DE FITAS BRUTAS
       FD  VID100A.
       01  REG-VID100A.
           05  DATA-MOVTO-V100A           PIC 9(8).
           05  SEQ-V100A                  PIC 9(3).
           05  NR-FITAS-V100A.
               10 CONTRATO-V100A          PIC 9(4).
               10 NR-FITA-V100A           PIC 9(5).
           05  CURSO-V100A                PIC X(10).
           05  EVENTO-V100A               PIC 999.
           05  DATA-EVENTO-V100A          PIC 9(8).
           05  CINEGRAFISTA-V100A         PIC 9(6).
           05  FILMADORA-V100A            PIC 9(2).
           05  LOCALIZACAO-V100A          PIC X(5).
           05  DIGITADOR-V100A            PIC X(4).
           05  IDENTIFICADOR-V100A        PIC 9(09).
           05  FILLER                     PIC X(91).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID100A            PIC XX       VALUE SPACES.
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
           MOVE "VID100"   TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100
           MOVE "VID100A"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100A

           OPEN I-O VID100 VID100A
           CLOSE    VID100 VID100A
           OPEN I-O VID100 VID100A

           CLOSE    CONTROLE
           IF ST-VID100 = "35"
              CLOSE VID100      OPEN OUTPUT  VID100
              CLOSE VID100      OPEN I-O     VID100
           END-IF.
           IF ST-VID100A = "35"
              CLOSE VID100A     OPEN OUTPUT  VID100A
              CLOSE VID100A     OPEN I-O     VID100A
           END-IF.

           display "vou comecar" stop " "

           INITIALIZE REG-VID100A
           START VID100A KEY IS NOT LESS CHAVEA-V100 INVALID KEY
                 MOVE "10" TO ST-VID100A.

           PERFORM UNTIL ST-VID100A = "10"
                 READ VID100A NEXT RECORD AT END
                      MOVE "10" TO ST-VID100A
                 NOT AT END
                      MOVE REG-VID100A TO REG-VID100
                      DISPLAY REG-VID100
                      WRITE REG-VID100
                 END-READ
           END-PERFORM.

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE VID100 VID100A

           EXIT PROGRAM
           STOP RUN.
