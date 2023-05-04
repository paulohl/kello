       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOVIP100.
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

           SELECT VID100a ASSIGN TO PATH-VID100a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID100a
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V100a = DATA-MOVTO-V100a SEQ-V100a
                  ALTERNATE RECORD KEY IS ALT-V100a = NR-FITA-V100a
                           DATA-EVENTO-V100a WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-V100a = CONTRATO-V100a
                            DATA-EVENTO-V100a WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-V100a =
                                    CINEGRAFISTA-V100a
                                    DATA-EVENTO-V100a WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-V100a = DATA-EVENTO-V100a
                            DATA-MOVTO-V100a SEQ-V100a.

           COPY VIPX101.

           SELECT VID101a ASSIGN TO PATH-VID101a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID101a
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS NR-FITA-V101a.

           COPY VIPX102.

           SELECT VID102a ASSIGN TO PATH-VID102a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID102a
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V102a = NR-FITA-V102a SEQ-V102a.

           COPY VIPX103.

           SELECT VID103a ASSIGN TO PATH-VID103a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID103a
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V103a = NR-FITA-V103a SEQ-V103a.

           COPY VIPX130.

           SELECT VID130a ASSIGN TO PATH-VID130a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-VID130a
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-V130a = DATA-V130a NR-FITA-V130a.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY VIPW100.

      *MOVIMENTO E RECEBIMENTO DE FITAS BRUTAS
       FD  VID100a.
       01  REG-VID100a.
           05  DATA-MOVTO-V100a           PIC 9(8).
           05  SEQ-V100a                  PIC 9(3).
           05  NR-FITA-V100a              PIC 9(5).
           05  CONTRATO-V100a             PIC 9(4).
           05  CURSO-V100a                PIC X(10).
           05  EVENTO-V100a               PIC 999.
           05  DATA-EVENTO-V100a          PIC 9(8).
           05  CINEGRAFISTA-V100a         PIC 9(6).
           05  FILMADORA-V100a            PIC 9(2).
           05  LOCALIZACAO-V100a          PIC X(5).
           05  DIGITADOR-V100a            PIC X(4).

           COPY VIPW101.

       FD  VID101a.
       01  REG-VID101a.
           05  NR-FITA-V101a              PIC 9(5).
           05  REVISOR-V101a              PIC 9(6).
           05  DATA-REVISAO-V101a         PIC 9(8).
           05  AVALIACAO-GERAL-V101a      PIC 9.
      *    AVALIACAO-GERAL = 1-PESSIMA  2-RUIM  3-REGULAR 4-BOA  5-OTIMA

           COPY VIPW102.

      * ARQUIVO DE PROBLEMA E MOMENTOS DETECTADOS NA AVALIACAO DE CINEG.
       FD  VID102a.
       01  REG-VID102a.
           05  NR-FITA-V102a              PIC 9(5).
           05  SEQ-V102a                  PIC 9(3).
           05  MOMENTO-V102a              PIC 9(3).
           05  PROBLEMA-V102a             PIC 9(3).
           05  TEMPO-V102a                PIC 9(7).
      **** TEMPO: 999.99.99 (HORA/MIN/SEG) *****
           05  DIGITADOR-V102a            PIC X(4).

           COPY VIPW103.

       FD  VID103a.
       01  REG-VID103a.
           05  NR-FITA-V103a             PIC 9(5).
           05  SEQ-V103a                 PIC 9(2).
           05  OBS-V103a                 PIC X(100).

           COPY VIPW130.

       FD  VID130a.
       01  REG-VID130a.
           05  DATA-V130a                PIC 9(8).
           05  NR-FITA-V130a             PIC 9(5).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID100a            PIC XX       VALUE SPACES.
           05  ST-VID101             PIC XX       VALUE SPACES.
           05  ST-VID101a            PIC XX       VALUE SPACES.
           05  ST-VID102             PIC XX       VALUE SPACES.
           05  ST-VID102a            PIC XX       VALUE SPACES.
           05  ST-VID103             PIC XX       VALUE SPACES.
           05  ST-VID103a            PIC XX       VALUE SPACES.
           05  ST-VID130             PIC XX       VALUE SPACES.
           05  ST-VID130a            PIC XX       VALUE SPACES.
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
           MOVE "VID100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100
           MOVE "VID100A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100A
           MOVE "VID101"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID101
           MOVE "VID101A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID101A
           MOVE "VID102"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID102
           MOVE "VID102A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID102A
           MOVE "VID103"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID103
           MOVE "VID103A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID103A
           MOVE "VID130"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID130
           MOVE "VID130A" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID130A

           OPEN I-O VID100 VID100A VID101 VID101A VID102 VID102A
                    VID103 VID103A VID130 VID130A
           CLOSE    VID100 VID100A VID101 VID101A VID102 VID102A
                    VID103 VID103A VID130 VID130A
           OPEN I-O VID100 VID100A VID101 VID101A VID102 VID102A
                    VID103 VID103A VID130 VID130A

           CLOSE    CONTROLE
           IF ST-VID100 = "35"
              CLOSE VID100      OPEN OUTPUT  VID100
              CLOSE VID100      OPEN I-O     VID100
           END-IF.

           IF ST-VID100A = "35"
              CLOSE VID100A      OPEN OUTPUT VID100A
              CLOSE VID100A      OPEN I-O    VID100A
           END-IF.

           IF ST-VID101 = "35"
              CLOSE VID101      OPEN OUTPUT  VID101
              CLOSE VID101      OPEN I-O     VID101
           END-IF.

           IF ST-VID101A = "35"
              CLOSE VID101A      OPEN OUTPUT VID101A
              CLOSE VID101A      OPEN I-O    VID101A
           END-IF.

           IF ST-VID102 = "35"
              CLOSE VID102      OPEN OUTPUT  VID102
              CLOSE VID102      OPEN I-O     VID102
           END-IF.

           IF ST-VID102A = "35"
              CLOSE VID102A      OPEN OUTPUT VID102A
              CLOSE VID102A      OPEN I-O    VID102A
           END-IF.

           IF ST-VID103 = "35"
              CLOSE VID103      OPEN OUTPUT  VID103
              CLOSE VID103      OPEN I-O     VID103
           END-IF.

           IF ST-VID103A = "35"
              CLOSE VID103A      OPEN OUTPUT VID103A
              CLOSE VID103A      OPEN I-O    VID103A
           END-IF.

           IF ST-VID130 = "35"
              CLOSE VID130      OPEN OUTPUT  VID130
              CLOSE VID130      OPEN I-O     VID130
           END-IF.

           IF ST-VID130A = "35"
              CLOSE VID130A      OPEN OUTPUT VID130A
              CLOSE VID130A      OPEN I-O    VID130A
           END-IF.


           display "vou comecar" stop " "

           INITIALIZE REG-VID100A
           START VID100a KEY IS NOT LESS CHAVE-V100a INVALID KEY
                 MOVE "10" TO ST-VID100a.

           PERFORM UNTIL ST-VID100a = "10"
                 READ VID100a NEXT RECORD AT END
                      MOVE "10" TO ST-VID100a
                 NOT AT END
                      INITIALIZE REG-VID100

                      MOVE DATA-MOVTO-V100a   TO DATA-MOVTO-V100
                      MOVE SEQ-V100a          TO SEQ-V100
                      MOVE CONTRATO-V100a     TO CONTRATO-V100
                      MOVE NR-FITA-V100a      TO NR-FITA-V100
                      MOVE CURSO-V100a        TO CURSO-V100
                      MOVE EVENTO-V100a       TO EVENTO-V100
                      MOVE DATA-EVENTO-V100a  TO DATA-EVENTO-V100
                      MOVE CINEGRAFISTA-V100a TO CINEGRAFISTA-V100
                      MOVE FILMADORA-V100a    TO FILMADORA-V100
                      MOVE LOCALIZACAO-V100a  TO LOCALIZACAO-V100
                      MOVE DIGITADOR-V100a    TO DIGITADOR-V100

                      DISPLAY REG-VID100

                      WRITE REG-VID100
                      END-WRITE

                      INITIALIZE REG-VID101a
                      MOVE NR-FITA-V100a      TO NR-FITA-V101a
                      START VID101a KEY IS NOT LESS NR-FITA-V101a
                                                             INVALID KEY
                            MOVE "10" TO ST-VID101a
                      END-START
                      PERFORM UNTIL ST-VID101a = "10"
                            READ VID101a NEXT AT END
                                 MOVE "10" TO ST-VID101a
                            NOT AT END
                                 IF NR-FITA-V100a <> NR-FITA-V101a
                                    MOVE "10" TO ST-VID101a
                                 ELSE
                                    INITIALIZE REG-VID101

                                    MOVE CONTRATO-V100
                                      TO CONTRATO-V101
                                    MOVE NR-FITA-V101a
                                      TO NR-FITA-V101
                                    MOVE REVISOR-V101a
                                      TO REVISOR-V101
                                    MOVE DATA-REVISAO-V101a
                                      TO DATA-REVISAO-V101
                                    MOVE AVALIACAO-GERAL-V101a
                                      TO AVALIACAO-GERAL-V101

                                    DISPLAY REG-VID101

                                    WRITE REG-VID101
                                 END-IF
                            END-READ
                      END-PERFORM

                      INITIALIZE REG-VID102a
                      MOVE NR-FITA-V100a      TO NR-FITA-V102a
                      START VID102a KEY IS NOT LESS CHAVE-V102a
                                                             INVALID KEY
                            MOVE "10" TO ST-VID102a
                      END-START
                      PERFORM UNTIL ST-VID102a = "10"
                            READ VID102a NEXT AT END
                                 MOVE "10" TO ST-VID102a
                            NOT AT END
                                 IF NR-FITA-V100a <> NR-FITA-V102a
                                    MOVE "10" TO ST-VID102a
                                 ELSE
                                    INITIALIZE REG-VID102

                                    MOVE CONTRATO-V100
                                      TO CONTRATO-V102

                                    MOVE NR-FITA-V102A
                                      TO NR-FITA-V102
                                    MOVE SEQ-V102A
                                      TO SEQ-V102
                                    MOVE MOMENTO-V102A
                                      TO MOMENTO-V102
                                    MOVE PROBLEMA-V102A
                                      TO PROBLEMA-V102
                                    MOVE TEMPO-V102A
                                      TO TEMPO-V102
                                    MOVE DIGITADOR-V102A
                                      TO DIGITADOR-V102

                                    DISPLAY REG-VID102

                                    WRITE REG-VID102
                                 END-IF
                            END-READ
                      END-PERFORM

                      INITIALIZE REG-VID103a
                      MOVE NR-FITA-V100a      TO NR-FITA-V103a
                      START VID103a KEY IS NOT LESS CHAVE-V103a
                                                             INVALID KEY
                            MOVE "10" TO ST-VID103a
                      END-START
                      PERFORM UNTIL ST-VID103a = "10"
                            READ VID103a NEXT AT END
                                 MOVE "10" TO ST-VID103a
                            NOT AT END
                                 IF NR-FITA-V100a <> NR-FITA-V103a
                                    MOVE "10" TO ST-VID103a
                                 ELSE
                                    INITIALIZE REG-VID103

                                    MOVE CONTRATO-V100
                                      TO CONTRATO-V103

                                    MOVE NR-FITA-V103A
                                      TO NR-FITA-V103

                                    MOVE SEQ-V103A
                                      TO SEQ-V103
                                    MOVE OBS-V103A
                                      TO OBS-V103

                                    DISPLAY REG-VID103

                                    WRITE REG-VID103
                                 END-IF
                            END-READ
                      END-PERFORM

                      INITIALIZE REG-VID130a
                      START VID130a KEY IS NOT LESS CHAVE-V130a INVALID
                                                                    KEY
                            MOVE "10" TO ST-VID130a
                      END-START
                      PERFORM UNTIL ST-VID130a = "10"
                            READ VID130a NEXT AT END
                                 MOVE "10" TO ST-VID130a
                            NOT AT END
                                 IF NR-FITA-V130a = NR-FITA-V100a
                                    MOVE CONTRATO-V100 TO CONTRATO-V130
                                    MOVE NR-FITA-V130a TO NR-FITA-V130
                                    MOVE DATA-V130a    TO DATA-V130
                                    WRITE REG-VID130
                                 END-IF
                            END-READ
                      END-PERFORM

                 END-READ
           END-PERFORM.

           DISPLAY "ACABOU"
           DISPLAY "ACABOU" STOP "  ".

           CLOSE VID100 VID100a VID101 VID101a VID102 VID102a VID103
                 VID103a VID130 VID130a
           EXIT PROGRAM
           STOP RUN.
