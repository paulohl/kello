       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO83.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 23-05-2007
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY LBPX105.

           SELECT LBD1051 ASSIGN TO PATH-LBD1051
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-LBD1051
                  RECORD KEY IS CHAVE-L1051 = DATA-MOVTO-L1051 SEQ-L1051
                  ALTERNATE RECORD KEY IS ALT1-L1051 = FUNCIONARIO-L1051
                            DATA-MOVTO-L1051 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-L1051 = OPERACAO-L1051
                            DATA-MOVTO-L1051 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-L1051 = TIPO-INTERR-L1051
                            DATA-MOVTO-L1051 WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY LBPW105.

       FD  LBD1051.
       01  REG-LBD1051.
           05  DATA-MOVTO-L1051           PIC 9(8).
           05  SEQ-L1051                  PIC 9(3).
           05  FUNCIONARIO-L1051          PIC 9(6).
           05  TURNO-L1051                PIC 9.
           05  OPERACAO-L1051             PIC 999.
           05  QTDE-OPER-L1051            PIC 9(5).
           05  QT-ALBUM-L1051             PIC 9(3).
           05  HORA-INIC-L1051            PIC 9(4).
           05  HORA-FIM-L1051             PIC 9(4).
           05  TEMPO-INTERRUPC-L1051      PIC 9(4).
           05  TIPO-INTERR-L1051          PIC 99.
           05  TEMPO-INTERVALO-L1051      PIC 9(4).
           05  DIGITADOR-L1051            PIC X(4).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ST-LBD1051            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "LBD105"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD105
           MOVE "LBD1051" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD1051
           MOVE ZEROS     TO ERRO-W.
           OPEN I-O LBD105
                    LBD1051

           CLOSE CONTROLE.

           IF ST-LBD105 = "35"
              CLOSE LBD105      OPEN OUTPUT LBD105
              CLOSE LBD105      OPEN I-O    LBD105
           END-IF.
           IF ST-LBD1051 = "35"
              CLOSE LBD1051     OPEN OUTPUT LBD1051
              CLOSE LBD1051     OPEN I-O    LBD1051
           END-IF.

          DISPLAY "VOU COMECAR A CONVERSAO DOS ARQUIVOS LBD105"
            STOP " ".

           INITIALIZE REG-LBD1051
           START LBD1051 KEY IS NOT LESS CHAVE-L1051 INVALID KEY
               MOVE "10" TO ST-LBD1051.

           PERFORM UNTIL ST-LBD1051 = "10"
               READ LBD1051 NEXT AT END
                   MOVE "10" TO ST-LBD1051
               NOT AT END
                   MOVE REG-LBD1051 TO REG-LBD105
                   READ LBD105 INVALID KEY
                       MOVE REG-LBD1051 TO REG-LBD105
                       display REG-LBD105
                       WRITE REG-LBD105 INVALID KEY
                          DISPLAY "Erro de Gravacao...LBD105" STOP " "
                       END-WRITE
                   END-READ
               END-READ
           END-PERFORM


           CLOSE LBD105 LBD1051
           STOP RUN.
