       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO63.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 15-05-2006.
      *FUNÇÃO: GERA UM NOVO ARQUIVO LBD105

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY LBPX105.

          SELECT LBD999 ASSIGN TO PATH-LBD999
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-LBD999.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY LBPW105.

       FD  LBD999.
       01  REG-LBD999.
           05  FILLER-CR999              PIC X(02).
           05  DATA-MOVTO-L999           PIC 9(8).
           05  SEQ-L999                  PIC 9(3).
           05  FUNCIONARIO-L999          PIC 9(6).
           05  TURNO-L999                PIC 9.
           05  OPERACAO-L999             PIC 999.
           05  QTDE-OPER-L999            PIC 9(5).
           05  QT-ALBUM-L999             PIC 9(3).
           05  HORA-INIC-L999            PIC 9(4).
           05  HORA-FIM-L999             PIC 9(4).
           05  TEMPO-INTERRUPC-L999      PIC 9(4).
           05  TIPO-INTERR-L999          PIC 99.
           05  TEMPO-INTERVALO-L999      PIC 9(4).
           05  DIGITADOR-L999            PIC X(4).
           05  FILLER2-CR999             PIC X(03).


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
      *    05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-LBD105             PIC XX       VALUE SPACES.
           05  ST-LBD999             PIC XX       VALUE SPACES.
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


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
      *    MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "LBD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD999
           MOVE "LBD105"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD105
           OPEN I-O   LBD105
           CLOSE      LBD105
           OPEN I-O   LBD105
           OPEN INPUT LBD999
           CLOSE CONTROLE.
      *    IF ST-RCD100 = "35"
      *       CLOSE RCD100      OPEN OUTPUT RCD100
      *       CLOSE RCD100      OPEN I-O RCD100
      *    END-IF.
           IF ST-LBD105 = "35"
              CLOSE LBD105      OPEN OUTPUT LBD105
              CLOSE LBD105      OPEN I-O    LBD105
           END-IF.

           DISPLAY "VOU COMECAR" STOP " "

           initialize reg-lbd999

           MOVE "00" TO ST-LBD999

           PERFORM UNTIL ST-LBD999 = "10"
               READ LBD999 NEXT AT END
                   MOVE "10" TO ST-LBD999
               NOT AT END
                   IF FILLER-CR999 = "@3"
                      MOVE DATA-MOVTO-L999      TO DATA-MOVTO-L105
                      MOVE SEQ-L999             TO SEQ-L105
                      MOVE FUNCIONARIO-L999     TO FUNCIONARIO-L105
                      MOVE TURNO-L999           TO TURNO-L105
                      MOVE OPERACAO-L999        TO OPERACAO-L105
                      MOVE QTDE-OPER-L999       TO QTDE-OPER-L105
                      MOVE QT-ALBUM-L999        TO QT-ALBUM-L105
                      MOVE HORA-INIC-L999       TO HORA-INIC-L105
                      MOVE HORA-FIM-L999        TO HORA-FIM-L105
                      MOVE TEMPO-INTERRUPC-L999 TO TEMPO-INTERRUPC-L105
                      MOVE TIPO-INTERR-L999     TO TIPO-INTERR-L105
                      MOVE TEMPO-INTERVALO-L999 TO TEMPO-INTERVALO-L105
                      MOVE DIGITADOR-L999       TO DIGITADOR-L105
                      DISPLAY REG-LBD105
                      WRITE REG-LBD105 INVALID KEY
                            DISPLAY "ERRO DE GRAVACAO" STOP " "
                      END-WRITE
                   ELSE
                      DISPLAY "ULTIMO = " REG-LBD105 STOP  " "
                      DISPLAY "PROXIMO = " REG-LBD999 STOP " "
                   END-IF
               END-READ
           END-PERFORM

      *    INITIALIZE REG-RCD100
      *
      *    MOVE ZEROS TO PAG-W.
      *
      *    START RCD100 KEY IS NOT LESS ALBUM-REC INVALID KEY
      *          MOVE "10" TO ST-RCD100.
      *
      *
      *    PERFORM UNTIL ST-RCD100 = "10"
      *      READ RCD100 NEXT RECORD AT END
      *           MOVE "10" TO ST-RCD100
      *      NOT AT END
      *           MOVE ZEROS  TO QFOTOS-EXTRA1-REC
      *           MOVE ZEROS  TO QFOTOS-EXTRA2-REC
      *           MOVE ZEROS  TO POSTER-ABERTURA-REC
      *           MOVE ZEROS  TO POSTER-XEROX-REC
      *           MOVE SPACES TO EXTRA-ENVIADO1-REC
      *           MOVE SPACES TO EXTRA-ENVIADO2-REC
      *
      *           DISPLAY REG-RCD100
      *           REWRITE REG-RCD100
      *       END-READ
      *    END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE LBD105 LBD999
           EXIT PROGRAM
           STOP RUN.
