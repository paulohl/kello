       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO53.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 28/07/2005.
      *FUNÇÃO: INICIALIZA OS NOVOS CAMPOS DO RCD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY CRPX200.

          SELECT CRD2003 ASSIGN TO PATH-CRD2003
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-CRD2003.

          SELECT CRD2005 ASSIGN TO PATH-CRD2005
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-CRD2005.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CRPW200.

       FD  CRD2003.
       01  REG-CRD2003.
           05  FILLER-CR2003        PIC X(01).

       FD  CRD2005.
       01  REG-CRD2005.
           05  FILLER-CR2005        PIC X(02).
           05  COD-COMPL-CR2005     PIC 9(9).
           05  SEQ-CR2005           PIC 9(3).
           05  DATA-MOVTO-CR2005    PIC 9(8).
           05  HORA-MOVTO-CR2005    PIC 9(4).
           05  DATA-RETORNO-CR2005  PIC 9(8).
           05  USUARIO-CR2005       PIC X(5).
           05  SITUACAO-ANOTACAO-CR2005 PIC 9.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
      *    05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD2003            PIC XX       VALUE SPACES.
           05  ST-CRD2005            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  IND                   PIC 9(2)     VALUE ZEROS.
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

       01 TAB-CRD2005                PIC X(48).
       01 TAB-CRD2005-R REDEFINES TAB-CRD2005 OCCURS 48 TIMES.
          05 TAB-POSICAO             PIC X(01).


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
      *    MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "CRD2003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD2003
           MOVE "CRD2005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD2005
           MOVE "CRD200"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200
           OPEN OUTPUT CRD2005
           OPEN I-O    CRD200
           CLOSE       CRD200
           OPEN I-O    CRD200
           OPEN INPUT  CRD2003
           CLOSE CONTROLE.
      *    IF ST-RCD100 = "35"
      *       CLOSE RCD100      OPEN OUTPUT RCD100
      *       CLOSE RCD100      OPEN I-O RCD100
      *    END-IF.
           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O CRD200
           END-IF.

           DISPLAY "VOU COMECAR" STOP " "
           DISPLAY "VOU COMECAR" STOP " "

           initialize reg-crd2003

           MOVE "00" TO ST-CRD2003

           PERFORM UNTIL ST-CRD2003 = "10"
               READ CRD2003 NEXT AT END
                   MOVE "10" TO ST-CRD2003
               NOT AT END
                   IF FILLER-CR2003 = "@"
                      MOVE SPACES TO TAB-CRD2005
                      MOVE 1 TO IND
                      MOVE FILLER-CR2003 TO TAB-POSICAO(IND)
                      PERFORM UNTIL IND = 40 OR ST-CRD2003 = "10"
                           READ CRD2003 NEXT AT END
                               MOVE "10" TO ST-CRD2003
                           NOT AT END
                               ADD 1              TO IND
                               MOVE FILLER-CR2003 TO TAB-POSICAO(IND)
                           END-READ
                      END-PERFORM
                      MOVE TAB-CRD2005            TO REG-CRD2005
                      DISPLAY REG-CRD2005
                      WRITE REG-CRD2005
                   END-IF
               END-READ
           END-PERFORM

           CLOSE CRD2005
           OPEN INPUT CRD2005


           initialize reg-crd2005

           MOVE "00" TO ST-CRD2005

           PERFORM UNTIL ST-CRD2005 = "10"
               READ CRD2005 NEXT AT END
                   MOVE "10" TO ST-CRD2005
               NOT AT END
                   IF USUARIO-CR2005 <> SPACES AND FILLER-CR2005 = "@&"
                      MOVE COD-COMPL-CR2005         TO COD-COMPL-CR200
                      MOVE SEQ-CR2005               TO SEQ-CR200
                      MOVE DATA-MOVTO-CR2005        TO DATA-MOVTO-CR200
                      MOVE HORA-MOVTO-CR2005        TO HORA-MOVTO-CR200
                      MOVE DATA-RETORNO-CR2005      TO
                           DATA-RETORNO-CR200
                      MOVE USUARIO-CR2005           TO USUARIO-CR200
                      MOVE SITUACAO-ANOTACAO-CR2005 TO
                           SITUACAO-ANOTACAO-CR200
                      DISPLAY REG-CRD200
                      WRITE REG-CRD200 INVALID KEY
                           DISPLAY "ERRO DE GRAVACAO" STOP " "
                      END-WRITE
                   ELSE
                   DISPLAY "ERRO NAO TA CERTO "
                   USUARIO-CR2005 " " FILLER-CR2005 STOP " "
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

           CLOSE CRD200 CRD2003 CRD2005
           EXIT PROGRAM
           STOP RUN.
