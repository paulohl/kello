       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO93.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 06/06/2009.
      *FUNÇÃO: COMPARAÇÃO DE DADOS CCD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CCPX100.

           SELECT CCD100ant ASSIGN TO PATH-CCD100ant
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CCD100ant
                  RECORD KEY IS CHAVE-CC100ant = FORNEC-CC100ant
                                                 SEQ-CC100ant
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CC100ant
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CC100ant =
                       SITUACAO-CC100ant
                       FORNEC-CC100ant
                       DATA-VENCTO-CC100ant
                       WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CC100ant =
                                          DATA-MOVTO-CC100ant
                                   SEQ-CAIXA-CC100ant
                       WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CC100ant =
                       SITUACAO-CC100ant
                       DATA-VENCTO-CC100ant
                       FORNEC-CC100ant WITH DUPLICATES.

           SELECT CCD100sem ASSIGN TO PATH-CCD100sem
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CCD100sem
                  RECORD KEY IS CHAVE-CC100sem = FORNEC-CC100sem
                                                 SEQ-CC100sem
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CC100sem
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CC100sem =
                       SITUACAO-CC100sem
                       FORNEC-CC100sem
                       DATA-VENCTO-CC100sem
                       WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CC100sem =
                                          DATA-MOVTO-CC100sem
                                   SEQ-CAIXA-CC100sem
                       WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CC100sem =
                       SITUACAO-CC100sem
                       DATA-VENCTO-CC100sem
                       FORNEC-CC100sem WITH DUPLICATES.

       COPY LOGCCD.SEL.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CCPW100.

       FD  CCD100ant.
       01  REG-CCD100ant.
           05  DATA-MOVTO-CC100ant         PIC 9(8).
           05  FORNEC-CC100ant             PIC 9(6).
           05  SEQ-CC100ant                PIC 9(5).
           05  TIPO-LCTO-CC100ant          PIC 99.
      *    TIPO-LCTO-CC100 = 00-FORNEC  01-FUNCION  02-VENDEDOR
      *                                 03-REPRES   04-REPORT
           05  TIPO-FORN-CC100ant          PIC 9(2).
           05  NR-DOCTO-CC100ant           PIC X(10).
           05  DATA-EMISSAO-CC100ant       PIC 9(8).
           05  DATA-VENCTO-CC100ant        PIC 9(8).
           05  DESCRICAO-CC100ant          PIC X(30).
           05  SITUACAO-CC100ant           PIC 9.
      *    SITUACAO =   00-OK   01-PAGA  02-ESTORNADA
           05  LIBERADO-CC100ant           PIC 9.
      *    LIBERADO =   00-NÃO  01-SIM
           05  TIPO-MOEDA-CC100ant         PIC 9.
      *    TIPO-MOEDA = 00-REAL 01-DOLAR
           05  NR-PARCELA-CC100ant.
               10  NR-PARC-CC100ant        PIC 99.
               10  TOT-PARC-CC100ant       PIC 99.
           05  CODREDUZ-APUR-CC100ant      PIC 9(5).
           05  VALOR-CC100ant              PIC 9(8)V99.
           05  CRED-DEB-CC100ant           PIC 9.
      *    0-DEBITO   1-CREDITO
           05  DATA-PGTO-CC100ant          PIC 9(8).
           05  JUROS-PAGO-CC100ant         PIC 9(6)V99.
           05  MULTA-PAGA-CC100ant         PIC 9(6)V99.
           05  DESCONTO-CC100ant           PIC 9(6)V99.
           05  VALOR-PAGO-CC100ant         PIC 9(8)V99.
           05  RESPONSAVEL-CC100ant        PIC X(5).
           05  DIGITADOR-CC100ant          PIC X(5).
           05  SEQ-CAIXA-CC100ant          PIC 9(3).

       FD  CCD100sem.
       01  REG-CCD100sem.
           05  DATA-MOVTO-CC100sem         PIC 9(8).
           05  FORNEC-CC100sem             PIC 9(6).
           05  SEQ-CC100sem                PIC 9(5).
           05  TIPO-LCTO-CC100sem          PIC 99.
      *    TIPO-LCTO-CC100 = 00-FORNEC  01-FUNCION  02-VENDEDOR
      *                                 03-REPRES   04-REPORT
           05  TIPO-FORN-CC100sem          PIC 9(2).
           05  NR-DOCTO-CC100sem           PIC X(10).
           05  DATA-EMISSAO-CC100sem       PIC 9(8).
           05  DATA-VENCTO-CC100sem        PIC 9(8).
           05  DESCRICAO-CC100sem          PIC X(30).
           05  SITUACAO-CC100sem           PIC 9.
      *    SITUACAO =   00-OK   01-PAGA  02-ESTORNADA
           05  LIBERADO-CC100sem           PIC 9.
      *    LIBERADO =   00-NÃO  01-SIM
           05  TIPO-MOEDA-CC100sem         PIC 9.
      *    TIPO-MOEDA = 00-REAL 01-DOLAR
           05  NR-PARCELA-CC100sem.
               10  NR-PARC-CC100sem        PIC 99.
               10  TOT-PARC-CC100sem       PIC 99.
           05  CODREDUZ-APUR-CC100sem      PIC 9(5).
           05  VALOR-CC100sem              PIC 9(8)V99.
           05  CRED-DEB-CC100sem           PIC 9.
      *    0-DEBITO   1-CREDITO
           05  DATA-PGTO-CC100sem          PIC 9(8).
           05  JUROS-PAGO-CC100sem         PIC 9(6)V99.
           05  MULTA-PAGA-CC100sem         PIC 9(6)V99.
           05  DESCONTO-CC100sem           PIC 9(6)V99.
           05  VALOR-PAGO-CC100sem         PIC 9(8)V99.
           05  RESPONSAVEL-CC100sem        PIC X(5).
           05  DIGITADOR-CC100sem          PIC X(5).
           05  SEQ-CAIXA-CC100sem          PIC 9(3).

       COPY LOGCCD.FD.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD100ant          PIC XX       VALUE SPACES.
           05  ST-CCD100sem          PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
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
      *    MOVE ZEROS TO PAG-W ERRO-W.
      *    OPEN INPUT CONTROLE
      *    READ CONTROLE
      *    MOVE EMPRESA          TO EMP-REC
           MOVE "f:\999\CCD100"  TO PATH-CCD100
           MOVE "f:\999\CCD199"  TO PATH-CCD100ant
           MOVE "f:\999\CCD198"  TO PATH-CCD100sem
           MOVE "f:\999\LOGCCD"  TO PATH-LOGCCD
           OPEN I-O   CCD100ANT
           CLOSE      CCD100ANT
           OPEN INPUT CCD100ANT

           OPEN INPUT LOGCCD

           OPEN I-O   CCD100
           CLOSE      CCD100
           OPEN INPUT CCD100

           OPEN I-O   CCD100sem
           CLOSE      CCD100sem
           OPEN I-O   CCD100sem

      *    CLOSE CONTROLE.
           IF ST-CCD100 = "35"
              CLOSE CCD100      OPEN OUTPUT CCD100
              CLOSE CCD100      OPEN I-O    CCD100
           END-IF.

      *    display "Vou comecar a comparacao" stop " ".
           display "Vou comecar a comparacao" stop " ".

      *    display "st-ccd100 = " st-ccd100 stop " "
      *    display "st-ccd100ant = " st-ccd100ant stop " "
      *    display "st-logccd = " st-logccd stop " "

           IF ST-CCD100 <> "00" OR ST-CCD100ANT <> "00" OR ST-LOGCCD
              CLOSE CCD100
                    CCD100ant
              stop run.


           INITIALIZE REG-CCD100ant

           move 3395 to fornec-cc100
           move 86   to seq-cc100
           read ccd100 invalid key
                display "nao achei" stop " "
           not invalid key
                display "achei " stop " "
                display "DATA-MOVTO-CC100 = " DATA-MOVTO-CC100 stop " "
                display "VALOR-CC100 = " VALOR-CC100 stop " "
                display "SITUACAO-CC100 = " SITUACAO-CC100 stop " "

                MOVE FORNEC-CC100    TO LOGCCD-FORNEC
                MOVE SEQ-CC100       TO LOGCCD-SEQ
                READ LOGCCD INVALID KEY
                     DISPLAY "Nao Achei o Log" STOP " "
                NOT INVALID KEY
                     DISPLAY "Usuario = " LOGCCD-USUARIO
                             " DATA = " LOGCCD-DATA
                             " LOGCCD-HORA = " LOGCCD-HORA
                             " LOGCCD-PROGRAMA = "
                             LOGCCD-PROGRAMA STOP " "
                END-READ
           end-read


      *    START CCD100ant KEY IS NOT LESS CHAVE-CC100ant INVALID KEY
      *          MOVE "10" TO ST-CCD100ANT.
      *
      *
      *    PERFORM UNTIL ST-CCD100ANT = "10"
      *      READ CCD100ant NEXT RECORD AT END
      *           MOVE "10" TO ST-CCD100ANT
      *      NOT AT END
      *           display reg-ccd100ant
      *           MOVE FORNEC-CC100ant TO FORNEC-CC100
      *           MOVE SEQ-CC100ant    TO SEQ-CC100
      *           READ CCD100 INVALID KEY
      *                DISPLAY "Nao achei FORNEC-CC100ant = "
      *                        FORNEC-CC100ant " SEQ-CC100ant = "
      *                        SEQ-CC100ant " DATA-MOVTO-CC100 = "
      *                        DATA-MOVTO-CC100 STOP " "
      *                MOVE FORNEC-CC100ant TO LOGCCD-FORNEC
      *                MOVE SEQ-CC100ant    TO LOGCCD-SEQ
      *                READ LOGCCD INVALID KEY
      *                     DISPLAY "Nao Achei o Log" STOP " "
      *                     MOVE REG-CCD100ANT TO REG-CCD100SEM
      *                     WRITE REG-CCD100SEM INVALID KEY
      *                           DISPLAY "Erro de gravacao CCD100SEM"
      *                              STOP " "
      *                     END-WRITE
      *                NOT INVALID KEY
      *                     DISPLAY "Usuario = " LOGCCD-USUARIO
      *                             " DATA = " LOGCCD-DATA
      *                             " LOGCCD-HORA = " LOGCCD-HORA
      *                             " LOGCCD-PROGRAMA = "
      *                             LOGCCD-PROGRAMA STOP " "
      *                END-READ
      *           END-READ
      *       END-READ
      *    END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CCD100
           CLOSE CCD100ant
           CLOSE CCD100sem
           CLOSE LOGCCD
           EXIT PROGRAM
           STOP RUN.
