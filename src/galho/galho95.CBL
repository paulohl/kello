       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO95.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 06/06/2009.
      *FUNÇÃO: ALTERA EXTRUTURA DO LOG DO CAIXA (ACRESCENTA OUTROS DADOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY LOGCAIXA.SEL.


           SELECT LOGCAIXA2 ASSIGN       TO ARQUIVO-LOGCAIXA2
                            ORGANIZATION IS           INDEXED
                            ACCESS MODE  IS           DYNAMIC
                            RECORD KEY   IS   CHAVE-LOGCAIXA2
                            ALTERNATE    RECORD           KEY
                            IS CHAVE-CX100-LOGCAIXA2 =
                               DATA-MOV-LOGCAIXA2
                               SEQ-LOGCAIXA2
                               DATA-LANCAMENTO-LOGCAIXA2
                               HORA-LANCAMENTO-LOGCAIXA2
                               SEQUENCIA-LANCAMENTO-LOGCAIXA2
                            WITH                   DUPLICATES
                            LOCK   MODE  IS         AUTOMATIC
                            WITH   LOCK  ON            RECORD
                            FILE STATUS  IS      FS-LOGCAIXA2.

           COPY LOGCCD.SEL.

           SELECT LOGCCD2 ASSIGN       TO   PATH-LOGCCD2
                          ORGANIZATION IS        INDEXED
                          ACCESS MODE  IS        DYNAMIC
                          LOCK MODE    IS      AUTOMATIC
                          WITH LOCK    ON         RECORD
                          STATUS       IS      ST-LOGCCD2
                          RECORD KEY   IS CHAVE-LOGCCD2 =
                                           LOGCCD2-FORNEC
                                              LOGCCD2-SEQ.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY LOGCAIXA.FD.

       FD LOGCAIXA2.
       01 REG-LOGCAIXA2.
          05 CHAVE-LOGCAIXA2.
             10 DATA-LANCAMENTO-LOGCAIXA2        PIC 9(08).
             10 HORA-LANCAMENTO-LOGCAIXA2        PIC 9(04).
             10 SEQUENCIA-LANCAMENTO-LOGCAIXA2   PIC 9(04).
          05 REGISTRO-CX1002.
             10 SEQ-LOGCAIXA2                    PIC 9(04).
             10 DATA-MOV-LOGCAIXA2               PIC 9(08).
             10 TIPO-LCTO-LOGCAIXA2              PIC 99.
      * Tipo-lcto < 50 - saídas     e tipo-lcto >= 50 - entradas
             10 HISTORICO-LOGCAIXA2              PIC X(30).
             10 DOCUMENTO-LOGCAIXA2              PIC X(10).
             10 VALOR-LOGCAIXA2                  PIC 9(10)V99.
             10 CONTAPART-LOGCAIXA2              PIC 9(06).
             10 CONTA-REDUZ-LOGCAIXA2            PIC 9(03).
             10 CONTABIL-LOGCAIXA2               PIC 9.
      * será usado no futuro - para informar se o lancto é contábil
             10 RESPONSAVEL-LOGCAIXA2            PIC X(5).
             10 SEQ-DESM-LOGCAIXA2               PIC 9(4).
      * operacao-logcaixa =
      *               "I" = Inclusão
      *               "T" = Exclusão de Registro c/ Problema na data
      *               "E" = Exclusão
          05  USUARIO-LOGCAIXA2                  PIC X(05).
          05  OPERACAO-LOGCAIXA2                 PIC X(01).

           COPY LOGCCD.FD.

       FD LOGCCD2.
       01 REG-LOGCCD2.
          05 LOGCCD2-FORNEC                 PIC 9(06).
          05 LOGCCD2-SEQ                    PIC 9(05).
          05 LOGCCD2-USUARIO                PIC X(05).
          05 LOGCCD2-DATA.
             10 LOGCCD2-ANO                 PIC 9(04).
             10 LOGCCD2-MES                 PIC 9(02).
             10 LOGCCD2-DIA                 PIC 9(02).
          05 LOGCCD2-HORA                   PIC 9(08).
          05 LOGCCD2-PROGRAMA               PIC X(10).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  FS-LOGCAIXA           PIC XX       VALUE SPACES.
           05  FS-LOGCAIXA2          PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  ST-LOGCCD2            PIC XX       VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC x.
           05  RESP-MSG              PIC x.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(8).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(13).
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
           OPEN INPUT CONTROLE
           READ       CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "LOGCAIX"          TO ARQ-REC.
           MOVE EMPRESA-REF        TO ARQUIVO-LOGCAIXA
           MOVE "LOGCAIX2"         TO ARQ-REC.
           MOVE EMPRESA-REF        TO ARQUIVO-LOGCAIXA2
           MOVE "LOGCCD"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-LOGCCD
           MOVE "LOGCCD2"          TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-LOGCCD2

           OPEN INPUT LOGCCD2
                      LOGCAIXA2

           OPEN I-O   LOGCCD
                      LOGCAIXA

           DISPLAY "VOU COMECAR " STOP " "

           INITIALIZE REG-LOGCAIXA2
           START LOGCAIXA2 KEY IS NOT LESS CHAVE-LOGCAIXA2 INVALID KEY
                MOVE "10" TO FS-LOGCAIXA2.

           PERFORM UNTIL FS-LOGCAIXA2 = "10"
                READ LOGCAIXA2 NEXT AT END
                     MOVE "10" TO FS-LOGCAIXA2
                NOT AT END
                     INITIALIZE REG-LOGCAIXA

                     MOVE DATA-LANCAMENTO-LOGCAIXA2
                       TO DATA-LANCAMENTO-LOGCAIXA
                     MOVE HORA-LANCAMENTO-LOGCAIXA2
                       TO HORA-LANCAMENTO-LOGCAIXA
                     MOVE SEQUENCIA-LANCAMENTO-LOGCAIXA2
                       TO SEQUENCIA-LANCAMENTO-LOGCAIXA
                     MOVE SEQ-LOGCAIXA2
                       TO SEQ-LOGCAIXA
                     MOVE DATA-MOV-LOGCAIXA2
                       TO DATA-MOV-LOGCAIXA
                     MOVE TIPO-LCTO-LOGCAIXA2
                       TO TIPO-LCTO-LOGCAIXA
                     MOVE HISTORICO-LOGCAIXA2
                       TO HISTORICO-LOGCAIXA
                     MOVE DOCUMENTO-LOGCAIXA2
                       TO DOCUMENTO-LOGCAIXA
                     MOVE VALOR-LOGCAIXA2
                       TO VALOR-LOGCAIXA
                     MOVE CONTAPART-LOGCAIXA2
                       TO CONTAPART-LOGCAIXA
                     MOVE CONTA-REDUZ-LOGCAIXA2
                       TO CONTA-REDUZ-LOGCAIXA
                     MOVE CONTABIL-LOGCAIXA2
                       TO CONTABIL-LOGCAIXA
                     MOVE RESPONSAVEL-LOGCAIXA2
                       TO RESPONSAVEL-LOGCAIXA
                     MOVE SEQ-DESM-LOGCAIXA2
                       TO SEQ-DESM-LOGCAIXA
                     MOVE USUARIO-LOGCAIXA2
                       TO USUARIO-LOGCAIXA
                     MOVE OPERACAO-LOGCAIXA2
                       TO OPERACAO-LOGCAIXA

                     DISPLAY "REG-LOGCAIXA = " REG-LOGCAIXA
                     WRITE REG-LOGCAIXA INVALID KEY
                          MOVE "Erro de Gravação...LOGCAIXA" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
                     END-WRITE
                END-READ
           END-PERFORM

           INITIALIZE REG-LOGCCD2
           START LOGCCD2 KEY IS NOT LESS CHAVE-LOGCCD2 INVALID KEY
                MOVE "10" TO ST-LOGCCD2.

           PERFORM UNTIL ST-LOGCCD2 = "10"
                READ LOGCCD2 NEXT AT END
                     MOVE "10" TO ST-LOGCCD2
                NOT AT END
                     INITIALIZE REG-LOGCCD

                     MOVE LOGCCD2-FORNEC    TO LOGCCD-FORNEC
                     MOVE LOGCCD2-SEQ       TO LOGCCD-SEQ
                     MOVE LOGCCD2-USUARIO   TO LOGCCD-USUARIO
                     MOVE LOGCCD2-ANO       TO LOGCCD-ANO
                     MOVE LOGCCD2-MES       TO LOGCCD-MES
                     MOVE LOGCCD2-DIA       TO LOGCCD-DIA
                     MOVE LOGCCD2-HORA      TO LOGCCD-HORA
                     MOVE LOGCCD2-PROGRAMA  TO LOGCCD-PROGRAMA

                     DISPLAY "REG-LOGCCD = " REG-LOGCCD
                     WRITE REG-LOGCCD INVALID KEY
                          MOVE "Erro de Gravação...LOGCCD" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM



           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE LOGCCD    LOGCCD2
           CLOSE LOGCAIXA  LOGCAIXA2
           EXIT PROGRAM
           STOP RUN.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

