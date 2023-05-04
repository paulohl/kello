       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO55.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09-08-2005.
      *FUNÇÃO: TESTA O LOGACESS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY LOGACESS.SEL.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.

           COPY LOGACESS.FD.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  contador              pic 9(18)    value zeros.
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
           05  sair                  pic x(01)    value spaces.

           COPY "PARAMETR".

       01 WS-DATA-SYS.
          10 WS-DATA-CPU.
             15 WS-ANO-CPU                PIC 9(04).
             15 WS-MES-CPU                PIC 9(02).
             15 WS-DIA-CPU                PIC 9(02).
          10 FILLER                    PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "LOGACESS"TO ARQ-REC.   MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           display arquivo-logacess stop " arquivo-logacess".
           display arquivo-logacess stop " arquivo-logacess".
           display arquivo-logacess stop " arquivo-logacess".
           OPEN I-O   LOGACESS
           CLOSE      LOGACESS
           OPEN I-O   LOGACESS
           CLOSE CONTROLE.
           IF FS-LOGACESS = "35"
              CLOSE LOGACESS      OPEN OUTPUT LOGACESS
              CLOSE LOGACESS      OPEN I-O    LOGACESS
           END-IF.

           DISPLAY "VOU COMECAR" STOP " "

           DISPLAY FS-LOGACESS STOP " FS-LOGACESS"
           DISPLAY "AQUI" STOP " "
           DISPLAY "AQUI" STOP " "

           initialize contador
           move "N" to sair

           perform until sair = "S" or contador > 99999999999999
               move function current-date to ws-data-sys

               move "ANDER"             to logacess-usuario
               move ws-data-cpu         to logacess-data
               accept ws-hora-sys from time
               move ws-hora-sys         to logacess-horas
               move 1                   to logacess-sequencia
               move "MENU"              to logacess-programa
               move "ABERTO"            to logacess-status
               move "10" to fs-logacess
               perform until fs-logacess = "00"
                  display reg-logacess
                  write reg-logacess invalid key
                      add 1 to logacess-sequencia
                  not invalid key
                      add 1 to contador
                      move "00" to fs-logacess
                  end-write
                  if fs-logacess = "48"
                     display "entrei no fs-logacess 48" stop " "
                     move "00" to fs-logacess
                     move "S" to sair
                  end-if
               end-perform

               move function current-date to ws-data-sys

               move "ANDER"             to logacess-usuario
               move ws-data-cpu         to logacess-data
               accept ws-hora-sys from time
               move ws-hora-sys         to logacess-horas
               move 1                   to logacess-sequencia
               move "MENU"              to logacess-programa
               move "FECHADO"           to logacess-status
               move "10" to fs-logacess
               perform until fs-logacess =  "00"
                  display reg-logacess
                  write reg-logacess invalid key
                      add 1 to logacess-sequencia
                  not invalid key
                      add 1 to contador
                      move "00" to fs-logacess
                  end-write
                  if fs-logacess = "48"
                     display "entrei no fs-logacess 48" stop " "
                     move "00" to fs-logacess
                     move "S" to sair
                  end-if
               end-perform

           end-perform

           close logacess

           display "contador = " contador stop " "

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE LOGACESS
           EXIT PROGRAM
           STOP RUN.
