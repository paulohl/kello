       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGACESS.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 28-01-2005.
      *DESCRIÇÃO: CONSULTA ACESSOS DOS USUARIOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS DATA-CH-WK = PERIODO-WK
                                             USUARIO-WK
                                             STATUS-WK
                  ALTERNATE RECORD KEY IS USUARIO-CH-WK =
                                          USUARIO-WK
                                          PERIODO-WK
                                          STATUS-WK
                  ALTERNATE RECORD KEY IS HORARIO-CH-WK =
                                          HORARIO-WK
                                          DATA-WK
                                          USUARIO-WK
                                          STATUS-WK
                  ALTERNATE RECORD KEY IS STATUS-CH-WK =
                                          STATUS-WK
                                          PERIODO-WK
                                          USUARIO-WK
                                          PROGRAMA-WK
                  ALTERNATE RECORD KEY IS PROGRAMA-CH-WK =
                                          PROGRAMA-WK
                                          PERIODO-WK
                                          USUARIO-WK
                                          STATUS-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY LOGACESS.FD.

       FD WORK.
       01 REG-WORK.
           05  USUARIO-WK            PIC X(05).
           05  PERIODO-WK.
               10 DATA-WK.
                  15 ANO-WK          PIC 9(04).
                  15 MES-WK          PIC 9(02).
                  15 DIA-WK          PIC 9(02).
               10 HORARIO-WK.
                  15 HORA-WK         PIC 9(02).
                  15 MINU-WK         PIC 9(02).
                  15 SEGU-WK         PIC 9(02).
                  15 MILE-WK         PIC 9(02).
               10 SEQUENCIA-WK       PIC 9(02).
           05  PROGRAMA-WK           PIC X(10).
           05  STATUS-WK             PIC X(10).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LOGACESS.CPB".
           COPY "LOGACESS.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".

       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  TIPO-W                PIC X(13)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC x(01).
           05  RESP-MSG              PIC x(01).
           05  TIPO1-W               PIC 9(01)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       01 DATA-FIM.
          05 FIM-ANO                 PIC 9(04).
          05 FIM-MES                 PIC 9(02).
          05 FIM-DIA                 PIC 9(02).

       77 janelaPrincipal            object reference.
       77 handle8                    pic 9(08) comp-x value zeros.
       77 wHandle                    pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELACAO DOS PROGRAMAS ACESSADOS: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "NO INTERVALO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05 FILLER               PIC X(01).
           05 FILLER               PIC X(19) VALUE "USUARIO".
           05 FILLER               PIC X(16) VALUE "DATA".
           05 FILLER               PIC X(19) VALUE "HORARIO".
           05 FILLER               PIC X(09) VALUE "PROGRAMA".
           05 FILLER               PIC x(06) VALUE "STATUS".

      *LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       procedure division.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL EXIT-FLAG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT VARIA-W FROM TIME.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE VERSION-NO         TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   LOGACESS
           CLOSE      LOGACESS
           OPEN INPUT LOGACESS
           MOVE 1 TO GRAVA-W.
           IF FS-LOGACESS <> "00"
              MOVE "ERRO ABERTURA LOGACESS: "  TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              MOVE 1 TO ERRO-W.

           IF ERRO-W = ZEROS
               PERFORM LOAD-SCREENSET.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LOGACESS" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.


       CORPO-PROGRAMA SECTION.
           EVALUATE FLAG
               WHEN "CENTRALIZA" PERFORM CENTRALIAR
               WHEN "CONSULTAR"  PERFORM CONSULTAR
               WHEN "IMPRIMIR"   COPY IMPRESSORA.CHAMA.
                                 IF LNK-MAPEAMENTO <> SPACES
                                    PERFORM IMPRIMIR-LISTA
                                 END-IF
               WHEN "USUARIO"    PERFORM CONSULTAR-USUARIO
               WHEN "DATA"       PERFORM CONSULTAR-DATA
               WHEN "HORARIO"    PERFORM CONSULTAR-HORARIO
               WHEN "PROGRAMA"   PERFORM CONSULTAR-PROGRAMA
               WHEN "STATUS"     PERFORM CONSULTAR-STATUS
           END-EVALUATE
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CONSULTAR-USUARIO SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS USUARIO-CH-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   STRING USUARIO-WK     INTO LINDET(3:5)
                   STRING DIA-WK         INTO LINDET(18:2)
                   STRING "/"            INTO LINDET(20:1)
                   STRING MES-WK         INTO LINDET(21:2)
                   STRING "/"            INTO LINDET(23:1)
                   STRING ANO-WK         INTO LINDET(24:4)
                   STRING HORA-WK        INTO LINDET(35:2)
                   STRING ":"            INTO LINDET(37:1)
                   STRING MINU-WK        INTO LINDET(38:2)
                   STRING ":"            INTO LINDET(40:1)
                   STRING SEGU-WK        INTO LINDET(41:2)
                   STRING ":"            INTO LINDET(43:1)
                   STRING MILE-WK        INTO LINDET(44:2)
                   STRING PROGRAMA-WK    INTO LINDET(56:8)
                   STRING STATUS-WK      INTO LINDET(65:10)
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.
       CONSULTAR-DATA SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS DATA-CH-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   STRING USUARIO-WK     INTO LINDET(3:5)
                   STRING DIA-WK         INTO LINDET(18:2)
                   STRING "/"            INTO LINDET(20:1)
                   STRING MES-WK         INTO LINDET(21:2)
                   STRING "/"            INTO LINDET(23:1)
                   STRING ANO-WK         INTO LINDET(24:4)
                   STRING HORA-WK        INTO LINDET(35:2)
                   STRING ":"            INTO LINDET(37:1)
                   STRING MINU-WK        INTO LINDET(38:2)
                   STRING ":"            INTO LINDET(40:1)
                   STRING SEGU-WK        INTO LINDET(41:2)
                   STRING ":"            INTO LINDET(43:1)
                   STRING MILE-WK        INTO LINDET(44:2)
                   STRING PROGRAMA-WK    INTO LINDET(56:8)
                   STRING STATUS-WK      INTO LINDET(65:10)
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.

       CONSULTAR-HORARIO SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS HORARIO-CH-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   STRING USUARIO-WK     INTO LINDET(3:5)
                   STRING DIA-WK         INTO LINDET(18:2)
                   STRING "/"            INTO LINDET(20:1)
                   STRING MES-WK         INTO LINDET(21:2)
                   STRING "/"            INTO LINDET(23:1)
                   STRING ANO-WK         INTO LINDET(24:4)
                   STRING HORA-WK        INTO LINDET(35:2)
                   STRING ":"            INTO LINDET(37:1)
                   STRING MINU-WK        INTO LINDET(38:2)
                   STRING ":"            INTO LINDET(40:1)
                   STRING SEGU-WK        INTO LINDET(41:2)
                   STRING ":"            INTO LINDET(43:1)
                   STRING MILE-WK        INTO LINDET(44:2)
                   STRING PROGRAMA-WK    INTO LINDET(56:8)
                   STRING STATUS-WK      INTO LINDET(65:10)
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.

       CONSULTAR-PROGRAMA SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS PROGRAMA-CH-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   STRING USUARIO-WK     INTO LINDET(3:5)
                   STRING DIA-WK         INTO LINDET(18:2)
                   STRING "/"            INTO LINDET(20:1)
                   STRING MES-WK         INTO LINDET(21:2)
                   STRING "/"            INTO LINDET(23:1)
                   STRING ANO-WK         INTO LINDET(24:4)
                   STRING HORA-WK        INTO LINDET(35:2)
                   STRING ":"            INTO LINDET(37:1)
                   STRING MINU-WK        INTO LINDET(38:2)
                   STRING ":"            INTO LINDET(40:1)
                   STRING SEGU-WK        INTO LINDET(41:2)
                   STRING ":"            INTO LINDET(43:1)
                   STRING MILE-WK        INTO LINDET(44:2)
                   STRING PROGRAMA-WK    INTO LINDET(56:8)
                   STRING STATUS-WK      INTO LINDET(65:10)
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.

       CONSULTAR-STATUS SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS STATUS-CH-WK INVALID KEY
               MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   STRING USUARIO-WK     INTO LINDET(3:5)
                   STRING DIA-WK         INTO LINDET(18:2)
                   STRING "/"            INTO LINDET(20:1)
                   STRING MES-WK         INTO LINDET(21:2)
                   STRING "/"            INTO LINDET(23:1)
                   STRING ANO-WK         INTO LINDET(24:4)
                   STRING HORA-WK        INTO LINDET(35:2)
                   STRING ":"            INTO LINDET(37:1)
                   STRING MINU-WK        INTO LINDET(38:2)
                   STRING ":"            INTO LINDET(40:1)
                   STRING SEGU-WK        INTO LINDET(41:2)
                   STRING ":"            INTO LINDET(43:1)
                   STRING MILE-WK        INTO LINDET(44:2)
                   STRING PROGRAMA-WK    INTO LINDET(56:8)
                   STRING STATUS-WK      INTO LINDET(65:10)
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.


       IMPRIMIR-LISTA SECTION.
           COPY "CBDATA1.CPY".
           move 01 to IMPRESSORA-W
           MOVE ACP-DTINI TO VENCTO-INI-REL
           MOVE ACP-DTFIM TO VENCTO-FIM-REL

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           MOVE SPACES TO LINDET
           MOVE 1      TO POSICAO
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINDET = SPACES
               MOVE LINDET TO REG-RELAT
               WRITE REG-RELAT AFTER 1

               ADD 1 TO LIN
               IF LIN > 56
                  PERFORM CABECALHO
               END-IF

               MOVE SPACES TO LINDET
               ADD 1 TO POSICAO
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           MOVE ORDEM TO ORDEM-REL
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.


       CONSULTAR SECTION.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           INITIALIZE TOTAL-ACESSOS REG-LOGACESS

           STRING ACP-DTFIM(1:2) INTO FIM-DIA
           STRING ACP-DTFIM(3:4) INTO FIM-MES
           STRING ACP-DTFIM(5:8) INTO FIM-ANO

           IF ACP-PROGRAMA = SPACES
               IF ACP-USUARIO <> SPACES
                  MOVE ACP-USUARIO TO LOGACESS-USUARIO
                  IF ACP-DTINI > 0
                     STRING ACP-DTINI(1:2) INTO LOGACESS-DIA
                     STRING ACP-DTINI(3:4) INTO LOGACESS-MES
                     STRING ACP-DTINI(5:8) INTO LOGACESS-ANO
                     START LOGACESS KEY IS NOT LESS LOGACESS-CHAVE
                                                             INVALID KEY
                           MOVE "10" TO FS-LOGACESS
                     END-START
                     PERFORM UNTIL FS-LOGACESS = "10"
                         READ LOGACESS NEXT RECORD AT END
                             MOVE "10" TO FS-LOGACESS
                         NOT AT END
                             IF ACP-USUARIO <> LOGACESS-USUARIO OR
                                LOGACESS-DATA > DATA-FIM
                                MOVE "10" TO FS-LOGACESS
                             ELSE
                                PERFORM MOVER-DADOS
                             END-IF
                         END-READ
                     END-PERFORM
                  ELSE
                     START LOGACESS KEY IS NOT LESS LOGACESS-CHAVE
                                                             INVALID KEY
                           MOVE "10" TO FS-LOGACESS
                     END-START
                     PERFORM UNTIL FS-LOGACESS = "10"
                         READ LOGACESS NEXT RECORD AT END
                             MOVE "10" TO FS-LOGACESS
                         NOT AT END
                             IF ACP-USUARIO <> LOGACESS-USUARIO
                                MOVE "10" TO FS-LOGACESS
                             ELSE
                                PERFORM MOVER-DADOS
                             END-IF
                         END-READ
                     END-PERFORM
                  END-IF
               ELSE
                  STRING ACP-DTINI(1:2) INTO LOGACESS-DIA
                  STRING ACP-DTINI(3:4) INTO LOGACESS-MES
                  STRING ACP-DTINI(5:8) INTO LOGACESS-ANO
                  START LOGACESS KEY IS NOT LESS LOGACESS-CH-DATA
                                                          INVALID KEY
                        MOVE "10" TO FS-LOGACESS
                  END-START
                  PERFORM UNTIL FS-LOGACESS = "10"
                      READ LOGACESS NEXT RECORD AT END
                          MOVE "10" TO FS-LOGACESS
                      NOT AT END
                          IF LOGACESS-DATA > DATA-FIM
                             MOVE "10" TO FS-LOGACESS
                          ELSE
                             IF ACP-PROGRAMA = SPACES OR ACP-PROGRAMA =
                                LOGACESS-PROGRAMA
                                PERFORM MOVER-DADOS
                             END-IF
                          END-IF
                      END-READ
                  END-PERFORM
               END-IF
           ELSE
               IF ACP-DTINI > 0
                  MOVE ACP-PROGRAMA TO LOGACESS-PROGRAMA
                  STRING ACP-DTINI(1:2) INTO LOGACESS-DIA
                  STRING ACP-DTINI(3:4) INTO LOGACESS-MES
                  STRING ACP-DTINI(5:8) INTO LOGACESS-ANO
                  START LOGACESS KEY IS NOT LESS LOGACESS-CH-PROGRAMA
                                                             INVALID KEY
                        MOVE "10" TO FS-LOGACESS
                  END-START
                  PERFORM UNTIL FS-LOGACESS = "10"
                      READ LOGACESS NEXT RECORD AT END
                          MOVE "10" TO FS-LOGACESS
                      NOT AT END
                          IF ACP-PROGRAMA <> LOGACESS-PROGRAMA OR
                             LOGACESS-DATA > DATA-FIM
                             MOVE "10" TO FS-LOGACESS
                          ELSE
                             IF ACP-USUARIO = SPACES OR ACP-USUARIO =
                                LOGACESS-USUARIO
                                PERFORM MOVER-DADOS
                             END-IF
                          END-IF
                      END-READ
                  END-PERFORM
               ELSE
                  MOVE ACP-PROGRAMA TO LOGACESS-PROGRAMA
                  START LOGACESS KEY IS NOT LESS LOGACESS-CH-PROGRAMA
                                                             INVALID KEY
                        MOVE "10" TO FS-LOGACESS
                  END-START
                  PERFORM UNTIL FS-LOGACESS = "10"
                      READ LOGACESS NEXT RECORD AT END
                          MOVE "10" TO FS-LOGACESS
                      NOT AT END
                          IF ACP-PROGRAMA <> LOGACESS-PROGRAMA
                             MOVE "10" TO FS-LOGACESS
                          ELSE
                             IF ACP-USUARIO = SPACES OR ACP-USUARIO =
                                LOGACESS-USUARIO
                                PERFORM MOVER-DADOS
                             END-IF
                          END-IF
                      END-READ
                  END-PERFORM.

           CLOSE      WORK
           OPEN INPUT WORK.

       MOVER-DADOS SECTION.
           ADD 1 TO TOTAL-ACESSOS
           MOVE SPACES TO LINDET
           MOVE LOGACESS-USUARIO  TO USUARIO-WK
           MOVE LOGACESS-PERIODO  TO PERIODO-WK
           MOVE LOGACESS-PROGRAMA TO PROGRAMA-WK
           MOVE LOGACESS-STATUS   TO STATUS-WK

           write reg-work

           STRING LOGACESS-USUARIO     INTO LINDET(3:5)
           STRING LOGACESS-DIA         INTO LINDET(18:2)
           STRING "/"                  INTO LINDET(20:1)
           STRING LOGACESS-MES         INTO LINDET(21:2)
           STRING "/"                  INTO LINDET(23:1)
           STRING LOGACESS-ANO         INTO LINDET(24:4)
           STRING LOGACESS-HORA        INTO LINDET(35:2)
           STRING ":"                  INTO LINDET(37:1)
           STRING LOGACESS-MINU        INTO LINDET(38:2)
           STRING ":"                  INTO LINDET(40:1)
           STRING LOGACESS-SEGU        INTO LINDET(41:2)
           STRING ":"                  INTO LINDET(43:1)
           STRING LOGACESS-MILE        INTO LINDET(44:2)
           STRING LOGACESS-PROGRAMA    INTO LINDET(56:8)
           STRING LOGACESS-STATUS      INTO LINDET(65:10)
           MOVE "INSERIR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE LOGACESS
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

