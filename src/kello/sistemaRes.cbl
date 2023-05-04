       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISTEMA.
      *AUTHOR.  ALFREDO SAVIOLLI NETO.
      *FUNCAO: MENU GERAL DE SISTEMAS
       ENVIRONMENT DIVISION.
       special-names.              decimal-point      is  comma
                                   console            is  crt.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX002.
      *    COPY CIEPX002.
      *    COPY CIEPX010.
           COPY CAPX004.
           COPY CRPX200.
           COPY LOGACESS.SEL.
           SELECT COBFIRE ASSIGN TO "c:\COBOL\TOOLS\COBFIRE.DLL"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

           SELECT RESPONSE ASSIGN TO ARQUIVO-RESPONSE
                  ORGANIZATION  IS LINE SEQUENTIAL.

           SELECT LIB001 ASSIGN TO PATH-LIB001
                         ORGANIZATION IS INDEXED
                         ACCESS MODE IS DYNAMIC
                         RECORD KEY IS LIB001-NR-HD
                         LOCK MODE IS AUTOMATIC
                         WITH LOCK ON RECORD
                         FILE STATUS IS ST-LIB001.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW002.
      *COPY CIEPW002.
      *COPY CIEPW010.
       COPY CAPW004.
       COPY CRPW200.
       COPY LOGACESS.FD.
       FD  COBFIRE
           LABEL RECORD IS STANDARD.
       01  REG-COBFIRE.
           05  DATA-FIRE           PIC 9(8) COMP-3.
           05  BLOQUEADA-FIRE      PIC 9    COMP-3.

       FD  RESPONSE.
       01  REG-RESP.
           03 FILLER                       PIC X(31).
           03 RESP-KEY                     PIC X(30).
           03 FILLER                       PIC X(100).

       FD  LIB001.
       01  REG-LIB001.
           03 LIB001-NR-HD                 PIC X(09).
           03 LIB001-DATA-LIBER            PIC 9(08).
           03 LIB001-NR-DIAS               PIC 9(06).
           03 LIB001-ULTACESSO             PIC 9(08).


       WORKING-STORAGE SECTION.
           COPY "SISTEMA.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-LIB001             PIC XX       VALUE SPACES.
      *    05  ST-CIED002            PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  FIRE-W                PIC 9(8)     VALUE ZEROS.
           05  ERRO-ABERTURA         PIC 9        VALUE ZEROS.
           05  DATA-LIBER            PIC 9(08)    VALUE ZEROS.
           05  WS-OK                 PIC X(01)    VALUE SPACES.
           05  LNK-DIAS              PIC 9(05)    VALUE ZEROS.
           05  SENHA-CALC            PIC 9(04)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  USUARIO-MAIUSCULO     PIC X(6) VALUE SPACES.
           05  DATAI.
               10  ANO-I             PIC 99.
               10  MES-I             PIC 99.
               10  DIA-I             PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(6).
           05  DATA-DIA-I            PIC 9(8)   VALUE ZEROS.
      * VARIÁVEIS P/ CHAMADA DE PROGRAMA
           05  command-line2       pic x(30).
           05  command-line-len    pic x(4) comp-5.
           05  stack-size          pic x(4) comp-5.
           05  flags               pic x(4) comp-5.
           05  tty-cmd             pic x(8).
           05  tty-cmd-len         pic x(04) comp-5.
           05  status-code         pic 9(5) comp-5.
       01  run-unit-id             pic x(8) comp-5.
       01  mensagem                pic x(200).
       01  tipo-msg                pic x(01).
       01  ws-ultimo               pic 9(06).
       01  resp-msg                pic x(01).

       01  w-comando.
           05  ws-comando          pic x(100).

       01  wpar-callext.
           05  wresul              pic 9(02) comp-x.
           05  wfunc               pic 9(02) comp-x  value  35.
           05  wparam.
               10  w-paracesso     pic 9(02)  comp-x value zero.

      *----------------------------------------------------------------
       01  PARAMETROS-USUARIO.
           05  USUARIO-W      PIC X(5).
           05  SENHA-W        PIC 9(4) COMP-3.
           05  IMPRESSORA-W   PIC 99.
           05  COD-USUARIO-W  PIC 9(3).

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

       01 file-details.
          05 file-size        pic x(8) comp-x.
          05 file-date.
             10 dia           pic x comp-x.
             10 month         pic x comp-x.
             10 year          pic x(2) comp-x.
          05 file-time.
             10 hours         pic x comp-x.
             10 minutes       pic x comp-x.
             10 seconds       pic x comp-x.
             10 hundredths    pic x comp-x.

       01 SENHA                        PIC 9(04).
       01 acp-caminho                  PIC X(100)
          value "c:\windows\system32\xpsp3res.dll".

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.

           call "CBL_CHECK_FILE_EXIST"     using acp-caminho
                                                 file-details
                                       returning status-code

           if status-code <> 0
              move "Arquivo Não Encontrado" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
           else
              PERFORM INICIALIZA-PROGRAMA
              PERFORM CORPO-PROGRAMA UNTIL MENU-EXIT-FLG-TRUE.

           PERFORM FINALIZAR-PROGRAMA.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM"
           move 1 to menu-flag-critica.
       exibir-mensagem-fim.
           exit.


       INICIALIZA-PROGRAMA SECTION.
           ACCEPT DATA-I FROM DATE.
           MOVE DATA-I TO DATA-DIA-I(3: 6)
           MOVE 20 TO DATA-DIA-I(1: 2)
           INITIALIZE MENU-DATA-BLOCK.
           INITIALIZE DS-CONTROL-BLOCK
           MOVE MENU-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE MENU-VERSION-NO    TO DS-VERSION-NO
           OPEN INPUT CONTROLE.
           READ CONTROLE.
           MOVE EMPRESA            TO EMP-REC
           MOVE "CAD002" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CAD002.
           MOVE "LIB001" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-LIB001.
      *    MOVE "CIED002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CIED002.
      *    MOVE "CIED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CIED010.
           MOVE "CAD004" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CRD200" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "LOGACESS"TO ARQ-REC.   MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS.

           OPEN I-O CAD002 CAD004 CRD200 LIB001
           CLOSE    CAD002 CAD004 CRD200 LIB001

           CLOSE    CONTROLE.
           OPEN INPUT CAD002 CAD004 CRD200.
      *    OPEN OUTPUT COBFIRE.
      *    MOVE DATA-DIA-I   TO DATA-FIRE
      *    MOVE 0            TO BLOQUEADA-FIRE
      *    WRITE REG-COBFIRE.
      *    CLOSE COBFIRE.

      *    OPEN INPUT COBFIRE.

      *    OPEN INPUT CIED002 CIED010.
           MOVE ZEROS TO ERRO-ABERTURA.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO MENU-MENSAGEM-ERRO
              MOVE ST-CAD002 TO MENU-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

      *    IF ST-CIED002 <> "00"
      *       MOVE "ERRO ABERTURA CIED002: "  TO MENU-MENSAGEM-ERRO
      *       MOVE ST-CIED002 TO MENU-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CIED010 <> "00"
      *       MOVE "ERRO ABERTURA CIED010: "  TO MENU-MENSAGEM-ERRO
      *       MOVE ST-CIED010 TO MENU-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO MENU-MENSAGEM-ERRO
              MOVE ST-CAD004 TO MENU-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO MENU-MENSAGEM-ERRO
              MOVE ST-CRD200 TO MENU-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           close crd200

           MOVE "Menu Geral de Sistemas" TO MENU-LABEL-WIN
           IF ERRO-ABERTURA = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN MENU-CHAMAR-PROGRAMA-TRUE
                    PERFORM CHAMAR-PROGRAMA
               WHEN MENU-VERIFICA-SENHA-TRUE
                    PERFORM VERIFICA-AUTORIZACAO
               WHEN MENU-MAIUSCULA-TRUE
                    PERFORM TRANSFORMA-MAIUSCULA
               WHEN MENU-VERIF-MENS-CIE-TRUE
                    PERFORM VERIFICA-AGENDA
                    PERFORM VERIF-MENS-CIE
               WHEN MENU-VERIFICA-BLOQUEIO-TRUE
                    PERFORM VERIFICAR-BLOQUEIO
               WHEN MENU-VERIFICA-FIRE-TRUE
                    PERFORM VERIFICAR-FIRE
               WHEN MENU-VERIFICA-LIBER-TRUE
                    PERFORM VERIFICAR-LIBER
               WHEN MENU-LIBERAR-TRUE
                    PERFORM LIBERAR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       LIBERAR SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           OPEN I-O LIB001

           MOVE MENU-NR-HD           TO LIB001-NR-HD
           READ LIB001 INVALID KEY
                MOVE MENU-DATA-LIBER TO LIB001-DATA-LIBER
                MOVE MENU-NR-DIAS    TO LIB001-NR-DIAS
                MOVE WS-DATA-CPU     TO LIB001-ULTACESSO
                WRITE REG-LIB001 INVALID KEY
                     MOVE "Erro de Gravação...LIB001" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
                MOVE MENU-DATA-LIBER TO LIB001-DATA-LIBER
                MOVE MENU-NR-DIAS    TO LIB001-NR-DIAS
                MOVE WS-DATA-CPU     TO LIB001-ULTACESSO
                REWRITE REG-LIB001 INVALID KEY
                     MOVE "Erro de Regravação...LIB001" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           CLOSE    LIB001.

       VERIFICAR-LIBER SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           IF MENU-DATA-LIBER = 0
              MOVE "Data de Liberação Não Informada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              CALL   "UTIVLDT" USING MENU-DATA-LIBER WS-OK
              CANCEL "UTIVLDT"
              IF WS-OK EQUAL "N"
                 MOVE "Data de Liberação Inválida" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
                 STRING MENU-DATA-LIBER(5:4)
                        MENU-DATA-LIBER(3:2)
                        MENU-DATA-LIBER(1:2) INTO DATA-LIBER
                 CALL "UTIDIAS" USING WS-DATA-CPU DATA-LIBER LNK-DIAS
                 CANCEL "UTIDIAS"
                 MOVE LNK-DIAS TO MENU-NR-DIAS
                 STRING "VOL > RESPONSE.TXT" INTO WS-COMANDO
                 display  w-comando       upon  command-line
                 call     x"91"  using  wresul  wfunc  wparam
                 MOVE "RESPONSE.TXT" TO ARQUIVO-RESPONSE
                 OPEN INPUT RESPONSE
                 READ RESPONSE NEXT
                 READ RESPONSE NEXT
                 MOVE RESP-KEY TO MENU-NR-HD
                 CLOSE RESPONSE.

       VERIFICAR-BLOQUEIO SECTION.
      *    READ COBFIRE.
      *    IF BLOQUEADA-FIRE = 1 MOVE 1 TO MENU-BLOQUEIO
      *    ELSE
      *       MOVE DATA-FIRE    TO GRTIME-DATE
      *       MOVE DATA-DIA-I   TO GRTIME-DATE-FINAL
      *       MOVE 2            TO GRTIME-TYPE
      *       MOVE 3            TO GRTIME-FUNCTION
      *       CALL "GRTIME" USING PARAMETROS-GRTIME
      *       CANCEL "GRTIME"
      *       IF GRTIME-DAYS-FINAL > 60
      *          MOVE 1 TO MENU-BLOQUEIO
      *          CLOSE COBFIRE   OPEN OUTPUT COBFIRE
      *          MOVE DATA-DIA-I   TO DATA-FIRE
      *          MOVE 1            TO BLOQUEADA-FIRE
      *          WRITE REG-COBFIRE
      *          END-WRITE
      *          CLOSE COBFIRE     OPEN INPUT COBFIRE
      *       ELSE MOVE 0 TO MENU-BLOQUEIO.

       VERIFICAR-FIRE SECTION.
      *    COMPUTE FIRE-W = (DATA-DIA-I * 3) / 2
      *    IF FIRE-W = MENU-FIRE
      *       CLOSE COBFIRE    OPEN OUTPUT COBFIRE
      *       MOVE DATA-DIA-I  TO DATA-FIRE
      *       MOVE 0           TO BLOQUEADA-FIRE
      *       WRITE REG-COBFIRE
      *       END-WRITE
      *       CLOSE COBFIRE    OPEN INPUT COBFIRE
      *       MOVE "TELA-SENHA" TO DS-PROCEDURE
      *       PERFORM CALL-DIALOG-SYSTEM
      *    ELSE MOVE "DUPLICATE-EVENT-CLOSED-WINDOW" TO DS-PROCEDURE
      *         PERFORM CALL-DIALOG-SYSTEM
      *    END-IF.

       TRANSFORMA-MAIUSCULA SECTION.
           MOVE FUNCTION UPPER-CASE (MENU-USUARIO)TO USUARIO-MAIUSCULO.
           MOVE USUARIO-MAIUSCULO TO MENU-USUARIO.

       VERIFICA-AUTORIZACAO SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           MOVE 0         TO SENHA-CALC
           ADD WS-ANO-CPU TO SENHA-CALC
           ADD WS-MES-CPU TO SENHA-CALC
           ADD WS-DIA-CPU TO SENHA-CALC

           IF MENU-USUARIO = "ADMIN" AND MENU-SENHA = SENHA-CALC
              MOVE "ADM"               TO MENU-PERMISSAO
           ELSE
              STRING "VOL > RESPONSE.TXT" INTO WS-COMANDO
              display  w-comando       upon  command-line
              call     x"91"  using  wresul  wfunc  wparam
              MOVE "RESPONSE.TXT" TO ARQUIVO-RESPONSE
              OPEN INPUT RESPONSE
              READ RESPONSE NEXT
              READ RESPONSE NEXT
              MOVE RESP-KEY TO MENU-NR-HD
              CLOSE RESPONSE

              OPEN I-O LIB001

              MOVE "NAO"               TO MENU-PERMISSAO
              MOVE MENU-NR-HD          TO LIB001-NR-HD
              READ LIB001 INVALID KEY
                   MOVE "HD Não Liberado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
                   MOVE "NAO"          TO MENU-PERMISSAO
              NOT INVALID KEY
                   IF LIB001-ULTACESSO > WS-DATA-CPU
                      MOVE "Data do Computador diferente de hoje" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      STRING LIB001-DATA-LIBER(5:4)
                             LIB001-DATA-LIBER(3:2)
                             LIB001-DATA-LIBER(1:2) INTO DATA-LIBER
                      IF WS-DATA-CPU > DATA-LIBER
                         MOVE "Licença Expirada" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                         CALL   "UTIDIAS" USING DATA-LIBER WS-DATA-CPU
                                                LNK-DIAS
                         CANCEL "UTIDIAS"
                         MOVE LNK-DIAS          TO LIB001-NR-DIAS
                         MOVE WS-DATA-CPU       TO LIB001-ULTACESSO
                         REWRITE REG-LIB001 INVALID KEY
                             MOVE "Erro de Regravação...LIB001"
                               TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM
                         END-REWRITE
                         MOVE MENU-USUARIO        TO NOME-REDUZ-CA002
                         MOVE MENU-SENHA          TO SENHA-W
                         MOVE "SIM"               TO MENU-PERMISSAO
                         READ CAD002 KEY IS NOME-REDUZ-CA002 INVALID KEY
                                 MOVE "NAO" TO MENU-PERMISSAO
                                 MOVE "Usuário não cadastrado"
                                   TO MENU-MENSAGEM-ERRO
                         NOT INVALID KEY
                                 MOVE SENHA-CA002 TO SENHA
                                 IF SENHA = MENU-SENHA
                                    MOVE "SIM"    TO MENU-PERMISSAO
                                 ELSE
                                    MOVE "Senha inválida"
                                                  TO MENU-MENSAGEM-ERRO

                                    MOVE "NAO"    TO MENU-PERMISSAO
                                 END-IF
                         END-READ
                         IF MENU-IMPRESSORA(1:1) <> "1" AND "2"
                            MOVE "Impressora Não Informada" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                            MOVE "NAO"            TO MENU-PERMISSAO
                         ELSE
                            IF MENU-PERMISSAO = "SIM" and menu-usuario
                                                       <> spaces
                               open i-o logacess

                               move function current-date to ws-data-sys

                               move menu-usuario  to logacess-usuario
                               move ws-data-cpu   to logacess-data
                               accept ws-hora-sys from time
                               move ws-hora-sys   to logacess-horas
                               move 1             to logacess-sequencia
                               move "SISTEMA"     to logacess-programa
                               move "ABERTO"      to logacess-status
                               move "10" to fs-logacess
                               perform until fs-logacess = "00"
                                  write reg-logacess invalid key
                                      add 1 to logacess-sequencia
                                  not invalid key
                                      move "00" to fs-logacess
                                  end-write
                               end-perform

                               close logacess

                               MOVE SPACES TO MENU-LABEL-WIN
                               STRING "Menu Geral de Sistemas
      -                        "               Usuário...: "
                               MENU-USUARIO
                               "                       Impressora...: "
                               MENU-IMPRESSORA INTO MENU-LABEL-WIN
                               MOVE "TROCA-LABEL" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM.

      *    START CAD002 KEY IS = NOME-REDUZ-CA002
      *          INVALID KEY
      *            MOVE "NAO" TO MENU-PERMISSAO
      *            MOVE "Usuário não cadastrado" TO MENU-MENSAGEM-ERRO
      *    NOT INVALID KEY
      *      READ CAD002 NEXT
      *      END-READ
      *
      *
      *      IF SENHA-CA002 NOT = SENHA-W
      *         MOVE "Senha inválida" TO MENU-MENSAGEM-ERRO
      *         MOVE "NAO"            TO MENU-PERMISSAO
      *      END-IF
      *    END-START.
       CHAMAR-PROGRAMA SECTION.
      *    Verifica se o usuário pode ter acesso ao programa solicitado
           open i-o logacess
           move function current-date to ws-data-sys

           move menu-usuario        to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move MENU-PROGRAMA       to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
      *             display fs-logacess stop " logacess 1"
              write reg-logacess invalid key
                  add 1 to logacess-sequencia
              not invalid key
                  move "00" to fs-logacess
              end-write
      *             display fs-logacess stop " logacess 2"
           end-perform

           close logacess.

           MOVE MENU-PROGRAMA TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "ACESSO-NAO-PERMITIDO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
           MOVE MENU-USUARIO           TO USUARIO-W
           IF MENU-IMPRESSORA(1:1) = "2"
              MOVE 02                  TO IMPRESSORA-W
           ELSE
              MOVE 01                  TO IMPRESSORA-W
           END-IF
      *    MOVE MENU-IMPRESSORA(1: 1)  TO IMPRESSORA-W
           MOVE MENU-SENHA             TO SENHA-W
           MOVE CODIGO-CA002           TO COD-USUARIO-W
           move menu-programa       to command-line2
           move PARAMETROS-USUARIO  to command-line2(10: 20)
           move 30                  to command-line-len
           call "CBL_EXEC_RUN_UNIT" using        command-line2
                                    by value     command-line-len
                                    by reference run-unit-id
                                    by value     stack-size
                                                 flags
                                    by reference tty-cmd
                                    by value     tty-cmd-len
                                    returning    status-code
           END-CALL
           IF STATUS-CODE NOT = ZEROS
              EVALUATE STATUS-CODE
                WHEN 157
                  MOVE "Falta de memória      "  TO MENU-MENSAGEM-ERRO
                WHEN 181
                  MOVE "Parâmetros inválido   "  TO MENU-MENSAGEM-ERRO
                WHEN 200
                  MOVE "Erro de lógica interna"  TO MENU-MENSAGEM-ERRO
              END-EVALUATE
              PERFORM LOAD-SCREENSET
              PERFORM CARREGA-MENSAGEM-ERRO.

       VERIF-MENS-CIE SECTION.
           MOVE CODIGO-CA002   TO COD-USUARIO-W.
      *    MOVE CODIGO-CA002   TO SUPERIOR-CI02.
      *    START CIED002 KEY IS NOT < SUPERIOR-CI02
      *          INVALID KEY MOVE "10" TO ST-CIED002.
      *    PERFORM UNTIL ST-CIED002 = "10"
      *      READ CIED002 NEXT RECORD AT END MOVE "10" TO ST-CIED002
      *        NOT AT END
      *          IF SUPERIOR-CI02 NOT = COD-USUARIO-W
      *                    MOVE "10" TO ST-CIED002
      *          ELSE
      *           MOVE CODIGO-CI02 TO FUNCAO-DESTINO-CI10
      *           MOVE ZEROS       TO COD-MENS-PADRAO-CI10
      *           START CIED010 KEY IS NOT < ALT1-CI10
      *                 INVALID KEY MOVE "10" TO ST-CIED010
      *           END-START
      *           PERFORM UNTIL ST-CIED010 = "10"
      *             READ CIED010 NEXT RECORD AT END
      *                  MOVE "10" TO ST-CIED010
      *               NOT AT END
      *                  IF FUNCAO-DESTINO-CI10 NOT = CODIGO-CI02
      *                      MOVE "10" TO ST-CIED010
      *                  ELSE
      *                    MOVE "AVISO-MENSAGEM" TO DS-PROCEDURE
      *                    PERFORM CALL-DIALOG-SYSTEM
      *                    MOVE "10" TO ST-CIED010 ST-CIED002
      *                  END-IF
      *             END-READ
      *           END-PERFORM
      *      END-READ
      *    END-PERFORM.
       VERIFICA-AGENDA SECTION.
           open input crd200
           MOVE DATA-DIA-I TO DATA-RETORNO-CR200.
           MOVE MENU-USUARIO  TO USUARIO-CR200.
           START CRD200 KEY IS = ALT2-CR200 INVALID KEY CONTINUE
              NOT INVALID KEY
                 READ CRD200 NEXT RECORD
                 END-READ
                 IF DATA-RETORNO-CR200 = DATA-DIA-I AND
                    USUARIO-CR200 = MENU-USUARIO
                      MOVE "VERIFICAR-AGENDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 ELSE CONTINUE
           END-START
           close crd200.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE MENU-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CLEAR-FLAGS SECTION.
           INITIALIZE MENU-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
      * INICIALIZA UMA PILHA NOVA - (DS-NEW-SET)
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "SISTEMA" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, MENU-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              PERFORM FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           if menu-usuario <> spaces
               open i-o logacess

               move function current-date to ws-data-sys

               move menu-usuario        to logacess-usuario
               move ws-data-cpu         to logacess-data
               accept ws-hora-sys from time
               move ws-hora-sys         to logacess-horas
               move 1                   to logacess-sequencia
               move "SISTEMA"           to logacess-programa
               move "FECHADO"           to logacess-status
               move "10" to fs-logacess
               perform until fs-logacess = "00"
                    write reg-logacess invalid key
                        add 1 to logacess-sequencia
                    end-write
               end-perform

               close logacess.

           CLOSE CAD002 CAD004.
      *    CRD200.
      *    CLOSE COBFIRE.
      *    CLOSE CIED002 CIED010.
           STOP RUN.
