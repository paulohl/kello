       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP022.
      *DATA: 29/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: CONFIRMA IDENTIFICAÇÃO DAS FICHAS DE FORMANDOS
      *FUNÇÃO:
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX008.
           COPY COPX040.
           COPY MTPX019.
           COPY MTPX025.
           COPY IEPX011.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = NOME-CURSO-WK.



       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY COPW008.
       COPY COPW040.
       COPY MTPW019.
       COPY MTPW025.
       COPY IEPW011.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGACESS.FD.

       FD  WORK.
       01  REG-WORK.
           05 CURSO-WK                  PIC 999.
           05 NOME-CURSO-WK             PIC X(12).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY "MTP022.CPB".
           COPY "MTP022.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
           COPY IMPRESSORA.
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-RELAT              PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  COMPACTA              PIC X(01)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  ACHEI                 PIC X(01).
           05  AUX-MOTIVO            PIC 9(03).
           05  AUX-DATA              PIC 9(08).
           05  QTDE-FORMANDOS        PIC 9(04) VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "RELACAO DE FORMANDOS IDENTIFICADOS - CONTRATO: ".
           05  CONTRATO-REL        PIC 9(4)    VALUE ZEROS.
           05  FILLER              PIC XX      VALUE SPACES.
           05  IDENTIFICACAO-REL   PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "ALB.  CURSO           NOME-FORMANDO                  IDENT".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

       02 status-code           PIC X(2) COMP-5.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.

           ACCEPT VARIA-W FROM TIME.

           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD008"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD008.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD025"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC.MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   MTD019 LOG001 LOG002 MTD025.
           OPEN INPUT COD040 IED011 CAD004 COD008.
           IF ST-MTD019 = "35"
              CLOSE MTD019   OPEN OUTPUT MTD019   CLOSE MTD019
              OPEN I-O MTD019.
           IF ST-MTD025 = "35"
              CLOSE MTD025   OPEN OUTPUT MTD025   CLOSE MTD025
              OPEN I-O MTD025.
           IF ST-LOG001 = "35"
              CLOSE LOG001   OPEN OUTPUT LOG001   CLOSE LOG001
              OPEN I-O LOG001.
           IF ST-LOG002 = "35"
              CLOSE LOG002   OPEN OUTPUT LOG002   CLOSE LOG002
              OPEN I-O LOG002.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD025 <> "00"
              MOVE "ERRO ABERTURA MTD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD019 MTD025 LOG001 LOG002
           OPEN INPUT MTD019 MTD025

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP022"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess


           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMA-POPUP-CONTRATO
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CARREGA-CURSOS-TRUE
                    PERFORM CARREGAR-CURSOS
               WHEN GS-SELECAO-TRUE
                    PERFORM MOSTRAR-SELECAO
               WHEN GS-VERIFICA-SITUACAO-TRUE
                    PERFORM VERIFICAR-SITUACAO
               WHEN GS-LE-MOTIVO-TRUE
                    PERFORM LER-MOTIVO
               WHEN GS-CHAMA-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR-MOTIVO
               WHEN GS-CHAMA-MTP029-TRUE
                    PERFORM CHAMA-MT029
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CHAMA-MT029 SECTION.
           MOVE GS-CONTRATO       TO CONTRATO-MT19
           MOVE GS-LINDET(1: 4)   TO SEQ-MT19

           CALL   "MTP029A" USING PARAMETROS-W CONTRATO-MT19 SEQ-MT19
           CANCEL "MTP029A".

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       INCLUIR-MOTIVO SECTION.
           IF GS-DESC-MOTIVO = "********" or spaces
              MOVE "Motivo Informado Inválido" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE SPACES TO GS-MOTIVOS
              MOVE 1      TO GS-CONT
              MOVE "N"    TO ACHEI
              MOVE "LER-MOTIVO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM UNTIL GS-MOTIVOS = SPACES OR ACHEI = "S"
                   MOVE GS-MOTIVOS(1:3) TO AUX-MOTIVO
                   IF AUX-MOTIVO = GS-MOTIVO
                      MOVE "S" TO ACHEI
                   END-IF
                   ADD 1 TO GS-CONT
                   MOVE SPACES TO GS-MOTIVOS
                   MOVE "LER-MOTIVO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM
              IF ACHEI = "S"
                 MOVE "Motivo já Informado !!!" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
                 MOVE ZEROS            TO GS-CONT
                 MOVE SPACES           TO GS-MOTIVOS
                 STRING GS-MOTIVO      INTO GS-MOTIVOS(1:3)
                 STRING GS-DESC-MOTIVO INTO GS-MOTIVOS(5:30).

       LER-MOTIVO SECTION.
           MOVE GS-MOTIVO          TO CODIGO-CO08.
           READ COD008 INVALID KEY
                MOVE "********" TO DESCRICAO-CO08.

           MOVE DESCRICAO-CO08     TO GS-DESC-MOTIVO.

       CHAMAR-POP-UP SECTION.
           CALL   "COP008T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP008T"
           MOVE PASSAR-STRING-1(32: 3) TO GS-MOTIVO
           MOVE PASSAR-STRING-1( 1:30) TO GS-DESC-MOTIVO
           PERFORM LER-MOTIVO.


       MOSTRAR-SELECAO SECTION.
           MOVE GS-LINHA-CURSOS(1:3) TO GS-CURSO
           MOVE GS-LINHA-CURSOS(5:15) TO GS-NOME-CURSO.

       CARREGAR-CURSOS SECTION.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE GS-CONTRATO TO CONTRATO-MT19
           MOVE ZEROS       TO SEQ-MT19
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
               READ MTD019 NEXT AT END
                   MOVE "10" TO ST-MTD019
               NOT AT END
                   IF CONTRATO-MT19 <> GS-CONTRATO
                      MOVE "10" TO ST-MTD019
                   ELSE
                      MOVE CURSO-MT19        TO CODIGO-IE11
                      READ IED011 INVALID KEY
                           MOVE SPACES TO NOME-REDUZ-IE11
                      END-READ
                      MOVE NOME-REDUZ-IE11   TO NOME-CURSO-WK
                      MOVE CURSO-MT19        TO CURSO-WK
                      WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM

           MOVE "LIMPAR-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK GS-CONT-CURSO
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   ADD  1              TO GS-CONT-CURSO
                   MOVE SPACES         TO GS-LINHA-CURSOS
                   MOVE CURSO-WK       TO GS-LINHA-CURSOS(1:3)
                   MOVE NOME-CURSO-WK  TO GS-LINHA-CURSOS(5:15)
                   MOVE "INSERIR-LB2"  TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           CLOSE WORK.

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.

       CHAMA-POPUP-CONTRATO SECTION.
           CALL "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           MOVE PASSAR-PARAMETROS(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       VERIFICAR-SITUACAO SECTION.
           MOVE "SENHA16" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
               MOVE "Você não está apto a atualizar a ficha de identific
      -        "ação solicitar o administrador do software" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM VERIFICAR.

       VERIFICAR SECTION.
           MOVE GS-CONTRATO       TO CONTRATO-MT19.
           MOVE GS-LINDET(1: 4)   TO SEQ-MT19.
           READ MTD019.
           IF IDENTIFICADO-MT19 = 1
              MOVE 0 TO GS-IDENTIFICADO
              PERFORM CARREGAR-MOTIVOS
           ELSE
              MOVE 1 TO GS-IDENTIFICADO.

       CARREGAR-MOTIVOS SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "N" TO ACHEI
           INITIALIZE REG-MTD025
           MOVE CONTRATO-MT19 TO CONTRATO-MT25
           MOVE SEQ-MT19      TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 = "10"
               READ MTD025 NEXT AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
                   IF CONTRATO-MT19 <> CONTRATO-MT25 OR SEQ-MT19 <>
                                                      ALBUM-MT25
                      MOVE "10" TO ST-MTD025
                   ELSE
                      MOVE "S"              TO ACHEI
                      MOVE SPACES           TO GS-MOTIVOS
                      STRING MOTIVO-MT25    INTO GS-MOTIVOS(1:3)
                      MOVE MOTIVO-MT25      TO CODIGO-CO08
                      READ COD008 INVALID KEY
                           MOVE "******"    TO DESCRICAO-CO08
                      END-READ
                      STRING DESCRICAO-CO08 INTO GS-MOTIVOS(5:30)
                      STRING DATA-MT25(7:2) INTO GS-MOTIVOS(31:2)
                      STRING "/"            INTO GS-MOTIVOS(33:1)
                      STRING DATA-MT25(5:2) INTO GS-MOTIVOS(34:2)
                      STRING "/"            INTO GS-MOTIVOS(36:1)
                      STRING DATA-MT25(1:4) INTO GS-MOTIVOS(37:4)
                      MOVE "S" TO GS-COLOQUEI
                      MOVE "INCLUIR-MOTIVO" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              MOVE SPACES           TO   GS-MOTIVOS
              MOVE 001              TO   CODIGO-CO08
              READ COD008 INVALID KEY
                   MOVE "********"  TO   DESCRICAO-CO08
              END-READ
              MOVE 001              TO   GS-MOTIVOS(1:3)
              STRING DESCRICAO-CO08 INTO GS-MOTIVOS(5:30)
              MOVE "S" TO GS-COLOQUEI
              MOVE "INCLUIR-MOTIVO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.


       ITEM-SELECIONADO SECTION.
           MOVE "SENHA16" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
               MOVE "Você não está apto a atualizar a ficha de identific
      -        "ação solicitar o administrador do software" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM ATUALIZAR-ITEM.


       ATUALIZAR-ITEM SECTION.
           CLOSE    MTD019
           OPEN I-O MTD019 LOG002

           MOVE GS-CONTRATO       TO CONTRATO-MT19.
           MOVE GS-LINDET(1: 4)   TO SEQ-MT19.
           READ MTD019.
           IF IDENTIFICADO-MT19 = 0
              MOVE 1 TO IDENTIFICADO-MT19
              REWRITE REG-MTD019 NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG2-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG2-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG2-HORAS
                      MOVE "A"         TO LOG2-OPERACAO
                      MOVE "MTD019"    TO LOG2-ARQUIVO
                      MOVE "MTP022"    TO LOG2-PROGRAMA
                      MOVE REG-MTD019  TO LOG2-REGISTRO
                      WRITE REG-LOG002
                      END-WRITE
              END-REWRITE
              MOVE "SIM" TO GS-LINDET(66: 3)
           ELSE
              MOVE 0 TO IDENTIFICADO-MT19
              REWRITE REG-MTD019 NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG2-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG2-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG2-HORAS
                      MOVE "A"         TO LOG2-OPERACAO
                      MOVE "MTD019"    TO LOG2-ARQUIVO
                      MOVE "MTP022"    TO LOG2-PROGRAMA
                      MOVE REG-MTD019  TO LOG2-REGISTRO
                      WRITE REG-LOG002
                      END-WRITE
              END-REWRITE
              PERFORM GRAVAR-MOTIVOS
              MOVE "NÃO" TO GS-LINDET(66: 3)
           END-IF

           CLOSE      MTD019 LOG002
           OPEN INPUT MTD019.

      ******************************************************************
      *Comentado no dia 20/06/2011 => Pedido pelo Anderson
      ******************************************************************
      *
      *    INITIALIZE REG-MTD019
      *               QTDE-FORMANDOS
      *    MOVE GS-CONTRATO            TO CONTRATO-MT19
      *    START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
      *         MOVE "10" TO ST-MTD019.
      *
      *    PERFORM UNTIL ST-MTD019 = "10"
      *         READ MTD019 NEXT AT END
      *              MOVE "10" TO ST-MTD019
      *         NOT AT END
      *              IF GS-CONTRATO <> CONTRATO-MT19
      *                 MOVE "10" TO ST-MTD019
      *              ELSE
      *                 ADD 1 TO QTDE-FORMANDOS
      *              END-IF
      *         END-READ
      *    END-PERFORM
      *
      *    IF QTDE-FORMANDOS > 0
      *       CLOSE      COD040
      *       OPEN I-O   COD040
      *
      *       MOVE GS-CONTRATO          TO NR-CONTRATO-CO40
      *       READ COD040 INVALID KEY
      *            MOVE "Contrato Não Encontrado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM
      *       NOT INVALID KEY
      *            MOVE QTDE-FORMANDOS  TO QTDE-FORM-CO40
      *            REWRITE REG-COD040 INVALID KEY
      *                 MOVE "Erro de Regravação...COD040" TO MENSAGEM
      *                 MOVE "C" TO TIPO-MSG
      *                 PERFORM EXIBIR-MENSAGEM
      *            END-REWRITE
      *       END-READ
      *
      *       CLOSE      COD040
      *       OPEN INPUT COD040.
      ******************************************************************

       GRAVAR-MOTIVOS SECTION.
           CLOSE    MTD025
           OPEN I-O LOG001 MTD025

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE SPACES TO GS-MOTIVOS
           MOVE 1      TO GS-CONT
           MOVE "N"    TO ACHEI
           MOVE "LER-MOTIVO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-MOTIVOS = SPACES OR ACHEI = "S"
                INITIALIZE REG-MTD025
                MOVE GS-MOTIVOS(1:3) TO AUX-MOTIVO
                MOVE CONTRATO-MT19   TO CONTRATO-MT25
                MOVE SEQ-MT19        TO ALBUM-MT25
                MOVE AUX-MOTIVO      TO MOTIVO-MT25
                MOVE WS-DATA-CPU     TO DATA-MT25
                WRITE REG-MTD025 NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG1-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG1-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG1-HORAS
                      MOVE "I"         TO LOG1-OPERACAO
                      MOVE "MTD025"    TO LOG1-ARQUIVO
                      MOVE "MTP019"    TO LOG1-PROGRAMA
                      MOVE REG-MTD025  TO LOG1-REGISTRO
                      WRITE REG-LOG001
                      END-WRITE
                END-WRITE
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-MOTIVOS
                MOVE "LER-MOTIVO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE MTD025 LOG001
           OPEN INPUT MTD025.



       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           EVALUATE GS-ORDER
             WHEN 1 MOVE GS-CONTRATO TO CONTRATO-MT19
                    MOVE ZEROS       TO SEQ-MT19
                    START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                          MOVE "10" TO ST-MTD019
                    END-START
             WHEN OTHER MOVE GS-CONTRATO   TO CONTRATO-MT19
                        MOVE ZEROS         TO CURSO-MT19
                        MOVE SPACES        TO NOME-FORM-MT19
                        START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                              MOVE "10" TO ST-MTD019
                        END-START
           END-EVALUATE.
           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
               IF CONTRATO-MT19 <> GS-CONTRATO MOVE "10" TO ST-MTD019
               ELSE
                  IF GS-CURSO = 0 OR CURSO-MT19 = GS-CURSO
                     MOVE SEQ-MT19        TO GS-LINDET(1: 10)
                     MOVE CURSO-MT19      TO CODIGO-IE11
                     READ IED011 INVALID KEY
                          MOVE SPACES TO NOME-REDUZ-IE11
                     END-READ
                     MOVE NOME-REDUZ-IE11 TO GS-LINDET(11: 16)
                     MOVE NOME-FORM-MT19  TO GS-LINDET(26: 40)
                     EVALUATE IDENTIFICADO-MT19
                       WHEN 0 MOVE "NÃO"  TO GS-LINDET(66: 3)
                       WHEN 1 MOVE "SIM"  TO GS-LINDET(66: 3)
                     END-EVALUATE
                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
               END-IF
              END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP022" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *--------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
      *    COPY "COND-IMP".

           MOVE GS-CONTRATO TO CONTRATO-REL.

      *    STRING "\ARQUIVOS\" CONTRATO-REL INTO ARQUIVO-IMPRESSAO
      *    OPEN OUTPUT RELAT.

           COPY CONDENSA.


           MOVE ZEROS TO LIN. PERFORM CABECALHO.

           MOVE SPACES TO LINDET-REL
           EVALUATE GS-ORDER
             WHEN 1 MOVE GS-CONTRATO TO CONTRATO-MT19
                    MOVE ZEROS       TO SEQ-MT19
                    START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                          MOVE "10" TO ST-MTD019
                    END-START
             WHEN OTHER MOVE GS-CONTRATO   TO CONTRATO-MT19
                        MOVE ZEROS         TO CURSO-MT19
                        MOVE SPACES        TO NOME-FORM-MT19
                        START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                              MOVE "10" TO ST-MTD019
                        END-START
           END-EVALUATE.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
             NOT AT END
              IF CONTRATO-MT19 <> GS-CONTRATO MOVE "10" TO ST-MTD019
              ELSE
               IF GS-TIPO-IMPRESSAO = 1 AND IDENTIFICADO-MT19 = 0 OR
      *           (imprimir apenas identificado)
                  GS-TIPO-IMPRESSAO = 2 AND IDENTIFICADO-MT19 = 1
      *           (imprimir apenas não identificados)
                  CONTINUE
               ELSE
                   IF GS-CURSO = 0 OR GS-CURSO = CURSO-MT19
                      MOVE SEQ-MT19        TO LINDET-REL(1: 6)
                      MOVE CURSO-MT19      TO CODIGO-IE11
                      READ IED011 INVALID KEY
                           MOVE SPACES TO NOME-REDUZ-IE11
                      END-READ
                      MOVE NOME-REDUZ-IE11 TO LINDET-REL(7: 16)
                      MOVE NOME-FORM-MT19  TO LINDET-REL(23: 31)
                      EVALUATE IDENTIFICADO-MT19
                           WHEN 0 MOVE "NAO"  TO LINDET-REL(54: 3)
                                  MOVE SPACES TO LINDET-REL(58:20)
                                  PERFORM VER-MOTIVO
                           WHEN 1 MOVE "SIM"  TO LINDET-REL(54: 3)
                                  MOVE SPACES TO LINDET-REL(58:20)
                      END-EVALUATE

                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                   END-IF
               END-IF
              END-IF
             END-READ
           END-PERFORM.

      *    MOVE "SALTAR PAGINA" TO REG-RELAT
      *    WRITE REG-RELAT
      *
      *    CLOSE RELAT.
      *
      *    IF GS-IMPRIMIR = "S"
      *       MOVE "N" TO COMPACTA
      *       CALL "PRP104" USING PARAMETROS-W
      *                           ARQUIVO-IMPRESSAO COMPACTA
      *                           IMPRESSORA-W
      *       CANCEL "PRP104"
      *       call "CBL_DELETE_FILE" using     ARQUIVO-IMPRESSAO
      *                              returning status-code
      *       if status-code <> "0000"
      *          move "Erro na Exclusão do Arquivo" to mensagem
      *         move "C" to tipo-msg
      *         perform exibir-mensagem.

           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.

           COPY DESCONDENSA.


       VER-MOTIVO SECTION.
           INITIALIZE REG-MTD025 AUX-DATA
           MOVE CONTRATO-MT19      TO CONTRATO-MT25
           MOVE SEQ-MT19           TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 = "10"
               READ MTD025 NEXT RECORD AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
               IF CONTRATO-MT19 <> CONTRATO-MT25 OR
                  SEQ-MT19      <> ALBUM-MT25
                  MOVE "10" TO ST-MTD025
               ELSE
                  IF DATA-MT25 > AUX-DATA OR DATA-MT25 = AUX-DATA
                     MOVE DATA-MT25   TO AUX-DATA
                     MOVE MOTIVO-MT25 TO CODIGO-CO08
                     READ COD008 INVALID KEY
                           MOVE "*****"        TO LINDET-REL(58:20)
                     NOT INVALID KEY
                           MOVE DESCRICAO-CO08 TO LINDET-REL(58:20)
                     END-READ.

       CABECALHO SECTION.
           MOVE GS-CONTRATO TO CONTRATO-REL.
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM CAB01
           ELSE
              MOVE "SALTAR PAGINA" TO REG-RELAT
              WRITE REG-RELAT
              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM CAB01.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT FROM CAB02

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD008 COD040 MTD019 MTD025 IED011 CAD004.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP022"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
