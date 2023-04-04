       COPY DSLANG.CPY.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP055X.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 28/09/2016.
      *FUNÇÃO: Movimento de CUSTEIO - SUBPROGRAMA

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX009.
           COPY COPX040.
           copy copx049.
           COPY COPX055.
           COPY COPX055i.
           COPY CGPX001.
           COPY IEPX011.
           COPY PARX001.
           COPY LOGX002.
           COPY LOGX005.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY COPW009.
       COPY COPW040.
       copy copw049.
       COPY COPW055.
       COPY COPW055i.
       COPY CGPW001.
       COPY IEPW011.
       COPY PARW001.
       COPY LOGW002.
       COPY LOGW005.
           COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP055X.CPB".
           COPY "COP055X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-COD009             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD049             PIC XX       VALUE SPACES.
           05  ST-COD055             PIC XX       VALUE SPACES.
           05  ST-COD055i            PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG005             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  ITEM-W                PIC X(2)     VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).

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
           05  DESABILITA            PIC 9(01)    VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02) VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02) VALUE ZEROS.


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE CUSTEIO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONT IT CUR TU INSUMO          QT FORM CUSTO-UNITÁR. DATA-VE
      -    "CTO VALOR-PREVIST DATA-SOLIC S R DATA-PAGTO    VALOR-PAGO  D
      -    "IAS FORNEC".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).
          05 FILLER                    PIC X(13).

       01 WS-HORA-SYS.
          05 WS-HO-SYS                 PIC 9(02).
          05 WS-MI-SYS                 PIC 9(02).
          05 WS-SE-SYS                 PIC 9(02).
          05 WS-MS-SYS                 PIC 9(02).

       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD009" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD009.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD049" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD049.
           MOVE "COD055" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD055.
           MOVE "COD055i" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD055i.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG005.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   COD055 COD055i LOG002 LOG005 COD049
           CLOSE      COD055 COD055i LOG002 LOG005 COD049
           OPEN INPUT COD055 COD055i COD049

           OPEN I-O   COD009
           CLOSE      COD009
           OPEN INPUT CAD004 IED011 COD009 CGD001 COD040 PAR001.
           IF ST-COD055 = "35"
              CLOSE COD055      OPEN OUTPUT COD055
              CLOSE COD055      OPEN I-O COD055
           END-IF.
           IF ST-COD009 <> "00"
              MOVE "ERRO ABERTURA COD009: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD009 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD049 <> "00"
              MOVE "ERRO ABERTURA COD049: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD049 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD055 <> "00"
              MOVE "ERRO ABERTURA COD055: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD055 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD055i <> "00"
              MOVE "ERRO ABERTURA COD055i: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD055i TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP055X"           to logacess-programa
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


           MOVE COD-USUARIO-W             TO COD-USUARIO-CA004
           MOVE "SENHA37"                 TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
                MOVE "DESABILITA-CAMPOS"  TO DS-PROCEDURE
                MOVE 1                    TO GS-DESABILITA
           NOT INVALID KEY
                MOVE "HABILITA-CAMPOS"    TO DS-PROCEDURE
                MOVE 0                    TO GS-DESABILITA
           END-READ
           PERFORM CALL-DIALOG-SYSTEM.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   MOVE SPACES TO MENSAGEM
                   IF SUSP-PREV-DEF-CO55 = 2 AND
                      GS-VALOR-PAGO > 0
                      MOVE "Não pode estar Suspenso com o Valor Pago Inf
      -               "ormado" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   END-IF
                   IF MENSAGEM = SPACES
                      IF GS-TIPO-GRAVACAO = 1
                         PERFORM REGRAVA-DADOS
                      ELSE
                         PERFORM GRAVA-DADOS
                      END-IF
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO55
                   MOVE GS-LINDET(6: 4)  TO ITEM-CO55
                   PERFORM CARREGAR-DADOS
                   PERFORM VERIFICA-CONTRATO
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-LE-BRINDE-TRUE
                   PERFORM LE-BRINDE
               WHEN GS-LE-FORNEC-TRUE
                   PERFORM LE-FORNECEDOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CALCULA-VLR-PREV-TRUE
                    PERFORM CALCULA-VLR-PREVISTO
               WHEN GS-CALCULA-DIAS-PRAZ-TRUE
                    PERFORM CALCULA-PRAZO-MEDIO
               WHEN GS-CHAMADA-SUBPROG-TRUE
                    PERFORM CHAMADA-SUBPROGRAMA
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-VERIFICA-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICA-CONTRATO SECTION.
           ENABLE-OBJECT PB1
           ENABLE-OBJECT PB3

           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                EVALUATE STATUS-CO40
                    WHEN WS-STATUS-REVENDIDO
                         MOVE "Contrato com STATUS Revendido" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                         DISABLE-OBJECT PB1
                         DISABLE-OBJECT PB3
                    WHEN WS-STATUS-ANALISE
                         MOVE "Contrato com STATUS Analise" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                         DISABLE-OBJECT PB1
                         DISABLE-OBJECT PB3
                    WHEN OTHER
                         MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
                         READ COD049 INVALID KEY
                              MOVE 0 TO GS-FLAG-CRITICA
                         NOT INVALID KEY
                              IF CANCELADO-CO49 = 1
                                 MOVE "Contrato ENCERRADO" to mensagem
                                 move "C" to tipo-msg
                                 perform exibir-mensagem
                                 DISABLE-OBJECT PB1
                                 DISABLE-OBJECT PB3
                              ELSE
                                 MOVE 0 TO GS-FLAG-CRITICA
                              END-IF
                         END-READ
                END-EVALUATE.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA14"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITA-BOTAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMADA-SUBPROGRAMA SECTION.
           MOVE STRING-1(1: 4) TO GS-CONTRATO.
           MOVE STRING-1(5: 4) TO GS-NR-ITEM
           MOVE STRING-1(9: 2) TO IMPRESSORA-W.
           MOVE STRING-1(11: 3) TO COD-USUARIO-W.
           IF GS-CONTRATO <> ZEROS  PERFORM CARREGA-ULTIMOS.
           MOVE STRING-1(1: 4)      TO NR-CONTRATO-CO55
           MOVE STRING-1(5: 4)      TO ITEM-CO55
           IF ITEM-CO55 <> ZEROS    PERFORM CARREGAR-DADOS
                                    MOVE 1 TO GS-TIPO-GRAVACAO.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA04"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA14"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITA-BOTAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.


       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CURSO
             WHEN 2 PERFORM CARREGA-POP-UP-BRINDE
             WHEN 3 PERFORM CARREGA-POP-UP-FORNEC
           END-EVALUATE.
       CARREGA-POP-UP-FORNEC SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                MOVE NOME-CG01(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD001
                ELSE
                  MOVE NOME-CG01       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CG01     TO GS-LINDET1(33: 06)
                  MOVE "INSERE-POP-UP-FORNEC" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-BRINDE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CO09.
           START COD009 KEY IS NOT < NOME-CO09 INVALID KEY
                 MOVE "10" TO ST-COD009.
           PERFORM UNTIL ST-COD009 = "10"
              READ COD009 NEXT RECORD AT END MOVE "10" TO ST-COD009
               NOT AT END
                MOVE NOME-CO09(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-COD009
                ELSE
                  MOVE NOME-CO09       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CO09     TO GS-LINDET1(33: 05)
                  MOVE "INSERE-POP-UP-BRINDE" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-CURSO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-IE11
           START IED011 KEY IS NOT < NOME-IE11 INVALID KEY
                 MOVE "10" TO ST-IED011.
           PERFORM UNTIL ST-IED011 = "10"
              READ IED011 NEXT RECORD AT END MOVE "10" TO ST-IED011
               NOT AT END
                MOVE NOME-IE11(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-IED011
                ELSE
                  MOVE NOME-IE11       TO GS-LINDET1(1: 42)
                  MOVE CODIGO-IE11     TO GS-LINDET1(43: 03)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1)
                          TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET1(43: 3) TO GS-CURSO
               MOVE GS-LINDET1(1: 40) TO GS-NOME-CURSO
             WHEN 2
               MOVE GS-LINDET1(33: 5) TO GS-COD-BRINDE
               MOVE GS-LINDET1(1: 30) TO GS-NOME-BRINDE
             WHEN 3
               MOVE GS-LINDET1(33: 6)  TO GS-FORNEC
               MOVE GS-LINDET1(1: 30)  TO GS-NOME-FORN
           END-EVALUATE.
      *----------------------------------------------------------------
       CALCULA-VLR-PREVISTO SECTION.
           MOVE GS-COD-BRINDE TO CODIGO-CO09
           READ COD009 INVALID KEY INITIALIZE REG-COD009.

           IF GS-CUSTO-UNITARIO = ZEROS
              MOVE VALOR-CO09 TO CUSTO-PREVISTO-W
              MOVE VALOR-CO09 TO GS-CUSTO-UNITARIO
           ELSE
              MOVE GS-CUSTO-UNITARIO TO CUSTO-PREVISTO-W.

           IF MULT-FORM-CO09 = 2
              COMPUTE GS-VALOR-PREVISTO = GS-QTDE-POR-FORM *
                                      CUSTO-PREVISTO-W
           ELSE
              COMPUTE GS-VALOR-PREVISTO = (GS-QTDE-POR-FORM *
                GS-QTDE-FORM) * CUSTO-PREVISTO-W.
      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    COD055 COD055i
           OPEN I-O COD055 COD055i

           MOVE REG-COD055 TO REG-COD055i
           READ COD055i NOT INVALID KEY
               DELETE COD055i NOT INVALID KEY
                      MOVE "E"      TO LOG2-OPERACAO
                      MOVE "COD055i" TO LOG2-ARQUIVO
                      PERFORM GRAVAR-LOG002.

           DELETE COD055 NOT INVALID KEY
                  MOVE "E"          TO LOG2-OPERACAO
                  MOVE "COD055"     TO LOG2-ARQUIVO
                  PERFORM GRAVAR-LOG005.

           CLOSE      COD055 COD055i
           OPEN INPUT COD055 COD055i

           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.

       GRAVAR-LOG002 SECTION.
           OPEN I-O LOG002

           MOVE FUNCTION CURRENT-DATE  TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           MOVE USUARIO-W              TO LOG2-USUARIO
           MOVE WS-DATA-CPU            TO LOG2-DATA
           MOVE WS-HORA-SYS            TO LOG2-HORAS
           MOVE "COP050"               TO LOG2-PROGRAMA
           MOVE REG-COD055i             TO LOG2-REGISTRO

           WRITE REG-LOG002 INVALID KEY
                 MOVE "Erro de Gravação...LOG002" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           END-WRITE

           CLOSE    LOG002.

       GRAVAR-LOG005 SECTION.
           OPEN I-O LOG005

           MOVE FUNCTION CURRENT-DATE  TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           MOVE USUARIO-W              TO LOG5-USUARIO
           MOVE WS-DATA-CPU            TO LOG5-DATA
           MOVE WS-HORA-SYS            TO LOG5-HORAS
           MOVE "COP050"               TO LOG5-PROGRAMA
           MOVE REG-COD055             TO LOG5-REGISTRO

           WRITE REG-LOG005 INVALID KEY
                 MOVE "Erro de Gravação...LOG005" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           END-WRITE

           CLOSE    LOG005.

       LE-FORNECEDOR SECTION.
           MOVE GS-FORNEC          TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FORN.
       LE-BRINDE SECTION.
           MOVE GS-COD-BRINDE      TO CODIGO-CO09.
           READ COD009 INVALID KEY MOVE "********" TO NOME-CO09.
           MOVE NOME-CO09          TO GS-NOME-BRINDE.
           IF GS-TIPO-GRAVACAO = 0
              MOVE VALOR-CO09         TO GS-CUSTO-UNITARIO.
       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "******" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-NOME-CURSO.
       CARREGAR-DADOS SECTION.
           START COD055 KEY IS = CHAVE-CO55 INVALID KEY CONTINUE.
           READ COD055 INVALID KEY INITIALIZE REG-COD055.
           MOVE NR-CONTRATO-CO55     TO  GS-CONTRATO
           MOVE ITEM-CO55            TO  GS-NR-ITEM
           MOVE CODBRINDE-CO55       TO  GS-COD-BRINDE CODIGO-CO09
           READ COD009 INVALID KEY MOVE "*******" TO NOME-CO09.
           MOVE NOME-CO09            TO  GS-NOME-BRINDE
           MOVE CURSO-CO55           TO  GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY MOVE "*****" TO NOME-IE11.
           MOVE NOME-IE11            TO  GS-NOME-CURSO.
           MOVE TURMA-CO55           TO  GS-TURMA
           MOVE QTDE-POR-FORM-CO55   TO  GS-QTDE-POR-FORM
           MOVE QTDE-FORM-CO55       TO  GS-QTDE-FORM
           MOVE CUSTO-UNIT-CO55      TO  GS-CUSTO-UNITARIO
           MOVE VALOR-PREVISTO-CO55  TO  GS-VALOR-PREVISTO
           MOVE DATA-VENCTO-CO55     TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-VENCTO
           MOVE DATA-SOLICIT-CO55    TO  GS-DATA-SOLICIT
           EVALUATE SUSP-PREV-DEF-CO55
             WHEN 0 MOVE "0-Previsto  "  TO GS-SUSP-PREV-DEF
             WHEN 1 MOVE "1-Definitivo"  TO GS-SUSP-PREV-DEF
             WHEN 2 MOVE "2-Suspenso  "  TO GS-SUSP-PREV-DEF
           END-EVALUATE.
           MOVE VALOR-PAGO-CO55      TO  GS-VALOR-PAGO
           MOVE DATA-PAGTO-CO55      TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-PAGTO
           EVALUATE REALIZADO-CO55
             WHEN 0 MOVE "0-Não"     TO  GS-REALIZADO
             WHEN 1 MOVE "1-Sim"     TO  GS-REALIZADO
           END-EVALUATE.
           MOVE DIAS-PRAZO-CO55      TO  GS-PRAZO-MEDIO
           MOVE COD-FORNEC-CO55      TO  GS-FORNEC CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01            TO  GS-NOME-FORN

           MOVE DATA-ENVIO-CO55         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                TO GS-DATA-ENVIO
           MOVE OBSERVACAO-CO55         TO GS-OBSERVACAO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-DESABILITA         TO DESABILITA
           INITIALIZE REG-COD055
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE DESABILITA            TO GS-DESABILITA.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO NR-CONTRATO-CO55
           MOVE GS-NR-ITEM            TO ITEM-CO55
           MOVE GS-COD-BRINDE         TO CODBRINDE-CO55
           MOVE GS-CURSO              TO CURSO-CO55
           MOVE GS-TURMA              TO TURMA-CO55
           MOVE GS-QTDE-POR-FORM      TO QTDE-POR-FORM-CO55
           MOVE GS-QTDE-FORM          TO QTDE-FORM-CO55
           MOVE GS-CUSTO-UNITARIO     TO CUSTO-UNIT-CO55
           MOVE GS-VALOR-PREVISTO     TO VALOR-PREVISTO-CO55
           MOVE GS-DATA-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-VENCTO-CO55
           MOVE GS-DATA-SOLICIT       TO DATA-SOLICIT-CO55
           MOVE GS-SUSP-PREV-DEF(1: 1) TO SUSP-PREV-DEF-CO55
           MOVE GS-VALOR-PAGO         TO VALOR-PAGO-CO55
           MOVE GS-DATA-PAGTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-PAGTO-CO55
           MOVE GS-REALIZADO(1: 1)    TO REALIZADO-CO55
           PERFORM CALCULA-PRAZO-MEDIO.
           MOVE GS-PRAZO-MEDIO        TO DIAS-PRAZO-CO55
           MOVE GS-FORNEC             TO COD-FORNEC-CO55
           MOVE GS-DATA-ENVIO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-ENVIO-CO55
           MOVE GS-OBSERVACAO         TO OBSERVACAO-CO55.
       CALCULA-PRAZO-MEDIO SECTION.
           IF GS-DATA-PAGTO = ZEROS
              MOVE GS-DATA-VENCTO     TO GRTIME-DATE
           ELSE
              MOVE GS-DATA-PAGTO      TO GRTIME-DATE.
           MOVE GRTIME-DATE           TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO GRTIME-DATE
           MOVE GS-CONTRATO           TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE ZEROS            TO DATA-PREV-VENDA-CO40.
           MOVE DATA-PREV-VENDA-CO40  TO GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO GRTIME-DATE-FINAL
           IF GRTIME-DATE > GRTIME-DATE-FINAL
              MOVE "EXIBE-ERRO-DATA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
              MOVE 2                     TO GRTIME-TYPE
              MOVE 3                     TO GRTIME-FUNCTION
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DAYS-FINAL     TO GS-PRAZO-MEDIO.

       GRAVA-DADOS SECTION.
           CLOSE COD055 COD055i
           OPEN I-O COD055 COD055i

           MOVE ZEROS TO ST-COD055.
           PERFORM UNTIL ST-COD055 = "10"
                WRITE REG-COD055 INVALID KEY
                      ADD 1 TO ITEM-CO55
                NOT INVALID KEY
                      MOVE "I"      TO LOG5-OPERACAO
                      MOVE "COD055" TO LOG5-ARQUIVO
                      PERFORM GRAVAR-LOG005
                      MOVE "10"     TO ST-COD055.

           MOVE ZEROS TO ST-COD055i.
           MOVE REG-COD055 TO REG-COD055i
           PERFORM UNTIL ST-COD055i = "10"
                WRITE REG-COD055i INVALID KEY
                      ADD 1 TO ITEM-CO55i
                NOT INVALID KEY
                      MOVE "I"      TO LOG2-OPERACAO
                      MOVE "COD055i" TO LOG2-ARQUIVO
                      PERFORM GRAVAR-LOG002
                      MOVE "10"     TO ST-COD055i.

           CLOSE COD055 COD055i
           OPEN INPUT COD055 COD055i

           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE    COD055
           OPEN I-O COD055
           REWRITE REG-COD055 INVALID KEY
                 MOVE "Erro Regravacao COD055" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD055 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE "A"      TO LOG5-OPERACAO
                 MOVE "COD055" TO LOG5-ARQUIVO
                 PERFORM GRAVAR-LOG005
           END-REWRITE
           CLOSE      COD055
           OPEN INPUT COD055
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO55.
           MOVE ZEROS           TO ITEM-CO55 GS-NR-ITEM.
           START COD055 KEY IS NOT < CHAVE-CO55
                    INVALID KEY MOVE "10" TO ST-COD055.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD055 = "10"
              READ COD055 NEXT RECORD AT END
                   MOVE "10" TO ST-COD055
              NOT AT END
                   IF NR-CONTRATO-CO55 <> GS-CONTRATO
                      MOVE "10" TO ST-COD055
                   ELSE
                      PERFORM MOVER-DADOS-LISTA
                      MOVE ITEM-CO55     TO GS-NR-ITEM
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE NR-CONTRATO-CO55   TO GS-LINDET(1: 5)
           MOVE ITEM-CO55          TO GS-LINDET(6: 5)
           MOVE CURSO-CO55         TO GS-LINDET(11: 4)
           MOVE TURMA-CO55         TO GS-LINDET(15: 3)
           MOVE CODBRINDE-CO55     TO CODIGO-CO09
           READ COD009 INVALID KEY MOVE "*******" TO NOME-CO09.
           MOVE NOME-CO09          TO GS-LINDET(18: 17)
           MOVE QTDE-POR-FORM-CO55 TO GS-LINDET(36: 06)
           MOVE QTDE-FORM-CO55     TO GS-LINDET(42: 05)
           MOVE CUSTO-UNIT-CO55    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(47: 14)
           MOVE DATA-VENCTO-CO55   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(61: 11)
           MOVE DATA-SOLICIT-CO55  TO DATA-E
           MOVE DATA-E             TO GS-LINDET(72: 11)
           MOVE SUSP-PREV-DEF-CO55 TO GS-LINDET(83: 2)
           MOVE REALIZADO-CO55     TO GS-LINDET(85: 2)
           MOVE DATA-PAGTO-CO55    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(87: 11)
           MOVE VALOR-PAGO-CO55    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(98: 13).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP055X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO55.
           MOVE ZEROS          TO ITEM-CO55.
           START COD055 KEY IS = CHAVE-CO55 INVALID KEY
                 MOVE "10" TO ST-COD055.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD055 = "10"
             READ COD055 NEXT RECORD AT END MOVE "10" TO ST-COD055
              NOT AT END
                IF NR-CONTRATO-CO55 <> GS-CONTRATO
                         MOVE "10" TO ST-COD055
                ELSE
                  MOVE NR-CONTRATO-CO55    TO LINDET-REL(1: 5)
                  MOVE ITEM-CO55           TO LINDET-REL(6: 5)
                  MOVE CURSO-CO55          TO LINDET-REL(11: 4)
                  MOVE TURMA-CO55          TO LINDET-REL(15: 3)
                  MOVE CODBRINDE-CO55      TO CODIGO-CO09
                  READ COD009 INVALID KEY MOVE SPACES TO NOME-CO09
                  END-READ
                  MOVE NOME-CO09           TO LINDET-REL(18: 12)
                  MOVE QTDE-POR-FORM-CO55  TO LINDET-REL(31: 6)
                  MOVE QTDE-FORM-CO55      TO LINDET-REL(37: 5)
                  MOVE CUSTO-UNIT-CO55     TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(42: 14)
                  MOVE DATA-VENCTO-CO55    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV            TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(56: 11)
                  MOVE VALOR-PREVISTO-CO55 TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(67: 14)
                  MOVE DATA-SOLICIT-CO55   TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(81: 11)
                  MOVE SUSP-PREV-DEF-CO55  TO LINDET-REL(92: 2)
                  MOVE REALIZADO-CO55      TO LINDET-REL(94: 2)
                  MOVE DATA-PAGTO-CO55     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV            TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(96: 11)
                  MOVE VALOR-PAGO-CO55     TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(107: 14)
                  MOVE DIAS-PRAZO-CO55     TO LINDET-REL(121: 6)
                  MOVE COD-FORNEC-CO55     TO LINDET-REL(127: 6)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 60 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       EXIBIR-MENSAGEM SECTION.
           move 1 to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD004 COD009 COD040 COD055 IED011 CGD001 COD055i
                 COD049.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "COP055X"           to logacess-programa
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


           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

