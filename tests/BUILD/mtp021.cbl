       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP021.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 04/07/2000.
      *FUNÇÃO: MOVTO P/ DEFINIÇÃO DE PRIORIDADE NA PRODUÇÃO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX004.
           COPY COPX040.
      *    COPY MTPX021.
           COPY MTPX021D.
           SELECT  MTD021 ASSIGN TO PATH-MTD021
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-MTD021
                   RECORD KEY IS CONTRATO-MT21
                   ALTERNATE RECORD KEY IS CHAVE-PRI-MT21 =
                                           CAMPANHA-MT21
                                           PRIORIDADE-MT21
                                           CONTRATO-MT21.
           COPY MTPX024.
           COPY LBPX027.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY COPW004.
       COPY COPW040.
      *COPY MTPW021.

      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO
       FD  MTD021.
       01  REG-MTD021.
           05  CONTRATO-MT21      PIC 9(4).
           05  PRIORIDADE-MT21    PIC 999.
           05  PRODUTO-MT21       PIC 9(2).
           05  OBS-MT21           PIC X(40).
           05  ORDEM-MT21         PIC 9(4).
           05  CAMPANHA-MT21      PIC 9(2).

      *COPY MTPW021D.

      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO (ANOTAÇÕES)
       FD  MTD021D.
       01  REG-MTD021D.
           05 CHAVE-MT21D.
              10 CONTRATO-MT21D      PIC 9(04).
              10 DATA-MT21D          PIC 9(08).
              10 SEQUENCIA-MT21D     PIC 9(03).
           05 ANOTACOES-MT21D        PIC X(791).
           05 USUARIO-MT21D          PIC X(05).
           05 HORA-MT21D             PIC 9(04).

       COPY MTPW024.
       COPY LBPW027.
       COPY LOGW002.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP021.CPB".
           COPY "MTP021.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-MTD021             PIC XX       VALUE SPACES.
           05  ST-MTD021D            PIC XX       VALUE SPACES.
           05  ST-MTD024             PIC XX       VALUE SPACES.
           05  ST-COD004             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  MASC-ORDEM            PIC ZZZ9     VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  CAMPANHA-W            PIC 9(02)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.

           05  AUX-DATA              PIC 9(08)    VALUE ZEROS.
           05  AUX-SEQUENCIA         PIC 9(03)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 DET-ANOTACOES.
          05 DET-DIA                   PIC 99/.
          05 DET-MES                   PIC 99/.
          05 DET-ANO                   PIC 9999.
          05 FILLER                    PIC X(01).
          05 DET-SEQ                   PIC ZZ9.
          05 FILLER                    PIC X(01).
          05 DET-HORA                  PIC 9(02).
          05 FILLER                    PIC X(01) VALUE ":".
          05 DET-MINU                  PIC 9(02).
          05 FILLER                    PIC X(02).
          05 DET-USUARIO               PIC X(05).
          05 FILLER                    PIC X(01).
          05 DET-ANOTACAO              PIC X(76).

       01 DET-ANOTACOES2.
          05 FILLER                    PIC X(028).
          05 DET-ANOTACAO2             PIC X(76).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "CONF.DEFINICAO DA PRIORIDADE PRODUCAO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "PRI   CONT   TAMANHO          OBSERVAÇÃO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

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

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004
           MOVE "COD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD004
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD027"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD027.
           MOVE "MTD021"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021.
           MOVE "MTD021D" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021D.
           MOVE "MTD024"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD024.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   MTD021  LOG002 MTD024 LBD027 CAD004 MTD021D
           CLOSE              MTD021 MTD024 LBD027 CAD004 MTD021D
           OPEN I-O   MTD024  MTD021D MTD021
           OPEN INPUT COD040  COD004 LBD027 CAD004

           IF ST-MTD021 = "35"
              CLOSE MTD021      OPEN OUTPUT MTD021
              CLOSE MTD021      OPEN I-O    MTD021
           END-IF.
           IF ST-MTD021D = "35"
              CLOSE MTD021D     OPEN OUTPUT MTD021D
              CLOSE MTD021D     OPEN I-O    MTD021D
           END-IF.
           IF ST-MTD024 = "35"
              CLOSE MTD024      OPEN OUTPUT MTD024
              CLOSE MTD024      OPEN I-O    MTD024
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O    LOG002
           END-IF.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD004 <> "00"
              MOVE "ERRO ABERTURA COD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-COD004 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021 <> "00"
              MOVE "ERRO ABERTURA MTD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021D <> "00"
              MOVE "ERRO ABERTURA MTD021D: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021D TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD024 <> "00"
              MOVE "ERRO ABERTURA MTD024: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD024 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD021 LOG002 MTD021D
           OPEN INPUT MTD021 MTD021D

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP021"            to logacess-programa
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

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                      MOVE CAMPANHA-CO40 TO CAMPANHA-MT24
                      READ MTD024 INVALID KEY
                          MOVE GS-ORDEM TO PRIORIDADE-MT24
                          WRITE REG-MTD024
                          END-WRITE
                      NOT INVALID KEY
                          MOVE GS-ORDEM TO PRIORIDADE-MT24
                          REWRITE REG-MTD024
                          END-REWRITE
                      END-READ
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
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE FUNCTION NUMVAL(GS-LINDET(6:4)) TO
                                        CONTRATO-MT21
                   PERFORM CARREGAR-DADOS
                   PERFORM VERIFICAR-PERMISSOES
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
      *        WHEN GS-VERIFICA-PRIORID-TRUE
      *            PERFORM VERIFICA-PRIORIDADE
               WHEN GS-VERIFICA-CONTRATO-TRUE
                   PERFORM VERIFICA-CONTRATO
      *        WHEN GS-ZERAR-PRIORIDADE-TRUE
      *            PERFORM ZERAR-PRIORIDADE
               WHEN GS-LE-PRODUTO-TRUE
                   PERFORM LER-PRODUTO
               WHEN GS-POPUP-PRODUTO-TRUE
                   PERFORM POPUP-PRODUTO
               WHEN GS-CARREGA-ANOTACAO-TRUE
                   PERFORM CARREGAR-ANOTACOES
               WHEN GS-SALVAR-ANOTACAO-TRUE
                   PERFORM SALVAR-ANOTACOES
                   PERFORM CARREGAR-ANOTACOES
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

           PERFORM VERIFICAR-PERMISSOES.

       SALVAR-ANOTACOES SECTION.
           CLOSE      MTD021D
           OPEN I-O   MTD021D

           INITIALIZE REG-MTD021D
                      AUX-SEQUENCIA
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           STRING GS-DATA-ANOT(5:4) GS-DATA-ANOT(3:2) GS-DATA-ANOT(1:2)
             INTO DATA-MT21D
           MOVE DATA-MT21D             TO AUX-DATA
           MOVE ALL "9"                TO SEQUENCIA-MT21D

           START MTD021D KEY IS LESS THAN CHAVE-MT21D NOT INVALID KEY
                 READ MTD021D PREVIOUS NOT AT END
                      IF GS-CONTRATO = CONTRATO-MT21D AND
                         AUX-DATA    = DATA-MT21D
                         MOVE SEQUENCIA-MT21D TO AUX-SEQUENCIA.

           ADD 1                       TO AUX-SEQUENCIA

           INITIALIZE REG-MTD021D
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           STRING GS-DATA-ANOT(5:4) GS-DATA-ANOT(3:2) GS-DATA-ANOT(1:2)
             INTO DATA-MT21D
           MOVE AUX-SEQUENCIA           TO SEQUENCIA-MT21D
           MOVE GS-ANOTACAO             TO ANOTACOES-MT21D
           MOVE USUARIO-W               TO USUARIO-MT21D
           ACCEPT WS-HORA-SYS FROM TIME
           STRING WS-HO-SYS WS-MI-SYS INTO HORA-MT21D

           WRITE REG-MTD021D INVALID KEY
                MOVE "Erro de Gravação...MTD021D" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM 140-EXIBIR-MENSAGEM.

           CLOSE      MTD021D
           OPEN INPUT MTD021D.

       CARREGAR-ANOTACOES SECTION.

           CLEAR-OBJECT LB2

           INITIALIZE REG-MTD021D
                      GS-CONT
                      GS-ANOTACAO
                      GS-DATA-ANOT

           MOVE GS-CONTRATO             TO CONTRATO-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF GS-CONTRATO <> CONTRATO-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        ADD 1                       TO GS-CONT

                        MOVE DATA-MT21D(1:4)        TO DET-ANO
                        MOVE DATA-MT21D(5:2)        TO DET-MES
                        MOVE DATA-MT21D(7:2)        TO DET-DIA

                        MOVE SEQUENCIA-MT21D        TO DET-SEQ

                        MOVE HORA-MT21D(1:2)        TO DET-HORA
                        MOVE HORA-MT21D(3:2)        TO DET-MINU

                        MOVE USUARIO-MT21D          TO DET-USUARIO

                        MOVE ANOTACOES-MT21D(1:76)  TO DET-ANOTACAO
                        MOVE DET-ANOTACOES          TO GS-LINDET
                        MOVE "INSERIR-ANOT"         TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM

                        IF ANOTACOES-MT21D(77:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(77:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(153:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(153:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(229:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(229:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(305:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(305:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(381:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(381:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(457:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(457:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(533:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(533:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(609:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(609:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(685:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(685:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(761:30) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(761:30)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           move function current-date to ws-data-sys
           string ws-dia-cpu ws-mes-cpu ws-ano-cpu into gs-data-anot

           REFRESH-OBJECT WIN1.

       LER-PRODUTO SECTION.
           MOVE GS-PRODUTO              TO CODIGO-LB27
           READ LBD027 INVALID KEY
                MOVE "******"           TO DESCRICAO-LB27.

           MOVE DESCRICAO-LB27          TO GS-TAMANHO.
       POPUP-PRODUTO SECTION.
           CALL   "LBP027T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "LBP027T"
           MOVE PASSAR-STRING-1(33: 2) TO GS-PRODUTO
           MOVE PASSAR-STRING-1(1: 20) TO GS-TAMANHO.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *ZERAR-PRIORIDADE SECTION.
      *    CLOSE       MTD021.
      *    OPEN OUTPUT MTD021.
      *    CLOSE       MTD021.
      *    OPEN I-O    MTD021.
      *    MOVE 1 TO   ULT-SEQ
      *                GS-PRIORIDADE.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
                    MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO
           END-EVALUATE
           PERFORM LE-CONTRATO.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE "******"      TO IDENTIFICACAO-CO40
                MOVE ZEROS         TO CAMPANHA-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO
           MOVE CAMPANHA-CO40      TO CODIGO-CO04
                                      GS-CAMPANHA
           READ COD004 INVALID KEY
                MOVE ZEROS TO GS-DTINI
                              GS-DTFIM
                              GS-CAMPANHA
           NOT INVALID KEY
               MOVE DATA-INI-CO04 TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV      TO GS-DTINI

               MOVE DATA-FIM-CO04 TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV      TO GS-DTFIM

           END-READ

           IF GS-ORDEM = 0
              INITIALIZE REG-MTD024
              MOVE CAMPANHA-CO40  TO CAMPANHA-MT24
              READ MTD024 NOT INVALID KEY
                   MOVE PRIORIDADE-MT24 TO GS-ORDEM
              END-READ
              ADD 1 TO GS-ORDEM.

       VERIFICA-CONTRATO SECTION.
           MOVE GS-CONTRATO    TO CONTRATO-MT21.
           READ MTD021 INVALID KEY
                MOVE 0 TO GS-TIPO-GRAVACAO
           NOT INVALID KEY
                EVALUATE PRIORIDADE-MT21
                    WHEN 0 MOVE "0-Nao Recebido"   TO GS-PRIORIDADE
                    WHEN 1 MOVE "1-Distribuicao"   TO GS-PRIORIDADE
                    WHEN 2 MOVE "2-Photoshop"      TO GS-PRIORIDADE
                    WHEN 3 MOVE "3-Correcao"       TO GS-PRIORIDADE
                    WHEN 4 MOVE "4-Impressao"      TO GS-PRIORIDADE
                    WHEN 5 MOVE "5-Serv. Externo"  TO GS-PRIORIDADE
                    WHEN 6 MOVE "6-Montagem"       TO GS-PRIORIDADE
                    WHEN 7 MOVE "7-Finalizado"     TO GS-PRIORIDADE
                    WHEN OTHER MOVE SPACES         TO GS-PRIORIDADE
                END-EVALUATE
                MOVE PRODUTO-MT21                  TO GS-PRODUTO
                                                      CODIGO-LB27
                READ LBD027 INVALID KEY
                     MOVE "*********"              TO DESCRICAO-LB27
                END-READ
                MOVE DESCRICAO-LB27                TO GS-TAMANHO

                MOVE OBS-MT21                      TO GS-OBS
                MOVE ORDEM-MT21                    TO GS-ORDEM

                MOVE GS-CONTRATO                   TO NR-CONTRATO-CO40
                READ COD040 INVALID KEY
                     INITIALIZE REG-COD040
                END-READ

                MOVE CAMPANHA-CO40                 TO CODIGO-CO04
                                                      GS-CAMPANHA
                READ COD004 INVALID KEY
                     MOVE ZEROS                    TO GS-DTINI
                                                      GS-DTFIM
                                                      GS-CAMPANHA
                NOT INVALID KEY
                    MOVE DATA-INI-CO04             TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV                  TO GS-DTINI

                    MOVE DATA-FIM-CO04             TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV                  TO GS-DTFIM

                END-READ

                MOVE 1                          TO GS-TIPO-GRAVACAO
           END-READ.
      *VERIFICA-PRIORIDADE SECTION.
      *    MOVE GS-PRIORIDADE TO PRIORIDADE-MT21.
      *    START MTD021 KEY IS = PRIORIDADE-MT21 INVALID KEY
      *       MOVE 0 TO GS-PRIORIDADE-EXISTE
      *      NOT INVALID KEY
      *       MOVE 1         TO GS-PRIORIDADE-EXISTE
      *       MOVE ULT-SEQ   TO GS-PRIORIDADE
      *    END-START.
      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD021 MTD021D
           OPEN I-O MTD021 MTD021D LOG002
           DELETE MTD021 NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG2-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG2-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG2-HORAS
                  MOVE "E"         TO LOG2-OPERACAO
                  MOVE "MTD021"    TO LOG2-ARQUIVO
                  MOVE "MTP021"    TO LOG2-PROGRAMA
                  MOVE REG-MTD021  TO LOG2-REGISTRO
                  WRITE REG-LOG002
                  END-WRITE
           END-DELETE

           INITIALIZE REG-MTD021D
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF GS-CONTRATO <> CONTRATO-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        DELETE MTD021D INVALID KEY
                            MOVE "Erro de Exclusão...MTD021" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM 140-EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           CLOSE      MTD021 MTD021D LOG002
           OPEN INPUT MTD021 MTD021D
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START MTD021 KEY IS = CONTRATO-MT21 INVALID KEY
                 CONTINUE.

           READ MTD021 INVALID KEY
                INITIALIZE REG-MTD021.

           MOVE CONTRATO-MT21        TO GS-CONTRATO
                                        NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO

           EVALUATE PRIORIDADE-MT21
               WHEN 0 MOVE "0-Nao Recebido"   TO GS-PRIORIDADE
               WHEN 1 MOVE "1-Distribuicao"   TO GS-PRIORIDADE
               WHEN 2 MOVE "2-Photoshop"      TO GS-PRIORIDADE
               WHEN 3 MOVE "3-Correcao"       TO GS-PRIORIDADE
               WHEN 4 MOVE "4-Impressao"      TO GS-PRIORIDADE
               WHEN 5 MOVE "5-Serv. Externo"  TO GS-PRIORIDADE
               WHEN 6 MOVE "6-Montagem"       TO GS-PRIORIDADE
               WHEN 7 MOVE "7-Finalizado"     TO GS-PRIORIDADE
               WHEN OTHER MOVE SPACES         TO GS-PRIORIDADE
           END-EVALUATE

           MOVE CAMPANHA-CO40                 TO CODIGO-CO04
                                                 GS-CAMPANHA
           READ COD004 INVALID KEY
                MOVE ZEROS                    TO GS-DTINI
                                                 GS-DTFIM
                                                 GS-CAMPANHA
           NOT INVALID KEY
               MOVE DATA-INI-CO04             TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV                  TO GS-DTINI

               MOVE DATA-FIM-CO04             TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV                  TO GS-DTFIM

           END-READ


           MOVE PRODUTO-MT21         TO GS-PRODUTO
                                        CODIGO-LB27
           READ LBD027 INVALID KEY
                MOVE "**********"    TO DESCRICAO-LB27
           END-READ
           MOVE DESCRICAO-LB27       TO GS-TAMANHO

           MOVE ORDEM-MT21           TO GS-ORDEM
           MOVE OBS-MT21             TO GS-OBS.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-MTD021
           MOVE GS-CONTRATO TO CONTRATO-W
           MOVE GS-CAMPANHA TO CAMPANHA-W
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE CONTRATO-W TO GS-CONTRATO
           MOVE CAMPANHA-W TO GS-CAMPANHA
           REFRESH-OBJECT PRINCIPAL.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-MT21
           IF GS-PRIORIDADE <> SPACES
              MOVE FUNCTION NUMVAL(GS-PRIORIDADE(1:3))
                TO PRIORIDADE-MT21
           ELSE
              MOVE 9 TO PRIORIDADE-MT21
           END-IF
           MOVE GS-PRODUTO            TO PRODUTO-MT21
           MOVE GS-OBS                TO OBS-MT21
           MOVE GS-ORDEM              TO ORDEM-MT21
           MOVE GS-CAMPANHA           TO CAMPANHA-MT21.

       GRAVA-DADOS SECTION.
           CLOSE    MTD021
           OPEN I-O MTD021 LOG002
           MOVE ZEROS TO ST-MTD021.
           WRITE REG-MTD021 INVALID KEY
                 MOVE "Erro-Contrato ou Prior.ja existente"
                     TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(35: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "MTD021"    TO LOG2-ARQUIVO
                 MOVE "MTP021"    TO LOG2-PROGRAMA
                 MOVE REG-MTD021  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 PERFORM MOVER-DADOS-LISTA
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

           CLOSE      MTD021
           OPEN INPUT MTD021 LOG002.
       REGRAVA-DADOS SECTION.
           CLOSE    MTD021
           OPEN I-O MTD021 LOG002
           REWRITE REG-MTD021 INVALID KEY
                 MOVE "Erro Regravacao MTD021" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "A"         TO LOG2-OPERACAO
                 MOVE "MTD021"    TO LOG2-ARQUIVO
                 MOVE "MTP021"    TO LOG2-PROGRAMA
                 MOVE REG-MTD021  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 PERFORM MOVER-DADOS-LISTA
                 PERFORM LIMPAR-DADOS
                 PERFORM CARREGA-ULTIMOS.

           CLOSE      MTD021 LOG002
           OPEN INPUT MTD021 .

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-MTD021
           MOVE GS-CAMPANHA      TO CAMPANHA-MT21
           MOVE ZEROS            TO PRIORIDADE-MT21
           START MTD021 KEY IS NOT < CHAVE-PRI-MT21 INVALID KEY
                  MOVE "10" TO ST-MTD021.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-MTD021 = "10"
                 READ MTD021 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD021
                 NOT AT END
                      IF GS-CAMPANHA <> CAMPANHA-MT21
                         MOVE "10" TO ST-MTD021
                      ELSE
                         PERFORM MOVER-DADOS-LISTA
                         MOVE "INSERE-LIST"   TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM.

       VERIFICAR-PERMISSOES SECTION.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA40"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF3
           NOT INVALID KEY
                ENABLE-OBJECT PB3
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA54"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT PB3
           NOT INVALID KEY
                ENABLE-OBJECT EF3
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA41"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT SB1
           NOT INVALID KEY
                ENABLE-OBJECT SB1
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA42"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF2
           NOT INVALID KEY
                IF GS-PRIORIDADE = SPACES
                   ENABLE-OBJECT EF2
                ELSE
                   DISABLE-OBJECT EF2
                END-IF
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA43"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF4
           NOT INVALID KEY
                IF GS-PRIORIDADE = SPACES
                   ENABLE-OBJECT EF4
                ELSE
                   DISABLE-OBJECT EF4
                END-IF
           END-READ.

       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE ORDEM-MT21         TO MASC-ORDEM
           MOVE MASC-ORDEM         TO GS-LINDET(1:4)
           MOVE CONTRATO-MT21      TO GS-LINDET(6:4)
                                      NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE ZEROS         TO CAMPANHA-CO40.

           MOVE CAMPANHA-CO40      TO CODIGO-CO04
           READ COD004 INVALID KEY
                MOVE ZEROS         TO DATA-INI-CO04
                MOVE ZEROS         TO DATA-FIM-CO04.

           MOVE DATA-INI-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DTINI

           MOVE DATA-FIM-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DTFIM

           STRING CODIGO-CO04 " - " GS-DTINI(1:2) "/" GS-DTINI(3:2) "/"
                                    GS-DTINI(5:4) " ATÉ " GS-DTFIM(1:2)
                                    "/" GS-DTFIM(3:2) "/" GS-DTFIM(5:4)
                                 INTO GS-LINDET(48:30)

           EVALUATE PRIORIDADE-MT21
               WHEN 0 MOVE "0-Não Recebido"  TO GS-LINDET(11:16)
               WHEN 1 MOVE "1-Distribuição"  TO GS-LINDET(11:16)
               WHEN 2 MOVE "2-Photoshop"     TO GS-LINDET(11:16)
               WHEN 3 MOVE "3-Correção"      TO GS-LINDET(11:16)
               WHEN 4 MOVE "4-Impressão"     TO GS-LINDET(11:16)
               WHEN 5 MOVE "5-Serv. Externo" TO GS-LINDET(11:16)
               WHEN 6 MOVE "6-Montagem"      TO GS-LINDET(11:16)
               WHEN 7 MOVE "7-Finalizado"    TO GS-LINDET(11:16)
               WHEN OTHER MOVE SPACES        TO GS-LINDET(11:16)
           END-EVALUATE

           MOVE PRODUTO-MT21           TO CODIGO-LB27
           READ LBD027 INVALID KEY
                MOVE "********"        TO DESCRICAO-LB27
           END-READ
           MOVE DESCRICAO-LB27         TO GS-LINDET(31: 16)
           MOVE OBS-MT21               TO GS-LINDET(79: 30)

           MOVE CONTRATO-MT21          TO CONTRATO-MT21D
           READ MTD021D INVALID KEY
                INITIALIZE REG-MTD021D
           END-READ.

           MOVE ANOTACOES-MT21D        TO GS-LINDET(109:100).
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP021" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.



           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE ZEROS TO PAG-W.
           MOVE ZEROS          TO CONTRATO-MT21
           START MTD021 KEY IS NOT LESS CONTRATO-MT21 INVALID KEY
                 MOVE "10" TO ST-MTD021.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-MTD021 = "10"
                 READ MTD021 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD021
                 NOT AT END
                      MOVE SPACES            TO LINDET-REL
                      PERFORM MOVER-DADOS-LISTA
                      MOVE GS-LINDET         TO LINDET-REL

                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
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

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 MTD021 MTD024 COD004 LBD027 CAD004 MTD021D.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP021"            to logacess-programa
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
