       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP019.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 28/06/2000.
      *FUNÇÃO: Movimento de FICHA DE IDENTIFICAÇÃO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass"
           AListview          is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY CGPX010.
           COPY CGPX011.
           COPY IEPX011.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX030.
           COPY COPX008.
           COPY MTPX025.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           COPY PARX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY CGPW010.
       COPY CGPW011.
       COPY IEPW011.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW030.
       COPY COPW008.
       COPY MTPW025.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGACESS.FD.
       COPY PARW001.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP019.CPB".
           COPY "MTP019.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(120).
       01  VARIAVEIS.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD030             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
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
           05  CIDADE-W              PIC 9(4)     VALUE ZEROS.
           05  NOME-CIDADE-W         PIC X(20)    VALUE SPACES.
           05  UF-W                  PIC XX       VALUE SPACES.
           05  CURSO-W               PIC 9(3)     VALUE ZEROS.
           05  NOME-CURSO-W          PIC X(20)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-MOTIVO            PIC 9(03).
           05  ACHEI                 PIC X(01).
           05  AUX-ORDEM             PIC 9(01).
           05  AUX-CONT              PIC 9(04).
           05  QTDE-FORMANDOS        PIC 9(05) VALUE ZEROS.
           05  AUX-INCLUSAO          PIC 9(01).
           05  TURMA-W               PIC X(03).
           05  TURNO-W               PIC X(10).
           05  DATA-DIA-I            PIC 9(8)  VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)  VALUE ZEROS.
           05  SENHA-COMP            PIC 9(4)  COMP-3.
           05  WS-STATUS-ANALISE     PIC 9(02) VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02) VALUE ZEROS.
           05  UMITEM                OBJECT REFERENCE VALUE NULL.
           05  UMOBJETO              OBJECT REFERENCE VALUE NULL.
           05  MASC-QTDE             PIC ZZZ.ZZ9.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(71)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(59)   VALUE
           "CONFERENCIA DA FICHA DE IDENTIFICACAO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(91)  VALUE
           "CONT-ALB. CURSO      CIDADE     NOME-FORMANDO
      -    "      FONE IDENT TUR TURNO".

       01  LINDET.
           05  LINDET-REL          PIC X(91)  VALUE SPACES.

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

       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.
       77 wsTexto                  pic x(255) value spaces.
       77 wsItem                   pic 9(009) comp-5 value zeros.

       01 lnktabela.
          02 lnkobjetoscol             object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela2.
          02 lnkobjetoscol2            object reference occurs 99 times.
       01 lnktabelaCol2.
          02 lnkcolunas2   pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnkusu.
          copy usuario.cpy.


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
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD030.
           MOVE "MTD025" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD008" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD008.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                       ARQUIVO-LOGACESS
           OPEN I-O   MTD019 MTD025 LOG001 LOG002 MTD030 MTD020
           CLOSE      MTD025 MTD030 MTD020
           OPEN I-O   MTD025 MTD030 MTD020
           CLOSE      MTD030 MTD020
           OPEN INPUT IED011 CAD010 COD040 CAD004 COD008 CGD010 CGD011
                      PAR001 COD060 COD003 MTD030 MTD020
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD025 = "35"
              CLOSE MTD025      OPEN OUTPUT MTD025
              CLOSE MTD025      OPEN I-O MTD025
           END-IF.
           IF ST-MTD030 = "35"
              CLOSE MTD030      OPEN OUTPUT MTD030
              CLOSE MTD030      OPEN I-O MTD030
           END-IF.
           IF ST-LOG001 = "35"
              CLOSE LOG001      OPEN OUTPUT LOG001
              CLOSE LOG001      OPEN I-O LOG001
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD025 <> "00"
              MOVE "ERRO ABERTURA MTD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD030 <> "00"
              MOVE "ERRO ABERTURA MTD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           close      mtd019 mtd025 log001 log002
           open input mtd019 mtd025

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP019"            to logacess-programa
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


           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
                   PERFORM CRIAR-LISTVIEW2
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   MOVE GS-CONT TO AUX-CONT
                   PERFORM GRAVAR-MOTIVOS
                   MOVE AUX-CONT TO GS-CONT
                   IF GS-TIPO-GRAVACAO = 1
                      MOVE 0 TO GS-INCLUSAO
                      PERFORM REGRAVA-DADOS
                   ELSE
                      MOVE 1 TO GS-INCLUSAO
                      PERFORM GRAVA-DADOS
                   END-IF
      ******************************************************************
      *Comentado no dia 20/06/2011 => Pedido pelo Anderson
      *            PERFORM ATUALIZAR-COD040
      ******************************************************************
                   PERFORM LIMPAR-DADOS
      *            PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
                   MOVE AUX-CONT TO GS-CONT
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM VALIDAR-MTD020
                   IF ACHEI = "N"
                      PERFORM EXCLUI
      ******************************************************************
      *Comentado no dia 20/06/2011 => Pedido pelo Anderson
      *            PERFORM ATUALIZAR-COD040
      ******************************************************************
                   END-IF
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-TODOS-TRUE
                   PERFORM VERIFICA-AUTORIZACAO
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
                   MOVE GS-LINDET(1: 4)  TO CONTRATO-MT19
                   MOVE GS-LINDET(6: 4)  TO SEQ-MT19
                   MOVE "SENHA25A" TO PROGRAMA-CA004
                   MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                   READ CAD004 INVALID KEY
                        MOVE "DESABILITA-ALTERACAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   NOT INVALID KEY
                        MOVE "HABILITA-ALTERACAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
                   MOVE "SENHA25E" TO PROGRAMA-CA004
                   MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                   READ CAD004 INVALID KEY
                        MOVE "DESABILITA-EXCLUSAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   NOT INVALID KEY
                        MOVE "HABILITA-EXCLUSAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-LE-MOTIVO-TRUE
                   PERFORM LE-MOTIVO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-LEITURA-TRANSF-TRUE
                    PERFORM LEITURA-TRANSFERE
               WHEN GS-POPUP-TRANSF-TRUE
                    PERFORM POPUP-TRANSFERE
               WHEN GS-TRANSFERE-ALBUM-TRUE
                    PERFORM TRANSFERE-ALBUM
               WHEN GS-VALIDA-ACESSO-TRUE
                    PERFORM VALIDA-ACESSO
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR-MOTIVO
               WHEN GS-CARREGAR-MOTIVOS-TRUE
                    PERFORM CARREGAR-MOTIVOS
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
               WHEN GS-CARREGAR-EVENTOS-TRUE
                    PERFORM CARREGAR-EVENTOS
               WHEN GS-INSERIR-QTDE-TRUE
                    PERFORM INSERIR-EVENTO
               WHEN GS-TRATAR-EVENTO-TRUE
                    EVALUATE GS-QUAL-LIST
                       WHEN 1 PERFORM TRATAR-EVENTO1
                       WHEN 2 PERFORM TRATAR-EVENTO2
                    END-EVALUATE
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VALIDAR-MTD020 SECTION.
           MOVE "N"            TO ACHEI
           MOVE CONTRATO-MT19  TO CONTRATO-MTG
           MOVE SEQ-MT19       TO NRALBUM-MTG
           READ MTD020 NOT INVALID KEY
                MOVE SPACES    TO MENSAGEM
                MOVE "Álbum com produtos lançados MTP020"
                  TO MENSAGEM
                MOVE "C"       TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE "S"       TO ACHEI.

       INSERIR-EVENTO SECTION.
           IF GS-ACP-QTDE-IMAGEM = 0
              MOVE "Quantidade de imagens não informada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              SET-FOCUS EF-QTDE-IMAGEM
           ELSE
              CLOSE    MTD030
              OPEN I-O MTD030
              MOVE GS-ACP-QTDE-IMAGEM TO QTDE-IMAGENS-MT30
              WRITE REG-MTD030 INVALID KEY
                    REWRITE REG-MTD030 INVALID KEY
                            MOVE "Erro de Gravação...MTD030" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                    NOT INVALID KEY
                            PERFORM OK-EVENTO
                    END-REWRITE
              NOT INVALID KEY
                    PERFORM OK-EVENTO
              END-WRITE
              CLOSE      MTD030
              OPEN INPUT MTD030.

       OK-EVENTO SECTION.
           INITIALIZE INDICE
           INVOKE GS-ACP-LISTVIEW-EVE "ADICIONARITEM"
                  RETURNING WSITEM

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           MOVE GS-ACP-QTDE-IMAGEM  TO MASC-QTDE
           STRING MASC-QTDE X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           MOVE COD-EVENTO-MT30     TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE "*******"      TO NOME-CO03
           END-READ

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING NOME-CO03 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING COD-EVENTO-MT30 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           ADD 1 TO INDICE
           INITIALIZE WSTEXTO
           STRING ITEM-EVENTO-MT30 X"00" INTO WSTEXTO
           INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
            USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

           PERFORM MOSTRAR-COLUNAS-FAVO2
           PERFORM MOSTRAR-FONTE-FAVO2
           PERFORM ZEBRAR-ITENS2.

       TRATAR-EVENTO1 SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34592  PERFORM EVENTO-ACEITO
               WHEN 34013  PERFORM EVENTO-ACEITO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO
               WHEN 34027  UNSHOW-WINDOW WIN4 PRINCIPAL
                           SET-FOCUS EF6
           END-EVALUATE.

       EVENTO-ACEITO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INITIALIZE REG-MTD030
              MOVE GS-CONTRATO         TO CONTRATO-MT30
              MOVE GS-NR-ALBUM         TO SEQ-MT30

      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas(2) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              READ MTD030 INVALID KEY
                   MOVE ZEROS               TO QTDE-IMAGENS-MT30
              END-READ

              MOVE QTDE-IMAGENS-MT30        TO GS-ACP-QTDE-IMAGEM
              REFRESH-OBJECT WIN5
              SET-FOCUS EF-QTDE-IMAGEM.


       TRATAR-EVENTO2 SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34592  PERFORM ALTERAR-QTDE
               WHEN 34013  PERFORM ALTERAR-QTDE
               WHEN 34046  PERFORM EXCLUIR-EVENTO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVO2
               WHEN 34027  INVOKE GS-ACP-LISTVIEW "SetFocus"
           END-EVALUATE.

       ALTERAR-QTDE SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW-EVE "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
      *>Quantidade Imagens
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(1) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-ACP-QTDE-IMAGEM
      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(4) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              MOVE GS-CONTRATO         TO CONTRATO-MT30
              MOVE GS-NR-ALBUM         TO SEQ-MT30
              READ MTD030 INVALID KEY
                   MOVE "Não encontrei o MTD030" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   CLOSE      MTD030
                   OPEN I-O   MTD030

                   DELETE MTD030 INVALID KEY
                          MOVE "Erro de Exclusão...MTD030" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                          INVOKE UMITEM "Finalize" RETURNING UMITEM
                          SET-FOCUS EF-QTDE-IMAGEM
                   END-DELETE

                   CLOSE      MTD030
                   OPEN INPUT MTD030.

       EXCLUIR-EVENTO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW-EVE "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
      *>Código Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(3) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO COD-EVENTO-MT30
      *>Item Evento
              INVOKE UMITEM "GETCOLUMNVALUE"
                     USING lnkcolunas2(4) RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO ITEM-EVENTO-MT30

              MOVE GS-CONTRATO         TO CONTRATO-MT30
              MOVE GS-NR-ALBUM         TO SEQ-MT30
              READ MTD030 INVALID KEY
                   MOVE "Não encontrei o MTD030" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   CLOSE      MTD030
                   OPEN I-O   MTD030

                   DELETE MTD030 INVALID KEY
                          MOVE "Erro de Exclusão...MTD030" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                          INVOKE UMITEM "Finalize" RETURNING UMITEM
                          INVOKE GS-ACP-LISTVIEW "Setfocus"
                   END-DELETE

                   CLOSE      MTD030
                   OPEN INPUT MTD030.

       CARREGAR-EVENTOS SECTION.
           move gs-contrato    to contrato-mt19
           move gs-nr-album    to seq-mt19
           read mtd019 invalid key
                move "Ficha de identificação não encontrada" to mensagem
                move "C" to tipo-msg
                perform exibir-mensagem
           not invalid key
                invoke gs-acp-listview "DeleteAll"
                invoke gs-acp-listview-eve "DeleteAll"

                PERFORM CARREGAR-EVENTOS-ALBUM

                INITIALIZE REG-COD060
                MOVE CONTRATO-MT19 TO NR-CONTRATO-CO60
                START COD060 KEY IS NOT LESS CHAVE-CO60 INVALID KEY
                      MOVE "10" TO ST-COD060
                END-START
                PERFORM UNTIL ST-COD060 = "10"
                      READ COD060 NEXT AT END
                           MOVE "10" TO ST-COD060
                      NOT AT END
                           IF CONTRATO-MT19 <> NR-CONTRATO-CO60
                              MOVE "10" TO ST-COD060
                           ELSE
                              initialize indice
                              invoke gs-acp-listview "adicionarItem"
                                           returning wsItem

                              move codevento-co60 to codigo-co03
                              read cod003 invalid key
                                   move "*****"   to nome-co03
                              end-read

                              add 1 to indice
                              initialize wsTexto
                              string nome-co03 X"00" delimited by "  "
                                into wsTexto
                              invoke gs-acp-listview "preencherColunaZ"
                               using wsItem lnkcolunas(indice) wsTexto

                              add 1 to indice
                              initialize wsTexto
                              string codevento-co60 X"00"
                                     delimited by "  " into wsTexto
                              invoke gs-acp-listview "preencherColunaZ"
                               using wsItem lnkcolunas(indice) wsTexto

                              add 1 to indice
                              initialize wsTexto
                              string item-co60 X"00" delimited by "  "
                                into wsTexto
                              invoke gs-acp-listview "preencherColunaZ"
                               using wsItem lnkcolunas(indice) wsTexto
                           END-IF
                      END-READ
                END-PERFORM

                PERFORM MOSTRAR-COLUNAS-FAVO
                PERFORM MOSTRAR-FONTE-FAVO
                PERFORM ZEBRAR-ITENS

                show-window win4
                show-window win5
                invoke gs-acp-listview "Setfocus".

       CARREGAR-EVENTOS-ALBUM SECTION.
           INVOKE GS-ACP-LISTVIEW-EVE "DeleteAll"

           INITIALIZE REG-MTD030
           MOVE GS-CONTRATO        TO CONTRATO-MT30
           MOVE GS-NR-ALBUM        TO SEQ-MT30
           START MTD030 KEY IS NOT LESS CHAVE-MT30 INVALID KEY
                 MOVE "10" TO ST-MTD030.

           PERFORM UNTIL ST-MTD030 = "10"
                 READ MTD030 NEXT AT END
                      MOVE "10" TO ST-MTD030
                 NOT AT END
                      IF GS-CONTRATO  <> CONTRATO-MT30 OR
                         GS-NR-ALBUM  <> SEQ-MT30
                         MOVE "10" TO ST-MTD030
                      ELSE
                         INITIALIZE INDICE
                         INVOKE GS-ACP-LISTVIEW-EVE "ADICIONARITEM"
                                RETURNING WSITEM

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-IMAGENS-MT30 TO MASC-QTDE
                         STRING MASC-QTDE X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         MOVE COD-EVENTO-MT30     TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE "*******"      TO NOME-CO03
                         END-READ

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING NOME-CO03 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING COD-EVENTO-MT30 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING ITEM-EVENTO-MT30 X"00" INTO WSTEXTO
                         INVOKE GS-ACP-LISTVIEW-EVE "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNAS2(INDICE) WSTEXTO

                      END-IF
                 END-READ
           END-PERFORM

           PERFORM MOSTRAR-COLUNAS-FAVO2
           PERFORM MOSTRAR-FONTE-FAVO2
           PERFORM ZEBRAR-ITENS2.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Evento" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Código" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Item" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-mtp019-1" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-mtp019-1" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       zebrar-itens section.
           move "listview-mtp019-1" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-mtp019-1" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       CRIAR-LISTVIEW2 SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Qtde Imagens" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "RightJustified"
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Evento" returning lnkobjetoscol2(indice)
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Código" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "centered"
          move indice to lnkcolunas2(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview-eve "adicionarColunaZ"
            using z"Item" returning lnkobjetoscol2(indice)
          invoke lnkobjetoscol2(indice) "centered"
          move indice to lnkcolunas2(indice)

          perform mostrar-fonte-favo2
          perform mostrar-colunas-favo2

          invoke gs-acp-listview-eve "gridLines"
          invoke gs-acp-listview-eve "noBorder".

       mostrar-colunas-favo2 section.
          initialize wsTexto
          move "listview-mtp019-2" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview-eve
                                  wsTexto
                                  lnktabela2.
       mostrar-colunas-favo2-fim.
           exit.

       mostrar-fonte-favo2 section.
           move "listview-mtp019-2" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview-eve wsTexto.
       mostrar-fonte-favo2-fim.
           exit.

       zebrar-itens2 section.
           move "listview-mtp019-2" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview-eve wsTexto
           invoke gs-acp-listview-eve "redrawallitems".
       zebrar-itens2-fim.
           exit.

       chamar-colunas-favo2 section.
           move "listview-mtp019-2" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview-eve
                               wsTexto
                               lnktabela2

           perform mostrar-colunas-favo2
           perform mostrar-fonte-favo2
           perform zebrar-itens2.
       chamar-colunas-favo2-fim.
           exit.

       VERIFICA-CONTRATO SECTION.
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
                    WHEN WS-STATUS-ANALISE
                         MOVE "Contrato com STATUS Analise" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    WHEN OTHER
                         MOVE 0 TO GS-FLAG-CRITICA
                END-EVALUATE.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win4 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       ATUALIZAR-COD040 SECTION.
           INITIALIZE REG-MTD019
                      QTDE-FORMANDOS
           MOVE GS-CONTRATO            TO CONTRATO-MT19
           START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
                MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
                READ MTD019 NEXT AT END
                     MOVE "10" TO ST-MTD019
                NOT AT END
                     IF GS-CONTRATO <> CONTRATO-MT19
                        MOVE "10" TO ST-MTD019
                     ELSE
      *                 IF IDENTIFICADO-MT19 = 1
                           ADD 1 TO QTDE-FORMANDOS
      *                 END-IF
                     END-IF
                END-READ
           END-PERFORM

           CLOSE      COD040
           OPEN I-O   COD040

           MOVE GS-CONTRATO          TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Contrato Não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                IF QTDE-FORMANDOS > 0
                   MOVE QTDE-FORMANDOS     TO QTDE-FORM-CO40
                ELSE
                   MOVE QTDE-FORM-INI-CO40 TO QTDE-FORM-CO40
                END-IF
                REWRITE REG-COD040 INVALID KEY
                     MOVE "Erro de Regravação...COD040" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ

           CLOSE      COD040
           OPEN INPUT COD040.

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
                MOVE GS-CONTRATO     TO CONTRATO-MT25
                MOVE GS-NR-ALBUM     TO ALBUM-MT25
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

       VALIDA-ACESSO SECTION.
           MOVE "SENHA15" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
              MOVE "DESABILITA-BOTAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "IEP011T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "IEP011T"
                    MOVE PASSAR-STRING-1(43: 3) TO GS-CURSO
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-CURSO
             WHEN 2 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP010T"
                    MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE
                    PERFORM LE-CIDADE
             WHEN 3 CALL   "COP008T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP008T"
                    MOVE PASSAR-STRING-1(32: 3) TO GS-MOTIVO
                    MOVE PASSAR-STRING-1( 1:30) TO GS-DESC-MOTIVO
                    PERFORM LE-MOTIVO
           END-EVALUATE.
       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "********" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-NOME-CURSO.
       LE-CIDADE SECTION.
           MOVE GS-CIDADE          TO CIDADE.
           READ CAD010 INVALID KEY MOVE "********" TO NOME-COMPL-CID.
           MOVE NOME-COMPL-CID     TO GS-NOME-CIDADE.
           MOVE UF-CID             TO GS-UF.
       LE-MOTIVO SECTION.
           MOVE GS-MOTIVO          TO CODIGO-CO08.
           READ COD008 INVALID KEY
                MOVE "********" TO DESCRICAO-CO08.

           MOVE DESCRICAO-CO08     TO GS-DESC-MOTIVO.

       LEITURA-TRANSFERE SECTION.
           MOVE GS-CONTRATO-TRANS TO CONTRATO-MT19
           EVALUATE GS-OPCAO-TRANSF
             WHEN 1 MOVE GS-DO-ALBUM   TO SEQ-MT19
             WHEN 2 MOVE GS-PARA-ALBUM TO SEQ-MT19
           END-EVALUATE
           READ MTD019 INVALID KEY
             EVALUATE GS-OPCAO-TRANSF
               WHEN 1 MOVE "Não cadastrado"  TO GS-DO-ALBUM-DESC
               WHEN 2 MOVE "Não cadastrado"  TO GS-PARA-ALBUM-DESC
             END-EVALUATE
            NOT INVALID KEY
             EVALUATE GS-OPCAO-TRANSF
               WHEN 1 MOVE NOME-FORM-MT19  TO GS-DO-ALBUM-DESC
               WHEN 2 MOVE NOME-FORM-MT19  TO GS-PARA-ALBUM-DESC
             END-EVALUATE
           END-READ.
       POPUP-TRANSFERE SECTION.
           MOVE GS-CONTRATO-TRANS TO PASSAR-PARAMETROS(40: 4).
           CALL   "MTP019T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "MTP019T".
           EVALUATE GS-OPCAO-TRANSF
             WHEN 1 MOVE PASSAR-PARAMETROS(1: 30) TO GS-DO-ALBUM-DESC
                    MOVE PASSAR-PARAMETROS(45: 4) TO GS-DO-ALBUM
             WHEN 2 MOVE PASSAR-PARAMETROS(1: 30) TO GS-PARA-ALBUM-DESC
                    MOVE PASSAR-PARAMETROS(45: 4) TO GS-PARA-ALBUM
           END-EVALUATE.
       TRANSFERE-ALBUM SECTION.
      *    OS ALBUNS QUE ESTIVEREM COM A SEQUENCIA PULADA, DEVERÁ PEGAR
      *    OS ÚLTIMOS ALBUNS P/ COLOCAR NESTAS SEQUENCIAS, PORQUE
      *    NÃO PODE HAVER BURACOS NA SEQUENCIA

           CLOSE MTD019
           OPEN I-O MTD019 LOG002

           MOVE GS-CONTRATO-TRANS       TO CONTRATO-MT19
           MOVE GS-PARA-ALBUM            TO SEQ-MT19
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP019"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                END-DELETE
           END-READ

           MOVE GS-CONTRATO-TRANS       TO CONTRATO-MT19
           MOVE GS-DO-ALBUM             TO SEQ-MT19
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
               DELETE MTD019 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "E"         TO LOG2-OPERACAO
                   MOVE "MTD019"    TO LOG2-ARQUIVO
                   MOVE "MTP019"    TO LOG2-PROGRAMA
                   MOVE REG-MTD019  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
               END-DELETE
               MOVE GS-CONTRATO-TRANS   TO CONTRATO-MT19
               MOVE GS-PARA-ALBUM       TO SEQ-MT19
               WRITE REG-MTD019 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "MTD019"    TO LOG2-ARQUIVO
                   MOVE "MTP019"    TO LOG2-PROGRAMA
                   MOVE REG-MTD019  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
               END-WRITE
           END-READ.

           CLOSE MTD019 LOG002
           OPEN INPUT MTD019

           PERFORM CARREGA-ULTIMOS.
      *----------------------------------------------------------------

       VERIFICA-AUTORIZACAO SECTION.
           MOVE "SIM"                     TO GS-PERMISSAO.
           MOVE GS-SENHA                  TO SENHA-COMP
           IF GS-ALBUM1 > GS-ALBUM2
              MOVE "Intervalo Incorreto"  TO GS-AUTORIZACAO
              MOVE "NAO"                  TO GS-PERMISSAO
           ELSE
              IF SENHA-COMP NOT = SENHA-W
                 MOVE "Senha inválida"    TO GS-AUTORIZACAO

                 MOVE "NAO"               TO GS-PERMISSAO
              ELSE
                 PERFORM EXCLUI-TODOS-ALBUNS
              END-IF
           END-IF.
      *----------------------------------------------------------------
       EXCLUI-TODOS-ALBUNS SECTION.
           CLOSE MTD019
           OPEN I-O MTD019 LOG002
           MOVE GS-CONTRATO    TO CONTRATO-MT19
           MOVE GS-ALBUM1      TO SEQ-MT19
           START MTD019 KEY IS NOT < ALBUM-MT19
                    INVALID KEY MOVE "10" TO ST-MTD019
           END-START

           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
                IF CONTRATO-MT19 <> GS-CONTRATO OR
                   SEQ-MT19 > GS-ALBUM2
                       MOVE "10" TO ST-MTD019
                ELSE
                   DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP019"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                   END-DELETE
                END-IF
              END-READ
           END-PERFORM

           CLOSE MTD019 LOG002
           OPEN INPUT MTD019.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD019 MTD030
           OPEN I-O MTD019 LOG002 MTD030

           MOVE GS-CONTRATO    TO ALBUM-MT19(1: 4)
           MOVE GS-NR-ALBUM    TO ALBUM-MT19(5: 4).
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP019"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                       INITIALIZE REG-MTD030
                       MOVE CONTRATO-MT19 TO CONTRATO-MT30
                       MOVE SEQ-MT19      TO SEQ-MT30
                       START MTD030 KEY IS NOT LESS CHAVE-MT30 INVALID
                                                                   KEY
                            MOVE "10" TO ST-MTD030
                       END-START
                       PERFORM UNTIL ST-MTD030 = "10"
                            READ MTD030 NEXT AT END
                                 MOVE "10" TO ST-MTD030
                            NOT AT END
                                 IF CONTRATO-MT19 <> CONTRATO-MT30 OR
                                    SEQ-MT19      <> SEQ-MT30
                                    MOVE "10" TO ST-MTD030
                                 ELSE
                                    DELETE MTD030 INVALID KEY
                                        MOVE "Erro de Exclusão...MTD030"
                                          TO MENSAGEM
                                        MOVE "C" TO TIPO-MSG
                                        PERFORM EXIBIR-MENSAGEM
                                    END-DELETE
                                 END-IF
                            END-READ
                       END-PERFORM
                END-DELETE
           END-READ.

           CLOSE      MTD019 LOG002 MTD030
           OPEN INPUT MTD019 MTD030

           PERFORM EXCLUIR-MOTIVOS
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.

       EXCLUIR-MOTIVOS SECTION.
           CLOSE    MTD025
           OPEN I-O MTD025 LOG001

           INITIALIZE REG-MTD025
           MOVE GS-CONTRATO    TO CONTRATO-MT25
           MOVE GS-NR-ALBUM    TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 <> "10"
               READ MTD025 NEXT AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-MT25 OR
                      GS-NR-ALBUM <> ALBUM-MT25
                      MOVE "10" TO ST-MTD025
                   ELSE
                      DELETE MTD025 NOT INVALID KEY
                             MOVE USUARIO-W   TO LOG1-USUARIO
                             MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                             MOVE WS-DATA-CPU TO LOG1-DATA
                             ACCEPT WS-HORA-SYS FROM TIME
                             MOVE WS-HORA-SYS TO LOG1-HORAS
                             MOVE "E"         TO LOG1-OPERACAO
                             MOVE "MTD025"    TO LOG1-ARQUIVO
                             MOVE "MTP019"    TO LOG1-PROGRAMA
                             MOVE REG-MTD025  TO LOG1-REGISTRO
                             WRITE REG-LOG001
                             END-WRITE
                      END-DELETE
                   END-IF
               END-READ
           END-PERFORM

           CLOSE      MTD025 LOG001
           OPEN INPUT MTD025.

       CARREGAR-DADOS SECTION.
           START MTD019 KEY IS = ALBUM-MT19 INVALID KEY
                 CONTINUE.

           READ MTD019 INVALID KEY
                INITIALIZE REG-MTD019.
           MOVE CONTRATO-MT19        TO  GS-CONTRATO
                                         NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO  GS-IDENTIFICACAO.
           MOVE SEQ-MT19             TO  GS-NR-ALBUM
           MOVE TURMA-MT19           TO  GS-TURMA
           MOVE TURNO-MT19           TO  GS-TURNO
           MOVE CURSO-MT19           TO  GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE "*****"         TO NOME-IE11.
           MOVE NOME-IE11            TO  GS-NOME-CURSO.
           MOVE UF-MT19              TO  GS-UF.
           MOVE CIDADE-MT19          TO  GS-CIDADE CIDADE.
           READ CAD010 INVALID KEY
                MOVE SPACES          TO NOME-COMPL-CID.
           MOVE NOME-COMPL-CID       TO  GS-NOME-CIDADE.
           MOVE NOME-FORM-MT19       TO  GS-NOME-FORMANDO.
           MOVE FONE-MT19            TO  GS-FONE.
           EVALUATE IDENTIFICADO-MT19
             WHEN 0 MOVE "0-Não"     TO  GS-IDENTIFICADO
             WHEN 1 MOVE "1-Sim"     TO  GS-IDENTIFICADO
           END-EVALUATE
           MOVE DATAMOV-MT19         TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-MOVTO

           MOVE 0                    TO CLASSIF-CG11
           STRING GS-CONTRATO GS-NR-ALBUM INTO CODIGO-CG11
           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011.

           EVALUATE SEXO2-CG11
               WHEN "F"   MOVE "Feminino"  TO GS-SEXO
               WHEN "M"   MOVE "Masculino" TO GS-SEXO
               WHEN OTHER MOVE SPACES      TO GS-SEXO
           END-EVALUATE.

       CARREGAR-MOTIVOS SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "N" TO ACHEI
           INITIALIZE REG-MTD025
           MOVE GS-CONTRATO TO CONTRATO-MT25
           MOVE GS-NR-ALBUM TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 = "10"
               READ MTD025 NEXT AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-MT25 OR GS-NR-ALBUM <>
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
              MOVE "INCLUIR-MOTIVO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO    TO DATAMOV-W
           MOVE GS-CONTRATO      TO CONTRATO-W
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-W
           MOVE GS-CIDADE        TO CIDADE-W
           MOVE GS-NOME-CIDADE   TO NOME-CIDADE-W
           MOVE GS-UF            TO UF-W
           MOVE GS-CURSO         TO CURSO-W
           MOVE GS-NOME-CURSO    TO NOME-CURSO-W
           MOVE GS-ORDER         TO AUX-ORDEM
           MOVE GS-CONT          TO AUX-CONT
           MOVE GS-INCLUSAO      TO AUX-INCLUSAO
           MOVE GS-TURMA         TO TURMA-W
           MOVE GS-TURNO         TO TURNO-W
           INITIALIZE REG-MTD019
           INITIALIZE GS-DATA-BLOCK
           MOVE AUX-CONT         TO GS-CONT
           MOVE AUX-INCLUSAO     TO GS-INCLUSAO

           MOVE AUX-ORDEM        TO GS-ORDER
           MOVE DATAMOV-W        TO GS-DATA-MOVTO
           MOVE CONTRATO-W       TO GS-CONTRATO
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO
           MOVE CIDADE-W         TO GS-CIDADE
           MOVE NOME-CIDADE-W    TO GS-NOME-CIDADE
           MOVE UF-W             TO GS-UF
           MOVE CURSO-W          TO GS-CURSO
           MOVE NOME-CURSO-W     TO GS-NOME-CURSO
           MOVE ULT-SEQ          TO GS-NR-ALBUM
           MOVE TURMA-W          TO GS-TURMA
           MOVE TURNO-W          TO GS-TURNO
           MOVE "0-Não"          TO GS-IDENTIFICADO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-MT19
           MOVE GS-NR-ALBUM           TO SEQ-MT19
           MOVE GS-CURSO              TO CURSO-MT19
           MOVE GS-TURMA              TO TURMA-MT19
           MOVE GS-TURNO              TO TURNO-MT19
           MOVE GS-NOME-FORMANDO      TO NOME-FORM-MT19
           MOVE GS-CIDADE             TO CIDADE-MT19
           MOVE GS-UF                 TO UF-MT19
           MOVE GS-FONE               TO FONE-MT19
           MOVE GS-IDENTIFICADO(1: 1) TO IDENTIFICADO-MT19
           MOVE GS-DATA-MOVTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATAMOV-MT19.

       GRAVA-DADOS SECTION.
           CLOSE    MTD019 CGD010 CGD011
           OPEN I-O MTD019 LOG002 CGD010 CGD011

           MOVE ZEROS TO ST-MTD019.
           PERFORM UNTIL ST-MTD019 = "10"
             WRITE REG-MTD019 INVALID KEY
                 ADD 1 TO SEQ-MT19
             NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "MTD019"    TO LOG2-ARQUIVO
                 MOVE "MTP019"    TO LOG2-PROGRAMA
                 MOVE REG-MTD019  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 MOVE "10" TO ST-MTD019.

           MOVE ZEROS      TO CLASSIF-CG10
           MOVE ALBUM-MT19 TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE ZEROS      TO CLASSIF-CG10
                MOVE ALBUM-MT19 TO CODIGO-CG10
                MOVE NOME-FORM-MT19 TO COMPRADOR-CG10
                WRITE REG-CGD010
                END-WRITE
           END-READ

           MOVE ZEROS      TO CLASSIF-CG11
           MOVE ALBUM-MT19 TO CODIGO-CG11
           READ CGD011 INVALID KEY
                MOVE GS-SEXO(1:1) TO SEXO2-CG11
                MOVE CIDADE-MT19  TO CIDADE2-CG11
                WRITE REG-CGD011
                END-WRITE
           NOT INVALID KEY
                MOVE GS-SEXO(1:1) TO SEXO2-CG11
                IF CIDADE2-CG11 = 0
                   MOVE CIDADE-MT19  TO CIDADE2-CG11
                END-IF
                REWRITE REG-CGD011
                END-REWRITE
           END-READ

           CLOSE      MTD019 LOG002 CGD010 CGD011
           OPEN INPUT MTD019 CGD010 CGD011


           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ    TO GS-NR-ALBUM.
       REGRAVA-DADOS SECTION.
           CLOSE    MTD019 CGD010 CGD011
           OPEN I-O MTD019 LOG002 CGD010 CGD011

           REWRITE REG-MTD019 INVALID KEY
                 MOVE "Erro Regravacao MTD019" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "A"         TO LOG2-OPERACAO
                 MOVE "MTD019"    TO LOG2-ARQUIVO
                 MOVE "MTP019"    TO LOG2-PROGRAMA
                 MOVE REG-MTD019  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE.

           MOVE ZEROS      TO CLASSIF-CG10
           MOVE ALBUM-MT19 TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE ZEROS      TO CLASSIF-CG10
                MOVE ALBUM-MT19 TO CODIGO-CG10
                MOVE NOME-FORM-MT19 TO COMPRADOR-CG10
                WRITE REG-CGD010
                END-WRITE
           END-READ

           MOVE ZEROS      TO CLASSIF-CG11
           MOVE ALBUM-MT19 TO CODIGO-CG11
           READ CGD011 INVALID KEY
                MOVE GS-SEXO(1:1) TO SEXO2-CG11
                MOVE CIDADE-MT19  TO CIDADE2-CG11
                WRITE REG-CGD011
                END-WRITE
           NOT INVALID KEY
                MOVE GS-SEXO(1:1) TO SEXO2-CG11
                IF CIDADE2-CG11 = 0
                   MOVE CIDADE-MT19  TO CIDADE2-CG11
                END-IF
                REWRITE REG-CGD011
                END-REWRITE
           END-READ

           CLOSE      MTD019 LOG002 CGD010 CGD011
           OPEN INPUT MTD019 CGD010 CGD011

           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ULT-SEQ  TO GS-NR-ALBUM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           initialize reg-mtd019

           MOVE GS-CONTRATO     TO CONTRATO-MT19 NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO.

           IF GS-ORDER = 1 MOVE ZEROS TO SEQ-MT19
              START MTD019 KEY IS NOT < ALBUM-MT19
                    INVALID KEY MOVE "10" TO ST-MTD019
              END-START
              MOVE ZEROS TO ULT-SEQ
           ELSE PERFORM ACHA-ULT-SEQUENCIA
              MOVE ZEROS TO CURSO-MT19
              MOVE SPACES TO NOME-FORM-MT19
              MOVE GS-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                    MOVE "10" TO ST-MTD019.

           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-MTD019 = "10"
                READ MTD019 NEXT RECORD AT END
                     MOVE "10" TO ST-MTD019
                NOT AT END
                     IF CONTRATO-MT19 <> GS-CONTRATO
                        MOVE "10" TO ST-MTD019
                     ELSE
                        IF GS-ORDER = 1
                           MOVE SEQ-MT19 TO ULT-SEQ
                        END-IF
                        PERFORM MOVER-DADOS-LISTA
                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ TO GS-NR-ALBUM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE CONTRATO-MT19      TO GS-LINDET(1: 5)
           MOVE SEQ-MT19           TO GS-LINDET(6: 5)
           MOVE CURSO-MT19         TO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE SPACES        TO NOME-IE11.
           MOVE NOME-IE11          TO GS-LINDET(11: 10)
           MOVE CIDADE-MT19        TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES        TO NOME-COMPL-CID.
           MOVE NOME-COMPL-CID     TO GS-LINDET(22: 10)
           MOVE NOME-FORM-MT19     TO GS-LINDET(33: 31)
           MOVE FONE-MT19          TO GS-LINDET(64: 9)
           MOVE IDENTIFICADO-MT19  TO GS-LINDET(73: 1)
           MOVE TURMA-MT19         TO GS-LINDET(78: 4)
           MOVE TURNO-MT19         TO GS-LINDET(82:10).
       ACHA-ULT-SEQUENCIA SECTION.
           MOVE GS-CONTRATO    TO CONTRATO-MT19.
           MOVE ZEROS          TO SEQ-MT19.
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-MTD019 = "10"
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10"        TO ST-MTD019
                 NOT AT END
                     IF CONTRATO-MT19 <> GS-CONTRATO
                        MOVE "10"      TO ST-MTD019
                     ELSE
                        MOVE SEQ-MT19  TO ULT-SEQ
                     END-IF
                 END-READ
           END-PERFORM.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP019" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           IF GS-ORDER = 1
              MOVE ZEROS       TO SEQ-MT19
              MOVE GS-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                    MOVE "10"  TO ST-MTD019
              END-START
           ELSE
              MOVE ZEROS       TO CURSO-MT19
              MOVE SPACES      TO NOME-FORM-MT19
              MOVE GS-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS = ALT1-MT19 INVALID KEY
                    MOVE "10"  TO ST-MTD019.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-MTD019 = "10"
                READ MTD019 NEXT RECORD AT END
                     MOVE "10" TO ST-MTD019
                 NOT AT END
                     IF CONTRATO-MT19 <> GS-CONTRATO
                        MOVE "10" TO ST-MTD019
                     ELSE
                        MOVE SPACES             TO LINDET-REL
                        MOVE CONTRATO-MT19      TO LINDET-REL(1: 5)
                        MOVE SEQ-MT19           TO LINDET-REL(6: 5)
                        MOVE CURSO-MT19         TO CODIGO-IE11
                        READ IED011 INVALID KEY
                             MOVE SPACES        TO NOME-IE11
                        END-READ
                        MOVE NOME-IE11          TO LINDET-REL(11: 10)
                        MOVE CIDADE-MT19        TO CIDADE
                        READ CAD010 INVALID KEY
                             MOVE SPACES        TO NOME-COMPL-CID
                        END-READ
                        MOVE NOME-COMPL-CID     TO LINDET-REL(22: 10)
                        MOVE NOME-FORM-MT19     TO LINDET-REL(33: 31)
                        MOVE FONE-MT19          TO LINDET-REL(64: 9)
                        MOVE IDENTIFICADO-MT19  TO LINDET-REL(73: 1)
                        MOVE TURMA-MT19         TO LINDET-REL(78: 4)
                        MOVE TURNO-MT19         TO LINDET-REL(82:10)

                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       POPUP-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO PASSAR-PARAMETROS
           CALL   "COP041T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP041T"
           MOVE PASSAR-STRING-1(1:4)   TO GS-CONTRATO
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
               MOVE "---"              TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40     TO GS-IDENTIFICACAO

           EVALUATE PASSAR-STRING-1(44:03)
               WHEN "Int" MOVE "Integral"    TO GS-TURNO
               WHEN "Mat" MOVE "Matutino"    TO GS-TURNO
               WHEN "Not" MOVE "Noturno"     TO GS-TURNO
               WHEN "Ves" MOVE "Vespertino"  TO GS-TURNO
           END-EVALUATE

           MOVE PASSAR-STRING-1(6:3)   TO GS-CURSO
                                          CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE "**********"      TO NOME-IE11
           END-READ
           MOVE NOME-IE11              TO GS-NOME-CURSO

           MOVE PASSAR-STRING-1(37:6)  TO GS-TURMA
           REFRESH-OBJECT PRINCIPAL.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO GS-FLAG-CRITICA.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 MTD019 IED011 CAD004 COD008  MTD025
                 CGD010 CGD011 COD060 COD003 MTD020
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP019"            to logacess-programa
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
