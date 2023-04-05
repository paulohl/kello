       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP020.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 03/07/2000.
      *FUNÇÃO: Movimento de PLANILHA DE ALBUM P/ MONTAGEM

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.
           COPY COPX040.
           COPY CGPX001.
           COPY MTPX001.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX020C.
           COPY LOGX002.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY COPW040.
           COPY CGPW001.
           COPY MTPW001.
           COPY MTPW002.
           COPY MTPW019.
           COPY MTPW020.
           COPY MTPW020C.
           COPY LOGW002.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "MTP020.CPB".
           COPY "MTP020.CPY".
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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD020C            PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
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
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  QT-FITA-W             PIC 9        VALUE ZEROS.
           05  QT-DVD-W              PIC 9        VALUE ZEROS.
           05  QT-PFITA-W            PIC 9        VALUE ZEROS.
           05  VISITA-W              PIC 9(3)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  TOTAL-AVULSA          PIC 9(09).
           05  TOTAL-FOTOS           PIC 9(09).
           05  TOTAL-CLIENTES        PIC 9(09).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  LIBERADO              PIC 9(01)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)     VALUE ZEROS.
      *    COPY "PARAMETR".
           COPY IMPRESSORA.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "MOVIMENTO PLANILHA MONTAGEM ALBUM".
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
           "ALB. ES EN FOLH FOTO PO FI PF DVD FC MO PD BK NOME-CLIENTE
      -    "        MES/ANO V".

       01  LINDET.
           05  LINDET-REL          PIC X(90)  VALUE SPACES.

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

       01 masc-qtde                    PIC Z.ZZZ.ZZ9 BLANK WHEN ZEROS.

       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas
                           pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice           pic 9(02).

       01 wssize           pic 9(09) comp-5 value zeros.
       01 wsIndice         pic 9(09) comp-5 value zeros.

       77 wsTexto          pic x(255) value spaces.
       77 wsItem           pic 9(009) comp-5 value zeros.
       77 UMITEM           object reference.
       77 UMOBJETO         object reference.

       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01 LNK-CONTRATO                 PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-W LNK-CONTRATO.
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
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD020C" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020C.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN I-O   MTD020 MTD001 LOG002 MTD020C
           CLOSE      MTD020C
           OPEN I-O   MTD020C

           OPEN INPUT MTD019 COD040 CGD001 MTD002 CAD004.

           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD020C = "35"
              CLOSE MTD020C     OPEN OUTPUT MTD020C
              CLOSE MTD020C     OPEN I-O MTD020C
           END-IF.
           IF ST-MTD001 = "35"
              CLOSE MTD001      OPEN OUTPUT MTD001
              CLOSE MTD001      OPEN I-O MTD001
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD002 <> "00"
              MOVE "ERRO ABERTURA MTD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020C <> "00"
              MOVE "ERRO ABERTURA MTD020C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD020 MTD001 LOG002 MTD020C
           OPEN INPUT MTD020 MTD001 MTD020C

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020"            to logacess-programa
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

           MOVE LNK-CONTRATO       TO GS-CONTRATO
                                      NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040.

           MOVE IDENTIFICACAO-CO40           TO GS-IDENTIFICACAO

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE     TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA.

           MOVE ZEROS             TO GS-LIBERADO-ALTERACAO
                                     GS-LIBERADO-EXCLUSAO


           MOVE "SENHA66"         TO PROGRAMA-CA004
           MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
           READ CAD004 NOT INVALID KEY
                MOVE 1            TO GS-LIBERADO-ALTERACAO
           END-READ

           MOVE "SENHA67"         TO PROGRAMA-CA004
           MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
           READ CAD004 NOT INVALID KEY
                MOVE 1            TO GS-LIBERADO-EXCLUSAO
           END-READ

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                      MOVE ZEROS TO GS-QT-FITA
                      MOVE ZEROS TO GS-QT-DVD
                      MOVE ZEROS TO GS-QT-PORTA-FITA
                      MOVE ZEROS TO GS-VISITA
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM ATUALIZAR-MTD001
                   PERFORM LIMPAR-DADOS
                   IF GS-ANOMES-VISITA = 0
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   MOVE ZEROS TO GS-QT-FITA
                   MOVE ZEROS TO GS-QT-DVD
                   MOVE ZEROS TO GS-QT-PORTA-FITA
                   MOVE ZEROS TO GS-VISITA
                   IF GS-ANOMES-VISITA = 0
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                   END-IF
                   PERFORM SET-UP-FOR-REFRESH-SCREEN
                   PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-CONTRATO      TO CONTRATO-MTG
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-ALBUM-TRUE
                   PERFORM LE-ALBUM
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-CONFERE-TOTAL-TRUE
                   PERFORM CONFERE-TOTAL
               WHEN GS-VERIFICA-MTD019-TRUE
                   PERFORM VERIFICAR-MTD019
               WHEN GS-TRATAR-EVENTO-TRUE
                   PERFORM TRATAR-EVENTO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               when 34123  perform chamar-colunas-favo
               when 34013  perform item-aceito
               when 34592  perform item-aceito
               when 34027  set-focus ef9
           END-EVALUATE.
       TRATAR-EVENTO-FIM.
           EXIT.

       item-aceito section.
           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-ACP-LISTVIEW "indexOf" USING UMITEM
                                           RETURNING WSITEM

              INVOKE UMITEM "GETCOLUMNVALUE"
                                         USING LNKCOLUNAS(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              MOVE GS-CONTRATO              TO CONTRATO-MTG
              MOVE FUNCTION NUMVAL(WSTEXTO) TO NRALBUM-MTG

              PERFORM CARREGAR-DADOS

              MOVE 1                        TO GS-TIPO-GRAVACAO
              DISABLE-OBJECT D-CONTRATO
              DISABLE-OBJECT EF9
              DISABLE-OBJECT EF15
              DISABLE-OBJECT EF6
              IF GS-DATA-MOVTO = 0
                 ENABLE-OBJECT EF6
              END-IF
              REFRESH-OBJECT PRINCIPAL
              SET-FOCUS D-ESTOJO.
       item-aceito-fim.
           exit.

       VERIFICAR-MTD019 SECTION.
           MOVE GS-CONTRATO    TO CONTRATO-MT19
           MOVE GS-NR-ALBUM    TO SEQ-MT19
           READ MTD019 INVALID KEY
                MOVE "Contrato X Álbum Não Cadastrado na Ficha de Identi
      -              "ficação (MTP019)" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

           MOVE GS-CONTRATO    TO CONTRATO-MT01
           READ MTD001 INVALID KEY
                MOVE "Não Foi Localizado a Capa de Lote do Contrato"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

           MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO

           REFRESH-OBJECT PRINCIPAL.


       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          move function current-date to ws-data-sys
          string ws-dia-cpu ws-mes-cpu ws-ano-cpu into gs-data-movto.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Estojo" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Encadernação" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Folha" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Foto" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Poster" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Fita" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Porta Fita" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DVD" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Foto CD" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Moldura" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Porta DVD" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Book" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Porta Retrato" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Pendrive" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Vídeo HD" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Revista" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Calendário" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Nome Cliente" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Mês/Ano" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Visita" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "centered"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".
       criar-listview-fim.
           exit.

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-acp-mtp020" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-acp-mtp020" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       EXPORTAR-PARA-EXCEL section.
           invoke aListview "ExportarParaOExcel"
                    using gs-acp-listview lnkTabela.
       EXPORTAR-PARA-EXCEL-fim.
           EXIT.

       zebrar-itens section.
           move "listview-acp-mtp020" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-acp-mtp020" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4)  TO GS-CONTRATO
                    MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO
             WHEN 2 CALL   "MTP019T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "MTP019T"
                    MOVE PASSAR-STRING-1(45: 4) TO GS-NR-ALBUM
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FORMANDO
           END-EVALUATE.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.

       LE-ALBUM SECTION.
           MOVE GS-CONTRATO        TO CONTRATO-MT19.
           MOVE GS-NR-ALBUM        TO SEQ-MT19.
           READ MTD019 INVALID KEY MOVE "********" TO NOME-FORM-MT19.
           MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.

       CONFERE-TOTAL SECTION.
           MOVE GS-CONTRATO        TO CONTRATO-MT01.
           READ MTD001 INVALID KEY
              MOVE ZEROS TO MONTADA-MT01 AVULSA-MT01.

           MOVE MONTADA-MT01       TO GS-FOTO-MONT.
           MOVE AVULSA-MT01        TO GS-AVULSA-MONT.
           MOVE GS-CONTRATO        TO CONTRATO-MTG.
           MOVE ZEROS              TO NRALBUM-MTG
           MOVE ZEROS              TO GS-FOTO-INDIV GS-AVULSA-INDIV
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
           PERFORM UNTIL ST-MTD020 = "10"
            READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
                IF CONTRATO-MTG <> GS-CONTRATO MOVE "10" TO ST-MTD020
                ELSE
                  IF NRALBUM-MTG = ZEROS
                     ADD QT-FOTOS-MTG    TO GS-AVULSA-INDIV
                  ELSE
                     ADD QT-FOTOS-MTG  TO GS-FOTO-INDIV
                  END-IF
                END-IF
            END-READ
           END-PERFORM.
           COMPUTE GS-FOTO-DIF = GS-FOTO-MONT - GS-FOTO-INDIV
           COMPUTE GS-AVULSA-DIF = GS-AVULSA-MONT - GS-AVULSA-INDIV.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020 MTD020C
           OPEN I-O MTD020 LOG002

           MOVE GS-CONTRATO   TO ALBUM-MTG(1: 4)
           MOVE GS-NR-ALBUM   TO ALBUM-MTG(5: 4)
           READ MTD020 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD020 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD020"    TO LOG2-ARQUIVO
                       MOVE "MTP020"    TO LOG2-PROGRAMA
                       MOVE REG-MTD020  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                END-DELETE.

           MOVE ALBUM-MTG TO ALBUM-MTG-C
           READ MTD020C NOT INVALID KEY
                DELETE MTD020C NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD020C"   TO LOG2-ARQUIVO
                       MOVE "MTP020"    TO LOG2-PROGRAMA
                       MOVE REG-MTD020C TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                END-DELETE
           END-READ

           CLOSE      MTD020 LOG002 MTD020C
           OPEN INPUT MTD020 MTD020C

           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START MTD020 KEY IS = ALBUM-MTG INVALID KEY
                 CONTINUE.

           READ MTD020 INVALID KEY
                INITIALIZE REG-MTD020.

           MOVE ALBUM-MTG TO ALBUM-MTG-C
           READ MTD020C INVALID KEY
                INITIALIZE REG-MTD020C.

           MOVE 0                 TO LIBERADO

           MOVE "SENHA47"         TO PROGRAMA-CA004
           MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
           READ CAD004 NOT INVALID KEY
                MOVE 1            TO LIBERADO
           END-READ


           IF (FOGO-MTG = 1 OR 8) AND LIBERADO = 0
              MOVE SPACES TO MENSAGEM
              STRING "Album com Status Vendido ou Vendido-Fogo," X"0DA0"
                     "Não podendo ser Alterado" INTO MENSAGEM
              MOVE   "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE CONTRATO-MTG        TO GS-CONTRATO
                                          NR-CONTRATO-CO40
                                          CONTRATO-MT19
              READ COD040 INVALID KEY
                   MOVE SPACES TO IDENTIFICACAO-CO40
              END-READ

              MOVE IDENTIFICACAO-CO40  TO GS-IDENTIFICACAO
              MOVE NRALBUM-MTG         TO GS-NR-ALBUM SEQ-MT19
              READ MTD019 INVALID KEY
                   MOVE SPACES         TO NOME-FORM-MT19
              END-READ

              MOVE NOME-FORM-MT19      TO GS-NOME-FORMANDO
              MOVE DATAMOV-MTG         TO DATA-INV
              CALL "GRIDAT1" USING DATA-INV
              MOVE DATA-INV            TO GS-DATA-MOVTO
              IF NAO-GEROU-ALBUM-MTG = 1
                 MOVE NAO-GEROU-ALBUM-MTG TO GS-NAO-GEROU
              ELSE
                 MOVE 0                   TO GS-NAO-GEROU
              END-IF
              MOVE QT-ESTOJO-MTG       TO GS-QT-ESTOJO
              MOVE QT-ENCADER-MTG      TO GS-QT-ENCADER
              MOVE QT-FOLHAS-MTG       TO GS-QT-FOLHA
              MOVE QT-FOTOS-MTG        TO GS-QT-FOTO
              MOVE QT-FITAS-MTG        TO GS-QT-FITA
              MOVE QT-DVD-MTG          TO GS-QT-DVD
              MOVE QT-POSTER-MTG       TO GS-QT-POSTER
              MOVE QT-PORTA-FITA-MTG   TO GS-QT-PORTA-FITA
              MOVE QT-FOTO-CD-MTG      TO GS-QT-FOTO-CD
              MOVE QT-MOLDURA-MTG      TO GS-QT-MOLDURA
              MOVE QT-PORTA-DVD-MTG    TO GS-QT-PORTA-DVD
              MOVE QT-BOOK-MTG         TO GS-QT-BOOK

              MOVE QT-PORTA-RETRATO-MTG-C TO GS-QT-PORTA-RETRATO
              MOVE QT-PENDRIVE-MTG-C      TO GS-QT-PENDRIVE
              MOVE QT-VIDEO-HD-MTG-C      TO GS-QT-VIDEO-HD
              MOVE QT-REVISTA-MTG-C       TO GS-QT-REVISTA
              MOVE QT-CALENDARIO-MTG-C    TO GS-QT-CALENDARIO


              EVALUATE FOGO-MTG
                WHEN 0 MOVE "0-Montagem    "    TO GS-FOGO
                WHEN 1 MOVE "1-Vendido     "    TO GS-FOGO
                WHEN 8 MOVE "8-Vendido-Fogo"    TO GS-FOGO
                WHEN 9 MOVE "9-Fogo        "    TO GS-FOGO
              END-EVALUATE
              MOVE DATA-FOGO-MTG             TO DATA-INV
              CALL "GRIDAT1" USING DATA-INV
              MOVE DATA-INV                  TO GS-DATA-FOGO
              MOVE ANOMES-VISITA-MTG(1: 4)   TO MESANO-W(3: 4)
              MOVE ANOMES-VISITA-MTG(5: 2)   TO MESANO-W(1: 2)
              MOVE MESANO-W            TO GS-ANOMES-VISITA
              IF GS-ANOMES-VISITA = 0
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                 PERFORM SET-UP-FOR-REFRESH-SCREEN
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF

              MOVE VISITA-MTG          TO GS-VISITA
              EVALUATE POSSE-MTG
                WHEN 1 MOVE '1-Estoque'  TO GS-POSSE
                       MOVE CODIGO-POSSE-MTG  TO CODIGO-MT02
                                                 GS-CODIGO-POSSE
                       READ MTD002 INVALID KEY
                            MOVE SPACES TO NOME-MT02
                       END-READ
                       MOVE NOME-MT02    TO GS-DESC-POSSE
                WHEN 2 MOVE '1-Vendedor' TO GS-POSSE
                       MOVE CODIGO-POSSE-MTG  TO CODIGO-CG01
                                                 GS-CODIGO-POSSE
                       READ CGD001 INVALID KEY
                            MOVE SPACES TO NOME-CG01
                       END-READ
                       MOVE NOME-CG01    TO GS-DESC-POSSE
                WHEN 3 MOVE '3-Montagem' TO GS-POSSE
                       MOVE SPACES       TO GS-DESC-POSSE
              END-EVALUATE.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO    TO DATAMOV-W
           MOVE GS-CONTRATO      TO CONTRATO-W
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-W
      *    MOVE GS-QT-FITA       TO QT-FITA-W
      *    MOVE GS-QT-DVD        TO QT-DVD-W
      *    MOVE GS-QT-PORTA-FITA TO QT-PFITA-W
           MOVE GS-VISITA        TO VISITA-W
           INITIALIZE REG-MTD019
           INITIALIZE GS-DATA-BLOCK
           MOVE "3-Montagem"     TO GS-POSSE
           MOVE "0-Montagem"     TO GS-FOGO
           MOVE DATAMOV-W        TO GS-DATA-MOVTO
           MOVE CONTRATO-W       TO GS-CONTRATO CONTRATO-MT19.
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO
           MOVE ULT-SEQ          TO GS-NR-ALBUM SEQ-MT19.
           READ MTD019 INVALID KEY
                MOVE SPACES TO NOME-FORM-MT19.

           MOVE NOME-FORM-MT19   TO GS-NOME-FORMANDO.
      *    MOVE QT-FITA-W        TO GS-QT-FITA
      *    MOVE QT-DVD-W         TO GS-QT-DVD
      *    MOVE QT-PFITA-W       TO GS-QT-PORTA-FITA
           MOVE VISITA-W         TO GS-VISITA
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO            TO CONTRATO-MTG
           MOVE GS-NR-ALBUM            TO NRALBUM-MTG
           MOVE GS-DATA-MOVTO          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATAMOV-MTG.
           MOVE GS-NAO-GEROU           TO NAO-GEROU-ALBUM-MTG
           MOVE GS-QT-ESTOJO           TO QT-ESTOJO-MTG.
           MOVE GS-QT-FITA             TO QT-FITAS-MTG
           MOVE GS-QT-DVD              TO QT-DVD-MTG
           MOVE GS-QT-FOLHA            TO QT-FOLHAS-MTG
           MOVE GS-QT-FOTO             TO QT-FOTOS-MTG
           MOVE GS-QT-PORTA-FITA       TO QT-PORTA-FITA-MTG
           MOVE GS-QT-ENCADER          TO QT-ENCADER-MTG
           MOVE GS-QT-POSTER           TO QT-POSTER-MTG
           MOVE GS-QT-FOTO-CD          TO QT-FOTO-CD-MTG
           MOVE GS-QT-MOLDURA          TO QT-MOLDURA-MTG
           MOVE GS-QT-BOOK             TO QT-BOOK-MTG
           MOVE GS-QT-PORTA-DVD        TO QT-PORTA-DVD-MTG
           MOVE GS-QT-PORTA-RETRATO    TO QT-PORTA-RETRATO-MTG-C
           MOVE GS-QT-PENDRIVE         TO QT-PENDRIVE-MTG-C
           MOVE GS-QT-VIDEO-HD         TO QT-VIDEO-HD-MTG-C
           MOVE GS-QT-REVISTA          TO QT-REVISTA-MTG-C
           MOVE GS-QT-CALENDARIO       TO QT-CALENDARIO-MTG-C
           MOVE GS-ANOMES-VISITA(1: 2) TO ANOMES-VISITA-MTG(5: 2)
           MOVE GS-ANOMES-VISITA(3: 4) TO ANOMES-VISITA-MTG(1: 4)
           MOVE GS-VISITA              TO VISITA-MTG.
           MOVE GS-POSSE(1: 1)         TO POSSE-MTG.
           MOVE GS-CODIGO-POSSE        TO CODIGO-POSSE-MTG.
       GRAVA-DADOS SECTION.
           CLOSE    MTD020 MTD020C
           OPEN I-O MTD020 LOG002 MTD020C
           MOVE SPACES TO MENSAGEM
           MOVE GS-NR-ALBUM       TO SEQ-MT19
           MOVE GS-CONTRATO       TO CONTRATO-MT19
           READ MTD019 INVALID KEY
                MOVE "Ficha de Identificação Não Cadastrada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE "Os dados Não foram salvos, pois não havia FICHA DE
      -              " IDENTIFICAÇÃO" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE SPACES TO NOME-FORM-MT19
           NOT INVALID KEY
                MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO
                MOVE ZEROS TO FOGO-MTG
                              DATA-FOGO-MTG
                MOVE 3     TO POSSE-MTG
                MOVE ZEROS TO ST-MTD020
                PERFORM UNTIL ST-MTD020 = "10"
                     WRITE REG-MTD020 INVALID KEY
                         ADD 1 TO NRALBUM-MTG
                     NOT INVALID KEY
                         MOVE USUARIO-W   TO LOG2-USUARIO
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         MOVE WS-DATA-CPU TO LOG2-DATA
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE WS-HORA-SYS TO LOG2-HORAS
                         MOVE "I"         TO LOG2-OPERACAO
                         MOVE "MTD020"    TO LOG2-ARQUIVO
                         MOVE "MTP020"    TO LOG2-PROGRAMA
                         MOVE REG-MTD020  TO LOG2-REGISTRO
                         WRITE REG-LOG002
                         END-WRITE
                         MOVE "10" TO ST-MTD020
                     END-WRITE
                     MOVE ALBUM-MTG       TO ALBUM-MTG-C
                     WRITE REG-MTD020C NOT INVALID KEY
                         MOVE USUARIO-W   TO LOG2-USUARIO
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         MOVE WS-DATA-CPU TO LOG2-DATA
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE WS-HORA-SYS TO LOG2-HORAS
                         MOVE "I"         TO LOG2-OPERACAO
                         MOVE "MTD020C"   TO LOG2-ARQUIVO
                         MOVE "MTP020"    TO LOG2-PROGRAMA
                         MOVE REG-MTD020C TO LOG2-REGISTRO
                         WRITE REG-LOG002
                         END-WRITE
                     END-WRITE
                     PERFORM MOVER-LISTVIEW
                     ADD 1 TO ULT-SEQ
                END-PERFORM
           END-READ

           CLOSE      MTD020 LOG002 MTD020C
           OPEN INPUT MTD020 MTD020C.

       ATUALIZAR-MTD001 SECTION.

           CLOSE    MTD001
           OPEN I-O MTD001 LOG002

           MOVE ZEROS           TO TOTAL-FOTOS TOTAL-AVULSA
                                   TOTAL-CLIENTES

           MOVE CONTRATO-MTG    TO CONTRATO-MT01
           READ MTD001 NOT INVALID KEY
               MOVE 0          TO PRODUZIDA-MT01
               MOVE 0          TO MONTADA-MT01
               MOVE 0          TO AVULSA-MT01
               MOVE 0          TO CLIEN-ALBUM-MT01
               REWRITE REG-MTD001 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "A"         TO LOG2-OPERACAO
                       MOVE "MTD001"    TO LOG2-ARQUIVO
                       MOVE "MTP020"    TO LOG2-PROGRAMA
                       MOVE REG-MTD001  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
               END-REWRITE
           END-READ.

           MOVE GS-CONTRATO     TO CONTRATO-MTG NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO.
           MOVE ZEROS           TO NRALBUM-MTG GS-NR-ALBUM.
           START MTD020 KEY IS NOT < ALBUM-MTG
                    INVALID KEY MOVE "10" TO ST-MTD020.
           PERFORM UNTIL ST-MTD020 = "10"
              READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
                IF CONTRATO-MTG <> GS-CONTRATO
                   MOVE "10" TO ST-MTD020
                ELSE
                  IF NRALBUM-MTG = ZEROS
                     ADD QT-FOTOS-MTG    TO TOTAL-AVULSA
                  ELSE
                     ADD QT-FOTOS-MTG  TO TOTAL-FOTOS
                     IF NAO-GEROU-ALBUM-MTG <> 1
                        ADD 1             TO  TOTAL-CLIENTES
                     END-IF
                  END-IF
                END-IF
              END-READ
           END-PERFORM.

           MOVE GS-CONTRATO     TO CONTRATO-MT01
           READ MTD001 NOT INVALID KEY
               ADD TOTAL-FOTOS     TO PRODUZIDA-MT01
               ADD PERDIDA-MT01    TO PRODUZIDA-MT01
               ADD TOTAL-AVULSA    TO PRODUZIDA-MT01
               MOVE TOTAL-AVULSA   TO AVULSA-MT01
               ADD TOTAL-FOTOS     TO MONTADA-MT01
               MOVE TOTAL-CLIENTES TO CLIEN-ALBUM-MT01
               REWRITE REG-MTD001 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "A"         TO LOG2-OPERACAO
                       MOVE "MTD001"    TO LOG2-ARQUIVO
                       MOVE "MTP020"    TO LOG2-PROGRAMA
                       MOVE REG-MTD001  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
               END-REWRITE
           END-READ

           CLOSE MTD001 LOG002
           OPEN INPUT MTD001.

      *    MOVE ULT-SEQ    TO GS-NR-ALBUM.
       REGRAVA-DADOS SECTION.
           CLOSE    MTD020 MTD020C
           OPEN I-O MTD020 LOG002 MTD020C
           REWRITE REG-MTD020 INVALID KEY
                 MOVE "Erro Regravacao MTD020" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "A"         TO LOG2-OPERACAO
                 MOVE "MTD020"    TO LOG2-ARQUIVO
                 MOVE "MTP020"    TO LOG2-PROGRAMA
                 MOVE REG-MTD020  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
           END-REWRITE

           MOVE ALBUM-MTG TO ALBUM-MTG-C
           WRITE REG-MTD020C INVALID KEY
                 REWRITE REG-MTD020C INVALID KEY
                     MOVE "Erro Regravacao MTD020C" TO GS-MENSAGEM-ERRO
                     MOVE ST-MTD020C TO GS-MENSAGEM-ERRO(24: 5)
                     MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                 NOT INVALID KEY
                     MOVE USUARIO-W   TO LOG2-USUARIO
                     MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                     MOVE WS-DATA-CPU TO LOG2-DATA
                     ACCEPT WS-HORA-SYS FROM TIME
                     MOVE WS-HORA-SYS TO LOG2-HORAS
                     MOVE "A"         TO LOG2-OPERACAO
                     MOVE "MTD020C"   TO LOG2-ARQUIVO
                     MOVE "MTP020"    TO LOG2-PROGRAMA
                     MOVE REG-MTD020C TO LOG2-REGISTRO
                     WRITE REG-LOG002
                     END-WRITE
                 END-REWRITE
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "MTD020C"   TO LOG2-ARQUIVO
                 MOVE "MTP020"    TO LOG2-PROGRAMA
                 MOVE REG-MTD020C TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
           END-WRITE

           CLOSE      MTD020 LOG002 MTD020C
           OPEN INPUT MTD020 MTD020C

           PERFORM ATUALIZAR-LISTVIEW.
      *    MOVE ULT-SEQ  TO GS-NR-ALBUM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           INVOKE GS-ACP-LISTVIEW "DeleteAll"

           MOVE ZEROS TO ULT-SEQ
           MOVE GS-CONTRATO          TO CONTRATO-MTG
                                        NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO.
           MOVE ZEROS                TO NRALBUM-MTG
                                        GS-NR-ALBUM.
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
           PERFORM UNTIL ST-MTD020 = "10"
                 READ MTD020 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD020
                 NOT AT END
                      IF CONTRATO-MTG <> GS-CONTRATO
                         MOVE "10" TO ST-MTD020
                      ELSE
                         MOVE ALBUM-MTG   TO ALBUM-MTG-C
                         READ MTD020C INVALID KEY
                              INITIALIZE REG-MTD020C
                         END-READ

                         MOVE NRALBUM-MTG TO ULT-SEQ

                         MOVE DATAMOV-MTG TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV    TO GS-DATA-MOVTO

                         PERFORM MOVER-LISTVIEW

                      END-IF
                 END-READ
           END-PERFORM.

           perform mostrar-fonte-favo
           perform mostrar-colunas-favo

           MOVE "3-Montagem"      TO GS-POSSE
           MOVE "0-Montagem"      TO GS-FOGO
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ           TO GS-NR-ALBUM SEQ-MT19.
           MOVE GS-CONTRATO       TO CONTRATO-MT19.
           READ MTD019 INVALID KEY
                MOVE SPACES       TO NOME-FORM-MT19.

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.


           MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO

          move function current-date to ws-data-sys
          string ws-dia-cpu ws-mes-cpu ws-ano-cpu into gs-data-movto.

           REFRESH-OBJECT PRINCIPAL.

       ATUALIZAR-LISTVIEW SECTION.
           invoke gs-acp-listview "indexof" using umitem
                                        returning wsindice
           move wsindice to wsItem
           perform carregar-dados
           perform mover-listview.


       MOVER-LISTVIEW SECTION.
           initialize indice
           if gs-tipo-gravacao <> 1
              invoke gs-acp-listview "adicionarItem" returning wsItem
           end-if

      *>Nº do álbum
           add 1 to indice
           initialize wsTexto
           string nralbum-mtg X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Estojo
           add 1 to indice
           initialize wsTexto
           move qt-estojo-mtg to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Encadernação
           add 1 to indice
           initialize wsTexto
           move qt-encader-mtg       to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Folhas
           add 1 to indice
           initialize wsTexto
           move qt-folhas-mtg       to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Fotos
           add 1 to indice
           initialize wsTexto
           move qt-fotos-mtg         to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Poster
           add 1 to indice
           initialize wsTexto
           move qt-poster-mtg        to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Fitas
           add 1 to indice
           initialize wsTexto
           move qt-fitas-mtg        to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Porta Fitas
           add 1 to indice
           initialize wsTexto
           move qt-porta-fita-mtg    to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt DVD
           add 1 to indice
           initialize wsTexto
           move qt-dvd-mtg           to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Foto CD
           add 1 to indice
           initialize wsTexto
           move qt-foto-cd-mtg       to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Moldura
           add 1 to indice
           initialize wsTexto
           move qt-moldura-mtg       to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Porta DVD
           add 1 to indice
           initialize wsTexto
           move qt-porta-dvd-mtg       to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt BOOK
           add 1 to indice
           initialize wsTexto
           move qt-book-mtg            to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Porta Retrato
           add 1 to indice
           initialize wsTexto
           move qt-porta-retrato-mtg-c to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Pendrive
           add 1 to indice
           initialize wsTexto
           move qt-pendrive-mtg-c    to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Video HD
           add 1 to indice
           initialize wsTexto
           move qt-video-hd-mtg-c    to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Revista
           add 1 to indice
           initialize wsTexto
           move qt-revista-mtg-c     to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Calendário
           add 1 to indice
           initialize wsTexto
           move qt-calendario-mtg-c to masc-qtde
           string masc-qtde X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Nome cliente
           MOVE GS-CONTRATO       TO CONTRATO-MT19
           MOVE NRALBUM-MTG       TO SEQ-MT19
           READ MTD019 INVALID KEY
                MOVE SPACES TO NOME-FORM-MT19
           END-READ
           add 1 to indice
           initialize wsTexto
           string nome-form-mt19 X"00" delimited by "   " into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Mês/ano
           MOVE ANOMES-VISITA-MTG(1: 4) TO MESANO-W(3: 4)
           MOVE ANOMES-VISITA-MTG(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E

           add 1 to indice
           initialize wsTexto
           string mesano-e X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto
      *>Qt Visita
           add 1 to indice
           initialize wsTexto
           string visita-mtg X"00"  into wsTexto
           invoke gs-acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto.
       MOVER-LISTVIEW-FIM.
           EXIT.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP020" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO CONTRATO-MTG.
           MOVE ZEROS          TO NRALBUM-MTG
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                 MOVE "10" TO ST-MTD020.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-MTD020 = "10"
                 READ MTD020 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD020
                 NOT AT END
                      IF CONTRATO-MTG <> GS-CONTRATO
                         MOVE "10"             TO ST-MTD020
                      ELSE
                        MOVE SPACES            TO LINDET-REL
                        PERFORM MOVER-DADOS-RELATORIO

                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE NRALBUM-MTG       TO LINDET-REL(1: 5)
           MOVE QT-ESTOJO-MTG     TO LINDET-REL(6: 3)
           MOVE QT-ENCADER-MTG    TO LINDET-REL(9: 3)
           MOVE QT-FOLHAS-MTG     TO LINDET-REL(12: 5)
           MOVE QT-FOTOS-MTG      TO LINDET-REL(17: 5)
           MOVE QT-POSTER-MTG     TO LINDET-REL(22: 3)
           MOVE QT-FITAS-MTG      TO LINDET-REL(25: 3)
           MOVE QT-PORTA-FITA-MTG TO LINDET-REL(28: 3)
           MOVE QT-DVD-MTG        TO LINDET-REL(31:3)
           MOVE QT-FOTO-CD-MTG    TO LINDET-REL(35:3)
           MOVE QT-MOLDURA-MTG    TO LINDET-REL(38:3)
           MOVE QT-PORTA-DVD-MTG  TO LINDET-REL(41:3)
           MOVE QT-BOOK-MTG       TO LINDET-REL(44:3)
           MOVE GS-CONTRATO       TO CONTRATO-MT19
           MOVE NRALBUM-MTG       TO SEQ-MT19
           READ MTD019 INVALID KEY MOVE SPACES TO NOME-FORM-MT19.
           MOVE NOME-FORM-MT19    TO LINDET-REL(47: 23)
           MOVE ANOMES-VISITA-MTG(1: 4) TO MESANO-W(3: 4)
           MOVE ANOMES-VISITA-MTG(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(71: 8)
           MOVE VISITA-MTG        TO LINDET-REL(79: 3).


       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       EXIBIR-MENSAGEM SECTION.
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
           CLOSE COD040 MTD001 MTD019 MTD020 MTD002 CGD001 CAD004
                 MTD020C.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020"            to logacess-programa
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
