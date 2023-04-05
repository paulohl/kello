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
           COPY MTPX021P.
           COPY CADPRO.SEL.
           COPY CADMOD.SEL.
           COPY CGPX001.
           COPY MTPX001.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX020P.
           COPY LOGX002.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY COPW040.
           COPY MTPW021P.
           COPY CADPRO.FD.
           COPY CADMOD.FD.
           COPY CGPW001.
           COPY MTPW001.
           COPY MTPW002.
           COPY MTPW019.
           COPY MTPW020.
           COPY MTPW020P.
           COPY LOGW002.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
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
           05  ST-MTD020P            PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD021P            PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
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
           05  TOTAL-MONTADAS        PIC 9(06).
           05  TOTAL-AVULSAS         PIC 9(06).
           05  TOTAL-CLIENTES        PIC 9(05).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  LIBERADO              PIC 9(01)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)     VALUE ZEROS.
           05  masc-qtde             pic zzz.zz9  BLANK WHEN ZEROS.
           05  masc-valor            pic zzz.zz9,999 blank when zeros.

       77 janelaPrincipal            object reference.
       77 handle8                    pic 9(08) comp-x value zeros.
       77 wHandle                    pic 9(09) comp-5 value zeros.
       77 wsTexto                    pic x(255) value spaces.
       77 wsItem                     pic 9(009) comp-5 value zeros.
       77 UMITEM                     object reference.
       77 UMOBJETO                   object reference.

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
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

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

       01 lnktabelaPro.
          02 lnkobjetoscolPro  object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaCont.
          02 lnkobjetoscolCont object reference occurs 99 times.
       01 lnktabelaColCont.
          02 lnkcolunasCont
                           pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice           pic 9(02).

       01 wssize           pic 9(09) comp-5 value zeros.
       01 wsIndice         pic 9(09) comp-5 value zeros.

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
           MOVE "MTD021P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021P
           MOVE "CADPRO"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO.
           MOVE "CADMOD"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD.
           MOVE "MTD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD020P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020P.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           OPEN I-O   MTD020  MTD001 LOG002 MTD021P CADPRO CADMOD
                      MTD020P
           CLOSE      MTD021P CADPRO CADMOD MTD020P
           OPEN INPUT MTD019  COD040 CGD001 MTD002 CAD004 MTD021P
                      CADPRO  CADMOD MTD020P

           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O    MTD020
           END-IF.
           IF ST-MTD020P = "35"
              CLOSE MTD020P     OPEN OUTPUT MTD020P
              CLOSE MTD020P     OPEN I-O    MTD020P
           END-IF.
           IF ST-MTD021P = "35"
              CLOSE MTD021P     OPEN OUTPUT MTD021P
              CLOSE MTD021P     OPEN I-O    MTD021P
           END-IF.
           IF ST-MTD001 = "35"
              CLOSE MTD001      OPEN OUTPUT MTD001
              CLOSE MTD001      OPEN I-O    MTD001
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O    LOG002
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
           IF ST-MTD021P <> "00"
              MOVE "ERRO ABERTURA MTD021P: " TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021P                TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADMOD TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-MTD020P <> "00"
              MOVE "ERRO ABERTURA MTD020P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020P TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD020 MTD001 LOG002
           OPEN INPUT MTD020 MTD001

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

           IF ERRO-W = 0
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW-PRODUTOS
                   PERFORM CRIAR-LISTVIEW-CONTRATOS
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                      MOVE ZEROS TO GS-VISITA
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM ATUALIZAR-MTD020P
                   PERFORM CARREGA-ULTIMOS

                   PERFORM ATUALIZAR-MTD001
                   PERFORM LIMPAR-DADOS
      *Listar produtos do contrato
                   PERFORM CARREGAR-MTD021P
                   IF GS-ANOMES-VISITA = 0
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM ATUALIZAR-MTD001
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   MOVE ZEROS TO GS-VISITA
                   IF GS-ANOMES-VISITA = 0
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                   END-IF
                   PERFORM SET-UP-FOR-REFRESH-SCREEN
                   PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
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
               WHEN GS-EVENTO-PRODUTOS-TRUE
                   PERFORM TRATAR-EVENTO-PRO
               WHEN GS-EVENTO-CONTRATOS-TRUE
                   PERFORM TRATAR-EVENTO-CONT
               WHEN GS-ALTERAR-QTDE-TRUE
                   PERFORM ALTERAR-QTDE
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       ALTERAR-QTDE SECTION.
           move 5                to indice
           initialize wsTexto
           move gs-qtde-planilha to masc-qtde
           string masc-qtde x"00" into wsTexto
           invoke gs-listview-produtos-cont "preencherColunaZ"
            using wsItem lnkcolunasPro(indice) wsTexto

           move 6                to indice
           initialize wsTexto
           move gs-custo         to masc-valor
           string masc-valor x"00" into wsTexto
           invoke gs-listview-produtos-cont "preencherColunaZ"
            using wsItem lnkcolunasPro(indice) wsTexto

           move 7                to indice
           initialize wsTexto
           move gs-venda         to masc-valor
           string masc-valor x"00" into wsTexto
           invoke gs-listview-produtos-cont "preencherColunaZ"
            using wsItem lnkcolunasPro(indice) wsTexto

           unshow-window win3 principal
           invoke gs-listview-produtos-cont "SetFocus".

       ATUALIZAR-MTD020P SECTION.
           CLOSE      MTD020P
           OPEN I-O   MTD020P

           INITIALIZE REG-MTD020P
           MOVE GS-CONTRATO            TO CONTRATO-MTGP
           MOVE GS-NR-ALBUM            TO NRALBUM-MTGP
           MOVE GS-SEQUENCIA           TO SEQUEN-MTGP
           START MTD020P KEY IS NOT LESS CHAVE-MTGP INVALID KEY
                MOVE "10" TO ST-MTD020P.

           PERFORM UNTIL ST-MTD020P = "10"
                READ MTD020P NEXT AT END
                     MOVE "10" TO ST-MTD020P
                NOT AT END
                     IF GS-CONTRATO  <> CONTRATO-MTGP OR
                        GS-NR-ALBUM  <> NRALBUM-MTGP  OR
                        GS-SEQUENCIA <> SEQUEN-MTGP
                        MOVE "10" TO ST-MTD020P
                     ELSE
                        DELETE MTD020P INVALID KEY
                            MOVE "Erro de Exclusão...MTD020P" TO
                            MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           initialize wsindice
                      wssize

           invoke gs-listview-produtos-cont "Size" returning wsSize
           perform wsSize times
              add 1 to wsindice
              invoke gs-listview-produtos-cont "ItemAtIndex"
                                                        using wsindice
                                                    returning umItem

              initialize indice

              add 1 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto

              move gs-contrato  to contrato-mtgp
              move gs-nr-album  to nralbum-mtgp
              move gs-sequencia to sequen-mtgp
              move function numval(wstexto(12:4)) to produto-mtgp
              move function numval(wstexto(16:4)) to modelo-mtgp


              move 4 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to qtde-contrato-mtgp



              add 1 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to qtde-planilha-mtgp

              add 1 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to custo-unit-mtgp

              add 1 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to venda-unit-mtgp

              write reg-mtd020p invalid key
                    move "Erro de Gravação...MTD020P" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem
              end-write

           end-perform


           CLOSE      MTD020P
           OPEN INPUT MTD020P.

       VERIFICAR-MTD019 SECTION.
           MOVE GS-CONTRATO    TO CONTRATO-MT19
           MOVE GS-NR-ALBUM    TO SEQ-MT19
           READ MTD019 INVALID KEY
                MOVE "Contrato X Álbum Não Cadastrado na Ficha de Identi
      -              "ficação (MTP019)" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE NOME-FORM-MT19  TO GS-NOME-FORMANDO
                REFRESH-OBJECT PRINCIPAL.

           MOVE GS-CONTRATO    TO CONTRATO-MT01
           READ MTD001 INVALID KEY
                MOVE "Não Foi Localizado a Capa de Lote do Contrato"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CRIAR-LISTVIEW-PRODUTOS SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
            using z"Identificação" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
                 using z"Produto" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
                using z"Modelo" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
           using z"Qtde Contrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
           using z"Qtde Planilhada" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
           using z"Valor Custo" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos-cont "adicionarColunaZ"
           using z"Valor Venda" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview-produtos-cont "gridLines"
          invoke gs-listview-produtos-cont "noBorder".
       CRIAR-LISTVIEW-PRODUTOS-FIM.
           EXIT.

       CRIAR-LISTVIEW-CONTRATOS SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-contratos "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscolCont(indice)
          invoke lnkobjetoscolCont(indice) "centered"
          move indice to lnkcolunasCont(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-contratos "adicionarColunaZ"
            using z"Seqüência" returning lnkobjetoscolCont(indice)
          invoke lnkobjetoscolCont(indice) "centered"
          move indice to lnkcolunasCont(indice)

          initialize reg-mtd021p
          move gs-contrato         to contrato-mt21p
          start mtd021p key is not less chave-mt21p invalid key
               move "10" to st-mtd021p.

          perform until st-mtd021p = "10"
               read mtd021p next at end
                    move "10" to st-mtd021p
               not at end
                    if gs-contrato <> contrato-mt21p
                       move "10" to st-mtd021p
                    else
                       move produto-mt21p  to cadpro-codigo
                       move modelo-mt21p   to cadpro-modelo
                       read cadpro invalid key
                            move "*******" to cadpro-nome
                       end-read
                       move modelo-mt21p   to cadmod-codigo
                       read cadmod invalid key
                            move "*******" to cadmod-nome
                       end-read

                       move spaces to wsTexto
                       string cadpro-nome delimited by "   "
                              " - " cadmod-nome delimited by "   "
                              x"00"
                         into wsTexto
      *>---
      *>---
                    add 1 to indice
                    invoke gs-listview-contratos "adicionarColunaZ"
                    using wsTexto returning lnkobjetoscolCont(indice)
                    move indice to lnkcolunasCont(indice)

                    end-if
               end-read
          end-perform

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-contratos "adicionarColunaZ"
              using z"Nome Formando" returning lnkobjetoscolCont(indice)
          move indice to lnkcolunasCont(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-contratos "adicionarColunaZ"
                using z"Mês/Ano" returning lnkobjetoscolCont(indice)
          move indice to lnkcolunasCont(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-contratos "adicionarColunaZ"
                using z"Visita" returning lnkobjetoscolCont(indice)
          move indice to lnkcolunasCont(indice)

          perform mostrar-fonte-favoCont
          perform mostrar-colunas-favoCont

          invoke gs-listview-contratos "gridLines"
          invoke gs-listview-contratos "noBorder".

       tratar-evento-pro section.
           evaluate gs-evento
               when 34123  perform chamar-colunas-favopro
               when 34013  perform produto-aceito
               when 34592  perform produto-aceito
               when 34027  set-focus ef9
           end-evaluate.

       produto-aceito section.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS-CONT "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS-CONT "indexOf" USING UMITEM
                                                     RETURNING WSITEM

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO


              MOVE FUNCTION NUMVAL(WSTEXTO(1:4))  TO CONTRATO-MT21P
              MOVE FUNCTION NUMVAL(WSTEXTO(5:4))  TO ALBUM-MT21P
              MOVE FUNCTION NUMVAL(WSTEXTO(9:3))  TO SEQ-MT21P
              MOVE FUNCTION NUMVAL(WSTEXTO(12:4)) TO PRODUTO-MT21P
              MOVE FUNCTION NUMVAL(WSTEXTO(16:4)) TO MODELO-MT21P
              READ MTD021P INVALID KEY
                  MOVE "Produto do MTP021 Não Encontrado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                  MOVE PRODUTO-MT21P            TO GS-ACP-PRODUTO
                                                   CADPRO-CODIGO
                  MOVE MODELO-MT21P             TO GS-ACP-MODELO
                                                   CADPRO-MODELO
                                                   CADMOD-CODIGO
                  READ CADPRO INVALID KEY
                       MOVE "********"          TO CADPRO-NOME
                  END-READ
                  MOVE CADPRO-NOME              TO GS-DESC-PRODUTO

                  READ CADMOD INVALID KEY
                       MOVE "********"          TO CADMOD-NOME
                  END-READ
                  MOVE CADMOD-NOME              TO GS-DESC-MODELO

                  MOVE QTDE-PLANILHADA-MT21P    TO GS-QTDE-CONTRATO
                  INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(5)
                                         RETURNING UMOBJETO
                  INITIALIZE WSTEXTO
                  INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                  MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-QTDE-PLANILHA


                  INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(6)
                                         RETURNING UMOBJETO
                  INITIALIZE WSTEXTO
                  INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                  MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-CUSTO

                  INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(7)
                                         RETURNING UMOBJETO
                  INITIALIZE WSTEXTO
                  INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                  MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-VENDA

                  REFRESH-OBJECT WIN3
                  SHOW-WINDOW WIN3
                  SET-FOCUS EF-QTDE-PLANILHA.

       tratar-evento-cont section.
           evaluate gs-evento
               when 34123  perform chamar-colunas-favocont
               when 34013  perform contrato-aceito
               when 34592  perform contrato-aceito
               when 34591  perform contrato-selecionado
               when 34027  set-focus ef9
           end-evaluate.

       contrato-selecionado section.
           initialize wsitem
           invoke gs-listview-contratos "nextselecteditem"
                  using wsitem returning umitem

           if umitem not equal null
              invoke gs-listview-contratos "indexof" using umitem
                                                 returning wsitem

              invoke umitem "getcolumnvalue" using lnkcolunascont(1)
                                         returning umobjeto
              initialize wstexto
              invoke umobjeto "getvalue" returning wstexto
              move gs-contrato              to contrato-mtg
              move function numval(wstexto) to nralbum-mtg
                                               gs-nr-album


              invoke umitem "getcolumnvalue" using lnkcolunascont(2)
                                         returning umobjeto
              initialize wstexto
              invoke umobjeto "getvalue" returning wstexto
              move gs-contrato              to contrato-mtg
              move function numval(wstexto) to nralbum-mtg
                                               gs-sequencia

              move 1                        to gs-tipo-gravacao
              perform carregar-dados
              refresh-object principal.

       contrato-aceito section.
           initialize wsitem
           invoke gs-listview-contratos "nextselecteditem"
                  using wsitem returning umitem

           if umitem not equal null
              invoke gs-listview-contratos "indexof" using umitem
                                                 returning wsitem

              invoke umitem "getcolumnvalue" using lnkcolunascont(1)
                                         returning umobjeto
              initialize wstexto
              invoke umobjeto "getvalue" returning wstexto
              move gs-contrato              to contrato-mtg
              move function numval(wstexto) to nralbum-mtg
                                               gs-nr-album


              invoke umitem "getcolumnvalue" using lnkcolunascont(2)
                                         returning umobjeto
              initialize wstexto
              invoke umobjeto "getvalue" returning wstexto
              move gs-contrato              to contrato-mtg
              move function numval(wstexto) to nralbum-mtg
                                               gs-sequencia

              move 1 to gs-tipo-gravacao
              perform carregar-dados
              refresh-object principal.


       CHAMAR-POP-UP SECTION.
           CALL   "MTP019T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "MTP019T"
           MOVE PASSAR-STRING-1(45: 4) TO GS-NR-ALBUM
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FORMANDO.
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
           MOVE GS-CONTRATO        TO CONTRATO-MT19
           MOVE GS-NR-ALBUM        TO SEQ-MT19
           READ MTD019 INVALID KEY
                MOVE "********"    TO NOME-FORM-MT19
           END-READ
           MOVE NOME-FORM-MT19     TO GS-NOME-FORMANDO

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE     TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.
      *Listar produtos do contrato
           PERFORM CARREGAR-MTD021P.

       CARREGAR-MTD021P SECTION.
           INVOKE GS-LISTVIEW-PRODUTOS-CONT "DeleteAll"
           INITIALIZE REG-MTD021P
           MOVE GS-CONTRATO                TO CONTRATO-MT21P
           IF GS-SEQUENCIA > 0
              MOVE GS-NR-ALBUM             TO ALBUM-MT21P
              MOVE GS-SEQUENCIA            TO SEQ-MT21P
           END-IF
           START MTD021P KEY IS NOT LESS CHAVE-MT21P INVALID KEY
                 MOVE "10" TO ST-MTD021P.

           PERFORM UNTIL ST-MTD021P = "10"
                 READ MTD021P NEXT AT END
                      MOVE "10" TO ST-MTD021P
                 NOT AT END
                      IF GS-CONTRATO  <> CONTRATO-MT21P
                         MOVE "10" TO ST-MTD021P
                      ELSE
                         IF GS-SEQUENCIA > 0 AND
                           (GS-NR-ALBUM  <> ALBUM-MT21P OR
                            GS-SEQUENCIA <> SEQ-MT21P)
                            MOVE "10" TO ST-MTD021P
                         ELSE
                            IF (SEQ-MT21P > 0 and
                               (GS-SEQUENCIA = SEQ-MT21P and
                                GS-NR-ALBUM = ALBUM-MT21P)) OR
                                SEQ-MT21P = 0
                              initialize indice
                              invoke gs-listview-produtos-cont
                                     "adicionarItem" returning wsItem

                              add 1 to indice
                              initialize wsTexto
                              string chave-mt21p X"00"  into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto

                              move produto-mt21p to cadpro-codigo
                              move modelo-mt21p  to cadpro-modelo
                              read cadpro invalid key
                                   move "**********" to cadpro-nome
                              end-read

                              move modelo-mt21p  to cadmod-codigo
                              read cadmod invalid key
                                   move "**********" to cadmod-nome
                              end-read

                              add 1 to indice
                              initialize wsTexto
                              string cadpro-nome x"00" delimited by "  "
                                into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto


                              add 1 to indice
                              initialize wsTexto
                              string cadmod-nome x"00" delimited by "  "
                                into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto


                              add 1 to indice
                              initialize wsTexto
                              move qtde-planilhada-mt21p to masc-qtde
                              string masc-qtde x"00"
                                into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto

                              move gs-contrato       to contrato-mtgp
                              move gs-nr-album       to nralbum-mtgp
                              move gs-sequencia      to sequen-mtgp
                              move produto-mt21p     to produto-mtgp
                              move modelo-mt21p      to modelo-mtgp
                              read mtd020p invalid key
                                   move zeros   to qtde-planilha-mtgp
                              end-read

                              add 1 to indice
                              initialize wsTexto
                              move qtde-planilha-mtgp to masc-qtde
                              string masc-qtde x"00"
                                into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto

                              add 1 to indice
                              initialize wsTexto
                              move custo-unit-mtgp to masc-valor
                              string masc-valor x"00" into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto

                              add 1 to indice
                              initialize wsTexto
                              move venda-unit-mtgp to masc-valor
                              string masc-valor x"00" into wsTexto
                              invoke gs-listview-produtos-cont
                                     "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto
      *                     END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           perform mostrar-colunas-favoPro
           perform zebrar-itensPro

           invoke gs-listview-produtos-cont "Size" returning wsSize

           if wsSize > 0
             INVOKE GS-LISTVIEW-PRODUTOS-CONT "SetFocus"
             MOVE 1 TO WSITEM
             INVOKE GS-LISTVIEW-PRODUTOS-CONT "itemAtIndex" USING WSITEM
                                           RETURNING UMITEM
             INVOKE UMITEM "SetSelected".

       CONFERE-TOTAL SECTION.
      *    MOVE GS-CONTRATO        TO CONTRATO-MT01.
      *    READ MTD001 INVALID KEY
      *       MOVE ZEROS TO MONTADA-MT01 AVULSA-MT01.
      *
      *    MOVE MONTADA-MT01       TO GS-FOTO-MONT.
      *    MOVE AVULSA-MT01        TO GS-AVULSA-MONT.
      *    MOVE GS-CONTRATO        TO CONTRATO-MTG.
      *    MOVE ZEROS              TO SEQ-MTG.
      *    MOVE ZEROS              TO GS-FOTO-INDIV GS-AVULSA-INDIV
      *    START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
      *          MOVE "10" TO ST-MTD020.
      *    PERFORM UNTIL ST-MTD020 = "10"
      *     READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
      *       NOT AT END
      *         IF CONTRATO-MTG <> GS-CONTRATO MOVE "10" TO ST-MTD020
      *         ELSE
      *           IF SEQ-MTG = ZEROS
      *              ADD QT-FOTOS-MTG    TO GS-AVULSA-INDIV
      *           ELSE
      *              ADD QT-FOTOS-MTG  TO GS-FOTO-INDIV
      *           END-IF
      *         END-IF
      *     END-READ
      *    END-PERFORM.
      *    COMPUTE GS-FOTO-DIF = GS-FOTO-MONT - GS-FOTO-INDIV
      *    COMPUTE GS-AVULSA-DIF = GS-AVULSA-MONT - GS-AVULSA-INDIV.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020 MTD020P
           OPEN I-O MTD020 LOG002 MTD020P

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

                       INITIALIZE REG-MTD020P
                       MOVE ALBUM-MTG         TO ALBUM-MTGP
                       START MTD020P KEY IS NOT LESS CHAVE-MTGP INVALID
                                                                    KEY
                            MOVE "10" TO ST-MTD020P
                       END-START
                       PERFORM UNTIL ST-MTD020P = "10"
                            READ MTD020P NEXT AT END
                                 MOVE "10" TO ST-MTD020P
                            NOT AT END
                                 IF ALBUM-MTG <> ALBUM-MTGP
                                    MOVE "10" TO ST-MTD020P
                                 ELSE
                                    DELETE MTD020P INVALID KEY
                                        MOVE "Erro de Exclusão...MTD020P
      -                                      "" TO MENSAGEM
                                        MOVE "C" TO TIPO-MSG
                                        PERFORM EXIBIR-MENSAGEM
                                    END-DELETE
                                 END-IF
                            END-READ
                       END-PERFORM
                END-DELETE.

           CLOSE      MTD020 LOG002 MTD020P
           OPEN INPUT MTD020 MTD020P

           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START MTD020 KEY IS = ALBUM-MTG INVALID KEY
                 CONTINUE.

           READ MTD020 INVALID KEY
                INITIALIZE REG-MTD020.

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
              MOVE ANOMES-VISITA-MTG(1: 4)   TO MESANO-W(3: 4)
              MOVE ANOMES-VISITA-MTG(5: 2)   TO MESANO-W(1: 2)
              MOVE MESANO-W            TO GS-ANOMES-VISITA
              IF GS-ANOMES-VISITA = 0
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
                 PERFORM SET-UP-FOR-REFRESH-SCREEN
                 PERFORM CALL-DIALOG-SYSTEM.

           PERFORM CARREGAR-MTD021P.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO    TO DATAMOV-W
           MOVE GS-CONTRATO      TO CONTRATO-W
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-W
           MOVE GS-VISITA        TO VISITA-W
           INITIALIZE REG-MTD019
           INITIALIZE GS-DATA-BLOCK
           MOVE DATAMOV-W        TO GS-DATA-MOVTO
           MOVE CONTRATO-W       TO GS-CONTRATO CONTRATO-MT19.
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO
           MOVE ULT-SEQ          TO GS-NR-ALBUM SEQ-MT19.
           READ MTD019 INVALID KEY
                MOVE SPACES      TO NOME-FORM-MT19.

           MOVE NOME-FORM-MT19   TO GS-NOME-FORMANDO.
           MOVE VISITA-W         TO GS-VISITA
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO            TO CONTRATO-MTG
           MOVE GS-NR-ALBUM            TO NRALBUM-MTG
      *    MOVE GS-SEQUENCIA           TO SEQUEN-MTG
           MOVE GS-DATA-MOVTO          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATAMOV-MTG.
           MOVE GS-NAO-GEROU           TO NAO-GEROU-ALBUM-MTG
           MOVE GS-ANOMES-VISITA(1: 2) TO ANOMES-VISITA-MTG(5: 2)
           MOVE GS-ANOMES-VISITA(3: 4) TO ANOMES-VISITA-MTG(1: 4)
           MOVE GS-VISITA              TO VISITA-MTG.
           IF GS-TIPO-GRAVACAO <> 1
              MOVE ZEROS               TO POSSE-MTG
              MOVE ZEROS               TO CODIGO-POSSE-MTG.
       GRAVA-DADOS SECTION.
           CLOSE MTD020
           OPEN I-O MTD020 LOG002
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
                     ADD 1 TO ULT-SEQ
                END-PERFORM
           END-READ

           CLOSE MTD020 LOG002
           OPEN INPUT MTD020.

       ATUALIZAR-MTD001 SECTION.

           CLOSE    MTD001
           OPEN I-O MTD001 LOG002

           initialize total-montadas
                      total-avulsas
                      total-clientes
                      reg-mtd020

           move gs-contrato   to contrato-mtg
           start mtd020 key is not less album-mtg invalid key
                move "10" to st-mtd020.

           perform until st-mtd020 = "10"
                read mtd020 next at end
                     move "10" to st-mtd020
                not at end
                     if gs-contrato <> contrato-mtg
                        move "10" to st-mtd020
                     else
                        if nralbum-mtg = 0
                           add qt-fotos-mtg to total-avulsas
                        else
                           add qt-fotos-mtg to total-montadas
                        end-if
                        if nao-gerou-album-mtg <> 1 and
                           nralbum-mtg <> 0
                           add 1            to total-clientes
                        end-if
                     end-if
                end-read
           end-perform

           MOVE GS-CONTRATO     TO CONTRATO-MT01
           READ MTD001 NOT INVALID KEY
                MOVE TOTAL-MONTADAS      TO MONTADA-MT01
                MOVE TOTAL-AVULSAS       TO AVULSA-MT01
                MOVE TOTAL-CLIENTES      TO CLIEN-ALBUM-MT01
                COMPUTE PRODUZIDA-MT01 = MONTADA-MT01 +
                                         PERDIDA-MT01 +
                                         AVULSA-MT01
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

           CLOSE MTD001 LOG002
           OPEN INPUT MTD001.

      *    MOVE ULT-SEQ    TO GS-NR-ALBUM.
       REGRAVA-DADOS SECTION.
           CLOSE MTD020
           OPEN I-O MTD020 LOG002
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
                 END-WRITE.

           CLOSE      MTD020 LOG002
           OPEN INPUT MTD020.

      *    MOVE ULT-SEQ  TO GS-NR-ALBUM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO"            TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CARREGA-ULTIMOS SECTION.
           invoke gs-listview-contratos "DeleteAll"

           move zeros              to ult-seq
           move gs-contrato        to contrato-mtg
                                      nr-contrato-co40
           read cod040 invalid key
                move spaces        to identificacao-co40.

           move identificacao-co40 to gs-identificacao
           move zeros              to nralbum-mtg
      *                               sequen-mtg
                                      gs-nr-album
           start mtd020 key is not < album-mtg invalid key
                 move "10"         to st-mtd020.

           move spaces             to gs-lindet.
           perform until st-mtd020 = "10"
              read mtd020 next record at end
                   move "10"       to st-mtd020
              not at end
                   if contrato-mtg <> gs-contrato
                      move "10"        to st-mtd020
                   else
                      move datamov-mtg to data-inv
                      call "gridat1" using data-inv
                      move data-inv    to gs-data-movto

                      perform mover-dados-lista
                      move nralbum-mtg     to ult-seq
                   end-if
              end-read
           end-perform.
           add 1 to ult-seq.
           move ult-seq           to gs-nr-album seq-mt19.
           move gs-contrato       to contrato-mt19.
           read mtd019 invalid key
                move spaces to nome-form-mt19.

          perform mostrar-fonte-favoCont
          perform mostrar-colunas-favoCont

           IF GS-ANOMES-VISITA = 0
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-MES-CPU WS-ANO-CPU INTO GS-ANOMES-VISITA
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.


           MOVE NOME-FORM-MT19    TO GS-NOME-FORMANDO
           REFRESH-OBJECT PRINCIPAL
           PERFORM CARREGAR-MTD021P.

           set-focus EF9.

       MOVER-DADOS-LISTA SECTION.
           initialize indice

           invoke gs-listview-contratos "adicionarItem" returning wsItem

           add 1                       to indice
           initialize wsTexto
           string nralbum-mtg     x"00"  into wstexto
           invoke gs-listview-contratos "preencherColunaZ"
             using wsItem lnkcolunasCont(indice) wsTexto

           add 1                       to indice
           initialize wsTexto
           string nralbum-mtg     x"00"  into wstexto
           invoke gs-listview-contratos "preencherColunaZ"
             using wsItem lnkcolunasCont(indice) wsTexto

           initialize reg-mtd021p
           move gs-contrato         to contrato-mt21p
           move gs-nr-album         to album-mt21p
           move gs-sequencia        to seq-mt21p
           start mtd021p key is not less chave-mt21p invalid key
                move "10" to st-mtd021p.

           perform until st-mtd021p = "10"
                read mtd021p next at end
                     move "10" to st-mtd021p
                not at end
                     if gs-contrato  <> contrato-mt21p or
                        gs-nr-album  <> album-mt21p    or
                        gs-sequencia <> seq-mt21p
                        move "10"                to st-mtd021p
                     else
                        initialize reg-mtd020p
                        move contrato-mt21p      to contrato-mtgp
                        move album-mt21p         to nralbum-mtgp
                        move seq-mt21p           to sequen-mtgp
                        move produto-mt21p       to produto-mtgp
                        move modelo-mt21p        to modelo-mtgp

                        read mtd020p invalid key
                             move zeros          to qtde-planilha-mtgp
                        end-read
      *>----
      *>----
                        add 1 to indice
                        move qtde-planilha-mtgp  to masc-qtde
                        string masc-qtde x"00" into wsTexto
                        invoke gs-listview-contratos "preencherColunaZ"
                         using wsItem lnkcolunasCont(indice) wsTexto

                     end-if
                end-read
           end-perform

           move gs-contrato         to contrato-mt19
           move nralbum-mtg         to seq-mt19
           read mtd019 invalid key
                move spaces         to nome-form-mt19
           end-read
      *>----
      *>----
           add 1                    to indice
           string nome-form-mt19 x"00" delimited by "  " into wsTexto
           invoke gs-listview-contratos "preencherColunaZ"
            using wsItem lnkcolunasCont(indice) wsTexto

      *>----
      *>----
           move anomes-visita-mtg(1: 4) to mesano-w(3: 4)
           move anomes-visita-mtg(5: 2) to mesano-w(1: 2)
           move mesano-w                to mesano-e

           add 1                        to indice
           string mesano-e x"00" delimited by "  " into wsTexto
           invoke gs-listview-contratos "preencherColunaZ"
            using wsItem lnkcolunasCont(indice) wsTexto

      *>----
      *>----
           add 1                    to indice
           move visita-mtg          to masc-qtde
           string masc-qtde x"00" into wsTexto
           invoke gs-listview-contratos "preencherColunaZ"
            using wsItem lnkcolunasCont(indice) wsTexto.
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

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE GS-CONTRATO    TO CONTRATO-MTG.
           MOVE ZEROS          TO NRALBUM-MTG.
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
                        PERFORM MOVER-DADOS-LISTA
                        MOVE GS-LINDET         TO LINDET-REL

                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
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

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-mtp020-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos-cont
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-mtp020-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos-cont wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-produtos-cont lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-mtp020-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos-cont wsTexto
           invoke gs-listview-produtos-cont "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-mtp020-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos-cont
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.


       mostrar-colunas-favoCont section.
          initialize wsTexto
          string "listview-mtp020-cont-" gs-contrato into wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-contratos
                                  wsTexto
                                  lnktabelaCont.
       mostrar-colunas-favoCont-fim.
           exit.

       mostrar-fonte-favoCont section.
           string "listview-mtp020-cont-" gs-contrato into wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-contratos wsTexto.
       mostrar-fonte-favoCont-fim.
           exit.

       EXPORTAR-PARA-EXCEL-CONT section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-contratos lnkTabelaCont.
       EXPORTAR-PARA-EXCEL-CONT-fim.
           EXIT.


       zebrar-itensCont section.
           move spaces to wsTexto
           string "listview-mtp020-cont-" gs-contrato into wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-contratos wsTexto
           invoke gs-listview-contratos "redrawallitems".
       zebrar-itensCont-fim.
           exit.

       chamar-colunas-favoCont section.
           string "listview-mtp020-cont-" gs-contrato into wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-contratos
                               wsTexto
                               lnktabelaCont

           perform mostrar-colunas-favoCont
           perform mostrar-fonte-favoCont
           perform zebrar-itensCont.
       chamar-colunas-favoCont-fim.
           exit.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040  MTD001 MTD019 MTD020 MTD002 CGD001 CAD004
                 MTD021P CADPRO CADMOD MTD020P.

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
