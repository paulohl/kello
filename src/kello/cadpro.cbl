       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADPRO.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 25-03-2011
      *DESCRIÇÃO: Cadastro de Produtos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.
           COPY CADPRO.SEL.
           COPY CADGRU.SEL.
           COPY CADSEG.SEL.
           COPY CADMOD.SEL.
           COPY CGPX001.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY CADPRO.FD.
           COPY CADGRU.FD.
           COPY CADSEG.FD.
           COPY CADMOD.FD.
           COPY CGPW001.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "CADPRO.CPB".
           COPY "CADPRO.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADGRU             PIC XX       VALUE SPACES.
           05  ST-CADSEG             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de região utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
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
           COPY "PARAMETR".

       01 command-line2             pic x(87).
       01 command-line-len          pic x(4) comp-5.
       01 run-unit-id               pic x(8) comp-5.
       01 stack-size                pic x(4) comp-5.
       01 flags                     pic x(4) comp-5.
       01 tty-cmd                   pic x(8).
       01 tty-cmd-len               pic x(4) comp-5.
       01 status-code               pic 9(5) comp-5.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 PASSAR-STRING-1      PIC X(65)    VALUE SPACES.
       01 mensagem             pic x(200).
       01 tipo-msg             pic x(01).
       01 resp-msg             pic x(01).
       01 ws-ok                pic x(01).
       01 nome-prog            pic x(8).

       77 wscodbarra           pic 9(12) value zeros.
       77 ws-digito            pic 9(02).
       77 wsFator              pic 9.
       77 wsSoma               pic 9(06).
       77 ws-ind               pic 9(02).
       77 ws-numero            pic 9.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO DE GRUPOS         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CODIGO NOME GRUPO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

           COPY IMPRESSORA.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP012-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CAP012-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CAP012-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP012-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CAD004
           MOVE "CADPRO"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CADPRO
           MOVE "CADGRU"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CADGRU
           MOVE "CADSEG"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CADSEG
           MOVE "CADMOD"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CADMOD
           MOVE "CGD001"           TO ARQ-REC
           MOVE EMPRESA-REF        TO PATH-CGD001
           MOVE "LOGACESS"         TO ARQ-REC
           MOVE EMPRESA-REF        TO ARQUIVO-LOGACESS

           OPEN I-O CAD004 CADPRO CADGRU CADSEG CADMOD CGD001
           CLOSE    CAD004 CADPRO CADGRU CADSEG CADMOD CGD001
           OPEN I-O CAD004 CADPRO CADGRU CADSEG CADMOD CGD001
           MOVE 1 TO GRAVA-W.
           IF ST-CAD004 = "35"
              CLOSE CAD004      OPEN OUTPUT CAD004
              CLOSE CAD004      OPEN I-O    CAD004
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CADPRO      OPEN OUTPUT CADPRO
              CLOSE CADPRO      OPEN I-O CADPRO
           END-IF.
           IF ST-CADGRU = "35"
              CLOSE CADGRU      OPEN OUTPUT CADGRU
              CLOSE CADGRU      OPEN I-O    CADGRU
           END-IF.
           IF ST-CADSEG = "35"
              CLOSE CADSEG      OPEN OUTPUT CADSEG
              CLOSE CADSEG      OPEN I-O CADSEG
           END-IF.
           IF ST-CADMOD = "35"
              CLOSE CADMOD      OPEN OUTPUT CADMOD
              CLOSE CADMOD      OPEN I-O CADMOD
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CGD001      OPEN OUTPUT CGD001
              CLOSE CGD001      OPEN I-O CGD001
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CAD004 TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CADPRO TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADGRU <> "00"
              MOVE "ERRO ABERTURA CADGRU: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CADGRU TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADSEG <> "00"
              MOVE "ERRO ABERTURA CADSEG: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CADSEG TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CADMOD TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CAP012-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CAP012-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CAP012-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CADPRO"            to logacess-programa
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

           close cadpro
           open input cadpro

           IF ERRO-W = ZEROS
                MOVE 1 TO CAP012-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP012-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CAP012-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-PRINTER-FLG-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP012-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CAP012-CARREGA-LIST-BOX-TRUE
                   MOVE FUNCTION NUMVAL(CAP012-LINDET(1: 4))
                                                        TO CAP012-CODIGO
                   MOVE FUNCTION NUMVAL(CAP012-LINDET(49:04))
                                                        TO CAP012-MODELO
                   PERFORM CARREGAR-DADOS
               WHEN CAP012-VALIDA-FLG-TRUE
                   PERFORM VALIDAR-DADOS
               WHEN CAP012-POPUP-FLG-TRUE
                   PERFORM POPUP
               WHEN CAP012-CADASTRA-FLG-TRUE
                   PERFORM CADASTRAR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CAP012-CODIGO       TO CADPRO-CODIGO
           MOVE CAP012-MODELO       TO CADPRO-MODELO
           READ CADPRO INVALID KEY
                INITIALIZE REG-CADPRO
                MOVE 1              TO GRAVA-W.

           MOVE CAP012-MODELO         TO CADMOD-CODIGO
           READ CADMOD INVALID KEY
                INITIALIZE REG-CADMOD.

           MOVE CADMOD-NOME           TO CAP012-DESC-MODELO

           MOVE CADPRO-CODBARRAS      TO CAP012-COD-BARRAS
           MOVE CADPRO-REFERENCIA     TO CAP012-REFERENCIA
           MOVE CADPRO-NOME           TO CAP012-NOME
           MOVE CADPRO-UNIDADE        TO CAP012-UNIDADE
           MOVE CADPRO-GRUPO          TO CAP012-GRUPO
                                         CADGRU-CODIGO
           READ CADGRU INVALID KEY
                INITIALIZE REG-CADGRU.
           MOVE CADGRU-NOME           TO CAP012-DESC-GRUPO

           MOVE CADPRO-SEGMENTO       TO CAP012-SEGMENTO
                                         CADSEG-CODIGO
           READ CADSEG INVALID KEY
                INITIALIZE REG-CADSEG.
           MOVE CADSEG-NOME           TO CAP012-DESC-SEGMENTO

           MOVE CADPRO-QTDE-MINIMA    TO CAP012-QTDE-MINIMA
           MOVE CADPRO-QTDE-ESTOQUE   TO CAP012-QTDE-ESTOQUE
           MOVE CADPRO-ICMS           TO CAP012-ICMS
           MOVE CADPRO-IPI            TO CAP012-IPI
           MOVE CADPRO-ULT-COMPRA     TO CAP012-ULT-COMPRA
           MOVE CADPRO-CUSTO-COMPRA   TO CAP012-CUSTO-COMPRA
           MOVE CADPRO-CUSTO-MEDIO    TO CAP012-CUSTO-MEDIO
           MOVE CADPRO-ULT-VENDA      TO CAP012-ULT-VENDA
           MOVE CADPRO-PRECO-VENDA    TO CAP012-PRECO-VENDA
           MOVE CADPRO-PERC-LUCRO     TO CAP012-PERC-LUCRO
           MOVE CADPRO-PESO           TO CAP012-PESO
           MOVE CADPRO-PERC-COMIS     TO CAP012-PERC-COMIS
           MOVE CADPRO-VLR-COMIS      TO CAP012-VLR-COMIS
           MOVE CADPRO-CLASSIF-FISCAL TO CAP012-CLASSIF-FISCAL
           MOVE CADPRO-FORN-PREF      TO CAP012-FORN-PREF
                                         CODIGO-CG01
           READ CGD001 INVALID KEY
                INITIALIZE REG-CGD001.
           MOVE NOME-CG01             TO CAP012-DESC-FORN-PREF

           MOVE CADPRO-ULT-FORN       TO CAP012-ULT-FORN
                                         CODIGO-CG01
           READ CGD001 INVALID KEY
                INITIALIZE REG-CGD001.
           MOVE NOME-CG01             TO CAP012-DESC-ULT-FORN

           MOVE CADPRO-APLICACAO      TO CAP012-APLICACAO

           IF CADPRO-COMPRA IS NOT NUMERIC
              MOVE 0                  TO CADPRO-COMPRA
           END-IF
           MOVE CADPRO-COMPRA         TO CAP012-COMPRA

           IF CADPRO-VENDA IS NOT NUMERIC
              MOVE 0                  TO CADPRO-VENDA
           END-IF
           MOVE CADPRO-VENDA          TO CAP012-VENDA
           .
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CADPRO
           MOVE CAP012-ORDER TO ORDEM-W
           INITIALIZE CAP012-DATA-BLOCK
           MOVE ORDEM-W TO CAP012-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           close      cadpro
           open i-o   cadpro
           DELETE CADPRO.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
           close      cadpro
           open input cadpro.
       SALVAR-DADOS SECTION.
           close cadpro
           open i-o cadpro
           MOVE CAP012-CODIGO         TO CADPRO-CODIGO
           MOVE CAP012-MODELO         TO CADPRO-MODELO
           MOVE CAP012-COD-BARRAS     TO CADPRO-CODBARRAS
           MOVE CAP012-REFERENCIA     TO CADPRO-REFERENCIA
           MOVE CAP012-NOME           TO CADPRO-NOME
           MOVE CAP012-MODELO         TO CADPRO-MODELO
           MOVE CAP012-UNIDADE        TO CADPRO-UNIDADE
           MOVE CAP012-GRUPO          TO CADPRO-GRUPO
           MOVE CAP012-SEGMENTO       TO CADPRO-SEGMENTO
           MOVE CAP012-QTDE-MINIMA    TO CADPRO-QTDE-MINIMA
           MOVE CAP012-QTDE-ESTOQUE   TO CADPRO-QTDE-ESTOQUE
           MOVE CAP012-ICMS           TO CADPRO-ICMS
           MOVE CAP012-IPI            TO CADPRO-IPI
           MOVE CAP012-ULT-COMPRA     TO CADPRO-ULT-COMPRA
           MOVE CAP012-CUSTO-COMPRA   TO CADPRO-CUSTO-COMPRA
           MOVE CAP012-CUSTO-MEDIO    TO CADPRO-CUSTO-MEDIO
           MOVE CAP012-ULT-VENDA      TO CADPRO-ULT-VENDA
           MOVE CAP012-PRECO-VENDA    TO CADPRO-PRECO-VENDA
           MOVE CAP012-PERC-LUCRO     TO CADPRO-PERC-LUCRO
           MOVE CAP012-PESO           TO CADPRO-PESO
           MOVE CAP012-PERC-COMIS     TO CADPRO-PERC-COMIS
           MOVE CAP012-VLR-COMIS      TO CADPRO-VLR-COMIS
           MOVE CAP012-CLASSIF-FISCAL TO CADPRO-CLASSIF-FISCAL
           MOVE CAP012-FORN-PREF      TO CADPRO-FORN-PREF
           MOVE CAP012-ULT-FORN       TO CADPRO-ULT-FORN
           MOVE CAP012-APLICACAO      TO CADPRO-APLICACAO
           MOVE CAP012-COMPRA         TO CADPRO-COMPRA
           MOVE CAP012-VENDA          TO CADPRO-VENDA

           IF GRAVA-W = 1
              WRITE REG-CADPRO INVALID KEY
                   PERFORM ERRO-GRAVACAO
              END-WRITE
           ELSE
              REWRITE REG-CADPRO INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
           close      cadpro
           open input cadpro.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CAP012-MENSAGEM-ERRO
           MOVE ST-CADPRO       TO CAP012-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CAP012-ORDER = ZEROS
              MOVE SPACES TO CADPRO-NOME
              START CADPRO KEY IS NOT < CADPRO-CH-NOME
                    INVALID KEY MOVE "10" TO ST-CADPRO
           ELSE
              MOVE ZEROS TO CADPRO-CODIGO
               START CADPRO KEY IS NOT < CADPRO-CHAVE
                 INVALID KEY MOVE "10" TO ST-CADPRO.

           MOVE SPACES TO CAP012-LINDET.
           MOVE ZEROS TO CAP012-CONT
                         CAP012-CODIGO
           PERFORM UNTIL ST-CADPRO = "10"
              READ CADPRO NEXT RECORD AT END
                   MOVE "10" TO ST-CADPRO
              NOT AT END
                   ADD 1                  TO CAP012-CONT
      *            MOVE SPACES            TO CAP012-LINDET
                   MOVE CADPRO-CODIGO     TO CAP012-LINDET(01: 06)
                                             CAP012-CODIGO
                   MOVE CADPRO-NOME       TO CAP012-LINDET(08: 40)
                   MOVE CADPRO-MODELO     TO CADMOD-CODIGO
                   READ CADMOD INVALID KEY
                        MOVE SPACES       TO CADMOD-NOME
                   END-READ
                   MOVE CADPRO-MODELO     TO CAP012-LINDET(49:04)
                   MOVE CADMOD-NOME       TO CAP012-LINDET(54:20)
                   MOVE "INSERE-LIST"     TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM
           ADD 1 TO CAP012-CODIGO.

       CLEAR-FLAGS SECTION.
           INITIALIZE CAP012-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CADPRO" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           IF CAP012-ORDER = 1
              MOVE ZEROS TO CADPRO-CODIGO
              START CADPRO KEY IS NOT < CADPRO-CHAVE INVALID KEY
                           MOVE "10" TO ST-CADPRO
           ELSE
              MOVE SPACES TO CADPRO-NOME
              START CADPRO KEY IS NOT < CADPRO-CH-NOME INVALID KEY
                    MOVE "10" TO ST-CADPRO.
           MOVE ZEROS TO LIN
           PERFORM CABECALHO
           PERFORM UNTIL ST-CADPRO = "10"
             READ CADPRO NEXT RECORD AT END
                  MOVE "10" TO ST-CADPRO
             NOT AT END
                  MOVE SPACES             TO LINDET-REL
                  MOVE CADPRO-CODIGO      TO LINDET-REL(01: 06)
                  MOVE CADPRO-NOME        TO LINDET-REL(08: 40)
                   MOVE CADPRO-MODELO     TO CADMOD-CODIGO
                   READ CADMOD INVALID KEY
                        MOVE SPACES       TO CADMOD-NOME
                   END-READ
                   MOVE CADMOD-NOME       TO CAP012-LINDET(49:20)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56
                     PERFORM CABECALHO
                  END-IF
             END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       ACHAR-CODIGO SECTION.
           INITIALIZE REG-CADPRO
           MOVE ALL "9" TO CADPRO-CODIGO
           MOVE ZEROS   TO ULT-CODIGO
           START CADPRO KEY IS LESS THAN CADPRO-CHAVE NOT INVALID KEY
                READ CADPRO PREVIOUS WITH IGNORE LOCK NOT AT END
                     MOVE CADPRO-CODIGO TO ULT-CODIGO.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1  TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CAP012-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1          TO GRAVA-W.
           MOVE ULT-CODIGO TO CAP012-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CADASTRAR SECTION.
           EVALUATE CAP012-CAMPO-CRITICA
               WHEN "EF-MODELO"     PERFORM NAVEGACAO-MODELO
               WHEN "EF-GRUPO"      PERFORM NAVEGACAO-GRUPO
               WHEN "EF-SEGMENTO"   PERFORM NAVEGACAO-SEGMENTO
               WHEN "EF-FORN-PREF"  PERFORM NAVEGACAO-FORN-PREF
               WHEN "EF-ULT-FORN"   PERFORM NAVEGACAO-ULT-FORN
               WHEN OTHER           MOVE "Navegação Inexistente" TO
                                          MENSAGEM
                                    MOVE "C" TO TIPO-MSG
                                    PERFORM 140-EXIBIR-MENSAGEM.

       NAVEGACAO-MODELO SECTION.
           MOVE "CADMOD"      TO NOME-PROG
           MOVE NOME-PROG     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão de Acesso" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                move nome-prog            to command-line2
                move PARAMETROS-W         to command-line2(10: 77)
                move 87                   to command-line-len
                call "CBL_EXEC_RUN_UNIT"  using        command-line2
                                          by value     command-line-len
                                          by reference run-unit-id
                                          by value     stack-size
                                                       flags
                                          by reference tty-cmd
                                          by value     tty-cmd-len
                                          returning    status-code
                END-CALL.

       NAVEGACAO-GRUPO SECTION.
           MOVE "CADGRU"      TO NOME-PROG
           MOVE NOME-PROG     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão de Acesso" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                move nome-prog            to command-line2
                move PARAMETROS-W         to command-line2(10: 77)
                move 87                   to command-line-len
                call "CBL_EXEC_RUN_UNIT"  using        command-line2
                                          by value     command-line-len
                                          by reference run-unit-id
                                          by value     stack-size
                                                       flags
                                          by reference tty-cmd
                                          by value     tty-cmd-len
                                          returning    status-code
                END-CALL.

       NAVEGACAO-SEGMENTO SECTION.
           MOVE "CADSEG"      TO NOME-PROG
           MOVE NOME-PROG     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão de Acesso" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                move nome-prog            to command-line2
                move PARAMETROS-W         to command-line2(10: 77)
                move 87                   to command-line-len
                call "CBL_EXEC_RUN_UNIT"  using        command-line2
                                          by value     command-line-len
                                          by reference run-unit-id
                                          by value     stack-size
                                                       flags
                                          by reference tty-cmd
                                          by value     tty-cmd-len
                                          returning    status-code
                END-CALL.

       NAVEGACAO-FORN-PREF SECTION.
           MOVE "CGP001"      TO NOME-PROG
           MOVE NOME-PROG     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão de Acesso" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                move nome-prog            to command-line2
                move PARAMETROS-W         to command-line2(10: 77)
                move 87                   to command-line-len
                call "CBL_EXEC_RUN_UNIT"  using        command-line2
                                          by value     command-line-len
                                          by reference run-unit-id
                                          by value     stack-size
                                                       flags
                                          by reference tty-cmd
                                          by value     tty-cmd-len
                                          returning    status-code
                END-CALL.

       NAVEGACAO-ULT-FORN SECTION.
           MOVE "CGP001"      TO NOME-PROG
           MOVE NOME-PROG     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "Usuário Sem Permissão de Acesso" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                move nome-prog            to command-line2
                move PARAMETROS-W         to command-line2(10: 77)
                move 87                   to command-line-len
                call "CBL_EXEC_RUN_UNIT"  using        command-line2
                                          by value     command-line-len
                                          by reference run-unit-id
                                          by value     stack-size
                                                       flags
                                          by reference tty-cmd
                                          by value     tty-cmd-len
                                          returning    status-code
                END-CALL.


       exibir-mensagem section.
           move spaces to resp-msg
           call   "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel "MENSAGEM".
       exibir-mensagem-fim.
           exit.

       POPUP SECTION.
           EVALUATE CAP012-CAMPO-CRITICA
               WHEN "EF-MODELO"     PERFORM SUGESTAO-MODELO
               WHEN "EF-GRUPO"      PERFORM SUGESTAO-GRUPO
               WHEN "EF-SEGMENTO"   PERFORM SUGESTAO-SEGMENTO
               WHEN "EF-FORN-PREF"  PERFORM SUGESTAO-FORN-PREF
               WHEN "EF-ULT-FORN"   PERFORM SUGESTAO-ULT-FORN
               WHEN OTHER           MOVE "Sugestão Inexistente" TO
                                          MENSAGEM
                                    MOVE "C" TO TIPO-MSG
                                    PERFORM 140-EXIBIR-MENSAGEM.

       SUGESTAO-MODELO SECTION.
           CALL   "CONMOD" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CONMOD".
           MOVE PASSAR-STRING-1(33: 4) TO CAP012-MODELO
           MOVE PASSAR-STRING-1(1: 30) TO CAP012-DESC-MODELO
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-GRUPO SECTION.
           CALL   "CONGRU" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CONGRU".
           MOVE PASSAR-STRING-1(33: 4) TO CAP012-GRUPO
           MOVE PASSAR-STRING-1(1: 30) TO CAP012-DESC-GRUPO
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-SEGMENTO SECTION.
           CALL   "CONSEG" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CONSEG".
           MOVE PASSAR-STRING-1(33: 4) TO CAP012-SEGMENTO
           MOVE PASSAR-STRING-1(1: 30) TO CAP012-DESC-SEGMENTO
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-FORN-PREF SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO CAP012-DESC-FORN-PREF
           MOVE PASSAR-STRING-1(33: 6) TO CAP012-FORN-PREF
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-ULT-FORN SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO CAP012-DESC-ULT-FORN
           MOVE PASSAR-STRING-1(33: 6) TO CAP012-ULT-FORN
           REFRESH-OBJECT PRINCIPAL.

       VALIDAR-DADOS SECTION.
           EVALUATE CAP012-CAMPO-CRITICA
               WHEN "EF-COD-BARRAS"      PERFORM CRITICAR-CODBARRAS
               WHEN "EF-MODELO"          PERFORM CRITICAR-MODELO
               WHEN "EF-NOME"            PERFORM CRITICAR-NOME
               WHEN "EF-UNIDADE"         PERFORM CRITICAR-UNIDADE
               WHEN "EF-GRUPO"           PERFORM CRITICAR-GRUPO
               WHEN "EF-SEGMENTO"        PERFORM CRITICAR-SEGMENTO
               WHEN "EF-QTDE-MINIMA"     PERFORM CRITICAR-QTDE-MINIMA
               WHEN "EF-QTDE-ESTOQUE"    PERFORM CRITICAR-QTDE-ESTOQUE
               WHEN "EF-ICMS"            PERFORM CRITICAR-ICMS
               WHEN "EF-IPI"             PERFORM CRITICAR-IPI
               WHEN "EF-ULT-COMPRA"      PERFORM CRITICAR-ULT-COMPRA
               WHEN "EF-CUSTO-COMPRA"    PERFORM CRITICAR-CUSTO-COMPRA
               WHEN "EF-CUSTO-MEDIO"     PERFORM CRITICAR-CUSTO-MEDIO
               WHEN "EF-ULT-VENDA"       PERFORM CRITICAR-ULT-VENDA
               WHEN "EF-PRECO-VENDA"     PERFORM CRITICAR-PRECO-VENDA
               WHEN "EF-PERC-LUCRO"      PERFORM CRITICAR-PERC-LUCRO
               WHEN "EF-PESO"            PERFORM CRITICAR-PESO
               WHEN "EF-PERC-COMIS"      PERFORM CRITICAR-PERC-COMIS
               WHEN "EF-VLR-COMIS"       PERFORM CRITICAR-VLR-COMIS
               WHEN "EF-CLASSIF-FISCAL"  PERFORM CRITICAR-CLASSIF-FIS
               WHEN "EF-FORN-PREF"       PERFORM CRITICAR-FORN-PREF
               WHEN "EF-ULT-FORN"        PERFORM CRITICAR-ULT-FORN
               WHEN "MLE-APLICACAO"      PERFORM CRITICAR-APLICACAO
           END-EVALUATE
           MOVE SPACES TO MENSAGEM.

       CRITICAR-CODBARRAS SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-COD-BARRAS EQUAL ZEROS
                 STRING CAP012-CODIGO CAP012-MODELO INTO WSCODBARRA
                 PERFORM DIGITO-VERIFICADOR

                 IF WS-DIGITO = 10
                    STRING WSCODBARRA "0"     INTO CAP012-COD-BARRAS
                 ELSE
                    STRING WSCODBARRA
                           WS-DIGITO(2:1) INTO CAP012-COD-BARRAS
                 END-IF
                 REFRESH-OBJECT PRINCIPAL.

       CRITICAR-MODELO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-MODELO EQUAL ZEROS
                 MOVE SPACES TO CAP012-DESC-MODELO
                 REFRESH-OBJECT PRINCIPAL
      *          MOVE "Modelo do Produto Não Informado" TO MENSAGEM
      *          MOVE "C" TO TIPO-MSG
      *          PERFORM 140-EXIBIR-MENSAGEM
      *          SET-FOCUS EF-MODELO
              ELSE
                 MOVE CAP012-MODELO TO CADMOD-CODIGO
                 READ CADMOD INVALID KEY
                      MOVE "Modelo Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                      SET-FOCUS EF-GRUPO
                 NOT INVALID KEY
                      MOVE CADMOD-NOME TO CAP012-DESC-MODELO
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-NOME SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-NOME EQUAL SPACES
                 MOVE "Nome do Produto Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
                 SET-FOCUS EF-NOME.

       CRITICAR-UNIDADE SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-UNIDADE EQUAL SPACES
                 MOVE "Unidade Não Informada" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
                 SET-FOCUS EF-UNIDADE.

       CRITICAR-GRUPO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-GRUPO EQUAL ZEROS
                 MOVE "Grupo do Produto Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
                 SET-FOCUS EF-GRUPO
              ELSE
                 MOVE CAP012-GRUPO TO CADGRU-CODIGO
                 READ CADGRU INVALID KEY
                      MOVE "Grupo Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                      SET-FOCUS EF-GRUPO
                 NOT INVALID KEY
                      MOVE CADGRU-NOME TO CAP012-DESC-GRUPO
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-SEGMENTO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-SEGMENTO EQUAL ZEROS
                 MOVE "Segmento do Produto Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
                 SET-FOCUS EF-SEGMENTO
              ELSE
                 MOVE CAP012-SEGMENTO TO CADSEG-CODIGO
                 READ CADSEG INVALID KEY
                      MOVE "Segmento Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                      SET-FOCUS EF-SEGMENTO
                 NOT INVALID KEY
                      MOVE CADSEG-NOME TO CAP012-DESC-SEGMENTO
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-QTDE-MINIMA SECTION.

       CRITICAR-QTDE-ESTOQUE SECTION.

       CRITICAR-ICMS SECTION.

       CRITICAR-IPI SECTION.

       CRITICAR-ULT-COMPRA SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-ULT-COMPRA > ZEROS
                 CALL   "UTIVLDT" USING CAP012-ULT-COMPRA WS-OK
                 CANCEL "UTIVLDT"
                 IF WS-OK EQUAL "N"
                    MOVE "Data da Última Compra Inválida" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM 140-EXIBIR-MENSAGEM
                    SET-FOCUS EF-ULT-COMPRA.

       CRITICAR-CUSTO-COMPRA SECTION.

       CRITICAR-CUSTO-MEDIO SECTION.

       CRITICAR-ULT-VENDA SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-ULT-VENDA > ZEROS
                 CALL   "UTIVLDT" USING CAP012-ULT-COMPRA WS-OK
                 CANCEL "UTIVLDT"
                 IF WS-OK EQUAL "N"
                    MOVE "Data da Última Venda Inválida" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM 140-EXIBIR-MENSAGEM
                    SET-FOCUS EF-ULT-VENDA.

       CRITICAR-PRECO-VENDA SECTION.

       CRITICAR-PERC-LUCRO SECTION.

       CRITICAR-PESO SECTION.

       CRITICAR-PERC-COMIS SECTION.

       CRITICAR-VLR-COMIS SECTION.

       CRITICAR-CLASSIF-FIS SECTION.

       CRITICAR-FORN-PREF SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-FORN-PREF EQUAL ZEROS
                 MOVE SPACES TO CAP012-DESC-FORN-PREF
                 REFRESH-OBJECT PRINCIPAL
              ELSE
                 MOVE CAP012-FORN-PREF TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE "Fornecedor Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                      SET-FOCUS EF-FORN-PREF
                 NOT INVALID KEY
                      MOVE NOME-CG01 TO CAP012-DESC-FORN-PREF
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-ULT-FORN SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CAP012-ULT-FORN EQUAL ZEROS
                 MOVE SPACES TO CAP012-DESC-ULT-FORN
                 REFRESH-OBJECT PRINCIPAL
              ELSE
                 MOVE CAP012-ULT-FORN TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE "Fornecedor Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                      SET-FOCUS EF-ULT-FORN
                 NOT INVALID KEY
                      MOVE NOME-CG01 TO CAP012-DESC-ULT-FORN
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-APLICACAO SECTION.


       DIGITO-VERIFICADOR SECTION.

           MOVE 3                  TO WSFATOR
           MOVE ZEROS              TO WSSOMA
           MOVE ZEROS              TO WS-IND

           PERFORM 12 TIMES
              ADD 1                        TO WS-IND
              COMPUTE WSFATOR = 4 - WSFATOR
              MOVE WSCODBARRA(WS-IND:1)    TO WS-NUMERO
              COMPUTE WSSOMA = WSSOMA + (WS-NUMERO * WSFATOR)
           END-PERFORM

           COMPUTE WS-DIGITO = WSSOMA / 10

           COMPUTE WS-DIGITO = ((1 + WS-DIGITO ) * 10) - WSSOMA.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to cap012-flag-critica
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP012-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CADPRO"            to logacess-programa
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
           CLOSE CAD004 CADPRO CADGRU CADSEG CADMOD CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

