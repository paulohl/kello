       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP002.
       AUTHOR. MARELI AMANCIO VOLPATO.
      *PROGRAMA: CADASTRO DE USUÁRIO
       ENVIRONMENT DIVISION.
       class-control.
           AListview          is class "alistview"
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX002.

           COPY CAPX002E.

           COPY CAPX002D.

           COPY CAPX004.

           COPY CAPX001.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW002.

           COPY CAPW002E.

           COPY CAPW002D.

           COPY CAPW004.

           COPY CAPW001.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).
       WORKING-STORAGE SECTION.
           COPY "CAP002.CPB".
           COPY "CAP002.CPY".
           COPY "DS-CNTRL.MF".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD002E            PIC XX       VALUE SPACES.
           05  ST-CAD002D            PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".
       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 wsItem                    PIC 9(09) COMP-5 VALUE ZEROS.
       01 AITEM                     OBJECT REFERENCE.
       01 AOBJETO                   OBJECT REFERENCE.
       01 WSCOLUNACODIGO            PIC 9(09) COMP-5 VALUE 1.
       01 wsindice                  PIC 9(09) COMP-5 VALUE ZEROS.
       01 wssizelistview            PIC 9(09) COMP-5 VALUE ZEROS.
       01 WSTEXTO                   PIC X(255) VALUE SPACES.
       01 PASSAR-PARAMETRO          PIC X(40)  VALUE SPACES.
       01 MASC-DATA                 PIC 99/99/9999 BLANK WHEN ZEROS.

       01 mensagem                  pic x(200).
       01 tipo-msg                  pic x(01).
       01 resp-msg                  pic x(01).

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu          pic 9(04).
             10 ws-mes-cpu          pic 9(02).
             10 ws-dia-cpu          pic 9(02).
         05 filler                  pic x(13).

       01 lnktabela.
          02 lnkobjetoscoluna  object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.
       01 indice           pic 9(02).

       01  CAB01.
           05  EMPRESA-REL         PIC X(98)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(108)   VALUE
           "RELACAO DE CADASTRO DE USUARIOS".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(118)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(04)   VALUE "COD".
           05  FILLER              PIC X(31)   VALUE "NOME".
           05  FILLER              PIC X(09)   VALUE "REDUZIDO".
           05  FILLER              PIC X(21)   VALUE "DEPARTAMENTO".
           05  FILLER              PIC X(11)   VALUE "IMPRESSORA".
           05  FILLER              PIC X(11)   VALUE "DT. CADAST".
           05  FILLER              PIC X(07)   VALUE "CADAST.".
           05  FILLER              PIC X(06)   VALUE "SOLIC".
           05  FILLER              PIC x(08)   VALUE "STATUS".
           05  FILLER              PIC X(10)   VALUE "DT. BLOQ.".
           COPY "CBPRINT.CPY".

       01  LINDET.
           05  LINDET-REL          PIC X(122)   VALUE SPACES.

       01  lnkusu.
           copy "usuario.cpy".

           COPY IMPRESSORA.

      *LINKAGE SECTION.
       01 PASSAR-PARAMETROS        PIC X(65).


      *PROCEDURE DIVISION USING PASSAR-PARAMETROS.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CAP002-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           INITIALIZE CAP002-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE ZEROS TO ERRO-W.
           MOVE CAP002-DATA-BLOCK-VERSION-NO
                                    TO DS-DATA-BLOCK-VERSION-NO
           MOVE CAP002-VERSION-NO   TO DS-VERSION-NO

           MOVE EMPRESA-W           TO EMP-REC
           MOVE NOME-EMPRESA-W      TO EMPRESA-REL

           MOVE "\PROGRAMA\KELLO\*" TO LNK-PATH-SIS
           MOVE "SUPERV"            TO LNK-USUARIO
           MOVE "PADRAO"            TO LNK-EMPRESA

           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.

           OPEN I-O   CAD002 CAD002D CAD002E CAD001 CAD004
           CLOSE      CAD002 CAD002D CAD002E CAD001 CAD004
           OPEN I-O   CAD002 CAD002D CAD002E CAD001
           CLOSE      CAD002 CAD002D CAD002E CAD001 CAD004
           OPEN INPUT CAD002 CAD002D CAD002E CAD001 CAD004

           MOVE 1 TO GRAVA-W.
           IF ST-CAD002 = "35"
              CLOSE CAD002      OPEN OUTPUT CAD002
              CLOSE CAD002      OPEN I-O    CAD002
           END-IF.
           IF ST-CAD002E = "35"
              CLOSE CAD002E     OPEN OUTPUT CAD002E
              CLOSE CAD002E     OPEN I-O    CAD002E
           END-IF.
           IF ST-CAD002D = "35"
              CLOSE CAD002D     OPEN OUTPUT CAD002D
              CLOSE CAD002D     OPEN I-O    CAD002D
           END-IF.
           IF ST-CAD001 = "35"
              CLOSE CAD001      OPEN OUTPUT CAD001
              CLOSE CAD001      OPEN I-O    CAD001
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-CAD004 TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-CAD002 TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002E <> "00"
              MOVE "ERRO ABERTURA CAD002E: "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-CAD002E TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002D <> "00"
              MOVE "ERRO ABERTURA CAD002D: "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-CAD002D TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD001 <> "00"
              MOVE "ERRO ABERTURA CAD001: "  TO CAP002-MENSAGEM-ERRO
              MOVE ST-CAD001 TO CAP002-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM ACHAR-CODIGO
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CAP002-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
               WHEN CAP002-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CAP002-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN CAP002-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP002-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP002-PRINTER-FLG-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CAP002-LE-USUARIO-TRUE
                    PERFORM LE-USUARIO
               WHEN CAP002-POPUP-USUARIO-TRUE
                    PERFORM POPUP-USUARIO
               WHEN CAP002-VALIDA-STATUS-TRUE
                    PERFORM VALIDA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       LE-USUARIO SECTION.
           MOVE CAP002-SOLICITANTE  TO NOME-REDUZ-CA002
           READ CAD002 KEY IS NOME-REDUZ-CA002 INVALID KEY
                MOVE "Usuário Solicitante Não Encontrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                SET-FOCUS EF5
           NOT INVALID KEY
                MOVE NOME-CA002    TO CAP002-DESC-SOLICITANTE.
                REFRESH-OBJECT PRINCIPAL
                SET-FOCUS SB-IMPRESSORA.

       POPUP-USUARIO SECTION.
           CALL   "CAP002T" USING PARAMETROS-W PASSAR-PARAMETRO.
           CANCEL "CAP002T".
           MOVE PASSAR-PARAMETRO(38: 3)TO CODIGO-CA002
           READ CAD002 INVALID KEY
                INITIALIZE REG-CAD002.

           MOVE NOME-REDUZ-CA002 TO CAP002-SOLICITANTE
           PERFORM LE-USUARIO.

       VALIDA-STATUS SECTION.
           IF CAP002-STATUS EQUAL SPACES
              MOVE "Ativo" TO CAP002-STATUS
              REFRESH-OBJECT PRINCIPAL
           ELSE
              IF CAP002-STATUS <> "Ativo" AND "Inativo"
                 move "Status Inválido" to mensagem
                 move "C"               to tipo-msg
                 perform exibir-mensagem
              ELSE
                 IF CAP002-STATUS = "Inativo"
                    move function current-date to ws-data-sys
                    string ws-dia-cpu ws-mes-cpu ws-ano-cpu into
                           cap002-data-bloqueio
                 ELSE
                    MOVE ZEROS TO CAP002-DATA-BLOQUEIO
                 END-IF
                 refresh-object principal
                 set-focus ef4.

       CRIAR-LISTVIEW SECTION.
          initialize indice
          add 1 to indice
          invoke cap002-listview-empresa "adicionarColunaZ"
                 using z"Código" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke cap002-listview-empresa "adicionarColunaZ"
                 using z"Nome"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke cap002-listview-empresa "adicionarColunaZ"
                 using z"Endereço" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke cap002-listview-empresa "adicionarColunaZ"
                using z"Cidade" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke cap002-listview-empresa "adicionarColunaZ"
              using z"UF"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

           invoke cap002-listview-empresa "gridLines"
           invoke cap002-listview-empresa "noBorder".
       CRIAR-LISTVIEW-FIM.
           EXIT.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GRAVA-W.
           MOVE CAP002-CODIGO     TO CODIGO-CA002
           READ CAD002 INVALID KEY
                INITIALIZE REG-CAD002
                MOVE 1            TO GRAVA-W.

           MOVE NOME-CA002        TO CAP002-NOME.
           MOVE NOME-REDUZ-CA002  TO CAP002-REDUZIDO.
           MOVE IMPRESSORA-CA002  TO CAP002-IMPRESSORA(01: 02).
           MOVE SENHA-CA002       TO CAP002-SENHA.
           EVALUATE IMPRESSORA-CA002
             WHEN 00 MOVE "01"    TO CAP002-IMPRESSORA(01: 02)
                     MOVE "HP   " TO CAP002-IMPRESSORA(05: 05)
             WHEN 01 MOVE "HP   " TO CAP002-IMPRESSORA(05: 05)
             WHEN 02 MOVE "EPSON" TO CAP002-IMPRESSORA(05: 05)
           END-EVALUATE

           MOVE CAP002-CODIGO     TO CODIGO-CA002D
           READ CAD002D INVALID KEY
                INITIALIZE REG-CAD002D.

           MOVE DATA-CADASTRO-CAD002D      TO CAP002-DATA-CADASTRO
           MOVE USUARIO-CADASTRO-CAD002D   TO CAP002-USUARIO-CADASTRO
           MOVE STATUS-CAD002D             TO CAP002-STATUS
           MOVE DATA-BLOQUEIO-CAD002D      TO CAP002-DATA-BLOQUEIO
           MOVE DEPARTAMENTO-CAD002D       TO CAP002-DEPARTAMENTO
           MOVE SOLICITANTE-CAD002D        TO CAP002-SOLICITANTE
           PERFORM LE-USUARIO

           INVOKE CAP002-LISTVIEW-EMPRESA "DeleteAll"

           INITIALIZE REG-CAD002E
           MOVE CAP002-CODIGO      TO COD-USUARIO-CA002E
           START CAD002E KEY IS NOT LESS CHAVE-USU-CA002E INVALID KEY
                MOVE "10" TO ST-CAD002E.

           PERFORM UNTIL ST-CAD002E = "10"
                READ CAD002E NEXT AT END
                     MOVE "10" TO ST-CAD002E
                NOT AT END
                     IF CAP002-CODIGO <> COD-USUARIO-CA002E
                        MOVE "10" TO ST-CAD002E
                     ELSE
                        MOVE EMPRESA-CA002E TO CODIGO-CA001
                        READ CAD001 NOT INVALID KEY
                             PERFORM MOVER-EMPRESAS
                        END-READ
                     END-IF
                END-READ
           END-PERFORM

           IF GRAVA-W = 1
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
              CAP002-DATA-CADASTRO
              MOVE USUARIO-W                  TO CAP002-USUARIO-CADASTRO
              MOVE "Ativo"                    TO CAP002-STATUS.


           perform mostrar-colunas-favoritas
           perform zebrar-itens.

       MOVER-EMPRESAS SECTION.
           initialize indice
           invoke cap002-listview-empresa "adicionarItem"
                                                     returning wsItem

           add 1 to indice
           initialize wsTexto
           string codigo-ca001 X"00"  into wsTexto
           invoke cap002-listview-empresa "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string nome-emp-ca001 X"00" delimited by "  " into wsTexto
           invoke cap002-listview-empresa "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string endereco-ca001 X"00" delimited by "  " into wsTexto
           invoke cap002-listview-empresa "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string cidade-ca001 X"00" delimited by "  " into wsTexto
           invoke cap002-listview-empresa "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string uf-ca001 X"00" delimited by "  " into wsTexto
           invoke cap002-listview-empresa "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto.
       MOVER-EMPRESAS-FIM.
           EXIT.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CAD002
           INITIALIZE CAP002-DATA-BLOCK
           INVOKE CAP002-LISTVIEW-EMPRESA "DeleteAll"
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           CLOSE      CAD002 CAD002D CAD002E CAD004
           OPEN I-O   CAD002 CAD002D CAD002E CAD004

           MOVE CAP002-CODIGO TO CODIGO-CA002
           READ CAD002 NOT INVALID KEY
                DELETE CAD002
           END-READ

           MOVE CAP002-CODIGO TO CODIGO-CA002D
           READ CAD002D NOT INVALID KEY
                DELETE CAD002D
           END-READ

           INITIALIZE REG-CAD002E
           MOVE CAP002-CODIGO      TO COD-USUARIO-CA002E
           START CAD002E KEY IS NOT LESS CHAVE-USU-CA002E INVALID KEY
                MOVE "10" TO ST-CAD002E.

           PERFORM UNTIL ST-CAD002E = "10"
                READ CAD002E NEXT AT END
                     MOVE "10" TO ST-CAD002E
                NOT AT END
                     IF CAP002-CODIGO <> COD-USUARIO-CA002E
                        MOVE "10" TO ST-CAD002E
                     ELSE
                        DELETE CAD002E
                     END-IF
                END-READ
           END-PERFORM

           INITIALIZE REG-CAD004
           MOVE CAP002-CODIGO TO COD-USUARIO-CA004
           START CAD004 KEY IS NOT LESS CHAVE-CA004 invalid key
                 MOVE "10" TO ST-CAD004.

           PERFORM UNTIL ST-CAD004 = "10"
                 READ CAD004 NEXT AT END
                      MOVE "10" TO ST-CAD004
                 NOT AT END
                      IF CAP002-CODIGO <> COD-USUARIO-CA004
                         MOVE "10" TO ST-CAD004
                      ELSE
                         delete cad004
                      END-IF
                 END-READ
           END-PERFORM

           CLOSE      CAD002 CAD002D CAD002E CAD004
           OPEN INPUT CAD002 CAD002D CAD002E CAD004

           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           CLOSE      CAD002 CAD002D
           OPEN I-O   CAD002 CAD002D

           MOVE CAP002-CODIGO     TO CODIGO-CA002
                PASSAR-PARAMETROS(38: 3)
           MOVE CAP002-NOME       TO NOME-CA002
           MOVE CAP002-REDUZIDO   TO NOME-REDUZ-CA002
           MOVE CAP002-IMPRESSORA(01: 02) TO IMPRESSORA-CA002
           MOVE CAP002-SENHA      TO SENHA-CA002
           IF GRAVA-W = 1
              WRITE REG-CAD002 INVALID KEY
                    ADD 1 TO ULT-CODIGO
                    MOVE ULT-CODIGO TO CODIGO-CA002
                         WRITE REG-CAD002
                         END-WRITE
              END-WRITE
           ELSE
              REWRITE REG-CAD002
                      SUBTRACT 1 FROM ULT-CODIGO
                      MOVE 1 TO GRAVA-W
           END-IF

           MOVE CODIGO-CA002     TO CODIGO-CA002D
           READ CAD002D INVALID KEY
                MOVE CAP002-DATA-CADASTRO    TO DATA-CADASTRO-CAD002D
                MOVE CAP002-USUARIO-CADASTRO TO USUARIO-CADASTRO-CAD002D
                MOVE CAP002-STATUS           TO STATUS-CAD002D
                MOVE CAP002-DATA-BLOQUEIO    TO DATA-BLOQUEIO-CAD002D
                MOVE CAP002-DEPARTAMENTO     TO DEPARTAMENTO-CAD002D
                MOVE CAP002-SOLICITANTE      TO SOLICITANTE-CAD002D

                WRITE REG-CAD002D INVALID KEY
                      MOVE "Erro de Gravação...CAD002D" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
                MOVE CAP002-DATA-CADASTRO    TO DATA-CADASTRO-CAD002D
                MOVE CAP002-USUARIO-CADASTRO TO USUARIO-CADASTRO-CAD002D
                MOVE CAP002-STATUS           TO STATUS-CAD002D
                MOVE CAP002-DATA-BLOQUEIO    TO DATA-BLOQUEIO-CAD002D
                MOVE CAP002-DEPARTAMENTO     TO DEPARTAMENTO-CAD002D
                MOVE CAP002-SOLICITANTE      TO SOLICITANTE-CAD002D

                REWRITE REG-CAD002D INVALID KEY
                      MOVE "Erro de Regravação...CAD002D" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ.

           CLOSE      CAD002 CAD002D
           OPEN INPUT CAD002 CAD002D.

       CLEAR-FLAGS SECTION.
           INITIALIZE CAP002-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP002" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.

           copy condensa.

           MOVE ZEROS TO CODIGO-CA002.
           START CAD002 KEY IS NOT < CODIGO-CA002 INVALID KEY
               MOVE "10" TO ST-CAD002.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CAD002 = "10"
               READ CAD002 NEXT RECORD AT END
                    MOVE "10" TO ST-CAD002
               NOT AT END
                    MOVE CODIGO-CA002             TO CODIGO-CA002D
                    READ CAD002D INVALID KEY
                         INITIALIZE REG-CAD002D
                    END-READ
                    MOVE SPACES TO LINDET-REL
                    MOVE CODIGO-CA002             TO LINDET-REL(01:04)
                    MOVE NOME-CA002               TO LINDET-REL(05:31)
                    MOVE NOME-REDUZ-CA002         TO LINDET-REL(36:09)
                    MOVE DEPARTAMENTO-CAD002D     TO LINDET-REL(45:20)
                    MOVE IMPRESSORA-CA002         TO LINDET-REL(66:11)
                    MOVE DATA-CADASTRO-CAD002D    TO MASC-DATA
                    MOVE MASC-DATA                TO LINDET-REL(77:11)
                    MOVE USUARIO-CADASTRO-CAD002D TO LINDET-REL(88:07)
                    MOVE SOLICITANTE-CAD002D      TO LINDET-REL(94:06)
                    MOVE STATUS-CAD002D           TO LINDET-REL(100:08)
                    MOVE DATA-BLOQUEIO-CAD002D    TO MASC-DATA
                    MOVE MASC-DATA                TO LINDET-REL(108:10)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 80
                       PERFORM CABECALHO
                    END-IF
               END-READ
           END-PERFORM.

           copy descondensa.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
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
           MOVE ZEROS TO CODIGO-CA002
                         ULT-CODIGO
           START CAD002 KEY IS NOT < CODIGO-CA002 INVALID KEY
                 MOVE "10" TO ST-CAD002
           END-START
           PERFORM UNTIL ST-CAD002 = "10"
                 READ CAD002 NEXT RECORD AT END
                      MOVE "10"              TO ST-CAD002
                 NOT AT END
                      MOVE CODIGO-CA002      TO ULT-CODIGO
                 END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE ULT-CODIGO TO CAP002-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE ULT-CODIGO TO CAP002-CODIGO
           MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
           CAP002-DATA-CADASTRO
           MOVE USUARIO-W                  TO CAP002-USUARIO-CADASTRO
           MOVE "Ativo"                    TO CAP002-STATUS.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CAP002-DATA-BLOCK.
               IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD002 CAD002D CAD002E CAD001 CAD004.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

       mostrar-colunas-favoritas section.
          initialize wsTexto
          move "listview-cap001-consulta" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  cap002-listview-empresa
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favoritas-fim.
           exit.

       zebrar-itens section.
           move "listview-cap001-consulta" to wsTexto
           invoke aListview "zebrarCor"
                          using lnkusu cap002-listview-empresa wsTexto
           invoke cap002-listview-empresa "redrawallitems".
       zebrar-itens-fim.
           exit.

