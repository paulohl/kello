       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IEP010.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 23/07/1999
      *DESCRIÇÃO: Cadastro de Instituição de Ensino
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA
       PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY IEPX010.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY IEPW010.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "IEP010.CPB".
           COPY "IEP010.CPY".
           COPY "CBPRINT.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(8).
           05  ULT-CODIGO            PIC 9(5)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DDDFONE-E             PIC 99.999.9999 BLANK WHEN ZEROS.
           05  DATA-E                PIC 99/99/9999.
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

       01  CAB01.
           05  EMPRESA-REL         PIC X(112)  VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(115)   VALUE
           "RELACAO DE INSTITUICAO DE ENSINO         ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)   VALUE
           "CODIG DP NE NOME                                     SIGLA
      -    "    ENDERECO                      NR-END COMPLEMENTO     BAI
      -    "RRO         ".
       01  CAB05.
           05  FILLER              PIC X(132)   VALUE
           "CIDADE                           UF C.E.P.   CX.POST. DDD-FO
      -    "NE  FAX       RAMAL INICIO-DE  E-MAIL
      -    "            ".

       01  LINDET.
           05  LINDET-REL          PIC X(132)   VALUE SPACES.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           OPEN I-O IED010
           MOVE 1 TO GRAVA-W.
           IF ST-IED010 = "35"
              CLOSE IED010      OPEN OUTPUT IED010
              CLOSE IED010      OPEN I-O IED010
           END-IF.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
                MOVE 1 TO GS-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 5) TO GS-CODIGO
                   PERFORM CARREGAR-DADOS
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
           MOVE GS-CODIGO       TO CODIGO-IE10.
           READ IED010 INVALID KEY INITIALIZE REG-IED010
                                   MOVE 1 TO GRAVA-W
           NOT INVALID KEY
             EVALUATE DEP-EXT-IE10
               WHEN 01 MOVE "1-Estadual  " TO GS-DEP-EXT
               WHEN 02 MOVE "2-Federal   " TO GS-DEP-EXT
               WHEN 03 MOVE "3-Municipal " TO GS-DEP-EXT
               WHEN 04 MOVE "4-Particular" TO GS-DEP-EXT
               WHEN OTHER MOVE "****"       TO GS-DEP-EXT
             END-EVALUATE
             EVALUATE NATUR-EXT-IE10
               WHEN 01 MOVE "1-Universidade  " TO GS-NATUR-EXT
               WHEN 02 MOVE "2-Faculdade     " TO GS-NATUR-EXT
               WHEN 03 MOVE "3-Estab.Isolado " TO GS-NATUR-EXT
               WHEN 04 MOVE "4-Fac.Integrada " TO GS-NATUR-EXT
               WHEN 05 MOVE "5-Federeção     " TO GS-NATUR-EXT
               WHEN OTHER MOVE "****"          TO GS-NATUR-EXT
             END-EVALUATE
             MOVE NOME-IE10         TO GS-NOME
             MOVE SIGLA-IE10        TO GS-SIGLA
             MOVE ENDERECO-IE10     TO GS-ENDERECO
             MOVE NR-END-IE10       TO GS-NR-ENDERECO
             MOVE COMPLEMENTO-IE10  TO GS-COMPLEMENTO
             MOVE BAIRRO-IE10       TO GS-BAIRRO
             MOVE CIDADE-IE10       TO GS-CIDADE
             MOVE ESTADO-IE10       TO GS-ESTADO
             MOVE CEP-IE10          TO GS-CEP
             MOVE CX-POSTAL-IE10    TO GS-CX-POSTAL
             MOVE DDDFONE-IE10      TO GS-DDDFONE
             MOVE FAX-IE10          TO GS-FAX
             MOVE RAMAL-IE10        TO GS-RAMAL
             MOVE INICIODE-IE10     TO GS-INICIO-DE
             MOVE SITE-IE10         TO GS-HOME-PAGE
             MOVE E-MAIL-IE10       TO GS-E-MAIL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-IED010
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE IED010.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-CODIGO          TO CODIGO-IE10
           MOVE GS-DEP-EXT(1: 1)  TO DEP-EXT-IE10
           MOVE GS-NATUR-EXT(1: 1) TO NATUR-EXT-IE10
           MOVE GS-NOME         TO NOME-IE10
           MOVE GS-SIGLA        TO SIGLA-IE10
           MOVE GS-ENDERECO     TO ENDERECO-IE10
           MOVE GS-NR-ENDERECO  TO NR-END-IE10
           MOVE GS-COMPLEMENTO  TO COMPLEMENTO-IE10
           MOVE GS-BAIRRO       TO BAIRRO-IE10
           MOVE GS-CIDADE       TO CIDADE-IE10
           MOVE GS-ESTADO       TO ESTADO-IE10
           MOVE GS-CEP          TO CEP-IE10
           MOVE GS-CX-POSTAL    TO CX-POSTAL-IE10
           MOVE GS-DDDFONE      TO DDDFONE-IE10
           MOVE GS-FAX          TO FAX-IE10
           MOVE GS-RAMAL        TO RAMAL-IE10
           MOVE GS-INICIO-DE    TO INICIODE-IE10
           MOVE GS-HOME-PAGE    TO SITE-IE10
           MOVE GS-E-MAIL       TO E-MAIL-IE10
           IF GRAVA-W = 1
              WRITE REG-IED010 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-IED010 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-IED010       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF GS-ORDER = ZEROS
              MOVE SPACES TO NOME-IE10
              START IED010 KEY IS NOT < NOME-IE10
                    INVALID KEY MOVE "10" TO ST-IED010
           ELSE
             MOVE ZEROS TO CODIGO-IE10
               START IED010 KEY IS NOT < CODIGO-IE10
                 INVALID KEY MOVE "10" TO ST-IED010.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-IED010 = "10"
              READ IED010 NEXT RECORD AT END MOVE "10" TO ST-IED010
              NOT AT END
      *         ADD 1 TO GS-CONT
                MOVE SPACES TO GS-LINDET
                MOVE CODIGO-IE10       TO GS-LINDET(01: 06)
                MOVE DEP-EXT-IE10      TO GS-LINDET(07: 03)
                MOVE NATUR-EXT-IE10    TO GS-LINDET(10: 03)
                MOVE NOME-IE10         TO GS-LINDET(13: 41)
                MOVE SIGLA-IE10        TO GS-LINDET(54: 11)
                MOVE CIDADE-IE10       TO GS-LINDET(65: 16)
                MOVE ESTADO-IE10       TO GS-LINDET(81: 03)
                MOVE DDDFONE-IE10      TO DDDFONE-E
                MOVE DDDFONE-E         TO GS-LINDET(84: 12)
                MOVE INICIODE-IE10     TO DATA-E
                MOVE DATA-E            TO GS-LINDET(96: 10)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "IEP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           COPY CONDENSA.

           IF GS-ORDER = 1
              MOVE ZEROS TO CODIGO-IE10
              START IED010 KEY IS NOT < CODIGO-IE10 INVALID KEY
                           MOVE "10" TO ST-IED010
           ELSE MOVE SPACES TO NOME-IE10
                START IED010 KEY IS NOT < NOME-IE10 INVALID KEY
                           MOVE "10" TO ST-IED010.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-IED010 = "10"
             READ IED010 NEXT RECORD AT END MOVE "10" TO ST-IED010
              NOT AT END
               MOVE SPACES TO LINDET-REL
                MOVE CODIGO-IE10       TO LINDET-REL(01: 06)
                MOVE DEP-EXT-IE10      TO LINDET-REL(07: 03)
                MOVE NATUR-EXT-IE10    TO LINDET-REL(10: 03)
                MOVE NOME-IE10         TO LINDET-REL(13: 41)
                MOVE SIGLA-IE10        TO LINDET-REL(54: 11)
                MOVE ENDERECO-IE10     TO LINDET-REL(65: 31)
                MOVE NR-END-IE10       TO LINDET-REL(96: 07)
                MOVE COMPLEMENTO-IE10  TO LINDET-REL(103: 16)
                MOVE BAIRRO-IE10       TO LINDET-REL(119: 13)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF

                MOVE CIDADE-IE10       TO LINDET-REL(01: 33)
                MOVE ESTADO-IE10       TO LINDET-REL(34: 03)
                MOVE CEP-IE10          TO LINDET-REL(37: 09)
                MOVE CX-POSTAL-IE10    TO LINDET-REL(46: 09)
                MOVE DDDFONE-IE10      TO DDDFONE-E
                MOVE DDDFONE-E         TO LINDET-REL(55: 11)
                MOVE FAX-IE10          TO DDDFONE-E
                MOVE DDDFONE-E         TO LINDET-REL(66: 11)
                MOVE RAMAL-IE10        TO LINDET-REL(77: 05)
                MOVE INICIODE-IE10     TO DATA-E
                MOVE DATA-E            TO LINDET-REL(82: 11)
                MOVE E-MAIL-IE10       TO LINDET-REL(93: 30)
                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
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
           WRITE REG-RELAT FROM CAB05.
           MOVE 6 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-IE10 ULT-CODIGO
           START IED010 KEY IS NOT < CODIGO-IE10 INVALID KEY
                 MOVE "10" TO ST-IED010
           END-START
           PERFORM UNTIL ST-IED010 = "10"
              READ IED010 NEXT RECORD AT END MOVE "10" TO ST-IED010
                NOT AT END
                 MOVE CODIGO-IE10 TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO GS-CODIGO
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED010.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
