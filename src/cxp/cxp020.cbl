       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP020.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX020.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP020.CPB".
           COPY "CXP020.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  CODIG                 PIC 9.99.99.99.
           05  LIN-DETALHE-W         PIC X(80)    VALUE SPACES.
           05  ULT-CODIGO            PIC 9(5)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
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
           05  CONTA-APURAC.
               10  GRAU1   PIC 9.
               10  GRAU2   PIC 99.
               10  GRAU3   PIC 99.
               10  GRAU4   PIC 99.
           05  CONTA-APURACAO REDEFINES CONTA-APURAC PIC 9(7).
           05  CODIGO-E            PIC Z.ZZ.ZZ.ZZ.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO DO PLANO DE CONTAS".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP020-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CXP020-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP020-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP020-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN I-O CXD020
           MOVE 1 TO GRAVA-W.
           IF ST-CXD020 = "35"
              CLOSE CXD020      OPEN OUTPUT CXD020
              CLOSE CXD020      OPEN I-O CXD020
           END-IF.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP020-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP020-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP020-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
                MOVE 1 TO CXP020-ORDER
                PERFORM ACHAR-CODIGO
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP020-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP020-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM INCREMENTA-CODIGO
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP020-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP020-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP020-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP020-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN CXP020-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CXP020-CARREGA-LIST-BOX-TRUE
                   MOVE CXP020-LINDET(01: 05) TO CXP020-CODIGO-REDUZ
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
           MOVE CXP020-CODIGO-REDUZ TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY INITIALIZE REG-CXD020
                                   MOVE 1 TO GRAVA-W.
           MOVE CXP020-CODIGO-REDUZ TO CODIGO-REDUZ-CX20.
           MOVE CODIGO-REDUZ-CX20   TO CXP020-CODIGO-REDUZ
           MOVE CODIGO-COMPL-CX20   TO CXP020-CODIGO-COMPL
           MOVE SPACES TO CXP020-DESCRICAO
           MOVE DESCRICAO-CX20    TO CXP020-DESCRICAO
           IF TIPO-CONTA-CX20 = 0 MOVE 1 TO CXP020-TIPO-CONTA
           ELSE MOVE 0 TO CXP020-TIPO-CONTA.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CXD020
           MOVE CXP020-ORDER TO ORDEM-W
           INITIALIZE CXP020-DATA-BLOCK
           MOVE ORDEM-W TO CXP020-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE CXD020.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE CXP020-CODIGO-REDUZ TO CODIGO-REDUZ-CX20
           MOVE CXP020-CODIGO-COMPL TO CODIGO-COMPL-CX20
           MOVE CXP020-DESCRICAO    TO DESCRICAO-CX20
           IF CXP020-TIPO-CONTA = 1 MOVE 0 TO TIPO-CONTA-CX20
           ELSE MOVE 1 TO TIPO-CONTA-CX20.
           MOVE CXP020-CODIGO-COMPL TO CONTA-APURACAO.
           IF GRAU4 <> ZEROS
              MOVE 4 TO GRAU-CX20
           ELSE
              IF GRAU3 <> ZEROS
                 MOVE 3 TO GRAU-CX20
              ELSE
                  IF GRAU2 <> ZEROS
                     MOVE 2 TO GRAU-CX20
                  ELSE
                     MOVE 1 TO GRAU-CX20
                  END-IF
              END-IF
           END-IF.
           IF GRAVA-W = 1
              WRITE REG-CXD020 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE
              REWRITE REG-CXD020 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CXP020-MENSAGEM-ERRO
           MOVE ST-CXD020       TO CXP020-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF CXP020-ORDER = ZEROS
              MOVE SPACES TO DESCRICAO-CX20
              START CXD020 KEY IS NOT < DESCRICAO-CX20
                    INVALID KEY MOVE "10" TO ST-CXD020
           ELSE
             MOVE ZEROS TO CODIGO-COMPL-CX20
               START CXD020 KEY IS NOT < CODIGO-COMPL-CX20
                 INVALID KEY MOVE "10" TO ST-CXD020.
           MOVE SPACES TO CXP020-LINDET.
           MOVE ZEROS TO CXP020-CONT.
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD020
              NOT AT END
                   ADD 1 TO CXP020-CONT
                   MOVE SPACES            TO CXP020-LINDET
                   MOVE CODIGO-REDUZ-CX20 TO CXP020-LINDET (01: 06)
                   MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                   MOVE CODIGO-E          TO CXP020-LINDET (07: 11)
                   MOVE DESCRICAO-CX20    TO CXP020-LINDET (18: 31)
                   MOVE TIPO-CONTA-CX20   TO CXP020-LINDET (49: 02)
                   IF TIPO-CONTA-CX20 = 0
                      MOVE "NORMAL      " TO CXP020-LINDET(52: 12)
                   ELSE
                      MOVE "TOTALIZADORA" TO CXP020-LINDET(52: 12)
                   END-IF
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CXP020-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP020" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE ZEROS TO CODIGO-COMPL-CX20.
           START CXD020 KEY IS NOT < CODIGO-COMPL-CX20 INVALID KEY
                        MOVE "10" TO ST-CXD020.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CXD020 = "10"
             READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
              NOT AT END
              IF TIPO-CONTA-CX20 = 1
                 MOVE SPACES TO REG-RELAT
                 WRITE REG-RELAT
                 ADD 1 TO LIN
              END-IF
               MOVE SPACES TO LIN-DETALHE-W LINDET-REL
               MOVE CODIGO-COMPL-CX20 TO CODIGO-E
               MOVE CODIGO-E          TO LIN-DETALHE-W(01: 10)
               MOVE "("               TO LIN-DETALHE-W(11: 01)
               MOVE CODIGO-REDUZ-CX20 TO LIN-DETALHE-W(12: 05)
               MOVE ")"               TO LIN-DETALHE-W(17: 01)
               MOVE DESCRICAO-CX20    TO LIN-DETALHE-W(21: 33)
               EVALUATE GRAU-CX20
                 WHEN 1 MOVE LIN-DETALHE-W TO LINDET-REL(01: 48)
                 WHEN 2 MOVE LIN-DETALHE-W TO LINDET-REL(11: 48)
                 WHEN 3 MOVE LIN-DETALHE-W TO LINDET-REL(21: 48)
                 WHEN 4 MOVE LIN-DETALHE-W TO LINDET-REL(31: 48)
               END-EVALUATE
               IF TIPO-CONTA-CX20 = ZEROS
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
               ELSE WRITE REG-RELAT FROM LINDET AFTER 2
                    ADD 2 TO LIN
               END-IF
               IF LIN > 56 PERFORM CABECALHO
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
           MOVE 4 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE ZEROS TO CODIGO-REDUZ-CX20 ULT-CODIGO
           START CXD020 KEY IS NOT < CODIGO-REDUZ-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020
           END-START
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
                NOT AT END
                 MOVE CODIGO-REDUZ-CX20 TO ULT-CODIGO
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1 TO ULT-CODIGO.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CXP020-CODIGO-REDUZ.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1 TO GRAVA-W.
           MOVE ULT-CODIGO TO CXP020-CODIGO-REDUZ
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP020-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD020.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

