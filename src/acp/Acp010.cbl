       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACP010.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 14/03/2004
      *DESCRI��O: Cadastro de Tipo de atendimento � cliente
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY ACPX010.
           COPY ACPX020.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.

       FILE SECTION.
           COPY ACPW010.
           COPY ACPW020.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).

       WORKING-STORAGE SECTION.
           COPY "ACP010.CPB".
           COPY "ACP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-ACD020             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  TIPO-W                PIC X(13)    VALUE SPACES.
           05  TIPO1-W               PIC 9(03)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - num�rico
      *    ou alfab�tico
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
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-ASSUNTO           PIC 9(01).

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
           "RELACAO DE CADASTRO ATENDIMENTO A CLIENTE".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "TIPO                    COD DESCRI��O".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

           COPY IMPRESSORA.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
      *PROCEDURE DIVISION USING POP-UP.
       PROCEDURE DIVISION.

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
           MOVE GS-VERSION-NO      TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "ACD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD010.
           MOVE "ACD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD020.
           OPEN I-O ACD010 ACD020
           CLOSE    ACD010 ACD020
           OPEN I-O ACD010 ACD020
           MOVE 1 TO GRAVA-W.
           IF ST-ACD010 = "35"
              CLOSE ACD010      OPEN OUTPUT ACD010
              CLOSE ACD010      OPEN I-O ACD010
           END-IF.
           IF ST-ACD020 = "35"
              CLOSE ACD020      OPEN OUTPUT ACD020
              CLOSE ACD020      OPEN I-O ACD020
           END-IF.
           IF ST-ACD010 <> "00"
              MOVE "ERRO ABERTURA ACD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-ACD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-ACD020 <> "00"
              MOVE "ERRO ABERTURA ACD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-ACD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
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
               WHEN GS-VALIDA-ASSUNTO-TRUE
                   PERFORM VALIDAR-ASSUNTO
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM ACHAR-CODIGO
                   PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
                    PERFORM MOSTRA-ULT-CODIGO
               WHEN GS-CARREGA-SB-TRUE
                    PERFORM CARREGAR-SB
               WHEN GS-CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULTIMOS
                    PERFORM ACHAR-CODIGO
                    MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 13)         TO GS-TAB-TIPOS(1)
                   MOVE GS-LINDET(15: 3)         TO GS-CODIGO
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

       VALIDAR-ASSUNTO SECTION.
           MOVE GS-ASSUNTO(1:1) TO AUX-ASSUNTO
           IF AUX-ASSUNTO NOT NUMERIC
               MOVE "Assunto Inv�lido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO GS-FLAG-CRITICA.

       CARREGAR-SB SECTION.
           MOVE ZEROS           TO CODIGO-AC20 gs-cont
            START ACD020 KEY IS NOT < CHAVE-AC20
              INVALID KEY MOVE "10" TO ST-ACD020.

           PERFORM UNTIL ST-ACD020 = "10"
              READ ACD020 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD020
              NOT AT END
                   add 1 to gs-cont
                   MOVE SPACES TO GS-TAB-TIPOS(GS-CONT)
                   STRING CODIGO-AC20(3:1) "-" DESCRICAO-AC20 INTO
                   GS-TAB-TIPOS(GS-CONT)
              END-READ
           END-PERFORM
           MOVE "INSERE-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-DADOS SECTION.
           MOVE ZEROS                  TO GRAVA-W.
           MOVE GS-TAB-TIPOS(1)(1: 1)  TO TIPO-AC10
           MOVE GS-CODIGO              TO CODIGO-AC10
           READ ACD010 INVALID KEY
                INITIALIZE REG-ACD010
                MOVE 1 TO GRAVA-W.
           MOVE DESCRICAO-AC10    TO GS-DESCRICAO
           MOVE QTDE-DIAS-AC10    TO GS-QTDE-DIAS.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-ACD010
           MOVE GS-ORDER    TO ORDEM-W
           MOVE GS-TAB-TIPOS(1)     TO TIPO-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W     TO GS-ORDER
           MOVE TIPO-W      TO GS-TAB-TIPOS(1)
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE ACD010.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-TAB-TIPOS(1)(1: 1)   TO TIPO-AC10
           MOVE GS-CODIGO       TO CODIGO-AC10
           MOVE GS-DESCRICAO    TO DESCRICAO-AC10
           MOVE GS-QTDE-DIAS    TO QTDE-DIAS-AC10
           MOVE GS-ASSUNTO(1:1) TO AUX-ASSUNTO
           MOVE AUX-ASSUNTO     TO ASSUNTO-AC10.
           IF GRAVA-W = 1
              WRITE REG-ACD010 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-ACD010 INVALID KEY
                PERFORM ERRO-GRAVACAO
               NOT INVALID KEY
                SUBTRACT 1 FROM ULT-CODIGO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVA��O" TO GS-MENSAGEM-ERRO
           MOVE ST-ACD010       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO
           PERFORM ACHAR-CODIGO
           SUBTRACT 1 FROM ULT-CODIGO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-TAB-TIPOS(1)(1: 1)      TO TIPO-AC10
                                      TIPO1-W
           IF GS-ORDER = ZEROS
              MOVE SPACES          TO DESCRICAO-AC10
              START ACD010 KEY IS NOT < ALT-AC10
                    INVALID KEY MOVE "10" TO ST-ACD010
           ELSE
              MOVE ZEROS           TO CODIGO-AC10
               START ACD010 KEY IS NOT < CHAVE-AC10
                 INVALID KEY MOVE "10" TO ST-ACD010.

           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-ACD010 = "10"
              READ ACD010 NEXT RECORD AT END MOVE "10" TO ST-ACD010
              NOT AT END
      *         ADD 1                  TO GS-CONT
                IF TIPO-AC10 <> TIPO1-W
                   MOVE '10' TO ST-ACD010
                ELSE
                   MOVE SPACES                   TO GS-LINDET
                   MOVE TIPO-AC10                TO TIPO-AC20
                   MOVE SPACES TO GS-LINDET(1:20)
                   READ ACD020 INVALID KEY
                       STRING TIPO-AC10(3:1) "-INVALIDO" INTO
                                                         GS-LINDET(1:20)
                   NOT INVALID KEY
                       STRING TIPO-AC10(3:1) "-" DESCRICAO-AC20 INTO
                                                         GS-LINDET(1:20)
                   END-READ

                   MOVE CODIGO-AC10               TO GS-LINDET(25: 4)
                   MOVE DESCRICAO-AC10            TO GS-LINDET(29: 60)

                   EVALUATE ASSUNTO-AC10
                       WHEN 1
                            MOVE '1-MIN '         TO GS-LINDET(80: 6)
                       WHEN 2
                            MOVE '2-DCR '         TO GS-LINDET(80: 6)
                       WHEN 3
                            MOVE '3-O.S.'         TO GS-LINDET(80: 6)
                       WHEN 4
                            MOVE '4-DPT '         TO GS-LINDET(80: 6)
                       WHEN 5
                            MOVE '5-MEMO'         TO GS-LINDET(80: 6)
                       WHEN OTHER
                            MOVE 'ERRO  '         TO GS-LINDET(80: 6)
                   END-EVALUATE

                   MOVE QTDE-DIAS-AC10            TO GS-LINDET(88:3)

                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ACP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE GS-TAB-TIPOS(1)(1: 1)                  TO TIPO-AC10
                                                  TIPO1-W

           IF GS-ORDER = 1
              MOVE ZEROS                        TO CODIGO-AC10
              START ACD010 KEY IS NOT < CHAVE-AC10 INVALID KEY
                           MOVE "10" TO ST-ACD010
           ELSE MOVE SPACES                     TO DESCRICAO-AC10
                START ACD010 KEY IS NOT < ALT-AC10 INVALID KEY
                           MOVE "10" TO ST-ACD010.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-ACD010 = "10"
             READ ACD010 NEXT RECORD AT END MOVE "10" TO ST-ACD010
              NOT AT END
                IF TIPO-AC10 <> TIPO1-W
                   MOVE "10" TO ST-ACD010
                ELSE
                   MOVE SPACES                   TO LINDET-REL
                   MOVE TIPO-AC10                TO TIPO-AC20
                   MOVE SPACES TO GS-LINDET(1:20)
                   READ ACD020 INVALID KEY
                       STRING TIPO-AC10(3:1) "-INVALIDO" INTO
                                                    LINDET-REL(1:20)
                   NOT INVALID KEY
                       STRING TIPO-AC10(3:1) "-" DESCRICAO-AC20 INTO
                                                    LINDET-REL(1:20)
                   END-READ

                   MOVE CODIGO-AC10               TO LINDET-REL(25: 4)
                   MOVE DESCRICAO-AC10            TO LINDET-REL(29: 60)

                   EVALUATE ASSUNTO-AC10
                       WHEN 1
                            MOVE '1-MIN '         TO LINDET-REL(80: 6)
                       WHEN 2
                            MOVE '2-DCR '         TO LINDET-REL(80: 6)
                       WHEN 3
                            MOVE '3-O.S.'         TO LINDET-REL(80: 6)
                       WHEN 4
                            MOVE '4-DPT '         TO LINDET-REL(80: 6)
                       WHEN 5
                            MOVE '5-MEMO'         TO LINDET-REL(80: 6)
                       WHEN OTHER
                            MOVE 'ERRO  '         TO LINDET-REL(80: 6)
                   END-EVALUATE

                   MOVE QTDE-DIAS-AC10            TO LINDET-REL(88:3)

                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
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
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       ACHAR-CODIGO SECTION.
           MOVE GS-TAB-TIPOS(1)(1: 1)           TO TIPO-AC10
                                                  TIPO1-W
           MOVE ZEROS                           TO CODIGO-AC10
                                                   ULT-CODIGO
           START ACD010 KEY IS NOT < CHAVE-AC10 INVALID KEY
                 MOVE "10" TO ST-ACD010
           END-START
           PERFORM UNTIL ST-ACD010 = "10"
              READ ACD010 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD010
              NOT AT END
                   IF TIPO-AC10 <>  TIPO1-W
                      MOVE "10" TO ST-ACD010
                   ELSE
                       MOVE CODIGO-AC10  TO ULT-CODIGO
                   END-IF
              END-READ
           END-PERFORM.
           PERFORM INCREMENTA-CODIGO.
       INCREMENTA-CODIGO SECTION.
           ADD 1                                TO ULT-CODIGO
           MOVE 1                               TO GRAVA-W
           MOVE ULT-CODIGO                      TO GS-CODIGO.
       MOSTRA-ULT-CODIGO SECTION.
           MOVE 1                               TO GRAVA-W.
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
           CLOSE ACD010 ACD020.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

