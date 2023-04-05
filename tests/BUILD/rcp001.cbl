       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 27/09/2000
      *DESCRIÇÃO: Cadastro de TIPO DE COMISSAO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY RCPX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
           COPY RCPW001.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP001.CPB".
           COPY "RCP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RCD001             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ,ZZZ.
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
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO DE CADASTRO DE COMISSAO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
       "COD   CHEQUE  MOEDA  ANTECIP.  DUPLICATA  DEB.AUTOM.  CARTAO CRE
      -"D. VISITA".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

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
           MOVE "RCD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001.
           OPEN I-O RCD001
           IF ST-RCD001 = "35"
              CLOSE RCD001      OPEN OUTPUT RCD001
              CLOSE RCD001      OPEN I-O RCD001
           END-IF.
           IF ST-RCD001 <> "00"
              MOVE "ERRO ABERTURA RCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD001 TO GS-MENSAGEM-ERRO(23: 02)
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
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 3) TO GS-COMISSAO
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
           MOVE GS-COMISSAO       TO CODIGO-COMIS-RC01.
           READ RCD001 INVALID KEY INITIALIZE REG-RCD001
                                   MOVE 0 TO GS-GRAVA
           NOT INVALID KEY
                MOVE COMIS-CHEQUE-RC01            TO GS-CHEQUE
                MOVE COMIS-MOEDA-RC01             TO GS-MOEDA
                MOVE COMIS-ANTECIPADA-RC01        TO GS-ANTECIPADA
                MOVE COMIS-DUPLICATA-RC01         TO GS-DUPLICATA
                MOVE COMIS-DEBITO-AUTOMATICO-RC01 TO GS-DEB-AUTOM
                MOVE COMIS-CARTAO-CREDITO-RC01    TO GS-CARTA-CRED
                IF COMIS-VISITA-RC01 IS NOT NUMERIC
                     MOVE ZEROS TO COMIS-VISITA-RC01
                END-IF
                EVALUATE COMIS-VISITA-RC01
                    WHEN 1     MOVE "1ª Visita" TO GS-VISITA
                    WHEN 2     MOVE "2ª Visita" TO GS-VISITA
                    WHEN 3     MOVE "3ª Visita" TO GS-VISITA
                    WHEN 4     MOVE "4ª Visita" TO GS-VISITA
                    WHEN 5     MOVE "5ª Visita" TO GS-VISITA
                    WHEN OTHER MOVE SPACES      TO GS-VISITA.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-RCD001
           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           DELETE RCD001.
           PERFORM LIMPAR-DADOS.
       SALVAR-DADOS SECTION.
           MOVE GS-COMISSAO       TO CODIGO-COMIS-RC01.
           MOVE GS-CHEQUE         TO COMIS-CHEQUE-RC01
           MOVE GS-MOEDA          TO COMIS-MOEDA-RC01
           MOVE GS-ANTECIPADA     TO COMIS-ANTECIPADA-RC01
           MOVE GS-DUPLICATA      TO COMIS-DUPLICATA-RC01
           MOVE GS-DEB-AUTOM      TO COMIS-DEBITO-AUTOMATICO-RC01
           MOVE GS-CARTA-CRED     TO COMIS-CARTAO-CREDITO-RC01
           MOVE FUNCTION NUMVAL(GS-VISITA(1:1)) TO COMIS-VISITA-RC01
           IF GS-GRAVA = 0
              WRITE REG-RCD001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-RCD001 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           END-IF.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RCD001       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO CODIGO-COMIS-RC01
           START RCD001 KEY IS NOT < CODIGO-COMIS-RC01
               INVALID KEY MOVE "10" TO ST-RCD001.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-CONT.
           PERFORM UNTIL ST-RCD001 = "10"
              READ RCD001 NEXT RECORD AT END MOVE "10" TO ST-RCD001
              NOT AT END
                ADD 1 TO GS-CONT
      *         MOVE SPACES TO GS-LINDET
                MOVE CODIGO-COMIS-RC01            TO GS-LINDET(01: 06)
                MOVE COMIS-CHEQUE-RC01            TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(07: 06)
                MOVE COMIS-MOEDA-RC01             TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(14: 06)
                MOVE COMIS-ANTECIPADA-RC01        TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(24: 06)
                MOVE COMIS-DUPLICATA-RC01         TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(35: 06)
                MOVE COMIS-DEBITO-AUTOMATICO-RC01 TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(46: 06)
                MOVE COMIS-CARTAO-CREDITO-RC01    TO VALOR-E
                MOVE VALOR-E                      TO GS-LINDET(60: 06)
                EVALUATE COMIS-VISITA-RC01
                     WHEN 1     MOVE "1ª Visita"  TO GS-LINDET(68: 10)
                     WHEN 2     MOVE "2ª Visita"  TO GS-LINDET(68: 10)
                     WHEN 3     MOVE "3ª Visita"  TO GS-LINDET(68: 10)
                     WHEN 4     MOVE "4ª Visita"  TO GS-LINDET(68: 10)
                     WHEN 5     MOVE "5ª Visita"  TO GS-LINDET(68: 10)
                     WHEN OTHER MOVE SPACES       TO GS-LINDET(68: 10)
                END-EVALUATE

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
           MOVE "RCP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO CODIGO-COMIS-RC01
           START RCD001 KEY IS NOT < CODIGO-COMIS-RC01 INVALID KEY
                        MOVE "10" TO ST-RCD001
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-RCD001 = "10"
             READ RCD001 NEXT RECORD AT END
                MOVE "10" TO ST-RCD001
             NOT AT END
                MOVE SPACES                       TO LINDET-REL
                MOVE CODIGO-COMIS-RC01            TO LINDET-REL(01: 06)
                MOVE COMIS-CHEQUE-RC01            TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(07: 06)
                MOVE COMIS-MOEDA-RC01             TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(14: 06)
                MOVE COMIS-ANTECIPADA-RC01        TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(24: 06)
                MOVE COMIS-DUPLICATA-RC01         TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(35: 06)
                MOVE COMIS-DEBITO-AUTOMATICO-RC01 TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(46: 06)
                MOVE COMIS-CARTAO-CREDITO-RC01    TO VALOR-E
                MOVE VALOR-E                      TO LINDET-REL(60: 06)
                EVALUATE COMIS-VISITA-RC01
                     WHEN 1     MOVE "1ª Visita"  TO LINDET-REL(68: 10)
                     WHEN 2     MOVE "2ª Visita"  TO LINDET-REL(68: 10)
                     WHEN 3     MOVE "3ª Visita"  TO LINDET-REL(68: 10)
                     WHEN 4     MOVE "4ª Visita"  TO LINDET-REL(68: 10)
                     WHEN 5     MOVE "5ª Visita"  TO LINDET-REL(68: 10)
                     WHEN OTHER MOVE SPACES       TO LINDET-REL(68: 10)
                END-EVALUATE

                WRITE REG-RELAT FROM LINDET
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
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
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
