       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP132.
      *DATA: 14-06-2007
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LISTAGEM DE EVENTOS (CHECK LIST)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY IEPX011.
           COPY COPX040.
           COPY MTPX019.

           SELECT RELAT ASSIGN TO ARQUIVO-IMPRESSAO
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY IEPW011.
       COPY COPW040.
       COPY MTPW019.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(134).
       WORKING-STORAGE SECTION.
           COPY "REP132.CPB".
           COPY "REP132.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS         PIC X(65)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  COMPACTA              PIC X(01)    VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(95)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(140)  VALUE
           "EMISSAO CHECK LIST - FORMANDOS     ".
       01  CAB02A.
           05  FILLER              PIC X(10)   VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9(4)    VALUE ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  IDENTIFICACAO-REL   PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(134) VALUE ALL "_".
       01  CAB04.
           05  FILLER              PIC X(134) VALUE
           "|ALB.|CURSO               |FORMANDO                      |
      -    "   |     |     |     |     |     |     |     |     |     |
      -    "OBS.         |".
       01  CAB04A.
           05  FILLER              PIC X(134) VALUE
           "|____|____________________|______________________________|__
      -    "___|_____|_____|_____|_____|_____|_____|_____|_____|_____|__
      -    "_____________|".
       01  CAB04B.
           05  FILLER              PIC X(134) VALUE
           "|___________________________________________________________
      -    "____________________________________________________________
      -    "_____________|".
       01  LINDET.
           05  LINDET-REL          PIC X(134) VALUE
           "|    |                    |                              |
      -    "   |     |     |     |     |     |     |     |     |     |
      -    "             |".

       02 status-code           PIC X(2) COMP-5.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN INPUT COD040 IED011 MTD019.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-PRINTER-FLG-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMA-POPUP-CONTRATO
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.
       CHAMA-POPUP-CONTRATO SECTION.
           CALL "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           MOVE PASSAR-PARAMETROS(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CONTRATO TO CONTRATO-MT19.
           MOVE ZEROS       TO CURSO-MT19
           MOVE SPACES      TO NOME-FORM-MT19.
           START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.
           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
               IF CONTRATO-MT19 <> GS-CONTRATO MOVE "10" TO ST-MTD019
               ELSE
                MOVE SEQ-MT19        TO GS-LINDET(1: 5)
                MOVE CURSO-MT19      TO CODIGO-IE11
                READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                END-READ
                MOVE NOME-IE11       TO GS-LINDET(6: 20)
                MOVE NOME-FORM-MT19  TO GS-LINDET(27: 40)
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
           MOVE "REP132" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           MOVE "S" TO COMPACTA

           MOVE SPACES TO ARQUIVO-IMPRESSAO

           string "\ARQUIVOS\REP132-" GS-CONTRATO
                                                  INTO ARQUIVO-IMPRESSAO
           OPEN OUTPUT RELAT.


           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           INITIALIZE REG-MTD019
           MOVE GS-CONTRATO TO CONTRATO-MT19
           MOVE ZEROS       TO CURSO-MT19.
           MOVE SPACES      TO NOME-FORM-MT19.
           START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD019
              NOT AT END
                  IF CONTRATO-MT19 <> GS-CONTRATO
                     MOVE "10" TO ST-MTD019
                  ELSE
                     MOVE SEQ-MT19        TO LINDET-REL(2: 4)
                     MOVE CURSO-MT19      TO CODIGO-IE11
                     READ IED011 INVALID KEY
                          MOVE SPACES     TO NOME-IE11
                     END-READ
                     MOVE NOME-IE11       TO LINDET-REL(7: 20)
                     MOVE NOME-FORM-MT19  TO LINDET-REL(28: 30)
                     WRITE REG-RELAT FROM LINDET
                     WRITE REG-RELAT FROM CAB04A
                     END-WRITE
                     ADD 2 TO LIN
                     IF LIN > 56
                        PERFORM CABECALHO
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.

           MOVE "SALTAR PAGINA" TO REG-RELAT
           WRITE REG-RELAT.

           CLOSE RELAT.

           IF GS-IMPRIMIR = "S"
              MOVE "S" TO COMPACTA
              CALL "PRP102" USING PARAMETROS-W
                            ARQUIVO-IMPRESSAO COMPACTA IMPRESSORA-W
              CANCEL "PRP102"
              call "CBL_DELETE_FILE" using     ARQUIVO-IMPRESSAO
                                     returning status-code
              if status-code <> "0000"
                 move "Erro na Exclusão do Arquivo" to mensagem
                 move "C" to tipo-msg
                 perform exibir-mensagem.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM CAB01
           ELSE
              MOVE "SALTAR PAGINA" TO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM CAB01.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT FROM CAB02.
           MOVE GS-CONTRATO      TO CONTRATO-REL
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-REL.
           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           WRITE REG-RELAT FROM CAB02A
           WRITE REG-RELAT FROM CAB03
           WRITE REG-RELAT FROM CAB04
           WRITE REG-RELAT FROM CAB04A
           MOVE 8 TO LIN.

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
           CLOSE IED011 COD040 MTD019.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
