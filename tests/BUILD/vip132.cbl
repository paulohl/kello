       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP132.
      *DATA: 11/08/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: ROTEIRO DE INSERÇÃO DE CÓPIAS DE VÍDEO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY IEPX011.
           COPY COPX040.
           COPY MTPX019.
           COPY CGPX011.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2 = NOME-CURSO-WK2.


       DATA DIVISION.
       FILE SECTION.
       COPY IEPW011.
       COPY COPW040.
       COPY MTPW019.
       COPY CGPW011.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(145).

       FD  WORK2.
       01  REG-WORK2.
           05 COD-CURSO-WK2             PIC 999.
           05 NOME-CURSO-WK2            PIC X(12).

       WORKING-STORAGE SECTION.
           COPY "VIP132.CPB".
           COPY "VIP132.CPY".
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
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
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
           "ROTEIRO DE INSERCAO                ".
       01  CAB02A.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9(4)    VALUE ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  IDENTIFICACAO-REL   PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(140) VALUE ALL "_".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(140) VALUE
           "|ALB.|CURSO               |FORMANDO                      |IN
      -    "S.1|INS.2|INS.3|INS.4|INS.5|INS.6|INS.7|INS.8|INS.9|INS.0|
      -    "   OBSERVACAO      |".
       01  CAB04A.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(140) VALUE
           "|____|____________________|______________________________|__
      -    "___|_____|_____|_____|_____|_____|_____|_____|_____|_____|__
      -    "___________________|".
       01  CAB04B.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(140) VALUE
           "|___________________________________________________________
      -    "____________________________________________________________
      -    "___________________|".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(140) VALUE
           "|    |                    |                              |
      -    "   |     |     |     |     |     |     |     |     |     |
      -    "                   |".

           copy impressora.
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
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           OPEN INPUT COD040 IED011 MTD019 CGD011.

           ACCEPT VARIA-W FROM TIME.
           COMPUTE VARIA-W2 = VARIA-W + 10

           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
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
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMA-POPUP-CONTRATO
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-CURSOS-TRUE
                    PERFORM CARREGAR-CURSOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-CURSOS SECTION.
           OPEN OUTPUT WORK2
           CLOSE       WORK2
           OPEN I-O    WORK2

           INITIALIZE REG-MTD019
           MOVE GS-CONTRATO   TO CONTRATO-MT19.
           MOVE ZEROS         TO SEQ-MT19.
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                  MOVE "10" TO ST-MTD019.

           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD019
             NOT AT END
                 IF CONTRATO-MT19 <> GS-CONTRATO
                    MOVE "10" TO ST-MTD019
                 ELSE
                    MOVE CURSO-MT19        TO CODIGO-IE11
                    READ IED011 INVALID KEY
                         MOVE SPACES TO NOME-REDUZ-IE11
                    END-READ
                    MOVE NOME-REDUZ-IE11   TO NOME-CURSO-WK2
                    MOVE CURSO-MT19        TO COD-CURSO-WK2
                    WRITE REG-WORK2
                    END-WRITE
                 END-IF
             END-READ

           END-PERFORM

           MOVE "TODOS"           TO NOME-CURSO-WK2
           MOVE 0                 TO COD-CURSO-WK2
           WRITE REG-WORK2



           MOVE 20 TO GS-CONT

           MOVE "LIMPAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK2 GS-CONT
           MOVE SPACES TO NOME-CURSO-WK2

           START WORK2 KEY IS NOT LESS CHAVE-WK2 INVALID KEY
               MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
               READ WORK2 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK2
               NOT AT END
                   ADD  1              TO GS-CONT
                   MOVE SPACES         TO GS-CURSO
                   MOVE COD-CURSO-WK2  TO GS-CURSO(16:3)
                   MOVE NOME-CURSO-WK2 TO GS-CURSO(1:15)
                   MOVE "INSERIR-SB"  TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           CLOSE WORK2.

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
                 READ MTD019 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD019
                 NOT AT END
                      IF CONTRATO-MT19 <> GS-CONTRATO
                         MOVE "10" TO ST-MTD019
                      ELSE
                         STRING "0" ALBUMMT19 INTO COD-COMPL-CG11
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         IF GS-SEXO(1:1) = SPACES OR SEXO2-CG11
                            IF GS-CURSO(16:3) = "000" OR SPACES OR
                               CURSO-MT19
                               IF GS-IDENTIFICADO = 3 OR
                                  IDENTIFICADO-MT19

                                  MOVE SEQ-MT19    TO GS-LINDET(1: 5)
                                  MOVE CURSO-MT19  TO CODIGO-IE11
                                  READ IED011 INVALID KEY
                                       MOVE SPACES     TO NOME-IE11
                                  END-READ
                                  MOVE NOME-IE11      TO
                                       GS-LINDET(6: 20)
                                  MOVE NOME-FORM-MT19 TO
                                       GS-LINDET(27: 40)
                                  MOVE "INSERE-LIST"  TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP132" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
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
                         STRING "0" ALBUMMT19 INTO COD-COMPL-CG11
                         READ CGD011 INVALID KEY
                              INITIALIZE REG-CGD011
                         END-READ
                         IF GS-SEXO(1:1) = SPACES OR SEXO2-CG11
                            IF GS-CURSO(16:3) = "000" OR SPACES OR
                               CURSO-MT19
                               IF GS-IDENTIFICADO = 3 OR
                                  IDENTIFICADO-MT19
                                  MOVE SEQ-MT19   TO LINDET-REL(2: 4)
                                  MOVE CURSO-MT19 TO CODIGO-IE11
                                  READ IED011 INVALID KEY
                                       MOVE SPACES TO NOME-IE11
                                  END-READ
                                  MOVE NOME-IE11  TO LINDET-REL(7: 20)
                                  MOVE NOME-FORM-MT19
                                    TO LINDET-REL(28: 30)
                                  WRITE REG-RELAT FROM LINDET
                                  WRITE REG-RELAT FROM CAB04A
                                  END-WRITE
                                  ADD 2 TO LIN
                                  IF LIN > 56 PERFORM CABECALHO
                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           copy descondensa.

           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           MOVE GS-CONTRATO      TO CONTRATO-REL
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-REL.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB04A.
           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED011 COD040 MTD019 CGD011.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
