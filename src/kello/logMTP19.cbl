       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO69.
      *DATA: 05-10-2006.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LE LOG DO MTD019
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY LOGX002.

           COPY MTPX019.

       DATA DIVISION.
       FILE SECTION.

           COPY LOGW002.

           COPY MTPW019.

       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

           COPY "LOGMTP19.CPB".
           COPY "LOGMTP19.CPy".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  DATA-LOG              PIC X(10)    VALUE SPACES.
           05  HORARIO               PIC X(08)    VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01  CAB01.
           05  EMPRESA-REL         PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(10)   VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RELACAO DE CREDITOS DE REPORTAGEM".
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-REL          PIC 99/9999.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "FUNCIONARIO                     TOTAL-CREDITOS".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP120-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP120-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP120-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP120-VERSION-NO  TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL

           MOVE "LOG002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG002
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019

           OPEN INPUT LOG002 MTD019
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-LOG002 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-MTD019 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP120-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP120-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP120-CARREGA-LISTA-TRUE
                    PERFORM CARREGA-LISTA
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE CXP120-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-LOG002
           MOVE "MTD019" TO LOG2-ARQUIVO
           START LOG002 KEY IS NOT LESS LOG2-CH-ARQUIVO INVALID KEY
                 MOVE "10" TO ST-LOG002.

           PERFORM UNTIL ST-LOG002 = "10"
                 READ LOG002 NEXT RECORD AT END
                      MOVE "10" TO ST-LOG002
                 NOT AT END
                      MOVE REG-LOG002         TO CXP120-LINHA-ATU
                      MOVE "REFRESH-DATA"     TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF LOG2-OPERACAO = CXP120-ACP-OPERACAO
                         IF LOG2-PROGRAMA = "MTP019"
                            MOVE LOG2-REGISTRO TO REG-MTD019

                            IF CONTRATO-MT19 = CXP120-ACP-CONTRATO
                               IF CXP120-ACP-ALBUM = 0 OR SEQ-MT19

                                  STRING LOG2-DIA "/"
                                         LOG2-MES "/"
                                         LOG2-ANO INTO DATA-LOG

                                  STRING LOG2-HORA ":"
                                         LOG2-MINU ":"
                                         LOG2-SEGU INTO HORARIO

                                  MOVE SPACES TO CXP120-LINDET
                                  STRING "Album....: " ALBUMMT19
                                    INTO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM


                                  MOVE SPACES TO CXP120-LINDET
                                  STRING "Usuário..: " LOG2-USUARIO
                                    INTO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  MOVE SPACES TO CXP120-LINDET
                                  STRING "Data.....: " DATA-LOG
                                    INTO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  MOVE SPACES TO CXP120-LINDET
                                  STRING "Hora.....: " HORARIO
                                    INTO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  MOVE SPACES TO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  MOVE ALL "=" TO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  MOVE SPACES TO CXP120-LINDET
                                  MOVE "INSERE-LIST" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                               END-IF
                            END-IF

                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CXP120-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LOGMTP19"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP120-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD019 LOG002
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

