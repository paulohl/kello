       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP110.
      *DATA: 01-02-2005
      *AUTORA: ALFREDO SAVIOLLI NETO
      *RELATÓRIO: Procura Valores no Caixa
      *FUNÇÃO: Relaciona todo os lançamentos ocorridos
      *        na data de movto solicitada

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CXPX040.
           COPY CXPX100.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW040.
       COPY CXPW100.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(120).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP110.CPB".
           COPY "CXP110.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD040             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
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
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(1)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(1)    VALUE SPACES.
           05  FILLER              PIC X(51)   VALUE
           "PROCURA DE VALOR  ".
           05  FILLER              PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "   DATA MOVTO: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC x(05) VALUE " ATÉ ".
           05  MOVTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(1)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(1)    VALUE SPACES.
           05  FILLER              PIC X(12) VALUE "DATA MOVTO".
           05  FILLER              PIC X(104)  VALUE
           "SEQ. HISTORICO                      DOCUMENTO    VALOR-SAIDA
      -    "    VALOR-ENT REDUZ CONTA-FORNEC".
       01  LINDET.
           05  FILLER              PIC X(1)    VALUE SPACES.
           05  LINDET-REL          PIC X(116)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP110-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP110-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP110-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP110-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD040.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           OPEN INPUT CXD100 CGD001 CXD040.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP110-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP110-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO CXP110-MENSAGEM-ERRO
              MOVE ST-CXD040 TO CXP110-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP110-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP110-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP110-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP110-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP110-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP110-CARREGA-LISTA-TRUE
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

       CALCULA-SALDO-INICIAL SECTION.
           PERFORM INVERTE-DATA.
           MOVE "UNSHOW-WIN1" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CXP110-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP110-DATA-MOVTOI TO DATA-INV MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-INI.

           MOVE CXP110-DATA-MOVTOF TO DATA-INV MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-FIM.
       CARREGA-LISTA SECTION.
           PERFORM CALCULA-SALDO-INICIAL.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO CXP110-LINDET.
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           MOVE ZEROS             TO SEQ-CX100.
           MOVE ZEROS             TO SALDO-INTERVALO.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 > MOVTO-FIM
                   MOVE "10" TO ST-CXD100
                ELSE
                   IF VALOR-CX100 NOT < CXP110-VALOR AND
                      VALOR-CX100 NOT > CXP110-VALOR2
                      MOVE SPACES            TO CXP110-LINDET
                      STRING DATA-MOV-CX100(7:8) INTO CXP110-LINDET(1:2)
                      STRING "/"             INTO CXP110-LINDET(3:1)
                      STRING DATA-MOV-CX100(5:6) INTO CXP110-LINDET(4:2)
                      STRING "/"             INTO CXP110-LINDET(6:1)
                      STRING DATA-MOV-CX100(1:4) INTO CXP110-LINDET(7:4)
                      MOVE SEQ-CX100         TO CXP110-LINDET(13: 04)
                      MOVE HISTORICO-CX100   TO CXP110-LINDET(18: 31)
                      MOVE DOCUMENTO-CX100   TO CXP110-LINDET(50: 11)
                      MOVE VALOR-CX100       TO VALOR-E
                      IF TIPO-LCTO-CX100 NOT < 50
                         MOVE VALOR-E        TO CXP110-LINDET(74: 13)
                      ELSE
                         MOVE VALOR-E      TO CXP110-LINDET(60: 13)
                      END-IF
                      MOVE CONTA-REDUZ-CX100 TO CXP110-LINDET(88: 05)
                      MOVE CONTAPART-CX100   TO CXP110-LINDET(94: 06)
                                                CODIGO-CG01
                      READ CGD001 INVALID KEY
                           MOVE SPACES TO NOME-CG01
                      END-READ
                      MOVE NOME-CG01         TO CXP110-LINDET(101: 18)
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
                END-IF
              END-READ
           END-PERFORM.
           PERFORM CALCULA-SALDO-FINAL.
       CALCULA-SALDO-FINAL SECTION.
           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP110-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP110"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           MOVE ZEROS             TO SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 > MOVTO-FIM
                   MOVE "10" TO ST-CXD100
                ELSE
                   IF VALOR-CX100 NOT < CXP110-VALOR AND
                      VALOR-CX100 NOT > CXP110-VALOR2
                      MOVE SPACES            TO LINDET-REL
                      STRING DATA-MOV-CX100(7:8) INTO LINDET-REL(1:2)
                      STRING "/"             INTO LINDET-REL(3:1)
                      STRING DATA-MOV-CX100(5:6) INTO LINDET-REL(4:2)
                      STRING "/"             INTO LINDET-REL(6:1)
                      STRING DATA-MOV-CX100(1:4) INTO LINDET-REL(7:4)

                      MOVE SEQ-CX100         TO LINDET-REL(13: 04)
                      MOVE HISTORICO-CX100   TO LINDET-REL(18: 31)
                      MOVE DOCUMENTO-CX100   TO LINDET-REL(49: 11)
                      MOVE VALOR-CX100       TO VALOR-E
                      IF TIPO-LCTO-CX100 NOT < 50
                         MOVE VALOR-E        TO LINDET-REL(73: 13)
                         ADD VALOR-CX100     TO SALDO-INTERVALO
                      ELSE
                         MOVE VALOR-E      TO LINDET-REL(60: 13)
                         SUBTRACT VALOR-CX100 FROM SALDO-INTERVALO
                      END-IF
                      MOVE CONTA-REDUZ-CX100 TO LINDET-REL(87: 05)
                      MOVE CONTAPART-CX100   TO LINDET-REL(93: 06)
                                                CODIGO-CG01
                      READ CGD001 INVALID KEY
                           MOVE SPACES TO NOME-CG01
                      END-READ
                      MOVE NOME-CG01         TO LINDET-REL(100: 19)
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP110-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CGD001 CXD040.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

