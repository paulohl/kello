       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP130.
      *DATA: 07/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Extrato de Fornecedor - Caixa
      *FUNÇÃO: Relaciona todo o movimento dentro de um intervalo de
      *        movimento,referente ao fornecedor solicitado
      *        O sinal será oposto ao caixa(Ex: saída do caixa p/pgto
      *        de um fornecedor, que p/ ele será entrada.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CXPX020.
           COPY CXPX041.
           COPY CXPX100.
           COPY CXPX031.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW020.
       COPY CXPW041.
       COPY CXPW100.
       COPY CXPW031.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP130.CPB".
           COPY "CXP130.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD041             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CXD031             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
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
           05  VALOR1                PIC 9(08)V99 VALUE ZEROS.
           05  VALOR2                PIC 9(08)V99 VALUE ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  CONTADOR              PIC 9(02)    VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           COPY "PARAMETR".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(51)   VALUE
           "EXTRATO DE FORNECEDOR  ".
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV. MOVTO: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(104)  VALUE
           "SEQ. DATA-MOVTO HISTORICO                      DOCUMENTO
      -    "   VALOR-R$(D/C)   CONTA-RESULTADO".
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "FORNECEDOR: ".
           05  FORNECEDOR-REL      PIC ZZZ.ZZZ.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  NOME-FORN-REL       PIC X(42)   VALUE SPACES.
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(104)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "SALDO INICIAL: ".
           05  SALDO-INI-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(08)   VALUE SPACES.
           05  FILLER              PIC X(17)   VALUE "SALDO INTERVALO:".
           05  SALDO-INT-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "SALDO FINAL: ".
           05  SALDO-FIM-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP130-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP130-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP130-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP130-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD041.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CXD031"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD031
           OPEN INPUT CXD100 CGD001 CXD020 CXD041 CXD031.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP130-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP130-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP130-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP130-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD041 <> "00"
              MOVE "ERRO ABERTURA CXD041: "  TO CXP130-MENSAGEM-ERRO
              MOVE ST-CXD041 TO CXP130-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD031 <> "00"
              MOVE "ERRO ABERTURA CXD031: "  TO CXP130-MENSAGEM-ERRO
              MOVE ST-CXD031 TO CXP130-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP130-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP130-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP130-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP130-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP130-LE-FORNECEDOR-TRUE
                    PERFORM LE-FORNECEDOR
               WHEN CXP130-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP130-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CXP130-POPUP-FORNEC-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN CXP130-POPUP-TIPOLCTO-TRUE
                    PERFORM POPUP-TIPOLCTO
               WHEN CXP130-LER-TIPOLCTO-TRUE
                    PERFORM LER-TIPOLCTO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       POPUP-TIPOLCTO SECTION.
           CALL "CXP031T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(33: 2) TO CXP130-ACP-TIPOLCTO
           CANCEL "CXP031T"
           PERFORM LER-TIPOLCTO.
       POPUP-TIPOLCTO-FIM.
           EXIT.

       LER-TIPOLCTO SECTION.
           MOVE CXP130-ACP-TIPOLCTO  TO TIPO-LCTO-CX31
           READ CXD031 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX31
           END-READ

           MOVE DESCRICAO-CX31       TO CXP130-DESC-TIPOLCTO
           REFRESH-OBJECT PRINCIPAL.
       LER-TIPOLCTO-FIM.
           EXIT.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO CXP130-FORNECEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO CXP130-NOME-FORN.
       LE-FORNECEDOR SECTION.
           MOVE CXP130-FORNECEDOR TO CODIGO-CG01 FORNECEDOR-REL.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01    TO CXP130-NOME-FORN NOME-FORN-REL.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CXP130-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP130-MOVTO-INI TO DATA-INV MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-INI.
           MOVE DATA-INV(01: 06) TO MESANO-SALDO-ANT.
           MOVE CXP130-MOVTO-FIM TO DATA-INV MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-FIM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CXP130-LINDET.
           PERFORM INVERTE-DATA.
           PERFORM CALCULA-SALDO-INICIAL.
           MOVE CXP130-FORNECEDOR TO CONTAPART-CX100.
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           MOVE ZEROS             TO SALDO-INTERVALO VALOR1 VALOR2.
           START CXD100 KEY IS NOT < ALT-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                   IF DATA-MOV-CX100 > MOVTO-FIM
                      MOVE "10" TO ST-CXD100
                   ELSE
                      IF CONTAPART-CX100 NOT = CXP130-FORNECEDOR
                         MOVE "10" TO ST-CXD100
                      ELSE
                         IF CXP130-ACP-TIPOLCTO = 0 OR TIPO-LCTO-CX100
                            MOVE SPACES            TO CXP130-LINDET
                            MOVE SEQ-CX100         TO
                                 CXP130-LINDET(01: 04)
                            MOVE DATA-MOV-CX100    TO DATA-INV
                            CALL "GRIDAT1" USING DATA-INV
                            MOVE DATA-INV          TO DATA-E
                            MOVE DATA-E            TO
                                 CXP130-LINDET(06: 11)
                            MOVE HISTORICO-CX100   TO
                                 CXP130-LINDET(17: 31)
                            MOVE DOCUMENTO-CX100   TO
                                 CXP130-LINDET(48: 13)
                            MOVE VALOR-CX100       TO VALOR-E
                            MOVE VALOR-E           TO
                                 CXP130-LINDET(61: 15)
                            IF TIPO-LCTO-CX100 < 50
                               MOVE "C"            TO
                                   CXP130-LINDET(76: 04)
                               ADD VALOR-CX100     TO
                                   SALDO-INTERVALO
                               ADD VALOR-CX100     TO VALOR1
                            ELSE
                               MOVE "D"            TO
                                   CXP130-LINDET(76: 04)
                               SUBTRACT VALOR-CX100 FROM
                                   SALDO-INTERVALO
                               ADD VALOR-CX100     TO VALOR2
                            END-IF
                            MOVE CONTA-REDUZ-CX100 TO
                                 CODIGO-REDUZ-CX20
                                 CXP130-LINDET(80: 05)
                            READ CXD020 INVALID KEY
                                 MOVE SPACES       TO DESCRICAO-CX20
                            END-READ
                            MOVE DESCRICAO-CX20    TO
                                 CXP130-LINDET(86: 20)
                            MOVE "INSERE-LIST" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                         END-IF
                      END-IF
                   END-IF
              END-READ
           END-PERFORM
           MOVE SPACES TO CXP130-LINDET
           MOVE "TOTAL CREDITO . . . " TO CXP130-LINDET
           MOVE VALOR1                 TO VALOR-E
           MOVE VALOR-E                TO CXP130-LINDET(25:15)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO CXP130-LINDET
           MOVE "TOTAL DEBITO  . . . " TO CXP130-LINDET
           MOVE VALOR2                 TO VALOR-E
           MOVE VALOR-E                TO CXP130-LINDET(25:15)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           PERFORM CALCULA-SALDO-FINAL.
       CALCULA-SALDO-INICIAL SECTION.
           MOVE CXP130-FORNECEDOR TO CONTAPART-CX41.
           MOVE ZEROS             TO ANOMES-CX41 SALDO-INICIAL.
           START CXD041 KEY IS NOT < ALT-CX41 INVALID KEY
                 MOVE "10" TO ST-CXD041.
           PERFORM UNTIL ST-CXD041 = "10"
             READ CXD041 NEXT RECORD AT END MOVE "10" TO ST-CXD041
                NOT AT END
                  IF CONTAPART-CX41 NOT = CXP130-FORNECEDOR
                        MOVE "10" TO ST-CXD041
                  ELSE
                   IF ANOMES-CX41 NOT < MESANO-SALDO-ANT
                      MOVE "10" TO ST-CXD041
                   ELSE
                    ADD SALDOS-CX41 TO SALDO-INICIAL
                    SUBTRACT SALDOE-CX41 FROM SALDO-INICIAL
             END-READ
           END-PERFORM.
           MOVE MESANO-SALDO-ANT TO DATA-MOV-CX100(1: 6).
           MOVE 01               TO DATA-MOV-CX100(7: 2).
           MOVE CXP130-FORNECEDOR TO CONTAPART-CX100.
           START CXD100 KEY IS NOT < ALT-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
               READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                   IF DATA-MOV-CX100 NOT < MOVTO-INI
                        MOVE "10" TO ST-CXD100
                   ELSE
                     IF TIPO-LCTO-CX100 < 50
                        ADD VALOR-CX100 TO SALDO-INICIAL
                     ELSE
                        SUBTRACT VALOR-CX100 FROM SALDO-INICIAL
               END-READ
           END-PERFORM.

       CALCULA-SALDO-FINAL SECTION.
           COMPUTE SALDO-FINAL = SALDO-INICIAL + SALDO-INTERVALO.
           MOVE SALDO-INICIAL      TO CXP130-SALDO-INI SALDO-INI-REL.
           MOVE SALDO-INTERVALO    TO CXP130-SALDO-INT SALDO-INT-REL.
           MOVE SALDO-FINAL        TO CXP130-SALDO-FIM SALDO-FIM-REL.
           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP130-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP130"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE CXP130-FORNECEDOR TO FORNECEDOR-REL
           MOVE CXP130-NOME-FORN  TO NOME-FORN-REL

           MOVE ZEROS TO CONTADOR
           PERFORM UNTIL CONTADOR = CXP130-COPIAS
               ADD 1 TO CONTADOR
               PERFORM IMPRIMIR
           END-PERFORM.

       IMPRIMIR SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE CXP130-FORNECEDOR TO CONTAPART-CX100.
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           START CXD100 KEY IS NOT < ALT-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                   IF DATA-MOV-CX100 > MOVTO-FIM
                      MOVE "10" TO ST-CXD100
                   ELSE
                      IF CONTAPART-CX100 NOT = CXP130-FORNECEDOR
                         MOVE "10" TO ST-CXD100
                      ELSE
                         IF CXP130-ACP-TIPOLCTO = 0 OR TIPO-LCTO-CX100
                            MOVE SPACES            TO LINDET-REL
                            MOVE SEQ-CX100         TO LINDET-REL(01: 04)
                            MOVE DATA-MOV-CX100    TO DATA-INV
                            CALL "GRIDAT1" USING DATA-INV
                            MOVE DATA-INV          TO DATA-E
                            MOVE DATA-E            TO LINDET-REL(06: 11)
                            MOVE HISTORICO-CX100   TO LINDET-REL(17: 31)
                            MOVE DOCUMENTO-CX100   TO LINDET-REL(48: 13)
                            MOVE VALOR-CX100       TO VALOR-E
                            MOVE VALOR-E           TO LINDET-REL(61: 15)
                            IF TIPO-LCTO-CX100 < 50
                               MOVE "C"            TO LINDET-REL(76: 04)
                            ELSE
                               MOVE "D"            TO LINDET-REL(76: 04)
                            END-IF
                            MOVE CONTA-REDUZ-CX100 TO CODIGO-REDUZ-CX20
                                                      LINDET-REL(80: 05)
                            READ CXD020 INVALID KEY
                                    MOVE SPACES TO DESCRICAO-CX20
                            END-READ
                            MOVE DESCRICAO-CX20  TO LINDET-REL(86: 20)
                            WRITE REG-RELAT FROM LINDET
                            ADD 1 TO LIN
                            IF LIN > 56
                               PERFORM CABECALHO
                            END-IF
                         END-IF
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO LINDET-REL
           MOVE "TOTAL CREDITO . . . " TO LINDET-REL
           MOVE VALOR1                 TO VALOR-E
           MOVE VALOR-E                TO LINDET-REL(25:15)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           MOVE SPACES TO CXP130-LINDET
           MOVE "TOTAL DEBITO  . . . " TO LINDET-REL
           MOVE VALOR2                 TO VALOR-E
           MOVE VALOR-E                TO LINDET-REL(25:15)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           MOVE LINTOT TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           COPY DESCONDENSA.

       IMPRIMIR-FIM.
           EXIT.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP130-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CGD001 CXD020 CXD041 CXD031.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

