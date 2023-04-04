       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP140.
      *DATA: 10/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Extrato de Conta de Resultado - Caixa
      *FUNÇÃO: Relaciona todo o movimento dentro de um intervalo de
      *        movimento,referente a conta resultado solicitada
      *        O sinal será oposto ao caixa(Ex: saída do caixa p/pgto
      *        de uma conta, que p/ ele será entrada.
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
           COPY CXPX042.
           COPY CXPX100.
           COPY CXPX004.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW020.
       COPY CXPW042.
       COPY CXPW100.
       COPY CXPW004.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-SEQ                 PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DATA-MOVTO          PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-HISTORICO           PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DOCUMENTO           PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-VALOR               PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-DB                  PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-CONTA-FORNECEDOR    PIC X(30).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-NOME-FORNECEDOR     PIC X(30).
          05 FILLER                    PIC X(02) VALUE X"0DA0".


       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP140.CPB".
           COPY "CXP140.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD004             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD042             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65)    VALUE SPACES.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99-.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           COPY "PARAMETR".

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
           "EXTRATO DE CONTA DE RESULTADO  ".
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
      -    "   VALOR-R$(D/C)   CONTA-FORNECEDOR".
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(17)   VALUE "CONTA RESULTADO:".
           05  CONTA-REDUZ-REL     PIC ZZZZZ.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  NOME-CONTA-REDUZ-REL PIC X(42)   VALUE SPACES.
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
           PERFORM CORPO-PROGRAMA UNTIL CXP140-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP140-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP140-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP140-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD042"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD042.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           OPEN INPUT CXD100 CGD001 CXD020 CXD042 CXD004.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP140-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD004 <> "00"
              MOVE "ERRO ABERTURA CXD004: "  TO CXP140-MENSAGEM-ERRO
              MOVE ST-CXD004 TO CXP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP140-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD042 <> "00"
              MOVE "ERRO ABERTURA CXD042: "  TO CXP140-MENSAGEM-ERRO
              MOVE ST-CXD042 TO CXP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP140-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP140-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP140-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP140-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP140-LE-CONTA-REDUZ-TRUE
                    PERFORM LE-CONTA-REDUZ
               WHEN CXP140-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP140-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CXP140-POPUP-CONTAREDUZ-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN CXP140-EXCEL-TRUE
                   PERFORM GERAR-EXCEL
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL
           STRING "\ARQUIVOS\CXP-" CXP140-MOVTO-INI "-" CXP140-MOVTO-FIM
                  ".csv" INTO ARQUIVO-EXCEL

           OPEN OUTPUT EXCEL

           MOVE "INTERV. MOVTO: "    TO EXCEL-SEQ
           MOVE MOVTO-INI-REL        TO EXCEL-DATA-MOVTO
           MOVE " a "                TO EXCEL-HISTORICO
           MOVE MOVTO-FIM-REL        TO EXCEL-DOCUMENTO
           MOVE "CONTA RESULTADO:"   TO EXCEL-VALOR
           MOVE CONTA-REDUZ-REL      TO EXCEL-DB
           MOVE NOME-CONTA-REDUZ-REL TO EXCEL-CONTA-FORNECEDOR
           MOVE SPACES               TO EXCEL-NOME-FORNECEDOR
           WRITE REG-EXCEL





           MOVE "SEQ."             TO EXCEL-SEQ
           MOVE "DATA-MOVTO"       TO EXCEL-DATA-MOVTO
           MOVE "HISTORICO"        TO EXCEL-HISTORICO
           MOVE "DOCUMENTO"        TO EXCEL-DOCUMENTO
           MOVE "VALOR-R$"         TO EXCEL-VALOR
           MOVE "D/C"              TO EXCEL-DB
           MOVE "CTA-FORN."        TO EXCEL-CONTA-FORNECEDOR
           MOVE "NOME FORN."       TO EXCEL-NOME-FORNECEDOR
           WRITE REG-EXCEL


           MOVE CXP140-CONTA-REDUZ TO CONTA-REDUZ-CX100.
           MOVE MOVTO-INI          TO DATA-MOV-CX100.
           START CXD100 KEY IS NOT < ALT-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 > MOVTO-FIM MOVE "10" TO ST-CXD100
                ELSE
                 IF CONTA-REDUZ-CX100 NOT = CXP140-CONTA-REDUZ
                           MOVE "10" TO ST-CXD100
                  ELSE
                    MOVE SPACES            TO LINDET-REL
                    MOVE SEQ-CX100         TO EXCEL-SEQ
                    MOVE DATA-MOV-CX100    TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV          TO DATA-E
                    MOVE DATA-E            TO EXCEL-DATA-MOVTO
                    MOVE HISTORICO-CX100   TO EXCEL-HISTORICO
                    MOVE DOCUMENTO-CX100   TO EXCEL-DOCUMENTO
                    MOVE VALOR-CX100       TO VALOR-E
                    MOVE VALOR-E           TO EXCEL-VALOR
                    IF TIPO-LCTO-CX100 < 50
                       MOVE "C"            TO EXCEL-DB
                    ELSE
                       MOVE "D"            TO EXCEL-DB
                    END-IF
                    MOVE CONTAPART-CX100   TO CODIGO-CG01
                                              EXCEL-CONTA-FORNECEDOR
                    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01         TO EXCEL-NOME-FORNECEDOR
                    WRITE REG-EXCEL
              END-READ
           END-PERFORM.

           MOVE "SALDO INICIAL"    TO EXCEL-SEQ
           MOVE SALDO-INI-REL      TO EXCEL-DATA-MOVTO
           MOVE "SALDO INTERVALO"  TO EXCEL-HISTORICO
           MOVE SALDO-INT-REL      TO EXCEL-DOCUMENTO
           MOVE "SALDO FINAL"      TO EXCEL-VALOR
           MOVE SALDO-FIM-REL      TO EXCEL-DB
           MOVE SPACES             TO EXCEL-CONTA-FORNECEDOR
           MOVE SPACES             TO EXCEL-NOME-FORNECEDOR

           WRITE REG-EXCEL

           CLOSE EXCEL.

       CHAMAR-POPUP SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CXP020T".
           MOVE PASSAR-STRING-1(52: 5) TO CXP140-CONTA-REDUZ.
           PERFORM LE-CONTA-REDUZ.
       CALCULA-SALDO-INICIAL SECTION.
           MOVE ZEROS TO SALDO-INICIAL SALDO-FINAL.
           MOVE CXP140-CONTA-REDUZ TO CONTAREDUZ-CX42.
           MOVE ZEROS              TO ANOMES-CX42 SALDO-INICIAL.
           START CXD042 KEY IS NOT < ALT-CX42 INVALID KEY
                 MOVE "10" TO ST-CXD042.
           PERFORM UNTIL ST-CXD042 = "10"
             READ CXD042 NEXT RECORD AT END MOVE "10" TO ST-CXD042
                NOT AT END
                  IF CONTAREDUZ-CX42 NOT = CXP140-CONTA-REDUZ
                        MOVE "10" TO ST-CXD042
                  ELSE
                   IF ANOMES-CX42 NOT < MESANO-SALDO-ANT
                      MOVE "10" TO ST-CXD042
                   ELSE
                    ADD SALDOS-CX42 TO SALDO-INICIAL
                    SUBTRACT SALDOE-CX42 FROM SALDO-INICIAL
                    MOVE SPACES TO CXP140-ANDAMENTO
                    MOVE SALDO-INICIAL TO MASC-VALOR
                    STRING "SALDO INICIAL " MASC-VALOR INTO
                    CXP140-ANDAMENTO
                    MOVE "ATUALIZA-ANDAMENTO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
           MOVE MESANO-SALDO-ANT   TO DATA-MOV-CX100(1: 6).
           MOVE 01                 TO DATA-MOV-CX100(7: 2).
           MOVE CXP140-CONTA-REDUZ TO CONTA-REDUZ-CX100.
           START CXD100 KEY IS NOT < ALT-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
               READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                   IF DATA-MOV-CX100 NOT < MOVTO-INI
                        MOVE "10" TO ST-CXD100
                   ELSE
                     IF TIPO-LCTO-CX100 < 50
                        ADD VALOR-CX100 TO SALDO-INICIAL
                     ELSE SUBTRACT VALOR-CX100 FROM SALDO-INICIAL
               END-READ
           END-PERFORM.
       LE-CONTA-REDUZ SECTION.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CX004
           MOVE CXP140-CONTA-REDUZ TO PROGRAMA-CX004
           READ CXD004 INVALID KEY
                MOVE "Usuário Sem Permissão falar com o CPD para Acesso"
                TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENASGEM
           NOT INVALID KEY
                MOVE CXP140-CONTA-REDUZ TO CODIGO-REDUZ-CX20
                                           CONTA-REDUZ-REL
                READ CXD020 INVALID KEY
                     MOVE SPACES TO NOME-CG01
                END-READ
                MOVE DESCRICAO-CX20 TO CXP140-NOME-CONTA-REDUZ
                                       NOME-CONTA-REDUZ-REL.
       EXIBIR-MENASGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO CXP140-FLAG-CRITICA
           MOVE SPACES TO MENSAGEM.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CXP140-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP140-MOVTO-INI TO DATA-INV MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-INI.
           MOVE DATA-INV(01: 06) TO MESANO-SALDO-ANT.
           MOVE CXP140-MOVTO-FIM TO DATA-INV MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-FIM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CXP140-LINDET.
           PERFORM INVERTE-DATA.
           PERFORM CALCULA-SALDO-INICIAL.
           MOVE CXP140-CONTA-REDUZ TO CONTA-REDUZ-CX100.
           MOVE MOVTO-INI          TO DATA-MOV-CX100.
           MOVE ZEROS              TO SALDO-INTERVALO.
           START CXD100 KEY IS NOT < ALT-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 > MOVTO-FIM
                   MOVE "10" TO ST-CXD100
                ELSE
                 IF CONTA-REDUZ-CX100 NOT = CXP140-CONTA-REDUZ
                           MOVE "10" TO ST-CXD100
                  ELSE
                    MOVE SPACES            TO CXP140-LINDET
                    MOVE SEQ-CX100         TO CXP140-LINDET(01: 04)
                    MOVE DATA-MOV-CX100    TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV          TO DATA-E
                    MOVE DATA-E            TO CXP140-LINDET(06: 11)
                    MOVE HISTORICO-CX100   TO CXP140-LINDET(17: 31)
                    MOVE DOCUMENTO-CX100   TO CXP140-LINDET(48: 13)
                    MOVE VALOR-CX100       TO VALOR-E
                    MOVE VALOR-E           TO CXP140-LINDET(61: 15)
                    IF TIPO-LCTO-CX100 < 50
                       MOVE "C"            TO CXP140-LINDET(76: 04)
                       ADD VALOR-CX100     TO SALDO-INTERVALO
                    ELSE MOVE "D"          TO CXP140-LINDET(76: 02)
                         SUBTRACT VALOR-CX100 FROM SALDO-INTERVALO
                    END-IF

                    MOVE SPACES TO CXP140-ANDAMENTO
                    MOVE SALDO-INTERVALO TO MASC-VALOR
                    STRING "SALDO INTERVALO " MASC-VALOR INTO
                    CXP140-ANDAMENTO
                    MOVE "ATUALIZA-ANDAMENTO" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM

                    MOVE CONTAPART-CX100   TO CODIGO-CG01
                                              CXP140-LINDET(78: 06)
                    READ CGD001 INVALID KEY
                            MOVE SPACES TO NOME-CG01
                    END-READ
                    MOVE NOME-CG01         TO CXP140-LINDET(85: 20)
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM CALCULA-SALDO-FINAL.
       CALCULA-SALDO-FINAL SECTION.
           COMPUTE SALDO-FINAL = SALDO-INICIAL + SALDO-INTERVALO.

           MOVE SPACES TO CXP140-ANDAMENTO
           MOVE SALDO-FINAL TO MASC-VALOR
           STRING "SALDO FINAL " MASC-VALOR INTO
           CXP140-ANDAMENTO
           MOVE "ATUALIZA-ANDAMENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SALDO-INICIAL      TO CXP140-SALDO-INI SALDO-INI-REL.
           MOVE SALDO-INTERVALO    TO CXP140-SALDO-INT SALDO-INT-REL.
           MOVE SALDO-FINAL        TO CXP140-SALDO-FIM SALDO-FIM-REL.
           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP140-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP140"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE CXP140-CONTA-REDUZ TO CONTA-REDUZ-CX100.
           MOVE MOVTO-INI          TO DATA-MOV-CX100.
           START CXD100 KEY IS NOT < ALT-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                   MOVE "10" TO ST-CXD100
              NOT AT END
                IF DATA-MOV-CX100 > MOVTO-FIM
                   MOVE "10" TO ST-CXD100
                ELSE
                   IF CONTA-REDUZ-CX100 NOT = CXP140-CONTA-REDUZ
                      MOVE "10" TO ST-CXD100
                   ELSE
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
                      MOVE CONTAPART-CX100   TO CODIGO-CG01
                                                LINDET-REL(80: 06)
                      READ CGD001 INVALID KEY
                           MOVE SPACES TO NOME-CG01
                      END-READ
                      MOVE NOME-CG01       TO LINDET-REL(87: 17)
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
              END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINTOT AFTER 2.

           COPY DESCONDENSA.

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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP140-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CGD001 CXD020 CXD042 CXD004
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

