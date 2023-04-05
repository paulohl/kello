       COPY DSLANG.CPY.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP020X.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 20/01/2000.
      *FUNÇÃO: Movimento de BRINDES DE ORGANIZAÇÃO DE EVENTOS - subprog.

      *REVER DIAS-PRAZO DATA-VENCTO-OE20 ATE DATA-PREV-VENDA-CO40
      *E QUANDO TIVER DATA-PAGTO-OE20 RECALCULAR ???
      *verificar qtde por formando e valor previsto

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX002.
           COPY OEPX010.
           COPY OEPX020.
           COPY CGPX001.
           COPY IEPX011.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY COPW002.
       COPY OEPW010.
       COPY OEPW020.
       COPY CGPW001.
       COPY IEPW011.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP020X.CPB".
           COPY "OEP020X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE BRINDES DE ORG.EVENTO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONT IT CUR TU BRINDE          QT FORM CUSTO-UNITÁR. DATA-VE
      -    "CTO VALOR-PREVIST DATA-SOLIC S R DATA-PAGTO    VALOR-PAGO  D
      -    "IAS FORNEC".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.
       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1          PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W   TO EMPRESA-REL
           MOVE EMPRESA-W        TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "OED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "OED020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED020.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           OPEN I-O OED020.
           OPEN INPUT IED011 COD002 CGD001 OED010 CAD004.
           IF ST-OED020 = "35"
              CLOSE OED020      OPEN OUTPUT OED020
              CLOSE OED020      OPEN I-O OED020
           END-IF.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERR  ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF COD-USUARIO-W NOT NUMERIC
      *       MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1 PERFORM REGRAVA-DADOS
                   ELSE PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-OE20
                   MOVE GS-LINDET(6: 2)  TO ITEM-OE20
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-LE-BRINDE-TRUE
                   PERFORM LE-BRINDE
               WHEN GS-LE-FORNEC-TRUE
                   PERFORM LE-FORNECEDOR
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CALCULA-VLR-PREV-TRUE
                    PERFORM CALCULA-VLR-PREVISTO
               WHEN GS-CALCULA-DIAS-PRAZ-TRUE
                    PERFORM CALCULA-PRAZO-MEDIO
               WHEN GS-CHAMADA-SUBPROG-TRUE
                    PERFORM CHAMADA-SUBPROGRAMA
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMADA-SUBPROGRAMA SECTION.
           MOVE STRING-1(1: 4) TO GS-CONTRATO.
           MOVE STRING-1(5: 2) TO GS-NR-ITEM
           MOVE STRING-1(9: 2) TO IMPRESSORA-W.
           MOVE STRING-1(11: 3) TO COD-USUARIO-W.
           IF GS-CONTRATO <> ZEROS  PERFORM CARREGA-ULTIMOS.
           MOVE STRING-1(1: 4)      TO NR-CONTRATO-OE20
           MOVE STRING-1(5: 2)      TO ITEM-OE20
           IF ITEM-OE20 <> ZEROS    PERFORM CARREGAR-DADOS
                                    MOVE 1 TO GS-TIPO-GRAVACAO.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA04"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTOES" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA14"          TO PROGRAMA-CA004.
           READ CAD004 NOT INVALID KEY
               MOVE "HABILITA-BOTAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.


       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CURSO
             WHEN 2 PERFORM CARREGA-POP-UP-BRINDE
             WHEN 3 PERFORM CARREGA-POP-UP-FORNEC
           END-EVALUATE.
       CARREGA-POP-UP-FORNEC SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                MOVE NOME-CG01(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD001
                ELSE
                  MOVE NOME-CG01       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CG01     TO GS-LINDET1(33: 06)
                  MOVE "INSERE-POP-UP-FORNEC" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-BRINDE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CO02.
           START COD002 KEY IS NOT < NOME-CO02 INVALID KEY
                 MOVE "10" TO ST-COD002.
           PERFORM UNTIL ST-COD002 = "10"
              READ COD002 NEXT RECORD AT END MOVE "10" TO ST-COD002
               NOT AT END
                MOVE NOME-CO02(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-COD002
                ELSE
                  MOVE NOME-CO02       TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CO02     TO GS-LINDET1(33: 03)
                  MOVE "INSERE-POP-UP-BRINDE" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-CURSO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-IE11
           START IED011 KEY IS NOT < NOME-IE11 INVALID KEY
                 MOVE "10" TO ST-IED011.
           PERFORM UNTIL ST-IED011 = "10"
              READ IED011 NEXT RECORD AT END MOVE "10" TO ST-IED011
               NOT AT END
                MOVE NOME-IE11(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-IED011
                ELSE
                  MOVE NOME-IE11       TO GS-LINDET1(1: 42)
                  MOVE CODIGO-IE11     TO GS-LINDET1(43: 03)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1)
                          TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET1(43: 3) TO GS-CURSO
               MOVE GS-LINDET1(1: 40) TO GS-NOME-CURSO
             WHEN 2
               MOVE GS-LINDET1(33: 3) TO GS-COD-BRINDE
               MOVE GS-LINDET1(1: 30) TO GS-NOME-BRINDE
             WHEN 3
               MOVE GS-LINDET1(33: 6)  TO GS-FORNEC
               MOVE GS-LINDET1(1: 30)  TO GS-NOME-FORN
           END-EVALUATE.
      *----------------------------------------------------------------
       CALCULA-VLR-PREVISTO SECTION.
           MOVE GS-COD-BRINDE TO CODIGO-CO02
           READ COD002 INVALID KEY INITIALIZE REG-COD002.

           IF GS-CUSTO-UNITARIO = ZEROS
              MOVE VALOR-CO02 TO CUSTO-PREVISTO-W
           ELSE MOVE GS-CUSTO-UNITARIO TO CUSTO-PREVISTO-W.
           IF MULT-FORM-CO02 = 2
              COMPUTE GS-VALOR-PREVISTO = GS-QTDE-POR-FORM *
                                      CUSTO-PREVISTO-W
           ELSE
              COMPUTE GS-VALOR-PREVISTO = (GS-QTDE-POR-FORM *
                GS-QTDE-FORM) * CUSTO-PREVISTO-W.
      *----------------------------------------------------------------
       EXCLUI SECTION.
           DELETE OED020.
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       LE-FORNECEDOR SECTION.
           MOVE GS-FORNEC          TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-FORN.
       LE-BRINDE SECTION.
           MOVE GS-COD-BRINDE      TO CODIGO-CO02.
           READ COD002 INVALID KEY MOVE "********" TO NOME-CO02.
           MOVE NOME-CO02          TO GS-NOME-BRINDE.
       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "******" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-NOME-CURSO.
       CARREGAR-DADOS SECTION.
           START OED020 KEY IS = CHAVE-OE20 INVALID KEY CONTINUE.
           READ OED020 INVALID KEY INITIALIZE REG-OED020.
           MOVE NR-CONTRATO-OE20     TO  GS-CONTRATO
           MOVE ITEM-OE20            TO  GS-NR-ITEM
           MOVE CODBRINDE-OE20       TO  GS-COD-BRINDE CODIGO-CO02
           READ COD002 INVALID KEY MOVE "*******" TO NOME-CO02.
           MOVE NOME-CO02            TO  GS-NOME-BRINDE
           MOVE CURSO-OE20           TO  GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY MOVE "*****" TO NOME-IE11.
           MOVE NOME-IE11            TO  GS-NOME-CURSO.
           MOVE TURMA-OE20           TO  GS-TURMA
           MOVE QTDE-POR-FORM-OE20   TO  GS-QTDE-POR-FORM
           MOVE QTDE-FORM-OE20       TO  GS-QTDE-FORM
           MOVE CUSTO-UNIT-OE20      TO  GS-CUSTO-UNITARIO
           MOVE VALOR-PREVISTO-OE20  TO  GS-VALOR-PREVISTO
           MOVE DATA-VENCTO-OE20     TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-VENCTO
           MOVE DATA-SOLICIT-OE20    TO  GS-DATA-SOLICIT
           EVALUATE SUSP-PREV-DEF-OE20
             WHEN 0 MOVE "0-Previsto  "  TO GS-SUSP-PREV-DEF
             WHEN 1 MOVE "1-Definitivo"  TO GS-SUSP-PREV-DEF
             WHEN 2 MOVE "2-Suspenso  "  TO GS-SUSP-PREV-DEF
           END-EVALUATE.
           MOVE VALOR-PAGO-OE20      TO  GS-VALOR-PAGO
           MOVE DATA-PAGTO-OE20      TO  GS-DATA-PAGTO
           EVALUATE REALIZADO-OE20
             WHEN 0 MOVE "0-Não"     TO  GS-REALIZADO
             WHEN 1 MOVE "1-Sim"     TO  GS-REALIZADO
           END-EVALUATE.
           MOVE DIAS-PRAZO-OE20      TO  GS-PRAZO-MEDIO
           MOVE COD-FORNEC-OE20      TO  GS-FORNEC CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01            TO  GS-NOME-FORN.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-OED020
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO NR-CONTRATO-OE20
           MOVE GS-NR-ITEM            TO ITEM-OE20
           MOVE GS-COD-BRINDE         TO CODBRINDE-OE20
           MOVE GS-CURSO              TO CURSO-OE20
           MOVE GS-TURMA              TO TURMA-OE20
           MOVE GS-QTDE-POR-FORM      TO QTDE-POR-FORM-OE20
           MOVE GS-QTDE-FORM          TO QTDE-FORM-OE20
           MOVE GS-CUSTO-UNITARIO     TO CUSTO-UNIT-OE20
           MOVE GS-VALOR-PREVISTO     TO VALOR-PREVISTO-OE20
           MOVE GS-DATA-VENCTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-VENCTO-OE20
           MOVE GS-DATA-SOLICIT       TO DATA-SOLICIT-OE20
           MOVE GS-SUSP-PREV-DEF(1: 1) TO SUSP-PREV-DEF-OE20
           MOVE GS-VALOR-PAGO         TO VALOR-PAGO-OE20
           MOVE GS-DATA-PAGTO         TO DATA-PAGTO-OE20
           MOVE GS-REALIZADO(1: 1)    TO REALIZADO-OE20
           PERFORM CALCULA-PRAZO-MEDIO.
           MOVE GS-PRAZO-MEDIO        TO DIAS-PRAZO-OE20
           MOVE GS-FORNEC             TO COD-FORNEC-OE20.
       CALCULA-PRAZO-MEDIO SECTION.
           IF GS-DATA-PAGTO = ZEROS
              MOVE GS-DATA-VENCTO     TO GRTIME-DATE
           ELSE MOVE GS-DATA-PAGTO    TO GRTIME-DATE.
           MOVE GRTIME-DATE TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO GRTIME-DATE
           MOVE GS-CONTRATO           TO NR-CONTRATO-OE10.
           READ OED010 INVALID KEY MOVE ZEROS TO DATA-ULT-EVENTO-OE10.
           MOVE DATA-ULT-EVENTO-OE10  TO GRTIME-DATE-FINAL.
           IF GRTIME-DATE > GRTIME-DATE-FINAL
              MOVE "EXIBE-ERRO-DATA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
              MOVE 2                     TO GRTIME-TYPE
              MOVE 3                     TO GRTIME-FUNCTION
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DAYS-FINAL     TO GS-PRAZO-MEDIO.

       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
             WRITE REG-OED020 INVALID KEY
                 ADD 1 TO ITEM-OE20
               NOT INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-OED020 INVALID KEY
                 MOVE "Erro Regravacao OED020" TO GS-MENSAGEM-ERRO
                 MOVE ST-OED020 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-OE20.
           MOVE ZEROS           TO ITEM-OE20 GS-NR-ITEM.
           START OED020 KEY IS NOT < CHAVE-OE20
                    INVALID KEY MOVE "10" TO ST-OED020.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-OED020 = "10"
              READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
              NOT AT END
                IF NR-CONTRATO-OE20 <> GS-CONTRATO
                   MOVE "10" TO ST-OED020
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE ITEM-OE20     TO GS-NR-ITEM
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE NR-CONTRATO-OE20   TO GS-LINDET(1: 5)
           MOVE ITEM-OE20          TO GS-LINDET(6: 3)
           MOVE CURSO-OE20         TO GS-LINDET(9: 4)
           MOVE TURMA-OE20         TO GS-LINDET(13: 3)
           MOVE CODBRINDE-OE20     TO CODIGO-CO02
           READ COD002 INVALID KEY MOVE "*******" TO NOME-CO02.
           MOVE NOME-CO02          TO GS-LINDET(16: 17)
           MOVE QTDE-POR-FORM-OE20 TO GS-LINDET(34: 06)
           MOVE QTDE-FORM-OE20     TO GS-LINDET(40: 05)
           MOVE CUSTO-UNIT-OE20    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(45: 14)
           MOVE DATA-VENCTO-OE20   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(59: 11)
           MOVE DATA-SOLICIT-OE20  TO DATA-E
           MOVE DATA-E             TO GS-LINDET(70: 11)
           MOVE SUSP-PREV-DEF-OE20 TO GS-LINDET(81: 2)
           MOVE REALIZADO-OE20     TO GS-LINDET(83: 2)
           MOVE DATA-PAGTO-OE20    TO DATA-E
           MOVE DATA-E             TO GS-LINDET(85: 11)
           MOVE VALOR-PAGO-OE20    TO VALOR-E
           MOVE VALOR-E            TO GS-LINDET(96: 13).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "OEP020X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-OE20.
           MOVE ZEROS          TO ITEM-OE20.
           START OED020 KEY IS = CHAVE-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-OED020 = "10"
             READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
              NOT AT END
                IF NR-CONTRATO-OE20 <> GS-CONTRATO
                         MOVE "10" TO ST-OED020
                ELSE
                  MOVE NR-CONTRATO-OE20    TO LINDET-REL(1: 5)
                  MOVE ITEM-OE20           TO LINDET-REL(6: 3)
                  MOVE CURSO-OE20          TO LINDET-REL(9: 4)
                  MOVE TURMA-OE20          TO LINDET-REL(13: 3)
                  MOVE CODBRINDE-OE20      TO CODIGO-CO02
                  READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
                  END-READ
                  MOVE NOME-CO02           TO LINDET-REL(16: 12)
                  MOVE QTDE-POR-FORM-OE20  TO LINDET-REL(29: 6)
                  MOVE QTDE-FORM-OE20      TO LINDET-REL(35: 5)
                  MOVE CUSTO-UNIT-OE20     TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(40: 14)
                  MOVE DATA-VENCTO-OE20    TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV            TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(54: 11)
                  MOVE VALOR-PREVISTO-OE20 TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(65: 14)
                  MOVE DATA-SOLICIT-OE20   TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(79: 11)
                  MOVE SUSP-PREV-DEF-OE20  TO LINDET-REL(90: 2)
                  MOVE REALIZADO-OE20      TO LINDET-REL(92: 2)
                  MOVE DATA-PAGTO-OE20     TO DATA-E
                  MOVE DATA-E              TO LINDET-REL(94: 11)
                  MOVE VALOR-PAGO-OE20     TO VALOR-E
                  MOVE VALOR-E             TO LINDET-REL(105: 14)
                  MOVE DIAS-PRAZO-OE20     TO LINDET-REL(119: 6)
                  MOVE COD-FORNEC-OE20     TO LINDET-REL(125: 6)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 60 PERFORM CABECALHO
                  END-IF
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
           CLOSE COD002 OED010 OED020 IED011 CGD001 CAD004.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
