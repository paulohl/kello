       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP010.
       AUTHOR. MARELI AMANCIO VOLPATO
       DATE-WRITTEN. 12/01/2000.
      *FUNÇÃO: Cadastro de Contratos DE ORGANIZAÇÃO DE EVENTOS
      * A alteração em cima de uma data-ULT-EVENTO, altera
      * os dias de prazo de pagto de algum brinde(dias-prazo-OE20)
      * A exclusão não é permitida neste programa, porque muitos arqui-
      * vos estão relacionados com o cod040. Ex. brinde, contato

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CGPX001.
           COPY OEPX010.
           COPY OEPX020.
           COPY COPX040.
           COPY CGPX010.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY OEPW010.
       COPY OEPW020.
       COPY COPW040.
       COPY CGPW010.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP010.CPB".
           COPY "OEP010.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  NR-CONTRATO-W         PIC 9(4)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  TAXA-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
           05  DATA-PREV-VENDA-INI   PIC 9(8)     VALUE ZEROS.
      *    DATA-PREV-VENDA-INI - variável p/ controlar se houve mudança
      *    na mesma
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  MESANO-E              PIC 99/9999.
           05  CODIGO-W              PIC X(5)     VALUE SPACES.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas
           05  DATA-ULT-EVENTO-INI   PIC 9(8)     VALUE ZEROS.
      *    DATA-ULT-EVENTO-INI - variável p/ controlar se houve mudança
      *    na mesma

           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(112)  VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(78)   VALUE
           "CONFERENCIA DO CADASTRO DE CONTRATO -ORG.EVENTO".
           05  FILLER              PIC X(37)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "CONT DTA-ASSIN. DTA-ULT-EV REPRESENTANTE   PRESIDENTE      T
      -    "ESOUREIRO      TOT-ALUN   VALOR-TOTAL TOT-PARC TIPO-RECEBIME
      -    "NTO    ".

       01  CAB05.
           05  FILLER                   PIC X(132)  VALUE
           "CONDICAO-RECEBIMENTO           CONDICAO-REAJUSTE
      -    "  TAXA-JUROS MULTA-ATRASO         MULTA-RESCISAO      ".


       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "OED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "OED020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-OED020.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           OPEN I-O OED010 OED020.
           OPEN INPUT COD040 CAD010 CGD001 CGD010.
           IF ST-OED010 = "35"
              CLOSE OED010      OPEN OUTPUT OED010
              CLOSE OED010      OPEN I-O OED010
           END-IF.
           IF ST-OED020 = "35"
              CLOSE OED020      OPEN OUTPUT OED020
              CLOSE OED020      OPEN I-O OED020
           END-IF.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
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
      *             MOVE GS-CONTRATO TO NR-CONTRATO-W
                    PERFORM LIMPAR-DADOS
      *             IF GS-TIPO-GRAVACAO <> 1
      *                 MOVE NR-CONTRATO-W TO GS-CONTRATO
      *             END-IF
                    MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI
                    PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
      *             PERFORM IMPRIME-RELATORIO
               WHEN GS-LE-REPRES-TRUE
                    PERFORM LE-REPRES
               WHEN GS-LE-ALUNO-PRES-TRUE
                    PERFORM LE-ALUNO-PRESIDENTE
               WHEN GS-LE-ALUNO-TES-TRUE
                    PERFORM LE-ALUNO-TESOUREIRO
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-ACHAR-SEQ-CONT-TRUE
                    CONTINUE
      *             PERFORM ACHAR-SEQ-CONTRATO
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4)  TO GS-CONTRATO.
           MOVE PASSAR-STRING-1(22: 11) TO GS-DESCRICAO-CONTRATO(1: 12)
           MOVE PASSAR-STRING-1(1: 21) TO GS-DESCRICAO-CONTRATO(13: 21).

       ACHAR-SEQ-CONTRATO SECTION.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-REPRES
             WHEN 2 PERFORM CARREGA-POP-UP-COMISSAO
             WHEN 3 PERFORM CARREGA-POP-UP-COMISSAO
           END-EVALUATE.
       CARREGA-POP-UP-REPRES SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CG01
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                MOVE NOME-CG01(1: I)       TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD001
                ELSE
                  MOVE NOME-CG01         TO GS-LINDET1(1: 30)
                  MOVE CODIGO-CG01       TO GS-LINDET1(33: 06)
                  MOVE "INSERE-POP-UP-FORNEC" TO DS-PROCEDURE
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
               ELSE MOVE GS-LINDET1(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       CARREGA-POP-UP-COMISSAO SECTION.
           MOVE "CLEAR-LIST-BOX-COMISSAO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 0           TO CLASSIF-CG10
           MOVE GS-CONTRATO TO CODIGO-CG10(1: 4)
           MOVE ZEROS       TO CODIGO-CG10(5: 4)
           START CGD010 KEY IS NOT < COD-COMPL-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
               NOT AT END
                MOVE CODIGO-CG10(1: 4)   TO NR-CONTRATO-W
                IF NR-CONTRATO-W <> GS-CONTRATO
                   MOVE "10" TO ST-CGD010
                ELSE
                  MOVE COMPRADOR-CG10    TO GS-LINDET1(1: 30)
                  MOVE CODIGO-CG10(5: 4) TO GS-LINDET1(33: 04)
                  MOVE "INSERE-POP-UP-COMISSAO" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET1(33: 6) TO GS-REPRESENTANTE
               MOVE GS-LINDET1(1: 30) TO GS-NOME-REPRESENTANTE
             WHEN 2
               MOVE GS-LINDET1(33: 4) TO GS-ALUNO-PRES
               MOVE GS-LINDET1(1: 30) TO GS-NOME-ALUNO-PRES
             WHEN 3
               MOVE GS-LINDET1(33: 4) TO GS-ALUNO-TES
               MOVE GS-LINDET1(1: 30) TO GS-NOME-ALUNO-TES
           END-EVALUATE.
       LE-REPRES SECTION.
           MOVE GS-REPRESENTANTE   TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-REPRESENTANTE.
       LE-ALUNO-PRESIDENTE SECTION.
           MOVE 0 TO CLASSIF-CG10.
           MOVE GS-CONTRATO   TO CODIGO-CG10(1: 4)
           MOVE GS-ALUNO-PRES TO CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-NOME-ALUNO-PRES.
       LE-ALUNO-TESOUREIRO SECTION.
           MOVE 0 TO CLASSIF-CG10.
           MOVE GS-CONTRATO   TO CODIGO-CG10(1: 4)
           MOVE GS-ALUNO-TES  TO CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-NOME-ALUNO-TES.
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GS-TIPO-GRAVACAO.
           MOVE GS-CONTRATO TO NR-CONTRATO-OE10.
           START OED010 KEY IS = NR-CONTRATO-OE10 INVALID KEY CONTINUE.
           READ OED010 INVALID KEY INITIALIZE REG-OED010
             MOVE GS-CONTRATO              TO NR-CONTRATO-CO40
             READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40
                                     MOVE ZEROS  TO CIDADE-CO40
             END-READ
             MOVE IDENTIFICACAO-CO40   TO GS-DESCRICAO-CONTRATO(1: 11)
             MOVE CIDADE-CO40              TO CIDADE
             READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
             END-READ
             MOVE NOME-CID             TO GS-DESCRICAO-CONTRATO(13: 21)
           NOT INVALID KEY
             MOVE 1 TO GS-TIPO-GRAVACAO
             MOVE NR-CONTRATO-OE10         TO GS-CONTRATO
                                              NR-CONTRATO-CO40
             READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40
                                     MOVE ZEROS  TO CIDADE-CO40
             END-READ
             MOVE IDENTIFICACAO-CO40   TO GS-DESCRICAO-CONTRATO(1: 11)
             MOVE CIDADE-CO40              TO CIDADE
             READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
             END-READ
             MOVE NOME-CID             TO GS-DESCRICAO-CONTRATO(13: 21)
             MOVE REPRESENTANTE-OE10       TO GS-REPRESENTANTE
                                              CODIGO-CG01
             READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
             END-READ
             MOVE NOME-CG01                TO GS-NOME-REPRESENTANTE
             MOVE TOTAL-ALUNO-OE10         TO GS-TOT-ALUNOS
             MOVE ASSINATURA-OE10          TO DATA-INV
             CALL "GRIDAT1" USING DATA-INV
             MOVE DATA-INV                 TO GS-DATA-ASSINATURA
             MOVE DATA-ULT-EVENTO-OE10     TO DATA-INV
                                              DATA-ULT-EVENTO-INI
             CALL "GRIDAT1" USING DATA-INV
             MOVE DATA-INV                 TO GS-DATA-ULT-EVENTO
             MOVE ALUNO-PRES-OE10          TO GS-ALUNO-PRES
                                              CODIGO-CG10(5: 4)
             MOVE NR-CONTRATO-OE10         TO CODIGO-CG10(1: 4)
             MOVE 0                        TO CLASSIF-CG10
             READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10
             END-READ
             MOVE COMPRADOR-CG10           TO GS-NOME-ALUNO-PRES
             MOVE ALUNO-TESOU-OE10         TO GS-ALUNO-TES
                                              CODIGO-CG10(5: 4)
             MOVE NR-CONTRATO-OE10         TO CODIGO-CG10(1: 4)
             MOVE 0                        TO CLASSIF-CG10
             READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10
             END-READ
             MOVE COMPRADOR-CG10           TO GS-NOME-ALUNO-TES
             MOVE VLR-TOTAL-OE10           TO GS-VLR-TOTAL
             MOVE QTDE-PARCELA-OE10        TO GS-QTDE-PARCELA
             EVALUATE TIPO-RECTO-OE10
               WHEN 1 MOVE "1-'Parcelamento indiv.' de cada aluno" TO
                      GS-TIPO-RECTO
               WHEN 2 MOVE "2-Direto da Comissão" TO GS-TIPO-RECTO
             END-EVALUATE
             MOVE CONDICAO-RECTO-OE10      TO GS-COND-RECTO
             MOVE CONDICAO-REAJUSTE-OE10   TO GS-COND-REAJUSTE
             MOVE TAXA-JURO-ATRASO-OE10    TO GS-TAXA-JURO-ATRASO
             MOVE MULTA-ATRASO-OE10        TO GS-MULTA-ATRASO
             MOVE MULTA-RESCISAO-OE10      TO GS-MULTA-RESCISAO
           END-READ.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-OED010
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO                 TO NR-CONTRATO-OE10
           MOVE GS-DATA-ASSINATURA          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                    TO ASSINATURA-OE10
           MOVE GS-DATA-ULT-EVENTO          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                    TO DATA-ULT-EVENTO-OE10
           MOVE GS-MULTA-RESCISAO           TO MULTA-RESCISAO-OE10
           MOVE GS-REPRESENTANTE            TO REPRESENTANTE-OE10
           MOVE GS-TOT-ALUNOS               TO TOTAL-ALUNO-OE10
           MOVE GS-ALUNO-PRES               TO ALUNO-PRES-OE10
           MOVE GS-ALUNO-TES                TO ALUNO-TESOU-OE10
           MOVE GS-VLR-TOTAL                TO VLR-TOTAL-OE10
           MOVE GS-QTDE-PARCELA             TO QTDE-PARCELA-OE10
           MOVE GS-COND-RECTO               TO CONDICAO-RECTO-OE10
           MOVE GS-COND-REAJUSTE            TO CONDICAO-REAJUSTE-OE10
           MOVE GS-TAXA-JURO-ATRASO         TO TAXA-JURO-ATRASO-OE10
           MOVE GS-MULTA-ATRASO             TO MULTA-ATRASO-OE10
           MOVE GS-MULTA-RESCISAO           TO MULTA-RESCISAO-OE10
           MOVE GS-TIPO-RECTO(1: 1)         TO TIPO-RECTO-OE10.

       GRAVA-DADOS SECTION.
           WRITE REG-OED010 INVALID KEY
               MOVE "Erro Gravação OED010" TO GS-MENSAGEM-ERRO
               MOVE ST-OED010 TO GS-MENSAGEM-ERRO(24: 5)
               MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-OED010 INVALID KEY
                 MOVE "Erro Regravação OED010" TO GS-MENSAGEM-ERRO
                 MOVE ST-OED010 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           IF DATA-ULT-EVENTO-OE10 <> DATA-PREV-VENDA-INI
              PERFORM ALTERA-DIAS-PRAZO-OED020.

       EXCLUI SECTION.
      *    A EXCLUSÃ0 ESTÁ DESATIVADA, PORQUE ESTE ARQUIVO ESTÁ LIGADO
      *    COM VÁRIOS OUTROS ARQUIVOS. EX. brindes, contatos...

           DELETE OED010.
           PERFORM LIMPAR-DADOS.
       ALTERA-DIAS-PRAZO-OED020 SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-OE20.
           MOVE ZEROS       TO ITEM-OE20.
           START OED020 KEY IS NOT < CHAVE-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
             READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
               NOT AT END
                 IF NR-CONTRATO-OE20 <> GS-CONTRATO
                    MOVE "10" TO ST-OED020
                 ELSE
                    PERFORM CALCULA-PRAZO-MEDIO
                    REWRITE REG-OED020
                    END-REWRITE
                 END-IF
             END-READ
           END-PERFORM.
       CALCULA-PRAZO-MEDIO SECTION.
           IF DATA-PAGTO-OE20 = ZEROS
              MOVE DATA-VENCTO-OE20      TO GRTIME-DATE
           ELSE MOVE DATA-PAGTO-OE20     TO DATA-INV
                CALL "GRIDAT2" USING DATA-INV
                MOVE DATA-INV            TO GRTIME-DATE.
           MOVE DATA-ULT-EVENTO-OE10     TO GRTIME-DATE-FINAL.
           MOVE 2                     TO GRTIME-TYPE
           MOVE 3                     TO GRTIME-FUNCTION
           CALL "GRTIME" USING PARAMETROS-GRTIME
           MOVE GRTIME-DAYS-FINAL     TO DIAS-PRAZO-OE20.


       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "OEP010" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------------
      *IMPRIME-RELATORIO SECTION.
      *    COPY "COND-IMP".
      *    MOVE ZEROS TO PAG-W.
      *    OPEN OUTPUT RELAT.
      *    MOVE GS-CONTRATO    TO NR-CONTRATO-OE10.
      *    READ OED010 INVALID KEY MOVE SPACES TO REG-OED010.
      *
      *    MOVE SPACES TO LINDET.
      *    ADD 3 TO LIN.
      *    IF LIN > 56 PERFORM CABECALHO.
      *    MOVE NR-CONTRATO-OE10         TO LINDET-REL(1: 5)
      *    MOVE ASSINATURA-OE10          TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV                 TO DATA-E
      *    MOVE DATA-E                   TO LINDET-REL(6: 11)
      *    MOVE DATA-ULT-EVENTO-OE10     TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV                 TO DATA-E
      *    MOVE DATA-E                   TO LINDET-REL(17: 11)
      *    MOVE REPRESENTANTE-OE10       TO CODIGO-CG01
      *    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
      *    END-READ
      *    MOVE NOME-CG01                TO LINDET-REL(28: 15)
      *    MOVE ALUNO-PRES-OE10          TO CODIGO-CG10(5: 4)
      *    MOVE NR-CONTRATO-OE10         TO CODIGO-CG10(1: 4)
      *    MOVE 0                        TO CLASSIF-CG10
      *    READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10
      *    END-READ
      *    MOVE COMPRADOR-CG10           TO LINDET-REL(44: 15)
      *    MOVE ALUNO-TESOU-OE10         TO CODIGO-CG10(5: 4)
      *    MOVE NR-CONTRATO-OE10         TO CODIGO-CG10(1: 4)
      *    MOVE 0                        TO CLASSIF-CG10
      *    READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10
      *    END-READ
      *    MOVE COMPRADOR-CG10           TO LINDET-REL(60: 15)
      *    MOVE TOTAL-ALUNO-OE10         TO LINDET-REL(76: 9)
      *    MOVE VLR-TOTAL-OE10           TO VALOR-E
      *    MOVE VALOR-E                  TO LINDET-REL(85: 14)
      *    MOVE QTDE-PARCELA-OE10        TO LINDET-REL(99: 9)
      *
      *    EVALUATE TIPO-RECTO-OE10
      *      WHEN 1 MOVE "Parcelamento indiv" TO LINDET-REL(108: 20)
      *      WHEN 2 MOVE "Direto da Comissão" TO LINDET-REL(108: 20)
      *    END-EVALUATE
      *    WRITE REG-RELAT FROM LINDET.
      *
      *    MOVE CONDICAO-RECTO-OE10      TO LINDET-REL(1: 31)
      *    MOVE CONDICAO-REAJUSTE-OE10   TO LINDET-REL(32: 31)
      *    MOVE TAXA-JURO-ATRASO-OE10    TO TAXA-E
      *    MOVE TAXA-E                   TO LINDET-REL(63: 11)
      *    MOVE MULTA-ATRASO-OE10        TO LINDET-REL(74: 21)
      *    MOVE MULTA-RESCISAO-OE10      TO LINDET-REL(95: 20)
      *    WRITE REG-RELAT FROM LINDET.
      *
      *    MOVE SPACES TO LINDET-REL
      *    WRITE REG-RELAT FROM LINDET.
      *
      *    CLOSE RELAT.
      *    COPY "DESC-IMP".
      *
      *CABECALHO SECTION.
      *    ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
      *    IF PAG-W = 1
      *       WRITE REG-RELAT FROM CAB01
      *    ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
      *    WRITE REG-RELAT FROM CAB02 AFTER 2.
      *    WRITE REG-RELAT FROM CAB03.
      *    WRITE REG-RELAT FROM CAB04.
      *    WRITE REG-RELAT FROM CAB05.
      *    WRITE REG-RELAT FROM CAB03.
      *    MOVE 7 TO LIN.
      *-----------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD010 CAD010 COD040 OED010 OED020 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
