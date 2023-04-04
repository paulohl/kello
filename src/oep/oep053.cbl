       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP053.
       DATE-WRITTEN. 23/08/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 505
      * LISTAR APENAS OS CONTRATOS ATIVOS, OU SEJA, STATUS => 50
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY OEPX010.
           COPY OEPX020.
           COPY CRPX020.
           COPY IEPX011.
           COPY CAPX010.
      *    COPY CAPX011.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS ASSINATURA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS OBJ-ARREC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PATROCINIO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PERC-PATROC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PADRAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MESANO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PAT-FORM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REPRESENT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY IEPW011.
       COPY CAPW010.
      *COPY CAPW011.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY OEPW010.
       COPY OEPW020.
       COPY CRPW020.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  CURSO-WK            PIC X(10).
           05  CIDADE-WK           PIC X(10).
           05  REPRESENT-WK        PIC X(10).
           05  QT-FORM-WK          PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  QT-FOTOS-WK         PIC 9(5).
      *    QT-FOTOS-WK = qt-form-wk * prev-fotos-co05(cadastro padrao)
           05  MESANO-WK           PIC 9(6).
           05  ASSINATURA-WK       PIC 9(8).
           05  PRECO-WK            PIC 9(7)V99.
      *    se mesano-wk < 199506 preço-wk = 8,00
      *    senão                 preço-wk = 11,00
           05  OBJ-ARREC-WK        PIC 9(7)V99.
      *    OBJ-ARREC-WK = CONTAS A RECEBER + JUROS
           05  VLR-PAGO-WK         PIC 9(7)V99.
           05  VLR-PAGAR-WK        PIC 9(7)V99.
      *    VLR-PAGO-WK E VLR-PAGAR-WK = vlr-comissao + custo-prev-brinde
      *    ao qual o custo-prev-brinde=qtde-form * qtde-por-form * custo
      *    Após acumular, multiplicar pela taxa-acumulada
           05  PATROCINIO-WK       PIC 9(7)V99.
      *    PATROCINIO-WK = vlr-pago-wk + vlr-pagar-wk
           05  PERC-PATROC-WK      PIC 9(2)V99.
      *    PERC-PATROC-WK = (patrocinio-wk / obj-ARREC-wk) * 100
           05  PAT-FORM-WK         PIC 9(6)V99.
      *    PAT-FORM-WK = patrocinio-wk / qt-form-wk
           05  PM-WK               PIC 9(3).
      *    VLR-GERAL(acumulado de todos os brindes) = custo-brinde *
      *               (dias-prazo-OE20 / 30) + vlr-geral.
      *    PM-WK arredondado = vlr-geral / patrocinio-wk

      *    TAXA-ACUMULADA(loop do contador até = pm-wk, sendo que a
      *    taxa-acumulada inicia-se c/ 1 fora do loop)=
      *    taxa-acumulada * ((taxa-w / 100) + 1)
           05  ORG-EVENTO-WK       PIC 9.
      *    ORG-EVENTO-WK = 0(NÃO)  1(SIM)= cobertura-co40 = 1,3,4,7.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(134).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP053.CPB".
           COPY "OEP053.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD011             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-OED010             PIC XX       VALUE SPACES.
           05  ST-OED020             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CAD011             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZ,Z     BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.

           05  OBJ-ARREC-G           PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGO-G            PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR-G           PIC 9(8)V99  VALUE ZEROS.
           05  PATROCINIO-G          PIC 9(8)V99  VALUE ZEROS.
           05  PAT-FORM-G            PIC 9(8)V99  VALUE ZEROS.
           05  QT-CONTRATO-G         PIC 9(4)     VALUE ZEROS.
           05  QT-FOTO-G             PIC 9(8)     VALUE ZEROS.
           05  QT-FORM-G             PIC 9(8)     VALUE ZEROS.
      *    VARIAVEIS P/ CALCULO DE PATROCINIO PAGOS E A PAGAR
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR           PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGO            PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  TAXA-W                PIC 9(3)V99999 VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  VLR-PAGO              PIC 9(8)V99  VALUE ZEROS.
           05  VLR-PAGAR             PIC 9(8)V99  VALUE ZEROS.
           05  VLR-GERAL             PIC 9(8)V99  VALUE ZEROS.
           05  PAT-FORM-W            PIC 9(8)V99  VALUE ZEROS.
           05  PERC-PATROC-W         PIC 9(3)V99   VALUE ZEROS.

           05  TOT-ARRECADAR         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ACUM-ARRECADAR    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-JR-ARRECADAR      PIC 9(8)V99  VALUE ZEROS.
           05  TOT-PAGOS-OE          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-RESTANTE-OE       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRASADO-OE       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ACUM-PAGOS-OE     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ACUM-RESTANTE-OE  PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ACUM-ATRASADO-OE  PIC 9(8)V99  VALUE ZEROS.

           05  TOT-FORM              PIC 9(7)     VALUE ZEROS.
           05  TOT-OBJ-ARREC         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-PATROCINIO        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VLR-PAGO          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VLR-PAGAR         PIC 9(8)V99  VALUE ZEROS.

           05  ASSINATURA-ANT        PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  MESANO-ANT            PIC 9(6)     VALUE ZEROS.
           05  PADRAO-ANT            PIC X        VALUE SPACES.
           05  REPRESENT-ANT         PIC X(10)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(10)    VALUE SPACES.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(75)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(36)   VALUE
           "RELATORIO 505          - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(20)   VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(132)   VALUE
           "CONT IDENTIFIC. CIDADE     FORM P MES/ANO REPRESENT. ASSINAT
      -    "URA   OBJ.ARREC  R$-PATROC.    PAT/FORM %-PAT    VLR-PAGO
      -    "VLR-PAGAR".
       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(132)   VALUE SPACES.

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
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
      *    MOVE "CAD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD011.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "OED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-OED010.
           MOVE "OED020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-OED020.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           OPEN INPUT CAD010 IED011 COD040 COD005 OED010 OED020
                      COD002 CRD020
      *               CAD011 .
      *    IF ST-CAD011 <> "00"
      *       MOVE "ERRO ABERTURA CAD011: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CAD011 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED010 <> "00"
              MOVE "ERRO ABERTURA OED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-OED020 <> "00"
              MOVE "ERRO ABERTURA OED020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-OED020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM ZERA-VARIAVEIS-REL
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-INT-DATA
           END-EVALUATE
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
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       VERIFICA-INT-DATA SECTION.
           MOVE GS-MESANO-INI  TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-INI
           MOVE GS-MESANO-FIM  TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I       TO MESANO-FIM.
           MOVE 0 TO GS-ERRO-DATA.
           IF MESANO-INI > MESANO-FIM MOVE 1 TO GS-ERRO-DATA.
           MOVE GS-VENCTO-INI  TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-INI
           MOVE GS-VENCTO-FIM  TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-FIM
           IF VENCTO-INI > VENCTO-FIM MOVE 2 TO GS-ERRO-DATA.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO NR-CONTRATO-OE10.

           PERFORM UNTIL ST-OED010 = "10"
             READ OED010 NEXT RECORD AT END MOVE "10" TO ST-OED010
              NOT AT END
                  IF ASSINATURA-OE10 > VENCTO-FIM CONTINUE
                  ELSE MOVE ASSINATURA-OE10 TO GS-EXIBE-VENCTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       PERFORM MOVER-DADOS-WORK
                  END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE NR-CONTRATO-OE10 TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           IF STATUS-CO40 < 50 CONTINUE
           ELSE
              MOVE MESANO-PREV-CO40   TO MESANO-WK
              MOVE ASSINATURA-OE10    TO ASSINATURA-WK
              MOVE NR-CONTRATO-OE10   TO CONTRATO-WK
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID           TO CIDADE-WK
              MOVE REPRESENTANTE-OE10 TO CODIGO-CG01
              READ CGD001 INVALID KEY
                   MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01          TO REPRESENT-WK
              MOVE ZEROS TO VLR-PAGAR VLR-PAGO

              IF COBERTURA-CO40 = 1 OR 3 OR 4 OR 7
      *          tipo de cobertura que tem org-evento
                 MOVE 1 TO ORG-EVENTO-WK
              ELSE MOVE 0 TO ORG-EVENTO-WK
              END-IF
              MOVE TOTAL-ALUNO-OE10   TO QT-FORM-WK
              MOVE IDENTIFICACAO-CO40 TO CURSO-WK
              MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
              READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
              END-READ
              COMPUTE QT-FOTOS-WK = PREV-FOTOS-CO05 * QT-FORM-WK
              PERFORM CALCULAR-DADOS-BRINDE
              WRITE REG-WORK
           END-IF.
      * ---------ROTINAS PERFORMADAS(PELA SEÇÃO MOVER-DADOS-WORK)---
       CALCULAR-DADOS-BRINDE SECTION.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA VLR-GERAL JUROS-TOTAL
                         JUROS-PAGO JUROS-PAGAR.
           MOVE NR-CONTRATO-OE10   TO NR-CONTRATO-OE20.
           MOVE ZEROS              TO ITEM-OE20.
           START OED020 KEY IS NOT < CHAVE-OE20 INVALID KEY
                 MOVE "10" TO ST-OED020.
           PERFORM UNTIL ST-OED020 = "10"
              READ OED020 NEXT RECORD AT END MOVE "10" TO ST-OED020
                NOT AT END
                  IF NR-CONTRATO-OE20 <> NR-CONTRATO-CO40
                     MOVE "10" TO ST-OED020
                  ELSE
                     MOVE CODBRINDE-OE20 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                     END-READ
                     IF CUSTO-UNIT-OE20 <> ZEROS
                        MOVE CUSTO-UNIT-OE20 TO CUSTO-PREVISTO
                     ELSE MOVE VALOR-CO02 TO CUSTO-PREVISTO
                     END-IF
                     IF REALIZADO-OE20 <> 1
                        IF MULT-FORM-CO02 = 2
                           COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                             QTDE-POR-FORM-OE20
                        ELSE COMPUTE CUSTO-W = (QTDE-POR-FORM-OE20 *
                            QTDE-FORM-OE20) * CUSTO-PREVISTO
                        END-IF
                     ELSE MOVE VALOR-PAGO-OE20 TO CUSTO-W
                     END-IF
                     PERFORM CALCULA-JUROS-BRINDE-OE
                     IF REALIZADO-OE20 = 0
                        ADD CUSTO-W TO VLR-PAGAR
                        ADD JUROS-W TO JUROS-PAGAR
                     ELSE ADD CUSTO-W TO VLR-PAGO
                          ADD JUROS-W TO JUROS-PAGO
                     END-IF
                     COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30
                     COMPUTE VLR-GERAL = (CUSTO-W * MESES-W) + VLR-GERAL
                  END-IF
              END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / (VLR-PAGO +
                                         VLR-PAGAR)
           MOVE PRAZO-MEDIO TO PM-WK.
           PERFORM VERIFICA-PRECO.
           COMPUTE VLR-PAGO-WK = VLR-PAGO + JUROS-PAGO
           COMPUTE VLR-PAGAR-WK = VLR-PAGAR + JUROS-PAGAR
           COMPUTE JUROS-TOTAL = JUROS-PAGO + JUROS-PAGAR
           COMPUTE PATROCINIO-WK = VLR-PAGO-WK + VLR-PAGAR-WK
           PERFORM VERIFICA-TOT-ARRECADADO

      *    EM CASO DA DE NÃO CONSTAR NADA NO CTAS A RECEBER
           IF OBJ-ARREC-WK = ZEROS MOVE 1 TO OBJ-ARREC-WK.
           COMPUTE PERC-PATROC-WK = (PATROCINIO-WK / OBJ-ARREC-WK)
                                     * 100
           COMPUTE PAT-FORM-WK = PATROCINIO-WK / QT-FORM-WK.
       CALCULA-JUROS-BRINDE-OE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-OE20 / 30
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
                 COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                         ((GS-TAXA /100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).

       VERIFICA-PRECO SECTION.
           IF MESANO-WK < 199506 MOVE 8 TO PRECO-WK
           ELSE MOVE 11 TO PRECO-WK.
       VERIFICA-TOT-ARRECADADO SECTION.
           MOVE ZEROS TO TOT-ARRECADAR TOT-ACUM-ARRECADAR
                         TOT-PAGOS-OE TOT-ATRASADO-OE TOT-RESTANTE-OE
                         TOT-ACUM-PAGOS-OE TOT-ACUM-ATRASADO-OE
                         TOT-ACUM-RESTANTE-OE.
           MOVE 0                      TO COD-COMPL-CR20(1: 1)
           MOVE NR-CONTRATO-OE10       TO COD-COMPL-CR20(2: 4)
           MOVE ZEROS                  TO COD-COMPL-CR20(6: 4) SEQ-CR20
           START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      MOVE COD-COMPL-CR20(2: 4) TO CONTRATO-W
                      IF CONTRATO-W <> NR-CONTRATO-OE10
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF TIPO-DOCTO-CR20 <> 2
                            CONTINUE
                         ELSE
                            PERFORM CALCULA-DIAS-OE
                            IF DATA-RCTO-CR20 <> ZEROS
                               ADD VALOR-LIQ-CR20    TO TOT-PAGOS-OE
                               COMPUTE TOT-ACUM-PAGOS-OE =
                                 (GRTIME-DAYS-FINAL *
                                  VALOR-LIQ-CR20) + TOT-ACUM-PAGOS-OE
                               IF VALOR-SALDO-CR20 > 0
                                  ADD VALOR-SALDO-CR20 TO
                                      TOT-ATRASADO-OE
                                  COMPUTE TOT-ACUM-ATRASADO-OE =
                                  (GRTIME-DAYS-FINAL * VALOR-SALDO-CR20)
                                                 + TOT-ACUM-ATRASADO-OE
                               END-IF
                            ELSE
                               IF DATA-VENCTO-CR20 < DATA-DIA-I
                                  ADD VALOR-TOT-CR20 TO TOT-ATRASADO-OE
                                  COMPUTE TOT-ACUM-ATRASADO-OE =
                                  (GRTIME-DAYS-FINAL * VALOR-TOT-CR20)
                                   + TOT-ACUM-ATRASADO-OE
                               ELSE
                                 ADD VALOR-TOT-CR20  TO TOT-RESTANTE-OE
                                 COMPUTE TOT-ACUM-RESTANTE-OE =
                                 (GRTIME-DAYS-FINAL * VALOR-TOT-CR20) +
                                  TOT-ACUM-RESTANTE-OE
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO = (TOT-ACUM-PAGOS-OE /
                                  TOT-PAGOS-OE) / 30
           MOVE TOT-PAGOS-OE    TO CUSTO-PREVISTO
           ADD TOT-PAGOS-OE     TO TOT-ARRECADAR
           PERFORM CALCULO-JUROS-OE
           MOVE JUROS-W      TO TOT-JR-ARRECADAR

           COMPUTE PRAZO-MEDIO = (TOT-ACUM-ATRASADO-OE /
                                  TOT-ATRASADO-OE) / 30
           MOVE TOT-ATRASADO-OE TO CUSTO-PREVISTO
           ADD TOT-ATRASADO-OE  TO TOT-ARRECADAR
           PERFORM CALCULO-JUROS-OE
           ADD JUROS-W       TO TOT-JR-ARRECADAR

           COMPUTE PRAZO-MEDIO = (TOT-ACUM-RESTANTE-OE /
                                  TOT-RESTANTE-OE) / 30
           MOVE TOT-RESTANTE-OE    TO CUSTO-PREVISTO
           ADD TOT-RESTANTE-OE     TO TOT-ARRECADAR
           PERFORM CALCULO-JUROS-OE
           ADD JUROS-W       TO TOT-JR-ARRECADAR

           COMPUTE OBJ-ARREC-WK = TOT-ARRECADAR + TOT-JR-ARRECADAR.
       CALCULA-DIAS-OE SECTION.
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE DATA-RCTO-CR20      TO GRTIME-DATE
           ELSE MOVE DATA-VENCTO-CR20 TO GRTIME-DATE.
           IF GRTIME-DATE = 22004001
              DISPLAY COD-COMPL-CR20 AT 2311
              DISPLAY SEQ-CR20       AT 2330.
      *       DISPLAY (23, 11) COD-COMPL-CR20
      *       DISPLAY (23, 30) SEQ-CR20.

           MOVE DATA-ULT-EVENTO-OE10   TO GRTIME-DATE-FINAL.
           MOVE 2                     TO GRTIME-TYPE
           MOVE 3                     TO GRTIME-FUNCTION
           CALL "GRTIME" USING PARAMETROS-GRTIME.

       CALCULO-JUROS-OE SECTION.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-PREVISTO * (TAXA-ACUMULADA - 1).
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET
           INITIALIZE REG-WORK
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-LINDET
               END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           PERFORM TOTALIZA-TELA.
       TOTALIZA-TELA SECTION.
           MOVE SPACES TO GS-LINTOT
           MOVE ZEROS  TO GS-CONT-LINTOT
           MOVE "OBJ.ARREC: "  TO GS-LINTOT(1: 11)
           MOVE OBJ-ARREC-G    TO VALOR-E
           MOVE VALOR-E        TO GS-LINTOT(12: 13)
           MOVE "T.PAGO.: "    TO GS-LINTOT(35: 9)
           MOVE VLR-PAGO-G     TO VALOR-E
           MOVE VALOR-E        TO GS-LINTOT(44: 13)
           MOVE "T.PAGAR: "    TO GS-LINTOT(68: 9)
           MOVE VLR-PAGAR-G    TO VALOR-E
           MOVE VALOR-E        TO GS-LINTOT(77: 13)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINTOT
           MOVE "PATROCIN.: "  TO GS-LINTOT(1: 11)
           MOVE PATROCINIO-G   TO VALOR-E
           MOVE VALOR-E        TO GS-LINTOT(12: 13)
           MOVE "PT.FORM: "    TO GS-LINTOT(35: 9)
           COMPUTE PAT-FORM-G = PATROCINIO-G / QT-FORM-G.
           MOVE PAT-FORM-G     TO VALOR-E
           MOVE VALOR-E        TO GS-LINTOT(44: 13)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINTOT
           MOVE "TOT.FOTOS: "  TO GS-LINTOT(1: 11)
           MOVE QT-FOTO-G      TO QTDE-E
           MOVE QTDE-E         TO GS-LINTOT(12: 13)
           MOVE "T.FORM.: "    TO GS-LINTOT(35: 9)
           MOVE QT-FORM-G      TO QTDE-E
           MOVE QTDE-E         TO GS-LINTOT(44: 13)
           MOVE "T.CONTR: "    TO GS-LINTOT(68: 9)
           MOVE QT-CONTRATO-G  TO QTDE-E
           MOVE QTDE-E         TO GS-LINTOT(77: 13)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "PADRÃO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < PADRAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "MES/ANO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < MESANO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "REPRESENT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < REPRESENT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "ASSINATURA" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ASSINATURA-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "OBJ-ARREC" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < OBJ-ARREC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 8
                MOVE "PATROCINIO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < PATROCINIO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 9
                MOVE "PAT/FORM" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < PAT-FORM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 10
                MOVE "%-PATROC." TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < PERC-PATROC-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       VERIFICA-SUBTOTAL SECTION.
           EVALUATE GS-ORDEM
      *      WHEN 1
      *       IF CONTRATO-ANT NOT = ZEROS
      *          IF CONTRATO-ANT NOT = CONTRATO-WK
      *             PERFORM TOTALIZA
             WHEN 2
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF ASSINATURA-ANT  NOT = ASSINATURA-WK
                 IF ASSINATURA-ANT NOT = ASSINATURA-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

       MOVER-DADOS-LINDET SECTION.
           PERFORM VERIFICA-SUBTOTAL.
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE CURSO-WK          TO GS-LINDET(6: 11)
           MOVE CIDADE-WK         TO GS-LINDET(17: 11)
           MOVE QT-FORM-WK        TO GS-LINDET(28: 5)
           ADD QT-FORM-WK         TO QT-FORM-G TOT-FORM
           MOVE PADRAO-WK         TO GS-LINDET(33: 2)
      *    MOVE QT-FOTOS-WK       TO GS-LINDET(8: 7)
           ADD QT-FOTOS-WK        TO QT-FOTO-G
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(35: 8)
           MOVE REPRESENT-WK      TO GS-LINDET(43: 11)
           MOVE ASSINATURA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(54: 11)
      *    MOVE PRECO-WK          TO VALOR-E2
      *    MOVE VALOR-E2          TO GS-LINDET(34: 5)
           MOVE OBJ-ARREC-WK      TO VALOR-E1
           ADD OBJ-ARREC-WK       TO OBJ-ARREC-G TOT-OBJ-ARREC
           MOVE VALOR-E1          TO GS-LINDET(65: 12)
           MOVE PATROCINIO-WK     TO VALOR-E1
           ADD PATROCINIO-WK      TO PATROCINIO-G TOT-PATROCINIO
           MOVE VALOR-E1          TO GS-LINDET(77: 12)
           MOVE PAT-FORM-WK       TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(89: 12)
           MOVE PERC-PATROC-WK    TO PERC-E
           MOVE PERC-E            TO GS-LINDET(102: 6)
           MOVE PM-WK             TO GS-LINDET(108: 4)
           MOVE VLR-PAGO-WK       TO VALOR-E1
           ADD VLR-PAGO-WK        TO VLR-PAGO-G TOT-VLR-PAGO
           MOVE VALOR-E1          TO GS-LINDET(112: 12)
           MOVE VLR-PAGAR-WK      TO VALOR-E1
           ADD VLR-PAGAR-WK       TO VLR-PAGAR-G TOT-VLR-PAGAR
           MOVE VALOR-E1          TO GS-LINDET(124: 12)
           ADD 1                  TO QT-CONTRATO-G.

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-CHAVE-ANT SECTION.
           MOVE CIDADE-WK         TO CIDADE-ANT
           MOVE CONTRATO-WK       TO CONTRATO-ANT
           MOVE MESANO-WK         TO MESANO-ANT
           MOVE REPRESENT-WK      TO REPRESENT-ANT
           MOVE PADRAO-WK         TO PADRAO-ANT
           MOVE ASSINATURA-WK     TO ASSINATURA-ANT.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO QT-FOTO-G
                         QT-FORM-G
                         OBJ-ARREC-G
                         VLR-PAGO-G
                         VLR-PAGAR-G
                         PATROCINIO-G
                         QT-CONTRATO-G.

           MOVE ZEROS TO TOT-FORM
                         TOT-OBJ-ARREC
                         TOT-PATROCINIO
                         TOT-VLR-PAGO
                         TOT-VLR-PAGAR.

           MOVE ZEROS  TO CONTRATO-ANT
                          MESANO-ANT
                          ASSINATURA-ANT.

           MOVE SPACES TO CIDADE-ANT
                          REPRESENT-ANT
                          PADRAO-ANT.

       TOTALIZA SECTION.
           MOVE SPACES            TO GS-LINDET
           MOVE TOT-FORM          TO GS-LINDET(25: 8)
           MOVE TOT-OBJ-ARREC     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(65: 12)
           MOVE TOT-PATROCINIO    TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(77: 12)
           COMPUTE PAT-FORM-W = TOT-PATROCINIO / TOT-FORM.
           MOVE PAT-FORM-W        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(89: 12)
           COMPUTE PERC-PATROC-W =  (TOT-PATROCINIO/TOT-OBJ-ARREC) * 100
           MOVE PERC-PATROC-W     TO PERC-E
           MOVE PERC-E            TO GS-LINDET(102: 6)
           MOVE TOT-VLR-PAGO      TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(112: 12)
           MOVE TOT-VLR-PAGAR     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(124: 12)

           MOVE ZEROS TO TOT-FORM TOT-OBJ-ARREC TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.

           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "OEP053" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           INITIALIZE REG-WORK
           PERFORM ORDEM
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA1
           PERFORM TOTALIZA-REL.

       TOTALIZA-REL SECTION.
           MOVE SPACES         TO LINDET-REL.
           MOVE "OBJ.ARREC: "  TO LINDET-REL(1: 11)
           MOVE OBJ-ARREC-G    TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(12: 13)
           MOVE "T.PAGO.: "    TO LINDET-REL(29: 9)
           MOVE VLR-PAGO-G     TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(38: 13)
           MOVE "T.PAGAR: "    TO LINDET-REL(56: 9)
           MOVE VLR-PAGAR-G    TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(65: 13)
           WRITE REG-RELAT FROM LINDET-REL AFTER 2.

           MOVE SPACES         TO LINDET-REL.
           MOVE "PATROCIN.: "  TO LINDET-REL(1: 11)
           MOVE PATROCINIO-G   TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(12: 13)
           MOVE "PT.FORM: "    TO LINDET-REL(29: 9)
           COMPUTE PAT-FORM-G = PATROCINIO-G / QT-FORM-G.
           MOVE PAT-FORM-G     TO VALOR-E
           MOVE VALOR-E        TO LINDET-REL(38: 13)
           WRITE REG-RELAT FROM LINDET-REL.

           MOVE SPACES         TO LINDET-REL.
           MOVE "TOT.FOTOS: "  TO LINDET-REL(1: 11)
           MOVE QT-FOTO-G      TO QTDE-E
           MOVE QTDE-E         TO LINDET-REL(12: 13)
           MOVE "T.FORM.: "    TO LINDET-REL(29: 9)
           MOVE QT-FORM-G      TO QTDE-E
           MOVE QTDE-E         TO LINDET-REL(38: 13)
           MOVE "T.CONTR: "    TO GS-LINTOT(56: 9)
           MOVE QT-CONTRATO-G  TO QTDE-E
           MOVE QTDE-E         TO GS-LINTOT(65: 8)
           WRITE REG-RELAT FROM LINDET-REL.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM VERIFICA-SUBTOTAL1.
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE CURSO-WK          TO LINDET-REL(6: 11)
           MOVE CIDADE-WK         TO LINDET-REL(17: 11)
           MOVE QT-FORM-WK        TO LINDET-REL(28: 5)
           ADD QT-FORM-WK TO TOT-FORM
           MOVE PADRAO-WK         TO LINDET-REL(33: 2)
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(35: 8)
           MOVE REPRESENT-WK      TO LINDET-REL(43: 11)
           MOVE ASSINATURA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(54: 11)
           MOVE OBJ-ARREC-WK      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(65: 12)
           ADD OBJ-ARREC-WK TO TOT-OBJ-ARREC
           MOVE PATROCINIO-WK     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(77: 12)
           ADD PATROCINIO-WK TO TOT-PATROCINIO
           MOVE PAT-FORM-WK       TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(89: 12)
           MOVE PERC-PATROC-WK    TO PERC-E
           MOVE PERC-E            TO LINDET-REL(101: 6)
           MOVE VLR-PAGO-WK       TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(107: 12)
           ADD VLR-PAGO-WK TO TOT-VLR-PAGO
           MOVE VLR-PAGAR-WK      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(120: 12)
           ADD VLR-PAGAR-WK TO TOT-VLR-PAGAR
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 60 PERFORM CABECALHO.

       ZERA-VARIAVEIS-REL SECTION.
           MOVE ZEROS TO TOT-FORM TOT-OBJ-ARREC TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.
           MOVE ZEROS TO CONTRATO-ANT MESANO-ANT ASSINATURA-ANT.
           MOVE SPACES TO CIDADE-ANT REPRESENT-ANT PADRAO-ANT.
       VERIFICA-SUBTOTAL1 SECTION.
           EVALUATE GS-ORDEM
      *      WHEN 1
      *       IF CONTRATO-ANT NOT = ZEROS
      *          IF CONTRATO-ANT NOT = CONTRATO-WK
      *             PERFORM TOTALIZA1
             WHEN 2
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA1
             WHEN 3
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA1
             WHEN 4
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA1
             WHEN 5
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA1
             WHEN 6
              IF ASSINATURA-ANT  NOT = ASSINATURA-WK
                 IF ASSINATURA-ANT NOT = ASSINATURA-WK
                    PERFORM TOTALIZA1
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

       TOTALIZA1 SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE TOT-FORM          TO LINDET-REL(25: 8)
           MOVE TOT-OBJ-ARREC     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(65: 12)
           MOVE TOT-PATROCINIO    TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(77: 12)
           COMPUTE PAT-FORM-W = TOT-PATROCINIO / TOT-FORM.
           MOVE PAT-FORM-W        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(89: 12)
           COMPUTE PERC-PATROC-W =  (TOT-PATROCINIO/TOT-OBJ-ARREC) * 100
           MOVE PERC-PATROC-W     TO PERC-E
           MOVE PERC-E            TO LINDET-REL(101: 6)
           MOVE TOT-VLR-PAGO      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(107: 12)
           MOVE TOT-VLR-PAGAR     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(120: 12)

           MOVE ZEROS TO TOT-FORM TOT-OBJ-ARREC TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           ADD 2 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 OED010 OED020 IED011 CAD010
                 COD005 COD002 CRD020.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
