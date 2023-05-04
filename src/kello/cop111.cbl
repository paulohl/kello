       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP111.
       DATE-WRITTEN. 23/08/1999.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: Relatório 605 - colacao de grau
      *listar por ordem de data de colacao. Caso algum contrato não
      *contenha o evento colaçao, considerar o mesano previsto,
      *acrescentando 00 para o dia.
      * LISTAR APENAS OS CONTRATOS ATIVOS, OU SEJA, STATUS => 50
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX012.
           COPY CAPX004.
           COPY CGPX001.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY COPX060.
           COPY IEPX011.
           COPY COPX001.
           COPY COPX111.
           COPY CAPX010.
           COPY PARX001.
           COPY CPARX001.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS COLACAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS OBJ-VENDA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PATROCINIO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PERC-PATROC-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PADRAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                            MESANO-WK, COLACAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PAT-FORM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REPRESENT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES.
           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS CONTRATO-WK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW012.
       COPY CAPW004.
       COPY CGPW001.
       COPY IEPW011.
       COPY CAPW010.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY COPW001.
       COPY COPW111.
       COPY COPW060.
       COPY PARW001.
       COPY CPARW001.
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
           05  COLACAO-WK          PIC 9(8).
           05  PRECO-WK            PIC 9(7)V99.
      *    se mesano-wk < 199506 preço-wk = 8,00
      *    senão                 preço-wk = 11,00
           05  OBJ-VENDA-WK        PIC 9(7)V99.
      *    OBJ-VENDA-WK = qt-fotos-wk * 0,7 * preco-wk.
           05  VLR-PAGO-WK         PIC 9(7)V99.
           05  VLR-PAGAR-WK        PIC 9(7)V99.
      *    VLR-PAGO-WK E VLR-PAGAR-WK = vlr-comissao + custo-prev-brinde
      *    ao qual o custo-prev-brinde=qtde-form * qtde-por-form * custo
      *    Após acumular, multiplicar pela taxa-acumulada
           05  PATROCINIO-WK       PIC 9(7)V99.
      *    PATROCINIO-WK = vlr-pago-wk + vlr-pagar-wk
           05  PERC-PATROC-WK      PIC 9(2)V99.
      *    PERC-PATROC-WK = (patrocinio-wk / obj-venda-wk) * 100
           05  PAT-FORM-WK         PIC 9(6)V99.
      *    PAT-FORM-WK = patrocinio-wk / qt-form-wk
           05  PM-WK               PIC 9(3).
      *    VLR-GERAL(acumulado de todos os brindes) = custo-brinde *
      *               (dias-prazo-co50 / 30) + vlr-geral.
      *    PM-WK arredondado = vlr-geral / patrocinio-wk

      *    TAXA-ACUMULADA(loop do contador até = pm-wk, sendo que a
      *    taxa-acumulada inicia-se c/ 1 fora do loop)=
      *    taxa-acumulada * ((taxa-w / 100) + 1)
           05  ORG-EVENTO-WK       PIC 9.
      *    ORG-EVENTO-WK = 0(NÃO)  1(SIM)= cobertura-co40 = 1,3,4,7.

       FD  WORK1.
       01  REG-WORK1.
           05  CONTRATO-WK1        PIC 9(4).
           05  COLACAO-WK1         PIC 9(8).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(134).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP111.CPB".
           COPY "COP111.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD111             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CPAR001            PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
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
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  CT-00                 PIC 99       VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC ZZ,Z     BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 99       VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ACHEI                 pic x(01) value spaces.
           05  IND                   PIC 9(02)    VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas


           05  OBJ-VENDA-G           PIC 9(8)V99  VALUE ZEROS.
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
           05  JUROS-PAGO            PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-PAGAR           PIC 9(8)V99  VALUE ZEROS.
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

           05  TOT-FORM              PIC 9(7)     VALUE ZEROS.
           05  TOT-OBJ-VENDA         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-PATROCINIO        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VLR-PAGO          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VLR-PAGAR         PIC 9(8)V99  VALUE ZEROS.

           05  COLACAO-ANT        PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  MESANO-ANT            PIC 9(6)     VALUE ZEROS.
           05  PADRAO-ANT            PIC X        VALUE SPACES.
           05  REPRESENT-ANT         PIC X(10)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(10)    VALUE SPACES.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).


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
           "RELATORIO 605          - ORDEM: ".
           05  ORDEM-REL           PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(20)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(19)
               VALUE "INTERVALO COLACAO: ".
           05  DET-DTINI           PIC 99/99/9999.
           05  FILLER              PIC X(05) VALUE " ATE ".
           05  DET-DTFIM           PIC 99/99/9999.
           05  FILLER              PIC X(05).
           05  FILLER              PIC X(15)
               VALUE "REPRESENTANTE: ".
           05  DET-REPRESENTANTE   PIC X(30).
       01  CAB02B.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(08)
               VALUE "PADRAO: ".
           05  DET-PADRAO          PIC X(05).
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(08)
               VALUE "CIDADE: ".
           05  DET-CIDADE          PIC X(20).
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(08)
               VALUE "REGIAO: ".
           05  DET-REGIAO          PIC X(20).
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(04)
               VALUE "UF: ".
           05  DET-UF              PIC X(02).
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(132)   VALUE
           "CONT IDENTIFIC. CIDADE     FORM P MES/ANO REPRESENT. DT COLA
      -    "CAO   OBJ.VENDA  R$-PATROC.    PAT/FORM %-PAT    VLR-PAGO
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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD111"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD111.
           MOVE "PAR001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "CPAR001" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPAR001.
           OPEN INPUT CGD001 CAD010 IED011 COD040 COD005 COD050 PAR001
                      COD060 COD002 CAD012 COD001 CPAR001
           OPEN I-O   COD111
           CLOSE      COD111
           OPEN INPUT COD111.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1                         TO CHAVE-PAR001
              READ PAR001 INVALID KEY
                   MOVE 0 TO COLACAO-GRAU-PAR001
                   MOVE "Parametrização não Cadastrada" TO
                        GS-MENSAGEM-ERRO
                   PERFORM CARREGA-MENSAGEM-ERRO
              NOT INVALID KEY
                   IF COLACAO-GRAU-PAR001 NOT > 0
                      MOVE 0 TO COLACAO-GRAU-PAR001
                      MOVE SPACES TO GS-MENSAGEM-ERRO
                      STRING "Parametrização não Cadastrada" X"0DA0"
                             "Código da Colação de Grau Inválido"
                        INTO GS-MENSAGEM-ERRO
                      PERFORM CARREGA-MENSAGEM-ERRO
                   END-IF
              END-READ

              MOVE CHAVE-PAR001         TO CHAVE-CPAR001
              READ CPAR001 INVALID KEY
                   INITIALIZE REG-CPAR001
              END-READ
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM VERIFICAR-SENHA-STATUS
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
               WHEN GS-LE-REPRES-TRUE
                    PERFORM LER-REPRESENTANTE
               WHEN GS-POPUP-REPRES-TRUE
                    PERFORM CARREGA-POP-UP-REPRES
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM POPUP-CIDADE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM POPUP-REGIAO
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-COD111
           START COD111 KEY IS NOT LESS CODIGO-COP111 INVALID KEY
               MOVE "10" TO ST-COD111.

           PERFORM UNTIL ST-COD111 = "10"
               READ COD111 NEXT AT END
                    MOVE "10" TO ST-COD111
               NOT AT END
                    MOVE CODIGO-COP111 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              CLOSE      COD111
              OPEN I-O   COD111
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-COP111
                        WRITE REG-COD111

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      COD111
              OPEN INPUT COD111.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB13
           NOT INVALID KEY
               ENABLE-OBJECT PB13.

           CLOSE CAD004.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.


       CENTRALIZAR SECTION.
          move-object-handle win7 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LE-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID NOME-COMPL-CID.
           MOVE NOME-CID TO GS-DESC-CIDADE.

       LE-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.
           PERFORM LE-CIDADE.

       POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-REPRESENTANTE SECTION.
           MOVE GS-REPRESENTANTE       TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01        TO GS-NOME-REPRESENTANTE.
       CARREGA-POP-UP-REPRES SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
               NOT AT END
                MOVE NOME-CG01(1: I)     TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD001
                ELSE
                  MOVE NOME-CG01         TO GS-LINDET2(1: 30)
                  MOVE CODIGO-CG01       TO GS-LINDET2(33: 06)
                  MOVE "INSERE-POP-UP-FORNEC" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET2(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET2(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET2(33: 6) TO GS-REPRESENTANTE
           MOVE GS-LINDET2(1: 30) TO GS-NOME-REPRESENTANTE.

       VERIFICA-INT-DATA SECTION.
           MOVE 0 TO GS-ERRO-DATA.
           MOVE GS-VENCTO-INI  TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-INI
           MOVE GS-VENCTO-FIM  TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV       TO VENCTO-FIM
           IF VENCTO-INI > VENCTO-FIM MOVE 2 TO GS-ERRO-DATA.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           IF ST-WORK1 NOT = "35"
              CLOSE       WORK1
              DELETE FILE WORK1.

           ACCEPT VARIA-W1 FROM TIME.
           ADD 1 TO VARIA-W1.

           OPEN OUTPUT WORK1
           CLOSE       WORK1
           OPEN I-O    WORK1

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE VENCTO-INI  TO DATAREALIZA-CO60
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
                 READ COD060 NEXT RECORD AT END
                      MOVE "10" TO ST-COD060
                 NOT AT END
                      MOVE DATAREALIZA-CO60  TO MESANO-WK
                                                GS-EXIBE-VENCTO
                      MOVE "TELA-AGUARDA1"   TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATAREALIZA-CO60 > VENCTO-FIM
                         MOVE "10" TO ST-COD060
                      ELSE
      *                  IF CODEVENTO-CO60 NOT = 080
      *                     CONTINUE
      *                  ELSE
                            IF CODEVENTO-CO60 = COLACAO-GRAU-PAR001
                               PERFORM OK-COND-EVENTO
                            ELSE
                               PERFORM PROCURAR-TABELA-COLACAO
                               IF ACHEI = "S"
                                  PERFORM OK-COND-EVENTO
                               END-IF
                            END-IF
      *                  END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE VENCTO-INI(1:6) TO MESANO-INI
           MOVE VENCTO-FIM(1:6) TO MESANO-FIM
           MOVE MESANO-INI      TO MESANO-PREV-CO40
           MOVE ZEROS           TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

      *    CASO NAO EXISTA O EVENTO COLACAO DE GRAU PARA O CONTRATO
      *    CONSIDERAR A MESANO PREVISTO COM DIA = 00
           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT RECORD AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      MOVE MESANO-PREV-CO40   TO MESANO-WK
                           GS-EXIBE-VENCTO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF MESANO-PREV-CO40 > MESANO-FIM
                         MOVE "10" TO ST-COD040
                      ELSE
                         IF GS-CIDADE = 0 OR CIDADE-CO40
                            PERFORM VERIFICAR-COLACAO
                            IF ACHEI = "N"
                               MOVE CIDADE-CO40    TO CIDADE
                               READ CAD010 INVALID KEY
                                    INITIALIZE REG-CAD010
                               END-READ
                               IF GS-REGIAO = 0 OR REGIAO-CID
                                  IF GS-UF = SPACES OR UF-CID
                                     PERFORM PESQUISAR-STATUS
                                     IF ACHEI = "S"
                                        MOVE NR-CONTRATO-CO40
                                          TO CONTRATO-WK1
                                        MOVE MESANO-PREV-CO40
                                          TO COLACAO-WK1(1: 6)
                                        MOVE CT-00 TO COLACAO-WK1(7: 2)
                                        READ WORK1 INVALID KEY
                                        WRITE REG-WORK1
                                        END-READ
                                     END-IF
                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE ZEROS TO CONTRATO-WK1
           START WORK1 KEY IS NOT < CONTRATO-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
                 READ WORK1 NEXT RECORD AT END
                      MOVE "10" TO ST-WORK1
                  NOT AT END
                      MOVE CONTRATO-WK1 TO NR-CONTRATO-CO40
                      READ COD040 INVALID KEY CONTINUE
                         NOT INVALID KEY
                             PERFORM MOVER-DADOS-WORK
                      END-READ
                 END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-COLACAO SECTION.
           MOVE "N" TO ACHEI

           INITIALIZE REG-COD060
           MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO60
           START COD060 KEY IS NOT LESS CHAVE-CO60 INVALID KEY
                MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
                READ COD060 NEXT AT END
                     MOVE "10" TO ST-COD060
                NOT AT END
                     IF NR-CONTRATO-CO40 <> NR-CONTRATO-CO60
                        MOVE "10" TO ST-COD060
                     ELSE
                        IF CODEVENTO-CO60 = COLACAO-GRAU-PAR001
                           MOVE "S" TO ACHEI
                           MOVE "10" TO ST-COD060
                        ELSE
                           PERFORM PROCURAR-TABELA-COLACAO
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.

       OK-COND-EVENTO SECTION.
           MOVE NR-CONTRATO-CO60                  TO NR-CONTRATO-CO40
           READ COD040 NOT INVALID KEY
                IF GS-CIDADE = 0 OR CIDADE-CO40
                   PERFORM PESQUISAR-STATUS
                   IF ACHEI = "S"
                      MOVE CIDADE-CO40            TO CIDADE
                      READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                      END-READ
                      IF GS-REGIAO = 0 OR REGIAO-CID
                         IF GS-UF = SPACES OR UF-CID
                            MOVE NR-CONTRATO-CO60 TO CONTRATO-WK1
                            MOVE DATAREALIZA-CO60 TO COLACAO-WK1

                            WRITE REG-WORK1
                         END-IF
                      END-IF
                   END-IF
                END-IF
           END-READ.

       PROCURAR-TABELA-COLACAO SECTION.
           MOVE "N" TO ACHEI
           MOVE 0 TO IND
           PERFORM 30 TIMES
               ADD 1 TO IND

               IF TAB-COLACAO(IND) IS NOT NUMERIC
                  EXIT PERFORM
               ELSE
                  IF TAB-COLACAO(IND) = CODEVENTO-CO60
                     MOVE "S" TO ACHEI
                     EXIT PERFORM
                  END-IF
               END-IF
           END-PERFORM.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       MOVER-DADOS-WORK SECTION.
           IF STATUS-CO40 < 50 OR STATUS-CO40 = 56 CONTINUE
           ELSE
            IF GS-TIPO-PADRAO = 1 AND PADRAO-CO40 <> GS-PADRAO OR
               GS-TIPO-REPRESENTANTE = 1 AND
                  REPRESENTANTE-CO40 <> GS-REPRESENTANTE
               CONTINUE
            ELSE
              MOVE MESANO-PREV-CO40   TO MESANO-WK GS-EXIBE-VENCTO
              MOVE COLACAO-WK1        TO COLACAO-WK
              MOVE NR-CONTRATO-CO40   TO CONTRATO-WK
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID           TO CIDADE-WK
              MOVE REPRESENTANTE-CO40 TO CODIGO-CG01
              READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01          TO REPRESENT-WK
              IF ASSINATURA-CO40 > DATA-DIA-I
                 MOVE VLR-COMISSAO-CO40   TO VLR-PAGAR
                 MOVE ZEROS               TO VLR-PAGO
              ELSE MOVE VLR-COMISSAO-CO40 TO VLR-PAGO
                   MOVE ZEROS             TO VLR-PAGAR
              END-IF

              IF COBERTURA-CO40 = 1 OR 3 OR 4 OR 7
      *          tipo de cobertura que tem org-evento
                 MOVE 1 TO ORG-EVENTO-WK
              ELSE MOVE 0 TO ORG-EVENTO-WK
              END-IF
              MOVE QTDE-FORM-CO40     TO QT-FORM-WK
              MOVE IDENTIFICACAO-CO40 TO CURSO-WK
              MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
              READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
              END-READ
              COMPUTE QT-FOTOS-WK = PREV-FOTOS-CO05 * QT-FORM-WK
              PERFORM CALCULAR-DADOS-BRINDE
              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-WORK
            END-IF
           END-IF.
      * ---------ROTINAS PERFORMADAS(PELA SEÇÃO MOVER-DADOS-WORK)---
       CALCULAR-DADOS-BRINDE SECTION.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA VLR-GERAL JUROS-PAGAR
                         JUROS-PAGO.
           MOVE NR-CONTRATO-CO40   TO NR-CONTRATO-CO50.
           MOVE ZEROS              TO ITEM-CO50.
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
                NOT AT END
                  IF NR-CONTRATO-CO50 <> NR-CONTRATO-CO40
                     MOVE "10" TO ST-COD050
                  ELSE
                     IF SUSP-PREV-DEF-CO50 <> 2
                        MOVE CODBRINDE-CO50 TO CODIGO-CO02
                        READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                        END-READ
                        IF CUSTO-UNIT-CO50 <> ZEROS
                           MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
                        ELSE
                           MOVE VALOR-CO02 TO CUSTO-PREVISTO
                        END-IF
                        IF REALIZADO-CO50 <> 1
                           IF MULT-FORM-CO02 = 2
                              COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                                      QTDE-POR-FORM-CO50
                           ELSE
                              COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                                        QTDE-FORM-CO50) * CUSTO-PREVISTO
                           END-IF
                        ELSE
                           MOVE VALOR-PAGO-CO50 TO CUSTO-W
                        END-IF
                        PERFORM CALCULO-JUROS-BRINDE
                        IF REALIZADO-CO50 = 0
                           ADD CUSTO-W TO VLR-PAGAR
                           ADD JUROS-W TO JUROS-PAGAR
                        ELSE
                           ADD CUSTO-W TO VLR-PAGO
                           ADD JUROS-W TO JUROS-PAGO
                        END-IF
                        COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30
                        COMPUTE VLR-GERAL = (CUSTO-W * MESES-W)
                                                             + VLR-GERAL
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / (VLR-PAGO +
                                         VLR-PAGAR)
           MOVE PRAZO-MEDIO TO PM-WK.
      *    MOVE 1 TO TAXA-ACUMULADA.
      *    COMPUTE TAXA-W = (GS-TAXA / 100) + 1.
      *    PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
      *        COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA * TAXA-W
      *    END-PERFORM.

           PERFORM VERIFICA-PRECO.
      *    COMPUTE VLR-PAGO-WK = TAXA-ACUMULADA * VLR-PAGO
           COMPUTE VLR-PAGO-WK = VLR-PAGO + JUROS-PAGO
      *    COMPUTE VLR-PAGAR-WK = TAXA-ACUMULADA * VLR-PAGAR.
           COMPUTE VLR-PAGAR-WK = VLR-PAGAR + JUROS-PAGAR
           COMPUTE PATROCINIO-WK = VLR-PAGO-WK + VLR-PAGAR-WK.
           COMPUTE OBJ-VENDA-WK = QT-FOTOS-WK * 0,7 * PRECO-WK.
      *    EM CASO DA QTDE-FOTOS = ZEROS
           IF OBJ-VENDA-WK = ZEROS MOVE 1 TO OBJ-VENDA-WK.
           COMPUTE PERC-PATROC-WK = (PATROCINIO-WK / OBJ-VENDA-WK) * 100
           COMPUTE PAT-FORM-WK = PATROCINIO-WK / QT-FORM-WK.
       VERIFICA-PRECO SECTION.
           IF MESANO-WK < 199506 MOVE 8 TO PRECO-WK
           ELSE MOVE 11 TO PRECO-WK.
       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30.
           MOVE MESES-W TO PRAZO-MEDIO
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).

      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM INITIALIZE REG-WORK.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-C-EVENTO-TRUE AND ORG-EVENTO-WK = 1 OR
                   GS-S-EVENTO-TRUE AND ORG-EVENTO-WK = 0
                      PERFORM MOVER-DADOS-LINDET
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           PERFORM TOTALIZA-TELA.
       TOTALIZA-TELA SECTION.
           MOVE SPACES TO GS-LINTOT
           MOVE ZEROS  TO GS-CONT-LINTOT
           MOVE "OBJ.VENDA: "  TO GS-LINTOT(1: 11)
           MOVE OBJ-VENDA-G    TO VALOR-E
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
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "REPRESENT" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < REPRESENT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "COLACAO  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < COLACAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "OBJ-VENDA" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < OBJ-VENDA-WK INVALID KEY
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
              IF COLACAO-ANT  NOT = COLACAO-WK
                 IF COLACAO-ANT NOT = COLACAO-WK
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
           MOVE COLACAO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(54: 11)
      *    MOVE PRECO-WK          TO VALOR-E2
      *    MOVE VALOR-E2          TO GS-LINDET(34: 5)
           MOVE OBJ-VENDA-WK      TO VALOR-E1
           ADD OBJ-VENDA-WK       TO OBJ-VENDA-G TOT-OBJ-VENDA
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
           MOVE CIDADE-WK         TO CIDADE-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT
           MOVE MESANO-WK         TO MESANO-ANT
           MOVE REPRESENT-WK      TO REPRESENT-ANT
           MOVE PADRAO-WK         TO PADRAO-ANT.
           MOVE COLACAO-WK     TO COLACAO-ANT.

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO QT-FOTO-G QT-FORM-G OBJ-VENDA-G VLR-PAGO-G
                         VLR-PAGAR-G PATROCINIO-G QT-CONTRATO-G.
           MOVE ZEROS TO TOT-FORM TOT-OBJ-VENDA TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.
           MOVE ZEROS TO CONTRATO-ANT MESANO-ANT COLACAO-ANT.
           MOVE SPACES TO CIDADE-ANT REPRESENT-ANT PADRAO-ANT.

       TOTALIZA SECTION.
           MOVE SPACES        TO GS-LINDET
           MOVE TOT-FORM          TO GS-LINDET(25: 8)
           MOVE TOT-OBJ-VENDA     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(65: 12)
           MOVE TOT-PATROCINIO    TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(77: 12)
           COMPUTE PAT-FORM-W = TOT-PATROCINIO / TOT-FORM.
           MOVE PAT-FORM-W        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(89: 12)
           COMPUTE PERC-PATROC-W =  (TOT-PATROCINIO/TOT-OBJ-VENDA) * 100
           MOVE PERC-PATROC-W     TO PERC-E
           MOVE PERC-E            TO GS-LINDET(102: 6)
           MOVE TOT-VLR-PAGO      TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(112: 12)
           MOVE TOT-VLR-PAGAR     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(124: 12)

           MOVE ZEROS TO TOT-FORM TOT-OBJ-VENDA TOT-PATROCINIO
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
           MOVE "COP111" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM INITIALIZE REG-WORK.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-C-EVENTO-TRUE AND ORG-EVENTO-WK = 1 OR
                   GS-S-EVENTO-TRUE AND ORG-EVENTO-WK = 0
                      PERFORM MOVER-DADOS-RELATORIO
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA1.
           PERFORM TOTALIZA-REL.
       TOTALIZA-REL SECTION.
           MOVE SPACES         TO LINDET-REL.
           MOVE "OBJ.VENDA: "  TO LINDET-REL(1: 11)
           MOVE OBJ-VENDA-G    TO VALOR-E
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
           MOVE COLACAO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(54: 11)
           MOVE OBJ-VENDA-WK      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(65: 12)
           ADD OBJ-VENDA-WK TO TOT-OBJ-VENDA
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
           IF LIN > 56 PERFORM CABECALHO.

       ZERA-VARIAVEIS-REL SECTION.
           MOVE ZEROS TO TOT-FORM TOT-OBJ-VENDA TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.
           MOVE ZEROS TO CONTRATO-ANT MESANO-ANT COLACAO-ANT.
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
              IF COLACAO-ANT  NOT = COLACAO-WK
                 IF COLACAO-ANT NOT = COLACAO-WK
                    PERFORM TOTALIZA1
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.

       TOTALIZA1 SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE TOT-FORM          TO LINDET-REL(25: 8)
           MOVE TOT-OBJ-VENDA     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(65: 12)
           MOVE TOT-PATROCINIO    TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(77: 12)
           COMPUTE PAT-FORM-W = TOT-PATROCINIO / TOT-FORM.
           MOVE PAT-FORM-W        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(89: 12)
           COMPUTE PERC-PATROC-W =  (TOT-PATROCINIO/TOT-OBJ-VENDA) * 100
           MOVE PERC-PATROC-W     TO PERC-E
           MOVE PERC-E            TO LINDET-REL(101: 6)
           MOVE TOT-VLR-PAGO      TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(107: 12)
           MOVE TOT-VLR-PAGAR     TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(120: 12)

           MOVE ZEROS TO TOT-FORM TOT-OBJ-VENDA TOT-PATROCINIO
                         TOT-VLR-PAGO TOT-VLR-PAGAR.

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT
           ADD 2 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-VENCTO-INI      TO DET-DTINI
           MOVE GS-VENCTO-FIM      TO DET-DTFIM
           IF GS-TIPO-REPRESENTANTE = 1
              MOVE GS-NOME-REPRESENTANTE TO DET-REPRESENTANTE
           ELSE
              MOVE "GERAL"               TO DET-REPRESENTANTE
           END-IF

           IF GS-TIPO-PADRAO = 1
              MOVE GS-PADRAO       TO DET-PADRAO
           ELSE
              MOVE "GERAL"         TO DET-PADRAO
           END-IF

           MOVE GS-DESC-CIDADE     TO DET-CIDADE
           MOVE GS-DESC-REGIAO     TO DET-REGIAO
           MOVE GS-UF              TO DET-UF

           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB02A
           WRITE REG-RELAT FROM CAB02B
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 8 TO LIN.

       GRAVA-STATUS SECTION.
           CLOSE    COD111
           OPEN I-O COD111

           INITIALIZE REG-COD111
           START COD111 KEY IS NOT LESS CODIGO-COP111 INVALID KEY
                MOVE "10" TO ST-COD111.
           PERFORM UNTIL ST-COD111 = "10"
                READ COD111 NEXT AT END
                     MOVE "10" TO ST-COD111
                NOT AT END
                     DELETE COD111 INVALID KEY
                         MOVE "Erro de Exclusão...COD111" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-COP111
               WRITE REG-COD111
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      COD111
           OPEN INPUT COD111.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD050 COD060 IED011 CAD010  CGD001
                 COD005 COD002 CAD012 PAR001 CPAR001 COD111


           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
