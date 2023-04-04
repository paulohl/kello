       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP040.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 28/07/1999.
      *FUNÇÃO: Cadastro de Contratos
      * A alteração em cima de uma data-prev-venda, altera
      * os dias de prazo de pagto de algum brinde(dias-prazo-co50)
      * A exclusão não é permitida neste programa, porque muitos arqui-
      * vos estão relacionados com o cod040. Ex. brinde, evento, contato


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX001.
           COPY COPX001.
           COPY COPX004.
           COPY COPX005.
           COPY COPX040.
           COPY COPX049.
           COPY COPX050.
           COPY IEPX010.
           COPY LBPX027.
           COPY CAPX010.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW001.
       COPY COPW001.
       COPY COPW004.
       COPY COPW005.
       COPY COPW040.
       COPY COPW049.
       COPY COPW050.
       COPY IEPW010.
       COPY LBPW027.
       COPY CAPW010.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP040.CPB".
           COPY "COP040.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD004             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD049             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
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

           05  LINDET-W              PIC X(80)    VALUE SPACES.
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
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  INICIAL               PIC 9(05)    VALUE 1.
           05  QTD-FINAL             PIC 9(05)    VALUE 30.
           05  CONT                  PIC 9(05)    VALUE ZEROS.
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
           "CONFERENCIA DO CADASTRO DE CONTRATO".
           05  FILLER              PIC X(37)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "CONT OR INSTITUICAO     IDENTIFICACAO        TU FORM.INI PAD
      -    ".INI MES/ANO FORM P CA CIDADE        REPRESENTANTE    VLR.CO
      -    "MISSAO".


       01  CAB05.
           05  FILLER                   PIC X(132)  VALUE
           "DTA.ASSIN. STATUS          TF MULTA-CONTRATUAL     CIDADE-FO
      -    "RUM  COMERCIALIACAO                 OBS                  RES
      -    "PONSAVEL".

       01  CAB06.
           05  FILLER                   PIC X(132)  VALUE
           "DATA-VENDA ORGANIZADOR     FONE-ORG CONTATO-BECA
      -    "       CONTATO-CONVITE                OUTRO-CONTATO
      -    "         CO".


       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.

       01  WS-DATA-SYS                 PIC X(021).
       01  FILLER REDEFINES WS-DATA-SYS.
           03 WS-DATA-CPU              PIC 9(008).
           03 FILLER REDEFINES WS-DATA-CPU.
              05 WS-ANO-CPU            PIC 9(004).
              05 WS-MES-CPU            PIC 9(002).
              05 WS-DIA-CPU            PIC 9(002).
           03 FILLER                   PIC X(013).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).


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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD004.
           MOVE "COD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD049" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD049.
           MOVE "COD050" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "LBD027" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD027.
           OPEN I-O   COD040 COD050 COD049
           CLOSE      COD040 COD050 COD049
           OPEN INPUT COD040 COD050 COD049

           OPEN INPUT IED010 CGD001 COD001 COD004 LBD027 CAD010 CAD004
                      COD005
           IF ST-COD040 = "35"
              CLOSE COD040      OPEN OUTPUT COD040
              CLOSE COD040      OPEN I-O COD040
           END-IF.
           IF ST-COD049 = "35"
              CLOSE COD049      OPEN OUTPUT COD049
              CLOSE COD049      OPEN I-O COD049
           END-IF.
           IF ST-COD050 = "35"
              CLOSE COD050      OPEN OUTPUT COD050
              CLOSE COD050      OPEN I-O COD050
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD004 <> "00"
              MOVE "ERRO ABERTURA COD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD049 <> "00"
              MOVE "ERRO ABERTURA COD049: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD049 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM PREENCHER-SELECTION-BOX
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    IF GS-TIPO-GRAVACAO = 1
                       PERFORM REGRAVA-DADOS
                    ELSE
                       PERFORM GRAVA-DADOS
                    END-IF
                    CLOSE    COD049
                    OPEN I-O COD049
                    MOVE GS-CONTRATO TO NR-CONTRATO-CO49
                    READ COD049 INVALID KEY
                         MOVE GS-CONTRATO  TO NR-CONTRATO-CO49
                         MOVE GS-ENCERRADO TO CANCELADO-CO49
                         MOVE GS-USUARIO-CADASTRO TO
                                              USUARIO-CADASTRO-CO49
                         MOVE GS-DATA-CADASTRO TO DATA-CADASTRO-CO49
                         MOVE GS-HORA-CADASTRO TO HORA-CADASTRO-CO49
                         MOVE GS-PREPOSTO      TO PREPOSTO-CO49
                         WRITE REG-COD049
                         END-WRITE
                    NOT INVALID KEY
                         MOVE GS-CONTRATO  TO NR-CONTRATO-CO49
                         MOVE GS-ENCERRADO TO CANCELADO-CO49
                         IF USUARIO-CADASTRO-CO49 = SPACES
                            MOVE GS-USUARIO-CADASTRO TO
                                 USUARIO-CADASTRO-CO49
                            MOVE ZEROS TO DATA-CADASTRO-CO49
                                          HORA-CADASTRO-CO49
                         END-IF
                         MOVE GS-PREPOSTO      TO PREPOSTO-CO49
                         REWRITE REG-COD049
                         END-REWRITE
                    END-READ
                    CLOSE      COD049
                    OPEN INPUT COD049
                    MOVE GS-CONTRATO TO NR-CONTRATO-W
                    PERFORM LIMPAR-DADOS
                    IF GS-TIPO-GRAVACAO <> 1
                        MOVE NR-CONTRATO-W TO GS-CONTRATO
                        ADD 1 TO GS-CONTRATO
                    END-IF
                    MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
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
               WHEN GS-LE-INSTITUICAO-TRUE
                    PERFORM LE-INSTITUICAO
               WHEN GS-LE-REPRES-TRUE
                    PERFORM LE-REPRES
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-LE-PREPOSTO-TRUE
                    PERFORM LE-PREPOSTO
               WHEN GS-LE-TIPO-FOTOG-TRUE
                    PERFORM LE-TIPO-FOTOG
               WHEN GS-LE-CAMPANHA-TRUE
                    PERFORM LE-CAMPANHA
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CHAMAR-BRINDE-TRUE
                    PERFORM CHAMAR-BRINDE
               WHEN GS-CHAMAR-EVENTO-TRUE
                    PERFORM CHAMAR-EVENTO
               WHEN GS-CHAMAR-COMISSAO-TRUE
                    PERFORM CHAMAR-COMISSAO
               WHEN GS-CHAMAR-TURMA-TRUE
                    PERFORM CHAMAR-TURMA
               WHEN GS-ACHAR-SEQ-CONT-TRUE
                    PERFORM ACHAR-SEQ-CONTRATO
               WHEN GS-VALIDA-ACESSO-TRUE
                    PERFORM VALIDA-ACESSO
               WHEN GS-CHAMAR-PRODUTOS-TRUE
                    PERFORM CHAMAR-PRODUTO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA58"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF8
           NOT INVALID KEY
                ENABLE-OBJECT EF8.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       PREENCHER-SELECTION-BOX SECTION.
           MOVE "LIMPAR-SB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-COD005
                      GS-POSICAO
           START COD005 KEY IS NOT LESS PADRAO-CO05 INVALID KEY
                 MOVE "10" TO ST-COD005.

           PERFORM UNTIL ST-COD005 = "10"
                 READ COD005 NEXT AT END
                      MOVE "10" TO ST-COD005
                 NOT AT END
                      MOVE PADRAO-CO05  TO GS-PADRAO
                      MOVE "INSERIR-SB" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.

       LE-PREPOSTO SECTION.
           MOVE GS-PREPOSTO        TO CODIGO-CG01
           READ CGD001 INVALID KEY
               INITIALIZE REG-CGD001.

           MOVE NOME-CG01          TO GS-NOME-PREPOSTO.


       VALIDA-ACESSO SECTION.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA04"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-BOTAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA14"          TO PROGRAMA-CA004.
           READ CAD004 NOT INVALID KEY
               MOVE "HABILITA-BRINDE" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE "SENHA13"          TO PROGRAMA-CA004.
           READ CAD004 NOT INVALID KEY
               MOVE "HABILITA-EVENTO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE "SENHA23"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITA-EXCLUSAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITA-EXCLUSAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

           MOVE "SENHA30"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               MOVE "DESABILITAR-ENCERRADO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITA-ENCERRADO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.


       ACHAR-SEQ-CONTRATO SECTION.
      *Comentado no dia 01-08-2011
      *    MOVE ALL "8" TO NR-CONTRATO-CO40
      *    START COD040 KEY IS LESS THAN NR-CONTRATO-CO40
      *    READ COD040 NEXT
      *    IF ST-COD040 = "9d" OR "00"
      *       MOVE NR-CONTRATO-CO40 TO GS-CONTRATO
      *    ELSE
      *       MOVE 0 TO GS-CONTRATO
      *    END-IF
      *Comentado no dia 01-08-2011

      *Comentado no dia 20-06-2014
      *    MOVE 0 TO NR-CONTRATO-CO40
      *    PERFORM UNTIL ST-COD040 = "10"
      *        ADD 1 TO NR-CONTRATO-CO40
      *        READ COD040 INVALID KEY
      *             MOVE "10" TO ST-COD040
      *        END-READ
      *    END-PERFORM
      *
      *    MOVE NR-CONTRATO-CO40   TO GS-CONTRATO.
      *Fim Comentario no dia 20-06-2014

      *Rotina nova implementada dia 20-06-2014
           initialize reg-cod040
           MOVE ALL "8" TO NR-CONTRATO-CO40
           START COD040 KEY IS LESS THAN NR-CONTRATO-CO40
           READ COD040 NEXT
           IF ST-COD040 = "9d" OR "00"
              MOVE NR-CONTRATO-CO40 TO GS-CONTRATO
           ELSE
              MOVE 0 TO GS-CONTRATO
           END-IF

           ADD 1 TO GS-CONTRATO.

      *Fim Rotina nova implementada dia 20-06-2014

       CHAMAR-BRINDE SECTION.
           MOVE GS-CONTRATO    TO PASSAR-STRING-1(1: 4)
           MOVE 0000           TO PASSAR-STRING-1(5: 4)
           MOVE IMPRESSORA-W   TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W  TO PASSAR-STRING-1(11: 3)
           CALL   "COP050X" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP050X".
       CHAMAR-EVENTO SECTION.
           MOVE GS-CONTRATO    TO PASSAR-STRING-1(1: 4)
           MOVE 000            TO PASSAR-STRING-1(5: 3)
           MOVE IMPRESSORA-W   TO PASSAR-STRING-1(9: 2)
           MOVE COD-USUARIO-W  TO PASSAR-STRING-1(11:3).
           CALL   "COP060X" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP060X".
       CHAMAR-COMISSAO SECTION.
           CALL   "CGP010X" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010X".
       CHAMAR-TURMA SECTION.
           MOVE PARAMETROS-W TO PASSAR-PARAMETROS
           CALL   "COP041X" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP041X".
       CHAMAR-PRODUTO SECTION.
           MOVE SPACES         TO PASSAR-STRING-1
           MOVE GS-CONTRATO    TO PASSAR-STRING-1(1: 4)
           CALL   "COP045" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP045".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 MOVE GS-LINDET3 TO LINDET-W
                    PERFORM CARREGA-POP-UP-INSTITUICAO
             WHEN 2 MOVE GS-LINDET4 TO LINDET-W
                    PERFORM CARREGA-POP-UP-REPRES
             WHEN 3 CALL   "COP001T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "COP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-STATUS
                    MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
             WHEN 4 MOVE GS-LINDET1 TO LINDET-W
                    PERFORM CARREGA-POP-UP-CIDADE
             WHEN 5 CALL   "COP004T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "COP004T"
                    MOVE PASSAR-STRING-1(33: 2) TO GS-CAMPANHA
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CAMPANHA
             WHEN 6 CALL   "LBP027T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "LBP027T"
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-FOTOGRAFIA
                    MOVE PASSAR-STRING-1(1: 20) TO
                         GS-NOME-TIPO-FOTOGRAFIA
             WHEN 7 CALL   "CGP001T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(33: 6) TO GS-PREPOSTO
                    MOVE PASSAR-STRING-1(1:30)  TO GS-NOME-PREPOSTO
           END-EVALUATE.
       CARREGA-POP-UP-INSTITUICAO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO CIDADE-IE10.
           MOVE SPACES            TO NOME-IE10.
           START IED010 KEY IS NOT < ALT-IE10 INVALID KEY
                 MOVE "10" TO ST-IED010.
           PERFORM UNTIL ST-IED010 = "10"
              READ IED010 NEXT RECORD AT END MOVE "10" TO ST-IED010
               NOT AT END
                MOVE CIDADE-IE10(1: I)  TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-IED010
                ELSE
                  MOVE CIDADE-IE10     TO GS-LINDET3(1: 21)
                  MOVE NOME-IE10       TO GS-LINDET3(22: 41)
                  MOVE CODIGO-IE10     TO GS-LINDET3(63: 05)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
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
                  MOVE NOME-CG01         TO GS-LINDET4(1: 30)
                  MOVE CODIGO-CG01       TO GS-LINDET4(33: 06)
                  MOVE "INSERE-POP-UP-FORNEC" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-CIDADE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-CID.
           START CAD010 KEY IS NOT < NOME-CID INVALID KEY
                 MOVE "10" TO ST-CAD010.
           PERFORM UNTIL ST-CAD010 = "10"
              READ CAD010 NEXT RECORD AT END MOVE "10" TO ST-CAD010
               NOT AT END
                MOVE NOME-CID(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CAD010
                ELSE
                  MOVE NOME-CID        TO GS-LINDET1(1: 30)
                  MOVE CIDADE          TO GS-LINDET1(33: 04)
                  MOVE "INSERE-POP-UP-CIDADE" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE LINDET-W(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE LINDET-W(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET3(63: 5) TO CODIGO-W
               IF CODIGO-W = SPACES MOVE "00000" TO GS-LINDET1(63: 5)
               END-IF
               MOVE GS-LINDET3(63: 5) TO GS-INSTITUICAO
               MOVE GS-LINDET3(22: 30) TO GS-NOME-INSTITUICAO
             WHEN 2
               MOVE GS-LINDET4(33: 6) TO GS-REPRESENTANTE
               MOVE GS-LINDET4(1: 30) TO GS-NOME-REPRESENTANTE
             WHEN 4
               MOVE GS-LINDET1(33: 4)  TO GS-CODIGO-CAD010
               MOVE GS-LINDET1(1: 30)  TO GS-NOME-CAD010
           END-EVALUATE.
       LE-REPRES SECTION.
           MOVE GS-REPRESENTANTE   TO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-REPRESENTANTE.
       LE-STATUS SECTION.
           MOVE GS-STATUS          TO CODIGO-CO01.
           READ COD001 INVALID KEY MOVE "********" TO STATUS-CO01.
           MOVE STATUS-CO01        TO GS-NOME-STATUS.
       LE-INSTITUICAO SECTION.
           MOVE GS-INSTITUICAO     TO CODIGO-IE10.
           READ IED010 INVALID KEY MOVE "******" TO NOME-IE10.
           MOVE NOME-IE10          TO GS-NOME-INSTITUICAO.
       LE-TIPO-FOTOG SECTION.
           MOVE GS-TIPO-FOTOGRAFIA TO CODIGO-LB27.
           READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27.
           MOVE DESCRICAO-LB27     TO GS-NOME-TIPO-FOTOGRAFIA.
       LE-CIDADE SECTION.
           MOVE GS-CODIGO-CAD010   TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-NOME-CAD010.
       LE-CAMPANHA SECTION.
           MOVE GS-CAMPANHA        TO CODIGO-CO04.
           READ COD004 INVALID KEY MOVE ZEROS TO DATA-INI-CO04
                                                 DATA-FIM-CO04.
           MOVE DATA-INI-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-NOME-CAMPANHA(1: 11)
           MOVE "a"                TO GS-NOME-CAMPANHA(12: 2)
           MOVE DATA-FIM-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-NOME-CAMPANHA(14: 10).
      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           MOVE ZEROS TO GS-TIPO-GRAVACAO.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO40.
           START COD040 KEY IS = NR-CONTRATO-CO40 INVALID KEY CONTINUE.
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
                MOVE USUARIO-W TO GS-USUARIO-CADASTRO
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
                GS-DATA-CADASTRO
                ACCEPT WS-HORA-SYS FROM TIME
                STRING WS-HO-SYS WS-MI-SYS INTO GS-HORA-CADASTRO
           NOT INVALID KEY
             MOVE GS-CONTRATO TO NR-CONTRATO-CO49
             READ COD049 INVALID KEY
                  MOVE ZEROS        TO GS-ENCERRADO
                  MOVE SPACES       TO GS-USUARIO-CADASTRO
                  MOVE ZEROS        TO GS-DATA-CADASTRO
                  MOVE ZEROS        TO GS-HORA-CADASTRO
                  MOVE ZEROS        TO GS-PREPOSTO
                  MOVE SPACES       TO GS-NOME-PREPOSTO
             NOT INVALID KEY
                  MOVE CANCELADO-CO49 TO GS-ENCERRADO
                  MOVE USUARIO-CADASTRO-CO49 TO GS-USUARIO-CADASTRO
                  MOVE DATA-CADASTRO-CO49    TO GS-DATA-CADASTRO
                  MOVE HORA-CADASTRO-CO49    TO GS-HORA-CADASTRO
                  MOVE PREPOSTO-CO49         TO GS-PREPOSTO
                                                CODIGO-CG01
                  READ CGD001 INVALID KEY
                       INITIALIZE REG-CGD001
                  END-READ
                  MOVE NOME-CG01             TO GS-NOME-PREPOSTO
             END-READ

             MOVE 1 TO GS-TIPO-GRAVACAO
             EVALUATE ORIGEM-CO40
               WHEN 1 MOVE "1-KEL" TO GS-ORIGEM
               WHEN 2 MOVE "2-MIK" TO GS-ORIGEM
             END-EVALUATE
             MOVE INSTITUICAO-CO40         TO GS-INSTITUICAO CODIGO-IE10
             READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
             END-READ
             MOVE NOME-IE10                TO GS-NOME-INSTITUICAO
             MOVE IDENTIFICACAO-CO40       TO GS-IDENTIFICACAO
             MOVE "FORM: "                 TO GS-FORM-TURMA(1: 6)
             MOVE QTDE-FORM-CO40           TO GS-FORM-TURMA(7: 10)
             MOVE "TURMAS: "               TO GS-FORM-TURMA(19: 8)
             MOVE QTDE-TURMAS-CO40         TO GS-FORM-TURMA(27: 2)

             MOVE "FORM-INI: "             TO GS-PAD-FORM-INI(1: 10)
             MOVE QTDE-FORM-INI-CO40       TO GS-PAD-FORM-INI(11: 8)
             MOVE "PAD-INI: "              TO GS-PAD-FORM-INI(19: 9)
             MOVE PADRAO-INI-CO40          TO GS-PAD-FORM-INI(28: 1)

             MOVE PADRAO-CO40              TO GS-PADRAO
             MOVE MESANO-PREV-CO40         TO MESANO-I
             MOVE MESANO-I(5: 2)           TO MESANO-W(1: 2)
             MOVE MESANO-I(1: 4)           TO MESANO-W(3: 4)
             MOVE MESANO-W                 TO GS-MESANO-PREV
             MOVE CAMPANHA-CO40            TO GS-CAMPANHA CODIGO-CO04
             READ COD004 INVALID KEY MOVE ZEROS TO DATA-INI-CO04
                                                   DATA-FIM-CO04
             END-READ
             MOVE DATA-INI-CO04 TO DATA-INV
             CALL "GRIDAT1" USING DATA-INV
             MOVE DATA-INV                 TO DATA-E
             MOVE DATA-E                   TO GS-NOME-CAMPANHA(1: 11)
             MOVE "a"                      TO GS-NOME-CAMPANHA(12: 3)
             MOVE DATA-FIM-CO04            TO DATA-INV
             CALL "GRIDAT1" USING DATA-INV
             MOVE DATA-INV                 TO DATA-E
             MOVE DATA-E                   TO GS-NOME-CAMPANHA(15: 10)
             MOVE CIDADE-CO40              TO GS-CIDADE CIDADE
             READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
             END-READ
             MOVE NOME-CID                 TO GS-NOME-CIDADE
             MOVE REPRESENTANTE-CO40       TO GS-REPRESENTANTE
                                              CODIGO-CG01
             READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
             END-READ
             MOVE NOME-CG01                TO GS-NOME-REPRESENTANTE
             MOVE VLR-COMISSAO-CO40        TO GS-COMISSAO-REPRESENTANTE
             MOVE ASSINATURA-CO40          TO DATA-INV
             CALL "GRIDAT1" USING DATA-INV
             MOVE DATA-INV                 TO GS-DATA-ASSINATURA
             MOVE STATUS-CO40              TO GS-STATUS CODIGO-CO01
             READ COD001 INVALID KEY MOVE SPACES TO STATUS-CO01
             END-READ
             MOVE STATUS-CO01              TO GS-NOME-STATUS
             MOVE TIPO-FOTOG-CO40          TO GS-TIPO-FOTOGRAFIA
                                              CODIGO-LB27
             READ LBD027 INVALID KEY MOVE SPACES TO DESCRICAO-LB27
             END-READ
             MOVE DESCRICAO-LB27           TO GS-NOME-TIPO-FOTOGRAFIA
             MOVE MULTA-CONTRAT-CO40       TO GS-MULTA-RESCISAO
             MOVE CIDADE-FORUM-CO40        TO GS-CIDADE-FORUM CIDADE
             READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
             END-READ
             MOVE NOME-CID                 TO GS-NOME-CIDADE-FORUM
             MOVE COMERCIALIZACAO-CO40     TO GS-COMERCIALIZACAO
             MOVE OBS-IMPORTANTE-CO40      TO GS-OBS-IMPORTANTE
             MOVE RESPONSAVEL-ATEND-CO40   TO GS-RESPONSAVEL-ATEND
             MOVE DATA-PREV-VENDA-CO40     TO GS-DATA-PREV-VENDA
                                              DATA-PREV-VENDA-INI
             MOVE ORGANIZADOR-CO40         TO GS-ORGANIZADOR
             MOVE FONE-ORGANIZ-CO40        TO GS-FONE-ORGANIZADOR
             MOVE CONTATO-CONVITE-CO40     TO GS-CONTATO-CONVITE
             MOVE CONTATO-BECA-CO40        TO GS-CONTATO-BECA
             MOVE CONTATO-OUTRO-CO40       TO GS-CONTATO-OUTRO
             EVALUATE COBERTURA-CO40
               WHEN 01  MOVE "1 - F/V/O" TO GS-COBERTURA
               WHEN 02  MOVE "2 - F/V  " TO GS-COBERTURA
               WHEN 03  MOVE "3 - F/O  " TO GS-COBERTURA
               WHEN 04  MOVE "4 - V/O  " TO GS-COBERTURA
               WHEN 05  MOVE "5 - F    " TO GS-COBERTURA
               WHEN 06  MOVE "6 - V    " TO GS-COBERTURA
               WHEN 07  MOVE "7 - O    " TO GS-COBERTURA
             END-EVALUATE
           END-READ.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD040
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           IF GS-TIPO-GRAVACAO = 1
              MOVE GS-CONTRATO                 TO NR-CONTRATO-CO40
              READ COD040
              END-READ
           END-IF
           MOVE GS-CONTRATO                 TO NR-CONTRATO-CO40
           MOVE 1                           TO ORIGEM-CO40
           MOVE GS-INSTITUICAO              TO INSTITUICAO-CO40
           MOVE GS-IDENTIFICACAO            TO IDENTIFICACAO-CO40
           MOVE GS-PADRAO                   TO PADRAO-CO40
           MOVE GS-MESANO-PREV              TO MESANO-W
           MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
           MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I                    TO MESANO-PREV-CO40
           MOVE GS-CAMPANHA                 TO CAMPANHA-CO40
           MOVE GS-CIDADE                   TO CIDADE-CO40
           MOVE GS-REPRESENTANTE            TO REPRESENTANTE-CO40
           MOVE GS-COMISSAO-REPRESENTANTE   TO VLR-COMISSAO-CO40
           MOVE GS-DATA-ASSINATURA          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                    TO ASSINATURA-CO40
           MOVE GS-STATUS                   TO STATUS-CO40
           MOVE GS-TIPO-FOTOGRAFIA          TO TIPO-FOTOG-CO40
           MOVE GS-MULTA-RESCISAO           TO MULTA-CONTRAT-CO40
           MOVE GS-CIDADE-FORUM             TO CIDADE-FORUM-CO40
           MOVE GS-COMERCIALIZACAO          TO COMERCIALIZACAO-CO40
           MOVE GS-OBS-IMPORTANTE           TO OBS-IMPORTANTE-CO40
           MOVE GS-RESPONSAVEL-ATEND        TO RESPONSAVEL-ATEND-CO40
           MOVE GS-DATA-PREV-VENDA          TO DATA-PREV-VENDA-CO40
           MOVE GS-ORGANIZADOR              TO ORGANIZADOR-CO40
           MOVE GS-FONE-ORGANIZADOR         TO FONE-ORGANIZ-CO40
           MOVE GS-CONTATO-CONVITE          TO CONTATO-CONVITE-CO40
           MOVE GS-CONTATO-BECA             TO CONTATO-BECA-CO40
           MOVE GS-CONTATO-OUTRO            TO CONTATO-OUTRO-CO40
           MOVE GS-COBERTURA(1: 1)          TO COBERTURA-CO40.

       GRAVA-DADOS SECTION.
           CLOSE      COD040
           OPEN I-O   COD040
      *    qtde-turmas qtde-form-ini qtde-form serao atualizados pelo
      *    cadastro de turmas/cursos
           MOVE ZEROS TO QTDE-TURMAS-CO40 QTDE-FORM-INI-CO40
                         QTDE-FORM-CO40
           MOVE "S"   TO FLAG-GRAVA-CO40
           MOVE GS-PADRAO                   TO PADRAO-INI-CO40.
           WRITE REG-COD040 INVALID KEY
               MOVE "Erro Gravação COD040" TO GS-MENSAGEM-ERRO
               MOVE ST-COD040 TO GS-MENSAGEM-ERRO(24: 5)
               MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.
           CLOSE      COD040
           OPEN INPUT COD040.
       REGRAVA-DADOS SECTION.
           CLOSE      COD040
           OPEN I-O   COD040
           REWRITE REG-COD040 INVALID KEY
                 MOVE "Erro Regravação COD040" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD040 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
           CLOSE      COD040
           OPEN INPUT COD040
           IF GS-DATA-PREV-VENDA <> DATA-PREV-VENDA-INI
              PERFORM ALTERA-DIAS-PRAZO-COD050.
       EXCLUI SECTION.
           CLOSE      COD040
           OPEN I-O   COD040
      *    A EXCLUSÃ0 ESTÁ DESATIVADA, PORQUE ESTE ARQUIVO ESTÁ LIGADO
      *    COM VÁRIOS OUTROS ARQUIVOS. EX. brindes, eventos, contatos...

           DELETE COD040.
           CLOSE      COD040
           OPEN INPUT COD040
           PERFORM LIMPAR-DADOS.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       ALTERA-DIAS-PRAZO-COD050 SECTION.
           CLOSE      COD050
           OPEN I-O   COD050
           MOVE GS-CONTRATO TO NR-CONTRATO-CO50.
           MOVE ZEROS       TO ITEM-CO50.
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
             READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
               NOT AT END
                 IF NR-CONTRATO-CO50 <> GS-CONTRATO
                    MOVE "10" TO ST-COD050
                 ELSE
                    PERFORM CALCULA-PRAZO-MEDIO
                    REWRITE REG-COD050
                    END-REWRITE
                 END-IF
             END-READ
           END-PERFORM
           CLOSE      COD050
           OPEN INPUT COD050.
       CALCULA-PRAZO-MEDIO SECTION.
           IF DATA-PAGTO-CO50 = ZEROS
              MOVE DATA-VENCTO-CO50      TO GRTIME-DATE
           ELSE
              MOVE DATA-PAGTO-CO50       TO GRTIME-DATE.
      *       MOVE DATA-PAGTO-CO50       TO DATA-INV
      *       CALL "GRIDAT2" USING DATA-INV
      *       MOVE DATA-INV              TO GRTIME-DATE.

           MOVE DATA-PREV-VENDA-CO40     TO GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                 TO GRTIME-DATE-FINAL
           MOVE 2                     TO GRTIME-TYPE
           MOVE 3                     TO GRTIME-FUNCTION
           CALL "GRTIME" USING PARAMETROS-GRTIME
           MOVE GRTIME-DAYS-FINAL     TO DIAS-PRAZO-CO50.


       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP040" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO REG-COD040.

           MOVE SPACES TO LINDET.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
           MOVE NR-CONTRATO-CO40     TO LINDET-REL(1: 5)
           MOVE ORIGEM-CO40          TO LINDET-REL(6: 3)
           MOVE INSTITUICAO-CO40     TO CODIGO-IE10
           READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10.
           MOVE NOME-IE10            TO LINDET-REL(9: 15)
           MOVE IDENTIFICACAO-CO40   TO LINDET-REL(25: 21)
           MOVE QTDE-TURMAS-CO40     TO LINDET-REL(46: 03)
           MOVE QTDE-FORM-INI-CO40   TO LINDET-REL(49: 9)
           MOVE PADRAO-INI-CO40      TO LINDET-REL(58: 8)
           MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
           MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W             TO MESANO-E
           MOVE MESANO-E             TO LINDET-REL(66: 8)
           MOVE QTDE-FORM-CO40       TO LINDET-REL(74: 5)
           MOVE PADRAO-CO40          TO LINDET-REL(79: 2)
           MOVE CAMPANHA-CO40        TO LINDET-REL(81: 3)
           MOVE CIDADE-CO40          TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID             TO LINDET-REL(84: 14)
           MOVE REPRESENTANTE-CO40   TO CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01            TO LINDET-REL(98: 15)
           MOVE VLR-COMISSAO-CO40    TO VALOR-E
           MOVE VALOR-E              TO LINDET-REL(114: 13)
           WRITE REG-RELAT FROM LINDET.

           MOVE SPACES TO LINDET.
           MOVE ASSINATURA-CO40      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO LINDET-REL(1: 11)
           MOVE STATUS-CO40          TO CODIGO-CO01.
           READ COD001 INVALID KEY MOVE SPACES TO STATUS-CO01.
           MOVE STATUS-CO01          TO LINDET-REL(12: 15)
           MOVE TIPO-FOTOG-CO40      TO LINDET-REL(28: 03)
           MOVE MULTA-CONTRAT-CO40   TO LINDET-REL(31: 21)
           MOVE CIDADE-FORUM-CO40    TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID             TO LINDET-REL(52: 14)
           MOVE COMERCIALIZACAO-CO40 TO LINDET-REL(66: 31)
           MOVE OBS-IMPORTANTE-CO40  TO LINDET-REL(97: 21)
           MOVE RESPONSAVEL-ATEND-CO40 TO LINDET-REL(118: 10).
           WRITE REG-RELAT FROM LINDET.

           MOVE SPACES TO LINDET-REL.
           MOVE DATA-PREV-VENDA-CO40 TO DATA-E
           MOVE DATA-E               TO LINDET-REL(1: 11)
           MOVE ORGANIZADOR-CO40     TO LINDET-REL(12: 16)
           MOVE FONE-ORGANIZ-CO40    TO LINDET-REL(28: 9)
           MOVE CONTATO-BECA-CO40    TO LINDET-REL(37: 31)
           MOVE CONTATO-CONVITE-CO40 TO LINDET-REL(68: 31)
           MOVE CONTATO-OUTRO-CO40   TO LINDET-REL(99: 31)
           MOVE COBERTURA-CO40       TO LINDET-REL(130: 2)
           WRITE REG-RELAT FROM LINDET.

           COPY DESCONDENSA.


       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB03.
           MOVE 8 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD001 COD040 COD004 IED010 CAD010 CGD001 LBD027
                 COD050 CAD004 COD049 COD005
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
