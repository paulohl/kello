       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCP110.
      *DATA: 16/11/2003
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE RECTO DE CHEQUE E DUPLICATAS EM COBRANCA-KAC
      *        Listar todos os cheques e títulos do cliente
      *        dentro do intervalo de vencimento solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX010.
           COPY CAPX012.
           COPY CAPX018.
           COPY COPX040.
           COPY CHPX010.
           COPY CHPX010B.
           COPY CHPX013.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CGPX001.
           COPY CGPX010.
           COPY CBPX001.
           COPY CGPX011.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK, SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALBUM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-RECTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ORIGEM-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW012.
       COPY CAPW018.
       COPY COPW040.
       COPY CHPW010.
       COPY CHPW010B.
       COPY CHPW013.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CGPW001.
       COPY CGPW010.
       COPY CBPW001.
       COPY CGPW011.
       FD  WORK.
       01  REG-WORK.
           05  DATA-MOVTO-WK       PIC 9(08).
           05  SEQ-WK              PIC 9(04).
           05  DOCUMENTO-WK        PIC X(10).
           05  ALBUM-WK            PIC 9(08).
           05  FORMA-PGTO-WK       PIC X(10).
           05  VENCTO-WK           PIC 9(8).
           05  DATA-RECTO-WK       PIC 9(8).
           05  VLR-NOM-WK          PIC 9(8)V99.
           05  VLR-JUROS-WK        PIC 9(6)V99.
           05  VLR-DESC-WK         PIC 9(6)V99.
           05  VLR-TOT-WK          PIC 9(8)V99.
           05  ORIGEM-WK           PIC X(2).
           05  CIDADE-WK           PIC X(20).
           05  DCR-MEM-R-WK2       PIC X(15).
           05  CLIENTE-WK          PIC X(25).


       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(146).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CDCP110.CPB".
           COPY "CDCP110.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD010B            PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  RECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  RECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  RECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  RECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ.ZZZ,ZZ
                                                  BLANK WHEN ZEROS.

           05  VALOR-E1              PIC ZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E2              PIC Z.ZZ9,99 VALUE ZERO.
           05  QTDE-E                PIC Z.ZZZ    BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(4)     VALUE ZEROS.
           05  TOT-VALOR-NOM         PIC 9(9)V99  VALUE ZEROS.
           05  TOT-VALOR-JUR         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-DES         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR             PIC 9(8)V99  VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  AUX-CIDADE            PIC X(20)    VALUE SPACES.
           05  AUX-REGIAO            PIC X(20)    VALUE SPACES.
           05  AUX-VENCTO            PIC 9(8)     VALUE ZEROS.
           05  AUX-DATA-RECTO        PIC 9(8)     VALUE ZEROS.
           05  SUBTOT                PIC 9(12)V99 VALUE ZEROS.
           05  QTD-SUB               PIC 9(06)    VALUE ZEROS.
           05  JUROS                 PIC 9(12)V99 VALUE ZEROS.
           05  DESC                  PIC 9(12)V99 VALUE ZEROS.
           05  TOTAL                 PIC 9(12)V99 VALUE ZEROS.
           05  AUX-ORIGEM            PIC X(02)    VALUE SPACES.
           05  AUX-DOCUMENTO         PIC 9(08)    VALUE ZEROS.
           05  AUX-PORTADOR          PIC 9(04)    VALUE ZEROS.
           05  AUX-VENDEDOR          PIC X(30)    VALUE SPACES.
           05  AUX-FORMA-PAG         PIC X(10)    VALUE SPACES.
           05  AUX-CLIENTE.
               10 AUX-CONTRATO       PIC 9(04).
               10 AUX-ITEM           PIC 9(04).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       01 MASC-QTDE                PIC ZZZ.ZZ9.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RECEBTOS COBRANCA-CH/DUPL    -ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(05)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.RECTO.: ".
           05  RCTO-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  RCTO-FIM-REL        PIC 99/99/9999.
           05  FILLER              PIC X(12)   VALUE "TIPO DOCTO: ".
           05  DOCTO-REL           PIC X(12).
       01  CAB02B.
           05  FILLER              PIC X(16)
               VALUE "FORMA DE PAGTO: ".
           05  FORMA-PGTO-REL      PIC X(20).
           05  FILLER              PIC X(16)
               VALUE "CONTA CORRENTE: ".
           05  CONTA-REL           PIC 9(06).
           05  FILLER              PIC X(01).
           05  NOME-CONTA-REL      PIC X(30).
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(11)
               VALUE "RECEBEDOR: ".
           05  RECEBEDOR-REL       PIC X(10).

       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "ALBUM     FORMA-PGTO CLIENTE            DT-VENCTO  DATA-RECT
      -    "O     VALOR        JR+M     DESC     TOTAL  OR CIDADE".

       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE-TITULO     VLR-TOT-NOM         VLT-TOT-JUROS+M
      -    " VLR-DESCONTO     VLR-TOTAL".
       01  LINTOT.
           05  LINTOT-REL          PIC X(146)  VALUE SPACES.

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
           MOVE DATA-INV TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CAD010
           MOVE "CAD012"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CAD012
           MOVE "CAD018"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CAD018
           MOVE "COD040"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-COD040
           MOVE "CHD010"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD010B" TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CHD010B
           MOVE "CHD013"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CHD013
           MOVE "CRD020"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD020B" TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CRD020B
           MOVE "CGD001"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CGD001
           MOVE "CGD010"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CGD010
           MOVE "CGD011"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CGD011
           MOVE "CBD001"  TO ARQ-REC  MOVE EMPRESA-REF TO PATH-CBD001
           ACCEPT VARIA-W FROM TIME

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK CHD010B
           CLOSE            CHD010B

           OPEN INPUT CAD010 CAD012 CRD020 CHD010 CHD013  CAD004 CGD010
                      CGD011 CAD018 COD040 CGD001 CRD020B CHD010B
                      CBD001
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CHD010B <> "00"
              MOVE "ERRO ABERTURA CHD010B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

      *    MOVE 1 TO COD-USUARIO-W.
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
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       MOVE GS-ACP-CONTA         TO CONTA-REL
                       MOVE GS-DESC-CONTA        TO NOME-CONTA-REL
                       MOVE GS-FORMA-PAG         TO FORMA-PGTO-REL
                       MOVE GS-RECEBEDOR         TO RECEBEDOR-REL

                       IF GS-OPCAO = 1
                          PERFORM IMPRIME-RELATORIO
                       ELSE
                          PERFORM IMPRIME-RELATORIO2
                       END-IF
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    IF GS-OPCAO = 1
                       PERFORM CARREGA-LISTA
                    ELSE
                       PERFORM CARREGAR-LISTA2
                    END-IF
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    IF GS-OPCAO = 1
                       PERFORM CARREGA-LISTA
                    ELSE
                       PERFORM CARREGAR-LISTA2
                    END-IF
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM VERIFICA-ACESSO-DEVOL
               WHEN GS-CHAMA-ALTERACAO-TRUE
                    PERFORM CHAMA-ALTERACAO
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LER-VENDEDOR
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LER-CONTRATO
               WHEN GS-POP-UP-CIDADE-TRUE
                    PERFORM POP-UP-CIDADE
               WHEN GS-POP-UP-VENDEDOR-TRUE
                    PERFORM POP-UP-VENDEDOR
               WHEN GS-POP-UP-PORTADOR-TRUE
                    PERFORM POP-UP-PORTADOR
               WHEN GS-POP-UP-CLIENTE-TRUE
                    PERFORM POP-UP-CLIENTE
               WHEN GS-LE-CLIENTE-TRUE
                    PERFORM LE-CLIENTE
               WHEN GS-LE-CONTA-TRUE
                    PERFORM LER-CONTA
               WHEN GS-POPUP-CONTA-TRUE
                    PERFORM POPUP-CONTA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       LER-CONTA SECTION.
           IF GS-ACP-CONTA > 0
              MOVE GS-ACP-CONTA TO CODIGO-FORN-CB01
              READ CBD001 INVALID KEY
                   MOVE "Conta corrente inválida" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE TITULAR-CB01 TO GS-DESC-CONTA
                   REFRESH-OBJECT PRINCIPAL
              END-READ
           ELSE
              MOVE SPACES TO GS-DESC-CONTA
              REFRESH-OBJECT PRINCIPAL.

       POPUP-CONTA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CBP001T"
           MOVE PASSAR-STRING-1(49:6) TO GS-ACP-CONTA
           PERFORM LER-CONTA.

       POP-UP-CLIENTE SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(33: 8) TO COD-COMPL-CG10(2: 8)
           MOVE PASSAR-STRING-1(42: 1) TO COD-COMPL-CG10(1: 1)
           READ CGD010 NOT INVALID KEY
                EVALUATE CLASSIF-CG10
                   WHEN 0 MOVE  "0-Contrato"   TO GS-CLASSIFICACAO
                   WHEN 1 MOVE  "1-Comum"      TO GS-CLASSIFICACAO
                   WHEN 9 MOVE  "9-Unificado"  TO GS-CLASSIFICACAO
                END-EVALUATE
                MOVE CODIGO-CG10(1:4)          TO GS-CONTRATO
                MOVE CODIGO-CG10(5:4)          TO GS-ALBUM
                MOVE COMPRADOR-CG10            TO GS-DESC-CONTRATO
                REFRESH-OBJECT PRINCIPAL.

       LE-CLIENTE SECTION.
           IF GS-ALBUM > 0
              MOVE GS-CLASSIFICACAO(1:1)    TO CLASSIF-CG10
              STRING GS-CONTRATO GS-ALBUM INTO CODIGO-CG10
              READ CGD010 INVALID KEY
                   MOVE "Cliente Não Encontrado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE COMPRADOR-CG10 TO GS-DESC-CONTRATO
                   REFRESH-OBJECT PRINCIPAL.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CIDADE SECTION.
           MOVE GS-CIDADE TO CIDADE
           READ CAD010 INVALID KEY
               MOVE "---" TO NOME-CID
           END-READ
           MOVE NOME-CID TO GS-DESC-CIDADE
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-VENDEDOR SECTION.
           MOVE GS-VENDEDOR TO CODIGO-CG01
           READ CGD001 INVALID KEY
               MOVE "---" TO NOME-CG01
           END-READ
           MOVE NOME-CG01 TO GS-DESC-VENDEDOR
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR
           READ CAD018 INVALID KEY
               MOVE "---" TO NOME-PORT
           END-READ
           MOVE NOME-PORT TO GS-DESC-PORTADOR
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-CONTRATO SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
               MOVE "---" TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       POP-UP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP010T"
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE
           MOVE GS-CIDADE TO CIDADE
           READ CAD010 INVALID KEY
               MOVE "---" TO NOME-CID
           END-READ
           MOVE NOME-CID TO GS-DESC-CIDADE.

       POP-UP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-VENDEDOR.

       POP-UP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP018T"
           MOVE PASSAR-STRING-1(33:4)  TO GS-PORTADOR
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-PORTADOR.

       POP-UP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP040T"
           MOVE PASSAR-STRING-1(52:4)  TO GS-CONTRATO
           MOVE GS-CONTRATO TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
               MOVE "---" TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       VERIFICA-ACESSO-DEVOL SECTION.
           MOVE ZEROS         TO GS-AUTORIZADO
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           MOVE "SENHA01"     TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
                MOVE 0 TO GS-AUTORIZADO
           NOT INVALID KEY
                MOVE 1 TO GS-AUTORIZADO
           END-READ
           IF ORIGEM-WK <> "CH"
              MOVE 0   TO GS-TIPO-COBRANCA
           ELSE
              MOVE 1   TO GS-TIPO-COBRANCA
           END-IF.

       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES
              MOVE ZEROS TO GS-LINDET.

           MOVE GS-LINDET(150: 8) TO PASSAR-STRING-1(1: 8)
                                     DATA-MOVTO-CH10
                                     DATA-MOVTO-WK
           MOVE GS-LINDET(158: 4) TO PASSAR-STRING-1(10: 4)
                                     SEQ-CH10
                                     SEQ-WK.
           MOVE USUARIO-W         TO PASSAR-STRING-1(20: 5)
           MOVE COD-USUARIO-W     TO PASSAR-STRING-1(26: 3)

           CALL   "CHP013T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CHP013T".


       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-RECTO-INI TO DATA-INV
                                RCTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO RECTO-INI
           MOVE GS-RECTO-FIM TO DATA-INV
                                RECTO-FIM-ANT
                                RCTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO RECTO-FIM

           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO

           MOVE GS-TIPO-DOCTO        TO   DOCTO-REL

           MOVE GS-FORMA-PAG         TO   AUX-FORMA-PAG

           MOVE ZEROS  TO SEQ-WK
                          TOT-VALOR-DES
                          TOT-VALOR-NOM
                          TOT-VALOR-JUR
                          AUX-DOCUMENTO
                          AUX-PORTADOR
                          AUX-ORIGEM
                          AUX-VENCTO
                          SUBTOT
                          QTD-SUB
                          AUX-CLIENTE
                          TOT-VALOR

           MOVE SPACES TO AUX-VENDEDOR
                          AUX-CIDADE
                          AUX-REGIAO

           IF AUX-TIPO = 9 OR 5
              PERFORM GRAVA-CHEQUE.

           IF AUX-TIPO <> 5
              PERFORM GRAVA-A-RECEBER.

           CLOSE      WORK
           OPEN INPUT WORK.

       GRAVA-CHEQUE SECTION.
           INITIALIZE REG-CHD010B
           MOVE RECTO-INI         TO DATA-RCTO-CH10B
           START CHD010B KEY IS NOT LESS ALT6-CH10B INVALID KEY
                MOVE "10" TO ST-CHD010B.

           PERFORM UNTIL ST-CHD010B = "10"
                READ CHD010B NEXT AT END
                     MOVE "10" TO ST-CHD010B
                NOT AT END
                     IF DATA-RCTO-CH10B > RECTO-FIM
                        MOVE "10" TO ST-CHD010B
                     ELSE
                        IF GS-ACP-CONTA = 0 OR CODIGO-BANCARIO-CH10B
                           IF GS-RECEBEDOR = SPACES OR
                              RECEBEDOR-CH10B
                              MOVE DATA-RCTO-CH10B   TO GS-EXIBE-VECTO
                              MOVE "TELA-AGUARDA1"   TO DS-PROCEDURE
                              PERFORM CALL-DIALOG-SYSTEM
                              MOVE DATA-MOVTO-CH10B  TO DATA-MOVTO-CH10
                              MOVE SEQ-CH10B         TO SEQ-CH10
                              IF AUX-FORMA-PAG = SPACES OR
                                 AUX-FORMA-PAG = FORMA-PAGTO-CH10B
                                 READ CHD010 NOT INVALID KEY
                                      PERFORM MOVER-DADOS-WORK-CH
                                 END-READ
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

      *    MOVE RECTO-INI         TO DATA-RECTO-CH13.
      *    START CHD013 KEY IS NOT < DATA-RECTO-CH13 INVALID KEY
      *          MOVE "10" TO ST-CHD013.
      *    PERFORM UNTIL ST-CHD013 = "10"
      *          READ CHD013 NEXT RECORD AT END
      *               MOVE "10" TO ST-CHD013
      *          NOT AT END
      *               IF DATA-RECTO-CH13 > RECTO-FIM
      *                  MOVE "10" TO ST-CHD013
      *               ELSE
      *                  MOVE DATA-RECTO-CH13     TO GS-EXIBE-VECTO
      *                  MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
      *                  PERFORM CALL-DIALOG-SYSTEM
      *                  MOVE DATA-MOVTO-CH13     TO DATA-MOVTO-CH10
      *                  MOVE SEQ-CH13            TO SEQ-CH10
      *                  IF AUX-FORMA-PAG = SPACES OR
      *                     AUX-FORMA-PAG = FORMA-PAGTO-CH13
      *                     READ CHD010 NOT INVALID KEY
      *                          PERFORM MOVER-DADOS-WORK-CH
      *                     END-READ
      *                  END-IF
      *               END-IF
      *          END-READ
      *    END-PERFORM

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-WORK-CH SECTION.
           MOVE COD-COMPL-CH10     TO COD-COMPL-CG10
           MOVE CLIENTE-CH10       TO AUX-CLIENTE
           READ CGD010 INVALID KEY
                MOVE "********"    TO COMPRADOR-CG10
           END-READ
           IF GS-VENDEDOR = 0 OR VENDEDOR-CH10
              IF GS-PORTADOR = 0 OR PORTADOR-CH10
                    IF GS-DESC-CIDADE = "---" OR SPACES OR CIDADE-CH10
                       IF GS-CONTRATO = 0 OR AUX-CONTRATO
                          IF GS-ALBUM = 0 OR AUX-ITEM
                             MOVE CIDADE-CH10 TO CIDADE
                             READ CAD010 INVALID KEY
                                  MOVE SPACES TO UF-CID
                             END-READ
                             IF GS-ACP-UF = SPACES OR UF-CID
                                MOVE SEQ-CH10B        TO SEQ-WK
                                MOVE DATA-MOVTO-CH10B TO DATA-MOVTO-WK

                                READ WORK INVALID KEY
                                  MOVE DCR-MEM-CH10B  TO DCR-MEM-R-WK2
                                  MOVE COMPRADOR-CG10 TO CLIENTE-WK
                                  MOVE FORMA-PAGTO-CH10B
                                    TO FORMA-PGTO-WK

                                  COMPUTE VLR-JUROS-WK =
                                          JURO-RCTO-CH10B +
                                          MULTA-RCTO-CH10B

                                  ADD JURO-RCTO-CH10B  TO TOT-VALOR-JUR
                                  ADD MULTA-RCTO-CH10B TO TOT-VALOR-JUR

                                  MOVE DESCONTO-CH10B  TO VLR-DESC-WK

                                  ADD DESCONTO-CH10B   TO TOT-VALOR-DES
                                  MOVE DATA-RCTO-CH10B TO DATA-RECTO-WK

                                  MOVE "CH"            TO ORIGEM-WK

                                  MOVE COD-COMPL-CH10  TO COD-COMPL-CG11
                                  READ CGD011 INVALID KEY
                                       MOVE ZEROS      TO CIDADE1-CG11
                                  END-READ

                                  MOVE CIDADE1-CG11    TO CIDADE
                                  READ CAD010 INVALID KEY
                                       MOVE SPACES     TO NOME-CID
                                       MOVE ZEROS      TO REGIAO-CID
                                  END-READ

                                  MOVE NOME-CID        TO CIDADE-WK
                                  MOVE REGIAO-CID      TO CODIGO-REG

                                  READ CAD012 INVALID KEY
                                       MOVE SPACES TO NOME-REG
                                  END-READ

                                  MOVE DATA-MOVTO-CH10B
                                    TO DATA-MOVTO-CH10
                                  MOVE SEQ-CH10B       TO SEQ-CH10
                                  READ CHD010 INVALID KEY
                                       MOVE ZEROS      TO VENCTO-WK
                                                          VLR-NOM-WK
                                  NOT INVALID KEY
                                       MOVE DATA-VENCTO-CH10
                                                       TO VENCTO-WK
                                       COMPUTE VALOR-LIQ-CH10B =
                                               VALOR-LIQ-CH10B -
                                              (JURO-RCTO-CH10B +
                                               MULTA-RCTO-CH10B) +
                                               DESCONTO-CH10B
                                       MOVE VALOR-LIQ-CH10B
                                         TO VLR-NOM-WK
                                       ADD VALOR-LIQ-CH10B
                                         TO TOT-VALOR-NOM
                                       MOVE CLIENTE-CH10    TO ALBUM-WK
                                  END-READ

                                  COMPUTE VLR-TOT-WK = VLR-JUROS-WK -
                                                       VLR-DESC-WK  +
                                                       VLR-NOM-WK

                                  ADD VLR-TOT-WK            TO TOT-VALOR


      *                           MOVE VALOR-BAIXA-CR20B   TO VLR-TOT-WK
      *                           ADD VLR-TOT-WK           TO TOT-VALOR


                                  WRITE REG-WORK
                                  END-WRITE
                             NOT INVALID KEY
                                  COMPUTE VLR-JUROS-WK =
                                          VLR-JUROS-WK +
                                         (JURO-RCTO-CH10B +
                                          MULTA-RCTO-CH10B)

                                  ADD JURO-RCTO-CH10B  TO TOT-VALOR-JUR
                                  ADD MULTA-RCTO-CH10B TO TOT-VALOR-JUR

                                  ADD DESCONTO-CH10B   TO VLR-DESC-WK

                                  ADD DESCONTO-CH10B   TO TOT-VALOR-DES
                                  MOVE DATA-RCTO-CH10B TO DATA-RECTO-WK

                                  MOVE DATA-MOVTO-CH10B
                                    TO DATA-MOVTO-CH10
                                  MOVE SEQ-CH10B       TO SEQ-CH10
                                  READ CHD010 INVALID KEY
                                       MOVE ZEROS      TO VENCTO-WK
                                                          VLR-NOM-WK
                                  NOT INVALID KEY
                                       MOVE DATA-VENCTO-CH10
                                                       TO VENCTO-WK
                                       COMPUTE VALOR-LIQ-CH10B =
                                               VALOR-LIQ-CH10B -
                                              (JURO-RCTO-CH10B +
                                               MULTA-RCTO-CH10B) +
                                               DESCONTO-CH10B
                                       ADD  VALOR-LIQ-CH10B
                                         TO VLR-NOM-WK
                                       ADD VALOR-LIQ-CH10B
                                         TO TOT-VALOR-NOM
                                       MOVE CLIENTE-CH10    TO ALBUM-WK
                                  END-READ

                                  COMPUTE TOT-VALOR = TOT-VALOR -
                                                      VLR-TOT-WK

                                  COMPUTE VLR-TOT-WK =
                                    VLR-JUROS-WK - VLR-DESC-WK  +
                                    VLR-NOM-WK

                                  ADD VLR-TOT-WK            TO TOT-VALOR
                                  REWRITE REG-WORK.

       GRAVA-A-RECEBER SECTION.
           INITIALIZE REG-CRD020B
           MOVE RECTO-INI      TO DATA-RCTO-CR20B
           MOVE ZEROS          TO SEQ-CAIXA-CR20B
           START CRD020B KEY IS NOT < ALT6-CR20B INVALID KEY
                  MOVE "10" TO ST-CRD020B.
           PERFORM UNTIL ST-CRD020B = "10"
                  READ CRD020B NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020B
                  NOT AT END
                       IF DATA-RCTO-CR20B > RECTO-FIM
                          MOVE "10" TO ST-CRD020B
                       ELSE
                          IF GS-ACP-CONTA = 0 OR CONTA-CORRENTE-CR20B
                             IF GS-RECEBEDOR = SPACES OR RECEBEDOR-CR20B
                                MOVE DATA-RCTO-CR20B TO GS-EXIBE-VECTO
                                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM

                                MOVE CLASS-CLIENTE-CR20B TO
                                     CLASS-CLIENTE-CR20
                                MOVE CLIENTE-CR20B       TO CLIENTE-CR20
                                MOVE SEQ-CR20B           TO SEQ-CR20
                                READ CRD020 NOT INVALID KEY
                                     IF AUX-FORMA-PAG = SPACES OR
                                        AUX-FORMA-PAG = FORMA-PAGTO-CR20
                                        IF AUX-TIPO = TIPO-DOCTO-CR20 or
                                           AUX-TIPO = "9"
                                           PERFORM MOVER-DADOS-WORK-REC
                                        END-IF
                                     END-IF
                                END-READ
                             END-IF
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-WORK-REC SECTION.
           MOVE CLIENTE-CR20        TO AUX-CLIENTE

           MOVE COD-COMPL-CR20      TO COD-COMPL-CG11
           READ CGD011 INVALID KEY
                MOVE ZEROS          TO CIDADE1-CG11
           END-READ
           MOVE CIDADE1-CG11        TO CIDADE

           MOVE COD-COMPL-CR20      TO COD-COMPL-CG10
           READ CGD010 INVALID KEY
                MOVE "********"     TO COMPRADOR-CG10
           END-READ

           IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
              IF GS-PORTADOR = 00 OR PORTADOR-CR20
                 IF GS-DESC-CIDADE = "---" OR SPACES OR CIDADE
                    IF GS-CONTRATO = 0 OR AUX-CONTRATO
                       IF GS-ALBUM = 0 OR AUX-ITEM
                          MOVE CIDADE1-CG11 TO CIDADE
                          READ CAD010 INVALID KEY
                               MOVE SPACES TO UF-CID
                          END-READ
                          IF GS-ACP-UF = SPACES OR UF-CID
                             ADD  1                   TO SEQ-WK
                             MOVE SPACES              TO FORMA-PGTO-WK
                             MOVE SPACES              TO DCR-MEM-R-WK2
                             MOVE COMPRADOR-CG10      TO CLIENTE-WK
                             MOVE JURO-RCTO-CR20B     TO VLR-JUROS-WK
                             ADD JURO-RCTO-CR20B      TO TOT-VALOR-JUR
                             ADD MULTA-RCTO-CR20B     TO TOT-VALOR-JUR
                             MOVE DESCONTO-CR20B      TO VLR-DESC-WK
                             ADD DESCONTO-CR20B       TO TOT-VALOR-DES
                             MOVE DCR-MEM-CR20B       TO DCR-MEM-R-WK2
                             MOVE FORMA-PAGTO-CR20B   TO FORMA-PGTO-WK
                             MOVE DATA-RCTO-CR20B     TO DATA-RECTO-WK
                             MOVE "CR"                TO ORIGEM-WK
                             MOVE CLIENTE-CR20        TO ALBUM-WK

                             MOVE COD-COMPL-CR20      TO COD-COMPL-CG11
                             READ CGD011 INVALID KEY
                                  MOVE ZEROS          TO CIDADE1-CG11
                             END-READ
                             MOVE CIDADE1-CG11        TO CIDADE
                             READ CAD010 INVALID KEY
                                  MOVE SPACES         TO NOME-CID
                                  MOVE ZEROS          TO REGIAO-CID
                             END-READ
                             MOVE NOME-CID            TO CIDADE-WK

                             MOVE DATA-VENCTO-CR20    TO VENCTO-WK

                             COMPUTE VLR-TOT-WK = VALOR-BAIXA-CR20B +
                                                  JURO-RCTO-CR20B   +
                                                  MULTA-RCTO-CR20B  -
                                                  DESCONTO-CR20B
                             ADD VLR-TOT-WK           TO TOT-VALOR
      *                      MOVE VALOR-LIQ-CR20      TO VLR-TOT-WK
      *                      ADD VALOR-LIQ-CR20B      TO TOT-VALOR

                             MOVE VALOR-BAIXA-CR20B   TO VLR-NOM-WK
                             ADD  VALOR-BAIXA-CR20B   TO TOT-VALOR-NOM

      *                      MOVE VALOR-LIQ-CR20B     TO VLR-NOM-WK
      *                      ADD VALOR-LIQ-CR20B      TO TOT-VALOR-NOM

      *                      MOVE VALOR-BAIXA-CR20B   TO VLR-TOT-WK
      *                      ADD VLR-TOT-WK           TO TOT-VALOR
                             WRITE REG-WORK.


       CARREGA-LISTA SECTION.
           MOVE ZEROS            TO TOT-TITULO
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET
           PERFORM ORDEM
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-WORK
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-LINTOT.

       MOVER-DADOS-LINDET SECTION.
           MOVE ALBUM-WK          TO GS-LINDET(01: 09)
      *    MOVE DOCUMENTO-WK      TO GS-LINDET(01: 11)
           MOVE FORMA-PGTO-WK     TO GS-LINDET(11: 11)
           MOVE CLIENTE-WK        TO GS-LINDET(22:17)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(41: 11)
           MOVE DATA-RECTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(52: 11)

           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(63: 09)

           MOVE VLR-JUROS-WK      TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(74: 09)
           MOVE VLR-DESC-WK       TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(82: 09)

           MOVE VLR-TOT-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(92: 09)

           MOVE ORIGEM-WK         TO GS-LINDET(102: 2)

           MOVE CIDADE-WK         TO GS-LINDET(105:20)

           MOVE DATA-MOVTO-WK     TO GS-LINDET(150: 8)
           MOVE SEQ-WK            TO GS-LINDET(158: 4)
           ADD 1                  TO TOT-TITULO.
       MOVER-DADOS-LINTOT SECTION.
           MOVE TOT-TITULO        TO QTDE-E
           MOVE QTDE-E            TO GS-LINTOT(01: 13)

           MOVE TOT-VALOR-NOM     TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(14: 15)

           MOVE TOT-VALOR-JUR     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(43: 14)
           MOVE TOT-VALOR-DES     TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(64: 14)

           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(74: 14).
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK

             WHEN 2
                MOVE "ALBUM   " TO GS-DESCR-ORDEM
                MOVE ZEROS      TO ALBUM-WK
                START WORK KEY IS NOT < ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "DT.RECTO" TO GS-DESCR-ORDEM
                MOVE ZEROS      TO DATA-RECTO-WK
                START WORK KEY IS NOT < DATA-RECTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE    " TO GS-DESCR-ORDEM
                MOVE SPACES     TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "ORIGEM    " TO GS-DESCR-ORDEM
                MOVE SPACES       TO ORIGEM-WK
                START WORK KEY IS NOT < ORIGEM-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CDCP110" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-LINTOT-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE ALBUM-WK          TO LINDET-REL(01: 09)
      *    MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE FORMA-PGTO-WK     TO LINDET-REL(11: 11)
           MOVE CLIENTE-WK        TO LINDET-REL(22: 17)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(41: 11)
           MOVE DATA-RECTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(52: 11)
           MOVE VLR-NOM-WK        TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(64: 09)
           MOVE VLR-JUROS-WK      TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(76: 09)
           MOVE VLR-DESC-WK       TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(85: 09)
           MOVE VLR-TOT-WK        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(94: 09)
           MOVE ORIGEM-WK         TO LINDET-REL(105:02)
           MOVE CIDADE-WK         TO LINDET-REL(108:20)
           MOVE SEQ-WK            TO GS-LINDET(133: 4)
           MOVE DATA-MOVTO-WK     TO GS-LINDET(138: 8).

           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       MOVER-DADOS-LINTOT-REL SECTION.
           MOVE TOT-TITULO        TO QTDE-E
           MOVE QTDE-E            TO LINTOT-REL(01: 13)
           MOVE TOT-VALOR-NOM     TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(14: 15)
           MOVE TOT-VALOR-JUR     TO VALOR-E1
           MOVE VALOR-E1          TO LINTOT-REL(43: 14)
           MOVE TOT-VALOR-DES     TO VALOR-E1
           MOVE VALOR-E1          TO LINTOT-REL(64: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(74: 14).
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           add 2 to lin
           IF LIN > 56 PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINTOT-REL.
           add 1 to lin
           IF LIN > 56 PERFORM CABECALHO.


       IMPRIME-RELATORIO2 SECTION.
           MOVE ZEROS TO SUBTOT
                         QTD-SUB
                         JUROS
                         DESC
                         TOTAL

           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-RELATORIO2
               END-READ
           END-PERFORM.
           IF SUBTOT > 0
              PERFORM IMPRIMIR-SUBTOTAL2
              MOVE SPACES TO LINDET-REL
              WRITE REG-RELAT FROM LINDET-REL
              ADD 1 TO LIN
              IF LIN > 56
                 PERFORM CABECALHO.

           PERFORM MOVER-DADOS-LINTOT-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO2 SECTION.
           EVALUATE GS-ORDEM
             WHEN 1 IF AUX-VENCTO <> VENCTO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE VENCTO-WK TO AUX-VENCTO
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 2 IF AUX-ALBUM <> ALBUM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE ALBUM-WK TO AUX-ALBUM
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 3 IF AUX-DATA-RECTO <> DATA-RECTO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE DATA-RECTO-WK TO AUX-DATA-RECTO
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 4 IF AUX-CIDADE <> CIDADE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 5 IF AUX-ORIGEM <> ORIGEM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE ORIGEM-WK TO AUX-ORIGEM
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
           END-EVALUATE
           MOVE SPACES            TO LINDET-REL
           MOVE ALBUM-WK          TO LINDET-REL(01: 09)
      *    MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE FORMA-PGTO-WK     TO LINDET-REL(11: 11)
           MOVE CLIENTE-WK        TO LINDET-REL(22: 17)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(41: 11)
           MOVE DATA-RECTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(52: 11)
           MOVE VLR-NOM-WK        TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(64: 09)
           MOVE VLR-JUROS-WK      TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(76: 09)
           MOVE VLR-DESC-WK       TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(85: 09)
           MOVE VLR-TOT-WK        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(94: 09)
           MOVE ORIGEM-WK         TO LINDET-REL(105: 02)
           MOVE CIDADE-WK         TO LINDET-REL(108:20)
           MOVE SEQ-WK            TO GS-LINDET(133: 4)
           MOVE DATA-MOVTO-WK     TO GS-LINDET(138: 8).

           ADD  1                 TO QTD-SUB

           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       IMPRIMIR-SUBTOTAL2 SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE "SUB-TOTAL . . . " TO LINDET-REL
           MOVE QTD-SUB            TO MASC-QTDE
           MOVE MASC-QTDE          TO LINDET-REL(20:7)
           MOVE SUBTOT             TO VALOR-E1
           MOVE VALOR-E1           TO LINDET-REL(63:09)
           MOVE JUROS              TO VALOR-E2
           MOVE VALOR-E2           TO LINDET-REL(76:9)
           MOVE DESC               TO VALOR-E2
           MOVE VALOR-E2           TO LINDET-REL(85:9)
           MOVE TOTAL              TO VALOR-E1
           MOVE VALOR-E1           TO LINDET-REL(94:9)
           WRITE REG-RELAT FROM LINDET-REL.
           add 1 to lin
           IF LIN > 56 PERFORM CABECALHO.

       CARREGAR-LISTA2 SECTION.
           MOVE ZEROS TO SUBTOT
                         JUROS
                         DESC
                         TOTAL
                         QTD-SUB


           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET2
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           IF SUBTOT > 0
              PERFORM IMPRIMIR-SUBTOTAL
              MOVE SPACES TO GS-LINDET
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           END-IF

           PERFORM MOVER-DADOS-LINTOT.

       MOVER-DADOS-LINDET2 SECTION.
           EVALUATE GS-ORDEM
             WHEN 1 IF AUX-VENCTO <> VENCTO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE VENCTO-WK TO AUX-VENCTO
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 2 IF AUX-ALBUM <> ALBUM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE ALBUM-WK TO AUX-ALBUM
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 3 IF AUX-DATA-RECTO <> DATA-RECTO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE DATA-RECTO-WK TO AUX-DATA-RECTO
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 4 IF AUX-CIDADE <> CIDADE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
             WHEN 5 IF AUX-ORIGEM <> ORIGEM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE ORIGEM-WK TO AUX-ORIGEM
                       MOVE ZEROS TO SUBTOT
                                     QTD-SUB
                                     JUROS
                                     DESC
                                     TOTAL
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
                    ADD VLR-JUROS-WK TO JUROS
                    ADD VLR-DESC-WK  TO DESC
                    ADD VLR-TOT-WK   TO TOTAL
           END-EVALUATE

           MOVE ALBUM-WK          TO GS-LINDET(01: 09)
      *    MOVE DOCUMENTO-WK      TO GS-LINDET(01: 11)
           MOVE FORMA-PGTO-WK     TO GS-LINDET(11: 11)
           MOVE CLIENTE-WK        TO GS-LINDET(22:17)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(41: 11)
           MOVE DATA-RECTO-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(52: 11)

           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(63: 09)
           MOVE VLR-JUROS-WK      TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(74: 09)
           MOVE VLR-DESC-WK       TO VALOR-E2
           MOVE VALOR-E2          TO GS-LINDET(82: 09)
           MOVE VLR-TOT-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(92: 09)
           MOVE ORIGEM-WK         TO GS-LINDET(102: 2)

           MOVE CIDADE-WK         TO GS-LINDET(105:20)


           MOVE DATA-MOVTO-WK     TO GS-LINDET(150: 8)
           MOVE SEQ-WK            TO GS-LINDET(158: 4)
           ADD 1                  TO TOT-TITULO.

           ADD 1                  TO QTD-SUB.

       IMPRIMIR-SUBTOTAL SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE "SUB-TOTAL . . . " TO GS-LINDET
           MOVE QTD-SUB            TO MASC-QTDE
           MOVE MASC-QTDE          TO GS-LINDET(20:7)
           MOVE SUBTOT             TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(63:14)
           MOVE JUROS              TO VALOR-E2
           MOVE VALOR-E2           TO GS-LINDET(74:9)
           MOVE DESC               TO VALOR-E2
           MOVE VALOR-E2           TO GS-LINDET(82:9)
           MOVE TOTAL              TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(92:9)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02B.
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
           CLOSE CAD010 CAD012 CHD010 CHD013 WORK CRD020 CAD004 CGD010
                 CGD011 CAD018 COD040 CGD001 CRD020B CHD010B CBD001.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
