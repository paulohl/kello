       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCP130.
      *DATA: 16/11/2003
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE CHEQUE E DUPLICATAS EM COBRANCA-KAC
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
           COPY CAPX018.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX040.
           COPY CHPX010.
           COPY CHPX013.
           COPY CRPX020.
           COPY CRPX001.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REGIAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLIENTE-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW018.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW040.
       COPY CHPW010.
       COPY CHPW013.
       COPY CRPW020.
       COPY CRPW001.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(04).
           05  DOCUMENTO-WK        PIC 9(09).
           05  CIDADE-WK           PIC X(20).
           05  REGIAO-WK           PIC X(20).
           05  PORTADOR-WK         PIC 9(04).
           05  VENCTO-WK           PIC 9(8).
           05  VLR-NOM-WK          PIC 9(8)V99.
           05  VLR-SLD-WK          PIC 9(8)V99.
           05  ORIGEM-WK           PIC X(2).
           05  CLIENTE-WK          PIC X(30).
           05  DDD-WK              PIC 9(02).
           05  FONE-WK             PIC 9(08).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CDCP130.CPB".
           COPY "CDCP130.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-TIPO              PIC 9.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  LIN                   PIC 9(02).
           05  VALOR-E               PIC ZZZ.ZZZ.ZZZ,ZZ
                                                  BLANK WHEN ZEROS.

           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  QTDE-E                PIC Z.ZZZ    BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(4)     VALUE ZEROS.
           05  TOT-VALOR-NOM         PIC 9(9)V99  VALUE ZEROS.
           05  TOT-VALOR-SLD         PIC 9(9)V99  VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  AUX-DOCUMENTO         PIC 9(09)    VALUE ZEROS.
           05  AUX-CIDADE            PIC X(20)    VALUE SPACES.
           05  AUX-REGIAO            PIC X(20)    VALUE SPACES.
           05  AUX-PORTADOR          PIC 9(04)    VALUE ZEROS.
           05  AUX-VENCTO            PIC 9(8)     VALUE ZEROS.
           05  AUX-VENDEDOR          PIC X(30)    VALUE SPACES.
           05  AUX-CLIENTES          PIC X(30)    VALUE SPACES.
           05  SUBTOT                PIC 9(12)V99 VALUE ZEROS.
           05  SUBTOT-SLD            PIC 9(12)V99 VALUE ZEROS.
           05  AUX-CLASSIFICACAO     PIC 9(01).
           05  AUX-CLIENTE.
               10 AUX-CONTRATO       PIC 9(04).
               10 AUX-ITEM           PIC 9(04).

          COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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
           05  FILLER              PIC X(41)   VALUE
           "RELAT.CHEQ/DUPL EM COBRANCA  -ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VCTO-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VCTO-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(115)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(115) VALUE
           "CONT ALBU CLIENTE                        TELEFONE     CIDADE
      -    "               PT  DATA VECTO VLR.NOMINAL  VLR.SALDO OR".
      *    05  FILLER              PIC X(110)  VALUE
      *    " CONTRATO CIDADE               DT-VENCTO  REGIAO
      *    "   PT  VENDEDOR                  VALOR-NOMINAL".

       01 DET-01.
          05 DET-ALBUM             PIC 9999.9999.
          05 FILLER                PIC X(01).
          05 DET-CLIENTE           PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-DDD               PIC 9(02).
          05 FILLER                PIC X(01) VALUE "-".
          05 DET-FONE              PIC 9999B9999.
          05 FILLER                PIC X(01).
          05 DET-CIDADE            PIC X(20).
          05 FILLER                PIC X(01).
          05 DET-PT                PIC X(04).
          05 FILLER                PIC X(01).
          05 DET-DTVENC            PIC 99/99/9999 BLANK WHEN ZEROS.
          05 FILLER                PIC X(02).
          05 DET-VLRNOM            PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET-VLRSLD            PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET-ORIGEM            PIC X(02).

       01 DET-SUBTOT.
          05 FILLER                PIC X(91) VALUE "SUB-TOTAL . . .".
          05 DET-PARC-NOM          PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET-PARC-SLD          PIC ZZZ.ZZ9,99.

       01 DET-TOTAL.
          05 DET-TOT-TITULOS       PIC ZZZ.ZZZ.ZZ9.
          05 FILLER                PIC X(05) VALUE SPACES.
          05 DET-TOT-NOM           PIC ZZZ.ZZZ.ZZ9,99.
          05 FILLER                PIC X(02) VALUE SPACES.
          05 DET-TOT-SLD           PIC ZZZ.ZZZ.ZZ9,99.



       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTD TITULOS        VLR-TOT-NOM     VLR-TOT-SLD".

       01  LINTOT.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.

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
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD001.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CRD020 CHD010 CHD013 CAD010 CGD001 CGD011 CAD012
                      CAD018 COD040 CRD001 CGD010.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-LE-CIDADE-TRUE
                       PERFORM LER-CIDADE
               WHEN GS-LE-VENDEDOR-TRUE
                       PERFORM LER-VENDEDOR
               WHEN GS-LE-PORTADOR-TRUE
                       PERFORM LER-PORTADOR
               WHEN GS-LE-CONTRATO-TRUE
                       PERFORM LER-CONTRATO
               WHEN GS-LE-SITUACAO-TRUE
                       PERFORM LER-SITUACAO
               WHEN GS-POP-UP-CIDADE-TRUE
                       PERFORM POP-UP-CIDADE
               WHEN GS-POP-UP-VENDEDOR-TRUE
                       PERFORM POP-UP-VENDEDOR
               WHEN GS-POP-UP-PORTADOR-TRUE
                       PERFORM POP-UP-PORTADOR
               WHEN GS-POP-UP-CONTRATO-TRUE
                       PERFORM POP-UP-CONTRATO
               WHEN GS-POP-UP-SITUACAO-TRUE
                       PERFORM POP-UP-SITUACAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-SITUACAO SECTION.
           MOVE GS-SITUACAO-TIT TO CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "*********" TO
                   SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01  TO GS-DESC-SITUACAO.

       POP-UP-SITUACAO SECTION.
           CALL   "CRP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CRP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-SITUACAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT.

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
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO
           CLOSE WORK
           OPEN OUTPUT WORK

           CLOSE WORK
           OPEN I-O WORK.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-VENCTO-INI TO DATA-INV VCTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-INI.

           MOVE GS-VENCTO-FIM TO DATA-INV VCTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VENCTO-FIM.

           MOVE ZEROS TO SEQ-WK
                         TOT-VALOR-NOM
                         TOT-TITULO

           MOVE GS-CLASSIFICACAO(1:1)  TO AUX-CLASSIFICACAO

           IF AUX-TIPO = 9 OR 5
              PERFORM GRAVA-CHEQUE.

           IF AUX-TIPO <> 5
              PERFORM GRAVA-A-RECEBER.

           MOVE "TELA-AGUARDA2"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-CHEQUE SECTION.
           MOVE 5                  TO SITUACAO-CH10
           MOVE VENCTO-INI         TO DATA-VENCTO-CH10
           MOVE ZEROS              TO PORTADOR-CH10
           START CHD010 KEY IS NOT < ALT-CH2 INVALID KEY
                  MOVE "10" TO ST-CHD010
           END-START

           PERFORM UNTIL ST-CHD010 = "10"
             READ CHD010 NEXT RECORD AT END MOVE "10" TO ST-CHD010
             NOT AT END
             IF DATA-VENCTO-CH10 > VENCTO-FIM OR SITUACAO-CH10 <> 5
                MOVE "10" TO ST-CHD010
             ELSE
                MOVE DATA-VENCTO-CH10    TO GS-EXIBE-VECTO
                MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM


                IF AUX-CLASSIFICACAO = 9 OR AUX-CLASSIFICACAO =
                   CLASS-CLIENTE-CH10
                   PERFORM MOVER-DADOS-WORK-CH
                END-IF
             END-IF
             END-READ
           END-PERFORM.

       MOVER-DADOS-WORK-CH SECTION.
           MOVE COD-COMPL-CH10      TO COD-COMPL-CG11
           READ CGD011 INVALID KEY
                MOVE ZEROS   TO CIDADE1-CG11
           END-READ
           MOVE CIDADE1-CG11        TO CIDADE

           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID
                MOVE ZEROS  TO REGIAO-CID
           END-READ

           MOVE CLIENTE-CH10       TO AUX-CLIENTE
           IF GS-VENDEDOR = 0 OR VENDEDOR-CH10 = GS-VENDEDOR
              IF GS-PORTADOR = 00 OR PORTADOR-CH10 = GS-PORTADOR
                 IF GS-DESC-CIDADE = "---" OR GS-DESC-CIDADE = SPACES OR
                                            CIDADE = GS-CIDADE
                    IF GS-UF = SPACES OR GS-UF = UF-CID
                       IF GS-CONTRATO = 0 OR GS-CONTRATO = AUX-CONTRATO
                          IF GS-SITUACAO-TIT = 0 OR GS-SITUACAO-TIT =
                             SITUACAO-TIT-CH10

                             ADD 1                    TO SEQ-WK
                             ADD 1                    TO TOT-TITULO

                             MOVE SPACES              TO REGIAO-WK

      *                      MOVE VENDEDOR-CH10       TO CODIGO-CG01
      *
      *                      READ CGD001 INVALID KEY
      *                           MOVE SPACES          TO NOME-CG01
      *                      END-READ
      *
      *                      MOVE NOME-CG01           TO VENDEDOR-WK

                             MOVE COD-COMPL-CH10 TO COD-COMPL-CG10
                             READ CGD010 INVALID KEY
                                  MOVE SPACES         TO COMPRADOR-CG10
                             END-READ

                             MOVE COMPRADOR-CG10      TO CLIENTE-WK


                             MOVE COD-COMPL-CH10      TO COD-COMPL-CG11
                             READ CGD011 INVALID KEY
                                  MOVE ZEROS   TO CIDADE1-CG11
                             END-READ
                             MOVE FONE1-CG11          TO FONE-WK
                             MOVE CIDADE1-CG11        TO CIDADE
                             READ CAD010 INVALID KEY
                                  MOVE SPACES TO NOME-CID
                                  MOVE ZEROS  TO REGIAO-CID
                             END-READ
                             MOVE DDD-CID             TO DDD-WK
                             MOVE NOME-CID            TO CIDADE-WK
                             MOVE REGIAO-CID          TO CODIGO-REG
                             READ CAD012 INVALID KEY
                                  MOVE SPACES TO NOME-REG
                             END-READ
                             MOVE NOME-REG            TO REGIAO-WK

                             MOVE PORTADOR-CH10       TO PORTADOR-WK

                             MOVE DATA-VENCTO-CH10   TO VENCTO-WK
                             MOVE "CH"               TO ORIGEM-WK
                             MOVE VALOR-CH10         TO VLR-NOM-WK
                             MOVE VALOR-SALDO-CH10   TO VLR-SLD-WK
                             ADD VALOR-CH10          TO TOT-VALOR-NOM

                             MOVE COD-COMPL-CH10     TO DOCUMENTO-WK

      *                      IF CLASS-CLIENTE-CH10 EQUAL 0
      *                         MOVE CLIENTE-CH10    TO DOCUMENTO-WK
      *                      ELSE
      *                         MOVE ZEROS           TO DOCUMENTO-WK
      *                      END-IF

                             WRITE REG-WORK.

       GRAVA-A-RECEBER SECTION.
           MOVE VENCTO-INI      TO DATA-VENCTO-CR20
           MOVE ZEROS           TO SITUACAO-CR20
           MOVE ZEROS           TO COD-COMPL-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
             READ CRD020 NEXT RECORD AT END MOVE "10" TO ST-CRD020
              NOT AT END
              IF DATA-VENCTO-CR20 > VENCTO-FIM OR
                 SITUACAO-CR20 <> ZEROS
                   MOVE "10" TO ST-CRD020
              ELSE
                MOVE DATA-VENCTO-CR20      TO GS-EXIBE-VECTO
                MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM

                IF AUX-CLASSIFICACAO = 9 OR AUX-CLASSIFICACAO =
                   CLASS-CLIENTE-CR20
                   PERFORM MOVER-DADOS-WORK-REC
                END-IF

             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-WORK-REC SECTION.
           MOVE CLIENTE-CR20       TO AUX-CLIENTE

           MOVE COD-COMPL-CR20      TO COD-COMPL-CG11
           READ CGD011 INVALID KEY
                       MOVE ZEROS   TO CIDADE1-CG11
           END-READ
           MOVE CIDADE1-CG11        TO CIDADE

           MOVE CIDADE1-CG11        TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID
                MOVE ZEROS  TO REGIAO-CID
           END-READ

           IF GS-VENDEDOR = 0 OR VENDEDOR-CR20 = GS-VENDEDOR
              IF GS-PORTADOR = 00 OR PORTADOR-CR20 = GS-PORTADOR
                 IF GS-DESC-CIDADE = "---" OR GS-DESC-CIDADE = SPACES OR
                    CIDADE = GS-CIDADE
                    IF GS-UF = SPACES OR GS-UF = UF-CID
                       IF GS-CONTRATO = 0 OR GS-CONTRATO = AUX-CONTRATO
                          IF AUX-TIPO = TIPO-DOCTO-CR20 or aux-tipo = 9
                             IF GS-SITUACAO-TIT = 0 OR GS-SITUACAO-TIT =
                                SITUACAO-TIT-CR20

                                ADD  1                   TO SEQ-WK
                                                            TOT-TITULO

                                MOVE DATA-VENCTO-CR20    TO VENCTO-WK
                                MOVE VALOR-TOT-CR20      TO VLR-NOM-WK
                                MOVE VALOR-SALDO-CR20    TO VLR-SLD-WK

      *                         MOVE VENDEDOR-CR20       TO CODIGO-CG01
      *                         READ CGD001 INVALID KEY
      *                              MOVE SPACES          TO VENDEDOR-WK
      *                         NOT INVALID KEY
      *                              MOVE NOME-CG01       TO VENDEDOR-WK
      *                         END-READ

                                MOVE COD-COMPL-CR20 TO
                                     COD-COMPL-CG10
                                READ CGD010 INVALID KEY
                                     MOVE SPACES TO COMPRADOR-CG10
                                END-READ
                                MOVE COMPRADOR-CG10     TO CLIENTE-WK

                                ADD VALOR-TOT-CR20      TO TOT-VALOR-NOM

                                MOVE COD-COMPL-CR20     TO DOCUMENTO-WK


      *                         IF CLASS-CLIENTE-CR20 EQUAL 0
      *                            MOVE CLIENTE-CR20     TO DOCUMENTO-WK
      *                         ELSE
      *                            MOVE ZEROS            TO DOCUMENTO-WK
      *                         END-IF

                                MOVE PORTADOR-CR20       TO PORTADOR-WK
                                MOVE "CR"                TO ORIGEM-WK
                                MOVE COD-COMPL-CR20    TO COD-COMPL-CG11
                                READ CGD011 INVALID KEY
                                     MOVE ZEROS   TO CIDADE1-CG11
                                END-READ
                                MOVE FONE1-CG11          TO FONE-WK
                                MOVE CIDADE1-CG11        TO CIDADE
                                READ CAD010 INVALID KEY
                                     MOVE SPACES TO NOME-CID
                                     MOVE ZEROS  TO REGIAO-CID
                                END-READ
                                MOVE DDD-CID             TO DDD-WK
                                MOVE NOME-CID            TO CIDADE-WK
                                MOVE REGIAO-CID          TO CODIGO-REG
                                READ CAD012 INVALID KEY
                                     MOVE SPACES TO NOME-REG
                                END-READ
                                MOVE NOME-REG            TO REGIAO-WK
                                WRITE REG-WORK.

       CARREGAR-LISTA2 SECTION.
           MOVE ZEROS TO SUBTOT
                         SUBTOT-SLD
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
           IF SUBTOT > 0 OR SUBTOT-SLD
              PERFORM IMPRIMIR-SUBTOTAL
              MOVE SPACES TO GS-LINDET
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           END-IF

           PERFORM MOVER-DADOS-LINTOT.

       MOVER-DADOS-LINDET2 SECTION.
           EVALUATE GS-ORDEM
             WHEN 1 IF AUX-VENCTO <> VENCTO-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE VENCTO-WK TO AUX-VENCTO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD

             WHEN 2 IF AUX-DOCUMENTO <> DOCUMENTO-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE DOCUMENTO-WK TO AUX-DOCUMENTO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD
             WHEN 3 IF AUX-REGIAO <> REGIAO-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE REGIAO-WK TO AUX-REGIAO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD
             WHEN 4 IF AUX-PORTADOR <> PORTADOR-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE PORTADOR-WK TO AUX-PORTADOR
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD
             WHEN 5 IF AUX-CIDADE <> CIDADE-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD
      *      WHEN 6 IF AUX-VENDEDOR <> VENDEDOR-WK
      *                IF SUBTOT > 0
      *                   PERFORM IMPRIMIR-SUBTOTAL
      *                   MOVE SPACES TO GS-LINDET
      *                   MOVE "INSERE-LIST" TO DS-PROCEDURE
      *                   PERFORM CALL-DIALOG-SYSTEM
      *                END-IF
      *                MOVE VENDEDOR-WK TO AUX-VENDEDOR
      *                MOVE ZEROS TO SUBTOT
      *             END-IF
      *             ADD VLR-NOM-WK TO SUBTOT
             WHEN 6 IF AUX-CLIENTES <> CLIENTE-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE CLIENTE-WK TO AUX-CLIENTES
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-SLD-WK TO SUBTOT-SLD
           END-EVALUATE

      *    MOVE DOCUMENTO-WK(1:1) TO GS-LINDET(01: 02)
      *    MOVE DOCUMENTO-WK(2:8) TO GS-LINDET(03: 09)
      *    MOVE CIDADE-WK         TO GS-LINDET(12: 21)
      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(33: 11)
      *    MOVE REGIAO-WK         TO GS-LINDET(44: 21)
      *    MOVE PORTADOR-WK       TO GS-LINDET(65: 5)
      *    MOVE CLIENTE-WK        TO GS-LINDET(70:25)
      *    MOVE VLR-NOM-WK        TO VALOR-E1
      *    MOVE VALOR-E1          TO GS-LINDET(95: 14)
      *    MOVE ORIGEM-WK         TO GS-LINDET(109: 2)


           MOVE DOCUMENTO-WK(2:8) TO DET-ALBUM
           MOVE CLIENTE-WK        TO DET-CLIENTE
           MOVE DDD-WK            TO DET-DDD
           MOVE FONE-WK           TO DET-FONE
           MOVE CIDADE-WK         TO DET-CIDADE
           MOVE PORTADOR-WK       TO DET-PT
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET-DTVENC
           MOVE VLR-NOM-WK        TO DET-VLRNOM
           MOVE VLR-SLD-WK        TO DET-VLRSLD
           MOVE ORIGEM-WK         TO DET-ORIGEM

           MOVE DET-01            TO GS-LINDET.

       IMPRIMIR-SUBTOTAL SECTION.
           MOVE SUBTOT             TO DET-PARC-NOM
           MOVE SUBTOT-SLD         TO DET-PARC-SLD
           MOVE DET-SUBTOT         TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       CARREGA-LISTA SECTION.
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
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-LINTOT.

       MOVER-DADOS-LINDET SECTION.
      *    MOVE DOCUMENTO-WK(1:1) TO GS-LINDET(01:02)
      *    MOVE DOCUMENTO-WK(2:8) TO GS-LINDET(03:09)
      *    MOVE CIDADE-WK         TO GS-LINDET(12: 21)
      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(33: 11)
      *    MOVE REGIAO-WK         TO GS-LINDET(44: 21)
      *    MOVE PORTADOR-WK       TO GS-LINDET(65: 5)
      *    MOVE CLIENTE-WK        TO GS-LINDET(70:25)
      *    MOVE VLR-NOM-WK        TO VALOR-E1
      *    MOVE VALOR-E1          TO GS-LINDET(95: 14)
      *    MOVE ORIGEM-WK         TO GS-LINDET(109: 2).

           MOVE DOCUMENTO-WK(2:8) TO DET-ALBUM
           MOVE CLIENTE-WK        TO DET-CLIENTE
           MOVE DDD-WK            TO DET-DDD
           MOVE FONE-WK           TO DET-FONE
           MOVE CIDADE-WK         TO DET-CIDADE
           MOVE PORTADOR-WK       TO DET-PT
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET-DTVENC
           MOVE VLR-NOM-WK        TO DET-VLRNOM
           MOVE VLR-SLD-WK        TO DET-VLRSLD
           MOVE ORIGEM-WK         TO DET-ORIGEM

           MOVE DET-01            TO GS-LINDET.

       MOVER-DADOS-LINTOT SECTION.
           MOVE TOT-TITULO        TO DET-TOT-TITULOS
           MOVE TOT-VALOR-NOM     TO DET-TOT-NOM
           MOVE TOT-VALOR-SLD     TO DET-TOT-SLD
           MOVE DET-TOTAL         TO GS-LINTOT.

       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO " TO GS-DESCR-ORDEM
                MOVE ZEROS TO DOCUMENTO-WK
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "REGIAO    " TO GS-DESCR-ORDEM
                MOVE SPACES     TO REGIAO-WK
                START WORK KEY IS NOT < REGIAO-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "PORTADOR  " TO GS-DESCR-ORDEM
                MOVE ZEROS      TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK   INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "CIDADE    " TO GS-DESCR-ORDEM
                MOVE SPACES     TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
      *      WHEN 6
      *         MOVE "VENDEDOR  " TO GS-DESCR-ORDEM
      *         MOVE SPACES     TO VENDEDOR-WK
      *         START WORK KEY IS NOT < VENDEDOR-WK     INVALID KEY
      *               MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "CLIENTE   " TO GS-DESCR-ORDEM
                MOVE SPACES     TO CLIENTE-WK
                START WORK KEY IS NOT < CLIENTE-WK     INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CDCP130" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

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
      *    MOVE SPACES            TO LINDET-REL
      *    MOVE DOCUMENTO-WK(1:1) TO LINDET-REL(01: 02)
      *    MOVE DOCUMENTO-WK(2:8) TO LINDET-REL(03: 09)
      *    MOVE CIDADE-WK         TO LINDET-REL(12: 21)
      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(33: 11)
      *    MOVE REGIAO-WK         TO LINDET-REL(44: 21)
      *    MOVE PORTADOR-WK       TO LINDET-REL(65: 5)
      *    MOVE CLIENTE-WK        TO LINDET-REL(70:25)
      *    MOVE VLR-NOM-WK        TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(95: 14)
      *    MOVE ORIGEM-WK         TO LINDET-REL(109: 2).

           MOVE DOCUMENTO-WK(2:8) TO DET-ALBUM
           MOVE CLIENTE-WK        TO DET-CLIENTE
           MOVE DDD-WK            TO DET-DDD
           MOVE FONE-WK           TO DET-FONE
           MOVE CIDADE-WK         TO DET-CIDADE
           MOVE PORTADOR-WK       TO DET-PT
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET-DTVENC
           MOVE VLR-NOM-WK        TO DET-VLRNOM
           MOVE VLR-SLD-WK        TO DET-VLRSLD
           MOVE ORIGEM-WK         TO DET-ORIGEM

           WRITE REG-RELAT FROM DET-01
      *    WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       MOVER-DADOS-LINTOT-REL SECTION.
           MOVE TOT-TITULO        TO DET-TOT-TITULOS
           MOVE TOT-VALOR-NOM     TO DET-TOT-NOM
           MOVE TOT-VALOR-SLD     TO DET-TOT-SLD
           MOVE DET-TOTAL         TO LINTOT-REL

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT-REL.


       IMPRIME-RELATORIO2 SECTION.
           MOVE ZEROS TO SUBTOT
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           INITIALIZE AUX-DOCUMENTO
                      AUX-CIDADE
                      AUX-REGIAO
                      AUX-PORTADOR
                      AUX-VENCTO
                      AUX-VENDEDOR
                      SUBTOT
                      SUBTOT-SLD
                      AUX-CLIENTE
           PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO2
              END-READ
           END-PERFORM.

           IF SUBTOT > 0 OR SUBTOT-SLD
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
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE VENCTO-WK TO AUX-VENCTO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD

             WHEN 2 IF AUX-DOCUMENTO <> DOCUMENTO-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE DOCUMENTO-WK TO AUX-DOCUMENTO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD
             WHEN 3 IF AUX-REGIAO <> REGIAO-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE REGIAO-WK TO AUX-REGIAO
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD
             WHEN 4 IF AUX-PORTADOR <> PORTADOR-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE PORTADOR-WK TO AUX-PORTADOR
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD
             WHEN 5 IF AUX-CIDADE <> CIDADE-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD
      *      WHEN 6 IF AUX-VENDEDOR <> VENDEDOR-WK
      *                IF SUBTOT > 0
      *                   PERFORM IMPRIMIR-SUBTOTAL2
      *                   MOVE SPACES            TO LINDET-REL
      *                   WRITE REG-RELAT FROM LINDET-REL
      *                   ADD 1 TO LIN
      *                   IF LIN > 56
      *                      PERFORM CABECALHO
      *                   END-IF
      *                END-IF
      *                MOVE VENDEDOR-WK TO AUX-VENDEDOR
      *                MOVE ZEROS TO SUBTOT
      *             END-IF
      *             ADD VLR-NOM-WK TO SUBTOT
             WHEN 6 IF AUX-CLIENTES <> CLIENTE-WK
                       IF SUBTOT > 0 OR SUBTOT-SLD > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES            TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE CLIENTE-WK TO AUX-CLIENTES
                       MOVE ZEROS TO SUBTOT
                       MOVE ZEROS TO SUBTOT-SLD
                    END-IF
                    ADD VLR-NOM-WK TO SUBTOT
                    ADD VLR-NOM-WK TO SUBTOT-SLD
           END-EVALUATE

      *    MOVE SPACES            TO LINDET-REL
      *    MOVE DOCUMENTO-WK(1:1) TO LINDET-REL(01: 02)
      *    MOVE DOCUMENTO-WK(2:8) TO LINDET-REL(03: 09)
      *    MOVE CIDADE-WK         TO LINDET-REL(12: 21)
      *    MOVE VENCTO-WK         TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(33: 11)
      *    MOVE REGIAO-WK         TO LINDET-REL(44: 21)
      *    MOVE PORTADOR-WK       TO LINDET-REL(65: 9)
      *    MOVE CLIENTE-WK        TO LINDET-REL(70:25)
      *    MOVE VLR-NOM-WK        TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(95: 14)
      *    MOVE ORIGEM-WK         TO LINDET-REL(109: 2).

           MOVE DOCUMENTO-WK(2:8) TO DET-ALBUM
           MOVE CLIENTE-WK        TO DET-CLIENTE
           MOVE DDD-WK            TO DET-DDD
           MOVE FONE-WK           TO DET-FONE
           MOVE CIDADE-WK         TO DET-CIDADE
           MOVE PORTADOR-WK       TO DET-PT
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DET-DTVENC
           MOVE VLR-NOM-WK        TO DET-VLRNOM
           MOVE VLR-SLD-WK        TO DET-VLRSLD
           MOVE ORIGEM-WK         TO DET-ORIGEM

           WRITE REG-RELAT FROM DET-01

      *    WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       IMPRIMIR-SUBTOTAL2 SECTION.
          MOVE SUBTOT              TO DET-PARC-NOM
          MOVE SUBTOT-SLD          TO DET-PARC-SLD
          MOVE DET-SUBTOT          TO LINDET-REL
           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CHD010 CHD013 WORK CRD020 CAD010 CAD012 CGD001 CGD011
                 CAD018 COD040 CRD001 CGD010.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
