       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDCP120.
      *DATA: 22/11/2003
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO DE ENTRADAS DE CHEQUES EM COBRANCA-KAC
      *        Listar todos os cheques dentro do intervalo soslicitado.
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
           COPY CHPX013.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX001.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = DATA-MOVTO-WK, SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NR-CHEQUE-WK
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-ENTRADA-WK
                           WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLIENTE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-APRES-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS SITUACAO-WK
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VLR-NOM-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY CAPW012.
       COPY CAPW018.
       COPY COPW040.
       COPY CHPW010.
       COPY CHPW013.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW001.

       FD  WORK.
       01  REG-WORK.
           05  DATA-MOVTO-WK       PIC 9(08).
           05  SEQ-WK              PIC 9(04).
           05  DOCUMENTO-WK.
               10 ALBUM-WK         PIC 9(04).
               10 SEQUE-WK         PIC 9(04).
           05  CIDADE-WK           PIC X(20).
           05  DATA-COMPRA-WK      PIC 9(08).
           05  VENCTO-WK           PIC 9(8).
           05  DATA-ENTRADA-WK     PIC 9(8).
           05  DATA-APRES-WK       PIC 9(08).
           05  SITUACAO-WK         PIC X(10).
           05  VLR-NOM-WK          PIC 9(8)V99.
           05  ALINEA-WK           PIC 9(02).
           05  PORTADOR-WK         PIC 9(04).
           05  NR-CHEQUE-WK        PIC X(07).
           05  CLIENTE-WK          PIC X(20).
           05  COD-CIDADE-WK       PIC 9(04).
           05  COD-CLIENTE-WK      PIC 9(09).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CDCP120.CPB".
           COPY "CDCP120.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  SUBTOT                PIC 9(09)V99 VALUE ZEROS.
           05  AUX-ALBUM             PIC 9(4)     VALUE ZEROS.
           05  AUX-CIDADE            PIC 9(4)     VALUE ZEROS.
           05  AUX-DTENT             PIC 9(8)     VALUE ZEROS.
           05  AUX-NRCHEQUE          PIC X(7)     VALUE ZEROS.
           05  AUX-DTAPRES           PIC 9(8)     VALUE ZEROS.
           05  AUX-SITUACAO          PIC X(10)    VALUE SPACES.
           05  AUX-VLRNOMINAL        PIC 9(6)V99  VALUE ZEROS.
           05  AUX-VENCTO            PIC 9(8)     VALUE ZEROS.
           05  AUX-CLIENTE.
               10 AUX-CLASSIF        PIC 9(1)     VALUE ZEROS.
               10 AUX-CONTRATO       PIC 9(4)     VALUE ZEROS.
               10 AUX-ALBUM1         PIC 9(4)     VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
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
           05  entrada-ini          PIC 9(8)     VALUE ZEROS.
           05  entrada-fim          PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  entrada-ini-ANT      PIC 9(8)     VALUE ZEROS.
           05  entrada-fim-ANT      PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC Z.ZZZ    BLANK WHEN ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZ.ZZZ.ZZZ,ZZ
                                                  BLANK WHEN ZEROS.

           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(4)     VALUE ZEROS.
           05  TOT-VALOR-NOM         PIC 9(9)V99  VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(75)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "ENTRADA CHEQUES EM COBRANCA  -ORDEM: ".
           05  ORDEM-REL           PIC X(32)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.APRES.: ".
           05  APRES-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  APRES-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(126)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(112)  VALUE
           "CONTRATO CLIENTE              DT-VENCTO  DT.ENTRADA NR.CHE
      -    "   DATA-APRES DT-REAPRES AL PO  VALOR-NOMINAL CIDADE".
       01  LINDET.
           05  LINDET-REL          PIC X(126)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "          QTDE-TITULO   VLR-TOT-NOMINAL".
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
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD001.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CHD010 CHD013 CAD004 CAD018 COD040 CGD001 CGD010
                      CGD011 CAD012 CAD010 CRD001.

           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
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
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

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
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           CLOSE WORK
           OPEN OUTPUT WORK
           CLOSE WORK
           OPEN I-O WORK.

           MOVE "TELA-AGUARDA"           TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-entrada-ini          TO DATA-INV entrada-ini-ANT
                                            APRES-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO entrada-ini.
           MOVE GS-entrada-fim          TO DATA-INV entrada-fim-ANT
                                            APRES-FIM-REL.

           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO entrada-fim.
           MOVE ZEROS TO SEQ-WK TOT-VALOR-NOM TOT-TITULO

           PERFORM GRAVA-CHEQUE.

       GRAVA-CHEQUE SECTION.
           INITIALIZE REG-CHD013
           MOVE entrada-ini             TO DATA-ENTRADA-CH13.
           START CHD013 KEY IS NOT < DATA-ENTRADA-CH13 INVALID KEY
                MOVE "10" TO ST-CHD013.

           PERFORM UNTIL ST-CHD013 = "10"
                READ CHD013 NEXT AT END
                     MOVE "10" TO ST-CHD013
                NOT AT END
                     IF DATA-ENTRADA-CH13 > ENTRADA-FIM
                        MOVE "10" TO ST-CHD013
                     ELSE
                        MOVE DATA-ENTRADA-CH13   TO GS-EXIBE-VECTO
                        MOVE "TELA-AGUARDA1"        TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                        PERFORM MOVER-DADOS-WORK-CH
                     END-IF
                END-READ
           END-PERFORM

           MOVE "TELA-AGUARDA2"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           EXIT.

       MOVER-DADOS-WORK-CH SECTION.
           MOVE DATA-MOVTO-CH13     TO DATA-MOVTO-CH10
           MOVE SEQ-CH13            TO SEQ-CH10

           READ CHD010 INVALID KEY
                MOVE "NÃO ENCONTRADO CH10=" TO GS-MENSAGEM-ERRO
                MOVE DATA-MOVTO-CH13 TO GS-MENSAGEM-ERRO(27:10)
                MOVE SEQ-CH13        TO GS-MENSAGEM-ERRO(37: 4)
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                MOVE COD-COMPL-CH10      TO AUX-CLIENTE
                                            COD-CLIENTE-WK

                MOVE COD-COMPL-CH10      TO COD-COMPL-CG10
                READ CGD010 INVALID KEY
                     MOVE SPACES         TO COMPRADOR-CG10
                END-READ
                MOVE COMPRADOR-CG10      TO CLIENTE-WK

                MOVE COD-COMPL-CH10      TO COD-COMPL-CG11
                READ CGD011 INVALID KEY
                     MOVE ZEROS          TO CIDADE1-CG11
                END-READ
                MOVE CIDADE1-CG11        TO CIDADE
                READ CAD010 INVALID KEY
                     MOVE SPACES         TO NOME-CID
                     MOVE ZEROS          TO REGIAO-CID
                END-READ
                MOVE NOME-CID            TO CIDADE-WK
                MOVE CIDADE1-CG11        TO COD-CIDADE-WK

                IF GS-VENDEDOR = 0 OR VENDEDOR-CH10
                   IF GS-PORTADOR = 00 OR PORTADOR-CH10
                      IF GS-DESC-CIDADE = "---" OR SPACES
                                                OR CIDADE1-CG11
                         IF GS-CONTRATO = 0 OR AUX-CONTRATO
                            IF DATA-RECTO-CH13 NOT > 0
                               MOVE ALINEA-CH13        TO ALINEA-WK
                               MOVE DATA-COMPRA-CH13   TO DATA-COMPRA-WK
                               MOVE DATA-APRES-CH13    TO DATA-APRES-WK
                               MOVE SITUACAO-TIT-CH10  TO CODIGO-CR01
                               READ CRD001 INVALID KEY
                                   MOVE SPACES         TO SITUACAO-WK
                               NOT INVALID KEY
                                   MOVE DESCRICAO-CR01 TO SITUACAO-WK
                               END-READ
                               MOVE DATA-ENTRADA-CH13 TO DATA-ENTRADA-WK

                               MOVE DATA-MOVTO-CH10   TO DATA-MOVTO-WK
                               MOVE SEQ-CH10          TO SEQ-WK

      *                        MOVE CIDADE-CH10       TO CIDADE-WK


                               MOVE DATA-VENCTO-CH10  TO VENCTO-WK
                               MOVE VALOR-CH10        TO VLR-NOM-WK
                               MOVE PORTADOR-CH10     TO PORTADOR-WK
                               MOVE NR-CHEQUE-CH10    TO NR-CHEQUE-WK
                               ADD VALOR-CH10         TO TOT-VALOR-NOM
                               ADD 1                  TO TOT-TITULO
                               IF CLASS-CLIENTE-CH10 EQUAL 0
                                  MOVE CLIENTE-CH10   TO DOCUMENTO-WK
                               ELSE
                                  MOVE ZEROS          TO DOCUMENTO-WK
                               END-IF
                               WRITE REG-WORK.

       CARREGA-LISTA SECTION.
           MOVE ZEROS               TO TOT-TITULO
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           IF ST-WORK = "10"
              GO TO SAIR3.

       READ-WORK2.
           READ WORK NEXT RECORD AT END
               GO TO SAIR3.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK2.

           PERFORM MOVER-DADOS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           GO TO READ-WORK2.

       SAIR3.
           PERFORM MOVER-DADOS-LINTOT
           EXIT.

       MOVER-DADOS-LINDET SECTION.


           ADD 1                  TO TOT-TITULO
           MOVE DOCUMENTO-WK      TO GS-LINDET(01: 09)
           MOVE CLIENTE-WK        TO GS-LINDET(10: 21)

           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(31: 11)

           MOVE DATA-ENTRADA-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(42: 11)

           MOVE NR-CHEQUE-WK      TO GS-LINDET(53: 11)
      *    MOVE DATA-COMPRA-WK    TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(53: 11)

           MOVE DATA-APRES-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(64: 11)

           MOVE SITUACAO-WK       TO GS-LINDET(75:11)

           MOVE ALINEA-WK         TO GS-LINDET(86: 3)
           MOVE PORTADOR-WK       TO GS-LINDET(89: 5)
           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(94: 13)
           MOVE CIDADE-WK         TO GS-LINDET(107:20)
           MOVE SEQ-WK            TO GS-LINDET(127: 4)
           MOVE DATA-MOVTO-WK     TO GS-LINDET(132: 8).

       MOVER-DADOS-LINTOT SECTION.
           MOVE TOT-TITULO        TO QTDE-E
           MOVE QTDE-E            TO GS-LINTOT(01: 12)
           MOVE TOT-VALOR-NOM     TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(13: 14).

       CARREGAR-LISTA2 SECTION.
           MOVE ZEROS TO SUBTOT TOT-TITULO

           MOVE ZEROS TO AUX-ALBUM
                         AUX-CIDADE
                         AUX-DTENT
                         AUX-DTAPRES
                         AUX-VLRNOMINAL
                         AUX-VENCTO
                         AUX-CLIENTE

           MOVE SPACES TO AUX-NRCHEQUE
                          AUX-SITUACAO

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           IF ST-WORK = "10"
              GO TO SAIR2.

       READ-WORK.
           READ WORK NEXT RECORD AT END
               GO TO SAIR2.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK.

           PERFORM MOVER-DADOS-LINDET2
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           GO TO READ-WORK.

       SAIR2.
           IF SUBTOT > 0
              PERFORM IMPRIMIR-SUBTOTAL
              MOVE SPACES TO GS-LINDET
              MOVE "INSERE-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

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
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 2 IF AUX-ALBUM <> ALBUM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE ALBUM-WK TO AUX-ALBUM
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 3 IF AUX-NRCHEQUE <> NR-CHEQUE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE NR-CHEQUE-WK TO AUX-NRCHEQUE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 4 IF AUX-DTAPRES <> DATA-APRES-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE DATA-APRES-WK TO AUX-DTAPRES
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 5 IF AUX-SITUACAO <> SITUACAO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE SITUACAO-WK TO AUX-SITUACAO
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 6 IF AUX-CLIENTE <> COD-CLIENTE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE COD-CLIENTE-WK TO AUX-CLIENTE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 7 IF AUX-DTENT <> DATA-COMPRA-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE DATA-COMPRA-WK TO AUX-DTENT
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 8 IF AUX-VLRNOMINAL <> VLR-NOM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE VLR-NOM-WK TO AUX-VLRNOMINAL
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 9 IF AUX-CIDADE <> COD-CIDADE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL
                          MOVE SPACES TO GS-LINDET
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                       MOVE COD-CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
           END-EVALUATE

           MOVE DOCUMENTO-WK      TO GS-LINDET(01: 09)
           MOVE CLIENTE-WK        TO GS-LINDET(10: 21)

           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(31: 11)

           MOVE DATA-ENTRADA-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(42: 11)

           MOVE NR-CHEQUE-WK      TO GS-LINDET(53: 11)
      *    MOVE DATA-COMPRA-WK    TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO GS-LINDET(53: 11)

           MOVE DATA-APRES-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(64: 11)

           MOVE SITUACAO-WK       TO GS-LINDET(75: 11)

           MOVE ALINEA-WK         TO GS-LINDET(86: 3)
           MOVE PORTADOR-WK       TO GS-LINDET(89: 5)
           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(94: 13)
           MOVE CIDADE-WK         TO GS-LINDET(107:20)
           MOVE SEQ-WK            TO GS-LINDET(127: 4)
           MOVE DATA-MOVTO-WK     TO GS-LINDET(132: 8)
           ADD 1                  TO TOT-TITULO.


       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO"     TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10"   TO ST-WORK
             WHEN 2
                MOVE "DOCUMENTO " TO GS-DESCR-ORDEM
                MOVE ZEROS TO DOCUMENTO-WK
                START WORK KEY IS NOT < DOCUMENTO-WK INVALID KEY
                      MOVE "10"   TO ST-WORK
             WHEN 3
                MOVE "NR.CHEQUE " TO GS-DESCR-ORDEM
                MOVE SPACES       TO NR-CHEQUE-WK
                START WORK KEY IS NOT < NR-CHEQUE-WK   INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DT.APRESEN" TO GS-DESCR-ORDEM
                MOVE ZEROS        TO DATA-APRES-WK
                START WORK KEY IS NOT < DATA-APRES-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "SITUACAO" TO GS-DESCR-ORDEM
                MOVE SPACES       TO SITUACAO-WK
                START WORK KEY IS NOT < SITUACAO-WK INVALID KEY
                      MOVE "10"   TO ST-WORK
             WHEN 6
                MOVE "CLIENTE   " TO GS-DESCR-ORDEM
                MOVE SPACES       TO CLIENTE-WK
                START WORK KEY IS NOT < CLIENTE-WK INVALID KEY
                      MOVE "10"   TO ST-WORK

             WHEN 7
                MOVE "DT.ENTRADA" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DATA-ENTRADA-WK
                START WORK KEY IS NOT < DATA-ENTRADA-WK INVALID KEY
                      MOVE "10"   TO ST-WORK
             WHEN 8
                MOVE "VLR.NOMIN." TO GS-DESCR-ORDEM
                MOVE ZEROS TO VLR-NOM-WK
                START WORK KEY IS NOT < VLR-NOM-WK INVALID KEY
                      MOVE "10"   TO ST-WORK
             WHEN 9
                MOVE "CIDADE    " TO GS-DESCR-ORDEM
                MOVE SPACES       TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10"   TO ST-WORK

           END-EVALUATE.


       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA"    TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET       TO DS-CONTROL
           MOVE "CDCP120"         TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-ACESSO-DEVOL SECTION.
           MOVE ZEROS         TO GS-AUTORIZADO.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA01"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE 0 TO GS-AUTORIZADO
              NOT INVALID KEY
                MOVE 1 TO GS-AUTORIZADO
           END-READ.

       CHAMA-ALTERACAO SECTION.
           IF GS-LINDET = SPACES MOVE ZEROS TO GS-LINDET.
           MOVE GS-LINDET(132: 8) TO PASSAR-STRING-1(1: 8)
                                     DATA-MOVTO-CH10 DATA-MOVTO-WK
           MOVE GS-LINDET(127: 4) TO PASSAR-STRING-1(10: 4) SEQ-CH10
                                     SEQ-WK.
           move usuario-w         to passar-string-1(20: 5)
           MOVE COD-USUARIO-W     TO PASSAR-STRING-1(26: 3)


           CALL   "CHP013T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CHP013T".

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           IF ST-WORK = "10"
              GO TO SAIR4.

       READ-WORK3.
           READ WORK NEXT RECORD AT END
               GO TO SAIR4.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK3.

           PERFORM MOVER-DADOS-RELATORIO

           GO TO READ-WORK3.

       SAIR4.
           PERFORM MOVER-DADOS-LINTOT-REL

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.


       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE DOCUMENTO-WK      TO LINDET-REL(01: 09)
           MOVE CLIENTE-WK        TO LINDET-REL(10: 21)

           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(31: 11)

           MOVE DATA-ENTRADA-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(42: 11)

           MOVE NR-CHEQUE-WK      TO LINDET-REL(53: 11)
      *    MOVE DATA-COMPRA-WK    TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(53: 11)

           MOVE DATA-APRES-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(64: 11)

           MOVE SITUACAO-WK       TO LINDET-REL(75:11)

           MOVE ALINEA-WK         TO LINDET-REL(86: 3)
           MOVE PORTADOR-WK       TO LINDET-REL(89: 5)
           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(94: 13)

           MOVE CIDADE-WK         TO LINDET-REL(107:20)

           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
       MOVER-DADOS-LINTOT-REL SECTION.

           MOVE TOT-TITULO        TO QTDE-E
           MOVE QTDE-E            TO LINTOT-REL(11: 13)
           MOVE TOT-VALOR-NOM     TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(25: 14)

           WRITE REG-RELAT FROM CAB05 AFTER 2
           WRITE REG-RELAT FROM LINTOT-REL.

       IMPRIMIR-SUBTOTAL SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE "SUB-TOTAL . . . " TO GS-LINDET
           MOVE SUBTOT             TO VALOR-E1
           MOVE VALOR-E1           TO GS-LINDET(92:14)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO2 SECTION.
           MOVE ZEROS TO SUBTOT TOT-TITULO

           MOVE ZEROS TO AUX-ALBUM
                         AUX-CIDADE
                         AUX-DTENT
                         AUX-DTAPRES
                         AUX-VLRNOMINAL
                         AUX-VENCTO
                         AUX-CLIENTE

           MOVE SPACES TO AUX-NRCHEQUE
                          AUX-SITUACAO

           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           IF ST-WORK = "10"
              GO TO SAIR5.

       READ-WORK5.
           READ WORK NEXT RECORD AT END
               GO TO SAIR5.

           IF ST-WORK <> "00" AND "02"
              GO TO READ-WORK5.

           PERFORM MOVER-DADOS-RELATORIO2

           GO TO READ-WORK5.

       SAIR5.
           IF SUBTOT > 0
              PERFORM IMPRIMIR-SUBTOTAL2
              move spaces to lindet-rel
              write reg-relat from lindet-rel
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
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
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
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 3 IF AUX-NRCHEQUE <> NR-CHEQUE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE NR-CHEQUE-WK TO AUX-NRCHEQUE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 4 IF AUX-DTAPRES <> DATA-APRES-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE DATA-APRES-WK TO AUX-DTAPRES
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 5 IF AUX-SITUACAO <> SITUACAO-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE SITUACAO-WK TO AUX-SITUACAO
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 6 IF AUX-CLIENTE <> COD-CLIENTE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE COD-CLIENTE-WK TO AUX-CLIENTE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 7 IF AUX-DTENT <> DATA-COMPRA-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE DATA-COMPRA-WK TO AUX-DTENT
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 8 IF AUX-VLRNOMINAL <> VLR-NOM-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE VLR-NOM-WK TO AUX-VLRNOMINAL
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
             WHEN 9 IF AUX-CIDADE <> COD-CIDADE-WK
                       IF SUBTOT > 0
                          PERFORM IMPRIMIR-SUBTOTAL2
                          MOVE SPACES TO LINDET-REL
                          WRITE REG-RELAT FROM LINDET-REL
                          ADD 1 TO LIN
                          IF LIN > 56
                             PERFORM CABECALHO
                          END-IF
                       END-IF
                       MOVE COD-CIDADE-WK TO AUX-CIDADE
                       MOVE ZEROS TO SUBTOT
                    END-IF
                    ADD VLR-NOM-WK   TO SUBTOT
           END-EVALUATE

           MOVE SPACES            TO LINDET-REL
           MOVE DOCUMENTO-WK      TO LINDET-REL(01: 09)
           MOVE CLIENTE-WK        TO LINDET-REL(10: 21)

           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(31: 11)

           MOVE DATA-ENTRADA-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(42: 11)

           MOVE NR-CHEQUE-WK      TO LINDET-REL(53: 11)
      *    MOVE DATA-COMPRA-WK    TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV          TO DATA-E
      *    MOVE DATA-E            TO LINDET-REL(53: 11)

           MOVE DATA-APRES-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(64: 11)

           MOVE SITUACAO-WK       TO LINDET-REL(75:11)

           MOVE ALINEA-WK         TO LINDET-REL(86: 3)
           MOVE PORTADOR-WK       TO LINDET-REL(89: 5)
           MOVE VLR-NOM-WK        TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(94: 13)

           MOVE CIDADE-WK         TO LINDET-REL(107:20)

           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       IMPRIMIR-SUBTOTAL2 SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE "SUB-TOTAL . . . " TO LINDET-REL
           MOVE SUBTOT             TO VALOR-E1
           MOVE VALOR-E1           TO LINDET-REL(92:14)
           WRITE REG-RELAT FROM LINDET-REL
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.


       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM    TO ORDEM-REL.
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
           CLOSE CHD010 CHD013 CAD004 CAD018 COD040 CGD001 CGD010
                 CGD011 WORK CAD012 CAD010 CRD001.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
