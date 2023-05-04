       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP099.
      *DATA: 25/10/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Extrato de Cliente individual
      *        Listar todos os títulos com situação = 0(pagar) e 2(pago)
      *        dentro do intervalo de movimento solicitado.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX018.
           COPY CGPX010.
           COPY CRPX099.
           COPY CRPX001.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = COD-COMPL-WK SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DATA-PAGTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CAPW018.
       COPY CRPW099.
       COPY CRPW001.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  COD-COMPL-WK.
               10  CLASSIF-WK      PIC 9.
               10  CLIENTE-WK      PIC 9(8).
           05  SEQ-WK              PIC 9(5).
           05  DOCUMENTO-WK        PIC X(10).
           05  PORTADOR-WK         PIC X(10).
           05  CARTEIRA-WK         PIC X(4).
           05  SITUACAO-WK         PIC X(10).
           05  SITUACAO-SIST-WK    PIC 9.
           05  VENCTO-WK           PIC 9(8).
           05  DATA-PAGTO-WK       PIC 9(8).
           05  DIAS-ATRAS-WK       PIC 9(3).
           05  VALOR-WK            PIC 9(8)V99.
           05  JR-MULTA-WK         PIC 9(8)V99.
           05  NOSSO-NR-WK         PIC X(15).
           05  USUARIO-WK          PIC X(5).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP099.CPB".
           COPY "CRP099.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ- BLANK WHEN ZEROS.
           05  TAXA-E                PIC ZZ,Z     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ACUM            PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-ACUM            PIC 9(8)V99  VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  TOT-TITULO            PIC 9(3)     VALUE ZEROS.
           05  TOT-VALOR             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-VALOR-REC         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRAS-MEDIO       PIC 9(3)V99  VALUE ZEROS.
           05  TOT-VALOR-A-RECEB     PIC 9(8)V99  VALUE ZEROS.
           05  ATRASO-MEDIO-E        PIC ZZZ,ZZ.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  COD-COMPL-W           PIC 9(9)     VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           "RELACAO DE CONTAS A RECEBER-ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB02A.
           05  FILLER              PIC X(09)   VALUE "CLIENTE: ".
           05  CLASSIF-REL         PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  CLIENTE-REL         PIC ZZZZ.ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  NOME-CLIENTE-REL    PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-VECTO N.DOCTO    PO CART SITUACAO           VALOR DATA-
      -    "PAGTO DIAS    JR+MULTA NOSSO-NR       ".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE TITULOS     VALOR-TOTAL        VALOR REC.  ATRAS.MED
      -    "  VALOR A RECEB".
       01  LINTOT.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.


       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

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
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CRD099"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD099.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CRD001 CAD018 CGD010 CRD099.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD099 <> "00"
              MOVE "ERRO ABERTURA CRD099: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD099 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP099"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess


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
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CLIENTE-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".
       CHAMAR-POPUP SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(33: 8) TO GS-CLIENTE
                                          COD-COMPL-CG10(2: 8).
           MOVE PASSAR-STRING-1(42: 1) TO CLASSIF-W GS-CLASS(1: 1)
                                          COD-COMPL-CG10(1: 1).
           EVALUATE CLASSIF-W
              WHEN 0 MOVE "0-Contrato"       TO GS-CLASS
              WHEN 1 MOVE "1-Comum   "       TO GS-CLASS
              WHEN 9 MOVE "9-Unificado"      TO GS-CLASS
           END-EVALUATE.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CLIENTE.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CLIENTE SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-CG10.
           MOVE GS-CLIENTE     TO CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "****" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-NOME-CLIENTE.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO TOT-VALOR TOT-VALOR-REC TOT-VALOR-A-RECEB
                         TOTAL-ACUM TOT-TITULO.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-ANT
                                     VECTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-ANT
                                     VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.
           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CR99 CLASSIF-W
           MOVE GS-CLIENTE     TO CLIENTE-CR99.
           MOVE VECTO-INI         TO DATA-VENCTO-CR99.
           START CRD099 KEY IS NOT < ALT1-CR99 INVALID KEY
                 MOVE "10" TO ST-CRD099.
           PERFORM UNTIL ST-CRD099 = "10"
                 READ CRD099 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD099
                 NOT AT END
                      IF CLIENTE-CR99 NOT = GS-CLIENTE OR
                         CLASS-CLIENTE-CR99 NOT = CLASSIF-W
                         MOVE "10" TO ST-CRD099
                      ELSE
                         IF DATA-VENCTO-CR99 > VECTO-FIM
                            MOVE "10" TO ST-CRD099
                         ELSE
                            MOVE DATA-VENCTO-CR99    TO GS-EXIBE-VECTO
                            MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
      *                     IF SITUACAO-CR99 = 0 OR SITUACAO-CR99 = 2 OR
      *                        SITUACAO-CR99 = 4
                               PERFORM MOVER-DADOS-WORK
                               WRITE REG-WORK
      *                     ELSE CONTINUE
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE CLASS-CLIENTE-CR99  TO CLASSIF-WK
           MOVE CLIENTE-CR99        TO CLIENTE-WK
           MOVE SEQ-CR99            TO SEQ-WK
           EVALUATE CARTEIRA-CR99
              WHEN 1 MOVE "SIMP"    TO CARTEIRA-WK
              WHEN 2 MOVE "CAUC"    TO CARTEIRA-WK
              WHEN 3 MOVE "DESC"    TO CARTEIRA-WK
           END-EVALUATE

           MOVE NR-DOCTO-CR99       TO DOCUMENTO-WK
           MOVE PORTADOR-CR99       TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*****"        TO NOME-PORT
           END-READ
           MOVE NOME-PORT           TO PORTADOR-WK
           MOVE DATA-RCTO-CR99      TO DATA-PAGTO-WK
           MOVE SITUACAO-TIT-CR99   TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "******"       TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-CR99       TO SITUACAO-SIST-WK
           MOVE SITUACAO-TIT-CR01   TO SITUACAO-WK
           MOVE OUTRO-DOCTO-CR99    TO NOSSO-NR-WK
           COMPUTE JR-MULTA-WK = JURO-RCTO-CR99 + MULTA-RCTO-CR99
           MOVE VALOR-TOT-CR99      TO VALOR-WK
           MOVE DATA-VENCTO-CR99    TO VENCTO-WK
           MOVE DIGITADOR-CR99      TO USUARIO-WK
           PERFORM DIAS-ATRASO
           ADD 1                    TO TOT-TITULO
           ADD VALOR-LIQ-CR99       TO TOT-VALOR-REC
           ADD VALOR-TOT-CR99       TO TOT-VALOR.

       DIAS-ATRASO SECTION.
           IF DATA-RCTO-CR99 NOT = ZEROS
              MOVE DATA-VENCTO-CR99      TO GRDIAS-AAMMDD-INICIAL
              MOVE DATA-RCTO-CR99        TO GRDIAS-AAMMDD-FINAL
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              MOVE GRDIAS-NUM-DIAS       TO DIAS-ATRAS-WK
              COMPUTE VALOR-ACUM = DIAS-ATRAS-WK * VALOR-LIQ-CR99
              ADD VALOR-ACUM             TO TOTAL-ACUM
           ELSE MOVE ZEROS TO DIAS-ATRAS-WK TOTAL-ACUM.
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
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(01: 11)
           MOVE DOCUMENTO-WK      TO GS-LINDET(12: 11)
           MOVE PORTADOR-WK       TO GS-LINDET(23: 11)
           MOVE CARTEIRA-WK       TO GS-LINDET(34: 5)
           MOVE SITUACAO-WK       TO GS-LINDET(39: 15)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(54: 12)
           IF SITUACAO-SIST-WK = 4
              MOVE "CANCELADA" TO GS-LINDET(66: 11)
           ELSE IF SITUACAO-WK = 3
                   MOVE "ESTORNADA" TO GS-LINDET(66: 11)
                ELSE MOVE DATA-PAGTO-WK     TO DATA-INV
                     CALL "GRIDAT1" USING DATA-INV
                     MOVE DATA-INV          TO DATA-E
                     MOVE DATA-E            TO GS-LINDET(66: 11)
                END-IF
           END-IF
           MOVE USUARIO-WK        TO GS-LINDET(77: 5)
           MOVE JR-MULTA-WK       TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(82: 12)
           MOVE NOSSO-NR-WK       TO GS-LINDET(94: 16)
           MOVE SEQ-WK            TO GS-LINDET(111: 05).
           MOVE COD-COMPL-WK      TO GS-LINDET(116: 9).
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
                MOVE "PORTADOR" TO GS-DESCR-ORDEM
                MOVE SPACES TO PORTADOR-WK
                START WORK KEY IS NOT < PORTADOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA-PAGTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DATA-PAGTO-WK
                START WORK KEY IS NOT < DATA-PAGTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-TITULO        TO GS-LINTOT(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(18: 13)
           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(36: 13)
           DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO GS-LINTOT(54: 10)
           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(66: 13)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *CALCULA-DIAS SECTION.
      *    MOVE GS-EMISSAO1     TO GRTIME-DATE.
      *    MOVE GS-VENCTO2      TO GRTIME-DATE-FINAL.
      *    MOVE 3                   TO GRTIME-FUNCTION.
      *    MOVE 1                   TO GRTIME-TYPE.
      *    CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    MOVE GRTIME-DAYS-FINAL   TO GS-DIAS2.
      *CALCULA-JUROS SECTION.
      *    MOVE GS-TAXA2        TO GS-TAXA3.
      *    COMPUTE GS-VLR-JUROS2 = (GS-VLR-RETIR2 *
      *          (GS-TAXA2 / 100) /30) * GS-DIAS2.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP099" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE PORTADOR-WK       TO LINDET-REL(23: 03)
           MOVE CARTEIRA-WK       TO LINDET-REL(26: 5)
           MOVE SITUACAO-WK       TO LINDET-REL(31: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(42: 12)
           IF SITUACAO-SIST-WK = 4
              MOVE "CANCELADA"    TO LINDET-REL(66: 11)
           ELSE IF SITUACAO-SIST-WK = 3
                   MOVE "ESTORNADA" TO LINDET-REL(66: 11)

                ELSE MOVE DATA-PAGTO-WK     TO DATA-INV
                  CALL "GRIDAT1" USING DATA-INV
                  MOVE DATA-INV          TO DATA-E
                  MOVE DATA-E            TO LINDET-REL(56: 11)
                END-IF
           END-IF
           MOVE USUARIO-WK        TO LINDET-REL(67: 5)
           MOVE JR-MULTA-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(72: 12)
           MOVE NOSSO-NR-WK       TO LINDET-REL(85: 15)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-TITULO        TO LINTOT-REL(01: 14)
           MOVE TOT-VALOR         TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(15: 13)
           MOVE TOT-VALOR-REC     TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(33: 13)
           DIVIDE TOTAL-ACUM BY TOT-VALOR GIVING TOT-ATRAS-MEDIO
           MOVE TOT-ATRAS-MEDIO   TO ATRASO-MEDIO-E
           MOVE ATRASO-MEDIO-E    TO LINTOT-REL(51: 10)
           MOVE TOT-VALOR-A-RECEB TO VALOR-E
           MOVE VALOR-E           TO LINTOT-REL(63: 13)
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-CLASS(1: 1) TO CLASSIF-REL.
           MOVE GS-CLIENTE     TO CLIENTE-REL
           MOVE GS-NOME-CLIENTE TO NOME-CLIENTE-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB02A AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP099"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CGD010 CRD001 CRD099 CAD018 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
