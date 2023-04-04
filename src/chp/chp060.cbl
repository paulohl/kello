       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP060.
      *DATA: 24/06/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Extrato de Cliente individual - CONTAS A RECEBER E CHEQUE
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
           COPY CGPX010.
           COPY CHPX010.
           COPY CHPX013.
           COPY CRPX001.
           COPY CRPX020.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DESCR-SITUACAO-SIST-WK
                        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PORTADOR-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CAPW018.
       COPY CHPW010.
       COPY CHPW013.
       COPY CRPW001.
       COPY CRPW020.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  COD-COMPL-WK.
               10  CLASSIF-WK      PIC 9.
               10  CLIENTE-WK      PIC 9(8).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  SEQ-WK              PIC 9(4).
           05  DOCUMENTO-WK        PIC X(10).
           05  PORTADOR-WK         PIC X(10).
           05  CARTEIRA-WK         PIC X(4).
           05  SITUACAO-WK         PIC X(10).
           05  SITUACAO-SIST-WK    PIC 9.
           05  DESCR-SITUACAO-SIST-WK PIC X(15).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  DATA-BAIXA-WK       PIC 9(8).
           05  JUROS-WK            PIC 9(8)V99.
           05  ORIGEM-WK           PIC X(5).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP060.CPB".
           COPY "CHP060.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
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
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
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
           "EXTRATO - CHEQUES E A RECEBER-ORDEM: ".
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
           "DATA-VECTO DOCUMENTO  PORTADOR   CART SITUACAO            VA
      -    "LOR SIT-CHEQUE     DATA-RECTO    JUROS-RCTO".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "QTDE CHEQUES       VALOR-TOTAL        VALOR RECEBIDOS     VA
      -    "LOR A RECEBER".
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
           MOVE "CHD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CRD001 CAD018 CGD010 CRD020 CHD010 CHD013.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "CHP060"            to logacess-programa
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
                    end-if
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CLIENTE-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
               WHEN GS-GRAVA-ANOTACAO-TRUE
                    PERFORM GRAVA-ANOTACAO
               WHEN GS-LISTA-ANOTACAO-TRUE
                    PERFORM CARREGA-LISTA-ANOTACAO
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
       GRAVA-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE GS-CLASS(1: 1) TO COD-COMPL-W(1: 1).
           MOVE GS-CLIENTE TO COD-COMPL-W(2: 8)
           MOVE COD-COMPL-W TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-W
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-W  TO COD-COMPL-CR200.
           MOVE GS-DATA-AGENDADA TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-MOVTO-I TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200
      *    ATUALIZA A LISTA DE ANOTACAO
           MOVE SPACES           TO GS-LINDET1
           MOVE DATA-MOVTO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET1(1: 15)
           MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E           TO GS-LINDET1(16: 10)
           MOVE USUARIO-CR200    TO GS-LINDET1(26: 11)
           MOVE SEQ-CR200        TO GS-LINDET1(36: 10)
           MOVE "DATA AGENDADA: " TO GS-LINDET1(50: 15)
           MOVE DATA-RETORNO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET1(65: 10)
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-W    TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
                 MOVE SPACES            TO GS-LINDET1
                 MOVE ANOTACAO-W        TO GS-LINDET1(16: 80)
                 MOVE "INSERE-LIST1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE CRD200 CRD201.
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
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
           MOVE ZEROS TO SEQ-WK.
           PERFORM GRAVA-CHEQUE.
           PERFORM GRAVA-A-RECEBER.
       GRAVA-CHEQUE SECTION.
           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CH10 CLASSIF-W
           MOVE GS-CLIENTE     TO CLIENTE-CH10.
           MOVE VECTO-INI      TO DATA-VENCTO-CH10.
           START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
                 MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
                 READ CHD010 NEXT RECORD AT END
                      MOVE "10" TO ST-CHD010
                 NOT AT END
                      IF CLIENTE-CH10 NOT = GS-CLIENTE OR
                         CLASS-CLIENTE-CH10 NOT = CLASSIF-W
                           MOVE "10" TO ST-CHD010
                      ELSE
                        IF DATA-VENCTO-CH10 > VECTO-FIM
                           MOVE "10" TO ST-CHD010
                        ELSE
                           MOVE DATA-VENCTO-CH10    TO GS-EXIBE-VECTO
                           MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           PERFORM MOVER-DADOS-WORK-CH
                           WRITE REG-WORK
                        END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       MOVER-DADOS-WORK-CH SECTION.
           MOVE CLASS-CLIENTE-CH10  TO CLASSIF-WK
           MOVE CLIENTE-CH10        TO CLIENTE-WK
           MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-WK
           ADD 1                    TO SEQ-WK
           EVALUATE CARTEIRA-CH10
              WHEN 1 MOVE "SIMP"    TO CARTEIRA-WK
              WHEN 2 MOVE "CAUC"    TO CARTEIRA-WK
              WHEN 3 MOVE "DESC"    TO CARTEIRA-WK
           END-EVALUATE

           MOVE NR-CHEQUE-CH10      TO DOCUMENTO-WK
           MOVE PORTADOR-CH10       TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*****"        TO NOME-PORT
           END-READ
           MOVE NOME-PORT           TO PORTADOR-WK
           MOVE SITUACAO-TIT-CH10   TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "******"       TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-CH10       TO SITUACAO-SIST-WK
           EVALUATE SITUACAO-CH10
             WHEN 0 MOVE "OK"           TO DESCR-SITUACAO-SIST-WK
             WHEN 1 MOVE "PARCIAL"      TO DESCR-SITUACAO-SIST-WK
             WHEN 2 MOVE "RECEBIDO"     TO DESCR-SITUACAO-SIST-WK
             WHEN 3 MOVE "ESTORNADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 4 MOVE "CANCELADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 5 MOVE "DEVOLVIDO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 6 MOVE "PROBLEMATICO" TO DESCR-SITUACAO-SIST-WK
           END-EVALUATE.
      *    SITUACAO = 0-OK        1-PARCIAL     2-RECEBIDO
      *               3-ESTONADO  4-CANCELADO   5-DEVOLVIDO
      *               6-PROBLEMATICO
           MOVE SITUACAO-TIT-CR01   TO SITUACAO-WK
           MOVE VALOR-CH10          TO VALOR-WK
           MOVE DATA-VENCTO-CH10    TO VENCTO-WK
           MOVE DATA-MOVTO-CH10     TO DATA-MOVTO-CH13
           MOVE SEQ-CH10            TO SEQ-CH13
           READ CHD013 INVALID KEY
                INITIALIZE REG-CHD013.
           MOVE DATA-RECTO-CH13     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV            TO DATA-BAIXA-WK.
           COMPUTE JUROS-WK = VLR-JUROS-CH13 +
                              VLR-MULTA-CH13 -
                              VLR-DESCONTO-CH13.
           MOVE "CHEQ"              TO ORIGEM-WK.
       GRAVA-A-RECEBER SECTION.
           INITIALIZE REG-CRD020
           MOVE GS-CLASS(1: 1) TO CLASS-CLIENTE-CR20
                                  CLASSIF-W
           MOVE GS-CLIENTE     TO CLIENTE-CR20
           MOVE VECTO-INI      TO DATA-VENCTO-CR20
           START CRD020 KEY IS NOT < ALT1-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                  NOT AT END
                       IF CLIENTE-CR20       NOT = GS-CLIENTE OR
                          CLASS-CLIENTE-CR20 NOT = CLASSIF-W
                          MOVE "10" TO ST-CRD020
                       ELSE
                         IF DATA-VENCTO-CR20 > VECTO-FIM
                            MOVE "10" TO ST-CRD020
                         ELSE
                            MOVE DATA-VENCTO-CR20    TO GS-EXIBE-VECTO
                            MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                            PERFORM MOVER-DADOS-WORK-REC
                            WRITE REG-WORK
                         END-IF
                       END-IF
                  END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK-REC SECTION.
           MOVE CLASS-CLIENTE-CR20      TO CLASSIF-WK
           MOVE CLIENTE-CR20            TO CLIENTE-WK
           ADD 1                        TO SEQ-WK
           EVALUATE CARTEIRA-CR20
              WHEN 1 MOVE "SIMP"        TO CARTEIRA-WK
              WHEN 2 MOVE "CAUC"        TO CARTEIRA-WK
              WHEN 3 MOVE "DESC"        TO CARTEIRA-WK
           END-EVALUATE

           MOVE NR-DOCTO-CR20           TO DOCUMENTO-WK
           MOVE PORTADOR-CR20           TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*****"            TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO PORTADOR-WK
           MOVE SITUACAO-TIT-CR20       TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE "******"           TO SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-CR20           TO SITUACAO-SIST-WK
           EVALUATE SITUACAO-CR20
             WHEN 0 MOVE "OK"           TO DESCR-SITUACAO-SIST-WK
             WHEN 1 MOVE "REC.PARC"     TO DESCR-SITUACAO-SIST-WK
             WHEN 2 MOVE "RECEBIDO"     TO DESCR-SITUACAO-SIST-WK
             WHEN 3 MOVE "ESTORNADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 4 MOVE "CANCELADO"    TO DESCR-SITUACAO-SIST-WK
             WHEN 5 MOVE "COBRANCA"     TO DESCR-SITUACAO-SIST-WK
           END-EVALUATE.

           MOVE SITUACAO-TIT-CR01       TO SITUACAO-WK
           MOVE DATA-VENCTO-CR20        TO VENCTO-WK
           IF SITUACAO-CR20 = 0 OR 1
              ADD VALOR-SALDO-CR20      TO TOT-VALOR-A-RECEB
           END-IF.
           MOVE "REC."                  TO ORIGEM-WK.
           MOVE DATA-RCTO-CR20          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                TO DATA-BAIXA-WK.
           MOVE JURO-RCTO-CR20          TO JUROS-WK.

       CARREGA-LISTA-ANOTACAO SECTION.
           OPEN INPUT CRD200 CRD201.
           IF ST-CRD200 = "35"
              CLOSE       CRD200
              OPEN OUTPUT CRD200
              CLOSE       CRD200
              OPEN INPUT  CRD200.

           IF ST-CRD201 = "35"
              CLOSE       CRD201
              OPEN OUTPUT CRD201
              CLOSE       CRD201
              OPEN INPUT  CRD201.

           MOVE COD-COMPL-CG10   TO COD-COMPL-CR200
           MOVE ZEROS            TO SEQ-CR200
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.

           PERFORM UNTIL ST-CRD200 = "10"
              READ CRD200 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD200
              NOT AT END
                  MOVE SPACES                   TO GS-LINDET1
                  IF COD-COMPL-CR200 <> COD-COMPL-CG10
                     MOVE "10"                  TO ST-CRD200
                  ELSE
                    MOVE DATA-MOVTO-CR200       TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV               TO DATA-E
                    MOVE DATA-E                 TO GS-LINDET1(1: 15)
                    MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                    MOVE ":"                    TO HORA-E(3: 1)
                    MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                    MOVE HORA-E                 TO GS-LINDET1(16: 10)
                    MOVE USUARIO-CR200          TO GS-LINDET1(26: 11)
                    MOVE SEQ-CR200              TO GS-LINDET1(36: 10)
                    MOVE "DATA AGENDADA: "      TO GS-LINDET1(50: 15)
                    MOVE DATA-RETORNO-CR200     TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV               TO DATA-E
                    MOVE DATA-E                 TO GS-LINDET1(65: 10)
                    MOVE "INSERE-LIST1"         TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    PERFORM CARREGA-CRD201
              END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.
       CARREGA-CRD201 SECTION.
           MOVE COD-COMPL-CG10   TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CG10 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO GS-LINDET1
                        MOVE ANOTACAO-CR201 TO GS-LINDET1(16: 80)
                        MOVE "INSERE-LIST1" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET1.
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
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
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

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
           MOVE VALOR-E           TO GS-LINDET(52: 14)
           MOVE DESCR-SITUACAO-SIST-WK TO GS-LINDET(66: 11).
           MOVE DATA-BAIXA-WK        TO DATA-E
           MOVE DATA-E            TO GS-LINDET(81: 11)
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(92: 14)
           MOVE ORIGEM-WK         TO GS-LINDET(106: 5).
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
                MOVE "SIT-CHEQUE" TO GS-DESCR-ORDEM
                MOVE ZEROS TO DESCR-SITUACAO-SIST-WK
                START WORK KEY IS NOT < DESCR-SITUACAO-SIST-WK
                      INVALID KEY MOVE "10" TO ST-WORK
           END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP060" TO DS-SET-NAME
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

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(01: 11)
           MOVE DOCUMENTO-WK      TO LINDET-REL(12: 11)
           MOVE PORTADOR-WK       TO LINDET-REL(23: 03)
           MOVE CARTEIRA-WK       TO LINDET-REL(34: 5)
           MOVE SITUACAO-WK       TO LINDET-REL(39: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(52: 12)
           MOVE DESCR-SITUACAO-SIST-WK TO LINDET-REL(66: 11)
           MOVE DATA-BAIXA-WK        TO DATA-E
           MOVE DATA-E            TO LINDET-REL(81: 11)
           MOVE JUROS-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(92: 14)
           MOVE ORIGEM-WK         TO LINDET-REL(106: 5)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

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
           move "CHP060"            to logacess-programa
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

           CLOSE CGD010 CRD001 CHD010 CHD013 CAD018 WORK CRD020.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
