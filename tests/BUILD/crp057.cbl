       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP057.
      *DATA: 05/05/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relatorio de contas a receber por vendedor-int.vencto
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento, e que esteja com a situacao = 0 (ok),
      *        por ordem de vendedor
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX020.
           COPY CAPX018.
           COPY CRPX020.
           COPY RCPX100.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = COD-VENDEDOR-WK COD-COMPL-WK
                                SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK =
                    DATA-MOVTO-WK COD-VENDEDOR-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW020.
       COPY CAPW018.
       COPY CRPW020.
       COPY RCPW100.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  COD-COMPL-WK        PIC 9(9).
           05  SEQ-WK              PIC 9(5).
           05  DATA-MOVTO-WK       PIC 9(8).
           05  NOME-CLIEN-WK       PIC X(11).
           05  PORTADOR-WK         PIC X(10).
           05  NR-DOCTO-WK         PIC X(10).
           05  COD-VENDEDOR-WK     PIC 9(6).
           05  VENDEDOR-WK         PIC X(15).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  PARCELA-WK          PIC 9(02).
           05  QT-PARCELA-WK       PIC 9(02).
           05  TIPO-WK             PIC x(07).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP057.CPB".
           COPY "CRP057.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-INV        PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-FIM        PIC 9(8)     VALUE ZEROS.
           05  COD-VENDEDOR-ANT      PIC 9(6)     VALUE ZEROS.
           05  TOTAL-VENDEDOR        PIC 9(8)V99  VALUE ZEROS.
           05  TOTAL-GERAL           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(15)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(12)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(69)   VALUE
           "RELACAO DE CONTAS A RECEBER-ORDEM: VENDEDOR".
           05  FILLER              PIC X(17)   VALUE SPACES.
           05  DESC-REL            PIC X(16).
           05  DATA-MOVTO-REL      PIC 99/99/9999.
           05  FILLER              PIC x(05) VALUE " ATE ".
           05  DATA-MOVTO-RELF     PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)  VALUE
           "NOME-CLIENTE         NR-DOCTO PARC. VENDEDOR        TIPO
      -    "PORTADOR   DATA-VECTO         VALOR        TOTAL".

       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "QTDE-TITULOS: ".
           05  QTDE-TITULO-REL     PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER              PIC X(12)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "TOTAL GERAL: ".
           05  TOTAL-GERAL-REL     PIC ZZ.ZZZ.ZZZ,ZZ.

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
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD018 CGD001 CGD010 CRD020 CGD020 RCD100.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP057"            to logacess-programa
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
                    MOVE ZEROS  TO COD-VENDEDOR-ANT
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LER-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CARTAO SECTION.
           MOVE GS-CARTAO      TO CODIGO-CG20
           READ CGD020 INVALID KEY
               MOVE SPACES     TO NOME-CG20.

           MOVE NOME-CG20      TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-CARTAO
           PERFORM LER-CARTAO.


       LER-VENDEDOR SECTION.
           MOVE GS-VENDEDOR          TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01            TO GS-DESC-VENDEDOR.

       POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-VENDEDOR
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           MOVE GS-TIPO(1:1) TO AUX-TIPO

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS          TO TOTAL-VENDEDOR
                                  TOTAL-GERAL
      *> Data Inicial
           MOVE GS-DATA-INI    TO DATA-INV
                                  DATA-MOVTO-REL
           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV       TO DATA-MOVTO-INV

      *> Data Final
           MOVE GS-DATA-FIM    TO DATA-INV
                                  DATA-MOVTO-RELF
           CALL "GRIDAT2"   USING DATA-INV
           MOVE DATA-INV       TO DATA-MOVTO-FIM

           INITIALIZE REG-CRD020

           EVALUATE GS-ACP-TIPO
               WHEN 1 MOVE DATA-MOVTO-INV TO DATA-MOVTO-CR20
                      MOVE ZEROS          TO SITUACAO-CR20
                      START CRD020 KEY IS NOT < DATA-MOVTO-CR20
                                                             INVALID KEY
                           MOVE "10" TO ST-CRD020
                      END-START
                      PERFORM UNTIL ST-CRD020 = "10"
                           READ CRD020 NEXT RECORD AT END
                                MOVE "10" TO ST-CRD020
                           NOT AT END
                                IF DATA-MOVTO-CR20 > DATA-MOVTO-FIM
                                   MOVE "10" TO ST-CRD020
                                ELSE
                                   PERFORM CONTINUAR
                                END-IF
                           END-READ
                      END-PERFORM
               WHEN 2 MOVE DATA-MOVTO-INV TO DATAVEN-REC
                      START RCD100 KEY IS NOT < DATAVEN-REC INVALID KEY
                           MOVE "10" TO ST-RCD100
                      END-START
                      PERFORM UNTIL ST-RCD100 = "10"
                           READ RCD100 NEXT RECORD AT END
                                MOVE "10" TO ST-RCD100
                           NOT AT END
                                IF DATAVEN-REC  > DATA-MOVTO-FIM
                                   MOVE "10" TO ST-RCD100
                                ELSE
                                   INITIALIZE REG-CRD020
                                   MOVE 0         TO CLASS-CLIENTE-CR20
                                   MOVE ALBUM-REC TO CLIENTE-CR20

                                   START CRD020 KEY IS NOT LESS
                                                  CHAVE-CR20 INVALID KEY
                                        MOVE "10" TO ST-CRD020
                                   END-START
                                   PERFORM UNTIL ST-CRD020 = "10"
                                        READ CRD020 NEXT AT END
                                             MOVE "10" TO ST-CRD020
                                        NOT AT END
                                             IF ALBUM-REC <>
                                                CLIENTE-CR20
                                                MOVE "10" TO ST-CRD020
                                             ELSE
                                                PERFORM CONTINUAR
                                             END-IF
                                        END-READ
                                   END-PERFORM
                                END-IF
                           END-READ
                      END-PERFORM
           END-EVALUATE

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CONTINUAR SECTION.
           IF SITUACAO-CR20 = 0 OR 1
              IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
                 IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                    IF GS-CARTAO = 0 OR CARTAO-CRED-CR20
                       IF GS-LOTE = 0 OR LOTE-CR20
                          PERFORM MOVER-DADOS-WORK
                          MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                          WRITE REG-WORK
                          END-WRITE
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
       CONTINUAR-FIM.
           EXIT.

       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CR20    TO VENCTO-WK
                                       GS-EXIBE-VENCTO
           MOVE COD-COMPL-CR20      TO COD-COMPL-WK
                                       COD-COMPL-CG10.
           READ CGD010 INVALID KEY
                MOVE "*******"      TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10      TO NOME-CLIEN-WK.
           MOVE VENDEDOR-CR20       TO CODIGO-CG01
                                       COD-VENDEDOR-WK.
           READ CGD001 INVALID KEY
                MOVE "******"       TO NOME-CG01.
           MOVE NOME-CG01           TO VENDEDOR-WK.
           MOVE SEQ-CR20            TO SEQ-WK
           MOVE DATA-MOVTO-CR20     TO DATA-MOVTO-WK
           MOVE PORTADOR-CR20       TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"       TO NOME-PORT.
           MOVE NOME-PORT           TO PORTADOR-WK.
           MOVE NR-DOCTO-CR20       TO NR-DOCTO-WK.
           MOVE VALOR-SALDO-CR20    TO VALOR-WK
           EVALUATE TIPO-DOCTO-CR20
               WHEN 0 MOVE "0-Dupli" TO TIPO-WK
               WHEN 1 MOVE "1-Nt.Pr" TO TIPO-WK
               WHEN 2 MOVE "2-Org.E" TO TIPO-WK
               WHEN 3 MOVE "3-Debit" TO TIPO-WK
               WHEN 4 MOVE "4-Cart." TO TIPO-WK
           END-EVALUATE
           MOVE NR-PARC-CR20        TO PARCELA-WK
           MOVE tot-PARC-CR20     TO QT-PARCELA-WK.

       CARREGA-LISTA SECTION.
           MOVE ZEROS  TO COD-VENDEDOR-ANT
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK
           MOVE ZEROS TO GS-TOTAL-GERAL GS-QTDE-TITULOS TOTAL-W.
           MOVE ZEROS TO TOTAL-VENDEDOR TOTAL-GERAL COD-VENDEDOR-ANT.
           MOVE SPACES TO VENDEDOR-WK.
           MOVE ZEROS  TO DATA-MOVTO-WK.
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-VENDEDOR.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-LINDET SECTION.
           IF COD-VENDEDOR-ANT NOT = COD-VENDEDOR-WK
              PERFORM CABECALHO-VENDEDOR.
           MOVE COD-VENDEDOR-WK TO COD-VENDEDOR-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE COD-COMPL-WK(2: 8) TO GS-LINDET(1: 9)
           MOVE NOME-CLIEN-WK     TO GS-LINDET(10: 12)
           MOVE NR-DOCTO-WK       TO GS-LINDET(22: 09)
           MOVE PARCELA-WK        TO GS-LINDET(31:2)
           MOVE "/"               TO GS-LINDET(33:1)
           MOVE QT-PARCELA-WK     TO GS-LINDET(34:2)
           MOVE VENDEDOR-WK       TO GS-LINDET(37: 16)
           MOVE TIPO-WK           TO GS-LINDET(53:8)
           MOVE PORTADOR-WK       TO GS-LINDET(61: 11)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(72: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(83: 14)
           ADD VALOR-WK           TO TOTAL-VENDEDOR GS-TOTAL-GERAL.
           ADD 1                  TO GS-QTDE-TITULOS
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(97: 13).
       CABECALHO-VENDEDOR SECTION.
           PERFORM TOTALIZA-VENDEDOR.
           MOVE SPACES        TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE COD-VENDEDOR-WK   TO COD-VENDEDOR-ANT CODIGO-CG01
                                     GS-LINDET(1: 6)
           MOVE "-"           TO GS-LINDET(7: 1)
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01     TO GS-LINDET(8: 30).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       TOTALIZA-VENDEDOR SECTION.
           MOVE SPACES        TO GS-LINDET.
           IF COD-VENDEDOR-ANT <> SPACES
              MOVE "TOTAL VENDEDOR . . . " TO GS-LINDET(1: 17)
              MOVE TOTAL-VENDEDOR     TO VALOR-E
              MOVE VALOR-E            TO GS-LINDET(95: 13)
              MOVE "INSERE-LIST"      TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              ADD TOTAL-VENDEDOR      TO TOTAL-GERAL
              MOVE ZEROS              TO TOTAL-VENDEDOR.
       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS  TO COD-VENDEDOR-ANT.
           MOVE ZEROS  TO TOTAL-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP057" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W LIN

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO VENDEDOR-WK.
           MOVE ZEROS  TO DATA-MOVTO-WK
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-VENDEDOR-IMPR.
           MOVE GS-TOTAL-GERAL    TO TOTAL-GERAL-REL.
           MOVE GS-QTDE-TITULOS   TO QTDE-TITULO-REL
           WRITE REG-RELAT FROM LINTOT AFTER 2.
      *    MOVE SPACES TO REG-RELAT.
      *    WRITE REG-RELAT AFTER PAGE.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           IF VENDEDOR-WK NOT = COD-VENDEDOR-ANT
              PERFORM CABECALHO-VENDEDOR-REL.

           MOVE VENDEDOR-WK        TO COD-VENDEDOR-ANT.
           MOVE COD-COMPL-WK(2: 8) TO LINDET-REL(1: 9)
           MOVE NOME-CLIEN-WK      TO LINDET-REL(10: 12)
           MOVE NR-DOCTO-WK        TO LINDET-REL(22: 09)
           MOVE PARCELA-WK         TO LINDET-REL(31:2)
           MOVE "/"                TO LINDET-REL(33:1)
           MOVE QT-PARCELA-WK      TO LINDET-REL(34:2)
           MOVE VENDEDOR-WK        TO LINDET-REL(37: 16)
           MOVE TIPO-WK            TO LINDET-REL(53:8)
           MOVE PORTADOR-WK        TO LINDET-REL(61: 11)
           MOVE VENCTO-WK          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO LINDET-REL(72: 11)
           MOVE VALOR-WK           TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(83: 14)
           ADD VALOR-WK            TO TOTAL-W.
           ADD VALOR-WK            TO TOTAL-VENDEDOR
           MOVE TOTAL-W            TO VALOR-E
           MOVE VALOR-E            TO LINDET-REL(97: 13)

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.

       CABECALHO-VENDEDOR-REL SECTION.
           PERFORM TOTALIZA-VENDEDOR-IMPR.
           MOVE SPACES        TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56
              PERFORM CABECALHO.
           MOVE COD-VENDEDOR-WK  TO COD-VENDEDOR-ANT CODIGO-CG01
                                 LINDET-REL(1: 6)
           MOVE "-"           TO LINDET-REL(7: 1)
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.

           MOVE NOME-CG01     TO LINDET-REL(8: 30).
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.

           IF LIN > 56
              PERFORM CABECALHO.

           MOVE SPACES TO LINDET.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.

           IF LIN > 56
              PERFORM CABECALHO.

       TOTALIZA-VENDEDOR-IMPR SECTION.
           MOVE SPACES TO LINDET-REL.
           IF COD-VENDEDOR-ANT <> SPACES
              MOVE "TOTAL VENDEDOR . . . " TO LINDET-REL(1: 17)
              MOVE TOTAL-VENDEDOR     TO VALOR-E
              MOVE VALOR-E            TO LINDET-REL(95: 13)
              WRITE REG-RELAT FROM LINDET
              ADD 1 TO LIN
              IF LIN > 56 PERFORM CABECALHO
              END-IF
              MOVE ZEROS              TO TOTAL-VENDEDOR
              MOVE SPACES             TO LINDET-REL.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           EVALUATE GS-ACP-TIPO
               WHEN 1 MOVE "DATA MOVIMENTO: " TO DESC-REL
               WHEN 2 MOVE "DATA VENDA....: " TO DESC-REL
           END-EVALUATE

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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP057"            to logacess-programa
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

           CLOSE CRD020 CAD018 CGD010 CGD001 CGD020 RCD100
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
