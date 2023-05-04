       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP055.
      *DATA: 20/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RECEBIMENTOS DE TÍTULOS
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, cliente
      *        e VCTO/vcto.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CAPX018.
           COPY CBPX001.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CLASSIF-WK
                                           CLIENTE-WK
                                           SEQ-WK
                                           DATA-RCTO-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK
                                                   VALOR-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                     CLIENTE-WK TIPO-DOCUMENTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CGPW011.
       COPY CAPW018.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CBPW001.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  SEQ-WK              PIC 9(5).
           05  NOME-CLIEN-WK       PIC X(20).
           05  NOSSO-NR-WK         PIC X(15).
           05  PORTADOR-WK         PIC X(10).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  JUROS-WK            PIC 9(6)V99.
           05  MULTA-WK            PIC 9(6)V99.
           05  DESCONTO-WK         PIC 9(6)V99.
           05  VLR-LIQ-WK          PIC 9(8)V99.
           05  DATA-RCTO-WK        PIC 9(8).
           05  TIPO-DOCUMENTO-WK   PIC X(4).
           05  RESPONSAVEL-WK      PIC X(5).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP055.CPB".
           COPY "CRP055.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  LIN                   PIC 99       VALUE ZEROS.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(15)    VALUE SPACES.
           05  NOME-CLIEN-ANT        PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC X(10)    VALUE ZEROS.
           05  ALBUM-ANT             PIC 9(8)     VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
           05  TOT-VALOR-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-JUROS-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-MULTA-PARCIAL     PIC 9(09)V99 VALUE ZEROS.
           05  TOT-DESC-PARCIAL      PIC 9(09)V99 VALUE ZEROS.
           05  TOT-LIQ-PARCIAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-VALOR-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-JUROS-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-MULTA-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-DESC-TOTAL        PIC 9(09)V99 VALUE ZEROS.
           05  TOT-LIQ-TOTAL         PIC 9(09)V99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RECEBIMENTOS DE TITULOS     ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.VENCTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB02B.
           05  FILLER              PIC X(16)
               VALUE "FORMA DE PAGTO: ".
           05  FORMA-PGTO-REL      PIC X(20).
           05  FILLER              PIC X(16)
               VALUE "CONTA CORRENTE: ".
           05  CONTA-REL           PIC 9(06).
           05  FILLER              PIC X(01).
           05  NOME-CONTA-REL      PIC X(30).
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "NOME-CLIENTE         ALBUM      CART    DATA            VALO
      -    "R      JUROS      MULTA   DESCONTO   VLR-LIQUIDO".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       01  DET-TOTALIZA.
           05 DET-DESCRICAO        PIC X(47) VALUE SPACES.
           05 DET-TOT-VLR          PIC ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-JRS          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-MUL          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 DET-TOT-DES          PIC ZZZ.ZZ9,99     BLANK WHEN ZEROS.
           05 DET-TOT-LIQ          PIC ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.

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
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CBD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD001
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD018 CGD010 CGD011 CBD001.
           OPEN I-O CRD020 CRD020B.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: " TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23:2)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP055"            to logacess-programa
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
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-ACHAR-VENCTO-TRUE
                    PERFORM ACHAR-VENCTO
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-VERIFICA-SITUACAO-TRUE
                    PERFORM VERIFICA-SITUACAO
               WHEN GS-REVERTE-TRUE
                    PERFORM REVERTER-SITUACAO
               WHEN GS-REGRAVA-DADOS-TRUE
                    PERFORM CHAMA-ALTERACAO
               WHEN GS-CALCULA-VLR-LIQ-TRUE
                    PERFORM CALCULA-VLR-LIQUIDO
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
                   PERFORM 140-EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE TITULAR-CB01 TO GS-DESC-CONTA
                   REFRESH-OBJECT PRINCIPAL
              END-READ
           ELSE
              MOVE SPACES TO GS-DESC-CONTA
              REFRESH-OBJECT PRINCIPAL.

       POPUP-CONTA SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CBP001T"
           MOVE PASSAR-STRING(49:6) TO GS-ACP-CONTA
           PERFORM LER-CONTA.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".
       ACHAR-VENCTO SECTION.
           MOVE DATA-DIA-I     TO GRTIME-DATE.
           MOVE 2              TO GRTIME-TYPE.
           MOVE 1              TO GRTIME-FUNCTION.
           MOVE 7              TO GRTIME-DAYS.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE 01010001       TO GS-VENCTO-INI.
           CALL "GRIDAT1" USING GRTIME-DATE-FINAL.
           MOVE GRTIME-DATE-FINAL TO GS-VENCTO-FIM.

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

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-TIPO-DOCUMENTO(1:1) TO AUX-TIPO

           MOVE GS-VENCTO-INI          TO DATA-INV
                                          VENCTO-INI-ANT
                                          VENCTO-INI-REL

           CALL "GRIDAT2"           USING DATA-INV
           MOVE DATA-INV               TO VENCTO-INI

           MOVE GS-VENCTO-FIM          TO DATA-INV
                                          VENCTO-FIM-ANT
                                          VENCTO-FIM-REL

           CALL "GRIDAT2"           USING DATA-INV
           MOVE DATA-INV               TO VENCTO-FIM.

           INITIALIZE REG-CRD020

           IF GS-OPCAO = 1
               INITIALIZE REG-CRD020
               MOVE VENCTO-INI  TO DATA-VENCTO-CR20
               MOVE 1           TO SITUACAO-CR20
               MOVE ZEROS       TO CLIENTE-CR20 CLASS-CLIENTE-CR20
               START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                     MOVE "10" TO ST-CRD020
               END-START
               PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT RECORD AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF SITUACAO-CR20 <> 1
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF DATA-VENCTO-CR20 > VENCTO-FIM
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                   PERFORM MOVER-DADOS-WORK
                                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                   WRITE REG-WORK
                                END-IF
                             END-IF
                          END-IF
                     END-READ
               END-PERFORM
               INITIALIZE REG-CRD020
               MOVE VENCTO-INI  TO DATA-VENCTO-CR20
               MOVE 2           TO SITUACAO-CR20
               MOVE ZEROS       TO CLIENTE-CR20 CLASS-CLIENTE-CR20
               START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                     MOVE "10" TO ST-CRD020
               END-START
               PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT RECORD AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF SITUACAO-CR20 <> 2
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF DATA-VENCTO-CR20 > VENCTO-FIM
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                   PERFORM MOVER-DADOS-WORK
                                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                   WRITE REG-WORK
                                END-IF
                             END-IF
                          END-IF
                     END-READ
               END-PERFORM
           ELSE
               INITIALIZE REG-CRD020B
               MOVE VENCTO-INI      TO DATA-RCTO-CR20B
               MOVE ZEROS           TO SEQ-CR20B
               START CRD020B KEY IS NOT < ALT6-CR20B INVALID KEY
                     MOVE "10" TO ST-CRD020B
               END-START
               PERFORM UNTIL ST-CRD020B = "10"
                     READ CRD020B NEXT RECORD AT END
                          MOVE "10" TO ST-CRD020B
                     NOT AT END
                          IF DATA-RCTO-CR20B > VENCTO-FIM
                             MOVE "10" TO ST-CRD020B
                          ELSE
                             IF GS-FORMA-PAGTO-D(1:1) = "9" OR
                                GS-FORMA-PAGTO-D = FORMA-PAGTO-CR20B
                                IF GS-ACP-CONTA = 0 OR GS-ACP-CONTA =
                                   CONTA-CORRENTE-CR20B
                                   MOVE CLASS-CLIENTE-CR20B  TO
                                        CLASS-CLIENTE-CR20
                                   MOVE CLIENTE-CR20B  TO CLIENTE-CR20
                                   MOVE SEQ-CR20B      TO SEQ-CR20
                                   READ CRD020 NOT INVALID KEY
                                      IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                         PERFORM MOVER-DADOS-WORK
                                         MOVE "TELA-AGUARDA1" TO
                                                 DS-PROCEDURE
                                         PERFORM CALL-DIALOG-SYSTEM
                                         WRITE REG-WORK
                                      END-IF
                                   END-READ
                                END-IF
                             END-IF
                          END-IF
                     END-READ
               END-PERFORM.

           CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           IF GS-OPCAO = 1
               MOVE DATA-VENCTO-CR20    TO VENCTO-WK
                                           GS-EXIBE-VENCTO
           ELSE
              MOVE DATA-RCTO-CR20B      TO VENCTO-WK
                                           GS-EXIBE-VENCTO.

           MOVE CLASS-CLIENTE-CR20      TO CLASSIF-WK
                                           CLASSIF-CG10
                                           CLASSIF-CG11.
           MOVE CLIENTE-CR20            TO CLIENTE-WK
                                           CODIGO-CG10
                                           CODIGO-CG11.
           READ CGD010 INVALID KEY
                MOVE "*******"          TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10          TO NOME-CLIEN-WK.
           READ CGD011 INVALID KEY
                MOVE ZEROS              TO CIDADE1-CG11.

           MOVE SEQ-CR20                TO SEQ-WK
           MOVE PORTADOR-CR20           TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"           TO NOME-PORT.

           MOVE NOME-PORT               TO PORTADOR-WK.
           MOVE OUTRO-DOCTO-CR20        TO NOSSO-NR-WK.
           IF GS-OPCAO = 1
              MOVE VALOR-TOT-CR20       TO VALOR-WK
           ELSE
              MOVE VALOR-BAIXA-CR20B    TO VALOR-WK
           END-IF

      *    MOVE VALOR-LIQ-CR20B         TO VLR-LIQ-WK

           COMPUTE VLR-LIQ-WK = VALOR-WK         +
                                JURO-RCTO-CR20B  +
                                MULTA-RCTO-CR20B -
                                DESCONTO-CR20B

           MOVE JURO-RCTO-CR20B         TO JUROS-WK
           MOVE MULTA-RCTO-CR20B        TO MULTA-WK
           MOVE DESCONTO-CR20B          TO DESCONTO-WK
           MOVE DATA-RCTO-CR20B         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                TO DATA-RCTO-WK
           EVALUATE TIPO-DOCTO-CR20
             WHEN 0 MOVE "0-DU"         TO TIPO-DOCUMENTO-WK
             WHEN 1 MOVE "1-NT"         TO TIPO-DOCUMENTO-WK
             WHEN 2 MOVE "2-OR"         TO TIPO-DOCUMENTO-WK
             WHEN 3 MOVE "3-DE"         TO TIPO-DOCUMENTO-WK
             WHEN 4 MOVE "4-CA"         TO TIPO-DOCUMENTO-WK
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK
                      TOT-VALOR-PARCIAL
                      TOT-JUROS-PARCIAL
                      TOT-MULTA-PARCIAL
                      TOT-DESC-PARCIAL
                      TOT-LIQ-PARCIAL
                      TOT-VALOR-TOTAL
                      TOT-JUROS-TOTAL
                      TOT-MULTA-TOTAL
                      TOT-DESC-TOTAL
                      TOT-LIQ-TOTAL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM

           PERFORM TOTALIZAR-DADOS
           PERFORM MOVER-TOTAL-FINAL

           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "VENCTO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK
                START WORK KEY IS NOT < VENCTO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-CLIEN-WK
                START WORK KEY IS NOT < NOME-CLIEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "ALBUM" TO GS-DESCR-ORDEM
                INITIALIZE CLIENTE-WK TIPO-DOCUMENTO-WK
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "VCTO/VALOR" TO GS-DESCR-ORDEM
                MOVE ZEROS TO VENCTO-WK VALOR-WK
                START WORK KEY IS NOT < ALT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZAR-DADOS
                    PERFORM LINHA-BRANCO
             WHEN 2
              IF NOME-CLIEN-ANT  NOT = SPACES
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZAR-DADOS
                    PERFORM LINHA-BRANCO
             WHEN 3
              IF ALBUM-ANT NOT = ZEROS
                 IF ALBUM-ANT NOT = CLIENTE-WK
                    PERFORM TOTALIZAR-DADOS
                    PERFORM LINHA-BRANCO
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZAR-DADOS
                    PERFORM LINHA-BRANCO
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       TOTALIZAR-DADOS SECTION.
           MOVE "Totalizador ..."  TO DET-DESCRICAO
           MOVE TOT-VALOR-PARCIAL  TO DET-TOT-VLR
           MOVE TOT-JUROS-PARCIAL  TO DET-TOT-JRS
           MOVE TOT-MULTA-PARCIAL  TO DET-TOT-MUL
           MOVE TOT-DESC-PARCIAL   TO DET-TOT-DES
           MOVE TOT-LIQ-PARCIAL    TO DET-TOT-LIQ

           ADD  TOT-VALOR-PARCIAL  TO TOT-VALOR-TOTAL
           ADD  TOT-JUROS-PARCIAL  TO TOT-JUROS-TOTAL
           ADD  TOT-MULTA-PARCIAL  TO TOT-MULTA-TOTAL
           ADD  TOT-DESC-PARCIAL   TO TOT-DESC-TOTAL
           ADD  TOT-LIQ-PARCIAL    TO TOT-LIQ-TOTAL

           MOVE DET-TOTALIZA       TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE  TOT-VALOR-PARCIAL
                       TOT-JUROS-PARCIAL
                       TOT-MULTA-PARCIAL
                       TOT-DESC-PARCIAL
                       TOT-LIQ-PARCIAL.

       TOTALIZAR-DADOS-REL SECTION.
           MOVE "Totalizador ..."  TO DET-DESCRICAO
           MOVE TOT-VALOR-PARCIAL  TO DET-TOT-VLR
           MOVE TOT-JUROS-PARCIAL  TO DET-TOT-JRS
           MOVE TOT-MULTA-PARCIAL  TO DET-TOT-MUL
           MOVE TOT-DESC-PARCIAL   TO DET-TOT-DES
           MOVE TOT-LIQ-PARCIAL    TO DET-TOT-LIQ

           ADD  TOT-VALOR-PARCIAL  TO TOT-VALOR-TOTAL
           ADD  TOT-JUROS-PARCIAL  TO TOT-JUROS-TOTAL
           ADD  TOT-MULTA-PARCIAL  TO TOT-MULTA-TOTAL
           ADD  TOT-DESC-PARCIAL   TO TOT-DESC-TOTAL
           ADD  TOT-LIQ-PARCIAL    TO TOT-LIQ-TOTAL

           MOVE DET-TOTALIZA       TO REG-RELAT
           WRITE REG-RELAT

           INITIALIZE  TOT-VALOR-PARCIAL
                       TOT-JUROS-PARCIAL
                       TOT-MULTA-PARCIAL
                       TOT-DESC-PARCIAL
                       TOT-LIQ-PARCIAL.

       MOVER-TOTAL-FINAL SECTION.
           MOVE SPACES           TO GS-LINDET
           MOVE "INSERE-LIST"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "Totalizador Geral ..."  TO DET-DESCRICAO
           MOVE TOT-VALOR-TOTAL  TO DET-TOT-VLR
           MOVE TOT-JUROS-TOTAL  TO DET-TOT-JRS
           MOVE TOT-MULTA-TOTAL  TO DET-TOT-MUL
           MOVE TOT-DESC-TOTAL   TO DET-TOT-DES
           MOVE TOT-LIQ-TOTAL    TO DET-TOT-LIQ

           MOVE DET-TOTALIZA       TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-TOTAL-FINAL-REL SECTION.
           MOVE SPACES           TO REG-RELAT
           WRITE REG-RELAT

           MOVE "Totalizador Geral ..."  TO DET-DESCRICAO
           MOVE TOT-VALOR-TOTAL  TO DET-TOT-VLR
           MOVE TOT-JUROS-TOTAL  TO DET-TOT-JRS
           MOVE TOT-MULTA-TOTAL  TO DET-TOT-MUL
           MOVE TOT-DESC-TOTAL   TO DET-TOT-DES
           MOVE TOT-LIQ-TOTAL    TO DET-TOT-LIQ

           MOVE DET-TOTALIZA     TO REG-RELAT
           WRITE REG-RELAT.

       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO PORTADOR-ANT NOME-CLIEN-ANT.
           MOVE ZEROS TO VENCTO-ANT ALBUM-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-CLIEN-WK     TO NOME-CLIEN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT
           MOVE CLIENTE-WK        TO ALBUM-ANT.
       LINHA-BRANCO SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE SPACES TO GS-LINDET(1:21)
           STRING NOME-CLIEN-WK INTO GS-LINDET(1:21)
           MOVE CLIENTE-WK        TO GS-LINDET(22: 11)
           MOVE TIPO-DOCUMENTO-WK TO GS-LINDET(33: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(38: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(49: 14)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(63: 11)
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(74: 11)
           MOVE DESCONTO-WK       TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINDET(85: 11)
           MOVE VLR-LIQ-WK        TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(96: 14)
           MOVE CLASSIF-WK        TO GS-LINDET(110: 1)
           MOVE CLIENTE-WK        TO GS-LINDET(111: 8)
           MOVE SEQ-WK            TO GS-LINDET(119: 5)

           ADD VALOR-WK           TO TOT-VALOR-PARCIAL
           ADD JUROS-WK           TO TOT-JUROS-PARCIAL
           ADD MULTA-WK           TO TOT-MULTA-PARCIAL
           ADD DESCONTO-WK        TO TOT-DESC-PARCIAL
           ADD VLR-LIQ-WK         TO TOT-LIQ-PARCIAL.

       VERIFICA-SITUACAO SECTION.
           MOVE GS-LINDET(110: 9) TO COD-COMPL-CR20.
           MOVE GS-LINDET(119: 5) TO SEQ-CR20.
           READ CRD020.
           MOVE SITUACAO-CR20 TO GS-SITUACAO.
       REVERTER-SITUACAO SECTION.
           MOVE 0              TO SITUACAO-CR20.
           MOVE ZEROS          TO MULTA-RCTO-CR20
                                  JURO-RCTO-CR20
                                  DESCONTO-CR20
                                  VALOR-LIQ-CR20
                                  DATA-RCTO-CR20.
           MOVE VALOR-TOT-CR20 TO VALOR-SALDO-CR20
           REWRITE REG-CRD020.
           MOVE CLASS-CLIENTE-CR20 TO CLASSIF-WK
           MOVE CLIENTE-CR20       TO CLIENTE-WK
           MOVE SEQ-CR20           TO SEQ-WK.
           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
                 MOVE ZEROS TO DATA-RCTO-WK JUROS-WK MULTA-WK
                               DESCONTO-WK VLR-LIQ-WK
                 REWRITE REG-WORK
                 END-REWRITE
                 PERFORM MOVER-DADOS
                 MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-READ.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(110: 1) TO CLASSIF-WK
           MOVE GS-LINDET(111: 8) TO CLIENTE-WK
           MOVE GS-LINDET(119: 5) TO SEQ-WK
           READ WORK INVALID KEY INITIALIZE REG-WORK.
           MOVE DATA-RCTO-WK      TO GS-DATA-RCTO
           MOVE VALOR-WK          TO GS-VALOR-TITULO
           MOVE JUROS-WK          TO GS-JUROS
           MOVE MULTA-WK          TO GS-MULTA
           MOVE DESCONTO-WK       TO GS-DESCONTO
           MOVE VLR-LIQ-WK        TO GS-VALOR-LIQUIDO
           MOVE "REFRESH-WIN3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULA-VLR-LIQUIDO SECTION.
           COMPUTE GS-VALOR-LIQUIDO = (GS-VALOR-TITULO + GS-JUROS +
                   GS-MULTA) - GS-DESCONTO.
       CHAMA-ALTERACAO SECTION.
           MOVE CLASSIF-WK        TO CLASS-CLIENTE-CR20
           MOVE CLIENTE-WK        TO CLIENTE-CR20
           MOVE SEQ-WK            TO SEQ-CR20
           READ CRD020 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                 MOVE GS-JUROS         TO JURO-RCTO-CR20
                 MOVE GS-MULTA         TO MULTA-RCTO-CR20
                 MOVE GS-DESCONTO      TO DESCONTO-CR20
                 MOVE GS-VALOR-LIQUIDO TO VALOR-LIQ-CR20
                 MOVE GS-DATA-RCTO     TO DATA-INV
                 CALL "GRIDAT2" USING DATA-INV
                 MOVE DATA-INV         TO DATA-RCTO-CR20
                 MOVE 2                TO SITUACAO-CR20
                 REWRITE REG-CRD020
                 END-REWRITE
                 MOVE GS-DATA-RCTO     TO  DATA-RCTO-WK
                 MOVE GS-VALOR-TITULO  TO  VALOR-WK
                 MOVE GS-JUROS         TO  JUROS-WK
                 MOVE GS-MULTA         TO  MULTA-WK
                 MOVE GS-DESCONTO      TO  DESCONTO-WK
                 MOVE GS-VALOR-LIQUIDO TO  VLR-LIQ-WK
                 REWRITE REG-WORK
                 END-REWRITE
                 PERFORM MOVER-DADOS
                 MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-READ.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP055" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-FORMA-PAGTO-D  TO FORMA-PGTO-REL
           MOVE GS-ACP-CONTA      TO CONTA-REL
           MOVE GS-DESC-CONTA     TO NOME-CONTA-REL


           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL

           INITIALIZE REG-WORK
                      TOT-VALOR-PARCIAL
                      TOT-JUROS-PARCIAL
                      TOT-MULTA-PARCIAL
                      TOT-DESC-PARCIAL
                      TOT-LIQ-PARCIAL
                      TOT-VALOR-TOTAL
                      TOT-JUROS-TOTAL
                      TOT-MULTA-TOTAL
                      TOT-DESC-TOTAL
                      TOT-LIQ-TOTAL
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-RELATORIO
               END-READ
           END-PERFORM.

           PERFORM TOTALIZAR-DADOS-REL
           PERFORM MOVER-TOTAL-FINAL-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZAR-DADOS-REL
                    PERFORM LINHA-BRANCO-REL
             WHEN 2
              IF NOME-CLIEN-ANT NOT = SPACES
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZAR-DADOS-REL
                    PERFORM LINHA-BRANCO-REL
             WHEN 3
              IF ALBUM-ANT NOT = ZEROS
                 IF ALBUM-ANT NOT = CLIENTE-WK
                    PERFORM TOTALIZAR-DADOS-REL
                    PERFORM LINHA-BRANCO-REL
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZAR-DADOS-REL
                    PERFORM LINHA-BRANCO-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE SPACES            TO LINDET-REL(1:21)
           STRING NOME-CLIEN-WK INTO LINDET-REL(1:21)
           MOVE CLIENTE-WK        TO LINDET-REL(22: 11)
           MOVE TIPO-DOCUMENTO-WK TO LINDET-REL(33: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO LINDET-REL(38: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(49: 14)
           MOVE JUROS-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(63: 11)
           MOVE MULTA-WK          TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(74: 11)
           MOVE DESCONTO-WK       TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(85: 11)
           MOVE VLR-LIQ-WK        TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(96: 14)

           ADD VALOR-WK           TO TOT-VALOR-PARCIAL
           ADD JUROS-WK           TO TOT-JUROS-PARCIAL
           ADD MULTA-WK           TO TOT-MULTA-PARCIAL
           ADD DESCONTO-WK        TO TOT-DESC-PARCIAL
           ADD VLR-LIQ-WK         TO TOT-LIQ-PARCIAL.

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       LINHA-BRANCO-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB02B.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       140-EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM"
           MOVE SPACES TO MENSAGEM.

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
           move "CRP055"            to logacess-programa
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

           CLOSE CRD020 CRD020B CAD018 CGD010 CGD011 CBD001.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
