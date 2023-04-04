       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP053.
      *DATA: 09/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Boletim de Recebimento
      *FUNÇÃO: Listar todos os títulos que foram recebidos no intervalo
      *        de movimento solicitado. Lista apenas os títulos com
      *        situação = 2(pago)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CRPX020.
           COPY CRPX020B.
           COPY CBPX001.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CRPW020.
       COPY CRPW020B.
       COPY CBPW001.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP053.CPB".
           COPY "CRP053.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUX-TIPO              PIC 9        VALUE ZEROS.
           05  AUX-FORMAPG           PIC 9        VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  LIN                   PIC 9(02).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-RCTO-INI         PIC 9(8)     VALUE ZEROS.
           05  DATA-RCTO-FIM         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  TOT-VALOR-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-VLRREC-TOTAL      PIC 9(09)V99 VALUE ZEROS.
           05  TOT-JRMUL-TOTAL       PIC 9(09)V99 VALUE ZEROS.
           05  TOT-DESC-TOTAL        PIC 9(09)V99 VALUE ZEROS.

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
           05  FILLER              PIC X(27)   VALUE
           "BOLETIM DE RECEBIMENTO     ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV. RECTO: ".
           05  RECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  RECTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(12) VALUE "TIPO DOCTO: ".
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

       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-RECTO NOME-CLIENTE                      VALOR DATA-VECT
      -    "O DATA-RECTO VALOR-RECEBID JURO+MULTA   DESCONTO".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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

       01 PASSAR-STRING.
          05 PASSAR-STRING1            PIC X(65).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
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
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CRD020 CRD020B CGD010 CBD001.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: " TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23:2)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP053"            to logacess-programa
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
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
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

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE GS-DATA-RCTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-RCTO-INI.
           MOVE GS-DATA-RCTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-RCTO-FIM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM INVERTE-DATA.

           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO
           MOVE GS-FORMA-PAGTO-D(1:1)  TO AUX-FORMAPG
           MOVE SPACES TO GS-LINDET.

      *    INITIALIZE REG-CRD020
      *               TOT-VALOR-TOTAL
      *               TOT-VLRREC-TOTAL
      *               TOT-JRMUL-TOTAL
      *               TOT-DESC-TOTAL
      *
      *    MOVE ZEROS         TO SEQ-CAIXA-CR20
      *    MOVE DATA-RCTO-INI TO DATA-RCTO-CR20
      *    START CRD020 KEY IS NOT < ALT6-CR20 INVALID KEY
      *          MOVE "10" TO ST-CRD020.
      *    PERFORM UNTIL ST-CRD020 = "10"
      *       READ CRD020 NEXT RECORD AT END
      *            MOVE "10" TO ST-CRD020
      *       NOT AT END
      *         IF DATA-RCTO-CR20 > DATA-RCTO-FIM
      *            MOVE "10" TO ST-CRD020
      *         ELSE
      *           IF SITUACAO-CR20 NOT = 2
      *              CONTINUE
      *           ELSE
      *              IF AUX-TIPO = TIPO-DOCTO-CR20 OR AUX-TIPO = 9
      *                 MOVE DATA-RCTO-CR20  TO DATA-INV
      *                 CALL "GRIDAT1" USING DATA-INV
      *                 MOVE DATA-INV          TO DATA-E
      *                 MOVE DATA-E            TO GS-LINDET(01: 11)
      *                 MOVE CLASS-CLIENTE-CR20 TO CLASSIF-CG10
      *                 MOVE CLIENTE-CR20       TO CODIGO-CG10
      *                 READ CGD010 INVALID KEY
      *                      MOVE "****" TO COMPRADOR-CG10
      *                 END-READ
      *
      *                 ADD VALOR-TOT-CR20     TO TOT-VALOR-TOTAL
      *                 ADD VALOR-LIQ-CR20     TO TOT-VLRREC-TOTAL
      *                 ADD JURO-RCTO-CR20     TO TOT-JRMUL-TOTAL
      *                 ADD MULTA-RCTO-CR20    TO TOT-JRMUL-TOTAL
      *                 ADD DESCONTO-CR20      TO TOT-DESC-TOTAL
      *
      *                 MOVE COMPRADOR-CG10    TO GS-LINDET(12: 25)
      *                 MOVE VALOR-TOT-CR20    TO VALOR-E
      *                 MOVE VALOR-E           TO GS-LINDET(38: 14)
      *                 MOVE DATA-VENCTO-CR20  TO DATA-INV
      *                 CALL "GRIDAT1" USING DATA-INV
      *                 MOVE DATA-INV          TO DATA-E
      *                 MOVE DATA-E            TO GS-LINDET(52: 11)
      *                 MOVE DATA-RCTO-CR20    TO DATA-INV
      *                 CALL "GRIDAT1" USING DATA-INV
      *                 MOVE DATA-INV          TO DATA-E
      *                 MOVE DATA-E            TO GS-LINDET(63: 11)
      *                 MOVE VALOR-LIQ-CR20    TO VALOR-E
      *                 MOVE VALOR-E           TO GS-LINDET(74: 14)
      *                 ADD JURO-RCTO-CR20 TO MULTA-RCTO-CR20
      *                 GIVING VALOR-E1
      *                 MOVE VALOR-E1          TO GS-LINDET(88: 11)
      *                 MOVE DESCONTO-CR20     TO VALOR-E1
      *                 MOVE VALOR-E1          TO GS-LINDET(99: 10)
      *                 MOVE "INSERE-LIST" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *              END-IF
      *           END-IF
      *         END-IF
      *       END-READ
      *    END-PERFORM

           INITIALIZE REG-CRD020B
                      TOT-VALOR-TOTAL
                      TOT-VLRREC-TOTAL
                      TOT-JRMUL-TOTAL
                      TOT-DESC-TOTAL

           MOVE ZEROS         TO SEQ-CAIXA-CR20B
           MOVE DATA-RCTO-INI TO DATA-RCTO-CR20B
           START CRD020B KEY IS NOT < ALT6-CR20B INVALID KEY
                 MOVE "10" TO ST-CRD020B.
           PERFORM UNTIL ST-CRD020B = "10"
              READ CRD020B NEXT RECORD AT END
                   MOVE "10" TO ST-CRD020B
              NOT AT END
                IF DATA-RCTO-CR20B > DATA-RCTO-FIM
                   MOVE "10" TO ST-CRD020B
                ELSE
                   IF AUX-FORMAPG = 9 OR GS-FORMA-PAGTO-D =
                                         FORMA-PAGTO-CR20B
                      IF GS-ACP-CONTA = 0 OR CONTA-CORRENTE-CR20B
                         MOVE CLASS-CLIENTE-CR20B  TO CLASS-CLIENTE-CR20
                         MOVE CLIENTE-CR20B        TO CLIENTE-CR20
                         MOVE SEQ-CR20B            TO SEQ-CR20
                         READ CRD020 NOT INVALID KEY
                              IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                 MOVE DATA-RCTO-CR20B     TO DATA-INV
                                 CALL "GRIDAT1" USING DATA-INV
                                 MOVE DATA-INV            TO DATA-E
                                 MOVE DATA-E        TO GS-LINDET(01: 11)
                                 MOVE CLASS-CLIENTE-CR20B TO
                                      CLASSIF-CG10
                                 MOVE CLIENTE-CR20B TO CODIGO-CG10
                                 READ CGD010 INVALID KEY
                                      MOVE "****"   TO COMPRADOR-CG10
                                 END-READ

                                 ADD VALOR-TOT-CR20B TO TOT-VALOR-TOTAL
                                 ADD VALOR-LIQ-CR20B TO TOT-VLRREC-TOTAL
                                 ADD JURO-RCTO-CR20B TO TOT-JRMUL-TOTAL
                                 ADD MULTA-RCTO-CR20B TO TOT-JRMUL-TOTAL
                                 ADD DESCONTO-CR20B  TO TOT-DESC-TOTAL

                                 MOVE COMPRADOR-CG10 TO
                                      GS-LINDET(12: 25)
                                 MOVE VALOR-TOT-CR20B   TO VALOR-E
                                 MOVE VALOR-E      TO GS-LINDET(38: 14)
                                 MOVE DATA-VENCTO-CR20  TO DATA-INV
                                 CALL "GRIDAT1" USING DATA-INV
                                 MOVE DATA-INV     TO DATA-E
                                 MOVE DATA-E       TO GS-LINDET(52: 11)
                                 MOVE DATA-RCTO-CR20B   TO DATA-INV
                                 CALL "GRIDAT1" USING DATA-INV
                                 MOVE DATA-INV          TO DATA-E
                                 MOVE DATA-E     TO GS-LINDET(63: 11)
                                 MOVE VALOR-LIQ-CR20B   TO VALOR-E
                                 MOVE VALOR-E     TO GS-LINDET(74: 14)
                                 ADD JURO-RCTO-CR20B TO MULTA-RCTO-CR20B
                                 GIVING VALOR-E1
                                 MOVE VALOR-E1      TO GS-LINDET(88: 11)
                                 MOVE DESCONTO-CR20B    TO VALOR-E1
                                 MOVE VALOR-E1      TO GS-LINDET(99: 10)
                                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                                 PERFORM CALL-DIALOG-SYSTEM
                              END-IF
                         END-READ
                      END-IF
                   END-IF
                END-IF
              END-READ
           END-PERFORM

           MOVE SPACES TO GS-LINDET
           MOVE "Totalizador ..."              TO GS-LINDET(1:20)
           MOVE TOT-VALOR-TOTAL                TO VALOR-E
           MOVE VALOR-E                        TO GS-LINDET(38: 14)
           MOVE TOT-VLRREC-TOTAL               TO VALOR-E
           MOVE VALOR-E                        TO GS-LINDET(74: 14)
           MOVE TOT-JRMUL-TOTAL                TO VALOR-E1
           MOVE VALOR-E1                       TO GS-LINDET(88: 10)
           MOVE TOT-DESC-TOTAL                 TO VALOR-E1
           MOVE VALOR-E1                       TO GS-LINDET(99: 10)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP053" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-DATA-RCTO-INI     TO RECTO-INI-REL
           MOVE GS-DATA-RCTO-FIM     TO RECTO-FIM-REL
           MOVE GS-TIPO-DOCTO        TO DOCTO-REL

           MOVE GS-ACP-CONTA         TO CONTA-REL
           MOVE GS-DESC-CONTA        TO NOME-CONTA-REL
           MOVE GS-FORMA-PAGTO-D     TO FORMA-PGTO-REL

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO
           MOVE GS-FORMA-PAGTO-D       TO AUX-FORMAPG

      *    INITIALIZE REG-CRD020
      *               TOT-VALOR-TOTAL
      *               TOT-VLRREC-TOTAL
      *               TOT-JRMUL-TOTAL
      *               TOT-DESC-TOTAL
      *
      *
      *    MOVE SPACES        TO LINDET-REL
      *    MOVE ZEROS         TO SEQ-CAIXA-CR20.
      *    MOVE DATA-RCTO-INI TO DATA-RCTO-CR20.
      *
      *    START CRD020 KEY IS NOT < ALT6-CR20 INVALID KEY
      *          MOVE "10" TO ST-CRD020.
      *
      *    PERFORM UNTIL ST-CRD020 = "10"
      *       READ CRD020 NEXT RECORD AT END
      *            MOVE "10" TO ST-CRD020
      *       NOT AT END
      *            IF DATA-RCTO-CR20 > DATA-RCTO-FIM
      *               MOVE "10" TO ST-CRD020
      *            ELSE
      *               IF SITUACAO-CR20 NOT = 2
      *                  CONTINUE
      *               ELSE
      *                  IF AUX-TIPO = TIPO-DOCTO-CR20 OR AUX-TIPO = 9
      *                     ADD VALOR-TOT-CR20     TO TOT-VALOR-TOTAL
      *                     ADD VALOR-LIQ-CR20     TO TOT-VLRREC-TOTAL
      *                     ADD JURO-RCTO-CR20     TO TOT-JRMUL-TOTAL
      *                     ADD MULTA-RCTO-CR20    TO TOT-JRMUL-TOTAL
      *                     ADD DESCONTO-CR20      TO TOT-DESC-TOTAL
      *
      *                     MOVE DATA-RCTO-CR20    TO DATA-INV
      *                     CALL "GRIDAT1" USING DATA-INV
      *                     MOVE DATA-INV          TO DATA-E
      *                     MOVE DATA-E            TO LINDET-REL(01: 11)
      *                     MOVE CLASS-CLIENTE-CR20 TO CLASSIF-CG10
      *                     MOVE CLIENTE-CR20       TO CODIGO-CG10
      *                     READ CGD010 INVALID KEY
      *                          MOVE "****" TO COMPRADOR-CG10
      *                     END-READ
      *                     MOVE COMPRADOR-CG10    TO LINDET-REL(12: 25)
      *                     MOVE VALOR-TOT-CR20    TO VALOR-E
      *                     MOVE VALOR-E           TO LINDET-REL(38: 14)
      *                     MOVE DATA-VENCTO-CR20  TO DATA-INV
      *                     CALL "GRIDAT1" USING DATA-INV
      *                     MOVE DATA-INV          TO DATA-E
      *                     MOVE DATA-E            TO LINDET-REL(52: 11)
      *                     MOVE DATA-RCTO-CR20    TO DATA-INV
      *                     CALL "GRIDAT1" USING DATA-INV
      *                     MOVE DATA-INV          TO DATA-E
      *                     MOVE DATA-E            TO LINDET-REL(63: 11)
      *                     MOVE VALOR-LIQ-CR20    TO VALOR-E
      *                     MOVE VALOR-E           TO LINDET-REL(74: 14)
      *                     ADD JURO-RCTO-CR20 TO MULTA-RCTO-CR20
      *                     GIVING VALOR-E1
      *                     MOVE VALOR-E1          TO LINDET-REL(88: 11)
      *                     MOVE DESCONTO-CR20     TO VALOR-E1
      *                     MOVE VALOR-E1          TO LINDET-REL(99: 10)
      *                     WRITE REG-RELAT FROM LINDET
      *                     ADD 1 TO LIN
      *                     IF LIN > 56
      *                        PERFORM CABECALHO
      *                     END-IF
      *                  END-IF
      *               END-IF
      *            END-IF
      *       END-READ
      *    END-PERFORM

           INITIALIZE REG-CRD020B
                      TOT-VALOR-TOTAL
                      TOT-VLRREC-TOTAL
                      TOT-JRMUL-TOTAL
                      TOT-DESC-TOTAL


           MOVE SPACES        TO LINDET-REL
           MOVE ZEROS         TO SEQ-CAIXA-CR20B
           MOVE DATA-RCTO-INI TO DATA-RCTO-CR20B

           START CRD020B KEY IS NOT < ALT6-CR20B INVALID KEY
                 MOVE "10" TO ST-CRD020B.

           PERFORM UNTIL ST-CRD020B = "10"
              READ CRD020B NEXT RECORD AT END
                   MOVE "10" TO ST-CRD020B
              NOT AT END
                   IF DATA-RCTO-CR20B > DATA-RCTO-FIM
                      MOVE "10" TO ST-CRD020
                   ELSE
                      IF AUX-FORMAPG = 9 OR GS-FORMA-PAGTO-D =
                                            FORMA-PAGTO-CR20B
                         IF GS-ACP-CONTA = 0 OR CONTA-CORRENTE-CR20B
                            MOVE CLASS-CLIENTE-CR20B  TO
                                 CLASS-CLIENTE-CR20
                            MOVE CLIENTE-CR20B        TO CLIENTE-CR20
                            MOVE SEQ-CR20B            TO SEQ-CR20
                            READ CRD020 NOT INVALID KEY
                                 IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                    ADD VALOR-TOT-CR20B    TO
                                        TOT-VALOR-TOTAL
                                    ADD VALOR-LIQ-CR20B    TO
                                        TOT-VLRREC-TOTAL
                                    ADD JURO-RCTO-CR20B    TO
                                        TOT-JRMUL-TOTAL
                                    ADD MULTA-RCTO-CR20B   TO
                                        TOT-JRMUL-TOTAL
                                    ADD DESCONTO-CR20B     TO
                                        TOT-DESC-TOTAL

                                    MOVE DATA-RCTO-CR20B   TO DATA-INV
                                    CALL "GRIDAT1" USING DATA-INV
                                    MOVE DATA-INV      TO DATA-E
                                    MOVE DATA-E    TO LINDET-REL(01: 11)
                                    MOVE CLASS-CLIENTE-CR20B TO
                                         CLASSIF-CG10
                                    MOVE CLIENTE-CR20B TO CODIGO-CG10
                                    READ CGD010 INVALID KEY
                                         MOVE "****" TO COMPRADOR-CG10
                                    END-READ
                                    MOVE COMPRADOR-CG10  TO
                                         LINDET-REL(12: 25)
                                    MOVE VALOR-TOT-CR20B TO VALOR-E
                                    MOVE VALOR-E TO LINDET-REL(38: 14)
                                    MOVE DATA-VENCTO-CR20 TO DATA-INV
                                    CALL "GRIDAT1" USING DATA-INV
                                    MOVE DATA-INV        TO DATA-E
                                    MOVE DATA-E    TO LINDET-REL(52: 11)
                                    MOVE DATA-RCTO-CR20B TO DATA-INV
                                    CALL "GRIDAT1" USING DATA-INV
                                    MOVE DATA-INV        TO DATA-E
                                    MOVE DATA-E    TO LINDET-REL(63: 11)
                                    MOVE VALOR-LIQ-CR20B TO VALOR-E
                                    MOVE VALOR-E TO LINDET-REL(74: 14)
                                    ADD JURO-RCTO-CR20B  TO
                                        MULTA-RCTO-CR20B
                                    GIVING VALOR-E1
                                    MOVE VALOR-E1 TO LINDET-REL(88: 11)
                                    MOVE DESCONTO-CR20B  TO VALOR-E1
                                    MOVE VALOR-E1 TO LINDET-REL(99: 10)
                                    WRITE REG-RELAT FROM LINDET
                                    ADD 1 TO LIN
                                    IF LIN > 56
                                       PERFORM CABECALHO
                                    END-IF
                                 END-IF
                            END-READ
                         END-IF
                      END-IF
                   END-IF
              END-READ
           END-PERFORM


           MOVE SPACES TO LINDET
           MOVE "Totalizador ..."              TO LINDET(1:20)
           MOVE TOT-VALOR-TOTAL                TO VALOR-E
           MOVE VALOR-E                        TO LINDET(38: 14)
           MOVE TOT-VLRREC-TOTAL               TO VALOR-E
           MOVE VALOR-E                        TO LINDET(74: 14)
           MOVE TOT-JRMUL-TOTAL                TO VALOR-E1
           MOVE VALOR-E1                       TO LINDET(88: 11)
           MOVE TOT-DESC-TOTAL                 TO VALOR-E1
           MOVE VALOR-E1                       TO LINDET(99: 10)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF

           COPY DESCONDENSA.


       CABECALHO SECTION.
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
           move "CRP053"            to logacess-programa
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

           CLOSE CRD020 CRD020B CGD010 CBD001.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
