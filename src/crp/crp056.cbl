       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP056.
      *DATA: 12/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: EMISSÃO DE DUPLICATA COM ACEITE
      *FUNÇÃO: Listar todos os títulos que estiverem dentro do intervalo
      *        de vencimento. As ordens serão: Vencto, Portador, cliente
      *        e VCTO/vcto. Os títulos serão selecionados um a um para
      *        a impressão da duplicata.
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
           COPY CAPX010.
           COPY CAPX018.
           COPY RCPX100.
           COPY LOGACESS.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CLASSIF-WK CLIENTE-WK SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = VENCTO-WK VALOR-WK
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENCTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                     PORTADOR-WK CARTEIRA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CGPW011.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY RCPW100.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  SEQ-WK              PIC 9(5).
           05  NOME-CLIEN-WK       PIC X(20).
           05  CIDADE-WK           PIC X(20).
           05  UF-WK               PIC XX.
           05  NOSSO-NR-WK         PIC X(15).
           05  PORTADOR-WK         PIC X(10).
           05  VENCTO-WK           PIC 9(8).
           05  VALOR-WK            PIC 9(8)V99.
           05  CARTEIRA-WK         PIC X(4).
           05  RESPONSAVEL-WK      PIC X(5).
           05  IMPRESSO-WK         PIC 9.
      *    IMPRESSO-WK = 0 NAO   1-SIM
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP056.CPB".
           COPY "CRP056.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
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
           05  NAO-IMPRIME           PIC 9        VALUE ZEROS.
           05  DATAW.
               10  DIA-W             PIC 99.
               10  MES-W             PIC 99.
               10  ANO-W             PIC 9999.
           05  DATA-W REDEFINES DATAW PIC 9(8).
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  NOME-CLIEN-ANT        PIC X(20)    VALUE SPACES.
           05  PORTADOR-ANT          PIC X(10)    VALUE ZEROS.
           05  VENCTO-ANT            PIC 9(8)     VALUE ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(15)    VALUE SPACES.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  TABELA-DATA.
           05  FILLER         PIC X(10) VALUE "  JANEIRO ".
           05  FILLER         PIC X(10) VALUE " FEVEREIRO".
           05  FILLER         PIC X(10) VALUE "   MARCO  ".
           05  FILLER         PIC X(10) VALUE "   ABRIL  ".
           05  FILLER         PIC X(10) VALUE "   MAIO   ".
           05  FILLER         PIC X(10) VALUE "   JUNHO  ".
           05  FILLER         PIC X(10) VALUE "   JULHO  ".
           05  FILLER         PIC X(10) VALUE "  AGOSTO  ".
           05  FILLER         PIC X(10) VALUE " SETEMBRO ".
           05  FILLER         PIC X(10) VALUE "  OUTUBRO ".
           05  FILLER         PIC X(10) VALUE " NOVEMBRO ".
           05  FILLER         PIC X(10) VALUE " DEZEMBRO ".
       01  TAB-DAT REDEFINES TABELA-DATA.
           05  TABMES OCCURS 12 TIMES PIC X(10).

       01  UNIDADE-DEZENA.
           05  FILLER    PIC X(16)   VALUE 'UM              '.
           05  FILLER    PIC X(16)   VALUE 'DOIS            '.
           05  FILLER    PIC X(16)   VALUE 'TRES            '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TRO         '.
           05  FILLER    PIC X(16)   VALUE 'CIN-CO          '.
           05  FILLER    PIC X(16)   VALUE 'SEIS            '.
           05  FILLER    PIC X(16)   VALUE 'SE-TE           '.
           05  FILLER    PIC X(16)   VALUE 'OI-TO           '.
           05  FILLER    PIC X(16)   VALUE 'NO-VE           '.
           05  FILLER    PIC X(16)   VALUE 'DEZ             '.
           05  FILLER    PIC X(16)   VALUE 'ON-ZE           '.
           05  FILLER    PIC X(16)   VALUE 'DO-ZE           '.
           05  FILLER    PIC X(16)   VALUE 'TRE-ZE          '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TOR-ZE      '.
           05  FILLER    PIC X(16)   VALUE 'QUIN-ZE         '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZES-SEIS     '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZES-SE-TE    '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZOI-TO       '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZE-NO-VE     '.
       01  TABELA-UNI-DEZ REDEFINES UNIDADE-DEZENA.
           05  UNID-DEZ OCCURS 19 TIMES PIC X(16).

       01  DEZENA-1.
           05  FILLER    PIC X(16)   VALUE 'DEZ             '.
           05  FILLER    PIC X(16)   VALUE 'VIN-TE          '.
           05  FILLER    PIC X(16)   VALUE 'TRIN-TA         '.
           05  FILLER    PIC X(16)   VALUE 'QUA-REN-TA      '.
           05  FILLER    PIC X(16)   VALUE 'CIN-QUEN-TA     '.
           05  FILLER    PIC X(16)   VALUE 'SES-SEN-TA      '.
           05  FILLER    PIC X(16)   VALUE 'SE-TEN-TA       '.
           05  FILLER    PIC X(16)   VALUE 'OI-TEN-TA       '.
           05  FILLER    PIC X(16)   VALUE 'NO-VEN-TA       '.
       01  TABELA-DEZENA REDEFINES DEZENA-1.
           05  DEZENA OCCURS 9 TIMES PIC X(16).

       01  CENTENA-1.
           05  FILLER    PIC X(16)   VALUE 'CEM             '.
           05  FILLER    PIC X(16)   VALUE 'DU-ZEN-TOS      '.
           05  FILLER    PIC X(16)   VALUE 'TRE-ZEN-TOS     '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TRO-CEN-TOS '.
           05  FILLER    PIC X(16)   VALUE 'QUI-NHEN-TOS    '.
           05  FILLER    PIC X(16)   VALUE 'SEIS-CEN-TOS    '.
           05  FILLER    PIC X(16)   VALUE 'SE-TE-CEN-TOS   '.
           05  FILLER    PIC X(16)   VALUE 'OI-TO-CEN-TOS   '.
           05  FILLER    PIC X(16)   VALUE 'NO-VE-CEN-TOS   '.
       01  TABELA-CENTENA REDEFINES CENTENA-1.
           05  CENTENA OCCURS 9 TIMES PIC X(16).

       01  NUM-EXT.
           05  EXT OCCURS 16 TIMES PIC X.


      *    --- A tabela abaixo deve ter tantas ocorrˆncias quantos
      *        forem o n£mero de caracteres da linha detalhe.

       01  TABELA-S.
           05  TAB-S OCCURS 45 TIMES PIC X.

       01  NUM-3DIG.
           05  D1                        PIC 9 VALUE ZEROS.
           05  NUM-2DIG.
               10  D2                    PIC 9 VALUE ZEROS.
               10  D3                    PIC 9 VALUE ZEROS.
           05  NUM-2D REDEFINES NUM-2DIG PIC 99.
       01  NUM-3D REDEFINES NUM-3DIG     PIC 999.

       01  NUMERO.
           05  N1       PIC 99    VALUE ZEROS.
           05  N2       PIC 999   VALUE ZEROS.
           05  N3       PIC 999   VALUE ZEROS.
           05  N4       PIC 999   VALUE ZEROS.
           05  N5       PIC 99    VALUE ZEROS.
       01  NUM REDEFINES NUMERO PIC 9(11)V99.

       01  AUXILIARES.
           05  CONT     PIC 9     VALUE ZEROS.
           05  I        PIC 99    VALUE ZEROS.
           05  J        PIC 99    VALUE ZEROS.
           05  K        PIC 99    VALUE ZEROS.
           05  M        PIC 99    VALUE ZEROS.
           05  P        PIC 99    VALUE ZEROS.
           05  CONTDUP               PIC 9   VALUE ZEROS.
           05  S                     PIC 99       VALUE ZEROS.

           COPY "PARAMETR".
       01  LINDET1.
           05  FILLER                PIC X(53)    VALUE SPACES.
           05  DIA-VENDA             PIC ZZ       VALUE ZEROS.
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  MES-VENDA             PIC X(13)    VALUE SPACES.
           05  FILLER                PIC X(5)     VALUE SPACES.
           05  ANO-VENDA             PIC ZZ       VALUE ZEROS.
       01  LINDET2.
           05  FILLER                PIC X(2)     VALUE SPACES.
           05  VALOR-FATURA          PIC ZZ.ZZZ.ZZZ,ZZ.
           05  FILLER                PIC X(4)     VALUE SPACES.
           05  NR-FATURA             PIC X(7)     VALUE SPACES.
           05  FILLER                PIC X        VALUE SPACES.
           05  VALOR-DUPLICATA       PIC Z.ZZZ.ZZZ,ZZ.
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  NR-DUPLICATA          PIC X(7)     VALUE SPACES.
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  VENCTO-DUPLICATA      PIC ZZ/ZZ/ZZZZ.
       01  LINDET3.
           05  FILLER                PIC X(40)    VALUE SPACES.
           05  NOME-SACADO           PIC X(30)    VALUE SPACES.
       01  LINDET4.
           05  FILLER                PIC X(35)    VALUE SPACES.
           05  ENDERECO-SACADO       PIC X(30)    VALUE SPACES.
           05  FILLER                PIC X(1)     VALUE "-".
           05  BAIRRO-SACADO         PIC X(15)    VALUE SPACES.
       01  LINDET5.
           05  FILLER                PIC X(40)    VALUE SPACES.
           05  CEP-SACADO            PIC ZZZZZ.ZZZ.
           05  FILLER                PIC X(1)     VALUE SPACES.
           05  MUNICIPIO-SACADO      PIC X(20)    VALUE SPACES.
           05  FILLER                PIC X(8)     VALUE SPACES.
           05  UF-SACADO             PIC XX       VALUE SPACES.
       01  LINDET6.
           05  FILLER                PIC X(42)    VALUE SPACES.
           05  PRACA-PAGTO           PIC X(30)    VALUE SPACES.
           05  FILLER                PIC X(35)    VALUE SPACES.
       01  LINDET7.
           05  FILLER                PIC X(32)    VALUE SPACES.
           05  CPF-SACADO            PIC ZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ.
           05  FILLER                PIC X(12)    VALUE SPACES.
           05  RG-SACADO             PIC X(14)    VALUE SPACES.
       01  LINDET8.
           05  FILLER                PIC X(36)    VALUE SPACES.
           05  EXTENSO               PIC X(45)    VALUE SPACES.
       01  LINDET9.
           05  FILLER                PIC X(40)    VALUE SPACES.
           05  FILLER                PIC X(6)     VALUE "FONE: ".
           05  FONE-REL              PIC ZZZZ.ZZZZ.
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  FILLER                PIC X        VALUE "(".
           05  ALBUM-REL             PIC 9999.9999.
           05  FILLER                PIC X        VALUE ")".

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
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT  CAD018 CAD010 CGD010 CGD011 RCD100.
           OPEN I-O    CRD020
           COPY IMPRESSORA.CHAMA.
           IF LNK-MAPEAMENTO <> SPACES
              OPEN OUTPUT RELAT
           ELSE
              MOVE "IMPRESSORA NÃO SELECIONADA" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO
           END-IF
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP056"            to logacess-programa
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM IMPRESSO-DUPLICATA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
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
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-VENCTO-INI TO DATA-INV
                                 VENCTO-INI-ANT
           CALL "GRIDAT2"  USING DATA-INV
           MOVE DATA-INV      TO VENCTO-INI
           MOVE GS-VENCTO-FIM TO DATA-INV
                                 VENCTO-FIM-ANT
           CALL "GRIDAT2"  USING DATA-INV
           MOVE DATA-INV      TO VENCTO-FIM

           INITIALIZE REG-CRD020
           MOVE VENCTO-INI    TO DATA-VENCTO-CR20
           MOVE ZEROS         TO SITUACAO-CR20
                                 CLIENTE-CR20
                                 CLASS-CLIENTE-CR20
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                  READ CRD020 NEXT RECORD AT END
                       MOVE "10" TO ST-CRD020
                  NOT AT END
                       IF SITUACAO-CR20 <> 0
                          MOVE "10" TO ST-CRD020
                       ELSE
                          IF DATA-VENCTO-CR20 > VENCTO-FIM
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF GS-PORTADOR-ORIGEM = PORTADOR-CR20
                                   PERFORM MOVER-DADOS-WORK
                                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                   WRITE REG-WORK
                             END-IF
                          END-IF
                       END-IF
                  END-READ
           END-PERFORM
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE DATA-VENCTO-CR20     TO VENCTO-WK
                                        GS-EXIBE-VENCTO
           MOVE CLASS-CLIENTE-CR20   TO CLASSIF-WK CLASSIF-CG10
                                        CLASSIF-CG11
           MOVE CLIENTE-CR20         TO CLIENTE-WK CODIGO-CG10
                                        CODIGO-CG11
           READ CGD010 INVALID KEY
                MOVE "*******"       TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10       TO NOME-CLIEN-WK.
           READ CGD011 INVALID KEY
                MOVE ZEROS           TO CIDADE1-CG11.

           MOVE CIDADE1-CG11         TO CIDADE
           READ CAD010 INVALID KEY
                MOVE "********"      TO NOME-CID
                MOVE "**"            TO UF-CID.

           MOVE NOME-CID             TO CIDADE-WK
           MOVE UF-CID               TO UF-WK
           MOVE SEQ-CR20             TO SEQ-WK
           MOVE PORTADOR-CR20        TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "******"        TO NOME-PORT.

           MOVE NOME-PORT            TO PORTADOR-WK
           MOVE OUTRO-DOCTO-CR20     TO NOSSO-NR-WK
           MOVE VALOR-TOT-CR20       TO VALOR-WK
           EVALUATE CARTEIRA-CR20
             WHEN 1 MOVE "SIMP"      TO CARTEIRA-WK
             WHEN 2 MOVE "CAUC"      TO CARTEIRA-WK
             WHEN 3 MOVE "DESC"      TO CARTEIRA-WK
           END-EVALUATE
           MOVE ZEROS                TO IMPRESSO-WK.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO GS-LINDET
           PERFORM ORDEM
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-TOTAL-PERIODO
                         GS-TOTAL-VENCIDO
                         GS-TOTAL-AVENCER
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-LINDET
               END-READ
           END-PERFORM.
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
                MOVE "PORTADOR/CART" TO GS-DESCR-ORDEM
                MOVE SPACES TO PORTADOR-WK CARTEIRA-WK
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
                    PERFORM TOTALIZA
             WHEN 2
              IF NOME-CLIEN-ANT  NOT = ZEROS
                 IF NOME-CLIEN-ANT NOT = NOME-CLIEN-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF PORTADOR-ANT NOT = SPACES
                 IF PORTADOR-ANT NOT = PORTADOR-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF VENCTO-ANT NOT = ZEROS
                 IF VENCTO-ANT NOT = VENCTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE NOME-CLIEN-WK     TO GS-LINDET(01: 21)
           MOVE CIDADE-WK         TO GS-LINDET(22: 14)
           MOVE UF-WK             TO GS-LINDET(36: 3)
           MOVE NOSSO-NR-WK       TO GS-LINDET(39: 16)
           MOVE PORTADOR-WK       TO GS-LINDET(55: 11)
           MOVE CARTEIRA-WK       TO GS-LINDET(66: 5)
           MOVE VENCTO-WK         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(71: 11)
           MOVE VALOR-WK          TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(82: 14)
           ADD VALOR-WK           TO TOTAL-W GS-TOTAL-PERIODO.
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(96: 13)
           IF VENCTO-WK < DATA-DIA-I
              ADD VALOR-WK        TO GS-TOTAL-VENCIDO
           ELSE ADD VALOR-WK      TO GS-TOTAL-AVENCER.
           MOVE CLASSIF-WK        TO GS-LINDET(110: 1)
           MOVE CLIENTE-WK        TO GS-LINDET(111: 8)
           MOVE SEQ-WK            TO GS-LINDET(119: 5).
       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO PORTADOR-ANT NOME-CLIEN-ANT.
           MOVE ZEROS TO VENCTO-ANT TOTAL-W.
       MOVER-CHAVE-ANT SECTION.
           MOVE NOME-CLIEN-WK     TO NOME-CLIEN-ANT.
           MOVE VENCTO-WK         TO VENCTO-ANT.
           MOVE PORTADOR-WK       TO PORTADOR-ANT.
       TOTALIZA SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP056" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       IMPRESSO-DUPLICATA SECTION.
           MOVE GS-LINDET(110: 1) TO CLASSIF-WK CLASS-CLIENTE-CR20
           MOVE GS-LINDET(111: 8) TO CLIENTE-WK CLIENTE-CR20
           MOVE GS-LINDET(119: 5) TO SEQ-WK SEQ-CR20
           READ WORK.
           READ CRD020.
           MOVE ZEROS                 TO NAO-IMPRIME
           MOVE DATA-EMISSAO-CR20     TO DATA-W.
           MOVE DIA-W                 TO DIA-VENDA
           MOVE TABMES (MES-W)        TO MES-VENDA
           MOVE ANO-W                 TO ANO-VENDA
           MOVE CLIENTE-CR20          TO ALBUM-REC
           READ RCD100 INVALID KEY MOVE 1 TO NAO-IMPRIME
           END-READ
           MOVE VALOR-TOT-CR20        TO VALOR-FATURA
           MOVE NR-DOCTO-CR20(1: 5)   TO NR-FATURA
           MOVE VALOR-TOT-CR20        TO VALOR-DUPLICATA
           MOVE NR-DOCTO-CR20(1: 6)   TO NR-DUPLICATA
           MOVE DATA-VENCTO-CR20      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV              TO VENCTO-DUPLICATA
           MOVE COD-COMPL-CR20        TO COD-COMPL-CG10 COD-COMPL-CG11
           READ CGD010 INVALID MOVE 1 TO NAO-IMPRIME
                               INITIALIZE REG-CGD010
           END-READ
           READ CGD011 INVALID KEY MOVE 1 TO NAO-IMPRIME
                               INITIALIZE REG-CGD011
           END-READ
           MOVE COMPRADOR-CG10        TO NOME-SACADO
           MOVE ENDERECO1-CG11        TO ENDERECO-SACADO
           MOVE BAIRRO1-CG11          TO BAIRRO-SACADO
           MOVE FONE1-CG11            TO FONE-REL
           MOVE COD-COMPL-CG11        TO ALBUM-REL
           MOVE CIDADE-WK             TO MUNICIPIO-SACADO
           MOVE CEP1-CG11             TO CEP-SACADO
           MOVE UF-WK                 TO UF-SACADO
           MOVE CPF1-CG11             TO CPF-SACADO
           MOVE RG1-CG11              TO RG-SACADO.
           IF NAO-IMPRIME = 0 PERFORM CONT-IMPRESSAO
           ELSE MOVE "NAO-IMPRIME" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM.
       CONT-IMPRESSAO SECTION.
           WRITE REG-RELAT FROM LINDET1 AFTER 5.
           WRITE REG-RELAT FROM LINDET2 AFTER 5.
           WRITE REG-RELAT FROM LINDET3 AFTER 6.
           WRITE REG-RELAT FROM LINDET4.
           WRITE REG-RELAT FROM LINDET5 AFTER 2.
           WRITE REG-RELAT FROM LINDET6.
           WRITE REG-RELAT FROM LINDET7 AFTER 2.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.

           MOVE SPACES TO TABELA-S.
           MOVE ZEROS TO I, J, CONTDUP.
           MOVE 1 TO K.
           MOVE VALOR-TOT-CR20 TO NUM.
           PERFORM GRAVA-CHEQUE THRU TERMINO.
           WRITE REG-RELAT FROM LINDET9 AFTER 4.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           MOVE GS-PORTADOR-DESTINO   TO PORTADOR-CR20.
           REWRITE REG-CRD020.

      *  ********ROTINA EXTENSO DO VALOR *********************

       GRAVA-CHEQUE SECTION.
      *
      *
      *           ........   rotina de extenso   ..........
      *
      *
      *
           IF N1 = 1
              MOVE 'UM              ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              MOVE 'BI-LHAO         ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N1 = 0 OR = 1
              GO TO ESCR-MILHAO.
           MOVE N1 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'BI-LHOES        ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-MILHAO SECTION.
           IF N2 = 1
              MOVE 'UM               ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              MOVE 'MI-LHAO          ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N2 = 0 OR = 1
              GO TO ESCR-MIL.
           MOVE N2 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'MI-LHOES        ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-MIL SECTION.
           IF N3 = 0
              GO TO ESCR-UNID.
           MOVE N3 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'MIL             ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-UNID SECTION.
           IF N4 = 0
              GO TO ESCR-CRUZADO.
           MOVE N4 TO NUM-3D.
           IF D1 = 0 AND (N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0)
              MOVE 'E               ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF D1 NOT = 0 AND D2 = 0 AND D3 = 0
              IF N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0
                 MOVE 'E               ' TO NUM-EXT
                 PERFORM ESCR-EXT THRU FIM-EXT.
           PERFORM ESCR-3DIG THRU FIM-3DIG.

       ESCR-CRUZADO SECTION.
           IF N3 = 0 AND N4 = 0 AND (N1 NOT = 0 OR N2 NOT = 0)
              MOVE 'DE              ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N1 = 0 AND N2 = 0 AND N3 = 0 AND N4 = 1
              MOVE 'RE-AL           ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
      *       MOVE 'RE-AL           ' TO NUM-EXT
      *       PERFORM ESCR-EXT THRU FIM-EXT
           ELSE IF N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0 OR N4 NOT = 0
                MOVE 'RE-AIS          ' TO NUM-EXT
                PERFORM ESCR-EXT THRU FIM-EXT.
      *         MOVE 'RE-AIS          ' TO NUM-EXT
      *         PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-CENTAVO SECTION.
           MOVE ZEROS TO NUM-3D.
           MOVE N5 TO NUM-3D.
           IF N5 = 0
              GO MOVE-ASTER.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           IF N5 = 1
              MOVE 'CEN-TA-VO       ' TO NUM-EXT
           ELSE MOVE 'CEN-TA-VOS      ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.


      *    --- na compara‡„o abaixo   IF K < 45
      *        substituir o valor 45 pelo n£mero de caracteres
      *        existentes na linha detalhe.


       MOVE-ASTER SECTION.
           MOVE '*' TO TAB-S (K).
           ADD 1 TO K.
           IF K < 46
              GO TO MOVE-ASTER.
           PERFORM AJUSTE-DIR THRU FIM-AJUSTE.
           MOVE TABELA-S TO EXTENSO.
           WRITE REG-RELAT FROM LINDET8.
           IF CONTDUP NOT = 1
              MOVE ALL '*' TO EXTENSO
              WRITE REG-RELAT FROM LINDET8.

           GO TO TERMINO.

      *    ---  comando:
      *         GO TO  nome-par grafo.
      *    ---  colocar o nome do par grafo para onde deve prosseguir
      *         o programa ap•s a impress„o do n£mero em extenso.


      *    --- os par grafos abaixo s„o executados
      *        pelo comando PERFORM e devem ser colocados
      *        no fim do programa.

       ESCR-3DIG SECTION.
           IF D1 = 0 GO TO ESCR-2DIG.
           IF NUM-3D = 100
              MOVE 'CEM             ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              GO TO FIM-3DIG.
           IF D1 = 1
              MOVE 'CEN-TO          ' TO NUM-EXT
           ELSE MOVE CENTENA (D1) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           IF NUM-2D = 0 GO FIM-3DIG.
           MOVE 'E               ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-2DIG SECTION.
           IF D2 = 0 OR D2 = 1
              MOVE UNID-DEZ (NUM-2D) TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              GO FIM-3DIG.
           MOVE DEZENA (D2) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           IF D3 = 0 GO FIM-3DIG.
           MOVE 'E               ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           MOVE UNID-DEZ (D3) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
       FIM-3DIG SECTION.
           EXIT.

       ESCR-EXT SECTION.
           MOVE 1 TO CONT.
           ADD 1 TO I.
           MOVE I TO J.


      *    --- na compara‡„o abaixo  IF K + CONT < 45
      *        substituir o valor 45 pelo n£mero de caracteres
      *        definidos na tabela TABELA-S + 1.

       CONTA-CARAC SECTION.
           IF EXT (J) NOT = '-' AND NOT = ' '
              ADD 1 TO CONT, J
              GO CONTA-CARAC.
           IF K + CONT < 46
              GO MOVE-SILABA.
           COMPUTE S = I - 1.
           IF S < 1 MOVE 1 TO S.
           IF EXT (S) = '-'
              MOVE '-' TO TAB-S (K).
           PERFORM AJUSTE-DIR THRU FIM-AJUSTE.

           MOVE TABELA-S TO EXTENSO.
           WRITE REG-RELAT FROM LINDET8.
           MOVE SPACES TO TABELA-S .
           MOVE 1 TO K  CONTDUP.

       MOVE-SILABA SECTION.
           MOVE EXT (I) TO TAB-S (K).
           ADD 1 TO K, I.
           IF EXT (I) NOT = '-' AND NOT = ' '
              GO MOVE-SILABA.
           IF EXT (I) NOT = ' '
              GO ESCR-EXT.
           MOVE ZEROS TO I.
           ADD 1 TO K.
       FIM-EXT SECTION.
           EXIT.

       AJUSTE-DIR SECTION.
           MOVE 45 TO J, M.
           MOVE 1     TO CONT.

       CONTA-BRANCOS SECTION.
           IF TAB-S (J) = ' '
              ADD 1 TO CONT
              SUBTRACT 1 FROM J
              GO CONTA-BRANCOS
           ELSE SUBTRACT 1 FROM CONT.

       AJUSTA SECTION.
           SUBTRACT CONT FROM M GIVING P.
           IF TAB-S (P) NOT = ' '
              MOVE TAB-S (P) TO TAB-S (M)
              SUBTRACT 1 FROM M
              GO AJUSTA.
           MOVE TAB-S (P) TO TAB-S (M).
           IF CONT NOT = 0
              SUBTRACT 1 FROM CONT
              SUBTRACT 1 FROM M
              SUBTRACT CONT FROM M GIVING P
              MOVE TAB-S (P) TO TAB-S (M)
              SUBTRACT 1 FROM M
              GO TO AJUSTA.
       FIM-AJUSTE SECTION.
           EXIT.
       TERMINO SECTION.
      ***************************************************
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
           move "CRP056"            to logacess-programa
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

           CLOSE CRD020 CAD018 CGD010 CAD010 CGD011 RCD100.
           CLOSE RELAT.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
