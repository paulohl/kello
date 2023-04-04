       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP020A.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 22/04/1999
      *FUNÇÃO: MANUTENÇÃO de contas a receber

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CAPX002.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX020.
           COPY CXPX020.
           COPY CRPX001.
           COPY CRPX020.
           COPY CRPX099.
           COPY CRPX200.
           COPY CRPX201.
           COPY CAPX018.
           COPY RCPX100.
           COPY CRPX013.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGX003.
           COPY LOGACESS.SEL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW002.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW020.
       COPY CXPW020.
       COPY CAPW018.
       COPY CRPW001.
       COPY CRPW020.
       COPY CRPW099.
       COPY CRPW200.
       COPY CRPW201.
       COPY RCPW100.
       COPY CRPW013.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGW003.
       COPY LOGACESS.FD.

       WORKING-STORAGE SECTION.
           COPY "CRP020A.CPB".
           COPY "CRP020A.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD099             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-CRD013             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  WS-DATA-SISTEMA       PIC X(21)    VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATAWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 99.
               10  DIA-WI            PIC 99.
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  DATAWII.
               10  ANO-WII           PIC 9(4).
               10  MES-WII           PIC 99.
               10  DIA-WII           PIC 99.
           05  DATA-WII REDEFINES DATAWII PIC 9(8).
      * DATA-WII - Encontrar proxima data caso a data de vencto da conta
      * permanente seja invalida, por exemplo 30/02/1998
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZ.ZZ.ZZ.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU           PIC 9(04).
             10 WS-MES-CPU           PIC 9(02).
             10 WS-DIA-CPU           PIC 9(02).
          05 FILLER                  PIC X(13).

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       LINKAGE SECTION.
           COPY "PARAMETR".
       01  STRING-1       PIC X(65).
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE STRING-1(20: 5) TO USUARIO-W.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           MOVE DATA-INV TO DATA-DIA-I.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD099.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CRD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD013.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOG003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN I-O   CRD020 CRD013
                      LOG001 LOG002 LOG003.
           OPEN INPUT CAD018 CGD001 CXD020 CAD002 CRD001 CGD010 RCD100
                      CGD020 CAD004
           CLOSE CRD013
           OPEN I-O CRD013
           OPEN I-O CRD099.
           IF ST-CRD099 = "35"
              CLOSE CRD099      OPEN OUTPUT CRD099
              CLOSE CRD099      OPEN I-O CRD099.
           CLOSE CRD099.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.
           IF ST-CRD013 = "35"
              CLOSE CRD013      OPEN OUTPUT CRD013
              CLOSE CRD013      OPEN I-O CRD013
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD013 <> "00"
              MOVE "ERRO ABERTURA CRD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG003 <> "00"
              MOVE "ERRO ABERTURA LOG003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP020A"           to logacess-programa
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

           CLOSE      CRD013 CRD020
           OPEN INPUT CRD013 CRD020

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM GRAVA-ANOTACAO
               WHEN GS-EXCLUI-FLG-TRUE
                   MOVE 3 TO SITUACAO-CR20
                   PERFORM EXCLUI
               WHEN GS-CANCELA-FLG-TRUE
                    IF SITUACAO-CR20 = 4
                       MOVE "Título Cancelado, Deseja Reverter o Cancela
      -                     "mento ? " TO MENSAGEM
                       MOVE "Q" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                       IF RESP-MSG = "S"
                          MOVE 0 TO SITUACAO-CR20
                          PERFORM CANCELA
                       END-IF
                    ELSE
                       MOVE "Deseja Cancelar esse Título? " TO MENSAGEM
                       MOVE "Q" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                       IF RESP-MSG = "S"
                          MOVE 4 TO SITUACAO-CR20
                          PERFORM CANCELA
                       END-IF
                    END-IF
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CLIENTE-TRUE
                   PERFORM LE-CLIENTE
               WHEN GS-LE-PORTADOR-TRUE
                   PERFORM LE-PORTADOR
               WHEN GS-LE-SITUACAO-TIT-TRUE
                   PERFORM LE-SITUACAO-TIT
               WHEN GS-LE-COD-APURACAO-TRUE
                   PERFORM LE-COD-APURACAO
               WHEN GS-LE-VENDEDOR-TRUE
                   PERFORM LE-VENDEDOR
               WHEN GS-CHAMAR-APURACAO-TRUE
                   PERFORM CHAMAR-APURACAO
               WHEN GS-EMISSAO-VENCTO-TRUE
                   PERFORM INVERTE-EMIS-VENCTO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-CARREGA-COBRANCA-TRUE
                    PERFORM CARREGA-COBRANCA
               WHEN GS-EXCLUI-COBRANCA-TRUE
                    PERFORM EXCLUI-COBRANCA
               WHEN GS-GRAVAR-COBRANCA-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CR20
                    PERFORM GRAVAR-DADOS-COBRANCA
                    PERFORM CANCELA-COBRANCA
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

           MOVE "SENHA51"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT PB3
           NOT INVALID KEY
                ENABLE-OBJECT PB3.

           MOVE "SENHA64"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY
                MOVE "N"      TO GS-OK-SENHA64
           NOT INVALID KEY
                MOVE "S"      TO GS-OK-SENHA64.

           CLOSE CAD004.

       LER-CARTAO SECTION.
           MOVE GS-ACP-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
                MOVE SPACES TO NOME-CG20.
           MOVE NOME-CG20 TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL "CGP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-CARTAO.

       GRAVAR-DADOS-COBRANCA SECTION.
           CLOSE    CRD013 CRD020
           OPEN I-O CRD013 CRD020
           MOVE DATA-MOVTO-CR20         TO DATA-MOVTO-CR13
           MOVE SEQ-CR20                TO SEQ-CR13

           READ CRD013 INVALID KEY
                PERFORM MOVER-DADOS-COBRANCA
                WRITE REG-CRD013 NOT INVALID KEY
                          MOVE USUARIO-W   TO LOG3-USUARIO
                          MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                          MOVE WS-DATA-CPU TO LOG3-DATA
                          ACCEPT WS-HORA-SYS FROM TIME
                          MOVE WS-HORA-SYS TO LOG3-HORAS
                          MOVE "I"         TO LOG3-OPERACAO
                          MOVE "CRD013"    TO LOG3-ARQUIVO
                          MOVE "CRP020A"   TO LOG3-PROGRAMA
                          MOVE REG-CRD013  TO LOG3-REGISTRO
                          WRITE REG-LOG003
                          END-WRITE
                END-WRITE
           NOT INVALID KEY
                PERFORM MOVER-DADOS-COBRANCA
                REWRITE REG-CRD013 NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG3-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG3-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG3-HORAS
                        MOVE "A"         TO LOG3-OPERACAO
                        MOVE "CRD013"    TO LOG3-ARQUIVO
                        MOVE "CRP020A"   TO LOG3-PROGRAMA
                        MOVE REG-CRD013  TO LOG3-REGISTRO
                        WRITE REG-LOG003
                        END-WRITE
                END-REWRITE
           END-READ

           MOVE GS-ACP-CARTAO TO CARTAO-CRED-CR20 CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020.

           MOVE TAXA-CREDITO-CG20  TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-PARCELA-CG20  TO TAXA-ADMINIST-PARCELA-CR20


           REWRITE REG-CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP020A"   TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.

           CLOSE      CRD013 CRD020
           OPEN INPUT CRD013 CRD020.
       MOVER-DADOS-COBRANCA SECTION.
           MOVE GS-DATA-COMPRA-D        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-COMPRA-CR13

           MOVE GS-DATA-ENTRADA-D       TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-ENTRADA-CR13

           MOVE GS-DCR-MEM-D            TO DCR-MEM-CR13
           MOVE GS-STATUS-D             TO STATUS-CR13

      *    ATUALIZA DADOS DO MOVIMENTO CHD010
           MOVE GS-PORTADOR             TO PORTADOR-CR20
           MOVE GS-SITUACAO-TIT         TO SITUACAO-TIT-CR20.
       CANCELA-COBRANCA SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020
           IF SITUACAO-CR20 = 5
              CONTINUE
           ELSE
              MOVE 5                    TO SITUACAO-CR20
              PERFORM ANOTACAO-COBRANCA
           END-IF
           MOVE GS-ACP-CARTAO TO CARTAO-CRED-CR20 CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020.

           MOVE TAXA-CREDITO-CG20  TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-PARCELA-CG20  TO TAXA-ADMINIST-PARCELA-CR20

           REWRITE REG-CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "A"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP020A"   TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.

           CLOSE      CRD020
           OPEN INPUT CRD020.

       ANOTACAO-COBRANCA SECTION.
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

           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                      IF COD-COMPL-CR200 <> COD-COMPL-CR20
                         MOVE "10" TO ST-CRD200
                      ELSE
                         MOVE SEQ-CR200 TO ULT-SEQ
                         CONTINUE
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1               TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS          TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                    ADD 1 TO SEQ-CR200
                    CONTINUE
              NOT INVALID KEY
                    MOVE "10" TO ST-CRD200
              END-WRITE
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE 1               TO SUBSEQ-CR201.
           MOVE "A T E N C A O - DUPLIC. COBRANCA NR: "
                  TO ANOTACAO-CR201(1: 37)
           MOVE NR-DOCTO-CR20   TO ANOTACAO-CR201(38: 08)
           STRING NR-PARC-CR20 "/" TOT-PARC-CR20
                                           INTO ANOTACAO-CR201(50: 05)
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
           ADD 1 TO SUBSEQ-CR201
           MOVE SPACES           TO ANOTACAO-CR201
           MOVE "MOVTO: "        TO ANOTACAO-CR201(1: 07)
           MOVE DATA-MOVTO-CR20  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(08: 11)
           MOVE "VENCTO:"        TO ANOTACAO-CR201(23: 08)
           MOVE DATA-VENCTO-CR20 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(31: 11)
           MOVE "VALOR: "        TO ANOTACAO-CR201(42: 7)
           MOVE VALOR-TOT-CR20   TO VALOR-E
           MOVE VALOR-E          TO ANOTACAO-CR201(49: 13)
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
             NOT INVALID KEY
                   MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
           CLOSE CRD200 CRD201.


       EXCLUI-COBRANCA SECTION.
           CLOSE    CRD013 CRD020
           OPEN I-O CRD013 CRD020
           MOVE DATA-MOVTO-CR20         TO DATA-MOVTO-CR13
           MOVE SEQ-CR20                TO SEQ-CR13

           READ CRD013 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE CRD013 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "E"         TO LOG3-OPERACAO
                       MOVE "CRD013"    TO LOG3-ARQUIVO
                       MOVE "CRP020A"   TO LOG3-PROGRAMA
                       MOVE REG-CRD013  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
                END-DELETE
           END-READ.
           MOVE 0                       TO SITUACAO-CR20
           REWRITE REG-CRD020 INVALID KEY
                MOVE USUARIO-W   TO LOG3-USUARIO
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                MOVE WS-DATA-CPU TO LOG3-DATA
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE WS-HORA-SYS TO LOG3-HORAS
                MOVE "A"         TO LOG3-OPERACAO
                MOVE "CRD020"    TO LOG3-ARQUIVO
                MOVE "CRP020A"   TO LOG3-PROGRAMA
                MOVE REG-CRD020  TO LOG3-REGISTRO
                WRITE REG-LOG003
                END-WRITE
           END-REWRITE.

           CLOSE      CRD013 CRD020
           OPEN INPUT CRD013 CRD020.

       CARREGA-COBRANCA SECTION.
           MOVE DATA-MOVTO-CR20         TO DATA-MOVTO-CR13
           MOVE SEQ-CR20                TO SEQ-CR13
           READ CRD013 INVALID KEY
                PERFORM CARREGAR-DATAS-COBRANCA
           NOT INVALID KEY
                MOVE DATA-COMPRA-CR13     TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV             TO GS-DATA-COMPRA-D

                MOVE DATA-ENTRADA-CR13    TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV             TO GS-DATA-ENTRADA-D

                MOVE DCR-MEM-CR13         TO GS-DCR-MEM-D
                MOVE STATUS-CR13          TO GS-STATUS-D
           END-READ.

       CARREGAR-DATAS-COBRANCA SECTION.
           move function current-date to ws-data-sistema
           MOVE WS-DATA-SISTEMA(1: 8)         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                      TO GS-DATA-ENTRADA-D
           MOVE CLIENTE-CR20                  TO ALBUM-REC
           READ RCD100 INVALID KEY
                MOVE ZEROS TO GS-DATA-COMPRA-D
           NOT INVALID KEY
                MOVE DATAVEN-REC          TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV             TO GS-DATA-COMPRA-D
           END-READ.


       CHAMAR-APURACAO SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
           PERFORM LE-COD-APURACAO.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CLIENTE
             WHEN 2 PERFORM CARREGA-POP-UP-VENDEDOR
             WHEN 3 CALL "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CAP018T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-PORTADOR
                    MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR
             WHEN 4 CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CXP020T"
                    MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
                    PERFORM LE-COD-APURACAO
                    MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO
             WHEN 5 CALL "CRP001T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CRP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SITUACAO-TIT
                    MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT
           END-EVALUATE.
       CARREGA-POP-UP-CLIENTE SECTION.
           MOVE GS-LINDET1(1: 1) TO COMPRADOR-CG10 LETRA.
      *    MOVE SPACES TO NOME-CG01.
           START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END
                 MOVE "10" TO ST-CGD010
              NOT AT END
                  MOVE COMPRADOR-CG10 TO LETRA1
                  IF LETRA1 NOT = LETRA
                     MOVE "10" TO ST-CGD010
                  ELSE
                     CONTINUE
                     MOVE COMPRADOR-CG10  TO GS-LINDET1(1: 32)
                     MOVE CODIGO-CG10     TO GS-LINDET1(33: 08)
                     MOVE CLASSIF-CG10    TO GS-LINDET1(43: 1)
                     MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-POP-UP-VENDEDOR SECTION.
           MOVE GS-LINDET1(1: 1) TO NOME-CG01 LETRA.
           MOVE "CLEAR-LIST-BOX-VEND" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE SPACES TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
                 READ CGD001 NEXT RECORD AT END
                      MOVE "10" TO ST-CGD001
                 NOT AT END
                      MOVE NOME-CG01     TO LETRA1
                      IF LETRA1 NOT = LETRA
                         MOVE "10" TO ST-CGD001
                      ELSE
                         CONTINUE
                         IF T-VEND-CG01 = 0
                            CONTINUE
                         ELSE
                            MOVE NOME-CG01     TO GS-LINDET1(1: 32)
                            MOVE CODIGO-CG01   TO GS-LINDET1(33: 06)
                            MOVE "INSERE-POP-UP-VENDEDOR"
                                               TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           IF GS-OPCAO-POP-UP = 4
              PERFORM ITEM-SELECIONADO-APURACAO
           ELSE
              IF GS-OPCAO-POP-UP = 2
                 MOVE GS-LINDET1(33: 6)      TO GS-VENDEDOR
                 MOVE GS-LINDET1(1: 30)      TO GS-DESCR-VENDEDOR
              ELSE
                 MOVE GS-LINDET1(33: 8)      TO GS-COD-CLIENTE
                 MOVE GS-LINDET1(43: 1)      TO CLASSIF-W
                 EVALUATE CLASSIF-W
                    WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
                    WHEN 1 MOVE "1-Comum"          TO GS-CLASSIFICACAO
                    WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
                 END-EVALUATE
                 MOVE GS-LINDET1(1: 30)      TO GS-DESCR-CLIENTE.

       ITEM-SELECIONADO-APURACAO SECTION.
           MOVE GS-LINDET1(52: 5)TO GS-COD-APURACAO.
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO.
       CARREGA-POP-UP-APURACAO SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CODIGO-COMPL-CX20.
           START CXD020 KEY IS NOT < CODIGO-COMPL-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
                 READ CXD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD020
                 NOT AT END
                      MOVE SPACES TO GS-LINDET1
                      MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                      EVALUATE GRAU-CX20
                        WHEN 1 PERFORM GRAU-1
                        WHEN 2 PERFORM GRAU-2
                        WHEN 3 PERFORM GRAU-3
                        WHEN 4 PERFORM GRAU-4
                      END-EVALUATE
                      MOVE "INSERE-POP-UP-APUR" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
       GRAU-1 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(1: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 03).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(4: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 03).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(7: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 03).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO GS-LINDET1(10: 11)
           MOVE DESCRICAO-CX20    TO GS-LINDET1(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO GS-LINDET1(52: 03).

       INVERTE-EMIS-VENCTO SECTION.
           MOVE GS-DATA-EMISSAO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-EMISSAO-INV.
           MOVE GS-DATA-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-VENCTO-INV.
       EXCLUI SECTION.
           CLOSE    CRD020 CRD013
           OPEN I-O CRD099 CRD020 CRD013
           MOVE REG-CRD020 TO REG-CRD099
           MOVE USUARIO-W  TO USUARIO-EXCLUSAO-CR99
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
           DATA-EXCLUSAO-CR99
           ACCEPT HORA-EXCLUSAO-CR99 FROM TIME
           WRITE REG-CRD099 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "I"         TO LOG3-OPERACAO
                 MOVE "CRD099"    TO LOG3-ARQUIVO
                 MOVE "CRP020A"   TO LOG3-PROGRAMA
                 MOVE REG-CRD099  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.
           DELETE CRD020 NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "E"         TO LOG3-OPERACAO
                 MOVE "CRD020"    TO LOG3-ARQUIVO
                 MOVE "CRP020A"   TO LOG3-PROGRAMA
                 MOVE REG-CRD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE.

           CLOSE CRD099.

           MOVE DATA-MOVTO-CR20   TO DATA-MOVTO-CR13
           MOVE SEQ-CR20          TO SEQ-CR13
           READ CRD013 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE CRD013 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG3-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG3-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG3-HORAS
                       MOVE "E"         TO LOG3-OPERACAO
                       MOVE "CRD013"    TO LOG3-ARQUIVO
                       MOVE "CRP020A"   TO LOG3-PROGRAMA
                       MOVE REG-CRD013  TO LOG3-REGISTRO
                       WRITE REG-LOG003
                       END-WRITE
               END-DELETE
           END-READ.

           CLOSE      CRD020 CRD013
           OPEN INPUT CRD020 CRD013.

       CANCELA SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020
           REWRITE REG-CRD020 NOT INVALID KEY
                MOVE USUARIO-W   TO LOG3-USUARIO
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                MOVE WS-DATA-CPU TO LOG3-DATA
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE WS-HORA-SYS TO LOG3-HORAS
                MOVE "A"         TO LOG3-OPERACAO
                MOVE "CRD020"    TO LOG3-ARQUIVO
                MOVE "CRP020A"   TO LOG3-PROGRAMA
                MOVE REG-CRD020  TO LOG3-REGISTRO
                WRITE REG-LOG003
                END-WRITE.

           CLOSE      CRD020
           OPEN INPUT CRD020.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR            TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "********"        TO NOME-CG01.
           MOVE NOME-CG01              TO GS-DESCR-VENDEDOR.
       LE-CLIENTE SECTION.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10.
           MOVE GS-COD-CLIENTE         TO CODIGO-CG10.
           READ CGD010 INVALID KEY
                MOVE "********"        TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10         TO GS-DESCR-CLIENTE.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR            TO PORTADOR.
           READ CAD018 INVALID KEY
                 MOVE "******"         TO NOME-PORT.
           MOVE NOME-PORT              TO GS-DESCR-PORTADOR.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO        TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"           TO DESCRICAO-CX20
           MOVE ZEROS                  TO TIPO-CONTA-CX20.
           MOVE DESCRICAO-CX20         TO GS-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
              MOVE 0                   TO GS-TIPO-CONTA-APUR
           ELSE
              MOVE 1                   TO GS-TIPO-CONTA-APUR.
       LE-SITUACAO-TIT SECTION.
           MOVE GS-SITUACAO-TIT        TO CODIGO-CR01.
           READ CRD001 INVALID KEY
                MOVE "*********"       TO SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01      TO GS-DESCR-SITUACAO-TIT.
       CARREGAR-DADOS SECTION.
           MOVE STRING-1(1: 9)         TO COD-COMPL-CR20
           MOVE STRING-1(10: 5)        TO SEQ-CR20
           START CRD020 KEY IS = CHAVE-CR20 INVALID KEY
                CONTINUE.
           READ CRD020 INVALID KEY
                INITIALIZE REG-CRD020.
           MOVE DATA-MOVTO-CR20        TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV               TO GS-DATA-MOVTO.
           EVALUATE CLASS-CLIENTE-CR20
             WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum   "       TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
           END-EVALUATE
           MOVE CLASS-CLIENTE-CR20     TO CLASSIF-CG10.
           MOVE CLIENTE-CR20           TO GS-COD-CLIENTE
                                          CODIGO-CG10.
           READ CGD010 INVALID KEY
                MOVE "*****"           TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10         TO GS-DESCR-CLIENTE.
           MOVE PORTADOR-CR20          TO GS-PORTADOR PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "******"          TO NOME-PORT.
           MOVE NOME-PORT              TO GS-DESCR-PORTADOR.
           EVALUATE CARTEIRA-CR20
             WHEN 1 MOVE "1-Simples "  TO GS-CARTEIRA
             WHEN 2 MOVE "2-Caução  "  TO GS-CARTEIRA
             WHEN 3 MOVE "3-Desconto"  TO GS-CARTEIRA
           END-EVALUATE
           MOVE NR-DOCTO-CR20          TO GS-NR-DOCTO.
           MOVE OUTRO-DOCTO-CR20       TO GS-OUTRO-DOCTO.
           MOVE NR-NOTA-FISCAL-CR20    TO GS-NR-NOTA-FISCAL.
           MOVE DATA-NTA-FISCAL-CR20   TO GS-DATA-NTA-FISCAL.
           MOVE DATA-EMISSAO-CR20      TO GS-DATA-EMISSAO.
           MOVE DATA-VENCTO-CR20       TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV               TO GS-DATA-VENCTO.
           MOVE DESCRICAO-CR20         TO GS-DESCRICAO.
           MOVE DIGITADOR-CR20         TO GS-DIGITADOR.
           MOVE TIPO-MOEDA-CR20        TO GS-TIPO-MOEDA.
           EVALUATE TIPO-MOEDA-CR20
             WHEN 0 MOVE "-Real"       TO GS-TIPO-MOEDA(2: 6)
             WHEN 1 MOVE "-Dolar"      TO GS-TIPO-MOEDA(2: 5)
           END-EVALUATE
           MOVE SITUACAO-TIT-CR20      TO GS-SITUACAO-TIT CODIGO-CR01.
           READ CRD001 INVALID KEY
                MOVE "********"        TO SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01      TO GS-DESCR-SITUACAO-TIT.
           MOVE CODREDUZ-APUR-CR20     TO GS-COD-APURACAO
                                          CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"           TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20         TO GS-DESCR-APURACAO.
           MOVE TIPO-DOCTO-CR20        TO GS-TIPO-DOCTOW
           EVALUATE TIPO-DOCTO-CR20
             WHEN 0 MOVE "0-Boleto           " TO GS-TIPO-DOCTO(1: 19)
             WHEN 1 MOVE "1-Dupl/Promis      " TO GS-TIPO-DOCTO(1: 19)
             WHEN 2 MOVE "2-Org.Evento       " TO GS-TIPO-DOCTO(1: 19)
             WHEN 3 MOVE "3-Debito Automatico" TO GS-TIPO-DOCTO(1: 19)
             WHEN 4 MOVE "4-Cartao de Credito" TO GS-TIPO-DOCTO(1: 19)
           END-EVALUATE
           MOVE VALOR-TOT-CR20         TO GS-VALOR-TOTAL.
           MOVE VENDEDOR-CR20          TO GS-VENDEDOR CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "*****"           TO NOME-CG01
           END-READ
           MOVE NOME-CG01              TO GS-DESCR-VENDEDOR
           MOVE RESPONSAVEL-CR20       TO GS-RESPONSAVEL.

           MOVE SITUACAO-CR20          TO GS-SITUACAO.
           MOVE SITUACAO-TIT-CR20      TO GS-SITUACAO-TIT
                                          CODIGO-CR01.
           READ CRD001 INVALID KEY
                MOVE "********"        TO SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01      TO GS-DESCR-SITUACAO-TIT.
           MOVE CARTAO-CRED-CR20       TO GS-ACP-CARTAO CODIGO-CG20
           READ CGD020 INVALID KEY
               MOVE SPACES             TO NOME-CG20.

           MOVE NOME-CG20              TO GS-DESC-CARTAO

           MOVE VALOR-SALDO-CR20               TO GS-VALOR-SALDO.

           REFRESH-OBJECT PRINCIPAL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       SALVAR-DADOS SECTION.
           CLOSE      CRD020
           OPEN I-O   CRD020
           MOVE GS-DATA-MOVTO              TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-MOVTO-CR20.
           IF GS-CLASSIFICACAO = SPACES
              MOVE "0"                     TO CLASS-CLIENTE-CR20
           ELSE
              MOVE GS-CLASSIFICACAO(1: 1)  TO CLASS-CLIENTE-CR20.
           MOVE GS-COD-CLIENTE             TO CLIENTE-CR20
           MOVE GS-PORTADOR                TO PORTADOR-CR20.

           IF GS-CARTEIRA = SPACES
              MOVE "0" TO CARTEIRA-CR20
           ELSE
              MOVE GS-CARTEIRA(1: 1) TO CARTEIRA-CR20.

           IF GS-TIPO-DOCTO = SPACES
              MOVE "0" TO TIPO-DOCTO-CR20
           ELSE
              MOVE GS-TIPO-DOCTO(1: 1) TO TIPO-DOCTO-CR20.

           IF GS-SITUACAO-TIT = SPACES
              MOVE "00" TO SITUACAO-TIT-CR20
           ELSE
              MOVE GS-SITUACAO-TIT(1: 2) TO SITUACAO-TIT-CR20.

           MOVE GS-NR-DOCTO       TO NR-DOCTO-CR20.
           MOVE GS-OUTRO-DOCTO    TO OUTRO-DOCTO-CR20.
           MOVE GS-DATA-EMISSAO   TO DATA-EMISSAO-CR20
           MOVE GS-VENCTO-INV     TO DATA-VENCTO-CR20
           MOVE GS-DESCRICAO      TO DESCRICAO-CR20

           IF GS-TIPO-MOEDA = SPACES
              MOVE "0" TO TIPO-MOEDA-CR20
           ELSE
              MOVE GS-TIPO-MOEDA(1: 1) TO TIPO-MOEDA-CR20.

           MOVE GS-COD-APURACAO        TO CODREDUZ-APUR-CR20
           MOVE GS-RESPONSAVEL         TO RESPONSAVEL-CR20
           MOVE USUARIO-W              TO DIGITADOR-CR20.
           MOVE GS-VALOR-TOTAL         TO VALOR-TOT-CR20.
           MOVE GS-VENDEDOR            TO VENDEDOR-CR20
           MOVE GS-NR-NOTA-FISCAL      TO NR-NOTA-FISCAL-CR20.
           MOVE GS-DATA-NTA-FISCAL     TO DATA-NTA-FISCAL-CR20.

           MOVE GS-ACP-CARTAO          TO CARTAO-CRED-CR20 CODIGO-CG20
           READ CGD020 INVALID KEY
               INITIALIZE REG-CGD020.

           MOVE TAXA-CREDITO-CG20      TO TAXA-ADMINIST-CREDITO-CR20
           MOVE TAXA-PARCELA-CG20      TO TAXA-ADMINIST-PARCELA-CR20

      *    MOVE GS-VALOR-TOTAL         TO VALOR-SALDO-CR20

           IF SITUACAO-CR20 = 0
           IF GS-VALOR-TOTAL <> GS-VALOR-SALDO
              MOVE "Valor Informado Diferente do Valor do Saldo do Chequ
      -            "e Deseja Atualiza-lo ?" TO MENSAGEM
              MOVE "Q" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              IF RESP-MSG = "S"
                 MOVE GS-VALOR-TOTAL TO VALOR-SALDO-CR20
              ELSE
                 MOVE GS-VALOR-SALDO TO VALOR-SALDO-CR20.

           REWRITE REG-CRD020 INVALID KEY
                PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
                MOVE USUARIO-W   TO LOG3-USUARIO
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                MOVE WS-DATA-CPU TO LOG3-DATA
                ACCEPT WS-HORA-SYS FROM TIME
                MOVE WS-HORA-SYS TO LOG3-HORAS
                MOVE "A"         TO LOG3-OPERACAO
                MOVE "CRD020"    TO LOG3-ARQUIVO
                MOVE "CRP020A"   TO LOG3-PROGRAMA
                MOVE REG-CRD020  TO LOG3-REGISTRO
                WRITE REG-LOG003
                END-WRITE
                CONTINUE.

           CLOSE      CRD020
           OPEN INPUT CRD020.

       GRAVA-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"
              CLOSE CRD200  OPEN OUTPUT CRD200
              CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"
              CLOSE CRD201  OPEN OUTPUT CRD201
              CLOSE CRD201  OPEN I-O CRD201.

           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                 READ CRD200 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD200
                 NOT AT END
                      IF COD-COMPL-CR200 <> COD-COMPL-CR20
                         MOVE "10" TO ST-CRD200
                      ELSE
                         MOVE SEQ-CR200 TO ULT-SEQ
                         CONTINUE
                 END-READ
           END-PERFORM.
           ADD 1               TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS          TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                   ADD 1 TO SEQ-CR200
                   CONTINUE
               NOT INVALID KEY
                   MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE 1              TO SUBSEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.
           MOVE "ALTERACAO EFETUADA NO TITULO            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 40)
           MOVE NR-DOCTO-CR20  TO ANOTACAO-CR201(30: 10).
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

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
              END-IF
           END-PERFORM.
           CLOSE CRD200 CRD201.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP020A" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

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
           move "CRP020A"           to logacess-programa
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

           CLOSE CAD018 CRD001 CRD020 CGD001 CGD010
                 CXD020 CAD002 RCD100 CRD013 CGD020 LOG001 LOG002 LOG003
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
