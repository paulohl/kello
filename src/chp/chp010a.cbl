       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP010A.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 21/06/1999.
      *FUNÇÃO: Movimento de CHEQUES

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CAPX004.
           COPY CGPX001.
           COPY CGPX010.
           COPY CXPX020.
           COPY CAPX018.
           COPY CRPX001.
           COPY CHPX010.
           COPY CHPX011.
           COPY CHPX012.
           COPY CHPX013.
           COPY CHPX099.
           COPY CRPX200.
           COPY CRPX201.
           COPY RCPX100.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CAPW004.
       COPY CGPW001.
       COPY CGPW010.
       COPY CXPW020.
       COPY CAPW018.
       COPY CRPW001.
       COPY CHPW010.
       COPY CHPW011.
       COPY CHPW012.
       COPY CHPW013.
       COPY CHPW099.
       COPY CRPW200.
       COPY CRPW201.
       COPY RCPW100.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CHP010A.CPB".
           COPY "CHP010A.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD011             PIC XX       VALUE SPACES.
           05  ST-CHD012             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  WS-DATA-SISTEMA       PIC X(21)    VALUE SPACES.
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

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  FLAG-ACESSO           PIC 9(01)    VALUE 3.
               88  ACESSO-MOVTO                   VALUE 1.
               88  ACESSO-DEVOLV                  VALUE 2.
               88  ACESSO-NEGAD                   VALUE 3.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

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

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
           COPY "PARAMETR".
       01  STRING-1       PIC X(65).
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.

           PERFORM VALIDAR-ACESSO.

           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE STRING-1(20: 5) TO USUARIO-W.
           MOVE STRING-1(26: 3) TO COD-USUARIO-W

           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.

           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).

           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I DATA-MOVTO-I.

           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD011.
           MOVE "CHD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD012.
           MOVE "CHD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CHD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O   CHD010 CHD099 CHD011 CHD013
           CLOSE      CHD010 CHD099 CHD011 CHD013
           OPEN INPUT CHD010 CHD099 CHD011 CHD013.

           OPEN INPUT CAD018 CGD001 CXD020 CAD002 CGD010 CRD001 CHD012
                      CAD004 RCD100.
           OPEN I-O CHD099.
           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O CHD099.
           CLOSE CHD099.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.
           IF ST-CHD011 = "35"
              CLOSE CHD011      OPEN OUTPUT CHD011
              CLOSE CHD011      OPEN I-O CHD011
           END-IF.
           IF ST-CHD013 = "35"
              CLOSE CHD013      OPEN OUTPUT CHD013
              CLOSE CHD013      OPEN I-O CHD013
           END-IF.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD011 <> "00"
              MOVE "ERRO ABERTURA CHD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD012 <> "00"
              MOVE "ERRO ABERTURA CHD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CHP010A"           to logacess-programa
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

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       VALIDAR-ACESSO SECTION.
      *    VALIDA ACESSO - SE TEM ACESSO CHP010-MOVTO CHEQUES SEM BLOQ.
      *    SE TEM ACESSO A SENHA01-MANUTENCAO DEVOLVIDO, ENTAO SOMENTE
      *    AO BOTAO DEVOLVIDO, SENAO TODOS OS BOTOES DESABILITADO
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "CHP010"      TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE COD-USUARIO-W   TO COD-USUARIO-CA004
                MOVE "SENHA01"       TO PROGRAMA-CA004
                READ CAD004 INVALID KEY
                     MOVE COD-USUARIO-W   TO COD-USUARIO-CA004
                     MOVE "SENHA12"       TO PROGRAMA-CA004
                     READ CAD004 INVALID KEY
      *                   NEGADO
                          MOVE 3  TO GS-TIPO-ACESSO
                     NOT INVALID KEY
      *                   DEVOLVIDO
                          MOVE 2  TO GS-TIPO-ACESSO
                     END-READ
                   NOT INVALID KEY
      *              DEVOLVIDO
                     MOVE 2  TO GS-TIPO-ACESSO
                END-READ
             NOT INVALID KEY
      *         ACESSO A MOVTO DE CHEQUES
                MOVE 1       TO GS-TIPO-ACESSO
           END-READ.
      *
      *    EVALUATE TRUE
      *       WHEN ACESSO-NEGAD
      *            MOVE "DESABILITA-OBJETOS" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *       WHEN ACESSO-MOVTO
      *            MOVE "ACESSO-MANUTENCAO" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *       WHEN ACESSO-DEVOLV
      *            MOVE "ACESSO-DEVOLVIDO" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *       WHEN OTHER
      *            MOVE "DESABILITA-OBJETOS" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *    END-EVALUATE.
      *    PERFORM CARREGAR-DADOS
      *    PERFORM CLEAR-FLAGS.
      *    PERFORM CALL-DIALOG-SYSTEM.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM REGRAVA-DADOS
                   PERFORM GRAVA-ANOTACAO
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   MOVE USUARIO-W TO DIGITADOR-CH10
                   MOVE 3 TO SITUACAO-CH10
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-DEVOLVIDO-FLG-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CH10

                    PERFORM GRAVAR-DADOS-DEVOLVIDO
                    PERFORM CANCELA-DEVOLVIDO
               WHEN GS-EXCLUI-DEVOLVIDO-TRUE
                    PERFORM EXCLUI-DEVOLVIDO
               WHEN GS-VERIFICA-SENHA-TRUE
                    PERFORM VERIFICA-ACESSO-DEVOL
               WHEN GS-CARREGAR-DADOS-DEV-TRUE
                    PERFORM CARREGAR-DADOS-DEVOLVIDO
               WHEN GS-BAIXAR-DEVOLVIDO-TRUE
                    PERFORM BAIXAR-CHEQUE-DEVOLVIDO
               WHEN GS-PROBLEMATICO-FLG-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CH10
                    IF SITUACAO-CH10 = 6
                       MOVE 0 TO SITUACAO-CH10
                    ELSE MOVE 6 TO SITUACAO-CH10
                    END-IF
                    PERFORM CANCELA-PROBLEMATICO
               WHEN GS-CARREGAR-DADOS-TRUE
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
               WHEN GS-VALIDA-CPF-TRUE
                    PERFORM VALIDA-CPF
               WHEN GS-LE-ORIGEM-TRUE
                    PERFORM LE-ORIGEM
               WHEN GS-REGRAVA-CGD010-TRUE
                    PERFORM REGRAVAR-CGD010
               WHEN GS-VERIF-SENHA-TRUE
                    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
                    MOVE "SENHA60"          TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                         MOVE "S" TO GS-OK
                    NOT INVALID KEY
                         MOVE "N" TO GS-OK
                    END-READ
               WHEN GS-VERIFICAR-SENHA61-TRUE
                    MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
                    MOVE "SENHA61"          TO PROGRAMA-CA004
                    READ CAD004 INVALID KEY
                         MOVE "N"           TO GS-OK
                    NOT INVALID KEY
                         MOVE "S"           TO GS-OK
                    END-READ
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       REGRAVAR-CGD010 SECTION.
           CLOSE      CGD010
           OPEN I-O   CGD010

           MOVE "Você deseja realmente Regravar o Nome ?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG EQUAL "S"
              MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10
              MOVE GS-COD-CLIENTE         TO CODIGO-CG10
              READ CGD010 NOT INVALID KEY
                   MOVE GS-NOME TO COMPRADOR-CG10
                   REWRITE REG-CGD010 INVALID KEY
                       MOVE "Erro de Regravação...CGD010" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

           CLOSE      CGD010
           OPEN INPUT CGD010.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       VERIFICA-ACESSO-DEVOL SECTION.
           MOVE ZEROS TO GS-AUTORIZADO GS-TIPO-ACESSO-SENHA.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA01"     TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                MOVE 0 TO GS-AUTORIZADO
                MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                MOVE "SENHA12"     TO PROGRAMA-CA004
                READ CAD004 INVALID KEY
                     MOVE 0 TO GS-AUTORIZADO
                NOT INVALID KEY
                     MOVE 1 TO GS-AUTORIZADO
                     MOVE 2 TO GS-TIPO-ACESSO-SENHA
                END-READ
           NOT INVALID KEY
                MOVE 1 TO GS-AUTORIZADO
                MOVE 1 TO GS-TIPO-ACESSO-SENHA
           END-READ.

       VALIDA-CPF SECTION.
           MOVE 0 TO GS-CPF-CORRETO.
           IF GS-CPF <> ZEROS
              MOVE GS-CPF TO GRDCIC-CIC
              CALL "GRDCIC" USING PARAMETROS-GRDCIC
              CANCEL "GRDCIC"
              IF GRDCIC-RETORNO = "00"
                 MOVE ZEROS TO GS-CPF-CORRETO
              ELSE MOVE 1 TO GS-CPF-CORRETO.
      *    0-CORRETO  1-INCORRETO
       CHAMAR-APURACAO SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
           PERFORM LE-COD-APURACAO.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CLIENTE
             WHEN 2 PERFORM CARREGA-POP-UP-VENDEDOR
             WHEN 3 CALL    "CAP018T" USING PARAMETROS-W
                                            PASSAR-PARAMETROS
                    CANCEL "CAP018T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-PORTADOR
                    MOVE PASSAR-STRING-1(33: 4) TO GS-PORTADOR
             WHEN 4 CALL   "CXP020T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CXP020T"
                    MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
                    PERFORM LE-COD-APURACAO
                    MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO
             WHEN 5 CALL   "CRP001T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CRP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SITUACAO-TIT
                    MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT
             WHEN 6 CALL   "CHP012T" USING PARAMETROS-W
                                           PASSAR-PARAMETROS
                    CANCEL "CHP012T"
                    MOVE PASSAR-STRING-1(1: 10) TO GS-DESCR-ORIGEM
                    MOVE PASSAR-STRING-1(33: 2) TO GS-ORIGEM
           END-EVALUATE.
       CARREGA-POP-UP-CLIENTE SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO COMPRADOR-CG10.
           START CGD010 KEY IS NOT < COMPRADOR-CG10 INVALID KEY
                 MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
              READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
                NOT AT END
               MOVE COMPRADOR-CG10(1: I) TO INICIAL-A-COMPARAR
               IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-CGD010
               ELSE
                  MOVE COMPRADOR-CG10  TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CG10     TO GS-LINDET1(33: 08)
                  MOVE CLASSIF-CG10    TO GS-LINDET1(43: 1)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       CARREGA-POP-UP-VENDEDOR SECTION.
           MOVE GS-LINDET1(1: 1) TO NOME-CG01 LETRA.
           MOVE "CLEAR-LIST-BOX-VEND" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE SPACES TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
                NOT AT END
                  MOVE NOME-CG01     TO LETRA1
                  IF LETRA1 NOT = LETRA MOVE "10" TO ST-CGD001
                  ELSE CONTINUE
                  IF T-VEND-CG01 = 0 CONTINUE
                  ELSE
                  MOVE NOME-CG01     TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CG01   TO GS-LINDET1(33: 06)
                  MOVE "INSERE-POP-UP-VENDEDOR" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           IF GS-OPCAO-POP-UP = 4
              PERFORM ITEM-SELECIONADO-APURACAO
           ELSE
            IF GS-OPCAO-POP-UP = 2
                MOVE GS-LINDET1(33: 6) TO GS-VENDEDOR
                MOVE GS-LINDET1(1: 30) TO GS-DESCR-VENDEDOR
            ELSE MOVE GS-LINDET1(33: 8) TO GS-COD-CLIENTE
                 MOVE GS-LINDET1(43: 1) TO CLASSIF-W
                 EVALUATE CLASSIF-W
                    WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
                    WHEN 1 MOVE "1-Comum"          TO GS-CLASSIFICACAO
                    WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
                 END-EVALUATE
                 MOVE GS-LINDET1(1: 30) TO GS-DESCR-CLIENTE.

       ITEM-SELECIONADO-APURACAO SECTION.
           MOVE GS-LINDET1(52: 5)TO GS-COD-APURACAO.
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO.
       CARREGA-POP-UP-APURACAO SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CODIGO-COMPL-CX20.
              START CXD020 KEY IS NOT < CODIGO-COMPL-CX20
                    INVALID KEY MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
              READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
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
           CLOSE    CHD010 CHD013
           OPEN I-O CHD010 CHD013

           OPEN I-O CHD099
           MOVE REG-CHD010 TO REG-CHD099
           MOVE USUARIO-W  TO USUARIO-EXCLUSAO-CH99
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
           DATA-EXCLUSAO-CH99
           ACCEPT HORA-EXCLUSAO-CH99 FROM TIME

           WRITE REG-CHD099.
           DELETE CHD010.
           MOVE DATA-MOVTO-CH10   TO DATA-MOVTO-CH13
           MOVE SEQ-CH10          TO SEQ-CH13
           READ CHD013 INVALID KEY
                CONTINUE
              NOT INVALID KEY
                DELETE CHD013
           END-READ

           CLOSE CHD099 CHD010 CHD013
           OPEN INPUT   CHD010 CHD013.
           PERFORM LIMPAR-DADOS.
       EXCLUI-DEVOLVIDO SECTION.
           CLOSE    CHD010 CHD013
           OPEN I-O CHD010 CHD013
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
           MOVE SEQ-CH10                TO SEQ-CH13

           READ CHD013 INVALID KEY
                CONTINUE

              NOT INVALID KEY
                  DELETE CHD013
           END-READ.
           MOVE 0                       TO SITUACAO-CH10
           REWRITE REG-CHD010
           END-REWRITE.
           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013.

       BAIXAR-CHEQUE-DEVOLVIDO SECTION.
           MOVE 2                       TO SITUACAO-CH10

           PERFORM GRAVAR-DADOS-DEVOLVIDO.
       CARREGAR-DADOS-DEVOLVIDO SECTION.
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
           MOVE SEQ-CH10                TO SEQ-CH13
           READ CHD013 INVALID KEY
                PERFORM CARREGAR-DATAS-DEVOLVIDO
           NOT INVALID KEY
               MOVE ALINEA-CH13          TO GS-ALINEA-D

               MOVE DATA-COMPRA-CH13     TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-COMPRA-D

               MOVE DATA-ENTRADA-CH13    TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-ENTRADA-D

               MOVE DATA-APRES-CH13      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-APRES-D

               MOVE DATA-REAPRES-CH13    TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-REPRES-D

               MOVE DATA-RECTO-CH13      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-RECTO-D

               MOVE VLR-JUROS-CH13       TO GS-VLR-JUROS-D
               MOVE VLR-MULTA-CH13       TO GS-VLR-MULTA-D
               MOVE VLR-DESCONTO-CH13    TO GS-VLR-DESCONTO-D
               MOVE FORMA-PAGTO-CH13     TO GS-FORMA-PAGTO-D
               MOVE DCR-MEM-CH13         TO GS-DCR-MEM-D
               MOVE DCR-MEM-R-CH13       TO GS-DCR-MEM-R
           END-READ.
       CARREGAR-DATAS-DEVOLVIDO SECTION.
           move function current-date to ws-data-sistema
           MOVE WS-DATA-SISTEMA(1: 8)         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                      TO GS-DATA-ENTRADA-D
           MOVE CLIENTE-CH10                  TO ALBUM-REC
           READ RCD100 INVALID KEY MOVE ZEROS TO GS-DATA-COMPRA-D
                NOT INVALID KEY
                    MOVE DATAVEN-REC          TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV             TO GS-DATA-COMPRA-D
           END-READ.

       GRAVAR-DADOS-DEVOLVIDO SECTION.
           CLOSE    CHD010 CHD013
           OPEN I-O CHD010 CHD013
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
           MOVE SEQ-CH10                TO SEQ-CH13

           READ CHD013 INVALID KEY
                PERFORM MOVER-DADOS-DEVOLVIDO
                WRITE REG-CHD013
                END-WRITE
              NOT INVALID KEY
                  PERFORM MOVER-DADOS-DEVOLVIDO
                  REWRITE REG-CHD013
                  END-REWRITE
           END-READ

           REWRITE REG-CHD010.
           CLOSE      CHD010 CHD013
           OPEN INPUT CHD010 CHD013.
       MOVER-DADOS-DEVOLVIDO SECTION.
           MOVE GS-ALINEA-D             TO ALINEA-CH13
           MOVE GS-DATA-COMPRA-D        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-COMPRA-CH13

           MOVE GS-DATA-ENTRADA-D       TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-ENTRADA-CH13

           MOVE GS-DATA-APRES-D         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-APRES-CH13

           MOVE GS-DATA-REPRES-D        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-REAPRES-CH13

           MOVE ZEROS                   TO DATA-RECTO-CH13

      *    MOVE GS-DATA-RECTO-D         TO DATA-INV
      *    CALL "GRIDAT2" USING DATA-INV
      *    MOVE DATA-INV                TO DATA-RECTO-CH13

           MOVE GS-VLR-JUROS-D          TO VLR-JUROS-CH13
           MOVE GS-VLR-MULTA-D          TO VLR-MULTA-CH13
           MOVE GS-VLR-DESCONTO-D       TO VLR-DESCONTO-CH13
           MOVE GS-FORMA-PAGTO-D        TO FORMA-PAGTO-CH13
           MOVE GS-DCR-MEM-D            TO DCR-MEM-CH13
           MOVE GS-DCR-MEM-R            TO DCR-MEM-R-CH13

      *    ATUALIZA DADOS DO MOVIMENTO CHD010
           MOVE GS-PORTADOR             TO PORTADOR-CH10
           MOVE GS-SITUACAO-TIT         TO SITUACAO-TIT-CH10.
       CANCELA-DEVOLVIDO SECTION.
           CLOSE CHD010
           OPEN I-O CHD010
           IF SITUACAO-CH10 = 5
              CONTINUE
           ELSE
              MOVE 5                    TO SITUACAO-CH10
              PERFORM ANOTACAO-DEVOLVIDO
           END-IF
           REWRITE REG-CHD010.

           CLOSE CHD010
           OPEN INPUT CHD010.

       CANCELA-PROBLEMATICO SECTION.
           CLOSE CHD010
           OPEN I-O CHD010
           REWRITE REG-CHD010.
           CLOSE CHD010
           OPEN INPUT CHD010
           PERFORM LIMPAR-DADOS.
       ANOTACAO-DEVOLVIDO SECTION.
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
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-CH10
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE 1               TO SUBSEQ-CR201.
           MOVE "A T E N C A O - CHEQUE DEVOLVIDO NR: "
                  TO ANOTACAO-CR201(1: 37)
           MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(38: 08)
           MOVE NOME-CH10       TO ANOTACAO-CR201(50: 30)
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
           MOVE DATA-MOVTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(08: 11)
           MOVE "VENCTO:"        TO ANOTACAO-CR201(23: 08)
           MOVE DATA-VENCTO-CH10 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(31: 11)
           MOVE "VALOR: "        TO ANOTACAO-CR201(42: 7)
           MOVE VALOR-CH10       TO VALOR-E
           MOVE VALOR-E          TO ANOTACAO-CR201(49: 13)
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
           CLOSE CRD200 CRD201.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01      TO GS-DESCR-VENDEDOR.
       LE-CLIENTE SECTION.
           MOVE GS-CLASSIFICACAO(1: 1) TO CLASSIF-CG10.
           MOVE GS-COD-CLIENTE   TO CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "********" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10    TO GS-DESCR-CLIENTE.
       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR    TO PORTADOR
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT
           END-READ
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20
                                   MOVE ZEROS TO TIPO-CONTA-CX20.
           MOVE DESCRICAO-CX20    TO GS-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
               MOVE 0 TO GS-TIPO-CONTA-APUR
           ELSE MOVE 1 TO GS-TIPO-CONTA-APUR.
       LE-SITUACAO-TIT SECTION.
           MOVE GS-SITUACAO-TIT TO CODIGO-CR01
           READ CRD001 INVALID KEY MOVE "*********" TO
                                       SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-TIT-CR01  TO GS-DESCR-SITUACAO-TIT.

       LE-ORIGEM SECTION.
           MOVE GS-ORIGEM TO CODIGO-CH12.
           READ CHD012 INVALID KEY MOVE SPACES TO DESCR-ORIGEM-CH12.
           MOVE DESCR-ORIGEM-CH12 TO GS-DESCR-ORIGEM.
       CARREGAR-DADOS SECTION.
           MOVE STRING-1(1: 8) TO DATA-MOVTO-CH10.
           MOVE STRING-1(10: 4) TO SEQ-CH10.
           START CHD010 KEY IS = CHAVE-CH10 INVALID KEY CONTINUE.
           READ CHD010 INVALID KEY INITIALIZE REG-CHD010.
           MOVE DATA-MOVTO-CH10    TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV           TO GS-DATA-MOVTO.
           EVALUATE CLASS-CLIENTE-CH10
             WHEN 0 MOVE "0-Contrato"       TO GS-CLASSIFICACAO
             WHEN 1 MOVE "1-Comum   "       TO GS-CLASSIFICACAO
             WHEN 9 MOVE "9-Unificado"      TO GS-CLASSIFICACAO
           END-EVALUATE
           MOVE CLASS-CLIENTE-CH10 TO CLASSIF-CG10.
           MOVE CLIENTE-CH10       TO GS-COD-CLIENTE CODIGO-CG10.
           READ CGD010 INVALID KEY MOVE "*****" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10     TO GS-DESCR-CLIENTE.
           MOVE PORTADOR-CH10      TO GS-PORTADOR PORTADOR.
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT.
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
           EVALUATE CARTEIRA-CH10
             WHEN 1 MOVE "1-Simples " TO GS-CARTEIRA
             WHEN 2 MOVE "2-Caução  " TO GS-CARTEIRA
             WHEN 3 MOVE "3-Desconto" TO GS-CARTEIRA
           END-EVALUATE
           MOVE NR-CHEQUE-CH10        TO GS-NR-CHEQUE.
           MOVE OUTRO-DOCTO-CH10      TO GS-OUTRO-DOCTO.
           MOVE NR-NOTA-FISCAL-CH10   TO GS-NR-NOTA-FISCAL.
           MOVE DATA-NTA-FISCAL-CH10  TO GS-DATA-NTA-FISCAL.
           MOVE DATA-VENCTO-CH10      TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV              TO GS-DATA-VENCTO.
           MOVE NOME-CH10             TO GS-NOME
           MOVE CIDADE-CH10           TO GS-CIDADE
           MOVE CPF-CH10              TO GS-CPF
           MOVE LOTE-CH10             TO GS-LOTE
           MOVE BANCO-CH10            TO GS-BANCO
           MOVE AGENCIA-CH10          TO GS-AGENCIA
           MOVE DV-AGENCIA-CH10       TO GS-DV-AGENCIA
           MOVE DIGITADOR-CH10        TO GS-DIGITADOR.
           MOVE SITUACAO-TIT-CH10  TO GS-SITUACAO-TIT CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "********"  TO
                   SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01      TO GS-DESCR-SITUACAO-TIT.
           MOVE CODREDUZ-APUR-CH10 TO GS-COD-APURACAO
                                      CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20     TO GS-DESCR-APURACAO.
           MOVE VALOR-CH10     TO GS-VALOR-TOTAL.
           MOVE VENDEDOR-CH10      TO GS-VENDEDOR CODIGO-CG01
           READ CGD001 INVALID KEY MOVE "*****" TO NOME-CG01
           END-READ
           MOVE NOME-CG01          TO GS-DESCR-VENDEDOR
           MOVE SITUACAO-CH10      TO GS-SITUACAO.
      *    MOVE DATA-BAIXA-CH-DEVOLV-CH10 TO GS-DATA-BAIXA.
           MOVE SITUACAO-TIT-CH10   TO GS-SITUACAO-TIT CODIGO-CR01.
           READ CRD001 INVALID KEY MOVE "********"  TO
                  SITUACAO-TIT-CR01.
           MOVE SITUACAO-TIT-CR01   TO GS-DESCR-SITUACAO-TIT.
           MOVE ORIGEM-CH10         TO GS-ORIGEM CODIGO-CH12.
           READ CHD012 INVALID KEY MOVE SPACES TO DESCR-ORIGEM-CH12.
           MOVE DESCR-ORIGEM-CH12   TO GS-DESCR-ORIGEM.


           INITIALIZE GS-ALINEA-D
                      GS-DATA-COMPRA-D
                      GS-DATA-ENTRADA-D
                      GS-DATA-APRES-D
                      GS-DATA-REPRES-D
                      GS-DATA-RECTO-D
                      GS-VLR-JUROS-D
                      GS-VLR-MULTA-D
                      GS-VLR-DESCONTO-D
                      GS-FORMA-PAGTO-D
                      GS-DCR-MEM-D
                      GS-DCR-MEM-R.

           if valor-saldo-ch10 = 0
              move "Cheque já recebido impossibilitado a manutenção" to
                          mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
              disable-object principal
              enable-object pb11.
      *       move 1 to gs-exit-flg.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CHD010
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           IF GS-CLASSIFICACAO = SPACES
              MOVE "0" TO CLASS-CLIENTE-CH10
           ELSE
              MOVE GS-CLASSIFICACAO(1: 1)  TO CLASS-CLIENTE-CH10.

           MOVE GS-COD-CLIENTE    TO CLIENTE-CH10
           MOVE GS-PORTADOR       TO PORTADOR-CH10.
           IF GS-CARTEIRA = SPACES
              MOVE "0" TO CARTEIRA-CH10
           ELSE
              MOVE GS-CARTEIRA(1: 1) TO CARTEIRA-CH10.

           IF GS-SITUACAO-TIT = SPACES
              MOVE "00" TO SITUACAO-TIT-CH10
           ELSE
              MOVE GS-SITUACAO-TIT(1: 2) TO SITUACAO-TIT-CH10.

           MOVE GS-NR-CHEQUE       TO NR-CHEQUE-CH10.
           MOVE GS-OUTRO-DOCTO     TO OUTRO-DOCTO-CH10.
           MOVE GS-VENCTO-INV      TO DATA-VENCTO-CH10
           MOVE GS-NOME            TO NOME-CH10
           MOVE GS-BANCO           TO BANCO-CH10
           MOVE GS-AGENCIA         TO AGENCIA-CH10
           MOVE GS-DV-AGENCIA      TO DV-AGENCIA-CH10
           MOVE GS-ORIGEM          TO ORIGEM-CH10
           MOVE GS-COD-APURACAO    TO CODREDUZ-APUR-CH10
           MOVE GS-CIDADE          TO CIDADE-CH10
           MOVE USUARIO-W          TO DIGITADOR-CH10.
           MOVE GS-VALOR-TOTAL     TO VALOR-CH10
           MOVE GS-VENDEDOR        TO VENDEDOR-CH10
           MOVE GS-LOTE            TO LOTE-CH10
           MOVE GS-NR-NOTA-FISCAL  TO NR-NOTA-FISCAL-CH10.
           MOVE GS-DATA-NTA-FISCAL TO DATA-NTA-FISCAL-CH10.

           IF GS-VALOR-TOTAL <> VALOR-SALDO-CH10
              MOVE "Valor Informado Diferente do Valor do Saldo do Chequ
      -            "e Deseja Atualiza-lo ?" TO MENSAGEM
              MOVE "Q" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              IF RESP-MSG = "S"
                 MOVE GS-VALOR-TOTAL TO VALOR-SALDO-CH10.

       REGRAVA-DADOS SECTION.
           CLOSE CHD010
           OPEN I-O CHD010
           REWRITE REG-CHD010 INVALID KEY
                 MOVE "Erro Regravacao CHD010" TO GS-MENSAGEM-ERRO
                 MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE "00" TO ST-CHD010
           END-REWRITE
           IF ST-CHD010 <> "00"
              MOVE "Erro Regravacao CHD010" TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(24: 5)
              MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.
           CLOSE CHD010
           OPEN INPUT CHD010.
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
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-CH10
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE ZEROS           TO SUBSEQ-CR201.
           MOVE "ALTERACAO EFETUADA NO CHEQUE            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 80)
           MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(30: 10).
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
           MOVE "CHP010A" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.


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
           move "CHP010A"           to logacess-programa
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

           CLOSE CAD018 CHD010 CHD011 CHD099 CGD001 CGD010 CRD001
                 CXD020 CAD002 CHD013 CAD004 RCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
