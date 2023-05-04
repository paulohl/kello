       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP029.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 11-08-2005.
      *FUNÇÃO: NOVA FICHA DE MOVIMENTO DE FICHA DE IDENTIFICAÇÃO

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
           COPY COPX040.
           COPY IEPX010.
           COPY IEPX011.
           COPY PARX001.
           COPY MTPX019.
           COPY MTPX019F.
           COPY MTPX020.
           COPY COPX008.
           COPY MTPX025.
           COPY CGPX010.
           COPY CGPX011.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CAPW010.
       COPY COPW040.
       COPY IEPW010.
       COPY IEPW011.
       COPY PARW001.
       COPY MTPW019.
       COPY MTPW019F.
       COPY MTPW020.
       COPY COPW008.
       COPY MTPW025.
       COPY CGPW010.
       COPY CGPW011.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGACESS.FD.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP029a.CPB".
           COPY "MTP029a.CPY".
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
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD019F            PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD025             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-COD008             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  CIDADE-W              PIC 9(4)     VALUE ZEROS.
           05  NOME-CIDADE-W         PIC X(20)    VALUE SPACES.
           05  UF-W                  PIC XX       VALUE SPACES.
           05  DDD-W                 PIC XX       VALUE SPACES.
           05  CURSO-W               PIC 9(3)     VALUE ZEROS.
           05  NOME-CURSO-W          PIC X(20)    VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-MOTIVO            PIC 9(03).
           05  ACHEI                 PIC X(01).
           05  AUX-ORDEM             PIC 9(01).
           05  AUX-CONT              PIC 9(04).
           05  AUX-INCLUSAO          PIC 9(01).
           05  AUX-MES-ANO           PIC 9(06)    VALUE ZEROS.
           05  AUX-COD-ENTIDADE      PIC 9(05)    VALUE ZEROS.
           05  AUX-ENTIDADE          PIC X(40)    VALUE SPACES.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)     VALUE ZEROS.
           05  SENHA-COMP            PIC 9(4)     COMP-3.
           05  CAMINHO-IMAGEM        PIC X(500)   VALUE SPACES.
           05  EXTENSAO-ARQ          PIC X(05)    VALUE SPACES.
           05  WSINDICE              PIC 9(03)    VALUE ZEROS.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 file-details.
          05 file-size              pic x(8) comp-x.
          05 file-date.
             10 dia                 pic x comp-x.
             10 month               pic x comp-x.
             10 year                pic x(2) comp-x.
          05 file-time.
             10 hours               pic x comp-x.
             10 minutes             pic x comp-x.
             10 seconds             pic x comp-x.
             10 hundredths          pic x comp-x.

       01 status-code               pic x(2) comp-5.

       01  CAB01.
           05  EMPRESA-REL         PIC X(71)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(59)   VALUE
           "CONFERENCIA DA FICHA DE IDENTIFICACAO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(91)  VALUE
           "CONT-ALB. CURSO      CIDADE     NOME-FORMANDO
      -    "      FONE IDENT TUR TURNO".

       01  LINDET.
           05  LINDET-REL          PIC X(91)  VALUE SPACES.

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

       01  linka-cpf.
           05  link-cpf             pic 9(11).
           05  link-cpf-r redefines link-cpf.
               10  link-cpf-num     pic 9(09).
               10  link-cpf-dig1    pic 9(01).
               10  link-cpf-dig2    pic 9(01).
           05  link-cpf-conf        pic 9(01).

       LINKAGE SECTION.
           COPY "PARAMETR".

       01 LNK-CONTRATO              PIC 9(04).
       01 LNK-ALBUM                 PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-W LNK-CONTRATO LNK-ALBUM.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.

      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "PAR001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD019F" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019F.
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD025"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD025.
           MOVE "CGD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "IED010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD008"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD008.
           MOVE "LOG001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                       ARQUIVO-LOGACESS
           OPEN I-O   MTD019 MTD019F MTD025 LOG001 LOG002 CGD010 CGD011
                      MTD020
           CLOSE      MTD025 CGD010 CGD011 MTD019F MTD020
           OPEN I-O   MTD025 MTD020
           OPEN INPUT IED011 CAD010 COD040 CAD004 COD008 CGD010 CGD011
                      IED010 MTD019F PAR001 .
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD019F = "35"
              CLOSE MTD019F     OPEN OUTPUT MTD019F
              CLOSE MTD019F     OPEN I-O    MTD019F
           END-IF.
           IF ST-MTD025 = "35"
              CLOSE MTD025      OPEN OUTPUT MTD025
              CLOSE MTD025      OPEN I-O MTD025
           END-IF.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-LOG001 = "35"
              CLOSE LOG001      OPEN OUTPUT LOG001
              CLOSE LOG001      OPEN I-O LOG001
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O LOG002
           END-IF.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD025 <> "00"
              MOVE "ERRO ABERTURA MTD025: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD025 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PAR001 <> "00"
              MOVE "ERRO ABERTURA PAR001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019F <> "00"
              MOVE "ERRO ABERTURA MTD019F: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019F TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD008 <> "00"
              MOVE "ERRO ABERTURA COD008: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD008 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF COD-USUARIO-W NOT NUMERIC
      *       MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO.

           close      mtd019 mtd025 log001 log002
           open input mtd019 mtd025

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP029"            to logacess-programa
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

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM SALVAR-CGD010
                   PERFORM SALVAR-CGD011
                   MOVE GS-CONT TO AUX-CONT
                   PERFORM GRAVAR-MOTIVOS
                   MOVE AUX-CONT TO GS-CONT
                   IF GS-ACP-IMAGEM(1:10) <> SPACES
                      PERFORM COPIAR-IMAGEM
                   END-IF
                   IF GS-TIPO-GRAVACAO = 1
                      MOVE 0 TO GS-INCLUSAO
                      PERFORM REGRAVA-DADOS
                   ELSE
                      MOVE 1 TO GS-INCLUSAO
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
      *            PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
                   MOVE AUX-CONT TO GS-CONT
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM VALIDAR-MTD020
                   IF ACHEI = "N"
                      PERFORM EXCLUI
                   END-IF
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-TODOS-TRUE
                   PERFORM VERIFICA-AUTORIZACAO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO CONTRATO-MT19
                   MOVE GS-LINDET(6: 4)  TO SEQ-MT19
                   MOVE "SENHA25A" TO PROGRAMA-CA004
                   MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                   READ CAD004 INVALID KEY
                        MOVE "DESABILITA-ALTERACAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   NOT INVALID KEY
                        MOVE "HABILITA-ALTERACAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
                   MOVE "SENHA25E" TO PROGRAMA-CA004
                   MOVE COD-USUARIO-W TO COD-USUARIO-CA004
                   READ CAD004 INVALID KEY
                        MOVE "DESABILITA-EXCLUSAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   NOT INVALID KEY
                        MOVE "HABILITA-EXCLUSAO" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-LE-CIDADE-TRUE
                   PERFORM LE-CIDADE
               WHEN GS-LE-MOTIVO-TRUE
                   PERFORM LE-MOTIVO
               WHEN GS-POP-UP-CURSO-TRUE
                   PERFORM POP-UP-CURSO
               WHEN GS-POP-UP-CIDADE-TRUE
                   PERFORM POP-UP-CIDADE
               WHEN GS-POP-UP-MOTIVO-TRUE
                   PERFORM POP-UP-MOTIVO
               WHEN GS-LEITURA-TRANSF-TRUE
                    PERFORM LEITURA-TRANSFERE
               WHEN GS-POPUP-TRANSF-TRUE
                    PERFORM POPUP-TRANSFERE
               WHEN GS-TRANSFERE-ALBUM-TRUE
                    PERFORM TRANSFERE-ALBUM
               WHEN GS-VALIDA-ACESSO-TRUE
                    PERFORM VALIDA-ACESSO
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR-MOTIVO
               WHEN GS-CARREGAR-MOTIVOS-TRUE
                    PERFORM CARREGAR-MOTIVOS
               WHEN GS-CRITICAR-TRUE
                    PERFORM CRITICAR-REGISTRO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA57"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF40
                DISABLE-OBJECT PB16
           NOT INVALID KEY
                ENABLE-OBJECT EF40
                ENABLE-OBJECT PB16
           END-READ.

       VALIDAR-MTD020 SECTION.
           MOVE "N"            TO ACHEI
           MOVE CONTRATO-MT19  TO CONTRATO-MTG
           MOVE SEQ-MT19       TO NRALBUM-MTG
           READ MTD020 NOT INVALID KEY
                MOVE SPACES    TO MENSAGEM
                MOVE "Álbum com produtos lançados MTP020"
                  TO MENSAGEM
                MOVE "C"       TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE "S"       TO ACHEI.


       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win2 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       CRITICAR-REGISTRO SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-CPF"           PERFORM CRITICAR-CPF
               WHEN "EF-ENDERECO-RES"  PERFORM CRITICAR-ENDERECO-RES
               WHEN "EF-BAIRRO-RES"    PERFORM CRITICAR-BAIRRO-RES
               WHEN "EF-CEP-RES"       PERFORM CRITICAR-CEP-RES
               WHEN "EF-ENDERECO-COM"  PERFORM CRITICAR-ENDERECO-COM
               WHEN "EF-BAIRRO-COM"    PERFORM CRITICAR-BAIRRO-COM
               WHEN "EF-ENDERECO-PAI"  PERFORM CRITICAR-ENDERECO-PAI
               WHEN "EF-BAIRRO-REP"    PERFORM CRITICAR-BAIRRO-REP
               WHEN "EF-CEP-REP"       PERFORM CRITICAR-CEP-REP
               WHEN "REGISTRO"         PERFORM CRITICAR-CPF
                                          THRU CRITICAR-CEP-REP
               WHEN "IMAGEM"           PERFORM CRITICAR-IMAGEM
               WHEN "VISUALIZA"        PERFORM VISUALIZAR-IMAGEM
               WHEN "SALVAR"           PERFORM SALVAR.

       SALVAR SECTION.
           PERFORM SALVAR-DADOS
           PERFORM SALVAR-CGD010
           PERFORM SALVAR-CGD011
           MOVE GS-CONT TO AUX-CONT
           PERFORM GRAVAR-MOTIVOS
           MOVE AUX-CONT TO GS-CONT
           PERFORM COPIAR-IMAGEM
           IF GS-TIPO-GRAVACAO = 1
              MOVE 0 TO GS-INCLUSAO
              PERFORM REGRAVA-DADOS
           ELSE
              MOVE 1 TO GS-INCLUSAO
              PERFORM GRAVA-DADOS
           END-IF
           PERFORM LIMPAR-DADOS
      *    PERFORM CARREGA-ULTIMOS
           MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
           MOVE AUX-CONT TO GS-CONT.


       CRITICAR-CPF SECTION.
           IF GS-CPF > ZEROS
              move  gs-cpf       to link-cpf
              call     "digcpf"      using   linka-cpf
              cancel   "digcpf"
              if link-cpf-conf       =       1
                 move "C.P.F. Inválido!" to mensagem
                 move "C"                to tipo-msg
                 perform exibir-mensagem.

       CRITICAR-ENDERECO-RES SECTION.
      *    IF GS-ENDERECO-RES = SPACES
      *       MOVE "Endereço Residencial Não Informado" TO MENSAGEM
      *       MOVE "C" TO TIPO-MSG
      *       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BAIRRO-RES SECTION.
      *    IF GS-BAIRRO-RES = SPACES
      *       MOVE "Bairro Residencial Não Informado" TO MENSAGEM
      *       MOVE "C" TO TIPO-MSG
      *       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CEP-RES SECTION.
      *    IF GS-CEP-RES = SPACES
      *       MOVE "CEP Residencial Não Informado" TO MENSAGEM
      *       MOVE "C" TO TIPO-MSG
      *       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-ENDERECO-COM SECTION.
      *    IF GS-LOCAL-TRABALHO <> SPACES
      *        IF GS-ENDERECO-COM = SPACES
      *            MOVE "Bairro Comercial Não Informado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BAIRRO-COM SECTION.
      *    IF GS-LOCAL-TRABALHO <> SPACES
      *        IF GS-BAIRRO-COM = SPACES
      *            MOVE "Bairro Comercial Não Informado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM.

       CRITICAR-ENDERECO-PAI SECTION.
      *    IF GS-PAI <> SPACES
      *        IF GS-ENDERECO-PAI = SPACES
      *            MOVE "Endereço dos Pais Não Informado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BAIRRO-REP SECTION.
      *    IF GS-ENDERECO-REPUBLICA <> SPACES
      *        IF GS-BAIRRO-REPUBLICA = SPACES
      *            MOVE "Bairro da República Não Informado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CEP-REP SECTION.
      *    IF GS-ENDERECO-REPUBLICA <> SPACES
      *        IF GS-CEP-REPUBLICA = SPACES
      *            MOVE "Cep da República Não Informado" TO MENSAGEM
      *            MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM.

       CRITICAR-IMAGEM SECTION.
              IF GS-ACP-IMAGEM EQUAL SPACES
                 move "CHAMAR-OPENDIALOG" to ds-procedure
                 perform call-dialog-system
              END-IF
              IF GS-ACP-IMAGEM EQUAL SPACES
                 MOVE "Imagem do Formando Não Informada" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
                 CALL "CBL_CHECK_FILE_EXIST"     USING GS-ACP-IMAGEM
                                                        FILE-DETAILS
                                               RETURNING STATUS-CODE
                 IF STATUS-CODE <> ZEROS
                    MOVE "Imagem Não Encontrada" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM.

       VISUALIZAR-IMAGEM SECTION.
           CALL "CBL_CHECK_FILE_EXIST"     USING GS-ACP-IMAGEM
                                                  FILE-DETAILS
                                         RETURNING STATUS-CODE
           IF STATUS-CODE <> 0
              MOVE X"00" TO GS-ACP-IMAGEM.

           STRING GS-ACP-IMAGEM X"00" DELIMITED BY "  " INTO
                  GS-ACP-IMAGEM

           INVOKE JANELAPRINCIPAL "SetarImagemDeFundo" using
                  GS-ACP-IMAGEM

           SHOW-WINDOW WIN-FOTO.

       GRAVAR-MOTIVOS SECTION.
           CLOSE    MTD025
           OPEN I-O LOG001 MTD025

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE SPACES TO GS-MOTIVOS
           MOVE 1      TO GS-CONT
           MOVE "N"    TO ACHEI
           MOVE "LER-MOTIVO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-MOTIVOS = SPACES OR ACHEI = "S"
                INITIALIZE REG-MTD025
                MOVE GS-MOTIVOS(1:3) TO AUX-MOTIVO
                MOVE GS-NR-CONTRATO  TO CONTRATO-MT25
                MOVE GS-NR-ALBUM     TO ALBUM-MT25
                MOVE AUX-MOTIVO      TO MOTIVO-MT25
                MOVE WS-DATA-CPU     TO DATA-MT25
                WRITE REG-MTD025 NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG1-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG1-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG1-HORAS
                      MOVE "I"         TO LOG1-OPERACAO
                      MOVE "MTD025"    TO LOG1-ARQUIVO
                      MOVE "MTP029"    TO LOG1-PROGRAMA
                      MOVE REG-MTD025  TO LOG1-REGISTRO
                      WRITE REG-LOG001
                      END-WRITE
                END-WRITE
                ADD 1 TO GS-CONT
                MOVE SPACES TO GS-MOTIVOS
                MOVE "LER-MOTIVO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE MTD025 LOG001
           OPEN INPUT MTD025.

       INCLUIR-MOTIVO SECTION.
           IF GS-DESC-MOTIVO = "********" or spaces
              MOVE "Motivo Informado Inválido" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE SPACES TO GS-MOTIVOS
              MOVE 1      TO GS-CONT
              MOVE "N"    TO ACHEI
              MOVE "LER-MOTIVO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM UNTIL GS-MOTIVOS = SPACES OR ACHEI = "S"
                   MOVE GS-MOTIVOS(1:3) TO AUX-MOTIVO
                   IF AUX-MOTIVO = GS-MOTIVO
                      MOVE "S" TO ACHEI
                   END-IF
                   ADD 1 TO GS-CONT
                   MOVE SPACES TO GS-MOTIVOS
                   MOVE "LER-MOTIVO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM
              IF ACHEI = "S"
                 MOVE "Motivo já Informado !!!" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              ELSE
                 MOVE ZEROS            TO GS-CONT
                 MOVE SPACES           TO GS-MOTIVOS
                 STRING GS-MOTIVO      INTO GS-MOTIVOS(1:3)
                 STRING GS-DESC-MOTIVO INTO GS-MOTIVOS(5:30).

       VALIDA-ACESSO SECTION.
           MOVE "SENHA15" TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
              MOVE "DESABILITA-BOTAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

           MOVE LNK-CONTRATO          TO CONTRATO-MT19  GS-NR-CONTRATO
           MOVE LNK-ALBUM             TO SEQ-MT19       GS-NR-ALBUM
           READ MTD019 INVALID KEY
                MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
                                                           GS-DATA-MOVTO

                MOVE GS-NR-CONTRATO        TO NR-CONTRATO-CO40
                READ COD040 INVALID KEY
                     MOVE SPACES           TO IDENTIFICACAO-CO40
                     MOVE ZEROS            TO INSTITUICAO-CO40
                     MOVE ZEROS            TO MESANO-PREV-CO40
                END-READ

                STRING MESANO-PREV-CO40(5:2) MESANO-PREV-CO40(1:4)
                  INTO GS-MES-ANO

                MOVE IDENTIFICACAO-CO40    TO GS-DESC-CONTRATO

                MOVE INSTITUICAO-CO40      TO GS-COD-ENTIDADE
                                              CODIGO-IE10
                READ IED010 INVALID KEY
                     MOVE SPACES           TO NOME-IE10
                END-READ
                MOVE NOME-IE10             TO GS-ENTIDADE

                MOVE 0                     TO GS-TIPO-GRAVACAO
           NOT INVALID KEY
                MOVE 1                     TO GS-TIPO-GRAVACAO
                PERFORM CARREGAR-DADOS.

           REFRESH-OBJECT TABP1
           REFRESH-OBJECT TABP2
           REFRESH-OBJECT TABP3.

       POP-UP-CURSO SECTION.
           CALL   "IEP011T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "IEP011T"
           MOVE PASSAR-STRING-1(43: 3) TO GS-CURSO
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CURSO.

       POP-UP-CIDADE SECTION.
           EVALUATE GS-OPCAO-CIDADE
               WHEN 1 CALL   "CAP010T" USING PARAMETROS-W
                                             PASSAR-STRING-1
                      CANCEL "CAP010T"
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-RES
                      PERFORM LE-CIDADE
               WHEN 2 CALL   "CAP010T" USING PARAMETROS-W
                                             PASSAR-STRING-1
                      CANCEL "CAP010T"
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-COM
                      PERFORM LE-CIDADE
               WHEN 3 CALL   "CAP010T" USING PARAMETROS-W
                                             PASSAR-STRING-1
                      CANCEL "CAP010T"
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-PAI
                      PERFORM LE-CIDADE
               WHEN 4 CALL   "CAP010T" USING PARAMETROS-W
                                             PASSAR-STRING-1
                      CANCEL "CAP010T"
                      MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE-REPUBLICA
                      PERFORM LE-CIDADE.

       POP-UP-MOTIVO SECTION.
           CALL   "COP008T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP008T"
           MOVE PASSAR-STRING-1(32: 3) TO GS-MOTIVO
           MOVE PASSAR-STRING-1( 1:30) TO GS-DESC-MOTIVO
           PERFORM LE-MOTIVO.

       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "********" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-DESC-CURSO.
       LE-CIDADE SECTION.
           EVALUATE GS-OPCAO-CIDADE
               WHEN 1 MOVE GS-CIDADE-RES       TO CIDADE
               WHEN 2 MOVE GS-CIDADE-COM       TO CIDADE
               WHEN 3 MOVE GS-CIDADE-PAI       TO CIDADE
               WHEN 4 MOVE GS-CIDADE-REPUBLICA TO CIDADE
           END-EVALUATE
           READ CAD010 INVALID KEY MOVE "********" TO NOME-COMPL-CID.
           EVALUATE GS-OPCAO-CIDADE
               WHEN 1 MOVE NOME-COMPL-CID      TO GS-DESC-CIDADE-RES
                      MOVE UF-CID              TO GS-UF-RES
                      MOVE DDD-CID             TO GS-DDD-RES
                      MOVE DDD-CID             TO GS-DDD-CELULAR-RES
                      IF GS-CEP-RES = 0
                         MOVE CEP-CID          TO GS-CEP-RES
                      END-IF
               WHEN 2 MOVE NOME-COMPL-CID      TO GS-DESC-CIDADE-COM
                      MOVE UF-CID              TO GS-UF-COM
                      MOVE DDD-CID             TO GS-DDD-COM
               WHEN 3 MOVE NOME-COMPL-CID      TO GS-DESC-CIDADE-PAI
                      MOVE UF-CID              TO GS-UF-PAI
                      MOVE DDD-CID             TO GS-DDD-PAI
                      MOVE DDD-CID             TO GS-DDD-CELULAR-PAI
                      IF GS-CEP-PAI = 0
                         MOVE CEP-CID          TO GS-CEP-PAI
                      END-IF
               WHEN 4 MOVE NOME-COMPL-CID      TO
                                               GS-DESC-CIDADE-REPUBLICA
                      MOVE UF-CID              TO GS-UF-REPUBLICA
                      IF GS-CEP-REPUBLICA = 0
                         MOVE CEP-CID          TO GS-CEP-REPUBLICA
                      END-IF
           END-EVALUATE.
       LE-MOTIVO SECTION.
           MOVE GS-MOTIVO          TO CODIGO-CO08.
           READ COD008 INVALID KEY
                MOVE "********" TO DESCRICAO-CO08.

           MOVE DESCRICAO-CO08     TO GS-DESC-MOTIVO.

       LEITURA-TRANSFERE SECTION.
           MOVE GS-CONTRATO-TRANS TO CONTRATO-MT19
           EVALUATE GS-OPCAO-TRANSF
             WHEN 1 MOVE GS-DO-ALBUM   TO SEQ-MT19
             WHEN 2 MOVE GS-PARA-ALBUM TO SEQ-MT19
           END-EVALUATE
           READ MTD019 INVALID KEY
             EVALUATE GS-OPCAO-TRANSF
               WHEN 1 MOVE "Não cadastrado"  TO GS-DO-ALBUM-DESC
               WHEN 2 MOVE "Não cadastrado"  TO GS-PARA-ALBUM-DESC
             END-EVALUATE
            NOT INVALID KEY
             EVALUATE GS-OPCAO-TRANSF
               WHEN 1 MOVE NOME-FORM-MT19  TO GS-DO-ALBUM-DESC
               WHEN 2 MOVE NOME-FORM-MT19  TO GS-PARA-ALBUM-DESC
             END-EVALUATE
           END-READ.
       POPUP-TRANSFERE SECTION.
           MOVE GS-CONTRATO-TRANS TO PASSAR-PARAMETROS(40: 4).
           CALL   "MTP019T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "MTP019T".
           EVALUATE GS-OPCAO-TRANSF
             WHEN 1 MOVE PASSAR-PARAMETROS(1: 30) TO GS-DO-ALBUM-DESC
                    MOVE PASSAR-PARAMETROS(45: 4) TO GS-DO-ALBUM
             WHEN 2 MOVE PASSAR-PARAMETROS(1: 30) TO GS-PARA-ALBUM-DESC
                    MOVE PASSAR-PARAMETROS(45: 4) TO GS-PARA-ALBUM
           END-EVALUATE.
       TRANSFERE-ALBUM SECTION.
      *    OS ALBUNS QUE ESTIVEREM COM A SEQUENCIA PULADA, DEVERÁ PEGAR
      *    OS ÚLTIMOS ALBUNS P/ COLOCAR NESTAS SEQUENCIAS, PORQUE
      *    NÃO PODE HAVER BURACOS NA SEQUENCIA

           CLOSE    MTD019
           OPEN I-O MTD019 LOG002

           MOVE GS-CONTRATO-TRANS       TO CONTRATO-MT19
           MOVE GS-PARA-ALBUM            TO SEQ-MT19
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP029"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE
                END-DELETE
           END-READ

           MOVE GS-CONTRATO-TRANS       TO CONTRATO-MT19
           MOVE GS-DO-ALBUM             TO SEQ-MT19
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
               DELETE MTD019 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "E"         TO LOG2-OPERACAO
                   MOVE "MTD019"    TO LOG2-ARQUIVO
                   MOVE "MTP029"    TO LOG2-PROGRAMA
                   MOVE REG-MTD019  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
               END-DELETE
               MOVE GS-CONTRATO-TRANS   TO CONTRATO-MT19
               MOVE GS-PARA-ALBUM       TO SEQ-MT19
               WRITE REG-MTD019 NOT INVALID KEY
                   MOVE USUARIO-W   TO LOG2-USUARIO
                   MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                   MOVE WS-DATA-CPU TO LOG2-DATA
                   ACCEPT WS-HORA-SYS FROM TIME
                   MOVE WS-HORA-SYS TO LOG2-HORAS
                   MOVE "I"         TO LOG2-OPERACAO
                   MOVE "MTD019"    TO LOG2-ARQUIVO
                   MOVE "MTP029"    TO LOG2-PROGRAMA
                   MOVE REG-MTD019  TO LOG2-REGISTRO
                   WRITE REG-LOG002
                   END-WRITE
               END-WRITE
           END-READ.

           CLOSE MTD019 LOG002
           OPEN INPUT MTD019

           PERFORM CARREGA-ULTIMOS.
      *----------------------------------------------------------------
      *
       VERIFICA-AUTORIZACAO SECTION.
           MOVE "SIM"                     TO GS-PERMISSAO.
           MOVE GS-SENHA                  TO SENHA-COMP
           IF GS-ALBUM1 > GS-ALBUM2
              MOVE "Intervalo Incorreto"  TO GS-AUTORIZACAO
              MOVE "NAO"                  TO GS-PERMISSAO
           ELSE
              IF SENHA-COMP NOT = SENHA-W
                 MOVE "Senha inválida"    TO GS-AUTORIZACAO

                 MOVE "NAO"               TO GS-PERMISSAO
              ELSE
                 PERFORM EXCLUI-TODOS-ALBUNS
              END-IF
           END-IF.
      *----------------------------------------------------------------
       EXCLUI-TODOS-ALBUNS SECTION.
           CLOSE    MTD019 MTD019F
           OPEN I-O MTD019 LOG002 MTD019F
           MOVE GS-NR-CONTRATO    TO CONTRATO-MT19
           MOVE GS-ALBUM1      TO SEQ-MT19
           START MTD019 KEY IS NOT < ALBUM-MT19
                    INVALID KEY MOVE "10" TO ST-MTD019
           END-START

           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
                IF CONTRATO-MT19 <> GS-NR-CONTRATO OR
                   SEQ-MT19 > GS-ALBUM2
                       MOVE "10" TO ST-MTD019
                ELSE
                   DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP029"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE

                       MOVE ALBUMMT19   TO ALBUMMT19F
                       READ MTD019F NOT INVALID KEY
                            DELETE MTD019F
                            END-DELETE
                       END-READ
                   END-DELETE
                END-IF
              END-READ
           END-PERFORM

           CLOSE      MTD019 LOG002  MTD019F
           OPEN INPUT MTD019 MTD019F.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD019 MTD019F
           OPEN I-O MTD019 LOG002 MTD019F

           MOVE GS-NR-CONTRATO TO ALBUM-MT19(1: 4)
           MOVE GS-NR-ALBUM    TO ALBUM-MT19(5: 4).
           READ MTD019 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD019 NOT INVALID KEY
                       MOVE USUARIO-W   TO LOG2-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU TO LOG2-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS TO LOG2-HORAS
                       MOVE "E"         TO LOG2-OPERACAO
                       MOVE "MTD019"    TO LOG2-ARQUIVO
                       MOVE "MTP029"    TO LOG2-PROGRAMA
                       MOVE REG-MTD019  TO LOG2-REGISTRO
                       WRITE REG-LOG002
                       END-WRITE

                       MOVE ALBUMMT19   TO ALBUMMT19F
                       READ MTD019F NOT INVALID KEY
                            DELETE MTD019F
                            END-DELETE
                       END-READ.

           CLOSE      MTD019 LOG002 MTD019F
           OPEN INPUT MTD019 MTD019F

           PERFORM EXCLUIR-MOTIVOS
           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.

       EXCLUIR-MOTIVOS SECTION.
           CLOSE    MTD025
           OPEN I-O MTD025 LOG001

           INITIALIZE REG-MTD025
           MOVE GS-NR-CONTRATO TO CONTRATO-MT25
           MOVE GS-NR-ALBUM    TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 <> "10"
               READ MTD025 NEXT AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
                   IF GS-NR-CONTRATO <> CONTRATO-MT25 OR
                      GS-NR-ALBUM    <> ALBUM-MT25
                      MOVE "10" TO ST-MTD025
                   ELSE
                      DELETE MTD025 NOT INVALID KEY
                             MOVE USUARIO-W   TO LOG1-USUARIO
                             MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                             MOVE WS-DATA-CPU TO LOG1-DATA
                             ACCEPT WS-HORA-SYS FROM TIME
                             MOVE WS-HORA-SYS TO LOG1-HORAS
                             MOVE "E"         TO LOG1-OPERACAO
                             MOVE "MTD025"    TO LOG1-ARQUIVO
                             MOVE "MTP029"    TO LOG1-PROGRAMA
                             MOVE REG-MTD025  TO LOG1-REGISTRO
                             WRITE REG-LOG001
                             END-WRITE
                      END-DELETE
                   END-IF
               END-READ
           END-PERFORM

           CLOSE      MTD025 LOG001
           OPEN INPUT MTD025.

       CARREGAR-DADOS SECTION.
           START MTD019 KEY IS = ALBUM-MT19 INVALID KEY
                 CONTINUE.
           READ MTD019 INVALID KEY
                 INITIALIZE REG-MTD019.

           MOVE CONTRATO-MT19        TO  GS-NR-CONTRATO
                                         NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO  GS-DESC-CONTRATO.
           MOVE SEQ-MT19             TO  GS-NR-ALBUM
           MOVE TURMA-MT19           TO  GS-TURMA
           MOVE TURNO-MT19           TO  GS-TURNO
           MOVE CURSO-MT19           TO  GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE "*****"         TO  NOME-IE11.
           MOVE NOME-IE11            TO  GS-DESC-CURSO.
           MOVE UF-MT19              TO  GS-UF-RES.
           MOVE CIDADE-MT19          TO  GS-CIDADE-RES CIDADE.
           READ CAD010 INVALID KEY
                MOVE SPACES          TO  NOME-CID.
           MOVE NOME-CID             TO  GS-DESC-CIDADE-RES.
           MOVE UF-CID               TO  GS-UF-RES
           MOVE DDD-CID              TO  GS-DDD-RES
           MOVE DDD-CID              TO  GS-DDD-CELULAR-RES
           MOVE NOME-FORM-MT19       TO  GS-NOME-FORMANDO.
           MOVE FONE-MT19            TO  GS-TELEFONE-RES
           EVALUATE IDENTIFICADO-MT19
             WHEN 0 MOVE "0-Não"     TO  GS-IDENTIFICADO
             WHEN 1 MOVE "1-Sim"     TO  GS-IDENTIFICADO
           END-EVALUATE
           MOVE DATAMOV-MT19         TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-MOVTO

           MOVE "0"                  TO  CLASSIF-CG11
           MOVE ALBUM-MT19           TO  CODIGO-CG11
           READ CGD011 INVALID KEY
               INITIALIZE REG-CGD011.

           MOVE ENDERECO2-CG11        TO  GS-ENDERECO-RES
           MOVE COMPLEMENTO2-CG11     TO  GS-COMPLEMENTO-RES
           MOVE PONTO-REFER2-CG11     TO  GS-REFERENCIA-RES
           MOVE BAIRRO2-CG11          TO  GS-BAIRRO-RES
           MOVE CIDADE2-CG11          TO  GS-CIDADE-RES CIDADE
           READ CAD010 INVALID KEY
               INITIALIZE REG-CAD010.
           MOVE NOME-CID              TO GS-DESC-CIDADE-RES
           MOVE UF-CID                TO GS-UF-RES
           MOVE DDD-CID               TO GS-DDD-RES
           MOVE DDD-CID               TO GS-DDD-CELULAR-RES
           MOVE CEP2-CG11             TO GS-CEP-RES
           MOVE FONE2-CG11            TO GS-TELEFONE-RES
           MOVE EMPRESA-CG11          TO GS-LOCAL-TRABALHO
           MOVE ENDERECO3-CG11        TO GS-ENDERECO-COM
           MOVE PONTO-REFER3-CG11     TO GS-REFERENCIA-COM
           MOVE BAIRRO3-CG11          TO GS-BAIRRO-COM
           MOVE CIDADE3-CG11          TO GS-CIDADE-COM CIDADE
           READ CAD010 INVALID KEY
               INITIALIZE REG-CAD010.
           MOVE NOME-CID              TO GS-DESC-CIDADE-COM
           MOVE UF-CID                TO GS-UF-COM
           MOVE DDD-CID               TO GS-DDD-COM
           MOVE FONE3-CG11            TO GS-TELEFONE-COM
           MOVE RAMAL3-CG11           TO GS-RAMAL-COM
           MOVE CELULAR2-CG11         TO GS-CELULAR-RES
           MOVE CPF2-CG11             TO GS-CPF
           MOVE RG2-CG11              TO GS-RG
           MOVE DT-EXPEDICAO2-CG11    TO GS-DATA-EXPEDICAO
           MOVE ORGAO-EXPEDICAO2-CG11 TO GS-ORGAO-EXPEDICAO
           STRING DATA-NASC2-CG11(7:2)
                  DATA-NASC2-CG11(5:2)
                  DATA-NASC2-CG11(1:4) INTO GS-DATA-NASCIMENTO
           MOVE NOME-PAI-CG11         TO GS-PAI
           MOVE NOME-MAE-CG11         TO GS-MAE
           MOVE ENDERECO-PAIS-CG11    TO GS-ENDERECO-PAI
           MOVE CIDADE-PAIS-CG11      TO GS-CIDADE-PAI CIDADE
           READ CAD010 INVALID KEY
               INITIALIZE REG-CAD010.
           MOVE NOME-CID              TO GS-DESC-CIDADE-PAI
           MOVE UF-CID                TO GS-UF-PAI
           MOVE DDD-CID               TO GS-DDD-PAI
           MOVE DDD-CID               TO GS-DDD-CELULAR-PAI
           MOVE FONE-PAIS-CG11        TO GS-TELEFONE-PAI
           MOVE CELULAR-PAIS-CG11     TO GS-CELULAR-PAI
           MOVE CEP-PAIS-CG11         TO GS-CEP-PAI
           MOVE ENDERECO-REP-CG11     TO GS-ENDERECO-REPUBLICA.
           MOVE BAIRRO-REP-CG11       TO GS-BAIRRO-REPUBLICA
           MOVE CIDADE-REP-CG11       TO GS-CIDADE-REPUBLICA CIDADE
           READ CAD010 INVALID KEY
               INITIALIZE REG-CAD010.
           MOVE NOME-CID              TO GS-DESC-CIDADE-REPUBLICA
           MOVE UF-CID                TO GS-UF-REPUBLICA
           MOVE CEP-REP-CG11          TO GS-CEP-REPUBLICA.


       CARREGAR-MOTIVOS SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "N" TO ACHEI
           INITIALIZE REG-MTD025
           MOVE GS-NR-CONTRATO TO CONTRATO-MT25
           MOVE GS-NR-ALBUM TO ALBUM-MT25
           START MTD025 KEY IS NOT LESS CHAVE1-MT25 INVALID KEY
               MOVE "10" TO ST-MTD025.
           PERFORM UNTIL ST-MTD025 = "10"
               READ MTD025 NEXT AT END
                   MOVE "10" TO ST-MTD025
               NOT AT END
                   IF GS-NR-CONTRATO <> CONTRATO-MT25 OR GS-NR-ALBUM <>
                                                      ALBUM-MT25
                      MOVE "10" TO ST-MTD025
                   ELSE
                      MOVE "S"              TO ACHEI
                      MOVE SPACES           TO GS-MOTIVOS
                      STRING MOTIVO-MT25    INTO GS-MOTIVOS(1:3)
                      MOVE MOTIVO-MT25      TO CODIGO-CO08
                      READ COD008 INVALID KEY
                           MOVE "******"    TO DESCRICAO-CO08
                      END-READ
                      STRING DESCRICAO-CO08 INTO GS-MOTIVOS(5:30)
                      STRING DATA-MT25(7:2) INTO GS-MOTIVOS(31:2)
                      STRING "/"            INTO GS-MOTIVOS(33:1)
                      STRING DATA-MT25(5:2) INTO GS-MOTIVOS(34:2)
                      STRING "/"            INTO GS-MOTIVOS(36:1)
                      STRING DATA-MT25(1:4) INTO GS-MOTIVOS(37:4)
                      MOVE "INCLUIR-MOTIVO" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              MOVE SPACES           TO   GS-MOTIVOS
              MOVE 001              TO   CODIGO-CO08
              READ COD008 INVALID KEY
                   MOVE "********"  TO   DESCRICAO-CO08
              END-READ
              MOVE 001              TO   GS-MOTIVOS(1:3)
              STRING DESCRICAO-CO08 INTO GS-MOTIVOS(5:30)
              MOVE "INCLUIR-MOTIVO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO        TO DATAMOV-W
           MOVE GS-NR-CONTRATO       TO CONTRATO-W
           MOVE GS-DESC-CONTRATO     TO IDENTIFICACAO-W
           MOVE GS-CIDADE-RES        TO CIDADE-W
           MOVE GS-DESC-CIDADE-RES   TO NOME-CIDADE-W
           MOVE GS-UF-RES            TO UF-W
           MOVE GS-DDD-RES           TO DDD-W
           MOVE GS-CURSO             TO CURSO-W
           MOVE GS-DESC-CURSO        TO NOME-CURSO-W
           MOVE GS-ORDEM             TO AUX-ORDEM
           MOVE GS-CONT              TO AUX-CONT
           MOVE GS-INCLUSAO          TO AUX-INCLUSAO
           MOVE GS-MES-ANO           TO AUX-MES-ANO
           MOVE GS-COD-ENTIDADE      TO AUX-COD-ENTIDADE
           MOVE GS-ENTIDADE          TO AUX-ENTIDADE
           INITIALIZE REG-MTD019
           INITIALIZE GS-DATA-BLOCK
           MOVE AUX-CONT         TO GS-CONT
           MOVE AUX-INCLUSAO     TO GS-INCLUSAO
           MOVE AUX-MES-ANO      TO GS-MES-ANO
           MOVE AUX-COD-ENTIDADE TO GS-COD-ENTIDADE
           MOVE AUX-ENTIDADE     TO GS-ENTIDADE
           MOVE AUX-ORDEM        TO GS-ORDEM
           MOVE DATAMOV-W        TO GS-DATA-MOVTO
           MOVE CONTRATO-W       TO GS-NR-CONTRATO
           MOVE IDENTIFICACAO-W  TO GS-DESC-CONTRATO
           MOVE CIDADE-W         TO GS-CIDADE-RES
           MOVE NOME-CIDADE-W    TO GS-DESC-CIDADE-RES
           MOVE UF-W             TO GS-UF-RES
           MOVE DDD-W            TO GS-DDD-RES
           MOVE CURSO-W          TO GS-CURSO
           MOVE NOME-CURSO-W     TO GS-DESC-CURSO
           MOVE ULT-SEQ          TO GS-NR-ALBUM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-NR-CONTRATO        TO CONTRATO-MT19
           MOVE GS-NR-ALBUM           TO SEQ-MT19
           MOVE GS-CURSO              TO CURSO-MT19
           MOVE GS-TURMA              TO TURMA-MT19
           MOVE GS-TURNO              TO TURNO-MT19
           MOVE GS-NOME-FORMANDO      TO NOME-FORM-MT19
           MOVE GS-CIDADE-RES         TO CIDADE-MT19
           MOVE GS-UF-RES             TO UF-MT19
           MOVE GS-TELEFONE-RES       TO FONE-MT19
           MOVE GS-IDENTIFICADO(1: 1) TO IDENTIFICADO-MT19
           MOVE GS-DATA-MOVTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATAMOV-MT19.

       SALVAR-CGD010 SECTION.
           CLOSE CGD010
           OPEN I-O CGD010
           INITIALIZE REG-CGD010
           MOVE "0"                   TO CLASSIF-CG10
           STRING GS-NR-CONTRATO GS-NR-ALBUM INTO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                TO COMPRADOR-CG10
                WRITE REG-CGD010 INVALID KEY
                    MOVE "Erro de Gravação...CGD010" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
                REWRITE REG-CGD010 INVALID KEY
                    MOVE "Erro de Regravação...CGD010" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ
           CLOSE CGD010
           OPEN INPUT CGD010.

       SALVAR-CGD011 SECTION.
           CLOSE CGD011
           OPEN I-O CGD011
           INITIALIZE REG-CGD011
           MOVE "0"                   TO CLASSIF-CG11
           STRING GS-NR-CONTRATO GS-NR-ALBUM INTO CODIGO-CG11
           READ CGD011 INVALID KEY

      *         MOVE GS-ENDERECO-RES      TO ENDERECO1-CG11
      *         MOVE GS-COMPLEMENTO-RES   TO COMPLEMENTO1-CG11
      *         MOVE GS-REFERENCIA-RES    TO PONTO-REFER1-CG11
      *         MOVE GS-BAIRRO-RES        TO BAIRRO1-CG11
      *         MOVE GS-CIDADE-RES        TO CIDADE1-CG11
      *         MOVE GS-DDD-RES           TO DDDD-CELULAR-CG11
      *         MOVE GS-CEP-RES           TO CEP1-CG11
      *         MOVE GS-TELEFONE-RES      TO FONE1-CG11

                MOVE GS-ENDERECO-RES      TO ENDERECO2-CG11
                MOVE GS-COMPLEMENTO-RES   TO COMPLEMENTO2-CG11
                MOVE GS-REFERENCIA-RES    TO PONTO-REFER2-CG11
                MOVE GS-BAIRRO-RES        TO BAIRRO2-CG11
                MOVE GS-CIDADE-RES        TO CIDADE2-CG11
                MOVE GS-DDD-RES           TO DDD-CELULAR2-CG11
                MOVE GS-CEP-RES           TO CEP2-CG11
                MOVE GS-TELEFONE-RES      TO FONE2-CG11

                MOVE GS-LOCAL-TRABALHO    TO EMPRESA-CG11

      *         MOVE GS-ENDERECO-COM      TO ENDERECO2-CG11
      *         MOVE GS-REFERENCIA-COM    TO PONTO-REFER2-CG11
      *         MOVE GS-BAIRRO-COM        TO BAIRRO2-CG11
      *         MOVE GS-CIDADE-COM        TO CIDADE2-CG11
      *         MOVE GS-TELEFONE-COM      TO FONE2-CG11
      *         MOVE GS-RAMAL-COM         TO RAMAL2-CG11

                MOVE GS-ENDERECO-COM      TO ENDERECO3-CG11
                MOVE GS-REFERENCIA-COM    TO PONTO-REFER3-CG11
                MOVE GS-BAIRRO-COM        TO BAIRRO3-CG11
                MOVE GS-CIDADE-COM        TO CIDADE3-CG11
                MOVE GS-TELEFONE-COM      TO FONE3-CG11
                MOVE GS-RAMAL-COM         TO RAMAL3-CG11

                MOVE GS-CELULAR-RES       TO CELULAR2-CG11
                MOVE GS-CPF               TO CPF2-CG11
                MOVE GS-RG                TO RG2-CG11
                MOVE GS-DATA-EXPEDICAO    TO DT-EXPEDICAO2-CG11
                MOVE GS-ORGAO-EXPEDICAO   TO ORGAO-EXPEDICAO2-CG11
                STRING GS-DATA-NASCIMENTO(5:4)
                       GS-DATA-NASCIMENTO(3:2)
                       GS-DATA-NASCIMENTO(1:2)
                  INTO DATA-NASC2-CG11
                MOVE GS-PAI                     TO NOME-PAI-CG11
                MOVE GS-MAE                     TO NOME-MAE-CG11
                MOVE GS-ENDERECO-PAI            TO ENDERECO-PAIS-CG11
                MOVE GS-CIDADE-PAI              TO CIDADE-PAIS-CG11
                MOVE GS-TELEFONE-PAI            TO FONE-PAIS-CG11
                MOVE GS-CELULAR-PAI             TO CELULAR-PAIS-CG11
                MOVE GS-CEP-PAI                 TO CEP-PAIS-CG11
                MOVE GS-ENDERECO-REPUBLICA      TO ENDERECO-REP-CG11
                MOVE GS-BAIRRO-REPUBLICA        TO BAIRRO-REP-CG11
                MOVE GS-CIDADE-REPUBLICA        TO CIDADE-REP-CG11
                MOVE GS-CEP-REPUBLICA           TO CEP-REP-CG11

                WRITE REG-CGD011 INVALID KEY
                    MOVE "Erro de Gravação...CGD011" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-WRITE
           NOT INVALID KEY
      *         MOVE GS-ENDERECO-RES      TO ENDERECO1-CG11
      *         MOVE GS-COMPLEMENTO-RES   TO COMPLEMENTO1-CG11
      *         MOVE GS-REFERENCIA-RES    TO PONTO-REFER1-CG11
      *         MOVE GS-BAIRRO-RES        TO BAIRRO1-CG11
      *         MOVE GS-CIDADE-RES        TO CIDADE1-CG11
      *         MOVE GS-DDD-RES           TO DDDD-CELULAR-CG11
      *         MOVE GS-CEP-RES           TO CEP1-CG11
      *         MOVE GS-TELEFONE-RES      TO FONE1-CG11

                MOVE GS-ENDERECO-RES      TO ENDERECO2-CG11
                MOVE GS-COMPLEMENTO-RES   TO COMPLEMENTO2-CG11
                MOVE GS-REFERENCIA-RES    TO PONTO-REFER2-CG11
                MOVE GS-BAIRRO-RES        TO BAIRRO2-CG11
                MOVE GS-CIDADE-RES        TO CIDADE2-CG11
                MOVE GS-DDD-RES           TO DDD-CELULAR2-CG11
                MOVE GS-CEP-RES           TO CEP2-CG11
                MOVE GS-TELEFONE-RES      TO FONE2-CG11

                MOVE GS-LOCAL-TRABALHO    TO EMPRESA-CG11

      *         MOVE GS-ENDERECO-COM      TO ENDERECO2-CG11
      *         MOVE GS-REFERENCIA-COM    TO PONTO-REFER2-CG11
      *         MOVE GS-BAIRRO-COM        TO BAIRRO2-CG11
      *         MOVE GS-CIDADE-COM        TO CIDADE2-CG11
      *         MOVE GS-TELEFONE-COM      TO FONE2-CG11
      *         MOVE GS-RAMAL-COM         TO RAMAL2-CG11

                MOVE GS-ENDERECO-COM      TO ENDERECO3-CG11
                MOVE GS-REFERENCIA-COM    TO PONTO-REFER3-CG11
                MOVE GS-BAIRRO-COM        TO BAIRRO3-CG11
                MOVE GS-CIDADE-COM        TO CIDADE3-CG11
                MOVE GS-TELEFONE-COM      TO FONE3-CG11
                MOVE GS-RAMAL-COM         TO RAMAL3-CG11


                MOVE GS-CELULAR-RES       TO CELULAR2-CG11
                MOVE GS-CPF               TO CPF2-CG11
                MOVE GS-RG                TO RG2-CG11
                MOVE GS-DATA-EXPEDICAO    TO DT-EXPEDICAO2-CG11
                MOVE GS-ORGAO-EXPEDICAO   TO ORGAO-EXPEDICAO2-CG11
                STRING GS-DATA-NASCIMENTO(5:4)
                       GS-DATA-NASCIMENTO(3:2)
                       GS-DATA-NASCIMENTO(1:2)
                  INTO DATA-NASC2-CG11
                MOVE GS-PAI                     TO NOME-PAI-CG11
                MOVE GS-MAE                     TO NOME-MAE-CG11
                MOVE GS-ENDERECO-PAI            TO ENDERECO-PAIS-CG11
                MOVE GS-CIDADE-PAI              TO CIDADE-PAIS-CG11
                MOVE GS-TELEFONE-PAI            TO FONE-PAIS-CG11
                MOVE GS-CELULAR-PAI             TO CELULAR-PAIS-CG11
                MOVE GS-CEP-PAI                 TO CEP-PAIS-CG11
                MOVE GS-ENDERECO-REPUBLICA      TO ENDERECO-REP-CG11
                MOVE GS-BAIRRO-REPUBLICA        TO BAIRRO-REP-CG11
                MOVE GS-CIDADE-REPUBLICA        TO CIDADE-REP-CG11
                MOVE GS-CEP-REPUBLICA           TO CEP-REP-CG11

                REWRITE REG-CGD011 INVALID KEY
                    MOVE "Erro de Regravação...CGD011" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ
           CLOSE CGD011
           OPEN INPUT CGD011.


       GRAVA-DADOS SECTION.
           CLOSE    MTD019 MTD019F
           OPEN I-O MTD019 LOG002 MTD019F

           MOVE ZEROS TO ST-MTD019.
           PERFORM UNTIL ST-MTD019 = "10"
             WRITE REG-MTD019 INVALID KEY
                 ADD 1 TO SEQ-MT19
             NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "MTD019"    TO LOG2-ARQUIVO
                 MOVE "MTP029"    TO LOG2-PROGRAMA
                 MOVE REG-MTD019  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 MOVE "10" TO ST-MTD019.

           MOVE CONTRATO-MT19     TO CONTRATO-MT19F
           MOVE SEQ-MT19          TO SEQ-MT19F
           MOVE GS-ACP-IMAGEM     TO IMAGEM-MT19F
           WRITE REG-MTD019F INVALID KEY
                MOVE "Erro de Gravação...MTD019F" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           END-WRITE

           CLOSE      MTD019 LOG002  MTD019F
           OPEN INPUT MTD019 MTD019F


           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ    TO GS-NR-ALBUM.
       REGRAVA-DADOS SECTION.
           CLOSE    MTD019 MTD019F
           OPEN I-O MTD019 LOG002 MTD019F

           REWRITE REG-MTD019 INVALID KEY
                 MOVE "Erro Regravacao MTD019" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "A"         TO LOG2-OPERACAO
                 MOVE "MTD019"    TO LOG2-ARQUIVO
                 MOVE "MTP029"    TO LOG2-PROGRAMA
                 MOVE REG-MTD019  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE.

           MOVE CONTRATO-MT19     TO CONTRATO-MT19F
           MOVE SEQ-MT19          TO SEQ-MT19F
           MOVE GS-ACP-IMAGEM     TO IMAGEM-MT19F
           WRITE REG-MTD019F INVALID KEY
                REWRITE REG-MTD019F INVALID KEY
                     MOVE "Erro de Regravação...MTD019F" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-WRITE

           CLOSE      MTD019 LOG002 MTD019F
           OPEN INPUT MTD019 MTD019F

           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ULT-SEQ  TO GS-NR-ALBUM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           initialize reg-mtd019

           MOVE GS-NR-CONTRATO     TO CONTRATO-MT19 NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE SPACES TO IDENTIFICACAO-CO40
                MOVE ZEROS  TO INSTITUICAO-CO40
                MOVE ZEROS  TO MESANO-PREV-CO40.

           STRING MESANO-PREV-CO40(5:2) MESANO-PREV-CO40(1:4) INTO
                  GS-MES-ANO

           MOVE IDENTIFICACAO-CO40   TO GS-DESC-CONTRATO.

           MOVE INSTITUICAO-CO40     TO GS-COD-ENTIDADE
                                        CODIGO-IE10
           READ IED010 INVALID KEY
                MOVE SPACES TO NOME-IE10
           END-READ
           MOVE NOME-IE10            TO GS-ENTIDADE

           IF GS-ORDEM = 1 MOVE ZEROS TO SEQ-MT19
              START MTD019 KEY IS NOT < ALBUM-MT19
                    INVALID KEY MOVE "10" TO ST-MTD019
              END-START
              MOVE ZEROS TO ULT-SEQ
           ELSE PERFORM ACHA-ULT-SEQUENCIA
              MOVE ZEROS TO CURSO-MT19
              MOVE SPACES TO NOME-FORM-MT19
              MOVE GS-NR-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS NOT < ALT1-MT19 INVALID KEY
                    MOVE "10" TO ST-MTD019.

           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-MTD019 = "10"
              READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
                IF CONTRATO-MT19 <> GS-NR-CONTRATO
                   MOVE "10" TO ST-MTD019
                ELSE
                   IF GS-ORDEM = 1
                      MOVE SEQ-MT19 TO ULT-SEQ
                   END-IF
                   PERFORM MOVER-DADOS-LISTA
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ TO GS-NR-ALBUM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE CONTRATO-MT19      TO GS-LINDET(1: 5)
           MOVE SEQ-MT19           TO GS-LINDET(6: 5)
           MOVE CURSO-MT19         TO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE SPACES TO NOME-IE11.
           MOVE NOME-IE11          TO GS-LINDET(11: 10)
           MOVE CIDADE-MT19        TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-COMPL-CID.
           MOVE NOME-COMPL-CID     TO GS-LINDET(22: 10)
           MOVE NOME-FORM-MT19     TO GS-LINDET(33: 31)
           MOVE FONE-MT19          TO GS-LINDET(64: 9)
           MOVE IDENTIFICADO-MT19  TO GS-LINDET(73: 1)
           MOVE TURMA-MT19         TO GS-LINDET(78: 4)
           MOVE TURNO-MT19         TO GS-LINDET(82:10).
       ACHA-ULT-SEQUENCIA SECTION.
           MOVE GS-NR-CONTRATO    TO CONTRATO-MT19.
           MOVE ZEROS          TO SEQ-MT19.
           START MTD019 KEY IS NOT < ALBUM-MT19 INVALID KEY
                 MOVE "10" TO ST-MTD019.
           MOVE ZEROS TO ULT-SEQ.
           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
               NOT AT END
                 IF CONTRATO-MT19 <> GS-NR-CONTRATO
                    MOVE "10" TO ST-MTD019
                 ELSE
                    MOVE SEQ-MT19  TO ULT-SEQ
                 END-IF
              END-READ
           END-PERFORM.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP029a" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           IF GS-ORDEM = 1 MOVE ZEROS TO SEQ-MT19
                           MOVE GS-NR-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS NOT < ALBUM-MT19
                    INVALID KEY MOVE "10" TO ST-MTD019
              END-START
           ELSE
              MOVE ZEROS TO CURSO-MT19
              MOVE SPACES TO NOME-FORM-MT19
              MOVE GS-NR-CONTRATO TO CONTRATO-MT19
              START MTD019 KEY IS = ALT1-MT19 INVALID KEY
                    MOVE "10" TO ST-MTD019.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-MTD019 = "10"
             READ MTD019 NEXT RECORD AT END MOVE "10" TO ST-MTD019
              NOT AT END
                IF CONTRATO-MT19 <> GS-NR-CONTRATO
                         MOVE "10" TO ST-MTD019
                ELSE
                  MOVE SPACES             TO LINDET-REL
                  MOVE CONTRATO-MT19      TO LINDET-REL(1: 5)
                  MOVE SEQ-MT19           TO LINDET-REL(6: 5)
                  MOVE CURSO-MT19         TO CODIGO-IE11
                  READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                  END-READ
                  MOVE NOME-IE11          TO LINDET-REL(11: 10)
                  MOVE CIDADE-MT19        TO CIDADE
                  READ CAD010 INVALID KEY MOVE SPACES TO NOME-COMPL-CID
                  END-READ
                  MOVE NOME-COMPL-CID     TO LINDET-REL(22: 10)
                  MOVE NOME-FORM-MT19     TO LINDET-REL(33: 31)
                  MOVE FONE-MT19          TO LINDET-REL(64: 9)
                  MOVE IDENTIFICADO-MT19  TO LINDET-REL(73: 1)
                  MOVE TURMA-MT19         TO LINDET-REL(78: 4)
                  MOVE TURNO-MT19         TO LINDET-REL(82:10)

                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO GS-FLAG-CRITICA
           move spaces to mensagem.

       COPIAR-IMAGEM SECTION.
           MOVE 1                         TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                INITIALIZE REG-PAR001.

           move 255 to wsIndice
           perform until gs-acp-imagem(wsIndice:1) = "\"
              if gs-acp-imagem(wsIndice:1) = "."
                 move gs-acp-imagem(wsindice:5)   to EXTENSAO-ARQ
              end-if
              subtract 1   from wsIndice
           end-perform

           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-PAR001 DELIMITED BY " " "\" EMPRESA-W
             INTO CAMINHO-IMAGEM

           call "CBL_CREATE_DIR" using     caminho-imagem
                                 returning status-code

           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-PAR001 DELIMITED BY " " "\" EMPRESA-W
                  "\" GS-NR-CONTRATO INTO CAMINHO-IMAGEM
           call "CBL_CREATE_DIR" using     caminho-imagem
                                 returning status-code


           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-PAR001 DELIMITED BY " " "\" EMPRESA-W
              "\" GS-NR-CONTRATO '\"' GS-NR-ALBUM "-" GS-NOME-FORMANDO
              '"'
                  delimited by "  "
                  EXTENSAO-ARQ delimited by " " '"'
                  INTO CAMINHO-IMAGEM

           IF GS-ACP-IMAGEM <> CAMINHO-IMAGEM
              call "CBL_COPY_FILE" using gs-acp-imagem
                                         caminho-imagem
                               returning status-code
              MOVE CAMINHO-IMAGEM TO GS-ACP-IMAGEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 MTD019 IED011  CAD004 COD008  MTD025
                 CGD010 CGD011 IED010 MTD019F PAR001 MTD020

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP029"            to logacess-programa
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

           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
