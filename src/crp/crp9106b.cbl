       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9106b.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO REMESSA P/ BRADESCO
       DATE-WRITTEN.  31-03-2010.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX020.
           COPY CAPX002.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX014.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT SEQBRAD ASSIGN TO PATH-SEQBRAD
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-SEQ
                  RECORD KEY IS CONT-SEQUENCIA.
           SELECT REMESSA ASSIGN TO REMESSA-NOME
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT REMESSA2 ASSIGN TO REMESSA-NOME2
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT SACADO ASSIGN TO ARQUIVO-SACADO
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.
           SELECT AUXILIAR ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-AUXILIAR
                  RECORD KEY IS AUX-COD-COMPL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CAPW002.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW014.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  SEQBRAD.
       01  REG-SEQBRAD.
           05  CONT-SEQUENCIA  PIC 9.
           05  SEQUENCIA       PIC 9(10).
           05  NOSSO-NUMERO    PIC 9(11).

       FD  REMESSA.
       01  REG-REMESSA.
           05  ID-REG-REM       PIC X(01).
           05  DADOS-REM        PIC X(399).
       FD  REMESSA2
           label record is omitted.
       01  REG-REMESSA2.
           05  ID-REG-REM2      PIC X(01).
           05  DADOS-REM2       PIC X(399).
           05  pula-rem2        pic x(02).

       FD  SACADO
           label record is omitted.
       01  REG-SACADO.
           05 DADOS-SACADO      PIC X(284).
           05 PULA-SACADO       PIC X(02).

       FD  AUXILIAR.
       01  REG-AUXILIAR.
           05 AUX-COD-COMPL     PIC 9(09).

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(35).
           05  ENDERECO-WK      PIC X(35).
           05  CEP-WK           PIC 9(8).
           05  CIDADE-WK        PIC X(15).
           05  UF-WK            PIC XX.
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9106B.CPB".
           COPY "CRP9106B.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD010           PIC XX     VALUE SPACES.
           05 ST-CAD018           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-CAD002           PIC XX     VALUE SPACES.
           05 ST-REM              PIC XX     VALUE SPACES.
           05 ST-CGD010           PIC XX     VALUE SPACES.
           05 ST-CGD011           PIC XX     VALUE SPACES.
           05 ST-CGD014           PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX     VALUE SPACES.
           05 ST-CRD201           PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 ST-AUXILIAR         PIC XX     VALUE SPACES.
           05 ST-SEQ              PIC XX     VALUE SPACES.
           05 ST-WORK             PIC XX     VALUE SPACES.
           05 VARIA-W             PIC 9(8)   VALUE ZEROS.
           05 VARIA-W2            PIC 9(8)   VALUE ZEROS.
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
           05 SENHA               PIC 9(04)  VALUE ZEROS.
           05 TIPO-W              PIC 9      VALUE ZEROS.
           05 SEQ-W               PIC 9(6)   VALUE ZEROS.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(8)   VALUE ZEROS.
           05 HORA-BRA            PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ             PIC 9(5)   VALUE ZEROS.
           05 DATA-E             PIC 99/99/99.
           05 REMESSA-NOME        PIC X(12)  VALUE SPACES.
           05 SEQUENCIA-W         PIC 9(10)     VALUE ZEROS.
           05 NOSSO-NUMERO-W      PIC 9(11)     VALUE ZEROS.
           05 VALOR-ATRASO        PIC 9(11)V99 VALUE ZEROS.
           05 COD-COMPL-CR20-W    PIC 9(09)  VALUE ZEROS.
           05 CONF                PIC X      VALUE SPACES.
           05 VALOR-TOTAL         PIC 9(12)V99 VALUE ZEROS.
           05 QTDE-TIT            PIC 9(4)     VALUE ZEROS.
           05 ERRO-W              PIC 9        VALUE ZEROS.
           05 DATA-INV            PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV      PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV      PIC 9(8)     VALUE ZEROS.
           05 MOVTO-INI-INV       PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV       PIC 9(8)     VALUE ZEROS.
           05 TAXA-JUROS          PIC 9(03)V99 VALUE ZEROS.
           05 LIN                 PIC 9(02)    VALUE ZEROS.
           05 IND                 PIC 9(02)    VALUE ZEROS.
           05 TOTAL               PIC 9(4)     VALUE ZEROS.
           05 TOTAL2              PIC 9(4)     VALUE ZEROS.
           05 AUX-TIPO-DOCTO      PIC X(20)    VALUE SPACES.
           05 MENSAGEM            PIC X(200).
           05 TIPO-MSG            PIC X(01).
           05 RESP-MSG            PIC X(01).
           05 VALOR               PIC 9(4)     VALUE ZEROS.
           05 RESTO               PIC 9(2)     VALUE ZEROS.
           05 AUX-TIPO            PIC 9(1)     VALUE ZEROS.
           05 DIGITO-VERIFICADOR  PIC X(1)     VALUE SPACES.
           05 SEQUENCIA-REGISTRO  PIC 9(6)     VALUE ZEROS.
           05 DATAW.
              10  DIA-W       PIC 99.
              10  MES-W       PIC 99.
              10  ANO-W       PIC 99.
           05 DATA-W REDEFINES DATAW PIC 9(6).
           05 DATAI.
              10  ANO-I       PIC 99.
              10  MES-I       PIC 99.
              10  DIA-I       PIC 99.
           05 DATA-I REDEFINES DATAI PIC 9(6).
           05 REM-TIPO0.
              10  T0-IDENTIFICACAO-ARQ-REM  PIC X(01)  VALUE "1".
              10  T0-LITERAL-REMESSA        PIC X(07)  VALUE "Remessa".
              10  T0-CODIGO-SERVICO         PIC X(02)  VALUE "01".
              10  T0-LITERAL-SERVICO        PIC x(15)  VALUE "Cobrança".
              10  T0-CODIGO-EMPRESA         PIC X(20)
                  VALUE "00000000000004295788".
              10  T0-NOME-EMPRESA           PIC x(30)
                  VALUE "FERREIRA FOTO E VIDEO LTDA".
              10  T0-NUMERO-BRADESCO-COMP   PIC X(03)  VALUE "237".
              10  T0-NOME-BANCO-EXTENSO     PIC X(15)  VALUE "Bradesco".
              10  T0-DATA-GRAVACAO-ARQ.
                  15 T0-DIA-GRAVACAO-ARQ    PIC 9(02).
                  15 T0-MES-GRAVACAO-ARQ    PIC 9(02).
                  15 T0-ANO-GRAVACAO-ARQ    PIC 9(02).
              10  FILLER                    PIC x(08).
              10  T0-IDENTIFICACAO-SISTEMA  PIC X(02)  VALUE "MX".
              10  T0-SEQUENCIAL-ARQUIVO     PIC 9(07)  VALUE ZEROS.
              10  FILLER                    PIC X(277) VALUE SPACES.
              10  T0-N-SEQUENCIAL-REGISTRO  PIC 9(06)  VALUE 000001.
           05 REM-TIPO1.
              10  T1-AGENCIA-DEBITO            PIC 9(05) VALUE ZEROS.
              10  T1-DIGITO-AGENCIA-DEBITO     PIC X(01) VALUE SPACES.
              10  T1-RAZAO-CONTA-CORRENTE      PIC X(05) VALUE SPACES.
              10  T1-CONTA-CORRENTE            PIC 9(07) VALUE ZEROS.
              10  T1-DIGITO-CONTA-CORRENTE     PIC X(01) VALUE SPACES.
              10  T1-IDENTIFICACAO-EMPRESA-CED PIC X(17) VALUE SPACES.
              10  T1-NUM-CONTROLE-PARTICIPANTE PIC X(25) VALUE SPACES.
              10  T1-COD-BANCO-DEB-COMP        PIC 9(03) VALUE 237.
              10  FILLER                       PIC 9(05) VALUE ZEROS.
              10  T1-IDENTIF-TITULO-BANCO      PIC X(12) VALUE SPACES.
              10  T1-DESCONTO-BONIF-DIA        PIC 9(10) VALUE ZEROS.
              10  T1-CONDICAO-EMISSAO-PAPELETA PIC 9(01) VALUE 2.
              10  T1-IDENT-PAPELETA-DEB-AUT    PIC X(01) VALUE SPACES.
              10  T1-IDENTIF-OP-BANCO          PIC X(10) VALUE SPACES.
              10  T1-INDICADOR-RATEIO-CRED     PIC X(01) VALUE "R".
              10  T1-ENDERECAMENTO-AVISO-DEB   PIC 9(01) VALUE ZEROS.
              10  FILLER                       PIC X(02) VALUE SPACES.
              10  T1-IDENTIFICACAO-OCORRENC    PIC 9(02) VALUE ZEROS.
              10  T1-NUM-DOCUMENTO             PIC X(10) VALUE SPACES.
              10  T1-DATA-VENCTO-TITULO.
                  15 T1-DIA-VENCTO-TIT         PIC 9(02).
                  15 T1-MES-VENCTO-TIT         PIC 9(02).
                  15 T1-ANO-VENCTO-TIT         PIC 9(02).
              10  T1-VALOR-TITULO              PIC 9(11)V99 VALUE ZEROS.
              10  T1-BANCO-ENCARREG-COB        PIC 9(03).
              10  T1-AGENCIA-DEPOSITARIA       PIC 9(05) VALUE ZEROS.
              10  T1-ESPECIE-TITULO            PIC X(02) VALUE "01".
              10  T1-IDENTIFICADO              PIC X(01) VALUE "N".
              10  T1-DATA-EMISSAO-TIT.
                  15 T1-DIA-EMISSAO-TIT        PIC 9(02).
                  15 T1-MES-EMISSAO-TIT        PIC 9(02).
                  15 T1-ANO-EMISSAO-TIT        PIC 9(02).
              10  T1-INSTRUCAO1                PIC X(02).
              10  T1-INSTRUCAO2                PIC X(02).
              10  T1-VALOR-COB-DIA             PIC 9(11)V99.
              10  T1-DATA-LIMITE-CONSC-DESC.
                  15 T1-DIA-LIMITE             PIC 9(02).
                  15 T1-MES-LIMITE             PIC 9(02).
                  15 T1-ANO-LIMITE             PIC 9(02).
              10  T1-VALOR-DESCONTO            PIC 9(11)V99.
              10  T1-VALOR-IOF                 PIC 9(11)V99.
              10  T1-VALOR-ABATIMENTO          PIC 9(11)V99.
              10  T1-TIPO-INSC-SACADO          PIC 9(02).
              10  T1-NUMERO-INSCRICAO-SACADO   PIC 9(14).
              10  T1-NOME-SACADO               PIC X(40).
              10  T1-ENDERECO-SACADO           PIC X(40).
              10  T1-MENSAGEM1                 PIC X(12).
              10  T1-CEP-SACADO                PIC 9(08).
              10  T1-SACADOR-AVALISTA          PIC X(60).
              10  T1-SEQUENCIAL-REGISTRO       PIC 9(06).
           05 REM-TIPO9.
              10 T9-BRANCO                     PIC X(393) VALUE SPACES.
              10 T9-QUANTIDADE                 PIC 9(06)  VALUE ZEROS.
           05 REM-SACADO.
              10 SACADO-CODIGO                 PIC X(15).
              10 SACADO-TIPO-DOCTO             PIC X(01).
              10 SACADO-CNPJ                   PIC X(14).
              10 SACADO-NOME                   PIC X(40).
              10 SACADO-ENVIAR-EMAIL           PIC 9(01).
              10 SACADO-EMAIL                  PIC X(50).
              10 SACADO-OPCAO-EMAIL            PIC X(01).
              10 SACADO-ENDERECO               PIC X(40).
              10 FILLER                        PIC X(08).
              10 FILLER                        PIC X(15).
              10 SACADO-BAIRRO                 PIC X(30).
              10 SACADO-CIDADE                 PIC X(30).
              10 SACADO-UF                     PIC X(02).
              10 SACADO-CEP                    PIC 9(08).
              10 FILLER                        PIC 9(03) VALUE ZEROS.
              10 FILLER                        PIC 9(05) VALUE ZEROS.
              10 FILLER                        PIC 9(01) VALUE ZEROS.
              10 FILLER                        PIC 9(12) VALUE ZEROS.
              10 FILLER                        PIC 9(01) VALUE ZEROS.
              10 FILLER                        PIC X(05) VALUE SPACE.
              10 FILLER                        PIC 9(01) VALUE ZEROS.
              10 FILLER                        PIC X(01) VALUE "N".
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BANCO BRADESCO'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                                ENDERECO
      -    "            CEP       CIDADE          UF DOCUMENTO
      -    " VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(35) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(35) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.ZZZ.
           05  FILLER               PIC X     VALUE SPACES.
           05  CIDADE-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  UF-REL               PIC XX    VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  DOCTO-REL            PIC X(11) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  VALOR-REL            PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET1.
           05  FILLER               PIC X(20) VALUE 'VALOR TOTAL.: '.
           05  VALOR-TOTAL-REL      PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(20) VALUE 'QTDE TITULOS: '.
           05  QTDE-TIT-TOTAL-REL   PIC ZZZ.ZZZ.

       01  TAB-SEQUENCIA            PIC 9(13).
       01  FILLER REDEFINES TAB-SEQUENCIA OCCURS 13 TIMES.
           05 TAB-SEQ               PIC 9(01).

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


       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-I FROM DATE.
           MOVE DIA-I TO DIA-W.
           MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W.
           MOVE DATA-W TO DATA-DIA.
           MOVE DATA-I(3: 4) TO DATA-DIA-I(5: 4)
           MOVE ANO-I        TO DATA-DIA-I(3: 2)
           IF ANO-I > 90 MOVE "19" TO DATA-DIA-I(1: 2)
           ELSE MOVE 20 TO DATA-DIA-I(1: 2).
           ACCEPT HORA-BRA FROM TIME.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD014.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "SEQBRADb" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-SEQBRAD.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CGD014 CAD010 CAD018 CAD002.
           OPEN I-O   CRD020
           CLOSE      CRD020
           OPEN INPUT CRD020

           OPEN I-O SEQBRAD.
           IF ST-SEQ = "35"
              CLOSE       SEQBRAD
              OPEN OUTPUT SEQBRAD
              CLOSE       SEQBRAD
              OPEN I-O    SEQBRAD
              MOVE 1     TO CONT-SEQUENCIA
              MOVE ZEROS TO SEQUENCIA
              MOVE ZEROS TO NOSSO-NUMERO
              WRITE REG-SEQBRAD
              CLOSE       SEQBRAD
              OPEN I-O    SEQBRAD.

           CLOSE      SEQBRAD
           OPEN INPUT SEQBRAD

           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD014 <> "00"
              MOVE "ERRO ABERTURA CGD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD014 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP9106B"          to logacess-programa
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
               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-REMESSA-TRUE
                    MOVE GS-TIPO-DOCTO TO AUX-TIPO-DOCTO
                    IF GS-ACP-TIPO = 1
                       PERFORM GERAR-SACADO
                    ELSE
                       PERFORM GERA-ARQ-REMESSA
                    END-IF
                    MOVE AUX-TIPO-DOCTO TO GS-TIPO-DOCTO
                    MOVE "REFRESH-DATA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-ATUALIZA-PORTADOR-TRUE
                    MOVE "Deseja Realmente Transferir o Portador ?"
                      TO MENSAGEM
                    MOVE "Q" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                    IF RESP-MSG EQUAL "S"
                       PERFORM ATUALIZA-PORTADOR-RECEBER
                    END-IF
               WHEN GS-LE-PORTADOR2-TRUE
                    PERFORM LER-PORTADOR2
               WHEN GS-POPUP-PORTADOR2-TRUE
                    PERFORM POPUP-PORTADOR2
               WHEN GS-VALIDA-SENHA-TRUE
                    PERFORM VALIDAR-SENHA
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDAR-SENHA SECTION.
           MOVE USUARIO-W           TO NOME-REDUZ-CA002
           READ CAD002 KEY IS NOME-REDUZ-CA002 INVALID KEY
                MOVE "Usuário Não Cadastrado" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SENHA-CA002    TO SENHA
                IF SENHA <> GS-ACP-SENHA
                   MOVE "Senha do Usuário Logado Inválida" TO MENSAGEM
                   MOVE "C"                                TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       GERAR-SACADO SECTION.
           ACCEPT VARIA-W2 FROM TIME.
           ADD  50 TO VARIA-W2

           OPEN OUTPUT AUXILIAR
           CLOSE       AUXILIAR
           OPEN I-O    AUXILIAR

           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO

           MOVE GS-NOME-ARQ-REMESSA  TO ARQUIVO-SACADO
           OPEN OUTPUT SACADO


           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV.
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO VENCTO-FIM-INV.

           MOVE GS-MOVTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI-INV.
           MOVE GS-MOVTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO MOVTO-FIM-INV.


           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBRAD.
           MOVE SEQUENCIA         TO SEQUENCIA-W
           MOVE NOSSO-NUMERO      TO NOSSO-NUMERO-W.

           INITIALIZE REG-CRD020
                      SEQUENCIA-REGISTRO

           MOVE ZEROS TO VALOR-TOTAL SEQ-W.
           IF GS-ALBUM > 0
              STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
              MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                   MOVE "10" TO ST-CRD020
              END-START

              PERFORM UNTIL ST-CRD020 = "10"
                   READ CRD020 NEXT WITH IGNORE LOCK AT END
                       MOVE "10" TO ST-CRD020
                   NOT AT END
                       IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                          MOVE "10" TO ST-CRD020
                       ELSE
                           IF SITUACAO-CR20 = 0
                              IF MOVTO-INI-INV = 0 OR
                                 (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                   AND
                                 DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                 IF VENCTO-INI-INV = 0 OR
                                    (DATA-VENCTO-CR20 NOT <
                                     VENCTO-INI-INV AND
                                     DATA-VENCTO-CR20 NOT >
                                     VENCTO-FIM-INV)
                                    PERFORM FAZER-COMPARACOES
                                 END-IF
                              END-IF
                           END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-CONTRATO > 0
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                 START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                      MOVE "10" TO ST-CRD020
                 END-START
                 PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT WITH IGNORE LOCK AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF CLIENTE-CR20(1:4) <> GS-CONTRATO
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF SITUACAO-CR20 = 0
                                IF MOVTO-INI-INV = 0 OR
                                   (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                    AND
                                    DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                    IF VENCTO-INI-INV = 0 OR
                                       (DATA-VENCTO-CR20 NOT <
                                        VENCTO-INI-INV AND
                                        DATA-VENCTO-CR20 NOT >
                                        VENCTO-FIM-INV)
                                        PERFORM FAZER-COMPARACOES
                                     END-IF
                                 END-IF
                             END-IF
                          END-IF
                     END-READ
                 END-PERFORM
              ELSE
                 IF GS-MOVTO-INI > 0
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE MOVTO-INI-INV TO DATA-MOVTO-CR20
                    START CRD020 KEY IS NOT LESS ALT3-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-MOVTO-CR20 > MOVTO-FIM-INV OR
                                SITUACAO-CR20 <> 0
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF VENCTO-INI-INV = 0 OR
                                   (DATA-VENCTO-CR20 NOT <
                                    VENCTO-INI-INV AND
                                    DATA-VENCTO-CR20 NOT >
                                    VENCTO-FIM-INV)
                                    PERFORM FAZER-COMPARACOES
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM
                 ELSE
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE VENCTO-INI-INV TO DATA-VENCTO-CR20
                    START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-VENCTO-CR20 > VENCTO-FIM-INV
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF SITUACAO-CR20 = 0
                                   IF MOVTO-INI-INV = 0 OR
                                      (DATA-MOVTO-CR20 NOT <
                                       MOVTO-INI-INV AND
                                       DATA-MOVTO-CR20 NOT >
                                       MOVTO-FIM-INV)
                                       PERFORM FAZER-COMPARACOES
                                   END-IF
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM.

           CLOSE      AUXILIAR
           OPEN INPUT AUXILIAR

           INITIALIZE REG-AUXILIAR
           START AUXILIAR KEY IS NOT LESS AUX-COD-COMPL INVALID KEY
                 MOVE "10" TO ST-AUXILIAR.

           PERFORM UNTIL ST-AUXILIAR = "10"
                 READ AUXILIAR NEXT WITH IGNORE LOCK AT END
                      MOVE "10" TO ST-AUXILIAR
                 NOT AT END
                      MOVE AUX-COD-COMPL       TO COD-COMPL-CG10
                                                  COD-COMPL-CG11
                      READ CGD010 INVALID KEY
                           INITIALIZE REG-CGD010
                      END-READ

                      READ CGD011 INVALID KEY
                           INITIALIZE REG-CGD011
                      END-READ

                      MOVE SPACES          TO SACADO-CODIGO
                      MOVE AUX-COD-COMPL   TO SACADO-CODIGO(7:9)
                      MOVE COMPRADOR-CG10  TO SACADO-NOME

                      MOVE SPACES TO SACADO-CNPJ

                      IF CPF1-CG11 > 100000000000
                         MOVE "J"             TO SACADO-TIPO-DOCTO
                         MOVE CPF1-CG11(3:14) TO SACADO-CNPJ
                      ELSE
                         MOVE "F"             TO SACADO-TIPO-DOCTO
                         MOVE CPF1-CG11(6:11) TO SACADO-CNPJ(4:11)
                      END-IF
                      MOVE ENDERECO1-CG11  TO SACADO-ENDERECO
                      MOVE CEP1-CG11       TO SACADO-CEP
                      MOVE SPACES          TO SACADO-EMAIL
      *               IF E-MAIL-CG11 <> SPACES
      *                  MOVE 1            TO SACADO-ENVIAR-EMAIL
      *               ELSE
                         MOVE 0            TO SACADO-ENVIAR-EMAIL
      *               END-IF
                      MOVE SPACES          TO SACADO-OPCAO-EMAIL
                      MOVE CIDADE1-CG11    TO CIDADE
                      READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                      END-READ
                      MOVE BAIRRO1-CG11    TO SACADO-BAIRRO
                      MOVE NOME-CID        TO SACADO-CIDADE
                      MOVE UF-CID          TO SACADO-UF

                      MOVE REM-SACADO      TO DADOS-SACADO
                      MOVE X"0D0A"         TO PULA-SACADO

                      WRITE REG-SACADO
                 END-READ
           END-PERFORM


           CLOSE      AUXILIAR

           MOVE ZEROS TO GS-SEQ.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE SACADO.

           PERFORM CARREGA-LISTA2.

       GERA-ARQ-REMESSA SECTION.
           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO

           MOVE "AUXILIAR"       TO REMESSA-NOME
           MOVE GS-NOME-ARQ-REMESSA  TO REMESSA-NOME2.
           OPEN OUTPUT REMESSA REMESSA2.


           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV.
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO VENCTO-FIM-INV.

           MOVE GS-MOVTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI-INV.
           MOVE GS-MOVTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO MOVTO-FIM-INV.


           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBRAD.
           MOVE SEQUENCIA         TO SEQUENCIA-W
           MOVE NOSSO-NUMERO      TO NOSSO-NUMERO-W.

           INITIALIZE REG-CRD020
                      SEQUENCIA-REGISTRO

           MOVE ZEROS TO VALOR-TOTAL SEQ-W.
           PERFORM MOVER-DADOS-TIPO0.

           IF GS-ALBUM > 0
              STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
              MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                   MOVE "10" TO ST-CRD020
              END-START

              PERFORM UNTIL ST-CRD020 = "10"
                   READ CRD020 NEXT WITH IGNORE LOCK AT END
                       MOVE "10" TO ST-CRD020
                   NOT AT END
                       IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                          MOVE "10" TO ST-CRD020
                       ELSE
                           IF SITUACAO-CR20 = 0
                              IF MOVTO-INI-INV = 0 OR
                                 (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                   AND
                                 DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                 IF VENCTO-INI-INV = 0 OR
                                    (DATA-VENCTO-CR20 NOT <
                                     VENCTO-INI-INV AND
                                     DATA-VENCTO-CR20 NOT >
                                     VENCTO-FIM-INV)
                                    PERFORM FAZER-OUTRAS-COMPARACOES
                                 END-IF
                              END-IF
                           END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-CONTRATO > 0
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                 START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                      MOVE "10" TO ST-CRD020
                 END-START
                 PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT WITH IGNORE LOCK AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF CLIENTE-CR20(1:4) <> GS-CONTRATO
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF SITUACAO-CR20 = 0
                                IF MOVTO-INI-INV = 0 OR
                                   (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                    AND
                                    DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                    IF VENCTO-INI-INV = 0 OR
                                       (DATA-VENCTO-CR20 NOT <
                                        VENCTO-INI-INV AND
                                        DATA-VENCTO-CR20 NOT >
                                        VENCTO-FIM-INV)
                                        PERFORM FAZER-OUTRAS-COMPARACOES
                                     END-IF
                                 END-IF
                             END-IF
                          END-IF
                     END-READ
                 END-PERFORM
              ELSE
                 IF GS-MOVTO-INI > 0
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE MOVTO-INI-INV TO DATA-MOVTO-CR20
                    START CRD020 KEY IS NOT LESS ALT3-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-MOVTO-CR20 > MOVTO-FIM-INV OR
                                SITUACAO-CR20 <> 0
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF VENCTO-INI-INV = 0 OR
                                   (DATA-VENCTO-CR20 NOT <
                                    VENCTO-INI-INV AND
                                    DATA-VENCTO-CR20 NOT >
                                    VENCTO-FIM-INV)
                                    PERFORM FAZER-OUTRAS-COMPARACOES
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM
                 ELSE
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE VENCTO-INI-INV TO DATA-VENCTO-CR20
                    START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-VENCTO-CR20 > VENCTO-FIM-INV
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF SITUACAO-CR20 = 0
                                   IF MOVTO-INI-INV = 0 OR
                                      (DATA-MOVTO-CR20 NOT <
                                       MOVTO-INI-INV AND
                                       DATA-MOVTO-CR20 NOT >
                                       MOVTO-FIM-INV)
                                       PERFORM FAZER-OUTRAS-COMPARACOES
                                   END-IF
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM.



           MOVE ZEROS TO GS-SEQ.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-TIPO4.
           CLOSE REMESSA REMESSA2.
           PERFORM CARREGA-LISTA.


       FAZER-OUTRAS-COMPARACOES SECTION.
           IF PORTADOR-CR20 = GS-PORTADOR
              IF TIPO-DOCTO-CR20 = AUX-TIPO
                 MOVE SEQ-W         TO GS-SEQ
                 MOVE "REFRESH-DATA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 PERFORM MOVER-DADOS-TIPO1.

       FAZER-COMPARACOES SECTION.
           IF PORTADOR-CR20 = GS-PORTADOR
              IF TIPO-DOCTO-CR20 = AUX-TIPO
                 MOVE SEQ-W          TO GS-SEQ
                 MOVE "REFRESH-DATA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE COD-COMPL-CR20 TO AUX-COD-COMPL
                 WRITE REG-AUXILIAR.

       ATUALIZA-PORTADOR-RECEBER SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020

           OPEN INPUT REMESSA.
           PERFORM ABRE-ARQUIVO-ANOTACAO.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
               READ REMESSA AT END
                    MOVE "10" TO ST-REM
               NOT AT END
                    MOVE REG-REMESSA(395: 6) TO GS-EXIBE-SEQ
                    MOVE "REFRESH-WIN3"      TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    MOVE REG-REMESSA(1: 1)   TO TIPO-W
                    IF TIPO-W = 0 OR 9
                       CONTINUE
                    ELSE
                         MOVE DADOS-REM           TO REM-TIPO1
                         MOVE T1-NUM-CONTROLE-PARTICIPANTE(1:9)
                           TO COD-COMPL-CR20
                         MOVE T1-NUM-CONTROLE-PARTICIPANTE(10:5)
                           TO SEQ-CR20
                         READ CRD020 INVALID KEY
                              MOVE SPACES TO MENSAGEM
                              STRING "Título Não Encontrado para Atualiz
      -                              "ar o Contas a Receber" X"0DA0"
                                     "COD-COMPL-CR20 = "
                                      COD-COMPL-CR20         X"0DA0"
                                     "SEQ-CR20 = "
                                      SEQ-CR20 INTO MENSAGEM
                              MOVE   "C" TO TIPO-MSG
                              PERFORM EXIBIR-MENSAGEM
                         NOT INVALID KEY
                              PERFORM GRAVA-ANOTACAO
                              MOVE GS-ACP-PORTADOR TO PORTADOR-CR20
                              REWRITE REG-CRD020
                              END-REWRITE
                         END-READ
                    END-IF
               END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201 CRD020
           OPEN INPUT   CRD020.

           PERFORM ATUALIZA-SEQUENCIA

           MOVE "Portador Atualizado com Sucesso" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           PERFORM LIMPAR-TELA

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.

       LIMPAR-TELA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE GS-NOME-ARQ-REMESSA
                      GS-SEQ
                      GS-NOME-ARQ-REMESSA
                      GS-PORTADOR
                      GS-DESCR-PORTADOR
                      GS-VENCTO-INI
                      GS-VENCTO-FIM
                      GS-TAXA-JURO
                      GS-PROTESTO
                      GS-CONTRATO
                      GS-ALBUM
                      GS-MOVTO-INI
                      GS-MOVTO-FIM
                      GS-CARTEIRA
                      GS-CARTEIRA-BCO
                      GS-QTDE-TITULO
                      GS-VALOR-TOTAL
                      GS-LINDET
                      GS-CONT
                      GS-TIPO-DOCTO
                      GS-ACP-TIPO
                      GS-ACP-PORTADOR
                      GS-DESC-PORTADOR
                      GS-ACP-SENHA
                      GS-FLAG-CRITICA

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ATUALIZA-SEQUENCIA SECTION.
           CLOSE      SEQBRAD
           OPEN I-O   SEQBRAD

           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBRAD.
           MOVE SEQUENCIA-W       TO SEQUENCIA
           MOVE NOSSO-NUMERO-W    TO NOSSO-NUMERO.
           REWRITE REG-SEQBRAD.
           CLOSE      SEQBRAD
           OPEN INPUT SEQBRAD.
       ABRE-ARQUIVO-ANOTACAO SECTION.
           OPEN I-O       CRD200
                          CRD201.

           IF ST-CRD200 = "35"
              CLOSE       CRD200
              OPEN OUTPUT CRD200
              CLOSE       CRD200
              OPEN I-O    CRD200.

           IF ST-CRD201 = "35"
              CLOSE       CRD201
              OPEN OUTPUT CRD201
              CLOSE       CRD201
              OPEN I-O    CRD201.

           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

       GRAVA-ANOTACAO SECTION.
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
           MOVE ZEROS           TO SITUACAO-ANOTACAO-CR200
           ADD 1                TO ULT-SEQ
           MOVE ULT-SEQ         TO SEQ-CR200
           MOVE COD-COMPL-CR20  TO COD-COMPL-CR200
           MOVE ZEROS           TO DATA-RETORNO-CR200
           MOVE USUARIO-W       TO USUARIO-CR200
           MOVE DATA-DIA-I      TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4)  TO HORA-MOVTO-CR200

           MOVE ZEROS           TO ST-CRD200
           PERFORM UNTIL ST-CRD200 = "10"
                WRITE REG-CRD200 INVALID KEY
                      ADD 1     TO SEQ-CR200
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200             TO SEQ-CR201
           MOVE COD-COMPL-CR20        TO COD-COMPL-CR201
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 99-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20         TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20         TO ANOTACAO-CR201(38: 4)
                                         PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT.
           MOVE NOME-PORT             TO ANOTACAO-CR201(43: 16)
           MOVE GS-ACP-PORTADOR       TO ANOTACAO-CR201(63: 4)
                                         PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT.
           MOVE NOME-PORT             TO ANOTACAO-CR201(67: 16)
           MOVE ZEROS                 TO ST-CRD201.
           MOVE 1                     TO SUBSEQ-CR201.

           PERFORM UNTIL ST-CRD201 = "10"
                WRITE REG-CRD201 INVALID KEY
                      ADD 1 TO SUBSEQ-CR201
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD201
                END-WRITE
           END-PERFORM.

       MOVER-DADOS-TIPO0 SECTION.
           MOVE 1                            TO SEQ-W.
           MOVE "0"                          TO ID-REG-REM.

           MOVE "1"                          TO T0-IDENTIFICACAO-ARQ-REM
           MOVE "Remessa"                    TO T0-LITERAL-REMESSA
           MOVE "01"                         TO T0-CODIGO-SERVICO
           MOVE "Cobrança"                   TO T0-LITERAL-SERVICO
           MOVE "00000000000004350734"       TO T0-CODIGO-EMPRESA
           MOVE "VIP FORMATURAS LTDA"        TO T0-NOME-EMPRESA
           MOVE "237"                        TO T0-NUMERO-BRADESCO-COMP
           MOVE "Bradesco"                   TO T0-NOME-BANCO-EXTENSO
           MOVE WS-DIA-CPU                   TO T0-DIA-GRAVACAO-ARQ
           MOVE WS-MES-CPU                   TO T0-MES-GRAVACAO-ARQ
           MOVE WS-ANO-CPU(3:2)              TO T0-ANO-GRAVACAO-ARQ
           MOVE "MX"                         TO T0-IDENTIFICACAO-SISTEMA
           MOVE SEQUENCIA-W                  TO T0-SEQUENCIAL-ARQUIVO
           MOVE 000001                       TO T0-N-SEQUENCIAL-REGISTRO

           MOVE REM-TIPO0                    TO DADOS-REM
           WRITE REG-REMESSA
           MOVE REG-REMESSA                  TO REG-REMESSA2
           MOVE X"0D0A"                      TO PULA-REM2
           WRITE REG-REMESSA2
           MOVE ZEROS                        TO VALOR-TOTAL
                                                QTDE-TIT

           ADD 1                             TO SEQUENCIA-W.

       MOVER-DADOS-TIPO1 SECTION.
           INITIALIZE REM-TIPO1
           MOVE "1"                TO ID-REG-REM

           MOVE ZEROS              TO T1-AGENCIA-DEBITO
           MOVE SPACES             TO T1-DIGITO-AGENCIA-DEBITO
           MOVE SPACES             TO T1-RAZAO-CONTA-CORRENTE
           MOVE ZEROS              TO T1-CONTA-CORRENTE
           MOVE SPACES             TO T1-DIGITO-CONTA-CORRENTE

           MOVE "0"                TO T1-IDENTIFICACAO-EMPRESA-CED(1:1)
           MOVE "009"              TO T1-IDENTIFICACAO-EMPRESA-CED(2:3)
           MOVE "3294"             TO T1-IDENTIFICACAO-EMPRESA-CED(5:4)
           MOVE "7271"             TO T1-IDENTIFICACAO-EMPRESA-CED(10:6)
           MOVE "0"                TO T1-IDENTIFICACAO-EMPRESA-CED(17:1)

           MOVE SPACES             TO T1-NUM-CONTROLE-PARTICIPANTE
           STRING COD-COMPL-CR20 SEQ-CR20 INTO
                                      T1-NUM-CONTROLE-PARTICIPANTE


           MOVE "237"              TO T1-COD-BANCO-DEB-COMP


           ADD 1                      TO NOSSO-NUMERO-W
           MOVE ZEROS TO TAB-SEQUENCIA
           STRING "09" NOSSO-NUMERO-W INTO TAB-SEQUENCIA

           MOVE 0                     TO IND
                                         TOTAL
           PERFORM UNTIL IND = 13
               ADD 1 TO IND
               EVALUATE IND
                   WHEN 1   COMPUTE VALOR = TAB-SEQ(IND) * 2
                   WHEN 2   COMPUTE VALOR = TAB-SEQ(IND) * 7
                   WHEN 3   COMPUTE VALOR = TAB-SEQ(IND) * 6
                   WHEN 4   COMPUTE VALOR = TAB-SEQ(IND) * 5
                   WHEN 5   COMPUTE VALOR = TAB-SEQ(IND) * 4
                   WHEN 6   COMPUTE VALOR = TAB-SEQ(IND) * 3
                   WHEN 7   COMPUTE VALOR = TAB-SEQ(IND) * 2
                   WHEN 8   COMPUTE VALOR = TAB-SEQ(IND) * 7
                   WHEN 9   COMPUTE VALOR = TAB-SEQ(IND) * 6
                   WHEN 10  COMPUTE VALOR = TAB-SEQ(IND) * 5
                   WHEN 11  COMPUTE VALOR = TAB-SEQ(IND) * 4
                   WHEN 12  COMPUTE VALOR = TAB-SEQ(IND) * 3
                   WHEN 13  COMPUTE VALOR = TAB-SEQ(IND) * 2
               END-EVALUATE
               COMPUTE TOTAL = TOTAL + VALOR
           END-PERFORM

           COMPUTE TOTAL2 = TOTAL / 11

           COMPUTE RESTO = TOTAL - (TOTAL2 * 11)

           EVALUATE RESTO
               WHEN 1      MOVE "P"   TO DIGITO-VERIFICADOR
               WHEN 0      MOVE "0"   TO DIGITO-VERIFICADOR
               WHEN OTHER  COMPUTE RESTO = 11 - RESTO
                           MOVE RESTO(2:1) TO DIGITO-VERIFICADOR
           END-EVALUATE

           STRING NOSSO-NUMERO-W DIGITO-VERIFICADOR
             INTO T1-IDENTIF-TITULO-BANCO

           MOVE ZEROS                 TO T1-DESCONTO-BONIF-DIA
           MOVE 2                     TO T1-CONDICAO-EMISSAO-PAPELETA
           MOVE "S"                   TO T1-IDENT-PAPELETA-DEB-AUT
           MOVE SPACES                TO T1-IDENTIF-OP-BANCO
           MOVE SPACES                TO T1-INDICADOR-RATEIO-CRED
           MOVE 2                     TO T1-ENDERECAMENTO-AVISO-DEB
           MOVE 01                    TO T1-IDENTIFICACAO-OCORRENC
           MOVE NR-DOCTO-CR20         TO T1-NUM-DOCUMENTO

           MOVE DATA-VENCTO-CR20(3:2) TO T1-ANO-VENCTO-TIT
           MOVE DATA-VENCTO-CR20(5:2) TO T1-MES-VENCTO-TIT
           MOVE DATA-VENCTO-CR20(7:2) TO T1-DIA-VENCTO-TIT

           MOVE VALOR-TOT-CR20        TO T1-VALOR-TITULO

           MOVE ZEROS                 TO T1-BANCO-ENCARREG-COB
           MOVE ZEROS                 TO T1-AGENCIA-DEPOSITARIA
           MOVE "01"                  TO T1-ESPECIE-TITULO
           MOVE "N"                   TO T1-IDENTIFICADO

           MOVE DATA-EMISSAO-CR20(1:2) TO T1-DIA-EMISSAO-TIT
           MOVE DATA-EMISSAO-CR20(3:2) TO T1-MES-EMISSAO-TIT
           MOVE DATA-EMISSAO-CR20(7:2) TO T1-ANO-EMISSAO-TIT
           MOVE "00"                   TO T1-INSTRUCAO1
           MOVE "00"                   TO T1-INSTRUCAO2

           COMPUTE VALOR-ATRASO ROUNDED =
                  (VALOR-TOT-CR20 * (GS-TAXA-JURO /100)) / 30

           MOVE VALOR-ATRASO           TO T1-VALOR-COB-DIA

      *    MOVE DATA-VENCTO-CR20(3:2) TO T1-ANO-LIMITE
      *    MOVE DATA-VENCTO-CR20(5:2) TO T1-MES-LIMITE
      *    MOVE DATA-VENCTO-CR20(7:2) TO T1-DIA-LIMITE

           MOVE ZEROS                 TO T1-ANO-LIMITE
           MOVE ZEROS                 TO T1-MES-LIMITE
           MOVE ZEROS                 TO T1-DIA-LIMITE


           MOVE ZEROS                 TO T1-VALOR-DESCONTO
           MOVE ZEROS                 TO T1-VALOR-IOF
           MOVE ZEROS                 TO T1-VALOR-ABATIMENTO

           MOVE COD-COMPL-CR20        TO COD-COMPL-CG10
                                         COD-COMPL-CG11
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010.

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011.

           MOVE COMPRADOR-CG10           TO T1-NOME-SACADO
           MOVE ENDERECO1-CG11           TO T1-ENDERECO-SACADO

      *    MOVE COD-COMPL-CR20        TO COD-COMPL-CG14
      *    READ CGD014 INVALID KEY
      *        MOVE ZEROS             TO T1-TIPO-INSC-SACADO
      *        MOVE ZEROS             TO T1-NUMERO-INSCRICAO-SACADO
      *    NOT INVALID KEY
               IF CPF1-CG11 > 100000000000
                  MOVE "02"                TO T1-TIPO-INSC-SACADO
                  MOVE CPF1-CG11           TO T1-NUMERO-INSCRICAO-SACADO
               ELSE
                  MOVE "01"                TO T1-TIPO-INSC-SACADO
                  MOVE CPF1-CG11           TO T1-NUMERO-INSCRICAO-SACADO
               END-IF
      *    END-READ
           MOVE ENDERECO1-CG11            TO T1-ENDERECO-SACADO
           MOVE CEP1-CG11                 TO T1-CEP-SACADO
           MOVE SPACES                    TO T1-SACADOR-AVALISTA
           ADD  1                         TO SEQUENCIA-REGISTRO
           MOVE SEQUENCIA-REGISTRO        TO T1-SEQUENCIAL-REGISTRO

           ADD  1                         TO SEQ-W

           MOVE REM-TIPO1                 TO DADOS-REM
           WRITE REG-REMESSA
           MOVE REG-REMESSA               TO REG-REMESSA2
           MOVE X"0D0A"                   TO PULA-REM2
           WRITE REG-REMESSA2.

       MOVER-DADOS-TIPO4 SECTION.
           MOVE "9"                       TO ID-REG-REM.
           MOVE SEQ-W                     TO T9-QUANTIDADE
           MOVE SPACES                    TO T9-BRANCO
           MOVE REM-TIPO9                 TO DADOS-REM.
           WRITE REG-REMESSA
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMESSA.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM
                         VALOR-TOTAL
                         QTDE-TIT
           PERFORM UNTIL ST-REM = "10"
               READ REMESSA AT END
                    MOVE "10" TO ST-REM
               NOT AT END
                    MOVE REG-REMESSA(1:1) TO TIPO-W
                    EVALUATE TIPO-W
                      WHEN 0  PERFORM CABECALHO-TIPO0
                              PERFORM LINDET-TIPO0
                              PERFORM CABECALHO-TIPO1
                      WHEN 1  PERFORM LINDET-TIPO1
                      WHEN 9  PERFORM CABECALHO-TIPO9
                              PERFORM LINDET-TIPO9
                    END-EVALUATE
               END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO GS-VALOR-TOTAL
           MOVE QTDE-TIT           TO GS-QTDE-TITULO
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.

       CARREGA-LISTA2 SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT SACADO
           MOVE SPACES TO GS-LINDET
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
               READ SACADO AT END
                    MOVE "10" TO ST-REM
               NOT AT END
                    MOVE DADOS-SACADO        TO GS-LINDET
                    MOVE "INSERE-LIST"       TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE SACADO.

       CABECALHO SECTION.
           MOVE DATA-DIA TO EMISSAO-REL.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
       IMPRIME-RELATORIO SECTION.
           ACCEPT VARIA-W  FROM TIME.
           OPEN OUTPUT WORK.
           OPEN INPUT REMESSA.
           MOVE ZEROS TO SEQ-WK.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
                READ REMESSA AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMESSA(1: 2) TO TIPO-W
                     IF TIPO-W <> 1
                        CONTINUE
                     ELSE
                        MOVE REG-REMESSA(2:399)     TO REM-TIPO1
                        ADD 1 TO SEQ-WK
                        MOVE T1-NOME-SACADO         TO NOME-WK
                        MOVE T1-ENDERECO-SACADO     TO ENDERECO-WK

                        MOVE T1-NUM-CONTROLE-PARTICIPANTE(1:9)
                          TO COD-COMPL-CG11
                        READ CGD011 INVALID KEY
                             MOVE "********"        TO CIDADE-WK
                             MOVE "**"              TO UF-WK
                        NOT INVALID KEY
                             MOVE CIDADE1-CG11      TO CIDADE
                             READ CAD010 INVALID KEY
                                  MOVE "*******"    TO CIDADE-WK
                                  MOVE "**"         TO UF-WK
                             NOT INVALID KEY
                                  MOVE NOME-CID     TO CIDADE-WK
                                  MOVE UF-CID       TO UF-WK
                             END-READ
                        END-READ
                        MOVE T1-CEP-SACADO          TO CEP-WK
                        MOVE T1-NUM-CONTROLE-PARTICIPANTE
                                                    TO DOCTO-WK
                        MOVE T1-VALOR-TITULO        TO VALOR-WK
                        WRITE REG-WORK
                        END-WRITE
                     END-IF
                END-READ
           END-PERFORM.

           COPY CONDENSA.

           CLOSE      WORK.
           OPEN INPUT WORK.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE NOME-WK           TO NOME-REL
                      MOVE ENDERECO-WK       TO ENDERECO-REL
                      MOVE CIDADE-WK         TO CIDADE-REL
                      MOVE UF-WK             TO UF-REL
                      MOVE CEP-WK            TO CEP-REL
                      MOVE DOCTO-WK          TO DOCTO-REL
                      MOVE VALOR-WK          TO VALOR-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL             TO VALOR-TOTAL-REL.
           MOVE QTDE-TIT                TO QTDE-TIT-TOTAL-REL.
           WRITE REG-RELAT FROM LINDET1 AFTER 3.
           CLOSE REMESSA WORK.
           DELETE FILE WORK.

           COPY DESCONDENSA.

      *---------------------------------------------------
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR     TO PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "*******"  TO NOME-PORT.

           MOVE NOME-PORT       TO GS-DESCR-PORTADOR.

       LER-PORTADOR2 SECTION.
           MOVE GS-ACP-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "*******"  TO NOME-PORT.

           MOVE NOME-PORT       TO GS-DESC-PORTADOR.

       POPUP-PORTADOR SECTION.
           CALL "CAP018T" USING PARAMETROS-W STRING-1
           CANCEL "CAP018T"
           MOVE STRING-1(1: 30) TO GS-DESCR-PORTADOR
           MOVE STRING-1(33: 4) TO GS-PORTADOR.

       POPUP-PORTADOR2 SECTION.
           CALL "CAP018T" USING PARAMETROS-W STRING-1
           CANCEL "CAP018T"
           MOVE STRING-1(1: 30) TO GS-DESC-PORTADOR
           MOVE STRING-1(33: 4) TO GS-ACP-PORTADOR.

      *----------------------------------------------------------
       CABECALHO-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "I"                 TO GS-LINDET(01:01)
           MOVE "R"                 TO GS-LINDET(02:01)
           MOVE "Literal"           TO GS-LINDET(03:07)
           MOVE "CD"                TO GS-LINDET(10:02)
           MOVE "Literal"           TO GS-LINDET(12:15)
           MOVE "Codigo Empresa"    TO GS-LINDET(27:20)
           MOVE "Nome Empresa"      TO GS-LINDET(47:30)
           MOVE "Com"               TO GS-LINDET(77:03)
           MOVE "Banco"             TO GS-LINDET(80:15)
           MOVE " Data "            TO GS-LINDET(95:06)
           MOVE "Branco"            TO GS-LINDET(101:08)
           MOVE "ID"                TO GS-LINDET(109:02)
           MOVE "Sequen"            TO GS-LINDET(111:07)
           MOVE "Branco"            TO GS-LINDET(118:277)
           MOVE "Sequen"            TO GS-LINDET(395:06)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO0 SECTION.
           MOVE REG-REMESSA(1: 400) TO GS-LINDET
           MOVE "INSERE-LIST"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO-TIPO1 SECTION.
           MOVE SPACES              TO GS-LINDET
           MOVE "INSERE-LIST"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "I"                 TO GS-LINDET(01:01)
           MOVE "Agencia"           TO GS-LINDET(02:06)
           MOVE "Razao"             TO GS-LINDET(08:05)
           MOVE "Conta"             TO GS-LINDET(13:08)
           MOVE "ID Empresa"        TO GS-LINDET(21:17)
           MOVE "N.Controle"        TO GS-LINDET(38:25)
           MOVE "Cod"               TO GS-LINDET(63:03)
           MOVE "Zeros"             TO GS-LINDET(66:05)
           MOVE "ID Banco"          TO GS-LINDET(71:12)
           MOVE "Desc. Dia"         TO GS-LINDET(83:10)
           MOVE "E"                 TO GS-LINDET(93:01)
           MOVE "N"                 TO GS-LINDEt(94:01)
           MOVE "Op. Banco"         TO GS-LINDET(95:10)
           MOVE "R"                 TO GS-LINDET(105:01)
           MOVE "D"                 TO GS-LINDET(106:01)
           MOVE "BA"                TO GS-LINDET(107:02)
           MOVE "IO"                TO GS-LINDET(109:02)
           MOVE "N.Docto"           TO GS-LINDET(111:10)
           MOVE "Vencto"            TO GS-LINDET(121:06)
           MOVE "Vlr.Titulo"        TO GS-LINDET(127:13)
           MOVE "BCO"               TO GS-LINDET(140:03)
           MOVE "AgDep"             TO GS-LINDET(143:05)
           MOVE "ES"                TO GS-LINDET(148:02)
           MOVE "I"                 TO GS-LINDET(150:01)
           MOVE "Dt.Emi"            TO GS-LINDET(151:06)
           MOVE "I1"                TO GS-LINDET(157:02)
           MOVE "I2"                TO GS-LINDET(159:02)
           MOVE "Vl.Atraso"         TO GS-LINDET(161:13)
           MOVE "Dt.Des"            TO GS-LINDET(174:06)
           MOVE "Vlr.Desc"          TO GS-LINDET(180:13)
           MOVE "Vlr.IOF"           TO GS-LINDET(193:13)
           MOVE "Vlr.Abat"          TO GS-LINDET(206:13)
           MOVE "IS"                TO GS-LINDET(219:02)
           MOVE "CPF/CNPJ"          TO GS-LINDET(221:14)
           MOVE "Nome Sacado"       TO GS-LINDET(235:40)
           MOVE "Endereço"          TO GS-LINDET(275:40)
           MOVE "Mensagem"          TO GS-LINDET(315:12)
           MOVE "CEP"               TO GS-LINDET(327:08)
           MOVE "Sacador Avalista"  TO GS-LINDET(335:60)
           MOVE "Sequencial"        TO GS-LINDET(395:06)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LINDET-TIPO1 SECTION.
           MOVE SPACES         TO GS-LINDET.
           MOVE REG-REMESSA    TO GS-LINDET
                                  REM-TIPO1

           ADD T1-VALOR-TITULO TO VALOR-TOTAL
           ADD 1               TO QTDE-TIT

           MOVE "INSERE-LIST"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CABECALHO-TIPO9 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "BRANCOS"     TO GS-LINDET(01: 394)
           MOVE "SEQUEN"      TO GS-LINDET(395: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO9 SECTION.
           MOVE SPACES              TO GS-LINDET.
           MOVE SPACES              TO GS-LINDET(1: 394)
           MOVE SEQ-W               TO GS-LINDET(395: 6).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO GS-EXIT-FLG.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP9106B"  TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
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
           move "CRP9106B"          to logacess-programa
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

           CLOSE CRD020 SEQBRAD CGD010 CGD011 CGD014 CAD010 CAD018.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
