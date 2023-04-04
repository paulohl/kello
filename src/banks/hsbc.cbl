       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HSBC.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO XXXXXXXX.REM P/ HSBC
       DATE-WRITTEN.  08-10-2010.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX020.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX200.
           COPY CRPX201.
           COPY BOLHSBC.SEL.
           COPY LOGACESS.SEL.

           select remes
                  assign       to   arquivo-remes
                  organization is      sequential
                  access mode  is      sequential
                  file status  is        fs-remes.

           SELECT AUXILIAR ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-AUXILIAR
                  RECORD KEY IS AUX-COD-COMPL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.

           SELECT SACADO ASSIGN TO ARQUIVO-SACADO
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-SACADO.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW200.
       COPY CRPW201.
       COPY BOLHSBC.FD.
       COPY LOGACESS.FD.

       fd remes label record is omitted.
       01 reg-remes.
           05  dados-remes      pic x(400).
           05  pula-remes       pic x(002).

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(40).
           05  ENDERECO-WK      PIC X(40).
           05  BAIRRO-WK        PIC X(12).
           05  CEP-WK           PIC 9(8).
           05  CIDADE-WK        PIC X(15).
           05  UF-WK            PIC XX.
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.

       FD  SACADO
           label record is omitted.
       01  REG-SACADO.
           05 CPF-SACADO        PIC 9(14).
           05 NOME-SACADO       PIC X(40).
           05 ENDERECO-SACADO   PIC X(38).
           05 CEP-SACADO        PIC X(08).
           05 BAIRRO-SACADO     PIC X(15).
           05 CIDADE-SACADO     PIC X(15).
           05 UF-SACADO         PIC X(02).
           05 PULA-SACADO       PIC X(02).

       FD  AUXILIAR.
       01  REG-AUXILIAR.
           05 AUX-COD-COMPL     PIC 9(09).

       FD  RELAT.
       01  REG-RELAT.
           05  FILLER          PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "HSBC.CPB".
           COPY "HSBC.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1                 PIC XX     VALUE SPACES.
           05 ST-CAD001              PIC XX     VALUE SPACES.
           05 ST-CAD010              PIC XX     VALUE SPACES.
           05 ST-CAD018              PIC XX     VALUE SPACES.
           05 ST-CRD020              PIC XX     VALUE SPACES.
           05 ST-CGD001              PIC XX     VALUE SPACES.
           05 ST-CGD010              PIC XX     VALUE SPACES.
           05 ST-CGD011              PIC XX     VALUE SPACES.
           05 ST-CRD200              PIC XX     VALUE SPACES.
           05 ST-CRD201              PIC XX     VALUE SPACES.
           05 ST-SACADO              PIC XX     VALUE SPACES.
           05 ST-AUXILIAR            PIC XX     VALUE SPACES.
           05 FS-LOGACESS            PIC XX     VALUE SPACES.
           05 FS-BOLHSBC             PIC XX     VALUE SPACES.
           05 ST-REM                 PIC XX     VALUE SPACES.
           05 ST-WORK                PIC XX     VALUE SPACES.
           05 FS-REMES               PIC XX     VALUE SPACES.
           05 VARIA-W                PIC 9(8)   VALUE ZEROS.
           05 VALOR-W                PIC 9(11)V99 VALUE ZEROS.
           05 SEQ-W                  PIC 9(6)   VALUE ZEROS.
           05 OPCAO                  PIC 9      VALUE ZEROS.
           05 VARIA-W2               PIC 9(8)   VALUE ZEROS.
           05 SEQUENCIA-W            PIC 9(10)     VALUE ZEROS.
           05 TIPO-W                 PIC 9      VALUE ZEROS.
           05 DATA-DIA               PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I             PIC 9(6)   VALUE ZEROS.
           05 HORA-BRA               PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ                PIC 9(5)   VALUE ZEROS.
           05 DATA-E                 PIC 99/99/99.
           05 VALOR-ATRASO           PIC 9(11)V99 VALUE ZEROS.
           05 CONF                   PIC X      VALUE SPACES.
           05 VALOR-TOTAL            PIC 9(12)V99 VALUE ZEROS.
           05 ERRO-W                 PIC 9        VALUE ZEROS.
           05 DATA-INV               PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV         PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV         PIC 9(8)     VALUE ZEROS.
           05 LIN                    PIC 9(02)    VALUE ZEROS.
           05 IND                    PIC 9(03)    VALUE ZEROS.
           05 TOTAL                  PIC 9(4)     VALUE ZEROS.
           05 VALOR                  PIC 9(4)     VALUE ZEROS.
           05 COD-COMPL-CR20-W       PIC 9(09)  VALUE ZEROS.
           05 RESTO                  PIC 9(4)     VALUE ZEROS.
           05 RESTO2                 PIC 9(4)     VALUE ZEROS.
           05 DIGITO-VERIFICADOR     PIC 9(1)     VALUE ZEROS.
           05 PASSAR-STRING-1        PIC X(65).
           05 MASC-VALOR             PIC ZZ9,99   VALUE ZEROS.
           05 MOVTO-INI              PIC 9(08)    VALUE ZEROS.
           05 MOVTO-FIM              PIC 9(08)    VALUE ZEROS.
           05 QTDE-TIT-T2            PIC 9(8).
           05 AUX-TIPO               PIC 9(1)     VALUE ZEROS.
           05 VALOR-TOT-TIT-T2       PIC 9(14).
           05 VALOR-MULTA            PIC 9(08)V99 VALUE ZEROS.
           05 ACHEI                  PIC X(01)    VALUE SPACES.
           05 MOVTO-INI-INV          PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV          PIC 9(8)     VALUE ZEROS.
           05 AUX-VENCTO             PIC 9(06)    VALUE ZEROS.
           05  ALB-BUM.
               10  CONTR             PIC 9(04).
               10  ALB               PIC 9(04).
           05  ALBUM-W REDEFINES ALB-BUM PIC 9(08).
           05 DATAW.
              10  DIA-W              PIC 99.
              10  MES-W              PIC 99.
              10  ANO-W              PIC 99.
           05 DATA-W REDEFINES DATAW PIC 9(6).
           05 DATAI.
              10  ANO-I              PIC 99.
              10  MES-I              PIC 99.
              10  DIA-I              PIC 99.
           05 DATA-I REDEFINES DATAI PIC 9(6).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".


       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       01 DETALHE-ARQUIVO.
          02 D-TIPO-ARQUIVO        PIC 9(001) VALUE 1.
          02 D-TIPO-INSCRICAO      PIC 9(002).
          02 D-INSC-EMPRESA        PIC 9(014).
          02 D-AGENCIA             PIC 9(005).
          02 D-SUB-CONTA           PIC 9(002).
          02 D-CONTRATO-COB        PIC X(011).
          02 FILLER                PIC X(027).
          02 D-NOSSO-NUMERO        PIC X(011).
          02 D-DATA-DESCONTO-2     PIC 9(006).
          02 D-VALOR-DESCONTO-2    PIC 9(009)V99.
          02 D-DATA-DESCONTO-3     PIC 9(006).
          02 D-VALOR-DESCONTO-3    PIC 9(009)V99.
          02 D-CARTEIRA            PIC 9(001).
          02 D-OCORRENCIA          PIC 9(002).
          02 D-USO-EMPRESA         PIC X(010).
          02 D-VENCIMENTO          PIC 9(006).
          02 D-VALOR-TITULO        PIC 9(011)V99.
          02 D-BANCO               PIC 9(003).
          02 D-AGENCIA-DEPOSITARIA PIC 9(005).
          02 D-ESPECIE             PIC X(002).
          02 D-ACEITE              PIC X(001).
          02 D-EMISSAO             PIC 9(006).
          02 D-INSTRUCAO1          PIC 9(002).
          02 D-INSTRUCAO2          PIC 9(002).
          02 D-JUROS-MORA          PIC 9(011)V99.
          02 D-DATA-DESCONTO       PIC 9(006).
          02 D-VALOR-DESCONTO      PIC 9(011)V99.
          02 D-VALOR-IOF           PIC 9(011)V99.
          02 D-ABATIMENTO          PIC X(013).
          02 D-IDENTIF-SACADO      PIC 9(002).
          02 D-INSC-SACADO         PIC 9(014).
          02 D-NOME-SACADO         PIC X(040).
          02 D-ENDERECO-SACADO     PIC X(038).
          02 D-DIAS                PIC X(002).
          02 D-BAIRRO-SACADO       PIC X(012).
          02 D-CEP-SACADO          PIC 9(008).
          02 D-CIDADE-SACADO       PIC X(015).
          02 D-UF-SACADO           PIC X(002).
          02 D-SACADO-AVALISTA     PIC X(040).
          02 D-PRAZO               PIC 9(002).
          02 D-TIPO-MOEDA          PIC 9(001).
          02 D-NUMERO-REMESSA      PIC 9(006).

       77 ws-int                   pic 9(02).
       01 ws-data-desconto.
          05 desconto-ano          pic 9(04).
          05 desconto-mes          pic 9(02).
          05 desconto-dia          pic 9(02).

       01 tabela-dias              pic x(024)
                                   value "312831303130313130313031".
       01 tab-mes                  pic 9(002) occurs 12 times
                                   redefines tabela-dias.

       01  TAB-SEQUENCIA            PIC 9(11).
       01  FILLER REDEFINES TAB-SEQUENCIA OCCURS 11 TIMES.
           05 TAB-SEQ               PIC 9(01).

       01  linka-cpf.
           05  link-cpf             pic 9(11).
           05  link-cpf-r redefines link-cpf.
               10  link-cpf-num     pic 9(09).
               10  link-cpf-dig1    pic 9(01).
               10  link-cpf-dig2    pic 9(01).
           05  link-cpf-conf        pic 9(01).

       01 OK                        PIC X(01).

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                     pic x(200).
       01 tipo-msg                     pic x(01).
       01 resp-msg                     pic x(01).

       01  CAB01.
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BANESTADO'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                           ENDERECO
      -    "    BAIRRO       CEP       CIDADE          UF DOCUMENTO
      -    "     VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  BAIRRO-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.
           05  FILLER               PIC X     VALUE ".".
           05  SUFIXO-REL           PIC ZZZ.
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


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

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
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "BOLHSBC"  TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-BOLHSBC
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CAD010 CAD018 CAD001 CGD001
           OPEN I-O   BOLHSBC
           CLOSE      BOLHSBC
           OPEN I-O   BOLHSBC CRD020
           CLOSE      CRD020
           OPEN INPUT CRD020

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "HSBC"              to logacess-programa
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
                   MOVE 1 TO GS-CEP GS-CPF GS-ENDERECO
                   REFRESH-OBJECT PRINCIPAL
                   PERFORM LER
               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-REMESSA-TRUE
                    IF GS-OPCAO = 1
                       PERFORM GERAR-SACADO
                    ELSE
                       PERFORM GERA-ARQ-REMESSA
                    END-IF
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-ATUALIZA-PORTADOR-TRUE
                    PERFORM ATUALIZA-PORTADOR-RECEBER
               WHEN GS-ATUALIZ-SEQUENCIA-TRUE
                    PERFORM ATUALIZA-SEQUENCIA
               WHEN GS-GRAVAR-TRUE
                    PERFORM GRAVAR
               WHEN GS-LE-PARAM-TRUE
                    PERFORM LER
               WHEN GS-CRITICAR-TRUE
                    PERFORM CRITICAR
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR        TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "****"        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-DESC-VENDEDOR.

       POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-VENDEDOR.

       CRITICAR SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-PORTADOR"  PERFORM CRITICAR-PORTADOR
           END-EVALUATE.

       CRITICAR-PORTADOR SECTION.
           IF GS-ACP-PORTADOR = ZEROS
              CALL   "CAP018T" USING PARAMETROS-W POP-UP
              CANCEL "CAP018T"
              MOVE POP-UP(1: 30)            TO GS-DESC-PORTADOR
              MOVE POP-UP(33: 4)            TO GS-ACP-PORTADOR
           END-IF
           IF GS-ACP-PORTADOR = ZEROS
              MOVE "Portador Não Informado" TO MENSAGEM
              MOVE "C"                      TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE GS-ACP-PORTADOR          TO PORTADOR
              READ CAD018 INVALID KEY
                   MOVE "*******"           TO NOME-PORT
              END-READ
              MOVE NOME-PORT                TO GS-DESC-PORTADOR
              REFRESH-OBJECT WIN1.


       LER SECTION.
           MOVE 1                       TO BOLHSBC-CODIGO
           READ BOLHSBC INVALID KEY
                INITIALIZE REG-BOLHSBC.

           PERFORM MOVER-DADOS.

       MOVER-DADOS SECTION.
           MOVE BOLHSBC-NUM-BANCO       TO GS-ACP-I-CODBAN
           MOVE BOLHSBC-NOME-BANCO      TO GS-ACP-I-NOMEBAN
           MOVE BOLHSBC-AGENCIA         TO GS-ACP-I-AGENCIA
           MOVE BOLHSBC-DIG-AGENCIA     TO GS-ACP-I-DIG-AGENCIA
           MOVE BOLHSBC-CONTA           TO GS-ACP-I-CONTA
           MOVE BOLHSBC-DIG-CONTA       TO GS-ACP-I-DIG-CONTA
           MOVE BOLHSBC-CNPJ            TO GS-ACP-I-CNPJ
           MOVE BOLHSBC-cod-cliente     TO GS-ACP-I-CODIGO-CLIENTE
           MOVE BOLHSBC-NOSSO-NUM       TO GS-ACP-I-NOSSO-NUMERO
           MOVE BOLHSBC-QTDE-DIAS       TO GS-ACP-I-DIAS-INSTRUCAO
           MOVE BOLHSBC-JUROS-1-DIA     TO GS-ACP-I-JUROS-1-DIA
           MOVE BOLHSBC-DESCONTO        TO GS-ACP-I-DESCONTO
           MOVE BOLHSBC-MULTA           TO GS-ACP-I-MULTA
           MOVE BOLHSBC-DIAS-DESCONTO   TO GS-ACP-I-DIAS-DESCONTO

           INITIALIZE GS-ACP-I-CARTEIRA
           EVALUATE BOLHSBC-CARTEIRA
              WHEN 102
              MOVE "102 - I - SEM REGISTRO COM EMISSÃO INTEGRAL CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 103
              MOVE "103 - I - SEM REGISTRO COM EMISSÃO E ENTREGA CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 104
              MOVE "104 - I - ESCRITURAL ELETRÔNICA CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 107
              MOVE "107 - I - SEM REGISTRO COM EMISSÃO INTEGRAL 15 POSIÇ
      -            "ÕES - CARNÊ" TO GS-ACP-I-CARTEIRA
              WHEN 108
              MOVE "108 - I - DIRETA ELETRÔNICA EMISSÃO INTEGRAL CARNÊ"
                TO GS-ACP-I-CARTEIRA
              WHEN 109
              MOVE "109 - I - DIRETA ELETRÔNICA SEM EMISSÃO SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 112
              MOVE "112 - I - ESCRITURAL ELETRÔNICA SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 121
              MOVE "121 - I - DIRETA ELETRÔNICA EMISSÃO PARCIAL SIMPLES"
                TO GS-ACP-I-CARTEIRA
              WHEN 129
              MOVE "129 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 2%" TO GS-ACP-I-CARTEIRA
              WHEN 139
              MOVE "139 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 4%" TO GS-ACP-I-CARTEIRA
              WHEN 142
              MOVE "142 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS IOF 4%
      -            " " TO GS-ACP-I-CARTEIRA
              WHEN 143
              MOVE "143 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS COM IO
      -            "F 7%" TO GS-ACP-I-CARTEIRA
              WHEN 147
              MOVE "147 - E - ESCRITURAL ELETRÔNICA DÓLAR"
                TO GS-ACP-I-CARTEIRA
              WHEN 150
              MOVE "150 - U - DIRETA ELETRÔNICA SEM EMISSÃO DÓLAR"
                TO GS-ACP-I-CARTEIRA
              WHEN 169
              MOVE "169 - I - SEM REGISTRO EMISSÃO PARCIAL SEGUROS COM I
      -            "OF 7%" TO GS-ACP-I-CARTEIRA
              WHEN 172
              MOVE "172 - I - SEM REGISTRO COM EMISSÃO INTEGRAL"
                TO GS-ACP-I-CARTEIRA
              WHEN 173
              MOVE "173 - I - SEM REGISTRO COM EMISSÃO E ENTREGA"
                TO GS-ACP-I-CARTEIRA
              WHEN 174
              MOVE " 174 - I - SEM REGISTRO EMISSÃO PARCIAL COM PROTESTO
      -            " BORDERÔ" TO GS-ACP-I-CARTEIRA
              WHEN 175
              MOVE "175 - I - SEM REGISTRO SEM EMISSÃO COM PROTESTO ELET
      -            "RÔNICO" TO GS-ACP-I-CARTEIRA
              WHEN 177
              MOVE "177 - I - SEM REGISTRO EMISSÃO PARCIAL COM PROTESTO
      -            "ELETRÔNICO" TO GS-ACP-I-CARTEIRA
              WHEN 180
              MOVE "180 - I - DIRETA ELETRÔNICA EMISSÃO INTEGRAL SIMPLES
      -            " " TO GS-ACP-I-CARTEIRA
              WHEN 188
              MOVE "188 - I - ESCRITURAL ELETRÔNICA - COBRANÇA INTELIGEN
      -            "TE (B2B)" TO GS-ACP-I-CARTEIRA
              WHEN 196
              MOVE "196 - I - SEM REGISTRO COM EMISSÃO E ENTREGA 15 POSI
      -            "ÇÕES" TO GS-ACP-I-CARTEIRA
              WHEN 198
              MOVE "198 - I - SEM REGISTRO SEM EMISSÃO 15 DÍGITOS"
                TO GS-ACP-I-CARTEIRA
           END-EVALUATE

           INITIALIZE GS-ACP-I-ESPECIE
           EVALUATE BOLHSBC-ESPECIE
              when "01"
                 MOVE "01 - DUPLICATA MERCANTIL"   TO GS-ACP-I-ESPECIE
              when "02"
                 MOVE "02 - NOTA PROMISSORIA"      TO GS-ACP-I-ESPECIE
              when "03"
                 MOVE "03 - NOTA DE SEGURO"        TO GS-ACP-I-ESPECIE
              when "04"
                 MOVE "04 - MENSALIDADE ESCOLAR"   TO GS-ACP-I-ESPECIE
              when "05"
                 MOVE "05 - RECIBO"                TO GS-ACP-I-ESPECIE
              when "06"
                 MOVE "06 - CONTRATO"              TO GS-ACP-I-ESPECIE
              when "07"
                 MOVE "07 - COSSEGUROS"            TO GS-ACP-I-ESPECIE
              when "08"
                 MOVE "08 - DUPLICATA DE SERVIÇOS" TO GS-ACP-I-ESPECIE
              when "09"
                 MOVE "09 - LETRA DE CÂMBIO"       TO GS-ACP-I-ESPECIE
              when "13"
                 MOVE "13 - NOTA DE DÉBITOS"       TO GS-ACP-I-ESPECIE
              when "15"
                 MOVE "15 - DOCUMENTO DE DÍVIDA"   TO GS-ACP-I-ESPECIE
              when "99"
                 MOVE "99 - DIVERSOS"              TO GS-ACP-I-ESPECIE
           end-evaluate

           INITIALIZE GS-ACP-I-INSTUCAO-01
           evaluate bolhsbc-instrucao-1
               WHEN 15
              MOVE "15 - Multa de .... por cento após dia .../.../....."
                TO GS-ACP-I-INSTUCAO-01
               WHEN 16
              MOVE "16 - Após .../.... multa dia de ....... máximo .....
      -            "..." TO GS-ACP-I-INSTUCAO-01
               WHEN 19
              MOVE "19 - Multa de R$ _____ após _____ dias corridos do v
      -            "encimento" TO GS-ACP-I-INSTUCAO-01
               WHEN 20
              MOVE "20 - Cobrar juros só após 07 dias do vencimento"
                TO GS-ACP-I-INSTUCAO-01
               WHEN 22
              MOVE "22 - Multa de R$ _______ após _____ dias úteis do ve
      -            "ncimento" TO GS-ACP-I-INSTUCAO-01
               WHEN 23
              MOVE "23 - Não receber após o vencimento" TO
                                 GS-ACP-I-INSTUCAO-01
               WHEN 24
              MOVE "24 - Multa de R$ ____ após o vencimento" TO
                                 GS-ACP-I-INSTUCAO-01
               WHEN 29
              MOVE "29 - Juros só após .../.../....., cobrar desde o ven
      -            "cimento" TO GS-ACP-I-INSTUCAO-01
               WHEN 34
              MOVE "34 - Conceder abatimento conforme proposto pelo saca
      -            "do" TO GS-ACP-I-INSTUCAO-01
               WHEN 36
              MOVE "36 - Após vencimento multa de 10 por cento"
                        TO GS-ACP-I-INSTUCAO-01
               WHEN 40
              MOVE "40 - Conceder desconto mesmo se pago após o vencimen
      -            "to" TO GS-ACP-I-INSTUCAO-01
               WHEN 42
              MOVE "42 - Não receber antes do vencimento - instrução do
      -            "cedente" TO GS-ACP-I-INSTUCAO-01
               WHEN 53
              MOVE "53 - Após vencimento multa de 20 % mais mora de 1 %
      -            "ao mês" TO GS-ACP-I-INSTUCAO-01
               WHEN 56
              MOVE "56 - Não receber antes do vencimento ou 10 dias após
      -            "" TO GS-ACP-I-INSTUCAO-01
               WHEN 65
              MOVE "65 - Abatimento/desconto só com instrução do cedente
      -            "" TO GS-ACP-I-INSTUCAO-01
               WHEN 67
              MOVE "67 - Título sujeito a protesto após o vencimento"
                      TO GS-ACP-I-INSTUCAO-01
               WHEN 68
              MOVE "68 - Após o vencimento multa de 2 por cento"
                      TO GS-ACP-I-INSTUCAO-01
               WHEN 71
              MOVE "71 - Não receber após .... dias corridos do vencimen
      -            "to" TO GS-ACP-I-INSTUCAO-01
               WHEN 72
              MOVE "72 - Não receber após .... dias úteis do vencimento"
                        TO GS-ACP-I-INSTUCAO-01
               WHEN 73
              MOVE "73 - Multa de .... por cento após .... dias corridos
      -            " do vencimento" TO GS-ACP-I-INSTUCAO-01
               WHEN 74
              MOVE "74 - Multa de .... por cento após .... dias úteis do
      -            " vencimento" TO GS-ACP-I-INSTUCAO-01
               WHEN 75
              MOVE "75 - Protestar .... dias corridos após o vencimento
      -            "se não pago" TO GS-ACP-I-INSTUCAO-01
               WHEN 76
              MOVE "76 - Protestar .... dias úteis após o vencimento se
      -            "não pago"    TO GS-ACP-I-INSTUCAO-01
               WHEN 77
              MOVE "77 - Protestar .... dias úteis após o vencimento se
      -            "não pago"    TO GS-ACP-I-INSTUCAO-01
               WHEN 84
              MOVE "84 - Protestar .... dias corridos após o vencimento,
      -            " se não pago" TO GS-ACP-I-INSTUCAO-01
           END-EVALUATE

           INITIALIZE GS-ACP-I-INSTRUCAO-02
           evaluate bolhsbc-instrucao-2
               WHEN 15
              MOVE "15 - Multa de .... por cento após dia .../.../....."
                TO GS-ACP-I-INSTRUCAO-02
               WHEN 16
              MOVE "16 - Após .../.... multa dia de ....... máximo .....
      -            "..." TO GS-ACP-I-INSTRUCAO-02
               WHEN 19
              MOVE "19 - Multa de R$ _____ após _____ dias corridos do v
      -            "encimento" TO GS-ACP-I-INSTRUCAO-02
               WHEN 20
              MOVE "20 - Cobrar juros só após 07 dias do vencimento"
                TO GS-ACP-I-INSTRUCAO-02
               WHEN 22
              MOVE "22 - Multa de R$ _______ após _____ dias úteis do ve
      -            "ncimento" TO GS-ACP-I-INSTRUCAO-02
               WHEN 23
              MOVE "23 - Não receber após o vencimento" TO
                                 GS-ACP-I-INSTRUCAO-02
               WHEN 24
              MOVE "24 - Multa de R$ ____ após o vencimento" TO
                                 GS-ACP-I-INSTRUCAO-02
               WHEN 29
              MOVE "29 - Juros só após .../.../....., cobrar desde o ven
      -            "cimento" TO GS-ACP-I-INSTRUCAO-02
               WHEN 34
              MOVE "34 - Conceder abatimento conforme proposto pelo saca
      -            "do" TO GS-ACP-I-INSTRUCAO-02
               WHEN 36
              MOVE "36 - Após vencimento multa de 10 por cento"
                        TO GS-ACP-I-INSTRUCAO-02
               WHEN 40
              MOVE "40 - Conceder desconto mesmo se pago após o vencimen
      -            "to" TO GS-ACP-I-INSTRUCAO-02
               WHEN 42
              MOVE "42 - Não receber antes do vencimento - instrução do
      -            "cedente" TO GS-ACP-I-INSTRUCAO-02
               WHEN 53
              MOVE "53 - Após vencimento multa de 20 % mais mora de 1 %
      -            "ao mês" TO GS-ACP-I-INSTRUCAO-02
               WHEN 56
              MOVE "56 - Não receber antes do vencimento ou 10 dias após
      -            "" TO GS-ACP-I-INSTRUCAO-02
               WHEN 65
              MOVE "65 - Abatimento/desconto só com instrução do cedente
      -            "" TO GS-ACP-I-INSTRUCAO-02
               WHEN 67
              MOVE "67 - Título sujeito a protesto após o vencimento"
                      TO GS-ACP-I-INSTRUCAO-02
               WHEN 68
              MOVE "68 - Após o vencimento multa de 2 por cento"
                      TO GS-ACP-I-INSTRUCAO-02
               WHEN 71
              MOVE "71 - Não receber após .... dias corridos do vencimen
      -            "to" TO GS-ACP-I-INSTRUCAO-02
               WHEN 72
              MOVE "72 - Não receber após .... dias úteis do vencimento"
                        TO GS-ACP-I-INSTRUCAO-02
               WHEN 73
              MOVE "73 - Multa de .... por cento após .... dias corridos
      -            " do vencimento" TO GS-ACP-I-INSTRUCAO-02
               WHEN 74
              MOVE "74 - Multa de .... por cento após .... dias úteis do
      -            " vencimento" TO GS-ACP-I-INSTRUCAO-02
               WHEN 75
              MOVE "75 - Protestar .... dias corridos após o vencimento
      -            "se não pago" TO GS-ACP-I-INSTRUCAO-02
               WHEN 76
              MOVE "76 - Protestar .... dias úteis após o vencimento se
      -            "não pago"    TO GS-ACP-I-INSTRUCAO-02
               WHEN 77
              MOVE "77 - Protestar .... dias úteis após o vencimento se
      -            "não pago"    TO GS-ACP-I-INSTRUCAO-02
               WHEN 84
              MOVE "84 - Protestar .... dias corridos após o vencimento,
      -            " se não pago" TO GS-ACP-I-INSTRUCAO-02
           end-evaluate

           MOVE BOLHSBC-INSTRUCAO1      TO GS-ACP-I-INSTRUCAO1
           MOVE BOLHSBC-INSTRUCAO2      TO GS-ACP-I-INSTRUCAO2
           MOVE BOLHSBC-INSTRUCAO3      TO GS-ACP-I-INSTRUCAO3
           MOVE BOLHSBC-INSTRUCAO4      TO GS-ACP-I-INSTRUCAO4
           MOVE BOLHSBC-INSTRUCAO5      TO GS-ACP-I-INSTRUCAO5
                                                                                                                                                                                                                                                              *

           MOVE BOLHSBC-LOCAL1          TO GS-ACP-I-LOCAL1
           MOVE BOLHSBC-LOCAL2          TO GS-ACP-I-LOCAL2

           MOVE BOLHSBC-PORTADOR        TO GS-ACP-PORTADOR
                                           PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"          TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO GS-DESC-PORTADOR
           MOVE BOLHSBC-CONVENIO        TO GS-CONVENIO

           REFRESH-OBJECT WIN1.


       GRAVAR SECTION.
           INITIALIZE REG-BOLHSBC

           MOVE 1                       TO BOLHSBC-CODIGO
           MOVE GS-ACP-I-CODBAN         TO BOLHSBC-NUM-BANCO
           MOVE GS-ACP-I-NOMEBAN        TO BOLHSBC-NOME-BANCO
           MOVE GS-ACP-I-AGENCIA        TO BOLHSBC-AGENCIA
           MOVE GS-ACP-I-DIG-AGENCIA    TO BOLHSBC-DIG-AGENCIA
           MOVE GS-ACP-I-CONTA          TO BOLHSBC-CONTA
           MOVE GS-ACP-I-DIG-CONTA      TO BOLHSBC-DIG-CONTA
           MOVE SPACES                  TO BOLHSBC-EMPRESA
           MOVE GS-ACP-I-CNPJ           TO BOLHSBC-CNPJ
           MOVE GS-ACP-I-CODIGO-CLIENTE TO BOLHSBC-cod-cliente
           MOVE GS-ACP-I-NOSSO-NUMERO   TO BOLHSBC-NOSSO-NUM
           MOVE GS-ACP-I-DIAS-INSTRUCAO TO BOLHSBC-QTDE-DIAS
           MOVE GS-ACP-I-JUROS-1-DIA    TO BOLHSBC-JUROS-1-DIA
           MOVE GS-ACP-I-DESCONTO       TO BOLHSBC-DESCONTO
           MOVE GS-ACP-I-MULTA          TO BOLHSBC-MULTA
           MOVE GS-ACP-I-DIAS-DESCONTO  TO BOLHSBC-DIAS-DESCONTO

           MOVE GS-ACP-I-CARTEIRA(1:3)  TO BOLHSBC-CARTEIRA
           MOVE GS-ACP-I-CARTEIRA(7:1)  TO BOLHSBC-COD-CARTEIRA

           MOVE GS-ACP-I-ESPECIE(1:2)   TO BOLHSBC-ESPECIE

           MOVE GS-ACP-I-INSTUCAO-01(1:2)  TO BOLHSBC-INSTRUCAO-1
           MOVE GS-ACP-I-INSTRUCAO-02(1:2) TO BOLHSBC-INSTRUCAO-2

           MOVE GS-ACP-I-INSTRUCAO1      TO BOLHSBC-INSTRUCAO1
           MOVE GS-ACP-I-INSTRUCAO2      TO BOLHSBC-INSTRUCAO2
           MOVE GS-ACP-I-INSTRUCAO3      TO BOLHSBC-INSTRUCAO3
           MOVE GS-ACP-I-INSTRUCAO4      TO BOLHSBC-INSTRUCAO4
           MOVE GS-ACP-I-INSTRUCAO5      TO BOLHSBC-INSTRUCAO5

           MOVE GS-ACP-I-LOCAL1          TO BOLHSBC-LOCAL1
           MOVE GS-ACP-I-LOCAL2          TO BOLHSBC-LOCAL2

           MOVE GS-ACP-PORTADOR          TO BOLHSBC-PORTADOR
           MOVE GS-CONVENIO              TO BOLHSBC-CONVENIO
           WRITE REG-BOLHSBC INVALID KEY
                 REWRITE REG-BOLHSBC INVALID KEY
                      MOVE "Erro de Regravação...BOLHSBC" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERAR-SACADO SECTION.
           ACCEPT VARIA-W2 FROM TIME.
           ADD  50 TO VARIA-W2

           OPEN OUTPUT AUXILIAR
           CLOSE       AUXILIAR
           OPEN I-O    AUXILIAR

           MOVE FUNCTION NUMVAL(GS-TIPO-DOCTO(1:1)) TO AUX-TIPO


           MOVE "CLEAR-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-MOVTO-INI      TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI
           MOVE GS-MOVTO-FIM      TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-FIM


           MOVE GS-VENCTO-INI     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV
           MOVE GS-VENCTO-FIM     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-FIM-INV

           MOVE 1 TO BOLHSBC-CODIGO
           READ BOLHSBC INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SPACES              TO ARQUIVO-SACADO
                STRING "\PROGRAMA\KELLO\TEMP\SACADO-"
                        GS-NOME-ARQ-REMESSA INTO ARQUIVO-SACADO
                OPEN OUTPUT SACADO

                INITIALIZE REG-CRD020
                MOVE VENCTO-INI-INV      TO DATA-VENCTO-CR20
                MOVE ZEROS               TO SITUACAO-CR20
                                            COD-COMPL-CR20
                START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                      MOVE "10" TO ST-CRD020
                END-START
                PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT RECORD AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF SITUACAO-CR20 > 0 OR
                             DATA-VENCTO-CR20 > VENCTO-FIM-INV
                             MOVE "10" TO ST-CRD020
                          ELSE
                             MOVE REG-CRD020 TO GS-REGISTRO1
                             IF PORTADOR-CR20 = GS-PORTADOR
                                IF GS-CONTRATO = 0 OR CLIENTE-CR20(1:4)
                                   IF GS-ALBUM = 0 OR CLIENTE-CR20(5:4)
                                      IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                                        PERFORM FAZER-OUTRAS-COMP2
                                      END-IF
                                   END-IF
                                END-IF
                             END-IF
                             MOVE "REFRESH-DATA" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM
                          END-IF
                     END-READ
                END-PERFORM
                MOVE ZEROS          TO GS-SEQ
                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                CLOSE SACADO AUXILIAR
                OPEN INPUT   AUXILIAR

                INITIALIZE REG-AUXILIAR
                START AUXILIAR KEY IS NOT LESS AUX-COD-COMPL INVALID KEY
                      MOVE "10" TO ST-AUXILIAR
                END-START
                PERFORM UNTIL ST-AUXILIAR = "10"
                      READ AUXILIAR NEXT AT END
                           MOVE "10" TO ST-AUXILIAR
                      NOT AT END
                           MOVE AUX-COD-COMPL  TO COD-COMPL-CG10
                           READ CGD010 INVALID KEY
                                INITIALIZE REG-CGD010
                           END-READ
                           MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                           READ CGD011 INVALID KEY
                                INITIALIZE REG-CGD011
                           END-READ
                           MOVE COMPRADOR-CG10 TO NOME-SACADO
                           MOVE CPF1-CG11      TO CPF-SACADO
                           MOVE ENDERECO1-CG11 TO ENDERECO-SACADO
                           MOVE CEP1-CG11      TO CEP-SACADO
                           MOVE BAIRRO1-CG11   TO BAIRRO-SACADO
                           MOVE CIDADE1-CG11   TO CIDADE
                           READ CAD010 INVALID KEY
                                INITIALIZE REG-CAD010
                           END-READ
                           MOVE NOME-CID       TO CIDADE-SACADO
                           MOVE UF-CID         TO UF-SACADO
                           MOVE X"0D0A"        TO PULA-SACADO

                           WRITE REG-SACADO

                      END-READ
                END-PERFORM
                CLOSE AUXILIAR SACADO
                OPEN INPUT     SACADO

                MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM

                INITIALIZE REG-SACADO
                PERFORM UNTIL ST-SACADO = "10"
                    READ SACADO NEXT AT END
                         MOVE "10" TO ST-SACADO
                    NOT AT END
                         MOVE REG-SACADO       TO DETALHE-ARQUIVO
                         MOVE D-NUMERO-REMESSA TO GS-SEQ
                         MOVE "REFRESH-DATA"   TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                         MOVE REG-SACADO       TO GS-LINDET
                         MOVE "INSERE-LIST"    TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM

                    END-READ
                END-PERFORM
                REFRESH-OBJECT PRINCIPAL.

       GERA-ARQ-REMESSA SECTION.
           MOVE FUNCTION NUMVAL(GS-TIPO-DOCTO(1:1)) TO AUX-TIPO


           MOVE "CLEAR-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-MOVTO-INI      TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI
           MOVE GS-MOVTO-FIM      TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-FIM


           MOVE GS-VENCTO-INI     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV
           MOVE GS-VENCTO-FIM     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-FIM-INV

           MOVE GS-MOVTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI-INV.
           MOVE GS-MOVTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO MOVTO-FIM-INV.

           MOVE 1 TO BOLHSBC-CODIGO
           READ BOLHSBC INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM MOVER-DADOS
                MOVE BOLHSBC-NOSSO-NUM   TO SEQUENCIA-W

                MOVE SPACES              TO ARQUIVO-REMES
                STRING "\PROGRAMA\KELLO\TEMP\" GS-NOME-ARQ-REMESSA
                  INTO ARQUIVO-REMES
                OPEN OUTPUT REMES



                INITIALIZE GS-QTDE-TITULO
                           GS-VALOR-TOTAL
                           REG-CRD020
                MOVE ZEROS TO VALOR-TOTAL SEQ-W
                IF GS-ALBUM > 0
                   STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                   MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
                   START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                        MOVE "10" TO ST-CRD020
                   END-START

                   PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT AT END
                            MOVE "10" TO ST-CRD020
                        NOT AT END
                            IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                               MOVE "10" TO ST-CRD020
                            ELSE
                                MOVE REG-CRD020 TO GS-REGISTRO1
                                IF SITUACAO-CR20 = 0
                                   IF MOVTO-INI-INV = 0 OR
                                     (DATA-MOVTO-CR20 NOT <
                                      MOVTO-INI-INV AND
                                      DATA-MOVTO-CR20 NOT >
                                      MOVTO-FIM-INV)
                                      IF VENCTO-INI-INV = 0 OR
                                         (DATA-VENCTO-CR20 NOT <
                                          VENCTO-INI-INV AND
                                          DATA-VENCTO-CR20 NOT >
                                          VENCTO-FIM-INV)
                                         PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                      END-IF
                                   END-IF
                                END-IF
                                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM
                            END-IF
                        END-READ
                   END-PERFORM
                ELSE
                   IF GS-CONTRATO > 0
                      STRING "0" GS-CONTRATO GS-ALBUM INTO
                                                          COD-COMPL-CR20
                      START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID
                                                                     KEY
                           MOVE "10" TO ST-CRD020
                      END-START
                      PERFORM UNTIL ST-CRD020 = "10"
                          READ CRD020 NEXT AT END
                               MOVE "10" TO ST-CRD020
                          NOT AT END
                               IF CLIENTE-CR20(1:4) <> GS-CONTRATO
                                  MOVE "10" TO ST-CRD020
                               ELSE
                                  MOVE REG-CRD020 TO GS-REGISTRO1
                                  IF SITUACAO-CR20 = 0
                                     IF MOVTO-INI-INV = 0 OR
                                        (DATA-MOVTO-CR20 NOT <
                                         MOVTO-INI-INV AND
                                         DATA-MOVTO-CR20 NOT >
                                         MOVTO-FIM-INV)
                                         IF VENCTO-INI-INV = 0 OR
                                            (DATA-VENCTO-CR20 NOT <
                                             VENCTO-INI-INV AND
                                             DATA-VENCTO-CR20 NOT >
                                             VENCTO-FIM-INV)
                                             PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                          END-IF
                                      END-IF
                                  END-IF
                                  MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM
                               END-IF
                          END-READ
                      END-PERFORM
                   ELSE
                      IF GS-MOVTO-INI > 0
                         MOVE ZEROS TO SITUACAO-CR20
                         MOVE MOVTO-INI-INV TO DATA-MOVTO-CR20
                         START CRD020 KEY IS NOT LESS ALT3-CR20 INVALID
                                                                     KEY
                               MOVE "10" TO ST-CRD020
                         END-START

                         PERFORM UNTIL ST-CRD020 = "10"
                             READ CRD020 NEXT AT END
                                  MOVE "10" TO ST-CRD020
                             NOT AT END
                                  IF DATA-MOVTO-CR20 > MOVTO-FIM-INV OR
                                     SITUACAO-CR20 <> 0
                                     MOVE "10" TO ST-CRD020
                                  ELSE
                                     MOVE REG-CRD020 TO GS-REGISTRO1
                                     IF VENCTO-INI-INV = 0 OR
                                        (DATA-VENCTO-CR20 NOT <
                                         VENCTO-INI-INV AND
                                         DATA-VENCTO-CR20 NOT >
                                         VENCTO-FIM-INV)
                                         PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                     END-IF
                                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                                  END-IF
                             END-READ
                         END-PERFORM
                      ELSE
                         MOVE ZEROS TO SITUACAO-CR20
                         MOVE VENCTO-INI-INV TO DATA-VENCTO-CR20
                         START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID
                                                                     KEY
                               MOVE "10" TO ST-CRD020
                         END-START

                         PERFORM UNTIL ST-CRD020 = "10"
                             READ CRD020 NEXT AT END
                                  MOVE "10" TO ST-CRD020
                             NOT AT END
                                  IF DATA-VENCTO-CR20 > VENCTO-FIM-INV
                                     MOVE "10" TO ST-CRD020
                                  ELSE
                                     MOVE REG-CRD020 TO GS-REGISTRO1
                                     IF SITUACAO-CR20 = 0
                                        IF MOVTO-INI-INV = 0 OR
                                           (DATA-MOVTO-CR20 NOT <
                                            MOVTO-INI-INV AND
                                            DATA-MOVTO-CR20 NOT >
                                            MOVTO-FIM-INV)
                                            PERFORM
                                                FAZER-OUTRAS-COMPARACOES
                                        END-IF
                                     END-IF
                                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                                     PERFORM CALL-DIALOG-SYSTEM
                                  END-IF
                             END-READ
                         END-PERFORM
                      END-IF
                   END-IF
                END-IF
                MOVE ZEROS          TO GS-SEQ
                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                CLOSE REMES
                PERFORM CARREGA-LISTA
                REFRESH-OBJECT PRINCIPAL.

       FAZER-OUTRAS-COMPARACOES SECTION.
           MOVE REG-CRD020 TO GS-REGISTRO1
           IF GS-PORTADOR = 0 OR PORTADOR-CR20
              IF GS-CONTRATO = 0 OR CLIENTE-CR20(1:4)
                 IF GS-ALBUM = 0 OR CLIENTE-CR20(5:4)
                    IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                       IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
                          MOVE SEQ-W          TO GS-SEQ
                          MOVE "REFRESH-DATA" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                          PERFORM MOVER-DADOS-TIPO1.

       FAZER-OUTRAS-COMP2 SECTION.
           MOVE REG-CRD020 TO GS-REGISTRO1
           IF GS-PORTADOR = 0 OR PORTADOR-CR20
              IF GS-CONTRATO = 0 OR CLIENTE-CR20(1:4)
                 IF GS-ALBUM = 0 OR CLIENTE-CR20(5:4)
                    IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                       IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
                          IF GS-MOVTO-INI = 0 OR
                            (DATA-MOVTO-CR20 NOT < MOVTO-INI AND
                             DATA-MOVTO-CR20 NOT > MOVTO-FIM)
                             MOVE SEQ-W          TO GS-SEQ
                             MOVE "REFRESH-DATA" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM

                             MOVE COD-COMPL-CR20 TO AUX-COD-COMPL
                             WRITE REG-AUXILIAR.

       ATUALIZA-PORTADOR-RECEBER SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020
           OPEN INPUT REMES
           PERFORM ABRE-ARQUIVO-ANOTACAO
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
                READ REMES AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMES(395: 6) TO GS-EXIBE-SEQ
                     MOVE "REFRESH-WIN3" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     MOVE REG-REMES(1: 1)   TO TIPO-W
                     EVALUATE TIPO-W
                        WHEN 1 MOVE REG-REMES
                                 TO DETALHE-ARQUIVO
                               PERFORM PROCURAR-TITULO
                     END-EVALUATE
                END-READ
           END-PERFORM
           CLOSE      CRD020 CRD200 CRD201
           OPEN INPUT CRD020
           MOVE "Portador Atualizado no Contas a Receber" TO MENSAGEM
           MOVE "C"           TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMES.

       PROCURAR-TITULO SECTION.
           MOVE "N" TO ACHEI

           INITIALIZE REG-CRD020
           MOVE D-USO-EMPRESA(2:9) TO COD-COMPL-CR20
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF D-USO-EMPRESA(2:9) <> COD-COMPL-CR20
                         MOVE "10" TO ST-CRD020
                      ELSE
                         STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2) INTO AUX-VENCTO
                         IF D-VENCIMENTO = AUX-VENCTO
                            MOVE "S" TO ACHEI
                            PERFORM GRAVA-ANOTACAO
                            MOVE GS-ACP-PORTADOR TO
                                 PORTADOR-CR20
                            REWRITE REG-CRD020
                            END-REWRITE
                            MOVE "10" TO ST-CRD020
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           IF ACHEI = "N"
              STRING "Contas a Receber Não Encontrado" X"0DA0"
                     "D-USO-EMPRESA => " D-USO-EMPRESA INTO MENSAGEM
                MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
       PROCURAR-TITULO-FIM.
           EXIT.

       ATUALIZA-SEQUENCIA SECTION.
           MOVE 1 TO BOLHSBC-CODIGO
           READ BOLHSBC INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SEQUENCIA-W TO BOLHSBC-NOSSO-NUM
                REWRITE REG-BOLHSBC INVALID KEY
                    MOVE "Erro de Regravação...BOLHSBC" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                    MOVE "Nosso Número Atualizado com Sucesso" TO
                    MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
                END-REWRITE
           END-READ.

       ABRE-ARQUIVO-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O    CRD200
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O    CRD201.

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
           END-PERFORM
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ        TO SEQ-CR200.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS          TO DATA-RETORNO-CR200
           MOVE USUARIO-W      TO USUARIO-CR200
           MOVE DATA-DIA-I     TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
                WRITE REG-CRD200 INVALID KEY
                      ADD 1     TO SEQ-CR200
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD200
                END-WRITE
           END-PERFORM.

           MOVE SEQ-CR200             TO SEQ-CR201.
           MOVE COD-COMPL-CR20        TO COD-COMPL-CR201.
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20         TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20         TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT
           END-READ
           MOVE NOME-PORT             TO ANOTACAO-CR201(43: 16)
           MOVE GS-ACP-PORTADOR       TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY
                MOVE SPACES           TO NOME-PORT
           END-READ
           MOVE NOME-PORT             TO ANOTACAO-CR201(68: 16)
           MOVE ZEROS                 TO ST-CRD201
           MOVE 1                     TO SUBSEQ-CR201
           PERFORM UNTIL ST-CRD201 = "10"
                WRITE REG-CRD201 INVALID KEY
                      ADD 1 TO SUBSEQ-CR201
                      CONTINUE
                NOT INVALID KEY
                      MOVE "10" TO ST-CRD201
                END-WRITE
           END-PERFORM.

       MOVER-DADOS-TIPO0 SECTION.

       MOVER-DADOS-TIPO1 SECTION.
      *>SEQUENCIA NOSSO NUMERO (BANCO)

           MOVE COD-COMPL-CR20         TO COD-COMPL-CG10
                                          COD-COMPL-CG11
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010
           END-READ

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011
           END-READ

           MOVE "S"                      TO OK

           IF GS-CPF = 1
              MOVE  CPF1-CG11 TO   LINK-CPF
              CALL   "DIGCPF"      USING   LINKA-CPF
              CANCEL "DIGCPF"
              IF LINK-CPF-CONF       =       1
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CR20
                     WHEN 0  STRING "Cliente com o CPF Inválido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o CPF Inválido" " | "
                                    "Comum" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF GS-CEP = 1
              IF CEP1-CG11 < 10000000
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CR20
                     WHEN 0  STRING "Cliente com o CEP Inválido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o CEP Inválido" " | "
                                    "Comum" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF GS-ENDERECO = 1
              IF ENDERECO1-CG11 = SPACES OR BAIRRO1-CG11 = SPACES
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CR20
                     WHEN 0  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Comum" " | "
                                     CLIENTE-CR20(1:4) "-"
                                     CLIENTE-CR20(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF OK = "S"
              ADD 1                         TO GS-QTDE-TITULO
              ADD VALOR-TOT-CR20            TO GS-VALOR-TOTAL


              ADD 1                         TO SEQ-W
              MOVE DATA-VENCTO-CR20         TO WS-DATA-DESCONTO
              IF BOLHSBC-DIAS-DESCONTO > 0
                 PERFORM CALCULAR-DATA-DESCONTO
              ELSE
                 INITIALIZE WS-DATA-DESCONTO
              END-IF

              INITIALIZE DETALHE-ARQUIVO
              MOVE 1                        TO D-TIPO-ARQUIVO
              MOVE 2                        TO D-TIPO-INSCRICAO
              MOVE BOLHSBC-CNPJ             TO D-INSC-EMPRESA
              STRING BOLHSBC-AGENCIA BOLHSBC-DIG-AGENCIA
                                          INTO D-AGENCIA
              MOVE "55"                     TO D-SUB-CONTA
              MOVE BOLHSBC-CONVENIO         TO D-CONTRATO-COB
      *CALCULAR DIGITO VERIFICADOR
              MOVE ZEROS                    TO TAB-SEQUENCIA
              ADD 1                         TO SEQUENCIA-W
              MOVE SEQUENCIA-W              TO BOLHSBC-nosso-num
              STRING BOLHSBC-cod-cliente BOLHSBC-nosso-num
                                          INTO TAB-SEQUENCIA
              MOVE 0                        TO IND TOTAL
              PERFORM UNTIL IND = 10
                  ADD 1 TO IND
                  EVALUATE IND
                      WHEN 1   COMPUTE VALOR = TAB-SEQ(IND) * 5
                      WHEN 2   COMPUTE VALOR = TAB-SEQ(IND) * 4
                      WHEN 3   COMPUTE VALOR = TAB-SEQ(IND) * 3
                      WHEN 4   COMPUTE VALOR = TAB-SEQ(IND) * 2
                      WHEN 5   COMPUTE VALOR = TAB-SEQ(IND) * 7
                      WHEN 6   COMPUTE VALOR = TAB-SEQ(IND) * 6
                      WHEN 7   COMPUTE VALOR = TAB-SEQ(IND) * 5
                      WHEN 8   COMPUTE VALOR = TAB-SEQ(IND) * 4
                      WHEN 9   COMPUTE VALOR = TAB-SEQ(IND) * 3
                      WHEN 10  COMPUTE VALOR = TAB-SEQ(IND) * 2
                  END-EVALUATE
                  COMPUTE TOTAL = TOTAL + VALOR
              END-PERFORM

              DIVIDE TOTAL BY 11 GIVING RESTO REMAINDER RESTO2

              IF RESTO2 = 0 OR 1
                 MOVE 0 TO DIGITO-VERIFICADOR
              ELSE
                 COMPUTE DIGITO-VERIFICADOR = 11 - RESTO2
              END-IF
              MOVE DIGITO-VERIFICADOR       TO TAB-SEQ(11)
              MOVE TAB-SEQUENCIA            TO D-NOSSO-NUMERO
      *
      *FIM CALCULAR DIGITO VERIFICADOR
              MOVE ZEROS                    TO D-DATA-DESCONTO-2
                                               D-VALOR-DESCONTO-2
                                               D-DATA-DESCONTO-3
                                               D-VALOR-DESCONTO-3
              MOVE FUNCTION NUMVAL(GS-CARTEIRA(1:1))
                                            TO D-CARTEIRA
              MOVE 01                       TO D-OCORRENCIA

              MOVE "X"                      TO D-USO-EMPRESA(1: 1)
              MOVE COD-COMPL-CR20           TO D-USO-EMPRESA(2: 9)
              STRING DATA-VENCTO-CR20(7:2)
                     DATA-VENCTO-CR20(5:2)
                     DATA-VENCTO-CR20(3:2)
                                          INTO D-VENCIMENTO
              MOVE VALOR-TOT-CR20           TO D-VALOR-TITULO
              MOVE 399                      TO D-BANCO
              MOVE ZEROS                    TO D-AGENCIA-DEPOSITARIA
              MOVE "98"                     TO D-ESPECIE
              MOVE "N"                      TO D-ACEITE
              STRING DATA-EMISSAO-CR20(1:2)
                     DATA-EMISSAO-CR20(3:2)
                     DATA-EMISSAO-CR20(7:2)
                                          INTO D-EMISSAO
              MOVE FUNCTION NUMVAL(BOLHSBC-INSTRUCAO-1) TO WS-INT
              MOVE WS-INT                               TO D-INSTRUCAO1
              MOVE FUNCTION NUMVAL(BOLHSBC-INSTRUCAO-2) TO WS-INT
              MOVE WS-INT                               TO D-INSTRUCAO2

              COMPUTE D-JUROS-MORA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-JUROS-1-DIA / 100))

              MOVE ZEROS                    TO D-VALOR-IOF
              MOVE ZEROS                    TO D-ABATIMENTO

              EVALUATE D-INSTRUCAO1
                 WHEN 15 MOVE ZEROS         TO D-ABATIMENTO
                         STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                         MOVE BOLHSBC-MULTA(8:4) TO D-ABATIMENTO(7:4)
                 WHEN 16 MOVE "V"           TO DETALHE-ARQUIVO(216:1)
                         COMPUTE D-VALOR-IOF ROUNDED = (VALOR-TOT-CR20 *
                                         (BOLHSBC-JUROS-1-DIA / 100))
                         STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                         MOVE SPACES        TO D-ABATIMENTO(212:4)
                 WHEN 19 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "0"               TO D-ABATIMENTO(11:1)
                         MOVE BOLHSBC-QTDE-DIAS TO D-ABATIMENTO(12:2)
                 WHEN 22 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "0"               TO D-ABATIMENTO(11:1)
                         MOVE BOLHSBC-QTDE-DIAS TO D-ABATIMENTO(12:2)
                 WHEN 24 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "000"             TO D-ABATIMENTO(11:3)
                 WHEN 29 STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                 WHEN 71 MOVE BOLHSBC-QTDE-DIAS TO D-DIAS
                 WHEN 72 MOVE BOLHSBC-QTDE-DIAS TO D-DIAS
                 WHEN 73 MOVE SPACES TO DETALHE-ARQUIVO(206:6)
                         MOVE BOLHSBC-MULTA(8:4)TO
                                        DETALHE-ARQUIVO(212:4)
                         MOVE "0"               TO
                                        DETALHE-ARQUIVO(216:1)
                         MOVE BOLHSBC-QTDE-DIAS TO
                                        DETALHE-ARQUIVO(217:2)
                 WHEN 74 MOVE SPACES TO DETALHE-ARQUIVO(206:6)
                         MOVE BOLHSBC-MULTA(8:4)TO
                                        DETALHE-ARQUIVO(212:4)
                         MOVE "0"               TO
                                        DETALHE-ARQUIVO(216:1)
                         MOVE BOLHSBC-QTDE-DIAS TO
                                        DETALHE-ARQUIVO(217:2)
                 WHEN 75 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 76 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 77 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 84 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
              END-EVALUATE

              EVALUATE D-INSTRUCAO2
                 WHEN 15 MOVE ZEROS         TO D-ABATIMENTO
                         STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                         MOVE BOLHSBC-MULTA(8:4) TO D-ABATIMENTO(7:4)
                 WHEN 16 MOVE "V"           TO DETALHE-ARQUIVO(216:1)
                         COMPUTE D-VALOR-IOF ROUNDED = (VALOR-TOT-CR20 *
                                         (BOLHSBC-JUROS-1-DIA / 100))
                         STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                         MOVE SPACES        TO D-ABATIMENTO(212:4)
                 WHEN 19 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "0"               TO D-ABATIMENTO(11:1)
                         MOVE BOLHSBC-QTDE-DIAS TO D-ABATIMENTO(12:2)
                 WHEN 22 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "0"               TO D-ABATIMENTO(11:1)
                         MOVE BOLHSBC-QTDE-DIAS TO D-ABATIMENTO(12:2)
                 WHEN 24 COMPUTE VALOR-MULTA ROUNDED = (VALOR-TOT-CR20 *
                                            (BOLHSBC-MULTA / 100))
                         MOVE VALOR-MULTA(1:8)  TO D-ABATIMENTO(1:8)
                         MOVE VALOR-MULTA(9:2)  TO D-ABATIMENTO(9:2)
                         MOVE "000"             TO D-ABATIMENTO(11:3)
                 WHEN 29 STRING DATA-VENCTO-CR20(7:2)
                                DATA-VENCTO-CR20(5:2)
                                DATA-VENCTO-CR20(3:2)
                                          INTO D-ABATIMENTO(1:6)
                 WHEN 71 MOVE BOLHSBC-QTDE-DIAS TO D-DIAS
                 WHEN 72 MOVE BOLHSBC-QTDE-DIAS TO D-DIAS
                 WHEN 73 MOVE SPACES TO DETALHE-ARQUIVO(206:6)
                         MOVE BOLHSBC-MULTA(8:4)TO
                                        DETALHE-ARQUIVO(212:4)
                         MOVE "0"               TO
                                        DETALHE-ARQUIVO(216:1)
                         MOVE BOLHSBC-QTDE-DIAS TO
                                        DETALHE-ARQUIVO(217:2)
                 WHEN 74 MOVE SPACES TO DETALHE-ARQUIVO(206:6)
                         MOVE BOLHSBC-MULTA(8:4)TO
                                        DETALHE-ARQUIVO(212:4)
                         MOVE "0"               TO
                                        DETALHE-ARQUIVO(216:1)
                         MOVE BOLHSBC-QTDE-DIAS TO
                                        DETALHE-ARQUIVO(217:2)
                 WHEN 75 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 76 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 77 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
                 WHEN 84 MOVE BOLHSBC-QTDE-DIAS TO D-PRAZO
              END-EVALUATE

              STRING DESCONTO-DIA
                     DESCONTO-MES
                     DESCONTO-ANO(3:2)  INTO D-DATA-DESCONTO

              COMPUTE D-VALOR-DESCONTO ROUNDED = (VALOR-TOT-CR20 *
                                               (BOLHSBC-DESCONTO / 100))


              MOVE 01                       TO D-IDENTIF-SACADO
              MOVE CPF1-CG11(3: 14)         TO D-INSC-SACADO
              MOVE COMPRADOR-CG10           TO D-NOME-SACADO
              MOVE ENDERECO1-CG11           TO D-ENDERECO-SACADO
              MOVE BAIRRO1-CG11             TO D-BAIRRO-SACADO
              MOVE CEP1-CG11                TO D-CEP-SACADO

              MOVE CIDADE1-CG11             TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE SPACES              TO NOME-CID UF-CID
              END-READ
              MOVE NOME-CID                 TO D-CIDADE-SACADO
              MOVE UF-CID                   TO D-UF-SACADO
              MOVE 9                        TO D-TIPO-MOEDA
              MOVE SPACES                   TO D-SACADO-AVALISTA

              MOVE BOLHSBC-QTDE-DIAS        TO D-PRAZO
              MOVE SEQ-W                    TO D-NUMERO-REMESSA
              MOVE DETALHE-ARQUIVO          TO DADOS-REMES
              MOVE X"0D0A"                  TO PULA-REMES

              WRITE REG-REMES INVALID KEY
                 MOVE "ERRO DE GRAVAÇÃO...LOTE-ARQUIVO"
                                            TO MENSAGEM
                 MOVE "C"                   TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
              END-WRITE.

       CALCULAR-DATA-DESCONTO SECTION.
           INITIALIZE IND
           PERFORM UNTIL IND = BOLHSBC-DIAS-DESCONTO
               ADD 1 TO IND

               SUBTRACT 1 FROM DESCONTO-DIA
               IF DESCONTO-DIA = 0
                  SUBTRACT 1 FROM DESCONTO-MES
                  IF DESCONTO-MES = 0
                     MOVE 12          TO DESCONTO-MES
                     SUBTRACT 1 FROM DESCONTO-ANO
                  END-IF
                  MOVE TAB-MES(DESCONTO-MES) TO DESCONTO-DIA
               END-IF
           END-PERFORM.
       CALCULAR-DATA-DESCONTO-FIM.
           EXIT.


       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMES.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
                READ REMES AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMES        TO DETALHE-ARQUIVO
                     MOVE D-NUMERO-REMESSA TO GS-SEQ
                     MOVE "REFRESH-DATA"   TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     MOVE REG-REMES         TO GS-LINDET
                     MOVE "INSERE-LIST" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMES.
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
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           OPEN INPUT REMES
           MOVE ZEROS TO SEQ-WK
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
             READ REMES AT END
                  MOVE "10" TO ST-REM
             NOT AT END
                  MOVE REG-REMES (1: 1) TO TIPO-W
                  EVALUATE TIPO-W
                     WHEN 1 MOVE REG-REMES           TO DETALHE-ARQUIVO
                            ADD 1                    TO SEQ-WK
                            MOVE D-NOME-SACADO       TO NOME-WK
                            MOVE D-ENDERECO-SACADO   TO ENDERECO-WK
                            MOVE D-BAIRRO-SACADO     TO BAIRRO-WK
                            MOVE D-CIDADE-SACADO     TO CIDADE-WK
                            MOVE D-UF-SACADO         TO UF-WK
                            MOVE D-CEP-SACADO        TO CEP-WK
                            MOVE D-USO-EMPRESA       TO DOCTO-WK
                            MOVE D-VALOR-TITULO      TO VALOR-WK
                            WRITE REG-WORK
                            END-WRITE
                  END-EVALUATE
             END-READ
           END-PERFORM.

           COPY CONDENSA.

           CLOSE       WORK
           OPEN INPUT  WORK
           MOVE ZEROS  TO LIN
           PERFORM CABECALHO
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE NOME-WK           TO NOME-REL
                      MOVE ENDERECO-WK       TO ENDERECO-REL
                      MOVE BAIRRO-WK         TO BAIRRO-REL
                      MOVE CIDADE-WK         TO CIDADE-REL
                      MOVE UF-WK             TO UF-REL
                      MOVE CEP-WK(1: 5)      TO CEP-REL
                      MOVE CEP-WK(6: 3)      TO SUFIXO-REL
                      MOVE DOCTO-WK          TO DOCTO-REL
                      MOVE VALOR-WK          TO VALOR-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO VALOR-TOTAL-REL
           MOVE QTDE-TIT-T2        TO QTDE-TIT-TOTAL-REL
           WRITE REG-RELAT FROM LINDET1 AFTER 3
           CLOSE REMES WORK
           DELETE FILE WORK.

           COPY DESCONDENSA.
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR     TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"  TO NOME-PORT
           END-READ
           MOVE NOME-PORT       TO GS-DESCR-PORTADOR.

       POPUP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W POP-UP
           CANCEL "CAP018T"
           MOVE POP-UP(1: 30) TO GS-DESCR-PORTADOR
           MOVE POP-UP(33: 4) TO GS-PORTADOR.

      *---------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W GS-EXIT-FLG.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "HSBC"    TO DS-SET-NAME
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
           move "HSBC"              to logacess-programa
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

           CLOSE CRD020 CGD010 CGD011 CAD010 CAD018 CAD001 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
