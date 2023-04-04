      $set mfoo
      $set ooctrl(+P)
       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    VIRTUA.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO XXXXXXXX.XLS P/ VIRTUA
       DATE-WRITTEN.  29-02-2016.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       class-control.
           Popupmenu          is class "popupmnu"
           MSExcel            is class "$OLE$Excel.Application"
           AListview          is class "alistview"
           JDUtils            is class "jdutils"
           Window             is class "wclass".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX020.
           COPY MTPX019.

          $set IDXFORMAT"4" FILETYPE"4"
          SELECT CRD020x ASSIGN TO PATH-CRD020x
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD020x
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR20x = COD-COMPL-CR20x SEQ-CR20x
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR20x
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR20x = COD-COMPL-CR20x
                        DATA-VENCTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR20x = SITUACAO-CR20x
                       DATA-VENCTO-CR20x COD-COMPL-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR20x = SITUACAO-CR20x
                        DATA-MOVTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR20x = SITUACAO-CR20x
                        CLIENTE-CR20x DATA-VENCTO-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR20x = DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-CR20x = NR-DOCTO-CR20x
                       DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8-CR20x = OUTRO-DOCTO-CR20x
                       DATA-RCTO-CR20x
                       SEQ-CAIXA-CR20x WITH DUPLICATES.
           $set IDXFORMAT"0" FILETYPE"0"

           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX014.
           COPY CRPX200.
           COPY CRPX201.
           COPY CHPX010.
           COPY VIRTUA.SEL.
           COPY LOGACESS.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT REMESSA ASSIGN       TO ARQUIVO-REMESSA
                          ORGANIZATION IS         INDEXED
                          ACCESS MODE  IS         DYNAMIC
                          RECORD KEY   IS   REMESSA-CHAVE
                          FILE STATUS  IS      FS-REMESSA.

           select acesso
                  assign       to   arquivo-acesso
                  organization is          indexed
                  access mode  is          dynamic
                  record key   is    acesso-codigo
                  file status  is        fs-acesso.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY MTPW019.

      * Arquivo de Movimento de contas a receber
       FD  CRD020x.
       01  REG-CRD020x.
           05  DATA-MOVTO-CR20x             PIC 9(8).
           05  COD-COMPL-CR20x.
               10  CLASS-CLIENTE-CR20x      PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20x            PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20x                    PIC 9(5).
           05  PORTADOR-CR20x                   PIC 9999.
           05  CARTEIRA-CR20x                   PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20x               PIC 99.
           05  NR-DOCTO-CR20x                   PIC X(10).
           05  OUTRO-DOCTO-CR20x                PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20x                 PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20x               PIC 9(8).
           05  DATA-VENCTO-CR20x                PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20x                  PIC X(30).
           05  SITUACAO-CR20x                   PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20x                 PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20x.
               10  NR-PARC-CR20x                PIC 99.
               10  TOT-PARC-CR20x               PIC 99.
           05  CODREDUZ-APUR-CR20x              PIC 9(5).
           05  VALOR-TOT-CR20x                  PIC 9(8)V99.
           05  JURO-RCTO-CR20x                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20x                 PIC 9(6)V99.
           05  DESCONTO-CR20x                   PIC 9(6)V99.
           05  DATA-RCTO-CR20x                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20x                  PIC 9(8)V99.
           05  CONTABILIZADO-CR20x              PIC 9.
           05  VENDEDOR-CR20x                   PIC 9(6).
           05  RESPONSAVEL-CR20x                PIC X(5).
           05  DIGITADOR-CR20x                  PIC X(5).
           05  SEQ-CAIXA-CR20x                  PIC 9(3).
           05  NR-NOTA-FISCAL-CR20x             PIC X(10).
           05  DATA-NTA-FISCAL-CR20x            PIC 9(8).
           05  FORMA-PAGTO-CR20x                PIC X(10).
           05  DCR-MEM-CR20x                    PIC X(15).
           05  CARTAO-CRED-CR20x                PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20x      PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20x      PIC 9(03)V99.
           05  VALOR-SALDO-CR20x                PIC 9(08)V99.
           05  FILLER                           PIC X(08).

       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW014.
       COPY CRPW200.
       COPY CRPW201.
       COPY CHPW010.
       COPY VIRTUA.FD.
       COPY LOGACESS.FD.

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
           05  TIPO-WK          PIC X(16).

       FD  RELAT.
       01  REG-RELAT.
           05  FILLER           PIC X(132).

       FD  ACESSO.
       01  REG-ACESSO.
           05 ACESSO-CODIGO     PIC 9(01).
           05 ACESSO-USUARIO    PIC X(05).
           05 ACESSO-DATA       PIC 9(08).
           05 ACESSO-HORARIO    PIC 9(08).

      *> REMESSA-TIPO => 1 Contas a receber, 2 Cheque
       FD  REMESSA.
       01  REG-REMESSA.
           05 REMESSA-CHAVE.
              10 REMESSA-TIPO          PIC 9(01).
              10 REMESSA-CHAVE-ARQUIVO PIC X(20).
           05 REMESSA-NOME             PIC X(60).
           05 REMESSA-ENDERECO         PIC X(60).
           05 REMESSA-BAIRRO           PIC X(30).
           05 REMESSA-CIDADE           PIC X(30).
           05 REMESSA-UF               PIC X(02).
           05 REMESSA-CEP              PIC X(08).
           05 REMESSA-NUMERO           PIC X(10).
           05 REMESSA-VALOR            PIC 9(06)V99.


       WORKING-STORAGE SECTION.
           COPY "VIRTUA.CPB".
           COPY "VIRTUA.CPY".
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
           05 ST-CRD020x             PIC XX     VALUE SPACES.
           05 ST-CGD001              PIC XX     VALUE SPACES.
           05 ST-CGD010              PIC XX     VALUE SPACES.
           05 ST-MTD019              PIC XX     VALUE SPACES.
           05 ST-CGD011              PIC XX     VALUE SPACES.
           05 ST-CGD014              PIC XX     VALUE SPACES.
           05 ST-CRD200              PIC XX     VALUE SPACES.
           05 ST-CRD201              PIC XX     VALUE SPACES.
           05 ST-CHD010              PIC XX     VALUE SPACES.
           05 FS-LOGACESS            PIC XX     VALUE SPACES.
           05 FS-VIRTUA              PIC XX     VALUE SPACES.
           05 FS-ACESSO              PIC XX     VALUE SPACES.
           05 FS-REMESSA             PIC XX     VALUE SPACES.
           05 ST-REM                 PIC XX     VALUE SPACES.
           05 ST-WORK                PIC XX     VALUE SPACES.
           05 VARIA-W                PIC 9(8)   VALUE ZEROS.
           05 VALOR-W                PIC 9(11)V99 VALUE ZEROS.
           05 SEQ-W                  PIC 9(6)   VALUE ZEROS.
           05 OPCAO                  PIC 9      VALUE ZEROS.
           05 SEQUENCIA-W            PIC 9(10)     VALUE ZEROS.
           05 TIPO-W                 PIC 9      VALUE ZEROS.
           05 DATA-DIA               PIC 9(6)   VALUE ZEROS.
           05 COD-COMPL-CR20-W       PIC 9(09)  VALUE ZEROS.
           05 COD-COMPL-CH10-W       PIC 9(09)  VALUE ZEROS.
           05 DATA-DIA-I             PIC 9(6)   VALUE ZEROS.
           05 HORA-BRA               PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ                PIC 9(5)   VALUE ZEROS.
           05 DATA-E                 PIC 99/99/99.
           05 VALOR-ATRASO           PIC 9(11)V99 VALUE ZEROS.
           05 CONF                   PIC X      VALUE SPACES.
           05 ERRO-W                 PIC 9        VALUE ZEROS.
           05 DATA-INV               PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV         PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV         PIC 9(8)     VALUE ZEROS.
           05 LIN                    PIC 9(02)    VALUE ZEROS.
           05 IND                    PIC 9(03)    VALUE ZEROS.
           05 IND2                   PIC 9(03)    VALUE ZEROS.
           05 PASSAR-STRING-1        PIC X(65).
           05 MASC-VALOR             PIC ZZZ.ZZZ.ZZZ,99 VALUE ZEROS.
           05 MOVTO-INI-INV          PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV          PIC 9(8)     VALUE ZEROS.
           05 QTDE-TIT-T2            PIC 9(8).
           05 AUX-TIPO               PIC 9(1)     VALUE ZEROS.
           05 AUX-CARTEIRA           PIC 9(1)     VALUE ZEROS.
           05 MASC-NUMERO            PIC ZZ.ZZZ.ZZ9 VALUE ZEROS.
           05 QTDE-PARCELAS          PIC 9(03)    VALUE ZEROS.
           05 VALOR-TOT-TIT-T2       PIC 9(14).
           05 CNPJ                   PIC 9(14).
           05 CPF                    PIC 9(11).
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

       01 lnkusu.
          copy usuario.cpy.

       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.
       01 wsPopMenu                    object reference value null.
       01 aExcel                       object reference value null.
       01 aItemDeMenu                  object reference value null.
       77 lsIndice                     pic 9(09) comp-5 value zeros.
       77 lsSizeLitView                pic 9(09) comp-5 value zeros.
       77 lsIndiceColuna               pic 9(09) comp-5 value zeros.
       77 lsIndiceColunaX              pic 9(09) comp-5 value zeros.
       01 wsPai                        object reference value null.
       01 wsBarra                      object reference value null.

       01 ExcelObject                  object reference.
       01 WorkBooksCollection          object reference.
       01 WorkBook                     object reference.

       01 lnktabela.
          02 lnkobjetoscol             object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas                pic 9(09) comp-5 value zeros
                                       occurs 99 times.

       01 indice                       pic 9(02).
       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 NumeroDeColunas              pic 9(9) comp-5.
       01 LsNumeroCelulas              pic 9(9).
       77 lsContador                   pic 9(09).
       77 lsContadorEx                 pic 9(09) comp-5.
       01 indiceLinha                  pic 9(06) value zeros.
       77 lsTamanhoColuna              pic 9(09) comp-5.
       77 lsObjTexto                   object reference value null.
       01 lsColunaString               pic x(255) value spaces.
       01 Cell                         object reference.
       77 lsTexto                      pic x(255).
       01 lsInd                        pic 9(9).
       77 lsLinhaTexto                 pic x(255) value spaces.
       77 lsLetra                      pic x(02).
       77 lsLetraNulo                  pic x(03).
       01 CellRange                    object reference.

       01 masc-agencia                 pic 9(04).
       01 masc-conta                   pic 9(08).


       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

       01 DETALHE-ARQUIVO.
      *   05 D-SEU-NUMERO          PIC X(11).
          05 D-SEU-NUMERO          PIC X(15).
      *   05 FILLER                PIC X(04).
          05 D-CODIGO              PIC X(15).
          05 D-VECTO               PIC 9(08).
          05 D-EMISSAO             PIC 9(08).
          05 D-ESPECIE             PIC 9(02).
          05 D-ACEITE              PIC X(01).
          05 D-PROCESSAMENTO       PIC 9(08).
          05 D-FORMA-ENTREGA       PIC 9(01).
          05 D-NOSSO-NUMERO        PIC X(11).
          05 FILLER                PIC X(06).
          05 D-VALOR-TITULO        PIC 9(13)V99.
          05 D-CARTEIRA            PIC 9(01).
          05 D-VALOR-DESCONTO      PIC 9(12)V99.
          05 D-DATA-DESCONTO       PIC 9(08).
          05 D-VALOR-DESCONTO2     PIC 9(12)V99.
          05 D-DATA-DESCONTO2      PIC 9(08).
          05 D-VALOR-DESCONTO3     PIC 9(12)V99.
          05 D-DATA-DESCONTO3      PIC 9(08).
          05 D-VALOR-MULTA         PIC 9(13)V99.
          05 D-DATA-MULTA          PIC 9(08).
          05 D-VALOR-JUROS         PIC 9(13)V99.
          05 D-VALOR-ABATIMEN      PIC 9(13)V99.
          05 D-PRAZO-PROTESTO      PIC 9(02).
          05 D-MENSAGEM1           PIC X(40).
          05 D-MENSAGEM2           PIC X(40).
          05 D-MENSAGEM3           PIC X(40).
          05 D-MENSAGEM4           PIC X(40).
          05 D-MENSAGEM5           PIC X(40).
          05 D-MENSAGEM6           PIC X(40).
      *   05 D-AVALISTA            PIC X(20).
          05 D-AVALISTA            PIC X(30).
          05 D-TIPO-PES            PIC X(01). *> F / J
      *   05 D-CPF-CNPJ            PIC X(20).
          05 D-CPF-CNPJ            PIC 9(14).
          05 D-TIPO-BLOQUETO       PIC X(01). *> UNICO / CARNE
          05 D-QTDE-PARCE          PIC 9(03). *> CAMPO NOVO
          05 D-IMPRESSO            PIC X(01). *> S/N

       01 RESTANTE.
          05 D-INSTRUCAO-COB       PIC 9(01).
          05 D-OPCAO-VECTO         PIC 9(01).
          05 D-COD-MOEDA           PIC 9(02).
          05 D-COMPENSACAO         PIC X(01).
          05 D-FILLER              PIC X(03).


       77 ws-int                   pic 9(02).
       01 ws-data-desconto.
          05 desconto-ano          pic 9(04).
          05 desconto-mes          pic 9(02).
          05 desconto-dia          pic 9(02).

       01 tabela-dias              pic x(024)
                                   value "312831303130313130313031".
       01 tab-mes                  pic 9(002) occurs 12 times
                                   redefines tabela-dias.

       01  linka-cgc.
           05  link-cgc             pic 9(14).
           05  link-cgc-r redefines link-cgc.
               10  link-cgc-num     pic 9(12).
               10  link-cgc-dig1    pic 9(01).
               10  link-cgc-dig2    pic 9(01).
           05  link-cgc-conf        pic 9(01).

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
       01 aux-valor                    pic 9(06) value zeros.
       01 aux-numero2                  pic 9(06) value zeros.
       01 resto                        pic 9(02) value zeros.
       01 parcela                      pic 9(03) value zeros.

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
           05  FILLER               PIC X     VALUE SPACES.
           05  TIPO-REL             PIC X(16).
       01  LINDET1.
           05  FILLER               PIC X(20) VALUE 'VALOR TOTAL.: '.
           05  VALOR-TOTAL-REL      PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(20) VALUE 'QTDE TITULOS: '.
           05  QTDE-TIT-TOTAL-REL   PIC ZZZ.ZZZ.

       01 TABELA-NUMERO             PIC 9(10).
       01 REDEFINES TABELA-NUMERO OCCURS 10 TIMES.
          05 TAB-NUMERO             PIC 9(01).

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

           COPY IMPRESSORA.

       77  POP-UP                  PIC X(65).
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
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD014.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020x.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "VIRTUA" TO ARQ-REC.  MOVE EMPRESA-REF TO ARQUIVO-VIRTUA
           MOVE "ACESSO"   TO ARQ-REC.  MOVE EMPRESA-REF
                                                      TO ARQUIVO-ACESSO
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CAD010 CAD018 CAD001 CGD001 CGD014
                      CHD010 MTD019
           OPEN I-O   VIRTUA
           CLOSE      VIRTUA
           OPEN I-O   VIRTUA CRD020
           CLOSE      CRD020 CRD020X
           OPEN INPUT CRD020 CRD020X

           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
           IF ST-CGD014 <> "00"
              MOVE "ERRO ABERTURA CGD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD014 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "VIRTUA"            to logacess-programa
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

           open i-o acesso

           move 8                      to acesso-codigo
           read acesso invalid key
                move 8                 to acesso-codigo
                move usuario-w         to acesso-usuario
                move ws-data-cpu       to acesso-data
                move ws-hora-sys       to acesso-horario
                write reg-acesso invalid key
                     move "Erro de Gravação...ACESSO" to mensagem
                     move "C" to tipo-msg
                     perform exibir-mensagem
                end-write
                move "S" to ok
           not invalid key
                move "N" to ok
           end-read

           close    acesso

           if ok = "N"
              move spaces to mensagem
              string "Outro Usuário está com o programa Aberto" x"0da0"
                     "Usuário => " acesso-usuario x"0da0"
                     "Data => " acesso-data(7:2) "/"
                                acesso-data(5:2) "/"
                                acesso-data(1:4)  x"0da0"
                     "Horário => " acesso-horario(1:2) ":"
                                   acesso-horario(3:2) ":"
                                   acesso-horario(5:2)
                into mensagem
                move "C" to tipo-msg
              perform exibir-mensagem
              move 1 to erro-w
              move 1 to gs-exit-flg.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
                   MOVE 1 TO GS-CEP GS-CPF GS-ENDERECO
                   REFRESH-OBJECT PRINCIPAL
                   PERFORM LER

      *            MOVE VIRTUA-NOSSO-NUMERO     TO MASC-NUMERO
      *            MOVE SPACES                  TO MENSAGEM
      *            STRING "Preste Atenção na Seqüência Mostrada" x"0da0"
      *                   "Seqüência -> " masc-numero
      *              INTO MENSAGEM
      *              MOVE "C" TO TIPO-MSG
      *            PERFORM EXIBIR-MENSAGEM

               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
               WHEN GS-GERAR-REMESSA-TRUE
                    PERFORM GERA-ARQ-REMESSA
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
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTO
               WHEN GS-ITEM-MENU-TRUE
                    PERFORM MENU-SELECIONADO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.


       MENU-SELECIONADO SECTION.
           invoke JanelaPrincipal "menuItemAtCommand"
                  using gs-menu-selecionado returning aItemDeMenu

           evaluate aItemDeMenu
              when aExcel perform exportar-excel
           end-evaluate.

       exportar-excel section.
           invoke gs-acp-listview "getAncestor" returning wsPai
           invoke gs-acp-listview "size" returning lsSizeLitView

           move spaces to lsColunaString
           move z"Exportando informações, aguarde" to lsColunaString
           invoke jdUtils "CriarBarraDeProgresso"
                   using lsSizeLitView wsPai lsColunaString
                           returning wsBarra



           initialize LsNumeroCelulas NumeroDeColunas
           invoke gs-acp-listview "numberOfColumns"
                  returning NumeroDeColunas

           invoke MSExcel "new" returning ExcelObject
           invoke ExcelObject "setVisible" using by value 0
           invoke ExcelObject "getWorkBooks"
                  returning WorkBooksCollection

           invoke WorkBooksCollection "Add" returning WorkBook

      *>Dados iniciais
           move 1 to indiceLinha

      *>                                                   Data Remessa
           move 1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Dt. Remessa" to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                                              Número da Remessa
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Número da Remessa" to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                                              Código da Empresa
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Código da Empresa" to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                            Código de Evento Ref. A Atualização
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Código de Evento Ref. A Atualização" to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                        Retomar/Liquidar Operacao não Presentes
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Retomar/Liquidar Operacao não Presentes"
                                       to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                        Ver: 07-05-2015
           move 8 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move "Ver: 07-05-2015"
                                       to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *            PREENCHER AS INFORMAÇÕES DO CABEÇALHO
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

           move function current-date  to ws-data-sys

           add  1 to indiceLinha

      *>                                                   Data Remessa
           move 1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move spaces to lsColunaString
           string ws-dia-cpu "/" ws-mes-cpu "/" ws-ano-cpu into
                                 lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                                              Número da Remessa
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move spaces to lsColunaString
           string gs-acp-codemp ws-ano-cpu(3:2) ws-mes-cpu ws-dia-cpu
             into lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                                              Código da Empresa
           add  1 to lsContadorEX
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           move gs-acp-codemp  to lsColunaString
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>                            Código de Evento Ref. A Atualização
      *    add  1 to lsContadorEX
      *    invoke ExcelObject "getCells"
      *           using by value indiceLinha
      *                 by value lsContadorEx returning Cell
      *    move "Código de Evento Ref. A Atualização" to lsColunaString
      *    string lsColunaString x"00" delimited by "     "
      *           into lsTexto
      *    invoke Cell "SetValue" using lsTexto
      *    invoke Cell "finalize" returning Cell
      *>                        Retomar/Liquidar Operacao não Presentes
      *    add  1 to lsContadorEX
      *    invoke ExcelObject "getCells"
      *           using by value indiceLinha
      *                 by value lsContadorEx returning Cell
      *    move "Retomar/Liquidar Operacao não Presentes"
      *                                to lsColunaString
      *    string lsColunaString x"00" delimited by "     "
      *           into lsTexto
      *    invoke Cell "SetValue" using lsTexto
      *    invoke Cell "finalize" returning Cell
      *>                        Ver: 07-05-2015
      *    move 8 to lsContadorEX
      *    invoke ExcelObject "getCells"
      *           using by value indiceLinha
      *                 by value lsContadorEx returning Cell
      *    move "Ver: 07-05-2015"
      *                                to lsColunaString
      *    string lsColunaString x"00" delimited by "     "
      *           into lsTexto
      *    invoke Cell "SetValue" using lsTexto
      *    invoke Cell "finalize" returning Cell


      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      *            PREENCHER COM OS DADOS DO LISTVIEW
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      *>Exporta o cabeçalho do listview
           initialize lsContador
                      lsContadorEx

           add 1        to indiceLinha
           perform numeroDeColunas times
               add 1    to lsContador

               initialize lsTamanhoColuna
               invoke lnkobjetoscol(lscontador) "GetWidth"
                      returning lsTamanhoColuna

               if lsTamanhoColuna > 0
                  add 1 to lsContadorEX lsNumeroCelulas
                  invoke lnkobjetoscol(lscontador) "GetLabel"
                         returning lsObjTexto
                  move spaces to lsColunaString
                  invoke lsObjTexto "getValue" returning lsColunaString

                  invoke ExcelObject "getCells"
                         using by value indiceLinha
                               by value lsContadorEx returning Cell
                  string lsColunaString x"00" delimited by "     "
                         into lsTexto
                  invoke Cell "SetValue" using lsTexto
                  invoke Cell "finalize" returning Cell

               end-if
           end-perform.
      *>Exporta os dados do listview

           initialize lsIndice
                      lsSizeLitView
                      lsIndiceColuna

           invoke gs-acp-listview "size" returning lsSizeLitView

           perform lsSizeLitView times
               add 1 to lsIndice
               invoke gs-acp-listview "itemAtIndex" using lsIndice
                                                returning umitem

               invoke wsBarra "setPosition" using lsIndice


               add 1 to indiceLinha
               initialize lsIndiceColuna lsIndiceColunaX
               perform numeroDeColunas times

                  add 1 to lsIndiceColuna
                  invoke lnkobjetoscol(lsIndiceColuna) "GetWidth"
                         returning lsTamanhoColuna

                  if lsTamanhoColuna > 0
                        add 1 to lsIndiceColunaX
                        invoke umitem "getColumnValue"
                               using lsIndiceColuna returning lsObjTexto

                        initialize lsTexto
                        invoke lsObjTexto "getValue" returning lsTexto

                        initialize lsInd lsLinhaTexto
                        inspect function reverse (lsTexto)
                             tallying lsInd for leading spaces

                        compute lsInd = length of lsTexto - lsInd
                        string lsTexto(1:lsInd) x"00" into lsLinhaTexto

                        invoke ExcelObject "getCells" using
                                            by value indiceLinha
                                            by value lsIndiceColunaX
                                            returning Cell

                        invoke Cell "Select"
                        invoke Cell "SetValue" using lsLinhaTexto

                        invoke Cell "finalize" returning Cell
                  end-if
               end-perform
           end-perform

           initialize lsContador
           perform lsNumeroCelulas times

              add 1 to lsContador
              perform trazer-Letra

              string lsLetra x"00" delimited by " " into lsLetraNulo
              invoke ExcelObject "getColumns" using lsLetraNulo
                                          returning cellRange
              invoke cellRange "Select"
              invoke cellRange "AutoFit"
              invoke cellRange "finalize" returning cellRange

           end-perform

           invoke WorkBook "finalize" returning WorkBook
           invoke WorkBooksCollection "finalize"
                  returning WorkBooksCollection

           invoke ExcelObject "setVisible" using by value 1
           invoke jdUtils "FecharBarraDeProgresso" using wsBarra.


       TRAZER-LETRA SECTION.
           evaluate lsContador
              when 1  move "A"  to lsLetra
              when 2  move "B"  to lsLetra
              when 3  move "C"  to lsLetra
              when 4  move "D"  to lsLetra
              when 5  move "E"  to lsLetra
              when 6  move "F"  to lsLetra
              when 7  move "G"  to lsLetra
              when 8  move "H"  to lsLetra
              when 9  move "I"  to lsLetra
              when 10 move "J"  to lsLetra
              when 11 move "K"  to lsLetra
              when 12 move "L"  to lsLetra
              when 13 move "M"  to lsLetra
              when 14 move "N"  to lsLetra
              when 15 move "O"  to lsLetra
              when 16 move "P"  to lsLetra
              when 17 move "Q"  to lsLetra
              when 18 move "R"  to lsLetra
              when 19 move "S"  to lsLetra
              when 20 move "T"  to lsLetra
              when 21 move "U"  to lsLetra
              when 22 move "V"  to lsLetra
              when 23 move "W"  to lsLetra
              when 24 move "X"  to lsLetra
              when 25 move "Y"  to lsLetra
              when 26 move "Z"  to lsLetra
              when 27 move "AA" to lsLetra
              when 28 move "AB" to lsLetra
              when 29 move "AC" to lsLetra
              when 30 move "AD" to lsLetra
              when 31 move "AE" to lsLetra
              when 32 move "AF" to lsLetra
              when 33 move "AG" to lsLetra
              when 34 move "AH" to lsLetra
              when 35 move "AI" to lsLetra
              when 36 move "AJ" to lsLetra
              when 37 move "AK" to lsLetra
              when 38 move "AL" to lsLetra
              when 39 move "AM" to lsLetra
              when 40 move "AN" to lsLetra
              when 41 move "AO" to lsLetra
              when 42 move "AP" to lsLetra
              when 43 move "AQ" to lsLetra
              when 44 move "AR" to lsLetra
              when 45 move "AS" to lsLetra
              when 46 move "AT" to lsLetra
              when 47 move "AU" to lsLetra
              when 48 move "AV" to lsLetra
              when 49 move "AW" to lsLetra
              when 50 move "AX" to lsLetra
              when 51 move "AY" to lsLetra
              when 52 move "AZ" to lsLetra
              when 53 move "BA" to lsLetra
              when 54 move "BB" to lsLetra
              when 55 move "BC" to lsLetra
              when 56 move "BD" to lsLetra
              when 57 move "BE" to lsLetra
              when 58 move "BF" to lsLetra
              when 59 move "BG" to lsLetra
              when 60 move "BH" to lsLetra
              when 61 move "BI" to lsLetra
              when 62 move "BJ" to lsLetra
              when 63 move "BK" to lsLetra
              when 64 move "BL" to lsLetra
              when 65 move "BM" to lsLetra
              when 66 move "BN" to lsLetra
              when 67 move "BO" to lsLetra
              when 68 move "BP" to lsLetra
              when 69 move "BQ" to lsLetra
              when 70 move "BR" to lsLetra
              when 71 move "BS" to lsLetra
              when 72 move "BT" to lsLetra
              when 73 move "BU" to lsLetra
              when 74 move "BV" to lsLetra
              when 75 move "BW" to lsLetra
              when 76 move "BX" to lsLetra
              when 77 move "BY" to lsLetra
              when 78 move "BZ" to lsLetra
           end-evaluate.


       CRIAR-LISTVIEW SECTION.
          initialize indice

      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TIPO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NR OPERAÇÃO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME OPERAÇÃO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"AGÊNCIA" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CONTA" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"PRODUTO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DT. ATUALIZADO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DT. VENCIMENTO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VALOR OPERAÇÃO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"VALOR VENCIDO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"COND. NEGOCIAIS" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CPF / CNPJ" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"MCI" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NR FICHA" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME DO CLIENTE" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"ENDEREÇO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"BAIRRO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CEP" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CIDADE" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"UF" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE1" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 3" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 4" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 5" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 6" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"DATA NASCIMENTO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NATURALIDADE" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"SEXO" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"ESTADO CIVIL" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME DO PAI" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME DA MÃE" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME AVALISTA 1" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CPF/CNPJ AVALISTA 1" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"ENDEREÇO AVALISTA 1" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"BAIRRO AVALISTA 1" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CEP AVALISTA 1" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CIDADE AVALISTA 1" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"UF AVALISTA 1" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 1 AVALISTA 1"
            returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"TELEFONE 2 AVALISTA 1"
            returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"NOME AVALISTA 2"
            returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CPF/CNPJ AVALISTA 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"ENDEREÇO AVALISTA 2" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"BAIRRO AVALISTA 2" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CEP AVALISTA 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"CIDADE AVALISTA 2" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"UF AVALISTA 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
          using z"TELEFONE 1 AVALISTA 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
          using z"TELEFONE 2 AVALISTA 2" returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
          using z"PROFISSÃO" returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"NOME LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"ENDEREÇO LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"BAIRRO LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"CEP LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"CIDADE LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"UF LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 1 LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 2 LOCAL DE TRABALHO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"REFERENCIA PESSOAL"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 1 REFERENCIA"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 2 REFERENCIA"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"REFERENCIA PESSOAL 2"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 1 REFERENCIA 2"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 2 REFERENCIA 2"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"REFERENCIA PESSOAL 3"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 1 REFERENCIA 3"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"TELEFONE 2 REFERENCIA 3"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"SPC/SERASA"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"E-MAIL"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"DT. EMISSÃO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"VALOR PROTESTO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"OBS. OPERAÇÃO"
                       returning lnkobjetoscol(indice)
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"DT. FIMTERCERIZAÇÃO"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "Centered"
          move indice to lnkcolunas(indice)
      *>---
      *>---
          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"VALOR JUROS"
                       returning lnkobjetoscol(indice)
          invoke lnkobjetoscol(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-acp-listview "gridLines"
          invoke gs-acp-listview "noBorder".

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-virtua" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-virtua" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       zebrar-itens section.
           move "listview-virtua" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-virtua" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       TRATAR-EVENTO SECTION.
           evaluate gs-acp-evento
      *>F5
               when 34116  perform exportar-excel
               when 34123  perform chamar-colunas-favo
               when 34593  perform 052-mostrarpopup                     *>BOTAO DIREITO
               when 34027  set-focus ef4
           end-evaluate.

       052-mostrarpopup section.
           invoke wsPopMenu "showAtMouse".
       052-mostrarpopup-fim.
           exit.

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
           MOVE 1                           TO VIRTUA-CODIGO
           READ VIRTUA INVALID KEY
                MOVE "Não Existe a Parâmetrização" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                INITIALIZE REG-VIRTUA.

           PERFORM MOVER-DADOS.

       MOVER-DADOS SECTION.
           MOVE VIRTUA-NOSSO-NUMERO     TO GS-ACP-I-NOSSO-NUMERO

           MOVE VIRTUA-COD-EMPRESA      TO GS-ACP-CODEMP
           MOVE VIRTUA-PORTADOR         TO GS-ACP-PORTADOR
                                           PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"          TO NOME-PORT
           END-READ
           MOVE NOME-PORT               TO GS-DESC-PORTADOR.


       GRAVAR SECTION.
           INITIALIZE REG-VIRTUA

           MOVE 1                       TO VIRTUA-CODIGO
           MOVE GS-ACP-I-NOSSO-NUMERO   TO VIRTUA-NOSSO-NUMERO
           MOVE GS-ACP-PORTADOR         TO VIRTUA-PORTADOR
           MOVE GS-ACP-CODEMP           TO VIRTUA-COD-EMPRESA

           WRITE REG-VIRTUA INVALID KEY
                 REWRITE REG-VIRTUA INVALID KEY
                      MOVE "Erro de Regravação...VIRTUA" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal
          invoke janelaPrincipal "CentralizarNoDesktop"

           *>>>>>>>>>>>>>>>>>>>

           invoke PopupMenu "new" using janelaPrincipal
                  returning wsPopMenu

           invoke wsPopMenu "AddItemZ"
                  using z"Exportar excel"
                  returning aExcel

           invoke wsPopMenu "create".

       GERA-ARQ-REMESSA SECTION.
           MOVE FUNCTION NUMVAL(GS-TIPO-DOCTO(1:1)) TO AUX-TIPO
           MOVE FUNCTION NUMVAL(GS-CARTEIRA(1:1))   TO AUX-CARTEIRA

           ACCEPT WS-HORA-SYS FROM TIME

           MOVE SPACES              TO ARQUIVO-REMESSA
           STRING "\PROGRAMA\KELLO\TEMP\" WS-HORA-SYS
                                  INTO ARQUIVO-REMESSA

           invoke gs-acp-listview "DeleteAll"

           OPEN OUTPUT REMESSA
           CLOSE       REMESSA
           OPEN I-O    REMESSA


           MOVE "CLEAR-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

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


           MOVE 1 TO VIRTUA-CODIGO
           READ VIRTUA INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM MOVER-DADOS
                MOVE VIRTUA-NOSSO-NUMERO TO SEQUENCIA-W
                                            SEQ-W

                MOVE ZEROS               TO GS-QTDE-TITULO
                                            GS-VALOR-TOTAL

                IF GS-LISTAR-RECEBER = 1
                   PERFORM LISTAR-RECEBER
                END-IF

                IF GS-LISTAR-CHEQUES = 1
                   PERFORM LISTAR-CHEQUES
                END-IF

                MOVE ZEROS          TO GS-SEQ
                MOVE "REFRESH-DATA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                PERFORM CARREGA-LISTA
                REFRESH-OBJECT PRINCIPAL
           END-READ
           CLOSE       REMESSA.

       LISTAR-RECEBER SECTION.
           INITIALIZE REG-CRD020
           IF GS-ALBUM > 0
              IF GS-CONTRATO = 0
                 STRING "9" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                 MOVE COD-COMPL-CR20               TO COD-COMPL-CR20-W
              ELSE
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                 MOVE COD-COMPL-CR20               TO COD-COMPL-CR20-W
              END-IF
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
                                (DATA-MOVTO-CR20 NOT <
                                 MOVTO-INI-INV AND
                                 DATA-MOVTO-CR20 NOT >
                                 MOVTO-FIM-INV)
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
                                   (DATA-MOVTO-CR20 NOT <
                                    MOVTO-INI-INV AND
                                    DATA-MOVTO-CR20 NOT >
                                    MOVTO-FIM-INV)
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
                    MOVE ZEROS         TO SITUACAO-CR20
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
                    MOVE ZEROS          TO SITUACAO-CR20
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
                    END-PERFORM
                 END-IF
              END-IF
           END-IF.
       LISTAR-RECEBER-FIM.
           EXIT.

       LISTAR-CHEQUES SECTION.
           INITIALIZE REG-CHD010
           IF GS-ALBUM > 0
              IF GS-CONTRATO = 0
                 STRING "9" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CH10
                 MOVE COD-COMPL-CH10               TO COD-COMPL-CH10-W
              ELSE
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CH10
                 MOVE COD-COMPL-CH10               TO COD-COMPL-CH10-W
              END-IF
              START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
                    MOVE "10" TO ST-CHD010
              END-START
              PERFORM UNTIL ST-CHD010 = "10"
                   READ CHD010 NEXT WITH IGNORE LOCK AT END
                        MOVE "10" TO ST-CHD010
                   NOT AT END
                        IF COD-COMPL-CH10 <> COD-COMPL-CH10-W
                           MOVE "10" TO ST-CHD010
                        ELSE
                           IF SITUACAO-CH10 = 5
                              IF MOVTO-INI-INV = 0 OR
                                (DATA-MOVTO-CH10 NOT <
                                 MOVTO-INI-INV AND
                                 DATA-MOVTO-CH10 NOT >
                                 MOVTO-FIM-INV)
                                 IF VENCTO-INI-INV = 0 OR
                                    (DATA-VENCTO-CH10 NOT <
                                     VENCTO-INI-INV AND
                                     DATA-VENCTO-CH10 NOT >
                                     VENCTO-FIM-INV)
                                    PERFORM FAZER-OUTRAS-COMP-CHQ
                                 END-IF
                              END-IF
                           END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-CONTRATO > 0
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CH10
                 START CHD010 KEY IS NOT LESS CHAVE-CH10 INVALID KEY
                       MOVE "10" TO ST-CHD010
                 END-START
                 PERFORM UNTIL ST-CHD010 = "10"
                     READ CHD010 NEXT WITH IGNORE LOCK AT END
                          MOVE "10" TO ST-CHD010
                     NOT AT END
                          IF CLIENTE-CH10(1:4) <> GS-CONTRATO
                             MOVE "10" TO ST-CHD010
                          ELSE
                             IF SITUACAO-CH10 = 5
                                IF MOVTO-INI-INV = 0 OR
                                   (DATA-MOVTO-CH10 NOT <
                                    MOVTO-INI-INV AND
                                    DATA-MOVTO-CH10 NOT >
                                    MOVTO-FIM-INV)
                                    IF VENCTO-INI-INV = 0 OR
                                       (DATA-VENCTO-CH10 NOT <
                                        VENCTO-INI-INV AND
                                        DATA-VENCTO-CH10 NOT >
                                        VENCTO-FIM-INV)
                                        PERFORM FAZER-OUTRAS-COMP-CHQ
                                     END-IF
                                 END-IF
                             END-IF
                          END-IF
                     END-READ
                 END-PERFORM
              ELSE
                 IF GS-MOVTO-INI > 0
                    MOVE MOVTO-INI-INV TO DATA-MOVTO-CH10
                    START CHD010 KEY IS NOT LESS CHAVE-CH10 INVALID KEY
                          MOVE "10" TO ST-CHD010
                    END-START

                    PERFORM UNTIL ST-CHD010 = "10"
                        READ CHD010 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CHD010
                        NOT AT END
                             IF DATA-MOVTO-CH10 > MOVTO-FIM-INV
                                MOVE "10" TO ST-CHD010
                             ELSE
                                IF SITUACAO-CH10 = 5
                                   IF VENCTO-INI-INV = 0 OR
                                      (DATA-VENCTO-CH10 NOT <
                                       VENCTO-INI-INV AND
                                       DATA-VENCTO-CH10 NOT >
                                       VENCTO-FIM-INV)
                                       PERFORM FAZER-OUTRAS-COMP-CHQ
                                   END-IF
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM
                 ELSE
                    MOVE 5              TO SITUACAO-CH10
                    MOVE VENCTO-INI-INV TO DATA-VENCTO-CH10
                    START CHD010 KEY IS NOT LESS ALT-CH2 INVALID KEY
                          MOVE "10" TO ST-CHD010
                    END-START

                    PERFORM UNTIL ST-CHD010 = "10"
                        READ CHD010 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CHD010
                        NOT AT END
                             IF SITUACAO-CH10 <> 5 OR
                                DATA-VENCTO-CH10 > VENCTO-FIM-INV
                                MOVE "10" TO ST-CHD010
                             ELSE
                                   IF MOVTO-INI-INV = 0 OR
                                      (DATA-MOVTO-CH10 NOT <
                                       MOVTO-INI-INV AND
                                       DATA-MOVTO-CH10 NOT >
                                       MOVTO-FIM-INV)
                                       PERFORM FAZER-OUTRAS-COMP-CHQ
                                   END-IF
                             END-IF
                        END-READ
                    END-PERFORM
                 END-IF
              END-IF
           END-IF.
       LISTAR-CHEQUES-FIM.
           EXIT.

       FAZER-OUTRAS-COMPARACOES SECTION.
           MOVE SPACES TO GS-REGISTRO1
           STRING "Contas a receber . . ." x"0a"
                  "Portador -> " portador-cr20 x"0a"
                  REG-CRD020 INTO GS-REGISTRO1

           IF GS-VENDEDOR = 0 OR VENDEDOR-CR20
              IF GS-PORTADOR = 0 OR PORTADOR-CR20
                 IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                    IF AUX-CARTEIRA = 0 OR CARTEIRA-CR20
                        MOVE SEQ-W          TO GS-SEQ
                        PERFORM MOVER-DADOS-TIPO1.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       FAZER-OUTRAS-COMP-CHQ SECTION.
           MOVE SPACES TO GS-REGISTRO1
           STRING "Cheque . . ." x"0a"
                  "Portador -> " portador-ch10 x"0a"
                  REG-CHD010 INTO GS-REGISTRO1
           IF GS-VENDEDOR = 0 OR VENDEDOR-CH10
              IF GS-PORTADOR = 0 OR PORTADOR-CH10
                 IF AUX-CARTEIRA = 0 OR CARTEIRA-CH10
                    MOVE SEQ-W          TO GS-SEQ
                    PERFORM MOVER-DADOS-TIPO1-CH.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



       ATUALIZA-PORTADOR-RECEBER SECTION.
           SHOW-WINDOW WIN3

           CLOSE    CRD020 CHD010
           OPEN I-O CRD020 CHD010
           OPEN INPUT REMESSA
           PERFORM ABRE-ARQUIVO-ANOTACAO
           MOVE ZEROS TO FS-REMESSA
           PERFORM UNTIL FS-REMESSA = "10"
                READ REMESSA NEXT AT END
                     MOVE "10" TO FS-REMESSA
                NOT AT END
                     MOVE REG-REMESSA    TO GS-EXIBE-SEQ
                     MOVE "REFRESH-WIN3" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     MOVE REG-REMESSA             TO DETALHE-ARQUIVO

      *D-SEU-NUMERO(01:9) => CODIGO COMPLETO DO CLIENTE
      *D-SEU-NUMERO(10:3) => SEQUENCIA SEQ-CR20
      *D-SEU-NUMERO(13:3) => PARCELA

                     EVALUATE REMESSA-TIPO
      *>Contas a receber
                          WHEN 1
                                 MOVE REMESSA-CHAVE-ARQUIVO TO
                                      CHAVE-CR20
                                 READ CRD020 INVALID KEY
                                      MOVE SPACES TO MENSAGEM
                                      STRING "Contas a receber não encon
      -                                      "trado" x"0a"
                                             "COD-COMPL-CR20 = "
                                             COD-COMPL-CR20 x"0a"
                                             "SEQ-CR20 = "
                                             SEQ-CR20 INTO MENSAGEM
                                        MOVE "C" TO TIPO-MSG
                                      PERFORM EXIBIR-MENSAGEM
                                 NOT INVALID KEY
                                      PERFORM GRAVA-ANOTACAO
                                      MOVE GS-ACP-PORTADOR TO
                                           PORTADOR-CR20
                                      REWRITE REG-CRD020
                                      END-REWRITE
                                 END-READ
      *>Cheques
                          WHEN 2
                                 MOVE REMESSA-CHAVE-ARQUIVO(1:8) TO
                                      DATA-MOVTO-CH10
                                 MOVE REMESSA-CHAVE-ARQUIVO(9:4) TO
                                      SEQ-CH10
                                 READ CHD010 INVALID KEY
                                      MOVE SPACES TO MENSAGEM
                                      STRING "Cheque não encontrado"
                                            x"0a"
                                             "DATA-MOVTO-CH10 = "
                                              DATA-MOVTO-CH10 x"0a"
                                             "SEQ-CH10 = "
                                              SEQ-CH10 INTO MENSAGEM
                                       MOVE  "C" TO TIPO-MSG
                                      PERFORM EXIBIR-MENSAGEM
                                 NOT INVALID KEY
                                      MOVE COD-COMPL-CH10 TO
                                           COD-COMPL-CR20
                                      MOVE NR-CHEQUE-CH10 TO
                                           NR-DOCTO-CR20
                                      MOVE PORTADOR-CH10  TO
                                           PORTADOR-CR20
                                      PERFORM GRAVA-ANOTACAO
                                      MOVE GS-ACP-PORTADOR TO
                                           PORTADOR-CH10
                                      REWRITE REG-CHD010
                                      END-REWRITE
                                 END-READ
                     END-EVALUATE
                END-READ
           END-PERFORM
           CLOSE      CRD020 CHD010
           OPEN INPUT CRD020 CHD010
           MOVE "Portador Atualizado" TO MENSAGEM
           MOVE "C"                   TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           UNSHOW-WINDOW WIN3 PRINCIPAL
           SET-FOCUS EF4

           CLOSE REMESSA.
       ATUALIZA-SEQUENCIA SECTION.
           MOVE 1 TO VIRTUA-CODIGO
           READ VIRTUA INVALID KEY
                MOVE "Parâmetrização Não Realizada" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE SEQUENCIA-W TO VIRTUA-NOSSO-NUMERO
                REWRITE REG-VIRTUA INVALID KEY
                    MOVE "Erro de Regravação...VIRTUA" TO MENSAGEM
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
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU    TO DATA-MOVTO-CR200
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

           MOVE PORTADOR-CR20            TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"           TO NOME-PORT
           END-READ

           MOVE "S"                      TO OK

           IF GS-CPF = 1
              IF TIPO-PESSOA-CG11 = "J"
                 MOVE  CPF1-CG11 TO   LINK-CGC
                 CALL   "DIGCGC"      USING   LINKA-CGC
                 CANCEL "DIGCGC"
                 IF LINK-CGC-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CR20
                        WHEN 0  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              ELSE
                 MOVE  CPF1-CG11 TO   LINK-CPF
                 CALL   "DIGCPF"      USING   LINKA-CPF
                 CANCEL "DIGCPF"
                 IF LINK-CPF-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CR20
                        WHEN 0  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CR20(1:4) "-"
                                        CLIENTE-CR20(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              END-IF
           END-IF

           IF GS-CEP = 1
              IF CEP1-CG11 < 1000000
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

              MOVE COD-COMPL-CG10           TO COD-COMPL-CG14
              READ CGD014 INVALID KEY
                   INITIALIZE REG-CGD014
              END-READ

              MOVE CODIGO-CG10             TO ALBUMMT19
              READ MTD019 INVALID KEY
                   INITIALIZE REG-MTD019
              END-READ


              ADD 1                         TO GS-QTDE-TITULO
              ADD VALOR-TOT-CR20            TO GS-VALOR-TOTAL


              ADD 1                         TO SEQ-W
              ADD 1                         TO SEQUENCIA-W

              initialize indice
              invoke gs-acp-listview "adicionarItem" returning wsItem
      *>Tipo
              add 1 to indice
              initialize wsTexto
              string "1" X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Nr Operação
              add 1 to indice
              initialize wsTexto
              string class-cliente-cr20
                     cliente-cr20
                     seq-cr20 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Nome Operação
              add 1 to indice
              initialize wsTexto
              string "Contas a Receber" X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Agência
              add 1 to indice
              initialize wsTexto
              string  X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Conta
              add 1 to indice
              initialize wsTexto
              string cod-compl-cr20 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Produto
              add 1 to indice
              initialize wsTexto
              string nome-port x"00" DELIMITED BY "  "  into wstexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Data Atualizado
              add 1 to indice
              initialize wsTexto
              string  ws-dia-cpu "/"
                      ws-mes-cpu "/"
                      ws-ano-cpu X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Data Vencimento
              add 1 to indice
              initialize wsTexto
              string  DATA-VENCTO-CR20(7:2) "/"
                      DATA-VENCTO-CR20(5:2) "/"
                      DATA-VENCTO-CR20(1:4) X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Valor Operação
              add 1 to indice
              initialize wsTexto
              move valor-tot-cr20 to masc-valor
              string masc-valor X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Valor Vencido
              add 1 to indice
              initialize wsTexto
              move valor-saldo-cr20 to masc-valor
              string masc-valor X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Condições negociáveis
              add 1 to indice
              initialize wsTexto
              string nome-form-mt19 x"00" delimited by "   "
              into wstexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF/CNPJ
              add 1 to indice
              initialize wsTexto
              string cpf1-cg11(6:11) X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>MCI
              add 1 to indice
              initialize wsTexto
              string cod-compl-cr20 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NR FICHA
              add 1 to indice
              initialize wsTexto
              string nr-docto-cr20 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DO CLIENTE
              add 1 to indice
              initialize wsTexto
              string comprador-cg10 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO
              add 1 to indice
              initialize wsTexto
              string endereco1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO
              add 1 to indice
              initialize wsTexto
              string bairro1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP
              add 1 to indice
              initialize wsTexto
              string cep1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE
              move cidade1-cg11 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read

              add 1 to indice
              initialize wsTexto
              string nome-cid X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF
              add 1 to indice
              initialize wsTexto
              string uf-cid X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE1
              add 1 to indice
              initialize wsTexto
              if fone1-cg11 > 0
                 if ddd-cid > 0
                    string ddd-cid(3:2) "-" fone1-cg11 X"00"
                           delimited by "  " into wsTexto
                 else
                    string fone1-cg11 X"00"
                           delimited by "  " into wsTexto
                 end-if
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE2
              add 1 to indice
              initialize wsTexto
              if fax1-cg11 > 0
                 if ddd-cid > 0
                    string ddd-cid(3:2) "-" fax1-cg11 X"00"
                           delimited by "  " into wsTexto
                 else
                    string fax1-cg11 X"00"
                           delimited by "  " into wsTexto
                 end-if
              else
                 string x"00" into wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE3
              add 1 to indice
              initialize wsTexto
              if celular1-cg11 > 0
                 if ddd-celular1-cg11 > 0
                    string ddd-celular1-cg11 "-" celular1-cg11 x"00"
                           delimited by "  " into wsTexto
                 else
                    string celular1-cg11 x"00"
                           delimited by "  " into wsTexto
                 end-if
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE4
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE5
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE6
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DATA NASCIMENTO
              add 1 to indice
              initialize wsTexto
              if data-nasc1-cg11 > 0
                 string data-nasc1-cg11(7:2) "/"
                        data-nasc1-cg11(5:2) "/"
                        data-nasc1-cg11(1:4) x"00"
                        delimited by "  " into wsTexto
              else
                 move x"00" to wstexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NATURALIDADE
              add 1 to indice
              initialize wsTexto
              string x"00"
                     delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>SEXO
              add 1 to indice
              initialize wsTexto
              string sexo1-cg11 x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ESTADO CIVIL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DO PAI
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-pai-cg11) to nome-pai-cg11
              string nome-pai-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DA MAE
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-mae-cg11) to nome-mae-cg11
              string nome-mae-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME AVALISTA
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-cg14) to nome-cg14
              string nome-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF AVALISTA
              add 1 to indice
              initialize wsTexto
              if cpf-cg14 > 0
                 string cpf-cg14(4:11) x"00" into wsTexto
              else
                 string x"00" into wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO AVALISTA
              add 1 to indice
              initialize wsTexto
              string endereco-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO AVALISTA
              add 1 to indice
              initialize wsTexto
              string bairro-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP AVALISTA
              add 1 to indice
              initialize wsTexto
              if cep-cg14 > 0
                 string cep-cg14 x"00" delimited by "   " into wsTexto
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE AVALISTA
              move cidade-cg14 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read
              add 1 to indice
              initialize wsTexto
              string nome-cid x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF AVALISTA
              add 1 to indice
              initialize wsTexto
              string uf-cid x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 AVALISTA
              add 1 to indice
              initialize wsTexto
              IF COMP-TEL-AVAL-CG14 > 0
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" COMP-TEL-AVAL-CG14
                           TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 else
                    string COMP-TEL-AVAL-CG14
                           TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 end-if
              ELSE
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 else
                    string TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 end-if
              END-IF
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 AVALISTA
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>PROFISSAO
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              string empresa-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO TRABALHO
              add 1 to indice
              initialize wsTexto
              string endereco3-cg11 x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO TRABALHO
              add 1 to indice
              initialize wsTexto
              string bairro3-cg11 x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP TRABALHO
              add 1 to indice
              initialize wsTexto
              if cep3-cg11 > 0
                 string cep3-cg11 x"00" delimited by "   "
                   into wsTexto
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE TRABALHO
              move cidade3-cg11 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read
              add 1 to indice
              initialize wsTexto
              string nome-cid x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF TRABALHO
              add 1 to indice
              initialize wsTexto
              string uf-cid x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              IF FONE3-CG11 > 0
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" FONE3-CG11 x"00"
                           delimited by "   " into wsTexto
                 else
                    string FONE3-CG11 x"00"
                           delimited by "   " into wsTexto
                 end-if
              ELSE
                 string x"00" into wsTexto
              END-IF
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>SPC/SERASA
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>EMAIL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DT EMISSAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>VALOR PROTESTO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>OBSERVACAO OPERACAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DT FIM TERCERIZACAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>VALOR JUROS
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto


              INITIALIZE REG-REMESSA
              MOVE 1                   TO REMESSA-TIPO
              MOVE CHAVE-CR20          TO REMESSA-CHAVE-ARQUIVO
              MOVE COMPRADOR-CG10      TO REMESSA-NOME
              MOVE ENDERECO1-CG11      TO REMESSA-ENDERECO
              MOVE BAIRRO1-CG11        TO REMESSA-BAIRRO
              MOVE CIDADE1-CG11        TO CIDADE
              READ CAD010 INVALID KEY
                   INITIALIZE REG-CAD010
              END-READ
              MOVE NOME-CID            TO REMESSA-CIDADE
              MOVE UF-CID              TO REMESSA-UF
              MOVE CEP1-CG11           TO REMESSA-CEP
              MOVE NR-DOCTO-CR20       TO REMESSA-NUMERO
              MOVE VALOR-TOT-CR20      TO REMESSA-VALOR

              WRITE REG-REMESSA INVALID KEY
                  MOVE "Erro de gravação...REMESSA" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       MOVER-DADOS-TIPO1-CH SECTION.
      *>SEQUENCIA NOSSO NUMERO (BANCO)
           MOVE COD-COMPL-CH10         TO COD-COMPL-CG10
                                          COD-COMPL-CG11
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010
           END-READ

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011
           END-READ

           MOVE CODIGO-CG10             TO ALBUMMT19
           READ MTD019 INVALID KEY
                INITIALIZE REG-MTD019
           END-READ

           MOVE PORTADOR-CH10            TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "*******"           TO NOME-PORT
           END-READ

           MOVE "S"                      TO OK

           IF GS-CPF = 1
              IF TIPO-PESSOA-CG11 = "J"
                 MOVE  CPF1-CG11 TO   LINK-CGC
                 CALL   "DIGCGC"      USING   LINKA-CGC
                 CANCEL "DIGCGC"
                 IF LINK-CGC-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CH10
                        WHEN 0  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CH10(1:4) "-"
                                        CLIENTE-CH10(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CNPJ Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CH10(1:4) "-"
                                        CLIENTE-CH10(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              ELSE
                 MOVE  CPF1-CG11 TO   LINK-CPF
                 CALL   "DIGCPF"      USING   LINKA-CPF
                 CANCEL "DIGCPF"
                 IF LINK-CPF-CONF       =       1
                    MOVE "N"                TO OK
                    MOVE SPACES             TO MENSAGEM
                    EVALUATE CLASS-CLIENTE-CH10
                        WHEN 0  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Contrato" " | "
                                        CLIENTE-CH10(1:4) "-"
                                        CLIENTE-CH10(5:4) INTO MENSAGEM

                        WHEN 1  STRING "Cliente com o CPF Inválido"
                                       " | "
                                       "Comum" " | "
                                        CLIENTE-CH10(1:4) "-"
                                        CLIENTE-CH10(5:4) INTO MENSAGEM
                    END-EVALUATE
                    MOVE MENSAGEM TO GS-LINDET
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              END-IF
           END-IF

           IF GS-CEP = 1
              IF CEP1-CG11 < 1000000
                 MOVE "N"                TO OK
                 MOVE SPACES             TO MENSAGEM
                 EVALUATE CLASS-CLIENTE-CH10
                     WHEN 0  STRING "Cliente com o CEP Inválido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CH10(1:4) "-"
                                     CLIENTE-CH10(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o CEP Inválido" " | "
                                    "Comum" " | "
                                     CLIENTE-CH10(1:4) "-"
                                     CLIENTE-CH10(5:4) INTO MENSAGEM
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
                 EVALUATE CLASS-CLIENTE-CH10
                     WHEN 0  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Contrato" " | "
                                     CLIENTE-CH10(1:4) "-"
                                     CLIENTE-CH10(5:4) INTO MENSAGEM

                     WHEN 1  STRING "Cliente com o ENDEREÇO ou BAIRRO In
      -                             "válido" " | "
                                    "Comum" " | "
                                     CLIENTE-CH10(1:4) "-"
                                     CLIENTE-CH10(5:4) INTO MENSAGEM
                 END-EVALUATE
                 MOVE MENSAGEM TO GS-LINDET
                 MOVE "INSERE-LIST2" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-IF

           IF OK = "S"

              MOVE COD-COMPL-CG10           TO COD-COMPL-CG14
              READ CGD014 INVALID KEY
                   INITIALIZE REG-CGD014
              END-READ


              ADD 1                         TO GS-QTDE-TITULO
              ADD VALOR-CH10                TO GS-VALOR-TOTAL


              ADD 1                         TO SEQ-W
              ADD 1                         TO SEQUENCIA-W

              initialize indice
              invoke gs-acp-listview "adicionarItem" returning wsItem
      *>Tipo
              add 1 to indice
              initialize wsTexto
              string "2" X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Nr Operação
              add 1 to indice
              initialize wsTexto
              string data-movto-ch10 seq-ch10 x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Nome Operação
              add 1 to indice
              initialize wsTexto
              string "Cheque" X"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *       move agencia-ch10 to masc-agencia
      *>Agência
              add 1 to indice
              initialize wsTexto
              string X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *       move conta-corr-ch10 to masc-conta
      *>Conta
              add 1 to indice
              initialize wsTexto
              string cod-compl-ch10 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Produto
              add 1 to indice
              initialize wsTexto
              string nome-port X"00" delimited by "  "  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Data Atualizado
              add 1 to indice
              initialize wsTexto
              string  ws-dia-cpu "/"
                      ws-mes-cpu "/"
                      ws-ano-cpu X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Data Vencimento
              add 1 to indice
              initialize wsTexto
              string  DATA-VENCTO-CH10(7:2) "/"
                      DATA-VENCTO-CH10(5:2) "/"
                      DATA-VENCTO-CH10(1:4) X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Valor Operação
              add 1 to indice
              initialize wsTexto
              move valor-ch10 to masc-valor
              string masc-valor X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Valor Vencido
              add 1 to indice
              initialize wsTexto
              move valor-saldo-ch10 to masc-valor
              string masc-valor X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>Condições negociáveis
              add 1 to indice
              initialize wsTexto

              string nome-form-mt19 x"00" delimited by "   "
                into wstexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF/CNPJ
              add 1 to indice
              initialize wsTexto
              string cpf1-cg11(6:11) X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>MCI
              add 1 to indice
              initialize wsTexto
              string cod-compl-ch10 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NR FICHA
              add 1 to indice
              initialize wsTexto
              string nr-cheque-ch10 X"00"  into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DO CLIENTE
              add 1 to indice
              initialize wsTexto
              string comprador-cg10 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO
              add 1 to indice
              initialize wsTexto
              string endereco1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO
              add 1 to indice
              initialize wsTexto
              string bairro1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP
              add 1 to indice
              initialize wsTexto
              string cep1-cg11 X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE
              move cidade1-cg11 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read

              add 1 to indice
              initialize wsTexto
              string nome-cid X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF
              add 1 to indice
              initialize wsTexto
              string uf-cid X"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE1
              add 1 to indice
              initialize wsTexto
              if fone1-cg11 > 0
                 if ddd-cid > 0
                    string ddd-cid(3:2) "-" fone1-cg11 X"00"
                           delimited by "  " into wsTexto
                 else
                    string fone1-cg11 X"00"
                           delimited by "  " into wsTexto
                 end-if
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE2
              add 1 to indice
              initialize wsTexto
              if fax1-cg11 > 0
                 if ddd-cid > 0
                    string ddd-cid(3:2) "-" fax1-cg11 X"00"
                           delimited by "  " into wsTexto
                 else
                    string fax1-cg11 X"00"
                           delimited by "  " into wsTexto
                 end-if
              else
                 string x"00" into wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE3
              add 1 to indice
              initialize wsTexto
              if celular1-cg11 > 0
                 string ddd-celular1-cg11 "-" celular1-cg11 x"00"
                        delimited by "  " into wsTexto
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE4
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE5
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE6
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DATA NASCIMENTO
              add 1 to indice
              initialize wsTexto
              if data-nasc1-cg11 > 0
                 string data-nasc1-cg11(7:2) "/"
                        data-nasc1-cg11(5:2) "/"
                        data-nasc1-cg11(1:4) x"00"
                        delimited by "  " into wsTexto
              else
                 move x"00" to wstexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NATURALIDADE
              add 1 to indice
              initialize wsTexto
              string x"00"
                     delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>SEXO
              add 1 to indice
              initialize wsTexto
              string sexo1-cg11 x"00" delimited by "  " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ESTADO CIVIL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DO PAI
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-pai-cg11) to nome-pai-cg11
              string nome-pai-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME DA MAE
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-mae-cg11) to nome-mae-cg11
              string nome-mae-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME AVALISTA
              add 1 to indice
              initialize wsTexto
              move function upper-case(nome-cg14) to nome-cg14
              string nome-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF AVALISTA
              add 1 to indice
              initialize wsTexto
              if cpf-cg14 > 0
                 string cpf-cg14(4:11) x"00" into wsTexto
              else
                 string x"00" into wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO AVALISTA
              add 1 to indice
              initialize wsTexto
              string endereco-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO AVALISTA
              add 1 to indice
              initialize wsTexto
              string bairro-cg14 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP AVALISTA
              add 1 to indice
              initialize wsTexto
              if cep-cg14 > 0
                 string cep-cg14 x"00" delimited by "   " into wsTexto
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE AVALISTA
              move cidade-cg14 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read
              add 1 to indice
              initialize wsTexto
              string nome-cid x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF AVALISTA
              add 1 to indice
              initialize wsTexto
              string uf-cid x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 AVALISTA
              add 1 to indice
              initialize wsTexto
              IF COMP-TEL-AVAL-CG14 > 0
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" COMP-TEL-AVAL-CG14
                           TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 else
                    string COMP-TEL-AVAL-CG14
                           TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 end-if
              ELSE
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 else
                    string TELEFONE-CG14 x"00"
                           delimited by "   " into wsTexto
                 end-if
              END-IF
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 AVALISTA
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CPF AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 AVALISTA 2
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>PROFISSAO
              add 1 to indice
              initialize wsTexto
              string x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>NOME LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              string empresa-cg11 x"00" delimited by "   " into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>ENDEREÇO TRABALHO
              add 1 to indice
              initialize wsTexto
              string endereco3-cg11 x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>BAIRRO TRABALHO
              add 1 to indice
              initialize wsTexto
              string bairro3-cg11 x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CEP TRABALHO
              add 1 to indice
              initialize wsTexto
              if cep3-cg11 > 0
                 string cep3-cg11 x"00" delimited by "   "
                   into wsTexto
              else
                 move x"00" to wsTexto
              end-if
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>CIDADE TRABALHO
              move cidade3-cg11 to cidade
              read cad010 invalid key
                   initialize reg-cad010
              end-read
              add 1 to indice
              initialize wsTexto
              string nome-cid x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>UF TRABALHO
              add 1 to indice
              initialize wsTexto
              string uf-cid x"00" delimited by "   "
                into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              IF FONE3-CG11 > 0
                 if ddd-cid > 0
                    string DDD-cid(3:2) "-" FONE3-CG11 x"00"
                           delimited by "   " into wsTexto
                 else
                    string FONE3-CG11 x"00"
                           delimited by "   " into wsTexto
                 end-if
              ELSE
                 string x"00" into wsTexto
              END-IF
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 LOCAL DE TRABALHO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA PESSOAL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA 2
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>REFERENCIA PESSOAL 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 1 REFERENCIA 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>TELEFONE 2 REFERENCIA 3
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>SPC/SERASA
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>EMAIL
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DT EMISSAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>VALOR PROTESTO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>OBSERVACAO OPERACAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>DT FIM TERCERIZACAO
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto
      *>VALOR JUROS
              add 1 to indice
              initialize wsTexto
              string x"00" into wsTexto
              invoke gs-acp-listview "preencherColunaZ"
                using wsItem lnkcolunas(indice) wsTexto


              INITIALIZE REG-REMESSA
              MOVE 2                   TO REMESSA-TIPO
              STRING DATA-MOVTO-CH10 SEQ-CH10 INTO REMESSA-CHAVE-ARQUIVO
              MOVE COMPRADOR-CG10      TO REMESSA-NOME
              MOVE ENDERECO1-CG11      TO REMESSA-ENDERECO
              MOVE BAIRRO1-CG11        TO REMESSA-BAIRRO
              MOVE CIDADE1-CG11        TO CIDADE
              READ CAD010 INVALID KEY
                   INITIALIZE REG-CAD010
              END-READ
              MOVE NOME-CID            TO REMESSA-CIDADE
              MOVE UF-CID              TO REMESSA-UF
              MOVE CEP1-CG11           TO REMESSA-CEP
              MOVE NR-CHEQUE-CH10      TO REMESSA-NUMERO
              MOVE VALOR-CH10          TO REMESSA-VALOR

              WRITE REG-REMESSA INVALID KEY
                  MOVE "Erro de gravação...REMESSA" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.


       CARREGA-LISTA SECTION.
           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.

      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    OPEN INPUT REMES.
      *    MOVE SPACES TO GS-LINDET.
      *    MOVE ZEROS TO ST-REM GS-SEQ.
      *    PERFORM UNTIL ST-REM = "10"
      *         READ REMES AT END
      *              MOVE "10" TO ST-REM
      *         NOT AT END
      *              MOVE REG-REMES        TO DETALHE-ARQUIVO
      *              ADD 1                 TO GS-SEQ
      *              MOVE "REFRESH-DATA"   TO DS-PROCEDURE
      *              PERFORM CALL-DIALOG-SYSTEM
      *
      *              MOVE REG-REMES         TO GS-LINDET
      *              MOVE "INSERE-LIST" TO DS-PROCEDURE
      *              PERFORM CALL-DIALOG-SYSTEM
      *         END-READ
      *    END-PERFORM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    CLOSE REMES.
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
           OPEN INPUT REMESSA
           MOVE ZEROS TO SEQ-WK
           MOVE ZEROS TO ST-REM
           PERFORM UNTIL ST-REM = "10"
                READ REMESSA NEXT AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REMESSA-NOME     TO NOME-WK
                     MOVE REMESSA-ENDERECO TO ENDERECO-WK
                     MOVE REMESSA-BAIRRO   TO BAIRRO-WK
                     MOVE REMESSA-CIDADE   TO CIDADE-WK
                     MOVE REMESSA-UF       TO UF-WK
                     MOVE REMESSA-CEP      TO CEP-WK
                     MOVE REMESSA-NUMERO   TO DOCTO-WK
                     MOVE REMESSA-VALOR    TO VALOR-WK
                     MOVE REMESSA-TIPO     TO TIPO-WK
                     WRITE REG-WORK
                     END-WRITE
                END-READ
           END-PERFORM.


           copy condensa.

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
                      MOVE TIPO-WK           TO TIPO-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE GS-VALOR-TOTAL     TO VALOR-TOTAL-REL
           MOVE QTDE-TIT-T2        TO QTDE-TIT-TOTAL-REL
           WRITE REG-RELAT FROM LINDET1 AFTER 3
           CLOSE REMESSA WORK
           DELETE FILE WORK.

           copy descondensa.

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
           MOVE "VIRTUA"    TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           open i-o acesso

           move 8 to acesso-codigo
           read acesso not invalid key
                delete acesso invalid key
                    move "Erro de Exclusão...ACESSO" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem
                end-delete
           end-read

           close    acesso


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "VIRTUA"            to logacess-programa
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

           CLOSE CRD020  CGD010 CGD011 CAD010 CAD018 CAD001 CGD001
                 CRD020X CGD014 CHD010 MTD019
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
