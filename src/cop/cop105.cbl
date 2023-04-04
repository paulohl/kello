       COPY DSLANG.CPY.
        IDENTIFICATION DIVISION.
       PROGRAM-ID. COP105.
      *AUTHOR: MARELI AMANCIO VOLPATO
      *DATA: 17/11/1999
      *DESCRIÇÃO: Avaliacao Individual
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
           COPY CGPX011.
           COPY CGPX012.
           COPY COPX001.
           COPY COPX002.
           COPY COPX003.
           COPY COPX005.
           COPY COPX006.
           COPY COPX040.
           COPY COPX050.
           COPY CAPX010.
           COPY CAPX012.
           COPY IEPX010.
           COPY REPX030.
           COPY REPX300.
           COPY REPX301.
           COPY REPX302.
           COPY REPX303.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS DOCTO-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW012.
       COPY COPW001.
       COPY COPW002.
       COPY COPW003.
       COPY COPW005.
       COPY COPW006.
       COPY COPW040.
       COPY COPW050.
       COPY CAPW010.
       COPY CAPW012.
       COPY IEPW010.
       COPY REPW030.
       COPY REPW300.
       COPY REPW301.
       COPY REPW302.
       COPY REPW303.
       FD  WORK.
       01  REG-WORK.
           05  DOCTO-WK         PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP105.CPB".
           COPY "COP105.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CBDATA.CPY".
           COPY "CPDIAS1.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD006             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-RED030             PIC XX       VALUE SPACES.
           05  ST-RED300             PIC XX       VALUE SPACES.
           05  ST-RED301             PIC XX       VALUE SPACES.
           05  ST-RED302             PIC XX       VALUE SPACES.
           05  ST-RED303             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  NR-CONTRATO-W         PIC 9(4)     VALUE ZEROS.
           05  NR-INIC-PAD           PIC X(6)     VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  COMISSAO  OCCURS 3 TIMES PIC X(30).
           05  FONE      OCCURS 3 TIMES PIC 9(8).
           05  CUSTO-TOTAL           PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  TOTAL-FORMANDO        PIC 9(8)     VALUE ZEROS.
           05  QTDE-CONTRATO         PIC 9(2)     VALUE ZEROS.
           05  PERC-FORMANDO         PIC 9(3)V9(6) VALUE ZEROS.
           05  ITEM-W                PIC 9(3)      VALUE ZEROS.
           05  DESPESAS-W            PIC 9(8)V99   VALUE ZEROS.
           05  QTDE-ITEM             PIC 9(3)      VALUE ZEROS.
           05  QTDE-EVENTO           PIC 9(2)      VALUE ZEROS.
           05  ULT-EVENTO            PIC 9(5)      VALUE ZEROS.
           05  TOTAL-W               PIC S9(8)V99  VALUE ZEROS.
           05  TOTAL-REPORTAGEM      PIC 9(8)V99   VALUE ZEROS.
           05  TOTAL-IND-W           PIC 9(8)V99   VALUE ZEROS.
           05  TOTAL-JUROS-W         PIC 9(8)V99   VALUE ZEROS.
           05  TOTAL-GERAL-W         PIC 9(8)V99   VALUE ZEROS.
           05  PREV-VENDAS-G         PIC 9(8)V99   VALUE ZEROS.
           05  PATROCINIO-G          PIC 9(8)V99   VALUE ZEROS.
           05  REPORTAGEM-G          PIC 9(8)V99   VALUE ZEROS.
           05  PERC-W                PIC 9(3)V99   VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  FONE-E                PIC ZZZZBZZZZ.
           05  TAXA-E                PIC ZZ,ZZ.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  QTDE4-E               PIC ZZZZ.
           05  QTDE5-E               PIC ZZZZZ.
           05  QTDE-W                PIC 9(5)      VALUE ZEROS.
           05  OBJ-VENDAS-W          PIC 9(8)V99   VALUE ZEROS.
           05  OBJ-VENDAS-TOT        PIC 9(8)V99   VALUE ZEROS.

           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  MESES-W               PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 9        VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  DDD-W                 PIC 9(3)     VALUE ZEROS.
      *    VARIAVEIS P/ CONTATO
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-ITEM              PIC 9(2)     VALUE ZEROS.
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

      *------------------------------------------------------
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "AVALIACAO INDIVIDUAL DE CONTRATO   -                   ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 EMISSAO-REL                    PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 HORA-REL                       PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ":".
           05 MINUTO-REL                     PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE "h (".
           05 USUARIO-REL                    PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(040) VALUE
              ")                                   PG: ".
           05 PAG-REL                        PIC  9(002) VALUE ZEROS.
       02  LINHA-02.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(128) VALUE ALL "=".
       02  LINHA-03.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ORIGEM-REL                     PIC  X(003) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 CONTRATO-REL                   PIC  9(004) VALUE ZEROS.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 IDENTIFICACAO-REL              PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(022) VALUE SPACES.
           05 INSTITUICAO-REL                PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 CIDADE-REL                     PIC  X(013) VALUE SPACES.
           05 FILLER                         PIC  X(025) VALUE SPACES.
           05 MESANO-REL                     PIC  99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FORM-REL                       PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PADRAO-REL                     PIC  X(002) VALUE SPACES.
       02  LINHA-04.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 STATUS-REL                     PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(028) VALUE SPACES.
           05 MENSAGEM-REL                   PIC  X(040) VALUE SPACES.
           05 FILLER                         PIC  X(013) VALUE SPACES.
           05 SERVICO-REL                    PIC  X(005) VALUE SPACES.
       02  LINHA-05.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 REPRESENT-REL                  PIC  X(019) VALUE SPACES.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 REGIAO-REL                     PIC  X(012) VALUE SPACES.
           05 FILLER                         PIC  X(016) VALUE SPACES.
           05 ASSINATURA-REL                 PIC  99/99/9999.
           05 FILLER                         PIC  X(052) VALUE
              "                                           inicial:(".
           05 FORM-INI-REL                   PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PADRAO-INI-REL                 PIC  X(002) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE ")".
       02  LINHA-06.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE
              "multa: ".
           05 MULTA-REL                      PIC  X(20)  VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 FORUM-REL                      PIC  X(013) VALUE SPACES.
           05 FILLER                         PIC  X(015) VALUE SPACES.
           05 COMERCIAL-REL                  PIC  X(030) VALUE SPACES.
       02  LINHA-07.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Organizador:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 ORGANIZADOR-REL                PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD1-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE1-REL                      PIC  9999B99999.
       02  LINHA-08.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Comissao...:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 COMISSAO2-REL                  PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD2-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE2-REL                      PIC  9999B99999.
       02  LINHA-09.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Comissao...:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 COMISSAO3-REL                  PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD3-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE3-REL                      PIC  9999B99999.
       02  LINHA-10.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Comissao...:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 COMISSAO4-REL                  PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD4-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE4-REL                      PIC  9999B99999.
       02  LINHA-11.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Convites...:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CONVITE5-REL                   PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD5-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE5-REL                      PIC  9999B99999.
       02  LINHA-12.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Beca.......:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 BECA6-REL                      PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD6-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE6-REL                      PIC  9999B99999.
       02  LINHA-13.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Outro......:                                           ".
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 OUTRO7-REL                     PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "                       (".
           05 DDD7-REL                       PIC  9(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ")".
           05 FONE7-REL                      PIC  9999B99999.
       02  LINHA-14.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(020) VALUE
              "VENDA PREVISTA PARA ".
           05 DATA-PREV-REL                  PIC  99/99/9999.
       02  LINHA-15.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(128) VALUE ALL "*".
       02  LINHA-16.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE
              "PREVISAO DE VENDAS".
       02  LINHA-17.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "item                         R$unit   indice      qtde ".
           05 FILLER                         PIC  X(017) VALUE
              "     R$obj.vendas".
       02  LINHA-18.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(072) VALUE ALL "=".
       02  LINHA-19.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM1-REL                      PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 VLR-UNIT1-REL                  PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 INDICE1-REL                    PIC  Z(003) VALUE ZEROS.
           05 FILLER                         PIC  X(005) VALUE "%    ".
           05 QTDE1-REL                      PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 OBJ-VENDA1-REL                 PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-20.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "VENDAS ANTECIPADAS DE FOTOGRAFIAS E VIDEO (correcao de ".
           05 PERC2-REL                      PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(029) VALUE
              "% ate data prevista de venda)".
       02  LINHA-21.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "it         Data             Valor       Dias           ".
           05 FILLER                         PIC  X(048) VALUE
              "       Juros             Total        Observacao".
       02  LINHA-22.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(123) VALUE ALL "=".
       02  LINHA-23.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM2-REL                      PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 DATA2-REL                      PIC  99/99/9999.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 VALOR2-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 DIAS2-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(010) VALUE SPACES.
           05 JUROS2-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 TOTAL2-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(008) VALUE SPACES.
           05 OBS2-REL                       PIC  X(030) VALUE SPACES.
       02  LINHA-24.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(032) VALUE
              "CUSTOS COM BRINDES (correcao de ".
           05 PERC3-REL                      PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(029) VALUE
              "% ate data prevista da venda)".
       02  LINHA-25.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(044) VALUE
              "Item Descricao            Qtde  Form       R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(012) VALUE
              "Unit       R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(041) VALUE
              "Total   Vencimento  Pagamento Dias      R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(019) VALUE
              "Juros   Custo-Total".
       02  LINHA-26.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(122) VALUE ALL "=".
       02  LINHA-27.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM3-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACES.
           05 DESCRICAO3-REL                 PIC  X(019) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 QTDE3-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 FORM3-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VLR-UNIT3-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 TOTAL3-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 VENCTO3-REL                    PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PAGTO3-REL                     PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DIAS3-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 JUROS3-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CUSTO-TOTAL3-REL               PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-28.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(036) VALUE
              "CUSTOS COM REPORTAGENS (correcao de ".
           05 PERC4-REL                      PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(029) VALUE
              "% ate data prevista da venda)".
       02  LINHA-29.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(049) VALUE
              "Item Evento               Form       Data Nr.rela".
           05 FILLER                         PIC  X(008) VALUE
              "t      R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(053) VALUE
              "Total Coordenador    Dias         Juros   Custo-total".
       02  LINHA-30.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(118) VALUE ALL "=".
       02  LINHA-31.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM4-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACES.
           05 EVENTO4-REL                    PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FORM4-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DATA4-REL                      PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NR-RELAT4-REL                  PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TOTAL4-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 COORDEN4-REL                   PIC  X(014) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DIAS4-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 JUROS4-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CUSTO-TOTAL4-REL               PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-32.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(030) VALUE
              "PREVISAO DE CUSTOS DE PRODUCAO".
       02  LINHA-33.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(050) VALUE
              "Itens                Cod. Tipo                   R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(040) VALUE
              "Unitario   Qtde   Custo-total Observacao".
       02  LINHA-34.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(111) VALUE ALL "=".
       02  LINHA-35.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITENS5-REL                     PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CODIGO5-REL                    PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TIPO5-REL                      PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VLR-UNIT5-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 QTDE5-REL                      PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CUSTO-TOTAL5-REL               PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 OBS5-REL                       PIC  X(029) VALUE SPACES.
       02  LINHA-36.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(033) VALUE
              "VENDA DE ORGANIZACAO DE EVENTOS/A".
           05 FILLER                         PIC  X(002) VALUE ALL "R".
           05 FILLER                         PIC  X(022) VALUE
              "ECADACAO (correcao de ".
           05 PERC6-REL                      PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%)".
       02  LINHA-37.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "It Descricao                             Valor Dias    ".
           05 FILLER                         PIC  X(024) VALUE
              "    Juros          Total".
       02  LINHA-38.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(079) VALUE ALL "=".
       02  LINHA-39.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM6-REL                      PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DESCRICAO6-REL                 PIC  X(029) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VALOR6-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DIAS6-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 JUROS6-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TOTAL6-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-40.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(032) VALUE
              "CUSTOS DE ORGANIZACAO DE EVENTOS".
           05 FILLER                         PIC  X(002) VALUE "/C".
           05 FILLER                         PIC  X(022) VALUE
              "OMPROMISSOS (correcao ".
           05 PERC7-REL                      PIC  ZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%)".
       02  LINHA-41.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(046) VALUE
              "It Descricao                     Qtde Form   R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(015) VALUE
              "Unitario      R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(039) VALUE
              "Total     Vencto  Pagamento Dias      R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(019) VALUE
              "Juros   Custo-total".
       02  LINHA-42.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(125) VALUE ALL "=".
       02  LINHA-43.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM7-REL                      PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DESCRICAO7-REL                 PIC  X(029) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 QTDE7-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FORM7-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VLR-UNIT7-REL                  PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TOTAL7-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VENCTO7-REL                    PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PAGTO7-REL                     PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DIAS7-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 JUROS7-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CUSTO-TOTAL7-REL               PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-44.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(044) VALUE
              "CUSTOS DE ORGANIZACAO DE EVENTOS(relatorios)".
       02  LINHA-45.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "Item Descricao                           Data Nr-relat ".
           05 FILLER                         PIC  X(006) VALUE
              "     R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(033) VALUE
              "Total Nome            Dias      R".
           05 FILLER                         PIC  X(002) VALUE "$-".
           05 FILLER                         PIC  X(019) VALUE
              "Juros   Custo-total".
       02  LINHA-46.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  X(117) VALUE ALL "=".
       02  LINHA-47.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 ITEM8-REL                      PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 DESCRICAO8-REL                 PIC  X(029) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DATA8-REL                      PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NR-RELAT8-REL                  PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TOTAL8-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 NOME8-REL                      PIC  X(015) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DIAS8-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 JUROS8-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TOTAL8-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-48.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  XX     VALUE "I ".
           05 FILLER                         PIC  X(038) VALUE ALL "-".
           05 FILLER                         PIC  X(017) VALUE
              " AVALIACAO GERAL ".
           05 FILLER                         PIC  X(038) VALUE ALL "-".
           05 FILLER                         PIC  XX     VALUE "I ".
       02  LINHA-49.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  XX     VALUE "I ".
           05 DESCRICAO9-REL                 PIC  X(041) VALUE SPACES.
           05 FILLER                         PIC  X(005) VALUE SPACES.
           05 VALOR9-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ-.
           05 FILLER                         PIC  X(006) VALUE SPACES.
           05 TOTAL9-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ-.
           05 FILLER                         PIC  X(005) VALUE "    (".
           05 PERC9-REL                      PIC  ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE "%)".
           05 FILLER                         PIC  XX     VALUE "I ".
       01  LINHA-50.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  XX     VALUE "I ".
           05 FILLER                         PIC  X(93)  VALUE ALL "-".
           05 FILLER                         PIC  XX     VALUE "I ".
       01  LINHA-51.
           05 FILLER                         PIC  X(4)   VALUE SPACES.
           05 FILLER                         PIC  XX     VALUE "I ".
           05 FILLER                         PIC  X(93)  VALUE SPACES.
           05 FILLER                         PIC  XX     VALUE "I ".
      *------------------------------------------------------------
       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV       TO EMISSAO-REL.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO DATA-DIA-I
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "IED010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD006" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD006.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "111" TO EMP-REC.
           MOVE "RED030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED030.
           MOVE "RED300" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED300.
           MOVE "RED301" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED301.
           MOVE "RED302" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED302.
           MOVE "RED303" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED303.
           MOVE ZEROS TO ERRO-W.
           OPEN INPUT CAD010 IED010 CGD010 COD001 COD002
                      COD003 COD040 COD050 CAD012 CGD001 COD005.
           OPEN I-O COD006.

           IF ST-COD006 = "35"  CLOSE COD006  OPEN OUTPUT COD006
                                CLOSE COD006  OPEN I-O COD006.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD006 <> "00"
              MOVE "ERRO ABERTURA COD006: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD006 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-CARREGAR-DADOS-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-DADOS
                    END-IF
      *        WHEN GS-PRINTER-FLG-TRUE
      *             PERFORM IMPRIME-RELATORIO
               WHEN GS-VERIF-PREV-VENDAS-TRUE
                    PERFORM VERIFICA-PREV-VENDAS
               WHEN GS-GRAVA-PREV-VENDAS-TRUE
                    PERFORM GRAVA-PREV-VENDAS
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM POPUP-CONTRATO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
      *------------------------------------------------------------
      * busca os ultimos preços de custo p/ a previsão de vendas deste
      * contrato, caso nunca tenha feita a prev.do mesmo então grava os
      * custos
       VERIFICA-PREV-VENDAS SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO06.
           READ COD006 INVALID KEY INITIALIZE REG-COD006.
           MOVE FOTO-CO06              TO GS-FOTOS
           MOVE IND-FOTO-CO06          TO GS-IND-FOTOS
           MOVE ALBUM-CO06             TO GS-ALBUM
           MOVE IND-ALBUM-CO06         TO GS-IND-ALBUM
           MOVE CAPA-ALBUM-CO06        TO GS-CAPA-ALBUM
           MOVE IND-CAPA-ALBUM-CO06    TO GS-IND-CAPA-ALBUM
           MOVE FITA-CO06              TO GS-FITAS
           MOVE IND-FITA-CO06          TO GS-IND-FITAS
           MOVE CAPA-FITA-CO06         TO GS-CAPA-FITA
           MOVE IND-CAPA-FITA-CO06     TO GS-IND-CAPA-FITA.
       GRAVA-PREV-VENDAS SECTION.
           MOVE GS-CONTRATO            TO  NR-CONTRATO-CO06
           MOVE GS-FOTOS               TO  FOTO-CO06
           MOVE GS-IND-FOTOS           TO  IND-FOTO-CO06
           MOVE GS-ALBUM               TO  ALBUM-CO06
           MOVE GS-IND-ALBUM           TO  IND-ALBUM-CO06
           MOVE GS-CAPA-ALBUM          TO  CAPA-ALBUM-CO06
           MOVE GS-IND-CAPA-ALBUM      TO  IND-CAPA-ALBUM-CO06
           MOVE GS-FITAS               TO  FITA-CO06
           MOVE GS-IND-FITAS           TO  IND-FITA-CO06
           MOVE GS-CAPA-FITA           TO  CAPA-FITA-CO06
           MOVE GS-IND-CAPA-FITA       TO  IND-CAPA-FITA-CO06
           WRITE REG-COD006.
      *----------------------------------------------------------
       IMPRIMIR-DADOS SECTION.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.

           MOVE GS-TAXA TO PERC2-REL PERC3-REL PERC4-REL PERC6-REL
           MOVE USUARIO-W   TO USUARIO-REL.

           MOVE GS-CONTRATO      TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY CONTINUE
             NOT INVALID KEY
                 PERFORM IMPRIMIR-DADOS-CONTRATO
                 PERFORM PREVISAO-VENDA-REL
                 PERFORM VENDA-ANTECIPADA-REL
                 PERFORM CUSTO-BRINDE-REL
                 PERFORM CUSTO-REPORTAGEM-REL
                 PERFORM CUSTO-PRODUCAO-REL
                 PERFORM VENDA-ORGEV-REL
                 PERFORM CUSTO-ORGEV-COMPROMISSO-REL
                 PERFORM CUSTO-ORGEV-RELATORIO-REL
                 PERFORM AVALIACAO-GERAL-REL.

           COPY DESCONDENSA.

       IMPRIMIR-DADOS-CONTRATO SECTION.
           EVALUATE ORIGEM-CO40
             WHEN 1 MOVE "KEL"   TO ORIGEM-REL
             WHEN 2 MOVE "MIK"   TO ORIGEM-REL
           END-EVALUATE.
           MOVE GS-CONTRATO      TO CONTRATO-REL
           MOVE IDENTIFICACAO-CO40 TO IDENTIFICACAO-REL
           MOVE INSTITUICAO-CO40 TO CODIGO-IE10
           READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10.
           MOVE NOME-IE10        TO INSTITUICAO-REL
           MOVE CIDADE-CO40      TO CIDADE
           READ CAD010 INVALID KEY INITIALIZE REG-CAD010.
           MOVE NOME-CID         TO CIDADE-REL
           MOVE DDD-CID(2: 3)    TO DDD-W
           MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
           MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W         TO MESANO-REL
           MOVE QTDE-FORM-CO40   TO FORM-REL
           MOVE PADRAO-CO40      TO PADRAO-REL
           EVALUATE COBERTURA-CO40
             WHEN 1 MOVE "F/V/O" TO SERVICO-REL
             WHEN 2 MOVE "F/V  " TO SERVICO-REL
             WHEN 3 MOVE "F/O  " TO SERVICO-REL
             WHEN 4 MOVE "V/O  " TO SERVICO-REL
             WHEN 5 MOVE "F    " TO SERVICO-REL
             WHEN 6 MOVE "V    " TO SERVICO-REL
             WHEN 7 MOVE "O    " TO SERVICO-REL
           END-EVALUATE.
           MOVE STATUS-CO40      TO CODIGO-CO01.
           READ COD001 INVALID KEY MOVE SPACES TO STATUS-CO01.
           MOVE STATUS-CO01      TO STATUS-REL.
           MOVE OBS-IMPORTANTE-CO40 TO MENSAGEM-REL.
           MOVE REGIAO-CID       TO CODIGO-REG
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG         TO REGIAO-REL
           MOVE REPRESENTANTE-CO40 TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01        TO REPRESENT-REL
           MOVE ASSINATURA-CO40  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO ASSINATURA-REL
           MOVE MULTA-CONTRAT-CO40 TO MULTA-REL
           MOVE CIDADE-FORUM-CO40  TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO FORUM-REL
           MOVE COMERCIALIZACAO-CO40 TO COMERCIAL-REL
           MOVE QTDE-FORM-INI-CO40 TO FORM-INI-REL
           MOVE PADRAO-INI-CO40    TO PADRAO-INI-REL
           MOVE ORGANIZADOR-CO40   TO ORGANIZADOR-REL
           MOVE DDD-W              TO DDD1-REL
           MOVE FONE-ORGANIZ-CO40  TO FONE1-REL
           MOVE DATA-PREV-VENDA-CO40 TO DATA-PREV-REL

           OPEN INPUT CGD011 CGD012.
           MOVE 0                  TO CLASSIF-CG10.
           MOVE GS-CONTRATO        TO CODIGO-CG10(1: 4)
           MOVE ZEROS              TO CODIGO-CG10(5: 4).

           PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-04 AFTER 2.
           WRITE REG-RELAT FROM LINHA-05 AFTER 2.
           WRITE REG-RELAT FROM LINHA-06.

           MOVE SPACES TO COMISSAO(1) COMISSAO(2) COMISSAO(3).
           MOVE ZEROS TO FONE(1) FONE(2) FONE(3) I.
           MOVE CODIGO-CG10        TO CODIGO-CG12
           START CGD012 KEY IS NOT < CODIGO-CG12 INVALID KEY
                 MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
             READ CGD012 NEXT RECORD AT END MOVE "10" TO ST-CGD012
               NOT AT END
                 ADD 1 TO I
                 MOVE CODIGO-CG12(1: 4) TO NR-CONTRATO-W
                 IF NR-CONTRATO-W <> GS-CONTRATO OR I > 3
                    MOVE "10" TO ST-CGD012
                 ELSE
                   IF CARGO-COMISSAO-CG12 < 1 OR > 4 CONTINUE
                   ELSE
                     MOVE CODIGO-CG12 TO CODIGO-CG10
                     READ CGD010 INVALID KEY SUBTRACT 1 FROM I
                                             CONTINUE
                      NOT INVALID KEY
                         MOVE COMPRADOR-CG10 TO COMISSAO(I)
                         MOVE CODIGO-CG10 TO CODIGO-CG11
                         READ CGD011 INVALID KEY INITIALIZE REG-CGD011
                         END-READ
                         MOVE FONE1-CG11 TO FONE(I)
                     END-READ
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE CGD011 CGD012.

           MOVE COMISSAO(1)        TO COMISSAO2-REL
           MOVE DDD-W              TO DDD2-REL
           MOVE FONE(1)            TO FONE2-REL
           MOVE COMISSAO(2)        TO COMISSAO3-REL
           MOVE DDD-W              TO DDD3-REL
           MOVE FONE(2)            TO FONE3-REL
           MOVE COMISSAO(3)        TO COMISSAO4-REL
           MOVE DDD-W              TO DDD4-REL
           MOVE FONE(3)            TO FONE4-REL
           MOVE CONTATO-CONVITE-CO40 TO CONVITE5-REL
           MOVE CONTATO-BECA-CO40  TO BECA6-REL
           MOVE CONTATO-OUTRO-CO40 TO OUTRO7-REL.
           WRITE REG-RELAT FROM LINHA-07 AFTER 2.
           WRITE REG-RELAT FROM LINHA-08.
           WRITE REG-RELAT FROM LINHA-09.
           WRITE REG-RELAT FROM LINHA-10.
           WRITE REG-RELAT FROM LINHA-11.
           WRITE REG-RELAT FROM LINHA-12.
           WRITE REG-RELAT FROM LINHA-13.
           WRITE REG-RELAT FROM LINHA-14 AFTER 2.
           WRITE REG-RELAT FROM LINHA-15.
           ADD 17 TO LIN.

       PREVISAO-VENDA-REL SECTION.
           MOVE ZEROS TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-16 AFTER 2.
           WRITE REG-RELAT FROM LINHA-17.
           WRITE REG-RELAT FROM LINHA-18.
           ADD 11 TO LIN.

           MOVE "Fotos"        TO ITEM1-REL
           MOVE GS-FOTOS       TO VLR-UNIT1-REL
           MOVE PADRAO-CO40    TO PADRAO-CO05
           READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05
           END-READ
           MOVE GS-IND-FOTOS   TO INDICE1-REL
           COMPUTE QTDE-W  = (GS-IND-FOTOS / 100) *
                                  (PREV-FOTOS-CO05 * QTDE-FORM-CO40)
           MOVE QTDE-W         TO QTDE1-REL
           COMPUTE OBJ-VENDAS-W = QTDE-W * GS-FOTOS
           MOVE OBJ-VENDAS-W   TO OBJ-VENDA1-REL.
           ADD OBJ-VENDAS-W    TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-19.

           MOVE "Album p/ 50 fotos"        TO ITEM1-REL
           MOVE GS-ALBUM       TO VLR-UNIT1-REL
           MOVE GS-IND-FOTOS   TO INDICE1-REL
           COMPUTE QTDE-W = (GS-IND-ALBUM / 100) * QTDE-FORM-CO40
           MOVE QTDE-W         TO QTDE1-REL
           COMPUTE OBJ-VENDAS-W = QTDE-W * GS-ALBUM
           MOVE OBJ-VENDAS-W   TO OBJ-VENDA1-REL.
           ADD OBJ-VENDAS-W    TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-19.

           MOVE "Capa do album"   TO ITEM1-REL
           MOVE GS-CAPA-ALBUM     TO VLR-UNIT1-REL
           MOVE GS-IND-CAPA-ALBUM TO INDICE1-REL
           COMPUTE QTDE-W = (GS-IND-CAPA-ALBUM / 100) *
                                      QTDE-FORM-CO40
           MOVE QTDE-W            TO QTDE1-REL
           COMPUTE OBJ-VENDAS-W = QTDE-W * GS-CAPA-ALBUM
           MOVE OBJ-VENDAS-W      TO OBJ-VENDA1-REL.
           ADD OBJ-VENDAS-W    TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-19.

           MOVE "Fitas"           TO ITEM1-REL
           MOVE GS-FITAS          TO VLR-UNIT1-REL
           MOVE GS-IND-FITAS      TO INDICE1-REL
           COMPUTE QTDE-W =(GS-IND-FITAS / 100) * QTDE-FORM-CO40
           MOVE QTDE-W            TO QTDE1-REL
           COMPUTE OBJ-VENDAS-W = QTDE-W * GS-FITAS
           MOVE OBJ-VENDAS-W      TO OBJ-VENDA1-REL.
           ADD OBJ-VENDAS-W    TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-19.

           MOVE "Capa fita"       TO ITEM1-REL
           MOVE GS-CAPA-FITA      TO VLR-UNIT1-REL
           MOVE GS-IND-CAPA-FITA  TO INDICE1-REL
           COMPUTE QTDE-W = (GS-IND-CAPA-FITA / 100) * QTDE-FORM-CO40
           MOVE QTDE-W            TO QTDE1-REL
           COMPUTE OBJ-VENDAS-W = QTDE-W * GS-CAPA-FITA
           MOVE OBJ-VENDAS-W      TO OBJ-VENDA1-REL.
           ADD OBJ-VENDAS-W    TO OBJ-VENDAS-TOT.
           WRITE REG-RELAT FROM LINHA-19.

           MOVE "Previsao Geral de vendas"       TO ITEM1-REL
           MOVE ZEROS             TO VLR-UNIT1-REL INDICE1-REL QTDE1-REL
           MOVE OBJ-VENDAS-TOT    TO OBJ-VENDA1-REL.
           WRITE REG-RELAT FROM LINHA-19.
           MOVE OBJ-VENDAS-TOT    TO PREV-VENDAS-G.

       VENDA-ANTECIPADA-REL SECTION.
           WRITE REG-RELAT FROM LINHA-20 AFTER 2.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-22.
           ADD 4 TO LIN.

      **    WRITE REG-RELAT FROM LINHA-23.
      **    ADD 1 TO LIN.
      **    IF LIN > 56 PERFORM CABECALHO.

       CUSTO-BRINDE-REL SECTION.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-24 AFTER 2.
           WRITE REG-RELAT FROM LINHA-25 AFTER 2.
           WRITE REG-RELAT FROM LINHA-26.
           MOVE ZEROS TO CUSTO-TOTAL CUSTO-PREVISTO CUSTO-W JUROS-W
                         CONT TAXA-ACUMULADA QTDE-ITEM TOTAL-IND-W
                         TOTAL-JUROS-W TOTAL-GERAL-W TOTAL-W.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO50.
           MOVE ZEROS              TO ITEM-CO50 GS-CONT-BRINDE
                                      DATA-VENCTO-CO50 REALIZADO-CO50.
           START COD050 KEY IS NOT < ALT1-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           PERFORM UNTIL ST-COD050 = "10"
              READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
                NOT AT END
                  IF NR-CONTRATO-CO50 <> GS-CONTRATO
                     MOVE "10" TO ST-COD050
                  ELSE
                     IF SUSP-PREV-DEF-CO50 <> 2
                        ADD 1                   TO QTDE-ITEM
                        MOVE ITEM-CO50          TO ITEM3-REL
                        MOVE CODBRINDE-CO50     TO CODIGO-CO02
                        READ COD002 INVALID KEY MOVE SPACES TO NOME-CO02
                        END-READ
                        MOVE NOME-CO02          TO DESCRICAO3-REL
                        MOVE QTDE-POR-FORM-CO50 TO QTDE3-REL
                        MOVE QTDE-FORM-CO50     TO FORM3-REL
                        PERFORM CALCULA-CUSTO-BRINDE
                        MOVE CUSTO-PREVISTO     TO VLR-UNIT3-REL
                        ADD CUSTO-PREVISTO      TO TOTAL-IND-W
                        ADD CUSTO-W             TO TOTAL-W
                        MOVE CUSTO-W            TO TOTAL3-REL
                        MOVE DATA-VENCTO-CO50   TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV           TO VENCTO3-REL
                        MOVE DATA-PAGTO-CO50    TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV           TO PAGTO3-REL
                        MOVE DIAS-PRAZO-CO50    TO DIAS3-REL
                        PERFORM CALCULO-JUROS-BRINDE
                        MOVE JUROS-W            TO JUROS3-REL
                        ADD JUROS-W             TO TOTAL-JUROS-W
                        COMPUTE CUSTO-TOTAL = CUSTO-W + JUROS-W
                        ADD CUSTO-TOTAL         TO TOTAL-GERAL-W
                        MOVE CUSTO-TOTAL        TO CUSTO-TOTAL3-REL
                        WRITE REG-RELAT FROM LINHA-27
                        ADD 1 TO LIN
                        IF LIN > 56 PERFORM CABECALHO
                        END-IF
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
           MOVE ZEROS             TO ITEM3-REL
           MOVE QTDE-ITEM         TO DESCRICAO3-REL
           MOVE ZEROS             TO QTDE3-REL FORM3-REL
           MOVE TOTAL-IND-W       TO VLR-UNIT3-REL
           MOVE ZEROS             TO VENCTO3-REL PAGTO3-REL DIAS3-REL
           MOVE TOTAL-JUROS-W     TO JUROS3-REL
           MOVE TOTAL-W           TO TOTAL3-REL
           MOVE TOTAL-GERAL-W     TO CUSTO-TOTAL3-REL.
           MOVE TOTAL-GERAL-W     TO PATROCINIO-G.
           WRITE REG-RELAT FROM LINHA-27 AFTER 2.
           ADD 2 TO  LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CALCULA-CUSTO-BRINDE SECTION.
      *  se o brinde já foi pago então o custo será conforme o
      *  valor-pago, senão verifica se o brinde é do tipo mult-por-form
      *  então multiplica qtde-form pela qtde-por-form e custo-previsto,
      *  onde o custo-previsto é o custo unitário do brinde caso ele
      *  tenha sido preenchido, senão o custo-previsto virá do cadastro
      *  do brinde
           IF CUSTO-UNIT-CO50 <> ZEROS
              MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
           ELSE MOVE VALOR-CO02    TO CUSTO-PREVISTO
           END-IF.

           IF REALIZADO-CO50 <> 1
              IF MULT-FORM-CO02 = 1
                 COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50 *
                    QTDE-FORM-CO50) * CUSTO-PREVISTO
              ELSE
                 COMPUTE CUSTO-W = QTDE-POR-FORM-CO50 * CUSTO-PREVISTO
              END-IF
           ELSE MOVE VALOR-PAGO-CO50   TO CUSTO-W
           END-IF.

       CALCULO-JUROS-BRINDE SECTION.
           COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (CUSTO-PREVISTO * MESES-W) /
                                  CUSTO-PREVISTO.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = CUSTO-W * (TAXA-ACUMULADA - 1).

       CUSTO-REPORTAGEM-REL SECTION.
           ADD 5 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-28 AFTER 2.
           WRITE REG-RELAT FROM LINHA-29 AFTER 2.
           WRITE REG-RELAT FROM LINHA-30.
           PERFORM IMPRIME-REPORTAGEM.
           MOVE QTDE-ITEM    TO EVENTO4-REL.
           MOVE TOTAL-IND-W  TO TOTAL4-REL.
           MOVE TOTAL-JUROS-W TO JUROS4-REL.
           MOVE TOTAL-GERAL-W TO CUSTO-TOTAL4-REL REPORTAGEM-G.
           MOVE ZEROS         TO DATA4-REL FORM4-REL NR-RELAT4-REL
                                 ITEM4-REL DIAS4-REL
           MOVE SPACES        TO COORDEN4-REL
           WRITE REG-RELAT FROM LINHA-31 AFTER 2.
           ADD 2 TO LIN
           IF LIN > 56 PERFORM CABECALHO
                       ADD 2 TO LIN.
      *-----------------------------------------------------
       IMPRIME-REPORTAGEM SECTION.
           OPEN INPUT RED030 RED300 RED301 RED302 RED303.
           IF ST-RED030 <> "00"
              MOVE "ERRO ABERTURA RED030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED300 <> "00"
              MOVE "ERRO ABERTURA RED300: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED300 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED301 <> "00"
              MOVE "ERRO ABERTURA RED301: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED301 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED302 <> "00"
              MOVE "ERRO ABERTURA RED302: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED302 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED303 <> "00"
              MOVE "ERRO ABERTURA RED303: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED303 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE GS-CONTRATO TO CONTRATO-R301.
           START RED301 KEY IS NOT < CONTRATO-R301 INVALID KEY
                 MOVE "10" TO ST-RED301.
           PERFORM UNTIL ST-RED301 = "10"
             READ RED301 NEXT RECORD AT END MOVE "10" TO ST-RED301
               NOT AT END
                 IF CONTRATO-R301 <> GS-CONTRATO
                    MOVE "10" TO ST-RED301
                 ELSE MOVE DOCTO-R301 TO DOCTO-WK
                      WRITE REG-WORK
                      END-WRITE
                 END-IF
             END-READ
           END-PERFORM.

           MOVE ZEROS TO ITEM-W QTDE-ITEM TOTAL-IND-W TOTAL-JUROS-W
                         TOTAL-GERAL-W.
           MOVE ZEROS TO DOCTO-WK.
           START WORK KEY IS NOT < DOCTO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 ADD 1 TO ITEM-W QTDE-ITEM
                 MOVE ITEM-W   TO ITEM4-REL
                 PERFORM VERIF-QTDE-CONTRATO
                 PERFORM CALC-DESPESAS-REPORTAGEM
                 PERFORM CALC-QTDE-EVENTO
                 IF QTDE-EVENTO > 1 MOVE "VARIOS" TO EVENTO4-REL
                 ELSE MOVE ULT-EVENTO TO CODIGO-CO03
                   READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                   END-READ
                   MOVE NOME-CO03     TO EVENTO4-REL
                 END-IF
                 PERFORM CALC-VLR-REPORTAGEM
      *          se existe mais de um contrato o total de reportagem
      *          sera calculada pela percentagem do contrato solicitado
                 COMPUTE TOTAL-REPORTAGEM =
                    PERC-FORMANDO * (TOTAL-REPORTAGEM + DESPESAS-W)
                 MOVE TOTAL-REPORTAGEM TO TOTAL4-REL
                 ADD TOTAL-REPORTAGEM  TO TOTAL-IND-W
                 PERFORM CALC-JUROS-REPORTAGEM
                 MOVE JUROS-W          TO JUROS4-REL
                 ADD JUROS-W           TO TOTAL-JUROS-W
                 COMPUTE TOTAL-W = JUROS-W + TOTAL-REPORTAGEM
                 ADD TOTAL-W           TO TOTAL-GERAL-W
                 MOVE TOTAL-W          TO CUSTO-TOTAL4-REL

                 WRITE REG-RELAT FROM LINHA-31
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE RED030 RED300 RED301 RED302 RED303 WORK.
           DELETE FILE WORK.
       CALC-JUROS-REPORTAGEM SECTION.
           COMPUTE MESES-W = GRDIAS-NUM-DIAS / 30.
           COMPUTE PRAZO-MEDIO ROUNDED = (TOTAL-REPORTAGEM * MESES-W) /
                                  TOTAL-REPORTAGEM.
           MOVE 1 TO TAXA-ACUMULADA.
           PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > PRAZO-MEDIO
               COMPUTE TAXA-ACUMULADA = TAXA-ACUMULADA *
                        ((GS-TAXA / 100) + 1)
           END-PERFORM.
           COMPUTE JUROS-W = TOTAL-REPORTAGEM * (TAXA-ACUMULADA - 1).

      * VERIFICA QTDE DE CONTRATOS NO DOCTO
       VERIF-QTDE-CONTRATO SECTION.
           MOVE DOCTO-WK TO DOCTO-R301
           MOVE ZEROS    TO CONTRATO-R301.
           START RED301 KEY IS NOT < CHAVE-R301 INVALID KEY
                 MOVE "10" TO ST-RED301.
           MOVE ZEROS TO QTDE-CONTRATO TOTAL-FORMANDO
           PERFORM UNTIL ST-RED301 = "10"
             READ RED301 NEXT RECORD AT END MOVE "10" TO ST-RED301
               NOT AT END
                 IF DOCTO-R301 <> DOCTO-WK MOVE "10" TO ST-RED301
                 ELSE
                    ADD 1 TO QTDE-CONTRATO
                    MOVE CONTRATO-R301 TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY MOVE ZEROS TO QTDE-FORM-CO40
                    END-READ
                    ADD QTDE-FORM-CO40 TO TOTAL-FORMANDO
                 END-IF
             END-READ
           END-PERFORM.
       CALC-DESPESAS-REPORTAGEM SECTION.
           MOVE DOCTO-WK    TO DOCTO-R300.
           READ RED300 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE GS-CONTRATO TO NR-CONTRATO-CO40
               READ COD040 INVALID KEY MOVE ZEROS TO QTDE-FORM-CO40
               END-READ
               MOVE QTDE-FORM-CO40 TO FORM4-REL
      *        se tiver apenas um contrato nesse docto entao perc = 100%
      *        senao calcula a percentagem de formando
               IF QTDE-CONTRATO = 1
                  MOVE 1  TO PERC-FORMANDO
               ELSE COMPUTE PERC-FORMANDO =
                    QTDE-FORM-CO40 / TOTAL-FORMANDO
               END-IF
               COMPUTE DESPESAS-W = VLR-COMB-R300 + VLR-HOSP-R300 +
                  VLR-REFEICAO-R300 + VLR-PASSAGEM-R300 +
                  VLR-ALUGUEL-R300 + VLR-MAT-R300 + VLR-OUTROS-R300
               MOVE DATA-MOV-R300   TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV        TO DATA4-REL
               MOVE DOCTO-R300      TO NR-RELAT4-REL
               MOVE DATA-MOV-R300   TO GRDIAS-AAMMDD-INICIAL
               MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
               CALL "GRIDAT2" USING DATA-INV
               MOVE DATA-INV        TO GRDIAS-AAMMDD-FINAL
               CALL "GRDIAS1" USING PARAMETROS-GRDIAS
               MOVE GRDIAS-NUM-DIAS TO DIAS4-REL
           END-READ.
       CALC-QTDE-EVENTO SECTION.
           MOVE DOCTO-WK  TO DOCTO-R302
           MOVE ZEROS     TO EVENTO-R302.
           MOVE ZEROS     TO QTDE-EVENTO.
           START RED302 KEY IS NOT < CHAVE-R302 INVALID KEY
                 MOVE "10" TO ST-RED302.
           PERFORM UNTIL ST-RED302 = "10"
             READ RED302 NEXT RECORD AT END MOVE "10" TO ST-RED302
               NOT AT END
                 IF DOCTO-R302 <> DOCTO-WK MOVE "10" TO ST-RED302
                 ELSE ADD 1 TO QTDE-EVENTO
                      MOVE EVENTO-R302 TO ULT-EVENTO
                 END-IF
             END-READ
           END-PERFORM.
       CALC-VLR-REPORTAGEM SECTION.
           MOVE SPACES    TO COORDEN4-REL.
           MOVE ZEROS     TO TOTAL-REPORTAGEM.
           MOVE DOCTO-WK  TO DOCTO-R303.
           MOVE ZEROS     TO REPRES-R303 FUNCAO-R303.
           START RED303 KEY IS NOT < CHAVE-R303 INVALID KEY
               MOVE "10" TO ST-RED303.
           PERFORM UNTIL ST-RED303 = "10"
             READ RED303 NEXT RECORD AT END MOVE "10" TO ST-RED303
               NOT AT END
      *          funcao-r303 = 04 -> coordenador
                 IF DOCTO-WK <> DOCTO-R303 MOVE "10" TO ST-RED303
                 ELSE
                  IF FUNCAO-R303 = 04
                     MOVE REPRES-R303 TO CODIGO-R030
                     READ RED030 INVALID KEY MOVE SPACES TO NOME-R030
                     END-READ
                     MOVE NOME-R030   TO COORDEN4-REL
                  END-IF
                  ADD VLR-REPORT-R303 TO TOTAL-REPORTAGEM
                 END-IF
             END-READ
           END-PERFORM.
      *-----------------------------------------------------------
       CUSTO-PRODUCAO-REL SECTION.
           ADD 5 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-32 AFTER 2.
           WRITE REG-RELAT FROM LINHA-33 AFTER 2.
           WRITE REG-RELAT FROM LINHA-34.

      **    WRITE REG-RELAT FROM LINHA-35.
      **    ADD 1 TO LIN.
      **    IF LIN > 56 PERFORM CABECALHO.
       VENDA-ORGEV-REL SECTION.
           ADD 5 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-36 AFTER 2.
           WRITE REG-RELAT FROM LINHA-37 AFTER 2.
           WRITE REG-RELAT FROM LINHA-38.

      **    WRITE REG-RELAT FROM LINHA-39.
      **    ADD 1 TO LIN.
      **    IF LIN > 56 PERFORM CABECALHO.
       CUSTO-ORGEV-COMPROMISSO-REL SECTION.
           ADD 5 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-40 AFTER 2.
           WRITE REG-RELAT FROM LINHA-41 AFTER 2.
           WRITE REG-RELAT FROM LINHA-42.

      **    WRITE REG-RELAT FROM LINHA-43.
      **    ADD 1 TO LIN.
      **    IF LIN > 56 PERFORM CABECALHO.
       CUSTO-ORGEV-RELATORIO-REL SECTION.
           ADD 5 TO LIN.
           IF LIN > 56 PERFORM CABECALHO
                       ADD 5 TO LIN.
           WRITE REG-RELAT FROM LINHA-44 AFTER 2.
           WRITE REG-RELAT FROM LINHA-45 AFTER 2.
           WRITE REG-RELAT FROM LINHA-46.

      **    WRITE REG-RELAT FROM LINHA-47.
      **    ADD 1 TO LIN.
      **    IF LIN > 56 PERFORM CABECALHO.
       AVALIACAO-GERAL-REL SECTION.
           PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-48 AFTER 2.

           MOVE "PREVISAO DE VENDAS TOTAIS" TO DESCRICAO9-REL
           MOVE PREV-VENDAS-G               TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE 100                         TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "RECEITAS COM VENDAS ATECIPADAS" TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           WRITE REG-RELAT FROM LINHA-51.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS DE VENDAS         " TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS COM REPRESENTANTES" TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS COM PATROCINIOS  " TO DESCRICAO9-REL
           MOVE PATROCINIO-G               TO VALOR9-REL
           MOVE ZEROS                      TO TOTAL9-REL
           COMPUTE PERC-W = (PATROCINIO-G / PREV-VENDAS-G) * 100
           MOVE PERC-W                     TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS COM REPORTAGEM   " TO DESCRICAO9-REL
           MOVE REPORTAGEM-G               TO VALOR9-REL
           MOVE ZEROS                      TO TOTAL9-REL
           COMPUTE PERC-W = (REPORTAGEM-G / PREV-VENDAS-G) * 100
           MOVE PERC-W                     TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS COM ORGANIZACAO/OUTROS" TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS COM PRODUCAO      " TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "LUCRO OPERACIONAL       " TO DESCRICAO9-REL
      *    COMPUTE TOTAL-W = PREV-VENDAS-G -
      *                     (REPORTAGEM-G + PATROCINIO-G).
           MOVE ZEROS                      TO VALOR9-REL
      *    MOVE TOTAL-W                    TO TOTAL9-REL
           MOVE ZEROS                      TO TOTAL9-REL
           COMPUTE PERC-W = (TOTAL-W / PREV-VENDAS-G) * 100
           MOVE PERC-W                     TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           WRITE REG-RELAT FROM LINHA-51.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "VENDAS DE ORGANIZACAO DE EVENTOS" TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS DE ORGANIZACAO DE EVENTOS" TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           WRITE REG-RELAT FROM LINHA-51.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "RESULTADO PREVISTO DO CONTRATO  " TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.

           WRITE REG-RELAT FROM LINHA-51.
           WRITE REG-RELAT FROM LINHA-50.

           MOVE "CUSTOS ADMINISTRATIVOS   " TO DESCRICAO9-REL
           MOVE ZEROS                       TO VALOR9-REL
           MOVE ZEROS                       TO TOTAL9-REL
           MOVE ZEROS                       TO PERC9-REL
           WRITE REG-RELAT FROM LINHA-49.
           WRITE REG-RELAT FROM LINHA-50.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "COP105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------------

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM LINHA-01
           ELSE WRITE REG-RELAT FROM LINHA-01 AFTER PAGE.
           WRITE REG-RELAT FROM LINHA-02.
           WRITE REG-RELAT FROM LINHA-03 AFTER 2.
           MOVE 4 TO LIN.
      *------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED010 CGD010 CAD010 CAD012 COD001 COD002
                 COD003 COD005 COD040 COD050  CGD001 COD006.
           EXIT PROGRAM.
