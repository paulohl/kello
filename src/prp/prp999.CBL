       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP101.
      *-------------------------------------------------------*
      *    DATA  : 20/04/2000
      *    AUTORA: MARA
      *    FUNCAO: CONFERENCIA DO PLANEJAMENTO
      *-------------------------------------------------------*
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA
               PRINTER IS LPRINTER.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX010.
           COPY MTPX001.
           COPY IEPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY COPX061.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX012.
           COPY REPX002.
           COPY REPX003.
           COPY REPX005.
           COPY REPX006.
           COPY PRPX010.
           COPY PRPX011.
           COPY PRPX012.
           COPY PRPX100.
           COPY PRPX101.
           COPY PRPX102.
           COPY PRPX103.
           COPY PRPX104.
           COPY PRPX105.
           COPY IEPX011.

           SELECT RELAT ASSIGN TO ARQUIVO-IMPRESSAO
                        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW010.
       COPY MTPW001.
       COPY IEPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY COPW061.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW012.
       COPY REPW002.
       COPY REPW003.
       COPY REPW005.
       COPY REPW006.

       COPY PRPW010.
       COPY PRPW011.
       COPY PRPW012.
       COPY PRPW100.
       COPY PRPW101.
       COPY PRPW102.
       COPY PRPW103.
       COPY PRPW104.
       COPY PRPW105.
       COPY IEPW011.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(140).

       WORKING-STORAGE SECTION.
           COPY "PRP101.CPB".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ERRO-1          PIC XX          VALUE SPACES.
           05  ST-CAD001       PIC XX          VALUE SPACES.
           05  ST-CAD010       PIC XX          VALUE SPACES.
           05  ST-COD040       PIC XX          VALUE SPACES.
           05  ST-COD060       PIC XX          VALUE SPACES.
           05  ST-COD061       PIC XX          VALUE SPACES.
           05  ST-COD003       PIC XX          VALUE SPACES.
           05  ST-IED010       PIC XX          VALUE SPACES.
           05  ST-CGD001       PIC XX          VALUE SPACES.
           05  ST-CGD010       PIC XX          VALUE SPACES.
           05  ST-CGD011       PIC XX          VALUE SPACES.
           05  ST-CGD012       PIC XX          VALUE SPACES.
           05  ST-RED002       PIC XX          VALUE SPACES.
           05  ST-RED003       PIC XX          VALUE SPACES.
           05  ST-RED005       PIC XX          VALUE SPACES.
           05  ST-RED006       PIC XX          VALUE SPACES.
           05  ST-PRD010       PIC XX          VALUE SPACES.
           05  ST-PRD011       PIC XX          VALUE SPACES.
           05  ST-PRD012       PIC XX          VALUE SPACES.
           05  ST-PRD100       PIC XX          VALUE SPACES.
           05  ST-PRD101       PIC XX          VALUE SPACES.
           05  ST-PRD102       PIC XX          VALUE SPACES.
           05  ST-PRD103       PIC XX          VALUE SPACES.
           05  ST-PRD104       PIC XX          VALUE SPACES.
           05  ST-PRD105       PIC XX          VALUE SPACES.
           05  ST-MTD001       PIC XX          VALUE SPACES.
           05  ST-IED011       PIC XX          VALUE SPACES.
           05  FS-ORDSERV      PIC XX          VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LINHA                 PIC 99       VALUE ZEROS.
           05  COMPACTA              PIC X(01)    VALUE SPACES.
           05  NR-PLAN-W             PIC 9(14)    VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  FUNCAO-ANT            PIC 99       VALUE ZEROS.
           05  MASC-QTDE             PIC Z.ZZ9    BLANK WHEN ZEROS.
           05  CODIGO-ANT            PIC 9(6)     VALUE ZEROS.
           05  DIAMES-W              PIC 9(4)     VALUE ZEROS.
           05  PRIMEIRO              PIC X(01)    VALUE SPACES.
           05  IND                   PIC 9(03)    VALUE ZEROS.
           05  IND2                  PIC 9(04)    VALUE ZEROS.
           05  IND3                  PIC 9(04)    VALUE ZEROS.
           05  LIN4                  PIC 9(02)    VALUE ZEROS.
           05  MASC-DATA             PIC ZZ/ZZ.
           05  LINDET-REL            PIC X(140)    VALUE SPACES.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  TABELA-EQUIPE OCCURS 90 TIMES.
               10  CODIGO-TAB     PIC 9(6).
               10  FUNCAO-TAB     PIC 99.
               10  PADRAO-TAB     PIC X.
               10  VEICULO-TAB    PIC 99.
               10  SEQ-EV-TAB OCCURS 50 TIMES PIC 99.

      *    MAIOR-SEQ-EVENTO - VERIFICA QUAL ULTIMA SEQ-EVENTO UTILIZADA
           05  MAIOR-SEQ-EVENTO         PIC 99    VALUE ZEROS.
      *    LIMITE-EVENTO - QUAL O NIVEL MAXIMO UTILIZADO P/ SEQUENCIA
           05  LIMITE-EVENTO            PIC 99    VALUE ZEROS.
      *    LIM-INF E LIM-SUP = DENTRO DO NIVEL DO LIMITE-EVENTO VERIFICA
      *    -SE QUAL O LIMITE INFERIOR E LIMITE SUPERIOR SUPORTADO
           05  LIM-INF                  PIC 99    VALUE ZEROS.
           05  LIM-SUP                  PIC 99    VALUE ZEROS.
           05  CONT                     PIC 99    VALUE ZEROS.
      *    CONT - CONTROLA LINHAS EM BRANCO DE EQUIPE

      *    i - controla o evento dentro da equipe a ser impresso
           05  I                        PIC 99    VALUE ZEROS.
      *    j - controla a equipe a ser impressa
           05  J                        PIC 99    VALUE ZEROS.
      *    M - controla o intervalo de eventoRque deverá ser impresso
           05  M                        PIC 99    VALUE ZEROS.
      *    P controla a coluna de impressão
           05  P                        PIC 99    VALUE ZEROS.
           05  LIN                      PIC 9(02) VALUE ZEROS.
           05  INICIO                   PIC 9(02) VALUE ZEROS.
           05  NAO                      PIC X(01) VALUE SPACES.

           COPY "PARAMETR".

       02 status-code           PIC X(2) COMP-5.

       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(032) VALUE
              "RELATORIO DE PLANEJAMENTO -  PG ".
           05 PG-REL                         PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(5)   VALUE SPACES.
           05 PARTE-REL                      PIC  X(27)  VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "                                              EMISSAO: ".
           05 EMISSAO-REL                    PIC  99/99/9999.

       02  LINHA-02.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(004) VALUE "NR: ".
           05 NR-REL                         PIC  9999.99.99.9999.99.
           05 FILLER                         PIC  X(003) VALUE " - ".
           05 CIDADE-REL                     PIC  X(015) VALUE SPACES.
           05 FILLER                         PIC  X(029) VALUE SPACES.
           05 FILLER                         PIC  X(039) VALUE
              "                                   DE: ".
           05 DATAI-REL                      PIC  99/99/9999.
           05 FILLER                         PIC  X(003) VALUE " a ".
           05 DATAF-REL                      PIC  99/99/9999.
       02  LINHA-03.
           05 FILLER                         PIC  X(132) VALUE
           "____________________________________________________________
      -    "____________________________________________________________
      -    "____________".
       02  LINHA-04.
           05 FILLER                         PIC  X(132) VALUE
      -    "|
      -    "
      -    "           |".
       02  LINHA-05.
           05 FILLER                         PIC  X(132) VALUE
           "| IT   DATA   HORA    EVENTO                 FORM   P   CONT
      -    "  CURSO               LOCAL                       T     B  C
      -    "  IDENT    |".
       02  LINHA-06.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 ITEM-REL                       PIC  XX.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 DATA-REL                       PIC  ZZ/ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 HORA-REL                       PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACE.
           05 COD-EVENTO-REL                 PIC  ZZZ.
           05 FILLER                         PIC  X(001) VALUE "-".
           05 EVENTO-REL                     PIC  X(017) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 FORM-REL                       PIC  ZZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PADRAO-REL                     PIC  X(002) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CONTRATO-REL                   PIC  ZZZZ.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CURSO-REL                      PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 LOCAL-REL                      PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 TELAO-REL                      PIC  X      VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACE.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 BECA-REL                       PIC  X      VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CLIP-REL                       PIC  X      VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 IDENT-REL                      PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-08.
           05 FILLER                         PIC  X(132) VALUE
           "|___________________________________________________________
      -    "____________________________________________________________
      -    "___________|".
       02  LINHA-09.
           05 FILLER                         PIC  X(043) VALUE
              "|                                        | ".
           05 ITEM1-REL OCCURS 10 TIMES.
              10 IT-REL                      PIC ZZ.
              10 FILLER                      PIC X(4)    VALUE "  | ".
           05 FILLER                         PIC X(030)  VALUE
           "Q.REP| TOT|     R$-VALOR    |".
       02  LINHA-10.
           05 FILLER                         PIC  X(042) VALUE
              "|                                        |".
           05 TABELA-DATA OCCURS 10 TIMES.
              10 DATA1-REL                   PIC  ZZ/ZZ.
              10 FILLER                      PIC  X(001) VALUE "|".
           05 FILLER                         PIC  X(030) VALUE
           "      |    |                 |".
       02  LINHA-11.
           05 FILLER                         PIC  X(042) VALUE
              "|                                        |".
           05 TABELA-HORA OCCURS 10 TIMES.
              10 HORA1-REL                   PIC  X(005) VALUE SPACES.
              10 FILLER                      PIC  X(001) VALUE "|".
           05 FILLER                         PIC  X(030) VALUE
           "      |    |                 |".
       02  LINHA-12.
           05 FILLER                         PIC  X(042) VALUE
              "|________________________________________|".
           05 TABELA-EVENTO OCCURS 10 TIMES.
              10 EVENTO1-REL                 PIC  X(005) VALUE SPACES.
              10 FILLER                      PIC  X(001) VALUE "|".
           05 FILLER                         PIC  X(030) VALUE
           "      |    |                 |".
       02  LINHA-13.
           05 FILLER                         PIC  X(042) VALUE
              "| IT | COD | NOME          FUNCAO  P  VE |".
           05 TABELA-CONTRATO1 OCCURS 10 TIMES.
              10 CONT1-REL                   PIC  ZZZZ.
              10 FILLER                      PIC  X(002) VALUE " |".
           05 FILLER                         PIC  X(030) VALUE
           "      |    |                 |".
       02  LINHA-13A.
           05 FILLER                         PIC  X(132) VALUE
           "|  0|      | Nr.de Participantes         |     |     |     |
      -    "     |     |     |     |     |     |     |      |    |
      -    "           |".
       02  LINHA-14.
           05 FILLER                         PIC  X(132) VALUE
           "|___|______|_____________________________|_____|_____|_____|
      -    "_____|_____|_____|_____|_____|_____|_____|______|____|______
      -    "___________|".
       02  LINHA-15.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 SEQ-REL                        PIC  Z9.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 CODIGO-REL                     PIC  9(006) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 NOME-REL                       PIC  X(012) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 FUNCAO-REL                     PIC  99-.
           05 NFUNCAO-REL                    PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X      VALUE SPACES.
           05 PADR-REL                       PIC  X(002) VALUE SPACES.
           05 VEIC-REL                       PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE " |".
           05 TABELA-ESCALA OCCURS 10 TIMES.
              10 ESCAL1-REL                  PIC  X(002) VALUE SPACES.
              10 FILLER                      PIC  X(003) VALUE SPACES.
              10 FILLER                      PIC  X(001) VALUE "|".
           05 FILLER                         PIC  X(030) VALUE
           "      |    |                 |".
       02  LINHA-16.
           05 FILLER                         PIC  X(132) VALUE
           "|   |      |                             |     |     |     |
      -    "     |     |     |     |     |     |     |      |    |
      -    "           |".
       02  LINHA-18.
           05 FILLER                         PIC  X(132) VALUE
           "|      COORDENADOR        |        GERENCIA         |
      -    " DIRETORIA        |           CAIXA          |         DIGIT
      -    "ACAO       |".
       02  LINHA-20.
           05 FILLER                         PIC  X(132) VALUE
           "|                         |                         |
      -    "                  |                          |
      -    "           |".
       02  LINHA-21.
           05 FILLER                         PIC  X(132) VALUE
           "|_________________________|_________________________|_______
      -    "__________________|__________________________|______________
      -    "___________|".
       02  LINHA-31.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(009) VALUE
              "VEICULOS:".
       02  LINHA-33.
           05 FILLER                         PIC  X(132) VALUE
           "| IT  CD  MODELO                     PROPRIETARIO       PAS
      -    " DATA-SAIDA  HORAS  LOCAL                                 |
      -    "    KM     |".
       02  LINHA-34.
           05 FILLER                         PIC  X(132) VALUE
           "|___________________________________________________________
      -    "__________________________________________________________|_
      -    "___________|".
       02  LINHA-35.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 ITEM2-REL                      PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CODIGO2-REL                    PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 MODELO2-REL                    PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PROPRIETARIO2-REL              PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PASSAGEIRO2-REL                PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE SPACE.
           05 DATA-S2-REL                    PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 HORA2-REL                      PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 LOCAL2-REL                     PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(017) VALUE SPACE.
           05 FILLER                         PIC  X(002) VALUE " |".
           05 FILLER                         PIC  X(013) VALUE
           "            |".
       02  LINHA-37.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(006) VALUE
              "HOTEL:".
       02  LINHA-38.
           05 FILLER                         PIC  X(132) VALUE
           "|       | COD | HOTEL                           | CONTATO
      -    "         |       FONE       |      VALOR     |   OBS
      -    "           |".
       02  LINHA-39.
           05 FILLER                PIC  X(010) VALUE "| PREV: | ".
           05 CODIGO3-REL                    PIC  Z(003) VALUE ZEROS.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 HOTEL3-REL                     PIC  X(031) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 CONTATO3-REL                   PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE " |(".
           05 DDD3-REL                       PIC  ZZZZ.
           05 FILLER                         PIC  X      VALUE ")".
           05 FONE3-REL                      PIC  ZZZ.ZZZ.ZZZZ.
           05 FILLER                         PIC  X(005) VALUE "|".
           05 VALOR3-REL                     PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE "  |".
           05 OBS3-REL                       PIC  X(024) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-39A.
           05 FILLER                         PIC  X(132) VALUE
           "| REAL: |     |                                 |
      -    "         |                  |                |
      -    "           |".
       02  LINHA-39B.
           05 FILLER                         PIC  X(132) VALUE
           "|_______|_____|_________________________________|___________
      -    "_________|__________________|________________|______________
      -    "___________|".
       02  LINHA-40.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(021) VALUE
              "PREVISAO DE DESPESAS:".
       02  LINHA-41.
           05 FILLER                         PIC  X(132) VALUE
           "|       |    VEICULOS | COMBUSTIVEL | HOSPEDAGENS |  REFEICO
      -    "ES |  PASSAGENS |   ALUGUEIS |  MATERIAIS |     OUTROS |
      -    "     TOTAL |".
       02  LINHA-41A.
           05 FILLER                         PIC  X(132) VALUE
           "|_______|_____________|_____________|_____________|_________
      -    "___|____________|____________|____________|____________|____
      -    "___________|".
       02  LINHA-42.
           05 FILLER                         PIC  X(011) VALUE
           "| PREV: |  ".
           05 VLR-VEICULO4-REL               PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE " | ".
           05 VLR-COMBUSTIVEL4-REL           PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(004) VALUE " | ".
           05 VLR-HOSPEDAGEM4-REL            PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 VLR-REFEICAO4-REL              PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 VLR-PASSAGEM4-REL              PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 VLR-ALUGUEL4-REL               PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 VLR-MATERIAL4-REL              PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 VLR-OUTROS4-REL                PIC  ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 TOTAL4-REL                     PIC  ZZ.ZZZ.ZZZ,ZZ.
           05 FILLER                         PIC  X(002) VALUE " |".
       02  LINHA-42A.
           05 FILLER                         PIC  X(132) VALUE
           "| REAL: |             |             |             |
      -    "   |            |            |            |            |
      -    "           |".
       02  LINHA-43.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(022) VALUE
              "MATERIAIS NECESSARIOS:".
       02  LINHA-44.
           05 FILLER                         PIC  X(132) VALUE
           "| DESCRICAO                                   QTDE
      -    "  |       DESCRICAO                                     QTDE
      -    "           |".
       02  LINHA-44A.
           05 FILLER                         PIC  X(132) VALUE
           "|___________________________________________________________
      -    "__|_________________________________________________________
      -    "___________|".
       02  LINHA-45.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 DESCR5-REL                     PIC  X(041) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 QTDE5-REL                      PIC  ZZ.ZZZ.
           05 FILLER               PIC  X(020) VALUE "            |   ".
           05 DESCR6-REL                     PIC  X(043) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 QTDE6-REL                      PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(012) VALUE
              "           |".
       02  LINHA-46.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(011) VALUE
              "INSTRUCOES:".
       02  LINHA-47.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS7-REL                       PIC  X(130) VALUE SPACES.
           05 FILLER                         PIC  X(015) VALUE "|".
       02  LINHA-48.
           05 FILLER                         PIC  X(011) VALUE
           "| CONTATO: ".
           05 REPRES8-REL                    PIC  X(034) VALUE SPACES.
           05 CONTRATO8-REL                  PIC  ZZZZ.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 CURSO8-REL                     PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 INSTITUICAO8-REL               PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 FONE8-REL                      PIC  ZZZ.ZZZZ.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-49.
           05 FILLER                         PIC  X(132) VALUE
           "| IT   DATA   HORA    EVENTO             FORM   P   CONT  CU
      -    "RSO               LOCAL
      -    "           |".
       02  LINHA-50.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 ITEM9-REL                      PIC  XX.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 DATA9-REL                      PIC  ZZ/ZZ.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 HORA9-REL                      PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACE.
           05 EVENTO9-REL                    PIC  X(017) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 FORM9-REL                      PIC  ZZZZ.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 PADRAO9-REL                    PIC  X(002) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CONTRATO9-REL                  PIC  ZZZZ.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 CURSO9-REL                     PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACE.
           05 LOCAL9-REL                     PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE "   |".
       02  LINHA-51.
           05  FILLER                        PIC  X(132) VALUE
           'RELATORIO DE ATIVIDADES '.

       02  LINHA-PULA.
           05 FILLER                         PIC x(04) VALUE "<br>".

       01 TAB-HTML-X                         PIC X(127).
       01 TAB-HTML-X2 REDEFINES TAB-HTML-X OCCURS 127 TIMES.
          05 TAB-HTML                        PIC X(01).

       01 TAB-HTML-Y                         PIC X(1000).
       01 TAB-HTML-Y2 REDEFINES TAB-HTML-Y OCCURS 1000 TIMES.
          05 TAB-HTML2                       PIC X(01).


       LINKAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED003.
           MOVE "RED005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED005.
           MOVE "RED006"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED006.
           MOVE "PRD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD010.
           MOVE "PRD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD011.
           MOVE "PRD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD012.
           MOVE "PRD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD100.
           MOVE "PRD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD101.
           MOVE "PRD102"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD102.
           MOVE "PRD103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD103.
           MOVE "PRD104"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD104.
           MOVE "PRD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD105.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.

           OPEN INPUT CAD010 COD003 COD040 COD060 CGD001 RED002 RED005
                      RED006 RED003 PRD100 PRD101 PRD102 PRD103 PRD104
                      PRD105 MTD001 COD061 IED011.

           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD100 <> "00"
              MOVE "ERRO ABERTURA PRD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD101 <> "00"
              MOVE "ERRO ABERTURA PRD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD102 <> "00"
              MOVE "ERRO ABERTURA PRD102: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD102 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD103 <> "00"
              MOVE "ERRO ABERTURA PRD103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD104 <> "00"
              MOVE "ERRO ABERTURA PRD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD104 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD105 <> "00"
              MOVE "ERRO ABERTURA PRD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W IMPRESSORA-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.
       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-IMPRIME-RELATORIO-TRUE
                    PERFORM VERIFICA-CODIGO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICA-CODIGO SECTION.

           MOVE GS-NR-PLAN         TO NR-PLAN-W(1: 8)
           MOVE GS-ANO             TO NR-PLAN-W(9: 4)
           MOVE GS-SEQ             TO NR-PLAN-W(13: 2)
           MOVE NR-PLAN-W(1: 8)    TO GS-NR-PLAN
           MOVE NR-PLAN-W(9: 4)    TO GS-ANO
           MOVE NR-PLAN-W(13: 2)   TO GS-SEQ
           MOVE NR-PLAN-W          TO NR-REL

           MOVE SPACES TO ARQUIVO-IMPRESSAO

           STRING "\ARQUIVOS\" NR-REL INTO ARQUIVO-IMPRESSAO

           MOVE GS-NR-PLAN(1: 4)   TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO CIDADE-REL
           MOVE GS-NR-PLAN(5: 4)   TO GRTIME-DATE(5: 4)
           MOVE GS-ANO             TO GRTIME-DATE(1: 4).
           PERFORM VERIFICA-DATA.
       VERIFICA-DATA SECTION.
           MOVE 2         TO GRTIME-TYPE.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA
             PERFORM IMPRIME-PLANEJAMENTO.
      *      verifica se o planejamento já está cadastrado
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GRTIME-DATE   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO DATAI-REL

           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO DATAF-REL.
       CHAMA-GRTIME-ADAY SECTION.
      *    VERIFICA ULTIMO DIA DA SEMANA SOLICITADA
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 6         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".

       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.

           IF PAG-W = 1
              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT
              WRITE REG-RELAT FROM LINHA-01
           ELSE
              MOVE "SALTAR PAGINA" TO REG-RELAT
              WRITE REG-RELAT

              MOVE "INICIO" TO REG-RELAT
              WRITE REG-RELAT

              WRITE REG-RELAT FROM LINHA-01.

           WRITE REG-RELAT FROM LINHA-02

           MOVE 2 TO LIN.

       IMPRIME-PLANEJAMENTO SECTION.
      *    COPY "COND-IMP".

           OPEN OUTPUT RELAT.

           MOVE ZEROS TO PAG-W.
           MOVE "PARTE-1" TO PARTE-REL.
           IF GS-PARTE1 = 1
              PERFORM CABECALHO
              PERFORM IMPRIME-DADOS-EVENTO
              PERFORM IMPRIME-DADOS-EQUIPE
              PERFORM IMPRIME-DADOS-VEICULO.

           IF GS-PARTE2 = 1
              PERFORM IMPRIME-DADOS-HOTEL.
           IF GS-PARTE3 = 1
              PERFORM IMPRIME-DADOS-MATERIAL
              PERFORM IMPRIME-DADOS-INSTRUCOES.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-ATIVIDADE
              PERFORM IMPRIME-EVENTOS-ATIVIDADE
           END-PERFORM.

           MOVE "SALTAR PAGINA" TO REG-RELAT
           WRITE REG-RELAT

      *    AFTER PAGE.

           CLOSE RELAT.
      *    COPY "DESC-IMP".

           IF GS-IMPRIMIR = "S"
              MOVE "S" TO COMPACTA
              CALL "PRP102" USING PARAMETROS-W ARQUIVO-IMPRESSAO
                                  COMPACTA IMPRESSORA-W
              CANCEL "PRP102"
              call "CBL_DELETE_FILE" using     ARQUIVO-IMPRESSAO
                                     returning status-code
              if status-code <> "0000"
                 move "Erro na Exclusão do Arquivo" to mensagem
                move "C" to tipo-msg
                perform exibir-mensagem.



       IMPRIME-DADOS-EVENTO SECTION.
           WRITE REG-RELAT FROM LINHA-03
           WRITE REG-RELAT FROM LINHA-05
           WRITE REG-RELAT FROM LINHA-04
           ADD 3 TO LIN.

           MOVE NR-PLAN-W        TO NR-PLAN-PR101.
           MOVE ZEROS            TO SEQ-EVE-PR101
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                 MOVE "10" TO ST-PRD101.

           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD101
             NOT AT END
                IF NR-PLAN-PR101 <> NR-PLAN-W
                   MOVE "10" TO ST-PRD101
                ELSE
                 MOVE SEQ-EVE-PR101         TO ITEM-REL MAIOR-SEQ-EVENTO
                 MOVE CONTRATO-PR101        TO CONTRATO-REL
                                               NR-CONTRATO-CO60
                                               NR-CONTRATO-CO40
                 MOVE ITEM-PR101            TO ITEM-CO60
                 READ COD060 INVALID KEY
                      INITIALIZE REG-COD060
                 END-READ
                 MOVE CODEVENTO-CO60       TO CODIGO-CO03
                                              COD-EVENTO-REL
                 READ COD003 INVALID KEY
                      MOVE SPACES TO NOME-CO03
                 END-READ
                 MOVE NOME-CO03             TO EVENTO-REL
                 MOVE DATAREALIZA-CO60      TO DATA-INV
                 CALL "GRIDAT1" USING DATA-INV
                 MOVE DATA-INV(1: 4)        TO DATA-REL
                 MOVE QT-PARTICIPANTE-CO60  TO FORM-REL
                 MOVE HORARIO-CO60          TO HORA-REL
                 MOVE LOCAL-CO60            TO LOCAL-REL
                 READ COD040 INVALID KEY
                      INITIALIZE REG-COD040
                 END-READ
                 MOVE IDENTIFICACAO-CO40    TO CURSO-REL
                 MOVE PADRAO-CO40           TO PADRAO-REL
                 IF QT-TELAO-CO60 = ZEROS
                    MOVE SPACES TO TELAO-REL
                 ELSE
                    MOVE QT-TELAO-CO60    TO TELAO-REL
                 END-IF
                 IF BECA-CO60 = 0
                    MOVE SPACES TO BECA-REL
                 ELSE
                    MOVE "S"                TO BECA-REL
                 END-IF
                 IF CLIP-CO60 = 0
                    MOVE SPACES TO CLIP-REL
                 ELSE
                    MOVE "S"                TO CLIP-REL
                 END-IF
                 MOVE CONTRATO-PR101          TO CONTRATO-MT01
                 READ MTD001 INVALID KEY
                      MOVE ZEROS TO CLIEN-ALBUM-MT01
                 END-READ
                 MOVE CLIEN-ALBUM-MT01        TO IDENT-REL
                 ADD 1 TO LIN

                 WRITE REG-RELAT FROM LINHA-06
                 PERFORM VERIFICA-QUEBRA
                END-IF
             END-READ
           END-PERFORM.
           ADD 1 TO LIN
           WRITE REG-RELAT FROM LINHA-08.

       IMPRIME-DADOS-EQUIPE SECTION.
      *    ZERA TABELA
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 90
             MOVE ZEROS  TO CODIGO-TAB(J) FUNCAO-TAB(J) VEICULO-TAB(J)
             MOVE SPACES TO PADRAO-TAB(J)
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 40
               MOVE ZEROS TO SEQ-EV-TAB(J, I)
             END-PERFORM
           END-PERFORM.

      *    grava p/ a tabela a equipe com seus respectivos eventos
           MOVE NR-PLAN-W      TO NR-PLAN-PR100
           MOVE ZEROS          TO CODIGO-PR100 FUNCAO-PR100
           MOVE ZEROS          TO J.
           START PRD100 KEY IS NOT < ALT-PR100 INVALID KEY
              MOVE "10" TO ST-PRD100.

           PERFORM UNTIL ST-PRD100 = "10"
             READ PRD100 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD100
             NOT AT END
                IF NR-PLAN-PR100 <> NR-PLAN-W
                   MOVE "10" TO ST-PRD100
                ELSE
                   IF SEQ-EVE-PR100 > 40
                      CONTINUE
                   ELSE
                      IF CODIGO-PR100 <> CODIGO-ANT OR
                         FUNCAO-PR100 <> FUNCAO-ANT
                         MOVE CODIGO-PR100   TO CODIGO-ANT
                         MOVE FUNCAO-PR100   TO FUNCAO-ANT
                         ADD 1 TO J
                         IF J > 90
                            MOVE "10" TO ST-PRD100
                         ELSE
                            MOVE CODIGO-PR100 TO CODIGO-TAB(J)
                            MOVE FUNCAO-PR100 TO FUNCAO-TAB(J)
                            MOVE PADRAO-PR100 TO PADRAO-TAB(J)
                            MOVE SEQ-VEICULO-PR100 TO VEICULO-TAB(J)
                            MOVE 1       TO SEQ-EV-TAB(J, SEQ-EVE-PR100)
                         END-IF
                      ELSE
                         MOVE 1    TO SEQ-EV-TAB(J, SEQ-EVE-PR100)
                      END-IF
                   END-IF
                END-IF
             END-READ
           END-PERFORM.

      *    ACHA O NÍVEL QUE SE DEVE CHEGAR O CONTADOR DE EVENTO
           EVALUATE MAIOR-SEQ-EVENTO
             WHEN > 30 MOVE 4 TO LIMITE-EVENTO
             WHEN > 20 MOVE 3 TO LIMITE-EVENTO
             WHEN > 10 MOVE 2 TO LIMITE-EVENTO
             WHEN > 00 MOVE 1 TO LIMITE-EVENTO
           END-EVALUATE

      *    LOOP DO M = CONTROLA QUAL INTERVALO DE EVENTO DEVERÁ SER
      *    IMPRESSO. Ex. 1 à 10, 11 à 20 ou 21 à 30, 31 à 40

           MOVE ZEROS TO M
           PERFORM VARYING M FROM 1 BY 1 UNTIL M > LIMITE-EVENTO
      *      DELIMITA OS EVENTOS QUE DEVERÃO SER SELECIONADOS
             EVALUATE M
              WHEN 4 MOVE 31 TO LIM-INF   MOVE 40 TO LIM-SUP
              WHEN 3 MOVE 21 TO LIM-INF   MOVE 30 TO LIM-SUP
              WHEN 2 MOVE 11 TO LIM-INF   MOVE 20 TO LIM-SUP
              WHEN 1 MOVE 01 TO LIM-INF   MOVE 10 TO LIM-SUP
             END-EVALUATE

             PERFORM CABECALHO-EQUIPE

      *      loop do J - controla a equipe a ser impressa dentro do
      *      limite de eventos
             MOVE 0 TO J
             PERFORM VARYING J FROM 1 BY 1
                 UNTIL J > 90 OR CODIGO-TAB(J) = ZEROS
                  MOVE J                 TO SEQ-REL
                  MOVE CODIGO-TAB(J)     TO CODIGO-REL CODIGO-CG01
                  READ CGD001 INVALID KEY
                       MOVE SPACES TO NOME-CG01
                  END-READ
                  MOVE NOME-CG01         TO NOME-REL
                  MOVE FUNCAO-TAB(J)     TO FUNCAO-REL CODIGO-RE02
                  READ RED002 INVALID KEY
                       MOVE SPACES TO DESCRICAO-RE02
                  END-READ
                  MOVE DESCRICAO-RE02    TO NFUNCAO-REL
                  MOVE PADRAO-TAB(J)     TO PADR-REL
                  MOVE VEICULO-TAB(J)    TO VEIC-REL

                  MOVE LIM-INF TO I
                  MOVE ZEROS   TO P
      *           P - CONTROLA A COLUNA DE IMPRESSÃO
                  PERFORM VARYING I FROM I BY 1 UNTIL I > LIM-SUP
                    MOVE I TO P
                    EVALUATE M
                      WHEN 2 SUBTRACT 10 FROM P
                      WHEN 3 SUBTRACT 20 FROM P
                      WHEN 4 SUBTRACT 30 FROM P
                    END-EVALUATE
                    IF SEQ-EV-TAB(J, I) = 1 MOVE "x" TO ESCAL1-REL(P)
                    ELSE MOVE SPACES TO ESCAL1-REL(P)
                    END-IF
                  END-PERFORM

                  WRITE REG-RELAT FROM LINHA-15
                  WRITE REG-RELAT FROM LINHA-14

                  ADD 2 TO LIN
                  IF LIN > 51 PERFORM RODAPE-ASSINATURA
                              PERFORM CABECALHO
                              WRITE REG-RELAT FROM LINHA-03
                              ADD 1 TO LIN
                              PERFORM CABECALHO-EQUIPE
                  END-IF
             END-PERFORM

      *      IMPRIME 5 LINHAS EM BRANCO
             MOVE 0 TO CONT
             PERFORM VARYING CONT FROM 1 BY 1 UNTIL CONT > 5

               WRITE REG-RELAT FROM LINHA-16
               WRITE REG-RELAT FROM LINHA-14

               ADD 2 TO LIN
               IF LIN > 51 PERFORM RODAPE-ASSINATURA
                           PERFORM CABECALHO

                           WRITE REG-RELAT FROM LINHA-03

                           ADD 1 TO LIN
                           IF CONT < 5
                              PERFORM CABECALHO-EQUIPE
                           END-IF
               END-IF
             END-PERFORM

           END-PERFORM.

       CABECALHO-EQUIPE SECTION.
      *    IMPRIME-ITENS
           MOVE LIM-INF          TO I
           MOVE ZEROS            TO P
      *    P - CONTROLA A COLUNA DE IMPRESSÃO
           PERFORM VARYING I FROM I BY 1 UNTIL I > LIM-SUP
               ADD 1 TO P
               MOVE I                      TO IT-REL(P)
               MOVE I                      TO SEQ-EVE-PR101
               MOVE NR-PLAN-W              TO NR-PLAN-PR101
               READ PRD101 INVALID KEY
                    INITIALIZE REG-PRD101
               END-READ
               MOVE CONTRATO-PR101         TO NR-CONTRATO-CO60
               MOVE ITEM-PR101             TO ITEM-CO60
               READ COD060 INVALID KEY
                    INITIALIZE REG-COD060
               END-READ
               MOVE DATAREALIZA-CO60(5: 2) TO DIAMES-W(3: 2)
               MOVE DATAREALIZA-CO60(7: 2) TO DIAMES-W(1: 2)
               MOVE DIAMES-W       TO DATA1-REL(P)
               MOVE HORARIO-CO60(1: 5)     TO HORA1-REL(P)
               MOVE CODEVENTO-CO60         TO CODIGO-CO03
               READ COD003 INVALID KEY
                    MOVE SPACES TO NOME-CO03
               END-READ
               MOVE NOME-CO03(1: 5)        TO EVENTO1-REL(P)
               MOVE NR-CONTRATO-CO60       TO CONT1-REL(P)
           END-PERFORM.

           WRITE REG-RELAT FROM LINHA-09

           WRITE REG-RELAT FROM LINHA-10.

           WRITE REG-RELAT FROM LINHA-11.

           WRITE REG-RELAT FROM LINHA-12.

           WRITE REG-RELAT FROM LINHA-13.

           WRITE REG-RELAT FROM LINHA-14.

           WRITE REG-RELAT FROM LINHA-13A.

           WRITE REG-RELAT FROM LINHA-14.

           ADD 8 TO LIN.

       IMPRIME-DADOS-VEICULO SECTION.
           COMPUTE LINHA = LIN + 6.

           IF LINHA > 51 PERFORM RODAPE-ASSINATURA
                         PERFORM CABECALHO.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-31
      *    AFTER 2.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-03
      *    AFTER 2.


           WRITE REG-RELAT FROM LINHA-33

           WRITE REG-RELAT FROM LINHA-34

           ADD 6 TO LIN.
           IF LIN > 51 PERFORM RODAPE-ASSINATURA
                       PERFORM CABECALHO.

           MOVE NR-PLAN-W TO NR-PLAN-PR102.
           MOVE ZEROS     TO SEQ-VEI-PR102.
           START PRD102 KEY IS NOT < CHAVE-PR102 INVALID KEY
                 MOVE "10" TO ST-PRD102.

           PERFORM UNTIL ST-PRD102 = "10"
             READ PRD102 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD102
             NOT AT END
                 IF NR-PLAN-PR102 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD102
                 ELSE
                  MOVE SEQ-VEI-PR102       TO ITEM2-REL
                  MOVE CODIGO-PR102        TO CODIGO2-REL CODIGO-RE06
                  READ RED006 INVALID KEY
                       INITIALIZE REG-RED006
                  END-READ
                  MOVE VEICULO-RE06        TO MODELO2-REL
                  MOVE PROPRIETARIO-RE06   TO CODIGO-CG01
                  READ CGD001 INVALID KEY
                       MOVE SPACES TO NOME-CG01
                  END-READ
                  MOVE NOME-CG01           TO PROPRIETARIO2-REL
                  MOVE QTDE-PASSAG-RE06    TO PASSAGEIRO2-REL
                  MOVE DATA-SAIDA-PR102    TO DATA-S2-REL
                  MOVE HORA-PR102          TO HORA2-REL
                  MOVE LOCAL-PR102         TO LOCAL2-REL

                  WRITE REG-RELAT FROM LINHA-35

                  WRITE REG-RELAT FROM LINHA-34

                  ADD 2 TO LIN
                  IF LIN > 51 PERFORM RODAPE-ASSINATURA
                              PERFORM CABECALHO
                  END-IF
                 END-IF
             END-READ
           END-PERFORM.

           ADD 10 TO LIN.
           IF LIN > 51 SUBTRACT 10 FROM LIN
                       PERFORM RODAPE-ASSINATURA
                       PERFORM CABECALHO
                       ADD 10 TO LIN.
      *    imprime previsão financeira

           MOVE NR-PLAN-W TO NR-PLAN-PR105.
           READ PRD105 INVALID KEY INITIALIZE REG-PRD105
           END-READ

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-40
      *    AFTER 2


           WRITE REG-RELAT FROM LINHA-03
      *    AFTER 2


           WRITE REG-RELAT FROM LINHA-41

           WRITE REG-RELAT FROM LINHA-41A

           MOVE PREV-VEIC-PR105        TO VLR-VEICULO4-REL
           MOVE ZEROS                  TO VLR-COMBUSTIVEL4-REL
           MOVE PREV-HOSP-PR105        TO VLR-HOSPEDAGEM4-REL
           MOVE PREV-REFEIC-PR105      TO VLR-REFEICAO4-REL
           MOVE ZEROS                  TO VLR-PASSAGEM4-REL
           MOVE ZEROS                  TO VLR-ALUGUEL4-REL
           MOVE ZEROS                  TO VLR-MATERIAL4-REL
           MOVE PREV-OUTROS-PR105      TO VLR-OUTROS4-REL
           ADD PREV-OUTROS-PR105 PREV-REFEIC-PR105 PREV-VEIC-PR105
               PREV-HOSP-PR105   GIVING TOTAL4-REL.

           WRITE REG-RELAT FROM LINHA-42

           WRITE REG-RELAT FROM LINHA-41A

           WRITE REG-RELAT FROM LINHA-42A

           WRITE REG-RELAT FROM LINHA-41A

           PERFORM RODAPE-ASSINATURA.

       IMPRIME-DADOS-HOTEL SECTION.
           MOVE "PARTE-2" TO PARTE-REL.
           PERFORM CABECALHO.
      *    não há necessidade de controle de linhas nesta pagina

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-37
      *    AFTER 2

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-03
      *    AFTER 2

           WRITE REG-RELAT FROM LINHA-38.

           WRITE REG-RELAT FROM LINHA-39B.

           ADD 6 TO LIN

           MOVE NR-PLAN-W TO NR-PLAN-PR105.
           READ PRD105 INVALID KEY
                INITIALIZE REG-PRD105
           END-READ
           MOVE HOTEL-PR105          TO CODIGO-RE05 CODIGO3-REL
           READ RED005 INVALID KEY
                INITIALIZE REG-RED005.
           MOVE CIDADE-RE05          TO CIDADE
           READ CAD010 INVALID KEY
                INITIALIZE REG-CAD010.
           MOVE HOTEL-RE05           TO HOTEL3-REL
           MOVE CONTATO-PR105        TO CONTATO3-REL
           MOVE DDD-CID              TO DDD3-REL
           MOVE FONE1-RE05           TO FONE3-REL
           MOVE VALOR-DIARIA-PR105   TO VALOR3-REL
           MOVE OBS-PR105            TO OBS3-REL.
           WRITE REG-RELAT FROM LINHA-39

           WRITE REG-RELAT FROM LINHA-39B

           WRITE REG-RELAT FROM LINHA-39A

           WRITE REG-RELAT FROM LINHA-39B.

           WRITE REG-RELAT FROM LINHA-39A

           WRITE REG-RELAT FROM LINHA-39B.

           ADD 6 TO LIN

      *    imprime previsão financeira

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-40
      *    AFTER 2

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-03
      *    AFTER 2

           WRITE REG-RELAT FROM LINHA-41

           WRITE REG-RELAT FROM LINHA-41A

           ADD 6 TO LIN

           MOVE PREV-VEIC-PR105        TO VLR-VEICULO4-REL
           MOVE ZEROS                  TO VLR-COMBUSTIVEL4-REL
           MOVE PREV-HOSP-PR105        TO VLR-HOSPEDAGEM4-REL
           MOVE PREV-REFEIC-PR105      TO VLR-REFEICAO4-REL
           MOVE ZEROS                  TO VLR-PASSAGEM4-REL
           MOVE ZEROS                  TO VLR-ALUGUEL4-REL
           MOVE ZEROS                  TO VLR-MATERIAL4-REL
           MOVE PREV-OUTROS-PR105      TO VLR-OUTROS4-REL
           ADD PREV-OUTROS-PR105 PREV-REFEIC-PR105 PREV-VEIC-PR105
               PREV-HOSP-PR105   GIVING TOTAL4-REL.
           WRITE REG-RELAT FROM LINHA-42

           WRITE REG-RELAT FROM LINHA-41A

           WRITE REG-RELAT FROM LINHA-42A

           WRITE REG-RELAT FROM LINHA-41A.

           ADD 4 TO LIN

           PERFORM RODAPE-ASSINATURA.

       RODAPE-ASSINATURA SECTION.
           COMPUTE LIN4 = 53 - LIN.

           MOVE 0 TO IND3
           PERFORM UNTIL LIN4 = IND3
               ADD 1 TO IND3

               MOVE SPACES TO REG-RELAT
               WRITE REG-RELAT

           END-PERFORM

           WRITE REG-RELAT FROM LINHA-03
      *    AFTER LIN


           WRITE REG-RELAT FROM LINHA-18

           WRITE REG-RELAT FROM LINHA-20

           WRITE REG-RELAT FROM LINHA-20

           WRITE REG-RELAT FROM LINHA-21.


       IMPRIME-DADOS-MATERIAL SECTION.
           MOVE "PARTE-3" TO PARTE-REL.
           PERFORM CABECALHO.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-43
      *    AFTER 2.


           WRITE REG-RELAT FROM LINHA-03.

           WRITE REG-RELAT FROM LINHA-44.

           WRITE REG-RELAT FROM LINHA-44A.

           ADD 5 TO LIN.

           MOVE NR-PLAN-W        TO NR-PLAN-PR103.
           MOVE ZEROS            TO CODIGO-PR103.

           START PRD103 KEY IS NOT < CHAVE-PR103 INVALID KEY
                 MOVE "10" TO ST-PRD103.

           PERFORM UNTIL ST-PRD103 = "10"
             READ PRD103 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD103
             NOT AT END
                 IF NR-PLAN-PR103 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD103
                 ELSE
                    MOVE CODIGO-PR103    TO CODIGO-RE03
                    READ RED003 INVALID KEY
                         MOVE SPACES TO DESCRICAO-RE03
                    END-READ
                    MOVE DESCRICAO-RE03  TO DESCR5-REL
                    MOVE QTDE-PR103      TO QTDE5-REL
                    READ PRD103 NEXT RECORD AT END
                         MOVE SPACES TO DESCR6-REL
                         MOVE ZEROS  TO QTDE6-REL
                         WRITE REG-RELAT FROM LINHA-45

                         ADD 1 TO LIN

                         MOVE "10" TO ST-PRD103
                    NOT AT END
                      IF NR-PLAN-PR103 <> NR-PLAN-W
                         MOVE SPACES TO DESCR6-REL
                         MOVE ZEROS  TO QTDE6-REL

                         WRITE REG-RELAT FROM LINHA-45

                         ADD 1 TO LIN
                         MOVE "10" TO ST-PRD103
                      ELSE
                        MOVE CODIGO-PR103    TO CODIGO-RE03
                        READ RED003 INVALID KEY MOVE
                              SPACES TO DESCRICAO-RE03
                        END-READ
                        MOVE DESCRICAO-RE03  TO DESCR6-REL
                        MOVE QTDE-PR103      TO QTDE6-REL

                        WRITE REG-RELAT FROM LINHA-45

                        ADD 1 TO LIN
                      END-IF
                   END-READ
                 END-IF
             END-READ
           END-PERFORM.

           WRITE REG-RELAT FROM LINHA-44A

           ADD 1 TO LIN.
       IMPRIME-DADOS-INSTRUCOES SECTION.
           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-46
      *    AFTER 2

           WRITE REG-RELAT FROM LINHA-03

           ADD 3 TO LIN.
           MOVE NR-PLAN-W    TO NR-PLAN-PR101
           MOVE ZEROS        TO CONTRATO-PR101 CONTRATO-ANT
           START PRD101 KEY IS NOT < ALT-PR101 INVALID KEY
                 MOVE "10" TO ST-PRD101.

           PERFORM UNTIL ST-PRD101 = "10"
            READ PRD101 NEXT RECORD AT END
                 MOVE "10"  TO ST-PRD101
            NOT AT END
                IF NR-PLAN-PR101 <> NR-PLAN-W
                   MOVE "10" TO ST-PRD101
                ELSE
                  IF CONTRATO-PR101 <> CONTRATO-ANT
                     MOVE CONTRATO-PR101 TO CONTRATO-ANT
                                            CODIGO-CG12(1: 4)
                     MOVE ZEROS          TO CODIGO-CG12(5: 4)
                     PERFORM PROCURA-PRESIDENTE
                  END-IF
                END-IF
            END-READ
           END-PERFORM.
           PERFORM IMPRIME-OBS-PLANEJAMENTO
           PERFORM IMPRIME-OBS-GERAL
           PERFORM IMPRIME-OBS-EQUIPE-REPORT
           WRITE REG-RELAT FROM LINHA-08

           PERFORM IMPRIME-CURSOS

           PERFORM RODAPE-ASSINATURA.

       IMPRIME-CURSOS SECTION.
           MOVE
           "|SEQ CONTRATO CURSO                TURMA   NR.FORM.
      -    "DATA   HORA    EVENTO
      -    "           |"  TO LINDET-REL.

           WRITE REG-RELAT FROM LINDET-REL

           WRITE REG-RELAT FROM LINHA-08
           ADD 2 TO LIN

           INITIALIZE REG-PRD101
           MOVE NR-PLAN-W        TO NR-PLAN-PR101.
           MOVE ZEROS            TO SEQ-EVE-PR101
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                 MOVE "10" TO ST-PRD101.

           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD101
             NOT AT END
                IF NR-PLAN-PR101 <> NR-PLAN-W
                   MOVE "10" TO ST-PRD101
                ELSE
                   MOVE SPACES             TO LINDET-REL
                   MOVE "S"                TO PRIMEIRO
                   MOVE SEQ-EVE-PR101      TO LINDET-REL(3:4)
                   MOVE NR-PLAN-PR101      TO LINDET-REL(10:4)
                   INITIALIZE REG-COD061
                   MOVE CONTRATO-PR101     TO NR-CONTRATO-CO61
                                              NR-CONTRATO-CO60
                   MOVE ITEM-PR101         TO ITEM-CO61
                                              ITEM-CO60
                   READ COD060 INVALID KEY
                        MOVE ZEROS         TO REG-COD060
                   END-READ

                   MOVE QT-PARTICIPANTE-CO60 TO MASC-QTDE
                   MOVE MASC-QTDE          TO LINDET-REL(47:7)

                   MOVE DATAREALIZA-CO60(5: 2) TO DIAMES-W(3: 2)
                   MOVE DATAREALIZA-CO60(7: 2) TO DIAMES-W(1: 2)
                   MOVE DIAMES-W               TO MASC-DATA
                   MOVE MASC-DATA              TO LINDET-REL(61:5)
                   MOVE HORARIO-CO60(1: 5)     TO LINDET-REL(67:5)
                   MOVE CODEVENTO-CO60         TO CODIGO-CO03
                   READ COD003 INVALID KEY
                        MOVE SPACES TO NOME-CO03
                   END-READ
                   STRING NOME-CO03(1:30) DELIMITED BY "   "
                                               INTO LINDET-REL(76:30)

                   START COD061 KEY IS NOT LESS CHAVE-CO61 INVALID KEY
                         MOVE "10" TO ST-COD061
                   END-START
                   PERFORM UNTIL ST-COD061 = "10"
                       READ COD061 NEXT RECORD AT END
                            MOVE "10" TO ST-COD061
                       NOT AT END
                            IF CONTRATO-PR101  <> NR-CONTRATO-CO61 OR
                               ITEM-PR101      <> ITEM-CO61
                               MOVE "10" TO ST-COD061
                            ELSE
                               IF PRIMEIRO = "N"
                                  MOVE SPACES       TO LINDET-REL(3:14)
                                  MOVE SPACES       TO LINDET-REL(47:7)
                                  MOVE SPACES       TO LINDET-REL(61:5)
                                  MOVE SPACES       TO LINDET-REL(67:5)
                                  MOVE SPACES       TO LINDET-REL(76:30)
                               END-IF
                               MOVE CURSO-CO61      TO CODIGO-IE11
                               READ IED011 INVALID KEY
                                    MOVE "------"   TO NOME-REDUZ-IE11
                               END-READ
                               MOVE NOME-REDUZ-IE11 TO LINDET-REL(15:20)
                               MOVE TURMA-CO61      TO LINDET-REL(36:2)
                               MOVE "N" TO PRIMEIRO
                               WRITE REG-RELAT FROM LINDET-REL
                               ADD 1 TO LIN
                               IF LIN > 51
                                  PERFORM RODAPE-ASSINATURA
                                  PERFORM CABECALHO
                                  WRITE REG-RELAT FROM LINHA-08
                                  MOVE
           "|SEQ CONTRATO CURSO                TURMA   NR.FORM.
      -    "DATA   HORA    EVENTO
      -    "           |"  TO LINDET-REL

                                  WRITE REG-RELAT FROM LINDET-REL
                                  MOVE SPACES TO LINDET-REL
                                  WRITE REG-RELAT FROM LINHA-08
                                  ADD 3 TO LIN

                               END-IF
                           END-IF
                       END-READ
                   END-PERFORM
                END-IF
             END-READ
           END-PERFORM.

       PROCURA-PRESIDENTE SECTION.
           OPEN INPUT IED010 CGD010 CGD011 CGD012 PRD012.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PRD012 <> "00"
              MOVE "ERRO ABERTURA PRD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           START CGD012 KEY IS NOT < CODIGO-CG12 INVALID KEY
                 MOVE "10" TO ST-CGD012.
           MOVE SPACES TO OBS7-REL.
           PERFORM UNTIL ST-CGD012 = "10"
            READ CGD012 NEXT RECORD AT END MOVE "10" TO ST-CGD012
              NOT AT END
              MOVE CODIGO-CG12(1: 4) TO CONTRATO-W
              IF CONTRATO-W <> CONTRATO-PR101 MOVE "10" TO ST-CGD012
              ELSE
                 IF CARGO-COMISSAO-CG12 = 1
                    MOVE 0              TO CLASSIF-CG10 CLASSIF-CG11
                    MOVE CODIGO-CG12    TO CODIGO-CG10 CODIGO-CG11
                    READ CGD010 INVALID KEY
                                 MOVE SPACES TO COMPRADOR-CG10
                    END-READ
                    MOVE COMPRADOR-CG10 TO REPRES8-REL
                    READ CGD011 INVALID KEY MOVE ZEROS TO FONE1-CG11
                    END-READ
                    MOVE FONE1-CG11     TO FONE8-REL
                    MOVE CONTRATO-PR101 TO CONTRATO8-REL
                                           NR-CONTRATO-CO40
                    READ COD040 INVALID KEY INITIALIZE REG-COD040
                    END-READ
                    MOVE IDENTIFICACAO-CO40 TO CURSO8-REL
                    MOVE INSTITUICAO-CO40   TO CODIGO-IE10
                    READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
                    END-READ
                    MOVE NOME-IE10          TO INSTITUICAO8-REL

                    WRITE REG-RELAT FROM LINHA-48

                    ADD 1 TO LIN
                    INITIALIZE REG-PRD012
                    MOVE CONTRATO-W         TO CONTRATO-PR12
                    START PRD012 KEY IS NOT LESS CHAVE-PR12 INVALID KEY
                       MOVE "10" TO ST-PRD012
                    END-START
                    PERFORM UNTIL ST-PRD012 = "10"
                       READ PRD012 NEXT RECORD AT END
                           MOVE "10" TO ST-PRD012
                       NOT AT END
                           IF CONTRATO-W <> CONTRATO-PR12
                              MOVE "10" TO ST-PRD012
                           ELSE
                              MOVE OBSERVACAO-PR12 TO OBS7-REL
                              WRITE REG-RELAT FROM LINHA-47
                              ADD 1 TO LIN
                           END-IF
                    END-PERFORM
                 END-IF
              END-IF
            END-READ
           END-PERFORM.
           CLOSE IED010 CGD010 CGD011 CGD012 PRD012.

       IMPRIME-OBS-PLANEJAMENTO SECTION.
           MOVE SPACES        TO OBS7-REL

           WRITE REG-RELAT FROM LINHA-47

           ADD 1 TO LIN.
           MOVE NR-PLAN-W     TO NR-PLAN-PR104
           MOVE ZEROS         TO SEQ-OBS-PR104.
           START PRD104 KEY IS NOT < CHAVE-PR104 INVALID KEY
                 MOVE "10" TO ST-PRD104.

           PERFORM UNTIL ST-PRD104 = "10"
             READ PRD104 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD104
             NOT AT END
                 IF NR-PLAN-PR104 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD104
                 ELSE
                    MOVE 1 TO I
                    MOVE 1 TO INICIO
                    MOVE "N" TO NAO
                    PERFORM UNTIL I = 80
                         IF OBSERVACAO-PR104(I:1) = X"0a"
                            MOVE "S" TO NAO
                            MOVE SPACES TO OBS7-REL
                            STRING OBSERVACAO-PR104(INICIO:I)
                                   DELIMITED BY X"0a" INTO OBS7-REL
                            WRITE REG-RELAT FROM LINHA-47
                            MOVE I TO INICIO
                            ADD  1 TO INICIO
                         END-IF
                         ADD 1 TO I
                    END-PERFORM
                    IF NAO = "N"
                       MOVE OBSERVACAO-PR104 TO OBS7-REL
                       WRITE REG-RELAT FROM LINHA-47
                    ELSE
                       MOVE SPACES TO OBS7-REL
                       STRING OBSERVACAO-PR104(INICIO:80)
                              DELIMITED BY "    " INTO OBS7-REL
                       WRITE REG-RELAT FROM LINHA-47
                    END-IF
                    PERFORM VERIFICA-QUEBRA
                 END-IF
             END-READ
           END-PERFORM.
       IMPRIME-OBS-GERAL SECTION.
           OPEN INPUT PRD010.
           READ PRD010.
           MOVE SPACES TO OBS7-REL.

           WRITE REG-RELAT FROM LINHA-47

           ADD 1 TO LIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF OBS-PR010(I) = SPACES
                  CONTINUE
               ELSE
                   MOVE OBS-PR010(I) TO OBS7-REL
                   WRITE REG-RELAT FROM LINHA-47

                   PERFORM VERIFICA-QUEBRA
               END-IF
           END-PERFORM.
           MOVE SPACES TO OBS7-REL
           WRITE REG-RELAT FROM LINHA-47
           ADD 1 TO LIN.
           CLOSE PRD010.
       IMPRIME-OBS-EQUIPE-REPORT SECTION.
           OPEN I-O PRD011.
           MOVE NR-PLAN-W      TO NR-PLAN-PR100.
           MOVE ZEROS          TO CODIGO-PR100 CODIGO-ANT.
           START PRD100 KEY IS NOT < ALT-PR100 INVALID KEY
                 MOVE "10" TO ST-PRD100.

           PERFORM UNTIL ST-PRD100 = "10"
             READ PRD100 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD100
             NOT AT END
                IF NR-PLAN-W <> NR-PLAN-PR100
                   MOVE "10" TO ST-PRD100
                ELSE
                  IF CODIGO-PR100 <> CODIGO-ANT
                    MOVE CODIGO-PR100 TO CODIGO-ANT CODIGO-PR11
                    MOVE 1            TO VEZ-APRESENTA-PR11
                    START PRD011 KEY IS NOT < ALT-PR11 INVALID KEY
                       MOVE "10" TO ST-PRD011
                    END-START
                    PERFORM VERIFICA-VEZ-APRESENTA
                  END-IF
                END-IF
             END-READ
           END-PERFORM.
           CLOSE PRD011.
       VERIFICA-VEZ-APRESENTA SECTION.

           PERFORM UNTIL ST-PRD011 = "10"
             READ PRD011 NEXT RECORD AT END MOVE "10" TO ST-PRD011
              NOT AT END
                IF CODIGO-PR11 <> CODIGO-PR100
                   MOVE "10" TO ST-PRD011
                ELSE
                   IF VEZ-APRESENTA-PR11 <> ZEROS
                      SUBTRACT 1 FROM VEZ-APRESENTA-PR11
                      REWRITE REG-PRD011
                      END-REWRITE
                      MOVE CODIGO-PR11      TO CODIGO-CG01
                      READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                      END-READ
                      MOVE SPACES           TO OBS7-REL
                      MOVE NOME-CG01        TO OBS7-REL(1: 40)
                      MOVE DATA-REPORT-PR11 TO DATA-E
                      MOVE DATA-E           TO OBS7-REL(41: 15)
                      MOVE CIDADE-PR11      TO CIDADE
                      READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                      END-READ
                      MOVE NOME-CID         TO OBS7-REL(56: 14)

                      WRITE REG-RELAT FROM LINHA-47

                      PERFORM VERIFICA-QUEBRA
                      MOVE SPACES           TO OBS7-REL
                      IF OBSERVACAO-PR11(1) <> SPACES
                         MOVE OBSERVACAO-PR11(1)  TO OBS7-REL
                         WRITE REG-RELAT   FROM LINHA-47
                         PERFORM VERIFICA-QUEBRA
                      END-IF
                      IF OBSERVACAO-PR11(2) <> SPACES
                         MOVE OBSERVACAO-PR11(2)  TO OBS7-REL
                         WRITE REG-RELAT   FROM LINHA-47
                         PERFORM VERIFICA-QUEBRA
                      END-IF
                      IF OBSERVACAO-PR11(3) <> SPACES
                         MOVE OBSERVACAO-PR11(3)  TO OBS7-REL
                         WRITE REG-RELAT   FROM LINHA-47
                         PERFORM VERIFICA-QUEBRA
                      END-IF
                   END-IF
                END-IF
             END-READ
           END-PERFORM.
      *    MOVE SPACES TO REG-RELAT.
      *    WRITE REG-RELAT.
      *    ADD 1 TO LIN.

       VERIFICA-QUEBRA SECTION.
           ADD 1 TO LIN
           IF LIN > 51
              PERFORM RODAPE-ASSINATURA
              PERFORM CABECALHO
           END-IF.

       IMPRIME-EVENTOS-ATIVIDADE SECTION.
           PERFORM CABECALHO.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-03
      *    AFTER 2.

           WRITE REG-RELAT FROM LINHA-49.

           WRITE REG-RELAT FROM LINHA-04.

           ADD 4 TO LIN.
           MOVE NR-PLAN-W    TO NR-PLAN-PR101.
           MOVE ZEROS        TO SEQ-EVE-PR101
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                 MOVE "10" TO ST-PRD101.

           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD101
             NOT AT END
                IF NR-PLAN-PR101 <> NR-PLAN-W
                   MOVE "10" TO ST-PRD101
                ELSE
                   MOVE SEQ-EVE-PR101         TO ITEM9-REL
                   MOVE CONTRATO-PR101        TO CONTRATO9-REL
                                                 NR-CONTRATO-CO60
                                                 NR-CONTRATO-CO40
                   MOVE ITEM-PR101            TO ITEM-CO60
                   READ COD060 INVALID KEY
                        INITIALIZE REG-COD060
                   END-READ
                   MOVE CODEVENTO-CO60        TO CODIGO-CO03
                   READ COD003 INVALID KEY
                        MOVE SPACES TO NOME-CO03
                   END-READ
                   MOVE NOME-CO03             TO EVENTO9-REL
                   MOVE DATAREALIZA-CO60      TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV(1: 4)        TO DATA9-REL
                   MOVE QT-PARTICIPANTE-CO60  TO FORM9-REL
                   MOVE HORARIO-CO60          TO HORA9-REL
                   MOVE LOCAL-CO60            TO LOCAL9-REL
                   READ COD040 INVALID KEY
                        INITIALIZE REG-COD040
                   END-READ
                   MOVE IDENTIFICACAO-CO40    TO CURSO9-REL
                   MOVE PADRAO-CO40           TO PADRAO9-REL

                   WRITE REG-RELAT FROM LINHA-50

                   ADD 1 TO LIN
                END-IF
             END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-08.

           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           WRITE REG-RELAT FROM LINHA-51
      *    AFTER 2.

           WRITE REG-RELAT FROM LINHA-03.

           ADD 4 TO LIN.
           PERFORM VARYING LIN FROM LIN BY 2 UNTIL LIN > 56
              WRITE REG-RELAT FROM LINHA-04

              WRITE REG-RELAT FROM LINHA-08
           END-PERFORM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
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
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "PRP101" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       COLOCA-SPACOS-HTML SECTION.
           MOVE SPACES TO TAB-HTML-Y

           MOVE 0 TO IND
           MOVE 0 TO IND2
           PERFORM UNTIL IND = 127
               ADD 1 TO IND
               IF TAB-HTML(IND) = " "
                  ADD   1  TO IND2
                  MOVE "&" TO TAB-HTML2(IND2)
                  ADD   1  TO IND2
                  MOVE "n" TO TAB-HTML2(IND2)
                  ADD   1  TO IND2
                  MOVE "b" TO TAB-HTML2(IND2)
                  ADD   1  TO IND2
                  MOVE "s" TO TAB-HTML2(IND2)
                  ADD   1  TO IND2
                  MOVE "p" TO TAB-HTML2(IND2)
                  ADD   1  TO IND2
                  MOVE ";" TO TAB-HTML2(IND2)
               ELSE
                  ADD 1 TO IND2
                  MOVE TAB-HTML(IND) TO TAB-HTML2(IND2)
               END-IF
           END-PERFORM.

       EXIBIR-MENSAGEM SECTION.
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
      *--------------------------------------------------------------
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 CGD001 RED002
                 RED003 RED005 RED006 PRD100 PRD101 PRD102
                 PRD103 PRD104 PRD105 MTD001 COD061 IED011
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

