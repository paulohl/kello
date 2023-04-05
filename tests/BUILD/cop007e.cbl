       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP007e.
      *--------------------------------------------------------------*
      *    DATA  : 16 AGOSTO 2005
      *    AUTOR : ALFREDO SAVIOLLI NETO
      *    FUNCAO: GERA ARQUIVO 05 - PRE-GRAVADO - EXECUTÁVEL
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       PRINTER IS LPRINTER
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX001.
           COPY COPX050.
           COPY COPX040.
           COPY COPX002.
           COPY CAPX010.
           COPY COPX005.
           COPY MTPX001.
           COPY MTPX020.
           COPY RCPX100.
           COPY COPX007.

       DATA DIVISION.
       FILE SECTION.

       COPY CAPW001.
       COPY CGPW001.
       COPY COPW050.
       COPY COPW040.
       COPY COPW002.
       COPY COPW005.
       COPY CAPW010.
       COPY MTPW001.
       COPY MTPW020.
       COPY RCPW100.
       COPY COPW007.

       WORKING-STORAGE SECTION.
       copy cpaday1.
       copy cpdias1.
       COPY "DS-CNTRL.MF".
       COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).

       01  REG-WORK.
           05  CONTRATO-WK          PIC 9(4).
           05  DATA-WK              PIC 9(8).
           05  ALBPROD-WK           PIC 9999.
           05  ALBVEND-WK           PIC 9999.
           05  FOTPROD-WK           PIC 9(06).
           05  FOTVEND-WK           PIC 9(05).
           05  FOTAVUL-WK           PIC 9(05).
           05  FOTMONT-WK           PIC 9(06).
           05  FOTFOGO-WK           PIC 9(05).
           05  FOTDEVO-WK           PIC S9(5).
           05  FITA-WK              PIC 9(05).
           05  FATVENBRU-WK         PIC 9(11)V99.
           05  PRAZO-WK             PIC 9(14)V99.
           05  VENLIQDEF-WK         PIC 9(10)V99.
           05  VENLIQIND-WK         PIC 9(10)V99.
           05  ALBPERD-WK           PIC 999.
           05  FOTPERD-WK           PIC 9999.
           05  FOTCOMIS-WK          PIC 9(5).
           05  VENDAVUL-WK          PIC 9(5).
           05  FORMANDOS-WK         PIC 9(05).
       01  linha-gera.
           05 filler pic x(11) value'Gerado em :'.
           05 data-ger pic 99/99/9999.
           05 filler pic x(4) value' as '.
           05 hora-ger  pic z9.
           05 filler pic x value ':'.
           05 min-ger pic 99.
       01  numero-reg   pic 999.
       01  numero-reg1  pic 999.
       01  data-g         pic 9(8).
       01  data1-g        pic 9(6).
       01  VARIAVEIS.
      *    05  DATA-COMPLETA.
      *        10 ANO-COMP  PIC 9(4).
      *        10 MES-COMP  PIC 99.
      *        10 DIA-COMP  PIC 99.
      *        10 FILLER    PIC X(13).
           05  ST-CAD001       PIC XX          VALUE SPACES.
           05  ST-CAD010       PIC XX          VALUE SPACES.
           05  ST-CGD001       PIC XX          VALUE SPACES.
           05  ST-COD040       PIC XX          VALUE SPACES.
           05  ST-COD002       PIC XX          VALUE SPACES.
           05  ST-COD005       PIC XX          VALUE SPACES.
           05  ST-MTD001       PIC XX          VALUE SPACES.
           05  ST-MTD020       PIC XX          VALUE SPACES.
           05  ST-COD050       PIC XX          VALUE SPACES.
           05  ST-RCD100       PIC XX          VALUE SPACES.
           05  ST-COD007       PIC XX          VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.

           05  CONF            PIC X           VALUE SPACES.
           05  OP              PIC 9           VALUE ZEROS.
           05  TRACO           PIC X(70)       VALUE ALL '_'.
           05  REC-FILE.
               10  DRI-VE      PIC X           VALUE "\".
               10  EMP-REC     PIC XXX.
               10  BAR-RA      PIC X           VALUE "\".
               10  RES-TO      PIC X(7)        VALUE SPACES.
           05  RECFILE REDEFINES REC-FILE PIC X(12).
           05  ANO             PIC 99          VALUE ZEROS.
           05  MESANO1.
               10  ANO-1       PIC 9999.
               10  MES-1       PIC 99.
           05  MESANO-1 REDEFINES MESANO1 PIC 9(6).
           05  MESANOW.
               10  MES-W1      PIC 99.
               10  ANO-W1      PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  ALBUMW.
               10  CONTR-W     PIC 9(04).
               10  ALB-W       PIC 9999.
           05  EVENTO-W REDEFINES ALBUMW PIC 9(08).
           05  DATA-AUX        PIC 9(08)       VALUE ZEROS.
           05  DATAW.
               10  DIA-W       PIC 99.
               10  MES-W       PIC 99.
               10  ANO-W       PIC 9999.
           05  DATA-W REDEFINES DATAW PIC 9(08).
           05  DATAI.
               10  ANO-I       PIC 9999.
               10  MES-I       PIC 99.
               10  DIA-I       PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(08).
           05  FLAG-GERAL      PIC 9             VALUE ZEROS.
           05  TOTAL-FORMANDOS PIC 9(06)         VALUE ZEROS.
           05  DIVISAO-W       PIC 9(6)V9(6)     VALUE ZEROS.
           05  FOTDEVO-W       PIC S9(5)         VALUE ZEROS.
           05  I               PIC 99            VALUE ZEROS.
           05  J               PIC 99            VALUE ZEROS.
           05  CUSTO-PREVISTO        PIC 9(8)V99  VALUE ZEROS.
           05  CUSTO-W               PIC 9(8)V99  VALUE ZEROS.
           05  QTDE-FOTOS-W    PIC 9(5)           VALUE ZEROS.
           05  PERC-W          PIC 9(5)V9999       VALUE ZEROS.
           05  VALOR-W         PIC 9(10)V99      VALUE ZEROS.
           05  VENMEDCLI-W     PIC 9(08)V99      VALUE ZEROS.
           05  VALOR-PM        PIC 9(14)V99      VALUE ZEROS.
           05  SALALB-W        PIC 9(05)         VALUE ZEROS.
           05  SALFOT-W        PIC 9(08)         VALUE ZEROS.
           05  PRAZO-W         PIC 9(05)V999     VALUE ZEROS.
           05  TOTAL-ALBPROD   PIC 9(06)         VALUE ZEROS.
           05  TOTAL-ALBVEND   PIC 9(06)         VALUE ZEROS.
           05  TOTAL-ALBSALDO  PIC 9(06)         VALUE ZEROS.
           05  TOTAL-FOGO      PIC 9(06)         VALUE ZEROS.
           05  TOTAL-FOTSALDO  PIC 9(08)         VALUE ZEROS.
           05  TOTAL-FITAW     PIC 9(08)         VALUE ZEROS.
           05  TOTAL-FITAW1    PIC 9(08)         VALUE ZEROS.
           05  TOTAL-BRUTO     PIC 9(14)V99      VALUE ZEROS.
           05  PRAZO-GERAL     PIC 9(14)V99      VALUE ZEROS.
           05  TOTAL-VENLIQDEF PIC 9(14)V99      VALUE ZEROS.
           05  TOTAL-VENLIQIND PIC 9(14)V99      VALUE ZEROS.
           05  TOTAL-FOTPROD   PIC 9(08)         VALUE ZEROS.
           05  TOTAL-DISPONIVEL PIC 9(08)        VALUE ZEROS.
           05  PRECOMED-W      PIC 9(08)V99      VALUE ZEROS.
           05  TOTAL-FOTVEND   PIC 9(08)         VALUE ZEROS.
           05  TOTAL-FOTDEVO   PIC 9(08)         VALUE ZEROS.
           05  TOTAL-FOTOS     PIC 9(8)          VALUE ZEROS.
           05  TOTAL-FOTOS1    PIC 9(8)          VALUE ZEROS.
           05  TOTAL-COMISSAO  PIC 9(6)          VALUE ZEROS.
           05  TOTAL-AVULSA    PIC 9(6)          VALUE ZEROS.
           05  TOTAL-FITAS     PIC 9(8)          VALUE ZEROS.
           05  TOTAL-OBJETIVO  PIC 9(14)V99      VALUE ZEROS.
           05  TOTAL-OBJETIVO1 PIC 9(14)V99      VALUE ZEROS.
           05  TOTAL-PATROCINIO PIC 9(14)V99     VALUE ZEROS.
           05  TOTAL-PATROCINIO1 PIC 9(14)V99     VALUE ZEROS.
           05  TOTAL-CUSTO      PIC 9(14)V99     VALUE ZEROS.
           05  TOTAL-FORM       PIC 9(8)         VALUE ZEROS.
           05  TOTAL-FORM1      PIC 9(8)         VALUE ZEROS.
           05  TOTAL-FOTPERD    PIC 9(8)         VALUE ZEROS.
           05  TIPO-W           PIC 9            VALUE ZEROS.
           05  NAO-REALIZADO-W  PIC 9            VALUE ZEROS.
           05  CAMPANHA1.
               10  ANO-CAMP   PIC 9999.
               10  MES-CAMP   PIC 99.
           05  CAMPANHA-1 REDEFINES CAMPANHA1 PIC 9(6).
           05  CAMPANHA2.
               10  ANO1-CAMP   PIC 9999.
               10  MES1-CAMP   PIC 99.
           05  CAMPANHA-2 REDEFINES CAMPANHA2 PIC 9(6).

           05  MESANOI.
               10  ANO-II     PIC 9999.
               10  MES-II     PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(06).
           05  DATAWI.
               10  MESANO-WI   PIC 999999.
               10  DIA-WI      PIC 99.
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  TOT-PATR1       PIC 9(12)V99      VALUE ZEROS.
           05  TOT-OBJETIVO-W  PIC 9(9)V99       VALUE ZEROS.
           05  OBJETIVO-W      PIC 9(9)V99        VALUE ZEROS.
           05  ITEM-W          PIC 99             VALUE ZEROS.
      *    05  PATROCINIO1-W   PIC 9(12)V99       VALUE ZEROS.
           05  DISPONIVEL-W    PIC 9(9)           VALUE ZEROS.
           05  TOTAL-PATR1     PIC 9(12)V99       VALUE ZEROS.
      *    05  TOTAL1-W        PIC 9(12)V99       VALUE ZEROS.
           05  VALOR-PARC1     PIC 9(14)V99        VALUE ZEROS.
           05  VALOR1-W        PIC 9(10)V99        VALUE ZEROS.
           05  VCTO-CONT1.
               10  DIA-VCTO    PIC 99.
               10  MES-VCTO    PIC 99.
               10  ANO-VCTO    PIC 9999.
           05  VCTO-CONT REDEFINES VCTO-CONT1 PIC 9(8).
           05  CONT-W          PIC 9(4)            VALUE ZEROS.
           05  CONTRATO-WS     PIC 9(4)            VALUE ZEROS.
           05  TAXA-W          PIC 99V99           VALUE ZEROS.
           05  TAXA-WI         PIC 99V99           VALUE ZEROS.
           05  TAXA-E          PIC ZZ,99.
           05  SOMA-W          PIC 99               VALUE ZEROS.
           05  ANO-T           PIC 99               VALUE ZEROS.
           05  PAT-FORM-W      PIC 9(8)V99          VALUE ZEROS.

           05  CONT-MES        PIC 9(4)            VALUE ZEROS.
           05  TAXA-ACUM       PIC 99V999999       VALUE ZEROS.
           05  DATADIA-INV     PIC 9(8)            VALUE ZEROS.
           05  MESES-W         PIC 9(3)            VALUE ZEROS.
           05  PRAZO-MEDIO     PIC 9(4)            VALUE ZEROS.
           05  VALOR-W1        PIC 9(10)V99        VALUE ZEROS.
           05  VALOR-PARC      PIC 9(14)V99        VALUE ZEROS.
           05  VLR-GERAL       PIC 9(10)V99        VALUE ZEROS.
           05  PRECO-W         PIC 9(08)V99        VALUE ZEROS.

           05  DATA-W1         PIC 9(8)            VALUE ZEROS.
           05  DATA-W2         PIC 9(8)            VALUE ZEROS.
           05  TECLA           PIC 99              VALUE ZEROS.
           05  AUX-ALBUM.
               10 AUX-CONTRATO PIC 9(04).
               10 AUX-SEQ      PIC 9(04).
           COPY "PARAMETR".


       01  LINDET.
         03  LINDETP1.
         04  LINDETP11.
           05  CONTRATO-REL    PIC 9(04)    VALUE ZEROS.
           05  FILLER          PIC X        VALUE "-".
           05  DESCRICAO-REL   PIC X(13)    VALUE SPACES.
           05  FILLER          PIC X        VALUE "-".
           05  CIDADE-REL      PIC X(09)    VALUE SPACES.
           05  FILLER          PIC X        VALUE SPACES.
           05  REALIZ-REL      PIC 99/9999  BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FORM-REL        PIC ZZ99      BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  PADRAO-REL      PIC XX       VALUE SPACES.
         04 LINDETP12.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTOS-REL       PIC ZZ999    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
      *    05  PREFITAS-REL    PIC Z99      BLANK WHEN ZEROS.
           05  PREFITAS-REL    PIC XXX      VALUE SPACES.
         04 LINDETP13.
           05  FILLER          PIC X        VALUE SPACES.
           05  PATROCINIO-REL  PIC ZZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  PATROC-FORM-REL PIC ZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
           05  FILLER          PIC X(5)     VALUE SPACES.
           05  PREV-VENDAS-REL PIC ZZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
           05  FILLER          PIC XX       VALUE SPACES.
      *   03  LINDETSEPARADOR.
      *    05  FILLER          PIC XX       VALUE "II".
          03  LINDETP2.
           05  MES-REL         PIC 99       BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE "/".
           05  ANO-REL         PIC 99.
           05  FILLER          PIC X        VALUE SPACES.
           05  ALBPROD-REL     PIC ZZ99      BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  ALBVEND-REL     PIC ZZ99     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  ALBSALD-REL     PIC ZZ99     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTPROD-REL     PIC ZZ999     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTCOMIS-REL    PIC ZZ99     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTFOGO-REL     PIC ZZ99     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTVEND-REL     PIC ZZ999    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTDEVO-REL     PIC ZZ999    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTSALDO-REL    PIC ZZ999    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  VENDAFITA-REL   PIC ZZ99     BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTVPERC-REL    PIC ZZ9,9    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTDPERC-REL    PIC ZZ9,9    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  FOTSPERC-REL    PIC ZZ9,9    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
      *    05  FATBRU-REL      PIC ZZZZ.ZZZ.ZZZ,99 BLANK WHEN ZEROS.
      *    05  FILLER          PIC X        VALUE SPACES.
           05  PM-REL          PIC ZZZ,99   BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  VENLIQIND-REL   PIC ZZZ.ZZZ,99 BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  PREMEDFOT-REL   PIC ZZZ,9    BLANK WHEN ZEROS.
           05  FILLER          PIC X        VALUE SPACES.
           05  VENMEDCLI-REL   PIC ZZ.ZZZ,99  BLANK WHEN ZEROS.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       LINKAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE DS-CONTROL-BLOCK

           MOVE 0 TO NUMERO-REG NUMERO-REG1.


           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "COD007"  TO  RES-TO.  MOVE RECFILE TO PATH-COD007.
           MOVE "COD040"  TO  RES-TO.  MOVE RECFILE TO PATH-COD040.
           MOVE "CAD010"  TO  RES-TO.  MOVE RECFILE TO PATH-CAD010.
           MOVE "COD050"  TO  RES-TO.  MOVE RECFILE TO PATH-COD050.
           MOVE "COD002"  TO  RES-TO.  MOVE RECFILE TO PATH-COD002.
           MOVE "COD005"  TO  RES-TO.  MOVE RECFILE TO PATH-COD005.
           MOVE "CGD001"  TO  RES-TO.  MOVE RECFILE TO PATH-CGD001.
           MOVE "RCD100"  TO  RES-TO.  MOVE RECFILE TO PATH-RCD100.
           MOVE "MTD001"  TO  RES-TO.  MOVE RECFILE TO PATH-MTD001.
           MOVE "MTD020"  TO  RES-TO.  MOVE RECFILE TO PATH-MTD020.
           CLOSE CONTROLE.
           OPEN INPUT COD007.
           OPEN INPUT MTD001, MTD020, RCD100, CAD010, COD005,
                      COD040, COD050, COD002, CGD001.
           IF ST-COD007 = "00" CLOSE COD007    DELETE FILE COD007
                               OPEN OUTPUT COD007
           ELSE CLOSE COD007   OPEN OUTPUT COD007.
           accept data1-g from date.
           move data1-g(1:2) to data-g(7:2) ANO.
           IF ANO > 90 MOVE "19" TO DATA-G(5: 2)
           ELSE MOVE "20" TO DATA-G(5: 2).
           move data1-g(3:2) to data-g(3:2).
           move data1-g(5:2) to data-g(1:2).
           move data-g to data-ger.
           accept data-g from time.
           move data-g(1:2) to hora-ger(1:2).
           move data-g(3:2) to min-ger(1:2).
           initialize reg-COD007.
           move 9999 to contrato-CO007 num-cid-CO007.
           move linha-gera to part1-CO007.
           write reg-COD007.

           IF ST-RCD100 NOT = "00"
              STRING "ERRO ABERTURA RCD100: "  ST-RCD100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CAD010 NOT = "00"
              STRING "ERRO ABERTURA CAD010: "  ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD001 NOT = "00"
              STRING "ERRO ABERTURA CGD001: "  ST-CGD001 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-COD050 NOT = "00"
              STRING "ERRO ABERTURA COD050: "  ST-COD050 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-COD005 NOT = "00"
              STRING "ERRO ABERTURA COD005: "  ST-COD005 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-COD002 NOT = "00"
              STRING "ERRO ABERTURA COD002: "  ST-COD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-COD040 NOT = "00"
              STRING "ERRO ABERTURA COD004: "  ST-COD040 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD001 NOT = "00"
              STRING "ERRO ABERTURA MTD001: "  ST-MTD001 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD020 NOT = "00"
              STRING "ERRO ABERTURA MTD020: "  ST-MTD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CORPO-PROGRAMA SECTION.
           PERFORM MONTA-ARQUIVO.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.

      ******************************************************************
       MONTA-ARQUIVO SECTION.
           MOVE 0 TO NUMERO-REG NUMERO-REG1.
           MOVE ZEROS TO TOTAL-FORM TOTAL-FOTOS TOTAL-FITAS
      *                  TOTAL1-W PATROCINIO1-W
                         SOMA-W TOTAL-PATROCINIO
                         TOTAL-OBJETIVO TOTAL-ALBPROD TOTAL-FOTPERD
                         TOTAL-ALBSALDO TOTAL-FOTPROD TOTAL-DISPONIVEL
                         TOTAL-ALBVEND TOTAL-FOGO.

           MOVE ZEROS TO TOTAL-FOTVEND TOTAL-FOTDEVO TOTAL-FOTSALDO
                         TOTAL-FITAW TOTAL-BRUTO PRAZO-GERAL
                         TOTAL-VENLIQDEF TOTAL-VENLIQIND TOTAL-FORMANDOS
                         TOTAL-AVULSA TOTAL-COMISSAO.

           MOVE ZEROS TO I, J, FLAG-GERAL.
           MOVE 4 to TAXA-W TAXA-E.
           COMPUTE TAXA-WI = TAXA-W / 30.

           MOVE  1 to MES-W1 MES-CAMp.
           MOVE  1990 to ANO-W1 ANO-CAMP.

           MOVE 12 to MES1-CAMP MES-W1.
           MOVE 1999 to ANO1-CAMP ANO-W1.

           PERFORM INICIO-MONTAGEM-GERAL.

           PERFORM LEITURA-MTD001-GERAL.

           PERFORM NAOREALIZADO THRU FIM-NAOREALIZADO.

       INICIO-MONTAGEM-GERAL SECTION.
           MOVE 1 TO TIPO-W.
           MOVE ZEROS TO CONTRATO-MT01.
           START MTD001 KEY IS NOT < CONTRATO-MT01 invalid key
                 MOVE "10" TO ST-MTD001.

       LEITURA-MTD001-GERAL SECTION.
           PERFORM UNTIL ST-MTD001 = "10"
            READ MTD001 NEXT RECORD AT END
                 MOVE "10" TO ST-MTD001
            NOT AT END
                 MOVE CONTRATO-MT01 TO CONTRATO-WK CONTRATO-WS CONTR-W
                                       NR-CONTRATO-CO40
                 READ COD040 INVALID KEY
                      CONTINUE
                 NOT INVALID KEY
                      IF MESANO-PREV-CO40 < CAMPANHA-1 OR
                         MESANO-PREV-CO40 > CAMPANHA-2
                         CONTINUE
                      ELSE
                         MOVE ZEROS TO DATA-WK ALBPROD-WK
                                       ALBVEND-WK FOTPROD-WK
                                       FOTVEND-WK FITA-WK FATVENBRU-WK
                                       FOTAVUL-WK PRAZO-WK VENLIQDEF-WK
                                       VENLIQIND-WK FORMANDOS-WK
                                       ALBPERD-WK FOTCOMIS-WK
                                       VENDAVUL-WK FOTMONT-WK
                                       FOTDEVO-WK FOTPERD-WK FOTFOGO-WK
                         MOVE PRODUZIDA-MT01 TO FOTPROD-WK
                         MOVE PERDIDA-MT01   TO FOTPERD-WK
                         MOVE MONTADA-MT01   TO FOTMONT-WK
                         MOVE AVULSA-MT01    TO FOTAVUL-WK
                         MOVE CLIEN-ALBUM-MT01 TO ALBPROD-WK
                         MOVE ZEROS TO ALB-W FOTFOGO-WK
                         MOVE EVENTO-W TO ALBUM-MTG
                         START MTD020 KEY IS NOT < ALBUM-MTG invalid key
                               MOVE "10" TO ST-MTD020
                         END-START
                         PERFORM LEITURA-MTD020-GERAL
                         PERFORM LEITURA-RCD100-GERAL
      *                  PERFORM CALC-OBJ-PATROCINIO

                         IF ALBVEND-WK = ZEROS
                            CONTINUE
                         ELSE
                            PERFORM MOVER-DADOS THRU FIM-DADOS
                         END-IF
                      END-IF
                 END-READ
            END-READ
           END-PERFORM.
       LEITURA-MTD020-GERAL SECTION.
           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
               NOT AT END
                IF FOGO-MTG NOT NUMERIC MOVE ZEROS TO FOGO-MTG
                END-IF
                MOVE ALBUM-MTG TO EVENTO-W
                IF CONTR-W NOT = CONTRATO-WS MOVE "10" TO ST-MTD020
                ELSE
                  IF FOGO-MTG = 9 ADD QT-ENCADER-MTG TO ALBPERD-WK
                           ADD QT-FOTOS-MTG TO FOTFOGO-WK
                  ELSE CONTINUE
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

       LEITURA-RCD100-GERAL SECTION.
           MOVE ZEROS TO ALB-W.
           MOVE CONTRATO-WS TO CONTR-W.
           MOVE EVENTO-W TO ALBUM-REC.
           START RCD100 KEY IS NOT < ALBUM-REC invalid key
                        MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
             READ RCD100 NEXT RECORD AT END MOVE "10" TO ST-RCD100
              NOT AT END
               MOVE ALBUM-REC TO EVENTO-W ALBUM-MTG
               IF CONTR-W NOT = CONTRATO-WS MOVE "10" TO ST-RCD100
               ELSE
                READ MTD020 INVALID KEY CONTINUE
                 NOT INVALID KEY
                   IF QT-FOTOS-MTG NOT NUMERIC
                      MOVE ZEROS TO QT-FOTOS-MTG
                   END-IF
                      COMPUTE FOTDEVO-W = QT-FOTOS-MTG -
                               (QFOTOS-REC + QABERTURA-REC
                               + QCOMISSAO-REC +QAVULSAS-REC)
                   ADD FOTDEVO-W TO FOTDEVO-WK
                END-READ
                PERFORM CONT-LEIT-RCD100-GERAL
               END-IF
             END-READ
           END-PERFORM.

       CONT-LEIT-RCD100-GERAL SECTION.
           MOVE ALBUM-REC TO AUX-ALBUM
           IF AUX-SEQ > ZEROS
              ADD 1             TO FORMANDOS-WK.

           ADD QCOMISSAO-REC TO FOTCOMIS-WK.
           ADD QAVULSAS-REC  TO VENDAVUL-WK.
           ADD QAVULSAS-REC  TO FOTVEND-WK
           ADD QFOTOS-REC    TO FOTVEND-WK.
           ADD QABERTURA-REC TO FOTVEND-WK
           ADD QENCADER-REC  TO ALBVEND-WK.
           ADD QFITAS-REC    TO FITA-WK.
           ADD TOTAL-REC     TO FATVENBRU-WK.
           ADD TOTAL-DEF-REC TO VENLIQDEF-WK VENLIQIND-WK abc-CO007.
           COMPUTE VALOR-PM = (TOTAL-REC * PM-REC).
           ADD VALOR-PM TO PRAZO-WK.
           IF DATA-WK = ZEROS
              MOVE DATAVEN-REC TO DATA-WK.

      ***********************************************************
      *   PARTE RESPONSAVEL PELA CRIACAO DOS DADOS PARA A CONSULTA

       MOVER-DADOS SECTION.
           MOVE  CONTRATO-WK          TO NR-CONTRATO-CO40, CONTRATO-REL
                                         contrato-CO007.
           READ  COD040 INVALID KEY  MOVE "*********"
                               TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO NOME-CURSO-CO007.

       MOVER-DADOS1 SECTION.
           MOVE ZEROS TO VALOR-PARC1 VALOR-PARC VALOR1-W
      *              TOTAL1-W PATROCINIO1-W
           MOVE IDENTIFICACAO-CO40    TO DESCRICAO-REL.
           MOVE MESANO-PREV-CO40 TO MESANO-1.
           subtract mesano-1 from 999999 giving mesano-CO007.
           MOVE ANO-1 TO ANO-W1.
           MOVE MES-1 TO MES-W1.
           MOVE MESANO-W TO REALIZ-REL.
           move representante-CO40 to CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01 TO REPRESENTANTE-CO007.
           MOVE QTDE-FORM-CO40 TO FORM-REL.
           PERFORM CALCULA-QTDE-FOTOS.
           IF FLAG-GERAL = ZEROS
              ADD QTDE-FORM-CO40 TO TOTAL-FORM
              ADD QTDE-FOTOS-W TO TOTAL-FOTOS
           ELSE ADD QTDE-FORM-CO40 TO TOTAL-FORM1
                ADD QTDE-FOTOS-W TO TOTAL-FOTOS1.
           MOVE PADRAO-CO40  TO PADRAO-REL.
           MOVE QTDE-FOTOS-W TO FOTOS-REL.
           IF COBERTURA-CO40 = 1 OR 2 OR 4 OR 6
              MOVE "S"   TO PREFITAS-REL
           ELSE MOVE "N" TO PREFITAS-REL.
           MOVE CIDADE-CO40 TO CIDADE num-cid-CO007.
           READ CAD010 INVALID KEY MOVE '******' TO NOME-CID.
           MOVE NOME-CID TO CIDADE-REL nome-cid-CO007.
           MOVE ZEROS TO CONT-W.

       CALC-OBJ-PATROCINIO SECTION.
           MOVE DATA-PREV-VENDA-CO40 TO VCTO-CONT DATA-W.
           MOVE DIA-W TO DIA-I.
           MOVE MES-W TO MES-I.
           MOVE ANO-W TO ANO-I ANO-T.
           MOVE DATA-I TO DATA-WI GRDIAS-AAMMDD-FINAL.
           MOVE ASSINATURA-CO40 TO GRDIAS-AAMMDD-INICIAL DATA-I
           CALL "GRDIAS1" USING PARAMETROS-GRDIAS.
           MOVE GRDIAS-NUM-DIAS TO CONT-W.
           MOVE VLR-COMISSAO-CO40 TO TOT-PATR1.
           COMPUTE MESES-W = CONT-W / 30.
           COMPUTE VLR-GERAL = MESES-W * VLR-COMISSAO-CO40.
           MOVE ZEROS TO SOMA-W ITEM-W.
           MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO50.
           MOVE 1 TO ITEM-CO50.
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.

      *MOVE-PATROCINIO
           PERFORM UNTIL ST-COD050 = "10"
             READ COD050 NEXT RECORD AT END MOVE "10" TO ST-COD050
              NOT AT END
               IF NR-CONTRATO-CO50 NOT = NR-CONTRATO-CO40
                  MOVE "10" TO ST-COD050
               ELSE
                  IF SUSP-PREV-DEF-CO50 <> 2
                     MOVE CODBRINDE-CO50 TO CODIGO-CO02
                     READ COD002 INVALID KEY MOVE ZEROS TO VALOR-CO02
                     END-READ
                     PERFORM CALCULA-CUSTO-BRINDE
                     ADD CUSTO-W TO TOT-PATR1

      *              ADD CUSTO-W         TO PATROCINIO1-W TOTAL1-W
                     COMPUTE MESES-W = DIAS-PRAZO-CO50 / 30
                     COMPUTE VLR-GERAL = (MESES-W * CUSTO-W) + VLR-GERAL
                  END-IF
               END-IF
             END-READ
           END-PERFORM.

       CALCULA-CUSTO SECTION.
           COMPUTE PRAZO-MEDIO ROUNDED = VLR-GERAL / TOT-PATR1.
           MOVE ZEROS TO CONT-MES.
           MOVE 1 TO TAXA-ACUM.

           PERFORM VARYING CONT-MES FROM 1 BY 1 UNTIL
                    CONT-MES > PRAZO-MEDIO
             COMPUTE TAXA-ACUM = TAXA-ACUM * (TAXA-W / 100 + 1)
           END-PERFORM.

           COMPUTE TOT-PATR1 = TAXA-ACUM * TOT-PATR1.
           COMPUTE PAT-FORM-W = TOT-PATR1 / QTDE-FORM-CO40.

           MOVE PAT-FORM-W TO PATROC-FORM-REL.
           MOVE TOT-PATR1 TO PATROCINIO-REL.
           PERFORM VERIFICA-PRECO.
           COMPUTE TOT-OBJETIVO-W ROUNDED =
                   QTDE-FOTOS-W * 0,7 * PRECO-W.
           MOVE TOT-OBJETIVO-W TO PREV-VENDAS-REL.
           IF FLAG-GERAL = ZEROS
              ADD TOT-OBJETIVO-W TO TOTAL-OBJETIVO
              ADD TOT-PATR1      TO TOTAL-PATROCINIO
           ELSE ADD TOT-OBJETIVO-W TO TOTAL-OBJETIVO1
                ADD TOT-PATR1      TO TOTAL-PATROCINIO1.
       FIM-CUSTO SECTION.


       CONT-IMPR SECTION.
           MOVE DATA-WK             TO DATA-I.
           MOVE MES-I               TO MES-REL.
           MOVE ANO-I(3: 2)         TO ANO-REL.
           MOVE ALBPROD-WK          TO ALBPROD-REL.
           MOVE FOTCOMIS-WK         TO FOTCOMIS-REL.
           MOVE FOTFOGO-WK          TO FOTFOGO-REL
           MOVE FORMANDOS-WK        TO ALBVEND-REL.
           COMPUTE SALALB-W = ALBPROD-WK - FORMANDOS-WK.
           MOVE SALALB-W            TO ALBSALD-REL.

           COMPUTE DISPONIVEL-W = FOTPROD-WK - FOTPERD-WK

      *    COMPUTE DISPONIVEL-W = (FOTMONT-WK + FOTAVUL-WK) -
      *                           (FOTCOMIS-WK + FOTFOGO-WK).
           MOVE DISPONIVEL-W        TO FOTPROD-REL.
           MOVE FOTVEND-WK          TO FOTVEND-REL.
           MOVE FOTDEVO-WK          TO FOTDEVO-REL.

           COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
                              FOTDEVO-WK + FOTCOMIS-WK +
      *                       VENDAVUL-WK
      *                       +
                              FOTFOGO-WK).

      *    COMPUTE SALFOT-W = FOTPROD-WK - (FOTVEND-WK + FOTPERD-WK +
      *                       FOTDEVO-WK + FOTCOMIS-WK + VENDAVUL-WK
      *                       + FOTFOGO-WK).
           MOVE SALFOT-W            TO FOTSALDO-REL
           MOVE FITA-WK             TO VENDAFITA-REL.

           COMPUTE DIVISAO-W = FOTVEND-WK / DISPONIVEL-W
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W              TO FOTVPERC-REL.
           COMPUTE DIVISAO-W = FOTDEVO-WK / DISPONIVEL-W
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W              TO FOTDPERC-REL.
           COMPUTE DIVISAO-W = SALFOT-W / DISPONIVEL-W
           COMPUTE PERC-W = DIVISAO-W * 100.
           MOVE PERC-W              TO FOTSPERC-REL.

      *    MOVE FATVENBRU-WK        TO FATBRU-REL.
           COMPUTE PRAZO-W = PRAZO-WK / FATVENBRU-WK.
           MOVE PRAZO-W             TO PM-REL.

           MOVE VENLIQIND-WK        TO VENLIQIND-REL.
           COMPUTE PRECOMED-W = VENLIQIND-WK / FOTVEND-WK.
           MOVE PRECOMED-W           TO PREMEDFOT-REL.
           COMPUTE VENMEDCLI-W = VENLIQIND-WK / FORMANDOS-WK.
           MOVE VENMEDCLI-W TO VENMEDCLI-REL.

           ADD ALBPROD-WK            TO TOTAL-ALBPROD.
           ADD FOTPERD-WK            TO TOTAL-FOTPERD.
           ADD FORMANDOS-WK          TO TOTAL-ALBVEND.
           ADD SALALB-W              TO TOTAL-ALBSALDO.
           ADD FOTPROD-WK            TO TOTAL-FOTPROD.
           ADD DISPONIVEL-W          TO TOTAL-DISPONIVEL.
           ADD FOTVEND-WK            TO TOTAL-FOTVEND.
           ADD FOTDEVO-WK            TO TOTAL-FOTDEVO.
           ADD SALFOT-W              TO TOTAL-FOTSALDO.
           ADD FITA-WK               TO TOTAL-FITAW TOTAL-FITAW1.
           ADD FATVENBRU-WK          TO TOTAL-BRUTO.
           ADD PRAZO-WK              TO PRAZO-GERAL.
           ADD VENLIQDEF-WK          TO TOTAL-VENLIQDEF.
           ADD VENLIQIND-WK          TO TOTAL-VENLIQIND.
           ADD FORMANDOS-WK          TO TOTAL-FORMANDOS.
           ADD FOTAVUL-WK            TO TOTAL-AVULSA.
           ADD FOTCOMIS-WK           TO TOTAL-COMISSAO.
           ADD FOTFOGO-WK            TO TOTAL-FOGO.
      *    DISPLAY (26 , 1) 'FORAM PROCESSADOS'
      *    DISPLAY (25 , 19) CONTrato-CO007 '  com ' patrocinio-rel
      *    DISPLAY (26 , 25) 'CONTRATOS'.
       FIM-DADOS SECTION.
           MOVE LINDETP11 TO part1-CO007.
           MOVE LINDETP12 TO PART1-CO007(46: 10)
           MOVE LINDETP13 (1 : 49) TO part1-CO007 (56 : 49).
           MOVE LINDETP2 TO part2-CO007.
           WRITE REG-COD007 INVALID KEY
             PERFORM ERRO-GRAVACAO.
       CALCULA-CUSTO-BRINDE SECTION.
      *  se o brinde já foi pago então o custo será conforme o
      *  valor-pago, senão verifica se o brinde é do tipo mult-por-form
      *  então multiplica qtde-form pela qtde-por-form e custo-previsto,
      *  onde o custo-previsto é o custo unitário do brinde caso ele
      *  tenha sido preenchido, senão o custo-previsto virá do cadastro
      *  do brinde
            IF CUSTO-UNIT-CO50 <> ZEROS
               MOVE CUSTO-UNIT-CO50 TO CUSTO-PREVISTO
            ELSE
               MOVE VALOR-CO02 TO CUSTO-PREVISTO
            END-IF
      * 0 NÃO - 1 SIM
            IF REALIZADO-CO50 <> 1
      * 1 SIM - 2 NÃO
               IF MULT-FORM-CO02 = 2
                  COMPUTE CUSTO-W = CUSTO-PREVISTO *
                                      QTDE-POR-FORM-CO50
               ELSE
                  COMPUTE CUSTO-W = (QTDE-POR-FORM-CO50
                      * QTDE-FORM-CO50) * CUSTO-PREVISTO
               END-IF
           ELSE
              MOVE VALOR-PAGO-CO50 TO CUSTO-W
           END-IF.

       CALCULA-QTDE-FOTOS SECTION.
           MOVE PADRAO-CO40 TO PADRAO-CO05.
           READ COD005 INVALID KEY MOVE ZEROS TO PREV-FOTOS-CO05.
           COMPUTE QTDE-FOTOS-W = PREV-FOTOS-CO05 * QTDE-FORM-CO40.

      *---------------------------------------------------------------
       NAOREALIZADO SECTION.
           MOVE ZEROS TO NR-CONTRATO-CO40 NAO-REALIZADO-W NUMERO-REG1.
           MOVE CAMPANHA-1 TO MESANO-PREV-CO40.
           MOVE 1 TO FLAG-GERAL.
           MOVE ZEROS TO TOTAL-FOTOS1 TOTAL-OBJETIVO1 TOTAL-PATROCINIO1
                         TOTAL-FORM1 TOTAL-FITAW1.

           START COD040  KEY IS NOT < ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
             READ COD040 NEXT RECORD AT END MOVE "10" TO ST-COD040
               NOT AT END
                IF MESANO-PREV-CO40 > CAMPANHA-2
                   MOVE "10" TO ST-COD040
                ELSE
                 MOVE NR-CONTRATO-CO40 TO ALBUM-MTG (01: 04)
                 MOVE ZEROS           TO ALBUM-MTG (05: 04)
                 START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                              PERFORM PROSSEGUE-COD040
                    NOT INVALID KEY
                     READ MTD020 NEXT RECORD AT END
                          PERFORM PROSSEGUE-COD040
                      NOT AT END
                       MOVE ALBUM-MTG TO EVENTO-W
                       IF NR-CONTRATO-CO40 NOT = CONTR-W
                              PERFORM PROSSEGUE-COD040
                       END-IF
                     END-READ
                 END-START
             END-READ
           END-PERFORM.

       PROSSEGUE-COD040.
           PERFORM MOVER-DADOS1 THRU FIM-CUSTO.
           MOVE NR-CONTRATO-CO40 TO CONTRATO-REL contrato-CO007.
           move IDENTIFICACAO-CO40 to nome-curso-CO007.
           MOVE ZEROS TO MES-REL ANO-REL ALBPROD-REL ALBVEND-REL
                         ALBSALD-REL FOTPROD-REL FOTVEND-REL FOTDEVO-REL
                         FOTSALDO-REL PREFITAS-REL FOTVPERC-REL
                         FOTDPERC-REL FOTCOMIS-REL
                         FOTSPERC-REL PM-REL VENLIQIND-REL
                         PREMEDFOT-REL VENMEDCLI-REL VENDAFITA-REL
      *                  FATBRU-REL
                         abc-CO007.
           MOVE 1 TO NAO-REALIZADO-W.
      *    DISPLAY (26 , 1) 'FORAM PROCESSADOS'
      *    DISPLAY (25 , 19) CONTrato-CO007'  com ' patrocinio-rel
      *    DISPLAY (26 , 25) 'CONTRATOS'.
           MOVE LINDETP11 TO part1-CO007.
           MOVE LINDETP13 (1 : 49) TO part1-CO007 (46 : 49).
           MOVE LINDETP2 TO part2-CO007.
           WRITE reg-COD007 INVALID KEY
              PERFORM ERRO-GRAVACAO.
       FIM-NAOREALIZADO SECTION.
      *------------------------------------------------------------

       VERIFICA-PRECO SECTION.
           EVALUATE MESANO-PREV-CO40
             WHEN < 199506 MOVE 8  TO PRECO-W
             WHEN NOT < 199506 MOVE 11 TO PRECO-W
           END-EVALUATE.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE SPACES TO MENSAGEM.

       FINALIZAR-PROGRAMA SECTION.
      *    MOVE "ACABEI" TO MENSAGEM
      *    MOVE "C" TO TIPO-MSG
      *    PERFORM EXIBIR-MENSAGEM
           CLOSE  RCD100, MTD001, MTD020, COD040, COD005,
                  COD050, CAD010, COD002, COD007, CGD001.

           EXIT PROGRAM
           STOP RUN.
