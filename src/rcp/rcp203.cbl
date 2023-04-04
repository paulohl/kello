       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP203.
      *DATA: 22/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELATORIO/RESUMO GERAL DE VENDAS
      *        POR CONTRATO OU POR INTERVALO DE DATA(MOVTO OU VENDA)
      *        QTDE DE FOTOS VENDEDIDAS = QFOTOS-REC + QABERTURA-REC
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY CAPX012.
           COPY CGPX001.
           COPY COPX001.
           COPY COPX040.
           COPY MTPX001.
           COPY MTPX020.
           COPY MTPX023.
           COPY RCPX100.
           COPY RCPX101.
           COPY RCPX203.
           COPY CEAPX010.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ALBUM-WK
                  ALTERNATE RECORD KEY IS VENDEDOR-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TOTVEN-WK WITH DUPLICATES.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS VENDEDOR-WK1.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CONTRATO-WK2.

           SELECT WORK3 ASSIGN TO VARIA-W3
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK3
                  RECORD KEY IS CHAVE-WK3.

           SELECT WORK5 ASSIGN TO VARIA-W5
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK5
                  RECORD KEY IS CHAVE-WK5.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
      *    "teste2.txt"
      *    organization is line sequential.
      *

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL
                        FILE STATUS IS FS-EXCEL.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY CGPW001.
       COPY COPW001.
       COPY COPW040.
       COPY MTPW001.
       COPY MTPW020.
       COPY MTPW023.
       COPY RCPW100.
       COPY RCPW101.
       COPY RCPW203.
       COPY CEAPW010.

       FD  WORK.
       01  REG-WORK.
           05  DTAMOV-WK             PIC 9(08).
           05  ALBUM-WK              PIC 9(8).
           05  TOTALB-WK             PIC 9.
           05  TOTFOT-WK             PIC 999.
           05  TOTFITA-WK            PIC 99.
           05  TOTDVD-WK             PIC 99.
           05  TOTFOL-WK             PIC 999.
           05  TOTPOSTER-WK          PIC 99.
           05  TOTPORTA-FITA-WK      PIC 99.
           05  TOTPORTA-DVD-WK       PIC 99.
           05  TOTFOTO-CD-WK         PIC 99.
           05  TOTMOLDURA-WK         PIC 99.
           05  TOTAVUL-WK            PIC 999.
           05  TOTCOMIS-WK           PIC 999.
           05  TOTVEN-WK             PIC 9(10)V99.
           05  PM-WK                 PIC 9(14)V99.
           05  TOTVENIDX-WK          PIC 9(09)V99.
           05  DATA-VENDA-WK         PIC 9(08).
           05  COD-VENDEDOR-WK       PIC 9(6).
           05  VENDEDOR-WK           PIC X(40).
           05  FITDEV-WK             PIC S9(3).
           05  DVDDEV-WK             PIC S9(3).
           05  FOTO-CD-DEV-WK        PIC S9(3).
           05  ALBDEV-WK             PIC S9(3).
           05  CODFOT-WK             PIC 9(05).
           05  CODALB-WK             PIC 9(05).
           05  DEVOL-WK              PIC S9(05).
           05  VALOR-COMISSAO-WK     PIC 9(09)V99.

       FD  WORK1.
       01  REG-WORK1.
           05  COD-VENDEDOR-WK1      PIC 9(06).
           05  VENDEDOR-WK1          PIC X(40).
           05  ENCADER2-WK1          PIC 99999.
           05  ENCADER-WK1           PIC 99999.
           05  FOTOS-WK1             PIC 9(06).
           05  DEVOL-WK1             PIC S9(05).
           05  DEVFIT-WK1            PIC S9(5).
           05  DEVDVD-WK1            PIC S9(5).
           05  DEVALB-WK1            PIC S9(5).
           05  DEVFOTO-CD-WK1        PIC S9(5).
           05  FITAS-WK1             PIC 99999.
           05  DVD-WK1               PIC 99999.
           05  POSTER-WK1            PIC 99999.
           05  FOTO-CD-WK1           PIC 99999.
           05  VENBRU-WK1            PIC 9(12)V99.
           05  PM-WK1                PIC 9(14)V99.
           05  VENDOL-WK1            PIC 9(08)V99.
           05  PFITA-WK1             PIC 9(10)V99.
           05  PALBUM-WK1            PIC 9(10)V99.
           05  ENCARD-RELAT-WK1      PIC 9(03).
           05  FOTOS-RELAT-WK1       PIC 9(05).
           05  FITAS-RELAT-WK1       PIC 9(05).
           05  DVD-RELAT-WK1         PIC 9(05).
           05  POSTER-RELAT-WK1      PIC 9(05).
           05  FOTO-CD-RELAT-WK1     PIC 9(05).

       FD  WORK2.
       01  REG-WORK2.
           05  CONTRATO-WK2          PIC 9(4).
           05  PRODUZIDA-WK2         PIC 9(9).
           05  MONTADA-WK2           PIC 9(9).
           05  AVULSA-WK2            PIC 9(9).
           05  PERDIDA-WK2           PIC 9(9).
           05  FORM-PROD-WK2         PIC 9(6).

       FD  WORK3.
       01  REG-WORK3.
           05  CHAVE-WK3.
               10 VENDEDOR-WK3       PIC X(40).
               10 CONTRATO-WK3       PIC 9(8).
               10 DATA-VISITA-WK3    PIC 9(8).
           05  ALBUM-WK3             PIC 9(03).
           05  FOTO-WK3              PIC 9(03).
           05  FITAS-WK3             PIC 9(03).
           05  DVD-WK3               PIC 9(03).
           05  POSTER-WK3            PIC 9(03).
           05  FOTO-CD-WK3           PIC 9(03).
           05  MOLDURA-WK3           PIC 9(03).

       FD  WORK5.
       01  REG-WORK5.
           05  CHAVE-WK5.
               10 VENDEDOR-WK5       PIC X(40).
           05  TOT-VENDA-BRUTA       PIC 9(09)V99.
           05  TOT-MOEDA             PIC 9(09)V99.
           05  TOT-CHEQUE            PIC 9(09)V99.
           05  TOT-ANTECIPADA        PIC 9(09)V99.
           05  TOT-DUPLICATA         PIC 9(09)V99.
           05  TOT-DEBITO            PIC 9(09)V99.
           05  TOT-CARTAO            PIC 9(09)V99.


       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-ALBUM           PIC X(08).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-AL              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FOT             PIC X(03).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FI              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FOL             PIC X(03).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-P               PIC X(01).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-PF              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-DVD             PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-PDV             PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FC              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-MD              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-VENDA-BRUTA     PIC X(08).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-PM              PIC X(06).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-DATA-VENDA      PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-VL-TOT          PIC X(06).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-VENDEDOR        PIC X(10).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FOT2            PIC X(03).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-PERC-FOT        PIC X(06).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FIT             PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-AL2             PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-DV              PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 EXCEL-FC2             PIC X(02).
          05 FILLER                PIC X(01) VALUE ";".
          05 FILLER                PIC X(02) VALUE X"0DA0".

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(150).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP203.CPB".
           COPY "RCP203.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-RCD203             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD023             PIC XX       VALUE SPACES.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ST-WORK3              PIC XX       VALUE SPACES.
           05  ST-WORK5              PIC XX       VALUE SPACES.
           05  FS-EXCEL              PIC XX       VALUE SPACES.
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
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W3              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W5              PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VECTO-INI-ANT         PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM-ANT         PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC ZZZ.ZZZ.ZZ9,99.
           05  VALOR-E1              PIC ZZZ.ZZ9,99.
           05  VALOR-E1A             PIC ZZ9,99.
           05  VALOR-E2A             PIC ZZZ9,99.
           05  VALOR-E2              PIC ZZ9,99.
           05  VALOR-E3              PIC ZZZ.ZZ9,99.
           05  VALOR-E31             PIC ZZ.ZZZ.ZZ9,99.
           05  VALOR-E32             PIC Z.ZZZ.ZZ9,99.
           05  VALOR-E4              PIC Z9,99.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  DESPESA-W             PIC 9(8)V99  VALUE ZEROS.
           05  masc-valor            PIC ZZZ.ZZZ.ZZ9,99.
           05  masc-valor2           PIC ZZZ.ZZZ.ZZ9,99.
           05  VALIDA-TOTAL          PIC 9(8)V99  VALUE ZEROS.

           05  SALALB-W        PIC 9(05)         VALUE ZEROS.
           05  PRAZO-W         PIC 9(05)V999     VALUE ZEROS.
           05  SALDO-W         PIC S9(05)        VALUE ZEROS.
           05  VALOR-FOTOW     PIC 9(8)V99       VALUE ZEROS.
           05  VALOR-FITAW     PIC 9(8)V99       VALUE ZEROS.
           05  VALOR-DVDW      PIC 9(8)V99       VALUE ZEROS.
           05  VALOR-ALBUMW    PIC 9(8)V99       VALUE ZEROS.
           05  SOMA-AUX        PIC 9(07)         VALUE ZEROS.

           05  VALORU-W         PIC 9(10)V99      VALUE ZEROS.
           05  COMISSAO-W       PIC 9(10)V99      VALUE ZEROS.
           05  TOTALB-W         PIC 9(05)         VALUE ZEROS.
           05  TOTFOT-W         PIC 9(05)         VALUE ZEROS.
           05  TOTFITA-W        PIC 9(05)         VALUE ZEROS.
           05  TOTDVD-W         PIC 9(05)         VALUE ZEROS.
           05  TOTFOL-W         PIC 9(05)         VALUE ZEROS.
           05  TOTVEN-W         PIC 9(15)V99      VALUE ZEROS.
           05  TOTPORTA-W       PIC 9(05)         VALUE ZEROS.
           05  TOTPOSTER-W      PIC 9(5)          VALUE ZEROS.
           05  PERC-W           PIC 9(3)V99       VALUE ZEROS.
           05  PERC-E           PIC ZZ9,9 blank when zeros.
           05  PRAZO-W1         PIC 9(15)V99      VALUE ZEROS.
           05  PRAZO-W2         PIC 9(15)V99      VALUE ZEROS.
           05  CONTRATO-W       PIC 9(4)          VALUE ZEROS.
           05  VALOR-FOTO-W     PIC 9(3)V99       VALUE ZEROS.
           05  TOTAL-VENDA      PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-MOEDA      PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-CHEQUE     PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-ANTECIPADA PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-DUPLICATA  PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-DEBITO     PIC 9(09)V99     VALUE ZEROS.
           05  TOTAL-CARTAO     PIC 9(09)V99     VALUE ZEROS.
      *    05  nome-impressora  pic x(100) value spaces.

           05  WS-ALBUM.
               10  WS-ALB1     PIC 9(04)         VALUE ZEROS.
               10  WS-ALB2     PIC 9(04)         VALUE ZEROS.

           05  QTDE-E9               PIC ZZ9-.
           05  QTDE-E44              PIC ZZZ9.
           05  QTDE-E                PIC ZZZZ9.
           05  QTDE-E7               PIC ZZ9.
           05  QTDE-E1               PIC Z9.
           05  QTDE-E2               PIC 9.
           05  QTDE-E3               PIC ZZZZ.ZZ9.
           05  QTDE-E4               PIC ZZZZ9.
           05  QTDE-E5               PIC ZZZZZZ9.
           05  QTDE-E6               PIC ZZZZZ9.
           05  QTDE-E61              PIC ZZZZ9.
           05  TOT-PARTICIPANTE      PIC 9(6)     VALUE ZEROS.
      *    CALCULO PM E JUROS
           05  CONT                  PIC 9(4)     VALUE ZEROS.
           05  TAXA-ACUMULADA        PIC 9(3)V9(8) VALUE ZEROS.
           05  PRAZO-MEDIO           PIC 9(4)     VALUE ZEROS.
           05  PM-E                  PIC ZZ9,99.

           05  TOT1-ALBUM         PIC 9(6)     VALUE ZEROS.
           05  TOT1-FOTO          PIC 9(9)     VALUE ZEROS.
           05  TOT1-FITA          PIC 9(6)     VALUE ZEROS.
           05  TOT1-DVD           PIC 9(6)     VALUE ZEROS.
           05  TOT1-PORTA-DVD     PIC 9(6)     VALUE ZEROS.
           05  TOT1-FOTO-CD       PIC 9(6)     VALUE ZEROS.
           05  TOT1-MOLDURA       PIC 9(6)     VALUE ZEROS.
           05  TOT1-FOLHA         PIC 9(9)     VALUE ZEROS.
           05  TOT1-POSTER        PIC 9(6)     VALUE ZEROS.
           05  TOT1-PFITA         PIC 9(6)     VALUE ZEROS.
           05  TOT1-VENDA-BR      PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VENDA-IDX     PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VLR-UNIT-FOTO PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VLR-FOTO      PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VLR-ALBUM     PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VLR-FITA      PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-VLR-DVD       PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-COMISSAO      PIC 9(9)V99  VALUE ZEROS.
           05  TOT1-DEV-FOTO      PIC 9(6)     VALUE ZEROS.
           05  TOT1-DEV-FITA      PIC 9(6)     VALUE ZEROS.
           05  TOT1-DEV-DVD       PIC 9(6)     VALUE ZEROS.
           05  TOT1-DEV-ALBUM     PIC 9(6)     VALUE ZEROS.

           05  TOT2-FORM-PROD     PIC 9(6)     VALUE ZEROS.
           05  TOT2-FORM-VEND     PIC 9(6)     VALUE ZEROS.
           05  TOT2-FORM-SALD     PIC 9(6)     VALUE ZEROS.
           05  TOT2-FOTO-DISP     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOTO-COMI     PIC 9(6)     VALUE ZEROS.
           05  TOT2-FOTO-VEND     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOTO-FOGO     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOTO-DEVL     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOTO-SALD     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FITA-VEND     PIC 9(9)     VALUE ZEROS.
           05  TOT2-DVD-VEND      PIC 9(9)     VALUE ZEROS.
           05  TOT2-PERC-VEND     PIC 9(3)V9   VALUE ZEROS.
           05  TOT2-PERC-COMIS    PIC 9(3)V9   VALUE ZEROS.
           05  TOT2-PERC-FOGO     PIC 9(3)V9   VALUE ZEROS.
           05  TOT2-PERC-DEVL     PIC 9(3)V9   VALUE ZEROS.
           05  TOT2-PERC-SALD     PIC 9(3)V9   VALUE ZEROS.
           05  TOT2-VENDA-BR      PIC 9(8)V99  VALUE ZEROS.
           05  TOT2-VENDA-LIQ     PIC 9(8)V99  VALUE ZEROS.
           05  TOT2-PRECO-FOT     PIC 9(3)V99  VALUE ZEROS.
           05  TOT2-VENDA-CLI     PIC 9(4)V99  VALUE ZEROS.
           05  TOT2-AVULSA        PIC 9(9)     VALUE ZEROS.
           05  TOT2-MONTADA       PIC 9(9)     VALUE ZEROS.
           05  TOT2-PRODUZIDA     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOTOS-MTG     PIC 9(9)     VALUE ZEROS.
           05  TOT2-FOGO          PIC 9(9)     VALUE ZEROS.
           05  TOT2-PERDIDA       PIC 9(9)     VALUE ZEROS.
           05  TOT2-PM-ACUM       PIC 9(11)V99 VALUE ZEROS.
           05  TOT2-AVULSA-VEND   PIC 9(9)     VALUE ZEROS.
           05  LIN                PIC 9(02)    VALUE ZEROS.
           05  PASSAR-STRING-1    PIC X(65).
           05  contador           pic 9(10).
           05  quantidade         pic 9(02)    value zeros.

           05  TOT-ENCADER2       PIC 9(09)    VALUE ZEROS.
           05  TOT-FOTOS          PIC 9(09)    VALUE ZEROS.
           05  TOT-FITAS          PIC 9(09)    VALUE ZEROS.
           05  TOT-DVD            PIC 9(09)    VALUE ZEROS.
           05  TOT-POSTER         PIC 9(09)    VALUE ZEROS.
           05  TOT-FOTO-CD        PIC 9(09)    VALUE ZEROS.
           05  TOT-ENCARD-RELAT   PIC 9(09)    VALUE ZEROS.
           05  TOT-FOTOS-RELAT    PIC 9(09)    VALUE ZEROS.
           05  TOT-FITAS-RELAT    PIC 9(09)    VALUE ZEROS.
           05  TOT-DVD-RELAT      PIC 9(09)    VALUE ZEROS.
           05  TOT-POSTER-RELAT   PIC 9(09)    VALUE ZEROS.
           05  TOT-FOTO-CD-RELAT  PIC 9(09)    VALUE ZEROS.

           05  ACHEI              PIC X(01)    VALUE SPACES.

           05  MENSAGEM           PIC X(200).
           05  TIPO-MSG           PIC X(01).
           05  RESP-MSG           PIC X(01).
           05  MESANO-INI         PIC 9(06) VALUE ZEROS.
           05  MESANO-FIM         PIC 9(06) VALUE ZEROS.

       01 AUX-ALBUM.
          05 AUX-CONTRATO          PIC 9(04).
          05 AUX-CLIENTE           PIC 9(04).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELATORIO GERAL DE VENDAS   ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  DESC-ORDEM-REL      PIC X(78)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(41)   VALUE
           "RESUMO DE VENDAS            ORDEM: ".
           05  ORDEM1-REL          PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  DESC1-ORDEM-REL     PIC X(78)   VALUE SPACES.
       01  CAB02B.
           05  FILLER              PIC X(41)   VALUE
           "ESTATISTICA DE VENDAS       ORDEM: ".
           05  ORDEM2-REL          PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  DESC2-ORDEM-REL     PIC X(78)   VALUE SPACES.
       01  CAB02C.
           05  FILLER              PIC X(41)   VALUE
           "ALBUNS DE RELATORIOS        ORDEM: ".
           05  ORDEM3-REL          PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  DESC3-ORDEM-REL     PIC X(78)   VALUE SPACES.

       01  CAB03.
           05  FILLER              PIC X(131)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "         ------------------- VENDAS ---------------
      -    "     CUSTO-                     -- DEVOLUCAO ---".
       01  CAB05.
           05  FILLER              PIC X(110)  VALUE
           "ALBUM    AL  FOT FI FOL P PF DVD PDV FC MD VENDA-BR    P.M.
      -    "DT-VENDA VL-FOT VENDEDOR   FOT  %-FOT FI AL DV FC".

       01  CAB06.
           05  FILLER              PIC X(127)  VALUE
           "VENDEDOR   ALBUM AL-DV FOTOS FT-DV  FITA FI-DV DVD DVD-DEV F
      -    "OTO-CD FOTO-CD-DEV   VENDA-BRUTA   P.M.  TOTAL-VENDA   R$-UN
      -    "  %-VEN".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.
       01  LINDET1.
           05  LINDET1-REL         PIC X(130)  VALUE SPACES.
       01  CAB07.
           05  FILLER              PIC X(110)  VALUE
           "------------------------------ VENDAS ----------------------
      -    "----                         ---- DEVOLUCAO -----".
       01  CAB08.
           05  FILLER              PIC X(110)  VALUE
           "ALB   FOTOS FITA FOLHAS POS PFT   VENDA-BRUTA  P.M. VENDA-IN
      -    "DEXAD VLR-FOTO VLR-COMISSAO  FOTOS  %-DEV FITA ALB".


       01 CAB009.
          05  FILLER              PIC X(131) VALUE
                "           ------------- VENDIDOS -------------- ------
      -    "------ RELATORIOS -------------------- --------- TOTAL -----
      -    "---------  --%--".

       01 CAB010.
          05   FILLER             PIC X(131) VALUE
                "VENDEDOR   ALBUM  FOTOS FITAS DVD POSTER FOTO-CD ALBUM
      -    " FOTOS FITAS DVD POSTER FOTO-CD ALBUM  FOTOS FITAS DVD POSTE
      -    "R FOTO-CD  ALBUM".

       01 CAB011.
           05 FILLER               PIC X(91) VALUE
            "VENDEDOR   CONTRATO    DATA VISTA    ALBUM    FOTOS   FITAS
      -     "   DVD   POSTER  FOTO-CD MOLDURA".

       01  LINTOT.
           05  LINTOT-REL          PIC X(151)  VALUE SPACES.
       01  LINTOT1.
           05  LINTOT1-REL         PIC X(151)  VALUE SPACES.


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD023"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD023.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "RCD203"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD203.

           OPEN I-O   RCD203 MTD020
           CLOSE      RCD203 MTD020
           OPEN INPUT RCD203.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.
           ACCEPT VARIA-W1 FROM TIME.
           ADD 8525 TO VARIA-W1
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1.
           ACCEPT VARIA-W2 FROM TIME.
           ADD 9555 TO VARIA-W2
           OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2.
           ACCEPT VARIA-W3 FROM TIME.
           ADD 10580 TO VARIA-W3
           OPEN OUTPUT WORK3  CLOSE WORK3  OPEN I-O WORK3.
           ACCEPT VARIA-W5 FROM TIME.
           ADD 26150 TO VARIA-W5
           OPEN OUTPUT WORK5  CLOSE WORK5  OPEN I-O WORK5.

           OPEN I-O CEAD010
           CLOSE    CEAD010


           OPEN INPUT CEAD010 CGD001 COD040 RCD100 MTD001 MTD020
                      MTD023 RCD101  CAD010 CAD012 COD001.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CEAD010 <> "00"
              MOVE "ERRO ABERTURA CEAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CEAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD203 <> "00"
              MOVE "ERRO ABERTURA RCD203: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD023 <> "00"
              MOVE "ERRO ABERTURA MTD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTA1
                    PERFORM CARREGA-LISTA5
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTA1
                    PERFORM CARREGA-LISTA5
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMAR-POPUP-CONTRATO
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-CONTRATO-TRUE
                    PERFORM LE-CONTRATO
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-EXCEL-TRUE
                    PERFORM GERAR-EXCEL
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    RCD203
           OPEN I-O RCD203

           INITIALIZE REG-RCD203
           START RCD203 KEY IS NOT LESS CODIGO-RCD203 INVALID KEY
                MOVE "10" TO ST-RCD203.
           PERFORM UNTIL ST-RCD203 = "10"
                READ RCD203 NEXT AT END
                     MOVE "10" TO ST-RCD203
                NOT AT END
                     DELETE RCD203 INVALID KEY
                         MOVE "Erro de Exclusão...RCD203" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-RCD203
               WRITE REG-RCD203
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      RCD203
           OPEN INPUT RCD203.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-RCD203
           START RCD203 KEY IS NOT LESS CODIGO-RCD203 INVALID KEY
               MOVE "10" TO ST-RCD203.

           PERFORM UNTIL ST-RCD203 = "10"
               READ RCD203 NEXT AT END
                    MOVE "10" TO ST-RCD203
               NOT AT END
                    MOVE CODIGO-RCD203 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM


           IF ACHEI = "N"
              CLOSE      RCD203
              OPEN I-O   RCD203
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-RCD203
                        WRITE REG-RCD203

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      RCD203
              OPEN INPUT RCD203.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       LE-REGIAO SECTION.
           MOVE GS-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       GERAR-EXCEL SECTION.
           MOVE ZEROS TO TOT1-ALBUM TOT1-FOTO TOT1-FITA TOT1-FOLHA
                         TOT1-PFITA TOT1-VENDA-BR TOT1-VENDA-IDX
                         TOT1-VLR-UNIT-FOTO TOT1-DEV-FOTO TOT1-DEV-FITA
                         TOT1-DEV-ALBUM TOT1-VLR-FOTO TOT1-VLR-ALBUM
                         TOT1-VLR-FITA TOT1-COMISSAO TOT1-DVD
                         TOT1-PORTA-DVD TOT1-FOTO-CD TOT1-MOLDURA
                         TOT1-DEV-FITA TOT1-VLR-DVD TOT1-POSTER

           MOVE SPACES TO ARQUIVO-EXCEL

           STRING "\ARQUIVOS\RCP203-" GS-CONTRATO INTO ARQUIVO-EXCEL.

           OPEN OUTPUT EXCEL

           MOVE "ALBUM"         TO EXCEL-ALBUM
           MOVE "AL"            TO EXCEL-AL
           MOVE "FOT"           TO EXCEL-FOT
           MOVE "FI"            TO EXCEL-FI
           MOVE "FOL"           TO EXCEL-FOL
           MOVE "P"             TO EXCEL-P
           MOVE "PF"            TO EXCEL-PF
           MOVE "DVD"           TO EXCEL-DVD
           MOVE "PDV"           TO EXCEL-PDV
           MOVE "FC"            TO EXCEL-FC
           MOVE "MD"            TO EXCEL-MD
           MOVE "VENDA-BR"      TO EXCEL-VENDA-BRUTA
           MOVE "P.M."          TO EXCEL-PM
           MOVE "DATA-VENDA"    TO EXCEL-DATA-VENDA
           MOVE "VL-FOT"        TO EXCEL-VL-TOT
           MOVE "VENDEDOR"      TO EXCEL-VENDEDOR
           MOVE "FOT"           TO EXCEL-FOT2
           MOVE "%-FOT"         TO EXCEL-PERC-FOT
           MOVE "FI"            TO EXCEL-FIT
           MOVE "AL"            TO EXCEL-AL2
           MOVE "DV"            TO EXCEL-DV
           MOVE "FC"            TO EXCEL-FC2
           WRITE REG-EXCEL

           PERFORM ORDEM.

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET-EXCEL
                   WRITE REG-EXCEL
              END-READ
           END-PERFORM.

      *    PERFORM TOTALIZA-EXCEL
      *    WRITE REG-EXCEL

           CLOSE EXCEL.

       MOVER-DADOS-LINDET-EXCEL SECTION.
           INITIALIZE REG-EXCEL
           MOVE ALBUM-WK          TO EXCEL-ALBUM WS-ALBUM
           MOVE VENDEDOR-WK       TO VENDEDOR-WK1
           READ WORK1 INVALID KEY
                INITIALIZE REG-WORK1.
           MOVE TOTALB-WK         TO QTDE-E1
           IF WS-ALB2 NOT EQUAL ZEROS
              ADD 1               TO TOT1-ALBUM
           END-IF

           MOVE QTDE-E1           TO EXCEL-AL
           MOVE TOTFOT-WK         TO QTDE-E
           ADD TOTFOT-WK          TO TOT1-FOTO
           MOVE QTDE-E            TO EXCEL-FOT
           MOVE TOTFITA-WK        TO QTDE-E1
           ADD TOTFITA-WK         TO TOT1-FITA
           MOVE QTDE-E1           TO EXCEL-FI
           MOVE TOTFOL-WK         TO QTDE-E7
           ADD TOTFOL-WK          TO TOT1-FOLHA
           MOVE QTDE-E7           TO EXCEL-FOL
           MOVE TOTPOSTER-WK      TO QTDE-E2
           ADD TOTPOSTER-WK       TO TOT1-POSTER
           MOVE QTDE-E2           TO EXCEL-P
           MOVE TOTPORTA-FITA-WK  TO QTDE-E1
           ADD TOTPORTA-FITA-WK   TO TOT1-PFITA
           MOVE QTDE-E1           TO EXCEL-PF
           MOVE TOTDVD-WK         TO QTDE-E1
           ADD TOTDVD-WK          TO TOT1-DVD
           MOVE QTDE-E1           TO EXCEL-DVD
           MOVE TOTPORTA-DVD-WK   TO QTDE-E1
           ADD TOTPORTA-DVD-WK    TO TOT1-PORTA-DVD
           MOVE QTDE-E1           TO EXCEL-PDV
           MOVE TOTFOTO-CD-WK     TO QTDE-E1
           ADD TOTFOTO-CD-WK      TO TOT1-FOTO-CD
           MOVE QTDE-E1           TO EXCEL-FC
           MOVE TOTMOLDURA-WK     TO QTDE-E1
           ADD TOTMOLDURA-WK      TO TOT1-MOLDURA
           MOVE QTDE-E1           TO EXCEL-MD

           MOVE TOTVEN-WK         TO VALOR-E1
           ADD TOTVEN-WK          TO TOT1-VENDA-BR
           MOVE VALOR-E1          TO EXCEL-VENDA-BRUTA
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO EXCEL-PM
      *    MOVE TOTVENIDX-WK      TO VALOR-E1
           ADD TOTVENIDX-WK      TO TOT1-VENDA-IDX
      *    MOVE VALOR-E1          TO GS-LINDET(45: 9)
           MOVE CODALB-WK TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-ALBUMW = TOTALB-WK * TABELA-PROD.
           MOVE VALOR-ALBUMW        TO VALOR-E2
           ADD VALOR-ALBUMW         TO TOT1-VLR-ALBUM
      *    MOVE VALOR-E2          TO GS-LINDET(54: 7)
           MOVE 1000 TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-FITAW = TOTFITA-WK * TABELA-PROD.
           MOVE VALOR-FITAW       TO VALOR-E2
           ADD VALOR-FITAW        TO TOT1-VLR-FITA
      *    MOVE VALOR-E2          TO GS-LINDET(61: 7)

           MOVE DATA-VENDA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO EXCEL-DATA-VENDA

           COMPUTE VALOR-FOTOW = (TOTVENIDX-WK - VALOR-ALBUMW -
                               VALOR-FITAW) / TOTFOT-WK.
           MOVE VALOR-FOTOW       TO VALOR-E2
           ADD VALOR-FOTOW        TO TOT1-VLR-FOTO
           MOVE VALOR-E2          TO EXCEL-VL-TOT
           MOVE VENDEDOR-WK       TO EXCEL-VENDEDOR
           MOVE ALBUM-WK          TO ALBUM-MTG
           READ MTD020 INVALID KEY
                MOVE ZEROS TO VISITA-MTG.


           MOVE VALOR-COMISSAO-WK TO COMISSAO-W

      *    MOVE COMISSAO-W        TO VALOR-E1A
           ADD COMISSAO-W         TO TOT1-COMISSAO
      *    MOVE VALOR-E1A         TO GS-LINDET(85: 7)
           MOVE DEVOL-WK          TO QTDE-E9
           ADD DEVOL-WK           TO TOT1-DEV-FOTO
           MOVE QTDE-E9           TO EXCEL-FOT2
           COMPUTE SOMA-AUX = DEVOL-WK + TOTFOT-WK.
           COMPUTE PERC-W = (DEVOL-WK / (DEVOL-WK + TOTFOT-WK)) * 100
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO EXCEL-PERC-FOT
           MOVE FITDEV-WK         TO QTDE-E1
           ADD FITDEV-WK          TO TOT1-DEV-FITA
           MOVE QTDE-E1           TO EXCEL-FIT
           MOVE ALBDEV-WK         TO QTDE-E1
           ADD ALBDEV-WK          TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO EXCEL-AL2
           MOVE DVDDEV-WK         TO QTDE-E1
           ADD DVDDEV-WK          TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO EXCEL-DV
           MOVE FOTO-CD-DEV-WK    TO QTDE-E1
           ADD FOTO-CD-DEV-WK     TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO EXCEL-FC2.


       TOTALIZA-EXCEL SECTION.
           INITIALIZE REG-EXCEL
           MOVE SPACES               TO GS-LINTOT.
           MOVE TOT1-ALBUM           TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINTOT(1: 5)
           MOVE TOT1-FOTO            TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINTOT(6: 7)
           MOVE TOT1-FITA            TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINTOT(13: 5)
           MOVE TOT1-FOLHA           TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINTOT(18: 7)
           MOVE TOT1-POSTER          TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(25: 4)
           MOVE TOT1-PFITA           TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(29: 4)
           MOVE TOT1-VENDA-BR        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(37: 14)
           COMPUTE PRAZO-W = PRAZO-W1 / TOT1-VENDA-BR.
           MOVE PRAZO-W              TO PM-E
           MOVE PM-E                 TO GS-LINTOT(51: 6)
           MOVE TOT1-VENDA-IDX       TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(57: 14)
           COMPUTE TOT1-VLR-UNIT-FOTO = (TOT1-VENDA-IDX - TOT1-VLR-FITA
                              - TOT1-VLR-ALBUM) / TOT1-FOTO.

           MOVE TOT1-VLR-UNIT-FOTO   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINTOT(71: 9)
           MOVE TOT1-COMISSAO        TO VALOR-E
           MOVE VALOR-E              TO GS-LINTOT(80: 14)
           MOVE TOT1-DEV-FOTO        TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINTOT(94: 5).
           COMPUTE PERC-W = (TOT1-DEV-FOTO / (TOT1-DEV-FOTO +
                                  TOT1-FOTO)) * 100
           MOVE PERC-W               TO PERC-E
           MOVE PERC-E               TO GS-LINTOT(100: 7)
           MOVE TOT1-DEV-FITA        TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(107: 4).
           MOVE TOT1-DEV-ALBUM       TO QTDE-E
           MOVE QTDE-E               TO GS-LINTOT(111: 3).

           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.






       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "****" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO.
       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR  TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "****" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-VENDEDOR.

       CHAMAR-POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.
       CHAMAR-POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR.

      *----------------------------------------------------------
       INVERTE-DATA SECTION.
           MOVE GS-VECTO-INI TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV    TO VECTO-FIM.

       INVERTE-MESANO SECTION.
           STRING GS-MESANO-INI(3:4) GS-MESANO-INI(1:2) INTO MESANO-INI
           STRING GS-MESANO-FIM(3:4) GS-MESANO-FIM(1:2) INTO MESANO-FIM.


       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK   OPEN I-O WORK.
           CLOSE WORK1 OPEN OUTPUT WORK1 CLOSE WORK1  OPEN I-O WORK1.
           CLOSE WORK2 OPEN OUTPUT WORK2 CLOSE WORK2  OPEN I-O WORK2.
           CLOSE WORK3 OPEN OUTPUT WORK3 CLOSE WORK3  OPEN I-O WORK3.
           CLOSE WORK5 OPEN OUTPUT WORK5 CLOSE WORK5  OPEN I-O WORK5.

           MOVE ZEROS TO PRAZO-W1 PRAZO-W2.

           MOVE ZEROS TO TOT2-FORM-PROD TOT2-FORM-VEND TOT2-FORM-SALD
                         TOT2-FOTO-DISP TOT2-FOTO-COMI TOT2-FOTO-VEND
                         TOT2-FOTO-SALD TOT2-FITA-VEND TOT2-PERC-VEND
                         TOT2-DVD-VEND  TOT2-FOTO-FOGO TOT2-PERC-COMIS
                         TOT2-PERC-FOGO
                         TOT2-PERC-DEVL TOT2-PERC-SALD TOT2-VENDA-BR
                         TOT2-VENDA-LIQ TOT2-PRECO-FOT TOT2-VENDA-CLI
                         TOT2-MONTADA TOT2-AVULSA  TOT2-PERDIDA
                         TOT2-PRODUZIDA TOT2-FOTOS-MTG TOT2-PM-ACUM
                         TOT2-AVULSA-VEND.

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           INITIALIZE REG-RCD100

           EVALUATE GS-TIPO-REL
             WHEN 1 PERFORM INVERTE-DATA
                    MOVE VECTO-INI      TO DATA-MOVTO-REC
                    MOVE ZEROS          TO ALBUM-REC CONTADOR
                    START RCD100 KEY IS NOT < ALT-REC INVALID KEY
                          MOVE "10" TO ST-RCD100
                    END-START
                    PERFORM UNTIL ST-RCD100 = "10"
                       READ RCD100 NEXT AT END
                           MOVE "10" TO ST-RCD100
                       NOT AT END
                           ADD 1 TO CONTADOR
                           MOVE CONTADOR TO GS-EXIBE-MOVTO2
                           MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           IF DATA-MOVTO-REC > VECTO-FIM
                              MOVE "10" TO ST-RCD100
                           ELSE
                              IF GS-VENDEDOR = ZEROS OR GS-VENDEDOR =
                                 VENDEDOR-REC
                                 IF GS-CONTRATO = ZEROS OR GS-CONTRATO =
                                    ALBUM-REC(1:4)

                               MOVE ALBUM-REC(1:4) TO NR-CONTRATO-CO40
                               READ COD040 INVALID KEY
                                   INITIALIZE REG-COD040
                               END-READ

                                    PERFORM MOVER-DADOS-WORK
      *                             MOVE ALBUM-REC(1:4) TO GS-CONTRATO
      *                             PERFORM VER-FOGO
                                 END-IF
                              END-IF
                           END-IF
                       END-READ
                    END-PERFORM
             WHEN 2 PERFORM INVERTE-DATA
                    MOVE VECTO-INI     TO DATAVEN-REC
                    START RCD100 KEY IS NOT < DATAVEN-REC INVALID KEY
                          MOVE "10" TO ST-RCD100
                    END-START
                    PERFORM UNTIL ST-RCD100 = "10"
                       READ RCD100 NEXT AT END
                           MOVE "10" TO ST-RCD100
                       NOT AT END
                           ADD 1 TO CONTADOR
                           MOVE CONTADOR TO GS-EXIBE-MOVTO2
                           MOVE DATAVEN-REC     TO GS-EXIBE-MOVTO
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           IF DATAVEN-REC > VECTO-FIM
                              MOVE "10" TO ST-RCD100
                           ELSE
                              IF GS-VENDEDOR = ZEROS OR GS-VENDEDOR =
                                 VENDEDOR-REC
                                 IF GS-CONTRATO = 0 OR GS-CONTRATO =
                                    ALBUM-REC(1:4)
                               MOVE ALBUM-REC(1:4) TO NR-CONTRATO-CO40
                               READ COD040 INVALID KEY
                                   INITIALIZE REG-COD040
                               END-READ

                                    PERFORM MOVER-DADOS-WORK
      *                             MOVE ALBUM-REC(1:4) TO GS-CONTRATO
      *                             PERFORM VER-FOGO
                                 END-IF
                              END-IF
                           END-IF
                       END-READ
             WHEN 3 MOVE GS-CONTRATO    TO ALBUM-REC(1: 4)
                    MOVE ZEROS          TO ALBUM-REC(5: 4)
                    START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                          MOVE "10" TO ST-RCD100
                    END-START
                    PERFORM UNTIL ST-RCD100 = "10"
                       READ RCD100 NEXT AT END
                           MOVE "10" TO ST-RCD100
                       NOT AT END
                           ADD 1 TO CONTADOR
                           MOVE CONTADOR TO GS-EXIBE-MOVTO2
                           MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           MOVE ALBUM-REC(1: 4)    TO CONTRATO-W
                           IF CONTRATO-W <> GS-CONTRATO
                              MOVE "10" TO ST-RCD100
                           ELSE
                              IF GS-VENDEDOR = 0 OR GS-VENDEDOR =
                                 VENDEDOR-REC
                               MOVE ALBUM-REC(1:4) TO NR-CONTRATO-CO40
                               READ COD040 INVALID KEY
                                   INITIALIZE REG-COD040
                               END-READ

                                 PERFORM MOVER-DADOS-WORK
                                 MOVE ALBUM-REC(1:4) TO GS-CONTRATO
                              END-IF
                           END-IF
                       END-READ
                    END-PERFORM
             WHEN 4 PERFORM INVERTE-DATA
                    INITIALIZE REG-MTD020
                               CONTADOR
                    MOVE VECTO-INI     TO DATAMOV-MTG
                    START MTD020 KEY IS NOT LESS CHAVE-MTG INVALID KEY
                         MOVE "10" TO ST-MTD020
                    END-START
                    PERFORM UNTIL ST-MTD020 = "10"
                         READ MTD020 NEXT AT END
                              MOVE "10" TO ST-MTD020
                         NOT AT END
                              ADD 1 TO CONTADOR
                              MOVE CONTADOR        TO GS-EXIBE-MOVTO2
                              MOVE DATAMOV-MTG     TO GS-EXIBE-MOVTO
                              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                              PERFORM CALL-DIALOG-SYSTEM
                              IF DATAMOV-MTG > VECTO-FIM
                                 MOVE "10" TO ST-MTD020
                              ELSE
                                 MOVE ALBUM-MTG    TO ALBUM-REC
                                 READ RCD100 NOT INVALID KEY
                                      IF GS-VENDEDOR = 0 OR VENDEDOR-REC
                                         IF GS-CONTRATO = 0 OR
                                            ALBUM-REC(1:4)
                               MOVE ALBUM-REC(1:4) TO NR-CONTRATO-CO40
                               READ COD040 INVALID KEY
                                   INITIALIZE REG-COD040
                               END-READ

                                            PERFORM MOVER-DADOS-WORK
                                         END-IF
                                      END-IF
                                 END-READ
                              END-IF
                         END-READ
                    END-PERFORM
             WHEN 5 PERFORM INVERTE-MESANO
                    INITIALIZE REG-COD040
                               CONTADOR
                    MOVE MESANO-INI TO MESANO-PREV-CO40
                    MOVE ZEROS      TO NR-CONTRATO-CO40
                    START COD040 KEY IS NOT LESS ALT1-CO40 INVALID KEY
                          MOVE "10" TO ST-COD040
                    END-START
                    PERFORM UNTIL ST-COD040 = "10"
                          READ COD040 NEXT AT END
                               MOVE "10" TO ST-COD040
                          NOT AT END
                               IF MESANO-PREV-CO40 > MESANO-FIM
                                  MOVE "10" TO ST-COD040
                               ELSE
                                  ADD 1 TO CONTADOR
                                  MOVE CONTADOR         TO
                                       GS-EXIBE-MOVTO2
                                  MOVE MESANO-PREV-CO40 TO
                                       GS-EXIBE-MOVTO
                                  MOVE "TELA-AGUARDA1"  TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  INITIALIZE REG-RCD100
                                  MOVE NR-CONTRATO-CO40 TO
                                       ALBUM-REC(1:4)
                                  START RCD100 KEY IS NOT LESS ALBUM-REC
                                                             INVALID KEY
                                       MOVE "10" TO ST-RCD100
                                  END-START
                                  PERFORM UNTIL ST-RCD100 = "10"
                                       READ RCD100 NEXT AT END
                                            MOVE "10" TO ST-RCD100
                                       NOT AT END
                                            IF NR-CONTRATO-CO40 <>
                                               ALBUM-REC(1:4)
                                               MOVE "10" TO ST-RCD100
                                            ELSE
                                               IF GS-VENDEDOR = 0 OR
                                                  VENDEDOR-REC
                                                  IF GS-CONTRATO = 0 OR
                                                     ALBUM-REC(1:4)
                                                     PERFORM
                                                        MOVER-DADOS-WORK
                                                  END-IF
                                               END-IF
                                            END-IF
                                       END-READ
                                  END-PERFORM
                               END-IF
                          END-READ
                    END-PERFORM
           END-EVALUATE.

      *    EVALUATE GS-TIPO-REL
      *        WHEN 1 PERFORM INVERTE-DATA
      *               MOVE VECTO-INI     TO DATA-MOVTO-REC
      *               MOVE ZEROS          TO ALBUM-REC
      *               START RCD100 KEY IS NOT < ALT-REC INVALID KEY
      *                     MOVE "10" TO ST-RCD100
      *               END-START
      *               PERFORM UNTIL ST-RCD100 = "10"
      *                    READ RCD100 NEXT RECORD AT END
      *                         MOVE "10" TO ST-RCD100
      *                    NOT AT END
      *                         MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
      *                         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *                         PERFORM CALL-DIALOG-SYSTEM
      *                         IF DATA-MOVTO-REC > VECTO-FIM
      *                            MOVE "10" TO ST-RCD100
      *                         ELSE
      *                            MOVE ALBUM-REC(1:4) TO GS-CONTRATO
      *                         END-IF
      *                         MOVE 9999              TO ALBUM-REC(5:8)
      *                    END-READ
      *                END-PERFORM
      *        WHEN 2 PERFORM INVERTE-DATA
      *               MOVE VECTO-INI     TO DATAVEN-REC
      *               START RCD100 KEY IS NOT < DATAVEN-REC INVALID KEY
      *                     MOVE "10" TO ST-RCD100
      *               END-START
      *               PERFORM UNTIL ST-RCD100 = "10"
      *                    READ RCD100 NEXT RECORD AT END
      *                         MOVE "10" TO ST-RCD100
      *                    NOT AT END
      *                         MOVE DATA-MOVTO-REC  TO GS-EXIBE-MOVTO
      *                         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
      *                         PERFORM CALL-DIALOG-SYSTEM
      *                         IF DATAVEN-REC > VECTO-FIM
      *                            MOVE "10" TO ST-RCD100
      *                         ELSE
      *                            IF GS-VENDEDOR = ZEROS
      *                               MOVE ALBUM-REC(1:4) TO GS-CONTRATO
      *                               PERFORM VER-FOGO
      *                            ELSE
      *                               IF GS-VENDEDOR = VENDEDOR-REC
      *                                  MOVE ALBUM-REC(1:4)
      *                                                   TO GS-CONTRATO
      *                                  PERFORM VER-FOGO
      *                                END-IF
      *                            END-IF
      *                         END-IF
      *                    END-READ
      *               END-PERFORM
      *        WHEN 3 perform ver-fogo
      *    END-EVALUATE

           PERFORM GRAVA-WORK1-RELAT.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE ZEROS TO CONTRATO-WK2.
           START WORK2 KEY IS NOT < CONTRATO-WK2 INVALID KEY
                 MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
             READ WORK2 NEXT RECORD AT END MOVE "10" TO ST-WORK2
               NOT AT END
                  ADD MONTADA-WK2   TO TOT2-MONTADA
                  ADD PRODUZIDA-WK2 TO TOT2-PRODUZIDA
                  ADD AVULSA-WK2    TO TOT2-AVULSA
                  ADD PERDIDA-WK2   TO TOT2-PERDIDA
                  ADD FORM-PROD-WK2 TO TOT2-FORM-PROD
             END-READ
           END-PERFORM.

           CLOSE WORK  OPEN INPUT WORK
           CLOSE WORK1 OPEN INPUT WORK1
           CLOSE WORK2 OPEN INPUT WORK2
           CLOSE WORK3 OPEN INPUT WORK3
           CLOSE WORK5 OPEN INPUT WORK5.


       MOVER-DADOS-WORK SECTION.

           PERFORM PESQUISAR-STATUS
           IF ACHEI = "S"
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY
                  MOVE SPACES         TO NOME-CID
                  MOVE ZEROS          TO REGIAO-CID
              END-READ

              IF GS-REGIAO = 0 OR GS-REGIAO = REGIAO-CID
                 IF GS-UF = SPACES OR UF-CID
                    MOVE ALBUM-REC          TO AUX-ALBUM
                    IF AUX-CLIENTE > ZEROS
                       ADD 1                TO TOT2-FORM-VEND
                    END-IF

                    MOVE DATA-MOVTO-REC     TO DTAMOV-WK
                    MOVE ALBUM-REC          TO ALBUM-WK

                    IF QENCADER-REC IS NOT NUMERIC
                       MOVE ZEROS           TO QENCADER-REC
                    END-IF

                    MOVE QENCADER-REC       TO TOTALB-WK

                    MOVE DATAVEN-REC        TO DATA-VENDA-WK

                    COMPUTE TOTFOT-WK = QFOTOS-REC + QABERTURA-REC +
                                        QAVULSAS-REC

                    ADD QFOTOS-REC          TO TOT2-FOTO-VEND

                    ADD QAVULSAS-REC        TO TOT2-FOTO-VEND

                    ADD QABERTURA-REC       TO TOT2-FOTO-VEND
                    MOVE QAVULSAS-REC       TO TOTAVUL-WK

                    ADD QAVULSAS-REC        TO TOT2-AVULSA-VEND

                    MOVE QCOMISSAO-REC      TO TOTCOMIS-WK

                    ADD QCOMISSAO-REC       TO TOT2-FOTO-COMI
                    MOVE QFITAS-REC         TO TOTFITA-WK
                    MOVE QDVD-REC           TO TOTDVD-WK

                    ADD QFITAS-REC          TO TOT2-FITA-VEND
                    ADD QDVD-REC            TO TOT2-DVD-VEND

                    MOVE QPOSTER-REC        TO TOTPOSTER-WK
                    MOVE QPFITA-REC         TO TOTPORTA-FITA-WK
                    MOVE QFOLHAS-REC        TO TOTFOL-WK
                    MOVE QPORTA-DVD-REC     TO TOTPORTA-DVD-WK
                    MOVE QFOTO-CD-REC       TO TOTFOTO-CD-WK
                    MOVE QMOLDURA-REC       TO TOTMOLDURA-WK

                    MOVE TOTAL-REC          TO TOTVEN-WK

                    ADD TOTAL-REC           TO TOT2-VENDA-BR
                    MOVE PM-REC             TO PM-WK
                    MOVE TOTAL-DEF-REC      TO TOTVENIDX-WK

                    ADD TOTAL-DEF-REC       TO TOT2-VENDA-LIQ
                    COMPUTE TOT2-PM-ACUM = TOT2-PM-ACUM +
                                          (TOTAL-REC * PM-REC)

                    MOVE VENDEDOR-REC       TO CODIGO-CG01
                                               COD-VENDEDOR-WK1
                                               COD-VENDEDOR-WK

                    READ CGD001 INVALID KEY
                         MOVE SPACES TO NOME-CG01
                    END-READ

                    MOVE NOME-CG01          TO VENDEDOR-WK
                                               VENDEDOR-WK1
                                               VENDEDOR-WK5

                    READ WORK5 INVALID KEY
                         MOVE ZEROS          TO TOT-MOEDA
                                                TOT-CHEQUE
                                                TOT-ANTECIPADA
                                                TOT-DUPLICATA
                                                TOT-DEBITO
                                                TOT-CARTAO
                                                TOT-VENDA-BRUTA
                         WRITE REG-WORK5
                    END-READ

                    PERFORM GRAVA-DEVOLUCAO

                    PERFORM GRAVA-WORK1

                    INITIALIZE REG-RCD101 VALOR-COMISSAO-WK
                               VALIDA-TOTAL

                    MOVE ALBUM-REC          TO ALBUM-REC1
                    START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                          MOVE "10" TO ST-RCD101
                    END-START

                    PERFORM UNTIL ST-RCD101 = "10"
                        READ RCD101 NEXT RECORD AT END
                            MOVE "10" TO ST-RCD101
                        NOT AT END
                            IF ALBUM-REC <> ALBUM-REC1
                               MOVE "10" TO ST-RCD101
                            ELSE
                               EVALUATE TIPO-REC1
                                  WHEN 1 ADD VALOR-REC1   TO TOT-CHEQUE
                                  WHEN 2 ADD VALOR-REC1   TO TOT-MOEDA
                                  WHEN 3 ADD VALOR-REC1
                                          TO TOT-ANTECIPADA
                                  WHEN 4 ADD VALOR-REC1
                                          TO TOT-DUPLICATA
                                  WHEN 5 ADD VALOR-REC1   TO TOT-DEBITO
                                  WHEN 6 ADD VALOR-REC1   TO TOT-CARTAO
      *                           WHEN OTHER
      *                                IF VENCTO-REC1 NOT > DATA-MOVTO-REC
      *                                   ADD VALOR-REC1 TO TOT-MOEDA
      *                                ELSE
      *                                   ADD VALOR-REC1 TO TOT-CHEQUE
      *                                END-IF
                               END-EVALUATE
                               IF TIPO-REC1 <> 9
                                  ADD VALOR-REC1   TO TOT-VENDA-BRUTA
                                  COMPUTE VALOR-COMISSAO-WK =
                                          VALOR-COMISSAO-WK +
                                         (VALOR-REC1 * COMIS-PARC-REC1)
                                  ADD VALOR-REC1   TO VALIDA-TOTAL
                               END-IF
                            END-IF
                        END-READ
                    END-PERFORM

                    IF VALIDA-TOTAL <> TOTAL-REC
                       move valida-total to masc-valor
                       move total-rec    to masc-valor2
                       MOVE SPACES TO MENSAGEM
                       STRING "Álbum com valores diferentes "
                               ALBUM-REC x"0a"
                              "Valor das Parcelas = " masc-valor x"0a"
                              "Valor do Recibo = " masc-valor2
                         INTO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    END-IF

                    WRITE REG-WORK

                    REWRITE REG-WORK5
                    END-REWRITE.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       GRAVA-WORK1-RELAT SECTION.
           INITIALIZE REG-MTD023

           EVALUATE GS-TIPO-REL
               WHEN 1 PERFORM POR-DATA
               WHEN 2 PERFORM POR-DATA
               WHEN 3 PERFORM POR-CONTRATO
           END-EVALUATE.

       POR-DATA SECTION.
           MOVE VECTO-INI              TO DATA-MTG3
           START MTD023 KEY IS NOT < CHAVE-MTG3 INVALID KEY
               GO TO CONTINUAR.

           IF ST-MTD023 <> "00" AND "02"
              GO TO POR-DATA.

       READ-MTD023.
           READ MTD023 NEXT RECORD AT END
               GO TO CONTINUAR.

           IF ST-MTD023 <> "00" AND "02"
              GO TO READ-MTD023.

           IF DATA-MTG3 > VECTO-FIM
              GO TO CONTINUAR.

           IF GS-VENDEDOR = 0 OR GS-VENDEDOR = VENDEDOR-MTG3
              IF GS-CONTRATO = 0 OR GS-CONTRATO = ALBUM-MTG3(1:4)
                 PERFORM MOVER-DADOS.


           GO TO READ-MTD023.
       CONTINUAR.
           EXIT.


       POR-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO ALBUM-MTG3
           START MTD023 KEY IS NOT < ALBUM-MTG3 INVALID KEY
               GO TO CONTINUAR2.

           IF ST-MTD023 <> "00" AND "02"
              GO TO POR-CONTRATO.

       READ-MTD023-2.
           READ MTD023 NEXT RECORD AT END
               GO TO CONTINUAR2.

           IF ST-MTD023 <> "00" AND "02"
              GO TO READ-MTD023-2.

           IF CONTRATO-MTG3 <> GS-CONTRATO
               GO TO CONTINUAR2.

           IF GS-VENDEDOR = 0 OR GS-VENDEDOR = VENDEDOR-MTG3
              PERFORM MOVER-DADOS.

           GO TO READ-MTD023-2.
       CONTINUAR2.
           EXIT.

       MOVER-DADOS SECTION.
           MOVE VENDEDOR-MTG3 TO CODIGO-CG01 COD-VENDEDOR-WK1
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01
           END-READ

           MOVE NOME-CG01     TO VENDEDOR-WK1

           MOVE ZEROS TO GRAVA-W
           READ WORK1 INVALID KEY
                INITIALIZE REG-WORK1
                MOVE NOME-CG01 TO VENDEDOR-WK1
                MOVE 1 TO GRAVA-W
           END-READ

           MOVE ALBUM-MTG3 TO ALBUM-MTG
           READ MTD020 NOT INVALID KEY
                ADD 1                     TO ENCARD-RELAT-WK1
      *         ADD QT-ENCADER-MTG        TO ENCARD-RELAT-WK1
                ADD QT-FOTOS-MTG          TO FOTOS-RELAT-WK1
                ADD QT-FITAS-MTG          TO FITAS-RELAT-WK1
                ADD QT-DVD-MTG            TO DVD-RELAT-WK1
                ADD QT-POSTER-MTG         TO POSTER-RELAT-WK1
                ADD QT-FOTO-CD-MTG        TO FOTO-CD-RELAT-WK1

                IF FOGO-MTG = 9
                   ADD QT-FOTOS-MTG       TO TOT2-FOTO-FOGO
                END-IF

           END-READ

           IF GRAVA-W = 1
              WRITE REG-WORK1
           ELSE
              REWRITE REG-WORK1
           END-IF



           MOVE NOME-CG01     TO VENDEDOR-WK3
           MOVE DATA-MTG3     TO DATA-VISITA-WK3
           MOVE ALBUM-MTG3    TO CONTRATO-WK3

           MOVE ZEROS TO GRAVA-W
           READ WORK3 INVALID KEY
                INITIALIZE REG-WORK3
                MOVE NOME-CG01 TO VENDEDOR-WK3
                MOVE DATA-MTG3     TO DATA-VISITA-WK3
                MOVE ALBUM-MTG3    TO CONTRATO-WK3
                MOVE 1 TO GRAVA-W
           END-READ

           MOVE ALBUM-MTG3 TO ALBUM-MTG
           READ MTD020 NOT INVALID KEY
                ADD 1                     TO ALBUM-WK3
      *         ADD QT-ENCADER-MTG        TO ALBUM-WK3
                ADD QT-FOTOS-MTG          TO FOTO-WK3
                ADD QT-FITAS-MTG          TO FITAS-WK3
                ADD QT-DVD-MTG            TO DVD-WK3
                ADD QT-POSTER-MTG         TO POSTER-WK3
                ADD QT-FOTO-CD-MTG        TO FOTO-CD-WK3
                ADD QT-MOLDURA-MTG        TO MOLDURA-WK3
      *         ADD QT-DVD-MTG            TO DVD-WK3
           END-READ

           IF GRAVA-W = 1
              WRITE REG-WORK3
           ELSE
              REWRITE REG-WORK3
           END-IF.


       GRAVA-WORK1 SECTION.
        MOVE ZEROS TO GRAVA-W.

           READ WORK1 INVALID KEY
               INITIALIZE REG-WORK1
               MOVE NOME-CG01 TO VENDEDOR-WK1
               MOVE 1 TO GRAVA-W.

           MOVE VENDEDOR-REC       TO COD-VENDEDOR-WK

           ADD 1                   TO ENCADER2-WK1

           ADD QENCADER-REC        TO ENCADER-WK1
           ADD QFOTOS-REC          TO FOTOS-WK1
           ADD QABERTURA-REC       TO FOTOS-WK1
           ADD QFITAS-REC          TO FITAS-WK1.
           ADD QDVD-REC            TO DVD-WK1.
           ADD QPOSTER-REC         TO POSTER-WK1
           ADD QFOTO-CD-REC        TO FOTO-CD-WK1

           MOVE VENDEDOR-REC       TO COD-VENDEDOR-WK1
           MOVE NOME-CG01          TO VENDEDOR-WK1.

           ADD TOTAL-REC           TO VENBRU-WK1
           ADD TOTAL-DEF-REC       TO VENDOL-WK1
           ADD DEVOL-WK            TO DEVOL-WK1.
           ADD FITDEV-WK           TO DEVFIT-WK1.
           ADD DVDDEV-WK           TO DEVDVD-WK1.
           ADD ALBDEV-WK           TO DEVALB-WK1.

           MOVE 1000 TO PRODUTO.

           READ CEAD010.

           COMPUTE PFITA-WK1 = QFITAS-REC * TABELA-PROD.

           MOVE CODALB-WK          TO PRODUTO.

           READ CEAD010 INVALID KEY
               MOVE ZEROS TO TABELA-PROD.

           COMPUTE PALBUM-WK1 = PALBUM-WK1 +
                               (QENCADER-REC * TABELA-PROD).
           COMPUTE PRAZO-W1 = PRAZO-W1 + (TOTVEN-WK * PM-WK).
           COMPUTE PRAZO-W2 = TOTVEN-WK * PM-WK.

           ADD PRAZO-W2            TO PM-WK1.

           IF GRAVA-W = 1
               WRITE REG-WORK1
           ELSE
               REWRITE REG-WORK1.


      *VER-FOGO SECTION.
      *    MOVE GS-CONTRATO       TO ALBUM-MTG(1: 4)
      *    MOVE ZEROS             TO ALBUM-MTG(5: 4)
      *
      *    START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
      *          MOVE "10" TO ST-MTD020.
      *    PERFORM UNTIL ST-MTD020 = "10"
      *      READ MTD020 NEXT RECORD AT END
      *           MOVE "10" TO ST-MTD020
      *      NOT AT END
      *        IF QT-FOTOS-MTG NOT NUMERIC
      *           MOVE ZEROS TO QT-FOTOS-MTG
      *        END-IF
      *        IF QT-ENCADER-MTG NOT NUMERIC
      *           MOVE ZEROS TO QT-ENCADER-MTG
      *        END-IF
      *        MOVE ALBUM-MTG(1: 4) TO CONTRATO-W
      *        IF FOGO-MTG NOT NUMERIC
      *           MOVE 1 TO FOGO-MTG
      *        END-IF
      *        IF CONTRATO-W NOT = GS-CONTRATO
      *          MOVE "10" TO ST-MTD020
      *        ELSE
      *           IF FOGO-MTG = 9
      *              ADD QT-FOTOS-MTG TO TOT2-FOGO
      *           END-IF
      *        END-IF
      *      END-READ
      *    END-PERFORM.

       GRAVA-DEVOLUCAO SECTION.
           IF GS-TIPO-REL <> 4
              MOVE ALBUM-REC      TO ALBUM-MTG
              READ MTD020 INVALID KEY
                   INITIALIZE REG-MTD020
              END-READ
           END-IF

           MOVE ALBUM-REC(1: 4) TO CONTRATO-MT01.

           IF FOGO-MTG = 9
               ADD QT-FOTOS-MTG   TO TOT2-FOGO
           ELSE
               ADD QT-FOTOS-MTG   TO TOT2-FOTOS-MTG.

           READ MTD001.

           MOVE CODALBUM-MT01   TO CODALB-WK.
           MOVE CODFOTO-MT01    TO CODFOT-WK.
           MOVE CONTRATO-MT01   TO CONTRATO-WK2.

           READ WORK2 INVALID KEY
                MOVE AVULSA-MT01      TO AVULSA-WK2
                MOVE PRODUZIDA-MT01   TO PRODUZIDA-WK2
                MOVE MONTADA-MT01     TO MONTADA-WK2
                MOVE PERDIDA-MT01     TO PERDIDA-WK2
                MOVE CLIEN-ALBUM-MT01 TO FORM-PROD-WK2
                WRITE REG-WORK2
                END-WRITE
           END-READ.

      *    COMPUTE SALDO-W = QT-FOTOS-MTG -
      *              (TOTFOT-WK + TOTCOMIS-WK + TOTAVUL-WK).

           COMPUTE SALDO-W = QT-FOTOS-MTG - (TOTFOT-WK + TOTCOMIS-WK).
           MOVE SALDO-W        TO DEVOL-WK.

           COMPUTE SALDO-W = QT-FITAS-MTG - TOTFITA-WK.
           MOVE SALDO-W        TO FITDEV-WK.

           COMPUTE SALDO-W = QT-DVD-MTG - TOTDVD-WK.
           MOVE SALDO-W        TO DVDDEV-WK.


           COMPUTE SALDO-W = QT-ENCADER-MTG - TOTALB-WK.
           MOVE SALDO-W        TO ALBDEV-WK.
      *--------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT1-ALBUM TOT1-FOTO TOT1-FITA TOT1-FOLHA
                         TOT1-PFITA TOT1-VENDA-BR TOT1-VENDA-IDX
                         TOT1-VLR-UNIT-FOTO TOT1-DEV-FOTO TOT1-DEV-FITA
                         TOT1-DEV-ALBUM TOT1-VLR-FOTO TOT1-VLR-ALBUM
                         TOT1-VLR-FITA TOT1-COMISSAO TOT1-DVD
                         TOT1-PORTA-DVD TOT1-FOTO-CD TOT1-MOLDURA
                         TOT1-DEV-FITA TOT1-VLR-DVD TOT1-POSTER

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.

           PERFORM ORDEM.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
      *>Número do Álbum
           MOVE ALBUM-WK          TO GS-LINDET(1: 9) WS-ALBUM

           MOVE VENDEDOR-WK       TO VENDEDOR-WK1
           READ WORK1 INVALID KEY
                INITIALIZE REG-WORK1.

      *>Álbum
           MOVE TOTALB-WK         TO QTDE-E1
           IF WS-ALB2 NOT EQUAL ZEROS
              ADD 1               TO TOT1-ALBUM
           END-IF
           MOVE QTDE-E1           TO GS-LINDET(10: 3)


      *>Fotos
           MOVE TOTFOT-WK         TO QTDE-E
           ADD TOTFOT-WK          TO TOT1-FOTO
           MOVE QTDE-E            TO GS-LINDET(13: 6)

      *>Fitas
           MOVE TOTFITA-WK        TO QTDE-E1
           ADD TOTFITA-WK         TO TOT1-FITA
           MOVE QTDE-E1           TO GS-LINDET(19: 3)

      *>Folhas
           MOVE TOTFOL-WK         TO QTDE-E7
           ADD TOTFOL-WK          TO TOT1-FOLHA
           MOVE QTDE-E7           TO GS-LINDET(22: 4)

      *>Poster
           MOVE TOTPOSTER-WK      TO QTDE-E2
           ADD TOTPOSTER-WK       TO TOT1-POSTER
           MOVE QTDE-E2           TO GS-LINDET(26: 02)

      *>Porta fita
           MOVE TOTPORTA-FITA-WK  TO QTDE-E1
           ADD TOTPORTA-FITA-WK   TO TOT1-PFITA
           MOVE QTDE-E1           TO GS-LINDET(28: 3)

      *>DVD
           MOVE TOTDVD-WK         TO QTDE-E1
           ADD TOTDVD-WK          TO TOT1-DVD
           MOVE QTDE-E1           TO GS-LINDET(32: 3)

      *>Porta DVD
           MOVE TOTPORTA-DVD-WK   TO QTDE-E1
           ADD TOTPORTA-DVD-WK    TO TOT1-PORTA-DVD
           MOVE QTDE-E1           TO GS-LINDET(36: 3)

      *>Foto CD
           MOVE TOTFOTO-CD-WK     TO QTDE-E1
           ADD TOTFOTO-CD-WK      TO TOT1-FOTO-CD
           MOVE QTDE-E1           TO GS-LINDET(39: 3)

      *>Moldura
           MOVE TOTMOLDURA-WK     TO QTDE-E1
           ADD TOTMOLDURA-WK      TO TOT1-MOLDURA
           MOVE QTDE-E1           TO GS-LINDET(42: 3)

      *>Total Vendas
           MOVE TOTVEN-WK         TO VALOR-E1
           ADD TOTVEN-WK          TO TOT1-VENDA-BR
           MOVE VALOR-E1          TO GS-LINDET(44: 11)

      *>Prazo Médio
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO GS-LINDET(55: 7)
      *    MOVE TOTVENIDX-WK      TO VALOR-E1
           ADD TOTVENIDX-WK       TO TOT1-VENDA-IDX
      *    MOVE VALOR-E1          TO GS-LINDET(45: 9)
           MOVE CODALB-WK TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-ALBUMW = TOTALB-WK * TABELA-PROD.
           MOVE VALOR-ALBUMW        TO VALOR-E2
           ADD VALOR-ALBUMW         TO TOT1-VLR-ALBUM
      *    MOVE VALOR-E2          TO GS-LINDET(54: 7)
           MOVE 1000 TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-FITAW = TOTFITA-WK * TABELA-PROD.
           MOVE VALOR-FITAW       TO VALOR-E2
           ADD VALOR-FITAW        TO TOT1-VLR-FITA
      *    MOVE VALOR-E2          TO GS-LINDET(61: 7)

      *>Data Venda
           MOVE DATA-VENDA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE SPACES            TO GS-LINDET(62:09)
           STRING DATA-INV(1:2) "/" DATA-INV(3:2) "/" DATA-INV(7:2)
                                INTO GS-LINDET(62:09)

           COMPUTE VALOR-FOTOW = (TOTVENIDX-WK - VALOR-ALBUMW -
                               VALOR-FITAW) / TOTFOT-WK.
      *>Valor por Foto
           MOVE VALOR-FOTOW       TO VALOR-E2
           ADD VALOR-FOTOW        TO TOT1-VLR-FOTO
           MOVE VALOR-E2          TO GS-LINDET(71: 7)

      *>Nome Vendedor
           MOVE VENDEDOR-WK       TO GS-LINDET(78: 10)
           MOVE ALBUM-WK          TO ALBUM-MTG
           READ MTD020 INVALID KEY
                MOVE ZEROS TO VISITA-MTG.


           MOVE VALOR-COMISSAO-WK TO COMISSAO-W

      *    MOVE COMISSAO-W        TO VALOR-E1A
           ADD COMISSAO-W         TO TOT1-COMISSAO
      *    MOVE VALOR-E1A         TO GS-LINDET(85: 7)

      *>Fotos Devolução
           MOVE DEVOL-WK          TO QTDE-E9
           ADD DEVOL-WK           TO TOT1-DEV-FOTO
           MOVE QTDE-E9           TO GS-LINDET(89: 5)

      *>%Devolução
           COMPUTE SOMA-AUX = DEVOL-WK + TOTFOT-WK.
           COMPUTE PERC-W = (DEVOL-WK / (DEVOL-WK + TOTFOT-WK)) * 100
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO GS-LINDET(94: 6)

      *>Fitas Devolvidas
           MOVE FITDEV-WK         TO QTDE-E1
           ADD FITDEV-WK          TO TOT1-DEV-FITA
           MOVE QTDE-E1           TO GS-LINDET(100: 3)

      *>Álbum Devolvido
           MOVE ALBDEV-WK         TO QTDE-E1
           ADD ALBDEV-WK          TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO GS-LINDET(103: 3)

      *DVD Devolvido
           MOVE DVDDEV-WK         TO QTDE-E1
           ADD DVDDEV-WK          TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO GS-LINDET(106: 3)

      *>Foto CD Devolvido
           MOVE FOTO-CD-DEV-WK    TO QTDE-E1
           ADD FOTO-CD-DEV-WK     TO TOT1-DEV-ALBUM
           MOVE QTDE-E1           TO GS-LINDET(109: 2).

       CARREGA-LISTA1 SECTION.
           MOVE ZEROS TO GS-CONT GS-CONT2 GS-CONT3
                         TOT-ENCADER2
                         TOT-FOTOS
                         TOT-FITAS
                         TOT-DVD
                         TOT-POSTER
                         TOT-FOTO-CD
                         TOT-ENCARD-RELAT
                         TOT-FOTOS-RELAT
                         TOT-FITAS-RELAT
                         TOT-DVD-RELAT
                         TOT-POSTER-RELAT
                         TOT-FOTO-CD-RELAT.

           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "VENDEDOR    ALBUM ALB-DV FOTOS FT-DV   FITA  FI-DV DVD
      -    "DVD-DV FOTO-CD FOTO-CD-DEV   VENDA-BRUTA   P.M.    TOTAL-VEN
      -    "DA  R$-UN %-VEN" TO  GS-LINDET1
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------" TO GS-LINDET1
           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE "                            VENDIDOS
      -    "          RELATÓRIOS                                TOTAL
      -    "                   %  " TO GS-LINDET2
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE "            -------------------------------------  ----
      -    "------------------------------------ -----------------------
      -    "---------------  -----" TO GS-LINDET2
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "VENDEDOR    ALBUM  FOTOS  FITAS DVD POSTER  FT-CD  ALBU
      -    "M  FOTOS  FITAS   DVD POSTER FOTO-CD ALBUM  FOTOS FITAS DVD
      -    " POSTER FOTO-CD  ALBUM" TO GS-LINDET2
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "----------------------" TO GS-LINDET2
           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE SPACES TO GS-LINDET GS-LINDET2.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO VENDEDOR-WK1.
           START WORK1 KEY IS NOT < VENDEDOR-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET1

                   IF VENBRU-WK1 > 0
                      MOVE "INSERE-LIST1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF

                   MOVE "INSERE-LIST2" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

              END-READ
           END-PERFORM.

           PERFORM TOTALIZA1.

       START-WORK3.
           INITIALIZE REG-WORK3.

           MOVE SPACES TO VENDEDOR-WK3
           START WORK3 KEY IS NOT < CHAVE-WK3 INVALID KEY
               MOVE "10" TO ST-WORK3.

           PERFORM UNTIL ST-WORK3 = "10"
               READ WORK3 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK3
               NOT AT END
                   PERFORM MOVER-DADOS-LINDET2
                   MOVE "INSERE-LIST3" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.
       SAIR.
           EXIT.

       MOVER-DADOS-LINDET2 SECTION.
           MOVE SPACES              TO GS-LINDET3
           MOVE VENDEDOR-WK3        TO GS-LINDET3(1:10)
           MOVE CONTRATO-WK3        TO GS-LINDET3(12:8)
           STRING DATA-VISITA-WK3(7:2) "/" DATA-VISITA-WK3(5:2) "/"
           DATA-VISITA-WK3(1:4) INTO GS-LINDET3(24:10)

           MOVE ALBUM-WK3           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(38:6)
           MOVE FOTO-WK3            TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(47:6)
           MOVE FITAS-WK3           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(55:6)
           MOVE DVD-WK3             TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(62:6)
           MOVE POSTER-WK3          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(70:6)
           MOVE FOTO-CD-WK3         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(79:6)
           MOVE MOLDURA-WK3         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET3(87:6).


       MOVER-DADOS-LINDET1 SECTION.
           MOVE SPACES TO GS-LINDET1
      *>Nome Vendedor
           MOVE VENDEDOR-WK1        TO GS-LINDET1(1:10)
      *>Encadernações
           MOVE ENCADER-WK1         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(12: 7)
      *>Devoluções Álbum
           MOVE DEVALB-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(19: 7)
      *>Fotos
           MOVE FOTOS-WK1           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(27: 7)
      *Devoluções Fotos
           MOVE DEVOL-WK1           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(31: 7)
      *Fitas
           MOVE FITAS-WK1           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(38: 7)
      *Devoluções Fitas
           MOVE DEVFIT-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(45: 7)
      *DVD
           MOVE DVD-WK1             TO QTDE-E7
           MOVE QTDE-E7             TO GS-LINDET1(52: 4)
      *Devolução DVD
           MOVE DEVDVD-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(56: 7)
      *>Foto CD
           MOVE FOTO-CD-WK1         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(64:7)
      *>Devolução Foto CD
           MOVE DEVFOTO-CD-WK1      TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET1(76:6)
      *>Venda Bruta
           MOVE VENBRU-WK1          TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET1(82: 15)
      *>Prazo Médio
           COMPUTE PRAZO-W = PM-WK1 / VENBRU-WK1
           MOVE PRAZO-W             TO PM-E
           MOVE PM-E                TO GS-LINDET1(97: 7)
      *>Venda Líquida
           MOVE VENDOL-WK1          TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET1(104: 15)
      *>Valor por Foto
           COMPUTE VALOR-FOTO-W = (VENDOL-WK1 - PFITA-WK1 - PALBUM-WK1)
                                    / FOTOS-WK1.
           MOVE VALOR-FOTO-W        TO VALOR-E1A
           MOVE VALOR-E1A           TO GS-LINDET1(119: 7)
      *>Percentual foto
           COMPUTE PERC-W = (FOTOS-WK1 / (DEVOL-WK1 + FOTOS-WK1)) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(126: 6)

      *ESTATISTICA
           MOVE SPACES TO GS-LINDET2
      *>Vendedor
           MOVE VENDEDOR-WK1        TO GS-LINDET2(1:10)
      *>Encadernação
           MOVE ENCADER2-WK1        TO QTDE-E6
           ADD  ENCADER2-WK1        TO TOT-ENCADER2
           MOVE QTDE-E6             TO GS-LINDET2(12: 7)
      *>Fotos
           MOVE FOTOS-WK1           TO QTDE-E6
           ADD  FOTOS-WK1           TO TOT-FOTOS
           MOVE QTDE-E6             TO GS-LINDET2(19: 7)
      *>Fitas
           MOVE FITAS-WK1           TO QTDE-E6
           ADD  FITAS-WK1           TO TOT-FITAS
           MOVE QTDE-E6             TO GS-LINDET2(26: 7)
      *>DVD
           MOVE DVD-WK1             TO QTDE-E7
           ADD  DVD-WK1             TO TOT-DVD
           MOVE QTDE-E7             TO GS-LINDET2(33: 4)
      *>Poster
           MOVE POSTER-WK1          TO QTDE-E6
           ADD  POSTER-WK1          TO TOT-POSTER
           MOVE QTDE-E6             TO GS-LINDET2(37:7)
      *>Foto CD
           MOVE FOTO-CD-WK1         TO QTDE-E6
           ADD  FOTO-CD-WK1         TO TOT-FOTO-CD
           MOVE QTDE-E6             TO GS-LINDET2(44:7)
      *>Encadernação
           MOVE ENCARD-RELAT-WK1    TO QTDE-E6
           ADD  ENCARD-RELAT-WK1    TO TOT-ENCARD-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(51: 7)
      *>Fotos
           MOVE FOTOS-RELAT-WK1     TO QTDE-E6
           ADD  FOTOS-RELAT-WK1     TO TOT-FOTOS-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(58: 7)
      *>Fitas
           MOVE FITAS-RELAT-WK1     TO QTDE-E6
           ADD  FITAS-RELAT-WK1     TO TOT-FITAS-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(65: 7)
      *DVD
           MOVE DVD-RELAT-WK1       TO QTDE-E
           ADD  DVD-RELAT-WK1       TO TOT-DVD-RELAT
           MOVE QTDE-E              TO GS-LINDET2(72: 5)
      *Poster
           MOVE POSTER-RELAT-WK1    TO QTDE-E
           ADD  POSTER-RELAT-WK1    TO TOT-POSTER-RELAT
           MOVE QTDE-E              TO GS-LINDET2(79: 5).
      *Foto CD
           MOVE FOTO-CD-RELAT-WK1   TO QTDE-E
           ADD  FOTO-CD-RELAT-WK1   TO TOT-FOTO-CD-RELAT
           MOVE QTDE-E              TO GS-LINDET2(87:5).
      *Total Album
           COMPUTE QTDE-E61 = ENCADER2-WK1 + ENCARD-RELAT-WK1
           MOVE QTDE-E61             TO GS-LINDET2(93: 6)
      *Total Fotos
           COMPUTE QTDE-E61 = FOTOS-WK1 + FOTOS-RELAT-WK1
           MOVE QTDE-E61             TO GS-LINDET2(100: 6)
      *Total Fitas
           COMPUTE QTDE-E61 = FITAS-WK1 + FITAS-RELAT-WK1
           MOVE QTDE-E61             TO GS-LINDET2(106: 6).
      *Total DVD
           COMPUTE QTDE-E7 = DVD-WK1 + DVD-RELAT-WK1
           MOVE QTDE-E7            TO GS-LINDET2(112: 3).
      *Total Poster
           COMPUTE QTDE-E6 = POSTER-WK1 + POSTER-RELAT-WK1
           MOVE QTDE-E6             TO GS-LINDET2(117: 6).
      *Total Foto CD
           COMPUTE QTDE-E6 = FOTO-CD-WK1 + FOTO-CD-RELAT-WK1
           MOVE QTDE-E6             TO GS-LINDET2(125: 6).

           COMPUTE PERC-W = (ENCARD-RELAT-WK1 /
                             (ENCADER2-WK1 + ENCARD-RELAT-WK1)) * 100
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET2(133: 6).

       CARREGA-LISTA5 SECTION.
           MOVE ZEROS TO GS-CONT5
                         TOTAL-VENDA
                         TOTAL-MOEDA
                         TOTAL-CHEQUE
                         TOTAL-ANTECIPADA
                         TOTAL-DUPLICATA
                         TOTAL-DEBITO
                         TOTAL-CARTAO

           MOVE SPACES TO GS-LINDET5.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "VENDEDOR      VENDA-BRUTA    TOT-MOEDA   PERC   TOT-CHE
      -    "QUE   PERC  TOT-ANTEC   PERC TOT-BOL/DP   PERC TOT-DEBIT   P
      -    "ERC TOT-CARTAO   PERC"
           TO GS-LINDET5

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------------"
           TO GS-LINDET5

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET5



           MOVE SPACES TO VENDEDOR-WK5.
           START WORK5 KEY IS NOT < VENDEDOR-WK5 INVALID KEY
                 MOVE "10" TO ST-WORK5.

           PERFORM UNTIL ST-WORK5 = "10"
              READ WORK5 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK5
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET5

                   MOVE "INSERE-LIST5" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------------"
           TO GS-LINDET5

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "GERAL         VENDA-BRUTA    TOT-MOEDA   PERC   TOT-CHE
      -    "QUE   PERC  TOT-ANTEC   PERC TOT-BOL/DP   PERC TOT-DEBIT   P
      -    "ERC TOT-CARTAO   PERC"
           TO GS-LINDET5

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------------"
           TO GS-LINDET5

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET5

           MOVE TOTAL-VENDA              TO VALOR-E31
           MOVE VALOR-E31                TO GS-LINDET5(13:14)

           MOVE TOTAL-MOEDA              TO VALOR-E32
           MOVE VALOR-E32                TO GS-LINDET5(27:13)

           COMPUTE PERC-W = TOTAL-MOEDA * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(40: 6)

           MOVE TOTAL-CHEQUE             TO VALOR-E32
           MOVE VALOR-E32                TO GS-LINDET5(47:13)

           COMPUTE PERC-W = TOTAL-CHEQUE * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(60: 6)

           MOVE TOTAL-ANTECIPADA         TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(67:11)

           COMPUTE PERC-W = TOTAL-ANTECIPADA * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(78: 6)

           MOVE TOTAL-DUPLICATA          TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(85:11)

           COMPUTE PERC-W = TOTAL-DUPLICATA * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(96: 6)

           MOVE TOTAL-DEBITO             TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(102:11)

           COMPUTE PERC-W = TOTAL-DEBITO * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(113: 6)


           MOVE TOTAL-CARTAO             TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(120:11)

           COMPUTE PERC-W = TOTAL-CARTAO * 100 / TOTAL-VENDA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(131: 6)

           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "---------------------"
           TO GS-LINDET5
           MOVE "INSERE-LIST5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET5 SECTION.
           MOVE VENDEDOR-WK5             TO GS-LINDET5(1:11)
           MOVE TOT-VENDA-BRUTA          TO VALOR-E31
           MOVE VALOR-E31                TO GS-LINDET5(13:14)

           MOVE TOT-MOEDA                TO VALOR-E32
           MOVE VALOR-E32                TO GS-LINDET5(27:13)

           COMPUTE PERC-W = TOT-MOEDA * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(40: 6)

           MOVE TOT-CHEQUE               TO VALOR-E32
           MOVE VALOR-E32                TO GS-LINDET5(47:13)

           COMPUTE PERC-W = TOT-CHEQUE * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(60: 6)

           MOVE TOT-ANTECIPADA           TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(67:11)

           COMPUTE PERC-W = TOT-ANTECIPADA * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(78: 6)

           MOVE TOT-DUPLICATA            TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(85:11)

           COMPUTE PERC-W = TOT-DUPLICATA * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(96: 6)

           MOVE TOT-DEBITO               TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(102:11)

           COMPUTE PERC-W = TOT-DEBITO * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(113: 6)


           MOVE TOT-CARTAO               TO VALOR-E3
           MOVE VALOR-E3                 TO GS-LINDET5(120:11)

           COMPUTE PERC-W = TOT-CARTAO * 100 / TOT-VENDA-BRUTA

           MOVE PERC-W                   TO PERC-E
           MOVE PERC-E                   TO GS-LINDET5(131: 6).

           ADD TOT-VENDA-BRUTA           TO TOTAL-VENDA
           ADD TOT-MOEDA                 TO TOTAL-MOEDA
           ADD TOT-CHEQUE                TO TOTAL-CHEQUE
           ADD TOT-ANTECIPADA            TO TOTAL-ANTECIPADA
           ADD TOT-DUPLICATA             TO TOTAL-DUPLICATA
           ADD TOT-DEBITO                TO TOTAL-DEBITO
           ADD TOT-CARTAO                TO TOTAL-CARTAO.


       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                START WORK KEY IS NOT < ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                START WORK KEY IS NOT < TOTVEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                START WORK KEY IS NOT < VENDEDOR-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       TOTALIZA SECTION.
           MOVE "-------------------------------------------------------
      -"----------------------------------------------------------------
      -"--------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "----------------------------------- VENDAS ------------
      -"-------------------                             ------ DEVOLUÇÃO
      -" -------"
             TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE " CLI.   FOTOS  FITA  FOLHAS   POS   PFT    VENDA-BRUTA
      -"  P.M. VENDA-INDEXAD   VLR-FOTO   VLR-COMISSÃO  FOTOS  %-DEV  FI
      -"TA   ALB" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "-------------------------------------------------------
      -"----------------------------------------------------------------
      -"--------" TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES               TO GS-LINDET.
           MOVE TOT1-ALBUM           TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINDET(1: 6)
           MOVE TOT1-FOTO            TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINDET(7: 8)
           MOVE TOT1-FITA            TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINDET(15: 6)
           MOVE TOT1-FOLHA           TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINDET(21: 8)
           MOVE TOT1-POSTER          TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(29: 6)
           MOVE TOT1-PFITA           TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(35: 6)
           MOVE TOT1-VENDA-BR        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(41: 15)
           COMPUTE PRAZO-W = PRAZO-W1 / TOT1-VENDA-BR.
           MOVE PRAZO-W              TO PM-E
           MOVE PM-E                 TO GS-LINDET(56: 6)
           MOVE TOT1-VENDA-IDX       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(62: 15)
           COMPUTE TOT1-VLR-UNIT-FOTO = (TOT1-VENDA-IDX - TOT1-VLR-FITA
                              - TOT1-VLR-ALBUM) / TOT1-FOTO.

           MOVE TOT1-VLR-UNIT-FOTO   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET(77:11)
           MOVE TOT1-COMISSAO        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(88: 15)
           MOVE TOT1-DEV-FOTO        TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINDET(103: 6).
           COMPUTE PERC-W = (TOT1-DEV-FOTO / (TOT1-DEV-FOTO +
                                  TOT1-FOTO)) * 100
           MOVE PERC-W               TO PERC-E
           MOVE PERC-E               TO GS-LINDET(111: 6)
           MOVE TOT1-DEV-FITA        TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(117: 6).
           MOVE TOT1-DEV-ALBUM       TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(123: 5).

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *    MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA1 SECTION.
           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "-------------------------------" TO GS-LINDET1
           MOVE "INSERE-LIST1"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "--- FORMANDOS ---  --------------- FOTOGRAFIAS---------
      -    "------       VEND  ---- RESULT FOTOS %----   ----------FATUR
      -    "AMENTO----------- P.MED V.MED." TO GS-LINDET1
           MOVE "INSERE-LIST1"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE " PROD  VEND  SALD  DISPON   COM   VENDA  DEVOLU  FOGO
      -    " SALDO  FITA VENDA COMIS DEVOL  FOGO SALDO   VENDA-BRUTA   P
      -    ".M.    VLIQ-INDEX R$FOT CLIENTE" TO GS-LINDET1
           MOVE "INSERE-LIST1"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "-------------------------------" TO GS-LINDET1
           MOVE "INSERE-LIST1"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES TO GS-LINDET1.
      * Produção Formandos
           MOVE TOT2-FORM-PROD      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(1: 6)
      * Vendas Formandos
           MOVE TOT2-FORM-VEND      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(7: 6)
      * Saldo Formandos
           COMPUTE TOT2-FORM-SALD = TOT2-FORM-PROD - TOT2-FORM-VEND.
           MOVE TOT2-FORM-SALD      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(13: 6)
      *Foto Disponivel
           COMPUTE TOT2-FOTO-DISP = (TOT2-MONTADA + TOT2-AVULSA)
           MOVE TOT2-FOTO-DISP      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(19: 8)

      *Foto Comissão
           MOVE TOT2-FOTO-COMI      TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET1(27: 6)
      *Fotos Vendidas
           MOVE TOT2-FOTO-VEND      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(33: 8)

      *Fotos Devolução
           COMPUTE TOT2-FOTO-DEVL = TOT2-FOTOS-MTG -
                   TOT2-FOTO-VEND - TOT2-FOTO-COMI
           MOVE TOT2-FOTO-DEVL      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(41: 8)

      *Fotos Fogo
           MOVE TOT2-FOTO-FOGO      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(49:6)

      *Fotos Saldo
           COMPUTE TOT2-FOTO-SALD = TOT2-PRODUZIDA - (TOT2-FOTO-VEND +
                    TOT2-PERDIDA + TOT2-FOTO-DEVL +
                    TOT2-FOTO-COMI + TOT2-FOTO-FOGO).

           MOVE TOT2-FOTO-SALD      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(55: 8)

      *Fitas Vendidas
           MOVE TOT2-FITA-VEND      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(63: 6)

      *% Vendido
           COMPUTE TOT2-PERC-VEND = (TOT2-FOTO-VEND / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-VEND      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(69: 6)


      *% Comissao
           COMPUTE TOT2-PERC-COMIS = (TOT2-FOTO-COMI / TOT2-FOTO-DISP) *
                                      100.
           MOVE TOT2-PERC-COMIS     TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(75: 6)

      *% Devolvido
           COMPUTE TOT2-PERC-DEVL = (TOT2-FOTO-DEVL / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-DEVL      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(81: 6)

      *% Fogo
           COMPUTE TOT2-PERC-FOGO = (TOT2-FOTO-FOGO / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-FOGO      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(87: 6)

      *% Saldo
           COMPUTE TOT2-PERC-SALD = (TOT2-FOTO-SALD / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-SALD      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(93: 6)

      * Venda Bruta
           MOVE TOT2-VENDA-BR       TO VALOR-E31
           MOVE VALOR-E31           TO GS-LINDET1(99: 14)

      * Prazo Médio
           COMPUTE PRAZO-W = TOT2-PM-ACUM / TOT2-VENDA-BR.
           MOVE PRAZO-W             TO PM-E
           MOVE PM-E                TO GS-LINDET1(113: 7)

      * Venda Líquida
           MOVE TOT2-VENDA-LIQ      TO VALOR-E31
           MOVE VALOR-E31           TO GS-LINDET1(120: 14)

      * Preço Médio Foto
           COMPUTE TOT2-PRECO-FOT = TOT2-VENDA-LIQ / TOT2-FOTO-VEND.
           MOVE TOT2-PRECO-FOT      TO VALOR-E4
           MOVE VALOR-E4            TO GS-LINDET1(134: 6)

      * Preço Médio Formando
           COMPUTE TOT2-VENDA-CLI = TOT2-VENDA-LIQ / TOT2-FORM-VEND.
           MOVE TOT2-VENDA-CLI      TO VALOR-E2A.
           MOVE VALOR-E2A           TO GS-LINDET1(140: 7).

           MOVE "INSERE-LIST1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


      * TOTAL ESTATISTICA

           MOVE ALL "-" TO GS-LINDET2(1:138)

           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE SPACES TO GS-LINDET2
      *>Encadernações
           MOVE TOT-ENCADER2        TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(12: 7)
      *>Fotos
           MOVE TOT-FOTOS           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(19: 7)
      *>Fitas
           MOVE TOT-FITAS           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(26: 7)
      *>DVD
           MOVE TOT-DVD             TO QTDE-E7
           MOVE QTDE-E7             TO GS-LINDET2(33: 4)
      *>Poster
           MOVE TOT-POSTER          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(37:7)
      *>Foto CD
           MOVE TOT-FOTO-CD         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(44:7)
      *>Encadernações
           MOVE TOT-ENCARD-RELAT    TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(51: 7)
      *>Fotos
           MOVE TOT-FOTOS-RELAT     TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(58: 7)
      *>Fitas
           MOVE TOT-FITAS-RELAT     TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(65: 7)
      *>DVD
           MOVE TOT-DVD-RELAT       TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(72: 5)
      *>Poster
           MOVE TOT-POSTER-RELAT    TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(79: 5).
      *>Foto CD
           MOVE TOT-FOTO-CD-RELAT   TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(87:5).

      *>Encadernações
           COMPUTE QTDE-E61 = TOT-ENCADER2 + TOT-ENCARD-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(93: 6)

      *>Fotos
           COMPUTE QTDE-E61 = TOT-FOTOS + TOT-FOTOS-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(100: 6)

      *>Fitas
           COMPUTE QTDE-E61 = TOT-FITAS + TOT-FITAS-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(106: 6).

      *>DVD
           COMPUTE QTDE-E7 = TOT-DVD + TOT-DVD-RELAT
           MOVE QTDE-E7            TO GS-LINDET2(112: 3).

      *>Poster
           COMPUTE QTDE-E6 = TOT-POSTER + TOT-POSTER-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(117: 6).

      *>Foto CD
           COMPUTE QTDE-E6 = TOT-FOTO-CD + TOT-FOTO-CD-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(125: 6).

      *>Percentual
           COMPUTE PERC-W = (TOT-ENCARD-RELAT /
                            (TOT-ENCADER2 + TOT-ENCARD-RELAT)) * 100
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET2(133: 6).

           MOVE "INSERE-LIST2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP203" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE 0 TO QUANTIDADE
           PERFORM UNTIL QUANTIDADE = GS-COPIAS
               ADD 1 TO QUANTIDADE
               PERFORM IMPRIMIR
           END-PERFORM.

       IMPRIMIR SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           IF GS-IMPR-RELATORIO-TRUE
              PERFORM MOVER-WORK-REL.

           IF GS-IMPR-RESUMO-TRUE
              PERFORM IMPRIME-RESUMO.

           IF GS-IMPR-ESTATISTICA-TRUE
              PERFORM IMPRIME-ESTATISTICA.

           IF GS-IMPR-ALBUM-TRUE
              PERFORM IMPRIME-ALBUM.

           IF GS-IMPR-RATEIO-TRUE
              PERFORM IMPRIME-RATEIO.

           COPY DESCONDENSA.


       MOVER-WORK-REL SECTION.
           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE ALBUM-WK          TO LINDET-REL(1: 9)

           MOVE VENDEDOR-WK       TO VENDEDOR-WK1
           READ WORK1 INVALID KEY INITIALIZE REG-WORK1.
           MOVE TOTALB-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(10: 3)
           MOVE TOTFOT-WK         TO QTDE-E44
           MOVE QTDE-E44          TO LINDET-REL(12: 4)
           MOVE TOTFITA-WK        TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(18: 3)
           MOVE TOTFOL-WK         TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(20: 3)
           MOVE TOTPOSTER-WK      TO QTDE-E2
           MOVE QTDE-E2           TO LINDET-REL(24: 02)
           MOVE TOTPORTA-FITA-WK  TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(26: 3)
           MOVE TOTDVD-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(31:3)
           MOVE TOTPORTA-DVD-WK   TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(34:3)
           MOVE TOTFOTO-CD-WK     TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(37:3)
           MOVE TOTMOLDURA-WK     TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(41:3)
           MOVE TOTVEN-WK         TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(44: 9)
           MOVE PM-WK             TO PM-E
           MOVE PM-E              TO LINDET-REL(53: 7)
      *    MOVE TOTVENIDX-WK      TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(45: 9)
           MOVE CODALB-WK TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-ALBUMW = TOTALB-WK * TABELA-PROD.
           MOVE VALOR-ALBUMW      TO VALOR-E2
      *    MOVE VALOR-E2          TO LINDET-REL(54: 7)
           MOVE 1000 TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE ZEROS TO TABELA-PROD.
           COMPUTE VALOR-FITAW = TOTFITA-WK * TABELA-PROD.
           MOVE VALOR-FITAW       TO VALOR-E2
      *    MOVE VALOR-E2          TO LINDET-REL(61: 7)

           MOVE DATA-VENDA-WK     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV

           MOVE SPACES            TO LINDET-REL(61:08)
           STRING DATA-INV(1:2) "/" DATA-INV(3:2) "/" DATA-INV(7:2)
                                INTO LINDET-REL(61:08)

           COMPUTE VALOR-FOTOW = (TOTVENIDX-WK - VALOR-ALBUMW -
                               VALOR-FITAW) / TOTFOT-WK.
           MOVE VALOR-FOTOW       TO VALOR-E2
           MOVE VALOR-E2          TO LINDET-REL(69: 7)
           MOVE VENDEDOR-WK       TO LINDET-REL(76: 10)
           MOVE ALBUM-WK          TO ALBUM-MTG
           READ MTD020 INVALID KEY MOVE ZEROS TO VISITA-MTG.

           MOVE VALOR-COMISSAO-WK TO COMISSAO-W

      *    MOVE COMISSAO-W        TO VALOR-E1
      *    MOVE VALOR-E1          TO LINDET-REL(85: 9)
           MOVE DEVOL-WK          TO QTDE-E9
           MOVE QTDE-E9           TO LINDET-REL(87: 4)
           COMPUTE SOMA-AUX = DEVOL-WK + TOTFOT-WK.
           COMPUTE PERC-W = (DEVOL-WK / (DEVOL-WK + TOTFOT-WK)) * 100
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO LINDET-REL(91: 7)
           MOVE FITDEV-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(98: 3)
           MOVE ALBDEV-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(101: 2)
           MOVE DVDDEV-WK         TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(104:2)
           MOVE FOTO-CD-DEV-WK    TO QTDE-E1
           MOVE QTDE-E1           TO LINDET-REL(107:2).

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       IMPRIME-RESUMO SECTION.
           PERFORM CABECALHO1.
           MOVE SPACES TO LINDET1-REL.
           MOVE SPACES TO VENDEDOR-WK1.
           START WORK1 KEY IS NOT < VENDEDOR-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   IF VENBRU-WK1 > 0
                      PERFORM MOVER-DADOS-RELAT1-REL
                      WRITE REG-RELAT FROM LINDET1
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO1
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA1-REL.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.

       MOVER-DADOS-RELAT1-REL SECTION.
           MOVE VENDEDOR-WK1        TO LINDET1-REL(1:11)
           MOVE ENCADER-WK1         TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(12: 6)
           MOVE DEVALB-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(18: 6)
           MOVE FOTOS-WK1           TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(24: 6)
           MOVE DEVOL-WK1           TO QTDE-E9
           MOVE QTDE-E9             TO LINDET1-REL(30: 6)
           MOVE FITAS-WK1           TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(36: 6)
           MOVE DEVFIT-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(42: 6)
           MOVE DVD-WK1             TO QTDE-E7
           MOVE QTDE-E7             TO LINDET1-REL(48:4)
           MOVE DEVDVD-WK1          TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(54:6)
           MOVE FOTO-CD-WK1         TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(62:6)
           MOVE DEVFOTO-CD-WK1      TO QTDE-E6
           MOVE QTDE-E6             TO LINDET1-REL(74:6)

           MOVE VENBRU-WK1          TO VALOR-E
           MOVE VALOR-E             TO LINDET1-REL(80: 14)
           COMPUTE PRAZO-W = PM-WK1 / VENBRU-WK1
           MOVE PRAZO-W             TO PM-E
           MOVE PM-E                TO LINDET1-REL(94: 6)
           MOVE VENDOL-WK1          TO VALOR-E
           MOVE VALOR-E             TO LINDET1-REL(100: 14)
           COMPUTE VALOR-FOTO-W = (VENDOL-WK1 - PFITA-WK1 - PALBUM-WK1)
                                    / FOTOS-WK1.
           MOVE VALOR-FOTO-W        TO VALOR-E1
           MOVE VALOR-E1            TO LINDET1-REL(114: 9)

           COMPUTE PERC-W = (FOTOS-WK1 / (DEVOL-WK1 + FOTOS-WK1)) * 100.
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO LINDET1-REL(123: 6).

       IMPRIME-ESTATISTICA SECTION.
           MOVE ZEROS TO TOT-ENCADER2
                         TOT-FOTOS
                         TOT-FITAS
                         TOT-DVD
                         TOT-POSTER
                         TOT-FOTO-CD
                         TOT-ENCARD-RELAT
                         TOT-FOTOS-RELAT
                         TOT-FITAS-RELAT
                         TOT-DVD-RELAT
                         TOT-POSTER-RELAT
                         TOT-FOTO-CD-RELAT.

           PERFORM CABECALHO3
           MOVE SPACES TO VENDEDOR-WK1.
           START WORK1 KEY IS NOT < VENDEDOR-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   MOVE SPACES TO GS-LINDET2

                   MOVE VENDEDOR-WK1        TO GS-LINDET2(1:11)

                   MOVE ENCADER2-WK1        TO QTDE-E6
                   ADD  ENCADER2-WK1        TO TOT-ENCADER2
                   MOVE QTDE-E6             TO GS-LINDET2(12: 6)

                   MOVE FOTOS-WK1           TO QTDE-E6
                   ADD  FOTOS-WK1           TO TOT-FOTOS
                   MOVE QTDE-E6             TO GS-LINDET2(19: 6)

                   MOVE FITAS-WK1           TO QTDE-E6
                   ADD  FITAS-WK1           TO TOT-FITAS
                   MOVE QTDE-E6             TO GS-LINDET2(25: 6)

                   MOVE DVD-WK1             TO QTDE-E7
                   ADD  DVD-WK1             TO TOT-DVD
                   MOVE QTDE-E7             TO GS-LINDET2(31:3)

                   MOVE POSTER-WK1          TO QTDE-E6
                   ADD  POSTER-WK1          TO TOT-POSTER
                   MOVE QTDE-E6             TO GS-LINDET2(36:6)

                   MOVE FOTO-CD-WK1         TO QTDE-E6
                   ADD  FOTO-CD-WK1         TO TOT-FOTO-CD
                   MOVE QTDE-E6             TO GS-LINDET2(44:6)

                   MOVE ENCARD-RELAT-WK1    TO QTDE-E6
                   ADD  ENCARD-RELAT-WK1    TO TOT-ENCARD-RELAT
                   MOVE QTDE-E6             TO GS-LINDET2(50: 6)

                   MOVE FOTOS-RELAT-WK1     TO QTDE-E6
                   ADD  FOTOS-RELAT-WK1     TO TOT-FOTOS-RELAT
                   MOVE QTDE-E6             TO GS-LINDET2(57: 6)

                   MOVE FITAS-RELAT-WK1     TO QTDE-E6
                   ADD  FITAS-RELAT-WK1     TO TOT-FITAS-RELAT
                   MOVE QTDE-E6             TO GS-LINDET2(63: 6)

                   MOVE DVD-RELAT-WK1       TO QTDE-E
                   ADD  DVD-RELAT-WK1       TO TOT-DVD-RELAT
                   MOVE QTDE-E              TO GS-LINDET2(68:4)

                   MOVE POSTER-RELAT-WK1    TO QTDE-E
                   ADD  POSTER-RELAT-WK1    TO TOT-POSTER-RELAT
                   MOVE QTDE-E              TO GS-LINDET2(75:4)

                   MOVE FOTO-CD-RELAT-WK1   TO QTDE-E
                   ADD  TOT-FOTO-CD-RELAT   TO TOT-FOTO-CD-RELAT
                   MOVE QTDE-E              TO GS-LINDET2(83:4)


                   COMPUTE QTDE-E61 = ENCADER2-WK1 + ENCARD-RELAT-WK1
                   MOVE QTDE-E61             TO GS-LINDET2(88: 6)

                   COMPUTE QTDE-E61 = FOTOS-WK1 + FOTOS-RELAT-WK1
                   MOVE QTDE-E61             TO GS-LINDET2(95: 6)

                   COMPUTE QTDE-E61 = FITAS-WK1 + FITAS-RELAT-WK1
                   MOVE QTDE-E61             TO GS-LINDET2(101: 6)

                   COMPUTE QTDE-E7 = DVD-WK1 + DVD-RELAT-WK1
                   MOVE QTDE-E7             TO GS-LINDET2(107: 3)

                   COMPUTE QTDE-E6  = POSTER-WK1 + POSTER-RELAT-WK1
                   MOVE QTDE-E6              TO GS-LINDET2(112: 6)

                   COMPUTE QTDE-E6  = FOTO-CD-WK1 + FOTO-CD-RELAT-WK1
                   MOVE QTDE-E6              TO GS-LINDET2(120: 6)

                   COMPUTE PERC-W = (ENCARD-RELAT-WK1 /
                                (ENCADER2-WK1 + ENCARD-RELAT-WK1)) * 100
                   MOVE PERC-W              TO PERC-E
                   MOVE PERC-E              TO GS-LINDET2(126: 6)

                   MOVE GS-LINDET2 TO LINTOT1-REL

                   WRITE REG-RELAT FROM LINTOT1
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO3
                   END-IF

              END-READ
           END-PERFORM.

           MOVE ALL "-" TO LINTOT1-REL(1:138)
           WRITE REG-RELAT FROM LINTOT1

           MOVE SPACES TO GS-LINDET2

           MOVE TOT-ENCADER2        TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(12: 6)
           MOVE TOT-FOTOS           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(19: 6)
           MOVE TOT-FITAS           TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(25: 6)
           MOVE TOT-DVD             TO QTDE-E7
           MOVE QTDE-E7             TO GS-LINDET2(31: 3)
           MOVE TOT-POSTER          TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(36:6)
           MOVE TOT-FOTO-CD         TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(44:6)

           MOVE TOT-ENCARD-RELAT    TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(50: 6)
           MOVE TOT-FOTOS-RELAT     TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(57: 6)
           MOVE TOT-FITAS-RELAT     TO QTDE-E6
           MOVE QTDE-E6             TO GS-LINDET2(63: 6)
           MOVE TOT-DVD-RELAT       TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(68: 4)
           MOVE TOT-POSTER-RELAT    TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(75: 4).
           MOVE TOT-FOTO-CD-RELAT   TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET2(83:4).

           COMPUTE QTDE-E61 = TOT-ENCADER2 + TOT-ENCARD-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(88: 6)

           COMPUTE QTDE-E61 = TOT-FOTOS + TOT-FOTOS-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(95: 6)

           COMPUTE QTDE-E61 = TOT-FITAS + TOT-FITAS-RELAT
           MOVE QTDE-E61             TO GS-LINDET2(101: 6).

           COMPUTE QTDE-E7 = TOT-DVD + TOT-DVD-RELAT
           MOVE QTDE-E7            TO GS-LINDET2(107: 3).

           COMPUTE QTDE-E6 = TOT-POSTER + TOT-POSTER-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(112: 6).

           COMPUTE QTDE-E6 = TOT-FOTO-CD + TOT-FOTO-CD-RELAT
           MOVE QTDE-E6             TO GS-LINDET2(120: 6).

           COMPUTE PERC-W = (TOT-ENCARD-RELAT /
                            (TOT-ENCADER2 + TOT-ENCARD-RELAT)) * 100
           MOVE PERC-W              TO PERC-E
           MOVE PERC-E              TO GS-LINDET2(126: 6).

           MOVE GS-LINDET2 TO LINTOT1-REL

           WRITE REG-RELAT FROM LINTOT1.


       IMPRIME-ALBUM SECTION.
           PERFORM CABECALHO4
           INITIALIZE REG-WORK3.

           MOVE SPACES TO VENDEDOR-WK3
           START WORK3 KEY IS NOT < CHAVE-WK3 INVALID KEY
               GO TO SAIR3.

           IF ST-WORK3 <> "00" AND "02"
              GO TO IMPRIME-ALBUM.

       READ-WORK33.
           READ WORK3 NEXT RECORD AT END
               GO TO SAIR3.

           IF ST-WORK3 <> "00" AND "02"
              GO TO READ-WORK33.

           PERFORM MOVER-DADOS-LINDET2

           MOVE GS-LINDET3 TO LINTOT1-REL

           WRITE REG-RELAT FROM LINTOT1
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO4
           END-IF

           GO TO READ-WORK33.

       SAIR3.
           EXIT.

       IMPRIME-RATEIO SECTION.
           PERFORM CABECALHO5

           MOVE 1 TO GS-CONT5
           MOVE SPACES TO GS-LINDET5
           MOVE "LER-LB5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET5 = SPACES
               WRITE REG-RELAT FROM GS-LINDET5
               ADD 1 TO LIN
               IF LIN > 56
                  PERFORM CABECALHO5
               END-IF
               ADD 1 TO GS-CONT5
               MOVE SPACES TO GS-LINDET5
               MOVE "LER-LB5" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.


       TOTALIZA-REL SECTION.
           MOVE "------------------------------ VENDAS -----------------
      -"---------                         ---- DEVOLUÇÃO -----" TO
           GS-LINDET
           WRITE REG-RELAT FROM GS-LINDET AFTER 2.

           MOVE "CLI.  FOTOS FITA FOLHAS  POS PFT      VENDA-BRUTA   P.M
      -".  VENDA-INDEXAD VLR-FOTO VLR-COMISSÃO  FOTOS  %-DEV FITA ALB"
           TO GS-LINDET
           WRITE REG-RELAT FROM GS-LINDET

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "-------------" TO GS-LINDET
           WRITE REG-RELAT FROM GS-LINDET


           MOVE SPACES               TO GS-LINDET.
           MOVE TOT1-ALBUM           TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINDET(1: 5)
           MOVE TOT1-FOTO            TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINDET(6: 7)
           MOVE TOT1-FITA            TO QTDE-E4
           MOVE QTDE-E4              TO GS-LINDET(13: 5)
           MOVE TOT1-FOLHA           TO QTDE-E5
           MOVE QTDE-E5              TO GS-LINDET(18: 7)
           MOVE TOT1-POSTER          TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(25: 4)
           MOVE TOT1-PFITA           TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(29: 4)
           MOVE TOT1-VENDA-BR        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(37: 14)
           COMPUTE PRAZO-W = PRAZO-W1 / TOT1-VENDA-BR.
           MOVE PRAZO-W              TO PM-E
           MOVE PM-E                 TO GS-LINDET(51: 6)
           MOVE TOT1-VENDA-IDX       TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(59: 14)
           COMPUTE TOT1-VLR-UNIT-FOTO = (TOT1-VENDA-IDX - TOT1-VLR-FITA
                              - TOT1-VLR-ALBUM) / TOT1-FOTO.

           MOVE TOT1-VLR-UNIT-FOTO   TO VALOR-E1
           MOVE VALOR-E1             TO GS-LINDET(73: 9)
           MOVE TOT1-COMISSAO        TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(81: 14)
           MOVE TOT1-DEV-FOTO        TO QTDE-E6
           MOVE QTDE-E6              TO GS-LINDET(96: 5).
           COMPUTE PERC-W = (TOT1-DEV-FOTO / (TOT1-DEV-FOTO +
                                  TOT1-FOTO)) * 100
           MOVE PERC-W               TO PERC-E
           MOVE PERC-E               TO GS-LINDET(102: 7)
           MOVE TOT1-DEV-FITA        TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(109: 4).
           MOVE TOT1-DEV-ALBUM       TO QTDE-E
           MOVE QTDE-E               TO GS-LINDET(113: 3).

           WRITE REG-RELAT FROM GS-LINDET.

       TOTALIZA1-REL SECTION.

           MOVE "-- FORMANDOS-- ---------- FOTOGRAFIAS--------- VEND --R
      -   "ESULT FOTOS %--   FATURAMENTO                  P-MED  V.MED."
           TO GS-LINDET1
           WRITE REG-RELAT FROM GS-LINDET1 AFTER 2.

           MOVE "PROD VEND SALD DISPON COM  VENDA DEVOLU  SALDO FITA  VE
      -   "NDA DEVOL SALDO   VENDA-BRUTA  P.M. VLIQ-INDEX R$FOT CLIENTE"
           TO GS-LINDET1
           WRITE REG-RELAT FROM GS-LINDET1

           MOVE "-------------------------------------------------------
      -    "------------------------------------------------------------
      -    "-------------" TO GS-LINDET1
           WRITE REG-RELAT FROM GS-LINDET1

           MOVE SPACES TO GS-LINDET1.
           MOVE TOT2-FORM-PROD      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(1: 5)
           MOVE TOT2-FORM-VEND      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(6: 5)
           COMPUTE TOT2-FORM-SALD = TOT2-FORM-PROD - TOT2-FORM-VEND.
           MOVE TOT2-FORM-SALD      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(11: 5)

           COMPUTE TOT2-FOTO-DISP = (TOT2-MONTADA + TOT2-AVULSA)
      *    -
      *                             (TOT2-FOTO-COMI)
      *                             + TOT2-FOGO).
           MOVE TOT2-FOTO-DISP      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(16: 7)
           MOVE TOT2-FOTO-COMI      TO QTDE-E
           MOVE QTDE-E              TO GS-LINDET1(23: 4)
           MOVE TOT2-FOTO-VEND      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(27: 7)

           COMPUTE TOT2-FOTO-DEVL = TOT2-FOTOS-MTG -
                   TOT2-FOTO-VEND - TOT2-FOTO-COMI


           MOVE TOT2-FOTO-DEVL      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(34: 7)
           COMPUTE TOT2-FOTO-SALD = TOT2-PRODUZIDA - (TOT2-FOTO-VEND +
                    TOT2-PERDIDA + TOT2-FOTO-DEVL +
                    TOT2-FOTO-COMI + TOT2-FOTO-FOGO).

           MOVE TOT2-FOTO-SALD      TO QTDE-E5
           MOVE QTDE-E5             TO GS-LINDET1(41: 7)
           MOVE TOT2-FITA-VEND      TO QTDE-E4
           MOVE QTDE-E4             TO GS-LINDET1(48: 5)
           COMPUTE TOT2-PERC-VEND = (TOT2-FOTO-VEND / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-VEND      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(53: 6)
           COMPUTE TOT2-PERC-DEVL = (TOT2-FOTO-DEVL / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-DEVL      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(59: 6)
           COMPUTE TOT2-PERC-SALD = (TOT2-FOTO-SALD / TOT2-FOTO-DISP) *
                                     100.
           MOVE TOT2-PERC-SALD      TO PERC-E
           MOVE PERC-E              TO GS-LINDET1(65: 6)
           MOVE TOT2-VENDA-BR       TO VALOR-E3
           MOVE VALOR-E3            TO GS-LINDET1(75: 11)
           COMPUTE PRAZO-W = TOT2-PM-ACUM / TOT2-VENDA-BR.
           MOVE PRAZO-W             TO PM-E
           MOVE PM-E                TO GS-LINDET1(85: 6)
           MOVE TOT2-VENDA-LIQ      TO VALOR-E3
           MOVE VALOR-E3            TO GS-LINDET1(92: 11)
           COMPUTE TOT2-PRECO-FOT = TOT2-VENDA-BR / TOT2-FOTO-VEND.
           MOVE TOT2-PRECO-FOT      TO VALOR-E4
           MOVE VALOR-E4            TO GS-LINDET1(103: 6)
           COMPUTE TOT2-VENDA-CLI = TOT2-VENDA-LIQ / TOT2-FORM-VEND.
           MOVE TOT2-VENDA-CLI      TO VALOR-E2A.
           MOVE VALOR-E2A           TO GS-LINDET1(109: 7).

           WRITE REG-RELAT FROM GS-LINDET1.

       CABECALHO SECTION.
           EVALUATE GS-TIPO-REL
              WHEN 1 MOVE "DATA MOVTO"    TO ORDEM-REL
                     MOVE "DATA MOVTO: "  TO DESC-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 2 MOVE "DATA VENDA"    TO ORDEM-REL
                     MOVE "DATA VENDA: "  TO DESC-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 3 MOVE "CONTRATO  "    TO ORDEM-REL
                     MOVE "CONTRATO..: "  TO DESC-ORDEM-REL(1: 12)
                     MOVE GS-CONTRATO     TO DESC-ORDEM-REL(13: 4)
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
           END-EVALUATE.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           ADD 6 TO LIN.
       CABECALHO1 SECTION.
           EVALUATE GS-TIPO-REL
              WHEN 1 MOVE "DATA MOVTO"    TO ORDEM1-REL
                                             ORDEM2-REL
                                             ORDEM3-REL
                     MOVE "DATA MOVTO: "  TO DESC1-ORDEM-REL(1: 12)
                                             DESC2-ORDEM-REL(1: 12)
                                             DESC3-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 2 MOVE "DATA VENDA"    TO ORDEM1-REL
                                             ORDEM2-REL
                                             ORDEM3-REL
                     MOVE "DATA VENDA: "  TO DESC1-ORDEM-REL(1: 12)
                                             DESC2-ORDEM-REL(1: 12)
                                             DESC3-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 3 MOVE "CONTRATO  "    TO ORDEM1-REL
                                             ORDEM2-REL
                                             ORDEM3-REL
                     MOVE "CONTRATO..: "  TO DESC1-ORDEM-REL(1: 12)
                                             DESC2-ORDEM-REL(1: 12)
                                             DESC3-ORDEM-REL(1: 12)
                     MOVE GS-CONTRATO     TO DESC1-ORDEM-REL(13: 4)
                                             DESC2-ORDEM-REL(13: 4)
                                             DESC3-ORDEM-REL(13: 4)
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
           END-EVALUATE.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02A.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB03.
           ADD 5 TO LIN.
       CABECALHO3 SECTION.
           EVALUATE GS-TIPO-REL
              WHEN 1 MOVE "DATA MOVTO"    TO ORDEM1-REL
                     MOVE "DATA MOVTO: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 2 MOVE "DATA VENDA"    TO ORDEM1-REL
                     MOVE "DATA VENDA: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 3 MOVE "CONTRATO  "    TO ORDEM1-REL
                     MOVE "CONTRATO..: "  TO DESC1-ORDEM-REL(1: 12)
                     MOVE GS-CONTRATO     TO DESC1-ORDEM-REL(13: 4)
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
           END-EVALUATE.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02B.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB009.
           WRITE REG-RELAT FROM CAB010.
           WRITE REG-RELAT FROM CAB03.
           ADD 6 TO LIN.

       CABECALHO4 SECTION.
           EVALUATE GS-TIPO-REL
              WHEN 1 MOVE "DATA MOVTO"    TO ORDEM1-REL
                     MOVE "DATA MOVTO: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 2 MOVE "DATA VENDA"    TO ORDEM1-REL
                     MOVE "DATA VENDA: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 3 MOVE "CONTRATO  "    TO ORDEM1-REL
                     MOVE "CONTRATO..: "  TO DESC1-ORDEM-REL(1: 12)
                     MOVE GS-CONTRATO     TO DESC1-ORDEM-REL(13: 4)
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
           END-EVALUATE.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02C.
           MOVE 2 TO LIN.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB011.
           WRITE REG-RELAT FROM CAB03.
           ADD 5 TO LIN.

       CABECALHO5 SECTION.
           EVALUATE GS-TIPO-REL
              WHEN 1 MOVE "DATA MOVTO"    TO ORDEM1-REL
                     MOVE "DATA MOVTO: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 2 MOVE "DATA VENDA"    TO ORDEM1-REL
                     MOVE "DATA VENDA: "  TO DESC1-ORDEM-REL(1: 12)
                     PERFORM MOVER-DATA-REL
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
              WHEN 3 MOVE "CONTRATO  "    TO ORDEM1-REL
                     MOVE "CONTRATO..: "  TO DESC1-ORDEM-REL(1: 12)
                     MOVE GS-CONTRATO     TO DESC1-ORDEM-REL(13: 4)
                     MOVE "REGIAO..: "    TO DESC-ORDEM-REL(37:10)
                     MOVE GS-DESC-REGIAO  TO DESC-ORDEM-REL(48:10)
           END-EVALUATE.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02C.

           WRITE REG-RELAT FROM CAB03.
           ADD 3 TO LIN.



       MOVER-DATA-REL SECTION.
           MOVE GS-VECTO-INI    TO DATA-E
           MOVE DATA-E          TO DESC1-ORDEM-REL(13: 11)
                                   DESC-ORDEM-REL(13: 11)
           MOVE "a"             TO DESC1-ORDEM-REL(24: 2)
                                   DESC-ORDEM-REL(24: 2)
           MOVE GS-VECTO-FIM    TO DATA-E
           MOVE DATA-E          TO DESC1-ORDEM-REL(26: 10)
                                   DESC-ORDEM-REL(26: 10).

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CEAD010 COD040 CGD001 MTD001 MTD020 RCD100 CAD010
                 MTD023 WORK WORK1 WORK2 WORK3 WORK5 RCD101 CAD012
                 COD001 RCD203
           DELETE FILE WORK.
           DELETE FILE WORK1.
           DELETE FILE WORK2.
           DELETE FILE WORK3.
           DELETE FILE WORK5.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
