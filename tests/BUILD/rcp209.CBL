       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP209.
      *DATA: 26/05/2005
      *AUTOR: ALFREDO SAVIOLLI NETO
      *FUNÇÃO: RELATORIO DE PRODUTOS VENDIDOS
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass"
           AListview          is class "alistview".

       SPECIAL-NAMES.
           decimal-point is comma
           call-convention 2 is dsdll.

      *  DECIMAL-POINT IS COMMA
      *  PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CGPX001.
           COPY CGPX011.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX020C.
           COPY RCPX100.
           COPY RCPX100C.
           COPY ACPX100.
           COPY COBREL.SEL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS
                  VENDEDOR-CH-WK = VENDEDOR-WK
                                   CONTRATO-WK
                  ALTERNATE RECORD KEY IS
                  DTVENDA-CH-WK = DTVENDA-WK
                                  CONTRATO-WK
                  ALTERNATE RECORD KEY IS
                  FORMANDO-WK
                  DUPLICATES.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS VENDEDOR-WK2.


      *    SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CGPW001.
       COPY CGPW011.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW020C.
       COPY RCPW100.
       COPY RCPW100C.
       COPY ACPW100.
       COPY COBREL.FD.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(8).
           05  VENDEDOR-WK         PIC X(30).
           05  FORMANDO-WK         PIC X(25).
           05  DTVENDA-WK.
               10 ANO-VENDA-WK     PIC 9(04).
               10 MES-VENDA-WK     PIC 9(02).
               10 DIA-VENDA-WK     PIC 9(02).
           05  FITA-WK             PIC 9(5).
           05  DVD-WK              PIC 9(5).
           05  POSTER-WK           PIC 9(5).
           05  TIPO-WK             PIC X(2).
           05  FOTO-CD-WK          PIC 9(5).
           05  MOLDURA-WK          pic 9(5).
           05  PFITA-WK            PIC 9(5).
           05  PDVD-WK             PIC 9(5).
           05  CFITA-WK            PIC 9(5).
           05  CDVD-WK             PIC 9(5).
           05  BOOK-WK             PIC 9(5).
           05  PORTA-RETRATO-WK    PIC 9(5).
           05  PENDRIVE-WK         PIC 9(5).
           05  VIDEO-HD-WK         PIC 9(5).
           05  REVISTA-WK          PIC 9(5).
           05  CALENDARIO-WK       PIC 9(5).

       FD  WORK2.
       01  REG-WORK2.
           05  VENDEDOR-WK2        PIC X(30).
           05  FITA-WK2            PIC 9(5).
           05  DVD-WK2             PIC 9(5).
           05  POSTER-WK2          PIC 9(5).
           05  FOTO-CD-WK2         PIC 9(5).
           05  MOLDURA-WK2         pic 9(5).
           05  PFITA-WK2           PIC 9(5).
           05  PDVD-WK2            PIC 9(5).
           05  CFITA-WK2           PIC 9(5).
           05  CDVD-WK2            PIC 9(5).
           05  BOOK-WK2            PIC 9(5).
           05  PORTA-RETRATO-WK2   PIC 9(5).
           05  PENDRIVE-WK2        PIC 9(5).
           05  VIDEO-HD-WK2        PIC 9(5).
           05  REVISTA-WK2         PIC 9(5).
           05  CALENDARIO-WK2      PIC 9(5).

      *FD  RELAT
      *    LABEL RECORD IS OMITTED.
      *01  REG-RELAT.
      *    05  FILLER              PIC X(150).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP209.CPB".
           COPY "RCP209.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD100             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD100C            PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD020C            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  MOVER                 PIC X(01)    VALUE SPACES.
           05  QUAL-CAB              PIC 9(01)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  VENDA-INI             PIC 9(8)     VALUE ZEROS.
           05  VENDA-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.

           05  ALBUM-E               PIC 9999.9999.
           05  QTDE-E                PIC ZZZZ9.
           05  QTDE-E1               PIC ZZZ9.
           05  QTDE1-E               PIC ZZZZ.ZZ9.
           05  ULT-SEQ               PIC 9(8)     VALUE ZEROS.
           05  TOT-FITA              PIC 9(5)     VALUE ZEROS.
           05  TOT-DVD               PIC 9(5)     VALUE ZEROS.
           05  TOT-POSTER            PIC 9(5)     VALUE ZEROS.
           05  TOT-FOTO-CD           PIC 9(5)     VALUE ZEROS.
           05  TOT-MOLDURA           PIC 9(5)     VALUE ZEROS.
           05  TOT-PFITA             PIC 9(5)     VALUE ZEROS.
           05  TOT-PDVD              PIC 9(5)     VALUE ZEROS.
           05  TOT-CFITA             PIC 9(5)     VALUE ZEROS.
           05  TOT-CDVD              PIC 9(5)     VALUE ZEROS.
           05  TOT-BOOK              PIC 9(5)     VALUE ZEROS.
           05  TOT-PORTA-RETRATO     PIC 9(5)     VALUE ZEROS.
           05  TOT-PENDRIVE          PIC 9(5)     VALUE ZEROS.
           05  TOT-VIDEOHD           PIC 9(5)     VALUE ZEROS.
           05  TOT-REVISTA           PIC 9(5)     VALUE ZEROS.
           05  TOT-CALENDARIO        PIC 9(5)     VALUE ZEROS.
           05  TOT-FITA2             PIC 9(5)     VALUE ZEROS.
           05  TOT-DVD2              PIC 9(5)     VALUE ZEROS.
           05  TOT-POSTER2           PIC 9(5)     VALUE ZEROS.
           05  TOT-FOTO-CD2          PIC 9(5)     VALUE ZEROS.
           05  TOT-MOLDURA2          PIC 9(5)     VALUE ZEROS.
           05  TOT-PFITA2            PIC 9(5)     VALUE ZEROS.
           05  TOT-PDVD2             PIC 9(5)     VALUE ZEROS.
           05  TOT-CFITA2            PIC 9(5)     VALUE ZEROS.
           05  TOT-CDVD2             PIC 9(5)     VALUE ZEROS.
           05  TOT-BOOK2             PIC 9(5)     VALUE ZEROS.
           05  TOT-PORTA-RETRATO2    PIC 9(5)     VALUE ZEROS.
           05  TOT-PENDRIVE2         PIC 9(5)     VALUE ZEROS.
           05  TOT-VIDEOHD2          PIC 9(5)     VALUE ZEROS.
           05  TOT-REVISTA2          PIC 9(5)     VALUE ZEROS.
           05  TOT-CALENDARIO2       PIC 9(5)     VALUE ZEROS.
           05  TOTAL-LINHAS          PIC 9(5)     VALUE ZEROS.
           05  DATA-AUX              PIC 9(8).
           05  DATA-AUX-R REDEFINES DATA-AUX.
               10 AUX-ANO            PIC 9(4).
               10 AUX-MES            PIC 9(2).
               10 AUX-DIA            PIC 9(2).
           05  PASSAR-STRING-1       PIC X(40).
           COPY "PARAMETR".


       copy "cobrel.par".
       copy "cobrel.uti".
       copy "cobrel.cpy".
       copy "cobrel-parametros.cpy".

       01 DET-CABECALHO1-p.
          05 FILLER                      PIC X(05).
          05 DET-EMPRESA-p               PIC X(04).
          05 FILLER                      PIC X(05).
          05 DET-RAZAO-p                 PIC X(76).
          05 FILLER                      PIC X(38).
          05 FILLER                      PIC X(07) VALUE "PÁGINA ".
          05 DET-PAGINA-p                PIC 9(06) VALUE ZEROS.

       01 DET-CABECALHO2-p.
          05 FILLER                      PIC X(05).
          05 DET-TITULO-CAB-p            PIC X(108).
          05 FILLER                      PIC X(12) VALUE "EMITIDO EM:".
          05 DET-DIA-EMIS-p              PIC 99/.
          05 DET-MES-EMIS-p              PIC 99/.
          05 DET-ANO-EMIS-p              PIC 9999.
          05 FILLER                      PIC X(01).
          05 DET-HO-EMIS-p               PIC 99.
          05 FILLER                      PIC X(01) VALUE ":".
          05 DET-MI-EMIS-p               PIC 99.

       01 DET-CABECALHO3-p.
          05 FILLER                      PIC X(05).
          05 FILLER                      PIC X(12) VALUE "ORDENAÇÃO.: ".
          05 DET-ORDEM-P                 PIC X(20).
          05 FILLER                      PIC X(76).
          05 FILLER                      PIC X(12) VALUE "USUÁRIO...:".
          05 DET-USUARIO-P               PIC X(05).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.

       01 indice                       pic 9(02).
       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.

       01 lnktabelaPro.
          02 lnkobjetoscolPro          object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaVen.
          02 lnkobjetoscolVen          object reference occurs 99 times.
       01 lnktabelaColVen.
          02 lnkcolunasVen pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnkusu.
          copy usuario.cpy.

       01  CAB01.
           05  EMPRESA-REL         PIC X(96)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(05).
           05  FILLER              PIC X(31)   VALUE
           "RELAT. PROD. VEND. DT.MOVTO..: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MOVTO-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(03).
           05  FILLER              PIC X(10) VALUE "DT.VENDA: ".
           05  VENDA-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENDA-FIM-REL       PIC 99/99/9999.
           05  FILLER              PIC X(03).
           05  FILLER              PIC X(07) VALUE "CONT.: ".
           05  CONTRATO-REL        PIC 9(04) VALUE ZEROS.
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(05) VALUE "UF.: ".
           05  UF-REL              PIC X(02).
           05  FILLER              PIC X(01).
           05  CONSIDERA-REL       PIC X(23) VALUE SPACES.
       01  CAB03.
           05  FILLER              PIC X(130) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(150)  VALUE
           "CONTRATO NOME FORMANDO         DT.VENDA  VENDEDOR    FITAS D
      -    "VD POST SGL FTCD MOLD P.FITA P.DVD C.FITA C.DVD BOOK DATA EN
      -    "TREGA".

       01  CAB05.
           05  FILLER              PIC X(150)  VALUE
           "VENDEDOR                       FITAS   DVD  POSTER  FOTOCD
      -    "MOLDURA  P.FITA  P.DVD C.FITA C.DVD BOOK".


       01 cab-01.
          05 filler                pic x(08)
             value "CONTRATO".
          05 filler                pic x(01).
          05 filler                pic x(19)
             value "NOME FORMANDO".
          05 filler                pic x(01).
          05 filler                pic x(10)
             value " DT.VENDA".
          05 filler                pic x(01).
          05 filler                pic x(20)
             value "VENDEDOR".
          05 filler                pic x(01).
          05 filler                pic x(05)
             value "FITAS".
          05 filler                pic x(01).
          05 filler                pic x(05)
             value "  DVD".
          05 filler                pic x(01).
          05 filler                pic x(05)
             value " POST".
          05 filler                pic x(01).
          05 filler                pic x(02)
             value "TP".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "FTCD".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "MOLD".
          05 filler                pic x(01).
          05 filler                pic x(05)
             value "PFITA".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "PDVD".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "CFIT".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "CDVD".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "BOOK".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "PRET".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "PDRI".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "VIHD".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "REVI".
          05 filler                pic x(01).
          05 filler                pic x(04)
             value "CALE".
          05 filler                pic x(01).
          05 filler                pic x(13)
             value "DATA ENTREGA".

       01 det-01.
          05 det-contrato          pic 9(08) BLANK WHEN ZEROS.
          05 filler                pic x(01).
          05 det-formando          pic x(19).
          05 filler                pic x(01).
          05 det-dtvenda           pic x(10).
          05 filler                pic x(01).
          05 det-vendedor          pic x(20).
          05 filler                pic x(01).
          05 det-fita              pic zzzz9.
          05 filler                pic x(01).
          05 det-dvd               pic zzzz9.
          05 filler                pic x(01).
          05 det-poster            pic zzzz9.
          05 filler                pic x(01).
          05 det-tipo              pic x(02).
          05 filler                pic x(01).
          05 det-fotocd            pic zzz9.
          05 filler                pic x(01).
          05 det-moldura           pic zzz9.
          05 filler                pic x(01).
          05 det-pfita             pic zzzz9.
          05 filler                pic x(01).
          05 det-pdvd              pic zzz9.
          05 filler                pic x(01).
          05 det-cfita             pic zzz9.
          05 filler                pic x(01).
          05 det-cdvd              pic zzz9.
          05 filler                pic x(01).
          05 det-book              pic zzz9.
          05 filler                pic x(01).
          05 det-porta-retrato     pic zzz9.
          05 filler                pic x(01).
          05 det-pendrive          pic zzz9.
          05 filler                pic x(01).
          05 det-videohd           pic zzz9.
          05 filler                pic x(01).
          05 det-revista           pic zzz9.
          05 filler                pic x(01).
          05 det-calendario        pic zzz9.
          05 filler                pic x(01).
          05 det-data              pic x(13).

       01 det-sep1                 pic x(156)
          value all "-".

       01 cab-02.
          05 filler                pic x(31)
             value "VENDEDOR".
          05 filler                pic x(06)
             value "FITAS".
          05 filler                pic x(06)
             value "  DVD".
          05 filler                pic x(06)
             value "POSTE".
          05 filler                pic x(06)
             value " FTCD".
          05 filler                pic x(06)
             value "MOLDU".
          05 filler                pic x(06)
             value "P.FIT".
          05 filler                pic x(06)
             value "P.DVD".
          05 filler                pic x(06)
             value "C.FIT".
          05 filler                pic x(06)
             value "C.DVD".
          05 filler                pic x(06)
             value " BOOK".
          05 filler                pic x(06)
             value "P.RET".
          05 filler                pic x(06)
             value " PDRI".
          05 filler                pic x(06)
             value " VIHD".
          05 filler                pic x(06)
             value " REVI".
          05 filler                pic x(05)
             value " CALE".

       01 det-02.
          05 DET-VENDEDOR2         PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-FITA2             PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-DVD2              PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-POSTER2           PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-FOTO-CD2          PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-MOLDURA2          PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-PFITA2            PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-PDVD2             PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-CFITA2            PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-CDVD2             PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-BOOK2             PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-PORT-RET2         PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-PENDRIVE2         PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-VIDEOHD2          PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-REVISTA2          PIC ZZZZ9.
          05 FILLER                PIC X(01).
          05 DET-CALENDARIO2       PIC ZZZZ9.

       01 DET-SEP2                 PIC X(120)
          VALUE ALL "-".

       01  LINDET.
           05  LINDET-REL          PIC X(150) VALUE SPACES.

       01  STRING-1               PIC X(65) VALUE SPACES.
       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

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
           MOVE GS-VERSION-NO         TO DS-VERSION-NO
           MOVE EMPRESA-W             TO EMP-REC
           MOVE NOME-EMPRESA-W        TO EMPRESA-REL
           move "\PROGRAMA\KELLO\*"   to lnk-path-sis
           move empresa-w             to lnk-empresa
           move USUARIO-W             to lnk-usuario
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD020C" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020C.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD100C" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100C.
           MOVE "ACD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-ACD100.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK   CLOSE WORK   OPEN I-O WORK.
           COMPUTE VARIA-W2 = VARIA-W + 10
           OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2.

           OPEN I-O   ACD100 RCD100C MTD020C
           CLOSE      ACD100 RCD100C MTD020C
           OPEN INPUT ACD100
           IF ST-ACD100 <> "00"
              MOVE "ERRO ABERTURA ACD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-ACD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CAD010
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT RCD100
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT RCD100C
           IF ST-RCD100C <> "00"
              MOVE "ERRO ABERTURA RCD100C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100C TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CGD001
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CGD011
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT MTD019
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT MTD020
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT MTD020C
           IF ST-MTD020C <> "00"
              MOVE "ERRO ABERTURA MTD020C: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020C TO GS-MENSAGEM-ERRO(23: 02)
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
                   PERFORM CRIAR-LISTVIEW
               WHEN GS-PRINTER-FLG-TRUE
      *             COPY IMPRESSORA.CHAMA.
      *             IF LNK-MAPEAMENTO <> SPACES
      *                PERFORM IMPRIME-RELATORIO
      *             END-IF

                    IF GS-IMPR-RELATORIO = 1
                       MOVE 1 TO QUAL-CAB
                    ELSE
                       IF GS-IMPR-RESUMO = 1
                          MOVE 2 TO QUAL-CAB
                       END-IF
                    END-IF

                    perform 0010-inicio
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTA2
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
                    PERFORM CARREGA-LISTA2
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LER-VENDEDOR
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM POPUP-VENDEDOR
               WHEN GS-GERAR-KAC-TRUE
                    PERFORM GERAR-KAC
               WHEN GS-TRATAR-EVENTO-TRUE
                    EVALUATE GS-QUAL-LIST
                        WHEN 1 PERFORM EVENTO-PRODUTOS
                        WHEN 2 PERFORM EVENTO-VENDEDOR
                    END-EVALUATE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CRIAR-LISTVIEW SECTION.
           PERFORM CRIAR-LISTVIEW-PRO
           PERFORM CRIAR-LISTVIEW-VEN.
       CRIAR-LISTVIEW-FIM.
           EXIT.

       CRIAR-LISTVIEW-PRO SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Contrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Nome Formando" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Dt Venda" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Vendedor" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Fitas" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"DVD" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Poster" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"SGL" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Ft CD" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Mold" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"P.Fita" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"P.DVD" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"C.Fita" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"C.DVD" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Book" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"P.Retrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Pendrive" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"VideoHD" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Revista" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Calend" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview-produtos "gridLines"
          invoke gs-listview-produtos "noBorder".
       CRIAR-LISTVIEW-PRO-FIM.
           EXIT.

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-rcp209-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-rcp209-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       zebrar-itensPro section.
           move "listview-rcp209-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos wsTexto
           invoke gs-listview-produtos "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-rcp209-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.

       CRIAR-LISTVIEW-VEN SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Vendedor" returning lnkobjetoscolVen(indice)
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Fitas" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"DVD" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Poster" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"SGL" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Ft CD" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Mold" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"P.Fita" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"P.DVD" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"C.Fita" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"C.DVD" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Book" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"P.Retrato" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Pendrive" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"VideoHD" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Revista" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-vendedor "adicionarColunaZ"
            using z"Calend" returning lnkobjetoscolVen(indice)
          invoke lnkobjetoscolVen(indice) "RightJustified"
          move indice to lnkcolunasVen(indice)

          perform mostrar-fonte-favoVen
          perform mostrar-colunas-favoVen

          invoke gs-listview-vendedor "gridLines"
          invoke gs-listview-vendedor "noBorder".
       CRIAR-LISTVIEW-VEN-FIM.
           EXIT.

       mostrar-colunas-favoVen section.
          initialize wsTexto
          move "listview-rcp209-vendedor" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-vendedor
                                  wsTexto
                                  lnktabelaVen.
       mostrar-colunas-favoVen-fim.
           exit.

       mostrar-fonte-favoVen section.
           move "listview-rcp209-vendedor" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-vendedor wsTexto.
       mostrar-fonte-favoVen-fim.
           exit.

       zebrar-itensVen section.
           move "listview-rcp209-vendedor" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-vendedor wsTexto
           invoke gs-listview-vendedor "redrawallitems".
       zebrar-itensVen-fim.
           exit.

       chamar-colunas-favoVen section.
           move "listview-rcp209-vendedor" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-vendedor
                               wsTexto
                               lnktabelaVen

           perform mostrar-colunas-favoVen
           perform mostrar-fonte-favoVen
           perform zebrar-itensVen.
       chamar-colunas-favoVen-fim.
           exit.

       EVENTO-PRODUTOS SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favopro
               when 34027  set-focus ef1
           end-evaluate.
       EVENTO-PRODUTOS-FIM.
           EXIT.

       EVENTO-VENDEDOR SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favoven
               when 34027  set-focus ef1
           end-evaluate.
       EVENTO-VENDEDOR-FIM.
           EXIT.

       GERAR-KAC SECTION.
            MOVE "Você deseja Realmente Transferir os produtos para o KA
      -    "C?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG EQUAL "S"
               CLOSE       RCD100 ACD100
               OPEN I-O    RCD100 ACD100
               MOVE 0                  TO GS-LINHA
               PERFORM UNTIL GS-LINHA = TOTAL-LINHAS
                   ADD  1              TO GS-LINHA
                   MOVE "VER-MARCACAO" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   IF GS-SITUACAO = 1
                      MOVE GS-LINDET(1:8) TO ALBUM-REC
                      READ RCD100 INVALID KEY
                           MOVE "Álbum Não Encontrado" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           PERFORM GRAVAR-KAC
                      END-READ
                   END-IF
                END-PERFORM
                CLOSE      RCD100 ACD100
                OPEN INPUT RCD100 ACD100.

       GRAVAR-KAC SECTION.
           MOVE ALL "9" TO NR-ATENDIMENTO-AC100
           START ACD100 KEY IS LESS THAN CHAVE-AC100
           READ ACD100 NEXT RECORD
               IF ST-ACD100 = "00" OR "9d"
                  MOVE NR-ATENDIMENTO-AC100 TO ULT-SEQ
               ELSE
                  MOVE ZEROS TO ULT-SEQ.

           ADD 1 TO ULT-SEQ
           INITIALIZE REG-ACD100
           MOVE ULT-SEQ            TO NR-ATENDIMENTO-AC100
           MOVE USUARIO-W          TO ULTIMO-ATENDENTE-AC100
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU        TO DATA-ABERTURA-AC100
           accept WS-HORA-SYS from time
           STRING WS-HO-SYS ":" WS-MI-SYS ":" WS-SE-SYS
                                                INTO HORA-ABERTURA-AC100
           MOVE "1-Aberto"         TO STATUS-ATUAL-AC100
           MOVE ALBUM-REC          TO CLIENTE-AC100
           MOVE 1                  TO ATENDIMENTO1-AC100

           WRITE REG-ACD100 INVALID KEY
               MOVE "Erro de Gravação....ACD100" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE "S"                  TO GERADO-KAC
               MOVE NR-ATENDIMENTO-AC100 TO KAC-ATENDIMENTO
               REWRITE REG-RCD100 INVALID KEY
                   MOVE "Erro de Regravação...RCD100(KAC)" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE.

       LER-VENDEDOR SECTION.
           MOVE GS-VENDEDOR TO CODIGO-CG01
           READ CGD001 INVALID KEY
               MOVE "----" TO GS-NOME-VENDEDOR
           NOT INVALID KEY
               MOVE NOME-CG01 TO GS-NOME-VENDEDOR.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W STRING-1
           CANCEL "CGP001T"

           MOVE STRING-1(33: 6) TO GS-VENDEDOR.
           PERFORM LER-VENDEDOR.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *--------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           CLOSE WORK2  OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-MOVTO-INI TO DATA-INV MOVTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO MOVTO-INI
           MOVE GS-MOVTO-FIM TO DATA-INV MOVTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO MOVTO-FIM

           MOVE GS-VENDA-INI TO DATA-INV VENDA-INI-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO VENDA-INI
           MOVE GS-VENDA-FIM TO DATA-INV VENDA-FIM-REL
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO VENDA-FIM.

           MOVE GS-CONTRATO  TO CONTRATO-REL
           MOVE GS-UF        TO UF-REL

           EVALUATE GS-CONSIDERAR
               WHEN 1     MOVE "CONSIDERA PLANILHA"   TO CONSIDERA-REL
               WHEN OTHER MOVE "N CONSIDERA PLANILHA" TO CONSIDERA-REL.


           INITIALIZE REG-RCD100

           IF GS-CONTRATO > 0
              STRING GS-CONTRATO "0000"  INTO ALBUM-REC
              START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
                    MOVE "10" TO ST-RCD100
              END-START
              PERFORM UNTIL ST-RCD100 = "10"
                   READ RCD100 NEXT AT END
                       MOVE "10" TO ST-RCD100
                   NOT AT END
                       IF GS-CONTRATO <> ALBUM-REC(1:4)
                          MOVE "10" TO ST-RCD100
                       ELSE
                          MOVE 0               TO CLASSIF-CG11
                          MOVE ALBUM-REC       TO CODIGO-CG11
                          READ CGD011 INVALID KEY
                               MOVE 0          TO CIDADE1-CG11
                          END-READ
                          MOVE CIDADE1-CG11    TO CIDADE
                          READ CAD010 INVALID KEY
                               MOVE SPACES     TO UF-CID
                          END-READ
                          IF GS-VENDEDOR = 0             OR
                             GS-VENDEDOR = VENDEDOR-REC
                             IF GS-MOVTO-INI = 0         OR
                                (DATA-MOVTO-REC NOT < MOVTO-INI AND
                                 DATA-MOVTO-REC NOT > MOVTO-FIM)
                                IF GS-VENDA-INI = 0      OR
                                   (DATAVEN-REC NOT < VENDA-INI AND
                                    DATAVEN-REC NOT > VENDA-FIM)
                                    IF GS-UF = SPACES OR GS-UF = UF-CID
                                       MOVE ALBUM-REC TO GS-EXIBE-MOVTO
                                       MOVE "TELA-AGUARDA1" TO
                                            DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       PERFORM MOVER-DADOS
                                    END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-MOVTO-INI > 0
                 MOVE MOVTO-INI           TO DATA-MOVTO-REC
                 MOVE ZEROS               TO ALBUM-REC
                 START RCD100 KEY IS NOT < ALT-REC INVALID KEY
                   MOVE "10" TO ST-RCD100
                 END-START
                 PERFORM UNTIL ST-RCD100 = "10"
                   READ RCD100 NEXT AT END
                       MOVE "10" TO ST-RCD100
                   NOT AT END
                       IF DATA-MOVTO-REC > MOVTO-FIM
                          MOVE "10" TO ST-RCD100
                       ELSE
                          MOVE 0               TO CLASSIF-CG11
                          MOVE ALBUM-REC       TO CODIGO-CG11
                          READ CGD011 INVALID KEY
                               MOVE 0          TO CIDADE1-CG11
                          END-READ
                          MOVE CIDADE1-CG11    TO CIDADE
                          READ CAD010 INVALID KEY
                               MOVE SPACES     TO UF-CID
                          END-READ
                          IF GS-CONTRATO = 0 OR
                             GS-CONTRATO = ALBUM-REC(1:4)
                             IF GS-VENDEDOR = 0 OR
                                GS-VENDEDOR = VENDEDOR-REC
                                IF VENDA-INI = 0 OR
                                   (DATAVEN-REC NOT < VENDA-INI AND
                                    DATAVEN-REC NOT > VENDA-FIM)
                                    IF GS-UF = SPACES OR GS-UF = UF-CID
                                       MOVE DATA-MOVTO-REC TO
                                                          GS-EXIBE-MOVTO
                                       MOVE "TELA-AGUARDA1" TO
                                                          DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       PERFORM MOVER-DADOS
                                    END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-READ
                 END-PERFORM
              ELSE
                 MOVE VENDA-INI           TO DATAVEN-REC
                 START RCD100 KEY IS NOT < DATAVEN-REC INVALID KEY
                   MOVE "10" TO ST-RCD100
                 END-START
                 PERFORM UNTIL ST-RCD100 = "10"
                   READ RCD100 NEXT AT END
                       MOVE "10" TO ST-RCD100
                   NOT AT END
                       IF DATAVEN-REC > VENDA-FIM
                           MOVE "10" TO ST-RCD100
                       ELSE
                          MOVE 0               TO CLASSIF-CG11
                          MOVE ALBUM-REC       TO CODIGO-CG11
                          READ CGD011 INVALID KEY
                               MOVE 0          TO CIDADE1-CG11
                          END-READ
                          MOVE CIDADE1-CG11    TO CIDADE
                          READ CAD010 INVALID KEY
                               MOVE SPACES     TO UF-CID
                          END-READ
                          IF GS-CONTRATO = 0 OR
                             GS-CONTRATO = ALBUM-REC(1:4)
                             IF GS-VENDEDOR = 0 OR
                                GS-VENDEDOR = VENDEDOR-REC
                                IF MOVTO-INI = 0 OR
                                   (DATA-MOVTO-REC NOT < MOVTO-INI AND
                                    DATA-MOVTO-REC NOT > MOVTO-FIM)
                                    IF GS-UF = SPACES OR GS-UF = UF-CID
                                       MOVE DATAVEN-REC TO
                                                          GS-EXIBE-MOVTO
                                       MOVE "TELA-AGUARDA1" TO
                                                            DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                       PERFORM MOVER-DADOS
                                    END-IF
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                   END-READ
                 END-PERFORM
              END-IF
           END-IF

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS SECTION.
           MOVE "S" TO MOVER

           MOVE ALBUM-REC TO ALBUM-REC-C
           READ RCD100C INVALID KEY
                INITIALIZE REG-RCD100C
           END-READ

      *         DISPLAY ERASE AT 0101
      *         DISPLAY "ALBUM-REC             = "  AT 0101
      *         DISPLAY  ALBUM-REC                  AT 0130
      *         DISPLAY "QFITAS-REC            = "  AT 0201
      *         DISPLAY  QFITAS-REC                 AT 0230
      *         DISPLAY "QBOOK-REC             = "  AT 0301
      *         DISPLAY  QBOOK-REC                  AT 0330
      *         DISPLAY "QPORTA-RETRATO-REC-C  = "  AT 0401
      *         DISPLAY  QPORTA-RETRATO-REC-C       AT 0430
      *         DISPLAY "QPENDRIVE-REC-C       = "  AT 0501
      *         DISPLAY  QPENDRIVE-REC-C            AT 0530
      *         DISPLAY "QVIDEO-HD-REC-C       = "  AT 0601
      *         DISPLAY  QVIDEO-HD-REC-C            AT 0630
      *         DISPLAY "QREVISTA-REC-C        = "  AT 0701
      *         DISPLAY  QREVISTA-REC-C             AT 0730
      *         DISPLAY "QCALENDARIO-REC-C     = "  AT 0801
      *         DISPLAY  QCALENDARIO-REC-C          AT 0830
      *
      *
      *         STOP " NA LEITURA"


           IF GS-SO-FITA = 1 AND QFITAS-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-DVD = 1 AND QDVD-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-POSTER = 1 AND QPOSTER-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-FOTOCD = 1 AND QFOTO-CD-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-MOLDURA = 1 AND QMOLDURA-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-PORTA-DVD = 1 AND QPORTA-DVD-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-PORTA-FITA = 1 AND QPFITA-REC = 0
              MOVE "N" TO MOVER.

           IF GS-SO-PORTA-RETRATO = 1 AND QPORTA-RETRATO-REC-C = 0
              MOVE "N" TO MOVER.

           IF GS-SO-PENDRIVE = 1 AND QPENDRIVE-REC-C = 0
              MOVE "N" TO MOVER.

           IF GS-SO-VIDEO-HD = 1 AND QVIDEO-HD-REC-C = 0
              MOVE "N" TO MOVER.

           IF GS-SO-REVISTA = 1 AND  QREVISTA-REC-C = 0
              MOVE "N" TO MOVER.

           IF GS-SO-CALENDARIO = 1 AND QCALENDARIO-REC-C = 0
              MOVE "N" TO MOVER.

           IF GS-PEDIDO-EXTRA = 0
              IF QFITAS-REC               = 0 AND QDVD-REC        = 0
                 AND QPOSTER-REC          = 0 AND QFOTO-CD-REC    = 0
                 AND QPFITA-REC           = 0 AND QMOLDURA-REC    = 0
                 AND QPORTA-DVD-REC       = 0 AND QBOOK-REC       = 0
                 AND QPORTA-RETRATO-REC-C = 0 AND QPENDRIVE-REC-C = 0
                 AND QVIDEO-HD-REC-C      = 0 AND QREVISTA-REC-C  = 0
                 AND QCALENDARIO-REC-C    = 0
                 MOVE "N" TO MOVER.

           IF PEDIDO-EXTRA-REC IS NOT NUMERIC
              MOVE 0 TO PEDIDO-EXTRA-REC
           END-IF

           IF GS-PEDIDO-EXTRA = 1
              IF PED-EXT-BOOK-C      = 0 AND
                 PED-EXT-PORTA-RET-C = 0 AND
                 PED-EXT-PENDRIVE-C  = 0 AND
                 PED-EXT-VIDEO-HD-C  = 0 AND
                 PED-EXT-REVISTA-C   = 0 AND
                 PED-EXT-CALEND-C    = 0 AND
                 PEDIDO-EXTRA-REC    = 0
                 MOVE "N" TO MOVER
              END-IF
              IF PED-EXT-BOOK-C = 0
                 MOVE 0              TO QBOOK-REC
              END-IF
              IF PED-EXT-PORTA-RET-C = 0
                 MOVE 0              TO QPORTA-RETRATO-REC-C
              END-IF
              IF PED-EXT-PENDRIVE-C = 0
                 MOVE 0              TO QPENDRIVE-REC-C
              END-IF
              IF PED-EXT-VIDEO-HD-C = 0
                 MOVE 0              TO QVIDEO-HD-REC-C
              END-IF
              IF PED-EXT-REVISTA-C = 0
                 MOVE 0              TO QREVISTA-REC-C
              END-IF
              IF PED-EXT-CALEND-C = 0
                 MOVE 0              TO QCALENDARIO-REC-C
              END-IF


           END-IF

           IF GS-SO-BOOK = 1 AND QBOOK-REC = 0
              MOVE "N" TO MOVER.

           IF GS-CONSIDERAR = 1
              MOVE ALBUM-REC TO ALBUM-MTG
              READ MTD020 NOT INVALID KEY
                   MOVE ALBUM-MTG TO ALBUM-MTG-C
                   READ MTD020C INVALID KEY
                        INITIALIZE REG-MTD020C
                   END-READ
                   IF GS-SO-FITA = 1 AND QT-FITAS-MTG NOT < QFITAS-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-DVD  = 1 AND QT-DVD-MTG NOT < QDVD-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-POSTER = 1 AND QT-POSTER-MTG NOT <
                      QPOSTER-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-FOTOCD = 1 AND QT-FOTO-CD-MTG NOT <
                      QFOTO-CD-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-MOLDURA = 1 AND QT-MOLDURA-MTG NOT <
                      QMOLDURA-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-PORTA-DVD = 1 AND QT-PORTA-DVD-MTG NOT <
                      QPORTA-DVD-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-PORTA-FITA = 1 AND QT-PORTA-FITA-MTG NOT <
                      QPFITA-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-BOOK = 1 AND QT-BOOK-MTG NOT < QBOOK-REC
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-PORTA-RETRATO = 1 AND
                      QT-PORTA-RETRATO-MTG-C NOT < QPORTA-RETRATO-REC-C
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-PENDRIVE = 1 AND
                      QT-PENDRIVE-MTG-C NOT < QPENDRIVE-REC-C
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-VIDEO-HD = 1 AND
                      QT-VIDEO-HD-MTG-C NOT < QVIDEO-HD-REC-C
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-REVISTA = 1 AND
                      QT-REVISTA-MTG-C NOT <  QREVISTA-REC-C
                      MOVE "N" TO MOVER
                   END-IF
                   IF GS-SO-CALENDARIO = 1 AND
                      QT-CALENDARIO-MTG-C NOT < QCALENDARIO-REC-C
                      MOVE "N" TO MOVER
                   END-IF


                   IF QFITAS-REC NOT < QT-FITAS-MTG
                      COMPUTE QFITAS-REC      = QFITAS-REC      -
                                                QT-FITAS-MTG
                   ELSE
                      MOVE ZEROS              TO QFITAS-REC
                   END-IF
                   IF QDVD-REC NOT < QT-DVD-MTG
                      COMPUTE QDVD-REC        = QDVD-REC        -
                                                QT-DVD-MTG
                   ELSE
                      MOVE ZEROS              TO QDVD-REC
                   END-IF
                   IF QPOSTER-REC NOT < QT-POSTER-MTG
                      COMPUTE QPOSTER-REC     = QPOSTER-REC     -
                                                QT-POSTER-MTG
                   ELSE
                      MOVE ZEROS              TO QPOSTER-REC
                   END-IF
                   IF QFOTO-CD-REC NOT < QT-FOTO-CD-MTG
                      COMPUTE QFOTO-CD-REC    = QFOTO-CD-REC    -
                                                QT-FOTO-CD-MTG
                   ELSE
                      MOVE ZEROS              TO QFOTO-CD-REC
                   END-IF
                   IF QMOLDURA-REC NOT < QT-MOLDURA-MTG
                      COMPUTE QMOLDURA-REC    = QMOLDURA-REC    -
                                                QT-MOLDURA-MTG
                   ELSE
                      MOVE ZEROS              TO QMOLDURA-REC
                   END-IF
                   IF QPFITA-REC NOT < QT-PORTA-FITA-MTG
                      COMPUTE QPFITA-REC      = QPFITA-REC      -
                                                QT-PORTA-FITA-MTG
                   ELSE
                      MOVE ZEROS              TO QPFITA-REC
                   END-IF
                   IF QPORTA-DVD-REC NOT < QT-PORTA-DVD-MTG
                      COMPUTE QPORTA-DVD-REC  = QPORTA-DVD-REC  -
                                                QT-PORTA-DVD-MTG
                   ELSE
                      MOVE ZEROS              TO QPORTA-DVD-REC
                   END-IF

                   IF QBOOK-REC NOT < QT-BOOK-MTG
                      COMPUTE QBOOK-REC = QBOOK-REC - QT-BOOK-MTG
                   ELSE
                      MOVE ZEROS TO QBOOK-REC
                   END-IF

                   IF QPORTA-RETRATO-REC-C NOT < QT-PORTA-RETRATO-MTG-C
                      COMPUTE QPORTA-RETRATO-REC-C =
                              QPORTA-RETRATO-REC-C -
                              QT-PORTA-RETRATO-MTG-C
                   ELSE
                      MOVE ZEROS TO QPORTA-RETRATO-REC-C
                   END-IF

                   IF QPENDRIVE-REC-C NOT < QT-PENDRIVE-MTG-C
                      COMPUTE QPENDRIVE-REC-C =
                              QPENDRIVE-REC-C -
                              QT-PENDRIVE-MTG-C
                   ELSE
                      MOVE ZEROS TO QPENDRIVE-REC-C
                   END-IF

                   IF QVIDEO-HD-REC-C NOT < QT-VIDEO-HD-MTG-C
                      COMPUTE QVIDEO-HD-REC-C =
                              QVIDEO-HD-REC-C -
                              QT-VIDEO-HD-MTG-C
                   ELSE
                      MOVE ZEROS TO QVIDEO-HD-REC-C
                   END-IF

                   IF QREVISTA-REC-C NOT < QT-REVISTA-MTG-C
                      COMPUTE QREVISTA-REC-C =
                              QREVISTA-REC-C -
                              QT-REVISTA-MTG-C
                   ELSE
                      MOVE ZEROS TO QREVISTA-REC-C
                   END-IF

                   IF QCALENDARIO-REC-C NOT < QT-CALENDARIO-MTG-C
                      COMPUTE QCALENDARIO-REC-C =
                              QCALENDARIO-REC-C -
                              QT-CALENDARIO-MTG-C
                   ELSE
                      MOVE ZEROS TO QCALENDARIO-REC-C
                   END-IF

              END-READ
           END-IF

      *    IF PEDIDO-EXTRA-REC IS NOT NUMERIC
      *       MOVE 0 TO PEDIDO-EXTRA-REC
      *    END-IF
      *
      *    IF GS-PEDIDO-EXTRA = 1 AND GS-PEDIDO-EXTRA <>
      *       PEDIDO-EXTRA-REC
      *       MOVE "N" TO MOVER.

           IF MOVER = "S"
              PERFORM MOVER-REALMENTE.

       MOVER-REALMENTE SECTION.
           MOVE ALBUM-REC                    TO CONTRATO-WK
           READ WORK INVALID KEY
                MOVE VENDEDOR-REC             TO CODIGO-CG01
                READ CGD001 INVALID KEY
                    MOVE "--------"           TO VENDEDOR-WK
                                                 VENDEDOR-WK2
                NOT INVALID KEY
                    MOVE NOME-CG01            TO VENDEDOR-WK
                                                 VENDEDOR-WK2
                END-READ
                MOVE ALBUM-REC                TO ALBUM-MT19
                READ MTD019 INVALID KEY
                    MOVE "--------"           TO FORMANDO-WK
                NOT INVALID KEY
                    MOVE NOME-FORM-MT19       TO FORMANDO-WK
                END-READ
      *         DISPLAY ERASE AT 0101
      *         DISPLAY "ALBUM-REC             = "  AT 0101
      *         DISPLAY  ALBUM-REC                  AT 0130
      *         DISPLAY "QFITAS-REC            = "  AT 0201
      *         DISPLAY  QFITAS-REC                 AT 0230
      *         DISPLAY "QBOOK-REC             = "  AT 0301
      *         DISPLAY  QBOOK-REC                  AT 0330
      *         DISPLAY "QPORTA-RETRATO-REC-C  = "  AT 0401
      *         DISPLAY  QPORTA-RETRATO-REC-C       AT 0430
      *         DISPLAY "QPENDRIVE-REC-C       = "  AT 0501
      *         DISPLAY  QPENDRIVE-REC-C            AT 0530
      *         DISPLAY "QVIDEO-HD-REC-C       = "  AT 0601
      *         DISPLAY  QVIDEO-HD-REC-C            AT 0630
      *         DISPLAY "QREVISTA-REC-C        = "  AT 0701
      *         DISPLAY  QREVISTA-REC-C             AT 0730
      *         DISPLAY "QCALENDARIO-REC-C     = "  AT 0801
      *         DISPLAY  QCALENDARIO-REC-C          AT 0830
      *
      *
      *         STOP " "


                READ WORK2 INVALID KEY
                    MOVE QFITAS-REC           TO FITA-WK2
                    MOVE QDVD-REC             TO DVD-WK2
                    MOVE QPOSTER-REC          TO POSTER-WK2
                    MOVE QFOTO-CD-REC         TO FOTO-CD-WK2
                    MOVE QMOLDURA-REC         TO MOLDURA-WK2
                    MOVE QPFITA-REC           TO PFITA-WK2
                    MOVE QPORTA-DVD-REC       TO PDVD-WK2
                    MOVE QCOBERTURA-REC       TO CFITA-WK2
                    MOVE QT-COBERTURA-DVD-REC TO CDVD-WK2
                    MOVE QBOOK-REC            TO BOOK-WK2
                    MOVE QPORTA-RETRATO-REC-C TO PORTA-RETRATO-WK2
                    MOVE QPENDRIVE-REC-C      TO PENDRIVE-WK2
                    MOVE QVIDEO-HD-REC-C      TO VIDEO-HD-WK2
                    MOVE QREVISTA-REC-C       TO REVISTA-WK2
                    MOVE QCALENDARIO-REC-C    TO CALENDARIO-WK2
                    WRITE REG-WORK2
                    END-WRITE
                NOT INVALID KEY
                    ADD  QFITAS-REC           TO FITA-WK2
                    ADD  QDVD-REC             TO DVD-WK2
                    ADD  QPOSTER-REC          TO POSTER-WK2
                    ADD  QFOTO-CD-REC         TO FOTO-CD-WK2
                    ADD  QMOLDURA-REC         TO MOLDURA-WK2
                    ADD  QPFITA-REC           TO PFITA-WK2
                    ADD  QPORTA-DVD-REC       TO PDVD-WK2
                    ADD  QCOBERTURA-REC       TO CFITA-WK2
                    ADD  QT-COBERTURA-DVD-REC TO CDVD-WK2
                    ADD  QBOOK-REC            TO BOOK-WK2
                    ADD  QPORTA-RETRATO-REC-C TO PORTA-RETRATO-WK2
                    ADD  QPENDRIVE-REC-C      TO PENDRIVE-WK2
                    ADD  QVIDEO-HD-REC-C      TO VIDEO-HD-WK2
                    ADD  QREVISTA-REC-C       TO REVISTA-WK2
                    ADD  QCALENDARIO-REC-C    TO CALENDARIO-WK2
                    REWRITE REG-WORK2
                    END-REWRITE
                END-READ
                MOVE DATAVEN-REC              TO DATA-AUX
                MOVE AUX-ANO                  TO ANO-VENDA-WK
                MOVE AUX-MES                  TO MES-VENDA-WK
                MOVE AUX-DIA                  TO DIA-VENDA-WK
                MOVE QFITAS-REC               TO FITA-WK
                MOVE QDVD-REC                 TO DVD-WK
                MOVE QPOSTER-REC              TO POSTER-WK
                MOVE QFOTO-CD-REC             TO FOTO-CD-WK
                MOVE QMOLDURA-REC             TO MOLDURA-WK
                MOVE QPFITA-REC               TO PFITA-WK
                MOVE QPORTA-DVD-REC           TO PDVD-WK
                MOVE QCOBERTURA-REC           TO CFITA-WK
                MOVE QT-COBERTURA-DVD-REC     TO CDVD-WK
                MOVE QBOOK-REC                TO BOOK-WK
                MOVE QPORTA-RETRATO-REC-C     TO PORTA-RETRATO-WK
                MOVE QPENDRIVE-REC-C          TO PENDRIVE-WK
                MOVE QVIDEO-HD-REC-C          TO VIDEO-HD-WK
                MOVE QREVISTA-REC-C           TO REVISTA-WK
                MOVE QCALENDARIO-REC-C        TO CALENDARIO-WK
                MOVE SPACES                   TO TIPO-WK
                IF POSTER-ABERTURA-REC = 1
                    MOVE "AB" TO TIPO-WK
                END-IF
                IF POSTER-XEROX-REC = 1
                    MOVE "XE" TO TIPO-WK
                END-IF
                WRITE REG-WORK
                END-WRITE
             NOT INVALID KEY
                MOVE VENDEDOR-WK              TO VENDEDOR-WK2
                READ WORK2 INVALID KEY
                    MOVE QFITAS-REC           TO FITA-WK2
                    MOVE QDVD-REC             TO DVD-WK2
                    MOVE QPOSTER-REC          TO POSTER-WK2
                    MOVE QFOTO-CD-REC         TO FOTO-CD-WK2
                    MOVE QMOLDURA-REC         TO MOLDURA-WK2
                    MOVE QPFITA-REC           TO PFITA-WK2
                    MOVE QPORTA-DVD-REC       TO PDVD-WK2
                    MOVE QCOBERTURA-REC       TO CFITA-WK2
                    MOVE QT-COBERTURA-DVD-REC TO CDVD-WK2
                    MOVE QBOOK-REC            TO BOOK-WK2
                    MOVE QPORTA-RETRATO-REC-C TO PORTA-RETRATO-WK2
                    MOVE QPENDRIVE-REC-C      TO PENDRIVE-WK2
                    MOVE QVIDEO-HD-REC-C      TO VIDEO-HD-WK2
                    MOVE QREVISTA-REC-C       TO REVISTA-WK2
                    MOVE QCALENDARIO-REC-C    TO CALENDARIO-WK2
                    WRITE REG-WORK2
                    END-WRITE
                NOT INVALID KEY
                    ADD  QFITAS-REC           TO FITA-WK2
                    ADD  QDVD-REC             TO DVD-WK2
                    ADD  QPOSTER-REC          TO POSTER-WK2
                    ADD  QFOTO-CD-REC         TO FOTO-CD-WK2
                    ADD  QMOLDURA-REC         TO MOLDURA-WK2
                    ADD  QPFITA-REC           TO PFITA-WK2
                    ADD  QPORTA-DVD-REC       TO PDVD-WK2
                    ADD  QCOBERTURA-REC       TO CFITA-WK2
                    ADD  QT-COBERTURA-DVD-REC TO CDVD-WK2
                    ADD  QBOOK-REC            TO BOOK-WK2
                    ADD  QPORTA-RETRATO-REC-C TO PORTA-RETRATO-WK2
                    ADD  QPENDRIVE-REC-C      TO PENDRIVE-WK2
                    ADD  QVIDEO-HD-REC-C      TO VIDEO-HD-WK2
                    ADD  QREVISTA-REC-C       TO REVISTA-WK2
                    ADD  QCALENDARIO-REC-C    TO CALENDARIO-WK2
                    REWRITE REG-WORK2
                    END-REWRITE
                END-READ

                ADD QFITAS-REC                TO FITA-WK
                ADD QDVD-REC                  TO DVD-WK
                ADD QPOSTER-REC               TO POSTER-WK
                ADD QFOTO-CD-REC              TO FOTO-CD-WK
                ADD QMOLDURA-REC              TO MOLDURA-WK
                ADD QPFITA-REC                TO PFITA-WK
                ADD QPORTA-DVD-REC            TO PDVD-WK
                ADD QBOOK-REC                 TO BOOK-WK
                ADD QPORTA-RETRATO-REC-C      TO PORTA-RETRATO-WK
                ADD QPENDRIVE-REC-C           TO PENDRIVE-WK
                ADD QVIDEO-HD-REC-C           TO VIDEO-HD-WK
                ADD QREVISTA-REC-C            TO REVISTA-WK
                ADD QCALENDARIO-REC-C         TO CALENDARIO-WK
                MOVE QCOBERTURA-REC           TO CFITA-WK
                MOVE QT-COBERTURA-DVD-REC     TO CDVD-WK
                MOVE SPACES                   TO TIPO-WK
                IF POSTER-ABERTURA-REC = 1
                    MOVE "AB"                 TO TIPO-WK
                END-IF
                IF POSTER-XEROX-REC = 1
                    MOVE "XE"                 TO TIPO-WK
                END-IF
                REWRITE REG-WORK
                END-REWRITE
           END-READ.

      *-------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-FITA
                         TOT-DVD
                         TOT-POSTER
                         TOT-FOTO-CD
                         TOT-MOLDURA
                         TOT-PFITA
                         TOT-PDVD
                         TOT-CFITA
                         TOT-CDVD
                         TOT-BOOK
                         TOT-PORTA-RETRATO
                         TOT-PENDRIVE
                         TOT-VIDEOHD
                         TOT-REVISTA
                         TOT-CALENDARIO

           INVOKE GS-LISTVIEW-PRODUTOS "DeleteAll"

           INITIALIZE REG-WORK
                      TOTAL-LINHAS

           EVALUATE GS-ORDEM
               WHEN 1 START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 2 START WORK KEY IS NOT < FORMANDO-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 3 START WORK KEY IS NOT < DTVENDA-CH-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 4 START WORK KEY IS NOT < VENDEDOR-CH-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
           END-EVALUATE

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   ADD 1              TO TOTAL-LINHAS
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-GER

           perform mostrar-fonte-favoPro
           perform mostrar-colunas-favoPro.

       CARREGA-LISTA2 SECTION.
           INITIALIZE REG-WORK2
                      TOT-FITA2
                      TOT-DVD2
                      TOT-POSTER2
                      TOT-FOTO-CD2
                      TOT-MOLDURA2
                      TOT-PFITA2
                      TOT-PDVD2
                      TOT-CFITA2
                      TOT-CDVD2
                      TOT-BOOK2
                      TOT-PORTA-RETRATO2
                      TOT-PENDRIVE2
                      TOT-VIDEOHD2
                      TOT-REVISTA2
                      TOT-CALENDARIO2

           INVOKE GS-LISTVIEW-VENDEDOR "DeleteAll"

           START WORK2 KEY IS NOT < VENDEDOR-WK2 INVALID KEY
                 MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
              READ WORK2 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK2
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET2
              END-READ
           END-PERFORM.

      ******************************************************************
      * INSERIR NO LISTVIEW DOS VENDEDORES
      ******************************************************************
           initialize indice
           invoke gs-listview-vendedor
                  "adicionarItem" returning wsItem
      *>Vendedor
           add 1 to indice
      *>Fita
           add 1 to indice
           initialize wsTexto
           move tot-fita2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>DVD
           add 1 to indice
           initialize wsTexto
           move tot-dvd2          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Poster
           add 1 to indice
           initialize wsTexto
           move tot-poster2       to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>SGL
           add 1 to indice
      *>Foto CD
           add 1 to indice
           initialize wsTexto
           move tot-foto-cd2      to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Moldura
           add 1 to indice
           initialize wsTexto
           move tot-moldura2      to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta Fita
           add 1 to indice
           initialize wsTexto
           move tot-pfita2        to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta DVD
           add 1 to indice
           initialize wsTexto
           move tot-pdvd2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>C Fita
           add 1 to indice
           initialize wsTexto
           move tot-cfita2        to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>C DVD
           add 1 to indice
           initialize wsTexto
           move tot-cdvd2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Book
           add 1 to indice
           initialize wsTexto
           move tot-book2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta Retrato
           add 1 to indice
           initialize wsTexto
           move tot-porta-retrato2 to qtde-e
           string qtde-e X"00"   into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Pendrive
           add 1 to indice
           initialize wsTexto
           move tot-pendrive2     to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Video HD
           add 1 to indice
           initialize wsTexto
           move tot-videohd2      to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Revista
           add 1 to indice
           initialize wsTexto
           move tot-revista2      to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Calendário
           add 1 to indice
           initialize wsTexto
           move tot-calendario2   to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto.


           perform mostrar-fonte-favoVen
           perform mostrar-colunas-favoVen.

       MOVER-DADOS-LINDET2 SECTION.
           ADD  FITA-WK2           TO TOT-FITA2
           ADD  DVD-WK2            TO TOT-DVD2
           ADD  POSTER-WK2         TO TOT-POSTER2
           ADD  FOTO-CD-WK2        TO TOT-FOTO-CD2
           ADD  MOLDURA-WK2        TO TOT-MOLDURA2
           ADD  PFITA-WK2          TO TOT-PFITA2
           ADD  PDVD-WK2           TO TOT-PDVD2
           ADD  CFITA-WK2          TO TOT-CFITA2
           ADD  CDVD-WK2           TO TOT-CDVD2
           ADD  BOOK-WK2           TO TOT-BOOK2
           ADD  PORTA-RETRATO-WK2  TO TOT-PORTA-RETRATO2
           ADD  PENDRIVE-WK2       TO TOT-PENDRIVE2
           ADD  VIDEO-HD-WK2       TO TOT-VIDEOHD2
           ADD  REVISTA-WK2        TO TOT-REVISTA2
           ADD  CALENDARIO-WK2     TO TOT-CALENDARIO2

      ******************************************************************
      * INSERIR NO LISTVIEW DOS VENDEDORES
      ******************************************************************
           initialize indice
           invoke gs-listview-vendedor
                  "adicionarItem" returning wsItem
      *>Vendedor
           add 1 to indice
           initialize wsTexto
           string vendedor-wk2 X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Fita
           add 1 to indice
           initialize wsTexto
           move fita-wk2          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>DVD
           add 1 to indice
           initialize wsTexto
           move dvd-wk2           to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Poster
           add 1 to indice
           initialize wsTexto
           move poster-wk2        to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>SGL
           add 1 to indice
      *>Foto CD
           add 1 to indice
           initialize wsTexto
           move foto-cd-wk2       to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Moldura
           add 1 to indice
           initialize wsTexto
           move moldura-wk2       to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta Fita
           add 1 to indice
           initialize wsTexto
           move pfita-wk2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta DVD
           add 1 to indice
           initialize wsTexto
           move pdvd-wk2          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>C Fita
           add 1 to indice
           initialize wsTexto
           move cfita-wk2         to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>C DVD
           add 1 to indice
           initialize wsTexto
           move cdvd-wk2          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Book
           add 1 to indice
           initialize wsTexto
           move book-wk2          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Porta Retrato
           add 1 to indice
           initialize wsTexto
           move porta-retrato-wk2 to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Pendrive
           add 1 to indice
           initialize wsTexto
           move pendrive-wk2 to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Video HD
           add 1 to indice
           initialize wsTexto
           move video-hd-wk2      to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Revista
           add 1 to indice
           initialize wsTexto
           move revista-wk2       to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto
      *>Calendário
           add 1 to indice
           initialize wsTexto
           move calendario-wk2    to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-vendedor "preencherColunaZ"
             using wsItem lnkcolunasVen(indice) wsTexto.


       MOVER-DADOS-LINDET SECTION.
           ADD FITA-WK              TO TOT-FITA
           ADD DVD-WK               TO TOT-DVD
           ADD POSTER-WK            TO TOT-POSTER
           ADD FOTO-CD-WK           TO TOT-FOTO-CD
           ADD MOLDURA-WK           TO TOT-MOLDURA
           ADD PFITA-WK             TO TOT-PFITA
           ADD PDVD-WK              TO TOT-PDVD
           ADD CFITA-WK             TO TOT-CFITA
           ADD CDVD-WK              TO TOT-CDVD
           ADD BOOK-WK              TO TOT-BOOK

           ADD PORTA-RETRATO-WK     TO TOT-PORTA-RETRATO
           ADD PENDRIVE-WK          TO TOT-PENDRIVE
           ADD VIDEO-HD-WK          TO TOT-VIDEOHD
           ADD REVISTA-WK           TO TOT-REVISTA
           ADD CALENDARIO-WK        TO TOT-CALENDARIO

      ******************************************************************
      * INSERIR NO LISTVIEW DOS PRODUTOS
      ******************************************************************
           initialize indice
           invoke gs-listview-produtos
                  "adicionarItem" returning wsItem
      *>Contrato
           add 1 to indice
           initialize wsTexto
           string contrato-wk X"00"  into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Nome do formando
           add 1 to indice
           initialize wsTexto
           string formando-wk X"00" delimited by "   " into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Data Venda
           add 1 to indice
           initialize wsTexto
           string dia-venda-wk "/" mes-venda-wk "/" ano-venda-wk X"00"
             into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Vendedor
           add 1 to indice
           initialize wsTexto
           string vendedor-wk X"00" delimited by "   " into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Fita
           add 1 to indice
           initialize wsTexto
           move fita-wk           to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>DVD
           add 1 to indice
           initialize wsTexto
           move dvd-wk            to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Poster
           add 1 to indice
           initialize wsTexto
           move poster-wk         to qtde-e
           string qtde-e  X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Tipo
           add 1 to indice
           initialize wsTexto
           string tipo-wk X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Foto CD
           add 1 to indice
           initialize wsTexto
           move foto-cd-wk        to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Moldura
           add 1 to indice
           initialize wsTexto
           move moldura-wk        to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta Fita
           add 1 to indice
           initialize wsTexto
           move pfita-wk          to qtde-e
           string qtde-e  X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta DVD
           add 1 to indice
           initialize wsTexto
           move pDVD-wk           to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta C Fita
           add 1 to indice
           initialize wsTexto
           move cfita-wk          to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta C DVD
           add 1 to indice
           initialize wsTexto
           move cdvd-wk           to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Book
           add 1 to indice
           initialize wsTexto
           move book-wk           to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta Retrato
           add 1 to indice
           initialize wsTexto
           move porta-retrato-wk  to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Pendrive
           add 1 to indice
           initialize wsTexto
           move pendrive-wk       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Video HD
           add 1 to indice
           initialize wsTexto
           move video-hd-wk       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Revista
           add 1 to indice
           initialize wsTexto
           move revista-wk        to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Calendário
           add 1 to indice
           initialize wsTexto
           move calendario-wk     to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto.

       TOTALIZA-GER SECTION.
           initialize indice
           invoke gs-listview-produtos "adicionarItem" returning wsItem
      *>Contrato
           add 1 to indice
      *>Nome do formando
           add 1 to indice
      *>Data Venda
           add 1 to indice
      *>Vendedor
           add 1 to indice
      *>Fita
           add 1 to indice
           initialize wsTexto
           move tot-fita          to qtde-e
           string qtde-e X"00"  into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>DVD
           add 1 to indice
           initialize wsTexto
           move tot-dvd           to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Poster
           add 1 to indice
           initialize wsTexto
           move tot-poster        to qtde-e
           string qtde-e  X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Tipo
           add 1 to indice
      *>Foto CD
           add 1 to indice
           initialize wsTexto
           move tot-foto-cd       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Moldura
           add 1 to indice
           initialize wsTexto
           move tot-moldura       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta Fita
           add 1 to indice
           initialize wsTexto
           move tot-pfita         to qtde-e
           string qtde-e  X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta DVD
           add 1 to indice
           initialize wsTexto
           move tot-pdvd          to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta C Fita
           add 1 to indice
           initialize wsTexto
           move tot-cfita         to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta C DVD
           add 1 to indice
           initialize wsTexto
           move tot-cdvd          to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Book
           add 1 to indice
           initialize wsTexto
           move tot-book          to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Porta Retrato
           add 1 to indice
           initialize wsTexto
           move tot-porta-retrato to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Pendrive
           add 1 to indice
           initialize wsTexto
           move tot-pendrive      to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Video HD
           add 1 to indice
           initialize wsTexto
           move tot-videohd       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Revista
           add 1 to indice
           initialize wsTexto
           move tot-revista       to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto
      *>Calendário
           add 1 to indice
           initialize wsTexto
           move tot-calendario    to qtde-e1
           string qtde-e1 X"00" into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto.


       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP209" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
       IMPRIMIR-RELATORIO SECTION.
      *    MOVE ZEROS TO PAG-W.

      *    COPY CONDENSA.

           IF GS-IMPR-RELATORIO = 1
              PERFORM IMPRIMIR-PRODUTOS.

           IF GS-IMPR-RESUMO = 1
      *       MOVE SPACES TO REG-RELAT
      *       WRITE REG-RELAT AFTER PAGE
              PERFORM IMPRIMIR-RESUMO.

      *    COPY DESCONDENSA.


       IMPRIMIR-PRODUTOS SECTION.
           MOVE ZEROS TO LIN.
           MOVE 1 TO QUAL-CAB

           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
               WHEN 1 START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 2 START WORK KEY IS NOT < FORMANDO-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 3 START WORK KEY IS NOT < DTVENDA-CH-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
               WHEN 4 START WORK KEY IS NOT < VENDEDOR-CH-WK INVALID KEY
                         MOVE "10" TO ST-WORK
                      END-START
           END-EVALUATE
           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    PERFORM MOVER-DADOS-RELATORIO
      *             WRITE REG-RELAT FROM LINDET
      *             ADD 1 TO LIN
      *             IF LIN > 56
      *                PERFORM CABECALHO
      *             END-IF
               END-READ
           END-PERFORM.
           PERFORM TOTALIZA-GER-REL.

       IMPRIMIR-RESUMO SECTION.
           MOVE ZEROS TO LIN.

           MOVE 2     TO QUAL-CAB

           INITIALIZE REG-WORK2
                      TOT-FITA2
                      TOT-DVD2
                      TOT-POSTER2
                      TOT-FOTO-CD2
                      TOT-MOLDURA2
                      TOT-PFITA2
                      TOT-PDVD2
                      TOT-BOOK2
                      TOT-PORTA-RETRATO2
                      TOT-PENDRIVE2
                      TOT-VIDEOHD2
                      TOT-REVISTA2
                      TOT-CALENDARIO2


           START WORK2 KEY IS NOT < VENDEDOR-WK2 INVALID KEY
                 MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
                 READ WORK2 NEXT RECORD AT END
                      MOVE "10" TO ST-WORK2
                 NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO2
      *               WRITE REG-RELAT FROM LINDET
      *               ADD 1 TO LIN
      *               IF LIN > 56
      *                  PERFORM CABECALHO2
      *               END-IF
                 END-READ
           END-PERFORM.

           MOVE DET-SEP2            TO ATVAR-R1
           WRITE ATVAR-R1
           PERFORM VERIFICAR-LINHA

           MOVE "TOTALIZADOR"       TO DET-VENDEDOR2
           MOVE TOT-FITA2           TO DET-FITA2
           MOVE TOT-DVD2            TO DET-DVD2
           MOVE TOT-POSTER2         TO DET-POSTER2
           MOVE TOT-FOTO-CD2        TO DET-FOTO-CD2
           MOVE TOT-MOLDURA2        TO DET-MOLDURA2
           MOVE TOT-PFITA2          TO DET-PFITA2
           MOVE TOT-PDVD2           TO DET-PDVD2
           MOVE TOT-CFITA2          TO DET-CFITA2
           MOVE TOT-CDVD2           TO DET-CDVD2
           MOVE TOT-BOOK2           TO DET-BOOK2
           MOVE TOT-PORTA-RETRATO2  TO DET-PORT-RET2
           MOVE TOT-PENDRIVE2       TO DET-PENDRIVE2
           MOVE TOT-VIDEOHD2        TO DET-VIDEOHD2
           MOVE TOT-REVISTA2        TO DET-REVISTA2
           MOVE TOT-CALENDARIO2     TO DET-CALENDARIO2

           MOVE DET-02              TO ATVAR-R1
           WRITE ATVAR-R1
           PERFORM VERIFICAR-LINHA.

      *    MOVE ALL "-" TO LINDET(1:94)
      *    WRITE REG-RELAT FROM LINDET

      *    MOVE SPACES             TO LINDET
      *    MOVE TOT-FITA2          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(32:6)
      *
      *    MOVE TOT-DVD2           TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(38:6)
      *
      *    MOVE TOT-POSTER2        TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(46:6)
      *
      *    MOVE TOT-FOTO-CD2       TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(54:6)
      *
      *    MOVE TOT-MOLDURA2       TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(63:6)
      *
      *    MOVE TOT-PFITA2         TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(72:6)
      *
      *    MOVE TOT-PDVD2          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(78:6).
      *
      *    MOVE TOT-CFITA2         TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(84:6)
      *
      *    MOVE TOT-CDVD2          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(90:6)
      *
      *    MOVE TOT-BOOK2          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET(96:6)
      *
      *    WRITE REG-RELAT FROM LINDET.



       MOVER-DADOS-RELATORIO2 SECTION.
           MOVE VENDEDOR-WK2       TO DET-VENDEDOR2
           MOVE FITA-WK2           TO DET-FITA2
           MOVE DVD-WK2            TO DET-DVD2
           MOVE POSTER-WK2         TO DET-POSTER2
           MOVE FOTO-CD-WK2        TO DET-FOTO-CD2
           MOVE MOLDURA-WK2        TO DET-MOLDURA2
           MOVE PFITA-WK2          TO DET-PFITA2
           MOVE PDVD-WK2           TO DET-PDVD2
           MOVE CFITA-WK2          TO DET-CFITA2
           MOVE CDVD-WK2           TO DET-CDVD2
           MOVE BOOK-WK2           TO DET-BOOK2
           MOVE PORTA-RETRATO-WK2  TO DET-PORT-RET2
           MOVE PENDRIVE-WK2       TO DET-PENDRIVE2
           MOVE VIDEO-HD-WK2       TO DET-VIDEOHD2
           MOVE REVISTA-WK2        TO DET-REVISTA2
           MOVE CALENDARIO-WK2     TO DET-CALENDARIO2


           ADD FITA-WK2            TO TOT-FITA2
           ADD DVD-WK2             TO TOT-DVD2
           ADD POSTER-WK2          TO TOT-POSTER2
           ADD FOTO-CD-WK2         TO TOT-FOTO-CD2
           ADD MOLDURA-WK2         TO TOT-MOLDURA2
           ADD PFITA-WK2           TO TOT-PFITA2
           ADD PDVD-WK2            TO TOT-PDVD2
           ADD CFITA-WK2           TO TOT-CFITA2
           ADD CDVD-WK2            TO TOT-CDVD2
           ADD BOOK-WK2            TO TOT-BOOK2
           ADD PORTA-RETRATO-WK2   TO TOT-PORTA-RETRATO2
           ADD PENDRIVE-WK2        TO TOT-PENDRIVE2
           ADD VIDEO-HD-WK2        TO TOT-VIDEOHD2
           ADD REVISTA-WK2         TO TOT-REVISTA2
           ADD CALENDARIO-WK2      TO TOT-CALENDARIO2

           MOVE DET-02             TO ATVAR-R1
           WRITE ATVAR-R1
           PERFORM VERIFICAR-LINHA.



      *    MOVE SPACES             TO LINDET
      *    MOVE VENDEDOR-WK2       TO LINDET(1:30)
      *    MOVE FITA-WK2           TO QTDE-E
      *    ADD  FITA-WK2           TO TOT-FITA2
      *    MOVE QTDE-E             TO LINDET(32:6)
      *
      *    MOVE DVD-WK2            TO QTDE-E
      *    ADD  DVD-WK2            TO TOT-DVD2
      *    MOVE QTDE-E             TO LINDET(38:6)
      *
      *    MOVE POSTER-WK2         TO QTDE-E
      *    ADD  POSTER-WK2         TO TOT-POSTER2
      *    MOVE QTDE-E             TO LINDET(46:6)
      *
      *    MOVE FOTO-CD-WK2        TO QTDE-E
      *    ADD  FOTO-CD-WK2        TO TOT-FOTO-CD2
      *    MOVE QTDE-E             TO LINDET(54:6)
      *
      *    MOVE MOLDURA-WK2        TO QTDE-E
      *    ADD  MOLDURA-WK2        TO TOT-MOLDURA2
      *    MOVE QTDE-E             TO LINDET(63:6)
      *
      *    MOVE PFITA-WK2          TO QTDE-E
      *    ADD  PFITA-WK2          TO TOT-PFITA2
      *    MOVE QTDE-E             TO LINDET(72:6)
      *
      *    MOVE PDVD-WK2           TO QTDE-E
      *    ADD  PDVD-WK2           TO TOT-PDVD2
      *    MOVE QTDE-E             TO LINDET(78:6)
      *
      *    MOVE CFITA-WK2          TO QTDE-E
      *    ADD  CFITA-WK2          TO TOT-CFITA2
      *    MOVE QTDE-E             TO LINDET(84:6)
      *
      *    MOVE CDVD-WK2           TO QTDE-E
      *    ADD  CDVD-WK2           TO TOT-CDVD2
      *    MOVE QTDE-E             TO LINDET(90:6)
      *
      *    MOVE BOOK-WK2           TO QTDE-E
      *    ADD  BOOK-WK2           TO TOT-BOOK2
      *    MOVE QTDE-E             TO LINDET(90:6).


       MOVER-DADOS-RELATORIO SECTION.
           move contrato-wk        to det-contrato
           move formando-wk        to det-formando
           string dia-venda-wk "/" mes-venda-wk "/" ano-venda-wk into
                                      det-dtvenda
           move vendedor-wk        to det-vendedor
           move fita-wk            to det-fita
           move dvd-wk             to det-dvd
           move poster-wk          to det-poster
           move tipo-wk            to det-tipo
           move foto-cd-wk         to det-fotocd
           move moldura-wk         to det-moldura
           move pfita-wk           to det-pfita
           move pdvd-wk            to det-pdvd
           move cfita-wk           to det-cfita
           move cdvd-wk            to det-cdvd
           move book-wk            to det-book
           move porta-retrato-wk   to det-porta-retrato
           move pendrive-wk        to det-pendrive
           move video-hd-wk        to det-videohd
           move revista-wk         to det-revista
           move calendario-wk      to det-calendario
           move "___/___/_____"    to det-data

           move det-01             to atvar-r1
           write atvar-r1
           perform verificar-linha.

      *    MOVE SPACES             TO LINDET-REL.
      *
      *    MOVE CONTRATO-WK        TO LINDET-REL(1: 8)
      *    MOVE FORMANDO-WK        TO LINDET-REL(10:19)
      *    STRING DIA-VENDA-WK "/" MES-VENDA-WK "/" ANO-VENDA-WK INTO
      *    LINDET-REL(30:10)
      *    MOVE VENDEDOR-WK        TO LINDET-REL(41:11)
      *    MOVE FITA-WK            TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(54: 6)
      *    MOVE DVD-WK             TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(59: 5)
      *    MOVE POSTER-WK          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(64: 6)
      *    MOVE TIPO-WK            TO LINDET-REL(70:3)
      *    MOVE FOTO-CD-WK         TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(73: 5)
      *    MOVE MOLDURA-WK         TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(78: 5)
      *    MOVE PFITA-WK           TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(84: 6)
      *    MOVE PDVD-WK            TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(91:5)
      *    MOVE CFITA-WK           TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(98:5)
      *    MOVE CDVD-WK            TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(103:5)
      *    MOVE BOOK-WK            TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(108:5)
      *    MOVE "___/___/_____"    TO LINDET-REL(114:13).

       TOTALIZA-GER-REL SECTION.
      *    MOVE SPACES             TO LINDET-REL.
      *
      *    MOVE TOT-FITA           TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(54: 6)
      *    MOVE TOT-DVD            TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(59: 5)
      *    MOVE TOT-POSTER         TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(64: 6)
      *    MOVE TOT-FOTO-CD        TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(73: 5)
      *    MOVE TOT-MOLDURA        TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(78: 5)
      *    MOVE TOT-PFITA          TO QTDE-E
      *    MOVE QTDE-E             TO LINDET-REL(84: 6)
      *    MOVE TOT-PDVD           TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(91: 5)
      *    MOVE TOT-CFITA          TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(98: 5)
      *    MOVE TOT-CDVD           TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(103: 5)
      *    MOVE TOT-BOOK           TO QTDE-E1
      *    MOVE QTDE-E1            TO LINDET-REL(108: 5)

           move det-sep1           to atvar-r1
           write atvar-r1
           perform verificar-linha

           move zeros              to det-contrato
           move spaces             to det-formando
           move spaces             to det-dtvenda
           move "TOTALIZADOR"      to det-vendedor
           move tot-fita           to det-fita
           move tot-dvd            to det-dvd
           move tot-poster         to det-poster
           move spaces             to det-tipo
           move tot-foto-cd        to det-fotocd
           move tot-moldura        to det-moldura
           move tot-pfita          to det-pfita
           move tot-pdvd           to det-pdvd
           move tot-cfita          to det-cfita
           move tot-cdvd           to det-cdvd
           move tot-book           to det-book
           move tot-porta-retrato  to det-porta-retrato
           move tot-pendrive       to det-pendrive
           move tot-videohd        to det-videohd
           move tot-revista        to det-revista
           move tot-calendario     to det-calendario
           move spaces             to det-data


           MOVE DET-01             TO ATVAR-R1
           WRITE ATVAR-R1
           PERFORM VERIFICAR-LINHA.

      *    WRITE REG-RELAT FROM LINDET AFTER 2.

      *CABECALHO SECTION.
      *    ADD 1 TO LIN PAG-W.
      *    MOVE PAG-W TO PG-REL.
      *    IF LIN = 1
      *       WRITE REG-RELAT FROM CAB01 AFTER 0
      *    ELSE
      *       WRITE REG-RELAT FROM CAB01 AFTER PAGE.
      *
      *    WRITE REG-RELAT FROM CAB02.
      *
      *    WRITE REG-RELAT FROM CAB03.
      *    WRITE REG-RELAT FROM CAB04.
      *    WRITE REG-RELAT FROM CAB03.
      *    MOVE 6 TO LIN.
      *
      *CABECALHO2 SECTION.
      *    ADD 1 TO LIN PAG-W.
      *    MOVE PAG-W TO PG-REL.
      *    IF LIN = 1
      *       WRITE REG-RELAT FROM CAB01 AFTER 0
      *    ELSE
      *       WRITE REG-RELAT FROM CAB01 AFTER PAGE.
      *
      *    WRITE REG-RELAT FROM CAB02.
      *
      *    WRITE REG-RELAT FROM CAB03.
      *    WRITE REG-RELAT FROM CAB05.
      *    WRITE REG-RELAT FROM CAB03.
      *    MOVE 6 TO LIN.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

      *---------------------------- CABECALHO
       ADICIONAR-CABECALHO SECTION.
           move function current-date to ws-data-sys
           accept ws-hora-sys from time
           open output atvar

           initialize conta-linha

           add 1 to w-pagina
           move lnk-empresa                 to det-empresa-p
           move lnk-razao-emp               to det-razao-p
           move w-pagina                    to det-pagina-p
           move det-cabecalho1-p            to atvar-r1
                                         write atvar-r1

           MOVE "RCP209 - RELATÓRIO DE PRODUTOS VENDIDOS"
                                            TO det-titulo-cab-p

           move ws-dia-cpu                  to det-dia-emis-p
           move ws-mes-cpu                  to det-mes-emis-p
           move ws-ano-cpu                  to det-ano-emis-p
           move ws-ho-sys                   to det-ho-emis-p
           move ws-mi-sys                   to det-mi-emis-p
           move det-cabecalho2-p            to atvar-r1
                                         write atvar-r1

           move usuario-w                   to det-usuario-p
           move cab02                       to atvar-r1
                                         write atvar-r1

           EVALUATE QUAL-CAB
               WHEN 1 MOVE CAB-01           to atvar-r1
               WHEN 2 MOVE CAB-02           to atvar-r1
           END-EVALUATE
           WRITE ATVAR-R1.
       ADICIONAR-CABECALHO-FIM.
           EXIT.

      *----------------------- DEFINIÇÃO
      *
       PARAMETROS-IMPRESSAO SECTION.
      *   VISUALIZAÇÃO DO RELATÓRIO | 1 = PAGINA INTEIRA  | 2 = 100% |
      *   | 3 = PARA ENCAIXAR |
          MOVE 2   TO COBREL-VISUALIZACAO

          MOVE "P" TO COBREL-ORIENTACAO-PAGINA

      *CONTROLE DA QUANTIDADE DE LINHA PARA O CABECALHO 2
          MOVE 1   TO COBREL-QTD-LINHA-CAB2
      *CONTROLE DA QUANTIDADE DE LINHA PARA O CABECALHO 3
          MOVE 0   TO COBREL-QTD-LINHA-CAB3
      *CONTROLE DA QUANTIDADE DE LINHA PARA O CABECALHO 4
          MOVE 0   TO COBREL-QTD-LINHA-CAB4
      *CONTROLE DA QUANTIDADE DE LINHA PARA O CABECALHO 5
          MOVE 0   TO COBREL-QTD-LINHA-CAB5
      *CONTROLE DA BARRA DE STATUS DO RELATORIO
          MOVE "N" TO COBREL-BARRA-STATUS
      *CONTROLA O TITULO DO RELATORIO
          MOVE "RCP209 - RELATÓRIO DE PRODUTOS VENDIDOS" TO
                COBREL-TITULO-RELATORIO.
       PARAMETROS-IMPRESSAO-FIM.
           EXIT.
      *--------------------------------------------------------------FIM

       COPY  DEFINICAO-UNICA.CPY.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE RCD100 CGD001 MTD019 WORK WORK2 MTD020 ACD100 CGD011
                 CAD010 RCD100C MTD020C.
           DELETE FILE WORK
           DELETE FILE WORK2.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
