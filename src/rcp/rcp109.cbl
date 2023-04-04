       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP109.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 24-10-2007
      *DESCRIÇÃO: Cadastro de ESCALA DE VENDAS
       ENVIRONMENT DIVISION.
       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX010.
           COPY COPX040.
           COPY COPX041.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX020P.
           COPY RCPX109.
           COPY RCPX109P.
           COPY RCPX1101.
           COPY RCPX110.
           COPY RCPX111.
           COPY RCPX112.
           COPY IEPX010.
           COPY IEPX011.
           COPY CADPRO.SEL.
           COPY CADMOD.SEL.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW010.
           COPY COPW040.
           COPY COPW041.
           COPY MTPW019.
           COPY MTPW020.
           COPY MTPW020P.
           COPY RCPW109.
           COPY RCPW109P.
           COPY RCPW1101.
           COPY RCPW110.
           COPY RCPW111.
           COPY RCPW112.
           COPY IEPW010.
           COPY IEPW011.
           COPY CADPRO.FD.
           COPY CADMOD.FD.

       WORKING-STORAGE SECTION.
           COPY "RCP109.CPB".
           COPY "RCP109.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010                   PIC XX       VALUE SPACES.
           05  ST-COD040                   PIC XX       VALUE SPACES.
           05  ST-COD041                   PIC XX       VALUE SPACES.
           05  ST-MTD019                   PIC XX       VALUE SPACES.
           05  ST-MTD020                   PIC XX       VALUE SPACES.
           05  ST-MTD020P                  PIC XX       VALUE SPACES.
           05  ST-RCD109                   PIC XX       VALUE SPACES.
           05  ST-RCD109P                  PIC XX       VALUE SPACES.
           05  ST-RCD1101                  PIC XX       VALUE SPACES.
           05  ST-RCD110                   PIC XX       VALUE SPACES.
           05  ST-RCD111                   PIC XX       VALUE SPACES.
           05  ST-RCD112                   PIC XX       VALUE SPACES.
           05  ST-IED010                   PIC XX       VALUE SPACES.
           05  ST-IED011                   PIC XX       VALUE SPACES.
           05  ST-CADPRO                   PIC XX       VALUE SPACES.
           05  ST-CADMOD                   PIC XX       VALUE SPACES.
           05  ERRO-W                      PIC 9        VALUE ZEROS.
           05  GS-IND                      PIC 9(03)    VALUE ZEROS.
           05  AUX-CURSO                   PIC X(30)    VALUE SPACES.
           05  ORDEM-W                     PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                      PIC 9(8)     VALUE ZEROS.
           05  LIN                         PIC 9(2)     VALUE ZEROS.
           05  MESANO-W                    PIC 9(6)     VALUE ZEROS.
           05  PAG-W                       PIC 9(2)     VALUE ZEROS.
           05  VALOR-E                     PIC ZZ,ZZZ.
           05  VALOR-AVISTA                PIC 9(06)V99 VALUE ZEROS.
           05  VALOR-CALCULADO             PIC 9(06)V99 VALUE ZEROS.
           05  VALOR-JUROS                 PIC 9(06)V99 VALUE ZEROS.
           05  MASC-VALOR                  PIC ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC-QTDE                   PIC Z.ZZ9    VALUE ZEROS.
           05  QTDE-LINHA                  PIC 9(02)    VALUE ZEROS.
           05  ERRO                        PIC X(01)    VALUE SPACES.
           05  PRIMEIRA                    PIC X(01)    VALUE SPACES.
           05  NUMERO-PARCELA              PIC 9(03)    VALUE ZEROS.
           05  AUX-NUMERO-PARCELA          PIC 9(03)    VALUE ZEROS.
           05  QTDE-IMPRESSO               PIC S9(02)    VALUE ZEROS.
           05  WS-OK                       PIC X(01)    VALUE SPACES.
           05  QUAL-COLUNA                 PIC 9(02)    VALUE ZEROS.
           05  CONTROLE-LINHA              PIC 9(02)    VALUE ZEROS.
           05  AUX-VALOR-BASE-PRODUTO      PIC 9(06)V99.
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

       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.
       01 indice                       pic 9(02).
       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.

       01 lnktabelaPro.
          02 lnkobjetoscolPro  object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).

       01 lnk-printer.
          03 lnk-num-cabs        pic 9(02).
          03 lnk-timbrado        pic x(01).
          03 lnk-cabecalhos      pic x(160) occurs 10 times.
          03 lnk-tamrel          pic 9(03).
          03 lnk-title-text      pic x(50).
          03 lnk-path-video      pic x(50).
          03 lnk-detalhe         pic x(264).
          03 lnk-funcao          pic x(04).
          03 lnk-video-impr      pic x(01).
          03 lnk-fs-print        pic x(02).
          03 lnk-linhas-pag      pic 9(03).
          03 lnk-colunas-pag     pic 9(03).
          03 lnk-linhas          pic 9(03).

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 AUX-QTD-INI         PIC 9(06).
       01 AUX-QTD-FIN         PIC 9(06).

       01 DET-D-ESCALAS.
          05 FILLER                  PIC X(10)
             VALUE "NR.ESCALA".
          05 FILLER                  PIC X(11)
             VALUE "DATA VIGOR".
          05 FILLER                  PIC X(31)
             VALUE "CURSO".
          05 FILLER                  PIC X(12)
             VALUE "FAIXA DESC.".
          05 FILLER                  PIC X(50)
             VALUE "PLANO PAGTO".

       01 DET-C-ESCALAS.
          05 FILLER                  PIC X(116)
             VALUE ALL "-".

       01 DET-ESCALAS.
          05 FILLER                  PIC X(06).
          05 DET-ESCALA              PIC ZZ9.
          05 FILLER                  PIC X(01).
          05 DET-DATA-VIGOR          PIC 99/99/9999.
          05 FILLER                  PIC X(01).
          05 DET-CURSO               PIC X(30).
          05 FILLER                  PIC X(01).
          05 DET-FAIXA-DESC          PIC X(11).
          05 FILLER                  PIC X(01).
          05 DET-PLANO-PAGTO         PIC X(50).

       01 DET-D-PRODUTO.
          05 FILLER                  PIC X(26)
             VALUE "PRODUTO".
          05 FILLER                  PIC X(11)
             VALUE "VALOR BASE".
          05 DET-D-FAIXA1            PIC X(11).
          05 DET-D-FAIXA2            PIC X(11).
          05 DET-D-FAIXA3            PIC X(11).
          05 DET-D-FAIXA4            PIC X(11).
          05 DET-D-FAIXA5            PIC X(10).

       01 DET-C-PRODUTO.
          05 FILLER                  PIC X(110)
             VALUE ALL "-".

       01 DET-PRODUTOS.
          05 DET-PRODUTO             PIC X(25).
          05 FILLER                  PIC X(01).
          05 DET-VALOR-BASE          PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FAIXA1              PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FAIXA2              PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FAIXA3              PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FAIXA4              PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FAIXA5              PIC ZZZ.ZZ9,99 BLANK WHEN ZEROS.

      ****************************************************************
      *   LINHAS DETALHE DA ETIQUETA
      ****************************************************************

       01 DET-1LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-CONTRATO1           PIC 9(04) BLANK WHEN ZEROS.
          05 DET-IFEM1               PIC X(01).
          05 DET-ALBUM1              PIC 9(04) BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FORMANDO1           PIC X(30).
          05 FILLER                  PIC X(07).
          05 DET-CONTRATO2           PIC 9(04) BLANK WHEN ZEROS.
          05 DET-IFEM2               PIC X(01).
          05 DET-ALBUM2              PIC 9(04) BLANK WHEN ZEROS.
          05 FILLER                  PIC X(01).
          05 DET-FORMANDO2           PIC X(30).

       01 DET-2LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO1         PIC X(07).
          05 DET-SIFRAO1             PIC X(04).
          05 DET-VALOR-PAGTO1        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO2         PIC X(07).
          05 DET-SIFRAO2             PIC X(04).
          05 DET-VALOR-PAGTO2        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO1        PIC X(07).
          05 DET-2SIFRAO1            PIC X(04).
          05 DET-2VALOR-PAGTO1       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO2        PIC X(07).
          05 DET-2SIFRAO2            PIC X(04).
          05 DET-2VALOR-PAGTO2       PIC Z.ZZ9,99 BLANK WHEN ZEROS.


       01 DET-3LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO3         PIC X(07).
          05 DET-SIFRAO3             PIC X(04).
          05 DET-VALOR-PAGTO3        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO4         PIC X(07).
          05 DET-SIFRAO4             PIC X(04).
          05 DET-VALOR-PAGTO4        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO3        PIC X(07).
          05 DET-2SIFRAO3            PIC X(04).
          05 DET-2VALOR-PAGTO3       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO4        PIC X(07).
          05 DET-2SIFRAO4            PIC X(04).
          05 DET-2VALOR-PAGTO4       PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 DET-4LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO5         PIC X(07).
          05 DET-SIFRAO5             PIC X(04).
          05 DET-VALOR-PAGTO5        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO6         PIC X(07).
          05 DET-SIFRAO6             PIC X(04).
          05 DET-VALOR-PAGTO6        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO5        PIC X(07).
          05 DET-2SIFRAO5            PIC X(04).
          05 DET-2VALOR-PAGTO5       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO6        PIC X(07).
          05 DET-2SIFRAO6            PIC X(04).
          05 DET-2VALOR-PAGTO6       PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 DET-5LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO7         PIC X(07).
          05 DET-SIFRAO7             PIC X(04).
          05 DET-VALOR-PAGTO7        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO8         PIC X(07).
          05 DET-SIFRAO8             PIC X(04).
          05 DET-VALOR-PAGTO8        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO7        PIC X(07).
          05 DET-2SIFRAO7            PIC X(04).
          05 DET-2VALOR-PAGTO7       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO8        PIC X(07).
          05 DET-2SIFRAO8            PIC X(04).
          05 DET-2VALOR-PAGTO8       PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 DET-6LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO9         PIC X(07).
          05 DET-SIFRAO9             PIC X(04).
          05 DET-VALOR-PAGTO9        PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO10        PIC X(07).
          05 DET-SIFRAO10            PIC X(04).
          05 DET-VALOR-PAGTO10       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO9        PIC X(07).
          05 DET-2SIFRAO9            PIC X(04).
          05 DET-2VALOR-PAGTO9       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO10       PIC X(07).
          05 DET-2SIFRAO10           PIC X(04).
          05 DET-2VALOR-PAGTO10      PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 DET-7LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO11        PIC X(07).
          05 DET-SIFRAO11            PIC X(04).
          05 DET-VALOR-PAGTO11       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO12        PIC X(07).
          05 DET-SIFRAO12            PIC X(04).
          05 DET-VALOR-PAGTO12       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO11       PIC X(07).
          05 DET-2SIFRAO11           PIC X(04).
          05 DET-2VALOR-PAGTO11      PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO12       PIC X(07).
          05 DET-2SIFRAO12           PIC X(04).
          05 DET-2VALOR-PAGTO12      PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 DET-8LINHA-ETIQ.
          05 filler                  pic x(01).
          05 DET-COND-PAGTO13        PIC X(07).
          05 DET-SIFRAO13            PIC X(04).
          05 DET-VALOR-PAGTO13       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(04).
          05 DET-COND-PAGTO14        PIC X(07).
          05 DET-SIFRAO14            PIC X(04).
          05 DET-VALOR-PAGTO14       PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(05).
          05 DET-2COND-PAGTO13       PIC X(07).
          05 DET-2SIFRAO13           PIC X(04).
          05 DET-2VALOR-PAGTO13      PIC Z.ZZ9,99 BLANK WHEN ZEROS.
          05 FILLER                  PIC X(02).
          05 DET-2COND-PAGTO14       PIC X(07).
          05 DET-2SIFRAO14           PIC X(04).
          05 DET-2VALOR-PAGTO14      PIC Z.ZZ9,99 BLANK WHEN ZEROS.

       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040
           MOVE "COD041"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           MOVE "MTD020P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020P
           MOVE "RCD109"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD109
           MOVE "RCD109P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD109P
           MOVE "RCD1101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD1101
           MOVE "RCD110"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD110
           MOVE "RCD111"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD111
           MOVE "RCD112"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD112
           MOVE "IED010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED010
           MOVE "IED011"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011
           MOVE "CADPRO"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO
           MOVE "CADMOD"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD
           OPEN INPUT COD040 COD041 CAD010 IED010 IED011 MTD020 MTD019
           OPEN I-O   RCD109 RCD110 RCD1101 RCD111 RCD112 CADPRO CADMOD
                      RCD109P MTD020P
           CLOSE      RCD109 RCD110 RCD1101 RCD111 RCD112 CADPRO CADMOD
                      RCD109P MTD020P
           OPEN INPUT RCD109 RCD110 RCD1101 RCD111 RCD112 CADPRO CADMOD
                      RCD109P MTD020P

           IF ST-CAD010 = "35"
              CLOSE CAD010      OPEN OUTPUT CAD010
              CLOSE CAD010      OPEN I-O    CAD010
           END-IF.
           IF ST-CADPRO = "35"
              CLOSE CADPRO      OPEN OUTPUT CADPRO
              CLOSE CADPRO      OPEN I-O    CADPRO
           END-IF.
           IF ST-CADMOD = "35"
              CLOSE CADMOD      OPEN OUTPUT CADMOD
              CLOSE CADMOD      OPEN I-O    CADMOD
           END-IF.
           IF ST-COD040 = "35"
              CLOSE COD040      OPEN OUTPUT COD040
              CLOSE COD040      OPEN I-O    COD040
           END-IF.
           IF ST-COD041 = "35"
              CLOSE COD041      OPEN OUTPUT COD041
              CLOSE COD041      OPEN I-O    COD041
           END-IF.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O    MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O    MTD020
           END-IF.
           IF ST-MTD020P = "35"
              CLOSE MTD020P     OPEN OUTPUT MTD020P
              CLOSE MTD020P     OPEN I-O    MTD020P
           END-IF.
           IF ST-RCD109 = "35"
              CLOSE RCD109      OPEN OUTPUT RCD109
              CLOSE RCD109      OPEN I-O    RCD109
           END-IF.
           IF ST-RCD109P = "35"
              CLOSE RCD109P     OPEN OUTPUT RCD109P
              CLOSE RCD109P     OPEN I-O    RCD109P
           END-IF.
           IF ST-RCD110 = "35"
              CLOSE RCD110      OPEN OUTPUT RCD110
              CLOSE RCD110      OPEN I-O    RCD110
           END-IF.
           IF ST-RCD1101 = "35"
              CLOSE RCD1101     OPEN OUTPUT RCD1101
              CLOSE RCD1101     OPEN I-O    RCD1101
           END-IF.
           IF ST-RCD111 = "35"
              CLOSE RCD111      OPEN OUTPUT RCD111
              CLOSE RCD111      OPEN I-O    RCD111
           END-IF.
           IF ST-RCD112 = "35"
              CLOSE RCD112      OPEN OUTPUT RCD112
              CLOSE RCD112      OPEN I-O    RCD112
           END-IF.
           IF ST-IED010 = "35"
              CLOSE IED010      OPEN OUTPUT IED010
              CLOSE IED010      OPEN I-O    IED010
           END-IF.
           IF ST-IED011 = "35"
              CLOSE IED011      OPEN OUTPUT IED011
              CLOSE IED011      OPEN I-O    IED011
           END-IF.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADPRO <> "00"
              MOVE "ERRO ABERTURA CADPRO: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADPRO TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CADMOD <> "00"
              MOVE "ERRO ABERTURA CADMOD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CADMOD TO GS-MENSAGEM-ERRO(23: 02)
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
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020P <> "00"
              MOVE "ERRO ABERTURA MTD020P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020P TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD109 <> "00"
              MOVE "ERRO ABERTURA RCD109: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD109 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD109P <> "00"
              MOVE "ERRO ABERTURA RCD109P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD109P TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD1101 <> "00"
              MOVE "ERRO ABERTURA RCD1101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD1101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD110 <> "00"
              MOVE "ERRO ABERTURA RCD110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD111 <> "00"
              MOVE "ERRO ABERTURA RCD111: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD111 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD112 <> "00"
              MOVE "ERRO ABERTURA RCD112: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD112 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW-PRODUTOS
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGAR-ULTIMO
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-VERIFICA-CODIGO-TRUE
                   PERFORM VERIFICA-CODIGO
                   PERFORM CARREGAR-ULTIMO
               WHEN GS-POPUP-FLG-TRUE
                   PERFORM POPUP-CONTRATO
               WHEN GS-VALIDA-DT-VIGOR-TRUE
                   PERFORM VALIDA-DT-VIGOR
               WHEN GS-VALIDA-DT-LIBER-TRUE
                   PERFORM VALIDA-DT-LIBER
               WHEN GS-LER-PLANO-TRUE
                   PERFORM LER-PLANO
               WHEN GS-ALTERAR-ESCALA-TRUE
                   PERFORM ALTERAR-ESCALA
               WHEN GS-LER-FAIXA-TRUE
                   PERFORM LER-PRODUTOS
               WHEN GS-ALTERAR-PRODUTO-TRUE
                   PERFORM ALTERAR-PRODUTO
               WHEN GS-CARREGAR-ETIQUETA-TRUE
                   PERFORM CARREGAR-ETIQUETA
               WHEN GS-DADOS-IMPRESSAO-TRUE
                   PERFORM DADOS-IMPRESSAO
               WHEN GS-IMPRIMIR-ETIQUETA-TRUE
                   PERFORM IMPRIMIR-ETIQUETA
               WHEN GS-VER-EVENTO-TRUE
                   PERFORM VERIFICAR-EVENTOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CRIAR-LISTVIEW-PRODUTOS SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Identificação" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                 using z"Produto" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                using z"Modelo" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Base" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Faixa1" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Faixa2" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Faixa3" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Faixa4" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Faixa5" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)


          perform mostrar-fonte-favo
          perform mostrar-colunas-favo

          invoke gs-listview-produtos "gridLines"
          invoke gs-listview-produtos "noBorder".
       CRIAR-LISTVIEW-PRODUTOS-FIM.
           EXIT.

       mostrar-colunas-favo section.
          initialize wsTexto
          move "listview-rcp109-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favo-fim.
           exit.

       mostrar-fonte-favo section.
           move "listview-rcp109-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos wsTexto.
       mostrar-fonte-favo-fim.
           exit.

       EXPORTAR-PARA-EXCEL section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-produtos lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-fim.
           EXIT.


       zebrar-itens section.
           move "listview-rcp109-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos wsTexto
           invoke gs-listview-produtos "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-rcp109-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       VERIFICAR-EVENTOS SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favo
               when 34013  perform produto-aceito
               when 34592  perform produto-aceito
               when 34027  set-focus sb3
           end-evaluate.
       VERIFICAR-EVENTOS-FIM.
           EXIT.

       produto-aceito section.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS "indexOf" USING UMITEM
                                                RETURNING WSITEM

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO) TO GS-COD-PRODUTO

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(2)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE WSTEXTO                  TO GS-PRODUTO

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(3)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE WSTEXTO                  TO GS-MODELO

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(4)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
              MOVE FUNCTION NUMVAL(WSTEXTO)     TO GS-VALOR-BASE


              REFRESH-OBJECT WIN1
              SHOW-WINDOW WIN1
              SET-FOCUS EF3.
       produto-aceito-fim.
           exit.

       IMPRIMIR-ETIQUETA SECTION.
           PERFORM 120-SETAR-PARAMETROS
           PERFORM 130-ABRIR-IMPRESSORA
           IF LNK-FS-PRINT = "00"
              PERFORM 150-IMPRIMIR-ETIQUETA
              PERFORM 160-FECHAR-IMPRESSORA.

       120-SETAR-PARAMETROS SECTION.
           INITIALIZE LNK-PRINTER
           MOVE 0                TO LNK-NUM-CABS
           MOVE "N"              TO LNK-TIMBRADO
           MOVE 125              TO LNK-TAMREL
           MOVE "RCP209.REL"     TO LNK-TITLE-TEXT
           MOVE "C:\RCP209.REL"  TO LNK-PATH-VIDEO
           MOVE "I"              TO LNK-VIDEO-IMPR.
       120-SETAR-PARAMETROS-FIM.
           EXIT.

       130-ABRIR-IMPRESSORA SECTION.
           MOVE "ABRE" TO LNK-FUNCAO
           CALL "PRINT" USING LNK-PRINTER.
       130-ABRIR-IMPRESSORA-FIM.
           EXIT.

       160-FECHAR-IMPRESSORA SECTION.
           MOVE "FECH" TO LNK-FUNCAO
           CALL "PRINT" USING LNK-PRINTER.
       160-FECHAR-IMPRESSORA-FIM.
           EXIT.

       150-IMPRIMIR-ETIQUETA SECTION.
           MOVE GS-ETIQ-LINHA  TO QTDE-LINHA
           MOVE 0              TO QTDE-IMPRESSO
           MOVE "S"            TO PRIMEIRA
           MOVE SPACES         TO GS-LINDET
           MOVE 1              TO GS-LINHA
           MOVE "LER-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = "FIM"
               IF GS-LINDET(1:3) <> "---"
                  MOVE GS-LINDET     TO LNK-DETALHE
                  IF GS-LINDET(2:4)  = GS-CONTRATO OR
                     GS-LINDET(49:4) = GS-CONTRATO
      *              ADD 1          TO QTDE-IMPRESSO
                     MOVE SPACES    TO LNK-DETALHE
                     PERFORM IMPRIMIR-LINHA
                     PERFORM IMPRIMIR-LINHA
      *              PERFORM IMPRIMIR-LINHA
                     MOVE SPACES    TO LNK-DETALHE
                     PERFORM IMPRIMIR-LINHA

                     MOVE GS-LINDET TO LNK-DETALHE
                     PERFORM IMPRIMIR-NEGRITO
                  ELSE
                     PERFORM IMPRIMIR-LINHA
                  END-IF
               ELSE
      *           IF QTDE-LINHA <> 1
      *              MOVE SPACES    TO LNK-DETALHE
      *              PERFORM IMPRIMIR-LINHA
      *              PERFORM IMPRIMIR-LINHA

      *              MOVE SPACES    TO LNK-DETALHE
      *              PERFORM IMPRIMIR-LINHA
      *              COMPUTE QTDE-LINHA = QTDE-LINHA - 1
      *           END-IF
                  ADD 1 TO QTDE-IMPRESSO
               END-IF
               ADD 1               TO GS-LINHA
               MOVE SPACES         TO GS-LINDET
               MOVE "LER-ETIQUETA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       imprimir-linha section.
           IF QTDE-IMPRESSO = 5
              move "S"    to primeira
              move "SALT" to LNK-FUNCAO
              call "PRINT" using LNK-PRINTER
              move 0 to qtde-impresso.

           move "IMPR" to LNK-FUNCAO
           call "PRINT" using LNK-PRINTER.
       imprimir-linha-fim.
           exit.

       imprimir-negrito section.
           move "IMPN" to LNK-FUNCAO
           call "PRINT" using LNK-PRINTER.
       imprimir-negrito-fim.
           exit.

       DADOS-IMPRESSAO SECTION.
           MOVE 1          TO GS-ETIQ-LINHA
           MOVE 1          TO GS-ETIQ-COLUNA
           MOVE 1          TO GS-ETIQ-QTDE
           MOVE 1          TO GS-ETIQ-ALBUM1

           INITIALIZE REG-MTD020
           MOVE GS-CONTRATO          TO CONTRATO-MTG
           MOVE ALL "9"              TO SEQ-MTG
           START MTD020 KEY IS LESS THAN ALBUM-MTG INVALID KEY
               MOVE "Este Contrato Não Possui Álbuns Montados" TO
               MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               READ MTD020 NEXT AT END
                    MOVE "Este Contrato Não Possui Álbuns Montados" TO
                    MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               NOT AT END
                    IF GS-CONTRATO <> CONTRATO-MTG
                       MOVE "Este Contrato Não Possui Álbuns Montados"
                         TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    ELSE
                       MOVE SEQ-MTG    TO GS-ETIQ-ALBUM2.

       CARREGAR-ETIQUETA SECTION.
           MOVE 0 TO GS-FLAG-CRITICA

           IF GS-ETIQ-LINHA = 0 OR GS-ETIQ-LINHA > 5
              STRING "Número de Linhas Informado Incompatível" X"0DA0"
                     "O Permitido é de 1...5" INTO MENSAGEM
                MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF GS-ETIQ-COLUNA = 0 OR GS-ETIQ-COLUNA > 2
              STRING "Número de Colunas Informado Incompatível" X"0DA0"
                     "O Permitido é de 1...2" INTO MENSAGEM
                MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF GS-ETIQ-ALBUM1 = 0
              MOVE "Número do Álbum Inicial Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF GS-ETIQ-ALBUM2 = 0
              MOVE "Número do Álbum Final Não Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF GS-FLAG-CRITICA = 0
              PERFORM LER-DADOS-ETIQUETA.

       LER-DADOS-ETIQUETA SECTION.
           MOVE "APAGAR-ETIQUETA"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE DET-CONTRATO1      DET-IFEM1  DET-IFEM2
                      DET-ALBUM1
                      DET-FORMANDO1
                      DET-CONTRATO2
                      DET-ALBUM2
                      DET-FORMANDO2
                      DET-COND-PAGTO1    DET-2COND-PAGTO1
                      DET-VALOR-PAGTO1   DET-2VALOR-PAGTO1
                      DET-COND-PAGTO2    DET-2COND-PAGTO2
                      DET-VALOR-PAGTO2   DET-2VALOR-PAGTO2
                      DET-COND-PAGTO3    DET-2COND-PAGTO3
                      DET-VALOR-PAGTO3   DET-2VALOR-PAGTO3
                      DET-COND-PAGTO4    DET-2COND-PAGTO4
                      DET-VALOR-PAGTO4   DET-2VALOR-PAGTO4
                      DET-COND-PAGTO5    DET-2COND-PAGTO5
                      DET-VALOR-PAGTO5   DET-2VALOR-PAGTO5
                      DET-COND-PAGTO6    DET-2COND-PAGTO6
                      DET-VALOR-PAGTO6   DET-2VALOR-PAGTO6
                      DET-COND-PAGTO7    DET-2COND-PAGTO7
                      DET-VALOR-PAGTO7   DET-2VALOR-PAGTO7
                      DET-COND-PAGTO8    DET-2COND-PAGTO8
                      DET-VALOR-PAGTO8   DET-2VALOR-PAGTO8
                      DET-COND-PAGTO9    DET-2COND-PAGTO9
                      DET-VALOR-PAGTO9   DET-2VALOR-PAGTO9
                      DET-COND-PAGTO10   DET-2COND-PAGTO10
                      DET-VALOR-PAGTO10  DET-2VALOR-PAGTO10
                      DET-COND-PAGTO11   DET-2COND-PAGTO11
                      DET-VALOR-PAGTO11  DET-2VALOR-PAGTO11
                      DET-COND-PAGTO12   DET-2COND-PAGTO12
                      DET-VALOR-PAGTO12  DET-2VALOR-PAGTO12
                      DET-COND-PAGTO13   DET-2COND-PAGTO13
                      DET-VALOR-PAGTO13  DET-2VALOR-PAGTO13
                      DET-COND-PAGTO14   DET-2COND-PAGTO14
                      DET-VALOR-PAGTO14  DET-2VALOR-PAGTO14

                      DET-SIFRAO1        DET-2SIFRAO1
                      DET-SIFRAO2        DET-2SIFRAO2
                      DET-SIFRAO3        DET-2SIFRAO3
                      DET-SIFRAO4        DET-2SIFRAO4
                      DET-SIFRAO5        DET-SIFRAO5
                      DET-SIFRAO6        DET-2SIFRAO6
                      DET-SIFRAO7        DET-2SIFRAO7
                      DET-SIFRAO8        DET-2SIFRAO8
                      DET-SIFRAO9        DET-2SIFRAO9
                      DET-SIFRAO10       DET-2SIFRAO10
                      DET-SIFRAO11       DET-2SIFRAO11
                      DET-SIFRAO12       DET-2SIFRAO12
                      DET-SIFRAO13       DET-2SIFRAO13
                      DET-SIFRAO14       DET-2SIFRAO14

           IF GS-ETIQ-LINHA > 1
              MOVE 1 TO QTDE-LINHA
              PERFORM UNTIL QTDE-LINHA = GS-ETIQ-LINHA
                   ADD 1 TO QTDE-LINHA

                   MOVE SPACES               TO GS-LINDET
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   MOVE SPACES               TO GS-LINDET
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   MOVE ALL "-" TO GS-LINDET(1:89)
                   ADD 1       TO GS-LINHA
                   MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM
           END-IF

           MOVE GS-ETIQ-COLUNA     TO QUAL-COLUNA

           INITIALIZE REG-MTD020
           MOVE GS-CONTRATO        TO CONTRATO-MTG
           MOVE GS-ETIQ-ALBUM1     TO SEQ-MTG
           START MTD020 KEY IS NOT LESS ALBUM-MTG INVALID KEY
               MOVE "10" TO ST-MTD020.


           PERFORM UNTIL ST-MTD020 = "10"
               READ MTD020 NEXT AT END
                   MOVE "10" TO ST-MTD020
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-MTG OR
                      SEQ-MTG      > GS-ETIQ-ALBUM2
                      MOVE "10" TO ST-MTD020
                   ELSE
                      MOVE ALBUM-MTG            TO ALBUM-MT19
                      READ MTD019 INVALID KEY
                           MOVE "-------------" TO NOME-FORM-MT19
                      END-READ

                      IF GS-CURSO(27:3) = "000" OR CURSO-MT19
                         MOVE ZEROS TO VALOR-AVISTA
                         INITIALIZE REG-MTD020P
                         MOVE ALBUM-MTG   TO ALBUM-MTGP
                         START MTD020P KEY IS NOT LESS CHAVE-MTGP
                                                             INVALID KEY
                              MOVE "10" TO ST-MTD020P
                         END-START
                         PERFORM UNTIL ST-MTD020P = "10"
                              READ MTD020P NEXT AT END
                                   MOVE "10" TO ST-MTD020P
                              NOT AT END
                                   IF ALBUM-MTG <> ALBUM-MTGP
                                      MOVE "10" TO ST-MTD020P
                                   ELSE
                                      IF OP-QTDE-ZERO-RC109 = 1
                                         IF QTDE-PLANILHA-MTGP = 0
                                            MOVE 1 TO QTDE-PLANILHA-MTGP
                                         END-IF
                                      END-IF

                                      MOVE GS-CONTRATO      TO
                                           CONTRATO-RC109P
                                      MOVE GS-FAIXAS(45:6)  TO
                                           ESCALA-RC109P
                                      MOVE PRODUTO-MTGP     TO
                                           PRODUTO-RC109P
                                      READ RCD109P NOT INVALID KEY
                                         MOVE VALOR-BASE-RC109P TO
                                              AUX-VALOR-BASE-PRODUTO
                                         PERFORM CALCULAR-VALORES-FAIXAS
                                         COMPUTE VALOR-AVISTA =
                                                 VALOR-AVISTA +
                                                (QTDE-PLANILHA-MTGP *
                                                 AUX-VALOR-BASE-PRODUTO)
                                      END-READ
                                   END-IF
                              END-READ
                         END-PERFORM
                         MOVE 30                TO NUMERO-PARCELA
                         move zeros             to controle-linha

                         EVALUATE QUAL-COLUNA
                            WHEN 1 PERFORM DADOS-COLUNA1
                                   ADD 1  TO QUAL-COLUNA
                            WHEN 2 PERFORM DADOS-COLUNA2
                                   MOVE 1 TO QUAL-COLUNA
                                   MOVE ALL "-"   TO GS-LINDET(1:89)
                                   ADD  1         TO GS-LINHA
                                   MOVE "INSERIR-ETIQUETA"
                                     TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                         END-EVALUATE
                     END-IF
                   END-IF
               END-READ
           END-PERFORM

           ADD  1                    TO GS-LINHA
           MOVE "FIM"                TO GS-LINDET
           MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CALCULAR-VALORES-FAIXAS SECTION.
           INITIALIZE REG-RCD112
           MOVE FAIXA-RC109                TO CODIGO-RC112
           START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
               MOVE "10" TO ST-RCD112.

           PERFORM UNTIL ST-RCD112 = "10"
               READ RCD112 NEXT AT END
                   MOVE "10" TO ST-RCD112
               NOT AT END
                   IF FAIXA-RC109 <> CODIGO-RC112
                      MOVE "10" TO ST-RCD112
                   ELSE
                      IF PRODUTO-RC112 = PRODUTO-RC109P
                         COMPUTE AUX-VALOR-BASE-PRODUTO =
                                 AUX-VALOR-BASE-PRODUTO -
                                (AUX-VALOR-BASE-PRODUTO *
                                 DESC-PERC-RC112 / 100)
                      END-IF
                   END-IF
               END-READ
           END-PERFORM.

       DADOS-COLUNA1 SECTION.
           INITIALIZE DET-CONTRATO1      DET-IFEM1  DET-IFEM2
                      DET-ALBUM1
                      DET-FORMANDO1
                      DET-CONTRATO2
                      DET-ALBUM2
                      DET-FORMANDO2
                      DET-COND-PAGTO1    DET-2COND-PAGTO1
                      DET-VALOR-PAGTO1   DET-2VALOR-PAGTO1
                      DET-COND-PAGTO2    DET-2COND-PAGTO2
                      DET-VALOR-PAGTO2   DET-2VALOR-PAGTO2
                      DET-COND-PAGTO3    DET-2COND-PAGTO3
                      DET-VALOR-PAGTO3   DET-2VALOR-PAGTO3
                      DET-COND-PAGTO4    DET-2COND-PAGTO4
                      DET-VALOR-PAGTO4   DET-2VALOR-PAGTO4
                      DET-COND-PAGTO5    DET-2COND-PAGTO5
                      DET-VALOR-PAGTO5   DET-2VALOR-PAGTO5
                      DET-COND-PAGTO6    DET-2COND-PAGTO6
                      DET-VALOR-PAGTO6   DET-2VALOR-PAGTO6
                      DET-COND-PAGTO7    DET-2COND-PAGTO7
                      DET-VALOR-PAGTO7   DET-2VALOR-PAGTO7
                      DET-COND-PAGTO8    DET-2COND-PAGTO8
                      DET-VALOR-PAGTO8   DET-2VALOR-PAGTO8
                      DET-COND-PAGTO9    DET-2COND-PAGTO9
                      DET-VALOR-PAGTO9   DET-2VALOR-PAGTO9
                      DET-COND-PAGTO10   DET-2COND-PAGTO10
                      DET-VALOR-PAGTO10  DET-2VALOR-PAGTO10
                      DET-COND-PAGTO11   DET-2COND-PAGTO11
                      DET-VALOR-PAGTO11  DET-2VALOR-PAGTO11
                      DET-COND-PAGTO12   DET-2COND-PAGTO12
                      DET-VALOR-PAGTO12  DET-2VALOR-PAGTO12
                      DET-COND-PAGTO13   DET-2COND-PAGTO13
                      DET-VALOR-PAGTO13  DET-2VALOR-PAGTO13
                      DET-COND-PAGTO14   DET-2COND-PAGTO14
                      DET-VALOR-PAGTO14  DET-2VALOR-PAGTO14

                      DET-SIFRAO1        DET-2SIFRAO1
                      DET-SIFRAO2        DET-2SIFRAO2
                      DET-SIFRAO3        DET-2SIFRAO3
                      DET-SIFRAO4        DET-2SIFRAO4
                      DET-SIFRAO5        DET-SIFRAO5
                      DET-SIFRAO6        DET-2SIFRAO6
                      DET-SIFRAO7        DET-2SIFRAO7
                      DET-SIFRAO8        DET-2SIFRAO8
                      DET-SIFRAO9        DET-2SIFRAO9
                      DET-SIFRAO10       DET-2SIFRAO10
                      DET-SIFRAO11       DET-2SIFRAO11
                      DET-SIFRAO12       DET-2SIFRAO12
                      DET-SIFRAO13       DET-2SIFRAO13
                      DET-SIFRAO14       DET-2SIFRAO14


           MOVE "-"                      TO DET-IFEM1
           MOVE CONTRATO-MTG             TO DET-CONTRATO1
           MOVE SEQ-MTG                  TO DET-ALBUM1
           MOVE NOME-FORM-MT19           TO DET-FORMANDO1

           MOVE PLANO-PAGTO-RC109        TO CODIGO-RC110
           READ RCD110 INVALID KEY
               INITIALIZE REG-RCD110.

           MOVE 2                        TO CONTROLE-LINHA

           MOVE "À VISTA"                TO DET-COND-PAGTO1
           MOVE " R$"                    TO DET-SIFRAO1
           MOVE VALOR-AVISTA             TO DET-VALOR-PAGTO1


           INITIALIZE REG-RCD1101
           MOVE PLANO-PAGTO-RC109        TO CODIGO-RC1101
           START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
               MOVE "10" TO ST-RCD1101
           END-START
           PERFORM UNTIL ST-RCD1101 = "10"
               READ RCD1101 NEXT AT END
                   MOVE "10" TO ST-RCD1101
               NOT AT END
                   IF PLANO-PAGTO-RC109 <> CODIGO-RC1101
                      MOVE "10" TO ST-RCD1101
                   ELSE
                      EVALUATE JUROS-RC1101
                         WHEN "SIM" COMPUTE VALOR-JUROS ROUNDED =
                                    VALOR-AVISTA + (VALOR-AVISTA *
                                    GS-TAXA-JUROS / 100)
                                    MOVE VALOR-JUROS TO VALOR-AVISTA
                         WHEN "NÃO" COMPUTE VALOR-CALCULADO =
                                         VALOR-AVISTA
                                    MOVE VALOR-AVISTA TO VALOR-JUROS
                      END-EVALUATE
                      EVALUATE PARCELA-RC1101
                         WHEN "0 + 1"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO3
                              MOVE " R$"           TO DET-SIFRAO3
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 1
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO3
                         WHEN "1 + 1"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO3
                              MOVE " R$"           TO DET-SIFRAO3
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 2
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO3
                         WHEN "0 + 2"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO5
                              MOVE " R$"           TO DET-SIFRAO5
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 2
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO5
                         WHEN "1 + 2"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO5
                              MOVE " R$"           TO DET-SIFRAO5
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 3
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO5
                         WHEN "0 + 3"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO7
                              MOVE " R$"           TO DET-SIFRAO7
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 3
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO7
                         WHEN "1 + 3"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO7
                              MOVE " R$"           TO DET-SIFRAO7
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 4
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO7
                         WHEN "0 + 4"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO9
                              MOVE " R$"           TO DET-SIFRAO9
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 4
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO9
                         WHEN "1 + 4"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO9
                              MOVE " R$"           TO DET-SIFRAO9
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 5
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO9
                         WHEN "0 + 5"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO11
                              MOVE " R$"           TO DET-SIFRAO11
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 5
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO11
                         WHEN "1 + 5"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO11
                              MOVE " R$"           TO DET-SIFRAO11
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 6
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO11
                         WHEN "0 + 6"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO13
                              MOVE " R$"           TO DET-SIFRAO13
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 6
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO13
                         WHEN "1 + 6"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO13
                              MOVE " R$"           TO DET-SIFRAO13
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 7
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO13
                         WHEN "0 + 7"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO2
                              MOVE " R$"           TO DET-SIFRAO2
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 7
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO2
                         WHEN "1 + 7"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO2
                              MOVE " R$"           TO DET-SIFRAO2
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 8
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO2
                         WHEN "0 + 8"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO4
                              MOVE " R$"           TO DET-SIFRAO4
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 8
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO4
                         WHEN "1 + 8"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO4
                              MOVE " R$"           TO DET-SIFRAO4
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 9
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO4
                         WHEN "0 + 9"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO6
                              MOVE " R$"           TO DET-SIFRAO6
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 9
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO6
                         WHEN "1 + 9"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO6
                              MOVE " R$"           TO DET-SIFRAO6
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 10
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO6
                         WHEN "0 + 10"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO8
                              MOVE " R$"           TO DET-SIFRAO8
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 10
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO8
                         WHEN "1 + 10"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO8
                              MOVE " R$"           TO DET-SIFRAO8
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 11
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO8
                         WHEN "0 + 11"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO10
                              MOVE " R$"           TO DET-SIFRAO10
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 11
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO10
                         WHEN "1 + 11"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO10
                              MOVE " R$"           TO DET-SIFRAO10
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 12
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO10
                         WHEN "0 + 12"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO12
                              MOVE " R$"           TO DET-SIFRAO12
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 12
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO12
                         WHEN "1 + 12"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO12
                              MOVE " R$"           TO DET-SIFRAO12
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 13
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO12
                         WHEN "0 + 13"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO14
                              MOVE " R$"           TO DET-SIFRAO14
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 13
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO14
                         WHEN "1 + 13"
                              MOVE PARCELA-RC1101  TO DET-COND-PAGTO14
                              MOVE " R$"           TO DET-SIFRAO14
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 14
                              MOVE VALOR-CALCULADO TO DET-VALOR-PAGTO14
                      END-EVALUATE
                   END-IF
               END-READ
           END-PERFORM.

       DADOS-COLUNA2 SECTION.
           ADD  1                    TO CONTROLE-LINHA

           MOVE "-"                      TO DET-IFEM2
           MOVE CONTRATO-MTG         TO DET-CONTRATO2
           MOVE SEQ-MTG              TO DET-ALBUM2
           MOVE NOME-FORM-MT19       TO DET-FORMANDO2

           MOVE DET-1LINHA-ETIQ      TO GS-LINDET
           ADD 1                     TO GS-LINHA

           MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES               TO GS-LINDET
           ADD 1                     TO GS-LINHA

           MOVE "INSERIR-ETIQUETA"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE PLANO-PAGTO-RC109    TO CODIGO-RC110
           READ RCD110 INVALID KEY
               INITIALIZE REG-RCD110.

           MOVE "À VISTA"                TO DET-2COND-PAGTO1
           MOVE " R$"                    TO DET-2SIFRAO1
           MOVE VALOR-AVISTA             TO DET-2VALOR-PAGTO1


           INITIALIZE REG-RCD1101
           MOVE PLANO-PAGTO-RC109        TO CODIGO-RC1101
           START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
               MOVE "10" TO ST-RCD1101
           END-START
           PERFORM UNTIL ST-RCD1101 = "10"
               READ RCD1101 NEXT AT END
                   MOVE "10" TO ST-RCD1101
               NOT AT END
                   IF PLANO-PAGTO-RC109 <> CODIGO-RC1101
                      MOVE "10" TO ST-RCD1101
                   ELSE
                      EVALUATE JUROS-RC1101
                         WHEN "SIM" COMPUTE VALOR-JUROS ROUNDED =
                                    VALOR-AVISTA + (VALOR-AVISTA *
                                    GS-TAXA-JUROS / 100)
                                    MOVE VALOR-JUROS TO VALOR-AVISTA
                         WHEN "NÃO" COMPUTE VALOR-CALCULADO =
                                         VALOR-AVISTA
                                    MOVE VALOR-AVISTA TO VALOR-JUROS
                      END-EVALUATE
                      EVALUATE PARCELA-RC1101
                         WHEN "0 + 1"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO3
                              MOVE " R$"           TO DET-2SIFRAO3
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 1
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO3
                         WHEN "1 + 1"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO3
                              MOVE " R$"           TO DET-2SIFRAO3
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 2
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO3
                         WHEN "0 + 2"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO5
                              MOVE " R$"           TO DET-2SIFRAO5
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 2
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO5
                         WHEN "1 + 2"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO5
                              MOVE " R$"           TO DET-2SIFRAO5
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 3
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO5
                         WHEN "0 + 3"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO7
                              MOVE " R$"           TO DET-2SIFRAO7
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 3
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO7
                         WHEN "1 + 3"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO7
                              MOVE " R$"           TO DET-2SIFRAO7
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 4
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO7
                         WHEN "0 + 4"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO9
                              MOVE " R$"           TO DET-2SIFRAO9
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 4
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO9
                         WHEN "1 + 4"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO9
                              MOVE " R$"           TO DET-2SIFRAO9
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 5
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO9
                         WHEN "0 + 5"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO11
                              MOVE " R$"           TO DET-2SIFRAO11
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 5
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO11
                         WHEN "1 + 5"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO11
                              MOVE " R$"           TO DET-2SIFRAO11
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 6
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO11
                         WHEN "0 + 6"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO13
                              MOVE " R$"           TO DET-2SIFRAO13
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 6
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO13
                         WHEN "1 + 6"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO13
                              MOVE " R$"           TO DET-2SIFRAO13
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 7
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO13
                         WHEN "0 + 7"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO2
                              MOVE " R$"           TO DET-2SIFRAO2
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 7
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO2
                         WHEN "1 + 7"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO2
                              MOVE " R$"           TO DET-2SIFRAO2
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 8
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO2
                         WHEN "0 + 8"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO4
                              MOVE " R$"           TO DET-2SIFRAO4
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 8
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO4
                         WHEN "1 + 8"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO4
                              MOVE " R$"           TO DET-2SIFRAO4
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 9
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO4
                         WHEN "0 + 9"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO6
                              MOVE " R$"           TO DET-2SIFRAO6
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 9
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO6
                         WHEN "1 + 9"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO6
                              MOVE " R$"           TO DET-2SIFRAO6
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 10
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO6
                         WHEN "0 + 10"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO8
                              MOVE " R$"           TO DET-2SIFRAO8
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 10
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO8
                         WHEN "1 + 10"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO8
                              MOVE " R$"           TO DET-2SIFRAO8
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 11
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO8
                         WHEN "0 + 11"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO10
                              MOVE " R$"           TO DET-2SIFRAO10
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 11
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO10
                         WHEN "1 + 11"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO10
                              MOVE " R$"           TO DET-2SIFRAO10
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 12
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO10
                         WHEN "0 + 12"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO12
                              MOVE " R$"           TO DET-2SIFRAO12
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 12
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO12
                         WHEN "1 + 12"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO12
                              MOVE " R$"           TO DET-2SIFRAO12
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 13
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO12
                         WHEN "0 + 13"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO14
                              MOVE " R$"           TO DET-2SIFRAO14
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 13
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO14
                         WHEN "1 + 13"
                              MOVE PARCELA-RC1101  TO DET-2COND-PAGTO14
                              MOVE " R$"           TO DET-2SIFRAO14
                              COMPUTE VALOR-CALCULADO ROUNDED =
                                      VALOR-JUROS / 14
                              MOVE VALOR-CALCULADO TO DET-2VALOR-PAGTO14
                      END-EVALUATE
                   END-IF
               END-READ
           END-PERFORM.

           MOVE DET-2LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-3LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-4LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-5LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-6LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-7LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE DET-8LINHA-ETIQ    TO GS-LINDET
           ADD 1                   TO GS-LINHA
           MOVE "INSERIR-ETIQUETA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



       ALTERAR-PRODUTO SECTION.
           MOVE GS-VALOR-BASE           TO DET-VALOR-BASE

           move 4                       to indice
           initialize wsTexto
           string det-valor-base X"00"  into wsTexto
           invoke gs-listview-produtos "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           INITIALIZE REG-RCD112
           MOVE GS-FAIXAS(45:6)         TO CODIGO-RC112
           START RCD112 KEY IS NOT LESS CHAVE-RC112 INVALID KEY
               MOVE "10" TO ST-RCD112.


           PERFORM UNTIL ST-RCD112 = "10"
               READ RCD112 NEXT AT END
                   MOVE "10" TO ST-RCD112
               NOT AT END
                   IF GS-FAIXAS(45:6) <> CODIGO-RC112
                      MOVE "10" TO ST-RCD112
                   ELSE
                      IF GS-COD-PRODUTO = PRODUTO-RC112
                         add 1  to indice
                         COMPUTE DET-FAIXA1 = GS-VALOR-BASE -
                                (GS-VALOR-BASE * DESC-PERC-RC112 / 100)
                         initialize wsTexto
                         string det-faixa1 X"00" into wsTexto
                         invoke gs-listview-produtos "preencherColunaZ"
                           using wsItem lnkcolunasPro(indice) wsTexto
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.

       LER-PRODUTOS SECTION.
           INVOKE GS-LISTVIEW-PRODUTOS "DELETEALL"

           INITIALIZE REG-CADPRO
           START CADPRO KEY IS NOT LESS CADPRO-CH-NOME INVALID KEY
                 MOVE "10" TO ST-CADPRO.

           PERFORM UNTIL ST-CADPRO = "10"
                 READ CADPRO NEXT AT END
                      MOVE "10" TO ST-CADPRO
                 NOT AT END
                      initialize indice
                      invoke gs-listview-produtos "adicionarItem"
                                                        returning wsItem

                      add 1 to indice
                      initialize wsTexto
                      string cadpro-codigo X"00"  into wsTexto
                      invoke gs-listview-produtos "preencherColunaZ"
                        using wsItem lnkcolunasPro(indice) wsTexto


                      add 1 to indice
                      initialize wsTexto
                      string cadpro-nome X"00"  into wsTexto
                      invoke gs-listview-produtos "preencherColunaZ"
                        using wsItem lnkcolunasPro(indice) wsTexto


                      move cadpro-modelo to cadmod-codigo
                      read cadmod invalid key
                           move "******" to cadmod-nome
                      end-read

                      add 1 to indice
                      initialize wsTexto
                      string cadmod-nome X"00"  into wsTexto
                      invoke gs-listview-produtos "preencherColunaZ"
                        using wsItem lnkcolunasPro(indice) wsTexto

                 END-READ
           END-PERFORM

           invoke gs-listview-produtos "Size" returning wsSize
           if wsSize > 0
              invoke gs-listview-produtos "enable"
              invoke gs-listview-produtos "Setfocus"
              move 1 to wsItem
              invoke gs-listview-produtos "itemAtIndex" using wsItem
                                                    returning umItem
              invoke umItem "SetSelected".

           perform mostrar-colunas-favo
           perform mostrar-fonte-favo
           perform zebrar-itens.


       ALTERAR-ESCALA SECTION.
           MOVE GS-LINDET             TO DET-ESCALAS
           MOVE DET-ESCALA            TO GS-ESCALA

           MOVE GS-CONTRATO           TO CONTRATO-RC109
           MOVE GS-ESCALA             TO ESCALA-RC109
           READ RCD109 INVALID KEY
               MOVE "Escala Não Encontrada" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE 1                    TO GS-GRAVA
               MOVE GS-CONTRATO          TO NR-CONTRATO-CO40
               READ COD040 INVALID KEY
                   MOVE "Contrato Não Encontrado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               NOT INVALID KEY
                    PERFORM CONT-CARREGAR-DADOS
               END-READ
               MOVE CURSO-RC109          TO CODIGO-IE11
               IF CODIGO-IE11 > 0
                  READ IED011 INVALID KEY
                       MOVE SPACES TO NOME-IE11
                  END-READ
               ELSE
                  MOVE "TODOS"           TO NOME-IE11
               END-IF
               MOVE NOME-IE11            TO AUX-CURSO(1:25)
               MOVE CURSO-RC109          TO AUX-CURSO(27:3)
               MOVE AUX-CURSO            TO GS-CURSO
               MOVE DATA-VIGOR-RC109     TO GS-DATA-VIGOR
               MOVE DATA-LIBERADO-RC109  TO GS-DATA-LIBERACAO
               MOVE TAXA-JURO-RC109      TO GS-TAXA-JUROS
               MOVE FAIXA-RC109          TO CODIGO-RC111
               READ RCD111 INVALID KEY
                   MOVE "Faixa Não Encontrada" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               NOT INVALID KEY
                   MOVE DESCRICAO-RC111  TO GS-FAIXAS
                   MOVE CODIGO-RC111     TO GS-FAIXAS(45:6)
               END-READ
               MOVE OP-QTDE-ZERO-RC109   TO GS-OP-PRODUTO-ZERO
               MOVE PLANO-PAGTO-RC109    TO CODIGO-RC110
               READ RCD110 INVALID KEY
                   MOVE "Plano de Pagamento Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               NOT INVALID KEY
                   MOVE DESCRICAO-RC110    TO GS-PLANO-PAGTO
                   MOVE CODIGO-RC110       TO GS-PLANO-PAGTO(45:6)

                   MOVE SPACES             TO GS-DESC-PAGTO

                   INITIALIZE REG-RCD1101
                   MOVE CODIGO-RC110       TO CODIGO-RC1101
                   START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID
                   KEY
                       MOVE "10" TO ST-RCD1101
                   END-START
                   PERFORM UNTIL ST-RCD1101 = "10"
                       READ RCD1101 NEXT AT END
                           MOVE "10" TO ST-RCD1101
                       NOT AT END
                           IF CODIGO-RC110 <> CODIGO-RC1101
                              MOVE "10" TO ST-RCD1101
                           ELSE
                              STRING GS-DESC-PAGTO "  " PARCELA-RC1101
                              DELIMITED BY "   " INTO GS-DESC-PAGTO
                           END-IF
                       END-READ
                   END-PERFORM
               END-READ


               invoke gs-listview-produtos "deleteall"

               initialize reg-rcd109p
               move contrato-rc109 to contrato-rc109p
               move escala-rc109   to escala-rc109p
               start rcd109p key is not less chave-rc109p invalid key
                     move "10" to st-rcd109p
               end-start
               perform until st-rcd109p = "10"
                     read rcd109p next at end
                          move "10" to st-rcd109p
                     not at end
                          if contrato-rc109   <> contrato-rc109p or
                             escala-rc109     <> escala-rc109p
                             move "10" to st-rcd109p
                          else
                             initialize indice
                             invoke gs-listview-produtos "adicionarItem"
                                                        returning wsItem

                             move produto-rc109p to cadpro-codigo
                             read cadpro invalid key
                                  move "********" to cadpro-nome
                                  move zeros      to cadpro-modelo
                             end-read

                             add 1 to indice
                             initialize wsTexto
                             string cadpro-codigo X"00"  into wsTexto
                             invoke gs-listview-produtos
                                                      "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto


                             add 1 to indice
                             initialize wsTexto
                             string cadpro-nome X"00"  into wsTexto
                             invoke gs-listview-produtos
                                                      "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto


                             move cadpro-modelo to cadmod-codigo
                             read cadmod invalid key
                                  move "******" to cadmod-nome
                             end-read

                             add 1 to indice
                             initialize wsTexto
                             string cadmod-nome X"00"  into wsTexto
                             invoke gs-listview-produtos
                                                      "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto

                             add 1 to indice
                             initialize wsTexto
                             move valor-base-rc109p to det-valor-base
                             string det-valor-base X"00"  into wsTexto
                             invoke gs-listview-produtos
                                                      "preencherColunaZ"
                              using wsItem lnkcolunasPro(indice) wsTexto


                             initialize reg-rcd112
                             move escala-rc109p to codigo-rc112
                             start rcd112 key is not less chave-rc112
                                                             invalid key
                                   move "10" to st-rcd112
                             end-start
                             perform until st-rcd112 = "10"
                                   read rcd112 next at end
                                        move "10" to st-rcd112
                                   not at end
                                        if escala-rc109p <> codigo-rc112
                                           move "10" to st-rcd112
                                        else
                                           if produto-rc109p =
                                                           produto-rc112
                                              add 1  to indice
                                              compute det-faixa1 =
                                                     valor-base-rc109p -
                                                    (valor-base-rc109p *
                                                  desc-perc-rc112 / 100)
                                              initialize wsTexto
                                              string det-faixa1 X"00"
                                                            into wsTexto
                                             invoke gs-listview-produtos
                                                      "preencherColunaZ"
                                             using wsItem
                                                   lnkcolunasPro(indice)
                                                   wsTexto
                                           end-if
                                        end-if
                                   end-read
                             end-perform
                          END-IF
                     END-READ
               END-PERFORM

               perform mostrar-colunas-favo
               perform mostrar-fonte-favo
               perform zebrar-itens.

       LER-PLANO SECTION.
           MOVE GS-PLANO-PAGTO(45:6)  TO CODIGO-RC110
           READ RCD110 INVALID KEY
               MOVE "Plano de Pagamento Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE SPACES             TO GS-DESC-PAGTO

               INITIALIZE REG-RCD1101
               MOVE CODIGO-RC110       TO CODIGO-RC1101
               START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID KEY
                   MOVE "10" TO ST-RCD1101
               END-START
               PERFORM UNTIL ST-RCD1101 = "10"
                   READ RCD1101 NEXT AT END
                       MOVE "10" TO ST-RCD1101
                   NOT AT END
                       IF CODIGO-RC110 <> CODIGO-RC1101
                          MOVE "10" TO ST-RCD1101
                       ELSE
                         STRING GS-DESC-PAGTO "  " PARCELA-RC1101
                            DELIMITED BY "   " INTO GS-DESC-PAGTO
                       END-IF
                   END-READ
               END-PERFORM.

       VALIDA-DT-VIGOR SECTION.
           CALL   "UTIVLDT" USING GS-DATA-VIGOR WS-OK
           CANCEL "UTIVLDT"
           IF WS-OK EQUAL "N"
              MOVE "Data de Vigor Inválida" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       VALIDA-DT-LIBER SECTION.
           CALL   "UTIVLDT" USING GS-DATA-LIBERACAO WS-OK
           CANCEL "UTIVLDT"
           IF WS-OK EQUAL "N"
              MOVE "Data de Liberação Inválida" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.


       POPUP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.

       VERIFICA-CODIGO SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "APAGAR-CURSO"   TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-CONTRATO      TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
               MOVE "Contrato Não Encontrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                PERFORM CONT-CARREGAR-DADOS
           END-READ.

           MOVE "APAGAR-FAIXA"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-RCD111
                      gs-linha
           START RCD111 KEY IS NOT LESS CODIGO-RC111 INVALID KEY
               MOVE "10" TO ST-RCD111.

           PERFORM UNTIL ST-RCD111 = "10"
               READ RCD111 NEXT AT END
                   MOVE "10" TO ST-RCD111
               NOT AT END
                   MOVE DESCRICAO-RC111    TO GS-FAIXAS
                   MOVE CODIGO-RC111       TO GS-FAIXAS(45:6)
                   add 1 to gs-linha
                   MOVE "INSERIR-FAIXA"    TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM

           MOVE ZEROS             TO GS-LINHA

           MOVE "APAGAR-ESCALAS"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           ADD  1                 TO GS-LINHA
           MOVE DET-D-ESCALAS     TO GS-LINDET
           MOVE "INSERIR-ESCALAS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           ADD  1                 TO GS-LINHA
           MOVE DET-C-ESCALAS     TO GS-LINDET
           MOVE "INSERIR-ESCALAS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-RCD109
           MOVE GS-CONTRATO      TO CONTRATO-RC109
           START RCD109 KEY IS NOT LESS CHAVE-RC109 INVALID KEY
               MOVE "10" TO ST-RCD109.

           PERFORM UNTIL ST-RCD109 = "10"
               READ RCD109 NEXT AT END
                   MOVE "10" TO ST-RCD109
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-RC109
                      MOVE "10" TO ST-RCD109
                   ELSE
                      ADD  1                 TO GS-LINHA

                      MOVE ESCALA-RC109      TO DET-ESCALA
                      MOVE DATA-VIGOR-RC109  TO DET-DATA-VIGOR
                      MOVE CURSO-RC109       TO CODIGO-IE11
                      IF CODIGO-IE11 > 0
                         READ IED011 INVALID KEY
                              MOVE SPACES TO NOME-IE11
                         END-READ
                      ELSE
                         MOVE "TODOS"        TO NOME-IE11
                      END-IF
                      MOVE NOME-IE11         TO DET-CURSO
                      MOVE FAIXA-RC109       TO CODIGO-RC111
                      READ RCD111 INVALID KEY
                           MOVE "-----"         TO DET-FAIXA-DESC
                      NOT INVALID KEY
                           MOVE DESCRICAO-RC111 TO DET-FAIXA-DESC
                      END-READ
                      MOVE PLANO-PAGTO-RC109 TO CODIGO-RC110
                      READ RCD110 INVALID KEY
                           MOVE SPACES       TO DESCRICAO-RC110
                      END-READ
                      MOVE DESCRICAO-RC110   TO DET-PLANO-PAGTO

                      MOVE DET-ESCALAS       TO GS-LINDET
                      MOVE "INSERIR-ESCALAS" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM

           MOVE "APAGAR-PAGTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-RCD110 GS-LINHA
           START RCD110 KEY IS NOT LESS CODIGO-RC110 INVALID KEY
               MOVE "10" TO ST-RCD110.

           PERFORM UNTIL ST-RCD110 = "10"
               READ RCD110 NEXT AT END
                   MOVE "10" TO ST-RCD110
               NOT AT END
                   MOVE DESCRICAO-RC110    TO GS-PLANO-PAGTO(1:44)
                   MOVE CODIGO-RC110       TO GS-PLANO-PAGTO(45:6)

                   MOVE SPACES TO GS-DESC-PAGTO

                   INITIALIZE REG-RCD1101
                   MOVE CODIGO-RC110       TO CODIGO-RC1101
                   START RCD1101 KEY IS NOT LESS CHAVE-RC1101 INVALID
                   KEY
                       MOVE "10" TO ST-RCD1101
                   END-START
                   PERFORM UNTIL ST-RCD1101 = "10"
                       READ RCD1101 NEXT AT END
                           MOVE "10" TO ST-RCD1101
                       NOT AT END
                           IF CODIGO-RC110 <> CODIGO-RC1101
                              MOVE "10" TO ST-RCD1101
                           ELSE
                              STRING GS-DESC-PAGTO "  " PARCELA-RC1101
                               DELIMITED BY "   " INTO GS-DESC-PAGTO
                           END-IF
                       END-READ
                   END-PERFORM
                   ADD 1 TO GS-LINHA

                   MOVE "INSERIR-PAGTO"    TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

               END-READ
           END-PERFORM.

       CONT-CARREGAR-DADOS SECTION.
           MOVE QTDE-TURMAS-CO40       TO GS-QTD-TURMAS
           MOVE IDENTIFICACAO-CO40     TO GS-IDENTIFICACAO
           MOVE INSTITUICAO-CO40       TO CODIGO-IE10
           READ IED010 INVALID KEY
                MOVE SPACES            TO SIGLA-IE10
                MOVE SPACES            TO NOME-IE10.
           MOVE SPACES                 TO GS-INSTITUICAO
           MOVE NOME-IE10(1:15)        TO GS-INSTITUICAO
           MOVE SIGLA-IE10             TO GS-INSTITUICAO(17:3)
           MOVE CIDADE-CO40            TO CIDADE
           READ CAD010 INVALID KEY
                INITIALIZE REG-CAD010.
           MOVE SPACES                 TO GS-CIDADE
           MOVE NOME-COMPL-CID(1:17)   TO GS-CIDADE
           MOVE UF-CID                 TO GS-CIDADE(19:2)
           MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
           MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
           MOVE MESANO-W               TO GS-MESANO
           MOVE QTDE-FORM-CO40         TO GS-FORM-FINAL
           MOVE QTDE-FORM-INI-CO40     TO GS-FORM-INICAL

           PERFORM CARREGAR-DADOS-CURSOS.

       CARREGAR-DADOS-CURSOS SECTION.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO41.
           MOVE ZEROS       TO CURSO-CO41 GS-LINHA
           MOVE SPACES      TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           PERFORM UNTIL ST-COD041 = "10"
              READ COD041 NEXT RECORD AT END
                   MOVE "10" TO ST-COD041
              NOT AT END
                  IF NR-CONTRATO-CO41 <> GS-CONTRATO
                     MOVE "10" TO ST-COD041
                  ELSE
                    MOVE SPACES        TO GS-LINDET
                    MOVE CURSO-CO41    TO CODIGO-IE11
                    IF CODIGO-IE11 > 0
                       READ IED011 INVALID KEY
                            MOVE SPACES TO NOME-IE11
                       END-READ
                    ELSE
                       MOVE "TODOS"           TO NOME-IE11
                    END-IF
                    MOVE NOME-IE11            TO GS-CURSO(1:25)
                    MOVE CURSO-CO41           TO GS-CURSO(27:3)
                    ADD  1                    TO GS-LINHA
                    MOVE "INSERIR-CURSO"      TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM
           MOVE "TODOS"              TO GS-CURSO(1:25)
           MOVE 000                  TO GS-CURSO(27:3)
           ADD  1                    TO GS-LINHA
           MOVE "INSERIR-CURSO"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to gs-flag-critica
           move spaces to mensagem.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE 1 TO ERRO-W.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LIMPAR-DADOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-RCD111
                      REG-RCD112

           INVOKE GS-LISTVIEW-PRODUTOS "DELETEALL"

           MOVE GS-ORDER TO ORDEM-W
           INITIALIZE GS-DATA-BLOCK
           MOVE ORDEM-W TO GS-ORDER
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       EXCLUI-RECORD SECTION.
           CLOSE      RCD109 RCD109P
           OPEN I-O   RCD109 RCD109P

           DELETE RCD109 NOT INVALID KEY
                  PERFORM EXCLUIR-RCD109P
           END-DELETE

           CLOSE      RCD109 RCD109P
           OPEN INPUT RCD109 RCD109P
           PERFORM LIMPAR-DADOS.

       SALVAR-DADOS SECTION.
           CLOSE      RCD109 RCD109P
           OPEN I-O   RCD109 RCD109P

           MOVE GS-CONTRATO          TO CONTRATO-RC109
           MOVE GS-ESCALA            TO ESCALA-RC109
           MOVE GS-CURSO(27:3)       TO CURSO-RC109
           MOVE GS-DATA-VIGOR        TO DATA-VIGOR-RC109
           MOVE GS-TAXA-JUROS        TO TAXA-JURO-RC109
           MOVE GS-DATA-LIBERACAO    TO DATA-LIBERADO-RC109
           MOVE GS-FAIXAS(45:6)      TO FAIXA-RC109
           MOVE GS-OP-PRODUTO-ZERO   TO OP-QTDE-ZERO-RC109
           MOVE GS-PLANO-PAGTO(45:6) TO PLANO-PAGTO-RC109

           IF GS-GRAVA = 0
              WRITE REG-RCD109 INVALID KEY
                  MOVE "Erro de Gravação...RCD109" TO MENSAGEM
                  MOVE "C"                         TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                  PERFORM EXCLUIR-RCD109P
                  PERFORM GRAVAR-RCD109P
              END-WRITE
           ELSE
              REWRITE REG-RCD109 INVALID KEY
                  MOVE "Erro de Regravação...RCD109" TO MENSAGEM
                  MOVE "C"                         TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                  PERFORM EXCLUIR-RCD109P
                  PERFORM GRAVAR-RCD109P
              END-REWRITE
           END-IF

           CLOSE      RCD109 RCD109P
           OPEN INPUT RCD109 RCD109P.

       EXCLUIR-RCD109P SECTION.
           INITIALIZE REG-RCD109P
           MOVE CONTRATO-RC109          TO CONTRATO-RC109P
           MOVE ESCALA-RC109            TO ESCALA-RC109P
           START RCD109P KEY IS NOT LESS CHAVE-RC109P INVALID KEY
                MOVE "10" TO ST-RCD109P.

           PERFORM UNTIL ST-RCD109P = "10"
                READ RCD109P NEXT AT END
                     MOVE "10" TO ST-RCD109P
                NOT AT END
                     IF CONTRATO-RC109 <> CONTRATO-RC109P OR
                        ESCALA-RC109   <> ESCALA-RC109P
                        MOVE "10"      TO ST-RCD109P
                     ELSE
                        DELETE RCD109P INVALID KEY
                            MOVE "Erro de Exclusão...RCD109P" TO
                            MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM.

       GRAVAR-RCD109P SECTION.
           initialize wsindice
                      wssize

           invoke gs-listview-produtos "Size" returning wsSize
           perform wsSize times
              add 1 to wsindice
              invoke gs-listview-produtos "ItemAtIndex" using wsindice
                                                    returning umItem

              initialize reg-rcd109p

              move gs-contrato                  to contrato-rc109p
              move gs-escala                    to escala-rc109p

              initialize indice

              add 1 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to produto-rc109p


              move 4 to indice
              initialize wstexto
              invoke umItem "GetColumnValue" using lnkcolunasPro(indice)
                                         returning umObjeto
              invoke umObjeto "getvalue" returning wstexto
              move function numval(wstexto)     to valor-base-rc109p

              write reg-rcd109p invalid key
                    move "Erro de Gravação...RCD109P" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem
              end-write
           end-perform.


       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-ULTIMO SECTION.
           INITIALIZE REG-RCD109
           MOVE GS-CONTRATO TO CONTRATO-RC109
           MOVE ALL "9"     TO ESCALA-RC109
           START RCD109 KEY IS LESS THAN CHAVE-RC109 INVALID KEY
               MOVE 0 TO GS-ESCALA
           NOT INVALID KEY
               READ RCD109 PREVIOUS AT END
                   MOVE 0 TO GS-ESCALA
               NOT AT END
                   IF GS-CONTRATO <> CONTRATO-RC109
                      MOVE 0 TO GS-ESCALA
                   ELSE
                      MOVE ESCALA-RC109 TO GS-ESCALA
                   END-IF
               END-READ
           END-START
           ADD 1 TO GS-ESCALA.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-RCD109       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP109"    TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 COD041 RCD109 RCD111 RCD112 IED010 IED011
                 RCD110 MTD019 RCD1101 CADPRO CADMOD RCD109P MTD020P
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
