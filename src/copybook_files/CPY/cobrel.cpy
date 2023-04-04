      *
      * Vari†veis utilizadas em cada objeto de impress∆o
      *
       01  CR-PAGINA.
           03  CRP-TIPO                PIC  9(01).
           03  CRP-MEDIDA              PIC  9(01).
           03  CRP-MARGEM-SUP          PIC  9(04)V9.
           03  CRP-MARGEM-ESQ          PIC  9(04)V9.
           03  CRP-MARGEM-INF          PIC  9(04)V9.
           03  CRP-MARGEM-DIR          PIC  9(04)V9.
           03  CRP-ORIENTACAO          PIC  X(01).
           03  CRP-PAPEL               PIC  9(02).
           03  CRP-ALTURA              PIC  9(04)V9.
           03  CRP-LARGURA             PIC  9(04)V9.
           03  CRP-DOCUMENTO           PIC  X(64).
           03  CRP-DIRETORIO           PIC  X(50).
           03  CRP-EMAIL               PIC  X(50).
      *
       01  CR-LABEL.
           03  CRL-TIPO                PIC  9(01).
           03  CRL-TEXTO               PIC  X(128).
           03  CRL-SUPERIOR            PIC  9(04)V9.
           03  CRL-ESQUERDA            PIC  9(04)V9.
           03  CRL-FONTE               PIC  X(64).
           03  CRL-TAM-FONTE           PIC  9(03).
           03  CRL-R                   PIC  9(03).
           03  CRL-G                   PIC  9(03).
           03  CRL-B                   PIC  9(03).
           03  CRL-NEGRITO             PIC  9(01).
           03  CRL-ITALICO             PIC  9(01).
           03  CRL-SUBLINHADO          PIC  9(01).
           03  CRL-RISCADO             PIC  9(01).
           03  CRL-TRANSPARENTE        PIC  9(01).
           03  CRL-LARGURA             PIC  9(04)V9.
           03  CRL-ALINHAMENTO         PIC  9(01).
      *
       01  CR-SHAPE.
           03  CRS-TIPO                PIC  9(01).
           03  CRS-SHAPE               PIC  9(01).
           03  CRS-POS-SUP             PIC  9(04)V9.
           03  CRS-POS-ESQ             PIC  9(04)V9.
           03  CRS-POS-ALT             PIC  9(04)V9.
           03  CRS-POS-LARG            PIC  9(04)V9.
           03  CRS-SHAPE-R             PIC  9(03).
           03  CRS-SHAPE-G             PIC  9(03).
           03  CRS-SHAPE-B             PIC  9(03).
           03  CRS-INTERIOR            PIC  9(01).
           03  CRS-TRACO-R             PIC  9(03).
           03  CRS-TRACO-G             PIC  9(03).
           03  CRS-TRACO-B             PIC  9(03).
           03  CRS-PONTOS              PIC  9(01).
      *
       01  CR-FIGURA.
           03  CRF-TIPO                PIC  9(01).
           03  CRF-TIPO-VAR            PIC  9(01).
           03  CRF-POS-SUP             PIC  9(04)V9.
           03  CRF-POS-ESQ             PIC  9(04)V9.
           03  CRF-POS-ALT             PIC  9(04)V9.
           03  CRF-POS-LARG            PIC  9(04)V9.
           03  CRF-AUTO                PIC  9(01).
           03  CRF-AJUSTADO            PIC  9(01).
           03  CRF-TIPO-IMG            PIC  9(01).

       01  CR-FIGURA2.
           03  CRF2-TIPO               PIC  9(01).
           03  CRF2-PATH               PIC  X(128).
           03  CRF2-POS-SUP            PIC  9(04)V9.
           03  CRF2-POS-ESQ            PIC  9(04)V9.
           03  CRF2-POS-ALT            PIC  9(04)V9.
           03  CRF2-POS-LARG           PIC  9(04)V9.
           03  CRF2-AUTO               PIC  9(01).
           03  CRF2-AJUSTADO           PIC  9(01).

      *
       01  CR-VARIAVEL.
           03  CRV-TIPO                PIC  9(01).
           03  CRV-VARIAVEL            PIC  9(01).
           03  CRV-COMPLEMENTO         PIC  X(1000).
      *
       01  CR-COMPLEMENTO-LABEL.
           03  CRCL-TAMANHO            PIC  9(03).
           03  CRCL-SUPERIOR           PIC  9(04)V9.
           03  CRCL-ESQUERDA           PIC  9(04)V9.
           03  CRCL-FONTE              PIC  X(64).
           03  CRCL-TAM-FONTE          PIC  9(03).
           03  CRCL-R                  PIC  9(03).
           03  CRCL-G                  PIC  9(03).
           03  CRCL-B                  PIC  9(03).
           03  CRCL-NEGRITO            PIC  9(01).
           03  CRCL-ITALICO            PIC  9(01).
           03  CRCL-SUBLINHADO         PIC  9(01).
           03  CRCL-RISCADO            PIC  9(01).
           03  CRCL-TRANSPARENTE       PIC  9(01).
           03  CRCL-LARGURA            PIC  9(04)V9.
           03  CRCL-ALINHAMENTO        PIC  9(01).
      *
       01  CR-COMPLEMENTO-COD-BARRA.
           03  CRCB-TAMANHO            PIC  9(02).
           03  CRCB-TIPO               PIC  9(02).
           03  CRCB-POS-SUP            PIC  9(04)V9.
           03  CRCB-POS-ESQ            PIC  9(04)V9.
           03  CRCB-POS-ALT            PIC  9(04)V9.
           03  CRCB-R                  PIC  9(03).
           03  CRCB-G                  PIC  9(03).
           03  CRCB-B                  PIC  9(03).
           03  CRCB-TEXTO              PIC  9(01).
           03  CRCB-TRANSPARENTE       PIC  9(01).
           03  CRCB-AUTO               PIC  9(01).
      *
       01  CR-COMPLEMENTO-FIGURA.
           03  CRCF-POS-SUP            PIC  9(04)V9.
           03  CRCF-POS-ESQ            PIC  9(04)V9.
           03  CRCF-POS-ALT            PIC  9(04)V9.
           03  CRCF-POS-LARG           PIC  9(04)V9.
           03  CRCF-AUTO               PIC  9(01).
           03  CRCF-AJUSTADO           PIC  9(01).
           03  CRCF-TIPO               PIC  9(01).
      *
       01  CR-COMPLEMENTO-GRAFICO.
           03  CRCG-POS-SUP            PIC  9(04)V9.
           03  CRCG-POS-ESQ            PIC  9(04)V9.
           03  CRCG-POS-ALT            PIC  9(04)V9.
           03  CRCG-POS-LARG           PIC  9(04)V9.
      *
      * Vari†veis utilizadas em gr†ficos
      *
       01  CR-CABECALHO-GERAL.
           03  CR-CG-POS-ALT           PIC  9(04)V9.
           03  CR-CG-POS-LARG          PIC  9(04)V9.
           03  CR-CG-TIPO-GRAFICO      PIC  9(02).
           03  CR-CG-RGB1-TIT-JAN      PIC  9(03).
           03  CR-CG-RGB2-TIT-JAN      PIC  9(03).
           03  CR-CG-RGB3-TIT-JAN      PIC  9(03).
           03  CR-CG-3D                PIC  9(01).
           03  CR-CG-POSICAO-LEGENDA   PIC  9(01).
           03  CR-CG-GRADIENTE         PIC  9(01).
           03  CR-CG-RGB1-I-GRAD       PIC  9(03).
           03  CR-CG-RGB2-I-GRAD       PIC  9(03).
           03  CR-CG-RGB3-I-GRAD       PIC  9(03).
           03  CR-CG-RGB1-F-GRAD       PIC  9(03).
           03  CR-CG-RGB2-F-GRAD       PIC  9(03).
           03  CR-CG-RGB3-F-GRAD       PIC  9(03).
           03  CR-CG-FIGURA            PIC  X(60).
           03  CR-CG-FIG-INT-GRAF      PIC  9(01).
           03  CR-CG-APR-FIG-FUNDO     PIC  9(01).
           03  CR-CG-FONTE-TAM-FIXO    PIC  9(01).
      *
       01  CR-CABECALHO-BARRA-SIMPLES.
           03  CR-CBS-TITULO-GRAFICO   PIC  X(30).
           03  CR-CBS-TITULO-COLUNA    PIC  X(30).
           03  CR-CBS-TITULO-LINHA     PIC  X(30).
           03  CR-CBS-EIXOS            PIC  9(01).
           03  CR-CBS-QTD-BARRAS       PIC  9(02).
           03  CR-CBS-MARCAS           PIC  9(01).
      *
       01  CR-DETALHE-BARRA-SIMPLES.
           03  CR-DBS-VALOR            PIC  9(12)V99.
           03  CR-DBS-RGB1-BARRA       PIC  9(03).
           03  CR-DBS-RGB2-BARRA       PIC  9(03).
           03  CR-DBS-RGB3-BARRA       PIC  9(03).
           03  CR-DBS-LEGENDA          PIC  X(10).
      *
       01  CR-CABECALHO-BARRAS-MULTIPLAS.
           03  CR-CBM-TITULO-GRAFICO   PIC  X(30).
           03  CR-CBM-TITULO-COLUNA    PIC  X(30).
           03  CR-CBM-TITULO-LINHA     PIC  X(30).
           03  CR-CBM-EIXOS            PIC  9(01).
           03  CR-CBM-QTD-ENTIDADES    PIC  9(02).
           03  CR-CBM-QTD-BARRAS       PIC  9(02).
           03  CR-CBM-TIPO-BARRAS      PIC  9(01).
      *
       01  CR-ENTIDADE-BARRAS-MULTIPLAS.
           03  CR-EBM-RGB1-ENT         PIC  9(03).
           03  CR-EBM-RGB2-ENT         PIC  9(03).
           03  CR-EBM-RGB3-ENT         PIC  9(03).
           03  CR-EBM-LEGENDA          PIC  X(10).
      *
       01  CR-DETALHE-BARRAS-MULTIPLAS.
           03  CR-DBM-IDENT            PIC  9(02).
           03  CR-DBM-VALOR            PIC  9(12)V99.
           03  CR-DBM-LEGENDA          PIC  X(10).
      *
       01  CR-CABECALHO-PIZZA.
           03  CR-CP-TITULO-GRAFICO    PIC  X(30).
           03  CR-CP-QTD-FATIAS        PIC  9(02).
           03  CR-CP-TIPO-LEGENDA      PIC  9(01).
           03  CR-CP-MARCA-LEGENDA     PIC  9(01).
      *
       01  CR-DETALHE-PIZZA.
           03  CR-DP-VALOR             PIC  9(12)V99.
           03  CR-DP-RGB1-PIZZA        PIC  9(03).
           03  CR-DP-RGB2-PIZZA        PIC  9(03).
           03  CR-DP-RGB3-PIZZA        PIC  9(03).
           03  CR-DP-LEGENDA           PIC  X(10).
      *
       01  CR-CABECALHO-AREA.
           03  CR-CA-TITULO-GRAFICO    PIC  X(30).
           03  CR-CA-TITULO-COLUNA     PIC  X(30).
           03  CR-CA-TITULO-LINHA      PIC  X(30).
           03  CR-CA-EIXOS             PIC  9(01).
           03  CR-CA-QTD-ENTIDADES     PIC  9(02).
           03  CR-CA-QTD-PONTOS        PIC  9(02).
           03  CR-CA-TIPO-AREAS        PIC  9(01).
      *
       01  CR-ENTIDADE-AREA.
           03  CR-EA-RGB1-ENT          PIC  9(03).
           03  CR-EA-RGB2-ENT          PIC  9(03).
           03  CR-EA-RGB3-ENT          PIC  9(03).
           03  CR-EA-LEGENDA           PIC  X(10).
           03  CR-EA-PADRAO            PIC  9(01).
           03  CR-EA-ESCADA            PIC  9(01).
      *
       01  CR-DETALHE-AREA.
           03  CR-DA-IDENT             PIC  9(02).
           03  CR-DA-VALOR             PIC  9(12)V99.
           03  CR-DA-LEGENDA           PIC  X(10).
      *
       01  CR-CABECALHO-LINHA.
           03  CR-CL-TITULO-GRAFICO    PIC  X(30).
           03  CR-CL-TITULO-COLUNA     PIC  X(30).
           03  CR-CL-TITULO-LINHA      PIC  X(30).
           03  CR-CL-EIXOS             PIC  9(01).
           03  CR-CL-QTD-ENTIDADES     PIC  9(02).
           03  CR-CL-QTD-PONTOS        PIC  9(02).
      *
       01  CR-ENTIDADE-LINHA.
           03  CR-EL-RGB1-ENT          PIC  9(03).
           03  CR-EL-RGB2-ENT          PIC  9(03).
           03  CR-EL-RGB3-ENT          PIC  9(03).
           03  CR-EL-LEGENDA           PIC  X(10).
           03  CR-EL-PADRAO            PIC  9(01).
           03  CR-EL-ESCADA            PIC  9(01).
      *
       01  CR-DETALHE-LINHA.
           03  CR-DL-IDENT             PIC  9(02).
           03  CR-DL-VALOR             PIC  9(12)V99.
           03  CR-DL-LEGENDA           PIC  X(10).
