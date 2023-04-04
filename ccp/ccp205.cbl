       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP205.
      *DATA: 04/02/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: QUADRO POR SETOR
      *FUNÇÃO: -Tem por finalidade fazer as atualizações referentes
      *        a pagamento - Emitir cheque, recibo, lançar no caixa,
      *        lançar no conta corrente, baixar valores pagos no valor
      *        previsto lançado no contas a pagar.

      *        a pedido do vonei, foi pedido p/ cancelar a atualização
      *        automática no contas a pagar e cont.bancário 03/2001
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CGPX004.
           COPY CCPX001.
           COPY CCPX010.
           COPY CCPX100.
           COPY CCPX101.
           COPY CCPX105.
           COPY CCPX115.
           COPY CCPX110.
           COPY CCPX120.
           COPY CPPX020.
           COPY CPPX021.
           COPY CXPX100.
           COPY CBPX001.
           COPY CBPX100.
      *    COPY CIEPX010.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CODIGO-WK SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW004.
      *COPY CIEPW010.
       COPY CCPW001.
       COPY CCPW010.
       COPY CCPW100.
       COPY CCPW101.
       COPY CCPW105.
       COPY CCPW115.
       COPY CCPW110.
       COPY CCPW120.
       COPY CPPW020.
       COPY CPPW021.
       COPY CBPW001.
       COPY CBPW100.
       COPY CXPW100.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-WK             PIC X(30).
           05  NR-CHEQUE-WK        PIC 9(6).
           05  SALARIO1-WK         PIC 9(7)V99.
           05  SALARIO2-WK         PIC 9(7)V99.
           05  TOT-CREDITO-WK      PIC 9(7)V99.
           05  SALDO-CTACORR-WK    PIC S9(7)V99.
           05  VALOR-APAGAR-WK     PIC 9(7)V99.
           05  PAGAR-WK            PIC 9.
      * PAGAR = 0(NÃO)  1(SIM)X
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP205.CPB".
           COPY "CCP205.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPEXTE.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  TABELA-DATA.
           05  FILLER         PIC X(10) VALUE "  JANEIRO ".
           05  FILLER         PIC X(10) VALUE " FEVEREIRO".
           05  FILLER         PIC X(10) VALUE "   MARCO  ".
           05  FILLER         PIC X(10) VALUE "   ABRIL  ".
           05  FILLER         PIC X(10) VALUE "   MAIO   ".
           05  FILLER         PIC X(10) VALUE "   JUNHO  ".
           05  FILLER         PIC X(10) VALUE "   JULHO  ".
           05  FILLER         PIC X(10) VALUE "  AGOSTO  ".
           05  FILLER         PIC X(10) VALUE " SETEMBRO ".
           05  FILLER         PIC X(10) VALUE "  OUTUBRO ".
           05  FILLER         PIC X(10) VALUE " NOVEMBRO ".
           05  FILLER         PIC X(10) VALUE " DEZEMBRO ".
       01  TAB-DAT REDEFINES TABELA-DATA.
           05  TABMES OCCURS 12 TIMES PIC X(10).

       01  VARIAVEIS.
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-CCD115             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 99.
               10  DIA-I             PIC 99.
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  MES-EMISSAO           PIC 9(2)     VALUE ZEROS.
           05  QTDE-POR-FOLHA        PIC 9        VALUE ZEROS.
           05  COD-EMPRESA-W         PIC 9        VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ-.
           05  SEQ-CAIXA             PIC 9(4)     VALUE ZERO.
           05  NR-CHEQUE             PIC 9(6)     VALUE ZEROS.
           05  TOT-VALOR-SEL         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-CHEQUE-SEL        PIC 9(4)     VALUE ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  SALDO-APAGAR          PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-CTACORR-W       PIC S9(8)V99 VALUE ZEROS.
           05  SALARIO1-TOT          PIC 9(8)V99  VALUE ZEROS.
           05  SALARIO2-TOT          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-CREDITO-TOT       PIC 9(8)V99  VALUE ZEROS.
           05  SALDO-CTACORR-TOT     PIC S9(8)V99 VALUE ZEROS.
           05  CTACORR-LIQUIDO       PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  VALOR-APAGAR-TOT      PIC S9(8)V99 VALUE ZEROS.
           05  STRING-RECIBO         PIC X(40)    VALUE SPACES.
           05  STRING-BANCO1.
               10  VALOR-BANC        PIC 9(8)V99.
               10  NOME-BANC         PIC X(30).
           05  STRING-BANCO REDEFINES STRING-BANCO1 PIC X(40).

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(62)   VALUE
           "QUADRO DE SALARIOS -                                ".
           05  FILLER              PIC X(15)   VALUE "       VENCTO: ".
           05  VENCTO-REL          PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "MESANO-BASE: ".
           05  MESANO-BASE-REL     PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "NOME                            CREDITO(1)  CREDITO(2) CTA.C
      -    "ORRENTE SALDO-APAGAR L S VALOR-APAGAR".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

      *LAYOUT - RECIBO REPORTAGEM
       01  LINDET0-REC1.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "MES/ANO-BASE: ".
           05  MESANO-BASE-REC1    PIC 99/9999.
       01  LINDET1-REC1.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  CODIGO-REC1         PIC ZZZ.ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  NOME-REC1           PIC X(30)   VALUE SPACES.
       01  LINDET2-REC1.
           05  FILLER              PIC X(6)    VALUE SPACES.
           05  DESCRICAO-REC1      PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X(9)    VALUE SPACES.
           05  VALOR1-REC1         PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET3-REC1.
           05  FILLER              PIC X(43)   VALUE SPACES.
           05  VALOR2-REC1         PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET4-REC1.
           05  FILLER              PIC X(57)   VALUE SPACES.
           05  VALOR3-REC1         PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET5-REC1.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  VALOR4-REC1         PIC ZZ.ZZZ.ZZZ,ZZ.
      *  LAYOUT RECIBOS - P/ TODOS OS CHEQUES EMITIDOS
       01  LINDET-REC2             PIC X(80)   VALUE
       "                               R E C I B O".
       01  LINDET0-REC2.
           05  FILLER              PIC X(57)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "VALOR-R$: ".
           05  VALOR1-REC2         PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET1-REC2.
           05  FILLER              PIC X(18)   VALUE
           "RECEBI DA EMPRESA ".
           05  EMPRESA-REC2        PIC X(28)   VALUE SPACES.
           05  ENDERECO-REC2       PIC X(27)   VALUE SPACES.
       01  LINDET2-REC2.
           05  FILLER              PIC X(08)   VALUE "C.G.C.: ".
           05  CGC-REC2            PIC X(18)   VALUE SPACES.
           05  FILLER              PIC X(17)   VALUE ", O VALOR DE R$:".
           05  VALOR2-REC2         PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET3-REC2.
           05  FILLER              PIC X       VALUE "(".
           05  VALOR-EXTENSO-REC2  PIC X(78)   VALUE SPACES.
           05  FILLER              PIC X       VALUE ")".
       01  LINDET4-REC2.
           05  FILLER              PIC X(12)   VALUE "REFERENTE A ".
           05  DESCRICAO-REC2      PIC X(30)   VALUE SPACES.
       01  LINDET5-REC2.
           05  FILLER              PIC X(7)    VALUE "BANCO: ".
           05  BANCO-REC2          PIC 9(4)    VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  NOME-BANCO-REC2     PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "   NR.CHEQUE: ".
           05  NR-CHEQUE-REC2      PIC 9(6)    VALUE ZEROS.
       01  LINDET6-REC2.
           05  FILLER              PIC X(48)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE 'SANTA FE, '.
           05  DIA-REC2            PIC 99      VALUE ZEROS.
           05  FILLER              PIC X(2)    VALUE ', '.
           05  MES-REC2            PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(4)    VALUE " DE ".
           05  ANO-REC2            PIC 9999    VALUE ZEROS.
       01  LINDET7-REC2.
           05  FILLER              PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(40)   VALUE ALL "_".
       01  LINDET8-REC2.
           05  FILLER              PIC X(43)   VALUE SPACES.
           05  CODIGO-REC2         PIC 9(6)    VALUE ZEROS.
           05  FILLER              PIC X       VALUE "-".
           05  NOME-REC2           PIC X(30)   VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD004.
           MOVE "CCD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CCD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD105.
           MOVE "CCD115"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD115.
           MOVE "CCD110"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD110.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CBD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD100.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           OPEN INPUT CCD100 CGD001 CGD004 CCD105 CCD115 CCD001
                      CCD110 CCD120 CCD010.
      *    OPEN I-O CIED010.
      *    IF ST-CIED010 = "35"
      *       CLOSE CIED010   OPEN OUTPUT CIED010   CLOSE CIED010
      *       OPEN I-O CIED010.
           IF ST-CCD001 <> "00"
              MOVE "ERRO ABERTURA CCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD010 <> "00"
              MOVE "ERRO ABERTURA CCD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CIED010 <> "00"
      *       MOVE "ERRO ABERTURA CIED010: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CIED010 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD004 <> "00"
              MOVE "ERRO ABERTURA CGD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD105 <> "00"
              MOVE "ERRO ABERTURA CCD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD115 <> "00"
              MOVE "ERRO ABERTURA CCD115: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD115 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD110 <> "00"
              MOVE "ERRO ABERTURA CCD110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD120 <> "00"
              MOVE "ERRO ABERTURA CCD120: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD120 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-SELECIONAR-TUDO-TRUE
                    PERFORM SELECIONAR-TUDO
               WHEN GS-VERIF-NR-CHEQUE-TRUE
                    PERFORM VERIF-NR-CHEQUE
               WHEN GS-EMITIR-CHEQUE-TRUE
                    PERFORM IMPRIMIR-CHEQUE
               WHEN GS-EMITIR-RECIBO-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RECIBO-VLR-CHEQUE
                    end-if
               WHEN GS-EMIT-RECIBO-REPOR-TRUE
                    PERFORM IMPRIMIR-RECIBO-REPORTAGEM
               WHEN GS-EMIT-RECIBO-FUNC-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIMIR-RECIBO-FUNC
                    end-if
               WHEN GS-POPUP-BANCO-TRUE
                    PERFORM POPUP-BANCO
               WHEN GS-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN GS-EXIBE-DADOS-SELEC-TRUE
                    PERFORM EXIBE-DADOS-SELEC
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO-USUARIO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       VERIFICA-ACESSO-USUARIO SECTION.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-ACESSO-CC01.
           MOVE USUARIO-W          TO NOME-REDUZ-CC01.
           READ CCD001 INVALID KEY
                MOVE 0  TO GS-LIBERA-ACESSO
             NOT INVALID KEY
                MOVE 1 TO GS-LIBERA-ACESSO.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *ACHA-SEQ-CIE SECTION.
      *    MOVE DATA-DIA-INV    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
      *       NOT AT END
      *         IF DATA-CI10 NOT = DATA-DIA-INV MOVE "10" TO ST-CIED010
      *         ELSE CONTINUE
      *      END-READ
      *    END-PERFORM.
      *GRAVA-CIE SECTION.
      *    PERFORM ACHA-SEQ-CIE.
      *    MOVE DATA-DIA-INV   TO DATA-CI10
      *    ADD 1               TO SEQ-CI10
      *    ACCEPT HORA-W       FROM TIME.
      *    MOVE HORA-W(1: 4)   TO HORA-CI10
      *    MOVE USUARIO-W      TO ORIGEM-CI10
      *                        TO COD-FUNCAO-CI10
      * Função que exerce o destinatario

      *    VERIFICAR CADASTRO EM SANTA FÉ
      *    MOVE 7              TO FUNCAO-DESTINO-CI10.
      *    MOVE 24             TO COD-MENS-PADRAO-CI10.
      *    MOVE "EMITIR RELATORIO DE APROVACAO DE FOLHA-PGTO  "
      *                        TO DESCRICAO-MENS-CI10.
      *    MOVE ZEROS          TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.
       SELECIONAR-TUDO SECTION.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                IF VALOR-APAGAR-WK NOT = ZEROS MOVE 1 TO PAGAR-WK
                   ADD VALOR-APAGAR-WK TO TOT-VALOR-SEL
                   ADD 1               TO TOT-CHEQUE-SEL
                ELSE MOVE 0 TO PAGAR-WK
                END-IF
                REWRITE REG-WORK
             END-READ
           END-PERFORM.
           PERFORM CARREGA-LISTA.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(114: 6) TO CODIGO-WK.
           MOVE GS-LINDET(121: 5) TO SEQ-WK.
           READ WORK INVALID KEY CONTINUE
            NOT INVALID KEY
             IF VALOR-APAGAR-WK = ZEROS CONTINUE
             ELSE
              IF PAGAR-WK = 0
                 MOVE 1       TO PAGAR-WK
                 MOVE "X"     TO GS-LINDET(83: 02)
                 ADD VALOR-APAGAR-WK TO TOT-VALOR-SEL
                 ADD 1               TO TOT-CHEQUE-SEL
                 REWRITE REG-WORK
              ELSE MOVE 0          TO PAGAR-WK
                 MOVE " "          TO GS-LINDET(83: 02)
                 SUBTRACT VALOR-APAGAR-WK FROM TOT-VALOR-SEL
                 SUBTRACT 1 FROM TOT-CHEQUE-SEL
                 REWRITE REG-WORK.
       EXIBE-DADOS-SELEC SECTION.
           MOVE TOT-VALOR-SEL  TO GS-TOT-VALOR-SEL.
           MOVE TOT-CHEQUE-SEL TO GS-TOT-CHEQUE-SEL.
       GRAVA-WORK SECTION.
           MOVE ZEROS TO TOT-VALOR-SEL TOT-CHEQUE-SEL.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW TO ANO-II.
           MOVE MES-WW TO MES-II.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           EVALUATE TIPO-LCTO-W
             WHEN 1 PERFORM GRAVA-WORK-CCD105
             WHEN 2 PERFORM GRAVA-WORK-CCD120
             WHEN 3 PERFORM GRAVA-WORK-CCD115
             WHEN 4 PERFORM GRAVA-WORK-CCD110
           END-EVALUATE.
           PERFORM GRAVA-SALDO-CTACORR.
       GRAVA-WORK-CCD105 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC105.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC105.
           START CCD105 KEY IS NOT < CHAVE-CC105 INVALID KEY
                  MOVE "10" TO ST-CCD105.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD105 = "10"
             READ CCD105 NEXT RECORD AT END MOVE "10" TO ST-CCD105
              NOT AT END
                  IF MESANO-BASE-CC105 NOT = MESANO-I
                       MOVE "10" TO ST-CCD105
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC105        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE SALARIO1-CC105      TO SALARIO1-WK
                   MOVE SALARIO2-CC105      TO SALARIO2-WK
                   COMPUTE TOT-CREDITO-WK = SALARIO1-WK + SALARIO2-WK
                   MOVE VALOR-LIBERADO-CC105 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
                   MOVE ZEROS               TO PAGAR-WK
                   MOVE ZEROS               TO NR-CHEQUE-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD115 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC115.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC115.
           START CCD115 KEY IS NOT < CHAVE-CC115 INVALID KEY
                  MOVE "10" TO ST-CCD115.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD115 = "10"
             READ CCD115 NEXT RECORD AT END MOVE "10" TO ST-CCD115
              NOT AT END
                  IF MESANO-BASE-CC115 NOT = MESANO-I
                       MOVE "10" TO ST-CCD115
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC115        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE SALARIO1-CC115      TO SALARIO1-WK
                   MOVE SALARIO2-CC115      TO SALARIO2-WK
                   COMPUTE TOT-CREDITO-WK = SALARIO1-WK + SALARIO2-WK
                   MOVE VALOR-LIBERADO-CC115 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
                   MOVE ZEROS               TO PAGAR-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD110 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC110.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC110.
           START CCD110 KEY IS NOT < CHAVE-CC110 INVALID KEY
                  MOVE "10" TO ST-CCD110.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD110 = "10"
             READ CCD110 NEXT RECORD AT END MOVE "10" TO ST-CCD110
              NOT AT END
                  IF MESANO-BASE-CC110 NOT = MESANO-I
                       MOVE "10" TO ST-CCD110
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC110        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE VALOR-CREDITO-CC110 TO SALARIO2-WK
                   MOVE ZEROS               TO SALARIO1-WK
                   COMPUTE TOT-CREDITO-WK = SALARIO1-WK + SALARIO2-WK
                   MOVE VALOR-LIBERADO-CC110 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
                   MOVE ZEROS               TO PAGAR-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-WORK-CCD120 SECTION.
           MOVE MESANO-I TO MESANO-BASE-CC120.
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-W.
           MOVE ZEROS TO CODIGO-CC120.
           START CCD120 KEY IS NOT < CHAVE-CC120 INVALID KEY
                  MOVE "10" TO ST-CCD120.
           INITIALIZE REG-WORK.
           PERFORM UNTIL ST-CCD120 = "10"
             READ CCD120 NEXT RECORD AT END MOVE "10" TO ST-CCD120
              NOT AT END
                  IF MESANO-BASE-CC120 NOT = MESANO-I
                       MOVE "10" TO ST-CCD120
                  ELSE
                   ADD 1 TO SEQ-WK
                   MOVE CODIGO-CC120        TO CODIGO-WK CODIGO-CG01
                                               GS-EXIBE-PROCESS
                   READ CGD001 INVALID KEY MOVE "*******" TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01           TO NOME-WK
                   MOVE VALOR-CREDITO-CC120 TO SALARIO2-WK
                   MOVE ZEROS               TO SALARIO1-WK
                   COMPUTE TOT-CREDITO-WK = SALARIO1-WK + SALARIO2-WK
                   MOVE VALOR-LIBERADO-CC120 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
                   MOVE ZEROS               TO PAGAR-WK
                   MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-WORK
                   END-IF
               END-READ
           END-PERFORM.
           CLOSE WORK.  OPEN I-O WORK.
       GRAVA-SALDO-CTACORR SECTION.
           MOVE GS-VENCTO(1: 2) TO VENCTO-INV(7: 2)
           MOVE GS-VENCTO(3: 2) TO VENCTO-INV(5: 2)
           MOVE GS-VENCTO(5: 4) TO VENCTO-INV(1: 4).

      *    GRAVAR SALDO CONTA CORRENTE

           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.

           MOVE VENCTO-INV(1: 6) TO MESANO-I
           PERFORM UNTIL ST-WORK = "10"
            READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
      *        pegar saldo anterior (acumulado)
               MOVE CODIGO-WK   TO FORNEC-CC10
               MOVE ZEROS       TO ANOMES-VCTO-CC10
               START CCD010 KEY IS NOT < ALT-CC10 INVALID KEY
                     MOVE "10" TO ST-CCD010
               END-START
               PERFORM UNTIL ST-CCD010 = "10"
                 READ CCD010 NEXT RECORD AT END MOVE "10" TO ST-CCD010
                   NOT AT END
                    IF ANOMES-VCTO-CC10 NOT < MESANO-I OR
                       FORNEC-CC10 NOT = CODIGO-WK
                         MOVE "10" TO ST-CCD010
                    ELSE
                     ADD SALDOS-CC10 TO SALDO-CTACORR-WK
                     SUBTRACT SALDOE-CC10 FROM SALDO-CTACORR-WK
                    END-IF
                 END-READ
               END-PERFORM
               MOVE CODIGO-WK TO FORNEC-CC100
               MOVE ZEROS     TO SITUACAO-CC100
               MOVE VENCTO-INV(1: 6) TO DATA-VENCTO-CC100(1: 6)
               MOVE 01               TO DATA-VENCTO-CC100(7: 2)
               START CCD100 KEY IS NOT < ALT1-CC100 INVALID KEY
                         MOVE "10" TO ST-CCD100
               END-START
               PERFORM UNTIL ST-CCD100 = "10"
                READ CCD100 NEXT RECORD AT END
                                MOVE "10" TO ST-CCD100
                  NOT AT END
                  IF FORNEC-CC100 NOT = CODIGO-WK OR SITUACAO-CC100 > 0
                     OR DATA-VENCTO-CC100 > VENCTO-INV
                        MOVE "10" TO ST-CCD100
                  ELSE
                     IF CRED-DEB-CC100 = 0
                         ADD VALOR-CC100 TO SALDO-CTACORR-WK
                     ELSE SUBTRACT VALOR-CC100 FROM SALDO-CTACORR-WK
                     END-IF
                END-READ
               END-PERFORM
               REWRITE REG-WORK
            END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIF-NR-CHEQUE SECTION.
           OPEN INPUT CBD100.
           MOVE ZEROS TO I GS-ACHOU-NR-CHEQUE.
           PERFORM VARYING NR-CHEQUE FROM GS-NR-CHEQUE BY 1 UNTIL
                                  I = TOT-CHEQUE-SEL OR
                                  GS-ACHOU-NR-CHEQUE = 1
             MOVE GS-NR-BANCO   TO CODIGO-FORN-CB100
             MOVE NR-CHEQUE     TO NR-CHEQUE-CB100
             ADD 1 TO I
             READ CBD100 INVALID KEY MOVE 1 TO GS-ACHOU-NR-CHEQUE
                NOT INVALID KEY
      * SITUACAO-CB100 = 1 (CHEQUE DISPONIVEL)
                   IF SITUACAO-CB100 NOT = 1
                      MOVE 1 TO GS-ACHOU-NR-CHEQUE
                   ELSE MOVE 0 TO GS-ACHOU-NR-CHEQUE
                   END-IF
             END-READ
           END-PERFORM.
           CLOSE CBD100.
       ACHAR-SEQ-CAIXA SECTION.
           MOVE DATA-DIA-INV TO DATA-MOV-CX100.
           MOVE ZEROS TO SEQ-CAIXA SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                  IF DATA-MOV-CX100 NOT = DATA-DIA-INV
                     MOVE "10" TO ST-CXD100
                  ELSE
                     MOVE SEQ-CX100 TO SEQ-CAIXA
                  END-IF
             END-READ
           END-PERFORM.
       IMPRIMIR-CHEQUE SECTION.
           CLOSE CCD100.
           OPEN I-O CCD100 CCD101 CPD020 CXD100 CBD100.
           PERFORM ACHAR-SEQ-CAIXA.
           MOVE GS-MESANO-BASE TO MESANO-W
           MOVE MES-WW TO MES-II
           MOVE ANO-WW TO ANO-II

           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE GS-NR-CHEQUE TO NR-CHEQUE.
           SUBTRACT 1 FROM NR-CHEQUE.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 IF PAGAR-WK = 0 CONTINUE
                 ELSE
                   ADD 1 TO NR-CHEQUE
                   MOVE NR-CHEQUE        TO NR-CHEQUE-WK
                   REWRITE REG-WORK
                   END-REWRITE
                   MOVE VALOR-APAGAR-WK  TO VALOR-BANC
                   MOVE NOME-WK          TO NOME-BANC
                   CALL   "CBP200" USING PARAMETROS-W STRING-BANCO
                   CANCEL "CBP200"
      *            a pedido do vonei, eliminar atualização automática
      *            no caixa e contas a pagar

      *            PERFORM ATUALIZA-CAIXA
                   PERFORM ATUALIZA-CONT-BANCARIO
                   PERFORM ATUALIZA-CTA-CORRENTE
      *            ATENÇÃO - A ORDEM DE PERFORM É IMPORTANTE
             END-READ
           END-PERFORM.
      *    IF GS-VENCTO-CHEQUE = ZEROS
      *       PERFORM ATUALIZA-CONTADOBRADA-CAIXA.
      *    PERFORM ATUALIZA-CONTAS-PAGAR.
           CLOSE CCD100 CCD101 CPD020 CXD100 CBD100.
           OPEN INPUT CCD100.
       IMPRIMIR-RECIBO-FUNC SECTION.
           OPEN OUTPUT RELAT.
           MOVE GS-MESANO-BASE TO MESANO-W MESANO-BASE-REC1
           MOVE MES-WW TO MES-II
           MOVE ANO-WW TO ANO-II
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE GS-NR-CHEQUE TO NR-CHEQUE.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 IF PAGAR-WK = 0 CONTINUE
                 ELSE
                   MOVE "SALARIO NORMAL" TO DESCRICAO-REC1
                   MOVE SALARIO2-WK      TO VALOR1-REC1 VALOR2-REC1
                                            VALOR3-REC1 VALOR4-REC1
                   MOVE NOME-WK          TO NOME-REC1
                   MOVE CODIGO-WK        TO CODIGO-REC1
                   WRITE REG-RELAT FROM LINDET0-REC1 AFTER 4
                   WRITE REG-RELAT FROM LINDET1-REC1 AFTER 3
                   WRITE REG-RELAT FROM LINDET2-REC1 AFTER 5
                   WRITE REG-RELAT FROM LINDET3-REC1 AFTER 15
                   WRITE REG-RELAT FROM LINDET3-REC1 AFTER 2
                   WRITE REG-RELAT FROM LINDET3-REC1 AFTER 2
                   MOVE SPACES TO REG-RELAT
                   WRITE REG-RELAT AFTER 2
             END-READ
           END-PERFORM.
           CLOSE RELAT.
       IMPRIMIR-RECIBO-REPORTAGEM SECTION.
           MOVE GS-MESANO-BASE TO MESANO-W
           MOVE MES-WW TO MES-II
           MOVE ANO-WW TO ANO-II
           MOVE MESANO-I TO STRING-RECIBO(1: 6).
           CALL   "REP424" USING PARAMETROS-W STRING-RECIBO.
           CANCEL "REP424".
       ATUALIZA-CONT-BANCARIO SECTION.
           MOVE NR-CHEQUE TO NR-CHEQUE-CB100
           MOVE GS-NR-BANCO TO CODIGO-FORN-CB100
           READ CBD100 INVALID KEY CONTINUE
             NOT INVALID KEY
               MOVE CODIGO-WK                 TO NOMINAL-A-CB100
               MOVE VALOR-APAGAR-WK           TO VALOR-CB100
               IF GS-VENCTO-CHEQUE = ZEROS
                  MOVE 2    TO SITUACAO-CB100
                  MOVE ZEROS TO DATA-VENCTO-CB100
                  MOVE ZEROS TO SEQ-CTA-PAGAR-CB100
               ELSE MOVE 3  TO SITUACAO-CB100
                    MOVE GS-VENCTO-CHEQUE TO DATA-VENCTO-CB100
                    MOVE SEQ-CP21 TO SEQ-CTA-PAGAR-CB100
               END-IF
      *        SITUACAO-CB100 = 2(CH.A VISTA)  3(CH.PRE-DATADO)
               MOVE DATA-MOVTO-W              TO DATA-EMISSAO-CB100
               REWRITE REG-CBD100
               END-REWRITE
           END-READ.
       ATUALIZA-CAIXA SECTION.
      *    Se VENCTO DO CHEQUE não for a vista
      *      - Não atualizar no caixa
      *      - Fazer a atualização no contas a pagar como uma conta
      *        com cheque pre datado e vencto conforme o informado
      *      - No conta corrente o vencto vai ser a data de emissão
      *      - No controle bancário, irá entrar como cheque pre-datado
           IF GS-VENCTO-CHEQUE NOT = ZEROS
              PERFORM LANCA-PREDATADO-APAGAR
           ELSE
            MOVE DATA-DIA-INV           TO DATA-MOV-CX100
            MOVE 21                     TO TIPO-LCTO-CX100
      *     21 - SAIDA - CONTA CORRENTE
            MOVE "PAGTO SALARIO"        TO HISTORICO-CX100
            MOVE NR-CHEQUE              TO DOCUMENTO-CX100
            MOVE VALOR-APAGAR-WK        TO VALOR-CX100
            MOVE CODIGO-WK              TO CONTAPART-CX100 CODIGO-CG04
      *     TIPO-LCTO-W = 1-FUNCIONARIO   2-VENDEDOR   4-REPORTAGEM
            EVALUATE TIPO-LCTO-W
              WHEN 1
                 READ CGD004 INVALID KEY MOVE 999 TO CONTA-APUR-RED-CG04
                 END-READ
                 MOVE CONTA-APUR-RED-CG04    TO CONTA-REDUZ-CX100
              WHEN 4 MOVE 79 TO CONTA-REDUZ-CX100
              WHEN 3 MOVE 31 TO CONTA-REDUZ-CX100
              WHEN 2 MOVE 45 TO CONTA-REDUZ-CX100
            END-EVALUATE
      *     MOVE ZEROS                  TO CONTABIL-CX100
            MOVE USUARIO-W              TO RESPONSAVEL-CX100
            MOVE ZEROS                  TO SEQ-DESM-CX100
            MOVE ZEROS TO ST-CXD100
            PERFORM UNTIL ST-CXD100 = "10"
                 ADD 1 TO SEQ-CAIXA
                 MOVE SEQ-CAIXA TO SEQ-CX100
                 WRITE REG-CXD100 INVALID KEY CONTINUE
                   NOT INVALID KEY MOVE "10" TO ST-CXD100
                 END-WRITE
            END-PERFORM.
       LANCA-PREDATADO-APAGAR SECTION.
           MOVE DATA-DIA-INV           TO DATA-MOVTO-CP20
           MOVE CODIGO-WK              TO FORNEC-CP20 FORNEC-CP21
           PERFORM ACHA-SEQ-APAGAR
           MOVE SEQ-CP21               TO SEQ-CP20
           MOVE 16                     TO TIPO-FORN-CP20
      *    TIPO-FORN = 16 (CONTA CORRENTE)
           MOVE 12                     TO PORTADOR-CP20
      *    PORTADOR = 12 (CHEQUE PRE DATADO)
           MOVE NR-CHEQUE              TO NR-DOCTO-CP20
           MOVE DATA-DIA-INV           TO DATA-EMISSAO-CP20
           MOVE GS-VENCTO-CHEQUE TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV               TO DATA-VENCTO-CP20
           MOVE 0                      TO PREV-DEF-CP20
      *    PREV-DEF-CP20 = 0(DEFINITIVO)  1(PREVISTO)
           MOVE 0                      TO SITUACAO-CP20
      *    SITUACAO = 0(OK)
           MOVE 0                      TO LIBERADO-CP20
      *    LIBERADO-CP20 = 0(NÃO)   1(SIM)  (LIBERADO PELA DIRETORIA)
           MOVE 0                      TO TIPO-MOEDA-CP20
           MOVE 0101                   TO NR-PARCELA-CP20
           EVALUATE TIPO-LCTO-W
              WHEN 1
                READ CGD004 INVALID KEY MOVE 999 TO CONTA-APUR-RED-CG04
                END-READ
                MOVE CONTA-APUR-RED-CG04    TO CODREDUZ-APUR-CP20
                MOVE "PGTO FUNCIONARIO" TO DESCRICAO-CP20
              WHEN 4 MOVE 79   TO CODREDUZ-APUR-CP20
                     MOVE "PGTO REPORTAGEM"  TO DESCRICAO-CP20
              WHEN 2 MOVE 45 TO CODREDUZ-APUR-CP20
                     MOVE "PGTO VENDEDOR"    TO DESCRICAO-CP20
              WHEN 3 MOVE 31 TO CODREDUZ-APUR-CP20
                     MOVE "PGTO REPRESENTANTE" TO DESCRICAO-CP20
           END-EVALUATE
           MOVE ZEROS  TO JUROS-MORA-CP20 MULTA-PAGA-CP20 DESCONTO-CP20
             MULTA-ATRASO-CP20 CONTABILIZADO-CP20 SEQ-CAIXA
             TAXA-APLIC-CP20 JURO-PAGO-CP20 DATA-PGTO-CP20
             VALOR-LIQ-CP20
           MOVE VALOR-APAGAR-WK        TO VALOR-TOT-CP20
           MOVE USUARIO-W              TO RESPONSAVEL-CP20
           MOVE USUARIO-W              TO DIGITADOR-CP20
           MOVE 0                      TO TIPO-CONTA-CP20
      *    TIPO-CONTA  = 0(TEMPORARIA)  1(PERMANENTE)
           MOVE ZEROS TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             WRITE REG-CPD020 INVALID KEY
                 PERFORM ACHA-SEQ-APAGAR
                 MOVE SEQ-CP21 TO SEQ-CP20
              NOT INVALID KEY MOVE "10" TO ST-CPD020.
       ATUALIZA-CONTADOBRADA-CAIXA SECTION.
           MOVE TOT-VALOR-SEL          TO VALOR-CX100
           MOVE DATA-DIA-INV           TO DATA-MOV-CX100
           MOVE 68                     TO TIPO-LCTO-CX100
      *    68 - ENTRADA - CONTA CORRENTE
           MOVE "PAGTO SALARIO"        TO HISTORICO-CX100
           MOVE SPACES                 TO DOCUMENTO-CX100
           MOVE GS-NR-BANCO            TO CONTAPART-CX100
           MOVE 023                    TO CONTA-REDUZ-CX100
      *    MOVE ZEROS                  TO CONTABIL-CX100
           MOVE USUARIO-W              TO RESPONSAVEL-CX100
           MOVE ZEROS                  TO SEQ-DESM-CX100
           MOVE ZEROS TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
                ADD 1 TO SEQ-CAIXA
                MOVE SEQ-CAIXA TO SEQ-CX100
                WRITE REG-CXD100 INVALID KEY CONTINUE
                  NOT INVALID KEY MOVE "10" TO ST-CXD100
                END-WRITE
           END-PERFORM.
       IMPRIME-RECIBO-VLR-CHEQUE SECTION.
           CLOSE CCD105 CCD110 CCD115 CCD120.
           OPEN I-O CCD105 CCD110 CCD115 CCD120.

           OPEN OUTPUT RELAT
           MOVE GS-MESANO-BASE TO MESANO-W MESANO-BASE-REC1
           MOVE MES-WW TO MES-II
           MOVE ANO-WW TO ANO-II
           MOVE ZEROS TO QTDE-POR-FOLHA.
           MOVE ZEROS TO CODIGO-WK SEQ-WK
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK
           END-START
           PERFORM UNTIL ST-WORK = "10"
            READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
             NOT AT END
               IF PAGAR-WK = 0 CONTINUE
               ELSE
                MOVE "Atualizando... " TO GS-TEXTO-AGUARDE(1: 15)
                MOVE CODIGO-WK         TO GS-TEXTO-AGUARDE(16: 6)
                MOVE "REFRESH-WIN3" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                IF GS-RECIBO-CHEQUE = 2 PERFORM ZERA-VLR-LIBERADO
                ELSE
                 MOVE GS-NR-BANCO         TO BANCO-REC2
                 MOVE GS-DESC-BANCO       TO NOME-BANCO-REC2
                 MOVE NR-CHEQUE-WK        TO NR-CHEQUE-REC2
                 MOVE GS-DESCRICAO-RECIBO TO DESCRICAO-REC2
                 MOVE VALOR-APAGAR-WK TO VALOR1-REC2 VALOR2-REC2
                                         GREXTE-VALOR-BASE
                 CALL   "GREXTE" USING PARAMETROS-GREXTE
                 CANCEL "GREXTE"
                 MOVE GREXTE-VALOR-EXTENSO TO VALOR-EXTENSO-REC2
                 MOVE NOME-WK          TO NOME-REC2
                 MOVE CODIGO-WK        TO CODIGO-REC2
                 MOVE GS-NOME-EMPRESA(3: 27) TO EMPRESA-REC2
                 MOVE GS-NOME-EMPRESA(1: 1) TO COD-EMPRESA-W
                 EVALUATE COD-EMPRESA-W
                   WHEN 1 MOVE "AV.GETULIO VARGAS, 719" TO ENDERECO-REC2
                          MOVE "79.981.940/0001-69" TO CGC-REC2
                   WHEN 2 MOVE "RUA PONTA GROSSA, 378" TO ENDERECO-REC2
                          MOVE "02.284.205/0001-71" TO CGC-REC2
                 END-EVALUATE
                 MOVE DATA-MOVTO-W(1: 2)  TO DIA-REC2
                 MOVE DATA-MOVTO-W(3: 2)  TO MES-EMISSAO
                 MOVE DATA-MOVTO-W(5: 4)  TO ANO-REC2
                 MOVE TABMES(MES-EMISSAO) TO MES-REC2
                 ADD 1 TO QTDE-POR-FOLHA
                 PERFORM IMPRIME-RECIBO-VLR-CHEQUE1
                 PERFORM ZERA-VLR-LIBERADO
                END-IF
               END-IF
            END-READ
           END-PERFORM.
           CLOSE RELAT.
           CLOSE CCD105 CCD110 CCD115 CCD120.
           OPEN INPUT CCD105 CCD110 CCD115 CCD120.
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM GRAVA-WORK.
       IMPRIME-RECIBO-VLR-CHEQUE1 SECTION.
           IF QTDE-POR-FOLHA > 2
              WRITE REG-RELAT FROM LINDET-REC2 AFTER PAGE
              MOVE 1 TO QTDE-POR-FOLHA
           ELSE WRITE REG-RELAT FROM LINDET-REC2 AFTER 2.
           WRITE REG-RELAT FROM LINDET0-REC2 AFTER 4.
           WRITE REG-RELAT FROM LINDET1-REC2 AFTER 3.
           WRITE REG-RELAT FROM LINDET2-REC2
           WRITE REG-RELAT FROM LINDET3-REC2
           WRITE REG-RELAT FROM LINDET4-REC2
           WRITE REG-RELAT FROM LINDET5-REC2 AFTER 2
           WRITE REG-RELAT FROM LINDET6-REC2 AFTER 3
           WRITE REG-RELAT FROM LINDET7-REC2 AFTER 3
           WRITE REG-RELAT FROM LINDET8-REC2
           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT AFTER 5.
       ZERA-VLR-LIBERADO SECTION.
      *    ZERA VALOR LIBERADO NOS LANCAMENTOS DE SALARIOS DE CADA
      *    CATEGORIA
           MOVE MESANO-I           TO MESANO-BASE-CC105
                                      MESANO-BASE-CC110
                                      MESANO-BASE-CC120
                                      MESANO-BASE-CC115
           MOVE CODIGO-WK          TO CODIGO-CC105 CODIGO-CC110
                                      CODIGO-CC120 CODIGO-CC115.
           EVALUATE TIPO-LCTO-W
             WHEN 1
                READ CCD105 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE ZEROS TO VALOR-LIBERADO-CC105
                     REWRITE REG-CCD105
                     END-REWRITE
                END-READ
             WHEN 2
                READ CCD120 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE ZEROS TO VALOR-LIBERADO-CC120
                     REWRITE REG-CCD120
                     END-REWRITE
                END-READ
             WHEN 3
                READ CCD115 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE ZEROS TO VALOR-LIBERADO-CC115
                     REWRITE REG-CCD115
                     END-REWRITE
                END-READ
             WHEN 4
                READ CCD110 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE ZEROS TO VALOR-LIBERADO-CC110
                     REWRITE REG-CCD110
                     END-REWRITE
                END-READ
           END-EVALUATE.

       ATUALIZA-CTA-CORRENTE SECTION.
      *    LANÇA PAGAMENTO NO CONTA CORRENTE(CCD100)

           INITIALIZE REG-CCD100
           MOVE VALOR-APAGAR-WK    TO VALOR-CC100
           MOVE 1                  TO CRED-DEB-CC100
           MOVE DATA-DIA-INV       TO DATA-MOVTO-CC100
                                      DATA-EMISSAO-CC100
           MOVE GS-TIPO-LCTO(1: 1) TO TIPO-LCTO-CC100
           MOVE ZEROS              TO TIPO-FORN-CC100
           MOVE NR-CHEQUE          TO NR-DOCTO-CC100
           MOVE GS-VENCTO          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV           TO DATA-VENCTO-CC100
           MOVE "SALARIO - MES: "  TO DESCRICAO-CC100(1: 15)
           MOVE GS-MESANO-BASE     TO MESANO-E
           MOVE MESANO-E           TO DESCRICAO-CC100(16: 15)
           MOVE 0101               TO NR-PARCELA-CC100
           MOVE ZEROS TO SITUACAO-CC100 LIBERADO-CC100
             JUROS-PAGO-CC100 TIPO-MOEDA-CC100
             MULTA-PAGA-CC100 DESCONTO-CC100 VALOR-PAGO-CC100
             SEQ-CAIXA-CC100 DATA-PGTO-CC100
           MOVE ZEROS              TO CODREDUZ-APUR-CC100
           MOVE USUARIO-W          TO RESPONSAVEL-CC100
                                      DIGITADOR-CC100
           MOVE CODIGO-WK TO FORNEC-CC100 FORNEC-CC101
           PERFORM ATUALIZA-SEQ-CCD101
           MOVE SEQ-CC101          TO SEQ-CC100
           WRITE REG-CCD100 INVALID KEY
                 MOVE "CCD100" TO GS-MENSAGEM-ERRO(15: 7)
                 MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 2)
                 PERFORM ERRO-GRAVACAO
           END-WRITE.
       ATUALIZA-SEQ-CCD101 SECTION.
           READ CCD101 INVALID KEY
                MOVE 1 TO SEQ-CC101
                WRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CC101
                  REWRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD101" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                  END-REWRITE
           END-READ.
       ATUALIZA-CONTAS-PAGAR SECTION.
           EVALUATE TIPO-LCTO-W
             WHEN 1 MOVE 1363 TO FORNEC-CP20
             WHEN 2 MOVE 1365 TO FORNEC-CP20
             WHEN 3 MOVE 1895 TO FORNEC-CP20
             WHEN 4 MOVE 1364 TO FORNEC-CP20
           END-EVALUATE.
      *    codigo 1363: codigo de funcionarios no cadastro geral
      *    codigo 1364: codigo de REPORTAGEM   no cadastro geral
      *    codigo 1362: codigo de VENDEDOR     no cadastro geral
      *    codigo 1895: codigo de REPRESENTANTE no cadastro geral
           MOVE GS-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-I
           MOVE DATA-INV(1: 6) TO DATA-VENCTO-CP20(1: 6)
           MOVE 01             TO DATA-VENCTO-CP20(7: 2)
           MOVE ZEROS          TO SITUACAO-CP20.
      *    estornar todas as previsões acima da data de vencimento
           START CPD020 KEY IS NOT < ALT4-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
               NOT AT END
                 EVALUATE TIPO-LCTO-W
                 WHEN 1
                   IF SITUACAO-CP20 > 0 OR FORNEC-CP20 NOT = 1363
                      MOVE "10" TO ST-CPD020
                 WHEN 2
                   IF SITUACAO-CP20 > 0 OR FORNEC-CP20 NOT = 1365
                      MOVE "10" TO ST-CPD020
                 WHEN 3
                   IF SITUACAO-CP20 > 0 OR FORNEC-CP20 NOT = 1895
                      MOVE "10" TO ST-CPD020
                 WHEN 4
                   IF SITUACAO-CP20 > 0 OR FORNEC-CP20 NOT = 1364
                      MOVE "10" TO ST-CPD020
                 END-EVALUATE
                 IF ST-CPD020 NOT = "10"
                   IF NR-DOCTO-CP20 = "SALARIO"
                      MOVE "2"           TO SITUACAO-CP20
      *               SITUACAO-CP20 = 2 (PAGA)
                      MOVE TOT-VALOR-SEL TO VALOR-LIQ-CP20
                      MOVE DATA-DIA-INV  TO DATA-PGTO-CP20
                      MOVE SEQ-CAIXA     TO SEQ-CAIXA-CP20
                      REWRITE REG-CPD020
                      END-REWRITE
                      COMPUTE VALOR-TOT-CP20 = VALOR-TOT-CP20 -
                                               VALOR-LIQ-CP20
                      IF VALOR-TOT-CP20 NOT = ZEROS
                         PERFORM GRAVA-VLR-PARCIAL
                      END-IF
                      MOVE "10" TO ST-CPD020
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
       ACHA-SEQ-APAGAR SECTION.
           OPEN I-O CPD021.
           READ CPD021 INVALID KEY MOVE 1 TO SEQ-CP21
                WRITE REG-CPD021
                END-WRITE
             NOT INVALID KEY ADD 1 TO SEQ-CP21
                             REWRITE REG-CPD021.
           CLOSE CPD021.
       GRAVA-VLR-PARCIAL SECTION.
           EVALUATE TIPO-LCTO-W
             WHEN 1 MOVE 1363 TO FORNEC-CP21
             WHEN 2 MOVE 1365 TO FORNEC-CP21
             WHEN 3 MOVE 1895 TO FORNEC-CP21
             WHEN 4 MOVE 1364 TO FORNEC-CP21
           END-EVALUATE.
           PERFORM ACHA-SEQ-APAGAR.
           MOVE SEQ-CP21            TO SEQ-CP20.
           MOVE "0"                 TO SITUACAO-CP20.
           MOVE ZEROS               TO SEQ-CAIXA-CP20.
           MOVE ZEROS TO JURO-PAGO-CP20 MULTA-PAGA-CP20 DESCONTO-CP20
                         DATA-PGTO-CP20 VALOR-LIQ-CP20.
           MOVE USUARIO-W           TO DIGITADOR-CP20.
           WRITE REG-CPD020 INVALID KEY
               ADD 1 TO SEQ-CP21 SEQ-CP20
               REWRITE REG-CPD021
               END-REWRITE
               WRITE REG-CPD020 INVALID KEY
                 PERFORM ERRO-GRAVACAO
               END-WRITE.
       POPUP-BANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING(49: 6) TO GS-NR-BANCO.
           PERFORM LER-BANCO.
       LER-BANCO SECTION.
           OPEN INPUT CBD001.
           MOVE GS-NR-BANCO TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY MOVE SPACES TO TITULAR-CB01.
           MOVE TITULAR-CB01 TO GS-DESC-BANCO.
           CLOSE CBD001.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO GS-TOTAL-A-PAGAR GS-TOTAL-LIBERADO.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO SALARIO1-TOT SALARIO2-TOT TOT-CREDITO-TOT
                         SALDO-CTACORR-TOT SALDO-APAGAR-TOT
                         VALOR-APAGAR-TOT CTACORR-LIQUIDO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                          MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                MOVE CODIGO-WK        TO GS-LINDET(1: 7)
                MOVE NOME-WK          TO GS-LINDET(8: 31)
                MOVE SALARIO1-WK      TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(39: 12)
                ADD SALARIO1-WK       TO SALARIO1-TOT
                MOVE SALARIO2-WK      TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(51: 12)
                ADD SALARIO2-WK       TO SALARIO2-TOT
                MOVE TOT-CREDITO-WK   TO VALOR-E
                ADD TOT-CREDITO-WK    TO TOT-CREDITO-TOT
                MOVE VALOR-E          TO GS-LINDET(63: 12)
                ADD SALDO-CTACORR-WK  TO SALDO-CTACORR-TOT
                ADD SALDO-CTACORR-WK  TO CTACORR-LIQUIDO
                MOVE SALDO-CTACORR-WK TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(75: 13)
                IF VALOR-APAGAR-WK = ZEROS
                             MOVE "N" TO GS-LINDET(88: 2)
                ELSE MOVE "S"         TO GS-LINDET(88: 2)
                END-IF
                IF PAGAR-WK = 1 MOVE "X" TO GS-LINDET(90: 2)
                ELSE MOVE " " TO GS-LINDET(90: 2)
                END-IF
                MOVE VALOR-APAGAR-WK  TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(92: 13)
                ADD VALOR-APAGAR-WK   TO VALOR-APAGAR-TOT
                MOVE CODIGO-WK        TO GS-LINDET(114: 06)
                MOVE SEQ-WK           TO GS-LINDET(121: 05)
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM TOTALIZA.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE "TOTAL..."        TO GS-LINTOT(1: 10)
           MOVE SALARIO1-TOT      TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(32: 12)
           MOVE SALARIO2-TOT      TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(44: 12)
           MOVE TOT-CREDITO-TOT   TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(56: 12)
           MOVE SALDO-CTACORR-TOT TO VALOR-E1
           MOVE VALOR-E1          TO GS-LINTOT(68: 13)
           MOVE VALOR-APAGAR-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-LINTOT(85: 13)
           MOVE ZEROS             TO GS-LINTOT(110: 21).
           MOVE "INSERE-TOTAL"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP205" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                  MOVE CODIGO-WK        TO LINDET-REL(1: 7)
                  MOVE NOME-WK          TO LINDET-REL(8: 31)
                  MOVE SALARIO1-WK      TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(39: 12)
                  MOVE SALARIO2-WK      TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(51: 12)
                  MOVE TOT-CREDITO-WK   TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(63: 12)
                  MOVE SALDO-CTACORR-WK TO VALOR-E1
                  MOVE VALOR-E1         TO LINDET-REL(75: 13)
                  IF VALOR-APAGAR-WK = ZEROS
                               MOVE "N" TO GS-LINDET(88: 2)
                  ELSE MOVE "S"         TO LINDET-REL(88: 2)
                  END-IF
                  IF PAGAR-WK = 1 MOVE "X" TO LINDET-REL(90: 2)
                  ELSE MOVE " " TO LINDET-REL(90: 2)
                  END-IF
                  MOVE VALOR-APAGAR-WK  TO VALOR-E1
                  MOVE VALOR-E1         TO LINDET-REL(92: 12)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.
           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINDET-REL.
           MOVE "TOTAL..."        TO LINDET-REL(1: 10)
           MOVE SALARIO1-TOT      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(32: 12)
           MOVE SALARIO2-TOT      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(44: 12)
           MOVE TOT-CREDITO-TOT   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(56: 12)
           MOVE SALDO-CTACORR-TOT TO VALOR-E1
           MOVE VALOR-E1          TO LINDET-REL(68: 13)
           MOVE VALOR-APAGAR-TOT  TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(85: 12)
           WRITE REG-RELAT FROM LINDET.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-VENCTO TO VENCTO-REL.
           MOVE GS-MESANO-BASE TO MESANO-BASE-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CGD004 CCD010 CCD100 CCD105 CCD110 CCD115
                 CCD120 WORK CCD001.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
