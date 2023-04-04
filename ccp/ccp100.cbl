       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP100.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 11/12/1998
      *FUNÇÃO: Movimento de Contas Correntes

      * Se a descricao = "somente programado - cta.pagar"
      *   nao permitir alteracao do campo "descricao"
      *   e fazer as alterações no contas a pagar também


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CAPX004.
           COPY CAPX019.
           COPY CGPX001.
           COPY CXPX020.
           COPY CCPX100.
           COPY CCPX101.
           COPY LOGCCD.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE1-WK
                  ALTERNATE RECORD KEY IS CHAVE2-WK = NOME-WK
                                                    FORNEC-WK
                                                       SEQ-WK
                  ALTERNATE RECORD KEY IS CHAVE3-WK =
                                                  DESCRICAO-WK
                                                       NOME-WK
                                                     FORNEC-WK
                                                        SEQ-WK
                  ALTERNATE RECORD KEY IS CHAVE4-WK = VALOR-WK
                                                       NOME-WK
                                                     FORNEC-WK
                                                        SEQ-WK.

      *    COPY CPPX020.
      *    COPY CIEPX001.
      *    COPY CIEPX010.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CAPW004.
       COPY CAPW019.
       COPY CGPW001.
       COPY CXPW020.
       COPY CCPW100.
       COPY CCPW101.
       COPY LOGCCD.FD.
      *COPY CPPW020.
      *COPY CIEPW001.
      *COPY CIEPW010.

       FD WORK.
       01 REG-WORK.
          05 CHAVE1-WK.
             10 FORNEC-WK                  PIC 9(06).
             10 SEQ-WK                     PIC 9(05).
          05 DESCRICAO-WK                  PIC X(30).
          05 VALOR-WK                      PIC 9(08)V99.
          05 NOME-WK                       PIC X(60).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP100.CPB".
           COPY "CCP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD019             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
      *    05  ST-CPD020             PIC XX       VALUE SPACES.
      *    05  ST-CIED001            PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  MESANO-INV            PIC 9(6)     VALUE ZEROS.
           05  MESANO-GS             PIC 9(6)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  DOCTO-W               PIC X(10)    VALUE SPACES.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  DATAWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 99.
               10  DIA-WI            PIC 99.
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  DATAWII.
               10  ANO-WII           PIC 9(4).
               10  MES-WII           PIC 99.
               10  DIA-WII           PIC 99.
           05  DATA-WII REDEFINES DATAWII PIC 9(8).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9999.
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).

           05  TIPO-GRAVACAO-W       PIC 9        VALUE ZEROS.
           05  FORNEC-E              PIC ZZZZZZ   VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZZ.ZZZ.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE CONTAS CORRENTES".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "FORNEC.  SEQUEN  TIP  DOCUMENTO  DESCRICAO
      -    "    PO  SITUACAO  LIB  APUR.  MOEDA   RESP.  DIGIT   SEQ.CAI
      -    "XA TC CONTAB".
       01  CAB05.
           05  FILLER              PIC X(130)  VALUE
           "PARC.    EMISSAO VENCIMENTO TX.APL MULTA-ATRA JUROS-MORA   V
      -    "ALOR-TOTAL   DATA-PAGTO  VLR-JUROS  VLR-MULTA   DESCONTO   V
      -    "LR-LIQUIDO".
       01  LINDET.
           05  FORNECEDOR-REL      PIC ZZZ.ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  SEQ-REL             PIC ZZ.ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  TIPO-FORN-REL       PIC ZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DOCUMENTO-REL       PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  DESCRICAO-REL       PIC X(30)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  SITUACAO-REL        PIC 9       VALUE ZEROS.
           05  FILLER              PIC X       VALUE "-".
           05  DESC-SITUACAO-REL   PIC X(6)    VALUE SPACES.
           05  FILLER              PIC XX      VALUE SPACES.
           05  LIBERADO-REL        PIC XXX     VALUE SPACES.
           05  FILLER              PIC XX      VALUE SPACES.
           05  APURACAO-REL        PIC ZZZZZ.
           05  FILLER              PIC XX      VALUE SPACES.
           05  MOEDA-REL           PIC XXXXX   VALUE ZEROS.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  RESPONSAVEL-REL     PIC X(5)    VALUE SPACES.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DIGITADOR-REL       PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  SEQ-CAIXA-REL       PIC ZZZ     BLANK WHEN ZEROS.
           05  FILLER              PIC X(05)   VALUE SPACES.
           05  TIPO-CONTA-REL      PIC 9       VALUE ZEROS.
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  CONTABILIZADO-REL   PIC 9       VALUE ZEROS.
       01  LINDET1.
           05  PARCELA-REL         PIC 99/99   VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  DATA-EMISSAO-REL    PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VENCTO-REL          PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR-TOTAL-REL     PIC ZZ.ZZZ.ZZZ,ZZ VALUE ZEROS.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DEB-CRED-REL        PIC X       VALUE SPACES.
           05  FILLER              PIC XX      VALUE SPACES.
           05  DATA-PAGTO-REL      PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU    PIC 9(04).
             10 WS-MES-CPU    PIC 9(02).
             10 WS-DIA-CPU    PIC 9(02).
          05 FILLER           PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       01 lnkIP                pic x(20).
       01 lnkMac               pic x(20).
       01 lnkTipoIp            pic x(20).
       01 lnkHost              pic x(20).

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           call   "PEGAIP" using lnkIp lnkMac lnkTipoIp lnkHost
           cancel "PEGAIP"

      *    display "lnkIP       => " at 0101
      *    display  lnkIp            at 0116
      *    display "lnkMac      => " at 0201
      *    display  lnkMac           at 0216
      *    display "lnkTipoIp   => " at 0301
      *    display  lnkTipoIp        at 0316
      *    display "lnkHost     => " at 0401
      *    display  lnkHost          at 0416  stop " "

           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO DATA-DIA-INV
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD019.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "LOGCCD" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOGCCD.
      *    MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020.
      *    MOVE "CIED001" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED001.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           OPEN I-O   CCD100 CCD101 LOGCCD
           CLOSE      CCD100 CCD101 LOGCCD
           OPEN INPUT CCD100 CCD101 LOGCCD

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

      *    OPEN I-O CIED001 CIED010.
           OPEN INPUT CGD001 CXD020 CAD002 CAD019 CAD004.
           IF ST-LOGCCD = "35"
              CLOSE LOGCCD      OPEN OUTPUT LOGCCD
              CLOSE LOGCCD      OPEN I-O    LOGCCD
           END-IF.
           IF ST-CCD100 = "35"
              CLOSE CCD100      OPEN OUTPUT CCD100
              CLOSE CCD100      OPEN I-O CCD100
           END-IF.
           IF ST-CCD101 = "35"
              CLOSE CCD101      OPEN OUTPUT CCD101
              CLOSE CCD101      OPEN I-O CCD101
           END-IF.
      *    IF ST-CIED001 = "35"
      *       CLOSE CIED001     OPEN OUTPUT CIED001
      *       CLOSE CIED001     OPEN I-O CIED001
      *    END-IF.
      *    IF ST-CIED010 = "35"
      *       CLOSE CIED010     OPEN OUTPUT CIED010
      *       CLOSE CIED010     OPEN I-O CIED010
      *    END-IF.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD019 <> "00"
              MOVE "ERRO ABERTURA CAD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD101 <> "00"
              MOVE "ERRO ABERTURA CCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOGCCD <> "00"
              MOVE "ERRO ABERTURA LOGCCD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOGCCD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA31"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITAR-EXCLUIR" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITAR-EXCLUIR" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-READ.



       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-VERIF-DATA-ATRAS-TRUE
                    PERFORM VERIFICA-DATA-ATRASADA
               WHEN GS-SAVE-FLG-TRUE
                   IF GS-PARCELA = 1
                      PERFORM GRAVA-PARCELAS
                   ELSE
                      PERFORM SALVAR-DADOS
                      IF GS-TIPO-GRAVACAO = 1
                         PERFORM REGRAVA-DADOS
                      ELSE
                         PERFORM GRAVA-DADOS
                      END-IF
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
      *        WHEN GS-LOAD-FLG-TRUE
      *            PERFORM CARREGAR-DADOS
      *            MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   MOVE 2 TO SITUACAO-CC100
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 6) TO FORNEC-CC100
                   MOVE GS-LINDET(8: 5) TO SEQ-CC100
                   PERFORM CARREGAR-DADOS
               WHEN GS-DIVIDE-PARCELA-TRUE
                   PERFORM DIVIDE-PARCELAS
               WHEN GS-VERIF-TOT-PARC-TRUE
                   PERFORM VERIFICA-TOTAL-PARCELA
               WHEN GS-LE-FORNEC-TRUE
                   PERFORM LE-FORNEC
               WHEN GS-LE-TIPO-FORNEC-TRUE
                   PERFORM LE-TIPO-FORNEC
               WHEN GS-LE-COD-APURACAO-TRUE
                   PERFORM LE-COD-APURACAO
      *        WHEN GS-CHAMAR-APURACAO-TRUE
      *            PERFORM CHAMAR-APURACAO
               WHEN GS-CARREGA-DATA-TRUE
                   PERFORM CARREGA-DATA
               WHEN GS-EMISSAO-VENCTO-TRUE
                   PERFORM INVERTE-EMIS-VENCTO
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO-FORN
               WHEN GS-MOSTRAR-CAIXA-TRUE
                    PERFORM MOSTRAR-CAIXA
               WHEN GS-CARREGA-TRUE
                    PERFORM CARREGAR
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004

           MOVE "SENHA31"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "DESABILITAR-EXCLUIR" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE "HABILITAR-EXCLUIR" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-READ.

       MOSTRAR-CAIXA SECTION.
           MOVE GS-LINDET(1: 6) TO FORNEC-CC100
           MOVE GS-LINDET(8: 5) TO SEQ-CC100
           START CCD100 KEY IS = CHAVE-CC100 INVALID KEY
                 CONTINUE.
           READ CCD100 INVALID KEY
                INITIALIZE REG-CCD100.

           MOVE DATA-MOVTO-CC100   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DATA-CAIXA.

           MOVE SEQ-CAIXA-CC100    TO GS-SEQ-CAIXA.


       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *CHAMAR-APURACAO SECTION.
      *    CALL "CXP020T" USING PASSAR-PARAMETROS
      *    CANCEL "CXP020T"
      *    MOVE PASSAR-STRING-1(52: 3) TO GS-COD-APURACAO
      *    PERFORM LE-COD-APURACAO.
       VERIFICA-DATA-ATRASADA SECTION.
           MOVE DATA-DIA-INV(1: 6) TO MESANO-INV.
           MOVE 0 TO GS-DATA-ATRASADA.
           MOVE GS-DATA-MOVTO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV(1: 6) TO MESANO-GS.
      *    IF MESANO-GS < MESANO-INV
      *       MOVE 1 TO GS-DATA-ATRASADA.
           MOVE GS-DATA-VENCTO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV(1: 6) TO MESANO-GS.
      *    IF MESANO-GS < MESANO-INV
      *       MOVE 1 TO GS-DATA-ATRASADA.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-FORNEC
             WHEN 2 PERFORM CARREGA-POP-UP-TIPO-FORNEC
             WHEN 4 PERFORM CARREGA-POP-UP-APURACAO
           END-EVALUATE.
       ITEM-SELECIONADO-TIPO-FORN SECTION.
           MOVE GS-LINDET1(33: 2)TO GS-TIPO-FORN
           MOVE GS-LINDET1(1: 30) TO GS-DESCR-TIPO-FORN.
       CARREGA-POP-UP-TIPO-FORNEC SECTION.
           MOVE SPACES TO NOME-TIPO.
           START CAD019 KEY IS NOT < NOME-TIPO INVALID KEY
                 MOVE "10" TO ST-CAD019.
           PERFORM UNTIL ST-CAD019 = "10"
              READ CAD019 NEXT RECORD AT END
                   MOVE "10" TO ST-CAD019
              NOT AT END
                   MOVE NOME-TIPO     TO GS-LINDET1(1: 32)
                   MOVE CODIGO-TIPO   TO GS-LINDET1(33: 02)
                   MOVE "INSERE-POP-UP-TFORN" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO-FORN SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 2 PERFORM ITEM-SELECIONADO-TIPO-FORN
             WHEN 4 PERFORM ITEM-SELECIONADO-APURACAO
             WHEN OTHER
                    MOVE GS-LINDET1(33: 6)TO GS-COD-FORN
                    MOVE GS-LINDET1(1: 30) TO GS-DESCR-FORN
           END-EVALUATE.
       CARREGA-POP-UP-FORNEC SECTION.
           MOVE GS-LINDET1(1: 1) TO NOME-CG01 LETRA.
      *    MOVE SPACES TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
              READ CGD001 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD001
              NOT AT END
                  MOVE NOME-CG01     TO LETRA1
                  IF LETRA1 NOT = LETRA
                     MOVE "10" TO ST-CGD001
                  ELSE
                     CONTINUE
                  MOVE NOME-CG01     TO GS-LINDET1(1: 32)
                  MOVE CODIGO-CG01   TO GS-LINDET1(33: 06)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO-APURACAO SECTION.
           MOVE GS-LINDET1(52: 5)TO GS-COD-APURACAO.
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO.
       CARREGA-POP-UP-APURACAO SECTION.
           CALL   "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CXP020T"
           MOVE PASSAR-STRING-1(52: 5) TO GS-COD-APURACAO
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20 TO GS-DESCR-APURACAO.
       INVERTE-EMIS-VENCTO SECTION.
           MOVE GS-DATA-EMISSAO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-EMISSAO-INV.
           MOVE GS-DATA-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO GS-VENCTO-INV.
       CARREGA-DATA SECTION.
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           MOVE "CARREGAR-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           CLOSE      CCD100 LOGCCD
           OPEN I-O   CCD100 LOGCCD

           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           ACCEPT WS-HORA-SYS FROM TIME

           REWRITE REG-CCD100 NOT INVALID KEY
               MOVE FORNEC-CC100       TO LOGCCD-FORNEC
               MOVE SEQ-CC100          TO LOGCCD-SEQ
               MOVE USUARIO-W          TO LOGCCD-USUARIO
               MOVE WS-DATA-CPU        TO LOGCCD-DATA
               MOVE WS-HORA-SYS        TO LOGCCD-HORA
               MOVE "CCP100"           TO LOGCCD-PROGRAMA
               MOVE lnkIp              TO LOGCCD-IP-MAQUINA
               MOVE lnkMAC             TO LOGCCD-MAC-MAQUINA
               MOVE LNKHOST            TO LOGCCD-HOST
               WRITE REG-LOGCCD INVALID KEY
                    MOVE "Erro de Gravação...LOGCCD"
                      TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM EXIBIR-MENSAGEM
               END-WRITE
           END-REWRITE.

           CLOSE      CCD100
           OPEN INPUT CCD100.
      *    IF DESCRICAO-CC100 = "SOMENTE PROGRAMADO - CTA.PAGAR"
      *       OPEN I-O CPD020
      *       MOVE DATA-VENCTO-CC100 TO DATA-VENCTO-CP20
      *       MOVE FORNEC-CC100 TO FORNEC-CP20
      *       START CPD020 KEY IS NOT < ALT1-CP20
      *             INVALID KEY MOVE "10" TO ST-CPD020
      *       END-START
      *       PERFORM UNTIL ST-CPD020 = "10"
      *         READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
      *           NOT AT END
      *             IF FORNEC-CP20 <> FORNEC-CC100 OR
      *                DATA-VENCTO-CP20 <> DATA-VENCTO-CC100
      *                 MOVE "REGISTRO NÃO ENCOTRADO NO A PAGAR"
      *
      *                      TO GS-MENSAGEM-ERRO
      *                 MOVE "ERRO-GRAVACAO-CPD020" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *                 MOVE "10" TO ST-CPD020
      *             ELSE
      *              MOVE SEQ-CC100 TO DOCTO-W
      *              IF NR-DOCTO-CP20 = DOCTO-W
      *                 MOVE 3 TO SITUACAO-CP20
      *                 REWRITE REG-CPD020
      *                 END-REWRITE
      *                 MOVE "10" TO ST-CPD020
      *              END-IF
      *             END-IF
      *         END-READ
      *       END-PERFORM
      *       CLOSE CPD020
      *    END-IF.

           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       LE-FORNEC SECTION.
           MOVE GS-COD-FORN   TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01         TO GS-DESCR-FORN.
       LE-TIPO-FORNEC SECTION.
           MOVE GS-TIPO-FORN  TO CODIGO-TIPO.
           READ CAD019 INVALID KEY MOVE "********" TO NOME-TIPO.
           MOVE NOME-TIPO     TO GS-DESCR-TIPO-FORN.
       LE-COD-APURACAO SECTION.
           MOVE GS-COD-APURACAO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****" TO DESCRICAO-CX20
                MOVE ZEROS TO TIPO-CONTA-CX20.

           MOVE DESCRICAO-CX20     TO GS-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
              MOVE "0-NORMAL"      TO GS-TIPO-CONTA
           ELSE
              MOVE "1-TOTALIZAD."  TO GS-TIPO-CONTA.
           MOVE TIPO-CONTA-CX20    TO GS-TIPO-CONTAW.
       CARREGAR-DADOS SECTION.
           START CCD100 KEY IS = CHAVE-CC100 INVALID KEY CONTINUE.
           READ CCD100 INVALID KEY INITIALIZE REG-CCD100.
           MOVE DATA-MOVTO-W       TO GS-DATA-MOVTO.
           MOVE FORNEC-CC100       TO GS-COD-FORN CODIGO-CG01.
           MOVE SEQ-CC100          TO GS-SEQ
           READ CGD001 INVALID KEY MOVE "*****" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-DESCR-FORN.
           EVALUATE TIPO-LCTO-CC100
             WHEN 00   MOVE "0-FORNECEDOR " TO GS-TIPO-CONTA
             WHEN 01   MOVE "1-FUNCIONARIO" TO GS-TIPO-CONTA
             WHEN 02   MOVE "2-VENDEDOR   " TO GS-TIPO-CONTA
             WHEN 03   MOVE "3-REPORTAGEM " TO GS-TIPO-CONTA
             WHEN 04   MOVE "4-REPRESENT. " TO GS-TIPO-CONTA
           END-EVALUATE.
           MOVE NR-DOCTO-CC100     TO GS-NR-DOCTO.
           MOVE DATA-EMISSAO-CC100 TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV           TO GS-DATA-EMISSAO.
           MOVE DATA-VENCTO-CC100  TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV           TO GS-DATA-VENCTO.
           MOVE DESCRICAO-CC100    TO GS-DESCRICAO.
           MOVE TIPO-MOEDA-CC100   TO GS-TIPO-MOEDA.
           MOVE DIGITADOR-CC100    TO GS-DIGITADOR.
           MOVE DATA-PGTO-CC100    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DATA-PGTO
      *    A VARIAVEL GS-ACESSO-DIGITADOR É PARA CONTROLAR SE A PESSOA
      *    QUE ESTÁ TENTANDO FAZER A MANUTENÇÃO É A MESMA QUE EFETUOU
      *    A ENTRADA (SENÃO NÃO SERÁ POSSIVEL A MANUTENÇÃO)
           IF DIGITADOR-CC100 NOT = USUARIO-W
              MOVE 1 TO GS-ACESSO-DIGITADOR
           ELSE MOVE 0 TO GS-ACESSO-DIGITADOR.
           EVALUATE TIPO-MOEDA-CC100
             WHEN 0 MOVE "-Real"   TO GS-TIPO-MOEDA(2: 6)
             WHEN 1 MOVE "-Dolar"  TO GS-TIPO-MOEDA(2: 5)
           END-EVALUATE
           MOVE CODREDUZ-APUR-CC100 TO GS-COD-APURACAO
                                      CODIGO-COMPL-CX20.
           READ CXD020 INVALID KEY MOVE "*****" TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20     TO GS-DESCR-APURACAO.
           MOVE VALOR-CC100         TO GS-VALOR-TOTAL.
           MOVE TIPO-FORN-CC100     TO GS-TIPO-FORN CODIGO-TIPO
           READ CAD019.
           MOVE NOME-TIPO           TO GS-DESCR-TIPO-FORN
           MOVE RESPONSAVEL-CC100   TO GS-RESPONSAVEL.
           MOVE SITUACAO-CC100      TO GS-SITUACAO.
           IF SITUACAO-CC100 = 0 MOVE "S" TO GS-SITUACAO1
           ELSE MOVE "N" TO GS-SITUACAO1.
           MOVE CRED-DEB-CC100      TO GS-DEB-CRED.
           EVALUATE CRED-DEB-CC100
             WHEN 0 MOVE "-Débito"  TO GS-DEB-CRED(2: 11)
             WHEN 1 MOVE "-Crédito" TO GS-DEB-CRED(2: 11)
           END-EVALUATE
           MOVE SEQ-CAIXA-CC100     TO GS-SEQ-CAIXA.

       CARREGAR SECTION.
           CLEAR-OBJECT LB1

           INITIALIZE REG-WORK

           EVALUATE GS-OPCAO-ORD
               WHEN 1 START WORK KEY IS NOT LESS CHAVE1-WK INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
               WHEN 2 START WORK KEY IS NOT LESS CHAVE2-WK INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
               WHEN 3 START WORK KEY IS NOT LESS CHAVE3-WK INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
               WHEN 4 START WORK KEY IS NOT LESS CHAVE4-WK INVALID KEY
                            MOVE "10" TO ST-WORK
                      END-START
           END-EVALUATE

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT AT END
                    MOVE "10" TO ST-WORK
               NOT AT END
                    MOVE FORNEC-WK         TO FORNEC-CC100
                    MOVE SEQ-WK            TO SEQ-CC100
                    READ CCD100 NOT INVALID KEY
                         PERFORM MOVER-DADOS-LISTA
                         MOVE "INSERE-LIST" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       ATUALIZA-SEQ-CCD101 SECTION.
           CLOSE      CCD101
           OPEN I-O   CCD101
           READ CCD101 INVALID KEY
                MOVE 1 TO SEQ-CC101
                WRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD101" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CC101
                  REWRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD101" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                  END-REWRITE

           END-READ
           CLOSE      CCD101
           OPEN INPUT CCD101.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CCD100
           INITIALIZE GS-DATA-BLOCK
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-PARCELA.
       DIVIDE-PARCELAS SECTION.
           COMPUTE VLR-PARCELA = GS-VALOR-TOTAL / GS-QT-PARCELA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 72
               MOVE ZEROS TO GS-VALOR(I) GS-NR(I)
                             GS-VENCTO(I)
           END-PERFORM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-PARCELA
               MOVE VLR-PARCELA       TO GS-VALOR(I)
               MOVE I                 TO GS-NR(I)
           END-PERFORM.
           COMPUTE GS-VALOR(1) = (GS-VALOR-TOTAL - (
                   GS-QT-PARCELA * VLR-PARCELA)) + VLR-PARCELA.
           MOVE GS-DATA-VENCTO     TO GS-VENCTO(1).
           PERFORM INVERTE-EMIS-VENCTO.
           MOVE GS-VENCTO-INV TO DATA-WI.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > GS-QT-PARCELA
                   ADD 1 TO MES-WI
                   IF MES-WI > 12
                      MOVE 1 TO MES-WI  ADD 1 TO ANO-WI
                   END-IF
                   MOVE 2         TO GRTIME-TYPE
                   MOVE 7         TO GRTIME-FUNCTION
                   MOVE DATA-WI   TO GRTIME-DATE DATA-WII DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV  TO GS-VENCTO(I)
                   CALL "GRTIME" USING PARAMETROS-GRTIME
                   CANCEL "GRTIME"
                   IF GRTIME-DATE-FINAL = ZEROS
                      MOVE 1      TO DIA-WII
                      ADD 1       TO MES-WII
                      IF MES-WII = 13 MOVE 01 TO MES-WII
                                      ADD 1   TO ANO-WII
                      END-IF
                      MOVE DATA-WII TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV TO GS-VENCTO(I)
                   END-IF
           END-PERFORM.
       VERIFICA-TOTAL-PARCELA SECTION.
           MOVE ZEROS TO VLR-PARCELA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GS-QT-PARCELA
              ADD GS-VALOR(I) TO VLR-PARCELA
           END-PERFORM.
           MOVE VLR-PARCELA  TO GS-VLR-TOT-PARCELA.
       SALVAR-DADOS SECTION.
           MOVE GS-DATA-MOVTO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO DATA-MOVTO-CC100.
           MOVE GS-COD-FORN       TO FORNEC-CC100
           MOVE GS-SEQ            TO SEQ-CC100
           MOVE SPACES            TO NR-DOCTO-CC100
           STRING GS-NR-DOCTO "-" I DELIMITED BY " " INTO NR-DOCTO-CC100
           MOVE GS-EMISSAO-INV    TO DATA-EMISSAO-CC100
           MOVE GS-VENCTO-INV     TO DATA-VENCTO-CC100
           MOVE SPACES            TO DESCRICAO-CC100
           STRING GS-DESCRICAO "-" I "/" GS-QT-PARCELA
             DELIMITED BY "  " INTO DESCRICAO-CC100
           IF GS-TIPO-MOEDA = SPACES MOVE "00" TO TIPO-MOEDA-CC100
           ELSE MOVE GS-TIPO-MOEDA(1: 1) TO TIPO-MOEDA-CC100.
           MOVE GS-DEB-CRED(1: 1) TO CRED-DEB-CC100.
           MOVE GS-COD-APURACAO   TO CODREDUZ-APUR-CC100
           MOVE GS-RESPONSAVEL    TO RESPONSAVEL-CC100
           MOVE PARAMETROS-W(1: 5)    TO DIGITADOR-CC100.
           IF GS-TIPO-CONTA = SPACES MOVE 0 TO TIPO-LCTO-CC100
           ELSE MOVE GS-TIPO-CONTA(1: 1) TO TIPO-LCTO-CC100.
           MOVE 0101                  TO NR-PARCELA-CC100.
           MOVE GS-VALOR-TOTAL    TO VALOR-CC100.
           MOVE GS-TIPO-FORN      TO TIPO-FORN-CC100.
       ACHA-SEQ-CIE SECTION.
      *    MOVE DATA-MOVTO-I    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
      *       NOT AT END
      *         IF DATA-CI10 NOT = DATA-MOVTO-I MOVE "10" TO ST-CIED010
      *         ELSE CONTINUE
      *      END-READ
      *    END-PERFORM.
       GRAVA-CIE SECTION.
      *    MOVE 01                  TO COD-MENS-PADRAO-CI10
      *    MOVE SPACES              TO DESCRICAO-MENS-CI10.
      *    MOVE GS-DESCR-FORN       TO DESCRICAO-MENS-CI10(1: 10)
      *    MOVE DESCRICAO-CC100     TO DESCRICAO-MENS-CI10(12: 27)
      *    MOVE DATA-VENCTO-CC100 TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV            TO DATA-E
      *    MOVE DATA-E              TO DESCRICAO-MENS-CI10(40: 11)
      *    MOVE VALOR-CC100         TO VALOR-E
      *    MOVE VALOR-E             TO DESCRICAO-MENS-CI10(51: 10)
      *    PERFORM ACHA-SEQ-CIE.
      *    MOVE DATA-MOVTO-I        TO DATA-CI10
      *    ADD 1                    TO SEQ-CI10
      *    ACCEPT HORA-W            FROM TIME.
      *    MOVE HORA-W(1: 4)        TO HORA-CI10
      *    MOVE USUARIO-W           TO ORIGEM-CI10
      *
      ** Função que exerce o destinatario
      *    MOVE 1                   TO FUNCAO-DESTINO-CI10
      **    CODIGO DO USUARIO DESTINO (KELLO)
      *    MOVE ZEROS               TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.

       GRAVA-PARCELAS SECTION.
           MOVE ZEROS TO K.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 72
               IF GS-NR(I) = ZEROS MOVE 72 TO I
               ELSE
                  PERFORM SALVAR-DADOS
                  MOVE GS-NR(I)      TO NR-PARCELA-CC100
                  MOVE GS-QT-PARCELA TO TOT-PARC-CC100
                  MOVE GS-VENCTO(I)  TO DATA-INV
                  CALL "GRIDAT2" USING DATA-INV
                  MOVE DATA-INV          TO DATA-VENCTO-CC100
                  MOVE GS-VALOR(I)       TO VALOR-CC100
                  PERFORM GRAVA-DADOS
           END-PERFORM.
       GRAVA-DADOS SECTION.
           CLOSE      CCD100
           OPEN I-O   CCD100
           MOVE ZEROS TO SITUACAO-CC100 LIBERADO-CC100 JUROS-PAGO-CC100
                MULTA-PAGA-CC100 DESCONTO-CC100 VALOR-PAGO-CC100
                SEQ-CAIXA-CC100 DATA-PGTO-CC100.
           MOVE GS-COD-FORN       TO FORNEC-CC101.
           PERFORM ATUALIZA-SEQ-CCD101.
           MOVE SEQ-CC101     TO SEQ-CC100.
           WRITE REG-CCD100 INVALID KEY
                 MOVE "CCD100"  TO GS-MENSAGEM-ERRO(15: 07)
                 MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
                 PERFORM ERRO-GRAVACAO.
           CLOSE      CCD100
           OPEN INPUT CCD100
      *    PERFORM GRAVA-CIE.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE      CCD100
           OPEN I-O   CCD100
           REWRITE REG-CCD100 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
                   CONTINUE.
           CLOSE      CCD100
           OPEN INPUT CCD100
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    PERFORM REGRAVA-PROGRAMACAO-APAGAR.
      *REGRAVA-PROGRAMACAO-APAGAR SECTION.
      *    IF DESCRICAO-CC100 = "SOMENTE PROGRAMADO - CTA.PAGAR"
      *       OPEN I-O CPD020
      *       MOVE DATA-VENCTO-CC100 TO DATA-VENCTO-CP20
      *       MOVE FORNEC-CC100 TO FORNEC-CP20
      *       START CPD020 KEY IS NOT < ALT1-CP20
      *             INVALID KEY MOVE "10" TO ST-CPD020
      *       END-START
      *       PERFORM UNTIL ST-CPD020 = "10"
      *         READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
      *           NOT AT END
      *             IF FORNEC-CP20 <> FORNEC-CC100 OR
      *                DATA-VENCTO-CP20 <> DATA-VENCTO-CC100
      *                 MOVE "REGISTRO NÃO ENCOTRADO NO A PAGAR"
      *                      TO GS-MENSAGEM-ERRO
      *                 MOVE "ERRO-GRAVACAO-CPD020" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *                 MOVE "10" TO ST-CPD020
      *             ELSE
      *              MOVE SEQ-CC100 TO DOCTO-W
      *              IF NR-DOCTO-CP20 = DOCTO-W
      *                MOVE DATA-EMISSAO-CC100 TO DATA-EMISSAO-CP20
      *                MOVE DATA-VENCTO-CC100  TO DATA-VENCTO-CP20
      *                MOVE CODREDUZ-APUR-CC100   TO CODREDUZ-APUR-CP20
      *                MOVE RESPONSAVEL-CC100  TO RESPONSAVEL-CP20
      *                MOVE USUARIO-W          TO DIGITADOR-CP20
      *                MOVE VALOR-CC100        TO VALOR-TOT-CP20
      *                REWRITE REG-CPD020
      *                END-REWRITE
      *                MOVE "10" TO ST-CPD020
      *              END-IF
      *         END-READ
      *       END-PERFORM
      *       CLOSE CPD020
      *    END-IF.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CARREGA-ULTIMOS SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK
           MOVE GS-TIPO-GRAVACAO TO TIPO-GRAVACAO-W.
      *    GUARDAR O GS-TIPO-GRAVACAO POIS SERA CLAREADO
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA-MOVTO TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO DATA-MOVTO-CC100 DATA-MOVTO-I.
           START CCD100 KEY IS NOT < DATA-MOVTO-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-CCD100 = "10"
                 READ CCD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CCD100
                 NOT AT END
                      IF DATA-MOVTO-CC100 NOT = DATA-MOVTO-I
                         MOVE "10" TO ST-CCD100
                      ELSE
                         IF SITUACAO-CC100 = 1 OR SITUACAO-CC100 = 2
                            CONTINUE
                         ELSE
                            PERFORM MOVER-DADOS-LISTA

                            MOVE FORNEC-CC100 TO FORNEC-WK
                                                 CODIGO-CG01
                            READ CGD001 INVALID KEY
                                 INITIALIZE REG-CGD001
                            END-READ
                            MOVE NOME-CG01    TO NOME-WK
                            MOVE SEQ-CC100    TO SEQ-WK
                            MOVE VALOR-CC100  TO VALOR-WK
                            MOVE DESCRICAO-CC100 TO DESCRICAO-WK
                            WRITE REG-WORK INVALID KEY
                                MOVE "Erro de Gravação...WORK" TO
                                MENSAGEM
                                MOVE "C" TO TIPO-MSG
                                PERFORM EXIBIR-MENSAGEM
                            END-WRITE

                            MOVE "INSERE-LIST" TO DS-PROCEDURE
                            PERFORM CALL-DIALOG-SYSTEM
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           CLOSE      WORK
           OPEN INPUT WORK

           MOVE TIPO-GRAVACAO-W TO GS-TIPO-GRAVACAO.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES               TO GS-LINDET
           MOVE FORNEC-CC100         TO FORNEC-E
                                        CODIGO-CG01
           MOVE FORNEC-CC100         TO GS-LINDET(01: 07)
           MOVE SEQ-CC100            TO SEQ-E
           READ CGD001 INVALID KEY
                MOVE "************"  TO NOME-CG01.
           MOVE SEQ-CC100            TO GS-LINDET(08: 06)
           MOVE NOME-CG01(1: 10)     TO GS-LINDET(14: 11)
           MOVE DESCRICAO-CC100      TO GS-LINDET(25: 31)
           MOVE NR-DOCTO-CC100       TO GS-LINDET(56: 11)
           MOVE TIPO-LCTO-CC100      TO GS-LINDET(67: 04)
           MOVE SITUACAO-CC100       TO GS-LINDET(74: 02)
           MOVE CODREDUZ-APUR-CC100  TO GS-LINDET(76: 06)
           MOVE RESPONSAVEL-CC100    TO GS-LINDET(82: 06)
           MOVE DATA-VENCTO-CC100    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO GS-LINDET(88: 11)
           MOVE VALOR-CC100          TO VALOR-E
           MOVE VALOR-E              TO GS-LINDET(99: 10).
           IF CRED-DEB-CC100 = 0
              MOVE "D"               TO GS-LINDET(109: 1)
           ELSE
              MOVE "C"               TO GS-LINDET(109: 1).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE DATA-MOVTO-I TO DATA-MOVTO-CC100.
           START CCD100 KEY IS = DATA-MOVTO-CC100 INVALID KEY
                 MOVE "10" TO ST-CCD100.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-CCD100 = "10"
               READ CCD100 NEXT RECORD AT END
                    MOVE "10" TO ST-CCD100
               NOT AT END
                    IF DATA-MOVTO-CC100 NOT = DATA-MOVTO-I
                       MOVE "10"                    TO ST-CCD100
                    ELSE
                       MOVE FORNEC-CC100            TO FORNECEDOR-REL
                       MOVE SEQ-CC100               TO SEQ-REL
                       MOVE TIPO-LCTO-CC100         TO TIPO-FORN-REL
                       MOVE NR-DOCTO-CC100          TO DOCUMENTO-REL
                       MOVE DESCRICAO-CC100         TO DESCRICAO-REL
                       MOVE SITUACAO-CC100          TO SITUACAO-REL
                       EVALUATE SITUACAO-CC100
                          WHEN 0 MOVE "0K"          TO DESC-SITUACAO-REL
                          WHEN 1 MOVE "PAGA"        TO DESC-SITUACAO-REL
                          WHEN 2 MOVE "ESTORN"      TO DESC-SITUACAO-REL
                       END-EVALUATE
                       EVALUATE LIBERADO-CC100
                          WHEN 0 MOVE "NAO"         TO LIBERADO-REL
                          WHEN 1 MOVE "SIM"         TO LIBERADO-REL
                       END-EVALUATE
                       MOVE CODREDUZ-APUR-CC100     TO APURACAO-REL
                       EVALUATE TIPO-MOEDA-CC100
                          WHEN 0 MOVE "REAL "       TO MOEDA-REL
                          WHEN 1 MOVE "DOLAR"       TO MOEDA-REL
                       END-EVALUATE
                       MOVE RESPONSAVEL-CC100       TO RESPONSAVEL-REL
                       MOVE DIGITADOR-CC100         TO DIGITADOR-REL
                       MOVE SEQ-CAIXA-CC100         TO SEQ-CAIXA-REL
                       MOVE NR-PARCELA-CC100        TO PARCELA-REL
                       MOVE DATA-EMISSAO-CC100      TO DATA-INV
                       CALL "GRIDAT1" USING DATA-INV
                       MOVE DATA-INV                TO DATA-EMISSAO-REL
                       MOVE DATA-VENCTO-CC100       TO DATA-INV
                       CALL "GRIDAT1" USING DATA-INV
                       MOVE DATA-INV                TO VENCTO-REL
                       MOVE VALOR-CC100             TO VALOR-TOTAL-REL
                       MOVE DATA-PGTO-CC100         TO DATA-INV
                       CALL "GRIDAT1" USING DATA-INV
                       MOVE DATA-INV               TO DATA-PAGTO-REL

                       WRITE REG-RELAT FROM LINDET
                       WRITE REG-RELAT FROM LINDET1
                       ADD 2 TO LIN
                       IF LIN > 56
                          PERFORM CABECALHO
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.

           copy descondensa.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD100 CCD101 CGD001 CAD019
                 CXD020 CAD002 CAD004 LOGCCD.
      *    CLOSE CIED001 CIED010.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
