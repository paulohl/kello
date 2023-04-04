       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP204.
      *DATA: 03/02/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relatório de Aprovação - De créditos(salários) e CC
      *FUNÇÃO: -Tem por função, fazer as liberações de valores para
      *        pagamentos e gravar no ccd105 ou ccd110 CCD115 CCD120.

      *        -Gravar no contas a pagar o total do valor de salários
      *        com 12 parcelas previstas(conta permanente)
      *        a pedido do vonei, foi eliminada a atualização no CP
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX001.
           COPY CCPX010.
           COPY CCPX100.
           COPY CCPX101.
           COPY CCPX105.
           COPY CCPX110.
           COPY CCPX115.
           COPY CCPX120.
           COPY CPPX020.
           COPY CPPX021.
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
       COPY CCPW001.
       COPY CGPW001.
      *COPY CIEPW010.
       COPY CCPW010.
       COPY CCPW100.
       COPY CCPW101.
       COPY CCPW105.
       COPY CCPW110.
       COPY CCPW115.
       COPY CCPW120.
       COPY CPPW020.
       COPY CPPW021.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  SEQ-WK              PIC 9(5).
           05  NOME-WK             PIC X(30).
           05  SALARIO1-WK         PIC 9(7)V99.
           05  SALARIO2-WK         PIC 9(7)V99.
           05  TOT-CREDITO-WK      PIC 9(7)V99.
           05  SALDO-CTACORR-WK    PIC S9(7)V99.
           05  VALOR-APAGAR-WK     PIC 9(7)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP204.CPB".
           COPY "CCP204.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  ST-CCD010             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-CCD110             PIC XX       VALUE SPACES.
           05  ST-CCD115             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  CODIGO-W              PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  VALOR-E               PIC ZZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZZZ.ZZZ,ZZ-.
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
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
           05  FILLER              PIC X(62)   VALUE
           "RELATORIO DE APROVACAO DE PAGAMENTO - CONTA CORRENTE".
           05  FILLER              PIC X(15)   VALUE "       VENCTO: ".
           05  VENCTO-REL          PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "MESANO-BASE: ".
           05  MESANO-BASE-REL     PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "NOME                            CREDITO(1)  CREDITO(2) CTA.C
      -    "ORRENTE SALDO-APAGAR LIB VALOR-APAGAR".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
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
           MOVE "CCD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD010.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CCD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD105.
           MOVE "CCD110"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD110.
           MOVE "CCD115"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD115.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD021.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           OPEN INPUT CCD100 CGD001 CCD105 CCD010 CCD110 CCD115
                      CCD120 CCD001.
      *    OPEN I-O CIED010.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
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
      *     MOVE 1 TO COD-USUARIO-W.
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
               WHEN GS-LIBERACAO-TRUE
                    PERFORM ATUALIZA-LIBERACAO
               WHEN GS-LIBERAR-PGTO-TRUE
                    PERFORM LIBERAR-PGTO
               WHEN GS-ALTERA-VLR-APAGAR-TRUE
                    PERFORM ALTERA-VLR-APAGAR
               WHEN GS-ATUALIZA-CTAPAGAR-TRUE
                    PERFORM ATUALIZA-CONTAS-APAGAR
               WHEN GS-ALTER-VLR-APAGAR1-TRUE
                    PERFORM ALTERA-VLR-APAGAR1
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
       ATUALIZA-LIBERACAO SECTION.
      * ****************
           MOVE GS-SENHA TO SENHA-WW.
           IF SENHA-WW = SENHA-W
              EVALUATE TIPO-LCTO-W
                 WHEN 1 PERFORM ATUALIZA-LIBERACAO-CCD105
                 WHEN 2 PERFORM ATUALIZA-LIBERACAO-CCD120
                 WHEN 3 PERFORM ATUALIZA-LIBERACAO-CCD115
                 WHEN 4 PERFORM ATUALIZA-LIBERACAO-CCD110
              END-EVALUATE
           ELSE MOVE "TELA-ATUALIZA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
       ATUALIZA-CONTAS-APAGAR SECTION.
      **    ATENÇÃO: O nr-docto-cp20 deverá ser = "SALARIO" pois é o
      **             controle de lctos permanentes de previsão de
      **             contas a pagar

      *    OPEN I-O CPD020 CPD021.
      *    EVALUATE TIPO-LCTO-W
      *      WHEN 1 MOVE 1363 TO FORNEC-CP20 CODIGO-W
      *      WHEN 2 MOVE 1365 TO FORNEC-CP20 CODIGO-W
      *      WHEN 3 MOVE 1895 TO FORNEC-CP20 CODIGO-W
      *      WHEN 4 MOVE 1364 TO FORNEC-CP20 CODIGO-W
      *    END-EVALUATE.
      **    codigo 1363: codigo de funcionarios no cadastro geral
      **    codigo 1364: codigo de fotog/cinegr no cadastro geral
      **    codigo 1895: codigo de REPRESENTANTE no cadastro geral
      **    codigo 1365: codigo de VENDEDOR     no cadastro geral
      *    MOVE GS-VENCTO TO DATA-INV.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    MOVE DATA-INV       TO DATA-I
      *    MOVE DATA-INV(1: 6) TO DATA-VENCTO-CP20(1: 6)
      *    MOVE 01             TO DATA-VENCTO-CP20(7: 2)
      *    MOVE ZEROS          TO SITUACAO-CP20.
      *
      **    estornar todas as previsões acima da data de vencimento
      *    START CPD020 KEY IS NOT < ALT4-CP20 INVALID KEY
      *          MOVE "10" TO ST-CPD020.
      *    PERFORM UNTIL ST-CPD020 = "10"
      *      READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
      *        NOT AT END
      *          IF SITUACAO-CP20 > 0 OR FORNEC-CP20 NOT = CODIGO-W
      *             MOVE "10" TO ST-CPD020
      *          ELSE
      *            IF NR-DOCTO-CP20 = "SALARIO"
      *               DELETE CPD020
      *            ELSE CONTINUE
      *            END-IF
      *          END-IF
      *      END-READ
      *    END-PERFORM.
      **    gravar as 12 parcelas permanentes - previstas
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
      *       INITIALIZE REG-CPD020
      **      a primeira parcela será considerado o valor total de salá-
      **       rio mais o saldo do conta corrente, as demais parcelas
      **       será considerada apenas o valor do salário
      *       IF I = 1 COMPUTE VALOR-TOT-CP20 = TOT-CREDITO-TOT +
      *                          CTACORR-LIQUIDO
      *       ELSE MOVE TOT-CREDITO-TOT TO VALOR-TOT-CP20
      *       END-IF
      *       MOVE "SALARIO"           TO NR-DOCTO-CP20
      *       MOVE DATA-INV            TO DATA-MOVTO-CP20
      *       MOVE 1                   TO PORTADOR-CP20
      *       MOVE DATA-INV            TO DATA-EMISSAO-CP20
      *       MOVE DATA-I              TO DATA-VENCTO-CP20
      *       MOVE 1                   TO PREV-DEF-CP20
      *       MOVE 0                   TO SITUACAO-CP20
      *       MOVE 0                   TO LIBERADO-CP20
      *       MOVE 0                   TO TIPO-MOEDA-CP20
      *       MOVE 0101                TO NR-PARCELA-CP20
      *       MOVE ZEROS   TO JUROS-MORA-CP20
      *          MULTA-ATRASO-CP20 TAXA-APLIC-CP20
      *          JURO-PAGO-CP20 MULTA-PAGA-CP20 DESCONTO-CP20
      *          DATA-PGTO-CP20 VALOR-LIQ-CP20 CONTABILIZADO-CP20
      *       MOVE USUARIO-W           TO RESPONSAVEL-CP20
      *       MOVE USUARIO-W           TO DIGITADOR-CP20
      *       MOVE ZEROS               TO SEQ-CAIXA-CP20
      *       MOVE 1                   TO TIPO-CONTA-CP20
      *       EVALUATE TIPO-LCTO-W
      *         WHEN 1 MOVE "PGTO-FUNCIONARIO    "  TO DESCRICAO-CP20
      *                MOVE 129   TO CODREDUZ-APUR-CP20
      *                MOVE 1     TO TIPO-FORN-CP20
      *         WHEN 2 MOVE "PGTO-VENDEDOR    " TO DESCRICAO-CP20
      *                MOVE 45    TO CODREDUZ-APUR-CP20
      *                MOVE 1     TO TIPO-FORN-CP20
      *         WHEN 3 MOVE "PGTO-REPRESENTANTE     " TO DESCRICAO-CP20
      *                MOVE 31    TO CODREDUZ-APUR-CP20
      *                MOVE 1     TO TIPO-FORN-CP20
      *         WHEN 4 MOVE "PGTO-REPORTAGEM    "   TO DESCRICAO-CP20
      *                MOVE 79    TO CODREDUZ-APUR-CP20
      *                MOVE 13    TO TIPO-FORN-CP20
      *       END-EVALUATE
      ** TIPO-FORN-CP20 = 1(FUNCIONARIOS)  13(PGTO DE FOTOGRAFO)
      *       MOVE CODIGO-W            TO FORNEC-CP20 FORNEC-CP21
      *       PERFORM PROCURA-SEQ-CPD021
      *       MOVE SEQ-CP21            TO SEQ-CP20
      *       MOVE ZEROS TO ST-CPD020
      *       PERFORM UNTIL ST-CPD020 = "10"
      *         WRITE REG-CPD020 INVALID KEY
      *               PERFORM PROCURA-SEQ-CPD021
      *               MOVE SEQ-CP21  TO SEQ-CP20
      *             NOT INVALID KEY
      *               MOVE "10" TO ST-CPD020
      *         END-WRITE
      *       END-PERFORM
      *       ADD 1 TO MES-I
      *       IF MES-I > 12  MOVE 1 TO MES-I   ADD 1 TO ANO-I
      *       END-IF
      *    END-PERFORM.
      *    CLOSE CPD020 CPD021.
      *PROCURA-SEQ-CPD021 SECTION.
      *    READ CPD021 INVALID KEY
      *            MOVE 1 TO SEQ-CP21
      *            WRITE REG-CPD021
      *            END-WRITE
      *        NOT INVALID KEY
      *            ADD 1 TO SEQ-CP21
      *            REWRITE REG-CPD021
      *            END-REWRITE
      *    END-READ.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *ACHA-SEQ-CIE SECTION.
      *    MOVE DATA-DIA-INV    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10.
      *
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
      *
      *                        TO COD-FUNCAO-CI10
      ** Função que exerce o destinatario

      **    VERIFICAR CADASTRO EM SANTA FÉ
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
       ATUALIZA-LIBERACAO-CCD105 SECTION.
      *    PERFORM GRAVA-CIE.
           CLOSE CCD105.  OPEN I-O CCD105.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                  MOVE CODIGO-WK  TO CODIGO-CC105
                  MOVE GS-MESANO-BASE TO MESANO-W
                  MOVE ANO-WW TO ANO-II
                  MOVE MES-WW TO MES-II
                  MOVE MESANO-I       TO MESANO-BASE-CC105
                  READ CCD105 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE VALOR-APAGAR-WK TO VALOR-LIBERADO-CC105
                     REWRITE REG-CCD105
                     END-REWRITE
                  END-READ
             END-READ
           END-PERFORM.
       ATUALIZA-LIBERACAO-CCD115 SECTION.
      *    PERFORM GRAVA-CIE.
           CLOSE CCD115.  OPEN I-O CCD115.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                  MOVE CODIGO-WK  TO CODIGO-CC115
                  MOVE GS-MESANO-BASE TO MESANO-W
                  MOVE ANO-WW TO ANO-II
                  MOVE MES-WW TO MES-II
                  MOVE MESANO-I       TO MESANO-BASE-CC115
                  READ CCD115 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE VALOR-APAGAR-WK TO VALOR-LIBERADO-CC115
                     REWRITE REG-CCD115
                     END-REWRITE
                  END-READ
             END-READ
           END-PERFORM.
       ATUALIZA-LIBERACAO-CCD120 SECTION.
      *    PERFORM GRAVA-CIE.
           CLOSE CCD120.  OPEN I-O CCD120.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                  MOVE CODIGO-WK  TO CODIGO-CC120
                  MOVE GS-MESANO-BASE TO MESANO-W
                  MOVE ANO-WW TO ANO-II
                  MOVE MES-WW TO MES-II
                  MOVE MESANO-I       TO MESANO-BASE-CC120
                  READ CCD120 INVALID KEY CONTINUE
                    NOT INVALID KEY
                     MOVE VALOR-APAGAR-WK TO VALOR-LIBERADO-CC120
                     REWRITE REG-CCD120
                     END-REWRITE
                  END-READ
             END-READ
           END-PERFORM.
       ATUALIZA-LIBERACAO-CCD110 SECTION.
      *    PERFORM GRAVA-CIE.
           CLOSE CCD110.  OPEN I-O CCD110.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                  MOVE "10" TO ST-WORK
             NOT AT END
                  MOVE CODIGO-WK  TO CODIGO-CC110
                  MOVE GS-MESANO-BASE TO MESANO-W
                  MOVE ANO-WW TO ANO-II
                  MOVE MES-WW TO MES-II
                  MOVE MESANO-I           TO MESANO-BASE-CC110
                  READ CCD110 INVALID KEY
                       CONTINUE
                  NOT INVALID KEY
                     MOVE VALOR-APAGAR-WK TO VALOR-LIBERADO-CC110
                     REWRITE REG-CCD110
                     END-REWRITE
                  END-READ
             END-READ
           END-PERFORM.

       LIBERAR-PGTO SECTION.
           MOVE ZEROS TO CODIGO-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                MOVE ZEROS     TO VALOR-APAGAR-WK
                EVALUATE GS-ORDEM-LIBERAR
                  WHEN 1 IF SALARIO1-WK > ZEROS
                            MOVE SALARIO1-WK TO VALOR-APAGAR-WK
                         END-IF
                  WHEN 2 IF SALARIO2-WK > ZEROS
                            MOVE SALARIO2-WK TO VALOR-APAGAR-WK
                         END-IF
                  WHEN 3 IF TOT-CREDITO-WK > ZEROS
                            MOVE TOT-CREDITO-WK TO VALOR-APAGAR-WK
                         END-IF
                  WHEN 4 IF SALDO-CTACORR-WK > ZEROS
                            MOVE SALDO-CTACORR-WK TO VALOR-APAGAR-WK
                         END-IF
                END-EVALUATE
                REWRITE REG-WORK
             END-READ
           END-PERFORM.
       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(114: 6) TO CODIGO-WK.
           MOVE GS-LINDET(121: 5) TO SEQ-WK.
           READ WORK INVALID KEY CONTINUE
            NOT INVALID KEY
             IF VALOR-APAGAR-WK = ZEROS
                MOVE "SIM"   TO GS-LINDET(81: 04)
                MOVE SALDO-CTACORR-WK TO GS-VLR-APAGAR VALOR-E1
                                         VALOR-APAGAR-WK
                MOVE VALOR-E1 TO GS-LINDET(85: 13)
                ADD SALDO-CTACORR-WK TO VALOR-APAGAR-TOT
                MOVE VALOR-APAGAR-TOT TO VALOR-E1
                MOVE VALOR-E1 TO GS-LINTOT(85: 13)
                REWRITE REG-WORK
             ELSE
                MOVE "NÃO"        TO GS-LINDET(81: 04)
                SUBTRACT VALOR-APAGAR-WK FROM VALOR-APAGAR-TOT
                MOVE ZEROS            TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(85: 13)
                MOVE VALOR-APAGAR-TOT TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINTOT(85: 13)
                MOVE ZEROS            TO VALOR-APAGAR-WK
                REWRITE REG-WORK.
       ALTERA-VLR-APAGAR SECTION.
           MOVE GS-LINDET(114: 6) TO CODIGO-WK.
           MOVE GS-LINDET(121: 5) TO SEQ-WK.
           READ WORK INVALID KEY CONTINUE
            NOT INVALID KEY
              MOVE VALOR-APAGAR-WK TO GS-VLR-APAGAR GS-VLR-APAGAR-ANT
              MOVE  NOME-WK         TO GS-NOME-APAGAR
           END-READ.
       ALTERA-VLR-APAGAR1 SECTION.
           MOVE "SIM"            TO GS-LINDET(81: 4)
           MOVE GS-VLR-APAGAR    TO VALOR-APAGAR-WK VALOR-E1
           MOVE VALOR-E1         TO GS-LINDET(85: 13)
           ADD GS-VLR-APAGAR     TO VALOR-APAGAR-TOT
           SUBTRACT GS-VLR-APAGAR-ANT FROM VALOR-APAGAR-TOT
           MOVE VALOR-APAGAR-TOT TO VALOR-E1
           MOVE VALOR-E1 TO GS-LINTOT(85: 13)
           REWRITE REG-WORK.
       GRAVA-WORK SECTION.
           CLOSE WORK. OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
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
                   COMPUTE TOT-CREDITO-WK = SALARIO1-CC115 +
                                            SALARIO2-CC115
                   MOVE VALOR-LIBERADO-CC115 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
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
                                               TOT-CREDITO-WK
                   MOVE ZEROS               TO SALARIO1-WK
                   MOVE VALOR-LIBERADO-CC120 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
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
                                               TOT-CREDITO-WK
                   MOVE ZEROS               TO SALARIO1-WK
                   MOVE VALOR-LIBERADO-CC110 TO VALOR-APAGAR-WK
                   MOVE ZEROS               TO SALDO-CTACORR-WK
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
                MOVE NOME-WK          TO GS-LINDET(1: 31)
                MOVE SALARIO1-WK      TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(32: 12)
                ADD SALARIO1-WK       TO SALARIO1-TOT
                MOVE SALARIO2-WK      TO VALOR-E
                MOVE VALOR-E          TO GS-LINDET(44: 12)
                ADD SALARIO2-WK       TO SALARIO2-TOT
                MOVE TOT-CREDITO-WK   TO VALOR-E
                ADD TOT-CREDITO-WK    TO TOT-CREDITO-TOT
                MOVE VALOR-E          TO GS-LINDET(56: 12)
                ADD SALDO-CTACORR-WK  TO SALDO-CTACORR-TOT
                ADD SALDO-CTACORR-WK  TO CTACORR-LIQUIDO
                MOVE SALDO-CTACORR-WK TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(68: 13)
                IF VALOR-APAGAR-WK = ZEROS
                           MOVE "NÃO" TO GS-LINDET(81: 4)
                ELSE MOVE "SIM"       TO GS-LINDET(81: 4)
                END-IF
                MOVE VALOR-APAGAR-WK  TO VALOR-E1
                MOVE VALOR-E1         TO GS-LINDET(85: 13)
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
           MOVE "CCP204" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                  MOVE NOME-WK          TO LINDET-REL(1: 31)
                  MOVE SALARIO1-WK      TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(32: 12)
                  MOVE SALARIO2-WK      TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(44: 12)
                  MOVE TOT-CREDITO-WK   TO VALOR-E
                  MOVE VALOR-E          TO LINDET-REL(56: 12)
                  MOVE SALDO-CTACORR-WK TO VALOR-E1
                  MOVE VALOR-E1         TO LINDET-REL(68: 13)
                  IF VALOR-APAGAR-WK = ZEROS
                             MOVE "NÃO" TO GS-LINDET(81: 4)
                  ELSE MOVE "SIM"       TO LINDET-REL(81: 4)
                  END-IF
                  MOVE VALOR-APAGAR-WK  TO VALOR-E1
                  MOVE VALOR-E1         TO LINDET-REL(85: 12)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.

           copy descondensa.
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
           MOVE SALDO-APAGAR-TOT  TO VALOR-E1
           MOVE VALOR-E           TO LINDET-REL(85: 12)
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
           CLOSE CCD010 CCD100 CCD105 CCD115 CCD110 CCD120
                 CCD001 WORK.
      *    CLOSE CIED010.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
