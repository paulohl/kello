       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP105.
      *DATA: 17/12/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: LANÇAMENTOS DE SALÁRIOS - CONTAS CORRENTES
      *FUNÇÃO: Listar todos os nomes (que estiverem na função
      *        selecionada) buscando do cadastro geral, para que seja
      *        lançado o salário referênte ao mês.
      *        Após ler os lançamentos no CCD105.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX105.
           COPY CCPX100.
           COPY CCPX101.
           COPY CXPX020.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CODIGO-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-WK = TIPO-WK CODIGO-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW105.
       COPY CCPW100.
       COPY CCPW101.
       COPY CXPW020.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  NOME-WK             PIC X(30).
           05  TIPO-WK             PIC 9(2).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP105.CPB".
           COPY "CCP105.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CCD105             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANO-DUP            PIC 9(6)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  TOTAL-GERAL           PIC 9(09)V99 VALUE ZEROS.
           05  TOTAL-SALARIO1        PIC 9(09)V99 VALUE ZEROS.
           05  TOTAL-SALARIO2        PIC 9(09)V99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(57)   VALUE
           "RELACAO DE LANCAMENTOS DE SALARIOS ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(06)   VALUE "TIPO: ".
           05  TIPO-REL            PIC X(19)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "FUNCIONARIO                     DESCRICAO
      -    "      SALARIO-1    SALARIO-2        SALDO  MES/ANO".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
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
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CCD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD105.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN INPUT CGD001 CXD020.
           OPEN I-O CCD105.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-CCD105 = "35"
              CLOSE CCD105    OPEN OUTPUT CCD105  CLOSE CCD105
              OPEN I-O CCD105.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CCD100.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CCD100.  OPEN INPUT CCD101.
           IF ST-CCD101 <> "00"
              MOVE "ERRO ABERTURA CCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CCD101.
           IF ST-CCD105 <> "00"
              MOVE "ERRO ABERTURA CCD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD105 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM ENTRA-DADOS
               WHEN GS-DUPLICA-LANCAMENTOS-TRUE
                    PERFORM DUPLICA-LANCAMENTOS
               WHEN GS-GRAVA-DADOS-TRUE
                    EVALUATE GRAVA1-REGRAVA2
                      WHEN 1 PERFORM GRAVA-DADOS
                      WHEN 2 PERFORM REGRAVA-DADOS
                    END-EVALUATE
                    PERFORM CARREGA-LISTA
                    MOVE "POSICIONA-LINHA"  TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
      *             MOVE "SELEC-PROX-LISTA" TO DS-PROCEDURE
      *             PERFORM CALL-DIALOG-SYSTEM
               WHEN GS-LIMPAR-ENTRADA-TRUE
                    PERFORM LIMPAR-ENTRADA
               WHEN GS-ATUALIZA-CTACORR-TRUE
                    PERFORM ATUALIZA-CTACORRENTE
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
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CODIGO-CG01.
           START CGD001 KEY IS NOT < CODIGO-CG01 INVALID KEY
                  MOVE "10" TO ST-CGD001.
           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD001
              NOT AT END
                  IF SITUACAO-CG01 <> 2
                     PERFORM VERIFICA-TIPO-LCTO
                     IF IMPRIME-W = 0
                        CONTINUE
                     ELSE
                        MOVE CODIGO-CG01     TO GS-EXIBE-CODIGO
                        MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                        MOVE CODIGO-CG01     TO CODIGO-WK
                        MOVE NOME-CG01       TO NOME-WK
                        WRITE REG-WORK
                        END-WRITE
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       DUPLICA-LANCAMENTOS SECTION.
           MOVE GS-MES-DUPLICA(1: 2)  TO MESANO-DUP(5: 2)
           MOVE GS-MES-DUPLICA(3: 4)  TO MESANO-DUP(1: 4)
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW  TO ANO-WI
           MOVE MES-WW  TO MES-WI
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
               MOVE CODIGO-WK         TO CODIGO-CC105
               MOVE MESANO-I          TO MESANO-BASE-CC105
               READ CCD105 INVALID KEY CONTINUE
                  NOT INVALID KEY
                     MOVE MESANO-DUP        TO MESANO-BASE-CC105
                     MOVE ZEROS             TO VALOR-LIBERADO-CC105
                                               ATUALIZADO-CC-CC105
                     MOVE USUARIO-W         TO DIGITADOR-CC105
                     MOVE DATA-DIA-I        TO DATA-MOVTO-CC105
                     WRITE REG-CCD105
                     END-WRITE
                END-READ
              END-READ
           END-PERFORM.

       VERIFICA-TIPO-LCTO SECTION.
           MOVE ZEROS TO IMPRIME-W.
      *    EVALUATE CARACTERISTICA-W
      *      WHEN 1 IF T-FUNC-CG01 = 1   MOVE 1 TO IMPRIME-W
      *      WHEN 2 IF T-VEND-CG01 = 1   MOVE 1 TO IMPRIME-W
      *      WHEN 3 IF T-REPRES-CG01 = 1 MOVE 1 TO IMPRIME-W
      *      WHEN 4 IF T-FOTOG-CG01 = 1 OR T-CINEG-CG01 = 1
      *                                  MOVE 1 TO IMPRIME-W
      *    END-EVALUATE.
           IF T-FUNC-CG01 = 1 MOVE 1 TO IMPRIME-W.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW  TO ANO-WI
           MOVE MES-WW  TO MES-WI
           MOVE SPACES TO NOME-WK
           move zeros  to total-geral
                          total-salario1
                          total-salario2
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
               MOVE SPACES            TO GS-LINDET
               MOVE NOME-WK           TO GS-LINDET(1: 31)
               MOVE CODIGO-WK         TO CODIGO-CC105 GS-LINDET(115: 6)
               MOVE MESANO-I          TO MESANO-BASE-CC105
               READ CCD105 INVALID KEY CONTINUE
                  NOT INVALID KEY
                     MOVE SALARIO1-CC105    TO VALOR-E TOTAL-W
                     MOVE VALOR-E           TO GS-LINDET(33: 13)
                     MOVE SALARIO2-CC105    TO VALOR-E
                     MOVE VALOR-E           TO GS-LINDET(46: 13)
                     ADD SALARIO2-CC105     TO TOTAL-W
                     MOVE TOTAL-W           TO VALOR-E
                     MOVE VALOR-E           TO GS-LINDET(59: 13)
                     ADD SALARIO1-CC105     TO TOTAL-SALARIO1
                     ADD SALARIO2-CC105     TO TOTAL-SALARIO2
                END-READ
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           ADD TOTAL-SALARIO1               TO TOTAL-GERAL
           ADD TOTAL-SALARIO2               TO TOTAL-GERAL

           MOVE SPACES                       TO GS-LINHA-TOTAL
           MOVE "TOTAL GERAL . . ."          TO GS-LINHA-TOTAL
           MOVE TOTAL-SALARIO1               TO VALOR-E
           MOVE VALOR-E                      TO GS-LINHA-TOTAL(33:13)
           MOVE TOTAL-SALARIO2               TO VALOR-E
           MOVE VALOR-E                      TO GS-LINHA-TOTAL(46:13)
           MOVE TOTAL-GERAL                  TO VALOR-E
           MOVE VALOR-E                      TO GS-LINHA-TOTAL(59:13)

           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ENTRA-DADOS SECTION.
           MOVE GS-LINDET(1: 30)   TO GS-NOME
           MOVE GS-LINDET(115: 6)  TO CODIGO-CC105.
           MOVE MESANO-I           TO MESANO-BASE-CC105
           READ CCD105 INVALID KEY
                       MOVE 1 TO GRAVA1-REGRAVA2
                       MOVE ZEROS TO GS-SALARIO1 GS-SALARIO2
             NOT INVALID KEY
                 MOVE SALARIO1-CC105   TO GS-SALARIO1
                 MOVE SALARIO2-CC105   TO GS-SALARIO2
                 MOVE 2                TO GRAVA1-REGRAVA2
           END-READ
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-DADOS SECTION.
           MOVE GS-SALARIO1       TO SALARIO1-CC105 VALOR-E TOTAL-W
           MOVE VALOR-E           TO GS-LINDET(33: 13)
           MOVE GS-SALARIO2       TO SALARIO2-CC105 VALOR-E
           MOVE VALOR-E           TO GS-LINDET(46: 13)
           ADD GS-SALARIO2        TO TOTAL-W
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(59: 13)
           MOVE MESANO-I          TO MESANO-BASE-CC105
           MOVE ZEROS             TO ATUALIZADO-CC-CC105
           MOVE USUARIO-W         TO DIGITADOR-CC105
           MOVE DATA-DIA-I        TO DATA-MOVTO-CC105
           MOVE ZEROS             TO VALOR-LIBERADO-CC105
           WRITE REG-CCD105 INVALID KEY
                        MOVE "CCD105"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD105" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO.
           PERFORM LIMPAR-ENTRADA.
       ATUALIZA-CTACORRENTE SECTION.
           OPEN I-O CCD100 CCD101.

           MOVE GS-MESANO-BASE(1: 2) TO MESANO-I(5: 2)
           MOVE GS-MESANO-BASE(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I TO MESANO-BASE-CC105
           MOVE ZEROS                TO CODIGO-CC105.
           START CCD105 KEY IS NOT < CHAVE-CC105 INVALID KEY
                 MOVE "10" TO ST-CCD105.
           PERFORM UNTIL ST-CCD105 = "10"
             READ CCD105 NEXT RECORD AT END MOVE "10" TO ST-CCD105
               NOT AT END
                 IF MESANO-BASE-CC105 NOT = MESANO-I
                        MOVE "10" TO ST-CCD105
                 ELSE
                 INITIALIZE REG-CCD100
                 COMPUTE VALOR-CC100 = SALARIO1-CC105 + SALARIO2-CC105
                 MOVE 0                  TO CRED-DEB-CC100
                 MOVE DATA-DIA-I         TO DATA-MOVTO-CC100
                                            DATA-EMISSAO-CC100
                 MOVE 1                  TO TIPO-LCTO-CC100
                 MOVE ZEROS              TO TIPO-FORN-CC100
                 MOVE SPACES             TO NR-DOCTO-CC100
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
                 MOVE CODIGO-CC105 TO FORNEC-CC100 FORNEC-CC101
                 PERFORM ATUALIZA-SEQ-CCD101
                 MOVE SEQ-CC101          TO SEQ-CC100
                 WRITE REG-CCD100 INVALID KEY
                       MOVE "CCD100" TO GS-MENSAGEM-ERRO(15: 7)
                       MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 2)
                       PERFORM ERRO-GRAVACAO
                 END-WRITE
             END-READ
           END-PERFORM.
           CLOSE CCD100 CCD101.
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

       LIMPAR-ENTRADA SECTION.
           MOVE "LIMPAR-ENTR-DADOS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           MOVE GS-SALARIO1       TO SALARIO1-CC105 VALOR-E TOTAL-W
           MOVE VALOR-E           TO GS-LINDET(33: 14)
           MOVE GS-SALARIO2       TO SALARIO2-CC105 VALOR-E
           MOVE VALOR-E           TO GS-LINDET(47: 14)
           ADD GS-SALARIO2        TO TOTAL-W
           MOVE TOTAL-W           TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(61: 14)
           MOVE GS-MESANO-BASE TO MESANO-W
           MOVE MES-WW TO MES-WI
           MOVE ANO-WW TO ANO-WI
           MOVE MESANO-I          TO MESANO-BASE-CC105
           MOVE USUARIO-W         TO DIGITADOR-CC105
           REWRITE REG-CCD105 INVALID KEY
                        MOVE "CCD105"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD105" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO.
           PERFORM LIMPAR-ENTRADA.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           INITIALIZE TOTAL-GERAL
                      TOTAL-SALARIO1
                      TOTAL-SALARIO2
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                MOVE SPACES            TO LINDET-REL
                MOVE NOME-WK           TO LINDET-REL(1: 31)
                MOVE CODIGO-WK         TO CODIGO-CC105
                MOVE MESANO-I          TO MESANO-BASE-CC105
                READ CCD105 INVALID KEY CONTINUE
                  NOT INVALID KEY
                     MOVE SALARIO1-CC105    TO VALOR-E TOTAL-W
                     MOVE VALOR-E           TO LINDET-REL(33: 13)
                     MOVE SALARIO2-CC105    TO VALOR-E
                     ADD SALARIO2-CC105     TO TOTAL-W
                     MOVE VALOR-E           TO LINDET-REL(46: 13)
                     MOVE TOTAL-W           TO VALOR-E
                     MOVE VALOR-E           TO LINDET-REL(59: 13)
                     ADD SALARIO1-CC105     TO TOTAL-SALARIO1
                     ADD SALARIO2-CC105     TO TOTAL-SALARIO2
                     WRITE REG-RELAT FROM LINDET
                     END-WRITE
                     ADD 1 TO LIN
                     IF LIN > 56 PERFORM CABECALHO
                     END-IF
                END-READ
              END-READ
           END-PERFORM
           ADD TOTAL-SALARIO1      TO TOTAL-GERAL
           ADD TOTAL-SALARIO2      TO TOTAL-GERAL.

           copy descondensa.


       CABECALHO SECTION.
           MOVE GS-VENCTO TO VENCTO-REL.
           MOVE "1-FUNCIONARIO" TO TIPO-REL.
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
           CLOSE CCD105 CGD001 CXD020.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
