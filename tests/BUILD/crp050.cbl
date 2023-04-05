       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP050.
      *DATA: 10/04/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Quadro geral de contas a receber/Resumo por portador e
      *        carteira
      *        Função de listar todos os títulos a receber,
      *        por intervalo de 30 dias.
      *        Será considerada apenas situacao = 0(a pagar)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORK ASSIGN TO VARIA-W
                ACCESS MODE IS DYNAMIC
                ORGANIZATION IS INDEXED
                STATUS IS ST-WORK
                RECORD KEY IS PORTADOR-WK.
           SELECT WORK1 ASSIGN TO VARIA-W1
                ACCESS MODE IS DYNAMIC
                ORGANIZATION IS INDEXED
                STATUS IS ST-WORK1
                RECORD KEY IS CARTEIRA-WK1.
           COPY CAPX018.
           COPY CRPX020.
           COPY CGPX020.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW018.
       COPY CRPW020.
       COPY CGPW020.
       COPY LOGACESS.FD.
       FD  WORK.
       01  REG-WORK.
           05  VLR-TOT-WK      PIC 9(8)V99.
           05  VLR-ATRASADO-WK PIC 9(8)V99.
           05  VLR-1-30-WK     PIC 9(8)V99.
           05  VLR-31-60-WK    PIC 9(8)V99.
           05  VLR-61-90-WK    PIC 9(8)V99.
           05  VLR-91-120-WK   PIC 9(8)V99.
           05  VLR-121-MAIS-WK PIC 9(8)V99.
           05  PORTADOR-WK     PIC 9(4).
       FD  WORK1.
       01  REG-WORK1.
           05  VLR-TOT-WK1      PIC 9(8)V99.
           05  VLR-ATRASADO-WK1 PIC 9(8)V99.
           05  VLR-1-30-WK1     PIC 9(8)V99.
           05  VLR-31-60-WK1    PIC 9(8)V99.
           05  VLR-61-90-WK1    PIC 9(8)V99.
           05  VLR-91-120-WK1   PIC 9(8)V99.
           05  VLR-121-MAIS-WK1 PIC 9(8)V99.
           05  CARTEIRA-WK1     PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP050.CPB".
           COPY "CRP050.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  DATA-DIA-W            PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA              PIC 9(8)     VALUE ZEROS.
           05  DATA-30               PIC 9(8)     VALUE ZEROS.
           05  DATA-60               PIC 9(8)     VALUE ZEROS.
           05  DATA-90               PIC 9(8)     VALUE ZEROS.
           05  DATA-120              PIC 9(8)     VALUE ZEROS.
           05  TOT-TOTAL             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-ATRASADO          PIC 9(8)V99  VALUE ZEROS.
           05  TOT-1-30              PIC 9(8)V99  VALUE ZEROS.
           05  TOT-31-60             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-61-90             PIC 9(8)V99  VALUE ZEROS.
           05  TOT-91-120            PIC 9(8)V99  VALUE ZEROS.
           05  TOT-121-MAIS          PIC 9(8)V99  VALUE ZEROS.
           05  PERC                  PIC 999V99   VALUE ZEROS.
           05  PERC-E                PIC ZZZ,ZZ.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(112)  VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(115)   VALUE
           "QUADRO GERAL-CONTAS A RECEBER/RESUMO POR PORTADOR/CARTEIRA".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)   VALUE
           "PORTADOR                            TOTAL   PERC      ATRASA
      -    "DO        1 a 30       31 a 60       61 a 90      91 a 120
      -    "    120 a +".
       01  CAB04A.
           05  FILLER              PIC X(132)   VALUE
           "CARTEIRA                            TOTAL   PERC      ATRASA
      -    "DO        1 a 30       31 a 60       61 a 90      91 a 120
      -    "    120 a +".
       01  LINDET.
           05  LINDET-REL          PIC X(132)   VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

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
           MOVE DATA-INV TO DATA-DIA-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD018"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CAD018 CRD020 CGD020.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK. CLOSE WORK.  OPEN I-O WORK.
           ACCEPT VARIA-W1 FROM TIME.
           ADD 5 TO VARIA-W1
           OPEN OUTPUT WORK1. CLOSE WORK1.  OPEN I-O WORK1.

           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP050"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

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
               WHEN GS-CARREGA-LISTA-TRUE
                    PERFORM VERIFICA-DATAS
                    PERFORM ZERA-VARIAVEIS
                    PERFORM INICIO-PORTADOR
                    PERFORM INICIO-CARTEIRA
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-CARTAO SECTION.
           MOVE GS-ACP-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
                MOVE SPACES TO NOME-CG20.
           MOVE NOME-CG20 TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-CARTAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INICIO-PORTADOR SECTION.
           MOVE "SHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE WORK.  OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS    TO SITUACAO-CR20 DATA-VENCTO-CR20
                            CLASS-CLIENTE-CR20 CLIENTE-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 > 1
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF GS-TIPO(1:1) = TIPO-DOCTO-CR20 OR "9"
                            IF GS-ACP-CARTAO = 0 OR CARTAO-CRED-CR20
                               MOVE DATA-VENCTO-CR20 TO GS-TEXTO
                               MOVE "REFRESH-AGUARDA" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM
                               MOVE PORTADOR-CR20  TO PORTADOR-WK
                               READ WORK INVALID KEY
                                    PERFORM GRAVA-WORK
                               NOT INVALID KEY
                                    PERFORM REGRAVA-WORK
                               END-READ
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
       GRAVA-WORK SECTION.
           INITIALIZE REG-WORK.
           MOVE PORTADOR-CR20    TO PORTADOR-WK
           IF DATA-VENCTO-CR20 = 19990110 CONTINUE.
           EVALUATE DATA-VENCTO-CR20
              WHEN < DATA-DIA
                   MOVE VALOR-SALDO-CR20 TO VLR-ATRASADO-WK
                   ADD VALOR-SALDO-CR20  TO TOT-ATRASADO
              WHEN < DATA-30
                   MOVE VALOR-SALDO-CR20 TO VLR-1-30-WK
                   ADD VALOR-SALDO-CR20  TO TOT-1-30
              WHEN < DATA-60
                   MOVE VALOR-SALDO-CR20 TO VLR-31-60-WK
                   ADD VALOR-SALDO-CR20  TO TOT-31-60
              WHEN < DATA-90
                   MOVE VALOR-SALDO-CR20 TO VLR-61-90-WK
                   ADD VALOR-SALDO-CR20  TO TOT-61-90
              WHEN < DATA-120
                   MOVE VALOR-SALDO-CR20 TO VLR-91-120-WK
                   ADD VALOR-SALDO-CR20  TO TOT-91-120
              WHEN OTHER
                   MOVE VALOR-SALDO-CR20 TO VLR-121-MAIS-WK
                   ADD VALOR-SALDO-CR20  TO TOT-121-MAIS
           END-EVALUATE.

           MOVE VALOR-SALDO-CR20    TO VLR-TOT-WK.
           ADD VALOR-SALDO-CR20     TO TOT-TOTAL.
           WRITE REG-WORK.
       REGRAVA-WORK SECTION.
           EVALUATE DATA-VENCTO-CR20
              WHEN < DATA-DIA
                   ADD VALOR-SALDO-CR20 TO VLR-ATRASADO-WK TOT-ATRASADO
              WHEN < DATA-30
                   ADD VALOR-SALDO-CR20 TO VLR-1-30-WK TOT-1-30
              WHEN < DATA-60
                   ADD VALOR-SALDO-CR20 TO VLR-31-60-WK TOT-31-60
              WHEN < DATA-90
                   ADD VALOR-SALDO-CR20 TO VLR-61-90-WK TOT-61-90
              WHEN < DATA-120
                   ADD VALOR-SALDO-CR20 TO VLR-91-120-WK TOT-91-120
              WHEN OTHER
                   ADD VALOR-SALDO-CR20 TO VLR-121-MAIS-WK TOT-121-MAIS
           END-EVALUATE.
           ADD VALOR-SALDO-CR20 TO VLR-TOT-WK TOT-TOTAL.
           REWRITE REG-WORK.

       INICIO-CARTEIRA SECTION.
           CLOSE WORK1. OPEN OUTPUT WORK1.  CLOSE WORK1. OPEN I-O WORK1.
           MOVE ZEROS    TO SITUACAO-CR20 DATA-VENCTO-CR20
                            CLASS-CLIENTE-CR20 CLIENTE-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT RECORD AT END
                    MOVE "10" TO ST-CRD020
               NOT AT END
                    IF SITUACAO-CR20 > 1
                       MOVE "10" TO ST-CRD020
                    ELSE
                       IF GS-TIPO(1:1) = TIPO-DOCTO-CR20 OR "9"
                          IF GS-ACP-CARTAO = 0 OR CARTAO-CRED-CR20
                             MOVE CARTEIRA-CR20  TO CARTEIRA-WK1
                             READ WORK1 INVALID KEY
                                  PERFORM GRAVA-WORK1
                             NOT INVALID KEY
                                  PERFORM REGRAVA-WORK1
                             END-READ
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.
           MOVE "UNSHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-WORK1 SECTION.
           INITIALIZE REG-WORK1.
           MOVE DATA-VENCTO-CR20 TO GS-TEXTO.
           MOVE CARTEIRA-CR20 TO CARTEIRA-WK1.
           MOVE "REFRESH-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           EVALUATE DATA-VENCTO-CR20
              WHEN < DATA-DIA
                   MOVE VALOR-SALDO-CR20 TO VLR-ATRASADO-WK1
              WHEN < DATA-30
                   MOVE VALOR-SALDO-CR20 TO VLR-1-30-WK1
              WHEN < DATA-60
                   MOVE VALOR-SALDO-CR20 TO VLR-31-60-WK1
              WHEN < DATA-90
                   MOVE VALOR-SALDO-CR20 TO VLR-61-90-WK1
              WHEN < DATA-120
                   MOVE VALOR-SALDO-CR20 TO VLR-91-120-WK1
              WHEN OTHER
                   MOVE VALOR-SALDO-CR20 TO VLR-121-MAIS-WK1
           END-EVALUATE.

           MOVE VALOR-SALDO-CR20    TO VLR-TOT-WK1.
           WRITE REG-WORK1.

       REGRAVA-WORK1 SECTION.
           EVALUATE DATA-VENCTO-CR20
              WHEN < DATA-DIA
                   ADD VALOR-SALDO-CR20 TO VLR-ATRASADO-WK1
              WHEN < DATA-30
                   ADD VALOR-SALDO-CR20 TO VLR-1-30-WK1
              WHEN < DATA-60
                   ADD VALOR-SALDO-CR20 TO VLR-31-60-WK1
              WHEN < DATA-90
                   ADD VALOR-SALDO-CR20 TO VLR-61-90-WK1
              WHEN < DATA-120
                   ADD VALOR-SALDO-CR20 TO VLR-91-120-WK1
              WHEN OTHER
                   ADD VALOR-SALDO-CR20 TO VLR-121-MAIS-WK1
           END-EVALUATE.
           ADD VALOR-SALDO-CR20 TO VLR-TOT-WK1.
           REWRITE REG-WORK1.
       VERIFICA-DATAS SECTION.
           MOVE DATA-DIA-W       TO GRADAY-DATA DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV         TO DATA-DIA.
           MOVE 30               TO GRADAY-DIAS.

           CALL "GRADAY1" USING PARAMETROS-GRADAY.
           MOVE GRADAY-DATA      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV         TO DATA-30.

           CALL "GRADAY1" USING PARAMETROS-GRADAY.
           MOVE GRADAY-DATA      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV         TO DATA-60.

           CALL "GRADAY1" USING PARAMETROS-GRADAY.
           MOVE GRADAY-DATA      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV         TO DATA-90.

           CALL "GRADAY1" USING PARAMETROS-GRADAY.
           MOVE GRADAY-DATA      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV         TO DATA-120.
       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOT-TOTAL TOT-ATRASADO TOT-1-30 TOT-31-60
                         TOT-61-90 TOT-91-120 TOT-121-MAIS.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO PORTADOR-WK.
           START WORK KEY IS NOT < PORTADOR-WK
                 INVALID KEY MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                 NOT AT END
                   MOVE PORTADOR-WK TO PORTADOR
                   READ CAD018 INVALID KEY MOVE "*****" TO NOME-PORT
                   END-READ
                   MOVE SPACES TO GS-LINDET(01:21)
                   STRING PORTADOR-WK "-" NOME-PORT
                   INTO GS-LINDET(01: 21)
                   DIVIDE VLR-TOT-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO GS-LINDET(22: 14)
                   COMPUTE PERC = ((VLR-TOT-WK/1000) / (TOT-TOTAL/1000))
                                   * 100
                   MOVE PERC            TO PERC-E
                   MOVE PERC-E          TO GS-LINDET(36: 06)
                   DIVIDE VLR-ATRASADO-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(42: 11)
                   DIVIDE VLR-1-30-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(53: 11)
                   DIVIDE VLR-31-60-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(64: 11)
                   DIVIDE VLR-61-90-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(75: 11)
                   DIVIDE VLR-91-120-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(86: 11)
                   DIVIDE VLR-121-MAIS-WK BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET(97: 11)
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
              MOVE "TOTAL"         TO GS-LINTOT(01: 21)
              DIVIDE TOT-TOTAL BY 1000 GIVING VALOR-E
              MOVE VALOR-E         TO GS-LINTOT(22: 14)
              MOVE 100             TO PERC-E
              MOVE PERC-E          TO GS-LINTOT(36: 06)
              DIVIDE TOT-ATRASADO BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(42: 11)
              DIVIDE TOT-1-30 BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(53: 11)
              DIVIDE TOT-31-60 BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(64: 11)
              DIVIDE TOT-61-90 BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(75: 11)
              DIVIDE TOT-91-120 BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(86: 11)
              DIVIDE TOT-121-MAIS BY 1000 GIVING VALOR-E1
              MOVE VALOR-E1        TO GS-LINTOT(97: 11)
              MOVE "INSERE-TOTAL" TO DS-PROCEDURE.
              PERFORM CALL-DIALOG-SYSTEM.
           MOVE "CLEAR-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CARTEIRA-WK1.
           START WORK1 KEY IS NOT < CARTEIRA-WK1
                 INVALID KEY MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END
                   MOVE "10" TO ST-WORK1
              NOT AT END
                   EVALUATE CARTEIRA-WK1
                     WHEN 1 MOVE "SIMPLES" TO GS-LINDET1(1: 21)
                     WHEN 2 MOVE "CAUÇÃO"  TO GS-LINDET1(1: 21)
                     WHEN 3 MOVE "DESCONTO" TO GS-LINDET1(1: 21)
                   END-EVALUATE
                   DIVIDE VLR-TOT-WK1 BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO GS-LINDET1(22: 14)
                   COMPUTE PERC = ((VLR-TOT-WK1/1000)
                    / (TOT-TOTAL/1000)) * 100
                   MOVE PERC            TO PERC-E
                   MOVE PERC-E          TO GS-LINDET1(36: 06)
                   DIVIDE VLR-ATRASADO-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(42: 11)
                   DIVIDE VLR-1-30-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(53: 11)
                   DIVIDE VLR-31-60-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(64: 11)
                   DIVIDE VLR-61-90-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(75: 11)
                   DIVIDE VLR-91-120-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(86: 11)
                   DIVIDE VLR-121-MAIS-WK1 BY 1000 GIVING VALOR-E1
                   MOVE VALOR-E1        TO GS-LINDET1(97: 11)

                   MOVE "INSERE-LIST2" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP050" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN PAG-W. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE ZEROS TO PORTADOR-WK.
           START WORK KEY IS NOT < PORTADOR-WK
                 INVALID KEY MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE PORTADOR-WK TO PORTADOR
                   READ CAD018 INVALID KEY MOVE "*****" TO NOME-PORT
                   END-READ
                   MOVE SPACES          TO LINDET-REL
                   STRING PORTADOR-WK "-" NOME-PORT INTO
                   LINDET-REL(01:27)
                   DIVIDE VLR-TOT-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(29: 14)
                   COMPUTE PERC = ((VLR-TOT-WK/1000) / (TOT-TOTAL/1000))
                                        * 100
                   MOVE PERC            TO PERC-E
                   MOVE PERC-E          TO LINDET-REL(43: 07)
                   DIVIDE VLR-ATRASADO-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(50: 14)
                   DIVIDE VLR-1-30-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(64: 14)
                   DIVIDE VLR-31-60-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(78: 14)
                   DIVIDE VLR-61-90-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(92: 14)
                   DIVIDE VLR-91-120-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(106: 14)
                   DIVIDE VLR-121-MAIS-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(120: 13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
              END-READ
           END-PERFORM.
           MOVE "TOTAL......"   TO LINDET-REL(01: 27)
           DIVIDE TOT-TOTAL BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(29: 14)
           MOVE 100             TO PERC-E
           MOVE PERC-E          TO LINDET-REL(43: 07)
           DIVIDE TOT-ATRASADO BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(50: 14)
           DIVIDE TOT-1-30 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(64: 14)
           DIVIDE TOT-31-60 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(78: 14)
           DIVIDE TOT-61-90 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(92: 14)
           DIVIDE TOT-91-120 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(106: 14)
           DIVIDE TOT-121-MAIS BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(120: 13).
           PERFORM IMPRIME-CARTEIRA.
       IMPRIME-CARTEIRA SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           MOVE ZEROS TO LIN PAG-W. PERFORM CABECALHO1.
           MOVE ZEROS TO CARTEIRA-WK1.
           START WORK1 KEY IS NOT < CARTEIRA-WK1
                 INVALID KEY MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   EVALUATE CARTEIRA-WK1
                     WHEN 1 MOVE "SIMPLES" TO LINDET-REL(1: 27)
                     WHEN 2 MOVE "CAUCAO"  TO LINDET-REL(1: 27)
                     WHEN 3 MOVE "DESCONTO" TO LINDET-REL(1: 27)
                   END-EVALUATE
                   MOVE SPACES          TO LINDET-REL
                   DIVIDE VLR-TOT-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(29: 14)
                   COMPUTE PERC = ((VLR-TOT-WK/1000) / (TOT-TOTAL/1000))
                                        * 100
                   MOVE PERC            TO PERC-E
                   MOVE PERC-E          TO LINDET-REL(43: 07)
                   DIVIDE VLR-ATRASADO-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(50: 14)
                   DIVIDE VLR-1-30-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(64: 14)
                   DIVIDE VLR-31-60-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(78: 14)
                   DIVIDE VLR-61-90-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(92: 14)
                   DIVIDE VLR-91-120-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(106: 14)
                   DIVIDE VLR-121-MAIS-WK BY 1000 GIVING VALOR-E
                   MOVE VALOR-E         TO LINDET-REL(120: 13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
              END-READ
           END-PERFORM.
           MOVE "TOTAL......"   TO LINDET-REL(01: 27)
           DIVIDE TOT-TOTAL BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(29: 14)
           MOVE 100             TO PERC-E
           MOVE PERC-E          TO LINDET-REL(43: 07)
           DIVIDE TOT-ATRASADO BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(50: 14)
           DIVIDE TOT-1-30 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(64: 14)
           DIVIDE TOT-31-60 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(78: 14)
           DIVIDE TOT-61-90 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(92: 14)
           DIVIDE TOT-91-120 BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(106: 14)
           DIVIDE TOT-121-MAIS BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO LINDET-REL(120: 13).

           COPY DESCONDENSA.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PAG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM GS-TIPO
           MOVE 7 TO LIN.
       CABECALHO1 SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PAG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04A.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP050"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CRD020 CAD018 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
