       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP050.
      *DATA: 21/07/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Quadro geral de contas a pagar/Resumo por tipo fornecedor
      *        Função de listar todos os títulos a pagar, por intervalo
      *        de 30 dias. Será considerada apenas situacao = 0(a pagar)
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
                RECORD KEY IS TIPO-FORN-WK.

           SELECT WORK2 ASSIGN TO VARIA-W2
                        ACCESS MODE IS DYNAMIC
                        ORGANIZATION IS INDEXED
                        RECORD KEY IS CONTA-WK2
                        ALTERNATE RECORD KEY IS CONTA-REDUZ-WK2
                        STATUS IS ST-WORK2.

           COPY CPPX020.
           COPY CGPX001.
           COPY CXPX020.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CPPW020.
       COPY CGPW001.
       COPY CXPW020.

       FD  WORK.
       01  REG-WORK.
           05  VLR-TOT-WK      PIC 9(8)V99.
           05  VLR-ATRASADO-WK PIC 9(8)V99.
           05  VLR-1-30-WK     PIC 9(8)V99.
           05  VLR-31-60-WK    PIC 9(8)V99.
           05  VLR-61-90-WK    PIC 9(8)V99.
           05  VLR-91-120-WK   PIC 9(8)V99.
           05  VLR-121-MAIS-WK PIC 9(8)V99.
           05  TIPO-FORN-WK    PIC X(30).

       FD  WORK2.
       01  REG-WORK2.
           05  CONTA-WK2       PIC 9(7).
           05  CONTA-REDUZ-WK2 PIC 9(5).
           05  VALOR-WK2       PIC S9(8)V99.
           05  GRAU-WK2        PIC 9.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY "CPP050.CPB".
           COPY "CPP050.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE "35".
           05  ST-WORK2              PIC XX       VALUE "35".
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
           05  lin                   pic 9(02).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR-E1              PIC ZZZ.ZZ9,99.
      *        BLANK WHEN ZEROS.
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
           05  CONTA-E               PIC 9.99.99.99.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  TOTAL-PERC            PIC 9(8)V99  VALUE ZEROS.
           05  PERC-W                PIC 999V99   VALUE ZEROS.
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
           "QUADRO GERAL DE CONTAS A PAGAR/RESUMO POR TIPO FORNECEDOR".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(132)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)   VALUE
           "TIPO FORNECEDOR              TOTAL   PERC   ATRASADO     1 a
      -    " 30    31 a 60    61 a 90   91 a 120    120 a +
      -    "".
       01  CAB05.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(105)  VALUE
           "DESCRICAO DAS CONTAS
      -    "                   VALOR-R$ PERC.%".

       01  LINDET.
           05  LINDET-REL          PIC X(132)   VALUE SPACES.

          COPY IMPRESSORA.CPY.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP050-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-DIA-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CPP050-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP050-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP050-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           OPEN INPUT CPD020 CGD001 CXD020.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP050-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP050-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CPP050-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CPP050-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CPP050-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CPP050-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CPP050-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CPP050-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CPP050-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN CPP050-CARREGA-LISTA-TRUE
                    PERFORM PREPARA-ARQ-WORK
                    PERFORM VERIFICA-DATAS
                    PERFORM ZERA-VARIAVEIS
                    PERFORM INICIO-CPD020
                    EVALUATE CPP050-FORMATO-RELATORIO
                       WHEN 1 PERFORM CARREGA-LISTA
                       WHEN 2 PERFORM CARREGA-LISTA2
                    END-EVALUATE
               WHEN CPP050-MUDAR-CABECALHO-TRUE
                    EVALUATE CPP050-FORMATO-RELATORIO
                       WHEN 1 PERFORM CABECALHO-TIPO-FORN
                       WHEN 2 PERFORM CABECALHO-APURACAO
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

       CABECALHO-TIPO-FORN SECTION.
           MOVE CAB04          TO CPP050-LINHA-CABECALHO.

       CABECALHO-APURACAO SECTION.
           MOVE CAB05          TO CPP050-LINHA-CABECALHO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CPP050-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INICIO-CPD020 SECTION.
           MOVE ZEROS    TO SITUACAO-CP20 DATA-VENCTO-CP20 FORNEC-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20.
           PERFORM VERIFICA-OPCAO
              THRU FIM-VERIFICA-OPCAO.

       VERIFICA-OPCAO SECTION.
           READ CPD020 NEXT RECORD AT END
                GO FIM-VERIFICA-OPCAO.

           IF SITUACAO-CP20 > 1
              GO FIM-VERIFICA-OPCAO.

           IF CPP050-A-PAGAR = 1
              IF CPP050-SUSPENSA = 1
                 IF SITUACAO-CP20 > 1
                    GO VERIFICA-OPCAO
                 ELSE
                    CONTINUE
              ELSE
                 IF SITUACAO-CP20 NOT = 0
                    GO VERIFICA-OPCAO
                ELSE
                    CONTINUE
           ELSE
              IF CPP050-SUSPENSA = 1
                 IF SITUACAO-CP20 NOT = 1
                    GO VERIFICA-OPCAO
                 ELSE
                    CONTINUE
              ELSE
                 GO VERIFICA-OPCAO.

           IF CPP050-DEFINITIVO = 1
              IF CPP050-PREVISTO = 1
                 CONTINUE
              ELSE
                 IF PREV-DEF-CP20 NOT = ZEROS
                    GO VERIFICA-OPCAO
                 ELSE
                    CONTINUE
           ELSE
              IF CPP050-PREVISTO = 1
                 IF PREV-DEF-CP20 NOT = 1
                    GO VERIFICA-OPCAO
                 ELSE
                    CONTINUE.


           EVALUATE CPP050-FORMATO-RELATORIO
               WHEN 1 MOVE TIPO-FORN-CP20        TO TIPO-FORN-WK
                      READ WORK INVALID KEY
                           INITIALIZE REG-WORK
                           PERFORM GRAVA-WORK
                      NOT INVALID KEY
                           PERFORM REGRAVA-WORK
                      END-READ
               WHEN 2 MOVE CODREDUZ-APUR-CP20    TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                           INITIALIZE REG-CXD020
                      END-READ
                      MOVE CODIGO-COMPL-CX20     TO CONTA-WK2
                      READ WORK2 INVALID KEY
                           MOVE VALOR-TOT-CP20   TO VALOR-WK2
                           WRITE REG-WORK2
                           END-WRITE
                      NOT INVALID KEY
                           ADD  VALOR-TOT-CP20   TO VALOR-WK2
                           REWRITE REG-WORK2
                           END-REWRITE
                      END-READ
           END-EVALUATE

           GO VERIFICA-OPCAO.

       FIM-VERIFICA-OPCAO SECTION.

       PREPARA-ARQ-WORK SECTION.
           IF ST-WORK = "35"
              ACCEPT VARIA-W FROM TIME
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK
           ELSE
              CLOSE       WORK
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK.

           IF ST-WORK2 = "35"
              ACCEPT VARIA-W2 FROM TIME
              ADD 300 TO VARIA-W2
              OPEN OUTPUT WORK2
              CLOSE       WORK2
              OPEN I-O    WORK2
           ELSE
              CLOSE       WORK2
              OPEN OUTPUT WORK2
              CLOSE       WORK2
              OPEN I-O    WORK2.

           IF CPP050-FORMATO-RELATORIO = 2
              MOVE ZEROS      TO CODIGO-REDUZ-CX20
              START CXD020 KEY IS NOT < CODIGO-REDUZ-CX20 INVALID KEY
                    MOVE "10" TO ST-CXD020
              END-START
              PERFORM UNTIL ST-CXD020 = "10"
                    READ CXD020 NEXT RECORD AT END
                         MOVE "10" TO ST-CXD020
                    NOT AT END
                        MOVE CODIGO-REDUZ-CX20     TO CONTA-REDUZ-WK2
                        MOVE CODIGO-COMPL-CX20     TO CONTA-WK2
                        MOVE GRAU-CX20             TO GRAU-WK2
                        MOVE ZEROS                 TO VALOR-WK2
                        WRITE REG-WORK2
                    END-READ
              END-PERFORM
           END-IF.


       GRAVA-WORK SECTION.
           MOVE FORNEC-CP20 TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "*********" TO TIPO-FORN-WK
           NOT INVALID KEY
                IF T-FRANQUIA-CG01 = 1
                   MOVE "FRANQUIA"   TO TIPO-FORN-WK
                ELSE
                   IF T-TERCEIRIZADO-CG01 = 1
                      MOVE "TERCEIRIZADO" TO TIPO-FORN-WK
                   ELSE
                      IF OUTRO3-CG01 = 1
                         MOVE "OUTROS" TO TIPO-FORN-WK
                      ELSE
                         IF T-INVESTIDOR-CG01 = 1
                            MOVE "INVESTIDOR" TO TIPO-FORN-WK
                         ELSE
                            IF T-IMPOSTO-CG01 = 1
                               MOVE "IMPOSTO" TO TIPO-FORN-WK
                            ELSE
                               IF T-VEND-CG01 = 1
                                  MOVE "VENDEDOR" TO TIPO-FORN-WK
                               ELSE
                                  IF T-CINEG-CG01 = 1
                                     MOVE "CINEGRAFISTA" TO TIPO-FORN-WK
                                  ELSE
                                     IF T-FOTOG-CG01 = 1
                                        MOVE "FOTOGRAFO" TO TIPO-FORN-WK
                                     ELSE
                                        IF T-REPRES-CG01 = 1
                                           MOVE "REPRESENTANTE" TO
                                                 TIPO-FORN-WK
                                        ELSE
                                           IF T-FUNC-CG01 = 1
                                              MOVE "FUNCIONARIOS" TO
                                                    TIPO-FORN-WK
                                           ELSE
                                              IF T-PESJUR-CG01 = 1
                                                 MOVE "JURIDICA" TO
                                                     TIPO-FORN-WK
                                              ELSE
                                                 IF T-PESFIS-CG01 = 1
                                                    MOVE "FISICA" TO
                                                       TIPO-FORN-WK
                                                 ELSE
                                                    MOVE "*********" TO
                                                       TIPO-FORN-WK.

           IF DATA-VENCTO-CP20 < DATA-DIA
              MOVE VALOR-TOT-CP20 TO VLR-ATRASADO-WK
              ADD VALOR-TOT-CP20  TO TOT-ATRASADO
           ELSE
             IF DATA-VENCTO-CP20 < DATA-30
                MOVE VALOR-TOT-CP20 TO VLR-1-30-WK
                ADD VALOR-TOT-CP20  TO TOT-1-30
             ELSE
               IF DATA-VENCTO-CP20 < DATA-60
                  MOVE VALOR-TOT-CP20 TO VLR-31-60-WK
                  ADD VALOR-TOT-CP20  TO TOT-31-60
               ELSE
                 IF DATA-VENCTO-CP20 < DATA-90
                    MOVE VALOR-TOT-CP20 TO VLR-61-90-WK
                    ADD VALOR-TOT-CP20  TO TOT-61-90
                 ELSE
                   IF DATA-VENCTO-CP20 < DATA-120
                      MOVE VALOR-TOT-CP20 TO VLR-91-120-WK
                      ADD VALOR-TOT-CP20  TO TOT-91-120
                   ELSE
                      MOVE VALOR-TOT-CP20 TO VLR-121-MAIS-WK
                      ADD VALOR-TOT-CP20  TO TOT-121-MAIS.

           MOVE VALOR-TOT-CP20    TO VLR-TOT-WK.
           ADD VALOR-TOT-CP20     TO TOT-TOTAL.
           WRITE REG-WORK.
       REGRAVA-WORK SECTION.
           IF DATA-VENCTO-CP20 < DATA-DIA
              ADD VALOR-TOT-CP20 TO VLR-ATRASADO-WK TOT-ATRASADO
           ELSE
             IF DATA-VENCTO-CP20 < DATA-30
                ADD VALOR-TOT-CP20 TO VLR-1-30-WK TOT-1-30
             ELSE
               IF DATA-VENCTO-CP20 < DATA-60
                  ADD VALOR-TOT-CP20 TO VLR-31-60-WK TOT-31-60
               ELSE
                 IF DATA-VENCTO-CP20 < DATA-90
                    ADD VALOR-TOT-CP20 TO VLR-61-90-WK TOT-61-90
                 ELSE
                   IF DATA-VENCTO-CP20 < DATA-120
                     ADD VALOR-TOT-CP20 TO VLR-91-120-WK TOT-91-120
                   ELSE
                     ADD VALOR-TOT-CP20 TO VLR-121-MAIS-WK TOT-121-MAIS.

           ADD VALOR-TOT-CP20 TO VLR-TOT-WK TOT-TOTAL.
           REWRITE REG-WORK.
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
           MOVE ZEROS TO TOT-TOTAL
                         TOT-ATRASADO
                         TOT-1-30
                         TOT-31-60
                         TOT-61-90
                         TOT-91-120
                         TOT-121-MAIS.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO TIPO-FORN-WK.
           START WORK KEY IS NOT < TIPO-FORN-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE SPACES          TO CPP050-LINDET
                      MOVE TIPO-FORN-WK    TO CPP050-LINDET(01: 21)
                      DIVIDE VLR-TOT-WK BY 1000 GIVING VALOR-E
                      MOVE VALOR-E         TO CPP050-LINDET(22: 14)
                      COMPUTE PERC = ((VLR-TOT-WK/1000) /
                                      (TOT-TOTAL/1000)) * 100
                      MOVE PERC            TO PERC-E
                      MOVE PERC-E          TO CPP050-LINDET(36: 06)
                      DIVIDE VLR-ATRASADO-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(42: 11)
                      DIVIDE VLR-1-30-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(53: 11)
                      DIVIDE VLR-31-60-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(64: 11)
                      DIVIDE VLR-61-90-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(75: 11)
                      DIVIDE VLR-91-120-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(86: 11)
                      DIVIDE VLR-121-MAIS-WK BY 1000 GIVING VALOR-E1
                      MOVE VALOR-E1        TO CPP050-LINDET(97: 11)
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
           MOVE "TOTAL"         TO CPP050-LINTOT(01: 21)
           DIVIDE TOT-TOTAL BY 1000 GIVING VALOR-E
           MOVE VALOR-E         TO CPP050-LINTOT(22: 14)
           MOVE 100             TO PERC-E
           MOVE PERC-E          TO CPP050-LINTOT(36: 06)
           DIVIDE TOT-ATRASADO BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(42: 11)
           DIVIDE TOT-1-30 BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(53: 11)
           DIVIDE TOT-31-60 BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(64: 11)
           DIVIDE TOT-61-90 BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(75: 11)
           DIVIDE TOT-91-120 BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(86: 11)
           DIVIDE TOT-121-MAIS BY 1000 GIVING VALOR-E1
           MOVE VALOR-E1        TO CPP050-LINTOT(97: 11)
           MOVE "INSERE-TOTAL" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-LISTA2 SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE 1010000         TO CONTA-WK2.
           READ WORK2 INVALID KEY
                MOVE ZEROS      TO VALOR-WK2 TOTAL-PERC
           NOT INVALID KEY
                MOVE VALOR-WK2  TO TOTAL-PERC.

           INITIALIZE REG-WORK2
           MOVE ZEROS          TO CONTA-WK2.
           START WORK2 KEY IS NOT < CONTA-WK2  INVALID KEY
                 MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
                 READ WORK2 NEXT RECORD AT END
                      MOVE "10" TO ST-WORK2
                 NOT AT END
                      MOVE SPACES          TO CPP050-LINDET
                      MOVE CONTA-WK2       TO CONTA-E
                      MOVE CONTA-REDUZ-WK2 TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                             MOVE SPACES   TO DESCRICAO-CX20
                             MOVE ZEROS    TO TIPO-CONTA-CX20
                      END-READ
                      IF TIPO-CONTA-CX20 = 1
                         MOVE SPACES       TO CPP050-LINDET
                         MOVE "INSERE-LIST" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                      EVALUATE GRAU-WK2
                        WHEN 1 PERFORM GRAU1
                        WHEN 2 PERFORM GRAU2
                        WHEN 3 PERFORM GRAU3
                        WHEN 4 PERFORM GRAU4
                      END-EVALUATE
                      MOVE VALOR-WK2         TO VALOR-E
                      MOVE VALOR-E           TO CPP050-LINDET(84: 14)
                      COMPUTE PERC-W = (VALOR-WK2 / TOTAL-PERC) * 100
                      MOVE PERC-W            TO PERC-E
                      MOVE PERC-E            TO CPP050-LINDET(99: 66)

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
           MOVE SPACES           TO CPP050-LINDET
           MOVE "INSERE-LIST"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       GRAU1 SECTION.
           MOVE CONTA-E           TO CPP050-LINDET(01: 10)
           MOVE "("               TO CPP050-LINDET(12: 01)
           MOVE CONTA-REDUZ-WK2   TO CPP050-LINDET(13: 05)
           MOVE ")"               TO CPP050-LINDET(18: 01).
           MOVE DESCRICAO-CX20    TO CPP050-LINDET(19: 30).
       GRAU2 SECTION.
           MOVE CONTA-E           TO CPP050-LINDET(12: 10)
           MOVE "("               TO CPP050-LINDET(23: 01)
           MOVE CONTA-REDUZ-WK2   TO CPP050-LINDET(24: 05)
           MOVE ")"               TO CPP050-LINDET(29: 01).
           MOVE DESCRICAO-CX20    TO CPP050-LINDET(30: 30).
       GRAU3 SECTION.
           MOVE CONTA-E           TO CPP050-LINDET(23: 10)
           MOVE "("               TO CPP050-LINDET(34: 01)
           MOVE CONTA-REDUZ-WK2   TO CPP050-LINDET(35: 05)
           MOVE ")"               TO CPP050-LINDET(40: 01).
           MOVE DESCRICAO-CX20    TO CPP050-LINDET(41: 30).
       GRAU4 SECTION.
           MOVE CONTA-E           TO CPP050-LINDET(34: 10)
           MOVE "("               TO CPP050-LINDET(45: 01)
           MOVE CONTA-REDUZ-WK2   TO CPP050-LINDET(46: 05)
           MOVE ")"               TO CPP050-LINDET(51: 01).
           MOVE DESCRICAO-CX20    TO CPP050-LINDET(52: 30).

       CLEAR-FLAGS SECTION.
           INITIALIZE CPP050-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP050" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN PAG-W.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL

           EVALUATE CPP050-FORMATO-RELATORIO
                WHEN 1 PERFORM IMPRIMIR-TIPO-FORN
                WHEN 2 PERFORM IMPRIMIR-APURACAO
           END-EVALUATE

           copy descondensa.

       IMPRIMIR-TIPO-FORN SECTION.
           MOVE ZEROS TO TIPO-FORN-WK.
           START WORK KEY IS NOT < TIPO-FORN-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      MOVE SPACES          TO LINDET-REL
                      MOVE TIPO-FORN-WK    TO LINDET-REL(1:27)
                      DIVIDE VLR-TOT-WK BY 1000 GIVING VALOR-E
                      MOVE VALOR-E         TO LINDET-REL(29: 14)
                      COMPUTE PERC = ((VLR-TOT-WK/1000) /
                                      (TOT-TOTAL/1000)) * 100
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
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
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
           WRITE REG-RELAT FROM LINDET AFTER 2.

       IMPRIMIR-APURACAO SECTION.
           MOVE 1010000         TO CONTA-WK2.
           READ WORK2 INVALID KEY
                MOVE ZEROS      TO VALOR-WK2 TOTAL-PERC
           NOT INVALID KEY
                MOVE VALOR-WK2  TO TOTAL-PERC.

           INITIALIZE REG-WORK2
           MOVE ZEROS          TO CONTA-WK2.
           START WORK2 KEY IS NOT < CONTA-WK2  INVALID KEY
                 MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
                 READ WORK2 NEXT RECORD AT END
                      MOVE "10" TO ST-WORK2
                 NOT AT END
                      MOVE SPACES          TO CPP050-LINDET
                      MOVE CONTA-WK2       TO CONTA-E
                      MOVE CONTA-REDUZ-WK2 TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                             MOVE SPACES   TO DESCRICAO-CX20
                             MOVE ZEROS    TO TIPO-CONTA-CX20
                      END-READ
                      IF TIPO-CONTA-CX20 = 1
                         MOVE SPACES       TO CPP050-LINDET
                         WRITE REG-RELAT FROM CPP050-LINDET
                      END-IF
                      EVALUATE GRAU-WK2
                        WHEN 1 PERFORM GRAU1
                        WHEN 2 PERFORM GRAU2
                        WHEN 3 PERFORM GRAU3
                        WHEN 4 PERFORM GRAU4
                      END-EVALUATE
                      MOVE VALOR-WK2         TO VALOR-E
                      MOVE VALOR-E           TO CPP050-LINDET(84: 14)
                      COMPUTE PERC-W = (VALOR-WK2 / TOTAL-PERC) * 100
                      MOVE PERC-W            TO PERC-E
                      MOVE PERC-E            TO CPP050-LINDET(99: 66)

                      WRITE REG-RELAT FROM CPP050-LINDET
                 END-READ
           END-PERFORM.
           MOVE SPACES           TO CPP050-LINDET
           WRITE REG-RELAT FROM CPP050-LINDET.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PAG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           EVALUATE CPP050-FORMATO-RELATORIO
               WHEN 1 WRITE REG-RELAT FROM CAB04
               WHEN 2 WRITE REG-RELAT FROM CAB05
           END-EVALUATE
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP050-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CPD020 CGD001 WORK WORK2 CXD020
           DELETE FILE WORK.
           DELETE FILE WORK2.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.
