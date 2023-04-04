       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP151.
      *DATA: 11/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Relatório de Apuração de Resultados - Caixa -MES/ANO
      *FUNÇÃO: Relaciona todo o movimento dentro de um intervalo de
      *        MES/ANO solicitado. A conta a ser considerada 100%
      *        será a conta 1.01.00.00(260)-Entrada de recursos. As
      *        percentagens serão arredondadas.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX042.
           COPY CXPX020.
           COPY CXPX100.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CONTA-WK
                  ALTERNATE RECORD KEY IS CONTA-REDUZ-WK
                  STATUS IS ST-WORK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       COPY CXPW042.
       COPY CXPW100.

       FD  WORK.
       01  REG-WORK.
           05  CONTA-WK        PIC 9(7).
           05  CONTA-REDUZ-WK  PIC 9(5).
           05  VALOR-WK        PIC S9(8)V99.
           05  GRAU-WK         PIC 9.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD EXCEL.
       01 REG-EXCEL.
          05 EXCEL-DESCRICAO           PIC X(90).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-VALOR               PIC X(20).
          05 FILLER                    PIC X(01) VALUE ";".
          05 EXCEL-PERCENTUAL          PIC X(20).
          05 FILLER                    PIC X(02) VALUE X"0DA0".

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP151.CPB".
           COPY "CXP151.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD042             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  CONTA-E               PIC 9.99.99.99.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ENT             PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-SAI             PIC 9(8)V99  VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  MESANO-CORRENTE       PIC 9(6)     VALUE ZEROS.
           05  cont                  pic 9(2)     value zeros.
      *  MES/ANO CORRENTE - LERÁ O SALDO A PARTIR DO ARQUIVO CXD100
      *      ENQUANTO OS DEMAIS DO CXD040(ARQUIVO DE SALDO)
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  TOTAL-PERC            PIC 9(8)V99  VALUE ZEROS.
           05  PERC-W                PIC 999V99   VALUE ZEROS.
           05  PERC-E                PIC ZZZ,ZZ.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(52)   VALUE
           "APURACAO DE RESULTADOS - MES/ANO".
           05  FILLER              PIC X(21)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV. MOVTO: ".
           05  MESANO-INI-REL      PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL      PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(105)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(105)  VALUE
           "DESCRICAO DAS CONTAS
      -    "                   VALOR-R$ PERC.%".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(105)  VALUE SPACES.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP151-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-INV(5: 4) TO MESANO-CORRENTE(1: 4).
           MOVE DATA-INV(3: 2) TO MESANO-CORRENTE(5: 2)
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP151-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP151-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP151-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD042"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD042.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           OPEN INPUT CXD100 CXD042 CXD020.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP151-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP151-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD042 <> "00"
              MOVE "ERRO ABERTURA CXD042: "  TO CXP151-MENSAGEM-ERRO
              MOVE ST-CXD042 TO CXP151-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP151-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP151-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP151-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP151-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP151-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP151-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CXP151-VERIFICA-DATA-TRUE
                    PERFORM INVERTE-DATA
               WHEN CXP151-EXCEL-TRUE
                    PERFORM GERAR-EXCEL
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       GERAR-EXCEL SECTION.
           MOVE SPACES TO ARQUIVO-EXCEL
           STRING "\ARQUIVOS\CXP151-" CXP151-MESANO-INI "-"
                                      CXP151-MESANO-FIM ".CSV"
           INTO ARQUIVO-EXCEL

           OPEN OUTPUT EXCEL

           INITIALIZE REG-EXCEL
           STRING "INTERV. MOVTO: " CXP151-MESANO-INI(1:2) "/"
                                    CXP151-MESANO-INI(3:4)
                  " ATE " CXP151-MESANO-FIM(1:2) "/"
                          CXP151-MESANO-FIM(3:4) INTO EXCEL-DESCRICAO
           WRITE REG-EXCEL



           MOVE "DESCRICAO"        TO EXCEL-DESCRICAO
           MOVE "VALOR"            TO EXCEL-VALOR
           MOVE "%"                TO EXCEL-PERCENTUAL
           WRITE REG-EXCEL


           MOVE ZEROS             TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-CXD100
                 NOT AT END
                      INITIALIZE EXCEL-DESCRICAO
                                 EXCEL-VALOR
                                 EXCEL-PERCENTUAL
                      MOVE SPACES            TO LINDET-REL
                      MOVE CONTA-WK          TO CONTA-E
                      MOVE CONTA-REDUZ-WK    TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                           MOVE SPACES TO DESCRICAO-CX20
                           MOVE ZEROS  TO TIPO-CONTA-CX20
                      END-READ
                      IF TIPO-CONTA-CX20 = 1
                         INITIALIZE EXCEL-DESCRICAO
                                    EXCEL-VALOR
                                    EXCEL-PERCENTUAL
                         WRITE REG-EXCEL
                      END-IF
                      EVALUATE GRAU-WK
                         WHEN 1 PERFORM GRAU1-IMP
                         WHEN 2 PERFORM GRAU2-IMP
                         WHEN 3 PERFORM GRAU3-IMP
                         WHEN 4 PERFORM GRAU4-IMP
                      END-EVALUATE
                      MOVE LINDET-REL(1:80)  TO EXCEL-DESCRICAO
                      MOVE VALOR-WK          TO VALOR-E
                      MOVE VALOR-E           TO EXCEL-VALOR
                      COMPUTE PERC-W = (VALOR-WK / TOTAL-PERC) * 100
                      MOVE PERC-W            TO PERC-E
                      MOVE PERC-E            TO EXCEL-PERCENTUAL
                      WRITE REG-EXCEL
                 END-READ
           END-PERFORM.
           INITIALIZE EXCEL-DESCRICAO
                      EXCEL-VALOR
                      EXCEL-PERCENTUAL
           WRITE REG-EXCEL

           INITIALIZE EXCEL-DESCRICAO
                      EXCEL-VALOR
                      EXCEL-PERCENTUAL
           MOVE "TOTAL GERAL..." TO EXCEL-DESCRICAO
           MOVE SALDO-FINAL      TO VALOR-E
           MOVE VALOR-E          TO EXCEL-VALOR
           COMPUTE PERC-W = (SALDO-FINAL / TOTAL-PERC) * 100.
           MOVE PERC-W           TO PERC-E.
           MOVE PERC-E           TO EXCEL-PERCENTUAL
           WRITE REG-EXCEL

           CLOSE EXCEL.
       GERAR-EXCEL-FIM.
           EXIT.


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
           INITIALIZE CXP151-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP151-MESANO-INI       TO MESANO-INI-REL.
           MOVE CXP151-MESANO-INI(1: 2) TO MESANO-INI(5: 2)
           MOVE CXP151-MESANO-INI(3: 4) TO MESANO-INI(1: 4).
           MOVE MESANO-INI              TO MESANO-SALDO-ANT.
           MOVE CXP151-MESANO-FIM TO MESANO-FIM-REL.
           MOVE CXP151-MESANO-FIM(1: 2) TO MESANO-FIM(5: 2)
           MOVE CXP151-MESANO-FIM(3: 4) TO MESANO-FIM(1: 4).

           IF MESANO-INI > MESANO-CORRENTE MOVE 1 TO CXP151-ERRO-DATA
           ELSE MOVE 0 TO CXP151-ERRO-DATA.
       INICIO-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK
      *    Grava todas as contas existentes no cxd020
           MOVE "GRAVANDO..." TO CXP151-MENSAGEM-AGUARDA.
           MOVE 1          TO CONTA-WK.
           MOVE 99999      TO CONTA-REDUZ-WK.
           MOVE 1          TO GRAU-WK.
           MOVE ZEROS      TO VALOR-WK.
           WRITE REG-WORK.
      *    Conta p/ gravar valores de contas não cadastradas
           MOVE ZEROS      TO CODIGO-REDUZ-CX20.
           START CXD020 KEY IS NOT < CODIGO-REDUZ-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
             READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
               NOT AT END
                 MOVE CODIGO-REDUZ-CX20     TO
                            CXP151-MENSAGEM-AGUARDA(12: 25)
                 MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE CODIGO-REDUZ-CX20     TO CONTA-REDUZ-WK
                 MOVE CODIGO-COMPL-CX20     TO CONTA-WK
                 MOVE GRAU-CX20             TO GRAU-WK
                 MOVE ZEROS                 TO VALOR-WK
                 WRITE REG-WORK
             END-READ
           END-PERFORM.
           MOVE MESANO-INI    TO ANOMES-CX42.
           MOVE ZEROS         TO CONTAREDUZ-CX42 SALDO-FINAL.
           START CXD042 KEY IS NOT < CHAVE-CX42 INVALID KEY
                 MOVE "10" TO ST-CXD042.
           PERFORM UNTIL ST-CXD042 = "10"
             READ CXD042 NEXT RECORD AT END MOVE "10" TO ST-CXD042
               NOT AT END
                 IF ANOMES-CX42 > MESANO-FIM MOVE "10" TO ST-CXD042
                 ELSE
                   IF ANOMES-CX42 NOT < MESANO-CORRENTE
                          MOVE "10" TO ST-CXD042
                   ELSE
                      PERFORM LE-CODIGOCOMPL-42
                      MOVE CODIGO-COMPL-CX20 TO CONTA-WK
                      READ WORK
                      END-READ
                      MOVE CONTA-REDUZ-WK     TO
                            CXP151-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE ZEROS TO VALOR-SAI VALOR-ENT
                      MOVE SALDOE-CX42 TO VALOR-ENT
                      MOVE SALDOS-CX42 TO VALOR-SAI
                      ADD SALDOE-CX42 TO VALOR-WK SALDO-FINAL
                      SUBTRACT SALDOS-CX42 FROM VALOR-WK SALDO-FINAL
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
           MOVE MESANO-CORRENTE TO DATA-MOV-CX100(1: 6)
           MOVE 01              TO DATA-MOV-CX100(7: 2)
           MOVE ZEROS           TO SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                 MOVE DATA-MOV-CX100(1: 6) TO MESANO-I
                 IF MESANO-I > MESANO-FIM
                               MOVE "10" TO ST-CXD100
                 ELSE
                   IF CONTA-REDUZ-CX100 = 888 CONTINUE
                   ELSE
                      MOVE CONTA-REDUZ-CX100 TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                           MOVE 1 TO CODIGO-COMPL-CX20
                      END-READ
                      MOVE CODIGO-COMPL-CX20 TO CONTA-WK
                      READ WORK
                      END-READ
                      MOVE CONTA-REDUZ-WK     TO
                            CXP151-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE ZEROS TO VALOR-SAI VALOR-ENT
                      IF TIPO-LCTO-CX100 < 50
                             MOVE VALOR-CX100 TO VALOR-SAI
                             SUBTRACT
                                VALOR-CX100 FROM VALOR-WK SALDO-FINAL
                      ELSE ADD VALOR-CX100 TO VALOR-WK SALDO-FINAL
                           MOVE VALOR-CX100 TO VALOR-ENT
                      END-IF
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA
                   END-IF
             END-READ
           END-PERFORM.
       LE-CODIGOCOMPL-42 SECTION.
           MOVE CONTAREDUZ-CX42 TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE 1 TO CODIGO-COMPL-CX20.
       TOTALIZA SECTION.
           EVALUATE GRAU-WK
             WHEN 4 MOVE ZEROS TO CONTA-WK(6: 2)
                    PERFORM TOTALIZA1
                    MOVE ZEROS TO CONTA-WK(4: 4)
                    PERFORM TOTALIZA1
                    MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA1
             WHEN 3 MOVE ZEROS TO CONTA-WK(4: 4)
                    PERFORM TOTALIZA1
                    MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA1
             WHEN 2 MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA1
           END-EVALUATE.
       TOTALIZA1 SECTION.
           READ WORK INVALID KEY CONTINUE
             NOT INVALID KEY ADD VALOR-ENT      TO VALOR-WK
                             SUBTRACT VALOR-SAI FROM VALOR-WK
                             REWRITE REG-WORK.
       CONTA-PERCENTAGEM SECTION.
           MOVE "UNSHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1010000 TO CONTA-WK.
           READ WORK INVALID KEY MOVE ZEROS TO VALOR-WK TOTAL-PERC
               NOT INVALID KEY MOVE VALOR-WK  TO TOTAL-PERC.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CXP151-LINDET.
           PERFORM INICIO-WORK.
           PERFORM CONTA-PERCENTAGEM.
           MOVE ZEROS          TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-CXD100
              NOT AT END
                    MOVE SPACES            TO CXP151-LINDET
                    MOVE CONTA-WK       TO CONTA-E
                    MOVE CONTA-REDUZ-WK TO CODIGO-REDUZ-CX20
                    READ CXD020 INVALID KEY
                           MOVE SPACES TO DESCRICAO-CX20
                           MOVE ZEROS  TO TIPO-CONTA-CX20
                    END-READ
                    IF TIPO-CONTA-CX20 = 1 PERFORM INSERE-LINHA-BRANCO
                    END-IF
                    EVALUATE GRAU-WK
                      WHEN 1 PERFORM GRAU1
                      WHEN 2 PERFORM GRAU2
                      WHEN 3 PERFORM GRAU3
                      WHEN 4 PERFORM GRAU4
                    END-EVALUATE
                    MOVE VALOR-WK          TO VALOR-E
                    MOVE VALOR-E           TO CXP151-LINDET(84: 14)
                    COMPUTE PERC-W = (VALOR-WK / TOTAL-PERC) * 100
                    MOVE PERC-W            TO PERC-E
                    MOVE PERC-E            TO CXP151-LINDET(99: 66)
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE SPACES TO CXP151-LINDET.
           PERFORM INSERE-LINHA-BRANCO.
           MOVE "TOTAL GERAL..." TO CXP151-LINDET(60: 14).
           MOVE SALDO-FINAL      TO VALOR-E.
           MOVE VALOR-E          TO CXP151-LINDET(84: 14).
           COMPUTE PERC-W = (SALDO-FINAL / TOTAL-PERC) * 100.
           MOVE PERC-W           TO PERC-E.
           MOVE PERC-E           TO CXP151-LINDET(99: 66).
           MOVE "INSERE-LIST"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAU1 SECTION.
           MOVE CONTA-E           TO CXP151-LINDET(01: 10)
           MOVE "("               TO CXP151-LINDET(12: 01)
           MOVE CONTA-REDUZ-WK    TO CXP151-LINDET(13: 05)
           MOVE ")"               TO CXP151-LINDET(18: 01).
           MOVE DESCRICAO-CX20    TO CXP151-LINDET(19: 30).
       GRAU2 SECTION.
           MOVE CONTA-E           TO CXP151-LINDET(12: 10)
           MOVE "("               TO CXP151-LINDET(23: 01)
           MOVE CONTA-REDUZ-WK    TO CXP151-LINDET(24: 05)
           MOVE ")"               TO CXP151-LINDET(29: 01).
           MOVE DESCRICAO-CX20    TO CXP151-LINDET(30: 30).
       GRAU3 SECTION.
           MOVE CONTA-E           TO CXP151-LINDET(23: 10)
           MOVE "("               TO CXP151-LINDET(34: 01)
           MOVE CONTA-REDUZ-WK    TO CXP151-LINDET(35: 05)
           MOVE ")"               TO CXP151-LINDET(40: 01).
           MOVE DESCRICAO-CX20    TO CXP151-LINDET(41: 30).
       GRAU4 SECTION.
           MOVE CONTA-E           TO CXP151-LINDET(34: 10)
           MOVE "("               TO CXP151-LINDET(45: 01)
           MOVE CONTA-REDUZ-WK    TO CXP151-LINDET(46: 05)
           MOVE ")"               TO CXP151-LINDET(51: 01).
           MOVE DESCRICAO-CX20    TO CXP151-LINDET(52: 30).
       INSERE-LINHA-BRANCO SECTION.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP151-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP151"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE ZEROS             TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-CXD100
              NOT AT END
                    MOVE SPACES            TO LINDET-REL
                    MOVE CONTA-WK          TO CONTA-E
                    MOVE CONTA-REDUZ-WK    TO CODIGO-REDUZ-CX20
                    READ CXD020 INVALID KEY
                         MOVE SPACES TO DESCRICAO-CX20
                         MOVE ZEROS  TO TIPO-CONTA-CX20
                    END-READ
                    IF TIPO-CONTA-CX20 = 1 PERFORM LINHA-BRANCO-IMP
                    END-IF
                    EVALUATE GRAU-WK
                      WHEN 1 PERFORM GRAU1-IMP
                      WHEN 2 PERFORM GRAU2-IMP
                      WHEN 3 PERFORM GRAU3-IMP
                      WHEN 4 PERFORM GRAU4-IMP
                    END-EVALUATE
                    MOVE VALOR-WK          TO VALOR-E
                    MOVE VALOR-E           TO LINDET-REL(84: 14)
                    COMPUTE PERC-W = (VALOR-WK / TOTAL-PERC) * 100
                    MOVE PERC-W            TO PERC-E
                    MOVE PERC-E            TO LINDET-REL(99: 6)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO CXP151-LINDET.
           PERFORM LINHA-BRANCO-IMP.
           MOVE "TOTAL GERAL..." TO LINDET-REL(60: 14).
           MOVE SALDO-FINAL      TO VALOR-E.
           MOVE VALOR-E          TO LINDET-REL(84: 14).
           COMPUTE PERC-W = (SALDO-FINAL / TOTAL-PERC) * 100.
           MOVE PERC-W           TO PERC-E.
           MOVE PERC-E           TO LINDET-REL(99: 66).
           WRITE REG-RELAT FROM LINDET AFTER 2.

           COPY DESCONDENSA.

       GRAU1-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(01: 10)
           MOVE "("               TO LINDET-REL(12: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(13: 05)
           MOVE ")"               TO LINDET-REL(18: 01).
           MOVE DESCRICAO-CX20    TO LINDET-REL(19: 30).
       GRAU2-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(12: 10)
           MOVE "("               TO LINDET-REL(23: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(24: 05)
           MOVE ")"               TO LINDET-REL(29: 01).
           MOVE DESCRICAO-CX20    TO LINDET-REL(30: 30).
       GRAU3-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(23: 10)
           MOVE "("               TO LINDET-REL(34: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(35: 05)
           MOVE ")"               TO LINDET-REL(40: 01).
           MOVE DESCRICAO-CX20    TO LINDET-REL(41: 30).
       GRAU4-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(34: 10)
           MOVE "("               TO LINDET-REL(45: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(46: 05)
           MOVE ")"               TO LINDET-REL(51: 01).
           MOVE DESCRICAO-CX20    TO LINDET-REL(52: 30).
       LINHA-BRANCO-IMP SECTION.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO  LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP151-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CXD020 CXD042 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
