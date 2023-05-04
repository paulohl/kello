       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP060.
      *DATA: 17/03/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Relatório de Apuração de Resultados - Caixa - Int.dias
      *FUNÇÃO: Relaciona todo o movimento dentro de um intervalo de
      *        dias solicitado. A conta a ser considerada 100%
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
           COPY CXPX020.
           COPY CXPX100.
           COPY CPPX020.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CONTA-WK
                  ALTERNATE RECORD KEY IS CONTA-REDUZ-WK
                  STATUS IS ST-WORK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       COPY CXPW100.
       COPY CPPW020.
       FD  WORK.
       01  REG-WORK.
           05  CONTA-WK        PIC 9(7).
           05  CONTA-REDUZ-WK  PIC 9(3).
           05  VALOR-CAIXA-WK  PIC S9(8)V99.
           05  VALOR-PG-VENC-WK PIC S9(8)V99.
           05  VALOR-PG-AVEN-WK PIC S9(8)V99.
           05  TOTAL-WK        PIC S9(8)V99.
           05  GRAU-WK         PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CPP060.CPB".
           COPY "CPP060.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  VALOR-ENT             PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-SAI             PIC 9(8)V99  VALUE ZEROS.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-CORRENTE       PIC 9(6)     VALUE ZEROS.
      *  MES/ANO CORRENTE - LERÁ O SALDO A PARTIR DO ARQUIVO CPD020
      *      ENQUANTO OS DEMAIS DO CXD040(ARQUIVO DE SALDO)
           05  DATA-DIA              PIC 9(8)     VALUE ZEROS.
           05  SALDO-FINAL-CAIXA     PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL-AVENCER   PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL-VENCIDO   PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL-TOTAL     PIC S9(8)V99 VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  TOTAL-PERC            PIC 9(8)V99  VALUE ZEROS.
           05  PERC-W                PIC 999V99   VALUE ZEROS.
           05  PERC-E                PIC ZZZ,ZZ.
           05  LIN                   PIC 9(02) VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(66)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(74)   VALUE
           "PLANO DE CONTAS - PENDENCIAS NO CONTAS A PAGAR - IND-DIAS".
           05  FILLER              PIC X(15)   VALUE "INTERV. MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(112)  VALUE
           "DESCRIÇÃO DAS CONTAS                                   VALOR
      -    "-CAIXA  PAGAR-VENCIDO   PAGAR-VENCER          TOTAL".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(112)  VALUE SPACES.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-INV(5: 4) TO MESANO-CORRENTE(1: 4).
           MOVE DATA-INV(3: 2) TO MESANO-CORRENTE(5: 2).
           MOVE DATA-INV(1: 2) TO DATA-DIA(7: 2)
           MOVE DATA-INV(3: 2) TO DATA-DIA(5: 2)
           MOVE DATA-INV(5: 4) TO DATA-DIA(1: 4).
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           OPEN INPUT CPD020 CXD020 CXD100.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
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
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
           END-EVALUATE.
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
       INVERTE-DATA SECTION.
           MOVE GS-DATA-INI    TO DATA-INI-REL DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV           TO DATA-INI.
           MOVE GS-DATA-FIM    TO DATA-FIM-REL DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV           TO DATA-FIM.
           PERFORM INICIO-WORK.
       INICIO-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK
      *    Grava todas as contas existentes no cxd020
           MOVE "GRAVANDO..." TO GS-MENSAGEM-AGUARDA.
           MOVE 1          TO CONTA-WK.
           MOVE 999        TO CONTA-REDUZ-WK.
           MOVE 1          TO GRAU-WK.
           MOVE ZEROS      TO VALOR-CAIXA-WK VALOR-PG-VENC-WK
                              VALOR-PG-AVEN-WK TOTAL-WK.
           WRITE REG-WORK.
      *    Conta p/ gravar valores de contas não cadastradas
           MOVE ZEROS      TO CODIGO-REDUZ-CX20.
           START CXD020 KEY IS NOT < CODIGO-REDUZ-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
             READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
               NOT AT END
                 MOVE CODIGO-REDUZ-CX20     TO
                            GS-MENSAGEM-AGUARDA(12: 25)
                 MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE CODIGO-REDUZ-CX20     TO CONTA-REDUZ-WK
                 MOVE CODIGO-COMPL-CX20     TO CONTA-WK
                 MOVE GRAU-CX20             TO GRAU-WK
                 MOVE ZEROS                 TO VALOR-CAIXA-WK
                                               VALOR-PG-VENC-WK
                                               VALOR-PG-AVEN-WK
                                               TOTAL-WK
                 WRITE REG-WORK
             END-READ
           END-PERFORM.
           MOVE ZEROS           TO SALDO-FINAL-CAIXA
                                   SALDO-FINAL-VENCIDO
                                   SALDO-FINAL-AVENCER
                                   SALDO-FINAL-TOTAL
           MOVE ZEROS           TO SITUACAO-CP20 FORNEC-CP20.
           MOVE DATA-INI        TO DATA-VENCTO-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
                NOT AT END
                 IF DATA-VENCTO-CP20 > DATA-FIM OR SITUACAO-CP20 > 0
                    MOVE "10" TO ST-CPD020
                 ELSE
                   IF CODREDUZ-APUR-CP20 = 888 CONTINUE
                   ELSE
                      MOVE CODREDUZ-APUR-CP20 TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                           MOVE 1 TO CODIGO-COMPL-CX20
                      END-READ
                      MOVE CODIGO-COMPL-CX20 TO CONTA-WK
                      READ WORK
                      END-READ
                      MOVE CONTA-REDUZ-WK     TO
                            GS-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF DATA-VENCTO-CP20 < DATA-DIA
                         SUBTRACT VALOR-TOT-CP20 FROM
                             VALOR-PG-VENC-WK SALDO-FINAL-VENCIDO
                             TOTAL-WK SALDO-FINAL-TOTAL
                      ELSE
                         SUBTRACT VALOR-TOT-CP20 FROM
                             VALOR-PG-AVEN-WK SALDO-FINAL-AVENCER
                             TOTAL-WK SALDO-FINAL-TOTAL
                      END-IF
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA-PAGAR
                   END-IF
             END-READ
           END-PERFORM.
      *  MOVER CAIXA
           MOVE DATA-INI        TO DATA-MOV-CX100
           MOVE ZEROS           TO SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                 IF DATA-MOV-CX100 > DATA-FIM MOVE "10" TO ST-CXD100
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
                            GS-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE ZEROS TO VALOR-SAI VALOR-ENT
                      IF TIPO-LCTO-CX100 < 50
                             MOVE VALOR-CX100 TO VALOR-SAI
                             SUBTRACT VALOR-CX100 FROM
                              VALOR-CAIXA-WK SALDO-FINAL-CAIXA TOTAL-WK
                              SALDO-FINAL-TOTAL
                      ELSE ADD VALOR-CX100 TO VALOR-CAIXA-WK
                           SALDO-FINAL-CAIXA TOTAL-WK SALDO-FINAL-TOTAL
                           MOVE VALOR-CX100 TO VALOR-ENT
                      END-IF
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA-CAIXA
                   END-IF
             END-READ
           END-PERFORM.
           MOVE "UNSHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA-CAIXA SECTION.
           EVALUATE GRAU-WK
             WHEN 4 MOVE ZEROS TO CONTA-WK(6: 2)
                    PERFORM TOTALIZA0
                    MOVE ZEROS TO CONTA-WK(4: 4)
                    PERFORM TOTALIZA0
                    MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA0
             WHEN 3 MOVE ZEROS TO CONTA-WK(4: 4)
                    PERFORM TOTALIZA0
                    MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA0
             WHEN 2 MOVE ZEROS TO CONTA-WK(2: 6)
                    PERFORM TOTALIZA0
           END-EVALUATE.
       TOTALIZA0 SECTION.
           READ WORK INVALID KEY CONTINUE
             NOT INVALID KEY ADD VALOR-ENT      TO VALOR-CAIXA-WK
                                                   TOTAL-WK
                             SUBTRACT VALOR-SAI FROM VALOR-CAIXA-WK
                                                     TOTAL-WK
                             REWRITE REG-WORK.

       TOTALIZA-PAGAR SECTION.
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
             NOT INVALID KEY
                 IF DATA-VENCTO-CP20 < DATA-DIA
                    SUBTRACT VALOR-TOT-CP20 FROM
                        VALOR-PG-VENC-WK TOTAL-WK
                 ELSE
                    SUBTRACT VALOR-TOT-CP20 FROM
                        VALOR-PG-AVEN-WK TOTAL-WK
                 END-IF
                 REWRITE REG-WORK
                 END-REWRITE.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM INVERTE-DATA.
           MOVE ZEROS          TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-CPD020
              NOT AT END
                    MOVE SPACES            TO GS-LINDET
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
                    MOVE VALOR-CAIXA-WK    TO VALOR-E
                    MOVE VALOR-E           TO GS-LINDET(53: 15)
                    MOVE VALOR-PG-VENC-WK  TO VALOR-E
                    MOVE VALOR-E           TO GS-LINDET(68: 15)
                    MOVE VALOR-PG-AVEN-WK  TO VALOR-E
                    MOVE VALOR-E           TO GS-LINDET(83: 15)
                    MOVE TOTAL-WK          TO VALOR-E
                    MOVE VALOR-E           TO GS-LINDET(97: 14)
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET.
           PERFORM INSERE-LINHA-BRANCO.
           MOVE "TOTAL GERAL..."    TO GS-LINDET(35: 14).
           MOVE SALDO-FINAL-CAIXA   TO VALOR-E.
           MOVE VALOR-E             TO GS-LINDET(53: 15).
           MOVE SALDO-FINAL-VENCIDO TO VALOR-E.
           MOVE VALOR-E             TO GS-LINDET(68: 15).
           MOVE SALDO-FINAL-AVENCER TO VALOR-E.
           MOVE VALOR-E             TO GS-LINDET(83: 15).
           MOVE SALDO-FINAL-TOTAL   TO VALOR-E
           MOVE VALOR-E             TO GS-LINDET(97: 14).
           MOVE "INSERE-LIST"       TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAU1 SECTION.
           MOVE CONTA-E           TO GS-LINDET(01: 11)
           MOVE "("               TO GS-LINDET(12: 01)
           MOVE CONTA-REDUZ-WK    TO GS-LINDET(13: 03)
           MOVE ")"               TO GS-LINDET(16: 02).
           MOVE DESCRICAO-CX20    TO GS-LINDET(18: 30).
       GRAU2 SECTION.
           MOVE CONTA-E           TO GS-LINDET(02: 11)
           MOVE "("               TO GS-LINDET(13: 01)
           MOVE CONTA-REDUZ-WK    TO GS-LINDET(14: 03)
           MOVE ")"               TO GS-LINDET(17: 02).
           MOVE DESCRICAO-CX20    TO GS-LINDET(19: 30).
       GRAU3 SECTION.
           MOVE CONTA-E           TO GS-LINDET(04: 11)
           MOVE "("               TO GS-LINDET(15: 01)
           MOVE CONTA-REDUZ-WK    TO GS-LINDET(16: 03)
           MOVE ")"               TO GS-LINDET(19: 02).
           MOVE DESCRICAO-CX20    TO GS-LINDET(21: 30).
       GRAU4 SECTION.
           MOVE CONTA-E           TO GS-LINDET(06: 11)
           MOVE "("               TO GS-LINDET(17: 01)
           MOVE CONTA-REDUZ-WK    TO GS-LINDET(18: 03)
           MOVE ")"               TO GS-LINDET(21: 02).
           MOVE DESCRICAO-CX20    TO GS-LINDET(23: 30).
       INSERE-LINHA-BRANCO SECTION.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP060"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE ZEROS             TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
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
                    MOVE VALOR-CAIXA-WK    TO VALOR-E
                    MOVE VALOR-E           TO LINDET-REL(53: 15)
                    MOVE VALOR-PG-VENC-WK  TO VALOR-E
                    MOVE VALOR-E           TO LINDET-REL(68: 15)
                    MOVE VALOR-PG-AVEN-WK  TO VALOR-E
                    MOVE VALOR-E           TO LINDET-REL(83: 15)
                    MOVE TOTAL-WK          TO VALOR-E
                    MOVE VALOR-E           TO LINDET-REL(97: 14)
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET.
           PERFORM LINHA-BRANCO-IMP.

           MOVE "TOTAL GERAL..."    TO LINDET-REL(35: 14).
           MOVE SALDO-FINAL-CAIXA   TO VALOR-E.
           MOVE VALOR-E             TO LINDET-REL(53: 15).
           MOVE SALDO-FINAL-VENCIDO TO VALOR-E.
           MOVE VALOR-E             TO LINDET-REL(68: 15).
           MOVE SALDO-FINAL-AVENCER TO VALOR-E.
           MOVE VALOR-E             TO LINDET-REL(83: 15).
           MOVE SALDO-FINAL-TOTAL   TO VALOR-E
           MOVE VALOR-E             TO LINDET-REL(97: 14).

           WRITE REG-RELAT FROM LINDET AFTER 2.

           COPY DESCONDENSA.
       GRAU1-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(01: 11)
           MOVE "("               TO LINDET-REL(12: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(13: 03)
           MOVE ")"               TO LINDET-REL(16: 02).
           MOVE DESCRICAO-CX20    TO LINDET-REL(18: 30).
       GRAU2-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(05: 11)
           MOVE "("               TO LINDET-REL(16: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(17: 03)
           MOVE ")"               TO LINDET-REL(20: 02).
           MOVE DESCRICAO-CX20    TO LINDET-REL(22: 30).
       GRAU3-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(10: 11)
           MOVE "("               TO LINDET-REL(21: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(22: 03)
           MOVE ")"               TO LINDET-REL(25: 02).
           MOVE DESCRICAO-CX20    TO LINDET-REL(27: 30).
       GRAU4-IMP SECTION.
           MOVE CONTA-E           TO LINDET-REL(15: 11)
           MOVE "("               TO LINDET-REL(26: 01)
           MOVE CONTA-REDUZ-WK    TO LINDET-REL(27: 03)
           MOVE ")"               TO LINDET-REL(30: 02).
           MOVE DESCRICAO-CX20    TO LINDET-REL(32: 30).
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
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CPD020 CXD020 CXD100 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
