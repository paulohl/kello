       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP152.
      *DATA: 11/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Relatório de Apuração de Resultados TRIMESTRAL
      *FUNÇÃO: Relaciona todo o movimento dentro de um intervalo de
      *        3 MESES. A conta a ser considerada 100%
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

       DATA DIVISION.
       FILE SECTION.
       COPY CXPW020.
       COPY CXPW042.
       COPY CXPW100.
       FD  WORK.
       01  REG-WORK.
           05  CONTA-WK        PIC 9(7).
           05  CONTA-REDUZ-WK  PIC 9(5).
           05  GRAU-WK         PIC 9.
           05  VALOR-WK OCCURS 3 TIMES PIC S9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP152.CPB".
           COPY "CXP152.CPY".
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANO-TAB OCCURS 3 TIMES PIC 9(6).
           05  MESANOWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 9(2).
           05  MESANO-WI REDEFINES MESANOWI PIC 9(6).
           05  MESANOWW.
               10  MES-WW            PIC 9(2).
               10  ANO-WW            PIC 9(4).
           05  MESANO-WW REDEFINES MESANOWW PIC 9(6).
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  CONTA-E               PIC 9.99.99.99.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  MESANO-CORRENTE       PIC 9(6)     VALUE ZEROS.
      *  MES/ANO CORRENTE - LERÁ O SALDO A PARTIR DO ARQUIVO CXD100
      *      ENQUANTO OS DEMAIS DO CXD040(ARQUIVO DE SALDO)
           05  SALDO-FINAL OCCURS 3 TIMES PIC S9(8)V99.
           05  TOTAL-PERC OCCURS 3 TIMES PIC S9(8)V99.
      *   Total-perc - conta 100% mensal, p/ calculo percentagem
           05  TOTAL-PERC-TRIM       PIC S9(8)V99  VALUE ZEROS.
      *   Total da conta 100% trimestral, p/ calculo percentagem
           05  ACUM-TRIMESTRE        PIC S9(8)V99  VALUE ZEROS.
           05  TOT-ACUM-TRIMESTRE    PIC S9(8)V99  VALUE ZEROS.
           05  POS1                  PIC 9(3)     VALUE ZEROS.
           05  POS2                  PIC 9(3)     VALUE ZEROS.
      * pos1 e pos2 p/ alinhar as colunas dos valores trimestrais
           05  PERC-W                PIC 999V99   VALUE ZEROS.
           05  PERC-E                PIC ZZZ,ZZ.
           05  VALOR-SAI             PIC 9(8)V99  VALUE ZEROS.
           05  VALOR-ENT             PIC 9(8)V99  VALUE ZEROS.
           05  I                     PIC 9        VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(125)  VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(79)   VALUE
           "APURACAO DE RESULTADOS - TRIMESTRAL".
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(04)   VALUE "HR: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "DATA EMISSAO: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.

       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(45)   VALUE
           "DESCRICAO DAS CONTAS".
           05  FILLER              PIC X(22)   VALUE
           "        VALOR       % ".
           05  FILLER              PIC X(22)   VALUE
           "        VALOR       % ".
           05  FILLER              PIC X(22)   VALUE
           "        VALOR       % ".
           05  FILLER              PIC X(21)   VALUE
           "        VALOR       %".
       01  CAB05.
           05  FILLER              PIC X(45)   VALUE SPACES.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  MESANO1-REL         PIC 99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  MESANO2-REL         PIC 99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  MESANO3-REL         PIC 99/9999 VALUE ZEROS.
           05  FILLER              PIC X(8)    VALUE SPACES.
           05  FILLER              PIC X(21)   VALUE
           "       TRIMESTRE     ".

       01  LINDET.
           05  CONTA-REL           PIC 9.99.99.99 VALUE ZEROS.
           05  FILLER              PIC X       VALUE "(".
           05  CONTA-REDUZ-REL     PIC 999     VALUE ZEROS.
           05  FILLER              PIC XX      VALUE ")-".
           05  DESCRICAO-REL       PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR1-REL          PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC1-REL           PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR2-REL          PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC2-REL           PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR3-REL          PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC3-REL           PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR4-REL          PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC4-REL           PIC ZZZ,ZZ.
       01  LINTOT.
           05  DESCRICAO-RELT      PIC X(44)   VALUE SPACES.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR1-RELT         PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC1-RELT          PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR2-RELT         PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC2-RELT          PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR3-RELT         PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC3-RELT          PIC ZZZ,ZZ.
           05  FILLER              PIC X       VALUE SPACES.
           05  VALOR4-RELT         PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X       VALUE SPACES.
           05  PERC4-RELT          PIC ZZZ,ZZ.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP152-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE DATA-INV(5: 4) TO MESANO-CORRENTE(1: 4).
           MOVE DATA-INV(3: 2) TO MESANO-CORRENTE(5: 2).
           MOVE MESANO-CORRENTE TO MESANO-TAB(3) MESANO-WI.
           IF MES-WI = 01 MOVE 12 TO MES-WI
                          SUBTRACT 1 FROM ANO-WI
           ELSE SUBTRACT 1 FROM MES-WI.
           MOVE MESANO-WI       TO MESANO-TAB(2).
           IF MES-WI = 01 MOVE 12 TO MES-WI
                          SUBTRACT 1 FROM ANO-WI
           ELSE SUBTRACT 1 FROM MES-WI.
           MOVE MESANO-WI       TO MESANO-TAB(1).
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP152-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP152-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP152-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD042"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD042.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           OPEN INPUT CXD100 CXD042 CXD020.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP152-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP152-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD042 <> "00"
              MOVE "ERRO ABERTURA CXD042: "  TO CXP152-MENSAGEM-ERRO
              MOVE ST-CXD042 TO CXP152-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP152-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP152-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP152-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP152-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CXP152-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP152-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CXP152-OPCAO-RETORNO-TRUE
                    PERFORM RETORNA-MES
               WHEN CXP152-OPCAO-AVANCAR-TRUE
                    PERFORM AVANCA-MES
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
           INITIALIZE CXP152-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXIBE-MESANO SECTION.
           MOVE MESANO-TAB(1) TO MESANO-WI.
           MOVE MES-WI TO MES-WW
           MOVE ANO-WI TO ANO-WW.
           MOVE MESANO-WW TO CXP152-MESANO-1 MESANO1-REL.
           MOVE MESANO-TAB(2) TO MESANO-WI.
           MOVE MES-WI TO MES-WW
           MOVE ANO-WI TO ANO-WW.
           MOVE MESANO-WW TO CXP152-MESANO-2 MESANO2-REL.
           MOVE MESANO-TAB(3) TO MESANO-WI.
           MOVE MES-WI TO MES-WW
           MOVE ANO-WI TO ANO-WW.
           MOVE MESANO-WW TO CXP152-MESANO-3 MESANO3-REL.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       AVANCA-MES SECTION.
           IF MESANO-TAB(3) = MESANO-CORRENTE CONTINUE
           ELSE MOVE MESANO-TAB(2) TO MESANO-TAB(1)
                MOVE MESANO-TAB(3) TO MESANO-TAB(2) MESANO-WI
                ADD 1 TO MES-WI
                IF MES-WI > 12 MOVE 01 TO MES-WI  ADD 1 TO ANO-WI
                END-IF
                MOVE MESANO-WI     TO MESANO-TAB(3)
                PERFORM CARREGA-LISTA.
       RETORNA-MES SECTION.
           MOVE MESANO-TAB(2) TO MESANO-TAB(3).
           MOVE MESANO-TAB(1) TO MESANO-TAB(2) MESANO-WI.
           SUBTRACT 1 FROM MES-WI.
           IF MES-WI = ZEROS MOVE 12 TO MES-WI SUBTRACT 1 FROM ANO-WI.
           MOVE MESANO-WI     TO MESANO-TAB(1).
           PERFORM CARREGA-LISTA.
       INICIO-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK
      *    Grava todas as contas existentes no cxd020
           MOVE 1          TO CONTA-WK.
           MOVE 99999      TO CONTA-REDUZ-WK.
           MOVE 1          TO GRAU-WK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE ZEROS TO VALOR-WK(I)
           END-PERFORM.
           WRITE REG-WORK.
      *    Conta p/ gravar valores de contas não cadastradas
           MOVE "GRAVANDO..." TO CXP152-MENSAGEM-AGUARDA.

           MOVE ZEROS      TO CODIGO-REDUZ-CX20.
           START CXD020 KEY IS NOT < CODIGO-REDUZ-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
             READ CXD020 NEXT RECORD AT END MOVE "10" TO ST-CXD020
               NOT AT END
                 MOVE CODIGO-REDUZ-CX20     TO
                            CXP152-MENSAGEM-AGUARDA(12: 25)
                 MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM

                 MOVE CODIGO-REDUZ-CX20     TO CONTA-REDUZ-WK
                 MOVE CODIGO-COMPL-CX20     TO CONTA-WK
                 MOVE GRAU-CX20             TO GRAU-WK
                 MOVE ZEROS TO VALOR-WK(1) VALOR-WK(2) VALOR-WK(3)
                 WRITE REG-WORK
             END-READ
           END-PERFORM.
           MOVE MESANO-TAB(1) TO ANOMES-CX42.
           MOVE ZEROS TO SALDO-FINAL(1) SALDO-FINAL(2) SALDO-FINAL(3).
           MOVE ZEROS         TO CONTAREDUZ-CX42.
           IF MESANO-TAB(3) = MESANO-CORRENTE
              MOVE MESANO-TAB(2) TO MESANO-FIM
           ELSE MOVE MESANO-TAB(3) TO MESANO-FIM.
           START CXD042 KEY IS NOT < CHAVE-CX42 INVALID KEY
                 MOVE "10" TO ST-CXD042.
           PERFORM UNTIL ST-CXD042 = "10"
             READ CXD042 NEXT RECORD AT END MOVE "10" TO ST-CXD042
               NOT AT END
                 IF ANOMES-CX42 > MESANO-FIM MOVE "10" TO ST-CXD042
                 ELSE
                      PERFORM LE-CODIGOCOMPL-42
                      MOVE CODIGO-COMPL-CX20 TO CONTA-WK
                      READ WORK
                      END-READ
                      MOVE CONTA-REDUZ-WK     TO
                            CXP152-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE SALDOE-CX42       TO VALOR-ENT
                      MOVE SALDOS-CX42       TO VALOR-SAI
                      PERFORM VERIFICA-PONTEIRO
                      ADD VALOR-ENT TO VALOR-WK(I) SALDO-FINAL(I)
                      SUBTRACT VALOR-SAI FROM VALOR-WK(I) SALDO-FINAL(I)
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA
                 END-IF
             END-READ
           END-PERFORM.
           MOVE 3  TO I.
           IF MESANO-CORRENTE = MESANO-TAB(3)
              MOVE MESANO-CORRENTE TO DATA-MOV-CX100(1: 6)
              MOVE 01              TO DATA-MOV-CX100(7: 2)
              MOVE ZEROS           TO SEQ-CX100
              START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100
           ELSE MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                 MOVE DATA-MOV-CX100 TO MESANO-I(1: 6)
                 IF MESANO-I > MESANO-CORRENTE
                               MOVE "10" TO ST-CXD100
                 ELSE
                   IF CONTA-REDUZ-CX100 = 888 CONTINUE
                   ELSE
                      MOVE ZEROS TO VALOR-SAI VALOR-ENT
                      IF TIPO-LCTO-CX100 < 50
                             MOVE VALOR-CX100 TO VALOR-SAI
                      ELSE MOVE VALOR-CX100 TO VALOR-ENT
                      END-IF
                      MOVE CONTA-REDUZ-CX100 TO CODIGO-REDUZ-CX20
                      READ CXD020 INVALID KEY
                           MOVE 1 TO CODIGO-COMPL-CX20
                      END-READ
                      MOVE CODIGO-COMPL-CX20 TO CONTA-WK
                      READ WORK
                      END-READ
                      MOVE CONTA-REDUZ-WK     TO
                            CXP152-MENSAGEM-AGUARDA(12: 25)
                      MOVE "TELA-AGUARDA" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      ADD VALOR-ENT       TO VALOR-WK(3) SALDO-FINAL(3)
                      SUBTRACT VALOR-SAI FROM VALOR-WK(3) SALDO-FINAL(3)
                      REWRITE REG-WORK
                      END-REWRITE
                      PERFORM TOTALIZA
                   END-IF
             END-READ
           END-PERFORM.
       VERIFICA-PONTEIRO SECTION.
           IF ANOMES-CX42 = MESANO-TAB(1) MOVE 1 TO I.
           IF ANOMES-CX42 = MESANO-TAB(2) MOVE 2 TO I.
           IF ANOMES-CX42 = MESANO-TAB(3) MOVE 3 TO I.
       LE-CODIGOCOMPL-42 SECTION.
           MOVE CONTAREDUZ-CX42 TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY MOVE 1 TO CODIGO-COMPL-CX20
                                   MOVE 99999 TO CODIGO-REDUZ-CX20.
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
             NOT INVALID KEY
                 ADD VALOR-ENT       TO VALOR-WK(I)
                 SUBTRACT VALOR-SAI  FROM VALOR-WK(I)
                 REWRITE REG-WORK
           END-READ.
       CONTA-PERCENTAGEM SECTION.
           MOVE "UNSHOW-TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO TOTAL-PERC-TRIM.
           MOVE 1010000 TO CONTA-WK.
           READ WORK INVALID KEY
              MOVE ZEROS TO TOTAL-PERC(1) TOTAL-PERC(2) TOTAL-PERC(3)
                            TOTAL-PERC-TRIM
            NOT INVALID KEY
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               MOVE VALOR-WK(I) TO TOTAL-PERC(I)
               ADD VALOR-WK(I)  TO TOTAL-PERC-TRIM
             END-PERFORM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO CXP152-LINDET.
           PERFORM INICIO-WORK.
           PERFORM EXIBE-MESANO.
           PERFORM CONTA-PERCENTAGEM.
           MOVE ZEROS          TO CONTA-WK.
           MOVE ZEROS          TO TOT-ACUM-TRIMESTRE.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-CXD100
              NOT AT END
                    MOVE SPACES            TO CXP152-LINDET
                    MOVE CONTA-WK       TO CONTA-E
                    MOVE CONTA-REDUZ-WK TO CODIGO-REDUZ-CX20
                    READ CXD020 INVALID KEY
                           MOVE SPACES TO DESCRICAO-CX20
                           MOVE ZEROS  TO TIPO-CONTA-CX20
                    END-READ
                    IF TIPO-CONTA-CX20 = 1 PERFORM INSERE-LINHA-BRANCO
                    END-IF
                    MOVE CONTA-E           TO CXP152-LINDET(01: 10)
                    MOVE "("               TO CXP152-LINDET(12: 01)
                    MOVE CONTA-REDUZ-WK    TO CXP152-LINDET(13: 05)
                    MOVE ")"               TO CXP152-LINDET(18: 01)
                    MOVE DESCRICAO-CX20    TO CXP152-LINDET(19: 30)
                    MOVE ZEROS             TO ACUM-TRIMESTRE
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                      MOVE VALOR-WK(I) TO VALOR-E
                      ADD VALOR-WK(I)  TO ACUM-TRIMESTRE
                      COMPUTE POS1 = (I * 25) + 25
                      COMPUTE POS2 = (I * 25) + 40
                      MOVE VALOR-E         TO CXP152-LINDET(POS1: 16)
                      COMPUTE PERC-W = (VALOR-WK(I)/TOTAL-PERC(I))*100
                      MOVE PERC-W          TO PERC-E
                      MOVE PERC-E          TO CXP152-LINDET(POS2: 09)
                    END-PERFORM
                    MOVE ACUM-TRIMESTRE    TO VALOR-E
                    MOVE VALOR-E           TO CXP152-LINDET(125: 16)
                    ADD ACUM-TRIMESTRE     TO TOT-ACUM-TRIMESTRE
                    COMPUTE PERC-W =
                          (ACUM-TRIMESTRE / TOTAL-PERC-TRIM) * 100
                    MOVE PERC-W            TO PERC-E
                    MOVE PERC-E            TO CXP152-LINDET(140: 09)
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE SPACES TO CXP152-LINDET.
           PERFORM INSERE-LINHA-BRANCO.
           MOVE "TOTAL GERAL..." TO CXP152-LINDET(01: 14).
           MOVE ZEROS             TO TOT-ACUM-TRIMESTRE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE SALDO-FINAL(I) TO VALOR-E
             ADD SALDO-FINAL(I)  TO TOT-ACUM-TRIMESTRE
             COMPUTE POS1 = (I * 25) + 25
             COMPUTE POS2 = (I * 25) + 40
             MOVE VALOR-E         TO CXP152-LINDET(POS1: 16)
             COMPUTE PERC-W = (SALDO-FINAL(I) / TOTAL-PERC(I)) * 100
             MOVE PERC-W          TO PERC-E
             MOVE PERC-E          TO CXP152-LINDET(POS2: 09)
           END-PERFORM
           MOVE TOT-ACUM-TRIMESTRE  TO VALOR-E
           MOVE VALOR-E             TO CXP152-LINDET(125: 16)
           COMPUTE PERC-W = (TOT-ACUM-TRIMESTRE/TOTAL-PERC-TRIM)*100
           MOVE PERC-W            TO PERC-E
           MOVE PERC-E            TO CXP152-LINDET(140: 09)
           MOVE "INSERE-LIST"    TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       INSERE-LINHA-BRANCO SECTION.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP152-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "CXP152"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE ZEROS             TO CONTA-WK.
           START WORK KEY IS NOT < CONTA-WK  INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-CXD100
              NOT AT END
                    MOVE CONTA-WK          TO CONTA-REL
                    MOVE CONTA-REDUZ-WK    TO CODIGO-REDUZ-CX20
                                              CONTA-REDUZ-REL
                    READ CXD020 INVALID KEY
                           MOVE SPACES TO DESCRICAO-CX20
                           MOVE ZEROS  TO TIPO-CONTA-CX20
                    END-READ
                    IF TIPO-CONTA-CX20 = 1 PERFORM LINHA-BRANCO-IMP
                    END-IF
                    MOVE DESCRICAO-CX20    TO DESCRICAO-REL
                    MOVE VALOR-WK(1)       TO VALOR1-REL
                    COMPUTE PERC-W = (VALOR-WK(1)/TOTAL-PERC(1))*100
                    MOVE PERC-W            TO PERC1-REL
                    MOVE VALOR-WK(2)       TO VALOR2-REL
                    COMPUTE PERC-W = (VALOR-WK(2)/TOTAL-PERC(2))*100
                    MOVE PERC-W            TO PERC2-REL
                    MOVE VALOR-WK(3)       TO VALOR3-REL
                    COMPUTE PERC-W = (VALOR-WK(3)/TOTAL-PERC(3))*100
                    MOVE PERC-W            TO PERC3-REL
                    COMPUTE ACUM-TRIMESTRE = VALOR-WK(1)+ VALOR-WK(2)+
                                       VALOR-WK(3)
                    MOVE ACUM-TRIMESTRE    TO VALOR4-REL
                    COMPUTE PERC-W =
                       (ACUM-TRIMESTRE / TOTAL-PERC-TRIM) * 100
                    MOVE PERC-W            TO PERC4-REL
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO CXP152-LINDET.
           PERFORM LINHA-BRANCO-IMP.
           MOVE "TOTAL GERAL..." TO DESCRICAO-RELT.
           MOVE SALDO-FINAL(1)   TO VALOR1-RELT
           COMPUTE PERC-W = (SALDO-FINAL(1) / TOTAL-PERC(1)) * 100
           MOVE PERC-W           TO PERC1-RELT
           MOVE SALDO-FINAL(2)   TO VALOR2-RELT
           COMPUTE PERC-W = (SALDO-FINAL(2) / TOTAL-PERC(2)) * 100
           MOVE PERC-W           TO PERC2-RELT
           MOVE SALDO-FINAL(2)   TO VALOR3-RELT
           COMPUTE PERC-W = (SALDO-FINAL(3) / TOTAL-PERC(3)) * 100
           MOVE PERC-W           TO PERC3-RELT
           COMPUTE TOT-ACUM-TRIMESTRE = SALDO-FINAL(1) + SALDO-FINAL(2)
                                        + SALDO-FINAL(3).
           MOVE TOT-ACUM-TRIMESTRE  TO VALOR4-RELT
           COMPUTE PERC-W = (TOT-ACUM-TRIMESTRE/TOTAL-PERC-TRIM)*100
           MOVE PERC-W            TO PERC4-RELT
           WRITE REG-RELAT FROM LINDET AFTER 2.

           COPY DESCONDENSA.
       LINHA-BRANCO-IMP SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP152-DATA-BLOCK.
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

