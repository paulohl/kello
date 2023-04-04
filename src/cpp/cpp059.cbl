       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP059.
      *DATA: 09/03/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Atualização automática de cheques pre-datados
      *FUNÇÃO: Serão atualizados todos os titulos do contas a pagar
      *        com o tipo de portador 12 (cheque pre-datado) e data de
      *        vencto <= data-dia.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX001.
           COPY CPPX020.
           COPY CXPX100.
           COPY CBPX100.
      *    COPY CIEPX010.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = FORNEC-WK SEQ-WK.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW001.
       COPY CPPW020.
       COPY CXPW100.
       COPY CBPW100.
      *COPY CIEPW010.
       FD  WORK.
       01  REG-WORK.
           05  FORNEC-WK              PIC 9(6).
           05  SEQ-WK                 PIC 9(5).
           05  COD-BANCO-WK           PIC 9(6).
           05  NR-CHEQUE-WK           PIC 9(6).

       WORKING-STORAGE SECTION.
           COPY "CPP059.CPB".
           COPY "CPP059.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VENCTOW.
               10  ANO-VENC          PIC 9(4).
               10  MES-VENC          PIC 9(2).
               10  DIA-VENC          PIC 9(2).
           05  VENCTO-W REDEFINES VENCTOW PIC 9(8).
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
           05  JUROS-W               PIC 9(8)V99  VALUE ZEROS.
           05  ATUALIZAR-SIM-NAO     PIC X        VALUE SPACES.
           05  SENHA-WW              PIC 9(4)     COMP-3.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.

           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CBD100.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           OPEN INPUT CGD001 CBD100 CAD004.
           OPEN I-O CPD020 CXD100.
      *    OPEN I-O CIED010.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CIED010 <> "00"
      *       MOVE "ERRO ABERTURA CIED010: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CIED010 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-ATUALIZA-TRUE
                    PERFORM ATUALIZA-CHEQUEPRE
               WHEN GS-VERIFICA-SENHA-TRUE
                    PERFORM VERIFICA-SENHA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICA-SENHA SECTION.
           MOVE "SENHA06" TO PROGRAMA-CA004.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           READ CAD004 INVALID KEY MOVE 1 TO GS-SENHA-INVALIDA
              NOT INVALID KEY MOVE 0 TO GS-SENHA-INVALIDA.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           MOVE "Gravando..." TO GS-EXIBE-LEITURA.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE ZEROS TO DATA-VENCTO-CP20 FORNEC-CP20 SITUACAO-CP20.
           START CPD020 KEY IS NOT < ALT2-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
             READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
               NOT AT END
                MOVE FORNEC-CP20  TO GS-EXIBE-LEITURA(14: 6)
                MOVE "EXIBE-LEITURA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                IF SITUACAO-CP20 NOT = 0 OR
                   DATA-VENCTO-CP20 > DATA-DIA-INV
                       MOVE "10" TO ST-CPD020
                ELSE
                 IF PORTADOR-CP20 NOT = 12 CONTINUE
                 ELSE
                   PERFORM VERIFICA-CONT-BANCARIO
                   IF ATUALIZAR-SIM-NAO = "S"
                      MOVE FORNEC-CP20     TO FORNEC-WK
                      MOVE SEQ-CP20        TO SEQ-WK
                      MOVE NR-CHEQUE-CB100 TO NR-CHEQUE-WK
                      MOVE CODIGO-FORN-CB100 TO COD-BANCO-WK
                      WRITE REG-WORK
                      END-WRITE
                   ELSE CONTINUE
                   END-IF
                 END-IF
                END-IF
             END-READ
           END-PERFORM.
       VERIFICA-CONT-BANCARIO SECTION.
           MOVE "N" TO ATUALIZAR-SIM-NAO.
           MOVE SEQ-CP20             TO SEQ-CTA-PAGAR-CB100
           MOVE FORNEC-CP20          TO NOMINAL-A-CB100
           START CBD100 KEY IS = ALT2-CB100 INVALID KEY
      *          MOVE FORNEC-WK TO GS-CODIGO
      *          MOVE "ERRO-ATUALIZACAO" TO DS-PROCEDURE
      *          PERFORM CALL-DIALOG-SYSTEM
                 CONTINUE
             NOT INVALID KEY
                READ CBD100 NEXT RECORD
                END-READ
                IF SITUACAO-CB100 NOT = 03
      *            MOVE FORNEC-WK TO GS-CODIGO
      *            MOVE "ERRO-ATUALIZACAO" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
                   CONTINUE
                ELSE
                  MOVE "S" TO ATUALIZAR-SIM-NAO
                END-IF
           END-START.

       ATUALIZA-CHEQUEPRE SECTION.
           PERFORM GRAVA-WORK.
           CLOSE WORK.  OPEN INPUT WORK.
           MOVE "Atualizando" TO GS-EXIBE-LEITURA.
           PERFORM ACHA-SEQ-CAIXA.
           MOVE ZEROS TO FORNEC-WK SEQ-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                    MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                MOVE FORNEC-WK   TO GS-EXIBE-LEITURA(14: 6)
                MOVE "EXIBE-LEITURA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE FORNEC-WK   TO FORNEC-CP20
                MOVE SEQ-WK      TO SEQ-CP20
                READ CPD020 INVALID KEY CONTINUE
                NOT INVALID KEY
                  PERFORM GRAVA-CAIXA
                  MOVE SEQ-CX100          TO SEQ-CAIXA-CP20
                  MOVE 2                  TO SITUACAO-CP20
      *           SITUACAO = 2 (PAGO)
                  MOVE DATA-DIA-INV       TO DATA-PGTO-CP20
                  MOVE VALOR-TOT-CP20     TO VALOR-LIQ-CP20
                  REWRITE REG-CPD020
                  END-REWRITE
      *           PERFORM GRAVA-CIE
             END-READ
           END-PERFORM.
           MOVE 1 TO GS-EXIT-FLG.
           GO FINALIZAR-PROGRAMA.
       GRAVA-CAIXA SECTION.
      *    CONTA DOBRADA
           MOVE DATA-DIA-INV         TO DATA-MOV-CX100.
           MOVE DESCRICAO-CP20       TO HISTORICO-CX100.
           MOVE NR-CHEQUE-WK         TO DOCUMENTO-CX100.
           MOVE VALOR-TOT-CP20       TO VALOR-CX100
           MOVE ZEROS                TO CONTABIL-CX100 SEQ-DESM-CX100.
           MOVE COD-BANCO-WK         TO CONTAPART-CX100.
           MOVE 023                  TO CONTA-REDUZ-CX100.
           MOVE 50                   TO TIPO-LCTO-CX100.
           ADD 1 TO SEQ-CAIXA.
           MOVE ZEROS TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             MOVE SEQ-CAIXA TO SEQ-CX100
             WRITE REG-CXD100 INVALID KEY
                   ADD 1 TO SEQ-CAIXA
             NOT INVALID KEY MOVE "10" TO ST-CXD100
           END-PERFORM.

           MOVE FORNEC-CP20          TO CONTAPART-CX100.
           MOVE CODREDUZ-APUR-CP20   TO CONTA-REDUZ-CX100.
           MOVE 02                   TO TIPO-LCTO-CX100.
           ADD 1 TO SEQ-CAIXA.
           MOVE ZEROS TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             MOVE SEQ-CAIXA TO SEQ-CX100
             WRITE REG-CXD100 INVALID KEY
                   ADD 1 TO SEQ-CAIXA
             NOT INVALID KEY MOVE "10" TO ST-CXD100
           END-PERFORM.

       ACHA-SEQ-CAIXA SECTION.
           MOVE DATA-DIA-INV         TO DATA-MOV-CX100.
           MOVE ZEROS TO SEQ-CX100 ST-CXD100 SEQ-CAIXA.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                NOT AT END
                   IF DATA-MOV-CX100 NOT = DATA-DIA-INV
                        MOVE "10" TO ST-CXD100
                   ELSE MOVE SEQ-CX100 TO SEQ-CAIXA
             END-READ
           END-PERFORM.

      *GRAVA-CIE SECTION.
      *    PERFORM ACHA-SEQ-CIE.
      *    MOVE SPACES TO DESCRICAO-MENS-CI10.
      *    MOVE 02               TO COD-MENS-PADRAO-CI10
      *    MOVE 12               TO FUNCAO-DESTINO-CI10
      *    MOVE FORNEC-CP20      TO CODIGO-CG01
      *    READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
      *    MOVE NOME-CG01        TO DESCRICAO-MENS-CI10(01: 31)
      *    MOVE DATA-VENCTO-CP20 TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV         TO DATA-E
      *    MOVE DATA-E           TO DESCRICAO-MENS-CI10(32: 11)
      *    MOVE VALOR-TOT-CP20   TO VALOR-E
      *    MOVE VALOR-E          TO DESCRICAO-MENS-CI10(43: 14)
      *    MOVE DATA-DIA-INV     TO DATA-CI10
      *    ADD 1                 TO SEQ-CI10
      *    ACCEPT HORA-W       FROM TIME.
      *    MOVE HORA-W(1: 4)     TO HORA-CI10
      *    MOVE STRING-1(1: 5)   TO ORIGEM-CI10
      *    MOVE ZEROS          TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.

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


       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP059" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CPD020 CXD100 CGD001 CBD100 WORK CAD004.
      *    CLOSE CIED010.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

