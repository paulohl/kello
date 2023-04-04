       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP105.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/10/1998
      *DESCRIÇÃO: Emissão de cheque - avista ou pré-datado
      * A emissão irá atualizar o controle de cheques(talão)
      * Os cheque pre-datados deverão estar no contas a pagar e o valor
      * deverá ser o mesmo, caso contrario, não emite.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX001.
           COPY CBPX002.
           COPY CBPX100.
      *    COPY CPPX020.
           COPY CGPX001.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW001.
       COPY CBPW002.
       COPY CBPW100.
      *COPY CPPW020.
       COPY CGPW001.
       FD  RELAT       LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP105.CPB".
           COPY "CBP105.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPADAY1".
           COPY "CPDIAS1".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD002             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
      *    05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  STRING-BANCO1.
               10  VALOR-BANC        PIC 9(8)V99.
               10  NOME-BANC         PIC X(30).
           05  STRING-BANCO REDEFINES STRING-BANCO1 PIC X(40).
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - VARIAVEL P/ VERIFICAR SE HOUVE ALGUM ERRO ABERTURA
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CBP105-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.

           MOVE DATA-INV    TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE DATA-INV    TO DATA-DIA-INV.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CBP105-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CBP105-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CBP105-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
      *    MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CBD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD002.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.

           OPEN I-O CBD100.
           OPEN INPUT CGD001 CBD001 CBD002.
      *    OPEN INPUT CPD020.
           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O CBD100
           END-IF.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CBP105-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CBP105-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD002 <> "00"
              MOVE "ERRO ABERTURA CBD002: "  TO CBP105-MENSAGEM-ERRO
              MOVE ST-CBD002 TO CBP105-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO CBP105-MENSAGEM-ERRO
              MOVE ST-CBD100 TO CBP105-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CBP105-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CBP105-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CPD020 <> "00"
      *       MOVE "ERRO ABERTURA CPD020: "  TO CBP105-MENSAGEM-ERRO
      *       MOVE ST-CPD020 TO CBP105-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CBP105-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CBP105-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CBP105-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN CBP105-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-BANCO
               WHEN CBP105-POPUP-FORNEC-TRUE
                    PERFORM CHAMAR-POPUP-FORNEC
               WHEN CBP105-IMPRIMIR-CHEQUE-TRUE
                    PERFORM IMPRIMIR-CHEQUE
               WHEN CBP105-GRAVAR-CHEQUE-TRUE
                    PERFORM GRAVA-CHEQUE
               WHEN CBP105-VERIF-NR-CHEQUE-TRUE
                    PERFORM VERIF-NR-CHEQUE
               WHEN CBP105-LER-BANCO-TRUE
                    PERFORM LER-BANCO
               WHEN CBP105-VALIDA-CH-TRUE
                    PERFORM VALIDA-CH
               WHEN CBP105-LER-FORNEC-TRUE
                    PERFORM LER-FORNEC
      *        WHEN CBP105-VERIF-APAGAR-TRUE
      *             PERFORM VERIF-APAGAR
               WHEN CBP105-VERIF-PROX-CHEQUE-TRUE
                    PERFORM PROX-NR-CHEQUE
      *        WHEN CBP105-VERIF-SEL-APAGAR-TRUE
      *             PERFORM VERIF-SELECAO-APAGAR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDA-CH SECTION.
           MOVE CBP105-BANCO-CH     TO CODIGO-FORN-CB02
           MOVE CBP105-NR-CHEQUE-CH TO FL-INICIAL-CB02
           READ CBD002 INVALID KEY
               MOVE "Número da Folha do Talão de cheque não Cadastrada"
               TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

       EXIBIR-MENSAGEM SECTION.
           MOVE 1      TO CBP105-FLAG-CRITICA
           MOVE SPACES TO RESP-MSG
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".

       LER-BANCO SECTION.
           MOVE CBP105-BANCO-CH TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY MOVE SPACES TO TITULAR-CB01.
           MOVE NOME-BANCO-CB01 TO CBP105-NOME-BANCO-CH.
       LER-FORNEC SECTION.
           MOVE CBP105-NOMINAL-A-CH TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01 TO CBP105-NOME-NOMINAL-A-CH.
       VERIF-NR-CHEQUE SECTION.
           MOVE CBP105-BANCO-CH           TO CODIGO-FORN-CB100.
           MOVE CBP105-NR-CHEQUE-CH       TO NR-CHEQUE-CB100.
           READ CBD100 INVALID KEY MOVE 1 TO CBP105-ACHOU-NR-CH
                NOT INVALID KEY
                   IF SITUACAO-CB100 NOT = 1
                      MOVE 1 TO CBP105-ACHOU-NR-CH
                   ELSE MOVE 0 TO CBP105-ACHOU-NR-CH.
       GRAVA-CHEQUE SECTION.
           MOVE CBP105-BANCO-CH           TO CODIGO-FORN-CB100.
           MOVE CBP105-NR-CHEQUE-CH       TO NR-CHEQUE-CB100.
           MOVE CBP105-NOMINAL-A-CH       TO NOMINAL-A-CB100.
           MOVE CBP105-VALOR              TO VALOR-CB100.
           MOVE DATA-MOVTO-W              TO DATA-EMISSAO-CB100.
           MOVE CBP105-VENCTO             TO DATA-VENCTO-CB100.
      *    situacao = 2 (cheque a vista)    situacao= 3 (cheq.a prazo)
           MOVE CBP105-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           IF DATA-INV NOT > DATA-DIA-INV
              MOVE 2                      TO SITUACAO-CB100
           ELSE MOVE 3                    TO SITUACAO-CB100.
           REWRITE REG-CBD100.
       IMPRIMIR-CHEQUE SECTION.
           MOVE CBP105-NOMINAL-A-CH      TO VALOR-BANC
           MOVE CBP105-NOME-NOMINAL-A-CH TO NOME-BANC
           MOVE CBP105-VALOR             TO VALOR-BANC.
           CALL   "CBP200" USING PARAMETROS-W STRING-BANCO.
           CANCEL "CBP200".
      *VERIF-APAGAR SECTION.
      *    MOVE CBP105-VENCTO TO DATA-INV.
      *    CALL "GRIDAT2" USING DATA-INV.
      *    IF DATA-INV NOT > DATA-DIA-INV CONTINUE
      *    ELSE PERFORM LER-A-PAGAR.
      *VERIF-SELECAO-APAGAR SECTION.
      *    MOVE CBP105-LINDET(75: 5) TO CBP105-SEQ-SEL-APAGAR SEQ-CP20.
      *    MOVE CBP105-NOMINAL-A-CH  TO FORNEC-CP20.
      *    READ CPD020 INVALID KEY MOVE ZEROS TO VALOR-TOT-CP20.
      *    IF VALOR-TOT-CP20 NOT = CBP105-VALOR
      *       MOVE 1 TO CBP105-VERIF-VALOR
      *    ELSE MOVE 0 TO CBP105-VERIF-VALOR.
       CHAMAR-POPUP-BANCO SECTION.
           CALL   "CBP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING(49: 6) TO CBP105-BANCO-CH.
           PERFORM LER-BANCO.
       CHAMAR-POPUP-FORNEC SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING(33: 6) TO CBP105-NOMINAL-A-CH.
           MOVE PASSAR-STRING(1: 30) TO CBP105-NOME-NOMINAL-A-CH.
       PROX-NR-CHEQUE SECTION.
           MOVE CBP105-BANCO-CH       TO CODIGO-FORN-CB100.
           MOVE ZEROS                 TO NR-CHEQUE-CB100 SITUACAO-CB100.
           START CBD100 KEY IS NOT < ALT1-CB100 INVALID KEY
                 MOVE "10" TO ST-CBD100.
           PERFORM UNTIL ST-CBD100 = "10"
             READ CBD100 NEXT RECORD AT END MOVE "10" TO ST-CBD100
              NOT AT END
                IF CODIGO-FORN-CB100 NOT = CBP105-BANCO-CH
                   MOVE ZEROS TO CBP105-NR-CHEQUE-CH
                ELSE MOVE NR-CHEQUE-CB100 TO CBP105-NR-CHEQUE-CH
                END-IF
                MOVE "10" TO ST-CBD100
             END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CBD100
           INITIALIZE CBP105-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CBP105-MENSAGEM-ERRO
           MOVE ST-CBD100       TO CBP105-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *LER-A-PAGAR SECTION.
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *    MOVE CBP105-NOMINAL-A-CH TO FORNEC-CP20.
      *    MOVE ZEROS TO SITUACAO-CP20 DATA-VENCTO-CP20 ST-CPD020.
      *    START CPD020 KEY IS NOT < ALT4-CP20 INVALID KEY
      *          MOVE "10" TO ST-CPD020.
      *    MOVE ZEROS TO CBP105-CONT.
      *    MOVE ZEROS TO CBP105-LINDET(75: 5).
      *    PERFORM UNTIL ST-CPD020 = "10"
      *       READ CPD020 NEXT RECORD AT END MOVE "10" TO ST-CPD020
      *        NOT AT END
      *          IF SITUACAO-CP20 > 0 MOVE "10" TO ST-CPD020
      *          ELSE IF FORNEC-CP20 NOT = CBP105-NOMINAL-A-CH
      *                  MOVE "10" TO ST-CPD020
      *               ELSE
      *                 MOVE SPACES           TO CBP105-LINDET
      *                 MOVE DATA-VENCTO-CP20 TO DATA-INV
      *                 CALL "GRIDAT1" USING DATA-INV
      *                 CANCEL "GRIDAT1"
      *                 MOVE DATA-INV         TO DATA-E
      *                 MOVE DATA-E           TO CBP105-LINDET(01: 11)
      *                 MOVE DESCRICAO-CP20   TO CBP105-LINDET(12: 30)
      *                 MOVE VALOR-TOT-CP20   TO VALOR-E
      *                 MOVE VALOR-E          TO CBP105-LINDET(44: 13)
      *                 IF PREV-DEF-CP20 = 0
      *                    MOVE "D"           TO CBP105-LINDET(60: 01)
      *                 ELSE MOVE "P"         TO CBP105-LINDET(60: 01)
      *                 END-IF
      *                 IF LIBERADO-CP20 = 0
      *                    MOVE "NÃO" TO CBP105-LINDET(64: 03)
      *                 ELSE MOVE "SIM" TO CBP105-LINDET(64: 03)
      *                 END-IF
      *                 MOVE SEQ-CP20         TO CBP105-LINDET(75: 05)
      *                 MOVE "INSERE-LIST1" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *        END-READ
      *    END-PERFORM.
      *    MOVE "REFRESH-LIST" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE ZEROS TO SEQ-CTA-PAGAR-CB100.
       CLEAR-FLAGS SECTION.
           INITIALIZE CBP105-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CBP105-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CBD001 CBD100 CBD002
      *    CLOSE CPD020.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
