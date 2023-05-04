       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPP020B.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 13/07/1998
      *FUNÇÃO: Movimento de contas a pagar

      *-Para lançamento de contas permanentes - Caso seja inclusão -
      * incluir 13 meses(sendo o 1o.mês definitivo os demais previsto),
      * e verificar se existe a chave=fornecedor/docto e situação = 0 ou
      * = 1, caso afirmativo
      * não permitir a inclusão(solicitar mudança no nr.docto)
      * caso Alteração - alterar os 12 lançamentos c/ os novos dados
      * caso exclusão/suspensão- excluir/suspender os 12 lançamentos
      * Em caso de alteração de previsto p/ definitivo incluir uma
      * parcela(tendo assim sempre 12 prevista). E em caso de alteraçao
      * perguntar se a alteração é em uma parcela ou em todas)
      * Caso cancelamento - não permitir

      *-Um lançamento será considerado suspenso, aquelas contas que não
      * tem previsão p/ pagto, podendo mais tarde, voltar a ser conside-
      * rada uma conta normal

      *-As baixas de contas só serão permitidas, pelo sistema de caixa

      *-Quando entrar com o fornecedor, abrir janela para verificar
      * se existe programação financeira p/ o lançamento, caso contrá-
      * rio enviar uma CIE para o responsável.

      *- Contas desmebranda - é para ser utilizada no caixa, para lança-
      *  mentos na conta reduzida correta.

      *- O PORTADOR 49 NAO PODERA SER USADO NO CONTAS A PAGAR, ESSE
      *  PORTADOR SIGNIFICA QUE O LANCTO VEIO PELO CONTA CORRENTE
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CXPX020.
           COPY CPPX020.
           COPY CPPX021.
           COPY CPPX022.
           COPY CPPX099.
           COPY CAPX018.
           COPY CAPX019.
           COPY CBPX001.
           COPY CBPX003.
           COPY CBPX100.
           COPY GERX001.
           COPY GERX002.
      *    COPY CIEPX001.
      *    COPY CIEPX010.
           COPY LOGX001.
           COPY LOGX002.
           COPY LOGX003.
           COPY LOGACESS.SEL.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW020.
       COPY CAPW018.
       COPY CAPW019.
       COPY CBPW001.
       COPY CBPW003.
       COPY CBPW100.
       COPY CPPW020.
       COPY CPPW021.
       COPY CPPW022.
       COPY CPPW099.
       COPY GERW001.
       COPY GERW002.
      *COPY CIEPW001.
      *COPY CIEPW010.
       COPY LOGW001.
       COPY LOGW002.
       COPY LOGW003.
       COPY LOGACESS.FD.

       WORKING-STORAGE SECTION.
           COPY "CPP020B.CPB".
           COPY "CPP020B.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CPD021             PIC XX       VALUE SPACES.
           05  ST-CPD022             PIC XX       VALUE SPACES.
           05  ST-CPD099             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CAD019             PIC XX       VALUE SPACES.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD003             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-CIED001            PIC XX       VALUE SPACES.
           05  ST-GED001             PIC XX       VALUE SPACES.
           05  ST-GED002             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-LOG003             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
      *    05  ST-CIED010            PIC XX       VALUE SPACES.
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
      * DATA-WII - Encontrar proxima data caso a data de vencto da conta
      * permanente seja invalida, por exemplo 30/02/1998
           05  FORNEC-E              PIC ZZZZZZ   VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZ.ZZ.ZZ.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  NR-CHEQUE-W           PIC 9(6)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  SEQ-CIE               PIC 9(3)     VALUE ZEROS.
           05  DATA-LIMITE           PIC 9(08)    VALUE 19950101.
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  VALOR-TOT-DESM        PIC 9(8)V99  VALUE ZEROS.
      *  verifica total desmembrado de cada parcela p/ jogar o
      *  arredondamento na ultima parcela desmembrada
           05  QTDE-DESM             PIC 9(2)     VALUE ZEROS.
      *  verifica a quantidade de desmembradas
           05  PERC-GR OCCURS 10 PIC 9(3)V99.
      *  percentagem que será calculado em cima do parcelamento, desmem-
      *  brado por depto
           05  DIA-PADRAO            PIC 99       VALUE ZEROS.
      *  dia p/ alterar o vencto de contas permanentes
           05  QT-PARCELAS           PIC 99       VALUE ZEROS.
      *  QT-PARCELAS - qtde de parcelas permanentes programadas
           05  ULT-VENCTO            PIC 9(8)     VALUE ZEROS.
      *  ultima data de vencto de conta permanente programada.
           05  FORNEC-W              PIC 9(6)     VALUE ZEROS.
           05  DOCTO-W               PIC X(10)    VALUE SPACES.
      * FORNEC-W E DOCTO-W. VARIÁVEIS UTILIZADAS P/ ENCONTRAR AS CONTAS
      * PERMANENTES, RELACIONADAS COM AS MESMAS.
           05  SEQ-ALTERADA          PIC 9(5)     VALUE ZEROS.
      *  SEQ-ALTERADA - é a 1 sequencia da parcela da conta permanente
      *                 alterada.
           COPY "PARAMETR".

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU                 PIC 9(04).
             10 WS-MES-CPU                 PIC 9(02).
             10 WS-DIA-CPU                 PIC 9(02).
          05 FILLER                        PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       LINKAGE SECTION.
       01  STRING-1       PIC X(20).

       PROCEDURE DIVISION USING STRING-1 PARAMETROS-W.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CPP020B-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE CPP020B-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CPP020B-DATA-BLOCK-VERSION-NO
                                    TO DS-DATA-BLOCK-VERSION-NO
           MOVE CPP020B-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W           TO EMP-REC
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CAD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD019.
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD003.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CPD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CPD021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD021.
           MOVE "CPD022" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD022.
           MOVE "CPD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPD099.
           MOVE "GED001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-GED001.
           MOVE "GED002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-GED002.
      *    MOVE "CIED001" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED001.
      *    MOVE "CIED010" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CIED010.
           MOVE "LOG001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG001
           MOVE "LOG002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002
           MOVE "LOG003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG003
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O CPD020 CPD021 CPD022 CBD100
                    LOG001 LOG002 LOG003.
      *    OPEN I-O CIED001 CIED010.
           OPEN INPUT CAD018 CAD019 CGD001 CXD020 GED001 GED002.
           OPEN I-O CPD099.
           IF ST-CPD099 = "35"
              CLOSE CPD099      OPEN OUTPUT CPD099
              CLOSE CPD099      OPEN I-O CPD099.
           CLOSE CPD099.
           IF ST-CPD020 = "35"
              CLOSE CPD020      OPEN OUTPUT CPD020
              CLOSE CPD020      OPEN I-O CPD020
           END-IF.
           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O CBD100
           END-IF.
           IF ST-CPD021 = "35"
              CLOSE CPD021      OPEN OUTPUT CPD021
              CLOSE CPD021      OPEN I-O CPD021
           END-IF.
           IF ST-CPD022 = "35"
              CLOSE CPD022      OPEN OUTPUT CPD022
              CLOSE CPD022      OPEN I-O CPD022
           END-IF.
      *    IF ST-CIED001 = "35"
      *       CLOSE CIED001     OPEN OUTPUT CIED001
      *       CLOSE CIED001     OPEN I-O CIED001
      *    END-IF.
      *    IF ST-CIED010 = "35"
      *       CLOSE CIED010     OPEN OUTPUT CIED010
      *       CLOSE CIED010     OPEN I-O CIED010
      *    END-IF.
           IF ST-GED001 <> "00"
              MOVE "ERRO ABERTURA GED001: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-GED001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-GED002 <> "00"
              MOVE "ERRO ABERTURA GED002: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-GED002 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CAD018 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD019 <> "00"
              MOVE "ERRO ABERTURA CAD019: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CAD019 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD100 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CPD020 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD021 <> "00"
              MOVE "ERRO ABERTURA CPD021: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CPD021 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD022 <> "00"
              MOVE "ERRO ABERTURA CPD022: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CPD022 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CBD001.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG001 <> "00"
              MOVE "ERRO ABERTURA LOG001: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOG003 <> "00"
              MOVE "ERRO ABERTURA LOG003: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD001 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CBD001.
           OPEN INPUT CBD003.
           IF ST-CBD003 <> "00"
              MOVE "ERRO ABERTURA CBD003: "  TO CPP020B-MENSAGEM-ERRO
              MOVE ST-CBD003 TO CPP020B-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CBD003.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP020B"           to logacess-programa
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


           INITIALIZE REG-GED001
           MOVE "N" TO ACHEI
           MOVE "CPP020 - Movimento do Contas a Pagar" TO PROGRAMA-GE01
           START GED001 KEY IS NOT LESS ALT-GE01 INVALID KEY
               MOVE "10" TO ST-GED001.
           PERFORM UNTIL ST-GED001 = "10"
               READ GED001 NEXT AT END
                   MOVE "10" TO ST-GED001
               NOT AT END
                   IF "CPP020 - Movimento do Contas a Pagar" <>
                      PROGRAMA-GE01
                      MOVE "10" TO ST-GED001
                   ELSE
                      MOVE USUARIO-GE01          TO USUARIO-C-GE02
                      MOVE COD-USUARIO-W      TO USUARIO-P-GE02
                      MOVE "CPP020 - Movimento do Contas a Pagar" TO
                           PROGRAMA-GE02
                      MOVE SPACES             TO IDENTIFICACAO-GE02
                      STRING STRING-1(1:6) STRING-1(7: 5) INTO
                           IDENTIFICACAO-GE02
                      READ GED002 NOT INVALID KEY
                           MOVE "S"           TO ACHEI
                           MOVE "10"          TO ST-GED001
                      END-READ
                   END-IF
               END-READ
           END-PERFORM

      *    IF ACHEI = "N"
      *       MOVE "USUÁRIO SEM PERMISSÃO PARA MANUTENÇÃO" TO
      *       CPP020B-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO
      *    ELSE
      *       IF OPERACAO-GE02 = "Alteração"
      *          MOVE "DESABILITA-EXCLUSAO" TO DS-PROCEDURE
      *       ELSE
      *          MOVE "DESABILITA-ALTERACAO" TO DS-PROCEDURE.

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CPP020B-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN CPP020B-SAVE-FLG-TRUE
                    PERFORM REGRAVA-DADOS
               WHEN CPP020B-LOAD-FLG-TRUE
                    PERFORM CARREGAR-DADOS
                    refresh-object principal
                    MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN CPP020B-EXCLUI-FLG-TRUE
                    MOVE FORNEC-CP20   TO FORNEC-W
                    MOVE NR-DOCTO-CP20 TO DOCTO-W
                    MOVE 3 TO SITUACAO-CP20
                    PERFORM EXCLUI
               WHEN CPP020B-SUSPENDE-FLG-TRUE
                    IF CPP020B-SITUACAO = 0
                       MOVE 1 TO SITUACAO-CP20
                    ELSE
                       MOVE 0 TO SITUACAO-CP20
                    END-IF
                    MOVE FORNEC-CP20   TO FORNEC-W
                    MOVE NR-DOCTO-CP20 TO DOCTO-W
                    PERFORM SUSPENDE-CANCELA
               WHEN CPP020B-CANCELA-FLG-TRUE
                    IF CPP020B-CANCELA-FLG-TRUE AND
                       TIPO-CONTA-CP20 = 1
                       MOVE "NAO-PERMITE-CANCELAR" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                    ELSE
                       MOVE FORNEC-CP20 TO FORNEC-W
                       MOVE NR-DOCTO-CP20 TO DOCTO-W
                       MOVE 4 TO SITUACAO-CP20
                       PERFORM SUSPENDE-CANCELA
                    END-IF
               WHEN CPP020B-VERIFICA-TALAO-TRUE
                    PERFORM VERIFICA-TALAO
               WHEN CPP020B-LE-NOMINAL-TRUE
                    PERFORM LE-NOMINAL
               WHEN CPP020B-LE-FORNEC-TRUE
                    PERFORM LE-FORNEC
               WHEN CPP020B-LE-BANCO-TRUE
                    PERFORM LE-BANCO
               WHEN CPP020B-LE-SITUACAO-TRUE
                    PERFORM LE-SITUACAO
               WHEN CPP020B-LE-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN CPP020B-LE-TIPO-FORNEC-TRUE
                    PERFORM LE-TIPO-FORNEC
               WHEN CPP020B-LE-COD-APURACAO-TRUE
                    PERFORM LE-COD-APURACAO
      *        WHEN CPP020B-CHAMAR-APURACAO-TRUE
      *             PERFORM CHAMAR-APURACAO
               WHEN CPP020B-CARREGA-DATA-TRUE
                    PERFORM CARREGA-DATA
               WHEN CPP020B-VERIF-DOCTO-TRUE
                    PERFORM VERIFICA-DOCTO-PERMANENTE
      *        WHEN CPP020B-VERIF-PROGRAMACAO-TRUE
      *             PERFORM VERIFICA-PROGRAMACAO
               WHEN CPP020B-EMISSAO-VENCTO-TRUE
                    PERFORM INVERTE-EMIS-VENCTO
               WHEN CPP020B-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POP-UP
      *        WHEN CPP020B-ITEM-SELEC-PROG-TRUE
      *             PERFORM PROGRAMACAO-SELECIONADA
               WHEN CPP020B-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO-FORN
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *CHAMAR-APURACAO SECTION.
      *    CALL "CXP020T" USING PASSAR-PARAMETROS
      *    CANCEL "CXP020T"
      *    MOVE PASSAR-STRING-1(52: 3) TO CPP020B-COD-APURACAO
      *    PERFORM LE-COD-APURACAO.
       CHAMAR-POP-UP SECTION.
           EVALUATE CPP020B-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-FORNEC
      *             CALL "CGP001T" USING PASSAR-PARAMETROS
      *             CANCEL "CGP001T"
      *             MOVE PASSAR-STRING-1(33: 6) TO CPP020B-COD-FORN
             WHEN 2 CALL "CAP019T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CAP019T"
                    MOVE PASSAR-STRING-1(1: 30) TO
                         CPP020B-DESCR-TIPO-FORN
                    MOVE PASSAR-STRING-1(33: 2) TO CPP020B-TIPO-FORN
             WHEN 3 CALL "CAP018T" USING PARAMETROS-W PASSAR-PARAMETROS
                   CANCEL "CAP018T"
                   MOVE PASSAR-STRING-1(1: 30) TO CPP020B-DESCR-PORTADOR
                   MOVE PASSAR-STRING-1(33: 4) TO CPP020B-PORTADOR
             WHEN 4
      *             PERFORM CARREGA-POP-UP-APURACAO
                    CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CXP020T"
                    MOVE PASSAR-PARAMETROS(52: 5)TO CPP020b-COD-APURACAO
                    MOVE CPP020B-COD-APURACAO    TO CODIGO-REDUZ-CX20
                    READ CXD020 INVALID KEY
                         MOVE SPACES          TO DESCRICAO-CX20
                    END-READ
                    MOVE DESCRICAO-CX20       TO CPP020B-DESCR-APURACAO
             WHEN 5 CALL "CBP001T" USING PARAMETROS-W PASSAR-PARAMETROS
                   CANCEL "CBP001T"
                   MOVE PASSAR-STRING-1(17: 15) TO CPP020B-DESC-BANCO-CH
                   MOVE PASSAR-STRING-1(49: 6)  TO CPP020B-BANCO-CH
             WHEN 6 CALL "CBP003T" USING PARAMETROS-W PASSAR-PARAMETROS
                    CANCEL "CBP003T"
                    MOVE PASSAR-STRING-1(1: 30) TO
                                                CPP020B-DESC-SITUACAO-CH
                    MOVE PASSAR-STRING-1(40: 2) TO CPP020B-SITUACAO-CH
           END-EVALUATE.
       ITEM-SELECIONADO-FORN SECTION.
           IF CPP020B-OPCAO-POP-UP = 4
              PERFORM ITEM-SELECIONADO-APURACAO
           ELSE
              IF CPP020B-OPCAO-POP-UP = 7
                 MOVE CPP020B-LINDET1(33: 6) TO CPP020B-NOMINAL-A-CH
                 MOVE CPP020B-LINDET1(1: 30) TO CPP020B-NOME-CH
              ELSE
                 MOVE CPP020B-LINDET1(33: 6) TO CPP020B-COD-FORN
                 MOVE CPP020B-LINDET1(1: 30) TO CPP020B-DESCR-FORN.
       CARREGA-POP-UP-FORNEC SECTION.
           MOVE CPP020B-LINDET1(1: 1)        TO NOME-CG01 LETRA.
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
                    MOVE NOME-CG01     TO CPP020B-LINDET1(1: 32)
                    MOVE CODIGO-CG01   TO CPP020B-LINDET1(33: 06)
                    MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              END-READ
           END-PERFORM.
       ITEM-SELECIONADO-APURACAO SECTION.
           MOVE CPP020B-LINDET1(52: 5) TO CPP020B-COD-APURACAO.
           PERFORM LE-COD-APURACAO.
           MOVE DESCRICAO-CX20         TO CPP020B-DESCR-APURACAO.
       CARREGA-POP-UP-APURACAO SECTION.
           MOVE "CLEAR-LIST-BOX1"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS                  TO CODIGO-COMPL-CX20.
           START CXD020 KEY IS NOT < CODIGO-COMPL-CX20 INVALID KEY
                 MOVE "10" TO ST-CXD020.
           PERFORM UNTIL ST-CXD020 = "10"
                 READ CXD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD020
                 NOT AT END
                      MOVE SPACES TO CPP020B-LINDET1
                      MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                      EVALUATE GRAU-CX20
                          WHEN 1 PERFORM GRAU-1
                          WHEN 2 PERFORM GRAU-2
                          WHEN 3 PERFORM GRAU-3
                          WHEN 4 PERFORM GRAU-4
                      END-EVALUATE
                      MOVE "INSERE-POP-UP-APUR" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
       GRAU-1 SECTION.
           MOVE CODIGO-E          TO CPP020B-LINDET1(1: 11)
           MOVE DESCRICAO-CX20    TO CPP020B-LINDET1(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO CPP020B-LINDET1(52: 05).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO CPP020B-LINDET1(4: 11)
           MOVE DESCRICAO-CX20    TO CPP020B-LINDET1(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO CPP020B-LINDET1(52: 05).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO CPP020B-LINDET1(7: 11)
           MOVE DESCRICAO-CX20    TO CPP020B-LINDET1(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO CPP020B-LINDET1(52: 05).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO CPP020B-LINDET1(10: 11)
           MOVE DESCRICAO-CX20    TO CPP020B-LINDET1(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO CPP020B-LINDET1(52: 05).

       INVERTE-EMIS-VENCTO SECTION.
           MOVE CPP020B-DATA-EMISSAO TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO CPP020B-EMISSAO-INV.
           MOVE CPP020B-DATA-VENCTO TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           CANCEL "GRIDAT2".
           MOVE DATA-INV TO CPP020B-VENCTO-INV.
       CARREGA-DATA SECTION.
           MOVE DATA-MOVTO-W    TO CPP020B-DATA-MOVTO.
           MOVE "CARREGAR-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI SECTION.
           OPEN I-O CPD099
           MOVE REG-CPD020 TO REG-CPD099
           WRITE REG-CPD099 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "I"         TO LOG3-OPERACAO
               MOVE "CPD099"    TO LOG3-ARQUIVO
               MOVE "CPP020B"   TO LOG3-PROGRAMA
               MOVE REG-CPD099  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           DELETE CPD020 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "E"         TO LOG3-OPERACAO
               MOVE "CPD020"    TO LOG3-ARQUIVO
               MOVE "CPP020B"   TO LOG3-PROGRAMA
               MOVE REG-CPD020  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           IF PORTADOR-CP20 = 12
              MOVE 8 TO SITUACAO-CB100
              IF CPP020B-BANCO-CH     <> ZEROS AND
                 CPP020B-NR-CHEQUE-CH <> ZEROS
                 REWRITE REG-CBD100 NOT INVALID KEY
                     MOVE USUARIO-W   TO LOG2-USUARIO
                     MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                     MOVE WS-DATA-CPU TO LOG2-DATA
                     ACCEPT WS-HORA-SYS FROM TIME
                     MOVE WS-HORA-SYS TO LOG2-HORAS
                     MOVE "A"         TO LOG2-OPERACAO
                     MOVE "CBD100"    TO LOG2-ARQUIVO
                     MOVE "CPP020B"   TO LOG2-PROGRAMA
                     MOVE REG-CBD100  TO LOG2-REGISTRO
                     WRITE REG-LOG002
                     END-WRITE.

           IF TIPO-CONTA-CP20 = 1
              PERFORM ESTORNA-PERMANENTE
           ELSE
              CLOSE CPD099.

           PERFORM LIMPAR-DADOS.
       SUSPENDE-CANCELA SECTION.
           PERFORM VALIDA-DATA-LIMITE

           REWRITE REG-CPD020 NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "A"         TO LOG3-OPERACAO
               MOVE "CPD020"    TO LOG3-ARQUIVO
               MOVE "CPP020B"   TO LOG3-PROGRAMA
               MOVE REG-CPD020  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE.

           MOVE FORNEC-CP20 TO NOMINAL-A-CB100.
           MOVE SEQ-CP20 TO SEQ-CTA-PAGAR-CB100.
           START CBD100 KEY IS = ALT2-CB100 INVALID KEY
                 CONTINUE
           NOT INVALID KEY
                READ CBD100 NEXT RECORD
                END-READ
                MOVE 8 TO SITUACAO-CB100
                REWRITE REG-CBD100 NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG2-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG2-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG2-HORAS
                        MOVE "A"         TO LOG2-OPERACAO
                        MOVE "CBD100"    TO LOG2-ARQUIVO
                        MOVE "CPP020B"   TO LOG2-PROGRAMA
                        MOVE REG-CBD100  TO LOG2-REGISTRO
                        WRITE REG-LOG002
                        END-WRITE.
           IF CPP020B-SUSPENDE-FLG-TRUE
              IF TIPO-CONTA-CP20 = 1
                 PERFORM SUSPENDE-CANCELA-PERMANENTE.
           IF CPP020B-CANCELA-FLG-TRUE
              IF TIPO-CONTA-CP20 = 1
                 PERFORM SUSPENDE-CANCELA-PERMANENTE.
           PERFORM LIMPAR-DADOS.
       SUSPENDE-CANCELA-PERMANENTE SECTION.
      *    SE SITUACAO = 0 PASSA SER 1 OU SITUACAO = 1 PASSA A SER 0
           MOVE FORNEC-W TO FORNEC-CP20.
           MOVE ZEROS TO SEQ-CP20.
           START CPD020 KEY IS NOT < CHAVE-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CPD020
              NOT AT END
                  IF FORNEC-CP20 NOT = FORNEC-W
                     MOVE "10" TO ST-CPD020
                  ELSE
                     IF NR-DOCTO-CP20 NOT = DOCTO-W OR
      *                 SITUACAO-CP20 NOT = 0 OR
                        TIPO-CONTA-CP20 NOT = 1
                          CONTINUE
                     ELSE
                       IF CPP020B-SUSPENDE-FLG-TRUE
                          IF CPP020B-SITUACAO = 0
                             MOVE 1 TO SITUACAO-CP20
                          ELSE
                             MOVE 0 TO SITUACAO-CP20
                          END-IF
                       END-IF
                       IF CPP020B-CANCELA-FLG-TRUE
                          MOVE 4 TO SITUACAO-CP20
                       END-IF
                       PERFORM VALIDA-DATA-LIMITE
                       REWRITE REG-CPD020 NOT INVALID KEY
                             MOVE USUARIO-W   TO LOG3-USUARIO
                             MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                             MOVE WS-DATA-CPU TO LOG3-DATA
                             ACCEPT WS-HORA-SYS FROM TIME
                             MOVE WS-HORA-SYS TO LOG3-HORAS
                             MOVE "A"         TO LOG3-OPERACAO
                             MOVE "CPD020"    TO LOG3-ARQUIVO
                             MOVE "CPP020B"   TO LOG3-PROGRAMA
                             MOVE REG-CPD020  TO LOG3-REGISTRO
                             WRITE REG-LOG003
                             END-WRITE
                       END-REWRITE
                       MOVE FORNEC-CP20 TO NOMINAL-A-CB100
                       MOVE SEQ-CP20    TO SEQ-CTA-PAGAR-CB100
                       START CBD100 KEY IS = ALT2-CB100 INVALID KEY
                            CONTINUE
                       NOT INVALID KEY
                            READ CBD100 NEXT RECORD
                            END-READ
                            MOVE 8 TO SITUACAO-CB100
                             REWRITE REG-CBD100 NOT INVALID KEY
                                  MOVE USUARIO-W   TO LOG2-USUARIO
                                  MOVE FUNCTION CURRENT-DATE TO
                                       WS-DATA-SYS
                                  MOVE WS-DATA-CPU TO LOG2-DATA
                                  ACCEPT WS-HORA-SYS FROM TIME
                                  MOVE WS-HORA-SYS TO LOG2-HORAS
                                  MOVE "A"         TO LOG2-OPERACAO
                                  MOVE "CBD100"    TO LOG2-ARQUIVO
                                  MOVE "CPP020B"   TO LOG2-PROGRAMA
                                  MOVE REG-CBD100  TO LOG2-REGISTRO
                                  WRITE REG-LOG002
                                  END-WRITE
                             END-REWRITE
                       END-START
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.
      * Quando suspende uma conta permanente,as demais parcelas relacio-
      * nadas também deverão ser suspensas
       ESTORNA-PERMANENTE SECTION.
           MOVE ZEROS TO SEQ-CP20.
           MOVE FORNEC-W TO FORNEC-CP20.
           START CPD020 KEY IS NOT < CHAVE-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                 MOVE "10" TO ST-CPD020
              NOT AT END
                  IF FORNEC-CP20 NOT = FORNEC-W
                     MOVE "10" TO ST-CPD020
                  ELSE
                     IF NR-DOCTO-CP20 NOT = DOCTO-W OR
                        SITUACAO-CP20 NOT = 0 OR
                        TIPO-CONTA-CP20 NOT = 1
                        CONTINUE
                     ELSE
                        MOVE 3 TO SITUACAO-CP20
                        MOVE REG-CPD020 TO REG-CPD099
                        WRITE REG-CPD099 NOT INVALID KEY
                              MOVE USUARIO-W   TO LOG3-USUARIO
                              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                              MOVE WS-DATA-CPU TO LOG3-DATA
                              ACCEPT WS-HORA-SYS FROM TIME
                              MOVE WS-HORA-SYS TO LOG3-HORAS
                              MOVE "I"         TO LOG3-OPERACAO
                              MOVE "CPD099"    TO LOG3-ARQUIVO
                              MOVE "CPP020B"    TO LOG3-PROGRAMA
                              MOVE REG-CPD099  TO LOG3-REGISTRO
                              WRITE REG-LOG003
                              END-WRITE
                        END-WRITE
                        DELETE CPD020 NOT INVALID KEY
                              MOVE USUARIO-W   TO LOG3-USUARIO
                              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                              MOVE WS-DATA-CPU TO LOG3-DATA
                              ACCEPT WS-HORA-SYS FROM TIME
                              MOVE WS-HORA-SYS TO LOG3-HORAS
                              MOVE "E"         TO LOG3-OPERACAO
                              MOVE "CPD020"    TO LOG3-ARQUIVO
                              MOVE "CPP020B"    TO LOG3-PROGRAMA
                              MOVE REG-CPD020  TO LOG3-REGISTRO
                              WRITE REG-LOG003
                              END-WRITE
                        END-DELETE
                        MOVE FORNEC-CP20 TO NOMINAL-A-CB100
                        MOVE SEQ-CP20    TO SEQ-CTA-PAGAR-CB100
                        START CBD100 KEY IS = ALT2-CB100 INVALID KEY
                              CONTINUE
                        NOT INVALID KEY
                              READ CBD100 NEXT RECORD
                              END-READ
                              MOVE 8 TO SITUACAO-CB100
                              REWRITE REG-CBD100 NOT INVALID KEY
                                  MOVE USUARIO-W   TO LOG2-USUARIO
                                  MOVE FUNCTION CURRENT-DATE
                                                   TO WS-DATA-SYS
                                  MOVE WS-DATA-CPU TO LOG2-DATA
                                  ACCEPT WS-HORA-SYS FROM TIME
                                  MOVE WS-HORA-SYS TO LOG2-HORAS
                                  MOVE "A"         TO LOG2-OPERACAO
                                  MOVE "CBD100"    TO LOG2-ARQUIVO
                                  MOVE "CPP020B"    TO LOG2-PROGRAMA
                                  MOVE REG-CBD100  TO LOG2-REGISTRO
                                  WRITE REG-LOG002
                                  END-WRITE
                              END-REWRITE
                        END-START
                     END-IF
              END-READ
           END-PERFORM.
           CLOSE CPD099.
      * Quando estorna uma conta permanente, as demais parcelas relacio-
      * nadas também deverão ser excluídas
       VERIFICA-TALAO SECTION.
           MOVE ZEROS TO I CPP020B-ERRO-NR-CHEQUE.
           IF CPP020B-QT-PARCELA <> ZEROS
              MOVE CPP020B-NR-CHEQUE-CH TO NR-CHEQUE-W NR-CHEQUE-CB100
              MOVE CPP020B-BANCO-CH     TO NOMINAL-A-CB100
              IF CPP020B-NR-CHEQUE-CH = ZEROS AND
                 CPP020B-BANCO-CH = ZEROS
                 CONTINUE
              ELSE
                 START CBD100 KEY IS NOT < CHAVE-CB100 INVALID KEY
                       MOVE "10" TO ST-CBD100
                 END-START
                 MOVE 1 TO CPP020B-ERRO-NR-CHEQUE
                 PERFORM UNTIL ST-CBD100 = "10"
                       READ CBD100 NEXT RECORD AT END
                            MOVE "10" TO ST-CBD100
                       NOT AT END
      *  SITUACAO = 01-CH.EM BRANCO  03-CH.A PRAZO
                            IF SITUACAO-CB100 <> 01 AND <> 03
                               MOVE "10" TO ST-CBD100
                            ELSE
                               ADD 1 TO I
                               IF I = CPP020B-QT-PARCELA
                                  MOVE "10" TO ST-CBD100
                                  MOVE 0 TO CPP020B-ERRO-NR-CHEQUE
                               END-IF
                            END-IF
                       END-READ
                 END-PERFORM
            END-IF
           ELSE
            IF CPP020B-NR-CHEQUE-CH = ZEROS AND
               CPP020B-BANCO-CH     = ZEROS
               CONTINUE
            ELSE
                MOVE CPP020B-NR-CHEQUE-CH TO NR-CHEQUE-CB100
                MOVE CPP020B-BANCO-CH     TO CODIGO-FORN-CB100
                READ CBD100 INVALID KEY
                     MOVE 1 TO CPP020B-ERRO-NR-CHEQUE
                NOT INVALID KEY
                     CONTINUE
                END-READ
            END-IF
           END-IF.
       VERIFICA-12PERMANENTE SECTION.
           MOVE ZEROS         TO DATA-VENCTO-CP20
                                 SITUACAO-CP20.
           MOVE FORNEC-CP20   TO FORNEC-W.
           MOVE NR-DOCTO-CP20 TO DOCTO-W.
           MOVE ZEROS         TO QT-PARCELAS
                                 ULT-VENCTO.
           START CPD020 KEY IS NOT < ALT4-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CPD020
                 NOT AT END
                     IF FORNEC-CP20   NOT = FORNEC-W OR
                        SITUACAO-CP20 NOT = 0
                        MOVE "10" TO ST-CPD020
                     ELSE
                       IF NR-DOCTO-CP20   NOT = DOCTO-W OR
                          TIPO-CONTA-CP20 NOT = 1       OR
                          PREV-DEF-CP20 = 0
                          CONTINUE
                       ELSE
                          ADD 1 TO QT-PARCELAS
                          IF DATA-VENCTO-CP20 > ULT-VENCTO
                             MOVE DATA-VENCTO-CP20 TO ULT-VENCTO
                          END-IF
                       END-IF
                    END-IF
                 END-READ
           END-PERFORM.
           IF QT-PARCELAS < 12
              PERFORM UNTIL QT-PARCELAS = 12
                 ADD 1                 TO QT-PARCELAS
                 MOVE ULT-VENCTO       TO DATA-WI
                 MOVE CPP020B-COD-FORN TO FORNEC-CP21
                 ADD 1                 TO MES-WI
                 IF MES-WI > 12
                    MOVE 1 TO MES-WI
                    ADD 1  TO ANO-WI
                 END-IF
                 PERFORM ATUALIZA-SEQ-CPD021
                 MOVE SEQ-CP21         TO SEQ-CP20
                 MOVE 1                TO PREV-DEF-CP20
                 MOVE 2                TO GRTIME-TYPE
                 MOVE 7                TO GRTIME-FUNCTION
                 MOVE DATA-WI          TO DATA-VENCTO-CP20
                                          GRTIME-DATE
                                          DATA-WII
                 CALL "GRTIME" USING PARAMETROS-GRTIME
                 CANCEL "GRTIME"
                 IF GRTIME-DATE-FINAL = ZEROS
                    MOVE 1             TO DIA-WII
                    ADD  1             TO MES-WII
                    IF MES-WII = 13
                       MOVE 01         TO MES-WII
                       ADD 1           TO ANO-WII
                    END-IF
                    MOVE DATA-WII      TO DATA-VENCTO-CP20
                 END-IF

                 PERFORM VALIDA-DATA-LIMITE

                 WRITE REG-CPD020 INVALID KEY
                    MOVE "CPD020"      TO CPP020B-MENSAGEM-ERRO(15: 07)
                    MOVE ST-CPD020     TO CPP020B-MENSAGEM-ERRO(23: 02)
                    PERFORM ERRO-GRAVACAO
                 NOT INVALID KEY
                    MOVE USUARIO-W   TO LOG3-USUARIO
                    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                    MOVE WS-DATA-CPU TO LOG3-DATA
                    ACCEPT WS-HORA-SYS FROM TIME
                    MOVE WS-HORA-SYS TO LOG3-HORAS
                    MOVE "A"         TO LOG3-OPERACAO
                    MOVE "CPD020"    TO LOG3-ARQUIVO
                    MOVE "CPP020B"    TO LOG3-PROGRAMA
                    MOVE REG-CPD020  TO LOG3-REGISTRO
                    WRITE REG-LOG003
                    END-WRITE
                 END-WRITE
      *          PERFORM GRAVA-CIE
              END-PERFORM
           END-IF.
      *    verifica se existe 12 parcelas permanentes programadas,
      *    caso não haja, completar.
       ALTERA-PERMANENTE SECTION.
           MOVE SEQ-CP20 TO SEQ-ALTERADA.
           MOVE ZEROS TO SITUACAO-CP20 DATA-VENCTO-CP20.
           MOVE FORNEC-W      TO FORNEC-CP20.
           MOVE ZEROS         TO SEQ-CP20.
           MOVE CPP020B-VENCTO-INV(7: 2) TO DIA-PADRAO.
      *    altera apenas o dia de vencto
           START CPD020 KEY IS NOT < CHAVE-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CPD020
              NOT AT END
                   IF FORNEC-CP20 NOT = FORNEC-W
                      MOVE "10" TO ST-CPD020
                   ELSE
                      IF NR-DOCTO-CP20   NOT = DOCTO-W      OR
                         TIPO-CONTA-CP20 NOT = 1            OR
                         SEQ-CP20            = SEQ-ALTERADA OR
                         SITUACAO-CP20   NOT = 0
                         CONTINUE
                      ELSE
                         MOVE CPP020B-TIPO-FORN    TO TIPO-FORN-CP20
                         MOVE CPP020B-PORTADOR     TO PORTADOR-CP20
                         MOVE CPP020B-DESCRICAO    TO DESCRICAO-CP20
                         MOVE CPP020B-TIPO-MOEDA(1: 1)
                                                   TO TIPO-MOEDA-CP20
                         MOVE CPP020B-COD-APURACAO TO CODREDUZ-APUR-CP20
                         MOVE CPP020B-JUROS-MORA   TO JUROS-MORA-CP20
                         MOVE CPP020B-MULTA-ATRASO TO MULTA-ATRASO-CP20
                         MOVE CPP020B-VALOR-TOTAL  TO VALOR-TOT-CP20
                         MOVE CPP020B-EMISSAO-INV  TO DATA-EMISSAO-CP20
                         MOVE DATA-VENCTO-CP20     TO DATA-WI ULT-VENCTO
                         MOVE DIA-PADRAO           TO DIA-WI
                         MOVE 2                    TO GRTIME-TYPE
                         MOVE 7                    TO GRTIME-FUNCTION
                         MOVE DATA-WI              TO DATA-VENCTO-CP20
                                                      GRTIME-DATE
                                                      DATA-WII
                         CALL "GRTIME" USING PARAMETROS-GRTIME
                         CANCEL "GRTIME"
                         IF GRTIME-DATE-FINAL = ZEROS
                            MOVE ULT-VENCTO TO DATA-VENCTO-CP20
                         END-IF

                         PERFORM VALIDA-DATA-LIMITE
                         REWRITE REG-CPD020 NOT INVALID KEY
                               MOVE USUARIO-W   TO LOG3-USUARIO
                               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                               MOVE WS-DATA-CPU TO LOG3-DATA
                               ACCEPT WS-HORA-SYS FROM TIME
                               MOVE WS-HORA-SYS TO LOG3-HORAS
                               MOVE "A"         TO LOG3-OPERACAO
                               MOVE "CPD020"    TO LOG3-ARQUIVO
                               MOVE "CPP020B"    TO LOG3-PROGRAMA
                               MOVE REG-CPD020  TO LOG3-REGISTRO
                               WRITE REG-LOG003
                               END-WRITE
                         END-REWRITE
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.
      * caso a opcao op-alter-permanente = 2 (todas) altera todas as
      * contas permanentes previstas
       LE-FORNEC SECTION.
           MOVE CPP020B-COD-FORN     TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "********"      TO NOME-CG01.
           MOVE NOME-CG01            TO CPP020B-DESCR-FORN
                                        CPP020B-NOME-CH.
       LE-NOMINAL SECTION.
           MOVE CPP020B-NOMINAL-A-CH TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "********"      TO NOME-CG01.
           MOVE NOME-CG01            TO CPP020B-NOME-CH.
       LE-BANCO SECTION.
           OPEN INPUT CBD001.
           MOVE CPP020B-BANCO-CH     TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY
                MOVE "********"      TO NOME-BANCO-CB01.
           MOVE NOME-BANCO-CB01      TO CPP020B-DESC-BANCO-CH.
           CLOSE CBD001.
       LE-SITUACAO SECTION.
           OPEN INPUT CBD003.
           MOVE CPP020B-SITUACAO-CH  TO SITUACAO-CB03.
           READ CBD003 INVALID KEY
                MOVE "********"      TO NOME-SIT-CB03.
           MOVE NOME-SIT-CB03        TO CPP020B-DESC-SITUACAO-CH.
           CLOSE CBD003.
       LE-PORTADOR SECTION.
           MOVE CPP020B-PORTADOR     TO PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "******"        TO NOME-PORT.
           MOVE NOME-PORT            TO CPP020B-DESCR-PORTADOR.
       LE-TIPO-FORNEC SECTION.
           MOVE CPP020B-TIPO-FORN    TO CODIGO-TIPO.
           READ CAD019 INVALID KEY
                MOVE "****"          TO NOME-TIPO.
           MOVE NOME-TIPO            TO CPP020B-DESCR-TIPO-FORN.
       LE-COD-APURACAO SECTION.
           MOVE CPP020B-COD-APURACAO TO CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"         TO DESCRICAO-CX20
                MOVE ZEROS           TO TIPO-CONTA-CX20.
           MOVE DESCRICAO-CX20       TO CPP020B-DESCR-APURACAO.
           IF TIPO-CONTA-CX20 = 0
              MOVE 0                 TO CPP020B-TIPO-CONTA-APUR
           ELSE
              MOVE 1                 TO CPP020B-TIPO-CONTA-APUR.
       CARREGAR-DADOS SECTION.
           MOVE STRING-1(1: 6)       TO FORNEC-CP20.
           MOVE STRING-1(7: 5)       TO SEQ-CP20.
           START CPD020 KEY IS = CHAVE-CP20 INVALID KEY
                CONTINUE.
           READ CPD020 INVALID KEY
                INITIALIZE REG-CPD020.

           MOVE DATA-MOVTO-CP20      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO CPP020B-DATA-MOVTO.
           MOVE FORNEC-CP20          TO CPP020B-COD-FORN CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "*****"         TO NOME-CG01.
           MOVE NOME-CG01            TO CPP020B-DESCR-FORN.
           MOVE TIPO-FORN-CP20       TO CPP020B-TIPO-FORN CODIGO-TIPO.
           READ CAD019 INVALID KEY
                MOVE "****"          TO NOME-TIPO.
           MOVE NOME-TIPO            TO CPP020B-DESCR-TIPO-FORN.
           MOVE PORTADOR-CP20        TO CPP020B-PORTADOR PORTADOR.
           READ CAD018 INVALID KEY
                MOVE "******"        TO NOME-PORT.
           MOVE NOME-PORT            TO CPP020B-DESCR-PORTADOR.
           MOVE NR-DOCTO-CP20        TO CPP020B-NR-DOCTO.
           MOVE DATA-EMISSAO-CP20    TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV             TO CPP020B-DATA-EMISSAO.
           MOVE DATA-VENCTO-CP20     TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV             TO CPP020B-DATA-VENCTO.
           MOVE DESCRICAO-CP20       TO CPP020B-DESCRICAO.
           MOVE DIGITADOR-CP20       TO CPP020B-DIGITADOR.
           MOVE TIPO-MOEDA-CP20      TO CPP020B-TIPO-MOEDA.
           EVALUATE TIPO-MOEDA-CP20
             WHEN 0 MOVE "-Real"     TO CPP020B-TIPO-MOEDA(2: 6)
             WHEN 1 MOVE "-Dolar"    TO CPP020B-TIPO-MOEDA(2: 5)
           END-EVALUATE
           MOVE CODREDUZ-APUR-CP20   TO CPP020B-COD-APURACAO
                                        CODIGO-REDUZ-CX20.
           READ CXD020 INVALID KEY
                MOVE "*****"         TO DESCRICAO-CX20.
           MOVE DESCRICAO-CX20       TO CPP020B-DESCR-APURACAO.
           MOVE PREV-DEF-CP20        TO CPP020B-PREV-DEF
           EVALUATE PREV-DEF-CP20
             WHEN 0 MOVE "-Definitivo" TO CPP020B-PREV-DEF(2: 11)
             WHEN 1 MOVE "-Previsto  " TO CPP020B-PREV-DEF(2: 11)
           END-EVALUATE
           MOVE JUROS-MORA-CP20      TO CPP020B-JUROS-MORA.
           MOVE MULTA-ATRASO-CP20    TO CPP020B-MULTA-ATRASO.
           MOVE TAXA-APLIC-CP20      TO CPP020B-TAXA
           MOVE VALOR-TOT-CP20       TO CPP020B-VALOR-TOTAL.
           MOVE RESPONSAVEL-CP20     TO CPP020B-RESPONSAVEL.
           MOVE SITUACAO-CP20        TO CPP020B-SITUACAO.
           MOVE TIPO-CONTA-CP20      TO CPP020B-TIPO-CONTA
                                        CPP020B-TIPO-CONTAW.

           MOVE SEQ-CAIXA-CP20     TO CPP020B-SEQ-CAIXA

           IF CPP020B-SEQ-CAIXA > 0
              MOVE SPACES          TO MENSAGEM
              STRING "Contas a Pagar já DADO BAIXA" X"0DA0"
                     "Data => " DATA-PGTO-CP20(7:2) "/"
                                DATA-PGTO-CP20(5:2) "/"
                                DATA-PGTO-CP20(1:4) X"0DA0"
                     "Sequência Caixa => " SEQ-CAIXA-CP20
                INTO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           END-IF


           EVALUATE TIPO-CONTA-CP20
             WHEN 0 MOVE "-Temporária" TO CPP020B-TIPO-CONTA(2: 11)
             WHEN 1 MOVE "-Permanente" TO CPP020B-TIPO-CONTA(2: 11)
           END-EVALUATE.
      *    Se for um lançamento com cheque pré-datado
           IF PORTADOR-CP20 = 12
              MOVE SEQ-CP20          TO SEQ-CTA-PAGAR-CB100
              MOVE FORNEC-CP20       TO NOMINAL-A-CB100
              START CBD100 KEY IS = ALT2-CB100 INVALID KEY
                   CONTINUE
              NOT INVALID KEY
                  READ CBD100 NEXT RECORD
                  END-READ
                  OPEN INPUT CBD001 CBD003
                  MOVE CODIGO-FORN-CB100    TO CPP020B-BANCO-CH
                                               CODIGO-FORN-CB01
                  READ CBD001 INVALID KEY
                       MOVE SPACES TO NOME-BANCO-CB01
                  END-READ
                  MOVE NOME-BANCO-CB01      TO CPP020B-DESC-BANCO-CH
                  MOVE NR-CHEQUE-CB100      TO CPP020B-NR-CHEQUE-CH
                  MOVE NOMINAL-A-CB100      TO CPP020B-NOMINAL-A-CH
                                               CODIGO-CG01
                  READ CGD001 INVALID KEY
                       MOVE SPACES TO NOME-CG01
                  END-READ
                  MOVE NOME-CG01            TO CPP020B-NOME-CH
                  MOVE VALOR-CB100          TO CPP020B-VALOR-CH
                  MOVE DATA-EMISSAO-CB100   TO CPP020B-EMISSAO-CH
                  MOVE DATA-VENCTO-CB100    TO CPP020B-VENCTO-CH
                  MOVE SITUACAO-CB100       TO CPP020B-SITUACAO-CH
                                               SITUACAO-CB03
                  READ CBD003 INVALID KEY MOVE SPACES TO NOME-SIT-CB03
                  END-READ
                  MOVE NOME-SIT-CB03        TO CPP020B-DESC-SITUACAO-CH
                  CLOSE CBD001 CBD003.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       ATUALIZA-SEQ-CPD021 SECTION.
           READ CPD021 INVALID KEY
                MOVE 1 TO SEQ-CP21
                WRITE REG-CPD021 INVALID KEY
                        MOVE "CPD021"    TO CPP020B-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CPD021" TO CPP020B-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "I"         TO LOG1-OPERACAO
                        MOVE "CPD021"    TO LOG1-ARQUIVO
                        MOVE "CPP020B"    TO LOG1-PROGRAMA
                        MOVE REG-CPD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CP21
                  REWRITE REG-CPD021 INVALID KEY
                        MOVE "CPD021"    TO CPP020B-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CPD021" TO CPP020B-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                  NOT INVALID KEY
                        MOVE USUARIO-W   TO LOG1-USUARIO
                        MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                        MOVE WS-DATA-CPU TO LOG1-DATA
                        ACCEPT WS-HORA-SYS FROM TIME
                        MOVE WS-HORA-SYS TO LOG1-HORAS
                        MOVE "A"         TO LOG1-OPERACAO
                        MOVE "CPD021"    TO LOG1-ARQUIVO
                        MOVE "CPP020B"    TO LOG1-PROGRAMA
                        MOVE REG-CPD021  TO LOG1-REGISTRO
                        WRITE REG-LOG001
                        END-WRITE
                  END-REWRITE
           END-READ.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CPD020
           INITIALIZE CPP020B-DATA-BLOCK
           MOVE DATA-MOVTO-W TO CPP020B-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS        TO CPP020B-GRAVA-OBS.
       SALVAR-DADOS SECTION.
           MOVE CPP020B-DATA-MOVTO         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                   TO DATA-MOVTO-CP20.
           MOVE CPP020B-COD-FORN           TO FORNEC-CP20
           MOVE CPP020B-TIPO-FORN          TO TIPO-FORN-CP20
           MOVE CPP020B-PORTADOR           TO PORTADOR-CP20.
           MOVE CPP020B-NR-DOCTO           TO NR-DOCTO-CP20.
           MOVE CPP020B-EMISSAO-INV        TO DATA-EMISSAO-CP20
           MOVE CPP020B-VENCTO-INV         TO DATA-VENCTO-CP20
           MOVE CPP020B-DESCRICAO          TO DESCRICAO-CP20
           IF CPP020B-TIPO-MOEDA = SPACES
              MOVE "0"                     TO TIPO-MOEDA-CP20
           ELSE
              MOVE CPP020B-TIPO-MOEDA(1:1) TO TIPO-MOEDA-CP20.
           MOVE CPP020B-COD-APURACAO       TO CODREDUZ-APUR-CP20
           MOVE CPP020B-JUROS-MORA         TO JUROS-MORA-CP20
           MOVE CPP020B-MULTA-ATRASO       TO MULTA-ATRASO-CP20
           IF CPP020B-PREV-DEF = SPACES
              MOVE "0"                     TO PREV-DEF-CP20
           ELSE
              MOVE CPP020B-PREV-DEF(1:1)   TO PREV-DEF-CP20.
           MOVE CPP020B-TAXA               TO TAXA-APLIC-CP20
           MOVE CPP020B-RESPONSAVEL        TO RESPONSAVEL-CP20
           MOVE ZEROS                      TO LIBERADO-CP20
           MOVE STRING-1(13: 5)            TO DIGITADOR-CP20.
           IF CPP020B-TIPO-CONTA = SPACES
              MOVE "0" TO TIPO-CONTA-CP20
           ELSE
              MOVE CPP020B-TIPO-CONTA(1:1) TO TIPO-CONTA-CP20.
           MOVE 0101                       TO NR-PARCELA-CP20.
           MOVE CPP020B-VALOR-TOTAL        TO VALOR-TOT-CP20.
           IF PORTADOR-CP20 = 12
              IF CPP020B-BANCO-CH <> ZEROS AND
                 CPP020B-NR-CHEQUE-CH <> ZEROS
                  MOVE CPP020B-BANCO-CH      TO CODIGO-FORN-CB100
                  MOVE CPP020B-NR-CHEQUE-CH  TO NR-CHEQUE-CB100
                  MOVE CPP020B-NOMINAL-A-CH  TO NOMINAL-A-CB100
                  MOVE CPP020B-EMISSAO-CH    TO DATA-EMISSAO-CB100
                  MOVE CPP020B-VENCTO-CH     TO DATA-VENCTO-CB100
                  MOVE CPP020B-VALOR-CH      TO VALOR-CB100
                  MOVE CPP020B-SITUACAO-CH   TO SITUACAO-CB100.

       GRAVA-OBS SECTION.
           MOVE CPP020B-COD-FORN    TO FORNEC-CP22
           MOVE SEQ-CP21           TO SEQ-CP22
           MOVE CPP020B-OBSERVACAO  TO OBS-CP22
           WRITE REG-CPD022 INVALID KEY
               MOVE "CPD022"  TO CPP020B-MENSAGEM-ERRO(15: 07)
               MOVE ST-CPD022 TO CPP020B-MENSAGEM-ERRO(23: 02)
               PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
               MOVE USUARIO-W   TO LOG3-USUARIO
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               MOVE WS-DATA-CPU TO LOG3-DATA
               ACCEPT WS-HORA-SYS FROM TIME
               MOVE WS-HORA-SYS TO LOG3-HORAS
               MOVE "I"         TO LOG3-OPERACAO
               MOVE "CPD022"    TO LOG3-ARQUIVO
               MOVE "CPP020B"   TO LOG3-PROGRAMA
               MOVE REG-CPD022  TO LOG3-REGISTRO
               WRITE REG-LOG003
               END-WRITE
           END-WRITE.
      *ACHA-SEQ-CIE SECTION.
      *    MOVE DATA-MOVTO-I    TO DATA-CI10.
      *    MOVE ZEROS           TO SEQ-CI10 SEQ-CIE.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
      *       NOT AT END
      *         IF DATA-CI10 NOT = DATA-MOVTO-I MOVE "10" TO ST-CIED010
      *            MOVE SEQ-CI10 TO SEQ-CIE
      *         ELSE CONTINUE
      *      END-READ
      *    END-PERFORM.
      *GRAVA-CIE SECTION.
      *    PERFORM ACHA-SEQ-CIE.
      *    MOVE 01                  TO COD-MENS-PADRAO-CI10
      *    MOVE SPACES              TO DESCRICAO-MENS-CI10.
      *    MOVE CPP020B-DESCR-FORN   TO DESCRICAO-MENS-CI10(1: 10)
      *    MOVE DESCRICAO-CP20      TO DESCRICAO-MENS-CI10(12: 27)
      *    MOVE DATA-VENCTO-CP20 TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV            TO DATA-E
      *    MOVE DATA-E              TO DESCRICAO-MENS-CI10(40: 11)
      *    MOVE VALOR-TOT-CP20      TO VALOR-E
      *    MOVE VALOR-E             TO DESCRICAO-MENS-CI10(51: 10)
      *    MOVE DATA-MOVTO-I        TO DATA-CI10
      *    ADD 1                    TO SEQ-CIE
      *    MOVE SEQ-CIE             TO SEQ-CI10
      *    ACCEPT HORA-W            FROM TIME.
      *    MOVE HORA-W(1: 4)        TO HORA-CI10
      *    MOVE USUARIO-W           TO ORIGEM-CI10
      *
      ** Função que exerce o destinatario
      *    MOVE 1                   TO FUNCAO-DESTINO-CI10
      *
      *
      **    CODIGO DO USUARIO DESTINO (KELLO)
      *    MOVE ZEROS               TO ST-CIED010.
      *    PERFORM UNTIL ST-CIED010 = "10"
      *      WRITE REG-CIED010 INVALID KEY
      *             ADD 1 TO SEQ-CI10
      *         NOT INVALID KEY MOVE "10" TO ST-CIED010
      *    END-PERFORM.

       GRAVA-CHEQUES SECTION.
           MOVE SEQ-CP20  TO SEQ-CTA-PAGAR-CB100.
           WRITE REG-CBD100 INVALID KEY
                 REWRITE REG-CBD100 NOT INVALID KEY
                      MOVE USUARIO-W   TO LOG2-USUARIO
                      MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                      MOVE WS-DATA-CPU TO LOG2-DATA
                      ACCEPT WS-HORA-SYS FROM TIME
                      MOVE WS-HORA-SYS TO LOG2-HORAS
                      MOVE "A"         TO LOG2-OPERACAO
                      MOVE "CBD100"    TO LOG2-ARQUIVO
                      MOVE "CPP020B"    TO LOG2-PROGRAMA
                      MOVE REG-CBD100  TO LOG2-REGISTRO
                      WRITE REG-LOG002
                      END-WRITE
                 END-REWRITE
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "CBD100"    TO LOG2-ARQUIVO
                 MOVE "CPP020B"   TO LOG2-PROGRAMA
                 MOVE REG-CBD100  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
           END-WRITE.
       REGRAVA-DADOS SECTION.
           PERFORM SALVAR-DADOS.

           PERFORM VALIDA-DATA-LIMITE

           REWRITE REG-CPD020 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG3-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG3-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG3-HORAS
                 MOVE "I"         TO LOG3-OPERACAO
                 MOVE "CPD020"    TO LOG3-ARQUIVO
                 MOVE "CPP020B"    TO LOG3-PROGRAMA
                 MOVE REG-CPD020  TO LOG3-REGISTRO
                 WRITE REG-LOG003
                 END-WRITE
                 CONTINUE.
           IF PORTADOR-CP20 = 12
             IF CPP020B-BANCO-CH <> ZEROS AND
                CPP020B-NR-CHEQUE-CH <> ZEROS
                PERFORM GRAVA-CHEQUES.
           IF TIPO-CONTA-CP20 = 1
              MOVE SEQ-CP20 TO SEQ-ALTERADA
              PERFORM VERIFICA-12PERMANENTE
              IF CPP020B-OP-ALTER-PERMANEN = 2
                 PERFORM ALTERA-PERMANENTE.
       VERIFICA-DOCTO-PERMANENTE SECTION.
           MOVE CPP020B-COD-FORN   TO FORNEC-CP20 FORNEC-W
           MOVE CPP020B-NR-DOCTO   TO DOCTO-W
           MOVE ZEROS             TO SEQ-CP20 CPP020B-ERRO.
           START CPD020 KEY IS NOT < CHAVE-CP20 INVALID KEY
                MOVE "10" TO ST-CPD020.
           PERFORM UNTIL ST-CPD020 = "10"
              READ CPD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CPD020
              NOT AT END
                   IF FORNEC-CP20 NOT = FORNEC-W
                      MOVE "10" TO ST-CPD020
                   ELSE
                      IF NR-DOCTO-CP20 NOT = DOCTO-W
                         CONTINUE
                      ELSE
                         IF SITUACAO-CP20 NOT > 1
                            MOVE 1 TO CPP020B-ERRO
                            MOVE "10" TO ST-CPD020
                         END-IF
                      END-IF
                  END-IF
              END-READ
           END-PERFORM.
      * Função p/ verificar se existe o nr-docto no respectivo
      * fornecedor, porque p/ lançamentos permanentes não pode existir
      * o mesmo nr-docto p/ fornecedor

       VALIDA-DATA-LIMITE SECTION.
      *    VERIFICA SE DATA DE MOVTO OU VENCTO < 19950101
           IF DATA-MOVTO-CP20 < DATA-LIMITE
              MOVE "DATA-MOVTO-INV:"  TO CPP020B-MENSAGEM-ERRO(15: 15)
              MOVE DATA-MOVTO-CP20 TO CPP020B-MENSAGEM-ERRO(30: 08)
              PERFORM ERRO-GRAVACAO
           END-IF

           IF DATA-VENCTO-CP20 < DATA-LIMITE
              MOVE "DATA-VENCTO-INV:"  TO CPP020B-MENSAGEM-ERRO(15: 15)
              MOVE DATA-VENCTO-CP20 TO CPP020B-MENSAGEM-ERRO(30: 08)
              PERFORM ERRO-GRAVACAO
           END-IF.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO CPP020B-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE CPP020B-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CPP020B" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CPP020B-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD018 CAD019 CPD020 CPD021 CPD022 CGD001 CXD020
                 GED001 GED002 LOG001 LOG002 LOG003.
      *    CLOSE CIED001 CIED010.
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CPP020B"           to logacess-programa
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

           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
