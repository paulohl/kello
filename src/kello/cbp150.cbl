       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP150.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 05/11/1998
      *DESCRIÇÃO: Relatório de conciliação bancária
      *FUNÇÃO: Conferir se os lançamentos nos extratos bancários
      *        conferem com os lançamentos de caixa.
      *        Os débitos bancário, como CPMF, JUROS, MULTAS, TARIFAS,
      *          DESPESAS BANCÁRIAS, serão lançados no caixa automática-
      *          mente através da conciliação.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CBPX001.
           COPY CBPX003.
           COPY CBPX004.
           COPY CBPX005.
           COPY CBPX100.
           COPY CBPX101.
           COPY CXPX100.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CBPW001.
       COPY CBPW003.
       COPY CBPW004.
       COPY CBPW005.
       COPY CBPW100.
       COPY CBPW101.
       COPY CXPW100.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK             PIC 9(3).
           05  LINDET-WK          PIC X(115).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(115).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBP150.CPB".
           COPY "CBP150.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CPADAY1".
           COPY "CPDIAS1".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(65).
       01  PASSAR-USUARIO            PIC X(20)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CBD001             PIC XX       VALUE SPACES.
           05  ST-CBD003             PIC XX       VALUE SPACES.
           05  ST-CBD004             PIC XX       VALUE SPACES.
           05  ST-CBD005             PIC XX       VALUE SPACES.
           05  ST-CBD100             PIC XX       VALUE SPACES.
           05  ST-CBD101             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(3)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W = 0 (não ocorreu erro abertura) erro-w=1 (houve erro)
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-EXTRATO-I        PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-E                PIC 99/99/9999.
           05  ENCONTROU             PIC 9        VALUE ZEROS.
           05  HISTORICO-W           PIC X(30)    VALUE SPACES.
           05  HISTORICO-ANT         PIC 99       VALUE ZEROS.
           05  BANCO-HEAD            PIC 9(6)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  SEQ-CAIXA             PIC 9(3)     VALUE ZEROS.
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

       01  CAB01.
           05  EMPRESA-REL         PIC X(95)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(88)   VALUE
           "CONCILIACAO BANCARIA                              ".
           05  FILLER              PIC X(10)   VALUE "    HORA: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(115)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(115)   VALUE
           "DESCRICAO MENSAGEM
      -    "HISTORICO            NR-DOC         VALOR".
       01  LINDET.
           05  LINDET-REL          PIC X(115)   VALUE SPACES.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV    TO DATA-INV DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV
           CANCEL "GRIDAT2"
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CBD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD001.
           MOVE "CBD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD003.
           MOVE "CBD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD004.
           MOVE "CBD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD005.
           MOVE "CBD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD100.
           MOVE "CBD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CBD101.
           MOVE "CXD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD100.

           OPEN I-O   CBD100 CBD101 CXD100
           CLOSE      CBD100 CBD101 CXD100
           OPEN INPUT CBD100 CBD101 CXD100

           OPEN INPUT CBD001 CBD003 CBD004 CBD005.
           IF ST-CBD100 = "35"
              CLOSE CBD100      OPEN OUTPUT CBD100
              CLOSE CBD100      OPEN I-O CBD100
           END-IF.
           IF ST-CBD101 = "35"
              CLOSE CBD101      OPEN OUTPUT CBD101
              CLOSE CBD101      OPEN I-O CBD101
           END-IF.
           IF ST-CXD100 = "35"
              CLOSE CXD100      OPEN OUTPUT CXD100
              CLOSE CXD100      OPEN I-O CXD100
           END-IF.
           IF ST-CBD001 <> "00"
              MOVE "ERRO ABERTURA CBD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD003 <> "00"
              MOVE "ERRO ABERTURA CBD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD004 <> "00"
              MOVE "ERRO ABERTURA CBD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD005 <> "00"
              MOVE "ERRO ABERTURA CBD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD100 <> "00"
              MOVE "ERRO ABERTURA CBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CBD101 <> "00"
              MOVE "ERRO ABERTURA CBD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CBD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0
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
               WHEN GS-CONCILIAR-TRUE
                    PERFORM CONCILIAR
      *        WHEN GS-CARREGA-ULT-TRUE
      *             PERFORM CARREGA-ULTIMOS
               WHEN GS-POPUP-BANCO-TRUE
                    PERFORM CHAMAR-POPUP-BANCO
               WHEN GS-LER-BANCO-TRUE
                    PERFORM LER-BANCO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-BANCO SECTION.
           MOVE GS-BANCO TO CODIGO-FORN-CB01.
           READ CBD001 INVALID KEY MOVE SPACES TO TITULAR-CB01.
           MOVE TITULAR-CB01 TO GS-DESC-BANCO.

       CONCILIAR SECTION.
           CLOSE      CBD100 CBD101 CXD100
           OPEN I-O   CBD100 CBD101 CXD100

           ACCEPT VARIA-W FROM TIME.
           OPEN INPUT     WORK.
           MOVE ZEROS TO SEQ-W.

           IF ST-WORK = "35"
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK
           ELSE
              CLOSE       WORK
              OPEN OUTPUT WORK
              CLOSE       WORK
              OPEN I-O    WORK.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-EXTRATO       TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV              TO DATA-EXTRATO-CB101
                                         DATA-EXTRATO-I
           MOVE GS-BANCO              TO CONTA-BANCO-CB101.
           MOVE ZEROS                 TO HISTORICO-ANT SEQ-CB101.
           START CBD101 KEY IS NOT < CHAVE-CB101 INVALID KEY
                 MOVE "10" TO ST-CBD101.
           PERFORM UNTIL ST-CBD101 = "10"
             READ CBD101 NEXT RECORD AT END MOVE "10" TO ST-CBD101
               NOT AT END
                 IF CONTA-BANCO-CB101 <> GS-BANCO OR
                    DATA-EXTRATO-CB101 <> DATA-EXTRATO-I
                    MOVE "10" TO ST-CBD101
                 ELSE
                  IF RESOLVER-CB101 = 1
                     PERFORM TIPO-HISTORICO
                     MOVE SPACES TO GS-LINDET
                     MOVE HISTORICO-CB101 TO HISTORICO-ANT
                  END-IF
             END-READ
           END-PERFORM
           CLOSE      CBD100 CBD101 CXD100
           OPEN INPUT CBD100 CBD101 CXD100.
       TIPO-HISTORICO SECTION.
      *  depósitos/créditos diversos, verificar se existe no caixa
      *  caso contrário emitir aviso
           MOVE ZEROS TO ENCONTROU.
           IF HISTORICO-CB101 = 50
              MOVE DATA-EXTRATO-I     TO DATA-MOV-CX100
              MOVE ZEROS              TO SEQ-CX100
              START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                          MOVE "10" TO ST-CXD100
                NOT INVALID KEY
                PERFORM UNTIL ST-CXD100 = "10"
                 READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
                    NOT AT END
                      IF DATA-MOV-CX100 NOT = DATA-EXTRATO-I
                         MOVE "10" TO ST-CXD100
                      ELSE
                        IF TIPO-LCTO-CX100 NOT < 50
                           IF VALOR-CX100 = VALOR-CB101
                              MOVE 1 TO ENCONTROU
                           ELSE CONTINUE
                        ELSE CONTINUE
                      END-IF
                 END-READ
                END-PERFORM
                IF ENCONTROU = ZEROS PERFORM MOVER-DADOS-LINDET
                ELSE MOVE 0 TO RESOLVER-CB101
                     REWRITE REG-CBD101
                END-IF
              END-START.

      *  devolução de nosso cheque
           IF HISTORICO-CB101 = 51 PERFORM MOVER-DADOS-LINDET.

      *  débitos automáticos - home bank
           IF HISTORICO-CB101 = 08 PERFORM MOVER-DADOS-LINDET.

      *  devolução de cheque depositado
      *  tem que gerar no caixa o débito correspondente, na data deste
      *  relatório (por enquanto não está fazendo).
           IF HISTORICO-CB101 = 01 PERFORM MOVER-DADOS-LINDET.

      *  CHEQUE COMPENSADO
      *  Se cheque não existir no talão (cbd100) emitir a mensagem "ch.
      *  compensado não existe no talão". Se vencto do cheque > data-ex
      *  data-extrato emitir "cheque pre-datado compensado antes vcto",
      *  se o valor não conferir, emitir "ch.compesando não confere o
      *  valor".
      *  Gravar no talão como cheque compensado.
      *  Implementar mais tarde "ch.existe no talão mais não foi lança-
      *  do no caixa(problema terá que ler todos os lançamentos do
      *  banco em questão p/ achar o nr.docto = nr-cheque.
           IF HISTORICO-CB101 = 02
              MOVE CONTA-BANCO-CB101  TO CODIGO-FORN-CB100
              MOVE NR-CHEQUE-CB101    TO NR-CHEQUE-CB100
              READ CBD100 INVALID KEY
                  MOVE "CHEQUE COMPENSADO - NAO EXISTE NOS TALOES"
                       TO GS-LINDET(1: 60)
                       PERFORM MOVER-DADOS-LINDET
                       MOVE 4 TO SITUACAO-CB100
                       REWRITE REG-CBD100
                       END-REWRITE
                 NOT INVALID KEY
                  MOVE DATA-VENCTO-CB100 TO DATA-INV
                  CALL "GRIDAT2" USING DATA-INV
                  IF DATA-INV  > DATA-EXTRATO-I
                     MOVE "CH PRE-DATADO,COMPENSADO ANTES DO VENCTO"
                        TO GS-LINDET(1: 60)
                        PERFORM MOVER-DADOS-LINDET
                        MOVE 6 TO SITUACAO-CB100
                        REWRITE REG-CBD100
                        END-REWRITE
                  ELSE
                    IF VALOR-CB100 NOT = VALOR-CB101
                       MOVE "CHEQUE COMPENSADO NAO CONFERE O VALOR"
                          TO GS-LINDET(1: 60)
                       PERFORM MOVER-DADOS-LINDET
                       MOVE 4 TO SITUACAO-CB100
                       REWRITE REG-CBD100
                       END-REWRITE
                    END-IF
                  END-IF
                  PERFORM PROCURA-LCTO-APAGAR
              END-READ
           END-IF.

      *    GRAVA DEBITOS BANCÁRIOS NO CAIXA
           IF HISTORICO-CB101 = 3 MOVE "CPFM" TO HISTORICO-W
                                  PERFORM GRAVA-CAIXA.
           IF HISTORICO-CB101 = 4 MOVE "JUROS" TO HISTORICO-W
                                  PERFORM GRAVA-CAIXA.
           IF HISTORICO-CB101 = 5
              MOVE "DESPESAS BANCARIAS" TO HISTORICO-W
              PERFORM GRAVA-CAIXA.
           IF HISTORICO-CB101 = 6 MOVE "TARIFAS" TO HISTORICO-W
                                  PERFORM GRAVA-CAIXA.
           IF HISTORICO-CB101 = 7 MOVE "MULTAS" TO HISTORICO-W
                                  PERFORM GRAVA-CAIXA.
       PROCURA-LCTO-APAGAR SECTION.
      *    PROCURA CHEQUE COMPENSADO NO CAIXA
           MOVE DATA-EMISSAO-CB100 TO GRTIME-DATE.
           MOVE 1                  TO GRTIME-TYPE
           MOVE 5                  TO GRTIME-FUNCTION.
           MOVE 5                  TO GRTIME-DAYS.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE GRTIME-DATE-FINAL  TO GRTIME-DATE.
           MOVE 4                  TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           MOVE GRTIME-DATE-FINAL  TO DATA-MOV-CX100.
           MOVE CONTA-BANCO-CB101  TO CONTAPART-CX100
           START CXD100 KEY IS NOT < ALT-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           MOVE ZEROS TO ENCONTROU.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
               NOT AT END
                 IF CONTAPART-CX100 <> CONTA-BANCO-CB101
                           MOVE "10" TO ST-CXD100
                 ELSE
                   IF DOCUMENTO-CX100 = NR-CHEQUE-CB100
                      MOVE "10" TO ST-CXD100
                      MOVE 1    TO ENCONTROU
                   ELSE CONTINUE
                 END-IF
             END-READ
           END-PERFORM.
           IF ENCONTROU = 0
              MOVE "CH PRE-DATADO COMPENSADO, NAO CONSTA NO CAIXA"
                        TO GS-LINDET(1: 60)
              PERFORM MOVER-DADOS-LINDET.
       ACHA-SEQ-CAIXA SECTION.
           MOVE DATA-EXTRATO-I    TO DATA-MOV-CX100.
           MOVE ZEROS             TO SEQ-CX100 SEQ-CAIXA.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
             READ CXD100 NEXT RECORD AT END MOVE "10" TO ST-CXD100
               NOT AT END
                 IF DATA-MOV-CX100 <> DATA-EXTRATO-I
                              MOVE "10" TO ST-CXD100
                 ELSE MOVE SEQ-CX100 TO SEQ-CAIXA
                 END-IF
             END-READ
           END-PERFORM.
       GRAVA-CAIXA SECTION.
           PERFORM ACHA-SEQ-CAIXA.
           INITIALIZE REG-CXD100.
           MOVE DATA-EXTRATO-I       TO DATA-MOV-CX100.
           ADD 1 TO SEQ-CAIXA.
           MOVE SEQ-CAIXA            TO SEQ-CX100.
           MOVE ZEROS TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
               WRITE REG-CXD100 INVALID KEY
                     PERFORM ACHA-SEQ-CAIXA
                     ADD 1 TO SEQ-CAIXA
                     MOVE SEQ-CAIXA TO SEQ-CX100
                NOT INVALID KEY
                  MOVE "10" TO ST-CXD100
               END-WRITE
           END-PERFORM.
           MOVE HISTORICO-W          TO HISTORICO-CX100.
           MOVE 14                   TO TIPO-LCTO-CX100.
      *    MOVE ZEROS                TO CONTABIL-CX100.
           MOVE VALOR-CB101          TO VALOR-CX100.
           MOVE NR-CHEQUE-CB101      TO DOCUMENTO-CX100
           MOVE HISTORICO-CB101 TO CODIGO-CB04.
           READ CBD004 INVALID KEY MOVE 999 TO APURACAO-RED-C-CB04
                                   MOVE 999 TO APURACAO-RED-D-CB04.
           MOVE APURACAO-RED-D-CB04  TO CONTA-REDUZ-CX100
      *    MOVE CONTA-BANCO-CB101    TO CONTAPART-CX100

           MOVE HISTORICO-CB101      TO TIPO-HIST-CB05.
           MOVE CONTA-BANCO-CB101    TO BANCO-CB05.
           READ CBD005 INVALID KEY
                MOVE BANCO-CB05 TO CONTRAPART-BANCO-CB05.
           MOVE CONTRAPART-BANCO-CB05 TO CONTAPART-CX100.

           MOVE USUARIO-W            TO RESPONSAVEL-CX100
           REWRITE REG-CXD100.
      *  GRAVA CONTRAPARTIDA.
           MOVE DATA-EXTRATO-I       TO DATA-MOV-CX100.
           ADD 1 TO SEQ-CAIXA.
           MOVE SEQ-CAIXA            TO SEQ-CX100.
           MOVE ZEROS TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
               WRITE REG-CXD100 INVALID KEY
                     PERFORM ACHA-SEQ-CAIXA
                     ADD 1 TO SEQ-CAIXA
                     MOVE SEQ-CAIXA TO SEQ-CX100
                NOT INVALID KEY
                  MOVE "10" TO ST-CXD100
               END-WRITE
           END-PERFORM.

           MOVE HISTORICO-W          TO HISTORICO-CX100.
           MOVE 61                   TO TIPO-LCTO-CX100.
      *    MOVE ZEROS                TO CONTABIL-CX100.
           MOVE NR-CHEQUE-CB101      TO DOCUMENTO-CX100
           MOVE VALOR-CB101          TO VALOR-CX100.
           MOVE APURACAO-RED-C-CB04  TO CONTA-REDUZ-CX100
      *    MOVE CONTA-BANCO-CB101    TO CONTAPART-CX100
           MOVE USUARIO-W            TO RESPONSAVEL-CX100
           MOVE HISTORICO-CB101      TO TIPO-HIST-CB05.
           MOVE CONTA-BANCO-CB101    TO BANCO-CB05.
           MOVE CONTA-BANCO-CB101    TO CONTAPART-CX100.
      *    READ CBD005 INVALID KEY
      *         MOVE BANCO-CB05 TO CONTRAPART-BANCO-CB05.
      *    MOVE CONTRAPART-BANCO-CB05 TO CONTAPART-CX100.
           REWRITE REG-CXD100.
           MOVE 0 TO RESOLVER-CB101.
           REWRITE REG-CBD101.

       EMITIR-CABECALHO SECTION.
           EVALUATE HISTORICO-CB101
             WHEN 50 MOVE "DEPOSITO NAO IDENTIFICADO"
                  TO GS-LINDET(1: 60)
             WHEN 51 MOVE "CHEQUE DEVOLVIDO DA NOSSA EMPRESA"
                          TO GS-LINDET(1: 60)
             WHEN 01 MOVE "DEPOSITO ESTORNADO - DEVOLUCAO DE CHEQUE DO C
      -        "LIENTE" TO GS-LINDET(1: 60)
             WHEN 08 MOVE "DEBITOS AUTOMATICOS" TO GS-LINDET(1: 60)
             WHEN 02 CONTINUE
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           PERFORM EMITIR-CABECALHO.
           MOVE HISTORICO-CB101   TO CODIGO-CB04.
           READ CBD004 INVALID KEY MOVE SPACES TO HISTORICO-CB04.
           MOVE HISTORICO-CB04    TO GS-LINDET(61: 20).
           MOVE NR-CHEQUE-CB101   TO GS-LINDET(82: 07)
           MOVE VALOR-CB101       TO VALOR-E.
           MOVE VALOR-E           TO GS-LINDET(89: 13).
           MOVE GS-LINDET         TO LINDET-WK.
           ADD 1                  TO SEQ-W.
           MOVE SEQ-W             TO SEQ-WK.
           WRITE REG-WORK.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CHAMAR-POPUP-BANCO SECTION.
           CALL "CBP001T" USING PARAMETROS-W PASSAR-STRING.
           CANCEL "CBP001T".
           MOVE PASSAR-STRING(49: 6) TO GS-BANCO.
           PERFORM LER-BANCO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CBD100       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CBP150" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       IMPRIME-RELATORIO SECTION.
           CLOSE WORK.  OPEN INPUT WORK.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.
           MOVE ZEROS TO SEQ-WK.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           START WORK KEY IS NOT < SEQ-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                 MOVE LINDET-WK  TO LINDET-REL
                 WRITE REG-RELAT FROM LINDET
                 END-WRITE
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
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
           CLOSE CBD001 CBD003 CBD004 CBD005 CBD100 CBD101 CXD100 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
