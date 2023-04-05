       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP100.
      *DATA: 23/03/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Planejamento de reportagem
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY COPX061.
           COPY CGPX001.
           COPY CGPX005.
           COPY REPX002.
           COPY REPX003.
           COPY REPX005.
           COPY REPX006.
           COPY REPX501.
           COPY PRPX100.
           COPY PRPX101.
           COPY PRPX102.
           COPY PRPX103.
           COPY PRPX104.
           COPY PRPX105.
           COPY IEPX011.
           COPY PARX001.
           COPY LOGX001.
           SELECT WORKEVENTO ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS SEQ-EV-WK.
           SELECT WORKEQUIPE ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS SEQ-EQ-WK
                  ALTERNATE RECORD KEY IS NOME-EQ-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FUNCAO-EQ-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VEICULO-EQ-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS SEQ-EV-EQ-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS EVENTO-EV-EQ-WK
                                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CONTRATO-EV-EQ-WK
                                          WITH DUPLICATES.
           SELECT WORKVEICULO ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK3
                  RECORD KEY IS SEQ-VEI-WK.
           SELECT WORKMATERIAL ASSIGN TO VARIA-W3
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK4
                  RECORD KEY IS CODIGO-MAT-WK.
           SELECT WORKCONTRATO ASSIGN TO VARIA-W4
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK5
                  RECORD KEY IS CHAVE-CONT-WK = CONTRATO-CONT-WK
                     EVENTO-CONT-WK ITEM-CONT-WK.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY COPW061.
       COPY CGPW001.
       COPY CGPW005.
       COPY REPW002.
       COPY REPW003.
       COPY REPW005.
       COPY REPW006.
       COPY REPW501.
       COPY PRPW100.
       COPY PRPW101.
       COPY PRPW102.
       COPY PRPW103.
       COPY PRPW104.
       COPY PRPW105.
       COPY IEPW011.
       COPY PARW001.
       COPY LOGW001.

       FD  WORKEVENTO.
       01  REG-WORKEVENTO.
           05  SEQ-EV-WK           PIC 9(2).
           05  DATA-EV-WK          PIC 9(4).
           05  HORA-EV-WK          PIC X(5).
           05  COD-EVENTO-EV-WK    PIC 9(5).
           05  EVENTO-EV-WK        PIC X(15).
           05  FORM-EV-WK          PIC 9(4).
           05  PADRAO-EV-WK        PIC X.
           05  CONTRATO-EV-WK      PIC 9(4).
           05  ITEM-EV-WK          PIC 9(3).
           05  CURSO-EV-WK         PIC X(10).
           05  LOCAL-EV-WK         PIC X(15).
           05  TELAO-EV-WK         PIC 9.
           05  STUDIO-EV-WK        PIC 9.
           05  BECA-EV-WK          PIC 9.
           05  CLIP-EV-WK          PIC 9.
           05  FOTOG-EV-WK         PIC 9(2).
           05  AUXIL-EV-WK         PIC 9(2).
           05  CINEG-EV-WK         PIC 9(2).
           05  FOTOG-SEL-EV-WK     PIC 9(2).
           05  AUXIL-SEL-EV-WK     PIC 9(2).
           05  CINEG-SEL-EV-WK     PIC 9(2).
           05  NR-PLAN-WK.
               10 CIDADE-WK        PIC 9(4).
               10 MESDIA-WK        PIC 9(4).
               10 ANO-WK           PIC 9(4).
               10 SEQ-WK           PIC 9(2).


       FD  WORKEQUIPE.
       01  REG-WORKEQUIPE.
           05  SEQ-EQ-WK           PIC 9(2).
           05  CODIGO-EQ-WK        PIC 9(6).
           05  NOME-EQ-WK          PIC X(14).
           05  FUNCAO-EQ-WK        PIC 99.
           05  PADRAO-EQ-WK        PIC X.
           05  VEICULO-EQ-WK       PIC 9(3).
           05  SEQ-EV-EQ-WK        PIC 9(2).
           05  DATA-EV-EQ-WK       PIC 9(4).
           05  HORAS-EV-EQ-WK      PIC X(5).
           05  EVENTO-EV-EQ-WK     PIC X(15).
           05  CONTRATO-EV-EQ-WK   PIC 9(4).
       FD  WORKVEICULO.
       01  REG-WORKVEICULO.
           05  SEQ-VEI-WK          PIC 9(2).
           05  CODIGO-VEI-WK       PIC 9(3).
           05  DATA-SAIDA-VEI-WK   PIC 9(8).
           05  HORA-SAIDA-VEI-WK   PIC X(5).
           05  LOCAL-SAIDA-VEI-WK  PIC X(15).
       FD  WORKMATERIAL.
       01  REG-WORKMATERIAL.
           05  CODIGO-MAT-WK       PIC 9(3).
           05  QTDE-MAT-WK         PIC 9(5)V99.
       FD  WORKCONTRATO.
       01  REG-WORKCONTRATO.
           05  CONTRATO-CONT-WK    PIC 9(4).
           05  EVENTO-CONT-WK      PIC 9(5).
           05  DATA-EVENTO-CONT-WK PIC 9(8).
           05  ITEM-CONT-WK        PIC 9(3).
           05  SELECIONADO-CONT-WK PIC 9.
           05  QTDE-FORMANDOS-WK   PIC 9(06).
           05  HORARIO-WK          PIC X(10).
      *    IDENTIFICA SE O CONTRATO FAZ PARTE DO PLANEJAMENTO
      *    QUANDO O PLANEJAMENTO É DIVIDIDO POR CIDADE
       WORKING-STORAGE SECTION.
           COPY "PRP100.CPB".
           COPY "PRP100.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
      *    COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED003             PIC XX       VALUE SPACES.
           05  ST-RED005             PIC XX       VALUE SPACES.
           05  ST-RED006             PIC XX       VALUE SPACES.
           05  ST-RED501             PIC XX       VALUE SPACES.
           05  ST-PRD100             PIC XX       VALUE SPACES.
           05  ST-PRD101             PIC XX       VALUE SPACES.
           05  ST-PRD102             PIC XX       VALUE SPACES.
           05  ST-PRD103             PIC XX       VALUE SPACES.
           05  ST-PRD104             PIC XX       VALUE SPACES.
           05  ST-PRD105             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ST-WORK3              PIC XX       VALUE SPACES.
           05  ST-WORK4              PIC XX       VALUE SPACES.
           05  ST-WORK5              PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-LOG001             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  AUXILIAR              PIC 9(04).
           05  MASC-QTDE             PIC ZZZ.ZZ9.
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
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W3              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W4              PIC 9(8)     VALUE ZEROS.
      *    GRAVAR-W = 0 JÁ ESTA GRAVADO   GRAVAR-W = 1 NÃO
           05  PRIMEIRO-WORK         PIC 9        VALUE ZEROS.
      *    0-SIM  1-NÃO
           05  NR-PLAN-W             PIC 9(14)    VALUE ZEROS.
           05  NOME-W                PIC X(5)     VALUE SPACES.
           05  NOME-W1               PIC X(5)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  I                     PIC 9999     VALUE ZEROS.
           05  SEQ-W                 PIC XX       VALUE SPACES.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ,ZZ.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  CIDADE-W              PIC 9(4)     VALUE ZEROS.
           05  DIAMES-E              PIC ZZ/ZZ.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ-EQUIPE        PIC 9(2)     VALUE ZEROS.
      *    ULT-SEQ-EQUIPE - ARMAZENA QUAL ULT.SEQUENCIA EQUIPE UTILIZADA
           05  ULT-SEQ-VEICULO       PIC 9(2)     VALUE ZEROS.
      *    ULT-SEQ-VEICULO- ARMAZENA QUAL ULT.SEQUEN.VEICULO UTILIZADA
           05  PASSAR-STRING-1       PIC X(50).
           05 mensagem               pic x(200).
           05 tipo-msg               pic x(01).
           05 resp-msg               pic x(01).
           05 JA                     PIC X(01).
           05 EM-BRANCO              PIC 9(01).
           05 SAIR                   PIC X(01).
           05 ULTIMO                 PIC 9(04).
           05 INICIO                 PIC 9(04).
           05 QUANTIDADE             PIC 9(02).
           05 WS-STATUS-REVENDIDO    PIC 9(02).
           05 WS-STATUS-ANALISE      PIC 9(02).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).
          05 FILLER                    PIC X(13).

       01 WS-HORA-SYS.
          05 WS-HO-SYS                 PIC 9(02).
          05 WS-MI-SYS                 PIC 9(02).
          05 WS-SE-SYS                 PIC 9(02).
          05 WS-MS-SYS                 PIC 9(02).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE "RED002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED002.
           MOVE "RED003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED003.
           MOVE "RED005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED005.
           MOVE "RED006"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED006.
           MOVE "RED501"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED501.
           MOVE "PRD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD100.
           MOVE "PRD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD101.
           MOVE "PRD102"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD102.
           MOVE "PRD103"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD103.
           MOVE "PRD104"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD104.
           MOVE "PRD105"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PRD105.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "PAR001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "LOG001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG001.

           OPEN I-O   CAD010 COD003 COD040 COD060 CGD001 RED002 RED005
                      RED006 RED003 RED501 CGD005 COD061 IED011 PAR001

           CLOSE      CAD010 COD003 COD040 COD060 CGD001 RED002 RED005
                      RED006 RED003 RED501 CGD005 COD061 IED011 PAR001.

           OPEN INPUT CAD010 COD003 COD040 COD060 CGD001 RED002 RED005
                      RED006 RED003 RED501 CGD005 COD061 IED011 PAR001.

           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD005 <> "00"
              MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED002 <> "00"
              MOVE "ERRO ABERTURA RED002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED003 <> "00"
              MOVE "ERRO ABERTURA RED003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED005 <> "00"
              MOVE "ERRO ABERTURA RED005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED006 <> "00"
              MOVE "ERRO ABERTURA RED006: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED006 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED501 <> "00"
              MOVE "ERRO ABERTURA RED501: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED501 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001

           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
      *        WHEN GS-PRINTER-FLG-TRUE
      *             PERFORM IMPRIME-RELATORIO
               WHEN GS-VERIFICA-CODIGO-TRUE
                    PERFORM VERIFICA-CODIGO
               WHEN GS-VERIFICA-SEQ-VEIC-TRUE
                    PERFORM VERIFICA-SEQ-VEICULO
               WHEN GS-CARREGA-CURSOS-TRUE
                    PERFORM CARREGA-CURSOS
               WHEN GS-ENTRAR-EQUIPE-TRUE
                    PERFORM GRAVA-WORK-EQUIPE
                    PERFORM CARREGA-LISTA-EQUIPE
                    EVALUATE FUNCAO-EQ-WK
                      WHEN 1 ADD 1 TO FOTOG-SEL-EV-WK
                      WHEN 2 ADD 1 TO CINEG-SEL-EV-WK
                      WHEN 5 ADD 1 TO AUXIL-SEL-EV-WK
                      WHEN OTHER CONTINUE
                    END-EVALUATE
                    REWRITE REG-WORKEVENTO
                    END-REWRITE
               WHEN GS-ENTRAR-VEICULO-TRUE
                    PERFORM GRAVA-WORK-VEICULO
                    PERFORM CARREGA-LISTA-VEICULO
               WHEN GS-ENTRAR-CONTRATO-TRUE
                    PERFORM GRAVA-SELECAO-CONTRATO
               WHEN GS-ENTRAR-MATERIAL-TRUE
                    PERFORM GRAVA-WORK-MATERIAL-SELECAO
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM GRAVAR-PRD101
                    PERFORM GRAVAR-PRD100
                    PERFORM GRAVAR-PRD102
                    PERFORM GRAVAR-PRD103
                    PERFORM GRAVAR-PRD104
                    PERFORM GRAVAR-PRD105
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-PLANEJAMENTO
               WHEN GS-SELECAO-MATERIAL-TRUE
                    PERFORM SELECAO-MATERIAL
               WHEN GS-POPUP-GERAL-TRUE
                    PERFORM CARREGAR-POPUP
               WHEN GS-CARREGAR-EVENTO-TRUE
                    PERFORM CARREGAR-LISTA-EVENTO
               WHEN GS-CARREGAR-EVE-CONT-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGAR-LISTA-EVENTO
               WHEN GS-CARREGAR-EQUIPE-TRUE
                    PERFORM CARREGA-EQUIPE-ASELECIONAR
               WHEN GS-LE-EQUIPE-TRUE
                    PERFORM LER-EQUIPE
               WHEN GS-LE-FUNCAO-TRUE
                    PERFORM LER-FUNCAO
               WHEN GS-LE-VEICULO-TRUE
                    PERFORM LER-VEICULO
               WHEN GS-LE-HOTEL-TRUE
                    PERFORM LER-HOTEL
               WHEN GS-ORDEM-EQUIPE-TRUE
                    PERFORM CARREGAR-ORDEM-EQUIPE
               WHEN GS-EXCLUI-IT-EVENTO-TRUE
                    PERFORM EXCLUI-ITEM-EVENTO
               WHEN GS-EXCLUI-IT-EQUIPE-TRUE
                    PERFORM EXCLUI-ITEM-EQUIPE
               WHEN GS-EXCLUI-IT-VEIC-TRUE
                    PERFORM EXCLUI-ITEM-VEICULO
               WHEN GS-ITEM-EQUIPE-SEL-TRUE
                    PERFORM ITEM-SELECIONADO-EQUIPE
               WHEN GS-PEGAR-PROXIMO-TRUE
                    PERFORM PEGAR-PROXIMO
      *        WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM GRAVA-WORK
      *             PERFORM CARREGA-LISTA
      *        WHEN GS-CARREGA-LISTA-FLG-TRUE
      *             PERFORM CARREGA-LISTA
      *        WHEN GS-ITEM-SELECIONADO-TRUE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       PEGAR-PROXIMO SECTION.
           OPEN INPUT PRD100

           INITIALIZE REG-PRD100
                      GS-SEQ-PLAN
           MOVE GS-NR-PLAN(1:4) TO CIDADE-PR100
           MOVE GS-NR-PLAN(5:4) TO MESDIA-PR100
           MOVE GS-ANO-PLAN     TO ANO-PR100

           START PRD100 KEY IS NOT LESS CHAVE-PR100 INVALID KEY
                MOVE "10" TO ST-PRD100.

           PERFORM UNTIL ST-PRD100 = "10"
                READ PRD100 NEXT AT END
                     MOVE "10" TO ST-PRD100
                NOT AT END
                     IF GS-NR-PLAN(1:4) <> CIDADE-PR100 OR
                        GS-NR-PLAN(5:4) <> MESDIA-PR100 OR
                        GS-ANO-PLAN     <> ANO-PR100
                        MOVE "10"          TO ST-PRD100
                     ELSE
                        IF SEQ-PR100 > GS-SEQ-PLAN
                           MOVE SEQ-PR100  TO GS-SEQ-PLAN
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           CLOSE PRD100

           ADD 1 TO GS-SEQ-PLAN.
       EXCLUI-ITEM-EVENTO SECTION.
           MOVE GS-LINDET(1:2) TO SEQ-EV-WK
           READ WORKEVENTO INVALID KEY
               MOVE "Evento Não Encontrado" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE "Você Tem certeza da Exclusão do Evento ?" TO
               MENSAGEM
               MOVE "Q" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               IF RESP-MSG EQUAL "S"
                  MOVE 0 TO GS-FLAG-CRITICA
                  CLOSE      COD060
                  OPEN I-O   COD060
                  DELETE WORKEVENTO INVALID KEY
                       MOVE "NÃO ENCONTREI O WORKEVENTO" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                  NOT INVALID KEY
                       PERFORM EXCLUIR-EQUIPE
                       MOVE CONTRATO-EV-WK  TO NR-CONTRATO-CO60
                       MOVE ITEM-EV-WK      TO ITEM-CO60
                       READ COD060 INVALID KEY
                            MOVE "NÃO ENCONTREI O COD060" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                       NOT INVALID KEY
                            MOVE ZEROS  TO NR-PLANEJ-CO60
                            REWRITE REG-COD060 INVALID KEY
                                 MOVE "Erro de Regravação...COD060" TO
                                 MENSAGEM
                                 MOVE "C" TO TIPO-MSG
                                 PERFORM EXIBIR-MENSAGEM
                            END-REWRITE
                       END-READ
                  END-DELETE
                  CLOSE      COD060
                  OPEN INPUT COD060.


           PERFORM CARREGAR-ORDEM-EQUIPE.

           MOVE "SELECIONAR-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUIR-EQUIPE SECTION.
           INITIALIZE REG-WORKEQUIPE
           MOVE EVENTO-EV-WK TO EVENTO-EV-EQ-WK
           START WORKEQUIPE KEY IS NOT LESS EVENTO-EV-EQ-WK INVALID KEY
               MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
               READ WORKEQUIPE NEXT AT END
                   MOVE "10" TO ST-WORK2
               NOT AT END
                   IF EVENTO-EV-WK <> EVENTO-EV-EQ-WK
                       MOVE "10" TO ST-WORK2
                   ELSE
                       DELETE WORKEQUIPE INVALID KEY
                           MOVE "Erro de Exclusão...WORKEQUIPE" TO
                           MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                       END-DELETE
                   END-IF
               END-READ
           END-PERFORM.

       CARREGA-CURSOS SECTION.
           INITIALIZE REG-COD061
           MOVE GS-LINDET2(1: 4)     TO NR-CONTRATO-CO61
           MOVE GS-LINDET2(31: 3)    TO ITEM-CO61
           START COD061 KEY IS NOT LESS CHAVE-CO61 INVALID KEY
               MOVE "10" TO ST-COD061.

           PERFORM UNTIL ST-COD061 = "10"
               READ COD061 NEXT RECORD AT END
                   MOVE "10" TO ST-COD061
               NOT AT END
                   IF GS-LINDET2(1:4)  <> NR-CONTRATO-CO61 OR
                      GS-LINDET2(31:3) <> ITEM-CO61
                      MOVE "10" TO ST-COD061
                   ELSE
                      MOVE GS-LINDET2(1:4) TO GS-LINDET5(1:4)
                      MOVE CURSO-CO61      TO CODIGO-IE11
                      READ IED011 INVALID KEY
                           MOVE "------"   TO NOME-REDUZ-IE11
                      END-READ
                      MOVE NOME-REDUZ-IE11 TO GS-LINDET5(6:15)
                      MOVE TURMA-CO61 TO GS-LINDET5(22:2)
                      MOVE "INSERE-LIST-CURSO" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

      *----------------------------------------------------
       LER-EQUIPE SECTION.
           MOVE GS-COD-EQ  TO CODIGO-CG01 CODIGO-CG05.
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01   TO GS-NOME-EQ.
           READ CGD005 INVALID KEY MOVE ZEROS TO FUNCAO-CG05
                                   MOVE SPACES TO PADRAO-CG05.
           MOVE FUNCAO-CG05 TO GS-FUNCAO-EQ
           PERFORM LER-FUNCAO.
           MOVE PADRAO-CG05 TO GS-PADRAO-EQ.
       LER-VEICULO SECTION.
           MOVE GS-CODIGO-VEI  TO CODIGO-RE06.
           READ RED006 INVALID KEY MOVE SPACES TO VEICULO-RE06.
           MOVE VEICULO-RE06   TO GS-DESC-VEI.
       LER-FUNCAO SECTION.
           MOVE GS-FUNCAO-EQ   TO CODIGO-RE02.
           READ RED002 INVALID KEY MOVE SPACES TO DESCRICAO-RE02.
           MOVE DESCRICAO-RE02 TO GS-DESC-FUNCAO-EQ.
       LER-HOTEL SECTION.
           MOVE GS-CODIGO-HOTEL TO CODIGO-RE05.
           READ RED005 INVALID KEY MOVE SPACES TO HOTEL-RE05.
           MOVE HOTEL-RE05      TO GS-DESC-HOTEL.
           MOVE FONE1-RE05      TO GS-FONE-HOTEL.
       CARREGAR-POPUP SECTION.
           EVALUATE GS-TIPO-POPUP
             WHEN 1 PERFORM CARREGA-POPUP-EQUIPE
             WHEN 2 CALL   "REP002T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "REP002T"
                    MOVE PASSAR-STRING-1(22: 2) TO GS-FUNCAO-EQ
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-FUNCAO-EQ
             WHEN 3 CALL   "REP006T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "REP006T"
                    MOVE PASSAR-STRING-1(32: 3) TO GS-CODIGO-VEI
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-VEI
             WHEN 4 CALL   "REP005T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "REP005T"
                    MOVE PASSAR-STRING-1(37: 4)  TO GS-CODIGO-HOTEL
                    PERFORM LER-HOTEL
             WHEN 5
                    MOVE DATA-INI TO PASSAR-STRING-1(1: 8)
                    CALL   "REP501T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "REP501T"
                    MOVE PASSAR-STRING-1(42: 6) TO GS-COD-EQ
                    MOVE PASSAR-STRING-1(12: 29) TO GS-NOME-EQ
           END-EVALUATE.
       ITEM-SELECIONADO-EQUIPE SECTION.
           MOVE GS-LINDET4(33: 6)   TO GS-COD-EQ
           MOVE GS-LINDET4(1: 30)   TO GS-NOME-EQ
           MOVE GS-LINDET4(40: 2)   TO GS-FUNCAO-EQ
           PERFORM LER-FUNCAO
           MOVE GS-LINDET4(44: 1) TO GS-PADRAO-EQ.

       CARREGA-POPUP-EQUIPE SECTION.
           MOVE GS-LINDET4(1: 5) TO NOME-W
           MOVE ZEROS TO SAIR-W
           PERFORM VARYING I FROM 1 BY 1 UNTIL SAIR-W = 1
            IF I = 6 MOVE 1 TO SAIR-W
            ELSE
             MOVE NOME-W(I: 1) TO LETRA
             IF LETRA = " " MOVE 1 TO SAIR-W
             END-IF
            END-IF
           END-PERFORM.
      *    MOVE "CLEAR-LIST-POPUP-EQUIPE" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM

           SUBTRACT 2 FROM I.
           MOVE NOME-W(1: I) TO NOME-CG01.
           START CGD001 KEY IS NOT < NOME-CG01 INVALID KEY
                 MOVE "10" TO ST-CGD001.
           MOVE ZEROS TO GS-CONT-EQUIPE.
           MOVE SPACES TO GS-LINDET4.
           PERFORM UNTIL ST-CGD001 = "10"
             READ CGD001 NEXT RECORD AT END MOVE "10" TO ST-CGD001
             NOT AT END
              IF T-FOTOG-CG01 = ZEROS AND
                 T-CINEG-CG01 = ZEROS CONTINUE
              ELSE
               MOVE NOME-CG01(1: I)   TO NOME-W1
               IF NOME-W1 <> NOME-W CONTINUE
               ELSE
                MOVE NOME-CG01         TO GS-LINDET4(01: 30)
                MOVE CODIGO-CG01       TO GS-LINDET4(33: 06)
                MOVE CODIGO-CG01       TO CODIGO-CG05
                READ CGD005 INVALID KEY MOVE ZEROS TO FUNCAO-CG05
                                        MOVE SPACES TO PADRAO-CG05
                END-READ
                MOVE FUNCAO-CG05       TO GS-LINDET4(40: 4)
                MOVE PADRAO-CG05       TO GS-LINDET4(44: 2)
                MOVE "INSERE-LIST-POPUP-EQUIPE" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
               END-IF
              END-IF
             END-READ
           END-PERFORM.


      *----------------------------------------------------
       VERIFICA-CODIGO SECTION.
           IF PRIMEIRO-WORK = 0
              PERFORM ABRE-ARQUIVO-WORK
           ELSE
             CLOSE WORKEVENTO
                   WORKEQUIPE
                   WORKCONTRATO
                   WORKVEICULO
                   WORKMATERIAL
             DELETE FILE WORKEVENTO
                         WORKEQUIPE
                         WORKCONTRATO
                         WORKVEICULO
                         WORKMATERIAL
             PERFORM ABRE-ARQUIVO-WORK
           END-IF.

           MOVE "Gerando arquivo"  TO GS-MENSAGEM
           MOVE "EXIBE-MENSAGEM"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-NR-PLAN         TO NR-PLAN-W(1: 8)
           MOVE GS-ANO-PLAN        TO NR-PLAN-W(9: 4)
           MOVE GS-SEQ-PLAN        TO NR-PLAN-W(13: 2)
           MOVE "CLEAR-OBJECT-PRINCIPAL" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE NR-PLAN-W(1: 8)    TO GS-NR-PLAN
           MOVE NR-PLAN-W(9: 4)    TO GS-ANO-PLAN
           MOVE NR-PLAN-W(13: 2)   TO GS-SEQ-PLAN

           MOVE GS-NR-PLAN(1: 4)   TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-CIDADE
           MOVE GS-NR-PLAN(5: 4)   TO GRTIME-DATE(5: 4)
           MOVE GS-ANO-PLAN        TO GRTIME-DATE(1: 4).
           PERFORM VERIFICA-DATA.
           MOVE "UNSHOW-MENSAGEM"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ABRE-ARQUIVO-WORK SECTION.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORKEVENTO
           CLOSE       WORKEVENTO
           OPEN I-O    WORKEVENTO.

           ADD 1 TO VARIA-W.
           MOVE VARIA-W TO VARIA-W1.
           OPEN OUTPUT WORKEQUIPE
           CLOSE       WORKEQUIPE
           OPEN I-O    WORKEQUIPE.

           ADD 1 TO VARIA-W1.
           MOVE VARIA-W1 TO VARIA-W2.
           OPEN OUTPUT WORKVEICULO
           CLOSE       WORKVEICULO
           OPEN I-O    WORKVEICULO.

           ADD 1 TO VARIA-W2.
           MOVE VARIA-W2 TO VARIA-W3.
           OPEN OUTPUT WORKMATERIAL
           CLOSE       WORKMATERIAL
           OPEN I-O    WORKMATERIAL.

           ADD 1 TO VARIA-W3.
           MOVE VARIA-W3 TO VARIA-W4.
           OPEN OUTPUT WORKCONTRATO
           CLOSE       WORKCONTRATO
           OPEN I-O    WORKCONTRATO.

           MOVE 1 TO PRIMEIRO-WORK.
       VERIFICA-DATA SECTION.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS     TO ULT-SEQ-EQUIPE ULT-SEQ-VEICULO.
           MOVE 2         TO GRTIME-TYPE.
           MOVE 7         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-DATE-FINAL = ZEROS
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE PERFORM VERIFICA-DATA1.

       VERIFICA-DATA1 SECTION.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA
             PERFORM VERIFICA-PLANEJAMENTO.
      *      verifica se o planejamento já está cadastrado
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GRTIME-DATE   TO DATA-INI DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO GS-DATA-INI

           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL   TO DATA-FIM DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV      TO GS-DATA-FIM.
       CHAMA-GRTIME-ADAY SECTION.
      *    VERIFICA ULTIMO DIA DA SEMANA SOLICITADA
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 6         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".
      *----------------------------------------------------------
       VERIFICA-CONTRATO SECTION.
      *    LISTA QUAIS SÃO OS CONTRATOS DA SEMANA NA CIDADE SELECIONADA
           INITIALIZE REG-WORKCONTRATO.
           MOVE DATA-INI  TO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
                READ COD060 NEXT RECORD AT END
                     MOVE "10" TO ST-COD060
                NOT AT END
                     IF DATAREALIZA-CO60 > DATA-FIM
                        MOVE "10" TO ST-COD060
                     ELSE
                        MOVE ZEROS                TO SELECIONADO-CONT-WK
                        MOVE CODEVENTO-CO60       TO EVENTO-CONT-WK
                        MOVE DATAREALIZA-CO60     TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV             TO DATA-EVENTO-CONT-WK
                        MOVE ITEM-CO60            TO ITEM-CONT-WK
                        MOVE QT-PARTICIPANTE-CO60 TO QTDE-FORMANDOS-WK
                        MOVE HORARIO-CO60         TO HORARIO-WK
                        MOVE NR-CONTRATO-CO60     TO CONTRATO-CONT-WK
                                                     NR-CONTRATO-CO40
                        READ COD040 INVALID KEY
                             MOVE ZEROS TO CIDADE-CO40
                        END-READ
                        IF CIDADE-CO40 <> CIDADE
                           CONTINUE
                        ELSE
                           IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                              WS-STATUS-ANALISE
                              READ WORKCONTRATO INVALID KEY
                                   WRITE REG-WORKCONTRATO
                                   END-WRITE
                              END-READ
                           END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM.
           PERFORM MOVER-CONTRATO-LISTA.
       MOVER-CONTRATO-LISTA SECTION.
      *    MOVE OS CONTRATOS DA SEMANA/CIDADE P/ A LISTA DE SELEÇÃO
           MOVE "SHOW-TELA-CONTRATO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CONTRATO-CONT-WK.
           START WORKCONTRATO KEY IS NOT < CHAVE-CONT-WK
                 INVALID KEY MOVE "10" TO ST-WORK5.
           PERFORM UNTIL ST-WORK5 = "10"
             READ WORKCONTRATO NEXT RECORD AT END MOVE "10" TO ST-WORK5
              NOT AT END
                MOVE CONTRATO-CONT-WK     TO GS-LINDET2(1: 4)
                MOVE EVENTO-CONT-WK       TO GS-LINDET2(6: 5)
                                             CODIGO-CO03
                READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                END-READ
                MOVE NOME-CO03(1: 20)     TO GS-LINDET2(12: 21)
                MOVE ITEM-CONT-WK         TO GS-LINDET2(33: 3)
                MOVE DATA-EVENTO-CONT-WK  TO DATA-E
                MOVE DATA-E               TO GS-LINDET2(37: 10)
                MOVE HORARIO-WK           TO GS-LINDET2(48:11)
                MOVE QTDE-FORMANDOS-WK    TO MASC-QTDE
                MOVE MASC-QTDE            TO GS-LINDET2(59:7)
                MOVE "INSERE-LIST-CONTRATO" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
       GRAVA-SELECAO-CONTRATO SECTION.
      *    SETA A VAR SELECIOANDO-CONT-WK = 1 CASO CONTRATO SELECIONADO
           MOVE GS-LINDET2(1: 4) TO CONTRATO-CONT-WK.
           MOVE GS-LINDET2(6: 5) TO EVENTO-CONT-WK.
           MOVE GS-LINDET2(33: 3) TO ITEM-CONT-WK
           READ WORKCONTRATO INVALID KEY CONTINUE
             NOT INVALID KEY
                MOVE 1 TO SELECIONADO-CONT-WK
                REWRITE REG-WORKCONTRATO
                END-REWRITE
           END-READ.
      *    PERFORM GRAVA-WORK
      *    PERFORM CARREGAR-LISTA-EVENTO.
      *----------------------------------------------------------

      /  DADOS DO EVENTO
       GRAVA-WORK SECTION.
           INITIALIZE REG-WORKEVENTO.
           MOVE DATA-INI  TO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END
                  MOVE "10" TO ST-COD060
             NOT AT END
                 IF DATAREALIZA-CO60 > DATA-FIM
                    MOVE "10" TO ST-COD060
                 ELSE
                    MOVE NR-CONTRATO-CO60     TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY
                         INITIALIZE REG-COD040
                    END-READ
                    IF CIDADE-CO40 <> CIDADE
                       CONTINUE
                    ELSE
                       IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                                         WS-STATUS-ANALISE
                          IF GS-SEQ-PLAN <> ZEROS
                             MOVE NR-CONTRATO-CO60 TO CONTRATO-CONT-WK
                             MOVE CODEVENTO-CO60   TO EVENTO-CONT-WK
                             MOVE DATAREALIZA-CO60 TO DATA-INV
                             CALL "GRIDAT1" USING DATA-INV
                             MOVE DATA-INV  TO DATA-EVENTO-CONT-WK
                             MOVE ITEM-CO60 TO ITEM-CONT-WK
                             READ WORKCONTRATO INVALID KEY
                                  CONTINUE
                             NOT INVALID KEY
                                  IF SELECIONADO-CONT-WK = 1
                                     PERFORM MOVER-DADOS-WORK-EVENTO
                                     ADD 1      TO SEQ-EV-WK
                                     WRITE REG-WORKEVENTO
                                     END-WRITE
                                  END-IF
                             END-READ
                          ELSE
                             PERFORM MOVER-DADOS-WORK-EVENTO
                             ADD 1      TO SEQ-EV-WK
                             WRITE REG-WORKEVENTO
                             END-WRITE
                          END-IF
                       END-IF
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.
       MOVER-DADOS-WORK-EVENTO SECTION.
           MOVE DATAREALIZA-CO60(5: 2) TO DATA-EV-WK(3: 2)
           MOVE DATAREALIZA-CO60(7: 2) TO DATA-EV-WK(1: 2)
           MOVE HORARIO-CO60       TO HORA-EV-WK
           MOVE CODEVENTO-CO60     TO CODIGO-CO03 COD-EVENTO-EV-WK
           READ COD003 INVALID KEY
                MOVE SPACES TO NOME-CO03
           END-READ
           MOVE NOME-CO03          TO EVENTO-EV-WK
           MOVE NR-CONTRATO-CO60   TO NR-CONTRATO-CO40
                                      CONTRATO-EV-WK
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
           END-READ
           MOVE PADRAO-CO40        TO PADRAO-EV-WK
           IF QT-PARTICIPANTE-CO60 = ZEROS
              MOVE QTDE-FORM-CO40  TO FORM-EV-WK
           ELSE
              MOVE QT-PARTICIPANTE-CO60 TO FORM-EV-WK
           END-IF
           MOVE ITEM-CO60          TO ITEM-EV-WK
           MOVE IDENTIFICACAO-CO40 TO CURSO-EV-WK
           MOVE LOCAL-CO60         TO LOCAL-EV-WK
           MOVE QT-TELAO-CO60      TO TELAO-EV-WK
           MOVE BECA-CO60          TO BECA-EV-WK
           MOVE CLIP-CO60          TO CLIP-EV-WK
           MOVE ZEROS              TO STUDIO-EV-WK
           MOVE ZEROS              TO FOTOG-EV-WK AUXIL-EV-WK
                CINEG-EV-WK FOTOG-SEL-EV-WK AUXIL-SEL-EV-WK
                                      CINEG-SEL-EV-WK.
       CARREGAR-LISTA-EVENTO SECTION.
           MOVE "CLEAR-LIST-BOX-EVENTO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO GS-CONT-EV
           MOVE ZEROS TO SEQ-EV-WK.
           START WORKEVENTO KEY IS NOT < SEQ-EV-WK INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORKEVENTO NEXT RECORD AT END
                  MOVE "10" TO ST-WORK1
             NOT AT END
                  MOVE SPACES         TO GS-LINDET
                  MOVE SEQ-EV-WK      TO GS-LINDET(1: 3)
                  MOVE DATA-EV-WK     TO DIAMES-E
                  MOVE DIAMES-E       TO GS-LINDET(4: 6)
                  MOVE HORA-EV-WK     TO GS-LINDET(10: 6)
                  MOVE COD-EVENTO-EV-WK TO GS-LINDET(16: 5)
                  MOVE "-"            TO GS-LINDET(21: 1)
                  MOVE EVENTO-EV-WK   TO GS-LINDET(22: 16)
                  MOVE FORM-EV-WK     TO GS-LINDET(38: 5)
                  MOVE PADRAO-EV-WK   TO GS-LINDET(43: 2)
                  MOVE CONTRATO-EV-WK TO GS-LINDET(45: 5)
                  MOVE CURSO-EV-WK    TO GS-LINDET(50: 11)
                  MOVE LOCAL-EV-WK    TO GS-LINDET(61: 16)
                  MOVE TELAO-EV-WK    TO GS-LINDET(77: 2)
                  MOVE STUDIO-EV-WK   TO GS-LINDET(79: 2)
                  MOVE BECA-EV-WK     TO GS-LINDET(81: 2)
                  MOVE CLIP-EV-WK     TO GS-LINDET(83: 2)
                  MOVE FOTOG-EV-WK    TO GS-LINDET(87: 3)
                  MOVE AUXIL-EV-WK    TO GS-LINDET(90: 3)
                  MOVE CINEG-EV-WK    TO GS-LINDET(93: 3)
                  MOVE FOTOG-SEL-EV-WK    TO GS-LINDET(98: 3)
                  MOVE AUXIL-SEL-EV-WK    TO GS-LINDET(101: 3)
                  MOVE CINEG-SEL-EV-WK    TO GS-LINDET(104: 3)
                  MOVE "INSERE-LIST-EVENTO" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
      *-----------------------------------------------------------
      *  DADOS DA EQUIPE
       GRAVA-WORK-EQUIPE SECTION.
           MOVE GS-LINDET(1: 2) TO SEQ-W.
           IF SEQ-W = SPACES MOVE "00" TO SEQ-W.
           MOVE SEQ-W     TO SEQ-EV-WK.
           ADD 1 TO ULT-SEQ-EQUIPE
           READ WORKEVENTO INVALID KEY
                CONTINUE
           NOT INVALID KEY
                MOVE ULT-SEQ-EQUIPE    TO SEQ-EQ-WK
                IF GS-PRE-EQUIPE = 1
                   MOVE GS-LINDET3(40: 6) TO CODIGO-EQ-WK
                   MOVE GS-LINDET3(9: 30) TO NOME-EQ-WK
                   IF GS-FUNCAO-EQ = ZEROS
                      MOVE GS-LINDET3(1: 2) TO FUNCAO-EQ-WK
                   ELSE
                      MOVE GS-FUNCAO-EQ TO FUNCAO-EQ-WK
                   END-IF
                   MOVE GS-LINDET3(47: 1) TO PADRAO-EQ-WK
                ELSE
                   MOVE GS-COD-EQ        TO CODIGO-EQ-WK
                   MOVE GS-NOME-EQ        TO NOME-EQ-WK
                   MOVE GS-FUNCAO-EQ      TO FUNCAO-EQ-WK
                   MOVE GS-PADRAO-EQ      TO PADRAO-EQ-WK
                END-IF
                MOVE GS-VEICULO-EQ     TO VEICULO-EQ-WK
                MOVE SEQ-EV-WK         TO SEQ-EV-EQ-WK
                MOVE DATA-EV-WK        TO DATA-EV-EQ-WK
                MOVE HORA-EV-WK        TO HORAS-EV-EQ-WK
                MOVE EVENTO-EV-WK      TO EVENTO-EV-EQ-WK
                MOVE CONTRATO-EV-WK    TO CONTRATO-EV-EQ-WK
                WRITE REG-WORKEQUIPE
                END-WRITE
           END-READ.

       CARREGA-EQUIPE-ASELECIONAR SECTION.
           MOVE "CLEAR-LIST-BOX6" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-CONT.
           MOVE DATA-INI          TO DATA-SEG-RE501
           MOVE GS-NR-PLAN(1: 4)  TO CIDADE-RE501 CIDADE-W.
           MOVE ZEROS             TO FUNCAO-RE501 CODIGO-RE501.

           START RED501 KEY IS NOT < CHAVE-RE501
                    INVALID KEY MOVE "10" TO ST-RED501.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-RED501 = "10"
              READ RED501 NEXT RECORD AT END
                   MOVE "10" TO ST-RED501
              NOT AT END
                   IF DATA-SEG-RE501 > DATA-INI OR
                      CIDADE-RE501 <> CIDADE-W
                      MOVE "10" TO ST-RED501
                   ELSE
                      ADD 1 TO GS-CONT
                      MOVE FUNCAO-RE501      TO GS-LINDET3(01: 2)
                                                CODIGO-RE02
                      READ RED002 INVALID KEY
                           MOVE SPACES TO DESCRICAO-RE02
                      END-READ
                      MOVE "-"               TO GS-LINDET3(3: 1)
                      MOVE DESCRICAO-RE02    TO GS-LINDET3(4: 4)
                      MOVE CODIGO-RE501      TO CODIGO-CG01
                      READ CGD001 INVALID KEY
                           MOVE SPACES TO NOME-CG01
                      END-READ
                      MOVE NOME-CG01         TO GS-LINDET3(9: 31)
                      MOVE CODIGO-RE501      TO GS-LINDET3(40: 06)
                      MOVE PADRAO-RE501      TO GS-LINDET3(47: 1)
                      MOVE "INSERE-LIST-SEL-EQUIPE" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM.

       CARREGA-LISTA-EQUIPE SECTION.
           IF ULT-SEQ-EQUIPE = 1
              MOVE "CLEAR-LIST-BOX-EQUIPE" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              PERFORM SET-UP-FOR-REFRESH-SCREEN
              PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES            TO GS-LINDET1
           MOVE SEQ-EQ-WK         TO GS-LINDET1(1: 3)
           MOVE CODIGO-EQ-WK      TO GS-LINDET1(4: 7)
           MOVE NOME-EQ-WK        TO GS-LINDET1(11: 31)
           MOVE FUNCAO-EQ-WK      TO GS-LINDET1(42: 2) CODIGO-RE02
           READ RED002 INVALID KEY
                MOVE SPACES TO DESCRICAO-RE02.
           MOVE "-"               TO GS-LINDET1(44: 1)
           MOVE DESCRICAO-RE02    TO GS-LINDET1(45: 4)
           MOVE PADRAO-EQ-WK      TO GS-LINDET1(50: 2)
           MOVE VEICULO-EQ-WK     TO GS-LINDET1(52: 6)
           MOVE SEQ-EV-EQ-WK      TO GS-LINDET1(58: 3)
           MOVE DATA-EV-EQ-WK     TO DIAMES-E
           MOVE DIAMES-E          TO GS-LINDET1(61: 6)
           MOVE HORAS-EV-EQ-WK    TO GS-LINDET1(67: 6)
           MOVE EVENTO-EV-EQ-WK   TO GS-LINDET1(73: 16)
           MOVE CONTRATO-EV-EQ-WK TO GS-LINDET1(89: 4)
           MOVE "INSERE-LIST-EQUIPE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-ORDEM-EQUIPE SECTION.
           MOVE "CLEAR-LIST-BOX-EQUIPE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO GS-CONT-EQ.
           PERFORM ORDEM-EQUIPE

           PERFORM UNTIL ST-WORK2 = "10"
                READ WORKEQUIPE NEXT RECORD AT END
                     MOVE "10" TO ST-WORK2
                NOT AT END
                     PERFORM CARREGA-LISTA-EQUIPE
                END-READ
           END-PERFORM.

       ORDEM-EQUIPE SECTION.
           INITIALIZE REG-WORKEQUIPE
           if gs-ordem = 0
              move 6 to gs-ordem.
           EVALUATE GS-ORDEM
             WHEN 1 START WORKEQUIPE KEY IS NOT < SEQ-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 2 START WORKEQUIPE KEY IS NOT < NOME-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 3 START WORKEQUIPE KEY IS NOT < FUNCAO-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 4 START WORKEQUIPE KEY IS NOT < VEICULO-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 5 START WORKEQUIPE KEY IS NOT < SEQ-EV-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 6 START WORKEQUIPE KEY IS NOT < EVENTO-EV-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
             WHEN 7 START WORKEQUIPE KEY IS NOT < CONTRATO-EV-EQ-WK
                          INVALID KEY MOVE "10" TO ST-WORK2
                    END-START
           END-EVALUATE.

       EXCLUI-ITEM-EQUIPE SECTION.
           MOVE GS-LINDET1(1: 2) TO SEQ-EQ-WK.
           READ WORKEQUIPE INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE WORKEQUIPE
                END-DELETE
                MOVE SEQ-EV-EQ-WK TO SEQ-EV-WK
                READ WORKEVENTO INVALID KEY CONTINUE
                  NOT INVALID KEY
                    EVALUATE FUNCAO-EQ-WK
                      WHEN 1 SUBTRACT 1 FROM FOTOG-SEL-EV-WK
                      WHEN 2 SUBTRACT 1 FROM CINEG-SEL-EV-WK
                      WHEN 5 SUBTRACT 1 FROM AUXIL-SEL-EV-WK
                      WHEN OTHER CONTINUE
                    END-EVALUATE
                    REWRITE REG-WORKEVENTO
                    END-REWRITE
                END-READ
           END-READ.
           PERFORM CARREGAR-LISTA-EVENTO.

      *------------------------------------------------------
      * DADOS VEICULO
       GRAVA-WORK-VEICULO SECTION.
           MOVE GS-SEQ-VEI         TO SEQ-VEI-WK.
           MOVE GS-CODIGO-VEI      TO CODIGO-VEI-WK.
           MOVE GS-DATA-SAIDA-VEI  TO DATA-SAIDA-VEI-WK
           MOVE GS-HORA-SAIDA-VEI  TO HORA-SAIDA-VEI-WK
           MOVE GS-LOCAL-SAIDA-VEI TO LOCAL-SAIDA-VEI-WK.
           WRITE REG-WORKVEICULO.
       VERIFICA-SEQ-VEICULO SECTION.
           ADD 1 TO ULT-SEQ-VEICULO.
           MOVE ULT-SEQ-VEICULO    TO GS-SEQ-VEI.
       CARREGA-LISTA-VEICULO SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-VEI-WK         TO GS-LINDET(1: 3)
           MOVE CODIGO-VEI-WK      TO GS-LINDET(4: 4) CODIGO-RE06.
           READ RED006.
           MOVE VEICULO-RE06       TO GS-LINDET(8: 20)
           MOVE PROPRIETARIO-RE06  TO CODIGO-CG01.
           READ CGD001.
           MOVE NOME-CG01          TO GS-LINDET(29: 15)
           MOVE QTDE-PASSAG-RE06   TO GS-LINDET(46: 4)
           MOVE DATA-SAIDA-VEI-WK  TO DATA-E
           MOVE DATA-E             TO GS-LINDET(50: 11)
           MOVE HORA-SAIDA-VEI-WK  TO GS-LINDET(61: 6)
           MOVE LOCAL-SAIDA-VEI-WK TO GS-LINDET(67: 15)
           MOVE DISTANCIA-CID      TO GS-LINDET(83: 5)
           MOVE "INSERE-LIST-VEICULO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI-ITEM-VEICULO SECTION.
           MOVE GS-LINDET(1: 2) TO SEQ-VEI-WK.
           READ WORKVEICULO INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE WORKVEICULO
                END-DELETE
           END-READ.
      *    VERIFICA ULTIMA SEQUENCIA VEICULO
           MOVE ZEROS TO SEQ-VEI-WK.
           START WORKVEICULO KEY IS NOT < SEQ-VEI-WK INVALID KEY
                MOVE "10" TO ST-WORK3.
           PERFORM UNTIL ST-WORK3 = "10"
             READ WORKVEICULO NEXT RECORD AT END
                MOVE "10" TO ST-WORK3
             NOT AT END
                MOVE SEQ-VEI-WK TO ULT-SEQ-VEICULO
             END-READ
           END-PERFORM.
      *--------------------------------------------------------------
      * DADOS DE MATERIAL P/ O PLANEJAMENTO

       GRAVA-WORK-MATERIAL-INI SECTION.

           MOVE ZEROS TO CODIGO-RE03.
           START RED003 KEY IS NOT < CODIGO-RE03 INVALID KEY
                 MOVE "10" TO ST-RED003.
           PERFORM UNTIL ST-RED003 = "10"
             READ RED003 NEXT RECORD AT END
                  MOVE "10" TO ST-RED003
             NOT AT END
                  MOVE CODIGO-RE03     TO CODIGO-MAT-WK
                  MOVE ZEROS           TO QTDE-MAT-WK
                  WRITE REG-WORKMATERIAL
                  END-WRITE
             END-READ
           END-PERFORM.

       GRAVA-WORK-MATERIAL-SELECAO SECTION.
           MOVE GS-LINDET(1: 3)       TO CODIGO-MAT-WK
           MOVE GS-QTDE-MAT           TO QTDE-E
           READ WORKMATERIAL INVALID KEY
                 MOVE ZEROS TO QTDE-MAT-WK
           NOT INVALID KEY
                 MOVE QTDE-E          TO GS-LINDET(39: 9)
                 MOVE GS-QTDE-MAT     TO QTDE-MAT-WK
                 REWRITE REG-WORKMATERIAL
                 END-REWRITE
           END-READ.
       SELECAO-MATERIAL SECTION.
           MOVE GS-LINDET(1: 3)      TO CODIGO-MAT-WK
           READ WORKMATERIAL INVALID KEY
                INITIALIZE REG-WORKMATERIAL.

           MOVE CODIGO-MAT-WK        TO CODIGO-RE03.
           READ RED003 INVALID KEY
                MOVE SPACES TO DESCRICAO-RE03.

           MOVE DESCRICAO-RE03       TO GS-DESC-MAT
           MOVE QTDE-MAT-WK          TO GS-QTDE-MAT.
       CARREGA-LISTA-MATERIAL SECTION.
           MOVE ZEROS        TO GS-CONT-MAT
           MOVE "CLEAR-LIST-BOX-MATERIAL" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO CODIGO-MAT-WK.
           START WORKMATERIAL KEY IS NOT < CODIGO-MAT-WK INVALID KEY
                 MOVE "10" TO ST-WORK4.
           PERFORM UNTIL ST-WORK4 = "10"
             READ WORKMATERIAL NEXT RECORD AT END
                  MOVE "10" TO ST-WORK4
             NOT AT END
                  MOVE SPACES          TO GS-LINDET
                  MOVE CODIGO-MAT-WK   TO GS-LINDET(1: 3) CODIGO-RE03
                  READ RED003 INVALID KEY
                       MOVE SPACES     TO DESCRICAO-RE03
                  END-READ
                  MOVE DESCRICAO-RE03  TO GS-LINDET(9: 30)
                  MOVE QTDE-MAT-WK     TO QTDE-E
                  MOVE QTDE-E          TO GS-LINDET(39: 9)
                  MOVE "INSERE-LIST-MATERIAL" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
             END-READ
           END-PERFORM.
      *--------------------------------------------------------------
      * VERIFICA SE O PLANEJAMENTO JÁ ESTÁ CADASTRADO
      * CASO AFIRMATIVO GRAVAR OS ARQUIVOS WORK RESPECTIVOS
       VERIFICA-PLANEJAMENTO SECTION.
           OPEN INPUT PRD101.
           IF ST-PRD101 = "35"
              CLOSE PRD101  OPEN OUTPUT PRD101
              CLOSE PRD101  OPEN INPUT  PRD101.
           IF ST-PRD101 <> "00"
              MOVE "ERRO ABERTURA PRD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM GRAVA-WORK-EVENTO-G
      *    PERFORM CARREGAR-LISTA-EVENTO
           CLOSE PRD101.

           OPEN INPUT PRD100.
           IF ST-PRD100 = "35"
              CLOSE PRD100  OPEN OUTPUT PRD100
              CLOSE PRD100  OPEN INPUT  PRD100.

           IF ST-PRD100 <> "00"
              MOVE "ERRO ABERTURA PRD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM GRAVA-WORK-EQUIPE-G.
           CLOSE PRD100.

           OPEN INPUT PRD102.
           IF ST-PRD102 = "35"
              CLOSE PRD102  OPEN OUTPUT PRD102
              CLOSE PRD102  OPEN INPUT  PRD102.

           IF ST-PRD102 <> "00"
              MOVE "ERRO ABERTURA PRD102: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD102 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM GRAVA-WORK-VEICULO-G.
           CLOSE PRD102.

           OPEN INPUT PRD103.
           IF ST-PRD103 = "35"
              CLOSE PRD103  OPEN OUTPUT PRD103
              CLOSE PRD103  OPEN INPUT  PRD103.

           IF ST-PRD103 <> "00"
              MOVE "ERRO ABERTURA PRD103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM GRAVA-WORK-MATERIAL-INI.
           PERFORM GRAVA-WORK-MATERIAL-G.
           PERFORM CARREGA-LISTA-MATERIAL.
           CLOSE      PRD103.

           OPEN INPUT PRD104.
           IF ST-PRD104 = "35" CLOSE PRD104  OPEN OUTPUT PRD104
                              CLOSE PRD104  OPEN INPUT PRD104.
           IF ST-PRD104 <> "00"
              MOVE "ERRO ABERTURA PRD104: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD104 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           PERFORM CARREGA-LISTA-OBS
           CLOSE PRD104.

           OPEN INPUT PRD105.
           IF ST-PRD105 = "35"
              CLOSE PRD105  OPEN OUTPUT PRD105
              CLOSE PRD105  OPEN INPUT  PRD105.

           IF ST-PRD105 <> "00"
              MOVE "ERRO ABERTURA PRD105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PRD105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE NR-PLAN-W   TO NR-PLAN-PR105.
           READ PRD105 INVALID KEY
                CONTINUE
          NOT INVALID KEY
                MOVE HOTEL-PR105         TO GS-CODIGO-HOTEL
                PERFORM LER-HOTEL
                MOVE CONTATO-PR105       TO GS-CONTATO-HOTEL
                MOVE VALOR-DIARIA-PR105  TO GS-VALOR-HOTEL
                MOVE OBS-PR105           TO GS-OBS-HOTEL
                MOVE PREV-VEIC-PR105     TO GS-VLR-VEICULO
                MOVE PREV-HOSP-PR105     TO GS-VLR-HOSPEDAGEM
                MOVE PREV-REFEIC-PR105   TO GS-VLR-REFEICAO
                MOVE PREV-OUTROS-PR105   TO GS-VLR-OUTROS
                MOVE QT-VEICULO-PR105    TO GS-QT-VEICULO
      *         MOVE QT-DIAS-PR105       TO
           END-READ.
           CLOSE PRD105.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-LISTA-OBS SECTION.
           INITIALIZE GS-OBS
           MOVE NR-PLAN-W  TO NR-PLAN-PR104.
           MOVE ZEROS      TO SEQ-OBS-PR104.
           MOVE SPACES     TO GS-OBS.
           MOVE 1          TO I.

           START PRD104 KEY IS NOT < CHAVE-PR104 INVALID KEY
                 MOVE "10" TO ST-PRD104.

           PERFORM UNTIL ST-PRD104 = "10"
             READ PRD104 NEXT RECORD AT END MOVE "10" TO ST-PRD104
               NOT AT END
                 IF NR-PLAN-PR104 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD104
                 ELSE
                    MOVE OBSERVACAO-PR104 TO GS-OBS(I:80)
                    ADD 80 TO I
                 END-IF
             END-READ
           END-PERFORM.
       GRAVA-WORK-MATERIAL-G SECTION.
           MOVE "CLEAR-LIST-BOX-MATERIAL" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS        TO CODIGO-MAT-WK.
           START WORKMATERIAL KEY IS NOT < CODIGO-MAT-WK INVALID KEY
                        MOVE "10" TO ST-WORK4.
           PERFORM UNTIL ST-WORK4 = "10"
             READ WORKMATERIAL NEXT RECORD AT END MOVE "10" TO ST-WORK4
               NOT AT END
                 MOVE CODIGO-MAT-WK     TO CODIGO-PR103
                 MOVE NR-PLAN-W         TO NR-PLAN-PR103
                 READ PRD103 INVALID KEY MOVE ZEROS TO QTDE-PR103
                 END-READ
                 MOVE QTDE-PR103        TO QTDE-MAT-WK
                 REWRITE REG-WORKMATERIAL
                 END-REWRITE
             END-READ
           END-PERFORM.

       GRAVA-WORK-VEICULO-G SECTION.
           INITIALIZE REG-WORKVEICULO.
           MOVE "CLEAR-LIST-BOX-VEICULO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE NR-PLAN-W    TO NR-PLAN-PR102
           MOVE ZEROS        TO SEQ-VEI-PR102.
           START PRD102 KEY IS NOT < CHAVE-PR102 INVALID KEY
                        MOVE "10" TO ST-PRD102.
           PERFORM UNTIL ST-PRD102 = "10"
             READ PRD102 NEXT RECORD AT END MOVE "10" TO ST-PRD102
               NOT AT END
                 IF NR-PLAN-PR102 <> NR-PLAN-W MOVE "10" TO ST-PRD102
                 ELSE
                   MOVE SEQ-VEI-PR102        TO SEQ-VEI-WK
                                                ULT-SEQ-VEICULO
                   MOVE CODIGO-PR102         TO CODIGO-VEI-WK
                   MOVE DATA-SAIDA-PR102     TO DATA-SAIDA-VEI-WK
                   MOVE LOCAL-PR102          TO LOCAL-SAIDA-VEI-WK
                   MOVE HORA-PR102           TO HORA-SAIDA-VEI-WK
                   WRITE REG-WORKVEICULO
                   END-WRITE
                   PERFORM CARREGA-LISTA-VEICULO
                 END-IF
             END-READ
           END-PERFORM.
       GRAVA-WORK-EQUIPE-G SECTION.
           INITIALIZE REG-WORKEQUIPE.
           MOVE "CLEAR-LIST-BOX-EQUIPE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE NR-PLAN-W    TO NR-PLAN-PR100
           MOVE ZEROS        TO SEQ-EQ-PR100.
           START PRD100 KEY IS NOT < CHAVE-PR100 INVALID KEY
                        MOVE "10" TO ST-PRD100.
           PERFORM UNTIL ST-PRD100 = "10"
             READ PRD100 NEXT RECORD AT END MOVE "10" TO ST-PRD100
               NOT AT END
                 IF NR-PLAN-PR100 <> NR-PLAN-W MOVE "10" TO ST-PRD100
                 ELSE
                   MOVE SEQ-EQ-PR100      TO SEQ-EQ-WK ULT-SEQ-EQUIPE
                   MOVE CODIGO-PR100      TO CODIGO-EQ-WK CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01         TO NOME-EQ-WK
                   MOVE FUNCAO-PR100      TO FUNCAO-EQ-WK
                   MOVE PADRAO-PR100      TO PADRAO-EQ-WK
                   MOVE SEQ-VEICULO-PR100 TO VEICULO-EQ-WK
                   MOVE SEQ-EVE-PR100      TO SEQ-EV-EQ-WK SEQ-EV-WK
                   READ WORKEVENTO INVALID KEY
                        INITIALIZE REG-WORKEVENTO
                   END-READ
                   MOVE DATA-EV-WK        TO DATA-EV-EQ-WK
                   MOVE HORA-EV-WK        TO HORAS-EV-EQ-WK
                   MOVE EVENTO-EV-WK      TO EVENTO-EV-EQ-WK
                   MOVE CONTRATO-EV-WK    TO CONTRATO-EV-EQ-WK
                   WRITE REG-WORKEQUIPE
                   END-WRITE
                   PERFORM CARREGA-LISTA-EQUIPE
                 END-IF
             END-READ
           END-PERFORM.
       GRAVA-WORK-EVENTO-G SECTION.
           INITIALIZE REG-WORKEVENTO.
           MOVE NR-PLAN-W TO NR-PLAN-PR101.
           MOVE ZEROS     TO SEQ-EVE-PR101 SEQ-EV-WK.
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                        MOVE "10" TO ST-PRD101.
           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END MOVE "10" TO ST-PRD101
               NOT AT END
                 IF NR-PLAN-PR101 <> NR-PLAN-W MOVE "10" TO ST-PRD101
                 ELSE
                    MOVE CONTRATO-PR101     TO NR-CONTRATO-CO60
                    MOVE ITEM-PR101         TO ITEM-CO60
                    READ COD060 INVALID KEY
                         INITIALIZE REG-COD060
                    END-READ
                    PERFORM MOVER-DADOS-WORK-EVENTO
                    MOVE NR-PLAN-PR101      TO NR-PLAN-WK
                    MOVE FOTOG-SEL-PR101    TO FOTOG-SEL-EV-WK
                    MOVE CINEG-SEL-PR101    TO CINEG-SEL-EV-WK
                    MOVE AUXIL-SEL-PR101    TO AUXIL-SEL-EV-WK
                    MOVE SEQ-EVE-PR101      TO SEQ-EV-WK
                    WRITE REG-WORKEVENTO
                    END-WRITE
                 END-IF
             END-READ
           END-PERFORM.
           IF SEQ-EV-WK = ZEROS
               IF GS-SEQ-PLAN <> ZEROS
                  PERFORM VERIFICA-CONTRATO
               ELSE
                  PERFORM GRAVA-WORK
                  PERFORM CARREGAR-LISTA-EVENTO
               END-IF
           ELSE PERFORM CARREGAR-LISTA-EVENTO.
      *------------------------------------------------------
      *   GRAVA PLANEJAMENTO - SE JÁ EXISTIR O NR DO PLANEJAMENTO,
      *   PRIMEIRO EXCLUI E DEPOIS GRAVA NOVAMENTE

       GRAVAR-PRD101 SECTION.
           MOVE GS-NR-PLAN         TO NR-PLAN-W(1: 8)
           MOVE GS-ANO-PLAN        TO NR-PLAN-W(9: 4)
           MOVE GS-SEQ-PLAN        TO NR-PLAN-W(13: 2)

           PERFORM EXCLUI-PLANEJAMENTO.

           MOVE "Gravando arquivo... "  TO GS-MENSAGEM
           MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           CLOSE    COD060
           OPEN I-O PRD101 COD060

           MOVE ZEROS TO SEQ-EV-WK.
           START WORKEVENTO KEY IS NOT < SEQ-EV-WK INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORKEVENTO NEXT RECORD AT END
                  MOVE "10" TO ST-WORK1
             NOT AT END
                 MOVE NR-PLAN-W       TO NR-PLAN-PR101
                 MOVE SEQ-EV-WK       TO SEQ-EVE-PR101
                 MOVE CONTRATO-EV-WK  TO CONTRATO-PR101
                                         NR-CONTRATO-CO60
                 MOVE ITEM-EV-WK      TO ITEM-PR101
                                         ITEM-CO60
                 READ COD060 INVALID KEY
                       MOVE "NÃO ENCONTREI O COD060" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                 NOT INVALID KEY
                       MOVE NR-PLAN-W(1:8) TO NR-PLANEJ-CO60
                       REWRITE REG-COD060 INVALID KEY
                           MOVE "Erro de Regravação...COD060" TO
                           MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                       END-REWRITE
                 END-READ
                 MOVE FOTOG-SEL-EV-WK TO FOTOG-SEL-PR101
                 MOVE AUXIL-SEL-EV-WK TO AUXIL-SEL-PR101
                 MOVE CINEG-SEL-EV-WK TO CINEG-SEL-PR101
                 WRITE REG-PRD101 INVALID KEY
                       MOVE "Erro de Gravação..PRD101" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "I"         TO LOG1-OPERACAO
                         MOVE "PRD101"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD101  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(1)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                 END-WRITE
             END-READ
           END-PERFORM.
           CLOSE PRD101 COD060
           OPEN INPUT COD060.

       GRAVAR-PRD100 SECTION.
           OPEN I-O PRD100.
           MOVE ZEROS TO SEQ-EQ-WK.
           START WORKEQUIPE KEY IS NOT < SEQ-EQ-WK INVALID KEY
                 MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
             READ WORKEQUIPE NEXT RECORD AT END
                  MOVE "10" TO ST-WORK2
             NOT AT END
                 MOVE NR-PLAN-W     TO NR-PLAN-PR100
                 MOVE SEQ-EQ-WK     TO SEQ-EQ-PR100
                 MOVE CODIGO-EQ-WK  TO CODIGO-PR100
                 MOVE FUNCAO-EQ-WK  TO FUNCAO-PR100
                 MOVE PADRAO-EQ-WK  TO PADRAO-PR100
                 MOVE VEICULO-EQ-WK TO SEQ-VEICULO-PR100
                 MOVE SEQ-EV-EQ-WK  TO SEQ-EVE-PR100
                 WRITE REG-PRD100 INVALID KEY
                       OPEN I-O LOG001
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE USUARIO-W TO LOG1-USUARIO
                       MOVE WS-DATA-CPU TO LOG1-DATA
                       MOVE WS-HORA-SYS TO LOG1-HORAS
                       MOVE "I"         TO LOG1-OPERACAO
                       MOVE "PRD100"    TO LOG1-ARQUIVO
                       MOVE "PRP100"    TO LOG1-PROGRAMA
                       MOVE REG-PRD100  TO LOG1-REGISTRO
                       WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM

      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(2)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                     MOVE "C" TO TIPO-MSG
      *                     PERFORM EXIBIR-MENSAGEM
                       END-WRITE
                       CLOSE LOG001
                 END-WRITE
             END-READ
           END-PERFORM.
           CLOSE PRD100.

       GRAVAR-PRD102 SECTION.
           OPEN I-O PRD102.
           MOVE ZEROS TO SEQ-VEI-WK.
           START WORKVEICULO KEY IS NOT < SEQ-VEI-WK INVALID KEY
                 MOVE "10" TO ST-WORK3.
           PERFORM UNTIL ST-WORK3 = "10"
             READ WORKVEICULO NEXT RECORD AT END
                  MOVE "10" TO ST-WORK3
             NOT AT END
                  MOVE NR-PLAN-W          TO NR-PLAN-PR102
                  MOVE SEQ-VEI-WK         TO SEQ-VEI-PR102
                  MOVE CODIGO-VEI-WK      TO CODIGO-PR102
                  MOVE DATA-SAIDA-VEI-WK  TO DATA-SAIDA-PR102
                  MOVE HORA-SAIDA-VEI-WK  TO HORA-PR102
                  MOVE LOCAL-SAIDA-VEI-WK TO LOCAL-PR102
                  WRITE REG-PRD102 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "I"         TO LOG1-OPERACAO
                         MOVE "PRD102"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD102  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(3)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                  END-WRITE
             END-READ
           END-PERFORM.
           CLOSE PRD102.

       GRAVAR-PRD103 SECTION.
           OPEN I-O PRD103.
           MOVE ZEROS TO CODIGO-MAT-WK.
           START WORKMATERIAL KEY IS NOT < CODIGO-MAT-WK INVALID KEY
                 MOVE "10" TO ST-WORK4.
           PERFORM UNTIL ST-WORK4 = "10"
             READ WORKMATERIAL NEXT RECORD AT END
                  MOVE "10" TO ST-WORK4
             NOT AT END
                  IF QTDE-MAT-WK <> ZEROS
                     MOVE NR-PLAN-W          TO NR-PLAN-PR103
                     MOVE CODIGO-MAT-WK      TO CODIGO-PR103
                     MOVE QTDE-MAT-WK        TO QTDE-PR103
                     WRITE REG-PRD103 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "I"         TO LOG1-OPERACAO
                         MOVE "PRD103"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD103  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(4)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                     END-WRITE
                  END-IF
             END-READ
           END-PERFORM.
           CLOSE PRD103.

       GRAVAR-PRD104 SECTION.
           OPEN I-O PRD104.

           MOVE 1 TO I.
           MOVE ZEROS TO SEQ-PR104
           PERFORM VARYING I FROM 1 BY 80 UNTIL I > 2400
             MOVE NR-PLAN-W       TO NR-PLAN-PR104
             MOVE GS-OBS(I: 80)   TO OBSERVACAO-PR104
             IF OBSERVACAO-PR104 = SPACES CONTINUE
             ELSE ADD 1 TO SEQ-OBS-PR104
                  WRITE REG-PRD104 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "I"         TO LOG1-OPERACAO
                         MOVE "PRD104"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD104  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(5)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                  END-WRITE
             END-IF
           END-PERFORM.



      *    MOVE 1 TO I
      *    MOVE 1 TO INICIO
      *    MOVE 0 TO AUXILIAR
      *    MOVE 1 TO QUANTIDADE
      *    MOVE SPACES TO OBSERVACAO-PR104
      *    PERFORM UNTIL I > 2400
      *        IF GS-OBS(I:1) = X"0a" OR QUANTIDADE = 80
      *           IF OBSERVACAO-PR104 <> GS-OBS(INICIO:AUXILIAR)
      *              MOVE GS-OBS(INICIO:AUXILIAR) TO OBSERVACAO-PR104
      *              IF OBSERVACAO-PR104 <> SPACES
      *                 MOVE NR-PLAN-W            TO NR-PLAN-PR104
      *                 ADD  1                    TO SEQ-OBS-PR104
      *                 WRITE REG-PRD104
      *                 END-WRITE
      *                 MOVE AUXILIAR TO INICIO
      *                 IF GS-OBS(AUXILIAR:1) = X"0a"
      *                    ADD 1 TO INICIO
      *                 END-IF
      *                 MOVE 1 TO QUANTIDADE
      *              END-IF
      *           END-IF
      *        ELSE
      *           ADD 1 TO AUXILIAR
      *           ADD 1 TO QUANTIDADE
      *        END-IF
      *        ADD 1 TO I
      *    END-PERFORM.

           CLOSE PRD104.

       GRAVAR-PRD105 SECTION.
           OPEN I-O PRD105.
           MOVE NR-PLAN-W          TO NR-PLAN-PR105
           MOVE GS-CODIGO-HOTEL    TO HOTEL-PR105
           MOVE GS-CONTATO-HOTEL   TO CONTATO-PR105
           MOVE GS-VALOR-HOTEL     TO VALOR-DIARIA-PR105
           MOVE GS-OBS-HOTEL       TO OBS-PR105
           MOVE GS-VLR-VEICULO     TO PREV-VEIC-PR105
           MOVE GS-VLR-HOSPEDAGEM  TO PREV-HOSP-PR105
           MOVE GS-VLR-REFEICAO    TO PREV-REFEIC-PR105
           MOVE GS-VLR-OUTROS      TO PREV-OUTROS-PR105
           MOVE GS-QT-VEICULO      TO QT-VEICULO-PR105
           MOVE ZEROS              TO QT-DIAS-PR105
           WRITE REG-PRD105 NOT INVALID KEY
                 OPEN I-O LOG001
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE USUARIO-W TO LOG1-USUARIO
                 MOVE WS-DATA-CPU TO LOG1-DATA
                 MOVE WS-HORA-SYS TO LOG1-HORAS
                 MOVE "E"         TO LOG1-OPERACAO
                 MOVE "PRD105"    TO LOG1-ARQUIVO
                 MOVE "PRP100"    TO LOG1-PROGRAMA
                 MOVE REG-PRD105  TO LOG1-REGISTRO
                 WRITE REG-LOG001 INVALID KEY
                      PERFORM UNTIL ST-LOG001 = "00"
                           ACCEPT WS-HORA-SYS FROM TIME
                           MOVE WS-HORA-SYS TO LOG1-HORAS
                           WRITE REG-LOG001 NOT INVALID KEY
                                MOVE "00" TO ST-LOG001
                           END-WRITE
                      END-PERFORM
      *               MOVE SPACES TO MENSAGEM
      *               STRING "Erro de Gravação...LOG001(6)"
      *               X"0A" "Status =>" ST-LOG001
      *                      INTO MENSAGEM
      *               MOVE "C" TO TIPO-MSG
      *               PERFORM EXIBIR-MENSAGEM
                 END-WRITE
                 CLOSE LOG001
           END-WRITE
           CLOSE PRD105.
           MOVE "UNSHOW-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *--------------------------------------------------------
       EXCLUI-PLANEJAMENTO SECTION.
           MOVE "Deletando arquivo... " TO GS-MENSAGEM
           MOVE "EXIBE-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           CLOSE    COD060
           OPEN I-O PRD101 COD060.
           MOVE NR-PLAN-W     TO NR-PLAN-PR101
           MOVE ZEROS         TO SEQ-EVE-PR101
           START PRD101 KEY IS NOT < CHAVE-PR101 INVALID KEY
                 MOVE "10" TO ST-PRD101.

           PERFORM UNTIL ST-PRD101 = "10"
             READ PRD101 NEXT RECORD AT END
                 MOVE "10" TO ST-PRD101
             NOT AT END
                 IF NR-PLAN-PR101 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD101
                 ELSE
                    MOVE CONTRATO-PR101 TO NR-CONTRATO-CO60
                    MOVE ITEM-PR101     TO ITEM-CO60
                    READ COD060 INVALID KEY
                         MOVE "NÃO ENCONTREI O EVENTO" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    NOT INVALID KEY
                         MOVE ZEROS          TO NR-PLANEJ-CO60
                         REWRITE REG-COD060 INVALID KEY
                              MOVE "Erro de Regravação...COD060" TO
                              MENSAGEM
                              MOVE "C" TO TIPO-MSG
                              PERFORM EXIBIR-MENSAGEM
                         END-REWRITE
                    END-READ
                    DELETE PRD101 INVALID KEY
                         MOVE "Erro de Exclusão...PRD101" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                    NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "E"         TO LOG1-OPERACAO
                         MOVE "PRD101"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD101  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(7)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                    END-DELETE
             END-READ
           END-PERFORM.
           CLOSE PRD101 COD060
           OPEN INPUT COD060.

           OPEN I-O PRD100.
           MOVE NR-PLAN-W     TO NR-PLAN-PR100
           MOVE ZEROS         TO SEQ-EQ-PR100
           START PRD100 KEY IS NOT < CHAVE-PR100 INVALID KEY
                 MOVE "10" TO ST-PRD100.
           PERFORM UNTIL ST-PRD100 = "10"
             READ PRD100 NEXT RECORD AT END
                 MOVE "10" TO ST-PRD100
             NOT AT END
                 IF NR-PLAN-PR100 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD100
                 ELSE
                    DELETE PRD100 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "E"         TO LOG1-OPERACAO
                         MOVE "PRD100"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD100  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(8)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                    END-DELETE
             END-READ
           END-PERFORM.
           CLOSE PRD100.

           OPEN I-O PRD102.
           MOVE NR-PLAN-W     TO NR-PLAN-PR102
           MOVE ZEROS         TO SEQ-VEI-PR102
           START PRD102 KEY IS NOT < CHAVE-PR102 INVALID KEY
                 MOVE "10" TO ST-PRD102.
           PERFORM UNTIL ST-PRD102 = "10"
             READ PRD102 NEXT RECORD AT END MOVE "10" TO ST-PRD102
               NOT AT END
                 IF NR-PLAN-PR102 <> NR-PLAN-W MOVE "10" TO ST-PRD102
                 ELSE
                    DELETE PRD102 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "E"         TO LOG1-OPERACAO
                         MOVE "PRD102"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD102  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(9)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                    END-DELETE
             END-READ
           END-PERFORM.
           CLOSE PRD102.

           OPEN I-O PRD103.
           MOVE NR-PLAN-W     TO NR-PLAN-PR103
           MOVE ZEROS         TO CODIGO-PR103
           START PRD103 KEY IS NOT < CHAVE-PR103 INVALID KEY
                 MOVE "10" TO ST-PRD103.
           PERFORM UNTIL ST-PRD103 = "10"
             READ PRD103 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD103
             NOT AT END
                  IF NR-PLAN-PR103 <> NR-PLAN-W
                     MOVE "10" TO ST-PRD103
                  ELSE
                     DELETE PRD103 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "E"         TO LOG1-OPERACAO
                         MOVE "PRD103"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD103  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(10)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                     END-DELETE
                  END-IF
             END-READ
           END-PERFORM.
           CLOSE PRD103.

           OPEN I-O PRD104.
           MOVE NR-PLAN-W     TO NR-PLAN-PR104
           MOVE ZEROS         TO SEQ-OBS-PR104.
           START PRD104 KEY IS NOT < CHAVE-PR104 INVALID KEY
                 MOVE "10" TO ST-PRD104.
           PERFORM UNTIL ST-PRD104 = "10"
             READ PRD104 NEXT RECORD AT END
                  MOVE "10" TO ST-PRD104
             NOT AT END
                 IF NR-PLAN-PR104 <> NR-PLAN-W
                    MOVE "10" TO ST-PRD104
                 ELSE
                    DELETE PRD104 NOT INVALID KEY
                         OPEN I-O LOG001
                         MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                         ACCEPT WS-HORA-SYS FROM TIME
                         MOVE USUARIO-W TO LOG1-USUARIO
                         MOVE WS-DATA-CPU TO LOG1-DATA
                         MOVE WS-HORA-SYS TO LOG1-HORAS
                         MOVE "E"         TO LOG1-OPERACAO
                         MOVE "PRD104"    TO LOG1-ARQUIVO
                         MOVE "PRP100"    TO LOG1-PROGRAMA
                         MOVE REG-PRD104  TO LOG1-REGISTRO
                         WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(11)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                       MOVE "C" TO TIPO-MSG
      *                       PERFORM EXIBIR-MENSAGEM
                         END-WRITE
                         CLOSE LOG001
                    END-DELETE
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE PRD104.

           OPEN I-O PRD105.
           MOVE NR-PLAN-W     TO NR-PLAN-PR105
           READ PRD105 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE PRD105 NOT INVALID KEY
                       OPEN I-O LOG001
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE USUARIO-W TO LOG1-USUARIO
                       MOVE WS-DATA-CPU TO LOG1-DATA
                       MOVE WS-HORA-SYS TO LOG1-HORAS
                       MOVE "E"         TO LOG1-OPERACAO
                       MOVE "PRD105"    TO LOG1-ARQUIVO
                       MOVE "PRP100"    TO LOG1-PROGRAMA
                       MOVE REG-PRD105  TO LOG1-REGISTRO
                       WRITE REG-LOG001 INVALID KEY
                              PERFORM UNTIL ST-LOG001 = "00"
                                   ACCEPT WS-HORA-SYS FROM TIME
                                   MOVE WS-HORA-SYS TO LOG1-HORAS
                                   WRITE REG-LOG001 NOT INVALID KEY
                                        MOVE "00" TO ST-LOG001
                                   END-WRITE
                              END-PERFORM
      *                       MOVE SPACES TO MENSAGEM
      *                       STRING "Erro de Gravação...LOG001(12)"
      *                       X"0A" "Status =>" ST-LOG001
      *                              INTO MENSAGEM
      *                     MOVE "C" TO TIPO-MSG
      *                     PERFORM EXIBIR-MENSAGEM
                       END-WRITE
                       CLOSE LOG001
                END-DELETE
           END-READ.
           CLOSE PRD105.
           MOVE "UNSHOW-MENSAGEM" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *--------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       ORDEM SECTION.
      *    EVALUATE GS-ORDEM
      *      WHEN 1
      *         MOVE "MOVTO" TO GS-DESCR-ORDEM
      *         MOVE ZEROS TO DATA-MOVTO-WK
      *         START WORK KEY IS NOT < DATA-MOVTO-WK INVALID KEY
      *               MOVE "10" TO ST-WORK1
      *    END-EVALUATE.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "PRP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------
      *IMPRIME-RELATORIO SECTION.
      *    MOVE GS-NR-PLAN  TO PASSAR-STRING-1(1: 8)
      *    MOVE GS-ANO-PLAN TO PASSAR-STRING-1(9: 4)
      *    MOVE GS-SEQ-PLAN TO PASSAR-STRING-1(13: 2)
      *    CALL "PRP101" USING PASSAR-STRING-1.
      *    CANCEL "PRP101".
      *-------------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM"
           MOVE 1 TO GS-FLAG-CRITICA.

      *--------------------------------------------------------------
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 CGD001 RED002 CGD005
                 RED005 RED006 RED003 RED501 WORKEQUIPE COD061 IED011
                 WORKMATERIAL WORKCONTRATO WORKVEICULO WORKEVENTO
           DELETE FILE WORKEVENTO WORKEQUIPE WORKVEICULO WORKMATERIAL
                       WORKCONTRATO.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.


