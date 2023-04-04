       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP081.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 11/09/2001.
      *FUNÇÃO: Movimento de DE DESPESAS FIXAS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX040.
           COPY COPX081.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW040.
       COPY COPW081.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP081.CPB".
           COPY "COP081.CPY".
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
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD081             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  ITEM-W                PIC 9(2)     VALUE ZEROS.
           05  I                     PIC 99       VALUE ZEROS.

           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(34)   VALUE
           "CONFERENCIA MVTO DESPESAS FIXAS".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "CONT DESCRICAO                         QTDE    VALOR-UNIT".

       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD081" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD081.
           OPEN I-O COD081.
           OPEN INPUT COD040.
           IF ST-COD081 = "35"
              CLOSE COD081      OPEN OUTPUT COD081
              CLOSE COD081      OPEN I-O COD081
           END-IF.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD081 <> "00"
              MOVE "ERRO ABERTURA COD081: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD081 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1 PERFORM REGRAVA-DADOS
                   ELSE PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-LOAD-FLG-TRUE
                    MOVE GS-CONTRATO  TO NR-CONTRATO-CO81
                    PERFORM CARREGAR-DADOS
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *----------------------------------------------------------------
       EXCLUI SECTION.
           DELETE COD081.
           PERFORM LIMPAR-DADOS.
       CARREGAR-DADOS SECTION.
           START COD081 KEY IS = NR-CONTRATO-CO81 INVALID KEY CONTINUE.
           READ COD081 INVALID KEY INITIALIZE REG-COD081.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
             MOVE QTDE-CO81(I)     TO GS-QTDE-DESP(I)
             MOVE VALOR-CO81(I)    TO GS-VALOR-UNI-DESP(I)
           END-PERFORM.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD081
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO NR-CONTRATO-CO81
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
             MOVE GS-QTDE-DESP(I)      TO QTDE-CO81(I)
             MOVE GS-VALOR-UNI-DESP(I) TO VALOR-CO81(I)
           END-PERFORM.

       GRAVA-DADOS SECTION.
           MOVE ZEROS TO ST-COD081.
           WRITE REG-COD081 INVALID KEY
                 REWRITE REG-COD081.

       REGRAVA-DADOS SECTION.
           REWRITE REG-COD081 INVALID KEY
                 MOVE "Erro Regravacao COD081" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD081 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *-----------------------------------------------------------

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP081" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO81.
           MOVE ZEROS          TO ITEM-CO81.
           START COD081 KEY IS = NR-CONTRATO-CO81 INVALID KEY
                 MOVE "10" TO ST-COD081.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE ZEROS TO I.
           PERFORM UNTIL ST-COD081 = "10"
             READ COD081 NEXT RECORD AT END MOVE "10" TO ST-COD081
              NOT AT END
                IF NR-CONTRATO-CO81 <> GS-CONTRATO
                         MOVE "10" TO ST-COD081
                ELSE
                  ADD 1 TO I
                  MOVE NR-CONTRATO-CO81   TO LINDET-REL(1: 5)
                  EVALUATE I
                     WHEN 1 MOVE "01- Custo dos Filmes"
                                  TO LINDET-REL(6: 30)
                     WHEN 2 MOVE "02- Custo das Fotos "
                                  TO LINDET-REL(6: 30)
                     WHEN 3 MOVE "03- Custo do Álbum/Estojo"
                                  TO LINDET-REL(6: 30)
                     WHEN 4 MOVE "04- Custo das Fitas S-VHS"
                                  TO LINDET-REL(6: 30)
                     WHEN 5 MOVE "05- Custo das Fitas VHS"
                                  TO LINDET-REL(6: 30)
                     WHEN 6 MOVE "06- Custo dos Estojos de Fitas"
                                  TO LINDET-REL(6: 30)
                     WHEN 7 MOVE "07- Custo de Montagem"
                                  TO LINDET-REL(6: 30)
                     WHEN 8 MOVE "08- Custo das Vendas"
                                  TO LINDET-REL(6: 30)
                  END-EVALUATE
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD081.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.
