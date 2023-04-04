       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGCAIXA.
      *DATA: 11/07/2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *RELATÓRIO: VISUALIZA LOG DO CAIXA

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CXPX040.
           COPY CXPX100.
           COPY LOGCAIXA.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CXPW040.
       COPY CXPW100.
       COPY LOGCAIXA.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(129).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LOGCAIXA.CPB".
           COPY "LOGCAIXA.CPy".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".


       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD040             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  FS-LOGCAIXA           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR1                PIC 9(08)V99 VALUE ZEROS.
           05  VALOR2                PIC 9(08)V99 VALUE ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
           05  CONTADOR              PIC 9(02)    VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 CAB01.
          05  FILLER              PIC X(5)    VALUE SPACES.
          05  EMPRESA-REL         PIC X(85)   VALUE SPACES.
          05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
          05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
          05  FILLER              PIC X       VALUE SPACES.
          05  HORA-REL            PIC X(5)    VALUE "  :  ".
          05  FILLER              PIC X(10)   VALUE SPACES.
          05  FILLER              PIC X(5)    VALUE "PAG: ".
          05  PG-REL              PIC Z9      VALUE ZEROS.
       01 CAB02.
          05  FILLER              PIC X(82)   VALUE
          "RELACAO DO CAIXA  ".
          05  FILLER              PIC X(28)   VALUE SPACES.
          05  FILLER              PIC X(15)   VALUE "   DATA MOVTO: ".
          05  MOVTO-INI-REL       PIC 99/99/9999.
       01 CAB03.
          05  FILLER              PIC X(135)  VALUE ALL "=".
       01 CAB04.
          05 FILLER                PIC X(11) VALUE " DATA LOG".
          05 FILLER                PIC X(06) VALUE "HORAS".
          05 FILLER                PIC X(05) VALUE " SEQ.".
          05 FILLER                PIC X(06) VALUE "USUAR".
          05 FILLER                PIC X(04) VALUE "OP.".
          05 FILLER                PIC X(11) VALUE "DATA CAIXA".
          05 FILLER                PIC X(05) VALUE " SEQ.".
          05 FILLER                PIC X(03) VALUE "TP".
          05 FILLER                PIC X(11) VALUE "   CREDITO".
          05 FILLER                PIC X(11) VALUE "    DEBITO".
          05 FILLER                PIC X(31) VALUE "HISTORICO".
          05 FILLER                PIC X(11) VALUE "DOCTO".
          05 FILLER                PIC X(07) VALUE "CT.PAR".
          05 FILLER                PIC X(06) VALUE "C.RED".
          05 FILLER                PIC X(02) VALUE "C".
          05 FILLER                PIC X(05) VALUE "RESP.".

       01  LINDET.
           05  LINDET-REL          PIC X(135)  VALUE SPACES.

       01 DET-DADOS.
          05 FILLER                PIC X(11) VALUE " DATA LOG".
          05 FILLER                PIC X(06) VALUE "HORAS".
          05 FILLER                PIC X(05) VALUE " SEQ.".
          05 FILLER                PIC X(06) VALUE "USUAR".
          05 FILLER                PIC X(04) VALUE "OP.".
          05 FILLER                PIC X(11) VALUE "DATA CAIXA".
          05 FILLER                PIC X(05) VALUE " SEQ.".
          05 FILLER                PIC X(03) VALUE "TP".
          05 FILLER                PIC X(11) VALUE "   CREDITO".
          05 FILLER                PIC X(11) VALUE "    DEBITO".
          05 FILLER                PIC X(31) VALUE "HISTORICO".
          05 FILLER                PIC X(11) VALUE "DOCTO".
          05 FILLER                PIC X(07) VALUE "CT.PAR".
          05 FILLER                PIC X(06) VALUE "C.RED".
          05 FILLER                PIC X(02) VALUE "C".
          05 FILLER                PIC X(05) VALUE "RESP.".

       01 DET-01.
          05 DET-DIA               PIC 99/.
          05 DET-MES               PIC 99/.
          05 DET-ANO               PIC 9999.
          05 FILLER                PIC X(01).
          05 DET-HORA              PIC 9(02).
          05 FILLER                PIC X(01) VALUE ":".
          05 DET-MINU              PIC 9(02).
          05 FILLER                PIC X(01).
          05 DET-SEQUE             PIC 9999.
          05 FILLER                PIC X(01).
          05 DET-USUARIO           PIC X(05).
          05 FILLER                PIC X(03).
          05 DET-OPERACAO          PIC X(01).
          05 FILLER                PIC X(01).
          05 DET-DIA-CX            PIC 99/.
          05 DET-MES-CX            PIC 99/.
          05 DET-ANO-CX            PIC 9999.
          05 FILLER                PIC X(01).
          05 DET-SEQUE-CX          PIC 9999.
          05 FILLER                PIC X(01).
          05 DET-TIPO              PIC 9(02).
          05 FILLER                PIC X(01).
          05 DET-CREDITO           PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET-DEBITO            PIC ZZZ.ZZ9,99.
          05 FILLER                PIC X(01).
          05 DET-HISTORI           PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-DOCTO             PIC X(10).
          05 FILLER                PIC X(01).
          05 DET-CONTPART          PIC 9(06).
          05 FILLER                PIC X(03).
          05 DET-CONTRED           PIC 9(03).
          05 FILLER                PIC X(01).
          05 DET-CONTABIL          PIC 9.
          05 FILLER                PIC X(01).
          05 DET-RESPONS           PIC X(05).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP120-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP120-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP120-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP120-VERSION-NO  TO DS-VERSION-NO

           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL

           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD040.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "LOGCAIX" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGCAIXA

           OPEN INPUT CXD100 CGD001 CXD040 LOGCAIXA
           IF FS-LOGCAIXA <> "00"
              MOVE "ERRO ABERTURA LOGCAIXA: "  TO CXP120-MENSAGEM-ERRO
              MOVE FS-LOGCAIXA TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD040 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP120-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP120-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP120-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP120-CARREGA-LISTA-TRUE
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
           INITIALIZE CXP120-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP120-DATA-MOVTO TO DATA-INV MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV TO MOVTO-INI.
           MOVE DATA-INV(01: 06) TO MESANO-SALDO-ANT.
       CARREGA-LISTA SECTION.
           PERFORM INVERTE-DATA
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE ALL "-" TO CXP120-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE DET-DADOS     TO CXP120-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ALL "-" TO CXP120-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE SPACES            TO CXP120-LINDET.
           INITIALIZE REG-LOGCAIXA
           MOVE MOVTO-INI         TO DATA-MOV-LOGCAIXA.
           MOVE CXP120-SEQUENCIA  TO SEQ-LOGCAIXA
           START LOGCAIXA KEY IS NOT < CHAVE-CX100-LOGCAIXA INVALID KEY
                 MOVE "10" TO FS-LOGCAIXA.

           PERFORM UNTIL FS-LOGCAIXA = "10"
              READ LOGCAIXA NEXT WITH IGNORE LOCK AT END
                   MOVE "10" TO FS-LOGCAIXA
              NOT AT END
                   IF DATA-MOV-LOGCAIXA > MOVTO-INI OR
                      (CXP120-SEQUENCIA > 0 and seq-logcaixa
                      <> CXP120-SEQUENCIA)
                      MOVE "10" TO FS-LOGCAIXA
                   ELSE

                      MOVE SPACES            TO CXP120-LINDET

                      INITIALIZE DET-01

                      MOVE DATA-LANCAMENTO-LOGCAIXA(1:4) TO DET-ANO
                      MOVE DATA-LANCAMENTO-LOGCAIXA(5:2) TO DET-MES
                      MOVE DATA-LANCAMENTO-LOGCAIXA(7:2) TO DET-DIA
                      MOVE HORA-LANCAMENTO-LOGCAIXA(1:2) TO DET-HORA
                      MOVE HORA-LANCAMENTO-LOGCAIXA(3:2) TO DET-MINU
                      MOVE SEQ-LOGCAIXA                  TO DET-SEQUE
                      MOVE USUARIO-LOGCAIXA              TO DET-USUARIO
                      MOVE OPERACAO-LOGCAIXA             TO DET-OPERACAO

                      MOVE DATA-MOV-LOGCAIXA(1:4)        TO DET-ANO-CX
                      MOVE DATA-MOV-LOGCAIXA(5:2)        TO DET-MES-CX
                      MOVE DATA-MOV-LOGCAIXA(7:2)        TO DET-DIA-CX
                      MOVE SEQ-LOGCAIXA                  TO DET-SEQUE-CX

                      MOVE TIPO-LCTO-LOGCAIXA            TO DET-TIPO

                      IF TIPO-LCTO-LOGCAIXA NOT < 50
                         MOVE VALOR-LOGCAIXA             TO DET-CREDITO
                      ELSE
                         MOVE VALOR-LOGCAIXA             TO DET-DEBITO
                      END-IF

                      MOVE HISTORICO-LOGCAIXA            TO DET-HISTORI
                      MOVE DOCUMENTO-LOGCAIXA            TO DET-DOCTO
                      MOVE CONTAPART-LOGCAIXA            TO DET-CONTPART
                      MOVE CONTA-REDUZ-LOGCAIXA          TO DET-CONTRED
                      MOVE CONTABIL-LOGCAIXA             TO DET-CONTABIL
                      MOVE RESPONSAVEL-LOGCAIXA          TO DET-RESPONS
                      MOVE DET-01                       TO CXP120-LINDET
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM.

           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE CXP120-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LOGCAIXA"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE 0 TO CONTADOR
           PERFORM UNTIL CONTADOR = CXP120-COPIAS
               ADD 1 TO CONTADOR
               PERFORM IMPRIMIR
           END-PERFORM.
       IMPRIME-RELATORIO-FIM.
           EXIT.

       IMPRIMIR SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           INITIALIZE REG-LOGCAIXA
           MOVE MOVTO-INI         TO DATA-MOV-LOGCAIXA.
           MOVE CXP120-SEQUENCIA  TO SEQ-LOGCAIXA
           START LOGCAIXA KEY IS NOT < CHAVE-CX100-LOGCAIXA INVALID KEY
                 MOVE "10" TO FS-LOGCAIXA.

           PERFORM UNTIL FS-LOGCAIXA = "10"
              READ LOGCAIXA NEXT RECORD AT END
                   MOVE "10" TO FS-LOGCAIXA
              NOT AT END
                   IF DATA-MOV-LOGCAIXA > MOVTO-INI    OR
                      SEQ-LOGCAIXA <> CXP120-SEQUENCIA
                      MOVE "10" TO FS-LOGCAIXA
                   ELSE
                      MOVE SPACES            TO CXP120-LINDET

                      INITIALIZE DET-01

                      MOVE DATA-LANCAMENTO-LOGCAIXA(1:4) TO DET-ANO
                      MOVE DATA-LANCAMENTO-LOGCAIXA(5:2) TO DET-MES
                      MOVE DATA-LANCAMENTO-LOGCAIXA(7:2) TO DET-DIA
                      MOVE HORA-LANCAMENTO-LOGCAIXA(1:2) TO DET-HORA
                      MOVE HORA-LANCAMENTO-LOGCAIXA(3:2) TO DET-MINU
                      MOVE SEQUENCIA-LANCAMENTO-LOGCAIXA TO DET-SEQUE
                      MOVE USUARIO-LOGCAIXA              TO DET-USUARIO
                      MOVE OPERACAO-LOGCAIXA             TO DET-OPERACAO


                      MOVE DATA-MOV-LOGCAIXA(1:4)        TO DET-ANO-CX
                      MOVE DATA-MOV-LOGCAIXA(5:2)        TO DET-MES-CX
                      MOVE DATA-MOV-LOGCAIXA(7:2)        TO DET-DIA-CX
                      MOVE SEQ-LOGCAIXA                  TO DET-SEQUE-CX

                      MOVE TIPO-LCTO-LOGCAIXA            TO DET-TIPO

                      IF TIPO-LCTO-LOGCAIXA NOT < 50
                         MOVE VALOR-LOGCAIXA             TO DET-CREDITO
                      ELSE
                         MOVE VALOR-LOGCAIXA             TO DET-DEBITO
                      END-IF

                      MOVE HISTORICO-LOGCAIXA            TO DET-HISTORI
                      MOVE DOCUMENTO-LOGCAIXA            TO DET-DOCTO
                      MOVE CONTAPART-LOGCAIXA            TO DET-CONTPART
                      MOVE CONTA-REDUZ-LOGCAIXA          TO DET-CONTRED
                      MOVE CONTABIL-LOGCAIXA             TO DET-CONTABIL
                      MOVE RESPONSAVEL-LOGCAIXA          TO DET-RESPONS
                      MOVE DET-01                        TO LINDET-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.
       IMPRIMIR-FIM.
           EXIT.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP120-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CGD001 CXD040 LOGCAIXA.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

