       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP040e.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 16-08-2005
      *DESCRIÇÃO: Gera arquivo de CONTRATO P/ POPUP NO EXTRATO
      *  arquivo   - COD040X - EXECUTÁVEL
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY COPX040.
           COPY COPX040X.
           COPY IEPX010.
           COPY CAPX010.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY COPW040.
       COPY COPW040X.
       COPY IEPW010.
       COPY CAPW010.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD040X            PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W(FLAG)- PARA SABER SE OCORREU ERRO-ABERTURA ARQUIVO
      *  ERRO-W = 0 (NÃO)  ERRO-W = 1 (SIM)
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE SAIDA
           COPY "PARAMETR".

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE DS-CONTROL-BLOCK


           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.

       CORPO-PROGRAMA SECTION.
           INITIALIZE REG-CAD001
           START CAD001 KEY IS NOT LESS CODIGO-CA001 INVALID KEY
                 MOVE "10" TO ST-CAD001.

           PERFORM UNTIL ST-CAD001 = "10"
                 READ CAD001 NEXT AT END
                      MOVE "10" TO ST-CAD001
                 NOT AT END
                      PERFORM ABRIR-ARQUIVOS
                      PERFORM GERA-ARQUIVO
                      PERFORM FECHAR-ARQUIVOS
                 END-READ
           END-PERFORM

           CLOSE CAD001.

       ABRIR-ARQUIVOS SECTION.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD040"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD040
           MOVE "COD040X"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD040X
           MOVE "IED010"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-IED010
           MOVE "CAD010"               TO ARQ-REC.
           MOVE EMPRESA-REF            TO PATH-CAD010.
           OPEN I-O   IED010
                      CAD010
                      COD040

           CLOSE      IED010
                      CAD010
                      COD040

           OPEN INPUT IED010
                      CAD010
                      COD040

           OPEN I-O   COD040X
           IF ST-COD040X = "35"
              CLOSE       COD040X
              OPEN OUTPUT COD040X
              CLOSE       COD040X
              OPEN I-O    COD040X
           END-IF

           IF ST-COD040 <> "00"
              STRING "ERRO ABERTURA COD040: " ST-COD040 X"0DA0"
                      PATH-COD040 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-IED010 <> "00"
              STRING "ERRO ABERTURA IED010: " ST-IED010 X"0DA0"
                      PATH-IED010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CAD010 <> "00"
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 X"0DA0"
                      PATH-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-COD040X <> "00"
              STRING "ERRO ABERTURA COD040X " ST-COD040X X"0DA0"
                      PATH-COD040X INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       GERA-ARQUIVO SECTION.
           CLOSE       COD040X
           OPEN OUTPUT COD040X
           CLOSE       COD040X
           OPEN I-O    COD040X

           INITIALIZE REG-COD040X
           START COD040X KEY IS NOT LESS SEQ-CO40X INVALID KEY
                 MOVE "10" TO ST-COD040X.

           PERFORM UNTIL ST-COD040X = "10"
                 READ COD040X NEXT AT END
                      MOVE "10" TO ST-COD040X
                 NOT AT END
                      DELETE COD040X
                      END-DELETE
                 END-READ
           END-PERFORM


           INITIALIZE REG-COD040
           MOVE ZEROS                             TO SEQ-CO40X
           MOVE ZEROS                             TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10"                        TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT RECORD AT END
                      MOVE "10"                   TO ST-COD040
                 NOT AT END
                      ADD 1                       TO SEQ-CO40X
                      MOVE NR-CONTRATO-CO40       TO NR-CONTRATO-CO40X
                      MOVE IDENTIFICACAO-CO40     TO IDENTIFICACAO-CO40X
                      MOVE MESANO-PREV-CO40(1: 4) TO MESANO-W(3: 4)
                      MOVE MESANO-PREV-CO40(5: 2) TO MESANO-W(1: 2)
                      MOVE MESANO-W               TO MESANO-CO40X
                      MOVE INSTITUICAO-CO40       TO CODIGO-IE10
                      READ IED010 INVALID KEY
                           MOVE SPACES            TO SIGLA-IE10
                      END-READ
                      MOVE SIGLA-IE10             TO INSTITUICAO-CO40X
                      MOVE CIDADE-CO40            TO CIDADE
                      READ CAD010 INVALID KEY
                           MOVE SPACES            TO NOME-CID
                      END-READ
                      MOVE NOME-CID               TO CIDADE-CO40X
                      WRITE REG-COD040X INVALID KEY
                          REWRITE REG-COD040X
                          END-REWRITE
                      END-WRITE
                  END-READ
           END-PERFORM.

       FECHAR-ARQUIVOS SECTION.
           CLOSE COD040 IED010 CAD010 COD040X.

       LIMPAR-DADOS SECTION.
           INITIALIZE REG-COD040X.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.

       FINALIZAR-PROGRAMA SECTION.
           EXIT PROGRAM.
           STOP RUN.

