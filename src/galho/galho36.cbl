       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO36.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 22/02/2005
      *DESCRIÇÃO: PROGRAMA Q MOSTRA OS CONTRATOS Q NÃO SATISFAZEM A
      *REGRA => ENDERECO1 CIDADE1 CPF

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CAPX010.
           COPY CGPX010.
           COPY CGPX011.
           COPY RCPX100.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = CONTRATO-WK.

           SELECT RELAT ASSIGN TO PRINTER.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CAPW010.
           COPY CGPW010.
           COPY CGPW011.
           COPY RCPW100.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK        PIC 9(08).
           05  filler             pic x(01).
           05  DATA-MOVTO-WK      PIC X(10).
           05  filler             pic x(01).
           05  CLIENTE-WK         PIC X(30).
           05  filler             pic x(01).
           05  ENDERECO-WK        PIC X(30).
           05  filler             pic x(01).
           05  CIDADE-WK          PIC X(13).
           05  filler             pic x(01).
           05  CPF-WK             PIC 9(16).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).


       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "GALHO36.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  VARIA-W          PIC 9(8)          VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  ERRO-W                PIC 9(01).
           05  WS-OK                 PIC X(01).
           05  MOVTO-INI             PIC 9(08).
           05  MOVTO-FIM             PIC 9(08).
           05  total                 pic 9(08).

       01 AUX-ALBUM.
          05 AUX-LL                  PIC 9(04).
          05 AUX-CONTRATO            PIC 9(04).


       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(41)   VALUE
           "RELACAO CLIENTES C/ PROBLEMA: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "INTERV.MOVTO:  ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER             PIC X(09) VALUE "CONTRATO".
           05  FILLER             PIC X(11) VALUE "DATA MOVTO".
           05  FILLER             PIC X(30) VALUE "CLIENTE".
           05  FILLER             pic x(01).
           05  FILLER             PIC X(30) VALUE "ENDERECO".
           05  FILLER             pic x(01).
           05  FILLER             PIC X(13) VALUE "CIDADE".
           05  FILLER             pic x(01).
           05  FILLER             PIC X(16) VALUE "CPF".


       PROCEDURE DIVISION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE 0 TO ERRO-W
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100

           OPEN INPUT CAD010 CGD010 CGD011 RCD100
           CLOSE CONTROLE.

           ACCEPT VARIA-W FROM TIME.

           IF ST-CAD010 <> "00"
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CGD010 <> "00"
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CGD011 <> "00"
              STRING "ERRO ABERTURA CGD011: " ST-CGD011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-RCD100 <> "00"
              STRING "ERRO ABERTURA RCD100: " ST-RCD100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO-W = ZEROS
               PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CRITICA-TRUE
                    PERFORM CRITICAR-REGISTRO
               WHEN GS-CARREGAR-LB-TRUE
                    PERFORM GRAVAR-WORK
                    PERFORM CARREGAR-LB
               WHEN GS-IMPRIMIR-TRUE
                    PERFORM IMPRIMIR-RELATORIO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CRITICAR-REGISTRO SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-DTINI"       PERFORM CRITICAR-DTINI
               WHEN "EF-DTFIM"       PERFORM CRITICAR-DTFIM
               WHEN "SB-IMPRESSORA"  PERFORM CRITICAR-IMPRESSORA
               WHEN "REGISTRO"       PERFORM CRITICAR-DTINI
                                        THRU CRITICAR-IMPRESSORA
           END-EVALUATE.

       CRITICAR-DTINI SECTION.
           IF MENSAGEM EQUAL SPACES
               IF GS-ACP-DTINI EQUAL ZEROS
                   MOVE "Data inicial não Informada" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   CALL "UTIVLDT" USING GS-ACP-DTINI WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                      MOVE "Data inicial Inválida" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DTFIM SECTION.
           IF MENSAGEM EQUAL SPACES
               IF GS-ACP-DTFIM EQUAL ZEROS
                   MOVE "Data final não Informada" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   CALL "UTIVLDT" USING GS-ACP-DTFIM WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                      MOVE "Data final Inválida" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       CRITICAR-IMPRESSORA SECTION.
           IF MENSAGEM EQUAL SPACES
               IF GS-IMPRESSORA <> "Epson" AND "Hp"
                  MOVE "Impressora Informada Inválida" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       GRAVAR-WORK SECTION.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE GS-ACP-DTINI TO DATA-INV  VENCTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-INI.
           MOVE GS-ACP-DTFIM TO DATA-INV  VENCTO-FIM-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO MOVTO-FIM.


           INITIALIZE REG-RCD100

           MOVE MOVTO-INI      TO DATA-MOVTO-REC
           COMPUTE DATA-MOVTO-REC = DATA-MOVTO-REC - 1
           MOVE ALL "9"        TO ALBUM-REC

           START RCD100 KEY IS GREATER THAN ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT RECORD AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   IF DATA-MOVTO-REC > MOVTO-FIM
                      MOVE "10" TO ST-RCD100
                   ELSE
                      MOVE ZEROS       TO CLASSIF-CG10
                      MOVE ALBUM-REC   TO CODIGO-CG10
                      READ CGD010 INVALID KEY
                           INITIALIZE REG-CGD010
                           INITIALIZE REG-CGD011
                           PERFORM MOVER-DADOS
                      NOT INVALID KEY
                          MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                          READ CGD011 INVALID KEY
                               INITIALIZE REG-CGD011
                               PERFORM MOVER-DADOS
                          NOT INVALID KEY
                               MOVE CIDADE1-CG11 TO CIDADE
                               READ CAD010 INVALID KEY
                                   INITIALIZE REG-CAD010
                               END-READ
                               IF ENDERECO1-CG11 = SPACES OR
                                  NOME-CID       = SPACES OR
                                  CPF-CG11       = 0      OR
                                  COMPRADOR-CG10 = SPACES
                                  PERFORM MOVER-DADOS
                               END-IF
                          END-READ
                      END-READ
                   END-IF
               END-READ
           END-PERFORM

           CLOSE WORK.

       MOVER-DADOS SECTION.
           MOVE ALBUM-REC                TO   AUX-ALBUM
           IF AUX-CONTRATO > 0
              MOVE ALBUM-REC             TO   CONTRATO-WK
              MOVE COMPRADOR-CG10        TO   CLIENTE-WK
              STRING DATA-MOVTO-REC(1:4) INTO DATA-MOVTO-WK(7:4)
              STRING "/"                 INTO DATA-MOVTO-WK(6:1)
              STRING DATA-MOVTO-REC(5:6) INTO DATA-MOVTO-WK(4:2)
              STRING "/"                 INTO DATA-MOVTO-WK(3:1)
              STRING DATA-MOVTO-REC(7:8) INTO DATA-MOVTO-WK(1:2)
              MOVE ENDERECO1-CG11        TO   ENDERECO-WK
              MOVE NOME-CID              TO   CIDADE-WK
              MOVE CPF-CG11              TO   CPF-WK
              MOVE SPACES TO GS-STATUS
              STRING "MOVENDO DADOS " REG-WORK INTO GS-STATUS
              MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-WORK.

       CARREGAR-LB SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           open input work
           INITIALIZE REG-WORK total
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   add 1 to total
                   MOVE SPACES TO GS-STATUS
                   STRING "CARREGANDO DADOS " REG-WORK INTO GS-STATUS
                   MOVE REG-WORK TO GS-LINDET
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
               END-READ
           END-PERFORM.

           MOVE SPACES TO GS-STATUS

           string "total = " total into gs-status
           move "ALTERAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           close work.

       IMPRIMIR-RELATORIO SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           if gs-impressora = "Epson"
              DISPLAY COND-EP UPON LPRINTER
           else
              DISPLAY COND-HP UPON LPRINTER.

           open output relat

           PERFORM CABECALHO

           open input work
           INITIALIZE REG-WORK total
           START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   add 1 to total
                   MOVE SPACES TO GS-STATUS
                   STRING "CARREGANDO DADOS " REG-WORK INTO GS-STATUS
                   MOVE REG-WORK TO GS-LINDET
                   MOVE "INSERIR-LB" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-RELAT FROM REG-WORK
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
               END-READ
           END-PERFORM.

           MOVE SPACES TO GS-STATUS

           WRITE REG-RELAT FROM SPACES AFTER PAGE

           string "total = " total into gs-status
           move "ALTERAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           close work RELAT.

           if gs-impressora = "Epson"
              DISPLAY DESCOND-EP UPON LPRINTER
           else
              DISPLAY DESCOND-HP UPON LPRINTER.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

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


       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM"
           MOVE 1 TO ERRO-W.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "GALHO36" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD010 CGD011 CAD010 RCD100

           EXIT PROGRAM
           STOP RUN.


