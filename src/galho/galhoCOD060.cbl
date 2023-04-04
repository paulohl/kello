       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCOD050.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-02-2011
      *DESCRIÇÃO: Conversão GALHOCOD060

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY COPX060.

           SELECT  COD060A ASSIGN TO PATH-COD060A
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD060A
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO60A = NR-CONTRATO-CO60A
                                               ITEM-CO60A
                   ALTERNATE RECORD KEY IS DATAREALIZA-CO60A
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO60A =
                                           NR-CONTRATO-CO60A
                                           DATAREALIZA-CO60A
                                           ITEM-CO60A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW060.

      *  CADASTRO DE EVENTOS
       FD  COD060A.
       01  REG-COD060A.
           05  NR-CONTRATO-CO60A      PIC 9(04).
           05  ITEM-CO60A             PIC 999.
           05  CODEVENTO-CO60A        PIC 999.
           05  DATA-SOLIC-CO60A       PIC 9(08)  COMP-3.
           05  DATAREALIZA-CO60A      PIC 9(08).
      *    DATAREALIZA-CO60 = AAAA/MM/DD.
           05  HORARIO-CO60A          PIC X(10).
           05  LOCAL-CO60A            PIC X(25).
           05  REFERENCIA-CO60A       PIC X(30).
           05  ENDERECO-CO60A         PIC X(40).
           05  PESSOA-CONTATO-CO60A   PIC X(30).
           05  UNIFORME-CO60A         PIC X(20).
      *    LOCAL, HORARIO, REFERENCIA, ENDERECO, PESSOA-CONTATO SÃO
      *    INFORMACOES REFERENTES A LOCALIZACAO DO EVENTO
           05  COD-COMISSAO-CO60A     PIC 9(4).
      *    COD-COMISSAO = NR-CONTRATO+COD-COMISSAO
           05  QT-PARTICIPANTE-CO60A  PIC 9(4).
           05  QT-TELAO-CO60A         PIC 9.
           05  FOTO-CO60A             PIC 9.
           05  VIDEO-CO60A            PIC 9.
           05  BECA-CO60A             PIC 9.
           05  CLIP-CO60A             PIC 9.
           05  FAX-CO60A              PIC 9.
           05  APROVACAO-CO60A        PIC 9.
           05  ORGANIZADOR-CO60A      PIC X(15).
           05  OBSERVACAO-CO60A       PIC X(80).
           05  NR-REL-REPOR-CO60A     PIC 9(6)   COMP-3.
           05  NR-PLANEJ-CO60A        PIC 9(8)   COMP-3.
           05  DATA-CANCELAM-CO60A    PIC 9(8)  COMP-3.
           05  HORA-CANCELAM-CO60A    PIC X(5).
           05  DT-PREV-REAL-CO60A     PIC 9(1).
      *    DATA-CANCELAM E HORAS-CANCELAM - DADOS P/ O CANCELAMETO DO
      *    EVENTO
      *    DT-PREV-REAL-CO60 -> 1 DATA-PREVISTA, 2 DATA-REALIZADA, 3

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD060A            PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MENSAGEM              PIC X(200).
           05  FLAG-CRITICA          PIC 9(01) VALUE ZEROS.
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.
           05  ACP-CAMINHO1          PIC X(255) VALUE SPACES.
           05  ACP-CAMINHO2          PIC X(255) VALUE SPACES.
           05  ACP-DTINI             PIC 9(08)  VALUE ZEROS.
           05  ACP-DTFIM             PIC 9(08)  VALUE ZEROS.
           05  WS-OK                 PIC X(01)  VALUE SPACES.
           05  DATA-INI              PIC 9(08)  VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)  VALUE ZEROS.
           05  RESP                  PIC X(01)  VALUE SPACES.
           05  ACHEI                 PIC X(01)  VALUE SPACES.
           05  QTDE-FORM             PIC 9(05)  VALUE ZEROS.
           05  MASC-VALOR            PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  AUX-PREVISTO          PIC 9(09)V99 VALUE ZEROS.
           05  AUX-VLRPAGO           PIC 9(09)V99 VALUE ZEROS.
           05  MASC1                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC2                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC3                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  MASC4                 PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu           pic 9(04).
             10 ws-mes-cpu           pic 9(02).
             10 ws-dia-cpu           pic 9(02).
          05 filler                  pic x(13).


       01 file-details.
          05 file-size               pic x(8) comp-x.
          05 file-date.
             10 dia                  pic x comp-x.
             10 month                pic x comp-x.
             10 year                 pic x(2) comp-x.
          05 file-time.
             10 hours                pic x comp-x.
             10 minutes              pic x comp-x.
             10 seconds              pic x comp-x.
             10 hundredths           pic x comp-x.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           display erase at 0101.

           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.


           initialize reg-cad001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
                      DISPLAY "CODIGO-CA001 = " CODIGO-CA001 STOP " "

                      perform renomear-arquivos
                      perform abrir-arquivos
                      perform converter-arquivo
                      perform fechar-arquivos

                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       renomear-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD060"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD060A

           OPEN I-O COD060A
           CLOSE    COD060A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD060"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD060

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD060W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD060A

           call "CBL_RENAME_FILE" using PATH-COD060
                                        PATH-COD060A
                              returning status-code

           STRING PATH-COD060 ".idx" DELIMITED BY " " INTO PATH-COD060

           STRING PATH-COD060A ".idx" DELIMITED BY " " INTO PATH-COD060A

           call "CBL_RENAME_FILE" using PATH-COD060
                                        PATH-COD060A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD060"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD060

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD060W"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD060A

           open i-o   cod060
           close      cod060
           open i-o   cod060

           open input cod060a.

       converter-arquivo section.

           INITIALIZE REG-COD060A
           START COD060A KEY IS NOT LESS CHAVE-CO60A INVALID KEY
                MOVE "10" TO ST-COD060A.

           PERFORM UNTIL ST-COD060A = "10"
                READ COD060A NEXT AT END
                     MOVE "10" TO ST-COD060A
                NOT AT END
                     DISPLAY "REG-COD060A = " REG-COD060A

                     MOVE NR-CONTRATO-CO60A      TO NR-CONTRATO-CO60
                     MOVE ITEM-CO60A             TO ITEM-CO60
                     MOVE CODEVENTO-CO60A        TO CODEVENTO-CO60
                     MOVE DATA-SOLIC-CO60A       TO DATA-SOLIC-CO60
                     MOVE DATAREALIZA-CO60A      TO DATAREALIZA-CO60
                     MOVE HORARIO-CO60A          TO HORARIO-CO60
                     MOVE LOCAL-CO60A            TO LOCAL-CO60
                     MOVE REFERENCIA-CO60A       TO REFERENCIA-CO60
                     MOVE ENDERECO-CO60A         TO ENDERECO-CO60
                     MOVE PESSOA-CONTATO-CO60A   TO PESSOA-CONTATO-CO60
                     MOVE UNIFORME-CO60A         TO UNIFORME-CO60
                     MOVE COD-COMISSAO-CO60A     TO COD-COMISSAO-CO60
                     MOVE QT-PARTICIPANTE-CO60A  TO QT-PARTICIPANTE-CO60
                     MOVE QT-TELAO-CO60A         TO QT-TELAO-CO60
                     MOVE FOTO-CO60A             TO FOTO-CO60
                     MOVE VIDEO-CO60A            TO VIDEO-CO60
                     MOVE BECA-CO60A             TO BECA-CO60
                     MOVE CLIP-CO60A             TO CLIP-CO60
                     MOVE FAX-CO60A              TO FAX-CO60
                     MOVE APROVACAO-CO60A        TO APROVACAO-CO60
                     MOVE ORGANIZADOR-CO60A      TO ORGANIZADOR-CO60
                     MOVE OBSERVACAO-CO60A       TO OBSERVACAO-CO60
                     MOVE NR-REL-REPOR-CO60A     TO NR-REL-REPOR-CO60
                     MOVE NR-PLANEJ-CO60A        TO NR-PLANEJ-CO60
                     MOVE DATA-CANCELAM-CO60A    TO DATA-CANCELAM-CO60
                     MOVE HORA-CANCELAM-CO60A    TO HORA-CANCELAM-CO60
                     MOVE DT-PREV-REAL-CO60A     TO DT-PREV-REAL-CO60

                     WRITE REG-COD060 INVALID KEY
                           MOVE "Erro de Gravação...COD060" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cod060 cod060a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
