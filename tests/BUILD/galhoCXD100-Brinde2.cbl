       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCXD100.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 23-08-2010
      *DESCRIÇÃO: Conversão GALHOCXD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY COPX002.

           COPY CXPX100.

           COPY CPPX020.

           COPY COPX040.

           COPY COPX050.

           select arqlog assign to arquivo-arqlog
                         organization is line sequential
                         access mode is sequential
                         file status is fs-arqlog.

           select arqlog2 assign to arquivo-arqlog2
                          organization is line sequential
                          access mode is sequential
                          file status is fs-arqlog2.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW002.

           COPY CXPW100.

           COPY CPPW020.

           COPY COPW040.

           COPY COPW050.

       fd arqlog.
       01 reg-arqlog                   pic x(100).

       fd arqlog2.
       01 reg-arqlog2                  pic x(100).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  FS-ARQLOG             PIC XX       VALUE SPACES.
           05  FS-ARQLOG2            PIC XX       VALUE SPACES.
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
           05  DATA-ANT.
               10 DIA-ANT            PIC 99.
               10 FILLER             PIC X(01) VALUE "/".
               10 MES-ANT            PIC 99.
               10 FILLER             PIC X(01) VALUE "/".
               10 ANO-ANT            PIC 9999.
           05  DATA-ATUAL.
               10 DIA-ATUAL          PIC 99.
               10 FILLER             PIC X(01) VALUE "/".
               10 MES-ATUAL          PIC 99.
               10 FILLER             PIC X(01) VALUE "/".
               10 ANO-ATUAL          PIC 9999.

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
                      perform abrir-arquivos
                      perform ajustar-saldo
                      perform fechar-arquivos
                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CXD100"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD100

           display "path-cxd100 = " path-cxd100 stop " "

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CPD020"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPD020

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD040"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD040

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD050"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD050

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CXD100.TXT"           TO ARQ-REC
           MOVE EMPRESA-REF            TO ARQUIVO-ARQLOG

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "BRIND.TXT"            TO ARQ-REC
           MOVE EMPRESA-REF            TO ARQUIVO-ARQLOG2

           OPEN I-O CPD020 CXD100 COD002 COD040 COD050

           OPEN OUTPUT ARQLOG ARQLOG2.

       ajustar-saldo section.

           INITIALIZE REG-CXD100
           START CXD100 KEY IS NOT LESS CHAVE-CX100 INVALID KEY
                MOVE "10" TO ST-CXD100.

           PERFORM UNTIL ST-CXD100 = "10"
                READ CXD100 NEXT AT END
                     MOVE "10" TO ST-CXD100
                NOT AT END
                     DISPLAY "REG-CXD100 = " REG-CXD100

                     IF TIPO-LCTO-CX100 = 3
                        MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(1:4)) TO
                              NR-CONTRATO-CO40
                             NR-CONTRATO-CO50
                        MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(6:3)) TO
                             ITEM-CO50

                        READ COD040 INVALID KEY
                             MOVE SPACES TO MENSAGEM
                             STRING "Contrato Não Encontrado" X"0DA0"
                                    "NR-CONTRATO-CO40 => "
                                     NR-CONTRATO-CO40 INTO MENSAGEM
                               MOVE "C" TO TIPO-MSG
                               PERFORM EXIBIR-MENSAGEM
                        NOT INVALID KEY
                               READ COD050 INVALID KEY
                                    STRING "Brinde Não Encontrado"
                                            X"0DA0"
                                           "NR-CONTRATO-CO50 => "
                                            NR-CONTRATO-CO50
                                            X"0DA0"
                                           "ITEM-CO50 => "
                                            ITEM-CO50 INTO MENSAGEM
                                      MOVE "C" TO TIPO-MSG
                                    PERFORM EXIBIR-MENSAGEM
                               NOT INVALID KEY
                                    IF DATA-PAGTO-CO50 <> DATA-MOV-CX100
                                       MOVE DATA-PAGTO-CO50(1:4)
                                         TO ANO-ANT
                                       MOVE DATA-PAGTO-CO50(5:2)
                                         TO MES-ANT
                                       MOVE DATA-PAGTO-CO50(7:2)
                                         TO DIA-ANT

                                       MOVE DATA-MOV-CX100 TO
                                            DATA-PAGTO-CO50

                                       MOVE DATA-MOV-CX100(1:4)
                                         TO ANO-ATUAL
                                       MOVE DATA-MOV-CX100(5:2)
                                         TO MES-ATUAL
                                       MOVE DATA-MOV-CX100(7:2)
                                         TO DIA-ATUAL

                                       DISPLAY ERASE AT 0101
                                       DISPLAY "VOU ATUALIZAR" AT 0101

                                       DISPLAY "NR-CONTRATO-CO50 => "
                                                               AT 0301
                                       DISPLAY NR-CONTRATO-CO50 AT 0321

                                       DISPLAY "ITEM-CO50        => "
                                                               AT 0401
                                       DISPLAY ITEM-CO50       AT 0421

                                       DISPLAY "CODBRINDE-CO50   => "
                                                               AT 0501
                                       DISPLAY CODBRINDE-CO50  AT 0521

                                       DISPLAY "DATA-PGTO        => "
                                                               AT 0601
                                       DISPLAY DATA-PAGTO-CO50 AT 0621

      *                                STOP " "

                                       REWRITE REG-COD050 INVALID KEY
                                          MOVE "Erro de Regravação...COD
      -                                        "050" TO MENSAGEM
                                          MOVE "C" TO TIPO-MSG
                                          PERFORM EXIBIR-MENSAGEM
                                       NOT INVALID KEY
                                          MOVE SPACES TO MENSAGEM
                                          STRING "Contrato/Item => "
                                            NR-CONTRATO-CO50 "/"
                                            ITEM-CO50 x"0da0"
                                            "Brinde => " CODBRINDE-CO50
                                            x"0da0"
                                            "Data Pagto Ant => "
                                             data-ant
                                            "Data Pagto Atual => "
                                             data-atual
                                            into mensagem
                                          PERFORM EXIBIR-MENSAGEM2
                                       END-REWRITE
                                    END-IF
                               END-READ
                        END-READ
                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cpd020 cxd100 cod040 cod050 cod002 arqlog.

       EXIBIR-MENSAGEM SECTION.
           MOVE MENSAGEM TO REG-ARQLOG
           WRITE REG-ARQLOG
      *    MOVE    SPACES TO RESP-MSG.
      *    CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
      *    CANCEL  "MENSAGEM".
           move spaces to mensagem.
      *    move 1      to flag-critica.

       EXIBIR-MENSAGEM2 SECTION.
           MOVE MENSAGEM TO REG-ARQLOG2
           WRITE REG-ARQLOG2
      *    MOVE    SPACES TO RESP-MSG.
      *    CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
      *    CANCEL  "MENSAGEM".
           move spaces to mensagem.
      *    move 1      to flag-critica.

