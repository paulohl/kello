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

           SELECT aMTD020 ASSIGN       TO     PATH-aMTD020
                          ORGANIZATION IS          INDEXED
                          ACCESS MODE  IS          DYNAMIC
                          STATUS       IS       ST-aMTD020
                          LOCK MODE    IS        AUTOMATIC
                          WITH LOCK    ON           RECORD
                          RECORD KEY   IS       ALBUM-aMTG
                          ALTERNATE    RECORD       KEY IS
                                              CHAVE-aMTG =
                                              DATAMOV-aMTG,
                                                ALBUM-aMTG
                          ALTERNATE    RECORD       KEY IS
                                        ANOMES-VISITA-aMTG
                                           WITH DUPLICATES
                          ALTERNATE    RECORD       KEY IS
                                                ALT-aMTG =
                                         DATAROMANEIO-aMTG,
                                                ALBUM-aMTG.

           SELECT aRCD100 ASSIGN       TO     PATH-aRCD100
                          ORGANIZATION IS          INDEXED
                          ACCESS MODE  IS          DYNAMIC
                          LOCK MODE    IS        AUTOMATIC
                          WITH LOCK    ON           RECORD
                          STATUS       IS       ST-aRCD100
                          RECORD KEY   IS CHAVE-ALBUM-aREC
                          ALTERNATE    RECORD       KEY IS
                                ALT-aREC = DATA-MOVTO-aREC
                                          CHAVE-ALBUM-aREC
                          ALTERNATE    RECORD       KEY IS
                              DATAVEN-aREC WITH DUPLICATES.

           SELECT aRCD101 ASSIGN       TO      PATH-aRCD101
                          ORGANIZATION IS           INDEXED
                          ACCESS MODE  IS           DYNAMIC
                          STATUS       IS        ST-aRCD101
                          LOCK MODE    IS         AUTOMATIC
                          WITH LOCK    ON            RECORD
                          RECORD KEY   IS     CHAVE-aREC1 =
                                          CHAVE-ALBUM-aREC1
                                               VENCTO-aREC1
                                                BANCO-aREC1
                                               NUMERO-aREC1
                                              PARCELA-aREC1
                          ALTERNATE    RECORD        KEY IS
                          CHAVE-ALBUM-aREC1 WITH DUPLICATES
                          ALTERNATE RECORD KEY IS ALT-aREC1 =
                                            DTA-BAIXA-aREC1
                          CHAVE-ALBUM-aREC1 WITH DUPLICATES.



       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

      *ARQUIVO DE MONTAGEM NO ALBUM
       FD  aMTD020.
       01  REG-aMTD020.
           05  ALBUM-aMTG.
               10  CONTRATO-aMTG    PIC 9(4).
               10  NRALBUM-aMTG     PIC 9(4).
           05  DATAMOV-aMTG         PIC 9(8).
           05  QT-ESTOJO-aMTG       PIC 9.
           05  QT-ENCADER-aMTG      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-aMTG       PIC 9999.
           05  QT-FOTOS-aMTG        PIC 9999.
           05  QT-FITAS-aMTG        PIC 9.
           05  QT-POSTER-aMTG       PIC 9.
           05  QT-PORTA-FITA-aMTG   PIC 9.
           05  QT-FOTO-CD-aMTG      PIC 99.
           05  QT-MOLDURA-aMTG      PIC 99.
           05  QT-PORTA-DVD-aMTG    PIC 99.
           05  FOGO-aMTG            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-aMTG       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG   PIC 9(6).
           05  VISITA-MTG          PIC 999.
           05  POSSE-MTG           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem  4-revendido
           05  CODIGO-POSSE-MTG    PIC 9(6).
           05  QT-DVD-MTG          PIC 9(1).
           05  NAO-GEROU-ALBUM-MTG PIC 9(1).
           05  DATAROMANEIO-MTG    PIC 9(8).
           05  QT-BOOK-MTG         PIC 9(2).
           05  FILLER              PIC X(38).

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
           05  ST-OED020             PIC XX       VALUE SPACES.
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
           05  QTDE                  PIC 9(09) VALUE ZEROS.
           05  MASC-QTDE             PIC ZZZ.ZZZ.ZZ9.
           05  AUX-EMPRESA           PIC X(04) VALUE SPACES.

           COPY "CPDIAS1".

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
           display "Informar a Empresa desejada: " at 0101
           accept aux-empresa                      at 0130


           move aux-empresa to codigo-ca001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
                      if aux-empresa <> spaces and
                         aux-empresa <> codigo-ca001
                         move "10" to st-cad001
                      else
                         DISPLAY "CODIGO-CA001 = " CODIGO-CA001 STOP " "
                         perform abrir-arquivos
                         perform ajustar-saldo
                         perform fechar-arquivos
                         display "ACABEI ESSA EMPRESA" STOP " "
                      end-if
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
           MOVE "OED020"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-OED020

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "COD002"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-COD002

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CXD100.TXT"           TO ARQ-REC
           MOVE EMPRESA-REF            TO ARQUIVO-ARQLOG

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "BRINDES.TXT"          TO ARQ-REC
           MOVE EMPRESA-REF            TO ARQUIVO-ARQLOG2

           OPEN I-O CPD020 CXD100 COD002 COD040 COD050 OED020

           OPEN OUTPUT ARQLOG ARQLOG2.

       ajustar-saldo section.

      *    INITIALIZE REG-CXD100
      *    START CXD100 KEY IS NOT LESS CHAVE-CX100 INVALID KEY
      *         MOVE "10" TO ST-CXD100.
      *
      *    PERFORM UNTIL ST-CXD100 = "10"
      *         READ CXD100 NEXT AT END
      *              MOVE "10" TO ST-CXD100
      *         NOT AT END
      *              DISPLAY "REG-CXD100 = " REG-CXD100
      *
      *              IF TIPO-LCTO-CX100 = 3
      *                 MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(1:4)) TO
      *                       NR-CONTRATO-CO40
      *                      NR-CONTRATO-CO50
      *                 MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(6:3)) TO
      *                      ITEM-CO50
      *
      *                 READ COD040 INVALID KEY
      *                      MOVE SPACES TO MENSAGEM
      *                      STRING "Contrato Não Encontrado" X"0DA0"
      *                             "NR-CONTRATO-CO40 => "
      *                              NR-CONTRATO-CO40 INTO MENSAGEM
      *                        MOVE "C" TO TIPO-MSG
      *                        PERFORM EXIBIR-MENSAGEM
      *                 NOT INVALID KEY
      *                        READ COD050 INVALID KEY
      *                             STRING "Brinde Não Encontrado"
      *                                     X"0DA0"
      *                                    "NR-CONTRATO-CO50 => "
      *                                     NR-CONTRATO-CO50
      *                                     X"0DA0"
      *                                    "ITEM-CO50 => "
      *                                     ITEM-CO50 INTO MENSAGEM
      *                               MOVE "C" TO TIPO-MSG
      *                             PERFORM EXIBIR-MENSAGEM
      *                        NOT INVALID KEY
      *                             MOVE CODBRINDE-CO50 TO CODIGO-CO02
      *                             READ COD002 INVALID KEY
      *                                  MOVE ZEROS    TO VALOR-CO02
      *                                  MOVE 0        TO MULT-FORM-CO02
      *                             END-READ
      *
      *                             IF MULT-FORM-CO02 = "S" OR "s"
      *                                COMPUTE QTDE-FORM =
      *                                        QTDE-FORM-CO50 *
      *                                        QTDE-FORM-CO40
      *                             ELSE
      *                                MOVE QTDE-FORM-CO50  TO
      *                                     QTDE-FORM
      *                             END-IF
      *
      *                             PERFORM PROCURAR-CPD020
      *                             IF ACHEI = "S"
      *                                DISPLAY ERASE AT 0101
      *                                DISPLAY "VOU ATUALIZAR" AT 0101
      *
      *                                DISPLAY "NR-CONTRATO-CO50 => "
      *                                                        AT 0301
      *                                DISPLAY NR-CONTRATO-CO50 AT 0321
      *
      *                                DISPLAY "ITEM-CO50        => "
      *                                                        AT 0401
      *                                DISPLAY ITEM-CO50       AT 0421
      *
      *                                DISPLAY "CODBRINDE-CO50   => "
      *                                                        AT 0501
      *                                DISPLAY CODBRINDE-CO50  AT 0521
      *
      *                                DISPLAY "VLR-PREVISTO     => "
      *                                                        AT 0601
      *                                MOVE VALOR-PREVISTO-CO50 TO
      *                                     MASC-VALOR
      *                                DISPLAY MASC-VALOR      AT 0621
      *
      *                                DISPLAY "VLR-PAGO         => "
      *                                                        AT 0701
      *                                MOVE VALOR-PAGO-CO50     TO
      *                                     MASC-VALOR
      *                                DISPLAY MASC-VALOR      AT 0721
      *
      *                                DISPLAY "DATA-PGTO        => "
      *                                                        AT 0801
      *                                DISPLAY DATA-PAGTO-CO50 AT 0821
      *
      *                                STOP " "
      *
      *                                REWRITE REG-COD050 INVALID KEY
      *                                   MOVE "Erro de Regravação...COD
      *                                        "050" TO MENSAGEM
      *                                   MOVE "C" TO TIPO-MSG
      *                                   PERFORM EXIBIR-MENSAGEM
      *                                NOT INVALID KEY
      *                                   MOVE AUX-PREVISTO TO MASC1
      *                                   MOVE AUX-VLRPAGO  TO MASC2
      *
      *                                   MOVE VALOR-PREVISTO-CO50 TO
      *                                   MASC3
      *                                   MOVE VALOR-PAGO-CO50 TO MASC4
      *
      *                                   MOVE SPACES TO MENSAGEM
      *                                   STRING "Contrato/Item => "
      *                                     NR-CONTRATO-CO50 "/"
      *                                     ITEM-CO50 x"0da0"
      *                                     "Brinde => " CODBRINDE-CO50
      *                                     x"0da0"
      *                                     "VlrPrevisto Ant => " masc1
      *                                     " Atual => " masc3 x"0da0"
      *                                     "Valor Pago Ant => " masc2
      *                                     " Atual => " masc4 into
      *                                     mensagem
      *                                   PERFORM EXIBIR-MENSAGEM2
      *                                END-REWRITE
      *                             END-IF
      *                        END-READ
      *                 END-READ
      *              ELSE
      *                 IF TIPO-LCTO-CX100 = 6
      *                    MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(1:4))
      *                      TO NR-CONTRATO-CO40
      *                    MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-OE20
      *
      *                    MOVE FUNCTION NUMVAL(DOCUMENTO-CX100(6:3))
      *                      TO ITEM-OE20
      *
      *                    READ COD040 INVALID KEY
      *                         DISPLAY "NAO ACHEI O COD040 = "
      *                                  NR-CONTRATO-CO40 STOP " "
      *                         EXIT PERFORM
      *                    NOT INVALID KEY
      *                         READ OED020 INVALID KEY
      *                              DISPLAY "BRINDE INEXISTENTE = "
      *                                    NR-CONTRATO-CO40
      *                                    " " ITEM-OE20 STOP " "
      *                         NOT INVALID KEY
      *                              MOVE CODBRINDE-OE20 TO CODIGO-CO02
      *                              READ COD002 INVALID KEY
      *                                   MOVE ZEROS  TO VALOR-CO02
      *                                   MOVE 0      TO MULT-FORM-CO02
      *                             END-READ
      *
      *                             IF MULT-FORM-CO02 = "S" OR "s"
      *                                COMPUTE QTDE-FORM =
      *                                        QTDE-FORM-OE20 *
      *                                        QTDE-FORM-CO40
      *                             ELSE
      *                                MOVE QTDE-FORM-OE20 TO QTDE-FORM
      *                             END-IF
      *                             COMPUTE VALOR-PREVISTO-OE20 =
      *                                     VALOR-TOT-CP20 / QTDE-FORM
      *                             MOVE VALOR-TOT-CP20      TO
      *                                  VALOR-PAGO-OE20
      *
      *                             MOVE 1              TO
      *                                  REALIZADO-OE20
      *                             MOVE DATA-MOV-CX100 TO
      *                                  DATA-PAGTO-OE20
      *                             PERFORM CALCULA-DIAS-PRAZO-OE
      *                             REWRITE REG-OED020 INVALID KEY
      *                                 MOVE "Erro de Regravação...OED02
      *                                      "0" TO MENSAGEM
      *                                 MOVE "C" TO TIPO-MSG
      *                                 PERFORM EXIBIR-MENSAGEM
      *                             END-REWRITE
      *                         END-READ
      *                    END-READ
      *                 END-IF
      *              END-IF
      *         END-READ
      *    END-PERFORM


           INITIALIZE REG-CPD020
                      QTDE
           MOVE 1 TO DATA-PGTO-CP20
           START CPD020 KEY IS NOT LESS ALT6-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT AT END
                      MOVE "10" TO ST-CPD020
                 NOT AT END
                      DISPLAY "REG-CPD020 = " REG-CPD020
                      IF DATA-PGTO-CP20 > 0
                         IF SEQ-CAIXA-CP20 > 0
                            IF VALOR-LIQ-CP20 NOT > 0
                               MOVE DATA-PGTO-CP20 TO DATA-MOV-CX100
                               MOVE SEQ-CAIXA-CP20 TO SEQ-CX100
                               READ CXD100 NOT INVALID KEY
                                    ADD 1              TO QTDE
                                    MOVE QTDE          TO MASC-QTDE
                                    DISPLAY "ALTEREI " AT 0501
                                    DISPLAY MASC-QTDE  AT 0520
                                    MOVE VALOR-CX100 TO VALOR-LIQ-CP20
                                    REWRITE REG-CPD020 INVALID KEY
                                        MOVE "Erro de GRAVACAO...CPD020"
                                         TO MENSAGEM
                                        PERFORM EXIBIR-MENSAGEM
                                    END-REWRITE
                               END-READ
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       CALCULA-DIAS-PRAZO-OE SECTION.
           MOVE DATA-PREV-VENDA-CO40 TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO GRDIAS-AAMMDD-FINAL
           MOVE DATA-VENCTO-OE20     TO GRDIAS-AAMMDD-INICIAL
           IF GRDIAS-AAMMDD-INICIAL > GRDIAS-AAMMDD-FINAL
              MOVE ZEROS TO DIAS-PRAZO-CO50
           ELSE
              CALL "GRDIAS1" USING PARAMETROS-GRDIAS
              CANCEL "GRDIAS1"
              MOVE GRDIAS-NUM-DIAS TO DIAS-PRAZO-OE20.

      *PROCURAR-CPD020 SECTION.
      *    MOVE VALOR-PREVISTO-CO50    TO AUX-PREVISTO
      *    MOVE VALOR-PAGO-CO50        TO AUX-VLRPAGO
      *
      *    MOVE "N" TO ACHEI
      *    INITIALIZE REG-CPD020
      *    MOVE SEQ-CX100           TO SEQ-CAIXA-CP20
      *    MOVE DATA-MOV-CX100      TO DATA-PGTO-CP20
      *    START CPD020 KEY IS = ALT6-CP20 INVALID KEY
      *         MOVE "10" TO ST-CPD020
      *    END-START
      *    PERFORM UNTIL ST-CPD020 = "10"
      *         READ CPD020 NEXT AT END
      *              MOVE "10" TO ST-CPD020
      *         NOT AT END
      *              IF SEQ-CX100      <> SEQ-CAIXA-CP20 OR
      *                 DATA-MOV-CX100 <> DATA-PGTO-CP20
      *                 MOVE "10" TO ST-CPD020
      *              ELSE
      *                 IF NR-DOCTO-CP20 = DOCUMENTO-CX100
      *                   COMPUTE VALOR-PREVISTO-CO50 = VALOR-TOT-CP20 /
      *                           QTDE-FORM
      *                   MOVE VALOR-TOT-CP20      TO VALOR-PAGO-CO50
      *
      *                   IF VALOR-LIQ-CP20 NOT > 0
      *                      MOVE VALOR-PAGO-CO50 TO VALOR-LIQ-CP20
      *                      REWRITE REG-CPD020 INVALID KEY
      *                          MOVE "ERRO DE GRAVACAO CPD020" TO
      *                          MENSAGEM
      *                          PERFORM EXIBIR-MENSAGEM
      *                      END-REWRITE
      *                   END-IF
      *
      *                   MOVE "S" TO ACHEI
      *                 END-IF
      *              END-IF
      *         END-READ
      *    END-PERFORM.

       fechar-arquivos section.
           close cpd020 cxd100 cod040 cod050 cod002 oed020 arqlog.

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

