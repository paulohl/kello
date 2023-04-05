       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO91.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 09-03-2009.
      *FUNÇÃO: ATUALIZA O ARQUIVO RED100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY REPX100.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY REPW100.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
      *    variáveis p/ listar os nomes com iniciais solicitadas
           05  ACP-MESANO            PIC 9(06)    VALUE ZEROS.
           05  MASC-MESANO           PIC 99/9999.
           05  AUX-MESANO            PIC 9(06)    VALUE ZEROS.

           COPY "PARAMETR".

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "RED100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED100
           CLOSE CONTROLE.
           OPEN I-O   RED100
           IF ST-RED100 = "35"
              CLOSE RED100      OPEN OUTPUT RED100
              CLOSE RED100      OPEN I-O    RED100
           END-IF.

       0010-PROCURAR-MESANO.
           DISPLAY ERASE AT 0101

           DISPLAY "Informar o Mês/Ano: " AT 0101
           MOVE ZEROS              TO ACP-MESANO
           MOVE ACP-MESANO         TO MASC-MESANO
           DISPLAY MASC-MESANO     AT 0121
           ACCEPT  MASC-MESANO     AT 0121

           MOVE MASC-MESANO        TO ACP-MESANO

           IF ACP-MESANO = 0
              GO TO 0010-PROCURAR-MESANO.

           INITIALIZE REG-RED100
           STRING ACP-MESANO(3:4) ACP-MESANO(1:2) INTO AUX-MESANO
           MOVE AUX-MESANO                          TO ANOMES-R100
           START RED100 KEY IS NOT LESS ANOMES-R100 INVALID KEY
                 MOVE "10" TO ST-RED100
                 DISPLAY ST-RED100 STOP "ST-RED100".

           PERFORM UNTIL ST-RED100 = "10"
                 READ RED100 NEXT RECORD AT END
                      MOVE "10" TO ST-RED100
                 NOT AT END
                      IF AUX-MESANO <> ANOMES-R100
                         MOVE "10" TO ST-RED100
                      ELSE
                         DISPLAY REG-RED100
                         MOVE 0 TO LCTO-CTA-CORR-R100
                         REWRITE REG-RED100 INVALID KEY
                             DISPLAY "Erro de Regravação...RED100" STOP
                             " "
                         END-REWRITE
                      END-IF
                 END-READ
           END-PERFORM

           CLOSE RED100
           EXIT PROGRAM
           STOP RUN.

