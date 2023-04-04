       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCGD001.
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

           COPY CGPX001.

           SELECT CGD001a ASSIGN TO PATH-CGD001a
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG01a
                  STATUS IS ST-CGD001a
                  ALTERNATE RECORD KEY IS NOME-CG01a.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CGPW001.

       FD  CGD001a.
       01  REG-CGD001a.
           05  CODIGO-CG01a         PIC 9(6).
           05  NOME-CG01a           PIC X(30).
           05  NOME-RED-CG01a       PIC X(13).
           05  SEXO-CG01a           PIC X(01).
           05  SITUACAO-CG01a       PIC 9(01).
           05  T-PESFIS-CG01a       PIC 9.
           05  T-PESJUR-CG01a       PIC 9.
           05  T-FUNC-CG01a         PIC 9.
           05  T-REPRES-CG01a       PIC 9.
           05  T-FOTOG-CG01a        PIC 9.
           05  T-CINEG-CG01a        PIC 9.
           05  T-VEND-CG01a         PIC 9.
           05  T-IMPOSTO-CG01a      PIC 9.
           05  T-INVESTIDOR-CG01a   PIC 9.
           05  OUTRO3-CG01a         PIC 9.
           05  T-TERCEIRIZADO-CG01a PIC 9.
           05  T-FRANQUIA-CG01a     PIC 9.
           05  COD-RED-CG01a        PIC 9(03).
           05  FILLER               PIC X(47).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD001A            PIC XX       VALUE SPACES.
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
           05  AUX-CODIGO            PIC 9(03)    VALUE ZEROS.

       01 AUX-DATA                   PIC 9(08).
       01 FILLER REDEFINES AUX-DATA.
          05 AUX-DIA                 PIC 9(02).
          05 AUX-MES                 PIC 9(02).
          05 AUX-ANO                 PIC 9(04).

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
      *    display "Informar a Empresa desejada: " at 0101
      *    accept aux-codigo                       at 0130

      *    move aux-codigo     to codigo-ca001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
      *               if aux-codigo <> codigo-ca001
      *                  move "10" to st-cad001
      *               else
                         DISPLAY "CODIGO-CA001 = " CODIGO-CA001
                         STOP " "

                         perform renomear-arquivos
                         perform abrir-arquivos
                         perform converter-arquivo
                         perform fechar-arquivos

                         display "ACABEI ESSA EMPRESA" STOP " "
      *               end-if
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       renomear-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD001A

           OPEN I-O CGD001A
           CLOSE    CGD001A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD001A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD001A

           call "CBL_RENAME_FILE" using PATH-CGD001
                                        PATH-CGD001A
                              returning status-code

           STRING PATH-CGD001 ".idx" DELIMITED BY " " INTO PATH-CGD001

           STRING PATH-CGD001A ".idx" DELIMITED BY " " INTO PATH-CGD001A

           call "CBL_RENAME_FILE" using PATH-CGD001
                                        PATH-CGD001A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD001A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD001A

           open i-o   cgd001
           close      cgd001
           open i-o   cgd001

           open input cgd001a.

       converter-arquivo section.

           INITIALIZE REG-CGD001A
           START CGD001A KEY IS NOT LESS CODIGO-CG01A INVALID KEY
                MOVE "10" TO ST-CGD001A.

           PERFORM UNTIL ST-CGD001A = "10"
                READ CGD001A NEXT AT END
                     MOVE "10" TO ST-CGD001A
                NOT AT END
                     DISPLAY "REG-CGD001A = " REG-CGD001A

                     INITIALIZE REG-CGD001

                     MOVE CODIGO-CG01a         TO CODIGO-CG01
                     MOVE NOME-CG01a           TO NOME-CG01
                     MOVE NOME-RED-CG01a       TO NOME-RED-CG01
                     MOVE SEXO-CG01a           TO SEXO-CG01
                     MOVE SITUACAO-CG01a       TO SITUACAO-CG01
                     MOVE T-PESFIS-CG01a       TO T-PESFIS-CG01
                     MOVE T-PESJUR-CG01a       TO T-PESJUR-CG01
                     MOVE T-FUNC-CG01a         TO T-FUNC-CG01
                     MOVE T-REPRES-CG01a       TO T-REPRES-CG01
                     MOVE T-FOTOG-CG01a        TO T-FOTOG-CG01
                     MOVE T-CINEG-CG01a        TO T-CINEG-CG01
                     MOVE T-VEND-CG01a         TO T-VEND-CG01
                     MOVE T-IMPOSTO-CG01a      TO T-IMPOSTO-CG01
                     MOVE T-INVESTIDOR-CG01a   TO T-INVESTIDOR-CG01
                     MOVE OUTRO3-CG01a         TO OUTRO3-CG01
                     MOVE T-TERCEIRIZADO-CG01a TO T-TERCEIRIZADO-CG01
                     MOVE T-FRANQUIA-CG01a     TO T-FRANQUIA-CG01
                     MOVE COD-RED-CG01a        TO COD-RED-CG01

                     WRITE REG-CGD001 INVALID KEY
                           MOVE "Erro de Gravação...CGD001" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cgd001 cgd001a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
