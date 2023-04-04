       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCGD011.
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

           COPY CGPX011.

           SELECT CGD011A ASSIGN TO PATH-CGD011A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG11A
                  ALTERNATE RECORD KEY IS
                  ALT2-CG11A =   CPF-CG11A
                              CODIGO-CG11A
                  WITH DUPLICATES
                  STATUS IS ST-CGD011A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CGPW011.

       FD  CGD011A.
       01  REG-CGD011A.
           05  COD-COMPL-CG11A.
               10  CLASSIF-CG11A      PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG11A       PIC 9(8).
           05  ENDERECO1-CG11A        PIC X(45).
           05  COMPLEMENTO1-CG11A     PIC X(30).
           05  PONTO-REFER1-CG11A     PIC X(40).
           05  BAIRRO1-CG11A          PIC X(25).
           05  CIDADE1-CG11A          PIC 9(4).
           05  CEP1-CG11A             PIC 9(8).
           05  FONE1-CG11A            PIC 9(8).
           05  CX-POSTAL1-CG11A       PIC 9(5).
           05  EMPRESA-CG11A          PIC X(30).
           05  ENDERECO2-CG11A        PIC X(45).
           05  COMPLEMENTO2-CG11A     PIC X(30).
           05  PONTO-REFER2-CG11A     PIC X(40).
           05  BAIRRO2-CG11A          PIC X(25).
           05  CIDADE2-CG11A          PIC 9(4).
           05  CEP2-CG11A             PIC 9(8).
           05  FONE2-CG11A            PIC 9(8).
           05  RAMAL2-CG11A           PIC 9(3).
           05  CX-POSTAL2-CG11A       PIC 9(5).
           05  E-MAIL-CG11A           PIC X(30).
           05  CELULAR-CG11A          PIC 9(8).
           05  FAX-CG11A              PIC 9(8).
           05  CPF-CG11A              PIC 9(16).
           05  RG-CG11A               PIC X(15).
           05  DT-EXPEDICAO-CG11A     PIC 9(8).
           05  ORGAO-EXPEDICAO-CG11A  PIC X(8).
           05  DATA-NASC-CG11A        PIC 9(8).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  NOME-PAI-CG11A         PIC X(30).
           05  NOME-MAE-CG11A         PIC X(30).
           05  SITUACAO-CLI-CG11A     PIC 9.
           05  TURMA-CG11A            PIC X(03).
           05  TURNO-CG11A            PIC X(10).
           05  ENDERECO-PAIS-CG11A    PIC X(45).
           05  BAIRRO-PAIS-CG11A      PIC X(25).
           05  CIDADE-PAIS-CG11A      PIC 9(4).
           05  FONE-PAIS-CG11A        PIC 9(8).
           05  CELULAR-PAIS-CG11A     PIC 9(8).
           05  CEP-PAIS-CG11A         PIC 9(8).
           05  ENDERECO-REP-CG11A     PIC X(45).
           05  COMPLEMENTO-PAIS-CG11A PIC X(30).
           05  DDDD-CELULAR-CG11A     PIC 9(02).
           05  SEXO-CG11A             PIC X(01).
           05  BAIRRO-REP-CG11A       PIC X(25).
           05  CIDADE-REP-CG11A       PIC 9(4).
           05  CEP-REP-CG11A          PIC 9(8).
           05  FILLER                 PIC X(60).
      *    SITUAÇÃO 0(OK)   1-PROTESTADO

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD011A            PIC XX       VALUE SPACES.
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
           MOVE "CGD011"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011A

           OPEN I-O CGD011A
           CLOSE    CGD011A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011A

           call "CBL_RENAME_FILE" using PATH-CGD011
                                        PATH-CGD011A
                              returning status-code

           STRING PATH-CGD011 ".idx" DELIMITED BY " " INTO PATH-CGD011

           STRING PATH-CGD011A ".idx" DELIMITED BY " " INTO PATH-CGD011A

           call "CBL_RENAME_FILE" using PATH-CGD011
                                        PATH-CGD011A
                              returning status-code.
       renomear-arquivos-fim.
           exit.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011A

           open i-o   cgd011
           close      cgd011
           open i-o   cgd011

           open input cgd011a.

       converter-arquivo section.

           INITIALIZE REG-CGD011A
           START CGD011A KEY IS NOT LESS COD-COMPL-CG11A INVALID KEY
                MOVE "10" TO ST-CGD011A.

           PERFORM UNTIL ST-CGD011A = "10"
                READ CGD011A NEXT AT END
                     MOVE "10" TO ST-CGD011A
                NOT AT END
                     DISPLAY "REG-CGD011A = " REG-CGD011A

                     INITIALIZE REG-CGD011

                     MOVE CLASSIF-CG11A          TO CLASSIF-CG11
                     MOVE CODIGO-CG11A           TO CODIGO-CG11
                     MOVE ENDERECO1-CG11A        TO ENDERECO1-CG11
                                                    ENDERECO2-CG11
                     MOVE COMPLEMENTO1-CG11A     TO COMPLEMENTO1-CG11
                                                    COMPLEMENTO2-CG11
                     MOVE PONTO-REFER1-CG11A     TO PONTO-REFER1-CG11
                                                    PONTO-REFER2-CG11
                     MOVE BAIRRO1-CG11A          TO BAIRRO1-CG11
                                                    BAIRRO2-CG11
                     MOVE CIDADE1-CG11A          TO CIDADE1-CG11
                                                    CIDADE2-CG11
                     MOVE CEP1-CG11A             TO CEP1-CG11
                                                    CEP2-CG11
                     MOVE FONE1-CG11A            TO FONE1-CG11
                                                    FONE2-CG11
                     MOVE CX-POSTAL1-CG11A       TO CX-POSTAL1-CG11
                                                    CX-POSTAL2-CG11

                     MOVE EMPRESA-CG11A          TO EMPRESA-CG11
                     MOVE ENDERECO2-CG11A        TO ENDERECO3-CG11
                     MOVE COMPLEMENTO2-CG11A     TO COMPLEMENTO3-CG11
                     MOVE PONTO-REFER2-CG11A     TO PONTO-REFER3-CG11
                     MOVE BAIRRO2-CG11A          TO BAIRRO3-CG11
                     MOVE CIDADE2-CG11A          TO CIDADE3-CG11
                     MOVE CEP2-CG11A             TO CEP3-CG11
                     MOVE FONE2-CG11A            TO FONE3-CG11
                     MOVE RAMAL2-CG11A           TO RAMAL3-CG11
                     MOVE CX-POSTAL2-CG11A       TO CX-POSTAL3-CG11
                     MOVE E-MAIL-CG11A           TO E-MAIL-CG11
                     MOVE CELULAR-CG11A          TO CELULAR-CG11
                     MOVE FAX-CG11A              TO FAX-CG11
                     MOVE CPF-CG11A              TO CPF-CG11
                     MOVE RG-CG11A               TO RG-CG11
                     MOVE DT-EXPEDICAO-CG11A     TO DT-EXPEDICAO-CG11
                     MOVE ORGAO-EXPEDICAO-CG11A  TO ORGAO-EXPEDICAO-CG11
                     MOVE DATA-NASC-CG11A        TO DATA-NASC-CG11
                     MOVE NOME-PAI-CG11A         TO NOME-PAI-CG11
                     MOVE NOME-MAE-CG11A         TO NOME-MAE-CG11
                     MOVE SITUACAO-CLI-CG11A     TO SITUACAO-CLI-CG11
                     MOVE TURMA-CG11A            TO TURMA-CG11
                     MOVE TURNO-CG11A            TO TURNO-CG11
                     MOVE ENDERECO-PAIS-CG11A    TO ENDERECO-PAIS-CG11
                     MOVE BAIRRO-PAIS-CG11A      TO BAIRRO-PAIS-CG11
                     MOVE CIDADE-PAIS-CG11A      TO CIDADE-PAIS-CG11
                     MOVE FONE-PAIS-CG11A        TO FONE-PAIS-CG11
                     MOVE CELULAR-PAIS-CG11A     TO CELULAR-PAIS-CG11
                     MOVE CEP-PAIS-CG11A         TO CEP-PAIS-CG11
                     MOVE ENDERECO-REP-CG11A     TO ENDERECO-REP-CG11
                     MOVE COMPLEMENTO-PAIS-CG11A TO
                                                   COMPLEMENTO-PAIS-CG11
                     MOVE DDDD-CELULAR-CG11A     TO DDDD-CELULAR-CG11
                     MOVE SEXO-CG11A             TO SEXO-CG11
                     MOVE BAIRRO-REP-CG11A       TO BAIRRO-REP-CG11
                     MOVE CIDADE-REP-CG11A       TO CIDADE-REP-CG11
                     MOVE CEP-REP-CG11A          TO CEP-REP-CG11

                     WRITE REG-CGD011 INVALID KEY
                           MOVE "Erro de Gravação...CGD011" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                     END-WRITE

                END-READ
           END-PERFORM.

       fechar-arquivos section.
           close cgd011 cgd011a.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
