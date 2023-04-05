       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCGD011REFAZ.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-05-2010
      *DESCRIÇÃO: Conversão MTD001

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
                  ALT2-CG11A = CPF1-CG11A
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
      *>Dados Cobrança
           05  ENDERECO1-CG11A        PIC X(45).
           05  BAIRRO1-CG11A          PIC X(25).
           05  CIDADE1-CG11A          PIC 9(4).
           05  CEP1-CG11A             PIC 9(8).
           05  FONE1-CG11A            PIC 9(8).
           05  CX-POSTAL1-CG11A       PIC 9(5).
           05  COMPLEMENTO1-CG11A     PIC X(30).
           05  E-MAIL1-CG11A          PIC X(30).
           05  PONTO-REFER1-CG11A     PIC X(40).
           05  CPF1-CG11A             PIC 9(16).
           05  RG1-CG11A              PIC X(15).
           05  DT-EXPEDICAO1-CG11A    PIC 9(8).
           05  ORGAO-EXPEDICAO1-CG11A PIC X(8).
           05  FAX1-CG11A             PIC 9(8).
           05  DDD-CELULAR1-CG11A     PIC 9(02).
           05  CELULAR1-CG11A         PIC 9(8).
           05  SEXO1-CG11A            PIC X(01).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  DATA-NASC1-CG11A       PIC 9(8).
           05  SITUACAO-CLI-CG11A     PIC 9.
      *>Dados Formnando
           05  ENDERECO2-CG11A        PIC X(45).
           05  BAIRRO2-CG11A          PIC X(25).
           05  CIDADE2-CG11A          PIC 9(4).
           05  CEP2-CG11A             PIC 9(8).
           05  FONE2-CG11A            PIC 9(8).
           05  RAMAL2-CG11A           PIC 9(3).
           05  COMPLEMENTO2-CG11A     PIC X(30).
           05  CX-POSTAL2-CG11A       PIC 9(5).
           05  PONTO-REFER2-CG11A     PIC X(40).
           05  E-MAIL2-CG11A          PIC X(30).
           05  CPF2-CG11A             PIC 9(16).
           05  RG2-CG11A              PIC X(15).
           05  DT-EXPEDICAO2-CG11A    PIC 9(8).
           05  ORGAO-EXPEDICAO2-CG11A PIC X(8).
           05  DDD-CELULAR2-CG11A     PIC 9(02).
           05  CELULAR2-CG11A         PIC 9(8).
           05  FAX2-CG11A             PIC 9(8).
      *    DATA-NASC-CG11 = AAAAMMDD
           05  DATA-NASC2-CG11A       PIC 9(8).
           05  SEXO2-CG11A            PIC X(01).
      *>Dados dos pai
           05  NOME-PAI-CG11A         PIC X(30).
           05  NOME-MAE-CG11A         PIC X(30).
           05  ENDERECO-PAIS-CG11A    PIC X(45).
           05  COMPLEMENTO-PAIS-CG11A PIC X(30).
           05  BAIRRO-PAIS-CG11A      PIC X(25).
           05  FONE-PAIS-CG11A        PIC 9(8).
           05  CELULAR-PAIS-CG11A     PIC 9(8).
           05  CIDADE-PAIS-CG11A      PIC 9(4).
           05  CEP-PAIS-CG11A         PIC 9(8).
      *Dados da República
           05  ENDERECO-REP-CG11A     PIC X(45).
           05  BAIRRO-REP-CG11A       PIC X(25).
           05  CIDADE-REP-CG11A       PIC 9(4).
           05  CEP-REP-CG11A          PIC 9(8).
      *>Dados Empresa
           05  EMPRESA-CG11A         PIC X(30).
           05  ENDERECO3-CG11A       PIC X(45).
           05  COMPLEMENTO3-CG11A    PIC X(30).
           05  PONTO-REFER3-CG11A    PIC X(40).
           05  BAIRRO3-CG11A         PIC X(25).
           05  CIDADE3-CG11A         PIC 9(4).
           05  CEP3-CG11A            PIC 9(8).
           05  FONE3-CG11A           PIC 9(8).
           05  RAMAL3-CG11A          PIC 9(3).
           05  CX-POSTAL3-CG11A      PIC 9(5).
           05  TURMA-CG11A           PIC X(03).
           05  TURNO-CG11A           PIC X(10).
      *>Complemento de Telefones
           05  COMP-TEL1-CG11A       PIC 9(01).
           05  COMP-FAX1-CG11A       PIC 9(01).
           05  COMP-CEL1-CG11A       PIC 9(01).
           05  COMP-TEL2-CG11A       PIC 9(01).
           05  COMP-CEL2-CG11A       PIC 9(01).
           05  COMP-FAX2-CG11A       PIC 9(01).
           05  COMP-TEL-PAIS-CG11A   PIC 9(01).
           05  COMP-CEL-PAIS-CG11A   PIC 9(01).
           05  COMP-TEL3-CG11A       PIC 9(01).
           05  TIPO-PESSOA-CG11A     PIC X(01).
           05  FILLER                PIC X(50).
      *    SITUAÇÃO 0(OK)   1-PROTESTADO *** TIPO-PESSOA-CG11 =
      *                                      " " -> FISICA
      *                                      "F" -> FISICA
      *                                      "J" -> JURIDICA

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
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
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
                      DISPLAY "CODIGO-CA001 = " CODIGO-CA001
                      perform renomear-arquivos
                      perform abrir-arquivos
                      perform converter-arquivo
                      perform fechar-arquivos
                      display "ACABEI ESSA EMPRESA"
                 end-read
           end-perform

           close cad001

           move "acabei" to mensagem
           move "C" to tipo-msg
           perform exibir-mensagem

           STOP RUN.

       renomear-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "GCD011"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011A

           OPEN I-O CGD011A
           CLOSE    CGD011A

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CGD011B2"             TO ARQ-REC
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
           MOVE "CGD011B2"             TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CGD011A

           open i-o   cgd011
           close      cgd011
           open i-o   cgd011

           open i-o cgd011a.
       abrir-arquivos-fim.
           exit.

       converter-arquivo section.
           initialize reg-cgd011a
           start cgd011a key is not less cod-compl-cg11a invalid key
                 move "10" to st-cgd011a.

           perform until st-cgd011a  = "10"
                 read cgd011a next at end
                      move "10" to st-cgd011a
                 not at end
                      move reg-cgd011a to reg-cgd011

                      display "reg-cgd011 = " reg-cgd011
                      write reg-cgd011
      *                    invalid key
      *                    move "Erro de Gravação...CGD011" to mensagem
      *                    move "C" to tipo-msg
      *                    perform exibir-mensagem
                      end-write
                 end-read
           end-perform.
       converter-arquivo-fim.
           exit.

       fechar-arquivos section.
           close cgd011 cgd011a.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.
