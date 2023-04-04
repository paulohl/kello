         IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD020B.
      *AUTORA: ALFREDO SAVIOLLI
      *DATA: 22/10/2009
      *Atualiza o CRD020B (Arquivo das Baixas do Contas a Receber)
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CXPX100I.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CXPW100I.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CXD100I            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9(01)    VALUE ZEROS.
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
           05  CONTEUDO1             PIC X(200) VALUE SPACES.
           05  CONTEUDO2             PIC X(200) VALUE SPACES.
           05  AUX-CXD100I           PIC X(267) VALUE SPACES.

      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".


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
                      perform alterar-caminho
                      perform fechar-arquivos
                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           MOVE "Acabei!" TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM

           EXIT PROGRAM
           STOP RUN.


       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CXD100I"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CXD100I

           OPEN I-O CXD100I.
       abrir-arquivos-fim.
           exit.

       alterar-caminho section.
           INITIALIZE REG-CXD100I
           START CXD100I KEY IS NOT LESS CHAVE-CX100I INVALID KEY
                 MOVE "10" TO ST-CXD100I
           END-START
           PERFORM UNTIL ST-CXD100I = "10"
                 READ CXD100I NEXT AT END
                      MOVE "10" TO ST-CXD100I
                 NOT AT END
                      DISPLAY "REG-CXD100I = " REG-CXD100I

                      INITIALIZE CONTEUDO1
                                 CONTEUDO2
                      UNSTRING IMAGEM-CX100I DELIMITED BY
                                             "\\100.10.10.2"
                          INTO CONTEUDO1
                               CONTEUDO2

                      IF CONTEUDO2 <> SPACES
                         MOVE REG-CXD100I TO AUX-CXD100I
                         DELETE CXD100I
                         INITIALIZE IMAGEM-CX100I
                         STRING "\\192.168.10.2" CONTEUDO2 INTO
                                 IMAGEM-CX100I
                         WRITE REG-CXD100I
                         INITIALIZE REG-CXD100I
                         MOVE AUX-CXD100I TO REG-CXD100I
                         START CXD100I KEY IS NOT LESS CHAVE-CX100I
                                                             INVALID KEY
                               MOVE "10" TO ST-CXD100I
                         END-START
                      END-IF

                 END-READ
           END-PERFORM.
       alterar-caminho-fim.
           exit.

       fechar-arquivos section.
           CLOSE CXD100I.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1      to erro-w
           move spaces to mensagem.
