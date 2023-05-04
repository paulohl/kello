       IDENTIFICATION DIVISION.
       PROGRAM-ID. galho58.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 15-08-2005
      *exclui anotações do contrato
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY COPX003.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY COPW003.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *   tipo de ordem de impressão
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *  IMPRIME-W = 0 (o registro nao faz parte da opcao) e = 1 Sim
           05  ULT-CODIGO            PIC 9(6)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CADASTRO utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
      *    Grava = 0 (regravar)   grava = 1 (gravar)
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE 999                TO EMP-REC
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003
           MOVE ZEROS TO ERRO-W.
           OPEN I-O COD003
           CLOSE CONTROLE.
           IF ST-COD003 = "35"
              CLOSE COD003      OPEN OUTPUT COD003
              CLOSE COD003      OPEN I-O COD003
           END-IF.

           INITIALIZE REG-COD003

           START COD003 KEY IS NOT LESS CODIGO-CO03 INVALID KEY
               MOVE "10" TO ST-COD003.
           PERFORM UNTIL ST-COD003 = "10"
               READ COD003 NEXT AT END
                   MOVE "10" TO ST-COD003
               NOT AT END
                   MOVE 1 TO RELAT-ESTAT-CO03
                   REWRITE REG-COD003
                   END-REWRITE
               END-READ
           END-PERFORM

           CLOSE COD003
           EXIT PROGRAM
           STOP RUN.


       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

