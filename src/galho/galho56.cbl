       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO56.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 10-08-2005
      *exclui gera um novo controle.dat
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       WORKING-STORAGE SECTION.
           COPY "GALHO10.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD002             PIC XX       VALUE SPACES.
           05  ST-CGD003             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD014             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED006             PIC XX       VALUE SPACES.
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


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           OPEN OUTPUT CONTROLE.
       CODIGO.
           DISPLAY "INFORMAR O CODIGO 3 DIGITOS: " AT 0101
           ACCEPT EMPRESA                          AT 0130
           IF EMPRESA = SPACES
              GO TO CODIGO.
      *    MOVE "888"             TO EMPRESA
           MOVE "EMPRESA TESTE"   TO NOME-EMP
           MOVE "TESTETESTETESTE" TO  NOME-EMP-RED
           WRITE REG-CONTROLE

           CLOSE CONTROLE.
           EXIT PROGRAM.
           STOP RUN.
