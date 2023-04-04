       IDENTIFICATION DIVISION.
       PROGRAM-ID.           UTISGUS.
       AUTHOR.      ALFREDO SAVIOLLI.
       DATE-WRITTEN.  04-12-2003.
 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.        DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CXPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CXPW020.

 
       WORKING-STORAGE SECTION.
       77  TEXTO-FIM                   PIC X(58)
              VALUE "* * * * * * * * * * * * * * * * * * * * * * * * * *
      -       " * * * ".
       77  MODULO-W                    PIC X(08)  VALUE SPACES.
       77  PONTEIRO                    PIC 9(02)  COMP-X VALUE 1.
       77  PONTEIRO-INICIO             PIC 9(02)  COMP-X VALUE 1.
       77  PONTEIRO-FIM                PIC 9(02)  COMP-X VALUE ZERO.
       77  LIMITE-TABELA               PIC 9(02)  COMP-X VALUE 15.
       77  UM                          PIC 9(02)  COMP-X VALUE 1.
       77  FLAG-TABELA                 PIC 9(02)  COMP-X VALUE ZERO.
           88  COMPLETOU-TABELA   VALUE 1.
       77  FLAG-ORDEM                  PIC 9(02)  COMP-X VALUE ZERO.
           88  INCREMENTAR-ORDEM  VALUE 1.
       77  FLAG                        PIC 9(02)  COMP-X VALUE ZERO.
           88  FLAG-LIGADO        VALUE 1.
       77  ST-CAD001                   PIC X(02).
       01  ST-CXD020                   PIC X(02)  VALUE "00".


       01  TABELA-DA-TELA  VALUE SPACES.
           05  CHAVE-TAB
                  OCCURS 15 TIMES.
               10  COD-RED           PIC 9(07).
 
       01  VARIAVEIS.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  CODIGO-W              PIC X(3)   VALUE SPACES.
           05  CODIGO-E              PIC 9.99.99.99.

       LINKAGE SECTION.
       77  FUNCAO                      PIC X(01).
       01  AREAIO.
           05  CHAVE-SG.
               10  CHAVES              PIC X(02).
               10  ORDEM               PIC 9(05)  COMP-3.
           05  TAMANHO-EXSG            PIC 9(02)  COMP.
           05  TEXTO.
               10  TEXTO-LINDET        PIC X(58).

       77  FS-IO                       PIC X(02).
           88  NORMAL  VALUE "00".
           88  FIMARQ  VALUE "10", "46".
       77  MODULO                      PIC X(08).
 
       PROCEDURE DIVISION USING FUNCAO AREAIO FS-IO MODULO.
       RAIZ SECTION.
       RAIZ-01.
           OPEN INPUT CONTROLE.
           READ CONTROLE
           MOVE EMPRESA   TO EMP-REC
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           CLOSE CONTROLE.


           EVALUATE FUNCAO
              WHEN "N"
                 READ CXD020 NEXT WITH IGNORE LOCK
                 IF ST-CXD020 = "00"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE SPACES TO REG-CXD020
                    IF ST-CXD020 = "10" OR "46"
                       MOVE "10" TO FS-IO
                    ELSE
                       MOVE "99" TO FS-IO
                    END-IF
                 END-IF
                 IF NORMAL OF FS-IO
                    MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                    EVALUATE GRAU-CX20
                      WHEN 1 PERFORM GRAU-1
                      WHEN 2 PERFORM GRAU-2
                      WHEN 3 PERFORM GRAU-3
                      WHEN 4 PERFORM GRAU-4
                    END-EVALUATE
                    ADD UM TO PONTEIRO-FIM
                    IF PONTEIRO-FIM > LIMITE-TABELA
                       MOVE UM TO PONTEIRO-FIM
                                  FLAG-TABELA
                    END-IF
                    MOVE CODIGO-COMPL-CX20 TO CHAVE-TAB(PONTEIRO-FIM)
                    IF INCREMENTAR-ORDEM
                       ADD UM TO ORDEM OF AREAIO
                    END-IF
                    MOVE UM TO FLAG-ORDEM
                    MOVE PONTEIRO-FIM TO PONTEIRO
                    IF COMPLETOU-TABELA
                       ADD UM TO PONTEIRO-INICIO
                       IF PONTEIRO-INICIO > LIMITE-TABELA
                          MOVE UM TO PONTEIRO-INICIO
                       END-IF
                    END-IF
                 ELSE
                    IF FS-IO EQUAL "99" 
                       MOVE "46" TO FS-IO
                    END-IF
                    IF FIMARQ OF FS-IO  AND
                       FLAG-LIGADO
                       ADD UM TO PONTEIRO-FIM
                       IF PONTEIRO-FIM > LIMITE-TABELA
                          MOVE UM TO PONTEIRO-FIM
                                     FLAG-TABELA
                       END-IF
                       MOVE TEXTO-FIM TO CHAVE-TAB (PONTEIRO-FIM)
                       MOVE PONTEIRO-FIM TO PONTEIRO
                       IF COMPLETOU-TABELA
                          ADD UM TO PONTEIRO-INICIO
                          IF PONTEIRO-INICIO > LIMITE-TABELA
                             MOVE UM TO PONTEIRO-INICIO
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              WHEN "T"
                 MOVE ZERO TO FLAG
                 MOVE COD-RED OF CHAVE-TAB(PONTEIRO-INICIO) TO
                      CODIGO-COMPL-CX20 OF REG-CXD020
                 START CXD020 KEY LESS CODIGO-COMPL-CX20 OF REG-CXD020
                 READ CXD020 PREVIOUS
                 IF ST-CXD020 = "00"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE SPACES TO REG-CXD020
                    IF ST-CXD020 = "10" or "46"
                       MOVE "10" TO FS-IO
                    ELSE
                       MOVE "99" TO FS-IO
                    END-IF
                 END-IF
                 IF NORMAL OF FS-IO
                    MOVE CODIGO-COMPL-CX20 TO CODIGO-E
                    EVALUATE GRAU-CX20
                      WHEN 1 PERFORM GRAU-1
                      WHEN 2 PERFORM GRAU-2
                      WHEN 3 PERFORM GRAU-3
                      WHEN 4 PERFORM GRAU-4
                    END-EVALUATE
                    SUBTRACT UM FROM PONTEIRO-INICIO
                    IF PONTEIRO-INICIO = ZERO
                       MOVE LIMITE-TABELA TO PONTEIRO-INICIO
                    END-IF
                    MOVE CODIGO-COMPL-CX20 OF REG-CXD020 TO
                         COD-RED  OF CHAVE-TAB (PONTEIRO-INICIO)
                    SUBTRACT UM FROM ORDEM OF AREAIO
                    IF COMPLETOU-TABELA
                       SUBTRACT UM FROM PONTEIRO-FIM
                       IF PONTEIRO-FIM = ZERO
                          MOVE LIMITE-TABELA TO PONTEIRO-FIM
                       END-IF
                    ELSE
                       IF PONTEIRO-FIM = PONTEIRO-INICIO
                          MOVE UM TO FLAG-TABELA
                          SUBTRACT UM FROM PONTEIRO-FIM
                          IF PONTEIRO-FIM = ZERO
                             MOVE LIMITE-TABELA TO PONTEIRO-FIM
                          END-IF
                       END-IF
                    END-IF
                    MOVE PONTEIRO-FIM TO PONTEIRO
                 ELSE
                    IF FS-IO EQUAL "99" 
                       MOVE "46" TO FS-IO
                    END-IF   
                 END-IF
              WHEN "P"
                 MOVE ZERO TO FLAG
                 IF ORDEM OF AREAIO = UM
                    MOVE ZEROS TO CODIGO-COMPL-CX20  OF REG-CXD020
                 ELSE
                    MOVE COD-RED           OF CHAVE-TAB (PONTEIRO) TO
                         CODIGO-COMPL-CX20 OF REG-CXD020
                 END-IF

                 START CXD020 KEY IS NOT LESS CODIGO-COMPL-CX20 OF
                    REG-CXD020
                 IF ST-CXD020 = "00"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE "10" TO FS-IO
                 END-IF
                 MOVE UM TO PONTEIRO PONTEIRO-INICIO
                 MOVE ZERO TO PONTEIRO-FIM FLAG-ORDEM FLAG-TABELA
                 MOVE SPACES TO TABELA-DA-TELA
              WHEN "S"
                 MOVE COD-RED           OF CHAVE-TAB (PONTEIRO) TO
                      CODIGO-COMPL-CX20 OF REG-CXD020
                 START CXD020 KEY GREATER CODIGO-COMPL-CX20 OF
                    REG-CXD020
                 IF ST-CXD020 = "00"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE "10" TO FS-IO
                 END-IF
                 MOVE UM TO FLAG-ORDEM FLAG
              WHEN "I"
              WHEN "U"
              WHEN "O"
                 MOVE "Usuário         Nome                           Ca
      -          "rgo" TO LNK-DESCR-SG
                 MOVE ZERO TO FLAG
                 MOVE MODULO TO MODULO-W
                 OPEN I-O CXD020
                 IF ST-CXD020 = "00" OR "41" OR "05"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE "99" TO FS-IO
                 END-IF
                 MOVE SPACES TO TEXTO
                 MOVE LENGTH OF TEXTO TO TAMANHO-EXSG
                 ADD 01               TO TAMANHO-EXSG
              WHEN "F"
                 MOVE ZERO TO FLAG
                 CLOSE CXD020
                 IF ST-CXD020 = "00"
                    MOVE "00" TO FS-IO
                 ELSE
                    MOVE "99" TO FS-IO
                 END-IF
              WHEN OTHER
                 MOVE ZERO TO FLAG
                 MOVE "99" TO FS-IO
           END-EVALUATE.
       RAIZ-99.
           EXIT-PROGRAM.


       GRAU-1 SECTION.
           MOVE CODIGO-E          TO TEXTO-LINDET(1: 11)
           MOVE DESCRICAO-CX20    TO TEXTO-LINDET(12: 31)
           MOVE CODIGO-REDUZ-CX20 TO TEXTO-LINDET(52: 03)
           MOVE GRAU-CX20         TO TEXTO-LINDET(57:01).
       GRAU-2 SECTION.
           MOVE CODIGO-E          TO TEXTO-LINDET(4: 11)
           MOVE DESCRICAO-CX20    TO TEXTO-LINDET(15: 31)
           MOVE CODIGO-REDUZ-CX20 TO TEXTO-LINDET(52: 03)
           MOVE GRAU-CX20         TO TEXTO-LINDET(57:01).
       GRAU-3 SECTION.
           MOVE CODIGO-E          TO TEXTO-LINDET(7: 11)
           MOVE DESCRICAO-CX20    TO TEXTO-LINDET(18: 31)
           MOVE CODIGO-REDUZ-CX20 TO TEXTO-LINDET(52: 03)
           MOVE GRAU-CX20         TO TEXTO-LINDET(57:01).
       GRAU-4 SECTION.
           MOVE CODIGO-E          TO TEXTO-LINDET(10: 11)
           MOVE DESCRICAO-CX20    TO TEXTO-LINDET(21: 31)
           MOVE CODIGO-REDUZ-CX20 TO TEXTO-LINDET(52: 03)
           MOVE GRAU-CX20         TO TEXTO-LINDET(57:01).
