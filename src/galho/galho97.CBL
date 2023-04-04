       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO97.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 06/06/2009.
      *FUNÇÃO: ALTERACAO DO LOG003

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CXPX100.

           COPY CPPX020.

           COPY CGPX001.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CXPW100.

           COPY CPPW020.

           COPY CGPW001.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC x.
           05  RESP-MSG              PIC x.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(8).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(13).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  AUX-CONT              PIC 9(04)    VALUE ZEROS.
           05  AUX-CONT2             PIC 9(04)    VALUE ZEROS.

           COPY "PARAMETR".

       01  LINDET.
           05  CXP100-LINDET         PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ       CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CXD100"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CXD100
           MOVE "CPD020"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CPD020
           MOVE "CGD001"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CGD001

           OPEN I-O   CXD100 CPD020 CGD001

           DISPLAY "VOU COMECAR " STOP " "

           INITIALIZE REG-CXD100 AUX-CONT AUX-CONT2
           MOVE 20090710           TO DATA-MOV-CX100
           START CXD100 KEY IS NOT LESS CHAVE-CX100 INVALID KEY
                MOVE "10" TO ST-CXD100.

           PERFORM UNTIL ST-CXD100 = "10"
                READ CXD100 NEXT AT END
                     MOVE "10" TO ST-CXD100
                NOT AT END
                     IF DATA-MOV-CX100 <> 20090710
                        MOVE "10" TO ST-CXD100
                     ELSE
                        IF TIPO-LCTO-CX100 = 31
                           ADD 1 TO AUX-CONT
                           DISPLAY "PRECISO ALTERAR " AUX-CONT
                           MOVE SPACES          TO CXP100-LINDET
                           MOVE SEQ-CX100       TO CXP100-LINDET(01:06)
      *                                            ULT-SEQUENCIA
                           MOVE TIPO-LCTO-CX100 TO CXP100-LINDET(07:02)
                           MOVE HISTORICO-CX100 TO CXP100-LINDET(11:30)
                           MOVE DOCUMENTO-CX100 TO CXP100-LINDET(42:10)
                           MOVE VALOR-CX100     TO VALOR-E
                           MOVE VALOR-E         TO CXP100-LINDET(54:13)
                           IF TIPO-LCTO-CX100 < 50
                              MOVE "D"          TO CXP100-LINDET(68: 1)
                           ELSE
                              MOVE "C"          TO CXP100-LINDET(68: 1)
                           END-IF
                           MOVE CONTAPART-CX100 TO CXP100-LINDET(71:06)
                                                   CODIGO-CG01
                           READ CGD001 INVALID KEY
                                MOVE SPACES TO NOME-CG01
                           END-READ

                           MOVE NOME-CG01         TO
                                CXP100-LINDET(78: 21)
                           MOVE CONTA-REDUZ-CX100 TO
                                CXP100-LINDET(101:05)
                           MOVE SEQ-CX100           TO SEQ-CAIXA-CP20
                           MOVE DATA-MOV-CX100      TO DATA-PGTO-CP20
                           START CPD020 KEY IS NOT LESS ALT6-CP20
                                                             INVALID KEY
                                MOVE "10" TO ST-CPD020
                           END-START
                           PERFORM UNTIL ST-CPD020 = "10"
                                READ CPD020 NEXT AT END
                                     MOVE "10" TO ST-CPD020
                                NOT AT END
                                     IF SEQ-CAIXA-CP20 <> SEQ-CX100 OR
                                        DATA-PGTO-CP20 <> DATA-MOV-CX100
                                        MOVE "10" TO ST-CPD020
                                     ELSE
                                        IF DESCRICAO-CP20 =
                                           HISTORICO-CX100 AND
                                           NR-DOCTO-CP20 =
                                           DOCUMENTO-CX100
                                           DISPLAY
                                              "CONTA-REDUZ-CX100 = "
                                               CONTA-REDUZ-CX100
                                              " CODREDUZ-APUR-CP20 = "
                                             CODREDUZ-APUR-CP20
                                           MOVE CODREDUZ-APUR-CP20
                                             TO CONTA-REDUZ-CX100
                                           ADD 1 TO AUX-CONT2
                                           DISPLAY "VOU ALTERAR "
                                                   AUX-CONT2
                                           REWRITE REG-CXD100 INVALID
                                           KEY
                                               MOVE "Erro de Regravação.
      -                                             "..CXD100" TO
                                                    MENSAGEM
                                               MOVE "C" TO TIPO-MSG
                                               PERFORM
                                                     140-EXIBIR-MENSAGEM
                                           END-REWRITE
                                           MOVE "10" TO ST-CPD020
                                        END-IF
                                     END-IF
                                END-READ
                           END-PERFORM
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CPD020 CXD100 CGD001
           EXIT PROGRAM
           STOP RUN.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

