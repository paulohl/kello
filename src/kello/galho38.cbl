       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO38.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 11-05-2005.
      *FUNÇÃO: GERA UM NOVO ARQUIVO MTD019 MTD020 CGD011 RCD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY MTPX019.

           SELECT MTD919 ASSIGN TO PATH-MTD919
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD919
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MT99
                  ALTERNATE RECORD KEY IS ALT-MT99 =
                            DATAMOV-MT99, ALBUM-MT99
                  ALTERNATE RECORD KEY IS CIDADE-MT99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-MT99 = CONTRATO-MT99
                            CURSO-MT99 NOME-FORM-MT99 WITH DUPLICATES.

           COPY MTPX020.

           SELECT MTD920 ASSIGN TO PATH-MTD920
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-MTD920
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS ALBUM-MTG92
                  ALTERNATE RECORD KEY IS CHAVE-MTG92 =
                            DATAMOV-MTG92, ALBUM-MTG92
                  ALTERNATE RECORD KEY IS ANOMES-VISITA-MTG92
                            WITH DUPLICATES.

           COPY CGPX011.

           SELECT CGD911 ASSIGN TO PATH-CGD911
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG91
                  STATUS IS ST-CGD911.

           COPY RCPX100.

           SELECT RCD900 ASSIGN TO PATH-RCD900
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  STATUS IS ST-RCD900
                  RECORD KEY IS ALBUM-REC99
                  ALTERNATE RECORD KEY IS ALT-REC99 = DATA-MOVTO-REC99
                                          ALBUM-REC99
                  ALTERNATE RECORD KEY IS DATAVEN-REC99 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY MTPW019.

       FD  MTD919.
       01  REG-MTD919.
           05  ALBUMMT99.
               10  CONTRATO-MT99   PIC 9(4).
               10  SEQ-MT99        PIC 9(4).
           05  ALBUM-MT99 REDEFINES ALBUMMT99 PIC 9(8).
           05  DATAMOV-MT99        PIC 9(8).
           05  NOME-FORM-MT99      PIC X(30).
           05  CIDADE-MT99         PIC 9(04).
           05  CURSO-MT99          PIC 9(03).
           05  UF-MT99             PIC XX.
           05  FONE-MT99           PIC 9(8).
           05  IDENTIFICADO-MT99   PIC 9.
      * identificado = 0(nao)  1(sim)

           COPY MTPW020.

       FD  MTD920.
       01  REG-MTD920.
           05  ALBUM-MTG92.
               10  CONTRATO-MTG92    PIC 9(4).
               10  SEQ-MTG92         PIC 9(4).
           05  DATAMOV-MTG92         PIC 9(8).
           05  QT-ESTOJO-MTG92       PIC 9.
           05  QT-ENCADER-MTG92      PIC 9.
      *    CAPA OU ENCADERNACAO
           05  QT-FOLHAS-MTG92       PIC 9999.
           05  QT-FOTOS-MTG92        PIC 9999.
           05  QT-FITAS-MTG92        PIC 9.
           05  QT-POSTER-MTG92       PIC 9.
           05  QT-PORTA-FITA-MTG92   PIC 9.
           05  FOGO-MTG92            PIC 9. *> 0-Montagem   1-vendido
                                          *> 8-Vend-Fogo  9-Fogo
           05  DATA-FOGO-MTG92       PIC 9(8).  *> DATA-INVERTIDA
           05  ANOMES-VISITA-MTG92   PIC 9(6).
           05  VISITA-MTG92          PIC 999.
           05  POSSE-MTG92           PIC 9.
      *    1-EM ESTOQUE    2-COM VENDEDOR  3-montagem
           05  CODIGO-POSSE-MTG92    PIC 9(6).
           05  QT-DVD-MTG92          PIC 9(1).
           05  NAO-GEROU-ALBUM-MTG92 PIC 9(1).
           05  FILLER                PIC X(40).

           COPY CGPW011.

       FD  CGD911.
       01  REG-CGD911.
           05  COD-COMPL-CG91.
               10  CLASSIF-CG91    PIC 9.
      *     CLASSIF = 0-CONTRATO  1-COMUM
               10  CODIGO-CG91     PIC 9(8).
           05  ENDERECO1-CG91      PIC X(30).
           05  BAIRRO1-CG91        PIC X(15).
           05  CIDADE1-CG91        PIC 9(4).
           05  CEP1-CG91           PIC 9(8).
           05  FONE1-CG91          PIC 9(8).
           05  CX-POSTAL1-CG91     PIC 9(5).
           05  EMPRESA-CG91        PIC X(30).
           05  ENDERECO2-CG91      PIC X(30).
           05  BAIRRO2-CG91        PIC X(15).
           05  CIDADE2-CG91        PIC 9(4).
           05  CEP2-CG91           PIC 9(8).
           05  FONE2-CG91          PIC 9(8).
           05  RAMAL2-CG91         PIC 9(3).
           05  CX-POSTAL2-CG91     PIC 9(5).
           05  E-MAIL-CG91         PIC X(30).
           05  CELULAR-CG91        PIC 9(8).
           05  FAX-CG91            PIC 9(8).
           05  CPF-CG91            PIC 9(16).
           05  RG-CG91             PIC X(15).
           05  DATA-NASC-CG91      PIC 9(8).
      *    DATA-NASC-CG91 = AAAAMMDD
           05  NOME-PAI-CG91       PIC X(30).
           05  NOME-MAE-CG91       PIC X(30).
           05  SITUACAO-CLI-CG91   PIC 9.
      *    SITUAÇÃO 0(OK)   1-PROTESTADO

           COPY RCPW100.

       FD  RCD900.
       01  REG-RCD900.
           05  DATA-MOVTO-REC99       PIC 9(08).
      *    DATA-MOVTO-REC - INVERTIDA
           05  ALBUM-REC99            PIC 9(08).
           05  DATAVEN-REC99          PIC 9(08)     COMP-3.
      *    DATAVEN-REC - INVERTIDA
           05  VISITA-REC99           PIC 9.
           05  QENCADER-REC99         PIC 9.
      *    CAPA OU ENCADERNACAO  -
           05  QESTOJO-REC99          PIC 9.
      *    ESTOJO(MALETA DO ALBUM)
           05  QFOTOS-REC99           PIC 999.
           05  QFOLHAS-REC99          PIC 999.
           05  QFITAS-REC99           PIC 9.
           05  QPFITA-REC99           PIC 9.
      *    PORTA-FITA
           05  QCOBERTURA-REC99       PIC 9(2).
           05  QABERTURA-REC99        PIC 9.
           05  QPOSTER-REC99          PIC 9(2).
           05  VENDEDOR-REC99         PIC 9(06).
           05  PM-REC99               PIC 999V99.
           05  TOTAL-REC99            PIC 9(08)V99   COMP-3.
           05  TOTAL-DEF-REC99        PIC 9(08)V99   COMP-3.
           05  QT-COBERTURA-DVD-REC99 PIC 9(05)      COMP-3.
           05  QDVD-REC99             PIC 999.
           05  QAVULSAS-REC99         PIC 999.
           05  QCOMISSAO-REC99        PIC 999.
           05  TAXA-REC99             PIC 99V99.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD919             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD920             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD911             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD900             PIC XX       VALUE SPACES.
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

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019
           MOVE "MTD919" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD919
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020
           MOVE "MTD920" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD920
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011
           MOVE "CGD911" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD911
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "RCD900" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD900

           OPEN I-O   MTD019 MTD020 CGD011 RCD100
           CLOSE      MTD019 MTD020 CGD011 RCD100
           OPEN I-O   MTD019 MTD020 CGD011 RCD100

           OPEN INPUT MTD919 MTD920 CGD911 RCD900

           CLOSE CONTROLE.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-RCD100 = "35"
              CLOSE RCD100      OPEN OUTPUT RCD100
              CLOSE RCD100      OPEN I-O RCD100
           END-IF.

           display "Vou comecar a atualizar o MTD019" stop " ".
           display "Vou comecar a atualizar o MTD019" stop " ".

           IF ST-MTD019 <> "00" OR ST-MTD020 <> "00" OR ST-CGD011 <>
           "00" OR ST-RCD100 <> "00"
              CLOSE MTD019 MTD020 CGD011 RCD100
                    MTD919 MTD920 CGD911 RCD900
              stop run.

           INITIALIZE REG-MTD919

           START MTD919 KEY IS NOT LESS ALBUM-MT99 INVALID KEY
                 MOVE "10" TO ST-MTD919.


           PERFORM UNTIL ST-MTD919 = "10"
             READ MTD919 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD919
             NOT AT END
                  INITIALIZE REG-MTD019
                  MOVE CONTRATO-MT99     TO CONTRATO-MT19
                  MOVE SEQ-MT99          TO SEQ-MT19
                  MOVE DATAMOV-MT99      TO DATAMOV-MT19
                  MOVE NOME-FORM-MT99    TO NOME-FORM-MT19
                  MOVE CIDADE-MT99       TO CIDADE-MT19
                  MOVE CURSO-MT99        TO CURSO-MT19
                  MOVE UF-MT99           TO UF-MT19
                  MOVE FONE-MT99         TO FONE-MT19
                  MOVE IDENTIFICADO-MT99 TO IDENTIFICADO-MT19
                  MOVE SPACES            TO TURMA-MT19
                  MOVE SPACES            TO TURNO-MT19

                  DISPLAY REG-MTD019
                  WRITE REG-MTD019
              END-READ
           END-PERFORM

           CLOSE MTD919 MTD019

           display "Acabei de Atualizar o MTD019" stop " ".

           display "Vou comecar a atualizar o MTD020" stop " ".

           INITIALIZE REG-MTD920

           START MTD920 KEY IS NOT LESS ALBUM-MTG92 INVALID KEY
                 MOVE "10" TO ST-MTD920.


           PERFORM UNTIL ST-MTD920 = "10"
             READ MTD920 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD920
             NOT AT END
                  INITIALIZE REG-MTD020
                  MOVE CONTRATO-MTG92        TO CONTRATO-MTG
                  MOVE SEQ-MTG92             TO SEQ-MTG
                  MOVE DATAMOV-MTG92         TO DATAMOV-MTG
                  MOVE QT-ESTOJO-MTG92       TO QT-ESTOJO-MTG
                  MOVE QT-ENCADER-MTG92      TO QT-ENCADER-MTG
                  MOVE QT-FOLHAS-MTG92       TO QT-FOLHAS-MTG
                  MOVE QT-FOTOS-MTG92        TO QT-FOTOS-MTG
                  MOVE QT-FITAS-MTG92        TO QT-FITAS-MTG
                  MOVE QT-POSTER-MTG92       TO QT-POSTER-MTG
                  MOVE QT-PORTA-FITA-MTG92   TO QT-PORTA-FITA-MTG
                  MOVE 0                     TO QT-FOTO-CD-MTG
                  MOVE 0                     TO QT-MOLDURA-MTG
                  MOVE 0                     TO QT-PORTA-DVD-MTG
                  MOVE FOGO-MTG92            TO FOGO-MTG
                  MOVE DATA-FOGO-MTG92       TO DATA-FOGO-MTG
                  MOVE ANOMES-VISITA-MTG92   TO ANOMES-VISITA-MTG
                  MOVE VISITA-MTG92          TO VISITA-MTG
                  MOVE POSSE-MTG92           TO POSSE-MTG
                  MOVE CODIGO-POSSE-MTG92    TO CODIGO-POSSE-MTG
                  MOVE QT-DVD-MTG92          TO QT-DVD-MTG
                  MOVE NAO-GEROU-ALBUM-MTG92 TO NAO-GEROU-ALBUM-MTG

                  DISPLAY REG-MTD020
                  WRITE REG-MTD020
              END-READ
           END-PERFORM

           CLOSE MTD920 MTD020

           display "Acabei de Atualizar o MTD020" stop " ".

           display "Vou comecar a atualizar o CGD011" stop " ".

           INITIALIZE REG-CGD911

           START CGD911 KEY IS NOT LESS COD-COMPL-CG91 INVALID KEY
                 MOVE "10" TO ST-CGD911.


           PERFORM UNTIL ST-CGD911 = "10"
             READ CGD911 NEXT RECORD AT END
                  MOVE "10" TO ST-CGD911
             NOT AT END
                  INITIALIZE REG-CGD011
                  MOVE CLASSIF-CG91         TO CLASSIF-CG11
                  MOVE CODIGO-CG91          TO CODIGO-CG11
                  MOVE ENDERECO1-CG91       TO ENDERECO1-CG11
                  MOVE SPACES               TO COMPLEMENTO1-CG11
                  MOVE SPACES               TO PONTO-REFER1-CG11
                  MOVE BAIRRO1-CG91         TO BAIRRO1-CG11
                  MOVE CIDADE1-CG91         TO CIDADE1-CG11
                  MOVE CEP1-CG91            TO CEP1-CG11
                  MOVE FONE1-CG91           TO FONE1-CG11
                  MOVE CX-POSTAL1-CG91      TO CX-POSTAL1-CG11
                  MOVE EMPRESA-CG91         TO EMPRESA-CG11
                  MOVE ENDERECO2-CG91       TO ENDERECO2-CG11
                  MOVE SPACES               TO COMPLEMENTO2-CG11
                  MOVE SPACES               TO PONTO-REFER2-CG11
                  MOVE BAIRRO2-CG91         TO BAIRRO2-CG11
                  MOVE CIDADE2-CG91         TO CIDADE2-CG11
                  MOVE CEP2-CG91            TO CEP2-CG11
                  MOVE FONE2-CG91           TO FONE2-CG11
                  MOVE RAMAL2-CG91          TO RAMAL2-CG11
                  MOVE CX-POSTAL2-CG91      TO CX-POSTAL2-CG11
                  MOVE E-MAIL-CG91          TO E-MAIL-CG11
                  MOVE CELULAR-CG91         TO CELULAR-CG11
                  MOVE FAX-CG91             TO FAX-CG11
                  MOVE CPF-CG91             TO CPF-CG11
                  MOVE RG-CG91              TO RG-CG11
                  MOVE ZEROS                TO DT-EXPEDICAO-CG11
                  MOVE SPACES               TO ORGAO-EXPEDICAO-CG11
                  MOVE DATA-NASC-CG91       TO DATA-NASC-CG11
                  MOVE NOME-PAI-CG91        TO NOME-PAI-CG11
                  MOVE NOME-MAE-CG91        TO NOME-MAE-CG11
                  MOVE SITUACAO-CLI-CG91    TO SITUACAO-CLI-CG11
                  MOVE SPACES               TO TURMA-CG11
                  MOVE SPACES               TO TURNO-CG11
                  DISPLAY REG-CGD011
                  WRITE REG-CGD011
              END-READ
           END-PERFORM

           CLOSE CGD911 CGD011

           display "Acabei de Atualizar o CGD011" stop " ".

           display "Vou comecar a atualizar o RCD100" stop " ".

           INITIALIZE REG-RCD900

           START RCD900 KEY IS NOT LESS ALBUM-REC99 INVALID KEY
                 MOVE "10" TO ST-RCD900.


           PERFORM UNTIL ST-RCD900 = "10"
             READ RCD900 NEXT RECORD AT END
                  MOVE "10" TO ST-RCD900
             NOT AT END
                  INITIALIZE REG-RCD100
                  MOVE DATA-MOVTO-REC99       TO DATA-MOVTO-REC
                  MOVE ALBUM-REC99            TO ALBUM-REC
                  MOVE DATAVEN-REC99          TO DATAVEN-REC
                  MOVE VISITA-REC99           TO VISITA-REC
                  MOVE QENCADER-REC99         TO QENCADER-REC
                  MOVE QESTOJO-REC99          TO QESTOJO-REC
                  MOVE QFOTOS-REC99           TO QFOTOS-REC
                  MOVE QFOLHAS-REC99          TO QFOLHAS-REC
                  MOVE QFITAS-REC99           TO QFITAS-REC
                  MOVE QPFITA-REC99           TO QPFITA-REC
                  MOVE QCOBERTURA-REC99       TO QCOBERTURA-REC
                  MOVE QABERTURA-REC99        TO QABERTURA-REC
                  MOVE QPOSTER-REC99          TO QPOSTER-REC
                  MOVE VENDEDOR-REC99         TO VENDEDOR-REC
                  MOVE PM-REC99               TO PM-REC
                  MOVE TOTAL-REC99            TO TOTAL-REC
                  MOVE TOTAL-DEF-REC99        TO TOTAL-DEF-REC
                  MOVE QT-COBERTURA-DVD-REC99 TO QT-COBERTURA-DVD-REC
                  MOVE QDVD-REC99             TO QDVD-REC
                  MOVE 0                      TO QFOTO-CD-REC
                  MOVE 0                      TO QMOLDURA-REC
                  MOVE 0                      TO QPORTA-DVD-REC
                  MOVE QAVULSAS-REC99         TO QAVULSAS-REC
                  MOVE QCOMISSAO-REC99        TO QCOMISSAO-REC
                  MOVE TAXA-REC99             TO TAXA-REC

                  DISPLAY REG-RCD100
                  WRITE REG-RCD100
              END-READ
           END-PERFORM

           CLOSE RCD900 RCD100.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           EXIT PROGRAM
           STOP RUN.
