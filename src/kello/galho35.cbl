       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO35.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 02/02/2005
      *DESCRIÇÃO: PROGRAMA Q ARRUMA OS CONTRATOS 745 1397 1494

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CGPX010.
           SELECT CGD999 ASSIGN TO PATH-CGD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG99
                  STATUS IS ST-CGD999
                  ALTERNATE RECORD KEY IS COMPRADOR-CG99
                            WITH DUPLICATES.

           COPY CGPX011.

           SELECT CGD991 ASSIGN TO PATH-CGD991
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS COD-COMPL-CG91
                  STATUS IS ST-CGD991.

           COPY CGPX012.

           SELECT CGD992 ASSIGN TO PATH-CGD992
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG92
                  STATUS IS ST-CGD992.

           COPY CRPX020.

          SELECT CRD999 ASSIGN TO PATH-CRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR99 = COD-COMPL-CR99 SEQ-CR99
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR99 = COD-COMPL-CR99
                        DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR99 = SITUACAO-CR99
                        DATA-VENCTO-CR99 COD-COMPL-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR99 = SITUACAO-CR99
                        DATA-MOVTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR99 = SITUACAO-CR99
                        CLIENTE-CR99 DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR99 = DATA-RCTO-CR99
                       SEQ-CAIXA-CR99 WITH DUPLICATES.

           COPY CRPX200.

          SELECT CRD299 ASSIGN TO PATH-CRD299
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD299
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR299 = COD-COMPL-CR299 SEQ-CR299
                  ALTERNATE RECORD KEY IS ALT1-CR299 = COD-COMPL-CR299
                         DATA-MOVTO-CR299 HORA-MOVTO-CR299
                         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR299 =
                      DATA-RETORNO-CR299 USUARIO-CR299 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR299 = DATA-MOVTO-CR299
                      HORA-MOVTO-CR299 USUARIO-CR299 WITH DUPLICATES.

           COPY CRPX201.

          SELECT CRD291 ASSIGN TO PATH-CRD291
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD291
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR291 = COD-COMPL-CR291 SEQ-CR291
                                              SUBSEQ-CR291.

           COPY CHPX010.

           SELECT CHD999 ASSIGN TO PATH-CHD999
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH99 = DATA-MOVTO-CH99 SEQ-CH99
                  ALTERNATE RECORD KEY ALT-CH91 =
                     NOME-CH99, PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH92 = SITUACAO-CH99
                     DATA-VENCTO-CH99 PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH93 = DATA-MOVTO-CH99
                     VENDEDOR-CH99 SEQ-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH94 = COD-COMPL-CH99
                     DATA-VENCTO-CH99 WITH DUPLICATES
                  STATUS IS ST-CHD999.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CGPW010.

       FD  CGD999.
       01  REG-CGD999.
           05  COD-COMPL-CG99.
               10  CLASSIF-CG99        PIC 9.
      *    TIPO 0-CONTRATO   1-COMUM
               10  CODIGO-CG99         PIC 9(8).
      *    CODIGO, quando se tratar de tipo contrato o código a ser
      *    considerado será do tipo contrato/album(9999-9999)
           05  COMPRADOR-CG99      PIC X(30).

           COPY CGPW011.

       FD  CGD991.
       01  REG-CGD991.
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

           COPY CGPW012.

       FD  CGD992.
       01  REG-CGD992.
           05  CODIGO-CG92         PIC 9(8).
           05  CURSO-CG92          PIC 9(3).
           05  TURMA-CG92          PIC XX.
           05  TAMANHO-BECA-CG92   PIC XX.
           05  FOTO-IDENTIFIC-CG92 PIC 9.
           05  CARGO-COMISSAO-CG92 PIC 9.

           COPY CRPW020.

       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR99          PIC 9(8).
           05  COD-COMPL-CR99.
               10  CLASS-CLIENTE-CR99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR99                 PIC 9(5).
           05  PORTADOR-CR99            PIC 99.
           05  CARTEIRA-CR99            PIC 9.
      *    CARTEIRA-CR99  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR99        PIC 99.
           05  NR-DOCTO-CR99            PIC X(10).
           05  OUTRO-DOCTO-CR99         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR99          PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
           05  DATA-EMISSAO-CR99        PIC 9(8).
           05  DATA-VENCTO-CR99         PIC 9(8).
      *    DATA-VENCTO-CR99 - AAAAMMDD
           05  DESCRICAO-CR99           PIC X(30).
           05  SITUACAO-CR99            PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR99          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR99.
               10  NR-PARC-CR99         PIC 99.
               10  TOT-PARC-CR99        PIC 99.
           05  CODREDUZ-APUR-CR99       PIC 9(3).
           05  VALOR-TOT-CR99           PIC 9(8)V99.
           05  JURO-RCTO-CR99           PIC 9(6)V99.
           05  MULTA-RCTO-CR99          PIC 9(6)V99.
           05  DESCONTO-CR99            PIC 9(6)V99.
           05  DATA-RCTO-CR99           PIC 9(8).
      *    DATA-RCTO-CR99 = AAAA/MM/DD
           05  VALOR-LIQ-CR99           PIC 9(8)V99.
           05  CONTABILIZADO-CR99       PIC 9.
           05  VENDEDOR-CR99            PIC 9(6).
           05  RESPONSAVEL-CR99         PIC X(5).
           05  DIGITADOR-CR99           PIC X(5).
           05  SEQ-CAIXA-CR99           PIC 9(3).
           05  NR-NOTA-FISCAL-CR99      PIC X(10).
           05  DATA-NTA-FISCAL-CR99     PIC 9(8).

           COPY CRPW200.

       FD  CRD299.
       01  REG-CRD299.
           05  COD-COMPL-CR299     PIC 9(9).
           05  SEQ-CR299           PIC 9(3).
           05  DATA-MOVTO-CR299    PIC 9(8).
           05  HORA-MOVTO-CR299    PIC 9(4).
           05  DATA-RETORNO-CR299  PIC 9(8).
           05  USUARIO-CR299       PIC X(5).
           05  SITUACAO-ANOTACAO-CR299 PIC 9.

           COPY CRPW201.

       FD  CRD291.
       01  REG-CRD291.
           05  COD-COMPL-CR291     PIC 9(9).
           05  SEQ-CR291           PIC 9(3).
           05  SUBSEQ-CR291        PIC 9(2).
           05  ANOTACAO-CR291      PIC X(80).

           COPY CHPW010.

       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH99          PIC 9(8).
           05  SEQ-CH99                 PIC 9(4).
           05  COD-COMPL-CH99.
               10  CLASS-CLIENTE-CH99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH99                PIC X(30).
           05  CIDADE-CH99              PIC X(19).
           05  LOTE-CH99                PIC X(01).
           05  CONTA-CORR-CH99          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH99            PIC X.
           05  BANCO-CH99               PIC 9(4)   COMP-3.
           05  AGENCIA-CH99             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH99          PIC X.
           05  COMPENSACAO-CH99         PIC 9(3).
           05  CPF-CH99                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH99           PIC X(7).
           05  OUTRO-DOCTO-CH99         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH99         PIC 9(8).
           05  PORTADOR-CH99            PIC 99.
           05  CARTEIRA-CH99            PIC 9.
      *    CARTEIRA-CH99  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH99        PIC 99.
           05  SITUACAO-CH99            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH99       PIC 9(3).
           05  VALOR-CH99               PIC 9(8)V99.
           05  VENDEDOR-CH99            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH99           PIC X(5).
           05  SEQ-CAIXA-CH99           PIC 9(3).
           05  NR-NOTA-FISCAL-CH99      PIC X(10).
           05  DATA-NTA-FISCAL-CH99     PIC 9(8)   COMP-3.
           05  ORIGEM-CH99              PIC 99.


       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".

       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD999             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD991             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-CGD992             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD999             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD299             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CRD291             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  TIPO-W                PIC X(13)    VALUE SPACES.
           05  TIPO1-W               PIC 9(01)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-ASSUNTO           PIC 9(01).
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC Z.ZZ9,99.
           05  REG-AUX               PIC X(700).
       77  COMPARA                   PIC 9(09).

       01 AUX-ERRADO.
          05 AUX-E                   PIC 9(01).
          05 AUX-CLIENTE             PIC 9(08).

           COPY "PARAMETR".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           MOVE 0 TO ERRO-W
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010
           MOVE "CGD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD999
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011
           MOVE "CGD991" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD991
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012
           MOVE "CGD992" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD992
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200
           MOVE "CRD299" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD299
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201
           MOVE "CRD291" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD291
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999

           OPEN I-O
                    CGD010
                    CGD011
                    CGD012
                    CRD020
                    CRD200
                    CRD201
                    CHD010

           OPEN INPUT
                      CGD999
                      CGD991
                      CGD992
                      CRD999
                      CRD299
                      CRD291
                      CHD999

           CLOSE CONTROLE.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-CGD012 = "35"
              CLOSE CGD012      OPEN OUTPUT CGD012
              CLOSE CGD012      OPEN I-O CGD012
           END-IF.
           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O CRD200
           END-IF.
           IF ST-CRD201 = "35"
              CLOSE CRD201      OPEN OUTPUT CRD201
              CLOSE CRD201      OPEN I-O CRD201
           END-IF.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.
           IF ST-CGD010 <> "00"
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CGD999 <> "00"
              STRING "ERRO ABERTURA CGD999: " ST-CGD999 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CGD011 <> "00"
              STRING "ERRO ABERTURA CGD011: " ST-CGD011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CGD991 <> "00"
              STRING "ERRO ABERTURA CGD991: " ST-CGD991 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD200 <> "00"
              STRING "ERRO ABERTURA CRD200: " ST-CRD200 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD299 <> "00"
              STRING "ERRO ABERTURA CRD299: " ST-CRD299 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD201 <> "00"
              STRING "ERRO ABERTURA CRD201: " ST-CRD201 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD291 <> "00"
              STRING "ERRO ABERTURA CRD291: " ST-CRD291 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD020 <> "00"
              STRING "ERRO ABERTURA CRD020: " ST-CRD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CRD999 <> "00"
              STRING "ERRO ABERTURA CRD999: " ST-CRD999 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CHD010 <> "00"
              STRING "ERRO ABERTURA CHD010: " ST-CHD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ST-CHD999 <> "00"
              STRING "ERRO ABERTURA CHD999: " ST-CHD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.


           IF ERRO-W = 1
              PERFORM FINALIZAR-PROGRAMA
           ELSE
              DISPLAY "VOU COMECAR A APAGAR OS ARQUIVOS"
              DISPLAY "VOU COMECAR A APAGAR OS ARQUIVOS" STOP " "
              PERFORM APAGAR-CONTRATOS
              PERFORM APAGAR-CHD010
              PERFORM APAGAR-CRD020
              PERFORM APAGAR-CRD200
              PERFORM APAGAR-CRD201

              DISPLAY "VOU COMECAR A VOLTAR O BACKUP" STOP " "
              PERFORM ATUALIZAR-CONTRATOS
              PERFORM ATUALIZAR-CHD010
              PERFORM ATUALIZAR-CRD020
              PERFORM ATUALIZAR-CRD200
              PERFORM ATUALIZAR-CRD201

              DISPLAY "ACABEI DE ATUALIZAR TUDO" STOP " "
              PERFORM FINALIZAR-PROGRAMA.

       APAGAR-CONTRATOS SECTION.
           INITIALIZE REG-CGD010
           MOVE 0          TO CLASSIF-CG10
           MOVE 07450000   TO CODIGO-CG10
           START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
               READ CGD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD010
               NOT AT END
                   IF CODIGO-CG10(1:4) = "0745"
                      DISPLAY "APAGANDO O CGD010 = 745 " REG-CGD010
                      DELETE CGD010
                   ELSE
                      MOVE "10" TO ST-CGD010
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD011
           MOVE 0          TO CLASSIF-CG11
           MOVE 07450000   TO CODIGO-CG11
           START CGD011 KEY IS NOT LESS COD-COMPL-CG11 INVALID KEY
               MOVE "10" TO ST-CGD011.
           PERFORM UNTIL ST-CGD011 = "10"
               READ CGD011 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD011
               NOT AT END
                   IF CODIGO-CG11(1:4) = "0745"
                      DISPLAY "APAGANDO O CGD011 = 745 " REG-CGD011
                      DELETE CGD011
                   ELSE
                      MOVE "10" TO ST-CGD011
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD012
           MOVE 07450000   TO CODIGO-CG12
           START CGD012 KEY IS NOT LESS CODIGO-CG12 INVALID KEY
               MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
               READ CGD012 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD012
               NOT AT END
                   IF CODIGO-CG12(1:4) = "0745"
                      DISPLAY "APAGANDO O CGD012 = 745 " REG-CGD012
                      DELETE CGD012
                   ELSE
                      MOVE "10" TO ST-CGD012
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD010
           MOVE 0          TO CLASSIF-CG10
           MOVE 13970000   TO CODIGO-CG10
           START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
               READ CGD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD010
               NOT AT END
                   IF CODIGO-CG10(1:4) = "1397"
                      DISPLAY "APAGANDO O CGD010 = 1397 " REG-CGD010
                      DELETE CGD010
                   ELSE
                      MOVE "10" TO ST-CGD010
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD011
           MOVE 0          TO CLASSIF-CG11
           MOVE 13970000   TO CODIGO-CG11
           START CGD011 KEY IS NOT LESS COD-COMPL-CG11 INVALID KEY
               MOVE "10" TO ST-CGD011.
           PERFORM UNTIL ST-CGD011 = "10"
               READ CGD011 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD011
               NOT AT END
                   IF CODIGO-CG11(1:4) = "1397"
                      DISPLAY "APAGANDO O CGD011 = 1397 " REG-CGD011
                      DELETE CGD011
                   ELSE
                      MOVE "10" TO ST-CGD011
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD012
           MOVE 13970000   TO CODIGO-CG12
           START CGD012 KEY IS NOT LESS CODIGO-CG12 INVALID KEY
               MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
               READ CGD012 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD012
               NOT AT END
                   IF CODIGO-CG12(1:4) = "1397"
                      DISPLAY "APAGANDO O CGD012 = 1397 " REG-CGD012
                      DELETE CGD012
                   ELSE
                      MOVE "10" TO ST-CGD012
                   END-IF
               END-READ
           END-PERFORM


           INITIALIZE REG-CGD010
           MOVE 0          TO CLASSIF-CG10
           MOVE 14940000   TO CODIGO-CG10
           START CGD010 KEY IS NOT LESS COD-COMPL-CG10 INVALID KEY
               MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
               READ CGD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD010
               NOT AT END
                   IF CODIGO-CG10(1:4) = "1494"
                      DISPLAY "APAGANDO O CGD010 = 1494 " REG-CGD010
                      DELETE CGD010
                   ELSE
                      MOVE "10" TO ST-CGD010
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD011
           MOVE 0          TO CLASSIF-CG11
           MOVE 14940000   TO CODIGO-CG11
           START CGD011 KEY IS NOT LESS COD-COMPL-CG11 INVALID KEY
               MOVE "10" TO ST-CGD011.
           PERFORM UNTIL ST-CGD011 = "10"
               READ CGD011 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD011
               NOT AT END
                   IF CODIGO-CG11(1:4) = "1494"
                      DISPLAY "APAGANDO O CGD011 = 1494 " REG-CGD011
                      DELETE CGD011
                   ELSE
                      MOVE "10" TO ST-CGD011
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD012
           MOVE 14940000   TO CODIGO-CG12
           START CGD012 KEY IS NOT LESS CODIGO-CG12 INVALID KEY
               MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
               READ CGD012 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD012
               NOT AT END
                   IF CODIGO-CG12(1:4) = "1494"
                      DISPLAY "APAGANDO O CGD012 = 1494 " REG-CGD012
                      DELETE CGD012
                   ELSE
                      MOVE "10" TO ST-CGD012
                   END-IF
               END-READ
           END-PERFORM.

       APAGAR-CHD010 SECTION.
           INITIALIZE REG-CHD010
           MOVE 007450000  TO COD-COMPL-CH10
           START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
               MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
               READ CHD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD010
               NOT AT END
                   IF COD-COMPL-CH10(1:5) = "00745"
                      DISPLAY "APAGANDO O CHD010 = 745 " REG-CHD010
                      DELETE CHD010
                   ELSE
                      MOVE "10" TO ST-CHD010
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CHD010
           MOVE 013970000  TO COD-COMPL-CH10
           START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
               MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
               READ CHD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD010
               NOT AT END
                   IF COD-COMPL-CH10(1:5) = "01397"
                      DISPLAY "APAGANDO O CHD010 = 1397 " REG-CHD010
                      DELETE CHD010
                   ELSE
                      MOVE "10" TO ST-CHD010
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CHD010
           MOVE 014940000  TO COD-COMPL-CH10
           START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
               MOVE "10" TO ST-CHD010.
           PERFORM UNTIL ST-CHD010 = "10"
               READ CHD010 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD010
               NOT AT END
                   IF COD-COMPL-CH10(1:5) = "01494"
                      DISPLAY "APAGANDO O CHD010 = 1494 " REG-CHD010
                      DELETE CHD010
                   ELSE
                      MOVE "10" TO ST-CHD010
                   END-IF
               END-READ
           END-PERFORM.


       APAGAR-CRD020 SECTION.
           INITIALIZE REG-CRD020
           MOVE 007450000  TO COD-COMPL-CR20
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF COD-COMPL-CR20(1:5) = "00745"
                      DISPLAY "APAGANDO O CRD020 = 745 " REG-CRD020
                      DELETE CRD020
                   ELSE
                      MOVE "10" TO ST-CRD020
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD020
           MOVE 013970000  TO COD-COMPL-CR20
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF COD-COMPL-CR20(1:5) = "01397"
                      DISPLAY "APAGANDO O CRD020 = 1397 " REG-CRD020
                      DELETE CRD020
                   ELSE
                      MOVE "10" TO ST-CRD020
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD020
           MOVE 014940000  TO COD-COMPL-CR20
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF COD-COMPL-CR20(1:5) = "01494"
                      DISPLAY "APAGANDO O CRD020 = 1494 " REG-CRD020
                      DELETE CRD020
                   ELSE
                      MOVE "10" TO ST-CRD020
                   END-IF
               END-READ
           END-PERFORM.

       APAGAR-CRD200 SECTION.
           INITIALIZE REG-CRD200
           MOVE 007450000  TO COD-COMPL-CR200
           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
               READ CRD200 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD200
               NOT AT END
                   IF COD-COMPL-CR200(1:5) = "00745"
                      DISPLAY "APAGANDO O CRD200 = 745 " REG-CRD200
                      DELETE CRD200
                   ELSE
                      MOVE "10" TO ST-CRD200
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD200
           MOVE 013970000  TO COD-COMPL-CR200
           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
               READ CRD200 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD200
               NOT AT END
                   IF COD-COMPL-CR200(1:5) = "01397"
                      DISPLAY "APAGANDO O CRD200 = 1397 " REG-CRD200
                      DELETE CRD200
                   ELSE
                      MOVE "10" TO ST-CRD200
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD200
           MOVE 014940000  TO COD-COMPL-CR200
           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
               MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
               READ CRD200 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD200
               NOT AT END
                   IF COD-COMPL-CR200(1:5) = "01494"
                      DISPLAY "APAGANDO O CRD200 = 1494 " REG-CRD200
                      DELETE CRD200
                   ELSE
                      MOVE "10" TO ST-CRD200
                   END-IF
               END-READ
           END-PERFORM.

       APAGAR-CRD201 SECTION.
           INITIALIZE REG-CRD201
           MOVE 007450000  TO COD-COMPL-CR201
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
                   IF COD-COMPL-CR201(1:5) = "00745"
                      DISPLAY "APAGANDO O CRD201 = 745 " REG-CRD201
                      DELETE CRD201
                   ELSE
                      MOVE "10" TO ST-CRD201
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD201
           MOVE 013970000  TO COD-COMPL-CR201
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
                   IF COD-COMPL-CR201(1:5) = "01397"
                      DISPLAY "APAGANDO O CRD201 = 1397 " REG-CRD201
                      DELETE CRD201
                   ELSE
                      MOVE "10" TO ST-CRD201
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD201
           MOVE 014940000  TO COD-COMPL-CR201
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
                   IF COD-COMPL-CR201(1:5) = "01494"
                      DISPLAY "APAGANDO O CRD201 = 1494 " REG-CRD201
                      DELETE CRD201
                   ELSE
                      MOVE "10" TO ST-CRD201
                   END-IF
               END-READ
           END-PERFORM.

      * VOLTA DOS BACKUPS

       ATUALIZAR-CONTRATOS SECTION.
           INITIALIZE REG-CGD999
           MOVE 0          TO CLASSIF-CG99
           MOVE 07450000   TO CODIGO-CG99
           START CGD999 KEY IS NOT LESS COD-COMPL-CG99 INVALID KEY
               MOVE "10" TO ST-CGD999.
           PERFORM UNTIL ST-CGD999 = "10"
               READ CGD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD999
               NOT AT END
                   IF CODIGO-CG99(1:4) = "0745"
                      DISPLAY "ATUALIZANDO O CGD999 = 745 " REG-CGD999
                      MOVE REG-CGD999 TO REG-CGD010
                      WRITE REG-CGD010
                   ELSE
                      MOVE "10" TO ST-CGD999
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD991
           MOVE 0          TO CLASSIF-CG91
           MOVE 07450000   TO CODIGO-CG91
           START CGD991 KEY IS NOT LESS COD-COMPL-CG91 INVALID KEY
               MOVE "10" TO ST-CGD991.
           PERFORM UNTIL ST-CGD991 = "10"
               READ CGD991 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD991
               NOT AT END
                   IF CODIGO-CG91(1:4) = "0745"
                      DISPLAY "ATUALIZANDO O CGD991 = 745 " REG-CGD991
                      MOVE REG-CGD991 TO REG-CGD011
                      WRITE REG-CGD011
                   ELSE
                      MOVE "10" TO ST-CGD991
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD992
           MOVE 07450000   TO CODIGO-CG92
           START CGD992 KEY IS NOT LESS CODIGO-CG92 INVALID KEY
               MOVE "10" TO ST-CGD992.
           PERFORM UNTIL ST-CGD992 = "10"
               READ CGD992 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD992
               NOT AT END
                   IF CODIGO-CG92(1:4) = "0745"
                      DISPLAY "ATUALIZANDO O CGD992 = 745 " REG-CGD992
                      MOVE REG-CGD992 TO REG-CGD012
                      WRITE REG-CGD012
                   ELSE
                      MOVE "10" TO ST-CGD992
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD999
           MOVE 0          TO CLASSIF-CG99
           MOVE 13970000   TO CODIGO-CG99
           START CGD999 KEY IS NOT LESS COD-COMPL-CG99 INVALID KEY
               MOVE "10" TO ST-CGD999.
           PERFORM UNTIL ST-CGD999 = "10"
               READ CGD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD999
               NOT AT END
                   IF CODIGO-CG99(1:4) = "1397"
                      DISPLAY "ATUALIZANDO O CGD999 = 1397 " REG-CGD999
                      MOVE REG-CGD999 TO REG-CGD010
                      WRITE REG-CGD010
                   ELSE
                      MOVE "10" TO ST-CGD999
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD991
           MOVE 0          TO CLASSIF-CG91
           MOVE 13970000   TO CODIGO-CG91
           START CGD991 KEY IS NOT LESS COD-COMPL-CG91 INVALID KEY
               MOVE "10" TO ST-CGD991.
           PERFORM UNTIL ST-CGD991 = "10"
               READ CGD991 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD991
               NOT AT END
                   IF CODIGO-CG91(1:4) = "1397"
                      DISPLAY "ATUALIZANDO O CGD991 = 1397 " REG-CGD991
                      MOVE REG-CGD991 TO REG-CGD011
                      WRITE REG-CGD011
                   ELSE
                      MOVE "10" TO ST-CGD991
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD992
           MOVE 13970000   TO CODIGO-CG92
           START CGD992 KEY IS NOT LESS CODIGO-CG92 INVALID KEY
               MOVE "10" TO ST-CGD992.
           PERFORM UNTIL ST-CGD992 = "10"
               READ CGD992 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD992
               NOT AT END
                   IF CODIGO-CG92(1:4) = "1397"
                      DISPLAY "ATUALIZANDO O CGD992 = 1397 " REG-CGD992
                      MOVE REG-CGD992 TO REG-CGD012
                      WRITE REG-CGD012
                   ELSE
                      MOVE "10" TO ST-CGD992
                   END-IF
               END-READ
           END-PERFORM


           INITIALIZE REG-CGD999
           MOVE 0          TO CLASSIF-CG99
           MOVE 14940000   TO CODIGO-CG99
           START CGD999 KEY IS NOT LESS COD-COMPL-CG99 INVALID KEY
               MOVE "10" TO ST-CGD999.
           PERFORM UNTIL ST-CGD999 = "10"
               READ CGD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD999
               NOT AT END
                   IF CODIGO-CG99(1:4) = "1494"
                      DISPLAY "ATUALIZANDO O CGD999 = 1494 " REG-CGD999
                      MOVE REG-CGD999 TO REG-CGD010
                      WRITE REG-CGD010
                   ELSE
                      MOVE "10" TO ST-CGD999
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD991
           MOVE 0          TO CLASSIF-CG91
           MOVE 14940000   TO CODIGO-CG91
           START CGD991 KEY IS NOT LESS COD-COMPL-CG91 INVALID KEY
               MOVE "10" TO ST-CGD991.
           PERFORM UNTIL ST-CGD991 = "10"
               READ CGD991 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD991
               NOT AT END
                   IF CODIGO-CG91(1:4) = "1494"
                      DISPLAY "ATUALIZANDO O CGD991 = 1494 " REG-CGD991
                      MOVE REG-CGD991 TO REG-CGD011
                      WRITE REG-CGD011
                   ELSE
                      MOVE "10" TO ST-CGD991
                   END-IF
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD992
           MOVE 14940000   TO CODIGO-CG92
           START CGD992 KEY IS NOT LESS CODIGO-CG92 INVALID KEY
               MOVE "10" TO ST-CGD992.
           PERFORM UNTIL ST-CGD992 = "10"
               READ CGD992 NEXT RECORD AT END
                   MOVE "10" TO ST-CGD992
               NOT AT END
                   IF CODIGO-CG92(1:4) = "1494"
                      DISPLAY "ATUALIZANDO O CGD992 = 1494 " REG-CGD992
                      MOVE REG-CGD992 TO REG-CGD012
                      WRITE REG-CGD012
                   ELSE
                      MOVE "10" TO ST-CGD992
                   END-IF
               END-READ
           END-PERFORM.

       ATUALIZAR-CHD010 SECTION.
           INITIALIZE REG-CHD999
           MOVE 007450000  TO COD-COMPL-CH99
           START CHD999 KEY IS NOT LESS ALT-CH94 INVALID KEY
               MOVE "10" TO ST-CHD999.
           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
                   IF COD-COMPL-CH99(1:5) = "00745"
                      DISPLAY "ATUALIZANDO O CHD999 = 745 " REG-CHD999
                      MOVE REG-CHD999 TO REG-CHD010
                      WRITE REG-CHD010
                   ELSE
                      MOVE "10" TO ST-CHD999
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CHD999
           MOVE 013970000  TO COD-COMPL-CH99
           START CHD999 KEY IS NOT LESS ALT-CH94 INVALID KEY
               MOVE "10" TO ST-CHD999.
           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
                   IF COD-COMPL-CH99(1:5) = "01397"
                      DISPLAY "ATUALIZANDO O CHD999 = 1397 " REG-CHD999
                      MOVE REG-CHD999 TO REG-CHD010
                      WRITE REG-CHD010
                   ELSE
                      MOVE "10" TO ST-CHD999
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CHD999
           MOVE 014940000  TO COD-COMPL-CH99
           START CHD999 KEY IS NOT LESS ALT-CH94 INVALID KEY
               MOVE "10" TO ST-CHD999.
           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
                   IF COD-COMPL-CH99(1:5) = "01494"
                      DISPLAY "ATUALIZANDO O CHD999 = 1494 " REG-CHD999
                      MOVE REG-CHD999 TO REG-CHD010
                      WRITE REG-CHD010
                   ELSE
                      MOVE "10" TO ST-CHD999
                   END-IF
               END-READ
           END-PERFORM.

       ATUALIZAR-CRD020 SECTION.
           INITIALIZE REG-CRD999
           MOVE 007450000  TO COD-COMPL-CR99
           START CRD999 KEY IS NOT LESS CHAVE-CR99 INVALID KEY
               MOVE "10" TO ST-CRD999.
           PERFORM UNTIL ST-CRD999 = "10"
               READ CRD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD999
               NOT AT END
                   IF COD-COMPL-CR99(1:5) = "00745"
                      DISPLAY "ATUALIZANDO O CRD999 = 745 " REG-CRD999
                      MOVE REG-CRD999 TO REG-CRD020
                      WRITE REG-CRD020
                   ELSE
                      MOVE "10" TO ST-CRD999
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD999
           MOVE 013970000  TO COD-COMPL-CR99
           START CRD999 KEY IS NOT LESS CHAVE-CR99 INVALID KEY
               MOVE "10" TO ST-CRD999.
           PERFORM UNTIL ST-CRD999 = "10"
               READ CRD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD999
               NOT AT END
                   IF COD-COMPL-CR99(1:5) = "01397"
                      DISPLAY "ATUALIZANDO O CRD999 = 1397 " REG-CRD999
                      MOVE REG-CRD999 TO REG-CRD020
                      WRITE REG-CRD020
                   ELSE
                      MOVE "10" TO ST-CRD999
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD999
           MOVE 014940000  TO COD-COMPL-CR99
           START CRD999 KEY IS NOT LESS CHAVE-CR99 INVALID KEY
               MOVE "10" TO ST-CRD999.
           PERFORM UNTIL ST-CRD999 = "10"
               READ CRD999 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD999
               NOT AT END
                   IF COD-COMPL-CR99(1:5) = "01494"
                      DISPLAY "ATUALIZANDO O CRD999 = 1494 " REG-CRD999
                      MOVE REG-CRD999 TO REG-CRD020
                      WRITE REG-CRD020
                   ELSE
                      MOVE "10" TO ST-CRD999
                   END-IF
               END-READ
           END-PERFORM.

       ATUALIZAR-CRD200 SECTION.
           INITIALIZE REG-CRD299
           MOVE 007450000  TO COD-COMPL-CR299
           START CRD299 KEY IS NOT LESS CHAVE-CR299 INVALID KEY
               MOVE "10" TO ST-CRD299.
           PERFORM UNTIL ST-CRD299 = "10"
               READ CRD299 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD299
               NOT AT END
                   IF COD-COMPL-CR299(1:5) = "00745"
                      DISPLAY "ATUALIZANDO O CRD299 = 745 " REG-CRD299
                      MOVE REG-CRD299 TO REG-CRD200
                      WRITE REG-CRD200
                   ELSE
                      MOVE "10" TO ST-CRD299
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD299
           MOVE 013970000  TO COD-COMPL-CR299
           START CRD299 KEY IS NOT LESS CHAVE-CR299 INVALID KEY
               MOVE "10" TO ST-CRD299.
           PERFORM UNTIL ST-CRD299 = "10"
               READ CRD299 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD299
               NOT AT END
                   IF COD-COMPL-CR299(1:5) = "01397"
                      DISPLAY "ATUALIZANDO O CRD299 = 1397 " REG-CRD299
                      MOVE REG-CRD299 TO REG-CRD200
                      WRITE REG-CRD200
                   ELSE
                      MOVE "10" TO ST-CRD299
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD299
           MOVE 014940000  TO COD-COMPL-CR299
           START CRD299 KEY IS NOT LESS CHAVE-CR299 INVALID KEY
               MOVE "10" TO ST-CRD299.
           PERFORM UNTIL ST-CRD299 = "10"
               READ CRD299 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD299
               NOT AT END
                   IF COD-COMPL-CR299(1:5) = "01494"
                      DISPLAY "ATUALIZANDO O CRD299 = 1494 " REG-CRD299
                      MOVE REG-CRD299 TO REG-CRD200
                      WRITE REG-CRD200
                   ELSE
                      MOVE "10" TO ST-CRD299
                   END-IF
               END-READ
           END-PERFORM.

       ATUALIZAR-CRD201 SECTION.
           INITIALIZE REG-CRD291
           MOVE 007450000  TO COD-COMPL-CR291
           START CRD291 KEY IS NOT LESS CHAVE-CR291 INVALID KEY
               MOVE "10" TO ST-CRD291.
           PERFORM UNTIL ST-CRD291 = "10"
               READ CRD291 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD291
               NOT AT END
                   IF COD-COMPL-CR291(1:5) = "00745"
                      DISPLAY "ATUALIZANDO O CRD291 = 745 " REG-CRD291
                      MOVE REG-CRD291 TO REG-CRD201
                      WRITE REG-CRD201
                   ELSE
                      MOVE "10" TO ST-CRD291
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD291
           MOVE 013970000  TO COD-COMPL-CR291
           START CRD291 KEY IS NOT LESS CHAVE-CR291 INVALID KEY
               MOVE "10" TO ST-CRD291.
           PERFORM UNTIL ST-CRD291 = "10"
               READ CRD291 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD291
               NOT AT END
                   IF COD-COMPL-CR291(1:5) = "01397"
                      DISPLAY "ATUALIZANDO O CRD291 = 1397 " REG-CRD291
                      MOVE REG-CRD291 TO REG-CRD201
                      WRITE REG-CRD201
                   ELSE
                      MOVE "10" TO ST-CRD291
                   END-IF
               END-READ
           END-PERFORM.

           INITIALIZE REG-CRD291
           MOVE 014940000  TO COD-COMPL-CR291
           START CRD291 KEY IS NOT LESS CHAVE-CR291 INVALID KEY
               MOVE "10" TO ST-CRD291.
           PERFORM UNTIL ST-CRD291 = "10"
               READ CRD291 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD291
               NOT AT END
                   IF COD-COMPL-CR291(1:5) = "01494"
                      DISPLAY "ATUALIZANDO O CRD291 = 1494 " REG-CRD291
                      MOVE REG-CRD291 TO REG-CRD201
                      WRITE REG-CRD201
                   ELSE
                      MOVE "10" TO ST-CRD291
                   END-IF
               END-READ
           END-PERFORM.


       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM"
           MOVE 1 TO ERRO-W.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CHD010
                 CGD010 CRD020 CRD200 CRD201
                 CGD011 CGD012

           CLOSE CHD999
                 CGD999 CRD999 CRD299 CRD291
                 CGD991 CGD992

           EXIT PROGRAM
           STOP RUN.


