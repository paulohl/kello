       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD020Juntar.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 02-11-2010
      *DESCRIÇÃO: Juntar Arquivos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.

          SELECT CRD020A ASSIGN TO PATH-CRD020A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD020A
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVEA-CR20 = COD-COMPL-CR20A SEQ-CR20A
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR20A
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1A-CR20 = COD-COMPL-CR20A
                        DATA-VENCTO-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2A-CR20 = SITUACAO-CR20A
                        DATA-VENCTO-CR20A
                        COD-COMPL-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3A-CR20 = SITUACAO-CR20A
                        DATA-MOVTO-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4A-CR20 = SITUACAO-CR20A
                        CLIENTE-CR20A DATA-VENCTO-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6A-CR20 = DATA-RCTO-CR20A
                       SEQ-CAIXA-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7A-CR20 = NR-DOCTO-CR20A
                       DATA-RCTO-CR20A
                       SEQ-CAIXA-CR20A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8A-CR20 = OUTRO-DOCTO-CR20A
                       DATA-RCTO-CR20A
                       SEQ-CAIXA-CR20A WITH DUPLICATES.

          SELECT CRD020B ASSIGN TO PATH-CRD020B
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD020B
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVEB-CR20 = COD-COMPL-CR20B SEQ-CR20B
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR20B
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1B-CR20 = COD-COMPL-CR20B
                        DATA-VENCTO-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2B-CR20 = SITUACAO-CR20B
                        DATA-VENCTO-CR20B
                        COD-COMPL-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3B-CR20 = SITUACAO-CR20B
                        DATA-MOVTO-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4B-CR20 = SITUACAO-CR20B
                        CLIENTE-CR20B DATA-VENCTO-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6B-CR20 = DATA-RCTO-CR20B
                       SEQ-CAIXA-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7B-CR20 = NR-DOCTO-CR20B
                       DATA-RCTO-CR20B
                       SEQ-CAIXA-CR20B WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8B-CR20 = OUTRO-DOCTO-CR20B
                       DATA-RCTO-CR20B
                       SEQ-CAIXA-CR20B WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.


      * Arquivo de Movimento de contas a receber
       FD  CRD020A.
       01  REG-CRD020A.
           05  DATA-MOVTO-CR20A                 PIC 9(8).
           05  COD-COMPL-CR20A.
               10  CLASS-CLIENTE-CR20A      PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20A            PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20A                    PIC 9(5).
           05  PORTADOR-CR20A                   PIC 9999.
           05  CARTEIRA-CR20A                   PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20A               PIC 99.
           05  NR-DOCTO-CR20A                   PIC X(10).
           05  OUTRO-DOCTO-CR20A                PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20A                 PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20A               PIC 9(8).
           05  DATA-VENCTO-CR20A                PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20A                  PIC X(30).
           05  SITUACAO-CR20A                   PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20A                 PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20A.
               10  NR-PARC-CR20A                PIC 99.
               10  TOT-PARC-CR20A               PIC 99.
           05  CODREDUZ-APUR-CR20A              PIC 9(5).
           05  VALOR-TOT-CR20A                  PIC 9(8)V99.
           05  JURO-RCTO-CR20A                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20A                 PIC 9(6)V99.
           05  DESCONTO-CR20A                   PIC 9(6)V99.
           05  DATA-RCTO-CR20A                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20A                  PIC 9(8)V99.
           05  CONTABILIZADO-CR20A              PIC 9.
           05  VENDEDOR-CR20A                   PIC 9(6).
           05  RESPONSAVEL-CR20A                PIC X(5).
           05  DIGITADOR-CR20A                  PIC X(5).
           05  SEQ-CAIXA-CR20A                  PIC 9(3).
           05  NR-NOTA-FISCAL-CR20A             PIC X(10).
           05  DATA-NTA-FISCAL-CR20A            PIC 9(8).
           05  FORMA-PAGTO-CR20A                PIC X(10).
           05  DCR-MEM-CR20A                    PIC X(15).
           05  CARTAO-CRED-CR20A                PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20A      PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20A      PIC 9(03)V99.
           05  VALOR-SALDO-CR20A                PIC 9(08)V99.
           05  FILLER                           PIC X(08).

      * Arquivo de Movimento de contas a receber
       FD  CRD020B.
       01  REG-CRD020B.
           05  DATA-MOVTO-CR20B                 PIC 9(8).
           05  COD-COMPL-CR20B.
               10  CLASS-CLIENTE-CR20B      PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20B            PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20B                    PIC 9(5).
           05  PORTADOR-CR20B                   PIC 9999.
           05  CARTEIRA-CR20B                   PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20B               PIC 99.
           05  NR-DOCTO-CR20B                   PIC X(10).
           05  OUTRO-DOCTO-CR20B                PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20B                 PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20B               PIC 9(8).
           05  DATA-VENCTO-CR20B                PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20B                  PIC X(30).
           05  SITUACAO-CR20B                   PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20B                 PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20B.
               10  NR-PARC-CR20B                PIC 99.
               10  TOT-PARC-CR20B               PIC 99.
           05  CODREDUZ-APUR-CR20B              PIC 9(5).
           05  VALOR-TOT-CR20B                  PIC 9(8)V99.
           05  JURO-RCTO-CR20B                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20B                 PIC 9(6)V99.
           05  DESCONTO-CR20B                   PIC 9(6)V99.
           05  DATA-RCTO-CR20B                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20B                  PIC 9(8)V99.
           05  CONTABILIZADO-CR20B              PIC 9.
           05  VENDEDOR-CR20B                   PIC 9(6).
           05  RESPONSAVEL-CR20B                PIC X(5).
           05  DIGITADOR-CR20B                  PIC X(5).
           05  SEQ-CAIXA-CR20B                  PIC 9(3).
           05  NR-NOTA-FISCAL-CR20B             PIC X(10).
           05  DATA-NTA-FISCAL-CR20B            PIC 9(8).
           05  FORMA-PAGTO-CR20B                PIC X(10).
           05  DCR-MEM-CR20B                    PIC X(15).
           05  CARTAO-CRED-CR20B                PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20B      PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20B      PIC 9(03)V99.
           05  VALOR-SALDO-CR20B                PIC 9(08)V99.
           05  FILLER                           PIC X(08).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020A            PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
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
           05  ACP-CAMINHO3          PIC X(255) VALUE SPACES.
           05  ACP-DTINI             PIC 9(08)  VALUE ZEROS.
           05  ACP-DTFIM             PIC 9(08)  VALUE ZEROS.
           05  WS-OK                 PIC X(01)  VALUE SPACES.
           05  DATA-INI              PIC 9(08)  VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)  VALUE ZEROS.
           05  RESP                  PIC X(01)  VALUE SPACES.

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

       001-caminho1.
           display "Informar o Caminho Arquivo Original: "
                at 0101
           accept acp-caminho1 at 0138

           if acp-caminho1 = spaces
              go to 001-caminho1
           else
              call "CBL_CHECK_FILE_EXIST"     using acp-caminho1
                                                    file-details
                                          returning status-code
              if status-code <> 0
                 display "Arquivo Nao Encontrado" at 1001
                    stop " "
                 display "                      " at 1001
                 go to 001-caminho1
              else
                 display "                      " at 1001.

       002-caminho2.
           display "Informar o Caminho Arquivo Backup..: "
                at 0201
           accept acp-caminho2 at 0238

           if acp-caminho2 = spaces
              go to 002-caminho2
           else
              call "CBL_CHECK_FILE_EXIST"     using acp-caminho2
                                                    file-details
                                          returning status-code
              if status-code <> 0
                 display "Arquivo de Backup Nao Encontrado" at 1001
                    stop " "
                 display "                                "  at 1001
                 go to 002-caminho2
              else
                 display "                                " at 1001.

       002-caminho3.
           display "Informar o Caminho Arquivo Novo....: "
                at 0301
           accept acp-caminho3 at 0338

           if acp-caminho3 = spaces
              go to 002-caminho3.


           move acp-caminho1                     to path-crd020
           open i-o crd020

           if st-crd020 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho1 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho2                     to path-crd020a
           open i-o crd020a

           if st-crd020a <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho2 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho3                     to path-crd020b
           open i-o crd020b

           if st-crd020b <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho3 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.


           initialize reg-crd020a
           start crd020a key is not less chavea-cr20 invalid key
                 move "10" to st-crd020a
           end-start
           perform until st-crd020a = "10"
                 read crd020a next at end
                      move "10" to st-crd020a
                 not at end
                         display reg-crd020a  at 1001
                         move reg-crd020a to reg-crd020b
                         write reg-crd020b
                 end-read
           end-perform

           initialize reg-crd020
           start crd020 key is not less chave-cr20 invalid key
                 move "10" to st-crd020
           end-start
           perform until st-crd020 = "10"
                 read crd020 next at end
                      move "10" to st-crd020
                 not at end
                         display reg-crd020  at 1001
                         move reg-crd020 to reg-crd020b
                         write reg-crd020b
                 end-read
           end-perform.


       006-sair.
           display "acabei " stop " "
           close crd020 crd020a crd020b

           STOP RUN.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem
           move 1      to flag-critica.
