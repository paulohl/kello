       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD020juntar5.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 23-08-2010
      *DESCRIÇÃO: Conversão GALHOCRD020Juntar5

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.

          $set IDXFORMAT"4" FILETYPE"4"
          SELECT CRD020T ASSIGN TO PATH-CRD020T
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD020T
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR20T = COD-COMPL-CR20T SEQ-CR20T
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR20T
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR20T = COD-COMPL-CR20T
                        DATA-VENCTO-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR20T = SITUACAO-CR20T
                       DATA-VENCTO-CR20T COD-COMPL-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR20T = SITUACAO-CR20T
                        DATA-MOVTO-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR20T = SITUACAO-CR20T
                        CLIENTE-CR20T DATA-VENCTO-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR20T = DATA-RCTO-CR20T
                       SEQ-CAIXA-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT7-CR20T = NR-DOCTO-CR20T
                       DATA-RCTO-CR20T
                       SEQ-CAIXA-CR20T WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT8-CR20T = OUTRO-DOCTO-CR20T
                       DATA-RCTO-CR20T
                       SEQ-CAIXA-CR20T WITH DUPLICATES.
           $set IDXFORMAT"0" FILETYPE"0"

          SELECT CRD020a ASSIGN TO PATH-CRD020a
                  ORGANIZATION  IS   SEQUENTIAL
                  ACCESS MODE   IS   SEQUENTIAL
                  STATUS        IS   ST-CRD020a.

          SELECT REPETI ASSIGN TO PATH-REPETI
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.

       FD CRD020a.
       01 REG-CRD020A                PIC X(01).

      * Arquivo de Movimento de contas a receber
       FD  CRD020T.
       01  REG-CRD020T.
           05  DATA-MOVTO-CR20T                 PIC 9(8).
           05  COD-COMPL-CR20T.
               10  CLASS-CLIENTE-CR20T      PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR20T            PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR20T                    PIC 9(5).
           05  PORTADOR-CR20T                   PIC 9999.
           05  CARTEIRA-CR20T                   PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20T               PIC 99.
           05  NR-DOCTO-CR20T                   PIC X(10).
           05  OUTRO-DOCTO-CR20T                PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20T                 PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20T               PIC 9(8).
           05  DATA-VENCTO-CR20T                PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20T                  PIC X(30).
           05  SITUACAO-CR20T                   PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20T                 PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20T.
               10  NR-PARC-CR20T                PIC 99.
               10  TOT-PARC-CR20T               PIC 99.
           05  CODREDUZ-APUR-CR20T              PIC 9(5).
           05  VALOR-TOT-CR20T                  PIC 9(8)V99.
           05  JURO-RCTO-CR20T                  PIC 9(6)V99.
           05  MULTA-RCTO-CR20T                 PIC 9(6)V99.
           05  DESCONTO-CR20T                   PIC 9(6)V99.
           05  DATA-RCTO-CR20T                  PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20T                  PIC 9(8)V99.
           05  CONTABILIZADO-CR20T              PIC 9.
           05  VENDEDOR-CR20T                   PIC 9(6).
           05  RESPONSAVEL-CR20T                PIC X(5).
           05  DIGITADOR-CR20T                  PIC X(5).
           05  SEQ-CAIXA-CR20T                  PIC 9(3).
           05  NR-NOTA-FISCAL-CR20T             PIC X(10).
           05  DATA-NTA-FISCAL-CR20T            PIC 9(8).
           05  FORMA-PAGTO-CR20T                PIC X(10).
           05  DCR-MEM-CR20T                    PIC X(15).
           05  CARTAO-CRED-CR20T                PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20T      PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20T      PIC 9(03)V99.
           05  VALOR-SALDO-CR20T                PIC 9(08)V99.
           05  LOTE-CR20T                       PIC 9(02).
           05  FILLER                           PIC X(06).

       FD REPETI.
       01 REG-REPETI.
          05 REPETI-CLASS                       PIC 9(01).
          05 FILLER                             PIC X(01).
          05 REPETI-CLIENTE                     PIC 9(08).
          05 FILLER                             PIC X(01).
          05 REPETI-DIA                         PIC 9(02).
          05 FILLER                             PIC X(01) VALUE "/".
          05 REPETI-MES                         PIC 9(02).
          05 FILLER                             PIC X(01) VALUE "/".
          05 REPETI-ANO                         PIC 9(04).
          05 FILLER                             PIC X(01).
          05 REPETI-VALOR                       PIC ZZ.ZZZ.ZZ9,99.
          05 FILLER                             PIC X(01).
          05 REPETI-DIA-MOV                     PIC 9(02).
          05 FILLER                             PIC X(01) VALUE "/".
          05 REPETI-MES-MOV                     PIC 9(02).
          05 FILLER                             PIC X(01) VALUE "/".
          05 REPETI-ANO-MOV                     PIC 9(04).
          05 FILLER                             PIC X(01).
          05 REPETI-SEQUENCIA                   PIC ZZ.ZZZ.ZZ9.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020t            PIC XX       VALUE SPACES.
           05  ST-CRD020a            PIC XX       VALUE SPACES.
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
           05  qtd-titulo            pic 9(09)  value zeros.
           05  igual                 pic 9(09)  value zeros.
           05  gravar                pic x(01)  value spaces.
           05  masc-qtde             pic zzz.zzz.zz9 value zeros.
           05  masc-igual            pic zzz.zzz.zz9 value zeros.
           05  ind                   pic 9(03)  value zeros.

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
              go to 001-caminho1.
      *    else
      *       call "CBL_CHECK_FILE_EXIST"     using acp-caminho1
      *                                             file-details
      *                                   returning status-code
      *       if status-code <> 0
      *          display "Arquivo Nao Encontrado" at 1001
      *             stop " "
      *          display "                      " at 1001
      *          go to 001-caminho1
      *       else
      *          display "                      " at 1001.

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

           move acp-caminho1                     to path-crd020
           move acp-caminho1                     to path-crd020T
           open i-o crd020 crd020T

           if st-crd020 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho1 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho2                     to path-crd020a
           open input crd020a

           if st-crd020a <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho2 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.


           move "REPETI.TXT" to path-repeti

           open output repeti.


      *003-data-ini.
      *    move zeros                          to acp-dtini
      *    display "Informar a Data Inicial: " at 0401
      *    accept acp-dtini                    at 0426
      *
      *    if acp-dtini equal zeros
      *       move function current-date to ws-data-sys
      *       string ws-dia-cpu ws-mes-cpu ws-ano-cpu into acp-dtini
      *    else
      *       call   "UTIVLDT" using acp-dtini ws-ok
      *       cancel "UTIVLDT"
      *       if ws-ok equal "N"
      *          display "Data Inicial Invalida" at 1001 stop " "
      *          display "                     " at 1001
      *          go to 003-data-ini
      *       end-if
      *    end-if
      *
      *    string acp-dtini(5:4) acp-dtini(3:2) acp-dtini(1:2)
      *      into data-ini.
      *
      *004-data-fim.
      *    move zeros                          to acp-dtfim
      *    display "Informar a Data Final..: " at 0501
      *    accept acp-dtfim                    at 0526
      *
      *    if acp-dtfim equal zeros
      *       move function current-date to ws-data-sys
      *       string ws-dia-cpu ws-mes-cpu ws-ano-cpu into acp-dtfim
      *    else
      *       call   "UTIVLDT" using acp-dtfim ws-ok
      *       cancel "UTIVLDT"
      *       if ws-ok equal "N"
      *          display "Data Final Invalida"   at 1001 stop " "
      *          display "                   "   at 1001
      *          go to 004-data-fim
      *       end-if
      *    end-if
      *
      *    string acp-dtfim(5:4) acp-dtfim(3:2) acp-dtfim(1:2)
      *      into data-fim
      *
      *    if data-fim < data-ini
      *       display "Data Final Menor que a Inicial" at 1001 stop " "
      *       display "                              " at 1001
      *       go to 004-data-fim.


           if flag-critica = 0
              initialize reg-crd020a
                         qtd-titulo
                         igual
              perform until st-crd020a = "10"
                    read crd020a next at end
                         move "10" to st-crd020a
                    not at end
                         display reg-crd020a  at 1001
                         if reg-crd020a = "A"
                            read crd020a next at end
                                 move "10" to st-crd020a
                            not at end
                                 if reg-crd020a = " "
                                    perform ler-267
                                 end-if
                            end-read
                         end-if
                    end-read
              end-perform
           end-if.

       006-sair.
           move qtd-titulo to masc-qtde
           move igual      to masc-igual

           move spaces to mensagem
           string "Quantidade de Registros Atualizados " masc-qtde
           x"0da0" "Registros Iguais " masc-igual
             into mensagem
           move   "C" to tipo-msg
           perform exibir-mensagem

           close crd020 crd020a crd020t repeti

           STOP RUN.

       ler-267 section.
           initialize reg-crd020

           move 0 to ind
           perform until ind = 267
               add 1 to ind
               read crd020a next at end
                    move 267 to ind
                    move "10" to st-crd020a
               not at end
                    move reg-crd020a to reg-crd020(ind:1)
               end-read
           end-perform
           if DATA-MOVTO-CR20 > 0
              move "S"        to gravar

              initialize reg-crd020t
              move reg-crd020 to reg-crd020t
              start crd020t key is not less alt1-cr20t invalid key
                    move "10" to st-crd020t
              end-start
              perform until st-crd020t = "10"
                    read crd020t next at end
                         move "10" to st-crd020t
                    not at end
                         if COD-COMPL-CR20 = COD-COMPL-CR20T and
                            DATA-VENCTO-CR20 = DATA-VENCTO-CR20T and
                            VALOR-TOT-CR20 = VALOR-TOT-CR20T
                            add 1    to igual
                            move "N" to gravar

                            move class-cliente-cr20    to repeti-class
                            move cliente-cr20          to repeti-cliente
                            move data-vencto-cr20(1:4) to repeti-ano
                            move data-vencto-cr20(5:2) to repeti-mes
                            move data-vencto-cr20(7:2) to repeti-dia
                            move valor-tot-cr20        to repeti-valor
                            move data-movto-cr20(1:4)  to repeti-ano-mov
                            move data-movto-cr20(5:2)  to repeti-mes-mov
                            move data-movto-cr20(7:2)  to repeti-dia-mov
                            move seq-cr20            to repeti-sequencia
                            write reg-repeti
                         else
                            move "10" to st-crd020t
                         end-if
                    end-read
              end-perform

              if gravar = "S"
                 display "reg-crd020 = " reg-crd020
                 write reg-crd020 not invalid key
                      add 1 to qtd-titulo
                 end-write.
       ler-267-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem
           move 1      to flag-critica.
