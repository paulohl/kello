      * Arquivo de Movimento de contas a receber
       FD  CRD020.
       01  REG-CRD020.
           05  DATA-MOVTO-CR20                  PIC 9(8).
           05  CHAVE-CR20.
               10  COD-COMPL-CR20.
                   15  CLASS-CLIENTE-CR20       PIC 9.
      *    classificação cliente =  0-contrato  1-comum
                   15  CLIENTE-CR20             PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
               10 SEQ-CR20                      PIC 9(5).
           05  PORTADOR-CR20                    PIC 9999.
           05  CARTEIRA-CR20                    PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR20                PIC 99.
           05  NR-DOCTO-CR20                    PIC X(10).
           05  OUTRO-DOCTO-CR20                 PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR20                  PIC 9.
      *    TIPO-DOCTO = 0-BOLETO     1-DUPL/PROMIS.     2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR20                PIC 9(8).
           05  DATA-VENCTO-CR20                 PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR20                   PIC X(30).
           05  SITUACAO-CR20                    PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA 4-CANCELADA  5-DESCONTADA
      *               1-PARCIAL
           05  TIPO-MOEDA-CR20                  PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR20.
               10  NR-PARC-CR20                 PIC 99.
               10  TOT-PARC-CR20                PIC 99.
           05  CODREDUZ-APUR-CR20               PIC 9(5).
           05  VALOR-TOT-CR20                   PIC 9(8)V99.
           05  JURO-RCTO-CR20                   PIC 9(6)V99.
           05  MULTA-RCTO-CR20                  PIC 9(6)V99.
           05  DESCONTO-CR20                    PIC 9(6)V99.
           05  DATA-RCTO-CR20                   PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR20                   PIC 9(8)V99.
           05  CONTABILIZADO-CR20               PIC 9.
           05  VENDEDOR-CR20                    PIC 9(6).
           05  RESPONSAVEL-CR20                 PIC X(5).
           05  DIGITADOR-CR20                   PIC X(5).
           05  SEQ-CAIXA-CR20                   PIC 9(3).
           05  NR-NOTA-FISCAL-CR20              PIC X(10).
           05  DATA-NTA-FISCAL-CR20             PIC 9(8).
           05  FORMA-PAGTO-CR20                 PIC X(10).
           05  DCR-MEM-CR20                     PIC X(15).
           05  CARTAO-CRED-CR20                 PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR20       PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR20       PIC 9(03)V99.
           05  VALOR-SALDO-CR20                 PIC 9(08)V99.
           05  LOTE-CR20                        PIC 9(02).
           05  RECEBEDOR-CR20                   PIC X(06).
