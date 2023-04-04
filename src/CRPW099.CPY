      * registro excluido do contas a receber
       FD  CRD099.
       01  REG-CRD099.
           05  DATA-MOVTO-CR99             PIC 9(8).
           05  CLASS-CLIENTE-CR99          PIC 9.
      *    classificação cliente =  0-contrato  1-comum
           05  CLIENTE-CR99                PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR99                    PIC 9(5).
           05  PORTADOR-CR99               PIC 9999.
           05  CARTEIRA-CR99               PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR99           PIC 99.
           05  NR-DOCTO-CR99               PIC X(10).
           05  OUTRO-DOCTO-CR99            PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR99             PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
           05  DATA-EMISSAO-CR99           PIC 9(8).
           05  DATA-VENCTO-CR99            PIC 9(8).
           05  DESCRICAO-CR99              PIC X(30).
           05  SITUACAO-CR99               PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR99             PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR99.
               10  NR-PARC-CR99            PIC 99.
               10  TOT-PARC-CR99           PIC 99.
           05  CODREDUZ-APUR-CR99          PIC 9(5).
           05  VALOR-TOT-CR99              PIC 9(8)V99.
           05  JURO-RCTO-CR99              PIC 9(6)V99.
           05  MULTA-RCTO-CR99             PIC 9(6)V99.
           05  DESCONTO-CR99               PIC 9(6)V99.
           05  DATA-RCTO-CR99              PIC 9(8).
           05  VALOR-LIQ-CR99              PIC 9(8)V99.
           05  CONTABILIZADO-CR99          PIC 9.
           05  RESPONSAVEL-CR99            PIC X(5).
           05  DIGITADOR-CR99              PIC X(5).
           05  SEQ-CAIXA-CR99              PIC 9(3).
           05  NR-NOTA-FISCAL-CR99         PIC X(10).
           05  DATA-NTA-FISCAL-CR99        PIC 9(8).
           05  FORMA-PAGTO-CR99            PIC X(10).
           05  DCR-MEM-CR99                PIC X(15).
           05  CARTAO-CRED-CR99            PIC 9(02).
           05  TAXA-ADMINIST-CREDITO-CR99  PIC 9(03)V99.
           05  TAXA-ADMINIST-PARCELA-CR99  PIC 9(03)V99.
           05  USUARIO-EXCLUSAO-CR99       PIC X(05).
           05  DATA-EXCLUSAO-CR99          PIC 9(08).
           05  HORA-EXCLUSAO-CR99          PIC 9(04).
           05  FILLER                      PIC X(01).
