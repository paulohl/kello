      * ARQUIVO DO MOVIMENTO DE CHEQUES - CHEQUES EXCLUÍDOS
       FD  CHD099.
       01  REG-CHD099.
           05  DATA-MOVTO-CH99          PIC 9(8).
           05  SEQ-CH99                 PIC 9(4).
           05  COD-COMPL-CH99.
               10  CLASS-CLIENTE-CH99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH99                PIC X(30).
           05  CIDADE-CH99              PIC X(20).
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
           05  PORTADOR-CH99            PIC 9999.
           05  CARTEIRA-CH99            PIC 9.
      *    CARTEIRA-CH99  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH99        PIC 99.
           05  SITUACAO-CH99            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH99       PIC 9(5).
           05  VALOR-CH99               PIC 9(8)V99.
           05  VENDEDOR-CH99            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH99           PIC X(5).
           05  SEQ-CAIXA-CH99           PIC 9(3).
           05  NR-NOTA-FISCAL-CH99      PIC X(10).
           05  DATA-NTA-FISCAL-CH99     PIC 9(8)   COMP-3.
           05  ORIGEM-CH99              PIC 99.
           05  USUARIO-EXCLUSAO-CH99    PIC X(5).
           05  DATA-EXCLUSAO-CH99       PIC 9(08).
           05  HORA-EXCLUSAO-CH99       PIC 9(06).
           05  FILLER                   PIC X(30).
