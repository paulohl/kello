      * ARQUIVO DO MOVIMENTO DE CHEQUES
       FD  CHD010.
       01  REG-CHD010.
           05  DATA-MOVTO-CH10          PIC 9(8).
           05  SEQ-CH10                 PIC 9(4).
           05  COD-COMPL-CH10.
               10  CLASS-CLIENTE-CH10   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH10         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH10                PIC X(30).
           05  CIDADE-CH10              PIC X(19).
           05  LOTE-CH10                PIC X(01).
           05  CONTA-CORR-CH10          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH10            PIC X.
           05  BANCO-CH10               PIC 9(4)   COMP-3.
           05  AGENCIA-CH10             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH10          PIC X.
           05  COMPENSACAO-CH10         PIC 9(3).
           05  CPF-CH10                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH10           PIC X(7).
           05  OUTRO-DOCTO-CH10         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH10         PIC 9(8).
           05  PORTADOR-CH10            PIC 9999.
           05  CARTEIRA-CH10            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH10        PIC 99.
           05  SITUACAO-CH10            PIC 9.
      *    SITUACAO = 0-OK 1-PARCIAL 2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH10       PIC 9(5).
           05  VALOR-CH10               PIC 9(8)V99.
           05  VENDEDOR-CH10            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH10           PIC X(5).
           05  SEQ-CAIXA-CH10           PIC 9(3).
           05  NR-NOTA-FISCAL-CH10      PIC X(10).
           05  DATA-NTA-FISCAL-CH10     PIC 9(8)   COMP-3.
           05  ORIGEM-CH10              PIC 99.
           05  VALOR-SALDO-CH10         PIC 9(08)V99.
           05  FILLER                   PIC X(100).
