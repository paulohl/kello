      * Arquivo de Movimento de contas a pagar
       FD  CPD020.
       01  REG-CPD020.
           05  DATA-MOVTO-CP20          PIC 9(8).
      *    DATA-MOVTO-CP20 - DATA-INVERTIDA
           05  FORNEC-CP20              PIC 9(6).
           05  SEQ-CP20                 PIC 9(5).
           05  TIPO-FORN-CP20           PIC 9(2).
           05  PORTADOR-CP20            PIC 9999.
           05  NR-DOCTO-CP20            PIC X(10).
           05  DATA-EMISSAO-CP20        PIC 9(8).
      *    DATA-EMISSAO-CP20 - DATA-INVERTIDA
           05  DATA-VENCTO-CP20         PIC 9(8).
           05  DESCRICAO-CP20           PIC X(30).
           05  PREV-DEF-CP20            PIC 9.
      *    PREV-DEF = 0-DEFIN  1-PREVISTO
           05  SITUACAO-CP20            PIC 9.
      *    SITUACAO = 0-OK  1-SUSPENSA  2-PAGA  3-ESTONADA  4-CANCELADA
           05  LIBERADO-CP20            PIC 9.
      *    LIBERADO = 0-NÃO     1-SIM
           05  TIPO-MOEDA-CP20          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CP20.
               10  NR-PARC-CP20         PIC 99.
               10  TOT-PARC-CP20        PIC 99.
           05  CODREDUZ-APUR-CP20       PIC 9(5).
           05  JUROS-MORA-CP20          PIC 9(6)V99.
           05  MULTA-ATRASO-CP20        PIC 9(6)V99.
           05  TAXA-APLIC-CP20          PIC 99V99.
           05  VALOR-TOT-CP20           PIC 9(8)V99.
           05  JURO-PAGO-CP20           PIC 9(6)V99.
           05  MULTA-PAGA-CP20          PIC 9(6)V99.
           05  DESCONTO-CP20            PIC 9(6)V99.
           05  DATA-PGTO-CP20           PIC 9(8).
           05  VALOR-LIQ-CP20           PIC 9(8)V99.
           05  CONTABILIZADO-CP20       PIC 9.
           05  RESPONSAVEL-CP20         PIC X(5).
           05  DIGITADOR-CP20           PIC X(5).
           05  SEQ-CAIXA-CP20           PIC 9(3).
           05  TIPO-CONTA-CP20          PIC 9.
      *  TIPO-CONTA = 0(TEMPORARIA)   1-PERMANENTE
