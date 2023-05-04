      * Arquivo de registros estornados do CPD020
       FD  CPD099.
       01  REG-CPD099.
           05  DATA-MOVTO-CP99          PIC 9(8).
           05  FORNEC-CP99              PIC 9(6).
           05  SEQ-CP99                 PIC 9(5).
           05  TIPO-FORN-CP99           PIC 9(2).
           05  PORTADOR-CP99            PIC 9999.
           05  NR-DOCTO-CP99            PIC X(10).
           05  DATA-EMISSAO-CP99        PIC 9(8).
           05  DATA-VENCTO-CP99         PIC 9(8).
           05  DESCRICAO-CP99           PIC X(30).
           05  PREV-DEF-CP99            PIC 9.
      *    PREV-DEF = 0-DEFIN  1-PREVISTO
           05  SITUACAO-CP99            PIC 9.
      *    SITUACAO = 0-OK  1-SUSPENSA  2-PAGA  3-ESTONADA  4-CANCELADA
           05  LIBERADO-CP99            PIC 9.
      *    LIBERADO = 0-NÃO     1-SIM
           05  TIPO-MOEDA-CP99          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CP99.
               10  NR-PARC-CP99         PIC 99.
               10  TOT-PARC-CP99        PIC 99.
           05  CODREDUZ-APUR-CP99       PIC 9(5).
           05  JUROS-MORA-CP99          PIC 9(6)V99.
           05  MULTA-ATRASO-CP99        PIC 9(6)V99.
           05  TAXA-APLIC-CP99          PIC 99V99.
           05  VALOR-TOT-CP99           PIC 9(8)V99.
           05  JURO-PAGO-CP99           PIC 9(6)V99.
           05  MULTA-PAGA-CP99          PIC 9(6)V99.
           05  DESCONTO-CP99            PIC 9(6)V99.
           05  DATA-PGTO-CP99           PIC 9(8).
           05  VALOR-LIQ-CP99           PIC 9(8)V99.
           05  CONTABILIZADO-CP99       PIC 9.
           05  RESPONSAVEL-CP99         PIC X(5).
           05  DIGITADOR-CP99           PIC X(5).
           05  SEQ-CAIXA-CP99           PIC 9(3).
           05  TIPO-CONTA-CP99          PIC 9.
      *  TIPO-CONTA = 0(TEMPORARIA)   1-PERMANENTE
