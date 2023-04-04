      * Cadatro de contrato de Organização de Evento
       FD  OED010.
       01  REG-OED010.
           05  NR-CONTRATO-OE10         PIC 9(4).
           05  TOTAL-ALUNO-OE10         PIC 9(4).
           05  REPRESENTANTE-OE10       PIC 9(6).
           05  ASSINATURA-OE10          PIC 9(8)    COMP-3.
      *    assinatura- data-assinatura invertida (aaaa/mm/dd)
           05  DATA-ULT-EVENTO-OE10     PIC 9(8).
      *    DATA-ULTIMO-EVENTO - INVERTIDA (AAAA/MM/DD)
           05  ALUNO-PRES-OE10          PIC 9(4).
           05  ALUNO-TESOU-OE10         PIC 9(4).
           05  VLR-TOTAL-OE10           PIC 9(8)V99.
           05  QTDE-PARCELA-OE10        PIC 9(3).
           05  TIPO-RECTO-OE10          PIC 9.
      *    1-'parcelamento individual' de cada aluno
      *    2-'direto da comissão' - o cliente neste caso deverá ter o
      *       código - ????-0000 (onde ???? será o nr do contrato).
           05  CONDICAO-RECTO-OE10      PIC X(30).
      *    campo descritivo que será do tipo - cobrança bancária,
      *    depósito no vencto, cheque pré-datado, etc...
           05  CONDICAO-REAJUSTE-OE10   PIC X(30).
      *    ex. gatilho de 7% de inflação
           05  TAXA-JURO-ATRASO-OE10    PIC 9(2)V99.
           05  MULTA-ATRASO-OE10        PIC X(20).
           05  MULTA-RESCISAO-OE10      PIC X(20).
