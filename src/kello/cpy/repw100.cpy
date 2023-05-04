      *CADASTRO DE REPORTAGEM  - ARQUIVO PRINCIPAL
       FD  RED100.
       01  REG-RED100.
           05  DOCTO-R100           PIC 9(6).
           05  DATA-MOV-R100        PIC 9(8)         COMP-3.
           05  ANOMES-R100          PIC 9(6)         COMP-3.
      *    ANOMES-R100 P/ PAGAMENTO DA REPORTAGEM
           05  LCTO-CTA-CORR-R100   PIC 9.
      *    LCTO-CTA-CORR-R100 = 0-NÃO   1-LCDO NO CTA CORRENTE
           05  QTDE-PESSOAS-R100    PIC 99.
           05  QTDE-VEICULOS-R100   PIC 99.
           05  QTDE-DIAS-R100       PIC 99V99.
           05  QTDE-FORM-R100       PIC 9999.
           05  VLR-COMB-R100        PIC 9(8)V99      COMP-3.
           05  VLR-HOSP-R100        PIC 9(8)V99      COMP-3.
           05  VLR-REFEICAO-R100    PIC 9(8)V99      COMP-3.
           05  VLR-PASSAGEM-R100    PIC 9(8)V99      COMP-3.
           05  VLR-ALUGUEL-R100     PIC 9(8)V99      COMP-3.
           05  VLR-MAT-R100         PIC 9(8)V99      COMP-3.
           05  VLR-OUTROS-R100      PIC 9(8)V99      COMP-3.
      *    DADOS DA DIGITAÇÃO DE REPORTAGEM
           05  VLR-TOT-REPORT-R100  PIC 9(8)V99      COMP-3.
           05  VLR-DESPESA-REPORT-R100 PIC 9(8)V99   COMP-3.
      *    FUNCAO = 12(LOCACAO)  15-EQUIPAMENTO CONSIDERADO VLR-DESPESA
           05  TOT-FILME-REPORT-R100 PIC 9(3).
           05  TOT-FITA-REPORT-R100  PIC 99.
