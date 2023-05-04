      *   Cadastro de Tipo de Atendimento
       FD  ACD010.
       01  REG-ACD010.
           05  TIPO-AC10           PIC 9(03).
           05  CODIGO-AC10         PIC 9(03).
           05  DESCRICAO-AC10      PIC X(60).
           05  ASSUNTO-AC10        PIC 9(01).
           05  QTDE-DIAS-AC10      PIC 9(03).
           05  FILLER              PIC X(50).

      * Assunto-ac10 = 1 - MIN
      *                2 - DCR
      *                3 - O.S.
      *                4 - DPT
      *                5 - MEMO

