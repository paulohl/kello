      *  Cadastro geral de fornecedores - arquivo simples
      *  situacao-cg01 => 2 -> inativo senão ativo
      *  sexo-cg01 => M - Masculino, F - Feminino
       FD  CGD001.
       01  REG-CGD001.
           05  CODIGO-CG01         PIC 9(6).
           05  NOME-CG01           PIC X(30).
           05  NOME-RED-CG01       PIC X(13).
           05  SEXO-CG01           PIC X(01).
           05  SITUACAO-CG01       PIC 9(01).
           05  T-PESFIS-CG01       PIC 9.
           05  T-PESJUR-CG01       PIC 9.
           05  T-FUNC-CG01         PIC 9.
           05  T-REPRES-CG01       PIC 9.
           05  T-FOTOG-CG01        PIC 9.
           05  T-CINEG-CG01        PIC 9.
           05  T-VEND-CG01         PIC 9.
           05  T-IMPOSTO-CG01      PIC 9.
           05  T-INVESTIDOR-CG01   PIC 9.
           05  OUTRO3-CG01         PIC 9.
           05  T-TERCEIRIZADO-CG01 PIC 9.
           05  T-FRANQUIA-CG01     PIC 9.
           05  COD-RED-CG01        PIC 9(05).
           05  FILLER              PIC X(45).
