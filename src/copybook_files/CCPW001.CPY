      * cadastro de usuários que poderão ter acesso aos relatórios de
      * contas correntes conforme tipo de acesso
       FD  CCD001.
       01  REG-CCD001.
           05  NOME-REDUZ-CC01     PIC X(5).
           05  TIPO-ACESSO-CC01    PIC 9(3).
      * TIPO-ACESSO É FIXO COM: 01 - Pessoa Física
      *                         02 - Pessoa Jurídica
      *                         03 - Funcionário
      *                         04 - Representante
      *                         05 - Fotógrafo
      *                         06 - Cinegrafista
      *                         07 - Vendedor
      *                         08 - Imposto
      *                         09 - Investidor
      *                         00 - Outros
      *                         10 - Terceirizado
      *                         11 - Franquia

