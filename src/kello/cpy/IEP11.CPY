           SELECT IED011 ASSIGN TO PATH-IED011
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-IED011
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-IE11
                  ALTERNATE RECORD KEY IS NOME-IE11.
      *  Cadastro de Cursos
       FD  IED011.
       01  REG-IED011.
           05  CODIGO-IE11              PIC 999.
           05  NOME-IE11                PIC X(40).
           05  NOME-REDUZ-IE11          PIC X(12).
