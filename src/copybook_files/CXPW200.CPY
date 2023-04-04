      *  ARQUIVO DE ANOTAÇÃO DO CAIXA
       FD  CXD200.
       01  REG-CXD200.
           05  DATA-OCORRENCIA-CX200   PIC 9(8).
           05  SEQ-CX200               PIC 9(3).
           05  HORA-OCORRENCIA-CX200   PIC X(5).
           05  USUARIO-CX200           PIC X(5).
           05  SITUACAO-ANOTACAO-CX200 PIC 9.
      *    0-PENDENTE  1-CHECADA
           05  DESCRICAO-CX200         PIC X(100).
