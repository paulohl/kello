           SELECT COD042 ASSIGN TO PATH-COD042
                  ORGANIZATION INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS SEQ-CO42
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  ALTERNATE RECORD KEY IS IDENTIFICACAO-CO42
                             WITH DUPLICATES
                  STATUS IS ST-COD042.
       FD  COD042.
       01  REG-COD042.
           05  SEQ-CO42            PIC 9(3).
           05  IDENTIFICACAO-CO42  PIC X(12).
           05  INSTITUICAO-CO42    PIC X(15).
           05  CIDADE-CO42         PIC X(13).
           05  QT-FORM-CO42        PIC 9(4).
           05  PADRAO-CO42         PIC X.
           05  QT-FOTOS-CO42       PIC 9(6).
           05  MESANO-CO42         PIC 9(6).
           05  ASSINATURA-CO42     PIC 9(8).
           05  VLR-COMISSAO-CO42   PIC 9(8)V99.
           05  REPRESENTANTE-CO42  PIC X(15).
