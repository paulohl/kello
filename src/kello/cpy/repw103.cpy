       FD  RED103.
       01  REG-RED103.
           05  DOCTO-R103           PIC 9(6).
           05  SEQ-R103             PIC 9(3).
           05  CODIGO-R103          PIC 9(6).
           05  FUNCAO-R103          PIC 9(2).
           05  FITA-FILME-R103      PIC 9.
      *    BOOLEAN - CONTROLA 0-NENHUM  1-FILME  2-FITA   3-KM
           05  TIPO-REPORT-R103     PIC 9.
      *    BOOLENA - 1-REPORT   2-DESPESA
           05  QT-FILMES-R103       PIC 999.
      *    PODE SER QTDE DE FILMES OU FITAS OU KILOMETROS DEPENDE
      *    DO TIPO DO CAMPO FITA-FILME-R103
      *    IDENTIFICA QTDE PRODUZIDA
           05  QT-REPORT-R103       PIC 9V9.
           05  VLR-REPORT-R103      PIC 9(8)V99.
