       01 DET-CABECALHO1.
          05 FILLER                    PIC X(33).
          05 DET-EMPRESA               PIC X(04) VALUE SPACES.
          05 FILLER                    PIC X(05) VALUE SPACES.
          05 DET-RAZAO                 PIC X(65) VALUE SPACES.
          05 FILLER                    PIC X(05) VALUE SPACES.
          05 FILLER                    PIC X(07) VALUE "PÁGINA ".
          05 DET-PAGINA                PIC 9(06) VALUE ZEROS.

       01 DET-CABECALHO2.
          05 FILLER                    PIC X(33).
          05 DET-TITULO-CAB            PIC X(62) VALUE SPACES.
          05 FILLER                    PIC X(02) VALUE SPACES.
          05 FILLER                    PIC X(12) VALUE "EMITIDO EM:".
          05 DET-DIA-EMIS              PIC 99/.
          05 DET-MES-EMIS              PIC 99/.
          05 DET-ANO-EMIS              PIC 9999.
          05 FILLER                    PIC X(01) VALUE SPACES.
          05 DET-HO-EMIS               PIC 99 VALUE ZEROS.
          05 FILLER                    PIC X(01) VALUE ":".
          05 DET-MI-EMIS               PIC 99 VALUE ZEROS.

       01 DET-CABECALHO3.
          05 FILLER                    PIC X(098) VALUE SPACES.
          05 FILLER                    PIC X(012)
                                                 VALUE "USUÁRIO...:".
          05 DET-USUARIO               PIC X(05).
