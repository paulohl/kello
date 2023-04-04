       01 DET-CABECALHO1-p.
          05 FILLER                      PIC X(34).
          05 DET-EMPRESA-p               PIC X(04).
          05 FILLER                      PIC X(05).
          05 DET-RAZAO-p                 PIC X(76).
          05 FILLER                      PIC X(38).
          05 FILLER                      PIC X(07) VALUE "PÁGINA ".
          05 DET-PAGINA-p                PIC 9(06) VALUE ZEROS.

       01 DET-CABECALHO2-p.
          05 FILLER                      PIC X(34).
          05 DET-TITULO-CAB-p            PIC X(108).
          05 FILLER                      PIC X(12) VALUE "EMITIDO EM:".
          05 DET-DIA-EMIS-p              PIC 99/.
          05 DET-MES-EMIS-p              PIC 99/.
          05 DET-ANO-EMIS-p              PIC 9999.
          05 FILLER                      PIC X(01).
          05 DET-HO-EMIS-p               PIC 99.
          05 FILLER                      PIC X(01) VALUE ":".
          05 DET-MI-EMIS-p               PIC 99.

       01 DET-CABECALHO3-p.
          05 FILLER                      PIC X(142).
          05 FILLER                      PIC X(12) VALUE "USUÁRIO...:".
          05 DET-USUARIO-P               PIC X(05).

