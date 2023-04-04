       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO30.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 10/12/2004.
      *FUNÇÃO: teste de arquivos .txt impressão compactada

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RELAT ASSIGN TO "C:\PLANEJ.TXT"
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-RELAT.


           SELECT IMPRESSORA ASSIGN TO
           PRINTER.


       DATA DIVISION.
       FILE SECTION.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(140).

       FD  IMPRESSORA
           LABEL RECORD IS OMITTED.
       01  REG-IMPRE.
           05  FILLER          PIC X(140).



       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  FS-RELAT              PIC X(02)    VALUE SPACES.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  CONTADOR              PIC 9(09)    VALUE ZEROS.

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".

           COPY "CBPRINT.CPY".

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.

           OPEN INPUT RELAT

           OPEN OUTPUT IMPRESSORA

           DISPLAY COND-HP UPON LPRINTER.

       READ-RELAT.

           READ RELAT NEXT AT END
               MOVE SPACES TO REG-IMPRE
               WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
               CLOSE RELAT IMPRESSORA
               DISPLAY DESCOND-HP UPON LPRINTER
               EXIT PROGRAM
               STOP RUN.

           IF FS-RELAT <> "00"
               CLOSE RELAT IMPRESSORA
               DISPLAY DESCOND-HP UPON LPRINTER
               EXIT PROGRAM
               STOP RUN
           ELSE
              IF REG-RELAT = "SALTAR PAGINA"
                 READ RELAT NEXT AT END
                      MOVE SPACES TO REG-IMPRE
                      WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                      CLOSE RELAT IMPRESSORA
                      DISPLAY DESCOND-HP UPON LPRINTER
                      EXIT PROGRAM
                      STOP RUN
                 END-READ
                 IF FS-RELAT <> "00"
                    CLOSE RELAT IMPRESSORA
                    DISPLAY DESCOND-HP UPON LPRINTER
                    EXIT PROGRAM
                    STOP RUN
                 ELSE
                    IF REG-RELAT = "INICIO"
                       READ RELAT NEXT AT END
                            MOVE SPACES TO REG-IMPRE
                            WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                            CLOSE RELAT IMPRESSORA
                            DISPLAY DESCOND-HP UPON LPRINTER
                            EXIT PROGRAM
                            STOP RUN
                       END-READ
                       IF FS-RELAT <> "00"
                          MOVE SPACES TO REG-IMPRE
                          WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                          CLOSE RELAT IMPRESSORA
                          DISPLAY DESCOND-HP UPON LPRINTER
                          EXIT PROGRAM
                          STOP RUN
                       ELSE
                          WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                       END-IF
                    ELSE
                       WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                    END-IF
                 END-IF
              ELSE
                 IF REG-RELAT = "INICIO"
                    READ RELAT NEXT AT END
                        MOVE SPACES TO REG-IMPRE
                        WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                        CLOSE RELAT IMPRESSORA
                        DISPLAY DESCOND-HP UPON LPRINTER
                        EXIT PROGRAM
                        STOP RUN
                    END-READ
                    IF FS-RELAT <> "00"
                       MOVE SPACES TO REG-IMPRE
                       WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                       CLOSE RELAT IMPRESSORA
                       DISPLAY DESCOND-HP UPON LPRINTER
                       EXIT PROGRAM
                       STOP RUN
                    ELSE
                       WRITE REG-IMPRE FROM REG-RELAT AFTER 0
                    END-IF
                 ELSE
                    WRITE REG-IMPRE FROM REG-RELAT.

           GO TO READ-RELAT.

