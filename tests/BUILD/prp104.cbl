       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRP104.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 10/12/2004.
      *FUNÇÃO: Impressão do prp101.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RELAT ASSIGN TO ARQUIVO-IMPRESSAO
                        ORGANIZATION IS LINE SEQUENTIAL
                        FILE STATUS IS FS-RELAT.


           SELECT IMPRESSORA ASSIGN TO PRINTER NOME-IMPRESSORA.


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
           COPY IMPRESSORA.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  FS-RELAT              PIC X(02)    VALUE SPACES.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           COPY "PARAMETR".

           COPY "CBPRINT.CPY".

       LINKAGE SECTION.

       01 ARQUIVO-IMPRESSAO                PIC X(50).
       01 COMPACTA                         PIC X(01).
       01 TIPO-IMPRESSORA                  PIC 9(02).


       PROCEDURE DIVISION USING PARAMETROS-W ARQUIVO-IMPRESSAO COMPACTA
                                TIPO-IMPRESSORA.
       MAIN-PROCESS SECTION.
           COPY IMPRESSORA.CHAMA.

           OPEN OUTPUT IMPRESSORA

           IF COMPACTA = "S"
              copy condensa.

           OPEN INPUT RELAT.


       READ-RELAT.

           READ RELAT NEXT AT END
               MOVE SPACES TO REG-IMPRE
               WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
               CLOSE RELAT
               IF COMPACTA = "S"
                  IF LNK-TIPO = 1
                     WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
                  ELSE
                     WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
                  END-IF
               END-IF
               CLOSE IMPRESSORA
               EXIT PROGRAM
               STOP RUN.

           IF FS-RELAT <> "00"
               CLOSE RELAT
               IF COMPACTA = "S"
                  IF LNK-TIPO = 1
                     WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
                  ELSE
                     WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
                  END-IF
               END-IF
               CLOSE IMPRESSORA
               EXIT PROGRAM
               STOP RUN
           ELSE
              IF REG-RELAT = "SALTAR PAGINA"
                 READ RELAT NEXT AT END
                      MOVE SPACES TO REG-IMPRE
                      WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                      CLOSE RELAT
                      IF COMPACTA = "S"
                         IF LNK-TIPO = 1
                            WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
                         ELSE
                            WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
                         END-IF
                      END-IF
                      CLOSE IMPRESSORA
                      EXIT PROGRAM
                      STOP RUN
                 END-READ
                 IF FS-RELAT <> "00"
                    CLOSE RELAT
                    IF COMPACTA = "S"
                       IF LNK-TIPO = 1
                          WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
                       ELSE
                          WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
                       END-IF
                    END-IF
                    CLOSE IMPRESSORA
                    EXIT PROGRAM
                    STOP RUN
                 ELSE
                    IF REG-RELAT = "INICIO"
                       READ RELAT NEXT AT END
                            MOVE SPACES TO REG-IMPRE
                            WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                            CLOSE RELAT
                            IF COMPACTA = "S"
                               IF LNK-TIPO = 1
                                  WRITE REG-RELAT FROM DESCOND-HP
                                                             BEFORE PAGE
                               ELSE
                                  WRITE REG-RELAT FROM DESCOND-EP
                                                             BEFORE PAGE
                               END-IF
                            END-IF
                            CLOSE IMPRESSORA
                            EXIT PROGRAM
                            STOP RUN
                       END-READ
                       IF FS-RELAT <> "00"
                          MOVE SPACES TO REG-IMPRE
                          WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                          CLOSE RELAT
                          IF COMPACTA = "S"
                             IF LNK-TIPO = 1
                                WRITE REG-RELAT FROM DESCOND-HP
                                                             BEFORE PAGE
                             ELSE
                                WRITE REG-RELAT FROM DESCOND-EP
                                                             BEFORE PAGE
                             END-IF
                          END-IF
                          CLOSE IMPRESSORA
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
                        CLOSE RELAT
                        IF COMPACTA = "S"
                           IF LNK-TIPO = 1
                              WRITE REG-RELAT FROM DESCOND-HP
                                                             BEFORE PAGE
                           ELSE
                              WRITE REG-RELAT FROM DESCOND-EP
                                                             BEFORE PAGE
                           END-IF
                        END-IF
                        CLOSE IMPRESSORA
                        EXIT PROGRAM
                        STOP RUN
                    END-READ
                    IF FS-RELAT <> "00"
                       MOVE SPACES TO REG-IMPRE
                       WRITE REG-IMPRE FROM REG-RELAT AFTER PAGE
                       CLOSE RELAT
                       IF COMPACTA = "S"
                          IF LNK-TIPO = 01
                              WRITE REG-RELAT FROM DESCOND-HP
                                                             BEFORE PAGE
                          ELSE
                              WRITE REG-RELAT FROM DESCOND-EP
                                                             BEFORE PAGE
                          END-IF
                       END-IF
                       CLOSE IMPRESSORA
                       EXIT PROGRAM
                       STOP RUN
                    ELSE
                       WRITE REG-IMPRE FROM REG-RELAT AFTER 0
                    END-IF
                 ELSE
                    WRITE REG-IMPRE FROM REG-RELAT.

           GO TO READ-RELAT.

