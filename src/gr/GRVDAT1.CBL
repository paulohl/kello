       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRVDAT1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/89.
       SECURITY.      *************************************************
                      *                                               *
                      *   Verifica validade de uma data               *
                      *                                               *
                      *   Formato: CALL "GRVDAT" USING DDMMAA         *
                      *                                               *
                      *   Se a data invalida DDMMAA sera zerada       *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 FILLER VALUE "31??31303130313130313031".
              10 FIM-MES OCCURS 12 PIC  9(002).
           05 POR-4                PIC  9(004).
           05 RESTO                PIC  9(004)V9(2).

       LINKAGE SECTION.

       01  DDMMAA.
           05 DD                   PIC 9(02).
           05 MM                   PIC 9(02).
           05 AA                   PIC 9(04).

       PROCEDURE DIVISION USING DDMMAA.

       000-INICIO.

           DIVIDE AA BY 4 GIVING POR-4 REMAINDER RESTO

           IF   RESTO EQUAL ZERO
                MOVE 29 TO FIM-MES (2)
           ELSE
                MOVE 28 TO FIM-MES (2).

           IF   ((DDMMAA NOT NUMERIC)
           OR   (MM EQUAL ZEROS) OR (MM GREATER 12)
           OR   (DD EQUAL ZEROS) OR (DD GREATER FIM-MES (MM)))
                MOVE ZEROS TO DDMMAA.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM GRVDAT1.
