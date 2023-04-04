       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRIDAT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Inverte DATA de DDMMAA para AAMMDD          *
                      *                ou AAMMDD para DDMMAA          *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  WS                    PIC  X(002) VALUE ZERO.

       LINKAGE SECTION.

       01  INVERTE               PIC  X(006).

       PROCEDURE DIVISION USING INVERTE.

           MOVE INVERTE (5: 2) TO WS
           MOVE INVERTE (1: 2) TO INVERTE (5: 2)
           MOVE WS             TO INVERTE (1: 2)
           EXIT PROGRAM.

       END PROGRAM GRIDAT.
