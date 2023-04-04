       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRIDAT2.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Inverte DATA de DDMMAADD para AAAAMMDD      *
                      *                                               *
                      *  CALL "GRIDAT2" USING DDMMAAAA                *
                      *                                               *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  WS                    PIC  X(004) VALUE ZERO.

       LINKAGE SECTION.

       01  INVERTE               PIC  X(008).

       PROCEDURE DIVISION USING INVERTE.

           MOVE INVERTE (5: 4) TO WS
           MOVE INVERTE (3: 2) TO INVERTE (5: 2)
           MOVE INVERTE (1: 2) TO INVERTE (7: 2)
           MOVE WS             TO INVERTE (1: 4)
           EXIT PROGRAM.

       END PROGRAM GRIDAT2.
