       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRIDAT1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Inverte DATA de
                      *                 AAAAMMDD para DDMMAAAA        *
                      *                                               *
                      *                                               *
                      *                                               *
                      *  CALL "GRIDAT" USING DDMMAA   ou              *
                      *                                               *
                      *  CALL "GRIDAT" USING AAMMDD                   *
                      *                                               *
                      *                                               *
                      *                                               *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  WS                    PIC  X(004) VALUE ZERO.

       LINKAGE SECTION.

       01  INVERTE               PIC  X(008).

       PROCEDURE DIVISION USING INVERTE.

           MOVE INVERTE (1: 4) TO WS
           MOVE INVERTE (7: 2) TO INVERTE (1: 2)
           MOVE INVERTE (5: 2) TO INVERTE (3: 2)
           MOVE WS             TO INVERTE (5: 4)
           EXIT PROGRAM.

       END PROGRAM GRIDAT1.
