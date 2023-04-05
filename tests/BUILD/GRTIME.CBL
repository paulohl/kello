       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRTIME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  22/07/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de tempo (datas e horas)          *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MIN-YEAR                       PIC  9(004) VALUE 0.
           05 MAX-YEAR                       PIC  9(004) VALUE 9999.
           05 WS-DATE.
              10 WS-ANO                      PIC  9(004).
              10 WS-MES                      PIC  9(002).
                 88 WS-MES-OK                      VALUE 1 THRU 12.
              10 WS-DIA                      PIC  9(002).
           05 WS-DATE2.
              10 WS-DIA2                     PIC  9(002).
              10 WS-MES2                     PIC  9(002).
              10 WS-ANO2                     PIC  9(004).
           05 WS-TIME                        PIC  X(006) VALUE SPACES.
           05 WS                             PIC S9(002) VALUE 0.

           05 SEG-1                          PIC  9(009) VALUE ZEROS.
           05 SEG-2                          PIC  9(009) VALUE ZEROS.
           05 MM                             PIC  9(002) VALUE 0.
           05 SS                             PIC  9(009) VALUE 0.

           05 T-DMAX             VALUE "312831303130313130313031".
              10 DIA-MAX OCCURS 12           PIC  9(002).
           05 W-TP-ANO                       PIC  9(001).
              88 ANO-BISSEXTO                            VALUE  0.
           05 W-DIA                          PIC  9(007).
           05 W-NADA                         PIC  9(004).

           05 TABELA-DE-MESES.
              10 PIC  X(9) VALUE "Janeiro".
              10 PIC  X(9) VALUE "Fevereiro".
              10 PIC  X(9) VALUE "Marco".
              10 PIC  X(9) VALUE "Abril".
              10 PIC  X(9) VALUE "Maio".
              10 PIC  X(9) VALUE "Junho".
              10 PIC  X(9) VALUE "Julho".
              10 PIC  X(9) VALUE "Agosto".
              10 PIC  X(9) VALUE "Setembro".
              10 PIC  X(9) VALUE "Outubro".
              10 PIC  X(9) VALUE "Novembro".
              10 PIC  X(9) VALUE "Dezembro".
           05 REDEFINES TABELA-DE-MESES.
              10 NOME-MES OCCURS 12          PIC  X(009).
           05 WS-LONG.
              10 WS-DIA-LONG                 PIC  Z(002) VALUE ZEROS.
              10                             PIC  X(004) VALUE " de ".
              10 WS-MES-LONG                 PIC  X(009) VALUE SPACES.
              10                             PIC  X(004) VALUE " de ".
              10 WS-ANO-LONG                 PIC  9(004) VALUE ZEROS.
              10                             PIC  X(002) VALUE SPACES.
           05 REDEFINES WS-LONG.
              10 BYTE-LONG OCCURS 25         PIC  X(001).
           05 WS-LONG2                                   VALUE SPACES.
              10 BYTE-LONG2 OCCURS 25        PIC  X(001).
           05 LONG-I                         PIC  9(002) VALUE 0.
           05 LONG-Y                         PIC  9(002) VALUE 0.

           05 DIAS-X                  COMP-3 PIC  9(007) VALUE ZERO.
           05 DIAS-1                  COMP-3 PIC  9(007) VALUE ZERO.
           05 DIAS-2                  COMP-3 PIC  9(007) VALUE ZERO.
           05 LIXO                    COMP-3 PIC  9(007) VALUE ZERO.
           05 I                       COMP-3 PIC  9(004) VALUE ZERO.
           05 DATE-INICIAL                   PIC  9(008) VALUE ZERO.
           05 REDEFINES DATE-INICIAL.
              10 AA-I                        PIC  9(004).
              10 MMDD-I.
                 15 MM-I                     PIC  9(002).
                 15 DD-I                     PIC  9(002).
           05 DATE-FINAL                     PIC  9(008) VALUE ZERO.
           05 REDEFINES DATE-FINAL.
              10 AA-F                        PIC  9(004).
              10 MMDD-F.
                 15 MM-F                     PIC  9(002).
                 15 DD-F                     PIC  9(002).
           05 NUM-DIAS                       PIC  9(006).
           05 WS-SINAL                       PIC  X(001) VALUE "+".

           05 NDIA-WEEK                      PIC  9(001) VALUE ZERO.
           05 TAB-NM-WEEK.
              10 PIC X(007) VALUE "Sabado ".
              10 PIC X(007) VALUE "Domingo".
              10 PIC X(007) VALUE "Segunda".
              10 PIC X(007) VALUE "Terca  ".
              10 PIC X(007) VALUE "Quarta".
              10 PIC X(007) VALUE "Quinta".
              10 PIC X(007) VALUE "Sexta  ".
           05 REDEFINES TAB-NM-WEEK.
              10 NM-WEEK OCCURS 7 PIC X(007).

       LINKAGE SECTION.

       01  PARAMETROS-GRTIME.
           05 GRTIME-TYPE                    PIC  9(002).
              88 GRTIME-NORMAL                           VALUE 1.
              88 GRTIME-REVERSED                         VALUE 2.
           05 GRTIME-FUNCTION                PIC  9(002).
              88 GRTIME-ADD-DAYS                         VALUE 1.
              88 GRTIME-EDIT                             VALUE 2.
              88 GRTIME-INTERVAL                         VALUE 3.
              88 GRTIME-REVERSE                          VALUE 4.
              88 GRTIME-SUBTRACT-DAYS                    VALUE 5.
              88 GRTIME-TODAY                            VALUE 6.
              88 GRTIME-VALIDATE                         VALUE 7.
              88 GRTIME-WEEK                             VALUE 8.
              88 GRTIME-MIN-YEAR                         VALUE 9.
              88 GRTIME-MAX-YEAR                         VALUE 0.
           05 GRTIME-DATE                    PIC  9(008).
           05 GRTIME-TIME                    PIC  9(006).
           05 REDEFINES GRTIME-TIME.
              10 HH-1                        PIC  9(002).
              10 MM-1                        PIC  9(002).
              10 SS-1                        PIC  9(002).
           05 GRTIME-DAYS                    PIC  9(007).
           05 GRTIME-DATE-FINAL              PIC  9(008).
           05 GRTIME-TIME-FINAL              PIC  9(006).
           05 REDEFINES GRTIME-TIME-FINAL.
              10 HH-2                        PIC  9(002).
              10 MM-2                        PIC  9(002).
              10 SS-2                        PIC  9(002).
           05 GRTIME-DAYS-FINAL              PIC S9(007).
           05 GRTIME-TOTAL-TIME              PIC  9(012).
           05 GRTIME-TOTAL-HOURS             PIC  9(009).
           05 GRTIME-TOTAL-MINUTES           PIC  9(002).
           05 GRTIME-TOTAL-SECONDS           PIC  9(002).
           05 GRTIME-YEARS-OLD               PIC S9(004).
           05 GRTIME-MOUNTHS-OLD             PIC S9(002).
           05 GRTIME-DAYS-OLD                PIC S9(002).
           05 GRTIME-DATE-EDITED             PIC  X(010).
           05 GRTIME-DATE-EDITED-LONG        PIC  X(023).
           05 GRTIME-MOUNTH-EDITED           PIC  X(014).
           05 GRTIME-TIME-EDITED             PIC  X(008).
           05 GRTIME-WEEK-NUM                PIC  9(001).
           05 GRTIME-WEEK-CHAR               PIC  X(007).

       PROCEDURE DIVISION USING PARAMETROS-GRTIME.

       INICIO.

           IF   GRTIME-MIN-YEAR
                MOVE GRTIME-YEARS-OLD TO MIN-YEAR
                GOBACK
           ELSE
                IF   GRTIME-MAX-YEAR
                     MOVE GRTIME-YEARS-OLD TO MAX-YEAR
                     GOBACK
                END-IF
           END-IF

           IF   GRTIME-NORMAL
                MOVE GRTIME-DATE TO WS-DATE2
                MOVE WS-ANO2     TO WS-ANO
                MOVE WS-MES2     TO WS-MES
                MOVE WS-DIA2     TO WS-DIA
           ELSE
                MOVE GRTIME-DATE TO WS-DATE
           END-IF

           EVALUATE TRUE
               WHEN GRTIME-ADD-DAYS
                    PERFORM GRADAY THRU FIM-GRADAY
               WHEN GRTIME-EDIT
                    MOVE WS-DIA TO GRTIME-DATE-EDITED (1: 2)
                                   WS-DIA-LONG
                    MOVE "/"    TO GRTIME-DATE-EDITED (3: 1)
                    MOVE WS-MES TO GRTIME-DATE-EDITED (4: 2)
                    MOVE "/"    TO GRTIME-DATE-EDITED (6: 1)
                    MOVE WS-ANO TO GRTIME-DATE-EDITED (7: 4)
                                   WS-ANO-LONG
                    IF   WS-MES-OK
                         MOVE NOME-MES (WS-MES) TO WS-MES-LONG
                                                   GRTIME-MOUNTH-EDITED
                    ELSE
                         MOVE WS-MES            TO WS-MES-LONG
                                                   GRTIME-MOUNTH-EDITED
                    END-IF
                    MOVE 0      TO LONG-I
                                   LONG-Y
                    IF  WS-DIA < 10
                        ADD 1   TO LONG-I
                    END-IF
                    PERFORM UNTIL LONG-I > 22
                            ADD 1 TO LONG-I
                            IF  BYTE-LONG (LONG-I) = SPACE
                            AND BYTE-LONG (LONG-I + 1) = SPACE
                                CONTINUE
                            ELSE
                                ADD  1 TO LONG-Y
                                MOVE BYTE-LONG  (LONG-I)
                                  TO BYTE-LONG2 (LONG-Y)
                            END-IF
                    END-PERFORM
                    MOVE WS-LONG2             TO GRTIME-DATE-EDITED-LONG
                    MOVE GRTIME-MOUNTH-EDITED TO WS-LONG2
                    PERFORM VARYING LONG-I FROM 1 BY 1
                                    UNTIL BYTE-LONG2 (LONG-I) = SPACE
                            CONTINUE
                    END-PERFORM
                    MOVE "/"      TO WS-LONG2 (LONG-I: )
                    ADD  1        TO LONG-I
                    MOVE WS-ANO   TO WS-LONG2 (LONG-I: )
                    MOVE WS-LONG2 TO GRTIME-MOUNTH-EDITED
                    MOVE HH-1     TO GRTIME-TIME-EDITED (1: 2)
                    MOVE ":"      TO GRTIME-TIME-EDITED (3: 1)
                    MOVE MM-1     TO GRTIME-TIME-EDITED (4: 2)
                    MOVE ":"      TO GRTIME-TIME-EDITED (6: 1)
                    MOVE SS-1     TO GRTIME-TIME-EDITED (7: 2)
               WHEN GRTIME-INTERVAL
                    PERFORM GRDIAS THRU FIM-GRDIAS
                    COMPUTE GRTIME-TOTAL-HOURS = GRTIME-DAYS-FINAL * 24
                    COMPUTE SEG-1 = HH-1  * 3600
                    COMPUTE SEG-2 = HH-2  * 3600
                    COMPUTE SEG-1 = SEG-1 + (MM-1 * 60)
                    COMPUTE SEG-2 = SEG-2 + (MM-2 * 60)
                    COMPUTE SEG-1 = SEG-1 + SS-1
                    COMPUTE SEG-2 = SEG-2 + SS-2
                    IF   SEG-1 > SEG-2
                         SUBTRACT 24  FROM GRTIME-TOTAL-HOURS
                         ADD      86400 TO SEG-2
                    END-IF
                    COMPUTE SS = SEG-2 - SEG-1
                    MOVE 0 TO MM
                    PERFORM UNTIL SS < 60
                            SUBTRACT 60 FROM SS
                            ADD      1    TO MM
                            IF   MM = 60
                                 ADD  1 TO GRTIME-TOTAL-HOURS
                                 MOVE 0 TO MM
                            END-IF
                    END-PERFORM
                    MOVE MM TO GRTIME-TOTAL-MINUTES
                    MOVE SS TO GRTIME-TOTAL-SECONDS
                    COMPUTE GRTIME-TOTAL-TIME =
                            (GRTIME-TOTAL-HOURS * 3600) +
                            (MM                 * 60)    +
                            SS
               WHEN GRTIME-REVERSE
                    IF   GRTIME-NORMAL
                         MOVE WS-DATE     TO GRTIME-DATE-FINAL
                    ELSE
                         MOVE GRTIME-DATE TO WS-DATE
                         MOVE WS-ANO      TO WS-ANO2
                         MOVE WS-MES      TO WS-MES2
                         MOVE WS-DIA      TO WS-DIA2
                         MOVE WS-DATE2    TO GRTIME-DATE-FINAL
                    END-IF
               WHEN GRTIME-SUBTRACT-DAYS
                    PERFORM GRSDAY THRU FIM-GRSDAY
               WHEN GRTIME-TODAY
                    ACCEPT GRTIME-DATE-FINAL  FROM DATE
                    EVALUATE TRUE
                             WHEN GRTIME-DATE-FINAL < 00990000
                                  ADD 20000000  TO GRTIME-DATE-FINAL
                             WHEN GRTIME-DATE-FINAL < 00999999
                                  ADD 19000000  TO GRTIME-DATE-FINAL
                    END-EVALUATE
                    ACCEPT WS-TIME            FROM TIME
                    MOVE   WS-TIME              TO GRTIME-TIME-FINAL
                    IF   GRTIME-NORMAL
                         MOVE GRTIME-DATE-FINAL TO WS-DATE
                         MOVE WS-ANO            TO WS-ANO2
                         MOVE WS-MES            TO WS-MES2
                         MOVE WS-DIA            TO WS-DIA2
                         MOVE WS-DATE2          TO GRTIME-DATE-FINAL
                    END-IF
               WHEN GRTIME-VALIDATE
                    DIVIDE WS-ANO BY 4 GIVING W-NADA REMAINDER W-TP-ANO
                    IF   ANO-BISSEXTO
                         MOVE 29 TO DIA-MAX (2)
                    ELSE
                         MOVE 28 TO DIA-MAX (2)
                    END-IF
                    IF   WS-DATE NOT NUMERIC
                    OR   WS-MES EQUAL ZEROS
                    OR   WS-MES GREATER 12
                    OR   WS-DIA EQUAL ZEROS
                    OR   WS-DIA GREATER DIA-MAX (WS-MES)
                    OR   WS-ANO LESS    MIN-YEAR
                    OR   WS-ANO GREATER MAX-YEAR
                         MOVE ZEROS   TO GRTIME-DATE-FINAL
                    ELSE
                         IF   GRTIME-NORMAL
                              MOVE WS-ANO   TO WS-ANO2
                              MOVE WS-MES   TO WS-MES2
                              MOVE WS-DIA   TO WS-DIA2
                              MOVE WS-DATE2 TO GRTIME-DATE-FINAL
                         ELSE
                              MOVE WS-DATE  TO GRTIME-DATE-FINAL
                         END-IF
                    END-IF
               WHEN GRTIME-WEEK
                    MOVE    WS-DATE           TO GRTIME-DATE-FINAL
                    MOVE    "00000101"        TO WS-DATE
                    PERFORM GRDIAS          THRU FIM-GRDIAS
                    DIVIDE GRTIME-DAYS-FINAL BY 7 GIVING LIXO
                                   REMAINDER NDIA-WEEK
                    IF   NDIA-WEEK = 0
                         MOVE 7 TO NDIA-WEEK
                    END-IF
                    MOVE    NM-WEEK (NDIA-WEEK) TO GRTIME-WEEK-CHAR
                    COMPUTE GRTIME-WEEK-NUM = NDIA-WEEK - 1
           END-EVALUATE.

       FIM. GOBACK.

       GRADAY.

           COMPUTE W-DIA = WS-DIA + GRTIME-DAYS.

       GRADAY-CHECK-ANO.

           DIVIDE WS-ANO BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF.

       GRADAY-MAIS-DIA.

           IF   W-DIA GREATER DIA-MAX (WS-MES)
                SUBTRACT DIA-MAX (WS-MES) FROM W-DIA
                ADD      1                  TO WS-MES
                IF   WS-MES EQUAL 13
                     MOVE 1 TO WS-MES
                     ADD  1 TO WS-ANO
                     GO     TO GRADAY-CHECK-ANO
                ELSE
                     GO     TO GRADAY-MAIS-DIA
                END-IF
           ELSE
                MOVE W-DIA TO WS-DIA
           END-IF

           IF   GRTIME-NORMAL
                MOVE WS-ANO     TO WS-ANO2
                MOVE WS-MES     TO WS-MES2
                MOVE WS-DIA     TO WS-DIA2
                MOVE WS-DATE2   TO GRTIME-DATE-FINAL
           ELSE
                MOVE WS-DATE    TO GRTIME-DATE-FINAL
           END-IF.

       FIM-GRADAY. EXIT.

       GRSDAY.

           IF   WS-DIA > GRTIME-DAYS
                COMPUTE W-DIA = WS-DIA - GRTIME-DAYS
                MOVE W-DIA TO WS-DIA
                GO TO GRSDAY-CHECK
           END-IF

           COMPUTE W-DIA = GRTIME-DAYS - WS-DIA.

       GRSDAY-CHECK-MES-ANO.

           COMPUTE WS-MES = WS-MES - 1.
           IF   WS-MES = ZERO
                MOVE 12 TO WS-MES
                SUBTRACT 1 FROM WS-ANO
           END-IF.

       GRSDAY-CHECK-ANO.

           DIVIDE WS-ANO BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO.

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF.

       GRSDAY-MENOS-DIA.

           IF   W-DIA GREATER DIA-MAX (WS-MES)
                COMPUTE W-DIA = W-DIA - DIA-MAX (WS-MES)
                GO     TO GRSDAY-CHECK-MES-ANO
           END-IF

           COMPUTE WS-DIA = DIA-MAX (WS-MES) - W-DIA.

       GRSDAY-CHECK.

           IF   GRTIME-NORMAL
                MOVE WS-ANO     TO WS-ANO2
                MOVE WS-MES     TO WS-MES2
                MOVE WS-DIA     TO WS-DIA2
                MOVE WS-DATE2   TO GRTIME-DATE-FINAL
           ELSE
                MOVE WS-DATE    TO GRTIME-DATE-FINAL
           END-IF.

       FIM-GRSDAY. EXIT.

       GRDIAS.

           MOVE WS-DATE TO DATE-INICIAL

           IF   NOT GRTIME-WEEK
           AND  GRTIME-NORMAL
                MOVE GRTIME-DATE-FINAL TO WS-DATE2
                MOVE WS-ANO2           TO WS-ANO
                MOVE WS-MES2           TO WS-MES
                MOVE WS-DIA2           TO WS-DIA
                MOVE WS-DATE           TO DATE-FINAL
           ELSE
                MOVE GRTIME-DATE-FINAL TO DATE-FINAL
           END-IF

           IF   DATE-INICIAL = DATE-FINAL
           OR   DATE-INICIAL = ZERO
           OR   DATE-FINAL   = ZERO
                MOVE ZERO TO GRTIME-DAYS-FINAL
                GO TO FIM-GRDIAS
           END-IF

           IF   DATE-FINAL < DATE-INICIAL
                MOVE DATE-FINAL   TO WS-DATE2
                MOVE DATE-INICIAL TO DATE-FINAL
                MOVE WS-DATE2     TO DATE-INICIAL
                MOVE "-"          TO WS-SINAL
           ELSE
                MOVE "+"          TO WS-SINAL
           END-IF

           DIVIDE AA-I BY 4 GIVING LIXO REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF

           MOVE ZERO TO DIAS-1

           COMPUTE DIAS-1 = ((AA-I - 1) / 4) + ((AA-I - 1) * 365) + 366

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-I
                   ADD DIA-MAX (I) TO DIAS-1
           END-PERFORM

           ADD    DD-I          TO DIAS-1
           DIVIDE AA-F BY 4 GIVING LIXO REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF

           COMPUTE DIAS-2 = ((AA-F - 1) / 4) + ((AA-F - 1) * 365) + 366

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-F
                    ADD DIA-MAX (I) TO DIAS-2
           END-PERFORM

           ADD DD-F TO DIAS-2

           COMPUTE GRTIME-DAYS-FINAL = DIAS-2 - DIAS-1

           COMPUTE GRTIME-YEARS-OLD = AA-F - AA-I
           EVALUATE TRUE
               WHEN MMDD-F = MMDD-I
                    MOVE 0 TO GRTIME-MOUNTHS-OLD
                              GRTIME-DAYS-OLD
               WHEN MMDD-F < MMDD-I
                    IF   GRTIME-YEARS-OLD NOT = 0
                         SUBTRACT 1 FROM GRTIME-YEARS-OLD
                    END-IF
                    COMPUTE GRTIME-MOUNTHS-OLD = 12 - MM-I + MM-F
                    EVALUATE TRUE
                        WHEN DD-F = DD-I
                             MOVE 0 TO GRTIME-DAYS-OLD
                        WHEN DD-F > DD-I
                             COMPUTE GRTIME-DAYS-OLD = DD-F - DD-I
                        WHEN DD-F < DD-I
                             SUBTRACT 1 FROM GRTIME-MOUNTHS-OLD
                             COMPUTE WS = DD-F - DD-I
                             COMPUTE GRTIME-DAYS-OLD =
                                      DIA-MAX (MM-F) + WS
                    END-EVALUATE
               WHEN MMDD-F > MMDD-I
                    COMPUTE GRTIME-MOUNTHS-OLD = MM-F - MM-I
                    EVALUATE TRUE
                        WHEN DD-F = DD-I
                             MOVE 0 TO GRTIME-DAYS-OLD
                        WHEN DD-F > DD-I
                             COMPUTE GRTIME-DAYS-OLD = DD-F - DD-I
                        WHEN DD-F < DD-I
                             SUBTRACT 1 FROM GRTIME-MOUNTHS-OLD
                             COMPUTE WS = DD-F - DD-I
                             COMPUTE GRTIME-DAYS-OLD =
                                      DIA-MAX (MM-F) + WS - 1
                    END-EVALUATE
           END-EVALUATE

           IF   WS-SINAL = "-"
                COMPUTE GRTIME-DAYS-FINAL  = GRTIME-DAYS-FINAL  * -1
                COMPUTE GRTIME-YEARS-OLD   = GRTIME-YEARS-OLD   * -1
                COMPUTE GRTIME-MOUNTHS-OLD = GRTIME-MOUNTHS-OLD * -1
                COMPUTE GRTIME-DAYS-OLD    = GRTIME-DAYS-OLD    * -1
           END-IF.

       FIM-GRDIAS. EXIT.
       END PROGRAM GRTIME.
