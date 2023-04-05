
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       78  dialog-system               VALUE "DSGRUN".
       01 Display-Error-No             PIC 9(4).
       01 Display-Details-1            PIC 9(4).
       01 Display-Details-2            PIC 9(4).

       COPY "DS-CNTRL.MF".
       COPY "mensagem.CPB".

       LINKAGE SECTION.
       01 TIPO-MSG-LNK                 PIC X(001).
       01 RESP-MSG-LNK                 PIC X(001).
       01 MENSAGEM-LNK                 PIC X(200).

       PROCEDURE DIVISION USING TIPO-MSG-LNK RESP-MSG-LNK MENSAGEM-LNK.

      *---------------------------------------------------------------*

       Main-Process SECTION.
          PERFORM Program-Initialize
          PERFORM Program-Body UNTIL EXIT-FLAG-TRUE
          PERFORM Program-Terminate.

      *---------------------------------------------------------------*

       Program-Initialize SECTION.

          INITIALIZE Ds-Control-Block
          INITIALIZE Data-block
          MOVE Data-block-version-no
                                   TO Ds-Data-Block-Version-No
          MOVE Version-no          TO Ds-Version-No

          MOVE Ds-Push-Set         TO Ds-Control
          MOVE "mensagem"          TO Ds-Set-Name.
          MOVE TIPO-MSG-LNK        TO TIPO-MSG.
          MOVE RESP-MSG-LNK        TO RESP-MSG.
          MOVE MENSAGEM-LNK        TO MENSAGEM.

      *---------------------------------------------------------------*

       Program-Body SECTION.

          PERFORM Call-Dialog-System.
          MOVE RESP-MSG TO RESP-MSG-LNK.

      *---------------------------------------------------------------*

       Program-Terminate SECTION.
          MOVE DS-QUIT-SET TO DS-CONTROL.
          PERFORM Call-Dialog-System
          EXIT PROGRAM
          STOP RUN.

      *---------------------------------------------------------------*

       Call-Dialog-System SECTION.

          CALL dialog-system USING Ds-Control-Block,
                                   Data-Block
          IF NOT Ds-No-Error
              MOVE Ds-Error-Code TO Display-error-no
              DISPLAY "DS ERROR NO:   "  Display-error-no
              DISPLAY "Error Details(1) :   "  Display-Details-1
              DISPLAY "Error Details(2) :   "  Display-Details-2
              PERFORM Program-Terminate
          END-IF.
