
      * This file contains generic Performed routines used by Win95
      * Common control code templates

       Fold-Func-Routine section.

      *     Convert the Call-Function to Uppercase

            MOVE LENGTH OF Call-Function TO Function-Length
            CALL "CBL_TOUPPER" USING Call-Function
                            BY VALUE Function-Length.


       GetWindow-Instance-Routine section.

      *   Use the Correct handle Storage format for getting
      *   the object reference... This is not a new window, just
      *   obtaining an object reference for an existing window

          INVOKE Window "fromHandle" USING aHwnd
                                     RETURNING aWindow.

          .


       Resize-Routine Section.

      *   The EntryWindow-Data flag is used in this context, as a value to 
      *   indicate whether to get or set the rectangle details. 
      *   0 = Get/ 1 = Set

          IF EntryWindow-Data = 0

      *      The Dialog System Runtime needs to know the current
      *      coordinates of the object instance so it can apply an adjustment
      *      factor to the values that need to change.
             INVOKE EntryObject-Reference "getRectangle"
                                  USING EntryPosition-X EntryPosition-Y
                                        EntrySize-Width EntrySize-Height

          ELSE

      *      Invoke the setRectangle method of the object instance to
      *      reset the physical sizes following a window-sized event
             INVOKE EntryObject-Reference "setRectangle"
                                  USING EntryPosition-X EntryPosition-Y
                                        EntrySize-Width EntrySize-Height
          END-IF
         .


       Post-User-Event-Routine section.

      * The EntryCallback class has an extended method to enable 
      * the posting of a user event to the panels2 module. The Dialog System
      * USER-EVENT dialog table may then act on this event

            INVOKE EntryCallback "PostP2Event" USING Queue-ID 
                                                     aHwnd 
                                                     Event-Type
            .


       Null-Terminate-Routine Section.

      *   This section loops back from the end of the field until it
      *   encounters a non space - & puts a null in the right place
      *---The buffer sizes used may require user defined values

          MOVE LENGTH OF Term-Field TO j
          PERFORM VARYING i FROM j BY -1 UNTIL i = 0
               IF Term-Field(i:1) NOT = Spaces
                AND NOT = X"00"
                  ADD 1      TO i
                  MOVE x"00" TO Term-Field(i:1)
                  EXIT SECTION
               END-IF
          END-PERFORM
          MOVE x"00" TO Term-Field(j:1).


       SaveDataBlockPointer Section.

      *    Save a pointer to the datablock in a dictionary. This is
      *    so we can get at the correct datablock in the callback code.

      *    I.E. The datablock thats associated with the window on
      *    which an event occurs.

      *    We dont need to retain a pointer to the object data params -
      *    this data is variable by object instance - allowing the
      *    program to create many instances of the control.

      *    Function-Data group occurs once in each screenset datablock
      *    object data Param groups exist for each instance created


           IF 1stTimeThru

      *       First establish a class for creation of our Association
      *       template. Now PIC X(4) Comp-5 is a class instance
              MOVE 4 TO i
              INVOKE CobolComp5 "newClass" USING i
                                         RETURNING aStorageClass
      *       Now Create an association template using an object
      *       reference and a Pic X(4) Comp-5 data item.
              SET anObject TO NULL
              INVOKE Association "newClass" USING anObject aStorageClass
                                        RETURNING anAssocTemplate

      *       Now create our dictionary (upto 32 items) of associations
              MOVE 32 TO i
              INVOKE Dictionary "ofAssociations" USING anAssocTemplate
                                                       i
                                             RETURNING aDictionary
              SET Not1stTimeThru TO TRUE
           END-IF

      *    Insert the Window/datablock Pointer association into the
      *    dictionary

      $IF useDataBlock NOT DEFINED
           SET DataBlock-Pointer TO ADDRESS OF Function-Data
      $ELSE 
           SET DataBlock-Pointer TO ADDRESS OF Data-Block
      $END
           INVOKE aDictionary "atPut" USING aWindow DataBlock-Pointer
           .


       GetDataBlockPointer section.

      *     This re-sets this entry point program Datablock variable
      *     to the memory location where we know it exists - the
      *     original location identified by the pointer, retrieved from
      *     our dictionary

            INVOKE aDictionary "at" USING aWindow
                                RETURNING DataBlock-Pointer

      $IF useDataBlock NOT DEFINED
            SET ADDRESS OF Function-Data TO DataBlock-Pointer
      $ELSE
            SET ADDRESS OF Data-Block TO DataBlock-Pointer
      $END
            .

