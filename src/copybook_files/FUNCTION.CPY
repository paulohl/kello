
      *Create-Object-Instance section.

      *   Example code Section detailed here for code template purpose

      *   Invoke an "new" instance of the relevant class, as a child
      *   of the Window instance, giving an object reference for
      *   this new instance, this object reference is returned in the
      *   datablock for later use.

      *---INVOKE <class>  "new" USING aWindow
      *---         RETURNING <anInstance>

      *---SET Object-Reference TO <anInstance>

      *   User required Sizes/Positions retrieved from linkage
      *   These values relate to panels2 generic co-ordinates
      *   X,Y co-ordinates from the top left of the parent

      *   Invoke the setRectangle method of the treeview instance to
      *   establish the physical sizes on creation

      *---INVOKE <anInstance> "setRectangle" USING Position-X Position-Y
      *---                                        Size-Width Size-Height

      *   Create the control, nothing will be painted until the
      *   class library system event loop is resumed

      *---INVOKE <anInstance> "create".


      *   The following sections are not class/instance dependant

       Object-Color-Function section.

      *        Note that this section provides very simple example of
      *        setting colors using named values... you may also set
      *        a full range using RGB values via the class library

      *        1stBuffer is foreground color, invoke the
      *        color class returning an instance of the color required

               MOVE IO-Text-Buffer TO Term-Field
               PERFORM Null-Terminate-Routine
               INVOKE color Term-Field RETURNING aForeColor

      *        and the background color

               MOVE IO-Text-Buffer2 TO Term-Field
               PERFORM Null-Terminate-Routine
               INVOKE color Term-Field RETURNING aBackColor

      *        Invoke the setColor method of the AbstractWindow class
               INVOKE Object-Reference "setColor" USING aForeColor
                                                        aBackColor.

      *        We wont be using these objects again so destroy
               INVOKE aForeColor "finalize" RETURNING aForeColor
               INVOKE aBackColor "finalize" RETURNING aBackColor
               .


       MouseShape-Function section.

      *        Note that this section provides very simple example of
      *        setting mouse shapes using named system shape values...
      *        You may also set your own pointer shapes using the
      *        fromResource method of the mouseCursor class

      *        1stBuffer is the mouse shape indicator.
      *        Set up a CharacterArray with the name of the pointer
      *        required. Invoke the Mouse setshape method with the
      *        control and pointer required.

      *        Recall this routine to set it normal for this control
               MOVE IO-Text-Buffer TO Term-Field
               PERFORM Null-Terminate-Routine
               INVOKE CharacterArray "withValue" USING Term-Field
                                             RETURNING aCursor

               INVOKE Mouse "setShape" USING Object-Reference
                                             aCursor.

      *        This mouse object reference wont be used again so destroy

               INVOKE aCursor "finalize" RETURNING aCursor
               .

      $IF statusBar NOT DEFINED
       Clear-Object-Function section.

      *        Delete all Object Instance items

               INVOKE Object-Reference "deleteAll"
               .
      $END

       Set-Font-Function section.

      *    This section provides a very simple example of setting
      *    the font of an object.

      *    Note that it is the responsibility of the application
      *    to finalize any new fonts created. These fonts should
      *    be finalized when the objects that reference them
      *    are finalized and when setting new fonts on objects.

      *    Create a character array to hold the facename
      *    specified in IO-Text-Buffer.

           MOVE LENGTH OF IO-Text-Buffer TO i
           INVOKE CharacterArray "fromBuffer" USING i
                                                    IO-Text-Buffer
                                          RETURNING aCharArray

      *    Create a new font using the specified facename, point size
      *    and style.

           INVOKE font "new" USING aCharArray     *> Facename
                                   Numeric-Value  *> Point size
                                   Numeric-Value2 *> Font style
                         RETURNING aFont

           INVOKE aCharArray "finalize" RETURNING aCharArray

      *    Apply the font to the object identified by Object-Reference.

           INVOKE Object-Reference "setFont" USING aFont

      *    Return the font in Object-Reference.

           SET Object-Reference TO aFont
           SET aFont TO NULL
           .

