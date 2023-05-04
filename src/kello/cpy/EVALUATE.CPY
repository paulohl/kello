          WHEN "REGISTER-CALLBACKS"

      *        This is provided as a mechanism to establish callbacks
      *        for events, when the gadget has been created and
      *        maintained using the dialog definition software

               SET anObject TO Object-Reference

               PERFORM Register-Callbacks

          WHEN "CLEAR-OBJECT"

              PERFORM Clear-Object-Function

          WHEN "ENABLE-OBJECT"

      *        Enable the object Instance, this uses the
      *        AbstractWindow Class method for this control

               INVOKE Object-Reference "Enable"

          WHEN "DISABLE-OBJECT"

      *        Disable the object Instance, this uses the
      *        AbstractWindow Class method for this control

               INVOKE Object-Reference "Disable"

          WHEN "SET-FOCUS"

      *        setFocus on the object Instance, this uses the
      *        AbstractWindow Class method for this control

      *        Note that invoking this method, and having this program
      *        with an entryCallback for this event requires existence
      *        of a a local-storage section to allow recursion, even
      *        though no data may be used.

               INVOKE Object-Reference "setFocus"

          WHEN "HIDE-OBJECT"

      *        Hide the Object Instance, this uses the
      *        AbstractWindow Class method for this control

               INVOKE Object-Reference "hide"

          WHEN "SHOW-OBJECT"

      *        show the object Instance, this uses the
      *        AbstractWindow Class method for this control

               INVOKE Object-Reference "show"


          WHEN "SET-MOUSE-SHAPE"

      *        Set the mouse shape of the pointer when
      *        over this object Instance

               PERFORM MouseShape-Function

          WHEN "SET-COLOR"

      *        Set the color of the object Instance

               PERFORM Object-Color-Function

          WHEN "SET-FONT"

      *        Set the font of the object Instance

               PERFORM Set-Font-Function

