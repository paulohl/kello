       78 NextAvailEvent value 67.
       78 p2ce-destroyed value 66.
       78 p2ce-Created value 65.
       78 p2ce-update value 64.
       78 p2ce-beginDrag    value 63.
       78 p2ce-BeginEditItem value 61.
       78 p2ce-EndEditItem value 62.

       78 p2ce-button3ClkOnItem value 60.
       78 p2ce-SelectionChanged value 25.

       78 p2ce-help     value 26.

       78 p2ce-noevent value 0.

      *> Window events sent to the window object

        *>> 1 - 9 has to be reserved for message box return values
        *>> for compatability with pan2link.cpy

       78 p2ce-move value 51.
       78 moved value 51.
       78 p2ce-resize value 52.
       78 resized value 52.
       78 p2ce-close value 53.
       78 closed value 53.
       78 p2ce-activate value 54.
       78 activated value 54.
       78 p2ce-deactivate value 55.
       78 deactivated value 55.

      *> General gadget events sent to the gadget object

       78 p2ce-gainfocus value 56.
       78 gained-focus value 56.
       78 p2ce-losefocus value 57.
       78 lost-focus value 57.
       78 p2ce-accept value 58.
       78 accepted value 58.

      *> Menu event sent to the window object

       78 p2ce-menuselect value 59.
       78 menuSelected value 59.

      *> Scroll Bar events sent to the scrollbar object

       78 p2ce-lineup value 10.
       78 lineUp value 10.
       78 p2ce-linedown value 11.
       78 lineDown value 11.
       78 p2ce-pageup value   12.
       78 pageUp value 12.
       78 p2ce-pagedown value 13.
       78 pageDown value 13.
       78 p2ce-verttrack value 14.
       78 verticalTrack value 14.
       78 p2ce-vertposition value 15.
       78 verticalPosition value 15.
       78 p2ce-vertend value 16.
       78 verticalEnd value 16.
       78 p2ce-lineleft value 17.
       78 lineLeft value 17.
       78 p2ce-lineright value 18.
       78 lineRight value 18.
       78 p2ce-pageleft value 19.
       78 pageLeft value 19.
       78 p2ce-pageright value 20.
       78 pageRight value 20.
       78 p2ce-horiztrack value 21.
       78 horizontalTrack value 21.
       78 p2ce-horizposition value 22.
       78 horizontalPosition value 22.
       78 p2ce-horizend value 23.
       78 horizontalEnd value 23.


      *> Button events sent to the to the button object

       78 p2ce-clicked value 24.
       78 clicked value 24.

      *> List Box events sent to the List Box object

       78 p2ce-select value 27.


      *> Mouse Events sent to the object under the mouse

       78 p2ce-mousemoved    value 28.
       78 mouseMoved         value 28.
       78 p2ce-button1down   value 29.
       78 button1Down        value 29.
       78 p2ce-button1up     value 30.
       78 button1Up          value 30.
       78 p2ce-button1dblclk value 31.
       78 p2ce-button2down   value 32.
       78 p2ce-button2up     value 33.
       78 p2ce-button2dblclk value 34.
       78 p2ce-button3down   value 35.
       78 button3Down        value 35.
       78 p2ce-button3up     value 36.
       78 button3Up          value 36.
       78 p2ce-button3dblclk value 37.
       78 button3dblclk      value 37.


      *>> Keyboard events sent to a window or gadget object with focus

       78 p2ce-dataKey         value 38.
       78 p2ce-functionkey     value 39.
       78 p2ce-systemkey       value 40.

       78 p2ce-command         value 42.
       78 p2ce-valueChanged    value 43.
       78 p2ce-itemResized     value 44.
       78 p2ce-itemResizing    value 45.

       78 selectionChanging        value 46.
       78 p2ce-selectionChanging   value 46.

       78 Itemcollapsed            value 47.
       78 p2ce-itemCollapsed       value 47.

       78 ItemExpanded             value 48.
       78 p2ce-itemExpanded        value 48.

       78 ItemExpanding           value 49.
       78 p2ce-ItemExpanding      value 49.

       78 ItemCollapsing          value 41.
       78 p2ce-ItemCollapsing     value 41.

       78 getContents          value 50.

      *> Events returned from message boxes

      $if mb-return-ok not defined
       78 mb-return-ok          value 1.
       78 mb-return-cancel      value 2.
       78 mb-return-abort       value 3.
       78 mb-return-retry       value 4.
       78 mb-return-ignore      value 5.
       78 mb-return-yes         value 6.
       78 mb-return-no          value 7.
       78 mb-return-help        value 8.
       78 mb-return-enter       value 9.
      $end

       78 p2ce-FilesDropped             value 50.

       *> keystates

       78 p2ce-ctrl            value 262144.
       78 p2ce-left-shift      value 131072.
       78 p2ce-right-shift     value 65536.
       78 p2ce-alt             value 524288.
       78 p2ce-ins-lock        value 8.
       78 p2ce-caps-lock       value 4.
       78 p2ce-num-lock        value 2.
       78 p2ce-scroll-lock     value 1.



           *> Values returned from getminmaxstate for sizing event
       78  Normal              value 0.
       78  Maximized           value 1.
       78  Minimized           value 2.


       78 IDC-OK               value 1.
       78 IDC-CANCEL           value 2.
