      *The generic interface to Win95 Control routines
      *

       01 Function-Data.
      *      The handle of the DS window
      *      (obtained from MOVE-OBJECT-HANDLE) function
          03 Window-Handle        Pic X(4) comp-5.

      *      The object reference which identifies from DS the object
      *      that functions are to be applied to
          03 Object-Reference     usage object reference.

      *      What function to be performed
          03 Call-Function        Pic X(30).

      *      Used as parameters in Function calls
          03 Numeric-Value        Pic X(4) comp-5.
          03 Numeric-Value2       Pic X(4) comp-5.

          03 Size-Width           Pic X(4) Comp-5.
          03 Size-Height          Pic X(4) Comp-5.
          03 Position-X           Pic X(4) Comp-5.
          03 Position-Y           Pic X(4) Comp-5.

          03 IO-Text-Buffer       Pic X(256).
          03 IO-Text-Buffer2      Pic X(256).
