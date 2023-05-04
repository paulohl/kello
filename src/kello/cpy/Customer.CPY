           EXEC SQL DECLARE Customer TABLE
           ( CustID                VARCHAR(5)
            ,Company               VARCHAR(40)
            ,Address               VARCHAR(40)
            ,City                  VARCHAR(20)
            ,Region                VARCHAR(20)
            ,PostalCode            VARCHAR(20)
            ,Country               VARCHAR(20)
            ,Phone                 VARCHAR(20)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE Customer                           *
      ******************************************************************
       01  DCLCustomer.
           03 Customer-CustID                  PIC X(5).
           03 Customer-Company                 PIC X(40).
           03 Customer-Address                 PIC X(40).
           03 Customer-City                    PIC X(20).
           03 Customer-Region                  PIC X(20).
           03 Customer-PostalCode              PIC X(20).
           03 Customer-Country                 PIC X(20).
           03 Customer-Phone                   PIC X(20).
      ******************************************************************
      * COBOL INDICATOR VARIABLES FOR TABLE                            *
      ******************************************************************
       01  DCLCustomer-NULL.
           03 Customer-CustID-NULL             PIC S9(04)  COMP-5.
           03 Customer-Company-NULL            PIC S9(04)  COMP-5.
           03 Customer-Address-NULL            PIC S9(04)  COMP-5.
           03 Customer-City-NULL               PIC S9(04)  COMP-5.
           03 Customer-Region-NULL             PIC S9(04)  COMP-5.
           03 Customer-PostalCode-NULL         PIC S9(04)  COMP-5.
           03 Customer-Country-NULL            PIC S9(04)  COMP-5.
           03 Customer-Phone-NULL              PIC S9(04)  COMP-5.
