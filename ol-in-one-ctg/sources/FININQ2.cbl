  IDENTIFICATION DIVISION.
        PROGRAM-ID. FININQ2.
        DATA DIVISION.
        LINKAGE SECTION.
        01 DFHCOMMAREA.
           03 CUST-ID               PIC X(10).
           03 CREDIT-CARDS      OCCURS 5 TIMES.
              05  CARD-NUMBER            PIC X(16).
              05  CARD-TYPE              PIC X(16).
              05  CARD-LIMIT             PIC S9(4) COMP.
              05  CARD-USAGE             PIC S9(4) COMP.
        PROCEDURE DIVISION.
            EVALUATE CUST-ID
            WHEN '1000012345'
               MOVE '4580523489278944' TO CARD-NUMBER(1)
               MOVE 'BASIC     ' TO CARD-TYPE(1)
               MOVE 2500         TO CARD-LIMIT(1)
               MOVE 1986         TO CARD-USAGE(1)
            WHEN '1000000000'
               MOVE '4580173782784961' TO CARD-NUMBER(1)
               MOVE 'PLATINUM  ' TO CARD-TYPE(1)
               MOVE 10000        TO CARD-LIMIT(1)
               MOVE 8937         TO CARD-USAGE(1)
               MOVE '4580983655281742' TO CARD-NUMBER(2)
               MOVE 'BUSINESS-P' TO CARD-TYPE(2)
               MOVE 10000        TO CARD-LIMIT(2)
               MOVE 100          TO CARD-USAGE(2)
            WHEN OTHER
               MOVE '4580123412341234' TO CARD-NUMBER(1)
               MOVE 'GOLD      ' TO CARD-TYPE(1)
               MOVE 5000         TO CARD-LIMIT(1)
               MOVE 1783         TO CARD-USAGE(1)
               MOVE '4580002377826452' TO CARD-NUMBER(2)
               MOVE 'PLATINUM  ' TO CARD-TYPE(2)
               MOVE 10000        TO CARD-LIMIT(2)
               MOVE 567          TO CARD-USAGE(2)
               MOVE '4580887386255265' TO CARD-NUMBER(3)
               MOVE 'BUSINESS-G' TO CARD-TYPE(3)
               MOVE 7000         TO CARD-LIMIT(3)
               MOVE 4873         TO CARD-USAGE(3)
               MOVE '4580108372533424' TO CARD-NUMBER(4)
               MOVE 'BASIC     ' TO CARD-TYPE(4)
               MOVE 1000         TO CARD-LIMIT(4)
               MOVE 0            TO CARD-USAGE(4)
               MOVE '4580773685986244' TO CARD-NUMBER(5)
               MOVE 'FT-MEMBER ' TO CARD-TYPE(5)
               MOVE 2000         TO CARD-LIMIT(5)
               MOVE 600          TO CARD-USAGE(5)
            END-EVALUATE
            EXEC CICS RETURN END-EXEC
            .