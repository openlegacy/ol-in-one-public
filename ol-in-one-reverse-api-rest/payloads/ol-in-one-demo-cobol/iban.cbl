        IDENTIFICATION DIVISION.
      * Please Provide a valid PROGRAM-ID
        PROGRAM-ID. XXXXXXXX.
      ********************************************************
      *             OpenLegacy Hub CICS Client               *
      ********************************************************
      *       Licensed Materials - Property of OpenLegacy    *
      *          "Restricted Materials of OpenLegacy"        *
      *           (C) Copyright OpenLegacy 2015-2022         *
      ********************************************************
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 RESP                  PIC S9(8) COMP.
        01 RESP2                 PIC S9(8) COMP.
        01 CLI-TOKEN             PIC X(8).
        01 CON-RESPONSE-LENGTH   PIC S9(8) COMP.
        01 HTTP-STATUS-CODE      PIC 9(04) COMP.
        01 HTTP-STATUS-TEXT      PIC X(256).
        01 OCTET-STREAM
           PIC X(56) VALUE 'application/octet-stream'.
        01 WS-PATH               PIC X(5) VALUE '/iban'.
        01 WS-PATH-LEN           PIC S9(8) COMP VALUE +5.

      * Please update with configured proxy URIMAP
        01 OPN-URIMAP            PIC X(8) VALUE 'XXXXXXXX'.

      * Request Response structure
        01 HTTP-REQUEST.
          03 BODY-PARAMS.
            05 IBAN-INPUT.
              07 X-API-KEY PIC X(255).
              07 IBAN PIC X(255).


        01 HTTP-RESPONSE.
          03 STATUS-CODE PIC 9(3).
          03 STATUS-MESSAGE PIC X(256).
          03 RESPONSES.
            05 RESPONSE-E2801E83-B346-4E06-AB.
              07 RESPONSE-E2801E83-B346-4E06-A2.
                09 IB-A-N-RESPONSE-OUTPUT-200.
                  11 IB-ANRESPONSE.
                    13 IBAN2 PIC X(255).
                    13 BANK-NAME PIC X(255).
                    13 ACCOUNT-NUMBER PIC X(255).
                    13 BANK-CODE PIC X(255).
                    13 COUNTRY PIC X(255).
                    13 CHECKSUM PIC X(255).
                    13 VALID PIC X(5).
                      88 VALID-TRUE VALUE 'true'.
                      88 VALID-FALSE VALUE 'false'.
                    13 BBAN PIC X(255).






        PROCEDURE DIVISION.
      * AUTO-GENERATED BLOCK - PROGRAM LOGIC STARTS HERE *****










      * AUTO-GENERATED BLOCK - PROGRAM LOGIC ENDS HERE *****

      * AUTO-GENERATED DO NOT EDIT *****
      * Open HTTP Connection *
            EXEC CICS WEB OPEN
                 SESSTOKEN(CLI-TOKEN)
                 URIMAP(OPN-URIMAP)
                 RESP(RESP)
                 RESP2(RESP2)
            END-EXEC.
            IF RESP NOT EQUAL DFHRESP(NORMAL)
               DISPLAY 'CLI-OPEN-FAILED'
               GO TO END-PROGRAM
            END-IF.

      * AUTO-GENERATED DO NOT EDIT *****


      * Sends an HTTP request and receives a response *
            EXEC CICS WEB CONVERSE
               POST
               SESSTOKEN(CLI-TOKEN)
               MEDIATYPE(OCTET-STREAM)
               FROM(HTTP-REQUEST)
               FROMLENGTH(LENGTH OF HTTP-REQUEST)
               INTO(HTTP-RESPONSE)
               MAXLENGTH(LENGTH OF HTTP-RESPONSE)
               TOLENGTH(CON-RESPONSE-LENGTH)
               STATUSCODE(HTTP-STATUS-CODE)
               PATH(WS-PATH)
               PATHLENGTH(WS-PATH-LEN)
               STATUSLEN(LENGTH OF HTTP-STATUS-TEXT)
               STATUSTEXT(HTTP-STATUS-TEXT)
               NOTRUNCATE
               NOCLICONVERT
            END-EXEC.

            IF RESP EQUAL DFHRESP(NORMAL)
               DISPLAY 'CLI-CONV-SUCCESS'
            ELSE
               DISPLAY 'CLI-CONV-FAILED'
               GO TO END-PROGRAM
            END-IF.
            EXEC CICS WEB CLOSE
               SESSTOKEN(CLI-TOKEN)
            END-EXEC.

      * AUTO-GENERATED BLOCK - API RESULT LOGIC STARTS HERE *****









      * AUTO-GENERATED BLOCK - API RESULT LOGIC ENDS HERE *****

          END-PROGRAM.
            EXEC CICS RETURN END-EXEC.
