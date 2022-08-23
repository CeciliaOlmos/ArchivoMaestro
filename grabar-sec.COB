      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
               ASSIGN TO "..\archFil.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-filial  pic 9.
           03 soc-socio pic 9(4).
           03 soc-importe  pic s9(8)V99.
           03 soc-modalidad pic x.
       WORKING-STORAGE SECTION.
       01  cont pic 999 value zeros.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-ING-FILIAL.
           PERFORM UNTIL soc-filial = 0
             PERFORM 300-INGRESO-RESTO
             PERFORM 400-PROCESO
             PERFORM 200-ING-FILIAL
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
      *************************** R U T I N A S ***********************
       100-INICIO.
           DISPLAY "Programa para grabar el archivo Socios".
           OPEN EXTEND SOCIOS.

       200-ING-FILIAL.
           PERFORM 220-TRAER-FILIAL.
           PERFORM 230-VERIF-FILIAL UNTIL soc-filial >= 0
           AND soc-filial <= 9.

       220-TRAER-FILIAL.
           DISPLAY "Ingrese nro de filial".
           ACCEPT soc-filial.

       230-VERIF-FILIAL.
           DISPLAY "Error! Ingrese un numero entre 0 y 9"
           PERFORM 220-TRAER-FILIAL.

       300-INGRESO-RESTO.
           PERFORM 320-ING-SOCIO.
           PERFORM 340-ING-IMPORTE.
           PERFORM 340-ING-MODALIDAD.

       320-ING-SOCIO.
           PERFORM 325-TRAER-SOCIO.
           PERFORM 230-VERIF-SOCIO UNTIL soc-socio >= 1
           AND soc-filial <= 9999.

       325-TRAER-SOCIO.
           display "Ingrese el codigo del socio".
           accept soc-socio.

       230-VERIF-SOCIO.
           DISPLAY "Error! Debe ser un numero de 4 digitos".
           PERFORM 325-TRAER-SOCIO.

       340-ING-IMPORTE.
           PERFORM 350-TRAER-IMPORTE.
           PERFORM 230-VERIF-IMPORTE UNTIL soc-importe >= 1.

       230-VERIF-IMPORTE.
           DISPLAY "Error! El importe no puede ser ni negativo ni cero".
           PERFORM 350-TRAER-IMPORTE.

       350-TRAER-IMPORTE.
           display "Ingrese el importe".
           accept soc-importe.

       340-ING-MODALIDAD.
           PERFORM 350-TRAER-MODALIDAD.
           PERFORM 230-VERIF-IMPORTE UNTIL soc-modalidad = 'D' OR
           soc-modalidad = 'E' OR soc-modalidad = 'T'.

       230-VERIF-MODALIDAD.
           DISPLAY "Error! Modalidad incorrecta".
           PERFORM 350-TRAER-MODALIDAD.

       350-TRAER-MODALIDAD.
           display "Ingrese la modalidad (D - E - T)".
           accept soc-modalidad.

       400-PROCESO.
           ADD 1 TO cont.
           WRITE soc-reg.

       500-FIN.
           CLOSE SOCIOS.
           display "se procesaron ", cont, " socios".

       END PROGRAM YOUR-PROGRAM-NAME.
