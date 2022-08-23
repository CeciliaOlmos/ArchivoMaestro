      ******************************************************************
      * Author: Grupo 7 - Gisele Muñoz, Gisela Dos Santos, Federico
      * Guzman
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILIALES
               ASSIGN TO "..\archFil.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES.
       01  fil-reg.
           03 fil-id     pic 9.
           03 fil-nombre pic X(15).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-ING-FILIAL
           PERFORM UNTIL fil-id = 0
             PERFORM 300-INGRESO-NOMBRE
             PERFORM 400-PROCESO
             PERFORM 200-ING-FILIAL
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
      *************************** R U T I N A S ***********************
       100-INICIO.
           DISPLAY "Programa para grabar un archivo de salida".
           OPEN OUTPUT FILIALES.

       200-ING-FILIAL.
           PERFORM 210-TRAIGO-FILIAL
           PERFORM UNTIL fil-id >= 0 AND fil-id < 10
               DISPLAY "ERROR"
               PERFORM 210-TRAIGO-FILIAL
           END-PERFORM.

       210-TRAIGO-FILIAL.
           DISPLAY "Ingrese nro. de filial, x fin ingrese 0".
           ACCEPT fil-id.

       300-INGRESO-NOMBRE.
           PERFORM 320-ING-NOMBRE.
           PERFORM UNTIL fil-nombre <> SPACE
               DISPLAY "ERROR"
               PERFORM 320-ING-NOMBRE
           END-PERFORM.

       320-ING-NOMBRE.
           DISPLAY "Ingrese el nombre de la filial".
           ACCEPT fil-nombre.

       400-PROCESO.
           WRITE fil-reg.

       500-FIN.
           CLOSE FILIALES.
       END PROGRAM YOUR-PROGRAM-NAME.
