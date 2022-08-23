      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA-OLMOS.
      *******************************************************************
      ************************** archivo fisico *************************
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archSoc.dat".
           SELECT FILIALES
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archFil.dat".
       DATA DIVISION.
       FILE SECTION.


      *******************************************************************
      ************************** archivo logico *************************
      *******************************************************************
       FD  SOCIOS.
       01  soc-reg.
           03 soc-filial pic 9.
           03 soc-socio pic 9(4).
           03 soc-importe pic s9(8)v99.
           03 soc-modal pic x.
       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 9.
           03 fil-nombre pic x(15).

       WORKING-STORAGE SECTION.
      *******************************************************************
      ************************** variables de trabajo *******************
      *******************************************************************
       01  gen-imp-trans pic s9(8)v99.
       01  fil-max pic x(15).
       01  fil-cod-max pic 9.
       01  cont-soc-max pic 999.
      ************************variables socios************************************
       01  flag-socio pic 9.
       01  soc-filial-ant pic 9.
       01  soc-soc-ant pic 9(4).
       01  soc-imp-acum pic s9(8)v99.
       01  gen-cont-soc pic 999.

      ************************variables filial*******************************************

       01  flag-filial pic 9.
       01  fil-codigo-ant pic 9.
       01  fil-acum-imp pic s9(8)v99.
       01  cont-soc-filial pic 999.


      *******************************************************************
      ************************** LINEAS DE IMPRESION ********************
      *******************************************************************
       01  lin-guarda.
           03 filler pic x(80) value all "*".
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "BANCO: EL CORRALITO".
           03 filler pic x(31) value spaces.
       01  lin-subtiltulo.
           03 filler pic x(17) value spaces.
           03 filler pic x(46) value "LISTADO DE TRANSFERENCIAS "-
           "BANCARIAS DE  SOCIOS".

       01  lin-filial.
           03 filler pic x(28) value spaces.
           03 filler pic x(8) value "FILIAL:".
           03 l-cod pic s9.
           03 filler pic x value "-".
           03 l-filial pic x(15) value spaces.
           03 filler pic x(27) value spaces.
       01  lin-soc.
           03 filler pic x(26) value spaces.
           03 filler pic x(5) value "SOCIO".
           03 filler pic x(20) value spaces.
           03 filler pic x(7) value "IMPORTE".
           03 filler pic x(22) value spaces.
       01  lin-val.
           03 filler pic x(26) value spaces.
           03 lin-socio pic x(5) value spaces.
           03 filler pic x(13) value spaces.
           03 lin-soc-imp pic z.zzz.zzz.zz9,99.
           03 filler pic x(24) value spaces.
       01  lin-guion.
           03 filler pic x(21) value spaces.
           03 filler pic x(12) value all "-".
           03 filler pic x(15) value spaces.
           03 filler pic x(12) value all "-".
           03 filler pic x(20) value spaces.
       01  lin-tot.
           03 filler pic x(12) value spaces.
           03 filler pic x(6) value "Total:".
           03 filler pic x(10) value spaces.
           03 l-cont pic 9.
           03 filler pic x(15) value spaces.
           03 l-imp-ac pic z.zzz.zzz.zz9,99.
           03 filler pic x(24) value spaces.
       01  lin-titulo-gen.
           03 filler pic x(30) value space.
           03 filler pic x(20) value "ESTADISTICA GENERAL:".
           03 filler pic x(30) value space.
       01  lin-subrayado.
           03 filler pic x(30) value space.
           03 filler pic x(20) value all "-".
           03 filler pic x(30) value space.
       01  lin-estadistica.
           03 filler pic x(24) value "En total se procesaron a".
           03 l-cont-gral pic zz9.
           03 filler pic x(7) value " socios".
           03 filler pic x(46) value spaces.
       01  lin-est-imp.
           03 filler pic x(38) value "El importe general transferido "-
           "fue de".
           03 l-imp-gral pic z.zzz.zzz.zz9,99.
           03 filler pic x(30) value spaces.
       01  lin-general.
           03 filler pic x(47) value "La filial que mas socios con "-
           "transferencia fue".
           03 l-cod-max pic x.
           03 filler pic x value "-".
           03 l-fil-max pic x(15).
           03 filler pic x(4) value "con ".
           03 l-cont-fil pic zz9.
           03 filler pic x(7) value " socios".
           03 filler pic x(2) value spaces.


       PROCEDURE DIVISION.
      *******************************************************************
      **************************PROGRAMA PRINCIPAL *********************
      *******************************************************************
       MAIN-PROCEDURE.

           PERFORM 100-INICIO-GENERAL.
             PERFORM 300-LEER-ARCHIVO-FILIAL.
             PERFORM 200-LEER-ARCHIVO-SOCIO.
             PERFORM UNTIl flag-filial IS EQUAL 1
              PERFORM 400-INICIO-FILIAL
              PERFORM UNTIL flag-socio IS EQUALS 1
                         OR fil-codigo IS NOT EQUALS soc-filial
                         OR fil-codigo IS NOT EQUALS fil-codigo-ant
                  PERFORM 600-INICIO-SOCIO
                  PERFORM UNTIL flag-socio IS EQUALS 1
                             OR fil-codigo IS NOT EQUALS soc-filial
     *                        OR fil-codigo IS NOT EQUALS fil-codigo-ant
                             OR soc-socio IS NOT EQUALS soc-soc-ant

                       PERFORM 900-PROCESO-SOCIO
                       PERFORM 200-LEER-ARCHIVO-SOCIO
                  END-PERFORM
                  PERFORM 700-FIN-SOCIO
              END-PERFORM
               PERFORM 500-FIN-FILIAL
               PERFORM 300-LEER-ARCHIVO-FILIAL
             END-PERFORM.
             PERFORM 800-FIN-GENERAL.
            STOP RUN.

        100-INICIO-GENERAL.
           PERFORM 110-ABRIR-ARCHIVO.
           PERFORM 120-INI-VAR.
           PERFORM 130-MUESTRO-TITULO.

        110-ABRIR-ARCHIVO.
            open INPUT SOCIOS.
            open INPUT FILIALES.

        120-INI-VAR.
            MOVE ZERO to flag-filial.
            MOVE ZERO to flag-socio.
            MOVE ZERO TO gen-cont-soc.
            MOVE ZERO TO gen-imp-trans.
            MOVE ZERO TO fil-max.
            MOVE ZERO to cont-soc-max.
            MOVE ZERO to fil-cod-max.

        130-MUESTRO-TITULO.
            DISPLAY lin-guarda.
            DISPLAY lin-titulo.
            DISPLAY lin-subtiltulo.
            DISPLAY lin-guarda.


        200-LEER-ARCHIVO-SOCIO.
            READ SOCIOS AT END MOVE 1 TO flag-socio.

        300-LEER-ARCHIVO-FILIAL.
            READ FILIALES AT END MOVE 1 TO flag-filial.

        400-INICIO-FILIAL.
           MOVE ZERO to fil-acum-imp.
           MOVE zero to cont-soc-filial.
           MOVE fil-nombre to l-filial.
           MOVE fil-codigo TO fil-codigo-ant.
           MOVE fil-codigo-ant to l-cod.

           DISPLAY lin-filial.
           DISPLAY lin-soc.

        500-FIN-FILIAL.
           ADD fil-acum-imp to gen-imp-trans.
           ADD cont-soc-filial to gen-cont-soc.

           IF cont-soc-filial > cont-soc-max THEN
                MOVE cont-soc-filial to cont-soc-max
                MOVE fil-nombre to fil-max
                MOVE fil-codigo-ant to fil-cod-max
           END-IF.
           MOVE cont-soc-filial to l-cont.
           MOVE fil-acum-imp to l-imp-ac.
           DISPLAY lin-guion.
           DISPLAY lin-tot.

        600-INICIO-SOCIO.
           MOVE soc-socio TO soc-soc-ant.
           MOVE zero to soc-imp-acum.

        700-FIN-SOCIO.

            IF soc-imp-acum > ZERO THEN
               ADD 1 TO cont-soc-filial
               ADD soc-imp-acum TO fil-acum-imp
               MOVE soc-imp-acum to lin-soc-imp
               MOVE soc-soc-ant to lin-socio
               DISPLAY lin-val

            END-IF.

        800-FIN-GENERAL.
            CLOSE FILIALES.
            CLOSE SOCIOS.
            PERFORM 850-MUESTRO-TOTALES.
        850-MUESTRO-TOTALES.
            MOVE gen-cont-soc to l-cont-gral.
            MOVE gen-imp-trans to l-imp-gral.
            MOVE fil-cod-max to l-cod-max.
            MOVE fil-max to l-fil-max.
            MOVE cont-soc-max to l-cont-fil.
            DISPLAY lin-guarda.
            DISPLAY lin-titulo-gen.
            DISPLAY lin-subrayado.
            DISPLAY lin-estadistica.
            DISPLAY lin-est-imp.
            DISPLAY lin-general.
            DISPLAY lin-guarda.

        900-PROCESO-SOCIO.
           IF soc-modal IS EQUALS "T" THEN
               ADD soc-importe to soc-imp-acum
           END-IF.


       END PROGRAM CECILIA-OLMOS.
