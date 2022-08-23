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
           "..\archSoc.dat".
           SELECT FILIALES
           ASSIGN TO
           "..\archFil.dat".
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
       01  w-soc-fil-ant pic 9.
       01  w-fil-cod-ant pic 9.
       01  w-soc-imp-acum pic s9(8)v99.
       01  w-flag-socio pic 9.
       01  w-flag-filial pic 9.
       01  w-cant-soc-fil pic 999.
       01  w-fil-nom pic x(15).
       01  w-fil-acum-imp pic s9(8)v99.
       01  w-gen-imp-trans pic s9(8)v99.
       01  w-gen-cont-soc pic 999.
       01  w-cont-soc-max pic 999.
       01  w-fil-max pic x(15).
       01  w-fil-cod-max pic 9.

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
       MAIN-PROCEDURE.

           PERFORM 100-INICIO-GENERAL.

           PERFORM 200-LEER-SOCIO.
           PERFORM UNTIL w-flag-socio IS EQUAL 1
             PERFORM 500-INICO-FILIAL
                PERFORM UNTIL  w-flag-socio IS EQUAL 1 OR
                              fil-codigo IS NOT EQUALS soc-filial
                         PERFORM 400-INICIO-SOCIO
                           PERFORM UNTIL w-flag-socio IS EQUAL 1 OR
                           soc-socio IS NOT EQUALS w-soc-fil-ant
                           OR fil-codigo IS NOT EQUALS w-fil-cod-ant
                               PERFORM 600-PROCESO
                                PERFORM 200-LEER-SOCIO
                           END-PERFORM
                         PERFORM 700-FIN-FILIAL

               END-PERFORM
                   PERFORM 800-FIN-SOCIO
           END-PERFORM.
           PERFORM 900-FIN-GENERAL.

        100-INICIO-GENERAL.
            PERFORM 110-ABRIR-ARCHIVO.
            PERFORM 120-INI-VAR.
            PERFORM 130-MUESTRO-TITULO.
        110-ABRIR-ARCHIVO.
            open INPUT SOCIOS.
            open INPUT FILIALES.
        120-INI-VAR.
            MOVE ZERO to w-flag-filial.
            MOVE ZERO to w-flag-socio.
            MOVE zero to  w-gen-imp-trans.
            MOVE ZERO to w-gen-cont-soc.
            MOVE ZERO to w-cont-soc-max.
            MOVE ZERO to w-fil-max.
            MOVE ZERO to w-fil-cod-max.

        130-MUESTRO-TITULO.
            DISPLAY lin-guarda.
            DISPLAY lin-titulo.
            DISPLAY lin-subtiltulo.
            DISPLAY lin-guarda.

        200-LEER-SOCIO.
            READ SOCIOS AT END MOVE 1 TO w-flag-socio.
        300-LEER-FILIAL.
            READ FILIALES AT END MOVE 1 TO w-flag-filial.

        400-INICIO-SOCIO.
            MOVE soc-socio to w-soc-fil-ant.
            MOVE ZERO to w-soc-imp-acum.

        500-INICO-FILIAL.
            MOVE ZERO TO w-fil-acum-imp.
            move zero to w-cant-soc-fil.
            PERFORM 300-LEER-FILIAL.
            PERFORM UNTIL w-flag-filial is EQUAL 1 OR
                          soc-socio IS EQUALS fil-codigo
                           PERFORM 300-LEER-FILIAL
            END-PERFORM.
            MOVE fil-codigo to w-fil-cod-ant.
            MOVE w-fil-cod-ant TO l-cod.
            DISPLAY lin-filial.
            DISPLAY lin-soc.

        600-PROCESO.
            IF soc-modal IS EQUAL "T" THEN
               ADD soc-importe to w-soc-imp-acum
             END-IF.

        700-FIN-FILIAL.
           ADD w-fil-acum-imp to w-gen-imp-trans.
           ADD w-cant-soc-fil to w-gen-cont-soc.
           IF w-cant-soc-fil > w-cont-soc-max THEN
                MOVE w-cant-soc-fil to w-cont-soc-max
                MOVE fil-nombre to w-fil-max
                MOVE w-fil-cod-ant to w-fil-cod-max
           END-IF.
           PERFORM 710-MUESTRO-ENCABEZADO.

        710-MUESTRO-ENCABEZADO.

           MOVE  w-cant-soc-fil to l-cont.
           MOVE w-fil-acum-imp to l-imp-ac.
           DISPLAY lin-guion.
           DISPLAY lin-tot.

        800-FIN-SOCIO.
              IF w-soc-imp-acum > ZERO THEN
               ADD 1 TO w-cant-soc-fil
               ADD w-soc-imp-acum TO w-fil-acum-imp
               MOVE w-soc-imp-acum to lin-soc-imp
               MOVE w-soc-fil-ant to lin-socio
               DISPLAY lin-val
            END-IF.

        900-FIN-GENERAL.
            CLOSE FILIALES.
            CLOSE SOCIOS.
            PERFORM 910-MUESTRO-TOTALES.
        910-MUESTRO-TOTALES.
            MOVE w-gen-cont-soc to l-cont-gral.
            MOVE w-gen-imp-trans to l-imp-gral.
            MOVE w-fil-cod-max to l-cod-max.
            MOVE w-fil-max to l-fil-max.
            MOVE w-cont-soc-max to l-cont-fil.
            DISPLAY lin-guarda.
            DISPLAY lin-titulo-gen.
            DISPLAY lin-subrayado.
            DISPLAY lin-estadistica.
            DISPLAY lin-est-imp.
            DISPLAY lin-general.
            DISPLAY lin-guarda.

            STOP RUN.
       END PROGRAM CECILIA-OLMOS.
