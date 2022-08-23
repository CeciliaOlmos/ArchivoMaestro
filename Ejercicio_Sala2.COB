      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ejercicio_Clase.
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
       FD  SOCIOS.
       01  soc-reg.
           03 soc-filial pic 9.
           03 soc-socio pic 9(4).
           03 soc-importe pic 9(8)v99.
           03 soc-modalidad pic x.

       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 9.
           03 fil-nombre pic x(15).

       WORKING-STORAGE SECTION.
      ***** Lineas de impresion ***********************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "BANCO: EL CORRALITO".
           03 filler pic x(31) value spaces.

       01  lin-sub-titulo.
           03 filler pic x(17) value spaces.
           03 filler pic x(46) value "LISTADO DE TRANSFERENCIAS "-
           "BANCARIAS DE  SOCIOS".

       01  lin-guion.
           03 filler pic x(80) value all "-".

       01  lin-titulo-filial.
           03 filler pic x(26) value spaces.
           03 filler pic x(8) value "FILIAL: ".
           03 fil-dat-cod pic x.
           03 filler pic x(3) value " - ".
           03 fil-dat-nom pic x(15).

       01  lin-titulo-soc.
           03 filler pic x(30) value spaces.
           03 soc-dat-cod pic x(5) value "SOCIO".
           03 filler  pic x(8) value space.
           03 soc-dat-imp pic x(7) value "IMPORTE".

       01  lin-dat-soc.
           03 filler pic x(31) value spaces.
           03 soc-cod pic 9(4).
           03 filler pic x(3) value spaces.
           03 soc-imp pic z.zzz.zzz.zz9,99.

       01  lin-guion-fin-soc.
           03 filler pic x(27) value spaces.
           03 filler pic x(12) value all "-".
           03 filler pic x(3) value spaces.
           03 filler pic x(12) value all "-".

       01  lin-tot-fil.
           03 filler pic x(20) value spaces.
           03 filler pic x(7) value "Total:".
           03 filler pic x(4) value spaces.
           03 fil-cant-trans pic zz9.
           03 filler pic x(4) value spaces.
           03 fil-cant-imp pic z.zzz.zzz.zz9,99.

       01  lin-titulo-gen.
           03 filler pic x(30) value space.
           03 filler pic x(20) value "ESTADISTICA GENERAL:".

       01  lin-tot-gen.
           03 filler pic x(25) value "En total se procesaron a".
           03 tot-trans pic zz9.
           03 filler pic x(7) value " socios".

       01  lin-tot-imp-gen.
           03 filler pic x(36) value "El importe total transferido fue"-
           " de".
           03 tot-imp pic zzz.zzz.zzz.zz9,99.
       01  lin-max-fil-gen.
           03 filler pic x(56) value "La filial que mas socios "-
           "operaron transferencias fue la".
           03 max-cod-fil pic 9.
           03 filler pic x(3) value " - ".
           03 max-nom-fil pic x(15).
           03 filler pic x(5) value " con".
           03 max-cont-soc pic zz9.
           03 filler pic x(7) value " socios".
      ***** Variables de trabajo***********************************************
       01  flagFilial pic 9.
       01  flagSocio pic 9.
      ***** Modulo Temático General *******************************************
       77  gen-tot-soc-porc pic 999.
       77  gen-tot-soc-imp pic 9(12)v99.
       77  gen-max-cod-fil pic 9.
       77  gen-max-nom-fil pic x(15).
       77  gen-max-cant-soc-fil pic 999.
      ***** Modulo Temático Filial  *******************************************
       77  fil-acum-importe pic 9(10)v99.
       77  fil-cant-soc-trans pic 999.
      ***** Modulo Temático Socio   *******************************************
       77  soc-cod-ant pic 9(4).
       77  soc-acum-importe pic 9(10)v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 230-LEER-FILIAL.
            PERFORM 330-LEER-SOCIO.
            PERFORM UNTIL flagFilial IS EQUAL 1
               PERFORM 200-INICIO-FILIAL
               PERFORM UNTIL flagSocio IS EQUALS 1 OR
                             fil-codigo IS NOT EQUALS soc-filial
                   PERFORM 300-INICIO-SOCIO
                   PERFORM UNTIL flagSocio IS EQUALS 1 OR
                                 fil-codigo IS NOT EQUALS soc-filial OR
                                 soc-socio IS NOT EQUALS soc-cod-ant
                       PERFORM 350-PROCESO-SOCIO
                       PERFORM 330-LEER-SOCIO
                   END-PERFORM
                   PERFORM 400-FIN-SOCIO
               END-PERFORM
               PERFORM 500-FIN-FILIAL
               PERFORM 230-LEER-FILIAL
            END-PERFORM.
            PERFORM 600-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           PERFORM 120-INICIALIZO-VARIABLES-GEN.
           PERFORM 125-ABRO-ARCHIVOS.
           PERFORM 140-MUESTRO-ENCABEZADO.

       120-INICIALIZO-VARIABLES-GEN.
           MOVE ZERO TO flagFilial.
           MOVE ZERO TO flagSocio.
      ******************************
           MOVE ZERO TO gen-tot-soc-porc.
           MOVE ZERO TO gen-tot-soc-imp.
           MOVE ZERO TO gen-max-cod-fil.
           MOVE " "  TO gen-max-nom-fil.
           MOVE ZERO TO gen-max-cant-soc-fil.

       125-ABRO-ARCHIVOS.
           OPEN INPUT FILIALES.
           OPEN INPUT SOCIOS.

       140-MUESTRO-ENCABEZADO.
           DISPLAY lin-titulo.
           DISPLAY lin-sub-titulo.
           DISPLAY lin-guion.

       200-INICIO-FILIAL.
           PERFORM 220-INICIALIZO-VARIABLES-FIL.
           PERFORM 240-MUESTRO-ENCABEZADO-FIL.

       220-INICIALIZO-VARIABLES-FIL.
           MOVE ZERO TO fil-acum-importe.
           MOVE ZERO TO fil-cant-soc-trans.

       230-LEER-FILIAL.
           READ FILIALES AT END MOVE 1 TO flagFilial.

       240-MUESTRO-ENCABEZADO-FIL.
           PERFORM 245-ARMO-LIN-TITULO-FILIAL.
           DISPLAY lin-titulo-filial.
           DISPLAY lin-titulo-soc.

       245-ARMO-LIN-TITULO-FILIAL.
           MOVE fil-codigo TO fil-dat-cod.
           MOVE fil-nombre TO fil-dat-nom.

       300-INICIO-SOCIO.
           MOVE soc-socio TO soc-cod-ant.
           MOVE ZERO TO soc-acum-importe.

       330-LEER-SOCIO.
           READ SOCIOS AT END MOVE 1 TO flagSocio.

       350-PROCESO-SOCIO.
           IF soc-modalidad IS EQUALS "T" THEN
               ADD soc-importe TO soc-acum-importe
           END-IF.

       400-FIN-SOCIO.
           IF soc-acum-importe IS NOT EQUALS ZERO
               PERFORM 440-MUESTRO-DATOS-SOCIO.
           PERFORM 460-PROCESO-FILIAL.

       440-MUESTRO-DATOS-SOCIO.
           PERFORM 445-ARMO-LIN-DAT-SOC.
           DISPLAY lin-dat-soc.

       445-ARMO-LIN-DAT-SOC.
           MOVE soc-cod-ant TO soc-cod.
           MOVE soc-acum-importe TO soc-imp.

       460-PROCESO-FILIAL.
           IF soc-acum-importe > ZERO THEN
               ADD 1 TO fil-cant-soc-trans
               ADD soc-acum-importe TO fil-acum-importe
           END-IF.

       500-FIN-FILIAL.
           PERFORM 560-PROCESO-GENERAL.
           PERFORM 540-MUESTRO-DATOS-FILIAL.

       540-MUESTRO-DATOS-FILIAL.
           PERFORM 545-ARMO-LIN-TOT-FIL.
           DISPLAY lin-guion-fin-soc.
           DISPLAY lin-tot-fil.

       545-ARMO-LIN-TOT-FIL.
           MOVE fil-acum-importe TO fil-cant-imp.
           MOVE fil-cant-soc-trans TO fil-cant-trans.

       560-PROCESO-GENERAL.
           ADD fil-cant-soc-trans TO gen-tot-soc-porc.
           ADD fil-acum-importe TO gen-tot-soc-imp.
           PERFORM 570-BUSCO-MAXIMO-FILIAL.

       570-BUSCO-MAXIMO-FILIAL.
           IF fil-cant-soc-trans > gen-max-cant-soc-fil THEN
               MOVE fil-cant-soc-trans TO gen-max-cant-soc-fil
               MOVE fil-codigo TO gen-max-cod-fil
               MOVE fil-nombre TO gen-max-nom-fil
           END-IF.

       600-FIN-GENERAL.
           PERFORM 625-CIERRO-ARCHIVOS.
           PERFORM 640-MUESTRO-ENCABEZADO-GEN.
           PERFORM 650-MUESTRO-DATOS-GENERAL.

       625-CIERRO-ARCHIVOS.
           CLOSE FILIALES.
           CLOSE SOCIOS.

       640-MUESTRO-ENCABEZADO-GEN.
           DISPLAY lin-titulo-gen.

       645-ARMO-LIN-TOT-GEN.
           MOVE gen-tot-soc-porc TO tot-trans.

       646-ARMO-LIN-TOT-IMP-GEN.
           MOVE gen-tot-soc-imp TO tot-imp.

       647-ARMO-LIN-MAX-FIL-GEN.
           MOVE gen-max-cod-fil TO max-cod-fil.
           MOVE gen-max-nom-fil TO max-nom-fil.
           MOVE gen-max-cant-soc-fil TO max-cont-soc.

       650-MUESTRO-DATOS-GENERAL.
           PERFORM 645-ARMO-LIN-TOT-GEN.
           PERFORM 646-ARMO-LIN-TOT-IMP-GEN.
           PERFORM 647-ARMO-LIN-MAX-FIL-GEN.
           DISPLAY lin-tot-gen.
           DISPLAY lin-tot-imp-gen.
           DISPLAY lin-max-fil-gen.
       END PROGRAM Ejercicio_Clase.
