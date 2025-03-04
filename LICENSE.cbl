      *****************************************************************
      * LICENSE.CBL - Licence du projet COBOLegend
      *
      * Ce fichier contient la licence MIT formatée en tant que programme
      * COBOL. Il n'est pas destiné à être exécuté, mais sert de
      * déclaration de licence pour le projet dans un format authentique.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIT-LICENSE.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  LICENSE-METADATA.
           05  LICENSE-NAME        PIC X(20) VALUE "MIT License".
           05  LICENSE-YEAR        PIC 9(4)  VALUE 2025.
           05  COPYRIGHT-HOLDER    PIC X(30) VALUE "COBOLegend Project".
      
       01  LICENSE-TEXT.
           05  LICENSE-PARAGRAPH-1  PIC X(80) VALUE
               "Permission is hereby granted, free of charge, to any ".
           05  LICENSE-PARAGRAPH-2  PIC X(80) VALUE
               "person obtaining a copy of this software and associated ".
           05  LICENSE-PARAGRAPH-3  PIC X(80) VALUE
               "documentation files (the 'Software'), to deal in the ".
           05  LICENSE-PARAGRAPH-4  PIC X(80) VALUE
               "Software without restriction, including without limitation ".
           05  LICENSE-PARAGRAPH-5  PIC X(80) VALUE
               "the rights to use, copy, modify, merge, publish, distribute, ".
           05  LICENSE-PARAGRAPH-6  PIC X(80) VALUE
               "sublicense, and/or sell copies of the Software, and to ".
           05  LICENSE-PARAGRAPH-7  PIC X(80) VALUE
               "permit persons to whom the Software is furnished to do so, ".
           05  LICENSE-PARAGRAPH-8  PIC X(80) VALUE
               "subject to the following conditions:".
      
           05  LICENSE-PARAGRAPH-9  PIC X(80) VALUE
               "The above copyright notice and this permission notice shall be ".
           05  LICENSE-PARAGRAPH-10 PIC X(80) VALUE
               "included in all copies or substantial portions of the Software.".
      
           05  LICENSE-PARAGRAPH-11 PIC X(80) VALUE
               "THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, ".
           05  LICENSE-PARAGRAPH-12 PIC X(80) VALUE
               "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES ".
           05  LICENSE-PARAGRAPH-13 PIC X(80) VALUE
               "OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ".
           05  LICENSE-PARAGRAPH-14 PIC X(80) VALUE
               "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT ".
           05  LICENSE-PARAGRAPH-15 PIC X(80) VALUE
               "HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, ".
           05  LICENSE-PARAGRAPH-16 PIC X(80) VALUE
               "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING ".
           05  LICENSE-PARAGRAPH-17 PIC X(80) VALUE
               "FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR ".
           05  LICENSE-PARAGRAPH-18 PIC X(80) VALUE
               "OTHER DEALINGS IN THE SOFTWARE.".
      
       PROCEDURE DIVISION.
       DISPLAY-LICENSE.
           DISPLAY SPACE.
           DISPLAY LICENSE-NAME.
           DISPLAY SPACE.
           DISPLAY "Copyright (C) " LICENSE-YEAR " " COPYRIGHT-HOLDER.
           DISPLAY SPACE.
           DISPLAY LICENSE-PARAGRAPH-1.
           DISPLAY LICENSE-PARAGRAPH-2.
           DISPLAY LICENSE-PARAGRAPH-3.
           DISPLAY LICENSE-PARAGRAPH-4.
           DISPLAY LICENSE-PARAGRAPH-5.
           DISPLAY LICENSE-PARAGRAPH-6.
           DISPLAY LICENSE-PARAGRAPH-7.
           DISPLAY LICENSE-PARAGRAPH-8.
           DISPLAY SPACE.
           DISPLAY LICENSE-PARAGRAPH-9.
           DISPLAY LICENSE-PARAGRAPH-10.
           DISPLAY SPACE.
           DISPLAY LICENSE-PARAGRAPH-11.
           DISPLAY LICENSE-PARAGRAPH-12.
           DISPLAY LICENSE-PARAGRAPH-13.
           DISPLAY LICENSE-PARAGRAPH-14.
           DISPLAY LICENSE-PARAGRAPH-15.
           DISPLAY LICENSE-PARAGRAPH-16.
           DISPLAY LICENSE-PARAGRAPH-17.
           DISPLAY LICENSE-PARAGRAPH-18.
           DISPLAY SPACE.
           STOP RUN.
       
       END PROGRAM MIT-LICENSE.
