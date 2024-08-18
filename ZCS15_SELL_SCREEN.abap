*&---------------------------------------------------------------------*
*&  Include           ZCS15_SEL_SCREEN
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*        Selektionsparameter                                          *
*---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-117.
SELECT-OPTIONS: s_idnrk FOR mara-matnr OBLIGATORY NO INTERVALS.
PARAMETERS:
*  Material
*del PM_IDNRK LIKE STPO-IDNRK MEMORY ID MAT,                  "YHG078090
  pm_idnrk     LIKE stpo-idnrk MEMORY ID mat NO-DISPLAY.    "YHG078090
*  Werk
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-118.
PARAMETERS:
  pm_werks LIKE marc-werks MEMORY ID wrk,
*  Stuecklistenverwendung
  pm_stlan LIKE stzu-stlan,
  pm_postp LIKE stpo-postp.
*  Datum gueltig ab
SELECTION-SCREEN: END OF BLOCK b2.
*  direkte Verwendung
*del PM_DIRKT LIKE CSDATA-XFELD DEFAULT 'X',       "YHG000381 "YHG077295

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-120.
PARAMETERS: pm_datuv LIKE stpo-datuv DEFAULT sy-datum,
*  Datum gueltig bis
            pm_datub LIKE stpo-datuv DEFAULT sy-datum.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-119.
PARAMETERS:
  pm_dirkt LIKE csdata-xfeld,                               "YHG077295
*  Verwendung ueber Klassen
  pm_uebkl LIKE csdata-xfeld.                               "YHG000381
*  mehrstufig
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-121.
PARAMETERS:
  pm_mehrs     LIKE csdata-xfeld.                           "YHG125492
*  Einsatzmenge
SELECTION-SCREEN: END OF BLOCK b5.

SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE text-122.
PARAMETERS:
  pm_alvsa         LIKE rc29l-valst.                        "HGA246532
SELECTION-SCREEN:END OF BLOCK b6.

SELECTION-SCREEN: BEGIN OF BLOCK b7 WITH FRAME TITLE text-123.
PARAMETERS:
  pm_emeng     LIKE stpo-menge,
*  zugehoeriges SUBMIT-Hilfsfeld:
  pm_hemng(13) TYPE n NO-DISPLAY,
*  Ergebnismenge
  pm_rmeng     LIKE stpo-menge.
*  zugehoeriges SUBMIT-Hilfsfeld:
SELECTION-SCREEN: END OF BLOCK b7.

SELECTION-SCREEN: BEGIN OF BLOCK b8 WITH FRAME TITLE text-124.
PARAMETERS:
  pm_hrmng(13) TYPE n NO-DISPLAY,
*  Profil zur Bildschirmanzeige
  pm_dsprf     LIKE klah-class NO-DISPLAY,                  "YHG109837
*  Profil beim Druck
  pm_prprf     LIKE klah-class NO-DISPLAY.                  "YHG109837
*  Einschraenken auf Positionsty
*  Positionslangtext anzeigen
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-125 FOR FIELD pm_ltext.
SELECTION-SCREEN POSITION 11.
PARAMETERS:
  pm_ltext LIKE csdata-xfeld.
*  Gueltigkeitsbereich anzeigen
SELECTION-SCREEN COMMENT 17(10) text-126 FOR FIELD pm_gbraz.
SELECTION-SCREEN POSITION 28.
PARAMETERS: pm_gbraz LIKE csdata-xfeld.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK b8.

SELECTION-SCREEN: BEGIN OF BLOCK b9 WITH FRAME TITLE text-127.
PARAMETERS:
*  EquiStueckliste
  pm_equtp LIKE csdata-xfeld,                               "YHG110068
*  KndAuftrStueckliste
  pm_kndtp LIKE csdata-xfeld,                               "MB075252
*  ProjektStueckliste
  pm_prjtp LIKE csdata-xfeld,                               "MBA089075
*  Materialstueckliste
  pm_mattp LIKE csdata-xfeld DEFAULT 'X',                   "YHG110068
*  Standardstueckliste
  pm_stdtp LIKE csdata-xfeld,                               "YHG110068
*  TechPlStueckliste
  pm_tpltp LIKE csdata-xfeld.                               "YHG110068
SELECTION-SCREEN: END OF BLOCK b9.
*  zukuenftig als Parameter definieren
