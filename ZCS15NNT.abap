***********************************************************************
*        D A T E N  -  Definitionen                                   *
***********************************************************************
*---------------------------------------------------------------------*
*        ATAB-Tabellen                                                *
*---------------------------------------------------------------------*
TABLES:
*  Stuecklisten; globale Modifikationsparameter
   TCS03,
*  Stuecklisten; Benutzerprofile
   TCSPR.

type-pools:  "MBALV
   icon,                                                       "Acc 2004
   slis.



*---------------------------------------------------------------------*
*        interne Tabellen                                             *
*---------------------------------------------------------------------*
*     erweiterte (Verwendungs-)SEKI-Tabelle; Schnittstelle zum FB
DATA: BEGIN OF LTB OCCURS 0.
         INCLUDE STRUCTURE STPOV.
DATA: END OF LTB.

*     Ausnahmentabelle
DATA: BEGIN OF EXCPT OCCURS 0,
         STLAN LIKE MAST-STLAN,
         ZWERK LIKE MAST-WERKS,
         STLTY LIKE STKOB-STLTY,
         BGDOC LIKE DOST-DOKNR,                               "YHG110068
         BGEQU LIKE EQST-EQUNR,
         BGKND LIKE KDST-VBELN,                               "YHG110068
         BGMAT LIKE MAST-MATNR,
         BGPRJ LIKE PRST-PSPNR,                               "MBA089075
         BGSTD LIKE STST-STOBJ,                               "YHG110068
         BGTPL LIKE TPST-TPLNR,                               "YHG110068
         KTEXT LIKE STPOX-OJTXP,
         XTLAL LIKE STPOX-STLAL,
         AUSNM LIKE STPOX-AUSNM,
      END OF EXCPT.

*     Tabelle fuer Langtexte
DATA: BEGIN OF TXT OCCURS 25.
         INCLUDE STRUCTURE TLINE.
DATA: END OF TXT.

*     Objektkataloge fuer Verwendungs FBs
DATA: BEGIN OF DOCCAT   OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCDOC.                            "YHG110068
DATA: END OF DOCCAT  .                                        "YHG110068

DATA: BEGIN OF EQUICAT  OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCEQUI.                           "YHG110068
DATA: END OF EQUICAT .                                        "YHG110068

DATA: BEGIN OF KNDCAT   OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCKND.                            "YHG110068
DATA: END OF KNDCAT  .                                        "YHG110068

DATA: BEGIN OF MATCAT   OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCMAT.                            "YHG110068
DATA: END OF MATCAT  .                                        "YHG110068

DATA: BEGIN OF PRJCAT   OCCURS 0.                             "MBA089075
         INCLUDE STRUCTURE CSCPRJ.                            "MBA089075
DATA: END OF PRJCAT.                                          "MBA089075

DATA: BEGIN OF STDCAT   OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCSTD.                            "YHG110068
DATA: END OF STDCAT  .                                        "YHG110068

DATA: BEGIN OF TPLCAT   OCCURS 0.                             "YHG110068
         INCLUDE STRUCTURE CSCTPL.                            "YHG110068
DATA: END OF TPLCAT  .                                        "YHG110068

DATA: BEGIN OF INP_SEL OCCURS 20.  "Selektionsparameter       "YHG139715
         INCLUDE STRUCTURE RSPARAMS.                          "YHG139715
DATA: END OF INP_SEL.                                         "YHG139715

*d DATA: BEGIN OF TXT_SEL OCCURS 20,  "Selektionstexte     "YHG139715"uc
*d         ID(9),                                          "YHG139715"uc
*d         TEXT(40),                                       "YHG139715"uc
*d      END OF TXT_SEL.                                    "YHG139715"uc

DATA: BEGIN OF TXT_SEL OCCURS 20.  "Selektionstexte                  "uc
         include structure textpool.                                 "uc
data: END OF TXT_SEL.                                                "uc

ENHANCEMENT-SECTION     RCS15NNT_02 SPOTS ES_RCS15NNT STATIC INCLUDE BOUND .
DATA: BEGIN OF DSP_SEL OCCURS 20,                             "YHG139715
         TEXT(30),                                            "YHG139715
         FILLER(2) VALUE '_ ',                                "YHG139715
         WERT(32),                                            "YHG139715
      END OF DSP_SEL.                                         "YHG139715
END-ENHANCEMENT-SECTION.

*---------------------------------------------------------------------*
*        interne Feldleisten                                          *
*---------------------------------------------------------------------*
*     Schnittstelle zur Positionsanzeige
DATA: BEGIN OF CSIN.
         INCLUDE STRUCTURE CSIN.
DATA: END OF CSIN.


*---------------------------------------------------------------------*
*        interne Felder                                               *
*---------------------------------------------------------------------*
DATA: DSP_IMENG(7)  TYPE P DECIMALS 3,
      DSP_RMENG(7)  TYPE P DECIMALS 3,
      SUM_FACTOR    TYPE F,
      UNT_FACTOR    TYPE F,            "Faktor f. EinhUmrech. "HGA118294
      TMP_FACTOR    TYPE F,                                   "HGA115686
      ACT_EMFAC     TYPE F,                                   "HGA115915
      ACT_EXTRM     LIKE CSDATA-XFELD, "Extrem                "HGA118294
      PRV_EXTRM     LIKE CSDATA-XFELD, "Extrem                "HGA118294
      ACT_LDSGN     LIKE CSDATA-XFELD, "Leitvorzeichen        "HGA118294
      DSP_POSNR     LIKE STPO-POSNR,
      DSP_STLAL     LIKE STKO-STLAL,
      OPO_MEINH     LIKE STPO-MEINS,
      OPO_MENGE     LIKE STPO-MENGE,                          "HGA115686
      TXT_KEY       LIKE THEAD-TDNAME,
      TXT_KYLN(3)   TYPE N,
      COL_SWITCH(1) TYPE C,
      TMP_HRECHF    TYPE F,
      OJTOP_MRK     LIKE CSDATA-XFELD,
      PAGE_DOC      LIKE SY-PAGNO,                            "YHG110068
      PAGE_EQUI     LIKE SY-PAGNO,
      PAGE_KND      LIKE SY-PAGNO,                            "YHG110068
      PAGE_MAT      LIKE SY-PAGNO,
      PAGE_PRJ      LIKE SY-PAGNO,                            "MBA089075
      PAGE_STD      LIKE SY-PAGNO,                            "YHG110068
      PAGE_TPL      LIKE SY-PAGNO,                            "YHG110068
      PAGE_EXC      LIKE SY-PAGNO,
      OUTPT_FLG     LIKE CSDATA-XFELD,                        "YHG134257
      DOSTB_FLG     LIKE CSDATA-XFELD,                        "YHG110068
      EQSTB_FLG     LIKE CSDATA-XFELD,
      KDSTB_FLG     LIKE CSDATA-XFELD,                        "YHG110068
      MASTB_FLG     LIKE CSDATA-XFELD,
      PRSTB_FLG     LIKE CSDATA-XFELD,                        "MBA089075
      STSTB_FLG     LIKE CSDATA-XFELD,                        "YHG110068
      TPSTB_FLG     LIKE CSDATA-XFELD,                        "YHG110068
      EOLST_FLG     LIKE CSDATA-XFELD,
      LTBEX_FLG     LIKE CSDATA-XFELD,
      CONTI_FLG     LIKE CSDATA-XFELD,
      SKIPL_FLG     LIKE CSDATA-XFELD,
      EOPTH_FLG     LIKE CSDATA-XFELD,                        "HGA032266
      CLASP_FLG     LIKE CSDATA-XFELD, "KLassenpos. kennz.    "YHG000381
      UNTFC_FLG     LIKE CSDATA-XFELD,                        "HGA118294
      EXCPT_TABIX   LIKE SY-TABIX,
      AUTH_CNT      LIKE SY-SUBRC,
*  AnwendungsKlasse, die per CALL TRANSACTION ruft
      CAL_ACLAS     LIKE TAPPL-APPLCLASS,                     "HGD072824
      SV_UCOMM      LIKE SY-UCOMM,                          "note 351902
      RET_CODE      LIKE SY-SUBRC.
* ---------------------------------
DATA: LINS_OUT     LIKE SY-LINNO,
      LINS_OUT_SAV LIKE SY-LINNO,
      LST_LIN_ON_P LIKE SY-LINNO,
      LINS_TO_SKIP LIKE SY-LINNO,
      LINS_PER_PAG LIKE SY-PAGNO.
* ---------------------------------
*del DATA: EDTLIN(79) TYPE C,                                 "YHG108937
*del      SAV_EDTLIN(79) TYPE C.                              "YHG108937

DATA:                                                         "HGC072824
   CATTAKTIV(1) TYPE C.                                       "HGC072824

*---------------------------------------------------------------------*
*        Konstanten                                                   *
*---------------------------------------------------------------------*
DATA:
   BOM_TXOBJ LIKE THEAD-TDOBJECT VALUE 'BOM',
   BOM_TXID  LIKE THEAD-TDID VALUE 'MPO',
*  maximal anzeigbare Menge
   MAX_NUM(7)  TYPE P DECIMALS 3 VALUE '9999999999.999',

   TYP_DOC   LIKE STZU-STLTY VALUE 'D',                       "YHG110068
   TYP_EQUI  LIKE STZU-STLTY VALUE 'E',
   TYP_KND   LIKE STZU-STLTY VALUE 'K',                       "YHG110068
   TYP_MAT   LIKE STZU-STLTY VALUE 'M',
   TYP_PRJ   LIKE STZU-STLTY VALUE 'P',                       "MBA089075
   TYP_STD   LIKE STZU-STLTY VALUE 'S',                       "YHG110068
   TYP_TPL   LIKE STZU-STLTY VALUE 'T',                       "YHG110068

   MIN_GRG   LIKE SY-DATUM   VALUE '19000101',
   MAX_GRG   LIKE SY-DATUM   VALUE '99991231'.

DATA:                                                         "HGA118294
   MAX_FNUM     TYPE F VALUE '9.999999999999000E+09',         "HGA118294
   PNULL(13)    TYPE P         VALUE 0,                       "HGA118294
   FNULL        TYPE F VALUE '0.000000000000000E+00'.         "HGA118294

*  StlTypen, die zu bearbeiten sind
DATA: BEGIN OF CHK_TYPES,                                     "YHG110068
         DOC LIKE STZU-STLTY,                                 "YHG110068
         EQU LIKE STZU-STLTY,                                 "YHG110068
         KND LIKE STZU-STLTY,                                 "YHG110068
         MAT LIKE STZU-STLTY,                                 "YHG110068
         PRJ LIKE STZU-STLTY,                                 "MBA089075
         STD LIKE STZU-STLTY,                                 "YHG110068
         TPL LIKE STZU-STLTY,                                 "YHG110068
         RS1 LIKE STZU-STLTY,                                 "YHG110068
         RS2 LIKE STZU-STLTY,                                 "YHG110068
         RS3 LIKE STZU-STLTY,                                 "YHG110068
         RS4 LIKE STZU-STLTY,                                 "YHG110068
      END OF CHK_TYPES.                                       "YHG110068

DATA: STLTP_IN LIKE RC29A-ATABR.                            "note 308150

DATA:                                                         "YHG000381
   OTYP_MAT(1) TYPE C VALUE '1',                              "YHG000381
   OTYP_DOC(1) TYPE C VALUE '3',                              "YHG000381
   OTYP_KLA(1) TYPE C VALUE '4'.                              "YHG000381

DATA:                                                         "YHG079102
   QNT_ANY(3)  TYPE C VALUE '* *',                            "YHG079102
   QNT_NONE(3) TYPE C VALUE '- -'.                            "YHG079102


*---------------------------------------------------------------------*
*        Export-/Import-Tabellen                                      *
*---------------------------------------------------------------------*
*     Schnittstelle Report/Modulpool
DATA: BEGIN OF CSBOMEX.
         INCLUDE STRUCTURE CSBOMEX.
DATA: END OF CSBOMEX.

*     WAs fuer AnzBlockAusgabe (var. Liste)
DATA: BEGIN OF WATAB OCCURS 0.                                "YHG108937
         INCLUDE STRUCTURE CLTABLE.                           "YHG108937
DATA: END OF WATAB.                                           "YHG108937

*     Sicherungstabelle der AnzBlockWAs
DATA: BEGIN OF SAV_WATAB OCCURS 0.                            "YHG108937
         INCLUDE STRUCTURE CLTABLE.                           "YHG108937
DATA: END OF SAV_WATAB.                                       "YHG108937

*     Uebergabestruktur Typ STPOV (fuer LTB)
DATA: BEGIN OF LTB_ORIG.                                      "YHG109837
         INCLUDE STRUCTURE STPOV.                             "YHG109837
DATA: END OF LTB_ORIG.                                        "YHG109837

*     Uebergabestruktur Typ STPOL_ADD
DATA: BEGIN OF LTB_ADD.                                       "YHG109837
         INCLUDE STRUCTURE STPOL_ADD.                         "YHG109837
DATA: END OF LTB_ADD.                                         "YHG109837

* ---------------------------------
*     EndOfBlock-Kennzeichen
DATA: EOBLC      LIKE CSDATA-XFELD,                           "YHG109837
*     Zeilenbreite aus dem aktuellen Profil
      ITF_PRFSZ  LIKE KLAH-LBREI,                   "YHG109837"YHG032486
*del  SAV_PRFSZ  LIKE KLAH-LBREI,                   "YHG109837"YHG032486
      SAV_PRFSZ  TYPE I,                                      "YHG032486
*     Zeilenbreite des akt. Profils plus Rand ( + 2 )
*del  SIZ_LINPF  LIKE KLAH-LBREI,  "Lnsz plus frame "YHG109837"YHG032486
      SIZ_LINPF  TYPE I,                                      "YHG032486
*     Kennzeichen 'akt. Zeile ist leer'
      LNMPT_FLG  LIKE CSDATA-XFELD,                           "YHG109837
*     das aktuell gueltige Profil
      ACT_PROFIL LIKE KLAH-CLASS,                             "YHG109837
*     BlockzeilenZaehler
      BLCLNS_CNT LIKE SY-LINNO,                               "YHG109837
*     Anzahl Zeilen Listenkopf
      NBR_HDRLNS LIKE SY-LINNO.                               "YHG109837

* ---------------------------------
*     langes leeres Feld
DATA: ECFLD(250) TYPE C.                                      "YHG109837

DATA: l_lines TYPE i,                        "N_2639218
      l_line TYPE i,                         "N_2639218
      l_count type i,                        "N_2639218
      switch_values type ldmswitch.          "N_2639218


DATA: BEGIN OF index_tab OCCURS 0,           "N_2639218
      high_level LIKE stpov-level,           "N_2639218
      high_index LIKE sy-tabix,              "N_2639218
      END OF index_tab.                      "N_2639218
*---------------------------------------------------------------------*
*        Feldsymbole                                                  *
*---------------------------------------------------------------------*
*     Parametername
FIELD-SYMBOLS: <PM_NAME>.                                     "YHG139715

field-symbols:                                                       "uc
  <x_watab-table>  type x,                                           "uc
  <x_stpol_add_wa> type x,                                           "uc
  <x_stpov_wa>     type x,                                           "uc
  <x_mc29s_wa>     type x,                                           "uc
  <x_cstdoc_wa>    type x,                                           "uc
  <x_cstcla_wa>    type x.                                           "uc
*&---------------------------------------------------------------------*
