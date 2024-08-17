REPORT rcs15001 MESSAGE-ID 29 LINE-SIZE 132
                NO STANDARD PAGE HEADING.
*--> Author Harsh Sharma
***********************************************************************
*        D A T E N  -  Definitionen                                   *
***********************************************************************
INCLUDE zcs15nnt.

*INCLUDE rcs15nnt.                                             "YHG000352

*---------------------------------------------------------------------*
*        ATAB-Tabellen                                                *
*---------------------------------------------------------------------*
TABLES:                                                     "YHG087082
*  Positionstypen
  t418,                                                     "YHG022170
*  Aenderungsdienst; Steuerungsdaten
  tcc08,
  mara.                                                     "YHG087082
DATA: lt_ltb TYPE STANDARD TABLE OF stpov.
DATA: tmat_revlv LIKE aeoi-revlv.                           "YHG083168
DATA: revl_sdatu LIKE sy-datum.                             "YHG083168
DATA: wu_ctab_loopx LIKE sy-tabix.                          "YHG125492
DATA: stack_prevx LIKE sy-tabix.                            "YHG125492
DATA: ltb_loopx LIKE sy-tabix.                              "YHG125492
DATA: ltb_strtx LIKE sy-tabix.                              "YHG125492
DATA: act_level LIKE stpov-level.                           "YHG125492
DATA: stack_flag LIKE csdata-xfeld.                         "YHG125492
DATA: pop_flag LIKE csdata-xfeld.                           "YHG125492
DATA: end_flag LIKE csdata-xfeld.                           "YHG125492
*d DATA: SAV_RMENG LIKE STPO-MENGE.                 "YHG125492"HGA131954
DATA: sav_frmng TYPE f.                                     "HGA131954

*     Loop-LTB (mehrstufig)                                   "YHG125492
DATA: BEGIN OF lltb OCCURS 0.                                 "YHG125492
        INCLUDE STRUCTURE stpov.                             "YHG125492
      DATA: END OF lltb.                                    "YHG125492

*     Loop-MATCAT (mehrstufig)                                "YHG125492
DATA: BEGIN OF lmatcat OCCURS 0.                              "YHG125492
        INCLUDE STRUCTURE cscmat.                            "YHG125492
      DATA: END OF lmatcat  .                               "YHG125492

DATA: BEGIN OF wu_ctab OCCURS 0,                            "YHG125492
        matnr LIKE mara-matnr,                              "YHG125492
        werks LIKE t001w-werks,                             "YHG125492
        stlan LIKE mast-stlan,                              "YHG125492
        wuchk LIKE csdata-xfeld, "Vw zu ermitteln versucht   "YHG125492
        isusd LIKE csdata-xfeld, "Vw gefunden                "YHG125492
        wutck LIKE csdata-xfeld, "Vw-Tab geprueft            "YHG125492
      END OF wu_ctab.                                       "YHG125492

DATA: BEGIN OF wu_ctab_key,                                 "YHG125492
        matnr LIKE mara-matnr,                              "YHG125492
        werks LIKE t001w-werks,                             "YHG125492
        stlan LIKE mast-stlan,                              "YHG125492
      END OF wu_ctab_key.                                   "YHG125492

DATA: BEGIN OF wu_ml_stack OCCURS 0,                        "YHG125492
        stufe LIKE stpov-level,                             "YHG125492
        loopx LIKE sy-index,                                "YHG125492
        matnr LIKE mara-matnr,                              "YHG125492
        werks LIKE t001w-werks,                             "YHG125492
        stlan LIKE mast-stlan,                              "YHG125492
      END OF wu_ml_stack.                                   "YHG125492

DATA: BEGIN OF mng_stack OCCURS 0,                          "YHG125492
        stufe LIKE stpov-level,                             "YHG125492
        emeng LIKE stpov-emeng,                             "YHG125492
        rmeng LIKE stpov-emeng,                             "YHG125492
        emfac TYPE f,                                       "HGA115915
        ldsgn LIKE csdata-xfeld,                            "HGA118294
        extrm LIKE csdata-xfeld,                            "HGA118294
      END OF mng_stack.                                     "YHG125492


*---------------------------------------------------------------------*
*        interne Feldleisten                                          *
*---------------------------------------------------------------------*
*     Materialdaten zum Einstiegsmaterial
DATA: BEGIN OF selpool.
        INCLUDE STRUCTURE mc29s.
      DATA: END OF selpool.

*     Materialdaten zum Einstiegsmaterial merken (mehrstufig)
DATA: BEGIN OF sav_selpool.                                   "YHG125492
        INCLUDE STRUCTURE mc29s.                             "YHG125492
      DATA: END OF sav_selpool.                             "YHG125492

*     Uebergabestruktur var. Liste
*d DATA: BEGIN OF TOPMAT.                           "YHG110068"YHG019433
*d          INCLUDE STRUCTURE CSTMAT.               "YHG110068"YHG019433
*d DATA: END OF TOPMAT.                             "YHG110068"YHG019433

DATA: BEGIN OF wu_memid,                                    "YHG125492
        tabid(2) TYPE c,                                    "YHG125492
        matnr    LIKE mara-matnr,                           "YHG125492
        werks    LIKE t001w-werks,                          "YHG125492
        stlan    LIKE mast-stlan,                           "YHG125492
      END OF wu_memid.                                      "YHG125492


*---------------------------------------------------------------------*
*        Selektionsparameter                                          *
*---------------------------------------------------------------------*
SELECT-OPTIONS: s_idnrk FOR mara-matnr OBLIGATORY NO INTERVALS.
PARAMETERS:
*  Material
*del PM_IDNRK LIKE STPO-IDNRK MEMORY ID MAT,                  "YHG078090
  pm_idnrk     LIKE stpo-idnrk MEMORY ID mat NO-DISPLAY,    "YHG078090
*  Werk
  pm_werks     LIKE marc-werks MEMORY ID wrk,
*  Stuecklistenverwendung
  pm_stlan     LIKE stzu-stlan,
*  Datum gueltig ab
  pm_datuv     LIKE stpo-datuv DEFAULT sy-datum,
*  Datum gueltig bis
  pm_datub     LIKE stpo-datuv DEFAULT sy-datum,
*  direkte Verwendung
*del PM_DIRKT LIKE CSDATA-XFELD DEFAULT 'X',       "YHG000381 "YHG077295
  pm_dirkt     LIKE csdata-xfeld,                           "YHG077295
*  Verwendung ueber Klassen
  pm_uebkl     LIKE csdata-xfeld,                           "YHG000381
*  mehrstufig
  pm_mehrs     LIKE csdata-xfeld,                           "YHG125492
*  Einsatzmenge
  pm_emeng     LIKE stpo-menge,
*  zugehoeriges SUBMIT-Hilfsfeld:
  pm_hemng(13) TYPE n NO-DISPLAY,
*  Ergebnismenge
  pm_rmeng     LIKE stpo-menge,
*  zugehoeriges SUBMIT-Hilfsfeld:
  pm_hrmng(13) TYPE n NO-DISPLAY,
  pm_alvsa     LIKE rc29l-valst,                            "HGA246532
*  Profil zur Bildschirmanzeige
  pm_dsprf     LIKE klah-class DEFAULT 'SAPCSWSDMP01      ', "YHG109837
*  Profil beim Druck
  pm_prprf     LIKE klah-class DEFAULT 'SAPCSWSDMP01      ', "YHG109837
*  Einschraenken auf Positionstyp
  pm_postp     LIKE stpo-postp,
*  Positionslangtext anzeigen
  pm_ltext     LIKE csdata-xfeld,
*  Gueltigkeitsbereich anzeigen
  pm_gbraz     LIKE csdata-xfeld.

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

*  zukuenftig als Parameter definieren
DATA:                                                       "YHG110068
*  Dokumentstueckliste
   pm_doctp LIKE csdata-xfeld.                              "YHG110068
*  KndAuftrStueckliste
*d pm_kndtp like csdata-xfeld.                       "YHG110068 MB075252
*  Projektstückliste                                          "MBA089075
*  pm_prjtp LIKE csdata-xfeld.                                "MBA089075

*  reporteigene Konstanten
DATA:
  list_id    LIKE klah-class VALUE 'SAPCSWSDM         ',    "YHG108937
*  Profil zur Bildschirmanzeige
  dflt_dsprf LIKE klah-class VALUE 'SAPCSWSDMP01      ',    "YHG135858
*  Profil beim Druck
  dflt_prprf LIKE klah-class VALUE 'SAPCSWSDMP01      '.    "YHG135858

DATA:
   cng_level LIKE stpov-level.                              "HGB154389

DATA:
   lst_bmein LIKE stko-bmein.                               "HGA013934

*---------------------------------------------------------------------*
*.. ALV_S  beg .............................................. "HGA246532

TYPE-POOLS: slis.
*..................................
DATA:
  report_name    LIKE sy-repid,
  alvlo_ltb      TYPE slis_layout_alv,
  alvvr          LIKE disvariant,
  alvvr_sav      TYPE c,
  exit_by_caller TYPE c,
  exit_by_user   TYPE slis_exit_by_user.

DATA:
   wa_ltb_fields_tb TYPE slis_fieldcat_alv.

DATA:
*  ALV Events complete
  alv_evnt_tb_cmpl TYPE slis_t_event,
*  ALV Events pf exit only
  alv_evnt_tb_pfxt TYPE slis_t_event,
*  ALV Top of page table
  alv_top_tb       TYPE slis_t_listheader,
*  field display properties  stb tab
  ltb_fields_tb    TYPE slis_t_fieldcat_alv.

DATA:
  alvvr_sav_all    TYPE c VALUE 'A',
  alvvr_sav_no_usr TYPE c VALUE 'X'.

DATA: BEGIN OF alv_ltb OCCURS 0.
        INCLUDE STRUCTURE stpov_alv.
        DATA:   info(3) TYPE c,
      END OF alv_ltb.

DATA: BEGIN OF ftab OCCURS 200.
        INCLUDE STRUCTURE dfies.
      DATA: END   OF ftab.

DATA: BEGIN OF xcpt_tb OCCURS 10,
        dobjt(42) TYPE c,
        ojtxp(40) TYPE c,
        info(3)   TYPE c,
      END OF xcpt_tb.

DATA:
*  ALV-Variante
  pm_alvvr LIKE ltdx-variant,
*  alv variant user specific
  pm_alvvu TYPE c.


*.. ALV_S  end .......................................................*
*---------------------------------------------------------------------*


*eject
***********************************************************************
*        M A I N  -  Routinen                                         *
***********************************************************************
*eject
INITIALIZATION.
*  CUA-Titel E01 setzen
  SET TITLEBAR 'E01' WITH text-001.

*  Schnittstelle zum Modulpool einlesen
  IMPORT csbomex FROM MEMORY ID 'CSNN_BOMEX'.
*  Anwendungsklasse (falls Transaktionscall) sichern
  cal_aclas = csbomex-aclas.                                "HGD072824

*  CATT-Info besorgen
  PERFORM import_catt_flag.                                 "HGC072824

*  Stuecklistenmodifikationsparameter einlesen
  PERFORM tcs03_lesen.

*  Stuecklistenbenutzerprofile einlesen
  PERFORM tcspr_lesen.

*  Kz Revisionsstand aktiv besorgen
  PERFORM tcc08_lesen.                                      "YHG087082

*  ?Report per SUBMIT gestartet
*  nein
  IF csbomex-submf IS INITIAL.
*     Schalter aus TCSPR setzen
    PERFORM set_schalter.                                   "YHG077295

*     Datumsdefault setzen
    PERFORM sel_grenzen_01.

*     StlVerwendungsdefault (GPA) setzen
    PERFORM sel_grenzen_02.

    IF NOT pm_alvsa IS INITIAL.                             "HGA246532
      PERFORM get_profs.                                    "HGA025100
    ENDIF.                                                  "HGA246532
  ENDIF.


*eject
* ---------------------------------
AT SELECTION-SCREEN.                                        "YHG078090
*  ?Report per SUBMIT gestartet
*  nein, ... per SE38 oder sonstwie
  IF csbomex-submf IS INITIAL.                              "YHG078090
*     ?weder direkte Verw. noch Verw. ueber Klassen
*     ja, weder - noch
    IF     pm_dirkt IS INITIAL                              "YHG078090
       AND pm_uebkl IS INITIAL.                             "YHG078090

*        ?Batchverarbeitung aktiv
*        nein
      IF     sy-batch IS INITIAL.                           "YHG078090
*           Msg.: wenigstens eine Art der Verw. auswaehlen
        MESSAGE e544 WITH ''.                               "YHG078090
*        ja, Report laeuft im Moment im Batch
      ELSE.                                                 "YHG078090
*           Report abbrechen - Selektion macht keinen Sinn
        LEAVE.                                              "YHG078090
      ENDIF.                                                "YHG078090
    ENDIF.                                                  "YHG078090

*     ?sowohl Einsatz- als auch Ergebnismenge angegeben
*     ja, beides
    IF     NOT pm_emeng IS INITIAL                          "YHG078090
       AND NOT pm_rmeng IS INITIAL.                         "YHG078090

*        ?Batchverarbeitung aktiv
*        nein
      IF     sy-batch IS INITIAL.                           "YHG078090
*           Msg.: nur Einsatz- od. nur Ergebnismenge eingeben
        MESSAGE e512 WITH ''.                               "YHG078090
*        ja, Report laeuft im Moment im Batch
      ELSE.                                                 "YHG078090
*           Report abbrechen - Selektion macht keinen Sinn
        LEAVE.                                              "YHG078090
      ENDIF.                                                "YHG078090
    ENDIF.                                                  "YHG078090
  ENDIF.                                                    "YHG078090


*eject
* ---------------------------------
START-OF-SELECTION.
*  ?Report per SUBMIT gestartet
*  nein, ... per SE38 oder sonstwie
  IF csbomex-submf IS INITIAL.
*     Parameterarea mit Stuecklistenverwendung versorgen
    SET PARAMETER ID 'CSV' FIELD pm_stlan.
  ENDIF.

  IF pm_alvsa IS INITIAL.                                   "HGA246532
    report_name = sy-repid.                                 "HGA246532

    alvlo_ltb-detail_popup = 'X'.                           "HGA246532
    alvlo_ltb-zebra        = 'X'.                           "HGA246532

    PERFORM alv_evnt_tb_prep                                "HGA246532
      USING                                                 "HGA246532
        'A'                                                 "HGA246532
        alv_evnt_tb_cmpl.                                   "HGA246532

    alvvr-report = report_name.                             "HGA246532
    IF NOT pm_alvvr IS INITIAL.                             "HGA246532
      alvvr-variant = pm_alvvr.                             "HGA246532

      IF NOT pm_alvvu IS INITIAL.                           "HGA246532
        alvvr-username = sy-uname.                          "HGA246532
      ENDIF.                                                "HGA246532
    ENDIF.                                                  "HGA246532

    alvvr_sav = alvvr_sav_all.                              "HGA246532
  ELSE.                                                     "HGA246532
    IF    pm_dsprf IS INITIAL                               "HGA246532
       OR pm_prprf IS INITIAL.                              "HGA246532

      PERFORM get_profs.                                    "HGA246532
    ENDIF.                                                  "HGA246532

*     Status SA15 setzen
    SET PF-STATUS 'SA15'.
*     Materialverwendung einstufig
    SET TITLEBAR 'A01'.
  ENDIF.                                                    "HGA246532

  PERFORM prep_chk_types.                                   "YHG110068
  PERFORM prep_multilv.                                     "YHG125492

*  sicherheitshalber MatMusseingabe pruefen
*  CHECK NOT pm_idnrk IS INITIAL.

*  numerische Eingaben anpassen
  PERFORM fit_input_01.

*  Revisionslevel-Suchdatum festlegen
  PERFORM get_revl_sdatu.                                   "YHG083168
  REFRESH ltb[].
  LOOP AT s_idnrk.
    pm_idnrk = s_idnrk-low.
    REFRESH lt_ltb[].
    PERFORM get_wu_recs                                     "YHG125492
       USING pm_idnrk                                       "YHG125492
             pm_werks                                       "YHG125492
             pm_stlan.                                      "YHG125492
  ENDLOOP.

*  ErrCheck
  CASE sy-subrc.
*     Material ist nicht vorhanden
    WHEN 2.
      CLEAR: csbomex.                                       "YHG000357
      csbomex-retcd = 4.                                    "YHG000357
      csbomex-msgno = '500'.                                "YHG000357
      csbomex-mvar1 = 'E:'.                                 "YHG000357
      WRITE pm_idnrk TO csbomex-mvar2.                      "YHG000357
      EXPORT csbomex TO MEMORY ID 'CSNN_BOMEX'.             "YHG000357

      MESSAGE s500 WITH 'E: ' pm_idnrk.
      IF sy-tcode = 'CS15'.                                 "N_2724546
        LEAVE.
      ELSE.                                                 "N_2724546
        EXIT.                                               "N_2724546
      ENDIF.                                                "N_2724546

*     Material wird nicht verwendet
    WHEN 3.
      CLEAR: csbomex.                                       "YHG000357
      csbomex-retcd = 12.                                   "YHG000357
      csbomex-msgno = '510'.                                "YHG000357
      csbomex-mvar1 = 'E:'.                                 "YHG000357
      WRITE pm_idnrk TO csbomex-mvar2.                      "YHG000357
      EXPORT csbomex TO MEMORY ID 'CSNN_BOMEX'.             "YHG000357

      MESSAGE s510 WITH 'E: ' pm_idnrk.
      IF sy-tcode = 'CS15'.                                 "N_2724546
        LEAVE.
      ELSE.                                                 "N_2724546
        EXIT.                                               "N_2724546
      ENDIF.                                                "N_2724546

*     keine Verwendung selektiert
    WHEN 4.
      CLEAR: csbomex.                                       "YHG000357
      csbomex-retcd = 8.                                    "YHG000357
      csbomex-msgno = '507'.                                "YHG000357
      csbomex-mvar1 = 'E:'.                                 "YHG000357
      WRITE pm_idnrk TO csbomex-mvar2.                      "YHG000357
      EXPORT csbomex TO MEMORY ID 'CSNN_BOMEX'.             "YHG000357

      MESSAGE s507 WITH 'E: ' pm_idnrk.
      IF sy-tcode = 'CS15'.                                 "N_2724546
        LEAVE.
      ELSE.                                                 "N_2724546
        EXIT.                                               "N_2724546
      ENDIF.                                                "N_2724546

*     keine Verwendung im eingegebenen Gueltigkeitsbereich
    WHEN 5.
      CLEAR: csbomex.                                       "YHG000357
      csbomex-retcd = 16.                                   "YHG000357
      csbomex-msgno = '515'.                                "YHG000357
      csbomex-mvar1 = 'E:'.                                 "YHG000357
      WRITE pm_idnrk TO csbomex-mvar2.                      "YHG000357
      EXPORT csbomex TO MEMORY ID 'CSNN_BOMEX'.             "YHG000357

      MESSAGE s515 WITH 'E: ' pm_idnrk.
      IF sy-tcode = 'CS15'.                                 "N_2724546
        LEAVE.
      ELSE.                                                 "N_2724546
        EXIT.                                               "N_2724546
      ENDIF.                                                "N_2724546
  ENDCASE.

  READ TABLE ltb INDEX 1.
*  wenn Tab. LTB nicht leer ist
  IF sy-subrc = 0 .
    IF NOT pm_mehrs IS INITIAL.
*        Daten zum Einstiegsmaterial sichern
      sav_selpool = selpool.

*        LTB und MATCAT exportieren;
      PERFORM exp_wutab
         USING pm_idnrk
               pm_werks
               pm_stlan.

*        Verwendungsergebnis registrieren
      PERFORM reg_wures
         USING pm_idnrk
               pm_werks
               pm_stlan
               'x' 'x' ' '.

      LOOP AT wu_ctab.
        wu_ctab_loopx = sy-tabix.
*              ... hat Verwendung
        CHECK NOT wu_ctab-isusd IS INITIAL.
*              ... zugeh. Verw.-Tab. noch nicht ueberprueft
        CHECK wu_ctab-wutck IS INITIAL.

        wu_memid-tabid = 'LT'.
        wu_memid-matnr = wu_ctab-matnr.
        wu_memid-werks = wu_ctab-werks.
*d          WU_MEMID-STLAN = WU_CTAB-STLAN.                   "HGA114476
        wu_memid-stlan = pm_stlan.                          "HGA114476

        CLEAR: lltb. REFRESH: lltb.
        IMPORT ltb TO lltb
           FROM MEMORY ID wu_memid.

        wu_memid-tabid = 'MT'.
        CLEAR: lmatcat. REFRESH: lmatcat.
        IMPORT matcat TO lmatcat
           FROM MEMORY ID wu_memid.

*d          LOOP AT LLTB.                                     "HGC154389
*d             CHECK LLTB-BMTYP = TYP_MAT.                    "HGC154389

*d             CHECK LLTB-REKRI IS INITIAL                    "HGC154389
*d                   AND LLTB-REKRS IS INITIAL.               "HGC154389
        LOOP AT lltb                                        "HGC154389
           WHERE     bmtyp EQ typ_mat                       "HGC154389
                 AND rekri IS INITIAL                       "HGC154389
*d                   AND rekrs IS INITIAL.          "HGC154389 MBA148624
                 AND rekrs IS INITIAL           "HGC154389 MBA148624
                 AND kzkup IS INITIAL                       "MBC167558
                 AND excpt NE 'CONV'.                       "MBA148624

          READ TABLE lmatcat INDEX lltb-ttidx.

          wu_ctab_key-matnr = lmatcat-matnr.
          wu_ctab_key-werks = lltb-werks.
*d             WU_CTAB_KEY-STLAN = LLTB-STLAN.                "HGA114476
          wu_ctab_key-stlan = pm_stlan.                     "HGA114476
          READ TABLE wu_ctab WITH KEY wu_ctab_key.
          CHECK sy-subrc <> 0.

          PERFORM get_wu_recs
             USING lmatcat-matnr
                   lltb-werks
*d                      LLTB-STLAN.                           "HGA114476
                   pm_stlan.                                "HGA114476

          IF sy-subrc = 0.
*                 LTB und MATCAT exportieren;
            PERFORM exp_wutab
               USING lmatcat-matnr
                     lltb-werks
*d                         LLTB-STLAN.                        "HGA114476
                     pm_stlan.                              "HGA114476

*                 Verwendungsergebnis registrieren
            PERFORM reg_wures
               USING lmatcat-matnr
                     lltb-werks
*d                         LLTB-STLAN                         "HGA114476
                     pm_stlan                               "HGA114476
                     'x' 'x' ' '.
          ELSE.
*                 Verwendungsergebnis registrieren
            PERFORM reg_wures
               USING lmatcat-matnr
                     lltb-werks
*d                         LLTB-STLAN                         "HGA114476
                     pm_stlan                               "HGA114476
                     'x' ' ' ' '.
          ENDIF.
        ENDLOOP.

        READ TABLE wu_ctab INDEX wu_ctab_loopx.
        wu_ctab-wutck = 'x'.
        MODIFY wu_ctab.
      ENDLOOP.

      selpool = sav_selpool.
      CLEAR: ltb. REFRESH: ltb.
      CLEAR: matcat. REFRESH: matcat.

      READ TABLE wu_ctab INDEX 1.
      wu_memid-tabid = 'LT'.
      wu_memid-matnr = wu_ctab-matnr.
      wu_memid-werks = wu_ctab-werks.
      wu_memid-stlan = wu_ctab-stlan.

      CLEAR: lltb. REFRESH: lltb.
      IMPORT ltb TO lltb
         FROM MEMORY ID wu_memid.

      wu_memid-tabid = 'MT'.
      CLEAR: lmatcat. REFRESH: lmatcat.
      IMPORT matcat TO lmatcat
         FROM MEMORY ID wu_memid.

      act_level = 1 .
      wu_ml_stack-stufe = act_level.
      wu_ml_stack-loopx = 1 .
      wu_ml_stack-matnr = wu_ctab-matnr.
      wu_ml_stack-werks = wu_ctab-werks.
      wu_ml_stack-stlan = wu_ctab-stlan.
      APPEND wu_ml_stack.
      ltb_strtx = 1 .

      WHILE end_flag IS INITIAL.
*d          LOOP AT LLTB FROM LTB_STRTX.                      "HGC154389
*d             CHECK LLTB-BMTYP = TYP_MAT.                    "HGC154389
        LOOP AT lltb FROM ltb_strtx                         "HGC154389
           WHERE bmtyp EQ typ_mat.                          "HGC154389

          ltb_loopx = sy-tabix.

          IF     pop_flag IS INITIAL
             OR  lltb-sumfg EQ 'x'.

            READ TABLE lmatcat INDEX lltb-ttidx.
            matcat = lmatcat. APPEND matcat.

            lltb-ttidx = sy-tabix.
            lltb-level = act_level.
            ltb = lltb.
            APPEND ltb.

            CHECK lltb-sumfg NE 'x'.
            CHECK lltb-rekri IS INITIAL
               AND lltb-excpt NE 'CONV'                     "MBB167558
               AND lltb-kzkup IS INITIAL              "note 593874
               AND lltb-rekrs IS INITIAL.


            wu_ctab_key-matnr = lmatcat-matnr.
            wu_ctab_key-werks = lltb-werks.
*d                WU_CTAB_KEY-STLAN = LLTB-STLAN.             "HGA114476
            wu_ctab_key-stlan = pm_stlan.                   "HGA114476
            READ TABLE wu_ctab WITH KEY wu_ctab_key.

            IF NOT wu_ctab-isusd IS INITIAL.
              wu_ml_stack-stufe = act_level + 1 .
              wu_ml_stack-loopx = 1 .
              wu_ml_stack-matnr = lmatcat-matnr.
              wu_ml_stack-werks = lltb-werks.
*d                   WU_ML_STACK-STLAN = LLTB-STLAN.          "HGA114476
              wu_ml_stack-stlan = pm_stlan.                 "HGA114476
              APPEND wu_ml_stack.
              stack_prevx = sy-tabix - 1 .

              pop_flag = 'x'.
            ENDIF.
          ELSE.
            READ TABLE wu_ml_stack
               WITH KEY act_level.
            wu_ml_stack-loopx = ltb_loopx.
            MODIFY wu_ml_stack INDEX sy-tabix.

            stack_flag = 'x'.

            EXIT.
          ENDIF.
        ENDLOOP.

        IF NOT pop_flag IS INITIAL.
          CLEAR: pop_flag.

*              ?war LLTB-Loop zuende
*              ja
          IF stack_flag IS INITIAL.
*                 vorletzten Stack-Satz einlesen
            READ TABLE wu_ml_stack INDEX stack_prevx.
*                 Anzahl Saetze der LLTB ermitteln
            DESCRIBE TABLE lltb LINES sy-tabix.
*                 Index um 1 hoeher setzen, damit kein weiterer
*                 Satz dieser LLTB mehr gelesen werden kann
            wu_ml_stack-loopx = sy-tabix + 1 .
*                 LeseIndex merken
            MODIFY wu_ml_stack INDEX stack_prevx.
          ELSE.
            CLEAR: stack_flag.
          ENDIF.

          act_level = act_level + 1 .
          READ TABLE wu_ml_stack
             WITH KEY act_level.
        ELSE.
          IF act_level = 1 .
            EXIT.
            end_flag = 'x'.
          ENDIF.

          DESCRIBE TABLE wu_ml_stack LINES sy-tabix.
          DELETE wu_ml_stack INDEX sy-tabix.
          sy-tabix = sy-tabix - 1.
          READ TABLE wu_ml_stack INDEX sy-tabix.
        ENDIF.

        wu_memid-tabid = 'LT'.
        wu_memid-matnr = wu_ml_stack-matnr.
        wu_memid-werks = wu_ml_stack-werks.
        wu_memid-stlan = wu_ml_stack-stlan.

        CLEAR: lltb. REFRESH: lltb.
        IMPORT ltb TO lltb
          FROM MEMORY ID wu_memid.

        wu_memid-tabid = 'MT'.
        CLEAR: lmatcat. REFRESH: lmatcat.
        IMPORT matcat TO lmatcat
           FROM MEMORY ID wu_memid.

        act_level = wu_ml_stack-stufe.
        ltb_strtx = wu_ml_stack-loopx.
      ENDWHILE.
    ENDIF.                                                  "YHG125492

*d    IF NOT SY-BATCH IS INITIAL.                   "YHG078090"HGC072824
    IF     NOT sy-batch IS INITIAL                          "HGC072824
       AND cattaktiv IS INITIAL.                            "HGC072824

      PERFORM print_mode_batch.                             "YHG078090
    ENDIF.                                                  "YHG078090

*     ?System mit Revisionsstaenden
*     ja
    IF NOT tcc08-ccrvl IS INITIAL.                          "YHG087082
*        ?gibt es Revisionsstaende zum Material
*        ja
      IF NOT selpool-kzrev IS INITIAL.                      "YHG083168
        PERFORM get_revlv.                                  "YHG083168
      ENDIF.                                                "YHG083168
    ENDIF.                                                  "YHG087082

    IF NOT pm_alvsa IS INITIAL.                             "HGAcxy...
      PERFORM create_dsp_sel.                               "YHG139715

*        LTB (Verwendungen) anzeigen
      PERFORM cs15.
    ELSE.                                                   "HGAcxy...
*      PERFORM alv_top_tb_prep USING alv_top_tb.            "HGAcxy...
      PERFORM cs15_alv.                                    "HGAcxy...
    ENDIF.                                                  "HGAcxy...
*  sonst ...
  ELSE.
*     Fehler: keine Verwendung selektiert
    MESSAGE s507 WITH 'E: ' pm_idnrk.
  ENDIF.


*eject
* ---------------------------------
TOP-OF-PAGE.
*  Seitenkopfausgabe
  PERFORM top_01_79 USING ltb-stlty.


*eject
* ---------------------------------
TOP-OF-PAGE DURING LINE-SELECTION.
*  Seitenkopfausgabe
  PERFORM top_01_79 USING ltb-stlty.


*eject
* ---------------------------------
END-OF-SELECTION.
*  Reset HIDE-Bereich
  PERFORM clr_hide_area.


*eject
***********************************************************************
*        F O R M  -  Routinen                                         *
***********************************************************************
  INCLUDE zcs15nn1.
*INCLUDE rcs15nn1.                                             "YHG000352


*eject
*---------------------------------------------------------------------*
*        CS15                                                         *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM cs15.
  DATA: get_prf_flg LIKE csdata-xfeld.                      "YHG139715

* ---------------------------------                           "YHG139715
*..verlegt.........................

  CLEAR: get_prf_flg.                                       "YHG139715
*  ?Druck eingeschaltet
*  ja
*d IF sy-ucomm EQ 'CSPR'.                         "YHG110068"note 351902
  IF sv_ucomm EQ 'CSPR'.                                   "note 351902
    IF     act_profil NE pm_prprf                           "YHG139715
       AND act_profil NE dflt_prprf.                        "YHG139715

*        Druckprofil aktivieren
      act_profil = pm_prprf.                                "YHG110068
*        neues Profil besorgen lassen
      get_prf_flg = 'x'.                                    "YHG139715
    ENDIF.                                                  "YHG139715
*  nein, Bildschirmausgabe
  ELSE.                                                     "YHG110068
    IF     act_profil NE pm_dsprf                           "YHG139715
       AND act_profil NE dflt_dsprf.                        "YHG139715

*        Bildschirmausgabeprofil aktivieren
      act_profil = pm_dsprf.                                "YHG110068
*        neues Profil besorgen lassen
      get_prf_flg = 'x'.                                    "YHG139715
    ENDIF.                                                  "YHG139715
  ENDIF.                                                    "YHG110068

  IF NOT get_prf_flg IS INITIAL.                            "YHG139715
*     im Profil definierte Zeilenbreite besorgen
    CALL FUNCTION 'CLFC_PROFILE_SIZE'                       "YHG110068
      EXPORTING                                          "YHG110068
        listid                = list_id               "YHG110068
        profile               = act_profil            "YHG110068
      IMPORTING                                          "YHG110068
*del            SIZE                  = SAV_PRFSZ   "YHG110068"YHG032486
        size                  = itf_prfsz             "YHG032486
      EXCEPTIONS                                         "YHG110068
        listid_not_found      = 01                    "YHG110068
        no_valid_listid       = 02                    "YHG110068
        no_valid_profile      = 03                    "YHG110068
        profile_not_found     = 04                    "YHG110068
        profile_not_in_listid = 05.                   "YHG110068

    sav_prfsz = itf_prfsz.                                  "YHG032486

    IF sy-subrc <> 0.                                       "YHG135858
*     ?Druck eingeschaltet
*     ja
*d       IF sy-ucomm EQ 'CSPR'.                   "YHG135858"note 351902
      IF sv_ucomm EQ 'CSPR'.                             "note 351902
*        Druckprofil aktivieren
        act_profil = dflt_prprf.                            "YHG135858
*     nein, Bildschirmausgabe
      ELSE.                                                 "YHG135858
*        Bildschirmausgabeprofil aktivieren
        act_profil = dflt_dsprf.                            "YHG135858
      ENDIF.                                                "YHG135858

      CALL FUNCTION 'CLFC_PROFILE_SIZE'                    "YHG135858
        EXPORTING                                       "YHG135858
          listid  = list_id                          "YHG135858
          profile = act_profil                       "YHG135858
        IMPORTING                                       "YHG135858
*del               SIZE    = SAV_PRFSZ.             "YHG135858"YHG032486
          size    = itf_prfsz.                       "YHG032486

      sav_prfsz = itf_prfsz.                                "YHG032486
    ENDIF.                                                  "YHG135858

*     Zeilenbreiten wg. Rahmen um 2 erhoehen
    siz_linpf = sav_prfsz + 2.                              "YHG110068
  ENDIF.                                                    "YHG139715
*..................................
* ---------------------------------                           "YHG139715

*d IF sy-ucomm EQ 'CSPR'.                         "YHG139715"note 351902
  IF sv_ucomm EQ 'CSPR'.                                   "note 351902
    PERFORM prep_druck.                                     "YHG139715
    PERFORM selkrit_druck.                                  "YHG139715
  ENDIF.                                                    "YHG139715

  CLEAR: outpt_flg.                                         "YHG134257

  mng_stack-stufe = 1 .                                     "YHG125492
  mng_stack-emeng = pm_emeng.                               "YHG125492
  mng_stack-rmeng = pm_rmeng.                               "YHG125492
  mng_stack-emfac = 1 .                                     "HGA115915
  APPEND mng_stack.                                         "YHG125492

  CLEAR: cng_level.                                         "HGB154389
*ENHANCEMENT-POINT RCS15001_L4 SPOTS ES_RCS15001.
*  pro LTB-Eintrag
  LOOP AT ltb.
    CHECK chk_types CA ltb-bmtyp.   "YHG110068 note0145676"note 387347
*     CHECK chk_types CA ltb-bmtyp AND          "note0145676"note 387347
*           ltb-bmtyp NE space.                 "note0145676"note 387347

    ltb_loopx = sy-tabix.                                   "YHG125492

    IF ltb-bmtyp NE space.                                "note 387347
*d    ON CHANGE OF LTB-LEVEL.                       "YHG125492"HGB154389
      IF ltb-level NE cng_level.                            "HGB154389
        cng_level = ltb-level.                              "HGB154389

*del     IF SY-TABIX <> 1.                                    "YHG125492
        IF NOT outpt_flg IS INITIAL.                        "YHG139715
          IF mng_stack-stufe < ltb-level .                  "YHG125492
            mng_stack-stufe = ltb-level.                    "YHG125492

*d             MNG_STACK-EMENG = SAV_RMENG.         "YHG125492"HGA131954

            IF     lst_bmein NE ltb-emeih                   "HGA013934
               AND NOT lst_bmein IS INITIAL.                "HGA013934

              CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'    "HGA013934
                EXPORTING                              "HGA013934
                  input    = sav_frmng              "HGA013934
                  kzmeinh  = 'X'                    "HGA013934
                  matnr    = matcat-matnr           "HGA013934
                  meinh    = lst_bmein              "HGA013934
                  meins    = ltb-emeih              "HGA013934
                  type_umr = '3'                    "HGA013934
                IMPORTING                              "HGA013934
*d                          output   = sav_frmng.   "HGA013934 MBA148624
                  output   = sav_frmng              "MBA148624
                             EXCEPTIONS                             "MBA148624
                             conversion_not_found              "MBA148624
                             input_invalid                     "MBA148624
                             material_not_found                "MBA148624
                             meinh_not_found                   "MBA148624
                             meins_missing                     "MBA148624
                             no_meinh                          "MBA148624
                             output_invalid                    "MBA148624
                             overflow.                         "MBA148624

              CLEAR: lst_bmein.                             "HGA013934
            ENDIF.                                          "HGA013934

            mng_stack-emeng = sav_frmng.                    "HGA131954
            mng_stack-rmeng = 0.                            "YHG125492
*d             MNG_STACK-EMFAC = MNG_STACK-EMFAC    "HGA115915"HGA118294
*d                               * ACT_EMFAC .      "HGA115915"HGA118294
            mng_stack-emfac = act_emfac.                    "HGA118294
            mng_stack-extrm = act_extrm.                    "HGA118294
            mng_stack-ldsgn = act_ldsgn.                    "HGA118294

            APPEND mng_stack.                               "YHG125492
            prv_extrm = act_extrm.                          "HGA118294
          ELSE.                                             "YHG125492
            DESCRIBE TABLE mng_stack LINES sy-tabix.        "YHG125492
            WHILE mng_stack-stufe > ltb-level.              "YHG125492
              DELETE mng_stack INDEX sy-tabix.              "YHG125492
              sy-tabix = sy-tabix - 1.                      "YHG125492
              READ TABLE mng_stack INDEX sy-tabix.          "YHG125492
              prv_extrm = mng_stack-extrm.                  "HGA118294
            ENDWHILE.                                       "YHG125492

            IF skipl_flg IS INITIAL.                        "HGA032266
              ULINE AT /1(siz_linpf).                       "YHG125492
            ELSE.                                           "HGA032266
              eopth_flg = 'x'.                              "HGA032266
            ENDIF.                                          "HGA032266
          ENDIF.                                            "YHG125492
        ENDIF.                                              "YHG125492
*d    ENDON.                                        "YHG125492"HGB154389
      ENDIF.                                                "HGB154389

      IF     NOT pm_rmeng IS INITIAL                        "HGA115915
         AND NOT ltb-sumfg EQ 'x'.                          "HGA115915

        act_emfac = ( ltb-emeng + ltb-fxmng )               "HGA115915
                    / ltb-bmeng .                           "HGA115915
      ENDIF.                                                "HGA115915

      PERFORM get_objdata.                                  "YHG110068
    ENDIF.                                                "note 387347

*     immer -  nur beim Drucken nicht
*del  IF SY-UCOMM NE 'CSPR'.                                  "YHG078090
*d    IF     sy-ucomm NE 'CSPR'                   "YHG078090"note 351902
    IF     sv_ucomm NE 'CSPR'                             "note 351902
       OR  NOT sy-batch IS INITIAL.                         "YHG078090

*        ggf. Ausnahmentabelle pflegen
      PERFORM keep_excpt.
    ENDIF.
*     nur weiter, wenn LSTFLG initial
    CHECK ltb-lstfg IS INITIAL.

*     fuer Stufe 1 - Original-Mengeneinheit merken
*d    PERFORM ACT_MEINH.                                      "HGA118294

    PERFORM mng_dsp_new.                                    "HGA118294

*d    SAV_RMENG = DSP_RMENG.                        "YHG125492"HGA131954

*     ?stimmt das in der Ueberschrift stehende Objekt noch
*     nein
    IF     ojtop_mrk NE ltb-stlty
       AND NOT ojtop_mrk IS INITIAL
       AND NOT ltb-stlty IS INITIAL
       AND NOT outpt_flg IS INITIAL.                        "YHG134257

*        ?Bildschirmausgabe
*        ja
*d       IF sy-ucomm NE 'CSPR'.                             "note 351902
      IF sv_ucomm NE 'CSPR'.                             "note 351902
*del        ULINE (81).                                       "YHG109837
        ULINE AT /1(siz_linpf).                             "YHG109837
*           Seite bis zum Ende mit Leerzeilen auffuellen
        PERFORM end_page.
      ENDIF.

*        Seitenvorschub
      NEW-PAGE.
    ENDIF.

*     die eigentliche Anzeige
    PERFORM list_01_79.

    IF     outpt_flg IS INITIAL.                            "YHG134257
      outpt_flg = 'x'.                                      "YHG134257
    ENDIF.                                                  "YHG134257

    sy-tabix = ltb_loopx.                                   "YHG125492
  ENDLOOP.

*  IF outpt_flg IS INITIAL.                      "YHG134257 "note 878684
  IF outpt_flg IS INITIAL AND ltb[] IS INITIAL.            "note 878684
*     Fehler: keine Verwendung selektiert
    MESSAGE s507 WITH 'E: ' pm_idnrk.                       "YHG134257
    CLEAR: eolst_flg.                                       "YHG134257

    EXIT.                                                   "YHG134257
  ENDIF.                                                    "YHG134257

*  Ausgabe der Nachweisliste bis Seitenende; Ausgabe EXCPT vorbereiten
  PERFORM end_of_list.
*  EXCPT ausgeben
  PERFORM list_exceptions.

*  ?Druckmodus
*  nein
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
  IF sv_ucomm NE 'CSPR'.                                   "note 351902
    READ TABLE excpt INDEX 1.
*     ?Ausnahmehinweise ausgegeben
*     ja
    IF sy-subrc = 0.
*        Leerzeile mit Seitenrahmen generieren
      PERFORM set_margin.
    ENDIF.

*     Rahmen unten abschliessen
*del  ULINE (81).                                             "YHG109837
    ULINE AT /1(siz_linpf).                                 "YHG109837
*     bis (Bilschirm-)Seitenende vorschieben
*del     PERFORM END_PAGE.                                    "YHG147318
  ELSE.                                                     "YHG139336
    SKIP.                                                   "YHG139336
*     Ende der Liste
    FORMAT COLOR COL_BACKGROUND.                            "YHG139336
    WRITE: /       text-098 INTENSIFIED.                    "YHG139336
  ENDIF.

*  Kennzeichen 'Nachweisliste komplett ausgegeben' zuruecksetzen
  CLEAR: eolst_flg.
  PERFORM set_status_gray.                                  "MBA089075
ENDFORM.



*eject
*---------------------------------------------------------------------*
*        EXNREG_WU                                                    *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM exp_wutab
   USING lcl_matnr
         lcl_werks
         lcl_stlan.

  wu_memid-tabid = 'LT'.
  wu_memid-matnr = lcl_matnr.
  wu_memid-werks = lcl_werks.
  wu_memid-stlan = lcl_stlan.
  EXPORT ltb TO MEMORY ID wu_memid.

  wu_memid-tabid = 'MT'.
  EXPORT matcat TO MEMORY ID wu_memid.
ENDFORM.


FORM reg_wures
   USING lcl_matnr
         lcl_werks
         lcl_stlan
         lcl_wuchk
         lcl_isusd
         lcl_wutck.

  wu_ctab_key-matnr = lcl_matnr.
  wu_ctab_key-werks = lcl_werks.
  wu_ctab_key-stlan = lcl_stlan.
  READ TABLE wu_ctab WITH KEY wu_ctab_key.

  IF sy-subrc <> 0.
    MOVE-CORRESPONDING wu_ctab_key TO wu_ctab.
    wu_ctab-wuchk = lcl_wuchk.
    wu_ctab-isusd = lcl_isusd.
    wu_ctab-wutck = lcl_wutck.
    APPEND wu_ctab.
  ELSE.
    IF NOT lcl_wuchk IS INITIAL.
      wu_ctab-wuchk = lcl_wuchk.
    ENDIF.

    IF NOT lcl_isusd IS INITIAL.
      wu_ctab-isusd = lcl_isusd.
    ENDIF.

    IF NOT lcl_wutck IS INITIAL.
      wu_ctab-wutck = lcl_wutck.
    ENDIF.

    MODIFY wu_ctab.
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        GET_REVLV                                                    *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM get_revlv.                                             "YHG083168
*  passenden Revisionsstand holen
  CALL FUNCTION 'REVISION_LEVEL_SELECT'
    EXPORTING
      matnr              = selpool-matnr
      datuv              = revl_sdatu
    IMPORTING
      arevlv             = tmat_revlv
    EXCEPTIONS
      date_not_found     = 1
      input_incomplete   = 2
      input_inconsistent = 3
      revision_not_found = 4.

*  auch wenn es keinen passenden RevStand gab, weitermachen
  IF sy-subrc <> 0.
    CLEAR: tmat_revlv.
    sy-subrc = 0.
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        GET_REVL_SDATU                                               *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM get_revl_sdatu.                                        "YHG083168
  CHECK NOT tcc08-ccrvl IS INITIAL.                         "YHG087082
  IF pm_datub <= pm_datuv.
    revl_sdatu = pm_datuv.
    IF revl_sdatu IS INITIAL.
      revl_sdatu = max_grg.
    ENDIF.
  ELSE.
    revl_sdatu = pm_datub.
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        GET_WU_RECS                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM get_wu_recs                                            "YHG125492
   USING lcl_matnr
         lcl_werks
         lcl_stlan.

*  ehem. komplett Teil von START-OF-SELECTION
*  ?nur direkte Verwendungen anzeigen
*  ja
  IF     NOT pm_dirkt IS INITIAL                            "YHG000381
     AND pm_uebkl IS INITIAL.                               "YHG000381

*     alle direkten Verwendungen nach LTB holen
    CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        datub                      = pm_datub
        datuv                      = pm_datuv
*del            MATNR = PM_IDNRK                              "YHG125492
        matnr                      = lcl_matnr                             "YHG125492
        postp                      = pm_postp
*del            STLAN = PM_STLAN                              "YHG125492
        stlan                      = lcl_stlan                             "YHG125492
*del            WERKS = PM_WERKS                              "YHG125492
        werks                      = lcl_werks                             "YHG125492
        stltp                      = stltp_in                            "note 308150
      IMPORTING
        topmat                     = selpool
      TABLES
        wultb                      = lt_ltb
        equicat                    = equicat                             "YHG110068
        kndcat                     = kndcat                              "YHG110068
        matcat                     = matcat                              "YHG110068
        stdcat                     = stdcat                              "YHG110068
        tplcat                     = tplcat                              "YHG110068
        prjcat                     = prjcat                              "MBA089075
      EXCEPTIONS
*       CALL_INVALID               = 01
        material_not_found         = 02
        no_where_used_rec_found    = 03
        no_where_used_rec_selected = 04
        no_where_used_rec_valid    = 05.
  ENDIF.                                                    "YHG000381

*  ?nur indirekte Verwendungen ueber Klassen anzeigen
*  ja
  IF     NOT pm_uebkl IS INITIAL                            "YHG000381
     AND pm_dirkt IS INITIAL.                               "YHG000381

*     alle Verwendungen von PM_IDNRK ueber Klassen nach LTB holen
    CALL FUNCTION 'CS_WHERE_USED_MAT_VIA_CLA'               "YHG000381
      EXPORTING                                          "YHG000381
        datub                      = pm_datub                              "YHG000381
        datuv                      = pm_datuv                              "YHG000381
*del            MATNR = PM_IDNRK                    "YHG000381"YHG125492
        matnr                      = lcl_matnr                             "YHG125492
        postp                      = pm_postp                              "YHG000381
*del            STLAN = PM_STLAN                    "YHG000381"YHG125492
        stlan                      = lcl_stlan                             "YHG125492
*del            WERKS = PM_WERKS                    "YHG000381"YHG125492
        werks                      = lcl_werks                             "YHG125492
        stltp                      = stltp_in                            "note 308150
      IMPORTING                                          "YHG000381
        topmat                     = selpool                              "YHG000381
      TABLES                                             "YHG000381
        wultb                      = lt_ltb                                   "YHG000381
        equicat                    = equicat                             "YHG110068
        kndcat                     = kndcat                              "YHG110068
        matcat                     = matcat                              "YHG110068
        stdcat                     = stdcat                              "YHG110068
        tplcat                     = tplcat                              "YHG110068
      EXCEPTIONS                                         "YHG000381
*       CALL_INVALID               = 01               "YHG000381
        material_not_found         = 02               "YHG000381
        no_where_used_rec_found    = 03               "YHG000381
        no_where_used_rec_selected = 04               "YHG000381
        no_where_used_rec_valid    = 05.              "YHG000381
  ENDIF.                                                    "YHG000381

*  ?sowohl direkte Verw. als auch Verw. ueber Klassen
*  ja, beides
  IF     NOT pm_dirkt IS INITIAL                            "YHG000381
     AND NOT pm_uebkl IS INITIAL.                           "YHG000381

*     Klassenposition auf Liste kennzeichnen
    clasp_flg = 'x'.                                        "YHG000381

*     alle direkten Verw.n als auch Verw.n ueber Klassen holen
    CALL FUNCTION 'CS_WHERE_USED_MAT_ANY'                   "YHG000381
      EXPORTING                                          "YHG000381
        datub                      = pm_datub                              "YHG000381
        datuv                      = pm_datuv                              "YHG000381
*del            MATNR = PM_IDNRK                    "YHG000381"YHG125492
        matnr                      = lcl_matnr                             "YHG125492
        postp                      = pm_postp                              "YHG000381
*del            STLAN = PM_STLAN                    "YHG000381"YHG125492
        stlan                      = lcl_stlan                             "YHG125492
*del            WERKS = PM_WERKS                    "YHG000381"YHG125492
        werks                      = lcl_werks                             "YHG125492
        stltp                      = stltp_in                            "note 308150
      IMPORTING                                          "YHG000381
        topmat                     = selpool                              "YHG000381
      TABLES                                             "YHG000381
        wultb                      = lt_ltb                                   "YHG000381
        equicat                    = equicat                             "YHG110068
        kndcat                     = kndcat                              "YHG110068
        matcat                     = matcat                              "YHG110068
        stdcat                     = stdcat                              "YHG110068
        tplcat                     = tplcat                              "YHG110068
      EXCEPTIONS                                         "YHG000381
*       CALL_INVALID               = 01               "YHG000381
        material_not_found         = 02               "YHG000381
        no_where_used_rec_found    = 03               "YHG000381
        no_where_used_rec_selected = 04               "YHG000381
        no_where_used_rec_valid    = 05.              "YHG000381
  ENDIF.                                                    "YHG000381
*  PERFORM get_sales_order_data.                    "MBA084935 MBA089075
  APPEND LINES OF lt_ltb[] TO ltb[].
ENDFORM.


*eject
FORM mng_dsp_new.                                           "HGA118294
  DATA: tmp_flt1 TYPE f,
        tmp_flt2 TYPE f.

* ---------------------------------

*  abs. Btrg.
  IF ltb-msign = '-'.
    ltb-menge = ltb-menge * -1 .
    ltb-emeng = ltb-emeng * -1 .
    ltb-fxmng = ltb-fxmng * -1 .
  ENDIF.

*  einstufig
  IF ltb-level = 1 .
    CASE ltb-sumfg.
      WHEN space.
        CLEAR: act_extrm.
        ltb-emeng = ltb-menge.
*d          LTB-EMEIH = LTB-MEINS.                            "HGA013934

        IF NOT pm_emeng IS INITIAL.
          IF ltb-fmeng IS INITIAL.
            dsp_imeng = pm_emeng.

            tmp_flt1 =   ltb-bmeng
                       * (   pm_emeng
                           / ltb-menge ) .
            IF tmp_flt1 < max_fnum.
              dsp_rmeng = tmp_flt1.
              sav_frmng = tmp_flt1.                         "HGA131954
            ELSE.
              dsp_rmeng = max_fnum.
              sav_frmng = max_fnum.                         "HGA131954
            ENDIF.
          ELSE.
            dsp_imeng = ltb-menge.

            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954

            IF ltb-menge >=  pm_emeng.
              act_extrm = '-'.
            ELSE.
              act_extrm = '+'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT pm_rmeng IS INITIAL.
          IF ltb-fmeng IS INITIAL.
            tmp_flt1 =   ltb-menge
                       * (   pm_rmeng
                           / ltb-bmeng ) .
            IF tmp_flt1 < max_fnum.
              dsp_imeng = tmp_flt1.
            ELSE.
              dsp_imeng = max_fnum.
            ENDIF.

            dsp_rmeng = pm_rmeng.
            sav_frmng = pm_rmeng.                           "HGA131954
          ELSE.
            dsp_imeng = ltb-menge.
            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954
            act_extrm = '+'.
          ENDIF.

*d             ACT_EMFAC = DSP_IMENG / DSP_RMENG .            "HGA131954

          IF sav_frmng <> fnull.                            "HGA131954
            act_emfac = dsp_imeng / sav_frmng .             "HGA131954
          ELSE.                                             "HGA131954
            act_emfac = max_fnum.                           "HGA131954
          ENDIF.                                            "HGA131954
        ENDIF.

        IF     pm_emeng IS INITIAL
           AND pm_rmeng IS INITIAL.

          IF ltb-fmeng IS INITIAL.
            dsp_imeng = ltb-menge.
            dsp_rmeng = ltb-bmeng.
            sav_frmng = ltb-bmeng.                          "HGA131954
          ELSE.
            dsp_imeng = ltb-menge.
            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954
            act_extrm = '+'.
          ENDIF.
        ENDIF.

        sum_factor = fnull.
        opo_meinh = ltb-meins.
        opo_menge = dsp_imeng.
        act_ldsgn = ltb-msign.


      WHEN '*'.
        CLEAR: act_extrm.

        IF NOT pm_emeng IS INITIAL.
          IF     NOT ltb-emeng IS INITIAL
             AND ltb-fxmng IS INITIAL.

            dsp_imeng = pm_emeng.

            tmp_flt1 =   ltb-bmeng
                       * (   pm_emeng
                           / ltb-emeng ) .
            IF tmp_flt1 < max_fnum.
              dsp_rmeng = tmp_flt1.
              sav_frmng = tmp_flt1.                         "HGA131954
            ELSE.
              dsp_rmeng = max_fnum.
              sav_frmng = max_fnum.                         "HGA131954
            ENDIF.

            sum_factor = dsp_imeng / ltb-emeng.
          ENDIF.

          IF     NOT ltb-fxmng IS INITIAL
             AND ltb-emeng IS INITIAL.

            dsp_imeng = ltb-fxmng.

            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954

            IF ltb-fxmng >= pm_emeng.
              act_extrm = '-'.
            ELSE.
              act_extrm = '+'.
            ENDIF.

            sum_factor = 1.
          ENDIF.

          IF     NOT ltb-emeng IS INITIAL
             AND NOT ltb-fxmng IS INITIAL.

            tmp_flt2 = pm_emeng - ltb-fxmng.
            IF tmp_flt2 <= pnull.
              dsp_imeng = ltb-fxmng.

              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954
              act_extrm = '-'.

              sum_factor = fnull.
            ELSE.
              dsp_imeng = pm_emeng.

              tmp_flt1 =   ltb-bmeng
                         * (   tmp_flt2
                             / ltb-emeng ) .
              IF tmp_flt1 < max_fnum.
                dsp_rmeng = tmp_flt1.
                sav_frmng = tmp_flt1.                       "HGA131954
              ELSE.
                dsp_rmeng = max_fnum.
                sav_frmng = max_fnum.                       "HGA131954
              ENDIF.

              sum_factor = tmp_flt2 / ltb-emeng.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT pm_rmeng IS INITIAL.
          IF     NOT ltb-emeng IS INITIAL
             AND ltb-fxmng IS INITIAL.

            tmp_flt1 =   ltb-emeng
                       * (   pm_rmeng
                           / ltb-bmeng ).
            IF tmp_flt1 < max_fnum.
              dsp_imeng = tmp_flt1.
            ELSE.
              dsp_imeng = max_fnum.
            ENDIF.

            dsp_rmeng = pm_rmeng.
            sav_frmng = pm_rmeng.                           "HGA131954

            sum_factor = dsp_imeng / ltb-emeng.
          ENDIF.

          IF     NOT ltb-fxmng IS INITIAL
             AND ltb-emeng IS INITIAL.

            dsp_imeng = ltb-fxmng.

            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954
            act_extrm = '+'.

            sum_factor = 1.
          ENDIF.

          IF     NOT ltb-emeng IS INITIAL
             AND NOT ltb-fxmng IS INITIAL.

            tmp_flt1 =   (  ltb-emeng
                          * (   pm_rmeng
                              / ltb-bmeng ) )
                       + ltb-fxmng.
            IF tmp_flt1 < max_fnum.
              dsp_imeng = tmp_flt1.
            ELSE.
              dsp_imeng = max_fnum.
            ENDIF.

            dsp_rmeng = pm_rmeng.
            sav_frmng = pm_rmeng.                           "HGA131954

            sum_factor = pm_rmeng / ltb-bmeng.
          ENDIF.

*d             ACT_EMFAC = DSP_IMENG / DSP_RMENG .            "HGA131954

          IF sav_frmng <> fnull.                            "HGA131954
            act_emfac = dsp_imeng / sav_frmng .             "HGA131954
          ELSE.                                             "HGA131954
            act_emfac = max_fnum.                           "HGA131954
          ENDIF.                                            "HGA131954
        ENDIF.

        IF     pm_emeng IS INITIAL
           AND pm_rmeng IS INITIAL.

          IF     NOT ltb-emeng IS INITIAL
             AND ltb-fxmng IS INITIAL.

            IF ltb-emeng < max_fnum.                        "MBB167558
              dsp_imeng = ltb-emeng.
            ELSE.                                           "MBB167558
              dsp_imeng = max_fnum.                         "MBB167558
            ENDIF.                                          "MBB167558
            dsp_rmeng = ltb-bmeng.
            sav_frmng = ltb-bmeng.                          "HGA131954
          ENDIF.

          IF     NOT ltb-fxmng IS INITIAL
             AND ltb-emeng IS INITIAL.

            dsp_imeng = ltb-fxmng.
            dsp_rmeng = pnull.
            sav_frmng = fnull.                              "HGA131954
            act_extrm = '+'.
          ENDIF.

          IF     NOT ltb-emeng IS INITIAL
             AND NOT ltb-fxmng IS INITIAL.

            dsp_imeng = ltb-emeng + ltb-fxmng.
            dsp_rmeng = ltb-bmeng.
            sav_frmng = ltb-bmeng.                          "HGA131954
          ENDIF.

          sum_factor = 1 .
        ENDIF.

        opo_meinh = ltb-emeih.
        opo_menge = dsp_imeng.
        act_ldsgn = ltb-msign.


      WHEN 'x'.
        IF ltb-fmeng IS INITIAL.
          tmp_flt1 = ltb-emeng * sum_factor.
          IF tmp_flt1 < max_fnum.
            dsp_imeng = tmp_flt1.
          ELSE.
            dsp_imeng = max_fnum.
          ENDIF.
        ELSE.
          dsp_imeng = ltb-emeng.
        ENDIF.

        IF act_ldsgn EQ '-'.
          dsp_imeng = dsp_imeng * -1 .
        ENDIF.

    ENDCASE.
*  mehrstufig
  ELSE.
    CHECK ltb-excpt NE 'CONV'.                              "MBA148624
    IF mng_stack-extrm IS INITIAL.
      CASE ltb-sumfg.
        WHEN space.
          CLEAR: untfc_flg,
                 act_extrm.

          IF NOT pm_emeng IS INITIAL.
            IF ltb-fmeng IS INITIAL.
              dsp_imeng = opo_menge.

              tmp_flt1 =   ltb-bmeng
                         * (   mng_stack-emeng
                             / ltb-emeng ).
              IF tmp_flt1 < max_fnum.
                dsp_rmeng = tmp_flt1.
                sav_frmng = tmp_flt1.                       "HGA131954
              ELSE.
                dsp_rmeng = max_fnum.
                sav_frmng = max_fnum.                       "HGA131954
              ENDIF.
            ELSE.
              tmp_flt1 =   (   ltb-emeng
                              * opo_menge )
                         / mng_stack-emeng .
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954

              IF mng_stack-emeng >= ltb-emeng.
                act_extrm = '+'.
              ELSE.
                act_extrm = '-'.
              ENDIF.
            ENDIF.
          ENDIF.

          IF NOT pm_rmeng IS INITIAL.
            IF ltb-fmeng IS INITIAL.
              tmp_flt1 =   (   (   ltb-emeng
                                 * pm_rmeng )
                             / ltb-bmeng )
                         * mng_stack-emfac.
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pm_rmeng.
              sav_frmng = pm_rmeng.                         "HGA131954

*d                ACT_EMFAC = DSP_IMENG / DSP_RMENG .         "HGA131954
              act_emfac = dsp_imeng / sav_frmng .           "HGA131954
            ELSE.
              dsp_imeng = ltb-emeng * mng_stack-emfac.
              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954
              act_extrm = '+'.
              act_emfac = dsp_imeng / ltb-bmeng .
            ENDIF.
          ENDIF.

          IF     pm_emeng IS INITIAL
             AND pm_rmeng IS INITIAL.

            IF ltb-fmeng IS INITIAL AND NOT ltb-emeng IS INITIAL. "3004526
              dsp_imeng = opo_menge.

              tmp_flt1 =   ltb-bmeng
                         * (   mng_stack-emeng
                             / ltb-emeng ).
              IF tmp_flt1 < max_fnum.
                dsp_rmeng = tmp_flt1.
                sav_frmng = tmp_flt1.                       "HGA131954

                IF sav_frmng = fnull.                       "HGA131954
                  act_extrm = '-'.                          "HGA131954
                ENDIF.                                      "HGA131954
              ELSE.
                dsp_rmeng = max_fnum.
                sav_frmng = max_fnum.                       "HGA131954
              ENDIF.
            ELSE.
              tmp_flt1 =   (   ltb-emeng
                              * opo_menge )
                         / mng_stack-emeng .
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954

              IF mng_stack-emeng >= ltb-emeng.
                act_extrm = '-'.
              ELSE.
                act_extrm = '+'.
              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR: sum_factor.

          IF ltb-msign NE mng_stack-ldsgn.
            act_ldsgn = '-'.
          ELSE.
            act_ldsgn = '+'.
          ENDIF.


        WHEN '*'.
          CLEAR: untfc_flg,
                 act_extrm.

          IF NOT pm_emeng IS INITIAL.
            IF     NOT ltb-emeng IS INITIAL
               AND ltb-fxmng IS INITIAL.

              dsp_imeng = opo_menge.

              tmp_flt1 =   ltb-bmeng
                         * (   mng_stack-emeng
                             / ltb-emeng ).
              IF tmp_flt1 < max_fnum.
                dsp_rmeng = tmp_flt1.
                sav_frmng = tmp_flt1.                       "HGA131954
              ELSE.
                dsp_rmeng = max_fnum.
                sav_frmng = max_fnum.                       "HGA131954
              ENDIF.

              sum_factor = dsp_imeng / ltb-emeng.
            ENDIF.

            IF     NOT ltb-fxmng IS INITIAL
               AND ltb-emeng IS INITIAL.

              tmp_flt1 =   (   ltb-fxmng
                              * opo_menge )
                         / mng_stack-emeng .
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954

              IF mng_stack-emeng >= ltb-fxmng.
                act_extrm = '+'.
              ELSE.
                act_extrm = '-'.
              ENDIF.

              unt_factor = opo_menge / mng_stack-emeng.
              untfc_flg = 'x'.
            ENDIF.

            IF     NOT ltb-emeng IS INITIAL
               AND NOT ltb-fxmng IS INITIAL.

              tmp_flt2 = mng_stack-emeng - ltb-fxmng.
              IF tmp_flt2 <= pnull.
                tmp_flt1 =   ltb-fxmng
                           * (   opo_menge
                               / mng_stack-emeng ).
                IF tmp_flt1 < max_fnum.
                  dsp_imeng = tmp_flt1.
                ELSE.
                  dsp_imeng = max_fnum.
                ENDIF.

                dsp_rmeng = pnull.
                sav_frmng = fnull.                          "HGA131954

                act_extrm = '-'.

                sum_factor = fnull.
              ELSE.
                dsp_imeng = opo_menge.

                tmp_flt1 =   ltb-bmeng
                           * (   tmp_flt2
                               / ltb-emeng ) .
                IF tmp_flt1 < max_fnum.
                  dsp_rmeng = tmp_flt1.
                  sav_frmng = tmp_flt1.                     "HGA131954
                ELSE.
                  dsp_rmeng = max_fnum.
                  sav_frmng = max_fnum.                     "HGA131954
                ENDIF.

                sum_factor =   ( tmp_flt2 / ltb-emeng )
                             * ( opo_menge / mng_stack-emeng ).
              ENDIF.

              unt_factor = opo_menge / mng_stack-emeng.
              untfc_flg = 'x'.
            ENDIF.
          ENDIF.

          IF NOT pm_rmeng IS INITIAL.
            IF     NOT ltb-emeng IS INITIAL
               AND ltb-fxmng IS INITIAL.

              tmp_flt1 =    ltb-emeng
                          * (   pm_rmeng
                              / ltb-bmeng )
                          * mng_stack-emfac.
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pm_rmeng.
              sav_frmng = pm_rmeng.                         "HGA131954

              sum_factor = dsp_imeng / ltb-emeng.

*d                ACT_EMFAC = DSP_IMENG / DSP_RMENG .         "HGA131954
              act_emfac = dsp_imeng / sav_frmng .           "HGA131954
            ENDIF.

            IF     NOT ltb-fxmng IS INITIAL
               AND ltb-emeng IS INITIAL.

              dsp_imeng =   ltb-fxmng
                          * mng_stack-emfac.
              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954
              act_extrm = '+'.

              sum_factor = 1.

              unt_factor = mng_stack-emfac.
              untfc_flg = 'x'.
              act_emfac = dsp_imeng / ltb-bmeng.
            ENDIF.

            IF     NOT ltb-emeng IS INITIAL
               AND NOT ltb-fxmng IS INITIAL.

              tmp_flt1 =   (  (  ltb-emeng
                               * (   pm_rmeng
                                   / ltb-bmeng ) )
                            + ltb-fxmng )
                         * mng_stack-emfac.
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pm_rmeng.
              sav_frmng = pm_rmeng.                         "HGA131954

              sum_factor =   mng_stack-emfac
                           * pm_rmeng
                           / ltb-bmeng.

              unt_factor = mng_stack-emfac.
              untfc_flg = 'x'.

*d                ACT_EMFAC = DSP_IMENG / DSP_RMENG .         "HGA131954
              act_emfac = dsp_imeng / sav_frmng .           "HGA131954
            ENDIF.
          ENDIF.

          IF     pm_emeng IS INITIAL
             AND pm_rmeng IS INITIAL.

            IF     NOT ltb-emeng IS INITIAL
               AND ltb-fxmng IS INITIAL.

              dsp_imeng = opo_menge.

              tmp_flt1 =   ltb-bmeng
                         * (   mng_stack-emeng
                             / ltb-emeng ).
              IF tmp_flt1 < max_fnum.
                dsp_rmeng = tmp_flt1.
                sav_frmng = tmp_flt1.                       "HGA131954
              ELSE.
                dsp_rmeng = max_fnum.
                sav_frmng = max_fnum.                       "HGA131954
              ENDIF.

              sum_factor = dsp_imeng / ltb-emeng.
            ENDIF.

            IF     NOT ltb-fxmng IS INITIAL
               AND ltb-emeng IS INITIAL.

              tmp_flt1 =   (   ltb-fxmng
                              * opo_menge )
                         / mng_stack-emeng .
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.

              dsp_rmeng = pnull.
              sav_frmng = fnull.                            "HGA131954
              act_extrm = '+'.

              unt_factor = opo_menge / mng_stack-emeng.
              untfc_flg = 'x'.
            ENDIF.

            IF     NOT ltb-emeng IS INITIAL
               AND NOT ltb-fxmng IS INITIAL.

              tmp_flt2 = mng_stack-emeng - ltb-fxmng.
              IF tmp_flt2 <= pnull.
                tmp_flt1 =   ltb-fxmng
                           * (   opo_menge
                               / mng_stack-emeng ).
                IF tmp_flt1 < max_fnum.
                  dsp_imeng = tmp_flt1.
                ELSE.
                  dsp_imeng = max_fnum.
                ENDIF.

                dsp_rmeng = pnull.
                sav_frmng = fnull.                          "HGA131954
                act_extrm = '-'.

                sum_factor = fnull.
              ELSE.
                dsp_imeng = opo_menge.

                tmp_flt1 =   ltb-bmeng
                           * (   tmp_flt2
                               / ltb-emeng ) .
                IF tmp_flt1 < max_fnum.
                  dsp_rmeng = tmp_flt1.
                  sav_frmng = tmp_flt1.                     "HGA131954
                ELSE.
                  dsp_rmeng = max_fnum.
                  sav_frmng = max_fnum.                     "HGA131954
                ENDIF.

                sum_factor =   ( tmp_flt2 / ltb-emeng )
                             * ( opo_menge / mng_stack-emeng ).
              ENDIF.
            ENDIF.

            unt_factor = opo_menge / mng_stack-emeng.
            untfc_flg = 'x'.
          ENDIF.

          IF ltb-msign NE mng_stack-ldsgn.
            act_ldsgn = '-'.
          ELSE.
            act_ldsgn = '+'.
          ENDIF.


        WHEN 'x'.
          IF ltb-fmeng IS INITIAL.
            tmp_flt1 = ltb-emeng * sum_factor.
            IF tmp_flt1 < max_fnum.
              dsp_imeng = tmp_flt1.
            ELSE.
              dsp_imeng = max_fnum.
            ENDIF.
          ELSE.
            dsp_imeng = ltb-emeng.

            IF NOT untfc_flg IS INITIAL.
              tmp_flt1 = dsp_imeng * unt_factor.
              IF tmp_flt1 < max_fnum.
                dsp_imeng = tmp_flt1.
              ELSE.
                dsp_imeng = max_fnum.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.

*d    IF act_ldsgn = '-'.                                    "note 57953
      IF mng_stack-ldsgn = '-'.                              "note 57953
        dsp_imeng = dsp_imeng * -1 .
      ENDIF.
    ELSE.
      dsp_imeng = pnull.
      dsp_rmeng = pnull.
      sav_frmng = fnull.                                    "HGA131954
    ENDIF.
  ENDIF.

*  abs. Btrg. rück
  IF     ltb-msign = '-'.
    ltb-menge = ltb-menge * -1 .
    ltb-emeng = ltb-emeng * -1 .
    ltb-fxmng = ltb-fxmng * -1 .
    dsp_imeng = dsp_imeng * -1 .
  ENDIF.

  lst_bmein = ltb-bmein.                                    "HGA013934
ENDFORM.


FORM prep_multilv.                                          "YHG125492
  IF NOT pm_mehrs IS INITIAL.
    CLEAR: chk_types.
    chk_types-mat = typ_mat.
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SET_CLA_SIGN_DRUCK                                           *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM set_cla_sign_druck.                                    "YHG077203
*  ?Klassenpositionen sollen als solche ausgewiesen werden und ...
*del IF     NOT CLASP_FLG IS INITIAL                          "YHG022170
*     ... die aktuelle ist eine
*del  AND LTB-IDNRK NE PM_IDNRK.                              "YHG022170

  IF NOT clasp_flg IS INITIAL.                              "YHG022170
    IF t418-postp NE ltb-postp.                             "YHG022170
      t418-postp = ltb-postp.                               "YHG022170
      READ TABLE t418.                                      "YHG022170
    ELSE.                                                   "YHG022170
      sy-subrc = 0.                                         "YHG022170
    ENDIF.                                                  "YHG022170

*     ... die aktuelle ist eine
*     ja
    IF     sy-subrc = 0                                     "YHG022170
       AND NOT t418-klpos IS INITIAL.                       "YHG022170
*        KlassenpositionsKz setzen
      WRITE  78     'X'.
    ELSE.                                                   "YHG022170
      CLEAR: t418.                                          "YHG022170
    ENDIF.                                                  "YHG022170
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SET_CLA_SIGN_LISTE                                           *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM set_cla_sign_liste.                                    "YHG077203
*  ?Klassenpositionen sollen als solche ausgewiesen werden und ...
*del IF     NOT CLASP_FLG IS INITIAL                          "YHG022170
*     ... die aktuelle ist eine
*del  AND LTB-IDNRK NE PM_IDNRK.                              "YHG022170

  IF NOT clasp_flg IS INITIAL.                              "YHG022170
    IF t418-postp NE ltb-postp.                             "YHG022170
      t418-postp = ltb-postp.                               "YHG022170
      READ TABLE t418.                                      "YHG022170
    ELSE.                                                   "YHG022170
      sy-subrc = 0.                                         "YHG022170
    ENDIF.                                                  "YHG022170

*     ... die aktuelle ist eine
*     ja
    IF     sy-subrc = 0                                     "YHG022170
       AND NOT t418-klpos IS INITIAL.                       "YHG022170
*        KlassenpositionsKz setzen
*del     WRITE 'X' TO EDTLIN+77(1) AS CHECKBOX.               "YHG083168
*del     WRITE  80     'X' AS CHECKBOX INPUT OFF.   "YHG083168"YHG110068
      ltb_add-clafg = 'X'.                                  "YHG110068
    ELSE.                                                   "YHG022170
      CLEAR: t418.                                          "YHG022170
    ENDIF.                                                  "YHG022170

  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TCC08_LESEN                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM tcc08_lesen.                                           "YHG087082
  tcc08-agbcc = 'CC'.
  READ TABLE tcc08.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TOP_01_79                                                    *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*        Seitenkopfausgabe                                            *
*                                                                     *
*---------------------------------------------------------------------*
FORM top_01_79 USING lcl_stlty.
*    ?Online-Liste
*    ja
*del IF SY-UCOMM NE 'CSPR'.
*  Datenaufbereitung im Online-Listenformat
  PERFORM top_01_79_liste USING lcl_stlty.
*del nein, Druck- bzw. Batchmodus aktiv
*del ELSE.
*       Datenaufbereitung im Druckformat
*del    PERFORM TOP_01_79_DRUCK USING LCL_STLTY.
*del ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TOP_01_79_DRUCK                                              *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*        Seitenkopfausgabe                                            *
*                                                                     *
*---------------------------------------------------------------------*
FORM top_01_79_druck USING lcl_stlty.
  FORMAT INTENSIFIED OFF.

*  1. Kopfzeile
  WRITE: /1(9)   text-007 INTENSIFIED ON.
  WRITE:  11(18) selpool-matnr INTENSIFIED OFF.
  WRITE:         tmat_revlv    INTENSIFIED OFF.             "YHG083168

*  2. Kopfzeile
  WRITE: /11(40) selpool-maktx INTENSIFIED OFF.

  IF pm_datub LE pm_datuv.
    WRITE:  52(10) text-016 INTENSIFIED ON.
    WRITE:  70(10) pm_datuv INTENSIFIED OFF.
  ELSE.
    WRITE:  52(6)  text-015 INTENSIFIED ON.
    WRITE:  59(10) pm_datuv INTENSIFIED OFF.
    WRITE:  69(1)  '-' INTENSIFIED ON.
    WRITE:  70(10) pm_datub INTENSIFIED OFF.
  ENDIF.

  FORMAT INTENSIFIED.
  ULINE AT /1(siz_linpf).
*  falls die LTB-Ausgabe beendete ist
  IF NOT eolst_flg IS INITIAL.
    SKIP 2.
*     Kopfzeilenausgabe abbrechen
    CHECK eolst_flg IS INITIAL.
  ENDIF.

*  1. Ueberschriftenzeile
  CASE lcl_stlty.
*     Verwendung in Materialstuecklisten
    WHEN typ_mat.
*        ?Seitennummer der Verw. in MatStuecklisten sitzt bereits
*        nein
      IF page_mat < 1.
*           Seitennummer merken
        page_mat = sy-pagno.
      ENDIF.

*        StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
      ojtop_mrk = lcl_stlty.

*        ?Klassenpositionen sollen als solche gekennzeichnet werden
*        ja
      IF NOT clasp_flg IS INITIAL.                          "YHG000381
*           Ueberschriftzeile mit Verweis auf Klassenposition
        WRITE: /       text-012.                            "YHG000381
*        nein, Klassenposition nicht hervorheben
      ELSE.                                                 "YHG000381
*           Standardueberschriftzeile ausgeben.
        WRITE: /       text-009.
      ENDIF.                                                "YHG000381

*     Verwendung in Equipmentstuecklisten
    WHEN typ_equi.
*        ?Seitennummer der Verw. in EqiStuecklisten sitzt bereits
*        nein
      IF page_equi < 1.
*           Seitennummer merken
        page_equi = sy-pagno.
      ENDIF.

*        StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
      ojtop_mrk = lcl_stlty.

*        ?Klassenpositionen sollen als solche gekennzeichnet werden
*        ja
      IF NOT clasp_flg IS INITIAL.                          "YHG000381
*           Ueberschriftzeile mit Verweis auf Klassenposition
        WRITE: /       text-011.                            "YHG000381
*        nein, Klassenposition nicht hervorheben
      ELSE.                                                 "YHG000381
*           Standardueberschriftzeile ausgeben.
        WRITE: /       text-008.
      ENDIF.                                                "YHG000381
  ENDCASE.

  FORMAT INTENSIFIED OFF.

*  2. Ueberschriftenzeile ...
  IF NOT pm_gbraz IS INITIAL.
*     ... ohne Gueltigkeitsbereichhinweis
    WRITE: /       text-014.
  ELSE.
*     ... mit Gueltigkeitsbereichhinweis
    WRITE: /       text-010.
  ENDIF.

  SKIP.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TOP_01_79_LISTE                                              *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*        Seitenkopfausgabe                                            *
*                                                                     *
*---------------------------------------------------------------------*
*  komplett ueberarbeitet (var. Liste)                        "YHG110068
FORM top_01_79_liste USING lcl_stlty.
*  Kein Seitenkopf im Selektionswindow
*d CHECK sy-ucomm NE 'CSSL'.                      "YHG139715"note 351902
  CHECK sv_ucomm NE 'CSSL'.                                "note 351902

*d CLEAR: TOPMAT.                                             "YHG019433

*d TOPMAT-MATNR = SELPOOL-MATNR.                              "YHG019433
*d TOPMAT-REVLV = TMAT_REVLV.                                 "YHG019433
*d TOPMAT-MAKTX = SELPOOL-MAKTX.                              "YHG019433
*D TOPMAT-DATUV = PM_DATUV.                                   "YHG019433

*  Zeilenzaehler initialisieren
  CLEAR: blclns_cnt.
*  SAV-WATAB-Entry initialisieren
  CLEAR: sav_watab.
*  SAV-WATAB leeren
  REFRESH: sav_watab.

*  die aktuellen WATab-Eintraege (var. Liste)
*del LOOP AT WATAB.                                           "YHG022170
*     ... nach SAV_WATAB ...
*del  SAV_WATAB = WATAB.                                      "YHG022170
*     ... sichern
*del  APPEND SAV_WATAB.                                       "YHG022170
*del ENDLOOP.                                                 "YHG022170
  sav_watab[] = watab[].                                    "YHG022170

*  Ausgabeformat festlegen
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

*  ?Druck
*  ja
*d IF     sy-ucomm EQ 'CSPR'                                "note 351902
  IF     sv_ucomm EQ 'CSPR'                                "note 351902
     OR  NOT sy-batch IS INITIAL.
*     Strich auf Zeile 2 mit Strichlinie aus AnzBlock ueberschreiben
*del  SKIP TO LINE 1.                                         "YHG137563
    SKIP TO LINE 2.                                         "YHG137563
  ENDIF.

*  WATAB initialisieren und komplett leeren
*d CLEAR: WATAB. REFRESH: WATAB.                              "YHG019433
*  Uebergabestruktur (Typ CSTMAT) ...
*d WATAB-TNAME = 'CSTMAT'. WATAB-TABLE = TOPMAT.              "YHG019433
*  ... sichern
*d APPEND WATAB.                                              "YHG019433

*  WATAB initialisieren und komplett leeren
  CLEAR: watab. REFRESH: watab.
*  Uebergabestruktur (Typ MC29S) ...
*d watab-tname = 'MC29S'. watab-table = selpool.                     "uc
  watab-tname = 'MC29S'.                                            "uc
  ASSIGN watab-table TO <x_watab-table>  CASTING.                   "uc
  ASSIGN selpool     TO <x_mc29s_wa>     CASTING.                   "uc
  <x_watab-table> = <x_mc29s_wa> .                                  "uc
*  ... sichern
  APPEND watab.

  CLEAR: watab,                                             "YHG019433
         ltb_add.                                           "YHG019433

  ltb_add-sldtv = pm_datuv.                                 "YHG019433
  ltb_add-revlv = tmat_revlv.                               "YHG019433

*  Uebergabestruktur (Typ STPOL_ADD) ...
*d watab-tname = 'STPOL_ADD'. watab-table = ltb_add .      "YHG019433"uc
  watab-tname = 'STPOL_ADD'.                                        "uc
  ASSIGN watab-table TO <x_watab-table>  CASTING.                   "uc
  ASSIGN ltb_add     TO <x_stpol_add_wa> CASTING.                   "uc
  <x_watab-table> = <x_stpol_add_wa> .                              "uc
*  ... sichern
  APPEND watab.                                             "YHG019433


*  WATAB initialisieren
  CLEAR watab.
*  Listenkopf ausgeben
  PERFORM write_block
     USING 'LISTHDR           '
*           ausgegebene Zeilen zaehlen
           'x'
*           Hide nicht ausfuehren
           ' '.                                             "YHG123656

  IF eolst_flg IS INITIAL.
*     WATAB initialisieren und komplett leeren
    CLEAR: watab. REFRESH: watab.
*     Uebergabestruktur (Typ STPOX) ...
    watab-tname = 'STPOX'.
*     ... sichern
    APPEND watab.

*     WATAB initialisieren
    CLEAR watab.
*     Uebergabestruktur (Typ STPOL_ADD) ...
    watab-tname = 'STPOL_ADD'.
*     ... sichern
    APPEND watab.

*     WATAB initialisieren
    CLEAR watab.

*     Ausgabe festlegen
    FORMAT COLOR COL_HEADING INTENSIFIED ON.

*     Listenueberschrift ausgeben
    CASE lcl_stlty.
      WHEN typ_mat.
*           ?Seitennummer der Verw. in MatStuecklisten sitzt bereits
*           nein
        IF page_mat < 1.
*              Seitennummer merken
          page_mat = sy-pagno.
        ENDIF.

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.

        PERFORM write_block
           USING 'LISTHDNG_M        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.                                       "YHG123656

      WHEN typ_equi.
*           ?Seitennummer der Verw. in EqiStuecklisten sitzt bereits
*           nein
        IF page_equi < 1.
*              Seitennummer merken
          page_equi = sy-pagno.
        ENDIF.

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.

        PERFORM write_block
           USING 'LISTHDNG_E        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.                                       "YHG123656

      WHEN typ_tpl.
*           ?Seitennummer der Verw. in TPlStuecklisten sitzt bereits
*           nein
        IF page_tpl  < 1.
*              Seitennummer merken
          page_tpl  = sy-pagno.
        ENDIF.

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.

        PERFORM write_block
           USING 'LISTHDNG_T        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.                                       "YHG123656

      WHEN typ_std.
*           ?Seitennummer der Verw. in StdStuecklisten sitzt bereits
*           nein
        IF page_std  < 1.
*              Seitennummer merken
          page_std  = sy-pagno.
        ENDIF.

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.

        PERFORM write_block
           USING 'LISTHDNG_S        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.                                       "YHG123656

      WHEN typ_knd.
*           ?Seitennummer der Verw. in StdStuecklisten sitzt bereits
*           nein
        IF page_knd < 1.                                    "MB075252
*             seitennummer merken
          page_knd = sy-pagno.                              "MB075252
        ENDIF.                                              "MB075252

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.                              "MB075252

        PERFORM write_block
           USING 'LISTHDNG_K        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.                                       "YHG123656

      WHEN typ_prj.                                         "MBA089075
*           ?Seitennummer der Verw. in PrjStuecklisten sitzt bereits
*           nein
        IF page_prj  < 1.
*              Seitennummer merken
          page_prj  = sy-pagno.
        ENDIF.

*           StlTyp der aktuellen Verwendung merken (f. Ueberschrifttyp)
        ojtop_mrk = lcl_stlty.

        PERFORM write_block
           USING 'LISTHDNG_P        '
*                    ausgegebene Zeilen zaehlen
                 'x'
*                    Hide nicht ausfuehren
                 ' '.
    ENDCASE.
  ENDIF.

*  WATab-Entry (var. Liste) initialisieren
  CLEAR: watab.
*  WATab (var. Liste) leeren
  REFRESH: watab.

*  gesicherte Saetze aus SAV_WATAB
*del LOOP AT SAV_WATAB.                                       "YHG022170
*     wieder nach WATAB ...
*del  WATAB = SAV_WATAB.                                      "YHG022170
*     ... uebernehmen
*del  APPEND WATAB.                                           "YHG022170
*del ENDLOOP.                                                 "YHG022170
  watab[] = sav_watab[].                                    "YHG022170

*  Anzahl Zeilen Listenkopf sichern
  nbr_hdrlns = blclns_cnt.
*  Reset Zeilenzaehler
  CLEAR: blclns_cnt.
ENDFORM.


FORM write_block                                            "YHG110068
     USING lcl_blcnm
           lcl_lncnt
           lcl_hidef.                                       "YHG123656

  DATA: first_swt(1) TYPE c.
* ---------------------------------
*  Kennzeichen 'erste Zeile ausgeben' setzen
  first_swt = 'x'.

*  bis zum St. Nimmerleinstag
  DO.
*     Zeile ausgeben
    PERFORM write_line
       USING list_id
             act_profil
             lcl_blcnm
             first_swt
             ' '.

*     ?Ist die auszugebende Zeile leer (und sitzt BLANK-LINES OFF!!)
*     nein
    IF lnmpt_flg IS INITIAL.
*        ?sollen die ausgegebenen Zeilen (weiter-) gezaehlt werden
*        ja
      IF NOT lcl_lncnt IS INITIAL.
*           Zeilenzaehler um eins erhoehen
        blclns_cnt = blclns_cnt + 1 .
      ENDIF.

*        ?wird gerade gedruckt
*        nein
*d       IF sy-ucomm NE 'CSPR'.                             "note 351902
      IF sv_ucomm NE 'CSPR'.                             "note 351902
*           gib die Rahmenstriche aus
        WRITE 1 sy-vline.
        WRITE AT siz_linpf sy-vline.

*           ... und - wenn gewuenscht -
        IF NOT lcl_hidef IS INITIAL.                        "YHG123656
*              ... versorge den HIDE-Bereich
          PERFORM hide_routine.
        ENDIF.                                              "YHG123656
      ELSE.                                                 "YHG140031
        IF sy-colno = 1.                                    "YHG140031
          SKIP.                                             "YHG140031
        ENDIF.                                              "YHG140031
      ENDIF.
    ENDIF.

*     ?sitzt EndOfBlock-Kennzeichen
*     ja
    IF NOT eoblc IS INITIAL.
*        ... dann Schleife beenden
      EXIT.
    ENDIF.

*     Kennzeichen 'erste Zeile ausgeben' zuruecknehmen
    CLEAR: first_swt.
  ENDDO.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        WRITE_LINE                                                   *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM write_line                                             "YHG108937
   USING lcl_lstid
         lcl_profl
         lcl_blcnm
         lcl_first
         lcl_nline.

  DATA: val_shift LIKE sy-cucol.

* ---------------------------------
*  ?wenn nicht gedruckt wird, ...
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
  IF sv_ucomm NE 'CSPR'.                                   "note 351902
*     ... Blockausgabe um eine Stelle nach rechts verschieben
    val_shift = 1 .
*  sonst ...
  ELSE.
*     ... nicht
    CLEAR: val_shift.
  ENDIF.

*  neue Zeile
  NEW-LINE.

*  ?wird gerade gedruckt
*  nein
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
  IF sv_ucomm NE 'CSPR'.                                   "note 351902
*     Leerzeile in Profillaenge + 2 ausgeben
    WRITE AT 2(sav_prfsz) ecfld.
**  ja, es wird gedruckt
*   ELSE.
**     Hintergrundfarben etc. ausschalten
*      FORMAT RESET.
  ENDIF.

*  Zeile endgueltig ausgeben
  CALL FUNCTION 'CS_VLIST_BLOCK_PROCESSING'
    EXPORTING
      blcnm        = lcl_blcnm
      lstid        = lcl_lstid
      profl        = lcl_profl
      first        = lcl_first
      rshift       = val_shift
      newline      = lcl_nline
    IMPORTING
      eoblc        = eoblc
      lnmpt        = lnmpt_flg
    TABLES
      watab        = watab
    EXCEPTIONS
      call_invalid = 4.
ENDFORM.


*----------------------------------------------------------------------*
*       SET_STATUS_GRAY
*----------------------------------------------------------------------*
*       Set all unused ObjTypes gray in Status
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM set_status_gray.                                       "MBA089075
  DATA: BEGIN OF tab OCCURS 5,
          fcode LIKE rsmpe-func,
        END OF tab.

  REFRESH tab.
  IF page_mat IS INITIAL.
    MOVE 'CSSM' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_equi IS INITIAL.
    MOVE 'CSSE' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_prj IS INITIAL.
    MOVE 'CSSP' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_std IS INITIAL.
    MOVE 'CSSS' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_tpl IS INITIAL.
    MOVE 'CSST' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_knd IS INITIAL.
    MOVE 'CSSK' TO tab-fcode.
    APPEND tab.
  ENDIF.

  IF page_exc IS INITIAL.
    MOVE 'CSSX' TO tab-fcode.
    APPEND tab.
  ENDIF.

  SET PF-STATUS 'SA15' EXCLUDING tab.

ENDFORM.                    " SET_STAUS_GRAY


*. Here begins ALV section ............................       "HGA246532
FORM alv_dsp_sel_dsp.
*...................................

  DATA:
    sel_fields_tb    TYPE slis_t_fieldcat_alv,
    wa_sel_fields_tb TYPE slis_fieldcat_alv.

  DATA:
    alvlo_sel TYPE slis_layout_alv.
*....................................

  PERFORM alv_dsp_sel_prep.

  PERFORM alv_evnt_tb_prep
    USING
      'B'
      alv_evnt_tb_pfxt.

  wa_sel_fields_tb-fieldname = 'TEXT'.
  wa_sel_fields_tb-outputlen = 30.
  wa_sel_fields_tb-col_pos   = 1.
  APPEND wa_sel_fields_tb TO sel_fields_tb.

  wa_sel_fields_tb-fieldname = 'WERT'.
  wa_sel_fields_tb-outputlen = 32.
  wa_sel_fields_tb-col_pos   = 2.
  APPEND wa_sel_fields_tb TO sel_fields_tb.

  WRITE text-050 TO alvlo_sel-window_titlebar.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = report_name
      is_layout               = alvlo_sel
      i_save                  = ' '
      it_fieldcat             = sel_fields_tb
      i_default               = ''                                  "NOTE_1382657
      it_events               = alv_evnt_tb_pfxt
      i_screen_start_column   = 7
      i_screen_start_line     = 8
      i_screen_end_column     = 71
      i_screen_end_line       = 16
    IMPORTING
      e_exit_caused_by_caller = exit_by_caller
      es_exit_caused_by_user  = exit_by_user
    TABLES
      t_outtab                = dsp_sel
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc = 0.
    IF exit_by_caller = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF exit_by_user-back = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF exit_by_user-exit = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF exit_by_user-cancel = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.
ENDFORM. "alv_dsp_sel_dsp


FORM alv_dsp_sel_prep.
*...................................

  CHECK dsp_sel[] IS INITIAL.

  READ TEXTPOOL sy-repid INTO txt_sel.

  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = report_name
    TABLES
      selection_table = inp_sel
    EXCEPTIONS
      not_found       = 01
      no_report       = 02.

  LOOP AT inp_sel
    WHERE selname NE 'PM_HEMNG'
      AND selname NE 'PM_HRMNG'
      AND selname NE 'PM_DSPRF'
      AND selname NE 'PM_LTEXT'
      AND selname NE 'PM_PRPRF'.

    LOOP AT txt_sel
*d    WHERE id+1 EQ inp_sel-selname.                                 "uc
      WHERE key EQ inp_sel-selname.                                  "uc

*d    ASSIGN (txt_sel-id+1) TO <pm_name>.                            "uc
      ASSIGN (txt_sel-key) TO <pm_name>.                             "uc
      IF NOT <pm_name> IS INITIAL.
*d      dsp_sel-text = txt_sel-text+8.                               "uc
        dsp_sel-text = txt_sel-entry.                                "uc

        dsp_sel-wert = inp_sel-low.

        IF inp_sel-selname EQ 'PM_DATUV'.
          CLEAR:
            dsp_sel-wert.
          WRITE pm_datuv TO dsp_sel-wert.
        ENDIF.

        IF inp_sel-selname EQ 'PM_DATUB'.
          CLEAR:
            dsp_sel-wert.
          WRITE pm_datub TO dsp_sel-wert.
        ENDIF.

        IF    inp_sel-selname EQ 'PM_EMENG'
          AND NOT pm_emeng IS INITIAL.

          CLEAR:
            dsp_sel-wert.
          WRITE pm_emeng TO dsp_sel-wert DECIMALS 3.

          WHILE dsp_sel-wert(1) EQ space.
            SHIFT dsp_sel-wert LEFT.
          ENDWHILE.
        ENDIF.

        IF    inp_sel-selname EQ 'PM_RMENG'
          AND NOT pm_rmeng IS INITIAL.

          CLEAR:
            dsp_sel-wert.
          WRITE pm_rmeng TO dsp_sel-wert DECIMALS 3.

          WHILE dsp_sel-wert(1) EQ space.
            SHIFT dsp_sel-wert LEFT.
          ENDWHILE.
        ENDIF.

        APPEND dsp_sel.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT dsp_sel BY text.

ENDFORM. "alv_dsp_sel_prep


FORM alv_evnt_tb_prep
  USING
    event_spec TYPE c
    event_tb TYPE slis_t_event.
*..................................

  DATA:
    wa_event_tb TYPE slis_alv_event.
*..................................

  CHECK event_tb[] IS INITIAL.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = event_tb.

  CASE event_spec.
*   complete
    WHEN 'A'.
      READ TABLE event_tb
        WITH KEY name = slis_ev_top_of_page
        INTO wa_event_tb.

      IF sy-subrc = 0.
        wa_event_tb-form = 'ALV_TOP_OF_PAGE'.
        APPEND wa_event_tb TO event_tb.
      ENDIF.


      READ TABLE event_tb
        WITH KEY name = slis_ev_user_command
        INTO wa_event_tb.

      IF sy-subrc = 0.
        wa_event_tb-form = 'ALV_USER_COMMAND'.
        APPEND wa_event_tb TO event_tb.
      ENDIF.


      READ TABLE event_tb
        WITH KEY name = slis_ev_pf_status_set
        INTO wa_event_tb.

      IF sy-subrc = 0.
        wa_event_tb-form = 'ALV_PF_STATUS_SET_MAIN'.
        APPEND wa_event_tb TO event_tb.
      ENDIF.

*   PF EXIT only
    WHEN 'B'.
      READ TABLE event_tb
        WITH KEY name = slis_ev_pf_status_set
        INTO wa_event_tb.

      IF sy-subrc = 0.
        wa_event_tb-form = 'ALV_PF_STATUS_SET_EXIT'.
        APPEND wa_event_tb TO event_tb.
      ENDIF.
  ENDCASE.
ENDFORM. "alv_evnt_tb_prep


FORM alv_ltb_prep.
* nur weiter, wenn eine StuecklistenNr sitzt
  CHECK NOT ltb-stlnr IS INITIAL.

* ?ist LTB-Satz Teil eines Summensatzes
* nein
  IF NOT ltb-sumfg = 'x'.
*   Verwendungsalternative in Anzeigefeld uebernehmen
    dsp_stlal = ltb-vwalt.
*   fuehrende Null ...
    IF dsp_stlal(1) = '0'.
* If ALV is used, do not clear dsp_stlal(1)        "Note 1327742
      IF NOT pm_alvsa IS INITIAL.                  "Note 1327742
*     ... ggf. entfernen
        dsp_stlal(1) = ' '.                        "Note 1327742
      ENDIF.                                       "Note 1327742
    ENDIF.

*   Positionsnummer in Anzeigefeld uebernehmen
    dsp_posnr = ltb-posnr.

*   falls aktueller LTB-Satz ein Summensatz ist, ...
    IF ltb-sumfg = '*'.
      CLEAR: dsp_posnr.
*     ... wird statt einer PosNr ein Stern angezeigt
      dsp_posnr(1) = '*'.
    ENDIF.

    PERFORM alv_ltb_prep_01.
* ja, aktueller LTB-Satz ist Teil eines Summensatzes
  ELSE.
*   Kennzeichen fuer - Leerzeile nach Aufzaehlungsende - setzen
    skipl_flg = 'x'.
*   vorerst keine Anzeige der Einzelsätze im ALV-Modus
*   PERFORM alv_ltb_prep_02.
  ENDIF.
ENDFORM.


FORM alv_ltb_prep_01.
*..................................

  DATA: lkl_matnr LIKE mara-matnr.                            "note 363714
*..................................

  CLEAR:
    alv_ltb,
    ltb_orig,
    ltb_add.

  ltb_orig = ltb.

* am Ende einer Einzelsatzaufzaehlung zu einem Summensatz ...
  IF NOT skipl_flg IS INITIAL.
    IF NOT eopth_flg IS INITIAL.
      CLEAR:
        alv_ltb.
      alv_ltb-info = 'C50'.
      APPEND alv_ltb.

      CLEAR:
        alv_ltb,
        eopth_flg.
    ENDIF.
*   Kennz. Ende Einzelsatzaufzaehlung zuruecknehmen
    CLEAR: skipl_flg.
  ENDIF.

  WRITE ltb-level TO ltb_add-dstuf(2) NO-SIGN.

  CASE ltb-bmtyp.
    WHEN typ_doc.
      CONCATENATE
        doccat-doknr
        doccat-dokar
        doccat-doktl
        doccat-dokvr
        INTO ecfld
        SEPARATED BY space.

*     ltb_add-objic = '@AR@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@AR@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@AR@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_equi.
      CONCATENATE
        equicat-equnr
        equicat-iwerk
        INTO ecfld
        SEPARATED BY space.

*     ltb_add-objic = '@AN@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@AN@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@AN@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_knd.
      CONCATENATE
        kndcat-vbeln
        kndcat-vbpos
        INTO ecfld
        SEPARATED BY space.

*     ltb_add-objic = '@9Z@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@9Z@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@9Z@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_mat.
*ENHANCEMENT-SECTION     RCS15001_L1 SPOTS ES_RCS15001.
      WRITE matcat-matnr TO lkl_matnr.                      "note 363714

      CONCATENATE
*d      matcat-matnr                                        "note 363714
        lkl_matnr                                           "note 363714
        matcat-revlv
        INTO ecfld
        SEPARATED BY space.
*END-ENHANCEMENT-SECTION.

*     ltb_add-objic = '@A6@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@A6@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@A6@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_prj.
      WRITE prjcat-pspnr TO ecfld.
*     ltb_add-objic = '@ED@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@ED@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@ED@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_std.
      WRITE stdcat-stobj TO ecfld.
*     ltb_add-objic = '@00@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@00@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@00@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004

    WHEN typ_tpl.
*      WRITE tplcat-tplnr TO ltb_add-tplnr.                  "N_1739263  "N_2112137
      CONCATENATE
        tplcat-tplnr                                         "N_1739263  "N_2112137
*        ltb_add-tplnr                                       "N_1739263  "N_2112137
        tplcat-iwerk
        INTO ecfld
        SEPARATED BY space.
      WRITE tplcat-tplnr TO ltb_add-dobjt(30).              "N_2112137
      CONCATENATE                                           "N_2112137
        ltb_add-dobjt                                       "N_2112137
        tplcat-iwerk                                        "N_2112137
        INTO ltb_add-dobjt                                  "N_2112137
        SEPARATED BY space.                                 "N_2112137

*     ltb_add-objic = '@AO@'.                                  "Acc 2004
      CALL FUNCTION 'ICON_CREATE'                              "Acc 2004
        EXPORTING                                              "Acc 2004
          name                  = '@AO@'                       "Acc 2004
        IMPORTING                                              "Acc 2004
          result                = ltb_add-objic                "Acc 2004
        EXCEPTIONS                                             "Acc 2004
          icon_not_found        = 1                            "Acc 2004
          outputfield_too_short = 2                            "Acc 2004
          OTHERS                = 3.                           "Acc 2004
      IF sy-subrc <> 0.                                     "Acc 2004
        MOVE: '@AO@' TO ltb_add-objic.                      "Acc 2004
      ENDIF.                                                "Acc 2004
      ltb_add-tplnr = tplcat-tplnr.                           "N_1739263   "N_2112137

  ENDCASE.

*d CONDENSE ecfld.                                          "note 515408
  IF ltb_add-dobjt IS INITIAL.                              "N_2112137
    ltb_add-dobjt = ecfld(40).
  ENDIF.                                                    "N_2112137
  CLEAR: ecfld.

  ltb_add-dstal = dsp_stlal.
  ltb_add-dposn = dsp_posnr.

  IF prv_extrm IS INITIAL.
    WRITE dsp_imeng TO ltb_add-dimng(15).
    ltb_add-meopo = opo_meinh.

    IF    ltb-fmeng IS INITIAL
      OR  (     NOT ltb-emeng IS INITIAL
            AND NOT ltb-fxmng IS INITIAL ) .

      WRITE dsp_rmeng TO ltb_add-drmng(15).
    ELSE.
      WRITE text-040 TO ltb_add-comfx(4).
      CLEAR:
        ltb_add-drmng,
        ltb_orig-bmein.
    ENDIF.

    IF pm_emeng =  0.
      IF dsp_imeng >= max_num.
        WRITE '*' TO ltb_add-imovf.
      ENDIF.
    ELSE.
      IF pm_rmeng =  0.
        IF dsp_rmeng >= max_num.
          WRITE '*' TO ltb_add-rmovf.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    IF prv_extrm EQ '+'.
      WRITE qnt_any TO ltb_add-dimng+11(3).
      CLEAR:
        ltb_add-drmng(15),
        ltb_add-meopo,
        ltb_orig-bmein.
    ELSE.
      WRITE dsp_rmeng TO ltb_add-drmng(15).
      WRITE qnt_none TO ltb_add-dimng+11(3).
      CLEAR: ltb_add-meopo.
    ENDIF.
  ENDIF.

  IF NOT ltb-loekz IS INITIAL.
    ltb_add-loefg = 'X'.
  ENDIF.

  IF NOT ltb-knobj IS INITIAL.                              "note 750173
    ltb_add-knofl = 'X'.                                   "note 750173
  ENDIF.                                                    "note 750173

*d  MOVE-CORRESPONDING ltb_orig TO alv_ltb.                 "note 331962
  MOVE-CORRESPONDING ltb_add  TO alv_ltb.
  MOVE-CORRESPONDING ltb_orig TO alv_ltb.                   "note 331962

  alv_ltb-index = ltb_loopx.
  APPEND alv_ltb.

  PERFORM separator_at_end.                                 "N_2639218

ENDFORM.


FORM alv_ltb_prep_02.
  ltb_orig = ltb.
  CLEAR: ltb_add.

  IF    dsp_imeng = 0
    AND ltb-emeng <> 0.

    WRITE qnt_none TO ltb_add-dimng+11(3).
    CLEAR:
      ltb_add-meopo.
  ELSE.
    WRITE dsp_imeng TO ltb_add-dimng(15).
    ltb_add-meopo = opo_meinh.
  ENDIF.

  ltb_add-dposn = ltb-posnr.

  IF NOT ltb-fmeng IS INITIAL.
    WRITE text-040 TO ltb_add-comfx(4).
  ENDIF.

  IF pm_emeng =  0.
    IF dsp_imeng >= max_num.
      WRITE '*' TO ltb_add-imovf.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING ltb_orig TO alv_ltb.
  MOVE-CORRESPONDING ltb_add  TO alv_ltb.
  APPEND alv_ltb.
ENDFORM.


FORM alv_pf_status_set_exit
  USING
    rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'SNN1' OF PROGRAM 'RCS11001'
    EXCLUDING rt_extab.
ENDFORM. "alv_pf_status_set_exit


FORM alv_pf_status_set_main
  USING
    rt_extab TYPE slis_t_extab.

*dSET PF-STATUS 'SA15_ALV'.                                 "note 355115
  SET PF-STATUS 'SA15_ALV'                                  "note 355115
    EXCLUDING rt_extab.                                     "note 355115
ENDFORM. "alv_pf_status_set_main


FORM alv_top_of_page.
*.....................................

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_top_tb.
ENDFORM. "alv_top_of_page


FORM alv_top_tb_prep
  USING
    top_tb TYPE slis_t_listheader.
*......................................

  DATA:
    wa_top_tb TYPE slis_listheader.

  DATA:
    lkl_matnr LIKE mara-matnr.                                "note 363714
*......................................

* CLEAR wa_top_tb.
* wa_top_tb-typ  = 'H'.
* CONCATENATE
*   text-001
*   text-002
*   INTO wa_top_tb-info
*   SEPARATED BY space(1).
* APPEND wa_top_tb TO top_tb.

  CLEAR wa_top_tb.
  wa_top_tb-typ  = 'S'.
  ecfld = text-007.
  wa_top_tb-key  = ecfld(11).
  CLEAR: ecfld.
*ENHANCEMENT-SECTION     RCS15001_L2 SPOTS ES_RCS15001.
  WRITE pm_idnrk TO lkl_matnr.                              "note 363714
*d wa_top_tb-info = pm_idnrk.                               "note 363714
  wa_top_tb-info = lkl_matnr.                               "note 363714
*END-ENHANCEMENT-SECTION.
  APPEND wa_top_tb TO top_tb.

  CLEAR wa_top_tb.
  wa_top_tb-typ  = 'S'.
  wa_top_tb-key = text-017.
  CONDENSE wa_top_tb-key.
  wa_top_tb-info = selpool-maktx.
  APPEND wa_top_tb TO top_tb.

  CLEAR wa_top_tb.
  wa_top_tb-typ  = 'S'.
  ecfld = text-021.
  wa_top_tb-key  = ecfld(11).
  CLEAR: ecfld.
  WRITE pm_datuv TO wa_top_tb-info.
  APPEND wa_top_tb TO top_tb.

  CLEAR wa_top_tb.
  wa_top_tb-typ  = 'S'.
* so it looks better
  APPEND wa_top_tb TO top_tb.
ENDFORM. "alv_top_tb_prep


FORM alv_user_command
  USING i_ucomm LIKE sy-ucomm
    i_selfield TYPE slis_selfield.
*.......................................

  CASE i_ucomm.
    WHEN 'ANMT'.
      LEAVE TO TRANSACTION 'CS15'.

    WHEN 'CSAP' OR '&IC1'.
      READ TABLE alv_ltb INDEX i_selfield-tabindex.
      IF NOT alv_ltb-index IS INITIAL.
        READ TABLE ltb INDEX alv_ltb-index.
        PERFORM get_objdata.
      ELSE.
        CLEAR:
          ltb,
          doccat,
          equicat,
          kndcat,
          matcat,
          prjcat,
          stdcat,
          tplcat.
      ENDIF.

      PERFORM position_anzeigen.

    WHEN 'CSAO'.
      READ TABLE alv_ltb INDEX i_selfield-tabindex.
      IF NOT alv_ltb-index IS INITIAL.
        READ TABLE ltb INDEX alv_ltb-index.
        PERFORM get_objdata.
      ELSE.
        CLEAR:
          ltb,
          doccat,
          equicat,
          kndcat,
          matcat,
          prjcat,
          stdcat,
          tplcat.
      ENDIF.

      CASE ltb-stlty.
*       ... Dokumentstueckliste
        WHEN typ_doc.
          PERFORM dokument_anzeigen.
*       ... Materialstueckliste
        WHEN typ_mat.
          PERFORM material_anzeigen.
*       ... Equipmentstueckliste
        WHEN typ_equi.
          PERFORM equi_anzeigen.
*       ... KndStueckliste
        WHEN typ_knd.
          PERFORM knd_anzeigen.
*       ... TechnPlatzstueckliste
        WHEN typ_tpl.
          PERFORM tpl_anzeigen.
*       ... PrjStueckliste
        WHEN typ_prj.
          PERFORM prj_anzeigen.
*       ... unbekannt
        WHEN OTHERS.
*         Cursor steht auf ungueltiger Zeile
          MESSAGE s150.
      ENDCASE.


    WHEN 'CSVA'.
      READ TABLE alv_ltb INDEX i_selfield-tabindex.
      IF NOT alv_ltb-index IS INITIAL.
        READ TABLE ltb INDEX alv_ltb-index.
        PERFORM get_objdata.
      ELSE.
        CLEAR:
          ltb,
          doccat,
          equicat,
          kndcat,
          matcat,
          prjcat,
          stdcat,
          tplcat.
      ENDIF.

      PERFORM verwendung_anzeigen.

    WHEN 'CSSL'.
      PERFORM alv_dsp_sel_dsp.

    WHEN 'CSSX'.
      PERFORM alv_xcpt_tb_dsp.

  ENDCASE.
ENDFORM. "alv_user_command


FORM alv_xcpt_tb_dsp.
*.......................................

  DATA:
    xcpt_fields_tb    TYPE slis_t_fieldcat_alv,
    wa_xcpt_fields_tb TYPE slis_fieldcat_alv.

  DATA:
    alvlo_xcpt TYPE slis_layout_alv.
*.......................................

  PERFORM alv_xcpt_tb_prep.

  PERFORM alv_evnt_tb_prep
    USING
      'B'
      alv_evnt_tb_pfxt.

  wa_xcpt_fields_tb-fieldname = 'DOBJT'.
  wa_xcpt_fields_tb-outputlen = 42.
  wa_xcpt_fields_tb-col_pos   = 1.
  APPEND wa_xcpt_fields_tb TO xcpt_fields_tb.

  wa_xcpt_fields_tb-fieldname = 'OJTXP'.
  wa_xcpt_fields_tb-outputlen = 40.
  wa_xcpt_fields_tb-col_pos   = 2.
  APPEND wa_xcpt_fields_tb TO xcpt_fields_tb.

  WRITE text-030 TO alvlo_xcpt-window_titlebar.
  alvlo_xcpt-info_fieldname = 'INFO'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = report_name
      is_layout               = alvlo_xcpt
      i_save                  = ' '
      it_fieldcat             = xcpt_fields_tb
      it_events               = alv_evnt_tb_pfxt
      i_screen_start_column   = 4
      i_screen_start_line     = 4
      i_screen_end_column     = 87
      i_screen_end_line       = 12
    IMPORTING
      e_exit_caused_by_caller = exit_by_caller
      es_exit_caused_by_user  = exit_by_user
    TABLES
      t_outtab                = xcpt_tb
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc = 0.
    IF exit_by_caller = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF exit_by_user-back = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF exit_by_user-exit = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF exit_by_user-cancel = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.
ENDFORM. "alv_xcpt_tb_dsp


FORM alv_xcpt_tb_prep.
*...................................

  DATA:
    BEGIN OF wa_xcpt_tb,
      dobjt(42) TYPE c,
      ojtxp(40) TYPE c,
      info(3)   TYPE c,
    END OF wa_xcpt_tb,

    dontshift(2) TYPE c VALUE '. ',

    lkl_savix    LIKE sy-tabix,
    lkl_ausnm    LIKE stpox-ausnm.
*..................................

  CHECK xcpt_tb[] IS INITIAL.

  CLEAR:
    wa_xcpt_tb,
    lkl_savix,
    lkl_ausnm.

  READ TABLE excpt INDEX 1.
  CHECK sy-subrc = 0 .

  SORT excpt
    ASCENDING
    BY ausnm
       stlty ASCENDING.

  CLEAR:
    ltb_add.

  LOOP AT excpt.
    IF lkl_ausnm NE excpt-ausnm.
      lkl_ausnm = excpt-ausnm.
      lkl_savix = sy-tabix.

      CLEAR:
        wa_xcpt_tb.

      IF sy-tabix > 1.
        APPEND wa_xcpt_tb TO xcpt_tb.
      ENDIF.

      CASE excpt-ausnm.
        WHEN 'NBER'.
          CLEAR: auth_cnt.
          LOOP AT excpt FROM lkl_savix.
            IF excpt-ausnm NE lkl_ausnm.
              READ TABLE excpt INDEX lkl_savix.
              EXIT.
            ENDIF.

            auth_cnt = auth_cnt + 1 .
          ENDLOOP.

          WRITE text-113 TO wa_xcpt_tb(76).
          WRITE auth_cnt TO wa_xcpt_tb+54(3).
          CLEAR: auth_cnt.

        WHEN 'DELE'.
          WRITE text-110 TO wa_xcpt_tb.
        WHEN 'NREK'.
          WRITE text-111 TO wa_xcpt_tb.
        WHEN 'REKU'.
          WRITE text-112 TO wa_xcpt_tb.
        WHEN 'CONV'.
          WRITE text-115 TO wa_xcpt_tb.
      ENDCASE.

      wa_xcpt_tb-info = 'C30'.
      APPEND wa_xcpt_tb TO xcpt_tb.
    ENDIF.

    CHECK lkl_ausnm NE 'NBER'.                              "note 615004

    CLEAR:
      ecfld,
      wa_xcpt_tb.

    ecfld = dontshift.
    CASE excpt-stlty.
      WHEN typ_doc.
        ecfld+2  = excpt-stlty.
        ecfld+14 = excpt-bgdoc.

      WHEN typ_equi.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgequ.

      WHEN typ_knd.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgknd.

      WHEN typ_mat.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgmat.

      WHEN typ_prj.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgprj.

      WHEN typ_std.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgstd.

      WHEN typ_tpl.
        ecfld+2  = excpt-stlty.
        ecfld+4  = excpt-stlan.
        ecfld+6  = excpt-xtlal.
        ecfld+9  = excpt-zwerk.
        ecfld+14 = excpt-bgtpl.

      WHEN OTHERS.
    ENDCASE.

    wa_xcpt_tb-dobjt = ecfld.
    wa_xcpt_tb-ojtxp = excpt-ktext.
    APPEND wa_xcpt_tb TO xcpt_tb.
  ENDLOOP.
ENDFORM. "alv_xcpt_tb_prep


FORM cs15_alv.
  CLEAR: outpt_flg.

  mng_stack-stufe = 1 .
  mng_stack-emeng = pm_emeng.
  mng_stack-rmeng = pm_rmeng.
  mng_stack-emfac = 1 .
  APPEND mng_stack.

  CLEAR: cng_level.

  LOOP AT ltb.
*d  CHECK chk_types CA ltb-bmtyp                            "note 615004
*d    AND ltb-bmtyp NE space.                               "note 615004
    CHECK chk_types CA ltb-bmtyp.                           "note 615004

    IF ojtop_mrk IS INITIAL.
      ojtop_mrk = ltb-stlty.
    ENDIF.

    ltb_loopx = sy-tabix.

    IF ltb-bmtyp NE space.                                  "note 615004
*   -->                                                     "note 615004
      IF ltb-level NE cng_level.
        cng_level = ltb-level.

        IF NOT outpt_flg IS INITIAL.
          IF mng_stack-stufe < ltb-level .
            mng_stack-stufe = ltb-level.

            IF    lst_bmein NE ltb-emeih
              AND NOT lst_bmein IS INITIAL.

              CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
                EXPORTING
                  input    = sav_frmng
                  kzmeinh  = 'X'
                  matnr    = matcat-matnr
                  meinh    = lst_bmein
                  meins    = ltb-emeih
                  type_umr = '3'
                IMPORTING
                  output   = sav_frmng
                             EXCEPTIONS
                             conversion_not_found
                             input_invalid
                             material_not_found
                             meinh_not_found
                             meins_missing
                             no_meinh
                             output_invalid
                             overflow.

              CLEAR: lst_bmein.
            ENDIF.

            mng_stack-emeng = sav_frmng.
            mng_stack-rmeng = 0.
            mng_stack-emfac = act_emfac.
            mng_stack-extrm = act_extrm.
            mng_stack-ldsgn = act_ldsgn.

            APPEND mng_stack.
            prv_extrm = act_extrm.
          ELSE.
            DESCRIBE TABLE mng_stack LINES sy-tabix.
            WHILE mng_stack-stufe > ltb-level.
              DELETE mng_stack INDEX sy-tabix.
              sy-tabix = sy-tabix - 1.
              READ TABLE mng_stack INDEX sy-tabix.
              prv_extrm = mng_stack-extrm.
            ENDWHILE.

            IF skipl_flg IS INITIAL.
              PERFORM determine_high_level.                 "N_2639218
              CLEAR: alv_ltb.
              alv_ltb-info = 'C30'.
              APPEND alv_ltb.
              PERFORM determine_index.                      "N_2639218
              CLEAR: alv_ltb.
            ELSE.
              eopth_flg = 'x'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM get_ldm_switch(saplcsdi) CHANGING switch_values IF FOUND. "N_2639218
      IF NOT switch_values-cs15_top_lvl IS INITIAL.         "N_2639218
        READ TABLE ltb INDEX ltb_loopx.                     "N_2639218
      ENDIF.                                                "N_2639218

      IF    NOT pm_rmeng IS INITIAL
        AND NOT ltb-sumfg EQ 'x'.

        act_emfac = ( ltb-emeng + ltb-fxmng )
                    / ltb-bmeng .
      ENDIF.

      PERFORM get_objdata.
*   <--                                                     "note 615004
    ENDIF.                                                  "note 615004

    PERFORM keep_excpt.

    CHECK ltb-lstfg IS INITIAL.

    PERFORM mng_dsp_new.

    IF    ojtop_mrk NE ltb-stlty
      AND NOT ojtop_mrk IS INITIAL
      AND NOT ltb-stlty IS INITIAL
      AND NOT outpt_flg IS INITIAL.

      ojtop_mrk = ltb-stlty.

      CLEAR: alv_ltb.
      alv_ltb-info = 'C50'.
      APPEND alv_ltb.
      CLEAR: alv_ltb.
    ENDIF.

    PERFORM alv_ltb_prep.

    IF outpt_flg IS INITIAL.
      outpt_flg = 'x'.
    ENDIF.

    sy-tabix = ltb_loopx.
  ENDLOOP.

  PERFORM set_toplevel_flag.                                "N_2639218

* IF outpt_flg IS INITIAL.                                 "note 878684
  IF outpt_flg IS INITIAL AND ltb[] IS INITIAL.            "note 878684
*   Fehler: keine Verwendung selektiert
    MESSAGE s507 WITH 'E: ' pm_idnrk.
    CLEAR: eolst_flg.

    EXIT.
  ENDIF.

  PERFORM ltb_fields_tb_prep.

  alvlo_ltb-info_fieldname = 'INFO'.
*ENHANCEMENT-POINT RCS15001_L3 SPOTS ES_RCS15001.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = report_name
      i_structure_name        = 'STPOV_ALV'
      is_layout               = alvlo_ltb
      i_save                  = alvvr_sav_all
      is_variant              = alvvr
      it_events               = alv_evnt_tb_cmpl
      it_fieldcat             = ltb_fields_tb
    IMPORTING
      e_exit_caused_by_caller = exit_by_caller
      es_exit_caused_by_user  = exit_by_user
    TABLES
      t_outtab                = alv_ltb
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc = 0.
    IF exit_by_caller = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF exit_by_user-back = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF exit_by_user-exit = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF exit_by_user-cancel = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE s513 WITH 'E: '.
    EXIT.
  ENDIF.
ENDFORM.


FORM ltb_fields_tb_prep.
*.....................................

*dCALL FUNCTION 'GET_FIELDTAB'                                "uc 070302
*d  EXPORTING                                                 "uc 070302
*d    langu    = sy-langu                                     "uc 070302
*d    tabname  = 'STPOV_ALV'                                  "uc 070302
*d    withtext = ' '                                          "uc 070302
*d    only     = 'T'                                          "uc 070302
*d  TABLES                                                    "uc 070302
*d    fieldtab = ftab                                         "uc 070302
*d  EXCEPTIONS                                                "uc 070302
*d    OTHERS   = 1.                                           "uc 070302

  CALL FUNCTION 'DDIF_FIELDINFO_GET'                          "uc 070302
    EXPORTING                                                 "uc 070302
      langu     = sy-langu                         "uc 070302
      tabname   = 'STPOV_ALV'                      "uc 070302
*     UCLEN     = '01'                             "uc 070302
    TABLES                                                    "uc 070302
      dfies_tab = ftab                             "uc 070302
    EXCEPTIONS                                                "uc 070302
      OTHERS    = 1.                               "uc 070302

  LOOP AT ftab.
    CLEAR: wa_ltb_fields_tb.

    CASE ftab-fieldname.
      WHEN 'DSTUF'.
        wa_ltb_fields_tb-fieldname = 'DSTUF'.
        wa_ltb_fields_tb-col_pos   =  1.
        wa_ltb_fields_tb-outputlen = 2 .
        wa_ltb_fields_tb-just      = 'R' .
        wa_ltb_fields_tb-fix_column = 'X' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'STLAN'.
        wa_ltb_fields_tb-fieldname = 'STLAN'.
        wa_ltb_fields_tb-col_pos   =  2.
        wa_ltb_fields_tb-fix_column =  'X' .
        wa_ltb_fields_tb-outputlen = 1 .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'WERKS'.
        wa_ltb_fields_tb-fieldname = 'WERKS'.
        wa_ltb_fields_tb-col_pos   =  3.
        wa_ltb_fields_tb-fix_column =  'X' .
        wa_ltb_fields_tb-outputlen = 4 .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'OBJIC'.
        wa_ltb_fields_tb-fieldname = 'OBJIC'.
        wa_ltb_fields_tb-col_pos   =  4.
        wa_ltb_fields_tb-fix_column =  'X' .
        wa_ltb_fields_tb-outputlen = 3 .
        wa_ltb_fields_tb-icon       =  'X' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'DOBJT'.
        wa_ltb_fields_tb-fieldname = 'DOBJT'.
        wa_ltb_fields_tb-col_pos   =  5.
        wa_ltb_fields_tb-fix_column =  'X' .
        wa_ltb_fields_tb-outputlen = 23 .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

*     WHEN 'OJTXB'.
*       wa_ltb_fields_tb-fieldname = 'OJTXB'.
*       wa_ltb_fields_tb-col_pos   =  6.
*       wa_ltb_fields_tb-outputlen = 24.
*       APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'DSTAL'.
        wa_ltb_fields_tb-fieldname = 'DSTAL'.
        wa_ltb_fields_tb-col_pos   =  7.
        wa_ltb_fields_tb-outputlen = 2 .
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'DPOSN'.
        wa_ltb_fields_tb-fieldname = 'DPOSN'.
        wa_ltb_fields_tb-col_pos   =  8.
        wa_ltb_fields_tb-outputlen = 4 .
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'IMOVF'.
        wa_ltb_fields_tb-fieldname = 'IMOVF'.
        wa_ltb_fields_tb-col_pos   = 9.
        wa_ltb_fields_tb-outputlen = 1 .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'DIMNG'.
        wa_ltb_fields_tb-fieldname = 'DIMNG'.
        wa_ltb_fields_tb-col_pos   = 10.
        wa_ltb_fields_tb-outputlen = 18.
        wa_ltb_fields_tb-no_sum    = 'X'.
        wa_ltb_fields_tb-no_zero   = 'X'.
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'MEOPO'.
        wa_ltb_fields_tb-fieldname = 'MEOPO'.
        wa_ltb_fields_tb-col_pos   = 11.
        wa_ltb_fields_tb-outputlen = 3 .
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'RMOVF'.
        wa_ltb_fields_tb-fieldname = 'RMOVF'.
        wa_ltb_fields_tb-col_pos   = 12.
        wa_ltb_fields_tb-outputlen = 1 .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'DRMNG'.
        wa_ltb_fields_tb-fieldname = 'DRMNG'.
        wa_ltb_fields_tb-col_pos   = 13.
        wa_ltb_fields_tb-outputlen = 18.
        wa_ltb_fields_tb-no_sum    = 'X'.
        wa_ltb_fields_tb-no_zero   = 'X'.
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

      WHEN 'BMEIN'.
        wa_ltb_fields_tb-fieldname = 'BMEIN'.
        wa_ltb_fields_tb-col_pos   = 14.
        wa_ltb_fields_tb-outputlen = 3 .
        wa_ltb_fields_tb-just      = 'R' .
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

*Start of insertion for Note 2639218
      WHEN 'CRTFG'.
        PERFORM get_ldm_switch(saplcsdi) CHANGING switch_values IF FOUND.
        IF NOT switch_values-cs15_top_lvl IS INITIAL.
          wa_ltb_fields_tb-fieldname = 'CRTFG'.
          wa_ltb_fields_tb-col_pos   = 15.
          wa_ltb_fields_tb-outputlen = 9 .
          wa_ltb_fields_tb-just      = 'R' .
          wa_ltb_fields_tb-seltext_s = text-116.
          wa_ltb_fields_tb-seltext_m = text-116.
          wa_ltb_fields_tb-seltext_l = text-116.
          APPEND wa_ltb_fields_tb TO ltb_fields_tb.
        ENDIF.
*End of insertion for Note 2639218

      WHEN OTHERS.
        wa_ltb_fields_tb-fieldname = ftab-fieldname.
        wa_ltb_fields_tb-no_out    = 'X'.
        wa_ltb_fields_tb-no_sum    = 'X'.
        APPEND wa_ltb_fields_tb TO ltb_fields_tb.

    ENDCASE.
  ENDLOOP.
ENDFORM. "ltb_fields_tb_prep
*. Here ends ALV section ..............................       "HGA246532
