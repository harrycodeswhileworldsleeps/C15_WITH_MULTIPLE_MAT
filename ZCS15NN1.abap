***********************************************************************
*        R C S 1 5 N N 1           Programme   R C S 1 5 0 0 1        *
*                                              R C S 1 5 0 1 1        *
*                                              R C S 1 5 0 2 1        *
***********************************************************************
*eject
TABLES:                                                       "HGA025100
   tcsvl,                                                     "HGA025100
   usr02.                                                     "HGA025100

DATA:                                                         "YHG032486
   shortln(30) TYPE c VALUE '______________________________', "YHG032486
   i28         TYPE i VALUE 28.                               "YHG032486

DATA:
   tmp_cnt     TYPE i.                                        "YHG032486

DATA: usr_class LIKE usr02-class.                             "HGA025100


*eject
* ---------------------------------
AT USER-COMMAND.
   SV_UCOMM = SY-UCOMM.                                     "note 351902

   CASE sy-ucomm.
*     Anderes Dokument
      WHEN 'ANDK'.
         LEAVE TO TRANSACTION 'CSD5'.                         "YHG135858
*     Anderes Material
      WHEN 'ANMT'.
*del     LEAVE TO TRANSACTION SY-TCODE.                       "YHG135858
         LEAVE TO TRANSACTION 'CS15'.                         "YHG135858
*     Andere Klasse
      WHEN 'ANCL'.                                            "YHG077515
*del     LEAVE TO TRANSACTION SY-TCODE.             "YHG077515"YHG135858
         LEAVE TO TRANSACTION 'CSC5'.                         "YHG135858
*     Position anzeigen
      WHEN 'CSAP'.
         PERFORM position_anzeigen.
*     Objektdaten (Material, Equi) anzeigen
      WHEN 'CSAO'.
*        Stueckliste ist ...
         CASE ltb-stlty.
*           ... Dokumentstueckliste
            WHEN typ_doc.                                     "YHG136936
               PERFORM dokument_anzeigen.                     "YHG136936
*           ... Materialstueckliste
            WHEN typ_mat.
               PERFORM material_anzeigen.
*           ... Equipmentstueckliste
            WHEN typ_equi.
               PERFORM equi_anzeigen.
*           ... KndStueckliste                                "MBA089075
            WHEN typ_knd.                                     "MBA089075
               PERFORM knd_anzeigen.                          "MBA089075
*           ... TechnPlatzstueckliste
            WHEN typ_tpl.                                     "HGD099459
               PERFORM tpl_anzeigen.                          "HGD099459
*           ... PrjStueckliste                                "MBA089075
            WHEN typ_prj.                                     "MBA089075
               PERFORM prj_anzeigen.                          "MBA089075
*           ... unbekannt
            WHEN OTHERS.
*              Cursor steht auf ungueltiger Zeile
               MESSAGE s150.
         ENDCASE.
*     Beenden
      WHEN 'CSEN'.
         IF cal_aclas = 'CC  '.                               "HGD072824
            LEAVE.                                            "HGD072824
         ELSE.                                                "HGD072824
            LEAVE TO TRANSACTION '    '.
         ENDIF.                                               "HGD072824
*     Abbrechen
      WHEN 'ABBR'.
         LEAVE.
*     Verwendung
*del  WHEN 'CSMV'.                                            "YHG135858
      WHEN 'CSVA'.                                            "YHG135858
         PERFORM verwendung_anzeigen.
*     Drucken
      WHEN 'CSPR'.
         PERFORM print_list.
*     gehe zu Objekt Dokument
      WHEN 'CSSD'.                                            "YHG135858
         CHECK NOT page_doc IS INITIAL.                       "YHG136936
         SCROLL LIST TO PAGE page_doc.                        "YHG135858
*     gehe zu Objekt Standardobjekt
      WHEN 'CSSS'.                                            "YHG110068
         CHECK NOT page_std IS INITIAL.                       "YHG136936
         SCROLL LIST TO PAGE page_std.                        "YHG110068
*     gehe zu Objekt TechPlatz
      WHEN 'CSST'.                                            "YHG110068
         CHECK NOT page_tpl IS INITIAL.                       "YHG136936
         SCROLL LIST TO PAGE page_tpl.                        "YHG110068
*     gehe zu Objekt Equipment
      WHEN 'CSSE'.
         CHECK NOT page_equi IS INITIAL.                      "YHG136936
         SCROLL LIST TO PAGE page_equi.
*     gehe zu Objekt Material
      WHEN 'CSSM'.
         CHECK NOT page_mat IS INITIAL.                       "YHG136936
         SCROLL LIST TO PAGE page_mat.
*     gehe zu Kundenauftragsstückliste
      WHEN 'CSSK'.                                             "MB075252
         CHECK NOT page_knd IS INITIAL.                        "MB075252
         SCROLL LIST TO PAGE page_knd.                         "MB075252
*     gehe zu Projektstückliste
      WHEN 'CSSP'.                                            "MBA089075
         CHECK NOT page_prj IS INITIAL.                       "MBA089075
         SCROLL LIST TO PAGE page_prj.                        "MBA089075
*     gehe zu Ausnahmehinweisen
      WHEN 'CSSX'.
         CHECK NOT page_exc IS INITIAL.                       "YHG136936
         SCROLL LIST TO PAGE page_exc.
*     Hilfe
      WHEN 'CSHP'.                                            "YHG109837
         PERFORM help_me.                                     "YHG109837
*     Selektionsparameter
      WHEN 'CSSL'.                                            "YHG139715
         PERFORM parameter_anzeigen.                          "YHG139715
   ENDCASE.
   PERFORM clr_hide_area.                                     "YHG123656

*eject
***********************************************************************
*        F O R M  -  Routinen                                         *
***********************************************************************
*eject
*---------------------------------------------------------------------*
*        ACT_MEINH                                                    *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM act_meinh.
*  falls erste Stufe,
   IF ltb-level = 1.
      IF     pm_emeng IS INITIAL                              "YHG095076
         AND ltb-sumfg NE '*'.                                "YHG095076

*        Mengeneinheit der OriginalPosition merken (zur Anzeige)
         opo_meinh = ltb-meins.                               "YHG095076
         opo_menge = ltb-menge.                               "HGA115686
      ELSE.
*        BasisMngEinht der OriginalPosition merken (zur Anzeige)
         opo_meinh = ltb-emeih.
         opo_menge = pm_emeng.                                "HGA115686

         IF ltb-msign EQ '-'.                                 "HGA115915
            opo_menge = opo_menge * -1 .                      "HGA115915
         ENDIF.                                               "HGA115915
      ENDIF.                                                  "YHG095076
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        AUTH_CNT_CHK                                                 *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM auth_cnt_chk USING lcl_nopos.
* ---------------------------------
DATA: act_linno LIKE sy-linno.

* ---------------------------------
*  nur weiter, wenn unberechtigte Zugriffe gezaehlt wurden
   CHECK auth_cnt <> 0.

*  ?sind unber. Zugriffe die letzten AusnmHinweise in der EXCPT
*  nein, es kommen noch andere
   IF lcl_nopos IS INITIAL.
*     ?Datenaufbereitung im Druckmodus
*     nein
*d    IF sy-ucomm NE 'CSPR'.                                "note 351902
      IF SV_UCOMM NE 'CSPR'.                                "note 351902
*        Positionierzeile fuer Bildschirmausgabe festlegen
         act_linno = sy-linno - 1 .
*     ja, Druck- bzw. Batchmodus aktiv
      ELSE.
*        Positionierzeile fuer Druckausgabe festlegen
         act_linno = sy-linno - 2.
      ENDIF.

*     zurueck auf Positionierzeile
      SKIP TO LINE act_linno.
*     Anzahl unberech. Zugriffe ausgeben
      WRITE: /57     auth_cnt
                     COLOR COL_BACKGROUND INTENSIFIED OFF.
      SKIP.
*  ja, unber. Zugriffe sind die letzten auszugebenden AusnmHinweise
   ELSE.
*     Anzahl unberech. Zugriffe ausgeben
      WRITE:  57     auth_cnt
                     COLOR COL_BACKGROUND INTENSIFIED OFF.
   ENDIF.

*  Zaehler der unber. Zugriffe initialisieren
   CLEAR: auth_cnt.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        CLR_HIDE_AREA                                                *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM clr_hide_area.
   CLEAR: ltb-werks,
*del      LTB-IWERK,                                          "YHG135858
          ltb-stlan,
          ltb-stlty,
          ltb-idnrk,
          ltb-vwalt,
          ltb-stlkn,
          ltb-stpoz,                                          "YHG115627
*del      LTB-EQUNR,                                          "YHG135858
*del      LTB-MATNR.                                          "YHG135858
          doccat-doknr,                                       "YHG135858
          doccat-dokar,                                       "YHG135858
          doccat-doktl,                                       "YHG135858
          doccat-dokvr,                                       "YHG135858
          equicat-equnr,                                      "YHG135858
          equicat-iwerk,                                      "YHG135858
          kndcat-vbeln,                                       "YHG135858
          kndcat-vbpos,                                       "YHG135858
          kndcat-vbeln,                                       "YHG135858
          kndcat-matnr,                                       "YHG135858
          matcat-matnr,                                       "YHG135858
          prjcat-pspnr,                                       "MBA089075
          prjcat-matnr,                                       "MBA089075
          stdcat-stobj,                                       "YHG135858
          tplcat-tplnr,                                       "YHG135858
          tplcat-iwerk.                                       "YHG135858
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        DOKUMENT_ANZEIGEN                         USER-COMMAND: CSAD *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM dokument_anzeigen.                                       "YHG136936
* ---------------------------------
DATA: lkl_dokar LIKE draw-dokar,
      lkl_doknr LIKE draw-doknr,
      lkl_dokvr LIKE draw-dokvr,
      lkl_doktl LIKE draw-doktl.
* ---------------------------------

   IF     doccat-dokar IS INITIAL
      OR  doccat-doknr IS INITIAL
      OR  doccat-dokvr IS INITIAL
      OR  doccat-doktl IS INITIAL.

      MESSAGE s150.

      CHECK     NOT doccat-dokar IS INITIAL
            AND NOT doccat-doknr IS INITIAL
            AND NOT doccat-dokvr IS INITIAL
            AND NOT doccat-doktl IS INITIAL.
   ENDIF.


   GET PARAMETER: ID 'CV2' FIELD lkl_dokar,
                  ID 'CV1' FIELD lkl_doknr,
                  ID 'CV3' FIELD lkl_dokvr,
                  ID 'CV4' FIELD lkl_doktl.

   SET PARAMETER: ID 'CV2' FIELD doccat-dokar,
                  ID 'CV1' FIELD doccat-doknr,
                  ID 'CV3' FIELD doccat-dokvr,
                  ID 'CV4' FIELD doccat-doktl.

*d CALL TRANSACTION 'CV03' AND SKIP FIRST SCREEN.             "HGC201257
   CALL TRANSACTION 'CV03N' AND SKIP FIRST SCREEN.            "HGC201257

   SET PARAMETER: ID 'CV2' FIELD lkl_dokar,
                  ID 'CV1' FIELD lkl_doknr,
                  ID 'CV3' FIELD lkl_dokvr,
                  ID 'CV4' FIELD lkl_doktl.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        CREATE_TXINCL_CMD                                            *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM create_txincl_cmd.                                       "HGB057558
DATA: chk_nxt  LIKE csdata-xfeld,
      txt_ridx LIKE sy-tabix,
      txt_offs TYPE i,
      hlp_flin LIKE tline-tdline.
*----------------------------------

   CLEAR: chk_nxt.
   LOOP AT txt.
*     ?war die Vorgaengerzeile eine INCLUDE-Anweisung
*     ja
      IF NOT chk_nxt IS INITIAL.
*        ?ist akt. Zeile mögliche Fortsetzung der Vorgaengerzeile
*        ja, es ist eine Kommentarzeile
         IF txt-tdformat EQ '/*'.
*           bestimme Endpos. des beschriebenen Abschnitts von HLP_FLIN
            IF hlp_flin CP '*# '. ENDIF.
*           ?liegt Endeposition am Feldende
*           ja
            IF sy-fdpos >= 132.                               "HGB072824
*              dann passt da nichts mehr hin; Zus.stellung abbr.
               CLEAR: chk_nxt.                                "HGB072824
*           nein, in der Hilfslinie ist noch Platz
            ELSE.                                             "HGB072824
*              bestimme Offset (= Endpos. + 1)
               txt_offs = sy-fdpos + 1 .
*              fuege Fortsetzungszeile ab Offset an
               hlp_flin+txt_offs = txt-tdline.
*              alles zusammenschieben; Luecken lassen
               CONDENSE hlp_flin.
*              LeseIndex des INCLUDE-Satzes ermitteln
               txt_ridx = sy-tabix - 1 .
*              INCLUDE-Satz wieder einlesen
               READ TABLE txt INDEX txt_ridx.
*              Satzverlaengerung uebernehmen ...
               txt-tdline = hlp_flin.
*              ... und sichern
               MODIFY txt INDEX txt_ridx.
*              Index des Fortsetzungssatzes ermitteln
               txt_ridx = txt_ridx + 1 .
*              Fortsetzungssatz löschen
               DELETE txt INDEX txt_ridx.
*              Format des INCLUDE-Satzes init.
               CLEAR: txt-tdformat.
            ENDIF.                                            "HGB072824
         ELSE.
*           NachfolgerCheck init.
            CLEAR: chk_nxt.
         ENDIF.
      ENDIF.

*     ?Ist akt. Zeile eine Steueranweisung
*     ja
      IF txt-tdformat EQ '/:'.
*        ?Ist akt. Zeile (sehr wahrscheinlich) eine INCL-Anweisung
         SEARCH txt-tdline FOR 'INCLUDE'.
*        ja
         IF sy-subrc = 0.
*           dann sollte die Nachfolgezeile geprüft werden ...
            chk_nxt = 'x'.
*           und der erste Teil der INCL-Anweisung gesichert werden
            hlp_flin = txt-tdline.
         ENDIF.
      ENDIF.
   ENDLOOP.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        END_OF_LIST                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM end_of_list.
* ---------------------------------
DATA: lindiff LIKE sy-linno.

* ---------------------------------
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

*  Kennz. 'letzter LTB- und Listenende-Satz ausgegeben'
   eolst_flg = 'x'.
*  Anzahl Ausnahmehinweise bestimmen
   DESCRIBE TABLE excpt LINES excpt_tabix.

*  falls Ausnahmehinweise ausgegeben werden muessen, ...
   IF excpt_tabix > 0.
*d    IF sy-ucomm NE 'CSPR'.                                "note 351902
      IF SV_UCOMM NE 'CSPR'.                                "note 351902
*del     ULINE (81).                                          "YHG109837
         ULINE AT /1(siz_linpf).                              "YHG109837
         PERFORM end_page.
      ENDIF.

*     dann neue Seite
      NEW-PAGE.

*     ggf. eine Leerzeile einschieben
*d    IF sy-ucomm NE 'CSPR'.                                "note 351902
      IF SV_UCOMM NE 'CSPR'.                                "note 351902
         PERFORM set_margin.
      ELSE.
         SKIP 1.
      ENDIF.

*     ?Seite, auf der die Ausnahmehinweise stehen, bereits bekannt
*     nein
      IF page_exc < 1.
*        ... dann ist sie das; also merken
         page_exc = sy-pagno.
      ENDIF.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        END_PAGE                                                     *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM end_page.
*  Anzahl Listenzeilen pro Bildschirmseite (7 = Kopfzeilen)
*del LINS_PER_PAG = SY-SROWS - 7 .                            "YHG139715
   lins_per_pag = sy-srows - nbr_hdrlns.                      "YHG139715
*  Anzahl bisher ausgegebener Zeilen (eine mehr; daher 6 nicht 7)
*del LINS_OUT     = LINS_OUT_SAV + SY-LINNO - 6 .             "YHG139715
   lins_out     = lins_out_sav + sy-linno - nbr_hdrlns + 1 .  "YHG139715
*  LINS_OUT sichern
   lins_out_sav = lins_out.

*  ermitteln, wieviele Zeilen auf der aktuellen Seite ausgegeben sind
   DO.
      IF lins_out <= lins_per_pag.
         EXIT.
      ENDIF.
      lins_out = lins_out - lins_per_pag.
   ENDDO.

*  Zeilenvorschub bis Bildschirmseitenende ermitteln
   lins_to_skip = lins_per_pag - lins_out.
*  vorzuschiebende Zeile auf ausgegebene Zeilen addieren
   lins_out_sav = lins_out_sav + lins_to_skip.

*  nur weiter, wenn auf dieser Seite ueberhaupt Zeilen ausgegeben sind
   CHECK lins_out > 1 .                                       "YHG077295
*  ?Druckmodus
*  nein
*d IF sy-ucomm NE 'CSPR'.                                   "note 351902
   IF SV_UCOMM NE 'CSPR'.                                   "note 351902
*     Vorschub bis Bildschirmseitenende
      DO.
         IF lins_to_skip < 1.
            EXIT.
         ENDIF.
         SKIP.
         lins_to_skip = lins_to_skip - 1 .
      ENDDO.

*     letzte Zeile (leer) schreiben
      WRITE: / ' '.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        EQUI_ANZEIGEN                             USER_COMMAND: CSAO *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM equi_anzeigen.
* ---------------------------------
DATA: sav_equnr LIKE equi-equnr.

* ---------------------------------
*  nur, wenn ueberhaupt eine EquiNr sitzt
*del IF LTB-EQUNR IS INITIAL.                                 "YHG135858
   IF equicat-equnr IS INITIAL.                               "YHG135858
      MESSAGE s150.
*del  CHECK LTB-EQUNR NE SPACE.                               "YHG135858
      CHECK equicat-equnr NE space.                           "YHG135858
   ENDIF.

*  "Schnittstelle" sichern
   GET PARAMETER ID 'EQN' FIELD sav_equnr.

*  Parameterarea mit EquiNr versorgen
*del SET PARAMETER ID 'EQN' FIELD LTB-EQUNR.                  "YHG135858
   SET PARAMETER ID 'EQN' FIELD equicat-equnr.                "YHG135858
*  Transaktion 'Anzeigen Equipment' aufrufen
   CALL TRANSACTION 'IE03'
        AND SKIP FIRST SCREEN.
*  Reset Parameterarea bzgl. Equipment
   SET PARAMETER ID 'EQN' FIELD ' '.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.

*  Restore "Schnittstelle"
   GET PARAMETER ID 'EQN' FIELD sav_equnr.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        FIT_INPUT_01                                                 *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM fit_input_01.
*  ?Report per SUBMIT gestartet
*  ja
*d IF CSBOMEX-SUBMF = 'x'.                                    "YHG046403
*     PM_EMENG ermitteln
*d    TMP_HRECHF = PM_HEMNG.                                  "YHG046403
*d    TMP_HRECHF = TMP_HRECHF / 1000.                         "YHG046403
*d    PM_EMENG   = TMP_HRECHF.                                "YHG046403

*     PM_RMENG ermitteln
*d    TMP_HRECHF = PM_HRMNG.                                  "YHG046403
*d    TMP_HRECHF = TMP_HRECHF / 1000.                         "YHG046403
*d    PM_RMENG   = TMP_HRECHF.                                "YHG046403
*d ENDIF.                                                     "YHG046403
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        HELP_ME                                                      *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM help_me.                                                 "YHG109837
  CALL FUNCTION 'CS_VLIST_BLOCK_FIELD_DOCU'
                     EXPORTING lstid = list_id
                               profl = act_profil.

ENDFORM.                                                      "YHG109837


*eject
*---------------------------------------------------------------------*
*        HIDE_ROUTINE                                                 *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM hide_routine.
   HIDE: ltb-werks,
*del     LTB-IWERK,                                           "YHG135858
         ltb-stlan,
         ltb-stlty,
         ltb-idnrk,
         ltb-vwalt,
         ltb-stlkn,
         ltb-stpoz,                                           "YHG115627
*del     LTB-EQUNR,                                           "YHG135858
*del     LTB-MATNR.                                           "YHG135858
         doccat-doknr,                                        "YHG135858
         doccat-dokar,                                        "YHG135858
         doccat-doktl,                                        "YHG135858
         doccat-dokvr,                                        "YHG135858
         equicat-equnr,                                       "YHG135858
         equicat-iwerk,                                       "YHG135858
         kndcat-vbeln,                                        "YHG135858
         kndcat-vbpos,                                        "YHG135858
         kndcat-vbeln,                                        "YHG135858
         kndcat-matnr,                                        "YHG135858
         prjcat-pspnr,                                        "MBA089075
         prjcat-matnr,                                        "MBA089075
         matcat-matnr,                                        "YHG135858
         stdcat-stobj,                                        "YHG135858
         tplcat-tplnr,                                        "YHG135858
         tplcat-iwerk.                                        "YHG135858
ENDFORM.


FORM import_catt_flag.                                        "HGC072824
   IMPORT cattaktiv FROM MEMORY ID 'CATT'.                    "HGC072824
ENDFORM.                                                      "HGC072824


*eject
*---------------------------------------------------------------------*
*        KEEP_EXCPT                                                   *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM keep_excpt.
*  nur, wenn ueberhaupt ein Ausnahmehinweis sitzt
   CHECK NOT ltb-excpt IS INITIAL.
   CLEAR: excpt.                                              "YHG110068

*  allgemeine Ausnahmedaten in Tab.-WA EXCPT uebernehmen
   excpt-stlan = ltb-stlan.
   excpt-stlty = ltb-stlty.
   excpt-xtlal = ltb-vwalt.
   excpt-ausnm = ltb-excpt.

*  spezielle Ausnahmedaten in Tab.-WA EXCPT uebernehmen
   CASE ltb-stlty.
      WHEN typ_doc.                                           "YHG110068
         excpt-bgdoc = doccat-doknr.                          "YHG110068
         excpt-ktext = ltb-ojtxb.                             "YHG110068
         CLEAR: excpt-zwerk.                                  "YHG110068
      WHEN typ_equi.
         excpt-bgequ = equicat-equnr.
         excpt-ktext = ltb-ojtxb.
         excpt-zwerk = equicat-iwerk.
      WHEN typ_knd.                                           "YHG110068
         excpt-bgknd = kndcat-matnr.                          "YHG110068
         excpt-ktext = ltb-ojtxb.                             "YHG110068
         excpt-zwerk = ltb-werks.                             "YHG110068
      WHEN typ_mat.
         excpt-bgmat = matcat-matnr.
         excpt-ktext = ltb-ojtxb.
         excpt-zwerk = ltb-werks.
      WHEN typ_prj.                                           "MBA089075
         excpt-bgprj = prjcat-pspnr.                          "MBA089075

      WHEN typ_std.                                           "YHG110068
         excpt-bgstd = stdcat-stobj.                          "YHG110068
         excpt-ktext = ltb-ojtxb.                             "YHG110068
         excpt-zwerk = ltb-werks.                             "YHG110068
      WHEN typ_tpl.                                           "YHG110068
         excpt-bgtpl = tplcat-tplnr.                          "YHG110068
         excpt-ktext = ltb-ojtxb.                             "YHG110068
         excpt-zwerk = tplcat-iwerk.                          "YHG110068
   ENDCASE.

*  EXCPT-Satz in Ausnahmen-Tab. uebernehmen.
   COLLECT excpt.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_01_79                                                   *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM list_01_79.
*  nur weiter, wenn eine StuecklistenNr sitzt
   CHECK NOT ltb-stlnr IS INITIAL.

*  ?ist LTB-Satz Teil eines Summensatzes
*  nein
   IF NOT ltb-sumfg = 'x'.
*     Verwendungsalternative in Anzeigefeld uebernehmen
      dsp_stlal = ltb-vwalt.
*     fuehrende Null ...
      IF dsp_stlal(1) = '0'.
*        ... ggf. entfernen
         dsp_stlal(1) = ' '.
      ENDIF.

*     Positionsnummer in Anzeigefeld uebernehmen
      dsp_posnr = ltb-posnr.

*     falls aktueller LTB-Satz ein Summensatz ist, ...
      IF ltb-sumfg = '*'.
         CLEAR: dsp_posnr.
*        ... wird statt einer PosNr ein Stern angezeigt
         dsp_posnr(1) = '*'.
      ENDIF.

*del  IF SY-UCOMM NE 'CSPR'.                                  "YHG109837
      PERFORM list_01_79_liste1.
*del  ELSE.                                                   "YHG109837
*del     PERFORM LIST_01_79_DRUCK1.                           "YHG109837
*del  ENDIF.                                                  "YHG109837
*  ja, aktueller LTB-Satz ist Teil eines Summensatzes
   ELSE.
*     Kennzeichen fuer - Leezeile nach Aufzaehlungsende - setzen
      skipl_flg = 'x'.

*del  IF SY-UCOMM NE 'CSPR'.                                  "YHG109837
      PERFORM list_01_79_liste2.
*del  ELSE.                                                   "YHG109837
*del     PERFORM LIST_01_79_DRUCK2.                           "YHG109837
*del  ENDIF.                                                  "YHG109837
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_01_79_DRUCK1                                            *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
*  Feldpositionen in Ausgabezeilen geaendert                  "YHG083168
FORM list_01_79_druck1.
   FORMAT INTENSIFIED.
   RESERVE 2 LINES.

*  am Ende einer Einzelsatzaufzaehlung zu einem Summensatz ...
   IF NOT skipl_flg IS INITIAL.
      IF sy-linno > 1.
*        ... eine Leerzeile einfuegen
         SKIP.

         IF NOT eopth_flg IS INITIAL.                         "HGA032266
            ULINE AT /1(siz_linpf).                           "HGA032266
            CLEAR: eopth_flg.                                 "HGA032266
         ENDIF.                                               "HGA032266
      ENDIF.
*     Kennz. Ende Einzelsatzaufzaehlung zuruecknehmen
      CLEAR: skipl_flg.
   ENDIF.
*  1. PosZeile
   WRITE: /1(2)   ltb-level NO-SIGN,  "Stufe
           4      ltb-stlan.          "StlVerwendung

   CASE ltb-stlty.
      WHEN typ_mat.
         WRITE:  6      ltb-werks, "Werk
*del             11     LTB-MATNR. "MaterialNr                "YHG083168
                 11     ltb-matnr, "MaterialNr                "YHG083168
                        ltb-revlv. "Revisionsstand            "YHG083168

      WHEN typ_equi.
*del     WRITE:  6      LTB-IWERK, "Standortwerk              "YHG096568
         WRITE:  6      ltb-werks,                            "YHG096568
                 11     ltb-equnr. "Equipment
   ENDCASE.

   WRITE:  33     dsp_stlal,       "StlAlternative
*del       33     LTB-LOEKZ,       "LoeschKz                  "YHG083168
           36     dsp_posnr,       "PositionsNr
           41     dsp_imeng,       "Einsatzmenge
           56     opo_meinh.       "MngEh d. orig. Pos.

   IF     ltb-fmeng IS INITIAL
*del  OR  ( LTB-SUMFG EQ '*' ) .                              "YHG079102
      OR  (     NOT ltb-emeng IS INITIAL                      "YHG079102
            AND NOT ltb-fxmng IS INITIAL ) .                  "YHG079102

      WRITE:  60     dsp_rmeng,    "Ergebnismenge
              75     ltb-bmein.    "BasismengenEh
   ELSE.
      WRITE:  60(4)  text-040.     "Menge fix
   ENDIF.

*del IF     NOT CLASP_FLG IS INITIAL               "YHG000381 "YHG077203
*del    AND LTB-IDNRK NE PM_IDNRK.                 "YHG000381 "YHG077203

*del    WRITE:  78     'X'.          "Klassenpos.  "YHG000381 "YHG077203
*del ENDIF.                                        "YHG000381 "YHG077203
   PERFORM set_cla_sign_druck.                                "YHG077203

   IF     pm_emeng =  0.
      IF dsp_imeng >= max_num.
         WRITE:  41     '*'.
      ENDIF.
   ELSE.
      IF     pm_rmeng =  0.
         IF dsp_rmeng >= max_num.
            WRITE:  60     '*'.
         ENDIF.
      ENDIF.
   ENDIF.

   FORMAT INTENSIFIED OFF.
*  2. PosZeile
   CASE ltb-stlty.
      WHEN typ_mat.
         WRITE: /11     ltb-ojtxb."MatKurztext
         WRITE:  53     ltb-loekz."LoeschKz                   "YHG083168

      WHEN typ_equi.
         WRITE: /11     ltb-ojtxb."EquiKurztext
         WRITE:  53     ltb-loekz."LoeschKz                   "YHG083168
   ENDCASE.

*  ?ist aktueller LTB-Satz ein Summensatz
*  nein
   IF NOT ltb-sumfg = '*'.
*     falls der Gueltigkeitsbereich angezeigt werden soll und ...
      IF NOT pm_gbraz IS INITIAL
*        dieser nicht vollstaendig im eingegebenen Zeitraum liegt,
         OR ltb-datuv > pm_datuv
         OR ltb-datub < pm_datub.
*        Gueltigkeitsbereich anzeigen
         WRITE:  59     ltb-datuv,          "von-Datum
                 69     '-',
                 70     ltb-datub.          "bis-Datum
      ENDIF.

*     falls Langtext angezeigt werden soll
      IF NOT pm_ltext IS INITIAL.
*        falls ein Langtext existiert (ein SrachKz dazu)
         IF NOT ltb-ltxsp IS INITIAL.
*           Langtext in Tab. TXT einlesen
            PERFORM ltext_holen.
*           falls das Einlesen von Langtext erfolgreich
            IF sy-subrc = 0.
*              Text ausgeben
               LOOP AT txt.
                  WRITE: /36     txt-tdline.
               ENDLOOP.

               IF sy-linno > 1.
*                 Leerzeile
                  SKIP. "weil's besser aussieht
               ENDIF.
            ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_01_79_DRUCK2                                            *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
*  Feldpositionen in Ausgabezeilen geaendert                  "YHG083168
FORM list_01_79_druck2.
*  Einzelsatz zum Summensatz anzeigen
*del WRITE: /35     LTB-POSNR,       "PosNr                   "YHG079102
*del         40     DSP_IMENG,       "Einsatzmenge            "YHG079102
*del         55     OPO_MEINH.       "MngEh EinstiegsPos      "YHG079102
*del WRITE: /35     LTB-POSNR.       "PosNr         "YHG079102
   WRITE: /36     ltb-posnr.       "PosNr                     "YHG079102
   IF     dsp_imeng = 0                                       "YHG079102
      AND ltb-emeng <> 0.                                     "YHG079102
      WRITE:  52     qnt_none.                                "YHG079102
   ELSE.                                                      "YHG079102
      WRITE:  41     dsp_imeng.                               "YHG079102
   ENDIF.                                                     "YHG079102

   WRITE:  56     opo_meinh.                                  "YHG079102

   IF dsp_imeng >= max_num.
      WRITE:  41     '*'.
   ENDIF.

   IF NOT ltb-fmeng IS INITIAL.
      WRITE:  60(4)  text-040.     "Menge fix
   ENDIF.

*  falls der Gueltigkeitsbereich angezeigt werden soll und
   IF NOT pm_gbraz IS INITIAL
*     dieser nicht vollst. im eingegebenen Bereich liegt,
      OR ltb-datuv > pm_datuv
      OR ltb-datub < pm_datub.

*     Gueltigkeitsbereich ausgeben
      WRITE:  59     ltb-datuv,
              69     '-',
              70     ltb-datub.
   ENDIF.

*  falls Langtext angezeigt werden soll
   IF NOT pm_ltext IS INITIAL.
*     falls ein Langtext existiert (ein SrachKz dazu)
      IF NOT ltb-ltxsp IS INITIAL.
*        Langtext in Tab. TXT einlesen
         PERFORM ltext_holen.
*        falls das Einlesen von Langtext erfolgreich
         IF sy-subrc = 0.
*           Text ausgeben
            LOOP AT txt.
               WRITE: /36     txt-tdline.
            ENDLOOP.
*           Leerzeile
            SKIP. "weil's besser aussieht
         ENDIF.
      ENDIF.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_01_79_LISTE1                                            *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
*  Feldpositionen in Ausgabezeilen geaendert                  "YHG083168
*  komplett ueberarbeitet (var. Liste)                        "YHG109837
FORM list_01_79_liste1.
*  am Ende einer Einzelsatzaufzaehlung zu einem Summensatz ...
   IF NOT skipl_flg IS INITIAL.
      IF sy-linno > 1.
*        ... eine Leerzeile einfuegen
         PERFORM set_margin.

         IF NOT eopth_flg IS INITIAL.                         "HGA032266
            ULINE AT /1(siz_linpf).                           "HGA032266
            CLEAR: eopth_flg.                                 "HGA032266
         ENDIF.                                               "HGA032266
      ENDIF.
*     Kennz. Ende Einzelsatzaufzaehlung zuruecknehmen
      CLEAR: skipl_flg.
   ENDIF.

*  ab hier neu                                                "YHG109837
   ltb_orig = ltb.
   CLEAR: ltb_add.

   WRITE ltb-level TO ltb_add-dstuf(2) NO-SIGN.

   CASE ltb-bmtyp.
      WHEN typ_doc.
         ltb_add-doknr = doccat-doknr.
         ltb_add-dokar = doccat-dokar.
         ltb_add-doktl = doccat-doktl.
         ltb_add-dokvr = doccat-dokvr.

      WHEN typ_equi.
         ltb_add-equnr = equicat-equnr.
         ltb_add-iwerk = equicat-iwerk.

      WHEN typ_knd.
         ltb_add-matnr = kndcat-matnr.
         ltb_add-vbeln = kndcat-vbeln.                        "MBA089075
         ltb_add-vbpos = kndcat-vbpos.                        "MBA089075
         ltb_add-revlv = kndcat-revlv.                        "MBA089075

      WHEN typ_mat.
         ltb_add-matnr = matcat-matnr.
         ltb_add-revlv = matcat-revlv.

      WHEN typ_prj.                                           "MBA089075
         ltb_add-matnr = prjcat-matnr.                        "MBA089075
         ltb_add-pspnr = prjcat-pspnr.                        "MBA089075
*        ltb_add-iwerk = prjcat-werks.                        "MBA089075

      WHEN typ_std.
         ltb_add-stobj = stdcat-stobj.

      WHEN typ_tpl.
         ltb_add-tplnr = tplcat-tplnr.
         ltb_add-iwerk = tplcat-iwerk.
   ENDCASE.

   ltb_add-dstal = dsp_stlal.
   ltb_add-dposn = dsp_posnr.


   IF prv_extrm IS INITIAL.                                   "HGA118294
      WRITE dsp_imeng TO ltb_add-dimng(15).
      ltb_add-meopo = opo_meinh.

      IF     ltb-fmeng IS INITIAL
         OR  (     NOT ltb-emeng IS INITIAL
               AND NOT ltb-fxmng IS INITIAL ) .

         WRITE dsp_rmeng TO ltb_add-drmng(15).
      ELSE.
         WRITE text-040 TO ltb_add-comfx(4).
         CLEAR: ltb_add-drmng,
                ltb_orig-bmein.
      ENDIF.

      IF     pm_emeng =  0.
         IF dsp_imeng >= max_num.
            WRITE '*' TO ltb_add-imovf.
         ENDIF.
      ELSE.
         IF     pm_rmeng =  0.
            IF dsp_rmeng >= max_num.
               WRITE '*' TO ltb_add-rmovf.
            ENDIF.
         ENDIF.
      ENDIF.
   ELSE.                                                      "HGA118294
      IF prv_extrm EQ '+'.                                    "HGA118294
         WRITE qnt_any TO ltb_add-dimng+11(3).                "HGA118294
         CLEAR: ltb_add-drmng(15),                            "HGA118294
                ltb_add-meopo,                                "HGA118294
                ltb_orig-bmein.                               "HGA118294
      ELSE.                                                   "HGA118294
         WRITE dsp_rmeng TO ltb_add-drmng(15).                "HGA118294
         WRITE qnt_none TO ltb_add-dimng+11(3).               "HGA118294
         CLEAR: ltb_add-meopo.                                "HGA118294
      ENDIF.                                                  "HGA118294
   ENDIF.                                                     "HGA118294

   IF NOT ltb-loekz IS INITIAL.
      ltb_add-loefg = 'X'.
   ENDIF.

   IF col_switch IS INITIAL.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      col_switch = 'x'.
   ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      CLEAR: col_switch.
   ENDIF.

   PERFORM set_cla_sign_liste.

*  WATAB initialisieren und komplett leeren
   CLEAR: watab. REFRESH: watab.
*  Uebergabestruktur (Typ STPOX) ...
*d watab-tname = 'STPOV'. watab-table = ltb_orig .                   "uc
   WATAB-TNAME = 'STPOV'.                                            "uc
   assign watab-table to <x_watab-table>  casting.                   "uc
   assign ltb_orig    to <x_stpov_wa>     casting.                   "uc
   <x_watab-table> = <x_stpov_wa> .                                  "uc
*  ... sichern
   APPEND watab.

*  WATAB initialisieren
   CLEAR watab.
*  Uebergabestruktur (Typ STPOL_ADD) ...
*d watab-tname = 'STPOL_ADD'. watab-table = ltb_add .                "uc
   WATAB-TNAME = 'STPOL_ADD'.                                        "uc
   assign watab-table to <x_watab-table>  casting.                   "uc
   assign ltb_add     to <x_STPOL_ADD_wa> casting.                   "uc
   <x_watab-table> = <x_STPOL_ADD_wa> .                              "uc
*  ... sichern
   APPEND watab.

*  WATAB initialisieren
   CLEAR watab.

*  Daten zur Position ausgeben
   IF    ltb-sumfg NE '*'.                                    "HGA088281
      CASE ltb-bmtyp.
         WHEN typ_doc.
            PERFORM write_block
               USING 'I_IN_D_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656

         WHEN typ_equi.
            PERFORM write_block
               USING 'I_IN_E_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656

         WHEN typ_knd.
            PERFORM write_block
               USING 'I_IN_K_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656

         WHEN typ_mat.
            PERFORM write_block
               USING 'I_IN_M_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656

         WHEN typ_prj.                                        "MBA089075
            PERFORM write_block                               "MBA089075
               USING 'I_IN_P_BOM        '                     "MBA089075
*                    ausgegebene Zeilen nicht zaehlen         "MBA089075
                     ' '                                      "MBA089075
*                    Hide ausführen                           "MBA089075
                     'x'.                                     "MBA089075

         WHEN typ_std.
            PERFORM write_block
               USING 'I_IN_S_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656

         WHEN typ_tpl.
            PERFORM write_block
               USING 'I_IN_T_BOM        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide ausfuehren
                     'x'.                                     "YHG123656
      ENDCASE.
   ELSE.                                                      "HGA088281
      CASE ltb-bmtyp.                                         "HGA088281
         WHEN typ_doc.                                        "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_D_BOM_SM     ' ' ' 'x'.            "HGA088281

         WHEN typ_equi.                                       "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_E_BOM_SM     ' ' ' 'x'.            "HGA088281

         WHEN typ_knd.                                        "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_K_BOM_SM     ' ' ' 'x'.            "HGA088281

         WHEN typ_mat.                                        "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_M_BOM_SM     ' ' ' 'x'.            "HGA088281

         WHEN typ_prj.                                        "MBA089075
            PERFORM write_block                               "MBA089075
               USING 'I_IN_P_BOM_SM     ' ' ' 'x'.            "MBA089075

         WHEN typ_std.                                        "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_S_BOM_SM     ' ' ' 'x'.            "HGA088281

         WHEN typ_tpl.                                        "HGA088281
            PERFORM write_block                               "HGA088281
               USING 'I_IN_T_BOM_SM     ' ' ' 'x'.            "HGA088281
      ENDCASE.                                                "HGA088281
   ENDIF.                                                     "HGA088281

*  ?soll Langtext ausgegeben werdem
*  ja
   IF NOT pm_ltext IS INITIAL.
*     ?gibt es Langtext
*     ja
*d    IF NOT ltb-ltxsp IS INITIAL.                            "MBA145524
      IF NOT ltb-ltxsp IS INITIAL AND ltb-sumfg NE '*'.       "MBA145524
*        Langtext einlesen
         PERFORM ltext_holen.

*        ?Langtext konnte eingelesen werden
*        ja
         IF sy-subrc = 0.
*           pro Textzeile ...
            LOOP AT txt.
*              Uebergabestruktur initialisieren
               CLEAR: ltb_add.
*              Textzeile in Uebergabestruktur uebernehmen
               ltb_add-tline = txt-tdline.

*              WATAB initialisieren und komplett leeren
               CLEAR watab. REFRESH watab.
*              Uebergabestruktur (Typ STPOL_ADD) ...
*d             watab-tname = 'STPOL_ADD'. watab-table = ltb_add .    "uc
               WATAB-TNAME = 'STPOL_ADD'.                            "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
*              ... sichern
               APPEND watab.

*              WATAB initialisieren und komplett leeren
               CLEAR watab.
*              Langtextzeile ausgeben
               PERFORM write_block
                  USING 'LTEXT_LIN         '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide ausfuehren
                        'x'.                                  "YHG123656
            ENDLOOP.
         ENDIF.
      ENDIF.
   ENDIF.
*  bis hier neu                                               "YHG109837
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_01_79_LISTE2                                            *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
*  Feldpositionen in Ausgabezeilen geaendert                  "YHG083168
*  komplett ueberarbeitet (var. Liste)                        "YHG109837
FORM list_01_79_liste2.
*  ab hier neu                                                "YHG109837
   ltb_orig = ltb.
   CLEAR: ltb_add.

   IF     dsp_imeng = 0
      AND ltb-emeng <> 0.
      WRITE qnt_none TO ltb_add-dimng+11(3).
      CLEAR: ltb_add-meopo.                                   "HGA118294
   ELSE.
      WRITE dsp_imeng TO ltb_add-dimng(15).
      ltb_add-meopo = opo_meinh.                              "HGA118294
   ENDIF.

   ltb_add-dposn = ltb-posnr.
*d LTB_ADD-MEOPO = OPO_MEINH.                                 "HGA118294

   IF NOT ltb-fmeng IS INITIAL.
      WRITE text-040 TO ltb_add-comfx(4).
   ENDIF.

   IF     pm_emeng =  0.
      IF dsp_imeng >= max_num.
         WRITE '*' TO ltb_add-imovf.
      ENDIF.
   ENDIF.

*  WATAB initialisieren und komplett leeren
   CLEAR: watab. REFRESH: watab.
*  Uebergabestruktur (Typ STPOX) ...
*d watab-tname = 'STPOV'. watab-table = ltb_orig .                   "uc
   WATAB-TNAME = 'STPOV'.                                            "uc
   assign watab-table to <x_watab-table>  casting.                   "uc
   assign ltb_orig    to <x_stpov_wa>     casting.                   "uc
   <x_watab-table> = <x_stpov_wa> .                                  "uc
*  ... sichern
   APPEND watab.

*  WATAB initialisieren
   CLEAR watab.
*  Uebergabestruktur (Typ STPOL_ADD) ...
*d watab-tname = 'STPOL_ADD'. watab-table = ltb_add .                "uc
   WATAB-TNAME = 'STPOL_ADD'.                                        "uc
   assign watab-table to <x_watab-table>  casting.                   "uc
   assign ltb_add     to <x_STPOL_ADD_wa> casting.                   "uc
   <x_watab-table> = <x_STPOL_ADD_wa> .                              "uc
*  ... sichern
   APPEND watab.

*  WATAB initialisieren
   CLEAR watab.

   PERFORM write_block
      USING 'SNGL_VAL_LIN      '
*           ausgegebene Zeilen nicht zaehlen
            ' '
*           Hide ausfuehren
            'x'.                                              "YHG123656

*  ?soll Langtext ausgegeben werdem
*  ja
   IF NOT pm_ltext IS INITIAL.
*     ?gibt es Langtext
*     ja
      IF NOT ltb-ltxsp IS INITIAL.
*        Langtext einlesen
         PERFORM ltext_holen.

*        ?Langtext konnte eingelesen werden
*        ja
         IF sy-subrc = 0.
*           pro Textzeile ...
            LOOP AT txt.
*              ?wenn Zeile 1 ...
*d             IF sy-tabix = 1.                               "MBA145524
*                 gleich der ersten PosTextzeile ist, - nicht ausgeben
*d                CHECK ltb-potx1 <> txt-tdline.              "MBA145524
*d             ENDIF.                                         "MBA145524

*              ?wenn Zeile 1 ...
*d             IF sy-tabix = 2.                               "MBA145524
*                 gleich der zweiten PosTextzeile ist, - nicht ausgeben
*d                CHECK ltb-potx2 <> txt-tdline.              "MBA145524
*d             ENDIF.                                         "MBA145524

*              Uebergabestruktur initialisieren
               CLEAR: ltb_add.
*              Textzeile in Uebergabestruktur uebernehmen
               ltb_add-tline = txt-tdline.

*              WATAB initialisieren und komplett leeren
               CLEAR watab. REFRESH watab.
*              Uebergabestruktur (Typ STPOL_ADD) ...
*d             watab-tname = 'STPOL_ADD'. watab-table = ltb_add .    "uc
               WATAB-TNAME = 'STPOL_ADD'.                            "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
*              ... sichern
               APPEND watab.

*              WATAB initialisieren und komplett leeren
               CLEAR watab.
*              Langtextzeile ausgeben
               PERFORM write_block
                  USING 'LTEXT_LIN         '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide ausfuehren
                        'x'.                                  "YHG123656
            ENDLOOP.
         ENDIF.
      ENDIF.
   ENDIF.
*  bis hier neu                                               "YHG109837
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_EXCEPTIONS                                              *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM list_exceptions.
*del IF SY-UCOMM NE 'CSPR'.                                   "YHG109837
   PERFORM list_exceptions_liste.
*del ELSE.                                                    "YHG109837
*del    PERFORM LIST_EXCEPTIONS_DRUCK.                        "YHG109837
*del ENDIF.                                                   "YHG109837
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_EXCEPTIONS_DRUCK                                        *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM list_exceptions_druck.
* ----------------------------------
DATA: lcl_lst_ausnm LIKE excpt-ausnm.

* ----------------------------------
CLEAR: lcl_lst_ausnm.

   READ TABLE excpt INDEX 1.
   IF sy-subrc = 0 .
      SORT excpt BY stlty DESCENDING
                    ausnm DESCENDING.
      RESERVE 4 LINES.

      FORMAT INTENSIFIED.
      WRITE: /5      text-100.

*del  CLEAR: AUTH_CNT.
      CLEAR: auth_cnt,
             lcl_lst_ausnm.

      LOOP AT excpt.
         IF lcl_lst_ausnm NE excpt-ausnm.
            lcl_lst_ausnm = excpt-ausnm.
            SKIP.
            CASE excpt-ausnm.
               WHEN 'NBER'.
                  FORMAT INTENSIFIED.
                  WRITE: /5      text-113.
               WHEN 'DELE'.
                  PERFORM auth_cnt_chk USING ' '.
                  FORMAT INTENSIFIED.
                  WRITE: /5      text-110.
               WHEN 'NREK'.
                  PERFORM auth_cnt_chk USING ' '.
                  FORMAT INTENSIFIED.
                  WRITE: /5      text-111.
               WHEN 'REKU'.
                  PERFORM auth_cnt_chk USING ' '.
                  FORMAT INTENSIFIED.
                  WRITE: /5      text-112.
            ENDCASE.
         ENDIF.

         IF excpt-ausnm = 'NBER'.
            auth_cnt = auth_cnt + 1 .
            CHECK auth_cnt IS INITIAL.
         ENDIF.

         FORMAT INTENSIFIED OFF.

         WRITE: /7      excpt-stlty,
                        excpt-stlan,
                        excpt-zwerk.

         CASE excpt-stlty.
            WHEN typ_mat.
               WRITE:         excpt-bgmat.
            WHEN typ_equi.
               WRITE:         excpt-bgequ.
         ENDCASE.

         WRITE:         excpt-xtlal,
                        excpt-ktext.
      ENDLOOP.

      IF NOT auth_cnt IS INITIAL.
         PERFORM auth_cnt_chk USING 'x'.
      ENDIF.

      CLEAR: lcl_lst_ausnm.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LIST_EXCEPTIONS_LISTE                                        *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
* komplett überarbeitet (var. Liste)                          "YHG109837
FORM list_exceptions_liste.
* ----------------------------------
DATA: lcl_lst_ausnm LIKE excpt-ausnm.
DATA: lcl_sav_tabix LIKE sy-tabix.

* ----------------------------------
   CLEAR: lcl_lst_ausnm.

   READ TABLE excpt INDEX 1.
   IF sy-subrc = 0 .
      SORT excpt BY stlty DESCENDING
                    ausnm DESCENDING.
      RESERVE 4 LINES.

      CLEAR: ltb_add.
      WRITE text-100 TO ltb_add-tline(76).

      FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.

      CLEAR: watab. REFRESH watab.

*d    watab-tname = 'STPOL_ADD'. watab-table = ltb_add .             "uc
      WATAB-TNAME = 'STPOL_ADD'.                                     "uc
      assign watab-table to <x_watab-table>  casting.                "uc
      assign ltb_add     to <x_STPOL_ADD_wa> casting.                "uc
      <x_watab-table> = <x_STPOL_ADD_wa> .                           "uc
      APPEND watab.
      CLEAR: watab.

      PERFORM write_block
         USING 'X_GNRL_HDNG       '
*              ausgegebene Zeilen nicht zaehlen
               ' '
*              Hide nicht ausfuehren
               ' '.                                           "YHG123656

      CLEAR: auth_cnt.

      LOOP AT excpt.
         IF lcl_lst_ausnm NE excpt-ausnm.
            lcl_lst_ausnm = excpt-ausnm.
            lcl_sav_tabix = sy-tabix.

            CLEAR: ltb_add.

            CASE excpt-ausnm.
               WHEN 'NBER'.
                  CLEAR: auth_cnt.
                  LOOP AT excpt FROM lcl_sav_tabix.
                     IF excpt-ausnm NE lcl_lst_ausnm.
                        READ TABLE excpt INDEX lcl_sav_tabix.
                        EXIT.
                     ENDIF.

                     auth_cnt = auth_cnt + 1 .
                  ENDLOOP.

                  WRITE text-113 TO ltb_add-tline(76).
                  WRITE auth_cnt TO ltb_add-tline+54(3).
                  CLEAR: auth_cnt.

               WHEN 'DELE'.
                  WRITE text-110 TO ltb_add-tline(76).

               WHEN 'NREK'.
                  WRITE text-111 TO ltb_add-tline(76).

               WHEN 'REKU'.
                  WRITE text-112 TO ltb_add-tline(76).

               WHEN 'CONV'.                                   "MBA148624
                  WRITE text-115 TO ltb_add-tline(76).        "MBA148624
            ENDCASE.

            CLEAR: watab. REFRESH watab.

*d          watab-tname = 'STPOL_ADD'. watab-table = ltb_add .       "uc
            WATAB-TNAME = 'STPOL_ADD'.                               "uc
            assign watab-table to <x_watab-table>  casting.          "uc
            assign ltb_add     to <x_STPOL_ADD_wa> casting.          "uc
            <x_watab-table> = <x_STPOL_ADD_wa> .                     "uc
            APPEND watab.
            CLEAR: watab.

            PERFORM write_block
               USING 'X_DTL_HDNG        '
*                    ausgegebene Zeilen nicht zaehlen
                     ' '
*                    Hide nicht ausfuehren
                     ' '.                                     "YHG123656
         ENDIF.

         CLEAR: ltb_add,
                ltb_orig.
         CLEAR: watab. REFRESH: watab.

         CASE excpt-stlty.
            WHEN typ_doc.
               ltb_add-doknr = excpt-bgdoc.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_D_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_equi.
               ltb_add-equnr = excpt-bgequ.
               ltb_add-dstal = excpt-xtlal.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_E_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_knd.
               ltb_add-vbeln = excpt-bgknd.
               ltb_add-dstal = excpt-xtlal.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_K_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_mat.
               ltb_add-matnr = excpt-bgmat.
               ltb_add-dstal = excpt-xtlal.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_M_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_prj.                                     "MBA089075
*d             LTB_ADD-STOBJ = EXCPT-BGSTD.                   "HGB246532
               ltb_add-stobj = excpt-bgprj.                   "HGB246532
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_P_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_std.
               ltb_add-stobj = excpt-bgstd.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_S_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656

            WHEN typ_tpl.
               ltb_add-tplnr = excpt-bgtpl.
               ltb_add-dstal = excpt-xtlal.
               ltb_orig-bmtyp = excpt-stlty.
               ltb_orig-stlan = excpt-stlan.
               ltb_orig-werks = excpt-zwerk.
               ltb_orig-ojtxb = excpt-ktext.

               watab-tname = 'STPOL_ADD'.
*d             watab-table = ltb_add .                               "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_add     to <x_STPOL_ADD_wa> casting.       "uc
               <x_watab-table> = <x_STPOL_ADD_wa> .                  "uc
               APPEND watab.
               CLEAR: watab.

               watab-tname = 'STPOV'.
*d             watab-table = ltb_orig .                              "uc
               assign watab-table to <x_watab-table>  casting.       "uc
               assign ltb_orig    to <x_stpov_wa>     casting.       "uc
               <x_watab-table> = <x_stpov_wa> .                      "uc
               APPEND watab.
               CLEAR: watab.

               FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
               PERFORM write_block
                  USING 'X_T_BOM           '
*                       ausgegebene Zeilen nicht zaehlen
                        ' '
*                       Hide nicht ausfuehren
                        ' '.                                  "YHG123656
         ENDCASE.
      ENDLOOP.

      CLEAR: lcl_lst_ausnm.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        LTEXT_HOLEN                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM ltext_holen.
DATA: BEGIN OF txhdr.                                         "HGB057558
         INCLUDE STRUCTURE thead.                             "HGB057558
DATA: END OF txhdr.                                           "HGB057558
*----------------------------------

   CLEAR: txt_key.
   WRITE sy-mandt  TO txt_key USING NO EDIT MASK.
   txt_kyln = strlen( txt_key ).
   WRITE ltb-stlty TO txt_key+txt_kyln.
   txt_kyln = strlen( txt_key ).
   WRITE ltb-stlnr TO txt_key+txt_kyln USING NO EDIT MASK.
   txt_kyln = strlen( txt_key ).
   WRITE ltb-stlkn TO txt_key+txt_kyln.
   txt_kyln = strlen( txt_key ).
   WRITE ltb-stpoz TO txt_key+txt_kyln.

   bom_txid(1) = ltb-stlty.                                   "HGA069822

   CALL FUNCTION 'READ_TEXT'
      EXPORTING
            id = bom_txid
            language = ltb-ltxsp
            name = txt_key
            object = bom_txobj
      IMPORTING                                               "HGB057558
            header = txhdr                                    "HGB057558
      TABLES
            lines =  txt.

   PERFORM create_txincl_cmd.                                 "HGB057558

   CALL FUNCTION 'TEXT_INCLUDE_REPLACE'                       "HGB057558
      EXPORTING                                               "HGB057558
            header = txhdr                                    "HGB057558
      TABLES                                                  "HGB057558
            lines =  txt.                                     "HGB057558

   PERFORM TXT_CONV_CHK                                     "note 310574
     TABLES                                                 "note 310574
       TXT                                                  "note 310574
     USING                                                  "note 310574
       ' '                                                  "note 310574
       ' '.                                                 "note 310574

   CALL FUNCTION 'FORMAT_TEXTLINES'                         "note 310574
      EXPORTING                                             "note 310574
*d          FORMATWIDTH = 41                                "note 310574
            FORMATWIDTH = 40                                "note 667950
            LINEWIDTH   = 40                                "note 875961
      TABLES                                                "note 310574
            LINES       = TXT.                              "note 310574
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        MATERIAL_ANZEIGEN                         USER-COMMAND: CSAO *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM material_anzeigen.
* ---------------------------------
DATA: all_pstat(9) TYPE c VALUE 'KVEDLAPSB',
      sav_matnr LIKE mara-matnr,
      sav_werks LIKE marc-werks.

* ---------------------------------
*  nur, wenn ueberhaupt eine MatNr sitzt
*del IF LTB-MATNR IS INITIAL.                                 "YHG135858
   IF matcat-matnr IS INITIAL.                                "YHG135858
      MESSAGE s150.
*del  CHECK LTB-MATNR NE SPACE.                               "YHG135858
      CHECK matcat-matnr NE space.                            "YHG135858
   ENDIF.

*  "Schnittstelle" sichern
   GET PARAMETER ID: 'MAT' FIELD sav_matnr,
                     'WRK' FIELD sav_werks.

*  "Schnittstelle" versorgen
*del SET PARAMETER ID: 'MAT' FIELD LTB-MATNR,                 "YHG135858
   SET PARAMETER ID: 'MAT' FIELD matcat-matnr,                "YHG135858
                     'WRK' FIELD ltb-werks,
                     'MXX' FIELD all_pstat.

*  Transaktion 'Anzeigen Material' aufrufen
   CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.

*  restore "Schnittstelle"
   SET PARAMETER ID: 'MAT' FIELD sav_matnr,
                     'WRK' FIELD sav_werks.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        POSITION_ANZEIGEN                         USER-COMMAND: CSAP *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM position_anzeigen.
   CASE ltb-stlty.
      WHEN typ_doc.                                           "YHG135858
*        nur, wenn ueberhaupt eine Dokumentnummer sitzt
         IF doccat-doknr IS INITIAL.                          "YHG135858
            MESSAGE s150.                                     "YHG135858
            CHECK NOT doccat-doknr IS INITIAL.                "YHG135858
         ENDIF.                                               "YHG135858

      WHEN typ_equi.
*        nur, wenn ueberhaupt eine Equipmentnummer sitzt
*del     IF LTB-EQUNR IS INITIAL.                             "YHG135858
         IF equicat-equnr IS INITIAL.                         "YHG135858
            MESSAGE s150.
*del        CHECK NOT LTB-EQUNR IS INITIAL.                   "YHG135858
            CHECK NOT equicat-equnr IS INITIAL.               "YHG135858
         ENDIF.

      WHEN typ_knd.                                           "YHG135858
*        nur, wenn ueberhaupt eine KndAuftragnummer sitzt
         IF kndcat-vbeln IS INITIAL.                          "YHG135858
            MESSAGE s150.                                     "YHG135858
            CHECK NOT kndcat-vbeln IS INITIAL.                "YHG135858
         ENDIF.                                               "YHG135858

      WHEN typ_mat.
*        nur, wenn ueberhaupt eine Materialnummer sitzt
*del     IF LTB-MATNR IS INITIAL.                             "YHG135858
         IF matcat-matnr IS INITIAL.                          "YHG135858
            MESSAGE s150.
*del        CHECK NOT LTB-MATNR IS INITIAL.                   "YHG135858
            CHECK NOT matcat-matnr IS INITIAL.                "YHG135858
         ENDIF.

      WHEN typ_prj.                                           "MBA089075
*        nur, wenn überhaupt ein Projektnummer sitzt          "MBA089075
         IF prjcat-pspnr IS INITIAL.                          "MBA089075
            MESSAGE s150.                                     "MBA089075
            CHECK NOT prjcat-pspnr IS INITIAL.                "MBA089075
         ENDIF.

      WHEN typ_std.                                           "YHG135858
*        nur, wenn ueberhaupt eine Standardobjekt sitzt
         IF stdcat-stobj IS INITIAL.                          "YHG135858
            MESSAGE s150.                                     "YHG135858
            CHECK NOT stdcat-stobj IS INITIAL.                "YHG135858
         ENDIF.                                               "YHG135858

      WHEN typ_tpl.                                           "YHG135858
*        nur, wenn ueberhaupt ein Techn.Platz sitzt
         IF tplcat-tplnr IS INITIAL.                          "YHG135858
            MESSAGE s150.                                     "YHG135858
            CHECK NOT tplcat-tplnr IS INITIAL.                "YHG135858
         ENDIF.                                               "YHG135858

      WHEN OTHERS.
*        raus hier
         MESSAGE s150.                                        "YHG123656
         EXIT.
   ENDCASE.

*  Schnittstelle versorgen
   CASE ltb-stlty.
      WHEN typ_doc.                                           "YHG135858
         csin-doknr = doccat-doknr.                           "YHG135858
         csin-dokar = doccat-dokar.                           "YHG135858
         csin-doktl = doccat-doktl.                           "YHG135858
         csin-dokvr = doccat-dokvr.                           "YHG135858

      WHEN typ_equi.
*del     CSIN-EQUNR = LTB-EQUNR.
         csin-equnr = equicat-equnr.
*del     CSIN-WERKS = LTB-IWERK.                              "YHG096568
         csin-werks = ltb-werks.                              "YHG096568

      WHEN typ_knd.                                           "YHG135858
         csin-vbeln = kndcat-vbeln.                           "YHG135858
         csin-vbpos = kndcat-vbpos.                           "YHG135858
         csin-matnr = kndcat-matnr.                           "YHG135858
         csin-werks = ltb-werks.                              "MBA089075

      WHEN typ_mat.
*del     CSIN-MATNR = LTB-MATNR.                              "YHG135858
         csin-matnr = matcat-matnr.                           "YHG135858
         csin-werks = ltb-werks.

      WHEN typ_prj.                                           "MBA089075
         csin-pspnr = prjcat-pspnr.                           "MBA089075
         csin-matnr = prjcat-matnr.                           "MBA089075
         csin-werks = ltb-werks.                              "MBA089075
*        csin-stlan = prjcat-stlan.                           "MBA089075

      WHEN typ_std.                                           "YHG135858
         csin-stobj = stdcat-stobj.                           "YHG135858
         csin-werks = ltb-werks.                              "YHG136936

      WHEN typ_tpl.                                           "YHG135858
         csin-tplnr = tplcat-tplnr.                           "YHG135858
         csin-werks = ltb-werks.                              "YHG135858
   ENDCASE.

   csin-stlty = ltb-stlty.

   IF     pm_datuv IS INITIAL                                 "HGA147053
      AND pm_datub IS INITIAL.                                "HGA147053

      csin-datuv = min_grg.                                   "HGA147053
      csin-datub = max_grg.                                   "HGA147053
   ELSE.                                                      "HGA147053
      csin-datuv = pm_datuv.
*d    CSIN-DATUB = PM_DATUV.                                  "HGA118836

      IF NOT pm_datub IS INITIAL.                             "HGA151849
         csin-datub = pm_datub.                               "HGA118836
      ELSE.                                                   "HGA151849
         csin-datub = pm_datuv.                               "HGA151849
      ENDIF.                                                  "HGA151849
   ENDIF.                                                     "HGA147053

   csin-emeng = 0 .
   csin-idnrk = ltb-idnrk.
   csin-stlal = ltb-vwalt.
   csin-stlan = ltb-stlan.
   csin-stlkn = ltb-stlkn.
*  Aktivitaet: Anzeigen
   csin-trtyp = 'A'.
*  Callmodus: 01
   csin-cmode = '01'.
   csin-stuez = ltb-stpoz.

*  Dialogbaustein Positionsanzeige aufrufen
   CALL DIALOG 'CS_BOM_DISPLAY'
      EXPORTING csin.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.
ENDFORM.


FORM prep_chk_types.                                          "YHG110068
   CLEAR: chk_types.

   IF NOT pm_doctp IS INITIAL.
      chk_types-doc = typ_doc.
   ENDIF.

   IF NOT pm_equtp IS INITIAL.
      chk_types-equ = typ_equi.
   ENDIF.

   IF NOT pm_kndtp IS INITIAL.
      chk_types-knd = typ_knd.
   ENDIF.

   IF NOT pm_mattp IS INITIAL.
      chk_types-mat = typ_mat.
   ENDIF.

   IF NOT pm_prjtp IS INITIAL.                                "MBA089075
      chk_types-prj = typ_prj.                                "MBA089075
   ENDIF.                                                     "MBA089075

   IF NOT pm_stdtp IS INITIAL.
      chk_types-std = typ_std.
   ENDIF.

   IF NOT pm_tpltp IS INITIAL.
      chk_types-tpl = typ_tpl.
   ENDIF.

   STLTP_IN = CHK_TYPES.                                    "note 308150
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        PRINT_LIST                                                   *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM print_list.
*  ?Druckbild gewuenscht
*  nein
*----------------------------------                           "YHG139715
*..verlegt.........................
*  IF TCSPR-PROPT IS INITIAL.
*     NEW-PAGE PRINT ON
*              LINE-COUNT 44
*              LINE-SIZE 120
*              LAYOUT 'X_44_120'
*              WITH-TITLE
*              NO DIALOG.
*  ja, Druckbild gewuenscht
*  ELSE.
*     NEW-PAGE PRINT ON
*              LINE-COUNT 44
*              LINE-SIZE 120
*              LAYOUT 'X_44_120'
*              WITH-TITLE.
*  ENDIF.
*..................................
*----------------------------------

*  erneute Ausgabe (in Spool, mit neuem Format)
   PERFORM cs15.

*  Druckauftrag bearbeitet
   MESSAGE s518.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        PRINT_MODE_BATCH                                             *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM print_mode_batch.                                        "YHG078090
*----------------------------------

DATA:                                                       "note 486930
  EX2MM TYPE C.                                             "note 486930
*----------------------------------

  call function 'IS_SUBMIT_TO_MEMORY'                       "note 486930
    importing                                               "note 486930
      flag = ex2mm                                          "note 486930
    exceptions                                              "note 486930
      others = 4.                                           "note 486930

  clear: sy-subrc.                                          "note 486930

  IF EX2MM IS INITIAL.                                      "note 486930
   sy-ucomm = 'CSPR'.
   SV_UCOMM = 'CSPR'.                                       "note 351902
  ENDIF.                                                    "note 486930

*d NEW-PAGE PRINT ON                                          "HGA054648
*d     DESTINATION               sy-pdest           "HGB177870"HGA054648
*d     COPIES                    sy-prcop           "HGB177870"HGA054648
*d     LIST NAME                 sy-plist           "HGB177870"HGA054648
*d     LIST DATASET              sy-prdsn           "HGB177870"HGA054648
*d     COVER TEXT                sy-prtxt           "HGB177870"HGA054648
*d     IMMEDIATELY               sy-primm           "HGB177870"HGA054648
*d     KEEP IN SPOOL             sy-prrel           "HGB177870"HGA054648
*d     NEW LIST IDENTIFICATION   sy-prnew           "HGB177870"HGA054648
*d     DATASET EXPIRATION        sy-pexpi           "HGB177870"HGA054648
*d     LINE-COUNT                44                           "HGA054648
*d     LINE-SIZE                 120                          "HGA054648
*d     LAYOUT                    'X_44_120'                   "HGA054648
*d     SAP COVER PAGE            sy-prbig           "HGB177870"HGA054648
*d     RECEIVER                  sy-prrec           "HGB177870"HGA054648
*d     DEPARTMENT                sy-prabt           "HGB177870"HGA054648
*d     WITH-TITLE                                             "HGA054648
*d     NO DIALOG.                                             "HGA054648
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SEL_GRENZEN_01                                               *
*---------------------------------------------------------------------*
*        -->   TCS03               allgem. StlParameter               *
*                                                                     *
*        <--   PM_DATUV            Defaultdatum                       *
*                                                                     *
*        setzt Defaultdatum                                           *
*---------------------------------------------------------------------*
FORM sel_grenzen_01.
*  ?soll Minimaldatum vorgeschlagen werden
*  nein, Systemdatum
   IF tcs03-lowdt IS INITIAL.
      pm_datuv = syst-datum.
*  ja
   ELSE.
      pm_datuv = min_grg.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SEL_GRENZEN_02                                               *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM sel_grenzen_02.
   IF csbomex-submf IS INITIAL.
      GET PARAMETER ID 'CSV' FIELD pm_stlan.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SET_MARGIN                                                   *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM set_margin.
*del IF SY-UCOMM NE 'CSPR'.                                   "YHG078090
*d IF     sy-ucomm NE 'CSPR'                      "YHG078090"note 351902
   IF     SV_UCOMM NE 'CSPR'                                "note 351902
      AND sy-batch IS INITIAL.                                "YHG078090

*del  WRITE: /1      TEXT-099.                                "YHG078090
*del  WRITE: /1      SY-VLINE, 81 SY-VLINE.         "YHG078090"YHG110068
*     gib die Rahmenstriche aus
      WRITE: /1 sy-vline.                                     "YHG110068
      WRITE AT siz_linpf sy-vline.                            "YHG110068
   ELSE.
      WRITE: /1      ' '.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        SET_SCHALTER                                                 *
*---------------------------------------------------------------------*
*        Input :                                                      *
*                                                                     *
*        Output:                                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM set_schalter.                                            "YHG077295
*  ?TCSPR erfolgreich eingelesen
   IF NOT tcspr-csusr IS INITIAL.                             "YHG077712
*     ?Schalter f. direkte Verwendung an
*     ja
      IF NOT tcspr-dirkt IS INITIAL.
*        Parameter vorbelegen
         pm_dirkt = 'X'.
      ENDIF.

*     ?Schalter f. Verwendungen ueber Klassen an
*     ja
      IF NOT tcspr-uebkl IS INITIAL.
*        Parameter vorbelegen
         pm_uebkl = 'X'.
      ENDIF.

      IF NOT tcspr-ltext IS INITIAL.                          "HGA127128
         pm_ltext = 'X'.                                      "HGA127128
      ENDIF.                                                  "HGA127128

      IF NOT tcspr-valst IS INITIAL.                          "HGA246532
         pm_alvsa = 'X'.                                      "HGA246532
      ENDIF.                                                  "HGA246532
   ENDIF.                                                     "YHG077712

*  ?weder direkte Verw. noch Verw. ueber Klassen sitzt
*  ja, weder - noch
   IF     pm_dirkt IS INITIAL
      AND pm_uebkl IS INITIAL.

*     Default 'dir. Verw.' setzen
      pm_dirkt = 'X'.
   ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TCS03_LESEN                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM tcs03_lesen.
   tcs03-agb29 = '29'.
   READ TABLE tcs03.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TCSPR_LESEN                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM tcspr_lesen.
*  TCSPR-Keyfeld fuellen
   tcspr-csusr = sy-uname.
*  Tabelle TCSPR lesen
   READ TABLE tcspr.

*  ?TCSPR-Lesen erfolgreich
*  nö
   IF sy-subrc > 0.                                           "YHG077712
      tcspr-csusr = 'DUMMY'.                                  "HGG054648
      READ TABLE tcspr.                                       "HGG054648

*     WA wieder initialisieren
*d    CLEAR: TCSPR.                                 "YHG077712"HGG054648
   ENDIF.                                                     "YHG077712
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        VERWENDUNG_ANZEIGEN                       USER_COMMAND: CSMV *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM verwendung_anzeigen.
   CASE ltb-stlty.                                            "YHG135858
      WHEN typ_mat.                                           "YHG135858
*        nur, wenn ueberhaupt eine MatNr sitzt
*del     IF LTB-MATNR IS INITIAL.                             "YHG135858
         IF matcat-matnr IS INITIAL.                          "YHG135858
            MESSAGE s150.
*del        CHECK LTB-MATNR NE SPACE.                         "YHG135858
            CHECK matcat-matnr NE space.                      "YHG135858
         ENDIF.

*        erneuter Aufruf von RCS15001
         SUBMIT rcs15001
*del        WITH PM_IDNRK INCL LTB-MATNR                      "YHG135858
            WITH pm_idnrk INCL matcat-matnr                   "YHG135858
            WITH pm_werks INCL ltb-werks
            WITH pm_stlan INCL pm_stlan
            WITH pm_datuv INCL pm_datuv
            WITH pm_datub INCL pm_datub
*del        WITH PM_GBRAZ INCL PM_GBRAZ                       "YHG135858
            WITH pm_alvsa INCL pm_alvsa                     "note 430354
            WITH pm_dsprf INCL pm_dsprf                       "YHG135858
            WITH pm_prprf INCL pm_prprf                       "YHG135858
            WITH pm_ltext INCL pm_ltext
            WITH pm_postp INCL pm_postp
            WITH pm_dirkt INCL pm_dirkt                       "YHG084043
            WITH pm_uebkl INCL pm_uebkl                       "YHG084043
            WITH pm_equtp INCL pm_equtp                       "YHG135858
            WITH pm_mehrs INCL space                          "YHG135858
            WITH pm_mattp INCL pm_mattp                       "YHG135858
            WITH pm_prjtp INCL pm_prjtp                       "MBA089075
            WITH pm_kndtp INCL pm_kndtp                       "MBA087033
            WITH pm_stdtp INCL pm_stdtp                       "YHG135858
            WITH pm_tpltp INCL pm_tpltp                       "YHG135858
            AND RETURN.

      WHEN typ_knd.                                           "MBA087033
*        nur, wenn ueberhaupt eine MatNr (für SOBOM) sitzt
         IF kndcat-matnr IS INITIAL.                          "MBA087033
            MESSAGE s150.                                     "MBA087033
            CHECK kndcat-matnr NE space.                      "MBA087033
         ENDIF.

*        erneuter Aufruf von RCS15001
         SUBMIT rcs15001
            WITH pm_idnrk INCL kndcat-matnr                   "MBA087033
            WITH pm_werks INCL ltb-werks                      "MBA087033
            WITH pm_stlan INCL pm_stlan                       "MBA087033
            WITH pm_datuv INCL pm_datuv                       "MBA087033
            WITH pm_datub INCL pm_datub                       "MBA087033
            WITH pm_alvsa INCL pm_alvsa                     "note 430354
            WITH pm_dsprf INCL pm_dsprf                       "MBA087033
            WITH pm_prprf INCL pm_prprf                       "MBA087033
            WITH pm_ltext INCL pm_ltext                       "MBA087033
            WITH pm_postp INCL pm_postp                       "MBA087033
            WITH pm_dirkt INCL pm_dirkt                       "MBA087033
            WITH pm_uebkl INCL pm_uebkl                       "MBA087033
            WITH pm_equtp INCL pm_equtp                       "MBA087033
            WITH pm_mehrs INCL space                          "MBA087033
            WITH pm_mattp INCL pm_mattp                       "MBA087033
            WITH pm_prjtp INCL pm_prjtp                       "MBA087033
            WITH pm_stdtp INCL pm_stdtp                       "MBA087033
            WITH pm_tpltp INCL pm_tpltp                       "MBA087033
            WITH pm_kndtp INCL pm_kndtp                       "MBA087033
            AND RETURN.                                       "MBA087033

      WHEN typ_doc.                                           "YHG135858
*        nur, wenn ueberhaupt eine DokNr sitzt
         IF doccat-doknr IS INITIAL.                          "YHG135858
            MESSAGE s150.                                     "YHG135858
            CHECK matcat-matnr NE space.                      "YHG135858
         ENDIF.                                               "YHG135858

*        erneuter Aufruf von RCS15011                         "YHG135858
         SUBMIT rcs15011                                      "YHG135858
            WITH pm_doknr INCL doccat-doknr                   "YHG135858
            WITH pm_dokar INCL doccat-dokar                   "YHG135858
            WITH pm_doktl INCL doccat-doktl                   "YHG135858
            WITH pm_dokvr INCL doccat-dokvr                   "YHG135858
            WITH pm_werks INCL ltb-werks                      "YHG135858
            WITH pm_stlan INCL pm_stlan                       "YHG135858
            WITH pm_datuv INCL pm_datuv                       "YHG135858
            WITH pm_datub INCL pm_datub                       "YHG135858
            WITH pm_dirkt INCL pm_dirkt                       "YHG135858
            WITH pm_uebkl INCL pm_uebkl                       "YHG135858
            WITH pm_alvsa INCL pm_alvsa                     "note 430354
            WITH pm_dsprf INCL pm_dsprf                       "YHG135858
            WITH pm_prprf INCL pm_prprf                       "YHG135858
            WITH pm_ltext INCL pm_ltext                       "YHG135858
            WITH pm_postp INCL pm_postp                       "YHG135858
            WITH pm_doctp INCL pm_doctp                       "YHG135858
            WITH pm_equtp INCL pm_equtp                       "YHG135858
            WITH pm_mattp INCL pm_mattp                       "YHG135858
            WITH pm_stdtp INCL pm_stdtp                       "YHG135858
            WITH pm_tpltp INCL pm_tpltp                       "YHG135858
            AND RETURN.                                       "YHG135858
      WHEN OTHERS.                                            "YHG123656
         MESSAGE s150.                                        "YHG123656
   ENDCASE.                                                   "YHG135858

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        GET_OBJDATA                                                  *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM get_objdata.                                             "YHG110068
   CLEAR: doccat,                                             "YHG135858
          equicat,                                            "YHG135858
          kndcat,                                             "YHG135858
          matcat,                                             "YHG135858
          prjcat,                                             "MBA089075
          stdcat,                                             "YHG135858
          tplcat.                                             "YHG135858

   CASE ltb-bmtyp.
      WHEN 'D'.
         READ TABLE doccat INDEX ltb-ttidx.
      WHEN 'E'.
         READ TABLE equicat INDEX ltb-ttidx.
      WHEN 'K'.
         READ TABLE kndcat INDEX ltb-ttidx.
      WHEN 'M'.
         READ TABLE matcat INDEX ltb-ttidx.
      WHEN 'P'.                                               "MBA089075
         READ TABLE prjcat INDEX ltb-ttidx.                   "MBA089075
      WHEN 'S'.
         READ TABLE stdcat INDEX ltb-ttidx.
      WHEN 'T'.
         READ TABLE tplcat INDEX ltb-ttidx.
      WHEN OTHERS.
   ENDCASE.
ENDFORM.


FORM get_profs.                                               "HGA025100
   SELECT SINGLE profl FROM tcsvl
      INTO pm_dsprf
      WHERE     uname EQ sy-uname
            AND progr EQ sy-repid
            AND pverw EQ 'DSPL'.

   IF sy-subrc <> 0.
      SELECT SINGLE class FROM usr02
         INTO usr_class
         WHERE bname EQ sy-uname.

      IF usr_class IS INITIAL.
         PERFORM get_profs_dummy_d.
      ELSE.
         SELECT SINGLE profl FROM tcsvl
            INTO pm_dsprf
            WHERE     uname EQ usr_class
                  AND progr EQ sy-repid
                  AND pverw EQ 'DSPL'.

         IF sy-subrc <> 0.
            PERFORM get_profs_dummy_d.
         ENDIF.
      ENDIF.
   ENDIF.

   SELECT SINGLE profl FROM tcsvl
      INTO pm_prprf
      WHERE     uname EQ sy-uname
            AND progr EQ sy-repid
            AND pverw EQ 'PRNT'.

   IF sy-subrc <> 0.
      SELECT SINGLE class FROM usr02
         INTO usr_class
         WHERE bname EQ sy-uname.

      IF usr_class IS INITIAL.
         PERFORM get_profs_dummy_p.
      ELSE.
         SELECT SINGLE profl FROM tcsvl
            INTO pm_prprf
            WHERE     uname EQ usr_class
                  AND progr EQ sy-repid
                  AND pverw EQ 'PRNT'.

         IF sy-subrc <> 0.
            PERFORM get_profs_dummy_p.
         ENDIF.
      ENDIF.
   ENDIF.
ENDFORM.


FORM get_profs_dummy_d.                                       "HGA025100
   SELECT SINGLE profl FROM tcsvl
      INTO pm_dsprf
      WHERE     uname EQ 'DUMMY       '
            AND progr EQ sy-repid
            AND pverw EQ 'DSPL'.

   IF sy-subrc <> 0.
      pm_dsprf = dflt_dsprf.
   ENDIF.
ENDFORM.


FORM get_profs_dummy_p.                                       "HGA025100
   SELECT SINGLE profl FROM tcsvl
      INTO pm_prprf
      WHERE     uname EQ 'DUMMY       '
            AND progr EQ sy-repid
            AND pverw EQ 'PRNT'.

   IF sy-subrc <> 0.
      pm_prprf = dflt_prprf.
   ENDIF.
ENDFORM.


FORM parameter_anzeigen.                                      "YHG139715
DATA: lcl_lngth LIKE sy-index.
* ---------------------------------

   WINDOW STARTING AT 15 1
          ENDING   AT 79 8.

   SET PF-STATUS 'SNN1'.
   SET TITLEBAR 'A02'.
*d DESCRIBE FIELD dsp_sel LENGTH lcl_lngth.                          "uc
   DESCRIBE FIELD dsp_sel                                            "uc
     LENGTH lcl_lngth                                                "uc
     in character mode.                                              "uc
   NEW-PAGE LINE-SIZE lcl_lngth.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

   LOOP AT dsp_sel.
      WRITE: / dsp_sel.
   ENDLOOP.
ENDFORM.


FORM create_dsp_sel.                                          "YHG139715
DATA: lcl_count LIKE sy-index,
      act_repid LIKE sy-repid.

   REFRESH:  dsp_sel,
             txt_sel.
   READ TEXTPOOL sy-repid INTO txt_sel.

   act_repid = sy-repid.
   CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
        EXPORTING
             curr_report     = act_repid
        TABLES
             selection_table = inp_sel
        EXCEPTIONS
             not_found       = 01
             no_report       = 02.

*d LOOP AT INP_SEL.                                           "HGC154389
*d    CHECK:     INP_SEL-SELNAME NE 'PM_HEMNG'      "YHG032486"HGC154389
*d           AND INP_SEL-SELNAME NE 'PM_HRMNG'.     "YHG032486"HGC154389
   LOOP AT inp_sel                                            "HGC154389
      WHERE     selname NE 'PM_HEMNG'                         "HGC154389
            AND selname NE 'PM_HRMNG'.                        "HGC154389

*d    LOOP AT TXT_SEL.                                        "HGC154389
*del    IF     INP_SEL-SELNAME EQ TXT_SEL-ID+1                "YHG032486
*del       AND INP_SEL-SELNAME NE 'PM_HEMNG'                  "YHG032486
*del       AND INP_SEL-SELNAME NE 'PM_HRMNG'.                 "YHG032486
*d      CHECK INP_SEL-SELNAME EQ TXT_SEL-ID+1.      "YHG032486"HGC154389
      LOOP AT txt_sel                                         "HGC154389
*d       WHERE id+1 EQ inp_sel-selname.                    "HGC154389"uc
         WHERE key EQ inp_sel-selname.                               "uc

*d         ASSIGN (txt_sel-id+1) TO <pm_name>.                       "uc
           ASSIGN (txt_sel-key) TO <pm_name>.                        "uc
           IF NOT <pm_name> IS INITIAL.
*d            dsp_sel-text = txt_sel-text+8.                         "uc
              dsp_sel-text = txt_sel-entry.                          "uc

*del          CLEAR: LCL_COUNT.                               "YHG032486
              CLEAR: lcl_count,                               "YHG032486
                     tmp_cnt.                                 "YHG032486

*del          WHILE DSP_SEL-TEXT+28(2) IS INITIAL.            "YHG032486
*del             DSP_SEL-TEXT+29(1) = '_'.                    "YHG032486
*del             SHIFT DSP_SEL-TEXT CIRCULAR RIGHT.           "YHG032486
*del             LCL_COUNT = LCL_COUNT + 1.                   "YHG032486
*del          ENDWHILE.                                       "YHG032486
*del          SHIFT DSP_SEL-TEXT                              "YHG032486
*del             BY LCL_COUNT PLACES CIRCULAR LEFT.           "YHG032486

              tmp_cnt = strlen( dsp_sel-text ).               "YHG032486
              IF tmp_cnt <= i28.                              "YHG032486
                 tmp_cnt = tmp_cnt + 1.                       "YHG032486
                 lcl_count = 30 - tmp_cnt.                    "YHG032486
                 dsp_sel-text+tmp_cnt = shortln(lcl_count).   "YHG032486
              ENDIF.                                          "YHG032486

              dsp_sel-wert = inp_sel-low.

ENHANCEMENT-POINT REATE_DSP_SEL_01 SPOTS ES_RCS15NN1 INCLUDE BOUND.
              IF inp_sel-selname EQ 'PM_DATUV'.
                 CLEAR: dsp_sel-wert.
                 WRITE pm_datuv TO dsp_sel-wert.
              ENDIF.

              IF inp_sel-selname EQ 'PM_DATUB'.
                 CLEAR: dsp_sel-wert.
                 WRITE pm_datub TO dsp_sel-wert.
              ENDIF.

              IF     inp_sel-selname EQ 'PM_EMENG'
                 AND NOT pm_emeng IS INITIAL.

                 CLEAR: dsp_sel-wert.
*d               WRITE PM_EMENG TO DSP_SEL-WERT.              "MBA087033
                 WRITE pm_emeng TO dsp_sel-wert DECIMALS 3.   "MBA087033
                 WHILE dsp_sel-wert(1) EQ space.
                    SHIFT dsp_sel-wert LEFT.
                 ENDWHILE.
              ENDIF.

              IF     inp_sel-selname EQ 'PM_RMENG'
                 AND NOT pm_rmeng IS INITIAL.

                 CLEAR: dsp_sel-wert.
*d               WRITE PM_RMENG TO DSP_SEL-WERT.              "MBA087033
                 WRITE pm_rmeng TO dsp_sel-wert DECIMALS 3.   "MBA087033
                 WHILE dsp_sel-wert(1) EQ space.
                    SHIFT dsp_sel-wert LEFT.
                 ENDWHILE.
              ENDIF.

              APPEND dsp_sel.
           ENDIF.
*del    ENDIF.                                                "YHG032486
      ENDLOOP.
   ENDLOOP.

   SORT dsp_sel BY text.
ENDFORM.


FORM selkrit_druck.
   CHECK NOT tcspr-prsel IS INITIAL.                          "HGA127128

   CLEAR: ltb_add.
   WRITE text-050 TO ltb_add-tline(76).

   FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.

   CLEAR: watab. REFRESH watab.

*d watab-tname = 'STPOL_ADD'. watab-table = ltb_add .                "uc
   WATAB-TNAME = 'STPOL_ADD'.                                        "uc
   assign watab-table to <x_watab-table>  casting.                   "uc
   assign ltb_add     to <x_STPOL_ADD_wa> casting.                   "uc
   <x_watab-table> = <x_STPOL_ADD_wa> .                              "uc
   APPEND watab.
   CLEAR: watab.

   PERFORM write_block
      USING 'SEL_GNRL_HDNG     '
*           ausgegebene Zeilen nicht zaehlen
            ' '
*           Hide nicht ausfuehren
            ' '.                                              "YHG123656

   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   LOOP AT dsp_sel.
      ltb_add-selnm = dsp_sel(32).
      TRANSLATE ltb_add-selnm USING '_ '.
      ltb_add-selvl = dsp_sel-wert.

      CLEAR: watab. REFRESH watab.

*d    watab-tname = 'STPOL_ADD'. watab-table = ltb_add .             "uc
      WATAB-TNAME = 'STPOL_ADD'.                                     "uc
      assign watab-table to <x_watab-table>  casting.                "uc
      assign ltb_add     to <x_STPOL_ADD_wa> casting.                "uc
      <x_watab-table> = <x_STPOL_ADD_wa> .                           "uc
      APPEND watab.
      CLEAR: watab.

      PERFORM write_block
         USING 'SEL_PARAM         '
*              ausgegebene Zeilen nicht zaehlen
               ' '
*              Hide nicht ausfuehren
               ' '.                                           "YHG123656
   ENDLOOP.
   NEW-PAGE.
ENDFORM.


FORM prep_druck.                                              "YHG139715
   siz_linpf = siz_linpf - 2.
   IF siz_linpf <= 79.
*     IF TCSPR-PROPT IS INITIAL.                              "HGA054648
      IF NOT syst-batch IS INITIAL.                           "HGA054648
         NEW-PAGE PRINT ON
             DESTINATION               sy-pdest               "HGB177870
             COPIES                    sy-prcop               "HGB177870
             LIST NAME                 sy-plist               "HGB177870
             LIST DATASET              sy-prdsn               "HGB177870
             COVER TEXT                sy-prtxt               "HGB177870
             IMMEDIATELY               sy-primm               "HGB177870
             KEEP IN SPOOL             sy-prrel               "HGB177870
             NEW LIST IDENTIFICATION   sy-prnew               "HGB177870
             DATASET EXPIRATION        sy-pexpi               "HGB177870
             LINE-COUNT                62
             LINE-SIZE                 siz_linpf
             LAYOUT                    'X_65_80'
             SAP COVER PAGE            sy-prbig               "HGB177870
             RECEIVER                  sy-prrec               "HGB177870
             DEPARTMENT                sy-prabt               "HGB177870
             WITH-TITLE
             NO DIALOG.
      ELSE.
         IF tcspr-propt IS INITIAL.                           "HGA054648
            NEW-PAGE PRINT ON                                 "HGA054648
                LINE-COUNT 62                                 "HGA054648
                LINE-SIZE siz_linpf                           "HGA054648
                LAYOUT 'X_65_80'                              "HGA054648
                WITH-TITLE                                    "HGA054648
                NO DIALOG.                                    "HGA054648
         ELSE.                                                "HGA054648
            NEW-PAGE PRINT ON
                     LINE-COUNT 62
                     LINE-SIZE siz_linpf
                     LAYOUT 'X_65_80'
                     WITH-TITLE.
         ENDIF.                                               "HGA054648
      ENDIF.
   ELSE.                                                      "YHG139715
*     IF TCSPR-PROPT IS INITIAL.                    "YHG139715"HGA054648
      IF NOT syst-batch IS INITIAL.                           "HGA054648
         NEW-PAGE PRINT ON                                    "YHG139715
             DESTINATION               sy-pdest               "HGB177870
             COPIES                    sy-prcop               "HGB177870
             LIST NAME                 sy-plist               "HGB177870
             LIST DATASET              sy-prdsn               "HGB177870
             COVER TEXT                sy-prtxt               "HGB177870
             IMMEDIATELY               sy-primm               "HGB177870
             KEEP IN SPOOL             sy-prrel               "HGB177870
             NEW LIST IDENTIFICATION   sy-prnew               "HGB177870
             DATASET EXPIRATION        sy-pexpi               "HGB177870
             LINE-COUNT                44                     "YHG139715
             LINE-SIZE                 siz_linpf              "YHG139715
             LAYOUT                    'X_44_120'             "YHG139715
             SAP COVER PAGE            sy-prbig               "HGB177870
             RECEIVER                  sy-prrec               "HGB177870
             DEPARTMENT                sy-prabt               "HGB177870
             WITH-TITLE                                       "YHG139715
             NO DIALOG.                                       "YHG139715
      ELSE.                                                   "YHG139715
         IF tcspr-propt IS INITIAL.                           "HGA054648
            NEW-PAGE PRINT ON                                 "HGA054648
                LINE-COUNT 44                                 "HGA054648
                LINE-SIZE siz_linpf                           "HGA054648
                LAYOUT 'X_44_120'                             "HGA054648
                WITH-TITLE                                    "HGA054648
                NO DIALOG.                                    "HGA054648
         ELSE.                                                "HGA054648
            NEW-PAGE PRINT ON                                 "YHG139715
                     LINE-COUNT 44                            "YHG139715
                     LINE-SIZE siz_linpf                      "YHG139715
                     LAYOUT 'X_44_120'                        "YHG139715
                     WITH-TITLE.                              "YHG139715
         ENDIF.                                               "HGA054648
      ENDIF.                                                  "YHG139715
   ENDIF.                                                     "YHG139715
   siz_linpf = siz_linpf + 2.
ENDFORM.


*eject
*---------------------------------------------------------------------*
*        TPL_ANZEIGEN                              USER_COMMAND: CSAO *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM tpl_anzeigen.                                            "HGD099459
* ---------------------------------
DATA: sav_tplnr LIKE tpst-tplnr.

* ---------------------------------
*  nur, wenn ueberhaupt eine TplNr sitzt
   IF tplcat-tplnr IS INITIAL.
      MESSAGE s150.
      CHECK tplcat-tplnr NE space.
   ENDIF.

*  "Schnittstelle" sichern
   GET PARAMETER ID 'IFL' FIELD sav_tplnr.

*  Parameterarea mit TplNr versorgen
   SET PARAMETER ID 'IFL' FIELD tplcat-tplnr.
*  Transaktion 'Anzeigen TechnPlatz' aufrufen
   CALL TRANSACTION 'IL03'
        AND SKIP FIRST SCREEN.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.

*  Restore "Schnittstelle"
   SET PARAMETER ID 'IFL' FIELD sav_tplnr.
ENDFORM.
*eject
*----------------------------------------------------------------------*
*       GET_SALES_ORDER_DATA
*----------------------------------------------------------------------*
*       Add Sales Order BOM information to LTB
*----------------------------------------------------------------------*
* MBA084935 230298 New - Add Sales Order Functionality
*----------------------------------------------------------------------*
FORM get_sales_order_data.                                    "MBA084935

* WorkArea KndAuftragsStl
DATA : BEGIN OF wa_kd OCCURS 0.
         INCLUDE STRUCTURE kdstb.
DATA : END OF wa_kd.

* Sy-subrc temporarily storing
DATA : sy_subrc_tmp LIKE sy-subrc.

CHECK NOT ( ltb IS INITIAL ).

sy_subrc_tmp = sy-subrc.

LOOP AT ltb WHERE stlty = 'K'.
    REFRESH wa_kd.
    CLEAR wa_kd.
    wa_kd-werks = ltb-werks.
    wa_kd-matnr = ltb-matnr.
    wa_kd-stlnr = ltb-stlnr.
    CALL FUNCTION 'GET_KDST'
        EXPORTING
             all             = ' '
             no_buffer       = ' '
             set             = 'X'
         TABLES
             wa              = wa_kd
         EXCEPTIONS
             call_invalid    = 1
             end_of_table    = 2
             get_without_set = 3
             key_incomplete  = 4
             key_invalid     = 5
             no_record_found = 6
             OTHERS          = 7.
    IF sy-subrc <> 0.
*       message exxx(29).
    ELSE.
        ltb-vbeln = wa_kd-vbeln.
        ltb-vbpos = wa_kd-vbpos.
        MODIFY ltb.
    ENDIF.
ENDLOOP.
sy-subrc = sy_subrc_tmp.
ENDFORM.                    " GET_SALES_ORDER_DATA
*eject
*---------------------------------------------------------------------*
*        PRJ_ANZEIGEN                                                 *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM prj_anzeigen.                                            "MBA089075
* ---------------------------------
DATA: sav_prjnr LIKE prst-pspnr,
      tmp_prjnr LIKE prps-posid,
      tmp_prps  LIKE prps.

* ---------------------------------
*  nur, wenn ueberhaupt eine PrjNr sitzt
   IF prjcat-pspnr IS INITIAL.
      MESSAGE s150.
      CHECK prjcat-pspnr NE space.
   ENDIF.

*  "Schnittstelle" sichern
   GET PARAMETER ID 'PRO' FIELD sav_prjnr.

*  'interne' PSPNR in 'externe' umwandeln
   CALL FUNCTION 'CJPN_GET_WBS_ELEMENT'
        EXPORTING
             i_pspnr         = prjcat-pspnr
        IMPORTING
             e_prps          = tmp_prps
        EXCEPTIONS
             input_error     = 1
             not_found       = 2
             OTHERS          = 3.
   CHECK sy-subrc IS INITIAL.

   tmp_prjnr = tmp_prps-posid.

*  Parameterarea mit PrjNr versorgen
   SET PARAMETER ID 'PRO' FIELD tmp_prjnr.
*  Transaktion 'Anzeigen PrjSnPlatz' aufrufen
   CALL TRANSACTION 'CJ13'
        AND SKIP FIRST SCREEN.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.

*  Restore "Schnittstelle"
   SET PARAMETER ID 'PRO' FIELD sav_prjnr.
ENDFORM.

*eject
*---------------------------------------------------------------------*
*        KND_ANZEIGEN (Detail)                                        *
*---------------------------------------------------------------------*
*        -->                                                          *
*                                                                     *
*        <--                                                          *
*                                                                     *
*---------------------------------------------------------------------*
FORM knd_anzeigen.                                            "MBA089075
* ---------------------------------
DATA: sav_kndnr LIKE kdst-vbeln.
* ---------------------------------
*  nur, wenn ueberhaupt eine KndNr sitzt
   IF kndcat-vbeln IS INITIAL.
      MESSAGE s150.
      CHECK kndcat-vbeln NE space.
   ENDIF.

*  "Schnittstelle" sichern
   GET PARAMETER ID 'AUN' FIELD sav_kndnr.

*  Parameterarea mit KndNr versorgen
   SET PARAMETER ID 'AUN' FIELD kndcat-vbeln.
*  Transaktion 'Anzeigen Kundenauftrag' aufrufen
   CALL TRANSACTION 'VA03'
        AND SKIP FIRST SCREEN.
*  Reset Parameterarea bzgl. KndAuftrag
   SET PARAMETER ID 'AUN' FIELD ' '.

*  Reset HIDE-Bereich
   PERFORM clr_hide_area.

*  Restore "Schnittstelle"
   GET PARAMETER ID 'AUN' FIELD sav_kndnr.
ENDFORM.
FORM TXT_CONV_CHK                                           "note 310574
  tables
    txt structure tline
  using
    potx1 like stpo-potx1
    potx2 like stpo-potx2 .

data:
  shft like sy-index.

  describe table txt lines sy-tabix.
  if sy-tabix <> 0.
    loop at txt
       where tdline cs '</>'.

       exit.
    endloop.

    if sy-subrc = 0.
      CALL FUNCTION 'CONVERT_TEXT'
        EXPORTING
          FORMAT_TYPE = 'ASCII'
        TABLES
          foreign     = txt
          itf_lines   = txt.

*d    describe field txt-tdformat length shft.                       "uc
      describe field txt-tdformat                                    "uc
        length shft                                                  "uc
        in character mode.                                           "uc

      loop at txt.
        shift txt by shft places right.
        txt-tdformat = '*'.
        modify txt.
      endloop.
    endif.
  else.
     if   (     potx1 cs '<'
            and potx1 cs '>')
       or (     potx2 cs '<'
            and potx2 cs '>') .

      txt-tdformat = '*'.
      txt-tdline = potx1.
      append txt.
      txt-tdline = potx2.
      append txt.

      CALL FUNCTION 'CONVERT_TEXT'
        EXPORTING
          FORMAT_TYPE = 'ASCII'
        TABLES
          foreign     = txt
          itf_lines   = txt.

*d    describe field txt-tdformat length shft.                       "uc
      describe field txt-tdformat                                    "uc
        length shft                                                  "uc
        in character mode.                                           "uc

      read table txt index 1.
        shift txt by shft places right.
        potx1 = txt-tdline.

      read table txt index 2.
        shift txt by shft places right.
        potx2 = txt-tdline.

      refresh:
        txt.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETERMINE_INDEX .
perform get_ldm_switch(saplcsdi) changing switch_values if found.
 if not switch_values-cs15_top_lvl is initial.
    if alv_ltb-level eq 0.
       index_tab-high_index = l_line.
       append index_tab.
       clear index_tab.
    endif.
 endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_TOPLEVEL_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_TOPLEVEL_FLAG .
perform get_ldm_switch(saplcsdi) changing switch_values if found.
 if not switch_values-cs15_top_lvl is initial.
 if index_tab[] is not initial.
     l_count = 1.
     loop at alv_ltb.
      read table index_tab index l_count.
      if sy-subrc = 0.
         if index_tab-high_level eq alv_ltb-level.
            alv_ltb-crtfg = 'X'.
            modify alv_ltb transporting crtfg.
         endif.
      endif.
      if alv_ltb-level = 0.
        l_count = l_count + 1.
      else.
         if index_tab-high_level eq alv_ltb-level.
            alv_ltb-crtfg = 'X'.
            modify alv_ltb transporting crtfg.
         endif.
      endif.
     endloop.
     clear l_count.
   endif.
endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEPARATOR_AT_END
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEPARATOR_AT_END .
perform get_ldm_switch(saplcsdi) changing switch_values if found.
 if not switch_values-cs15_top_lvl is initial.
    describe table ltb LINES l_lines.
    if l_lines = ltb_loopx.
       read table ltb index l_lines.
         if sy-subrc = 0.
           if ltb-level ne 0.
              index_tab-high_level = ltb-level.
           endif.
         endif.
       clear alv_ltb.
       alv_ltb-info = 'C30'.
       append alv_ltb.
      if alv_ltb-level eq 0.
        index_tab-high_index = l_lines.
        append index_tab.
        clear index_tab.
      endif.
    endif.
 endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_HIGH_LEVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETERMINE_HIGH_LEVEL .
perform get_ldm_switch(saplcsdi) changing switch_values if found.
 if not switch_values-cs15_top_lvl is initial.
    l_line = ltb_loopx - 1.
    read table ltb index l_line.
       if sy-subrc = 0.
          if ltb-level ne 0.
             index_tab-high_level = ltb-level.
          endif.
       endif.
 endif.
ENDFORM.
