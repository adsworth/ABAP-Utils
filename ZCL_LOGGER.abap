class zcl_logger definition
  create public .
  public section.
    types:
      begin of ty_log_data,
        header     type  bal_s_log,
        statistics type  bal_s_scnt,
        messages   type  bal_t_msgr,
        exceptions type  bal_t_excr_mass,
      end of ty_log_data .

    methods constructor
      importing
        !is_log_header      type bal_s_log optional
        !i_object           type balobj_d optional
        !i_subobject        type balsubobj optional
        !i_loghandle        type balloghndl optional
        !i_save_immediately type abap_bool default abap_true
        !i_keep_duration    type syst_index default '2'
      raising
        zcx_fm_std_error .

    methods level_up .
    methods level_set
      importing
        !i_level type ballevel .
    methods level_get
      returning
        value(r_level) type ballevel .
    methods level_down .
    methods get_log_messages_as_returntab
      returning
        value(rt_bapiret2) type bapiret2_t .

    class-methods add_exc_to_ret_tab
      importing
        !io_exc     type ref to cx_root
        !i_type     type symsgty optional
      changing
        !ct_ret_tab type bapiret2_t .

    class-methods create_logger_with_handle
      importing
        !i_loghandle     type balloghndl
      returning
        value(ro_logger) type ref to zcl_logger
      raising
        zcx_fm_std_error .
    class-methods create_message
      importing
        !i_message_class  type any default sy-msgid
        !i_message_type   type any default sy-msgty
        !i_message_number type any default sy-msgno
        !i_message_v1     type any default sy-msgv1
        !i_message_v2     type any default sy-msgv2
        !i_message_v3     type any default sy-msgv3
        !i_message_v4     type any default sy-msgv4
        !i_probclass      type balprobcl default '1'
      returning
        value(es_return)  type bapiret2 .
    class-methods has_bapiret_t_error
      importing
        !it_return       type bapiret2_tt
      returning
        value(has_error) type abap_bool .
    methods delete_from_db .
    methods flush
      returning value(r_lognumber) type bal_s_lgnm-lognumber.
    methods get_data
      returning
        value(rs_data) type ty_log_data
      raising
        zcx_fm_app_error .
    methods get_log_handle
      returning
        value(rval) type balloghndl .
    methods log_bapiret2
      importing
        !is_return   type bapiret2
        !i_probclass type balprobcl default '1'.
    methods log_bapiret2_table
      importing
        !it_return type bapiret2_t.
    methods log_debug
      importing
        !io_exception type ref to cx_root.
    methods log_error
      importing
        !io_exception type ref to cx_root
        !i_probclass  type balprobcl default '1' .
    methods log_info
      importing
        !io_exception type ref to cx_root.
    methods log_message
      importing
        !i_message_class  type any
        !i_message_type   type any default 'E'
        !i_message_number type any
        !i_message_v1     type any optional
        !i_message_v2     type any optional
        !i_message_v3     type any optional
        !i_message_v4     type any optional
        !i_probclass      type balprobcl default '1'.
    methods log_syst
      importing
        !i_probclass type balprobcl default '1'.
    methods log_text
      importing
        !i_msgty     type symsgty default 'I'
        !i_text      type string optional
        !it_text     type string_table
        !i_probclass type balprobcl default '1'.
    methods log_warning
      importing
        !io_exception type ref to cx_root.

  protected section.
    data detlevel type ballevel value '1' ##NO_TEXT.

  private section.
    types:
      t_problem_class_to_save type range of balprobcl .

    data mv_log_handle type balloghndl .
    data mv_save_immediatly type boolean .
    data mt_problem_class_to_add type t_problem_class_to_save .

    methods add_exception_to_log
      importing
        !ir_exception   type ref to cx_root
        !i_problemclass type balprobcl
        !i_message_type type symsgty.
    methods add_free_text_to_log
      importing
        !it_text        type string_table
        !i_problemclass type balprobcl
        !i_message_type type symsgty.
    methods add_message_to_log
      importing
        !is_message type bal_s_msg .
endclass.

class zcl_logger implementation.
  method add_exception_to_log.
**********************************************************************
* Zweck:     fügt ein Exceptionobjekt dem Log hinzu
* Input:     Exceptionobjekt,
*            Problemklasse
*               1 -> sehr wichtig
*               2 -> wichtig
*               3 -> mittel
*               4 -> Zusatzinformationen
*            Typ der Message (E,W,I,S..)
* Output:    <...>
* Anmerkung: eingebettete Exceptions (-> PREVIOUS) werden zu erst
*            ausgegeben
**********************************************************************
    data:
      ls_exception type bal_s_exc.

    if ir_exception is initial.
      return.
    endif.

    "Problemklasse muss speicherrelevant sein
    check i_problemclass in mt_problem_class_to_add.

* mögliche Vorgänger Exceptions zuerst ausgeben
    if not ir_exception->previous is initial.
      call method add_exception_to_log
        exporting
          ir_exception   = ir_exception->previous
          i_problemclass = i_problemclass
          i_message_type = i_message_type.
    endif.

* dann die eigentliche Exception
    ls_exception-exception = ir_exception.
    ls_exception-msgty     = i_message_type.
    ls_exception-probclass = i_problemclass.
    ls_exception-detlevel  = me->detlevel.

    call function 'BAL_LOG_EXCEPTION_ADD'
      exporting
        i_log_handle  = mv_log_handle
        i_s_exc       = ls_exception
      exceptions
        log_not_found = 0
        others        = 1.

    if sy-subrc <> 0.
      exit.
    endif.

* sofortiges Speichern, wenn gesetzt
    if not mv_save_immediatly is initial.
      me->flush( ).
    endif.
  endmethod.

  method add_exc_to_ret_tab.
* Eine Ausnahme an eine BAPIRETURN Tabelle hängen:
    call function 'RS_EXCEPTION_TO_BAPIRET2'
      exporting
        i_r_exception = io_exc
      changing
        c_t_bapiret2  = ct_ret_tab.
  endmethod.

  method add_free_text_to_log .
**********************************************************************
* Zweck:     fügt Freitext dem Log hinzu
* Input:     Texttabelle,
*            Problemklasse
*               1 -> sehr wichtig
*               2 -> wichtig
*               3 -> mittel
*               4 -> Zusatzinformationen
*            Typ der Message (E,W,I,S..)
*            Kontextdaten der Nachricht (z.B. Faktenmodel ID, Aktion-Name)
* Output:    <...>
    data:
      lv_text(200) type c.  " 4 * symsgv
* Problemklasse muss speicherrelevant sein
    check i_problemclass in mt_problem_class_to_add.

    loop at it_text into lv_text.
      call function 'BAL_LOG_MSG_ADD_FREE_TEXT'
        exporting
          i_log_handle     = mv_log_handle
          i_msgty          = i_message_type
          i_probclass      = i_problemclass
          i_text           = lv_text
          i_detlevel       = me->detlevel
        exceptions
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3.
    endloop.
* sofortiges Speichern, wenn gesetzt
    if not mv_save_immediatly is initial.
      me->flush( ).
    endif.
  endmethod.

  method add_message_to_log .
**********************************************************************
* Zweck:     fügt eine T100 Nachricht dem Log hinzu
* Input:     Anwendungs-Log: Daten einer Meldung,
*            Problemklasse
*               1 -> sehr wichtig
*               2 -> wichtig
*               3 -> mittel
*               4 -> Zusatzinformationen
*            Typ der Message (E,W,I,S..)
* Output:    <...>
**********************************************************************
* Problemklasse muss speicherrelevant sein
    check is_message-probclass in mt_problem_class_to_add.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle     = mv_log_handle
        i_s_msg          = is_message
      exceptions
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3.

* sofortiges Speichern, wenn gesetzt
    if not mv_save_immediatly is initial.
      me->flush( ).
    endif.

  endmethod.

  method constructor.
**********************************************************************
* Zweck:     Erzeugen einer Logger Instanz
* Input:     Protokollheader, Nachrichtenklasse (optional)
* Output:    <...>
* Anmerkung: - erzeugt ein Log und weist Log Handle der globalen
*              Variable mv_log_handle zu
*            - alle nachfolgenden Nachrichten werden zu diesem Handle
*              eingefügt
* Änderung: umbau auf CL_BAL_LOG
    data:
      ls_log_header    type bal_s_log,
      ls_sel_probclass like line of mt_problem_class_to_add,
      lt_log_handle    type bal_t_logh.

    mv_save_immediatly = i_save_immediately.

    ls_log_header = is_log_header.
    if ls_log_header-object is initial.
      ls_log_header-object = i_object.
    endif.
    if ls_log_header-subobject is initial.
      ls_log_header-subobject = i_subobject.
    endif.

*   Verfallsdatum setzen
    if i_loghandle is not initial.
      append i_loghandle to lt_log_handle.
      call function 'BAL_DB_LOAD'
        exporting
*         i_t_log_header     =
          i_t_log_handle     = lt_log_handle
*         i_t_lognumber      =
*         i_client           = sy-mandt
*         i_do_not_load_messages        = ' '
*         i_exception_if_already_loaded =
*         i_lock_handling    = 2
*        importing
*         e_t_log_handle     =
*         e_t_msg_handle     =
*         e_t_locked         =
        exceptions
          no_logs_specified  = 1
          log_not_found      = 2
          log_already_loaded = 3
          others             = 4.
*      "dump here
      me->mv_log_handle = i_loghandle.
    else.
* Default 2 Monate
      call function 'CALCULATE_DATE'
        exporting
*         DAYS        = ''
          months      = i_keep_duration
          start_date  = sy-datum
        importing
          result_date = ls_log_header-aldate_del.

      "Appl-Log: Protokoll ist bis zum Verfalldatum zu halten" setzen
      ls_log_header-del_before = abap_false.

      "welche Problemklasse soll gespeichert werden?
      insert value #( sign = 'I'
                      option = 'BT'
                      low = '1'
                      high = '5' ) into table mt_problem_class_to_add.

* Appl Log Instanz erzeugen und Handle beziehen
      call function 'BAL_LOG_CREATE'
        exporting
          i_s_log                 = ls_log_header
        importing
          e_log_handle            = mv_log_handle
        exceptions
          log_header_inconsistent = 1
          others                  = 2.
      if sy-subrc <> 0.
        raise exception type zcx_fm_std_error
          exporting
            textid = zcx_fm_std_error=>log_header_error.
      endif.
    endif.
  endmethod.

  method create_logger_with_handle.
    create object ro_logger
      exporting
        i_loghandle = i_loghandle.
  endmethod.

  method create_message .
**********************************************************************
* Zweck:     statische Methode zur Erzeugung einer BAPIRET2 Struktur
* Input:     untypisierte Message Parameter (alle Optional)
* Output:    gefüllte BAPIRET2 Struktur
    data:
      l_type   type bapireturn-type,
      l_cl     type sy-msgid,
      l_number type sy-msgno,
      l_v1     type sy-msgv1,
      l_v2     type sy-msgv1,
      l_v3     type sy-msgv1,
      l_v4     type sy-msgv1.
    move:
      i_message_class  to l_cl,
      i_message_type   to l_type,
      i_message_number to l_number,
      i_message_v1     to l_v1,
      i_message_v2     to l_v2,
      i_message_v3     to l_v3,
      i_message_v4     to l_v4.
    call function 'BALW_BAPIRETURN_GET2'
      exporting
        type   = l_type
        cl     = l_cl
        number = l_number
        par1   = l_v1
        par2   = l_v2
        par3   = l_v3
        par4   = l_v4
      importing
        return = es_return.
  endmethod.

  method delete_from_db.
    call function 'BAL_LOG_DELETE'
      exporting
        i_log_handle = me->mv_log_handle
*     EXCEPTIONS
*       LOG_NOT_FOUND       = 1
*       OTHERS       = 2
      .
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endmethod.

  method flush .
    data:
      lt_log_handle type bal_t_logh,
      lt_msgs       type bal_s_msg,
      lv_stat       type bal_s_scnt.

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle  = mv_log_handle
      importing
        e_statistics  = lv_stat
      exceptions
        log_not_found = 1.

    check lv_stat-msg_cnt_al gt 0.

    append mv_log_handle to lt_log_handle.

    data: lt_lognumbers type bal_t_lgnm.

    call function 'BAL_DB_SAVE'
      exporting
        i_t_log_handle   = lt_log_handle
      importing
        e_new_lognumbers = lt_lognumbers
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3.

    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    else.
      if lt_lognumbers is not initial.
        read table lt_lognumbers index 1 into data(ls_lognumber).
        r_lognumber = ls_lognumber-lognumber.
      endif.
    endif.
  endmethod.

  method get_log_handle.
    rval = mv_log_handle.
  endmethod.

  method has_bapiret_t_error.
* Hat die Übergebene Tabelle einen Eintrag vom Typ E oder A?
    read table it_return with key type = 'E' transporting no fields.
    if sy-subrc ne 0.
      read table it_return with key type = 'A' transporting no fields.
    endif.
    if sy-subrc eq 0.
      has_error = abap_true.
    else.
      has_error = abap_false.
    endif.
  endmethod.

  method log_bapiret2 .
**********************************************************************
* Zweck:     Loggen einer gefüllten BAPIRET2 Struktur
* Input:     gefüllte BAPIRET2 Struktur
* Output:    <...>
**********************************************************************
    data:
      ls_message type bal_s_msg.
    move:
      is_return-type       to ls_message-msgty,
      is_return-id         to ls_message-msgid,
      is_return-number     to ls_message-msgno,
      is_return-message_v1 to ls_message-msgv1,
      is_return-message_v2 to ls_message-msgv2,
      is_return-message_v3 to ls_message-msgv3,
      is_return-message_v4 to ls_message-msgv4.
    ls_message-probclass = i_probclass.
    ls_message-detlevel  = me->detlevel.
    me->add_message_to_log( ls_message  ).
  endmethod.

  method log_bapiret2_table .
**********************************************************************
* Zweck:     Loggen einer gefüllten BAPIRET2 Tabelle
* Input:     gefüllte BAPIRET2 Tabelle
* Output:    <...>
* Anmerkung: <Zusatzinfos, Vorgehen bei Implementierung
*             Dinge die man später noch wissen sollte, etc,>
**********************************************************************
    data:
      ls_message type bal_s_msg,
      ls_return  type bapiret2.
    loop at it_return into ls_return.
      me->log_bapiret2(
        is_return = ls_return ).
    endloop. "it_return
  endmethod.

  method log_debug .
**********************************************************************
* Zweck:     Loggen einer Debug Information
* Input:     Exceptionobjekt
* Output:    <...>
* Anmerkung: wird als Info mit Problemklasse 4 (Zusatzinformationen)
*            geloggt
**********************************************************************
    call method add_exception_to_log
      exporting
        ir_exception   = io_exception
        i_problemclass = '4'
        i_message_type = 'I'.
  endmethod.

  method log_error .
**********************************************************************
* Zweck:     Loggen einer Fehler Information
* Input:     Exceptionobjekt
* Output:    <...>
* Anmerkung: wird als Error mit Problemklasse 1 (sehr wichtig)
*            geloggt
**********************************************************************
    call method add_exception_to_log
      exporting
        ir_exception   = io_exception
        i_problemclass = i_probclass
        i_message_type = 'E'.
  endmethod.

  method log_info .
**********************************************************************
* Zweck:     Loggen einer Information
* Input:     Exceptionobjekt
* Output:    <...>
* Anmerkung: wird als Info mit Problemklasse 3 (mittel)
*            geloggt
**********************************************************************
    call method add_exception_to_log
      exporting
        ir_exception   = io_exception
        i_problemclass = '3'
        i_message_type = 'I'.
  endmethod.

  method log_message .
**********************************************************************
* Zweck:     erzeugt einen Log-Eintrag aus T001-Nachrichten
* Input:     untypisierte Parameter für T001-Nachricht (alle optinal)
* Output:    <...>
* Anmerkung: <Zusatzinfos, Vorgehen bei Implementierung
*             Dinge die man später noch wissen sollte, etc,>
**********************************************************************
    data:
      lv_message_class type string,
      ls_return        type bapiret2.
    call method create_message
      exporting
        i_message_class  = i_message_class
        i_message_type   = i_message_type
        i_message_number = i_message_number
        i_message_v1     = i_message_v1
        i_message_v2     = i_message_v2
        i_message_v3     = i_message_v3
        i_message_v4     = i_message_v4
      receiving
        es_return        = ls_return.

    call method me->log_bapiret2
      exporting
        is_return = ls_return.
  endmethod.

  method log_syst .
* Zweck:     loggen einer SYST-Message
    data:
      ls_return type bapiret2.
    call method create_message
      exporting
        i_message_class  = sy-msgid
        i_message_type   = sy-msgty
        i_message_number = sy-msgno
        i_message_v1     = sy-msgv1
        i_message_v2     = sy-msgv2
        i_message_v3     = sy-msgv3
        i_message_v4     = sy-msgv4
        i_probclass      = i_probclass
      receiving
        es_return        = ls_return.
    me->log_bapiret2(
      is_return = ls_return
      i_probclass = i_probclass ).
  endmethod.

  method log_text .
**********************************************************************
* Zweck:     Logging eines Freitexts  Freitext (String).
* Input:     Text als String oder Texttabelle und MessageTyp (E,W,I,..)
*            wenn nicht gesetzt: I = Info
*
**********************************************************************
    data:
      lt_text      type string_table,
      lv_probclass type balprobcl.
    if not i_text is initial.
      append i_text to lt_text.
    elseif not it_text[] is initial.
      lt_text = it_text[].
    endif.
    check not lt_text[] is initial.
    me->add_free_text_to_log(
        it_text        = lt_text
        i_problemclass = i_probclass
        i_message_type = i_msgty ).
  endmethod.

  method log_warning .
**********************************************************************
* Zweck:     Loggen einer Warning Information
* Input:     Exceptionobjekt
* Output:    <...>
* Anmerkung: wird als Warning mit Problemklasse 2 (wichtig)
*            geloggt
**********************************************************************
    me->add_exception_to_log(
        ir_exception   = io_exception
        i_problemclass = '2'
        i_message_type = 'W' ).
  endmethod.

  method get_data.
    call function 'BAL_LOG_READ'
      exporting
        i_log_handle  = me->mv_log_handle
*       I_READ_TEXTS  = ' '
*       I_LANGU       = SY-LANGU
      importing
        es_log        = rs_data-header
        es_statistics = rs_data-statistics
        et_msg        = rs_data-messages
        et_exc        = rs_data-exceptions
      exceptions
        log_not_found = 1
        others        = 2.
    if sy-subrc <> 0.
      zcx_fm_app_error=>raise_from_msg( ).
    endif.
  endmethod.

  method get_log_messages_as_returntab.
    data: ls_ret type bapiret2.
    clear rt_bapiret2.
    loop at me->get_data( )-messages into data(ls_message).
      call function 'BALW_BAPIRETURN_GET2'
        exporting
          type   = ls_message-msgty
          cl     = ls_message-msgid
          number = ls_message-msgno
          par1   = ls_message-msgv1
          par2   = ls_message-msgv2
          par3   = ls_message-msgv3
          par4   = ls_message-msgv4
        importing
          return = ls_ret.
      append ls_ret to rt_bapiret2.
    endloop.
  endmethod.

  method level_down.
    if me->detlevel < 9.
      add 1 to me->detlevel.
    endif.
  endmethod.

  method level_up.
    if me->detlevel > 1.
      subtract 1 from me->detlevel.
    endif.
  endmethod.

  method level_set.
    me->detlevel = i_level.
  endmethod.

  method level_get.
    r_level = me->detlevel.
  endmethod.
endclass.