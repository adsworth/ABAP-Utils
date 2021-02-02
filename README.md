ABAP-Utils
==========

ABAP utilities to make my live easier

LCL_ISU_STRUCTURE_READ.abap
===========================
contains a class that wraps the function module ISU_STRUCTURE_READ.
Using this class you don't need to create loads of local variables when using the function module.

```abap
data: lr_error type ref to lcx_error,
      ls_anlage type v_eanl
      .
try.
    create object gr_structure_read.

    perform get_installation using ls_anlage-anlage. "or whatever you need.

    gr_structure_read->add_eanl( ls_anlage ).

    gr_structure_read->add_step(
      iv_tabfrom = 'EANL'
      iv_tabto = 'EVER'
    ).
    gr_structure_read->add_step(
      iv_tabfrom = 'EVER'
      iv_tabto = 'FKKVKP'
    ).
    gr_structure_read->add_step(
      iv_tabfrom = 'FKKVKP'
      iv_tabto = 'EKUN'
    ).

    gr_structure_read->add_step(
      iv_tabfrom = 'EANL'
      iv_tabto = 'EVBS'
    ).

    gr_structure_read->read(
      iv_ab = sy-datum
      iv_bis = sy-datum
    ).

    lt_install[] = gr_structure_read->mt_eanl[].
  catch lcx_error into lr_error.
    mac_print_cancel lr_error->msgty lr_error->msgno lr_error->msgid
                     lr_error->msgv1 lr_error->msgv2 lr_error->msgv3 lr_error->msgv4.
endtry.
```
