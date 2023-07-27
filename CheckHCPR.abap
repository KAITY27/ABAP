*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
************************************************************************
* INITIALIZATION
***********************************************************************
TYPES: BEGIN OF ty_output,
          hcpr     TYPE rsohcprnm,
          alias    TYPE rsohcprcolnm,
          calcview TYPE rsohcprcolnm,
          bwobject TYPE rsohcprcolnm,
          prov_str TYPE rsohcprcolnm,
          cvalue   TYPE rsohcprcolnm,
          Tgt_Field TYPE rsdodsobject,
          Src_Field TYPE rsdodsobject,
        END OF ty_output.


data: l_hcpr type RSOHCPRNM,
      ls_output   TYPE ty_output,
      lt_output   TYPE STANDARD TABLE OF ty_output,
      lv_txt_s    TYPE scrtext_s,
      lv_txt_m    TYPE scrtext_m,
      lv_txt_l    TYPE scrtext_l.

data: ip_name type RSINFOPROV.
data: l_t_hana_xref TYPE CL_RSO_BW_HANA_OBJXREF=>NT_T_HANA_XREF.
data: l_t_hcpr_xref TYPE RSO_T_HCPR_XREF.
data: l_t_hcpr      Type RSO_T_HCPR.
data: l_s_hcpr      type RSOHCPR.

data: l_t_hana_xref2 TYPE CL_RSO_BW_HANA_OBJXREF=>NT_T_HANA_XREF.
data: l_t_hcpr_xref2 TYPE RSO_T_HCPR_XREF.
data: l_t_XML        Type STANDARD TABLE OF SMUM_XMLTB.
data: l_t_XML_LU     Type STANDARD TABLE OF SMUM_XMLTB.
data: l_t_return     Type STANDARD TABLE OF BAPIRET2.
data: l_cvalue       TYPE rsohcprcolnm.

DATA: lo_alv     TYPE REF TO cl_salv_table,
      lo_columns TYPE REF TO cl_salv_columns,
      lo_column  TYPE REF TO cl_salv_column,
      lo_funcs   TYPE REF TO cl_salv_functions,
      lo_msg      TYPE REF TO cx_root.

DATA: patt TYPE string,
      text TYPE string,
      off  TYPE i,
      moff TYPE i,
      mlen TYPE i.

DATA:  l_package type string,
       l_name    type string,
       l_node    type string,
       l_tabix   type i.


FIELD-SYMBOLS:
    <ls_xref> like line of l_t_hana_xref,
    <ls_hcpr> like LINE OF l_t_hcpr_xref,
    <l_s_hcpr> like LINE OF l_t_hcpr,
    <l_xml>    like LINE OF l_t_XML,
    <l_xml_lu> like LINE OF l_t_XML_lu,
    <ls_xref2> like line of l_t_hana_xref2,
    <ls_hcpr2> like LINE OF l_t_hcpr_xref2,
    <fs_any_tab>  TYPE any.

************************************************************************
* SELECTION SCREEN DETAILS
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_hcpr TYPE RSOHCPRNM OBLIGATORY.
PARAMETERS: p_cont AS CHECKBOX DEFAULT ''.
PARAMETERS: p_refi AS CHECKBOX DEFAULT ''.
PARAMETERS: p_hier AS CHECKBOX DEFAULT ''.
PARAMETERS: p_map AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK blk1.


CALL METHOD CL_RSO_RES_HCPR_DB=>READ_HCPR
  EXPORTING
    I_HCPRNM           = p_hcpr
    I_OBJVERS          = 'A'
  IMPORTING
    E_T_HCPR_XREF      = l_t_hcpr_xref
    E_T_HCPR           = l_t_hcpr
    E_T_HANA_XREF      = l_t_hana_xref.

sort l_t_hcpr_xref
  by objnm tlogo_dep objnm_dep.

delete ADJACENT DUPLICATES FROM l_t_hcpr_xref
  COMPARING
    objnm tlogo_dep objnm_dep.

* Check for hierarchy support of composite provider.
IF p_hier = 'X'.
  loop at l_t_hcpr ASSIGNING <l_s_hcpr>.
      CLEAR ls_output.
      ls_output-hcpr   = <l_s_hcpr>-hcprnm.
      ls_output-prov_str = 'Checking Hierarchy support'.
      APPEND ls_output TO lt_output.
    Loop at cl_rsd_infoprov_cache=>get( <l_s_hcpr>-hcprnm )->n_ts_part assigning field-symbol(<l_s_part>) where svsupphier = rs_c_false.
      CLEAR ls_output.
      ls_output-hcpr   = <l_s_hcpr>-hcprnm.
*     <l_s_part>-partprov  contains provider which do not support hierarchies.
      ls_output-bwobject = <l_s_part>-partprov.
      ls_output-prov_str = 'No Hierarchy support'.
      APPEND ls_output TO lt_output.
    Endloop.
  Endloop.
ENDIF.



*  Execute referential integrity check.
IF p_refi = 'X'.
  loop at l_t_hcpr ASSIGNING <l_s_hcpr>.
    CLEAR ls_output.
    ls_output-hcpr   = <l_s_hcpr>-hcprnm.
    ls_output-prov_str = 'Checking Referential Integrity'.
    APPEND ls_output TO lt_output.
    IF <l_s_hcpr>-XML_UI IS NOT INITIAL.

      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          XML_INPUT       = <l_s_hcpr>-XML_UI
        TABLES
          XML_TABLE       = l_t_XML
          RETURN          = l_t_return.
    ENDIF.
*   Get the top union node to check for ref. integrety.
    LOOP at l_t_xml ASSIGNING <l_xml>
      WHERE CNAME = 'defaultNode'.
        l_node = <l_xml>-cvalue+4(10). "This is the top node.
        l_tabix = sy-tabix + 1.
    ENDLOOP.
    LOOP at l_t_xml from l_tabix ASSIGNING <l_xml> WHERE CVALUE = l_node.
      l_tabix = sy-tabix.
    ENDLOOP.
    CLEAR l_cvalue.
    LOOP at l_t_xml FROM l_tabix ASSIGNING <l_xml>.
*     WHERE CNAME = 'infoObjectName' or CNAME = 'referentialIntegrity' or CNAME = 'objectType'.
*     Stop loop when next viewnode comes up.
      IF <l_xml>-CNAME = 'viewNode'.
        EXIT.
      ENDIF.
      IF <l_xml>-CNAME = 'infoObjectName'
*         Nav. attribute check
      AND NOT <l_xml>-cvalue cp '*__*'.
        IF l_cvalue is INITIAL.
          l_cvalue = <l_xml>-cvalue.
        ENDIF.
        IF l_cvalue <> <l_xml>-cvalue.
          IF ( ls_output-cvalue = 'CHA' OR ls_output-cvalue = 'UNI' ) and
            ls_output-prov_str = ' '.
            ls_output-prov_str = 'Ref.Int. Not Checked'.
            APPEND ls_output TO lt_output.
            l_cvalue = <l_xml>-cvalue.
          ENDIF.
        ENDIF.
        CLEAR ls_output.
        ls_output-hcpr   = p_hcpr.
        ls_output-bwobject = <l_xml>-CVALUE.
      ELSEIF <l_xml>-CNAME = 'referentialIntegrity'.
        ls_output-prov_str = 'Ref. Integrity'.
      ELSEIF <l_xml>-CNAME = 'objectType'.
        ls_output-cvalue = <l_xml>-CVALUE.
      ENDIF.
    ENDLOOP.
    IF ( ls_output-cvalue = 'CHA' OR ls_output-cvalue = 'UNI' ) and
      ls_output-prov_str = ' '.
      ls_output-prov_str = 'Ref.Int. Not Checked'.
      APPEND ls_output TO lt_output.
    ENDIF.
  ENDLOOP.
ENDIF.

*  Check for field mappings.
IF p_map = 'X'.
  loop at l_t_hcpr ASSIGNING <l_s_hcpr>.
    CLEAR ls_output.
    ls_output-hcpr     = <l_s_hcpr>-hcprnm.
    ls_output-prov_str = 'Field Mappings'.
    APPEND ls_output TO lt_output.
    ls_output-prov_str = ''.
    IF <l_s_hcpr>-XML_UI IS NOT INITIAL.
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          XML_INPUT       = <l_s_hcpr>-XML_UI
        TABLES
          XML_TABLE       = l_t_XML
          RETURN          = l_t_return.
    ENDIF.
    LOOP at l_t_xml ASSIGNING <l_xml>
      WHERE CNAME = 'alias' or CNAME = 'entity' or CNAME = 'sourceName' or CNAME = 'targetName'.
      IF <l_xml>-CNAME = 'alias'.
        ls_output-alias = <l_xml>-cvalue.
        ls_output-bwobject = ''.
        ls_output-calcview = ''.
        ls_output-cvalue = ''.
        ls_output-prov_str = ''.
      ENDIF.
      IF <l_xml>-CNAME = 'entity'.
*         Display the source provider
        l_cvalue = <l_xml>-cvalue.
        FIND '.composite#//' IN l_cvalue.
        IF sy-subrc = 0. "Found"
          REPLACE ALL OCCURRENCES OF '.composite#//' IN l_cvalue WITH ''.
          ls_output-bwobject = l_cvalue.
          ls_output-calcview = ''.
          ls_output-cvalue = ''.
          ls_output-prov_str = ''.
        ELSE.
          FIND '.calculationview#/' IN l_cvalue..
          IF sy-subrc = 0. "Found"
            REPLACE ALL OCCURRENCES OF '.calculationview#/' IN l_cvalue WITH ''.
            ls_output-bwobject = ''.
            FIND ALL OCCURRENCES OF '/' IN l_cvalue match OFFSET off.
            l_package = l_cvalue+0(off).
            REPLACE ALL OCCURRENCES OF '/' in l_package WITH '.'.
            ls_output-prov_str = l_package.
            off = off + 1.
            shift l_cvalue by off PLACES.
            l_name    = l_cvalue.
            ls_output-calcview = l_cvalue.

            CALL METHOD CL_RODPS_HANA_MODEL=>HASH_ODPNAME
              EXPORTING
                I_PACKAGE = l_package
                I_NAME    = l_name
              RECEIVING
                R_ODPNAME = ip_name .
            CONCATENATE '2H' ip_name into ip_name.
            ls_output-cvalue   = ip_name.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <l_xml>-CNAME = 'targetName'.
*         Display the source provider
          ls_output-tgt_Field = <l_xml>-CVALUE.
      ELSEIF <l_xml>-CNAME = 'sourceName'.
          ls_output-src_field = <l_xml>-CVALUE.
          APPEND ls_output TO lt_output.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDIF.

* Partprovider overview in 2 levels.
IF p_cont = 'X'.
  CLEAR ls_output.
  ls_output-hcpr   = p_hcpr.
  ls_output-prov_str = 'Displaying partproviders'.
  APPEND ls_output TO lt_output.


*   Handle Other providers in level 1
  loop at l_t_hcpr_xref ASSIGNING <ls_hcpr>
    WHERE tlogo_dep <> 'IOBJ' and tlogo_dep <> 'AREA' and tlogo_dep <> 'HCPR'.
    CLEAR ls_output.
    ls_output-hcpr   = p_hcpr.
    ls_output-bwobject = <ls_hcpr>-objnm_dep.
    ls_output-alias  = <ls_hcpr>-tlogo_dep.
    APPEND ls_output TO lt_output.
  ENDLOOP.

*   Handle Hana CLV used directly in level 1 HCPR
  loop at l_t_hana_xref ASSIGNING <ls_xref>.
    CALL METHOD CL_RODPS_HANA_MODEL=>HASH_ODPNAME
      EXPORTING
        I_PACKAGE = <ls_xref>-namespace
        I_NAME    = <ls_xref>-object_name
      RECEIVING
        R_ODPNAME = ip_name .
    CONCATENATE '2H' ip_name into ip_name.
    clear ls_output.
    ls_output-hcpr   = p_hcpr.
    ls_output-alias  = 'CALC'.
    ls_output-calcview = <ls_xref>-object_name.
    ls_output-prov_str = <ls_xref>-namespace.
    ls_output-cvalue   = ip_name.
    APPEND ls_output TO lt_output.
  endloop.

*   Handle 2. level Composite providers
  loop at l_t_hcpr_xref ASSIGNING <ls_hcpr>
    WHERE tlogo_dep = 'HCPR'.
      CLEAR ls_output.
      ls_output-hcpr   = p_hcpr.
      ls_output-bwobject = <ls_hcpr>-objnm_dep.
      ls_output-alias  = <ls_hcpr>-tlogo_dep.
      APPEND ls_output TO lt_output.

      l_hcpr = <ls_hcpr>-objnm_dep.
      REFRESH l_t_hana_xref2.
      CALL METHOD CL_RSO_RES_HCPR_DB=>READ_HCPR
        EXPORTING
          I_HCPRNM           = l_hcpr
          I_OBJVERS          = 'A'
        IMPORTING
          E_T_HCPR_XREF      = l_t_hcpr_xref2
          E_T_HANA_XREF      = l_t_hana_xref2.
      sort l_t_hcpr_xref2
            by objnm tlogo_dep objnm_dep.
      delete ADJACENT DUPLICATES FROM l_t_hcpr_xref2
            COMPARING objnm tlogo_dep objnm_dep.
*       Handle Other providers at level 2
      loop at l_t_hcpr_xref2 ASSIGNING <ls_hcpr2>
        WHERE tlogo_dep <> 'IOBJ' and tlogo_dep <> 'AREA' and tlogo_dep <> 'HCPR'.
        CLEAR ls_output.
        ls_output-hcpr   = l_hcpr.
        ls_output-bwobject = <ls_hcpr2>-objnm_dep.
        ls_output-alias  = <ls_hcpr2>-tlogo_dep.
        APPEND ls_output TO lt_output.
      ENDLOOP.


*     Handle Hana CLV used at level 2 HCPR
      loop at l_t_hana_xref2 ASSIGNING <ls_xref2>.
        CALL METHOD CL_RODPS_HANA_MODEL=>HASH_ODPNAME
          EXPORTING
            I_PACKAGE = <ls_xref2>-namespace
            I_NAME    = <ls_xref2>-object_name
          RECEIVING
            R_ODPNAME = ip_name .
        CONCATENATE '2H' ip_name into ip_name.
        clear ls_output.
        ls_output-hcpr   = l_hcpr.
        ls_output-alias  = 'CALC'.
        ls_output-calcview = <ls_xref2>-object_name.
        ls_output-prov_str = <ls_xref2>-namespace.
        ls_output-cvalue   = ip_name.
        APPEND ls_output TO lt_output.
      endloop.
  ENDLOOP.
ENDIF.
**********************************************************************
* Output
**********************************************************************
ASSIGN lt_output TO <fs_any_tab> .
TRY.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lo_alv
    CHANGING
      t_table = <fs_any_tab> ).

  "Set column optimized
  lo_columns = lo_alv->get_columns( ).
  lo_columns->set_optimize( ).

  "Set column title
  DATA(lt_colums) = lo_columns->get( ).
  LOOP AT lo_columns->get( ) ASSIGNING FIELD-SYMBOL(<lv_column>).
    lo_column = lo_columns->get_column( <lv_column>-columnname ).
    lv_txt_s = <lv_column>-columnname.
    lv_txt_m = <lv_column>-columnname.
    lv_txt_l = <lv_column>-columnname.
    lo_column->set_short_text( lv_txt_s ).
    lo_column->set_medium_text( lv_txt_m ).
    lo_column->set_long_text( lv_txt_l ).
  ENDLOOP.

  "Set functions
  lo_funcs = lo_alv->get_functions( ).
  lo_funcs->set_all( ).
  lo_alv->display( ).

  "Error handling
  CATCH cx_salv_msg INTO lo_msg.
   MESSAGE lo_msg TYPE 'E'.
  CATCH cx_salv_not_found INTO lo_msg.
   MESSAGE lo_msg TYPE 'E'.
ENDTRY.
