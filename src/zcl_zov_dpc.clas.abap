class ZCL_ZOV_DPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_DATA
  abstract
  create public .

public section.

  interfaces /IWBEP/IF_SB_DPC_COMM_SERVICES .
  interfaces /IWBEP/IF_SB_GEN_DPC_INJECTION .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_ENTITY
    redefinition .
protected section.

  data mo_injection type ref to /IWBEP/IF_SB_GEN_DPC_INJECTION .

  methods MENSAGEMSET_CREATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_MENSAGEM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MENSAGEMSET_DELETE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_D optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MENSAGEMSET_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_MENSAGEM
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MENSAGEMSET_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZOV_MPC=>TT_MENSAGEM
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MENSAGEMSET_UPDATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_U optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_MENSAGEM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_HEAD_CREATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_HEADER
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_HEAD_DELETE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_D optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_HEAD_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_HEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_HEAD_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZOV_MPC=>TT_SALES_ORDER_HEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_HEAD_UPDATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_U optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_HEADER
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_ITEM_CREATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_ITEM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_ITEM_DELETE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_D optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_ITEM_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_ITEM
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_ITEM_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZOV_MPC=>TT_SALES_ORDER_ITEM
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods SALES_ORDER_ITEM_UPDATE_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_U optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER optional
    exporting
      !ER_ENTITY type ZCL_ZOV_MPC=>TS_SALES_ORDER_ITEM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .

  methods CHECK_SUBSCRIPTION_AUTHORITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.
*&----------------------------------------------------------------------------------------------*
*&  Include           /IWBEP/DPC_TEMP_CRT_ENTITY_BASE
*&* This class has been generated on 18.07.2025 11:38:26 in client 100
*&*
*&*       WARNING--> NEVER MODIFY THIS CLASS <--WARNING
*&*   If you want to change the DPC implementation, use the
*&*   generated methods inside the DPC provider subclass - ZCL_ZOV_DPC_EXT
*&-----------------------------------------------------------------------------------------------*

 DATA mensagemset_create_entity TYPE zcl_zov_mpc=>ts_mensagem.
 DATA sales_order_head_create_entity TYPE zcl_zov_mpc=>ts_sales_order_header.
 DATA sales_order_item_create_entity TYPE zcl_zov_mpc=>ts_sales_order_item.
 DATA lv_entityset_name TYPE string.

lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  MensagemSet
*-------------------------------------------------------------------------*
     WHEN 'MensagemSet'.
*     Call the entity set generated method
    mensagemset_create_entity(
         EXPORTING iv_entity_name     = iv_entity_name
                   iv_entity_set_name = iv_entity_set_name
                   iv_source_name     = iv_source_name
                   io_data_provider   = io_data_provider
                   it_key_tab         = it_key_tab
                   it_navigation_path = it_navigation_path
                   io_tech_request_context = io_tech_request_context
       	 IMPORTING er_entity          = mensagemset_create_entity
    ).
*     Send specific entity data to the caller interfaces
    copy_data_to_ref(
      EXPORTING
        is_data = mensagemset_create_entity
      CHANGING
        cr_data = er_entity
   ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_HEADERSet
*-------------------------------------------------------------------------*
     WHEN 'SALES_ORDER_HEADERSet'.
*     Call the entity set generated method
    sales_order_head_create_entity(
         EXPORTING iv_entity_name     = iv_entity_name
                   iv_entity_set_name = iv_entity_set_name
                   iv_source_name     = iv_source_name
                   io_data_provider   = io_data_provider
                   it_key_tab         = it_key_tab
                   it_navigation_path = it_navigation_path
                   io_tech_request_context = io_tech_request_context
       	 IMPORTING er_entity          = sales_order_head_create_entity
    ).
*     Send specific entity data to the caller interfaces
    copy_data_to_ref(
      EXPORTING
        is_data = sales_order_head_create_entity
      CHANGING
        cr_data = er_entity
   ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_ITEMSet
*-------------------------------------------------------------------------*
     WHEN 'SALES_ORDER_ITEMSet'.
*     Call the entity set generated method
    sales_order_item_create_entity(
         EXPORTING iv_entity_name     = iv_entity_name
                   iv_entity_set_name = iv_entity_set_name
                   iv_source_name     = iv_source_name
                   io_data_provider   = io_data_provider
                   it_key_tab         = it_key_tab
                   it_navigation_path = it_navigation_path
                   io_tech_request_context = io_tech_request_context
       	 IMPORTING er_entity          = sales_order_item_create_entity
    ).
*     Send specific entity data to the caller interfaces
    copy_data_to_ref(
      EXPORTING
        is_data = sales_order_item_create_entity
      CHANGING
        cr_data = er_entity
   ).

  when others.
    super->/iwbep/if_mgw_appl_srv_runtime~create_entity(
       EXPORTING
         iv_entity_name = iv_entity_name
         iv_entity_set_name = iv_entity_set_name
         iv_source_name = iv_source_name
         io_data_provider   = io_data_provider
         it_key_tab = it_key_tab
         it_navigation_path = it_navigation_path
      IMPORTING
        er_entity = er_entity
  ).
ENDCASE.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~DELETE_ENTITY.
*&----------------------------------------------------------------------------------------------*
*&  Include           /IWBEP/DPC_TEMP_DEL_ENTITY_BASE
*&* This class has been generated on 18.07.2025 11:38:26 in client 100
*&*
*&*       WARNING--> NEVER MODIFY THIS CLASS <--WARNING
*&*   If you want to change the DPC implementation, use the
*&*   generated methods inside the DPC provider subclass - ZCL_ZOV_DPC_EXT
*&-----------------------------------------------------------------------------------------------*

 DATA lv_entityset_name TYPE string.

lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  MensagemSet
*-------------------------------------------------------------------------*
      when 'MensagemSet'.
*     Call the entity set generated method
     mensagemset_delete_entity(
          EXPORTING iv_entity_name     = iv_entity_name
                    iv_entity_set_name = iv_entity_set_name
                    iv_source_name     = iv_source_name
                    it_key_tab         = it_key_tab
                    it_navigation_path = it_navigation_path
                    io_tech_request_context = io_tech_request_context
     ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_HEADERSet
*-------------------------------------------------------------------------*
      when 'SALES_ORDER_HEADERSet'.
*     Call the entity set generated method
     sales_order_head_delete_entity(
          EXPORTING iv_entity_name     = iv_entity_name
                    iv_entity_set_name = iv_entity_set_name
                    iv_source_name     = iv_source_name
                    it_key_tab         = it_key_tab
                    it_navigation_path = it_navigation_path
                    io_tech_request_context = io_tech_request_context
     ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_ITEMSet
*-------------------------------------------------------------------------*
      when 'SALES_ORDER_ITEMSet'.
*     Call the entity set generated method
     sales_order_item_delete_entity(
          EXPORTING iv_entity_name     = iv_entity_name
                    iv_entity_set_name = iv_entity_set_name
                    iv_source_name     = iv_source_name
                    it_key_tab         = it_key_tab
                    it_navigation_path = it_navigation_path
                    io_tech_request_context = io_tech_request_context
     ).

   when others.
     super->/iwbep/if_mgw_appl_srv_runtime~delete_entity(
        EXPORTING
          iv_entity_name = iv_entity_name
          iv_entity_set_name = iv_entity_set_name
          iv_source_name = iv_source_name
          it_key_tab = it_key_tab
          it_navigation_path = it_navigation_path
 ).
 ENDCASE.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY.
*&-----------------------------------------------------------------------------------------------*
*&  Include           /IWBEP/DPC_TEMP_GETENTITY_BASE
*&* This class has been generated  on 18.07.2025 11:38:26 in client 100
*&*
*&*       WARNING--> NEVER MODIFY THIS CLASS <--WARNING
*&*   If you want to change the DPC implementation, use the
*&*   generated methods inside the DPC provider subclass - ZCL_ZOV_DPC_EXT
*&-----------------------------------------------------------------------------------------------*

 DATA mensagemset_get_entity TYPE zcl_zov_mpc=>ts_mensagem.
 DATA sales_order_head_get_entity TYPE zcl_zov_mpc=>ts_sales_order_header.
 DATA sales_order_item_get_entity TYPE zcl_zov_mpc=>ts_sales_order_item.
 DATA lv_entityset_name TYPE string.
 DATA lr_entity TYPE REF TO data.       "#EC NEEDED

lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  MensagemSet
*-------------------------------------------------------------------------*
      WHEN 'MensagemSet'.
*     Call the entity set generated method
          mensagemset_get_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = mensagemset_get_entity
                         es_response_context = es_response_context
          ).

        IF mensagemset_get_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = mensagemset_get_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.
*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_HEADERSet
*-------------------------------------------------------------------------*
      WHEN 'SALES_ORDER_HEADERSet'.
*     Call the entity set generated method
          sales_order_head_get_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = sales_order_head_get_entity
                         es_response_context = es_response_context
          ).

        IF sales_order_head_get_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = sales_order_head_get_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.
*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_ITEMSet
*-------------------------------------------------------------------------*
      WHEN 'SALES_ORDER_ITEMSet'.
*     Call the entity set generated method
          sales_order_item_get_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = sales_order_item_get_entity
                         es_response_context = es_response_context
          ).

        IF sales_order_item_get_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = sales_order_item_get_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.

      WHEN OTHERS.
        super->/iwbep/if_mgw_appl_srv_runtime~get_entity(
           EXPORTING
             iv_entity_name = iv_entity_name
             iv_entity_set_name = iv_entity_set_name
             iv_source_name = iv_source_name
             it_key_tab = it_key_tab
             it_navigation_path = it_navigation_path
          IMPORTING
            er_entity = er_entity
    ).
 ENDCASE.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.
*&----------------------------------------------------------------------------------------------*
*&  Include           /IWBEP/DPC_TMP_ENTITYSET_BASE
*&* This class has been generated on 18.07.2025 11:38:26 in client 100
*&*
*&*       WARNING--> NEVER MODIFY THIS CLASS <--WARNING
*&*   If you want to change the DPC implementation, use the
*&*   generated methods inside the DPC provider subclass - ZCL_ZOV_DPC_EXT
*&-----------------------------------------------------------------------------------------------*
 DATA mensagemset_get_entityset TYPE zcl_zov_mpc=>tt_mensagem.
 DATA sales_order_head_get_entityset TYPE zcl_zov_mpc=>tt_sales_order_header.
 DATA sales_order_item_get_entityset TYPE zcl_zov_mpc=>tt_sales_order_item.
 DATA lv_entityset_name TYPE string.

lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  MensagemSet
*-------------------------------------------------------------------------*
   WHEN 'MensagemSet'.
*     Call the entity set generated method
      mensagemset_get_entityset(
        EXPORTING
         iv_entity_name = iv_entity_name
         iv_entity_set_name = iv_entity_set_name
         iv_source_name = iv_source_name
         it_filter_select_options = it_filter_select_options
         it_order = it_order
         is_paging = is_paging
         it_navigation_path = it_navigation_path
         it_key_tab = it_key_tab
         iv_filter_string = iv_filter_string
         iv_search_string = iv_search_string
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset = mensagemset_get_entityset
         es_response_context = es_response_context
       ).
*     Send specific entity data to the caller interface
      copy_data_to_ref(
        EXPORTING
          is_data = mensagemset_get_entityset
        CHANGING
          cr_data = er_entityset
      ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_HEADERSet
*-------------------------------------------------------------------------*
   WHEN 'SALES_ORDER_HEADERSet'.
*     Call the entity set generated method
      sales_order_head_get_entityset(
        EXPORTING
         iv_entity_name = iv_entity_name
         iv_entity_set_name = iv_entity_set_name
         iv_source_name = iv_source_name
         it_filter_select_options = it_filter_select_options
         it_order = it_order
         is_paging = is_paging
         it_navigation_path = it_navigation_path
         it_key_tab = it_key_tab
         iv_filter_string = iv_filter_string
         iv_search_string = iv_search_string
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset = sales_order_head_get_entityset
         es_response_context = es_response_context
       ).
*     Send specific entity data to the caller interface
      copy_data_to_ref(
        EXPORTING
          is_data = sales_order_head_get_entityset
        CHANGING
          cr_data = er_entityset
      ).

*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_ITEMSet
*-------------------------------------------------------------------------*
   WHEN 'SALES_ORDER_ITEMSet'.
*     Call the entity set generated method
      sales_order_item_get_entityset(
        EXPORTING
         iv_entity_name = iv_entity_name
         iv_entity_set_name = iv_entity_set_name
         iv_source_name = iv_source_name
         it_filter_select_options = it_filter_select_options
         it_order = it_order
         is_paging = is_paging
         it_navigation_path = it_navigation_path
         it_key_tab = it_key_tab
         iv_filter_string = iv_filter_string
         iv_search_string = iv_search_string
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset = sales_order_item_get_entityset
         es_response_context = es_response_context
       ).
*     Send specific entity data to the caller interface
      copy_data_to_ref(
        EXPORTING
          is_data = sales_order_item_get_entityset
        CHANGING
          cr_data = er_entityset
      ).

    WHEN OTHERS.
      super->/iwbep/if_mgw_appl_srv_runtime~get_entityset(
        EXPORTING
          iv_entity_name = iv_entity_name
          iv_entity_set_name = iv_entity_set_name
          iv_source_name = iv_source_name
          it_filter_select_options = it_filter_select_options
          it_order = it_order
          is_paging = is_paging
          it_navigation_path = it_navigation_path
          it_key_tab = it_key_tab
          iv_filter_string = iv_filter_string
          iv_search_string = iv_search_string
          io_tech_request_context = io_tech_request_context
       IMPORTING
         er_entityset = er_entityset ).
 ENDCASE.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_ENTITY.
*&----------------------------------------------------------------------------------------------*
*&  Include           /IWBEP/DPC_TEMP_UPD_ENTITY_BASE
*&* This class has been generated on 18.07.2025 11:38:26 in client 100
*&*
*&*       WARNING--> NEVER MODIFY THIS CLASS <--WARNING
*&*   If you want to change the DPC implementation, use the
*&*   generated methods inside the DPC provider subclass - ZCL_ZOV_DPC_EXT
*&-----------------------------------------------------------------------------------------------*

 DATA mensagemset_update_entity TYPE zcl_zov_mpc=>ts_mensagem.
 DATA sales_order_head_update_entity TYPE zcl_zov_mpc=>ts_sales_order_header.
 DATA sales_order_item_update_entity TYPE zcl_zov_mpc=>ts_sales_order_item.
 DATA lv_entityset_name TYPE string.
 DATA lr_entity TYPE REF TO data. "#EC NEEDED

lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  MensagemSet
*-------------------------------------------------------------------------*
      WHEN 'MensagemSet'.
*     Call the entity set generated method
          mensagemset_update_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         io_data_provider   = io_data_provider
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = mensagemset_update_entity
          ).
       IF mensagemset_update_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = mensagemset_update_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.
*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_HEADERSet
*-------------------------------------------------------------------------*
      WHEN 'SALES_ORDER_HEADERSet'.
*     Call the entity set generated method
          sales_order_head_update_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         io_data_provider   = io_data_provider
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = sales_order_head_update_entity
          ).
       IF sales_order_head_update_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = sales_order_head_update_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.
*-------------------------------------------------------------------------*
*             EntitySet -  SALES_ORDER_ITEMSet
*-------------------------------------------------------------------------*
      WHEN 'SALES_ORDER_ITEMSet'.
*     Call the entity set generated method
          sales_order_item_update_entity(
               EXPORTING iv_entity_name     = iv_entity_name
                         iv_entity_set_name = iv_entity_set_name
                         iv_source_name     = iv_source_name
                         io_data_provider   = io_data_provider
                         it_key_tab         = it_key_tab
                         it_navigation_path = it_navigation_path
                         io_tech_request_context = io_tech_request_context
             	 IMPORTING er_entity          = sales_order_item_update_entity
          ).
       IF sales_order_item_update_entity IS NOT INITIAL.
*     Send specific entity data to the caller interface
          copy_data_to_ref(
            EXPORTING
              is_data = sales_order_item_update_entity
            CHANGING
              cr_data = er_entity
          ).
        ELSE.
*         In case of initial values - unbind the entity reference
          er_entity = lr_entity.
        ENDIF.
      WHEN OTHERS.
        super->/iwbep/if_mgw_appl_srv_runtime~update_entity(
           EXPORTING
             iv_entity_name = iv_entity_name
             iv_entity_set_name = iv_entity_set_name
             iv_source_name = iv_source_name
             io_data_provider   = io_data_provider
             it_key_tab = it_key_tab
             it_navigation_path = it_navigation_path
          IMPORTING
            er_entity = er_entity
    ).
 ENDCASE.
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~COMMIT_WORK.
* Call RFC commit work functionality
DATA lt_message      TYPE bapiret2. "#EC NEEDED
DATA lv_message_text TYPE BAPI_MSG.
DATA lo_logger       TYPE REF TO /iwbep/cl_cos_logger.
DATA lv_subrc        TYPE syst-subrc.

lo_logger = /iwbep/if_mgw_conv_srv_runtime~get_logger( ).

  IF iv_rfc_dest IS INITIAL OR iv_rfc_dest EQ 'NONE'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
      wait   = abap_true
    IMPORTING
      return = lt_message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      DESTINATION iv_rfc_dest
    EXPORTING
      wait                  = abap_true
    IMPORTING
      return                = lt_message
    EXCEPTIONS
      communication_failure = 1000 MESSAGE lv_message_text
      system_failure        = 1001 MESSAGE lv_message_text
      OTHERS                = 1002.

  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    /iwbep/cl_sb_gen_dpc_rt_util=>rfc_exception_handling(
        EXPORTING
          iv_subrc            = lv_subrc
          iv_exp_message_text = lv_message_text
          io_logger           = lo_logger ).
  ENDIF.
  ENDIF.
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~GET_GENERATION_STRATEGY.
* Get generation strategy
  rv_generation_strategy = '1'.
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~LOG_MESSAGE.
* Log message in the application log
DATA lo_logger TYPE REF TO /iwbep/cl_cos_logger.
DATA lv_text TYPE /iwbep/sup_msg_longtext.

  MESSAGE ID iv_msg_id TYPE iv_msg_type NUMBER iv_msg_number
    WITH iv_msg_v1 iv_msg_v2 iv_msg_v3 iv_msg_v4 INTO lv_text.

  lo_logger = mo_context->get_logger( ).
  lo_logger->log_message(
    EXPORTING
     iv_msg_type   = iv_msg_type
     iv_msg_id     = iv_msg_id
     iv_msg_number = iv_msg_number
     iv_msg_text   = lv_text
     iv_msg_v1     = iv_msg_v1
     iv_msg_v2     = iv_msg_v2
     iv_msg_v3     = iv_msg_v3
     iv_msg_v4     = iv_msg_v4
     iv_agent      = 'DPC' ).
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~RFC_EXCEPTION_HANDLING.
* RFC call exception handling
DATA lo_logger  TYPE REF TO /iwbep/cl_cos_logger.

lo_logger = /iwbep/if_mgw_conv_srv_runtime~get_logger( ).

/iwbep/cl_sb_gen_dpc_rt_util=>rfc_exception_handling(
  EXPORTING
    iv_subrc            = iv_subrc
    iv_exp_message_text = iv_exp_message_text
    io_logger           = lo_logger ).
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~RFC_SAVE_LOG.
  DATA lo_logger  TYPE REF TO /iwbep/cl_cos_logger.
  DATA lo_message_container TYPE REF TO /iwbep/if_message_container.

  lo_logger = /iwbep/if_mgw_conv_srv_runtime~get_logger( ).
  lo_message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  " Save the RFC call log in the application log
  /iwbep/cl_sb_gen_dpc_rt_util=>rfc_save_log(
    EXPORTING
      is_return            = is_return
      iv_entity_type       = iv_entity_type
      it_return            = it_return
      it_key_tab           = it_key_tab
      io_logger            = lo_logger
      io_message_container = lo_message_container ).
  endmethod.


  method /IWBEP/IF_SB_DPC_COMM_SERVICES~SET_INJECTION.
* Unit test injection
  IF io_unit IS BOUND.
    mo_injection = io_unit.
  ELSE.
    mo_injection = me.
  ENDIF.
  endmethod.


  method CHECK_SUBSCRIPTION_AUTHORITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'CHECK_SUBSCRIPTION_AUTHORITY'.
  endmethod.


  method MENSAGEMSET_CREATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_CREATE_ENTITY'.
  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_DELETE_ENTITY'.
  endmethod.


  method MENSAGEMSET_GET_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_GET_ENTITY'.
  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_GET_ENTITYSET'.
  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'MENSAGEMSET_UPDATE_ENTITY'.
  endmethod.


  method SALES_ORDER_HEAD_CREATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_HEAD_CREATE_ENTITY'.
  endmethod.


  method SALES_ORDER_HEAD_DELETE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_HEAD_DELETE_ENTITY'.
  endmethod.


  method SALES_ORDER_HEAD_GET_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_HEAD_GET_ENTITY'.
  endmethod.


  method SALES_ORDER_HEAD_GET_ENTITYSET.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_HEAD_GET_ENTITYSET'.
  endmethod.


  method SALES_ORDER_HEAD_UPDATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_HEAD_UPDATE_ENTITY'.
  endmethod.


  method SALES_ORDER_ITEM_CREATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_ITEM_CREATE_ENTITY'.
  endmethod.


  method SALES_ORDER_ITEM_DELETE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_ITEM_DELETE_ENTITY'.
  endmethod.


  method SALES_ORDER_ITEM_GET_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_ITEM_GET_ENTITY'.
  endmethod.


  method SALES_ORDER_ITEM_GET_ENTITYSET.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_ITEM_GET_ENTITYSET'.
  endmethod.


  method SALES_ORDER_ITEM_UPDATE_ENTITY.
  RAISE EXCEPTION TYPE /iwbep/cx_mgw_not_impl_exc
    EXPORTING
      textid = /iwbep/cx_mgw_not_impl_exc=>method_not_implemented
      method = 'SALES_ORDER_ITEM_UPDATE_ENTITY'.
  endmethod.
ENDCLASS.
