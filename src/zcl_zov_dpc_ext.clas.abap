class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
protected section.

  methods SALES_ORDER_HEAD_GET_ENTITY
    redefinition .
  methods SALES_ORDER_HEAD_GET_ENTITYSET
    redefinition .
  methods SALES_ORDER_ITEM_GET_ENTITY
    redefinition .
  methods SALES_ORDER_ITEM_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  METHOD sales_order_head_get_entity.

    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: lv_ordem   TYPE vbeln.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ORDEM'.

    IF sy-subrc EQ 0.
      lv_ordem = ls_key_tab-value.
    ENDIF.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ordem
      IMPORTING
        output = lv_ordem.


    GET TIME STAMP FIELD er_entity-data_criacao.

    SELECT * FROM vbak INTO TABLE @DATA(lt_vbak) WHERE vbeln = @lv_ordem.

    READ TABLE lt_vbak INTO DATA(ls_vbak) INDEX 1.

    er_entity-ordem         = ls_vbak-vbeln.
    er_entity-criado_por    = ls_vbak-ernam.
    er_entity-emissor_ordem = ls_vbak-kunnr.
    er_entity-vendedor      = ls_vbak-vkgrp.
    er_entity-org_vendas    = ls_vbak-vkorg.
    er_entity-tipo_doc      = ls_vbak-auart.

  ENDMETHOD.


  METHOD sales_order_head_get_entityset.

    DATA: lv_one_year_ago TYPE datum.

    lv_one_year_ago = sy-datum - 365 - 365.

    SELECT vbak~vbeln,
           vbak~vkbur,
           vbak~ernam,
           vbak~kunnr,
           vbak~auart,
           vbak~erdat,
           vbak~erzet,
           vbkd~bstkd,
           kna1~name1
      FROM vbak
      INNER JOIN kna1 ON vbak~kunnr = kna1~kunnr
      INNER JOIN vbkd ON vbak~vbeln = vbkd~vbeln
      INTO TABLE @DATA(lt_ordemvenda)
      WHERE vbak~erdat > @lv_one_year_ago.

    LOOP AT lt_ordemvenda INTO DATA(ls_ordemvenda).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>).
      <fs_entityset>-ordem         = ls_ordemvenda-vbeln.
      <fs_entityset>-centro        = ls_ordemvenda-vkbur.
      <fs_entityset>-criado_por    = ls_ordemvenda-ernam.
      <fs_entityset>-emissor_ordem = ls_ordemvenda-kunnr.
      <fs_entityset>-tipo_doc      = ls_ordemvenda-auart.
      <fs_entityset>-nome_emissor  = ls_ordemvenda-name1.
      <fs_entityset>-pedido        = ls_ordemvenda-bstkd.
      CONVERT DATE ls_ordemvenda-erdat
              TIME ls_ordemvenda-erzet
         INTO TIME STAMP <fs_entityset>-data_criacao
         TIME ZONE sy-zonlo.


    ENDLOOP.

  ENDMETHOD.


  METHOD sales_order_item_get_entity.


  ENDMETHOD.


  METHOD sales_order_item_get_entityset.

    DATA: lv_ordem        TYPE vbeln,
          es_entityset    LIKE LINE OF et_entityset,
          lr_ordem_range  TYPE RANGE OF vbeln,
          lv_one_year_ago TYPE datum,
          lt_vbap         TYPE TABLE OF vbap.

    lv_one_year_ago = sy-datum - 730.

    READ TABLE it_key_tab INTO DATA(ls_key_tab) WITH KEY name = 'ORDEM'.

    IF sy-subrc EQ 0.
      lv_ordem = ls_key_tab-value.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ordem
      IMPORTING
        output = lv_ordem.

    IF lv_ordem IS NOT INITIAL.
      SELECT * FROM vbap INTO TABLE lt_vbap WHERE vbeln = lv_ordem.
    ELSE.
      SELECT * FROM vbap INTO TABLE lt_vbap WHERE erdat > lv_one_year_ago.
    ENDIF.

    IF lt_vbap IS NOT INITIAL.
      LOOP AT lt_vbap INTO DATA(ls_vbap).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>).

        <fs_entityset>-ordem = lv_ordem.
        <fs_entityset>-item  = ls_vbap-posnr.
        <fs_entityset>-material = ls_vbap-matnr.
        <fs_entityset>-quantidade = ls_vbap-kwmeng.
        <fs_entityset>-centro = ls_vbap-werks.
        <fs_entityset>-lote = ls_vbap-charg.
        <fs_entityset>-deposito = ls_vbap-lgort.
        <fs_entityset>-unidade_basica = ls_vbap-meins.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
