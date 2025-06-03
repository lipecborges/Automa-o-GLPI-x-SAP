class ZCL_GLPI_API definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF json_ticket_validated,
             id             TYPE n LENGTH 10,
             id_answer      TYPE n LENGTH 10,
             id_requester   TYPE n LENGTH 10,
             name_requester TYPE char100,
    END OF json_ticket_validated .
  types:
    BEGIN OF json_adiciona_acompanhamento,
             content TYPE string,
             error   TYPE string,
             type    TYPE string,
             alert   TYPE string,
             solve   TYPE abap_bool,
             close   TYPE abap_bool,
    END OF json_adiciona_acompanhamento .

  constants RESPONSE_OK type INT4 value 200 ##NO_TEXT.
  constants RESPONSE_404 type INT4 value 404 ##NO_TEXT.
  constants DESTINATION_API type C LENGTH 32 value 'API_GLPI' ##NO_TEXT.

  methods GET
    importing
      !ENDPOINT type STRING
    returning
      value(RESPONSE) type STRING
    raising
      ZCX_API_ERROR .
  methods POST
    importing
      !ENDPOINT type STRING
      !DATA type ANY
    returning
      value(RESPONSE) type STRING
    raising
      ZCX_API_ERROR .
protected section.
private section.

  methods PUT
    importing
      !ENDPOINT type STRING
      !DATA type ref to DATA
    returning
      value(RESPONSE) type ref to DATA .
ENDCLASS.



CLASS ZCL_GLPI_API IMPLEMENTATION.


  METHOD get.

    DATA: lo_http_client TYPE REF TO if_http_client,
          ls_response    TYPE string.

    " Variáveis Locais
    DATA: lv_status_code      TYPE i,
          lv_status_text      TYPE string,
          lv_answers_endpoint TYPE string.

    " Tabelas Locais
    DATA: lt_ticket  TYPE TABLE OF json_ticket_validated,
          lt_answers TYPE TABLE OF string,
          ls_answers TYPE string.

    DATA: lc_answers TYPE string VALUE '/answers'.

    IF endpoint IS INITIAL.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = 'Endpoint não pode estar vázio.'.
      RETURN.
    ENDIF.

    " Cria o client HTTP usando o destino configurado na SM59
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = destination_api
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = 'Erro ao criar cliente HTTP.'.
      RETURN.
    ENDIF.

    lo_http_client->request->set_method( if_http_request=>co_request_method_get ).

    " Define o endpoint, que foi passado na chamada do método.
    lo_http_client->request->set_header_field(
      name  = '~request_uri'
      value = endpoint
    ).

    " Enviar requisição para a API
    lo_http_client->send( ).

    " Recebe a resposta da API
    lo_http_client->receive( ).

    " Capturando o STATUS da solicitação
    lo_http_client->response->get_status( IMPORTING code = lv_status_code reason = lv_status_text ).

    IF lv_status_code EQ response_ok.
      " Obter a resposta(body) da api.
      ls_response = lo_http_client->response->get_cdata( ).
      IF endpoint CS lc_answers.
        response = ls_response.
      ELSE.
        " Exportando dados para a tabela interna
        cl_fdt_json=>json_to_data( EXPORTING iv_json = ls_response CHANGING ca_data = lt_ticket ).
      ENDIF.

      IF lt_ticket IS NOT INITIAL.
        LOOP AT lt_ticket INTO DATA(ls_ticket).
          DATA(lv_ticket_id) = ls_ticket-id.
          lv_answers_endpoint = |{ lc_answers }/{ lv_ticket_id }|.

          " Chama o próprio método com endpoint de de answers.
          ls_answers = me->get( lv_answers_endpoint ).

          response = response && ls_answers.
        ENDLOOP.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = |Erro HTTP: { lv_status_code } - { lv_status_text }|.
      RETURN.
    ENDIF.

    " Fechar Conexão
    lo_http_client->close( ).
    RETURN.

  ENDMETHOD.


  METHOD post.
    DATA: lo_http_client TYPE REF TO if_http_client,
          ls_response    TYPE string.

    " Tabela para mapear a tabela com names UPPERCASE para LOWERCASE.
    DATA: lt_mapped_table TYPE /ui2/cl_json=>name_mappings.

    " Variáveis Locais
    DATA: lv_status_code TYPE i,
          lv_status_text TYPE string,
          lv_json        TYPE string.


    IF endpoint IS INITIAL.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = 'Endpoint não pode estar vázio.'.
      RETURN.
    ENDIF.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = destination_api
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.


    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = 'Erro ao criar cliente HTTP.'.
      RETURN.
    ENDIF.

    lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

    " Define o endpoint, que foi passado na chamada do método.
    lo_http_client->request->set_header_field(
      name  = '~request_uri'
      value = endpoint
    ).

    " Definindo o Content-Type como application/json.
    lo_http_client->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json'
     ).

    " Transforma a tabela recebida no DATA para o formato do JSON
    lv_json = zcl_json_utils=>json_name_map( data ).

    " Anexa o json no cdata(body)
    lo_http_client->request->set_cdata( lv_json ).

    " Enviar requisição para a API
    lo_http_client->send( ).

    " Recebe a resposta da API
    lo_http_client->receive( ).

    " Capturando o STATUS da solicitação
    lo_http_client->response->get_status( IMPORTING code = lv_status_code reason = lv_status_text ).

    IF lv_status_code EQ response_ok.
      " Obter a resposta(body) da api.
      ls_response = lo_http_client->response->get_cdata( ).

      " Exportando dados para a tabela interna
      cl_fdt_json=>json_to_data( EXPORTING iv_json = ls_response CHANGING ca_data = response ).
      RETURN.
    ELSE.
      RAISE EXCEPTION TYPE zcx_api_error EXPORTING message = |Erro HTTP: { lv_status_code } - { lv_status_text }|.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method PUT.
  endmethod.
ENDCLASS.
