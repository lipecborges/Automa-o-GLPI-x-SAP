*&---------------------------------------------------------------------*
*& Report ZGLPI_CONTROLA_JOBS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zglpi_controla_jobs.

DATA: lv_intervalo_char TYPE char20,
      lv_intervalo      TYPE i,
      lv_seconds        TYPE i,
      lv_timestamp      TYPE timestampl,
      lv_time           TYPE syuzeit.

SELECT SINGLE low INTO lv_intervalo_char FROM tvarvc
 WHERE name = 'ZGLPI_INTERVALO_JOB'.

IF lv_intervalo_char IS INITIAL.
  lv_intervalo = 30.
ELSE.
  lv_intervalo = lv_intervalo_char.
ENDIF.

GET TIME STAMP FIELD lv_timestamp.

CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo
  INTO DATE DATA(lv_date) TIME lv_time.

lv_seconds = lv_time+4(2).

IF lv_seconds < lv_intervalo.
  PERFORM executa_jobs.
ELSE.
  WAIT UP TO ( 60 - lv_intervalo ) SECONDS.
  PERFORM executa_jobs.
ENDIF.

FORM executa_jobs.

  DATA: lv_repeticoes TYPE i,
        lv_index      TYPE i,
        lv_job1 TYPE tbtcjob-jobname,
        lv_job2 TYPE tbtcjob-jobname,
        lv_job3 TYPE tbtcjob-jobname.

  lv_repeticoes = 60 / lv_intervalo.

  DO lv_repeticoes TIMES.

    lv_index = sy-index.

    lv_job1 = |ZGLPI_ESTORNA_OP_{ lv_index }|.
    lv_job2 = |ZGLPI_MOD_REG_INFO_{ lv_index }|.
    lv_job3 = |ZGLPI_JOB_CRIAOP_ { lv_index }|.

    PERFORM cria_job_individual USING:
      'ZGLPI_ESTORNA_OP'   lv_job1,
      'ZGLPI_MOD_REG_INFO' lv_job2,
      'ZGLPI_CRIA_OP'      lv_job3.

    " Aguarda o tempo definido na variável
    WAIT UP TO lv_intervalo SECONDS.

  ENDDO.

ENDFORM.


FORM cria_job_individual USING pv_report TYPE progname
                                pv_jobbase TYPE tbtcjob-jobname.

  DATA: lv_jobname   TYPE tbtcjob-jobname,
        lv_jobnumber TYPE tbtcjob-jobcount.

  lv_jobname = pv_jobbase.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobnumber
    EXCEPTIONS
      others           = 1.

  IF sy-subrc = 0.

    SUBMIT (pv_report)
      VIA JOB lv_jobname
      NUMBER lv_jobnumber
      AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobname   = lv_jobname
        jobcount  = lv_jobnumber
        strtimmed = 'X'
      EXCEPTIONS
        others    = 1.

  ELSE.
    MESSAGE |Erro ao abrir job para { pv_report }| TYPE 'E'.
  ENDIF.

ENDFORM.
