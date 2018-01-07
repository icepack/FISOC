
PROGRAM FISOC_main

  USE ESMF
  USE FISOC_utils_MOD
  USE FISOC_parent_MOD, ONLY : FISOC_parent_register

  IMPLICIT NONE

  
!------------------------------------------------------------------------------

  ! return code (generated by ESMF) and user return code (generated by user 
  ! code and passed through ESMF) 
  INTEGER :: rc, urc

  ! Timekeeping
  TYPE(ESMF_Clock)        :: FISOC_clock
  LOGICAL                 :: tight_coupling

  TYPE(ESMF_VM)           :: vm
  INTEGER                 :: localPet

  ! A parent gridded component is used to support the hierarchical approach  
  ! of ESMF, but the actual grid and state are dummy properties.  The parent 
  ! merely coordinates the child components (ice, ocean and regridding)
  TYPE(ESMF_GridComp)     :: FISOC_parent 
  TYPE(ESMF_config)       :: FISOC_config
  TYPE(ESMF_state)        :: importstate, exportstate


  ! initialize ESMF framework
  CALL ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
       defaultlogfilename="FISOC.Log", &
       logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) THEN
     CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  END IF

  CALL ESMF_LogSet(flush=.TRUE., rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  msg = "Initialised ESMF framework"  
  CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! Note: we are only checking log writing for success on first call to ESMF_LogWrite
  ! possibly this is naively optimistic...

  ! Load configuration file
  FISOC_config = ESMF_ConfigCreate(rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  CALL ESMF_ConfigLoadFile(FISOC_config, "./FISOC_config.rc", rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  msg = "Loaded FISOC configuration file"  
  CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)
  

  ! Create the parent Gridded Component (though we don't use its grid, we just 
  ! use it for coordinating child components...)
  FISOC_parent = ESMF_GridCompCreate(name="FISOC parent gridded component", config=FISOC_config, &
       contextflag=ESMF_CONTEXT_PARENT_VM,rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Register parent (parent registers run, init and fin routines, and child comps) 
  CALL ESMF_GridCompSetServices(FISOC_parent, FISOC_parent_register, &
       userRc=urc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  msg = "Completed FISOC_parent creation and registration"  
  CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)
  

!put all this into thingsToDo/manual once it settles a bit
!*** what calendar? 360 day?
  !------------------------------------------------------------------------------
  ! FISOC time model 
  ! 
  ! FISOC runs one clock which increments with the ocean timestep. The ice 
  ! timestep must be a multiple of the ocean timestep.  An ocean alarm is used 
  ! to indicate whether the ocean model should be run.  The alarm is event driven 
  ! and is set to always on for tightly coupled simulations.  An ice alarm is 
  ! used to indicate whether the ice model should be run.  This is a periodic 
  ! alarm.
  !
  ! ***we could also have a clock in each component and do some checks that the 
  ! clocks are on the same time?
  !
  ! ***loose coupling needs more thought - logical switch sent to FISOC_proc state?
  !
  ! FISOC time related variables:
  ! FISOC_clock          the main FISOC clock
  ! alarm_ISM            the (periodic) alarm for the ice model 
  ! alarm_OM            the (event driven) alarm for the ocean model
  ! tight_coupling (config) whether tight coupling (always call ocean) or loose 
  !                      coupling (call ocean after large geometry change) 
  ! OM_dt_sec (config)   ocean timestep in seconds
  ! dt_ratio   (config)  timestep ratio 
  ! ISM_dt_sec           ice timestep in seconds  (=OM_dt_sec*dt_ratio)
  ! OM_dt                ocean timestep in ESMF format
  ! ISM_dt               ice timestep in ESMF format
  ! start_year  (config) start and end dates for FISOC simulation
  ! start_month (config) ...
  ! end_year    (config) ...
  ! end_month   (config) ...
  !
  ! variables marked (config) are set in the FISOC_config file.
  !
  CALL FISOC_setClocks(FISOC_config, FISOC_clock, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------------
  ! The main bit: initialize, run and finalize FISOC_parent (the parent calls 
  ! all child components).
  importState = ESMF_StateCreate(name='parent import state', stateintent=ESMF_STATEINTENT_IMPORT, rc=rc) 
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  exportState = ESMF_StateCreate(name='parent export state', stateintent=ESMF_STATEINTENT_IMPORT, rc=rc) 
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_GridCompInitialize(FISOC_parent, &
       importState=importState, exportState=exportState, &
       clock=FISOC_clock, userRc=urc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  CALL ESMF_GridCompRun(FISOC_parent, &
       importState=importState, exportState=exportState, &
       clock=FISOC_clock, userRc=urc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------------
  CALL ESMF_GridCompGet(FISOC_parent, vm=vm, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------------
  msg = "FISOC run complete, tidying up..."  
  CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)

  CALL ESMF_GridCompFinalize(FISOC_parent, &
       importState=importState, exportState=exportState, &
       clock=FISOC_clock, userRc=urc, rc=rc)
  IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) &
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  msg = "FISOC finished"  
  CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)

  CALL ESMF_VMBarrier(vm, rc=rc)
  CALL ESMF_Finalize()
  

END PROGRAM FISOC_main
!------------------------------------------------------------------------------
