
MODULE FISOC_utils_MOD

  USE ESMF
  USE FISOC_types_MOD

  IMPLICIT NONE

  PRIVATE

  PUBLIC  FISOC_getStringListFromConfig, FISOC_populateFieldBundle, FISOC_ConfigDerivedAttribute, &
       FISOC_initCumulatorFB, FISOC_zeroBundle, FISOC_cumulateFB, FISOC_processCumulator, msg,    &
       FISOC_VM_MPI_Comm_dup, FISOC_FieldRegridStore, FISOC_FB2NC, FISOC_setClocks,               & 
       FISOC_destroyClocks, FISOC_ISM2OM, FISOC_OM2ISM, FISOC_OneGrid, FISOC_cavityCheckOptions 

  INTERFACE FISOC_populateFieldBundle
      MODULE PROCEDURE FISOC_populateFieldBundleOn2dGrid
      MODULE PROCEDURE FISOC_populateFieldBundleOnMesh
  END INTERFACE FISOC_populateFieldBundle

  INTERFACE FISOC_ConfigDerivedAttribute
      MODULE PROCEDURE FISOC_ConfigDerivedAttributeLogical
      MODULE PROCEDURE FISOC_ConfigDerivedAttributeInteger
      MODULE PROCEDURE FISOC_ConfigDerivedAttributeString
      MODULE PROCEDURE FISOC_ConfigDerivedAttributeStaggerLocArray
      MODULE PROCEDURE FISOC_ConfigDerivedAttributeRegridMethod
  END INTERFACE 

  CHARACTER(len=ESMF_MAXSTR) :: msg

CONTAINS



  !------------------------------------------------------------------------------
  ! 
  ! check whether too many cavity geometry options are being passed to the OM
  SUBROUTINE FISOC_cavityCheckOptions(FISOC_config,rc)

    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,INTENT(OUT)                   :: rc

    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: ISM2OM_Vars(:)
    CHARACTER(len=ESMF_MAXSTR)            :: label, OM_cavityUpdate
    INTEGER                               :: ii, count
    LOGICAL                               :: cavityUpdateMismatch

    rc = ESMF_FAILURE

    cavityUpdateMismatch = .FALSE.

    label = 'ISM2OM_vars:'
    CALL FISOC_getStringListFromConfig(FISOC_config, label, ISM2OM_Vars,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) THEN
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    CALL ESMF_ConfigGetAttribute(FISOC_config, OM_cavityUpdate, label='OM_cavityUpdate:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    count = 0
    DO ii=1,SIZE(ISM2OM_Vars)

          IF (TRIM('ISM_z_l0').EQ.TRIM(ISM2OM_Vars(ii))) THEN
             IF (OM_cavityUpdate.NE.'RecentIce') cavityUpdateMismatch = .TRUE.
             count = count + 1
          END IF
          
          IF (TRIM('ISM_z_l0_linterp').EQ.TRIM(ISM2OM_Vars(ii))) THEN
             IF (OM_cavityUpdate.NE.'Linterp') cavityUpdateMismatch = .TRUE.
             count = count + 1
          END IF

          IF (TRIM('ISM_dddt').EQ.TRIM(ISM2OM_Vars(ii))) THEN
             IF ( (OM_cavityUpdate.NE.'Rate')          &
                  .AND.                                &
                  (OM_cavityUpdate.NE.'CorrectedRate') &
                  ) THEN
                cavityUpdateMismatch = .TRUE.
                count = count + 1
             END IF
          END IF
          
    END DO

    IF (count.eq.0) THEN
       msg = 'no ISM cavity variable will be passed to the OM'
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
            line=__LINE__, file=__FILE__, rc=rc)          
    END IF

    IF (count.gt.1) THEN
       msg = 'only 1 ISM cavity variable should be passed to the OM'
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    IF (cavityUpdateMismatch) THEN
       msg = 'Cavity update mismatch (check ISM2OM vars) for OM_cavityUpdate: '//OM_cavityUpdate
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_cavityCheckOptions



  !------------------------------------------------------------------------------
  ! 
  ! check whether user wants this variable (fieldname) to be passed from the ISM 
  ! to the OM.
  LOGICAL FUNCTION FISOC_ISM2OM(fieldName,FISOC_config,rc)

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: fieldName
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: ISM2OM_Vars(:)
    CHARACTER(len=ESMF_MAXSTR)            :: label
    INTEGER                               :: ii

    rc = ESMF_FAILURE

    label = 'ISM2OM_vars:'
    CALL FISOC_getStringListFromConfig(FISOC_config, label, ISM2OM_Vars,rc=rc)
    IF (rc.EQ.ESMF_RC_NOT_FOUND) THEN
       msg = "ISM2OM_vars not found in FISOC config file, trying to pass all available vars"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
            line=__LINE__, file=__FILE__, rc=rc)          
       FISOC_ISM2OM = .TRUE. ! pass all vars if list is not present 
       RETURN
    ELSE IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) THEN
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    FISOC_ISM2OM = FISOC_listContains(fieldName,ISM2OM_Vars,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    rc = ESMF_SUCCESS

    RETURN

  END FUNCTION  FISOC_ISM2OM


  !------------------------------------------------------------------------------
  ! 
  ! check whether user wants this variable (fieldname) to be passed from the OM 
  ! to the ISM.
  ! ***TODO:resolve code duplication between this and ism2omroutine
  LOGICAL FUNCTION FISOC_OM2ISM(fieldName,FISOC_config,rc)

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: fieldName
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: OM2ISM_Vars(:)
    CHARACTER(len=ESMF_MAXSTR)            :: label
    INTEGER                               :: ii

    rc = ESMF_FAILURE

    label = 'OM2ISM_vars:'
    CALL FISOC_getStringListFromConfig(FISOC_config, label, OM2ISM_Vars,rc=rc)
    IF (rc.EQ.ESMF_RC_NOT_FOUND) THEN
       msg = "OM2ISM_vars not found in FISOC config file, trying to pass all available vars"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
            line=__LINE__, file=__FILE__, rc=rc)          
       FISOC_OM2ISM = .TRUE. ! pass all vars if list is not present 
       RETURN
    ELSE IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) THEN
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    FISOC_OM2ISM = FISOC_listContains(fieldName,OM2ISM_Vars,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    rc = ESMF_SUCCESS

    RETURN

  END FUNCTION  FISOC_OM2ISM



  !------------------------------------------------------------------------------
  LOGICAL FUNCTION FISOC_listContains(itemName,list,rc)

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: itemName, list(:)
    INTEGER,INTENT(OUT),OPTIONAL          :: rc
    INTEGER                               :: ii

    rc = ESMF_FAILURE

    IF (SIZE(list).EQ.0) THEN
       FISOC_listContains = .FALSE. ! return false if list is empty 
    ELSE
       FISOC_listContains = .FALSE.
       DO ii=1,SIZE(list)
          IF (TRIM(itemName).EQ.TRIM(list(ii))) THEN 
             FISOC_listContains = .TRUE.
          END IF
       END DO
    END IF

    rc = ESMF_SUCCESS

    RETURN

  END FUNCTION  FISOC_LISTCONTAINS



  !--------------------------------------------------------------------------------------
  ! use the ESMF VM to access the mpi communicator and return a duplicate
  SUBROUTINE FISOC_VM_MPI_Comm_dup(vm,mpic_dup,rc)

    TYPE(ESMF_VM),INTENT(IN)       :: VM
    INTEGER, INTENT(OUT)           :: mpic_dup
    INTEGER, OPTIONAL, INTENT(OUT) :: rc

    INTEGER                        :: mpic, ierr

    rc = ESMF_FAILURE

    !-------------------------------------------------------------------------------
    ! Get the parallel context, specifically the mpi communicator, for the OM to 
    ! use.
    ! The returned MPI communicator spans the same MPI processes that the VM
    ! is defined on.
    CALL ESMF_VMGet(vm, mpiCommunicator=mpic, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! Duplicate the MPI communicator not to interfere with ESMF communications.
    ! The duplicate MPI communicator can be used in any MPI call in the user
    ! code. 
    ! The ifdef statements are needed here because the MPI_comm_dup call will cause
    ! compilation to fail if the MPI library is not available.
#ifdef FISOC_MPI
    CALL MPI_Comm_dup(mpic, mpic_dup, ierr)
    IF (ierr.NE.0) THEN
       msg = "ERROR: Failed call to MPI_Comm_dup"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
#else
    mpic_dup = FISOC_mpic_missing
#endif

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_VM_MPI_Comm_dup



  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_destroyClocks(FISOC_clock,rc)
    
    TYPE(ESMF_Clock),INTENT(INOUT)     :: FISOC_clock
    INTEGER,OPTIONAL,INTENT(OUT)       :: rc
    
!TODO: get all alarms for this clock and destroy them first
!
!    CALL ESMF_AlarmDestroy(alarm_ISM, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
!    CALL ESMF_AlarmDestroy(alarm_OM, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_ClockDestroy(FISOC_clock, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  END SUBROUTINE FISOC_destroyClocks



  !------------------------------------------------------------------------------
  ! setting up clocks and alarms
  !
  SUBROUTINE FISOC_setClocks(FISOC_config, FISOC_clock, rc)

    TYPE(ESMF_config),INTENT(INOUT)    :: FISOC_config
    TYPE(ESMF_Clock),INTENT(INOUT)     :: FISOC_clock
    INTEGER,OPTIONAL,INTENT(OUT)       :: rc

    INTEGER                 :: ISM_dt_sec, OM_dt_sec, dt_ratio
    INTEGER                 :: start_year, end_year, start_month, end_month
    INTEGER                 :: OM_outputInterval, runLength_ISM_steps, runLength_secs
    TYPE(ESMF_TimeInterval) :: ISM_dt, OM_dt, runLength_timeInterval
    TYPE(ESMF_Time)         :: startTime, endTime
    TYPE(ESMF_Alarm)        :: alarm_OM, alarm_OM_output, alarm_ISM, alarm_ISM_exportAvailable
    LOGICAL                 :: gotRunLength

    CALL ESMF_ConfigGetAttribute(FISOC_config, dt_ratio, label='dt_ratio:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_ConfigGetAttribute(FISOC_config, OM_dt_sec, label='OM_dt_sec:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL FISOC_ConfigDerivedAttribute(FISOC_config, ISM_dt_sec, 'ISM_dt_sec',rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_ConfigGetAttribute(FISOC_config, start_month, label='start_month:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_ConfigGetAttribute(FISOC_config, start_year, label='start_year:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_ConfigGetAttribute(FISOC_config, runLength_ISM_steps, label='runLength_ISM_steps:', rc=rc)
    IF (rc.EQ.ESMF_RC_NOT_FOUND) THEN
       gotRunLength = .FALSE.
    ELSE IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) THEN
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    ELSE
       gotRunLength = .TRUE.
    END IF

    IF (.NOT.gotRunLength) THEN
       CALL ESMF_ConfigGetAttribute(FISOC_config, end_month, label='end_month:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CALL ESMF_ConfigGetAttribute(FISOC_config, end_year, label='end_year:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    CALL ESMF_ConfigGetAttribute(FISOC_config, OM_outputInterval, label='OM_outputInterval:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    IF (MOD(dt_ratio,OM_outputInterval).NE.0) THEN
       msg = "ERROR: dt_ratio/OM_outputInterval is required to be integer"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    CALL ESMF_TimeIntervalSet(OM_dt, s=OM_dt_sec, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_TimeIntervalSet(ISM_dt, s=ISM_dt_sec, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_TimeSet(startTime, yy=start_year, mm=start_month, dd=1, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    IF (gotRunLength) THEN
       runLength_secs = runLength_ISM_steps * ISM_dt_sec
       CALL ESMF_TimeIntervalSet(runLength_timeInterval, s=runLength_secs, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       endTime = startTime + runLength_timeInterval
       msg = "used runLength_ISM_steps to set the end time"  
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)       
    ELSE
       CALL ESMF_TimeSet(endTime, yy=end_year, mm=end_month, dd=1, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    FISOC_clock = ESMF_ClockCreate(OM_dt, startTime, stopTime=endTime, &
         name="FISOC main clock", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    alarm_OM = ESMF_AlarmCreate(clock=FISOC_clock, name="alarm_OM", &
         ringTime=startTime, ringInterval=OM_dt, sticky=.FALSE., rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    alarm_OM_output = ESMF_AlarmCreate(clock=FISOC_clock, name="alarm_OM_output", &
         ringTime=(startTime+(OM_dt*(OM_outputInterval-1))), ringInterval=OM_dt*OM_outputInterval, sticky=.FALSE., rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    alarm_ISM = ESMF_AlarmCreate(clock=FISOC_clock, name="alarm_ISM", &
         ringTime=((startTime+ISM_dt)-OM_dt), ringInterval=ISM_dt, sticky=.FALSE., rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    alarm_ISM_exportAvailable = ESMF_AlarmCreate(clock=FISOC_clock, &
         name="alarm_ISM_exportAvailable", &
         ringTime=(startTime+ISM_dt), ringInterval=ISM_dt, sticky=.FALSE.,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    msg = "created and initialised clocks and alarms"  
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_setClocks




  !--------------------------------------------------------------------------------------
  ! set values of fields in this bundle to zero
  SUBROUTINE FISOC_zeroBundle(fieldBundle,rc)
    
    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: fieldBundle
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc

    TYPE(ESMF_Field),ALLOCATABLE          :: fieldList(:)
    REAL(ESMF_KIND_R8),POINTER            :: field_ptr(:,:)
    INTEGER                               :: fieldCount, ii

    rc = ESMF_FAILURE

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldList(fieldCount))
    CALL ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! loop over fields, setting all values to zero
    DO ii=1,fieldCount
       CALL ESMF_FieldGet(field=fieldList(ii), farrayPtr=field_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       field_ptr(:,:) = 0.0

    END DO

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_zeroBundle

  !--------------------------------------------------------------------------------------
  ! add the values for the field in the bundle to the cumulator.  Later it will be divided 
  ! by the number of cumulation operations to give the average. 
  SUBROUTINE FISOC_cumulateFB(fieldBundle,FBcumulator,rc)

    TYPE(ESMF_fieldbundle),INTENT(IN)     :: fieldBundle
    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: FBcumulator
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc

    INTEGER                               :: fieldCount, ii, fieldCountCum
    REAL(ESMF_KIND_R8)                    :: init_value
    TYPE(ESMF_Field),ALLOCATABLE          :: fieldList(:),fieldListCum(:)
    TYPE(ESMF_TypeKind_Flag)              :: fieldTypeKind
    CHARACTER(len=ESMF_MAXSTR)            :: fieldName, fieldNameCum
    REAL(ESMF_KIND_R8),POINTER            :: fieldCum_ptr(:,:), field_ptr(:,:) 
    TYPE(ESMF_GRID)                       :: grid

    rc = ESMF_FAILURE

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(FBcumulator, fieldCount=fieldCountCum, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldListCum(fieldCountCum))
    CALL ESMF_FieldBundleGet(FBcumulator, fieldList=fieldListCum, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldList(fieldCount))
    CALL ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (fieldCountCum .NE. fieldCount) THEN
       msg = 'ERROR: fieldBundle and cumulator fieldBundle contain different number of fields '
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    DO ii=1,fieldCount

       CALL ESMF_FieldGet(fieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       CALL ESMF_FieldGet(fieldListCum(ii), name=fieldNameCum, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       IF (TRIM(fieldNameCum) .NE. TRIM(fieldName)//"_cum") THEN
          msg = 'ERROR: cumulator field name does not match field name '
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF
    
       CALL ESMF_FieldGet(field=fieldList(ii), localDe=0, farrayPtr=field_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       CALL ESMF_FieldGet(field=fieldListCum(ii), localDe=0, farrayPtr=fieldCum_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       IF (SIZE(field_ptr).NE.SIZE(fieldCum_ptr)) THEN
          msg = 'ERROR: cumulator field array pointer dims do not match '
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF
    
       fieldCum_ptr(:,:) = fieldCum_ptr(:,:) + field_ptr(:,:)

    END DO

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_cumulateFB


  !--------------------------------------------------------------------------------------
  ! Calculate the average from the cumulator, and stick it back in the field bundle. 
  ! We may one day wish to do other time processing such as keeping the total intead of 
  ! averaging (cant think why though...).
  ! Note: there is a lot of code duplication here of FISOC_cumulateFB.  Is there potential 
  ! for some kind of operatore overloading for field bundle operations?  Perhaps this 
  ! already exists in ESMF?
  SUBROUTINE FISOC_processCumulator(fieldBundle,FBcumulator,FISOC_config,rc)

    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: FBcumulator, fieldBundle
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc

!    TYPE(ESMF_field)                      :: cumulatorField
    CHARACTER(len=ESMF_MAXSTR)            :: fieldName, fieldNameCum
    INTEGER                               :: OM_cum_steps, ii, fieldCount, fieldCountCum 
    TYPE(ESMF_field)                      :: fieldCum, field
    REAL(ESMF_KIND_R8),POINTER            :: fieldCum_ptr(:,:), field_ptr(:,:) 
    TYPE(ESMF_Field),ALLOCATABLE          :: fieldList(:),fieldListCum(:)
    TYPE(ESMF_TypeKind_Flag)              :: fieldTypeKind

    rc = ESMF_FAILURE

    ! get number of cumulator steps: ts_ratio / OM_outputInterval
    CALL FISOC_ConfigDerivedAttributeInteger(FISOC_config, OM_cum_steps, 'OM_cum_steps', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(FBcumulator, fieldCount=fieldCountCum, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldListCum(fieldCountCum))
    CALL ESMF_FieldBundleGet(FBcumulator, fieldList=fieldListCum, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldList(fieldCount))
    CALL ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (fieldCountCum .NE. fieldCount) THEN
       msg = 'ERROR: fieldBundle and cumulator fieldBundle contain different number of fields '
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    DO ii=1,fieldCount

       CALL ESMF_FieldGet(fieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       CALL ESMF_FieldGet(fieldListCum(ii), name=fieldNameCum, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       IF (TRIM(fieldNameCum) .NE. TRIM(fieldName)//"_cum") THEN
          msg = 'ERROR: cumulator field name does not match field name '
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF
    
       CALL ESMF_FieldGet(field=fieldList(ii), localDe=0, farrayPtr=field_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       CALL ESMF_FieldGet(field=fieldListCum(ii), localDe=0, farrayPtr=fieldCum_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       IF (SIZE(field_ptr).NE.SIZE(fieldCum_ptr)) THEN
          msg = 'ERROR: cumulator field array pointer dims do not match '
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF
    
       field_ptr(:,:) = fieldCum_ptr(:,:) / OM_cum_steps

    END DO

    CALL FISOC_zeroBundle(FBcumulator,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_processCumulator


  !--------------------------------------------------------------------------------------
  ! populate a field bundle for cumulating outputs using the field info from an 
  ! existing field bundle.
  SUBROUTINE FISOC_initCumulatorFB(fieldBundle,FBcumulator,rc)

    TYPE(ESMF_fieldbundle),INTENT(IN)     :: fieldBundle
    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: FBcumulator
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc

    INTEGER                               :: fieldCount, ii
    REAL(ESMF_KIND_R8)                    :: init_value
    TYPE(ESMF_Field),ALLOCATABLE          :: fieldList(:),fieldListCum(:)
    TYPE(ESMF_TypeKind_Flag)              :: fieldTypeKind
    CHARACTER(len=ESMF_MAXSTR)            :: fieldName, fieldNameCum
    TYPE(ESMF_GRID)                       :: grid

    rc = ESMF_FAILURE

    init_value = 0.0

    ! How many fields in bundle?
    CALL ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(fieldList(fieldCount))
    CALL ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! loop over fields, creating a cumulator for each to add to the cumulator field bundle
    ! (cumulator is just another field in which we will sum the field over time)
    ALLOCATE(fieldListCum(fieldCount))
    DO ii=1,fieldCount

       CALL ESMF_FieldGet(fieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       fieldNameCum = TRIM(fieldName)//"_cum"

       CALL ESMF_FieldGet(fieldList(ii), grid=grid, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
       fieldListCum(ii) = ESMF_FieldCreate(grid, typekind=fieldTypeKind, name=fieldNameCum, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    END DO

    CALL ESMF_FieldBundleAdd(FBcumulator, fieldListCum, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_initCumulatorFB


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ConfigDerivedAttributeInteger(FISOC_config, derivedAttribute, label,rc)
    
    CHARACTER(len=*),INTENT(IN)           :: label
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,INTENT(OUT)                   :: derivedAttribute
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc
    
    INTEGER                               :: OM_dt_sec, dt_ratio, OM_outputInterval

    rc = ESMF_FAILURE

    SELECT CASE(label)

    CASE('ISM_dt_sec')
       CALL ESMF_ConfigGetAttribute(FISOC_config, dt_ratio, label='dt_ratio:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CALL ESMF_ConfigGetAttribute(FISOC_config, OM_dt_sec, label='OM_dt_sec:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       derivedAttribute = OM_dt_sec * dt_ratio
    CASE('OM_cum_steps')
       CALL ESMF_ConfigGetAttribute(FISOC_config, dt_ratio, label='dt_ratio:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CALL ESMF_ConfigGetAttribute(FISOC_config, OM_outputInterval, label='OM_outputInterval:', rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       derivedAttribute = dt_ratio / OM_outputInterval
    CASE DEFAULT
       msg = 'ERROR: unrecognised derived config attribute label '
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ConfigDerivedAttributeInteger


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ConfigDerivedAttributeRegridMethod(FISOC_config, derivedAttribute, label,rc)
    
    CHARACTER(len=*),INTENT(IN)             :: label
    TYPE(ESMF_config),INTENT(INOUT)         :: FISOC_config
    TYPE(ESMF_RegridMethod_Flag),INTENT(OUT):: derivedAttribute
    INTEGER,OPTIONAL,INTENT(OUT)            :: rc
    
    CHARACTER(len=ESMF_MAXSTR)              :: regridMethodChar
    INTEGER                                 :: rc_local

    rc = ESMF_FAILURE

    SELECT CASE(label)

    CASE('Regrid_method')
       CALL ESMF_ConfigGetAttribute(FISOC_config, regridMethodChar, label='Regrid_method:', rc=rc_local)
       IF (rc_local.EQ.ESMF_RC_NOT_FOUND) THEN
          regridMethodChar = "ESMF_REGRIDMETHOD_BILINEAR"
          msg = "WARNING: Regrid_method not found, setting to bilinear."
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
               line=__LINE__, file=__FILE__)
       ELSE
          IF (ESMF_LogFoundError(rcToCheck=rc_local, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF

       SELECT CASE (regridMethodChar)
       CASE("ESMF_REGRIDMETHOD_BILINEAR")
          derivedAttribute = ESMF_REGRIDMETHOD_BILINEAR
       CASE("ESMF_REGRIDMETHOD_NEAREST_DTOS")
          derivedAttribute = ESMF_REGRIDMETHOD_NEAREST_DTOS
       CASE("ESMF_REGRIDMETHOD_NEAREST_STOD")
          derivedAttribute = ESMF_REGRIDMETHOD_NEAREST_STOD
       CASE DEFAULT
          msg = 'ERROR: regrid method NYI in FISOC: '//regridMethodChar
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)          
       END SELECT

    CASE DEFAULT
       msg = 'ERROR: unrecognised derived config attribute label: '//label
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ConfigDerivedAttributeRegridMethod

  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ConfigDerivedAttributeLogical(FISOC_config, derivedAttribute, label,rc)
    
    CHARACTER(len=*),INTENT(IN)           :: label
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    LOGICAL,INTENT(OUT)                   :: derivedAttribute
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc

    INTEGER :: rc_local
    
    rc = ESMF_FAILURE

    SELECT CASE(label)

    CASE('ISM2OM_init_vars')
       CALL ESMF_ConfigGetAttribute(FISOC_config, derivedAttribute, label='ISM2OM_init_vars:', rc=rc_local)
       IF (rc_local.EQ.ESMF_RC_NOT_FOUND) THEN
          derivedAttribute = .TRUE.
          msg = "WARNING: ISM2OM_init_vars not found, setting to .TRUE."
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
               line=__LINE__, file=__FILE__)
       ELSE
          IF (ESMF_LogFoundError(rcToCheck=rc_local, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END IF

    CASE DEFAULT
       msg = 'ERROR: unrecognised derived config attribute label: '//label
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ConfigDerivedAttributeLogical


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ConfigDerivedAttributeString(FISOC_config, derivedAttribute, label,rc)
    
    CHARACTER(len=*),INTENT(IN)           :: label
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    CHARACTER(len=ESMF_MAXSTR),INTENT(OUT):: derivedAttribute
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc
    
    CHARACTER(len=ESMF_MAXSTR)            :: OM_cavityUpdate

    rc = ESMF_FAILURE

    SELECT CASE(label)

    CASE("IceDraft")
       CALL ESMF_ConfigGetAttribute(FISOC_config, OM_cavityUpdate, label='OM_cavityUpdate:', rc=rc)
print*,'catch error and set default if missing att'
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!          derivedAttribute = 'draft'
       SELECT CASE(OM_cavityUpdate)
       CASE('RecentIce','Linterp')
          derivedAttribute = 'actual'
       CASE('Rate', 'CorrectedRate')
          derivedAttribute = 'rate'
       CASE DEFAULT
       END SELECT
 
    CASE DEFAULT
       msg = 'ERROR: unrecognised derived config attribute label '
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ConfigDerivedAttributeString


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ConfigDerivedAttributeStaggerLocArray(FISOC_config, derivedAttribute, label, rc)
    
    CHARACTER(len=*),INTENT(IN)           :: label
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_staggerLoc),INTENT(INOUT)   :: derivedAttribute(:)
    INTEGER,OPTIONAL,INTENT(OUT)          :: rc
    
    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: attribute_stringList(:)
    INTEGER                               :: OM_dt_sec, dt_ratio, OM_outputInterval

    rc = ESMF_FAILURE

    SELECT CASE(label)

    CASE('OM_ReqVars_stagger:')
       CALL FISOC_getStringListFromConfig(FISOC_config, label, attribute_stringList,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CALL FISOC_OM_staggerCodes(derivedAttribute,attribute_stringList,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    CASE DEFAULT
       msg = 'ERROR: unrecognised derived config attribute label '
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ConfigDerivedAttributeStaggerLocArray


  !--------------------------------------------------------------------------------------
  ! convert a list of strings describning stagger location to ESMF stagger location 
  ! integer codes (for use in ESMF operations such as creating fields on a grid).
  SUBROUTINE FISOC_OM_staggerCodes(staggerLoc,staggerChar,rc)
    
    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE,INTENT(IN) :: staggerChar(:)
    TYPE(ESMF_staggerLoc),INTENT(OUT)                 :: staggerLoc(:)
    INTEGER,INTENT(OUT),OPTIONAL                      :: rc

    INTEGER                                           :: ii

    rc = ESMF_FAILURE
    
    IF (size(staggerLoc) .ne. size(staggerChar)) THEN
       msg = 'ERROR: stagger lists must be same length'
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
       
    DO ii = 1,size(staggerLoc)
       SELECT CASE(staggerChar(ii))
       CASE("EDGE1")
          staggerLoc(ii) = ESMF_STAGGERLOC_EDGE1
       CASE("EDGE2")
          staggerLoc(ii) = ESMF_STAGGERLOC_EDGE2
       CASE("CENTER")
          staggerLoc(ii) = ESMF_STAGGERLOC_CENTER
       CASE("CORNER")
          staggerLoc(ii) = ESMF_STAGGERLOC_CORNER
       CASE DEFAULT
          msg = 'ERROR: unrecognised staggerLoc string '
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END SELECT
    END DO

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_OM_staggerCodes


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_populateFieldBundleOn2dGrid(fieldNames,fieldBundle,grid,init_value,&
       fieldStagger,TLW,TUW,RouteHandle,rc)

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN)     :: fieldNames(:)
    TYPE(ESMF_grid),INTENT(IN)                :: grid
    REAL(ESMF_KIND_R8),INTENT(IN),OPTIONAL    :: init_value
    TYPE(ESMF_staggerLoc),INTENT(IN),OPTIONAL :: fieldStagger(:)
    INTEGER,INTENT(IN),OPTIONAL               :: TLW(2), TUW(2)
    TYPE(ESMF_RouteHandle),INTENT(IN),OPTIONAL:: RouteHandle

    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: fieldBundle
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    INTEGER                               :: ii, jj, localDECount
    REAL(ESMF_KIND_R8)                    :: initial_value
    TYPE(ESMF_field)                      :: field
    REAL(ESMF_KIND_R8),POINTER            :: field_ptr(:,:) 

    rc = ESMF_FAILURE

    NULLIFY(field_ptr)

    IF (PRESENT(init_value)) THEN
       initial_value = init_value
    ELSE
       initial_value = 0.0
    END IF

    IF ( ( (PRESENT(TUW)) .AND. (.NOT.PRESENT(TLW)) )   &
         .OR.                                           &
         ( (PRESENT(TLW)) .AND. (.NOT.PRESENT(TUW)) ) ) &
         THEN
       msg = "Expecting neither or both of TUW and TLW"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    IF (PRESENT(RouteHandle).AND.(.NOT.PRESENT(TUW))) THEN 
       msg = "Expecting TUW and TLW if computing halo route handle"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    CALL ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    DO ii=1,SIZE(fieldNames)
       IF (PRESENT(fieldStagger).AND.PRESENT(TUW)) THEN
          field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,      &
               name=TRIM(fieldNames(ii)),                                &
               totalLWidth=TLW,totalUWidth=TUW,                          &
               staggerloc=fieldStagger(ii),                              &
               rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       ELSEIF (PRESENT(fieldStagger)) THEN
          field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,      &
               name=TRIM(fieldNames(ii)),                                &
               staggerloc=fieldStagger(ii),                              &
               rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       ELSEIF (PRESENT(TUW)) THEN
          field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,      &
               name=TRIM(fieldNames(ii)),                                &
               totalLWidth=TLW,totalUWidth=TUW,                          &
               indexflag=ESMF_INDEX_GLOBAL,             &
               rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       ELSE 
          field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,      &
               name=TRIM(fieldNames(ii)),                                &
               rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       END IF

       IF ( (ii.EQ.1) .AND. (PRESENT(RouteHandle)) ) THEN
print*,"need RH"
print*,"need RH"
print*,"need RH"
print*,"need RH"
       END IF

       CALL ESMF_FieldGet(field=field, localDe=0, farrayPtr=field_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       field_ptr      = initial_value
       NULLIFY(field_ptr)

       CALL ESMF_FieldBundleAdd(fieldBundle, (/field/), rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    END DO

    rc = ESMF_SUCCESS
    
  END SUBROUTINE FISOC_populateFieldBundleOn2dGrid
  

  !--------------------------------------------------------------------------------------
  ! Note: this subroutine is almost identical to the grid version, and should probably 
  ! be auto generated in a precompile step rather than the current hard coded duplication.
  SUBROUTINE FISOC_populateFieldBundleOnMesh(fieldNames,fieldBundle,mesh,init_value,rc)

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: fieldNames(:)
    TYPE(ESMF_mesh),INTENT(IN)            :: mesh
    REAL(ESMF_KIND_R8),INTENT(IN),OPTIONAL:: init_value

    TYPE(ESMF_fieldbundle),INTENT(INOUT)  :: fieldBundle
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    INTEGER                               :: ii
    REAL(ESMF_KIND_R8)                    :: initial_value
    TYPE(ESMF_field)                      :: field
    REAL(ESMF_KIND_R8),POINTER            :: field_ptr(:) 

    rc = ESMF_FAILURE

    NULLIFY(field_ptr)

    IF (PRESENT(init_value)) THEN
       initial_value = init_value
    ELSE
       initial_value = 0.0
    END IF

    DO ii=1,SIZE(fieldNames)
       field = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, name=TRIM(fieldNames(ii)), rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CALL ESMF_FieldGet(field=field, localDe=0, farrayPtr=field_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       field_ptr = initial_value       
       CALL ESMF_FieldBundleAdd(fieldBundle, (/field/), rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END DO

    NULLIFY(field_ptr)

    rc = ESMF_SUCCESS
    
  END SUBROUTINE FISOC_populateFieldBundleOnMesh
  

  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_getStringListFromConfig(config,label,stringList,rc)

    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE,INTENT(INOUT) :: stringList(:)

    TYPE(ESMF_config),INTENT(INOUT)      :: config

    CHARACTER(len=ESMF_MAXSTR),INTENT(IN):: label
    INTEGER,INTENT(OUT)                  :: rc

    CHARACTER(len=ESMF_MAXSTR)           :: dummyString
    INTEGER                              :: listCount,ii

    CHARACTER(len=ESMF_MAXSTR)           :: local_label

    rc = ESMF_FAILURE

    ! point config to start of list 
    CALL ESMF_ConfigFindLabel(config,TRIM(label),rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         RETURN
    
    ! how many items in list?
    listCount = 0
    DO WHILE (rc.EQ.0)
       CALL ESMF_ConfigGetAttribute(config, dummyString,rc=rc) 
       IF  (rc.EQ.ESMF_RC_NOT_FOUND) EXIT
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       listCount = listCount + 1
    END DO
    CALL ESMF_ConfigFindLabel(config, TRIM(label),rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! retrieve list items
    ALLOCATE(stringList(listCount))
    DO ii = 1,listCount
       CALL ESMF_ConfigGetAttribute(config, stringList(ii),rc=rc) 
       IF  (rc.EQ.ESMF_RC_NOT_FOUND) EXIT
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END DO

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_getStringListFromConfig



  !--------------------------------------------------------------------
  SUBROUTINE FISOC_FB2NC(filename,fieldBundle,FISOC_config)


    CHARACTER(len=ESMF_MAXSTR),INTENT(INOUT):: filename
    TYPE(ESMF_FieldBundle),INTENT(IN)       :: fieldBundle
    TYPE(ESMF_config),INTENT(INOUT)         :: FISOC_config
    
    TYPE(ESMF_FileStatus_Flag)              :: NC_status
    INTEGER                                 :: rc
    CHARACTER(len=ESMF_MAXSTR)              :: output_dir

    CALL ESMF_ConfigGetAttribute(FISOC_config, output_dir, label='output_dir:', rc=rc)
    IF (rc.EQ.ESMF_RC_NOT_FOUND) THEN
       output_dir = "./"
       msg = "WARNING: output directory not found, setting to current dir"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
            line=__LINE__, file=__FILE__, rc=rc)
    ELSE
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    filename = TRIM(output_dir)//'/'//TRIM(filename)

    IF (ESMF_IO_NETCDF_PRESENT) THEN
       NC_status=ESMF_FILESTATUS_REPLACE
       CALL  ESMF_FieldBundleWrite(fieldBundle, TRIM(filename),  overwrite=.FALSE., & 
            status=NC_status, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)    
       msg = "FB2NC: written netcdf file"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    ELSE
       msg = "ERROR: trying to write NetCDF output but NetCDF "// &
            "not present in this ESMF build."
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)    
    END IF
    
  END SUBROUTINE FISOC_FB2NC



  !--------------------------------------------------------------------
  ! This is a wrapper for ESMF_FieldRegridStore.  This subroutine 
  ! exists because ESMF_FieldRegridStore does not preserve data in the 
  ! field used to create the route handle.  This wrapper copies the 
  ! data before calling ESMF_FieldRegridStore and writes it back in 
  ! afterwards.
  !
  SUBROUTINE FISOC_FieldRegridStore(vm, InField, OutField, regridmethod, &
       unmappedaction, routeHandle, rc)

    TYPE(ESMF_VM),INTENT(IN)                          :: vm
    TYPE(ESMF_Field),INTENT(INOUT)                    :: InField, OutField
    TYPE(ESMF_RegridMethod_Flag),INTENT(IN),OPTIONAL  :: regridmethod
    TYPE(ESMF_UnmappedAction_Flag),INTENT(IN),OPTIONAL:: unmappedaction

    TYPE(ESMF_RouteHandle),INTENT(OUT),OPTIONAL       :: routeHandle
    INTEGER,INTENT(OUT),OPTIONAL                      :: rc

    REAL(ESMF_KIND_R8),ALLOCATABLE   :: InFieldData1D_cp(:), OutFieldData1D_cp(:)
    REAL(ESMF_KIND_R8),ALLOCATABLE   :: InFieldData2D_cp(:,:), OutFieldData2D_cp(:,:)
    REAL(ESMF_KIND_R8),POINTER       :: InFieldData1D(:), OutFieldData1D(:)
    REAL(ESMF_KIND_R8),POINTER       :: InFieldData2D(:,:), OutFieldData2D(:,:)
    INTEGER                          :: InDims, OutDims

    rc = ESMF_FAILURE

    ! check dimensionality of fields
    CALL ESMF_FieldGet(field=InField, dimCount=InDims, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    CALL ESMF_FieldGet(field=OutField, dimCount=OutDims, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! take copy of fields
    SELECT CASE(InDims)
    CASE(1)
       CALL ESMF_FieldGet(field=InField, farrayPtr=InFieldData1D, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       ALLOCATE(InFieldData1D_cp(SIZE(InFieldData1D)))
       InFieldData1D_cp = InFieldData1D
    CASE(2)
       CALL ESMF_FieldGet(field=InField, farrayPtr=InFieldData2D, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       ALLOCATE(InFieldData2D_cp(SIZE(InFieldData2D,1),SIZE(InFieldData2D,2)))
       InFieldData2D_cp = InFieldData2D
    CASE DEFAULT
       msg = 'ERROR: field neither 1D nor 2D in regrid wrapper'
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT
    SELECT CASE(OutDims)
    CASE(1)
       CALL ESMF_FieldGet(field=OutField, farrayPtr=OutFieldData1D, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       ALLOCATE(OutFieldData1D_cp(SIZE(OutFieldData1D)))
       OutFieldData1D_cp = OutFieldData1D
    CASE(2)
       CALL ESMF_FieldGet(field=OutField, farrayPtr=OutFieldData2D, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       ALLOCATE(OutFieldData2D_cp(SIZE(OutFieldData2D,1),SIZE(OutFieldData2D,2)))
       OutFieldData2D_cp = OutFieldData2D       
    CASE DEFAULT
       msg = 'ERROR: field neither 1D nor 2D in regrid wrapper'
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT
    
    CALL ESMF_VMBarrier(vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create the routehandle for regridding
    CALL ESMF_FieldRegridStore(InField, OutField, regridmethod=regridmethod, &
         unmappedaction=unmappedaction, routehandle=routeHandle, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! copy the field data back in, cleaning up as we go
    SELECT CASE(InDims)
    CASE(1)
       InFieldData1D=InFieldData1D_cp
       DEALLOCATE(InFieldData1D_cp)
       NULLIFY(InFieldData1D)
    CASE(2)
       InFieldData2D=InFieldData2D_cp
       DEALLOCATE(InFieldData2D_cp)
       NULLIFY(InFieldData2D)
    END SELECT

    SELECT CASE(OutDims)
    CASE(1)
       OutFieldData1D=OutFieldData1D_cp
       DEALLOCATE(OutFieldData1D_cp)
       NULLIFY(OutFieldData1D)
    CASE(2)
       OutFieldData2D=OutFieldData2D_cp
       DEALLOCATE(OutFieldData2D_cp)
       NULLIFY(OutFieldData2D)
    END SELECT
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_FieldRegridStore


  !--------------------------------------------------------------------
  ! Each gridded component should have either a mesh or a grid.
  ! If a gridded component has neither or both it is generally fatal.
  SUBROUTINE FISOC_OneGrid(fatal,grid,mesh)

    LOGICAL,INTENT(IN)                  :: fatal
    TYPE(ESMF_grid),OPTIONAL,INTENT(IN) :: grid
    TYPE(ESMF_mesh),OPTIONAL,INTENT(IN) :: mesh
    
    IF   (                                                        &
         ( (PRESENT(grid)) .AND. (PRESENT(mesh)) )                & 
         .OR.                                                     &
         ( (.NOT.(PRESENT(grid))) .AND. (.NOT.(PRESENT(mesh))) )  & 
         ) THEN

       msg = "Expecting one mesh OR one grid per gridded component"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__)

       IF (fatal) CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    END IF

  END SUBROUTINE FISOC_OneGrid

! probably scrap this and use ESMF_FieldRead 
  !--------------------------------------------------------------------
  ! Read a netcdf variable into an ESMF field.  
  !
  ! Input arguments:
  !  fileName - name of netcdf file to be read (includes full path) 
  !  field    - ESMF field object
  !  varName  - name of variable in the netcdf file
  ! 
  ! The data for "varName" will be read from the netcdf file and written 
  ! to the vlaues for "field".  Dimensions must match.
  ! 
!  SUBROUTINE FISOC_NC2FB(fileName,varName,field,rc)
!    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: fileName, varName
!    TYPE(ESMF_Field),INTENT(INOUT)        :: field
!    INTEGER,INTENT(OUT),OPTIONAL          :: rc
!    TYPE(ESMF_FileStatus_Flag)            :: NC_status
!    filename = TRIM(output_dir)//'/'//TRIM(filename)
!  END SUBROUTINE FISOC_NC2FB    

END MODULE FISOC_utils_MOD
