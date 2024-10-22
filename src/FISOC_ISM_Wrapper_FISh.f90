
!--------------------------------------------------------------------------------------
! This wrapper is for the FISh ISM, the simple FrankPattyn Ice Shelf model.

MODULE FISOC_ISM_Wrapper

  USE ESMF
  USE FISOC_utils_MOD
  USE FISh_MOD

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: FISOC_ISM_Wrapper_Init_Phase1,  FISOC_ISM_Wrapper_Init_Phase2,  &
       FISOC_ISM_Wrapper_Run, FISOC_ISM_Wrapper_Finalize

  INTERFACE FISOC_ISM_Wrapper_Init_Phase1
      MODULE PROCEDURE FISOC_ISM_Wrapper_Init_Phase1_mesh
      MODULE PROCEDURE FISOC_ISM_Wrapper_Init_Phase1_grid
   END INTERFACE

CONTAINS

  SUBROUTINE FISOC_ISM_Wrapper_Init_Phase1_grid(FISOC_config,vm,ISM_ExpFB,ISM_grid,rc)

    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_VM),INTENT(INOUT)           :: vm
    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ExpFB
    TYPE(ESMF_grid),INTENT(OUT)           :: ISM_Grid
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    msg = "ERROR: Dummy subroutine called probably due to ISM_gridType error"
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
         line=__LINE__, file=__FILE__, rc=rc)
    CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

  END SUBROUTINE FISOC_ISM_Wrapper_Init_Phase1_grid


  !-----------------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Init_Phase1_mesh(FISOC_config,vm,ISM_ExpFB,ISM_mesh,rc)

    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_VM),INTENT(INOUT)           :: vm
    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ExpFB
    TYPE(ESMF_mesh),INTENT(OUT)           :: ISM_mesh
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    CHARACTER(len=ESMF_MAXSTR)            :: label
    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: ISM_ReqVarList(:)
    INTEGER                               :: localPet, FISh_dt, ISM_dt, tol
    LOGICAL                               :: verbose_coupling

    rc = ESMF_FAILURE

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       PRINT*,""
       PRINT*,"******************************************************************************"
       PRINT*,"**********    ISM FISh wrapper.  Init phase 1 method.    ********************"
       PRINT*,"******************************************************************************"
       PRINT*,""
       PRINT*,"Here we need to get the ISM mesh information into the ESMF_field type. "
       PRINT*,"We also need to create and initialise the required variables using the "
       PRINT*,"ESMF_field type and put them into an ESMF_fieldBundle type."
       PRINT*,""
    END IF

    CALL FISh_initialize()

    CALL FISh2ESMF_mesh(ISM_mesh,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL FISOC_populateFieldBundle(ISM_ReqVarList,ISM_ExpFB,ISM_mesh,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL getFieldDataFromISM(ISM_ExpFB,FISOC_config,rc)

    !--------------------------------------------------------------------------
    ! check that the FISh timestep is consistent with the FISOC ISM timestep
    !
    FISh_dt = INT(dt*secpyr) ! FISh timestep in seconds

    ! get the ISM timestep in seconds
    CALL FISOC_ConfigDerivedAttribute(FISOC_config, ISM_dt, 'ISM_dt_sec',rc=rc) 
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    tol = 10 ! tolerance in seconds
    !
    IF ( (FISh_dt.GT.(ISM_dt+tol)) .OR. (FISh_dt.LT.(ISM_dt-tol)) ) THEN
       WRITE (msg, "(A,I0,A,I0,A)") &
            "FATAL: FISh/ISM timestep inconsistency (", &
            FISh_dt, " and ", ISM_dt, ")"

       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
   
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ISM_Wrapper_Init_Phase1_mesh
  

  !-----------------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Init_Phase2(FISOC_config,vm,ISM_ImpFB,ISM_ExpFB,rc)

    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ImpFB, ISM_ExpFB
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_VM),INTENT(IN)              :: vm
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    INTEGER                               :: localPet
    LOGICAL                               :: verbose_coupling

    rc = ESMF_FAILURE

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       PRINT*,""
       PRINT*,"******************************************************************************"
       PRINT*,"**********    ISM FISh wrapper.  Init phase 2 method.    ********************"
       PRINT*,"******************************************************************************"
       PRINT*,""
       PRINT*,"Here we have access to the initialised OM fields, just in case the ISM needs "
       PRINT*,"to know about these in order to complete its initialisation."
       PRINT*,""
    END IF
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ISM_Wrapper_Init_Phase2


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Run(FISOC_config,vm,ISM_ExpFB,ISM_ImpFB,rc)

    TYPE(ESMF_fieldbundle),INTENT(INOUT),OPTIONAL :: ISM_ImpFB,ISM_ExpFB
    TYPE(ESMF_config),INTENT(INOUT) :: FISOC_config
    TYPE(ESMF_VM),INTENT(IN)        :: vm
    INTEGER,INTENT(OUT),OPTIONAL    :: rc

    INTEGER                      :: localPet
    TYPE(ESMF_field)             :: OM_dBdt_l0
    REAL(ESMF_KIND_R8),POINTER   :: OM_dBdt_l0_ptr(:)
    LOGICAL                      :: verbose_coupling

!    TYPE(ESMF_Field),ALLOCATABLE          :: fieldList(:)
!    CHARACTER(len=ESMF_MAXSTR)            :: fieldName
!    INTEGER                               :: ii, fieldCount

    NULLIFY(OM_dBdt_l0_ptr)

    ! query the FISOC config
    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       PRINT*,""
       PRINT*,"******************************************************************************"
       PRINT*,"*************       ISM FISh wrapper.  Run method.       ********************"
       PRINT*,"******************************************************************************"
       PRINT*,""
       PRINT*,"OM export fields are available.  Run the ISM and return ISM export fields "
       PRINT*,""
    END IF

    IF (localPet.EQ.0) THEN
       
       ! get basal melt rate as an import field and use it to set the mb (mass balance) in 
       ! the FISh model
       CALL ESMF_FieldBundleGet(ISM_ImpFB, fieldName="OM_dBdt_l0", field=OM_dBdt_l0, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
       CALL ESMF_FieldGet(field=OM_dBdt_l0, localDe=0, farrayPtr=OM_dBdt_l0_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       mb = OM_dBdt_l0_ptr(1:maxx)
!print*,"mb",mb

       NULLIFY(OM_dBdt_l0_ptr)
       
       ! now run the FISh model for one timestep
       CALL FISh_run()
       
       CALL getFieldDataFromISM(ISM_ExpFB,FISOC_config,rc)
       
    END IF
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ISM_Wrapper_Run



  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Finalize(FISOC_config,vm,rc)

    TYPE(ESMF_config),INTENT(INOUT)    :: FISOC_config
    INTEGER,INTENT(OUT),OPTIONAL       :: rc
    TYPE(ESMF_VM),INTENT(IN)           :: vm

    INTEGER                            :: localPet
    LOGICAL                            :: verbose_coupling

    rc = ESMF_FAILURE

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       PRINT*,""
       PRINT*,"******************************************************************************"
       PRINT*,"************    ISM FISh wrapper.  Finalise method.     *********************"
       PRINT*,"******************************************************************************"
       PRINT*,""
       PRINT*,"FISOC has taken care of clearing up ESMF types.  Here we just need to call the "
       PRINT*,"ISM finalise method."
    END IF

    CALL FISh_finalize

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_ISM_Wrapper_Finalize


  !--------------------------------------------------------------------------------------
  SUBROUTINE getFieldDataFromISM(ISM_ExpFB,FISOC_config,rc)

    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ExpFB
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    TYPE(ESMF_field)             :: ISM_velocity_l0, ISM_z_l0
    REAL(ESMF_KIND_R8),POINTER   :: ISM_velocity_l0_ptr(:),ISM_z_l0_ptr(:)


    NULLIFY(ISM_velocity_l0_ptr)
    NULLIFY(ISM_z_l0_ptr)

    ! access the ice shelf base depth from the ISM export field bundle and set it 
    ! using the FISh variable hb (short for height at the base).
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldName="ISM_z_l0", field=ISM_z_l0, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    CALL ESMF_FieldGet(field=ISM_z_l0, localDe=0, farrayPtr=ISM_z_l0_ptr, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    ISM_z_l0_ptr(1:maxx)        = hb
    ISM_z_l0_ptr(maxx+1:2*maxx) = hb
    
    ! access the ice shelf base velocity from the ISM export field bundle and set it 
    ! using the FISh variable u.
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldName="ISM_velocity_l0", field=ISM_velocity_l0, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_FieldGet(field=ISM_velocity_l0, localDe=0, farrayPtr=ISM_velocity_l0_ptr, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    ISM_velocity_l0_ptr(1:maxx)        = u
    ISM_velocity_l0_ptr(maxx+1:2*maxx) = u

    
    NULLIFY(ISM_velocity_l0_ptr)
    NULLIFY(ISM_z_l0_ptr)

  END SUBROUTINE getFieldDataFromISM

  
  !--------------------------------------------------------------------------------------
  SUBROUTINE FISh2ESMF_mesh(ESMF_FIShMesh,rc)
    
    TYPE(ESMF_mesh),INTENT(OUT)      :: ESMF_FIShMesh
    INTEGER,INTENT(OUT),OPTIONAL     :: rc

    REAL(ESMF_KIND_R8),PARAMETER     :: domainWidth = 20000.
    CHARACTER(len=ESMF_MAXSTR)       :: subroutineName = "FISh2ESMF_mesh"
    INTEGER                          :: ii, nn
    INTEGER                          :: numNodes, numQuadElems, nodeIndexDim1, nodeIndexDim2
    INTEGER,ALLOCATABLE              :: ESMF_elementTypeList(:),elementIDlist(:)
    INTEGER,ALLOCATABLE              :: nodeOwners(:)
    INTEGER,ALLOCATABLE              :: elemConn(:), nodeIds(:)
    REAL(ESMF_KIND_R8),ALLOCATABLE   :: nodeCoords(:) 


    ! access x(maxx) from FISh module (node locations)
    
    ! FISh is essentially 1D.  Here we make it 2D by defining a hard coded domain width and 
    ! making a mesh of quadrilateral elements
    !
    ! domain length is given by L in FISh module; width is hard coded above in domainWidth
    ! (units are metres)
    !
    ! node numbering layout for the FISh mesh as represented in 2D as an ESMF mesh:
    !
    ! maxx+1 -- maxx+2 -- maxx+3  ...  --maxx*2    
    !   |         |         |              | 
    !   |         |         |              | 
    !   1 ------- 2 ------- 3 --  ...  --maxx
    
    numQuadElems = maxx - 1
    
    ALLOCATE(ESMF_elementTypeList(numQuadElems))
    ALLOCATE(elementIDlist(numQuadElems))
    ALLOCATE(elemConn(4*numQuadElems))

    numNodes = maxx * 2

    ALLOCATE(nodeIds(numNodes))
    ALLOCATE(nodeCoords(numNodes*2)) ! we are making a 2D mesh
    ALLOCATE(nodeOwners(numNodes))

    nodeOwners=0 ! everything on PET 0

    nodeIds = (/(ii, ii=1, numNodes, 1)/)

    ESMF_elementTypeList = ESMF_MESHELEMTYPE_QUAD

    elementIDlist = (/(ii, ii=1, numQuadElems, 1)/)

    ! the ordering of nodes for each element is important in ESMF element connectivity
    DO nn = 1, numQuadElems
       ii = (nn-1)*4
       elemConn(ii+1) = nn
       elemConn(ii+2) = nn+1
       elemConn(ii+3) = nn+maxx+1
       elemConn(ii+4) = nn+maxx
    END DO

    ! loop over nodes to get coords
    DO ii = 1, maxx
       nodeIndexDim1 = (ii-1)*2+1
       nodeIndexDim2 = ii*2
       nodeCoords(nodeIndexDim1) = x(ii)
       nodeCoords(nodeIndexDim2) = 0.0
    END DO
    DO ii = maxx+1, 2*maxx
       nodeIndexDim1 = (ii-1)*2+1
       nodeIndexDim2 = ii*2
       nodeCoords(nodeIndexDim1) = x(ii-maxx)
       nodeCoords(nodeIndexDim2) = domainWidth
    END DO

    ! Create Mesh structure in 1 step
    ESMF_FIShMesh = ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elementIDlist,&
         elementTypes=ESMF_elementTypeList, elementConn=elemConn, &
         rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    
    DEALLOCATE(ESMF_elementTypeList)
    DEALLOCATE(elementIDlist)
    DEALLOCATE(elemConn)
    DEALLOCATE(nodeIds)
    DEALLOCATE(nodeCoords)
    DEALLOCATE(nodeOwners)

    rc = ESMF_SUCCESS

  END SUBROUTINE FISh2ESMF_mesh

END MODULE FISOC_ISM_Wrapper
