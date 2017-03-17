MODULE FISOC_coupler_MOD
  
  USE ESMF
  USE FISOC_utils_MOD
    
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC FISOC_coupler_register
    
CONTAINS
  
  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_coupler_register(FISOC_coupler, rc)
    
    TYPE(ESMF_CplComp)  :: FISOC_coupler
    INTEGER, INTENT(OUT) :: rc
    
    rc = ESMF_FAILURE

    CALL ESMF_CplCompSetEntryPoint(FISOC_coupler, ESMF_METHOD_INITIALIZE, &
         userRoutine=FISOC_coupler_init_phase1, phase=1, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_CplCompSetEntryPoint(FISOC_coupler, ESMF_METHOD_INITIALIZE, &
         userRoutine=FISOC_coupler_init_phase2, phase=2, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_CplCompSetEntryPoint(FISOC_coupler, ESMF_METHOD_RUN, &
         userRoutine=FISOC_coupler_run_phase1, phase=1, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_CplCompSetEntryPoint(FISOC_coupler, ESMF_METHOD_RUN, &
         userRoutine=FISOC_coupler_run_phase2, phase=2, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    CALL ESMF_CplCompSetEntryPoint(FISOC_coupler, ESMF_METHOD_FINALIZE, &
         userRoutine=FISOC_coupler_finalise, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    rc = ESMF_SUCCESS
 
  END SUBROUTINE FISOC_coupler_register

  !------------------------------------------------------------------------------
  ! Initialisation is implemented in two phases.  The first phase is to regrid 
  ! the ISM fields to the OM grid/mesh, and the second phase is to convert the OM 
  ! fields to the ISM grid/mesh.
  SUBROUTINE FISOC_coupler_init_phase1(FISOC_coupler, ISM_ExpSt, OM_ImpSt, FISOC_clock, rc)

    TYPE(ESMF_CplComp)     :: FISOC_coupler
    TYPE(ESMF_State)       :: ISM_ExpSt, OM_ImpSt
    TYPE(ESMF_Clock)       :: FISOC_clock
    INTEGER, INTENT(OUT)   :: rc

    TYPE(ESMF_fieldBundle)        :: ISM_ExpFB, OM_ExpFB, OM_ImpFB
    TYPE(ESMF_grid)               :: OM_grid
    TYPE(ESMF_mesh)               :: OM_mesh
    CHARACTER(len=ESMF_MAXSTR)    :: fieldName, ISM_gridType, OM_gridType
    INTEGER                       :: ISM_ExpFieldCount, OM_ExpFieldCount, ii
    TYPE(ESMF_Field),ALLOCATABLE  :: ISM_ExpFieldList(:), OM_ImpFieldList(:), OM_ExpFieldList(:)
    TYPE(ESMF_RouteHandle)        :: ISM2OM_regridRouteHandle
    TYPE(ESMF_TypeKind_Flag)      :: fieldTypeKind
    TYPE(ESMF_VM)                 :: vm
    LOGICAL                       :: verbose_coupling
    TYPE(ESMF_config)             :: FISOC_config
    TYPE(ESMF_RegridMethod_Flag)  :: Regrid_method

    rc = ESMF_FAILURE

    CALL ESMF_cplCompGet(FISOC_coupler, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_cplCompGet(FISOC_coupler, config=FISOC_config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL  FISOC_ConfigDerivedAttribute(FISOC_config, Regrid_method, 'Regrid_method', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, OM_gridType, label='OM_gridType:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Extract ISM field bundle for regridding...
    CALL ESMF_StateGet(ISM_ExpSt, "ISM export fields", ISM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(ISM_ExpFieldList(ISM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, fieldList=ISM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (verbose_coupling) THEN
       msg = "coupler extracted ISM fields from ISM export state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF
    
    ! Extract OM export field bundle just to make routehandle for regridding...
    CALL ESMF_StateGet(OM_ImpSt, "OM export fields", OM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(OM_ExpFieldList(OM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, fieldList=OM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    SELECT CASE (OM_gridType)

    CASE("ESMF_grid","ESMF_Grid")
       CALL ESMF_FieldGet(OM_ExpFieldList(1), grid=OM_grid, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CASE("ESMF_mesh","ESMF_Mesh")
       CALL ESMF_FieldGet(OM_ExpFieldList(1), mesh=OM_mesh, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CASE DEFAULT
       msg = "ERROR: FISOC does not recognise OM_gridType"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END SELECT

    IF (verbose_coupling) THEN
       msg = "coupler extracted OM fields from OM import state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF
       
    IF (SIZE(OM_ExpFieldList).LT.1) THEN
       msg = "OM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    IF (SIZE(ISM_ExpFieldList).LT.1) THEN
       msg = "ISM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

!         regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
!         regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
    ! Create a route handle to add to the state object.  This will be used for regridding.
    CALL FISOC_FieldRegridStore(vm, ISM_ExpFieldList(1), OM_ExpFieldList(1), &
         regridmethod=Regrid_method, &
         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
         routehandle=ISM2OM_regridRouteHandle, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_StateAdd(ISM_ExpSt, (/ISM2OM_regridRouteHandle/), rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "Regrid route handle created and added to ISM export state "
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)

    ! remove the OM export field bundle from OM export state 
    ! (we only put it there to set up the routehandle).
    CALL ESMF_StateRemove (OM_ImpSt, (/"OM export fields"/), rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! We will create OM import fields with names corresponding to the ISM 
    ! export field names and use the new routehandle to regrid the ISM 
    ! export fields onto the new fields.  Then bundle the OM import fields 
    ! and add them to the OM import state.

    ALLOCATE(OM_ImpFieldList(ISM_ExpFieldCount))

    loop_over_fields: DO ii = 1,ISM_ExpFieldCount 

       CALL ESMF_FieldGet(ISM_ExpFieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       SELECT CASE (OM_gridType)
       CASE("ESMF_grid","ESMF_Grid")
          OM_ImpFieldList(ii) = ESMF_FieldCreate(OM_grid, typekind=fieldTypeKind, name=fieldName, rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       CASE("ESMF_mesh","ESMF_Mesh")
          OM_ImpFieldList(ii) = ESMF_FieldCreate(OM_mesh, typekind=fieldTypeKind, name=fieldName, rc=rc)
          IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       END SELECT

       CALL ESMF_FieldRegrid(ISM_ExpFieldList(ii),OM_ImpFieldList(ii), &
            routehandle=ISM2OM_regridRouteHandle, zeroregion= ESMF_REGION_TOTAL, &
            checkflag=.TRUE.,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
       msg = "Regridded field "//fieldName
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)

    END DO loop_over_fields

    OM_ImpFB = ESMF_FieldBundleCreate(name="OM import fields", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_FieldBundleAdd(OM_ImpFB, OM_ImpFieldList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)


    CALL ESMF_StateAdd(OM_ImpSt, (/OM_ImpFB/), rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "Regriding complete. Regridded fields stored in OM import state"
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)

    rc = ESMF_SUCCESS


  END SUBROUTINE FISOC_coupler_init_phase1

! old code probably to me removed after checking for potential future relevance...
!    TYPE(ESMF_grid)               :: OM_grid, ISM_grid
!    TYPE(ESMF_mesh)               :: ISM_mesh, OM_mesh
!
!    ! get virtual machine
!    CALL ESMF_cplCompGet(FISOC_coupler, vm=vm, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

!    CALL ESMF_ConfigGetAttribute(FISOC_config, ISM_gridType, label='ISM_gridType:', rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    CALL ESMF_ConfigGetAttribute(FISOC_config, OM_gridType, label='OM_gridType:', rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    ! get the grid object for the OM and ISM.  Obtained via one of the fields.
!
!    SELECT CASE (ISM_gridType)
!
!    CASE("ESMF_grid","ESMF_Grid")
!       CALL ESMF_FieldGet(ISM_ExpFieldList(1), grid=ISM_grid, rc=rc)
!       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) &
!            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    CASE("ESMF_mesh","ESMF_Mesh")
!       CALL ESMF_FieldGet(ISM_ExpFieldList(1), mesh=ISM_mesh, rc=rc)
!       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) &
!            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    CASE DEFAULT
!       msg = "ERROR: FISOC does not recognise ISM_gridType"
!       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
!            line=__LINE__, file=__FILE__, rc=rc)
!       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!    END SELECT
!
!
!    SELECT CASE (OM_gridType)
!
!    CASE("ESMF_grid","ESMF_Grid")
!       CALL ESMF_FieldGet(OM_ExpFieldList(1), grid=OM_grid, rc=rc)
!       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) &
!            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    CASE("ESMF_mesh","ESMF_Mesh")
!       CALL ESMF_FieldGet(OM_ExpFieldList(1), mesh=OM_mesh, rc=rc)
!       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) &
!            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!    CASE DEFAULT
!       msg = "ERROR: FISOC does not recognise OM_gridType"
!       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
!            line=__LINE__, file=__FILE__, rc=rc)
!       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!    END SELECT
!
!
! bundles must have same number of fields
!    CALL ESMF_FieldBundleRegridStore(ISM_ExpFB, OM_ExpFB, &
!         regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
!         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!         routehandle=ISM2OM_regridRouteHandle, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! some code for getting info about the grids/meshes...
!    CALL ESMF_MeshGet(ISM_mesh, parametricDim=parametricDim, spatialDim=spatialDim, coordSys=IMS_mesh_coordSys, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!    print*,parametricDim, spatialDim, IMS_mesh_coordSys
!    print*,ESMF_COORDSYS_CART
!    print*,ESMF_COORDSYS_SPH_DEG
!    print*,ESMF_COORDSYS_SPH_RAD
!    CALL ESMF_GridGet(OM_grid, dimCount=dimCount, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)



!    CALL ESMF_FieldRegridStore(ISM_ExpFieldList(1), OM_ExpFieldList(1), &
!         regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
!         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!         routehandle=ISM2OM_regridRouteHandle, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)


!    CALL ESMF_StateGet(OM_ExpSt, "OM export fields", OM_ExpFB, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)    




  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_coupler_init_phase2(FISOC_coupler, OM_ExpSt, ISM_ImpSt, FISOC_clock, rc)

    TYPE(ESMF_CplComp)     :: FISOC_coupler
    TYPE(ESMF_State)       :: OM_ExpSt, ISM_ImpSt
    TYPE(ESMF_Clock)       :: FISOC_clock
    INTEGER, INTENT(OUT)   :: rc

    TYPE(ESMF_fieldBundle)        :: ISM_ExpFB, OM_ExpFB, ISM_ImpFB
    TYPE(ESMF_grid)               :: OM_grid
    TYPE(ESMF_mesh)               :: ISM_mesh
    CHARACTER(len=ESMF_MAXSTR)    :: fieldName
    INTEGER                       :: ISM_ExpFieldCount, OM_ExpFieldCount, ii
    TYPE(ESMF_Field),ALLOCATABLE  :: ISM_ExpFieldList(:), ISM_ImpFieldList(:), OM_ExpFieldList(:)
    TYPE(ESMF_RouteHandle)        :: OM2ISM_regridRouteHandle
    TYPE(ESMF_TypeKind_Flag)      :: fieldTypeKind
    TYPE(ESMF_VM)                 :: vm
    LOGICAL                       :: verbose_coupling
    TYPE(ESMF_config)             :: FISOC_config
    TYPE(ESMF_RegridMethod_Flag)  :: Regrid_method


    rc = ESMF_FAILURE

    CALL ESMF_cplCompGet(FISOC_coupler, vm=vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_cplCompGet(FISOC_coupler, config=FISOC_config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL  FISOC_ConfigDerivedAttribute(FISOC_config, Regrid_method, 'Regrid_method', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Extract OM field bundle for regridding...
    CALL ESMF_StateGet(OM_ExpSt, "OM export fields", OM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(OM_ExpFieldList(OM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, fieldList=OM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "coupler extracted OM fields from OM export state"
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)

    ! Extract ISM export field bundle just to get grid for regridding...
    CALL ESMF_StateGet(ISM_ImpSt, "ISM export fields", ISM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(ISM_ExpFieldList(ISM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, fieldList=ISM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "coupler extracted ISM fields from ISM import state"
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)

    IF (SIZE(OM_ExpFieldList).LT.1) THEN
       msg = "OM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    IF (SIZE(ISM_ExpFieldList).LT.1) THEN
       msg = "ISM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    CALL ESMF_FieldGet(ISM_ExpFieldList(1), mesh=ISM_mesh, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_FieldGet(OM_ExpFieldList(1), grid=OM_grid, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)


    ! Create a route handle to add to the state object.  This will be used for regridding.
    CALL FISOC_FieldRegridStore(vm,OM_ExpFieldList(1), ISM_ExpFieldList(1), &
         regridmethod=Regrid_method, &
         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
         routehandle=OM2ISM_regridRouteHandle, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!    CALL ESMF_FieldRegridStore(OM_ExpFieldList(1), ISM_ExpFieldList(1), &
!         regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
!         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!         routehandle=OM2ISM_regridRouteHandle, rc=rc)
!    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!         line=__LINE__, file=__FILE__)) &
!         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_StateAdd(OM_ExpSt, (/OM2ISM_regridRouteHandle/),rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "Regrid route handle created and added to OM export state "
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)

    ! remove the ISM export field bundle from ISM export state (we only put it there to get the ISM grid)
    CALL ESMF_StateRemove (ISM_ImpSt, (/"ISM export fields"/), rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ALLOCATE(ISM_ImpFieldList(OM_ExpFieldCount))

    loop_over_fields: DO ii = 1,OM_ExpFieldCount 

       CALL ESMF_FieldGet(OM_ExpFieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       ISM_ImpFieldList(ii) = ESMF_FieldCreate(ISM_mesh, typekind=fieldTypeKind, name=fieldName, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

       CALL ESMF_FieldRegrid(OM_ExpFieldList(ii),ISM_ImpFieldList(ii), &
            routehandle=OM2ISM_regridRouteHandle, zeroregion= ESMF_REGION_TOTAL, &
            checkflag=.TRUE.,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
       msg = "Regridded field "//fieldName
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)

    END DO loop_over_fields

    ISM_ImpFB = ESMF_FieldBundleCreate(name="ISM import fields", rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_FieldBundleAdd(ISM_ImpFB, ISM_ImpFieldList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_StateAdd(ISM_ImpSt, (/ISM_ImpFB/), rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    msg = "Regriding complete. Regridded fields stored in ISM import state"
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
         line=__LINE__, file=__FILE__, rc=rc)

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_coupler_init_phase2


  

  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_coupler_run_phase2(FISOC_coupler, ISM_ExpSt, OM_ImpSt, FISOC_clock, rc)
    TYPE(ESMF_CplComp)     :: FISOC_coupler
    TYPE(ESMF_State)       :: ISM_ExpSt, OM_ImpSt
    TYPE(ESMF_Clock)       :: FISOC_clock
    INTEGER, INTENT(OUT)   :: rc

    INTEGER,PARAMETER             :: ListLen = 20
    TYPE(ESMF_fieldBundle)        :: OM_ExpFB, ISM_ExpFB, OM_ImpFB
    TYPE(ESMF_config)             :: config
    CHARACTER(len=ESMF_MAXSTR)    :: OM_name, ISM_name, fieldName, ISM_ExpSt_NameList(ListLen), ISM2OM_HandleName
    TYPE(ESMF_StateItem_Flag)     :: ISM_ExpSt_TypeList(ListLen)
    INTEGER                       :: OM_ImpFieldCount, ISM_ExpFieldCount, ii, NumRouteHandleItems, RouteHandleIndex
    TYPE(ESMF_Field),ALLOCATABLE  :: OM_ImpFieldList(:), ISM_ExpFieldList(:)
    TYPE(ESMF_RouteHandle)        :: ISM2OM_regridRouteHandle
    TYPE(ESMF_TypeKind_Flag)      :: fieldTypeKind

    REAL(ESMF_KIND_R8),POINTER    :: optr(:,:),iptr(:)
    INTEGER                       :: nn
    LOGICAL                       :: verbose_coupling
    TYPE(ESMF_config)             :: FISOC_config

    rc = ESMF_FAILURE

    CALL ESMF_cplCompGet(FISOC_coupler, config=FISOC_config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get routehandle name (this is a bit tedious - need to work out how to define a name for a 
    ! route handle when adding it to a state)
    CALL ESMF_StateGet(ISM_ExpSt, itemNameList=ISM_ExpSt_NameList, itemTypeList=ISM_ExpSt_TypeList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    NumRouteHandleItems = 0
    DO ii = 1,ListLen
       IF (ISM_ExpSt_TypeList(ii).EQ.ESMF_STATEITEM_ROUTEHANDLE) THEN
          NumRouteHandleItems = NumRouteHandleItems + 1
          RouteHandleIndex = ii
       END IF
    END DO
    IF (NumRouteHandleItems.EQ.1) THEN
       ISM2OM_HandleName = ISM_ExpSt_NameList(RouteHandleIndex)
    ELSE
       WRITE (msg, "(A,I0)") &
            "Cannot get route handle from ISM export state.  Wrong number of route handle items: ", NumRouteHandleItems
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    ! Extract ISM2OM regrid routehandle for regridding...
    CALL ESMF_StateGet(ISM_ExpSt, ISM2OM_HandleName, ISM2OM_regridRouteHandle, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Extract ISM field bundle for regridding...
    CALL ESMF_StateGet(ISM_ExpSt, "ISM export fields", ISM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(ISM_ExpFieldList(ISM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=ISM_ExpFieldCount, fieldList=ISM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (verbose_coupling) THEN
       msg = "coupler extracted ISM fields from ISM export state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF

    IF (SIZE(ISM_ExpFieldList).LT.1) THEN
       msg = "ISM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    ! Extract OM field bundle for regridding...
    CALL ESMF_StateGet(OM_ImpSt, "OM import fields", OM_ImpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(OM_ImpFB, fieldCount=OM_ImpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(OM_ImpFieldList(OM_ImpFieldCount))
    CALL ESMF_FieldBundleGet(OM_ImpFB, fieldList=OM_ImpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    loop_over_fields: DO ii = 1,ISM_ExpFieldCount 

       CALL ESMF_FieldGet(ISM_ExpFieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

!CALL ESMF_FieldGet(ISM_ExpFieldList(ii), farrayPtr=iptr, rc=rc)
!IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, file=__FILE__)) &
!     CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!print*,fieldName,SIZE(iptr(:))
!do nn = 1,SIZE(iptr(:))
!iptr(nn) = 10.0 * nn
!end do
!print*,iptr(:)
!nullify(iptr)

       CALL ESMF_FieldRegrid(ISM_ExpFieldList(ii),OM_ImpFieldList(ii), &
            routehandle=ISM2OM_regridRouteHandle, zeroregion= ESMF_REGION_TOTAL, &
            checkflag=.TRUE.,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
!CALL ESMF_FieldGet(OM_ImpFieldList(ii), farrayPtr=optr, rc=rc)
!IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, file=__FILE__)) &
!     CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!print*,fieldName
!print*,optr
!nullify(optr)

       IF (verbose_coupling) THEN
          msg = "Regridded field "//fieldName
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
       END IF

    END DO loop_over_fields

    IF (verbose_coupling) THEN
       msg = "Regriding complete. Regridded fields stored in OM import state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_coupler_run_phase2


  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_coupler_run_phase1(FISOC_coupler, OM_ExpSt, ISM_ImpSt, FISOC_clock, rc)
    TYPE(ESMF_CplComp)     :: FISOC_coupler
    TYPE(ESMF_State)       :: OM_ExpSt, ISM_ImpSt
    TYPE(ESMF_Clock)       :: FISOC_clock
    INTEGER, INTENT(OUT)   :: rc

    INTEGER,PARAMETER             :: ListLen = 20
    TYPE(ESMF_fieldBundle)        :: ISM_ExpFB, OM_ExpFB, ISM_ImpFB
    TYPE(ESMF_config)             :: config
    CHARACTER(len=ESMF_MAXSTR)    :: ISM_name, OM_name, fieldName, OM_ExpSt_NameList(ListLen), OM2ISM_HandleName
    TYPE(ESMF_StateItem_Flag)     :: OM_ExpSt_TypeList(ListLen)
    INTEGER                       :: ISM_ImpFieldCount, OM_ExpFieldCount, ii, NumRouteHandleItems, RouteHandleIndex
    TYPE(ESMF_Field),ALLOCATABLE  :: ISM_ImpFieldList(:), OM_ExpFieldList(:)
    TYPE(ESMF_RouteHandle)        :: OM2ISM_regridRouteHandle
    TYPE(ESMF_TypeKind_Flag)      :: fieldTypeKind
    
    REAL(ESMF_KIND_R8),POINTER    :: optr(:,:),iptr(:)
    INTEGER                       :: nn
    LOGICAL                       :: verbose_coupling, OM_writeNetcdf
    TYPE(ESMF_config)             :: FISOC_config

    rc = ESMF_FAILURE

    CALL ESMF_cplCompGet(FISOC_coupler, config=FISOC_config, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get routehandle name (this is a bit tedious - need to work out how to define a name for a 
    ! route handle when adding it to a state)
    CALL ESMF_StateGet(OM_ExpSt, itemNameList=OM_ExpSt_NameList, itemTypeList=OM_ExpSt_TypeList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    NumRouteHandleItems = 0
    DO ii = 1,ListLen
       IF (OM_ExpSt_TypeList(ii).EQ.ESMF_STATEITEM_ROUTEHANDLE) THEN
          NumRouteHandleItems = NumRouteHandleItems + 1
          RouteHandleIndex = ii
       END IF
    END DO
    IF (NumRouteHandleItems.EQ.1) THEN
       OM2ISM_HandleName = OM_ExpSt_NameList(RouteHandleIndex)
    ELSE
       WRITE (msg, "(A,I0)") &
            "Cannot get route handle from OM export state.  Wrong number of route handle items: ", NumRouteHandleItems
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    ! Extract OM2ISM regrid routehandle for regridding...
    CALL ESMF_StateGet(OM_ExpSt, OM2ISM_HandleName, OM2ISM_regridRouteHandle, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Extract OM field bundle for regridding...
    CALL ESMF_StateGet(OM_ExpSt, "OM export fields", OM_ExpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(OM_ExpFieldList(OM_ExpFieldCount))
    CALL ESMF_FieldBundleGet(OM_ExpFB, fieldCount=OM_ExpFieldCount, fieldList=OM_ExpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF (verbose_coupling) THEN
       msg = "coupler extracted OM fields from OM export state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF
    
    IF (SIZE(OM_ExpFieldList).LT.1) THEN
       msg = "OM field list less than length 1"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, file=__FILE__, rc=rc)
       CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    ! Extract ISM field bundle for regridding...
    CALL ESMF_StateGet(ISM_ImpSt, "ISM import fields", ISM_ImpFB, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ...how many fields?...
    CALL ESMF_FieldBundleGet(ISM_ImpFB, fieldCount=ISM_ImpFieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! ... get list of fields from bundle.
    ALLOCATE(ISM_ImpFieldList(ISM_ImpFieldCount))
    CALL ESMF_FieldBundleGet(ISM_ImpFB, fieldList=ISM_ImpFieldList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    loop_over_fields: DO ii = 1,OM_ExpFieldCount 

       CALL ESMF_FieldGet(OM_ExpFieldList(ii), name=fieldName, typekind=fieldTypeKind, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

!CALL ESMF_FieldGet(OM_ExpFieldList(ii), farrayPtr=optr, rc=rc)
!IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, file=__FILE__)) &
!     CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!optr = 20.0
!DO nn = 1,SIZE(optr(1,:))
!print*,""
!print*,fieldName,SIZE(optr(:,1))
!print*,optr(:,2)
!print*,optr(:,99)

!END DO
!nullify(optr)

       CALL ESMF_FieldRegrid(OM_ExpFieldList(ii),ISM_ImpFieldList(ii), &
            routehandle=OM2ISM_regridRouteHandle, zeroregion= ESMF_REGION_TOTAL, &
            checkflag=.TRUE.,rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
!CALL ESMF_FieldGet(ISM_ImpFieldList(ii), farrayPtr=iptr, rc=rc)
!IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, file=__FILE__)) &
!     CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
!print*,fieldName
!print*,iptr
!nullify(iptr)

       IF (verbose_coupling) THEN
          msg = "Regridded field "//fieldName
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)          
       END IF

    END DO loop_over_fields

    IF (verbose_coupling) THEN
       msg = "Regriding complete. Regridded fields stored in ISM import state"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF

    rc = ESMF_SUCCESS
    
  END SUBROUTINE FISOC_coupler_run_phase1


  !------------------------------------------------------------------------------
  SUBROUTINE FISOC_coupler_finalise(FISOC_coupler, dummy_ExpSt, dummy_ImpSt, FISOC_clock, rc)
    TYPE(ESMF_CplComp)     :: FISOC_coupler
    TYPE(ESMF_State)       :: dummy_ExpSt, dummy_ImpSt
    TYPE(ESMF_Clock)       :: FISOC_clock
    INTEGER, INTENT(OUT)   :: rc

    rc = ESMF_FAILURE

    msg = "FISOC coupler finalise.  Empty routine."
    CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
       line=__LINE__, file=__FILE__, rc=rc)

    rc = ESMF_SUCCESS
    
  END SUBROUTINE FISOC_coupler_finalise


  !------------------------------------------------------------------------------
  TYPE(ESMF_grid) FUNCTION dummyCreateGrid(mesh, rc)

    TYPE(ESMF_mesh),INTENT(IN) :: mesh 
    INTEGER, INTENT(OUT)       :: rc

    TYPE(ESMF_grid)        :: grid 

    rc = ESMF_FAILURE

    dummyCreateGrid = grid

    rc = ESMF_SUCCESS

  END FUNCTION  dummyCreateGrid

END MODULE FISOC_coupler_MOD
