
MODULE FISOC_OM_Wrapper

  USE ESMF
  USE FISOC_utils

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: FISOC_OM_Wrapper_Init,  FISOC_OM_Wrapper_Run, FISOC_OM_Wrapper_Finalize

CONTAINS

  !--------------------------------------------------------------------------------------
  ! This dummy wrapper aims to create the dummy grid and required variables 
  ! in the ESMF formats.  
  SUBROUTINE FISOC_OM_Wrapper_Init(OM_ReqVarList,OM_ExpFB,OM_dummyGrid,FISOC_config,rc)

    TYPE(ESMF_config),INTENT(IN)          :: FISOC_config
    CHARACTER(len=ESMF_MAXSTR),INTENT(IN) :: OM_ReqVarList(:)

    TYPE(ESMF_grid),INTENT(OUT)           :: OM_dummyGrid
    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: OM_ExpFB
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    CALL dummyCreateGrid(OM_dummyGrid,rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL FISOC_populateFieldBundle(OM_ReqVarList,OM_ExpFB,OM_dummyGrid,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  END SUBROUTINE FISOC_OM_Wrapper_Init
  

  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_OM_Wrapper_Run(FISOC_config,OM_ExpFB,OM_ImpFB,rc)

    TYPE(ESMF_config),INTENT(INOUT)                :: FISOC_config
    TYPE(ESMF_fieldBundle),INTENT(INOUT),OPTIONAL  :: OM_ExpFB, OM_ImpFB 
    INTEGER,INTENT(OUT),OPTIONAL                   :: rc

    LOGICAL                :: verbose_coupling

    TYPE(ESMF_field)       :: ISM_dTdz_l0,ISM_z_l0, OM_dBdt_l0
    REAL(ESMF_KIND_R8),POINTER :: ISM_dTdz_l0_ptr(:,:), ISM_z_l0_ptr(:,:), OM_dBdt_l0_ptr(:,:)

    rc = ESMF_FAILURE

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)


    IF (verbose_coupling) THEN

       PRINT*,"******************************************************************************"
       PRINT*,"************       OM dummy wrapper.  Run method.       **********************"
       PRINT*,"******************************************************************************"
       PRINT*,""

       IF ((PRESENT(OM_ExpFB)).AND.(.NOT.(PRESENT(OM_ImpFB)))) THEN
          PRINT*,"We have no new inputs for the OM from the ISM.  We need to call the OM "
          PRINT*,"and record its output in the OM export field bundle."
       END IF

       IF ((PRESENT(OM_ExpFB)).AND.(PRESENT(OM_ImpFB))) THEN
          PRINT*,"We have new inputs for the OM from the ISM in the OMimport field bundle. "
          PRINT*,"We need to send these inputs to the OM,run one timestep, and record the OM "
          PRINT*,"outputs. "
       END IF
       
       IF ((.NOT.(PRESENT(OM_ExpFB))).AND.(PRESENT(OM_ImpFB))) THEN
          msg = "OM run error: it is an ISM step but not an ocean output step."
          CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
               line=__LINE__, file=__FILE__, rc=rc)
          CALL ESMF_Finalize(endflag=ESMF_END_ABORT)    
       END IF

       IF ((.NOT.(PRESENT(OM_ExpFB))).AND.(.NOT.(PRESENT(OM_ImpFB)))) THEN
          PRINT*,"We have no new inputs for the OM from the ISM, and we do not need to "
          PRINT*,"collect OM outputs.  Just run the OM one timestep."
       END IF

    END IF

    ! Lets get pointers to the depth of the ice base and the temperature gradient.  These we 
    ! get fromthe OM import state, which contains the ISM export fields on the ocean grid.
    IF (PRESENT(OM_ImpFB)) THEN

       CALL ESMF_FieldBundleGet(OM_ImpFB, fieldName="ISM_z_l0", field=ISM_z_l0, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)       
       CALL ESMF_FieldGet(field=ISM_z_l0, localDe=0, farrayPtr=ISM_z_l0_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
       
       CALL ESMF_FieldBundleGet(OM_ImpFB, fieldName="ISM_dTdz_l0", field=ISM_dTdz_l0, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)       
       CALL ESMF_FieldGet(field=ISM_dTdz_l0, localDe=0, farrayPtr=ISM_dTdz_l0_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF

    IF (PRESENT(OM_ExpFB)) THEN
       ! Lets get a pointer to the basal melt rate.  This we get from the OM export field bundle, which 
       ! contains the OM variables to be exported to the ISM.
       CALL ESMF_FieldBundleGet(OM_ExpFB, fieldName="OM_dBdt_l0", field=OM_dBdt_l0, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)       
       CALL ESMF_FieldGet(field=OM_dBdt_l0, localDe=0, farrayPtr=OM_dBdt_l0_ptr, rc=rc)
       IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            CALL ESMF_Finalize(endflag=ESMF_END_ABORT)
    END IF
    
    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_OM_Wrapper_Run


  !--------------------------------------------------------------------------------------
  SUBROUTINE FISOC_OM_Wrapper_Finalize(rc)

    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    rc = ESMF_FAILURE

    rc = ESMF_SUCCESS

  END SUBROUTINE FISOC_OM_Wrapper_Finalize


  !--------------------------------------------------------------------------------------
  SUBROUTINE dummyCreateGrid(OM_dummyGrid,rc)
    
    TYPE(ESMF_grid),INTENT(INOUT)        :: OM_dummyGrid    
    INTEGER,INTENT(OUT),OPTIONAL         :: rc

    REAL(ESMF_KIND_R8),POINTER :: coordY(:),coordX(:)
    INTEGER                    :: ii, jj, lbnd(1), ubnd(1)

    NULLIFY (coordY,coordX)

    ! this next grid creation section is more or less a copy from the ref documentation example:
    ! http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05083200000000000000
    !-------------------------------------------------------------------
    ! Create the Grid:  Allocate space for the Grid object, define the
    ! topology and distribution of the Grid, and specify that it 
    ! will have global indices.  Note that here aperiodic bounds are
    ! specified by the argument name. In this call the minIndex hasn't 
    ! been set, so it defaults to (1,1,...). The default is to 
    ! divide the index range as equally as possible among the DEs
    ! specified in regDecomp. This behavior can be changed by 
    ! specifying decompFlag. 
    !-------------------------------------------------------------------
    OM_dummyGrid=ESMF_GridCreateNoPeriDim(          &
         ! Define a regular distribution
         maxIndex=(/11,6/), & ! define index space
         !         regDecomp=(/2,3/),  & ! define how to divide among DEs
         coordSys=ESMF_COORDSYS_CART, &
         ! Specify mapping of coords dim to Grid dim
         coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
         coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
         indexflag=ESMF_INDEX_GLOBAL, &
         rc=rc)
    
    !-------------------------------------------------------------------
    ! Allocate coordinate storage and associate it with the center
    ! stagger location.  Since no coordinate values are specified in
    ! this call no coordinate values are set yet.
    !-------------------------------------------------------------------
    CALL ESMF_GridAddCoord(OM_dummyGrid,  & 
         staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    
    !-------------------------------------------------------------------
    ! Get the pointer to the first coordinate array and the bounds
    ! of its global indices on the local DE.   
    !-------------------------------------------------------------------
    CALL ESMF_GridGetCoord(OM_dummyGrid, coordDim=1, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         computationalLBound=lbnd, computationalUBound=ubnd, &
         farrayPtr=coordX, rc=rc)
    
    !-------------------------------------------------------------------
    ! Calculate and set coordinates in the first dimension.
    !-------------------------------------------------------------------
    DO ii=lbnd(1),ubnd(1)
       coordX(ii) = (ii-1)*180000.0
    END DO
    
    !-------------------------------------------------------------------
    ! Get the pointer to the second coordinate array and the bounds of
    ! its global indices on the local DE.
    !-------------------------------------------------------------------
    CALL ESMF_GridGetCoord(OM_dummyGrid, coordDim=2, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         computationalLBound=lbnd, computationalUBound=ubnd, &
         farrayPtr=coordY, rc=rc)
    
    !-------------------------------------------------------------------
    ! Calculate and set coordinates in the second dimension 
    !-------------------------------------------------------------------
    DO jj=lbnd(1),ubnd(1)
       coordY(jj) = (jj-1)*10000.0
    END DO

  END SUBROUTINE dummyCreateGrid


END MODULE FISOC_OM_Wrapper