
MODULE FISOC_ISM_Wrapper

  USE ESMF
  USE FISOC_utils_MOD
  USE FISOC_types_MOD

  USE icepack_fortran, only: simulation

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: FISOC_ISM_Wrapper_Init_Phase1, FISOC_ISM_WRAPPER_Init_Phase2, &
    FISOC_ISM_Wrapper_Run, FISOC_ISM_Wrapper_Finalize

  TYPE(simulation), SAVE :: icepack_simulation_data

CONTAINS

  ! ---------------------------------------------------------------------------
  ! This initialisation wrapper converts the icepack mesh and variables to ESMF
  ! data structures and performs sanity and consistency checks.
  SUBROUTINE FISOC_ISM_Wrapper_Init_Phase1(FISOC_config,vm,ISM_ExpFB,ISM_mesh,rc)
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_VM),INTENT(INOUT)           :: vm
    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ExpFB
    TYPE(ESMF_mesh),INTENT(OUT)           :: ISM_mesh
    INTEGER,INTENT(OUT),OPTIONAL          :: rc

    CHARACTER(len=ESMF_MAXSTR),ALLOCATABLE:: ISM_ReqVarList(:)
    CHARACTER(len=ESMF_MAXSTR)            :: label, icepack_configName
    INTEGER                               :: localPet, ISM_dt_sec
    LOGICAL                               :: verbose_coupling

    rc = ESMF_FAILURE

    CALL ESMF_ConfigGetAttribute(FISOC_config, verbose_coupling, label='verbose_coupling:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    label = 'FISOC_ISM_ReqVars:'
    CALL FISOC_getListFromConfig(FISOC_config, label, ISM_ReqVarList,rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL ESMF_ConfigGetAttribute(FISOC_config, icepack_configName, label='ISM_configFile:', rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    CALL icepack_simulation_data%initialize(icepack_configName)

    CALL ESMF_VMGet(vm, localPet=localPet, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       PRINT*,""
       PRINT*,"*****************************************************************************"
       PRINT*,"****   ISM icepack wrapper.  Init phase 1 method.    ************************"
       PRINT*,"*****************************************************************************"
       PRINT*,""
    END IF

    IF ((verbose_coupling).AND.(localPet.EQ.0)) THEN
       msg = "Converting icepack mesh to ESMF"
       CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, file=__FILE__, rc=rc)
    END IF
    CALL icepack2ESMF_mesh(FISOC_config, icepack_simulation_data, ISM_mesh, vm, rc=rc)

    CALL FISOC_populateFieldBundle(ISM_ReqVarList, ISM_ExpFB, ISM_mesh, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get required variables from the ice sheet model and convert them to the
    ! equivalent ESMF data structures.
    CALL getFieldDataFromISM(ISM_ExpFB, FISOC_config, vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    rc = ESMF_SUCCESS

  END SUBROUTINE


  ! ---------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Init_Phase2(FISOC_config,vm,ISM_ImpFB,ISM_ExpFB,rc)
    TYPE(ESMF_config),INTENT(INOUT)       :: FISOC_config
    TYPE(ESMF_fieldBundle),INTENT(INOUT)  :: ISM_ImpFB, ISM_ExpFB
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
       PRINT*,"**********    ISM icepack wrapper.  Init phase 2 method.    ******************"
       PRINT*,"******************************************************************************"
       PRINT*,""
       PRINT*,"Here we have access to the initialised OM fields, just in case the ISM needs "
       PRINT*,"to know about these in order to complete its initialisation."
       PRINT*,""
    END IF

    rc = ESMF_SUCCESS

  END SUBROUTINE


  ! ---------------------------------------------------------------------------
  SUBROUTINE FISOC_ISM_Wrapper_Run(FISOC_config,vm,ISM_ExpFB,ISM_ImpFB,rc)

    TYPE(ESMF_fieldbundle),INTENT(INOUT),OPTIONAL :: ISM_ImpFB,ISM_ExpFB
    TYPE(ESMF_config),INTENT(INOUT)      :: FISOC_config
    TYPE(ESMF_VM),INTENT(IN)             :: vm
    INTEGER,INTENT(OUT),OPTIONAL         :: rc

    INTEGER                      :: localPet
    LOGICAL                      :: verbose_coupling

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
       PRINT*,"************        ISM wrapper.  Run method.           **********************"
       PRINT*,"******************************************************************************"
       PRINT*,""
    END IF

    ! Get the ocean model variables and send them to the ice sheet model.
    ! TODO: write this

    CALL ESMF_VMBarrier(vm, rc=rc)
    ! Run the ice sheet model.
    CALL ESMF_VMBarrier(vm, rc=rc)

    CALL getFieldDataFromISM(ISM_ExpFB, FISOC_config, vm, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    rc = ESMF_SUCCESS

  END SUBROUTINE


  ! ---------------------------------------------------------------------------
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
       PRINT*,"************************************************************************"
       PRINT*,"************    ISM wrapper.  Finalise method.     *********************"
       PRINT*,"************************************************************************"
       PRINT*,""
       PRINT*,"FISOC has taken care of clearing up ESMF types.  Here we just need to call the "
       PRINT*,"ISM finalise method."
    END IF

    CALL icepack_simulation_data%destroy

    rc = ESMF_SUCCESS

  END SUBROUTINE


  SUBROUTINE icepack2ESMF_mesh(FISOC_config, simulation_data, ISM_mesh, vm, rc)
    TYPE(ESMF_config), INTENT(INOUT) :: FISOC_config
    TYPE(simulation), INTENT(IN) :: simulation_data
    TYPE(ESMF_mesh),INTENT(INOUT) :: ISM_mesh
    TYPE(ESMF_VM), INTENT(IN) :: vm
    INTEGER,INTENT(OUT),OPTIONAL :: rc

    ! Variables for reading the mesh on the Python side
    INTEGER :: num_cells
    INTEGER, DIMENSION(:,:), POINTER :: cells
    REAL(ESMF_KIND_R8), DIMENSION(:,:), POINTER :: coordinates

    ! Variables for filling the ESMF mesh
    INTEGER :: num_nodes
    INTEGER, DIMENSION(:), ALLOCATABLE :: node_ids, node_owners
    INTEGER, DIMENSION(:), ALLOCATABLE :: elem_types, elem_conn, elem_ids
    REAL(ESMF_KIND_R8), DIMENSION(:), ALLOCATABLE :: node_coords

    INTEGER :: i

    ! Get a pointer to the node coordinates of the ice sheet model mesh and
    ! copy them into an array to create the ESMF mesh.
    CALL simulation_data%get_mesh_coordinates(coordinates)
    num_nodes = size(coordinates, 2)

    ALLOCATE(node_ids(num_nodes))
    DO i = 1, num_nodes
      node_ids(i) = i
    ENDDO

    ALLOCATE(node_coords(2 * num_nodes))
    node_coords = reshape(coordinates, (/2 * num_nodes/))

    ! Make all nodes owned by PET 0.
    ! TODO: Get ownership from the PETSc DMPlex.
    ALLOCATE(node_owners(num_nodes))
    node_owners = 0

    ! Get a pointer to the cells of the ice sheet model mesh and copy all the
    ! connectivity data into an array to create the ESMF mesh.
    CALL simulation_data%get_mesh_cells(cells)
    num_cells = size(cells, 2)

    ALLOCATE(elem_ids(num_cells))
    DO i = 1, num_cells
      elem_ids(i) = i
    ENDDO

    ALLOCATE(elem_types(num_cells))
    elem_types = 3

    ALLOCATE(elem_conn(3 * num_cells))
    elem_conn = reshape(cells, (/3 * num_cells/)) + 1

    ISM_mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, coordSys=ESMF_COORDSYS_CART, &
        nodeIds=node_ids, nodeCoords=node_coords, nodeOwners=node_owners, &
        elementIds=elem_ids, elementTypes=elem_types, elementConn=elem_conn, &
        rc=rc)

  END SUBROUTINE


  SUBROUTINE getFieldDataFromISM(ISM_ExpFB, FISOC_config, vm, rc)
    TYPE(ESMF_fieldBundle),INTENT(INOUT)     :: ISM_ExpFB
    TYPE(ESMF_config),INTENT(INOUT)          :: FISOC_config
    TYPE(ESMF_VM),INTENT(IN)                 :: vm
    INTEGER,INTENT(OUT),OPTIONAL             :: rc

    INTEGER :: fieldCount, localPet, petCount
    TYPE(ESMF_Field), DIMENSION(:), ALLOCATABLE :: fieldList
    CHARACTER(len=ESMF_MAXSTR) :: fieldName
    REAL(ESMF_KIND_R8), DIMENSION(:), POINTER :: icepack_field
    REAL(ESMF_KIND_R8), DIMENSION(:), POINTER :: ptr
    INTEGER :: nn

    rc = ESMF_FAILURE

    CALL ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get a list of fields and their names from the ISM export field bundle
    fieldCount = 0
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldCount=fieldCount, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ALLOCATE(fieldList(fieldCount))
    CALL ESMF_FieldBundleGet(ISM_ExpFB, fieldList=fieldList, rc=rc)
    IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) &
         CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Copy the data for each field
    DO nn = 1, fieldCount
      CALL ESMF_FieldGet(fieldList(nn), name=fieldName, rc=rc)
      IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, file=__FILE__)) &
           CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

      CALL ESMF_FieldGet(fieldList(nn), farrayPtr=ptr, rc=rc)
      IF (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, file=__FILE__)) &
           CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

      SELECT CASE (TRIM(ADJUSTL(fieldName)))
      CASE ('ISM_thick')
        CALL icepack_simulation_data%get_thickness(icepack_field)
        ptr = icepack_field

      CASE ('ISM_z_l0')
        CALL icepack_simulation_data%get_thickness(icepack_field)
        ptr = -917.0 / 1024 * icepack_field

      CASE ('ISM_z_l0_previous', 'ISM_z_l1')
        msg = "WARNING: ignored variable: "//TRIM(ADJUSTL(fieldName))
        CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_WARNING, &
             line=__LINE__, file=__FILE__, rc=rc)

      CASE DEFAULT
        msg = "ERROR: unknown variable: "//TRIM(ADJUSTL(fieldName))
        CALL ESMF_LogWrite(msg, logmsgFlag=ESMF_LOGMSG_ERROR, &
             line=__LINE__, file=__FILE__, rc=rc)
        CALL ESMF_Finalize(endflag=ESMF_END_ABORT)

      ENDSELECT

      IF (ASSOCIATED(ptr)) NULLIFY(ptr)

    ENDDO

    rc = ESMF_SUCCESS

  END SUBROUTINE


END MODULE FISOC_ISM_Wrapper
