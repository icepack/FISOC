MODULE FISOC_OM
  
  USE ESMF
    
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC FISOC_OM_register
    
CONTAINS
  
  SUBROUTINE FISOC_OM_register(FISOC_OM, rc)
    
    TYPE(ESMF_GridComp)  :: FISOC_OM
    INTEGER, INTENT(OUT) :: rc
    
    rc = ESMF_FAILURE

    rc = ESMF_SUCCESS
 
  END SUBROUTINE FISOC_OM_register

END MODULE FISOC_OM