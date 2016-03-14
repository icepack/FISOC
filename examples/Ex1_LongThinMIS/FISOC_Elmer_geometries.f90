
! Functions for setting initial geometry for Elmer examples 
! used in FISOC.
!
! Ex1 aims to match the geometry for the ROMS iceshelf2d case.
!

MODULE  MOD_Ex1
  
  USE DefUtils
  IMPLICIT NONE

  !-------------------------------------------------------------------------------
  ! The key parameters defining geometry in ROMS are identified and hard coded 
  ! here.
  ! Note that some of this information is also used in the .grd file to create 
  ! the Elmer/Ice mesh.  Here is for bed rock and upper and lower surfaces.
  !-------------------------------------------------------------------------------
  !
  ! ROMS_gl_dist is the distance from inland boundary to initial grounding line.
  ! ROMS_j_over_Mm is the fractional distance through the domain in j direction.
  !
  !-------------------------------------------------------------------------------
  ! Where to find this stuff in ROMS:
  !-------------------------------------------------------------------------------
  ! depth is a parameter referred to as "Maximum depth of bathymetry (m)" and 
  !   and can be found in analytical.f90 (made from ROMS/Functionals/ana_grid.h).
  ! Esize is the length of the domain in the ETA-direction (j or y direction) 
  !   and can be found in analytical.f90.
  ! Mm is the number of internal grid points in the ETA-direction and can be 
  !   found in the .in file.
  ! dy is grid cell size in ETA-direction, calculated in analytical.f90.
  ! Grounding line distance is measured in units of grid cells and is hard coded 
  !   in several locations in analytical.f90 (TODO: hard code this only once).
  ! ROMS upper cavity boundary (bottom of ice shelf) is defined by zice, which 
  !   is initially hard coded in analytical.f90.
  !-------------------------------------------------------------------------------

  REAL(KIND=dp),PARAMETER    :: ROMS_depth     = 980.0_dp
  REAL(KIND=dp),PARAMETER    :: ROMS_Esize     = 500.0E+03_dp
  INTEGER,PARAMETER          :: ROMS_Mm        = 100
  REAL(KIND=dp),PARAMETER    :: ROMS_dy        = ROMS_Esize / REAL(ROMS_Mm,dp)
  REAL(KIND=dp),PARAMETER    :: ROMS_gl_dist   = 60.0_dp * ROMS_dy 
  REAL(KIND=dp)              :: ROMS_j_over_Mm

CONTAINS
  
  !-------------------------------------------------------------------------------
  REAL(KIND=dp) FUNCTION Ex1_LowerSurface(x_dist) RESULT(LowerSurface)    
    REAL(KIND=dp),INTENT(IN) :: x_dist
    ROMS_j_over_Mm = x_dist / ROMS_Esize
    IF (x_dist.LE.ROMS_gl_dist) THEN
       LowerSurface = Ex1_bedrock(x_dist)
    ELSE
       LowerSurface = Ex1_bedrock(ROMS_gl_dist) +                &
            ATAN((x_dist/ROMS_dy-59.0_dp)/10.0_dp) *             &
            (-Ex1_bedrock(ROMS_gl_dist)-300.0_dp)
    END IF
  END FUNCTION Ex1_LowerSurface

  !-------------------------------------------------------------------------------
  REAL(KIND=dp) FUNCTION Ex1_bedrock(x_dist) RESULT(bedrock) ! negative downwards
    REAL(KIND=dp),INTENT(IN)  :: x_dist
    ROMS_j_over_Mm = x_dist / ROMS_Esize    
    bedrock = - ( ROMS_depth * ROMS_j_over_Mm )
  END FUNCTION Ex1_bedrock
  
END MODULE MOD_Ex1


!-------------------------------------------------------------------------------
REAL(KIND=dp) FUNCTION Ex1_bedrock_w(Model, node, x_dist) RESULT(bedrock)
  USE DefUtils
  USE MOD_Ex1
  IMPLICIT NONE
  TYPE(Model_t),INTENT(IN)  :: Model
  INTEGER,INTENT(IN)        :: node
  REAL(KIND=dp),INTENT(IN)  :: x_dist

  bedrock = Ex1_bedrock(x_dist)

END FUNCTION Ex1_bedrock_w
  
  
!-------------------------------------------------------------------------------
REAL(KIND=dp) FUNCTION Ex1_LowerSurface_w(Model, node, x_dist) RESULT(LowerSurface)
  USE DefUtils
  USE MOD_Ex1
  IMPLICIT NONE
  TYPE(Model_t),INTENT(IN)  :: Model
  INTEGER,INTENT(IN)        :: node
  REAL(KIND=dp),INTENT(IN)  :: x_dist

  LowerSurface =  Ex1_LowerSurface(x_dist)
  
END FUNCTION Ex1_LowerSurface_w


!-------------------------------------------------------------------------------
REAL(KIND=dp) FUNCTION Ex1_UpperSurface_w(Model, node, x_dist) RESULT(UpperSurface)
  USE DefUtils
  USE MOD_Ex1
  IMPLICIT NONE
  TYPE(Model_t),INTENT(IN)  :: Model
  INTEGER,INTENT(IN)        :: node
  REAL(KIND=dp),INTENT(IN)  :: x_dist
  
  IF ( x_dist .LE. ROMS_gl_dist) THEN
     UpperSurface =  Ex1_bedrock(x_dist) - Ex1_bedrock(ROMS_gl_dist) * ( 1000.0_dp / 910.0_dp )
  ELSE
     UpperSurface = -Ex1_LowerSurface(x_dist) * ( 1000.0_dp / 910.0_dp - 1.0_dp )
  END IF
  
END FUNCTION Ex1_UpperSurface_w
!-------------------------------------------------------------------------------


