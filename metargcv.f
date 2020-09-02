!************************************************************************************************
! Other subroutines and functions exclusive to metargcv
!************************************************************************************************
!================================================================================================
! Subroutine to calculate Radius of Gyration
!================================================================================================
        SUBROUTINE  calc_colvar_Rg_val(natoms,x,s_value)
        IMPLICIT NONE
!   Input variables
        integer     natoms               ! number of atoms
        real*8      x(natoms*3)          ! coordinates of the atoms
!   Output variables
        real*8      s_value              !collective variable value at current configuration
!   Local variables and constants for Radius of Gyration calculation
        real*8      mass                  ! mass of Gold atoms 
        integer     j1,j2,j3,j4           ! to account for the atoms
        real*8      xrcom, yrcom, zrcom   ! centre of mass(com) for each component 
        real*8      xdiff, ydiff, zdiff   ! difference with com
        real*8      x2,y2,z2              ! squares of the diff

!========================================================
!   Initialise variables and constants
!========================================================
        mass      = 196.9666

        xrcom     = 0.0
        yrcom     = 0.0
        zrcom     = 0.0
        xdiff     = 0.0
        ydiff     = 0.0
        zdiff     = 0.0
        x2        = 0.0
        y2        = 0.0
        z2        = 0.0
        s_value   = 0.0

!========================================================
!   Calculate Radius of Gyration (as in Santarossa Au12)
!=========================================================
!   Calculate the difference between atomic coordinates and com
        CALL calc_centre_of_mass(x,natoms,mass,xrcom,yrcom,zrcom)

        DO j1=1, natoms
             j2=j1*3

             xdiff = x(j2-2) - xrcom
             ydiff = x(j2-1) - yrcom
             zdiff = x(j2  ) - zrcom

             x2= x2 + xdiff**2
             y2= y2 + ydiff**2
             z2= z2 + zdiff**2

        END DO

!   Calculate the value of the Radius of Gyration
        s_value = x2 + y2 + z2
        s_value = s_value / natoms
        s_value = DSQRT(s_value) * 2

        END SUBROUTINE  calc_colvar_Rg_val                    



!================================================================================================
!   Subroutine to calculate the centre of mass(com)
!================================================================================================
        SUBROUTINE calc_centre_of_mass(x,natoms,mass,
     1 xrcom,yrcom,zrcom)
!  Input variables 
        INTEGER natoms
        REAL*8  x(natoms*3)
        REAL*8  mass
!  Output variables
        REAL*8  xrcom, yrcom, zrcom
!  Dummy variables    
        INTEGER j1,j2

        DO j1=1, natoms
             j2=j1*3
             xrcom = xrcom + mass*x(j2-2)
             yrcom = yrcom + mass*x(j2-1)
             zrcom = zrcom + mass*x(j2)
        END DO

        xrcom = xrcom / natoms / mass
        yrcom = yrcom / natoms / mass
        zrcom = zrcom / natoms / mass


        END SUBROUTINE calc_centre_of_mass
