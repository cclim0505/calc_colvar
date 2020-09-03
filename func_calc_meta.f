!===============================================================================================
!       The collection of functions and subroutines available for metadynamics that are added
!       These functions and subroutines are not specific to any colvar
!===============================================================================================
!       Function to calculate the distance between atom i and atom j
!================================================================================================
        REAL(KIND=8) FUNCTION calc_rij(j1,j2,x,natoms)
        INTEGER       ::  j1, j2, j3, j4
        REAL(KIND=8)  ::  x(natoms*3)
        REAL(KIND=8)  ::  temp  
          
        j3=j1*3                                            ! indexing for xyz axis
        j4=j2*3                                            ! indexing for xyz axis

        temp      = sqrt(  ( x(j3-2)-x(j4-2)  )**2         ! x-coordinate distance
     1                   + ( x(j3-1)-x(j4-1)  )**2         ! y-coordinate distance 
     1                   + ( x(j3  )-x(j4  )  )**2     )   ! z-coordinate distance
        calc_rij  = temp   

        END FUNCTION calc_rij





!===============================================================================================
!       Function to calculate the xyz difference between atom i and atom j
!================================================================================================
        REAL(KIND=8) FUNCTION calc_xyz_diff(j1,j2,x,natoms,axis)
        INTEGER                  ::  j1, j2, j3, j4
        REAL(KIND=8)             ::  x(natoms*3)
        REAL(KIND=8)             ::  temp  
        CHARACTER(LEN=1)         ::  axis                      !input for axis type
          
        j3=j1*3                                            ! indexing for xyz axis
        j4=j2*3                                            ! indexing for xyz axis


        IF (axis.eq.'x') THEN
           temp      =  x(j3-2)-x(j4-2)         ! x-coordinate difference

        ELSE IF (axis.eq.'y') THEN
           temp      =  x(j3-1)-x(j4-1)         ! y-coordinate difference

        ELSE IF (axis.eq.'z') THEN
           temp      =  x(j3  )-x(j4  )         ! z-coordinate difference

        END IF
        calc_xyz_diff  = temp   

        END FUNCTION calc_xyz_diff





