
!************************************************************************************************
!   Other subroutines and functions exclusive to metacncv
!************************************************************************************************
!================================================================================================
!       Subroutine to Calculate Coordination Number 
!================================================================================================
        SUBROUTINE calc_colvar_CN_val(natoms,x,s_value)
        IMPLICIT NONE
!   Input variables          
        INTEGER            ::  natoms
        REAL(KIND=8)       ::  x(natoms*3)
!   Output variables          
        REAL(KIND=8)       ::  s_value 
!   Parameters or constants
        REAL(KIND=8)       ::  r0                   ! reference distance
        REAL(KIND=8)       ::  p_value, q_value     ! p and q parameters for CN
!   Dummy variables
        REAL(KIND=8)       ::  rij_value            ! stores atomic distance temporarily
        REAL(KIND=8)       ::  p_term, q_term
!   Dummy variables to account for the atoms
        INTEGER            ::  j1,j2,j3,j4
!   Declare functions
        REAL(KIND=8)       ::  calc_rij             ! function to calc distance between atom i and j
        REAL(KIND=8)       ::  calc_term            ! function to calc p_term or q_term

!=======================================================
!   Initialise variables and constants   
!=======================================================
        r0       = 3.4 
        p_value  = 10.0
        q_value  = 16.0
        p_term   = 0.0
        q_term   = 0.0
        s_value  = 0.0
!=======================================================
!   Calculate interparticle distance, rij
!=======================================================
        DO j1=1, natoms
           DO j2=1, natoms
!   Condition where atom i is not equal to atom j, i.NE.j, r_ii equals to null
              IF (j2.NE.j1) THEN
                 rij_value = calc_rij(j1,j2,x,natoms)         !call function:calc_rij

!=======================================================
!   Conditional function for the CV
!=======================================================
                 p_term   =  calc_term(rij_value,r0,p_value ) !call function:calc_term for p_value 
                 q_term   =  calc_term(rij_value,r0,q_value ) !call function:calc_term for q_value

                 s_value = s_value + (p_term / q_term)        !summation of all the terms
              END IF
           END DO
        END DO

        s_value = s_value / natoms                            !divided by the number of atoms


        END SUBROUTINE  calc_colvar_CN_val


!================================================================================================
!       Function to calculate the p & q fractional terms in the Coodination Number colvar
!================================================================================================
        REAL(KIND=8) FUNCTION calc_term(rij,r0,term_value  ) 
        REAL(KIND=8)     ::  rij
        REAL(KIND=8)     ::  r0, term_value                !term_value: p_value or q_value
        REAL(KIND=8)     ::  temp

        temp      = (rij / r0) ** term_value          !According to the Formula in Santarossa Au12

        calc_term = 1 - temp

        END FUNCTION calc_term


