      PROGRAM main
      IMPLICIT NONE
      CHARACTER(LEN=40) :: file_name
      CHARACTER(LEN=2)  :: dummy
      REAL(KIND=8)      :: CN_val,RG_val
      INTEGER           :: j1,j2,istat
      INTEGER           :: atoms
      INTEGER           :: timestep
      INTEGER           :: f_xyz, f_cv
      INTEGER           :: ierr
      INTEGER           :: counter=0
      REAL(KIND=8),ALLOCATABLE  :: x(:)

      CN_val = 0.0
      RG_val = 0.0

      WRITE(*,'(/A)') repeat("*",80)
      WRITE(*,*) 'Colvar Calculation Programme for'
      WRITE(*,*) 'Coordination Number, CN'
      WRITE(*,*) '           &                 '
      WRITE(*,*) ' Radius of Gyration, Rg'
      WRITE(*,*) '(Works for constant atom number)'
      WRITE(*,'(/A)') repeat("*",80)

      CALL get_command_argument(1,file_name)
      IF(file_name =='') THEN
        WRITE(*,*)
        WRITE(*,*) 'Use'
        WRITE(*,*) './colvar.out file_name'
        WRITE(*,*) 'Try again'
        WRITE(*,*)
        STOP
      END IF
!=======================================================
! Determine the number of atoms
      OPEN(NEWUNIT=f_xyz,FILE=trim(file_name),STATUS='old')
      READ(f_xyz,*) atoms
      CLOSE(f_xyz)

!=======================================================
! Display file and number of atoms
      WRITE(*,*) 'File read=', file_name
      WRITE(*,*) 'Atoms =', atoms
      WRITE(*,'(/A)') repeat("*",80)
! Allocate arrays
      ALLOCATE(x(atoms*3),STAT=istat)

      OPEN(NEWUNIT=f_cv,FILE='output_colvar.dat',STATUS='replace')
      WRITE(f_cv,*) 'timestep, CN, Rg'
!=======================================================
! Read coordinates
      OPEN(NEWUNIT=f_xyz,FILE=trim(file_name),STATUS='old')

      DO
        READ(f_xyz,*,IOSTAT=ierr) 
        IF (ierr /= 0) EXIT
        READ(f_xyz,*) dummy, dummy, timestep
        DO j1=1,atoms
           j2=j1*3
           READ(f_xyz,*) dummy, x(j2-2), x(j2-1), x(j2)
        END DO
  
        CALL calc_colvar_CN_val(atoms,x,CN_val)  
        CALL calc_colvar_RG_val(atoms,x,RG_val)  
  
        WRITE(f_cv,*) timestep, CN_val, RG_val
        counter = counter + 1
      END DO


      CLOSE(f_cv)
      CLOSE(f_xyz)





!=======================================================
! DEBUGGING STARTS
!=======================================================
!      WRITE(*,*) 'Colvars calculated:'
!      WRITE(*,*) 
!      WRITE(*,*) 'Timestep', timestep
!      WRITE(*,*) 'CN=', CN_val
!      WRITE(*,*) 'RG=', RG_val
!=======================================================
! DEBUGGING ENDS
!=======================================================


      WRITE(*,*) 
      WRITE(*,*) 'Results saved in:'
      WRITE(*,*) 'output_colvar.dat'
      WRITE(*,*) 
      WRITE(*,*) 'Total CVs calculated:', counter
      WRITE(*,*) 

      END PROGRAM main
