      PROGRAM main
      IMPLICIT NONE
      CHARACTER(LEN=40) :: file_name
      CHARACTER(LEN=2)  :: dummy
      REAL(KIND=8)      :: CN_val,RG_val
      INTEGER           :: j1,j2,istat
      INTEGER           :: atoms
      REAL(KIND=8),ALLOCATABLE  :: x(:)
      CN_val = 0.0
      RG_val = 0.0
      WRITE(*,'(/A)') repeat("*",80)
      WRITE(*,*) 'Colvar Calculation Programme for'
      WRITE(*,*) 'Coordination Number, CN'
      WRITE(*,*) '           &                 '
      WRITE(*,*) ' Radius of Gyration, Rg'
      WRITE(*,'(/A)') repeat("*",80)

      CALL get_command_argument(1,file_name)
      IF(file_name =='') THEN
        WRITE(*,*)
        WRITE(*,*) 'Use'
        WRITE(*,*) './x.colvar file_name'
        WRITE(*,*) 'Try again'
        WRITE(*,*)
        STOP
      END IF
!=======================================================
! Determine the number of atoms
      OPEN(20,file=trim(file_name),status='old')
      READ(20,*) atoms
      CLOSE(20)

!=======================================================
! Display file and number of atoms
      WRITE(*,*) 'File read=', file_name
      WRITE(*,*) 'Atoms =', atoms
      WRITE(*,'(/A)') repeat("*",80)
! Allocate arrays
      ALLOCATE(x(atoms*3),STAT=istat)

!=======================================================
! Read coordinates
      OPEN(22,file=trim(file_name),status='old')
      READ(22,*) 
      READ(22,*) 
      DO j1=1,atoms
         j2=j1*3
         READ(22,*) dummy, x(j2-2), x(j2-1), x(j2)
      END DO

!=======================================================
! Calculate colvar
      CALL calc_colvar_CN_val(atoms,x,CN_val)  
      CALL calc_colvar_RG_val(atoms,x,RG_val)  

!=======================================================
! Output
      OPEN(21,file='output_colvar.dat',status='replace')
      WRITE(21,*) 'CN=', CN_val
      WRITE(21,*) 'RG=', RG_val
      CLOSE(21)
      CLOSE(22)

      WRITE(*,*) 'Colvars calculated:'
      WRITE(*,*) 
      WRITE(*,*) 'CN=', CN_val
      WRITE(*,*) 'RG=', RG_val


      WRITE(*,*) 
      WRITE(*,*) 'Results saved in:'
      WRITE(*,*) 'output_colvar.dat'
      WRITE(*,*) 

      END PROGRAM main
