FC = gfortran

MAIN = main.f

METACN = metacncv.f
METARG = metargcv.f
CALCFUNC = func_calc_meta.f

all: colvar

colvar : $(MAIN) $(METACN) $(METARG) $(CALCFUNC)
	$(FC) $(MAIN) $(METACN) $(METARG) $(CALCFUNC) -o colvar.out

clean:
	rm colvar.out
	rm output_colvar.dat
