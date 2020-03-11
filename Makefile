FC = gfortran

MAIN = main.f

METACN = metacncv.f
METARG = metargcv.f
CALCFUNC = func_calc_meta.f

all: colvar

colvar : $(MAIN) $(METACN) $(METARG) $(CALCFUNC)
	$(FC) $(MAIN) $(METACN) $(METARG) $(CALCFUNC) -o x.colvar

clean:
	rm x.colvar
	rm output_colvar.dat
