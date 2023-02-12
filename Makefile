ANALYSE=./qbeanalyse.py
DOT=dot

%.dot: %.ssa
	${ANALYSE} $^ --dot > $@


%.svg: %.dot
	${DOT} $^ -Tsvg > $@
	@eog $@ 
