run: init.lisp
	clisp init.lisp

time: time.lisp
	clisp time.lisp

clean: clean-images clean-slime clean-temp clean-cache

clean-images:
	rm -f *.png *.ppm *.gif anim/*.ppm

clean-slime:
	rm -f *.fas *.lib

clean-temp:
	rm -f *~ \#*

clean-cache:
	rm -rf ~/.cache/common-lisp/

generate-script: generate.py
	python generate.py > script
