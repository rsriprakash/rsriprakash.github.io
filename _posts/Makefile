all: public/script.min.js

public/script.min.js: src/Main.elm src/GradeHistogram.elm src/RandomStudentData.elm src/GradeSuggestions.elm src/GradeBarChart.elm src/Student.elm
	elm make src/Main.elm  --optimize --output=public/script.js
	uglifyjs public/script.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs -o public/script.min.js
	$(RM) public/script.js

.PHONY: clean
clean:
	$(RM) public/script.min.js
