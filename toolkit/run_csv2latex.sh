#! /bin/bash
# alias for command lines
libreoffice='/Applications/LibreOffice.app/Contents/MacOS/soffice'
xlsx2csv='/Users/zhuqianqian/surfdrive/phd/xlsx2csv-master/xlsx2csv.py'

# from ods to xlsx
"$libreoffice" --headless --convert-to xlsx ../source/final_paper_sum.ods --outdir ../source
# from xlsx to csv
python "$xlsx2csv" -a ../source/final_paper_sum.xlsx ../source
# csv to summary
./csv2summary < ../source/all.csv > ../res/all.txt
./csv2summary < ../source/guide.csv > ../res/guide.txt
./csv2summary < ../source/assess.csv > ../res/assess.txt
./csv2summary < ../source/existing_tool.csv > ../res/existing_tool.txt
./csv2summary < ../source/mutation_operator.csv > ../res/mutation_operator.txt
./csv2summary < ../source/mop_language.csv > ../res/mop_language.txt

# csv to latex
./csv2latex < ../source/tool_op.csv > ../res/tool_op.txt
./csv2latex < ../source/tool_emp_rt.csv > ../res/tool_emp_rt.txt
./csv2latex < ../source/all.csv > ../res/all_latex.txt
./csv2latex < ../source/guide.csv > ../res/guide_latex.txt
./csv2latex < ../source/mutation_operator.csv > ../res/mutation_operator_latex.txt
