file(op,type)=sprintf("1/%s-%s.dat",op,type)
num_tx=200000
num_threads=9

time_min=0.007 #0.007870899
time_max=1 #0.9303171
retries_min=1
retries_max=1110176
alloc_min=45e5 #4808840
alloc_max=1059603928

#----------------------------------------------------------------------

set terminal epslatex size 14cm,18cm

set style line 1 lc rgb "#e41a1c" lw 2 pt 1     # red
set style line 2 lc rgb "#377eb8" lw 2 pt 7     # blue
set style line 3 lc rgb "#4daf4a" lw 2 pt 5     # green
set style line 4 lc rgb "black" lw 2

columns=4
set multiplot layout 3,columns

set border 3
unset key
set key autotitle columnheader
unset key
set xrange [-1:num_threads]
set xtics 0,1,num_threads-1 nomirror
set xtics add ("1" 0, "8" 4, "16" 8)
set format x ""
#set grid

#----------------------------------------------------------------------

set macros
POINTS = "u 4 ls 3, '' u 3 ls 2, '' u 2 ls 1"

SET_TIME_PLOT = "set style data linespoint; \
                 set logscale y 2; \
                 set yrange[time_min:time_max]; \
                 set ytics nomirror"

SET_RETRIES_PLOT = "set style data histogram; \
                    set style fill solid; \
                    set logscale y 10; \
                    set yrange [retries_min:retries_max]; \
                    set ytics nomirror; \
                    #set ytics add (num_tx num_tx)"

SET_ALLOC_PLOT = "set style data linespoint; \
                  set logscale y 2; \
                  set yrange[alloc_min:alloc_max]; \
                  set ytics (5e6, 125e6, 250e6, 500e6, 1e9); \
                  set ytics nomirror"

left_margin=0.2
gap=0.025

col_size = (1 - left_margin - (columns-1) * gap) / columns
lmargin_col(x) = left_margin + (x-1)*(col_size+gap)
rmargin_col(x) = lmargin_col(x) + col_size

COL1 = "set lmargin at screen lmargin_col(1); set rmargin at screen rmargin_col(1)"
COL2 = "set lmargin at screen lmargin_col(2); set rmargin at screen rmargin_col(2)"
COL3 = "set lmargin at screen lmargin_col(3); set rmargin at screen rmargin_col(3)"
COL4 = "set lmargin at screen lmargin_col(4); set rmargin at screen rmargin_col(4)"

ROW1 = "set tmargin at screen 0.98; set bmargin at screen 0.75"
ROW2 = "set tmargin at screen 0.675; set bmargin at screen 0.475"
ROW3 = "set tmargin at screen 0.4; set bmargin at screen 0.2"

#----------------------------------------------------------------------

@ROW1

set title "insert"
set ylabel "time"
set format y "%.0s %cs"
@COL1
@SET_TIME_PLOT
plot file("insert","time") @POINTS
unset ylabel
set format y ""

set title "update"
@COL2
@SET_TIME_PLOT
plot file("update","time") @POINTS

set title "lookup"
@COL3
@SET_TIME_PLOT
plot file("lookup","time") @POINTS

set title "delete"
@COL4
@SET_TIME_PLOT
plot file("delete","time") @POINTS
unset title


@ROW2

set ylabel "retries"
set format y "%.0s %c"
@COL1
@SET_RETRIES_PLOT
plot file("insert","retries") @POINTS, num_tx ls 4 lt 0
unset ylabel
set format y ""

@COL2
@SET_RETRIES_PLOT
plot file("update","retries") @POINTS, num_tx ls 4 lt 0

@COL3
@SET_RETRIES_PLOT
plot file("lookup","retries") @POINTS, num_tx ls 4 lt 0

@COL4
@SET_RETRIES_PLOT
plot file("delete","retries") @POINTS, num_tx ls 4 lt 0


@ROW3

set ylabel "allocation"
set format y "%.0s %cB"
@COL1
@SET_ALLOC_PLOT
plot file("insert","alloc") @POINTS
unset ylabel
set format y ""

@COL2
@SET_ALLOC_PLOT
plot file("update","alloc") @POINTS

@COL3
@SET_ALLOC_PLOT
plot file("lookup","alloc") @POINTS

@COL4
@SET_ALLOC_PLOT
plot file("delete","alloc") @POINTS


unset multiplot
