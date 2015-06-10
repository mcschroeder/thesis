file(op,type)=sprintf("5/%s-%s.dat",op,type)
num_tx=200000
num_threads=9

time_min=0.1 #0.1300471
time_max=2.629407
retries_min=1
retries_max=1134936
alloc_min=200e6 #252606664
alloc_max=2753618256

#----------------------------------------------------------------------

set terminal epslatex size 12cm,18cm

set style line 1 lc rgb "#e41a1c" lw 2 pt 1     # red
set style line 2 lc rgb "#377eb8" lw 2 pt 7     # blue
set style line 3 lc rgb "#4daf4a" lw 2 pt 5     # green
set style line 4 lc rgb "black" lw 2

set multiplot layout 3,1

set border 3
unset key
set key autotitle columnheader
unset key
set xrange [-1:num_threads]
set xtics 0,1,num_threads-1 nomirror
#set xtics add ("1" 0, "4" 2, "8" 4, "12" 6, "16" 8)
#set format x ""
set grid

#----------------------------------------------------------------------

set macros
POINTS = "u 4:xticlabels(1) ls 3, '' u 3 ls 2, '' u 2 ls 1"

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
                  set ytics (250e6, 500e6, 1e9, 2e9, 4e9); \
                  set ytics nomirror"

#----------------------------------------------------------------------

#set tmargin 1
set bmargin 3

set ylabel "time"
set format y "%.0s %cs"
@SET_TIME_PLOT
plot file("balanced","time") @POINTS
unset ylabel
set format y ""

set ylabel "retries"
set format y "%.0s %c"
@SET_RETRIES_PLOT
plot file("balanced","retries") @POINTS, num_tx ls 4 lt 0
unset ylabel
set format y ""

set ylabel "allocation"
set format y "%.0s %cB"
@SET_ALLOC_PLOT
plot file("balanced","alloc") @POINTS
unset ylabel
set format y ""


unset multiplot
