A = [0	0	cos(36.869)	1	0	0	0	0	;
0	1	sin(36.869)	0	0	0	0	0	;
0	0	0	-1	0	1	0	0	;
0	0	0	0	1	0	0	0	;
0	0	-cos(216.9)	0	0	0	cos(323.1)	0	;
0	0	sin(216.9)	0	-1	0	sin(323.1)	0	;
0	0	0	0	0	-1	cos(143.1)	0	;
0	0	0	0	0	0	sin(143.1)	0	;
]

B =[ 0	;
0	;
0	;
0	;
0	;
0	;
0	;
1	;
]

[L U] = lu(A)

 Y = L\B
 
 X = U\Y
 
 







