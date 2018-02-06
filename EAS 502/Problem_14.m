%x = 1;
MatC = []
for x = 0:0.1:2
t1 = asin(x/sqrt(x^2+1^2));
t2 = asin(1/sqrt((2-x)^2+1));

MatA = [-cos(t2) -1	0	0	0	0	0	0	0	0	0	;
-sin(t2)	0	0	0	0	0	0	0	0	0	0	;
0	0	-1	0	0	0	0	0	0	0	0	;
0	1	0	-1	0	0	0	0	0	0	0	;
cos(t2)	0	0	0	-1	-sin(t1)	0	0	0	0	0	;
sin(t2)	0	1	0	0	-cos(t1)	0	0	0	0	0	;
0	0	0	1	0	-sin(t2)	0	1	0	0	0	;
0	0	0	0	0	-cos(t2)	-1	0	-1	0	0	;
0	0	0	0	-1	0	0	0	0	1	0	;
0	0	0	0	0	0	-1	0	0	0	1	;]

MatB = [0	;
-1	;
0	;
0	;
0	;
0	;
0	;
0	;
0	;
0	;
]
Force = MatA\MatB;

MatC = horzcat(MatC,Force)
end
plot(MatC)
legend('Fac','Fab','Fbc','Fdb','Fce','Fcd','Fde','Dx','Dy','Ex','Ey')

