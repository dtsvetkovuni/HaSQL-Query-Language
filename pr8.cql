show (p1,q3)
where 
"P"(p1,p2,p3,p4) & "Q"(q1,q2,q3,q4) &
p4 = q4 &
if (p1="") then (p1<-q1) &
if (p2="") then (p2<-q2) &
if (p3="") then (p3<-q3)

