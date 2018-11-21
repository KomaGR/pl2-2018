def isPal3(x:str):
     xlist = list(x)
     ylist = list(x)
     ylist.reverse()
     return ylist == xlist

 # Dyo eis thn osa eixame prin

 # A[i,j] = A[i-1,j] + A[i,j-1] - A[i-1,j-1] +
 # (char(i) == char(j) ? A[i-1,j-1] + 1 : 0)
