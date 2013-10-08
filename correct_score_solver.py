"""
script to solve for match odds prices assuming 2d Poisson distribution
"""

import math, random

def poisson(m, n):
    p=math.exp(-m)
    r=[p]
    for i in range(1, n):
        p*=m/float(i)
        r.append(p)
    return r

def simulate_correct_score(mx, my, n):
    r=[]
    px, py = (poisson(mx, n), 
              poisson(my, n))
    for i in range(n):
        r.append([])
        for j in range(n):
            r[i].append(px[i]*py[j])
    return r

def simulate_match_odds(mx, my, n):
    r=[0.0 for i in range(3)]
    px, py = (poisson(mx, n), 
              poisson(my, n))
    for i in range(n):
        for j in range(n):
            if i > j:
                k=0
            elif i < j:
                k=1
            else:
                k=2
            r[k]+=px[i]*py[j]
    return r

def solve_match_odds(p, n):
    import numpy as np
    def errfn(p, target, n):
        r=simulate_match_odds(p[0], p[1], n)
        return np.sum(np.subtract(target, r)**2)    
    from scipy import optimize
    return optimize.fmin(errfn, (0, 0), args=(p, n))

if __name__=="__main__":
    Target, N = [0.5, 0.2, 0.3], 10    
    r=solve_match_odds(Target, N)
    print r
    # print simulate_correct_score(r[0], r[1], N)
    print simulate_match_odds(r[0], r[1], N)
