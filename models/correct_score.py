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
    return [[px[i]*py[j] for j in range(n)]
            for i in range(n)]

class Grid(list):

    def __init__(self, data):
        list.__init__(self, data)

    def sum(self, filterfn):
        return sum([self[i][j]                    
                    for i in range(len(self))
                    for j in range(len(self))
                    if filterfn(i, j)])
                    
    @property
    def home_win(self):
        return self.sum(lambda i, j: i > j)

    @property
    def away_win(self):
        return self.sum(lambda i, j: i < j)

    @property
    def draw(self):
        return self.sum(lambda i, j: i==j)

    @property
    def match_odds(self):
        return [self.home_win,
                self.away_win,
                self.draw]

    def correct_score(self, i, j):
        return self[i][j]

    def total_goals(self, overunder, strike):
        if overunder=="over":
            filterfn=lambda i, j: i+j > strike
        else:
            filterfn=lambda i, j: i+j < strike
        return self.sum(filterfn)

    def asian_handicap(self, homeaway, strike):
        if homeaway=="home":
            filterfn=lambda i, j: i-j > strike
        else:
            filterfn=lambda i, j: j-i > strike
        return self.sum(filterfn)

def solve_match_odds(prob, n):
    import numpy as np
    def errfn(m, target, n):
        grid=Grid(simulate_correct_score(mx=m[0], my=m[1], n=n))
        return np.sum(np.subtract(grid.match_odds, target)**2)
    from scipy import optimize
    return optimize.fmin(errfn, (1, 1), args=(prob, n))

if __name__=="__main__":
    Target, N = [0.5, 0.2, 0.3], 10    
    r=solve_match_odds(Target, N)
    grid=Grid(simulate_correct_score(r[0], r[1], N))
    print grid.match_odds
    print grid.total_goals("over", 2.5)
    print grid.total_goals("under", 2.5)
    print grid.asian_handicap("home", 1.5)
    print grid.asian_handicap("away", 1.5)
