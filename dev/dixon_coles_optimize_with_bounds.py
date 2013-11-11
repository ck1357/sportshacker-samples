"""
try and replace solve_inefficiently with bounded scipy.optimize.fmin
"""

"""
http://people.maths.ox.ac.uk/siamstudentchapter/webpages/2011_Conference/Robert_Talk.pdf
"""

import math, random

import numpy as np

BaseGoals=1.1
HomeAwayBias=1.3

def poisson(m, n):
    p=math.exp(-m)
    r=[p]
    for i in range(1, n):
        p*=m/float(i)
        r.append(p)
    return r

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

"""
two factor (attack, defence) model collapsed into one factor by using math.exp(ability) for attack, math.exp(-ability) for defence
so a team with a high attack factor will have a low defence factor by definition; this might not always be true empirically (eg Barcelona ?), but much easier to keep control of a one- factor model rather than a two- factor
"""

def generate_poisson_means(match, abilities):
    a0, a1, d0, d1 = (math.exp(abilities[match["home_team"]]),
                      math.exp(abilities[match["away_team"]]),
                      math.exp(-abilities[match["home_team"]]),
                      math.exp(-abilities[match["away_team"]]))
    m0, m1 = (BaseGoals*a0*d1*HomeAwayBias,
              BaseGoals*a1*d0/HomeAwayBias)
    return (m0, m1)

def calc_likelihood(results, abilities):
    total=0
    for match in results:
        m0, m1 = generate_poisson_means(match, abilities)
        hg, ag = match["score"]
        v0, v1 = (poisson(m0, hg+1),
                  poisson(m1, ag+1))
        total+=v0[hg]*v1[ag]
    return total

"""
this solver is probably very inefficient; however it has the merit of keeping abilities within sensible bounds, something which you may have no control over with other optimisation methods (eg scipy.optimize.fmin)
"""

def solve_inefficiently(teams, results, generations=1000, decay=2):
    abilities=dict([(team["name"], random.gauss(0, 1))
                    for team in teams])
    best=calc_likelihood(results=results, 
                         abilities=abilities)
    for i in range(generations):
        if 0==i%100:
            print (i, best)
        decayfac=((generations-i)/float(generations))**decay
        teamname=teams[i%len(teams)]["name"]
        delta=random.gauss(0, 1)*decayfac
        abilities[teamname]+=delta
        # up
        resp=calc_likelihood(results=results, 
                             abilities=abilities)
        if resp > best:
            best=resp
            continue
        # down
        abilities[teamname]-=2*delta # NB -=2*
        resp=calc_likelihood(results=results, 
                             abilities=abilities)
        if resp > best:
            best=resp 
            continue
        # reset
        abilities[teamname]+=delta
    return (abilities, best)

"""
http://stackoverflow.com/questions/11245869/using-scipy-optimize-fmin-cg-to-extract-distribution-parameters
"""

def solve_scipy(teams, results):
    teamnames=sorted([team["name"] for team in teams])
    def errfn(args):
        abilities=dict([(key, value)
                        for key, value in zip(teamnames, args)])
        return calc_likelihood(abilities=abilities,
                               results=results)
    from scipy import optimize
    resp=optimize.fmin_l_bfgs_b(errfn, 
                                x0=[0 for i in range(len(teams))],
                                bounds=[(-20, 20)
                                        for i in range(len(teams))],
                                approx_grad=True)
    x, f, d = resp
    abilities=dict([(key, value)
                    for key, value in zip(teamnames, x)])
    print abilities
    return (abilities, f)

def calc_ratings(teams, abilities, n=10):
    ratings=dict([(team["name"], 0)
                  for team in teams])
    denom=1/float(2*(len(teams)-1))
    for hometeam in teams:
        for awayteam in teams:
            if hometeam["name"]==awayteam["name"]:
                continue
            fixture={"home_team": hometeam["name"],
                     "away_team": awayteam["name"]}
            m0, m1 = generate_poisson_means(fixture, abilities)
            v0, v1 = (poisson(m0, n),
                      poisson(m1, n))
            grid=Grid(np.outer(v0, v1))
            p=grid.match_odds
            ratings[hometeam["name"]]+=(3*p[0]+p[-1])*denom
            ratings[awayteam["name"]]+=(3*p[1]+p[-1])*denom
    return ratings

if __name__=="__main__":
    from feeds.football_data import get_results
    results=get_results("http://www.football-data.co.uk/mmz4281/1213/E0.csv")
    def filter_teams(results):
        names=[]
        for result in results:
            for attr in ["home_team", "away_team"]:
                if result[attr] not in names:
                    names.append(result[attr])
        return [{"name": name}
                for name in sorted(names)]
    teams=filter_teams(results)
    print "Solving"
    # abilities, _ = solve_inefficiently(teams, results)
    abilities, _ = solve_scipy(teams, results)
    ratings=calc_ratings(teams, abilities)
    def format_name(text, n=16):
        if len(text) < n:
            return text+" ".join(["" for i in range(n-len(text))])
        else:
            return text[:n]
    print
    for key, value in sorted([(key, value) 
                              for key, value in ratings.items()],
                             key=lambda x: -x[-1]):
        print "%s %.5f" % (format_name(key), value)
