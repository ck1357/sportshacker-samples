"""
http://people.maths.ox.ac.uk/siamstudentchapter/webpages/2011_Conference/Robert_Talk.pdf
"""

import math, random

BaseGoals=1.1
HomeAwayBias=1.3

def poisson(m, n):
    p=math.exp(-m)
    r=[p]
    for i in range(1, n):
        p*=m/float(i)
        r.append(p)
    return r

"""
two factor (attack, defence) model collapsed into one factor by using math.exp(ability) for attack, math.exp(-ability) for defence
so a team with a high attack factor will have a low defence factor by definition; this might not always be true empirically (eg Barcelona ?), but much easier to keep control of a one- factor model rather than a two- factor
"""

"""
should maybe calculate full outer- product, to simplify ratings calculation 
"""

def simulate_match(match, abilities):
    a0, a1, d0, d1 = (math.exp(abilities[match["home_team"]]),
                      math.exp(abilities[match["away_team"]]),
                      math.exp(-abilities[match["home_team"]]),
                      math.exp(-abilities[match["away_team"]]))
    v0, v1 = (poisson(BaseGoals*a0*d1*HomeAwayBias, 1+match["score"][0]),
              poisson(BaseGoals*a1*d0/HomeAwayBias, 1+match["score"][1]))
    return v0[-1]*v1[-1]

def simulate_matches(results, abilities):
    return sum([simulate_match(match, abilities)
               for match in results])

"""
this solver is probably very inefficient; however it has the merit of keeping abilities within sensible bounds, something which you may have no control over with other optimisation methods (eg scipy.optimize.fmin)
"""

def solve_inefficiently(teams, results, generations=5000, decay=2):
    abilities=dict([(team["name"], random.gauss(0, 1))
                    for team in teams])
    best=simulate_matches(results=results, 
                          abilities=abilities)
    for i in range(generations):
        if 0==i%100:
            print (i, best)
        decayfac=((generations-i)/float(generations))**decay
        teamname=teams[i%len(teams)]["name"]
        delta=random.gauss(0, 1)*decayfac
        abilities[teamname]+=delta
        # up
        resp=simulate_matches(results=results, 
                              abilities=abilities)
        if resp > best:
            best=resp
            continue
        # down
        abilities[teamname]-=2*delta # NB -=2*
        resp=simulate_matches(results=results, 
                              abilities=abilities)
        if resp > best:
            best=resp 
            continue
        # reset
        abilities[teamname]+=delta
    return (abilities, best)

"""
iterate across all 380 matches [ENG.0]
for each match, calculate correct score matrix
then calculate match odds from correct score grid; and calculate expected points given correct score grid
"""

def calc_ratings(teams, abilities):
    pass


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
    abilities, _ = solve_inefficiently(teams, results)
    def format_name(text, n=16):
        if len(text) < n:
            return text+" ".join(["" for i in range(n-len(text))])
        else:
            return text[:n]
    print
    for key, value in sorted([(key, value) 
                              for key, value in abilities.items()],
                             key=lambda x: -x[-1]):
        print "%s %.5f" % (format_name(key), value)
