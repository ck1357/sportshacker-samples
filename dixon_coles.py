"""
http://people.maths.ox.ac.uk/siamstudentchapter/webpages/2011_Conference/Robert_Talk.pdf
"""

"""
NB assumes correlation of -1 between attack, defence ratings
attack = math.abs(gauss)
defence = math.abs(-gauss)
ie that a good attacking team will be equivalently parsimonious in defence
of course this is not always true - eg Barcelona
abilities are not normalised because of this 'self- normalising' attack/defence relationship
"""

import json, math, random, yaml

SolverParams=yaml.load("""
seed: 13
generations: 1000
decay: 2.0
""")

class Abilities(dict):

    def initialise(self, teams):
        for team in teams:
            self[team["name"]]=random.gauss(0, 1)

    def copy(self):
        a=Abilities()
        for key, value in self.items():
            a[key]=value
        return a

    def normalise(self):
        mean=sum(self.values())/float(len(self))
        for key, value in self.items():
            self[key]=self[key]-mean
        return self

def poisson(m, n):
    p=math.exp(-m)
    r=[p]
    for i in range(1, n):
        p*=m/float(i)
        r.append(p)
    return r

BaseGoals=1.1
HomeAwayBias=1.33

def simulate_match(match, abilities):
    a0, a1, d0, d1 = (math.exp(abilities[match["home_team"]]),
                      math.exp(abilities[match["away_team"]]),
                      math.exp(-abilities[match["home_team"]]),
                      math.exp(-abilities[match["away_team"]]))
    v0, v1 = (poisson(BaseGoals*HomeAwayBias*a0*d1, match["home_goals"]),
              poisson(BaseGoals*a1*d0, match["away_goals"]))
    return v0[-1]*v1[-1]

def solve(params, teams, results):
    random.seed(params["seed"])
    def simulate(abilities, key, delta):
        ab2=abilities.copy()
        ab2[teamname]+=delta
        # ab2.normalise()
        lval=sum([simulate_match(match, ab2)
                for match in results])
        return (ab2, lval)
    abilities=Abilities()
    abilities.initialise(teams)
    # abilities.normalise()
    best=sum([simulate_match(match, abilities)
              for match in results])
    for i in range(params["generations"]):
        if 0==i%100:
            print (i, best)
        j=i%len(teams)
        teamname=teams[j]["name"]
        factor=((params["generations"]-i)/float(params["generations"]))**params["decay"]
        delta=random.gauss(0, 1)*factor
        # up
        ab2, lval = simulate(abilities, teamname, delta)
        if lval > best:
            abilities, best = ab2, lval
            continue
        # down
        ab2, lval = simulate(abilities, teamname, -delta)
        if lval > best:
            abilities, best = ab2, lval
            continue
    return (abilities, best)

if __name__=="__main__":
    results=json.loads(file("english_premiership_results.json").read())
    teams={}
    for result in results:
        for attr in ["home_team", "away_team"]:
            teams.setdefault(result[attr], None)
    teams=[{"name": teamname}
           for teamname in sorted(teams.keys())]
    abilities, _ = solve(SolverParams, teams, results)
    abilities=[(key, value) for key, value in abilities.items()]
    for item in sorted(abilities, key=lambda x:-x[1]):
        print item
    print
    print sum([item[-1] for item in abilities])
