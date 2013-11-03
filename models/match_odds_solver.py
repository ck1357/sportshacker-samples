"""
NB individual team errors are unlikely to converge on zero as H/A bias, draw parameters aren't optimised
"""

import math, random

HomeAwayBias=1.3
DrawMax=0.3
DrawCurvature=0.75

def simulate_1x2_probabilities(fixture, abilities):
    homeability=math.exp(abilities[fixture["home_team"]])
    awayability=math.exp(abilities[fixture["away_team"]])
    homeability*=HomeAwayBias
    awayability/=HomeAwayBias
    ratio=homeability/float(homeability+awayability)
    drawprob=DrawMax-DrawCurvature*(ratio-0.5)**2
    return [ratio*(1-drawprob),
            (1-ratio)*(1-drawprob),
            drawprob]

def calc_1x2_error(abilities, trainingset):
    probabilities=[simulate_1x2_probabilities(fixture=fixture, 
                                              abilities=abilities)
                      for fixture in trainingset]
    def calc_error(X, Y):
        return (sum([(x-y)**2 
                     for x, y in zip(X, Y)])/float(len(X)))**0.5
    errors=[calc_error(prob, fixture["probabilities"])
            for prob, fixture in zip(probabilities,
                                     trainingset)]
    return sum(errors)/float(len(trainingset))

"""
this solver is probably very inefficient; however it has the merit of keeping abilities within sensible bounds, something which you may have no control over with other optimisation methods (eg scipy.optimize.fmin)
"""

def solve_inefficiently(teams, trainingset, generations=1000, decay=2):
    abilities=dict([(team["name"], random.gauss(0, 1))
                    for team in teams])
    best=calc_1x2_error(trainingset=trainingset, 
                    abilities=abilities)
    for i in range(generations):
        decayfac=((generations-i)/float(generations))**decay
        teamname=teams[i%len(teams)]["name"]
        delta=random.gauss(0, 1)*decayfac
        abilities[teamname]+=delta
        # up
        err=calc_1x2_error(trainingset=trainingset, 
                           abilities=abilities)
        if err < best:
            best=err
            continue
        # down
        abilities[teamname]-=2*delta # NB -=2*
        err=calc_1x2_error(trainingset=trainingset, 
                           abilities=abilities)
        if err < best:
            best=err 
            continue
        # reset
        abilities[teamname]+=delta
    return (abilities, best)

def calc_ratings(teams, abilities):
    ratings=dict([(team["name"], 0)
                  for team in teams])
    denom=1/float(2*(len(teams)-1))
    for hometeam in teams:
        for awayteam in teams:
            if hometeam["name"]==awayteam["name"]:
                continue
            fixture={"home_team": hometeam["name"],
                     "away_team": awayteam["name"]}
            p=simulate_1x2_probabilities(fixture=fixture,
                                         abilities=abilities)
            ratings[hometeam["name"]]+=(3*p[0]+p[-1])*denom
            ratings[awayteam["name"]]+=(3*p[1]+p[-1])*denom
    return ratings

if __name__=="__main__":
    print "Fetching data"
    from feeds.football_data import get_results
    results=get_results("http://www.football-data.co.uk/mmz4281/1314/E0.csv")
    def filter_teams(results):
        names=[]
        for result in results:
            for attr in ["home_team", "away_team"]:
                if result[attr] not in names:
                    names.append(result[attr])
        return [{"name": name}
                for name in sorted(names)]
    teams=filter_teams(results)
    results=sorted(results, key=lambda x: x["date"])
    results.reverse()
    n=6*len(teams)/2 # last 6 weeks
    trainingset=results[:n]
    print "Solving"
    abilities, _ = solve_inefficiently(teams, trainingset)
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

