import math, random

HomeAwayBias=1.3
DrawMax=0.3
DrawCurvature=0.75

def simulate_match(fixture, expabilities):
    homeability=expabilities[fixture["home_team"]]*HomeAwayBias
    awayability=expabilities[fixture["away_team"]]/HomeAwayBias
    ratio=homeability/float(homeability+awayability)
    drawprob=DrawMax-DrawCurvature*(ratio-0.5)**2
    return [ratio*(1-drawprob),
            (1-ratio)*(1-drawprob),
            drawprob]

def calc_error(trainingset, abilities):
    expabilities=dict([(key, math.exp(value))
                       for key, value in abilities.items()])
    errors=[sum([(x-y)**2 
                 for x, y in zip(simulate_match(fixture, 
                                                expabilities), 
                                 fixture["probabilities"])])/float(3)
            for fixture in trainingset]
    return (sum(errors)/float(len(trainingset)))**0.5

"""
this solver is probably very inefficient; however it does have the merits of keeping the abilities in sensible bounds
"""

def solve_inefficiently(teams, trainingset, generations=1000, decay=2):
    abilities=dict([(team["name"], random.gauss(0, 1))
                    for team in teams])
    best=calc_error(trainingset, abilities)
    for i in range(generations):
        factor=((generations-i)/float(generations))**decay
        teamname=teams[i%len(teams)]["name"]
        delta=random.gauss(0, 1)*factor
        abilities[teamname]+=delta
        # up
        err=calc_error(trainingset, abilities)
        if err < best:
            best=err
            continue
        # down
        abilities[teamname]-=2*delta
        err=calc_error(trainingset, abilities)
        if err < best:
            best=err 
            continue
        # reset
        abilities[teamname]+=delta
    return (abilities, best)

import json

SampleRequest=json.loads("""{"TrainingFixtures": [{"Date": "2013-04-14", "AwayTeam": "Sunderland", "HomeTeam": "Newcastle", "Probabilities": [0.5129607752620885, 0.21147823947319275, 0.2755609852647189]}, {"Date": "2013-04-14", "AwayTeam": "Man Utd", "HomeTeam": "Stoke", "Probabilities": [0.14109272404197226, 0.6125701220318249, 0.24633715392620292]}, {"Date": "2013-04-13", "AwayTeam": "Fulham", "HomeTeam": "Aston Villa", "Probabilities": [0.4261219661233819, 0.28487798210858833, 0.28900005176802973]}, {"Date": "2013-04-13", "AwayTeam": "Liverpool", "HomeTeam": "Reading", "Probabilities": [0.12378709918333401, 0.6707076813193568, 0.20550521949730924]}, {"Date": "2013-04-13", "AwayTeam": "QPR", "HomeTeam": "Everton", "Probabilities": [0.6679736441403039, 0.12302855683895265, 0.2089977990207434]}, {"Date": "2013-04-13", "AwayTeam": "Norwich", "HomeTeam": "Arsenal", "Probabilities": [0.7900947311632314, 0.06714321938755087, 0.1427620494492178]}, {"Date": "2013-04-13", "AwayTeam": "West Ham", "HomeTeam": "Southampton", "Probabilities": [0.5328565634056122, 0.2078040855148422, 0.25933935107954575]}, {"Date": "2013-04-08", "AwayTeam": "Man City", "HomeTeam": "Man Utd", "Probabilities": [0.4005852992167491, 0.3143367323450658, 0.28507796843818506]}, {"Date": "2013-04-07", "AwayTeam": "Fulham", "HomeTeam": "Newcastle", "Probabilities": [0.49408599367544553, 0.23448995426552266, 0.271424052059032]}, {"Date": "2013-04-07", "AwayTeam": "Sunderland", "HomeTeam": "Chelsea", "Probabilities": [0.7477243235513513, 0.08372445266432554, 0.16855122378432325]}, {"Date": "2013-04-07", "AwayTeam": "Everton", "HomeTeam": "Tottenham", "Probabilities": [0.4490976456736746, 0.26223917086274956, 0.2886631834635759]}, {"Date": "2013-04-07", "AwayTeam": "West Ham", "HomeTeam": "Liverpool", "Probabilities": [0.7441534053739515, 0.087389975940449, 0.16845661868559947]}, {"Date": "2013-04-07", "AwayTeam": "Wigan", "HomeTeam": "QPR", "Probabilities": [0.4066967068918961, 0.3118422767853023, 0.2814610163228016]}, {"Date": "2013-04-06", "AwayTeam": "Southampton", "HomeTeam": "Reading", "Probabilities": [0.27990180632889095, 0.4477594235672681, 0.2723387701038409]}, {"Date": "2013-04-06", "AwayTeam": "Aston Villa", "HomeTeam": "Stoke", "Probabilities": [0.45688456958399215, 0.25045538049181604, 0.29266004992419176]}, {"Date": "2013-04-06", "AwayTeam": "Swansea", "HomeTeam": "Norwich", "Probabilities": [0.37040723936029507, 0.33797384717369994, 0.2916189134660051]}, {"Date": "2013-04-06", "AwayTeam": "Arsenal", "HomeTeam": "West Brom", "Probabilities": [0.22809935845912646, 0.5161807414428818, 0.2557199000979918]}, {"Date": "2013-04-01", "AwayTeam": "QPR", "HomeTeam": "Fulham", "Probabilities": [0.47357493501982956, 0.2529113626306965, 0.273513702349474]}, {"Date": "2013-03-31", "AwayTeam": "Liverpool", "HomeTeam": "Aston Villa", "Probabilities": [0.18010636516204487, 0.5873251906434749, 0.23256844419448017]}, {"Date": "2013-03-30", "AwayTeam": "Reading", "HomeTeam": "Arsenal", "Probabilities": [0.7831753634012982, 0.06844022428501371, 0.14838441231368815]}, {"Date": "2013-03-30", "AwayTeam": "Norwich", "HomeTeam": "Wigan", "Probabilities": [0.5127025369689978, 0.21784565842261694, 0.26945180460838525]}, {"Date": "2013-03-30", "AwayTeam": "Chelsea", "HomeTeam": "Southampton", "Probabilities": [0.2787497116383176, 0.44437380895607287, 0.27687647940560955]}, {"Date": "2013-03-30", "AwayTeam": "Stoke", "HomeTeam": "Everton", "Probabilities": [0.5865822129397308, 0.15199591495749049, 0.2614218721027788]}, {"Date": "2013-03-30", "AwayTeam": "Tottenham", "HomeTeam": "Swansea", "Probabilities": [0.2978525226722354, 0.41897731964766116, 0.28317015768010356]}, {"Date": "2013-03-30", "AwayTeam": "Newcastle", "HomeTeam": "Man City", "Probabilities": [0.7488681924270639, 0.08038208415957857, 0.1707497234133575]}, {"Date": "2013-03-30", "AwayTeam": "Man Utd", "HomeTeam": "Sunderland", "Probabilities": [0.16099504839462517, 0.5816046550719481, 0.25740029653342666]}, {"Date": "2013-03-30", "AwayTeam": "West Brom", "HomeTeam": "West Ham", "Probabilities": [0.4487166611317841, 0.26578748333759417, 0.28549585553062173]}, {"Date": "2013-03-17", "AwayTeam": "Fulham", "HomeTeam": "Tottenham", "Probabilities": [0.6539616544348887, 0.12894940703405913, 0.21708893853105218]}, {"Date": "2013-03-17", "AwayTeam": "West Ham", "HomeTeam": "Chelsea", "Probabilities": [0.7198120136524615, 0.10107262933693328, 0.17911535701060516]}, {"Date": "2013-03-17", "AwayTeam": "Newcastle", "HomeTeam": "Wigan", "Probabilities": [0.43212295376513626, 0.28564167038616234, 0.28223537584870145]}, {"Date": "2013-03-16", "AwayTeam": "West Brom", "HomeTeam": "Stoke", "Probabilities": [0.3810821964716956, 0.31688811839869624, 0.30202968512960815]}, {"Date": "2013-03-16", "AwayTeam": "Arsenal", "HomeTeam": "Swansea", "Probabilities": [0.28956982982761575, 0.4349696143883443, 0.2754605557840399]}, {"Date": "2013-03-16", "AwayTeam": "Reading", "HomeTeam": "Man Utd", "Probabilities": [0.8566137940550506, 0.04400629065022381, 0.09937991529472562]}, {"Date": "2013-03-16", "AwayTeam": "Liverpool", "HomeTeam": "Southampton", "Probabilities": [0.23893448099634854, 0.5035179193832837, 0.2575475996203677]}, {"Date": "2013-03-16", "AwayTeam": "Man City", "HomeTeam": "Everton", "Probabilities": [0.25653427646654936, 0.46958170281520945, 0.27388402071824114]}, {"Date": "2013-03-16", "AwayTeam": "QPR", "HomeTeam": "Aston Villa", "Probabilities": [0.409984144725066, 0.300304477814487, 0.28971137746044695]}, {"Date": "2013-03-10", "AwayTeam": "Tottenham", "HomeTeam": "Liverpool", "Probabilities": [0.5214180496223182, 0.2346200999341033, 0.24396185044357852]}, {"Date": "2013-03-10", "AwayTeam": "Stoke", "HomeTeam": "Newcastle", "Probabilities": [0.5405415748695317, 0.18664974540026943, 0.2728086797301988]}, {"Date": "2013-03-09", "AwayTeam": "Swansea", "HomeTeam": "West Brom", "Probabilities": [0.40589901289751684, 0.3079179499051157, 0.28618303719736743]}, {"Date": "2013-03-09", "AwayTeam": "Southampton", "HomeTeam": "Norwich", "Probabilities": [0.3727515803196009, 0.3375261468925495, 0.2897222727878497]}, {"Date": "2013-03-09", "AwayTeam": "Sunderland", "HomeTeam": "QPR", "Probabilities": [0.42275747754633763, 0.28290988146005647, 0.29433264099360595]}, {"Date": "2013-03-04", "AwayTeam": "Man City", "HomeTeam": "Aston Villa", "Probabilities": [0.12409102968086673, 0.6690111104565108, 0.20689785986262255]}, {"Date": "2013-03-03", "AwayTeam": "Arsenal", "HomeTeam": "Tottenham", "Probabilities": [0.41388256109842864, 0.3082612525602925, 0.27785618634127895]}, {"Date": "2013-03-02", "AwayTeam": "West Ham", "HomeTeam": "Stoke", "Probabilities": [0.41665563470272193, 0.28432437609536076, 0.2990199892019173]}, {"Date": "2013-03-02", "AwayTeam": "Reading", "HomeTeam": "Everton", "Probabilities": [0.7082469108763009, 0.10268190487376491, 0.18907118424993427]}, {"Date": "2013-03-02", "AwayTeam": "Norwich", "HomeTeam": "Man Utd", "Probabilities": [0.7462522807807069, 0.08414608711202337, 0.16960163210726964]}, {"Date": "2013-03-02", "AwayTeam": "Liverpool", "HomeTeam": "Wigan", "Probabilities": [0.21017788917009853, 0.5449233443863961, 0.24489876644350542]}, {"Date": "2013-03-02", "AwayTeam": "Fulham", "HomeTeam": "Sunderland", "Probabilities": [0.4192456740280908, 0.285650914263361, 0.2951034117085482]}, {"Date": "2013-03-02", "AwayTeam": "West Brom", "HomeTeam": "Chelsea", "Probabilities": [0.695314722723712, 0.11233003763524649, 0.19235523964104162]}, {"Date": "2013-03-02", "AwayTeam": "Newcastle", "HomeTeam": "Swansea", "Probabilities": [0.4701894042510019, 0.2592221557881297, 0.27058843996086845]}, {"Date": "2013-02-25", "AwayTeam": "Tottenham", "HomeTeam": "West Ham", "Probabilities": [0.2760149140765332, 0.44177323866729873, 0.28221184725616816]}, {"Date": "2013-02-24", "AwayTeam": "Chelsea", "HomeTeam": "Man City", "Probabilities": [0.5012428550093193, 0.23871536183243897, 0.2600417831582416]}, {"Date": "2013-02-23", "AwayTeam": "Sunderland", "HomeTeam": "West Brom", "Probabilities": [0.5011295196999704, 0.22517254826740402, 0.27369793203262555]}, {"Date": "2013-02-23", "AwayTeam": "Stoke", "HomeTeam": "Fulham", "Probabilities": [0.4553777098229522, 0.2496571238301347, 0.294965166346913]}, {"Date": "2013-02-23", "AwayTeam": "Everton", "HomeTeam": "Norwich", "Probabilities": [0.2702264036588083, 0.44396175965600054, 0.2858118366851912]}, {"Date": "2013-02-23", "AwayTeam": "Aston Villa", "HomeTeam": "Arsenal", "Probabilities": [0.7377891023043383, 0.09128484500030488, 0.1709260526953568]}, {"Date": "2013-02-23", "AwayTeam": "Wigan", "HomeTeam": "Reading", "Probabilities": [0.37465020334322224, 0.34262155370570135, 0.28272824295107657]}, {"Date": "2013-02-23", "AwayTeam": "Man Utd", "HomeTeam": "QPR", "Probabilities": [0.1451773002092666, 0.6266259614341826, 0.22819673835655085]}, {"Date": "2013-02-17", "AwayTeam": "Swansea", "HomeTeam": "Liverpool", "Probabilities": [0.643112846445694, 0.14832925516300632, 0.2085578983912998]}, {"Date": "2013-02-09", "AwayTeam": "Man City", "HomeTeam": "Southampton", "Probabilities": [0.17121903642205485, 0.5936168272151514, 0.2351641363627937]}, {"Date": "2013-02-09", "AwayTeam": "Wigan", "HomeTeam": "Chelsea", "Probabilities": [0.7376556685255967, 0.09512939706310736, 0.16721493441129592]}], "Params": {"Seed": 13, "Generations": 5000, "Decay": 0.5}, "Teams": [{"GoalDifference": 29, "Points": 59, "Name": "Arsenal", "Played": 32}, {"GoalDifference": -24, "Points": 34, "Name": "Aston Villa", "Played": 33}, {"GoalDifference": 28, "Points": 58, "Name": "Chelsea", "Played": 31}, {"GoalDifference": 14, "Points": 55, "Name": "Everton", "Played": 32}, {"GoalDifference": -4, "Points": 40, "Name": "Fulham", "Played": 32}, {"GoalDifference": 19, "Points": 50, "Name": "Liverpool", "Played": 33}, {"GoalDifference": 30, "Points": 65, "Name": "Man City", "Played": 31}, {"GoalDifference": 40, "Points": 80, "Name": "Man Utd", "Played": 32}, {"GoalDifference": -17, "Points": 36, "Name": "Newcastle", "Played": 33}, {"GoalDifference": -21, "Points": 35, "Name": "Norwich", "Played": 33}, {"GoalDifference": -25, "Points": 24, "Name": "QPR", "Played": 33}, {"GoalDifference": -27, "Points": 24, "Name": "Reading", "Played": 33}, {"GoalDifference": -7, "Points": 38, "Name": "Southampton", "Played": 33}, {"GoalDifference": -13, "Points": 34, "Name": "Stoke", "Played": 33}, {"GoalDifference": -8, "Points": 34, "Name": "Sunderland", "Played": 33}, {"GoalDifference": 1, "Points": 41, "Name": "Swansea", "Played": 32}, {"GoalDifference": 15, "Points": 58, "Name": "Tottenham", "Played": 32}, {"GoalDifference": -1, "Points": 44, "Name": "West Brom", "Played": 32}, {"GoalDifference": -9, "Points": 38, "Name": "West Ham", "Played": 32}, {"GoalDifference": -20, "Points": 31, "Name": "Wigan", "Played": 31}]}""")

if __name__=="__main__":
    teams=sorted([{"name": team["Name"]}
                  for team in SampleRequest["Teams"]],
                 key=lambda x: x["name"])           
    trainingset=[{"home_team": fixture["HomeTeam"],
                  "away_team": fixture["AwayTeam"],
                  "probabilities": fixture["Probabilities"]}
                 for fixture in SampleRequest["TrainingFixtures"]]
    abilities, err = solve_inefficiently(teams, trainingset)
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
    print
    print "Error: %.5f" % err

