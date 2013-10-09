import lxml.html, json, re, urllib

def simulate_points(quotes, paths, draw_prob=0.3):
    import random
    simpoints=dict([(quote["name"], 
                     [quote["so_far"][0] 
                      for i in range(paths)])
                    for quote in quotes])
    ngames=2*(len(quotes)-1) 
    for quote in quotes:
        midprice=(quote["bid"]+quote["offer"])/float(2)
        currentpoints, played = quote["so_far"]
        toplay=ngames-played
        expectedpoints=(midprice-currentpoints)/float(toplay) 
        winprob=(expectedpoints-draw_prob)/float(3) 
        for i in range(paths):
            for j in range(toplay):
                q=random.random()
                if q < winprob:
                    simpoints[quote["name"]][i]+=3
                elif q < winprob+draw_prob:
                    simpoints[quote["name"]][i]+=1
    return [{"name": key,
             "simulated_points": value}
            for key, value in simpoints.items()]

def calc_position_probabilities(simpoints, paths):
    positionprob=dict([(team["name"], 
                        [0 for i in range(len(simpoints))])
                       for team in simpoints])
    for i in range(paths):
        sortedpoints=sorted([(team["name"], team["simulated_points"][i])
                             for team in simpoints],
                            key=lambda x: -x[-1])
        for j in range(len(simpoints)):
            name=sortedpoints[j][0]
            positionprob[name][j]+=1/float(paths)
    return [{"name": key,
             "position_probabilities": value}
            for key, value in positionprob.items()]

def generate_position_probabilities(url, paths):
    quotes=get_market_quotes(url)
    simpoints=simulate_points(quotes, paths)
    return calc_position_probabilities(simpoints, paths)

if __name__=="__main__":
    import feeds.sporting_index as spindex
    prices=spindex.get_prices("http://www.sportingindex.com/spread-betting/football-domestic/premier-league/mm4.uk.meeting.4191659/premier-league-points-2013-2014")
    paths=10000
    simpoints=simulate_points(prices, paths)
    print calc_position_probabilities(simpoints, paths)

