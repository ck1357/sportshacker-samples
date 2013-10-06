"""
http://www.sportshacker.net/posts/season_points_simulation.html
"""

"""
script to simulate finishing position probabilities given season points data from Sporting Index
"""

import lxml.html, json, re, urllib

LivePricingUrl="http://livepricing.sportingindex.com/LivePricing.svc/jsonp/GetLivePricesByMeeting?meetingKey="

def get_market_quotes(url):        
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    ids=dict([(li.attrib["key"], re.sub(" Points$", "", li.xpath("span[@class='markets']")[0].text))
              for li in doc.xpath("//ul[@class='prices']/li")
              if "key" in li.attrib])
    quotes=json.loads(urllib.urlopen(LivePricingUrl+url.split("/")[-2]).read())
    return [{"name": ids[quote["Key"]],
             "so_far": tuple([int(tok) for tok in quote["SoFar"].split("/")]),
             "bid": quote["Sell"],
             "offer": quote["Buy"]}
            for quote in quotes["Markets"]]

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
    print generate_position_probabilities("http://www.sportingindex.com/spread-betting/football-domestic/premier-league/mm4.uk.meeting.4191659/premier-league-points-2013-2014", 10000)
