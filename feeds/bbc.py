import lxml.html, urllib

def get_league_table(url):
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    teams=[]
    for row in doc.xpath("//table/tbody/tr"):
        name=row.xpath("td[@class='team-name']/a")[0]
        points=row.xpath("td[@class='points']")[0]
        gd=row.xpath("td[@class='goal-difference']")[0]
        played=row.xpath("td[@class='played']")[0]
        teams.append({"name": name.text,
                      "points": int(points.text),
                      "goal_difference": int(gd.text),
                      "played": int(played.text)})
    return teams

if __name__=="__main__":
    print get_football_table("http://polling.bbc.co.uk/sport/shared/football/league-tables/partial/competitions/118996114")
