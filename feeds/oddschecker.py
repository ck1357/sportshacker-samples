import lxml.html, re, urllib

def get_match_events(url):
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    fixtures=doc.xpath("//div[@id='fixtures']")[0]
    rows=fixtures.xpath("div/table/tbody/tr")
    events, date = [], None
    for row in rows:
        if ("class" in row.attrib and
            "date" in row.attrib["class"]):
            date=row.xpath("td[@class='day']/p")[0].text
        else:
            time=row.xpath("td[@class='time']/p")[0].text
            spans=row.xpath("td/p/span[@class='add-to-bet-basket']")
            hometeam, awayteam = (spans[0].attrib["data-name"], 
                                  spans[2].attrib["data-name"])
            link=row.xpath("td[@class='betting']/a")[0].attrib["href"]
            event={"kickoff": "%s %s" % (date, time),
                   "name": "%s vs %s" % (hometeam, awayteam),
                   "link": link}
            events.append(event)
    return events

def parse_fractional_quote(text):
    tokens=[int(tok) for tok in text.split("/")]
    if len(tokens)==1:
        tokens.append(1)
    return 100/(1+tokens[0]/float(tokens[1]))

def get_prices(url):
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    table=doc.xpath("//table[@class='eventTable ']")[0]
    rows=table.xpath("tbody/tr")
    items={}
    for row in rows:
        oddscheckerid=row.attrib["data-participant-id"]
        name=None
        for td in row.xpath("td"):
            if not "id" in td.attrib:
                continue
            suffix=td.attrib["id"].split("_")[-1]
            if suffix=="name":
                name=td.text
                continue
            if (suffix=="best" or
                td.text in [None, "SP"] or
                re.search("^\\d+\\-\\d+$", td.text)):
                continue
            items.setdefault(name, {"name": name,
                                    "bookmaker": None,
                                    "price": 1e10})
            price=parse_fractional_quote(td.text)
            if price < items[name]["price"]:
                items[name]["price"]=price
                items[name]["bookmaker"]=suffix
    return sorted(items.values(),
                  key=lambda x: -x["price"])

if __name__=="__main__":
    print get_match_events("http://www.oddschecker.com/football/english/premier-league")
    # print get_prices("http://www.oddschecker.com/football/english/premier-league/top-3-finish")
