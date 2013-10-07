import lxml.html, re, urllib

def parse_quote(text):
    tokens=[int(tok) for tok in text.split("/")]
    if len(tokens)==1:
        tokens.append(1)
    return 100/(1+tokens[0]/float(tokens[1]))

def get_prices(url):
    doc=lxml.html.fromstring(urllib.urlopen(url).read())
    table=doc.xpath("//table[@class='eventTable ']")[0]
    rows=table.xpath("tbody/tr")
    selections=[]
    for row in rows:
        oddscheckerid=row.attrib["data-participant-id"]
        selectionname=None
        for td in row.xpath("td"):
            if not "id" in td.attrib:
                continue
            suffix=td.attrib["id"].split("_")[-1]
            if suffix=="name":
                selectionname=td.text
                continue
            if (suffix=="best" or
                td.text in [None, "SP"] or
                re.search("^\\d+\\-\\d+$", td.text)):
                continue
            selection={"name": selectionname,
                       "bookmaker": suffix,
                       "price": parse_quote(td.text)}
            selections.append(selection)
    return selections

if __name__=="__main__":
    print get_prices("http://www.oddschecker.com/football/english/premier-league/winner")
