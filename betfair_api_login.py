import xml.dom.minidom as xml

XML=xml.Document()

def element(name, **attrs):
    el=XML.createElement(name)
    for name, value in attrs.items():
        el.setAttribute(name, value)
    return el

def subelement(parent, name, **attrs):
    el=element(name)
    for name, value in attrs.items():
        el.setAttribute(name, value)
    parent.appendChild(el)
    return el

def add_text(element, text):
    element.appendChild(XML.createTextNode(text))
    return element

def get_text(element):
    buf=[]
    for el in element.childNodes:
        if el.nodeType==el.TEXT_NODE:
            buf.append(el.data)
    return ''.join(buf)

def get_children(el, name):
    return el.getElementsByTagName(name)

def get_child(el, name):
    children=get_children(el, name)
    if children==[]:
        raise RuntimeError("No '%s' children" % name)
    return children[0]

def node_text(el, name):
    return get_text(get_child(el, name))

Global, Exchange = "global", "exchange"

APIEndpoints={
    "global": "/global/v3/BFGlobalService",
    "exchange": "/exchange/v5/BFExchangeService"
}

SOAPNamespaces={
    "xmlns:SOAP-ENV": "http://schemas.xmlsoap.org/soap/envelope/",
    "xmlns:SOAP-ENC": "http://schemas.xmlsoap.org/soap/encoding/",
    "xmlns:xsi": "http://www.w3.org/2001/XMLSchema-instance",
    "xmlns:xsd": "http://www.w3.org/2001/XMLSchema"
}

APINamespaces={
    "global": "https://api.betfair.com/global/v3/BFGlobalService",
    "exchange": "http://www.betfair.com/publicapi/v5/BFExchangeService"
}

def soap_decorator(ns, action):
    def wrap(fn):
        def wrapped_f(*args, **kwargs):
            env=element("SOAP-ENV:Envelope", **SOAPNamespaces)
            body=subelement(env, "SOAP-ENV:Body")
            actn=subelement(body, "m:%s" % action)
            actn.setAttribute("xmlns:m", APINamespaces[ns])
            actn.appendChild(fn(*args, **kwargs)) # result of decorated function is bound to envelope
            return env
        return wrapped_f
    return wrap

def init_request(token=None):
    req=element("m:request")
    if token:
        header=subelement(req, "header")
        for name, value in [("clientStamp", "0"),
                            ("sessionToken", token)]:
            child=subelement(header, name)
            add_text(child, value)
    return req

def assert_response_ok(fn):
    def wrapped_f(resp, *args, **kwargs):
        errcodes=[get_text(error) 
                  for error in get_children(resp, "errorCode")]
        for code in errcodes:
            if code!="OK":
                raise RuntimeError("Betfair returned '%s'" % code)
        return fn(resp, *args, **kwargs)
    return wrapped_f

def strip_response_result(fn):
    def wrapped_f(resp, *args, **kwargs):
        results=get_children(resp, "n:Result")
        return fn(results[0], *args, **kwargs)
    return wrapped_f

class BetfairAPITransport:

    Host="api.betfair.com"
    Port=443
    UserAgent="python27"

    def init_headers(self, action, payload):
        return {"SOAPAction": "urn:%s" % action,
                "Content-Type": "text/xml",
                "Content-Length": str(len(payload)),
                "User-Agent": self.UserAgent}  

    def fetch(self, path, action, request):
        payload=xml.Document().appendChild(request).toxml(encoding="utf-8")    
        # print payload
        headers=self.init_headers(action, payload)
        import httplib
        http=httplib.HTTPSConnection(self.Host, self.Port)
        http.request("POST", path, payload, headers)
        resp=http.getresponse()
        if resp.status!=200:
            raise RuntimeError("Betfair returned HTTP %i" % resp.status)
        body=resp.read()
        # print body
        return xml.parseString(body) 

def login(username, password, http):
    endpoint, action = Global, "login"
    @soap_decorator(endpoint, action)
    def create_request(username, password):
        req=init_request()
        for name, value in [("username", username),
                            ("password", password),
                            ("locationId", "0"),
                            ("vendorSoftwareId", "0"),
                            ("productId", "82")]: # NB Free API Product code
            el=subelement(req, name)
            add_text(el, value)
        return req
    @assert_response_ok
    @strip_response_result
    def handle_response(resp):
        return node_text(resp, "sessionToken")
    req=create_request(username, password)
    resp=http.fetch(APIEndpoints[endpoint], action, req)
    return handle_response(resp)

print login("woltrading", "zdrazvu1tye", BetfairAPITransport())
