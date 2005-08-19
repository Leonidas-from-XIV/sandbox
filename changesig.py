#!/usr/bin/env python
# -*- encoding: latin-1 -*-

# we need network capabilities
import urllib, urllib2
# and the great BeautifulSoup
from BeautifulSoup import BeautifulSoup
# available here: http://www.crummy.com/software/BeautifulSoup/

def extract_variables(html):
    soup = BeautifulSoup(html)

    textareas = soup('textarea')
    inputs = soup('input')
    selects = soup('select')

    variables = {}

    for element in inputs:
        if element['name'] != 'reset':
            try:
                if element['type'] == 'radio':
                    #print element
                    if element['checked']:
                        #print '^^ Take this'
                        variables[element['name']] = element['value']
                else:
                    variables[element['name']] = element['value']
            except KeyError, e:
                pass

    for area in textareas:
        variables[area['name']] = area.string
        
    for select in selects:
        for poss in select.contents:
            try:
                if poss['selected']:
                    variables[select['name']] = poss['value']
            except KeyError, e:
                pass
            
    return variables

class SignatureChanger(object):
    """Object for changing the signatures on phpBB 2.0.x boards.
    
    It's quite easy to use:
    
    >>> sc = SignatureChanger('MyNick', 'MyPassword', 'http://forum.domain.tld/phpBB2/')
    >>> sc.change('Your new signature')
    """
    def __init__(self, user, password, forumurl):
        """Forumurl ending with / """
        
        # create the urls
        self.loginpage = forumurl + 'login.php'
        self.profilepage = forumurl + 'profile.php'
        self.profileshowpage = forumurl + 'profile.php?mode=editprofile'
    
        self.datacookie, self.sidcookie = self.get_cookie()
        self.returncookie = self.datacookie + "; " + self.sidcookie
        
        logindata = {'username' : user,
            'password' : password, 'autologin' : 'on',
            'redirect' : '', 'login' : 'Login'}
        self.loginparam = urllib.urlencode(logindata)
        
        # login
        self.login()
    
    def login(self):
        """Login on the server, make the cookie belong to an registered user"""
        req = urllib2.Request(self.loginpage, data=self.loginparam)
        req.add_header('Cookie', self.returncookie)
        http = urllib2.urlopen(req)
        http.close()
    
    def get_cookie(self):
        # open the loginpage
        http = urllib2.urlopen(self.loginpage)
        # read the contents of Set-Cookie
        httpcookies = http.headers['Set-Cookie']
        # close the connection
        http.close()
    
        # split the cookie-parts
        httpcookies = httpcookies.split()
        httpcookies = [cookie[:-1] for cookie in httpcookies if '_' in cookie]
        return httpcookies
        
    def change(self, newsig):
        """You should only call this function after __init__"""
        
        # open the page with the possible options
        req = urllib2.Request(self.profileshowpage)
        req.add_header('Cookie', self.returncookie)
        http = urllib2.urlopen(req)
        # read the contents
        html = http.read()
        http.close()
        
        # extract the POST-variables from the HTML
        vars = extract_variables(html)
        vars['signature'] = newsig
        confirmparam = urllib.urlencode(vars)
    
    
        req = urllib2.Request(self.profilepage, data=confirmparam)
        req.add_header('Cookie', self.returncookie)
    
        http = urllib2.urlopen(req)
        http.close()
    
    
if __name__ == '__main__':
    main()