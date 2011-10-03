#!/usr/bin/env python2
# -*- coding: utf8 -*-
# Python 2 for now, since Requests does not support Unicode yet
"""
This is a login script for the NUWnet online service. NUWnet requires you to
log in every time your IP changes using the browser, but this is inconvenient.
Therefore this is a script to automate the process. It does not 'hack' you into
the network, you still need a valid username and password. Be sure to store
them securely.
"""

from __future__ import unicode_literals, print_function
import sys
import urlparse
import json
import requests

headers = {
        'User-Agent' : 'NUWNetLogin/0.9.1'
    }

def main():
    with open('nuwnet-login.json') as configfile:
        config = json.load(configfile)
        username = config['username']
        password = config['password']

    # determine whether we are online, and what page to visit
    url = 'http://xivilization.net/'
    snoop = requests.get(url, headers=headers)
    if snoop.url is None:
        print('We seem to be disconnected. Cable plugged in?', file=sys.stderr)
        return
    if snoop.url == url:
        # we seem to be properly online, or something
        print('We seem to be online, bailing out', file=sys.stderr)
        return

    # get the data from the URL we were redirected to
    parsed_url = urlparse.urlparse(snoop.url)
    parsed_qs = urlparse.parse_qs(parsed_url.query)
    nasip = parsed_qs['NASip'][0]
    naspo = parsed_qs['NASpo'][0]

    # construct the URL for the stage1 login
    stage1_url = urlparse.urlunparse((parsed_url.scheme, parsed_url.netloc,
        '/nu-wnet2/process_in.php', '', '', ''))
    # pass NAS stuff and user/password to the server
    stage1 = requests.post(stage1_url, data={
                'NASip' : nasip,
                'NASpo' : naspo,
                'keepid' : 'off',
                'username' : username,
                'password' : password
                }, headers=headers)

    # stage1 login returns an URL to which to post more auth details
    new_url = stage1.content
    # parse the url we got and extract the auth information we were passed
    parsed_url = urlparse.urlparse(new_url)
    parsed_qs = urlparse.parse_qs(parsed_url.query)
    # create the new URL without the auth information
    stage2_url = urlparse.urlunparse((parsed_url.scheme, parsed_url.netloc,
        parsed_url.path, '', '', ''))
    # pass the new auth information to the server again
    # together with some fields that aren't filled in anyway
    # possibly we can just leave those out
    stage2 = requests.post(stage2_url, data={
        'username' : parsed_qs['username'][0],
        'password' : parsed_qs['password'][0],
        'NASip' : nasip,
        'NASpo' : naspo,
        'AP' : '',
        'error' : '',
        'ldapname' : '',
        'id' : '',
        'multi' : '',
        'admin' : '',
        'fullname' : '',
        'class' : '',
        'affiliation' : '',
        'occupation' : ''
        }, headers=headers)

    print('Logged in.', file=sys.stderr)

if __name__ == '__main__':
    main()
