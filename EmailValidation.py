import sys, re

atext = r"""[a-zA-Z0-9!#$%&'*+/=?^_`{}|~-]"""
atom = r'{atext}+'.format(atext=atext)
dot_atom_text = r'{atext}+(\.{atext}+)*'.format(atext=atext)
dot_atom = r'{dot_atom_text}'.format(dot_atom_text=dot_atom_text)
qtext = r"""[!"-\\[-}]"""
qcontent = qtext
fws = '(\\s*\r\n\\s+)'
ctext = r"""[!-'*-[]-~]"""

# this part is not really from the standard
comment = r'(\({ctext}*\)){{0,1}}'.format(ctext=ctext)
domain = r'[a-zA-Z0-9]([a-zA-Z0-9.-])*'
quoted_string = "{qcontent}*".format(qcontent=qcontent)

localpart = r'{comment}({dot_atom}|"{quoted_string}"|"{quoted_string}"{dot_atom}|{dot_atom}"{quoted_string}"){comment}'.format(comment=comment, dot_atom=dot_atom, quoted_string=quoted_string)
address = r'^{local}@{domain}$'.format(local=localpart, domain=domain)

VALID_EMAIL = re.compile(address)

def process_line(candidate):
    match = VALID_EMAIL.match(candidate)
    return "true" if match else "false"

def main():
    with open(sys.argv[1]) as f:
        for line in f:
            print(process_line(line.strip()))

if __name__ == '__main__':
    main()
