# -*- coding: cp1252 -*-
from Tkinter import * 
from thread import start_new_thread 
from whatsonair import * 
from os import remove 
from time import sleep
from tkMessageBox import showwarning

all=[] 
for current in allparsers: 
    current=current() 
    all.append(current.__station__) 

def htmlspecialchars(chars):
    chars=chars.replace('&auml;', 'ä')
    chars=chars.replace('&ouml;', 'ö')
    chars=chars.replace('&uuml;', 'ü')
    chars=chars.replace('&Auml;', 'Ä')
    chars=chars.replace('&Ouml;', 'Ö')
    chars=chars.replace('&OUuml;', 'Ü')
    chars=chars.replace('&szlig;', 'ß')
    return chars

def update_it(var, root, reloadtime): 
    while 1: 
        uptodate=update(read_sets()) 
        i=0 
        checksum=read_sets() 
        for current in uptodate:
            current=htmlspecialchars(str(current))
            if checksum[i]==1: 
                var[i].set(current) 
            i=i+1
        sleep(reloadtime)

def update(toupdate): 
    i=0 
    result=[] 
    for current in allparsers: 
        if toupdate[i]==1:
            try: 
                current=current() 
                current.feed(current.pagecontent) 
                result.append(current.currenttrack()) 
            except:result.append('ERROR!') 
        else: 
            result.append(0) 
        i=i+1 
    return result 

def save_settings(values, settingsroot, root, reloadtime): 
    try:remove('WhatsOnAir_TkSettings.ini') 
    except:pass 
    file=open('WhatsOnAir_TkSettings.ini', 'w+') 
    for current in values: 
        current=current.get() 
        file.write(current+'|#|')
    file.write('\n++##++'+reloadtime.get())
    file.close() 
    close_it(settingsroot)
    showwarning('Achtung!', 'Die Änderungen werden erst nach Neustart von SeeWhatsOnAir aktiv!')

def read_sets(): 
    file=open('WhatsOnAir_TkSettings.ini', 'r') 
    result=file.readline() 
    result=result.split('|#|')
    stations=[] 
    for current in result: 
        if current=='': 
            current=0
        if current=='\n':
            current=0
        stations.append(int(current)) 
    return stations

def read_reload():
    file=open('WhatsOnAir_TkSettings.ini', 'r')
    result=file.read()
    result=result.split('++##++')
    return int(result[1])

def close_it(window): 
    window.quit() 
    window.destroy()

def start(): 
    root=Tk() 
    root.title('SeeWhatsOnAir_Tk 1.1') 
    aa=0
    s=0
    uptodate=update(read_sets()) 
    tracklabels=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
    checksum=read_sets() 
    var=[StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root),
         StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root), StringVar(root)]
    for current in allparsers: 
        if checksum[aa]==1: 
            current=current() 
            current=current.__station__ 
            var.append(StringVar(root)) 
            Label(root, text=current).place(x=4, y=20*s) 
            tracklabels[aa]=Label(root, textvariable=var[aa]) 
            var[aa].set(uptodate[aa]) 
            tracklabels[aa].place(x=100, y=20*s)
            s=s+1
        aa=aa+1 
    start_new_thread(update_it, (var, root, read_reload()))
    root.minsize(450, 20*s) 
    #root.maxsize(450, 20*s) 
    menu = Menu(root) 
    root.config(menu=menu) 
    menu.add_command(label='Settings', command=lambda:settings(root))
    root.mainloop() 

def settings(*root): 
    settingsroot=Tk() 
    settingsroot.title('Settings') 
    r=0 
    stationchecks=[] 
    v=[]
    try: sets=read_sets()
    except:pass
    for current in allparsers: 
        current=current() 
        current=current.__station__ 
        Label(settingsroot, text=current).place(x=4, y=20*r) 
        v.append(StringVar(settingsroot)) 
        stationchecks.append(Checkbutton(settingsroot, variable=v[r])) 
        stationchecks[r].place(x=150, y=20*r)
        try:
            if sets[r]==1:
                stationchecks[r].select()
        except:pass
        r=r+1
    Label(settingsroot, text='Refresh time: ').place(x=4, y=20*r+10)
    reloadtime=Entry(settingsroot, width=4)
    reloadtime.place(x=90, y=20*r+10)
    reloadtime.insert(END, str(read_reload()))
    settingsroot.minsize(200, 20*r+75) 
    settingsroot.maxsize(200, 20*r+75) 
    Button(settingsroot, text="Save", command=lambda:save_settings(v, settingsroot, root, reloadtime)).place(x=50, y=(20*r)+40) 
    Button(settingsroot, text="Abort", command=lambda:close_it(settingsroot)).place(x=90, y=(20*r)+40) 
    settingsroot.mainloop() 
try:
    feil=open('WhatsOnAir_TkSettings.ini', 'r')
    feil.close()
except: settings()

start() 
