# -*- coding: cp1252 -*-
from Tkinter import * 
from thread import start_new_thread 
from whatsonair import * 
from os import remove 
from time import sleep, time, ctime
from tkMessageBox import showwarning
from ScrolledText import ScrolledText
from os import popen

all=[] 
for current in allparsers:
    ##Makes a list of all working stations##
    all.append(current.__station__)

def htmlspecialchars(chars):
    ##replaces all HTML special chars like ÄÖÜ and so on##
    chars=chars.replace('&auml;', 'ä')
    chars=chars.replace('&ouml;', 'ö')
    chars=chars.replace('&uuml;', 'ü')
    chars=chars.replace('&Auml;', 'Ä')
    chars=chars.replace('&Ouml;', 'Ö')
    chars=chars.replace('&Uuml;', 'Ü')
    chars=chars.replace('&szlig;', 'ß')
    chars=chars.replace('&amp;', '&')
    chars=chars.replace('&nbsp;', ' ')
    return chars

def create_default_ini():
    file=open('WhatsOnAir_TkSettings.ini', 'a+')
    towrite=''
    for current in all:
        towrite=towrite+'|#|1'
    towrite=towrite+'\n++##++10'
    towrite=towrite+'\n;log;0'
    file.write(towrite)
    file.close()

def mkdir(dir, target):
    print 'md '+target+dir
    popen('md '+target+dir)

def chkdir(dir, target):
    pipe=popen('dir '+target)
    result=pipe.read()
    result=result.find(dir)
    if result >0:
        return 1
    else:
        return 0

def log(station, track):
    ##Logs the Current track##
    if(track !=0):
        if chkdir('logs', '')==0:
            mkdir('logs', '')
        else:pass
        if chkdir(station, 'logs\\')==0:
            mkdir(station, 'logs\\')
        else: pass
        writefile=open('logs/'+station+'/'+ctime(time()).split(' ')[1]+'_'+ctime(time()).split(' ')[2]+'_'+ctime(time()).split(' ')[4]+'_'+station+'.log', 'a+')
        readfile=open('logs/'+station+'/'+ctime(time()).split(' ')[1]+'_'+ctime(time()).split(' ')[2]+'_'+ctime(time()).split(' ')[4]+'_'+station+'.log', 'r')
        track=htmlspecialchars(track)
        
    try:
        a=readfile.read()
        a=a.split('\n')[-2]
        a=a.split(' | ')[1]
        if (a!=track and track !='No title info currently' and track !='ERROR!'
            and track !=0):
            towrite=ctime(time()).split(' ')[3]+' | '+track+'\n'
            writefile.write(towrite)
        else:
            pass
    except:
        if (track!=0 and track !='No title info currently' and track !='ERROR!'):
            towrite=ctime(time()).split(' ')[3]+' | '+track+'\n'
            writefile.write(towrite)
        else:
            pass
    try:
        writefile.close()
        readfile.close()
    except:pass
        
def openlogs(station, *root):
    try:
        readfile=open('logs/'+station+'/'+ctime(time()).split(' ')[1]+'_'+ctime(time()).split(' ')[2]+'_'+ctime(time()).split(' ')[4]+'_'+station+'.log', 'r')
        result=readfile.read()
        logroot=Tk()
        logroot.title(station)
        TrackBox=ScrolledText(logroot, width=80, height=15)
        Label(logroot, text=station).place(x=250, y=0)
        TrackBox.place(x=0, y=20)
        result=result.split('\n')
        for current in result:
            current=htmlspecialchars(current)
            if current !='\n':
                TrackBox.insert(END, current+'\n')
        logroot.minsize(530, 240)
        logroot.maxsize(530, 240)
        logroot.mainloop()
        readfile.close()
    except:pass

def update_it(var, root, reloadtime): 
    while 1: 
        uptodate=update(read_sets()) 
        i=0 
        checksum=read_sets() 
        for current in uptodate:
            if set_read_if_log()==1:
                log(all[i], current)
            current=htmlspecialchars(str(current))
            if checksum[i]==1: 
                var[i].set(current) 
            i=i+1
        sleep(reloadtime)

def update(toupdate): 
    i=0 
    result=[] 
    for current in allparsers: 
        try:
            if toupdate[i] == 1:
                try: 
                    current=current() 
                    current.feed(current.pagecontent) 
                    result.append(current.currenttrack()) 
                except:
                    result.append('ERROR!') 
            else: 
                result.append(0)
            i += 1 
        except:
            pass
    return result 

def save_settings(values, settingsroot, root, reloadtime, logging): 
    try:remove('WhatsOnAir_TkSettings.ini') 
    except:pass 
    file=open('WhatsOnAir_TkSettings.ini', 'w+') 
    for current in values: 
        current=current.get() 
        file.write(current+'|#|')
    file.write('\n++##++'+reloadtime.get())
    file.write('\n;log;'+logging.get())
    file.close() 
    close_it(settingsroot)
    showwarning('Achtung!', 'Die Änderungen werden erst nach Neustart von SeeWhatsOnAir aktiv!')

def read_sets():
    try:
        file=open('WhatsOnAir_TkSettings.ini', 'r') 
        result=file.readline() 
        result=result.split('|#|')
        stations=[] 
        for current in result: 
            if current=='': 
                current=0
            if current=='\n':
                current=0
            if current == '#':
                continue
            stations.append(int(current)) 
        return stations
    except: create_default_ini()

def set_read_if_log():
    try:
        file=open('WhatsOnAir_TkSettings.ini', 'r')
        result=file.read()
        result=result.split('\n')
        result=result[2].split(';log;')
        return int(result[1])
    except: pass

def read_reload():
    try:
        file=open('WhatsOnAir_TkSettings.ini', 'r')
        result=file.read()
        result=result.split('++##++')
        result=result[1].split('\n')
        return int(result[0])
    except: pass

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
    menu = Menu(root)
    logmenu=Menu(root)
    root.config(menu=menu)
    menu.add_command(label='Settings', command=lambda:settings(root))
    logmenu = Menu(menu) 
    menu.add_cascade(label='Logs', menu=logmenu) 
    for current in allparsers:
        try:
            if checksum[aa]==1: 
                current=current() 
                current=current.__station__ 
                var.append(StringVar(root))
                Label(root, text=current).place(x=4, y=23*s)
                Button(text=all[aa], command=lambda arg=all[aa]:openlogs(arg, root)).place(x=4, y=23*s)
                logmenu.add_command(label=current, command=lambda arg=all[aa]:openlogs(arg, root)) 
                tracklabels[aa]=Label(root, textvariable=var[aa]) 
                var[aa].set(uptodate[aa]) 
                tracklabels[aa].place(x=100, y=23*s)
                s += 1
            aa += 1 
        except:
            pass
    start_new_thread(update_it, (var, root, read_reload()))
    root.minsize(450, 23*s) 
    root.maxsize(450, 23*s) 
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
        try:
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
        except:pass
        r=r+1
    Label(settingsroot, text='Refresh time: ').place(x=4, y=20*r+10)
    reloadtime=Entry(settingsroot, width=4)
    reloadtime.place(x=90, y=20*r+10)
    Label(settingsroot, text='seconds').place(x=115, y=20*r+10)
    try:reloadtime.insert(END, str(read_reload()))
    except:reloadtime.insert(END, '5')
    logvalue=StringVar(settingsroot)
    logcheck=Checkbutton(settingsroot, variable=logvalue)
    Label(settingsroot, text='Logging').place(x=4, y=20*r+35)
    logcheck.place(x=90, y=20*r+35)
    if set_read_if_log()==1:
        logcheck.select()
    settingsroot.minsize(200, 20*r+95) 
    settingsroot.maxsize(200, 20*r+95) 
    Button(settingsroot, text="Save", command=lambda:save_settings(v, settingsroot, root, reloadtime, logvalue)).place(x=50, y=(20*r+15)+40) 
    Button(settingsroot, text="Abort", command=lambda:close_it(settingsroot)).place(x=90, y=(20*r+15)+40) 
    settingsroot.mainloop()


start() 
