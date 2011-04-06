import wnck, gtk
import hdaps


class EWMHSmack(hdaps.Smack):
    def __init__(self):
        hdaps.Smack.__init__(self)
        self.screen = wnck.screen_get_default()
        self._flush()
        self.current_ws = self.screen.get_active_workspace().get_number()
        self.workspace_count = self.screen.get_workspace_count()

    def hit_left(self):
        print "Hit left"
        new_workspace = (self.current_ws + 1) % self.workspace_count
        self.current_ws += 1
        self._switch(new_workspace)

    def hit_right(self):
        print "Hit right"
        new_workspace = (self.current_ws - 1) % self.workspace_count
        self.current_ws -= 1
        self._switch(new_workspace)

    def _switch(self, number):
        self.screen.get_workspace(number).activate(0)

    def _flush(self):
      while gtk.events_pending():
        gtk.main_iteration()

def main():
    emwh = EWMHSmack()
    try:
        emwh.loop()
    except KeyboardInterrupt:
        return

if __name__ == '__main__':
    main()
