enum ThinkVantage.Command {
	ZERO,
	QUIT
}


class ThinkVantage.Main : GLib.Object {
	public static int main(string[] args) {
		Gtk.init(ref args);
		var app = new Unique.App.with_commands("net.xivilization.thinkvantage", null,
		"quit", Command.QUIT,
		null);

		if (app.is_running) {
			stdout.printf("Already running\n");
			stdout.printf("Sending signal to quit\n");
			app.send_message(Command.QUIT, null);
			return 1;
		} else {
			stdout.printf("Not running, starting\n");
			app.message_received.connect((command, message_data, time_) => {
				stdout.printf("Got data\n");
				if (command == Command.QUIT) {
					Idle.add(() => {
						Gtk.main_quit();
						return false;
					});
				}
				return 0;
			});
		}

		stdout.printf("You hear me?!?\n");
		CanberraGtk.context_get().change_props(
			Canberra.PROP_APPLICATION_NAME, "thinkvantage",
			Canberra.PROP_APPLICATION_VERSION, "0.0.1",
			Canberra.PROP_APPLICATION_ID, "net.xivilization.thinkvantage",
			null);
		Canberra.Proplist proplist;
		Canberra.Proplist.create(out proplist);

		proplist.sets(Canberra.PROP_MEDIA_FILENAME, "bell.oga");

		CanberraGtk.context_get().play_full(1, proplist, (c, id, code) => {
			stdout.printf("Done\n");
			Idle.add(() => {
				stdout.printf("Closing\n");
				//Gtk.main_quit();
				return false;
			});
		});
		Gtk.main();
		return 0;
	}
}
