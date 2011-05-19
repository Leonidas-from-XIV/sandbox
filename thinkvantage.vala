// Plays sounds using Canberra
//
// Compile it with:
// valac thinkvantage.vala --pkg libcanberra --pkg libcanberra-gtk --pkg gtk+-2.0 --pkg unique-1.0

// define our possible commands
enum ThinkVantage.Command {
	// ZERO is reserved, never used
	ZERO,
	QUIT
}

class ThinkVantage.Main : GLib.Object {
	public static int main(string[] args) {
		// initialize GTK+, so it does not complain on runtime
		// (canberra) or just segfault (unique)
		Gtk.init(ref args);

		// create an App object that supports the QUIT command
		var app = new Unique.App.with_commands(
			"net.xivilization.thinkvantage", null,
			"quit", Command.QUIT,
			null);

		if (app.is_running) {
			stdout.printf("Already running\n");
			stdout.printf("Sending signal for it to quit\n");
			app.send_message(Command.QUIT, null);
			// bailing out early
			return 1;
		}

		stdout.printf("Not running, starting\n");

		// define a handler for receiving signals
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

		// now we can configure canberra
		CanberraGtk.context_get().change_props(
			Canberra.PROP_APPLICATION_NAME, "thinkvantage",
			Canberra.PROP_APPLICATION_VERSION, "0.0.1",
			Canberra.PROP_APPLICATION_ID, "net.xivilization.thinkvantage",
			null);
		Canberra.Proplist proplist;
		Canberra.Proplist.create(out proplist);

		// select the file to play
		proplist.sets(Canberra.PROP_MEDIA_FILENAME, "bell.oga");

		// let Canberra play the file, calling the cb when done
		stdout.printf("You hear me?!?\n");
		CanberraGtk.context_get().play_full(1, proplist, (c, id, code) => {
			// playing done, we can close the program
			stdout.printf("Done\n");
			Idle.add(() => {
				stdout.printf("Closing\n");
				Gtk.main_quit();
				return false;
			});
		});

		// enter GTK+ loop, come back when done
		Gtk.main();
		return 0;
	}
}
