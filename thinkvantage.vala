/**
 *  thinkvantage - Plays one sound file and terminate on second call
 *  Copyright (C) 2011  Marek Kubica
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

// Compile it with:
// valac thinkvantage.vala --pkg libcanberra --pkg libcanberra-gtk --pkg unique-1.0

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
		string filename = null;

		var oc = new GLib.OptionContext(" - thinkvantage");
		GLib.OptionEntry[] options = {
			OptionEntry() { long_name = "filename", short_name = 'f', flags=0, arg=GLib.OptionArg.FILENAME, arg_data=&filename, description="Bla", arg_description=null },
			OptionEntry() { long_name = null, short_name = 0, flags=0, arg=0, arg_data=null, description=null, arg_description=null }
		};

		Intl.setlocale(GLib.LocaleCategory.ALL, "");

		oc.add_main_entries(options, null);
		oc.add_group(Gtk.get_option_group(true));
		oc.set_help_enabled(true);
		try {
			oc.parse(ref args);
		} catch (GLib.OptionError e) {
			stdout.printf("Error parsing argument: %s\n",
				e.message);
			return 1;
		}

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

		if (filename == null) {
			stdout.printf("No file specified\n");
			return 1;
		}

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
			Canberra.PROP_APPLICATION_VERSION, "0.1.0",
			Canberra.PROP_APPLICATION_ID, "net.xivilization.thinkvantage",
			null);
		Canberra.Proplist proplist;
		Canberra.Proplist.create(out proplist);

		// select the file to play
		proplist.sets(Canberra.PROP_MEDIA_FILENAME, filename);

		// let Canberra play the file, calling the cb when done
		stdout.printf("You hear me?!?\n");
		var result = CanberraGtk.context_get().play_full(1, proplist, (c, id, code) => {
			// playing done, we can close the program
			stdout.printf("Done\n");
			Idle.add(() => {
				stdout.printf("Closing\n");
				Gtk.main_quit();
				return false;
			});
		});
		if (result < 0) {
			stdout.printf("Failed\n");
			return 1;
		}

		// enter GTK+ loop, come back when done
		Gtk.main();
		return 0;
	}
}
