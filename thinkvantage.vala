class ThinkVantage.Main : GLib.Object {
	//public static void callback(Canberra.Context ctx, uint32 id, int code) {
	//	stdout.printf("Exiting");
	//}
	public static int main(string[] args) {
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
				Gtk.main_quit();
				return false;
			});
		});
		Gtk.main();
		return 0;
	}
}
