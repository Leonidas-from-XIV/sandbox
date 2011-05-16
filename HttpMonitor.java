import java.net.Socket;
import java.io.IOException;

class HttpMonitor {
	private String host;
	private int port;
	private Socket conn;

	public HttpMonitor(String h, int p) {
		host = h;
		port = p;
	}

	public boolean checkOnline() {
		try {
			conn = new Socket(host, port);
			return true;
		}
		catch (IOException e) {
			return false;
		}
	}

	public static void main(String[] args) {
		String host = args[0];
		int port = Integer.parseInt(args[1]);
		HttpMonitor monitor = new HttpMonitor(host, port);

		if (monitor.checkOnline()) {
			System.out.println("Up");
		} else {
			System.out.println("Down");
		}
	}
}
