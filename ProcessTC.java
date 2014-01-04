import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class ProcessTC {
	private static String RSCRIPT = "Rscript";
	private static volatile ConcurrentLinkedQueue<String> tcFileAddr = new ConcurrentLinkedQueue<>();
	private static volatile ConcurrentLinkedQueue<String> tcFileNames = new ConcurrentLinkedQueue<>();
	private static volatile ConcurrentLinkedQueue<String> virtualMachines = new ConcurrentLinkedQueue<>();
	private static volatile String tcResultLocation;
	static ProcessTC pr = new ProcessTC();

	public static void main(String[] args) throws IOException, InterruptedException {
		if (args.length < 3)
			throw new RuntimeException("Wrong number of arguments!");

		String tcFolder = args[0];
		String virtualMachinesListFile = args[1];
		tcResultLocation = args[2];
		File tcs = new File(tcFolder);
		if (!tcs.exists())
			throw new RuntimeException(
					"Specified folder with TCs does not exist!");

		listFilesForFolder(tcs);

		BufferedReader br = new BufferedReader(new FileReader(
				virtualMachinesListFile));
		String line;
		while ((line = br.readLine()) != null) {
			virtualMachines.add(line);
		}
		br.close();
		System.out.println(tcFileAddr.size());
		System.out.println(tcFileNames.size());
		
		int cores = Runtime.getRuntime().availableProcessors();
		System.out.println("Number of available cores - " + cores);
		System.out.println("Number of available VMs - " + virtualMachines.size());
		int threads = cores < virtualMachines.size() ? cores : virtualMachines.size();
//		threads = 1;
		ExecutorService executor = Executors.newFixedThreadPool(threads);
		for (int i = 0; i < tcFileAddr.size() + threads; i++) {
			Runnable worker = pr.new TCThread(i);
			executor.execute(worker);
		}
		executor.shutdown();
		while (!executor.isTerminated()) {
		}
		System.out.println("Finished all threads");

	}

	private static void listFilesForFolder(final File folder) {
		for (final File fileEntry : folder.listFiles()) {
			if (fileEntry.isDirectory()) {
				listFilesForFolder(fileEntry);
			} else {
				tcFileAddr.add(fileEntry.getAbsolutePath());
				tcFileNames.add(fileEntry.getName());
			}
		}
	}

	private class TCThread implements Runnable {
		private int index;

		public TCThread(int i) {
			super();
			index = i;
		}

		@Override
		public void run() {
			String vm = "";
			String file;
			String name;
			try {
				synchronized (this) {
					vm = virtualMachines.poll();
					file = tcFileAddr.poll();
					name = tcFileNames.poll();
				}
				BufferedWriter writer = new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream("/home/roman/rWD/info/"
								+ name + "_info", true)));
				System.out.println("Starting file - " + file);
				String[] commands = { RSCRIPT, "--no-save", "--no-restore",
						"--slave", "--quiet", "process.r", vm, file,
						tcResultLocation };
				Runtime rt = Runtime.getRuntime();
				Process proc = rt.exec(commands);
//				proc.waitFor();

				InputStream is = proc.getInputStream();
				InputStreamReader isr = new InputStreamReader(is);
				BufferedReader br = new BufferedReader(isr);

				String line;
				int exit = -1;

				while ((line = br.readLine()) != null) {
					// Outputs your process execution

					writer.write(line + "\n");
					writer.flush();
					try {
						exit = proc.exitValue();
						if (exit == 0) {
							break;
						}
					} catch (IllegalThreadStateException t) {

					}
				}

				System.out.println("Finished file - " + file);
				virtualMachines.add(vm);

			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
