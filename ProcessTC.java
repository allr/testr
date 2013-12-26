import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class ProcessTC {
	private static String RSCRIPT = "Rscript";
	private static LinkedBlockingQueue<String> tcFileNames = new LinkedBlockingQueue<>();
	private static LinkedBlockingQueue<String> virtualMachines = new LinkedBlockingQueue<>();
	private static String tcResultLocation;
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
			virtualMachines.put(line);
		}
		br.close();

		ExecutorService executor = Executors.newFixedThreadPool(virtualMachines
				.size());
		for (int i = 0; i < tcFileNames.size(); i++) {
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
				tcFileNames.add(fileEntry.getAbsolutePath());
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
			try {
				vm = virtualMachines.take();

				file = tcFileNames.take();
				System.out.println("Starting file - " + file);
				String[] commands = { RSCRIPT, "--no-save", "--no-restore",
						"--slave", "--quiet",
						"process.r", vm, file,
						tcResultLocation };
				Runtime rt = Runtime.getRuntime();
				Process proc = rt.exec(commands);
				proc.waitFor();
				System.out.println("Finished file - " + file);
				virtualMachines.put(vm);
				
			} catch (IOException | InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
