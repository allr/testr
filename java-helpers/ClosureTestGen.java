import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.concurrent.LinkedBlockingQueue;

public class ClosureTestGen {

	static Path inputDir;
	static Path outputDir;
	static boolean verbose = false;
	static String TEST_GEN;
	static volatile boolean done = false;
    static final String FUNC_NAME_PREFIX = "func: ";
    static final String PRIM_FUNC_PREFIX = "prim: ";
    static final String BODY_PREFIX = "body: ";
    static final String ARGS_PREFIX = "args: ";
    static final String RETV_PREFIX = "retn: ";
    static final String SYM_PREFIX = "symb: ";
    static final String VSYM_PREFIX = "vsym: ";

	final static LinkedBlockingQueue<Path> ifileQueue = new LinkedBlockingQueue<Path>();

	public static void main(String[] args) {
		processArgument(args);
		setupWorkEnv();
		processFiles();
	}

	private static void processFiles() {
		if (verbose) {
			System.out.println("Read from: " + inputDir.toAbsolutePath());
			System.out.println("Output to: " + outputDir.toAbsolutePath());
		}
		try {
			if (!Files.isDirectory(inputDir)) {
				HashMap<String, HashSet<FuncEntry>> entryMap = processFile(inputDir);
				genTests(entryMap);
			} else {
				DirectoryStream<Path> seeds = Files
						.newDirectoryStream(inputDir);
				for (Path seed : seeds) {
					if (!Files.isDirectory(seed)) {
						HashMap<String, HashSet<FuncEntry>> entryMap = processFile(seed);
						genTests(entryMap);
					} else {
						DirectoryStream<Path> cseeds = Files
								.newDirectoryStream(seed);
						for (Path cseed : cseeds) {
							HashMap<String, HashSet<FuncEntry>> entryMap = processFile(cseed);
							genTests(entryMap);
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(0);
		}
		done = true;
	}

	private static void genTests(HashMap<String, HashSet<FuncEntry>> entryMap)
			throws IOException {
		Random random = new Random();
		String ifileName = Long.toString(System.currentTimeMillis()
				+ random.nextInt(10000000));
		Path ifile = Files.createFile(outputDir.resolve(ifileName));
		BufferedWriter writer = Files.newBufferedWriter(ifile,
				Charset.defaultCharset());
		for (HashSet<FuncEntry> entries : entryMap.values()) {
			for (FuncEntry entry : entries) {
                writer.write(entry.symb);
				writer.write(entry.func);
				writer.write('\n');
				writer.write(entry.body);
				// writer.write('\n');
				writer.write(entry.args);
		//		writer.write('\n');
				writer.write(entry.retn);
				writer.write('\n');
			}
		}
		writer.flush();
		writer.close();
		try {
			ifileQueue.put(ifile);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private static HashMap<String, HashSet<FuncEntry>> processFile(Path seed)
			throws IOException {
		if (verbose) {
			System.out.println(">>> processing file: " + seed.toAbsolutePath()
					+ "...");
		}
		List<String> lines = Files.readAllLines(seed, Charset.defaultCharset());
		HashMap<String, HashSet<FuncEntry>> entryMap = new HashMap<>();
		String[] linesArr = new String[lines.size()];
		lines.toArray(linesArr);
		assert (linesArr.length == 0);
		int[] s = { 0 };
		while (s[0] != linesArr.length) {
			FuncEntry entry = processEntry(linesArr, s);
			HashSet<FuncEntry> set = entryMap.get(entry.func);
			if (set == null) {
				set = new HashSet<>();
			}
			set.add(entry);
			entryMap.put(entry.func, set);
		}
		return entryMap;
	}

	private static FuncEntry processEntry(String[] lines, int[] ss) {
		int s = ss[0];
		FuncEntry entry = new FuncEntry();
		assert (s < lines.length);

        entry.symb = "";
        while (lines[s].startsWith(SYM_PREFIX) || lines[s].startsWith(VSYM_PREFIX)) {
            entry.symb += lines[s] + "\n";
            s++;
            assert (s < lines.length);
        }

        entry.func = lines[s++];
		assert (s < lines.length);
		entry.body = "";
		while (lines[s].startsWith(BODY_PREFIX)) {
			entry.body += lines[s] + "\n";
			s++;
			assert (s < lines.length);
		}
		entry.args = "";
		while (lines[s].startsWith(ARGS_PREFIX)) {
			entry.args += lines[s] + "\n";
			s++;
			assert (s < lines.length);
		}
		entry.retn = "";
		while (s < lines.length && lines[s].startsWith(RETV_PREFIX)) {
			entry.retn += lines[s] + "\n";
			s++;
		}
		s++;
		ss[0] = s;
		return entry;
	}

	private static void setupWorkEnv() {
		if (!Files.exists(inputDir, LinkOption.NOFOLLOW_LINKS)) {
			System.err.println("Cannot find input directory: " + inputDir);
			System.exit(0);
		}
		if (!Files.exists(outputDir, LinkOption.NOFOLLOW_LINKS)) {
			System.err.println("Cannot find output directory: " + outputDir);
			System.exit(0);
		}
		DateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd_HH_mm_ss");
		outputDir = outputDir.resolve(dateFormat.format(new Date()));
		try {
			Files.createDirectories(outputDir);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(0);
		}
		new Thread(new RscriptExecutor()).start();
	}

	static void processArgument(String[] args) {
		if (args.length < 3) {
			System.out
					.println("Usage: TestGen <input_dir> <output_dir> <testgen.r> <optional: true | false>");
			System.exit(0);
		}
		inputDir = FileSystems.getDefault().getPath(args[0]);
		outputDir = FileSystems.getDefault().getPath(args[1]);
		TEST_GEN = args[2];
		if (args.length > 3) {
			verbose = Boolean.valueOf(args[3]);
		}
	}

	static class FuncEntry {
		String func;
        String symb;
		String body;
		String args;
		String retn;

		@Override
		public int hashCode() {
			return symb.hashCode() ^ func.hashCode() ^ body.hashCode() ^ args.hashCode()
					^ retn.hashCode();
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof FuncEntry))
				return false;
			FuncEntry o1 = (FuncEntry) o;
			return o1.symb.equals(symb) && o1.func.equals(func) && o1.body.equals(body)
					&& o1.args.equals(args) && o1.retn.equals(retn);
		}

	}

	static class RscriptExecutor implements Runnable {
		public void run() {
			while (!ifileQueue.isEmpty() || !done) {
				try {
					process(ifileQueue.take());
				} catch (IOException | InterruptedException e) {
					e.printStackTrace();
				}
			}
		}

		void process(Path ifile) throws IOException {
			Process proc = null;
//		    throw new RuntimeException("Forced exit");
			System.out.println("Processing file - " + ifile.toString());
			proc = Runtime.getRuntime().exec(
					new String[] { "Rscript", TEST_GEN,
							outputDir.toAbsolutePath().toString(),
							ifile.toAbsolutePath().toString() });
			boolean doneWait = false;
			while (!doneWait) {
				try {
					doneWait = true;
					proc.waitFor();
				} catch (InterruptedException e) {
					doneWait = false;
				}
			}
			Files.delete(ifile);
		}
	}
}
