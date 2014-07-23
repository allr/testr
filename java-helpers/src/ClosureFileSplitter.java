import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ClosureFileSplitter {

	static String dirName = "functions";
	static final String FILE_PREFIX = "func_";
	static final String SYS_SEP = System.getProperty("file.separator");
	static final String SEP_REP = "sep";
	static final String FUNC_NAME_PREFIX = "func: ";
	static final String PRIM_FUNC_PREFIX = "prim: ";
	static final String BODY_PREFIX = "body: ";
	static final String ARGS_PREFIX = "args: ";
	static final String RETV_PREFIX = "retn: ";
	static final String SYM_PREFIX = "symb: ";
	static final String VSYM_PREFIX = "vsym: ";

	static boolean discardCapture = false;
	static int totalFunctionCalls = 0;
	static int savedFunctionCalls = 0;

	public static void main(String[] args) throws IOException {
		BufferedReader reader = null;
		BufferedWriter writer = null;
		StringBuilder bufferGeneral = new StringBuilder();
		StringBuilder bufferArguments = new StringBuilder();

		String line;
		String funcName;
		boolean sameArguments = false;
		boolean newFunction = false;
		Set<String> functions = new HashSet<>();
		Map<String, Integer> funcFileCounter = new HashMap<>();
		Map<String, HashSet<String>> funcArguments = new HashMap<>();
		if (args.length != 1)
			dirName = args[1];
		createDir(dirName);
        line = "";
		System.out.println("Starting split");
		try {
			reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(args[0])));
			line = reader.readLine();
			while (line != null) {
				totalFunctionCalls++;
				savedFunctionCalls++;
				sameArguments = false;
				discardCapture = false;
				int fileIndex = 1;
				// System.out.println(line);
				checkCombined(line);
				
                while (line.startsWith("symb: ") || line.startsWith("vsym: ")) {
					bufferGeneral.append(line + '\n');
					line = reader.readLine();
				}
				String tmpLine = line.substring(line.indexOf(':') + 2,
						line.length());
                funcName = tmpLine.substring(6, tmpLine.length() - 1); // quote removal
				// check if already encountered this function

				if (functions.contains(funcName)) {
					fileIndex = funcFileCounter.get(funcName);
					File forSizeCheck = new File(dirName + SYS_SEP
							+ FILE_PREFIX + funcName + SYS_SEP + FILE_PREFIX
							+ funcName + "_" + fileIndex);
					if (forSizeCheck.length() > 500 * 1000 * 1000) { // 500 MB
																		// files
																		// size
						fileIndex++;
						funcFileCounter.put(funcName, fileIndex);
					}
				}
				bufferGeneral.append(line + "\n"); // to have the same
				line = reader.readLine();

				while (line.startsWith(PRIM_FUNC_PREFIX)) {
					// bufferGeneral.append(line + '\n');
					line = reader.readLine(); // just skip all PRIM for now
				}
				while (line.startsWith(BODY_PREFIX)) {
					bufferGeneral.append(line + "\n");
					line = reader.readLine();
				}
				while (line.startsWith(ARGS_PREFIX)) {
					bufferArguments.append(line + "\n");
					bufferGeneral.append(line + "\n");
					line = reader.readLine();
				}
				// check if already encountered this function
				if (!functions.contains(funcName)) {
					functions.add(funcName);
					if (!discardCapture)
						createDir(dirName + SYS_SEP + FILE_PREFIX + funcName);
					funcFileCounter.put(funcName, fileIndex);
					HashSet<String> arguments = new HashSet<>();
					arguments.add(bufferArguments.toString());
					funcArguments.put(funcName, arguments);
				} else {
					HashSet<String> arguments = funcArguments.get(funcName);
					if (arguments.contains(bufferArguments.toString())) {
						sameArguments = true;
					} else {
						arguments.add(bufferArguments.toString());
						funcArguments.put(funcName, arguments);
					}
				}
				// null check for end of file
				while (line != null && line.startsWith(RETV_PREFIX)) {
					bufferGeneral.append(line + "\n");
					line = reader.readLine();
				}
				bufferGeneral.append("\n"); // for now for simplicity there is a
											// line break between captures
				if (!discardCapture && !sameArguments) {
					writer = new BufferedWriter(new OutputStreamWriter(
							new FileOutputStream(dirName + SYS_SEP
									+ FILE_PREFIX + funcName + SYS_SEP
									+ FILE_PREFIX + funcName + "_" + fileIndex,
									true)));
					writer.write(bufferGeneral.toString());
					writer.flush();
					writer.close();
				} else {
					savedFunctionCalls--;
				}
				bufferArguments = new StringBuilder();
				bufferGeneral = new StringBuilder();
				line = reader.readLine();
			}
		} catch (NullPointerException e){
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
			reader.close();
		}
		System.out.println("Function calls processed - " + totalFunctionCalls);
		System.out.println("Function calls saved - " + savedFunctionCalls);
		System.out.println("Split finished");
	}

	private static void createDir(String dirName) {
		File theDir = new File(dirName);
		if (!theDir.exists()) {
			boolean result = theDir.mkdir();
			if (!result) {
				throw new RuntimeException("Could not create directory - "
						+ dirName + ". Please check previlages");
			}
		} else {
			System.err
					.println("File or directory with specified name already exists");
		}
	}

	private static void checkCombined(String line) {
		// System.out.println(line);
		if (line.contains("`")) {
			discardCapture = false; // for now
			// savedFunctionCalls--;
			// System.out.println("Discard true");
		}
	}
}
