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

public class TraceFileSplitter {

	static String dirName = "functions";
	static final String prefix = "func_";
	static final String SYS_SEP = System.getProperty("file.separator");
	static final String SEP_REP = "sep";

	public static void main(String[] args) throws IOException {
		BufferedReader reader = null;
		BufferedWriter writer = null;
		StringBuilder bufferGeneral = new StringBuilder();
		StringBuilder bufferArguments = new StringBuilder();

		String line;
		int functionCalls = 0;
		String funcName;
		boolean sameArguments = false;

		Set<String> functions = new HashSet<>();
		Map<String, Integer> funcFileCounter = new HashMap<>();
		Map<String, HashSet<String>> funcArguments = new HashMap<>();
		if (args.length != 1) dirName = args[1];
		createDir(dirName);

		System.out.println("Starting split");
		try {
			reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0])));
			line = reader.readLine();
			while (line != null) {
				functionCalls++;
				sameArguments = false;
//				System.out.println(line);
				if (!line.startsWith("func:")) {
					System.err.println(line);
					System.exit(1);
				}
				funcName = line.substring(line.indexOf(':') + 2, line.length());
				funcName = funcName.replaceAll(SYS_SEP, SEP_REP);

				if (!functions.contains(funcName)) {
					// create dir and file
					functions.add(funcName);
					createDir(dirName + "/func_" + funcName);
					funcFileCounter.put(funcName, 1);
					
					writer = new BufferedWriter(new OutputStreamWriter(
							new FileOutputStream(dirName + "/" + prefix + funcName
									+ "/" + prefix + funcName + "_" + 1)));
					bufferGeneral.append(line + "\n");
					line = reader.readLine();
					while (line.startsWith("type:")) {
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}
					while (line.startsWith("args:")) {
						bufferArguments.append(line + '\n');
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}
					HashSet<String> arguments = new HashSet<>();
					arguments.add(bufferArguments.toString());
					funcArguments.put(funcName, arguments);
					bufferArguments = new StringBuilder();

					while (line != null && line.startsWith("retn:")) {
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}
					writer.write(bufferGeneral.toString());
					bufferGeneral = new StringBuilder();
					writer.flush();
					writer.close();

				} else {
					int index = funcFileCounter.get(funcName);
					File forSizeCheck = new File(dirName + "/" + prefix + funcName
							+ "/" + prefix + funcName + "_" + index);
					if (forSizeCheck.length() > 500 * 1000 * 1000){
						index++;
						funcFileCounter.put(funcName, index);
					}
					writer = new BufferedWriter(new OutputStreamWriter(
							new FileOutputStream(dirName + "/" + prefix + funcName
									+ "/" + prefix + funcName + "_" + index, true)));
					bufferGeneral.append(line + "\n");
					line = reader.readLine();
					while (line.startsWith("type:")) {
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}

					while (line.startsWith("args:")) {
						bufferArguments.append(line + '\n');
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}

					HashSet<String> arguments = funcArguments.get(funcName);
					if (arguments.contains(bufferArguments.toString())) {
						sameArguments = true;
					}
					arguments.add(bufferArguments.toString());
					funcArguments.put(funcName, arguments);
					while (line != null && line.startsWith("retn:")) {
						bufferGeneral.append(line + '\n');
						line = reader.readLine();
					}

					if (!sameArguments) {
						writer.write(bufferGeneral.toString());
					}
					bufferArguments = new StringBuilder();
					bufferGeneral = new StringBuilder();
					writer.flush();
					writer.close();
				}
			}
			 } catch (Exception e) {
			 System.err.println(e);

		} finally {
			reader.close();
		}
		System.out.println("Function calls processed - " + functionCalls);
		System.out.println("Split finished");
	}

	private static void createDir(String dirName) {
		File theDir = new File(dirName);

		// if the directory does not exist, create it
		if (!theDir.exists()) {
	//		System.out.println("creating directory: " + dirName);
			boolean result = theDir.mkdir();

			if (!result) {
				throw new RuntimeException("Could not create directory - " + dirName + ". Please check previlages");
			}
		} else {
			System.err
					.println("File or directory with specified name already exists");
		}
	}
	
}
