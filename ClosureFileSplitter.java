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

public class CopyOfClosureFileSplitter {

    static String dirName = "functions";
    static final String FILE_PREFIX = "func_";
    static final String SYS_SEP = System.getProperty("file.separator");
    static final String SEP_REP = "sep";
    static final String FUNC_NAME_PREFIX = "func:";
    static final String PRIM_FUNC_PREFIX = "prim:";
    static final String BODY_PREFIX = "body:";
    static final String ARGS_PREFIX = "args:";
    static final String RETV_PREFIX = "retn:";

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

                if (!line.startsWith(FUNC_NAME_PREFIX)) {
                    System.err.println("Function calls - " + totalFunctionCalls);
                    System.err.println("ERROR" + line);
                    System.exit(1);
                }

                String tmpline = line.substring(line.indexOf(':') + 2,
                        line.length());
                funcName = tmpline.substring(6, tmpline.length() - 1); // quote removal
                // check if already encountered this function

                if (functions.contains(funcName)) {
                    fileIndex = funcFileCounter.get(funcName);
                    File forSizeCheck = new File(dirName + "/" + FILE_PREFIX
                            + funcName + "/" + FILE_PREFIX + funcName + "_" + fileIndex);
                    if (forSizeCheck.length() > 500 * 1000 * 1000) { //500 MB files size
                        fileIndex++;
                        funcFileCounter.put(funcName, fileIndex);
                    }
                }
                bufferGeneral.append(line + "\n"); //to have the same
                line = reader.readLine();
//				while (line.startsWith(FUNC_NAME_PREFIX)) {
//					line = reader.readLine();
//				}
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
                        createDir(dirName + "/" + FILE_PREFIX + funcName); // replace with separator
                    funcFileCounter.put(funcName, fileIndex);
                    HashSet<String> arguments = new HashSet<>();
                    arguments.add(bufferArguments.toString());
                    funcArguments.put(funcName, arguments);
                } else {
                    HashSet<String> arguments = funcArguments.get(funcName);
                    if (arguments.contains(bufferArguments.toString())) {
                        sameArguments = true;
                    }
                    arguments.add(bufferArguments.toString());
                    funcArguments.put(funcName, arguments);
                }
                while (line != null && line.startsWith(RETV_PREFIX)) {
                    bufferGeneral.append(line + "\n");
                    line = reader.readLine();
                }
                bufferGeneral.append("\n");
                if (!discardCapture && !sameArguments) {
                    writer = new BufferedWriter(new OutputStreamWriter(
                            new FileOutputStream(dirName + "/" + FILE_PREFIX
                                    + funcName + "/" + FILE_PREFIX + funcName + "_"
                                    + fileIndex, true)
                    ));
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
        } catch (Exception e) {
            System.err.println(e);
        } finally {
            reader.close();
        }
        System.out.println("Function calls processed - " + totalFunctionCalls);
        System.out.println("Function calls saved - " + savedFunctionCalls);
        System.out.println("Split finished");
    }

    private static void createDir(String dirName) {
        File theDir = new File(dirName);

        // if the directory does not exist, create it
        if (!theDir.exists()) {
            // System.out.println("creating directory: " + dirName);
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
            discardCapture = true;
            savedFunctionCalls--;
            // System.out.println("Discard true");
        }
    }
}
