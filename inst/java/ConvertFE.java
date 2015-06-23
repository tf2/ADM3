import java.util.regex.*;
import java.io.*;
import java.io.FileNotFoundException;

public class ConvertFE {   
   
   	public static void FEtoBED(String file, String filename) {
    PrintWriter pw = null;  File output = new File(filename);
    if (output.exists()) System.out.println("File already exists - overwriting!!!");
     	DataInputStream dis = null; String record = null;
        int recCount = 0; int loc_int =0; int rat_int =0; int raterr_int =0; int pin =0;
        Pattern p = Pattern.compile("chr\\w\\w?:\\d+-\\d");
        try {
           	File f = new File(file); FileInputStream fis = new FileInputStream(f); BufferedInputStream bis = new BufferedInputStream(fis);
          	dis = new DataInputStream(bis); pw = new PrintWriter(new FileOutputStream(output), true);
        	while ( (record=dis.readLine()) != null ) {
           		recCount++;
        		if (recCount == 10){
                  String[] seq = record.split("\t");
              		for (int i =0; i<seq.length; i++ ) {
              			String temp = seq[i];
                 		if (temp.equals("SystematicName")) loc_int = i;
                		else if (temp.equals("LogRatio")) rat_int = i;
                		else if (temp.equals("LogRatioError")) raterr_int = i;
              		}
           		} else if (recCount > 10) {
                  String[] data = record.split("\t"); Matcher matc = p.matcher(record);
                  	if (matc.find()) {
                      	StringBuilder sb = new StringBuilder(512); String SyName = data[loc_int];
                      	String[] SyArray = SyName.split(":"); String SyTemp = SyArray[1]; String[] SyLocat = SyTemp.split("-");
                      	String chr = SyArray[0].substring(3);
                      		if (chr.equals("X")) chr = "23";
                      		else if (chr.equals("Y")) chr = "24";
                      		else if (chr.equals("XD")) chr = "25";
                      		else if (chr.equals("2L")) chr = "26";         
                      	int start = Integer.parseInt(SyLocat[0]); int end = Integer.parseInt(SyLocat[1]);		
 					  	double logR = Double.parseDouble(data[rat_int]); double logRerr = Double.parseDouble(data[raterr_int]);
					  	logR = Math.log(Math.pow(10, logR)) / Math.log(2);
                    	sb.append(chr + "\t" + start + "\t" + end + "\t" + logR + "\t" + logRerr+ "\n"); pw.print(sb); pin++;
                }
              }
           }
        } catch (IOException e) { System.out.println("Uh oh, got an IOException error!" + e.getMessage());
        	} finally { if (dis != null) { try { pw.close(); dis.close(); } catch (IOException ioe) {}
        }
      }
	}

	public static void main(String[] args) {		
		String file = ""; String out="";
		for (int u=0;u<args.length; u++) { 
			int pin = u+1; String s = args[u]; 
			if (s.equals("-f")) file = args[pin];  
			if (s.equals("-o")) out = args[pin]; 
		}
		FEtoBED(file, out);	
	}
      
}