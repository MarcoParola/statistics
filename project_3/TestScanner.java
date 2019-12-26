import java.io.*;
import java.util.*;
import java.text.SimpleDateFormat;  
import java.lang.Integer;


public class TestScanner {

    public static void main(String[] args) throws Exception {
        Scanner scanner = new Scanner(new File("uber.csv"));
        scanner.useDelimiter("\n");
        PrintWriter writer = new PrintWriter("uber_tab2.csv", "UTF-8");
        
                
        String str = scanner.next();
        
        String[] splits = str.split(",");
        String split1 = splits[0]; 
        String split2 = splits[1];
        
        String[] d = split2.split(":");
        String antOrPost = split2.substring(Math.max(split2.length() - 2, 0));
        String substring = split2.substring(Math.max(split2.length() - 9, 0));           
        String substring_ = substring.substring(0, substring.length() - 2);
        String h="";
        String s1 = "";
        
        
        if(d[0].compareTo("12") == 0 && antOrPost.compareTo("AM") == 0)
        	h = "00";
        else if(d[0].compareTo("12") != 0 && antOrPost.compareTo("PM") == 0)
        	h = String.valueOf(Integer.valueOf(d[0]) + 12);
        else
        	h = d[0];
        
        s1 = h;
        h = h + substring_;
        System.out.println(h + " " + split2);
        int count = 1;
        
        while(scanner.hasNext()){
            
            String s = scanner.next();
            
            String[] parts = s.split(",");
            String part1 = parts[0]; 
            String part2 = parts[1];
            
            String[] date = part2.split(":");
            String antPost = part2.substring(Math.max(part2.length() - 2, 0));
            String substring1 = part2.substring(Math.max(part2.length() - 9, 0));           
            String substring2 = substring1.substring(0, substring1.length() - 2);
            String hh="";
            String s2 = "";
            
            if(date[0].compareTo("12") == 0 && antPost.compareTo("AM") == 0)
            	hh = "00";
            else if(date[0].compareTo("12") != 0 && antPost.compareTo("PM") == 0)
            	hh = String.valueOf(Integer.valueOf(date[0]) + 12);
            else
            	hh = date[0];
            
            s2 = hh;
            hh = hh + substring2;
            
            if(s1.compareTo(s2) == 0)
            	count++;
            else {
            	System.out.println(part1 + " " + s1 + ":00" + ", " + count);
            	writer.println(part1 + " " + s1 + ":00" + ", " + count);
            	s1 = s2;
            	count = 0;
            }
        }
        scanner.close();
        writer.close(); 
    }

}
