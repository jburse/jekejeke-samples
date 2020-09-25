package example01;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * <p>Java code for the Java terminal main class.</p>
 * <p>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Terminal {

    /**
     * <p>Main method.</p>
     *
     * @param args Not used.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     * @throws IOException          Shit happens.
     */
    public static void main(String[] args)
            throws InterpreterException, InterpreterMessage, IOException {
        /* setup the Prolog runtime */
        Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
        Interpreter inter = know.iterable();
        try {
            Knowledgebase.initKnowledgebase(inter);
            /* load the Prolog code */
            Object consultGoal = inter.parseTerm("consult(example01/table)");
            inter.iterator(consultGoal).next().close();

            /* read the search criteria */
            Query query = new Query(inter);
            BufferedReader ttyin = new BufferedReader(new InputStreamReader(System.in));
            System.out.print("Firstname: ");
            query.setFirstname(ttyin.readLine());
            System.out.print("Name: ");
            query.setName(ttyin.readLine());
            System.out.print("Age From: ");
            query.setAgeFrom(ttyin.readLine());
            System.out.print("Age To: ");
            query.setAgeTo(ttyin.readLine());
            System.out.print("Salary From: ");
            query.setSalaryFrom(ttyin.readLine());
            System.out.print("Salary To: ");
            query.setSalaryTo(ttyin.readLine());

            /* build and display the query */
            System.out.println();
            System.out.println("Query Term:");
            String[] cols = query.listColumnIdentifiers();
            TermVar[] vars = query.makeVars();
            AbstractTerm queryTerm = query.makeQuery(vars);
            Object opt = query.makeVariableNames(cols, vars);
            System.out.println(inter.unparseTerm(queryTerm, opt));

            /* execute the query and display the results */
            System.out.println();
            System.out.println("Result Table:");

            /* flesh out the table header */
            for (int i = 0; i < cols.length; i++) {
                if (i != 0)
                    System.out.print('\t');
                System.out.print(cols[i]);
            }
            System.out.println();

            query.listRows(vars, queryTerm);
            Object[][] rows = query.getRows();
            for (int j = 0; j < rows.length; j++) {
                /* flesh out a table row */
                Object[] row = rows[j];
                for (int i = 0; i < row.length; i++) {
                    if (i != 0)
                        System.out.print('\t');
                    System.out.print(row[i]);
                }
                System.out.println();
            }
        } finally {
            know.finiKnowledgebase();
        }
    }

}
