package terminal;

import jekpro.frequent.stream.ForeignConsole;
import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Terminal {

    /**
     * <p>Main method.</p>
     *
     * @param args The command line arguments, not used.
     * @throws InterpreterException Problems with setup the knowledge base
     *                              or executing the query.
     * @throws InterpreterMessage   Problems with setup the knowledge base
     *                              or executing the query.
     * @throws IOException          Problems with reading from the standard input.
     */
    public static void main(String[] args) throws InterpreterException,
            InterpreterMessage, IOException {
        /* setup the knowledge base */
        Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
        Interpreter inter = know.iterable();
        Knowledgebase.initKnowledgebase(inter);
        Object consultGoal = inter.parseTerm("consult(library(terminal/table))");
        inter.iterator(consultGoal).next().close();

        /* read the search criteria */
        Query query = new Query(inter);
        Writer ttyout = (Writer) inter.getProperty(
                ToolkitLibrary.PROP_SYS_DISP_OUTPUT);
        Reader ttyin = (Reader) inter.getProperty(
                ToolkitLibrary.PROP_SYS_DISP_INPUT);
        ttyout.write("Firstname: ");
        ttyout.flush();
        query.setFirstname(ForeignConsole.readLine(ttyin));
        ttyout.write("Name: ");
        ttyout.flush();
        query.setName(ForeignConsole.readLine(ttyin));
        ttyout.write("Age From: ");
        ttyout.flush();
        query.setAgeFrom(ForeignConsole.readLine(ttyin));
        ttyout.write("Age To: ");
        ttyout.flush();
        query.setAgeTo(ForeignConsole.readLine(ttyin));
        ttyout.write("Salary From: ");
        ttyout.flush();
        query.setSalaryFrom(ForeignConsole.readLine(ttyin));
        ttyout.write("Salary To: ");
        ttyout.flush();
        query.setSalaryTo(ForeignConsole.readLine(ttyin));

        /* build and display the query */
        ttyout.write('\n');
        ttyout.flush();
        ttyout.write("Query Term:");
        ttyout.write('\n');
        ttyout.flush();
        String[] colids = query.listColumnIdentifiers();
        TermVar[] vars = query.makeVars();
        AbstractTerm queryTerm = query.makeQuery(vars);
        inter.unparseTerm(ttyout, query.makeVariableNames(colids, vars), queryTerm);
        ttyout.write('\n');
        ttyout.flush();

        /* execute the query and display the results */
        ttyout.write('\n');
        ttyout.flush();
        ttyout.write("Result Table:");
        ttyout.write('\n');
        ttyout.flush();
        for (int i = 0; i < colids.length; i++) {
            if (i != 0)
                ttyout.write('\t');
            ttyout.write(colids[i]);
        }
        ttyout.write('\n');
        ttyout.flush();

        query.listRows(vars, queryTerm);
        Object[][] rows = query.getRows();
        for (int j = 0; j < rows.length; j++) {
            Object[] row = rows[j];
            for (int i = 0; i < row.length; i++) {
                if (i != 0)
                    ttyout.write('\t');
                ttyout.write(row[i].toString());
            }
            ttyout.write('\n');
            ttyout.flush();
        }
    }

}
