package example03;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.*;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

import java.io.IOException;
import java.io.Writer;
import java.util.Enumeration;
import java.util.Vector;

/**
 * <p>Java code for the Call-out Employees Table example.</p>
 * <p/>
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
public final class OutTable {
    static Vector<String> employees = new Vector<String>();

    static {
        employees.addElement("bundy");
        employees.addElement("canby");
        employees.addElement("ozias");
        employees.addElement("watson");
    }

    /**
     * <p>Foreign predicate to enumerate employees.</p>
     *
     * @param co The call out.
     * @return The employee, or null.
     */
    public static String employee(CallOut co) {
        Enumeration<String> elements;
        if (co.getFirst()) {
            elements = employees.elements();
            co.setData(elements);
        } else {
            elements = (Enumeration<String>) co.getData();
        }
        if (elements.hasMoreElements()) {
            co.setRetry(true);
            return elements.nextElement();
        } else {
            return null;
        }
    }

    /**
     * <p>Runtime library variant of executing the example.</p>
     *
     * @param args The command line arguments.
     * @throws InterpreterException Exception in Jekejeke Prolog execution.
     * @throws InterpreterMessage   Message in Jekejeke Prolog execution.
     * @throws IOException          Problem writing to current output.
     */
    public static void main(String[] args) throws InterpreterException,
            InterpreterMessage, IOException {
        Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
        Interpreter inter = know.iterable();
        Knowledgebase.initKnowledgebase(inter);

        Object foreignGoal = inter.parseTerm("use_package(foreign(" +
                "jekpro/tools/call))");
        inter.iterator(foreignGoal).next().close();

        foreignGoal = inter.parseTerm("foreign(employee/1, " +
                "'example03.OutTable', employee('CallOut'))");
        inter.iterator(foreignGoal).next().close();

        TermVar employeeVar = new TermVar();
        TermCompound employeeGoal = new TermCompound("employee", employeeVar);

        Writer wr = (Writer) inter.getProperty(ToolkitLibrary.PROP_SYS_CUR_OUTPUT);
        CallIn callin = inter.iterator(employeeGoal);
        while (callin.hasNext()) {
            callin.next();
            wr.write(inter.unparseTerm(0, employeeVar));
            wr.write('\n');
            wr.flush();
        }
    }

}