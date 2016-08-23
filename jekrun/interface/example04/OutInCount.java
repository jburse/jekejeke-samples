package example04;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.*;
import jekpro.tools.term.*;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>Java code for the Call-out Call-in Solution Counter example.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class OutInCount {

    /**
     * <p>Foreign predicate to count solutions.</p>
     *
     * @param inter The call-in.
     * @param goal  The goal.
     * @return The count.
     * @throws InterpreterException Passed down from the goal.
     * @throws InterpreterMessage Shit happens.
     */
    public static int count(Interpreter inter, Term goal)
            throws InterpreterException, InterpreterMessage {
        CallIn callin = inter.iterator(goal);
        int count = 0;
        while (callin.hasNext()) {
            callin.next();
            count++;
        }
        return count;
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

        Object foreignGoal = Term.parseTerm("use_package(foreign(" +
                "jekpro/tools/call))", inter);
        inter.iterator(foreignGoal).nextClose();

        foreignGoal = Term.parseTerm("use_package(foreign(" +
                "jekpro/tools/term))", inter);
        inter.iterator(foreignGoal).nextClose();

        foreignGoal = Term.parseTerm("foreign(employee/1, " +
                "'example03.OutTable', employee('CallOut'))", inter);
        inter.iterator(foreignGoal).nextClose();

        foreignGoal = Term.parseTerm("foreign(count/2, " +
                "'example04.OutInCount', count('Interpreter', 'Term'))", inter);
        inter.iterator(foreignGoal).nextClose();

        TermVar[] employeeVars = TermVar.createVars(2);
        TermCompound employeeGoal = new TermCompound("employee", employeeVars[0]);
        TermCompound countGoal = new TermCompound("count", employeeGoal, employeeVars[1]);

        Writer wr = (Writer) inter.getProperty(ToolkitLibrary.PROP_SYS_CUR_OUTPUT);
        Object res = inter.iterator(employeeVars[1], countGoal).nextClose();
        wr.write(Term.toString(0, inter, res));
        wr.write('\n');
        wr.flush();
    }

}