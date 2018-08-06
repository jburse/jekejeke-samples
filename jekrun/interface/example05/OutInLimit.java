package example05;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>Java code for the Call-out Call-in Solution Limiter example.</p>
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
public class OutInLimit {
    /* choice point data */
    private final CallIn callin;
    private int count = 0;

    /**
     * <p>Create a choice point data.</p>
     *
     * @param c The call-in.
     */
    private OutInLimit(CallIn c) {
        callin = c;
    }

    /**
     * <p>Foreign predicate to limit solutions.</p>
     *
     * @param inter   The call-in.
     * @param callout The call-out object.
     * @param goal    The goal.
     * @param max     The maximum number of solutions to return.
     * @return True if a result was found, otherwise false.
     * @throws InterpreterException Passed down from the goal.
     */
    public static boolean limit(Interpreter inter,
                                CallOut callout,
                                AbstractTerm goal, int max)
            throws InterpreterException, InterpreterMessage {
        OutInLimit limit;
        if (callout.getFirst()) {
            limit = new OutInLimit(inter.iterator(goal));
            callout.setData(limit);
        } else {
            limit = (OutInLimit) callout.getData();
        }
        if (callout.getCleanup()) {
            InterpreterException ex = callout.getException();
            ex = limit.callin.cleanup(ex);
            callout.setException(ex);
            return true;
        }
        if (!(limit.count < max)) {
            limit.callin.close();
            return false;
        }
        if (limit.callin.hasNext()) {
            limit.callin.next();
            limit.count++;
            callout.setRetry(true);
            callout.setSpecial(true);
            callout.setCutter(true);
            return true;
        }
        return false;
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

        foreignGoal = inter.parseTerm("use_package(foreign(" +
                "jekpro/tools/term))");
        inter.iterator(foreignGoal).next().close();

        foreignGoal = inter.parseTerm("foreign(employee/1, " +
                "'example03.OutTable', employee('CallOut'))");
        inter.iterator(foreignGoal).next().close();

        foreignGoal = inter.parseTerm("foreign(limit/2, " +
                "'example05.OutInLimit', limit('Interpreter', 'CallOut'," +
                "'AbstractTerm', 'int'))");
        inter.iterator(foreignGoal).next().close();

        TermVar employeeVar = new TermVar();
        TermCompound employeeGoal = new TermCompound("employee", employeeVar);
        TermCompound limitGoal = new TermCompound("limit", employeeGoal, 3);

        Writer wr = (Writer) inter.getProperty(ToolkitLibrary.PROP_SYS_CUR_OUTPUT);
        CallIn callin = inter.iterator(limitGoal);
        while (callin.hasNext()) {
            callin.next();
            wr.write(inter.unparseTerm(0, employeeVar));
            wr.write('\n');
            wr.flush();
        }

        wr.write('\n');
        wr.flush();

        limitGoal = new TermCompound("limit", employeeGoal, 4);
        callin = inter.iterator(limitGoal);
        callin.next();
        wr.write(inter.unparseTerm(0, employeeVar));
        wr.write('\n');
        wr.flush();
        callin.close();

        wr.write('\n');
        wr.flush();

        limitGoal = new TermCompound("limit", employeeGoal, 6);

        callin = inter.iterator(limitGoal);
        while (callin.hasNext()) {
            callin.next();
            wr.write(inter.unparseTerm(0, employeeVar));
            wr.write('\n');
            wr.flush();
        }

    }

}
