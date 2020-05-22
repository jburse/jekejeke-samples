package example01;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

import java.io.IOException;

/**
 * <p>Java code for the Call-in Employees Table example.</p>
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
public final class InTable {

    /**
     * Consult the employees table and list the entries.
     * Expected output:
     *
     * bundy
     * canby
     * ozias
     * watson
     *
     * @param args Not used.
     * @throws InterpreterException Shit happens.
     * @throws InterpreterMessage   Shit happens.
     */
    public static void main(String[] args)
            throws InterpreterException, InterpreterMessage {
        Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
        Interpreter inter = know.iterable();
        try {
            Knowledgebase.initKnowledgebase(inter);
            Object consultGoal = inter.parseTerm("consult(example01/tablein)");
            inter.iterator(consultGoal).next().close();

            TermVar employeeVar = new TermVar();
            TermCompound employeeGoal = new TermCompound("employee", employeeVar);

            CallIn callin = inter.iterator(employeeGoal);
            while (callin.hasNext()) {
                callin.next();
                System.out.println(inter.unparseTerm(employeeVar));
            }
        } finally {
            know.finiKnowledgebase();
        }
    }

}