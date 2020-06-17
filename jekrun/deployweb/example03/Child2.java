package example03;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;

/**
 * <p>Java code for the child2 holder.</p>
 *
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
public final class Child2 {
    public static Knowledgebase know;

    /**
     * <p>If necessary do set up of the knowledge base.</p>
     *
     * @throws InterpreterMessage Initialization problem.
     * @throws InterpreterException Initialization problem.
     */
    public static void initKnowledgebase()
            throws InterpreterMessage, InterpreterException {
        if (know != null)
            return;
        synchronized(Child2.class) {
            if (know != null)
                return;
            know = new Knowledgebase(ToolkitLibrary.DEFAULT, Child2.class);
            /* setup the Prolog runtime */
            Interpreter inter = know.iterable();
            Knowledgebase.initKnowledgebase(inter);
            /* load the Prolog code */
            Object consultGoal = inter.parseTerm("consult(library(example01/table))");
            inter.iterator(consultGoal).next().close();
        }
    }

}
