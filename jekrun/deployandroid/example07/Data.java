package example07;

import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;

/**
 * <p>Java code for the data holder.</p>
 * <p>Same as the Swing version.</p>
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
public final class Data {
    public static Knowledgebase know;

    /**
     * <p>If necessary do set up of the knowledge base.</p>
     */
    public synchronized static void initKnowledgebase() {
        if (know != null)
            return;
        try {
            know = new Knowledgebase(ToolkitLibrary.DEFAULT);
            /* setup the Prolog runtime */
            Interpreter inter = know.iterable();
            Knowledgebase.initKnowledgebase(inter);
            /* load the Prolog code */
            Object consultGoal = inter.parseTerm("consult(library(example07/table))");
            inter.iterator(consultGoal).next().close();
        } catch (InterpreterMessage x) {
            throw new RuntimeException(x);
        } catch (InterpreterException x) {
            throw new RuntimeException(x);
        }
    }

}
