package ports;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

/**
 * <p>Java code for the API of the port sampler example.</p>
 * <p>This class provides an API to thread objects.</p>
 * <p>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ThreadAPI {
    private final static String OP_THREAD = "thread";
    private final static TermAtomic ATOM_THREAD = new TermAtomic(OP_THREAD);

    /**
     * <p>Start the sampler.</p>
     * <p>Java method called from Prolog.</p>
     *
     * @param inter The interpreter.
     * @return The output thread term.
     */
    public static AbstractTerm startSampler(Interpreter inter) {
        ThreadHeadless thread = new ThreadHeadless(inter);
        thread.start();
        return wrapThread(thread);
    }

    /**
     * <p>Stop the sampler.</p>
     * <p>Java method called from Prolog.</p>
     *
     * @param para The input thread term.
     * @throws InterpreterMessage Instantiation, domain or permission error.
     */
    public static void stopSampler(Object para)
            throws InterpreterMessage {
        ThreadHeadless thread = (ThreadHeadless) unwrapThread(para);
        thread.interrupt();
    }

    /**
     * <p>Wrap a mutex object into the mutex term.</p>
     * <p>Will create a term of the following form:</p>
     * <pre>
     *     thread(ref)
     * </pre>
     *
     * @param obj   The mutex object.
     * @return The mutex term.
     */
    private static TermCompound wrapThread(Thread obj) {
        return new TermCompound(ATOM_THREAD, obj);
    }

    /**
     * <p>Unwrap a mutex object from the mutex term.</p>
     * <p>Will check whether the term has the following form:</p>
     * <pre>
     *     thread(ref)
     * </pre>
     *
     * @param para The stream term.
     * @return The stream object.
     * @throws InterpreterMessage Instantiation or domain error.
     */
    private static Thread unwrapThread(Object para) throws InterpreterMessage {
        if (para instanceof TermVar)
            throw new InterpreterMessage(InterpreterMessage.instantiationError());
        if (!(para instanceof TermCompound) ||
                ((TermCompound) para).getArity() != 1 ||
                !((TermCompound) para).getFunctor().equals(ATOM_THREAD))
            throw new InterpreterMessage(InterpreterMessage.domainError(OP_THREAD, para));
        Object obj = ((TermCompound) para).getArg(0);
        obj = InterpreterMessage.castRef(obj);
        if (obj instanceof Thread) {
            return (Thread) obj;
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError(OP_THREAD, obj));
        }
    }

}
