package ports;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * <p>The port sampler thread.</p>
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class ThreadHeadless extends Thread {
    private final static long NANOS_PER_MILLI = 1000000;
    private final static long[] SLEEP = {NANOS_PER_MILLI / 2, 2 * NANOS_PER_MILLI};

    private Interpreter inter;
    private boolean on;
    private ReentrantLock lock = new ReentrantLock();
    private Condition cond = lock.newCondition();

    /**
     * <p>Create a port sampler thread.</p>
     *
     * @param i The interpreter.
     */
    ThreadHeadless(Interpreter i) {
        inter = i;
    }

    /**
     * <p>Constantly update the chart.</p>
     */
    public void run() {
        try {
            lock.lock();
            for (; ; ) {
                long lastTime = System.nanoTime();
                try {
                    inter.setProperty("sys_tdebug", (on ? "on" : "off"));
                } catch (InterpreterMessage x) {
                    throw new RuntimeException(x);
                }
                long sleep = SLEEP[on ? 1 : 0] - (System.nanoTime() - lastTime);
                try {
                    while (sleep > 0)
                        sleep = cond.awaitNanos(sleep);
                } catch (InterruptedException x) {
                    return;
                }
                on = !on;
            }
        } finally {
            lock.unlock();
        }
    }

}
