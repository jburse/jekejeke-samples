package example08;

/**
 * <p>Java code for the queue example.</p>
 * <p>The Java queue entry.</p>
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
final class QueueEntry {
    private final Runnable runnable;
    private final long when;

    /**
     * <p>Create the queue element.</p>
     *
     * @param r The runnable.
     * @param w The time point.
     */
    QueueEntry(Runnable r, long w) {
        runnable = r;
        when = w;
    }

    /**
     * <p>Retrieve the runnable.</p>
     *
     * @return The runnable.
     */
    Runnable getRunnable() {
        return runnable;
    }

    /**
     * <p>Retrieve the time point.</p>
     *
     * @return The time point.
     */
    long getWhen() {
        return when;
    }

}
