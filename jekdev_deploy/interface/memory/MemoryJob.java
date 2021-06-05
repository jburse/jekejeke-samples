package memory;

/**
 * <p>The memory monitor job.</p>
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
final class MemoryJob implements Runnable {
    private double when;
    private double[] values;
    private MemoryFrame frame;

    /**
     * <p>Set the time.</p>
     *
     * @param n The time.
     */
    void setWhen(double n) {
        when = n;
    }

    /**
     * <p>Set the values.</p>
     *
     * @param v The values.
     */
    void setValues(double[] v) {
        values = v;
    }

    /**
     * <p>Set the memory frame.</p>
     *
     * @param f The memory frame.
     */
    void setFrame(MemoryFrame f) {
        frame = f;
    }

    /**
     * <p>Perform the chart job.</p>
     */
    public void run() {
        frame.getPanel().addValues(when, values);
    }

}
