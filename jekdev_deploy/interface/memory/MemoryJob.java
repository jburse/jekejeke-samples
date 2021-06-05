package memory;

/**
 * <p>The memory monitor job.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
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
