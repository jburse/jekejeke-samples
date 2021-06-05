package memory;

import javax.swing.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * <p>The memory monitor thread.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
final class ThreadFrame extends Thread {
    private final static long NANOS_PER_MILLI = 1000000;
    private final static long SLEEP = 333 * NANOS_PER_MILLI;

    private final MemoryFrame frame;
    private ReentrantLock lock = new ReentrantLock();
    private Condition cond = lock.newCondition();

    /**
     * <p>Create a memory monitor thread.</p>
     *
     * @param f The memory frame.
     */
    ThreadFrame(MemoryFrame f) {
        frame = f;
    }

    /**
     * <p>Constantly update the chart.</p>
     */
    public void run() {
        long startTime = System.nanoTime();
        try {
            lock.lock();
            for (; ; ) {
                long lastTime = System.nanoTime();

                double[] values = new double[1];
                long total = Runtime.getRuntime().totalMemory();
                long free = Runtime.getRuntime().freeMemory();
                values[0] = (double) (total - free) / 1000000;
                int when = (int) ((lastTime - startTime + NANOS_PER_MILLI / 2) / NANOS_PER_MILLI);

                MemoryJob job = new MemoryJob();
                job.setWhen((double) when / 1000);
                job.setValues(values);
                job.setFrame(frame);
                SwingUtilities.invokeLater(job);

                long sleep = SLEEP - (System.nanoTime() - lastTime);
                try {
                    while (sleep > 0)
                        sleep = cond.awaitNanos(sleep);
                } catch (InterruptedException x) {
                    return;
                }
            }
        } finally {
            lock.unlock();
        }
    }

}
