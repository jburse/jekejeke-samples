package example08;

import java.util.ArrayList;

/**
 * <p>Java code for the queue example.</p>
 * <p>The Java queue.</p>
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
public final class Queue {
    private final ArrayList<QueueEntry> planned = new ArrayList<QueueEntry>();

    /**
     * <p>Create a new queue.</p>
     */
    public Queue() {
    }

    /**
     * <p>Take the next runnable from the queue.</p>
     *
     * @return The runnable.
     * @throws InterruptedException Thread was interrupted.
     */
    public Runnable take() throws InterruptedException {
        synchronized (this) {
            for (; ; ) {
                if (planned.size() == 0) {
                    this.wait();
                } else {
                    QueueEntry entry = planned.get(0);
                    long sleep = entry.getWhen() - System.currentTimeMillis();
                    if (sleep <= 0) {
                        planned.remove(0);
                        return entry.getRunnable();
                    } else {
                        this.wait(sleep);
                    }
                }
            }
        }
    }

    /**
     * <p>Post a runnable on the queue.</p>
     *
     * @param r The runnable.
     * @param w The time point.
     */
    public void post(Runnable r, long w) {
        QueueEntry entry = new QueueEntry(r, w);
        synchronized (this) {
            int i = 0;
            for (; i < planned.size(); i++) {
                QueueEntry entry2 = planned.get(i);
                if (w < entry2.getWhen())
                    break;
            }
            planned.add(i, entry);
            this.notifyAll();
        }
    }

}
