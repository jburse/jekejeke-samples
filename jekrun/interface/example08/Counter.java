package example08;

import jekpro.tools.proxy.RuntimeWrap;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>Java code for the queue example.</p>
 * <p>The Java counter.</p>
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
public final class Counter implements Runnable {
    private String name;
    private final Queue queue;
    private final Writer writer;
    private int count;

    /**
     * <p>Create a counter task.</p>
     *
     * @param n The name.
     * @param q The queue.
     * @param w The writer.
     */
    public Counter(String n, Queue q, Writer w) {
        name = n;
        queue = q;
        writer = w;
    }

    /**
     * <p>Execute the counter task.</p>
     */
    public void run() {
        try {
            long time = System.currentTimeMillis();
            StringBuilder buf = new StringBuilder();
            buf.append("Process ");
            buf.append(name);
            buf.append(": ");
            buf.append(count);
            buf.append('\n');
            writer.write(buf.toString());
            writer.flush();
            count++;
            if (count < 10)
                queue.post(this, time + 1000);
        } catch (IOException x) {
            throw new RuntimeWrap(x);
        }
    }

}
