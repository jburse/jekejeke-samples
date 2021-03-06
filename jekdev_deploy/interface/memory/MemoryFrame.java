package memory;

import javax.swing.*;
import java.awt.event.WindowEvent;

/**
 * <p>The memory monitor frame.</p>
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
public final class MemoryFrame extends JFrame {
    private ThreadFrame thread;
    private MemoryPanel panel;

    /**
     * <p>Create a memory monitor frame.</p>
     */
    private MemoryFrame() {
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                this_windowClosing(e);
            }
        });
        setTitle("Memory Monitor");
        panel = new MemoryPanel(1);
        getContentPane().add(panel);
    }

    /**
     * <p>Retrieve the memory panel.</p>
     *
     * @return The memory panel.
     */
    MemoryPanel getPanel() {
        return panel;
    }

    /**
     * <p>Handle window close event.</p>
     *
     * @param e The event.
     */
    private void this_windowClosing(WindowEvent e) {
        thread.interrupt();
        dispose();
    }

    /**
     * <p>Start the monitor.</p>
     * <p>Java method called from Prolog.</p>
     */
    public static void startMonitor() {
        MemoryFrame frame = new MemoryFrame();
        frame.setSize(500, 300);
        frame.setVisible(true);
        frame.thread = new ThreadFrame(frame);
        frame.thread.start();
    }

    /**
     * <p>Request garbage collection.</p>
     * <p>Java method called from Prolog.</p>
     */
    public static void gc() {
        System.gc();
    }

}
