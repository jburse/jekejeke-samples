package memory;

import javax.swing.*;
import java.awt.event.WindowEvent;

/**
 * <p>The memory monitor frame.</p>
 *
 * @author Copyright 2012, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
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
