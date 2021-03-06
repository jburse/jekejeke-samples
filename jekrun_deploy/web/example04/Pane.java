package example04;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.ActionListener;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.Callable;

/**
 * <p>Java code for the graphical user interface.</p>
 * <p>While the knowledge base is loading or a query
 * is processed We use the following layout with the
 * buttons disabled:</p>
 * <pre>
 *       Firstname: [          ]
 *       Name:      [          ]
 *       Age:       From [   ] To [   ]
 *       Salary:    From [     ] To [     ]
 *                           ( Debug ) ( Search )
 *       +------------ Progress Bar ------------+
 *       |                                      |
 *       |                                      |
 *       +--------------------------------------+
 * <pre>
 * <p>After the knowledge base is loaded or a query
 * has been processed we use the following layout with
 * the buttons enabled:</p>
 * <pre>
 *       Firstname: [          ]
 *       Name:      [          ]
 *       Age:       From [   ] To [   ]
 *       Salary:    From [     ] To [     ]
 *                           [ Debug ] [ Search ]
 *       +-------------- Result ----------------+
 *       |                                      |
 *       |                                      |
 *       +--------------------------------------+
 * <pre>
 *
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Pane {
    private final static String CARD_SCROLLPANE = "scrollpane";
    private final static String CARD_PROGRESS = "progress";

    private final JTextField firstName = new JTextField(12);
    private final JTextField name = new JTextField(12);
    private final JTextField ageFrom = new JTextField(3);
    private final JTextField ageTo = new JTextField(3);
    private final JTextField salaryFrom = new JTextField(6);
    private final JTextField salaryTo = new JTextField(6);
    private final JTable result = new JTable(new DefaultTableModel());
    private final JButton search = new JButton();
    private final JButton debug = new JButton();
    private final JPanel cards = new JPanel(new CardLayout());
    private final JProgressBar progress = new JProgressBar();

    private Exception exception;

    /**
     * <p>Layout the pane.</p>
     *
     * @param r The root pane.
     * @param s The  action listener.
     */
    public void initPane(JRootPane r, ActionListener s) {
        /* layout the input fields */
        Container c = r.getContentPane();
        c.setLayout(new GridBagLayout());
        c.add(new JLabel("Firstname:"), new GridBagConstraints(0, 0,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(firstName, new GridBagConstraints(1, 0,
                4, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("Name:"), new GridBagConstraints(0, 1,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(name, new GridBagConstraints(1, 1,
                4, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("Age:"), new GridBagConstraints(0, 2,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("From"), new GridBagConstraints(1, 2,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        ageFrom.setHorizontalAlignment(SwingConstants.RIGHT);
        c.add(ageFrom, new GridBagConstraints(2, 2,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("To"), new GridBagConstraints(3, 2,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        ageTo.setHorizontalAlignment(SwingConstants.RIGHT);
        c.add(ageTo, new GridBagConstraints(4, 2,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("Salary:"), new GridBagConstraints(0, 3,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("From"), new GridBagConstraints(1, 3,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        salaryFrom.setHorizontalAlignment(SwingConstants.RIGHT);
        c.add(salaryFrom, new GridBagConstraints(2, 3,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        c.add(new JLabel("To"), new GridBagConstraints(3, 3,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));
        salaryTo.setHorizontalAlignment(SwingConstants.RIGHT);
        c.add(salaryTo, new GridBagConstraints(4, 3,
                1, 1, 0.0, 0.0
                , GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));

        /* layout the action buttons */
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout());
        debug.setText("Debug");
        debug.setActionCommand("debug");
        debug.addActionListener(s);
        debug.setEnabled(false);
        panel.add(debug);
        search.setText("Search");
        search.setActionCommand("search");
        r.setDefaultButton(search);
        search.addActionListener(s);
        search.setEnabled(false);
        panel.add(search);
        c.add(panel, new GridBagConstraints(0, 4, 5, 1, 0.0, 0.0
                , GridBagConstraints.EAST,
                GridBagConstraints.NONE,
                new Insets(2, 2, 2, 2), 0, 0));

        /* layout the results table */
        result.setEnabled(false);
        result.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        JScrollPane scrollpane = new JScrollPane(result);
        scrollpane.setVerticalScrollBarPolicy(
                ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        scrollpane.setPreferredSize(new Dimension(400, 200));
        cards.add(scrollpane, CARD_SCROLLPANE);
        panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        panel.add(progress);
        cards.add(panel, CARD_PROGRESS);
        c.add(cards, new GridBagConstraints(0, 5,
                5, 1, 1.0, 1.0
                , GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(2, 2, 2, 2), 0, 0));
    }

    /**
     * <p>Start a job.</p>
     *
     * @param job The long running job.
     * @param c   The parent component.
     */
    public void startJob(final Callable job, final Component c) {
        ((CardLayout) cards.getLayout()).show(cards, CARD_PROGRESS);
        progress.setIndeterminate(true);

        search.setEnabled(false);
        debug.setEnabled(false);

        Thread thread = new Thread(new Runnable() {
            public void run() {
                try {
                    job.call();
                    exception = null;
                } catch (Exception e) {
                    exception = e;
                }
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        if (exception != null)
                            showError(exception, c);

                        ((CardLayout) cards.getLayout()).show(cards, CARD_SCROLLPANE);
                        progress.setIndeterminate(false);

                        search.setEnabled(true);
                        debug.setEnabled(true);
                    }
                });
            }
        });
        thread.start();
    }

    /**
     * <p>Retrieve the entered first name.
     *
     * @return The first name.
     */
    public String getFirstName() {
        return firstName.getText();
    }

    /**
     * <p>Retrieve the name.
     *
     * @return The name.
     */
    public String getName() {
        return name.getText();
    }

    /**
     * <p>Retrieve the age from.
     *
     * @return The age from.
     */
    public String getAgeFrom() {
        return ageFrom.getText();
    }

    /**
     * <p>Retrieve the age to.
     *
     * @return The age to.
     */
    public String getAgeTo() {
        return ageTo.getText();
    }

    /**
     * <p>Retrieve the salary from.
     *
     * @return The salary from.
     */
    public String getSalaryFrom() {
        return salaryFrom.getText();
    }

    /**
     * <p>Retrieve the salary to.
     *
     * @return The salary to.
     */
    public String getSalaryTo() {
        return salaryTo.getText();
    }

    /**
     * <p>Set the result.</p>
     *
     * @param colids The column identifiers.
     * @param rows   The rows.
     */
    public void setResult(String[] colids, Object[][] rows) {
        /* reset all rows and set the columns */
        ((DefaultTableModel) result.getModel()).setDataVector(null,
                colids);
        DefaultTableCellRenderer renderer = (DefaultTableCellRenderer)
                result.getDefaultRenderer(Integer.class);
        renderer.setHorizontalAlignment(SwingConstants.RIGHT);
        for (int i = 0; i < colids.length; i++)
            if (colids[i].endsWith("#"))
                result.getColumnModel().getColumn(
                        i).setCellRenderer(renderer);

        /* set the rows */
        for (int i = 0; i < rows.length; i++)
            ((DefaultTableModel) result.getModel()).addRow(rows[i]);
    }

    /**
     * <p>Show an error.</p>
     *
     * @param x The error.
     * @param c The parent component.
     */
    public void showError(Exception x, Component c) {
        /* simple problem exception handling */
        String txt;
        /* CheerpJ issue: https://github.com/leaningtech/cheerpj-meta/issues/103 */
        try {
            StringWriter sr = new StringWriter();
            x.printStackTrace(new PrintWriter(sr));
            txt = sr.toString();
        } catch (NullPointerException e) {
            txt = x.getMessage();
        }
        JTextArea textarea = new JTextArea(txt, 8, 40);
        textarea.setLineWrap(true);
        JOptionPane.showMessageDialog(c,
                new JScrollPane(textarea),
                "Problem Report", JOptionPane.ERROR_MESSAGE);
    }

}
