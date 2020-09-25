package example04;

import example01.Query;
import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Callable;

/**
 * <p>Java code for the Java applet.</p>
 * <p>
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
public final class Applet extends JFrame implements ActionListener {
    private final Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
    private final Pane pane = new Pane();

    /**
     * <p>Setup the knowledgebase and init the pane.</p>
     */
    private Applet() {
        /* init the pane */
        pane.initPane(getRootPane(), this);
        setTitle("Deployment Web - Applet");
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        /* load the Prolog */
        pane.startJob(new Callable() {
            public Object call() throws Exception {
                initKnowledgebase();
                return null;
            }
        }, this);
    }

    /**
     * <p>Do set up of the knowledge base.</p>
     */
    private void initKnowledgebase() throws Exception {
        know.setProperty(Knowledgebase.PROP_SYS_HINT,
                Integer.valueOf(Knowledgebase.HINT_MASK_LMTD));
        /* setup the Prolog runtime */
        Interpreter inter = know.iterable();
        Knowledgebase.initKnowledgebase(inter);
        /* load the Prolog code */
        Object consultGoal = inter.parseTerm("consult(library(example01/table))");
        inter.iterator(consultGoal).next().close();
    }

    /**
     * <p>Handle the search and the debug button.</p>
     *
     * @param e The action event.
     */
    public void actionPerformed(ActionEvent e) {
        try {
            Interpreter inter = know.iterable();
            final Query query = new Query(inter);
            /* retrieve the search criteria and build the query */
            query.setFirstname(pane.getFirstName());
            query.setName(pane.getName());
            query.setAgeFrom(pane.getAgeFrom());
            query.setAgeTo(pane.getAgeTo());
            query.setSalaryFrom(pane.getSalaryFrom());
            query.setSalaryTo(pane.getSalaryTo());
            final String[] colids = query.listColumnIdentifiers();
            final TermVar[] vars = query.makeVars();
            final AbstractTerm queryTerm = query.makeQuery(vars);
            if ("search".equals(e.getActionCommand())) {
                /* execute the query and populate the results */
                pane.startJob(new Callable() {
                    public Object call() throws Exception {
                        query.listRows(vars, queryTerm);
                        SwingUtilities.invokeAndWait(new Runnable() {
                            public void run() {
                                Object[][] rows = query.getRows();
                                pane.setResult(colids, rows);
                            }
                        });
                        return null;
                    }
                }, this);
            } else {
                Object opt = query.makeVariableNames(colids, vars);
                String qstr = inter.unparseTerm(queryTerm, opt);
                JOptionPane.showMessageDialog(this, qstr,
                        "Query Term", JOptionPane.PLAIN_MESSAGE);
            }
        } catch (Exception x) {
            pane.showError(x, this);
        }
    }

    /**
     * <p>Main method shows the standalone frame.</p>
     *
     * @param args The command line arguments, not used.
     * @throws Exception Initialization Error.
     */
    public static void main(String[] args) throws Exception {
//        String laf = UIManager.getSystemLookAndFeelClassName();
//        UIManager.setLookAndFeel(laf);
        Applet applet = new Applet();
        applet.pack();
        applet.setMinimumSize(applet.getSize());
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        Rectangle r = ge.getMaximumWindowBounds();
        applet.setBounds(r);
//        applet.setLocationRelativeTo(JOptionPane.getRootFrame());
        applet.setVisible(true);
    }

}
