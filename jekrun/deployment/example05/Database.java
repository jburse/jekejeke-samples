package example05;

import jekpro.platform.headless.ToolkitLibrary;
import example04.Stub;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;
import example02.Pane;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * <p>Java code for the Java client.</p>
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
public final class Database extends JFrame implements ActionListener {
    private Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
    private Pane pane = new Pane();

    /**
     * <p>Setup the know and init the pane.</p>
     */
    private Database() {
        /* init the pane */
        pane.initPane(getRootPane(), this);
        setTitle("Deployment Study - Database");
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        /* load the Prolog */
        pane.startJob(new Runnable() {
            public void run() {
                initKnowledgebase();
            }
        }, new Runnable() {
            public void run() {
            }
        });
    }

    /**
     * <p>do set up of the knowledge base.</p>
     */
    private void initKnowledgebase() {
        try {
            /* setup the Prolog runtime */
            Interpreter inter = know.iterable();
            Knowledgebase.initKnowledgebase(inter);
            /* load the Prolog code */
            Object consultGoal = inter.parseTerm("consult(library(example05/driver))");
            inter.iterator(consultGoal).next().close();
        } catch (InterpreterMessage x) {
            throw new RuntimeException(x);
        } catch (InterpreterException x) {
            throw new RuntimeException(x);
        }
    }

    /**
     * <p>Handle the search and the debug button.</p>
     *
     * @param e The action event.
     */
    public void actionPerformed(ActionEvent e) {
        try {
            Interpreter inter = know.iterable();
            final Stub stub = new Stub("drive", inter);
            /* retrieve the search criteria and build the query */
            stub.setFirstname(pane.getFirstName());
            stub.setName(pane.getName());
            stub.setAgeFrom(pane.getAgeFrom());
            stub.setAgeTo(pane.getAgeTo());
            stub.setSalaryFrom(pane.getSalaryFrom());
            stub.setSalaryTo(pane.getSalaryTo());
            final String[] colids = stub.listColumnIdentifiers();
            final TermVar[] vars = stub.makeVars();
            final AbstractTerm queryTerm = stub.makeQuery(vars);
            if ("search".equals(e.getActionCommand())) {
                /* execute the query and populate the results */
                pane.disableButtons();
                pane.startJob(new Runnable() {
                    public void run() {
                        stub.listRows(vars, queryTerm);
                    }
                }, new Runnable() {
                    public void run() {
                        Object[][] rows = stub.getRows();
                        pane.setResult(colids, rows);
                    }
                });
            } else {
                Object opt=stub.makeVariableNames(colids, vars);
                String qstr=inter.unparseTerm(queryTerm, opt);
                JOptionPane.showMessageDialog(this, qstr,
                        "Query Term", JOptionPane.PLAIN_MESSAGE);
            }
        } catch (Exception x) {
            /* simple problem exception handling */
            StringWriter sr = new StringWriter();
            x.printStackTrace(new PrintWriter(sr));
            JTextArea textarea = new JTextArea(sr.toString(), 8, 40);
            textarea.setLineWrap(true);
            JOptionPane.showMessageDialog(this,
                    new JScrollPane(textarea),
                    "Problem Report", JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * <p>Main method shows the standalone frame.</p>
     *
     * @param args The command line arguments, not used.
     */
    public static void main(String[] args) {
        String laf = UIManager.getSystemLookAndFeelClassName();
        try {
            UIManager.setLookAndFeel(laf);
        } catch (Exception x) {
            throw new RuntimeException(x);
        }
        Database database = new Database();
        database.pack();
        database.setMinimumSize(database.getSize());
        database.setLocationRelativeTo(JOptionPane.getRootFrame());
        database.setVisible(true);
    }

}
