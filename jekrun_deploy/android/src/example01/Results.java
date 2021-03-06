package example01;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import jekpro.platform.headless.ToolkitLibrary;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;

/**
 * <p>Java code for the results activity.</p>
 * <p>While the knowledge base is loading or a query
 * is processed we use the following layout with the
 * buttons disabled:</p>
 * <pre>
 *       ( Search ) ( Close )
 *       +------------ Progress Bar ------------+
 *       |                                      |
 *       |                                      |
 *       +--------------------------------------+
 * <pre>
 * <p>After the knowledge base is loaded or a query
 * has been processed we use the following layout with
 * the buttons enabled:</p>
 * <pre>
 *       [ Search ] [ Close ]
 *       +-------------- List  -----------------+
 *       |                                      |
 *       |                                      |
 *       +--------------------------------------+
 * <pre>
 * <p>Compare to the Swing version no error handling or query preview.</p>
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
public final class Results extends Activity implements View.OnClickListener {
    private Knowledgebase know = new Knowledgebase(ToolkitLibrary.DEFAULT);
    private ListView list;
    private Button search;
    private Button close;
    private ProgressBar progress;
    private LinearLayout root;

    /**
     * <p>Called when the activity is first created.</p>
     */
    public void onCreate(Bundle bundle) {
        super.onCreate(bundle);

        LinearLayout layout2 = new LinearLayout(this);
        layout2.setOrientation(LinearLayout.HORIZONTAL);

        search = new Button(this);
        search.setText("Search");
        search.setOnClickListener(this);
        search.setEnabled(false);
        LinearLayout.LayoutParams layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        layout2.addView(search, layoutparams2);

        close = new Button(this);
        close.setText("Close");
        close.setOnClickListener(this);
        close.setEnabled(false);
        layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        layout2.addView(close, layoutparams2);

        progress = new ProgressBar(this);
        list = new ListView(Results.this, null, 0, android.R.style.Widget_ListView_White);

        root = new LinearLayout(this);
        root.setOrientation(LinearLayout.VERTICAL);
        layoutparams2 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        root.addView(layout2, layoutparams2);
        layoutparams2 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, 0, 1);
        root.addView(list, layoutparams2);
        setContentView(root);

        startJob(new Runnable() {
            public void run() {
                initKnowledgebase();
            }
        }, new Runnable() {
            public void run() {
            }
        });
    }

    /**
     * <p>Do set up of the knowledge base.</p>
     */
    private void initKnowledgebase() {
        try {
            /* setup the Prolog runtime */
            Interpreter inter = know.iterable();
            Knowledgebase.initKnowledgebase(inter);
            /* load the Prolog code */
            Object consultGoal = inter.parseTerm("consult(library(example01/table))");
            inter.iterator(consultGoal).next().close();
        } catch (InterpreterMessage x) {
            throw new RuntimeException(x);
        } catch (InterpreterException x) {
            throw new RuntimeException(x);
        }
    }

    /**
     * <p>Start a job.</p>
     *
     * @param job  The long running job.
     * @param job2 The GUI update job.
     */
    private void startJob(final Runnable job, final Runnable job2) {
        root.removeView(list);

        final LinearLayout layout2 = new LinearLayout(this);
        layout2.setGravity(Gravity.CENTER);
        layout2.addView(progress);
        progress.setIndeterminate(true);

        LinearLayout.LayoutParams layoutparams4 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, 0, 1);
        root.addView(layout2, layoutparams4);
        new Thread(new Runnable() {
            public void run() {
                job.run();
                root.post(new Runnable() {
                    public void run() {
                        job2.run();

                        layout2.removeView(progress);
                        root.removeView(layout2);
                        progress.setIndeterminate(false);

                        LinearLayout.LayoutParams layoutparams4 = new LinearLayout.LayoutParams(
                                ViewGroup.LayoutParams.FILL_PARENT, 0, 1);
                        root.addView(list, layoutparams4);
                        search.setEnabled(true);
                        close.setEnabled(true);
                    }
                });
            }
        }).start();
    }


    /**
     * <p>Button has been clicked.</p>
     *
     * @param view The button.
     */
    public void onClick(View view) {
        if (view == search) {
            Intent intent = new Intent(this, Criterias.class);
            startActivityForResult(intent, 0);
        } else if (view == close) {
            finish();
        } else {
            throw new IllegalArgumentException("illegal button");
        }
    }

    /**
     * <p>Handle return from an intent.</p>
     *
     * @param request The request code.
     * @param result  The result code.
     * @param data    The data.
     */
    protected void onActivityResult(int request, int result, Intent data) {
        if (result != RESULT_OK)
            return;

        Interpreter inter = know.iterable();
        final Query query = new Query(inter);
        query.setName(data.getStringExtra("name"));
        query.setAgeFrom(data.getStringExtra("fromage"));
        query.setAgeTo(data.getStringExtra("toage"));
        final String[] cols = query.listColumnIdentifiers();
        final TermVar[] vars = query.makeVars();
        final AbstractTerm goal = query.makeQuery(vars);

        search.setEnabled(false);
        close.setEnabled(false);
        startJob(new Runnable() {
            public void run() {
                query.listRows(vars, goal);
            }
        }, new Runnable() {
            public void run() {
                Object[][] rows = query.getRows();
                Adapter adapter = new Adapter(cols, rows);
                list.setAdapter(adapter);
            }
        });
    }

}
