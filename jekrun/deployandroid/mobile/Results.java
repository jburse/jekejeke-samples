package mobile;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import jekpro.tools.term.TermVar;

/**
 * <p>Java code for the results activity.</p>
 * <p>While the knowledge base is loading We use the
 * following layout with the buttons disabled:</p>
 * <pre>
 *       ( Search ) ( Close )
 *       +------------ ProgressBar -------------+
 *       |                                      |
 *       |                                      |
 *       +--------------------------------------+
 * <pre>
 * <p>After the knowledge base is loaded we use
 * the following layout with the buttons enabled:</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class Results extends Activity implements View.OnClickListener {
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

        search = new Button(this);
        search.setText("Search");
        search.setOnClickListener(this);

        LinearLayout.LayoutParams layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        LinearLayout layout2 = new LinearLayout(this);
        layout2.setOrientation(LinearLayout.HORIZONTAL);
        layout2.addView(search, layoutparams2);

        close = new Button(this);
        close.setText("Close");
        close.setOnClickListener(this);
        layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        layout2.addView(close, layoutparams2);

        progress = new ProgressBar(this);
        list = new ListView(Results.this);

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
                Data.initKnowledgebase();
            }
        }, new Runnable() {
            public void run() {
            }
        });
    }

    /**
     * <p>Start a job.</p>
     *
     * @param job  The job.
     * @param job2 The job2.
     */
    private void startJob(final Runnable job, final Runnable job2) {
        root.removeView(list);
        search.setEnabled(false);
        close.setEnabled(false);
        progress.setIndeterminate(true);
        LinearLayout.LayoutParams layoutparams4 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, 0, 1);
        root.addView(progress, layoutparams4);
        new Thread(new Runnable() {
            public void run() {
                job.run();
                root.post(new Runnable() {
                    public void run() {
                        job2.run();
                        root.removeView(progress);
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

        final Query query = new Query(Data.know);
        query.setName(data.getStringExtra("name"));
        query.setAgeFrom(data.getStringExtra("fromage"));
        query.setAgeTo(data.getStringExtra("toage"));
        final String[] cols = query.listColumnIdentifiers();
        final TermVar[] vars = query.makeVars();
        final Object goal = query.makeQuery(vars);

        startJob(new Runnable() {
            public void run() {
                query.listRows(vars, goal);
            }
        }, new Runnable() {
            public void run() {
                Adapter adapter = new Adapter(cols, query.getRows());
                list.setAdapter(adapter);
            }
        });
    }

}
