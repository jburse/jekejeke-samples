package example01;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.text.InputType;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.widget.*;

/**
 * <p>Java code for the criterias activity.</p>
 * <p>We use the following layout:</p>
 * <pre>
 *       Name:      [          ]
 *       From Age:  [   ]
 *       To Age:    [   ]
 *       [ Ok ] [ Cancel ]
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
public final class Criterias extends Activity implements View.OnClickListener {
    private EditText name;
    private EditText fromage;
    private EditText toage;
    private Button ok;
    private Button cancel;

    /**
     * <p>Called when the activity is first created.</p>
     */
    public void onCreate(Bundle bundle) {
        super.onCreate(bundle);

        TextView labelname = new TextView(this);
        labelname.setTextAppearance(this,
                  android.R.style.TextAppearance_Medium);
        labelname.setText("Name:");
        TableRow.LayoutParams layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT);
        layoutparams.setMargins(0, 0, 12, 0);
        TableRow row = new TableRow(this);
        row.addView(labelname, layoutparams);

        name = new EditText(this);
        name.setWidth((int) (name.getPaint().measureText("#") * 10));
        name.setInputType(InputType.TYPE_CLASS_TEXT |
                InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS |
                InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD);
        name.setImeOptions(0x1000000 | /* flagNoPersonalizedLearning */
                EditorInfo.IME_FLAG_NAVIGATE_NEXT |
                EditorInfo.IME_FLAG_NO_FULLSCREEN |
                EditorInfo.IME_ACTION_NEXT);
        name.setFocusable(true);

        layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT);
        row.addView(name, layoutparams);

        TableLayout layout = new TableLayout(this);
        layout.setPadding(12, 12, 12, 12);
        TableLayout.LayoutParams layoutparams3 = new TableLayout.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layout.addView(row, layoutparams3);

        TextView labelfromage = new TextView(this);
        labelfromage.setTextAppearance(this, android.R.style.TextAppearance_Medium);
        labelfromage.setText("From Age:");
        layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layoutparams.setMargins(0, 0, 12, 0);
        row = new TableRow(this);
        row.addView(labelfromage, layoutparams);

        fromage = new EditText(this);
        fromage.setWidth((int) (fromage.getPaint().measureText("#") * 5));
        fromage.setInputType(InputType.TYPE_CLASS_TEXT |
                InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS |
                InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD);
        fromage.setImeOptions(0x1000000 | /* flagNoPersonalizedLearning */
                EditorInfo.IME_FLAG_NAVIGATE_NEXT |
                EditorInfo.IME_FLAG_NO_FULLSCREEN |
                EditorInfo.IME_ACTION_NEXT);
        fromage.setGravity(Gravity.RIGHT);

        layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        row.addView(fromage, layoutparams);
        layoutparams3 = new TableLayout.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layout.addView(row, layoutparams3);

        TextView labeltoage = new TextView(this);
        labeltoage.setTextAppearance(this, android.R.style.TextAppearance_Medium);
        labeltoage.setText("To Age:");
        layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layoutparams.setMargins(0, 0, 12, 0);
        row = new TableRow(this);
        row.addView(labeltoage, layoutparams);

        toage = new EditText(this);
        toage.setWidth((int) (toage.getPaint().measureText("#") * 5));
        toage.setInputType(InputType.TYPE_CLASS_TEXT |
                InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS |
                InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD);
        toage.setImeOptions(0x1000000 | /* flagNoPersonalizedLearning */
                EditorInfo.IME_FLAG_NAVIGATE_NEXT |
                EditorInfo.IME_FLAG_NO_FULLSCREEN |
                EditorInfo.IME_ACTION_NEXT);
        toage.setFocusable(true);
        toage.setGravity(Gravity.RIGHT);

        layoutparams = new TableRow.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        row.addView(toage, layoutparams);
        layoutparams3 = new TableLayout.LayoutParams(
                ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layout.addView(row, layoutparams3);

        ok = new Button(this);
        ok.setText("Ok");
        ok.setOnClickListener(this);
        LinearLayout.LayoutParams layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        LinearLayout layout2 = new LinearLayout(this);
        layout2.setOrientation(LinearLayout.HORIZONTAL);
        layout2.addView(ok, layoutparams2);

        cancel = new Button(this);
        cancel.setText("Cancel");
        cancel.setOnClickListener(this);
        layoutparams2 = new LinearLayout.LayoutParams(
                0, ViewGroup.LayoutParams.WRAP_CONTENT, 1);
        layout2.addView(cancel, layoutparams2);

        LinearLayout layout3 = new LinearLayout(this);
        layout3.setOrientation(LinearLayout.VERTICAL);
        layoutparams2 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, 0, 1);
        layout3.addView(layout, layoutparams2);
        layoutparams2 = new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.FILL_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
        layout3.addView(layout2, layoutparams2);

        setContentView(layout3);
    }

    /**
     * <p>Button has been clicked.</p>
     *
     * @param view The button.
     */
    public void onClick(View view) {
        if (view == ok) {
            Intent intent = new Intent();
            intent.putExtra("name", name.getText().toString());
            intent.putExtra("fromage", fromage.getText().toString());
            intent.putExtra("toage", toage.getText().toString());
            setResult(RESULT_OK, intent);
            finish();
        } else if (view == cancel) {
            finish();
        } else {
            throw new IllegalArgumentException("illegal button");
        }
    }

}
