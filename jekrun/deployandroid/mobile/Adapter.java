package mobile;

import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * <p>This class provides an adapter for the results list.</p>
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
public final class Adapter extends BaseAdapter {
    private final String[] cols;
    private final Object[][] rows;

    /**
     * <p>Create a results adapter.</p>
     *
     * @param c The column identifiers.
     * @param r The data rows.
     */
    public Adapter(String[] c, Object[][] r) {
        cols = c;
        rows = r;
    }

    /**
     * <p>Retrieve the number of rows.</p>
     *
     * @return The number of rows.
     */
    public int getCount() {
        return rows.length;
    }

    /**
     * <p>Retrieve a row.</p>
     *
     * @param i The row index.
     * @return The row.
     */
    public Object getItem(int i) {
        return rows[i];
    }

    /**
     * <p>Retrieve a row key.</p>
     *
     * @param i The row index.
     * @return The row key.
     */
    public long getItemId(int i) {
        return i;
    }

    /**
     * <p>Populate view with row data.</p>
     *
     * @param i         The row index.
     * @param view      The view or null.
     * @param viewGroup The view group.
     * @return The populated view.
     */
    public View getView(int i, View view, ViewGroup viewGroup) {
        if (view == null) {
            LinearLayout layout2 = new LinearLayout(viewGroup.getContext());
            layout2.setOrientation(LinearLayout.HORIZONTAL);
            for (int j = 0; j < cols.length; j++) {
                String col = cols[j];
                TextView text = new TextView(viewGroup.getContext());
                LinearLayout.LayoutParams layoutparams2;
                if (col.endsWith("#")) {
                    layoutparams2 = new LinearLayout.LayoutParams(
                            0, ViewGroup.LayoutParams.WRAP_CONTENT, 5);
                    text.setGravity(Gravity.RIGHT);
                } else {
                    layoutparams2 = new LinearLayout.LayoutParams(
                            0, ViewGroup.LayoutParams.WRAP_CONTENT, 10);
                }
                layout2.addView(text, layoutparams2);
            }
            layout2.setPadding(10, 5, 10, 5);
            view = layout2;
        }
        Object[] row = rows[i];
        for (int j = 0; j < row.length; j++) {
            TextView text = (TextView) ((ViewGroup) view).getChildAt(j);
            text.setText(row[j].toString());
        }
        return view;
    }

}
