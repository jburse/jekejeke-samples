package example01;

import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

import java.util.ArrayList;

/**
 * <p>Java code for the query interpreter.</p>
 * <p>Stripped down Version of the Swing version.</p>
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
public final class Query {
    private final static int COLUMN_NAME = 0;
    private final static int COLUMN_AGE = 1;
    private final static int COLUMN_COUNT = 2;

    private String name;
    private String agefrom;
    private String ageto;
    private Interpreter inter;
    private Object[][] rows;

    /**
     * <p>Create a query interpreter.</p>
     *
     * @param i The interpreter.
     */
    public Query(Interpreter i) {
        inter = i;
    }

    /**
     * <p>Set the name search criteria.</p>
     *
     * @param n The name search criteria.
     */
    public void setName(String n) {
        name = n;
    }

    /**
     * <p>Set the age from search criteria.</p>
     *
     * @param af The age from search criteria.
     */
    public void setAgeFrom(String af) {
        agefrom = af;
    }

    /**
     * <p>Set the age to search criteria.</p>
     *
     * @param at The age to search criteria.
     */
    public void setAgeTo(String at) {
        ageto = at;
    }

    /**
     * <p>List the column identifiers of the search result.</p>
     *
     * @return The column identifiers.
     */
    public String[] listColumnIdentifiers() {
        return new String[]{"Name", "Age#"};
    }

    /**
     * <p>Create the query variables.</p>
     *
     * @return The query variables.
     */
    public TermVar[] makeVars() {
        TermVar[] res = new TermVar[COLUMN_COUNT];
        for (int i = 0; i < res.length; i++)
            res[i] = new TermVar();
        return res;
    }

    /**
     * <p>Create the query term.</p>
     *
     * @param vars The query variables.
     * @return The query term.
     */
    public AbstractTerm makeQuery(TermVar[] vars) {
        ArrayList<AbstractTerm> literals = new ArrayList<AbstractTerm>();
        literals.add(new TermCompound(inter, "employee", vars[COLUMN_NAME], vars[COLUMN_AGE]));
        if (!"".equals(name))
            literals.add(0, new TermCompound(inter, "=",
                    name, vars[COLUMN_NAME]));
        if (!"".equals(agefrom))
            literals.add(new TermCompound(inter, "=<",
                    Integer.valueOf(agefrom), vars[COLUMN_AGE]));
        if (!"".equals(ageto))
            literals.add(new TermCompound(inter, ">=",
                    Integer.valueOf(ageto), vars[COLUMN_AGE]));
        AbstractTerm queryTerm = literals.get(literals.size() - 1);
        for (int i = literals.size() - 2; i >= 0; i--)
            queryTerm = new TermCompound(inter, ",",
                    literals.get(i), queryTerm);
        return queryTerm;
    }

    /**
     * <p>List the rows of the search result.</p>
     *
     * @param vars      The query variables.
     * @param queryTerm The query term.
     */
    public void listRows(TermVar[] vars, Object queryTerm) {
        try {
            ArrayList<Object[]>  res = new ArrayList<Object[]>();
            CallIn callin = inter.iterator(queryTerm);
            while (callin.hasNext()) {
                callin.next();
                Object[] row = new Object[]{
                        vars[Query.COLUMN_NAME].deref(),
                        vars[Query.COLUMN_AGE].deref()};
                res.add(row);
            }
            rows = new Object[res.size()][];
            res.toArray(rows);
        } catch (InterpreterMessage x) {
            throw new RuntimeException(x);
        } catch (InterpreterException x) {
            throw new RuntimeException(x);
        }
    }

    /**
     * <p>Retrieve the rows.</p>
     *
     * @return The rows.
     */
    public Object[][] getRows() {
        return rows;
    }

}
