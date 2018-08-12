package client;

import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;

/**
 * <p>Java code for the stub.</p>
 * <p/>
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
public final class Stub {
    private final static int COLUMN_FIRSTNAME = 0;
    private final static int COLUMN_NAME = 1;
    private final static int COLUMN_AGE = 2;
    private final static int COLUMN_SALARY = 3;
    private final static int COLUMN_COUNT = 4;

    private String firstname;
    private String name;
    private String agefrom;
    private String ageto;
    private String salaryfrom;
    private String salaryto;
    private final String functor;
    private Interpreter inter;
    private Object[][] rows;

    /**
     * <p>Create a stub for a given functor.</p>
     *
     * @param f The functor.
     * @param i The interpreter.
     */
    public Stub(String f, Interpreter i) {
        functor = f;
        inter = i;
    }

    /**
     * <p>Set the first name search criteria.</p>
     *
     * @param f The first name search criteria.
     */
    public void setFirstname(String f) {
        firstname = f;
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
     * <p>Set the salary from search criteria.</p>
     *
     * @param af The salary from search criteria.
     */
    public void setSalaryFrom(String af) {
        salaryfrom = af;
    }

    /**
     * <p>Set the salary to search criteria.</p>
     *
     * @param at The salary to search criteria.
     */
    public void setSalaryTo(String at) {
        salaryto = at;
    }

    /**
     * <p>List the column identifiers of the search result.</p>
     *
     * @return The column identifiers.
     */
    public String[] listColumnIdentifiers() {
        return new String[]{"Firstname", "Name", "Age#", "Salary#"};
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
        Object[] args = new Object[7];
        args[0] = firstname;
        args[1] = name;
        args[2] = agefrom;
        args[3] = ageto;
        args[4] = salaryfrom;
        args[5] = salaryto;
        args[6] = new TermCompound(inter, "employee", vars[COLUMN_FIRSTNAME],
                vars[COLUMN_NAME], vars[COLUMN_AGE], vars[COLUMN_SALARY]);
        return new TermCompound(inter, functor, args);
    }

    /**
     * <p>Make a variable names map.</p>
     *
     * @param colids The column ids.
     * @param vars   The variables.
     * @return The variable names map.
     */
    public Object makeVariableNames(String[] colids, TermVar[] vars) {
        Object res = Knowledgebase.OP_NIL;
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound("context", Integer.valueOf(0)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound("back_quotes", "variable"), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound("quoted", "true"), res);

        Object list = Knowledgebase.OP_NIL;
        for (int i = 0; i < vars.length; i++) {
            list = new TermCompound(inter, Knowledgebase.OP_CONS,
                    new TermCompound(inter, "=", colids[i], vars[i]), list);
        }

        res = new TermCompound(inter, Knowledgebase.OP_CONS,
                new TermCompound(inter, "variable_names", list), res);
        return res;
    }

    /**
     * <p>List the rows of the search result.</p>
     *
     * @param vars      The query variables.
     * @param queryTerm The query term.
     * @param inter     The interpreter.
     */
    public void listRows(TermVar[] vars, Object queryTerm, Interpreter inter) {
        try {
        ArrayList<Object[]> res = new ArrayList<Object[]>();
        CallIn callin = inter.iterator(queryTerm);
        while (callin.hasNext()) {
            callin.next();
            Object[] row = new Object[]{
                    vars[COLUMN_FIRSTNAME].deref(),
                    vars[COLUMN_NAME].deref(),
                    vars[COLUMN_AGE].deref(),
                    vars[COLUMN_SALARY].deref()
            };
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

    /**
     * <p>Java foreign predicate url_encode_utf8/2.</p>
     *
     * @param s The string.
     * @return The encoded string.
     * @throws InterpreterMessage Encoding missing.
     */
    public static String urlEncodeUTF8(String s)
            throws InterpreterMessage {
        try {
            return URLEncoder.encode(s, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new InterpreterMessage(InterpreterMessage.existenceError(
                    "encoding", "UTF-8"));
        }
    }

}
