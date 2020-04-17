package example06;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.sql.*;
import java.util.Properties;

/**
 * <p>Java code for the API of the JDBC example.</p>
 * <p>This class provides an API to statement objects.</p>
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
public final class StatementAPI {
    private final static Properties info = new Properties();
    private final static Driver driver;
    private final static String OP_SQL_STATEMENT = "sql_statement";

    static {
        try {
            info.put("user", "xxx");
            info.put("password", "xxx");
            Class<?> clazz = Class.forName("com.mysql.cj.jdbc.Driver");
            driver = (Driver) clazz.newInstance();
        } catch (ClassNotFoundException x) {
            throw new RuntimeException(x);
        } catch (InstantiationException x) {
            throw new RuntimeException(x);
        } catch (IllegalAccessException x) {
            throw new RuntimeException(x);
        }
    }

    /**
     * <p>Create a new database connection and create a new statement object.</p>
     *
     * @return The statement.
     * @throws InterpreterMessage If object could not be created.
     */
    public static Statement createStatement()
            throws InterpreterMessage {
        Connection con = null;
        try {
            con = driver.connect("jdbc:mysql://localhost:3306/demo?useSSL=false&serverTimezone=Europe/Paris", info);
            return con.createStatement();
        } catch (SQLException x) {
            if (con != null) {
                try {
                    con.close();
                } catch (SQLException y) {
                    /* */
                }
            }
            throw new InterpreterMessage(
                    InterpreterMessage.existenceError(OP_SQL_STATEMENT, x.getMessage()));
        }
    }

    /**
     * <p>Use the statement object to execute the given select statement
     * and retrieve column values for each result row.</p>
     *
     * @param co     The call out.
     * @param obj    The statement.
     * @param select The select.
     * @return The column values or null.
     * @throws InterpreterMessage If select could not be executed or column values not retrieved.
     */
    public static Object executeQuery(CallOut co,
                                      Statement obj, String select)
            throws InterpreterMessage {
        try {
            ResultSet set;
            if (co.getFirst()) {
                set = obj.executeQuery(select);
                co.setData(set);
            } else {
                set = (ResultSet) co.getData();
            }
            if (set.next()) {
                ResultSetMetaData meta = set.getMetaData();
                Object valList = Knowledgebase.OP_NIL;
                for (int i = meta.getColumnCount(); i >= 1; i--) {
                    Object col = set.getObject(i);
                    Object val;
                    if (col instanceof String) {
                        val = col;
                    } else if (col instanceof Integer) {
                        val = col;
                    } else {
                        throw new InterpreterMessage(
                            InterpreterMessage.representationError(OP_SQL_STATEMENT));
                    }
                    valList = new TermCompound(Knowledgebase.OP_CONS,
                            val, valList);
                }
                co.setRetry(true);
                return valList;
            }
            return null;
        } catch (SQLException x) {
            throw new InterpreterMessage(
                InterpreterMessage.resourceError(OP_SQL_STATEMENT));
        }
    }

    /**
     * <p>Close the statement object and the database connection.</p>
     *
     * @param obj The statement.
     * @throws InterpreterMessage If object could not be closed.
     */
    public static void closeStatement(Statement obj)
            throws InterpreterMessage {
        Connection con = null;
        try {
            con = obj.getConnection();
            obj.close();
            con.close();
        } catch (SQLException x) {
            if (con != null) {
                try {
                    con.close();
                } catch (SQLException y) {
                    /* */
                }
            }
            throw new InterpreterMessage(
                    InterpreterMessage.resourceError(OP_SQL_STATEMENT));
        }
    }

    /**
     * <p>Generate the SQL string from a string.</p>
     * <p>Will double quotes (').</p>
     * <p>Assumption is a latin 1 code page.</p>
     * <p>When there is a non latin 1 character, prepend by N.</p>
     *
     * @param s The string literal.
     * @return The encoded string literal.
     */
    public static String literalEncode(String s) {
        StringBuilder buf = null;
        int n = s.length();
        boolean nonlatin1 = false;
        for (int i = 0; i < n; i++) {
            int ch = s.charAt(i);
            nonlatin1 = nonlatin1 || (ch >= 256 || ch < 32);
            if (ch == '\'') {
                if (buf == null) {
                    buf = new StringBuilder();
                    buf.append(s.substring(0, i));
                }
                buf.append("''");
            } else {
                if (buf != null)
                    buf.append((char) ch);
            }
        }
        if (buf != null)
            s = buf.toString();
        if (nonlatin1) {
            return "N'" + s + "'";
        } else {
            return "'" + s + "'";
        }
    }

}
