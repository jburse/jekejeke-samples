package example01;

import jekpro.tools.call.Interpreter;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermVar;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * <p>Java code for the HTTP servlet.</p>
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
public final class Plain extends HttpServlet {

    /**
     * <p>Query the table and return plain results.</p>
     *
     * @param request  The HTTP request.
     * @param response The HTTP response.
     * @throws IOException Shit happens.
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        response.setContentType("text/plain; charset=UTF-8");
        request.setCharacterEncoding("UTF-8");
        String firstname = request.getParameter("firstname");
        String name = request.getParameter("name");
        String agefrom = request.getParameter("agefrom");
        String ageto = request.getParameter("ageto");
        String salaryfrom = request.getParameter("salaryfrom");
        String salaryto = request.getParameter("salaryto");
        PrintWriter wr = response.getWriter();
        try {
            Knowledgebase know = Data.getKnowledgebase();
            Interpreter inter = know.iterable();

            Query query = new Query(inter);
            query.setFirstname((firstname != null ? firstname : ""));
            query.setName((name != null ? name : ""));
            query.setAgeFrom((agefrom != null ? agefrom : ""));
            query.setAgeTo((ageto != null ? ageto : ""));
            query.setSalaryFrom((salaryfrom != null ? salaryfrom : ""));
            query.setSalaryTo((salaryto != null ? salaryto : ""));

            TermVar[] vars = query.makeVars();
            Object queryTerm = query.makeQuery(vars);
            query.listRows(vars, queryTerm);

            String[] cols = query.listColumnIdentifiers();
            for (int i = 0; i < cols.length; i++) {
                if (i != 0)
                    wr.print('\t');
                wr.print(cols[i]);
            }
            wr.println();

            Object[][] rows = query.getRows();
            for (int j = 0; j < rows.length; j++) {
                /* flesh out a table row */
                Object[] row = rows[j];
                for (int i = 0; i < row.length; i++) {
                    if (i != 0)
                        wr.print('\t');
                    wr.print(row[i]);
                }
                wr.println();
            }

        } catch (Exception x) {
            wr.println(x.toString());
        }
    }

}