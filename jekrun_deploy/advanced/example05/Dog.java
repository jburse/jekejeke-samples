package example05;

import jekpro.tools.call.Interpreter;

import java.io.IOException;
import java.io.Writer;

/**
 * Prolog code for the object oriented programming example.
 * Example 5: Call-Out Auto Loader
 * <p>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class Dog {

    public String barking() {
        return "woof";
    }

    public void bark() throws IOException {
        String voice = barking();
        Writer wr = (Writer) Interpreter.getInter().getProperty("sys_cur_output");
        wr.write(this + " says " + voice + ".\n");
        wr.flush();
    }

/*
?- example05/'Beagle':new(X), X::bark.
example05.Beagle@df379f7 says woof.
X = 0rdf379f7

?- example05/'Rottweiler':new(X), X::bark.
example05.Rottweiler@76e7b207 says ruff.
X = 0r76e7b207
*/

}