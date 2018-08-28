/**
 * Prolog code for the queried table.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_module(library(system/thread)).

% employee(Firstname, Name, Age, Salary)
employee('Сергей', 'Иванов', 53, 18500).
employee('Сергей', 'Беляев', 53, 19000) :-
   thread_sleep(5000).
employee('Hans', 'Fischer', 62, 21500).
employee('Филипп', 'Иванов', 32, 15000).
employee('Anna', 'Fischer', 36, 17000).
employee('Berta', 'Schmitt', 25, 14500).
employee('Carla', 'Fischer', 50, 22000).
employee('Κώστας', 'Βουτσάς', 22, 11000).
employee('Έλενα', 'Βουτσάς', 34, 16500).

