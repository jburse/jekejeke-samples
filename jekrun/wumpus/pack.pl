/**
 * Simple text based dungeon game in Prolog.
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

/**
 * Obtained rights, copyright notice of BASIC version found
 * in The Best of Creative Computing Volume 1 (published 1976)
 * https://www.atariarchives.org/bcc1/showpage.php?page=247
 * and that we translated to Prolog.
 *
 * 0010  REM- HUNT THE WUMPUS
 * 0015  REM:  BY GREGORY YOB
 *
 * Game must have been create before, we assume 1972 since
 * the German Wikipedia mentions this date. The Englis Wikipedia
 * refers probably to a TI-99/4A version from 1973.
 */
name(wumpus).

title('Hunt The Wumpus').
version('1.0.0').

home('http://www.jekejeke.ch/').
author('(c) 2019, XLOG Technologies GmbH, Switzerland', 'info@xlog.ch').
author('(c) 1972, Gregory Yob, United States', 'unknown@unknown').
