--
-- SQL code for the database schema.
--
-- Suitable for SQL Server 2008.
-- Works also with MySQL 8.0 form 2018
--
-- Warranty & Liability
-- To the extent permitted by applicable law and unless explicitly
-- otherwise agreed upon, XLOG Technologies GmbH makes no warranties
-- regarding the provided information. XLOG Technologies GmbH assumes
-- no liability that any problems might be solved with the information
-- provided by XLOG Technologies GmbH.
--
-- Rights & License
-- All industrial property rights regarding the information - copyright
-- and patent rights in particular - are the sole property of XLOG
-- Technologies GmbH. If the company was not the originator of some
-- excerpts, XLOG Technologies GmbH has at least obtained the right to
-- reproduce, change and translate the information.
--
-- Reproduction is restricted to the whole unaltered document.
-- Reproduction of the information is only allowed for non-commercial
-- uses. Selling, giving away or letting of the execution of the
-- library is prohibited. The library can be distributed as part of
-- your applications and libraries for execution provided this comment
-- remains unchanged.
--
-- Trademarks
-- Jekejeke is a registered trademark of XLOG Technologies GmbH.
--

CREATE TABLE demo.employee (
   firstname NVARCHAR(50) NOT NULL,
   name NVARCHAR(50) NOT NULL,
   age INTEGER NOT NULL,
   salary INTEGER NOT NULL);