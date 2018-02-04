module  Language.Haskell2010.Parsing  where
import qualified Control.Monad as Monad


type Pos = (Int, Int)
type AS = Pos
type BACKQUOTE = Pos
type CASE = Pos
type CLASS = Pos
type COLON_COLON = Pos
type COMMA = Pos
type DARROW = Pos
type DATA = Pos
type DEFAULT = Pos
type DERIVING = Pos
type DO = Pos
type DOT_DOT = Pos
type ELSE = Pos
type EQUAL = Pos
type EXCL = Pos
type EXPORT = Pos
type FOREIGN = Pos
type HIDING = Pos
type IMPORT = Pos
type IF = Pos
type IN = Pos
type INFIX = Pos
type INFIXL = Pos
type INFIXR = Pos
type INSTANCE = Pos
type INTEGER = (Pos, Integer)
type LAMBDA = Pos
type LARROW = Pos
type LBRACE = Pos
type LBRACKET = Pos
type LET = Pos
type LPAREN = Pos
type MINUS = Pos
type MODULE = Pos
type NEWTYPE = Pos
type OF = Pos
type PIPE = Pos
type QCONID = (Pos, String)
type QCONSYM = (Pos, String)
type QUALIFIED = Pos
type QVARID = (Pos, String)
type QVARSYM = (Pos, String)
type RBRACE = Pos
type RBRACKET = Pos
type RARROW = Pos
type RPAREN = Pos
type SEMICOLON = Pos
type STRING = (Pos, String)
type THEN = Pos
type TYPE = Pos
type WHERE = Pos

data Token =
    AS AS
  | BACKQUOTE BACKQUOTE
  | CASE CASE
  | CLASS CLASS
  | COLON_COLON COLON_COLON
  | COMMA COMMA
  | DARROW DARROW
  | DATA DATA
  | DEFAULT DEFAULT
  | DERIVING DERIVING
  | DO DO
  | DOT_DOT DOT_DOT
  | ELSE ELSE
  | EQUAL EQUAL
  | EXCL EXCL
  | EXPORT EXPORT
  | FOREIGN FOREIGN
  | HIDING HIDING
  | IF IF
  | IMPORT IMPORT
  | IN IN
  | INFIX INFIX
  | INFIXL INFIXL
  | INFIXR INFIXR
  | INSTANCE INSTANCE
  | INTEGER INTEGER
  | LAMBDA LAMBDA
  | LARROW LARROW
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | LET LET
  | LPAREN LPAREN
  | MINUS MINUS
  | MODULE MODULE
  | NEWTYPE NEWTYPE
  | OF OF
  | PIPE PIPE
  | QCONID QCONID
  | QCONSYM QCONSYM
  | QUALIFIED QUALIFIED
  | QVARID QVARID
  | QVARSYM QVARSYM
  | RARROW RARROW
  | RBRACE RBRACE
  | RBRACKET RBRACKET
  | RPAREN RPAREN
  | SEMICOLON SEMICOLON
  | STRING STRING
  | THEN THEN
  | TYPE TYPE
  | WHERE WHERE
  deriving (Eq, Ord, Read, Show)

data Action = Shift Int | Reduce Int Int | Accept
type ActionState = Int
data ActionSymbol = Token Token | EOF
type GotoState = Int
type GotoSymbol = Int

data Module' =
    Module'_implies_MODULE_modid_exports_opt_WHERE_body MODULE Modid Exports_opt WHERE Body
  | Module'_implies_body Body
  deriving (Eq, Ord, Read, Show)

data Modid =
    Modid_implies_QCONID QCONID
  deriving (Eq, Ord, Read, Show)

data Exports_opt =
    Exports_opt_implies
  | Exports_opt_implies_exports Exports
  deriving (Eq, Ord, Read, Show)

data Body =
    Body_implies_LBRACE_topdecls_RBRACE LBRACE Topdecls RBRACE
  deriving (Eq, Ord, Read, Show)

data Topdecls =
    Topdecls_implies_topdecl Topdecl
  | Topdecls_implies_topdecl_SEMICOLON_topdecls Topdecl SEMICOLON Topdecls
  deriving (Eq, Ord, Read, Show)

data Exports =
    Exports_implies_LPAREN_export_seq_RPAREN LPAREN Export_seq RPAREN
  deriving (Eq, Ord, Read, Show)

data Export_seq =
    Export_seq_implies
  | Export_seq_implies_export Export
  | Export_seq_implies_export_COMMA_export_seq Export COMMA Export_seq
  deriving (Eq, Ord, Read, Show)

data Export =
    Export_implies_var Var
  | Export_implies_con Con
  | Export_implies_con_LPAREN_RPAREN Con LPAREN RPAREN
  | Export_implies_con_LPAREN_DOT_DOT_RPAREN Con LPAREN DOT_DOT RPAREN
  | Export_implies_con_LPAREN_cname_seq_RPAREN Con LPAREN Cname_seq RPAREN
  | Export_implies_MODULE_modid MODULE Modid
  deriving (Eq, Ord, Read, Show)

data Var =
    Var_implies_AS AS
  | Var_implies_EXPORT EXPORT
  | Var_implies_QVARID QVARID
  | Var_implies_LPAREN_MINUS_RPAREN LPAREN MINUS RPAREN
  | Var_implies_LPAREN_QVARSYM_RPAREN LPAREN QVARSYM RPAREN
  deriving (Eq, Ord, Read, Show)

data Con =
    Con_implies_QCONID QCONID
  | Con_implies_LPAREN_QCONSYM_RPAREN LPAREN QCONSYM RPAREN
  deriving (Eq, Ord, Read, Show)

data Cname_seq =
    Cname_seq_implies_cname Cname
  | Cname_seq_implies_cname_COMMA_cname_seq Cname COMMA Cname_seq
  deriving (Eq, Ord, Read, Show)

data Import_seq =
    Import_seq_implies
  | Import_seq_implies_import' Import'
  | Import_seq_implies_import'_COMMA_import_seq Import' COMMA Import_seq
  deriving (Eq, Ord, Read, Show)

data Import' =
    Import'_implies_var Var
  | Import'_implies_con Con
  | Import'_implies_con_LPAREN_RPAREN Con LPAREN RPAREN
  | Import'_implies_con_LPAREN_DOT_DOT_RPAREN Con LPAREN DOT_DOT RPAREN
  | Import'_implies_con_LPAREN_cname_seq_RPAREN Con LPAREN Cname_seq RPAREN
  deriving (Eq, Ord, Read, Show)

data Cname =
    Cname_implies_var Var
  | Cname_implies_con Con
  deriving (Eq, Ord, Read, Show)

data Topdecl =
    Topdecl_implies_IMPORT_qualified_opt_modid_as_opt IMPORT Qualified_opt Modid As_opt
  | Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_LPAREN_import_seq_RPAREN IMPORT Qualified_opt Modid As_opt LPAREN Import_seq RPAREN
  | Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_HIDING_LPAREN_import_seq_RPAREN IMPORT Qualified_opt Modid As_opt HIDING LPAREN Import_seq RPAREN
  | Topdecl_implies_TYPE_btype_EQUAL_type' TYPE Btype EQUAL Type'
  | Topdecl_implies_DATA_btype_constrs_opt DATA Btype Constrs_opt
  | Topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass DATA Btype Constrs_opt DERIVING Dclass
  | Topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN DATA Btype Constrs_opt DERIVING LPAREN RPAREN
  | Topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN DATA Btype Constrs_opt DERIVING LPAREN Dclass_seq RPAREN
  | Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt DATA Btype DARROW Btype Constrs_opt
  | Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass DATA Btype DARROW Btype Constrs_opt DERIVING Dclass
  | Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN DATA Btype DARROW Btype Constrs_opt DERIVING LPAREN RPAREN
  | Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN DATA Btype DARROW Btype Constrs_opt DERIVING LPAREN Dclass_seq RPAREN
  | Topdecl_implies_NEWTYPE_btype_newconstr NEWTYPE Btype Newconstr
  | Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass NEWTYPE Btype Newconstr DERIVING Dclass
  | Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN NEWTYPE Btype Newconstr DERIVING LPAREN RPAREN
  | Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN NEWTYPE Btype Newconstr DERIVING LPAREN Dclass_seq RPAREN
  | Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr NEWTYPE Btype DARROW Btype Newconstr
  | Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass NEWTYPE Btype DARROW Btype Newconstr DERIVING Dclass
  | Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN NEWTYPE Btype DARROW Btype Newconstr DERIVING LPAREN RPAREN
  | Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN NEWTYPE Btype DARROW Btype Newconstr DERIVING LPAREN Dclass_seq RPAREN
  | Topdecl_implies_CLASS_btype_cdecls_opt CLASS Btype Cdecls_opt
  | Topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt CLASS Btype DARROW Btype Cdecls_opt
  | Topdecl_implies_INSTANCE_btype_idecls_opt INSTANCE Btype Idecls_opt
  | Topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt INSTANCE Btype DARROW Btype Idecls_opt
  | Topdecl_implies_DEFAULT_LPAREN_RPAREN DEFAULT LPAREN RPAREN
  | Topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN DEFAULT LPAREN Type_seq RPAREN
  | Topdecl_implies_FOREIGN_fdecl FOREIGN Fdecl
  | Topdecl_implies_decl Decl
  deriving (Eq, Ord, Read, Show)

data Qualified_opt =
    Qualified_opt_implies
  | Qualified_opt_implies_QUALIFIED QUALIFIED
  deriving (Eq, Ord, Read, Show)

data As_opt =
    As_opt_implies
  | As_opt_implies_AS_modid AS Modid
  deriving (Eq, Ord, Read, Show)

data Btype =
    Btype_implies_atype Atype
  | Btype_implies_btype_atype Btype Atype
  deriving (Eq, Ord, Read, Show)

data Type' =
    Type'_implies_btype Btype
  | Type'_implies_btype_RARROW_type' Btype RARROW Type'
  deriving (Eq, Ord, Read, Show)

data Constrs_opt =
    Constrs_opt_implies
  | Constrs_opt_implies_EQUAL_constrs EQUAL Constrs
  deriving (Eq, Ord, Read, Show)

data Dclass =
    Dclass_implies_QCONID QCONID
  deriving (Eq, Ord, Read, Show)

data Dclass_seq =
    Dclass_seq_implies_dclass Dclass
  | Dclass_seq_implies_dclass_COMMA_dclass_seq Dclass COMMA Dclass_seq
  deriving (Eq, Ord, Read, Show)

data Newconstr =
    Newconstr_implies_EQUAL_con_atype EQUAL Con Atype
  | Newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE EQUAL Con LBRACE Var COLON_COLON Type' RBRACE
  deriving (Eq, Ord, Read, Show)

data Cdecls_opt =
    Cdecls_opt_implies
  | Cdecls_opt_implies_WHERE_cdecls WHERE Cdecls
  deriving (Eq, Ord, Read, Show)

data Idecls_opt =
    Idecls_opt_implies
  | Idecls_opt_implies_WHERE_idecls WHERE Idecls
  deriving (Eq, Ord, Read, Show)

data Type_seq =
    Type_seq_implies_type' Type'
  | Type_seq_implies_type'_COMMA_type_seq Type' COMMA Type_seq
  deriving (Eq, Ord, Read, Show)

data Fdecl =
    Fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' IMPORT Callconv Impent Var COLON_COLON Type'
  | Fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' IMPORT Callconv Safety Impent Var COLON_COLON Type'
  | Fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' EXPORT Callconv Expent Var COLON_COLON Type'
  deriving (Eq, Ord, Read, Show)

data Decl =
    Decl_implies_gendecl Gendecl
  | Decl_implies_pat_EQUAL_exp Pat EQUAL Exp
  | Decl_implies_pat_EQUAL_exp_WHERE_decls Pat EQUAL Exp WHERE Decls
  | Decl_implies_pat_PIPE_gdrhs Pat PIPE Gdrhs
  | Decl_implies_pat_PIPE_gdrhs_WHERE_decls Pat PIPE Gdrhs WHERE Decls
  deriving (Eq, Ord, Read, Show)

data Decls =
    Decls_implies_LBRACE_decl_seq_RBRACE LBRACE Decl_seq RBRACE
  deriving (Eq, Ord, Read, Show)

data Decl_seq =
    Decl_seq_implies_decl Decl
  | Decl_seq_implies_decl_SEMICOLON_decl_seq Decl SEMICOLON Decl_seq
  deriving (Eq, Ord, Read, Show)

data Gendecl =
    Gendecl_implies
  | Gendecl_implies_vars_COLON_COLON_type' Vars COLON_COLON Type'
  | Gendecl_implies_vars_COLON_COLON_btype_DARROW_type' Vars COLON_COLON Btype DARROW Type'
  | Gendecl_implies_fixity_integer_opt_ops Fixity Integer_opt Ops
  deriving (Eq, Ord, Read, Show)

data Pat =
    Pat_implies_apat Apat
  | Pat_implies_pat_apat Pat Apat
  | Pat_implies_pat_MINUS_apat Pat MINUS Apat
  | Pat_implies_pat_op_apat Pat Op Apat
  deriving (Eq, Ord, Read, Show)

data Exp =
    Exp_implies_infixexp Infixexp
  deriving (Eq, Ord, Read, Show)

data Gdrhs =
    Gdrhs_implies_guards_EQUAL_exp Guards EQUAL Exp
  | Gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs Guards EQUAL Exp PIPE Gdrhs
  deriving (Eq, Ord, Read, Show)

data Cdecls =
    Cdecls_implies_LBRACE_cdecl_seq_RBRACE LBRACE Cdecl_seq RBRACE
  deriving (Eq, Ord, Read, Show)

data Cdecl_seq =
    Cdecl_seq_implies_cdecl Cdecl
  | Cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq Cdecl SEMICOLON Cdecl_seq
  deriving (Eq, Ord, Read, Show)

data Cdecl =
    Cdecl_implies_gendecl Gendecl
  | Cdecl_implies_pat_EQUAL_exp Pat EQUAL Exp
  | Cdecl_implies_pat_EQUAL_exp_WHERE_decls Pat EQUAL Exp WHERE Decls
  | Cdecl_implies_pat_PIPE_gdrhs Pat PIPE Gdrhs
  | Cdecl_implies_pat_PIPE_gdrhs_WHERE_decls Pat PIPE Gdrhs WHERE Decls
  deriving (Eq, Ord, Read, Show)

data Idecls =
    Idecls_implies_LBRACE_idecl_seq_RBRACE LBRACE Idecl_seq RBRACE
  deriving (Eq, Ord, Read, Show)

data Idecl_seq =
    Idecl_seq_implies_idecl Idecl
  | Idecl_seq_implies_idecl_SEMICOLON_idecl_seq Idecl SEMICOLON Idecl_seq
  deriving (Eq, Ord, Read, Show)

data Idecl =
    Idecl_implies
  | Idecl_implies_pat_EQUAL_exp Pat EQUAL Exp
  | Idecl_implies_pat_EQUAL_exp_WHERE_decls Pat EQUAL Exp WHERE Decls
  | Idecl_implies_pat_PIPE_gdrhs Pat PIPE Gdrhs
  | Idecl_implies_pat_PIPE_gdrhs_WHERE_decls Pat PIPE Gdrhs WHERE Decls
  deriving (Eq, Ord, Read, Show)

data Vars =
    Vars_implies_var Var
  | Vars_implies_var_COMMA_vars Var COMMA Vars
  deriving (Eq, Ord, Read, Show)

data Fixity =
    Fixity_implies_INFIXL INFIXL
  | Fixity_implies_INFIXR INFIXR
  | Fixity_implies_INFIX INFIX
  deriving (Eq, Ord, Read, Show)

data Integer_opt =
    Integer_opt_implies
  | Integer_opt_implies_INTEGER INTEGER
  deriving (Eq, Ord, Read, Show)

data Ops =
    Ops_implies_MINUS MINUS
  | Ops_implies_op Op
  | Ops_implies_MINUS_COMMA_ops MINUS COMMA Ops
  | Ops_implies_op_COMMA_ops Op COMMA Ops
  deriving (Eq, Ord, Read, Show)

data Op =
    Op_implies_varop Varop
  | Op_implies_conop Conop
  deriving (Eq, Ord, Read, Show)

data Atype =
    Atype_implies_gtycon Gtycon
  | Atype_implies_tyvar Tyvar
  | Atype_implies_LPAREN_type_seq2_RPAREN LPAREN Type_seq2 RPAREN
  | Atype_implies_LBRACKET_type'_RBRACKET LBRACKET Type' RBRACKET
  | Atype_implies_LPAREN_type'_RPAREN LPAREN Type' RPAREN
  | Atype_implies_EXCL_atype EXCL Atype
  deriving (Eq, Ord, Read, Show)

data Gtycon =
    Gtycon_implies_con Con
  | Gtycon_implies_LPAREN_RPAREN LPAREN RPAREN
  | Gtycon_implies_LBRACKET_RBRACKET LBRACKET RBRACKET
  | Gtycon_implies_LPAREN_RARROW_RPAREN LPAREN RARROW RPAREN
  | Gtycon_implies_LPAREN_comma_list_RPAREN LPAREN Comma_list RPAREN
  deriving (Eq, Ord, Read, Show)

data Tyvar =
    Tyvar_implies_AS AS
  | Tyvar_implies_EXPORT EXPORT
  | Tyvar_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Type_seq2 =
    Type_seq2_implies_type'_COMMA_type' Type' COMMA Type'
  | Type_seq2_implies_type'_COMMA_type_seq2 Type' COMMA Type_seq2
  deriving (Eq, Ord, Read, Show)

data Comma_list =
    Comma_list_implies_COMMA COMMA
  | Comma_list_implies_COMMA_comma_list COMMA Comma_list
  deriving (Eq, Ord, Read, Show)

data Constrs =
    Constrs_implies_constr Constr
  | Constrs_implies_constr_PIPE_constrs Constr PIPE Constrs
  deriving (Eq, Ord, Read, Show)

data Constr =
    Constr_implies_btype Btype
  | Constr_implies_btype_conop_btype Btype Conop Btype
  | Constr_implies_con_LBRACE_RBRACE Con LBRACE RBRACE
  | Constr_implies_con_LBRACE_fielddecl_seq_RBRACE Con LBRACE Fielddecl_seq RBRACE
  deriving (Eq, Ord, Read, Show)

data Conop =
    Conop_implies_QCONSYM QCONSYM
  | Conop_implies_BACKQUOTE_QCONID_BACKQUOTE BACKQUOTE QCONID BACKQUOTE
  deriving (Eq, Ord, Read, Show)

data Fielddecl_seq =
    Fielddecl_seq_implies_fielddecl Fielddecl
  | Fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq Fielddecl COMMA Fielddecl_seq
  deriving (Eq, Ord, Read, Show)

data Fielddecl =
    Fielddecl_implies_vars_COLON_COLON_type' Vars COLON_COLON Type'
  deriving (Eq, Ord, Read, Show)

data Callconv =
    Callconv_implies_AS AS
  | Callconv_implies_EXPORT EXPORT
  | Callconv_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Impent =
    Impent_implies_STRING STRING
  deriving (Eq, Ord, Read, Show)

data Safety =
    Safety_implies_AS AS
  | Safety_implies_EXPORT EXPORT
  | Safety_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Expent =
    Expent_implies_STRING STRING
  deriving (Eq, Ord, Read, Show)

data Guards =
    Guards_implies_guard Guard
  | Guards_implies_guard_COMMA_guards Guard COMMA Guards
  deriving (Eq, Ord, Read, Show)

data Guard =
    Guard_implies_infixexp_LARROW_exp Infixexp LARROW Exp
  | Guard_implies_LET_decls LET Decls
  | Guard_implies_infixexp Infixexp
  deriving (Eq, Ord, Read, Show)

data Infixexp =
    Infixexp_implies_LAMBDA_pat_RARROW_exp LAMBDA Pat RARROW Exp
  | Infixexp_implies_LET_decls_IN_exp LET Decls IN Exp
  | Infixexp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp IF Exp Semicolon_opt THEN Exp Semicolon_opt ELSE Exp
  | Infixexp_implies_lexp_MINUS_exp Lexp MINUS Exp
  | Infixexp_implies_lexp_QVARSYM_exp Lexp QVARSYM Exp
  | Infixexp_implies_lexp_BACKQUOTE_AS_BACKQUOTE_exp Lexp BACKQUOTE AS BACKQUOTE Exp
  | Infixexp_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_exp Lexp BACKQUOTE EXPORT BACKQUOTE Exp
  | Infixexp_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_exp Lexp BACKQUOTE QVARID BACKQUOTE Exp
  | Infixexp_implies_lexp_QCONSYM_exp Lexp QCONSYM Exp
  | Infixexp_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_exp Lexp BACKQUOTE QCONID BACKQUOTE Exp
  | Infixexp_implies_lexp_COLON_COLON_type' Lexp COLON_COLON Type'
  | Infixexp_implies_lexp_COLON_COLON_btype_DARROW_type' Lexp COLON_COLON Btype DARROW Type'
  | Infixexp_implies_lexp Lexp
  deriving (Eq, Ord, Read, Show)

data Semicolon_opt =
    Semicolon_opt_implies
  | Semicolon_opt_implies_SEMICOLON SEMICOLON
  deriving (Eq, Ord, Read, Show)

data Lexp =
    Lexp_implies_MINUS_lexp MINUS Lexp
  | Lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE CASE Exp OF LBRACE Alts RBRACE
  | Lexp_implies_DO_LBRACE_stmts_RBRACE DO LBRACE Stmts RBRACE
  | Lexp_implies_fexp Fexp
  deriving (Eq, Ord, Read, Show)

data Infixexp' =
    Infixexp'_implies_LAMBDA_pat_RARROW_infixexp' LAMBDA Pat RARROW Infixexp'
  | Infixexp'_implies_LET_decls_IN_infixexp' LET Decls IN Infixexp'
  | Infixexp'_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_infixexp' IF Exp Semicolon_opt THEN Exp Semicolon_opt ELSE Infixexp'
  | Infixexp'_implies_lexp_MINUS_infixexp' Lexp MINUS Infixexp'
  | Infixexp'_implies_lexp_QVARSYM_infixexp' Lexp QVARSYM Infixexp'
  | Infixexp'_implies_lexp_BACKQUOTE_AS_BACKQUOTE_infixexp' Lexp BACKQUOTE AS BACKQUOTE Infixexp'
  | Infixexp'_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_infixexp' Lexp BACKQUOTE EXPORT BACKQUOTE Infixexp'
  | Infixexp'_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_infixexp' Lexp BACKQUOTE QVARID BACKQUOTE Infixexp'
  | Infixexp'_implies_lexp_QCONSYM_infixexp' Lexp QCONSYM Infixexp'
  | Infixexp'_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_infixexp' Lexp BACKQUOTE QCONID BACKQUOTE Infixexp'
  | Infixexp'_implies_lexp Lexp
  deriving (Eq, Ord, Read, Show)

data Alts =
    Alts_implies_alt Alt
  | Alts_implies_alt_SEMICOLON_alts Alt SEMICOLON Alts
  deriving (Eq, Ord, Read, Show)

data Stmts =
    Stmts_implies_stmt Stmt
  | Stmts_implies_stmt_SEMICOLON_stmts Stmt SEMICOLON Stmts
  deriving (Eq, Ord, Read, Show)

data Fexp =
    Fexp_implies_aexp Aexp
  | Fexp_implies_fexp_aexp Fexp Aexp
  deriving (Eq, Ord, Read, Show)

data Aexp =
    Aexp_implies_var Var
  | Aexp_implies_INTEGER INTEGER
  | Aexp_implies_STRING STRING
  | Aexp_implies_LPAREN_exp_RPAREN LPAREN Exp RPAREN
  | Aexp_implies_LPAREN_exp_seq2_RPAREN LPAREN Exp_seq2 RPAREN
  | Aexp_implies_LBRACKET_exp_seq_RBRACKET LBRACKET Exp_seq RBRACKET
  | Aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET LBRACKET Exp DOT_DOT RBRACKET
  | Aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET LBRACKET Exp DOT_DOT Exp RBRACKET
  | Aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET LBRACKET Exp COMMA Exp DOT_DOT RBRACKET
  | Aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET LBRACKET Exp COMMA Exp DOT_DOT Exp RBRACKET
  | Aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN LPAREN QVARSYM Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE AS BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE EXPORT BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE QVARID BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN LPAREN QCONSYM Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE QCONID BACKQUOTE Infixexp RPAREN
  deriving (Eq, Ord, Read, Show)

data Exp_seq =
    Exp_seq_implies_exp Exp
  | Exp_seq_implies_exp_COMMA_exp_seq Exp COMMA Exp_seq
  deriving (Eq, Ord, Read, Show)

data Exp_seq2 =
    Exp_seq2_implies_exp_COMMA_exp Exp COMMA Exp
  | Exp_seq2_implies_exp_COMMA_exp_seq2 Exp COMMA Exp_seq2
  deriving (Eq, Ord, Read, Show)

data Alt =
    Alt_implies
  | Alt_implies_pat_RARROW_exp Pat RARROW Exp
  | Alt_implies_pat_RARROW_exp_WHERE_decls Pat RARROW Exp WHERE Decls
  | Alt_implies_pat_PIPE_gdpat Pat PIPE Gdpat
  | Alt_implies_pat_PIPE_gdpat_WHERE_decls Pat PIPE Gdpat WHERE Decls
  deriving (Eq, Ord, Read, Show)

data Gdpat =
    Gdpat_implies_patguards_RARROW_exp Patguards RARROW Exp
  | Gdpat_implies_patguards_RARROW_exp_PIPE_gdpat Patguards RARROW Exp PIPE Gdpat
  deriving (Eq, Ord, Read, Show)

data Patguards =
    Patguards_implies_patguard Patguard
  | Patguards_implies_patguard_COMMA_patguards Patguard COMMA Patguards
  deriving (Eq, Ord, Read, Show)

data Patguard =
    Patguard_implies_infixexp'_LARROW_infixexp' Infixexp' LARROW Infixexp'
  | Patguard_implies_LET_decls LET Decls
  | Patguard_implies_infixexp' Infixexp'
  deriving (Eq, Ord, Read, Show)

data Stmt =
    Stmt_implies
  | Stmt_implies_infixexp Infixexp
  | Stmt_implies_infixexp_LARROW_infixexp Infixexp LARROW Infixexp
  | Stmt_implies_LET_decls LET Decls
  deriving (Eq, Ord, Read, Show)

data Apat =
    Apat_implies_var Var
  | Apat_implies_LPAREN_pat_RPAREN LPAREN Pat RPAREN
  deriving (Eq, Ord, Read, Show)

data Varop =
    Varop_implies_QVARSYM QVARSYM
  | Varop_implies_BACKQUOTE_AS_BACKQUOTE BACKQUOTE AS BACKQUOTE
  | Varop_implies_BACKQUOTE_EXPORT_BACKQUOTE BACKQUOTE EXPORT BACKQUOTE
  | Varop_implies_BACKQUOTE_QVARID_BACKQUOTE BACKQUOTE QVARID BACKQUOTE
  deriving (Eq, Ord, Read, Show)

data Tycls =
    Tycls_implies_QCONID QCONID
  deriving (Eq, Ord, Read, Show)

data StackValue =
    StackValue_EOF
  | StackValue_MODULE MODULE
  | StackValue_WHERE WHERE
  | StackValue_LBRACE LBRACE
  | StackValue_RBRACE RBRACE
  | StackValue_LPAREN LPAREN
  | StackValue_RPAREN RPAREN
  | StackValue_COMMA COMMA
  | StackValue_DOT_DOT DOT_DOT
  | StackValue_SEMICOLON SEMICOLON
  | StackValue_IMPORT IMPORT
  | StackValue_HIDING HIDING
  | StackValue_TYPE TYPE
  | StackValue_EQUAL EQUAL
  | StackValue_DATA DATA
  | StackValue_DERIVING DERIVING
  | StackValue_DARROW DARROW
  | StackValue_NEWTYPE NEWTYPE
  | StackValue_CLASS CLASS
  | StackValue_INSTANCE INSTANCE
  | StackValue_DEFAULT DEFAULT
  | StackValue_FOREIGN FOREIGN
  | StackValue_PIPE PIPE
  | StackValue_COLON_COLON COLON_COLON
  | StackValue_MINUS MINUS
  | StackValue_INFIXL INFIXL
  | StackValue_INFIXR INFIXR
  | StackValue_INFIX INFIX
  | StackValue_RARROW RARROW
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_EXCL EXCL
  | StackValue_QCONID QCONID
  | StackValue_EXPORT EXPORT
  | StackValue_AS AS
  | StackValue_QVARID QVARID
  | StackValue_STRING STRING
  | StackValue_LARROW LARROW
  | StackValue_LET LET
  | StackValue_LAMBDA LAMBDA
  | StackValue_IN IN
  | StackValue_IF IF
  | StackValue_THEN THEN
  | StackValue_ELSE ELSE
  | StackValue_QVARSYM QVARSYM
  | StackValue_BACKQUOTE BACKQUOTE
  | StackValue_QCONSYM QCONSYM
  | StackValue_CASE CASE
  | StackValue_OF OF
  | StackValue_DO DO
  | StackValue_INTEGER INTEGER
  | StackValue_QUALIFIED QUALIFIED
  | StackValue_module' Module'
  | StackValue_modid Modid
  | StackValue_exports_opt Exports_opt
  | StackValue_body Body
  | StackValue_topdecls Topdecls
  | StackValue_exports Exports
  | StackValue_export_seq Export_seq
  | StackValue_export Export
  | StackValue_var Var
  | StackValue_con Con
  | StackValue_cname_seq Cname_seq
  | StackValue_import_seq Import_seq
  | StackValue_import' Import'
  | StackValue_cname Cname
  | StackValue_topdecl Topdecl
  | StackValue_qualified_opt Qualified_opt
  | StackValue_as_opt As_opt
  | StackValue_btype Btype
  | StackValue_type' Type'
  | StackValue_constrs_opt Constrs_opt
  | StackValue_dclass Dclass
  | StackValue_dclass_seq Dclass_seq
  | StackValue_newconstr Newconstr
  | StackValue_cdecls_opt Cdecls_opt
  | StackValue_idecls_opt Idecls_opt
  | StackValue_type_seq Type_seq
  | StackValue_fdecl Fdecl
  | StackValue_decl Decl
  | StackValue_decls Decls
  | StackValue_decl_seq Decl_seq
  | StackValue_gendecl Gendecl
  | StackValue_pat Pat
  | StackValue_exp Exp
  | StackValue_gdrhs Gdrhs
  | StackValue_cdecls Cdecls
  | StackValue_cdecl_seq Cdecl_seq
  | StackValue_cdecl Cdecl
  | StackValue_idecls Idecls
  | StackValue_idecl_seq Idecl_seq
  | StackValue_idecl Idecl
  | StackValue_vars Vars
  | StackValue_fixity Fixity
  | StackValue_integer_opt Integer_opt
  | StackValue_ops Ops
  | StackValue_op Op
  | StackValue_atype Atype
  | StackValue_gtycon Gtycon
  | StackValue_tyvar Tyvar
  | StackValue_type_seq2 Type_seq2
  | StackValue_comma_list Comma_list
  | StackValue_constrs Constrs
  | StackValue_constr Constr
  | StackValue_conop Conop
  | StackValue_fielddecl_seq Fielddecl_seq
  | StackValue_fielddecl Fielddecl
  | StackValue_callconv Callconv
  | StackValue_impent Impent
  | StackValue_safety Safety
  | StackValue_expent Expent
  | StackValue_guards Guards
  | StackValue_guard Guard
  | StackValue_infixexp Infixexp
  | StackValue_semicolon_opt Semicolon_opt
  | StackValue_lexp Lexp
  | StackValue_infixexp' Infixexp'
  | StackValue_alts Alts
  | StackValue_stmts Stmts
  | StackValue_fexp Fexp
  | StackValue_aexp Aexp
  | StackValue_exp_seq Exp_seq
  | StackValue_exp_seq2 Exp_seq2
  | StackValue_alt Alt
  | StackValue_gdpat Gdpat
  | StackValue_patguards Patguards
  | StackValue_patguard Patguard
  | StackValue_stmt Stmt
  | StackValue_apat Apat
  | StackValue_varop Varop
  | StackValue_tycls Tycls

data SemanticActions m = SemanticActions
  { module'_implies_MODULE_modid_exports_opt_WHERE_body :: MODULE -> Modid -> Exports_opt -> WHERE -> Body -> m Module'
  , module'_implies_body :: Body -> m Module'
  , body_implies_LBRACE_topdecls_RBRACE :: LBRACE -> Topdecls -> RBRACE -> m Body
  , exports_opt_implies :: m Exports_opt
  , exports_opt_implies_exports :: Exports -> m Exports_opt
  , exports_implies_LPAREN_export_seq_RPAREN :: LPAREN -> Export_seq -> RPAREN -> m Exports
  , export_seq_implies :: m Export_seq
  , export_seq_implies_export :: Export -> m Export_seq
  , export_seq_implies_export_COMMA_export_seq :: Export -> COMMA -> Export_seq -> m Export_seq
  , export_implies_var :: Var -> m Export
  , export_implies_con :: Con -> m Export
  , export_implies_con_LPAREN_RPAREN :: Con -> LPAREN -> RPAREN -> m Export
  , export_implies_con_LPAREN_DOT_DOT_RPAREN :: Con -> LPAREN -> DOT_DOT -> RPAREN -> m Export
  , export_implies_con_LPAREN_cname_seq_RPAREN :: Con -> LPAREN -> Cname_seq -> RPAREN -> m Export
  , export_implies_MODULE_modid :: MODULE -> Modid -> m Export
  , import_seq_implies :: m Import_seq
  , import_seq_implies_import' :: Import' -> m Import_seq
  , import_seq_implies_import'_COMMA_import_seq :: Import' -> COMMA -> Import_seq -> m Import_seq
  , import'_implies_var :: Var -> m Import'
  , import'_implies_con :: Con -> m Import'
  , import'_implies_con_LPAREN_RPAREN :: Con -> LPAREN -> RPAREN -> m Import'
  , import'_implies_con_LPAREN_DOT_DOT_RPAREN :: Con -> LPAREN -> DOT_DOT -> RPAREN -> m Import'
  , import'_implies_con_LPAREN_cname_seq_RPAREN :: Con -> LPAREN -> Cname_seq -> RPAREN -> m Import'
  , cname_seq_implies_cname :: Cname -> m Cname_seq
  , cname_seq_implies_cname_COMMA_cname_seq :: Cname -> COMMA -> Cname_seq -> m Cname_seq
  , cname_implies_var :: Var -> m Cname
  , cname_implies_con :: Con -> m Cname
  , topdecls_implies_topdecl :: Topdecl -> m Topdecls
  , topdecls_implies_topdecl_SEMICOLON_topdecls :: Topdecl -> SEMICOLON -> Topdecls -> m Topdecls
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt :: IMPORT -> Qualified_opt -> Modid -> As_opt -> m Topdecl
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_LPAREN_import_seq_RPAREN :: IMPORT -> Qualified_opt -> Modid -> As_opt -> LPAREN -> Import_seq -> RPAREN -> m Topdecl
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_HIDING_LPAREN_import_seq_RPAREN :: IMPORT -> Qualified_opt -> Modid -> As_opt -> HIDING -> LPAREN -> Import_seq -> RPAREN -> m Topdecl
  , topdecl_implies_TYPE_btype_EQUAL_type' :: TYPE -> Btype -> EQUAL -> Type' -> m Topdecl
  , topdecl_implies_DATA_btype_constrs_opt :: DATA -> Btype -> Constrs_opt -> m Topdecl
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass :: DATA -> Btype -> Constrs_opt -> DERIVING -> Dclass -> m Topdecl
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN :: DATA -> Btype -> Constrs_opt -> DERIVING -> LPAREN -> RPAREN -> m Topdecl
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN :: DATA -> Btype -> Constrs_opt -> DERIVING -> LPAREN -> Dclass_seq -> RPAREN -> m Topdecl
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt :: DATA -> Btype -> DARROW -> Btype -> Constrs_opt -> m Topdecl
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass :: DATA -> Btype -> DARROW -> Btype -> Constrs_opt -> DERIVING -> Dclass -> m Topdecl
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN :: DATA -> Btype -> DARROW -> Btype -> Constrs_opt -> DERIVING -> LPAREN -> RPAREN -> m Topdecl
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN :: DATA -> Btype -> DARROW -> Btype -> Constrs_opt -> DERIVING -> LPAREN -> Dclass_seq -> RPAREN -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_newconstr :: NEWTYPE -> Btype -> Newconstr -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass :: NEWTYPE -> Btype -> Newconstr -> DERIVING -> Dclass -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN :: NEWTYPE -> Btype -> Newconstr -> DERIVING -> LPAREN -> RPAREN -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN :: NEWTYPE -> Btype -> Newconstr -> DERIVING -> LPAREN -> Dclass_seq -> RPAREN -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr :: NEWTYPE -> Btype -> DARROW -> Btype -> Newconstr -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass :: NEWTYPE -> Btype -> DARROW -> Btype -> Newconstr -> DERIVING -> Dclass -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN :: NEWTYPE -> Btype -> DARROW -> Btype -> Newconstr -> DERIVING -> LPAREN -> RPAREN -> m Topdecl
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN :: NEWTYPE -> Btype -> DARROW -> Btype -> Newconstr -> DERIVING -> LPAREN -> Dclass_seq -> RPAREN -> m Topdecl
  , topdecl_implies_CLASS_btype_cdecls_opt :: CLASS -> Btype -> Cdecls_opt -> m Topdecl
  , topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt :: CLASS -> Btype -> DARROW -> Btype -> Cdecls_opt -> m Topdecl
  , topdecl_implies_INSTANCE_btype_idecls_opt :: INSTANCE -> Btype -> Idecls_opt -> m Topdecl
  , topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt :: INSTANCE -> Btype -> DARROW -> Btype -> Idecls_opt -> m Topdecl
  , topdecl_implies_DEFAULT_LPAREN_RPAREN :: DEFAULT -> LPAREN -> RPAREN -> m Topdecl
  , topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN :: DEFAULT -> LPAREN -> Type_seq -> RPAREN -> m Topdecl
  , topdecl_implies_FOREIGN_fdecl :: FOREIGN -> Fdecl -> m Topdecl
  , topdecl_implies_decl :: Decl -> m Topdecl
  , decls_implies_LBRACE_decl_seq_RBRACE :: LBRACE -> Decl_seq -> RBRACE -> m Decls
  , decl_seq_implies_decl :: Decl -> m Decl_seq
  , decl_seq_implies_decl_SEMICOLON_decl_seq :: Decl -> SEMICOLON -> Decl_seq -> m Decl_seq
  , decl_implies_gendecl :: Gendecl -> m Decl
  , decl_implies_pat_EQUAL_exp :: Pat -> EQUAL -> Exp -> m Decl
  , decl_implies_pat_EQUAL_exp_WHERE_decls :: Pat -> EQUAL -> Exp -> WHERE -> Decls -> m Decl
  , decl_implies_pat_PIPE_gdrhs :: Pat -> PIPE -> Gdrhs -> m Decl
  , decl_implies_pat_PIPE_gdrhs_WHERE_decls :: Pat -> PIPE -> Gdrhs -> WHERE -> Decls -> m Decl
  , cdecls_opt_implies :: m Cdecls_opt
  , cdecls_opt_implies_WHERE_cdecls :: WHERE -> Cdecls -> m Cdecls_opt
  , cdecls_implies_LBRACE_cdecl_seq_RBRACE :: LBRACE -> Cdecl_seq -> RBRACE -> m Cdecls
  , cdecl_seq_implies_cdecl :: Cdecl -> m Cdecl_seq
  , cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq :: Cdecl -> SEMICOLON -> Cdecl_seq -> m Cdecl_seq
  , cdecl_implies_gendecl :: Gendecl -> m Cdecl
  , cdecl_implies_pat_EQUAL_exp :: Pat -> EQUAL -> Exp -> m Cdecl
  , cdecl_implies_pat_EQUAL_exp_WHERE_decls :: Pat -> EQUAL -> Exp -> WHERE -> Decls -> m Cdecl
  , cdecl_implies_pat_PIPE_gdrhs :: Pat -> PIPE -> Gdrhs -> m Cdecl
  , cdecl_implies_pat_PIPE_gdrhs_WHERE_decls :: Pat -> PIPE -> Gdrhs -> WHERE -> Decls -> m Cdecl
  , idecls_opt_implies :: m Idecls_opt
  , idecls_opt_implies_WHERE_idecls :: WHERE -> Idecls -> m Idecls_opt
  , idecls_implies_LBRACE_idecl_seq_RBRACE :: LBRACE -> Idecl_seq -> RBRACE -> m Idecls
  , idecl_seq_implies_idecl :: Idecl -> m Idecl_seq
  , idecl_seq_implies_idecl_SEMICOLON_idecl_seq :: Idecl -> SEMICOLON -> Idecl_seq -> m Idecl_seq
  , idecl_implies :: m Idecl
  , idecl_implies_pat_EQUAL_exp :: Pat -> EQUAL -> Exp -> m Idecl
  , idecl_implies_pat_EQUAL_exp_WHERE_decls :: Pat -> EQUAL -> Exp -> WHERE -> Decls -> m Idecl
  , idecl_implies_pat_PIPE_gdrhs :: Pat -> PIPE -> Gdrhs -> m Idecl
  , idecl_implies_pat_PIPE_gdrhs_WHERE_decls :: Pat -> PIPE -> Gdrhs -> WHERE -> Decls -> m Idecl
  , gendecl_implies :: m Gendecl
  , gendecl_implies_vars_COLON_COLON_type' :: Vars -> COLON_COLON -> Type' -> m Gendecl
  , gendecl_implies_vars_COLON_COLON_btype_DARROW_type' :: Vars -> COLON_COLON -> Btype -> DARROW -> Type' -> m Gendecl
  , gendecl_implies_fixity_integer_opt_ops :: Fixity -> Integer_opt -> Ops -> m Gendecl
  , ops_implies_MINUS :: MINUS -> m Ops
  , ops_implies_op :: Op -> m Ops
  , ops_implies_MINUS_COMMA_ops :: MINUS -> COMMA -> Ops -> m Ops
  , ops_implies_op_COMMA_ops :: Op -> COMMA -> Ops -> m Ops
  , vars_implies_var :: Var -> m Vars
  , vars_implies_var_COMMA_vars :: Var -> COMMA -> Vars -> m Vars
  , fixity_implies_INFIXL :: INFIXL -> m Fixity
  , fixity_implies_INFIXR :: INFIXR -> m Fixity
  , fixity_implies_INFIX :: INFIX -> m Fixity
  , type_seq_implies_type' :: Type' -> m Type_seq
  , type_seq_implies_type'_COMMA_type_seq :: Type' -> COMMA -> Type_seq -> m Type_seq
  , type'_implies_btype :: Btype -> m Type'
  , type'_implies_btype_RARROW_type' :: Btype -> RARROW -> Type' -> m Type'
  , btype_implies_atype :: Atype -> m Btype
  , btype_implies_btype_atype :: Btype -> Atype -> m Btype
  , atype_implies_gtycon :: Gtycon -> m Atype
  , atype_implies_tyvar :: Tyvar -> m Atype
  , atype_implies_LPAREN_type_seq2_RPAREN :: LPAREN -> Type_seq2 -> RPAREN -> m Atype
  , atype_implies_LBRACKET_type'_RBRACKET :: LBRACKET -> Type' -> RBRACKET -> m Atype
  , atype_implies_LPAREN_type'_RPAREN :: LPAREN -> Type' -> RPAREN -> m Atype
  , atype_implies_EXCL_atype :: EXCL -> Atype -> m Atype
  , type_seq2_implies_type'_COMMA_type' :: Type' -> COMMA -> Type' -> m Type_seq2
  , type_seq2_implies_type'_COMMA_type_seq2 :: Type' -> COMMA -> Type_seq2 -> m Type_seq2
  , gtycon_implies_con :: Con -> m Gtycon
  , gtycon_implies_LPAREN_RPAREN :: LPAREN -> RPAREN -> m Gtycon
  , gtycon_implies_LBRACKET_RBRACKET :: LBRACKET -> RBRACKET -> m Gtycon
  , gtycon_implies_LPAREN_RARROW_RPAREN :: LPAREN -> RARROW -> RPAREN -> m Gtycon
  , gtycon_implies_LPAREN_comma_list_RPAREN :: LPAREN -> Comma_list -> RPAREN -> m Gtycon
  , comma_list_implies_COMMA :: COMMA -> m Comma_list
  , comma_list_implies_COMMA_comma_list :: COMMA -> Comma_list -> m Comma_list
  , constrs_opt_implies :: m Constrs_opt
  , constrs_opt_implies_EQUAL_constrs :: EQUAL -> Constrs -> m Constrs_opt
  , constrs_implies_constr :: Constr -> m Constrs
  , constrs_implies_constr_PIPE_constrs :: Constr -> PIPE -> Constrs -> m Constrs
  , constr_implies_btype :: Btype -> m Constr
  , constr_implies_btype_conop_btype :: Btype -> Conop -> Btype -> m Constr
  , constr_implies_con_LBRACE_RBRACE :: Con -> LBRACE -> RBRACE -> m Constr
  , constr_implies_con_LBRACE_fielddecl_seq_RBRACE :: Con -> LBRACE -> Fielddecl_seq -> RBRACE -> m Constr
  , newconstr_implies_EQUAL_con_atype :: EQUAL -> Con -> Atype -> m Newconstr
  , newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE :: EQUAL -> Con -> LBRACE -> Var -> COLON_COLON -> Type' -> RBRACE -> m Newconstr
  , fielddecl_seq_implies_fielddecl :: Fielddecl -> m Fielddecl_seq
  , fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq :: Fielddecl -> COMMA -> Fielddecl_seq -> m Fielddecl_seq
  , fielddecl_implies_vars_COLON_COLON_type' :: Vars -> COLON_COLON -> Type' -> m Fielddecl
  , dclass_seq_implies_dclass :: Dclass -> m Dclass_seq
  , dclass_seq_implies_dclass_COMMA_dclass_seq :: Dclass -> COMMA -> Dclass_seq -> m Dclass_seq
  , dclass_implies_QCONID :: QCONID -> m Dclass
  , fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' :: IMPORT -> Callconv -> Impent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' :: IMPORT -> Callconv -> Safety -> Impent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' :: EXPORT -> Callconv -> Expent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , callconv_implies_AS :: AS -> m Callconv
  , callconv_implies_EXPORT :: EXPORT -> m Callconv
  , callconv_implies_QVARID :: QVARID -> m Callconv
  , impent_implies_STRING :: STRING -> m Impent
  , expent_implies_STRING :: STRING -> m Expent
  , safety_implies_AS :: AS -> m Safety
  , safety_implies_EXPORT :: EXPORT -> m Safety
  , safety_implies_QVARID :: QVARID -> m Safety
  , gdrhs_implies_guards_EQUAL_exp :: Guards -> EQUAL -> Exp -> m Gdrhs
  , gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs :: Guards -> EQUAL -> Exp -> PIPE -> Gdrhs -> m Gdrhs
  , guards_implies_guard :: Guard -> m Guards
  , guards_implies_guard_COMMA_guards :: Guard -> COMMA -> Guards -> m Guards
  , guard_implies_infixexp_LARROW_exp :: Infixexp -> LARROW -> Exp -> m Guard
  , guard_implies_LET_decls :: LET -> Decls -> m Guard
  , guard_implies_infixexp :: Infixexp -> m Guard
  , exp_implies_infixexp :: Infixexp -> m Exp
  , infixexp_implies_LAMBDA_pat_RARROW_exp :: LAMBDA -> Pat -> RARROW -> Exp -> m Infixexp
  , infixexp_implies_LET_decls_IN_exp :: LET -> Decls -> IN -> Exp -> m Infixexp
  , infixexp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp :: IF -> Exp -> Semicolon_opt -> THEN -> Exp -> Semicolon_opt -> ELSE -> Exp -> m Infixexp
  , infixexp_implies_lexp_MINUS_exp :: Lexp -> MINUS -> Exp -> m Infixexp
  , infixexp_implies_lexp_QVARSYM_exp :: Lexp -> QVARSYM -> Exp -> m Infixexp
  , infixexp_implies_lexp_BACKQUOTE_AS_BACKQUOTE_exp :: Lexp -> BACKQUOTE -> AS -> BACKQUOTE -> Exp -> m Infixexp
  , infixexp_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_exp :: Lexp -> BACKQUOTE -> EXPORT -> BACKQUOTE -> Exp -> m Infixexp
  , infixexp_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_exp :: Lexp -> BACKQUOTE -> QVARID -> BACKQUOTE -> Exp -> m Infixexp
  , infixexp_implies_lexp_QCONSYM_exp :: Lexp -> QCONSYM -> Exp -> m Infixexp
  , infixexp_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_exp :: Lexp -> BACKQUOTE -> QCONID -> BACKQUOTE -> Exp -> m Infixexp
  , infixexp_implies_lexp_COLON_COLON_type' :: Lexp -> COLON_COLON -> Type' -> m Infixexp
  , infixexp_implies_lexp_COLON_COLON_btype_DARROW_type' :: Lexp -> COLON_COLON -> Btype -> DARROW -> Type' -> m Infixexp
  , infixexp_implies_lexp :: Lexp -> m Infixexp
  , infixexp'_implies_LAMBDA_pat_RARROW_infixexp' :: LAMBDA -> Pat -> RARROW -> Infixexp' -> m Infixexp'
  , infixexp'_implies_LET_decls_IN_infixexp' :: LET -> Decls -> IN -> Infixexp' -> m Infixexp'
  , infixexp'_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_infixexp' :: IF -> Exp -> Semicolon_opt -> THEN -> Exp -> Semicolon_opt -> ELSE -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_MINUS_infixexp' :: Lexp -> MINUS -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_QVARSYM_infixexp' :: Lexp -> QVARSYM -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_BACKQUOTE_AS_BACKQUOTE_infixexp' :: Lexp -> BACKQUOTE -> AS -> BACKQUOTE -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_infixexp' :: Lexp -> BACKQUOTE -> EXPORT -> BACKQUOTE -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_infixexp' :: Lexp -> BACKQUOTE -> QVARID -> BACKQUOTE -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_QCONSYM_infixexp' :: Lexp -> QCONSYM -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_infixexp' :: Lexp -> BACKQUOTE -> QCONID -> BACKQUOTE -> Infixexp' -> m Infixexp'
  , infixexp'_implies_lexp :: Lexp -> m Infixexp'
  , lexp_implies_MINUS_lexp :: MINUS -> Lexp -> m Lexp
  , lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE :: CASE -> Exp -> OF -> LBRACE -> Alts -> RBRACE -> m Lexp
  , lexp_implies_DO_LBRACE_stmts_RBRACE :: DO -> LBRACE -> Stmts -> RBRACE -> m Lexp
  , lexp_implies_fexp :: Fexp -> m Lexp
  , fexp_implies_aexp :: Aexp -> m Fexp
  , fexp_implies_fexp_aexp :: Fexp -> Aexp -> m Fexp
  , exp_seq_implies_exp :: Exp -> m Exp_seq
  , exp_seq_implies_exp_COMMA_exp_seq :: Exp -> COMMA -> Exp_seq -> m Exp_seq
  , exp_seq2_implies_exp_COMMA_exp :: Exp -> COMMA -> Exp -> m Exp_seq2
  , exp_seq2_implies_exp_COMMA_exp_seq2 :: Exp -> COMMA -> Exp_seq2 -> m Exp_seq2
  , aexp_implies_var :: Var -> m Aexp
  , aexp_implies_INTEGER :: INTEGER -> m Aexp
  , aexp_implies_STRING :: STRING -> m Aexp
  , aexp_implies_LPAREN_exp_RPAREN :: LPAREN -> Exp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_exp_seq2_RPAREN :: LPAREN -> Exp_seq2 -> RPAREN -> m Aexp
  , aexp_implies_LBRACKET_exp_seq_RBRACKET :: LBRACKET -> Exp_seq -> RBRACKET -> m Aexp
  , aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET :: LBRACKET -> Exp -> DOT_DOT -> RBRACKET -> m Aexp
  , aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET :: LBRACKET -> Exp -> DOT_DOT -> Exp -> RBRACKET -> m Aexp
  , aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET :: LBRACKET -> Exp -> COMMA -> Exp -> DOT_DOT -> RBRACKET -> m Aexp
  , aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET :: LBRACKET -> Exp -> COMMA -> Exp -> DOT_DOT -> Exp -> RBRACKET -> m Aexp
  , aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN :: LPAREN -> QVARSYM -> Infixexp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN :: LPAREN -> BACKQUOTE -> AS -> BACKQUOTE -> Infixexp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN :: LPAREN -> BACKQUOTE -> EXPORT -> BACKQUOTE -> Infixexp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN :: LPAREN -> BACKQUOTE -> QVARID -> BACKQUOTE -> Infixexp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN :: LPAREN -> QCONSYM -> Infixexp -> RPAREN -> m Aexp
  , aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN :: LPAREN -> BACKQUOTE -> QCONID -> BACKQUOTE -> Infixexp -> RPAREN -> m Aexp
  , alts_implies_alt :: Alt -> m Alts
  , alts_implies_alt_SEMICOLON_alts :: Alt -> SEMICOLON -> Alts -> m Alts
  , alt_implies :: m Alt
  , alt_implies_pat_RARROW_exp :: Pat -> RARROW -> Exp -> m Alt
  , alt_implies_pat_RARROW_exp_WHERE_decls :: Pat -> RARROW -> Exp -> WHERE -> Decls -> m Alt
  , alt_implies_pat_PIPE_gdpat :: Pat -> PIPE -> Gdpat -> m Alt
  , alt_implies_pat_PIPE_gdpat_WHERE_decls :: Pat -> PIPE -> Gdpat -> WHERE -> Decls -> m Alt
  , gdpat_implies_patguards_RARROW_exp :: Patguards -> RARROW -> Exp -> m Gdpat
  , gdpat_implies_patguards_RARROW_exp_PIPE_gdpat :: Patguards -> RARROW -> Exp -> PIPE -> Gdpat -> m Gdpat
  , patguards_implies_patguard :: Patguard -> m Patguards
  , patguards_implies_patguard_COMMA_patguards :: Patguard -> COMMA -> Patguards -> m Patguards
  , patguard_implies_infixexp'_LARROW_infixexp' :: Infixexp' -> LARROW -> Infixexp' -> m Patguard
  , patguard_implies_LET_decls :: LET -> Decls -> m Patguard
  , patguard_implies_infixexp' :: Infixexp' -> m Patguard
  , stmts_implies_stmt :: Stmt -> m Stmts
  , stmts_implies_stmt_SEMICOLON_stmts :: Stmt -> SEMICOLON -> Stmts -> m Stmts
  , stmt_implies :: m Stmt
  , stmt_implies_infixexp :: Infixexp -> m Stmt
  , stmt_implies_infixexp_LARROW_infixexp :: Infixexp -> LARROW -> Infixexp -> m Stmt
  , stmt_implies_LET_decls :: LET -> Decls -> m Stmt
  , pat_implies_apat :: Apat -> m Pat
  , pat_implies_pat_apat :: Pat -> Apat -> m Pat
  , pat_implies_pat_MINUS_apat :: Pat -> MINUS -> Apat -> m Pat
  , pat_implies_pat_op_apat :: Pat -> Op -> Apat -> m Pat
  , apat_implies_var :: Var -> m Apat
  , apat_implies_LPAREN_pat_RPAREN :: LPAREN -> Pat -> RPAREN -> m Apat
  , var_implies_AS :: AS -> m Var
  , var_implies_EXPORT :: EXPORT -> m Var
  , var_implies_QVARID :: QVARID -> m Var
  , var_implies_LPAREN_MINUS_RPAREN :: LPAREN -> MINUS -> RPAREN -> m Var
  , var_implies_LPAREN_QVARSYM_RPAREN :: LPAREN -> QVARSYM -> RPAREN -> m Var
  , con_implies_QCONID :: QCONID -> m Con
  , con_implies_LPAREN_QCONSYM_RPAREN :: LPAREN -> QCONSYM -> RPAREN -> m Con
  , varop_implies_QVARSYM :: QVARSYM -> m Varop
  , varop_implies_BACKQUOTE_AS_BACKQUOTE :: BACKQUOTE -> AS -> BACKQUOTE -> m Varop
  , varop_implies_BACKQUOTE_EXPORT_BACKQUOTE :: BACKQUOTE -> EXPORT -> BACKQUOTE -> m Varop
  , varop_implies_BACKQUOTE_QVARID_BACKQUOTE :: BACKQUOTE -> QVARID -> BACKQUOTE -> m Varop
  , conop_implies_QCONSYM :: QCONSYM -> m Conop
  , conop_implies_BACKQUOTE_QCONID_BACKQUOTE :: BACKQUOTE -> QCONID -> BACKQUOTE -> m Conop
  , op_implies_varop :: Varop -> m Op
  , op_implies_conop :: Conop -> m Op
  , as_opt_implies :: m As_opt
  , as_opt_implies_AS_modid :: AS -> Modid -> m As_opt
  , qualified_opt_implies :: m Qualified_opt
  , qualified_opt_implies_QUALIFIED :: QUALIFIED -> m Qualified_opt
  , tyvar_implies_AS :: AS -> m Tyvar
  , tyvar_implies_EXPORT :: EXPORT -> m Tyvar
  , tyvar_implies_QVARID :: QVARID -> m Tyvar
  , tycls_implies_QCONID :: QCONID -> m Tycls
  , modid_implies_QCONID :: QCONID -> m Modid
  , integer_opt_implies :: m Integer_opt
  , integer_opt_implies_INTEGER :: INTEGER -> m Integer_opt
  , semicolon_opt_implies :: m Semicolon_opt
  , semicolon_opt_implies_SEMICOLON :: SEMICOLON -> m Semicolon_opt }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (MODULE _)) -> Just (Shift 2)
    (0, Token (LBRACE _)) -> Just (Shift 13)
    (1, EOF) -> Just (Accept)
    (2, Token (QCONID _)) -> Just (Shift 11)
    (3, Token (LBRACE _)) -> Just (Shift 13)
    (4, Token (WHERE _)) -> Just (Reduce 0 3)
    (4, Token (LPAREN _)) -> Just (Shift 19)
    (5, Token (WHERE _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 1 1)
    (7, EOF) -> Just (Reduce 5 0)
    (8, Token (QCONID _)) -> Just (Shift 11)
    (9, Token (QCONID _)) -> Just (Shift 11)
    (10, Token (QCONID _)) -> Just (Shift 11)
    (11, Token (MODULE _)) -> Just (Reduce 1 253)
    (11, Token (WHERE _)) -> Just (Reduce 1 253)
    (11, Token (RBRACE _)) -> Just (Reduce 1 253)
    (11, Token (LPAREN _)) -> Just (Reduce 1 253)
    (11, Token (RPAREN _)) -> Just (Reduce 1 253)
    (11, Token (COMMA _)) -> Just (Reduce 1 253)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 253)
    (11, Token (HIDING _)) -> Just (Reduce 1 253)
    (11, Token (MINUS _)) -> Just (Reduce 1 253)
    (11, Token (QCONID _)) -> Just (Reduce 1 253)
    (11, Token (EXPORT _)) -> Just (Reduce 1 253)
    (11, Token (AS _)) -> Just (Reduce 1 253)
    (11, Token (QVARID _)) -> Just (Reduce 1 253)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 253)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 253)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 96)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 213)
    (13, Token (TYPE _)) -> Just (Shift 171)
    (13, Token (DATA _)) -> Just (Shift 149)
    (13, Token (NEWTYPE _)) -> Just (Shift 169)
    (13, Token (CLASS _)) -> Just (Shift 136)
    (13, Token (INSTANCE _)) -> Just (Shift 138)
    (13, Token (DEFAULT _)) -> Just (Shift 219)
    (13, Token (FOREIGN _)) -> Just (Shift 220)
    (13, Token (INFIXL _)) -> Just (Shift 332)
    (13, Token (INFIXR _)) -> Just (Shift 333)
    (13, Token (INFIX _)) -> Just (Shift 334)
    (13, Token (EXPORT _)) -> Just (Shift 131)
    (13, Token (AS _)) -> Just (Shift 132)
    (13, Token (QVARID _)) -> Just (Shift 133)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 96)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 213)
    (16, Token (TYPE _)) -> Just (Shift 171)
    (16, Token (DATA _)) -> Just (Shift 149)
    (16, Token (NEWTYPE _)) -> Just (Shift 169)
    (16, Token (CLASS _)) -> Just (Shift 136)
    (16, Token (INSTANCE _)) -> Just (Shift 138)
    (16, Token (DEFAULT _)) -> Just (Shift 219)
    (16, Token (FOREIGN _)) -> Just (Shift 220)
    (16, Token (INFIXL _)) -> Just (Shift 332)
    (16, Token (INFIXR _)) -> Just (Shift 333)
    (16, Token (INFIX _)) -> Just (Shift 334)
    (16, Token (EXPORT _)) -> Just (Shift 131)
    (16, Token (AS _)) -> Just (Shift 132)
    (16, Token (QVARID _)) -> Just (Shift 133)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 126)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 182)
    (19, Token (EXPORT _)) -> Just (Shift 131)
    (19, Token (AS _)) -> Just (Shift 132)
    (19, Token (QVARID _)) -> Just (Shift 133)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 126)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 182)
    (22, Token (EXPORT _)) -> Just (Shift 131)
    (22, Token (AS _)) -> Just (Shift 132)
    (22, Token (QVARID _)) -> Just (Shift 133)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 126)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 182)
    (25, Token (EXPORT _)) -> Just (Shift 131)
    (25, Token (AS _)) -> Just (Shift 132)
    (25, Token (QVARID _)) -> Just (Shift 133)
    (26, Token (RPAREN _)) -> Just (Reduce 3 11)
    (26, Token (COMMA _)) -> Just (Reduce 3 11)
    (27, Token (RPAREN _)) -> Just (Reduce 4 12)
    (27, Token (COMMA _)) -> Just (Reduce 4 12)
    (28, Token (RPAREN _)) -> Just (Reduce 4 13)
    (28, Token (COMMA _)) -> Just (Reduce 4 13)
    (29, Token (RPAREN _)) -> Just (Shift 27)
    (30, Token (RPAREN _)) -> Just (Reduce 2 14)
    (30, Token (COMMA _)) -> Just (Reduce 2 14)
    (31, Token (RPAREN _)) -> Just (Reduce 1 9)
    (31, Token (COMMA _)) -> Just (Reduce 1 9)
    (32, Token (LPAREN _)) -> Just (Shift 25)
    (32, Token (RPAREN _)) -> Just (Reduce 1 10)
    (32, Token (COMMA _)) -> Just (Reduce 1 10)
    (33, Token (RPAREN _)) -> Just (Shift 28)
    (34, Token (LPAREN _)) -> Just (Shift 55)
    (34, Token (MINUS _)) -> Just (Shift 34)
    (34, Token (LBRACKET _)) -> Just (Shift 65)
    (34, Token (EXPORT _)) -> Just (Shift 131)
    (34, Token (AS _)) -> Just (Shift 132)
    (34, Token (QVARID _)) -> Just (Shift 133)
    (34, Token (STRING _)) -> Just (Shift 473)
    (34, Token (CASE _)) -> Just (Shift 92)
    (34, Token (DO _)) -> Just (Shift 425)
    (34, Token (INTEGER _)) -> Just (Shift 475)
    (35, Token (WHERE _)) -> Just (Reduce 1 181)
    (35, Token (RBRACE _)) -> Just (Reduce 1 181)
    (35, Token (LPAREN _)) -> Just (Shift 55)
    (35, Token (RPAREN _)) -> Just (Reduce 1 181)
    (35, Token (COMMA _)) -> Just (Reduce 1 181)
    (35, Token (DOT_DOT _)) -> Just (Reduce 1 181)
    (35, Token (SEMICOLON _)) -> Just (Reduce 1 181)
    (35, Token (EQUAL _)) -> Just (Reduce 1 181)
    (35, Token (PIPE _)) -> Just (Reduce 1 181)
    (35, Token (COLON_COLON _)) -> Just (Reduce 1 181)
    (35, Token (MINUS _)) -> Just (Reduce 1 181)
    (35, Token (RARROW _)) -> Just (Reduce 1 181)
    (35, Token (LBRACKET _)) -> Just (Shift 65)
    (35, Token (RBRACKET _)) -> Just (Reduce 1 181)
    (35, Token (EXPORT _)) -> Just (Shift 131)
    (35, Token (AS _)) -> Just (Shift 132)
    (35, Token (QVARID _)) -> Just (Shift 133)
    (35, Token (STRING _)) -> Just (Shift 473)
    (35, Token (LARROW _)) -> Just (Reduce 1 181)
    (35, Token (THEN _)) -> Just (Reduce 1 181)
    (35, Token (ELSE _)) -> Just (Reduce 1 181)
    (35, Token (QVARSYM _)) -> Just (Reduce 1 181)
    (35, Token (BACKQUOTE _)) -> Just (Reduce 1 181)
    (35, Token (QCONSYM _)) -> Just (Reduce 1 181)
    (35, Token (OF _)) -> Just (Reduce 1 181)
    (35, Token (INTEGER _)) -> Just (Shift 475)
    (36, Token (LPAREN _)) -> Just (Shift 55)
    (36, Token (MINUS _)) -> Just (Shift 34)
    (36, Token (LBRACKET _)) -> Just (Shift 65)
    (36, Token (EXPORT _)) -> Just (Shift 131)
    (36, Token (AS _)) -> Just (Shift 132)
    (36, Token (QVARID _)) -> Just (Shift 133)
    (36, Token (STRING _)) -> Just (Shift 473)
    (36, Token (LET _)) -> Just (Shift 292)
    (36, Token (LAMBDA _)) -> Just (Shift 110)
    (36, Token (IF _)) -> Just (Shift 88)
    (36, Token (CASE _)) -> Just (Shift 92)
    (36, Token (DO _)) -> Just (Shift 425)
    (36, Token (INTEGER _)) -> Just (Shift 475)
    (37, Token (LPAREN _)) -> Just (Shift 55)
    (37, Token (MINUS _)) -> Just (Shift 34)
    (37, Token (LBRACKET _)) -> Just (Shift 65)
    (37, Token (EXPORT _)) -> Just (Shift 131)
    (37, Token (AS _)) -> Just (Shift 132)
    (37, Token (QVARID _)) -> Just (Shift 133)
    (37, Token (STRING _)) -> Just (Shift 473)
    (37, Token (LET _)) -> Just (Shift 292)
    (37, Token (LAMBDA _)) -> Just (Shift 110)
    (37, Token (IF _)) -> Just (Shift 88)
    (37, Token (CASE _)) -> Just (Shift 92)
    (37, Token (DO _)) -> Just (Shift 425)
    (37, Token (INTEGER _)) -> Just (Shift 475)
    (38, Token (LPAREN _)) -> Just (Shift 55)
    (38, Token (MINUS _)) -> Just (Shift 34)
    (38, Token (LBRACKET _)) -> Just (Shift 65)
    (38, Token (EXPORT _)) -> Just (Shift 131)
    (38, Token (AS _)) -> Just (Shift 132)
    (38, Token (QVARID _)) -> Just (Shift 133)
    (38, Token (STRING _)) -> Just (Shift 473)
    (38, Token (LET _)) -> Just (Shift 292)
    (38, Token (LAMBDA _)) -> Just (Shift 110)
    (38, Token (IF _)) -> Just (Shift 88)
    (38, Token (CASE _)) -> Just (Shift 92)
    (38, Token (DO _)) -> Just (Shift 425)
    (38, Token (INTEGER _)) -> Just (Shift 475)
    (39, Token (LPAREN _)) -> Just (Shift 55)
    (39, Token (MINUS _)) -> Just (Shift 34)
    (39, Token (LBRACKET _)) -> Just (Shift 65)
    (39, Token (EXPORT _)) -> Just (Shift 131)
    (39, Token (AS _)) -> Just (Shift 132)
    (39, Token (QVARID _)) -> Just (Shift 133)
    (39, Token (STRING _)) -> Just (Shift 473)
    (39, Token (LET _)) -> Just (Shift 292)
    (39, Token (LAMBDA _)) -> Just (Shift 110)
    (39, Token (IF _)) -> Just (Shift 88)
    (39, Token (CASE _)) -> Just (Shift 92)
    (39, Token (DO _)) -> Just (Shift 425)
    (39, Token (INTEGER _)) -> Just (Shift 475)
    (40, Token (LPAREN _)) -> Just (Shift 55)
    (40, Token (MINUS _)) -> Just (Shift 34)
    (40, Token (LBRACKET _)) -> Just (Shift 65)
    (40, Token (EXPORT _)) -> Just (Shift 131)
    (40, Token (AS _)) -> Just (Shift 132)
    (40, Token (QVARID _)) -> Just (Shift 133)
    (40, Token (STRING _)) -> Just (Shift 473)
    (40, Token (LET _)) -> Just (Shift 292)
    (40, Token (LAMBDA _)) -> Just (Shift 110)
    (40, Token (IF _)) -> Just (Shift 88)
    (40, Token (CASE _)) -> Just (Shift 92)
    (40, Token (DO _)) -> Just (Shift 425)
    (40, Token (INTEGER _)) -> Just (Shift 475)
    (41, Token (LPAREN _)) -> Just (Shift 55)
    (41, Token (MINUS _)) -> Just (Shift 34)
    (41, Token (LBRACKET _)) -> Just (Shift 65)
    (41, Token (EXPORT _)) -> Just (Shift 131)
    (41, Token (AS _)) -> Just (Shift 132)
    (41, Token (QVARID _)) -> Just (Shift 133)
    (41, Token (STRING _)) -> Just (Shift 473)
    (41, Token (LET _)) -> Just (Shift 292)
    (41, Token (LAMBDA _)) -> Just (Shift 110)
    (41, Token (IF _)) -> Just (Shift 88)
    (41, Token (CASE _)) -> Just (Shift 92)
    (41, Token (DO _)) -> Just (Shift 425)
    (41, Token (INTEGER _)) -> Just (Shift 475)
    (42, Token (LPAREN _)) -> Just (Shift 55)
    (42, Token (MINUS _)) -> Just (Shift 34)
    (42, Token (LBRACKET _)) -> Just (Shift 65)
    (42, Token (EXPORT _)) -> Just (Shift 131)
    (42, Token (AS _)) -> Just (Shift 132)
    (42, Token (QVARID _)) -> Just (Shift 133)
    (42, Token (STRING _)) -> Just (Shift 473)
    (42, Token (LET _)) -> Just (Shift 292)
    (42, Token (LAMBDA _)) -> Just (Shift 110)
    (42, Token (IF _)) -> Just (Shift 88)
    (42, Token (CASE _)) -> Just (Shift 92)
    (42, Token (DO _)) -> Just (Shift 425)
    (42, Token (INTEGER _)) -> Just (Shift 475)
    (43, Token (LPAREN _)) -> Just (Shift 55)
    (43, Token (MINUS _)) -> Just (Shift 34)
    (43, Token (LBRACKET _)) -> Just (Shift 65)
    (43, Token (EXPORT _)) -> Just (Shift 131)
    (43, Token (AS _)) -> Just (Shift 132)
    (43, Token (QVARID _)) -> Just (Shift 133)
    (43, Token (STRING _)) -> Just (Shift 473)
    (43, Token (LET _)) -> Just (Shift 292)
    (43, Token (LAMBDA _)) -> Just (Shift 110)
    (43, Token (IF _)) -> Just (Shift 88)
    (43, Token (CASE _)) -> Just (Shift 92)
    (43, Token (DO _)) -> Just (Shift 425)
    (43, Token (INTEGER _)) -> Just (Shift 475)
    (44, Token (LPAREN _)) -> Just (Shift 55)
    (44, Token (MINUS _)) -> Just (Shift 34)
    (44, Token (LBRACKET _)) -> Just (Shift 65)
    (44, Token (EXPORT _)) -> Just (Shift 131)
    (44, Token (AS _)) -> Just (Shift 132)
    (44, Token (QVARID _)) -> Just (Shift 133)
    (44, Token (STRING _)) -> Just (Shift 473)
    (44, Token (LET _)) -> Just (Shift 292)
    (44, Token (LAMBDA _)) -> Just (Shift 110)
    (44, Token (IF _)) -> Just (Shift 88)
    (44, Token (CASE _)) -> Just (Shift 92)
    (44, Token (DO _)) -> Just (Shift 425)
    (44, Token (INTEGER _)) -> Just (Shift 475)
    (45, Token (LPAREN _)) -> Just (Shift 55)
    (45, Token (MINUS _)) -> Just (Shift 34)
    (45, Token (LBRACKET _)) -> Just (Shift 65)
    (45, Token (EXPORT _)) -> Just (Shift 131)
    (45, Token (AS _)) -> Just (Shift 132)
    (45, Token (QVARID _)) -> Just (Shift 133)
    (45, Token (STRING _)) -> Just (Shift 473)
    (45, Token (LET _)) -> Just (Shift 292)
    (45, Token (LAMBDA _)) -> Just (Shift 110)
    (45, Token (IF _)) -> Just (Shift 88)
    (45, Token (CASE _)) -> Just (Shift 92)
    (45, Token (DO _)) -> Just (Shift 425)
    (45, Token (INTEGER _)) -> Just (Shift 475)
    (46, Token (LPAREN _)) -> Just (Shift 55)
    (46, Token (MINUS _)) -> Just (Shift 34)
    (46, Token (LBRACKET _)) -> Just (Shift 65)
    (46, Token (EXPORT _)) -> Just (Shift 131)
    (46, Token (AS _)) -> Just (Shift 132)
    (46, Token (QVARID _)) -> Just (Shift 133)
    (46, Token (STRING _)) -> Just (Shift 473)
    (46, Token (LET _)) -> Just (Shift 292)
    (46, Token (LAMBDA _)) -> Just (Shift 110)
    (46, Token (IF _)) -> Just (Shift 88)
    (46, Token (CASE _)) -> Just (Shift 92)
    (46, Token (DO _)) -> Just (Shift 425)
    (46, Token (INTEGER _)) -> Just (Shift 475)
    (47, Token (LPAREN _)) -> Just (Shift 55)
    (47, Token (MINUS _)) -> Just (Shift 34)
    (47, Token (LBRACKET _)) -> Just (Shift 65)
    (47, Token (EXPORT _)) -> Just (Shift 131)
    (47, Token (AS _)) -> Just (Shift 132)
    (47, Token (QVARID _)) -> Just (Shift 133)
    (47, Token (STRING _)) -> Just (Shift 473)
    (47, Token (LET _)) -> Just (Shift 292)
    (47, Token (LAMBDA _)) -> Just (Shift 110)
    (47, Token (IF _)) -> Just (Shift 88)
    (47, Token (CASE _)) -> Just (Shift 92)
    (47, Token (DO _)) -> Just (Shift 425)
    (47, Token (INTEGER _)) -> Just (Shift 475)
    (48, Token (LPAREN _)) -> Just (Shift 55)
    (48, Token (MINUS _)) -> Just (Shift 34)
    (48, Token (LBRACKET _)) -> Just (Shift 65)
    (48, Token (EXPORT _)) -> Just (Shift 131)
    (48, Token (AS _)) -> Just (Shift 132)
    (48, Token (QVARID _)) -> Just (Shift 133)
    (48, Token (STRING _)) -> Just (Shift 473)
    (48, Token (LET _)) -> Just (Shift 292)
    (48, Token (LAMBDA _)) -> Just (Shift 110)
    (48, Token (IF _)) -> Just (Shift 88)
    (48, Token (CASE _)) -> Just (Shift 92)
    (48, Token (DO _)) -> Just (Shift 425)
    (48, Token (INTEGER _)) -> Just (Shift 475)
    (49, Token (LPAREN _)) -> Just (Shift 55)
    (49, Token (MINUS _)) -> Just (Shift 34)
    (49, Token (LBRACKET _)) -> Just (Shift 65)
    (49, Token (EXPORT _)) -> Just (Shift 131)
    (49, Token (AS _)) -> Just (Shift 132)
    (49, Token (QVARID _)) -> Just (Shift 133)
    (49, Token (STRING _)) -> Just (Shift 473)
    (49, Token (LET _)) -> Just (Shift 292)
    (49, Token (LAMBDA _)) -> Just (Shift 110)
    (49, Token (IF _)) -> Just (Shift 88)
    (49, Token (CASE _)) -> Just (Shift 92)
    (49, Token (DO _)) -> Just (Shift 425)
    (49, Token (INTEGER _)) -> Just (Shift 475)
    (50, Token (LPAREN _)) -> Just (Shift 55)
    (50, Token (MINUS _)) -> Just (Shift 34)
    (50, Token (LBRACKET _)) -> Just (Shift 65)
    (50, Token (EXPORT _)) -> Just (Shift 131)
    (50, Token (AS _)) -> Just (Shift 132)
    (50, Token (QVARID _)) -> Just (Shift 133)
    (50, Token (STRING _)) -> Just (Shift 473)
    (50, Token (LET _)) -> Just (Shift 292)
    (50, Token (LAMBDA _)) -> Just (Shift 110)
    (50, Token (IF _)) -> Just (Shift 88)
    (50, Token (CASE _)) -> Just (Shift 92)
    (50, Token (DO _)) -> Just (Shift 425)
    (50, Token (INTEGER _)) -> Just (Shift 475)
    (51, Token (LPAREN _)) -> Just (Shift 55)
    (51, Token (MINUS _)) -> Just (Shift 34)
    (51, Token (LBRACKET _)) -> Just (Shift 65)
    (51, Token (EXPORT _)) -> Just (Shift 131)
    (51, Token (AS _)) -> Just (Shift 132)
    (51, Token (QVARID _)) -> Just (Shift 133)
    (51, Token (STRING _)) -> Just (Shift 473)
    (51, Token (LET _)) -> Just (Shift 292)
    (51, Token (LAMBDA _)) -> Just (Shift 110)
    (51, Token (IF _)) -> Just (Shift 88)
    (51, Token (CASE _)) -> Just (Shift 92)
    (51, Token (DO _)) -> Just (Shift 425)
    (51, Token (INTEGER _)) -> Just (Shift 475)
    (52, Token (RBRACE _)) -> Just (Reduce 0 220)
    (52, Token (LPAREN _)) -> Just (Shift 55)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 220)
    (52, Token (MINUS _)) -> Just (Shift 34)
    (52, Token (LBRACKET _)) -> Just (Shift 65)
    (52, Token (EXPORT _)) -> Just (Shift 131)
    (52, Token (AS _)) -> Just (Shift 132)
    (52, Token (QVARID _)) -> Just (Shift 133)
    (52, Token (STRING _)) -> Just (Shift 473)
    (52, Token (LET _)) -> Just (Shift 283)
    (52, Token (LAMBDA _)) -> Just (Shift 110)
    (52, Token (IF _)) -> Just (Shift 88)
    (52, Token (CASE _)) -> Just (Shift 92)
    (52, Token (DO _)) -> Just (Shift 425)
    (52, Token (INTEGER _)) -> Just (Shift 475)
    (53, Token (RBRACE _)) -> Just (Reduce 0 220)
    (53, Token (LPAREN _)) -> Just (Shift 55)
    (53, Token (SEMICOLON _)) -> Just (Reduce 0 220)
    (53, Token (MINUS _)) -> Just (Shift 34)
    (53, Token (LBRACKET _)) -> Just (Shift 65)
    (53, Token (EXPORT _)) -> Just (Shift 131)
    (53, Token (AS _)) -> Just (Shift 132)
    (53, Token (QVARID _)) -> Just (Shift 133)
    (53, Token (STRING _)) -> Just (Shift 473)
    (53, Token (LET _)) -> Just (Shift 283)
    (53, Token (LAMBDA _)) -> Just (Shift 110)
    (53, Token (IF _)) -> Just (Shift 88)
    (53, Token (CASE _)) -> Just (Shift 92)
    (53, Token (DO _)) -> Just (Shift 425)
    (53, Token (INTEGER _)) -> Just (Shift 475)
    (54, Token (LPAREN _)) -> Just (Shift 55)
    (54, Token (MINUS _)) -> Just (Shift 34)
    (54, Token (LBRACKET _)) -> Just (Shift 65)
    (54, Token (EXPORT _)) -> Just (Shift 131)
    (54, Token (AS _)) -> Just (Shift 132)
    (54, Token (QVARID _)) -> Just (Shift 133)
    (54, Token (STRING _)) -> Just (Shift 473)
    (54, Token (LET _)) -> Just (Shift 292)
    (54, Token (LAMBDA _)) -> Just (Shift 110)
    (54, Token (IF _)) -> Just (Shift 88)
    (54, Token (CASE _)) -> Just (Shift 92)
    (54, Token (DO _)) -> Just (Shift 425)
    (54, Token (INTEGER _)) -> Just (Shift 475)
    (55, Token (LPAREN _)) -> Just (Shift 55)
    (55, Token (MINUS _)) -> Just (Shift 56)
    (55, Token (LBRACKET _)) -> Just (Shift 65)
    (55, Token (EXPORT _)) -> Just (Shift 131)
    (55, Token (AS _)) -> Just (Shift 132)
    (55, Token (QVARID _)) -> Just (Shift 133)
    (55, Token (STRING _)) -> Just (Shift 473)
    (55, Token (LET _)) -> Just (Shift 292)
    (55, Token (LAMBDA _)) -> Just (Shift 110)
    (55, Token (IF _)) -> Just (Shift 88)
    (55, Token (QVARSYM _)) -> Just (Shift 57)
    (55, Token (BACKQUOTE _)) -> Just (Shift 474)
    (55, Token (QCONSYM _)) -> Just (Shift 63)
    (55, Token (CASE _)) -> Just (Shift 92)
    (55, Token (DO _)) -> Just (Shift 425)
    (55, Token (INTEGER _)) -> Just (Shift 475)
    (56, Token (LPAREN _)) -> Just (Shift 55)
    (56, Token (RPAREN _)) -> Just (Shift 128)
    (56, Token (MINUS _)) -> Just (Shift 34)
    (56, Token (LBRACKET _)) -> Just (Shift 65)
    (56, Token (EXPORT _)) -> Just (Shift 131)
    (56, Token (AS _)) -> Just (Shift 132)
    (56, Token (QVARID _)) -> Just (Shift 133)
    (56, Token (STRING _)) -> Just (Shift 473)
    (56, Token (CASE _)) -> Just (Shift 92)
    (56, Token (DO _)) -> Just (Shift 425)
    (56, Token (INTEGER _)) -> Just (Shift 475)
    (57, Token (LPAREN _)) -> Just (Shift 55)
    (57, Token (RPAREN _)) -> Just (Shift 129)
    (57, Token (MINUS _)) -> Just (Shift 34)
    (57, Token (LBRACKET _)) -> Just (Shift 65)
    (57, Token (EXPORT _)) -> Just (Shift 131)
    (57, Token (AS _)) -> Just (Shift 132)
    (57, Token (QVARID _)) -> Just (Shift 133)
    (57, Token (STRING _)) -> Just (Shift 473)
    (57, Token (LET _)) -> Just (Shift 292)
    (57, Token (LAMBDA _)) -> Just (Shift 110)
    (57, Token (IF _)) -> Just (Shift 88)
    (57, Token (CASE _)) -> Just (Shift 92)
    (57, Token (DO _)) -> Just (Shift 425)
    (57, Token (INTEGER _)) -> Just (Shift 475)
    (58, Token (LPAREN _)) -> Just (Shift 55)
    (58, Token (MINUS _)) -> Just (Shift 34)
    (58, Token (LBRACKET _)) -> Just (Shift 65)
    (58, Token (EXPORT _)) -> Just (Shift 131)
    (58, Token (AS _)) -> Just (Shift 132)
    (58, Token (QVARID _)) -> Just (Shift 133)
    (58, Token (STRING _)) -> Just (Shift 473)
    (58, Token (LET _)) -> Just (Shift 292)
    (58, Token (LAMBDA _)) -> Just (Shift 110)
    (58, Token (IF _)) -> Just (Shift 88)
    (58, Token (CASE _)) -> Just (Shift 92)
    (58, Token (DO _)) -> Just (Shift 425)
    (58, Token (INTEGER _)) -> Just (Shift 475)
    (59, Token (LPAREN _)) -> Just (Shift 55)
    (59, Token (MINUS _)) -> Just (Shift 34)
    (59, Token (LBRACKET _)) -> Just (Shift 65)
    (59, Token (EXPORT _)) -> Just (Shift 131)
    (59, Token (AS _)) -> Just (Shift 132)
    (59, Token (QVARID _)) -> Just (Shift 133)
    (59, Token (STRING _)) -> Just (Shift 473)
    (59, Token (LET _)) -> Just (Shift 292)
    (59, Token (LAMBDA _)) -> Just (Shift 110)
    (59, Token (IF _)) -> Just (Shift 88)
    (59, Token (CASE _)) -> Just (Shift 92)
    (59, Token (DO _)) -> Just (Shift 425)
    (59, Token (INTEGER _)) -> Just (Shift 475)
    (60, Token (LPAREN _)) -> Just (Shift 55)
    (60, Token (MINUS _)) -> Just (Shift 34)
    (60, Token (LBRACKET _)) -> Just (Shift 65)
    (60, Token (EXPORT _)) -> Just (Shift 131)
    (60, Token (AS _)) -> Just (Shift 132)
    (60, Token (QVARID _)) -> Just (Shift 133)
    (60, Token (STRING _)) -> Just (Shift 473)
    (60, Token (LET _)) -> Just (Shift 292)
    (60, Token (LAMBDA _)) -> Just (Shift 110)
    (60, Token (IF _)) -> Just (Shift 88)
    (60, Token (CASE _)) -> Just (Shift 92)
    (60, Token (DO _)) -> Just (Shift 425)
    (60, Token (INTEGER _)) -> Just (Shift 475)
    (61, Token (LPAREN _)) -> Just (Shift 55)
    (61, Token (MINUS _)) -> Just (Shift 34)
    (61, Token (LBRACKET _)) -> Just (Shift 65)
    (61, Token (EXPORT _)) -> Just (Shift 131)
    (61, Token (AS _)) -> Just (Shift 132)
    (61, Token (QVARID _)) -> Just (Shift 133)
    (61, Token (STRING _)) -> Just (Shift 473)
    (61, Token (LET _)) -> Just (Shift 292)
    (61, Token (LAMBDA _)) -> Just (Shift 110)
    (61, Token (IF _)) -> Just (Shift 88)
    (61, Token (CASE _)) -> Just (Shift 92)
    (61, Token (DO _)) -> Just (Shift 425)
    (61, Token (INTEGER _)) -> Just (Shift 475)
    (62, Token (LPAREN _)) -> Just (Shift 55)
    (62, Token (MINUS _)) -> Just (Shift 34)
    (62, Token (LBRACKET _)) -> Just (Shift 65)
    (62, Token (EXPORT _)) -> Just (Shift 131)
    (62, Token (AS _)) -> Just (Shift 132)
    (62, Token (QVARID _)) -> Just (Shift 133)
    (62, Token (STRING _)) -> Just (Shift 473)
    (62, Token (LET _)) -> Just (Shift 292)
    (62, Token (LAMBDA _)) -> Just (Shift 110)
    (62, Token (IF _)) -> Just (Shift 88)
    (62, Token (CASE _)) -> Just (Shift 92)
    (62, Token (DO _)) -> Just (Shift 425)
    (62, Token (INTEGER _)) -> Just (Shift 475)
    (63, Token (LPAREN _)) -> Just (Shift 55)
    (63, Token (MINUS _)) -> Just (Shift 34)
    (63, Token (LBRACKET _)) -> Just (Shift 65)
    (63, Token (EXPORT _)) -> Just (Shift 131)
    (63, Token (AS _)) -> Just (Shift 132)
    (63, Token (QVARID _)) -> Just (Shift 133)
    (63, Token (STRING _)) -> Just (Shift 473)
    (63, Token (LET _)) -> Just (Shift 292)
    (63, Token (LAMBDA _)) -> Just (Shift 110)
    (63, Token (IF _)) -> Just (Shift 88)
    (63, Token (CASE _)) -> Just (Shift 92)
    (63, Token (DO _)) -> Just (Shift 425)
    (63, Token (INTEGER _)) -> Just (Shift 475)
    (64, Token (LPAREN _)) -> Just (Shift 55)
    (64, Token (MINUS _)) -> Just (Shift 34)
    (64, Token (LBRACKET _)) -> Just (Shift 65)
    (64, Token (EXPORT _)) -> Just (Shift 131)
    (64, Token (AS _)) -> Just (Shift 132)
    (64, Token (QVARID _)) -> Just (Shift 133)
    (64, Token (STRING _)) -> Just (Shift 473)
    (64, Token (LET _)) -> Just (Shift 292)
    (64, Token (LAMBDA _)) -> Just (Shift 110)
    (64, Token (IF _)) -> Just (Shift 88)
    (64, Token (CASE _)) -> Just (Shift 92)
    (64, Token (DO _)) -> Just (Shift 425)
    (64, Token (INTEGER _)) -> Just (Shift 475)
    (65, Token (LPAREN _)) -> Just (Shift 55)
    (65, Token (MINUS _)) -> Just (Shift 34)
    (65, Token (LBRACKET _)) -> Just (Shift 65)
    (65, Token (EXPORT _)) -> Just (Shift 131)
    (65, Token (AS _)) -> Just (Shift 132)
    (65, Token (QVARID _)) -> Just (Shift 133)
    (65, Token (STRING _)) -> Just (Shift 473)
    (65, Token (LET _)) -> Just (Shift 292)
    (65, Token (LAMBDA _)) -> Just (Shift 110)
    (65, Token (IF _)) -> Just (Shift 88)
    (65, Token (CASE _)) -> Just (Shift 92)
    (65, Token (DO _)) -> Just (Shift 425)
    (65, Token (INTEGER _)) -> Just (Shift 475)
    (66, Token (LPAREN _)) -> Just (Shift 55)
    (66, Token (MINUS _)) -> Just (Shift 34)
    (66, Token (LBRACKET _)) -> Just (Shift 65)
    (66, Token (EXPORT _)) -> Just (Shift 131)
    (66, Token (AS _)) -> Just (Shift 132)
    (66, Token (QVARID _)) -> Just (Shift 133)
    (66, Token (STRING _)) -> Just (Shift 473)
    (66, Token (LET _)) -> Just (Shift 293)
    (66, Token (LAMBDA _)) -> Just (Shift 111)
    (66, Token (IF _)) -> Just (Shift 89)
    (66, Token (CASE _)) -> Just (Shift 92)
    (66, Token (DO _)) -> Just (Shift 425)
    (66, Token (INTEGER _)) -> Just (Shift 475)
    (67, Token (LPAREN _)) -> Just (Shift 55)
    (67, Token (MINUS _)) -> Just (Shift 34)
    (67, Token (LBRACKET _)) -> Just (Shift 65)
    (67, Token (EXPORT _)) -> Just (Shift 131)
    (67, Token (AS _)) -> Just (Shift 132)
    (67, Token (QVARID _)) -> Just (Shift 133)
    (67, Token (STRING _)) -> Just (Shift 473)
    (67, Token (LET _)) -> Just (Shift 293)
    (67, Token (LAMBDA _)) -> Just (Shift 111)
    (67, Token (IF _)) -> Just (Shift 89)
    (67, Token (CASE _)) -> Just (Shift 92)
    (67, Token (DO _)) -> Just (Shift 425)
    (67, Token (INTEGER _)) -> Just (Shift 475)
    (68, Token (LPAREN _)) -> Just (Shift 55)
    (68, Token (MINUS _)) -> Just (Shift 34)
    (68, Token (LBRACKET _)) -> Just (Shift 65)
    (68, Token (EXPORT _)) -> Just (Shift 131)
    (68, Token (AS _)) -> Just (Shift 132)
    (68, Token (QVARID _)) -> Just (Shift 133)
    (68, Token (STRING _)) -> Just (Shift 473)
    (68, Token (LET _)) -> Just (Shift 293)
    (68, Token (LAMBDA _)) -> Just (Shift 111)
    (68, Token (IF _)) -> Just (Shift 89)
    (68, Token (CASE _)) -> Just (Shift 92)
    (68, Token (DO _)) -> Just (Shift 425)
    (68, Token (INTEGER _)) -> Just (Shift 475)
    (69, Token (LPAREN _)) -> Just (Shift 55)
    (69, Token (MINUS _)) -> Just (Shift 34)
    (69, Token (LBRACKET _)) -> Just (Shift 65)
    (69, Token (EXPORT _)) -> Just (Shift 131)
    (69, Token (AS _)) -> Just (Shift 132)
    (69, Token (QVARID _)) -> Just (Shift 133)
    (69, Token (STRING _)) -> Just (Shift 473)
    (69, Token (LET _)) -> Just (Shift 293)
    (69, Token (LAMBDA _)) -> Just (Shift 111)
    (69, Token (IF _)) -> Just (Shift 89)
    (69, Token (CASE _)) -> Just (Shift 92)
    (69, Token (DO _)) -> Just (Shift 425)
    (69, Token (INTEGER _)) -> Just (Shift 475)
    (70, Token (LPAREN _)) -> Just (Shift 55)
    (70, Token (MINUS _)) -> Just (Shift 34)
    (70, Token (LBRACKET _)) -> Just (Shift 65)
    (70, Token (EXPORT _)) -> Just (Shift 131)
    (70, Token (AS _)) -> Just (Shift 132)
    (70, Token (QVARID _)) -> Just (Shift 133)
    (70, Token (STRING _)) -> Just (Shift 473)
    (70, Token (LET _)) -> Just (Shift 293)
    (70, Token (LAMBDA _)) -> Just (Shift 111)
    (70, Token (IF _)) -> Just (Shift 89)
    (70, Token (CASE _)) -> Just (Shift 92)
    (70, Token (DO _)) -> Just (Shift 425)
    (70, Token (INTEGER _)) -> Just (Shift 475)
    (71, Token (LPAREN _)) -> Just (Shift 55)
    (71, Token (MINUS _)) -> Just (Shift 34)
    (71, Token (LBRACKET _)) -> Just (Shift 65)
    (71, Token (EXPORT _)) -> Just (Shift 131)
    (71, Token (AS _)) -> Just (Shift 132)
    (71, Token (QVARID _)) -> Just (Shift 133)
    (71, Token (STRING _)) -> Just (Shift 473)
    (71, Token (LET _)) -> Just (Shift 293)
    (71, Token (LAMBDA _)) -> Just (Shift 111)
    (71, Token (IF _)) -> Just (Shift 89)
    (71, Token (CASE _)) -> Just (Shift 92)
    (71, Token (DO _)) -> Just (Shift 425)
    (71, Token (INTEGER _)) -> Just (Shift 475)
    (72, Token (LPAREN _)) -> Just (Shift 55)
    (72, Token (MINUS _)) -> Just (Shift 34)
    (72, Token (LBRACKET _)) -> Just (Shift 65)
    (72, Token (EXPORT _)) -> Just (Shift 131)
    (72, Token (AS _)) -> Just (Shift 132)
    (72, Token (QVARID _)) -> Just (Shift 133)
    (72, Token (STRING _)) -> Just (Shift 473)
    (72, Token (LET _)) -> Just (Shift 293)
    (72, Token (LAMBDA _)) -> Just (Shift 111)
    (72, Token (IF _)) -> Just (Shift 89)
    (72, Token (CASE _)) -> Just (Shift 92)
    (72, Token (DO _)) -> Just (Shift 425)
    (72, Token (INTEGER _)) -> Just (Shift 475)
    (73, Token (LPAREN _)) -> Just (Shift 55)
    (73, Token (MINUS _)) -> Just (Shift 34)
    (73, Token (LBRACKET _)) -> Just (Shift 65)
    (73, Token (EXPORT _)) -> Just (Shift 131)
    (73, Token (AS _)) -> Just (Shift 132)
    (73, Token (QVARID _)) -> Just (Shift 133)
    (73, Token (STRING _)) -> Just (Shift 473)
    (73, Token (LET _)) -> Just (Shift 293)
    (73, Token (LAMBDA _)) -> Just (Shift 111)
    (73, Token (IF _)) -> Just (Shift 89)
    (73, Token (CASE _)) -> Just (Shift 92)
    (73, Token (DO _)) -> Just (Shift 425)
    (73, Token (INTEGER _)) -> Just (Shift 475)
    (74, Token (LPAREN _)) -> Just (Shift 55)
    (74, Token (MINUS _)) -> Just (Shift 34)
    (74, Token (LBRACKET _)) -> Just (Shift 65)
    (74, Token (EXPORT _)) -> Just (Shift 131)
    (74, Token (AS _)) -> Just (Shift 132)
    (74, Token (QVARID _)) -> Just (Shift 133)
    (74, Token (STRING _)) -> Just (Shift 473)
    (74, Token (LET _)) -> Just (Shift 293)
    (74, Token (LAMBDA _)) -> Just (Shift 111)
    (74, Token (IF _)) -> Just (Shift 89)
    (74, Token (CASE _)) -> Just (Shift 92)
    (74, Token (DO _)) -> Just (Shift 425)
    (74, Token (INTEGER _)) -> Just (Shift 475)
    (75, Token (LPAREN _)) -> Just (Shift 55)
    (75, Token (MINUS _)) -> Just (Shift 34)
    (75, Token (LBRACKET _)) -> Just (Shift 65)
    (75, Token (EXPORT _)) -> Just (Shift 131)
    (75, Token (AS _)) -> Just (Shift 132)
    (75, Token (QVARID _)) -> Just (Shift 133)
    (75, Token (STRING _)) -> Just (Shift 473)
    (75, Token (LET _)) -> Just (Shift 293)
    (75, Token (LAMBDA _)) -> Just (Shift 111)
    (75, Token (IF _)) -> Just (Shift 89)
    (75, Token (CASE _)) -> Just (Shift 92)
    (75, Token (DO _)) -> Just (Shift 425)
    (75, Token (INTEGER _)) -> Just (Shift 475)
    (76, Token (LPAREN _)) -> Just (Shift 55)
    (76, Token (MINUS _)) -> Just (Shift 34)
    (76, Token (LBRACKET _)) -> Just (Shift 65)
    (76, Token (EXPORT _)) -> Just (Shift 131)
    (76, Token (AS _)) -> Just (Shift 132)
    (76, Token (QVARID _)) -> Just (Shift 133)
    (76, Token (STRING _)) -> Just (Shift 473)
    (76, Token (LET _)) -> Just (Shift 291)
    (76, Token (LAMBDA _)) -> Just (Shift 111)
    (76, Token (IF _)) -> Just (Shift 89)
    (76, Token (CASE _)) -> Just (Shift 92)
    (76, Token (DO _)) -> Just (Shift 425)
    (76, Token (INTEGER _)) -> Just (Shift 475)
    (77, Token (LPAREN _)) -> Just (Shift 55)
    (77, Token (MINUS _)) -> Just (Shift 34)
    (77, Token (LBRACKET _)) -> Just (Shift 65)
    (77, Token (EXPORT _)) -> Just (Shift 131)
    (77, Token (AS _)) -> Just (Shift 132)
    (77, Token (QVARID _)) -> Just (Shift 133)
    (77, Token (STRING _)) -> Just (Shift 473)
    (77, Token (LET _)) -> Just (Shift 291)
    (77, Token (LAMBDA _)) -> Just (Shift 111)
    (77, Token (IF _)) -> Just (Shift 89)
    (77, Token (CASE _)) -> Just (Shift 92)
    (77, Token (DO _)) -> Just (Shift 425)
    (77, Token (INTEGER _)) -> Just (Shift 475)
    (78, Token (LPAREN _)) -> Just (Shift 55)
    (78, Token (MINUS _)) -> Just (Shift 34)
    (78, Token (LBRACKET _)) -> Just (Shift 65)
    (78, Token (EXPORT _)) -> Just (Shift 131)
    (78, Token (AS _)) -> Just (Shift 132)
    (78, Token (QVARID _)) -> Just (Shift 133)
    (78, Token (STRING _)) -> Just (Shift 473)
    (78, Token (LET _)) -> Just (Shift 291)
    (78, Token (LAMBDA _)) -> Just (Shift 111)
    (78, Token (IF _)) -> Just (Shift 89)
    (78, Token (CASE _)) -> Just (Shift 92)
    (78, Token (DO _)) -> Just (Shift 425)
    (78, Token (INTEGER _)) -> Just (Shift 475)
    (79, Token (LPAREN _)) -> Just (Shift 55)
    (79, Token (MINUS _)) -> Just (Shift 34)
    (79, Token (LBRACKET _)) -> Just (Shift 65)
    (79, Token (EXPORT _)) -> Just (Shift 131)
    (79, Token (AS _)) -> Just (Shift 132)
    (79, Token (QVARID _)) -> Just (Shift 133)
    (79, Token (STRING _)) -> Just (Shift 473)
    (79, Token (LET _)) -> Just (Shift 293)
    (79, Token (LAMBDA _)) -> Just (Shift 111)
    (79, Token (IF _)) -> Just (Shift 89)
    (79, Token (CASE _)) -> Just (Shift 92)
    (79, Token (DO _)) -> Just (Shift 425)
    (79, Token (INTEGER _)) -> Just (Shift 475)
    (80, Token (LPAREN _)) -> Just (Shift 55)
    (80, Token (MINUS _)) -> Just (Shift 34)
    (80, Token (LBRACKET _)) -> Just (Shift 65)
    (80, Token (EXPORT _)) -> Just (Shift 131)
    (80, Token (AS _)) -> Just (Shift 132)
    (80, Token (QVARID _)) -> Just (Shift 133)
    (80, Token (STRING _)) -> Just (Shift 473)
    (80, Token (LET _)) -> Just (Shift 292)
    (80, Token (LAMBDA _)) -> Just (Shift 110)
    (80, Token (IF _)) -> Just (Shift 88)
    (80, Token (CASE _)) -> Just (Shift 92)
    (80, Token (DO _)) -> Just (Shift 425)
    (80, Token (INTEGER _)) -> Just (Shift 475)
    (81, Token (LPAREN _)) -> Just (Shift 55)
    (81, Token (MINUS _)) -> Just (Shift 34)
    (81, Token (LBRACKET _)) -> Just (Shift 65)
    (81, Token (RBRACKET _)) -> Just (Shift 464)
    (81, Token (EXPORT _)) -> Just (Shift 131)
    (81, Token (AS _)) -> Just (Shift 132)
    (81, Token (QVARID _)) -> Just (Shift 133)
    (81, Token (STRING _)) -> Just (Shift 473)
    (81, Token (LET _)) -> Just (Shift 292)
    (81, Token (LAMBDA _)) -> Just (Shift 110)
    (81, Token (IF _)) -> Just (Shift 88)
    (81, Token (CASE _)) -> Just (Shift 92)
    (81, Token (DO _)) -> Just (Shift 425)
    (81, Token (INTEGER _)) -> Just (Shift 475)
    (82, Token (LPAREN _)) -> Just (Shift 55)
    (82, Token (MINUS _)) -> Just (Shift 34)
    (82, Token (LBRACKET _)) -> Just (Shift 65)
    (82, Token (RBRACKET _)) -> Just (Shift 465)
    (82, Token (EXPORT _)) -> Just (Shift 131)
    (82, Token (AS _)) -> Just (Shift 132)
    (82, Token (QVARID _)) -> Just (Shift 133)
    (82, Token (STRING _)) -> Just (Shift 473)
    (82, Token (LET _)) -> Just (Shift 292)
    (82, Token (LAMBDA _)) -> Just (Shift 110)
    (82, Token (IF _)) -> Just (Shift 88)
    (82, Token (CASE _)) -> Just (Shift 92)
    (82, Token (DO _)) -> Just (Shift 425)
    (82, Token (INTEGER _)) -> Just (Shift 475)
    (83, Token (LPAREN _)) -> Just (Shift 55)
    (83, Token (MINUS _)) -> Just (Shift 34)
    (83, Token (LBRACKET _)) -> Just (Shift 65)
    (83, Token (EXPORT _)) -> Just (Shift 131)
    (83, Token (AS _)) -> Just (Shift 132)
    (83, Token (QVARID _)) -> Just (Shift 133)
    (83, Token (STRING _)) -> Just (Shift 473)
    (83, Token (LET _)) -> Just (Shift 290)
    (83, Token (LAMBDA _)) -> Just (Shift 110)
    (83, Token (IF _)) -> Just (Shift 88)
    (83, Token (CASE _)) -> Just (Shift 92)
    (83, Token (DO _)) -> Just (Shift 425)
    (83, Token (INTEGER _)) -> Just (Shift 475)
    (84, Token (LPAREN _)) -> Just (Shift 55)
    (84, Token (MINUS _)) -> Just (Shift 34)
    (84, Token (LBRACKET _)) -> Just (Shift 65)
    (84, Token (EXPORT _)) -> Just (Shift 131)
    (84, Token (AS _)) -> Just (Shift 132)
    (84, Token (QVARID _)) -> Just (Shift 133)
    (84, Token (STRING _)) -> Just (Shift 473)
    (84, Token (LET _)) -> Just (Shift 290)
    (84, Token (LAMBDA _)) -> Just (Shift 110)
    (84, Token (IF _)) -> Just (Shift 88)
    (84, Token (CASE _)) -> Just (Shift 92)
    (84, Token (DO _)) -> Just (Shift 425)
    (84, Token (INTEGER _)) -> Just (Shift 475)
    (85, Token (LPAREN _)) -> Just (Shift 55)
    (85, Token (MINUS _)) -> Just (Shift 34)
    (85, Token (LBRACKET _)) -> Just (Shift 65)
    (85, Token (EXPORT _)) -> Just (Shift 131)
    (85, Token (AS _)) -> Just (Shift 132)
    (85, Token (QVARID _)) -> Just (Shift 133)
    (85, Token (STRING _)) -> Just (Shift 473)
    (85, Token (LET _)) -> Just (Shift 290)
    (85, Token (LAMBDA _)) -> Just (Shift 110)
    (85, Token (IF _)) -> Just (Shift 88)
    (85, Token (CASE _)) -> Just (Shift 92)
    (85, Token (DO _)) -> Just (Shift 425)
    (85, Token (INTEGER _)) -> Just (Shift 475)
    (86, Token (LPAREN _)) -> Just (Shift 55)
    (86, Token (MINUS _)) -> Just (Shift 34)
    (86, Token (LBRACKET _)) -> Just (Shift 65)
    (86, Token (EXPORT _)) -> Just (Shift 131)
    (86, Token (AS _)) -> Just (Shift 132)
    (86, Token (QVARID _)) -> Just (Shift 133)
    (86, Token (STRING _)) -> Just (Shift 473)
    (86, Token (LET _)) -> Just (Shift 290)
    (86, Token (LAMBDA _)) -> Just (Shift 110)
    (86, Token (IF _)) -> Just (Shift 88)
    (86, Token (CASE _)) -> Just (Shift 92)
    (86, Token (DO _)) -> Just (Shift 425)
    (86, Token (INTEGER _)) -> Just (Shift 475)
    (87, Token (LPAREN _)) -> Just (Shift 55)
    (87, Token (MINUS _)) -> Just (Shift 34)
    (87, Token (LBRACKET _)) -> Just (Shift 65)
    (87, Token (EXPORT _)) -> Just (Shift 131)
    (87, Token (AS _)) -> Just (Shift 132)
    (87, Token (QVARID _)) -> Just (Shift 133)
    (87, Token (STRING _)) -> Just (Shift 473)
    (87, Token (LET _)) -> Just (Shift 290)
    (87, Token (LAMBDA _)) -> Just (Shift 110)
    (87, Token (IF _)) -> Just (Shift 88)
    (87, Token (CASE _)) -> Just (Shift 92)
    (87, Token (DO _)) -> Just (Shift 425)
    (87, Token (INTEGER _)) -> Just (Shift 475)
    (88, Token (LPAREN _)) -> Just (Shift 55)
    (88, Token (MINUS _)) -> Just (Shift 34)
    (88, Token (LBRACKET _)) -> Just (Shift 65)
    (88, Token (EXPORT _)) -> Just (Shift 131)
    (88, Token (AS _)) -> Just (Shift 132)
    (88, Token (QVARID _)) -> Just (Shift 133)
    (88, Token (STRING _)) -> Just (Shift 473)
    (88, Token (LET _)) -> Just (Shift 292)
    (88, Token (LAMBDA _)) -> Just (Shift 110)
    (88, Token (IF _)) -> Just (Shift 88)
    (88, Token (CASE _)) -> Just (Shift 92)
    (88, Token (DO _)) -> Just (Shift 425)
    (88, Token (INTEGER _)) -> Just (Shift 475)
    (89, Token (LPAREN _)) -> Just (Shift 55)
    (89, Token (MINUS _)) -> Just (Shift 34)
    (89, Token (LBRACKET _)) -> Just (Shift 65)
    (89, Token (EXPORT _)) -> Just (Shift 131)
    (89, Token (AS _)) -> Just (Shift 132)
    (89, Token (QVARID _)) -> Just (Shift 133)
    (89, Token (STRING _)) -> Just (Shift 473)
    (89, Token (LET _)) -> Just (Shift 292)
    (89, Token (LAMBDA _)) -> Just (Shift 110)
    (89, Token (IF _)) -> Just (Shift 88)
    (89, Token (CASE _)) -> Just (Shift 92)
    (89, Token (DO _)) -> Just (Shift 425)
    (89, Token (INTEGER _)) -> Just (Shift 475)
    (90, Token (LPAREN _)) -> Just (Shift 55)
    (90, Token (MINUS _)) -> Just (Shift 34)
    (90, Token (LBRACKET _)) -> Just (Shift 65)
    (90, Token (EXPORT _)) -> Just (Shift 131)
    (90, Token (AS _)) -> Just (Shift 132)
    (90, Token (QVARID _)) -> Just (Shift 133)
    (90, Token (STRING _)) -> Just (Shift 473)
    (90, Token (LET _)) -> Just (Shift 292)
    (90, Token (LAMBDA _)) -> Just (Shift 110)
    (90, Token (IF _)) -> Just (Shift 88)
    (90, Token (CASE _)) -> Just (Shift 92)
    (90, Token (DO _)) -> Just (Shift 425)
    (90, Token (INTEGER _)) -> Just (Shift 475)
    (91, Token (LPAREN _)) -> Just (Shift 55)
    (91, Token (MINUS _)) -> Just (Shift 34)
    (91, Token (LBRACKET _)) -> Just (Shift 65)
    (91, Token (EXPORT _)) -> Just (Shift 131)
    (91, Token (AS _)) -> Just (Shift 132)
    (91, Token (QVARID _)) -> Just (Shift 133)
    (91, Token (STRING _)) -> Just (Shift 473)
    (91, Token (LET _)) -> Just (Shift 292)
    (91, Token (LAMBDA _)) -> Just (Shift 110)
    (91, Token (IF _)) -> Just (Shift 88)
    (91, Token (CASE _)) -> Just (Shift 92)
    (91, Token (DO _)) -> Just (Shift 425)
    (91, Token (INTEGER _)) -> Just (Shift 475)
    (92, Token (LPAREN _)) -> Just (Shift 55)
    (92, Token (MINUS _)) -> Just (Shift 34)
    (92, Token (LBRACKET _)) -> Just (Shift 65)
    (92, Token (EXPORT _)) -> Just (Shift 131)
    (92, Token (AS _)) -> Just (Shift 132)
    (92, Token (QVARID _)) -> Just (Shift 133)
    (92, Token (STRING _)) -> Just (Shift 473)
    (92, Token (LET _)) -> Just (Shift 292)
    (92, Token (LAMBDA _)) -> Just (Shift 110)
    (92, Token (IF _)) -> Just (Shift 88)
    (92, Token (CASE _)) -> Just (Shift 92)
    (92, Token (DO _)) -> Just (Shift 425)
    (92, Token (INTEGER _)) -> Just (Shift 475)
    (93, Token (LPAREN _)) -> Just (Shift 55)
    (93, Token (MINUS _)) -> Just (Shift 34)
    (93, Token (LBRACKET _)) -> Just (Shift 65)
    (93, Token (EXPORT _)) -> Just (Shift 131)
    (93, Token (AS _)) -> Just (Shift 132)
    (93, Token (QVARID _)) -> Just (Shift 133)
    (93, Token (STRING _)) -> Just (Shift 473)
    (93, Token (LET _)) -> Just (Shift 292)
    (93, Token (LAMBDA _)) -> Just (Shift 110)
    (93, Token (IF _)) -> Just (Shift 88)
    (93, Token (CASE _)) -> Just (Shift 92)
    (93, Token (DO _)) -> Just (Shift 425)
    (93, Token (INTEGER _)) -> Just (Shift 475)
    (94, Token (LPAREN _)) -> Just (Shift 96)
    (94, Token (EXPORT _)) -> Just (Shift 131)
    (94, Token (AS _)) -> Just (Shift 132)
    (94, Token (QVARID _)) -> Just (Shift 133)
    (95, Token (LPAREN _)) -> Just (Shift 96)
    (95, Token (EXPORT _)) -> Just (Shift 131)
    (95, Token (AS _)) -> Just (Shift 132)
    (95, Token (QVARID _)) -> Just (Shift 133)
    (96, Token (LPAREN _)) -> Just (Shift 96)
    (96, Token (MINUS _)) -> Just (Shift 130)
    (96, Token (EXPORT _)) -> Just (Shift 131)
    (96, Token (AS _)) -> Just (Shift 132)
    (96, Token (QVARID _)) -> Just (Shift 133)
    (96, Token (QVARSYM _)) -> Just (Shift 134)
    (97, Token (LPAREN _)) -> Just (Shift 96)
    (97, Token (RPAREN _)) -> Just (Shift 507)
    (97, Token (MINUS _)) -> Just (Shift 94)
    (97, Token (EXPORT _)) -> Just (Shift 131)
    (97, Token (AS _)) -> Just (Shift 132)
    (97, Token (QVARID _)) -> Just (Shift 133)
    (97, Token (QVARSYM _)) -> Just (Shift 512)
    (97, Token (BACKQUOTE _)) -> Just (Shift 374)
    (97, Token (QCONSYM _)) -> Just (Shift 377)
    (98, Token (RBRACE _)) -> Just (Reduce 0 85)
    (98, Token (LPAREN _)) -> Just (Shift 96)
    (98, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (98, Token (INFIXL _)) -> Just (Shift 332)
    (98, Token (INFIXR _)) -> Just (Shift 333)
    (98, Token (INFIX _)) -> Just (Shift 334)
    (98, Token (EXPORT _)) -> Just (Shift 131)
    (98, Token (AS _)) -> Just (Shift 132)
    (98, Token (QVARID _)) -> Just (Shift 133)
    (99, Token (RBRACE _)) -> Just (Reduce 0 85)
    (99, Token (LPAREN _)) -> Just (Shift 96)
    (99, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (99, Token (INFIXL _)) -> Just (Shift 332)
    (99, Token (INFIXR _)) -> Just (Shift 333)
    (99, Token (INFIX _)) -> Just (Shift 334)
    (99, Token (EXPORT _)) -> Just (Shift 131)
    (99, Token (AS _)) -> Just (Shift 132)
    (99, Token (QVARID _)) -> Just (Shift 133)
    (100, Token (RBRACE _)) -> Just (Reduce 0 85)
    (100, Token (LPAREN _)) -> Just (Shift 96)
    (100, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (100, Token (INFIXL _)) -> Just (Shift 332)
    (100, Token (INFIXR _)) -> Just (Shift 333)
    (100, Token (INFIX _)) -> Just (Shift 334)
    (100, Token (EXPORT _)) -> Just (Shift 131)
    (100, Token (AS _)) -> Just (Shift 132)
    (100, Token (QVARID _)) -> Just (Shift 133)
    (101, Token (RBRACE _)) -> Just (Reduce 0 85)
    (101, Token (LPAREN _)) -> Just (Shift 96)
    (101, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (101, Token (INFIXL _)) -> Just (Shift 332)
    (101, Token (INFIXR _)) -> Just (Shift 333)
    (101, Token (INFIX _)) -> Just (Shift 334)
    (101, Token (EXPORT _)) -> Just (Shift 131)
    (101, Token (AS _)) -> Just (Shift 132)
    (101, Token (QVARID _)) -> Just (Shift 133)
    (102, Token (LPAREN _)) -> Just (Shift 96)
    (102, Token (EQUAL _)) -> Just (Shift 46)
    (102, Token (PIPE _)) -> Just (Shift 83)
    (102, Token (MINUS _)) -> Just (Shift 94)
    (102, Token (EXPORT _)) -> Just (Shift 131)
    (102, Token (AS _)) -> Just (Shift 132)
    (102, Token (QVARID _)) -> Just (Shift 133)
    (102, Token (QVARSYM _)) -> Just (Shift 512)
    (102, Token (BACKQUOTE _)) -> Just (Shift 374)
    (102, Token (QCONSYM _)) -> Just (Shift 377)
    (103, Token (RBRACE _)) -> Just (Reduce 0 80)
    (103, Token (LPAREN _)) -> Just (Shift 96)
    (103, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (103, Token (EXPORT _)) -> Just (Shift 131)
    (103, Token (AS _)) -> Just (Shift 132)
    (103, Token (QVARID _)) -> Just (Shift 133)
    (104, Token (RBRACE _)) -> Just (Reduce 0 80)
    (104, Token (LPAREN _)) -> Just (Shift 96)
    (104, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (104, Token (EXPORT _)) -> Just (Shift 131)
    (104, Token (AS _)) -> Just (Shift 132)
    (104, Token (QVARID _)) -> Just (Shift 133)
    (105, Token (LPAREN _)) -> Just (Shift 96)
    (105, Token (EQUAL _)) -> Just (Shift 49)
    (105, Token (PIPE _)) -> Just (Shift 85)
    (105, Token (MINUS _)) -> Just (Shift 94)
    (105, Token (EXPORT _)) -> Just (Shift 131)
    (105, Token (AS _)) -> Just (Shift 132)
    (105, Token (QVARID _)) -> Just (Shift 133)
    (105, Token (QVARSYM _)) -> Just (Shift 512)
    (105, Token (BACKQUOTE _)) -> Just (Shift 374)
    (105, Token (QCONSYM _)) -> Just (Shift 377)
    (106, Token (LPAREN _)) -> Just (Shift 96)
    (106, Token (EQUAL _)) -> Just (Shift 50)
    (106, Token (PIPE _)) -> Just (Shift 86)
    (106, Token (MINUS _)) -> Just (Shift 94)
    (106, Token (EXPORT _)) -> Just (Shift 131)
    (106, Token (AS _)) -> Just (Shift 132)
    (106, Token (QVARID _)) -> Just (Shift 133)
    (106, Token (QVARSYM _)) -> Just (Shift 512)
    (106, Token (BACKQUOTE _)) -> Just (Shift 374)
    (106, Token (QCONSYM _)) -> Just (Shift 377)
    (107, Token (RBRACE _)) -> Just (Reduce 0 206)
    (107, Token (LPAREN _)) -> Just (Shift 96)
    (107, Token (SEMICOLON _)) -> Just (Reduce 0 206)
    (107, Token (EXPORT _)) -> Just (Shift 131)
    (107, Token (AS _)) -> Just (Shift 132)
    (107, Token (QVARID _)) -> Just (Shift 133)
    (108, Token (RBRACE _)) -> Just (Reduce 0 206)
    (108, Token (LPAREN _)) -> Just (Shift 96)
    (108, Token (SEMICOLON _)) -> Just (Reduce 0 206)
    (108, Token (EXPORT _)) -> Just (Shift 131)
    (108, Token (AS _)) -> Just (Shift 132)
    (108, Token (QVARID _)) -> Just (Shift 133)
    (109, Token (LPAREN _)) -> Just (Shift 96)
    (109, Token (PIPE _)) -> Just (Shift 76)
    (109, Token (MINUS _)) -> Just (Shift 94)
    (109, Token (RARROW _)) -> Just (Shift 51)
    (109, Token (EXPORT _)) -> Just (Shift 131)
    (109, Token (AS _)) -> Just (Shift 132)
    (109, Token (QVARID _)) -> Just (Shift 133)
    (109, Token (QVARSYM _)) -> Just (Shift 512)
    (109, Token (BACKQUOTE _)) -> Just (Shift 374)
    (109, Token (QCONSYM _)) -> Just (Shift 377)
    (110, Token (LPAREN _)) -> Just (Shift 96)
    (110, Token (EXPORT _)) -> Just (Shift 131)
    (110, Token (AS _)) -> Just (Shift 132)
    (110, Token (QVARID _)) -> Just (Shift 133)
    (111, Token (LPAREN _)) -> Just (Shift 96)
    (111, Token (EXPORT _)) -> Just (Shift 131)
    (111, Token (AS _)) -> Just (Shift 132)
    (111, Token (QVARID _)) -> Just (Shift 133)
    (112, Token (LPAREN _)) -> Just (Shift 96)
    (112, Token (MINUS _)) -> Just (Shift 94)
    (112, Token (RARROW _)) -> Just (Shift 37)
    (112, Token (EXPORT _)) -> Just (Shift 131)
    (112, Token (AS _)) -> Just (Shift 132)
    (112, Token (QVARID _)) -> Just (Shift 133)
    (112, Token (QVARSYM _)) -> Just (Shift 512)
    (112, Token (BACKQUOTE _)) -> Just (Shift 374)
    (112, Token (QCONSYM _)) -> Just (Shift 377)
    (113, Token (LPAREN _)) -> Just (Shift 96)
    (113, Token (MINUS _)) -> Just (Shift 94)
    (113, Token (RARROW _)) -> Just (Shift 67)
    (113, Token (EXPORT _)) -> Just (Shift 131)
    (113, Token (AS _)) -> Just (Shift 132)
    (113, Token (QVARID _)) -> Just (Shift 133)
    (113, Token (QVARSYM _)) -> Just (Shift 512)
    (113, Token (BACKQUOTE _)) -> Just (Shift 374)
    (113, Token (QCONSYM _)) -> Just (Shift 377)
    (114, Token (LPAREN _)) -> Just (Shift 126)
    (114, Token (RPAREN _)) -> Just (Reduce 0 15)
    (114, Token (QCONID _)) -> Just (Shift 182)
    (114, Token (EXPORT _)) -> Just (Shift 131)
    (114, Token (AS _)) -> Just (Shift 132)
    (114, Token (QVARID _)) -> Just (Shift 133)
    (115, Token (LPAREN _)) -> Just (Shift 126)
    (115, Token (RPAREN _)) -> Just (Reduce 0 15)
    (115, Token (QCONID _)) -> Just (Shift 182)
    (115, Token (EXPORT _)) -> Just (Shift 131)
    (115, Token (AS _)) -> Just (Shift 132)
    (115, Token (QVARID _)) -> Just (Shift 133)
    (116, Token (LPAREN _)) -> Just (Shift 126)
    (116, Token (RPAREN _)) -> Just (Reduce 0 15)
    (116, Token (QCONID _)) -> Just (Shift 182)
    (116, Token (EXPORT _)) -> Just (Shift 131)
    (116, Token (AS _)) -> Just (Shift 132)
    (116, Token (QVARID _)) -> Just (Shift 133)
    (117, Token (LPAREN _)) -> Just (Shift 126)
    (117, Token (QCONID _)) -> Just (Shift 182)
    (117, Token (EXPORT _)) -> Just (Shift 131)
    (117, Token (AS _)) -> Just (Shift 132)
    (117, Token (QVARID _)) -> Just (Shift 133)
    (118, Token (LPAREN _)) -> Just (Shift 126)
    (118, Token (RPAREN _)) -> Just (Shift 188)
    (118, Token (DOT_DOT _)) -> Just (Shift 191)
    (118, Token (QCONID _)) -> Just (Shift 182)
    (118, Token (EXPORT _)) -> Just (Shift 131)
    (118, Token (AS _)) -> Just (Shift 132)
    (118, Token (QVARID _)) -> Just (Shift 133)
    (119, Token (LPAREN _)) -> Just (Shift 127)
    (119, Token (EXPORT _)) -> Just (Shift 131)
    (119, Token (AS _)) -> Just (Shift 132)
    (119, Token (QVARID _)) -> Just (Shift 133)
    (120, Token (RBRACE _)) -> Just (Shift 370)
    (120, Token (LPAREN _)) -> Just (Shift 127)
    (120, Token (EXPORT _)) -> Just (Shift 131)
    (120, Token (AS _)) -> Just (Shift 132)
    (120, Token (QVARID _)) -> Just (Shift 133)
    (121, Token (LPAREN _)) -> Just (Shift 127)
    (121, Token (EXPORT _)) -> Just (Shift 131)
    (121, Token (AS _)) -> Just (Shift 132)
    (121, Token (QVARID _)) -> Just (Shift 133)
    (122, Token (LPAREN _)) -> Just (Shift 127)
    (122, Token (EXPORT _)) -> Just (Shift 131)
    (122, Token (AS _)) -> Just (Shift 132)
    (122, Token (QVARID _)) -> Just (Shift 133)
    (123, Token (LPAREN _)) -> Just (Shift 127)
    (123, Token (EXPORT _)) -> Just (Shift 131)
    (123, Token (AS _)) -> Just (Shift 132)
    (123, Token (QVARID _)) -> Just (Shift 133)
    (124, Token (LPAREN _)) -> Just (Shift 127)
    (124, Token (EXPORT _)) -> Just (Shift 131)
    (124, Token (AS _)) -> Just (Shift 132)
    (124, Token (QVARID _)) -> Just (Shift 133)
    (125, Token (LPAREN _)) -> Just (Shift 127)
    (125, Token (EXPORT _)) -> Just (Shift 131)
    (125, Token (AS _)) -> Just (Shift 132)
    (125, Token (QVARID _)) -> Just (Shift 133)
    (126, Token (MINUS _)) -> Just (Shift 130)
    (126, Token (QVARSYM _)) -> Just (Shift 134)
    (126, Token (QCONSYM _)) -> Just (Shift 183)
    (127, Token (MINUS _)) -> Just (Shift 130)
    (127, Token (QVARSYM _)) -> Just (Shift 134)
    (128, Token (WHERE _)) -> Just (Reduce 3 233)
    (128, Token (LBRACE _)) -> Just (Reduce 3 233)
    (128, Token (RBRACE _)) -> Just (Reduce 3 233)
    (128, Token (LPAREN _)) -> Just (Reduce 3 233)
    (128, Token (RPAREN _)) -> Just (Reduce 3 233)
    (128, Token (COMMA _)) -> Just (Reduce 3 233)
    (128, Token (DOT_DOT _)) -> Just (Reduce 3 233)
    (128, Token (SEMICOLON _)) -> Just (Reduce 3 233)
    (128, Token (EQUAL _)) -> Just (Reduce 3 233)
    (128, Token (PIPE _)) -> Just (Reduce 3 233)
    (128, Token (COLON_COLON _)) -> Just (Reduce 3 233)
    (128, Token (MINUS _)) -> Just (Reduce 3 233)
    (128, Token (INFIXL _)) -> Just (Reduce 3 233)
    (128, Token (INFIXR _)) -> Just (Reduce 3 233)
    (128, Token (INFIX _)) -> Just (Reduce 3 233)
    (128, Token (RARROW _)) -> Just (Reduce 3 233)
    (128, Token (LBRACKET _)) -> Just (Reduce 3 233)
    (128, Token (RBRACKET _)) -> Just (Reduce 3 233)
    (128, Token (QCONID _)) -> Just (Reduce 3 233)
    (128, Token (EXPORT _)) -> Just (Reduce 3 233)
    (128, Token (AS _)) -> Just (Reduce 3 233)
    (128, Token (QVARID _)) -> Just (Reduce 3 233)
    (128, Token (STRING _)) -> Just (Reduce 3 233)
    (128, Token (LARROW _)) -> Just (Reduce 3 233)
    (128, Token (LET _)) -> Just (Reduce 3 233)
    (128, Token (LAMBDA _)) -> Just (Reduce 3 233)
    (128, Token (IF _)) -> Just (Reduce 3 233)
    (128, Token (THEN _)) -> Just (Reduce 3 233)
    (128, Token (ELSE _)) -> Just (Reduce 3 233)
    (128, Token (QVARSYM _)) -> Just (Reduce 3 233)
    (128, Token (BACKQUOTE _)) -> Just (Reduce 3 233)
    (128, Token (QCONSYM _)) -> Just (Reduce 3 233)
    (128, Token (CASE _)) -> Just (Reduce 3 233)
    (128, Token (OF _)) -> Just (Reduce 3 233)
    (128, Token (DO _)) -> Just (Reduce 3 233)
    (128, Token (INTEGER _)) -> Just (Reduce 3 233)
    (129, Token (WHERE _)) -> Just (Reduce 3 234)
    (129, Token (LBRACE _)) -> Just (Reduce 3 234)
    (129, Token (RBRACE _)) -> Just (Reduce 3 234)
    (129, Token (LPAREN _)) -> Just (Reduce 3 234)
    (129, Token (RPAREN _)) -> Just (Reduce 3 234)
    (129, Token (COMMA _)) -> Just (Reduce 3 234)
    (129, Token (DOT_DOT _)) -> Just (Reduce 3 234)
    (129, Token (SEMICOLON _)) -> Just (Reduce 3 234)
    (129, Token (EQUAL _)) -> Just (Reduce 3 234)
    (129, Token (PIPE _)) -> Just (Reduce 3 234)
    (129, Token (COLON_COLON _)) -> Just (Reduce 3 234)
    (129, Token (MINUS _)) -> Just (Reduce 3 234)
    (129, Token (INFIXL _)) -> Just (Reduce 3 234)
    (129, Token (INFIXR _)) -> Just (Reduce 3 234)
    (129, Token (INFIX _)) -> Just (Reduce 3 234)
    (129, Token (RARROW _)) -> Just (Reduce 3 234)
    (129, Token (LBRACKET _)) -> Just (Reduce 3 234)
    (129, Token (RBRACKET _)) -> Just (Reduce 3 234)
    (129, Token (QCONID _)) -> Just (Reduce 3 234)
    (129, Token (EXPORT _)) -> Just (Reduce 3 234)
    (129, Token (AS _)) -> Just (Reduce 3 234)
    (129, Token (QVARID _)) -> Just (Reduce 3 234)
    (129, Token (STRING _)) -> Just (Reduce 3 234)
    (129, Token (LARROW _)) -> Just (Reduce 3 234)
    (129, Token (LET _)) -> Just (Reduce 3 234)
    (129, Token (LAMBDA _)) -> Just (Reduce 3 234)
    (129, Token (IF _)) -> Just (Reduce 3 234)
    (129, Token (THEN _)) -> Just (Reduce 3 234)
    (129, Token (ELSE _)) -> Just (Reduce 3 234)
    (129, Token (QVARSYM _)) -> Just (Reduce 3 234)
    (129, Token (BACKQUOTE _)) -> Just (Reduce 3 234)
    (129, Token (QCONSYM _)) -> Just (Reduce 3 234)
    (129, Token (CASE _)) -> Just (Reduce 3 234)
    (129, Token (OF _)) -> Just (Reduce 3 234)
    (129, Token (DO _)) -> Just (Reduce 3 234)
    (129, Token (INTEGER _)) -> Just (Reduce 3 234)
    (130, Token (RPAREN _)) -> Just (Shift 128)
    (131, Token (WHERE _)) -> Just (Reduce 1 231)
    (131, Token (LBRACE _)) -> Just (Reduce 1 231)
    (131, Token (RBRACE _)) -> Just (Reduce 1 231)
    (131, Token (LPAREN _)) -> Just (Reduce 1 231)
    (131, Token (RPAREN _)) -> Just (Reduce 1 231)
    (131, Token (COMMA _)) -> Just (Reduce 1 231)
    (131, Token (DOT_DOT _)) -> Just (Reduce 1 231)
    (131, Token (SEMICOLON _)) -> Just (Reduce 1 231)
    (131, Token (EQUAL _)) -> Just (Reduce 1 231)
    (131, Token (PIPE _)) -> Just (Reduce 1 231)
    (131, Token (COLON_COLON _)) -> Just (Reduce 1 231)
    (131, Token (MINUS _)) -> Just (Reduce 1 231)
    (131, Token (INFIXL _)) -> Just (Reduce 1 231)
    (131, Token (INFIXR _)) -> Just (Reduce 1 231)
    (131, Token (INFIX _)) -> Just (Reduce 1 231)
    (131, Token (RARROW _)) -> Just (Reduce 1 231)
    (131, Token (LBRACKET _)) -> Just (Reduce 1 231)
    (131, Token (RBRACKET _)) -> Just (Reduce 1 231)
    (131, Token (QCONID _)) -> Just (Reduce 1 231)
    (131, Token (EXPORT _)) -> Just (Reduce 1 231)
    (131, Token (AS _)) -> Just (Reduce 1 231)
    (131, Token (QVARID _)) -> Just (Reduce 1 231)
    (131, Token (STRING _)) -> Just (Reduce 1 231)
    (131, Token (LARROW _)) -> Just (Reduce 1 231)
    (131, Token (LET _)) -> Just (Reduce 1 231)
    (131, Token (LAMBDA _)) -> Just (Reduce 1 231)
    (131, Token (IF _)) -> Just (Reduce 1 231)
    (131, Token (THEN _)) -> Just (Reduce 1 231)
    (131, Token (ELSE _)) -> Just (Reduce 1 231)
    (131, Token (QVARSYM _)) -> Just (Reduce 1 231)
    (131, Token (BACKQUOTE _)) -> Just (Reduce 1 231)
    (131, Token (QCONSYM _)) -> Just (Reduce 1 231)
    (131, Token (CASE _)) -> Just (Reduce 1 231)
    (131, Token (OF _)) -> Just (Reduce 1 231)
    (131, Token (DO _)) -> Just (Reduce 1 231)
    (131, Token (INTEGER _)) -> Just (Reduce 1 231)
    (132, Token (WHERE _)) -> Just (Reduce 1 230)
    (132, Token (LBRACE _)) -> Just (Reduce 1 230)
    (132, Token (RBRACE _)) -> Just (Reduce 1 230)
    (132, Token (LPAREN _)) -> Just (Reduce 1 230)
    (132, Token (RPAREN _)) -> Just (Reduce 1 230)
    (132, Token (COMMA _)) -> Just (Reduce 1 230)
    (132, Token (DOT_DOT _)) -> Just (Reduce 1 230)
    (132, Token (SEMICOLON _)) -> Just (Reduce 1 230)
    (132, Token (EQUAL _)) -> Just (Reduce 1 230)
    (132, Token (PIPE _)) -> Just (Reduce 1 230)
    (132, Token (COLON_COLON _)) -> Just (Reduce 1 230)
    (132, Token (MINUS _)) -> Just (Reduce 1 230)
    (132, Token (INFIXL _)) -> Just (Reduce 1 230)
    (132, Token (INFIXR _)) -> Just (Reduce 1 230)
    (132, Token (INFIX _)) -> Just (Reduce 1 230)
    (132, Token (RARROW _)) -> Just (Reduce 1 230)
    (132, Token (LBRACKET _)) -> Just (Reduce 1 230)
    (132, Token (RBRACKET _)) -> Just (Reduce 1 230)
    (132, Token (QCONID _)) -> Just (Reduce 1 230)
    (132, Token (EXPORT _)) -> Just (Reduce 1 230)
    (132, Token (AS _)) -> Just (Reduce 1 230)
    (132, Token (QVARID _)) -> Just (Reduce 1 230)
    (132, Token (STRING _)) -> Just (Reduce 1 230)
    (132, Token (LARROW _)) -> Just (Reduce 1 230)
    (132, Token (LET _)) -> Just (Reduce 1 230)
    (132, Token (LAMBDA _)) -> Just (Reduce 1 230)
    (132, Token (IF _)) -> Just (Reduce 1 230)
    (132, Token (THEN _)) -> Just (Reduce 1 230)
    (132, Token (ELSE _)) -> Just (Reduce 1 230)
    (132, Token (QVARSYM _)) -> Just (Reduce 1 230)
    (132, Token (BACKQUOTE _)) -> Just (Reduce 1 230)
    (132, Token (QCONSYM _)) -> Just (Reduce 1 230)
    (132, Token (CASE _)) -> Just (Reduce 1 230)
    (132, Token (OF _)) -> Just (Reduce 1 230)
    (132, Token (DO _)) -> Just (Reduce 1 230)
    (132, Token (INTEGER _)) -> Just (Reduce 1 230)
    (133, Token (WHERE _)) -> Just (Reduce 1 232)
    (133, Token (LBRACE _)) -> Just (Reduce 1 232)
    (133, Token (RBRACE _)) -> Just (Reduce 1 232)
    (133, Token (LPAREN _)) -> Just (Reduce 1 232)
    (133, Token (RPAREN _)) -> Just (Reduce 1 232)
    (133, Token (COMMA _)) -> Just (Reduce 1 232)
    (133, Token (DOT_DOT _)) -> Just (Reduce 1 232)
    (133, Token (SEMICOLON _)) -> Just (Reduce 1 232)
    (133, Token (EQUAL _)) -> Just (Reduce 1 232)
    (133, Token (PIPE _)) -> Just (Reduce 1 232)
    (133, Token (COLON_COLON _)) -> Just (Reduce 1 232)
    (133, Token (MINUS _)) -> Just (Reduce 1 232)
    (133, Token (INFIXL _)) -> Just (Reduce 1 232)
    (133, Token (INFIXR _)) -> Just (Reduce 1 232)
    (133, Token (INFIX _)) -> Just (Reduce 1 232)
    (133, Token (RARROW _)) -> Just (Reduce 1 232)
    (133, Token (LBRACKET _)) -> Just (Reduce 1 232)
    (133, Token (RBRACKET _)) -> Just (Reduce 1 232)
    (133, Token (QCONID _)) -> Just (Reduce 1 232)
    (133, Token (EXPORT _)) -> Just (Reduce 1 232)
    (133, Token (AS _)) -> Just (Reduce 1 232)
    (133, Token (QVARID _)) -> Just (Reduce 1 232)
    (133, Token (STRING _)) -> Just (Reduce 1 232)
    (133, Token (LARROW _)) -> Just (Reduce 1 232)
    (133, Token (LET _)) -> Just (Reduce 1 232)
    (133, Token (LAMBDA _)) -> Just (Reduce 1 232)
    (133, Token (IF _)) -> Just (Reduce 1 232)
    (133, Token (THEN _)) -> Just (Reduce 1 232)
    (133, Token (ELSE _)) -> Just (Reduce 1 232)
    (133, Token (QVARSYM _)) -> Just (Reduce 1 232)
    (133, Token (BACKQUOTE _)) -> Just (Reduce 1 232)
    (133, Token (QCONSYM _)) -> Just (Reduce 1 232)
    (133, Token (CASE _)) -> Just (Reduce 1 232)
    (133, Token (OF _)) -> Just (Reduce 1 232)
    (133, Token (DO _)) -> Just (Reduce 1 232)
    (133, Token (INTEGER _)) -> Just (Reduce 1 232)
    (134, Token (RPAREN _)) -> Just (Shift 129)
    (135, Token (LPAREN _)) -> Just (Shift 175)
    (135, Token (LBRACKET _)) -> Just (Shift 179)
    (135, Token (EXCL _)) -> Just (Shift 135)
    (135, Token (QCONID _)) -> Just (Shift 182)
    (135, Token (EXPORT _)) -> Just (Shift 361)
    (135, Token (AS _)) -> Just (Shift 362)
    (135, Token (QVARID _)) -> Just (Shift 363)
    (136, Token (LPAREN _)) -> Just (Shift 175)
    (136, Token (LBRACKET _)) -> Just (Shift 179)
    (136, Token (EXCL _)) -> Just (Shift 135)
    (136, Token (QCONID _)) -> Just (Shift 182)
    (136, Token (EXPORT _)) -> Just (Shift 361)
    (136, Token (AS _)) -> Just (Shift 362)
    (136, Token (QVARID _)) -> Just (Shift 363)
    (137, Token (WHERE _)) -> Just (Shift 259)
    (137, Token (RBRACE _)) -> Just (Reduce 0 65)
    (137, Token (LPAREN _)) -> Just (Shift 175)
    (137, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (137, Token (DARROW _)) -> Just (Shift 140)
    (137, Token (LBRACKET _)) -> Just (Shift 179)
    (137, Token (EXCL _)) -> Just (Shift 135)
    (137, Token (QCONID _)) -> Just (Shift 182)
    (137, Token (EXPORT _)) -> Just (Shift 361)
    (137, Token (AS _)) -> Just (Shift 362)
    (137, Token (QVARID _)) -> Just (Shift 363)
    (138, Token (LPAREN _)) -> Just (Shift 175)
    (138, Token (LBRACKET _)) -> Just (Shift 179)
    (138, Token (EXCL _)) -> Just (Shift 135)
    (138, Token (QCONID _)) -> Just (Shift 182)
    (138, Token (EXPORT _)) -> Just (Shift 361)
    (138, Token (AS _)) -> Just (Shift 362)
    (138, Token (QVARID _)) -> Just (Shift 363)
    (139, Token (WHERE _)) -> Just (Shift 261)
    (139, Token (RBRACE _)) -> Just (Reduce 0 75)
    (139, Token (LPAREN _)) -> Just (Shift 175)
    (139, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (139, Token (DARROW _)) -> Just (Shift 142)
    (139, Token (LBRACKET _)) -> Just (Shift 179)
    (139, Token (EXCL _)) -> Just (Shift 135)
    (139, Token (QCONID _)) -> Just (Shift 182)
    (139, Token (EXPORT _)) -> Just (Shift 361)
    (139, Token (AS _)) -> Just (Shift 362)
    (139, Token (QVARID _)) -> Just (Shift 363)
    (140, Token (LPAREN _)) -> Just (Shift 175)
    (140, Token (LBRACKET _)) -> Just (Shift 179)
    (140, Token (EXCL _)) -> Just (Shift 135)
    (140, Token (QCONID _)) -> Just (Shift 182)
    (140, Token (EXPORT _)) -> Just (Shift 361)
    (140, Token (AS _)) -> Just (Shift 362)
    (140, Token (QVARID _)) -> Just (Shift 363)
    (141, Token (WHERE _)) -> Just (Shift 259)
    (141, Token (RBRACE _)) -> Just (Reduce 0 65)
    (141, Token (LPAREN _)) -> Just (Shift 175)
    (141, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (141, Token (LBRACKET _)) -> Just (Shift 179)
    (141, Token (EXCL _)) -> Just (Shift 135)
    (141, Token (QCONID _)) -> Just (Shift 182)
    (141, Token (EXPORT _)) -> Just (Shift 361)
    (141, Token (AS _)) -> Just (Shift 362)
    (141, Token (QVARID _)) -> Just (Shift 363)
    (142, Token (LPAREN _)) -> Just (Shift 175)
    (142, Token (LBRACKET _)) -> Just (Shift 179)
    (142, Token (EXCL _)) -> Just (Shift 135)
    (142, Token (QCONID _)) -> Just (Shift 182)
    (142, Token (EXPORT _)) -> Just (Shift 361)
    (142, Token (AS _)) -> Just (Shift 362)
    (142, Token (QVARID _)) -> Just (Shift 363)
    (143, Token (WHERE _)) -> Just (Shift 261)
    (143, Token (RBRACE _)) -> Just (Reduce 0 75)
    (143, Token (LPAREN _)) -> Just (Shift 175)
    (143, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (143, Token (LBRACKET _)) -> Just (Shift 179)
    (143, Token (EXCL _)) -> Just (Shift 135)
    (143, Token (QCONID _)) -> Just (Shift 182)
    (143, Token (EXPORT _)) -> Just (Shift 361)
    (143, Token (AS _)) -> Just (Shift 362)
    (143, Token (QVARID _)) -> Just (Shift 363)
    (144, Token (LPAREN _)) -> Just (Shift 175)
    (144, Token (LBRACKET _)) -> Just (Shift 179)
    (144, Token (EXCL _)) -> Just (Shift 135)
    (144, Token (QCONID _)) -> Just (Shift 182)
    (144, Token (EXPORT _)) -> Just (Shift 361)
    (144, Token (AS _)) -> Just (Shift 362)
    (144, Token (QVARID _)) -> Just (Shift 363)
    (145, Token (WHERE _)) -> Just (Reduce 1 100)
    (145, Token (RBRACE _)) -> Just (Reduce 1 100)
    (145, Token (LPAREN _)) -> Just (Shift 175)
    (145, Token (RPAREN _)) -> Just (Reduce 1 100)
    (145, Token (COMMA _)) -> Just (Reduce 1 100)
    (145, Token (DOT_DOT _)) -> Just (Reduce 1 100)
    (145, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (145, Token (EQUAL _)) -> Just (Reduce 1 100)
    (145, Token (DARROW _)) -> Just (Shift 147)
    (145, Token (PIPE _)) -> Just (Reduce 1 100)
    (145, Token (RARROW _)) -> Just (Shift 146)
    (145, Token (LBRACKET _)) -> Just (Shift 179)
    (145, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (145, Token (EXCL _)) -> Just (Shift 135)
    (145, Token (QCONID _)) -> Just (Shift 182)
    (145, Token (EXPORT _)) -> Just (Shift 361)
    (145, Token (AS _)) -> Just (Shift 362)
    (145, Token (QVARID _)) -> Just (Shift 363)
    (145, Token (LARROW _)) -> Just (Reduce 1 100)
    (145, Token (THEN _)) -> Just (Reduce 1 100)
    (145, Token (ELSE _)) -> Just (Reduce 1 100)
    (145, Token (OF _)) -> Just (Reduce 1 100)
    (146, Token (LPAREN _)) -> Just (Shift 175)
    (146, Token (LBRACKET _)) -> Just (Shift 179)
    (146, Token (EXCL _)) -> Just (Shift 135)
    (146, Token (QCONID _)) -> Just (Shift 182)
    (146, Token (EXPORT _)) -> Just (Shift 361)
    (146, Token (AS _)) -> Just (Shift 362)
    (146, Token (QVARID _)) -> Just (Shift 363)
    (147, Token (LPAREN _)) -> Just (Shift 175)
    (147, Token (LBRACKET _)) -> Just (Shift 179)
    (147, Token (EXCL _)) -> Just (Shift 135)
    (147, Token (QCONID _)) -> Just (Shift 182)
    (147, Token (EXPORT _)) -> Just (Shift 361)
    (147, Token (AS _)) -> Just (Shift 362)
    (147, Token (QVARID _)) -> Just (Shift 363)
    (148, Token (WHERE _)) -> Just (Reduce 1 100)
    (148, Token (RBRACE _)) -> Just (Reduce 1 100)
    (148, Token (LPAREN _)) -> Just (Shift 175)
    (148, Token (RPAREN _)) -> Just (Reduce 1 100)
    (148, Token (COMMA _)) -> Just (Reduce 1 100)
    (148, Token (DOT_DOT _)) -> Just (Reduce 1 100)
    (148, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (148, Token (EQUAL _)) -> Just (Reduce 1 100)
    (148, Token (PIPE _)) -> Just (Reduce 1 100)
    (148, Token (RARROW _)) -> Just (Shift 146)
    (148, Token (LBRACKET _)) -> Just (Shift 179)
    (148, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (148, Token (EXCL _)) -> Just (Shift 135)
    (148, Token (QCONID _)) -> Just (Shift 182)
    (148, Token (EXPORT _)) -> Just (Shift 361)
    (148, Token (AS _)) -> Just (Shift 362)
    (148, Token (QVARID _)) -> Just (Shift 363)
    (148, Token (LARROW _)) -> Just (Reduce 1 100)
    (148, Token (THEN _)) -> Just (Reduce 1 100)
    (148, Token (ELSE _)) -> Just (Reduce 1 100)
    (148, Token (OF _)) -> Just (Reduce 1 100)
    (149, Token (LPAREN _)) -> Just (Shift 175)
    (149, Token (LBRACKET _)) -> Just (Shift 179)
    (149, Token (EXCL _)) -> Just (Shift 135)
    (149, Token (QCONID _)) -> Just (Shift 182)
    (149, Token (EXPORT _)) -> Just (Shift 361)
    (149, Token (AS _)) -> Just (Shift 362)
    (149, Token (QVARID _)) -> Just (Shift 363)
    (150, Token (RBRACE _)) -> Just (Reduce 0 119)
    (150, Token (LPAREN _)) -> Just (Shift 175)
    (150, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (150, Token (EQUAL _)) -> Just (Shift 153)
    (150, Token (DERIVING _)) -> Just (Reduce 0 119)
    (150, Token (DARROW _)) -> Just (Shift 151)
    (150, Token (LBRACKET _)) -> Just (Shift 179)
    (150, Token (EXCL _)) -> Just (Shift 135)
    (150, Token (QCONID _)) -> Just (Shift 182)
    (150, Token (EXPORT _)) -> Just (Shift 361)
    (150, Token (AS _)) -> Just (Shift 362)
    (150, Token (QVARID _)) -> Just (Shift 363)
    (151, Token (LPAREN _)) -> Just (Shift 175)
    (151, Token (LBRACKET _)) -> Just (Shift 179)
    (151, Token (EXCL _)) -> Just (Shift 135)
    (151, Token (QCONID _)) -> Just (Shift 182)
    (151, Token (EXPORT _)) -> Just (Shift 361)
    (151, Token (AS _)) -> Just (Shift 362)
    (151, Token (QVARID _)) -> Just (Shift 363)
    (152, Token (RBRACE _)) -> Just (Reduce 0 119)
    (152, Token (LPAREN _)) -> Just (Shift 175)
    (152, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (152, Token (EQUAL _)) -> Just (Shift 153)
    (152, Token (DERIVING _)) -> Just (Reduce 0 119)
    (152, Token (LBRACKET _)) -> Just (Shift 179)
    (152, Token (EXCL _)) -> Just (Shift 135)
    (152, Token (QCONID _)) -> Just (Shift 182)
    (152, Token (EXPORT _)) -> Just (Shift 361)
    (152, Token (AS _)) -> Just (Shift 362)
    (152, Token (QVARID _)) -> Just (Shift 363)
    (153, Token (LPAREN _)) -> Just (Shift 175)
    (153, Token (LBRACKET _)) -> Just (Shift 179)
    (153, Token (EXCL _)) -> Just (Shift 135)
    (153, Token (QCONID _)) -> Just (Shift 182)
    (153, Token (EXPORT _)) -> Just (Shift 361)
    (153, Token (AS _)) -> Just (Shift 362)
    (153, Token (QVARID _)) -> Just (Shift 363)
    (154, Token (LPAREN _)) -> Just (Shift 175)
    (154, Token (LBRACKET _)) -> Just (Shift 179)
    (154, Token (EXCL _)) -> Just (Shift 135)
    (154, Token (QCONID _)) -> Just (Shift 182)
    (154, Token (EXPORT _)) -> Just (Shift 361)
    (154, Token (AS _)) -> Just (Shift 362)
    (154, Token (QVARID _)) -> Just (Shift 363)
    (155, Token (LPAREN _)) -> Just (Shift 180)
    (155, Token (QCONID _)) -> Just (Shift 182)
    (156, Token (RBRACE _)) -> Just (Reduce 1 123)
    (156, Token (LPAREN _)) -> Just (Shift 175)
    (156, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (156, Token (DERIVING _)) -> Just (Reduce 1 123)
    (156, Token (PIPE _)) -> Just (Reduce 1 123)
    (156, Token (LBRACKET _)) -> Just (Shift 179)
    (156, Token (EXCL _)) -> Just (Shift 135)
    (156, Token (QCONID _)) -> Just (Shift 182)
    (156, Token (EXPORT _)) -> Just (Shift 361)
    (156, Token (AS _)) -> Just (Shift 362)
    (156, Token (QVARID _)) -> Just (Shift 363)
    (156, Token (BACKQUOTE _)) -> Just (Shift 375)
    (156, Token (QCONSYM _)) -> Just (Shift 377)
    (157, Token (LPAREN _)) -> Just (Shift 175)
    (157, Token (LBRACKET _)) -> Just (Shift 179)
    (157, Token (EXCL _)) -> Just (Shift 135)
    (157, Token (QCONID _)) -> Just (Shift 182)
    (157, Token (EXPORT _)) -> Just (Shift 361)
    (157, Token (AS _)) -> Just (Shift 362)
    (157, Token (QVARID _)) -> Just (Shift 363)
    (158, Token (RBRACE _)) -> Just (Reduce 3 124)
    (158, Token (LPAREN _)) -> Just (Shift 175)
    (158, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (158, Token (DERIVING _)) -> Just (Reduce 3 124)
    (158, Token (PIPE _)) -> Just (Reduce 3 124)
    (158, Token (LBRACKET _)) -> Just (Shift 179)
    (158, Token (EXCL _)) -> Just (Shift 135)
    (158, Token (QCONID _)) -> Just (Shift 182)
    (158, Token (EXPORT _)) -> Just (Shift 361)
    (158, Token (AS _)) -> Just (Shift 362)
    (158, Token (QVARID _)) -> Just (Shift 363)
    (159, Token (LPAREN _)) -> Just (Shift 175)
    (159, Token (LBRACKET _)) -> Just (Shift 179)
    (159, Token (EXCL _)) -> Just (Shift 135)
    (159, Token (QCONID _)) -> Just (Shift 182)
    (159, Token (EXPORT _)) -> Just (Shift 361)
    (159, Token (AS _)) -> Just (Shift 362)
    (159, Token (QVARID _)) -> Just (Shift 363)
    (160, Token (RBRACE _)) -> Just (Reduce 1 100)
    (160, Token (LPAREN _)) -> Just (Shift 175)
    (160, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (160, Token (DARROW _)) -> Just (Shift 165)
    (160, Token (RARROW _)) -> Just (Shift 146)
    (160, Token (LBRACKET _)) -> Just (Shift 179)
    (160, Token (EXCL _)) -> Just (Shift 135)
    (160, Token (QCONID _)) -> Just (Shift 182)
    (160, Token (EXPORT _)) -> Just (Shift 361)
    (160, Token (AS _)) -> Just (Shift 362)
    (160, Token (QVARID _)) -> Just (Shift 363)
    (161, Token (LPAREN _)) -> Just (Shift 175)
    (161, Token (LBRACKET _)) -> Just (Shift 179)
    (161, Token (EXCL _)) -> Just (Shift 135)
    (161, Token (QCONID _)) -> Just (Shift 182)
    (161, Token (EXPORT _)) -> Just (Shift 361)
    (161, Token (AS _)) -> Just (Shift 362)
    (161, Token (QVARID _)) -> Just (Shift 363)
    (162, Token (LPAREN _)) -> Just (Shift 175)
    (162, Token (LBRACKET _)) -> Just (Shift 179)
    (162, Token (EXCL _)) -> Just (Shift 135)
    (162, Token (QCONID _)) -> Just (Shift 182)
    (162, Token (EXPORT _)) -> Just (Shift 361)
    (162, Token (AS _)) -> Just (Shift 362)
    (162, Token (QVARID _)) -> Just (Shift 363)
    (163, Token (LPAREN _)) -> Just (Shift 175)
    (163, Token (LBRACKET _)) -> Just (Shift 179)
    (163, Token (EXCL _)) -> Just (Shift 135)
    (163, Token (QCONID _)) -> Just (Shift 182)
    (163, Token (EXPORT _)) -> Just (Shift 361)
    (163, Token (AS _)) -> Just (Shift 362)
    (163, Token (QVARID _)) -> Just (Shift 363)
    (164, Token (LPAREN _)) -> Just (Shift 175)
    (164, Token (LBRACKET _)) -> Just (Shift 179)
    (164, Token (EXCL _)) -> Just (Shift 135)
    (164, Token (QCONID _)) -> Just (Shift 182)
    (164, Token (EXPORT _)) -> Just (Shift 361)
    (164, Token (AS _)) -> Just (Shift 362)
    (164, Token (QVARID _)) -> Just (Shift 363)
    (165, Token (LPAREN _)) -> Just (Shift 175)
    (165, Token (LBRACKET _)) -> Just (Shift 179)
    (165, Token (EXCL _)) -> Just (Shift 135)
    (165, Token (QCONID _)) -> Just (Shift 182)
    (165, Token (EXPORT _)) -> Just (Shift 361)
    (165, Token (AS _)) -> Just (Shift 362)
    (165, Token (QVARID _)) -> Just (Shift 363)
    (166, Token (LPAREN _)) -> Just (Shift 175)
    (166, Token (LBRACKET _)) -> Just (Shift 179)
    (166, Token (EXCL _)) -> Just (Shift 135)
    (166, Token (QCONID _)) -> Just (Shift 182)
    (166, Token (EXPORT _)) -> Just (Shift 361)
    (166, Token (AS _)) -> Just (Shift 362)
    (166, Token (QVARID _)) -> Just (Shift 363)
    (167, Token (LPAREN _)) -> Just (Shift 175)
    (167, Token (LBRACKET _)) -> Just (Shift 179)
    (167, Token (EXCL _)) -> Just (Shift 135)
    (167, Token (QCONID _)) -> Just (Shift 182)
    (167, Token (EXPORT _)) -> Just (Shift 361)
    (167, Token (AS _)) -> Just (Shift 362)
    (167, Token (QVARID _)) -> Just (Shift 363)
    (168, Token (LBRACE _)) -> Just (Shift 122)
    (168, Token (LPAREN _)) -> Just (Shift 175)
    (168, Token (LBRACKET _)) -> Just (Shift 179)
    (168, Token (EXCL _)) -> Just (Shift 135)
    (168, Token (QCONID _)) -> Just (Shift 182)
    (168, Token (EXPORT _)) -> Just (Shift 361)
    (168, Token (AS _)) -> Just (Shift 362)
    (168, Token (QVARID _)) -> Just (Shift 363)
    (169, Token (LPAREN _)) -> Just (Shift 175)
    (169, Token (LBRACKET _)) -> Just (Shift 179)
    (169, Token (EXCL _)) -> Just (Shift 135)
    (169, Token (QCONID _)) -> Just (Shift 182)
    (169, Token (EXPORT _)) -> Just (Shift 361)
    (169, Token (AS _)) -> Just (Shift 362)
    (169, Token (QVARID _)) -> Just (Shift 363)
    (170, Token (LPAREN _)) -> Just (Shift 175)
    (170, Token (EQUAL _)) -> Just (Shift 155)
    (170, Token (DARROW _)) -> Just (Shift 172)
    (170, Token (LBRACKET _)) -> Just (Shift 179)
    (170, Token (EXCL _)) -> Just (Shift 135)
    (170, Token (QCONID _)) -> Just (Shift 182)
    (170, Token (EXPORT _)) -> Just (Shift 361)
    (170, Token (AS _)) -> Just (Shift 362)
    (170, Token (QVARID _)) -> Just (Shift 363)
    (171, Token (LPAREN _)) -> Just (Shift 175)
    (171, Token (LBRACKET _)) -> Just (Shift 179)
    (171, Token (EXCL _)) -> Just (Shift 135)
    (171, Token (QCONID _)) -> Just (Shift 182)
    (171, Token (EXPORT _)) -> Just (Shift 361)
    (171, Token (AS _)) -> Just (Shift 362)
    (171, Token (QVARID _)) -> Just (Shift 363)
    (172, Token (LPAREN _)) -> Just (Shift 175)
    (172, Token (LBRACKET _)) -> Just (Shift 179)
    (172, Token (EXCL _)) -> Just (Shift 135)
    (172, Token (QCONID _)) -> Just (Shift 182)
    (172, Token (EXPORT _)) -> Just (Shift 361)
    (172, Token (AS _)) -> Just (Shift 362)
    (172, Token (QVARID _)) -> Just (Shift 363)
    (173, Token (LPAREN _)) -> Just (Shift 175)
    (173, Token (EQUAL _)) -> Just (Shift 161)
    (173, Token (LBRACKET _)) -> Just (Shift 179)
    (173, Token (EXCL _)) -> Just (Shift 135)
    (173, Token (QCONID _)) -> Just (Shift 182)
    (173, Token (EXPORT _)) -> Just (Shift 361)
    (173, Token (AS _)) -> Just (Shift 362)
    (173, Token (QVARID _)) -> Just (Shift 363)
    (174, Token (LPAREN _)) -> Just (Shift 175)
    (174, Token (EQUAL _)) -> Just (Shift 155)
    (174, Token (LBRACKET _)) -> Just (Shift 179)
    (174, Token (EXCL _)) -> Just (Shift 135)
    (174, Token (QCONID _)) -> Just (Shift 182)
    (174, Token (EXPORT _)) -> Just (Shift 361)
    (174, Token (AS _)) -> Just (Shift 362)
    (174, Token (QVARID _)) -> Just (Shift 363)
    (175, Token (LPAREN _)) -> Just (Shift 175)
    (175, Token (RPAREN _)) -> Just (Shift 353)
    (175, Token (COMMA _)) -> Just (Shift 366)
    (175, Token (RARROW _)) -> Just (Shift 356)
    (175, Token (LBRACKET _)) -> Just (Shift 179)
    (175, Token (EXCL _)) -> Just (Shift 135)
    (175, Token (QCONID _)) -> Just (Shift 182)
    (175, Token (EXPORT _)) -> Just (Shift 361)
    (175, Token (AS _)) -> Just (Shift 362)
    (175, Token (QVARID _)) -> Just (Shift 363)
    (175, Token (QCONSYM _)) -> Just (Shift 183)
    (176, Token (LPAREN _)) -> Just (Shift 175)
    (176, Token (RPAREN _)) -> Just (Shift 205)
    (176, Token (LBRACKET _)) -> Just (Shift 179)
    (176, Token (EXCL _)) -> Just (Shift 135)
    (176, Token (QCONID _)) -> Just (Shift 182)
    (176, Token (EXPORT _)) -> Just (Shift 361)
    (176, Token (AS _)) -> Just (Shift 362)
    (176, Token (QVARID _)) -> Just (Shift 363)
    (177, Token (LPAREN _)) -> Just (Shift 175)
    (177, Token (LBRACKET _)) -> Just (Shift 179)
    (177, Token (EXCL _)) -> Just (Shift 135)
    (177, Token (QCONID _)) -> Just (Shift 182)
    (177, Token (EXPORT _)) -> Just (Shift 361)
    (177, Token (AS _)) -> Just (Shift 362)
    (177, Token (QVARID _)) -> Just (Shift 363)
    (178, Token (LPAREN _)) -> Just (Shift 175)
    (178, Token (LBRACKET _)) -> Just (Shift 179)
    (178, Token (EXCL _)) -> Just (Shift 135)
    (178, Token (QCONID _)) -> Just (Shift 182)
    (178, Token (EXPORT _)) -> Just (Shift 361)
    (178, Token (AS _)) -> Just (Shift 362)
    (178, Token (QVARID _)) -> Just (Shift 363)
    (179, Token (LPAREN _)) -> Just (Shift 175)
    (179, Token (LBRACKET _)) -> Just (Shift 179)
    (179, Token (RBRACKET _)) -> Just (Shift 357)
    (179, Token (EXCL _)) -> Just (Shift 135)
    (179, Token (QCONID _)) -> Just (Shift 182)
    (179, Token (EXPORT _)) -> Just (Shift 361)
    (179, Token (AS _)) -> Just (Shift 362)
    (179, Token (QVARID _)) -> Just (Shift 363)
    (180, Token (QCONSYM _)) -> Just (Shift 183)
    (181, Token (WHERE _)) -> Just (Reduce 3 236)
    (181, Token (LBRACE _)) -> Just (Reduce 3 236)
    (181, Token (RBRACE _)) -> Just (Reduce 3 236)
    (181, Token (LPAREN _)) -> Just (Reduce 3 236)
    (181, Token (RPAREN _)) -> Just (Reduce 3 236)
    (181, Token (COMMA _)) -> Just (Reduce 3 236)
    (181, Token (DOT_DOT _)) -> Just (Reduce 3 236)
    (181, Token (SEMICOLON _)) -> Just (Reduce 3 236)
    (181, Token (EQUAL _)) -> Just (Reduce 3 236)
    (181, Token (DERIVING _)) -> Just (Reduce 3 236)
    (181, Token (DARROW _)) -> Just (Reduce 3 236)
    (181, Token (PIPE _)) -> Just (Reduce 3 236)
    (181, Token (COLON_COLON _)) -> Just (Reduce 3 236)
    (181, Token (MINUS _)) -> Just (Reduce 3 236)
    (181, Token (INFIXL _)) -> Just (Reduce 3 236)
    (181, Token (INFIXR _)) -> Just (Reduce 3 236)
    (181, Token (INFIX _)) -> Just (Reduce 3 236)
    (181, Token (RARROW _)) -> Just (Reduce 3 236)
    (181, Token (LBRACKET _)) -> Just (Reduce 3 236)
    (181, Token (RBRACKET _)) -> Just (Reduce 3 236)
    (181, Token (EXCL _)) -> Just (Reduce 3 236)
    (181, Token (QCONID _)) -> Just (Reduce 3 236)
    (181, Token (EXPORT _)) -> Just (Reduce 3 236)
    (181, Token (AS _)) -> Just (Reduce 3 236)
    (181, Token (QVARID _)) -> Just (Reduce 3 236)
    (181, Token (LARROW _)) -> Just (Reduce 3 236)
    (181, Token (THEN _)) -> Just (Reduce 3 236)
    (181, Token (ELSE _)) -> Just (Reduce 3 236)
    (181, Token (QVARSYM _)) -> Just (Reduce 3 236)
    (181, Token (BACKQUOTE _)) -> Just (Reduce 3 236)
    (181, Token (QCONSYM _)) -> Just (Reduce 3 236)
    (181, Token (OF _)) -> Just (Reduce 3 236)
    (181, Token (INTEGER _)) -> Just (Reduce 3 236)
    (182, Token (WHERE _)) -> Just (Reduce 1 235)
    (182, Token (LBRACE _)) -> Just (Reduce 1 235)
    (182, Token (RBRACE _)) -> Just (Reduce 1 235)
    (182, Token (LPAREN _)) -> Just (Reduce 1 235)
    (182, Token (RPAREN _)) -> Just (Reduce 1 235)
    (182, Token (COMMA _)) -> Just (Reduce 1 235)
    (182, Token (DOT_DOT _)) -> Just (Reduce 1 235)
    (182, Token (SEMICOLON _)) -> Just (Reduce 1 235)
    (182, Token (EQUAL _)) -> Just (Reduce 1 235)
    (182, Token (DERIVING _)) -> Just (Reduce 1 235)
    (182, Token (DARROW _)) -> Just (Reduce 1 235)
    (182, Token (PIPE _)) -> Just (Reduce 1 235)
    (182, Token (COLON_COLON _)) -> Just (Reduce 1 235)
    (182, Token (MINUS _)) -> Just (Reduce 1 235)
    (182, Token (INFIXL _)) -> Just (Reduce 1 235)
    (182, Token (INFIXR _)) -> Just (Reduce 1 235)
    (182, Token (INFIX _)) -> Just (Reduce 1 235)
    (182, Token (RARROW _)) -> Just (Reduce 1 235)
    (182, Token (LBRACKET _)) -> Just (Reduce 1 235)
    (182, Token (RBRACKET _)) -> Just (Reduce 1 235)
    (182, Token (EXCL _)) -> Just (Reduce 1 235)
    (182, Token (QCONID _)) -> Just (Reduce 1 235)
    (182, Token (EXPORT _)) -> Just (Reduce 1 235)
    (182, Token (AS _)) -> Just (Reduce 1 235)
    (182, Token (QVARID _)) -> Just (Reduce 1 235)
    (182, Token (LARROW _)) -> Just (Reduce 1 235)
    (182, Token (THEN _)) -> Just (Reduce 1 235)
    (182, Token (ELSE _)) -> Just (Reduce 1 235)
    (182, Token (QVARSYM _)) -> Just (Reduce 1 235)
    (182, Token (BACKQUOTE _)) -> Just (Reduce 1 235)
    (182, Token (QCONSYM _)) -> Just (Reduce 1 235)
    (182, Token (OF _)) -> Just (Reduce 1 235)
    (182, Token (INTEGER _)) -> Just (Reduce 1 235)
    (183, Token (RPAREN _)) -> Just (Shift 181)
    (184, Token (RPAREN _)) -> Just (Reduce 3 24)
    (185, Token (RPAREN _)) -> Just (Reduce 1 23)
    (185, Token (COMMA _)) -> Just (Shift 117)
    (186, Token (RPAREN _)) -> Just (Reduce 3 17)
    (187, Token (RPAREN _)) -> Just (Reduce 1 16)
    (187, Token (COMMA _)) -> Just (Shift 114)
    (188, Token (RPAREN _)) -> Just (Reduce 3 20)
    (188, Token (COMMA _)) -> Just (Reduce 3 20)
    (189, Token (RPAREN _)) -> Just (Reduce 4 21)
    (189, Token (COMMA _)) -> Just (Reduce 4 21)
    (190, Token (RPAREN _)) -> Just (Reduce 4 22)
    (190, Token (COMMA _)) -> Just (Reduce 4 22)
    (191, Token (RPAREN _)) -> Just (Shift 189)
    (192, Token (RPAREN _)) -> Just (Reduce 1 18)
    (192, Token (COMMA _)) -> Just (Reduce 1 18)
    (193, Token (LPAREN _)) -> Just (Shift 118)
    (193, Token (RPAREN _)) -> Just (Reduce 1 19)
    (193, Token (COMMA _)) -> Just (Reduce 1 19)
    (194, Token (RPAREN _)) -> Just (Shift 190)
    (195, Token (RPAREN _)) -> Just (Reduce 1 25)
    (195, Token (COMMA _)) -> Just (Reduce 1 25)
    (196, Token (RPAREN _)) -> Just (Reduce 1 26)
    (196, Token (COMMA _)) -> Just (Reduce 1 26)
    (197, Token (RPAREN _)) -> Just (Shift 201)
    (197, Token (QCONID _)) -> Just (Shift 252)
    (198, Token (RPAREN _)) -> Just (Shift 202)
    (198, Token (QCONID _)) -> Just (Shift 252)
    (199, Token (RPAREN _)) -> Just (Shift 203)
    (199, Token (QCONID _)) -> Just (Shift 252)
    (200, Token (RPAREN _)) -> Just (Shift 204)
    (200, Token (QCONID _)) -> Just (Shift 252)
    (201, Token (RBRACE _)) -> Just (Reduce 6 35)
    (201, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (202, Token (RBRACE _)) -> Just (Reduce 8 39)
    (202, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (203, Token (RBRACE _)) -> Just (Reduce 8 47)
    (203, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (204, Token (RBRACE _)) -> Just (Reduce 6 43)
    (204, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (205, Token (RBRACE _)) -> Just (Reduce 3 53)
    (205, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (206, Token (RBRACE _)) -> Just (Reduce 8 31)
    (206, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (207, Token (RBRACE _)) -> Just (Reduce 7 30)
    (207, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (208, Token (RBRACE _)) -> Just (Reduce 7 36)
    (208, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (209, Token (RBRACE _)) -> Just (Reduce 9 40)
    (209, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (210, Token (RBRACE _)) -> Just (Reduce 9 48)
    (210, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (211, Token (RBRACE _)) -> Just (Reduce 7 44)
    (211, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (212, Token (RBRACE _)) -> Just (Reduce 4 54)
    (212, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (213, Token (QCONID _)) -> Just (Reduce 0 247)
    (213, Token (QUALIFIED _)) -> Just (Shift 245)
    (214, Token (LPAREN _)) -> Just (Shift 115)
    (215, Token (LPAREN _)) -> Just (Shift 197)
    (215, Token (QCONID _)) -> Just (Shift 252)
    (216, Token (LPAREN _)) -> Just (Shift 198)
    (216, Token (QCONID _)) -> Just (Shift 252)
    (217, Token (LPAREN _)) -> Just (Shift 199)
    (217, Token (QCONID _)) -> Just (Shift 252)
    (218, Token (LPAREN _)) -> Just (Shift 200)
    (218, Token (QCONID _)) -> Just (Shift 252)
    (219, Token (LPAREN _)) -> Just (Shift 176)
    (220, Token (IMPORT _)) -> Just (Shift 265)
    (220, Token (EXPORT _)) -> Just (Shift 266)
    (221, Token (RBRACE _)) -> Just (Reduce 0 245)
    (221, Token (LPAREN _)) -> Just (Reduce 0 245)
    (221, Token (SEMICOLON _)) -> Just (Reduce 0 245)
    (221, Token (HIDING _)) -> Just (Reduce 0 245)
    (221, Token (AS _)) -> Just (Shift 9)
    (222, Token (RPAREN _)) -> Just (Shift 206)
    (223, Token (RPAREN _)) -> Just (Shift 207)
    (224, Token (RBRACE _)) -> Just (Reduce 4 29)
    (224, Token (LPAREN _)) -> Just (Shift 116)
    (224, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (224, Token (HIDING _)) -> Just (Shift 214)
    (225, Token (RBRACE _)) -> Just (Reduce 4 32)
    (225, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (226, Token (RBRACE _)) -> Just (Reduce 3 33)
    (226, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (226, Token (DERIVING _)) -> Just (Shift 215)
    (227, Token (RBRACE _)) -> Just (Reduce 5 37)
    (227, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (227, Token (DERIVING _)) -> Just (Shift 216)
    (228, Token (RBRACE _)) -> Just (Reduce 5 34)
    (228, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (229, Token (RBRACE _)) -> Just (Reduce 7 38)
    (229, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (230, Token (RBRACE _)) -> Just (Reduce 7 46)
    (230, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (231, Token (RBRACE _)) -> Just (Reduce 5 42)
    (231, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (232, Token (RPAREN _)) -> Just (Shift 208)
    (233, Token (RPAREN _)) -> Just (Shift 209)
    (234, Token (RPAREN _)) -> Just (Shift 210)
    (235, Token (RPAREN _)) -> Just (Shift 211)
    (236, Token (RBRACE _)) -> Just (Reduce 5 45)
    (236, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (236, Token (DERIVING _)) -> Just (Shift 217)
    (237, Token (RBRACE _)) -> Just (Reduce 3 41)
    (237, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (237, Token (DERIVING _)) -> Just (Shift 218)
    (238, Token (RBRACE _)) -> Just (Reduce 5 50)
    (238, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (239, Token (RBRACE _)) -> Just (Reduce 3 49)
    (239, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (240, Token (RBRACE _)) -> Just (Reduce 5 52)
    (240, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (241, Token (RBRACE _)) -> Just (Reduce 3 51)
    (241, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (242, Token (RPAREN _)) -> Just (Shift 212)
    (243, Token (RBRACE _)) -> Just (Reduce 2 55)
    (243, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (244, Token (RBRACE _)) -> Just (Reduce 1 56)
    (244, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (245, Token (QCONID _)) -> Just (Reduce 1 248)
    (246, Token (RBRACE _)) -> Just (Reduce 2 246)
    (246, Token (LPAREN _)) -> Just (Reduce 2 246)
    (246, Token (SEMICOLON _)) -> Just (Reduce 2 246)
    (246, Token (HIDING _)) -> Just (Reduce 2 246)
    (247, Token (WHERE _)) -> Just (Reduce 1 102)
    (247, Token (LBRACE _)) -> Just (Reduce 1 102)
    (247, Token (RBRACE _)) -> Just (Reduce 1 102)
    (247, Token (LPAREN _)) -> Just (Reduce 1 102)
    (247, Token (RPAREN _)) -> Just (Reduce 1 102)
    (247, Token (COMMA _)) -> Just (Reduce 1 102)
    (247, Token (DOT_DOT _)) -> Just (Reduce 1 102)
    (247, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (247, Token (EQUAL _)) -> Just (Reduce 1 102)
    (247, Token (DERIVING _)) -> Just (Reduce 1 102)
    (247, Token (DARROW _)) -> Just (Reduce 1 102)
    (247, Token (PIPE _)) -> Just (Reduce 1 102)
    (247, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (247, Token (MINUS _)) -> Just (Reduce 1 102)
    (247, Token (INFIXL _)) -> Just (Reduce 1 102)
    (247, Token (INFIXR _)) -> Just (Reduce 1 102)
    (247, Token (INFIX _)) -> Just (Reduce 1 102)
    (247, Token (RARROW _)) -> Just (Reduce 1 102)
    (247, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (247, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (247, Token (EXCL _)) -> Just (Reduce 1 102)
    (247, Token (QCONID _)) -> Just (Reduce 1 102)
    (247, Token (EXPORT _)) -> Just (Reduce 1 102)
    (247, Token (AS _)) -> Just (Reduce 1 102)
    (247, Token (QVARID _)) -> Just (Reduce 1 102)
    (247, Token (LARROW _)) -> Just (Reduce 1 102)
    (247, Token (THEN _)) -> Just (Reduce 1 102)
    (247, Token (ELSE _)) -> Just (Reduce 1 102)
    (247, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (247, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (247, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (247, Token (OF _)) -> Just (Reduce 1 102)
    (247, Token (INTEGER _)) -> Just (Reduce 1 102)
    (248, Token (WHERE _)) -> Just (Reduce 2 103)
    (248, Token (LBRACE _)) -> Just (Reduce 2 103)
    (248, Token (RBRACE _)) -> Just (Reduce 2 103)
    (248, Token (LPAREN _)) -> Just (Reduce 2 103)
    (248, Token (RPAREN _)) -> Just (Reduce 2 103)
    (248, Token (COMMA _)) -> Just (Reduce 2 103)
    (248, Token (DOT_DOT _)) -> Just (Reduce 2 103)
    (248, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (248, Token (EQUAL _)) -> Just (Reduce 2 103)
    (248, Token (DERIVING _)) -> Just (Reduce 2 103)
    (248, Token (DARROW _)) -> Just (Reduce 2 103)
    (248, Token (PIPE _)) -> Just (Reduce 2 103)
    (248, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (248, Token (MINUS _)) -> Just (Reduce 2 103)
    (248, Token (INFIXL _)) -> Just (Reduce 2 103)
    (248, Token (INFIXR _)) -> Just (Reduce 2 103)
    (248, Token (INFIX _)) -> Just (Reduce 2 103)
    (248, Token (RARROW _)) -> Just (Reduce 2 103)
    (248, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (248, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (248, Token (EXCL _)) -> Just (Reduce 2 103)
    (248, Token (QCONID _)) -> Just (Reduce 2 103)
    (248, Token (EXPORT _)) -> Just (Reduce 2 103)
    (248, Token (AS _)) -> Just (Reduce 2 103)
    (248, Token (QVARID _)) -> Just (Reduce 2 103)
    (248, Token (LARROW _)) -> Just (Reduce 2 103)
    (248, Token (THEN _)) -> Just (Reduce 2 103)
    (248, Token (ELSE _)) -> Just (Reduce 2 103)
    (248, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (248, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (248, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (248, Token (OF _)) -> Just (Reduce 2 103)
    (248, Token (INTEGER _)) -> Just (Reduce 2 103)
    (249, Token (WHERE _)) -> Just (Reduce 3 101)
    (249, Token (RBRACE _)) -> Just (Reduce 3 101)
    (249, Token (RPAREN _)) -> Just (Reduce 3 101)
    (249, Token (COMMA _)) -> Just (Reduce 3 101)
    (249, Token (DOT_DOT _)) -> Just (Reduce 3 101)
    (249, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (249, Token (EQUAL _)) -> Just (Reduce 3 101)
    (249, Token (PIPE _)) -> Just (Reduce 3 101)
    (249, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (249, Token (LARROW _)) -> Just (Reduce 3 101)
    (249, Token (THEN _)) -> Just (Reduce 3 101)
    (249, Token (ELSE _)) -> Just (Reduce 3 101)
    (249, Token (OF _)) -> Just (Reduce 3 101)
    (250, Token (RBRACE _)) -> Just (Reduce 2 120)
    (250, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (250, Token (DERIVING _)) -> Just (Reduce 2 120)
    (251, Token (QCONID _)) -> Just (Shift 252)
    (252, Token (RBRACE _)) -> Just (Reduce 1 134)
    (252, Token (RPAREN _)) -> Just (Reduce 1 134)
    (252, Token (COMMA _)) -> Just (Reduce 1 134)
    (252, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (253, Token (RPAREN _)) -> Just (Reduce 1 132)
    (253, Token (COMMA _)) -> Just (Shift 251)
    (254, Token (RPAREN _)) -> Just (Reduce 3 133)
    (255, Token (RBRACE _)) -> Just (Reduce 7 128)
    (255, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (255, Token (DERIVING _)) -> Just (Reduce 7 128)
    (256, Token (COLON_COLON _)) -> Just (Shift 167)
    (257, Token (RBRACE _)) -> Just (Shift 255)
    (258, Token (RBRACE _)) -> Just (Reduce 3 127)
    (258, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (258, Token (DERIVING _)) -> Just (Reduce 3 127)
    (259, Token (LBRACE _)) -> Just (Shift 100)
    (260, Token (RBRACE _)) -> Just (Reduce 2 66)
    (260, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (261, Token (LBRACE _)) -> Just (Shift 103)
    (262, Token (RBRACE _)) -> Just (Reduce 2 76)
    (262, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (263, Token (RPAREN _)) -> Just (Reduce 1 98)
    (263, Token (COMMA _)) -> Just (Shift 177)
    (264, Token (RPAREN _)) -> Just (Reduce 3 99)
    (265, Token (EXPORT _)) -> Just (Shift 382)
    (265, Token (AS _)) -> Just (Shift 383)
    (265, Token (QVARID _)) -> Just (Shift 384)
    (266, Token (EXPORT _)) -> Just (Shift 382)
    (266, Token (AS _)) -> Just (Shift 383)
    (266, Token (QVARID _)) -> Just (Shift 384)
    (267, Token (COLON_COLON _)) -> Just (Shift 162)
    (268, Token (COLON_COLON _)) -> Just (Shift 163)
    (269, Token (COLON_COLON _)) -> Just (Shift 164)
    (270, Token (RBRACE _)) -> Just (Reduce 6 135)
    (270, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (271, Token (RBRACE _)) -> Just (Reduce 7 136)
    (271, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (272, Token (RBRACE _)) -> Just (Reduce 6 137)
    (272, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (273, Token (EXPORT _)) -> Just (Shift 386)
    (273, Token (AS _)) -> Just (Shift 387)
    (273, Token (QVARID _)) -> Just (Shift 388)
    (273, Token (STRING _)) -> Just (Shift 385)
    (274, Token (STRING _)) -> Just (Shift 389)
    (275, Token (STRING _)) -> Just (Shift 385)
    (276, Token (LBRACE _)) -> Just (Shift 98)
    (277, Token (LBRACE _)) -> Just (Shift 98)
    (278, Token (RBRACE _)) -> Just (Reduce 5 62)
    (278, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (279, Token (RBRACE _)) -> Just (Reduce 5 64)
    (279, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (280, Token (RBRACE _)) -> Just (Reduce 1 60)
    (280, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (281, Token (WHERE _)) -> Just (Shift 276)
    (281, Token (RBRACE _)) -> Just (Reduce 3 61)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (282, Token (WHERE _)) -> Just (Shift 277)
    (282, Token (RBRACE _)) -> Just (Reduce 3 63)
    (282, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (283, Token (LBRACE _)) -> Just (Shift 98)
    (284, Token (LBRACE _)) -> Just (Shift 98)
    (285, Token (LBRACE _)) -> Just (Shift 98)
    (286, Token (LBRACE _)) -> Just (Shift 98)
    (287, Token (LBRACE _)) -> Just (Shift 98)
    (288, Token (LBRACE _)) -> Just (Shift 98)
    (289, Token (LBRACE _)) -> Just (Shift 98)
    (290, Token (LBRACE _)) -> Just (Shift 98)
    (291, Token (LBRACE _)) -> Just (Shift 98)
    (292, Token (LBRACE _)) -> Just (Shift 98)
    (293, Token (LBRACE _)) -> Just (Shift 98)
    (294, Token (RBRACE _)) -> Just (Reduce 3 57)
    (294, Token (COMMA _)) -> Just (Reduce 3 57)
    (294, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (294, Token (EQUAL _)) -> Just (Reduce 3 57)
    (294, Token (RARROW _)) -> Just (Reduce 3 57)
    (294, Token (IN _)) -> Just (Reduce 3 57)
    (295, Token (RBRACE _)) -> Just (Shift 294)
    (296, Token (RBRACE _)) -> Just (Reduce 1 58)
    (296, Token (SEMICOLON _)) -> Just (Shift 99)
    (297, Token (RBRACE _)) -> Just (Reduce 3 59)
    (298, Token (RBRACE _)) -> Just (Reduce 5 87)
    (298, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (299, Token (RBRACE _)) -> Just (Reduce 3 86)
    (299, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (300, Token (COLON_COLON _)) -> Just (Shift 159)
    (301, Token (COMMA _)) -> Just (Reduce 0 254)
    (301, Token (MINUS _)) -> Just (Reduce 0 254)
    (301, Token (QCONID _)) -> Just (Reduce 0 254)
    (301, Token (EXPORT _)) -> Just (Reduce 0 254)
    (301, Token (AS _)) -> Just (Reduce 0 254)
    (301, Token (QVARID _)) -> Just (Reduce 0 254)
    (301, Token (QVARSYM _)) -> Just (Reduce 0 254)
    (301, Token (BACKQUOTE _)) -> Just (Reduce 0 254)
    (301, Token (QCONSYM _)) -> Just (Reduce 0 254)
    (301, Token (INTEGER _)) -> Just (Shift 335)
    (302, Token (MINUS _)) -> Just (Shift 338)
    (302, Token (QVARSYM _)) -> Just (Shift 512)
    (302, Token (BACKQUOTE _)) -> Just (Shift 374)
    (302, Token (QCONSYM _)) -> Just (Shift 377)
    (303, Token (RBRACE _)) -> Just (Reduce 3 88)
    (303, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (304, Token (LPAREN _)) -> Just (Reduce 1 224)
    (304, Token (RPAREN _)) -> Just (Reduce 1 224)
    (304, Token (EQUAL _)) -> Just (Reduce 1 224)
    (304, Token (PIPE _)) -> Just (Reduce 1 224)
    (304, Token (MINUS _)) -> Just (Reduce 1 224)
    (304, Token (RARROW _)) -> Just (Reduce 1 224)
    (304, Token (QCONID _)) -> Just (Reduce 1 224)
    (304, Token (EXPORT _)) -> Just (Reduce 1 224)
    (304, Token (AS _)) -> Just (Reduce 1 224)
    (304, Token (QVARID _)) -> Just (Reduce 1 224)
    (304, Token (QVARSYM _)) -> Just (Reduce 1 224)
    (304, Token (BACKQUOTE _)) -> Just (Reduce 1 224)
    (304, Token (QCONSYM _)) -> Just (Reduce 1 224)
    (305, Token (LPAREN _)) -> Just (Reduce 3 226)
    (305, Token (RPAREN _)) -> Just (Reduce 3 226)
    (305, Token (EQUAL _)) -> Just (Reduce 3 226)
    (305, Token (PIPE _)) -> Just (Reduce 3 226)
    (305, Token (MINUS _)) -> Just (Reduce 3 226)
    (305, Token (RARROW _)) -> Just (Reduce 3 226)
    (305, Token (QCONID _)) -> Just (Reduce 3 226)
    (305, Token (EXPORT _)) -> Just (Reduce 3 226)
    (305, Token (AS _)) -> Just (Reduce 3 226)
    (305, Token (QVARID _)) -> Just (Reduce 3 226)
    (305, Token (QVARSYM _)) -> Just (Reduce 3 226)
    (305, Token (BACKQUOTE _)) -> Just (Reduce 3 226)
    (305, Token (QCONSYM _)) -> Just (Reduce 3 226)
    (306, Token (LPAREN _)) -> Just (Reduce 2 225)
    (306, Token (RPAREN _)) -> Just (Reduce 2 225)
    (306, Token (EQUAL _)) -> Just (Reduce 2 225)
    (306, Token (PIPE _)) -> Just (Reduce 2 225)
    (306, Token (MINUS _)) -> Just (Reduce 2 225)
    (306, Token (RARROW _)) -> Just (Reduce 2 225)
    (306, Token (QCONID _)) -> Just (Reduce 2 225)
    (306, Token (EXPORT _)) -> Just (Reduce 2 225)
    (306, Token (AS _)) -> Just (Reduce 2 225)
    (306, Token (QVARID _)) -> Just (Reduce 2 225)
    (306, Token (QVARSYM _)) -> Just (Reduce 2 225)
    (306, Token (BACKQUOTE _)) -> Just (Reduce 2 225)
    (306, Token (QCONSYM _)) -> Just (Reduce 2 225)
    (307, Token (LPAREN _)) -> Just (Reduce 3 227)
    (307, Token (RPAREN _)) -> Just (Reduce 3 227)
    (307, Token (EQUAL _)) -> Just (Reduce 3 227)
    (307, Token (PIPE _)) -> Just (Reduce 3 227)
    (307, Token (MINUS _)) -> Just (Reduce 3 227)
    (307, Token (RARROW _)) -> Just (Reduce 3 227)
    (307, Token (QCONID _)) -> Just (Reduce 3 227)
    (307, Token (EXPORT _)) -> Just (Reduce 3 227)
    (307, Token (AS _)) -> Just (Reduce 3 227)
    (307, Token (QVARID _)) -> Just (Reduce 3 227)
    (307, Token (QVARSYM _)) -> Just (Reduce 3 227)
    (307, Token (BACKQUOTE _)) -> Just (Reduce 3 227)
    (307, Token (QCONSYM _)) -> Just (Reduce 3 227)
    (308, Token (WHERE _)) -> Just (Reduce 1 153)
    (308, Token (RBRACE _)) -> Just (Reduce 1 153)
    (308, Token (RPAREN _)) -> Just (Reduce 1 153)
    (308, Token (COMMA _)) -> Just (Reduce 1 153)
    (308, Token (DOT_DOT _)) -> Just (Reduce 1 153)
    (308, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (308, Token (EQUAL _)) -> Just (Reduce 1 153)
    (308, Token (PIPE _)) -> Just (Reduce 1 153)
    (308, Token (RBRACKET _)) -> Just (Reduce 1 153)
    (308, Token (LARROW _)) -> Just (Reduce 1 153)
    (308, Token (THEN _)) -> Just (Reduce 1 153)
    (308, Token (ELSE _)) -> Just (Reduce 1 153)
    (308, Token (OF _)) -> Just (Reduce 1 153)
    (309, Token (WHERE _)) -> Just (Reduce 3 146)
    (309, Token (RBRACE _)) -> Just (Reduce 3 146)
    (309, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (309, Token (PIPE _)) -> Just (Shift 84)
    (310, Token (WHERE _)) -> Just (Reduce 5 147)
    (310, Token (RBRACE _)) -> Just (Reduce 5 147)
    (310, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (311, Token (EQUAL _)) -> Just (Shift 47)
    (312, Token (RBRACE _)) -> Just (Reduce 3 67)
    (312, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (313, Token (RBRACE _)) -> Just (Shift 312)
    (314, Token (RBRACE _)) -> Just (Reduce 3 69)
    (315, Token (RBRACE _)) -> Just (Reduce 1 68)
    (315, Token (SEMICOLON _)) -> Just (Shift 101)
    (316, Token (RBRACE _)) -> Just (Reduce 5 72)
    (316, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (317, Token (RBRACE _)) -> Just (Reduce 5 74)
    (317, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (318, Token (RBRACE _)) -> Just (Reduce 1 70)
    (318, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (319, Token (WHERE _)) -> Just (Shift 284)
    (319, Token (RBRACE _)) -> Just (Reduce 3 71)
    (319, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (320, Token (WHERE _)) -> Just (Shift 285)
    (320, Token (RBRACE _)) -> Just (Reduce 3 73)
    (320, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (321, Token (RBRACE _)) -> Just (Reduce 3 77)
    (321, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (322, Token (RBRACE _)) -> Just (Shift 321)
    (323, Token (RBRACE _)) -> Just (Reduce 3 79)
    (324, Token (RBRACE _)) -> Just (Reduce 1 78)
    (324, Token (SEMICOLON _)) -> Just (Shift 104)
    (325, Token (RBRACE _)) -> Just (Reduce 5 82)
    (325, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (326, Token (RBRACE _)) -> Just (Reduce 5 84)
    (326, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (327, Token (WHERE _)) -> Just (Shift 286)
    (327, Token (RBRACE _)) -> Just (Reduce 3 81)
    (327, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (328, Token (WHERE _)) -> Just (Shift 287)
    (328, Token (RBRACE _)) -> Just (Reduce 3 83)
    (328, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (329, Token (COMMA _)) -> Just (Shift 119)
    (329, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (330, Token (LPAREN _)) -> Just (Reduce 1 228)
    (330, Token (COMMA _)) -> Just (Shift 119)
    (330, Token (EQUAL _)) -> Just (Reduce 1 228)
    (330, Token (PIPE _)) -> Just (Reduce 1 228)
    (330, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (330, Token (MINUS _)) -> Just (Reduce 1 228)
    (330, Token (QCONID _)) -> Just (Reduce 1 228)
    (330, Token (EXPORT _)) -> Just (Reduce 1 228)
    (330, Token (AS _)) -> Just (Reduce 1 228)
    (330, Token (QVARID _)) -> Just (Reduce 1 228)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 228)
    (330, Token (BACKQUOTE _)) -> Just (Reduce 1 228)
    (330, Token (QCONSYM _)) -> Just (Reduce 1 228)
    (331, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (332, Token (COMMA _)) -> Just (Reduce 1 95)
    (332, Token (MINUS _)) -> Just (Reduce 1 95)
    (332, Token (QCONID _)) -> Just (Reduce 1 95)
    (332, Token (EXPORT _)) -> Just (Reduce 1 95)
    (332, Token (AS _)) -> Just (Reduce 1 95)
    (332, Token (QVARID _)) -> Just (Reduce 1 95)
    (332, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (332, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (332, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (332, Token (INTEGER _)) -> Just (Reduce 1 95)
    (333, Token (COMMA _)) -> Just (Reduce 1 96)
    (333, Token (MINUS _)) -> Just (Reduce 1 96)
    (333, Token (QCONID _)) -> Just (Reduce 1 96)
    (333, Token (EXPORT _)) -> Just (Reduce 1 96)
    (333, Token (AS _)) -> Just (Reduce 1 96)
    (333, Token (QVARID _)) -> Just (Reduce 1 96)
    (333, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (333, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (333, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (333, Token (INTEGER _)) -> Just (Reduce 1 96)
    (334, Token (COMMA _)) -> Just (Reduce 1 97)
    (334, Token (MINUS _)) -> Just (Reduce 1 97)
    (334, Token (QCONID _)) -> Just (Reduce 1 97)
    (334, Token (EXPORT _)) -> Just (Reduce 1 97)
    (334, Token (AS _)) -> Just (Reduce 1 97)
    (334, Token (QVARID _)) -> Just (Reduce 1 97)
    (334, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (334, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (334, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (334, Token (INTEGER _)) -> Just (Reduce 1 97)
    (335, Token (COMMA _)) -> Just (Reduce 1 255)
    (335, Token (MINUS _)) -> Just (Reduce 1 255)
    (335, Token (QCONID _)) -> Just (Reduce 1 255)
    (335, Token (EXPORT _)) -> Just (Reduce 1 255)
    (335, Token (AS _)) -> Just (Reduce 1 255)
    (335, Token (QVARID _)) -> Just (Reduce 1 255)
    (335, Token (QVARSYM _)) -> Just (Reduce 1 255)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 1 255)
    (335, Token (QCONSYM _)) -> Just (Reduce 1 255)
    (336, Token (MINUS _)) -> Just (Shift 338)
    (336, Token (QVARSYM _)) -> Just (Shift 512)
    (336, Token (BACKQUOTE _)) -> Just (Shift 374)
    (336, Token (QCONSYM _)) -> Just (Shift 377)
    (337, Token (MINUS _)) -> Just (Shift 338)
    (337, Token (QVARSYM _)) -> Just (Shift 512)
    (337, Token (BACKQUOTE _)) -> Just (Shift 374)
    (337, Token (QCONSYM _)) -> Just (Shift 377)
    (338, Token (RBRACE _)) -> Just (Reduce 1 89)
    (338, Token (COMMA _)) -> Just (Shift 336)
    (338, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (339, Token (RBRACE _)) -> Just (Reduce 3 91)
    (339, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (340, Token (RBRACE _)) -> Just (Reduce 3 92)
    (340, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (341, Token (RBRACE _)) -> Just (Reduce 1 90)
    (341, Token (COMMA _)) -> Just (Shift 337)
    (341, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (342, Token (RBRACE _)) -> Just (Reduce 1 244)
    (342, Token (LPAREN _)) -> Just (Reduce 1 244)
    (342, Token (COMMA _)) -> Just (Reduce 1 244)
    (342, Token (SEMICOLON _)) -> Just (Reduce 1 244)
    (342, Token (MINUS _)) -> Just (Reduce 1 244)
    (342, Token (QCONID _)) -> Just (Reduce 1 244)
    (342, Token (EXPORT _)) -> Just (Reduce 1 244)
    (342, Token (AS _)) -> Just (Reduce 1 244)
    (342, Token (QVARID _)) -> Just (Reduce 1 244)
    (342, Token (QVARSYM _)) -> Just (Reduce 1 244)
    (342, Token (BACKQUOTE _)) -> Just (Reduce 1 244)
    (342, Token (QCONSYM _)) -> Just (Reduce 1 244)
    (343, Token (RBRACE _)) -> Just (Reduce 1 243)
    (343, Token (LPAREN _)) -> Just (Reduce 1 243)
    (343, Token (COMMA _)) -> Just (Reduce 1 243)
    (343, Token (SEMICOLON _)) -> Just (Reduce 1 243)
    (343, Token (MINUS _)) -> Just (Reduce 1 243)
    (343, Token (QCONID _)) -> Just (Reduce 1 243)
    (343, Token (EXPORT _)) -> Just (Reduce 1 243)
    (343, Token (AS _)) -> Just (Reduce 1 243)
    (343, Token (QVARID _)) -> Just (Reduce 1 243)
    (343, Token (QVARSYM _)) -> Just (Reduce 1 243)
    (343, Token (BACKQUOTE _)) -> Just (Reduce 1 243)
    (343, Token (QCONSYM _)) -> Just (Reduce 1 243)
    (344, Token (WHERE _)) -> Just (Reduce 3 108)
    (344, Token (LBRACE _)) -> Just (Reduce 3 108)
    (344, Token (RBRACE _)) -> Just (Reduce 3 108)
    (344, Token (LPAREN _)) -> Just (Reduce 3 108)
    (344, Token (RPAREN _)) -> Just (Reduce 3 108)
    (344, Token (COMMA _)) -> Just (Reduce 3 108)
    (344, Token (DOT_DOT _)) -> Just (Reduce 3 108)
    (344, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (344, Token (EQUAL _)) -> Just (Reduce 3 108)
    (344, Token (DERIVING _)) -> Just (Reduce 3 108)
    (344, Token (DARROW _)) -> Just (Reduce 3 108)
    (344, Token (PIPE _)) -> Just (Reduce 3 108)
    (344, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (344, Token (MINUS _)) -> Just (Reduce 3 108)
    (344, Token (INFIXL _)) -> Just (Reduce 3 108)
    (344, Token (INFIXR _)) -> Just (Reduce 3 108)
    (344, Token (INFIX _)) -> Just (Reduce 3 108)
    (344, Token (RARROW _)) -> Just (Reduce 3 108)
    (344, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (344, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (344, Token (EXCL _)) -> Just (Reduce 3 108)
    (344, Token (QCONID _)) -> Just (Reduce 3 108)
    (344, Token (EXPORT _)) -> Just (Reduce 3 108)
    (344, Token (AS _)) -> Just (Reduce 3 108)
    (344, Token (QVARID _)) -> Just (Reduce 3 108)
    (344, Token (LARROW _)) -> Just (Reduce 3 108)
    (344, Token (THEN _)) -> Just (Reduce 3 108)
    (344, Token (ELSE _)) -> Just (Reduce 3 108)
    (344, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (344, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (344, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (344, Token (OF _)) -> Just (Reduce 3 108)
    (344, Token (INTEGER _)) -> Just (Reduce 3 108)
    (345, Token (WHERE _)) -> Just (Reduce 3 106)
    (345, Token (LBRACE _)) -> Just (Reduce 3 106)
    (345, Token (RBRACE _)) -> Just (Reduce 3 106)
    (345, Token (LPAREN _)) -> Just (Reduce 3 106)
    (345, Token (RPAREN _)) -> Just (Reduce 3 106)
    (345, Token (COMMA _)) -> Just (Reduce 3 106)
    (345, Token (DOT_DOT _)) -> Just (Reduce 3 106)
    (345, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (345, Token (EQUAL _)) -> Just (Reduce 3 106)
    (345, Token (DERIVING _)) -> Just (Reduce 3 106)
    (345, Token (DARROW _)) -> Just (Reduce 3 106)
    (345, Token (PIPE _)) -> Just (Reduce 3 106)
    (345, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (345, Token (MINUS _)) -> Just (Reduce 3 106)
    (345, Token (INFIXL _)) -> Just (Reduce 3 106)
    (345, Token (INFIXR _)) -> Just (Reduce 3 106)
    (345, Token (INFIX _)) -> Just (Reduce 3 106)
    (345, Token (RARROW _)) -> Just (Reduce 3 106)
    (345, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (345, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (345, Token (EXCL _)) -> Just (Reduce 3 106)
    (345, Token (QCONID _)) -> Just (Reduce 3 106)
    (345, Token (EXPORT _)) -> Just (Reduce 3 106)
    (345, Token (AS _)) -> Just (Reduce 3 106)
    (345, Token (QVARID _)) -> Just (Reduce 3 106)
    (345, Token (LARROW _)) -> Just (Reduce 3 106)
    (345, Token (THEN _)) -> Just (Reduce 3 106)
    (345, Token (ELSE _)) -> Just (Reduce 3 106)
    (345, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (345, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (345, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (345, Token (OF _)) -> Just (Reduce 3 106)
    (345, Token (INTEGER _)) -> Just (Reduce 3 106)
    (346, Token (WHERE _)) -> Just (Reduce 3 107)
    (346, Token (LBRACE _)) -> Just (Reduce 3 107)
    (346, Token (RBRACE _)) -> Just (Reduce 3 107)
    (346, Token (LPAREN _)) -> Just (Reduce 3 107)
    (346, Token (RPAREN _)) -> Just (Reduce 3 107)
    (346, Token (COMMA _)) -> Just (Reduce 3 107)
    (346, Token (DOT_DOT _)) -> Just (Reduce 3 107)
    (346, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (346, Token (EQUAL _)) -> Just (Reduce 3 107)
    (346, Token (DERIVING _)) -> Just (Reduce 3 107)
    (346, Token (DARROW _)) -> Just (Reduce 3 107)
    (346, Token (PIPE _)) -> Just (Reduce 3 107)
    (346, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (346, Token (MINUS _)) -> Just (Reduce 3 107)
    (346, Token (INFIXL _)) -> Just (Reduce 3 107)
    (346, Token (INFIXR _)) -> Just (Reduce 3 107)
    (346, Token (INFIX _)) -> Just (Reduce 3 107)
    (346, Token (RARROW _)) -> Just (Reduce 3 107)
    (346, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (346, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (346, Token (EXCL _)) -> Just (Reduce 3 107)
    (346, Token (QCONID _)) -> Just (Reduce 3 107)
    (346, Token (EXPORT _)) -> Just (Reduce 3 107)
    (346, Token (AS _)) -> Just (Reduce 3 107)
    (346, Token (QVARID _)) -> Just (Reduce 3 107)
    (346, Token (LARROW _)) -> Just (Reduce 3 107)
    (346, Token (THEN _)) -> Just (Reduce 3 107)
    (346, Token (ELSE _)) -> Just (Reduce 3 107)
    (346, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (346, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (346, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (346, Token (OF _)) -> Just (Reduce 3 107)
    (346, Token (INTEGER _)) -> Just (Reduce 3 107)
    (347, Token (RPAREN _)) -> Just (Shift 344)
    (347, Token (COMMA _)) -> Just (Shift 178)
    (348, Token (RBRACKET _)) -> Just (Shift 346)
    (349, Token (WHERE _)) -> Just (Reduce 2 109)
    (349, Token (LBRACE _)) -> Just (Reduce 2 109)
    (349, Token (RBRACE _)) -> Just (Reduce 2 109)
    (349, Token (LPAREN _)) -> Just (Reduce 2 109)
    (349, Token (RPAREN _)) -> Just (Reduce 2 109)
    (349, Token (COMMA _)) -> Just (Reduce 2 109)
    (349, Token (DOT_DOT _)) -> Just (Reduce 2 109)
    (349, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (349, Token (EQUAL _)) -> Just (Reduce 2 109)
    (349, Token (DERIVING _)) -> Just (Reduce 2 109)
    (349, Token (DARROW _)) -> Just (Reduce 2 109)
    (349, Token (PIPE _)) -> Just (Reduce 2 109)
    (349, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (349, Token (MINUS _)) -> Just (Reduce 2 109)
    (349, Token (INFIXL _)) -> Just (Reduce 2 109)
    (349, Token (INFIXR _)) -> Just (Reduce 2 109)
    (349, Token (INFIX _)) -> Just (Reduce 2 109)
    (349, Token (RARROW _)) -> Just (Reduce 2 109)
    (349, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (349, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (349, Token (EXCL _)) -> Just (Reduce 2 109)
    (349, Token (QCONID _)) -> Just (Reduce 2 109)
    (349, Token (EXPORT _)) -> Just (Reduce 2 109)
    (349, Token (AS _)) -> Just (Reduce 2 109)
    (349, Token (QVARID _)) -> Just (Reduce 2 109)
    (349, Token (LARROW _)) -> Just (Reduce 2 109)
    (349, Token (THEN _)) -> Just (Reduce 2 109)
    (349, Token (ELSE _)) -> Just (Reduce 2 109)
    (349, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (349, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (349, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (349, Token (OF _)) -> Just (Reduce 2 109)
    (349, Token (INTEGER _)) -> Just (Reduce 2 109)
    (350, Token (WHERE _)) -> Just (Reduce 1 104)
    (350, Token (LBRACE _)) -> Just (Reduce 1 104)
    (350, Token (RBRACE _)) -> Just (Reduce 1 104)
    (350, Token (LPAREN _)) -> Just (Reduce 1 104)
    (350, Token (RPAREN _)) -> Just (Reduce 1 104)
    (350, Token (COMMA _)) -> Just (Reduce 1 104)
    (350, Token (DOT_DOT _)) -> Just (Reduce 1 104)
    (350, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (350, Token (EQUAL _)) -> Just (Reduce 1 104)
    (350, Token (DERIVING _)) -> Just (Reduce 1 104)
    (350, Token (DARROW _)) -> Just (Reduce 1 104)
    (350, Token (PIPE _)) -> Just (Reduce 1 104)
    (350, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (350, Token (MINUS _)) -> Just (Reduce 1 104)
    (350, Token (INFIXL _)) -> Just (Reduce 1 104)
    (350, Token (INFIXR _)) -> Just (Reduce 1 104)
    (350, Token (INFIX _)) -> Just (Reduce 1 104)
    (350, Token (RARROW _)) -> Just (Reduce 1 104)
    (350, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (350, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (350, Token (EXCL _)) -> Just (Reduce 1 104)
    (350, Token (QCONID _)) -> Just (Reduce 1 104)
    (350, Token (EXPORT _)) -> Just (Reduce 1 104)
    (350, Token (AS _)) -> Just (Reduce 1 104)
    (350, Token (QVARID _)) -> Just (Reduce 1 104)
    (350, Token (LARROW _)) -> Just (Reduce 1 104)
    (350, Token (THEN _)) -> Just (Reduce 1 104)
    (350, Token (ELSE _)) -> Just (Reduce 1 104)
    (350, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (350, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (350, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (350, Token (OF _)) -> Just (Reduce 1 104)
    (350, Token (INTEGER _)) -> Just (Reduce 1 104)
    (351, Token (WHERE _)) -> Just (Reduce 1 105)
    (351, Token (LBRACE _)) -> Just (Reduce 1 105)
    (351, Token (RBRACE _)) -> Just (Reduce 1 105)
    (351, Token (LPAREN _)) -> Just (Reduce 1 105)
    (351, Token (RPAREN _)) -> Just (Reduce 1 105)
    (351, Token (COMMA _)) -> Just (Reduce 1 105)
    (351, Token (DOT_DOT _)) -> Just (Reduce 1 105)
    (351, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (351, Token (EQUAL _)) -> Just (Reduce 1 105)
    (351, Token (DERIVING _)) -> Just (Reduce 1 105)
    (351, Token (DARROW _)) -> Just (Reduce 1 105)
    (351, Token (PIPE _)) -> Just (Reduce 1 105)
    (351, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (351, Token (MINUS _)) -> Just (Reduce 1 105)
    (351, Token (INFIXL _)) -> Just (Reduce 1 105)
    (351, Token (INFIXR _)) -> Just (Reduce 1 105)
    (351, Token (INFIX _)) -> Just (Reduce 1 105)
    (351, Token (RARROW _)) -> Just (Reduce 1 105)
    (351, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (351, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (351, Token (EXCL _)) -> Just (Reduce 1 105)
    (351, Token (QCONID _)) -> Just (Reduce 1 105)
    (351, Token (EXPORT _)) -> Just (Reduce 1 105)
    (351, Token (AS _)) -> Just (Reduce 1 105)
    (351, Token (QVARID _)) -> Just (Reduce 1 105)
    (351, Token (LARROW _)) -> Just (Reduce 1 105)
    (351, Token (THEN _)) -> Just (Reduce 1 105)
    (351, Token (ELSE _)) -> Just (Reduce 1 105)
    (351, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (351, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (351, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (351, Token (OF _)) -> Just (Reduce 1 105)
    (351, Token (INTEGER _)) -> Just (Reduce 1 105)
    (352, Token (RPAREN _)) -> Just (Shift 345)
    (353, Token (WHERE _)) -> Just (Reduce 2 113)
    (353, Token (LBRACE _)) -> Just (Reduce 2 113)
    (353, Token (RBRACE _)) -> Just (Reduce 2 113)
    (353, Token (LPAREN _)) -> Just (Reduce 2 113)
    (353, Token (RPAREN _)) -> Just (Reduce 2 113)
    (353, Token (COMMA _)) -> Just (Reduce 2 113)
    (353, Token (DOT_DOT _)) -> Just (Reduce 2 113)
    (353, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (353, Token (EQUAL _)) -> Just (Reduce 2 113)
    (353, Token (DERIVING _)) -> Just (Reduce 2 113)
    (353, Token (DARROW _)) -> Just (Reduce 2 113)
    (353, Token (PIPE _)) -> Just (Reduce 2 113)
    (353, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (353, Token (MINUS _)) -> Just (Reduce 2 113)
    (353, Token (INFIXL _)) -> Just (Reduce 2 113)
    (353, Token (INFIXR _)) -> Just (Reduce 2 113)
    (353, Token (INFIX _)) -> Just (Reduce 2 113)
    (353, Token (RARROW _)) -> Just (Reduce 2 113)
    (353, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (353, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (353, Token (EXCL _)) -> Just (Reduce 2 113)
    (353, Token (QCONID _)) -> Just (Reduce 2 113)
    (353, Token (EXPORT _)) -> Just (Reduce 2 113)
    (353, Token (AS _)) -> Just (Reduce 2 113)
    (353, Token (QVARID _)) -> Just (Reduce 2 113)
    (353, Token (LARROW _)) -> Just (Reduce 2 113)
    (353, Token (THEN _)) -> Just (Reduce 2 113)
    (353, Token (ELSE _)) -> Just (Reduce 2 113)
    (353, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (353, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (353, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (353, Token (OF _)) -> Just (Reduce 2 113)
    (353, Token (INTEGER _)) -> Just (Reduce 2 113)
    (354, Token (WHERE _)) -> Just (Reduce 3 115)
    (354, Token (LBRACE _)) -> Just (Reduce 3 115)
    (354, Token (RBRACE _)) -> Just (Reduce 3 115)
    (354, Token (LPAREN _)) -> Just (Reduce 3 115)
    (354, Token (RPAREN _)) -> Just (Reduce 3 115)
    (354, Token (COMMA _)) -> Just (Reduce 3 115)
    (354, Token (DOT_DOT _)) -> Just (Reduce 3 115)
    (354, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (354, Token (EQUAL _)) -> Just (Reduce 3 115)
    (354, Token (DERIVING _)) -> Just (Reduce 3 115)
    (354, Token (DARROW _)) -> Just (Reduce 3 115)
    (354, Token (PIPE _)) -> Just (Reduce 3 115)
    (354, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (354, Token (MINUS _)) -> Just (Reduce 3 115)
    (354, Token (INFIXL _)) -> Just (Reduce 3 115)
    (354, Token (INFIXR _)) -> Just (Reduce 3 115)
    (354, Token (INFIX _)) -> Just (Reduce 3 115)
    (354, Token (RARROW _)) -> Just (Reduce 3 115)
    (354, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (354, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (354, Token (EXCL _)) -> Just (Reduce 3 115)
    (354, Token (QCONID _)) -> Just (Reduce 3 115)
    (354, Token (EXPORT _)) -> Just (Reduce 3 115)
    (354, Token (AS _)) -> Just (Reduce 3 115)
    (354, Token (QVARID _)) -> Just (Reduce 3 115)
    (354, Token (LARROW _)) -> Just (Reduce 3 115)
    (354, Token (THEN _)) -> Just (Reduce 3 115)
    (354, Token (ELSE _)) -> Just (Reduce 3 115)
    (354, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (354, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (354, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (354, Token (OF _)) -> Just (Reduce 3 115)
    (354, Token (INTEGER _)) -> Just (Reduce 3 115)
    (355, Token (WHERE _)) -> Just (Reduce 3 116)
    (355, Token (LBRACE _)) -> Just (Reduce 3 116)
    (355, Token (RBRACE _)) -> Just (Reduce 3 116)
    (355, Token (LPAREN _)) -> Just (Reduce 3 116)
    (355, Token (RPAREN _)) -> Just (Reduce 3 116)
    (355, Token (COMMA _)) -> Just (Reduce 3 116)
    (355, Token (DOT_DOT _)) -> Just (Reduce 3 116)
    (355, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (355, Token (EQUAL _)) -> Just (Reduce 3 116)
    (355, Token (DERIVING _)) -> Just (Reduce 3 116)
    (355, Token (DARROW _)) -> Just (Reduce 3 116)
    (355, Token (PIPE _)) -> Just (Reduce 3 116)
    (355, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (355, Token (MINUS _)) -> Just (Reduce 3 116)
    (355, Token (INFIXL _)) -> Just (Reduce 3 116)
    (355, Token (INFIXR _)) -> Just (Reduce 3 116)
    (355, Token (INFIX _)) -> Just (Reduce 3 116)
    (355, Token (RARROW _)) -> Just (Reduce 3 116)
    (355, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (355, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (355, Token (EXCL _)) -> Just (Reduce 3 116)
    (355, Token (QCONID _)) -> Just (Reduce 3 116)
    (355, Token (EXPORT _)) -> Just (Reduce 3 116)
    (355, Token (AS _)) -> Just (Reduce 3 116)
    (355, Token (QVARID _)) -> Just (Reduce 3 116)
    (355, Token (LARROW _)) -> Just (Reduce 3 116)
    (355, Token (THEN _)) -> Just (Reduce 3 116)
    (355, Token (ELSE _)) -> Just (Reduce 3 116)
    (355, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (355, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (355, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (355, Token (OF _)) -> Just (Reduce 3 116)
    (355, Token (INTEGER _)) -> Just (Reduce 3 116)
    (356, Token (RPAREN _)) -> Just (Shift 354)
    (357, Token (WHERE _)) -> Just (Reduce 2 114)
    (357, Token (LBRACE _)) -> Just (Reduce 2 114)
    (357, Token (RBRACE _)) -> Just (Reduce 2 114)
    (357, Token (LPAREN _)) -> Just (Reduce 2 114)
    (357, Token (RPAREN _)) -> Just (Reduce 2 114)
    (357, Token (COMMA _)) -> Just (Reduce 2 114)
    (357, Token (DOT_DOT _)) -> Just (Reduce 2 114)
    (357, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (357, Token (EQUAL _)) -> Just (Reduce 2 114)
    (357, Token (DERIVING _)) -> Just (Reduce 2 114)
    (357, Token (DARROW _)) -> Just (Reduce 2 114)
    (357, Token (PIPE _)) -> Just (Reduce 2 114)
    (357, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (357, Token (MINUS _)) -> Just (Reduce 2 114)
    (357, Token (INFIXL _)) -> Just (Reduce 2 114)
    (357, Token (INFIXR _)) -> Just (Reduce 2 114)
    (357, Token (INFIX _)) -> Just (Reduce 2 114)
    (357, Token (RARROW _)) -> Just (Reduce 2 114)
    (357, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (357, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (357, Token (EXCL _)) -> Just (Reduce 2 114)
    (357, Token (QCONID _)) -> Just (Reduce 2 114)
    (357, Token (EXPORT _)) -> Just (Reduce 2 114)
    (357, Token (AS _)) -> Just (Reduce 2 114)
    (357, Token (QVARID _)) -> Just (Reduce 2 114)
    (357, Token (LARROW _)) -> Just (Reduce 2 114)
    (357, Token (THEN _)) -> Just (Reduce 2 114)
    (357, Token (ELSE _)) -> Just (Reduce 2 114)
    (357, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (357, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (357, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (357, Token (OF _)) -> Just (Reduce 2 114)
    (357, Token (INTEGER _)) -> Just (Reduce 2 114)
    (358, Token (WHERE _)) -> Just (Reduce 1 112)
    (358, Token (LBRACE _)) -> Just (Reduce 1 112)
    (358, Token (RBRACE _)) -> Just (Reduce 1 112)
    (358, Token (LPAREN _)) -> Just (Reduce 1 112)
    (358, Token (RPAREN _)) -> Just (Reduce 1 112)
    (358, Token (COMMA _)) -> Just (Reduce 1 112)
    (358, Token (DOT_DOT _)) -> Just (Reduce 1 112)
    (358, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (358, Token (EQUAL _)) -> Just (Reduce 1 112)
    (358, Token (DERIVING _)) -> Just (Reduce 1 112)
    (358, Token (DARROW _)) -> Just (Reduce 1 112)
    (358, Token (PIPE _)) -> Just (Reduce 1 112)
    (358, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (358, Token (MINUS _)) -> Just (Reduce 1 112)
    (358, Token (INFIXL _)) -> Just (Reduce 1 112)
    (358, Token (INFIXR _)) -> Just (Reduce 1 112)
    (358, Token (INFIX _)) -> Just (Reduce 1 112)
    (358, Token (RARROW _)) -> Just (Reduce 1 112)
    (358, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (358, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (358, Token (EXCL _)) -> Just (Reduce 1 112)
    (358, Token (QCONID _)) -> Just (Reduce 1 112)
    (358, Token (EXPORT _)) -> Just (Reduce 1 112)
    (358, Token (AS _)) -> Just (Reduce 1 112)
    (358, Token (QVARID _)) -> Just (Reduce 1 112)
    (358, Token (LARROW _)) -> Just (Reduce 1 112)
    (358, Token (THEN _)) -> Just (Reduce 1 112)
    (358, Token (ELSE _)) -> Just (Reduce 1 112)
    (358, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (358, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (358, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (358, Token (OF _)) -> Just (Reduce 1 112)
    (358, Token (INTEGER _)) -> Just (Reduce 1 112)
    (359, Token (LBRACE _)) -> Just (Shift 120)
    (359, Token (RBRACE _)) -> Just (Reduce 1 112)
    (359, Token (LPAREN _)) -> Just (Reduce 1 112)
    (359, Token (RPAREN _)) -> Just (Reduce 1 112)
    (359, Token (COMMA _)) -> Just (Reduce 1 112)
    (359, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (359, Token (DERIVING _)) -> Just (Reduce 1 112)
    (359, Token (PIPE _)) -> Just (Reduce 1 112)
    (359, Token (RARROW _)) -> Just (Reduce 1 112)
    (359, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (359, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (359, Token (EXCL _)) -> Just (Reduce 1 112)
    (359, Token (QCONID _)) -> Just (Reduce 1 112)
    (359, Token (EXPORT _)) -> Just (Reduce 1 112)
    (359, Token (AS _)) -> Just (Reduce 1 112)
    (359, Token (QVARID _)) -> Just (Reduce 1 112)
    (359, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (359, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (360, Token (RPAREN _)) -> Just (Shift 355)
    (361, Token (WHERE _)) -> Just (Reduce 1 250)
    (361, Token (LBRACE _)) -> Just (Reduce 1 250)
    (361, Token (RBRACE _)) -> Just (Reduce 1 250)
    (361, Token (LPAREN _)) -> Just (Reduce 1 250)
    (361, Token (RPAREN _)) -> Just (Reduce 1 250)
    (361, Token (COMMA _)) -> Just (Reduce 1 250)
    (361, Token (DOT_DOT _)) -> Just (Reduce 1 250)
    (361, Token (SEMICOLON _)) -> Just (Reduce 1 250)
    (361, Token (EQUAL _)) -> Just (Reduce 1 250)
    (361, Token (DERIVING _)) -> Just (Reduce 1 250)
    (361, Token (DARROW _)) -> Just (Reduce 1 250)
    (361, Token (PIPE _)) -> Just (Reduce 1 250)
    (361, Token (COLON_COLON _)) -> Just (Reduce 1 250)
    (361, Token (MINUS _)) -> Just (Reduce 1 250)
    (361, Token (INFIXL _)) -> Just (Reduce 1 250)
    (361, Token (INFIXR _)) -> Just (Reduce 1 250)
    (361, Token (INFIX _)) -> Just (Reduce 1 250)
    (361, Token (RARROW _)) -> Just (Reduce 1 250)
    (361, Token (LBRACKET _)) -> Just (Reduce 1 250)
    (361, Token (RBRACKET _)) -> Just (Reduce 1 250)
    (361, Token (EXCL _)) -> Just (Reduce 1 250)
    (361, Token (QCONID _)) -> Just (Reduce 1 250)
    (361, Token (EXPORT _)) -> Just (Reduce 1 250)
    (361, Token (AS _)) -> Just (Reduce 1 250)
    (361, Token (QVARID _)) -> Just (Reduce 1 250)
    (361, Token (LARROW _)) -> Just (Reduce 1 250)
    (361, Token (THEN _)) -> Just (Reduce 1 250)
    (361, Token (ELSE _)) -> Just (Reduce 1 250)
    (361, Token (QVARSYM _)) -> Just (Reduce 1 250)
    (361, Token (BACKQUOTE _)) -> Just (Reduce 1 250)
    (361, Token (QCONSYM _)) -> Just (Reduce 1 250)
    (361, Token (OF _)) -> Just (Reduce 1 250)
    (361, Token (INTEGER _)) -> Just (Reduce 1 250)
    (362, Token (WHERE _)) -> Just (Reduce 1 249)
    (362, Token (LBRACE _)) -> Just (Reduce 1 249)
    (362, Token (RBRACE _)) -> Just (Reduce 1 249)
    (362, Token (LPAREN _)) -> Just (Reduce 1 249)
    (362, Token (RPAREN _)) -> Just (Reduce 1 249)
    (362, Token (COMMA _)) -> Just (Reduce 1 249)
    (362, Token (DOT_DOT _)) -> Just (Reduce 1 249)
    (362, Token (SEMICOLON _)) -> Just (Reduce 1 249)
    (362, Token (EQUAL _)) -> Just (Reduce 1 249)
    (362, Token (DERIVING _)) -> Just (Reduce 1 249)
    (362, Token (DARROW _)) -> Just (Reduce 1 249)
    (362, Token (PIPE _)) -> Just (Reduce 1 249)
    (362, Token (COLON_COLON _)) -> Just (Reduce 1 249)
    (362, Token (MINUS _)) -> Just (Reduce 1 249)
    (362, Token (INFIXL _)) -> Just (Reduce 1 249)
    (362, Token (INFIXR _)) -> Just (Reduce 1 249)
    (362, Token (INFIX _)) -> Just (Reduce 1 249)
    (362, Token (RARROW _)) -> Just (Reduce 1 249)
    (362, Token (LBRACKET _)) -> Just (Reduce 1 249)
    (362, Token (RBRACKET _)) -> Just (Reduce 1 249)
    (362, Token (EXCL _)) -> Just (Reduce 1 249)
    (362, Token (QCONID _)) -> Just (Reduce 1 249)
    (362, Token (EXPORT _)) -> Just (Reduce 1 249)
    (362, Token (AS _)) -> Just (Reduce 1 249)
    (362, Token (QVARID _)) -> Just (Reduce 1 249)
    (362, Token (LARROW _)) -> Just (Reduce 1 249)
    (362, Token (THEN _)) -> Just (Reduce 1 249)
    (362, Token (ELSE _)) -> Just (Reduce 1 249)
    (362, Token (QVARSYM _)) -> Just (Reduce 1 249)
    (362, Token (BACKQUOTE _)) -> Just (Reduce 1 249)
    (362, Token (QCONSYM _)) -> Just (Reduce 1 249)
    (362, Token (OF _)) -> Just (Reduce 1 249)
    (362, Token (INTEGER _)) -> Just (Reduce 1 249)
    (363, Token (WHERE _)) -> Just (Reduce 1 251)
    (363, Token (LBRACE _)) -> Just (Reduce 1 251)
    (363, Token (RBRACE _)) -> Just (Reduce 1 251)
    (363, Token (LPAREN _)) -> Just (Reduce 1 251)
    (363, Token (RPAREN _)) -> Just (Reduce 1 251)
    (363, Token (COMMA _)) -> Just (Reduce 1 251)
    (363, Token (DOT_DOT _)) -> Just (Reduce 1 251)
    (363, Token (SEMICOLON _)) -> Just (Reduce 1 251)
    (363, Token (EQUAL _)) -> Just (Reduce 1 251)
    (363, Token (DERIVING _)) -> Just (Reduce 1 251)
    (363, Token (DARROW _)) -> Just (Reduce 1 251)
    (363, Token (PIPE _)) -> Just (Reduce 1 251)
    (363, Token (COLON_COLON _)) -> Just (Reduce 1 251)
    (363, Token (MINUS _)) -> Just (Reduce 1 251)
    (363, Token (INFIXL _)) -> Just (Reduce 1 251)
    (363, Token (INFIXR _)) -> Just (Reduce 1 251)
    (363, Token (INFIX _)) -> Just (Reduce 1 251)
    (363, Token (RARROW _)) -> Just (Reduce 1 251)
    (363, Token (LBRACKET _)) -> Just (Reduce 1 251)
    (363, Token (RBRACKET _)) -> Just (Reduce 1 251)
    (363, Token (EXCL _)) -> Just (Reduce 1 251)
    (363, Token (QCONID _)) -> Just (Reduce 1 251)
    (363, Token (EXPORT _)) -> Just (Reduce 1 251)
    (363, Token (AS _)) -> Just (Reduce 1 251)
    (363, Token (QVARID _)) -> Just (Reduce 1 251)
    (363, Token (LARROW _)) -> Just (Reduce 1 251)
    (363, Token (THEN _)) -> Just (Reduce 1 251)
    (363, Token (ELSE _)) -> Just (Reduce 1 251)
    (363, Token (QVARSYM _)) -> Just (Reduce 1 251)
    (363, Token (BACKQUOTE _)) -> Just (Reduce 1 251)
    (363, Token (QCONSYM _)) -> Just (Reduce 1 251)
    (363, Token (OF _)) -> Just (Reduce 1 251)
    (363, Token (INTEGER _)) -> Just (Reduce 1 251)
    (364, Token (RPAREN _)) -> Just (Reduce 3 110)
    (364, Token (COMMA _)) -> Just (Shift 178)
    (365, Token (RPAREN _)) -> Just (Reduce 3 111)
    (366, Token (RPAREN _)) -> Just (Reduce 1 117)
    (366, Token (COMMA _)) -> Just (Shift 366)
    (367, Token (RPAREN _)) -> Just (Reduce 2 118)
    (368, Token (RBRACE _)) -> Just (Reduce 3 122)
    (368, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (368, Token (DERIVING _)) -> Just (Reduce 3 122)
    (369, Token (RBRACE _)) -> Just (Reduce 1 121)
    (369, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (369, Token (DERIVING _)) -> Just (Reduce 1 121)
    (369, Token (PIPE _)) -> Just (Shift 154)
    (370, Token (RBRACE _)) -> Just (Reduce 3 125)
    (370, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (370, Token (DERIVING _)) -> Just (Reduce 3 125)
    (370, Token (PIPE _)) -> Just (Reduce 3 125)
    (371, Token (RBRACE _)) -> Just (Reduce 4 126)
    (371, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (371, Token (DERIVING _)) -> Just (Reduce 4 126)
    (371, Token (PIPE _)) -> Just (Reduce 4 126)
    (372, Token (RBRACE _)) -> Just (Shift 371)
    (373, Token (BACKQUOTE _)) -> Just (Shift 376)
    (374, Token (QCONID _)) -> Just (Shift 373)
    (374, Token (EXPORT _)) -> Just (Shift 509)
    (374, Token (AS _)) -> Just (Shift 510)
    (374, Token (QVARID _)) -> Just (Shift 511)
    (375, Token (QCONID _)) -> Just (Shift 373)
    (376, Token (RBRACE _)) -> Just (Reduce 3 242)
    (376, Token (LPAREN _)) -> Just (Reduce 3 242)
    (376, Token (RPAREN _)) -> Just (Reduce 3 242)
    (376, Token (COMMA _)) -> Just (Reduce 3 242)
    (376, Token (SEMICOLON _)) -> Just (Reduce 3 242)
    (376, Token (MINUS _)) -> Just (Reduce 3 242)
    (376, Token (RARROW _)) -> Just (Reduce 3 242)
    (376, Token (LBRACKET _)) -> Just (Reduce 3 242)
    (376, Token (RBRACKET _)) -> Just (Reduce 3 242)
    (376, Token (EXCL _)) -> Just (Reduce 3 242)
    (376, Token (QCONID _)) -> Just (Reduce 3 242)
    (376, Token (EXPORT _)) -> Just (Reduce 3 242)
    (376, Token (AS _)) -> Just (Reduce 3 242)
    (376, Token (QVARID _)) -> Just (Reduce 3 242)
    (376, Token (QVARSYM _)) -> Just (Reduce 3 242)
    (376, Token (BACKQUOTE _)) -> Just (Reduce 3 242)
    (376, Token (QCONSYM _)) -> Just (Reduce 3 242)
    (377, Token (RBRACE _)) -> Just (Reduce 1 241)
    (377, Token (LPAREN _)) -> Just (Reduce 1 241)
    (377, Token (RPAREN _)) -> Just (Reduce 1 241)
    (377, Token (COMMA _)) -> Just (Reduce 1 241)
    (377, Token (SEMICOLON _)) -> Just (Reduce 1 241)
    (377, Token (MINUS _)) -> Just (Reduce 1 241)
    (377, Token (RARROW _)) -> Just (Reduce 1 241)
    (377, Token (LBRACKET _)) -> Just (Reduce 1 241)
    (377, Token (RBRACKET _)) -> Just (Reduce 1 241)
    (377, Token (EXCL _)) -> Just (Reduce 1 241)
    (377, Token (QCONID _)) -> Just (Reduce 1 241)
    (377, Token (EXPORT _)) -> Just (Reduce 1 241)
    (377, Token (AS _)) -> Just (Reduce 1 241)
    (377, Token (QVARID _)) -> Just (Reduce 1 241)
    (377, Token (QVARSYM _)) -> Just (Reduce 1 241)
    (377, Token (BACKQUOTE _)) -> Just (Reduce 1 241)
    (377, Token (QCONSYM _)) -> Just (Reduce 1 241)
    (378, Token (RBRACE _)) -> Just (Reduce 3 130)
    (379, Token (RBRACE _)) -> Just (Reduce 1 129)
    (379, Token (COMMA _)) -> Just (Shift 121)
    (380, Token (RBRACE _)) -> Just (Reduce 3 131)
    (380, Token (COMMA _)) -> Just (Reduce 3 131)
    (381, Token (COLON_COLON _)) -> Just (Shift 166)
    (382, Token (EXPORT _)) -> Just (Reduce 1 139)
    (382, Token (AS _)) -> Just (Reduce 1 139)
    (382, Token (QVARID _)) -> Just (Reduce 1 139)
    (382, Token (STRING _)) -> Just (Reduce 1 139)
    (383, Token (EXPORT _)) -> Just (Reduce 1 138)
    (383, Token (AS _)) -> Just (Reduce 1 138)
    (383, Token (QVARID _)) -> Just (Reduce 1 138)
    (383, Token (STRING _)) -> Just (Reduce 1 138)
    (384, Token (EXPORT _)) -> Just (Reduce 1 140)
    (384, Token (AS _)) -> Just (Reduce 1 140)
    (384, Token (QVARID _)) -> Just (Reduce 1 140)
    (384, Token (STRING _)) -> Just (Reduce 1 140)
    (385, Token (LPAREN _)) -> Just (Reduce 1 141)
    (385, Token (MINUS _)) -> Just (Reduce 1 141)
    (385, Token (EXPORT _)) -> Just (Reduce 1 141)
    (385, Token (AS _)) -> Just (Reduce 1 141)
    (385, Token (QVARID _)) -> Just (Reduce 1 141)
    (385, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (386, Token (STRING _)) -> Just (Reduce 1 144)
    (387, Token (STRING _)) -> Just (Reduce 1 143)
    (388, Token (STRING _)) -> Just (Reduce 1 145)
    (389, Token (LPAREN _)) -> Just (Reduce 1 142)
    (389, Token (MINUS _)) -> Just (Reduce 1 142)
    (389, Token (EXPORT _)) -> Just (Reduce 1 142)
    (389, Token (AS _)) -> Just (Reduce 1 142)
    (389, Token (QVARID _)) -> Just (Reduce 1 142)
    (389, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (390, Token (EQUAL _)) -> Just (Reduce 3 149)
    (391, Token (COMMA _)) -> Just (Shift 87)
    (391, Token (EQUAL _)) -> Just (Reduce 1 148)
    (392, Token (COMMA _)) -> Just (Reduce 2 151)
    (392, Token (EQUAL _)) -> Just (Reduce 2 151)
    (392, Token (IN _)) -> Just (Shift 38)
    (393, Token (COMMA _)) -> Just (Reduce 3 150)
    (393, Token (EQUAL _)) -> Just (Reduce 3 150)
    (394, Token (COMMA _)) -> Just (Reduce 1 152)
    (394, Token (EQUAL _)) -> Just (Reduce 1 152)
    (394, Token (LARROW _)) -> Just (Shift 93)
    (395, Token (BACKQUOTE _)) -> Just (Shift 41)
    (396, Token (BACKQUOTE _)) -> Just (Shift 42)
    (397, Token (BACKQUOTE _)) -> Just (Shift 43)
    (398, Token (BACKQUOTE _)) -> Just (Shift 44)
    (399, Token (QCONID _)) -> Just (Shift 395)
    (399, Token (EXPORT _)) -> Just (Shift 396)
    (399, Token (AS _)) -> Just (Shift 397)
    (399, Token (QVARID _)) -> Just (Shift 398)
    (400, Token (WHERE _)) -> Just (Reduce 5 165)
    (400, Token (RBRACE _)) -> Just (Reduce 5 165)
    (400, Token (RPAREN _)) -> Just (Reduce 5 165)
    (400, Token (COMMA _)) -> Just (Reduce 5 165)
    (400, Token (DOT_DOT _)) -> Just (Reduce 5 165)
    (400, Token (SEMICOLON _)) -> Just (Reduce 5 165)
    (400, Token (EQUAL _)) -> Just (Reduce 5 165)
    (400, Token (PIPE _)) -> Just (Reduce 5 165)
    (400, Token (RBRACKET _)) -> Just (Reduce 5 165)
    (400, Token (LARROW _)) -> Just (Reduce 5 165)
    (400, Token (THEN _)) -> Just (Reduce 5 165)
    (400, Token (ELSE _)) -> Just (Reduce 5 165)
    (400, Token (OF _)) -> Just (Reduce 5 165)
    (401, Token (WHERE _)) -> Just (Reduce 3 164)
    (401, Token (RBRACE _)) -> Just (Reduce 3 164)
    (401, Token (RPAREN _)) -> Just (Reduce 3 164)
    (401, Token (COMMA _)) -> Just (Reduce 3 164)
    (401, Token (DOT_DOT _)) -> Just (Reduce 3 164)
    (401, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (401, Token (EQUAL _)) -> Just (Reduce 3 164)
    (401, Token (PIPE _)) -> Just (Reduce 3 164)
    (401, Token (RBRACKET _)) -> Just (Reduce 3 164)
    (401, Token (LARROW _)) -> Just (Reduce 3 164)
    (401, Token (THEN _)) -> Just (Reduce 3 164)
    (401, Token (ELSE _)) -> Just (Reduce 3 164)
    (401, Token (OF _)) -> Just (Reduce 3 164)
    (402, Token (IN _)) -> Just (Shift 38)
    (403, Token (RBRACE _)) -> Just (Reduce 2 223)
    (403, Token (SEMICOLON _)) -> Just (Reduce 2 223)
    (403, Token (IN _)) -> Just (Shift 38)
    (404, Token (WHERE _)) -> Just (Reduce 3 157)
    (404, Token (RBRACE _)) -> Just (Reduce 3 157)
    (404, Token (RPAREN _)) -> Just (Reduce 3 157)
    (404, Token (COMMA _)) -> Just (Reduce 3 157)
    (404, Token (DOT_DOT _)) -> Just (Reduce 3 157)
    (404, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (404, Token (EQUAL _)) -> Just (Reduce 3 157)
    (404, Token (PIPE _)) -> Just (Reduce 3 157)
    (404, Token (RBRACKET _)) -> Just (Reduce 3 157)
    (404, Token (LARROW _)) -> Just (Reduce 3 157)
    (404, Token (THEN _)) -> Just (Reduce 3 157)
    (404, Token (ELSE _)) -> Just (Reduce 3 157)
    (404, Token (OF _)) -> Just (Reduce 3 157)
    (405, Token (WHERE _)) -> Just (Reduce 4 154)
    (405, Token (RBRACE _)) -> Just (Reduce 4 154)
    (405, Token (RPAREN _)) -> Just (Reduce 4 154)
    (405, Token (COMMA _)) -> Just (Reduce 4 154)
    (405, Token (DOT_DOT _)) -> Just (Reduce 4 154)
    (405, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (405, Token (EQUAL _)) -> Just (Reduce 4 154)
    (405, Token (PIPE _)) -> Just (Reduce 4 154)
    (405, Token (RBRACKET _)) -> Just (Reduce 4 154)
    (405, Token (LARROW _)) -> Just (Reduce 4 154)
    (405, Token (THEN _)) -> Just (Reduce 4 154)
    (405, Token (ELSE _)) -> Just (Reduce 4 154)
    (405, Token (OF _)) -> Just (Reduce 4 154)
    (406, Token (WHERE _)) -> Just (Reduce 4 155)
    (406, Token (RBRACE _)) -> Just (Reduce 4 155)
    (406, Token (RPAREN _)) -> Just (Reduce 4 155)
    (406, Token (COMMA _)) -> Just (Reduce 4 155)
    (406, Token (DOT_DOT _)) -> Just (Reduce 4 155)
    (406, Token (SEMICOLON _)) -> Just (Reduce 4 155)
    (406, Token (EQUAL _)) -> Just (Reduce 4 155)
    (406, Token (PIPE _)) -> Just (Reduce 4 155)
    (406, Token (RBRACKET _)) -> Just (Reduce 4 155)
    (406, Token (LARROW _)) -> Just (Reduce 4 155)
    (406, Token (THEN _)) -> Just (Reduce 4 155)
    (406, Token (ELSE _)) -> Just (Reduce 4 155)
    (406, Token (OF _)) -> Just (Reduce 4 155)
    (407, Token (SEMICOLON _)) -> Just (Shift 421)
    (407, Token (THEN _)) -> Just (Reduce 0 256)
    (408, Token (SEMICOLON _)) -> Just (Shift 421)
    (408, Token (ELSE _)) -> Just (Reduce 0 256)
    (409, Token (WHERE _)) -> Just (Reduce 8 156)
    (409, Token (RBRACE _)) -> Just (Reduce 8 156)
    (409, Token (RPAREN _)) -> Just (Reduce 8 156)
    (409, Token (COMMA _)) -> Just (Reduce 8 156)
    (409, Token (DOT_DOT _)) -> Just (Reduce 8 156)
    (409, Token (SEMICOLON _)) -> Just (Reduce 8 156)
    (409, Token (EQUAL _)) -> Just (Reduce 8 156)
    (409, Token (PIPE _)) -> Just (Reduce 8 156)
    (409, Token (RBRACKET _)) -> Just (Reduce 8 156)
    (409, Token (LARROW _)) -> Just (Reduce 8 156)
    (409, Token (THEN _)) -> Just (Reduce 8 156)
    (409, Token (ELSE _)) -> Just (Reduce 8 156)
    (409, Token (OF _)) -> Just (Reduce 8 156)
    (410, Token (WHERE _)) -> Just (Reduce 3 158)
    (410, Token (RBRACE _)) -> Just (Reduce 3 158)
    (410, Token (RPAREN _)) -> Just (Reduce 3 158)
    (410, Token (COMMA _)) -> Just (Reduce 3 158)
    (410, Token (DOT_DOT _)) -> Just (Reduce 3 158)
    (410, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (410, Token (EQUAL _)) -> Just (Reduce 3 158)
    (410, Token (PIPE _)) -> Just (Reduce 3 158)
    (410, Token (RBRACKET _)) -> Just (Reduce 3 158)
    (410, Token (LARROW _)) -> Just (Reduce 3 158)
    (410, Token (THEN _)) -> Just (Reduce 3 158)
    (410, Token (ELSE _)) -> Just (Reduce 3 158)
    (410, Token (OF _)) -> Just (Reduce 3 158)
    (411, Token (WHERE _)) -> Just (Reduce 5 163)
    (411, Token (RBRACE _)) -> Just (Reduce 5 163)
    (411, Token (RPAREN _)) -> Just (Reduce 5 163)
    (411, Token (COMMA _)) -> Just (Reduce 5 163)
    (411, Token (DOT_DOT _)) -> Just (Reduce 5 163)
    (411, Token (SEMICOLON _)) -> Just (Reduce 5 163)
    (411, Token (EQUAL _)) -> Just (Reduce 5 163)
    (411, Token (PIPE _)) -> Just (Reduce 5 163)
    (411, Token (RBRACKET _)) -> Just (Reduce 5 163)
    (411, Token (LARROW _)) -> Just (Reduce 5 163)
    (411, Token (THEN _)) -> Just (Reduce 5 163)
    (411, Token (ELSE _)) -> Just (Reduce 5 163)
    (411, Token (OF _)) -> Just (Reduce 5 163)
    (412, Token (WHERE _)) -> Just (Reduce 5 160)
    (412, Token (RBRACE _)) -> Just (Reduce 5 160)
    (412, Token (RPAREN _)) -> Just (Reduce 5 160)
    (412, Token (COMMA _)) -> Just (Reduce 5 160)
    (412, Token (DOT_DOT _)) -> Just (Reduce 5 160)
    (412, Token (SEMICOLON _)) -> Just (Reduce 5 160)
    (412, Token (EQUAL _)) -> Just (Reduce 5 160)
    (412, Token (PIPE _)) -> Just (Reduce 5 160)
    (412, Token (RBRACKET _)) -> Just (Reduce 5 160)
    (412, Token (LARROW _)) -> Just (Reduce 5 160)
    (412, Token (THEN _)) -> Just (Reduce 5 160)
    (412, Token (ELSE _)) -> Just (Reduce 5 160)
    (412, Token (OF _)) -> Just (Reduce 5 160)
    (413, Token (WHERE _)) -> Just (Reduce 5 159)
    (413, Token (RBRACE _)) -> Just (Reduce 5 159)
    (413, Token (RPAREN _)) -> Just (Reduce 5 159)
    (413, Token (COMMA _)) -> Just (Reduce 5 159)
    (413, Token (DOT_DOT _)) -> Just (Reduce 5 159)
    (413, Token (SEMICOLON _)) -> Just (Reduce 5 159)
    (413, Token (EQUAL _)) -> Just (Reduce 5 159)
    (413, Token (PIPE _)) -> Just (Reduce 5 159)
    (413, Token (RBRACKET _)) -> Just (Reduce 5 159)
    (413, Token (LARROW _)) -> Just (Reduce 5 159)
    (413, Token (THEN _)) -> Just (Reduce 5 159)
    (413, Token (ELSE _)) -> Just (Reduce 5 159)
    (413, Token (OF _)) -> Just (Reduce 5 159)
    (414, Token (WHERE _)) -> Just (Reduce 5 161)
    (414, Token (RBRACE _)) -> Just (Reduce 5 161)
    (414, Token (RPAREN _)) -> Just (Reduce 5 161)
    (414, Token (COMMA _)) -> Just (Reduce 5 161)
    (414, Token (DOT_DOT _)) -> Just (Reduce 5 161)
    (414, Token (SEMICOLON _)) -> Just (Reduce 5 161)
    (414, Token (EQUAL _)) -> Just (Reduce 5 161)
    (414, Token (PIPE _)) -> Just (Reduce 5 161)
    (414, Token (RBRACKET _)) -> Just (Reduce 5 161)
    (414, Token (LARROW _)) -> Just (Reduce 5 161)
    (414, Token (THEN _)) -> Just (Reduce 5 161)
    (414, Token (ELSE _)) -> Just (Reduce 5 161)
    (414, Token (OF _)) -> Just (Reduce 5 161)
    (415, Token (WHERE _)) -> Just (Reduce 3 162)
    (415, Token (RBRACE _)) -> Just (Reduce 3 162)
    (415, Token (RPAREN _)) -> Just (Reduce 3 162)
    (415, Token (COMMA _)) -> Just (Reduce 3 162)
    (415, Token (DOT_DOT _)) -> Just (Reduce 3 162)
    (415, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (415, Token (EQUAL _)) -> Just (Reduce 3 162)
    (415, Token (PIPE _)) -> Just (Reduce 3 162)
    (415, Token (RBRACKET _)) -> Just (Reduce 3 162)
    (415, Token (LARROW _)) -> Just (Reduce 3 162)
    (415, Token (THEN _)) -> Just (Reduce 3 162)
    (415, Token (ELSE _)) -> Just (Reduce 3 162)
    (415, Token (OF _)) -> Just (Reduce 3 162)
    (416, Token (THEN _)) -> Just (Shift 90)
    (417, Token (ELSE _)) -> Just (Shift 39)
    (418, Token (WHERE _)) -> Just (Reduce 1 166)
    (418, Token (RBRACE _)) -> Just (Reduce 1 166)
    (418, Token (RPAREN _)) -> Just (Reduce 1 166)
    (418, Token (COMMA _)) -> Just (Reduce 1 166)
    (418, Token (DOT_DOT _)) -> Just (Reduce 1 166)
    (418, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (418, Token (EQUAL _)) -> Just (Reduce 1 166)
    (418, Token (PIPE _)) -> Just (Reduce 1 166)
    (418, Token (COLON_COLON _)) -> Just (Shift 144)
    (418, Token (MINUS _)) -> Just (Shift 36)
    (418, Token (RBRACKET _)) -> Just (Reduce 1 166)
    (418, Token (LARROW _)) -> Just (Reduce 1 166)
    (418, Token (THEN _)) -> Just (Reduce 1 166)
    (418, Token (ELSE _)) -> Just (Reduce 1 166)
    (418, Token (QVARSYM _)) -> Just (Shift 40)
    (418, Token (BACKQUOTE _)) -> Just (Shift 399)
    (418, Token (QCONSYM _)) -> Just (Shift 45)
    (418, Token (OF _)) -> Just (Reduce 1 166)
    (419, Token (SEMICOLON _)) -> Just (Shift 421)
    (419, Token (THEN _)) -> Just (Reduce 0 256)
    (420, Token (SEMICOLON _)) -> Just (Shift 421)
    (420, Token (ELSE _)) -> Just (Reduce 0 256)
    (421, Token (THEN _)) -> Just (Reduce 1 257)
    (421, Token (ELSE _)) -> Just (Reduce 1 257)
    (422, Token (WHERE _)) -> Just (Reduce 6 179)
    (422, Token (RBRACE _)) -> Just (Reduce 6 179)
    (422, Token (RPAREN _)) -> Just (Reduce 6 179)
    (422, Token (COMMA _)) -> Just (Reduce 6 179)
    (422, Token (DOT_DOT _)) -> Just (Reduce 6 179)
    (422, Token (SEMICOLON _)) -> Just (Reduce 6 179)
    (422, Token (EQUAL _)) -> Just (Reduce 6 179)
    (422, Token (PIPE _)) -> Just (Reduce 6 179)
    (422, Token (COLON_COLON _)) -> Just (Reduce 6 179)
    (422, Token (MINUS _)) -> Just (Reduce 6 179)
    (422, Token (RARROW _)) -> Just (Reduce 6 179)
    (422, Token (RBRACKET _)) -> Just (Reduce 6 179)
    (422, Token (LARROW _)) -> Just (Reduce 6 179)
    (422, Token (THEN _)) -> Just (Reduce 6 179)
    (422, Token (ELSE _)) -> Just (Reduce 6 179)
    (422, Token (QVARSYM _)) -> Just (Reduce 6 179)
    (422, Token (BACKQUOTE _)) -> Just (Reduce 6 179)
    (422, Token (QCONSYM _)) -> Just (Reduce 6 179)
    (422, Token (OF _)) -> Just (Reduce 6 179)
    (423, Token (WHERE _)) -> Just (Reduce 4 180)
    (423, Token (RBRACE _)) -> Just (Reduce 4 180)
    (423, Token (RPAREN _)) -> Just (Reduce 4 180)
    (423, Token (COMMA _)) -> Just (Reduce 4 180)
    (423, Token (DOT_DOT _)) -> Just (Reduce 4 180)
    (423, Token (SEMICOLON _)) -> Just (Reduce 4 180)
    (423, Token (EQUAL _)) -> Just (Reduce 4 180)
    (423, Token (PIPE _)) -> Just (Reduce 4 180)
    (423, Token (COLON_COLON _)) -> Just (Reduce 4 180)
    (423, Token (MINUS _)) -> Just (Reduce 4 180)
    (423, Token (RARROW _)) -> Just (Reduce 4 180)
    (423, Token (RBRACKET _)) -> Just (Reduce 4 180)
    (423, Token (LARROW _)) -> Just (Reduce 4 180)
    (423, Token (THEN _)) -> Just (Reduce 4 180)
    (423, Token (ELSE _)) -> Just (Reduce 4 180)
    (423, Token (QVARSYM _)) -> Just (Reduce 4 180)
    (423, Token (BACKQUOTE _)) -> Just (Reduce 4 180)
    (423, Token (QCONSYM _)) -> Just (Reduce 4 180)
    (423, Token (OF _)) -> Just (Reduce 4 180)
    (424, Token (LBRACE _)) -> Just (Shift 107)
    (425, Token (LBRACE _)) -> Just (Shift 52)
    (426, Token (OF _)) -> Just (Shift 424)
    (427, Token (WHERE _)) -> Just (Reduce 2 178)
    (427, Token (RBRACE _)) -> Just (Reduce 2 178)
    (427, Token (RPAREN _)) -> Just (Reduce 2 178)
    (427, Token (COMMA _)) -> Just (Reduce 2 178)
    (427, Token (DOT_DOT _)) -> Just (Reduce 2 178)
    (427, Token (SEMICOLON _)) -> Just (Reduce 2 178)
    (427, Token (EQUAL _)) -> Just (Reduce 2 178)
    (427, Token (PIPE _)) -> Just (Reduce 2 178)
    (427, Token (COLON_COLON _)) -> Just (Reduce 2 178)
    (427, Token (MINUS _)) -> Just (Reduce 2 178)
    (427, Token (RARROW _)) -> Just (Reduce 2 178)
    (427, Token (RBRACKET _)) -> Just (Reduce 2 178)
    (427, Token (LARROW _)) -> Just (Reduce 2 178)
    (427, Token (THEN _)) -> Just (Reduce 2 178)
    (427, Token (ELSE _)) -> Just (Reduce 2 178)
    (427, Token (QVARSYM _)) -> Just (Reduce 2 178)
    (427, Token (BACKQUOTE _)) -> Just (Reduce 2 178)
    (427, Token (QCONSYM _)) -> Just (Reduce 2 178)
    (427, Token (OF _)) -> Just (Reduce 2 178)
    (428, Token (RBRACE _)) -> Just (Shift 422)
    (429, Token (RBRACE _)) -> Just (Shift 423)
    (430, Token (BACKQUOTE _)) -> Just (Shift 71)
    (431, Token (BACKQUOTE _)) -> Just (Shift 72)
    (432, Token (BACKQUOTE _)) -> Just (Shift 73)
    (433, Token (BACKQUOTE _)) -> Just (Shift 74)
    (434, Token (QCONID _)) -> Just (Shift 430)
    (434, Token (EXPORT _)) -> Just (Shift 431)
    (434, Token (AS _)) -> Just (Shift 432)
    (434, Token (QVARID _)) -> Just (Shift 433)
    (435, Token (IN _)) -> Just (Shift 68)
    (436, Token (COMMA _)) -> Just (Reduce 2 216)
    (436, Token (RARROW _)) -> Just (Reduce 2 216)
    (436, Token (IN _)) -> Just (Shift 68)
    (437, Token (THEN _)) -> Just (Shift 91)
    (438, Token (ELSE _)) -> Just (Shift 69)
    (439, Token (COMMA _)) -> Just (Reduce 1 177)
    (439, Token (MINUS _)) -> Just (Shift 66)
    (439, Token (RARROW _)) -> Just (Reduce 1 177)
    (439, Token (LARROW _)) -> Just (Reduce 1 177)
    (439, Token (QVARSYM _)) -> Just (Shift 70)
    (439, Token (BACKQUOTE _)) -> Just (Shift 434)
    (439, Token (QCONSYM _)) -> Just (Shift 75)
    (440, Token (COMMA _)) -> Just (Reduce 3 170)
    (440, Token (RARROW _)) -> Just (Reduce 3 170)
    (440, Token (LARROW _)) -> Just (Reduce 3 170)
    (441, Token (COMMA _)) -> Just (Reduce 4 167)
    (441, Token (RARROW _)) -> Just (Reduce 4 167)
    (441, Token (LARROW _)) -> Just (Reduce 4 167)
    (442, Token (COMMA _)) -> Just (Reduce 4 168)
    (442, Token (RARROW _)) -> Just (Reduce 4 168)
    (442, Token (LARROW _)) -> Just (Reduce 4 168)
    (443, Token (COMMA _)) -> Just (Reduce 8 169)
    (443, Token (RARROW _)) -> Just (Reduce 8 169)
    (443, Token (LARROW _)) -> Just (Reduce 8 169)
    (444, Token (COMMA _)) -> Just (Reduce 3 171)
    (444, Token (RARROW _)) -> Just (Reduce 3 171)
    (444, Token (LARROW _)) -> Just (Reduce 3 171)
    (445, Token (COMMA _)) -> Just (Reduce 5 176)
    (445, Token (RARROW _)) -> Just (Reduce 5 176)
    (445, Token (LARROW _)) -> Just (Reduce 5 176)
    (446, Token (COMMA _)) -> Just (Reduce 5 173)
    (446, Token (RARROW _)) -> Just (Reduce 5 173)
    (446, Token (LARROW _)) -> Just (Reduce 5 173)
    (447, Token (COMMA _)) -> Just (Reduce 5 172)
    (447, Token (RARROW _)) -> Just (Reduce 5 172)
    (447, Token (LARROW _)) -> Just (Reduce 5 172)
    (448, Token (COMMA _)) -> Just (Reduce 5 174)
    (448, Token (RARROW _)) -> Just (Reduce 5 174)
    (448, Token (LARROW _)) -> Just (Reduce 5 174)
    (449, Token (COMMA _)) -> Just (Reduce 3 175)
    (449, Token (RARROW _)) -> Just (Reduce 3 175)
    (449, Token (LARROW _)) -> Just (Reduce 3 175)
    (450, Token (RBRACE _)) -> Just (Reduce 3 205)
    (451, Token (RBRACE _)) -> Just (Reduce 1 204)
    (451, Token (SEMICOLON _)) -> Just (Shift 108)
    (452, Token (RBRACE _)) -> Just (Reduce 3 219)
    (453, Token (RBRACE _)) -> Just (Reduce 1 218)
    (453, Token (SEMICOLON _)) -> Just (Shift 53)
    (454, Token (WHERE _)) -> Just (Reduce 1 182)
    (454, Token (LBRACE _)) -> Just (Reduce 1 182)
    (454, Token (RBRACE _)) -> Just (Reduce 1 182)
    (454, Token (LPAREN _)) -> Just (Reduce 1 182)
    (454, Token (RPAREN _)) -> Just (Reduce 1 182)
    (454, Token (COMMA _)) -> Just (Reduce 1 182)
    (454, Token (DOT_DOT _)) -> Just (Reduce 1 182)
    (454, Token (SEMICOLON _)) -> Just (Reduce 1 182)
    (454, Token (EQUAL _)) -> Just (Reduce 1 182)
    (454, Token (PIPE _)) -> Just (Reduce 1 182)
    (454, Token (COLON_COLON _)) -> Just (Reduce 1 182)
    (454, Token (MINUS _)) -> Just (Reduce 1 182)
    (454, Token (INFIXL _)) -> Just (Reduce 1 182)
    (454, Token (INFIXR _)) -> Just (Reduce 1 182)
    (454, Token (INFIX _)) -> Just (Reduce 1 182)
    (454, Token (RARROW _)) -> Just (Reduce 1 182)
    (454, Token (LBRACKET _)) -> Just (Reduce 1 182)
    (454, Token (RBRACKET _)) -> Just (Reduce 1 182)
    (454, Token (QCONID _)) -> Just (Reduce 1 182)
    (454, Token (EXPORT _)) -> Just (Reduce 1 182)
    (454, Token (AS _)) -> Just (Reduce 1 182)
    (454, Token (QVARID _)) -> Just (Reduce 1 182)
    (454, Token (STRING _)) -> Just (Reduce 1 182)
    (454, Token (LARROW _)) -> Just (Reduce 1 182)
    (454, Token (LET _)) -> Just (Reduce 1 182)
    (454, Token (LAMBDA _)) -> Just (Reduce 1 182)
    (454, Token (IF _)) -> Just (Reduce 1 182)
    (454, Token (THEN _)) -> Just (Reduce 1 182)
    (454, Token (ELSE _)) -> Just (Reduce 1 182)
    (454, Token (QVARSYM _)) -> Just (Reduce 1 182)
    (454, Token (BACKQUOTE _)) -> Just (Reduce 1 182)
    (454, Token (QCONSYM _)) -> Just (Reduce 1 182)
    (454, Token (CASE _)) -> Just (Reduce 1 182)
    (454, Token (OF _)) -> Just (Reduce 1 182)
    (454, Token (DO _)) -> Just (Reduce 1 182)
    (454, Token (INTEGER _)) -> Just (Reduce 1 182)
    (455, Token (WHERE _)) -> Just (Reduce 2 183)
    (455, Token (LBRACE _)) -> Just (Reduce 2 183)
    (455, Token (RBRACE _)) -> Just (Reduce 2 183)
    (455, Token (LPAREN _)) -> Just (Reduce 2 183)
    (455, Token (RPAREN _)) -> Just (Reduce 2 183)
    (455, Token (COMMA _)) -> Just (Reduce 2 183)
    (455, Token (DOT_DOT _)) -> Just (Reduce 2 183)
    (455, Token (SEMICOLON _)) -> Just (Reduce 2 183)
    (455, Token (EQUAL _)) -> Just (Reduce 2 183)
    (455, Token (PIPE _)) -> Just (Reduce 2 183)
    (455, Token (COLON_COLON _)) -> Just (Reduce 2 183)
    (455, Token (MINUS _)) -> Just (Reduce 2 183)
    (455, Token (INFIXL _)) -> Just (Reduce 2 183)
    (455, Token (INFIXR _)) -> Just (Reduce 2 183)
    (455, Token (INFIX _)) -> Just (Reduce 2 183)
    (455, Token (RARROW _)) -> Just (Reduce 2 183)
    (455, Token (LBRACKET _)) -> Just (Reduce 2 183)
    (455, Token (RBRACKET _)) -> Just (Reduce 2 183)
    (455, Token (QCONID _)) -> Just (Reduce 2 183)
    (455, Token (EXPORT _)) -> Just (Reduce 2 183)
    (455, Token (AS _)) -> Just (Reduce 2 183)
    (455, Token (QVARID _)) -> Just (Reduce 2 183)
    (455, Token (STRING _)) -> Just (Reduce 2 183)
    (455, Token (LARROW _)) -> Just (Reduce 2 183)
    (455, Token (LET _)) -> Just (Reduce 2 183)
    (455, Token (LAMBDA _)) -> Just (Reduce 2 183)
    (455, Token (IF _)) -> Just (Reduce 2 183)
    (455, Token (THEN _)) -> Just (Reduce 2 183)
    (455, Token (ELSE _)) -> Just (Reduce 2 183)
    (455, Token (QVARSYM _)) -> Just (Reduce 2 183)
    (455, Token (BACKQUOTE _)) -> Just (Reduce 2 183)
    (455, Token (QCONSYM _)) -> Just (Reduce 2 183)
    (455, Token (CASE _)) -> Just (Reduce 2 183)
    (455, Token (OF _)) -> Just (Reduce 2 183)
    (455, Token (DO _)) -> Just (Reduce 2 183)
    (455, Token (INTEGER _)) -> Just (Reduce 2 183)
    (456, Token (WHERE _)) -> Just (Reduce 3 191)
    (456, Token (LBRACE _)) -> Just (Reduce 3 191)
    (456, Token (RBRACE _)) -> Just (Reduce 3 191)
    (456, Token (LPAREN _)) -> Just (Reduce 3 191)
    (456, Token (RPAREN _)) -> Just (Reduce 3 191)
    (456, Token (COMMA _)) -> Just (Reduce 3 191)
    (456, Token (DOT_DOT _)) -> Just (Reduce 3 191)
    (456, Token (SEMICOLON _)) -> Just (Reduce 3 191)
    (456, Token (EQUAL _)) -> Just (Reduce 3 191)
    (456, Token (PIPE _)) -> Just (Reduce 3 191)
    (456, Token (COLON_COLON _)) -> Just (Reduce 3 191)
    (456, Token (MINUS _)) -> Just (Reduce 3 191)
    (456, Token (INFIXL _)) -> Just (Reduce 3 191)
    (456, Token (INFIXR _)) -> Just (Reduce 3 191)
    (456, Token (INFIX _)) -> Just (Reduce 3 191)
    (456, Token (RARROW _)) -> Just (Reduce 3 191)
    (456, Token (LBRACKET _)) -> Just (Reduce 3 191)
    (456, Token (RBRACKET _)) -> Just (Reduce 3 191)
    (456, Token (QCONID _)) -> Just (Reduce 3 191)
    (456, Token (EXPORT _)) -> Just (Reduce 3 191)
    (456, Token (AS _)) -> Just (Reduce 3 191)
    (456, Token (QVARID _)) -> Just (Reduce 3 191)
    (456, Token (STRING _)) -> Just (Reduce 3 191)
    (456, Token (LARROW _)) -> Just (Reduce 3 191)
    (456, Token (LET _)) -> Just (Reduce 3 191)
    (456, Token (LAMBDA _)) -> Just (Reduce 3 191)
    (456, Token (IF _)) -> Just (Reduce 3 191)
    (456, Token (THEN _)) -> Just (Reduce 3 191)
    (456, Token (ELSE _)) -> Just (Reduce 3 191)
    (456, Token (QVARSYM _)) -> Just (Reduce 3 191)
    (456, Token (BACKQUOTE _)) -> Just (Reduce 3 191)
    (456, Token (QCONSYM _)) -> Just (Reduce 3 191)
    (456, Token (CASE _)) -> Just (Reduce 3 191)
    (456, Token (OF _)) -> Just (Reduce 3 191)
    (456, Token (DO _)) -> Just (Reduce 3 191)
    (456, Token (INTEGER _)) -> Just (Reduce 3 191)
    (457, Token (WHERE _)) -> Just (Reduce 4 198)
    (457, Token (LBRACE _)) -> Just (Reduce 4 198)
    (457, Token (RBRACE _)) -> Just (Reduce 4 198)
    (457, Token (LPAREN _)) -> Just (Reduce 4 198)
    (457, Token (RPAREN _)) -> Just (Reduce 4 198)
    (457, Token (COMMA _)) -> Just (Reduce 4 198)
    (457, Token (DOT_DOT _)) -> Just (Reduce 4 198)
    (457, Token (SEMICOLON _)) -> Just (Reduce 4 198)
    (457, Token (EQUAL _)) -> Just (Reduce 4 198)
    (457, Token (PIPE _)) -> Just (Reduce 4 198)
    (457, Token (COLON_COLON _)) -> Just (Reduce 4 198)
    (457, Token (MINUS _)) -> Just (Reduce 4 198)
    (457, Token (INFIXL _)) -> Just (Reduce 4 198)
    (457, Token (INFIXR _)) -> Just (Reduce 4 198)
    (457, Token (INFIX _)) -> Just (Reduce 4 198)
    (457, Token (RARROW _)) -> Just (Reduce 4 198)
    (457, Token (LBRACKET _)) -> Just (Reduce 4 198)
    (457, Token (RBRACKET _)) -> Just (Reduce 4 198)
    (457, Token (QCONID _)) -> Just (Reduce 4 198)
    (457, Token (EXPORT _)) -> Just (Reduce 4 198)
    (457, Token (AS _)) -> Just (Reduce 4 198)
    (457, Token (QVARID _)) -> Just (Reduce 4 198)
    (457, Token (STRING _)) -> Just (Reduce 4 198)
    (457, Token (LARROW _)) -> Just (Reduce 4 198)
    (457, Token (LET _)) -> Just (Reduce 4 198)
    (457, Token (LAMBDA _)) -> Just (Reduce 4 198)
    (457, Token (IF _)) -> Just (Reduce 4 198)
    (457, Token (THEN _)) -> Just (Reduce 4 198)
    (457, Token (ELSE _)) -> Just (Reduce 4 198)
    (457, Token (QVARSYM _)) -> Just (Reduce 4 198)
    (457, Token (BACKQUOTE _)) -> Just (Reduce 4 198)
    (457, Token (QCONSYM _)) -> Just (Reduce 4 198)
    (457, Token (CASE _)) -> Just (Reduce 4 198)
    (457, Token (OF _)) -> Just (Reduce 4 198)
    (457, Token (DO _)) -> Just (Reduce 4 198)
    (457, Token (INTEGER _)) -> Just (Reduce 4 198)
    (458, Token (WHERE _)) -> Just (Reduce 6 203)
    (458, Token (LBRACE _)) -> Just (Reduce 6 203)
    (458, Token (RBRACE _)) -> Just (Reduce 6 203)
    (458, Token (LPAREN _)) -> Just (Reduce 6 203)
    (458, Token (RPAREN _)) -> Just (Reduce 6 203)
    (458, Token (COMMA _)) -> Just (Reduce 6 203)
    (458, Token (DOT_DOT _)) -> Just (Reduce 6 203)
    (458, Token (SEMICOLON _)) -> Just (Reduce 6 203)
    (458, Token (EQUAL _)) -> Just (Reduce 6 203)
    (458, Token (PIPE _)) -> Just (Reduce 6 203)
    (458, Token (COLON_COLON _)) -> Just (Reduce 6 203)
    (458, Token (MINUS _)) -> Just (Reduce 6 203)
    (458, Token (INFIXL _)) -> Just (Reduce 6 203)
    (458, Token (INFIXR _)) -> Just (Reduce 6 203)
    (458, Token (INFIX _)) -> Just (Reduce 6 203)
    (458, Token (RARROW _)) -> Just (Reduce 6 203)
    (458, Token (LBRACKET _)) -> Just (Reduce 6 203)
    (458, Token (RBRACKET _)) -> Just (Reduce 6 203)
    (458, Token (QCONID _)) -> Just (Reduce 6 203)
    (458, Token (EXPORT _)) -> Just (Reduce 6 203)
    (458, Token (AS _)) -> Just (Reduce 6 203)
    (458, Token (QVARID _)) -> Just (Reduce 6 203)
    (458, Token (STRING _)) -> Just (Reduce 6 203)
    (458, Token (LARROW _)) -> Just (Reduce 6 203)
    (458, Token (LET _)) -> Just (Reduce 6 203)
    (458, Token (LAMBDA _)) -> Just (Reduce 6 203)
    (458, Token (IF _)) -> Just (Reduce 6 203)
    (458, Token (THEN _)) -> Just (Reduce 6 203)
    (458, Token (ELSE _)) -> Just (Reduce 6 203)
    (458, Token (QVARSYM _)) -> Just (Reduce 6 203)
    (458, Token (BACKQUOTE _)) -> Just (Reduce 6 203)
    (458, Token (QCONSYM _)) -> Just (Reduce 6 203)
    (458, Token (CASE _)) -> Just (Reduce 6 203)
    (458, Token (OF _)) -> Just (Reduce 6 203)
    (458, Token (DO _)) -> Just (Reduce 6 203)
    (458, Token (INTEGER _)) -> Just (Reduce 6 203)
    (459, Token (WHERE _)) -> Just (Reduce 6 200)
    (459, Token (LBRACE _)) -> Just (Reduce 6 200)
    (459, Token (RBRACE _)) -> Just (Reduce 6 200)
    (459, Token (LPAREN _)) -> Just (Reduce 6 200)
    (459, Token (RPAREN _)) -> Just (Reduce 6 200)
    (459, Token (COMMA _)) -> Just (Reduce 6 200)
    (459, Token (DOT_DOT _)) -> Just (Reduce 6 200)
    (459, Token (SEMICOLON _)) -> Just (Reduce 6 200)
    (459, Token (EQUAL _)) -> Just (Reduce 6 200)
    (459, Token (PIPE _)) -> Just (Reduce 6 200)
    (459, Token (COLON_COLON _)) -> Just (Reduce 6 200)
    (459, Token (MINUS _)) -> Just (Reduce 6 200)
    (459, Token (INFIXL _)) -> Just (Reduce 6 200)
    (459, Token (INFIXR _)) -> Just (Reduce 6 200)
    (459, Token (INFIX _)) -> Just (Reduce 6 200)
    (459, Token (RARROW _)) -> Just (Reduce 6 200)
    (459, Token (LBRACKET _)) -> Just (Reduce 6 200)
    (459, Token (RBRACKET _)) -> Just (Reduce 6 200)
    (459, Token (QCONID _)) -> Just (Reduce 6 200)
    (459, Token (EXPORT _)) -> Just (Reduce 6 200)
    (459, Token (AS _)) -> Just (Reduce 6 200)
    (459, Token (QVARID _)) -> Just (Reduce 6 200)
    (459, Token (STRING _)) -> Just (Reduce 6 200)
    (459, Token (LARROW _)) -> Just (Reduce 6 200)
    (459, Token (LET _)) -> Just (Reduce 6 200)
    (459, Token (LAMBDA _)) -> Just (Reduce 6 200)
    (459, Token (IF _)) -> Just (Reduce 6 200)
    (459, Token (THEN _)) -> Just (Reduce 6 200)
    (459, Token (ELSE _)) -> Just (Reduce 6 200)
    (459, Token (QVARSYM _)) -> Just (Reduce 6 200)
    (459, Token (BACKQUOTE _)) -> Just (Reduce 6 200)
    (459, Token (QCONSYM _)) -> Just (Reduce 6 200)
    (459, Token (CASE _)) -> Just (Reduce 6 200)
    (459, Token (OF _)) -> Just (Reduce 6 200)
    (459, Token (DO _)) -> Just (Reduce 6 200)
    (459, Token (INTEGER _)) -> Just (Reduce 6 200)
    (460, Token (WHERE _)) -> Just (Reduce 6 199)
    (460, Token (LBRACE _)) -> Just (Reduce 6 199)
    (460, Token (RBRACE _)) -> Just (Reduce 6 199)
    (460, Token (LPAREN _)) -> Just (Reduce 6 199)
    (460, Token (RPAREN _)) -> Just (Reduce 6 199)
    (460, Token (COMMA _)) -> Just (Reduce 6 199)
    (460, Token (DOT_DOT _)) -> Just (Reduce 6 199)
    (460, Token (SEMICOLON _)) -> Just (Reduce 6 199)
    (460, Token (EQUAL _)) -> Just (Reduce 6 199)
    (460, Token (PIPE _)) -> Just (Reduce 6 199)
    (460, Token (COLON_COLON _)) -> Just (Reduce 6 199)
    (460, Token (MINUS _)) -> Just (Reduce 6 199)
    (460, Token (INFIXL _)) -> Just (Reduce 6 199)
    (460, Token (INFIXR _)) -> Just (Reduce 6 199)
    (460, Token (INFIX _)) -> Just (Reduce 6 199)
    (460, Token (RARROW _)) -> Just (Reduce 6 199)
    (460, Token (LBRACKET _)) -> Just (Reduce 6 199)
    (460, Token (RBRACKET _)) -> Just (Reduce 6 199)
    (460, Token (QCONID _)) -> Just (Reduce 6 199)
    (460, Token (EXPORT _)) -> Just (Reduce 6 199)
    (460, Token (AS _)) -> Just (Reduce 6 199)
    (460, Token (QVARID _)) -> Just (Reduce 6 199)
    (460, Token (STRING _)) -> Just (Reduce 6 199)
    (460, Token (LARROW _)) -> Just (Reduce 6 199)
    (460, Token (LET _)) -> Just (Reduce 6 199)
    (460, Token (LAMBDA _)) -> Just (Reduce 6 199)
    (460, Token (IF _)) -> Just (Reduce 6 199)
    (460, Token (THEN _)) -> Just (Reduce 6 199)
    (460, Token (ELSE _)) -> Just (Reduce 6 199)
    (460, Token (QVARSYM _)) -> Just (Reduce 6 199)
    (460, Token (BACKQUOTE _)) -> Just (Reduce 6 199)
    (460, Token (QCONSYM _)) -> Just (Reduce 6 199)
    (460, Token (CASE _)) -> Just (Reduce 6 199)
    (460, Token (OF _)) -> Just (Reduce 6 199)
    (460, Token (DO _)) -> Just (Reduce 6 199)
    (460, Token (INTEGER _)) -> Just (Reduce 6 199)
    (461, Token (WHERE _)) -> Just (Reduce 6 201)
    (461, Token (LBRACE _)) -> Just (Reduce 6 201)
    (461, Token (RBRACE _)) -> Just (Reduce 6 201)
    (461, Token (LPAREN _)) -> Just (Reduce 6 201)
    (461, Token (RPAREN _)) -> Just (Reduce 6 201)
    (461, Token (COMMA _)) -> Just (Reduce 6 201)
    (461, Token (DOT_DOT _)) -> Just (Reduce 6 201)
    (461, Token (SEMICOLON _)) -> Just (Reduce 6 201)
    (461, Token (EQUAL _)) -> Just (Reduce 6 201)
    (461, Token (PIPE _)) -> Just (Reduce 6 201)
    (461, Token (COLON_COLON _)) -> Just (Reduce 6 201)
    (461, Token (MINUS _)) -> Just (Reduce 6 201)
    (461, Token (INFIXL _)) -> Just (Reduce 6 201)
    (461, Token (INFIXR _)) -> Just (Reduce 6 201)
    (461, Token (INFIX _)) -> Just (Reduce 6 201)
    (461, Token (RARROW _)) -> Just (Reduce 6 201)
    (461, Token (LBRACKET _)) -> Just (Reduce 6 201)
    (461, Token (RBRACKET _)) -> Just (Reduce 6 201)
    (461, Token (QCONID _)) -> Just (Reduce 6 201)
    (461, Token (EXPORT _)) -> Just (Reduce 6 201)
    (461, Token (AS _)) -> Just (Reduce 6 201)
    (461, Token (QVARID _)) -> Just (Reduce 6 201)
    (461, Token (STRING _)) -> Just (Reduce 6 201)
    (461, Token (LARROW _)) -> Just (Reduce 6 201)
    (461, Token (LET _)) -> Just (Reduce 6 201)
    (461, Token (LAMBDA _)) -> Just (Reduce 6 201)
    (461, Token (IF _)) -> Just (Reduce 6 201)
    (461, Token (THEN _)) -> Just (Reduce 6 201)
    (461, Token (ELSE _)) -> Just (Reduce 6 201)
    (461, Token (QVARSYM _)) -> Just (Reduce 6 201)
    (461, Token (BACKQUOTE _)) -> Just (Reduce 6 201)
    (461, Token (QCONSYM _)) -> Just (Reduce 6 201)
    (461, Token (CASE _)) -> Just (Reduce 6 201)
    (461, Token (OF _)) -> Just (Reduce 6 201)
    (461, Token (DO _)) -> Just (Reduce 6 201)
    (461, Token (INTEGER _)) -> Just (Reduce 6 201)
    (462, Token (WHERE _)) -> Just (Reduce 4 202)
    (462, Token (LBRACE _)) -> Just (Reduce 4 202)
    (462, Token (RBRACE _)) -> Just (Reduce 4 202)
    (462, Token (LPAREN _)) -> Just (Reduce 4 202)
    (462, Token (RPAREN _)) -> Just (Reduce 4 202)
    (462, Token (COMMA _)) -> Just (Reduce 4 202)
    (462, Token (DOT_DOT _)) -> Just (Reduce 4 202)
    (462, Token (SEMICOLON _)) -> Just (Reduce 4 202)
    (462, Token (EQUAL _)) -> Just (Reduce 4 202)
    (462, Token (PIPE _)) -> Just (Reduce 4 202)
    (462, Token (COLON_COLON _)) -> Just (Reduce 4 202)
    (462, Token (MINUS _)) -> Just (Reduce 4 202)
    (462, Token (INFIXL _)) -> Just (Reduce 4 202)
    (462, Token (INFIXR _)) -> Just (Reduce 4 202)
    (462, Token (INFIX _)) -> Just (Reduce 4 202)
    (462, Token (RARROW _)) -> Just (Reduce 4 202)
    (462, Token (LBRACKET _)) -> Just (Reduce 4 202)
    (462, Token (RBRACKET _)) -> Just (Reduce 4 202)
    (462, Token (QCONID _)) -> Just (Reduce 4 202)
    (462, Token (EXPORT _)) -> Just (Reduce 4 202)
    (462, Token (AS _)) -> Just (Reduce 4 202)
    (462, Token (QVARID _)) -> Just (Reduce 4 202)
    (462, Token (STRING _)) -> Just (Reduce 4 202)
    (462, Token (LARROW _)) -> Just (Reduce 4 202)
    (462, Token (LET _)) -> Just (Reduce 4 202)
    (462, Token (LAMBDA _)) -> Just (Reduce 4 202)
    (462, Token (IF _)) -> Just (Reduce 4 202)
    (462, Token (THEN _)) -> Just (Reduce 4 202)
    (462, Token (ELSE _)) -> Just (Reduce 4 202)
    (462, Token (QVARSYM _)) -> Just (Reduce 4 202)
    (462, Token (BACKQUOTE _)) -> Just (Reduce 4 202)
    (462, Token (QCONSYM _)) -> Just (Reduce 4 202)
    (462, Token (CASE _)) -> Just (Reduce 4 202)
    (462, Token (OF _)) -> Just (Reduce 4 202)
    (462, Token (DO _)) -> Just (Reduce 4 202)
    (462, Token (INTEGER _)) -> Just (Reduce 4 202)
    (463, Token (WHERE _)) -> Just (Reduce 3 192)
    (463, Token (LBRACE _)) -> Just (Reduce 3 192)
    (463, Token (RBRACE _)) -> Just (Reduce 3 192)
    (463, Token (LPAREN _)) -> Just (Reduce 3 192)
    (463, Token (RPAREN _)) -> Just (Reduce 3 192)
    (463, Token (COMMA _)) -> Just (Reduce 3 192)
    (463, Token (DOT_DOT _)) -> Just (Reduce 3 192)
    (463, Token (SEMICOLON _)) -> Just (Reduce 3 192)
    (463, Token (EQUAL _)) -> Just (Reduce 3 192)
    (463, Token (PIPE _)) -> Just (Reduce 3 192)
    (463, Token (COLON_COLON _)) -> Just (Reduce 3 192)
    (463, Token (MINUS _)) -> Just (Reduce 3 192)
    (463, Token (INFIXL _)) -> Just (Reduce 3 192)
    (463, Token (INFIXR _)) -> Just (Reduce 3 192)
    (463, Token (INFIX _)) -> Just (Reduce 3 192)
    (463, Token (RARROW _)) -> Just (Reduce 3 192)
    (463, Token (LBRACKET _)) -> Just (Reduce 3 192)
    (463, Token (RBRACKET _)) -> Just (Reduce 3 192)
    (463, Token (QCONID _)) -> Just (Reduce 3 192)
    (463, Token (EXPORT _)) -> Just (Reduce 3 192)
    (463, Token (AS _)) -> Just (Reduce 3 192)
    (463, Token (QVARID _)) -> Just (Reduce 3 192)
    (463, Token (STRING _)) -> Just (Reduce 3 192)
    (463, Token (LARROW _)) -> Just (Reduce 3 192)
    (463, Token (LET _)) -> Just (Reduce 3 192)
    (463, Token (LAMBDA _)) -> Just (Reduce 3 192)
    (463, Token (IF _)) -> Just (Reduce 3 192)
    (463, Token (THEN _)) -> Just (Reduce 3 192)
    (463, Token (ELSE _)) -> Just (Reduce 3 192)
    (463, Token (QVARSYM _)) -> Just (Reduce 3 192)
    (463, Token (BACKQUOTE _)) -> Just (Reduce 3 192)
    (463, Token (QCONSYM _)) -> Just (Reduce 3 192)
    (463, Token (CASE _)) -> Just (Reduce 3 192)
    (463, Token (OF _)) -> Just (Reduce 3 192)
    (463, Token (DO _)) -> Just (Reduce 3 192)
    (463, Token (INTEGER _)) -> Just (Reduce 3 192)
    (464, Token (WHERE _)) -> Just (Reduce 6 196)
    (464, Token (LBRACE _)) -> Just (Reduce 6 196)
    (464, Token (RBRACE _)) -> Just (Reduce 6 196)
    (464, Token (LPAREN _)) -> Just (Reduce 6 196)
    (464, Token (RPAREN _)) -> Just (Reduce 6 196)
    (464, Token (COMMA _)) -> Just (Reduce 6 196)
    (464, Token (DOT_DOT _)) -> Just (Reduce 6 196)
    (464, Token (SEMICOLON _)) -> Just (Reduce 6 196)
    (464, Token (EQUAL _)) -> Just (Reduce 6 196)
    (464, Token (PIPE _)) -> Just (Reduce 6 196)
    (464, Token (COLON_COLON _)) -> Just (Reduce 6 196)
    (464, Token (MINUS _)) -> Just (Reduce 6 196)
    (464, Token (INFIXL _)) -> Just (Reduce 6 196)
    (464, Token (INFIXR _)) -> Just (Reduce 6 196)
    (464, Token (INFIX _)) -> Just (Reduce 6 196)
    (464, Token (RARROW _)) -> Just (Reduce 6 196)
    (464, Token (LBRACKET _)) -> Just (Reduce 6 196)
    (464, Token (RBRACKET _)) -> Just (Reduce 6 196)
    (464, Token (QCONID _)) -> Just (Reduce 6 196)
    (464, Token (EXPORT _)) -> Just (Reduce 6 196)
    (464, Token (AS _)) -> Just (Reduce 6 196)
    (464, Token (QVARID _)) -> Just (Reduce 6 196)
    (464, Token (STRING _)) -> Just (Reduce 6 196)
    (464, Token (LARROW _)) -> Just (Reduce 6 196)
    (464, Token (LET _)) -> Just (Reduce 6 196)
    (464, Token (LAMBDA _)) -> Just (Reduce 6 196)
    (464, Token (IF _)) -> Just (Reduce 6 196)
    (464, Token (THEN _)) -> Just (Reduce 6 196)
    (464, Token (ELSE _)) -> Just (Reduce 6 196)
    (464, Token (QVARSYM _)) -> Just (Reduce 6 196)
    (464, Token (BACKQUOTE _)) -> Just (Reduce 6 196)
    (464, Token (QCONSYM _)) -> Just (Reduce 6 196)
    (464, Token (CASE _)) -> Just (Reduce 6 196)
    (464, Token (OF _)) -> Just (Reduce 6 196)
    (464, Token (DO _)) -> Just (Reduce 6 196)
    (464, Token (INTEGER _)) -> Just (Reduce 6 196)
    (465, Token (WHERE _)) -> Just (Reduce 4 194)
    (465, Token (LBRACE _)) -> Just (Reduce 4 194)
    (465, Token (RBRACE _)) -> Just (Reduce 4 194)
    (465, Token (LPAREN _)) -> Just (Reduce 4 194)
    (465, Token (RPAREN _)) -> Just (Reduce 4 194)
    (465, Token (COMMA _)) -> Just (Reduce 4 194)
    (465, Token (DOT_DOT _)) -> Just (Reduce 4 194)
    (465, Token (SEMICOLON _)) -> Just (Reduce 4 194)
    (465, Token (EQUAL _)) -> Just (Reduce 4 194)
    (465, Token (PIPE _)) -> Just (Reduce 4 194)
    (465, Token (COLON_COLON _)) -> Just (Reduce 4 194)
    (465, Token (MINUS _)) -> Just (Reduce 4 194)
    (465, Token (INFIXL _)) -> Just (Reduce 4 194)
    (465, Token (INFIXR _)) -> Just (Reduce 4 194)
    (465, Token (INFIX _)) -> Just (Reduce 4 194)
    (465, Token (RARROW _)) -> Just (Reduce 4 194)
    (465, Token (LBRACKET _)) -> Just (Reduce 4 194)
    (465, Token (RBRACKET _)) -> Just (Reduce 4 194)
    (465, Token (QCONID _)) -> Just (Reduce 4 194)
    (465, Token (EXPORT _)) -> Just (Reduce 4 194)
    (465, Token (AS _)) -> Just (Reduce 4 194)
    (465, Token (QVARID _)) -> Just (Reduce 4 194)
    (465, Token (STRING _)) -> Just (Reduce 4 194)
    (465, Token (LARROW _)) -> Just (Reduce 4 194)
    (465, Token (LET _)) -> Just (Reduce 4 194)
    (465, Token (LAMBDA _)) -> Just (Reduce 4 194)
    (465, Token (IF _)) -> Just (Reduce 4 194)
    (465, Token (THEN _)) -> Just (Reduce 4 194)
    (465, Token (ELSE _)) -> Just (Reduce 4 194)
    (465, Token (QVARSYM _)) -> Just (Reduce 4 194)
    (465, Token (BACKQUOTE _)) -> Just (Reduce 4 194)
    (465, Token (QCONSYM _)) -> Just (Reduce 4 194)
    (465, Token (CASE _)) -> Just (Reduce 4 194)
    (465, Token (OF _)) -> Just (Reduce 4 194)
    (465, Token (DO _)) -> Just (Reduce 4 194)
    (465, Token (INTEGER _)) -> Just (Reduce 4 194)
    (466, Token (WHERE _)) -> Just (Reduce 7 197)
    (466, Token (LBRACE _)) -> Just (Reduce 7 197)
    (466, Token (RBRACE _)) -> Just (Reduce 7 197)
    (466, Token (LPAREN _)) -> Just (Reduce 7 197)
    (466, Token (RPAREN _)) -> Just (Reduce 7 197)
    (466, Token (COMMA _)) -> Just (Reduce 7 197)
    (466, Token (DOT_DOT _)) -> Just (Reduce 7 197)
    (466, Token (SEMICOLON _)) -> Just (Reduce 7 197)
    (466, Token (EQUAL _)) -> Just (Reduce 7 197)
    (466, Token (PIPE _)) -> Just (Reduce 7 197)
    (466, Token (COLON_COLON _)) -> Just (Reduce 7 197)
    (466, Token (MINUS _)) -> Just (Reduce 7 197)
    (466, Token (INFIXL _)) -> Just (Reduce 7 197)
    (466, Token (INFIXR _)) -> Just (Reduce 7 197)
    (466, Token (INFIX _)) -> Just (Reduce 7 197)
    (466, Token (RARROW _)) -> Just (Reduce 7 197)
    (466, Token (LBRACKET _)) -> Just (Reduce 7 197)
    (466, Token (RBRACKET _)) -> Just (Reduce 7 197)
    (466, Token (QCONID _)) -> Just (Reduce 7 197)
    (466, Token (EXPORT _)) -> Just (Reduce 7 197)
    (466, Token (AS _)) -> Just (Reduce 7 197)
    (466, Token (QVARID _)) -> Just (Reduce 7 197)
    (466, Token (STRING _)) -> Just (Reduce 7 197)
    (466, Token (LARROW _)) -> Just (Reduce 7 197)
    (466, Token (LET _)) -> Just (Reduce 7 197)
    (466, Token (LAMBDA _)) -> Just (Reduce 7 197)
    (466, Token (IF _)) -> Just (Reduce 7 197)
    (466, Token (THEN _)) -> Just (Reduce 7 197)
    (466, Token (ELSE _)) -> Just (Reduce 7 197)
    (466, Token (QVARSYM _)) -> Just (Reduce 7 197)
    (466, Token (BACKQUOTE _)) -> Just (Reduce 7 197)
    (466, Token (QCONSYM _)) -> Just (Reduce 7 197)
    (466, Token (CASE _)) -> Just (Reduce 7 197)
    (466, Token (OF _)) -> Just (Reduce 7 197)
    (466, Token (DO _)) -> Just (Reduce 7 197)
    (466, Token (INTEGER _)) -> Just (Reduce 7 197)
    (467, Token (WHERE _)) -> Just (Reduce 5 195)
    (467, Token (LBRACE _)) -> Just (Reduce 5 195)
    (467, Token (RBRACE _)) -> Just (Reduce 5 195)
    (467, Token (LPAREN _)) -> Just (Reduce 5 195)
    (467, Token (RPAREN _)) -> Just (Reduce 5 195)
    (467, Token (COMMA _)) -> Just (Reduce 5 195)
    (467, Token (DOT_DOT _)) -> Just (Reduce 5 195)
    (467, Token (SEMICOLON _)) -> Just (Reduce 5 195)
    (467, Token (EQUAL _)) -> Just (Reduce 5 195)
    (467, Token (PIPE _)) -> Just (Reduce 5 195)
    (467, Token (COLON_COLON _)) -> Just (Reduce 5 195)
    (467, Token (MINUS _)) -> Just (Reduce 5 195)
    (467, Token (INFIXL _)) -> Just (Reduce 5 195)
    (467, Token (INFIXR _)) -> Just (Reduce 5 195)
    (467, Token (INFIX _)) -> Just (Reduce 5 195)
    (467, Token (RARROW _)) -> Just (Reduce 5 195)
    (467, Token (LBRACKET _)) -> Just (Reduce 5 195)
    (467, Token (RBRACKET _)) -> Just (Reduce 5 195)
    (467, Token (QCONID _)) -> Just (Reduce 5 195)
    (467, Token (EXPORT _)) -> Just (Reduce 5 195)
    (467, Token (AS _)) -> Just (Reduce 5 195)
    (467, Token (QVARID _)) -> Just (Reduce 5 195)
    (467, Token (STRING _)) -> Just (Reduce 5 195)
    (467, Token (LARROW _)) -> Just (Reduce 5 195)
    (467, Token (LET _)) -> Just (Reduce 5 195)
    (467, Token (LAMBDA _)) -> Just (Reduce 5 195)
    (467, Token (IF _)) -> Just (Reduce 5 195)
    (467, Token (THEN _)) -> Just (Reduce 5 195)
    (467, Token (ELSE _)) -> Just (Reduce 5 195)
    (467, Token (QVARSYM _)) -> Just (Reduce 5 195)
    (467, Token (BACKQUOTE _)) -> Just (Reduce 5 195)
    (467, Token (QCONSYM _)) -> Just (Reduce 5 195)
    (467, Token (CASE _)) -> Just (Reduce 5 195)
    (467, Token (OF _)) -> Just (Reduce 5 195)
    (467, Token (DO _)) -> Just (Reduce 5 195)
    (467, Token (INTEGER _)) -> Just (Reduce 5 195)
    (468, Token (WHERE _)) -> Just (Reduce 3 193)
    (468, Token (LBRACE _)) -> Just (Reduce 3 193)
    (468, Token (RBRACE _)) -> Just (Reduce 3 193)
    (468, Token (LPAREN _)) -> Just (Reduce 3 193)
    (468, Token (RPAREN _)) -> Just (Reduce 3 193)
    (468, Token (COMMA _)) -> Just (Reduce 3 193)
    (468, Token (DOT_DOT _)) -> Just (Reduce 3 193)
    (468, Token (SEMICOLON _)) -> Just (Reduce 3 193)
    (468, Token (EQUAL _)) -> Just (Reduce 3 193)
    (468, Token (PIPE _)) -> Just (Reduce 3 193)
    (468, Token (COLON_COLON _)) -> Just (Reduce 3 193)
    (468, Token (MINUS _)) -> Just (Reduce 3 193)
    (468, Token (INFIXL _)) -> Just (Reduce 3 193)
    (468, Token (INFIXR _)) -> Just (Reduce 3 193)
    (468, Token (INFIX _)) -> Just (Reduce 3 193)
    (468, Token (RARROW _)) -> Just (Reduce 3 193)
    (468, Token (LBRACKET _)) -> Just (Reduce 3 193)
    (468, Token (RBRACKET _)) -> Just (Reduce 3 193)
    (468, Token (QCONID _)) -> Just (Reduce 3 193)
    (468, Token (EXPORT _)) -> Just (Reduce 3 193)
    (468, Token (AS _)) -> Just (Reduce 3 193)
    (468, Token (QVARID _)) -> Just (Reduce 3 193)
    (468, Token (STRING _)) -> Just (Reduce 3 193)
    (468, Token (LARROW _)) -> Just (Reduce 3 193)
    (468, Token (LET _)) -> Just (Reduce 3 193)
    (468, Token (LAMBDA _)) -> Just (Reduce 3 193)
    (468, Token (IF _)) -> Just (Reduce 3 193)
    (468, Token (THEN _)) -> Just (Reduce 3 193)
    (468, Token (ELSE _)) -> Just (Reduce 3 193)
    (468, Token (QVARSYM _)) -> Just (Reduce 3 193)
    (468, Token (BACKQUOTE _)) -> Just (Reduce 3 193)
    (468, Token (QCONSYM _)) -> Just (Reduce 3 193)
    (468, Token (CASE _)) -> Just (Reduce 3 193)
    (468, Token (OF _)) -> Just (Reduce 3 193)
    (468, Token (DO _)) -> Just (Reduce 3 193)
    (468, Token (INTEGER _)) -> Just (Reduce 3 193)
    (469, Token (BACKQUOTE _)) -> Just (Shift 59)
    (470, Token (BACKQUOTE _)) -> Just (Shift 60)
    (471, Token (BACKQUOTE _)) -> Just (Shift 61)
    (472, Token (BACKQUOTE _)) -> Just (Shift 62)
    (473, Token (WHERE _)) -> Just (Reduce 1 190)
    (473, Token (LBRACE _)) -> Just (Reduce 1 190)
    (473, Token (RBRACE _)) -> Just (Reduce 1 190)
    (473, Token (LPAREN _)) -> Just (Reduce 1 190)
    (473, Token (RPAREN _)) -> Just (Reduce 1 190)
    (473, Token (COMMA _)) -> Just (Reduce 1 190)
    (473, Token (DOT_DOT _)) -> Just (Reduce 1 190)
    (473, Token (SEMICOLON _)) -> Just (Reduce 1 190)
    (473, Token (EQUAL _)) -> Just (Reduce 1 190)
    (473, Token (PIPE _)) -> Just (Reduce 1 190)
    (473, Token (COLON_COLON _)) -> Just (Reduce 1 190)
    (473, Token (MINUS _)) -> Just (Reduce 1 190)
    (473, Token (INFIXL _)) -> Just (Reduce 1 190)
    (473, Token (INFIXR _)) -> Just (Reduce 1 190)
    (473, Token (INFIX _)) -> Just (Reduce 1 190)
    (473, Token (RARROW _)) -> Just (Reduce 1 190)
    (473, Token (LBRACKET _)) -> Just (Reduce 1 190)
    (473, Token (RBRACKET _)) -> Just (Reduce 1 190)
    (473, Token (QCONID _)) -> Just (Reduce 1 190)
    (473, Token (EXPORT _)) -> Just (Reduce 1 190)
    (473, Token (AS _)) -> Just (Reduce 1 190)
    (473, Token (QVARID _)) -> Just (Reduce 1 190)
    (473, Token (STRING _)) -> Just (Reduce 1 190)
    (473, Token (LARROW _)) -> Just (Reduce 1 190)
    (473, Token (LET _)) -> Just (Reduce 1 190)
    (473, Token (LAMBDA _)) -> Just (Reduce 1 190)
    (473, Token (IF _)) -> Just (Reduce 1 190)
    (473, Token (THEN _)) -> Just (Reduce 1 190)
    (473, Token (ELSE _)) -> Just (Reduce 1 190)
    (473, Token (QVARSYM _)) -> Just (Reduce 1 190)
    (473, Token (BACKQUOTE _)) -> Just (Reduce 1 190)
    (473, Token (QCONSYM _)) -> Just (Reduce 1 190)
    (473, Token (CASE _)) -> Just (Reduce 1 190)
    (473, Token (OF _)) -> Just (Reduce 1 190)
    (473, Token (DO _)) -> Just (Reduce 1 190)
    (473, Token (INTEGER _)) -> Just (Reduce 1 190)
    (474, Token (QCONID _)) -> Just (Shift 469)
    (474, Token (EXPORT _)) -> Just (Shift 470)
    (474, Token (AS _)) -> Just (Shift 471)
    (474, Token (QVARID _)) -> Just (Shift 472)
    (475, Token (WHERE _)) -> Just (Reduce 1 189)
    (475, Token (LBRACE _)) -> Just (Reduce 1 189)
    (475, Token (RBRACE _)) -> Just (Reduce 1 189)
    (475, Token (LPAREN _)) -> Just (Reduce 1 189)
    (475, Token (RPAREN _)) -> Just (Reduce 1 189)
    (475, Token (COMMA _)) -> Just (Reduce 1 189)
    (475, Token (DOT_DOT _)) -> Just (Reduce 1 189)
    (475, Token (SEMICOLON _)) -> Just (Reduce 1 189)
    (475, Token (EQUAL _)) -> Just (Reduce 1 189)
    (475, Token (PIPE _)) -> Just (Reduce 1 189)
    (475, Token (COLON_COLON _)) -> Just (Reduce 1 189)
    (475, Token (MINUS _)) -> Just (Reduce 1 189)
    (475, Token (INFIXL _)) -> Just (Reduce 1 189)
    (475, Token (INFIXR _)) -> Just (Reduce 1 189)
    (475, Token (INFIX _)) -> Just (Reduce 1 189)
    (475, Token (RARROW _)) -> Just (Reduce 1 189)
    (475, Token (LBRACKET _)) -> Just (Reduce 1 189)
    (475, Token (RBRACKET _)) -> Just (Reduce 1 189)
    (475, Token (QCONID _)) -> Just (Reduce 1 189)
    (475, Token (EXPORT _)) -> Just (Reduce 1 189)
    (475, Token (AS _)) -> Just (Reduce 1 189)
    (475, Token (QVARID _)) -> Just (Reduce 1 189)
    (475, Token (STRING _)) -> Just (Reduce 1 189)
    (475, Token (LARROW _)) -> Just (Reduce 1 189)
    (475, Token (LET _)) -> Just (Reduce 1 189)
    (475, Token (LAMBDA _)) -> Just (Reduce 1 189)
    (475, Token (IF _)) -> Just (Reduce 1 189)
    (475, Token (THEN _)) -> Just (Reduce 1 189)
    (475, Token (ELSE _)) -> Just (Reduce 1 189)
    (475, Token (QVARSYM _)) -> Just (Reduce 1 189)
    (475, Token (BACKQUOTE _)) -> Just (Reduce 1 189)
    (475, Token (QCONSYM _)) -> Just (Reduce 1 189)
    (475, Token (CASE _)) -> Just (Reduce 1 189)
    (475, Token (OF _)) -> Just (Reduce 1 189)
    (475, Token (DO _)) -> Just (Reduce 1 189)
    (475, Token (INTEGER _)) -> Just (Reduce 1 189)
    (476, Token (WHERE _)) -> Just (Reduce 1 188)
    (476, Token (LBRACE _)) -> Just (Reduce 1 188)
    (476, Token (RBRACE _)) -> Just (Reduce 1 188)
    (476, Token (LPAREN _)) -> Just (Reduce 1 188)
    (476, Token (RPAREN _)) -> Just (Reduce 1 188)
    (476, Token (COMMA _)) -> Just (Reduce 1 188)
    (476, Token (DOT_DOT _)) -> Just (Reduce 1 188)
    (476, Token (SEMICOLON _)) -> Just (Reduce 1 188)
    (476, Token (EQUAL _)) -> Just (Reduce 1 188)
    (476, Token (PIPE _)) -> Just (Reduce 1 188)
    (476, Token (COLON_COLON _)) -> Just (Reduce 1 188)
    (476, Token (MINUS _)) -> Just (Reduce 1 188)
    (476, Token (INFIXL _)) -> Just (Reduce 1 188)
    (476, Token (INFIXR _)) -> Just (Reduce 1 188)
    (476, Token (INFIX _)) -> Just (Reduce 1 188)
    (476, Token (RARROW _)) -> Just (Reduce 1 188)
    (476, Token (LBRACKET _)) -> Just (Reduce 1 188)
    (476, Token (RBRACKET _)) -> Just (Reduce 1 188)
    (476, Token (QCONID _)) -> Just (Reduce 1 188)
    (476, Token (EXPORT _)) -> Just (Reduce 1 188)
    (476, Token (AS _)) -> Just (Reduce 1 188)
    (476, Token (QVARID _)) -> Just (Reduce 1 188)
    (476, Token (STRING _)) -> Just (Reduce 1 188)
    (476, Token (LARROW _)) -> Just (Reduce 1 188)
    (476, Token (LET _)) -> Just (Reduce 1 188)
    (476, Token (LAMBDA _)) -> Just (Reduce 1 188)
    (476, Token (IF _)) -> Just (Reduce 1 188)
    (476, Token (THEN _)) -> Just (Reduce 1 188)
    (476, Token (ELSE _)) -> Just (Reduce 1 188)
    (476, Token (QVARSYM _)) -> Just (Reduce 1 188)
    (476, Token (BACKQUOTE _)) -> Just (Reduce 1 188)
    (476, Token (QCONSYM _)) -> Just (Reduce 1 188)
    (476, Token (CASE _)) -> Just (Reduce 1 188)
    (476, Token (OF _)) -> Just (Reduce 1 188)
    (476, Token (DO _)) -> Just (Reduce 1 188)
    (476, Token (INTEGER _)) -> Just (Reduce 1 188)
    (477, Token (RPAREN _)) -> Just (Shift 456)
    (477, Token (COMMA _)) -> Just (Shift 58)
    (478, Token (COMMA _)) -> Just (Shift 80)
    (478, Token (DOT_DOT _)) -> Just (Shift 81)
    (478, Token (RBRACKET _)) -> Just (Reduce 1 184)
    (479, Token (RBRACKET _)) -> Just (Shift 466)
    (480, Token (RBRACKET _)) -> Just (Shift 467)
    (481, Token (COMMA _)) -> Just (Shift 64)
    (481, Token (DOT_DOT _)) -> Just (Shift 82)
    (481, Token (RBRACKET _)) -> Just (Reduce 1 184)
    (482, Token (RPAREN _)) -> Just (Shift 457)
    (483, Token (RPAREN _)) -> Just (Shift 458)
    (484, Token (RPAREN _)) -> Just (Shift 459)
    (485, Token (RPAREN _)) -> Just (Shift 460)
    (486, Token (RPAREN _)) -> Just (Shift 461)
    (487, Token (RPAREN _)) -> Just (Shift 462)
    (488, Token (RBRACKET _)) -> Just (Shift 468)
    (489, Token (RPAREN _)) -> Just (Shift 463)
    (490, Token (COMMA _)) -> Just (Shift 80)
    (490, Token (RBRACKET _)) -> Just (Reduce 1 184)
    (491, Token (RBRACKET _)) -> Just (Reduce 3 185)
    (492, Token (RPAREN _)) -> Just (Reduce 3 186)
    (492, Token (COMMA _)) -> Just (Shift 58)
    (493, Token (RPAREN _)) -> Just (Reduce 3 187)
    (494, Token (RBRACE _)) -> Just (Reduce 5 208)
    (494, Token (SEMICOLON _)) -> Just (Reduce 5 208)
    (495, Token (RBRACE _)) -> Just (Reduce 5 210)
    (495, Token (SEMICOLON _)) -> Just (Reduce 5 210)
    (496, Token (WHERE _)) -> Just (Shift 288)
    (496, Token (RBRACE _)) -> Just (Reduce 3 207)
    (496, Token (SEMICOLON _)) -> Just (Reduce 3 207)
    (497, Token (WHERE _)) -> Just (Shift 289)
    (497, Token (RBRACE _)) -> Just (Reduce 3 209)
    (497, Token (SEMICOLON _)) -> Just (Reduce 3 209)
    (498, Token (WHERE _)) -> Just (Reduce 3 211)
    (498, Token (RBRACE _)) -> Just (Reduce 3 211)
    (498, Token (SEMICOLON _)) -> Just (Reduce 3 211)
    (498, Token (PIPE _)) -> Just (Shift 77)
    (499, Token (WHERE _)) -> Just (Reduce 5 212)
    (499, Token (RBRACE _)) -> Just (Reduce 5 212)
    (499, Token (SEMICOLON _)) -> Just (Reduce 5 212)
    (500, Token (RARROW _)) -> Just (Shift 48)
    (501, Token (RARROW _)) -> Just (Reduce 3 214)
    (502, Token (COMMA _)) -> Just (Shift 78)
    (502, Token (RARROW _)) -> Just (Reduce 1 213)
    (503, Token (COMMA _)) -> Just (Reduce 1 217)
    (503, Token (RARROW _)) -> Just (Reduce 1 217)
    (503, Token (LARROW _)) -> Just (Shift 79)
    (504, Token (COMMA _)) -> Just (Reduce 3 215)
    (504, Token (RARROW _)) -> Just (Reduce 3 215)
    (505, Token (RBRACE _)) -> Just (Reduce 1 221)
    (505, Token (SEMICOLON _)) -> Just (Reduce 1 221)
    (505, Token (LARROW _)) -> Just (Shift 54)
    (506, Token (RBRACE _)) -> Just (Reduce 3 222)
    (506, Token (SEMICOLON _)) -> Just (Reduce 3 222)
    (507, Token (LPAREN _)) -> Just (Reduce 3 229)
    (507, Token (RPAREN _)) -> Just (Reduce 3 229)
    (507, Token (EQUAL _)) -> Just (Reduce 3 229)
    (507, Token (PIPE _)) -> Just (Reduce 3 229)
    (507, Token (MINUS _)) -> Just (Reduce 3 229)
    (507, Token (RARROW _)) -> Just (Reduce 3 229)
    (507, Token (QCONID _)) -> Just (Reduce 3 229)
    (507, Token (EXPORT _)) -> Just (Reduce 3 229)
    (507, Token (AS _)) -> Just (Reduce 3 229)
    (507, Token (QVARID _)) -> Just (Reduce 3 229)
    (507, Token (QVARSYM _)) -> Just (Reduce 3 229)
    (507, Token (BACKQUOTE _)) -> Just (Reduce 3 229)
    (507, Token (QCONSYM _)) -> Just (Reduce 3 229)
    (508, Token (LPAREN _)) -> Just (Reduce 1 228)
    (508, Token (RPAREN _)) -> Just (Reduce 1 228)
    (508, Token (EQUAL _)) -> Just (Reduce 1 228)
    (508, Token (PIPE _)) -> Just (Reduce 1 228)
    (508, Token (MINUS _)) -> Just (Reduce 1 228)
    (508, Token (RARROW _)) -> Just (Reduce 1 228)
    (508, Token (QCONID _)) -> Just (Reduce 1 228)
    (508, Token (EXPORT _)) -> Just (Reduce 1 228)
    (508, Token (AS _)) -> Just (Reduce 1 228)
    (508, Token (QVARID _)) -> Just (Reduce 1 228)
    (508, Token (QVARSYM _)) -> Just (Reduce 1 228)
    (508, Token (BACKQUOTE _)) -> Just (Reduce 1 228)
    (508, Token (QCONSYM _)) -> Just (Reduce 1 228)
    (509, Token (BACKQUOTE _)) -> Just (Shift 513)
    (510, Token (BACKQUOTE _)) -> Just (Shift 514)
    (511, Token (BACKQUOTE _)) -> Just (Shift 515)
    (512, Token (RBRACE _)) -> Just (Reduce 1 237)
    (512, Token (LPAREN _)) -> Just (Reduce 1 237)
    (512, Token (COMMA _)) -> Just (Reduce 1 237)
    (512, Token (SEMICOLON _)) -> Just (Reduce 1 237)
    (512, Token (MINUS _)) -> Just (Reduce 1 237)
    (512, Token (QCONID _)) -> Just (Reduce 1 237)
    (512, Token (EXPORT _)) -> Just (Reduce 1 237)
    (512, Token (AS _)) -> Just (Reduce 1 237)
    (512, Token (QVARID _)) -> Just (Reduce 1 237)
    (512, Token (QVARSYM _)) -> Just (Reduce 1 237)
    (512, Token (BACKQUOTE _)) -> Just (Reduce 1 237)
    (512, Token (QCONSYM _)) -> Just (Reduce 1 237)
    (513, Token (RBRACE _)) -> Just (Reduce 3 239)
    (513, Token (LPAREN _)) -> Just (Reduce 3 239)
    (513, Token (COMMA _)) -> Just (Reduce 3 239)
    (513, Token (SEMICOLON _)) -> Just (Reduce 3 239)
    (513, Token (MINUS _)) -> Just (Reduce 3 239)
    (513, Token (QCONID _)) -> Just (Reduce 3 239)
    (513, Token (EXPORT _)) -> Just (Reduce 3 239)
    (513, Token (AS _)) -> Just (Reduce 3 239)
    (513, Token (QVARID _)) -> Just (Reduce 3 239)
    (513, Token (QVARSYM _)) -> Just (Reduce 3 239)
    (513, Token (BACKQUOTE _)) -> Just (Reduce 3 239)
    (513, Token (QCONSYM _)) -> Just (Reduce 3 239)
    (514, Token (RBRACE _)) -> Just (Reduce 3 238)
    (514, Token (LPAREN _)) -> Just (Reduce 3 238)
    (514, Token (COMMA _)) -> Just (Reduce 3 238)
    (514, Token (SEMICOLON _)) -> Just (Reduce 3 238)
    (514, Token (MINUS _)) -> Just (Reduce 3 238)
    (514, Token (QCONID _)) -> Just (Reduce 3 238)
    (514, Token (EXPORT _)) -> Just (Reduce 3 238)
    (514, Token (AS _)) -> Just (Reduce 3 238)
    (514, Token (QVARID _)) -> Just (Reduce 3 238)
    (514, Token (QVARSYM _)) -> Just (Reduce 3 238)
    (514, Token (BACKQUOTE _)) -> Just (Reduce 3 238)
    (514, Token (QCONSYM _)) -> Just (Reduce 3 238)
    (515, Token (RBRACE _)) -> Just (Reduce 3 240)
    (515, Token (LPAREN _)) -> Just (Reduce 3 240)
    (515, Token (COMMA _)) -> Just (Reduce 3 240)
    (515, Token (SEMICOLON _)) -> Just (Reduce 3 240)
    (515, Token (MINUS _)) -> Just (Reduce 3 240)
    (515, Token (QCONID _)) -> Just (Reduce 3 240)
    (515, Token (EXPORT _)) -> Just (Reduce 3 240)
    (515, Token (AS _)) -> Just (Reduce 3 240)
    (515, Token (QVARID _)) -> Just (Reduce 3 240)
    (515, Token (QVARSYM _)) -> Just (Reduce 3 240)
    (515, Token (BACKQUOTE _)) -> Just (Reduce 3 240)
    (515, Token (QCONSYM _)) -> Just (Reduce 3 240)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 3
production 3 = 2
production 4 = 2
production 5 = 5
production 6 = 6
production 7 = 6
production 8 = 6
production 9 = 7
production 10 = 7
production 11 = 7
production 12 = 7
production 13 = 7
production 14 = 7
production 15 = 11
production 16 = 11
production 17 = 11
production 18 = 12
production 19 = 12
production 20 = 12
production 21 = 12
production 22 = 12
production 23 = 10
production 24 = 10
production 25 = 13
production 26 = 13
production 27 = 4
production 28 = 4
production 29 = 14
production 30 = 14
production 31 = 14
production 32 = 14
production 33 = 14
production 34 = 14
production 35 = 14
production 36 = 14
production 37 = 14
production 38 = 14
production 39 = 14
production 40 = 14
production 41 = 14
production 42 = 14
production 43 = 14
production 44 = 14
production 45 = 14
production 46 = 14
production 47 = 14
production 48 = 14
production 49 = 14
production 50 = 14
production 51 = 14
production 52 = 14
production 53 = 14
production 54 = 14
production 55 = 14
production 56 = 14
production 57 = 28
production 58 = 29
production 59 = 29
production 60 = 27
production 61 = 27
production 62 = 27
production 63 = 27
production 64 = 27
production 65 = 23
production 66 = 23
production 67 = 34
production 68 = 35
production 69 = 35
production 70 = 36
production 71 = 36
production 72 = 36
production 73 = 36
production 74 = 36
production 75 = 24
production 76 = 24
production 77 = 37
production 78 = 38
production 79 = 38
production 80 = 39
production 81 = 39
production 82 = 39
production 83 = 39
production 84 = 39
production 85 = 30
production 86 = 30
production 87 = 30
production 88 = 30
production 89 = 43
production 90 = 43
production 91 = 43
production 92 = 43
production 93 = 40
production 94 = 40
production 95 = 41
production 96 = 41
production 97 = 41
production 98 = 25
production 99 = 25
production 100 = 18
production 101 = 18
production 102 = 17
production 103 = 17
production 104 = 45
production 105 = 45
production 106 = 45
production 107 = 45
production 108 = 45
production 109 = 45
production 110 = 48
production 111 = 48
production 112 = 46
production 113 = 46
production 114 = 46
production 115 = 46
production 116 = 46
production 117 = 49
production 118 = 49
production 119 = 19
production 120 = 19
production 121 = 50
production 122 = 50
production 123 = 51
production 124 = 51
production 125 = 51
production 126 = 51
production 127 = 22
production 128 = 22
production 129 = 53
production 130 = 53
production 131 = 54
production 132 = 21
production 133 = 21
production 134 = 20
production 135 = 26
production 136 = 26
production 137 = 26
production 138 = 55
production 139 = 55
production 140 = 55
production 141 = 56
production 142 = 58
production 143 = 57
production 144 = 57
production 145 = 57
production 146 = 33
production 147 = 33
production 148 = 59
production 149 = 59
production 150 = 60
production 151 = 60
production 152 = 60
production 153 = 32
production 154 = 61
production 155 = 61
production 156 = 61
production 157 = 61
production 158 = 61
production 159 = 61
production 160 = 61
production 161 = 61
production 162 = 61
production 163 = 61
production 164 = 61
production 165 = 61
production 166 = 61
production 167 = 64
production 168 = 64
production 169 = 64
production 170 = 64
production 171 = 64
production 172 = 64
production 173 = 64
production 174 = 64
production 175 = 64
production 176 = 64
production 177 = 64
production 178 = 63
production 179 = 63
production 180 = 63
production 181 = 63
production 182 = 67
production 183 = 67
production 184 = 69
production 185 = 69
production 186 = 70
production 187 = 70
production 188 = 68
production 189 = 68
production 190 = 68
production 191 = 68
production 192 = 68
production 193 = 68
production 194 = 68
production 195 = 68
production 196 = 68
production 197 = 68
production 198 = 68
production 199 = 68
production 200 = 68
production 201 = 68
production 202 = 68
production 203 = 68
production 204 = 65
production 205 = 65
production 206 = 71
production 207 = 71
production 208 = 71
production 209 = 71
production 210 = 71
production 211 = 72
production 212 = 72
production 213 = 73
production 214 = 73
production 215 = 74
production 216 = 74
production 217 = 74
production 218 = 66
production 219 = 66
production 220 = 75
production 221 = 75
production 222 = 75
production 223 = 75
production 224 = 31
production 225 = 31
production 226 = 31
production 227 = 31
production 228 = 76
production 229 = 76
production 230 = 8
production 231 = 8
production 232 = 8
production 233 = 8
production 234 = 8
production 235 = 9
production 236 = 9
production 237 = 77
production 238 = 77
production 239 = 77
production 240 = 77
production 241 = 52
production 242 = 52
production 243 = 44
production 244 = 44
production 245 = 16
production 246 = 16
production 247 = 15
production 248 = 15
production 249 = 47
production 250 = 47
production 251 = 47
production 252 = 78
production 253 = 1
production 254 = 42
production 255 = 42
production 256 = 62
production 257 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 221
    (9, 1) -> Just 246
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 330
    (13, 14) -> Just 18
    (13, 27) -> Just 244
    (13, 30) -> Just 280
    (13, 31) -> Just 102
    (13, 40) -> Just 300
    (13, 41) -> Just 301
    (13, 76) -> Just 304
    (16, 4) -> Just 17
    (16, 8) -> Just 330
    (16, 14) -> Just 18
    (16, 27) -> Just 244
    (16, 30) -> Just 280
    (16, 31) -> Just 102
    (16, 40) -> Just 300
    (16, 41) -> Just 301
    (16, 76) -> Just 304
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 195
    (25, 9) -> Just 196
    (25, 10) -> Just 33
    (25, 13) -> Just 185
    (34, 8) -> Just 476
    (34, 63) -> Just 427
    (34, 67) -> Just 35
    (34, 68) -> Just 454
    (35, 8) -> Just 476
    (35, 68) -> Just 455
    (36, 8) -> Just 476
    (36, 32) -> Just 404
    (36, 61) -> Just 308
    (36, 63) -> Just 418
    (36, 67) -> Just 35
    (36, 68) -> Just 454
    (37, 8) -> Just 476
    (37, 32) -> Just 405
    (37, 61) -> Just 308
    (37, 63) -> Just 418
    (37, 67) -> Just 35
    (37, 68) -> Just 454
    (38, 8) -> Just 476
    (38, 32) -> Just 406
    (38, 61) -> Just 308
    (38, 63) -> Just 418
    (38, 67) -> Just 35
    (38, 68) -> Just 454
    (39, 8) -> Just 476
    (39, 32) -> Just 409
    (39, 61) -> Just 308
    (39, 63) -> Just 418
    (39, 67) -> Just 35
    (39, 68) -> Just 454
    (40, 8) -> Just 476
    (40, 32) -> Just 410
    (40, 61) -> Just 308
    (40, 63) -> Just 418
    (40, 67) -> Just 35
    (40, 68) -> Just 454
    (41, 8) -> Just 476
    (41, 32) -> Just 411
    (41, 61) -> Just 308
    (41, 63) -> Just 418
    (41, 67) -> Just 35
    (41, 68) -> Just 454
    (42, 8) -> Just 476
    (42, 32) -> Just 412
    (42, 61) -> Just 308
    (42, 63) -> Just 418
    (42, 67) -> Just 35
    (42, 68) -> Just 454
    (43, 8) -> Just 476
    (43, 32) -> Just 413
    (43, 61) -> Just 308
    (43, 63) -> Just 418
    (43, 67) -> Just 35
    (43, 68) -> Just 454
    (44, 8) -> Just 476
    (44, 32) -> Just 414
    (44, 61) -> Just 308
    (44, 63) -> Just 418
    (44, 67) -> Just 35
    (44, 68) -> Just 454
    (45, 8) -> Just 476
    (45, 32) -> Just 415
    (45, 61) -> Just 308
    (45, 63) -> Just 418
    (45, 67) -> Just 35
    (45, 68) -> Just 454
    (46, 8) -> Just 476
    (46, 32) -> Just 281
    (46, 61) -> Just 308
    (46, 63) -> Just 418
    (46, 67) -> Just 35
    (46, 68) -> Just 454
    (47, 8) -> Just 476
    (47, 32) -> Just 309
    (47, 61) -> Just 308
    (47, 63) -> Just 418
    (47, 67) -> Just 35
    (47, 68) -> Just 454
    (48, 8) -> Just 476
    (48, 32) -> Just 498
    (48, 61) -> Just 308
    (48, 63) -> Just 418
    (48, 67) -> Just 35
    (48, 68) -> Just 454
    (49, 8) -> Just 476
    (49, 32) -> Just 319
    (49, 61) -> Just 308
    (49, 63) -> Just 418
    (49, 67) -> Just 35
    (49, 68) -> Just 454
    (50, 8) -> Just 476
    (50, 32) -> Just 327
    (50, 61) -> Just 308
    (50, 63) -> Just 418
    (50, 67) -> Just 35
    (50, 68) -> Just 454
    (51, 8) -> Just 476
    (51, 32) -> Just 496
    (51, 61) -> Just 308
    (51, 63) -> Just 418
    (51, 67) -> Just 35
    (51, 68) -> Just 454
    (52, 8) -> Just 476
    (52, 61) -> Just 505
    (52, 63) -> Just 418
    (52, 66) -> Just 429
    (52, 67) -> Just 35
    (52, 68) -> Just 454
    (52, 75) -> Just 453
    (53, 8) -> Just 476
    (53, 61) -> Just 505
    (53, 63) -> Just 418
    (53, 66) -> Just 452
    (53, 67) -> Just 35
    (53, 68) -> Just 454
    (53, 75) -> Just 453
    (54, 8) -> Just 476
    (54, 61) -> Just 506
    (54, 63) -> Just 418
    (54, 67) -> Just 35
    (54, 68) -> Just 454
    (55, 8) -> Just 476
    (55, 32) -> Just 477
    (55, 61) -> Just 308
    (55, 63) -> Just 418
    (55, 67) -> Just 35
    (55, 68) -> Just 454
    (55, 70) -> Just 489
    (56, 8) -> Just 476
    (56, 63) -> Just 427
    (56, 67) -> Just 35
    (56, 68) -> Just 454
    (57, 8) -> Just 476
    (57, 61) -> Just 482
    (57, 63) -> Just 418
    (57, 67) -> Just 35
    (57, 68) -> Just 454
    (58, 8) -> Just 476
    (58, 32) -> Just 492
    (58, 61) -> Just 308
    (58, 63) -> Just 418
    (58, 67) -> Just 35
    (58, 68) -> Just 454
    (58, 70) -> Just 493
    (59, 8) -> Just 476
    (59, 61) -> Just 483
    (59, 63) -> Just 418
    (59, 67) -> Just 35
    (59, 68) -> Just 454
    (60, 8) -> Just 476
    (60, 61) -> Just 484
    (60, 63) -> Just 418
    (60, 67) -> Just 35
    (60, 68) -> Just 454
    (61, 8) -> Just 476
    (61, 61) -> Just 485
    (61, 63) -> Just 418
    (61, 67) -> Just 35
    (61, 68) -> Just 454
    (62, 8) -> Just 476
    (62, 61) -> Just 486
    (62, 63) -> Just 418
    (62, 67) -> Just 35
    (62, 68) -> Just 454
    (63, 8) -> Just 476
    (63, 61) -> Just 487
    (63, 63) -> Just 418
    (63, 67) -> Just 35
    (63, 68) -> Just 454
    (64, 8) -> Just 476
    (64, 32) -> Just 478
    (64, 61) -> Just 308
    (64, 63) -> Just 418
    (64, 67) -> Just 35
    (64, 68) -> Just 454
    (64, 69) -> Just 491
    (65, 8) -> Just 476
    (65, 32) -> Just 481
    (65, 61) -> Just 308
    (65, 63) -> Just 418
    (65, 67) -> Just 35
    (65, 68) -> Just 454
    (65, 69) -> Just 488
    (66, 8) -> Just 476
    (66, 63) -> Just 439
    (66, 64) -> Just 440
    (66, 67) -> Just 35
    (66, 68) -> Just 454
    (67, 8) -> Just 476
    (67, 63) -> Just 439
    (67, 64) -> Just 441
    (67, 67) -> Just 35
    (67, 68) -> Just 454
    (68, 8) -> Just 476
    (68, 63) -> Just 439
    (68, 64) -> Just 442
    (68, 67) -> Just 35
    (68, 68) -> Just 454
    (69, 8) -> Just 476
    (69, 63) -> Just 439
    (69, 64) -> Just 443
    (69, 67) -> Just 35
    (69, 68) -> Just 454
    (70, 8) -> Just 476
    (70, 63) -> Just 439
    (70, 64) -> Just 444
    (70, 67) -> Just 35
    (70, 68) -> Just 454
    (71, 8) -> Just 476
    (71, 63) -> Just 439
    (71, 64) -> Just 445
    (71, 67) -> Just 35
    (71, 68) -> Just 454
    (72, 8) -> Just 476
    (72, 63) -> Just 439
    (72, 64) -> Just 446
    (72, 67) -> Just 35
    (72, 68) -> Just 454
    (73, 8) -> Just 476
    (73, 63) -> Just 439
    (73, 64) -> Just 447
    (73, 67) -> Just 35
    (73, 68) -> Just 454
    (74, 8) -> Just 476
    (74, 63) -> Just 439
    (74, 64) -> Just 448
    (74, 67) -> Just 35
    (74, 68) -> Just 454
    (75, 8) -> Just 476
    (75, 63) -> Just 439
    (75, 64) -> Just 449
    (75, 67) -> Just 35
    (75, 68) -> Just 454
    (76, 8) -> Just 476
    (76, 63) -> Just 439
    (76, 64) -> Just 503
    (76, 67) -> Just 35
    (76, 68) -> Just 454
    (76, 72) -> Just 497
    (76, 73) -> Just 500
    (76, 74) -> Just 502
    (77, 8) -> Just 476
    (77, 63) -> Just 439
    (77, 64) -> Just 503
    (77, 67) -> Just 35
    (77, 68) -> Just 454
    (77, 72) -> Just 499
    (77, 73) -> Just 500
    (77, 74) -> Just 502
    (78, 8) -> Just 476
    (78, 63) -> Just 439
    (78, 64) -> Just 503
    (78, 67) -> Just 35
    (78, 68) -> Just 454
    (78, 73) -> Just 501
    (78, 74) -> Just 502
    (79, 8) -> Just 476
    (79, 63) -> Just 439
    (79, 64) -> Just 504
    (79, 67) -> Just 35
    (79, 68) -> Just 454
    (80, 8) -> Just 476
    (80, 32) -> Just 490
    (80, 61) -> Just 308
    (80, 63) -> Just 418
    (80, 67) -> Just 35
    (80, 68) -> Just 454
    (80, 69) -> Just 491
    (81, 8) -> Just 476
    (81, 32) -> Just 479
    (81, 61) -> Just 308
    (81, 63) -> Just 418
    (81, 67) -> Just 35
    (81, 68) -> Just 454
    (82, 8) -> Just 476
    (82, 32) -> Just 480
    (82, 61) -> Just 308
    (82, 63) -> Just 418
    (82, 67) -> Just 35
    (82, 68) -> Just 454
    (83, 8) -> Just 476
    (83, 33) -> Just 282
    (83, 59) -> Just 311
    (83, 60) -> Just 391
    (83, 61) -> Just 394
    (83, 63) -> Just 418
    (83, 67) -> Just 35
    (83, 68) -> Just 454
    (84, 8) -> Just 476
    (84, 33) -> Just 310
    (84, 59) -> Just 311
    (84, 60) -> Just 391
    (84, 61) -> Just 394
    (84, 63) -> Just 418
    (84, 67) -> Just 35
    (84, 68) -> Just 454
    (85, 8) -> Just 476
    (85, 33) -> Just 320
    (85, 59) -> Just 311
    (85, 60) -> Just 391
    (85, 61) -> Just 394
    (85, 63) -> Just 418
    (85, 67) -> Just 35
    (85, 68) -> Just 454
    (86, 8) -> Just 476
    (86, 33) -> Just 328
    (86, 59) -> Just 311
    (86, 60) -> Just 391
    (86, 61) -> Just 394
    (86, 63) -> Just 418
    (86, 67) -> Just 35
    (86, 68) -> Just 454
    (87, 8) -> Just 476
    (87, 59) -> Just 390
    (87, 60) -> Just 391
    (87, 61) -> Just 394
    (87, 63) -> Just 418
    (87, 67) -> Just 35
    (87, 68) -> Just 454
    (88, 8) -> Just 476
    (88, 32) -> Just 407
    (88, 61) -> Just 308
    (88, 63) -> Just 418
    (88, 67) -> Just 35
    (88, 68) -> Just 454
    (89, 8) -> Just 476
    (89, 32) -> Just 419
    (89, 61) -> Just 308
    (89, 63) -> Just 418
    (89, 67) -> Just 35
    (89, 68) -> Just 454
    (90, 8) -> Just 476
    (90, 32) -> Just 408
    (90, 61) -> Just 308
    (90, 63) -> Just 418
    (90, 67) -> Just 35
    (90, 68) -> Just 454
    (91, 8) -> Just 476
    (91, 32) -> Just 420
    (91, 61) -> Just 308
    (91, 63) -> Just 418
    (91, 67) -> Just 35
    (91, 68) -> Just 454
    (92, 8) -> Just 476
    (92, 32) -> Just 426
    (92, 61) -> Just 308
    (92, 63) -> Just 418
    (92, 67) -> Just 35
    (92, 68) -> Just 454
    (93, 8) -> Just 476
    (93, 32) -> Just 393
    (93, 61) -> Just 308
    (93, 63) -> Just 418
    (93, 67) -> Just 35
    (93, 68) -> Just 454
    (94, 8) -> Just 508
    (94, 76) -> Just 305
    (95, 8) -> Just 508
    (95, 76) -> Just 307
    (96, 8) -> Just 508
    (96, 31) -> Just 97
    (96, 76) -> Just 304
    (97, 8) -> Just 508
    (97, 44) -> Just 95
    (97, 52) -> Just 342
    (97, 76) -> Just 306
    (97, 77) -> Just 343
    (98, 8) -> Just 330
    (98, 27) -> Just 296
    (98, 29) -> Just 295
    (98, 30) -> Just 280
    (98, 31) -> Just 102
    (98, 40) -> Just 300
    (98, 41) -> Just 301
    (98, 76) -> Just 304
    (99, 8) -> Just 330
    (99, 27) -> Just 296
    (99, 29) -> Just 297
    (99, 30) -> Just 280
    (99, 31) -> Just 102
    (99, 40) -> Just 300
    (99, 41) -> Just 301
    (99, 76) -> Just 304
    (100, 8) -> Just 330
    (100, 30) -> Just 318
    (100, 31) -> Just 105
    (100, 35) -> Just 313
    (100, 36) -> Just 315
    (100, 40) -> Just 300
    (100, 41) -> Just 301
    (100, 76) -> Just 304
    (101, 8) -> Just 330
    (101, 30) -> Just 318
    (101, 31) -> Just 105
    (101, 35) -> Just 314
    (101, 36) -> Just 315
    (101, 40) -> Just 300
    (101, 41) -> Just 301
    (101, 76) -> Just 304
    (102, 8) -> Just 508
    (102, 44) -> Just 95
    (102, 52) -> Just 342
    (102, 76) -> Just 306
    (102, 77) -> Just 343
    (103, 8) -> Just 508
    (103, 31) -> Just 106
    (103, 38) -> Just 322
    (103, 39) -> Just 324
    (103, 76) -> Just 304
    (104, 8) -> Just 508
    (104, 31) -> Just 106
    (104, 38) -> Just 323
    (104, 39) -> Just 324
    (104, 76) -> Just 304
    (105, 8) -> Just 508
    (105, 44) -> Just 95
    (105, 52) -> Just 342
    (105, 76) -> Just 306
    (105, 77) -> Just 343
    (106, 8) -> Just 508
    (106, 44) -> Just 95
    (106, 52) -> Just 342
    (106, 76) -> Just 306
    (106, 77) -> Just 343
    (107, 8) -> Just 508
    (107, 31) -> Just 109
    (107, 65) -> Just 428
    (107, 71) -> Just 451
    (107, 76) -> Just 304
    (108, 8) -> Just 508
    (108, 31) -> Just 109
    (108, 65) -> Just 450
    (108, 71) -> Just 451
    (108, 76) -> Just 304
    (109, 8) -> Just 508
    (109, 44) -> Just 95
    (109, 52) -> Just 342
    (109, 76) -> Just 306
    (109, 77) -> Just 343
    (110, 8) -> Just 508
    (110, 31) -> Just 112
    (110, 76) -> Just 304
    (111, 8) -> Just 508
    (111, 31) -> Just 113
    (111, 76) -> Just 304
    (112, 8) -> Just 508
    (112, 44) -> Just 95
    (112, 52) -> Just 342
    (112, 76) -> Just 306
    (112, 77) -> Just 343
    (113, 8) -> Just 508
    (113, 44) -> Just 95
    (113, 52) -> Just 342
    (113, 76) -> Just 306
    (113, 77) -> Just 343
    (114, 8) -> Just 192
    (114, 9) -> Just 193
    (114, 11) -> Just 186
    (114, 12) -> Just 187
    (115, 8) -> Just 192
    (115, 9) -> Just 193
    (115, 11) -> Just 222
    (115, 12) -> Just 187
    (116, 8) -> Just 192
    (116, 9) -> Just 193
    (116, 11) -> Just 223
    (116, 12) -> Just 187
    (117, 8) -> Just 195
    (117, 9) -> Just 196
    (117, 10) -> Just 184
    (117, 13) -> Just 185
    (118, 8) -> Just 195
    (118, 9) -> Just 196
    (118, 10) -> Just 194
    (118, 13) -> Just 185
    (119, 8) -> Just 329
    (119, 40) -> Just 331
    (120, 8) -> Just 329
    (120, 40) -> Just 381
    (120, 53) -> Just 372
    (120, 54) -> Just 379
    (121, 8) -> Just 329
    (121, 40) -> Just 381
    (121, 53) -> Just 378
    (121, 54) -> Just 379
    (122, 8) -> Just 256
    (123, 8) -> Just 267
    (124, 8) -> Just 268
    (125, 8) -> Just 269
    (135, 9) -> Just 358
    (135, 45) -> Just 349
    (135, 46) -> Just 350
    (135, 47) -> Just 351
    (136, 9) -> Just 358
    (136, 17) -> Just 137
    (136, 45) -> Just 247
    (136, 46) -> Just 350
    (136, 47) -> Just 351
    (137, 9) -> Just 358
    (137, 23) -> Just 239
    (137, 45) -> Just 248
    (137, 46) -> Just 350
    (137, 47) -> Just 351
    (138, 9) -> Just 358
    (138, 17) -> Just 139
    (138, 45) -> Just 247
    (138, 46) -> Just 350
    (138, 47) -> Just 351
    (139, 9) -> Just 358
    (139, 24) -> Just 241
    (139, 45) -> Just 248
    (139, 46) -> Just 350
    (139, 47) -> Just 351
    (140, 9) -> Just 358
    (140, 17) -> Just 141
    (140, 45) -> Just 247
    (140, 46) -> Just 350
    (140, 47) -> Just 351
    (141, 9) -> Just 358
    (141, 23) -> Just 238
    (141, 45) -> Just 248
    (141, 46) -> Just 350
    (141, 47) -> Just 351
    (142, 9) -> Just 358
    (142, 17) -> Just 143
    (142, 45) -> Just 247
    (142, 46) -> Just 350
    (142, 47) -> Just 351
    (143, 9) -> Just 358
    (143, 24) -> Just 240
    (143, 45) -> Just 248
    (143, 46) -> Just 350
    (143, 47) -> Just 351
    (144, 9) -> Just 358
    (144, 17) -> Just 145
    (144, 18) -> Just 401
    (144, 45) -> Just 247
    (144, 46) -> Just 350
    (144, 47) -> Just 351
    (145, 9) -> Just 358
    (145, 45) -> Just 248
    (145, 46) -> Just 350
    (145, 47) -> Just 351
    (146, 9) -> Just 358
    (146, 17) -> Just 148
    (146, 18) -> Just 249
    (146, 45) -> Just 247
    (146, 46) -> Just 350
    (146, 47) -> Just 351
    (147, 9) -> Just 358
    (147, 17) -> Just 148
    (147, 18) -> Just 400
    (147, 45) -> Just 247
    (147, 46) -> Just 350
    (147, 47) -> Just 351
    (148, 9) -> Just 358
    (148, 45) -> Just 248
    (148, 46) -> Just 350
    (148, 47) -> Just 351
    (149, 9) -> Just 358
    (149, 17) -> Just 150
    (149, 45) -> Just 247
    (149, 46) -> Just 350
    (149, 47) -> Just 351
    (150, 9) -> Just 358
    (150, 19) -> Just 226
    (150, 45) -> Just 248
    (150, 46) -> Just 350
    (150, 47) -> Just 351
    (151, 9) -> Just 358
    (151, 17) -> Just 152
    (151, 45) -> Just 247
    (151, 46) -> Just 350
    (151, 47) -> Just 351
    (152, 9) -> Just 358
    (152, 19) -> Just 227
    (152, 45) -> Just 248
    (152, 46) -> Just 350
    (152, 47) -> Just 351
    (153, 9) -> Just 359
    (153, 17) -> Just 156
    (153, 45) -> Just 247
    (153, 46) -> Just 350
    (153, 47) -> Just 351
    (153, 50) -> Just 250
    (153, 51) -> Just 369
    (154, 9) -> Just 359
    (154, 17) -> Just 156
    (154, 45) -> Just 247
    (154, 46) -> Just 350
    (154, 47) -> Just 351
    (154, 50) -> Just 368
    (154, 51) -> Just 369
    (155, 9) -> Just 168
    (156, 9) -> Just 358
    (156, 45) -> Just 248
    (156, 46) -> Just 350
    (156, 47) -> Just 351
    (156, 52) -> Just 157
    (157, 9) -> Just 358
    (157, 17) -> Just 158
    (157, 45) -> Just 247
    (157, 46) -> Just 350
    (157, 47) -> Just 351
    (158, 9) -> Just 358
    (158, 45) -> Just 248
    (158, 46) -> Just 350
    (158, 47) -> Just 351
    (159, 9) -> Just 358
    (159, 17) -> Just 160
    (159, 18) -> Just 299
    (159, 45) -> Just 247
    (159, 46) -> Just 350
    (159, 47) -> Just 351
    (160, 9) -> Just 358
    (160, 45) -> Just 248
    (160, 46) -> Just 350
    (160, 47) -> Just 351
    (161, 9) -> Just 358
    (161, 17) -> Just 148
    (161, 18) -> Just 225
    (161, 45) -> Just 247
    (161, 46) -> Just 350
    (161, 47) -> Just 351
    (162, 9) -> Just 358
    (162, 17) -> Just 148
    (162, 18) -> Just 270
    (162, 45) -> Just 247
    (162, 46) -> Just 350
    (162, 47) -> Just 351
    (163, 9) -> Just 358
    (163, 17) -> Just 148
    (163, 18) -> Just 271
    (163, 45) -> Just 247
    (163, 46) -> Just 350
    (163, 47) -> Just 351
    (164, 9) -> Just 358
    (164, 17) -> Just 148
    (164, 18) -> Just 272
    (164, 45) -> Just 247
    (164, 46) -> Just 350
    (164, 47) -> Just 351
    (165, 9) -> Just 358
    (165, 17) -> Just 148
    (165, 18) -> Just 298
    (165, 45) -> Just 247
    (165, 46) -> Just 350
    (165, 47) -> Just 351
    (166, 9) -> Just 358
    (166, 17) -> Just 148
    (166, 18) -> Just 380
    (166, 45) -> Just 247
    (166, 46) -> Just 350
    (166, 47) -> Just 351
    (167, 9) -> Just 358
    (167, 17) -> Just 148
    (167, 18) -> Just 257
    (167, 45) -> Just 247
    (167, 46) -> Just 350
    (167, 47) -> Just 351
    (168, 9) -> Just 358
    (168, 45) -> Just 258
    (168, 46) -> Just 350
    (168, 47) -> Just 351
    (169, 9) -> Just 358
    (169, 17) -> Just 170
    (169, 45) -> Just 247
    (169, 46) -> Just 350
    (169, 47) -> Just 351
    (170, 9) -> Just 358
    (170, 22) -> Just 237
    (170, 45) -> Just 248
    (170, 46) -> Just 350
    (170, 47) -> Just 351
    (171, 9) -> Just 358
    (171, 17) -> Just 173
    (171, 45) -> Just 247
    (171, 46) -> Just 350
    (171, 47) -> Just 351
    (172, 9) -> Just 358
    (172, 17) -> Just 174
    (172, 45) -> Just 247
    (172, 46) -> Just 350
    (172, 47) -> Just 351
    (173, 9) -> Just 358
    (173, 45) -> Just 248
    (173, 46) -> Just 350
    (173, 47) -> Just 351
    (174, 9) -> Just 358
    (174, 22) -> Just 236
    (174, 45) -> Just 248
    (174, 46) -> Just 350
    (174, 47) -> Just 351
    (175, 9) -> Just 358
    (175, 17) -> Just 148
    (175, 18) -> Just 347
    (175, 45) -> Just 247
    (175, 46) -> Just 350
    (175, 47) -> Just 351
    (175, 48) -> Just 352
    (175, 49) -> Just 360
    (176, 9) -> Just 358
    (176, 17) -> Just 148
    (176, 18) -> Just 263
    (176, 25) -> Just 242
    (176, 45) -> Just 247
    (176, 46) -> Just 350
    (176, 47) -> Just 351
    (177, 9) -> Just 358
    (177, 17) -> Just 148
    (177, 18) -> Just 263
    (177, 25) -> Just 264
    (177, 45) -> Just 247
    (177, 46) -> Just 350
    (177, 47) -> Just 351
    (178, 9) -> Just 358
    (178, 17) -> Just 148
    (178, 18) -> Just 364
    (178, 45) -> Just 247
    (178, 46) -> Just 350
    (178, 47) -> Just 351
    (178, 48) -> Just 365
    (179, 9) -> Just 358
    (179, 17) -> Just 148
    (179, 18) -> Just 348
    (179, 45) -> Just 247
    (179, 46) -> Just 350
    (179, 47) -> Just 351
    (197, 20) -> Just 253
    (197, 21) -> Just 232
    (198, 20) -> Just 253
    (198, 21) -> Just 233
    (199, 20) -> Just 253
    (199, 21) -> Just 234
    (200, 20) -> Just 253
    (200, 21) -> Just 235
    (213, 15) -> Just 8
    (215, 20) -> Just 228
    (216, 20) -> Just 229
    (217, 20) -> Just 230
    (218, 20) -> Just 231
    (220, 26) -> Just 243
    (221, 16) -> Just 224
    (251, 20) -> Just 253
    (251, 21) -> Just 254
    (259, 34) -> Just 260
    (261, 37) -> Just 262
    (265, 55) -> Just 273
    (266, 55) -> Just 274
    (273, 56) -> Just 123
    (273, 57) -> Just 275
    (274, 58) -> Just 125
    (275, 56) -> Just 124
    (276, 28) -> Just 278
    (277, 28) -> Just 279
    (283, 28) -> Just 403
    (284, 28) -> Just 316
    (285, 28) -> Just 317
    (286, 28) -> Just 325
    (287, 28) -> Just 326
    (288, 28) -> Just 494
    (289, 28) -> Just 495
    (290, 28) -> Just 392
    (291, 28) -> Just 436
    (292, 28) -> Just 402
    (293, 28) -> Just 435
    (301, 42) -> Just 302
    (302, 43) -> Just 303
    (302, 44) -> Just 341
    (302, 52) -> Just 342
    (302, 77) -> Just 343
    (336, 43) -> Just 339
    (336, 44) -> Just 341
    (336, 52) -> Just 342
    (336, 77) -> Just 343
    (337, 43) -> Just 340
    (337, 44) -> Just 341
    (337, 52) -> Just 342
    (337, 77) -> Just 343
    (366, 49) -> Just 367
    (407, 62) -> Just 416
    (408, 62) -> Just 417
    (419, 62) -> Just 437
    (420, 62) -> Just 438
    (_, _) -> Nothing

parse :: Monad m => SemanticActions m -> [Token] -> m (Either (Maybe Token) (Module', [Token]))
parse actions = parse' [] where
  parse' stack tokens =
    let p =
          case stack of
            [] -> 0
            ((q, _) : _) -> q in
    let symbol =
          case tokens of
            [] -> EOF
            (token : _) -> Token token in do
      case dfaActionTransition p symbol of
        Nothing ->
          case tokens of
            [] -> return $ Left $ Nothing
            (token : _) -> return $ Left $ Just token
        Just (Shift n) ->
          let value =
                case symbol of
                  EOF ->
                    StackValue_EOF
                  Token (MODULE semanticValue) ->
                    StackValue_MODULE semanticValue
                  Token (WHERE semanticValue) ->
                    StackValue_WHERE semanticValue
                  Token (LBRACE semanticValue) ->
                    StackValue_LBRACE semanticValue
                  Token (RBRACE semanticValue) ->
                    StackValue_RBRACE semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (DOT_DOT semanticValue) ->
                    StackValue_DOT_DOT semanticValue
                  Token (SEMICOLON semanticValue) ->
                    StackValue_SEMICOLON semanticValue
                  Token (IMPORT semanticValue) ->
                    StackValue_IMPORT semanticValue
                  Token (HIDING semanticValue) ->
                    StackValue_HIDING semanticValue
                  Token (TYPE semanticValue) ->
                    StackValue_TYPE semanticValue
                  Token (EQUAL semanticValue) ->
                    StackValue_EQUAL semanticValue
                  Token (DATA semanticValue) ->
                    StackValue_DATA semanticValue
                  Token (DERIVING semanticValue) ->
                    StackValue_DERIVING semanticValue
                  Token (DARROW semanticValue) ->
                    StackValue_DARROW semanticValue
                  Token (NEWTYPE semanticValue) ->
                    StackValue_NEWTYPE semanticValue
                  Token (CLASS semanticValue) ->
                    StackValue_CLASS semanticValue
                  Token (INSTANCE semanticValue) ->
                    StackValue_INSTANCE semanticValue
                  Token (DEFAULT semanticValue) ->
                    StackValue_DEFAULT semanticValue
                  Token (FOREIGN semanticValue) ->
                    StackValue_FOREIGN semanticValue
                  Token (PIPE semanticValue) ->
                    StackValue_PIPE semanticValue
                  Token (COLON_COLON semanticValue) ->
                    StackValue_COLON_COLON semanticValue
                  Token (MINUS semanticValue) ->
                    StackValue_MINUS semanticValue
                  Token (INFIXL semanticValue) ->
                    StackValue_INFIXL semanticValue
                  Token (INFIXR semanticValue) ->
                    StackValue_INFIXR semanticValue
                  Token (INFIX semanticValue) ->
                    StackValue_INFIX semanticValue
                  Token (RARROW semanticValue) ->
                    StackValue_RARROW semanticValue
                  Token (LBRACKET semanticValue) ->
                    StackValue_LBRACKET semanticValue
                  Token (RBRACKET semanticValue) ->
                    StackValue_RBRACKET semanticValue
                  Token (EXCL semanticValue) ->
                    StackValue_EXCL semanticValue
                  Token (QCONID semanticValue) ->
                    StackValue_QCONID semanticValue
                  Token (EXPORT semanticValue) ->
                    StackValue_EXPORT semanticValue
                  Token (AS semanticValue) ->
                    StackValue_AS semanticValue
                  Token (QVARID semanticValue) ->
                    StackValue_QVARID semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (LARROW semanticValue) ->
                    StackValue_LARROW semanticValue
                  Token (LET semanticValue) ->
                    StackValue_LET semanticValue
                  Token (LAMBDA semanticValue) ->
                    StackValue_LAMBDA semanticValue
                  Token (IN semanticValue) ->
                    StackValue_IN semanticValue
                  Token (IF semanticValue) ->
                    StackValue_IF semanticValue
                  Token (THEN semanticValue) ->
                    StackValue_THEN semanticValue
                  Token (ELSE semanticValue) ->
                    StackValue_ELSE semanticValue
                  Token (QVARSYM semanticValue) ->
                    StackValue_QVARSYM semanticValue
                  Token (BACKQUOTE semanticValue) ->
                    StackValue_BACKQUOTE semanticValue
                  Token (QCONSYM semanticValue) ->
                    StackValue_QCONSYM semanticValue
                  Token (CASE semanticValue) ->
                    StackValue_CASE semanticValue
                  Token (OF semanticValue) ->
                    StackValue_OF semanticValue
                  Token (DO semanticValue) ->
                    StackValue_DO semanticValue
                  Token (INTEGER semanticValue) ->
                    StackValue_INTEGER semanticValue
                  Token (QUALIFIED semanticValue) ->
                    StackValue_QUALIFIED semanticValue
          in parse' ((n, value) : stack) (tail tokens)
        Just (Reduce n m) ->
          let (pop, stack') = splitAt n stack in
            case
              case stack' of
                [] -> dfaGotoTransition 0 m
                ((q', _) : _) -> dfaGotoTransition q' m of
              Nothing ->
                case tokens of
                  [] -> return $ Left $ Nothing
                  (token : _) -> return $ Left $ Just token
              Just q -> do
                value <-
                  case m of
                    0 ->
                      Monad.liftM StackValue_module' $ module'_implies_MODULE_modid_exports_opt_WHERE_body actions (case snd (pop !! 4) of { StackValue_MODULE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_modid value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exports_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_body value -> value; _ -> undefined })
                    1 ->
                      Monad.liftM StackValue_module' $ module'_implies_body actions (case snd (pop !! 0) of { StackValue_body value -> value; _ -> undefined })
                    2 ->
                      Monad.liftM StackValue_body $ body_implies_LBRACE_topdecls_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_topdecls value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies actions
                    4 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies_exports actions (case snd (pop !! 0) of { StackValue_exports value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_exports $ exports_implies_LPAREN_export_seq_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_export_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies actions
                    7 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export actions (case snd (pop !! 0) of { StackValue_export value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export_COMMA_export_seq actions (case snd (pop !! 2) of { StackValue_export value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_export_seq value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_export $ export_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_export $ export_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_export $ export_implies_MODULE_modid actions (case snd (pop !! 1) of { StackValue_MODULE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies actions
                    16 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import' actions (case snd (pop !! 0) of { StackValue_import' value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import'_COMMA_import_seq actions (case snd (pop !! 2) of { StackValue_import' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_import_seq value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_import' $ import'_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_import' $ import'_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname actions (case snd (pop !! 0) of { StackValue_cname value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname_COMMA_cname_seq actions (case snd (pop !! 2) of { StackValue_cname value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cname_seq value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_cname $ cname_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_cname $ cname_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_topdecls $ topdecls_implies_topdecl actions (case snd (pop !! 0) of { StackValue_topdecl value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_topdecls $ topdecls_implies_topdecl_SEMICOLON_topdecls actions (case snd (pop !! 2) of { StackValue_topdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_topdecls value -> value; _ -> undefined })
                    29 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_IMPORT_qualified_opt_modid_as_opt actions (case snd (pop !! 3) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_qualified_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_modid value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_as_opt value -> value; _ -> undefined })
                    30 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_IMPORT_qualified_opt_modid_as_opt_LPAREN_import_seq_RPAREN actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_qualified_opt value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_modid value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_as_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    31 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_IMPORT_qualified_opt_modid_as_opt_HIDING_LPAREN_import_seq_RPAREN actions (case snd (pop !! 7) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_qualified_opt value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_modid value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_as_opt value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_HIDING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    32 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_TYPE_btype_EQUAL_type' actions (case snd (pop !! 3) of { StackValue_TYPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    33 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt actions (case snd (pop !! 2) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs_opt value -> value; _ -> undefined })
                    34 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass actions (case snd (pop !! 4) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    35 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 5) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    36 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 6) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    37 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt actions (case snd (pop !! 4) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs_opt value -> value; _ -> undefined })
                    38 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass actions (case snd (pop !! 6) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    39 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 7) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    40 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 8) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 7) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    41 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr actions (case snd (pop !! 2) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_newconstr value -> value; _ -> undefined })
                    42 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass actions (case snd (pop !! 4) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    43 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 5) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    44 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 6) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    45 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr actions (case snd (pop !! 4) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_newconstr value -> value; _ -> undefined })
                    46 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass actions (case snd (pop !! 6) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    47 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 7) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    48 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 8) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 7) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    49 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_CLASS_btype_cdecls_opt actions (case snd (pop !! 2) of { StackValue_CLASS value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls_opt value -> value; _ -> undefined })
                    50 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt actions (case snd (pop !! 4) of { StackValue_CLASS value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls_opt value -> value; _ -> undefined })
                    51 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_INSTANCE_btype_idecls_opt actions (case snd (pop !! 2) of { StackValue_INSTANCE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls_opt value -> value; _ -> undefined })
                    52 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt actions (case snd (pop !! 4) of { StackValue_INSTANCE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls_opt value -> value; _ -> undefined })
                    53 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DEFAULT_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_DEFAULT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    54 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_DEFAULT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    55 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_FOREIGN_fdecl actions (case snd (pop !! 1) of { StackValue_FOREIGN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fdecl value -> value; _ -> undefined })
                    56 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_decl actions (case snd (pop !! 0) of { StackValue_decl value -> value; _ -> undefined })
                    57 ->
                      Monad.liftM StackValue_decls $ decls_implies_LBRACE_decl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_decl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    58 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl actions (case snd (pop !! 0) of { StackValue_decl value -> value; _ -> undefined })
                    59 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl_SEMICOLON_decl_seq actions (case snd (pop !! 2) of { StackValue_decl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decl_seq value -> value; _ -> undefined })
                    60 ->
                      Monad.liftM StackValue_decl $ decl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    61 ->
                      Monad.liftM StackValue_decl $ decl_implies_pat_EQUAL_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    62 ->
                      Monad.liftM StackValue_decl $ decl_implies_pat_EQUAL_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    63 ->
                      Monad.liftM StackValue_decl $ decl_implies_pat_PIPE_gdrhs actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdrhs value -> value; _ -> undefined })
                    64 ->
                      Monad.liftM StackValue_decl $ decl_implies_pat_PIPE_gdrhs_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_gdrhs value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    65 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies actions
                    66 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies_WHERE_cdecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls value -> value; _ -> undefined })
                    67 ->
                      Monad.liftM StackValue_cdecls $ cdecls_implies_LBRACE_cdecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cdecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    68 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl actions (case snd (pop !! 0) of { StackValue_cdecl value -> value; _ -> undefined })
                    69 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq actions (case snd (pop !! 2) of { StackValue_cdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecl_seq value -> value; _ -> undefined })
                    70 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    71 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_pat_EQUAL_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    72 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_pat_EQUAL_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    73 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_pat_PIPE_gdrhs actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdrhs value -> value; _ -> undefined })
                    74 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_pat_PIPE_gdrhs_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_gdrhs value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    75 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies actions
                    76 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies_WHERE_idecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls value -> value; _ -> undefined })
                    77 ->
                      Monad.liftM StackValue_idecls $ idecls_implies_LBRACE_idecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_idecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    78 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl actions (case snd (pop !! 0) of { StackValue_idecl value -> value; _ -> undefined })
                    79 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl_SEMICOLON_idecl_seq actions (case snd (pop !! 2) of { StackValue_idecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecl_seq value -> value; _ -> undefined })
                    80 ->
                      Monad.liftM StackValue_idecl $ idecl_implies actions
                    81 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_pat_EQUAL_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    82 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_pat_EQUAL_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    83 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_pat_PIPE_gdrhs actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdrhs value -> value; _ -> undefined })
                    84 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_pat_PIPE_gdrhs_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_gdrhs value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    85 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies actions
                    86 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    87 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    88 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_fixity_integer_opt_ops actions (case snd (pop !! 2) of { StackValue_fixity value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_integer_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    89 ->
                      Monad.liftM StackValue_ops $ ops_implies_MINUS actions (case snd (pop !! 0) of { StackValue_MINUS value -> value; _ -> undefined })
                    90 ->
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    91 ->
                      Monad.liftM StackValue_ops $ ops_implies_MINUS_COMMA_ops actions (case snd (pop !! 2) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    94 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    98 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    101 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_RARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    109 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    112 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    118 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    120 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    128 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass_COMMA_dclass_seq actions (case snd (pop !! 2) of { StackValue_dclass value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass_seq value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    135 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_safety value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_expent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    139 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_impent $ impent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_expent $ expent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_safety $ safety_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_safety $ safety_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_safety $ safety_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    146 ->
                      Monad.liftM StackValue_gdrhs $ gdrhs_implies_guards_EQUAL_exp actions (case snd (pop !! 2) of { StackValue_guards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_gdrhs $ gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs actions (case snd (pop !! 4) of { StackValue_guards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdrhs value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_guards $ guards_implies_guard actions (case snd (pop !! 0) of { StackValue_guard value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_guards $ guards_implies_guard_COMMA_guards actions (case snd (pop !! 2) of { StackValue_guard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_guards value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_guard $ guard_implies_infixexp_LARROW_exp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_guard $ guard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_guard $ guard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_LAMBDA_pat_RARROW_exp actions (case snd (pop !! 3) of { StackValue_LAMBDA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_LET_decls_IN_exp actions (case snd (pop !! 3) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_decls value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_IN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp actions (case snd (pop !! 7) of { StackValue_IF value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_THEN value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ELSE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_MINUS_exp actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_QVARSYM_exp actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_BACKQUOTE_AS_BACKQUOTE_exp actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_exp actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_exp actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_QCONSYM_exp actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_exp actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_LAMBDA_pat_RARROW_infixexp' actions (case snd (pop !! 3) of { StackValue_LAMBDA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_LET_decls_IN_infixexp' actions (case snd (pop !! 3) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_decls value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_IN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_infixexp' actions (case snd (pop !! 7) of { StackValue_IF value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_THEN value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ELSE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_MINUS_infixexp' actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_QVARSYM_infixexp' actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_BACKQUOTE_AS_BACKQUOTE_infixexp' actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_infixexp' actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_infixexp' actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_QCONSYM_infixexp' actions (case snd (pop !! 2) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_infixexp' actions (case snd (pop !! 4) of { StackValue_lexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_infixexp' $ infixexp'_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_MINUS_lexp actions (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE actions (case snd (pop !! 5) of { StackValue_CASE value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_OF value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_alts value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_DO_LBRACE_stmts_RBRACE actions (case snd (pop !! 3) of { StackValue_DO value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_stmts value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_exp_seq $ exp_seq_implies_exp actions (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_exp_seq $ exp_seq_implies_exp_COMMA_exp_seq actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp_seq value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_exp_seq2 $ exp_seq2_implies_exp_COMMA_exp actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_exp_seq2 $ exp_seq2_implies_exp_COMMA_exp_seq2 actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp_seq2 value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_seq_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET actions (case snd (pop !! 3) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET actions (case snd (pop !! 4) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET actions (case snd (pop !! 5) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET actions (case snd (pop !! 6) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    198 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    201 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    203 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    204 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt actions (case snd (pop !! 0) of { StackValue_alt value -> value; _ -> undefined })
                    205 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt_SEMICOLON_alts actions (case snd (pop !! 2) of { StackValue_alt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_alts value -> value; _ -> undefined })
                    206 ->
                      Monad.liftM StackValue_alt $ alt_implies actions
                    207 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    208 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    209 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_PIPE_gdpat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdpat value -> value; _ -> undefined })
                    210 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_PIPE_gdpat_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_gdpat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    211 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp actions (case snd (pop !! 2) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    212 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp_PIPE_gdpat actions (case snd (pop !! 4) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdpat value -> value; _ -> undefined })
                    213 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard actions (case snd (pop !! 0) of { StackValue_patguard value -> value; _ -> undefined })
                    214 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard_COMMA_patguards actions (case snd (pop !! 2) of { StackValue_patguard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_patguards value -> value; _ -> undefined })
                    215 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp'_LARROW_infixexp' actions (case snd (pop !! 2) of { StackValue_infixexp' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    216 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    217 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp' actions (case snd (pop !! 0) of { StackValue_infixexp' value -> value; _ -> undefined })
                    218 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt actions (case snd (pop !! 0) of { StackValue_stmt value -> value; _ -> undefined })
                    219 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt_SEMICOLON_stmts actions (case snd (pop !! 2) of { StackValue_stmt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_stmts value -> value; _ -> undefined })
                    220 ->
                      Monad.liftM StackValue_stmt $ stmt_implies actions
                    221 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    222 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    223 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    224 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    225 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    226 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    227 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    228 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    229 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    230 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    231 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    232 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    233 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    234 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    235 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    236 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    237 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    238 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    239 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    240 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    241 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    242 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    243 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    244 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    245 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    246 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    247 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    248 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    249 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    250 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    251 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    252 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    253 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    254 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    255 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    256 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    257 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies_SEMICOLON actions (case snd (pop !! 0) of { StackValue_SEMICOLON value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_module' value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}


semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { module'_implies_MODULE_modid_exports_opt_WHERE_body = \mODULE0 modid1 exports_opt2 wHERE3 body4 ->
      return $ Module'_implies_MODULE_modid_exports_opt_WHERE_body mODULE0 modid1 exports_opt2 wHERE3 body4
  , module'_implies_body = \body0 ->
      return $ Module'_implies_body body0
  , body_implies_LBRACE_topdecls_RBRACE = \lBRACE0 topdecls1 rBRACE2 ->
      return $ Body_implies_LBRACE_topdecls_RBRACE lBRACE0 topdecls1 rBRACE2
  , exports_opt_implies =
      return $ Exports_opt_implies
  , exports_opt_implies_exports = \exports0 ->
      return $ Exports_opt_implies_exports exports0
  , exports_implies_LPAREN_export_seq_RPAREN = \lPAREN0 export_seq1 rPAREN2 ->
      return $ Exports_implies_LPAREN_export_seq_RPAREN lPAREN0 export_seq1 rPAREN2
  , export_seq_implies =
      return $ Export_seq_implies
  , export_seq_implies_export = \export0 ->
      return $ Export_seq_implies_export export0
  , export_seq_implies_export_COMMA_export_seq = \export0 cOMMA1 export_seq2 ->
      return $ Export_seq_implies_export_COMMA_export_seq export0 cOMMA1 export_seq2
  , export_implies_var = \var0 ->
      return $ Export_implies_var var0
  , export_implies_con = \con0 ->
      return $ Export_implies_con con0
  , export_implies_con_LPAREN_RPAREN = \con0 lPAREN1 rPAREN2 ->
      return $ Export_implies_con_LPAREN_RPAREN con0 lPAREN1 rPAREN2
  , export_implies_con_LPAREN_DOT_DOT_RPAREN = \con0 lPAREN1 dOT_DOT2 rPAREN3 ->
      return $ Export_implies_con_LPAREN_DOT_DOT_RPAREN con0 lPAREN1 dOT_DOT2 rPAREN3
  , export_implies_con_LPAREN_cname_seq_RPAREN = \con0 lPAREN1 cname_seq2 rPAREN3 ->
      return $ Export_implies_con_LPAREN_cname_seq_RPAREN con0 lPAREN1 cname_seq2 rPAREN3
  , export_implies_MODULE_modid = \mODULE0 modid1 ->
      return $ Export_implies_MODULE_modid mODULE0 modid1
  , import_seq_implies =
      return $ Import_seq_implies
  , import_seq_implies_import' = \import'0 ->
      return $ Import_seq_implies_import' import'0
  , import_seq_implies_import'_COMMA_import_seq = \import'0 cOMMA1 import_seq2 ->
      return $ Import_seq_implies_import'_COMMA_import_seq import'0 cOMMA1 import_seq2
  , import'_implies_var = \var0 ->
      return $ Import'_implies_var var0
  , import'_implies_con = \con0 ->
      return $ Import'_implies_con con0
  , import'_implies_con_LPAREN_RPAREN = \con0 lPAREN1 rPAREN2 ->
      return $ Import'_implies_con_LPAREN_RPAREN con0 lPAREN1 rPAREN2
  , import'_implies_con_LPAREN_DOT_DOT_RPAREN = \con0 lPAREN1 dOT_DOT2 rPAREN3 ->
      return $ Import'_implies_con_LPAREN_DOT_DOT_RPAREN con0 lPAREN1 dOT_DOT2 rPAREN3
  , import'_implies_con_LPAREN_cname_seq_RPAREN = \con0 lPAREN1 cname_seq2 rPAREN3 ->
      return $ Import'_implies_con_LPAREN_cname_seq_RPAREN con0 lPAREN1 cname_seq2 rPAREN3
  , cname_seq_implies_cname = \cname0 ->
      return $ Cname_seq_implies_cname cname0
  , cname_seq_implies_cname_COMMA_cname_seq = \cname0 cOMMA1 cname_seq2 ->
      return $ Cname_seq_implies_cname_COMMA_cname_seq cname0 cOMMA1 cname_seq2
  , cname_implies_var = \var0 ->
      return $ Cname_implies_var var0
  , cname_implies_con = \con0 ->
      return $ Cname_implies_con con0
  , topdecls_implies_topdecl = \topdecl0 ->
      return $ Topdecls_implies_topdecl topdecl0
  , topdecls_implies_topdecl_SEMICOLON_topdecls = \topdecl0 sEMICOLON1 topdecls2 ->
      return $ Topdecls_implies_topdecl_SEMICOLON_topdecls topdecl0 sEMICOLON1 topdecls2
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt = \iMPORT0 qualified_opt1 modid2 as_opt3 ->
      return $ Topdecl_implies_IMPORT_qualified_opt_modid_as_opt iMPORT0 qualified_opt1 modid2 as_opt3
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_LPAREN_import_seq_RPAREN = \iMPORT0 qualified_opt1 modid2 as_opt3 lPAREN4 import_seq5 rPAREN6 ->
      return $ Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_LPAREN_import_seq_RPAREN iMPORT0 qualified_opt1 modid2 as_opt3 lPAREN4 import_seq5 rPAREN6
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_HIDING_LPAREN_import_seq_RPAREN = \iMPORT0 qualified_opt1 modid2 as_opt3 hIDING4 lPAREN5 import_seq6 rPAREN7 ->
      return $ Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_HIDING_LPAREN_import_seq_RPAREN iMPORT0 qualified_opt1 modid2 as_opt3 hIDING4 lPAREN5 import_seq6 rPAREN7
  , topdecl_implies_TYPE_btype_EQUAL_type' = \tYPE0 btype1 eQUAL2 type'3 ->
      return $ Topdecl_implies_TYPE_btype_EQUAL_type' tYPE0 btype1 eQUAL2 type'3
  , topdecl_implies_DATA_btype_constrs_opt = \dATA0 btype1 constrs_opt2 ->
      return $ Topdecl_implies_DATA_btype_constrs_opt dATA0 btype1 constrs_opt2
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass = \dATA0 btype1 constrs_opt2 dERIVING3 dclass4 ->
      return $ Topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass dATA0 btype1 constrs_opt2 dERIVING3 dclass4
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN = \dATA0 btype1 constrs_opt2 dERIVING3 lPAREN4 rPAREN5 ->
      return $ Topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN dATA0 btype1 constrs_opt2 dERIVING3 lPAREN4 rPAREN5
  , topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN = \dATA0 btype1 constrs_opt2 dERIVING3 lPAREN4 dclass_seq5 rPAREN6 ->
      return $ Topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN dATA0 btype1 constrs_opt2 dERIVING3 lPAREN4 dclass_seq5 rPAREN6
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt = \dATA0 btype1 dARROW2 btype3 constrs_opt4 ->
      return $ Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt dATA0 btype1 dARROW2 btype3 constrs_opt4
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass = \dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 dclass6 ->
      return $ Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 dclass6
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN = \dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 lPAREN6 rPAREN7 ->
      return $ Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 lPAREN6 rPAREN7
  , topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN = \dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 lPAREN6 dclass_seq7 rPAREN8 ->
      return $ Topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN dATA0 btype1 dARROW2 btype3 constrs_opt4 dERIVING5 lPAREN6 dclass_seq7 rPAREN8
  , topdecl_implies_NEWTYPE_btype_newconstr = \nEWTYPE0 btype1 newconstr2 ->
      return $ Topdecl_implies_NEWTYPE_btype_newconstr nEWTYPE0 btype1 newconstr2
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass = \nEWTYPE0 btype1 newconstr2 dERIVING3 dclass4 ->
      return $ Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass nEWTYPE0 btype1 newconstr2 dERIVING3 dclass4
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN = \nEWTYPE0 btype1 newconstr2 dERIVING3 lPAREN4 rPAREN5 ->
      return $ Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN nEWTYPE0 btype1 newconstr2 dERIVING3 lPAREN4 rPAREN5
  , topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN = \nEWTYPE0 btype1 newconstr2 dERIVING3 lPAREN4 dclass_seq5 rPAREN6 ->
      return $ Topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN nEWTYPE0 btype1 newconstr2 dERIVING3 lPAREN4 dclass_seq5 rPAREN6
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr = \nEWTYPE0 btype1 dARROW2 btype3 newconstr4 ->
      return $ Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr nEWTYPE0 btype1 dARROW2 btype3 newconstr4
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass = \nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 dclass6 ->
      return $ Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 dclass6
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN = \nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 lPAREN6 rPAREN7 ->
      return $ Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 lPAREN6 rPAREN7
  , topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN = \nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 lPAREN6 dclass_seq7 rPAREN8 ->
      return $ Topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN nEWTYPE0 btype1 dARROW2 btype3 newconstr4 dERIVING5 lPAREN6 dclass_seq7 rPAREN8
  , topdecl_implies_CLASS_btype_cdecls_opt = \cLASS0 btype1 cdecls_opt2 ->
      return $ Topdecl_implies_CLASS_btype_cdecls_opt cLASS0 btype1 cdecls_opt2
  , topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt = \cLASS0 btype1 dARROW2 btype3 cdecls_opt4 ->
      return $ Topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt cLASS0 btype1 dARROW2 btype3 cdecls_opt4
  , topdecl_implies_INSTANCE_btype_idecls_opt = \iNSTANCE0 btype1 idecls_opt2 ->
      return $ Topdecl_implies_INSTANCE_btype_idecls_opt iNSTANCE0 btype1 idecls_opt2
  , topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt = \iNSTANCE0 btype1 dARROW2 btype3 idecls_opt4 ->
      return $ Topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt iNSTANCE0 btype1 dARROW2 btype3 idecls_opt4
  , topdecl_implies_DEFAULT_LPAREN_RPAREN = \dEFAULT0 lPAREN1 rPAREN2 ->
      return $ Topdecl_implies_DEFAULT_LPAREN_RPAREN dEFAULT0 lPAREN1 rPAREN2
  , topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN = \dEFAULT0 lPAREN1 type_seq2 rPAREN3 ->
      return $ Topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN dEFAULT0 lPAREN1 type_seq2 rPAREN3
  , topdecl_implies_FOREIGN_fdecl = \fOREIGN0 fdecl1 ->
      return $ Topdecl_implies_FOREIGN_fdecl fOREIGN0 fdecl1
  , topdecl_implies_decl = \decl0 ->
      return $ Topdecl_implies_decl decl0
  , decls_implies_LBRACE_decl_seq_RBRACE = \lBRACE0 decl_seq1 rBRACE2 ->
      return $ Decls_implies_LBRACE_decl_seq_RBRACE lBRACE0 decl_seq1 rBRACE2
  , decl_seq_implies_decl = \decl0 ->
      return $ Decl_seq_implies_decl decl0
  , decl_seq_implies_decl_SEMICOLON_decl_seq = \decl0 sEMICOLON1 decl_seq2 ->
      return $ Decl_seq_implies_decl_SEMICOLON_decl_seq decl0 sEMICOLON1 decl_seq2
  , decl_implies_gendecl = \gendecl0 ->
      return $ Decl_implies_gendecl gendecl0
  , decl_implies_pat_EQUAL_exp = \pat0 eQUAL1 exp2 ->
      return $ Decl_implies_pat_EQUAL_exp pat0 eQUAL1 exp2
  , decl_implies_pat_EQUAL_exp_WHERE_decls = \pat0 eQUAL1 exp2 wHERE3 decls4 ->
      return $ Decl_implies_pat_EQUAL_exp_WHERE_decls pat0 eQUAL1 exp2 wHERE3 decls4
  , decl_implies_pat_PIPE_gdrhs = \pat0 pIPE1 gdrhs2 ->
      return $ Decl_implies_pat_PIPE_gdrhs pat0 pIPE1 gdrhs2
  , decl_implies_pat_PIPE_gdrhs_WHERE_decls = \pat0 pIPE1 gdrhs2 wHERE3 decls4 ->
      return $ Decl_implies_pat_PIPE_gdrhs_WHERE_decls pat0 pIPE1 gdrhs2 wHERE3 decls4
  , cdecls_opt_implies =
      return $ Cdecls_opt_implies
  , cdecls_opt_implies_WHERE_cdecls = \wHERE0 cdecls1 ->
      return $ Cdecls_opt_implies_WHERE_cdecls wHERE0 cdecls1
  , cdecls_implies_LBRACE_cdecl_seq_RBRACE = \lBRACE0 cdecl_seq1 rBRACE2 ->
      return $ Cdecls_implies_LBRACE_cdecl_seq_RBRACE lBRACE0 cdecl_seq1 rBRACE2
  , cdecl_seq_implies_cdecl = \cdecl0 ->
      return $ Cdecl_seq_implies_cdecl cdecl0
  , cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq = \cdecl0 sEMICOLON1 cdecl_seq2 ->
      return $ Cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq cdecl0 sEMICOLON1 cdecl_seq2
  , cdecl_implies_gendecl = \gendecl0 ->
      return $ Cdecl_implies_gendecl gendecl0
  , cdecl_implies_pat_EQUAL_exp = \pat0 eQUAL1 exp2 ->
      return $ Cdecl_implies_pat_EQUAL_exp pat0 eQUAL1 exp2
  , cdecl_implies_pat_EQUAL_exp_WHERE_decls = \pat0 eQUAL1 exp2 wHERE3 decls4 ->
      return $ Cdecl_implies_pat_EQUAL_exp_WHERE_decls pat0 eQUAL1 exp2 wHERE3 decls4
  , cdecl_implies_pat_PIPE_gdrhs = \pat0 pIPE1 gdrhs2 ->
      return $ Cdecl_implies_pat_PIPE_gdrhs pat0 pIPE1 gdrhs2
  , cdecl_implies_pat_PIPE_gdrhs_WHERE_decls = \pat0 pIPE1 gdrhs2 wHERE3 decls4 ->
      return $ Cdecl_implies_pat_PIPE_gdrhs_WHERE_decls pat0 pIPE1 gdrhs2 wHERE3 decls4
  , idecls_opt_implies =
      return $ Idecls_opt_implies
  , idecls_opt_implies_WHERE_idecls = \wHERE0 idecls1 ->
      return $ Idecls_opt_implies_WHERE_idecls wHERE0 idecls1
  , idecls_implies_LBRACE_idecl_seq_RBRACE = \lBRACE0 idecl_seq1 rBRACE2 ->
      return $ Idecls_implies_LBRACE_idecl_seq_RBRACE lBRACE0 idecl_seq1 rBRACE2
  , idecl_seq_implies_idecl = \idecl0 ->
      return $ Idecl_seq_implies_idecl idecl0
  , idecl_seq_implies_idecl_SEMICOLON_idecl_seq = \idecl0 sEMICOLON1 idecl_seq2 ->
      return $ Idecl_seq_implies_idecl_SEMICOLON_idecl_seq idecl0 sEMICOLON1 idecl_seq2
  , idecl_implies =
      return $ Idecl_implies
  , idecl_implies_pat_EQUAL_exp = \pat0 eQUAL1 exp2 ->
      return $ Idecl_implies_pat_EQUAL_exp pat0 eQUAL1 exp2
  , idecl_implies_pat_EQUAL_exp_WHERE_decls = \pat0 eQUAL1 exp2 wHERE3 decls4 ->
      return $ Idecl_implies_pat_EQUAL_exp_WHERE_decls pat0 eQUAL1 exp2 wHERE3 decls4
  , idecl_implies_pat_PIPE_gdrhs = \pat0 pIPE1 gdrhs2 ->
      return $ Idecl_implies_pat_PIPE_gdrhs pat0 pIPE1 gdrhs2
  , idecl_implies_pat_PIPE_gdrhs_WHERE_decls = \pat0 pIPE1 gdrhs2 wHERE3 decls4 ->
      return $ Idecl_implies_pat_PIPE_gdrhs_WHERE_decls pat0 pIPE1 gdrhs2 wHERE3 decls4
  , gendecl_implies =
      return $ Gendecl_implies
  , gendecl_implies_vars_COLON_COLON_type' = \vars0 cOLON_COLON1 type'2 ->
      return $ Gendecl_implies_vars_COLON_COLON_type' vars0 cOLON_COLON1 type'2
  , gendecl_implies_vars_COLON_COLON_btype_DARROW_type' = \vars0 cOLON_COLON1 btype2 dARROW3 type'4 ->
      return $ Gendecl_implies_vars_COLON_COLON_btype_DARROW_type' vars0 cOLON_COLON1 btype2 dARROW3 type'4
  , gendecl_implies_fixity_integer_opt_ops = \fixity0 integer_opt1 ops2 ->
      return $ Gendecl_implies_fixity_integer_opt_ops fixity0 integer_opt1 ops2
  , ops_implies_MINUS = \mINUS0 ->
      return $ Ops_implies_MINUS mINUS0
  , ops_implies_op = \op0 ->
      return $ Ops_implies_op op0
  , ops_implies_MINUS_COMMA_ops = \mINUS0 cOMMA1 ops2 ->
      return $ Ops_implies_MINUS_COMMA_ops mINUS0 cOMMA1 ops2
  , ops_implies_op_COMMA_ops = \op0 cOMMA1 ops2 ->
      return $ Ops_implies_op_COMMA_ops op0 cOMMA1 ops2
  , vars_implies_var = \var0 ->
      return $ Vars_implies_var var0
  , vars_implies_var_COMMA_vars = \var0 cOMMA1 vars2 ->
      return $ Vars_implies_var_COMMA_vars var0 cOMMA1 vars2
  , fixity_implies_INFIXL = \iNFIXL0 ->
      return $ Fixity_implies_INFIXL iNFIXL0
  , fixity_implies_INFIXR = \iNFIXR0 ->
      return $ Fixity_implies_INFIXR iNFIXR0
  , fixity_implies_INFIX = \iNFIX0 ->
      return $ Fixity_implies_INFIX iNFIX0
  , type_seq_implies_type' = \type'0 ->
      return $ Type_seq_implies_type' type'0
  , type_seq_implies_type'_COMMA_type_seq = \type'0 cOMMA1 type_seq2 ->
      return $ Type_seq_implies_type'_COMMA_type_seq type'0 cOMMA1 type_seq2
  , type'_implies_btype = \btype0 ->
      return $ Type'_implies_btype btype0
  , type'_implies_btype_RARROW_type' = \btype0 rARROW1 type'2 ->
      return $ Type'_implies_btype_RARROW_type' btype0 rARROW1 type'2
  , btype_implies_atype = \atype0 ->
      return $ Btype_implies_atype atype0
  , btype_implies_btype_atype = \btype0 atype1 ->
      return $ Btype_implies_btype_atype btype0 atype1
  , atype_implies_gtycon = \gtycon0 ->
      return $ Atype_implies_gtycon gtycon0
  , atype_implies_tyvar = \tyvar0 ->
      return $ Atype_implies_tyvar tyvar0
  , atype_implies_LPAREN_type_seq2_RPAREN = \lPAREN0 type_seq21 rPAREN2 ->
      return $ Atype_implies_LPAREN_type_seq2_RPAREN lPAREN0 type_seq21 rPAREN2
  , atype_implies_LBRACKET_type'_RBRACKET = \lBRACKET0 type'1 rBRACKET2 ->
      return $ Atype_implies_LBRACKET_type'_RBRACKET lBRACKET0 type'1 rBRACKET2
  , atype_implies_LPAREN_type'_RPAREN = \lPAREN0 type'1 rPAREN2 ->
      return $ Atype_implies_LPAREN_type'_RPAREN lPAREN0 type'1 rPAREN2
  , atype_implies_EXCL_atype = \eXCL0 atype1 ->
      return $ Atype_implies_EXCL_atype eXCL0 atype1
  , type_seq2_implies_type'_COMMA_type' = \type'0 cOMMA1 type'2 ->
      return $ Type_seq2_implies_type'_COMMA_type' type'0 cOMMA1 type'2
  , type_seq2_implies_type'_COMMA_type_seq2 = \type'0 cOMMA1 type_seq22 ->
      return $ Type_seq2_implies_type'_COMMA_type_seq2 type'0 cOMMA1 type_seq22
  , gtycon_implies_con = \con0 ->
      return $ Gtycon_implies_con con0
  , gtycon_implies_LPAREN_RPAREN = \lPAREN0 rPAREN1 ->
      return $ Gtycon_implies_LPAREN_RPAREN lPAREN0 rPAREN1
  , gtycon_implies_LBRACKET_RBRACKET = \lBRACKET0 rBRACKET1 ->
      return $ Gtycon_implies_LBRACKET_RBRACKET lBRACKET0 rBRACKET1
  , gtycon_implies_LPAREN_RARROW_RPAREN = \lPAREN0 rARROW1 rPAREN2 ->
      return $ Gtycon_implies_LPAREN_RARROW_RPAREN lPAREN0 rARROW1 rPAREN2
  , gtycon_implies_LPAREN_comma_list_RPAREN = \lPAREN0 comma_list1 rPAREN2 ->
      return $ Gtycon_implies_LPAREN_comma_list_RPAREN lPAREN0 comma_list1 rPAREN2
  , comma_list_implies_COMMA = \cOMMA0 ->
      return $ Comma_list_implies_COMMA cOMMA0
  , comma_list_implies_COMMA_comma_list = \cOMMA0 comma_list1 ->
      return $ Comma_list_implies_COMMA_comma_list cOMMA0 comma_list1
  , constrs_opt_implies =
      return $ Constrs_opt_implies
  , constrs_opt_implies_EQUAL_constrs = \eQUAL0 constrs1 ->
      return $ Constrs_opt_implies_EQUAL_constrs eQUAL0 constrs1
  , constrs_implies_constr = \constr0 ->
      return $ Constrs_implies_constr constr0
  , constrs_implies_constr_PIPE_constrs = \constr0 pIPE1 constrs2 ->
      return $ Constrs_implies_constr_PIPE_constrs constr0 pIPE1 constrs2
  , constr_implies_btype = \btype0 ->
      return $ Constr_implies_btype btype0
  , constr_implies_btype_conop_btype = \btype0 conop1 btype2 ->
      return $ Constr_implies_btype_conop_btype btype0 conop1 btype2
  , constr_implies_con_LBRACE_RBRACE = \con0 lBRACE1 rBRACE2 ->
      return $ Constr_implies_con_LBRACE_RBRACE con0 lBRACE1 rBRACE2
  , constr_implies_con_LBRACE_fielddecl_seq_RBRACE = \con0 lBRACE1 fielddecl_seq2 rBRACE3 ->
      return $ Constr_implies_con_LBRACE_fielddecl_seq_RBRACE con0 lBRACE1 fielddecl_seq2 rBRACE3
  , newconstr_implies_EQUAL_con_atype = \eQUAL0 con1 atype2 ->
      return $ Newconstr_implies_EQUAL_con_atype eQUAL0 con1 atype2
  , newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE = \eQUAL0 con1 lBRACE2 var3 cOLON_COLON4 type'5 rBRACE6 ->
      return $ Newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE eQUAL0 con1 lBRACE2 var3 cOLON_COLON4 type'5 rBRACE6
  , fielddecl_seq_implies_fielddecl = \fielddecl0 ->
      return $ Fielddecl_seq_implies_fielddecl fielddecl0
  , fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq = \fielddecl0 cOMMA1 fielddecl_seq2 ->
      return $ Fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq fielddecl0 cOMMA1 fielddecl_seq2
  , fielddecl_implies_vars_COLON_COLON_type' = \vars0 cOLON_COLON1 type'2 ->
      return $ Fielddecl_implies_vars_COLON_COLON_type' vars0 cOLON_COLON1 type'2
  , dclass_seq_implies_dclass = \dclass0 ->
      return $ Dclass_seq_implies_dclass dclass0
  , dclass_seq_implies_dclass_COMMA_dclass_seq = \dclass0 cOMMA1 dclass_seq2 ->
      return $ Dclass_seq_implies_dclass_COMMA_dclass_seq dclass0 cOMMA1 dclass_seq2
  , dclass_implies_QCONID = \qCONID0 ->
      return $ Dclass_implies_QCONID qCONID0
  , fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' = \iMPORT0 callconv1 impent2 var3 cOLON_COLON4 type'5 ->
      return $ Fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' iMPORT0 callconv1 impent2 var3 cOLON_COLON4 type'5
  , fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' = \iMPORT0 callconv1 safety2 impent3 var4 cOLON_COLON5 type'6 ->
      return $ Fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' iMPORT0 callconv1 safety2 impent3 var4 cOLON_COLON5 type'6
  , fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' = \eXPORT0 callconv1 expent2 var3 cOLON_COLON4 type'5 ->
      return $ Fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' eXPORT0 callconv1 expent2 var3 cOLON_COLON4 type'5
  , callconv_implies_AS = \aS0 ->
      return $ Callconv_implies_AS aS0
  , callconv_implies_EXPORT = \eXPORT0 ->
      return $ Callconv_implies_EXPORT eXPORT0
  , callconv_implies_QVARID = \qVARID0 ->
      return $ Callconv_implies_QVARID qVARID0
  , impent_implies_STRING = \sTRING0 ->
      return $ Impent_implies_STRING sTRING0
  , expent_implies_STRING = \sTRING0 ->
      return $ Expent_implies_STRING sTRING0
  , safety_implies_AS = \aS0 ->
      return $ Safety_implies_AS aS0
  , safety_implies_EXPORT = \eXPORT0 ->
      return $ Safety_implies_EXPORT eXPORT0
  , safety_implies_QVARID = \qVARID0 ->
      return $ Safety_implies_QVARID qVARID0
  , gdrhs_implies_guards_EQUAL_exp = \guards0 eQUAL1 exp2 ->
      return $ Gdrhs_implies_guards_EQUAL_exp guards0 eQUAL1 exp2
  , gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs = \guards0 eQUAL1 exp2 pIPE3 gdrhs4 ->
      return $ Gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs guards0 eQUAL1 exp2 pIPE3 gdrhs4
  , guards_implies_guard = \guard0 ->
      return $ Guards_implies_guard guard0
  , guards_implies_guard_COMMA_guards = \guard0 cOMMA1 guards2 ->
      return $ Guards_implies_guard_COMMA_guards guard0 cOMMA1 guards2
  , guard_implies_infixexp_LARROW_exp = \infixexp0 lARROW1 exp2 ->
      return $ Guard_implies_infixexp_LARROW_exp infixexp0 lARROW1 exp2
  , guard_implies_LET_decls = \lET0 decls1 ->
      return $ Guard_implies_LET_decls lET0 decls1
  , guard_implies_infixexp = \infixexp0 ->
      return $ Guard_implies_infixexp infixexp0
  , exp_implies_infixexp = \infixexp0 ->
      return $ Exp_implies_infixexp infixexp0
  , infixexp_implies_LAMBDA_pat_RARROW_exp = \lAMBDA0 pat1 rARROW2 exp3 ->
      return $ Infixexp_implies_LAMBDA_pat_RARROW_exp lAMBDA0 pat1 rARROW2 exp3
  , infixexp_implies_LET_decls_IN_exp = \lET0 decls1 iN2 exp3 ->
      return $ Infixexp_implies_LET_decls_IN_exp lET0 decls1 iN2 exp3
  , infixexp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp = \iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 exp7 ->
      return $ Infixexp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 exp7
  , infixexp_implies_lexp_MINUS_exp = \lexp0 mINUS1 exp2 ->
      return $ Infixexp_implies_lexp_MINUS_exp lexp0 mINUS1 exp2
  , infixexp_implies_lexp_QVARSYM_exp = \lexp0 qVARSYM1 exp2 ->
      return $ Infixexp_implies_lexp_QVARSYM_exp lexp0 qVARSYM1 exp2
  , infixexp_implies_lexp_BACKQUOTE_AS_BACKQUOTE_exp = \lexp0 bACKQUOTE1 aS2 bACKQUOTE3 exp4 ->
      return $ Infixexp_implies_lexp_BACKQUOTE_AS_BACKQUOTE_exp lexp0 bACKQUOTE1 aS2 bACKQUOTE3 exp4
  , infixexp_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_exp = \lexp0 bACKQUOTE1 eXPORT2 bACKQUOTE3 exp4 ->
      return $ Infixexp_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_exp lexp0 bACKQUOTE1 eXPORT2 bACKQUOTE3 exp4
  , infixexp_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_exp = \lexp0 bACKQUOTE1 qVARID2 bACKQUOTE3 exp4 ->
      return $ Infixexp_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_exp lexp0 bACKQUOTE1 qVARID2 bACKQUOTE3 exp4
  , infixexp_implies_lexp_QCONSYM_exp = \lexp0 qCONSYM1 exp2 ->
      return $ Infixexp_implies_lexp_QCONSYM_exp lexp0 qCONSYM1 exp2
  , infixexp_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_exp = \lexp0 bACKQUOTE1 qCONID2 bACKQUOTE3 exp4 ->
      return $ Infixexp_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_exp lexp0 bACKQUOTE1 qCONID2 bACKQUOTE3 exp4
  , infixexp_implies_lexp_COLON_COLON_type' = \lexp0 cOLON_COLON1 type'2 ->
      return $ Infixexp_implies_lexp_COLON_COLON_type' lexp0 cOLON_COLON1 type'2
  , infixexp_implies_lexp_COLON_COLON_btype_DARROW_type' = \lexp0 cOLON_COLON1 btype2 dARROW3 type'4 ->
      return $ Infixexp_implies_lexp_COLON_COLON_btype_DARROW_type' lexp0 cOLON_COLON1 btype2 dARROW3 type'4
  , infixexp_implies_lexp = \lexp0 ->
      return $ Infixexp_implies_lexp lexp0
  , infixexp'_implies_LAMBDA_pat_RARROW_infixexp' = \lAMBDA0 pat1 rARROW2 infixexp'3 ->
      return $ Infixexp'_implies_LAMBDA_pat_RARROW_infixexp' lAMBDA0 pat1 rARROW2 infixexp'3
  , infixexp'_implies_LET_decls_IN_infixexp' = \lET0 decls1 iN2 infixexp'3 ->
      return $ Infixexp'_implies_LET_decls_IN_infixexp' lET0 decls1 iN2 infixexp'3
  , infixexp'_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_infixexp' = \iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 infixexp'7 ->
      return $ Infixexp'_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_infixexp' iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 infixexp'7
  , infixexp'_implies_lexp_MINUS_infixexp' = \lexp0 mINUS1 infixexp'2 ->
      return $ Infixexp'_implies_lexp_MINUS_infixexp' lexp0 mINUS1 infixexp'2
  , infixexp'_implies_lexp_QVARSYM_infixexp' = \lexp0 qVARSYM1 infixexp'2 ->
      return $ Infixexp'_implies_lexp_QVARSYM_infixexp' lexp0 qVARSYM1 infixexp'2
  , infixexp'_implies_lexp_BACKQUOTE_AS_BACKQUOTE_infixexp' = \lexp0 bACKQUOTE1 aS2 bACKQUOTE3 infixexp'4 ->
      return $ Infixexp'_implies_lexp_BACKQUOTE_AS_BACKQUOTE_infixexp' lexp0 bACKQUOTE1 aS2 bACKQUOTE3 infixexp'4
  , infixexp'_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_infixexp' = \lexp0 bACKQUOTE1 eXPORT2 bACKQUOTE3 infixexp'4 ->
      return $ Infixexp'_implies_lexp_BACKQUOTE_EXPORT_BACKQUOTE_infixexp' lexp0 bACKQUOTE1 eXPORT2 bACKQUOTE3 infixexp'4
  , infixexp'_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_infixexp' = \lexp0 bACKQUOTE1 qVARID2 bACKQUOTE3 infixexp'4 ->
      return $ Infixexp'_implies_lexp_BACKQUOTE_QVARID_BACKQUOTE_infixexp' lexp0 bACKQUOTE1 qVARID2 bACKQUOTE3 infixexp'4
  , infixexp'_implies_lexp_QCONSYM_infixexp' = \lexp0 qCONSYM1 infixexp'2 ->
      return $ Infixexp'_implies_lexp_QCONSYM_infixexp' lexp0 qCONSYM1 infixexp'2
  , infixexp'_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_infixexp' = \lexp0 bACKQUOTE1 qCONID2 bACKQUOTE3 infixexp'4 ->
      return $ Infixexp'_implies_lexp_BACKQUOTE_QCONID_BACKQUOTE_infixexp' lexp0 bACKQUOTE1 qCONID2 bACKQUOTE3 infixexp'4
  , infixexp'_implies_lexp = \lexp0 ->
      return $ Infixexp'_implies_lexp lexp0
  , lexp_implies_MINUS_lexp = \mINUS0 lexp1 ->
      return $ Lexp_implies_MINUS_lexp mINUS0 lexp1
  , lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE = \cASE0 exp1 oF2 lBRACE3 alts4 rBRACE5 ->
      return $ Lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE cASE0 exp1 oF2 lBRACE3 alts4 rBRACE5
  , lexp_implies_DO_LBRACE_stmts_RBRACE = \dO0 lBRACE1 stmts2 rBRACE3 ->
      return $ Lexp_implies_DO_LBRACE_stmts_RBRACE dO0 lBRACE1 stmts2 rBRACE3
  , lexp_implies_fexp = \fexp0 ->
      return $ Lexp_implies_fexp fexp0
  , fexp_implies_aexp = \aexp0 ->
      return $ Fexp_implies_aexp aexp0
  , fexp_implies_fexp_aexp = \fexp0 aexp1 ->
      return $ Fexp_implies_fexp_aexp fexp0 aexp1
  , exp_seq_implies_exp = \exp0 ->
      return $ Exp_seq_implies_exp exp0
  , exp_seq_implies_exp_COMMA_exp_seq = \exp0 cOMMA1 exp_seq2 ->
      return $ Exp_seq_implies_exp_COMMA_exp_seq exp0 cOMMA1 exp_seq2
  , exp_seq2_implies_exp_COMMA_exp = \exp0 cOMMA1 exp2 ->
      return $ Exp_seq2_implies_exp_COMMA_exp exp0 cOMMA1 exp2
  , exp_seq2_implies_exp_COMMA_exp_seq2 = \exp0 cOMMA1 exp_seq22 ->
      return $ Exp_seq2_implies_exp_COMMA_exp_seq2 exp0 cOMMA1 exp_seq22
  , aexp_implies_var = \var0 ->
      return $ Aexp_implies_var var0
  , aexp_implies_INTEGER = \iNTEGER0 ->
      return $ Aexp_implies_INTEGER iNTEGER0
  , aexp_implies_STRING = \sTRING0 ->
      return $ Aexp_implies_STRING sTRING0
  , aexp_implies_LPAREN_exp_RPAREN = \lPAREN0 exp1 rPAREN2 ->
      return $ Aexp_implies_LPAREN_exp_RPAREN lPAREN0 exp1 rPAREN2
  , aexp_implies_LPAREN_exp_seq2_RPAREN = \lPAREN0 exp_seq21 rPAREN2 ->
      return $ Aexp_implies_LPAREN_exp_seq2_RPAREN lPAREN0 exp_seq21 rPAREN2
  , aexp_implies_LBRACKET_exp_seq_RBRACKET = \lBRACKET0 exp_seq1 rBRACKET2 ->
      return $ Aexp_implies_LBRACKET_exp_seq_RBRACKET lBRACKET0 exp_seq1 rBRACKET2
  , aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET = \lBRACKET0 exp1 dOT_DOT2 rBRACKET3 ->
      return $ Aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET lBRACKET0 exp1 dOT_DOT2 rBRACKET3
  , aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET = \lBRACKET0 exp1 dOT_DOT2 exp3 rBRACKET4 ->
      return $ Aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET lBRACKET0 exp1 dOT_DOT2 exp3 rBRACKET4
  , aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET = \lBRACKET0 exp1 cOMMA2 exp3 dOT_DOT4 rBRACKET5 ->
      return $ Aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET lBRACKET0 exp1 cOMMA2 exp3 dOT_DOT4 rBRACKET5
  , aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET = \lBRACKET0 exp1 cOMMA2 exp3 dOT_DOT4 exp5 rBRACKET6 ->
      return $ Aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET lBRACKET0 exp1 cOMMA2 exp3 dOT_DOT4 exp5 rBRACKET6
  , aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN = \lPAREN0 qVARSYM1 infixexp2 rPAREN3 ->
      return $ Aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN lPAREN0 qVARSYM1 infixexp2 rPAREN3
  , aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN = \lPAREN0 bACKQUOTE1 aS2 bACKQUOTE3 infixexp4 rPAREN5 ->
      return $ Aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN lPAREN0 bACKQUOTE1 aS2 bACKQUOTE3 infixexp4 rPAREN5
  , aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN = \lPAREN0 bACKQUOTE1 eXPORT2 bACKQUOTE3 infixexp4 rPAREN5 ->
      return $ Aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN lPAREN0 bACKQUOTE1 eXPORT2 bACKQUOTE3 infixexp4 rPAREN5
  , aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN = \lPAREN0 bACKQUOTE1 qVARID2 bACKQUOTE3 infixexp4 rPAREN5 ->
      return $ Aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN lPAREN0 bACKQUOTE1 qVARID2 bACKQUOTE3 infixexp4 rPAREN5
  , aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN = \lPAREN0 qCONSYM1 infixexp2 rPAREN3 ->
      return $ Aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN lPAREN0 qCONSYM1 infixexp2 rPAREN3
  , aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN = \lPAREN0 bACKQUOTE1 qCONID2 bACKQUOTE3 infixexp4 rPAREN5 ->
      return $ Aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN lPAREN0 bACKQUOTE1 qCONID2 bACKQUOTE3 infixexp4 rPAREN5
  , alts_implies_alt = \alt0 ->
      return $ Alts_implies_alt alt0
  , alts_implies_alt_SEMICOLON_alts = \alt0 sEMICOLON1 alts2 ->
      return $ Alts_implies_alt_SEMICOLON_alts alt0 sEMICOLON1 alts2
  , alt_implies =
      return $ Alt_implies
  , alt_implies_pat_RARROW_exp = \pat0 rARROW1 exp2 ->
      return $ Alt_implies_pat_RARROW_exp pat0 rARROW1 exp2
  , alt_implies_pat_RARROW_exp_WHERE_decls = \pat0 rARROW1 exp2 wHERE3 decls4 ->
      return $ Alt_implies_pat_RARROW_exp_WHERE_decls pat0 rARROW1 exp2 wHERE3 decls4
  , alt_implies_pat_PIPE_gdpat = \pat0 pIPE1 gdpat2 ->
      return $ Alt_implies_pat_PIPE_gdpat pat0 pIPE1 gdpat2
  , alt_implies_pat_PIPE_gdpat_WHERE_decls = \pat0 pIPE1 gdpat2 wHERE3 decls4 ->
      return $ Alt_implies_pat_PIPE_gdpat_WHERE_decls pat0 pIPE1 gdpat2 wHERE3 decls4
  , gdpat_implies_patguards_RARROW_exp = \patguards0 rARROW1 exp2 ->
      return $ Gdpat_implies_patguards_RARROW_exp patguards0 rARROW1 exp2
  , gdpat_implies_patguards_RARROW_exp_PIPE_gdpat = \patguards0 rARROW1 exp2 pIPE3 gdpat4 ->
      return $ Gdpat_implies_patguards_RARROW_exp_PIPE_gdpat patguards0 rARROW1 exp2 pIPE3 gdpat4
  , patguards_implies_patguard = \patguard0 ->
      return $ Patguards_implies_patguard patguard0
  , patguards_implies_patguard_COMMA_patguards = \patguard0 cOMMA1 patguards2 ->
      return $ Patguards_implies_patguard_COMMA_patguards patguard0 cOMMA1 patguards2
  , patguard_implies_infixexp'_LARROW_infixexp' = \infixexp'0 lARROW1 infixexp'2 ->
      return $ Patguard_implies_infixexp'_LARROW_infixexp' infixexp'0 lARROW1 infixexp'2
  , patguard_implies_LET_decls = \lET0 decls1 ->
      return $ Patguard_implies_LET_decls lET0 decls1
  , patguard_implies_infixexp' = \infixexp'0 ->
      return $ Patguard_implies_infixexp' infixexp'0
  , stmts_implies_stmt = \stmt0 ->
      return $ Stmts_implies_stmt stmt0
  , stmts_implies_stmt_SEMICOLON_stmts = \stmt0 sEMICOLON1 stmts2 ->
      return $ Stmts_implies_stmt_SEMICOLON_stmts stmt0 sEMICOLON1 stmts2
  , stmt_implies =
      return $ Stmt_implies
  , stmt_implies_infixexp = \infixexp0 ->
      return $ Stmt_implies_infixexp infixexp0
  , stmt_implies_infixexp_LARROW_infixexp = \infixexp0 lARROW1 infixexp2 ->
      return $ Stmt_implies_infixexp_LARROW_infixexp infixexp0 lARROW1 infixexp2
  , stmt_implies_LET_decls = \lET0 decls1 ->
      return $ Stmt_implies_LET_decls lET0 decls1
  , pat_implies_apat = \apat0 ->
      return $ Pat_implies_apat apat0
  , pat_implies_pat_apat = \pat0 apat1 ->
      return $ Pat_implies_pat_apat pat0 apat1
  , pat_implies_pat_MINUS_apat = \pat0 mINUS1 apat2 ->
      return $ Pat_implies_pat_MINUS_apat pat0 mINUS1 apat2
  , pat_implies_pat_op_apat = \pat0 op1 apat2 ->
      return $ Pat_implies_pat_op_apat pat0 op1 apat2
  , apat_implies_var = \var0 ->
      return $ Apat_implies_var var0
  , apat_implies_LPAREN_pat_RPAREN = \lPAREN0 pat1 rPAREN2 ->
      return $ Apat_implies_LPAREN_pat_RPAREN lPAREN0 pat1 rPAREN2
  , var_implies_AS = \aS0 ->
      return $ Var_implies_AS aS0
  , var_implies_EXPORT = \eXPORT0 ->
      return $ Var_implies_EXPORT eXPORT0
  , var_implies_QVARID = \qVARID0 ->
      return $ Var_implies_QVARID qVARID0
  , var_implies_LPAREN_MINUS_RPAREN = \lPAREN0 mINUS1 rPAREN2 ->
      return $ Var_implies_LPAREN_MINUS_RPAREN lPAREN0 mINUS1 rPAREN2
  , var_implies_LPAREN_QVARSYM_RPAREN = \lPAREN0 qVARSYM1 rPAREN2 ->
      return $ Var_implies_LPAREN_QVARSYM_RPAREN lPAREN0 qVARSYM1 rPAREN2
  , con_implies_QCONID = \qCONID0 ->
      return $ Con_implies_QCONID qCONID0
  , con_implies_LPAREN_QCONSYM_RPAREN = \lPAREN0 qCONSYM1 rPAREN2 ->
      return $ Con_implies_LPAREN_QCONSYM_RPAREN lPAREN0 qCONSYM1 rPAREN2
  , varop_implies_QVARSYM = \qVARSYM0 ->
      return $ Varop_implies_QVARSYM qVARSYM0
  , varop_implies_BACKQUOTE_AS_BACKQUOTE = \bACKQUOTE0 aS1 bACKQUOTE2 ->
      return $ Varop_implies_BACKQUOTE_AS_BACKQUOTE bACKQUOTE0 aS1 bACKQUOTE2
  , varop_implies_BACKQUOTE_EXPORT_BACKQUOTE = \bACKQUOTE0 eXPORT1 bACKQUOTE2 ->
      return $ Varop_implies_BACKQUOTE_EXPORT_BACKQUOTE bACKQUOTE0 eXPORT1 bACKQUOTE2
  , varop_implies_BACKQUOTE_QVARID_BACKQUOTE = \bACKQUOTE0 qVARID1 bACKQUOTE2 ->
      return $ Varop_implies_BACKQUOTE_QVARID_BACKQUOTE bACKQUOTE0 qVARID1 bACKQUOTE2
  , conop_implies_QCONSYM = \qCONSYM0 ->
      return $ Conop_implies_QCONSYM qCONSYM0
  , conop_implies_BACKQUOTE_QCONID_BACKQUOTE = \bACKQUOTE0 qCONID1 bACKQUOTE2 ->
      return $ Conop_implies_BACKQUOTE_QCONID_BACKQUOTE bACKQUOTE0 qCONID1 bACKQUOTE2
  , op_implies_varop = \varop0 ->
      return $ Op_implies_varop varop0
  , op_implies_conop = \conop0 ->
      return $ Op_implies_conop conop0
  , as_opt_implies =
      return $ As_opt_implies
  , as_opt_implies_AS_modid = \aS0 modid1 ->
      return $ As_opt_implies_AS_modid aS0 modid1
  , qualified_opt_implies =
      return $ Qualified_opt_implies
  , qualified_opt_implies_QUALIFIED = \qUALIFIED0 ->
      return $ Qualified_opt_implies_QUALIFIED qUALIFIED0
  , tyvar_implies_AS = \aS0 ->
      return $ Tyvar_implies_AS aS0
  , tyvar_implies_EXPORT = \eXPORT0 ->
      return $ Tyvar_implies_EXPORT eXPORT0
  , tyvar_implies_QVARID = \qVARID0 ->
      return $ Tyvar_implies_QVARID qVARID0
  , tycls_implies_QCONID = \qCONID0 ->
      return $ Tycls_implies_QCONID qCONID0
  , modid_implies_QCONID = \qCONID0 ->
      return $ Modid_implies_QCONID qCONID0
  , integer_opt_implies =
      return $ Integer_opt_implies
  , integer_opt_implies_INTEGER = \iNTEGER0 ->
      return $ Integer_opt_implies_INTEGER iNTEGER0
  , semicolon_opt_implies =
      return $ Semicolon_opt_implies
  , semicolon_opt_implies_SEMICOLON = \sEMICOLON0 ->
      return $ Semicolon_opt_implies_SEMICOLON sEMICOLON0 }

