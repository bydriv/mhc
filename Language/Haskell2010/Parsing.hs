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
    Patguard_implies_infixexp_LARROW_infixexp Infixexp LARROW Infixexp
  | Patguard_implies_LET_decls LET Decls
  | Patguard_implies_infixexp Infixexp
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
  , gdpat_implies_patguards_RARROW_exp :: Patguards -> RARROW -> Exp -> m Gdpat
  , gdpat_implies_patguards_RARROW_exp_PIPE_gdpat :: Patguards -> RARROW -> Exp -> PIPE -> Gdpat -> m Gdpat
  , patguards_implies_patguard :: Patguard -> m Patguards
  , patguards_implies_patguard_COMMA_patguards :: Patguard -> COMMA -> Patguards -> m Patguards
  , patguard_implies_infixexp_LARROW_infixexp :: Infixexp -> LARROW -> Infixexp -> m Patguard
  , patguard_implies_LET_decls :: LET -> Decls -> m Patguard
  , patguard_implies_infixexp :: Infixexp -> m Patguard
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
    (11, Token (MODULE _)) -> Just (Reduce 1 240)
    (11, Token (WHERE _)) -> Just (Reduce 1 240)
    (11, Token (RBRACE _)) -> Just (Reduce 1 240)
    (11, Token (LPAREN _)) -> Just (Reduce 1 240)
    (11, Token (RPAREN _)) -> Just (Reduce 1 240)
    (11, Token (COMMA _)) -> Just (Reduce 1 240)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 240)
    (11, Token (HIDING _)) -> Just (Reduce 1 240)
    (11, Token (MINUS _)) -> Just (Reduce 1 240)
    (11, Token (QCONID _)) -> Just (Reduce 1 240)
    (11, Token (EXPORT _)) -> Just (Reduce 1 240)
    (11, Token (AS _)) -> Just (Reduce 1 240)
    (11, Token (QVARID _)) -> Just (Reduce 1 240)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 240)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 240)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 79)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 194)
    (13, Token (TYPE _)) -> Just (Shift 152)
    (13, Token (DATA _)) -> Just (Shift 130)
    (13, Token (NEWTYPE _)) -> Just (Shift 150)
    (13, Token (CLASS _)) -> Just (Shift 117)
    (13, Token (INSTANCE _)) -> Just (Shift 119)
    (13, Token (DEFAULT _)) -> Just (Shift 200)
    (13, Token (FOREIGN _)) -> Just (Shift 201)
    (13, Token (INFIXL _)) -> Just (Shift 310)
    (13, Token (INFIXR _)) -> Just (Shift 311)
    (13, Token (INFIX _)) -> Just (Shift 312)
    (13, Token (EXPORT _)) -> Just (Shift 112)
    (13, Token (AS _)) -> Just (Shift 113)
    (13, Token (QVARID _)) -> Just (Shift 114)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 79)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 194)
    (16, Token (TYPE _)) -> Just (Shift 152)
    (16, Token (DATA _)) -> Just (Shift 130)
    (16, Token (NEWTYPE _)) -> Just (Shift 150)
    (16, Token (CLASS _)) -> Just (Shift 117)
    (16, Token (INSTANCE _)) -> Just (Shift 119)
    (16, Token (DEFAULT _)) -> Just (Shift 200)
    (16, Token (FOREIGN _)) -> Just (Shift 201)
    (16, Token (INFIXL _)) -> Just (Shift 310)
    (16, Token (INFIXR _)) -> Just (Shift 311)
    (16, Token (INFIX _)) -> Just (Shift 312)
    (16, Token (EXPORT _)) -> Just (Shift 112)
    (16, Token (AS _)) -> Just (Shift 113)
    (16, Token (QVARID _)) -> Just (Shift 114)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 107)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 163)
    (19, Token (EXPORT _)) -> Just (Shift 112)
    (19, Token (AS _)) -> Just (Shift 113)
    (19, Token (QVARID _)) -> Just (Shift 114)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 107)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 163)
    (22, Token (EXPORT _)) -> Just (Shift 112)
    (22, Token (AS _)) -> Just (Shift 113)
    (22, Token (QVARID _)) -> Just (Shift 114)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 107)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 163)
    (25, Token (EXPORT _)) -> Just (Shift 112)
    (25, Token (AS _)) -> Just (Shift 113)
    (25, Token (QVARID _)) -> Just (Shift 114)
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
    (34, Token (LPAREN _)) -> Just (Shift 54)
    (34, Token (MINUS _)) -> Just (Shift 44)
    (34, Token (LBRACKET _)) -> Just (Shift 64)
    (34, Token (EXPORT _)) -> Just (Shift 112)
    (34, Token (AS _)) -> Just (Shift 113)
    (34, Token (QVARID _)) -> Just (Shift 114)
    (34, Token (STRING _)) -> Just (Shift 429)
    (34, Token (LET _)) -> Just (Shift 271)
    (34, Token (LAMBDA _)) -> Just (Shift 90)
    (34, Token (IF _)) -> Just (Shift 73)
    (34, Token (CASE _)) -> Just (Shift 75)
    (34, Token (DO _)) -> Just (Shift 401)
    (34, Token (INTEGER _)) -> Just (Shift 431)
    (35, Token (LPAREN _)) -> Just (Shift 54)
    (35, Token (MINUS _)) -> Just (Shift 44)
    (35, Token (LBRACKET _)) -> Just (Shift 64)
    (35, Token (EXPORT _)) -> Just (Shift 112)
    (35, Token (AS _)) -> Just (Shift 113)
    (35, Token (QVARID _)) -> Just (Shift 114)
    (35, Token (STRING _)) -> Just (Shift 429)
    (35, Token (LET _)) -> Just (Shift 271)
    (35, Token (LAMBDA _)) -> Just (Shift 90)
    (35, Token (IF _)) -> Just (Shift 73)
    (35, Token (CASE _)) -> Just (Shift 75)
    (35, Token (DO _)) -> Just (Shift 401)
    (35, Token (INTEGER _)) -> Just (Shift 431)
    (36, Token (LPAREN _)) -> Just (Shift 54)
    (36, Token (MINUS _)) -> Just (Shift 44)
    (36, Token (LBRACKET _)) -> Just (Shift 64)
    (36, Token (EXPORT _)) -> Just (Shift 112)
    (36, Token (AS _)) -> Just (Shift 113)
    (36, Token (QVARID _)) -> Just (Shift 114)
    (36, Token (STRING _)) -> Just (Shift 429)
    (36, Token (LET _)) -> Just (Shift 271)
    (36, Token (LAMBDA _)) -> Just (Shift 90)
    (36, Token (IF _)) -> Just (Shift 73)
    (36, Token (CASE _)) -> Just (Shift 75)
    (36, Token (DO _)) -> Just (Shift 401)
    (36, Token (INTEGER _)) -> Just (Shift 431)
    (37, Token (LPAREN _)) -> Just (Shift 54)
    (37, Token (MINUS _)) -> Just (Shift 44)
    (37, Token (LBRACKET _)) -> Just (Shift 64)
    (37, Token (EXPORT _)) -> Just (Shift 112)
    (37, Token (AS _)) -> Just (Shift 113)
    (37, Token (QVARID _)) -> Just (Shift 114)
    (37, Token (STRING _)) -> Just (Shift 429)
    (37, Token (LET _)) -> Just (Shift 271)
    (37, Token (LAMBDA _)) -> Just (Shift 90)
    (37, Token (IF _)) -> Just (Shift 73)
    (37, Token (CASE _)) -> Just (Shift 75)
    (37, Token (DO _)) -> Just (Shift 401)
    (37, Token (INTEGER _)) -> Just (Shift 431)
    (38, Token (LPAREN _)) -> Just (Shift 54)
    (38, Token (MINUS _)) -> Just (Shift 44)
    (38, Token (LBRACKET _)) -> Just (Shift 64)
    (38, Token (EXPORT _)) -> Just (Shift 112)
    (38, Token (AS _)) -> Just (Shift 113)
    (38, Token (QVARID _)) -> Just (Shift 114)
    (38, Token (STRING _)) -> Just (Shift 429)
    (38, Token (LET _)) -> Just (Shift 271)
    (38, Token (LAMBDA _)) -> Just (Shift 90)
    (38, Token (IF _)) -> Just (Shift 73)
    (38, Token (CASE _)) -> Just (Shift 75)
    (38, Token (DO _)) -> Just (Shift 401)
    (38, Token (INTEGER _)) -> Just (Shift 431)
    (39, Token (LPAREN _)) -> Just (Shift 54)
    (39, Token (MINUS _)) -> Just (Shift 44)
    (39, Token (LBRACKET _)) -> Just (Shift 64)
    (39, Token (EXPORT _)) -> Just (Shift 112)
    (39, Token (AS _)) -> Just (Shift 113)
    (39, Token (QVARID _)) -> Just (Shift 114)
    (39, Token (STRING _)) -> Just (Shift 429)
    (39, Token (LET _)) -> Just (Shift 271)
    (39, Token (LAMBDA _)) -> Just (Shift 90)
    (39, Token (IF _)) -> Just (Shift 73)
    (39, Token (CASE _)) -> Just (Shift 75)
    (39, Token (DO _)) -> Just (Shift 401)
    (39, Token (INTEGER _)) -> Just (Shift 431)
    (40, Token (LPAREN _)) -> Just (Shift 54)
    (40, Token (MINUS _)) -> Just (Shift 44)
    (40, Token (LBRACKET _)) -> Just (Shift 64)
    (40, Token (EXPORT _)) -> Just (Shift 112)
    (40, Token (AS _)) -> Just (Shift 113)
    (40, Token (QVARID _)) -> Just (Shift 114)
    (40, Token (STRING _)) -> Just (Shift 429)
    (40, Token (LET _)) -> Just (Shift 271)
    (40, Token (LAMBDA _)) -> Just (Shift 90)
    (40, Token (IF _)) -> Just (Shift 73)
    (40, Token (CASE _)) -> Just (Shift 75)
    (40, Token (DO _)) -> Just (Shift 401)
    (40, Token (INTEGER _)) -> Just (Shift 431)
    (41, Token (LPAREN _)) -> Just (Shift 54)
    (41, Token (MINUS _)) -> Just (Shift 44)
    (41, Token (LBRACKET _)) -> Just (Shift 64)
    (41, Token (EXPORT _)) -> Just (Shift 112)
    (41, Token (AS _)) -> Just (Shift 113)
    (41, Token (QVARID _)) -> Just (Shift 114)
    (41, Token (STRING _)) -> Just (Shift 429)
    (41, Token (LET _)) -> Just (Shift 271)
    (41, Token (LAMBDA _)) -> Just (Shift 90)
    (41, Token (IF _)) -> Just (Shift 73)
    (41, Token (CASE _)) -> Just (Shift 75)
    (41, Token (DO _)) -> Just (Shift 401)
    (41, Token (INTEGER _)) -> Just (Shift 431)
    (42, Token (LPAREN _)) -> Just (Shift 54)
    (42, Token (MINUS _)) -> Just (Shift 44)
    (42, Token (LBRACKET _)) -> Just (Shift 64)
    (42, Token (EXPORT _)) -> Just (Shift 112)
    (42, Token (AS _)) -> Just (Shift 113)
    (42, Token (QVARID _)) -> Just (Shift 114)
    (42, Token (STRING _)) -> Just (Shift 429)
    (42, Token (LET _)) -> Just (Shift 271)
    (42, Token (LAMBDA _)) -> Just (Shift 90)
    (42, Token (IF _)) -> Just (Shift 73)
    (42, Token (CASE _)) -> Just (Shift 75)
    (42, Token (DO _)) -> Just (Shift 401)
    (42, Token (INTEGER _)) -> Just (Shift 431)
    (43, Token (LPAREN _)) -> Just (Shift 54)
    (43, Token (MINUS _)) -> Just (Shift 44)
    (43, Token (LBRACKET _)) -> Just (Shift 64)
    (43, Token (EXPORT _)) -> Just (Shift 112)
    (43, Token (AS _)) -> Just (Shift 113)
    (43, Token (QVARID _)) -> Just (Shift 114)
    (43, Token (STRING _)) -> Just (Shift 429)
    (43, Token (LET _)) -> Just (Shift 271)
    (43, Token (LAMBDA _)) -> Just (Shift 90)
    (43, Token (IF _)) -> Just (Shift 73)
    (43, Token (CASE _)) -> Just (Shift 75)
    (43, Token (DO _)) -> Just (Shift 401)
    (43, Token (INTEGER _)) -> Just (Shift 431)
    (44, Token (LPAREN _)) -> Just (Shift 54)
    (44, Token (MINUS _)) -> Just (Shift 44)
    (44, Token (LBRACKET _)) -> Just (Shift 64)
    (44, Token (EXPORT _)) -> Just (Shift 112)
    (44, Token (AS _)) -> Just (Shift 113)
    (44, Token (QVARID _)) -> Just (Shift 114)
    (44, Token (STRING _)) -> Just (Shift 429)
    (44, Token (CASE _)) -> Just (Shift 75)
    (44, Token (DO _)) -> Just (Shift 401)
    (44, Token (INTEGER _)) -> Just (Shift 431)
    (45, Token (WHERE _)) -> Just (Reduce 1 170)
    (45, Token (RBRACE _)) -> Just (Reduce 1 170)
    (45, Token (LPAREN _)) -> Just (Shift 54)
    (45, Token (RPAREN _)) -> Just (Reduce 1 170)
    (45, Token (COMMA _)) -> Just (Reduce 1 170)
    (45, Token (DOT_DOT _)) -> Just (Reduce 1 170)
    (45, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (45, Token (EQUAL _)) -> Just (Reduce 1 170)
    (45, Token (PIPE _)) -> Just (Reduce 1 170)
    (45, Token (COLON_COLON _)) -> Just (Reduce 1 170)
    (45, Token (MINUS _)) -> Just (Reduce 1 170)
    (45, Token (LBRACKET _)) -> Just (Shift 64)
    (45, Token (RBRACKET _)) -> Just (Reduce 1 170)
    (45, Token (EXPORT _)) -> Just (Shift 112)
    (45, Token (AS _)) -> Just (Shift 113)
    (45, Token (QVARID _)) -> Just (Shift 114)
    (45, Token (STRING _)) -> Just (Shift 429)
    (45, Token (LARROW _)) -> Just (Reduce 1 170)
    (45, Token (THEN _)) -> Just (Reduce 1 170)
    (45, Token (ELSE _)) -> Just (Reduce 1 170)
    (45, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (45, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (45, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (45, Token (OF _)) -> Just (Reduce 1 170)
    (45, Token (INTEGER _)) -> Just (Shift 431)
    (46, Token (LPAREN _)) -> Just (Shift 54)
    (46, Token (MINUS _)) -> Just (Shift 44)
    (46, Token (LBRACKET _)) -> Just (Shift 64)
    (46, Token (EXPORT _)) -> Just (Shift 112)
    (46, Token (AS _)) -> Just (Shift 113)
    (46, Token (QVARID _)) -> Just (Shift 114)
    (46, Token (STRING _)) -> Just (Shift 429)
    (46, Token (LET _)) -> Just (Shift 271)
    (46, Token (LAMBDA _)) -> Just (Shift 90)
    (46, Token (IF _)) -> Just (Shift 73)
    (46, Token (CASE _)) -> Just (Shift 75)
    (46, Token (DO _)) -> Just (Shift 401)
    (46, Token (INTEGER _)) -> Just (Shift 431)
    (47, Token (LPAREN _)) -> Just (Shift 54)
    (47, Token (MINUS _)) -> Just (Shift 44)
    (47, Token (LBRACKET _)) -> Just (Shift 64)
    (47, Token (EXPORT _)) -> Just (Shift 112)
    (47, Token (AS _)) -> Just (Shift 113)
    (47, Token (QVARID _)) -> Just (Shift 114)
    (47, Token (STRING _)) -> Just (Shift 429)
    (47, Token (LET _)) -> Just (Shift 271)
    (47, Token (LAMBDA _)) -> Just (Shift 90)
    (47, Token (IF _)) -> Just (Shift 73)
    (47, Token (CASE _)) -> Just (Shift 75)
    (47, Token (DO _)) -> Just (Shift 401)
    (47, Token (INTEGER _)) -> Just (Shift 431)
    (48, Token (LPAREN _)) -> Just (Shift 54)
    (48, Token (MINUS _)) -> Just (Shift 44)
    (48, Token (LBRACKET _)) -> Just (Shift 64)
    (48, Token (EXPORT _)) -> Just (Shift 112)
    (48, Token (AS _)) -> Just (Shift 113)
    (48, Token (QVARID _)) -> Just (Shift 114)
    (48, Token (STRING _)) -> Just (Shift 429)
    (48, Token (LET _)) -> Just (Shift 271)
    (48, Token (LAMBDA _)) -> Just (Shift 90)
    (48, Token (IF _)) -> Just (Shift 73)
    (48, Token (CASE _)) -> Just (Shift 75)
    (48, Token (DO _)) -> Just (Shift 401)
    (48, Token (INTEGER _)) -> Just (Shift 431)
    (49, Token (LPAREN _)) -> Just (Shift 54)
    (49, Token (MINUS _)) -> Just (Shift 44)
    (49, Token (LBRACKET _)) -> Just (Shift 64)
    (49, Token (EXPORT _)) -> Just (Shift 112)
    (49, Token (AS _)) -> Just (Shift 113)
    (49, Token (QVARID _)) -> Just (Shift 114)
    (49, Token (STRING _)) -> Just (Shift 429)
    (49, Token (LET _)) -> Just (Shift 271)
    (49, Token (LAMBDA _)) -> Just (Shift 90)
    (49, Token (IF _)) -> Just (Shift 73)
    (49, Token (CASE _)) -> Just (Shift 75)
    (49, Token (DO _)) -> Just (Shift 401)
    (49, Token (INTEGER _)) -> Just (Shift 431)
    (50, Token (LPAREN _)) -> Just (Shift 54)
    (50, Token (MINUS _)) -> Just (Shift 44)
    (50, Token (LBRACKET _)) -> Just (Shift 64)
    (50, Token (EXPORT _)) -> Just (Shift 112)
    (50, Token (AS _)) -> Just (Shift 113)
    (50, Token (QVARID _)) -> Just (Shift 114)
    (50, Token (STRING _)) -> Just (Shift 429)
    (50, Token (LET _)) -> Just (Shift 271)
    (50, Token (LAMBDA _)) -> Just (Shift 90)
    (50, Token (IF _)) -> Just (Shift 73)
    (50, Token (CASE _)) -> Just (Shift 75)
    (50, Token (DO _)) -> Just (Shift 401)
    (50, Token (INTEGER _)) -> Just (Shift 431)
    (51, Token (RBRACE _)) -> Just (Reduce 0 207)
    (51, Token (LPAREN _)) -> Just (Shift 54)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 207)
    (51, Token (MINUS _)) -> Just (Shift 44)
    (51, Token (LBRACKET _)) -> Just (Shift 64)
    (51, Token (EXPORT _)) -> Just (Shift 112)
    (51, Token (AS _)) -> Just (Shift 113)
    (51, Token (QVARID _)) -> Just (Shift 114)
    (51, Token (STRING _)) -> Just (Shift 429)
    (51, Token (LET _)) -> Just (Shift 264)
    (51, Token (LAMBDA _)) -> Just (Shift 90)
    (51, Token (IF _)) -> Just (Shift 73)
    (51, Token (CASE _)) -> Just (Shift 75)
    (51, Token (DO _)) -> Just (Shift 401)
    (51, Token (INTEGER _)) -> Just (Shift 431)
    (52, Token (RBRACE _)) -> Just (Reduce 0 207)
    (52, Token (LPAREN _)) -> Just (Shift 54)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 207)
    (52, Token (MINUS _)) -> Just (Shift 44)
    (52, Token (LBRACKET _)) -> Just (Shift 64)
    (52, Token (EXPORT _)) -> Just (Shift 112)
    (52, Token (AS _)) -> Just (Shift 113)
    (52, Token (QVARID _)) -> Just (Shift 114)
    (52, Token (STRING _)) -> Just (Shift 429)
    (52, Token (LET _)) -> Just (Shift 264)
    (52, Token (LAMBDA _)) -> Just (Shift 90)
    (52, Token (IF _)) -> Just (Shift 73)
    (52, Token (CASE _)) -> Just (Shift 75)
    (52, Token (DO _)) -> Just (Shift 401)
    (52, Token (INTEGER _)) -> Just (Shift 431)
    (53, Token (LPAREN _)) -> Just (Shift 54)
    (53, Token (MINUS _)) -> Just (Shift 44)
    (53, Token (LBRACKET _)) -> Just (Shift 64)
    (53, Token (EXPORT _)) -> Just (Shift 112)
    (53, Token (AS _)) -> Just (Shift 113)
    (53, Token (QVARID _)) -> Just (Shift 114)
    (53, Token (STRING _)) -> Just (Shift 429)
    (53, Token (LET _)) -> Just (Shift 271)
    (53, Token (LAMBDA _)) -> Just (Shift 90)
    (53, Token (IF _)) -> Just (Shift 73)
    (53, Token (CASE _)) -> Just (Shift 75)
    (53, Token (DO _)) -> Just (Shift 401)
    (53, Token (INTEGER _)) -> Just (Shift 431)
    (54, Token (LPAREN _)) -> Just (Shift 54)
    (54, Token (MINUS _)) -> Just (Shift 55)
    (54, Token (LBRACKET _)) -> Just (Shift 64)
    (54, Token (EXPORT _)) -> Just (Shift 112)
    (54, Token (AS _)) -> Just (Shift 113)
    (54, Token (QVARID _)) -> Just (Shift 114)
    (54, Token (STRING _)) -> Just (Shift 429)
    (54, Token (LET _)) -> Just (Shift 271)
    (54, Token (LAMBDA _)) -> Just (Shift 90)
    (54, Token (IF _)) -> Just (Shift 73)
    (54, Token (QVARSYM _)) -> Just (Shift 56)
    (54, Token (BACKQUOTE _)) -> Just (Shift 430)
    (54, Token (QCONSYM _)) -> Just (Shift 62)
    (54, Token (CASE _)) -> Just (Shift 75)
    (54, Token (DO _)) -> Just (Shift 401)
    (54, Token (INTEGER _)) -> Just (Shift 431)
    (55, Token (LPAREN _)) -> Just (Shift 54)
    (55, Token (RPAREN _)) -> Just (Shift 109)
    (55, Token (MINUS _)) -> Just (Shift 44)
    (55, Token (LBRACKET _)) -> Just (Shift 64)
    (55, Token (EXPORT _)) -> Just (Shift 112)
    (55, Token (AS _)) -> Just (Shift 113)
    (55, Token (QVARID _)) -> Just (Shift 114)
    (55, Token (STRING _)) -> Just (Shift 429)
    (55, Token (CASE _)) -> Just (Shift 75)
    (55, Token (DO _)) -> Just (Shift 401)
    (55, Token (INTEGER _)) -> Just (Shift 431)
    (56, Token (LPAREN _)) -> Just (Shift 54)
    (56, Token (RPAREN _)) -> Just (Shift 110)
    (56, Token (MINUS _)) -> Just (Shift 44)
    (56, Token (LBRACKET _)) -> Just (Shift 64)
    (56, Token (EXPORT _)) -> Just (Shift 112)
    (56, Token (AS _)) -> Just (Shift 113)
    (56, Token (QVARID _)) -> Just (Shift 114)
    (56, Token (STRING _)) -> Just (Shift 429)
    (56, Token (LET _)) -> Just (Shift 271)
    (56, Token (LAMBDA _)) -> Just (Shift 90)
    (56, Token (IF _)) -> Just (Shift 73)
    (56, Token (CASE _)) -> Just (Shift 75)
    (56, Token (DO _)) -> Just (Shift 401)
    (56, Token (INTEGER _)) -> Just (Shift 431)
    (57, Token (LPAREN _)) -> Just (Shift 54)
    (57, Token (MINUS _)) -> Just (Shift 44)
    (57, Token (LBRACKET _)) -> Just (Shift 64)
    (57, Token (EXPORT _)) -> Just (Shift 112)
    (57, Token (AS _)) -> Just (Shift 113)
    (57, Token (QVARID _)) -> Just (Shift 114)
    (57, Token (STRING _)) -> Just (Shift 429)
    (57, Token (LET _)) -> Just (Shift 271)
    (57, Token (LAMBDA _)) -> Just (Shift 90)
    (57, Token (IF _)) -> Just (Shift 73)
    (57, Token (CASE _)) -> Just (Shift 75)
    (57, Token (DO _)) -> Just (Shift 401)
    (57, Token (INTEGER _)) -> Just (Shift 431)
    (58, Token (LPAREN _)) -> Just (Shift 54)
    (58, Token (MINUS _)) -> Just (Shift 44)
    (58, Token (LBRACKET _)) -> Just (Shift 64)
    (58, Token (EXPORT _)) -> Just (Shift 112)
    (58, Token (AS _)) -> Just (Shift 113)
    (58, Token (QVARID _)) -> Just (Shift 114)
    (58, Token (STRING _)) -> Just (Shift 429)
    (58, Token (LET _)) -> Just (Shift 271)
    (58, Token (LAMBDA _)) -> Just (Shift 90)
    (58, Token (IF _)) -> Just (Shift 73)
    (58, Token (CASE _)) -> Just (Shift 75)
    (58, Token (DO _)) -> Just (Shift 401)
    (58, Token (INTEGER _)) -> Just (Shift 431)
    (59, Token (LPAREN _)) -> Just (Shift 54)
    (59, Token (MINUS _)) -> Just (Shift 44)
    (59, Token (LBRACKET _)) -> Just (Shift 64)
    (59, Token (EXPORT _)) -> Just (Shift 112)
    (59, Token (AS _)) -> Just (Shift 113)
    (59, Token (QVARID _)) -> Just (Shift 114)
    (59, Token (STRING _)) -> Just (Shift 429)
    (59, Token (LET _)) -> Just (Shift 271)
    (59, Token (LAMBDA _)) -> Just (Shift 90)
    (59, Token (IF _)) -> Just (Shift 73)
    (59, Token (CASE _)) -> Just (Shift 75)
    (59, Token (DO _)) -> Just (Shift 401)
    (59, Token (INTEGER _)) -> Just (Shift 431)
    (60, Token (LPAREN _)) -> Just (Shift 54)
    (60, Token (MINUS _)) -> Just (Shift 44)
    (60, Token (LBRACKET _)) -> Just (Shift 64)
    (60, Token (EXPORT _)) -> Just (Shift 112)
    (60, Token (AS _)) -> Just (Shift 113)
    (60, Token (QVARID _)) -> Just (Shift 114)
    (60, Token (STRING _)) -> Just (Shift 429)
    (60, Token (LET _)) -> Just (Shift 271)
    (60, Token (LAMBDA _)) -> Just (Shift 90)
    (60, Token (IF _)) -> Just (Shift 73)
    (60, Token (CASE _)) -> Just (Shift 75)
    (60, Token (DO _)) -> Just (Shift 401)
    (60, Token (INTEGER _)) -> Just (Shift 431)
    (61, Token (LPAREN _)) -> Just (Shift 54)
    (61, Token (MINUS _)) -> Just (Shift 44)
    (61, Token (LBRACKET _)) -> Just (Shift 64)
    (61, Token (EXPORT _)) -> Just (Shift 112)
    (61, Token (AS _)) -> Just (Shift 113)
    (61, Token (QVARID _)) -> Just (Shift 114)
    (61, Token (STRING _)) -> Just (Shift 429)
    (61, Token (LET _)) -> Just (Shift 271)
    (61, Token (LAMBDA _)) -> Just (Shift 90)
    (61, Token (IF _)) -> Just (Shift 73)
    (61, Token (CASE _)) -> Just (Shift 75)
    (61, Token (DO _)) -> Just (Shift 401)
    (61, Token (INTEGER _)) -> Just (Shift 431)
    (62, Token (LPAREN _)) -> Just (Shift 54)
    (62, Token (MINUS _)) -> Just (Shift 44)
    (62, Token (LBRACKET _)) -> Just (Shift 64)
    (62, Token (EXPORT _)) -> Just (Shift 112)
    (62, Token (AS _)) -> Just (Shift 113)
    (62, Token (QVARID _)) -> Just (Shift 114)
    (62, Token (STRING _)) -> Just (Shift 429)
    (62, Token (LET _)) -> Just (Shift 271)
    (62, Token (LAMBDA _)) -> Just (Shift 90)
    (62, Token (IF _)) -> Just (Shift 73)
    (62, Token (CASE _)) -> Just (Shift 75)
    (62, Token (DO _)) -> Just (Shift 401)
    (62, Token (INTEGER _)) -> Just (Shift 431)
    (63, Token (LPAREN _)) -> Just (Shift 54)
    (63, Token (MINUS _)) -> Just (Shift 44)
    (63, Token (LBRACKET _)) -> Just (Shift 64)
    (63, Token (EXPORT _)) -> Just (Shift 112)
    (63, Token (AS _)) -> Just (Shift 113)
    (63, Token (QVARID _)) -> Just (Shift 114)
    (63, Token (STRING _)) -> Just (Shift 429)
    (63, Token (LET _)) -> Just (Shift 271)
    (63, Token (LAMBDA _)) -> Just (Shift 90)
    (63, Token (IF _)) -> Just (Shift 73)
    (63, Token (CASE _)) -> Just (Shift 75)
    (63, Token (DO _)) -> Just (Shift 401)
    (63, Token (INTEGER _)) -> Just (Shift 431)
    (64, Token (LPAREN _)) -> Just (Shift 54)
    (64, Token (MINUS _)) -> Just (Shift 44)
    (64, Token (LBRACKET _)) -> Just (Shift 64)
    (64, Token (EXPORT _)) -> Just (Shift 112)
    (64, Token (AS _)) -> Just (Shift 113)
    (64, Token (QVARID _)) -> Just (Shift 114)
    (64, Token (STRING _)) -> Just (Shift 429)
    (64, Token (LET _)) -> Just (Shift 271)
    (64, Token (LAMBDA _)) -> Just (Shift 90)
    (64, Token (IF _)) -> Just (Shift 73)
    (64, Token (CASE _)) -> Just (Shift 75)
    (64, Token (DO _)) -> Just (Shift 401)
    (64, Token (INTEGER _)) -> Just (Shift 431)
    (65, Token (LPAREN _)) -> Just (Shift 54)
    (65, Token (MINUS _)) -> Just (Shift 44)
    (65, Token (LBRACKET _)) -> Just (Shift 64)
    (65, Token (EXPORT _)) -> Just (Shift 112)
    (65, Token (AS _)) -> Just (Shift 113)
    (65, Token (QVARID _)) -> Just (Shift 114)
    (65, Token (STRING _)) -> Just (Shift 429)
    (65, Token (LET _)) -> Just (Shift 271)
    (65, Token (LAMBDA _)) -> Just (Shift 90)
    (65, Token (IF _)) -> Just (Shift 73)
    (65, Token (CASE _)) -> Just (Shift 75)
    (65, Token (DO _)) -> Just (Shift 401)
    (65, Token (INTEGER _)) -> Just (Shift 431)
    (66, Token (LPAREN _)) -> Just (Shift 54)
    (66, Token (MINUS _)) -> Just (Shift 44)
    (66, Token (LBRACKET _)) -> Just (Shift 64)
    (66, Token (RBRACKET _)) -> Just (Shift 420)
    (66, Token (EXPORT _)) -> Just (Shift 112)
    (66, Token (AS _)) -> Just (Shift 113)
    (66, Token (QVARID _)) -> Just (Shift 114)
    (66, Token (STRING _)) -> Just (Shift 429)
    (66, Token (LET _)) -> Just (Shift 271)
    (66, Token (LAMBDA _)) -> Just (Shift 90)
    (66, Token (IF _)) -> Just (Shift 73)
    (66, Token (CASE _)) -> Just (Shift 75)
    (66, Token (DO _)) -> Just (Shift 401)
    (66, Token (INTEGER _)) -> Just (Shift 431)
    (67, Token (LPAREN _)) -> Just (Shift 54)
    (67, Token (MINUS _)) -> Just (Shift 44)
    (67, Token (LBRACKET _)) -> Just (Shift 64)
    (67, Token (RBRACKET _)) -> Just (Shift 421)
    (67, Token (EXPORT _)) -> Just (Shift 112)
    (67, Token (AS _)) -> Just (Shift 113)
    (67, Token (QVARID _)) -> Just (Shift 114)
    (67, Token (STRING _)) -> Just (Shift 429)
    (67, Token (LET _)) -> Just (Shift 271)
    (67, Token (LAMBDA _)) -> Just (Shift 90)
    (67, Token (IF _)) -> Just (Shift 73)
    (67, Token (CASE _)) -> Just (Shift 75)
    (67, Token (DO _)) -> Just (Shift 401)
    (67, Token (INTEGER _)) -> Just (Shift 431)
    (68, Token (LPAREN _)) -> Just (Shift 54)
    (68, Token (MINUS _)) -> Just (Shift 44)
    (68, Token (LBRACKET _)) -> Just (Shift 64)
    (68, Token (EXPORT _)) -> Just (Shift 112)
    (68, Token (AS _)) -> Just (Shift 113)
    (68, Token (QVARID _)) -> Just (Shift 114)
    (68, Token (STRING _)) -> Just (Shift 429)
    (68, Token (LET _)) -> Just (Shift 270)
    (68, Token (LAMBDA _)) -> Just (Shift 90)
    (68, Token (IF _)) -> Just (Shift 73)
    (68, Token (CASE _)) -> Just (Shift 75)
    (68, Token (DO _)) -> Just (Shift 401)
    (68, Token (INTEGER _)) -> Just (Shift 431)
    (69, Token (LPAREN _)) -> Just (Shift 54)
    (69, Token (MINUS _)) -> Just (Shift 44)
    (69, Token (LBRACKET _)) -> Just (Shift 64)
    (69, Token (EXPORT _)) -> Just (Shift 112)
    (69, Token (AS _)) -> Just (Shift 113)
    (69, Token (QVARID _)) -> Just (Shift 114)
    (69, Token (STRING _)) -> Just (Shift 429)
    (69, Token (LET _)) -> Just (Shift 270)
    (69, Token (LAMBDA _)) -> Just (Shift 90)
    (69, Token (IF _)) -> Just (Shift 73)
    (69, Token (CASE _)) -> Just (Shift 75)
    (69, Token (DO _)) -> Just (Shift 401)
    (69, Token (INTEGER _)) -> Just (Shift 431)
    (70, Token (LPAREN _)) -> Just (Shift 54)
    (70, Token (MINUS _)) -> Just (Shift 44)
    (70, Token (LBRACKET _)) -> Just (Shift 64)
    (70, Token (EXPORT _)) -> Just (Shift 112)
    (70, Token (AS _)) -> Just (Shift 113)
    (70, Token (QVARID _)) -> Just (Shift 114)
    (70, Token (STRING _)) -> Just (Shift 429)
    (70, Token (LET _)) -> Just (Shift 270)
    (70, Token (LAMBDA _)) -> Just (Shift 90)
    (70, Token (IF _)) -> Just (Shift 73)
    (70, Token (CASE _)) -> Just (Shift 75)
    (70, Token (DO _)) -> Just (Shift 401)
    (70, Token (INTEGER _)) -> Just (Shift 431)
    (71, Token (LPAREN _)) -> Just (Shift 54)
    (71, Token (MINUS _)) -> Just (Shift 44)
    (71, Token (LBRACKET _)) -> Just (Shift 64)
    (71, Token (EXPORT _)) -> Just (Shift 112)
    (71, Token (AS _)) -> Just (Shift 113)
    (71, Token (QVARID _)) -> Just (Shift 114)
    (71, Token (STRING _)) -> Just (Shift 429)
    (71, Token (LET _)) -> Just (Shift 270)
    (71, Token (LAMBDA _)) -> Just (Shift 90)
    (71, Token (IF _)) -> Just (Shift 73)
    (71, Token (CASE _)) -> Just (Shift 75)
    (71, Token (DO _)) -> Just (Shift 401)
    (71, Token (INTEGER _)) -> Just (Shift 431)
    (72, Token (LPAREN _)) -> Just (Shift 54)
    (72, Token (MINUS _)) -> Just (Shift 44)
    (72, Token (LBRACKET _)) -> Just (Shift 64)
    (72, Token (EXPORT _)) -> Just (Shift 112)
    (72, Token (AS _)) -> Just (Shift 113)
    (72, Token (QVARID _)) -> Just (Shift 114)
    (72, Token (STRING _)) -> Just (Shift 429)
    (72, Token (LET _)) -> Just (Shift 270)
    (72, Token (LAMBDA _)) -> Just (Shift 90)
    (72, Token (IF _)) -> Just (Shift 73)
    (72, Token (CASE _)) -> Just (Shift 75)
    (72, Token (DO _)) -> Just (Shift 401)
    (72, Token (INTEGER _)) -> Just (Shift 431)
    (73, Token (LPAREN _)) -> Just (Shift 54)
    (73, Token (MINUS _)) -> Just (Shift 44)
    (73, Token (LBRACKET _)) -> Just (Shift 64)
    (73, Token (EXPORT _)) -> Just (Shift 112)
    (73, Token (AS _)) -> Just (Shift 113)
    (73, Token (QVARID _)) -> Just (Shift 114)
    (73, Token (STRING _)) -> Just (Shift 429)
    (73, Token (LET _)) -> Just (Shift 271)
    (73, Token (LAMBDA _)) -> Just (Shift 90)
    (73, Token (IF _)) -> Just (Shift 73)
    (73, Token (CASE _)) -> Just (Shift 75)
    (73, Token (DO _)) -> Just (Shift 401)
    (73, Token (INTEGER _)) -> Just (Shift 431)
    (74, Token (LPAREN _)) -> Just (Shift 54)
    (74, Token (MINUS _)) -> Just (Shift 44)
    (74, Token (LBRACKET _)) -> Just (Shift 64)
    (74, Token (EXPORT _)) -> Just (Shift 112)
    (74, Token (AS _)) -> Just (Shift 113)
    (74, Token (QVARID _)) -> Just (Shift 114)
    (74, Token (STRING _)) -> Just (Shift 429)
    (74, Token (LET _)) -> Just (Shift 271)
    (74, Token (LAMBDA _)) -> Just (Shift 90)
    (74, Token (IF _)) -> Just (Shift 73)
    (74, Token (CASE _)) -> Just (Shift 75)
    (74, Token (DO _)) -> Just (Shift 401)
    (74, Token (INTEGER _)) -> Just (Shift 431)
    (75, Token (LPAREN _)) -> Just (Shift 54)
    (75, Token (MINUS _)) -> Just (Shift 44)
    (75, Token (LBRACKET _)) -> Just (Shift 64)
    (75, Token (EXPORT _)) -> Just (Shift 112)
    (75, Token (AS _)) -> Just (Shift 113)
    (75, Token (QVARID _)) -> Just (Shift 114)
    (75, Token (STRING _)) -> Just (Shift 429)
    (75, Token (LET _)) -> Just (Shift 271)
    (75, Token (LAMBDA _)) -> Just (Shift 90)
    (75, Token (IF _)) -> Just (Shift 73)
    (75, Token (CASE _)) -> Just (Shift 75)
    (75, Token (DO _)) -> Just (Shift 401)
    (75, Token (INTEGER _)) -> Just (Shift 431)
    (76, Token (LPAREN _)) -> Just (Shift 54)
    (76, Token (MINUS _)) -> Just (Shift 44)
    (76, Token (LBRACKET _)) -> Just (Shift 64)
    (76, Token (EXPORT _)) -> Just (Shift 112)
    (76, Token (AS _)) -> Just (Shift 113)
    (76, Token (QVARID _)) -> Just (Shift 114)
    (76, Token (STRING _)) -> Just (Shift 429)
    (76, Token (LET _)) -> Just (Shift 271)
    (76, Token (LAMBDA _)) -> Just (Shift 90)
    (76, Token (IF _)) -> Just (Shift 73)
    (76, Token (CASE _)) -> Just (Shift 75)
    (76, Token (DO _)) -> Just (Shift 401)
    (76, Token (INTEGER _)) -> Just (Shift 431)
    (77, Token (LPAREN _)) -> Just (Shift 79)
    (77, Token (EXPORT _)) -> Just (Shift 112)
    (77, Token (AS _)) -> Just (Shift 113)
    (77, Token (QVARID _)) -> Just (Shift 114)
    (78, Token (LPAREN _)) -> Just (Shift 79)
    (78, Token (EXPORT _)) -> Just (Shift 112)
    (78, Token (AS _)) -> Just (Shift 113)
    (78, Token (QVARID _)) -> Just (Shift 114)
    (79, Token (LPAREN _)) -> Just (Shift 79)
    (79, Token (MINUS _)) -> Just (Shift 111)
    (79, Token (EXPORT _)) -> Just (Shift 112)
    (79, Token (AS _)) -> Just (Shift 113)
    (79, Token (QVARID _)) -> Just (Shift 114)
    (79, Token (QVARSYM _)) -> Just (Shift 115)
    (80, Token (LPAREN _)) -> Just (Shift 79)
    (80, Token (RPAREN _)) -> Just (Shift 454)
    (80, Token (MINUS _)) -> Just (Shift 77)
    (80, Token (EXPORT _)) -> Just (Shift 112)
    (80, Token (AS _)) -> Just (Shift 113)
    (80, Token (QVARID _)) -> Just (Shift 114)
    (80, Token (QVARSYM _)) -> Just (Shift 459)
    (80, Token (BACKQUOTE _)) -> Just (Shift 352)
    (80, Token (QCONSYM _)) -> Just (Shift 355)
    (81, Token (RBRACE _)) -> Just (Reduce 0 85)
    (81, Token (LPAREN _)) -> Just (Shift 79)
    (81, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (81, Token (INFIXL _)) -> Just (Shift 310)
    (81, Token (INFIXR _)) -> Just (Shift 311)
    (81, Token (INFIX _)) -> Just (Shift 312)
    (81, Token (EXPORT _)) -> Just (Shift 112)
    (81, Token (AS _)) -> Just (Shift 113)
    (81, Token (QVARID _)) -> Just (Shift 114)
    (82, Token (RBRACE _)) -> Just (Reduce 0 85)
    (82, Token (LPAREN _)) -> Just (Shift 79)
    (82, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (82, Token (INFIXL _)) -> Just (Shift 310)
    (82, Token (INFIXR _)) -> Just (Shift 311)
    (82, Token (INFIX _)) -> Just (Shift 312)
    (82, Token (EXPORT _)) -> Just (Shift 112)
    (82, Token (AS _)) -> Just (Shift 113)
    (82, Token (QVARID _)) -> Just (Shift 114)
    (83, Token (RBRACE _)) -> Just (Reduce 0 85)
    (83, Token (LPAREN _)) -> Just (Shift 79)
    (83, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (83, Token (INFIXL _)) -> Just (Shift 310)
    (83, Token (INFIXR _)) -> Just (Shift 311)
    (83, Token (INFIX _)) -> Just (Shift 312)
    (83, Token (EXPORT _)) -> Just (Shift 112)
    (83, Token (AS _)) -> Just (Shift 113)
    (83, Token (QVARID _)) -> Just (Shift 114)
    (84, Token (RBRACE _)) -> Just (Reduce 0 85)
    (84, Token (LPAREN _)) -> Just (Shift 79)
    (84, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (84, Token (INFIXL _)) -> Just (Shift 310)
    (84, Token (INFIXR _)) -> Just (Shift 311)
    (84, Token (INFIX _)) -> Just (Shift 312)
    (84, Token (EXPORT _)) -> Just (Shift 112)
    (84, Token (AS _)) -> Just (Shift 113)
    (84, Token (QVARID _)) -> Just (Shift 114)
    (85, Token (LPAREN _)) -> Just (Shift 79)
    (85, Token (EQUAL _)) -> Just (Shift 46)
    (85, Token (PIPE _)) -> Just (Shift 68)
    (85, Token (MINUS _)) -> Just (Shift 77)
    (85, Token (EXPORT _)) -> Just (Shift 112)
    (85, Token (AS _)) -> Just (Shift 113)
    (85, Token (QVARID _)) -> Just (Shift 114)
    (85, Token (QVARSYM _)) -> Just (Shift 459)
    (85, Token (BACKQUOTE _)) -> Just (Shift 352)
    (85, Token (QCONSYM _)) -> Just (Shift 355)
    (86, Token (RBRACE _)) -> Just (Reduce 0 80)
    (86, Token (LPAREN _)) -> Just (Shift 79)
    (86, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (86, Token (EXPORT _)) -> Just (Shift 112)
    (86, Token (AS _)) -> Just (Shift 113)
    (86, Token (QVARID _)) -> Just (Shift 114)
    (87, Token (RBRACE _)) -> Just (Reduce 0 80)
    (87, Token (LPAREN _)) -> Just (Shift 79)
    (87, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (87, Token (EXPORT _)) -> Just (Shift 112)
    (87, Token (AS _)) -> Just (Shift 113)
    (87, Token (QVARID _)) -> Just (Shift 114)
    (88, Token (LPAREN _)) -> Just (Shift 79)
    (88, Token (EQUAL _)) -> Just (Shift 48)
    (88, Token (PIPE _)) -> Just (Shift 70)
    (88, Token (MINUS _)) -> Just (Shift 77)
    (88, Token (EXPORT _)) -> Just (Shift 112)
    (88, Token (AS _)) -> Just (Shift 113)
    (88, Token (QVARID _)) -> Just (Shift 114)
    (88, Token (QVARSYM _)) -> Just (Shift 459)
    (88, Token (BACKQUOTE _)) -> Just (Shift 352)
    (88, Token (QCONSYM _)) -> Just (Shift 355)
    (89, Token (LPAREN _)) -> Just (Shift 79)
    (89, Token (EQUAL _)) -> Just (Shift 49)
    (89, Token (PIPE _)) -> Just (Shift 71)
    (89, Token (MINUS _)) -> Just (Shift 77)
    (89, Token (EXPORT _)) -> Just (Shift 112)
    (89, Token (AS _)) -> Just (Shift 113)
    (89, Token (QVARID _)) -> Just (Shift 114)
    (89, Token (QVARSYM _)) -> Just (Shift 459)
    (89, Token (BACKQUOTE _)) -> Just (Shift 352)
    (89, Token (QCONSYM _)) -> Just (Shift 355)
    (90, Token (LPAREN _)) -> Just (Shift 79)
    (90, Token (EXPORT _)) -> Just (Shift 112)
    (90, Token (AS _)) -> Just (Shift 113)
    (90, Token (QVARID _)) -> Just (Shift 114)
    (91, Token (RBRACE _)) -> Just (Reduce 0 195)
    (91, Token (LPAREN _)) -> Just (Shift 79)
    (91, Token (SEMICOLON _)) -> Just (Reduce 0 195)
    (91, Token (EXPORT _)) -> Just (Shift 112)
    (91, Token (AS _)) -> Just (Shift 113)
    (91, Token (QVARID _)) -> Just (Shift 114)
    (92, Token (RBRACE _)) -> Just (Reduce 0 195)
    (92, Token (LPAREN _)) -> Just (Shift 79)
    (92, Token (SEMICOLON _)) -> Just (Reduce 0 195)
    (92, Token (EXPORT _)) -> Just (Shift 112)
    (92, Token (AS _)) -> Just (Shift 113)
    (92, Token (QVARID _)) -> Just (Shift 114)
    (93, Token (LPAREN _)) -> Just (Shift 79)
    (93, Token (MINUS _)) -> Just (Shift 77)
    (93, Token (RARROW _)) -> Just (Shift 35)
    (93, Token (EXPORT _)) -> Just (Shift 112)
    (93, Token (AS _)) -> Just (Shift 113)
    (93, Token (QVARID _)) -> Just (Shift 114)
    (93, Token (QVARSYM _)) -> Just (Shift 459)
    (93, Token (BACKQUOTE _)) -> Just (Shift 352)
    (93, Token (QCONSYM _)) -> Just (Shift 355)
    (94, Token (LPAREN _)) -> Just (Shift 79)
    (94, Token (MINUS _)) -> Just (Shift 77)
    (94, Token (RARROW _)) -> Just (Shift 50)
    (94, Token (EXPORT _)) -> Just (Shift 112)
    (94, Token (AS _)) -> Just (Shift 113)
    (94, Token (QVARID _)) -> Just (Shift 114)
    (94, Token (QVARSYM _)) -> Just (Shift 459)
    (94, Token (BACKQUOTE _)) -> Just (Shift 352)
    (94, Token (QCONSYM _)) -> Just (Shift 355)
    (95, Token (LPAREN _)) -> Just (Shift 107)
    (95, Token (RPAREN _)) -> Just (Reduce 0 15)
    (95, Token (QCONID _)) -> Just (Shift 163)
    (95, Token (EXPORT _)) -> Just (Shift 112)
    (95, Token (AS _)) -> Just (Shift 113)
    (95, Token (QVARID _)) -> Just (Shift 114)
    (96, Token (LPAREN _)) -> Just (Shift 107)
    (96, Token (RPAREN _)) -> Just (Reduce 0 15)
    (96, Token (QCONID _)) -> Just (Shift 163)
    (96, Token (EXPORT _)) -> Just (Shift 112)
    (96, Token (AS _)) -> Just (Shift 113)
    (96, Token (QVARID _)) -> Just (Shift 114)
    (97, Token (LPAREN _)) -> Just (Shift 107)
    (97, Token (RPAREN _)) -> Just (Reduce 0 15)
    (97, Token (QCONID _)) -> Just (Shift 163)
    (97, Token (EXPORT _)) -> Just (Shift 112)
    (97, Token (AS _)) -> Just (Shift 113)
    (97, Token (QVARID _)) -> Just (Shift 114)
    (98, Token (LPAREN _)) -> Just (Shift 107)
    (98, Token (QCONID _)) -> Just (Shift 163)
    (98, Token (EXPORT _)) -> Just (Shift 112)
    (98, Token (AS _)) -> Just (Shift 113)
    (98, Token (QVARID _)) -> Just (Shift 114)
    (99, Token (LPAREN _)) -> Just (Shift 107)
    (99, Token (RPAREN _)) -> Just (Shift 169)
    (99, Token (DOT_DOT _)) -> Just (Shift 172)
    (99, Token (QCONID _)) -> Just (Shift 163)
    (99, Token (EXPORT _)) -> Just (Shift 112)
    (99, Token (AS _)) -> Just (Shift 113)
    (99, Token (QVARID _)) -> Just (Shift 114)
    (100, Token (LPAREN _)) -> Just (Shift 108)
    (100, Token (EXPORT _)) -> Just (Shift 112)
    (100, Token (AS _)) -> Just (Shift 113)
    (100, Token (QVARID _)) -> Just (Shift 114)
    (101, Token (RBRACE _)) -> Just (Shift 348)
    (101, Token (LPAREN _)) -> Just (Shift 108)
    (101, Token (EXPORT _)) -> Just (Shift 112)
    (101, Token (AS _)) -> Just (Shift 113)
    (101, Token (QVARID _)) -> Just (Shift 114)
    (102, Token (LPAREN _)) -> Just (Shift 108)
    (102, Token (EXPORT _)) -> Just (Shift 112)
    (102, Token (AS _)) -> Just (Shift 113)
    (102, Token (QVARID _)) -> Just (Shift 114)
    (103, Token (LPAREN _)) -> Just (Shift 108)
    (103, Token (EXPORT _)) -> Just (Shift 112)
    (103, Token (AS _)) -> Just (Shift 113)
    (103, Token (QVARID _)) -> Just (Shift 114)
    (104, Token (LPAREN _)) -> Just (Shift 108)
    (104, Token (EXPORT _)) -> Just (Shift 112)
    (104, Token (AS _)) -> Just (Shift 113)
    (104, Token (QVARID _)) -> Just (Shift 114)
    (105, Token (LPAREN _)) -> Just (Shift 108)
    (105, Token (EXPORT _)) -> Just (Shift 112)
    (105, Token (AS _)) -> Just (Shift 113)
    (105, Token (QVARID _)) -> Just (Shift 114)
    (106, Token (LPAREN _)) -> Just (Shift 108)
    (106, Token (EXPORT _)) -> Just (Shift 112)
    (106, Token (AS _)) -> Just (Shift 113)
    (106, Token (QVARID _)) -> Just (Shift 114)
    (107, Token (MINUS _)) -> Just (Shift 111)
    (107, Token (QVARSYM _)) -> Just (Shift 115)
    (107, Token (QCONSYM _)) -> Just (Shift 164)
    (108, Token (MINUS _)) -> Just (Shift 111)
    (108, Token (QVARSYM _)) -> Just (Shift 115)
    (109, Token (WHERE _)) -> Just (Reduce 3 220)
    (109, Token (LBRACE _)) -> Just (Reduce 3 220)
    (109, Token (RBRACE _)) -> Just (Reduce 3 220)
    (109, Token (LPAREN _)) -> Just (Reduce 3 220)
    (109, Token (RPAREN _)) -> Just (Reduce 3 220)
    (109, Token (COMMA _)) -> Just (Reduce 3 220)
    (109, Token (DOT_DOT _)) -> Just (Reduce 3 220)
    (109, Token (SEMICOLON _)) -> Just (Reduce 3 220)
    (109, Token (EQUAL _)) -> Just (Reduce 3 220)
    (109, Token (PIPE _)) -> Just (Reduce 3 220)
    (109, Token (COLON_COLON _)) -> Just (Reduce 3 220)
    (109, Token (MINUS _)) -> Just (Reduce 3 220)
    (109, Token (INFIXL _)) -> Just (Reduce 3 220)
    (109, Token (INFIXR _)) -> Just (Reduce 3 220)
    (109, Token (INFIX _)) -> Just (Reduce 3 220)
    (109, Token (RARROW _)) -> Just (Reduce 3 220)
    (109, Token (LBRACKET _)) -> Just (Reduce 3 220)
    (109, Token (RBRACKET _)) -> Just (Reduce 3 220)
    (109, Token (QCONID _)) -> Just (Reduce 3 220)
    (109, Token (EXPORT _)) -> Just (Reduce 3 220)
    (109, Token (AS _)) -> Just (Reduce 3 220)
    (109, Token (QVARID _)) -> Just (Reduce 3 220)
    (109, Token (STRING _)) -> Just (Reduce 3 220)
    (109, Token (LARROW _)) -> Just (Reduce 3 220)
    (109, Token (LET _)) -> Just (Reduce 3 220)
    (109, Token (LAMBDA _)) -> Just (Reduce 3 220)
    (109, Token (IF _)) -> Just (Reduce 3 220)
    (109, Token (THEN _)) -> Just (Reduce 3 220)
    (109, Token (ELSE _)) -> Just (Reduce 3 220)
    (109, Token (QVARSYM _)) -> Just (Reduce 3 220)
    (109, Token (BACKQUOTE _)) -> Just (Reduce 3 220)
    (109, Token (QCONSYM _)) -> Just (Reduce 3 220)
    (109, Token (CASE _)) -> Just (Reduce 3 220)
    (109, Token (OF _)) -> Just (Reduce 3 220)
    (109, Token (DO _)) -> Just (Reduce 3 220)
    (109, Token (INTEGER _)) -> Just (Reduce 3 220)
    (110, Token (WHERE _)) -> Just (Reduce 3 221)
    (110, Token (LBRACE _)) -> Just (Reduce 3 221)
    (110, Token (RBRACE _)) -> Just (Reduce 3 221)
    (110, Token (LPAREN _)) -> Just (Reduce 3 221)
    (110, Token (RPAREN _)) -> Just (Reduce 3 221)
    (110, Token (COMMA _)) -> Just (Reduce 3 221)
    (110, Token (DOT_DOT _)) -> Just (Reduce 3 221)
    (110, Token (SEMICOLON _)) -> Just (Reduce 3 221)
    (110, Token (EQUAL _)) -> Just (Reduce 3 221)
    (110, Token (PIPE _)) -> Just (Reduce 3 221)
    (110, Token (COLON_COLON _)) -> Just (Reduce 3 221)
    (110, Token (MINUS _)) -> Just (Reduce 3 221)
    (110, Token (INFIXL _)) -> Just (Reduce 3 221)
    (110, Token (INFIXR _)) -> Just (Reduce 3 221)
    (110, Token (INFIX _)) -> Just (Reduce 3 221)
    (110, Token (RARROW _)) -> Just (Reduce 3 221)
    (110, Token (LBRACKET _)) -> Just (Reduce 3 221)
    (110, Token (RBRACKET _)) -> Just (Reduce 3 221)
    (110, Token (QCONID _)) -> Just (Reduce 3 221)
    (110, Token (EXPORT _)) -> Just (Reduce 3 221)
    (110, Token (AS _)) -> Just (Reduce 3 221)
    (110, Token (QVARID _)) -> Just (Reduce 3 221)
    (110, Token (STRING _)) -> Just (Reduce 3 221)
    (110, Token (LARROW _)) -> Just (Reduce 3 221)
    (110, Token (LET _)) -> Just (Reduce 3 221)
    (110, Token (LAMBDA _)) -> Just (Reduce 3 221)
    (110, Token (IF _)) -> Just (Reduce 3 221)
    (110, Token (THEN _)) -> Just (Reduce 3 221)
    (110, Token (ELSE _)) -> Just (Reduce 3 221)
    (110, Token (QVARSYM _)) -> Just (Reduce 3 221)
    (110, Token (BACKQUOTE _)) -> Just (Reduce 3 221)
    (110, Token (QCONSYM _)) -> Just (Reduce 3 221)
    (110, Token (CASE _)) -> Just (Reduce 3 221)
    (110, Token (OF _)) -> Just (Reduce 3 221)
    (110, Token (DO _)) -> Just (Reduce 3 221)
    (110, Token (INTEGER _)) -> Just (Reduce 3 221)
    (111, Token (RPAREN _)) -> Just (Shift 109)
    (112, Token (WHERE _)) -> Just (Reduce 1 218)
    (112, Token (LBRACE _)) -> Just (Reduce 1 218)
    (112, Token (RBRACE _)) -> Just (Reduce 1 218)
    (112, Token (LPAREN _)) -> Just (Reduce 1 218)
    (112, Token (RPAREN _)) -> Just (Reduce 1 218)
    (112, Token (COMMA _)) -> Just (Reduce 1 218)
    (112, Token (DOT_DOT _)) -> Just (Reduce 1 218)
    (112, Token (SEMICOLON _)) -> Just (Reduce 1 218)
    (112, Token (EQUAL _)) -> Just (Reduce 1 218)
    (112, Token (PIPE _)) -> Just (Reduce 1 218)
    (112, Token (COLON_COLON _)) -> Just (Reduce 1 218)
    (112, Token (MINUS _)) -> Just (Reduce 1 218)
    (112, Token (INFIXL _)) -> Just (Reduce 1 218)
    (112, Token (INFIXR _)) -> Just (Reduce 1 218)
    (112, Token (INFIX _)) -> Just (Reduce 1 218)
    (112, Token (RARROW _)) -> Just (Reduce 1 218)
    (112, Token (LBRACKET _)) -> Just (Reduce 1 218)
    (112, Token (RBRACKET _)) -> Just (Reduce 1 218)
    (112, Token (QCONID _)) -> Just (Reduce 1 218)
    (112, Token (EXPORT _)) -> Just (Reduce 1 218)
    (112, Token (AS _)) -> Just (Reduce 1 218)
    (112, Token (QVARID _)) -> Just (Reduce 1 218)
    (112, Token (STRING _)) -> Just (Reduce 1 218)
    (112, Token (LARROW _)) -> Just (Reduce 1 218)
    (112, Token (LET _)) -> Just (Reduce 1 218)
    (112, Token (LAMBDA _)) -> Just (Reduce 1 218)
    (112, Token (IF _)) -> Just (Reduce 1 218)
    (112, Token (THEN _)) -> Just (Reduce 1 218)
    (112, Token (ELSE _)) -> Just (Reduce 1 218)
    (112, Token (QVARSYM _)) -> Just (Reduce 1 218)
    (112, Token (BACKQUOTE _)) -> Just (Reduce 1 218)
    (112, Token (QCONSYM _)) -> Just (Reduce 1 218)
    (112, Token (CASE _)) -> Just (Reduce 1 218)
    (112, Token (OF _)) -> Just (Reduce 1 218)
    (112, Token (DO _)) -> Just (Reduce 1 218)
    (112, Token (INTEGER _)) -> Just (Reduce 1 218)
    (113, Token (WHERE _)) -> Just (Reduce 1 217)
    (113, Token (LBRACE _)) -> Just (Reduce 1 217)
    (113, Token (RBRACE _)) -> Just (Reduce 1 217)
    (113, Token (LPAREN _)) -> Just (Reduce 1 217)
    (113, Token (RPAREN _)) -> Just (Reduce 1 217)
    (113, Token (COMMA _)) -> Just (Reduce 1 217)
    (113, Token (DOT_DOT _)) -> Just (Reduce 1 217)
    (113, Token (SEMICOLON _)) -> Just (Reduce 1 217)
    (113, Token (EQUAL _)) -> Just (Reduce 1 217)
    (113, Token (PIPE _)) -> Just (Reduce 1 217)
    (113, Token (COLON_COLON _)) -> Just (Reduce 1 217)
    (113, Token (MINUS _)) -> Just (Reduce 1 217)
    (113, Token (INFIXL _)) -> Just (Reduce 1 217)
    (113, Token (INFIXR _)) -> Just (Reduce 1 217)
    (113, Token (INFIX _)) -> Just (Reduce 1 217)
    (113, Token (RARROW _)) -> Just (Reduce 1 217)
    (113, Token (LBRACKET _)) -> Just (Reduce 1 217)
    (113, Token (RBRACKET _)) -> Just (Reduce 1 217)
    (113, Token (QCONID _)) -> Just (Reduce 1 217)
    (113, Token (EXPORT _)) -> Just (Reduce 1 217)
    (113, Token (AS _)) -> Just (Reduce 1 217)
    (113, Token (QVARID _)) -> Just (Reduce 1 217)
    (113, Token (STRING _)) -> Just (Reduce 1 217)
    (113, Token (LARROW _)) -> Just (Reduce 1 217)
    (113, Token (LET _)) -> Just (Reduce 1 217)
    (113, Token (LAMBDA _)) -> Just (Reduce 1 217)
    (113, Token (IF _)) -> Just (Reduce 1 217)
    (113, Token (THEN _)) -> Just (Reduce 1 217)
    (113, Token (ELSE _)) -> Just (Reduce 1 217)
    (113, Token (QVARSYM _)) -> Just (Reduce 1 217)
    (113, Token (BACKQUOTE _)) -> Just (Reduce 1 217)
    (113, Token (QCONSYM _)) -> Just (Reduce 1 217)
    (113, Token (CASE _)) -> Just (Reduce 1 217)
    (113, Token (OF _)) -> Just (Reduce 1 217)
    (113, Token (DO _)) -> Just (Reduce 1 217)
    (113, Token (INTEGER _)) -> Just (Reduce 1 217)
    (114, Token (WHERE _)) -> Just (Reduce 1 219)
    (114, Token (LBRACE _)) -> Just (Reduce 1 219)
    (114, Token (RBRACE _)) -> Just (Reduce 1 219)
    (114, Token (LPAREN _)) -> Just (Reduce 1 219)
    (114, Token (RPAREN _)) -> Just (Reduce 1 219)
    (114, Token (COMMA _)) -> Just (Reduce 1 219)
    (114, Token (DOT_DOT _)) -> Just (Reduce 1 219)
    (114, Token (SEMICOLON _)) -> Just (Reduce 1 219)
    (114, Token (EQUAL _)) -> Just (Reduce 1 219)
    (114, Token (PIPE _)) -> Just (Reduce 1 219)
    (114, Token (COLON_COLON _)) -> Just (Reduce 1 219)
    (114, Token (MINUS _)) -> Just (Reduce 1 219)
    (114, Token (INFIXL _)) -> Just (Reduce 1 219)
    (114, Token (INFIXR _)) -> Just (Reduce 1 219)
    (114, Token (INFIX _)) -> Just (Reduce 1 219)
    (114, Token (RARROW _)) -> Just (Reduce 1 219)
    (114, Token (LBRACKET _)) -> Just (Reduce 1 219)
    (114, Token (RBRACKET _)) -> Just (Reduce 1 219)
    (114, Token (QCONID _)) -> Just (Reduce 1 219)
    (114, Token (EXPORT _)) -> Just (Reduce 1 219)
    (114, Token (AS _)) -> Just (Reduce 1 219)
    (114, Token (QVARID _)) -> Just (Reduce 1 219)
    (114, Token (STRING _)) -> Just (Reduce 1 219)
    (114, Token (LARROW _)) -> Just (Reduce 1 219)
    (114, Token (LET _)) -> Just (Reduce 1 219)
    (114, Token (LAMBDA _)) -> Just (Reduce 1 219)
    (114, Token (IF _)) -> Just (Reduce 1 219)
    (114, Token (THEN _)) -> Just (Reduce 1 219)
    (114, Token (ELSE _)) -> Just (Reduce 1 219)
    (114, Token (QVARSYM _)) -> Just (Reduce 1 219)
    (114, Token (BACKQUOTE _)) -> Just (Reduce 1 219)
    (114, Token (QCONSYM _)) -> Just (Reduce 1 219)
    (114, Token (CASE _)) -> Just (Reduce 1 219)
    (114, Token (OF _)) -> Just (Reduce 1 219)
    (114, Token (DO _)) -> Just (Reduce 1 219)
    (114, Token (INTEGER _)) -> Just (Reduce 1 219)
    (115, Token (RPAREN _)) -> Just (Shift 110)
    (116, Token (LPAREN _)) -> Just (Shift 156)
    (116, Token (LBRACKET _)) -> Just (Shift 160)
    (116, Token (EXCL _)) -> Just (Shift 116)
    (116, Token (QCONID _)) -> Just (Shift 163)
    (116, Token (EXPORT _)) -> Just (Shift 339)
    (116, Token (AS _)) -> Just (Shift 340)
    (116, Token (QVARID _)) -> Just (Shift 341)
    (117, Token (LPAREN _)) -> Just (Shift 156)
    (117, Token (LBRACKET _)) -> Just (Shift 160)
    (117, Token (EXCL _)) -> Just (Shift 116)
    (117, Token (QCONID _)) -> Just (Shift 163)
    (117, Token (EXPORT _)) -> Just (Shift 339)
    (117, Token (AS _)) -> Just (Shift 340)
    (117, Token (QVARID _)) -> Just (Shift 341)
    (118, Token (WHERE _)) -> Just (Shift 240)
    (118, Token (RBRACE _)) -> Just (Reduce 0 65)
    (118, Token (LPAREN _)) -> Just (Shift 156)
    (118, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (118, Token (DARROW _)) -> Just (Shift 121)
    (118, Token (LBRACKET _)) -> Just (Shift 160)
    (118, Token (EXCL _)) -> Just (Shift 116)
    (118, Token (QCONID _)) -> Just (Shift 163)
    (118, Token (EXPORT _)) -> Just (Shift 339)
    (118, Token (AS _)) -> Just (Shift 340)
    (118, Token (QVARID _)) -> Just (Shift 341)
    (119, Token (LPAREN _)) -> Just (Shift 156)
    (119, Token (LBRACKET _)) -> Just (Shift 160)
    (119, Token (EXCL _)) -> Just (Shift 116)
    (119, Token (QCONID _)) -> Just (Shift 163)
    (119, Token (EXPORT _)) -> Just (Shift 339)
    (119, Token (AS _)) -> Just (Shift 340)
    (119, Token (QVARID _)) -> Just (Shift 341)
    (120, Token (WHERE _)) -> Just (Shift 242)
    (120, Token (RBRACE _)) -> Just (Reduce 0 75)
    (120, Token (LPAREN _)) -> Just (Shift 156)
    (120, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (120, Token (DARROW _)) -> Just (Shift 123)
    (120, Token (LBRACKET _)) -> Just (Shift 160)
    (120, Token (EXCL _)) -> Just (Shift 116)
    (120, Token (QCONID _)) -> Just (Shift 163)
    (120, Token (EXPORT _)) -> Just (Shift 339)
    (120, Token (AS _)) -> Just (Shift 340)
    (120, Token (QVARID _)) -> Just (Shift 341)
    (121, Token (LPAREN _)) -> Just (Shift 156)
    (121, Token (LBRACKET _)) -> Just (Shift 160)
    (121, Token (EXCL _)) -> Just (Shift 116)
    (121, Token (QCONID _)) -> Just (Shift 163)
    (121, Token (EXPORT _)) -> Just (Shift 339)
    (121, Token (AS _)) -> Just (Shift 340)
    (121, Token (QVARID _)) -> Just (Shift 341)
    (122, Token (WHERE _)) -> Just (Shift 240)
    (122, Token (RBRACE _)) -> Just (Reduce 0 65)
    (122, Token (LPAREN _)) -> Just (Shift 156)
    (122, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (122, Token (LBRACKET _)) -> Just (Shift 160)
    (122, Token (EXCL _)) -> Just (Shift 116)
    (122, Token (QCONID _)) -> Just (Shift 163)
    (122, Token (EXPORT _)) -> Just (Shift 339)
    (122, Token (AS _)) -> Just (Shift 340)
    (122, Token (QVARID _)) -> Just (Shift 341)
    (123, Token (LPAREN _)) -> Just (Shift 156)
    (123, Token (LBRACKET _)) -> Just (Shift 160)
    (123, Token (EXCL _)) -> Just (Shift 116)
    (123, Token (QCONID _)) -> Just (Shift 163)
    (123, Token (EXPORT _)) -> Just (Shift 339)
    (123, Token (AS _)) -> Just (Shift 340)
    (123, Token (QVARID _)) -> Just (Shift 341)
    (124, Token (WHERE _)) -> Just (Shift 242)
    (124, Token (RBRACE _)) -> Just (Reduce 0 75)
    (124, Token (LPAREN _)) -> Just (Shift 156)
    (124, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (124, Token (LBRACKET _)) -> Just (Shift 160)
    (124, Token (EXCL _)) -> Just (Shift 116)
    (124, Token (QCONID _)) -> Just (Shift 163)
    (124, Token (EXPORT _)) -> Just (Shift 339)
    (124, Token (AS _)) -> Just (Shift 340)
    (124, Token (QVARID _)) -> Just (Shift 341)
    (125, Token (LPAREN _)) -> Just (Shift 156)
    (125, Token (LBRACKET _)) -> Just (Shift 160)
    (125, Token (EXCL _)) -> Just (Shift 116)
    (125, Token (QCONID _)) -> Just (Shift 163)
    (125, Token (EXPORT _)) -> Just (Shift 339)
    (125, Token (AS _)) -> Just (Shift 340)
    (125, Token (QVARID _)) -> Just (Shift 341)
    (126, Token (WHERE _)) -> Just (Reduce 1 100)
    (126, Token (RBRACE _)) -> Just (Reduce 1 100)
    (126, Token (LPAREN _)) -> Just (Shift 156)
    (126, Token (RPAREN _)) -> Just (Reduce 1 100)
    (126, Token (COMMA _)) -> Just (Reduce 1 100)
    (126, Token (DOT_DOT _)) -> Just (Reduce 1 100)
    (126, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (126, Token (EQUAL _)) -> Just (Reduce 1 100)
    (126, Token (DARROW _)) -> Just (Shift 128)
    (126, Token (PIPE _)) -> Just (Reduce 1 100)
    (126, Token (RARROW _)) -> Just (Shift 127)
    (126, Token (LBRACKET _)) -> Just (Shift 160)
    (126, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (126, Token (EXCL _)) -> Just (Shift 116)
    (126, Token (QCONID _)) -> Just (Shift 163)
    (126, Token (EXPORT _)) -> Just (Shift 339)
    (126, Token (AS _)) -> Just (Shift 340)
    (126, Token (QVARID _)) -> Just (Shift 341)
    (126, Token (LARROW _)) -> Just (Reduce 1 100)
    (126, Token (THEN _)) -> Just (Reduce 1 100)
    (126, Token (ELSE _)) -> Just (Reduce 1 100)
    (126, Token (OF _)) -> Just (Reduce 1 100)
    (127, Token (LPAREN _)) -> Just (Shift 156)
    (127, Token (LBRACKET _)) -> Just (Shift 160)
    (127, Token (EXCL _)) -> Just (Shift 116)
    (127, Token (QCONID _)) -> Just (Shift 163)
    (127, Token (EXPORT _)) -> Just (Shift 339)
    (127, Token (AS _)) -> Just (Shift 340)
    (127, Token (QVARID _)) -> Just (Shift 341)
    (128, Token (LPAREN _)) -> Just (Shift 156)
    (128, Token (LBRACKET _)) -> Just (Shift 160)
    (128, Token (EXCL _)) -> Just (Shift 116)
    (128, Token (QCONID _)) -> Just (Shift 163)
    (128, Token (EXPORT _)) -> Just (Shift 339)
    (128, Token (AS _)) -> Just (Shift 340)
    (128, Token (QVARID _)) -> Just (Shift 341)
    (129, Token (WHERE _)) -> Just (Reduce 1 100)
    (129, Token (RBRACE _)) -> Just (Reduce 1 100)
    (129, Token (LPAREN _)) -> Just (Shift 156)
    (129, Token (RPAREN _)) -> Just (Reduce 1 100)
    (129, Token (COMMA _)) -> Just (Reduce 1 100)
    (129, Token (DOT_DOT _)) -> Just (Reduce 1 100)
    (129, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (129, Token (EQUAL _)) -> Just (Reduce 1 100)
    (129, Token (PIPE _)) -> Just (Reduce 1 100)
    (129, Token (RARROW _)) -> Just (Shift 127)
    (129, Token (LBRACKET _)) -> Just (Shift 160)
    (129, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (129, Token (EXCL _)) -> Just (Shift 116)
    (129, Token (QCONID _)) -> Just (Shift 163)
    (129, Token (EXPORT _)) -> Just (Shift 339)
    (129, Token (AS _)) -> Just (Shift 340)
    (129, Token (QVARID _)) -> Just (Shift 341)
    (129, Token (LARROW _)) -> Just (Reduce 1 100)
    (129, Token (THEN _)) -> Just (Reduce 1 100)
    (129, Token (ELSE _)) -> Just (Reduce 1 100)
    (129, Token (OF _)) -> Just (Reduce 1 100)
    (130, Token (LPAREN _)) -> Just (Shift 156)
    (130, Token (LBRACKET _)) -> Just (Shift 160)
    (130, Token (EXCL _)) -> Just (Shift 116)
    (130, Token (QCONID _)) -> Just (Shift 163)
    (130, Token (EXPORT _)) -> Just (Shift 339)
    (130, Token (AS _)) -> Just (Shift 340)
    (130, Token (QVARID _)) -> Just (Shift 341)
    (131, Token (RBRACE _)) -> Just (Reduce 0 119)
    (131, Token (LPAREN _)) -> Just (Shift 156)
    (131, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (131, Token (EQUAL _)) -> Just (Shift 134)
    (131, Token (DERIVING _)) -> Just (Reduce 0 119)
    (131, Token (DARROW _)) -> Just (Shift 132)
    (131, Token (LBRACKET _)) -> Just (Shift 160)
    (131, Token (EXCL _)) -> Just (Shift 116)
    (131, Token (QCONID _)) -> Just (Shift 163)
    (131, Token (EXPORT _)) -> Just (Shift 339)
    (131, Token (AS _)) -> Just (Shift 340)
    (131, Token (QVARID _)) -> Just (Shift 341)
    (132, Token (LPAREN _)) -> Just (Shift 156)
    (132, Token (LBRACKET _)) -> Just (Shift 160)
    (132, Token (EXCL _)) -> Just (Shift 116)
    (132, Token (QCONID _)) -> Just (Shift 163)
    (132, Token (EXPORT _)) -> Just (Shift 339)
    (132, Token (AS _)) -> Just (Shift 340)
    (132, Token (QVARID _)) -> Just (Shift 341)
    (133, Token (RBRACE _)) -> Just (Reduce 0 119)
    (133, Token (LPAREN _)) -> Just (Shift 156)
    (133, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (133, Token (EQUAL _)) -> Just (Shift 134)
    (133, Token (DERIVING _)) -> Just (Reduce 0 119)
    (133, Token (LBRACKET _)) -> Just (Shift 160)
    (133, Token (EXCL _)) -> Just (Shift 116)
    (133, Token (QCONID _)) -> Just (Shift 163)
    (133, Token (EXPORT _)) -> Just (Shift 339)
    (133, Token (AS _)) -> Just (Shift 340)
    (133, Token (QVARID _)) -> Just (Shift 341)
    (134, Token (LPAREN _)) -> Just (Shift 156)
    (134, Token (LBRACKET _)) -> Just (Shift 160)
    (134, Token (EXCL _)) -> Just (Shift 116)
    (134, Token (QCONID _)) -> Just (Shift 163)
    (134, Token (EXPORT _)) -> Just (Shift 339)
    (134, Token (AS _)) -> Just (Shift 340)
    (134, Token (QVARID _)) -> Just (Shift 341)
    (135, Token (LPAREN _)) -> Just (Shift 156)
    (135, Token (LBRACKET _)) -> Just (Shift 160)
    (135, Token (EXCL _)) -> Just (Shift 116)
    (135, Token (QCONID _)) -> Just (Shift 163)
    (135, Token (EXPORT _)) -> Just (Shift 339)
    (135, Token (AS _)) -> Just (Shift 340)
    (135, Token (QVARID _)) -> Just (Shift 341)
    (136, Token (LPAREN _)) -> Just (Shift 161)
    (136, Token (QCONID _)) -> Just (Shift 163)
    (137, Token (RBRACE _)) -> Just (Reduce 1 123)
    (137, Token (LPAREN _)) -> Just (Shift 156)
    (137, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (137, Token (DERIVING _)) -> Just (Reduce 1 123)
    (137, Token (PIPE _)) -> Just (Reduce 1 123)
    (137, Token (LBRACKET _)) -> Just (Shift 160)
    (137, Token (EXCL _)) -> Just (Shift 116)
    (137, Token (QCONID _)) -> Just (Shift 163)
    (137, Token (EXPORT _)) -> Just (Shift 339)
    (137, Token (AS _)) -> Just (Shift 340)
    (137, Token (QVARID _)) -> Just (Shift 341)
    (137, Token (BACKQUOTE _)) -> Just (Shift 353)
    (137, Token (QCONSYM _)) -> Just (Shift 355)
    (138, Token (LPAREN _)) -> Just (Shift 156)
    (138, Token (LBRACKET _)) -> Just (Shift 160)
    (138, Token (EXCL _)) -> Just (Shift 116)
    (138, Token (QCONID _)) -> Just (Shift 163)
    (138, Token (EXPORT _)) -> Just (Shift 339)
    (138, Token (AS _)) -> Just (Shift 340)
    (138, Token (QVARID _)) -> Just (Shift 341)
    (139, Token (RBRACE _)) -> Just (Reduce 3 124)
    (139, Token (LPAREN _)) -> Just (Shift 156)
    (139, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (139, Token (DERIVING _)) -> Just (Reduce 3 124)
    (139, Token (PIPE _)) -> Just (Reduce 3 124)
    (139, Token (LBRACKET _)) -> Just (Shift 160)
    (139, Token (EXCL _)) -> Just (Shift 116)
    (139, Token (QCONID _)) -> Just (Shift 163)
    (139, Token (EXPORT _)) -> Just (Shift 339)
    (139, Token (AS _)) -> Just (Shift 340)
    (139, Token (QVARID _)) -> Just (Shift 341)
    (140, Token (LPAREN _)) -> Just (Shift 156)
    (140, Token (LBRACKET _)) -> Just (Shift 160)
    (140, Token (EXCL _)) -> Just (Shift 116)
    (140, Token (QCONID _)) -> Just (Shift 163)
    (140, Token (EXPORT _)) -> Just (Shift 339)
    (140, Token (AS _)) -> Just (Shift 340)
    (140, Token (QVARID _)) -> Just (Shift 341)
    (141, Token (RBRACE _)) -> Just (Reduce 1 100)
    (141, Token (LPAREN _)) -> Just (Shift 156)
    (141, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (141, Token (DARROW _)) -> Just (Shift 146)
    (141, Token (RARROW _)) -> Just (Shift 127)
    (141, Token (LBRACKET _)) -> Just (Shift 160)
    (141, Token (EXCL _)) -> Just (Shift 116)
    (141, Token (QCONID _)) -> Just (Shift 163)
    (141, Token (EXPORT _)) -> Just (Shift 339)
    (141, Token (AS _)) -> Just (Shift 340)
    (141, Token (QVARID _)) -> Just (Shift 341)
    (142, Token (LPAREN _)) -> Just (Shift 156)
    (142, Token (LBRACKET _)) -> Just (Shift 160)
    (142, Token (EXCL _)) -> Just (Shift 116)
    (142, Token (QCONID _)) -> Just (Shift 163)
    (142, Token (EXPORT _)) -> Just (Shift 339)
    (142, Token (AS _)) -> Just (Shift 340)
    (142, Token (QVARID _)) -> Just (Shift 341)
    (143, Token (LPAREN _)) -> Just (Shift 156)
    (143, Token (LBRACKET _)) -> Just (Shift 160)
    (143, Token (EXCL _)) -> Just (Shift 116)
    (143, Token (QCONID _)) -> Just (Shift 163)
    (143, Token (EXPORT _)) -> Just (Shift 339)
    (143, Token (AS _)) -> Just (Shift 340)
    (143, Token (QVARID _)) -> Just (Shift 341)
    (144, Token (LPAREN _)) -> Just (Shift 156)
    (144, Token (LBRACKET _)) -> Just (Shift 160)
    (144, Token (EXCL _)) -> Just (Shift 116)
    (144, Token (QCONID _)) -> Just (Shift 163)
    (144, Token (EXPORT _)) -> Just (Shift 339)
    (144, Token (AS _)) -> Just (Shift 340)
    (144, Token (QVARID _)) -> Just (Shift 341)
    (145, Token (LPAREN _)) -> Just (Shift 156)
    (145, Token (LBRACKET _)) -> Just (Shift 160)
    (145, Token (EXCL _)) -> Just (Shift 116)
    (145, Token (QCONID _)) -> Just (Shift 163)
    (145, Token (EXPORT _)) -> Just (Shift 339)
    (145, Token (AS _)) -> Just (Shift 340)
    (145, Token (QVARID _)) -> Just (Shift 341)
    (146, Token (LPAREN _)) -> Just (Shift 156)
    (146, Token (LBRACKET _)) -> Just (Shift 160)
    (146, Token (EXCL _)) -> Just (Shift 116)
    (146, Token (QCONID _)) -> Just (Shift 163)
    (146, Token (EXPORT _)) -> Just (Shift 339)
    (146, Token (AS _)) -> Just (Shift 340)
    (146, Token (QVARID _)) -> Just (Shift 341)
    (147, Token (LPAREN _)) -> Just (Shift 156)
    (147, Token (LBRACKET _)) -> Just (Shift 160)
    (147, Token (EXCL _)) -> Just (Shift 116)
    (147, Token (QCONID _)) -> Just (Shift 163)
    (147, Token (EXPORT _)) -> Just (Shift 339)
    (147, Token (AS _)) -> Just (Shift 340)
    (147, Token (QVARID _)) -> Just (Shift 341)
    (148, Token (LPAREN _)) -> Just (Shift 156)
    (148, Token (LBRACKET _)) -> Just (Shift 160)
    (148, Token (EXCL _)) -> Just (Shift 116)
    (148, Token (QCONID _)) -> Just (Shift 163)
    (148, Token (EXPORT _)) -> Just (Shift 339)
    (148, Token (AS _)) -> Just (Shift 340)
    (148, Token (QVARID _)) -> Just (Shift 341)
    (149, Token (LBRACE _)) -> Just (Shift 103)
    (149, Token (LPAREN _)) -> Just (Shift 156)
    (149, Token (LBRACKET _)) -> Just (Shift 160)
    (149, Token (EXCL _)) -> Just (Shift 116)
    (149, Token (QCONID _)) -> Just (Shift 163)
    (149, Token (EXPORT _)) -> Just (Shift 339)
    (149, Token (AS _)) -> Just (Shift 340)
    (149, Token (QVARID _)) -> Just (Shift 341)
    (150, Token (LPAREN _)) -> Just (Shift 156)
    (150, Token (LBRACKET _)) -> Just (Shift 160)
    (150, Token (EXCL _)) -> Just (Shift 116)
    (150, Token (QCONID _)) -> Just (Shift 163)
    (150, Token (EXPORT _)) -> Just (Shift 339)
    (150, Token (AS _)) -> Just (Shift 340)
    (150, Token (QVARID _)) -> Just (Shift 341)
    (151, Token (LPAREN _)) -> Just (Shift 156)
    (151, Token (EQUAL _)) -> Just (Shift 136)
    (151, Token (DARROW _)) -> Just (Shift 153)
    (151, Token (LBRACKET _)) -> Just (Shift 160)
    (151, Token (EXCL _)) -> Just (Shift 116)
    (151, Token (QCONID _)) -> Just (Shift 163)
    (151, Token (EXPORT _)) -> Just (Shift 339)
    (151, Token (AS _)) -> Just (Shift 340)
    (151, Token (QVARID _)) -> Just (Shift 341)
    (152, Token (LPAREN _)) -> Just (Shift 156)
    (152, Token (LBRACKET _)) -> Just (Shift 160)
    (152, Token (EXCL _)) -> Just (Shift 116)
    (152, Token (QCONID _)) -> Just (Shift 163)
    (152, Token (EXPORT _)) -> Just (Shift 339)
    (152, Token (AS _)) -> Just (Shift 340)
    (152, Token (QVARID _)) -> Just (Shift 341)
    (153, Token (LPAREN _)) -> Just (Shift 156)
    (153, Token (LBRACKET _)) -> Just (Shift 160)
    (153, Token (EXCL _)) -> Just (Shift 116)
    (153, Token (QCONID _)) -> Just (Shift 163)
    (153, Token (EXPORT _)) -> Just (Shift 339)
    (153, Token (AS _)) -> Just (Shift 340)
    (153, Token (QVARID _)) -> Just (Shift 341)
    (154, Token (LPAREN _)) -> Just (Shift 156)
    (154, Token (EQUAL _)) -> Just (Shift 142)
    (154, Token (LBRACKET _)) -> Just (Shift 160)
    (154, Token (EXCL _)) -> Just (Shift 116)
    (154, Token (QCONID _)) -> Just (Shift 163)
    (154, Token (EXPORT _)) -> Just (Shift 339)
    (154, Token (AS _)) -> Just (Shift 340)
    (154, Token (QVARID _)) -> Just (Shift 341)
    (155, Token (LPAREN _)) -> Just (Shift 156)
    (155, Token (EQUAL _)) -> Just (Shift 136)
    (155, Token (LBRACKET _)) -> Just (Shift 160)
    (155, Token (EXCL _)) -> Just (Shift 116)
    (155, Token (QCONID _)) -> Just (Shift 163)
    (155, Token (EXPORT _)) -> Just (Shift 339)
    (155, Token (AS _)) -> Just (Shift 340)
    (155, Token (QVARID _)) -> Just (Shift 341)
    (156, Token (LPAREN _)) -> Just (Shift 156)
    (156, Token (RPAREN _)) -> Just (Shift 331)
    (156, Token (COMMA _)) -> Just (Shift 344)
    (156, Token (RARROW _)) -> Just (Shift 334)
    (156, Token (LBRACKET _)) -> Just (Shift 160)
    (156, Token (EXCL _)) -> Just (Shift 116)
    (156, Token (QCONID _)) -> Just (Shift 163)
    (156, Token (EXPORT _)) -> Just (Shift 339)
    (156, Token (AS _)) -> Just (Shift 340)
    (156, Token (QVARID _)) -> Just (Shift 341)
    (156, Token (QCONSYM _)) -> Just (Shift 164)
    (157, Token (LPAREN _)) -> Just (Shift 156)
    (157, Token (RPAREN _)) -> Just (Shift 186)
    (157, Token (LBRACKET _)) -> Just (Shift 160)
    (157, Token (EXCL _)) -> Just (Shift 116)
    (157, Token (QCONID _)) -> Just (Shift 163)
    (157, Token (EXPORT _)) -> Just (Shift 339)
    (157, Token (AS _)) -> Just (Shift 340)
    (157, Token (QVARID _)) -> Just (Shift 341)
    (158, Token (LPAREN _)) -> Just (Shift 156)
    (158, Token (LBRACKET _)) -> Just (Shift 160)
    (158, Token (EXCL _)) -> Just (Shift 116)
    (158, Token (QCONID _)) -> Just (Shift 163)
    (158, Token (EXPORT _)) -> Just (Shift 339)
    (158, Token (AS _)) -> Just (Shift 340)
    (158, Token (QVARID _)) -> Just (Shift 341)
    (159, Token (LPAREN _)) -> Just (Shift 156)
    (159, Token (LBRACKET _)) -> Just (Shift 160)
    (159, Token (EXCL _)) -> Just (Shift 116)
    (159, Token (QCONID _)) -> Just (Shift 163)
    (159, Token (EXPORT _)) -> Just (Shift 339)
    (159, Token (AS _)) -> Just (Shift 340)
    (159, Token (QVARID _)) -> Just (Shift 341)
    (160, Token (LPAREN _)) -> Just (Shift 156)
    (160, Token (LBRACKET _)) -> Just (Shift 160)
    (160, Token (RBRACKET _)) -> Just (Shift 335)
    (160, Token (EXCL _)) -> Just (Shift 116)
    (160, Token (QCONID _)) -> Just (Shift 163)
    (160, Token (EXPORT _)) -> Just (Shift 339)
    (160, Token (AS _)) -> Just (Shift 340)
    (160, Token (QVARID _)) -> Just (Shift 341)
    (161, Token (QCONSYM _)) -> Just (Shift 164)
    (162, Token (WHERE _)) -> Just (Reduce 3 223)
    (162, Token (LBRACE _)) -> Just (Reduce 3 223)
    (162, Token (RBRACE _)) -> Just (Reduce 3 223)
    (162, Token (LPAREN _)) -> Just (Reduce 3 223)
    (162, Token (RPAREN _)) -> Just (Reduce 3 223)
    (162, Token (COMMA _)) -> Just (Reduce 3 223)
    (162, Token (DOT_DOT _)) -> Just (Reduce 3 223)
    (162, Token (SEMICOLON _)) -> Just (Reduce 3 223)
    (162, Token (EQUAL _)) -> Just (Reduce 3 223)
    (162, Token (DERIVING _)) -> Just (Reduce 3 223)
    (162, Token (DARROW _)) -> Just (Reduce 3 223)
    (162, Token (PIPE _)) -> Just (Reduce 3 223)
    (162, Token (COLON_COLON _)) -> Just (Reduce 3 223)
    (162, Token (MINUS _)) -> Just (Reduce 3 223)
    (162, Token (INFIXL _)) -> Just (Reduce 3 223)
    (162, Token (INFIXR _)) -> Just (Reduce 3 223)
    (162, Token (INFIX _)) -> Just (Reduce 3 223)
    (162, Token (RARROW _)) -> Just (Reduce 3 223)
    (162, Token (LBRACKET _)) -> Just (Reduce 3 223)
    (162, Token (RBRACKET _)) -> Just (Reduce 3 223)
    (162, Token (EXCL _)) -> Just (Reduce 3 223)
    (162, Token (QCONID _)) -> Just (Reduce 3 223)
    (162, Token (EXPORT _)) -> Just (Reduce 3 223)
    (162, Token (AS _)) -> Just (Reduce 3 223)
    (162, Token (QVARID _)) -> Just (Reduce 3 223)
    (162, Token (LARROW _)) -> Just (Reduce 3 223)
    (162, Token (THEN _)) -> Just (Reduce 3 223)
    (162, Token (ELSE _)) -> Just (Reduce 3 223)
    (162, Token (QVARSYM _)) -> Just (Reduce 3 223)
    (162, Token (BACKQUOTE _)) -> Just (Reduce 3 223)
    (162, Token (QCONSYM _)) -> Just (Reduce 3 223)
    (162, Token (OF _)) -> Just (Reduce 3 223)
    (162, Token (INTEGER _)) -> Just (Reduce 3 223)
    (163, Token (WHERE _)) -> Just (Reduce 1 222)
    (163, Token (LBRACE _)) -> Just (Reduce 1 222)
    (163, Token (RBRACE _)) -> Just (Reduce 1 222)
    (163, Token (LPAREN _)) -> Just (Reduce 1 222)
    (163, Token (RPAREN _)) -> Just (Reduce 1 222)
    (163, Token (COMMA _)) -> Just (Reduce 1 222)
    (163, Token (DOT_DOT _)) -> Just (Reduce 1 222)
    (163, Token (SEMICOLON _)) -> Just (Reduce 1 222)
    (163, Token (EQUAL _)) -> Just (Reduce 1 222)
    (163, Token (DERIVING _)) -> Just (Reduce 1 222)
    (163, Token (DARROW _)) -> Just (Reduce 1 222)
    (163, Token (PIPE _)) -> Just (Reduce 1 222)
    (163, Token (COLON_COLON _)) -> Just (Reduce 1 222)
    (163, Token (MINUS _)) -> Just (Reduce 1 222)
    (163, Token (INFIXL _)) -> Just (Reduce 1 222)
    (163, Token (INFIXR _)) -> Just (Reduce 1 222)
    (163, Token (INFIX _)) -> Just (Reduce 1 222)
    (163, Token (RARROW _)) -> Just (Reduce 1 222)
    (163, Token (LBRACKET _)) -> Just (Reduce 1 222)
    (163, Token (RBRACKET _)) -> Just (Reduce 1 222)
    (163, Token (EXCL _)) -> Just (Reduce 1 222)
    (163, Token (QCONID _)) -> Just (Reduce 1 222)
    (163, Token (EXPORT _)) -> Just (Reduce 1 222)
    (163, Token (AS _)) -> Just (Reduce 1 222)
    (163, Token (QVARID _)) -> Just (Reduce 1 222)
    (163, Token (LARROW _)) -> Just (Reduce 1 222)
    (163, Token (THEN _)) -> Just (Reduce 1 222)
    (163, Token (ELSE _)) -> Just (Reduce 1 222)
    (163, Token (QVARSYM _)) -> Just (Reduce 1 222)
    (163, Token (BACKQUOTE _)) -> Just (Reduce 1 222)
    (163, Token (QCONSYM _)) -> Just (Reduce 1 222)
    (163, Token (OF _)) -> Just (Reduce 1 222)
    (163, Token (INTEGER _)) -> Just (Reduce 1 222)
    (164, Token (RPAREN _)) -> Just (Shift 162)
    (165, Token (RPAREN _)) -> Just (Reduce 3 24)
    (166, Token (RPAREN _)) -> Just (Reduce 1 23)
    (166, Token (COMMA _)) -> Just (Shift 98)
    (167, Token (RPAREN _)) -> Just (Reduce 3 17)
    (168, Token (RPAREN _)) -> Just (Reduce 1 16)
    (168, Token (COMMA _)) -> Just (Shift 95)
    (169, Token (RPAREN _)) -> Just (Reduce 3 20)
    (169, Token (COMMA _)) -> Just (Reduce 3 20)
    (170, Token (RPAREN _)) -> Just (Reduce 4 21)
    (170, Token (COMMA _)) -> Just (Reduce 4 21)
    (171, Token (RPAREN _)) -> Just (Reduce 4 22)
    (171, Token (COMMA _)) -> Just (Reduce 4 22)
    (172, Token (RPAREN _)) -> Just (Shift 170)
    (173, Token (RPAREN _)) -> Just (Reduce 1 18)
    (173, Token (COMMA _)) -> Just (Reduce 1 18)
    (174, Token (LPAREN _)) -> Just (Shift 99)
    (174, Token (RPAREN _)) -> Just (Reduce 1 19)
    (174, Token (COMMA _)) -> Just (Reduce 1 19)
    (175, Token (RPAREN _)) -> Just (Shift 171)
    (176, Token (RPAREN _)) -> Just (Reduce 1 25)
    (176, Token (COMMA _)) -> Just (Reduce 1 25)
    (177, Token (RPAREN _)) -> Just (Reduce 1 26)
    (177, Token (COMMA _)) -> Just (Reduce 1 26)
    (178, Token (RPAREN _)) -> Just (Shift 182)
    (178, Token (QCONID _)) -> Just (Shift 233)
    (179, Token (RPAREN _)) -> Just (Shift 183)
    (179, Token (QCONID _)) -> Just (Shift 233)
    (180, Token (RPAREN _)) -> Just (Shift 184)
    (180, Token (QCONID _)) -> Just (Shift 233)
    (181, Token (RPAREN _)) -> Just (Shift 185)
    (181, Token (QCONID _)) -> Just (Shift 233)
    (182, Token (RBRACE _)) -> Just (Reduce 6 35)
    (182, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (183, Token (RBRACE _)) -> Just (Reduce 8 39)
    (183, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (184, Token (RBRACE _)) -> Just (Reduce 8 47)
    (184, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (185, Token (RBRACE _)) -> Just (Reduce 6 43)
    (185, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (186, Token (RBRACE _)) -> Just (Reduce 3 53)
    (186, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (187, Token (RBRACE _)) -> Just (Reduce 8 31)
    (187, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (188, Token (RBRACE _)) -> Just (Reduce 7 30)
    (188, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (189, Token (RBRACE _)) -> Just (Reduce 7 36)
    (189, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (190, Token (RBRACE _)) -> Just (Reduce 9 40)
    (190, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (191, Token (RBRACE _)) -> Just (Reduce 9 48)
    (191, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (192, Token (RBRACE _)) -> Just (Reduce 7 44)
    (192, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (193, Token (RBRACE _)) -> Just (Reduce 4 54)
    (193, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (194, Token (QCONID _)) -> Just (Reduce 0 234)
    (194, Token (QUALIFIED _)) -> Just (Shift 226)
    (195, Token (LPAREN _)) -> Just (Shift 96)
    (196, Token (LPAREN _)) -> Just (Shift 178)
    (196, Token (QCONID _)) -> Just (Shift 233)
    (197, Token (LPAREN _)) -> Just (Shift 179)
    (197, Token (QCONID _)) -> Just (Shift 233)
    (198, Token (LPAREN _)) -> Just (Shift 180)
    (198, Token (QCONID _)) -> Just (Shift 233)
    (199, Token (LPAREN _)) -> Just (Shift 181)
    (199, Token (QCONID _)) -> Just (Shift 233)
    (200, Token (LPAREN _)) -> Just (Shift 157)
    (201, Token (IMPORT _)) -> Just (Shift 246)
    (201, Token (EXPORT _)) -> Just (Shift 247)
    (202, Token (RBRACE _)) -> Just (Reduce 0 232)
    (202, Token (LPAREN _)) -> Just (Reduce 0 232)
    (202, Token (SEMICOLON _)) -> Just (Reduce 0 232)
    (202, Token (HIDING _)) -> Just (Reduce 0 232)
    (202, Token (AS _)) -> Just (Shift 9)
    (203, Token (RPAREN _)) -> Just (Shift 187)
    (204, Token (RPAREN _)) -> Just (Shift 188)
    (205, Token (RBRACE _)) -> Just (Reduce 4 29)
    (205, Token (LPAREN _)) -> Just (Shift 97)
    (205, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (205, Token (HIDING _)) -> Just (Shift 195)
    (206, Token (RBRACE _)) -> Just (Reduce 4 32)
    (206, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (207, Token (RBRACE _)) -> Just (Reduce 3 33)
    (207, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (207, Token (DERIVING _)) -> Just (Shift 196)
    (208, Token (RBRACE _)) -> Just (Reduce 5 37)
    (208, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (208, Token (DERIVING _)) -> Just (Shift 197)
    (209, Token (RBRACE _)) -> Just (Reduce 5 34)
    (209, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (210, Token (RBRACE _)) -> Just (Reduce 7 38)
    (210, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (211, Token (RBRACE _)) -> Just (Reduce 7 46)
    (211, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (212, Token (RBRACE _)) -> Just (Reduce 5 42)
    (212, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (213, Token (RPAREN _)) -> Just (Shift 189)
    (214, Token (RPAREN _)) -> Just (Shift 190)
    (215, Token (RPAREN _)) -> Just (Shift 191)
    (216, Token (RPAREN _)) -> Just (Shift 192)
    (217, Token (RBRACE _)) -> Just (Reduce 5 45)
    (217, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (217, Token (DERIVING _)) -> Just (Shift 198)
    (218, Token (RBRACE _)) -> Just (Reduce 3 41)
    (218, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (218, Token (DERIVING _)) -> Just (Shift 199)
    (219, Token (RBRACE _)) -> Just (Reduce 5 50)
    (219, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (220, Token (RBRACE _)) -> Just (Reduce 3 49)
    (220, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (221, Token (RBRACE _)) -> Just (Reduce 5 52)
    (221, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (222, Token (RBRACE _)) -> Just (Reduce 3 51)
    (222, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (223, Token (RPAREN _)) -> Just (Shift 193)
    (224, Token (RBRACE _)) -> Just (Reduce 2 55)
    (224, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (225, Token (RBRACE _)) -> Just (Reduce 1 56)
    (225, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (226, Token (QCONID _)) -> Just (Reduce 1 235)
    (227, Token (RBRACE _)) -> Just (Reduce 2 233)
    (227, Token (LPAREN _)) -> Just (Reduce 2 233)
    (227, Token (SEMICOLON _)) -> Just (Reduce 2 233)
    (227, Token (HIDING _)) -> Just (Reduce 2 233)
    (228, Token (WHERE _)) -> Just (Reduce 1 102)
    (228, Token (LBRACE _)) -> Just (Reduce 1 102)
    (228, Token (RBRACE _)) -> Just (Reduce 1 102)
    (228, Token (LPAREN _)) -> Just (Reduce 1 102)
    (228, Token (RPAREN _)) -> Just (Reduce 1 102)
    (228, Token (COMMA _)) -> Just (Reduce 1 102)
    (228, Token (DOT_DOT _)) -> Just (Reduce 1 102)
    (228, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (228, Token (EQUAL _)) -> Just (Reduce 1 102)
    (228, Token (DERIVING _)) -> Just (Reduce 1 102)
    (228, Token (DARROW _)) -> Just (Reduce 1 102)
    (228, Token (PIPE _)) -> Just (Reduce 1 102)
    (228, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (228, Token (MINUS _)) -> Just (Reduce 1 102)
    (228, Token (INFIXL _)) -> Just (Reduce 1 102)
    (228, Token (INFIXR _)) -> Just (Reduce 1 102)
    (228, Token (INFIX _)) -> Just (Reduce 1 102)
    (228, Token (RARROW _)) -> Just (Reduce 1 102)
    (228, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (228, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (228, Token (EXCL _)) -> Just (Reduce 1 102)
    (228, Token (QCONID _)) -> Just (Reduce 1 102)
    (228, Token (EXPORT _)) -> Just (Reduce 1 102)
    (228, Token (AS _)) -> Just (Reduce 1 102)
    (228, Token (QVARID _)) -> Just (Reduce 1 102)
    (228, Token (LARROW _)) -> Just (Reduce 1 102)
    (228, Token (THEN _)) -> Just (Reduce 1 102)
    (228, Token (ELSE _)) -> Just (Reduce 1 102)
    (228, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (228, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (228, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (228, Token (OF _)) -> Just (Reduce 1 102)
    (228, Token (INTEGER _)) -> Just (Reduce 1 102)
    (229, Token (WHERE _)) -> Just (Reduce 2 103)
    (229, Token (LBRACE _)) -> Just (Reduce 2 103)
    (229, Token (RBRACE _)) -> Just (Reduce 2 103)
    (229, Token (LPAREN _)) -> Just (Reduce 2 103)
    (229, Token (RPAREN _)) -> Just (Reduce 2 103)
    (229, Token (COMMA _)) -> Just (Reduce 2 103)
    (229, Token (DOT_DOT _)) -> Just (Reduce 2 103)
    (229, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (229, Token (EQUAL _)) -> Just (Reduce 2 103)
    (229, Token (DERIVING _)) -> Just (Reduce 2 103)
    (229, Token (DARROW _)) -> Just (Reduce 2 103)
    (229, Token (PIPE _)) -> Just (Reduce 2 103)
    (229, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (229, Token (MINUS _)) -> Just (Reduce 2 103)
    (229, Token (INFIXL _)) -> Just (Reduce 2 103)
    (229, Token (INFIXR _)) -> Just (Reduce 2 103)
    (229, Token (INFIX _)) -> Just (Reduce 2 103)
    (229, Token (RARROW _)) -> Just (Reduce 2 103)
    (229, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (229, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (229, Token (EXCL _)) -> Just (Reduce 2 103)
    (229, Token (QCONID _)) -> Just (Reduce 2 103)
    (229, Token (EXPORT _)) -> Just (Reduce 2 103)
    (229, Token (AS _)) -> Just (Reduce 2 103)
    (229, Token (QVARID _)) -> Just (Reduce 2 103)
    (229, Token (LARROW _)) -> Just (Reduce 2 103)
    (229, Token (THEN _)) -> Just (Reduce 2 103)
    (229, Token (ELSE _)) -> Just (Reduce 2 103)
    (229, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (229, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (229, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (229, Token (OF _)) -> Just (Reduce 2 103)
    (229, Token (INTEGER _)) -> Just (Reduce 2 103)
    (230, Token (WHERE _)) -> Just (Reduce 3 101)
    (230, Token (RBRACE _)) -> Just (Reduce 3 101)
    (230, Token (RPAREN _)) -> Just (Reduce 3 101)
    (230, Token (COMMA _)) -> Just (Reduce 3 101)
    (230, Token (DOT_DOT _)) -> Just (Reduce 3 101)
    (230, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (230, Token (EQUAL _)) -> Just (Reduce 3 101)
    (230, Token (PIPE _)) -> Just (Reduce 3 101)
    (230, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (230, Token (LARROW _)) -> Just (Reduce 3 101)
    (230, Token (THEN _)) -> Just (Reduce 3 101)
    (230, Token (ELSE _)) -> Just (Reduce 3 101)
    (230, Token (OF _)) -> Just (Reduce 3 101)
    (231, Token (RBRACE _)) -> Just (Reduce 2 120)
    (231, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (231, Token (DERIVING _)) -> Just (Reduce 2 120)
    (232, Token (QCONID _)) -> Just (Shift 233)
    (233, Token (RBRACE _)) -> Just (Reduce 1 134)
    (233, Token (RPAREN _)) -> Just (Reduce 1 134)
    (233, Token (COMMA _)) -> Just (Reduce 1 134)
    (233, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (234, Token (RPAREN _)) -> Just (Reduce 1 132)
    (234, Token (COMMA _)) -> Just (Shift 232)
    (235, Token (RPAREN _)) -> Just (Reduce 3 133)
    (236, Token (RBRACE _)) -> Just (Reduce 7 128)
    (236, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (236, Token (DERIVING _)) -> Just (Reduce 7 128)
    (237, Token (COLON_COLON _)) -> Just (Shift 148)
    (238, Token (RBRACE _)) -> Just (Shift 236)
    (239, Token (RBRACE _)) -> Just (Reduce 3 127)
    (239, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (239, Token (DERIVING _)) -> Just (Reduce 3 127)
    (240, Token (LBRACE _)) -> Just (Shift 83)
    (241, Token (RBRACE _)) -> Just (Reduce 2 66)
    (241, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (242, Token (LBRACE _)) -> Just (Shift 86)
    (243, Token (RBRACE _)) -> Just (Reduce 2 76)
    (243, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (244, Token (RPAREN _)) -> Just (Reduce 1 98)
    (244, Token (COMMA _)) -> Just (Shift 158)
    (245, Token (RPAREN _)) -> Just (Reduce 3 99)
    (246, Token (EXPORT _)) -> Just (Shift 360)
    (246, Token (AS _)) -> Just (Shift 361)
    (246, Token (QVARID _)) -> Just (Shift 362)
    (247, Token (EXPORT _)) -> Just (Shift 360)
    (247, Token (AS _)) -> Just (Shift 361)
    (247, Token (QVARID _)) -> Just (Shift 362)
    (248, Token (COLON_COLON _)) -> Just (Shift 143)
    (249, Token (COLON_COLON _)) -> Just (Shift 144)
    (250, Token (COLON_COLON _)) -> Just (Shift 145)
    (251, Token (RBRACE _)) -> Just (Reduce 6 135)
    (251, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (252, Token (RBRACE _)) -> Just (Reduce 7 136)
    (252, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (253, Token (RBRACE _)) -> Just (Reduce 6 137)
    (253, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (254, Token (EXPORT _)) -> Just (Shift 364)
    (254, Token (AS _)) -> Just (Shift 365)
    (254, Token (QVARID _)) -> Just (Shift 366)
    (254, Token (STRING _)) -> Just (Shift 363)
    (255, Token (STRING _)) -> Just (Shift 367)
    (256, Token (STRING _)) -> Just (Shift 363)
    (257, Token (LBRACE _)) -> Just (Shift 81)
    (258, Token (LBRACE _)) -> Just (Shift 81)
    (259, Token (RBRACE _)) -> Just (Reduce 5 62)
    (259, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (260, Token (RBRACE _)) -> Just (Reduce 5 64)
    (260, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (261, Token (RBRACE _)) -> Just (Reduce 1 60)
    (261, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (262, Token (WHERE _)) -> Just (Shift 257)
    (262, Token (RBRACE _)) -> Just (Reduce 3 61)
    (262, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (263, Token (WHERE _)) -> Just (Shift 258)
    (263, Token (RBRACE _)) -> Just (Reduce 3 63)
    (263, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (264, Token (LBRACE _)) -> Just (Shift 81)
    (265, Token (LBRACE _)) -> Just (Shift 81)
    (266, Token (LBRACE _)) -> Just (Shift 81)
    (267, Token (LBRACE _)) -> Just (Shift 81)
    (268, Token (LBRACE _)) -> Just (Shift 81)
    (269, Token (LBRACE _)) -> Just (Shift 81)
    (270, Token (LBRACE _)) -> Just (Shift 81)
    (271, Token (LBRACE _)) -> Just (Shift 81)
    (272, Token (RBRACE _)) -> Just (Reduce 3 57)
    (272, Token (COMMA _)) -> Just (Reduce 3 57)
    (272, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (272, Token (EQUAL _)) -> Just (Reduce 3 57)
    (272, Token (IN _)) -> Just (Reduce 3 57)
    (273, Token (RBRACE _)) -> Just (Shift 272)
    (274, Token (RBRACE _)) -> Just (Reduce 1 58)
    (274, Token (SEMICOLON _)) -> Just (Shift 82)
    (275, Token (RBRACE _)) -> Just (Reduce 3 59)
    (276, Token (RBRACE _)) -> Just (Reduce 5 87)
    (276, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (277, Token (RBRACE _)) -> Just (Reduce 3 86)
    (277, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (278, Token (COLON_COLON _)) -> Just (Shift 140)
    (279, Token (COMMA _)) -> Just (Reduce 0 241)
    (279, Token (MINUS _)) -> Just (Reduce 0 241)
    (279, Token (QCONID _)) -> Just (Reduce 0 241)
    (279, Token (EXPORT _)) -> Just (Reduce 0 241)
    (279, Token (AS _)) -> Just (Reduce 0 241)
    (279, Token (QVARID _)) -> Just (Reduce 0 241)
    (279, Token (QVARSYM _)) -> Just (Reduce 0 241)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 0 241)
    (279, Token (QCONSYM _)) -> Just (Reduce 0 241)
    (279, Token (INTEGER _)) -> Just (Shift 313)
    (280, Token (MINUS _)) -> Just (Shift 316)
    (280, Token (QVARSYM _)) -> Just (Shift 459)
    (280, Token (BACKQUOTE _)) -> Just (Shift 352)
    (280, Token (QCONSYM _)) -> Just (Shift 355)
    (281, Token (RBRACE _)) -> Just (Reduce 3 88)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (282, Token (LPAREN _)) -> Just (Reduce 1 211)
    (282, Token (RPAREN _)) -> Just (Reduce 1 211)
    (282, Token (EQUAL _)) -> Just (Reduce 1 211)
    (282, Token (PIPE _)) -> Just (Reduce 1 211)
    (282, Token (MINUS _)) -> Just (Reduce 1 211)
    (282, Token (RARROW _)) -> Just (Reduce 1 211)
    (282, Token (QCONID _)) -> Just (Reduce 1 211)
    (282, Token (EXPORT _)) -> Just (Reduce 1 211)
    (282, Token (AS _)) -> Just (Reduce 1 211)
    (282, Token (QVARID _)) -> Just (Reduce 1 211)
    (282, Token (QVARSYM _)) -> Just (Reduce 1 211)
    (282, Token (BACKQUOTE _)) -> Just (Reduce 1 211)
    (282, Token (QCONSYM _)) -> Just (Reduce 1 211)
    (283, Token (LPAREN _)) -> Just (Reduce 3 213)
    (283, Token (RPAREN _)) -> Just (Reduce 3 213)
    (283, Token (EQUAL _)) -> Just (Reduce 3 213)
    (283, Token (PIPE _)) -> Just (Reduce 3 213)
    (283, Token (MINUS _)) -> Just (Reduce 3 213)
    (283, Token (RARROW _)) -> Just (Reduce 3 213)
    (283, Token (QCONID _)) -> Just (Reduce 3 213)
    (283, Token (EXPORT _)) -> Just (Reduce 3 213)
    (283, Token (AS _)) -> Just (Reduce 3 213)
    (283, Token (QVARID _)) -> Just (Reduce 3 213)
    (283, Token (QVARSYM _)) -> Just (Reduce 3 213)
    (283, Token (BACKQUOTE _)) -> Just (Reduce 3 213)
    (283, Token (QCONSYM _)) -> Just (Reduce 3 213)
    (284, Token (LPAREN _)) -> Just (Reduce 2 212)
    (284, Token (RPAREN _)) -> Just (Reduce 2 212)
    (284, Token (EQUAL _)) -> Just (Reduce 2 212)
    (284, Token (PIPE _)) -> Just (Reduce 2 212)
    (284, Token (MINUS _)) -> Just (Reduce 2 212)
    (284, Token (RARROW _)) -> Just (Reduce 2 212)
    (284, Token (QCONID _)) -> Just (Reduce 2 212)
    (284, Token (EXPORT _)) -> Just (Reduce 2 212)
    (284, Token (AS _)) -> Just (Reduce 2 212)
    (284, Token (QVARID _)) -> Just (Reduce 2 212)
    (284, Token (QVARSYM _)) -> Just (Reduce 2 212)
    (284, Token (BACKQUOTE _)) -> Just (Reduce 2 212)
    (284, Token (QCONSYM _)) -> Just (Reduce 2 212)
    (285, Token (LPAREN _)) -> Just (Reduce 3 214)
    (285, Token (RPAREN _)) -> Just (Reduce 3 214)
    (285, Token (EQUAL _)) -> Just (Reduce 3 214)
    (285, Token (PIPE _)) -> Just (Reduce 3 214)
    (285, Token (MINUS _)) -> Just (Reduce 3 214)
    (285, Token (RARROW _)) -> Just (Reduce 3 214)
    (285, Token (QCONID _)) -> Just (Reduce 3 214)
    (285, Token (EXPORT _)) -> Just (Reduce 3 214)
    (285, Token (AS _)) -> Just (Reduce 3 214)
    (285, Token (QVARID _)) -> Just (Reduce 3 214)
    (285, Token (QVARSYM _)) -> Just (Reduce 3 214)
    (285, Token (BACKQUOTE _)) -> Just (Reduce 3 214)
    (285, Token (QCONSYM _)) -> Just (Reduce 3 214)
    (286, Token (WHERE _)) -> Just (Reduce 1 153)
    (286, Token (RBRACE _)) -> Just (Reduce 1 153)
    (286, Token (RPAREN _)) -> Just (Reduce 1 153)
    (286, Token (COMMA _)) -> Just (Reduce 1 153)
    (286, Token (DOT_DOT _)) -> Just (Reduce 1 153)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (286, Token (EQUAL _)) -> Just (Reduce 1 153)
    (286, Token (PIPE _)) -> Just (Reduce 1 153)
    (286, Token (RBRACKET _)) -> Just (Reduce 1 153)
    (286, Token (LARROW _)) -> Just (Reduce 1 153)
    (286, Token (THEN _)) -> Just (Reduce 1 153)
    (286, Token (ELSE _)) -> Just (Reduce 1 153)
    (286, Token (OF _)) -> Just (Reduce 1 153)
    (287, Token (WHERE _)) -> Just (Reduce 3 146)
    (287, Token (RBRACE _)) -> Just (Reduce 3 146)
    (287, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (287, Token (PIPE _)) -> Just (Shift 69)
    (288, Token (WHERE _)) -> Just (Reduce 5 147)
    (288, Token (RBRACE _)) -> Just (Reduce 5 147)
    (288, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (289, Token (EQUAL _)) -> Just (Shift 47)
    (290, Token (RBRACE _)) -> Just (Reduce 3 67)
    (290, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (291, Token (RBRACE _)) -> Just (Shift 290)
    (292, Token (RBRACE _)) -> Just (Reduce 3 69)
    (293, Token (RBRACE _)) -> Just (Reduce 1 68)
    (293, Token (SEMICOLON _)) -> Just (Shift 84)
    (294, Token (RBRACE _)) -> Just (Reduce 5 72)
    (294, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (295, Token (RBRACE _)) -> Just (Reduce 5 74)
    (295, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (296, Token (RBRACE _)) -> Just (Reduce 1 70)
    (296, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (297, Token (WHERE _)) -> Just (Shift 265)
    (297, Token (RBRACE _)) -> Just (Reduce 3 71)
    (297, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (298, Token (WHERE _)) -> Just (Shift 266)
    (298, Token (RBRACE _)) -> Just (Reduce 3 73)
    (298, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (299, Token (RBRACE _)) -> Just (Reduce 3 77)
    (299, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (300, Token (RBRACE _)) -> Just (Shift 299)
    (301, Token (RBRACE _)) -> Just (Reduce 3 79)
    (302, Token (RBRACE _)) -> Just (Reduce 1 78)
    (302, Token (SEMICOLON _)) -> Just (Shift 87)
    (303, Token (RBRACE _)) -> Just (Reduce 5 82)
    (303, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (304, Token (RBRACE _)) -> Just (Reduce 5 84)
    (304, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (305, Token (WHERE _)) -> Just (Shift 267)
    (305, Token (RBRACE _)) -> Just (Reduce 3 81)
    (305, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (306, Token (WHERE _)) -> Just (Shift 268)
    (306, Token (RBRACE _)) -> Just (Reduce 3 83)
    (306, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (307, Token (COMMA _)) -> Just (Shift 100)
    (307, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (308, Token (LPAREN _)) -> Just (Reduce 1 215)
    (308, Token (COMMA _)) -> Just (Shift 100)
    (308, Token (EQUAL _)) -> Just (Reduce 1 215)
    (308, Token (PIPE _)) -> Just (Reduce 1 215)
    (308, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (308, Token (MINUS _)) -> Just (Reduce 1 215)
    (308, Token (QCONID _)) -> Just (Reduce 1 215)
    (308, Token (EXPORT _)) -> Just (Reduce 1 215)
    (308, Token (AS _)) -> Just (Reduce 1 215)
    (308, Token (QVARID _)) -> Just (Reduce 1 215)
    (308, Token (QVARSYM _)) -> Just (Reduce 1 215)
    (308, Token (BACKQUOTE _)) -> Just (Reduce 1 215)
    (308, Token (QCONSYM _)) -> Just (Reduce 1 215)
    (309, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (310, Token (COMMA _)) -> Just (Reduce 1 95)
    (310, Token (MINUS _)) -> Just (Reduce 1 95)
    (310, Token (QCONID _)) -> Just (Reduce 1 95)
    (310, Token (EXPORT _)) -> Just (Reduce 1 95)
    (310, Token (AS _)) -> Just (Reduce 1 95)
    (310, Token (QVARID _)) -> Just (Reduce 1 95)
    (310, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (310, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (310, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (310, Token (INTEGER _)) -> Just (Reduce 1 95)
    (311, Token (COMMA _)) -> Just (Reduce 1 96)
    (311, Token (MINUS _)) -> Just (Reduce 1 96)
    (311, Token (QCONID _)) -> Just (Reduce 1 96)
    (311, Token (EXPORT _)) -> Just (Reduce 1 96)
    (311, Token (AS _)) -> Just (Reduce 1 96)
    (311, Token (QVARID _)) -> Just (Reduce 1 96)
    (311, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (311, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (311, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (311, Token (INTEGER _)) -> Just (Reduce 1 96)
    (312, Token (COMMA _)) -> Just (Reduce 1 97)
    (312, Token (MINUS _)) -> Just (Reduce 1 97)
    (312, Token (QCONID _)) -> Just (Reduce 1 97)
    (312, Token (EXPORT _)) -> Just (Reduce 1 97)
    (312, Token (AS _)) -> Just (Reduce 1 97)
    (312, Token (QVARID _)) -> Just (Reduce 1 97)
    (312, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (312, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (312, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (312, Token (INTEGER _)) -> Just (Reduce 1 97)
    (313, Token (COMMA _)) -> Just (Reduce 1 242)
    (313, Token (MINUS _)) -> Just (Reduce 1 242)
    (313, Token (QCONID _)) -> Just (Reduce 1 242)
    (313, Token (EXPORT _)) -> Just (Reduce 1 242)
    (313, Token (AS _)) -> Just (Reduce 1 242)
    (313, Token (QVARID _)) -> Just (Reduce 1 242)
    (313, Token (QVARSYM _)) -> Just (Reduce 1 242)
    (313, Token (BACKQUOTE _)) -> Just (Reduce 1 242)
    (313, Token (QCONSYM _)) -> Just (Reduce 1 242)
    (314, Token (MINUS _)) -> Just (Shift 316)
    (314, Token (QVARSYM _)) -> Just (Shift 459)
    (314, Token (BACKQUOTE _)) -> Just (Shift 352)
    (314, Token (QCONSYM _)) -> Just (Shift 355)
    (315, Token (MINUS _)) -> Just (Shift 316)
    (315, Token (QVARSYM _)) -> Just (Shift 459)
    (315, Token (BACKQUOTE _)) -> Just (Shift 352)
    (315, Token (QCONSYM _)) -> Just (Shift 355)
    (316, Token (RBRACE _)) -> Just (Reduce 1 89)
    (316, Token (COMMA _)) -> Just (Shift 314)
    (316, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (317, Token (RBRACE _)) -> Just (Reduce 3 91)
    (317, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (318, Token (RBRACE _)) -> Just (Reduce 3 92)
    (318, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (319, Token (RBRACE _)) -> Just (Reduce 1 90)
    (319, Token (COMMA _)) -> Just (Shift 315)
    (319, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (320, Token (RBRACE _)) -> Just (Reduce 1 231)
    (320, Token (LPAREN _)) -> Just (Reduce 1 231)
    (320, Token (COMMA _)) -> Just (Reduce 1 231)
    (320, Token (SEMICOLON _)) -> Just (Reduce 1 231)
    (320, Token (MINUS _)) -> Just (Reduce 1 231)
    (320, Token (QCONID _)) -> Just (Reduce 1 231)
    (320, Token (EXPORT _)) -> Just (Reduce 1 231)
    (320, Token (AS _)) -> Just (Reduce 1 231)
    (320, Token (QVARID _)) -> Just (Reduce 1 231)
    (320, Token (QVARSYM _)) -> Just (Reduce 1 231)
    (320, Token (BACKQUOTE _)) -> Just (Reduce 1 231)
    (320, Token (QCONSYM _)) -> Just (Reduce 1 231)
    (321, Token (RBRACE _)) -> Just (Reduce 1 230)
    (321, Token (LPAREN _)) -> Just (Reduce 1 230)
    (321, Token (COMMA _)) -> Just (Reduce 1 230)
    (321, Token (SEMICOLON _)) -> Just (Reduce 1 230)
    (321, Token (MINUS _)) -> Just (Reduce 1 230)
    (321, Token (QCONID _)) -> Just (Reduce 1 230)
    (321, Token (EXPORT _)) -> Just (Reduce 1 230)
    (321, Token (AS _)) -> Just (Reduce 1 230)
    (321, Token (QVARID _)) -> Just (Reduce 1 230)
    (321, Token (QVARSYM _)) -> Just (Reduce 1 230)
    (321, Token (BACKQUOTE _)) -> Just (Reduce 1 230)
    (321, Token (QCONSYM _)) -> Just (Reduce 1 230)
    (322, Token (WHERE _)) -> Just (Reduce 3 108)
    (322, Token (LBRACE _)) -> Just (Reduce 3 108)
    (322, Token (RBRACE _)) -> Just (Reduce 3 108)
    (322, Token (LPAREN _)) -> Just (Reduce 3 108)
    (322, Token (RPAREN _)) -> Just (Reduce 3 108)
    (322, Token (COMMA _)) -> Just (Reduce 3 108)
    (322, Token (DOT_DOT _)) -> Just (Reduce 3 108)
    (322, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (322, Token (EQUAL _)) -> Just (Reduce 3 108)
    (322, Token (DERIVING _)) -> Just (Reduce 3 108)
    (322, Token (DARROW _)) -> Just (Reduce 3 108)
    (322, Token (PIPE _)) -> Just (Reduce 3 108)
    (322, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (322, Token (MINUS _)) -> Just (Reduce 3 108)
    (322, Token (INFIXL _)) -> Just (Reduce 3 108)
    (322, Token (INFIXR _)) -> Just (Reduce 3 108)
    (322, Token (INFIX _)) -> Just (Reduce 3 108)
    (322, Token (RARROW _)) -> Just (Reduce 3 108)
    (322, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (322, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (322, Token (EXCL _)) -> Just (Reduce 3 108)
    (322, Token (QCONID _)) -> Just (Reduce 3 108)
    (322, Token (EXPORT _)) -> Just (Reduce 3 108)
    (322, Token (AS _)) -> Just (Reduce 3 108)
    (322, Token (QVARID _)) -> Just (Reduce 3 108)
    (322, Token (LARROW _)) -> Just (Reduce 3 108)
    (322, Token (THEN _)) -> Just (Reduce 3 108)
    (322, Token (ELSE _)) -> Just (Reduce 3 108)
    (322, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (322, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (322, Token (OF _)) -> Just (Reduce 3 108)
    (322, Token (INTEGER _)) -> Just (Reduce 3 108)
    (323, Token (WHERE _)) -> Just (Reduce 3 106)
    (323, Token (LBRACE _)) -> Just (Reduce 3 106)
    (323, Token (RBRACE _)) -> Just (Reduce 3 106)
    (323, Token (LPAREN _)) -> Just (Reduce 3 106)
    (323, Token (RPAREN _)) -> Just (Reduce 3 106)
    (323, Token (COMMA _)) -> Just (Reduce 3 106)
    (323, Token (DOT_DOT _)) -> Just (Reduce 3 106)
    (323, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (323, Token (EQUAL _)) -> Just (Reduce 3 106)
    (323, Token (DERIVING _)) -> Just (Reduce 3 106)
    (323, Token (DARROW _)) -> Just (Reduce 3 106)
    (323, Token (PIPE _)) -> Just (Reduce 3 106)
    (323, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (323, Token (MINUS _)) -> Just (Reduce 3 106)
    (323, Token (INFIXL _)) -> Just (Reduce 3 106)
    (323, Token (INFIXR _)) -> Just (Reduce 3 106)
    (323, Token (INFIX _)) -> Just (Reduce 3 106)
    (323, Token (RARROW _)) -> Just (Reduce 3 106)
    (323, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (323, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (323, Token (EXCL _)) -> Just (Reduce 3 106)
    (323, Token (QCONID _)) -> Just (Reduce 3 106)
    (323, Token (EXPORT _)) -> Just (Reduce 3 106)
    (323, Token (AS _)) -> Just (Reduce 3 106)
    (323, Token (QVARID _)) -> Just (Reduce 3 106)
    (323, Token (LARROW _)) -> Just (Reduce 3 106)
    (323, Token (THEN _)) -> Just (Reduce 3 106)
    (323, Token (ELSE _)) -> Just (Reduce 3 106)
    (323, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (323, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (323, Token (OF _)) -> Just (Reduce 3 106)
    (323, Token (INTEGER _)) -> Just (Reduce 3 106)
    (324, Token (WHERE _)) -> Just (Reduce 3 107)
    (324, Token (LBRACE _)) -> Just (Reduce 3 107)
    (324, Token (RBRACE _)) -> Just (Reduce 3 107)
    (324, Token (LPAREN _)) -> Just (Reduce 3 107)
    (324, Token (RPAREN _)) -> Just (Reduce 3 107)
    (324, Token (COMMA _)) -> Just (Reduce 3 107)
    (324, Token (DOT_DOT _)) -> Just (Reduce 3 107)
    (324, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (324, Token (EQUAL _)) -> Just (Reduce 3 107)
    (324, Token (DERIVING _)) -> Just (Reduce 3 107)
    (324, Token (DARROW _)) -> Just (Reduce 3 107)
    (324, Token (PIPE _)) -> Just (Reduce 3 107)
    (324, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (324, Token (MINUS _)) -> Just (Reduce 3 107)
    (324, Token (INFIXL _)) -> Just (Reduce 3 107)
    (324, Token (INFIXR _)) -> Just (Reduce 3 107)
    (324, Token (INFIX _)) -> Just (Reduce 3 107)
    (324, Token (RARROW _)) -> Just (Reduce 3 107)
    (324, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (324, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (324, Token (EXCL _)) -> Just (Reduce 3 107)
    (324, Token (QCONID _)) -> Just (Reduce 3 107)
    (324, Token (EXPORT _)) -> Just (Reduce 3 107)
    (324, Token (AS _)) -> Just (Reduce 3 107)
    (324, Token (QVARID _)) -> Just (Reduce 3 107)
    (324, Token (LARROW _)) -> Just (Reduce 3 107)
    (324, Token (THEN _)) -> Just (Reduce 3 107)
    (324, Token (ELSE _)) -> Just (Reduce 3 107)
    (324, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (324, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (324, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (324, Token (OF _)) -> Just (Reduce 3 107)
    (324, Token (INTEGER _)) -> Just (Reduce 3 107)
    (325, Token (RPAREN _)) -> Just (Shift 322)
    (325, Token (COMMA _)) -> Just (Shift 159)
    (326, Token (RBRACKET _)) -> Just (Shift 324)
    (327, Token (WHERE _)) -> Just (Reduce 2 109)
    (327, Token (LBRACE _)) -> Just (Reduce 2 109)
    (327, Token (RBRACE _)) -> Just (Reduce 2 109)
    (327, Token (LPAREN _)) -> Just (Reduce 2 109)
    (327, Token (RPAREN _)) -> Just (Reduce 2 109)
    (327, Token (COMMA _)) -> Just (Reduce 2 109)
    (327, Token (DOT_DOT _)) -> Just (Reduce 2 109)
    (327, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (327, Token (EQUAL _)) -> Just (Reduce 2 109)
    (327, Token (DERIVING _)) -> Just (Reduce 2 109)
    (327, Token (DARROW _)) -> Just (Reduce 2 109)
    (327, Token (PIPE _)) -> Just (Reduce 2 109)
    (327, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (327, Token (MINUS _)) -> Just (Reduce 2 109)
    (327, Token (INFIXL _)) -> Just (Reduce 2 109)
    (327, Token (INFIXR _)) -> Just (Reduce 2 109)
    (327, Token (INFIX _)) -> Just (Reduce 2 109)
    (327, Token (RARROW _)) -> Just (Reduce 2 109)
    (327, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (327, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (327, Token (EXCL _)) -> Just (Reduce 2 109)
    (327, Token (QCONID _)) -> Just (Reduce 2 109)
    (327, Token (EXPORT _)) -> Just (Reduce 2 109)
    (327, Token (AS _)) -> Just (Reduce 2 109)
    (327, Token (QVARID _)) -> Just (Reduce 2 109)
    (327, Token (LARROW _)) -> Just (Reduce 2 109)
    (327, Token (THEN _)) -> Just (Reduce 2 109)
    (327, Token (ELSE _)) -> Just (Reduce 2 109)
    (327, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (327, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (327, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (327, Token (OF _)) -> Just (Reduce 2 109)
    (327, Token (INTEGER _)) -> Just (Reduce 2 109)
    (328, Token (WHERE _)) -> Just (Reduce 1 104)
    (328, Token (LBRACE _)) -> Just (Reduce 1 104)
    (328, Token (RBRACE _)) -> Just (Reduce 1 104)
    (328, Token (LPAREN _)) -> Just (Reduce 1 104)
    (328, Token (RPAREN _)) -> Just (Reduce 1 104)
    (328, Token (COMMA _)) -> Just (Reduce 1 104)
    (328, Token (DOT_DOT _)) -> Just (Reduce 1 104)
    (328, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (328, Token (EQUAL _)) -> Just (Reduce 1 104)
    (328, Token (DERIVING _)) -> Just (Reduce 1 104)
    (328, Token (DARROW _)) -> Just (Reduce 1 104)
    (328, Token (PIPE _)) -> Just (Reduce 1 104)
    (328, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (328, Token (MINUS _)) -> Just (Reduce 1 104)
    (328, Token (INFIXL _)) -> Just (Reduce 1 104)
    (328, Token (INFIXR _)) -> Just (Reduce 1 104)
    (328, Token (INFIX _)) -> Just (Reduce 1 104)
    (328, Token (RARROW _)) -> Just (Reduce 1 104)
    (328, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (328, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (328, Token (EXCL _)) -> Just (Reduce 1 104)
    (328, Token (QCONID _)) -> Just (Reduce 1 104)
    (328, Token (EXPORT _)) -> Just (Reduce 1 104)
    (328, Token (AS _)) -> Just (Reduce 1 104)
    (328, Token (QVARID _)) -> Just (Reduce 1 104)
    (328, Token (LARROW _)) -> Just (Reduce 1 104)
    (328, Token (THEN _)) -> Just (Reduce 1 104)
    (328, Token (ELSE _)) -> Just (Reduce 1 104)
    (328, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (328, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (328, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (328, Token (OF _)) -> Just (Reduce 1 104)
    (328, Token (INTEGER _)) -> Just (Reduce 1 104)
    (329, Token (WHERE _)) -> Just (Reduce 1 105)
    (329, Token (LBRACE _)) -> Just (Reduce 1 105)
    (329, Token (RBRACE _)) -> Just (Reduce 1 105)
    (329, Token (LPAREN _)) -> Just (Reduce 1 105)
    (329, Token (RPAREN _)) -> Just (Reduce 1 105)
    (329, Token (COMMA _)) -> Just (Reduce 1 105)
    (329, Token (DOT_DOT _)) -> Just (Reduce 1 105)
    (329, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (329, Token (EQUAL _)) -> Just (Reduce 1 105)
    (329, Token (DERIVING _)) -> Just (Reduce 1 105)
    (329, Token (DARROW _)) -> Just (Reduce 1 105)
    (329, Token (PIPE _)) -> Just (Reduce 1 105)
    (329, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (329, Token (MINUS _)) -> Just (Reduce 1 105)
    (329, Token (INFIXL _)) -> Just (Reduce 1 105)
    (329, Token (INFIXR _)) -> Just (Reduce 1 105)
    (329, Token (INFIX _)) -> Just (Reduce 1 105)
    (329, Token (RARROW _)) -> Just (Reduce 1 105)
    (329, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (329, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (329, Token (EXCL _)) -> Just (Reduce 1 105)
    (329, Token (QCONID _)) -> Just (Reduce 1 105)
    (329, Token (EXPORT _)) -> Just (Reduce 1 105)
    (329, Token (AS _)) -> Just (Reduce 1 105)
    (329, Token (QVARID _)) -> Just (Reduce 1 105)
    (329, Token (LARROW _)) -> Just (Reduce 1 105)
    (329, Token (THEN _)) -> Just (Reduce 1 105)
    (329, Token (ELSE _)) -> Just (Reduce 1 105)
    (329, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (329, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (329, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (329, Token (OF _)) -> Just (Reduce 1 105)
    (329, Token (INTEGER _)) -> Just (Reduce 1 105)
    (330, Token (RPAREN _)) -> Just (Shift 323)
    (331, Token (WHERE _)) -> Just (Reduce 2 113)
    (331, Token (LBRACE _)) -> Just (Reduce 2 113)
    (331, Token (RBRACE _)) -> Just (Reduce 2 113)
    (331, Token (LPAREN _)) -> Just (Reduce 2 113)
    (331, Token (RPAREN _)) -> Just (Reduce 2 113)
    (331, Token (COMMA _)) -> Just (Reduce 2 113)
    (331, Token (DOT_DOT _)) -> Just (Reduce 2 113)
    (331, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (331, Token (EQUAL _)) -> Just (Reduce 2 113)
    (331, Token (DERIVING _)) -> Just (Reduce 2 113)
    (331, Token (DARROW _)) -> Just (Reduce 2 113)
    (331, Token (PIPE _)) -> Just (Reduce 2 113)
    (331, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (331, Token (MINUS _)) -> Just (Reduce 2 113)
    (331, Token (INFIXL _)) -> Just (Reduce 2 113)
    (331, Token (INFIXR _)) -> Just (Reduce 2 113)
    (331, Token (INFIX _)) -> Just (Reduce 2 113)
    (331, Token (RARROW _)) -> Just (Reduce 2 113)
    (331, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (331, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (331, Token (EXCL _)) -> Just (Reduce 2 113)
    (331, Token (QCONID _)) -> Just (Reduce 2 113)
    (331, Token (EXPORT _)) -> Just (Reduce 2 113)
    (331, Token (AS _)) -> Just (Reduce 2 113)
    (331, Token (QVARID _)) -> Just (Reduce 2 113)
    (331, Token (LARROW _)) -> Just (Reduce 2 113)
    (331, Token (THEN _)) -> Just (Reduce 2 113)
    (331, Token (ELSE _)) -> Just (Reduce 2 113)
    (331, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (331, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (331, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (331, Token (OF _)) -> Just (Reduce 2 113)
    (331, Token (INTEGER _)) -> Just (Reduce 2 113)
    (332, Token (WHERE _)) -> Just (Reduce 3 115)
    (332, Token (LBRACE _)) -> Just (Reduce 3 115)
    (332, Token (RBRACE _)) -> Just (Reduce 3 115)
    (332, Token (LPAREN _)) -> Just (Reduce 3 115)
    (332, Token (RPAREN _)) -> Just (Reduce 3 115)
    (332, Token (COMMA _)) -> Just (Reduce 3 115)
    (332, Token (DOT_DOT _)) -> Just (Reduce 3 115)
    (332, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (332, Token (EQUAL _)) -> Just (Reduce 3 115)
    (332, Token (DERIVING _)) -> Just (Reduce 3 115)
    (332, Token (DARROW _)) -> Just (Reduce 3 115)
    (332, Token (PIPE _)) -> Just (Reduce 3 115)
    (332, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (332, Token (MINUS _)) -> Just (Reduce 3 115)
    (332, Token (INFIXL _)) -> Just (Reduce 3 115)
    (332, Token (INFIXR _)) -> Just (Reduce 3 115)
    (332, Token (INFIX _)) -> Just (Reduce 3 115)
    (332, Token (RARROW _)) -> Just (Reduce 3 115)
    (332, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (332, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (332, Token (EXCL _)) -> Just (Reduce 3 115)
    (332, Token (QCONID _)) -> Just (Reduce 3 115)
    (332, Token (EXPORT _)) -> Just (Reduce 3 115)
    (332, Token (AS _)) -> Just (Reduce 3 115)
    (332, Token (QVARID _)) -> Just (Reduce 3 115)
    (332, Token (LARROW _)) -> Just (Reduce 3 115)
    (332, Token (THEN _)) -> Just (Reduce 3 115)
    (332, Token (ELSE _)) -> Just (Reduce 3 115)
    (332, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (332, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (332, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (332, Token (OF _)) -> Just (Reduce 3 115)
    (332, Token (INTEGER _)) -> Just (Reduce 3 115)
    (333, Token (WHERE _)) -> Just (Reduce 3 116)
    (333, Token (LBRACE _)) -> Just (Reduce 3 116)
    (333, Token (RBRACE _)) -> Just (Reduce 3 116)
    (333, Token (LPAREN _)) -> Just (Reduce 3 116)
    (333, Token (RPAREN _)) -> Just (Reduce 3 116)
    (333, Token (COMMA _)) -> Just (Reduce 3 116)
    (333, Token (DOT_DOT _)) -> Just (Reduce 3 116)
    (333, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (333, Token (EQUAL _)) -> Just (Reduce 3 116)
    (333, Token (DERIVING _)) -> Just (Reduce 3 116)
    (333, Token (DARROW _)) -> Just (Reduce 3 116)
    (333, Token (PIPE _)) -> Just (Reduce 3 116)
    (333, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (333, Token (MINUS _)) -> Just (Reduce 3 116)
    (333, Token (INFIXL _)) -> Just (Reduce 3 116)
    (333, Token (INFIXR _)) -> Just (Reduce 3 116)
    (333, Token (INFIX _)) -> Just (Reduce 3 116)
    (333, Token (RARROW _)) -> Just (Reduce 3 116)
    (333, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (333, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (333, Token (EXCL _)) -> Just (Reduce 3 116)
    (333, Token (QCONID _)) -> Just (Reduce 3 116)
    (333, Token (EXPORT _)) -> Just (Reduce 3 116)
    (333, Token (AS _)) -> Just (Reduce 3 116)
    (333, Token (QVARID _)) -> Just (Reduce 3 116)
    (333, Token (LARROW _)) -> Just (Reduce 3 116)
    (333, Token (THEN _)) -> Just (Reduce 3 116)
    (333, Token (ELSE _)) -> Just (Reduce 3 116)
    (333, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (333, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (333, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (333, Token (OF _)) -> Just (Reduce 3 116)
    (333, Token (INTEGER _)) -> Just (Reduce 3 116)
    (334, Token (RPAREN _)) -> Just (Shift 332)
    (335, Token (WHERE _)) -> Just (Reduce 2 114)
    (335, Token (LBRACE _)) -> Just (Reduce 2 114)
    (335, Token (RBRACE _)) -> Just (Reduce 2 114)
    (335, Token (LPAREN _)) -> Just (Reduce 2 114)
    (335, Token (RPAREN _)) -> Just (Reduce 2 114)
    (335, Token (COMMA _)) -> Just (Reduce 2 114)
    (335, Token (DOT_DOT _)) -> Just (Reduce 2 114)
    (335, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (335, Token (EQUAL _)) -> Just (Reduce 2 114)
    (335, Token (DERIVING _)) -> Just (Reduce 2 114)
    (335, Token (DARROW _)) -> Just (Reduce 2 114)
    (335, Token (PIPE _)) -> Just (Reduce 2 114)
    (335, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (335, Token (MINUS _)) -> Just (Reduce 2 114)
    (335, Token (INFIXL _)) -> Just (Reduce 2 114)
    (335, Token (INFIXR _)) -> Just (Reduce 2 114)
    (335, Token (INFIX _)) -> Just (Reduce 2 114)
    (335, Token (RARROW _)) -> Just (Reduce 2 114)
    (335, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (335, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (335, Token (EXCL _)) -> Just (Reduce 2 114)
    (335, Token (QCONID _)) -> Just (Reduce 2 114)
    (335, Token (EXPORT _)) -> Just (Reduce 2 114)
    (335, Token (AS _)) -> Just (Reduce 2 114)
    (335, Token (QVARID _)) -> Just (Reduce 2 114)
    (335, Token (LARROW _)) -> Just (Reduce 2 114)
    (335, Token (THEN _)) -> Just (Reduce 2 114)
    (335, Token (ELSE _)) -> Just (Reduce 2 114)
    (335, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (335, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (335, Token (OF _)) -> Just (Reduce 2 114)
    (335, Token (INTEGER _)) -> Just (Reduce 2 114)
    (336, Token (WHERE _)) -> Just (Reduce 1 112)
    (336, Token (LBRACE _)) -> Just (Reduce 1 112)
    (336, Token (RBRACE _)) -> Just (Reduce 1 112)
    (336, Token (LPAREN _)) -> Just (Reduce 1 112)
    (336, Token (RPAREN _)) -> Just (Reduce 1 112)
    (336, Token (COMMA _)) -> Just (Reduce 1 112)
    (336, Token (DOT_DOT _)) -> Just (Reduce 1 112)
    (336, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (336, Token (EQUAL _)) -> Just (Reduce 1 112)
    (336, Token (DERIVING _)) -> Just (Reduce 1 112)
    (336, Token (DARROW _)) -> Just (Reduce 1 112)
    (336, Token (PIPE _)) -> Just (Reduce 1 112)
    (336, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (336, Token (MINUS _)) -> Just (Reduce 1 112)
    (336, Token (INFIXL _)) -> Just (Reduce 1 112)
    (336, Token (INFIXR _)) -> Just (Reduce 1 112)
    (336, Token (INFIX _)) -> Just (Reduce 1 112)
    (336, Token (RARROW _)) -> Just (Reduce 1 112)
    (336, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (336, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (336, Token (EXCL _)) -> Just (Reduce 1 112)
    (336, Token (QCONID _)) -> Just (Reduce 1 112)
    (336, Token (EXPORT _)) -> Just (Reduce 1 112)
    (336, Token (AS _)) -> Just (Reduce 1 112)
    (336, Token (QVARID _)) -> Just (Reduce 1 112)
    (336, Token (LARROW _)) -> Just (Reduce 1 112)
    (336, Token (THEN _)) -> Just (Reduce 1 112)
    (336, Token (ELSE _)) -> Just (Reduce 1 112)
    (336, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (336, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (336, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (336, Token (OF _)) -> Just (Reduce 1 112)
    (336, Token (INTEGER _)) -> Just (Reduce 1 112)
    (337, Token (LBRACE _)) -> Just (Shift 101)
    (337, Token (RBRACE _)) -> Just (Reduce 1 112)
    (337, Token (LPAREN _)) -> Just (Reduce 1 112)
    (337, Token (RPAREN _)) -> Just (Reduce 1 112)
    (337, Token (COMMA _)) -> Just (Reduce 1 112)
    (337, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (337, Token (DERIVING _)) -> Just (Reduce 1 112)
    (337, Token (PIPE _)) -> Just (Reduce 1 112)
    (337, Token (RARROW _)) -> Just (Reduce 1 112)
    (337, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (337, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (337, Token (EXCL _)) -> Just (Reduce 1 112)
    (337, Token (QCONID _)) -> Just (Reduce 1 112)
    (337, Token (EXPORT _)) -> Just (Reduce 1 112)
    (337, Token (AS _)) -> Just (Reduce 1 112)
    (337, Token (QVARID _)) -> Just (Reduce 1 112)
    (337, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (337, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (338, Token (RPAREN _)) -> Just (Shift 333)
    (339, Token (WHERE _)) -> Just (Reduce 1 237)
    (339, Token (LBRACE _)) -> Just (Reduce 1 237)
    (339, Token (RBRACE _)) -> Just (Reduce 1 237)
    (339, Token (LPAREN _)) -> Just (Reduce 1 237)
    (339, Token (RPAREN _)) -> Just (Reduce 1 237)
    (339, Token (COMMA _)) -> Just (Reduce 1 237)
    (339, Token (DOT_DOT _)) -> Just (Reduce 1 237)
    (339, Token (SEMICOLON _)) -> Just (Reduce 1 237)
    (339, Token (EQUAL _)) -> Just (Reduce 1 237)
    (339, Token (DERIVING _)) -> Just (Reduce 1 237)
    (339, Token (DARROW _)) -> Just (Reduce 1 237)
    (339, Token (PIPE _)) -> Just (Reduce 1 237)
    (339, Token (COLON_COLON _)) -> Just (Reduce 1 237)
    (339, Token (MINUS _)) -> Just (Reduce 1 237)
    (339, Token (INFIXL _)) -> Just (Reduce 1 237)
    (339, Token (INFIXR _)) -> Just (Reduce 1 237)
    (339, Token (INFIX _)) -> Just (Reduce 1 237)
    (339, Token (RARROW _)) -> Just (Reduce 1 237)
    (339, Token (LBRACKET _)) -> Just (Reduce 1 237)
    (339, Token (RBRACKET _)) -> Just (Reduce 1 237)
    (339, Token (EXCL _)) -> Just (Reduce 1 237)
    (339, Token (QCONID _)) -> Just (Reduce 1 237)
    (339, Token (EXPORT _)) -> Just (Reduce 1 237)
    (339, Token (AS _)) -> Just (Reduce 1 237)
    (339, Token (QVARID _)) -> Just (Reduce 1 237)
    (339, Token (LARROW _)) -> Just (Reduce 1 237)
    (339, Token (THEN _)) -> Just (Reduce 1 237)
    (339, Token (ELSE _)) -> Just (Reduce 1 237)
    (339, Token (QVARSYM _)) -> Just (Reduce 1 237)
    (339, Token (BACKQUOTE _)) -> Just (Reduce 1 237)
    (339, Token (QCONSYM _)) -> Just (Reduce 1 237)
    (339, Token (OF _)) -> Just (Reduce 1 237)
    (339, Token (INTEGER _)) -> Just (Reduce 1 237)
    (340, Token (WHERE _)) -> Just (Reduce 1 236)
    (340, Token (LBRACE _)) -> Just (Reduce 1 236)
    (340, Token (RBRACE _)) -> Just (Reduce 1 236)
    (340, Token (LPAREN _)) -> Just (Reduce 1 236)
    (340, Token (RPAREN _)) -> Just (Reduce 1 236)
    (340, Token (COMMA _)) -> Just (Reduce 1 236)
    (340, Token (DOT_DOT _)) -> Just (Reduce 1 236)
    (340, Token (SEMICOLON _)) -> Just (Reduce 1 236)
    (340, Token (EQUAL _)) -> Just (Reduce 1 236)
    (340, Token (DERIVING _)) -> Just (Reduce 1 236)
    (340, Token (DARROW _)) -> Just (Reduce 1 236)
    (340, Token (PIPE _)) -> Just (Reduce 1 236)
    (340, Token (COLON_COLON _)) -> Just (Reduce 1 236)
    (340, Token (MINUS _)) -> Just (Reduce 1 236)
    (340, Token (INFIXL _)) -> Just (Reduce 1 236)
    (340, Token (INFIXR _)) -> Just (Reduce 1 236)
    (340, Token (INFIX _)) -> Just (Reduce 1 236)
    (340, Token (RARROW _)) -> Just (Reduce 1 236)
    (340, Token (LBRACKET _)) -> Just (Reduce 1 236)
    (340, Token (RBRACKET _)) -> Just (Reduce 1 236)
    (340, Token (EXCL _)) -> Just (Reduce 1 236)
    (340, Token (QCONID _)) -> Just (Reduce 1 236)
    (340, Token (EXPORT _)) -> Just (Reduce 1 236)
    (340, Token (AS _)) -> Just (Reduce 1 236)
    (340, Token (QVARID _)) -> Just (Reduce 1 236)
    (340, Token (LARROW _)) -> Just (Reduce 1 236)
    (340, Token (THEN _)) -> Just (Reduce 1 236)
    (340, Token (ELSE _)) -> Just (Reduce 1 236)
    (340, Token (QVARSYM _)) -> Just (Reduce 1 236)
    (340, Token (BACKQUOTE _)) -> Just (Reduce 1 236)
    (340, Token (QCONSYM _)) -> Just (Reduce 1 236)
    (340, Token (OF _)) -> Just (Reduce 1 236)
    (340, Token (INTEGER _)) -> Just (Reduce 1 236)
    (341, Token (WHERE _)) -> Just (Reduce 1 238)
    (341, Token (LBRACE _)) -> Just (Reduce 1 238)
    (341, Token (RBRACE _)) -> Just (Reduce 1 238)
    (341, Token (LPAREN _)) -> Just (Reduce 1 238)
    (341, Token (RPAREN _)) -> Just (Reduce 1 238)
    (341, Token (COMMA _)) -> Just (Reduce 1 238)
    (341, Token (DOT_DOT _)) -> Just (Reduce 1 238)
    (341, Token (SEMICOLON _)) -> Just (Reduce 1 238)
    (341, Token (EQUAL _)) -> Just (Reduce 1 238)
    (341, Token (DERIVING _)) -> Just (Reduce 1 238)
    (341, Token (DARROW _)) -> Just (Reduce 1 238)
    (341, Token (PIPE _)) -> Just (Reduce 1 238)
    (341, Token (COLON_COLON _)) -> Just (Reduce 1 238)
    (341, Token (MINUS _)) -> Just (Reduce 1 238)
    (341, Token (INFIXL _)) -> Just (Reduce 1 238)
    (341, Token (INFIXR _)) -> Just (Reduce 1 238)
    (341, Token (INFIX _)) -> Just (Reduce 1 238)
    (341, Token (RARROW _)) -> Just (Reduce 1 238)
    (341, Token (LBRACKET _)) -> Just (Reduce 1 238)
    (341, Token (RBRACKET _)) -> Just (Reduce 1 238)
    (341, Token (EXCL _)) -> Just (Reduce 1 238)
    (341, Token (QCONID _)) -> Just (Reduce 1 238)
    (341, Token (EXPORT _)) -> Just (Reduce 1 238)
    (341, Token (AS _)) -> Just (Reduce 1 238)
    (341, Token (QVARID _)) -> Just (Reduce 1 238)
    (341, Token (LARROW _)) -> Just (Reduce 1 238)
    (341, Token (THEN _)) -> Just (Reduce 1 238)
    (341, Token (ELSE _)) -> Just (Reduce 1 238)
    (341, Token (QVARSYM _)) -> Just (Reduce 1 238)
    (341, Token (BACKQUOTE _)) -> Just (Reduce 1 238)
    (341, Token (QCONSYM _)) -> Just (Reduce 1 238)
    (341, Token (OF _)) -> Just (Reduce 1 238)
    (341, Token (INTEGER _)) -> Just (Reduce 1 238)
    (342, Token (RPAREN _)) -> Just (Reduce 3 110)
    (342, Token (COMMA _)) -> Just (Shift 159)
    (343, Token (RPAREN _)) -> Just (Reduce 3 111)
    (344, Token (RPAREN _)) -> Just (Reduce 1 117)
    (344, Token (COMMA _)) -> Just (Shift 344)
    (345, Token (RPAREN _)) -> Just (Reduce 2 118)
    (346, Token (RBRACE _)) -> Just (Reduce 3 122)
    (346, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (346, Token (DERIVING _)) -> Just (Reduce 3 122)
    (347, Token (RBRACE _)) -> Just (Reduce 1 121)
    (347, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (347, Token (DERIVING _)) -> Just (Reduce 1 121)
    (347, Token (PIPE _)) -> Just (Shift 135)
    (348, Token (RBRACE _)) -> Just (Reduce 3 125)
    (348, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (348, Token (DERIVING _)) -> Just (Reduce 3 125)
    (348, Token (PIPE _)) -> Just (Reduce 3 125)
    (349, Token (RBRACE _)) -> Just (Reduce 4 126)
    (349, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (349, Token (DERIVING _)) -> Just (Reduce 4 126)
    (349, Token (PIPE _)) -> Just (Reduce 4 126)
    (350, Token (RBRACE _)) -> Just (Shift 349)
    (351, Token (BACKQUOTE _)) -> Just (Shift 354)
    (352, Token (QCONID _)) -> Just (Shift 351)
    (352, Token (EXPORT _)) -> Just (Shift 456)
    (352, Token (AS _)) -> Just (Shift 457)
    (352, Token (QVARID _)) -> Just (Shift 458)
    (353, Token (QCONID _)) -> Just (Shift 351)
    (354, Token (RBRACE _)) -> Just (Reduce 3 229)
    (354, Token (LPAREN _)) -> Just (Reduce 3 229)
    (354, Token (RPAREN _)) -> Just (Reduce 3 229)
    (354, Token (COMMA _)) -> Just (Reduce 3 229)
    (354, Token (SEMICOLON _)) -> Just (Reduce 3 229)
    (354, Token (MINUS _)) -> Just (Reduce 3 229)
    (354, Token (RARROW _)) -> Just (Reduce 3 229)
    (354, Token (LBRACKET _)) -> Just (Reduce 3 229)
    (354, Token (RBRACKET _)) -> Just (Reduce 3 229)
    (354, Token (EXCL _)) -> Just (Reduce 3 229)
    (354, Token (QCONID _)) -> Just (Reduce 3 229)
    (354, Token (EXPORT _)) -> Just (Reduce 3 229)
    (354, Token (AS _)) -> Just (Reduce 3 229)
    (354, Token (QVARID _)) -> Just (Reduce 3 229)
    (354, Token (QVARSYM _)) -> Just (Reduce 3 229)
    (354, Token (BACKQUOTE _)) -> Just (Reduce 3 229)
    (354, Token (QCONSYM _)) -> Just (Reduce 3 229)
    (355, Token (RBRACE _)) -> Just (Reduce 1 228)
    (355, Token (LPAREN _)) -> Just (Reduce 1 228)
    (355, Token (RPAREN _)) -> Just (Reduce 1 228)
    (355, Token (COMMA _)) -> Just (Reduce 1 228)
    (355, Token (SEMICOLON _)) -> Just (Reduce 1 228)
    (355, Token (MINUS _)) -> Just (Reduce 1 228)
    (355, Token (RARROW _)) -> Just (Reduce 1 228)
    (355, Token (LBRACKET _)) -> Just (Reduce 1 228)
    (355, Token (RBRACKET _)) -> Just (Reduce 1 228)
    (355, Token (EXCL _)) -> Just (Reduce 1 228)
    (355, Token (QCONID _)) -> Just (Reduce 1 228)
    (355, Token (EXPORT _)) -> Just (Reduce 1 228)
    (355, Token (AS _)) -> Just (Reduce 1 228)
    (355, Token (QVARID _)) -> Just (Reduce 1 228)
    (355, Token (QVARSYM _)) -> Just (Reduce 1 228)
    (355, Token (BACKQUOTE _)) -> Just (Reduce 1 228)
    (355, Token (QCONSYM _)) -> Just (Reduce 1 228)
    (356, Token (RBRACE _)) -> Just (Reduce 3 130)
    (357, Token (RBRACE _)) -> Just (Reduce 1 129)
    (357, Token (COMMA _)) -> Just (Shift 102)
    (358, Token (RBRACE _)) -> Just (Reduce 3 131)
    (358, Token (COMMA _)) -> Just (Reduce 3 131)
    (359, Token (COLON_COLON _)) -> Just (Shift 147)
    (360, Token (EXPORT _)) -> Just (Reduce 1 139)
    (360, Token (AS _)) -> Just (Reduce 1 139)
    (360, Token (QVARID _)) -> Just (Reduce 1 139)
    (360, Token (STRING _)) -> Just (Reduce 1 139)
    (361, Token (EXPORT _)) -> Just (Reduce 1 138)
    (361, Token (AS _)) -> Just (Reduce 1 138)
    (361, Token (QVARID _)) -> Just (Reduce 1 138)
    (361, Token (STRING _)) -> Just (Reduce 1 138)
    (362, Token (EXPORT _)) -> Just (Reduce 1 140)
    (362, Token (AS _)) -> Just (Reduce 1 140)
    (362, Token (QVARID _)) -> Just (Reduce 1 140)
    (362, Token (STRING _)) -> Just (Reduce 1 140)
    (363, Token (LPAREN _)) -> Just (Reduce 1 141)
    (363, Token (MINUS _)) -> Just (Reduce 1 141)
    (363, Token (EXPORT _)) -> Just (Reduce 1 141)
    (363, Token (AS _)) -> Just (Reduce 1 141)
    (363, Token (QVARID _)) -> Just (Reduce 1 141)
    (363, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (364, Token (STRING _)) -> Just (Reduce 1 144)
    (365, Token (STRING _)) -> Just (Reduce 1 143)
    (366, Token (STRING _)) -> Just (Reduce 1 145)
    (367, Token (LPAREN _)) -> Just (Reduce 1 142)
    (367, Token (MINUS _)) -> Just (Reduce 1 142)
    (367, Token (EXPORT _)) -> Just (Reduce 1 142)
    (367, Token (AS _)) -> Just (Reduce 1 142)
    (367, Token (QVARID _)) -> Just (Reduce 1 142)
    (367, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (368, Token (EQUAL _)) -> Just (Reduce 3 149)
    (369, Token (COMMA _)) -> Just (Shift 72)
    (369, Token (EQUAL _)) -> Just (Reduce 1 148)
    (370, Token (COMMA _)) -> Just (Reduce 2 151)
    (370, Token (EQUAL _)) -> Just (Reduce 2 151)
    (370, Token (IN _)) -> Just (Shift 36)
    (371, Token (COMMA _)) -> Just (Reduce 3 150)
    (371, Token (EQUAL _)) -> Just (Reduce 3 150)
    (372, Token (COMMA _)) -> Just (Reduce 1 152)
    (372, Token (EQUAL _)) -> Just (Reduce 1 152)
    (372, Token (LARROW _)) -> Just (Shift 76)
    (373, Token (BACKQUOTE _)) -> Just (Shift 39)
    (374, Token (BACKQUOTE _)) -> Just (Shift 40)
    (375, Token (BACKQUOTE _)) -> Just (Shift 41)
    (376, Token (BACKQUOTE _)) -> Just (Shift 42)
    (377, Token (QCONID _)) -> Just (Shift 373)
    (377, Token (EXPORT _)) -> Just (Shift 374)
    (377, Token (AS _)) -> Just (Shift 375)
    (377, Token (QVARID _)) -> Just (Shift 376)
    (378, Token (WHERE _)) -> Just (Reduce 5 165)
    (378, Token (RBRACE _)) -> Just (Reduce 5 165)
    (378, Token (RPAREN _)) -> Just (Reduce 5 165)
    (378, Token (COMMA _)) -> Just (Reduce 5 165)
    (378, Token (DOT_DOT _)) -> Just (Reduce 5 165)
    (378, Token (SEMICOLON _)) -> Just (Reduce 5 165)
    (378, Token (EQUAL _)) -> Just (Reduce 5 165)
    (378, Token (PIPE _)) -> Just (Reduce 5 165)
    (378, Token (RBRACKET _)) -> Just (Reduce 5 165)
    (378, Token (LARROW _)) -> Just (Reduce 5 165)
    (378, Token (THEN _)) -> Just (Reduce 5 165)
    (378, Token (ELSE _)) -> Just (Reduce 5 165)
    (378, Token (OF _)) -> Just (Reduce 5 165)
    (379, Token (WHERE _)) -> Just (Reduce 3 164)
    (379, Token (RBRACE _)) -> Just (Reduce 3 164)
    (379, Token (RPAREN _)) -> Just (Reduce 3 164)
    (379, Token (COMMA _)) -> Just (Reduce 3 164)
    (379, Token (DOT_DOT _)) -> Just (Reduce 3 164)
    (379, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (379, Token (EQUAL _)) -> Just (Reduce 3 164)
    (379, Token (PIPE _)) -> Just (Reduce 3 164)
    (379, Token (RBRACKET _)) -> Just (Reduce 3 164)
    (379, Token (LARROW _)) -> Just (Reduce 3 164)
    (379, Token (THEN _)) -> Just (Reduce 3 164)
    (379, Token (ELSE _)) -> Just (Reduce 3 164)
    (379, Token (OF _)) -> Just (Reduce 3 164)
    (380, Token (IN _)) -> Just (Shift 36)
    (381, Token (RBRACE _)) -> Just (Reduce 2 210)
    (381, Token (SEMICOLON _)) -> Just (Reduce 2 210)
    (381, Token (IN _)) -> Just (Shift 36)
    (382, Token (WHERE _)) -> Just (Reduce 3 157)
    (382, Token (RBRACE _)) -> Just (Reduce 3 157)
    (382, Token (RPAREN _)) -> Just (Reduce 3 157)
    (382, Token (COMMA _)) -> Just (Reduce 3 157)
    (382, Token (DOT_DOT _)) -> Just (Reduce 3 157)
    (382, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (382, Token (EQUAL _)) -> Just (Reduce 3 157)
    (382, Token (PIPE _)) -> Just (Reduce 3 157)
    (382, Token (RBRACKET _)) -> Just (Reduce 3 157)
    (382, Token (LARROW _)) -> Just (Reduce 3 157)
    (382, Token (THEN _)) -> Just (Reduce 3 157)
    (382, Token (ELSE _)) -> Just (Reduce 3 157)
    (382, Token (OF _)) -> Just (Reduce 3 157)
    (383, Token (WHERE _)) -> Just (Reduce 4 154)
    (383, Token (RBRACE _)) -> Just (Reduce 4 154)
    (383, Token (RPAREN _)) -> Just (Reduce 4 154)
    (383, Token (COMMA _)) -> Just (Reduce 4 154)
    (383, Token (DOT_DOT _)) -> Just (Reduce 4 154)
    (383, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (383, Token (EQUAL _)) -> Just (Reduce 4 154)
    (383, Token (PIPE _)) -> Just (Reduce 4 154)
    (383, Token (RBRACKET _)) -> Just (Reduce 4 154)
    (383, Token (LARROW _)) -> Just (Reduce 4 154)
    (383, Token (THEN _)) -> Just (Reduce 4 154)
    (383, Token (ELSE _)) -> Just (Reduce 4 154)
    (383, Token (OF _)) -> Just (Reduce 4 154)
    (384, Token (WHERE _)) -> Just (Reduce 4 155)
    (384, Token (RBRACE _)) -> Just (Reduce 4 155)
    (384, Token (RPAREN _)) -> Just (Reduce 4 155)
    (384, Token (COMMA _)) -> Just (Reduce 4 155)
    (384, Token (DOT_DOT _)) -> Just (Reduce 4 155)
    (384, Token (SEMICOLON _)) -> Just (Reduce 4 155)
    (384, Token (EQUAL _)) -> Just (Reduce 4 155)
    (384, Token (PIPE _)) -> Just (Reduce 4 155)
    (384, Token (RBRACKET _)) -> Just (Reduce 4 155)
    (384, Token (LARROW _)) -> Just (Reduce 4 155)
    (384, Token (THEN _)) -> Just (Reduce 4 155)
    (384, Token (ELSE _)) -> Just (Reduce 4 155)
    (384, Token (OF _)) -> Just (Reduce 4 155)
    (385, Token (SEMICOLON _)) -> Just (Shift 397)
    (385, Token (THEN _)) -> Just (Reduce 0 243)
    (386, Token (SEMICOLON _)) -> Just (Shift 397)
    (386, Token (ELSE _)) -> Just (Reduce 0 243)
    (387, Token (WHERE _)) -> Just (Reduce 8 156)
    (387, Token (RBRACE _)) -> Just (Reduce 8 156)
    (387, Token (RPAREN _)) -> Just (Reduce 8 156)
    (387, Token (COMMA _)) -> Just (Reduce 8 156)
    (387, Token (DOT_DOT _)) -> Just (Reduce 8 156)
    (387, Token (SEMICOLON _)) -> Just (Reduce 8 156)
    (387, Token (EQUAL _)) -> Just (Reduce 8 156)
    (387, Token (PIPE _)) -> Just (Reduce 8 156)
    (387, Token (RBRACKET _)) -> Just (Reduce 8 156)
    (387, Token (LARROW _)) -> Just (Reduce 8 156)
    (387, Token (THEN _)) -> Just (Reduce 8 156)
    (387, Token (ELSE _)) -> Just (Reduce 8 156)
    (387, Token (OF _)) -> Just (Reduce 8 156)
    (388, Token (WHERE _)) -> Just (Reduce 3 158)
    (388, Token (RBRACE _)) -> Just (Reduce 3 158)
    (388, Token (RPAREN _)) -> Just (Reduce 3 158)
    (388, Token (COMMA _)) -> Just (Reduce 3 158)
    (388, Token (DOT_DOT _)) -> Just (Reduce 3 158)
    (388, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (388, Token (EQUAL _)) -> Just (Reduce 3 158)
    (388, Token (PIPE _)) -> Just (Reduce 3 158)
    (388, Token (RBRACKET _)) -> Just (Reduce 3 158)
    (388, Token (LARROW _)) -> Just (Reduce 3 158)
    (388, Token (THEN _)) -> Just (Reduce 3 158)
    (388, Token (ELSE _)) -> Just (Reduce 3 158)
    (388, Token (OF _)) -> Just (Reduce 3 158)
    (389, Token (WHERE _)) -> Just (Reduce 5 163)
    (389, Token (RBRACE _)) -> Just (Reduce 5 163)
    (389, Token (RPAREN _)) -> Just (Reduce 5 163)
    (389, Token (COMMA _)) -> Just (Reduce 5 163)
    (389, Token (DOT_DOT _)) -> Just (Reduce 5 163)
    (389, Token (SEMICOLON _)) -> Just (Reduce 5 163)
    (389, Token (EQUAL _)) -> Just (Reduce 5 163)
    (389, Token (PIPE _)) -> Just (Reduce 5 163)
    (389, Token (RBRACKET _)) -> Just (Reduce 5 163)
    (389, Token (LARROW _)) -> Just (Reduce 5 163)
    (389, Token (THEN _)) -> Just (Reduce 5 163)
    (389, Token (ELSE _)) -> Just (Reduce 5 163)
    (389, Token (OF _)) -> Just (Reduce 5 163)
    (390, Token (WHERE _)) -> Just (Reduce 5 160)
    (390, Token (RBRACE _)) -> Just (Reduce 5 160)
    (390, Token (RPAREN _)) -> Just (Reduce 5 160)
    (390, Token (COMMA _)) -> Just (Reduce 5 160)
    (390, Token (DOT_DOT _)) -> Just (Reduce 5 160)
    (390, Token (SEMICOLON _)) -> Just (Reduce 5 160)
    (390, Token (EQUAL _)) -> Just (Reduce 5 160)
    (390, Token (PIPE _)) -> Just (Reduce 5 160)
    (390, Token (RBRACKET _)) -> Just (Reduce 5 160)
    (390, Token (LARROW _)) -> Just (Reduce 5 160)
    (390, Token (THEN _)) -> Just (Reduce 5 160)
    (390, Token (ELSE _)) -> Just (Reduce 5 160)
    (390, Token (OF _)) -> Just (Reduce 5 160)
    (391, Token (WHERE _)) -> Just (Reduce 5 159)
    (391, Token (RBRACE _)) -> Just (Reduce 5 159)
    (391, Token (RPAREN _)) -> Just (Reduce 5 159)
    (391, Token (COMMA _)) -> Just (Reduce 5 159)
    (391, Token (DOT_DOT _)) -> Just (Reduce 5 159)
    (391, Token (SEMICOLON _)) -> Just (Reduce 5 159)
    (391, Token (EQUAL _)) -> Just (Reduce 5 159)
    (391, Token (PIPE _)) -> Just (Reduce 5 159)
    (391, Token (RBRACKET _)) -> Just (Reduce 5 159)
    (391, Token (LARROW _)) -> Just (Reduce 5 159)
    (391, Token (THEN _)) -> Just (Reduce 5 159)
    (391, Token (ELSE _)) -> Just (Reduce 5 159)
    (391, Token (OF _)) -> Just (Reduce 5 159)
    (392, Token (WHERE _)) -> Just (Reduce 5 161)
    (392, Token (RBRACE _)) -> Just (Reduce 5 161)
    (392, Token (RPAREN _)) -> Just (Reduce 5 161)
    (392, Token (COMMA _)) -> Just (Reduce 5 161)
    (392, Token (DOT_DOT _)) -> Just (Reduce 5 161)
    (392, Token (SEMICOLON _)) -> Just (Reduce 5 161)
    (392, Token (EQUAL _)) -> Just (Reduce 5 161)
    (392, Token (PIPE _)) -> Just (Reduce 5 161)
    (392, Token (RBRACKET _)) -> Just (Reduce 5 161)
    (392, Token (LARROW _)) -> Just (Reduce 5 161)
    (392, Token (THEN _)) -> Just (Reduce 5 161)
    (392, Token (ELSE _)) -> Just (Reduce 5 161)
    (392, Token (OF _)) -> Just (Reduce 5 161)
    (393, Token (WHERE _)) -> Just (Reduce 3 162)
    (393, Token (RBRACE _)) -> Just (Reduce 3 162)
    (393, Token (RPAREN _)) -> Just (Reduce 3 162)
    (393, Token (COMMA _)) -> Just (Reduce 3 162)
    (393, Token (DOT_DOT _)) -> Just (Reduce 3 162)
    (393, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (393, Token (EQUAL _)) -> Just (Reduce 3 162)
    (393, Token (PIPE _)) -> Just (Reduce 3 162)
    (393, Token (RBRACKET _)) -> Just (Reduce 3 162)
    (393, Token (LARROW _)) -> Just (Reduce 3 162)
    (393, Token (THEN _)) -> Just (Reduce 3 162)
    (393, Token (ELSE _)) -> Just (Reduce 3 162)
    (393, Token (OF _)) -> Just (Reduce 3 162)
    (394, Token (THEN _)) -> Just (Shift 74)
    (395, Token (ELSE _)) -> Just (Shift 37)
    (396, Token (WHERE _)) -> Just (Reduce 1 166)
    (396, Token (RBRACE _)) -> Just (Reduce 1 166)
    (396, Token (RPAREN _)) -> Just (Reduce 1 166)
    (396, Token (COMMA _)) -> Just (Reduce 1 166)
    (396, Token (DOT_DOT _)) -> Just (Reduce 1 166)
    (396, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (396, Token (EQUAL _)) -> Just (Reduce 1 166)
    (396, Token (PIPE _)) -> Just (Reduce 1 166)
    (396, Token (COLON_COLON _)) -> Just (Shift 125)
    (396, Token (MINUS _)) -> Just (Shift 34)
    (396, Token (RBRACKET _)) -> Just (Reduce 1 166)
    (396, Token (LARROW _)) -> Just (Reduce 1 166)
    (396, Token (THEN _)) -> Just (Reduce 1 166)
    (396, Token (ELSE _)) -> Just (Reduce 1 166)
    (396, Token (QVARSYM _)) -> Just (Shift 38)
    (396, Token (BACKQUOTE _)) -> Just (Shift 377)
    (396, Token (QCONSYM _)) -> Just (Shift 43)
    (396, Token (OF _)) -> Just (Reduce 1 166)
    (397, Token (THEN _)) -> Just (Reduce 1 244)
    (397, Token (ELSE _)) -> Just (Reduce 1 244)
    (398, Token (WHERE _)) -> Just (Reduce 6 168)
    (398, Token (RBRACE _)) -> Just (Reduce 6 168)
    (398, Token (RPAREN _)) -> Just (Reduce 6 168)
    (398, Token (COMMA _)) -> Just (Reduce 6 168)
    (398, Token (DOT_DOT _)) -> Just (Reduce 6 168)
    (398, Token (SEMICOLON _)) -> Just (Reduce 6 168)
    (398, Token (EQUAL _)) -> Just (Reduce 6 168)
    (398, Token (PIPE _)) -> Just (Reduce 6 168)
    (398, Token (COLON_COLON _)) -> Just (Reduce 6 168)
    (398, Token (MINUS _)) -> Just (Reduce 6 168)
    (398, Token (RBRACKET _)) -> Just (Reduce 6 168)
    (398, Token (LARROW _)) -> Just (Reduce 6 168)
    (398, Token (THEN _)) -> Just (Reduce 6 168)
    (398, Token (ELSE _)) -> Just (Reduce 6 168)
    (398, Token (QVARSYM _)) -> Just (Reduce 6 168)
    (398, Token (BACKQUOTE _)) -> Just (Reduce 6 168)
    (398, Token (QCONSYM _)) -> Just (Reduce 6 168)
    (398, Token (OF _)) -> Just (Reduce 6 168)
    (399, Token (WHERE _)) -> Just (Reduce 4 169)
    (399, Token (RBRACE _)) -> Just (Reduce 4 169)
    (399, Token (RPAREN _)) -> Just (Reduce 4 169)
    (399, Token (COMMA _)) -> Just (Reduce 4 169)
    (399, Token (DOT_DOT _)) -> Just (Reduce 4 169)
    (399, Token (SEMICOLON _)) -> Just (Reduce 4 169)
    (399, Token (EQUAL _)) -> Just (Reduce 4 169)
    (399, Token (PIPE _)) -> Just (Reduce 4 169)
    (399, Token (COLON_COLON _)) -> Just (Reduce 4 169)
    (399, Token (MINUS _)) -> Just (Reduce 4 169)
    (399, Token (RBRACKET _)) -> Just (Reduce 4 169)
    (399, Token (LARROW _)) -> Just (Reduce 4 169)
    (399, Token (THEN _)) -> Just (Reduce 4 169)
    (399, Token (ELSE _)) -> Just (Reduce 4 169)
    (399, Token (QVARSYM _)) -> Just (Reduce 4 169)
    (399, Token (BACKQUOTE _)) -> Just (Reduce 4 169)
    (399, Token (QCONSYM _)) -> Just (Reduce 4 169)
    (399, Token (OF _)) -> Just (Reduce 4 169)
    (400, Token (LBRACE _)) -> Just (Shift 91)
    (401, Token (LBRACE _)) -> Just (Shift 51)
    (402, Token (OF _)) -> Just (Shift 400)
    (403, Token (WHERE _)) -> Just (Reduce 2 167)
    (403, Token (RBRACE _)) -> Just (Reduce 2 167)
    (403, Token (RPAREN _)) -> Just (Reduce 2 167)
    (403, Token (COMMA _)) -> Just (Reduce 2 167)
    (403, Token (DOT_DOT _)) -> Just (Reduce 2 167)
    (403, Token (SEMICOLON _)) -> Just (Reduce 2 167)
    (403, Token (EQUAL _)) -> Just (Reduce 2 167)
    (403, Token (PIPE _)) -> Just (Reduce 2 167)
    (403, Token (COLON_COLON _)) -> Just (Reduce 2 167)
    (403, Token (MINUS _)) -> Just (Reduce 2 167)
    (403, Token (RBRACKET _)) -> Just (Reduce 2 167)
    (403, Token (LARROW _)) -> Just (Reduce 2 167)
    (403, Token (THEN _)) -> Just (Reduce 2 167)
    (403, Token (ELSE _)) -> Just (Reduce 2 167)
    (403, Token (QVARSYM _)) -> Just (Reduce 2 167)
    (403, Token (BACKQUOTE _)) -> Just (Reduce 2 167)
    (403, Token (QCONSYM _)) -> Just (Reduce 2 167)
    (403, Token (OF _)) -> Just (Reduce 2 167)
    (404, Token (RBRACE _)) -> Just (Shift 398)
    (405, Token (RBRACE _)) -> Just (Shift 399)
    (406, Token (RBRACE _)) -> Just (Reduce 3 194)
    (407, Token (RBRACE _)) -> Just (Reduce 1 193)
    (407, Token (SEMICOLON _)) -> Just (Shift 92)
    (408, Token (RBRACE _)) -> Just (Reduce 3 206)
    (409, Token (RBRACE _)) -> Just (Reduce 1 205)
    (409, Token (SEMICOLON _)) -> Just (Shift 52)
    (410, Token (WHERE _)) -> Just (Reduce 1 171)
    (410, Token (LBRACE _)) -> Just (Reduce 1 171)
    (410, Token (RBRACE _)) -> Just (Reduce 1 171)
    (410, Token (LPAREN _)) -> Just (Reduce 1 171)
    (410, Token (RPAREN _)) -> Just (Reduce 1 171)
    (410, Token (COMMA _)) -> Just (Reduce 1 171)
    (410, Token (DOT_DOT _)) -> Just (Reduce 1 171)
    (410, Token (SEMICOLON _)) -> Just (Reduce 1 171)
    (410, Token (EQUAL _)) -> Just (Reduce 1 171)
    (410, Token (PIPE _)) -> Just (Reduce 1 171)
    (410, Token (COLON_COLON _)) -> Just (Reduce 1 171)
    (410, Token (MINUS _)) -> Just (Reduce 1 171)
    (410, Token (INFIXL _)) -> Just (Reduce 1 171)
    (410, Token (INFIXR _)) -> Just (Reduce 1 171)
    (410, Token (INFIX _)) -> Just (Reduce 1 171)
    (410, Token (LBRACKET _)) -> Just (Reduce 1 171)
    (410, Token (RBRACKET _)) -> Just (Reduce 1 171)
    (410, Token (QCONID _)) -> Just (Reduce 1 171)
    (410, Token (EXPORT _)) -> Just (Reduce 1 171)
    (410, Token (AS _)) -> Just (Reduce 1 171)
    (410, Token (QVARID _)) -> Just (Reduce 1 171)
    (410, Token (STRING _)) -> Just (Reduce 1 171)
    (410, Token (LARROW _)) -> Just (Reduce 1 171)
    (410, Token (LET _)) -> Just (Reduce 1 171)
    (410, Token (LAMBDA _)) -> Just (Reduce 1 171)
    (410, Token (IF _)) -> Just (Reduce 1 171)
    (410, Token (THEN _)) -> Just (Reduce 1 171)
    (410, Token (ELSE _)) -> Just (Reduce 1 171)
    (410, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (410, Token (BACKQUOTE _)) -> Just (Reduce 1 171)
    (410, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (410, Token (CASE _)) -> Just (Reduce 1 171)
    (410, Token (OF _)) -> Just (Reduce 1 171)
    (410, Token (DO _)) -> Just (Reduce 1 171)
    (410, Token (INTEGER _)) -> Just (Reduce 1 171)
    (411, Token (WHERE _)) -> Just (Reduce 2 172)
    (411, Token (LBRACE _)) -> Just (Reduce 2 172)
    (411, Token (RBRACE _)) -> Just (Reduce 2 172)
    (411, Token (LPAREN _)) -> Just (Reduce 2 172)
    (411, Token (RPAREN _)) -> Just (Reduce 2 172)
    (411, Token (COMMA _)) -> Just (Reduce 2 172)
    (411, Token (DOT_DOT _)) -> Just (Reduce 2 172)
    (411, Token (SEMICOLON _)) -> Just (Reduce 2 172)
    (411, Token (EQUAL _)) -> Just (Reduce 2 172)
    (411, Token (PIPE _)) -> Just (Reduce 2 172)
    (411, Token (COLON_COLON _)) -> Just (Reduce 2 172)
    (411, Token (MINUS _)) -> Just (Reduce 2 172)
    (411, Token (INFIXL _)) -> Just (Reduce 2 172)
    (411, Token (INFIXR _)) -> Just (Reduce 2 172)
    (411, Token (INFIX _)) -> Just (Reduce 2 172)
    (411, Token (LBRACKET _)) -> Just (Reduce 2 172)
    (411, Token (RBRACKET _)) -> Just (Reduce 2 172)
    (411, Token (QCONID _)) -> Just (Reduce 2 172)
    (411, Token (EXPORT _)) -> Just (Reduce 2 172)
    (411, Token (AS _)) -> Just (Reduce 2 172)
    (411, Token (QVARID _)) -> Just (Reduce 2 172)
    (411, Token (STRING _)) -> Just (Reduce 2 172)
    (411, Token (LARROW _)) -> Just (Reduce 2 172)
    (411, Token (LET _)) -> Just (Reduce 2 172)
    (411, Token (LAMBDA _)) -> Just (Reduce 2 172)
    (411, Token (IF _)) -> Just (Reduce 2 172)
    (411, Token (THEN _)) -> Just (Reduce 2 172)
    (411, Token (ELSE _)) -> Just (Reduce 2 172)
    (411, Token (QVARSYM _)) -> Just (Reduce 2 172)
    (411, Token (BACKQUOTE _)) -> Just (Reduce 2 172)
    (411, Token (QCONSYM _)) -> Just (Reduce 2 172)
    (411, Token (CASE _)) -> Just (Reduce 2 172)
    (411, Token (OF _)) -> Just (Reduce 2 172)
    (411, Token (DO _)) -> Just (Reduce 2 172)
    (411, Token (INTEGER _)) -> Just (Reduce 2 172)
    (412, Token (WHERE _)) -> Just (Reduce 3 180)
    (412, Token (LBRACE _)) -> Just (Reduce 3 180)
    (412, Token (RBRACE _)) -> Just (Reduce 3 180)
    (412, Token (LPAREN _)) -> Just (Reduce 3 180)
    (412, Token (RPAREN _)) -> Just (Reduce 3 180)
    (412, Token (COMMA _)) -> Just (Reduce 3 180)
    (412, Token (DOT_DOT _)) -> Just (Reduce 3 180)
    (412, Token (SEMICOLON _)) -> Just (Reduce 3 180)
    (412, Token (EQUAL _)) -> Just (Reduce 3 180)
    (412, Token (PIPE _)) -> Just (Reduce 3 180)
    (412, Token (COLON_COLON _)) -> Just (Reduce 3 180)
    (412, Token (MINUS _)) -> Just (Reduce 3 180)
    (412, Token (INFIXL _)) -> Just (Reduce 3 180)
    (412, Token (INFIXR _)) -> Just (Reduce 3 180)
    (412, Token (INFIX _)) -> Just (Reduce 3 180)
    (412, Token (LBRACKET _)) -> Just (Reduce 3 180)
    (412, Token (RBRACKET _)) -> Just (Reduce 3 180)
    (412, Token (QCONID _)) -> Just (Reduce 3 180)
    (412, Token (EXPORT _)) -> Just (Reduce 3 180)
    (412, Token (AS _)) -> Just (Reduce 3 180)
    (412, Token (QVARID _)) -> Just (Reduce 3 180)
    (412, Token (STRING _)) -> Just (Reduce 3 180)
    (412, Token (LARROW _)) -> Just (Reduce 3 180)
    (412, Token (LET _)) -> Just (Reduce 3 180)
    (412, Token (LAMBDA _)) -> Just (Reduce 3 180)
    (412, Token (IF _)) -> Just (Reduce 3 180)
    (412, Token (THEN _)) -> Just (Reduce 3 180)
    (412, Token (ELSE _)) -> Just (Reduce 3 180)
    (412, Token (QVARSYM _)) -> Just (Reduce 3 180)
    (412, Token (BACKQUOTE _)) -> Just (Reduce 3 180)
    (412, Token (QCONSYM _)) -> Just (Reduce 3 180)
    (412, Token (CASE _)) -> Just (Reduce 3 180)
    (412, Token (OF _)) -> Just (Reduce 3 180)
    (412, Token (DO _)) -> Just (Reduce 3 180)
    (412, Token (INTEGER _)) -> Just (Reduce 3 180)
    (413, Token (WHERE _)) -> Just (Reduce 4 187)
    (413, Token (LBRACE _)) -> Just (Reduce 4 187)
    (413, Token (RBRACE _)) -> Just (Reduce 4 187)
    (413, Token (LPAREN _)) -> Just (Reduce 4 187)
    (413, Token (RPAREN _)) -> Just (Reduce 4 187)
    (413, Token (COMMA _)) -> Just (Reduce 4 187)
    (413, Token (DOT_DOT _)) -> Just (Reduce 4 187)
    (413, Token (SEMICOLON _)) -> Just (Reduce 4 187)
    (413, Token (EQUAL _)) -> Just (Reduce 4 187)
    (413, Token (PIPE _)) -> Just (Reduce 4 187)
    (413, Token (COLON_COLON _)) -> Just (Reduce 4 187)
    (413, Token (MINUS _)) -> Just (Reduce 4 187)
    (413, Token (INFIXL _)) -> Just (Reduce 4 187)
    (413, Token (INFIXR _)) -> Just (Reduce 4 187)
    (413, Token (INFIX _)) -> Just (Reduce 4 187)
    (413, Token (LBRACKET _)) -> Just (Reduce 4 187)
    (413, Token (RBRACKET _)) -> Just (Reduce 4 187)
    (413, Token (QCONID _)) -> Just (Reduce 4 187)
    (413, Token (EXPORT _)) -> Just (Reduce 4 187)
    (413, Token (AS _)) -> Just (Reduce 4 187)
    (413, Token (QVARID _)) -> Just (Reduce 4 187)
    (413, Token (STRING _)) -> Just (Reduce 4 187)
    (413, Token (LARROW _)) -> Just (Reduce 4 187)
    (413, Token (LET _)) -> Just (Reduce 4 187)
    (413, Token (LAMBDA _)) -> Just (Reduce 4 187)
    (413, Token (IF _)) -> Just (Reduce 4 187)
    (413, Token (THEN _)) -> Just (Reduce 4 187)
    (413, Token (ELSE _)) -> Just (Reduce 4 187)
    (413, Token (QVARSYM _)) -> Just (Reduce 4 187)
    (413, Token (BACKQUOTE _)) -> Just (Reduce 4 187)
    (413, Token (QCONSYM _)) -> Just (Reduce 4 187)
    (413, Token (CASE _)) -> Just (Reduce 4 187)
    (413, Token (OF _)) -> Just (Reduce 4 187)
    (413, Token (DO _)) -> Just (Reduce 4 187)
    (413, Token (INTEGER _)) -> Just (Reduce 4 187)
    (414, Token (WHERE _)) -> Just (Reduce 6 192)
    (414, Token (LBRACE _)) -> Just (Reduce 6 192)
    (414, Token (RBRACE _)) -> Just (Reduce 6 192)
    (414, Token (LPAREN _)) -> Just (Reduce 6 192)
    (414, Token (RPAREN _)) -> Just (Reduce 6 192)
    (414, Token (COMMA _)) -> Just (Reduce 6 192)
    (414, Token (DOT_DOT _)) -> Just (Reduce 6 192)
    (414, Token (SEMICOLON _)) -> Just (Reduce 6 192)
    (414, Token (EQUAL _)) -> Just (Reduce 6 192)
    (414, Token (PIPE _)) -> Just (Reduce 6 192)
    (414, Token (COLON_COLON _)) -> Just (Reduce 6 192)
    (414, Token (MINUS _)) -> Just (Reduce 6 192)
    (414, Token (INFIXL _)) -> Just (Reduce 6 192)
    (414, Token (INFIXR _)) -> Just (Reduce 6 192)
    (414, Token (INFIX _)) -> Just (Reduce 6 192)
    (414, Token (LBRACKET _)) -> Just (Reduce 6 192)
    (414, Token (RBRACKET _)) -> Just (Reduce 6 192)
    (414, Token (QCONID _)) -> Just (Reduce 6 192)
    (414, Token (EXPORT _)) -> Just (Reduce 6 192)
    (414, Token (AS _)) -> Just (Reduce 6 192)
    (414, Token (QVARID _)) -> Just (Reduce 6 192)
    (414, Token (STRING _)) -> Just (Reduce 6 192)
    (414, Token (LARROW _)) -> Just (Reduce 6 192)
    (414, Token (LET _)) -> Just (Reduce 6 192)
    (414, Token (LAMBDA _)) -> Just (Reduce 6 192)
    (414, Token (IF _)) -> Just (Reduce 6 192)
    (414, Token (THEN _)) -> Just (Reduce 6 192)
    (414, Token (ELSE _)) -> Just (Reduce 6 192)
    (414, Token (QVARSYM _)) -> Just (Reduce 6 192)
    (414, Token (BACKQUOTE _)) -> Just (Reduce 6 192)
    (414, Token (QCONSYM _)) -> Just (Reduce 6 192)
    (414, Token (CASE _)) -> Just (Reduce 6 192)
    (414, Token (OF _)) -> Just (Reduce 6 192)
    (414, Token (DO _)) -> Just (Reduce 6 192)
    (414, Token (INTEGER _)) -> Just (Reduce 6 192)
    (415, Token (WHERE _)) -> Just (Reduce 6 189)
    (415, Token (LBRACE _)) -> Just (Reduce 6 189)
    (415, Token (RBRACE _)) -> Just (Reduce 6 189)
    (415, Token (LPAREN _)) -> Just (Reduce 6 189)
    (415, Token (RPAREN _)) -> Just (Reduce 6 189)
    (415, Token (COMMA _)) -> Just (Reduce 6 189)
    (415, Token (DOT_DOT _)) -> Just (Reduce 6 189)
    (415, Token (SEMICOLON _)) -> Just (Reduce 6 189)
    (415, Token (EQUAL _)) -> Just (Reduce 6 189)
    (415, Token (PIPE _)) -> Just (Reduce 6 189)
    (415, Token (COLON_COLON _)) -> Just (Reduce 6 189)
    (415, Token (MINUS _)) -> Just (Reduce 6 189)
    (415, Token (INFIXL _)) -> Just (Reduce 6 189)
    (415, Token (INFIXR _)) -> Just (Reduce 6 189)
    (415, Token (INFIX _)) -> Just (Reduce 6 189)
    (415, Token (LBRACKET _)) -> Just (Reduce 6 189)
    (415, Token (RBRACKET _)) -> Just (Reduce 6 189)
    (415, Token (QCONID _)) -> Just (Reduce 6 189)
    (415, Token (EXPORT _)) -> Just (Reduce 6 189)
    (415, Token (AS _)) -> Just (Reduce 6 189)
    (415, Token (QVARID _)) -> Just (Reduce 6 189)
    (415, Token (STRING _)) -> Just (Reduce 6 189)
    (415, Token (LARROW _)) -> Just (Reduce 6 189)
    (415, Token (LET _)) -> Just (Reduce 6 189)
    (415, Token (LAMBDA _)) -> Just (Reduce 6 189)
    (415, Token (IF _)) -> Just (Reduce 6 189)
    (415, Token (THEN _)) -> Just (Reduce 6 189)
    (415, Token (ELSE _)) -> Just (Reduce 6 189)
    (415, Token (QVARSYM _)) -> Just (Reduce 6 189)
    (415, Token (BACKQUOTE _)) -> Just (Reduce 6 189)
    (415, Token (QCONSYM _)) -> Just (Reduce 6 189)
    (415, Token (CASE _)) -> Just (Reduce 6 189)
    (415, Token (OF _)) -> Just (Reduce 6 189)
    (415, Token (DO _)) -> Just (Reduce 6 189)
    (415, Token (INTEGER _)) -> Just (Reduce 6 189)
    (416, Token (WHERE _)) -> Just (Reduce 6 188)
    (416, Token (LBRACE _)) -> Just (Reduce 6 188)
    (416, Token (RBRACE _)) -> Just (Reduce 6 188)
    (416, Token (LPAREN _)) -> Just (Reduce 6 188)
    (416, Token (RPAREN _)) -> Just (Reduce 6 188)
    (416, Token (COMMA _)) -> Just (Reduce 6 188)
    (416, Token (DOT_DOT _)) -> Just (Reduce 6 188)
    (416, Token (SEMICOLON _)) -> Just (Reduce 6 188)
    (416, Token (EQUAL _)) -> Just (Reduce 6 188)
    (416, Token (PIPE _)) -> Just (Reduce 6 188)
    (416, Token (COLON_COLON _)) -> Just (Reduce 6 188)
    (416, Token (MINUS _)) -> Just (Reduce 6 188)
    (416, Token (INFIXL _)) -> Just (Reduce 6 188)
    (416, Token (INFIXR _)) -> Just (Reduce 6 188)
    (416, Token (INFIX _)) -> Just (Reduce 6 188)
    (416, Token (LBRACKET _)) -> Just (Reduce 6 188)
    (416, Token (RBRACKET _)) -> Just (Reduce 6 188)
    (416, Token (QCONID _)) -> Just (Reduce 6 188)
    (416, Token (EXPORT _)) -> Just (Reduce 6 188)
    (416, Token (AS _)) -> Just (Reduce 6 188)
    (416, Token (QVARID _)) -> Just (Reduce 6 188)
    (416, Token (STRING _)) -> Just (Reduce 6 188)
    (416, Token (LARROW _)) -> Just (Reduce 6 188)
    (416, Token (LET _)) -> Just (Reduce 6 188)
    (416, Token (LAMBDA _)) -> Just (Reduce 6 188)
    (416, Token (IF _)) -> Just (Reduce 6 188)
    (416, Token (THEN _)) -> Just (Reduce 6 188)
    (416, Token (ELSE _)) -> Just (Reduce 6 188)
    (416, Token (QVARSYM _)) -> Just (Reduce 6 188)
    (416, Token (BACKQUOTE _)) -> Just (Reduce 6 188)
    (416, Token (QCONSYM _)) -> Just (Reduce 6 188)
    (416, Token (CASE _)) -> Just (Reduce 6 188)
    (416, Token (OF _)) -> Just (Reduce 6 188)
    (416, Token (DO _)) -> Just (Reduce 6 188)
    (416, Token (INTEGER _)) -> Just (Reduce 6 188)
    (417, Token (WHERE _)) -> Just (Reduce 6 190)
    (417, Token (LBRACE _)) -> Just (Reduce 6 190)
    (417, Token (RBRACE _)) -> Just (Reduce 6 190)
    (417, Token (LPAREN _)) -> Just (Reduce 6 190)
    (417, Token (RPAREN _)) -> Just (Reduce 6 190)
    (417, Token (COMMA _)) -> Just (Reduce 6 190)
    (417, Token (DOT_DOT _)) -> Just (Reduce 6 190)
    (417, Token (SEMICOLON _)) -> Just (Reduce 6 190)
    (417, Token (EQUAL _)) -> Just (Reduce 6 190)
    (417, Token (PIPE _)) -> Just (Reduce 6 190)
    (417, Token (COLON_COLON _)) -> Just (Reduce 6 190)
    (417, Token (MINUS _)) -> Just (Reduce 6 190)
    (417, Token (INFIXL _)) -> Just (Reduce 6 190)
    (417, Token (INFIXR _)) -> Just (Reduce 6 190)
    (417, Token (INFIX _)) -> Just (Reduce 6 190)
    (417, Token (LBRACKET _)) -> Just (Reduce 6 190)
    (417, Token (RBRACKET _)) -> Just (Reduce 6 190)
    (417, Token (QCONID _)) -> Just (Reduce 6 190)
    (417, Token (EXPORT _)) -> Just (Reduce 6 190)
    (417, Token (AS _)) -> Just (Reduce 6 190)
    (417, Token (QVARID _)) -> Just (Reduce 6 190)
    (417, Token (STRING _)) -> Just (Reduce 6 190)
    (417, Token (LARROW _)) -> Just (Reduce 6 190)
    (417, Token (LET _)) -> Just (Reduce 6 190)
    (417, Token (LAMBDA _)) -> Just (Reduce 6 190)
    (417, Token (IF _)) -> Just (Reduce 6 190)
    (417, Token (THEN _)) -> Just (Reduce 6 190)
    (417, Token (ELSE _)) -> Just (Reduce 6 190)
    (417, Token (QVARSYM _)) -> Just (Reduce 6 190)
    (417, Token (BACKQUOTE _)) -> Just (Reduce 6 190)
    (417, Token (QCONSYM _)) -> Just (Reduce 6 190)
    (417, Token (CASE _)) -> Just (Reduce 6 190)
    (417, Token (OF _)) -> Just (Reduce 6 190)
    (417, Token (DO _)) -> Just (Reduce 6 190)
    (417, Token (INTEGER _)) -> Just (Reduce 6 190)
    (418, Token (WHERE _)) -> Just (Reduce 4 191)
    (418, Token (LBRACE _)) -> Just (Reduce 4 191)
    (418, Token (RBRACE _)) -> Just (Reduce 4 191)
    (418, Token (LPAREN _)) -> Just (Reduce 4 191)
    (418, Token (RPAREN _)) -> Just (Reduce 4 191)
    (418, Token (COMMA _)) -> Just (Reduce 4 191)
    (418, Token (DOT_DOT _)) -> Just (Reduce 4 191)
    (418, Token (SEMICOLON _)) -> Just (Reduce 4 191)
    (418, Token (EQUAL _)) -> Just (Reduce 4 191)
    (418, Token (PIPE _)) -> Just (Reduce 4 191)
    (418, Token (COLON_COLON _)) -> Just (Reduce 4 191)
    (418, Token (MINUS _)) -> Just (Reduce 4 191)
    (418, Token (INFIXL _)) -> Just (Reduce 4 191)
    (418, Token (INFIXR _)) -> Just (Reduce 4 191)
    (418, Token (INFIX _)) -> Just (Reduce 4 191)
    (418, Token (LBRACKET _)) -> Just (Reduce 4 191)
    (418, Token (RBRACKET _)) -> Just (Reduce 4 191)
    (418, Token (QCONID _)) -> Just (Reduce 4 191)
    (418, Token (EXPORT _)) -> Just (Reduce 4 191)
    (418, Token (AS _)) -> Just (Reduce 4 191)
    (418, Token (QVARID _)) -> Just (Reduce 4 191)
    (418, Token (STRING _)) -> Just (Reduce 4 191)
    (418, Token (LARROW _)) -> Just (Reduce 4 191)
    (418, Token (LET _)) -> Just (Reduce 4 191)
    (418, Token (LAMBDA _)) -> Just (Reduce 4 191)
    (418, Token (IF _)) -> Just (Reduce 4 191)
    (418, Token (THEN _)) -> Just (Reduce 4 191)
    (418, Token (ELSE _)) -> Just (Reduce 4 191)
    (418, Token (QVARSYM _)) -> Just (Reduce 4 191)
    (418, Token (BACKQUOTE _)) -> Just (Reduce 4 191)
    (418, Token (QCONSYM _)) -> Just (Reduce 4 191)
    (418, Token (CASE _)) -> Just (Reduce 4 191)
    (418, Token (OF _)) -> Just (Reduce 4 191)
    (418, Token (DO _)) -> Just (Reduce 4 191)
    (418, Token (INTEGER _)) -> Just (Reduce 4 191)
    (419, Token (WHERE _)) -> Just (Reduce 3 181)
    (419, Token (LBRACE _)) -> Just (Reduce 3 181)
    (419, Token (RBRACE _)) -> Just (Reduce 3 181)
    (419, Token (LPAREN _)) -> Just (Reduce 3 181)
    (419, Token (RPAREN _)) -> Just (Reduce 3 181)
    (419, Token (COMMA _)) -> Just (Reduce 3 181)
    (419, Token (DOT_DOT _)) -> Just (Reduce 3 181)
    (419, Token (SEMICOLON _)) -> Just (Reduce 3 181)
    (419, Token (EQUAL _)) -> Just (Reduce 3 181)
    (419, Token (PIPE _)) -> Just (Reduce 3 181)
    (419, Token (COLON_COLON _)) -> Just (Reduce 3 181)
    (419, Token (MINUS _)) -> Just (Reduce 3 181)
    (419, Token (INFIXL _)) -> Just (Reduce 3 181)
    (419, Token (INFIXR _)) -> Just (Reduce 3 181)
    (419, Token (INFIX _)) -> Just (Reduce 3 181)
    (419, Token (LBRACKET _)) -> Just (Reduce 3 181)
    (419, Token (RBRACKET _)) -> Just (Reduce 3 181)
    (419, Token (QCONID _)) -> Just (Reduce 3 181)
    (419, Token (EXPORT _)) -> Just (Reduce 3 181)
    (419, Token (AS _)) -> Just (Reduce 3 181)
    (419, Token (QVARID _)) -> Just (Reduce 3 181)
    (419, Token (STRING _)) -> Just (Reduce 3 181)
    (419, Token (LARROW _)) -> Just (Reduce 3 181)
    (419, Token (LET _)) -> Just (Reduce 3 181)
    (419, Token (LAMBDA _)) -> Just (Reduce 3 181)
    (419, Token (IF _)) -> Just (Reduce 3 181)
    (419, Token (THEN _)) -> Just (Reduce 3 181)
    (419, Token (ELSE _)) -> Just (Reduce 3 181)
    (419, Token (QVARSYM _)) -> Just (Reduce 3 181)
    (419, Token (BACKQUOTE _)) -> Just (Reduce 3 181)
    (419, Token (QCONSYM _)) -> Just (Reduce 3 181)
    (419, Token (CASE _)) -> Just (Reduce 3 181)
    (419, Token (OF _)) -> Just (Reduce 3 181)
    (419, Token (DO _)) -> Just (Reduce 3 181)
    (419, Token (INTEGER _)) -> Just (Reduce 3 181)
    (420, Token (WHERE _)) -> Just (Reduce 6 185)
    (420, Token (LBRACE _)) -> Just (Reduce 6 185)
    (420, Token (RBRACE _)) -> Just (Reduce 6 185)
    (420, Token (LPAREN _)) -> Just (Reduce 6 185)
    (420, Token (RPAREN _)) -> Just (Reduce 6 185)
    (420, Token (COMMA _)) -> Just (Reduce 6 185)
    (420, Token (DOT_DOT _)) -> Just (Reduce 6 185)
    (420, Token (SEMICOLON _)) -> Just (Reduce 6 185)
    (420, Token (EQUAL _)) -> Just (Reduce 6 185)
    (420, Token (PIPE _)) -> Just (Reduce 6 185)
    (420, Token (COLON_COLON _)) -> Just (Reduce 6 185)
    (420, Token (MINUS _)) -> Just (Reduce 6 185)
    (420, Token (INFIXL _)) -> Just (Reduce 6 185)
    (420, Token (INFIXR _)) -> Just (Reduce 6 185)
    (420, Token (INFIX _)) -> Just (Reduce 6 185)
    (420, Token (LBRACKET _)) -> Just (Reduce 6 185)
    (420, Token (RBRACKET _)) -> Just (Reduce 6 185)
    (420, Token (QCONID _)) -> Just (Reduce 6 185)
    (420, Token (EXPORT _)) -> Just (Reduce 6 185)
    (420, Token (AS _)) -> Just (Reduce 6 185)
    (420, Token (QVARID _)) -> Just (Reduce 6 185)
    (420, Token (STRING _)) -> Just (Reduce 6 185)
    (420, Token (LARROW _)) -> Just (Reduce 6 185)
    (420, Token (LET _)) -> Just (Reduce 6 185)
    (420, Token (LAMBDA _)) -> Just (Reduce 6 185)
    (420, Token (IF _)) -> Just (Reduce 6 185)
    (420, Token (THEN _)) -> Just (Reduce 6 185)
    (420, Token (ELSE _)) -> Just (Reduce 6 185)
    (420, Token (QVARSYM _)) -> Just (Reduce 6 185)
    (420, Token (BACKQUOTE _)) -> Just (Reduce 6 185)
    (420, Token (QCONSYM _)) -> Just (Reduce 6 185)
    (420, Token (CASE _)) -> Just (Reduce 6 185)
    (420, Token (OF _)) -> Just (Reduce 6 185)
    (420, Token (DO _)) -> Just (Reduce 6 185)
    (420, Token (INTEGER _)) -> Just (Reduce 6 185)
    (421, Token (WHERE _)) -> Just (Reduce 4 183)
    (421, Token (LBRACE _)) -> Just (Reduce 4 183)
    (421, Token (RBRACE _)) -> Just (Reduce 4 183)
    (421, Token (LPAREN _)) -> Just (Reduce 4 183)
    (421, Token (RPAREN _)) -> Just (Reduce 4 183)
    (421, Token (COMMA _)) -> Just (Reduce 4 183)
    (421, Token (DOT_DOT _)) -> Just (Reduce 4 183)
    (421, Token (SEMICOLON _)) -> Just (Reduce 4 183)
    (421, Token (EQUAL _)) -> Just (Reduce 4 183)
    (421, Token (PIPE _)) -> Just (Reduce 4 183)
    (421, Token (COLON_COLON _)) -> Just (Reduce 4 183)
    (421, Token (MINUS _)) -> Just (Reduce 4 183)
    (421, Token (INFIXL _)) -> Just (Reduce 4 183)
    (421, Token (INFIXR _)) -> Just (Reduce 4 183)
    (421, Token (INFIX _)) -> Just (Reduce 4 183)
    (421, Token (LBRACKET _)) -> Just (Reduce 4 183)
    (421, Token (RBRACKET _)) -> Just (Reduce 4 183)
    (421, Token (QCONID _)) -> Just (Reduce 4 183)
    (421, Token (EXPORT _)) -> Just (Reduce 4 183)
    (421, Token (AS _)) -> Just (Reduce 4 183)
    (421, Token (QVARID _)) -> Just (Reduce 4 183)
    (421, Token (STRING _)) -> Just (Reduce 4 183)
    (421, Token (LARROW _)) -> Just (Reduce 4 183)
    (421, Token (LET _)) -> Just (Reduce 4 183)
    (421, Token (LAMBDA _)) -> Just (Reduce 4 183)
    (421, Token (IF _)) -> Just (Reduce 4 183)
    (421, Token (THEN _)) -> Just (Reduce 4 183)
    (421, Token (ELSE _)) -> Just (Reduce 4 183)
    (421, Token (QVARSYM _)) -> Just (Reduce 4 183)
    (421, Token (BACKQUOTE _)) -> Just (Reduce 4 183)
    (421, Token (QCONSYM _)) -> Just (Reduce 4 183)
    (421, Token (CASE _)) -> Just (Reduce 4 183)
    (421, Token (OF _)) -> Just (Reduce 4 183)
    (421, Token (DO _)) -> Just (Reduce 4 183)
    (421, Token (INTEGER _)) -> Just (Reduce 4 183)
    (422, Token (WHERE _)) -> Just (Reduce 7 186)
    (422, Token (LBRACE _)) -> Just (Reduce 7 186)
    (422, Token (RBRACE _)) -> Just (Reduce 7 186)
    (422, Token (LPAREN _)) -> Just (Reduce 7 186)
    (422, Token (RPAREN _)) -> Just (Reduce 7 186)
    (422, Token (COMMA _)) -> Just (Reduce 7 186)
    (422, Token (DOT_DOT _)) -> Just (Reduce 7 186)
    (422, Token (SEMICOLON _)) -> Just (Reduce 7 186)
    (422, Token (EQUAL _)) -> Just (Reduce 7 186)
    (422, Token (PIPE _)) -> Just (Reduce 7 186)
    (422, Token (COLON_COLON _)) -> Just (Reduce 7 186)
    (422, Token (MINUS _)) -> Just (Reduce 7 186)
    (422, Token (INFIXL _)) -> Just (Reduce 7 186)
    (422, Token (INFIXR _)) -> Just (Reduce 7 186)
    (422, Token (INFIX _)) -> Just (Reduce 7 186)
    (422, Token (LBRACKET _)) -> Just (Reduce 7 186)
    (422, Token (RBRACKET _)) -> Just (Reduce 7 186)
    (422, Token (QCONID _)) -> Just (Reduce 7 186)
    (422, Token (EXPORT _)) -> Just (Reduce 7 186)
    (422, Token (AS _)) -> Just (Reduce 7 186)
    (422, Token (QVARID _)) -> Just (Reduce 7 186)
    (422, Token (STRING _)) -> Just (Reduce 7 186)
    (422, Token (LARROW _)) -> Just (Reduce 7 186)
    (422, Token (LET _)) -> Just (Reduce 7 186)
    (422, Token (LAMBDA _)) -> Just (Reduce 7 186)
    (422, Token (IF _)) -> Just (Reduce 7 186)
    (422, Token (THEN _)) -> Just (Reduce 7 186)
    (422, Token (ELSE _)) -> Just (Reduce 7 186)
    (422, Token (QVARSYM _)) -> Just (Reduce 7 186)
    (422, Token (BACKQUOTE _)) -> Just (Reduce 7 186)
    (422, Token (QCONSYM _)) -> Just (Reduce 7 186)
    (422, Token (CASE _)) -> Just (Reduce 7 186)
    (422, Token (OF _)) -> Just (Reduce 7 186)
    (422, Token (DO _)) -> Just (Reduce 7 186)
    (422, Token (INTEGER _)) -> Just (Reduce 7 186)
    (423, Token (WHERE _)) -> Just (Reduce 5 184)
    (423, Token (LBRACE _)) -> Just (Reduce 5 184)
    (423, Token (RBRACE _)) -> Just (Reduce 5 184)
    (423, Token (LPAREN _)) -> Just (Reduce 5 184)
    (423, Token (RPAREN _)) -> Just (Reduce 5 184)
    (423, Token (COMMA _)) -> Just (Reduce 5 184)
    (423, Token (DOT_DOT _)) -> Just (Reduce 5 184)
    (423, Token (SEMICOLON _)) -> Just (Reduce 5 184)
    (423, Token (EQUAL _)) -> Just (Reduce 5 184)
    (423, Token (PIPE _)) -> Just (Reduce 5 184)
    (423, Token (COLON_COLON _)) -> Just (Reduce 5 184)
    (423, Token (MINUS _)) -> Just (Reduce 5 184)
    (423, Token (INFIXL _)) -> Just (Reduce 5 184)
    (423, Token (INFIXR _)) -> Just (Reduce 5 184)
    (423, Token (INFIX _)) -> Just (Reduce 5 184)
    (423, Token (LBRACKET _)) -> Just (Reduce 5 184)
    (423, Token (RBRACKET _)) -> Just (Reduce 5 184)
    (423, Token (QCONID _)) -> Just (Reduce 5 184)
    (423, Token (EXPORT _)) -> Just (Reduce 5 184)
    (423, Token (AS _)) -> Just (Reduce 5 184)
    (423, Token (QVARID _)) -> Just (Reduce 5 184)
    (423, Token (STRING _)) -> Just (Reduce 5 184)
    (423, Token (LARROW _)) -> Just (Reduce 5 184)
    (423, Token (LET _)) -> Just (Reduce 5 184)
    (423, Token (LAMBDA _)) -> Just (Reduce 5 184)
    (423, Token (IF _)) -> Just (Reduce 5 184)
    (423, Token (THEN _)) -> Just (Reduce 5 184)
    (423, Token (ELSE _)) -> Just (Reduce 5 184)
    (423, Token (QVARSYM _)) -> Just (Reduce 5 184)
    (423, Token (BACKQUOTE _)) -> Just (Reduce 5 184)
    (423, Token (QCONSYM _)) -> Just (Reduce 5 184)
    (423, Token (CASE _)) -> Just (Reduce 5 184)
    (423, Token (OF _)) -> Just (Reduce 5 184)
    (423, Token (DO _)) -> Just (Reduce 5 184)
    (423, Token (INTEGER _)) -> Just (Reduce 5 184)
    (424, Token (WHERE _)) -> Just (Reduce 3 182)
    (424, Token (LBRACE _)) -> Just (Reduce 3 182)
    (424, Token (RBRACE _)) -> Just (Reduce 3 182)
    (424, Token (LPAREN _)) -> Just (Reduce 3 182)
    (424, Token (RPAREN _)) -> Just (Reduce 3 182)
    (424, Token (COMMA _)) -> Just (Reduce 3 182)
    (424, Token (DOT_DOT _)) -> Just (Reduce 3 182)
    (424, Token (SEMICOLON _)) -> Just (Reduce 3 182)
    (424, Token (EQUAL _)) -> Just (Reduce 3 182)
    (424, Token (PIPE _)) -> Just (Reduce 3 182)
    (424, Token (COLON_COLON _)) -> Just (Reduce 3 182)
    (424, Token (MINUS _)) -> Just (Reduce 3 182)
    (424, Token (INFIXL _)) -> Just (Reduce 3 182)
    (424, Token (INFIXR _)) -> Just (Reduce 3 182)
    (424, Token (INFIX _)) -> Just (Reduce 3 182)
    (424, Token (LBRACKET _)) -> Just (Reduce 3 182)
    (424, Token (RBRACKET _)) -> Just (Reduce 3 182)
    (424, Token (QCONID _)) -> Just (Reduce 3 182)
    (424, Token (EXPORT _)) -> Just (Reduce 3 182)
    (424, Token (AS _)) -> Just (Reduce 3 182)
    (424, Token (QVARID _)) -> Just (Reduce 3 182)
    (424, Token (STRING _)) -> Just (Reduce 3 182)
    (424, Token (LARROW _)) -> Just (Reduce 3 182)
    (424, Token (LET _)) -> Just (Reduce 3 182)
    (424, Token (LAMBDA _)) -> Just (Reduce 3 182)
    (424, Token (IF _)) -> Just (Reduce 3 182)
    (424, Token (THEN _)) -> Just (Reduce 3 182)
    (424, Token (ELSE _)) -> Just (Reduce 3 182)
    (424, Token (QVARSYM _)) -> Just (Reduce 3 182)
    (424, Token (BACKQUOTE _)) -> Just (Reduce 3 182)
    (424, Token (QCONSYM _)) -> Just (Reduce 3 182)
    (424, Token (CASE _)) -> Just (Reduce 3 182)
    (424, Token (OF _)) -> Just (Reduce 3 182)
    (424, Token (DO _)) -> Just (Reduce 3 182)
    (424, Token (INTEGER _)) -> Just (Reduce 3 182)
    (425, Token (BACKQUOTE _)) -> Just (Shift 58)
    (426, Token (BACKQUOTE _)) -> Just (Shift 59)
    (427, Token (BACKQUOTE _)) -> Just (Shift 60)
    (428, Token (BACKQUOTE _)) -> Just (Shift 61)
    (429, Token (WHERE _)) -> Just (Reduce 1 179)
    (429, Token (LBRACE _)) -> Just (Reduce 1 179)
    (429, Token (RBRACE _)) -> Just (Reduce 1 179)
    (429, Token (LPAREN _)) -> Just (Reduce 1 179)
    (429, Token (RPAREN _)) -> Just (Reduce 1 179)
    (429, Token (COMMA _)) -> Just (Reduce 1 179)
    (429, Token (DOT_DOT _)) -> Just (Reduce 1 179)
    (429, Token (SEMICOLON _)) -> Just (Reduce 1 179)
    (429, Token (EQUAL _)) -> Just (Reduce 1 179)
    (429, Token (PIPE _)) -> Just (Reduce 1 179)
    (429, Token (COLON_COLON _)) -> Just (Reduce 1 179)
    (429, Token (MINUS _)) -> Just (Reduce 1 179)
    (429, Token (INFIXL _)) -> Just (Reduce 1 179)
    (429, Token (INFIXR _)) -> Just (Reduce 1 179)
    (429, Token (INFIX _)) -> Just (Reduce 1 179)
    (429, Token (LBRACKET _)) -> Just (Reduce 1 179)
    (429, Token (RBRACKET _)) -> Just (Reduce 1 179)
    (429, Token (QCONID _)) -> Just (Reduce 1 179)
    (429, Token (EXPORT _)) -> Just (Reduce 1 179)
    (429, Token (AS _)) -> Just (Reduce 1 179)
    (429, Token (QVARID _)) -> Just (Reduce 1 179)
    (429, Token (STRING _)) -> Just (Reduce 1 179)
    (429, Token (LARROW _)) -> Just (Reduce 1 179)
    (429, Token (LET _)) -> Just (Reduce 1 179)
    (429, Token (LAMBDA _)) -> Just (Reduce 1 179)
    (429, Token (IF _)) -> Just (Reduce 1 179)
    (429, Token (THEN _)) -> Just (Reduce 1 179)
    (429, Token (ELSE _)) -> Just (Reduce 1 179)
    (429, Token (QVARSYM _)) -> Just (Reduce 1 179)
    (429, Token (BACKQUOTE _)) -> Just (Reduce 1 179)
    (429, Token (QCONSYM _)) -> Just (Reduce 1 179)
    (429, Token (CASE _)) -> Just (Reduce 1 179)
    (429, Token (OF _)) -> Just (Reduce 1 179)
    (429, Token (DO _)) -> Just (Reduce 1 179)
    (429, Token (INTEGER _)) -> Just (Reduce 1 179)
    (430, Token (QCONID _)) -> Just (Shift 425)
    (430, Token (EXPORT _)) -> Just (Shift 426)
    (430, Token (AS _)) -> Just (Shift 427)
    (430, Token (QVARID _)) -> Just (Shift 428)
    (431, Token (WHERE _)) -> Just (Reduce 1 178)
    (431, Token (LBRACE _)) -> Just (Reduce 1 178)
    (431, Token (RBRACE _)) -> Just (Reduce 1 178)
    (431, Token (LPAREN _)) -> Just (Reduce 1 178)
    (431, Token (RPAREN _)) -> Just (Reduce 1 178)
    (431, Token (COMMA _)) -> Just (Reduce 1 178)
    (431, Token (DOT_DOT _)) -> Just (Reduce 1 178)
    (431, Token (SEMICOLON _)) -> Just (Reduce 1 178)
    (431, Token (EQUAL _)) -> Just (Reduce 1 178)
    (431, Token (PIPE _)) -> Just (Reduce 1 178)
    (431, Token (COLON_COLON _)) -> Just (Reduce 1 178)
    (431, Token (MINUS _)) -> Just (Reduce 1 178)
    (431, Token (INFIXL _)) -> Just (Reduce 1 178)
    (431, Token (INFIXR _)) -> Just (Reduce 1 178)
    (431, Token (INFIX _)) -> Just (Reduce 1 178)
    (431, Token (LBRACKET _)) -> Just (Reduce 1 178)
    (431, Token (RBRACKET _)) -> Just (Reduce 1 178)
    (431, Token (QCONID _)) -> Just (Reduce 1 178)
    (431, Token (EXPORT _)) -> Just (Reduce 1 178)
    (431, Token (AS _)) -> Just (Reduce 1 178)
    (431, Token (QVARID _)) -> Just (Reduce 1 178)
    (431, Token (STRING _)) -> Just (Reduce 1 178)
    (431, Token (LARROW _)) -> Just (Reduce 1 178)
    (431, Token (LET _)) -> Just (Reduce 1 178)
    (431, Token (LAMBDA _)) -> Just (Reduce 1 178)
    (431, Token (IF _)) -> Just (Reduce 1 178)
    (431, Token (THEN _)) -> Just (Reduce 1 178)
    (431, Token (ELSE _)) -> Just (Reduce 1 178)
    (431, Token (QVARSYM _)) -> Just (Reduce 1 178)
    (431, Token (BACKQUOTE _)) -> Just (Reduce 1 178)
    (431, Token (QCONSYM _)) -> Just (Reduce 1 178)
    (431, Token (CASE _)) -> Just (Reduce 1 178)
    (431, Token (OF _)) -> Just (Reduce 1 178)
    (431, Token (DO _)) -> Just (Reduce 1 178)
    (431, Token (INTEGER _)) -> Just (Reduce 1 178)
    (432, Token (WHERE _)) -> Just (Reduce 1 177)
    (432, Token (LBRACE _)) -> Just (Reduce 1 177)
    (432, Token (RBRACE _)) -> Just (Reduce 1 177)
    (432, Token (LPAREN _)) -> Just (Reduce 1 177)
    (432, Token (RPAREN _)) -> Just (Reduce 1 177)
    (432, Token (COMMA _)) -> Just (Reduce 1 177)
    (432, Token (DOT_DOT _)) -> Just (Reduce 1 177)
    (432, Token (SEMICOLON _)) -> Just (Reduce 1 177)
    (432, Token (EQUAL _)) -> Just (Reduce 1 177)
    (432, Token (PIPE _)) -> Just (Reduce 1 177)
    (432, Token (COLON_COLON _)) -> Just (Reduce 1 177)
    (432, Token (MINUS _)) -> Just (Reduce 1 177)
    (432, Token (INFIXL _)) -> Just (Reduce 1 177)
    (432, Token (INFIXR _)) -> Just (Reduce 1 177)
    (432, Token (INFIX _)) -> Just (Reduce 1 177)
    (432, Token (LBRACKET _)) -> Just (Reduce 1 177)
    (432, Token (RBRACKET _)) -> Just (Reduce 1 177)
    (432, Token (QCONID _)) -> Just (Reduce 1 177)
    (432, Token (EXPORT _)) -> Just (Reduce 1 177)
    (432, Token (AS _)) -> Just (Reduce 1 177)
    (432, Token (QVARID _)) -> Just (Reduce 1 177)
    (432, Token (STRING _)) -> Just (Reduce 1 177)
    (432, Token (LARROW _)) -> Just (Reduce 1 177)
    (432, Token (LET _)) -> Just (Reduce 1 177)
    (432, Token (LAMBDA _)) -> Just (Reduce 1 177)
    (432, Token (IF _)) -> Just (Reduce 1 177)
    (432, Token (THEN _)) -> Just (Reduce 1 177)
    (432, Token (ELSE _)) -> Just (Reduce 1 177)
    (432, Token (QVARSYM _)) -> Just (Reduce 1 177)
    (432, Token (BACKQUOTE _)) -> Just (Reduce 1 177)
    (432, Token (QCONSYM _)) -> Just (Reduce 1 177)
    (432, Token (CASE _)) -> Just (Reduce 1 177)
    (432, Token (OF _)) -> Just (Reduce 1 177)
    (432, Token (DO _)) -> Just (Reduce 1 177)
    (432, Token (INTEGER _)) -> Just (Reduce 1 177)
    (433, Token (RPAREN _)) -> Just (Shift 412)
    (433, Token (COMMA _)) -> Just (Shift 57)
    (434, Token (COMMA _)) -> Just (Shift 65)
    (434, Token (DOT_DOT _)) -> Just (Shift 66)
    (434, Token (RBRACKET _)) -> Just (Reduce 1 173)
    (435, Token (RBRACKET _)) -> Just (Shift 422)
    (436, Token (RBRACKET _)) -> Just (Shift 423)
    (437, Token (COMMA _)) -> Just (Shift 63)
    (437, Token (DOT_DOT _)) -> Just (Shift 67)
    (437, Token (RBRACKET _)) -> Just (Reduce 1 173)
    (438, Token (RPAREN _)) -> Just (Shift 413)
    (439, Token (RPAREN _)) -> Just (Shift 414)
    (440, Token (RPAREN _)) -> Just (Shift 415)
    (441, Token (RPAREN _)) -> Just (Shift 416)
    (442, Token (RPAREN _)) -> Just (Shift 417)
    (443, Token (RPAREN _)) -> Just (Shift 418)
    (444, Token (RBRACKET _)) -> Just (Shift 424)
    (445, Token (RPAREN _)) -> Just (Shift 419)
    (446, Token (COMMA _)) -> Just (Shift 65)
    (446, Token (RBRACKET _)) -> Just (Reduce 1 173)
    (447, Token (RBRACKET _)) -> Just (Reduce 3 174)
    (448, Token (RPAREN _)) -> Just (Reduce 3 175)
    (448, Token (COMMA _)) -> Just (Shift 57)
    (449, Token (RPAREN _)) -> Just (Reduce 3 176)
    (450, Token (RBRACE _)) -> Just (Reduce 5 197)
    (450, Token (SEMICOLON _)) -> Just (Reduce 5 197)
    (451, Token (WHERE _)) -> Just (Shift 269)
    (451, Token (RBRACE _)) -> Just (Reduce 3 196)
    (451, Token (SEMICOLON _)) -> Just (Reduce 3 196)
    (452, Token (RBRACE _)) -> Just (Reduce 1 208)
    (452, Token (SEMICOLON _)) -> Just (Reduce 1 208)
    (452, Token (LARROW _)) -> Just (Shift 53)
    (453, Token (RBRACE _)) -> Just (Reduce 3 209)
    (453, Token (SEMICOLON _)) -> Just (Reduce 3 209)
    (454, Token (LPAREN _)) -> Just (Reduce 3 216)
    (454, Token (RPAREN _)) -> Just (Reduce 3 216)
    (454, Token (EQUAL _)) -> Just (Reduce 3 216)
    (454, Token (PIPE _)) -> Just (Reduce 3 216)
    (454, Token (MINUS _)) -> Just (Reduce 3 216)
    (454, Token (RARROW _)) -> Just (Reduce 3 216)
    (454, Token (QCONID _)) -> Just (Reduce 3 216)
    (454, Token (EXPORT _)) -> Just (Reduce 3 216)
    (454, Token (AS _)) -> Just (Reduce 3 216)
    (454, Token (QVARID _)) -> Just (Reduce 3 216)
    (454, Token (QVARSYM _)) -> Just (Reduce 3 216)
    (454, Token (BACKQUOTE _)) -> Just (Reduce 3 216)
    (454, Token (QCONSYM _)) -> Just (Reduce 3 216)
    (455, Token (LPAREN _)) -> Just (Reduce 1 215)
    (455, Token (RPAREN _)) -> Just (Reduce 1 215)
    (455, Token (EQUAL _)) -> Just (Reduce 1 215)
    (455, Token (PIPE _)) -> Just (Reduce 1 215)
    (455, Token (MINUS _)) -> Just (Reduce 1 215)
    (455, Token (RARROW _)) -> Just (Reduce 1 215)
    (455, Token (QCONID _)) -> Just (Reduce 1 215)
    (455, Token (EXPORT _)) -> Just (Reduce 1 215)
    (455, Token (AS _)) -> Just (Reduce 1 215)
    (455, Token (QVARID _)) -> Just (Reduce 1 215)
    (455, Token (QVARSYM _)) -> Just (Reduce 1 215)
    (455, Token (BACKQUOTE _)) -> Just (Reduce 1 215)
    (455, Token (QCONSYM _)) -> Just (Reduce 1 215)
    (456, Token (BACKQUOTE _)) -> Just (Shift 460)
    (457, Token (BACKQUOTE _)) -> Just (Shift 461)
    (458, Token (BACKQUOTE _)) -> Just (Shift 462)
    (459, Token (RBRACE _)) -> Just (Reduce 1 224)
    (459, Token (LPAREN _)) -> Just (Reduce 1 224)
    (459, Token (COMMA _)) -> Just (Reduce 1 224)
    (459, Token (SEMICOLON _)) -> Just (Reduce 1 224)
    (459, Token (MINUS _)) -> Just (Reduce 1 224)
    (459, Token (QCONID _)) -> Just (Reduce 1 224)
    (459, Token (EXPORT _)) -> Just (Reduce 1 224)
    (459, Token (AS _)) -> Just (Reduce 1 224)
    (459, Token (QVARID _)) -> Just (Reduce 1 224)
    (459, Token (QVARSYM _)) -> Just (Reduce 1 224)
    (459, Token (BACKQUOTE _)) -> Just (Reduce 1 224)
    (459, Token (QCONSYM _)) -> Just (Reduce 1 224)
    (460, Token (RBRACE _)) -> Just (Reduce 3 226)
    (460, Token (LPAREN _)) -> Just (Reduce 3 226)
    (460, Token (COMMA _)) -> Just (Reduce 3 226)
    (460, Token (SEMICOLON _)) -> Just (Reduce 3 226)
    (460, Token (MINUS _)) -> Just (Reduce 3 226)
    (460, Token (QCONID _)) -> Just (Reduce 3 226)
    (460, Token (EXPORT _)) -> Just (Reduce 3 226)
    (460, Token (AS _)) -> Just (Reduce 3 226)
    (460, Token (QVARID _)) -> Just (Reduce 3 226)
    (460, Token (QVARSYM _)) -> Just (Reduce 3 226)
    (460, Token (BACKQUOTE _)) -> Just (Reduce 3 226)
    (460, Token (QCONSYM _)) -> Just (Reduce 3 226)
    (461, Token (RBRACE _)) -> Just (Reduce 3 225)
    (461, Token (LPAREN _)) -> Just (Reduce 3 225)
    (461, Token (COMMA _)) -> Just (Reduce 3 225)
    (461, Token (SEMICOLON _)) -> Just (Reduce 3 225)
    (461, Token (MINUS _)) -> Just (Reduce 3 225)
    (461, Token (QCONID _)) -> Just (Reduce 3 225)
    (461, Token (EXPORT _)) -> Just (Reduce 3 225)
    (461, Token (AS _)) -> Just (Reduce 3 225)
    (461, Token (QVARID _)) -> Just (Reduce 3 225)
    (461, Token (QVARSYM _)) -> Just (Reduce 3 225)
    (461, Token (BACKQUOTE _)) -> Just (Reduce 3 225)
    (461, Token (QCONSYM _)) -> Just (Reduce 3 225)
    (462, Token (RBRACE _)) -> Just (Reduce 3 227)
    (462, Token (LPAREN _)) -> Just (Reduce 3 227)
    (462, Token (COMMA _)) -> Just (Reduce 3 227)
    (462, Token (SEMICOLON _)) -> Just (Reduce 3 227)
    (462, Token (MINUS _)) -> Just (Reduce 3 227)
    (462, Token (QCONID _)) -> Just (Reduce 3 227)
    (462, Token (EXPORT _)) -> Just (Reduce 3 227)
    (462, Token (AS _)) -> Just (Reduce 3 227)
    (462, Token (QVARID _)) -> Just (Reduce 3 227)
    (462, Token (QVARSYM _)) -> Just (Reduce 3 227)
    (462, Token (BACKQUOTE _)) -> Just (Reduce 3 227)
    (462, Token (QCONSYM _)) -> Just (Reduce 3 227)
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
production 167 = 63
production 168 = 63
production 169 = 63
production 170 = 63
production 171 = 66
production 172 = 66
production 173 = 68
production 174 = 68
production 175 = 69
production 176 = 69
production 177 = 67
production 178 = 67
production 179 = 67
production 180 = 67
production 181 = 67
production 182 = 67
production 183 = 67
production 184 = 67
production 185 = 67
production 186 = 67
production 187 = 67
production 188 = 67
production 189 = 67
production 190 = 67
production 191 = 67
production 192 = 67
production 193 = 64
production 194 = 64
production 195 = 70
production 196 = 70
production 197 = 70
production 198 = 71
production 199 = 71
production 200 = 72
production 201 = 72
production 202 = 73
production 203 = 73
production 204 = 73
production 205 = 65
production 206 = 65
production 207 = 74
production 208 = 74
production 209 = 74
production 210 = 74
production 211 = 31
production 212 = 31
production 213 = 31
production 214 = 31
production 215 = 75
production 216 = 75
production 217 = 8
production 218 = 8
production 219 = 8
production 220 = 8
production 221 = 8
production 222 = 9
production 223 = 9
production 224 = 76
production 225 = 76
production 226 = 76
production 227 = 76
production 228 = 52
production 229 = 52
production 230 = 44
production 231 = 44
production 232 = 16
production 233 = 16
production 234 = 15
production 235 = 15
production 236 = 47
production 237 = 47
production 238 = 47
production 239 = 77
production 240 = 1
production 241 = 42
production 242 = 42
production 243 = 62
production 244 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 202
    (9, 1) -> Just 227
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 308
    (13, 14) -> Just 18
    (13, 27) -> Just 225
    (13, 30) -> Just 261
    (13, 31) -> Just 85
    (13, 40) -> Just 278
    (13, 41) -> Just 279
    (13, 75) -> Just 282
    (16, 4) -> Just 17
    (16, 8) -> Just 308
    (16, 14) -> Just 18
    (16, 27) -> Just 225
    (16, 30) -> Just 261
    (16, 31) -> Just 85
    (16, 40) -> Just 278
    (16, 41) -> Just 279
    (16, 75) -> Just 282
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 176
    (25, 9) -> Just 177
    (25, 10) -> Just 33
    (25, 13) -> Just 166
    (34, 8) -> Just 432
    (34, 32) -> Just 382
    (34, 61) -> Just 286
    (34, 63) -> Just 396
    (34, 66) -> Just 45
    (34, 67) -> Just 410
    (35, 8) -> Just 432
    (35, 32) -> Just 383
    (35, 61) -> Just 286
    (35, 63) -> Just 396
    (35, 66) -> Just 45
    (35, 67) -> Just 410
    (36, 8) -> Just 432
    (36, 32) -> Just 384
    (36, 61) -> Just 286
    (36, 63) -> Just 396
    (36, 66) -> Just 45
    (36, 67) -> Just 410
    (37, 8) -> Just 432
    (37, 32) -> Just 387
    (37, 61) -> Just 286
    (37, 63) -> Just 396
    (37, 66) -> Just 45
    (37, 67) -> Just 410
    (38, 8) -> Just 432
    (38, 32) -> Just 388
    (38, 61) -> Just 286
    (38, 63) -> Just 396
    (38, 66) -> Just 45
    (38, 67) -> Just 410
    (39, 8) -> Just 432
    (39, 32) -> Just 389
    (39, 61) -> Just 286
    (39, 63) -> Just 396
    (39, 66) -> Just 45
    (39, 67) -> Just 410
    (40, 8) -> Just 432
    (40, 32) -> Just 390
    (40, 61) -> Just 286
    (40, 63) -> Just 396
    (40, 66) -> Just 45
    (40, 67) -> Just 410
    (41, 8) -> Just 432
    (41, 32) -> Just 391
    (41, 61) -> Just 286
    (41, 63) -> Just 396
    (41, 66) -> Just 45
    (41, 67) -> Just 410
    (42, 8) -> Just 432
    (42, 32) -> Just 392
    (42, 61) -> Just 286
    (42, 63) -> Just 396
    (42, 66) -> Just 45
    (42, 67) -> Just 410
    (43, 8) -> Just 432
    (43, 32) -> Just 393
    (43, 61) -> Just 286
    (43, 63) -> Just 396
    (43, 66) -> Just 45
    (43, 67) -> Just 410
    (44, 8) -> Just 432
    (44, 63) -> Just 403
    (44, 66) -> Just 45
    (44, 67) -> Just 410
    (45, 8) -> Just 432
    (45, 67) -> Just 411
    (46, 8) -> Just 432
    (46, 32) -> Just 262
    (46, 61) -> Just 286
    (46, 63) -> Just 396
    (46, 66) -> Just 45
    (46, 67) -> Just 410
    (47, 8) -> Just 432
    (47, 32) -> Just 287
    (47, 61) -> Just 286
    (47, 63) -> Just 396
    (47, 66) -> Just 45
    (47, 67) -> Just 410
    (48, 8) -> Just 432
    (48, 32) -> Just 297
    (48, 61) -> Just 286
    (48, 63) -> Just 396
    (48, 66) -> Just 45
    (48, 67) -> Just 410
    (49, 8) -> Just 432
    (49, 32) -> Just 305
    (49, 61) -> Just 286
    (49, 63) -> Just 396
    (49, 66) -> Just 45
    (49, 67) -> Just 410
    (50, 8) -> Just 432
    (50, 32) -> Just 451
    (50, 61) -> Just 286
    (50, 63) -> Just 396
    (50, 66) -> Just 45
    (50, 67) -> Just 410
    (51, 8) -> Just 432
    (51, 61) -> Just 452
    (51, 63) -> Just 396
    (51, 65) -> Just 405
    (51, 66) -> Just 45
    (51, 67) -> Just 410
    (51, 74) -> Just 409
    (52, 8) -> Just 432
    (52, 61) -> Just 452
    (52, 63) -> Just 396
    (52, 65) -> Just 408
    (52, 66) -> Just 45
    (52, 67) -> Just 410
    (52, 74) -> Just 409
    (53, 8) -> Just 432
    (53, 61) -> Just 453
    (53, 63) -> Just 396
    (53, 66) -> Just 45
    (53, 67) -> Just 410
    (54, 8) -> Just 432
    (54, 32) -> Just 433
    (54, 61) -> Just 286
    (54, 63) -> Just 396
    (54, 66) -> Just 45
    (54, 67) -> Just 410
    (54, 69) -> Just 445
    (55, 8) -> Just 432
    (55, 63) -> Just 403
    (55, 66) -> Just 45
    (55, 67) -> Just 410
    (56, 8) -> Just 432
    (56, 61) -> Just 438
    (56, 63) -> Just 396
    (56, 66) -> Just 45
    (56, 67) -> Just 410
    (57, 8) -> Just 432
    (57, 32) -> Just 448
    (57, 61) -> Just 286
    (57, 63) -> Just 396
    (57, 66) -> Just 45
    (57, 67) -> Just 410
    (57, 69) -> Just 449
    (58, 8) -> Just 432
    (58, 61) -> Just 439
    (58, 63) -> Just 396
    (58, 66) -> Just 45
    (58, 67) -> Just 410
    (59, 8) -> Just 432
    (59, 61) -> Just 440
    (59, 63) -> Just 396
    (59, 66) -> Just 45
    (59, 67) -> Just 410
    (60, 8) -> Just 432
    (60, 61) -> Just 441
    (60, 63) -> Just 396
    (60, 66) -> Just 45
    (60, 67) -> Just 410
    (61, 8) -> Just 432
    (61, 61) -> Just 442
    (61, 63) -> Just 396
    (61, 66) -> Just 45
    (61, 67) -> Just 410
    (62, 8) -> Just 432
    (62, 61) -> Just 443
    (62, 63) -> Just 396
    (62, 66) -> Just 45
    (62, 67) -> Just 410
    (63, 8) -> Just 432
    (63, 32) -> Just 434
    (63, 61) -> Just 286
    (63, 63) -> Just 396
    (63, 66) -> Just 45
    (63, 67) -> Just 410
    (63, 68) -> Just 447
    (64, 8) -> Just 432
    (64, 32) -> Just 437
    (64, 61) -> Just 286
    (64, 63) -> Just 396
    (64, 66) -> Just 45
    (64, 67) -> Just 410
    (64, 68) -> Just 444
    (65, 8) -> Just 432
    (65, 32) -> Just 446
    (65, 61) -> Just 286
    (65, 63) -> Just 396
    (65, 66) -> Just 45
    (65, 67) -> Just 410
    (65, 68) -> Just 447
    (66, 8) -> Just 432
    (66, 32) -> Just 435
    (66, 61) -> Just 286
    (66, 63) -> Just 396
    (66, 66) -> Just 45
    (66, 67) -> Just 410
    (67, 8) -> Just 432
    (67, 32) -> Just 436
    (67, 61) -> Just 286
    (67, 63) -> Just 396
    (67, 66) -> Just 45
    (67, 67) -> Just 410
    (68, 8) -> Just 432
    (68, 33) -> Just 263
    (68, 59) -> Just 289
    (68, 60) -> Just 369
    (68, 61) -> Just 372
    (68, 63) -> Just 396
    (68, 66) -> Just 45
    (68, 67) -> Just 410
    (69, 8) -> Just 432
    (69, 33) -> Just 288
    (69, 59) -> Just 289
    (69, 60) -> Just 369
    (69, 61) -> Just 372
    (69, 63) -> Just 396
    (69, 66) -> Just 45
    (69, 67) -> Just 410
    (70, 8) -> Just 432
    (70, 33) -> Just 298
    (70, 59) -> Just 289
    (70, 60) -> Just 369
    (70, 61) -> Just 372
    (70, 63) -> Just 396
    (70, 66) -> Just 45
    (70, 67) -> Just 410
    (71, 8) -> Just 432
    (71, 33) -> Just 306
    (71, 59) -> Just 289
    (71, 60) -> Just 369
    (71, 61) -> Just 372
    (71, 63) -> Just 396
    (71, 66) -> Just 45
    (71, 67) -> Just 410
    (72, 8) -> Just 432
    (72, 59) -> Just 368
    (72, 60) -> Just 369
    (72, 61) -> Just 372
    (72, 63) -> Just 396
    (72, 66) -> Just 45
    (72, 67) -> Just 410
    (73, 8) -> Just 432
    (73, 32) -> Just 385
    (73, 61) -> Just 286
    (73, 63) -> Just 396
    (73, 66) -> Just 45
    (73, 67) -> Just 410
    (74, 8) -> Just 432
    (74, 32) -> Just 386
    (74, 61) -> Just 286
    (74, 63) -> Just 396
    (74, 66) -> Just 45
    (74, 67) -> Just 410
    (75, 8) -> Just 432
    (75, 32) -> Just 402
    (75, 61) -> Just 286
    (75, 63) -> Just 396
    (75, 66) -> Just 45
    (75, 67) -> Just 410
    (76, 8) -> Just 432
    (76, 32) -> Just 371
    (76, 61) -> Just 286
    (76, 63) -> Just 396
    (76, 66) -> Just 45
    (76, 67) -> Just 410
    (77, 8) -> Just 455
    (77, 75) -> Just 283
    (78, 8) -> Just 455
    (78, 75) -> Just 285
    (79, 8) -> Just 455
    (79, 31) -> Just 80
    (79, 75) -> Just 282
    (80, 8) -> Just 455
    (80, 44) -> Just 78
    (80, 52) -> Just 320
    (80, 75) -> Just 284
    (80, 76) -> Just 321
    (81, 8) -> Just 308
    (81, 27) -> Just 274
    (81, 29) -> Just 273
    (81, 30) -> Just 261
    (81, 31) -> Just 85
    (81, 40) -> Just 278
    (81, 41) -> Just 279
    (81, 75) -> Just 282
    (82, 8) -> Just 308
    (82, 27) -> Just 274
    (82, 29) -> Just 275
    (82, 30) -> Just 261
    (82, 31) -> Just 85
    (82, 40) -> Just 278
    (82, 41) -> Just 279
    (82, 75) -> Just 282
    (83, 8) -> Just 308
    (83, 30) -> Just 296
    (83, 31) -> Just 88
    (83, 35) -> Just 291
    (83, 36) -> Just 293
    (83, 40) -> Just 278
    (83, 41) -> Just 279
    (83, 75) -> Just 282
    (84, 8) -> Just 308
    (84, 30) -> Just 296
    (84, 31) -> Just 88
    (84, 35) -> Just 292
    (84, 36) -> Just 293
    (84, 40) -> Just 278
    (84, 41) -> Just 279
    (84, 75) -> Just 282
    (85, 8) -> Just 455
    (85, 44) -> Just 78
    (85, 52) -> Just 320
    (85, 75) -> Just 284
    (85, 76) -> Just 321
    (86, 8) -> Just 455
    (86, 31) -> Just 89
    (86, 38) -> Just 300
    (86, 39) -> Just 302
    (86, 75) -> Just 282
    (87, 8) -> Just 455
    (87, 31) -> Just 89
    (87, 38) -> Just 301
    (87, 39) -> Just 302
    (87, 75) -> Just 282
    (88, 8) -> Just 455
    (88, 44) -> Just 78
    (88, 52) -> Just 320
    (88, 75) -> Just 284
    (88, 76) -> Just 321
    (89, 8) -> Just 455
    (89, 44) -> Just 78
    (89, 52) -> Just 320
    (89, 75) -> Just 284
    (89, 76) -> Just 321
    (90, 8) -> Just 455
    (90, 31) -> Just 93
    (90, 75) -> Just 282
    (91, 8) -> Just 455
    (91, 31) -> Just 94
    (91, 64) -> Just 404
    (91, 70) -> Just 407
    (91, 75) -> Just 282
    (92, 8) -> Just 455
    (92, 31) -> Just 94
    (92, 64) -> Just 406
    (92, 70) -> Just 407
    (92, 75) -> Just 282
    (93, 8) -> Just 455
    (93, 44) -> Just 78
    (93, 52) -> Just 320
    (93, 75) -> Just 284
    (93, 76) -> Just 321
    (94, 8) -> Just 455
    (94, 44) -> Just 78
    (94, 52) -> Just 320
    (94, 75) -> Just 284
    (94, 76) -> Just 321
    (95, 8) -> Just 173
    (95, 9) -> Just 174
    (95, 11) -> Just 167
    (95, 12) -> Just 168
    (96, 8) -> Just 173
    (96, 9) -> Just 174
    (96, 11) -> Just 203
    (96, 12) -> Just 168
    (97, 8) -> Just 173
    (97, 9) -> Just 174
    (97, 11) -> Just 204
    (97, 12) -> Just 168
    (98, 8) -> Just 176
    (98, 9) -> Just 177
    (98, 10) -> Just 165
    (98, 13) -> Just 166
    (99, 8) -> Just 176
    (99, 9) -> Just 177
    (99, 10) -> Just 175
    (99, 13) -> Just 166
    (100, 8) -> Just 307
    (100, 40) -> Just 309
    (101, 8) -> Just 307
    (101, 40) -> Just 359
    (101, 53) -> Just 350
    (101, 54) -> Just 357
    (102, 8) -> Just 307
    (102, 40) -> Just 359
    (102, 53) -> Just 356
    (102, 54) -> Just 357
    (103, 8) -> Just 237
    (104, 8) -> Just 248
    (105, 8) -> Just 249
    (106, 8) -> Just 250
    (116, 9) -> Just 336
    (116, 45) -> Just 327
    (116, 46) -> Just 328
    (116, 47) -> Just 329
    (117, 9) -> Just 336
    (117, 17) -> Just 118
    (117, 45) -> Just 228
    (117, 46) -> Just 328
    (117, 47) -> Just 329
    (118, 9) -> Just 336
    (118, 23) -> Just 220
    (118, 45) -> Just 229
    (118, 46) -> Just 328
    (118, 47) -> Just 329
    (119, 9) -> Just 336
    (119, 17) -> Just 120
    (119, 45) -> Just 228
    (119, 46) -> Just 328
    (119, 47) -> Just 329
    (120, 9) -> Just 336
    (120, 24) -> Just 222
    (120, 45) -> Just 229
    (120, 46) -> Just 328
    (120, 47) -> Just 329
    (121, 9) -> Just 336
    (121, 17) -> Just 122
    (121, 45) -> Just 228
    (121, 46) -> Just 328
    (121, 47) -> Just 329
    (122, 9) -> Just 336
    (122, 23) -> Just 219
    (122, 45) -> Just 229
    (122, 46) -> Just 328
    (122, 47) -> Just 329
    (123, 9) -> Just 336
    (123, 17) -> Just 124
    (123, 45) -> Just 228
    (123, 46) -> Just 328
    (123, 47) -> Just 329
    (124, 9) -> Just 336
    (124, 24) -> Just 221
    (124, 45) -> Just 229
    (124, 46) -> Just 328
    (124, 47) -> Just 329
    (125, 9) -> Just 336
    (125, 17) -> Just 126
    (125, 18) -> Just 379
    (125, 45) -> Just 228
    (125, 46) -> Just 328
    (125, 47) -> Just 329
    (126, 9) -> Just 336
    (126, 45) -> Just 229
    (126, 46) -> Just 328
    (126, 47) -> Just 329
    (127, 9) -> Just 336
    (127, 17) -> Just 129
    (127, 18) -> Just 230
    (127, 45) -> Just 228
    (127, 46) -> Just 328
    (127, 47) -> Just 329
    (128, 9) -> Just 336
    (128, 17) -> Just 129
    (128, 18) -> Just 378
    (128, 45) -> Just 228
    (128, 46) -> Just 328
    (128, 47) -> Just 329
    (129, 9) -> Just 336
    (129, 45) -> Just 229
    (129, 46) -> Just 328
    (129, 47) -> Just 329
    (130, 9) -> Just 336
    (130, 17) -> Just 131
    (130, 45) -> Just 228
    (130, 46) -> Just 328
    (130, 47) -> Just 329
    (131, 9) -> Just 336
    (131, 19) -> Just 207
    (131, 45) -> Just 229
    (131, 46) -> Just 328
    (131, 47) -> Just 329
    (132, 9) -> Just 336
    (132, 17) -> Just 133
    (132, 45) -> Just 228
    (132, 46) -> Just 328
    (132, 47) -> Just 329
    (133, 9) -> Just 336
    (133, 19) -> Just 208
    (133, 45) -> Just 229
    (133, 46) -> Just 328
    (133, 47) -> Just 329
    (134, 9) -> Just 337
    (134, 17) -> Just 137
    (134, 45) -> Just 228
    (134, 46) -> Just 328
    (134, 47) -> Just 329
    (134, 50) -> Just 231
    (134, 51) -> Just 347
    (135, 9) -> Just 337
    (135, 17) -> Just 137
    (135, 45) -> Just 228
    (135, 46) -> Just 328
    (135, 47) -> Just 329
    (135, 50) -> Just 346
    (135, 51) -> Just 347
    (136, 9) -> Just 149
    (137, 9) -> Just 336
    (137, 45) -> Just 229
    (137, 46) -> Just 328
    (137, 47) -> Just 329
    (137, 52) -> Just 138
    (138, 9) -> Just 336
    (138, 17) -> Just 139
    (138, 45) -> Just 228
    (138, 46) -> Just 328
    (138, 47) -> Just 329
    (139, 9) -> Just 336
    (139, 45) -> Just 229
    (139, 46) -> Just 328
    (139, 47) -> Just 329
    (140, 9) -> Just 336
    (140, 17) -> Just 141
    (140, 18) -> Just 277
    (140, 45) -> Just 228
    (140, 46) -> Just 328
    (140, 47) -> Just 329
    (141, 9) -> Just 336
    (141, 45) -> Just 229
    (141, 46) -> Just 328
    (141, 47) -> Just 329
    (142, 9) -> Just 336
    (142, 17) -> Just 129
    (142, 18) -> Just 206
    (142, 45) -> Just 228
    (142, 46) -> Just 328
    (142, 47) -> Just 329
    (143, 9) -> Just 336
    (143, 17) -> Just 129
    (143, 18) -> Just 251
    (143, 45) -> Just 228
    (143, 46) -> Just 328
    (143, 47) -> Just 329
    (144, 9) -> Just 336
    (144, 17) -> Just 129
    (144, 18) -> Just 252
    (144, 45) -> Just 228
    (144, 46) -> Just 328
    (144, 47) -> Just 329
    (145, 9) -> Just 336
    (145, 17) -> Just 129
    (145, 18) -> Just 253
    (145, 45) -> Just 228
    (145, 46) -> Just 328
    (145, 47) -> Just 329
    (146, 9) -> Just 336
    (146, 17) -> Just 129
    (146, 18) -> Just 276
    (146, 45) -> Just 228
    (146, 46) -> Just 328
    (146, 47) -> Just 329
    (147, 9) -> Just 336
    (147, 17) -> Just 129
    (147, 18) -> Just 358
    (147, 45) -> Just 228
    (147, 46) -> Just 328
    (147, 47) -> Just 329
    (148, 9) -> Just 336
    (148, 17) -> Just 129
    (148, 18) -> Just 238
    (148, 45) -> Just 228
    (148, 46) -> Just 328
    (148, 47) -> Just 329
    (149, 9) -> Just 336
    (149, 45) -> Just 239
    (149, 46) -> Just 328
    (149, 47) -> Just 329
    (150, 9) -> Just 336
    (150, 17) -> Just 151
    (150, 45) -> Just 228
    (150, 46) -> Just 328
    (150, 47) -> Just 329
    (151, 9) -> Just 336
    (151, 22) -> Just 218
    (151, 45) -> Just 229
    (151, 46) -> Just 328
    (151, 47) -> Just 329
    (152, 9) -> Just 336
    (152, 17) -> Just 154
    (152, 45) -> Just 228
    (152, 46) -> Just 328
    (152, 47) -> Just 329
    (153, 9) -> Just 336
    (153, 17) -> Just 155
    (153, 45) -> Just 228
    (153, 46) -> Just 328
    (153, 47) -> Just 329
    (154, 9) -> Just 336
    (154, 45) -> Just 229
    (154, 46) -> Just 328
    (154, 47) -> Just 329
    (155, 9) -> Just 336
    (155, 22) -> Just 217
    (155, 45) -> Just 229
    (155, 46) -> Just 328
    (155, 47) -> Just 329
    (156, 9) -> Just 336
    (156, 17) -> Just 129
    (156, 18) -> Just 325
    (156, 45) -> Just 228
    (156, 46) -> Just 328
    (156, 47) -> Just 329
    (156, 48) -> Just 330
    (156, 49) -> Just 338
    (157, 9) -> Just 336
    (157, 17) -> Just 129
    (157, 18) -> Just 244
    (157, 25) -> Just 223
    (157, 45) -> Just 228
    (157, 46) -> Just 328
    (157, 47) -> Just 329
    (158, 9) -> Just 336
    (158, 17) -> Just 129
    (158, 18) -> Just 244
    (158, 25) -> Just 245
    (158, 45) -> Just 228
    (158, 46) -> Just 328
    (158, 47) -> Just 329
    (159, 9) -> Just 336
    (159, 17) -> Just 129
    (159, 18) -> Just 342
    (159, 45) -> Just 228
    (159, 46) -> Just 328
    (159, 47) -> Just 329
    (159, 48) -> Just 343
    (160, 9) -> Just 336
    (160, 17) -> Just 129
    (160, 18) -> Just 326
    (160, 45) -> Just 228
    (160, 46) -> Just 328
    (160, 47) -> Just 329
    (178, 20) -> Just 234
    (178, 21) -> Just 213
    (179, 20) -> Just 234
    (179, 21) -> Just 214
    (180, 20) -> Just 234
    (180, 21) -> Just 215
    (181, 20) -> Just 234
    (181, 21) -> Just 216
    (194, 15) -> Just 8
    (196, 20) -> Just 209
    (197, 20) -> Just 210
    (198, 20) -> Just 211
    (199, 20) -> Just 212
    (201, 26) -> Just 224
    (202, 16) -> Just 205
    (232, 20) -> Just 234
    (232, 21) -> Just 235
    (240, 34) -> Just 241
    (242, 37) -> Just 243
    (246, 55) -> Just 254
    (247, 55) -> Just 255
    (254, 56) -> Just 104
    (254, 57) -> Just 256
    (255, 58) -> Just 106
    (256, 56) -> Just 105
    (257, 28) -> Just 259
    (258, 28) -> Just 260
    (264, 28) -> Just 381
    (265, 28) -> Just 294
    (266, 28) -> Just 295
    (267, 28) -> Just 303
    (268, 28) -> Just 304
    (269, 28) -> Just 450
    (270, 28) -> Just 370
    (271, 28) -> Just 380
    (279, 42) -> Just 280
    (280, 43) -> Just 281
    (280, 44) -> Just 319
    (280, 52) -> Just 320
    (280, 76) -> Just 321
    (314, 43) -> Just 317
    (314, 44) -> Just 319
    (314, 52) -> Just 320
    (314, 76) -> Just 321
    (315, 43) -> Just 318
    (315, 44) -> Just 319
    (315, 52) -> Just 320
    (315, 76) -> Just 321
    (344, 49) -> Just 345
    (385, 62) -> Just 394
    (386, 62) -> Just 395
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
                      Monad.liftM StackValue_lexp $ lexp_implies_MINUS_lexp actions (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_CASE_exp_OF_LBRACE_alts_RBRACE actions (case snd (pop !! 5) of { StackValue_CASE value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_OF value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_alts value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_DO_LBRACE_stmts_RBRACE actions (case snd (pop !! 3) of { StackValue_DO value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_stmts value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_exp_seq $ exp_seq_implies_exp actions (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_exp_seq $ exp_seq_implies_exp_COMMA_exp_seq actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp_seq value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_exp_seq2 $ exp_seq2_implies_exp_COMMA_exp actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_exp_seq2 $ exp_seq2_implies_exp_COMMA_exp_seq2 actions (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp_seq2 value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_seq_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_DOT_DOT_RBRACKET actions (case snd (pop !! 3) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_DOT_DOT_exp_RBRACKET actions (case snd (pop !! 4) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_RBRACKET actions (case snd (pop !! 5) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LBRACKET_exp_COMMA_exp_DOT_DOT_exp_RBRACKET actions (case snd (pop !! 6) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt actions (case snd (pop !! 0) of { StackValue_alt value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt_SEMICOLON_alts actions (case snd (pop !! 2) of { StackValue_alt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_alts value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_alt $ alt_implies actions
                    196 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    198 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp actions (case snd (pop !! 2) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp_PIPE_gdpat actions (case snd (pop !! 4) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdpat value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard actions (case snd (pop !! 0) of { StackValue_patguard value -> value; _ -> undefined })
                    201 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard_COMMA_patguards actions (case snd (pop !! 2) of { StackValue_patguard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_patguards value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    203 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    204 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    205 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt actions (case snd (pop !! 0) of { StackValue_stmt value -> value; _ -> undefined })
                    206 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt_SEMICOLON_stmts actions (case snd (pop !! 2) of { StackValue_stmt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_stmts value -> value; _ -> undefined })
                    207 ->
                      Monad.liftM StackValue_stmt $ stmt_implies actions
                    208 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    209 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    210 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    211 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    212 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    213 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    214 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    215 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    216 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    217 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    218 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    219 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    220 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    221 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    222 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    223 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    224 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    225 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    226 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    227 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    228 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    229 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    230 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    231 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    232 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    233 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    234 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    235 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    236 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    237 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    238 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    239 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    240 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    241 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    242 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    243 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    244 ->
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
  , gdpat_implies_patguards_RARROW_exp = \patguards0 rARROW1 exp2 ->
      return $ Gdpat_implies_patguards_RARROW_exp patguards0 rARROW1 exp2
  , gdpat_implies_patguards_RARROW_exp_PIPE_gdpat = \patguards0 rARROW1 exp2 pIPE3 gdpat4 ->
      return $ Gdpat_implies_patguards_RARROW_exp_PIPE_gdpat patguards0 rARROW1 exp2 pIPE3 gdpat4
  , patguards_implies_patguard = \patguard0 ->
      return $ Patguards_implies_patguard patguard0
  , patguards_implies_patguard_COMMA_patguards = \patguard0 cOMMA1 patguards2 ->
      return $ Patguards_implies_patguard_COMMA_patguards patguard0 cOMMA1 patguards2
  , patguard_implies_infixexp_LARROW_infixexp = \infixexp0 lARROW1 infixexp2 ->
      return $ Patguard_implies_infixexp_LARROW_infixexp infixexp0 lARROW1 infixexp2
  , patguard_implies_LET_decls = \lET0 decls1 ->
      return $ Patguard_implies_LET_decls lET0 decls1
  , patguard_implies_infixexp = \infixexp0 ->
      return $ Patguard_implies_infixexp infixexp0
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

