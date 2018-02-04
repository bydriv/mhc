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
  | Aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN LPAREN QVARSYM Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE AS BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE EXPORT BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE QVARID BACKQUOTE Infixexp RPAREN
  | Aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN LPAREN QCONSYM Infixexp RPAREN
  | Aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN LPAREN BACKQUOTE QCONID BACKQUOTE Infixexp RPAREN
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
  , aexp_implies_var :: Var -> m Aexp
  , aexp_implies_INTEGER :: INTEGER -> m Aexp
  , aexp_implies_STRING :: STRING -> m Aexp
  , aexp_implies_LPAREN_exp_RPAREN :: LPAREN -> Exp -> RPAREN -> m Aexp
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
    (11, Token (MODULE _)) -> Just (Reduce 1 230)
    (11, Token (WHERE _)) -> Just (Reduce 1 230)
    (11, Token (RBRACE _)) -> Just (Reduce 1 230)
    (11, Token (LPAREN _)) -> Just (Reduce 1 230)
    (11, Token (RPAREN _)) -> Just (Reduce 1 230)
    (11, Token (COMMA _)) -> Just (Reduce 1 230)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 230)
    (11, Token (HIDING _)) -> Just (Reduce 1 230)
    (11, Token (MINUS _)) -> Just (Reduce 1 230)
    (11, Token (QCONID _)) -> Just (Reduce 1 230)
    (11, Token (EXPORT _)) -> Just (Reduce 1 230)
    (11, Token (AS _)) -> Just (Reduce 1 230)
    (11, Token (QVARID _)) -> Just (Reduce 1 230)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 230)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 230)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 73)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 188)
    (13, Token (TYPE _)) -> Just (Shift 146)
    (13, Token (DATA _)) -> Just (Shift 124)
    (13, Token (NEWTYPE _)) -> Just (Shift 144)
    (13, Token (CLASS _)) -> Just (Shift 111)
    (13, Token (INSTANCE _)) -> Just (Shift 113)
    (13, Token (DEFAULT _)) -> Just (Shift 194)
    (13, Token (FOREIGN _)) -> Just (Shift 195)
    (13, Token (INFIXL _)) -> Just (Shift 304)
    (13, Token (INFIXR _)) -> Just (Shift 305)
    (13, Token (INFIX _)) -> Just (Shift 306)
    (13, Token (EXPORT _)) -> Just (Shift 106)
    (13, Token (AS _)) -> Just (Shift 107)
    (13, Token (QVARID _)) -> Just (Shift 108)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 73)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 188)
    (16, Token (TYPE _)) -> Just (Shift 146)
    (16, Token (DATA _)) -> Just (Shift 124)
    (16, Token (NEWTYPE _)) -> Just (Shift 144)
    (16, Token (CLASS _)) -> Just (Shift 111)
    (16, Token (INSTANCE _)) -> Just (Shift 113)
    (16, Token (DEFAULT _)) -> Just (Shift 194)
    (16, Token (FOREIGN _)) -> Just (Shift 195)
    (16, Token (INFIXL _)) -> Just (Shift 304)
    (16, Token (INFIXR _)) -> Just (Shift 305)
    (16, Token (INFIX _)) -> Just (Shift 306)
    (16, Token (EXPORT _)) -> Just (Shift 106)
    (16, Token (AS _)) -> Just (Shift 107)
    (16, Token (QVARID _)) -> Just (Shift 108)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 101)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 157)
    (19, Token (EXPORT _)) -> Just (Shift 106)
    (19, Token (AS _)) -> Just (Shift 107)
    (19, Token (QVARID _)) -> Just (Shift 108)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 101)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 157)
    (22, Token (EXPORT _)) -> Just (Shift 106)
    (22, Token (AS _)) -> Just (Shift 107)
    (22, Token (QVARID _)) -> Just (Shift 108)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 101)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 157)
    (25, Token (EXPORT _)) -> Just (Shift 106)
    (25, Token (AS _)) -> Just (Shift 107)
    (25, Token (QVARID _)) -> Just (Shift 108)
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
    (34, Token (EXPORT _)) -> Just (Shift 106)
    (34, Token (AS _)) -> Just (Shift 107)
    (34, Token (QVARID _)) -> Just (Shift 108)
    (34, Token (STRING _)) -> Just (Shift 417)
    (34, Token (LET _)) -> Just (Shift 265)
    (34, Token (LAMBDA _)) -> Just (Shift 84)
    (34, Token (IF _)) -> Just (Shift 67)
    (34, Token (CASE _)) -> Just (Shift 69)
    (34, Token (DO _)) -> Just (Shift 395)
    (34, Token (INTEGER _)) -> Just (Shift 419)
    (35, Token (LPAREN _)) -> Just (Shift 54)
    (35, Token (MINUS _)) -> Just (Shift 44)
    (35, Token (EXPORT _)) -> Just (Shift 106)
    (35, Token (AS _)) -> Just (Shift 107)
    (35, Token (QVARID _)) -> Just (Shift 108)
    (35, Token (STRING _)) -> Just (Shift 417)
    (35, Token (LET _)) -> Just (Shift 265)
    (35, Token (LAMBDA _)) -> Just (Shift 84)
    (35, Token (IF _)) -> Just (Shift 67)
    (35, Token (CASE _)) -> Just (Shift 69)
    (35, Token (DO _)) -> Just (Shift 395)
    (35, Token (INTEGER _)) -> Just (Shift 419)
    (36, Token (LPAREN _)) -> Just (Shift 54)
    (36, Token (MINUS _)) -> Just (Shift 44)
    (36, Token (EXPORT _)) -> Just (Shift 106)
    (36, Token (AS _)) -> Just (Shift 107)
    (36, Token (QVARID _)) -> Just (Shift 108)
    (36, Token (STRING _)) -> Just (Shift 417)
    (36, Token (LET _)) -> Just (Shift 265)
    (36, Token (LAMBDA _)) -> Just (Shift 84)
    (36, Token (IF _)) -> Just (Shift 67)
    (36, Token (CASE _)) -> Just (Shift 69)
    (36, Token (DO _)) -> Just (Shift 395)
    (36, Token (INTEGER _)) -> Just (Shift 419)
    (37, Token (LPAREN _)) -> Just (Shift 54)
    (37, Token (MINUS _)) -> Just (Shift 44)
    (37, Token (EXPORT _)) -> Just (Shift 106)
    (37, Token (AS _)) -> Just (Shift 107)
    (37, Token (QVARID _)) -> Just (Shift 108)
    (37, Token (STRING _)) -> Just (Shift 417)
    (37, Token (LET _)) -> Just (Shift 265)
    (37, Token (LAMBDA _)) -> Just (Shift 84)
    (37, Token (IF _)) -> Just (Shift 67)
    (37, Token (CASE _)) -> Just (Shift 69)
    (37, Token (DO _)) -> Just (Shift 395)
    (37, Token (INTEGER _)) -> Just (Shift 419)
    (38, Token (LPAREN _)) -> Just (Shift 54)
    (38, Token (MINUS _)) -> Just (Shift 44)
    (38, Token (EXPORT _)) -> Just (Shift 106)
    (38, Token (AS _)) -> Just (Shift 107)
    (38, Token (QVARID _)) -> Just (Shift 108)
    (38, Token (STRING _)) -> Just (Shift 417)
    (38, Token (LET _)) -> Just (Shift 265)
    (38, Token (LAMBDA _)) -> Just (Shift 84)
    (38, Token (IF _)) -> Just (Shift 67)
    (38, Token (CASE _)) -> Just (Shift 69)
    (38, Token (DO _)) -> Just (Shift 395)
    (38, Token (INTEGER _)) -> Just (Shift 419)
    (39, Token (LPAREN _)) -> Just (Shift 54)
    (39, Token (MINUS _)) -> Just (Shift 44)
    (39, Token (EXPORT _)) -> Just (Shift 106)
    (39, Token (AS _)) -> Just (Shift 107)
    (39, Token (QVARID _)) -> Just (Shift 108)
    (39, Token (STRING _)) -> Just (Shift 417)
    (39, Token (LET _)) -> Just (Shift 265)
    (39, Token (LAMBDA _)) -> Just (Shift 84)
    (39, Token (IF _)) -> Just (Shift 67)
    (39, Token (CASE _)) -> Just (Shift 69)
    (39, Token (DO _)) -> Just (Shift 395)
    (39, Token (INTEGER _)) -> Just (Shift 419)
    (40, Token (LPAREN _)) -> Just (Shift 54)
    (40, Token (MINUS _)) -> Just (Shift 44)
    (40, Token (EXPORT _)) -> Just (Shift 106)
    (40, Token (AS _)) -> Just (Shift 107)
    (40, Token (QVARID _)) -> Just (Shift 108)
    (40, Token (STRING _)) -> Just (Shift 417)
    (40, Token (LET _)) -> Just (Shift 265)
    (40, Token (LAMBDA _)) -> Just (Shift 84)
    (40, Token (IF _)) -> Just (Shift 67)
    (40, Token (CASE _)) -> Just (Shift 69)
    (40, Token (DO _)) -> Just (Shift 395)
    (40, Token (INTEGER _)) -> Just (Shift 419)
    (41, Token (LPAREN _)) -> Just (Shift 54)
    (41, Token (MINUS _)) -> Just (Shift 44)
    (41, Token (EXPORT _)) -> Just (Shift 106)
    (41, Token (AS _)) -> Just (Shift 107)
    (41, Token (QVARID _)) -> Just (Shift 108)
    (41, Token (STRING _)) -> Just (Shift 417)
    (41, Token (LET _)) -> Just (Shift 265)
    (41, Token (LAMBDA _)) -> Just (Shift 84)
    (41, Token (IF _)) -> Just (Shift 67)
    (41, Token (CASE _)) -> Just (Shift 69)
    (41, Token (DO _)) -> Just (Shift 395)
    (41, Token (INTEGER _)) -> Just (Shift 419)
    (42, Token (LPAREN _)) -> Just (Shift 54)
    (42, Token (MINUS _)) -> Just (Shift 44)
    (42, Token (EXPORT _)) -> Just (Shift 106)
    (42, Token (AS _)) -> Just (Shift 107)
    (42, Token (QVARID _)) -> Just (Shift 108)
    (42, Token (STRING _)) -> Just (Shift 417)
    (42, Token (LET _)) -> Just (Shift 265)
    (42, Token (LAMBDA _)) -> Just (Shift 84)
    (42, Token (IF _)) -> Just (Shift 67)
    (42, Token (CASE _)) -> Just (Shift 69)
    (42, Token (DO _)) -> Just (Shift 395)
    (42, Token (INTEGER _)) -> Just (Shift 419)
    (43, Token (LPAREN _)) -> Just (Shift 54)
    (43, Token (MINUS _)) -> Just (Shift 44)
    (43, Token (EXPORT _)) -> Just (Shift 106)
    (43, Token (AS _)) -> Just (Shift 107)
    (43, Token (QVARID _)) -> Just (Shift 108)
    (43, Token (STRING _)) -> Just (Shift 417)
    (43, Token (LET _)) -> Just (Shift 265)
    (43, Token (LAMBDA _)) -> Just (Shift 84)
    (43, Token (IF _)) -> Just (Shift 67)
    (43, Token (CASE _)) -> Just (Shift 69)
    (43, Token (DO _)) -> Just (Shift 395)
    (43, Token (INTEGER _)) -> Just (Shift 419)
    (44, Token (LPAREN _)) -> Just (Shift 54)
    (44, Token (MINUS _)) -> Just (Shift 44)
    (44, Token (EXPORT _)) -> Just (Shift 106)
    (44, Token (AS _)) -> Just (Shift 107)
    (44, Token (QVARID _)) -> Just (Shift 108)
    (44, Token (STRING _)) -> Just (Shift 417)
    (44, Token (CASE _)) -> Just (Shift 69)
    (44, Token (DO _)) -> Just (Shift 395)
    (44, Token (INTEGER _)) -> Just (Shift 419)
    (45, Token (WHERE _)) -> Just (Reduce 1 170)
    (45, Token (RBRACE _)) -> Just (Reduce 1 170)
    (45, Token (LPAREN _)) -> Just (Shift 54)
    (45, Token (RPAREN _)) -> Just (Reduce 1 170)
    (45, Token (COMMA _)) -> Just (Reduce 1 170)
    (45, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (45, Token (EQUAL _)) -> Just (Reduce 1 170)
    (45, Token (PIPE _)) -> Just (Reduce 1 170)
    (45, Token (COLON_COLON _)) -> Just (Reduce 1 170)
    (45, Token (MINUS _)) -> Just (Reduce 1 170)
    (45, Token (EXPORT _)) -> Just (Shift 106)
    (45, Token (AS _)) -> Just (Shift 107)
    (45, Token (QVARID _)) -> Just (Shift 108)
    (45, Token (STRING _)) -> Just (Shift 417)
    (45, Token (LARROW _)) -> Just (Reduce 1 170)
    (45, Token (THEN _)) -> Just (Reduce 1 170)
    (45, Token (ELSE _)) -> Just (Reduce 1 170)
    (45, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (45, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (45, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (45, Token (OF _)) -> Just (Reduce 1 170)
    (45, Token (INTEGER _)) -> Just (Shift 419)
    (46, Token (LPAREN _)) -> Just (Shift 54)
    (46, Token (MINUS _)) -> Just (Shift 44)
    (46, Token (EXPORT _)) -> Just (Shift 106)
    (46, Token (AS _)) -> Just (Shift 107)
    (46, Token (QVARID _)) -> Just (Shift 108)
    (46, Token (STRING _)) -> Just (Shift 417)
    (46, Token (LET _)) -> Just (Shift 265)
    (46, Token (LAMBDA _)) -> Just (Shift 84)
    (46, Token (IF _)) -> Just (Shift 67)
    (46, Token (CASE _)) -> Just (Shift 69)
    (46, Token (DO _)) -> Just (Shift 395)
    (46, Token (INTEGER _)) -> Just (Shift 419)
    (47, Token (LPAREN _)) -> Just (Shift 54)
    (47, Token (MINUS _)) -> Just (Shift 44)
    (47, Token (EXPORT _)) -> Just (Shift 106)
    (47, Token (AS _)) -> Just (Shift 107)
    (47, Token (QVARID _)) -> Just (Shift 108)
    (47, Token (STRING _)) -> Just (Shift 417)
    (47, Token (LET _)) -> Just (Shift 265)
    (47, Token (LAMBDA _)) -> Just (Shift 84)
    (47, Token (IF _)) -> Just (Shift 67)
    (47, Token (CASE _)) -> Just (Shift 69)
    (47, Token (DO _)) -> Just (Shift 395)
    (47, Token (INTEGER _)) -> Just (Shift 419)
    (48, Token (LPAREN _)) -> Just (Shift 54)
    (48, Token (MINUS _)) -> Just (Shift 44)
    (48, Token (EXPORT _)) -> Just (Shift 106)
    (48, Token (AS _)) -> Just (Shift 107)
    (48, Token (QVARID _)) -> Just (Shift 108)
    (48, Token (STRING _)) -> Just (Shift 417)
    (48, Token (LET _)) -> Just (Shift 265)
    (48, Token (LAMBDA _)) -> Just (Shift 84)
    (48, Token (IF _)) -> Just (Shift 67)
    (48, Token (CASE _)) -> Just (Shift 69)
    (48, Token (DO _)) -> Just (Shift 395)
    (48, Token (INTEGER _)) -> Just (Shift 419)
    (49, Token (LPAREN _)) -> Just (Shift 54)
    (49, Token (MINUS _)) -> Just (Shift 44)
    (49, Token (EXPORT _)) -> Just (Shift 106)
    (49, Token (AS _)) -> Just (Shift 107)
    (49, Token (QVARID _)) -> Just (Shift 108)
    (49, Token (STRING _)) -> Just (Shift 417)
    (49, Token (LET _)) -> Just (Shift 265)
    (49, Token (LAMBDA _)) -> Just (Shift 84)
    (49, Token (IF _)) -> Just (Shift 67)
    (49, Token (CASE _)) -> Just (Shift 69)
    (49, Token (DO _)) -> Just (Shift 395)
    (49, Token (INTEGER _)) -> Just (Shift 419)
    (50, Token (LPAREN _)) -> Just (Shift 54)
    (50, Token (MINUS _)) -> Just (Shift 44)
    (50, Token (EXPORT _)) -> Just (Shift 106)
    (50, Token (AS _)) -> Just (Shift 107)
    (50, Token (QVARID _)) -> Just (Shift 108)
    (50, Token (STRING _)) -> Just (Shift 417)
    (50, Token (LET _)) -> Just (Shift 265)
    (50, Token (LAMBDA _)) -> Just (Shift 84)
    (50, Token (IF _)) -> Just (Shift 67)
    (50, Token (CASE _)) -> Just (Shift 69)
    (50, Token (DO _)) -> Just (Shift 395)
    (50, Token (INTEGER _)) -> Just (Shift 419)
    (51, Token (RBRACE _)) -> Just (Reduce 0 197)
    (51, Token (LPAREN _)) -> Just (Shift 54)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 197)
    (51, Token (MINUS _)) -> Just (Shift 44)
    (51, Token (EXPORT _)) -> Just (Shift 106)
    (51, Token (AS _)) -> Just (Shift 107)
    (51, Token (QVARID _)) -> Just (Shift 108)
    (51, Token (STRING _)) -> Just (Shift 417)
    (51, Token (LET _)) -> Just (Shift 258)
    (51, Token (LAMBDA _)) -> Just (Shift 84)
    (51, Token (IF _)) -> Just (Shift 67)
    (51, Token (CASE _)) -> Just (Shift 69)
    (51, Token (DO _)) -> Just (Shift 395)
    (51, Token (INTEGER _)) -> Just (Shift 419)
    (52, Token (RBRACE _)) -> Just (Reduce 0 197)
    (52, Token (LPAREN _)) -> Just (Shift 54)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 197)
    (52, Token (MINUS _)) -> Just (Shift 44)
    (52, Token (EXPORT _)) -> Just (Shift 106)
    (52, Token (AS _)) -> Just (Shift 107)
    (52, Token (QVARID _)) -> Just (Shift 108)
    (52, Token (STRING _)) -> Just (Shift 417)
    (52, Token (LET _)) -> Just (Shift 258)
    (52, Token (LAMBDA _)) -> Just (Shift 84)
    (52, Token (IF _)) -> Just (Shift 67)
    (52, Token (CASE _)) -> Just (Shift 69)
    (52, Token (DO _)) -> Just (Shift 395)
    (52, Token (INTEGER _)) -> Just (Shift 419)
    (53, Token (LPAREN _)) -> Just (Shift 54)
    (53, Token (MINUS _)) -> Just (Shift 44)
    (53, Token (EXPORT _)) -> Just (Shift 106)
    (53, Token (AS _)) -> Just (Shift 107)
    (53, Token (QVARID _)) -> Just (Shift 108)
    (53, Token (STRING _)) -> Just (Shift 417)
    (53, Token (LET _)) -> Just (Shift 265)
    (53, Token (LAMBDA _)) -> Just (Shift 84)
    (53, Token (IF _)) -> Just (Shift 67)
    (53, Token (CASE _)) -> Just (Shift 69)
    (53, Token (DO _)) -> Just (Shift 395)
    (53, Token (INTEGER _)) -> Just (Shift 419)
    (54, Token (LPAREN _)) -> Just (Shift 54)
    (54, Token (MINUS _)) -> Just (Shift 55)
    (54, Token (EXPORT _)) -> Just (Shift 106)
    (54, Token (AS _)) -> Just (Shift 107)
    (54, Token (QVARID _)) -> Just (Shift 108)
    (54, Token (STRING _)) -> Just (Shift 417)
    (54, Token (LET _)) -> Just (Shift 265)
    (54, Token (LAMBDA _)) -> Just (Shift 84)
    (54, Token (IF _)) -> Just (Shift 67)
    (54, Token (QVARSYM _)) -> Just (Shift 56)
    (54, Token (BACKQUOTE _)) -> Just (Shift 418)
    (54, Token (QCONSYM _)) -> Just (Shift 61)
    (54, Token (CASE _)) -> Just (Shift 69)
    (54, Token (DO _)) -> Just (Shift 395)
    (54, Token (INTEGER _)) -> Just (Shift 419)
    (55, Token (LPAREN _)) -> Just (Shift 54)
    (55, Token (RPAREN _)) -> Just (Shift 103)
    (55, Token (MINUS _)) -> Just (Shift 44)
    (55, Token (EXPORT _)) -> Just (Shift 106)
    (55, Token (AS _)) -> Just (Shift 107)
    (55, Token (QVARID _)) -> Just (Shift 108)
    (55, Token (STRING _)) -> Just (Shift 417)
    (55, Token (CASE _)) -> Just (Shift 69)
    (55, Token (DO _)) -> Just (Shift 395)
    (55, Token (INTEGER _)) -> Just (Shift 419)
    (56, Token (LPAREN _)) -> Just (Shift 54)
    (56, Token (RPAREN _)) -> Just (Shift 104)
    (56, Token (MINUS _)) -> Just (Shift 44)
    (56, Token (EXPORT _)) -> Just (Shift 106)
    (56, Token (AS _)) -> Just (Shift 107)
    (56, Token (QVARID _)) -> Just (Shift 108)
    (56, Token (STRING _)) -> Just (Shift 417)
    (56, Token (LET _)) -> Just (Shift 265)
    (56, Token (LAMBDA _)) -> Just (Shift 84)
    (56, Token (IF _)) -> Just (Shift 67)
    (56, Token (CASE _)) -> Just (Shift 69)
    (56, Token (DO _)) -> Just (Shift 395)
    (56, Token (INTEGER _)) -> Just (Shift 419)
    (57, Token (LPAREN _)) -> Just (Shift 54)
    (57, Token (MINUS _)) -> Just (Shift 44)
    (57, Token (EXPORT _)) -> Just (Shift 106)
    (57, Token (AS _)) -> Just (Shift 107)
    (57, Token (QVARID _)) -> Just (Shift 108)
    (57, Token (STRING _)) -> Just (Shift 417)
    (57, Token (LET _)) -> Just (Shift 265)
    (57, Token (LAMBDA _)) -> Just (Shift 84)
    (57, Token (IF _)) -> Just (Shift 67)
    (57, Token (CASE _)) -> Just (Shift 69)
    (57, Token (DO _)) -> Just (Shift 395)
    (57, Token (INTEGER _)) -> Just (Shift 419)
    (58, Token (LPAREN _)) -> Just (Shift 54)
    (58, Token (MINUS _)) -> Just (Shift 44)
    (58, Token (EXPORT _)) -> Just (Shift 106)
    (58, Token (AS _)) -> Just (Shift 107)
    (58, Token (QVARID _)) -> Just (Shift 108)
    (58, Token (STRING _)) -> Just (Shift 417)
    (58, Token (LET _)) -> Just (Shift 265)
    (58, Token (LAMBDA _)) -> Just (Shift 84)
    (58, Token (IF _)) -> Just (Shift 67)
    (58, Token (CASE _)) -> Just (Shift 69)
    (58, Token (DO _)) -> Just (Shift 395)
    (58, Token (INTEGER _)) -> Just (Shift 419)
    (59, Token (LPAREN _)) -> Just (Shift 54)
    (59, Token (MINUS _)) -> Just (Shift 44)
    (59, Token (EXPORT _)) -> Just (Shift 106)
    (59, Token (AS _)) -> Just (Shift 107)
    (59, Token (QVARID _)) -> Just (Shift 108)
    (59, Token (STRING _)) -> Just (Shift 417)
    (59, Token (LET _)) -> Just (Shift 265)
    (59, Token (LAMBDA _)) -> Just (Shift 84)
    (59, Token (IF _)) -> Just (Shift 67)
    (59, Token (CASE _)) -> Just (Shift 69)
    (59, Token (DO _)) -> Just (Shift 395)
    (59, Token (INTEGER _)) -> Just (Shift 419)
    (60, Token (LPAREN _)) -> Just (Shift 54)
    (60, Token (MINUS _)) -> Just (Shift 44)
    (60, Token (EXPORT _)) -> Just (Shift 106)
    (60, Token (AS _)) -> Just (Shift 107)
    (60, Token (QVARID _)) -> Just (Shift 108)
    (60, Token (STRING _)) -> Just (Shift 417)
    (60, Token (LET _)) -> Just (Shift 265)
    (60, Token (LAMBDA _)) -> Just (Shift 84)
    (60, Token (IF _)) -> Just (Shift 67)
    (60, Token (CASE _)) -> Just (Shift 69)
    (60, Token (DO _)) -> Just (Shift 395)
    (60, Token (INTEGER _)) -> Just (Shift 419)
    (61, Token (LPAREN _)) -> Just (Shift 54)
    (61, Token (MINUS _)) -> Just (Shift 44)
    (61, Token (EXPORT _)) -> Just (Shift 106)
    (61, Token (AS _)) -> Just (Shift 107)
    (61, Token (QVARID _)) -> Just (Shift 108)
    (61, Token (STRING _)) -> Just (Shift 417)
    (61, Token (LET _)) -> Just (Shift 265)
    (61, Token (LAMBDA _)) -> Just (Shift 84)
    (61, Token (IF _)) -> Just (Shift 67)
    (61, Token (CASE _)) -> Just (Shift 69)
    (61, Token (DO _)) -> Just (Shift 395)
    (61, Token (INTEGER _)) -> Just (Shift 419)
    (62, Token (LPAREN _)) -> Just (Shift 54)
    (62, Token (MINUS _)) -> Just (Shift 44)
    (62, Token (EXPORT _)) -> Just (Shift 106)
    (62, Token (AS _)) -> Just (Shift 107)
    (62, Token (QVARID _)) -> Just (Shift 108)
    (62, Token (STRING _)) -> Just (Shift 417)
    (62, Token (LET _)) -> Just (Shift 264)
    (62, Token (LAMBDA _)) -> Just (Shift 84)
    (62, Token (IF _)) -> Just (Shift 67)
    (62, Token (CASE _)) -> Just (Shift 69)
    (62, Token (DO _)) -> Just (Shift 395)
    (62, Token (INTEGER _)) -> Just (Shift 419)
    (63, Token (LPAREN _)) -> Just (Shift 54)
    (63, Token (MINUS _)) -> Just (Shift 44)
    (63, Token (EXPORT _)) -> Just (Shift 106)
    (63, Token (AS _)) -> Just (Shift 107)
    (63, Token (QVARID _)) -> Just (Shift 108)
    (63, Token (STRING _)) -> Just (Shift 417)
    (63, Token (LET _)) -> Just (Shift 264)
    (63, Token (LAMBDA _)) -> Just (Shift 84)
    (63, Token (IF _)) -> Just (Shift 67)
    (63, Token (CASE _)) -> Just (Shift 69)
    (63, Token (DO _)) -> Just (Shift 395)
    (63, Token (INTEGER _)) -> Just (Shift 419)
    (64, Token (LPAREN _)) -> Just (Shift 54)
    (64, Token (MINUS _)) -> Just (Shift 44)
    (64, Token (EXPORT _)) -> Just (Shift 106)
    (64, Token (AS _)) -> Just (Shift 107)
    (64, Token (QVARID _)) -> Just (Shift 108)
    (64, Token (STRING _)) -> Just (Shift 417)
    (64, Token (LET _)) -> Just (Shift 264)
    (64, Token (LAMBDA _)) -> Just (Shift 84)
    (64, Token (IF _)) -> Just (Shift 67)
    (64, Token (CASE _)) -> Just (Shift 69)
    (64, Token (DO _)) -> Just (Shift 395)
    (64, Token (INTEGER _)) -> Just (Shift 419)
    (65, Token (LPAREN _)) -> Just (Shift 54)
    (65, Token (MINUS _)) -> Just (Shift 44)
    (65, Token (EXPORT _)) -> Just (Shift 106)
    (65, Token (AS _)) -> Just (Shift 107)
    (65, Token (QVARID _)) -> Just (Shift 108)
    (65, Token (STRING _)) -> Just (Shift 417)
    (65, Token (LET _)) -> Just (Shift 264)
    (65, Token (LAMBDA _)) -> Just (Shift 84)
    (65, Token (IF _)) -> Just (Shift 67)
    (65, Token (CASE _)) -> Just (Shift 69)
    (65, Token (DO _)) -> Just (Shift 395)
    (65, Token (INTEGER _)) -> Just (Shift 419)
    (66, Token (LPAREN _)) -> Just (Shift 54)
    (66, Token (MINUS _)) -> Just (Shift 44)
    (66, Token (EXPORT _)) -> Just (Shift 106)
    (66, Token (AS _)) -> Just (Shift 107)
    (66, Token (QVARID _)) -> Just (Shift 108)
    (66, Token (STRING _)) -> Just (Shift 417)
    (66, Token (LET _)) -> Just (Shift 264)
    (66, Token (LAMBDA _)) -> Just (Shift 84)
    (66, Token (IF _)) -> Just (Shift 67)
    (66, Token (CASE _)) -> Just (Shift 69)
    (66, Token (DO _)) -> Just (Shift 395)
    (66, Token (INTEGER _)) -> Just (Shift 419)
    (67, Token (LPAREN _)) -> Just (Shift 54)
    (67, Token (MINUS _)) -> Just (Shift 44)
    (67, Token (EXPORT _)) -> Just (Shift 106)
    (67, Token (AS _)) -> Just (Shift 107)
    (67, Token (QVARID _)) -> Just (Shift 108)
    (67, Token (STRING _)) -> Just (Shift 417)
    (67, Token (LET _)) -> Just (Shift 265)
    (67, Token (LAMBDA _)) -> Just (Shift 84)
    (67, Token (IF _)) -> Just (Shift 67)
    (67, Token (CASE _)) -> Just (Shift 69)
    (67, Token (DO _)) -> Just (Shift 395)
    (67, Token (INTEGER _)) -> Just (Shift 419)
    (68, Token (LPAREN _)) -> Just (Shift 54)
    (68, Token (MINUS _)) -> Just (Shift 44)
    (68, Token (EXPORT _)) -> Just (Shift 106)
    (68, Token (AS _)) -> Just (Shift 107)
    (68, Token (QVARID _)) -> Just (Shift 108)
    (68, Token (STRING _)) -> Just (Shift 417)
    (68, Token (LET _)) -> Just (Shift 265)
    (68, Token (LAMBDA _)) -> Just (Shift 84)
    (68, Token (IF _)) -> Just (Shift 67)
    (68, Token (CASE _)) -> Just (Shift 69)
    (68, Token (DO _)) -> Just (Shift 395)
    (68, Token (INTEGER _)) -> Just (Shift 419)
    (69, Token (LPAREN _)) -> Just (Shift 54)
    (69, Token (MINUS _)) -> Just (Shift 44)
    (69, Token (EXPORT _)) -> Just (Shift 106)
    (69, Token (AS _)) -> Just (Shift 107)
    (69, Token (QVARID _)) -> Just (Shift 108)
    (69, Token (STRING _)) -> Just (Shift 417)
    (69, Token (LET _)) -> Just (Shift 265)
    (69, Token (LAMBDA _)) -> Just (Shift 84)
    (69, Token (IF _)) -> Just (Shift 67)
    (69, Token (CASE _)) -> Just (Shift 69)
    (69, Token (DO _)) -> Just (Shift 395)
    (69, Token (INTEGER _)) -> Just (Shift 419)
    (70, Token (LPAREN _)) -> Just (Shift 54)
    (70, Token (MINUS _)) -> Just (Shift 44)
    (70, Token (EXPORT _)) -> Just (Shift 106)
    (70, Token (AS _)) -> Just (Shift 107)
    (70, Token (QVARID _)) -> Just (Shift 108)
    (70, Token (STRING _)) -> Just (Shift 417)
    (70, Token (LET _)) -> Just (Shift 265)
    (70, Token (LAMBDA _)) -> Just (Shift 84)
    (70, Token (IF _)) -> Just (Shift 67)
    (70, Token (CASE _)) -> Just (Shift 69)
    (70, Token (DO _)) -> Just (Shift 395)
    (70, Token (INTEGER _)) -> Just (Shift 419)
    (71, Token (LPAREN _)) -> Just (Shift 73)
    (71, Token (EXPORT _)) -> Just (Shift 106)
    (71, Token (AS _)) -> Just (Shift 107)
    (71, Token (QVARID _)) -> Just (Shift 108)
    (72, Token (LPAREN _)) -> Just (Shift 73)
    (72, Token (EXPORT _)) -> Just (Shift 106)
    (72, Token (AS _)) -> Just (Shift 107)
    (72, Token (QVARID _)) -> Just (Shift 108)
    (73, Token (LPAREN _)) -> Just (Shift 73)
    (73, Token (MINUS _)) -> Just (Shift 105)
    (73, Token (EXPORT _)) -> Just (Shift 106)
    (73, Token (AS _)) -> Just (Shift 107)
    (73, Token (QVARID _)) -> Just (Shift 108)
    (73, Token (QVARSYM _)) -> Just (Shift 109)
    (74, Token (LPAREN _)) -> Just (Shift 73)
    (74, Token (RPAREN _)) -> Just (Shift 432)
    (74, Token (MINUS _)) -> Just (Shift 71)
    (74, Token (EXPORT _)) -> Just (Shift 106)
    (74, Token (AS _)) -> Just (Shift 107)
    (74, Token (QVARID _)) -> Just (Shift 108)
    (74, Token (QVARSYM _)) -> Just (Shift 437)
    (74, Token (BACKQUOTE _)) -> Just (Shift 346)
    (74, Token (QCONSYM _)) -> Just (Shift 349)
    (75, Token (RBRACE _)) -> Just (Reduce 0 85)
    (75, Token (LPAREN _)) -> Just (Shift 73)
    (75, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (75, Token (INFIXL _)) -> Just (Shift 304)
    (75, Token (INFIXR _)) -> Just (Shift 305)
    (75, Token (INFIX _)) -> Just (Shift 306)
    (75, Token (EXPORT _)) -> Just (Shift 106)
    (75, Token (AS _)) -> Just (Shift 107)
    (75, Token (QVARID _)) -> Just (Shift 108)
    (76, Token (RBRACE _)) -> Just (Reduce 0 85)
    (76, Token (LPAREN _)) -> Just (Shift 73)
    (76, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (76, Token (INFIXL _)) -> Just (Shift 304)
    (76, Token (INFIXR _)) -> Just (Shift 305)
    (76, Token (INFIX _)) -> Just (Shift 306)
    (76, Token (EXPORT _)) -> Just (Shift 106)
    (76, Token (AS _)) -> Just (Shift 107)
    (76, Token (QVARID _)) -> Just (Shift 108)
    (77, Token (RBRACE _)) -> Just (Reduce 0 85)
    (77, Token (LPAREN _)) -> Just (Shift 73)
    (77, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (77, Token (INFIXL _)) -> Just (Shift 304)
    (77, Token (INFIXR _)) -> Just (Shift 305)
    (77, Token (INFIX _)) -> Just (Shift 306)
    (77, Token (EXPORT _)) -> Just (Shift 106)
    (77, Token (AS _)) -> Just (Shift 107)
    (77, Token (QVARID _)) -> Just (Shift 108)
    (78, Token (RBRACE _)) -> Just (Reduce 0 85)
    (78, Token (LPAREN _)) -> Just (Shift 73)
    (78, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (78, Token (INFIXL _)) -> Just (Shift 304)
    (78, Token (INFIXR _)) -> Just (Shift 305)
    (78, Token (INFIX _)) -> Just (Shift 306)
    (78, Token (EXPORT _)) -> Just (Shift 106)
    (78, Token (AS _)) -> Just (Shift 107)
    (78, Token (QVARID _)) -> Just (Shift 108)
    (79, Token (LPAREN _)) -> Just (Shift 73)
    (79, Token (EQUAL _)) -> Just (Shift 46)
    (79, Token (PIPE _)) -> Just (Shift 62)
    (79, Token (MINUS _)) -> Just (Shift 71)
    (79, Token (EXPORT _)) -> Just (Shift 106)
    (79, Token (AS _)) -> Just (Shift 107)
    (79, Token (QVARID _)) -> Just (Shift 108)
    (79, Token (QVARSYM _)) -> Just (Shift 437)
    (79, Token (BACKQUOTE _)) -> Just (Shift 346)
    (79, Token (QCONSYM _)) -> Just (Shift 349)
    (80, Token (RBRACE _)) -> Just (Reduce 0 80)
    (80, Token (LPAREN _)) -> Just (Shift 73)
    (80, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (80, Token (EXPORT _)) -> Just (Shift 106)
    (80, Token (AS _)) -> Just (Shift 107)
    (80, Token (QVARID _)) -> Just (Shift 108)
    (81, Token (RBRACE _)) -> Just (Reduce 0 80)
    (81, Token (LPAREN _)) -> Just (Shift 73)
    (81, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (81, Token (EXPORT _)) -> Just (Shift 106)
    (81, Token (AS _)) -> Just (Shift 107)
    (81, Token (QVARID _)) -> Just (Shift 108)
    (82, Token (LPAREN _)) -> Just (Shift 73)
    (82, Token (EQUAL _)) -> Just (Shift 48)
    (82, Token (PIPE _)) -> Just (Shift 64)
    (82, Token (MINUS _)) -> Just (Shift 71)
    (82, Token (EXPORT _)) -> Just (Shift 106)
    (82, Token (AS _)) -> Just (Shift 107)
    (82, Token (QVARID _)) -> Just (Shift 108)
    (82, Token (QVARSYM _)) -> Just (Shift 437)
    (82, Token (BACKQUOTE _)) -> Just (Shift 346)
    (82, Token (QCONSYM _)) -> Just (Shift 349)
    (83, Token (LPAREN _)) -> Just (Shift 73)
    (83, Token (EQUAL _)) -> Just (Shift 49)
    (83, Token (PIPE _)) -> Just (Shift 65)
    (83, Token (MINUS _)) -> Just (Shift 71)
    (83, Token (EXPORT _)) -> Just (Shift 106)
    (83, Token (AS _)) -> Just (Shift 107)
    (83, Token (QVARID _)) -> Just (Shift 108)
    (83, Token (QVARSYM _)) -> Just (Shift 437)
    (83, Token (BACKQUOTE _)) -> Just (Shift 346)
    (83, Token (QCONSYM _)) -> Just (Shift 349)
    (84, Token (LPAREN _)) -> Just (Shift 73)
    (84, Token (EXPORT _)) -> Just (Shift 106)
    (84, Token (AS _)) -> Just (Shift 107)
    (84, Token (QVARID _)) -> Just (Shift 108)
    (85, Token (RBRACE _)) -> Just (Reduce 0 185)
    (85, Token (LPAREN _)) -> Just (Shift 73)
    (85, Token (SEMICOLON _)) -> Just (Reduce 0 185)
    (85, Token (EXPORT _)) -> Just (Shift 106)
    (85, Token (AS _)) -> Just (Shift 107)
    (85, Token (QVARID _)) -> Just (Shift 108)
    (86, Token (RBRACE _)) -> Just (Reduce 0 185)
    (86, Token (LPAREN _)) -> Just (Shift 73)
    (86, Token (SEMICOLON _)) -> Just (Reduce 0 185)
    (86, Token (EXPORT _)) -> Just (Shift 106)
    (86, Token (AS _)) -> Just (Shift 107)
    (86, Token (QVARID _)) -> Just (Shift 108)
    (87, Token (LPAREN _)) -> Just (Shift 73)
    (87, Token (MINUS _)) -> Just (Shift 71)
    (87, Token (RARROW _)) -> Just (Shift 35)
    (87, Token (EXPORT _)) -> Just (Shift 106)
    (87, Token (AS _)) -> Just (Shift 107)
    (87, Token (QVARID _)) -> Just (Shift 108)
    (87, Token (QVARSYM _)) -> Just (Shift 437)
    (87, Token (BACKQUOTE _)) -> Just (Shift 346)
    (87, Token (QCONSYM _)) -> Just (Shift 349)
    (88, Token (LPAREN _)) -> Just (Shift 73)
    (88, Token (MINUS _)) -> Just (Shift 71)
    (88, Token (RARROW _)) -> Just (Shift 50)
    (88, Token (EXPORT _)) -> Just (Shift 106)
    (88, Token (AS _)) -> Just (Shift 107)
    (88, Token (QVARID _)) -> Just (Shift 108)
    (88, Token (QVARSYM _)) -> Just (Shift 437)
    (88, Token (BACKQUOTE _)) -> Just (Shift 346)
    (88, Token (QCONSYM _)) -> Just (Shift 349)
    (89, Token (LPAREN _)) -> Just (Shift 101)
    (89, Token (RPAREN _)) -> Just (Reduce 0 15)
    (89, Token (QCONID _)) -> Just (Shift 157)
    (89, Token (EXPORT _)) -> Just (Shift 106)
    (89, Token (AS _)) -> Just (Shift 107)
    (89, Token (QVARID _)) -> Just (Shift 108)
    (90, Token (LPAREN _)) -> Just (Shift 101)
    (90, Token (RPAREN _)) -> Just (Reduce 0 15)
    (90, Token (QCONID _)) -> Just (Shift 157)
    (90, Token (EXPORT _)) -> Just (Shift 106)
    (90, Token (AS _)) -> Just (Shift 107)
    (90, Token (QVARID _)) -> Just (Shift 108)
    (91, Token (LPAREN _)) -> Just (Shift 101)
    (91, Token (RPAREN _)) -> Just (Reduce 0 15)
    (91, Token (QCONID _)) -> Just (Shift 157)
    (91, Token (EXPORT _)) -> Just (Shift 106)
    (91, Token (AS _)) -> Just (Shift 107)
    (91, Token (QVARID _)) -> Just (Shift 108)
    (92, Token (LPAREN _)) -> Just (Shift 101)
    (92, Token (QCONID _)) -> Just (Shift 157)
    (92, Token (EXPORT _)) -> Just (Shift 106)
    (92, Token (AS _)) -> Just (Shift 107)
    (92, Token (QVARID _)) -> Just (Shift 108)
    (93, Token (LPAREN _)) -> Just (Shift 101)
    (93, Token (RPAREN _)) -> Just (Shift 163)
    (93, Token (DOT_DOT _)) -> Just (Shift 166)
    (93, Token (QCONID _)) -> Just (Shift 157)
    (93, Token (EXPORT _)) -> Just (Shift 106)
    (93, Token (AS _)) -> Just (Shift 107)
    (93, Token (QVARID _)) -> Just (Shift 108)
    (94, Token (LPAREN _)) -> Just (Shift 102)
    (94, Token (EXPORT _)) -> Just (Shift 106)
    (94, Token (AS _)) -> Just (Shift 107)
    (94, Token (QVARID _)) -> Just (Shift 108)
    (95, Token (RBRACE _)) -> Just (Shift 342)
    (95, Token (LPAREN _)) -> Just (Shift 102)
    (95, Token (EXPORT _)) -> Just (Shift 106)
    (95, Token (AS _)) -> Just (Shift 107)
    (95, Token (QVARID _)) -> Just (Shift 108)
    (96, Token (LPAREN _)) -> Just (Shift 102)
    (96, Token (EXPORT _)) -> Just (Shift 106)
    (96, Token (AS _)) -> Just (Shift 107)
    (96, Token (QVARID _)) -> Just (Shift 108)
    (97, Token (LPAREN _)) -> Just (Shift 102)
    (97, Token (EXPORT _)) -> Just (Shift 106)
    (97, Token (AS _)) -> Just (Shift 107)
    (97, Token (QVARID _)) -> Just (Shift 108)
    (98, Token (LPAREN _)) -> Just (Shift 102)
    (98, Token (EXPORT _)) -> Just (Shift 106)
    (98, Token (AS _)) -> Just (Shift 107)
    (98, Token (QVARID _)) -> Just (Shift 108)
    (99, Token (LPAREN _)) -> Just (Shift 102)
    (99, Token (EXPORT _)) -> Just (Shift 106)
    (99, Token (AS _)) -> Just (Shift 107)
    (99, Token (QVARID _)) -> Just (Shift 108)
    (100, Token (LPAREN _)) -> Just (Shift 102)
    (100, Token (EXPORT _)) -> Just (Shift 106)
    (100, Token (AS _)) -> Just (Shift 107)
    (100, Token (QVARID _)) -> Just (Shift 108)
    (101, Token (MINUS _)) -> Just (Shift 105)
    (101, Token (QVARSYM _)) -> Just (Shift 109)
    (101, Token (QCONSYM _)) -> Just (Shift 158)
    (102, Token (MINUS _)) -> Just (Shift 105)
    (102, Token (QVARSYM _)) -> Just (Shift 109)
    (103, Token (WHERE _)) -> Just (Reduce 3 210)
    (103, Token (LBRACE _)) -> Just (Reduce 3 210)
    (103, Token (RBRACE _)) -> Just (Reduce 3 210)
    (103, Token (LPAREN _)) -> Just (Reduce 3 210)
    (103, Token (RPAREN _)) -> Just (Reduce 3 210)
    (103, Token (COMMA _)) -> Just (Reduce 3 210)
    (103, Token (SEMICOLON _)) -> Just (Reduce 3 210)
    (103, Token (EQUAL _)) -> Just (Reduce 3 210)
    (103, Token (PIPE _)) -> Just (Reduce 3 210)
    (103, Token (COLON_COLON _)) -> Just (Reduce 3 210)
    (103, Token (MINUS _)) -> Just (Reduce 3 210)
    (103, Token (INFIXL _)) -> Just (Reduce 3 210)
    (103, Token (INFIXR _)) -> Just (Reduce 3 210)
    (103, Token (INFIX _)) -> Just (Reduce 3 210)
    (103, Token (RARROW _)) -> Just (Reduce 3 210)
    (103, Token (QCONID _)) -> Just (Reduce 3 210)
    (103, Token (EXPORT _)) -> Just (Reduce 3 210)
    (103, Token (AS _)) -> Just (Reduce 3 210)
    (103, Token (QVARID _)) -> Just (Reduce 3 210)
    (103, Token (STRING _)) -> Just (Reduce 3 210)
    (103, Token (LARROW _)) -> Just (Reduce 3 210)
    (103, Token (LET _)) -> Just (Reduce 3 210)
    (103, Token (LAMBDA _)) -> Just (Reduce 3 210)
    (103, Token (IF _)) -> Just (Reduce 3 210)
    (103, Token (THEN _)) -> Just (Reduce 3 210)
    (103, Token (ELSE _)) -> Just (Reduce 3 210)
    (103, Token (QVARSYM _)) -> Just (Reduce 3 210)
    (103, Token (BACKQUOTE _)) -> Just (Reduce 3 210)
    (103, Token (QCONSYM _)) -> Just (Reduce 3 210)
    (103, Token (CASE _)) -> Just (Reduce 3 210)
    (103, Token (OF _)) -> Just (Reduce 3 210)
    (103, Token (DO _)) -> Just (Reduce 3 210)
    (103, Token (INTEGER _)) -> Just (Reduce 3 210)
    (104, Token (WHERE _)) -> Just (Reduce 3 211)
    (104, Token (LBRACE _)) -> Just (Reduce 3 211)
    (104, Token (RBRACE _)) -> Just (Reduce 3 211)
    (104, Token (LPAREN _)) -> Just (Reduce 3 211)
    (104, Token (RPAREN _)) -> Just (Reduce 3 211)
    (104, Token (COMMA _)) -> Just (Reduce 3 211)
    (104, Token (SEMICOLON _)) -> Just (Reduce 3 211)
    (104, Token (EQUAL _)) -> Just (Reduce 3 211)
    (104, Token (PIPE _)) -> Just (Reduce 3 211)
    (104, Token (COLON_COLON _)) -> Just (Reduce 3 211)
    (104, Token (MINUS _)) -> Just (Reduce 3 211)
    (104, Token (INFIXL _)) -> Just (Reduce 3 211)
    (104, Token (INFIXR _)) -> Just (Reduce 3 211)
    (104, Token (INFIX _)) -> Just (Reduce 3 211)
    (104, Token (RARROW _)) -> Just (Reduce 3 211)
    (104, Token (QCONID _)) -> Just (Reduce 3 211)
    (104, Token (EXPORT _)) -> Just (Reduce 3 211)
    (104, Token (AS _)) -> Just (Reduce 3 211)
    (104, Token (QVARID _)) -> Just (Reduce 3 211)
    (104, Token (STRING _)) -> Just (Reduce 3 211)
    (104, Token (LARROW _)) -> Just (Reduce 3 211)
    (104, Token (LET _)) -> Just (Reduce 3 211)
    (104, Token (LAMBDA _)) -> Just (Reduce 3 211)
    (104, Token (IF _)) -> Just (Reduce 3 211)
    (104, Token (THEN _)) -> Just (Reduce 3 211)
    (104, Token (ELSE _)) -> Just (Reduce 3 211)
    (104, Token (QVARSYM _)) -> Just (Reduce 3 211)
    (104, Token (BACKQUOTE _)) -> Just (Reduce 3 211)
    (104, Token (QCONSYM _)) -> Just (Reduce 3 211)
    (104, Token (CASE _)) -> Just (Reduce 3 211)
    (104, Token (OF _)) -> Just (Reduce 3 211)
    (104, Token (DO _)) -> Just (Reduce 3 211)
    (104, Token (INTEGER _)) -> Just (Reduce 3 211)
    (105, Token (RPAREN _)) -> Just (Shift 103)
    (106, Token (WHERE _)) -> Just (Reduce 1 208)
    (106, Token (LBRACE _)) -> Just (Reduce 1 208)
    (106, Token (RBRACE _)) -> Just (Reduce 1 208)
    (106, Token (LPAREN _)) -> Just (Reduce 1 208)
    (106, Token (RPAREN _)) -> Just (Reduce 1 208)
    (106, Token (COMMA _)) -> Just (Reduce 1 208)
    (106, Token (SEMICOLON _)) -> Just (Reduce 1 208)
    (106, Token (EQUAL _)) -> Just (Reduce 1 208)
    (106, Token (PIPE _)) -> Just (Reduce 1 208)
    (106, Token (COLON_COLON _)) -> Just (Reduce 1 208)
    (106, Token (MINUS _)) -> Just (Reduce 1 208)
    (106, Token (INFIXL _)) -> Just (Reduce 1 208)
    (106, Token (INFIXR _)) -> Just (Reduce 1 208)
    (106, Token (INFIX _)) -> Just (Reduce 1 208)
    (106, Token (RARROW _)) -> Just (Reduce 1 208)
    (106, Token (QCONID _)) -> Just (Reduce 1 208)
    (106, Token (EXPORT _)) -> Just (Reduce 1 208)
    (106, Token (AS _)) -> Just (Reduce 1 208)
    (106, Token (QVARID _)) -> Just (Reduce 1 208)
    (106, Token (STRING _)) -> Just (Reduce 1 208)
    (106, Token (LARROW _)) -> Just (Reduce 1 208)
    (106, Token (LET _)) -> Just (Reduce 1 208)
    (106, Token (LAMBDA _)) -> Just (Reduce 1 208)
    (106, Token (IF _)) -> Just (Reduce 1 208)
    (106, Token (THEN _)) -> Just (Reduce 1 208)
    (106, Token (ELSE _)) -> Just (Reduce 1 208)
    (106, Token (QVARSYM _)) -> Just (Reduce 1 208)
    (106, Token (BACKQUOTE _)) -> Just (Reduce 1 208)
    (106, Token (QCONSYM _)) -> Just (Reduce 1 208)
    (106, Token (CASE _)) -> Just (Reduce 1 208)
    (106, Token (OF _)) -> Just (Reduce 1 208)
    (106, Token (DO _)) -> Just (Reduce 1 208)
    (106, Token (INTEGER _)) -> Just (Reduce 1 208)
    (107, Token (WHERE _)) -> Just (Reduce 1 207)
    (107, Token (LBRACE _)) -> Just (Reduce 1 207)
    (107, Token (RBRACE _)) -> Just (Reduce 1 207)
    (107, Token (LPAREN _)) -> Just (Reduce 1 207)
    (107, Token (RPAREN _)) -> Just (Reduce 1 207)
    (107, Token (COMMA _)) -> Just (Reduce 1 207)
    (107, Token (SEMICOLON _)) -> Just (Reduce 1 207)
    (107, Token (EQUAL _)) -> Just (Reduce 1 207)
    (107, Token (PIPE _)) -> Just (Reduce 1 207)
    (107, Token (COLON_COLON _)) -> Just (Reduce 1 207)
    (107, Token (MINUS _)) -> Just (Reduce 1 207)
    (107, Token (INFIXL _)) -> Just (Reduce 1 207)
    (107, Token (INFIXR _)) -> Just (Reduce 1 207)
    (107, Token (INFIX _)) -> Just (Reduce 1 207)
    (107, Token (RARROW _)) -> Just (Reduce 1 207)
    (107, Token (QCONID _)) -> Just (Reduce 1 207)
    (107, Token (EXPORT _)) -> Just (Reduce 1 207)
    (107, Token (AS _)) -> Just (Reduce 1 207)
    (107, Token (QVARID _)) -> Just (Reduce 1 207)
    (107, Token (STRING _)) -> Just (Reduce 1 207)
    (107, Token (LARROW _)) -> Just (Reduce 1 207)
    (107, Token (LET _)) -> Just (Reduce 1 207)
    (107, Token (LAMBDA _)) -> Just (Reduce 1 207)
    (107, Token (IF _)) -> Just (Reduce 1 207)
    (107, Token (THEN _)) -> Just (Reduce 1 207)
    (107, Token (ELSE _)) -> Just (Reduce 1 207)
    (107, Token (QVARSYM _)) -> Just (Reduce 1 207)
    (107, Token (BACKQUOTE _)) -> Just (Reduce 1 207)
    (107, Token (QCONSYM _)) -> Just (Reduce 1 207)
    (107, Token (CASE _)) -> Just (Reduce 1 207)
    (107, Token (OF _)) -> Just (Reduce 1 207)
    (107, Token (DO _)) -> Just (Reduce 1 207)
    (107, Token (INTEGER _)) -> Just (Reduce 1 207)
    (108, Token (WHERE _)) -> Just (Reduce 1 209)
    (108, Token (LBRACE _)) -> Just (Reduce 1 209)
    (108, Token (RBRACE _)) -> Just (Reduce 1 209)
    (108, Token (LPAREN _)) -> Just (Reduce 1 209)
    (108, Token (RPAREN _)) -> Just (Reduce 1 209)
    (108, Token (COMMA _)) -> Just (Reduce 1 209)
    (108, Token (SEMICOLON _)) -> Just (Reduce 1 209)
    (108, Token (EQUAL _)) -> Just (Reduce 1 209)
    (108, Token (PIPE _)) -> Just (Reduce 1 209)
    (108, Token (COLON_COLON _)) -> Just (Reduce 1 209)
    (108, Token (MINUS _)) -> Just (Reduce 1 209)
    (108, Token (INFIXL _)) -> Just (Reduce 1 209)
    (108, Token (INFIXR _)) -> Just (Reduce 1 209)
    (108, Token (INFIX _)) -> Just (Reduce 1 209)
    (108, Token (RARROW _)) -> Just (Reduce 1 209)
    (108, Token (QCONID _)) -> Just (Reduce 1 209)
    (108, Token (EXPORT _)) -> Just (Reduce 1 209)
    (108, Token (AS _)) -> Just (Reduce 1 209)
    (108, Token (QVARID _)) -> Just (Reduce 1 209)
    (108, Token (STRING _)) -> Just (Reduce 1 209)
    (108, Token (LARROW _)) -> Just (Reduce 1 209)
    (108, Token (LET _)) -> Just (Reduce 1 209)
    (108, Token (LAMBDA _)) -> Just (Reduce 1 209)
    (108, Token (IF _)) -> Just (Reduce 1 209)
    (108, Token (THEN _)) -> Just (Reduce 1 209)
    (108, Token (ELSE _)) -> Just (Reduce 1 209)
    (108, Token (QVARSYM _)) -> Just (Reduce 1 209)
    (108, Token (BACKQUOTE _)) -> Just (Reduce 1 209)
    (108, Token (QCONSYM _)) -> Just (Reduce 1 209)
    (108, Token (CASE _)) -> Just (Reduce 1 209)
    (108, Token (OF _)) -> Just (Reduce 1 209)
    (108, Token (DO _)) -> Just (Reduce 1 209)
    (108, Token (INTEGER _)) -> Just (Reduce 1 209)
    (109, Token (RPAREN _)) -> Just (Shift 104)
    (110, Token (LPAREN _)) -> Just (Shift 150)
    (110, Token (LBRACKET _)) -> Just (Shift 154)
    (110, Token (EXCL _)) -> Just (Shift 110)
    (110, Token (QCONID _)) -> Just (Shift 157)
    (110, Token (EXPORT _)) -> Just (Shift 333)
    (110, Token (AS _)) -> Just (Shift 334)
    (110, Token (QVARID _)) -> Just (Shift 335)
    (111, Token (LPAREN _)) -> Just (Shift 150)
    (111, Token (LBRACKET _)) -> Just (Shift 154)
    (111, Token (EXCL _)) -> Just (Shift 110)
    (111, Token (QCONID _)) -> Just (Shift 157)
    (111, Token (EXPORT _)) -> Just (Shift 333)
    (111, Token (AS _)) -> Just (Shift 334)
    (111, Token (QVARID _)) -> Just (Shift 335)
    (112, Token (WHERE _)) -> Just (Shift 234)
    (112, Token (RBRACE _)) -> Just (Reduce 0 65)
    (112, Token (LPAREN _)) -> Just (Shift 150)
    (112, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (112, Token (DARROW _)) -> Just (Shift 115)
    (112, Token (LBRACKET _)) -> Just (Shift 154)
    (112, Token (EXCL _)) -> Just (Shift 110)
    (112, Token (QCONID _)) -> Just (Shift 157)
    (112, Token (EXPORT _)) -> Just (Shift 333)
    (112, Token (AS _)) -> Just (Shift 334)
    (112, Token (QVARID _)) -> Just (Shift 335)
    (113, Token (LPAREN _)) -> Just (Shift 150)
    (113, Token (LBRACKET _)) -> Just (Shift 154)
    (113, Token (EXCL _)) -> Just (Shift 110)
    (113, Token (QCONID _)) -> Just (Shift 157)
    (113, Token (EXPORT _)) -> Just (Shift 333)
    (113, Token (AS _)) -> Just (Shift 334)
    (113, Token (QVARID _)) -> Just (Shift 335)
    (114, Token (WHERE _)) -> Just (Shift 236)
    (114, Token (RBRACE _)) -> Just (Reduce 0 75)
    (114, Token (LPAREN _)) -> Just (Shift 150)
    (114, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (114, Token (DARROW _)) -> Just (Shift 117)
    (114, Token (LBRACKET _)) -> Just (Shift 154)
    (114, Token (EXCL _)) -> Just (Shift 110)
    (114, Token (QCONID _)) -> Just (Shift 157)
    (114, Token (EXPORT _)) -> Just (Shift 333)
    (114, Token (AS _)) -> Just (Shift 334)
    (114, Token (QVARID _)) -> Just (Shift 335)
    (115, Token (LPAREN _)) -> Just (Shift 150)
    (115, Token (LBRACKET _)) -> Just (Shift 154)
    (115, Token (EXCL _)) -> Just (Shift 110)
    (115, Token (QCONID _)) -> Just (Shift 157)
    (115, Token (EXPORT _)) -> Just (Shift 333)
    (115, Token (AS _)) -> Just (Shift 334)
    (115, Token (QVARID _)) -> Just (Shift 335)
    (116, Token (WHERE _)) -> Just (Shift 234)
    (116, Token (RBRACE _)) -> Just (Reduce 0 65)
    (116, Token (LPAREN _)) -> Just (Shift 150)
    (116, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (116, Token (LBRACKET _)) -> Just (Shift 154)
    (116, Token (EXCL _)) -> Just (Shift 110)
    (116, Token (QCONID _)) -> Just (Shift 157)
    (116, Token (EXPORT _)) -> Just (Shift 333)
    (116, Token (AS _)) -> Just (Shift 334)
    (116, Token (QVARID _)) -> Just (Shift 335)
    (117, Token (LPAREN _)) -> Just (Shift 150)
    (117, Token (LBRACKET _)) -> Just (Shift 154)
    (117, Token (EXCL _)) -> Just (Shift 110)
    (117, Token (QCONID _)) -> Just (Shift 157)
    (117, Token (EXPORT _)) -> Just (Shift 333)
    (117, Token (AS _)) -> Just (Shift 334)
    (117, Token (QVARID _)) -> Just (Shift 335)
    (118, Token (WHERE _)) -> Just (Shift 236)
    (118, Token (RBRACE _)) -> Just (Reduce 0 75)
    (118, Token (LPAREN _)) -> Just (Shift 150)
    (118, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (118, Token (LBRACKET _)) -> Just (Shift 154)
    (118, Token (EXCL _)) -> Just (Shift 110)
    (118, Token (QCONID _)) -> Just (Shift 157)
    (118, Token (EXPORT _)) -> Just (Shift 333)
    (118, Token (AS _)) -> Just (Shift 334)
    (118, Token (QVARID _)) -> Just (Shift 335)
    (119, Token (LPAREN _)) -> Just (Shift 150)
    (119, Token (LBRACKET _)) -> Just (Shift 154)
    (119, Token (EXCL _)) -> Just (Shift 110)
    (119, Token (QCONID _)) -> Just (Shift 157)
    (119, Token (EXPORT _)) -> Just (Shift 333)
    (119, Token (AS _)) -> Just (Shift 334)
    (119, Token (QVARID _)) -> Just (Shift 335)
    (120, Token (WHERE _)) -> Just (Reduce 1 100)
    (120, Token (RBRACE _)) -> Just (Reduce 1 100)
    (120, Token (LPAREN _)) -> Just (Shift 150)
    (120, Token (RPAREN _)) -> Just (Reduce 1 100)
    (120, Token (COMMA _)) -> Just (Reduce 1 100)
    (120, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (120, Token (EQUAL _)) -> Just (Reduce 1 100)
    (120, Token (DARROW _)) -> Just (Shift 122)
    (120, Token (PIPE _)) -> Just (Reduce 1 100)
    (120, Token (RARROW _)) -> Just (Shift 121)
    (120, Token (LBRACKET _)) -> Just (Shift 154)
    (120, Token (EXCL _)) -> Just (Shift 110)
    (120, Token (QCONID _)) -> Just (Shift 157)
    (120, Token (EXPORT _)) -> Just (Shift 333)
    (120, Token (AS _)) -> Just (Shift 334)
    (120, Token (QVARID _)) -> Just (Shift 335)
    (120, Token (LARROW _)) -> Just (Reduce 1 100)
    (120, Token (THEN _)) -> Just (Reduce 1 100)
    (120, Token (ELSE _)) -> Just (Reduce 1 100)
    (120, Token (OF _)) -> Just (Reduce 1 100)
    (121, Token (LPAREN _)) -> Just (Shift 150)
    (121, Token (LBRACKET _)) -> Just (Shift 154)
    (121, Token (EXCL _)) -> Just (Shift 110)
    (121, Token (QCONID _)) -> Just (Shift 157)
    (121, Token (EXPORT _)) -> Just (Shift 333)
    (121, Token (AS _)) -> Just (Shift 334)
    (121, Token (QVARID _)) -> Just (Shift 335)
    (122, Token (LPAREN _)) -> Just (Shift 150)
    (122, Token (LBRACKET _)) -> Just (Shift 154)
    (122, Token (EXCL _)) -> Just (Shift 110)
    (122, Token (QCONID _)) -> Just (Shift 157)
    (122, Token (EXPORT _)) -> Just (Shift 333)
    (122, Token (AS _)) -> Just (Shift 334)
    (122, Token (QVARID _)) -> Just (Shift 335)
    (123, Token (WHERE _)) -> Just (Reduce 1 100)
    (123, Token (RBRACE _)) -> Just (Reduce 1 100)
    (123, Token (LPAREN _)) -> Just (Shift 150)
    (123, Token (RPAREN _)) -> Just (Reduce 1 100)
    (123, Token (COMMA _)) -> Just (Reduce 1 100)
    (123, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (123, Token (EQUAL _)) -> Just (Reduce 1 100)
    (123, Token (PIPE _)) -> Just (Reduce 1 100)
    (123, Token (RARROW _)) -> Just (Shift 121)
    (123, Token (LBRACKET _)) -> Just (Shift 154)
    (123, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (123, Token (EXCL _)) -> Just (Shift 110)
    (123, Token (QCONID _)) -> Just (Shift 157)
    (123, Token (EXPORT _)) -> Just (Shift 333)
    (123, Token (AS _)) -> Just (Shift 334)
    (123, Token (QVARID _)) -> Just (Shift 335)
    (123, Token (LARROW _)) -> Just (Reduce 1 100)
    (123, Token (THEN _)) -> Just (Reduce 1 100)
    (123, Token (ELSE _)) -> Just (Reduce 1 100)
    (123, Token (OF _)) -> Just (Reduce 1 100)
    (124, Token (LPAREN _)) -> Just (Shift 150)
    (124, Token (LBRACKET _)) -> Just (Shift 154)
    (124, Token (EXCL _)) -> Just (Shift 110)
    (124, Token (QCONID _)) -> Just (Shift 157)
    (124, Token (EXPORT _)) -> Just (Shift 333)
    (124, Token (AS _)) -> Just (Shift 334)
    (124, Token (QVARID _)) -> Just (Shift 335)
    (125, Token (RBRACE _)) -> Just (Reduce 0 119)
    (125, Token (LPAREN _)) -> Just (Shift 150)
    (125, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (125, Token (EQUAL _)) -> Just (Shift 128)
    (125, Token (DERIVING _)) -> Just (Reduce 0 119)
    (125, Token (DARROW _)) -> Just (Shift 126)
    (125, Token (LBRACKET _)) -> Just (Shift 154)
    (125, Token (EXCL _)) -> Just (Shift 110)
    (125, Token (QCONID _)) -> Just (Shift 157)
    (125, Token (EXPORT _)) -> Just (Shift 333)
    (125, Token (AS _)) -> Just (Shift 334)
    (125, Token (QVARID _)) -> Just (Shift 335)
    (126, Token (LPAREN _)) -> Just (Shift 150)
    (126, Token (LBRACKET _)) -> Just (Shift 154)
    (126, Token (EXCL _)) -> Just (Shift 110)
    (126, Token (QCONID _)) -> Just (Shift 157)
    (126, Token (EXPORT _)) -> Just (Shift 333)
    (126, Token (AS _)) -> Just (Shift 334)
    (126, Token (QVARID _)) -> Just (Shift 335)
    (127, Token (RBRACE _)) -> Just (Reduce 0 119)
    (127, Token (LPAREN _)) -> Just (Shift 150)
    (127, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (127, Token (EQUAL _)) -> Just (Shift 128)
    (127, Token (DERIVING _)) -> Just (Reduce 0 119)
    (127, Token (LBRACKET _)) -> Just (Shift 154)
    (127, Token (EXCL _)) -> Just (Shift 110)
    (127, Token (QCONID _)) -> Just (Shift 157)
    (127, Token (EXPORT _)) -> Just (Shift 333)
    (127, Token (AS _)) -> Just (Shift 334)
    (127, Token (QVARID _)) -> Just (Shift 335)
    (128, Token (LPAREN _)) -> Just (Shift 150)
    (128, Token (LBRACKET _)) -> Just (Shift 154)
    (128, Token (EXCL _)) -> Just (Shift 110)
    (128, Token (QCONID _)) -> Just (Shift 157)
    (128, Token (EXPORT _)) -> Just (Shift 333)
    (128, Token (AS _)) -> Just (Shift 334)
    (128, Token (QVARID _)) -> Just (Shift 335)
    (129, Token (LPAREN _)) -> Just (Shift 150)
    (129, Token (LBRACKET _)) -> Just (Shift 154)
    (129, Token (EXCL _)) -> Just (Shift 110)
    (129, Token (QCONID _)) -> Just (Shift 157)
    (129, Token (EXPORT _)) -> Just (Shift 333)
    (129, Token (AS _)) -> Just (Shift 334)
    (129, Token (QVARID _)) -> Just (Shift 335)
    (130, Token (LPAREN _)) -> Just (Shift 155)
    (130, Token (QCONID _)) -> Just (Shift 157)
    (131, Token (RBRACE _)) -> Just (Reduce 1 123)
    (131, Token (LPAREN _)) -> Just (Shift 150)
    (131, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (131, Token (DERIVING _)) -> Just (Reduce 1 123)
    (131, Token (PIPE _)) -> Just (Reduce 1 123)
    (131, Token (LBRACKET _)) -> Just (Shift 154)
    (131, Token (EXCL _)) -> Just (Shift 110)
    (131, Token (QCONID _)) -> Just (Shift 157)
    (131, Token (EXPORT _)) -> Just (Shift 333)
    (131, Token (AS _)) -> Just (Shift 334)
    (131, Token (QVARID _)) -> Just (Shift 335)
    (131, Token (BACKQUOTE _)) -> Just (Shift 347)
    (131, Token (QCONSYM _)) -> Just (Shift 349)
    (132, Token (LPAREN _)) -> Just (Shift 150)
    (132, Token (LBRACKET _)) -> Just (Shift 154)
    (132, Token (EXCL _)) -> Just (Shift 110)
    (132, Token (QCONID _)) -> Just (Shift 157)
    (132, Token (EXPORT _)) -> Just (Shift 333)
    (132, Token (AS _)) -> Just (Shift 334)
    (132, Token (QVARID _)) -> Just (Shift 335)
    (133, Token (RBRACE _)) -> Just (Reduce 3 124)
    (133, Token (LPAREN _)) -> Just (Shift 150)
    (133, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (133, Token (DERIVING _)) -> Just (Reduce 3 124)
    (133, Token (PIPE _)) -> Just (Reduce 3 124)
    (133, Token (LBRACKET _)) -> Just (Shift 154)
    (133, Token (EXCL _)) -> Just (Shift 110)
    (133, Token (QCONID _)) -> Just (Shift 157)
    (133, Token (EXPORT _)) -> Just (Shift 333)
    (133, Token (AS _)) -> Just (Shift 334)
    (133, Token (QVARID _)) -> Just (Shift 335)
    (134, Token (LPAREN _)) -> Just (Shift 150)
    (134, Token (LBRACKET _)) -> Just (Shift 154)
    (134, Token (EXCL _)) -> Just (Shift 110)
    (134, Token (QCONID _)) -> Just (Shift 157)
    (134, Token (EXPORT _)) -> Just (Shift 333)
    (134, Token (AS _)) -> Just (Shift 334)
    (134, Token (QVARID _)) -> Just (Shift 335)
    (135, Token (RBRACE _)) -> Just (Reduce 1 100)
    (135, Token (LPAREN _)) -> Just (Shift 150)
    (135, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (135, Token (DARROW _)) -> Just (Shift 140)
    (135, Token (RARROW _)) -> Just (Shift 121)
    (135, Token (LBRACKET _)) -> Just (Shift 154)
    (135, Token (EXCL _)) -> Just (Shift 110)
    (135, Token (QCONID _)) -> Just (Shift 157)
    (135, Token (EXPORT _)) -> Just (Shift 333)
    (135, Token (AS _)) -> Just (Shift 334)
    (135, Token (QVARID _)) -> Just (Shift 335)
    (136, Token (LPAREN _)) -> Just (Shift 150)
    (136, Token (LBRACKET _)) -> Just (Shift 154)
    (136, Token (EXCL _)) -> Just (Shift 110)
    (136, Token (QCONID _)) -> Just (Shift 157)
    (136, Token (EXPORT _)) -> Just (Shift 333)
    (136, Token (AS _)) -> Just (Shift 334)
    (136, Token (QVARID _)) -> Just (Shift 335)
    (137, Token (LPAREN _)) -> Just (Shift 150)
    (137, Token (LBRACKET _)) -> Just (Shift 154)
    (137, Token (EXCL _)) -> Just (Shift 110)
    (137, Token (QCONID _)) -> Just (Shift 157)
    (137, Token (EXPORT _)) -> Just (Shift 333)
    (137, Token (AS _)) -> Just (Shift 334)
    (137, Token (QVARID _)) -> Just (Shift 335)
    (138, Token (LPAREN _)) -> Just (Shift 150)
    (138, Token (LBRACKET _)) -> Just (Shift 154)
    (138, Token (EXCL _)) -> Just (Shift 110)
    (138, Token (QCONID _)) -> Just (Shift 157)
    (138, Token (EXPORT _)) -> Just (Shift 333)
    (138, Token (AS _)) -> Just (Shift 334)
    (138, Token (QVARID _)) -> Just (Shift 335)
    (139, Token (LPAREN _)) -> Just (Shift 150)
    (139, Token (LBRACKET _)) -> Just (Shift 154)
    (139, Token (EXCL _)) -> Just (Shift 110)
    (139, Token (QCONID _)) -> Just (Shift 157)
    (139, Token (EXPORT _)) -> Just (Shift 333)
    (139, Token (AS _)) -> Just (Shift 334)
    (139, Token (QVARID _)) -> Just (Shift 335)
    (140, Token (LPAREN _)) -> Just (Shift 150)
    (140, Token (LBRACKET _)) -> Just (Shift 154)
    (140, Token (EXCL _)) -> Just (Shift 110)
    (140, Token (QCONID _)) -> Just (Shift 157)
    (140, Token (EXPORT _)) -> Just (Shift 333)
    (140, Token (AS _)) -> Just (Shift 334)
    (140, Token (QVARID _)) -> Just (Shift 335)
    (141, Token (LPAREN _)) -> Just (Shift 150)
    (141, Token (LBRACKET _)) -> Just (Shift 154)
    (141, Token (EXCL _)) -> Just (Shift 110)
    (141, Token (QCONID _)) -> Just (Shift 157)
    (141, Token (EXPORT _)) -> Just (Shift 333)
    (141, Token (AS _)) -> Just (Shift 334)
    (141, Token (QVARID _)) -> Just (Shift 335)
    (142, Token (LPAREN _)) -> Just (Shift 150)
    (142, Token (LBRACKET _)) -> Just (Shift 154)
    (142, Token (EXCL _)) -> Just (Shift 110)
    (142, Token (QCONID _)) -> Just (Shift 157)
    (142, Token (EXPORT _)) -> Just (Shift 333)
    (142, Token (AS _)) -> Just (Shift 334)
    (142, Token (QVARID _)) -> Just (Shift 335)
    (143, Token (LBRACE _)) -> Just (Shift 97)
    (143, Token (LPAREN _)) -> Just (Shift 150)
    (143, Token (LBRACKET _)) -> Just (Shift 154)
    (143, Token (EXCL _)) -> Just (Shift 110)
    (143, Token (QCONID _)) -> Just (Shift 157)
    (143, Token (EXPORT _)) -> Just (Shift 333)
    (143, Token (AS _)) -> Just (Shift 334)
    (143, Token (QVARID _)) -> Just (Shift 335)
    (144, Token (LPAREN _)) -> Just (Shift 150)
    (144, Token (LBRACKET _)) -> Just (Shift 154)
    (144, Token (EXCL _)) -> Just (Shift 110)
    (144, Token (QCONID _)) -> Just (Shift 157)
    (144, Token (EXPORT _)) -> Just (Shift 333)
    (144, Token (AS _)) -> Just (Shift 334)
    (144, Token (QVARID _)) -> Just (Shift 335)
    (145, Token (LPAREN _)) -> Just (Shift 150)
    (145, Token (EQUAL _)) -> Just (Shift 130)
    (145, Token (DARROW _)) -> Just (Shift 147)
    (145, Token (LBRACKET _)) -> Just (Shift 154)
    (145, Token (EXCL _)) -> Just (Shift 110)
    (145, Token (QCONID _)) -> Just (Shift 157)
    (145, Token (EXPORT _)) -> Just (Shift 333)
    (145, Token (AS _)) -> Just (Shift 334)
    (145, Token (QVARID _)) -> Just (Shift 335)
    (146, Token (LPAREN _)) -> Just (Shift 150)
    (146, Token (LBRACKET _)) -> Just (Shift 154)
    (146, Token (EXCL _)) -> Just (Shift 110)
    (146, Token (QCONID _)) -> Just (Shift 157)
    (146, Token (EXPORT _)) -> Just (Shift 333)
    (146, Token (AS _)) -> Just (Shift 334)
    (146, Token (QVARID _)) -> Just (Shift 335)
    (147, Token (LPAREN _)) -> Just (Shift 150)
    (147, Token (LBRACKET _)) -> Just (Shift 154)
    (147, Token (EXCL _)) -> Just (Shift 110)
    (147, Token (QCONID _)) -> Just (Shift 157)
    (147, Token (EXPORT _)) -> Just (Shift 333)
    (147, Token (AS _)) -> Just (Shift 334)
    (147, Token (QVARID _)) -> Just (Shift 335)
    (148, Token (LPAREN _)) -> Just (Shift 150)
    (148, Token (EQUAL _)) -> Just (Shift 136)
    (148, Token (LBRACKET _)) -> Just (Shift 154)
    (148, Token (EXCL _)) -> Just (Shift 110)
    (148, Token (QCONID _)) -> Just (Shift 157)
    (148, Token (EXPORT _)) -> Just (Shift 333)
    (148, Token (AS _)) -> Just (Shift 334)
    (148, Token (QVARID _)) -> Just (Shift 335)
    (149, Token (LPAREN _)) -> Just (Shift 150)
    (149, Token (EQUAL _)) -> Just (Shift 130)
    (149, Token (LBRACKET _)) -> Just (Shift 154)
    (149, Token (EXCL _)) -> Just (Shift 110)
    (149, Token (QCONID _)) -> Just (Shift 157)
    (149, Token (EXPORT _)) -> Just (Shift 333)
    (149, Token (AS _)) -> Just (Shift 334)
    (149, Token (QVARID _)) -> Just (Shift 335)
    (150, Token (LPAREN _)) -> Just (Shift 150)
    (150, Token (RPAREN _)) -> Just (Shift 325)
    (150, Token (COMMA _)) -> Just (Shift 338)
    (150, Token (RARROW _)) -> Just (Shift 328)
    (150, Token (LBRACKET _)) -> Just (Shift 154)
    (150, Token (EXCL _)) -> Just (Shift 110)
    (150, Token (QCONID _)) -> Just (Shift 157)
    (150, Token (EXPORT _)) -> Just (Shift 333)
    (150, Token (AS _)) -> Just (Shift 334)
    (150, Token (QVARID _)) -> Just (Shift 335)
    (150, Token (QCONSYM _)) -> Just (Shift 158)
    (151, Token (LPAREN _)) -> Just (Shift 150)
    (151, Token (RPAREN _)) -> Just (Shift 180)
    (151, Token (LBRACKET _)) -> Just (Shift 154)
    (151, Token (EXCL _)) -> Just (Shift 110)
    (151, Token (QCONID _)) -> Just (Shift 157)
    (151, Token (EXPORT _)) -> Just (Shift 333)
    (151, Token (AS _)) -> Just (Shift 334)
    (151, Token (QVARID _)) -> Just (Shift 335)
    (152, Token (LPAREN _)) -> Just (Shift 150)
    (152, Token (LBRACKET _)) -> Just (Shift 154)
    (152, Token (EXCL _)) -> Just (Shift 110)
    (152, Token (QCONID _)) -> Just (Shift 157)
    (152, Token (EXPORT _)) -> Just (Shift 333)
    (152, Token (AS _)) -> Just (Shift 334)
    (152, Token (QVARID _)) -> Just (Shift 335)
    (153, Token (LPAREN _)) -> Just (Shift 150)
    (153, Token (LBRACKET _)) -> Just (Shift 154)
    (153, Token (EXCL _)) -> Just (Shift 110)
    (153, Token (QCONID _)) -> Just (Shift 157)
    (153, Token (EXPORT _)) -> Just (Shift 333)
    (153, Token (AS _)) -> Just (Shift 334)
    (153, Token (QVARID _)) -> Just (Shift 335)
    (154, Token (LPAREN _)) -> Just (Shift 150)
    (154, Token (LBRACKET _)) -> Just (Shift 154)
    (154, Token (RBRACKET _)) -> Just (Shift 329)
    (154, Token (EXCL _)) -> Just (Shift 110)
    (154, Token (QCONID _)) -> Just (Shift 157)
    (154, Token (EXPORT _)) -> Just (Shift 333)
    (154, Token (AS _)) -> Just (Shift 334)
    (154, Token (QVARID _)) -> Just (Shift 335)
    (155, Token (QCONSYM _)) -> Just (Shift 158)
    (156, Token (WHERE _)) -> Just (Reduce 3 213)
    (156, Token (LBRACE _)) -> Just (Reduce 3 213)
    (156, Token (RBRACE _)) -> Just (Reduce 3 213)
    (156, Token (LPAREN _)) -> Just (Reduce 3 213)
    (156, Token (RPAREN _)) -> Just (Reduce 3 213)
    (156, Token (COMMA _)) -> Just (Reduce 3 213)
    (156, Token (SEMICOLON _)) -> Just (Reduce 3 213)
    (156, Token (EQUAL _)) -> Just (Reduce 3 213)
    (156, Token (DERIVING _)) -> Just (Reduce 3 213)
    (156, Token (DARROW _)) -> Just (Reduce 3 213)
    (156, Token (PIPE _)) -> Just (Reduce 3 213)
    (156, Token (COLON_COLON _)) -> Just (Reduce 3 213)
    (156, Token (MINUS _)) -> Just (Reduce 3 213)
    (156, Token (INFIXL _)) -> Just (Reduce 3 213)
    (156, Token (INFIXR _)) -> Just (Reduce 3 213)
    (156, Token (INFIX _)) -> Just (Reduce 3 213)
    (156, Token (RARROW _)) -> Just (Reduce 3 213)
    (156, Token (LBRACKET _)) -> Just (Reduce 3 213)
    (156, Token (RBRACKET _)) -> Just (Reduce 3 213)
    (156, Token (EXCL _)) -> Just (Reduce 3 213)
    (156, Token (QCONID _)) -> Just (Reduce 3 213)
    (156, Token (EXPORT _)) -> Just (Reduce 3 213)
    (156, Token (AS _)) -> Just (Reduce 3 213)
    (156, Token (QVARID _)) -> Just (Reduce 3 213)
    (156, Token (LARROW _)) -> Just (Reduce 3 213)
    (156, Token (THEN _)) -> Just (Reduce 3 213)
    (156, Token (ELSE _)) -> Just (Reduce 3 213)
    (156, Token (QVARSYM _)) -> Just (Reduce 3 213)
    (156, Token (BACKQUOTE _)) -> Just (Reduce 3 213)
    (156, Token (QCONSYM _)) -> Just (Reduce 3 213)
    (156, Token (OF _)) -> Just (Reduce 3 213)
    (156, Token (INTEGER _)) -> Just (Reduce 3 213)
    (157, Token (WHERE _)) -> Just (Reduce 1 212)
    (157, Token (LBRACE _)) -> Just (Reduce 1 212)
    (157, Token (RBRACE _)) -> Just (Reduce 1 212)
    (157, Token (LPAREN _)) -> Just (Reduce 1 212)
    (157, Token (RPAREN _)) -> Just (Reduce 1 212)
    (157, Token (COMMA _)) -> Just (Reduce 1 212)
    (157, Token (SEMICOLON _)) -> Just (Reduce 1 212)
    (157, Token (EQUAL _)) -> Just (Reduce 1 212)
    (157, Token (DERIVING _)) -> Just (Reduce 1 212)
    (157, Token (DARROW _)) -> Just (Reduce 1 212)
    (157, Token (PIPE _)) -> Just (Reduce 1 212)
    (157, Token (COLON_COLON _)) -> Just (Reduce 1 212)
    (157, Token (MINUS _)) -> Just (Reduce 1 212)
    (157, Token (INFIXL _)) -> Just (Reduce 1 212)
    (157, Token (INFIXR _)) -> Just (Reduce 1 212)
    (157, Token (INFIX _)) -> Just (Reduce 1 212)
    (157, Token (RARROW _)) -> Just (Reduce 1 212)
    (157, Token (LBRACKET _)) -> Just (Reduce 1 212)
    (157, Token (RBRACKET _)) -> Just (Reduce 1 212)
    (157, Token (EXCL _)) -> Just (Reduce 1 212)
    (157, Token (QCONID _)) -> Just (Reduce 1 212)
    (157, Token (EXPORT _)) -> Just (Reduce 1 212)
    (157, Token (AS _)) -> Just (Reduce 1 212)
    (157, Token (QVARID _)) -> Just (Reduce 1 212)
    (157, Token (LARROW _)) -> Just (Reduce 1 212)
    (157, Token (THEN _)) -> Just (Reduce 1 212)
    (157, Token (ELSE _)) -> Just (Reduce 1 212)
    (157, Token (QVARSYM _)) -> Just (Reduce 1 212)
    (157, Token (BACKQUOTE _)) -> Just (Reduce 1 212)
    (157, Token (QCONSYM _)) -> Just (Reduce 1 212)
    (157, Token (OF _)) -> Just (Reduce 1 212)
    (157, Token (INTEGER _)) -> Just (Reduce 1 212)
    (158, Token (RPAREN _)) -> Just (Shift 156)
    (159, Token (RPAREN _)) -> Just (Reduce 3 24)
    (160, Token (RPAREN _)) -> Just (Reduce 1 23)
    (160, Token (COMMA _)) -> Just (Shift 92)
    (161, Token (RPAREN _)) -> Just (Reduce 3 17)
    (162, Token (RPAREN _)) -> Just (Reduce 1 16)
    (162, Token (COMMA _)) -> Just (Shift 89)
    (163, Token (RPAREN _)) -> Just (Reduce 3 20)
    (163, Token (COMMA _)) -> Just (Reduce 3 20)
    (164, Token (RPAREN _)) -> Just (Reduce 4 21)
    (164, Token (COMMA _)) -> Just (Reduce 4 21)
    (165, Token (RPAREN _)) -> Just (Reduce 4 22)
    (165, Token (COMMA _)) -> Just (Reduce 4 22)
    (166, Token (RPAREN _)) -> Just (Shift 164)
    (167, Token (RPAREN _)) -> Just (Reduce 1 18)
    (167, Token (COMMA _)) -> Just (Reduce 1 18)
    (168, Token (LPAREN _)) -> Just (Shift 93)
    (168, Token (RPAREN _)) -> Just (Reduce 1 19)
    (168, Token (COMMA _)) -> Just (Reduce 1 19)
    (169, Token (RPAREN _)) -> Just (Shift 165)
    (170, Token (RPAREN _)) -> Just (Reduce 1 25)
    (170, Token (COMMA _)) -> Just (Reduce 1 25)
    (171, Token (RPAREN _)) -> Just (Reduce 1 26)
    (171, Token (COMMA _)) -> Just (Reduce 1 26)
    (172, Token (RPAREN _)) -> Just (Shift 176)
    (172, Token (QCONID _)) -> Just (Shift 227)
    (173, Token (RPAREN _)) -> Just (Shift 177)
    (173, Token (QCONID _)) -> Just (Shift 227)
    (174, Token (RPAREN _)) -> Just (Shift 178)
    (174, Token (QCONID _)) -> Just (Shift 227)
    (175, Token (RPAREN _)) -> Just (Shift 179)
    (175, Token (QCONID _)) -> Just (Shift 227)
    (176, Token (RBRACE _)) -> Just (Reduce 6 35)
    (176, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (177, Token (RBRACE _)) -> Just (Reduce 8 39)
    (177, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (178, Token (RBRACE _)) -> Just (Reduce 8 47)
    (178, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (179, Token (RBRACE _)) -> Just (Reduce 6 43)
    (179, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (180, Token (RBRACE _)) -> Just (Reduce 3 53)
    (180, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (181, Token (RBRACE _)) -> Just (Reduce 8 31)
    (181, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (182, Token (RBRACE _)) -> Just (Reduce 7 30)
    (182, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (183, Token (RBRACE _)) -> Just (Reduce 7 36)
    (183, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (184, Token (RBRACE _)) -> Just (Reduce 9 40)
    (184, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (185, Token (RBRACE _)) -> Just (Reduce 9 48)
    (185, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (186, Token (RBRACE _)) -> Just (Reduce 7 44)
    (186, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (187, Token (RBRACE _)) -> Just (Reduce 4 54)
    (187, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (188, Token (QCONID _)) -> Just (Reduce 0 224)
    (188, Token (QUALIFIED _)) -> Just (Shift 220)
    (189, Token (LPAREN _)) -> Just (Shift 90)
    (190, Token (LPAREN _)) -> Just (Shift 172)
    (190, Token (QCONID _)) -> Just (Shift 227)
    (191, Token (LPAREN _)) -> Just (Shift 173)
    (191, Token (QCONID _)) -> Just (Shift 227)
    (192, Token (LPAREN _)) -> Just (Shift 174)
    (192, Token (QCONID _)) -> Just (Shift 227)
    (193, Token (LPAREN _)) -> Just (Shift 175)
    (193, Token (QCONID _)) -> Just (Shift 227)
    (194, Token (LPAREN _)) -> Just (Shift 151)
    (195, Token (IMPORT _)) -> Just (Shift 240)
    (195, Token (EXPORT _)) -> Just (Shift 241)
    (196, Token (RBRACE _)) -> Just (Reduce 0 222)
    (196, Token (LPAREN _)) -> Just (Reduce 0 222)
    (196, Token (SEMICOLON _)) -> Just (Reduce 0 222)
    (196, Token (HIDING _)) -> Just (Reduce 0 222)
    (196, Token (AS _)) -> Just (Shift 9)
    (197, Token (RPAREN _)) -> Just (Shift 181)
    (198, Token (RPAREN _)) -> Just (Shift 182)
    (199, Token (RBRACE _)) -> Just (Reduce 4 29)
    (199, Token (LPAREN _)) -> Just (Shift 91)
    (199, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (199, Token (HIDING _)) -> Just (Shift 189)
    (200, Token (RBRACE _)) -> Just (Reduce 4 32)
    (200, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (201, Token (RBRACE _)) -> Just (Reduce 3 33)
    (201, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (201, Token (DERIVING _)) -> Just (Shift 190)
    (202, Token (RBRACE _)) -> Just (Reduce 5 37)
    (202, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (202, Token (DERIVING _)) -> Just (Shift 191)
    (203, Token (RBRACE _)) -> Just (Reduce 5 34)
    (203, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (204, Token (RBRACE _)) -> Just (Reduce 7 38)
    (204, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (205, Token (RBRACE _)) -> Just (Reduce 7 46)
    (205, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (206, Token (RBRACE _)) -> Just (Reduce 5 42)
    (206, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (207, Token (RPAREN _)) -> Just (Shift 183)
    (208, Token (RPAREN _)) -> Just (Shift 184)
    (209, Token (RPAREN _)) -> Just (Shift 185)
    (210, Token (RPAREN _)) -> Just (Shift 186)
    (211, Token (RBRACE _)) -> Just (Reduce 5 45)
    (211, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (211, Token (DERIVING _)) -> Just (Shift 192)
    (212, Token (RBRACE _)) -> Just (Reduce 3 41)
    (212, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (212, Token (DERIVING _)) -> Just (Shift 193)
    (213, Token (RBRACE _)) -> Just (Reduce 5 50)
    (213, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (214, Token (RBRACE _)) -> Just (Reduce 3 49)
    (214, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (215, Token (RBRACE _)) -> Just (Reduce 5 52)
    (215, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (216, Token (RBRACE _)) -> Just (Reduce 3 51)
    (216, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (217, Token (RPAREN _)) -> Just (Shift 187)
    (218, Token (RBRACE _)) -> Just (Reduce 2 55)
    (218, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (219, Token (RBRACE _)) -> Just (Reduce 1 56)
    (219, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (220, Token (QCONID _)) -> Just (Reduce 1 225)
    (221, Token (RBRACE _)) -> Just (Reduce 2 223)
    (221, Token (LPAREN _)) -> Just (Reduce 2 223)
    (221, Token (SEMICOLON _)) -> Just (Reduce 2 223)
    (221, Token (HIDING _)) -> Just (Reduce 2 223)
    (222, Token (WHERE _)) -> Just (Reduce 1 102)
    (222, Token (LBRACE _)) -> Just (Reduce 1 102)
    (222, Token (RBRACE _)) -> Just (Reduce 1 102)
    (222, Token (LPAREN _)) -> Just (Reduce 1 102)
    (222, Token (RPAREN _)) -> Just (Reduce 1 102)
    (222, Token (COMMA _)) -> Just (Reduce 1 102)
    (222, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (222, Token (EQUAL _)) -> Just (Reduce 1 102)
    (222, Token (DERIVING _)) -> Just (Reduce 1 102)
    (222, Token (DARROW _)) -> Just (Reduce 1 102)
    (222, Token (PIPE _)) -> Just (Reduce 1 102)
    (222, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (222, Token (MINUS _)) -> Just (Reduce 1 102)
    (222, Token (INFIXL _)) -> Just (Reduce 1 102)
    (222, Token (INFIXR _)) -> Just (Reduce 1 102)
    (222, Token (INFIX _)) -> Just (Reduce 1 102)
    (222, Token (RARROW _)) -> Just (Reduce 1 102)
    (222, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (222, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (222, Token (EXCL _)) -> Just (Reduce 1 102)
    (222, Token (QCONID _)) -> Just (Reduce 1 102)
    (222, Token (EXPORT _)) -> Just (Reduce 1 102)
    (222, Token (AS _)) -> Just (Reduce 1 102)
    (222, Token (QVARID _)) -> Just (Reduce 1 102)
    (222, Token (LARROW _)) -> Just (Reduce 1 102)
    (222, Token (THEN _)) -> Just (Reduce 1 102)
    (222, Token (ELSE _)) -> Just (Reduce 1 102)
    (222, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (222, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (222, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (222, Token (OF _)) -> Just (Reduce 1 102)
    (222, Token (INTEGER _)) -> Just (Reduce 1 102)
    (223, Token (WHERE _)) -> Just (Reduce 2 103)
    (223, Token (LBRACE _)) -> Just (Reduce 2 103)
    (223, Token (RBRACE _)) -> Just (Reduce 2 103)
    (223, Token (LPAREN _)) -> Just (Reduce 2 103)
    (223, Token (RPAREN _)) -> Just (Reduce 2 103)
    (223, Token (COMMA _)) -> Just (Reduce 2 103)
    (223, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (223, Token (EQUAL _)) -> Just (Reduce 2 103)
    (223, Token (DERIVING _)) -> Just (Reduce 2 103)
    (223, Token (DARROW _)) -> Just (Reduce 2 103)
    (223, Token (PIPE _)) -> Just (Reduce 2 103)
    (223, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (223, Token (MINUS _)) -> Just (Reduce 2 103)
    (223, Token (INFIXL _)) -> Just (Reduce 2 103)
    (223, Token (INFIXR _)) -> Just (Reduce 2 103)
    (223, Token (INFIX _)) -> Just (Reduce 2 103)
    (223, Token (RARROW _)) -> Just (Reduce 2 103)
    (223, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (223, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (223, Token (EXCL _)) -> Just (Reduce 2 103)
    (223, Token (QCONID _)) -> Just (Reduce 2 103)
    (223, Token (EXPORT _)) -> Just (Reduce 2 103)
    (223, Token (AS _)) -> Just (Reduce 2 103)
    (223, Token (QVARID _)) -> Just (Reduce 2 103)
    (223, Token (LARROW _)) -> Just (Reduce 2 103)
    (223, Token (THEN _)) -> Just (Reduce 2 103)
    (223, Token (ELSE _)) -> Just (Reduce 2 103)
    (223, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (223, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (223, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (223, Token (OF _)) -> Just (Reduce 2 103)
    (223, Token (INTEGER _)) -> Just (Reduce 2 103)
    (224, Token (WHERE _)) -> Just (Reduce 3 101)
    (224, Token (RBRACE _)) -> Just (Reduce 3 101)
    (224, Token (RPAREN _)) -> Just (Reduce 3 101)
    (224, Token (COMMA _)) -> Just (Reduce 3 101)
    (224, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (224, Token (EQUAL _)) -> Just (Reduce 3 101)
    (224, Token (PIPE _)) -> Just (Reduce 3 101)
    (224, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (224, Token (LARROW _)) -> Just (Reduce 3 101)
    (224, Token (THEN _)) -> Just (Reduce 3 101)
    (224, Token (ELSE _)) -> Just (Reduce 3 101)
    (224, Token (OF _)) -> Just (Reduce 3 101)
    (225, Token (RBRACE _)) -> Just (Reduce 2 120)
    (225, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (225, Token (DERIVING _)) -> Just (Reduce 2 120)
    (226, Token (QCONID _)) -> Just (Shift 227)
    (227, Token (RBRACE _)) -> Just (Reduce 1 134)
    (227, Token (RPAREN _)) -> Just (Reduce 1 134)
    (227, Token (COMMA _)) -> Just (Reduce 1 134)
    (227, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (228, Token (RPAREN _)) -> Just (Reduce 1 132)
    (228, Token (COMMA _)) -> Just (Shift 226)
    (229, Token (RPAREN _)) -> Just (Reduce 3 133)
    (230, Token (RBRACE _)) -> Just (Reduce 7 128)
    (230, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (230, Token (DERIVING _)) -> Just (Reduce 7 128)
    (231, Token (COLON_COLON _)) -> Just (Shift 142)
    (232, Token (RBRACE _)) -> Just (Shift 230)
    (233, Token (RBRACE _)) -> Just (Reduce 3 127)
    (233, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (233, Token (DERIVING _)) -> Just (Reduce 3 127)
    (234, Token (LBRACE _)) -> Just (Shift 77)
    (235, Token (RBRACE _)) -> Just (Reduce 2 66)
    (235, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (236, Token (LBRACE _)) -> Just (Shift 80)
    (237, Token (RBRACE _)) -> Just (Reduce 2 76)
    (237, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (238, Token (RPAREN _)) -> Just (Reduce 1 98)
    (238, Token (COMMA _)) -> Just (Shift 152)
    (239, Token (RPAREN _)) -> Just (Reduce 3 99)
    (240, Token (EXPORT _)) -> Just (Shift 354)
    (240, Token (AS _)) -> Just (Shift 355)
    (240, Token (QVARID _)) -> Just (Shift 356)
    (241, Token (EXPORT _)) -> Just (Shift 354)
    (241, Token (AS _)) -> Just (Shift 355)
    (241, Token (QVARID _)) -> Just (Shift 356)
    (242, Token (COLON_COLON _)) -> Just (Shift 137)
    (243, Token (COLON_COLON _)) -> Just (Shift 138)
    (244, Token (COLON_COLON _)) -> Just (Shift 139)
    (245, Token (RBRACE _)) -> Just (Reduce 6 135)
    (245, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (246, Token (RBRACE _)) -> Just (Reduce 7 136)
    (246, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (247, Token (RBRACE _)) -> Just (Reduce 6 137)
    (247, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (248, Token (EXPORT _)) -> Just (Shift 358)
    (248, Token (AS _)) -> Just (Shift 359)
    (248, Token (QVARID _)) -> Just (Shift 360)
    (248, Token (STRING _)) -> Just (Shift 357)
    (249, Token (STRING _)) -> Just (Shift 361)
    (250, Token (STRING _)) -> Just (Shift 357)
    (251, Token (LBRACE _)) -> Just (Shift 75)
    (252, Token (LBRACE _)) -> Just (Shift 75)
    (253, Token (RBRACE _)) -> Just (Reduce 5 62)
    (253, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (254, Token (RBRACE _)) -> Just (Reduce 5 64)
    (254, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (255, Token (RBRACE _)) -> Just (Reduce 1 60)
    (255, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (256, Token (WHERE _)) -> Just (Shift 251)
    (256, Token (RBRACE _)) -> Just (Reduce 3 61)
    (256, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (257, Token (WHERE _)) -> Just (Shift 252)
    (257, Token (RBRACE _)) -> Just (Reduce 3 63)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (258, Token (LBRACE _)) -> Just (Shift 75)
    (259, Token (LBRACE _)) -> Just (Shift 75)
    (260, Token (LBRACE _)) -> Just (Shift 75)
    (261, Token (LBRACE _)) -> Just (Shift 75)
    (262, Token (LBRACE _)) -> Just (Shift 75)
    (263, Token (LBRACE _)) -> Just (Shift 75)
    (264, Token (LBRACE _)) -> Just (Shift 75)
    (265, Token (LBRACE _)) -> Just (Shift 75)
    (266, Token (RBRACE _)) -> Just (Reduce 3 57)
    (266, Token (COMMA _)) -> Just (Reduce 3 57)
    (266, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (266, Token (EQUAL _)) -> Just (Reduce 3 57)
    (266, Token (IN _)) -> Just (Reduce 3 57)
    (267, Token (RBRACE _)) -> Just (Shift 266)
    (268, Token (RBRACE _)) -> Just (Reduce 1 58)
    (268, Token (SEMICOLON _)) -> Just (Shift 76)
    (269, Token (RBRACE _)) -> Just (Reduce 3 59)
    (270, Token (RBRACE _)) -> Just (Reduce 5 87)
    (270, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (271, Token (RBRACE _)) -> Just (Reduce 3 86)
    (271, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (272, Token (COLON_COLON _)) -> Just (Shift 134)
    (273, Token (COMMA _)) -> Just (Reduce 0 231)
    (273, Token (MINUS _)) -> Just (Reduce 0 231)
    (273, Token (QCONID _)) -> Just (Reduce 0 231)
    (273, Token (EXPORT _)) -> Just (Reduce 0 231)
    (273, Token (AS _)) -> Just (Reduce 0 231)
    (273, Token (QVARID _)) -> Just (Reduce 0 231)
    (273, Token (QVARSYM _)) -> Just (Reduce 0 231)
    (273, Token (BACKQUOTE _)) -> Just (Reduce 0 231)
    (273, Token (QCONSYM _)) -> Just (Reduce 0 231)
    (273, Token (INTEGER _)) -> Just (Shift 307)
    (274, Token (MINUS _)) -> Just (Shift 310)
    (274, Token (QVARSYM _)) -> Just (Shift 437)
    (274, Token (BACKQUOTE _)) -> Just (Shift 346)
    (274, Token (QCONSYM _)) -> Just (Shift 349)
    (275, Token (RBRACE _)) -> Just (Reduce 3 88)
    (275, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (276, Token (LPAREN _)) -> Just (Reduce 1 201)
    (276, Token (RPAREN _)) -> Just (Reduce 1 201)
    (276, Token (EQUAL _)) -> Just (Reduce 1 201)
    (276, Token (PIPE _)) -> Just (Reduce 1 201)
    (276, Token (MINUS _)) -> Just (Reduce 1 201)
    (276, Token (RARROW _)) -> Just (Reduce 1 201)
    (276, Token (QCONID _)) -> Just (Reduce 1 201)
    (276, Token (EXPORT _)) -> Just (Reduce 1 201)
    (276, Token (AS _)) -> Just (Reduce 1 201)
    (276, Token (QVARID _)) -> Just (Reduce 1 201)
    (276, Token (QVARSYM _)) -> Just (Reduce 1 201)
    (276, Token (BACKQUOTE _)) -> Just (Reduce 1 201)
    (276, Token (QCONSYM _)) -> Just (Reduce 1 201)
    (277, Token (LPAREN _)) -> Just (Reduce 3 203)
    (277, Token (RPAREN _)) -> Just (Reduce 3 203)
    (277, Token (EQUAL _)) -> Just (Reduce 3 203)
    (277, Token (PIPE _)) -> Just (Reduce 3 203)
    (277, Token (MINUS _)) -> Just (Reduce 3 203)
    (277, Token (RARROW _)) -> Just (Reduce 3 203)
    (277, Token (QCONID _)) -> Just (Reduce 3 203)
    (277, Token (EXPORT _)) -> Just (Reduce 3 203)
    (277, Token (AS _)) -> Just (Reduce 3 203)
    (277, Token (QVARID _)) -> Just (Reduce 3 203)
    (277, Token (QVARSYM _)) -> Just (Reduce 3 203)
    (277, Token (BACKQUOTE _)) -> Just (Reduce 3 203)
    (277, Token (QCONSYM _)) -> Just (Reduce 3 203)
    (278, Token (LPAREN _)) -> Just (Reduce 2 202)
    (278, Token (RPAREN _)) -> Just (Reduce 2 202)
    (278, Token (EQUAL _)) -> Just (Reduce 2 202)
    (278, Token (PIPE _)) -> Just (Reduce 2 202)
    (278, Token (MINUS _)) -> Just (Reduce 2 202)
    (278, Token (RARROW _)) -> Just (Reduce 2 202)
    (278, Token (QCONID _)) -> Just (Reduce 2 202)
    (278, Token (EXPORT _)) -> Just (Reduce 2 202)
    (278, Token (AS _)) -> Just (Reduce 2 202)
    (278, Token (QVARID _)) -> Just (Reduce 2 202)
    (278, Token (QVARSYM _)) -> Just (Reduce 2 202)
    (278, Token (BACKQUOTE _)) -> Just (Reduce 2 202)
    (278, Token (QCONSYM _)) -> Just (Reduce 2 202)
    (279, Token (LPAREN _)) -> Just (Reduce 3 204)
    (279, Token (RPAREN _)) -> Just (Reduce 3 204)
    (279, Token (EQUAL _)) -> Just (Reduce 3 204)
    (279, Token (PIPE _)) -> Just (Reduce 3 204)
    (279, Token (MINUS _)) -> Just (Reduce 3 204)
    (279, Token (RARROW _)) -> Just (Reduce 3 204)
    (279, Token (QCONID _)) -> Just (Reduce 3 204)
    (279, Token (EXPORT _)) -> Just (Reduce 3 204)
    (279, Token (AS _)) -> Just (Reduce 3 204)
    (279, Token (QVARID _)) -> Just (Reduce 3 204)
    (279, Token (QVARSYM _)) -> Just (Reduce 3 204)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 3 204)
    (279, Token (QCONSYM _)) -> Just (Reduce 3 204)
    (280, Token (WHERE _)) -> Just (Reduce 1 153)
    (280, Token (RBRACE _)) -> Just (Reduce 1 153)
    (280, Token (RPAREN _)) -> Just (Reduce 1 153)
    (280, Token (COMMA _)) -> Just (Reduce 1 153)
    (280, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (280, Token (EQUAL _)) -> Just (Reduce 1 153)
    (280, Token (PIPE _)) -> Just (Reduce 1 153)
    (280, Token (LARROW _)) -> Just (Reduce 1 153)
    (280, Token (THEN _)) -> Just (Reduce 1 153)
    (280, Token (ELSE _)) -> Just (Reduce 1 153)
    (280, Token (OF _)) -> Just (Reduce 1 153)
    (281, Token (WHERE _)) -> Just (Reduce 3 146)
    (281, Token (RBRACE _)) -> Just (Reduce 3 146)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (281, Token (PIPE _)) -> Just (Shift 63)
    (282, Token (WHERE _)) -> Just (Reduce 5 147)
    (282, Token (RBRACE _)) -> Just (Reduce 5 147)
    (282, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (283, Token (EQUAL _)) -> Just (Shift 47)
    (284, Token (RBRACE _)) -> Just (Reduce 3 67)
    (284, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (285, Token (RBRACE _)) -> Just (Shift 284)
    (286, Token (RBRACE _)) -> Just (Reduce 3 69)
    (287, Token (RBRACE _)) -> Just (Reduce 1 68)
    (287, Token (SEMICOLON _)) -> Just (Shift 78)
    (288, Token (RBRACE _)) -> Just (Reduce 5 72)
    (288, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (289, Token (RBRACE _)) -> Just (Reduce 5 74)
    (289, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (290, Token (RBRACE _)) -> Just (Reduce 1 70)
    (290, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (291, Token (WHERE _)) -> Just (Shift 259)
    (291, Token (RBRACE _)) -> Just (Reduce 3 71)
    (291, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (292, Token (WHERE _)) -> Just (Shift 260)
    (292, Token (RBRACE _)) -> Just (Reduce 3 73)
    (292, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (293, Token (RBRACE _)) -> Just (Reduce 3 77)
    (293, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (294, Token (RBRACE _)) -> Just (Shift 293)
    (295, Token (RBRACE _)) -> Just (Reduce 3 79)
    (296, Token (RBRACE _)) -> Just (Reduce 1 78)
    (296, Token (SEMICOLON _)) -> Just (Shift 81)
    (297, Token (RBRACE _)) -> Just (Reduce 5 82)
    (297, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (298, Token (RBRACE _)) -> Just (Reduce 5 84)
    (298, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (299, Token (WHERE _)) -> Just (Shift 261)
    (299, Token (RBRACE _)) -> Just (Reduce 3 81)
    (299, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (300, Token (WHERE _)) -> Just (Shift 262)
    (300, Token (RBRACE _)) -> Just (Reduce 3 83)
    (300, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (301, Token (COMMA _)) -> Just (Shift 94)
    (301, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (302, Token (LPAREN _)) -> Just (Reduce 1 205)
    (302, Token (COMMA _)) -> Just (Shift 94)
    (302, Token (EQUAL _)) -> Just (Reduce 1 205)
    (302, Token (PIPE _)) -> Just (Reduce 1 205)
    (302, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (302, Token (MINUS _)) -> Just (Reduce 1 205)
    (302, Token (QCONID _)) -> Just (Reduce 1 205)
    (302, Token (EXPORT _)) -> Just (Reduce 1 205)
    (302, Token (AS _)) -> Just (Reduce 1 205)
    (302, Token (QVARID _)) -> Just (Reduce 1 205)
    (302, Token (QVARSYM _)) -> Just (Reduce 1 205)
    (302, Token (BACKQUOTE _)) -> Just (Reduce 1 205)
    (302, Token (QCONSYM _)) -> Just (Reduce 1 205)
    (303, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (304, Token (COMMA _)) -> Just (Reduce 1 95)
    (304, Token (MINUS _)) -> Just (Reduce 1 95)
    (304, Token (QCONID _)) -> Just (Reduce 1 95)
    (304, Token (EXPORT _)) -> Just (Reduce 1 95)
    (304, Token (AS _)) -> Just (Reduce 1 95)
    (304, Token (QVARID _)) -> Just (Reduce 1 95)
    (304, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (304, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (304, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (304, Token (INTEGER _)) -> Just (Reduce 1 95)
    (305, Token (COMMA _)) -> Just (Reduce 1 96)
    (305, Token (MINUS _)) -> Just (Reduce 1 96)
    (305, Token (QCONID _)) -> Just (Reduce 1 96)
    (305, Token (EXPORT _)) -> Just (Reduce 1 96)
    (305, Token (AS _)) -> Just (Reduce 1 96)
    (305, Token (QVARID _)) -> Just (Reduce 1 96)
    (305, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (305, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (305, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (305, Token (INTEGER _)) -> Just (Reduce 1 96)
    (306, Token (COMMA _)) -> Just (Reduce 1 97)
    (306, Token (MINUS _)) -> Just (Reduce 1 97)
    (306, Token (QCONID _)) -> Just (Reduce 1 97)
    (306, Token (EXPORT _)) -> Just (Reduce 1 97)
    (306, Token (AS _)) -> Just (Reduce 1 97)
    (306, Token (QVARID _)) -> Just (Reduce 1 97)
    (306, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (306, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (306, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (306, Token (INTEGER _)) -> Just (Reduce 1 97)
    (307, Token (COMMA _)) -> Just (Reduce 1 232)
    (307, Token (MINUS _)) -> Just (Reduce 1 232)
    (307, Token (QCONID _)) -> Just (Reduce 1 232)
    (307, Token (EXPORT _)) -> Just (Reduce 1 232)
    (307, Token (AS _)) -> Just (Reduce 1 232)
    (307, Token (QVARID _)) -> Just (Reduce 1 232)
    (307, Token (QVARSYM _)) -> Just (Reduce 1 232)
    (307, Token (BACKQUOTE _)) -> Just (Reduce 1 232)
    (307, Token (QCONSYM _)) -> Just (Reduce 1 232)
    (308, Token (MINUS _)) -> Just (Shift 310)
    (308, Token (QVARSYM _)) -> Just (Shift 437)
    (308, Token (BACKQUOTE _)) -> Just (Shift 346)
    (308, Token (QCONSYM _)) -> Just (Shift 349)
    (309, Token (MINUS _)) -> Just (Shift 310)
    (309, Token (QVARSYM _)) -> Just (Shift 437)
    (309, Token (BACKQUOTE _)) -> Just (Shift 346)
    (309, Token (QCONSYM _)) -> Just (Shift 349)
    (310, Token (RBRACE _)) -> Just (Reduce 1 89)
    (310, Token (COMMA _)) -> Just (Shift 308)
    (310, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (311, Token (RBRACE _)) -> Just (Reduce 3 91)
    (311, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (312, Token (RBRACE _)) -> Just (Reduce 3 92)
    (312, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (313, Token (RBRACE _)) -> Just (Reduce 1 90)
    (313, Token (COMMA _)) -> Just (Shift 309)
    (313, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (314, Token (RBRACE _)) -> Just (Reduce 1 221)
    (314, Token (LPAREN _)) -> Just (Reduce 1 221)
    (314, Token (COMMA _)) -> Just (Reduce 1 221)
    (314, Token (SEMICOLON _)) -> Just (Reduce 1 221)
    (314, Token (MINUS _)) -> Just (Reduce 1 221)
    (314, Token (QCONID _)) -> Just (Reduce 1 221)
    (314, Token (EXPORT _)) -> Just (Reduce 1 221)
    (314, Token (AS _)) -> Just (Reduce 1 221)
    (314, Token (QVARID _)) -> Just (Reduce 1 221)
    (314, Token (QVARSYM _)) -> Just (Reduce 1 221)
    (314, Token (BACKQUOTE _)) -> Just (Reduce 1 221)
    (314, Token (QCONSYM _)) -> Just (Reduce 1 221)
    (315, Token (RBRACE _)) -> Just (Reduce 1 220)
    (315, Token (LPAREN _)) -> Just (Reduce 1 220)
    (315, Token (COMMA _)) -> Just (Reduce 1 220)
    (315, Token (SEMICOLON _)) -> Just (Reduce 1 220)
    (315, Token (MINUS _)) -> Just (Reduce 1 220)
    (315, Token (QCONID _)) -> Just (Reduce 1 220)
    (315, Token (EXPORT _)) -> Just (Reduce 1 220)
    (315, Token (AS _)) -> Just (Reduce 1 220)
    (315, Token (QVARID _)) -> Just (Reduce 1 220)
    (315, Token (QVARSYM _)) -> Just (Reduce 1 220)
    (315, Token (BACKQUOTE _)) -> Just (Reduce 1 220)
    (315, Token (QCONSYM _)) -> Just (Reduce 1 220)
    (316, Token (WHERE _)) -> Just (Reduce 3 108)
    (316, Token (LBRACE _)) -> Just (Reduce 3 108)
    (316, Token (RBRACE _)) -> Just (Reduce 3 108)
    (316, Token (LPAREN _)) -> Just (Reduce 3 108)
    (316, Token (RPAREN _)) -> Just (Reduce 3 108)
    (316, Token (COMMA _)) -> Just (Reduce 3 108)
    (316, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (316, Token (EQUAL _)) -> Just (Reduce 3 108)
    (316, Token (DERIVING _)) -> Just (Reduce 3 108)
    (316, Token (DARROW _)) -> Just (Reduce 3 108)
    (316, Token (PIPE _)) -> Just (Reduce 3 108)
    (316, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (316, Token (MINUS _)) -> Just (Reduce 3 108)
    (316, Token (INFIXL _)) -> Just (Reduce 3 108)
    (316, Token (INFIXR _)) -> Just (Reduce 3 108)
    (316, Token (INFIX _)) -> Just (Reduce 3 108)
    (316, Token (RARROW _)) -> Just (Reduce 3 108)
    (316, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (316, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (316, Token (EXCL _)) -> Just (Reduce 3 108)
    (316, Token (QCONID _)) -> Just (Reduce 3 108)
    (316, Token (EXPORT _)) -> Just (Reduce 3 108)
    (316, Token (AS _)) -> Just (Reduce 3 108)
    (316, Token (QVARID _)) -> Just (Reduce 3 108)
    (316, Token (LARROW _)) -> Just (Reduce 3 108)
    (316, Token (THEN _)) -> Just (Reduce 3 108)
    (316, Token (ELSE _)) -> Just (Reduce 3 108)
    (316, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (316, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (316, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (316, Token (OF _)) -> Just (Reduce 3 108)
    (316, Token (INTEGER _)) -> Just (Reduce 3 108)
    (317, Token (WHERE _)) -> Just (Reduce 3 106)
    (317, Token (LBRACE _)) -> Just (Reduce 3 106)
    (317, Token (RBRACE _)) -> Just (Reduce 3 106)
    (317, Token (LPAREN _)) -> Just (Reduce 3 106)
    (317, Token (RPAREN _)) -> Just (Reduce 3 106)
    (317, Token (COMMA _)) -> Just (Reduce 3 106)
    (317, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (317, Token (EQUAL _)) -> Just (Reduce 3 106)
    (317, Token (DERIVING _)) -> Just (Reduce 3 106)
    (317, Token (DARROW _)) -> Just (Reduce 3 106)
    (317, Token (PIPE _)) -> Just (Reduce 3 106)
    (317, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (317, Token (MINUS _)) -> Just (Reduce 3 106)
    (317, Token (INFIXL _)) -> Just (Reduce 3 106)
    (317, Token (INFIXR _)) -> Just (Reduce 3 106)
    (317, Token (INFIX _)) -> Just (Reduce 3 106)
    (317, Token (RARROW _)) -> Just (Reduce 3 106)
    (317, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (317, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (317, Token (EXCL _)) -> Just (Reduce 3 106)
    (317, Token (QCONID _)) -> Just (Reduce 3 106)
    (317, Token (EXPORT _)) -> Just (Reduce 3 106)
    (317, Token (AS _)) -> Just (Reduce 3 106)
    (317, Token (QVARID _)) -> Just (Reduce 3 106)
    (317, Token (LARROW _)) -> Just (Reduce 3 106)
    (317, Token (THEN _)) -> Just (Reduce 3 106)
    (317, Token (ELSE _)) -> Just (Reduce 3 106)
    (317, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (317, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (317, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (317, Token (OF _)) -> Just (Reduce 3 106)
    (317, Token (INTEGER _)) -> Just (Reduce 3 106)
    (318, Token (WHERE _)) -> Just (Reduce 3 107)
    (318, Token (LBRACE _)) -> Just (Reduce 3 107)
    (318, Token (RBRACE _)) -> Just (Reduce 3 107)
    (318, Token (LPAREN _)) -> Just (Reduce 3 107)
    (318, Token (RPAREN _)) -> Just (Reduce 3 107)
    (318, Token (COMMA _)) -> Just (Reduce 3 107)
    (318, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (318, Token (EQUAL _)) -> Just (Reduce 3 107)
    (318, Token (DERIVING _)) -> Just (Reduce 3 107)
    (318, Token (DARROW _)) -> Just (Reduce 3 107)
    (318, Token (PIPE _)) -> Just (Reduce 3 107)
    (318, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (318, Token (MINUS _)) -> Just (Reduce 3 107)
    (318, Token (INFIXL _)) -> Just (Reduce 3 107)
    (318, Token (INFIXR _)) -> Just (Reduce 3 107)
    (318, Token (INFIX _)) -> Just (Reduce 3 107)
    (318, Token (RARROW _)) -> Just (Reduce 3 107)
    (318, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (318, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (318, Token (EXCL _)) -> Just (Reduce 3 107)
    (318, Token (QCONID _)) -> Just (Reduce 3 107)
    (318, Token (EXPORT _)) -> Just (Reduce 3 107)
    (318, Token (AS _)) -> Just (Reduce 3 107)
    (318, Token (QVARID _)) -> Just (Reduce 3 107)
    (318, Token (LARROW _)) -> Just (Reduce 3 107)
    (318, Token (THEN _)) -> Just (Reduce 3 107)
    (318, Token (ELSE _)) -> Just (Reduce 3 107)
    (318, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (318, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (318, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (318, Token (OF _)) -> Just (Reduce 3 107)
    (318, Token (INTEGER _)) -> Just (Reduce 3 107)
    (319, Token (RPAREN _)) -> Just (Shift 316)
    (319, Token (COMMA _)) -> Just (Shift 153)
    (320, Token (RBRACKET _)) -> Just (Shift 318)
    (321, Token (WHERE _)) -> Just (Reduce 2 109)
    (321, Token (LBRACE _)) -> Just (Reduce 2 109)
    (321, Token (RBRACE _)) -> Just (Reduce 2 109)
    (321, Token (LPAREN _)) -> Just (Reduce 2 109)
    (321, Token (RPAREN _)) -> Just (Reduce 2 109)
    (321, Token (COMMA _)) -> Just (Reduce 2 109)
    (321, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (321, Token (EQUAL _)) -> Just (Reduce 2 109)
    (321, Token (DERIVING _)) -> Just (Reduce 2 109)
    (321, Token (DARROW _)) -> Just (Reduce 2 109)
    (321, Token (PIPE _)) -> Just (Reduce 2 109)
    (321, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (321, Token (MINUS _)) -> Just (Reduce 2 109)
    (321, Token (INFIXL _)) -> Just (Reduce 2 109)
    (321, Token (INFIXR _)) -> Just (Reduce 2 109)
    (321, Token (INFIX _)) -> Just (Reduce 2 109)
    (321, Token (RARROW _)) -> Just (Reduce 2 109)
    (321, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (321, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (321, Token (EXCL _)) -> Just (Reduce 2 109)
    (321, Token (QCONID _)) -> Just (Reduce 2 109)
    (321, Token (EXPORT _)) -> Just (Reduce 2 109)
    (321, Token (AS _)) -> Just (Reduce 2 109)
    (321, Token (QVARID _)) -> Just (Reduce 2 109)
    (321, Token (LARROW _)) -> Just (Reduce 2 109)
    (321, Token (THEN _)) -> Just (Reduce 2 109)
    (321, Token (ELSE _)) -> Just (Reduce 2 109)
    (321, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (321, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (321, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (321, Token (OF _)) -> Just (Reduce 2 109)
    (321, Token (INTEGER _)) -> Just (Reduce 2 109)
    (322, Token (WHERE _)) -> Just (Reduce 1 104)
    (322, Token (LBRACE _)) -> Just (Reduce 1 104)
    (322, Token (RBRACE _)) -> Just (Reduce 1 104)
    (322, Token (LPAREN _)) -> Just (Reduce 1 104)
    (322, Token (RPAREN _)) -> Just (Reduce 1 104)
    (322, Token (COMMA _)) -> Just (Reduce 1 104)
    (322, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (322, Token (EQUAL _)) -> Just (Reduce 1 104)
    (322, Token (DERIVING _)) -> Just (Reduce 1 104)
    (322, Token (DARROW _)) -> Just (Reduce 1 104)
    (322, Token (PIPE _)) -> Just (Reduce 1 104)
    (322, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (322, Token (MINUS _)) -> Just (Reduce 1 104)
    (322, Token (INFIXL _)) -> Just (Reduce 1 104)
    (322, Token (INFIXR _)) -> Just (Reduce 1 104)
    (322, Token (INFIX _)) -> Just (Reduce 1 104)
    (322, Token (RARROW _)) -> Just (Reduce 1 104)
    (322, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (322, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (322, Token (EXCL _)) -> Just (Reduce 1 104)
    (322, Token (QCONID _)) -> Just (Reduce 1 104)
    (322, Token (EXPORT _)) -> Just (Reduce 1 104)
    (322, Token (AS _)) -> Just (Reduce 1 104)
    (322, Token (QVARID _)) -> Just (Reduce 1 104)
    (322, Token (LARROW _)) -> Just (Reduce 1 104)
    (322, Token (THEN _)) -> Just (Reduce 1 104)
    (322, Token (ELSE _)) -> Just (Reduce 1 104)
    (322, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (322, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (322, Token (OF _)) -> Just (Reduce 1 104)
    (322, Token (INTEGER _)) -> Just (Reduce 1 104)
    (323, Token (WHERE _)) -> Just (Reduce 1 105)
    (323, Token (LBRACE _)) -> Just (Reduce 1 105)
    (323, Token (RBRACE _)) -> Just (Reduce 1 105)
    (323, Token (LPAREN _)) -> Just (Reduce 1 105)
    (323, Token (RPAREN _)) -> Just (Reduce 1 105)
    (323, Token (COMMA _)) -> Just (Reduce 1 105)
    (323, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (323, Token (EQUAL _)) -> Just (Reduce 1 105)
    (323, Token (DERIVING _)) -> Just (Reduce 1 105)
    (323, Token (DARROW _)) -> Just (Reduce 1 105)
    (323, Token (PIPE _)) -> Just (Reduce 1 105)
    (323, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (323, Token (MINUS _)) -> Just (Reduce 1 105)
    (323, Token (INFIXL _)) -> Just (Reduce 1 105)
    (323, Token (INFIXR _)) -> Just (Reduce 1 105)
    (323, Token (INFIX _)) -> Just (Reduce 1 105)
    (323, Token (RARROW _)) -> Just (Reduce 1 105)
    (323, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (323, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (323, Token (EXCL _)) -> Just (Reduce 1 105)
    (323, Token (QCONID _)) -> Just (Reduce 1 105)
    (323, Token (EXPORT _)) -> Just (Reduce 1 105)
    (323, Token (AS _)) -> Just (Reduce 1 105)
    (323, Token (QVARID _)) -> Just (Reduce 1 105)
    (323, Token (LARROW _)) -> Just (Reduce 1 105)
    (323, Token (THEN _)) -> Just (Reduce 1 105)
    (323, Token (ELSE _)) -> Just (Reduce 1 105)
    (323, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (323, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (323, Token (OF _)) -> Just (Reduce 1 105)
    (323, Token (INTEGER _)) -> Just (Reduce 1 105)
    (324, Token (RPAREN _)) -> Just (Shift 317)
    (325, Token (WHERE _)) -> Just (Reduce 2 113)
    (325, Token (LBRACE _)) -> Just (Reduce 2 113)
    (325, Token (RBRACE _)) -> Just (Reduce 2 113)
    (325, Token (LPAREN _)) -> Just (Reduce 2 113)
    (325, Token (RPAREN _)) -> Just (Reduce 2 113)
    (325, Token (COMMA _)) -> Just (Reduce 2 113)
    (325, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (325, Token (EQUAL _)) -> Just (Reduce 2 113)
    (325, Token (DERIVING _)) -> Just (Reduce 2 113)
    (325, Token (DARROW _)) -> Just (Reduce 2 113)
    (325, Token (PIPE _)) -> Just (Reduce 2 113)
    (325, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (325, Token (MINUS _)) -> Just (Reduce 2 113)
    (325, Token (INFIXL _)) -> Just (Reduce 2 113)
    (325, Token (INFIXR _)) -> Just (Reduce 2 113)
    (325, Token (INFIX _)) -> Just (Reduce 2 113)
    (325, Token (RARROW _)) -> Just (Reduce 2 113)
    (325, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (325, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (325, Token (EXCL _)) -> Just (Reduce 2 113)
    (325, Token (QCONID _)) -> Just (Reduce 2 113)
    (325, Token (EXPORT _)) -> Just (Reduce 2 113)
    (325, Token (AS _)) -> Just (Reduce 2 113)
    (325, Token (QVARID _)) -> Just (Reduce 2 113)
    (325, Token (LARROW _)) -> Just (Reduce 2 113)
    (325, Token (THEN _)) -> Just (Reduce 2 113)
    (325, Token (ELSE _)) -> Just (Reduce 2 113)
    (325, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (325, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (325, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (325, Token (OF _)) -> Just (Reduce 2 113)
    (325, Token (INTEGER _)) -> Just (Reduce 2 113)
    (326, Token (WHERE _)) -> Just (Reduce 3 115)
    (326, Token (LBRACE _)) -> Just (Reduce 3 115)
    (326, Token (RBRACE _)) -> Just (Reduce 3 115)
    (326, Token (LPAREN _)) -> Just (Reduce 3 115)
    (326, Token (RPAREN _)) -> Just (Reduce 3 115)
    (326, Token (COMMA _)) -> Just (Reduce 3 115)
    (326, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (326, Token (EQUAL _)) -> Just (Reduce 3 115)
    (326, Token (DERIVING _)) -> Just (Reduce 3 115)
    (326, Token (DARROW _)) -> Just (Reduce 3 115)
    (326, Token (PIPE _)) -> Just (Reduce 3 115)
    (326, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (326, Token (MINUS _)) -> Just (Reduce 3 115)
    (326, Token (INFIXL _)) -> Just (Reduce 3 115)
    (326, Token (INFIXR _)) -> Just (Reduce 3 115)
    (326, Token (INFIX _)) -> Just (Reduce 3 115)
    (326, Token (RARROW _)) -> Just (Reduce 3 115)
    (326, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (326, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (326, Token (EXCL _)) -> Just (Reduce 3 115)
    (326, Token (QCONID _)) -> Just (Reduce 3 115)
    (326, Token (EXPORT _)) -> Just (Reduce 3 115)
    (326, Token (AS _)) -> Just (Reduce 3 115)
    (326, Token (QVARID _)) -> Just (Reduce 3 115)
    (326, Token (LARROW _)) -> Just (Reduce 3 115)
    (326, Token (THEN _)) -> Just (Reduce 3 115)
    (326, Token (ELSE _)) -> Just (Reduce 3 115)
    (326, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (326, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (326, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (326, Token (OF _)) -> Just (Reduce 3 115)
    (326, Token (INTEGER _)) -> Just (Reduce 3 115)
    (327, Token (WHERE _)) -> Just (Reduce 3 116)
    (327, Token (LBRACE _)) -> Just (Reduce 3 116)
    (327, Token (RBRACE _)) -> Just (Reduce 3 116)
    (327, Token (LPAREN _)) -> Just (Reduce 3 116)
    (327, Token (RPAREN _)) -> Just (Reduce 3 116)
    (327, Token (COMMA _)) -> Just (Reduce 3 116)
    (327, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (327, Token (EQUAL _)) -> Just (Reduce 3 116)
    (327, Token (DERIVING _)) -> Just (Reduce 3 116)
    (327, Token (DARROW _)) -> Just (Reduce 3 116)
    (327, Token (PIPE _)) -> Just (Reduce 3 116)
    (327, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (327, Token (MINUS _)) -> Just (Reduce 3 116)
    (327, Token (INFIXL _)) -> Just (Reduce 3 116)
    (327, Token (INFIXR _)) -> Just (Reduce 3 116)
    (327, Token (INFIX _)) -> Just (Reduce 3 116)
    (327, Token (RARROW _)) -> Just (Reduce 3 116)
    (327, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (327, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (327, Token (EXCL _)) -> Just (Reduce 3 116)
    (327, Token (QCONID _)) -> Just (Reduce 3 116)
    (327, Token (EXPORT _)) -> Just (Reduce 3 116)
    (327, Token (AS _)) -> Just (Reduce 3 116)
    (327, Token (QVARID _)) -> Just (Reduce 3 116)
    (327, Token (LARROW _)) -> Just (Reduce 3 116)
    (327, Token (THEN _)) -> Just (Reduce 3 116)
    (327, Token (ELSE _)) -> Just (Reduce 3 116)
    (327, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (327, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (327, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (327, Token (OF _)) -> Just (Reduce 3 116)
    (327, Token (INTEGER _)) -> Just (Reduce 3 116)
    (328, Token (RPAREN _)) -> Just (Shift 326)
    (329, Token (WHERE _)) -> Just (Reduce 2 114)
    (329, Token (LBRACE _)) -> Just (Reduce 2 114)
    (329, Token (RBRACE _)) -> Just (Reduce 2 114)
    (329, Token (LPAREN _)) -> Just (Reduce 2 114)
    (329, Token (RPAREN _)) -> Just (Reduce 2 114)
    (329, Token (COMMA _)) -> Just (Reduce 2 114)
    (329, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (329, Token (EQUAL _)) -> Just (Reduce 2 114)
    (329, Token (DERIVING _)) -> Just (Reduce 2 114)
    (329, Token (DARROW _)) -> Just (Reduce 2 114)
    (329, Token (PIPE _)) -> Just (Reduce 2 114)
    (329, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (329, Token (MINUS _)) -> Just (Reduce 2 114)
    (329, Token (INFIXL _)) -> Just (Reduce 2 114)
    (329, Token (INFIXR _)) -> Just (Reduce 2 114)
    (329, Token (INFIX _)) -> Just (Reduce 2 114)
    (329, Token (RARROW _)) -> Just (Reduce 2 114)
    (329, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (329, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (329, Token (EXCL _)) -> Just (Reduce 2 114)
    (329, Token (QCONID _)) -> Just (Reduce 2 114)
    (329, Token (EXPORT _)) -> Just (Reduce 2 114)
    (329, Token (AS _)) -> Just (Reduce 2 114)
    (329, Token (QVARID _)) -> Just (Reduce 2 114)
    (329, Token (LARROW _)) -> Just (Reduce 2 114)
    (329, Token (THEN _)) -> Just (Reduce 2 114)
    (329, Token (ELSE _)) -> Just (Reduce 2 114)
    (329, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (329, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (329, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (329, Token (OF _)) -> Just (Reduce 2 114)
    (329, Token (INTEGER _)) -> Just (Reduce 2 114)
    (330, Token (WHERE _)) -> Just (Reduce 1 112)
    (330, Token (LBRACE _)) -> Just (Reduce 1 112)
    (330, Token (RBRACE _)) -> Just (Reduce 1 112)
    (330, Token (LPAREN _)) -> Just (Reduce 1 112)
    (330, Token (RPAREN _)) -> Just (Reduce 1 112)
    (330, Token (COMMA _)) -> Just (Reduce 1 112)
    (330, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (330, Token (EQUAL _)) -> Just (Reduce 1 112)
    (330, Token (DERIVING _)) -> Just (Reduce 1 112)
    (330, Token (DARROW _)) -> Just (Reduce 1 112)
    (330, Token (PIPE _)) -> Just (Reduce 1 112)
    (330, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (330, Token (MINUS _)) -> Just (Reduce 1 112)
    (330, Token (INFIXL _)) -> Just (Reduce 1 112)
    (330, Token (INFIXR _)) -> Just (Reduce 1 112)
    (330, Token (INFIX _)) -> Just (Reduce 1 112)
    (330, Token (RARROW _)) -> Just (Reduce 1 112)
    (330, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (330, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (330, Token (EXCL _)) -> Just (Reduce 1 112)
    (330, Token (QCONID _)) -> Just (Reduce 1 112)
    (330, Token (EXPORT _)) -> Just (Reduce 1 112)
    (330, Token (AS _)) -> Just (Reduce 1 112)
    (330, Token (QVARID _)) -> Just (Reduce 1 112)
    (330, Token (LARROW _)) -> Just (Reduce 1 112)
    (330, Token (THEN _)) -> Just (Reduce 1 112)
    (330, Token (ELSE _)) -> Just (Reduce 1 112)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (330, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (330, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (330, Token (OF _)) -> Just (Reduce 1 112)
    (330, Token (INTEGER _)) -> Just (Reduce 1 112)
    (331, Token (LBRACE _)) -> Just (Shift 95)
    (331, Token (RBRACE _)) -> Just (Reduce 1 112)
    (331, Token (LPAREN _)) -> Just (Reduce 1 112)
    (331, Token (RPAREN _)) -> Just (Reduce 1 112)
    (331, Token (COMMA _)) -> Just (Reduce 1 112)
    (331, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (331, Token (DERIVING _)) -> Just (Reduce 1 112)
    (331, Token (PIPE _)) -> Just (Reduce 1 112)
    (331, Token (RARROW _)) -> Just (Reduce 1 112)
    (331, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (331, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (331, Token (EXCL _)) -> Just (Reduce 1 112)
    (331, Token (QCONID _)) -> Just (Reduce 1 112)
    (331, Token (EXPORT _)) -> Just (Reduce 1 112)
    (331, Token (AS _)) -> Just (Reduce 1 112)
    (331, Token (QVARID _)) -> Just (Reduce 1 112)
    (331, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (331, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (332, Token (RPAREN _)) -> Just (Shift 327)
    (333, Token (WHERE _)) -> Just (Reduce 1 227)
    (333, Token (LBRACE _)) -> Just (Reduce 1 227)
    (333, Token (RBRACE _)) -> Just (Reduce 1 227)
    (333, Token (LPAREN _)) -> Just (Reduce 1 227)
    (333, Token (RPAREN _)) -> Just (Reduce 1 227)
    (333, Token (COMMA _)) -> Just (Reduce 1 227)
    (333, Token (SEMICOLON _)) -> Just (Reduce 1 227)
    (333, Token (EQUAL _)) -> Just (Reduce 1 227)
    (333, Token (DERIVING _)) -> Just (Reduce 1 227)
    (333, Token (DARROW _)) -> Just (Reduce 1 227)
    (333, Token (PIPE _)) -> Just (Reduce 1 227)
    (333, Token (COLON_COLON _)) -> Just (Reduce 1 227)
    (333, Token (MINUS _)) -> Just (Reduce 1 227)
    (333, Token (INFIXL _)) -> Just (Reduce 1 227)
    (333, Token (INFIXR _)) -> Just (Reduce 1 227)
    (333, Token (INFIX _)) -> Just (Reduce 1 227)
    (333, Token (RARROW _)) -> Just (Reduce 1 227)
    (333, Token (LBRACKET _)) -> Just (Reduce 1 227)
    (333, Token (RBRACKET _)) -> Just (Reduce 1 227)
    (333, Token (EXCL _)) -> Just (Reduce 1 227)
    (333, Token (QCONID _)) -> Just (Reduce 1 227)
    (333, Token (EXPORT _)) -> Just (Reduce 1 227)
    (333, Token (AS _)) -> Just (Reduce 1 227)
    (333, Token (QVARID _)) -> Just (Reduce 1 227)
    (333, Token (LARROW _)) -> Just (Reduce 1 227)
    (333, Token (THEN _)) -> Just (Reduce 1 227)
    (333, Token (ELSE _)) -> Just (Reduce 1 227)
    (333, Token (QVARSYM _)) -> Just (Reduce 1 227)
    (333, Token (BACKQUOTE _)) -> Just (Reduce 1 227)
    (333, Token (QCONSYM _)) -> Just (Reduce 1 227)
    (333, Token (OF _)) -> Just (Reduce 1 227)
    (333, Token (INTEGER _)) -> Just (Reduce 1 227)
    (334, Token (WHERE _)) -> Just (Reduce 1 226)
    (334, Token (LBRACE _)) -> Just (Reduce 1 226)
    (334, Token (RBRACE _)) -> Just (Reduce 1 226)
    (334, Token (LPAREN _)) -> Just (Reduce 1 226)
    (334, Token (RPAREN _)) -> Just (Reduce 1 226)
    (334, Token (COMMA _)) -> Just (Reduce 1 226)
    (334, Token (SEMICOLON _)) -> Just (Reduce 1 226)
    (334, Token (EQUAL _)) -> Just (Reduce 1 226)
    (334, Token (DERIVING _)) -> Just (Reduce 1 226)
    (334, Token (DARROW _)) -> Just (Reduce 1 226)
    (334, Token (PIPE _)) -> Just (Reduce 1 226)
    (334, Token (COLON_COLON _)) -> Just (Reduce 1 226)
    (334, Token (MINUS _)) -> Just (Reduce 1 226)
    (334, Token (INFIXL _)) -> Just (Reduce 1 226)
    (334, Token (INFIXR _)) -> Just (Reduce 1 226)
    (334, Token (INFIX _)) -> Just (Reduce 1 226)
    (334, Token (RARROW _)) -> Just (Reduce 1 226)
    (334, Token (LBRACKET _)) -> Just (Reduce 1 226)
    (334, Token (RBRACKET _)) -> Just (Reduce 1 226)
    (334, Token (EXCL _)) -> Just (Reduce 1 226)
    (334, Token (QCONID _)) -> Just (Reduce 1 226)
    (334, Token (EXPORT _)) -> Just (Reduce 1 226)
    (334, Token (AS _)) -> Just (Reduce 1 226)
    (334, Token (QVARID _)) -> Just (Reduce 1 226)
    (334, Token (LARROW _)) -> Just (Reduce 1 226)
    (334, Token (THEN _)) -> Just (Reduce 1 226)
    (334, Token (ELSE _)) -> Just (Reduce 1 226)
    (334, Token (QVARSYM _)) -> Just (Reduce 1 226)
    (334, Token (BACKQUOTE _)) -> Just (Reduce 1 226)
    (334, Token (QCONSYM _)) -> Just (Reduce 1 226)
    (334, Token (OF _)) -> Just (Reduce 1 226)
    (334, Token (INTEGER _)) -> Just (Reduce 1 226)
    (335, Token (WHERE _)) -> Just (Reduce 1 228)
    (335, Token (LBRACE _)) -> Just (Reduce 1 228)
    (335, Token (RBRACE _)) -> Just (Reduce 1 228)
    (335, Token (LPAREN _)) -> Just (Reduce 1 228)
    (335, Token (RPAREN _)) -> Just (Reduce 1 228)
    (335, Token (COMMA _)) -> Just (Reduce 1 228)
    (335, Token (SEMICOLON _)) -> Just (Reduce 1 228)
    (335, Token (EQUAL _)) -> Just (Reduce 1 228)
    (335, Token (DERIVING _)) -> Just (Reduce 1 228)
    (335, Token (DARROW _)) -> Just (Reduce 1 228)
    (335, Token (PIPE _)) -> Just (Reduce 1 228)
    (335, Token (COLON_COLON _)) -> Just (Reduce 1 228)
    (335, Token (MINUS _)) -> Just (Reduce 1 228)
    (335, Token (INFIXL _)) -> Just (Reduce 1 228)
    (335, Token (INFIXR _)) -> Just (Reduce 1 228)
    (335, Token (INFIX _)) -> Just (Reduce 1 228)
    (335, Token (RARROW _)) -> Just (Reduce 1 228)
    (335, Token (LBRACKET _)) -> Just (Reduce 1 228)
    (335, Token (RBRACKET _)) -> Just (Reduce 1 228)
    (335, Token (EXCL _)) -> Just (Reduce 1 228)
    (335, Token (QCONID _)) -> Just (Reduce 1 228)
    (335, Token (EXPORT _)) -> Just (Reduce 1 228)
    (335, Token (AS _)) -> Just (Reduce 1 228)
    (335, Token (QVARID _)) -> Just (Reduce 1 228)
    (335, Token (LARROW _)) -> Just (Reduce 1 228)
    (335, Token (THEN _)) -> Just (Reduce 1 228)
    (335, Token (ELSE _)) -> Just (Reduce 1 228)
    (335, Token (QVARSYM _)) -> Just (Reduce 1 228)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 1 228)
    (335, Token (QCONSYM _)) -> Just (Reduce 1 228)
    (335, Token (OF _)) -> Just (Reduce 1 228)
    (335, Token (INTEGER _)) -> Just (Reduce 1 228)
    (336, Token (RPAREN _)) -> Just (Reduce 3 110)
    (336, Token (COMMA _)) -> Just (Shift 153)
    (337, Token (RPAREN _)) -> Just (Reduce 3 111)
    (338, Token (RPAREN _)) -> Just (Reduce 1 117)
    (338, Token (COMMA _)) -> Just (Shift 338)
    (339, Token (RPAREN _)) -> Just (Reduce 2 118)
    (340, Token (RBRACE _)) -> Just (Reduce 3 122)
    (340, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (340, Token (DERIVING _)) -> Just (Reduce 3 122)
    (341, Token (RBRACE _)) -> Just (Reduce 1 121)
    (341, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (341, Token (DERIVING _)) -> Just (Reduce 1 121)
    (341, Token (PIPE _)) -> Just (Shift 129)
    (342, Token (RBRACE _)) -> Just (Reduce 3 125)
    (342, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (342, Token (DERIVING _)) -> Just (Reduce 3 125)
    (342, Token (PIPE _)) -> Just (Reduce 3 125)
    (343, Token (RBRACE _)) -> Just (Reduce 4 126)
    (343, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (343, Token (DERIVING _)) -> Just (Reduce 4 126)
    (343, Token (PIPE _)) -> Just (Reduce 4 126)
    (344, Token (RBRACE _)) -> Just (Shift 343)
    (345, Token (BACKQUOTE _)) -> Just (Shift 348)
    (346, Token (QCONID _)) -> Just (Shift 345)
    (346, Token (EXPORT _)) -> Just (Shift 434)
    (346, Token (AS _)) -> Just (Shift 435)
    (346, Token (QVARID _)) -> Just (Shift 436)
    (347, Token (QCONID _)) -> Just (Shift 345)
    (348, Token (RBRACE _)) -> Just (Reduce 3 219)
    (348, Token (LPAREN _)) -> Just (Reduce 3 219)
    (348, Token (RPAREN _)) -> Just (Reduce 3 219)
    (348, Token (COMMA _)) -> Just (Reduce 3 219)
    (348, Token (SEMICOLON _)) -> Just (Reduce 3 219)
    (348, Token (MINUS _)) -> Just (Reduce 3 219)
    (348, Token (RARROW _)) -> Just (Reduce 3 219)
    (348, Token (LBRACKET _)) -> Just (Reduce 3 219)
    (348, Token (RBRACKET _)) -> Just (Reduce 3 219)
    (348, Token (EXCL _)) -> Just (Reduce 3 219)
    (348, Token (QCONID _)) -> Just (Reduce 3 219)
    (348, Token (EXPORT _)) -> Just (Reduce 3 219)
    (348, Token (AS _)) -> Just (Reduce 3 219)
    (348, Token (QVARID _)) -> Just (Reduce 3 219)
    (348, Token (QVARSYM _)) -> Just (Reduce 3 219)
    (348, Token (BACKQUOTE _)) -> Just (Reduce 3 219)
    (348, Token (QCONSYM _)) -> Just (Reduce 3 219)
    (349, Token (RBRACE _)) -> Just (Reduce 1 218)
    (349, Token (LPAREN _)) -> Just (Reduce 1 218)
    (349, Token (RPAREN _)) -> Just (Reduce 1 218)
    (349, Token (COMMA _)) -> Just (Reduce 1 218)
    (349, Token (SEMICOLON _)) -> Just (Reduce 1 218)
    (349, Token (MINUS _)) -> Just (Reduce 1 218)
    (349, Token (RARROW _)) -> Just (Reduce 1 218)
    (349, Token (LBRACKET _)) -> Just (Reduce 1 218)
    (349, Token (RBRACKET _)) -> Just (Reduce 1 218)
    (349, Token (EXCL _)) -> Just (Reduce 1 218)
    (349, Token (QCONID _)) -> Just (Reduce 1 218)
    (349, Token (EXPORT _)) -> Just (Reduce 1 218)
    (349, Token (AS _)) -> Just (Reduce 1 218)
    (349, Token (QVARID _)) -> Just (Reduce 1 218)
    (349, Token (QVARSYM _)) -> Just (Reduce 1 218)
    (349, Token (BACKQUOTE _)) -> Just (Reduce 1 218)
    (349, Token (QCONSYM _)) -> Just (Reduce 1 218)
    (350, Token (RBRACE _)) -> Just (Reduce 3 130)
    (351, Token (RBRACE _)) -> Just (Reduce 1 129)
    (351, Token (COMMA _)) -> Just (Shift 96)
    (352, Token (RBRACE _)) -> Just (Reduce 3 131)
    (352, Token (COMMA _)) -> Just (Reduce 3 131)
    (353, Token (COLON_COLON _)) -> Just (Shift 141)
    (354, Token (EXPORT _)) -> Just (Reduce 1 139)
    (354, Token (AS _)) -> Just (Reduce 1 139)
    (354, Token (QVARID _)) -> Just (Reduce 1 139)
    (354, Token (STRING _)) -> Just (Reduce 1 139)
    (355, Token (EXPORT _)) -> Just (Reduce 1 138)
    (355, Token (AS _)) -> Just (Reduce 1 138)
    (355, Token (QVARID _)) -> Just (Reduce 1 138)
    (355, Token (STRING _)) -> Just (Reduce 1 138)
    (356, Token (EXPORT _)) -> Just (Reduce 1 140)
    (356, Token (AS _)) -> Just (Reduce 1 140)
    (356, Token (QVARID _)) -> Just (Reduce 1 140)
    (356, Token (STRING _)) -> Just (Reduce 1 140)
    (357, Token (LPAREN _)) -> Just (Reduce 1 141)
    (357, Token (MINUS _)) -> Just (Reduce 1 141)
    (357, Token (EXPORT _)) -> Just (Reduce 1 141)
    (357, Token (AS _)) -> Just (Reduce 1 141)
    (357, Token (QVARID _)) -> Just (Reduce 1 141)
    (357, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (358, Token (STRING _)) -> Just (Reduce 1 144)
    (359, Token (STRING _)) -> Just (Reduce 1 143)
    (360, Token (STRING _)) -> Just (Reduce 1 145)
    (361, Token (LPAREN _)) -> Just (Reduce 1 142)
    (361, Token (MINUS _)) -> Just (Reduce 1 142)
    (361, Token (EXPORT _)) -> Just (Reduce 1 142)
    (361, Token (AS _)) -> Just (Reduce 1 142)
    (361, Token (QVARID _)) -> Just (Reduce 1 142)
    (361, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (362, Token (EQUAL _)) -> Just (Reduce 3 149)
    (363, Token (COMMA _)) -> Just (Shift 66)
    (363, Token (EQUAL _)) -> Just (Reduce 1 148)
    (364, Token (COMMA _)) -> Just (Reduce 2 151)
    (364, Token (EQUAL _)) -> Just (Reduce 2 151)
    (364, Token (IN _)) -> Just (Shift 36)
    (365, Token (COMMA _)) -> Just (Reduce 3 150)
    (365, Token (EQUAL _)) -> Just (Reduce 3 150)
    (366, Token (COMMA _)) -> Just (Reduce 1 152)
    (366, Token (EQUAL _)) -> Just (Reduce 1 152)
    (366, Token (LARROW _)) -> Just (Shift 70)
    (367, Token (BACKQUOTE _)) -> Just (Shift 39)
    (368, Token (BACKQUOTE _)) -> Just (Shift 40)
    (369, Token (BACKQUOTE _)) -> Just (Shift 41)
    (370, Token (BACKQUOTE _)) -> Just (Shift 42)
    (371, Token (QCONID _)) -> Just (Shift 367)
    (371, Token (EXPORT _)) -> Just (Shift 368)
    (371, Token (AS _)) -> Just (Shift 369)
    (371, Token (QVARID _)) -> Just (Shift 370)
    (372, Token (WHERE _)) -> Just (Reduce 5 165)
    (372, Token (RBRACE _)) -> Just (Reduce 5 165)
    (372, Token (RPAREN _)) -> Just (Reduce 5 165)
    (372, Token (COMMA _)) -> Just (Reduce 5 165)
    (372, Token (SEMICOLON _)) -> Just (Reduce 5 165)
    (372, Token (EQUAL _)) -> Just (Reduce 5 165)
    (372, Token (PIPE _)) -> Just (Reduce 5 165)
    (372, Token (LARROW _)) -> Just (Reduce 5 165)
    (372, Token (THEN _)) -> Just (Reduce 5 165)
    (372, Token (ELSE _)) -> Just (Reduce 5 165)
    (372, Token (OF _)) -> Just (Reduce 5 165)
    (373, Token (WHERE _)) -> Just (Reduce 3 164)
    (373, Token (RBRACE _)) -> Just (Reduce 3 164)
    (373, Token (RPAREN _)) -> Just (Reduce 3 164)
    (373, Token (COMMA _)) -> Just (Reduce 3 164)
    (373, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (373, Token (EQUAL _)) -> Just (Reduce 3 164)
    (373, Token (PIPE _)) -> Just (Reduce 3 164)
    (373, Token (LARROW _)) -> Just (Reduce 3 164)
    (373, Token (THEN _)) -> Just (Reduce 3 164)
    (373, Token (ELSE _)) -> Just (Reduce 3 164)
    (373, Token (OF _)) -> Just (Reduce 3 164)
    (374, Token (IN _)) -> Just (Shift 36)
    (375, Token (RBRACE _)) -> Just (Reduce 2 200)
    (375, Token (SEMICOLON _)) -> Just (Reduce 2 200)
    (375, Token (IN _)) -> Just (Shift 36)
    (376, Token (WHERE _)) -> Just (Reduce 3 157)
    (376, Token (RBRACE _)) -> Just (Reduce 3 157)
    (376, Token (RPAREN _)) -> Just (Reduce 3 157)
    (376, Token (COMMA _)) -> Just (Reduce 3 157)
    (376, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (376, Token (EQUAL _)) -> Just (Reduce 3 157)
    (376, Token (PIPE _)) -> Just (Reduce 3 157)
    (376, Token (LARROW _)) -> Just (Reduce 3 157)
    (376, Token (THEN _)) -> Just (Reduce 3 157)
    (376, Token (ELSE _)) -> Just (Reduce 3 157)
    (376, Token (OF _)) -> Just (Reduce 3 157)
    (377, Token (WHERE _)) -> Just (Reduce 4 154)
    (377, Token (RBRACE _)) -> Just (Reduce 4 154)
    (377, Token (RPAREN _)) -> Just (Reduce 4 154)
    (377, Token (COMMA _)) -> Just (Reduce 4 154)
    (377, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (377, Token (EQUAL _)) -> Just (Reduce 4 154)
    (377, Token (PIPE _)) -> Just (Reduce 4 154)
    (377, Token (LARROW _)) -> Just (Reduce 4 154)
    (377, Token (THEN _)) -> Just (Reduce 4 154)
    (377, Token (ELSE _)) -> Just (Reduce 4 154)
    (377, Token (OF _)) -> Just (Reduce 4 154)
    (378, Token (WHERE _)) -> Just (Reduce 4 155)
    (378, Token (RBRACE _)) -> Just (Reduce 4 155)
    (378, Token (RPAREN _)) -> Just (Reduce 4 155)
    (378, Token (COMMA _)) -> Just (Reduce 4 155)
    (378, Token (SEMICOLON _)) -> Just (Reduce 4 155)
    (378, Token (EQUAL _)) -> Just (Reduce 4 155)
    (378, Token (PIPE _)) -> Just (Reduce 4 155)
    (378, Token (LARROW _)) -> Just (Reduce 4 155)
    (378, Token (THEN _)) -> Just (Reduce 4 155)
    (378, Token (ELSE _)) -> Just (Reduce 4 155)
    (378, Token (OF _)) -> Just (Reduce 4 155)
    (379, Token (SEMICOLON _)) -> Just (Shift 391)
    (379, Token (THEN _)) -> Just (Reduce 0 233)
    (380, Token (SEMICOLON _)) -> Just (Shift 391)
    (380, Token (ELSE _)) -> Just (Reduce 0 233)
    (381, Token (WHERE _)) -> Just (Reduce 8 156)
    (381, Token (RBRACE _)) -> Just (Reduce 8 156)
    (381, Token (RPAREN _)) -> Just (Reduce 8 156)
    (381, Token (COMMA _)) -> Just (Reduce 8 156)
    (381, Token (SEMICOLON _)) -> Just (Reduce 8 156)
    (381, Token (EQUAL _)) -> Just (Reduce 8 156)
    (381, Token (PIPE _)) -> Just (Reduce 8 156)
    (381, Token (LARROW _)) -> Just (Reduce 8 156)
    (381, Token (THEN _)) -> Just (Reduce 8 156)
    (381, Token (ELSE _)) -> Just (Reduce 8 156)
    (381, Token (OF _)) -> Just (Reduce 8 156)
    (382, Token (WHERE _)) -> Just (Reduce 3 158)
    (382, Token (RBRACE _)) -> Just (Reduce 3 158)
    (382, Token (RPAREN _)) -> Just (Reduce 3 158)
    (382, Token (COMMA _)) -> Just (Reduce 3 158)
    (382, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (382, Token (EQUAL _)) -> Just (Reduce 3 158)
    (382, Token (PIPE _)) -> Just (Reduce 3 158)
    (382, Token (LARROW _)) -> Just (Reduce 3 158)
    (382, Token (THEN _)) -> Just (Reduce 3 158)
    (382, Token (ELSE _)) -> Just (Reduce 3 158)
    (382, Token (OF _)) -> Just (Reduce 3 158)
    (383, Token (WHERE _)) -> Just (Reduce 5 163)
    (383, Token (RBRACE _)) -> Just (Reduce 5 163)
    (383, Token (RPAREN _)) -> Just (Reduce 5 163)
    (383, Token (COMMA _)) -> Just (Reduce 5 163)
    (383, Token (SEMICOLON _)) -> Just (Reduce 5 163)
    (383, Token (EQUAL _)) -> Just (Reduce 5 163)
    (383, Token (PIPE _)) -> Just (Reduce 5 163)
    (383, Token (LARROW _)) -> Just (Reduce 5 163)
    (383, Token (THEN _)) -> Just (Reduce 5 163)
    (383, Token (ELSE _)) -> Just (Reduce 5 163)
    (383, Token (OF _)) -> Just (Reduce 5 163)
    (384, Token (WHERE _)) -> Just (Reduce 5 160)
    (384, Token (RBRACE _)) -> Just (Reduce 5 160)
    (384, Token (RPAREN _)) -> Just (Reduce 5 160)
    (384, Token (COMMA _)) -> Just (Reduce 5 160)
    (384, Token (SEMICOLON _)) -> Just (Reduce 5 160)
    (384, Token (EQUAL _)) -> Just (Reduce 5 160)
    (384, Token (PIPE _)) -> Just (Reduce 5 160)
    (384, Token (LARROW _)) -> Just (Reduce 5 160)
    (384, Token (THEN _)) -> Just (Reduce 5 160)
    (384, Token (ELSE _)) -> Just (Reduce 5 160)
    (384, Token (OF _)) -> Just (Reduce 5 160)
    (385, Token (WHERE _)) -> Just (Reduce 5 159)
    (385, Token (RBRACE _)) -> Just (Reduce 5 159)
    (385, Token (RPAREN _)) -> Just (Reduce 5 159)
    (385, Token (COMMA _)) -> Just (Reduce 5 159)
    (385, Token (SEMICOLON _)) -> Just (Reduce 5 159)
    (385, Token (EQUAL _)) -> Just (Reduce 5 159)
    (385, Token (PIPE _)) -> Just (Reduce 5 159)
    (385, Token (LARROW _)) -> Just (Reduce 5 159)
    (385, Token (THEN _)) -> Just (Reduce 5 159)
    (385, Token (ELSE _)) -> Just (Reduce 5 159)
    (385, Token (OF _)) -> Just (Reduce 5 159)
    (386, Token (WHERE _)) -> Just (Reduce 5 161)
    (386, Token (RBRACE _)) -> Just (Reduce 5 161)
    (386, Token (RPAREN _)) -> Just (Reduce 5 161)
    (386, Token (COMMA _)) -> Just (Reduce 5 161)
    (386, Token (SEMICOLON _)) -> Just (Reduce 5 161)
    (386, Token (EQUAL _)) -> Just (Reduce 5 161)
    (386, Token (PIPE _)) -> Just (Reduce 5 161)
    (386, Token (LARROW _)) -> Just (Reduce 5 161)
    (386, Token (THEN _)) -> Just (Reduce 5 161)
    (386, Token (ELSE _)) -> Just (Reduce 5 161)
    (386, Token (OF _)) -> Just (Reduce 5 161)
    (387, Token (WHERE _)) -> Just (Reduce 3 162)
    (387, Token (RBRACE _)) -> Just (Reduce 3 162)
    (387, Token (RPAREN _)) -> Just (Reduce 3 162)
    (387, Token (COMMA _)) -> Just (Reduce 3 162)
    (387, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (387, Token (EQUAL _)) -> Just (Reduce 3 162)
    (387, Token (PIPE _)) -> Just (Reduce 3 162)
    (387, Token (LARROW _)) -> Just (Reduce 3 162)
    (387, Token (THEN _)) -> Just (Reduce 3 162)
    (387, Token (ELSE _)) -> Just (Reduce 3 162)
    (387, Token (OF _)) -> Just (Reduce 3 162)
    (388, Token (THEN _)) -> Just (Shift 68)
    (389, Token (ELSE _)) -> Just (Shift 37)
    (390, Token (WHERE _)) -> Just (Reduce 1 166)
    (390, Token (RBRACE _)) -> Just (Reduce 1 166)
    (390, Token (RPAREN _)) -> Just (Reduce 1 166)
    (390, Token (COMMA _)) -> Just (Reduce 1 166)
    (390, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (390, Token (EQUAL _)) -> Just (Reduce 1 166)
    (390, Token (PIPE _)) -> Just (Reduce 1 166)
    (390, Token (COLON_COLON _)) -> Just (Shift 119)
    (390, Token (MINUS _)) -> Just (Shift 34)
    (390, Token (LARROW _)) -> Just (Reduce 1 166)
    (390, Token (THEN _)) -> Just (Reduce 1 166)
    (390, Token (ELSE _)) -> Just (Reduce 1 166)
    (390, Token (QVARSYM _)) -> Just (Shift 38)
    (390, Token (BACKQUOTE _)) -> Just (Shift 371)
    (390, Token (QCONSYM _)) -> Just (Shift 43)
    (390, Token (OF _)) -> Just (Reduce 1 166)
    (391, Token (THEN _)) -> Just (Reduce 1 234)
    (391, Token (ELSE _)) -> Just (Reduce 1 234)
    (392, Token (WHERE _)) -> Just (Reduce 6 168)
    (392, Token (RBRACE _)) -> Just (Reduce 6 168)
    (392, Token (RPAREN _)) -> Just (Reduce 6 168)
    (392, Token (COMMA _)) -> Just (Reduce 6 168)
    (392, Token (SEMICOLON _)) -> Just (Reduce 6 168)
    (392, Token (EQUAL _)) -> Just (Reduce 6 168)
    (392, Token (PIPE _)) -> Just (Reduce 6 168)
    (392, Token (COLON_COLON _)) -> Just (Reduce 6 168)
    (392, Token (MINUS _)) -> Just (Reduce 6 168)
    (392, Token (LARROW _)) -> Just (Reduce 6 168)
    (392, Token (THEN _)) -> Just (Reduce 6 168)
    (392, Token (ELSE _)) -> Just (Reduce 6 168)
    (392, Token (QVARSYM _)) -> Just (Reduce 6 168)
    (392, Token (BACKQUOTE _)) -> Just (Reduce 6 168)
    (392, Token (QCONSYM _)) -> Just (Reduce 6 168)
    (392, Token (OF _)) -> Just (Reduce 6 168)
    (393, Token (WHERE _)) -> Just (Reduce 4 169)
    (393, Token (RBRACE _)) -> Just (Reduce 4 169)
    (393, Token (RPAREN _)) -> Just (Reduce 4 169)
    (393, Token (COMMA _)) -> Just (Reduce 4 169)
    (393, Token (SEMICOLON _)) -> Just (Reduce 4 169)
    (393, Token (EQUAL _)) -> Just (Reduce 4 169)
    (393, Token (PIPE _)) -> Just (Reduce 4 169)
    (393, Token (COLON_COLON _)) -> Just (Reduce 4 169)
    (393, Token (MINUS _)) -> Just (Reduce 4 169)
    (393, Token (LARROW _)) -> Just (Reduce 4 169)
    (393, Token (THEN _)) -> Just (Reduce 4 169)
    (393, Token (ELSE _)) -> Just (Reduce 4 169)
    (393, Token (QVARSYM _)) -> Just (Reduce 4 169)
    (393, Token (BACKQUOTE _)) -> Just (Reduce 4 169)
    (393, Token (QCONSYM _)) -> Just (Reduce 4 169)
    (393, Token (OF _)) -> Just (Reduce 4 169)
    (394, Token (LBRACE _)) -> Just (Shift 85)
    (395, Token (LBRACE _)) -> Just (Shift 51)
    (396, Token (OF _)) -> Just (Shift 394)
    (397, Token (WHERE _)) -> Just (Reduce 2 167)
    (397, Token (RBRACE _)) -> Just (Reduce 2 167)
    (397, Token (RPAREN _)) -> Just (Reduce 2 167)
    (397, Token (COMMA _)) -> Just (Reduce 2 167)
    (397, Token (SEMICOLON _)) -> Just (Reduce 2 167)
    (397, Token (EQUAL _)) -> Just (Reduce 2 167)
    (397, Token (PIPE _)) -> Just (Reduce 2 167)
    (397, Token (COLON_COLON _)) -> Just (Reduce 2 167)
    (397, Token (MINUS _)) -> Just (Reduce 2 167)
    (397, Token (LARROW _)) -> Just (Reduce 2 167)
    (397, Token (THEN _)) -> Just (Reduce 2 167)
    (397, Token (ELSE _)) -> Just (Reduce 2 167)
    (397, Token (QVARSYM _)) -> Just (Reduce 2 167)
    (397, Token (BACKQUOTE _)) -> Just (Reduce 2 167)
    (397, Token (QCONSYM _)) -> Just (Reduce 2 167)
    (397, Token (OF _)) -> Just (Reduce 2 167)
    (398, Token (RBRACE _)) -> Just (Shift 392)
    (399, Token (RBRACE _)) -> Just (Shift 393)
    (400, Token (RBRACE _)) -> Just (Reduce 3 184)
    (401, Token (RBRACE _)) -> Just (Reduce 1 183)
    (401, Token (SEMICOLON _)) -> Just (Shift 86)
    (402, Token (RBRACE _)) -> Just (Reduce 3 196)
    (403, Token (RBRACE _)) -> Just (Reduce 1 195)
    (403, Token (SEMICOLON _)) -> Just (Shift 52)
    (404, Token (WHERE _)) -> Just (Reduce 1 171)
    (404, Token (LBRACE _)) -> Just (Reduce 1 171)
    (404, Token (RBRACE _)) -> Just (Reduce 1 171)
    (404, Token (LPAREN _)) -> Just (Reduce 1 171)
    (404, Token (RPAREN _)) -> Just (Reduce 1 171)
    (404, Token (COMMA _)) -> Just (Reduce 1 171)
    (404, Token (SEMICOLON _)) -> Just (Reduce 1 171)
    (404, Token (EQUAL _)) -> Just (Reduce 1 171)
    (404, Token (PIPE _)) -> Just (Reduce 1 171)
    (404, Token (COLON_COLON _)) -> Just (Reduce 1 171)
    (404, Token (MINUS _)) -> Just (Reduce 1 171)
    (404, Token (INFIXL _)) -> Just (Reduce 1 171)
    (404, Token (INFIXR _)) -> Just (Reduce 1 171)
    (404, Token (INFIX _)) -> Just (Reduce 1 171)
    (404, Token (QCONID _)) -> Just (Reduce 1 171)
    (404, Token (EXPORT _)) -> Just (Reduce 1 171)
    (404, Token (AS _)) -> Just (Reduce 1 171)
    (404, Token (QVARID _)) -> Just (Reduce 1 171)
    (404, Token (STRING _)) -> Just (Reduce 1 171)
    (404, Token (LARROW _)) -> Just (Reduce 1 171)
    (404, Token (LET _)) -> Just (Reduce 1 171)
    (404, Token (LAMBDA _)) -> Just (Reduce 1 171)
    (404, Token (IF _)) -> Just (Reduce 1 171)
    (404, Token (THEN _)) -> Just (Reduce 1 171)
    (404, Token (ELSE _)) -> Just (Reduce 1 171)
    (404, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (404, Token (BACKQUOTE _)) -> Just (Reduce 1 171)
    (404, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (404, Token (CASE _)) -> Just (Reduce 1 171)
    (404, Token (OF _)) -> Just (Reduce 1 171)
    (404, Token (DO _)) -> Just (Reduce 1 171)
    (404, Token (INTEGER _)) -> Just (Reduce 1 171)
    (405, Token (WHERE _)) -> Just (Reduce 2 172)
    (405, Token (LBRACE _)) -> Just (Reduce 2 172)
    (405, Token (RBRACE _)) -> Just (Reduce 2 172)
    (405, Token (LPAREN _)) -> Just (Reduce 2 172)
    (405, Token (RPAREN _)) -> Just (Reduce 2 172)
    (405, Token (COMMA _)) -> Just (Reduce 2 172)
    (405, Token (SEMICOLON _)) -> Just (Reduce 2 172)
    (405, Token (EQUAL _)) -> Just (Reduce 2 172)
    (405, Token (PIPE _)) -> Just (Reduce 2 172)
    (405, Token (COLON_COLON _)) -> Just (Reduce 2 172)
    (405, Token (MINUS _)) -> Just (Reduce 2 172)
    (405, Token (INFIXL _)) -> Just (Reduce 2 172)
    (405, Token (INFIXR _)) -> Just (Reduce 2 172)
    (405, Token (INFIX _)) -> Just (Reduce 2 172)
    (405, Token (QCONID _)) -> Just (Reduce 2 172)
    (405, Token (EXPORT _)) -> Just (Reduce 2 172)
    (405, Token (AS _)) -> Just (Reduce 2 172)
    (405, Token (QVARID _)) -> Just (Reduce 2 172)
    (405, Token (STRING _)) -> Just (Reduce 2 172)
    (405, Token (LARROW _)) -> Just (Reduce 2 172)
    (405, Token (LET _)) -> Just (Reduce 2 172)
    (405, Token (LAMBDA _)) -> Just (Reduce 2 172)
    (405, Token (IF _)) -> Just (Reduce 2 172)
    (405, Token (THEN _)) -> Just (Reduce 2 172)
    (405, Token (ELSE _)) -> Just (Reduce 2 172)
    (405, Token (QVARSYM _)) -> Just (Reduce 2 172)
    (405, Token (BACKQUOTE _)) -> Just (Reduce 2 172)
    (405, Token (QCONSYM _)) -> Just (Reduce 2 172)
    (405, Token (CASE _)) -> Just (Reduce 2 172)
    (405, Token (OF _)) -> Just (Reduce 2 172)
    (405, Token (DO _)) -> Just (Reduce 2 172)
    (405, Token (INTEGER _)) -> Just (Reduce 2 172)
    (406, Token (WHERE _)) -> Just (Reduce 3 176)
    (406, Token (LBRACE _)) -> Just (Reduce 3 176)
    (406, Token (RBRACE _)) -> Just (Reduce 3 176)
    (406, Token (LPAREN _)) -> Just (Reduce 3 176)
    (406, Token (RPAREN _)) -> Just (Reduce 3 176)
    (406, Token (COMMA _)) -> Just (Reduce 3 176)
    (406, Token (SEMICOLON _)) -> Just (Reduce 3 176)
    (406, Token (EQUAL _)) -> Just (Reduce 3 176)
    (406, Token (PIPE _)) -> Just (Reduce 3 176)
    (406, Token (COLON_COLON _)) -> Just (Reduce 3 176)
    (406, Token (MINUS _)) -> Just (Reduce 3 176)
    (406, Token (INFIXL _)) -> Just (Reduce 3 176)
    (406, Token (INFIXR _)) -> Just (Reduce 3 176)
    (406, Token (INFIX _)) -> Just (Reduce 3 176)
    (406, Token (QCONID _)) -> Just (Reduce 3 176)
    (406, Token (EXPORT _)) -> Just (Reduce 3 176)
    (406, Token (AS _)) -> Just (Reduce 3 176)
    (406, Token (QVARID _)) -> Just (Reduce 3 176)
    (406, Token (STRING _)) -> Just (Reduce 3 176)
    (406, Token (LARROW _)) -> Just (Reduce 3 176)
    (406, Token (LET _)) -> Just (Reduce 3 176)
    (406, Token (LAMBDA _)) -> Just (Reduce 3 176)
    (406, Token (IF _)) -> Just (Reduce 3 176)
    (406, Token (THEN _)) -> Just (Reduce 3 176)
    (406, Token (ELSE _)) -> Just (Reduce 3 176)
    (406, Token (QVARSYM _)) -> Just (Reduce 3 176)
    (406, Token (BACKQUOTE _)) -> Just (Reduce 3 176)
    (406, Token (QCONSYM _)) -> Just (Reduce 3 176)
    (406, Token (CASE _)) -> Just (Reduce 3 176)
    (406, Token (OF _)) -> Just (Reduce 3 176)
    (406, Token (DO _)) -> Just (Reduce 3 176)
    (406, Token (INTEGER _)) -> Just (Reduce 3 176)
    (407, Token (WHERE _)) -> Just (Reduce 4 177)
    (407, Token (LBRACE _)) -> Just (Reduce 4 177)
    (407, Token (RBRACE _)) -> Just (Reduce 4 177)
    (407, Token (LPAREN _)) -> Just (Reduce 4 177)
    (407, Token (RPAREN _)) -> Just (Reduce 4 177)
    (407, Token (COMMA _)) -> Just (Reduce 4 177)
    (407, Token (SEMICOLON _)) -> Just (Reduce 4 177)
    (407, Token (EQUAL _)) -> Just (Reduce 4 177)
    (407, Token (PIPE _)) -> Just (Reduce 4 177)
    (407, Token (COLON_COLON _)) -> Just (Reduce 4 177)
    (407, Token (MINUS _)) -> Just (Reduce 4 177)
    (407, Token (INFIXL _)) -> Just (Reduce 4 177)
    (407, Token (INFIXR _)) -> Just (Reduce 4 177)
    (407, Token (INFIX _)) -> Just (Reduce 4 177)
    (407, Token (QCONID _)) -> Just (Reduce 4 177)
    (407, Token (EXPORT _)) -> Just (Reduce 4 177)
    (407, Token (AS _)) -> Just (Reduce 4 177)
    (407, Token (QVARID _)) -> Just (Reduce 4 177)
    (407, Token (STRING _)) -> Just (Reduce 4 177)
    (407, Token (LARROW _)) -> Just (Reduce 4 177)
    (407, Token (LET _)) -> Just (Reduce 4 177)
    (407, Token (LAMBDA _)) -> Just (Reduce 4 177)
    (407, Token (IF _)) -> Just (Reduce 4 177)
    (407, Token (THEN _)) -> Just (Reduce 4 177)
    (407, Token (ELSE _)) -> Just (Reduce 4 177)
    (407, Token (QVARSYM _)) -> Just (Reduce 4 177)
    (407, Token (BACKQUOTE _)) -> Just (Reduce 4 177)
    (407, Token (QCONSYM _)) -> Just (Reduce 4 177)
    (407, Token (CASE _)) -> Just (Reduce 4 177)
    (407, Token (OF _)) -> Just (Reduce 4 177)
    (407, Token (DO _)) -> Just (Reduce 4 177)
    (407, Token (INTEGER _)) -> Just (Reduce 4 177)
    (408, Token (WHERE _)) -> Just (Reduce 6 182)
    (408, Token (LBRACE _)) -> Just (Reduce 6 182)
    (408, Token (RBRACE _)) -> Just (Reduce 6 182)
    (408, Token (LPAREN _)) -> Just (Reduce 6 182)
    (408, Token (RPAREN _)) -> Just (Reduce 6 182)
    (408, Token (COMMA _)) -> Just (Reduce 6 182)
    (408, Token (SEMICOLON _)) -> Just (Reduce 6 182)
    (408, Token (EQUAL _)) -> Just (Reduce 6 182)
    (408, Token (PIPE _)) -> Just (Reduce 6 182)
    (408, Token (COLON_COLON _)) -> Just (Reduce 6 182)
    (408, Token (MINUS _)) -> Just (Reduce 6 182)
    (408, Token (INFIXL _)) -> Just (Reduce 6 182)
    (408, Token (INFIXR _)) -> Just (Reduce 6 182)
    (408, Token (INFIX _)) -> Just (Reduce 6 182)
    (408, Token (QCONID _)) -> Just (Reduce 6 182)
    (408, Token (EXPORT _)) -> Just (Reduce 6 182)
    (408, Token (AS _)) -> Just (Reduce 6 182)
    (408, Token (QVARID _)) -> Just (Reduce 6 182)
    (408, Token (STRING _)) -> Just (Reduce 6 182)
    (408, Token (LARROW _)) -> Just (Reduce 6 182)
    (408, Token (LET _)) -> Just (Reduce 6 182)
    (408, Token (LAMBDA _)) -> Just (Reduce 6 182)
    (408, Token (IF _)) -> Just (Reduce 6 182)
    (408, Token (THEN _)) -> Just (Reduce 6 182)
    (408, Token (ELSE _)) -> Just (Reduce 6 182)
    (408, Token (QVARSYM _)) -> Just (Reduce 6 182)
    (408, Token (BACKQUOTE _)) -> Just (Reduce 6 182)
    (408, Token (QCONSYM _)) -> Just (Reduce 6 182)
    (408, Token (CASE _)) -> Just (Reduce 6 182)
    (408, Token (OF _)) -> Just (Reduce 6 182)
    (408, Token (DO _)) -> Just (Reduce 6 182)
    (408, Token (INTEGER _)) -> Just (Reduce 6 182)
    (409, Token (WHERE _)) -> Just (Reduce 6 179)
    (409, Token (LBRACE _)) -> Just (Reduce 6 179)
    (409, Token (RBRACE _)) -> Just (Reduce 6 179)
    (409, Token (LPAREN _)) -> Just (Reduce 6 179)
    (409, Token (RPAREN _)) -> Just (Reduce 6 179)
    (409, Token (COMMA _)) -> Just (Reduce 6 179)
    (409, Token (SEMICOLON _)) -> Just (Reduce 6 179)
    (409, Token (EQUAL _)) -> Just (Reduce 6 179)
    (409, Token (PIPE _)) -> Just (Reduce 6 179)
    (409, Token (COLON_COLON _)) -> Just (Reduce 6 179)
    (409, Token (MINUS _)) -> Just (Reduce 6 179)
    (409, Token (INFIXL _)) -> Just (Reduce 6 179)
    (409, Token (INFIXR _)) -> Just (Reduce 6 179)
    (409, Token (INFIX _)) -> Just (Reduce 6 179)
    (409, Token (QCONID _)) -> Just (Reduce 6 179)
    (409, Token (EXPORT _)) -> Just (Reduce 6 179)
    (409, Token (AS _)) -> Just (Reduce 6 179)
    (409, Token (QVARID _)) -> Just (Reduce 6 179)
    (409, Token (STRING _)) -> Just (Reduce 6 179)
    (409, Token (LARROW _)) -> Just (Reduce 6 179)
    (409, Token (LET _)) -> Just (Reduce 6 179)
    (409, Token (LAMBDA _)) -> Just (Reduce 6 179)
    (409, Token (IF _)) -> Just (Reduce 6 179)
    (409, Token (THEN _)) -> Just (Reduce 6 179)
    (409, Token (ELSE _)) -> Just (Reduce 6 179)
    (409, Token (QVARSYM _)) -> Just (Reduce 6 179)
    (409, Token (BACKQUOTE _)) -> Just (Reduce 6 179)
    (409, Token (QCONSYM _)) -> Just (Reduce 6 179)
    (409, Token (CASE _)) -> Just (Reduce 6 179)
    (409, Token (OF _)) -> Just (Reduce 6 179)
    (409, Token (DO _)) -> Just (Reduce 6 179)
    (409, Token (INTEGER _)) -> Just (Reduce 6 179)
    (410, Token (WHERE _)) -> Just (Reduce 6 178)
    (410, Token (LBRACE _)) -> Just (Reduce 6 178)
    (410, Token (RBRACE _)) -> Just (Reduce 6 178)
    (410, Token (LPAREN _)) -> Just (Reduce 6 178)
    (410, Token (RPAREN _)) -> Just (Reduce 6 178)
    (410, Token (COMMA _)) -> Just (Reduce 6 178)
    (410, Token (SEMICOLON _)) -> Just (Reduce 6 178)
    (410, Token (EQUAL _)) -> Just (Reduce 6 178)
    (410, Token (PIPE _)) -> Just (Reduce 6 178)
    (410, Token (COLON_COLON _)) -> Just (Reduce 6 178)
    (410, Token (MINUS _)) -> Just (Reduce 6 178)
    (410, Token (INFIXL _)) -> Just (Reduce 6 178)
    (410, Token (INFIXR _)) -> Just (Reduce 6 178)
    (410, Token (INFIX _)) -> Just (Reduce 6 178)
    (410, Token (QCONID _)) -> Just (Reduce 6 178)
    (410, Token (EXPORT _)) -> Just (Reduce 6 178)
    (410, Token (AS _)) -> Just (Reduce 6 178)
    (410, Token (QVARID _)) -> Just (Reduce 6 178)
    (410, Token (STRING _)) -> Just (Reduce 6 178)
    (410, Token (LARROW _)) -> Just (Reduce 6 178)
    (410, Token (LET _)) -> Just (Reduce 6 178)
    (410, Token (LAMBDA _)) -> Just (Reduce 6 178)
    (410, Token (IF _)) -> Just (Reduce 6 178)
    (410, Token (THEN _)) -> Just (Reduce 6 178)
    (410, Token (ELSE _)) -> Just (Reduce 6 178)
    (410, Token (QVARSYM _)) -> Just (Reduce 6 178)
    (410, Token (BACKQUOTE _)) -> Just (Reduce 6 178)
    (410, Token (QCONSYM _)) -> Just (Reduce 6 178)
    (410, Token (CASE _)) -> Just (Reduce 6 178)
    (410, Token (OF _)) -> Just (Reduce 6 178)
    (410, Token (DO _)) -> Just (Reduce 6 178)
    (410, Token (INTEGER _)) -> Just (Reduce 6 178)
    (411, Token (WHERE _)) -> Just (Reduce 6 180)
    (411, Token (LBRACE _)) -> Just (Reduce 6 180)
    (411, Token (RBRACE _)) -> Just (Reduce 6 180)
    (411, Token (LPAREN _)) -> Just (Reduce 6 180)
    (411, Token (RPAREN _)) -> Just (Reduce 6 180)
    (411, Token (COMMA _)) -> Just (Reduce 6 180)
    (411, Token (SEMICOLON _)) -> Just (Reduce 6 180)
    (411, Token (EQUAL _)) -> Just (Reduce 6 180)
    (411, Token (PIPE _)) -> Just (Reduce 6 180)
    (411, Token (COLON_COLON _)) -> Just (Reduce 6 180)
    (411, Token (MINUS _)) -> Just (Reduce 6 180)
    (411, Token (INFIXL _)) -> Just (Reduce 6 180)
    (411, Token (INFIXR _)) -> Just (Reduce 6 180)
    (411, Token (INFIX _)) -> Just (Reduce 6 180)
    (411, Token (QCONID _)) -> Just (Reduce 6 180)
    (411, Token (EXPORT _)) -> Just (Reduce 6 180)
    (411, Token (AS _)) -> Just (Reduce 6 180)
    (411, Token (QVARID _)) -> Just (Reduce 6 180)
    (411, Token (STRING _)) -> Just (Reduce 6 180)
    (411, Token (LARROW _)) -> Just (Reduce 6 180)
    (411, Token (LET _)) -> Just (Reduce 6 180)
    (411, Token (LAMBDA _)) -> Just (Reduce 6 180)
    (411, Token (IF _)) -> Just (Reduce 6 180)
    (411, Token (THEN _)) -> Just (Reduce 6 180)
    (411, Token (ELSE _)) -> Just (Reduce 6 180)
    (411, Token (QVARSYM _)) -> Just (Reduce 6 180)
    (411, Token (BACKQUOTE _)) -> Just (Reduce 6 180)
    (411, Token (QCONSYM _)) -> Just (Reduce 6 180)
    (411, Token (CASE _)) -> Just (Reduce 6 180)
    (411, Token (OF _)) -> Just (Reduce 6 180)
    (411, Token (DO _)) -> Just (Reduce 6 180)
    (411, Token (INTEGER _)) -> Just (Reduce 6 180)
    (412, Token (WHERE _)) -> Just (Reduce 4 181)
    (412, Token (LBRACE _)) -> Just (Reduce 4 181)
    (412, Token (RBRACE _)) -> Just (Reduce 4 181)
    (412, Token (LPAREN _)) -> Just (Reduce 4 181)
    (412, Token (RPAREN _)) -> Just (Reduce 4 181)
    (412, Token (COMMA _)) -> Just (Reduce 4 181)
    (412, Token (SEMICOLON _)) -> Just (Reduce 4 181)
    (412, Token (EQUAL _)) -> Just (Reduce 4 181)
    (412, Token (PIPE _)) -> Just (Reduce 4 181)
    (412, Token (COLON_COLON _)) -> Just (Reduce 4 181)
    (412, Token (MINUS _)) -> Just (Reduce 4 181)
    (412, Token (INFIXL _)) -> Just (Reduce 4 181)
    (412, Token (INFIXR _)) -> Just (Reduce 4 181)
    (412, Token (INFIX _)) -> Just (Reduce 4 181)
    (412, Token (QCONID _)) -> Just (Reduce 4 181)
    (412, Token (EXPORT _)) -> Just (Reduce 4 181)
    (412, Token (AS _)) -> Just (Reduce 4 181)
    (412, Token (QVARID _)) -> Just (Reduce 4 181)
    (412, Token (STRING _)) -> Just (Reduce 4 181)
    (412, Token (LARROW _)) -> Just (Reduce 4 181)
    (412, Token (LET _)) -> Just (Reduce 4 181)
    (412, Token (LAMBDA _)) -> Just (Reduce 4 181)
    (412, Token (IF _)) -> Just (Reduce 4 181)
    (412, Token (THEN _)) -> Just (Reduce 4 181)
    (412, Token (ELSE _)) -> Just (Reduce 4 181)
    (412, Token (QVARSYM _)) -> Just (Reduce 4 181)
    (412, Token (BACKQUOTE _)) -> Just (Reduce 4 181)
    (412, Token (QCONSYM _)) -> Just (Reduce 4 181)
    (412, Token (CASE _)) -> Just (Reduce 4 181)
    (412, Token (OF _)) -> Just (Reduce 4 181)
    (412, Token (DO _)) -> Just (Reduce 4 181)
    (412, Token (INTEGER _)) -> Just (Reduce 4 181)
    (413, Token (BACKQUOTE _)) -> Just (Shift 57)
    (414, Token (BACKQUOTE _)) -> Just (Shift 58)
    (415, Token (BACKQUOTE _)) -> Just (Shift 59)
    (416, Token (BACKQUOTE _)) -> Just (Shift 60)
    (417, Token (WHERE _)) -> Just (Reduce 1 175)
    (417, Token (LBRACE _)) -> Just (Reduce 1 175)
    (417, Token (RBRACE _)) -> Just (Reduce 1 175)
    (417, Token (LPAREN _)) -> Just (Reduce 1 175)
    (417, Token (RPAREN _)) -> Just (Reduce 1 175)
    (417, Token (COMMA _)) -> Just (Reduce 1 175)
    (417, Token (SEMICOLON _)) -> Just (Reduce 1 175)
    (417, Token (EQUAL _)) -> Just (Reduce 1 175)
    (417, Token (PIPE _)) -> Just (Reduce 1 175)
    (417, Token (COLON_COLON _)) -> Just (Reduce 1 175)
    (417, Token (MINUS _)) -> Just (Reduce 1 175)
    (417, Token (INFIXL _)) -> Just (Reduce 1 175)
    (417, Token (INFIXR _)) -> Just (Reduce 1 175)
    (417, Token (INFIX _)) -> Just (Reduce 1 175)
    (417, Token (QCONID _)) -> Just (Reduce 1 175)
    (417, Token (EXPORT _)) -> Just (Reduce 1 175)
    (417, Token (AS _)) -> Just (Reduce 1 175)
    (417, Token (QVARID _)) -> Just (Reduce 1 175)
    (417, Token (STRING _)) -> Just (Reduce 1 175)
    (417, Token (LARROW _)) -> Just (Reduce 1 175)
    (417, Token (LET _)) -> Just (Reduce 1 175)
    (417, Token (LAMBDA _)) -> Just (Reduce 1 175)
    (417, Token (IF _)) -> Just (Reduce 1 175)
    (417, Token (THEN _)) -> Just (Reduce 1 175)
    (417, Token (ELSE _)) -> Just (Reduce 1 175)
    (417, Token (QVARSYM _)) -> Just (Reduce 1 175)
    (417, Token (BACKQUOTE _)) -> Just (Reduce 1 175)
    (417, Token (QCONSYM _)) -> Just (Reduce 1 175)
    (417, Token (CASE _)) -> Just (Reduce 1 175)
    (417, Token (OF _)) -> Just (Reduce 1 175)
    (417, Token (DO _)) -> Just (Reduce 1 175)
    (417, Token (INTEGER _)) -> Just (Reduce 1 175)
    (418, Token (QCONID _)) -> Just (Shift 413)
    (418, Token (EXPORT _)) -> Just (Shift 414)
    (418, Token (AS _)) -> Just (Shift 415)
    (418, Token (QVARID _)) -> Just (Shift 416)
    (419, Token (WHERE _)) -> Just (Reduce 1 174)
    (419, Token (LBRACE _)) -> Just (Reduce 1 174)
    (419, Token (RBRACE _)) -> Just (Reduce 1 174)
    (419, Token (LPAREN _)) -> Just (Reduce 1 174)
    (419, Token (RPAREN _)) -> Just (Reduce 1 174)
    (419, Token (COMMA _)) -> Just (Reduce 1 174)
    (419, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (419, Token (EQUAL _)) -> Just (Reduce 1 174)
    (419, Token (PIPE _)) -> Just (Reduce 1 174)
    (419, Token (COLON_COLON _)) -> Just (Reduce 1 174)
    (419, Token (MINUS _)) -> Just (Reduce 1 174)
    (419, Token (INFIXL _)) -> Just (Reduce 1 174)
    (419, Token (INFIXR _)) -> Just (Reduce 1 174)
    (419, Token (INFIX _)) -> Just (Reduce 1 174)
    (419, Token (QCONID _)) -> Just (Reduce 1 174)
    (419, Token (EXPORT _)) -> Just (Reduce 1 174)
    (419, Token (AS _)) -> Just (Reduce 1 174)
    (419, Token (QVARID _)) -> Just (Reduce 1 174)
    (419, Token (STRING _)) -> Just (Reduce 1 174)
    (419, Token (LARROW _)) -> Just (Reduce 1 174)
    (419, Token (LET _)) -> Just (Reduce 1 174)
    (419, Token (LAMBDA _)) -> Just (Reduce 1 174)
    (419, Token (IF _)) -> Just (Reduce 1 174)
    (419, Token (THEN _)) -> Just (Reduce 1 174)
    (419, Token (ELSE _)) -> Just (Reduce 1 174)
    (419, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (419, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (419, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (419, Token (CASE _)) -> Just (Reduce 1 174)
    (419, Token (OF _)) -> Just (Reduce 1 174)
    (419, Token (DO _)) -> Just (Reduce 1 174)
    (419, Token (INTEGER _)) -> Just (Reduce 1 174)
    (420, Token (WHERE _)) -> Just (Reduce 1 173)
    (420, Token (LBRACE _)) -> Just (Reduce 1 173)
    (420, Token (RBRACE _)) -> Just (Reduce 1 173)
    (420, Token (LPAREN _)) -> Just (Reduce 1 173)
    (420, Token (RPAREN _)) -> Just (Reduce 1 173)
    (420, Token (COMMA _)) -> Just (Reduce 1 173)
    (420, Token (SEMICOLON _)) -> Just (Reduce 1 173)
    (420, Token (EQUAL _)) -> Just (Reduce 1 173)
    (420, Token (PIPE _)) -> Just (Reduce 1 173)
    (420, Token (COLON_COLON _)) -> Just (Reduce 1 173)
    (420, Token (MINUS _)) -> Just (Reduce 1 173)
    (420, Token (INFIXL _)) -> Just (Reduce 1 173)
    (420, Token (INFIXR _)) -> Just (Reduce 1 173)
    (420, Token (INFIX _)) -> Just (Reduce 1 173)
    (420, Token (QCONID _)) -> Just (Reduce 1 173)
    (420, Token (EXPORT _)) -> Just (Reduce 1 173)
    (420, Token (AS _)) -> Just (Reduce 1 173)
    (420, Token (QVARID _)) -> Just (Reduce 1 173)
    (420, Token (STRING _)) -> Just (Reduce 1 173)
    (420, Token (LARROW _)) -> Just (Reduce 1 173)
    (420, Token (LET _)) -> Just (Reduce 1 173)
    (420, Token (LAMBDA _)) -> Just (Reduce 1 173)
    (420, Token (IF _)) -> Just (Reduce 1 173)
    (420, Token (THEN _)) -> Just (Reduce 1 173)
    (420, Token (ELSE _)) -> Just (Reduce 1 173)
    (420, Token (QVARSYM _)) -> Just (Reduce 1 173)
    (420, Token (BACKQUOTE _)) -> Just (Reduce 1 173)
    (420, Token (QCONSYM _)) -> Just (Reduce 1 173)
    (420, Token (CASE _)) -> Just (Reduce 1 173)
    (420, Token (OF _)) -> Just (Reduce 1 173)
    (420, Token (DO _)) -> Just (Reduce 1 173)
    (420, Token (INTEGER _)) -> Just (Reduce 1 173)
    (421, Token (RPAREN _)) -> Just (Shift 406)
    (422, Token (RPAREN _)) -> Just (Shift 407)
    (423, Token (RPAREN _)) -> Just (Shift 408)
    (424, Token (RPAREN _)) -> Just (Shift 409)
    (425, Token (RPAREN _)) -> Just (Shift 410)
    (426, Token (RPAREN _)) -> Just (Shift 411)
    (427, Token (RPAREN _)) -> Just (Shift 412)
    (428, Token (RBRACE _)) -> Just (Reduce 5 187)
    (428, Token (SEMICOLON _)) -> Just (Reduce 5 187)
    (429, Token (WHERE _)) -> Just (Shift 263)
    (429, Token (RBRACE _)) -> Just (Reduce 3 186)
    (429, Token (SEMICOLON _)) -> Just (Reduce 3 186)
    (430, Token (RBRACE _)) -> Just (Reduce 1 198)
    (430, Token (SEMICOLON _)) -> Just (Reduce 1 198)
    (430, Token (LARROW _)) -> Just (Shift 53)
    (431, Token (RBRACE _)) -> Just (Reduce 3 199)
    (431, Token (SEMICOLON _)) -> Just (Reduce 3 199)
    (432, Token (LPAREN _)) -> Just (Reduce 3 206)
    (432, Token (RPAREN _)) -> Just (Reduce 3 206)
    (432, Token (EQUAL _)) -> Just (Reduce 3 206)
    (432, Token (PIPE _)) -> Just (Reduce 3 206)
    (432, Token (MINUS _)) -> Just (Reduce 3 206)
    (432, Token (RARROW _)) -> Just (Reduce 3 206)
    (432, Token (QCONID _)) -> Just (Reduce 3 206)
    (432, Token (EXPORT _)) -> Just (Reduce 3 206)
    (432, Token (AS _)) -> Just (Reduce 3 206)
    (432, Token (QVARID _)) -> Just (Reduce 3 206)
    (432, Token (QVARSYM _)) -> Just (Reduce 3 206)
    (432, Token (BACKQUOTE _)) -> Just (Reduce 3 206)
    (432, Token (QCONSYM _)) -> Just (Reduce 3 206)
    (433, Token (LPAREN _)) -> Just (Reduce 1 205)
    (433, Token (RPAREN _)) -> Just (Reduce 1 205)
    (433, Token (EQUAL _)) -> Just (Reduce 1 205)
    (433, Token (PIPE _)) -> Just (Reduce 1 205)
    (433, Token (MINUS _)) -> Just (Reduce 1 205)
    (433, Token (RARROW _)) -> Just (Reduce 1 205)
    (433, Token (QCONID _)) -> Just (Reduce 1 205)
    (433, Token (EXPORT _)) -> Just (Reduce 1 205)
    (433, Token (AS _)) -> Just (Reduce 1 205)
    (433, Token (QVARID _)) -> Just (Reduce 1 205)
    (433, Token (QVARSYM _)) -> Just (Reduce 1 205)
    (433, Token (BACKQUOTE _)) -> Just (Reduce 1 205)
    (433, Token (QCONSYM _)) -> Just (Reduce 1 205)
    (434, Token (BACKQUOTE _)) -> Just (Shift 438)
    (435, Token (BACKQUOTE _)) -> Just (Shift 439)
    (436, Token (BACKQUOTE _)) -> Just (Shift 440)
    (437, Token (RBRACE _)) -> Just (Reduce 1 214)
    (437, Token (LPAREN _)) -> Just (Reduce 1 214)
    (437, Token (COMMA _)) -> Just (Reduce 1 214)
    (437, Token (SEMICOLON _)) -> Just (Reduce 1 214)
    (437, Token (MINUS _)) -> Just (Reduce 1 214)
    (437, Token (QCONID _)) -> Just (Reduce 1 214)
    (437, Token (EXPORT _)) -> Just (Reduce 1 214)
    (437, Token (AS _)) -> Just (Reduce 1 214)
    (437, Token (QVARID _)) -> Just (Reduce 1 214)
    (437, Token (QVARSYM _)) -> Just (Reduce 1 214)
    (437, Token (BACKQUOTE _)) -> Just (Reduce 1 214)
    (437, Token (QCONSYM _)) -> Just (Reduce 1 214)
    (438, Token (RBRACE _)) -> Just (Reduce 3 216)
    (438, Token (LPAREN _)) -> Just (Reduce 3 216)
    (438, Token (COMMA _)) -> Just (Reduce 3 216)
    (438, Token (SEMICOLON _)) -> Just (Reduce 3 216)
    (438, Token (MINUS _)) -> Just (Reduce 3 216)
    (438, Token (QCONID _)) -> Just (Reduce 3 216)
    (438, Token (EXPORT _)) -> Just (Reduce 3 216)
    (438, Token (AS _)) -> Just (Reduce 3 216)
    (438, Token (QVARID _)) -> Just (Reduce 3 216)
    (438, Token (QVARSYM _)) -> Just (Reduce 3 216)
    (438, Token (BACKQUOTE _)) -> Just (Reduce 3 216)
    (438, Token (QCONSYM _)) -> Just (Reduce 3 216)
    (439, Token (RBRACE _)) -> Just (Reduce 3 215)
    (439, Token (LPAREN _)) -> Just (Reduce 3 215)
    (439, Token (COMMA _)) -> Just (Reduce 3 215)
    (439, Token (SEMICOLON _)) -> Just (Reduce 3 215)
    (439, Token (MINUS _)) -> Just (Reduce 3 215)
    (439, Token (QCONID _)) -> Just (Reduce 3 215)
    (439, Token (EXPORT _)) -> Just (Reduce 3 215)
    (439, Token (AS _)) -> Just (Reduce 3 215)
    (439, Token (QVARID _)) -> Just (Reduce 3 215)
    (439, Token (QVARSYM _)) -> Just (Reduce 3 215)
    (439, Token (BACKQUOTE _)) -> Just (Reduce 3 215)
    (439, Token (QCONSYM _)) -> Just (Reduce 3 215)
    (440, Token (RBRACE _)) -> Just (Reduce 3 217)
    (440, Token (LPAREN _)) -> Just (Reduce 3 217)
    (440, Token (COMMA _)) -> Just (Reduce 3 217)
    (440, Token (SEMICOLON _)) -> Just (Reduce 3 217)
    (440, Token (MINUS _)) -> Just (Reduce 3 217)
    (440, Token (QCONID _)) -> Just (Reduce 3 217)
    (440, Token (EXPORT _)) -> Just (Reduce 3 217)
    (440, Token (AS _)) -> Just (Reduce 3 217)
    (440, Token (QVARID _)) -> Just (Reduce 3 217)
    (440, Token (QVARSYM _)) -> Just (Reduce 3 217)
    (440, Token (BACKQUOTE _)) -> Just (Reduce 3 217)
    (440, Token (QCONSYM _)) -> Just (Reduce 3 217)
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
production 173 = 67
production 174 = 67
production 175 = 67
production 176 = 67
production 177 = 67
production 178 = 67
production 179 = 67
production 180 = 67
production 181 = 67
production 182 = 67
production 183 = 64
production 184 = 64
production 185 = 68
production 186 = 68
production 187 = 68
production 188 = 69
production 189 = 69
production 190 = 70
production 191 = 70
production 192 = 71
production 193 = 71
production 194 = 71
production 195 = 65
production 196 = 65
production 197 = 72
production 198 = 72
production 199 = 72
production 200 = 72
production 201 = 31
production 202 = 31
production 203 = 31
production 204 = 31
production 205 = 73
production 206 = 73
production 207 = 8
production 208 = 8
production 209 = 8
production 210 = 8
production 211 = 8
production 212 = 9
production 213 = 9
production 214 = 74
production 215 = 74
production 216 = 74
production 217 = 74
production 218 = 52
production 219 = 52
production 220 = 44
production 221 = 44
production 222 = 16
production 223 = 16
production 224 = 15
production 225 = 15
production 226 = 47
production 227 = 47
production 228 = 47
production 229 = 75
production 230 = 1
production 231 = 42
production 232 = 42
production 233 = 62
production 234 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 196
    (9, 1) -> Just 221
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 302
    (13, 14) -> Just 18
    (13, 27) -> Just 219
    (13, 30) -> Just 255
    (13, 31) -> Just 79
    (13, 40) -> Just 272
    (13, 41) -> Just 273
    (13, 73) -> Just 276
    (16, 4) -> Just 17
    (16, 8) -> Just 302
    (16, 14) -> Just 18
    (16, 27) -> Just 219
    (16, 30) -> Just 255
    (16, 31) -> Just 79
    (16, 40) -> Just 272
    (16, 41) -> Just 273
    (16, 73) -> Just 276
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 170
    (25, 9) -> Just 171
    (25, 10) -> Just 33
    (25, 13) -> Just 160
    (34, 8) -> Just 420
    (34, 32) -> Just 376
    (34, 61) -> Just 280
    (34, 63) -> Just 390
    (34, 66) -> Just 45
    (34, 67) -> Just 404
    (35, 8) -> Just 420
    (35, 32) -> Just 377
    (35, 61) -> Just 280
    (35, 63) -> Just 390
    (35, 66) -> Just 45
    (35, 67) -> Just 404
    (36, 8) -> Just 420
    (36, 32) -> Just 378
    (36, 61) -> Just 280
    (36, 63) -> Just 390
    (36, 66) -> Just 45
    (36, 67) -> Just 404
    (37, 8) -> Just 420
    (37, 32) -> Just 381
    (37, 61) -> Just 280
    (37, 63) -> Just 390
    (37, 66) -> Just 45
    (37, 67) -> Just 404
    (38, 8) -> Just 420
    (38, 32) -> Just 382
    (38, 61) -> Just 280
    (38, 63) -> Just 390
    (38, 66) -> Just 45
    (38, 67) -> Just 404
    (39, 8) -> Just 420
    (39, 32) -> Just 383
    (39, 61) -> Just 280
    (39, 63) -> Just 390
    (39, 66) -> Just 45
    (39, 67) -> Just 404
    (40, 8) -> Just 420
    (40, 32) -> Just 384
    (40, 61) -> Just 280
    (40, 63) -> Just 390
    (40, 66) -> Just 45
    (40, 67) -> Just 404
    (41, 8) -> Just 420
    (41, 32) -> Just 385
    (41, 61) -> Just 280
    (41, 63) -> Just 390
    (41, 66) -> Just 45
    (41, 67) -> Just 404
    (42, 8) -> Just 420
    (42, 32) -> Just 386
    (42, 61) -> Just 280
    (42, 63) -> Just 390
    (42, 66) -> Just 45
    (42, 67) -> Just 404
    (43, 8) -> Just 420
    (43, 32) -> Just 387
    (43, 61) -> Just 280
    (43, 63) -> Just 390
    (43, 66) -> Just 45
    (43, 67) -> Just 404
    (44, 8) -> Just 420
    (44, 63) -> Just 397
    (44, 66) -> Just 45
    (44, 67) -> Just 404
    (45, 8) -> Just 420
    (45, 67) -> Just 405
    (46, 8) -> Just 420
    (46, 32) -> Just 256
    (46, 61) -> Just 280
    (46, 63) -> Just 390
    (46, 66) -> Just 45
    (46, 67) -> Just 404
    (47, 8) -> Just 420
    (47, 32) -> Just 281
    (47, 61) -> Just 280
    (47, 63) -> Just 390
    (47, 66) -> Just 45
    (47, 67) -> Just 404
    (48, 8) -> Just 420
    (48, 32) -> Just 291
    (48, 61) -> Just 280
    (48, 63) -> Just 390
    (48, 66) -> Just 45
    (48, 67) -> Just 404
    (49, 8) -> Just 420
    (49, 32) -> Just 299
    (49, 61) -> Just 280
    (49, 63) -> Just 390
    (49, 66) -> Just 45
    (49, 67) -> Just 404
    (50, 8) -> Just 420
    (50, 32) -> Just 429
    (50, 61) -> Just 280
    (50, 63) -> Just 390
    (50, 66) -> Just 45
    (50, 67) -> Just 404
    (51, 8) -> Just 420
    (51, 61) -> Just 430
    (51, 63) -> Just 390
    (51, 65) -> Just 399
    (51, 66) -> Just 45
    (51, 67) -> Just 404
    (51, 72) -> Just 403
    (52, 8) -> Just 420
    (52, 61) -> Just 430
    (52, 63) -> Just 390
    (52, 65) -> Just 402
    (52, 66) -> Just 45
    (52, 67) -> Just 404
    (52, 72) -> Just 403
    (53, 8) -> Just 420
    (53, 61) -> Just 431
    (53, 63) -> Just 390
    (53, 66) -> Just 45
    (53, 67) -> Just 404
    (54, 8) -> Just 420
    (54, 32) -> Just 421
    (54, 61) -> Just 280
    (54, 63) -> Just 390
    (54, 66) -> Just 45
    (54, 67) -> Just 404
    (55, 8) -> Just 420
    (55, 63) -> Just 397
    (55, 66) -> Just 45
    (55, 67) -> Just 404
    (56, 8) -> Just 420
    (56, 61) -> Just 422
    (56, 63) -> Just 390
    (56, 66) -> Just 45
    (56, 67) -> Just 404
    (57, 8) -> Just 420
    (57, 61) -> Just 423
    (57, 63) -> Just 390
    (57, 66) -> Just 45
    (57, 67) -> Just 404
    (58, 8) -> Just 420
    (58, 61) -> Just 424
    (58, 63) -> Just 390
    (58, 66) -> Just 45
    (58, 67) -> Just 404
    (59, 8) -> Just 420
    (59, 61) -> Just 425
    (59, 63) -> Just 390
    (59, 66) -> Just 45
    (59, 67) -> Just 404
    (60, 8) -> Just 420
    (60, 61) -> Just 426
    (60, 63) -> Just 390
    (60, 66) -> Just 45
    (60, 67) -> Just 404
    (61, 8) -> Just 420
    (61, 61) -> Just 427
    (61, 63) -> Just 390
    (61, 66) -> Just 45
    (61, 67) -> Just 404
    (62, 8) -> Just 420
    (62, 33) -> Just 257
    (62, 59) -> Just 283
    (62, 60) -> Just 363
    (62, 61) -> Just 366
    (62, 63) -> Just 390
    (62, 66) -> Just 45
    (62, 67) -> Just 404
    (63, 8) -> Just 420
    (63, 33) -> Just 282
    (63, 59) -> Just 283
    (63, 60) -> Just 363
    (63, 61) -> Just 366
    (63, 63) -> Just 390
    (63, 66) -> Just 45
    (63, 67) -> Just 404
    (64, 8) -> Just 420
    (64, 33) -> Just 292
    (64, 59) -> Just 283
    (64, 60) -> Just 363
    (64, 61) -> Just 366
    (64, 63) -> Just 390
    (64, 66) -> Just 45
    (64, 67) -> Just 404
    (65, 8) -> Just 420
    (65, 33) -> Just 300
    (65, 59) -> Just 283
    (65, 60) -> Just 363
    (65, 61) -> Just 366
    (65, 63) -> Just 390
    (65, 66) -> Just 45
    (65, 67) -> Just 404
    (66, 8) -> Just 420
    (66, 59) -> Just 362
    (66, 60) -> Just 363
    (66, 61) -> Just 366
    (66, 63) -> Just 390
    (66, 66) -> Just 45
    (66, 67) -> Just 404
    (67, 8) -> Just 420
    (67, 32) -> Just 379
    (67, 61) -> Just 280
    (67, 63) -> Just 390
    (67, 66) -> Just 45
    (67, 67) -> Just 404
    (68, 8) -> Just 420
    (68, 32) -> Just 380
    (68, 61) -> Just 280
    (68, 63) -> Just 390
    (68, 66) -> Just 45
    (68, 67) -> Just 404
    (69, 8) -> Just 420
    (69, 32) -> Just 396
    (69, 61) -> Just 280
    (69, 63) -> Just 390
    (69, 66) -> Just 45
    (69, 67) -> Just 404
    (70, 8) -> Just 420
    (70, 32) -> Just 365
    (70, 61) -> Just 280
    (70, 63) -> Just 390
    (70, 66) -> Just 45
    (70, 67) -> Just 404
    (71, 8) -> Just 433
    (71, 73) -> Just 277
    (72, 8) -> Just 433
    (72, 73) -> Just 279
    (73, 8) -> Just 433
    (73, 31) -> Just 74
    (73, 73) -> Just 276
    (74, 8) -> Just 433
    (74, 44) -> Just 72
    (74, 52) -> Just 314
    (74, 73) -> Just 278
    (74, 74) -> Just 315
    (75, 8) -> Just 302
    (75, 27) -> Just 268
    (75, 29) -> Just 267
    (75, 30) -> Just 255
    (75, 31) -> Just 79
    (75, 40) -> Just 272
    (75, 41) -> Just 273
    (75, 73) -> Just 276
    (76, 8) -> Just 302
    (76, 27) -> Just 268
    (76, 29) -> Just 269
    (76, 30) -> Just 255
    (76, 31) -> Just 79
    (76, 40) -> Just 272
    (76, 41) -> Just 273
    (76, 73) -> Just 276
    (77, 8) -> Just 302
    (77, 30) -> Just 290
    (77, 31) -> Just 82
    (77, 35) -> Just 285
    (77, 36) -> Just 287
    (77, 40) -> Just 272
    (77, 41) -> Just 273
    (77, 73) -> Just 276
    (78, 8) -> Just 302
    (78, 30) -> Just 290
    (78, 31) -> Just 82
    (78, 35) -> Just 286
    (78, 36) -> Just 287
    (78, 40) -> Just 272
    (78, 41) -> Just 273
    (78, 73) -> Just 276
    (79, 8) -> Just 433
    (79, 44) -> Just 72
    (79, 52) -> Just 314
    (79, 73) -> Just 278
    (79, 74) -> Just 315
    (80, 8) -> Just 433
    (80, 31) -> Just 83
    (80, 38) -> Just 294
    (80, 39) -> Just 296
    (80, 73) -> Just 276
    (81, 8) -> Just 433
    (81, 31) -> Just 83
    (81, 38) -> Just 295
    (81, 39) -> Just 296
    (81, 73) -> Just 276
    (82, 8) -> Just 433
    (82, 44) -> Just 72
    (82, 52) -> Just 314
    (82, 73) -> Just 278
    (82, 74) -> Just 315
    (83, 8) -> Just 433
    (83, 44) -> Just 72
    (83, 52) -> Just 314
    (83, 73) -> Just 278
    (83, 74) -> Just 315
    (84, 8) -> Just 433
    (84, 31) -> Just 87
    (84, 73) -> Just 276
    (85, 8) -> Just 433
    (85, 31) -> Just 88
    (85, 64) -> Just 398
    (85, 68) -> Just 401
    (85, 73) -> Just 276
    (86, 8) -> Just 433
    (86, 31) -> Just 88
    (86, 64) -> Just 400
    (86, 68) -> Just 401
    (86, 73) -> Just 276
    (87, 8) -> Just 433
    (87, 44) -> Just 72
    (87, 52) -> Just 314
    (87, 73) -> Just 278
    (87, 74) -> Just 315
    (88, 8) -> Just 433
    (88, 44) -> Just 72
    (88, 52) -> Just 314
    (88, 73) -> Just 278
    (88, 74) -> Just 315
    (89, 8) -> Just 167
    (89, 9) -> Just 168
    (89, 11) -> Just 161
    (89, 12) -> Just 162
    (90, 8) -> Just 167
    (90, 9) -> Just 168
    (90, 11) -> Just 197
    (90, 12) -> Just 162
    (91, 8) -> Just 167
    (91, 9) -> Just 168
    (91, 11) -> Just 198
    (91, 12) -> Just 162
    (92, 8) -> Just 170
    (92, 9) -> Just 171
    (92, 10) -> Just 159
    (92, 13) -> Just 160
    (93, 8) -> Just 170
    (93, 9) -> Just 171
    (93, 10) -> Just 169
    (93, 13) -> Just 160
    (94, 8) -> Just 301
    (94, 40) -> Just 303
    (95, 8) -> Just 301
    (95, 40) -> Just 353
    (95, 53) -> Just 344
    (95, 54) -> Just 351
    (96, 8) -> Just 301
    (96, 40) -> Just 353
    (96, 53) -> Just 350
    (96, 54) -> Just 351
    (97, 8) -> Just 231
    (98, 8) -> Just 242
    (99, 8) -> Just 243
    (100, 8) -> Just 244
    (110, 9) -> Just 330
    (110, 45) -> Just 321
    (110, 46) -> Just 322
    (110, 47) -> Just 323
    (111, 9) -> Just 330
    (111, 17) -> Just 112
    (111, 45) -> Just 222
    (111, 46) -> Just 322
    (111, 47) -> Just 323
    (112, 9) -> Just 330
    (112, 23) -> Just 214
    (112, 45) -> Just 223
    (112, 46) -> Just 322
    (112, 47) -> Just 323
    (113, 9) -> Just 330
    (113, 17) -> Just 114
    (113, 45) -> Just 222
    (113, 46) -> Just 322
    (113, 47) -> Just 323
    (114, 9) -> Just 330
    (114, 24) -> Just 216
    (114, 45) -> Just 223
    (114, 46) -> Just 322
    (114, 47) -> Just 323
    (115, 9) -> Just 330
    (115, 17) -> Just 116
    (115, 45) -> Just 222
    (115, 46) -> Just 322
    (115, 47) -> Just 323
    (116, 9) -> Just 330
    (116, 23) -> Just 213
    (116, 45) -> Just 223
    (116, 46) -> Just 322
    (116, 47) -> Just 323
    (117, 9) -> Just 330
    (117, 17) -> Just 118
    (117, 45) -> Just 222
    (117, 46) -> Just 322
    (117, 47) -> Just 323
    (118, 9) -> Just 330
    (118, 24) -> Just 215
    (118, 45) -> Just 223
    (118, 46) -> Just 322
    (118, 47) -> Just 323
    (119, 9) -> Just 330
    (119, 17) -> Just 120
    (119, 18) -> Just 373
    (119, 45) -> Just 222
    (119, 46) -> Just 322
    (119, 47) -> Just 323
    (120, 9) -> Just 330
    (120, 45) -> Just 223
    (120, 46) -> Just 322
    (120, 47) -> Just 323
    (121, 9) -> Just 330
    (121, 17) -> Just 123
    (121, 18) -> Just 224
    (121, 45) -> Just 222
    (121, 46) -> Just 322
    (121, 47) -> Just 323
    (122, 9) -> Just 330
    (122, 17) -> Just 123
    (122, 18) -> Just 372
    (122, 45) -> Just 222
    (122, 46) -> Just 322
    (122, 47) -> Just 323
    (123, 9) -> Just 330
    (123, 45) -> Just 223
    (123, 46) -> Just 322
    (123, 47) -> Just 323
    (124, 9) -> Just 330
    (124, 17) -> Just 125
    (124, 45) -> Just 222
    (124, 46) -> Just 322
    (124, 47) -> Just 323
    (125, 9) -> Just 330
    (125, 19) -> Just 201
    (125, 45) -> Just 223
    (125, 46) -> Just 322
    (125, 47) -> Just 323
    (126, 9) -> Just 330
    (126, 17) -> Just 127
    (126, 45) -> Just 222
    (126, 46) -> Just 322
    (126, 47) -> Just 323
    (127, 9) -> Just 330
    (127, 19) -> Just 202
    (127, 45) -> Just 223
    (127, 46) -> Just 322
    (127, 47) -> Just 323
    (128, 9) -> Just 331
    (128, 17) -> Just 131
    (128, 45) -> Just 222
    (128, 46) -> Just 322
    (128, 47) -> Just 323
    (128, 50) -> Just 225
    (128, 51) -> Just 341
    (129, 9) -> Just 331
    (129, 17) -> Just 131
    (129, 45) -> Just 222
    (129, 46) -> Just 322
    (129, 47) -> Just 323
    (129, 50) -> Just 340
    (129, 51) -> Just 341
    (130, 9) -> Just 143
    (131, 9) -> Just 330
    (131, 45) -> Just 223
    (131, 46) -> Just 322
    (131, 47) -> Just 323
    (131, 52) -> Just 132
    (132, 9) -> Just 330
    (132, 17) -> Just 133
    (132, 45) -> Just 222
    (132, 46) -> Just 322
    (132, 47) -> Just 323
    (133, 9) -> Just 330
    (133, 45) -> Just 223
    (133, 46) -> Just 322
    (133, 47) -> Just 323
    (134, 9) -> Just 330
    (134, 17) -> Just 135
    (134, 18) -> Just 271
    (134, 45) -> Just 222
    (134, 46) -> Just 322
    (134, 47) -> Just 323
    (135, 9) -> Just 330
    (135, 45) -> Just 223
    (135, 46) -> Just 322
    (135, 47) -> Just 323
    (136, 9) -> Just 330
    (136, 17) -> Just 123
    (136, 18) -> Just 200
    (136, 45) -> Just 222
    (136, 46) -> Just 322
    (136, 47) -> Just 323
    (137, 9) -> Just 330
    (137, 17) -> Just 123
    (137, 18) -> Just 245
    (137, 45) -> Just 222
    (137, 46) -> Just 322
    (137, 47) -> Just 323
    (138, 9) -> Just 330
    (138, 17) -> Just 123
    (138, 18) -> Just 246
    (138, 45) -> Just 222
    (138, 46) -> Just 322
    (138, 47) -> Just 323
    (139, 9) -> Just 330
    (139, 17) -> Just 123
    (139, 18) -> Just 247
    (139, 45) -> Just 222
    (139, 46) -> Just 322
    (139, 47) -> Just 323
    (140, 9) -> Just 330
    (140, 17) -> Just 123
    (140, 18) -> Just 270
    (140, 45) -> Just 222
    (140, 46) -> Just 322
    (140, 47) -> Just 323
    (141, 9) -> Just 330
    (141, 17) -> Just 123
    (141, 18) -> Just 352
    (141, 45) -> Just 222
    (141, 46) -> Just 322
    (141, 47) -> Just 323
    (142, 9) -> Just 330
    (142, 17) -> Just 123
    (142, 18) -> Just 232
    (142, 45) -> Just 222
    (142, 46) -> Just 322
    (142, 47) -> Just 323
    (143, 9) -> Just 330
    (143, 45) -> Just 233
    (143, 46) -> Just 322
    (143, 47) -> Just 323
    (144, 9) -> Just 330
    (144, 17) -> Just 145
    (144, 45) -> Just 222
    (144, 46) -> Just 322
    (144, 47) -> Just 323
    (145, 9) -> Just 330
    (145, 22) -> Just 212
    (145, 45) -> Just 223
    (145, 46) -> Just 322
    (145, 47) -> Just 323
    (146, 9) -> Just 330
    (146, 17) -> Just 148
    (146, 45) -> Just 222
    (146, 46) -> Just 322
    (146, 47) -> Just 323
    (147, 9) -> Just 330
    (147, 17) -> Just 149
    (147, 45) -> Just 222
    (147, 46) -> Just 322
    (147, 47) -> Just 323
    (148, 9) -> Just 330
    (148, 45) -> Just 223
    (148, 46) -> Just 322
    (148, 47) -> Just 323
    (149, 9) -> Just 330
    (149, 22) -> Just 211
    (149, 45) -> Just 223
    (149, 46) -> Just 322
    (149, 47) -> Just 323
    (150, 9) -> Just 330
    (150, 17) -> Just 123
    (150, 18) -> Just 319
    (150, 45) -> Just 222
    (150, 46) -> Just 322
    (150, 47) -> Just 323
    (150, 48) -> Just 324
    (150, 49) -> Just 332
    (151, 9) -> Just 330
    (151, 17) -> Just 123
    (151, 18) -> Just 238
    (151, 25) -> Just 217
    (151, 45) -> Just 222
    (151, 46) -> Just 322
    (151, 47) -> Just 323
    (152, 9) -> Just 330
    (152, 17) -> Just 123
    (152, 18) -> Just 238
    (152, 25) -> Just 239
    (152, 45) -> Just 222
    (152, 46) -> Just 322
    (152, 47) -> Just 323
    (153, 9) -> Just 330
    (153, 17) -> Just 123
    (153, 18) -> Just 336
    (153, 45) -> Just 222
    (153, 46) -> Just 322
    (153, 47) -> Just 323
    (153, 48) -> Just 337
    (154, 9) -> Just 330
    (154, 17) -> Just 123
    (154, 18) -> Just 320
    (154, 45) -> Just 222
    (154, 46) -> Just 322
    (154, 47) -> Just 323
    (172, 20) -> Just 228
    (172, 21) -> Just 207
    (173, 20) -> Just 228
    (173, 21) -> Just 208
    (174, 20) -> Just 228
    (174, 21) -> Just 209
    (175, 20) -> Just 228
    (175, 21) -> Just 210
    (188, 15) -> Just 8
    (190, 20) -> Just 203
    (191, 20) -> Just 204
    (192, 20) -> Just 205
    (193, 20) -> Just 206
    (195, 26) -> Just 218
    (196, 16) -> Just 199
    (226, 20) -> Just 228
    (226, 21) -> Just 229
    (234, 34) -> Just 235
    (236, 37) -> Just 237
    (240, 55) -> Just 248
    (241, 55) -> Just 249
    (248, 56) -> Just 98
    (248, 57) -> Just 250
    (249, 58) -> Just 100
    (250, 56) -> Just 99
    (251, 28) -> Just 253
    (252, 28) -> Just 254
    (258, 28) -> Just 375
    (259, 28) -> Just 288
    (260, 28) -> Just 289
    (261, 28) -> Just 297
    (262, 28) -> Just 298
    (263, 28) -> Just 428
    (264, 28) -> Just 364
    (265, 28) -> Just 374
    (273, 42) -> Just 274
    (274, 43) -> Just 275
    (274, 44) -> Just 313
    (274, 52) -> Just 314
    (274, 74) -> Just 315
    (308, 43) -> Just 311
    (308, 44) -> Just 313
    (308, 52) -> Just 314
    (308, 74) -> Just 315
    (309, 43) -> Just 312
    (309, 44) -> Just 313
    (309, 52) -> Just 314
    (309, 74) -> Just 315
    (338, 49) -> Just 339
    (379, 62) -> Just 388
    (380, 62) -> Just 389
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
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt actions (case snd (pop !! 0) of { StackValue_alt value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_alts $ alts_implies_alt_SEMICOLON_alts actions (case snd (pop !! 2) of { StackValue_alt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_alts value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_alt $ alt_implies actions
                    186 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp actions (case snd (pop !! 2) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp_PIPE_gdpat actions (case snd (pop !! 4) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdpat value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard actions (case snd (pop !! 0) of { StackValue_patguard value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard_COMMA_patguards actions (case snd (pop !! 2) of { StackValue_patguard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_patguards value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt actions (case snd (pop !! 0) of { StackValue_stmt value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_stmts $ stmts_implies_stmt_SEMICOLON_stmts actions (case snd (pop !! 2) of { StackValue_stmt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_stmts value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_stmt $ stmt_implies actions
                    198 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_stmt $ stmt_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    201 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    203 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    204 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    205 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    206 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    207 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    208 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    209 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    210 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    211 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    212 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    213 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    214 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    215 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    216 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    217 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    218 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    219 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    220 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    221 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    222 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    223 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    224 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    225 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    226 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    227 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    228 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    229 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    230 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    231 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    232 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    233 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    234 ->
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
  , aexp_implies_var = \var0 ->
      return $ Aexp_implies_var var0
  , aexp_implies_INTEGER = \iNTEGER0 ->
      return $ Aexp_implies_INTEGER iNTEGER0
  , aexp_implies_STRING = \sTRING0 ->
      return $ Aexp_implies_STRING sTRING0
  , aexp_implies_LPAREN_exp_RPAREN = \lPAREN0 exp1 rPAREN2 ->
      return $ Aexp_implies_LPAREN_exp_RPAREN lPAREN0 exp1 rPAREN2
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

