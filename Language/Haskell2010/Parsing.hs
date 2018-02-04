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
    Stmts_implies
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
    Alt_implies_pat_RARROW_exp Pat RARROW Exp
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
  , alt_implies_pat_RARROW_exp :: Pat -> RARROW -> Exp -> m Alt
  , alt_implies_pat_RARROW_exp_WHERE_decls :: Pat -> RARROW -> Exp -> WHERE -> Decls -> m Alt
  , gdpat_implies_patguards_RARROW_exp :: Patguards -> RARROW -> Exp -> m Gdpat
  , gdpat_implies_patguards_RARROW_exp_PIPE_gdpat :: Patguards -> RARROW -> Exp -> PIPE -> Gdpat -> m Gdpat
  , patguards_implies_patguard :: Patguard -> m Patguards
  , patguards_implies_patguard_COMMA_patguards :: Patguard -> COMMA -> Patguards -> m Patguards
  , patguard_implies_infixexp_LARROW_infixexp :: Infixexp -> LARROW -> Infixexp -> m Patguard
  , patguard_implies_LET_decls :: LET -> Decls -> m Patguard
  , patguard_implies_infixexp :: Infixexp -> m Patguard
  , stmts_implies :: m Stmts
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
    (11, Token (MODULE _)) -> Just (Reduce 1 224)
    (11, Token (WHERE _)) -> Just (Reduce 1 224)
    (11, Token (RBRACE _)) -> Just (Reduce 1 224)
    (11, Token (LPAREN _)) -> Just (Reduce 1 224)
    (11, Token (RPAREN _)) -> Just (Reduce 1 224)
    (11, Token (COMMA _)) -> Just (Reduce 1 224)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 224)
    (11, Token (HIDING _)) -> Just (Reduce 1 224)
    (11, Token (MINUS _)) -> Just (Reduce 1 224)
    (11, Token (QCONID _)) -> Just (Reduce 1 224)
    (11, Token (EXPORT _)) -> Just (Reduce 1 224)
    (11, Token (AS _)) -> Just (Reduce 1 224)
    (11, Token (QVARID _)) -> Just (Reduce 1 224)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 224)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 224)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 70)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 185)
    (13, Token (TYPE _)) -> Just (Shift 143)
    (13, Token (DATA _)) -> Just (Shift 121)
    (13, Token (NEWTYPE _)) -> Just (Shift 141)
    (13, Token (CLASS _)) -> Just (Shift 108)
    (13, Token (INSTANCE _)) -> Just (Shift 110)
    (13, Token (DEFAULT _)) -> Just (Shift 191)
    (13, Token (FOREIGN _)) -> Just (Shift 192)
    (13, Token (INFIXL _)) -> Just (Shift 300)
    (13, Token (INFIXR _)) -> Just (Shift 301)
    (13, Token (INFIX _)) -> Just (Shift 302)
    (13, Token (EXPORT _)) -> Just (Shift 103)
    (13, Token (AS _)) -> Just (Shift 104)
    (13, Token (QVARID _)) -> Just (Shift 105)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 70)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 185)
    (16, Token (TYPE _)) -> Just (Shift 143)
    (16, Token (DATA _)) -> Just (Shift 121)
    (16, Token (NEWTYPE _)) -> Just (Shift 141)
    (16, Token (CLASS _)) -> Just (Shift 108)
    (16, Token (INSTANCE _)) -> Just (Shift 110)
    (16, Token (DEFAULT _)) -> Just (Shift 191)
    (16, Token (FOREIGN _)) -> Just (Shift 192)
    (16, Token (INFIXL _)) -> Just (Shift 300)
    (16, Token (INFIXR _)) -> Just (Shift 301)
    (16, Token (INFIX _)) -> Just (Shift 302)
    (16, Token (EXPORT _)) -> Just (Shift 103)
    (16, Token (AS _)) -> Just (Shift 104)
    (16, Token (QVARID _)) -> Just (Shift 105)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 98)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 154)
    (19, Token (EXPORT _)) -> Just (Shift 103)
    (19, Token (AS _)) -> Just (Shift 104)
    (19, Token (QVARID _)) -> Just (Shift 105)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 98)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 154)
    (22, Token (EXPORT _)) -> Just (Shift 103)
    (22, Token (AS _)) -> Just (Shift 104)
    (22, Token (QVARID _)) -> Just (Shift 105)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 98)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 154)
    (25, Token (EXPORT _)) -> Just (Shift 103)
    (25, Token (AS _)) -> Just (Shift 104)
    (25, Token (QVARID _)) -> Just (Shift 105)
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
    (34, Token (LPAREN _)) -> Just (Shift 51)
    (34, Token (MINUS _)) -> Just (Shift 44)
    (34, Token (EXPORT _)) -> Just (Shift 103)
    (34, Token (AS _)) -> Just (Shift 104)
    (34, Token (QVARID _)) -> Just (Shift 105)
    (34, Token (STRING _)) -> Just (Shift 411)
    (34, Token (LET _)) -> Just (Shift 261)
    (34, Token (LAMBDA _)) -> Just (Shift 81)
    (34, Token (IF _)) -> Just (Shift 64)
    (34, Token (CASE _)) -> Just (Shift 66)
    (34, Token (DO _)) -> Just (Shift 391)
    (34, Token (INTEGER _)) -> Just (Shift 413)
    (35, Token (LPAREN _)) -> Just (Shift 51)
    (35, Token (MINUS _)) -> Just (Shift 44)
    (35, Token (EXPORT _)) -> Just (Shift 103)
    (35, Token (AS _)) -> Just (Shift 104)
    (35, Token (QVARID _)) -> Just (Shift 105)
    (35, Token (STRING _)) -> Just (Shift 411)
    (35, Token (LET _)) -> Just (Shift 261)
    (35, Token (LAMBDA _)) -> Just (Shift 81)
    (35, Token (IF _)) -> Just (Shift 64)
    (35, Token (CASE _)) -> Just (Shift 66)
    (35, Token (DO _)) -> Just (Shift 391)
    (35, Token (INTEGER _)) -> Just (Shift 413)
    (36, Token (LPAREN _)) -> Just (Shift 51)
    (36, Token (MINUS _)) -> Just (Shift 44)
    (36, Token (EXPORT _)) -> Just (Shift 103)
    (36, Token (AS _)) -> Just (Shift 104)
    (36, Token (QVARID _)) -> Just (Shift 105)
    (36, Token (STRING _)) -> Just (Shift 411)
    (36, Token (LET _)) -> Just (Shift 261)
    (36, Token (LAMBDA _)) -> Just (Shift 81)
    (36, Token (IF _)) -> Just (Shift 64)
    (36, Token (CASE _)) -> Just (Shift 66)
    (36, Token (DO _)) -> Just (Shift 391)
    (36, Token (INTEGER _)) -> Just (Shift 413)
    (37, Token (LPAREN _)) -> Just (Shift 51)
    (37, Token (MINUS _)) -> Just (Shift 44)
    (37, Token (EXPORT _)) -> Just (Shift 103)
    (37, Token (AS _)) -> Just (Shift 104)
    (37, Token (QVARID _)) -> Just (Shift 105)
    (37, Token (STRING _)) -> Just (Shift 411)
    (37, Token (LET _)) -> Just (Shift 261)
    (37, Token (LAMBDA _)) -> Just (Shift 81)
    (37, Token (IF _)) -> Just (Shift 64)
    (37, Token (CASE _)) -> Just (Shift 66)
    (37, Token (DO _)) -> Just (Shift 391)
    (37, Token (INTEGER _)) -> Just (Shift 413)
    (38, Token (LPAREN _)) -> Just (Shift 51)
    (38, Token (MINUS _)) -> Just (Shift 44)
    (38, Token (EXPORT _)) -> Just (Shift 103)
    (38, Token (AS _)) -> Just (Shift 104)
    (38, Token (QVARID _)) -> Just (Shift 105)
    (38, Token (STRING _)) -> Just (Shift 411)
    (38, Token (LET _)) -> Just (Shift 261)
    (38, Token (LAMBDA _)) -> Just (Shift 81)
    (38, Token (IF _)) -> Just (Shift 64)
    (38, Token (CASE _)) -> Just (Shift 66)
    (38, Token (DO _)) -> Just (Shift 391)
    (38, Token (INTEGER _)) -> Just (Shift 413)
    (39, Token (LPAREN _)) -> Just (Shift 51)
    (39, Token (MINUS _)) -> Just (Shift 44)
    (39, Token (EXPORT _)) -> Just (Shift 103)
    (39, Token (AS _)) -> Just (Shift 104)
    (39, Token (QVARID _)) -> Just (Shift 105)
    (39, Token (STRING _)) -> Just (Shift 411)
    (39, Token (LET _)) -> Just (Shift 261)
    (39, Token (LAMBDA _)) -> Just (Shift 81)
    (39, Token (IF _)) -> Just (Shift 64)
    (39, Token (CASE _)) -> Just (Shift 66)
    (39, Token (DO _)) -> Just (Shift 391)
    (39, Token (INTEGER _)) -> Just (Shift 413)
    (40, Token (LPAREN _)) -> Just (Shift 51)
    (40, Token (MINUS _)) -> Just (Shift 44)
    (40, Token (EXPORT _)) -> Just (Shift 103)
    (40, Token (AS _)) -> Just (Shift 104)
    (40, Token (QVARID _)) -> Just (Shift 105)
    (40, Token (STRING _)) -> Just (Shift 411)
    (40, Token (LET _)) -> Just (Shift 261)
    (40, Token (LAMBDA _)) -> Just (Shift 81)
    (40, Token (IF _)) -> Just (Shift 64)
    (40, Token (CASE _)) -> Just (Shift 66)
    (40, Token (DO _)) -> Just (Shift 391)
    (40, Token (INTEGER _)) -> Just (Shift 413)
    (41, Token (LPAREN _)) -> Just (Shift 51)
    (41, Token (MINUS _)) -> Just (Shift 44)
    (41, Token (EXPORT _)) -> Just (Shift 103)
    (41, Token (AS _)) -> Just (Shift 104)
    (41, Token (QVARID _)) -> Just (Shift 105)
    (41, Token (STRING _)) -> Just (Shift 411)
    (41, Token (LET _)) -> Just (Shift 261)
    (41, Token (LAMBDA _)) -> Just (Shift 81)
    (41, Token (IF _)) -> Just (Shift 64)
    (41, Token (CASE _)) -> Just (Shift 66)
    (41, Token (DO _)) -> Just (Shift 391)
    (41, Token (INTEGER _)) -> Just (Shift 413)
    (42, Token (LPAREN _)) -> Just (Shift 51)
    (42, Token (MINUS _)) -> Just (Shift 44)
    (42, Token (EXPORT _)) -> Just (Shift 103)
    (42, Token (AS _)) -> Just (Shift 104)
    (42, Token (QVARID _)) -> Just (Shift 105)
    (42, Token (STRING _)) -> Just (Shift 411)
    (42, Token (LET _)) -> Just (Shift 261)
    (42, Token (LAMBDA _)) -> Just (Shift 81)
    (42, Token (IF _)) -> Just (Shift 64)
    (42, Token (CASE _)) -> Just (Shift 66)
    (42, Token (DO _)) -> Just (Shift 391)
    (42, Token (INTEGER _)) -> Just (Shift 413)
    (43, Token (LPAREN _)) -> Just (Shift 51)
    (43, Token (MINUS _)) -> Just (Shift 44)
    (43, Token (EXPORT _)) -> Just (Shift 103)
    (43, Token (AS _)) -> Just (Shift 104)
    (43, Token (QVARID _)) -> Just (Shift 105)
    (43, Token (STRING _)) -> Just (Shift 411)
    (43, Token (LET _)) -> Just (Shift 261)
    (43, Token (LAMBDA _)) -> Just (Shift 81)
    (43, Token (IF _)) -> Just (Shift 64)
    (43, Token (CASE _)) -> Just (Shift 66)
    (43, Token (DO _)) -> Just (Shift 391)
    (43, Token (INTEGER _)) -> Just (Shift 413)
    (44, Token (LPAREN _)) -> Just (Shift 51)
    (44, Token (MINUS _)) -> Just (Shift 44)
    (44, Token (EXPORT _)) -> Just (Shift 103)
    (44, Token (AS _)) -> Just (Shift 104)
    (44, Token (QVARID _)) -> Just (Shift 105)
    (44, Token (STRING _)) -> Just (Shift 411)
    (44, Token (CASE _)) -> Just (Shift 66)
    (44, Token (DO _)) -> Just (Shift 391)
    (44, Token (INTEGER _)) -> Just (Shift 413)
    (45, Token (WHERE _)) -> Just (Reduce 1 170)
    (45, Token (RBRACE _)) -> Just (Reduce 1 170)
    (45, Token (LPAREN _)) -> Just (Shift 51)
    (45, Token (RPAREN _)) -> Just (Reduce 1 170)
    (45, Token (COMMA _)) -> Just (Reduce 1 170)
    (45, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (45, Token (EQUAL _)) -> Just (Reduce 1 170)
    (45, Token (PIPE _)) -> Just (Reduce 1 170)
    (45, Token (COLON_COLON _)) -> Just (Reduce 1 170)
    (45, Token (MINUS _)) -> Just (Reduce 1 170)
    (45, Token (EXPORT _)) -> Just (Shift 103)
    (45, Token (AS _)) -> Just (Shift 104)
    (45, Token (QVARID _)) -> Just (Shift 105)
    (45, Token (STRING _)) -> Just (Shift 411)
    (45, Token (LARROW _)) -> Just (Reduce 1 170)
    (45, Token (THEN _)) -> Just (Reduce 1 170)
    (45, Token (ELSE _)) -> Just (Reduce 1 170)
    (45, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (45, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (45, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (45, Token (OF _)) -> Just (Reduce 1 170)
    (45, Token (INTEGER _)) -> Just (Shift 413)
    (46, Token (LPAREN _)) -> Just (Shift 51)
    (46, Token (MINUS _)) -> Just (Shift 44)
    (46, Token (EXPORT _)) -> Just (Shift 103)
    (46, Token (AS _)) -> Just (Shift 104)
    (46, Token (QVARID _)) -> Just (Shift 105)
    (46, Token (STRING _)) -> Just (Shift 411)
    (46, Token (LET _)) -> Just (Shift 261)
    (46, Token (LAMBDA _)) -> Just (Shift 81)
    (46, Token (IF _)) -> Just (Shift 64)
    (46, Token (CASE _)) -> Just (Shift 66)
    (46, Token (DO _)) -> Just (Shift 391)
    (46, Token (INTEGER _)) -> Just (Shift 413)
    (47, Token (LPAREN _)) -> Just (Shift 51)
    (47, Token (MINUS _)) -> Just (Shift 44)
    (47, Token (EXPORT _)) -> Just (Shift 103)
    (47, Token (AS _)) -> Just (Shift 104)
    (47, Token (QVARID _)) -> Just (Shift 105)
    (47, Token (STRING _)) -> Just (Shift 411)
    (47, Token (LET _)) -> Just (Shift 261)
    (47, Token (LAMBDA _)) -> Just (Shift 81)
    (47, Token (IF _)) -> Just (Shift 64)
    (47, Token (CASE _)) -> Just (Shift 66)
    (47, Token (DO _)) -> Just (Shift 391)
    (47, Token (INTEGER _)) -> Just (Shift 413)
    (48, Token (LPAREN _)) -> Just (Shift 51)
    (48, Token (MINUS _)) -> Just (Shift 44)
    (48, Token (EXPORT _)) -> Just (Shift 103)
    (48, Token (AS _)) -> Just (Shift 104)
    (48, Token (QVARID _)) -> Just (Shift 105)
    (48, Token (STRING _)) -> Just (Shift 411)
    (48, Token (LET _)) -> Just (Shift 261)
    (48, Token (LAMBDA _)) -> Just (Shift 81)
    (48, Token (IF _)) -> Just (Shift 64)
    (48, Token (CASE _)) -> Just (Shift 66)
    (48, Token (DO _)) -> Just (Shift 391)
    (48, Token (INTEGER _)) -> Just (Shift 413)
    (49, Token (LPAREN _)) -> Just (Shift 51)
    (49, Token (MINUS _)) -> Just (Shift 44)
    (49, Token (EXPORT _)) -> Just (Shift 103)
    (49, Token (AS _)) -> Just (Shift 104)
    (49, Token (QVARID _)) -> Just (Shift 105)
    (49, Token (STRING _)) -> Just (Shift 411)
    (49, Token (LET _)) -> Just (Shift 261)
    (49, Token (LAMBDA _)) -> Just (Shift 81)
    (49, Token (IF _)) -> Just (Shift 64)
    (49, Token (CASE _)) -> Just (Shift 66)
    (49, Token (DO _)) -> Just (Shift 391)
    (49, Token (INTEGER _)) -> Just (Shift 413)
    (50, Token (LPAREN _)) -> Just (Shift 51)
    (50, Token (MINUS _)) -> Just (Shift 44)
    (50, Token (EXPORT _)) -> Just (Shift 103)
    (50, Token (AS _)) -> Just (Shift 104)
    (50, Token (QVARID _)) -> Just (Shift 105)
    (50, Token (STRING _)) -> Just (Shift 411)
    (50, Token (LET _)) -> Just (Shift 261)
    (50, Token (LAMBDA _)) -> Just (Shift 81)
    (50, Token (IF _)) -> Just (Shift 64)
    (50, Token (CASE _)) -> Just (Shift 66)
    (50, Token (DO _)) -> Just (Shift 391)
    (50, Token (INTEGER _)) -> Just (Shift 413)
    (51, Token (LPAREN _)) -> Just (Shift 51)
    (51, Token (MINUS _)) -> Just (Shift 52)
    (51, Token (EXPORT _)) -> Just (Shift 103)
    (51, Token (AS _)) -> Just (Shift 104)
    (51, Token (QVARID _)) -> Just (Shift 105)
    (51, Token (STRING _)) -> Just (Shift 411)
    (51, Token (LET _)) -> Just (Shift 261)
    (51, Token (LAMBDA _)) -> Just (Shift 81)
    (51, Token (IF _)) -> Just (Shift 64)
    (51, Token (QVARSYM _)) -> Just (Shift 53)
    (51, Token (BACKQUOTE _)) -> Just (Shift 412)
    (51, Token (QCONSYM _)) -> Just (Shift 58)
    (51, Token (CASE _)) -> Just (Shift 66)
    (51, Token (DO _)) -> Just (Shift 391)
    (51, Token (INTEGER _)) -> Just (Shift 413)
    (52, Token (LPAREN _)) -> Just (Shift 51)
    (52, Token (RPAREN _)) -> Just (Shift 100)
    (52, Token (MINUS _)) -> Just (Shift 44)
    (52, Token (EXPORT _)) -> Just (Shift 103)
    (52, Token (AS _)) -> Just (Shift 104)
    (52, Token (QVARID _)) -> Just (Shift 105)
    (52, Token (STRING _)) -> Just (Shift 411)
    (52, Token (CASE _)) -> Just (Shift 66)
    (52, Token (DO _)) -> Just (Shift 391)
    (52, Token (INTEGER _)) -> Just (Shift 413)
    (53, Token (LPAREN _)) -> Just (Shift 51)
    (53, Token (RPAREN _)) -> Just (Shift 101)
    (53, Token (MINUS _)) -> Just (Shift 44)
    (53, Token (EXPORT _)) -> Just (Shift 103)
    (53, Token (AS _)) -> Just (Shift 104)
    (53, Token (QVARID _)) -> Just (Shift 105)
    (53, Token (STRING _)) -> Just (Shift 411)
    (53, Token (LET _)) -> Just (Shift 261)
    (53, Token (LAMBDA _)) -> Just (Shift 81)
    (53, Token (IF _)) -> Just (Shift 64)
    (53, Token (CASE _)) -> Just (Shift 66)
    (53, Token (DO _)) -> Just (Shift 391)
    (53, Token (INTEGER _)) -> Just (Shift 413)
    (54, Token (LPAREN _)) -> Just (Shift 51)
    (54, Token (MINUS _)) -> Just (Shift 44)
    (54, Token (EXPORT _)) -> Just (Shift 103)
    (54, Token (AS _)) -> Just (Shift 104)
    (54, Token (QVARID _)) -> Just (Shift 105)
    (54, Token (STRING _)) -> Just (Shift 411)
    (54, Token (LET _)) -> Just (Shift 261)
    (54, Token (LAMBDA _)) -> Just (Shift 81)
    (54, Token (IF _)) -> Just (Shift 64)
    (54, Token (CASE _)) -> Just (Shift 66)
    (54, Token (DO _)) -> Just (Shift 391)
    (54, Token (INTEGER _)) -> Just (Shift 413)
    (55, Token (LPAREN _)) -> Just (Shift 51)
    (55, Token (MINUS _)) -> Just (Shift 44)
    (55, Token (EXPORT _)) -> Just (Shift 103)
    (55, Token (AS _)) -> Just (Shift 104)
    (55, Token (QVARID _)) -> Just (Shift 105)
    (55, Token (STRING _)) -> Just (Shift 411)
    (55, Token (LET _)) -> Just (Shift 261)
    (55, Token (LAMBDA _)) -> Just (Shift 81)
    (55, Token (IF _)) -> Just (Shift 64)
    (55, Token (CASE _)) -> Just (Shift 66)
    (55, Token (DO _)) -> Just (Shift 391)
    (55, Token (INTEGER _)) -> Just (Shift 413)
    (56, Token (LPAREN _)) -> Just (Shift 51)
    (56, Token (MINUS _)) -> Just (Shift 44)
    (56, Token (EXPORT _)) -> Just (Shift 103)
    (56, Token (AS _)) -> Just (Shift 104)
    (56, Token (QVARID _)) -> Just (Shift 105)
    (56, Token (STRING _)) -> Just (Shift 411)
    (56, Token (LET _)) -> Just (Shift 261)
    (56, Token (LAMBDA _)) -> Just (Shift 81)
    (56, Token (IF _)) -> Just (Shift 64)
    (56, Token (CASE _)) -> Just (Shift 66)
    (56, Token (DO _)) -> Just (Shift 391)
    (56, Token (INTEGER _)) -> Just (Shift 413)
    (57, Token (LPAREN _)) -> Just (Shift 51)
    (57, Token (MINUS _)) -> Just (Shift 44)
    (57, Token (EXPORT _)) -> Just (Shift 103)
    (57, Token (AS _)) -> Just (Shift 104)
    (57, Token (QVARID _)) -> Just (Shift 105)
    (57, Token (STRING _)) -> Just (Shift 411)
    (57, Token (LET _)) -> Just (Shift 261)
    (57, Token (LAMBDA _)) -> Just (Shift 81)
    (57, Token (IF _)) -> Just (Shift 64)
    (57, Token (CASE _)) -> Just (Shift 66)
    (57, Token (DO _)) -> Just (Shift 391)
    (57, Token (INTEGER _)) -> Just (Shift 413)
    (58, Token (LPAREN _)) -> Just (Shift 51)
    (58, Token (MINUS _)) -> Just (Shift 44)
    (58, Token (EXPORT _)) -> Just (Shift 103)
    (58, Token (AS _)) -> Just (Shift 104)
    (58, Token (QVARID _)) -> Just (Shift 105)
    (58, Token (STRING _)) -> Just (Shift 411)
    (58, Token (LET _)) -> Just (Shift 261)
    (58, Token (LAMBDA _)) -> Just (Shift 81)
    (58, Token (IF _)) -> Just (Shift 64)
    (58, Token (CASE _)) -> Just (Shift 66)
    (58, Token (DO _)) -> Just (Shift 391)
    (58, Token (INTEGER _)) -> Just (Shift 413)
    (59, Token (LPAREN _)) -> Just (Shift 51)
    (59, Token (MINUS _)) -> Just (Shift 44)
    (59, Token (EXPORT _)) -> Just (Shift 103)
    (59, Token (AS _)) -> Just (Shift 104)
    (59, Token (QVARID _)) -> Just (Shift 105)
    (59, Token (STRING _)) -> Just (Shift 411)
    (59, Token (LET _)) -> Just (Shift 260)
    (59, Token (LAMBDA _)) -> Just (Shift 81)
    (59, Token (IF _)) -> Just (Shift 64)
    (59, Token (CASE _)) -> Just (Shift 66)
    (59, Token (DO _)) -> Just (Shift 391)
    (59, Token (INTEGER _)) -> Just (Shift 413)
    (60, Token (LPAREN _)) -> Just (Shift 51)
    (60, Token (MINUS _)) -> Just (Shift 44)
    (60, Token (EXPORT _)) -> Just (Shift 103)
    (60, Token (AS _)) -> Just (Shift 104)
    (60, Token (QVARID _)) -> Just (Shift 105)
    (60, Token (STRING _)) -> Just (Shift 411)
    (60, Token (LET _)) -> Just (Shift 260)
    (60, Token (LAMBDA _)) -> Just (Shift 81)
    (60, Token (IF _)) -> Just (Shift 64)
    (60, Token (CASE _)) -> Just (Shift 66)
    (60, Token (DO _)) -> Just (Shift 391)
    (60, Token (INTEGER _)) -> Just (Shift 413)
    (61, Token (LPAREN _)) -> Just (Shift 51)
    (61, Token (MINUS _)) -> Just (Shift 44)
    (61, Token (EXPORT _)) -> Just (Shift 103)
    (61, Token (AS _)) -> Just (Shift 104)
    (61, Token (QVARID _)) -> Just (Shift 105)
    (61, Token (STRING _)) -> Just (Shift 411)
    (61, Token (LET _)) -> Just (Shift 260)
    (61, Token (LAMBDA _)) -> Just (Shift 81)
    (61, Token (IF _)) -> Just (Shift 64)
    (61, Token (CASE _)) -> Just (Shift 66)
    (61, Token (DO _)) -> Just (Shift 391)
    (61, Token (INTEGER _)) -> Just (Shift 413)
    (62, Token (LPAREN _)) -> Just (Shift 51)
    (62, Token (MINUS _)) -> Just (Shift 44)
    (62, Token (EXPORT _)) -> Just (Shift 103)
    (62, Token (AS _)) -> Just (Shift 104)
    (62, Token (QVARID _)) -> Just (Shift 105)
    (62, Token (STRING _)) -> Just (Shift 411)
    (62, Token (LET _)) -> Just (Shift 260)
    (62, Token (LAMBDA _)) -> Just (Shift 81)
    (62, Token (IF _)) -> Just (Shift 64)
    (62, Token (CASE _)) -> Just (Shift 66)
    (62, Token (DO _)) -> Just (Shift 391)
    (62, Token (INTEGER _)) -> Just (Shift 413)
    (63, Token (LPAREN _)) -> Just (Shift 51)
    (63, Token (MINUS _)) -> Just (Shift 44)
    (63, Token (EXPORT _)) -> Just (Shift 103)
    (63, Token (AS _)) -> Just (Shift 104)
    (63, Token (QVARID _)) -> Just (Shift 105)
    (63, Token (STRING _)) -> Just (Shift 411)
    (63, Token (LET _)) -> Just (Shift 260)
    (63, Token (LAMBDA _)) -> Just (Shift 81)
    (63, Token (IF _)) -> Just (Shift 64)
    (63, Token (CASE _)) -> Just (Shift 66)
    (63, Token (DO _)) -> Just (Shift 391)
    (63, Token (INTEGER _)) -> Just (Shift 413)
    (64, Token (LPAREN _)) -> Just (Shift 51)
    (64, Token (MINUS _)) -> Just (Shift 44)
    (64, Token (EXPORT _)) -> Just (Shift 103)
    (64, Token (AS _)) -> Just (Shift 104)
    (64, Token (QVARID _)) -> Just (Shift 105)
    (64, Token (STRING _)) -> Just (Shift 411)
    (64, Token (LET _)) -> Just (Shift 261)
    (64, Token (LAMBDA _)) -> Just (Shift 81)
    (64, Token (IF _)) -> Just (Shift 64)
    (64, Token (CASE _)) -> Just (Shift 66)
    (64, Token (DO _)) -> Just (Shift 391)
    (64, Token (INTEGER _)) -> Just (Shift 413)
    (65, Token (LPAREN _)) -> Just (Shift 51)
    (65, Token (MINUS _)) -> Just (Shift 44)
    (65, Token (EXPORT _)) -> Just (Shift 103)
    (65, Token (AS _)) -> Just (Shift 104)
    (65, Token (QVARID _)) -> Just (Shift 105)
    (65, Token (STRING _)) -> Just (Shift 411)
    (65, Token (LET _)) -> Just (Shift 261)
    (65, Token (LAMBDA _)) -> Just (Shift 81)
    (65, Token (IF _)) -> Just (Shift 64)
    (65, Token (CASE _)) -> Just (Shift 66)
    (65, Token (DO _)) -> Just (Shift 391)
    (65, Token (INTEGER _)) -> Just (Shift 413)
    (66, Token (LPAREN _)) -> Just (Shift 51)
    (66, Token (MINUS _)) -> Just (Shift 44)
    (66, Token (EXPORT _)) -> Just (Shift 103)
    (66, Token (AS _)) -> Just (Shift 104)
    (66, Token (QVARID _)) -> Just (Shift 105)
    (66, Token (STRING _)) -> Just (Shift 411)
    (66, Token (LET _)) -> Just (Shift 261)
    (66, Token (LAMBDA _)) -> Just (Shift 81)
    (66, Token (IF _)) -> Just (Shift 64)
    (66, Token (CASE _)) -> Just (Shift 66)
    (66, Token (DO _)) -> Just (Shift 391)
    (66, Token (INTEGER _)) -> Just (Shift 413)
    (67, Token (LPAREN _)) -> Just (Shift 51)
    (67, Token (MINUS _)) -> Just (Shift 44)
    (67, Token (EXPORT _)) -> Just (Shift 103)
    (67, Token (AS _)) -> Just (Shift 104)
    (67, Token (QVARID _)) -> Just (Shift 105)
    (67, Token (STRING _)) -> Just (Shift 411)
    (67, Token (LET _)) -> Just (Shift 261)
    (67, Token (LAMBDA _)) -> Just (Shift 81)
    (67, Token (IF _)) -> Just (Shift 64)
    (67, Token (CASE _)) -> Just (Shift 66)
    (67, Token (DO _)) -> Just (Shift 391)
    (67, Token (INTEGER _)) -> Just (Shift 413)
    (68, Token (LPAREN _)) -> Just (Shift 70)
    (68, Token (EXPORT _)) -> Just (Shift 103)
    (68, Token (AS _)) -> Just (Shift 104)
    (68, Token (QVARID _)) -> Just (Shift 105)
    (69, Token (LPAREN _)) -> Just (Shift 70)
    (69, Token (EXPORT _)) -> Just (Shift 103)
    (69, Token (AS _)) -> Just (Shift 104)
    (69, Token (QVARID _)) -> Just (Shift 105)
    (70, Token (LPAREN _)) -> Just (Shift 70)
    (70, Token (MINUS _)) -> Just (Shift 102)
    (70, Token (EXPORT _)) -> Just (Shift 103)
    (70, Token (AS _)) -> Just (Shift 104)
    (70, Token (QVARID _)) -> Just (Shift 105)
    (70, Token (QVARSYM _)) -> Just (Shift 106)
    (71, Token (LPAREN _)) -> Just (Shift 70)
    (71, Token (RPAREN _)) -> Just (Shift 424)
    (71, Token (MINUS _)) -> Just (Shift 68)
    (71, Token (EXPORT _)) -> Just (Shift 103)
    (71, Token (AS _)) -> Just (Shift 104)
    (71, Token (QVARID _)) -> Just (Shift 105)
    (71, Token (QVARSYM _)) -> Just (Shift 429)
    (71, Token (BACKQUOTE _)) -> Just (Shift 342)
    (71, Token (QCONSYM _)) -> Just (Shift 345)
    (72, Token (RBRACE _)) -> Just (Reduce 0 85)
    (72, Token (LPAREN _)) -> Just (Shift 70)
    (72, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (72, Token (INFIXL _)) -> Just (Shift 300)
    (72, Token (INFIXR _)) -> Just (Shift 301)
    (72, Token (INFIX _)) -> Just (Shift 302)
    (72, Token (EXPORT _)) -> Just (Shift 103)
    (72, Token (AS _)) -> Just (Shift 104)
    (72, Token (QVARID _)) -> Just (Shift 105)
    (73, Token (RBRACE _)) -> Just (Reduce 0 85)
    (73, Token (LPAREN _)) -> Just (Shift 70)
    (73, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (73, Token (INFIXL _)) -> Just (Shift 300)
    (73, Token (INFIXR _)) -> Just (Shift 301)
    (73, Token (INFIX _)) -> Just (Shift 302)
    (73, Token (EXPORT _)) -> Just (Shift 103)
    (73, Token (AS _)) -> Just (Shift 104)
    (73, Token (QVARID _)) -> Just (Shift 105)
    (74, Token (RBRACE _)) -> Just (Reduce 0 85)
    (74, Token (LPAREN _)) -> Just (Shift 70)
    (74, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (74, Token (INFIXL _)) -> Just (Shift 300)
    (74, Token (INFIXR _)) -> Just (Shift 301)
    (74, Token (INFIX _)) -> Just (Shift 302)
    (74, Token (EXPORT _)) -> Just (Shift 103)
    (74, Token (AS _)) -> Just (Shift 104)
    (74, Token (QVARID _)) -> Just (Shift 105)
    (75, Token (RBRACE _)) -> Just (Reduce 0 85)
    (75, Token (LPAREN _)) -> Just (Shift 70)
    (75, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (75, Token (INFIXL _)) -> Just (Shift 300)
    (75, Token (INFIXR _)) -> Just (Shift 301)
    (75, Token (INFIX _)) -> Just (Shift 302)
    (75, Token (EXPORT _)) -> Just (Shift 103)
    (75, Token (AS _)) -> Just (Shift 104)
    (75, Token (QVARID _)) -> Just (Shift 105)
    (76, Token (LPAREN _)) -> Just (Shift 70)
    (76, Token (EQUAL _)) -> Just (Shift 46)
    (76, Token (PIPE _)) -> Just (Shift 59)
    (76, Token (MINUS _)) -> Just (Shift 68)
    (76, Token (EXPORT _)) -> Just (Shift 103)
    (76, Token (AS _)) -> Just (Shift 104)
    (76, Token (QVARID _)) -> Just (Shift 105)
    (76, Token (QVARSYM _)) -> Just (Shift 429)
    (76, Token (BACKQUOTE _)) -> Just (Shift 342)
    (76, Token (QCONSYM _)) -> Just (Shift 345)
    (77, Token (RBRACE _)) -> Just (Reduce 0 80)
    (77, Token (LPAREN _)) -> Just (Shift 70)
    (77, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (77, Token (EXPORT _)) -> Just (Shift 103)
    (77, Token (AS _)) -> Just (Shift 104)
    (77, Token (QVARID _)) -> Just (Shift 105)
    (78, Token (RBRACE _)) -> Just (Reduce 0 80)
    (78, Token (LPAREN _)) -> Just (Shift 70)
    (78, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (78, Token (EXPORT _)) -> Just (Shift 103)
    (78, Token (AS _)) -> Just (Shift 104)
    (78, Token (QVARID _)) -> Just (Shift 105)
    (79, Token (LPAREN _)) -> Just (Shift 70)
    (79, Token (EQUAL _)) -> Just (Shift 48)
    (79, Token (PIPE _)) -> Just (Shift 61)
    (79, Token (MINUS _)) -> Just (Shift 68)
    (79, Token (EXPORT _)) -> Just (Shift 103)
    (79, Token (AS _)) -> Just (Shift 104)
    (79, Token (QVARID _)) -> Just (Shift 105)
    (79, Token (QVARSYM _)) -> Just (Shift 429)
    (79, Token (BACKQUOTE _)) -> Just (Shift 342)
    (79, Token (QCONSYM _)) -> Just (Shift 345)
    (80, Token (LPAREN _)) -> Just (Shift 70)
    (80, Token (EQUAL _)) -> Just (Shift 49)
    (80, Token (PIPE _)) -> Just (Shift 62)
    (80, Token (MINUS _)) -> Just (Shift 68)
    (80, Token (EXPORT _)) -> Just (Shift 103)
    (80, Token (AS _)) -> Just (Shift 104)
    (80, Token (QVARID _)) -> Just (Shift 105)
    (80, Token (QVARSYM _)) -> Just (Shift 429)
    (80, Token (BACKQUOTE _)) -> Just (Shift 342)
    (80, Token (QCONSYM _)) -> Just (Shift 345)
    (81, Token (LPAREN _)) -> Just (Shift 70)
    (81, Token (EXPORT _)) -> Just (Shift 103)
    (81, Token (AS _)) -> Just (Shift 104)
    (81, Token (QVARID _)) -> Just (Shift 105)
    (82, Token (LPAREN _)) -> Just (Shift 70)
    (82, Token (EXPORT _)) -> Just (Shift 103)
    (82, Token (AS _)) -> Just (Shift 104)
    (82, Token (QVARID _)) -> Just (Shift 105)
    (83, Token (LPAREN _)) -> Just (Shift 70)
    (83, Token (EXPORT _)) -> Just (Shift 103)
    (83, Token (AS _)) -> Just (Shift 104)
    (83, Token (QVARID _)) -> Just (Shift 105)
    (84, Token (LPAREN _)) -> Just (Shift 70)
    (84, Token (MINUS _)) -> Just (Shift 68)
    (84, Token (RARROW _)) -> Just (Shift 35)
    (84, Token (EXPORT _)) -> Just (Shift 103)
    (84, Token (AS _)) -> Just (Shift 104)
    (84, Token (QVARID _)) -> Just (Shift 105)
    (84, Token (QVARSYM _)) -> Just (Shift 429)
    (84, Token (BACKQUOTE _)) -> Just (Shift 342)
    (84, Token (QCONSYM _)) -> Just (Shift 345)
    (85, Token (LPAREN _)) -> Just (Shift 70)
    (85, Token (MINUS _)) -> Just (Shift 68)
    (85, Token (RARROW _)) -> Just (Shift 50)
    (85, Token (EXPORT _)) -> Just (Shift 103)
    (85, Token (AS _)) -> Just (Shift 104)
    (85, Token (QVARID _)) -> Just (Shift 105)
    (85, Token (QVARSYM _)) -> Just (Shift 429)
    (85, Token (BACKQUOTE _)) -> Just (Shift 342)
    (85, Token (QCONSYM _)) -> Just (Shift 345)
    (86, Token (LPAREN _)) -> Just (Shift 98)
    (86, Token (RPAREN _)) -> Just (Reduce 0 15)
    (86, Token (QCONID _)) -> Just (Shift 154)
    (86, Token (EXPORT _)) -> Just (Shift 103)
    (86, Token (AS _)) -> Just (Shift 104)
    (86, Token (QVARID _)) -> Just (Shift 105)
    (87, Token (LPAREN _)) -> Just (Shift 98)
    (87, Token (RPAREN _)) -> Just (Reduce 0 15)
    (87, Token (QCONID _)) -> Just (Shift 154)
    (87, Token (EXPORT _)) -> Just (Shift 103)
    (87, Token (AS _)) -> Just (Shift 104)
    (87, Token (QVARID _)) -> Just (Shift 105)
    (88, Token (LPAREN _)) -> Just (Shift 98)
    (88, Token (RPAREN _)) -> Just (Reduce 0 15)
    (88, Token (QCONID _)) -> Just (Shift 154)
    (88, Token (EXPORT _)) -> Just (Shift 103)
    (88, Token (AS _)) -> Just (Shift 104)
    (88, Token (QVARID _)) -> Just (Shift 105)
    (89, Token (LPAREN _)) -> Just (Shift 98)
    (89, Token (QCONID _)) -> Just (Shift 154)
    (89, Token (EXPORT _)) -> Just (Shift 103)
    (89, Token (AS _)) -> Just (Shift 104)
    (89, Token (QVARID _)) -> Just (Shift 105)
    (90, Token (LPAREN _)) -> Just (Shift 98)
    (90, Token (RPAREN _)) -> Just (Shift 160)
    (90, Token (DOT_DOT _)) -> Just (Shift 163)
    (90, Token (QCONID _)) -> Just (Shift 154)
    (90, Token (EXPORT _)) -> Just (Shift 103)
    (90, Token (AS _)) -> Just (Shift 104)
    (90, Token (QVARID _)) -> Just (Shift 105)
    (91, Token (LPAREN _)) -> Just (Shift 99)
    (91, Token (EXPORT _)) -> Just (Shift 103)
    (91, Token (AS _)) -> Just (Shift 104)
    (91, Token (QVARID _)) -> Just (Shift 105)
    (92, Token (RBRACE _)) -> Just (Shift 338)
    (92, Token (LPAREN _)) -> Just (Shift 99)
    (92, Token (EXPORT _)) -> Just (Shift 103)
    (92, Token (AS _)) -> Just (Shift 104)
    (92, Token (QVARID _)) -> Just (Shift 105)
    (93, Token (LPAREN _)) -> Just (Shift 99)
    (93, Token (EXPORT _)) -> Just (Shift 103)
    (93, Token (AS _)) -> Just (Shift 104)
    (93, Token (QVARID _)) -> Just (Shift 105)
    (94, Token (LPAREN _)) -> Just (Shift 99)
    (94, Token (EXPORT _)) -> Just (Shift 103)
    (94, Token (AS _)) -> Just (Shift 104)
    (94, Token (QVARID _)) -> Just (Shift 105)
    (95, Token (LPAREN _)) -> Just (Shift 99)
    (95, Token (EXPORT _)) -> Just (Shift 103)
    (95, Token (AS _)) -> Just (Shift 104)
    (95, Token (QVARID _)) -> Just (Shift 105)
    (96, Token (LPAREN _)) -> Just (Shift 99)
    (96, Token (EXPORT _)) -> Just (Shift 103)
    (96, Token (AS _)) -> Just (Shift 104)
    (96, Token (QVARID _)) -> Just (Shift 105)
    (97, Token (LPAREN _)) -> Just (Shift 99)
    (97, Token (EXPORT _)) -> Just (Shift 103)
    (97, Token (AS _)) -> Just (Shift 104)
    (97, Token (QVARID _)) -> Just (Shift 105)
    (98, Token (MINUS _)) -> Just (Shift 102)
    (98, Token (QVARSYM _)) -> Just (Shift 106)
    (98, Token (QCONSYM _)) -> Just (Shift 155)
    (99, Token (MINUS _)) -> Just (Shift 102)
    (99, Token (QVARSYM _)) -> Just (Shift 106)
    (100, Token (WHERE _)) -> Just (Reduce 3 204)
    (100, Token (LBRACE _)) -> Just (Reduce 3 204)
    (100, Token (RBRACE _)) -> Just (Reduce 3 204)
    (100, Token (LPAREN _)) -> Just (Reduce 3 204)
    (100, Token (RPAREN _)) -> Just (Reduce 3 204)
    (100, Token (COMMA _)) -> Just (Reduce 3 204)
    (100, Token (SEMICOLON _)) -> Just (Reduce 3 204)
    (100, Token (EQUAL _)) -> Just (Reduce 3 204)
    (100, Token (PIPE _)) -> Just (Reduce 3 204)
    (100, Token (COLON_COLON _)) -> Just (Reduce 3 204)
    (100, Token (MINUS _)) -> Just (Reduce 3 204)
    (100, Token (INFIXL _)) -> Just (Reduce 3 204)
    (100, Token (INFIXR _)) -> Just (Reduce 3 204)
    (100, Token (INFIX _)) -> Just (Reduce 3 204)
    (100, Token (RARROW _)) -> Just (Reduce 3 204)
    (100, Token (QCONID _)) -> Just (Reduce 3 204)
    (100, Token (EXPORT _)) -> Just (Reduce 3 204)
    (100, Token (AS _)) -> Just (Reduce 3 204)
    (100, Token (QVARID _)) -> Just (Reduce 3 204)
    (100, Token (STRING _)) -> Just (Reduce 3 204)
    (100, Token (LARROW _)) -> Just (Reduce 3 204)
    (100, Token (LET _)) -> Just (Reduce 3 204)
    (100, Token (LAMBDA _)) -> Just (Reduce 3 204)
    (100, Token (IF _)) -> Just (Reduce 3 204)
    (100, Token (THEN _)) -> Just (Reduce 3 204)
    (100, Token (ELSE _)) -> Just (Reduce 3 204)
    (100, Token (QVARSYM _)) -> Just (Reduce 3 204)
    (100, Token (BACKQUOTE _)) -> Just (Reduce 3 204)
    (100, Token (QCONSYM _)) -> Just (Reduce 3 204)
    (100, Token (CASE _)) -> Just (Reduce 3 204)
    (100, Token (OF _)) -> Just (Reduce 3 204)
    (100, Token (DO _)) -> Just (Reduce 3 204)
    (100, Token (INTEGER _)) -> Just (Reduce 3 204)
    (101, Token (WHERE _)) -> Just (Reduce 3 205)
    (101, Token (LBRACE _)) -> Just (Reduce 3 205)
    (101, Token (RBRACE _)) -> Just (Reduce 3 205)
    (101, Token (LPAREN _)) -> Just (Reduce 3 205)
    (101, Token (RPAREN _)) -> Just (Reduce 3 205)
    (101, Token (COMMA _)) -> Just (Reduce 3 205)
    (101, Token (SEMICOLON _)) -> Just (Reduce 3 205)
    (101, Token (EQUAL _)) -> Just (Reduce 3 205)
    (101, Token (PIPE _)) -> Just (Reduce 3 205)
    (101, Token (COLON_COLON _)) -> Just (Reduce 3 205)
    (101, Token (MINUS _)) -> Just (Reduce 3 205)
    (101, Token (INFIXL _)) -> Just (Reduce 3 205)
    (101, Token (INFIXR _)) -> Just (Reduce 3 205)
    (101, Token (INFIX _)) -> Just (Reduce 3 205)
    (101, Token (RARROW _)) -> Just (Reduce 3 205)
    (101, Token (QCONID _)) -> Just (Reduce 3 205)
    (101, Token (EXPORT _)) -> Just (Reduce 3 205)
    (101, Token (AS _)) -> Just (Reduce 3 205)
    (101, Token (QVARID _)) -> Just (Reduce 3 205)
    (101, Token (STRING _)) -> Just (Reduce 3 205)
    (101, Token (LARROW _)) -> Just (Reduce 3 205)
    (101, Token (LET _)) -> Just (Reduce 3 205)
    (101, Token (LAMBDA _)) -> Just (Reduce 3 205)
    (101, Token (IF _)) -> Just (Reduce 3 205)
    (101, Token (THEN _)) -> Just (Reduce 3 205)
    (101, Token (ELSE _)) -> Just (Reduce 3 205)
    (101, Token (QVARSYM _)) -> Just (Reduce 3 205)
    (101, Token (BACKQUOTE _)) -> Just (Reduce 3 205)
    (101, Token (QCONSYM _)) -> Just (Reduce 3 205)
    (101, Token (CASE _)) -> Just (Reduce 3 205)
    (101, Token (OF _)) -> Just (Reduce 3 205)
    (101, Token (DO _)) -> Just (Reduce 3 205)
    (101, Token (INTEGER _)) -> Just (Reduce 3 205)
    (102, Token (RPAREN _)) -> Just (Shift 100)
    (103, Token (WHERE _)) -> Just (Reduce 1 202)
    (103, Token (LBRACE _)) -> Just (Reduce 1 202)
    (103, Token (RBRACE _)) -> Just (Reduce 1 202)
    (103, Token (LPAREN _)) -> Just (Reduce 1 202)
    (103, Token (RPAREN _)) -> Just (Reduce 1 202)
    (103, Token (COMMA _)) -> Just (Reduce 1 202)
    (103, Token (SEMICOLON _)) -> Just (Reduce 1 202)
    (103, Token (EQUAL _)) -> Just (Reduce 1 202)
    (103, Token (PIPE _)) -> Just (Reduce 1 202)
    (103, Token (COLON_COLON _)) -> Just (Reduce 1 202)
    (103, Token (MINUS _)) -> Just (Reduce 1 202)
    (103, Token (INFIXL _)) -> Just (Reduce 1 202)
    (103, Token (INFIXR _)) -> Just (Reduce 1 202)
    (103, Token (INFIX _)) -> Just (Reduce 1 202)
    (103, Token (RARROW _)) -> Just (Reduce 1 202)
    (103, Token (QCONID _)) -> Just (Reduce 1 202)
    (103, Token (EXPORT _)) -> Just (Reduce 1 202)
    (103, Token (AS _)) -> Just (Reduce 1 202)
    (103, Token (QVARID _)) -> Just (Reduce 1 202)
    (103, Token (STRING _)) -> Just (Reduce 1 202)
    (103, Token (LARROW _)) -> Just (Reduce 1 202)
    (103, Token (LET _)) -> Just (Reduce 1 202)
    (103, Token (LAMBDA _)) -> Just (Reduce 1 202)
    (103, Token (IF _)) -> Just (Reduce 1 202)
    (103, Token (THEN _)) -> Just (Reduce 1 202)
    (103, Token (ELSE _)) -> Just (Reduce 1 202)
    (103, Token (QVARSYM _)) -> Just (Reduce 1 202)
    (103, Token (BACKQUOTE _)) -> Just (Reduce 1 202)
    (103, Token (QCONSYM _)) -> Just (Reduce 1 202)
    (103, Token (CASE _)) -> Just (Reduce 1 202)
    (103, Token (OF _)) -> Just (Reduce 1 202)
    (103, Token (DO _)) -> Just (Reduce 1 202)
    (103, Token (INTEGER _)) -> Just (Reduce 1 202)
    (104, Token (WHERE _)) -> Just (Reduce 1 201)
    (104, Token (LBRACE _)) -> Just (Reduce 1 201)
    (104, Token (RBRACE _)) -> Just (Reduce 1 201)
    (104, Token (LPAREN _)) -> Just (Reduce 1 201)
    (104, Token (RPAREN _)) -> Just (Reduce 1 201)
    (104, Token (COMMA _)) -> Just (Reduce 1 201)
    (104, Token (SEMICOLON _)) -> Just (Reduce 1 201)
    (104, Token (EQUAL _)) -> Just (Reduce 1 201)
    (104, Token (PIPE _)) -> Just (Reduce 1 201)
    (104, Token (COLON_COLON _)) -> Just (Reduce 1 201)
    (104, Token (MINUS _)) -> Just (Reduce 1 201)
    (104, Token (INFIXL _)) -> Just (Reduce 1 201)
    (104, Token (INFIXR _)) -> Just (Reduce 1 201)
    (104, Token (INFIX _)) -> Just (Reduce 1 201)
    (104, Token (RARROW _)) -> Just (Reduce 1 201)
    (104, Token (QCONID _)) -> Just (Reduce 1 201)
    (104, Token (EXPORT _)) -> Just (Reduce 1 201)
    (104, Token (AS _)) -> Just (Reduce 1 201)
    (104, Token (QVARID _)) -> Just (Reduce 1 201)
    (104, Token (STRING _)) -> Just (Reduce 1 201)
    (104, Token (LARROW _)) -> Just (Reduce 1 201)
    (104, Token (LET _)) -> Just (Reduce 1 201)
    (104, Token (LAMBDA _)) -> Just (Reduce 1 201)
    (104, Token (IF _)) -> Just (Reduce 1 201)
    (104, Token (THEN _)) -> Just (Reduce 1 201)
    (104, Token (ELSE _)) -> Just (Reduce 1 201)
    (104, Token (QVARSYM _)) -> Just (Reduce 1 201)
    (104, Token (BACKQUOTE _)) -> Just (Reduce 1 201)
    (104, Token (QCONSYM _)) -> Just (Reduce 1 201)
    (104, Token (CASE _)) -> Just (Reduce 1 201)
    (104, Token (OF _)) -> Just (Reduce 1 201)
    (104, Token (DO _)) -> Just (Reduce 1 201)
    (104, Token (INTEGER _)) -> Just (Reduce 1 201)
    (105, Token (WHERE _)) -> Just (Reduce 1 203)
    (105, Token (LBRACE _)) -> Just (Reduce 1 203)
    (105, Token (RBRACE _)) -> Just (Reduce 1 203)
    (105, Token (LPAREN _)) -> Just (Reduce 1 203)
    (105, Token (RPAREN _)) -> Just (Reduce 1 203)
    (105, Token (COMMA _)) -> Just (Reduce 1 203)
    (105, Token (SEMICOLON _)) -> Just (Reduce 1 203)
    (105, Token (EQUAL _)) -> Just (Reduce 1 203)
    (105, Token (PIPE _)) -> Just (Reduce 1 203)
    (105, Token (COLON_COLON _)) -> Just (Reduce 1 203)
    (105, Token (MINUS _)) -> Just (Reduce 1 203)
    (105, Token (INFIXL _)) -> Just (Reduce 1 203)
    (105, Token (INFIXR _)) -> Just (Reduce 1 203)
    (105, Token (INFIX _)) -> Just (Reduce 1 203)
    (105, Token (RARROW _)) -> Just (Reduce 1 203)
    (105, Token (QCONID _)) -> Just (Reduce 1 203)
    (105, Token (EXPORT _)) -> Just (Reduce 1 203)
    (105, Token (AS _)) -> Just (Reduce 1 203)
    (105, Token (QVARID _)) -> Just (Reduce 1 203)
    (105, Token (STRING _)) -> Just (Reduce 1 203)
    (105, Token (LARROW _)) -> Just (Reduce 1 203)
    (105, Token (LET _)) -> Just (Reduce 1 203)
    (105, Token (LAMBDA _)) -> Just (Reduce 1 203)
    (105, Token (IF _)) -> Just (Reduce 1 203)
    (105, Token (THEN _)) -> Just (Reduce 1 203)
    (105, Token (ELSE _)) -> Just (Reduce 1 203)
    (105, Token (QVARSYM _)) -> Just (Reduce 1 203)
    (105, Token (BACKQUOTE _)) -> Just (Reduce 1 203)
    (105, Token (QCONSYM _)) -> Just (Reduce 1 203)
    (105, Token (CASE _)) -> Just (Reduce 1 203)
    (105, Token (OF _)) -> Just (Reduce 1 203)
    (105, Token (DO _)) -> Just (Reduce 1 203)
    (105, Token (INTEGER _)) -> Just (Reduce 1 203)
    (106, Token (RPAREN _)) -> Just (Shift 101)
    (107, Token (LPAREN _)) -> Just (Shift 147)
    (107, Token (LBRACKET _)) -> Just (Shift 151)
    (107, Token (EXCL _)) -> Just (Shift 107)
    (107, Token (QCONID _)) -> Just (Shift 154)
    (107, Token (EXPORT _)) -> Just (Shift 329)
    (107, Token (AS _)) -> Just (Shift 330)
    (107, Token (QVARID _)) -> Just (Shift 331)
    (108, Token (LPAREN _)) -> Just (Shift 147)
    (108, Token (LBRACKET _)) -> Just (Shift 151)
    (108, Token (EXCL _)) -> Just (Shift 107)
    (108, Token (QCONID _)) -> Just (Shift 154)
    (108, Token (EXPORT _)) -> Just (Shift 329)
    (108, Token (AS _)) -> Just (Shift 330)
    (108, Token (QVARID _)) -> Just (Shift 331)
    (109, Token (WHERE _)) -> Just (Shift 231)
    (109, Token (RBRACE _)) -> Just (Reduce 0 65)
    (109, Token (LPAREN _)) -> Just (Shift 147)
    (109, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (109, Token (DARROW _)) -> Just (Shift 112)
    (109, Token (LBRACKET _)) -> Just (Shift 151)
    (109, Token (EXCL _)) -> Just (Shift 107)
    (109, Token (QCONID _)) -> Just (Shift 154)
    (109, Token (EXPORT _)) -> Just (Shift 329)
    (109, Token (AS _)) -> Just (Shift 330)
    (109, Token (QVARID _)) -> Just (Shift 331)
    (110, Token (LPAREN _)) -> Just (Shift 147)
    (110, Token (LBRACKET _)) -> Just (Shift 151)
    (110, Token (EXCL _)) -> Just (Shift 107)
    (110, Token (QCONID _)) -> Just (Shift 154)
    (110, Token (EXPORT _)) -> Just (Shift 329)
    (110, Token (AS _)) -> Just (Shift 330)
    (110, Token (QVARID _)) -> Just (Shift 331)
    (111, Token (WHERE _)) -> Just (Shift 233)
    (111, Token (RBRACE _)) -> Just (Reduce 0 75)
    (111, Token (LPAREN _)) -> Just (Shift 147)
    (111, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (111, Token (DARROW _)) -> Just (Shift 114)
    (111, Token (LBRACKET _)) -> Just (Shift 151)
    (111, Token (EXCL _)) -> Just (Shift 107)
    (111, Token (QCONID _)) -> Just (Shift 154)
    (111, Token (EXPORT _)) -> Just (Shift 329)
    (111, Token (AS _)) -> Just (Shift 330)
    (111, Token (QVARID _)) -> Just (Shift 331)
    (112, Token (LPAREN _)) -> Just (Shift 147)
    (112, Token (LBRACKET _)) -> Just (Shift 151)
    (112, Token (EXCL _)) -> Just (Shift 107)
    (112, Token (QCONID _)) -> Just (Shift 154)
    (112, Token (EXPORT _)) -> Just (Shift 329)
    (112, Token (AS _)) -> Just (Shift 330)
    (112, Token (QVARID _)) -> Just (Shift 331)
    (113, Token (WHERE _)) -> Just (Shift 231)
    (113, Token (RBRACE _)) -> Just (Reduce 0 65)
    (113, Token (LPAREN _)) -> Just (Shift 147)
    (113, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (113, Token (LBRACKET _)) -> Just (Shift 151)
    (113, Token (EXCL _)) -> Just (Shift 107)
    (113, Token (QCONID _)) -> Just (Shift 154)
    (113, Token (EXPORT _)) -> Just (Shift 329)
    (113, Token (AS _)) -> Just (Shift 330)
    (113, Token (QVARID _)) -> Just (Shift 331)
    (114, Token (LPAREN _)) -> Just (Shift 147)
    (114, Token (LBRACKET _)) -> Just (Shift 151)
    (114, Token (EXCL _)) -> Just (Shift 107)
    (114, Token (QCONID _)) -> Just (Shift 154)
    (114, Token (EXPORT _)) -> Just (Shift 329)
    (114, Token (AS _)) -> Just (Shift 330)
    (114, Token (QVARID _)) -> Just (Shift 331)
    (115, Token (WHERE _)) -> Just (Shift 233)
    (115, Token (RBRACE _)) -> Just (Reduce 0 75)
    (115, Token (LPAREN _)) -> Just (Shift 147)
    (115, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (115, Token (LBRACKET _)) -> Just (Shift 151)
    (115, Token (EXCL _)) -> Just (Shift 107)
    (115, Token (QCONID _)) -> Just (Shift 154)
    (115, Token (EXPORT _)) -> Just (Shift 329)
    (115, Token (AS _)) -> Just (Shift 330)
    (115, Token (QVARID _)) -> Just (Shift 331)
    (116, Token (LPAREN _)) -> Just (Shift 147)
    (116, Token (LBRACKET _)) -> Just (Shift 151)
    (116, Token (EXCL _)) -> Just (Shift 107)
    (116, Token (QCONID _)) -> Just (Shift 154)
    (116, Token (EXPORT _)) -> Just (Shift 329)
    (116, Token (AS _)) -> Just (Shift 330)
    (116, Token (QVARID _)) -> Just (Shift 331)
    (117, Token (WHERE _)) -> Just (Reduce 1 100)
    (117, Token (RBRACE _)) -> Just (Reduce 1 100)
    (117, Token (LPAREN _)) -> Just (Shift 147)
    (117, Token (RPAREN _)) -> Just (Reduce 1 100)
    (117, Token (COMMA _)) -> Just (Reduce 1 100)
    (117, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (117, Token (EQUAL _)) -> Just (Reduce 1 100)
    (117, Token (DARROW _)) -> Just (Shift 119)
    (117, Token (PIPE _)) -> Just (Reduce 1 100)
    (117, Token (RARROW _)) -> Just (Shift 118)
    (117, Token (LBRACKET _)) -> Just (Shift 151)
    (117, Token (EXCL _)) -> Just (Shift 107)
    (117, Token (QCONID _)) -> Just (Shift 154)
    (117, Token (EXPORT _)) -> Just (Shift 329)
    (117, Token (AS _)) -> Just (Shift 330)
    (117, Token (QVARID _)) -> Just (Shift 331)
    (117, Token (LARROW _)) -> Just (Reduce 1 100)
    (117, Token (THEN _)) -> Just (Reduce 1 100)
    (117, Token (ELSE _)) -> Just (Reduce 1 100)
    (117, Token (OF _)) -> Just (Reduce 1 100)
    (118, Token (LPAREN _)) -> Just (Shift 147)
    (118, Token (LBRACKET _)) -> Just (Shift 151)
    (118, Token (EXCL _)) -> Just (Shift 107)
    (118, Token (QCONID _)) -> Just (Shift 154)
    (118, Token (EXPORT _)) -> Just (Shift 329)
    (118, Token (AS _)) -> Just (Shift 330)
    (118, Token (QVARID _)) -> Just (Shift 331)
    (119, Token (LPAREN _)) -> Just (Shift 147)
    (119, Token (LBRACKET _)) -> Just (Shift 151)
    (119, Token (EXCL _)) -> Just (Shift 107)
    (119, Token (QCONID _)) -> Just (Shift 154)
    (119, Token (EXPORT _)) -> Just (Shift 329)
    (119, Token (AS _)) -> Just (Shift 330)
    (119, Token (QVARID _)) -> Just (Shift 331)
    (120, Token (WHERE _)) -> Just (Reduce 1 100)
    (120, Token (RBRACE _)) -> Just (Reduce 1 100)
    (120, Token (LPAREN _)) -> Just (Shift 147)
    (120, Token (RPAREN _)) -> Just (Reduce 1 100)
    (120, Token (COMMA _)) -> Just (Reduce 1 100)
    (120, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (120, Token (EQUAL _)) -> Just (Reduce 1 100)
    (120, Token (PIPE _)) -> Just (Reduce 1 100)
    (120, Token (RARROW _)) -> Just (Shift 118)
    (120, Token (LBRACKET _)) -> Just (Shift 151)
    (120, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (120, Token (EXCL _)) -> Just (Shift 107)
    (120, Token (QCONID _)) -> Just (Shift 154)
    (120, Token (EXPORT _)) -> Just (Shift 329)
    (120, Token (AS _)) -> Just (Shift 330)
    (120, Token (QVARID _)) -> Just (Shift 331)
    (120, Token (LARROW _)) -> Just (Reduce 1 100)
    (120, Token (THEN _)) -> Just (Reduce 1 100)
    (120, Token (ELSE _)) -> Just (Reduce 1 100)
    (120, Token (OF _)) -> Just (Reduce 1 100)
    (121, Token (LPAREN _)) -> Just (Shift 147)
    (121, Token (LBRACKET _)) -> Just (Shift 151)
    (121, Token (EXCL _)) -> Just (Shift 107)
    (121, Token (QCONID _)) -> Just (Shift 154)
    (121, Token (EXPORT _)) -> Just (Shift 329)
    (121, Token (AS _)) -> Just (Shift 330)
    (121, Token (QVARID _)) -> Just (Shift 331)
    (122, Token (RBRACE _)) -> Just (Reduce 0 119)
    (122, Token (LPAREN _)) -> Just (Shift 147)
    (122, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (122, Token (EQUAL _)) -> Just (Shift 125)
    (122, Token (DERIVING _)) -> Just (Reduce 0 119)
    (122, Token (DARROW _)) -> Just (Shift 123)
    (122, Token (LBRACKET _)) -> Just (Shift 151)
    (122, Token (EXCL _)) -> Just (Shift 107)
    (122, Token (QCONID _)) -> Just (Shift 154)
    (122, Token (EXPORT _)) -> Just (Shift 329)
    (122, Token (AS _)) -> Just (Shift 330)
    (122, Token (QVARID _)) -> Just (Shift 331)
    (123, Token (LPAREN _)) -> Just (Shift 147)
    (123, Token (LBRACKET _)) -> Just (Shift 151)
    (123, Token (EXCL _)) -> Just (Shift 107)
    (123, Token (QCONID _)) -> Just (Shift 154)
    (123, Token (EXPORT _)) -> Just (Shift 329)
    (123, Token (AS _)) -> Just (Shift 330)
    (123, Token (QVARID _)) -> Just (Shift 331)
    (124, Token (RBRACE _)) -> Just (Reduce 0 119)
    (124, Token (LPAREN _)) -> Just (Shift 147)
    (124, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (124, Token (EQUAL _)) -> Just (Shift 125)
    (124, Token (DERIVING _)) -> Just (Reduce 0 119)
    (124, Token (LBRACKET _)) -> Just (Shift 151)
    (124, Token (EXCL _)) -> Just (Shift 107)
    (124, Token (QCONID _)) -> Just (Shift 154)
    (124, Token (EXPORT _)) -> Just (Shift 329)
    (124, Token (AS _)) -> Just (Shift 330)
    (124, Token (QVARID _)) -> Just (Shift 331)
    (125, Token (LPAREN _)) -> Just (Shift 147)
    (125, Token (LBRACKET _)) -> Just (Shift 151)
    (125, Token (EXCL _)) -> Just (Shift 107)
    (125, Token (QCONID _)) -> Just (Shift 154)
    (125, Token (EXPORT _)) -> Just (Shift 329)
    (125, Token (AS _)) -> Just (Shift 330)
    (125, Token (QVARID _)) -> Just (Shift 331)
    (126, Token (LPAREN _)) -> Just (Shift 147)
    (126, Token (LBRACKET _)) -> Just (Shift 151)
    (126, Token (EXCL _)) -> Just (Shift 107)
    (126, Token (QCONID _)) -> Just (Shift 154)
    (126, Token (EXPORT _)) -> Just (Shift 329)
    (126, Token (AS _)) -> Just (Shift 330)
    (126, Token (QVARID _)) -> Just (Shift 331)
    (127, Token (LPAREN _)) -> Just (Shift 152)
    (127, Token (QCONID _)) -> Just (Shift 154)
    (128, Token (RBRACE _)) -> Just (Reduce 1 123)
    (128, Token (LPAREN _)) -> Just (Shift 147)
    (128, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (128, Token (DERIVING _)) -> Just (Reduce 1 123)
    (128, Token (PIPE _)) -> Just (Reduce 1 123)
    (128, Token (LBRACKET _)) -> Just (Shift 151)
    (128, Token (EXCL _)) -> Just (Shift 107)
    (128, Token (QCONID _)) -> Just (Shift 154)
    (128, Token (EXPORT _)) -> Just (Shift 329)
    (128, Token (AS _)) -> Just (Shift 330)
    (128, Token (QVARID _)) -> Just (Shift 331)
    (128, Token (BACKQUOTE _)) -> Just (Shift 343)
    (128, Token (QCONSYM _)) -> Just (Shift 345)
    (129, Token (LPAREN _)) -> Just (Shift 147)
    (129, Token (LBRACKET _)) -> Just (Shift 151)
    (129, Token (EXCL _)) -> Just (Shift 107)
    (129, Token (QCONID _)) -> Just (Shift 154)
    (129, Token (EXPORT _)) -> Just (Shift 329)
    (129, Token (AS _)) -> Just (Shift 330)
    (129, Token (QVARID _)) -> Just (Shift 331)
    (130, Token (RBRACE _)) -> Just (Reduce 3 124)
    (130, Token (LPAREN _)) -> Just (Shift 147)
    (130, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (130, Token (DERIVING _)) -> Just (Reduce 3 124)
    (130, Token (PIPE _)) -> Just (Reduce 3 124)
    (130, Token (LBRACKET _)) -> Just (Shift 151)
    (130, Token (EXCL _)) -> Just (Shift 107)
    (130, Token (QCONID _)) -> Just (Shift 154)
    (130, Token (EXPORT _)) -> Just (Shift 329)
    (130, Token (AS _)) -> Just (Shift 330)
    (130, Token (QVARID _)) -> Just (Shift 331)
    (131, Token (LPAREN _)) -> Just (Shift 147)
    (131, Token (LBRACKET _)) -> Just (Shift 151)
    (131, Token (EXCL _)) -> Just (Shift 107)
    (131, Token (QCONID _)) -> Just (Shift 154)
    (131, Token (EXPORT _)) -> Just (Shift 329)
    (131, Token (AS _)) -> Just (Shift 330)
    (131, Token (QVARID _)) -> Just (Shift 331)
    (132, Token (RBRACE _)) -> Just (Reduce 1 100)
    (132, Token (LPAREN _)) -> Just (Shift 147)
    (132, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (132, Token (DARROW _)) -> Just (Shift 137)
    (132, Token (RARROW _)) -> Just (Shift 118)
    (132, Token (LBRACKET _)) -> Just (Shift 151)
    (132, Token (EXCL _)) -> Just (Shift 107)
    (132, Token (QCONID _)) -> Just (Shift 154)
    (132, Token (EXPORT _)) -> Just (Shift 329)
    (132, Token (AS _)) -> Just (Shift 330)
    (132, Token (QVARID _)) -> Just (Shift 331)
    (133, Token (LPAREN _)) -> Just (Shift 147)
    (133, Token (LBRACKET _)) -> Just (Shift 151)
    (133, Token (EXCL _)) -> Just (Shift 107)
    (133, Token (QCONID _)) -> Just (Shift 154)
    (133, Token (EXPORT _)) -> Just (Shift 329)
    (133, Token (AS _)) -> Just (Shift 330)
    (133, Token (QVARID _)) -> Just (Shift 331)
    (134, Token (LPAREN _)) -> Just (Shift 147)
    (134, Token (LBRACKET _)) -> Just (Shift 151)
    (134, Token (EXCL _)) -> Just (Shift 107)
    (134, Token (QCONID _)) -> Just (Shift 154)
    (134, Token (EXPORT _)) -> Just (Shift 329)
    (134, Token (AS _)) -> Just (Shift 330)
    (134, Token (QVARID _)) -> Just (Shift 331)
    (135, Token (LPAREN _)) -> Just (Shift 147)
    (135, Token (LBRACKET _)) -> Just (Shift 151)
    (135, Token (EXCL _)) -> Just (Shift 107)
    (135, Token (QCONID _)) -> Just (Shift 154)
    (135, Token (EXPORT _)) -> Just (Shift 329)
    (135, Token (AS _)) -> Just (Shift 330)
    (135, Token (QVARID _)) -> Just (Shift 331)
    (136, Token (LPAREN _)) -> Just (Shift 147)
    (136, Token (LBRACKET _)) -> Just (Shift 151)
    (136, Token (EXCL _)) -> Just (Shift 107)
    (136, Token (QCONID _)) -> Just (Shift 154)
    (136, Token (EXPORT _)) -> Just (Shift 329)
    (136, Token (AS _)) -> Just (Shift 330)
    (136, Token (QVARID _)) -> Just (Shift 331)
    (137, Token (LPAREN _)) -> Just (Shift 147)
    (137, Token (LBRACKET _)) -> Just (Shift 151)
    (137, Token (EXCL _)) -> Just (Shift 107)
    (137, Token (QCONID _)) -> Just (Shift 154)
    (137, Token (EXPORT _)) -> Just (Shift 329)
    (137, Token (AS _)) -> Just (Shift 330)
    (137, Token (QVARID _)) -> Just (Shift 331)
    (138, Token (LPAREN _)) -> Just (Shift 147)
    (138, Token (LBRACKET _)) -> Just (Shift 151)
    (138, Token (EXCL _)) -> Just (Shift 107)
    (138, Token (QCONID _)) -> Just (Shift 154)
    (138, Token (EXPORT _)) -> Just (Shift 329)
    (138, Token (AS _)) -> Just (Shift 330)
    (138, Token (QVARID _)) -> Just (Shift 331)
    (139, Token (LPAREN _)) -> Just (Shift 147)
    (139, Token (LBRACKET _)) -> Just (Shift 151)
    (139, Token (EXCL _)) -> Just (Shift 107)
    (139, Token (QCONID _)) -> Just (Shift 154)
    (139, Token (EXPORT _)) -> Just (Shift 329)
    (139, Token (AS _)) -> Just (Shift 330)
    (139, Token (QVARID _)) -> Just (Shift 331)
    (140, Token (LBRACE _)) -> Just (Shift 94)
    (140, Token (LPAREN _)) -> Just (Shift 147)
    (140, Token (LBRACKET _)) -> Just (Shift 151)
    (140, Token (EXCL _)) -> Just (Shift 107)
    (140, Token (QCONID _)) -> Just (Shift 154)
    (140, Token (EXPORT _)) -> Just (Shift 329)
    (140, Token (AS _)) -> Just (Shift 330)
    (140, Token (QVARID _)) -> Just (Shift 331)
    (141, Token (LPAREN _)) -> Just (Shift 147)
    (141, Token (LBRACKET _)) -> Just (Shift 151)
    (141, Token (EXCL _)) -> Just (Shift 107)
    (141, Token (QCONID _)) -> Just (Shift 154)
    (141, Token (EXPORT _)) -> Just (Shift 329)
    (141, Token (AS _)) -> Just (Shift 330)
    (141, Token (QVARID _)) -> Just (Shift 331)
    (142, Token (LPAREN _)) -> Just (Shift 147)
    (142, Token (EQUAL _)) -> Just (Shift 127)
    (142, Token (DARROW _)) -> Just (Shift 144)
    (142, Token (LBRACKET _)) -> Just (Shift 151)
    (142, Token (EXCL _)) -> Just (Shift 107)
    (142, Token (QCONID _)) -> Just (Shift 154)
    (142, Token (EXPORT _)) -> Just (Shift 329)
    (142, Token (AS _)) -> Just (Shift 330)
    (142, Token (QVARID _)) -> Just (Shift 331)
    (143, Token (LPAREN _)) -> Just (Shift 147)
    (143, Token (LBRACKET _)) -> Just (Shift 151)
    (143, Token (EXCL _)) -> Just (Shift 107)
    (143, Token (QCONID _)) -> Just (Shift 154)
    (143, Token (EXPORT _)) -> Just (Shift 329)
    (143, Token (AS _)) -> Just (Shift 330)
    (143, Token (QVARID _)) -> Just (Shift 331)
    (144, Token (LPAREN _)) -> Just (Shift 147)
    (144, Token (LBRACKET _)) -> Just (Shift 151)
    (144, Token (EXCL _)) -> Just (Shift 107)
    (144, Token (QCONID _)) -> Just (Shift 154)
    (144, Token (EXPORT _)) -> Just (Shift 329)
    (144, Token (AS _)) -> Just (Shift 330)
    (144, Token (QVARID _)) -> Just (Shift 331)
    (145, Token (LPAREN _)) -> Just (Shift 147)
    (145, Token (EQUAL _)) -> Just (Shift 133)
    (145, Token (LBRACKET _)) -> Just (Shift 151)
    (145, Token (EXCL _)) -> Just (Shift 107)
    (145, Token (QCONID _)) -> Just (Shift 154)
    (145, Token (EXPORT _)) -> Just (Shift 329)
    (145, Token (AS _)) -> Just (Shift 330)
    (145, Token (QVARID _)) -> Just (Shift 331)
    (146, Token (LPAREN _)) -> Just (Shift 147)
    (146, Token (EQUAL _)) -> Just (Shift 127)
    (146, Token (LBRACKET _)) -> Just (Shift 151)
    (146, Token (EXCL _)) -> Just (Shift 107)
    (146, Token (QCONID _)) -> Just (Shift 154)
    (146, Token (EXPORT _)) -> Just (Shift 329)
    (146, Token (AS _)) -> Just (Shift 330)
    (146, Token (QVARID _)) -> Just (Shift 331)
    (147, Token (LPAREN _)) -> Just (Shift 147)
    (147, Token (RPAREN _)) -> Just (Shift 321)
    (147, Token (COMMA _)) -> Just (Shift 334)
    (147, Token (RARROW _)) -> Just (Shift 324)
    (147, Token (LBRACKET _)) -> Just (Shift 151)
    (147, Token (EXCL _)) -> Just (Shift 107)
    (147, Token (QCONID _)) -> Just (Shift 154)
    (147, Token (EXPORT _)) -> Just (Shift 329)
    (147, Token (AS _)) -> Just (Shift 330)
    (147, Token (QVARID _)) -> Just (Shift 331)
    (147, Token (QCONSYM _)) -> Just (Shift 155)
    (148, Token (LPAREN _)) -> Just (Shift 147)
    (148, Token (RPAREN _)) -> Just (Shift 177)
    (148, Token (LBRACKET _)) -> Just (Shift 151)
    (148, Token (EXCL _)) -> Just (Shift 107)
    (148, Token (QCONID _)) -> Just (Shift 154)
    (148, Token (EXPORT _)) -> Just (Shift 329)
    (148, Token (AS _)) -> Just (Shift 330)
    (148, Token (QVARID _)) -> Just (Shift 331)
    (149, Token (LPAREN _)) -> Just (Shift 147)
    (149, Token (LBRACKET _)) -> Just (Shift 151)
    (149, Token (EXCL _)) -> Just (Shift 107)
    (149, Token (QCONID _)) -> Just (Shift 154)
    (149, Token (EXPORT _)) -> Just (Shift 329)
    (149, Token (AS _)) -> Just (Shift 330)
    (149, Token (QVARID _)) -> Just (Shift 331)
    (150, Token (LPAREN _)) -> Just (Shift 147)
    (150, Token (LBRACKET _)) -> Just (Shift 151)
    (150, Token (EXCL _)) -> Just (Shift 107)
    (150, Token (QCONID _)) -> Just (Shift 154)
    (150, Token (EXPORT _)) -> Just (Shift 329)
    (150, Token (AS _)) -> Just (Shift 330)
    (150, Token (QVARID _)) -> Just (Shift 331)
    (151, Token (LPAREN _)) -> Just (Shift 147)
    (151, Token (LBRACKET _)) -> Just (Shift 151)
    (151, Token (RBRACKET _)) -> Just (Shift 325)
    (151, Token (EXCL _)) -> Just (Shift 107)
    (151, Token (QCONID _)) -> Just (Shift 154)
    (151, Token (EXPORT _)) -> Just (Shift 329)
    (151, Token (AS _)) -> Just (Shift 330)
    (151, Token (QVARID _)) -> Just (Shift 331)
    (152, Token (QCONSYM _)) -> Just (Shift 155)
    (153, Token (WHERE _)) -> Just (Reduce 3 207)
    (153, Token (LBRACE _)) -> Just (Reduce 3 207)
    (153, Token (RBRACE _)) -> Just (Reduce 3 207)
    (153, Token (LPAREN _)) -> Just (Reduce 3 207)
    (153, Token (RPAREN _)) -> Just (Reduce 3 207)
    (153, Token (COMMA _)) -> Just (Reduce 3 207)
    (153, Token (SEMICOLON _)) -> Just (Reduce 3 207)
    (153, Token (EQUAL _)) -> Just (Reduce 3 207)
    (153, Token (DERIVING _)) -> Just (Reduce 3 207)
    (153, Token (DARROW _)) -> Just (Reduce 3 207)
    (153, Token (PIPE _)) -> Just (Reduce 3 207)
    (153, Token (COLON_COLON _)) -> Just (Reduce 3 207)
    (153, Token (MINUS _)) -> Just (Reduce 3 207)
    (153, Token (INFIXL _)) -> Just (Reduce 3 207)
    (153, Token (INFIXR _)) -> Just (Reduce 3 207)
    (153, Token (INFIX _)) -> Just (Reduce 3 207)
    (153, Token (RARROW _)) -> Just (Reduce 3 207)
    (153, Token (LBRACKET _)) -> Just (Reduce 3 207)
    (153, Token (RBRACKET _)) -> Just (Reduce 3 207)
    (153, Token (EXCL _)) -> Just (Reduce 3 207)
    (153, Token (QCONID _)) -> Just (Reduce 3 207)
    (153, Token (EXPORT _)) -> Just (Reduce 3 207)
    (153, Token (AS _)) -> Just (Reduce 3 207)
    (153, Token (QVARID _)) -> Just (Reduce 3 207)
    (153, Token (LARROW _)) -> Just (Reduce 3 207)
    (153, Token (THEN _)) -> Just (Reduce 3 207)
    (153, Token (ELSE _)) -> Just (Reduce 3 207)
    (153, Token (QVARSYM _)) -> Just (Reduce 3 207)
    (153, Token (BACKQUOTE _)) -> Just (Reduce 3 207)
    (153, Token (QCONSYM _)) -> Just (Reduce 3 207)
    (153, Token (OF _)) -> Just (Reduce 3 207)
    (153, Token (INTEGER _)) -> Just (Reduce 3 207)
    (154, Token (WHERE _)) -> Just (Reduce 1 206)
    (154, Token (LBRACE _)) -> Just (Reduce 1 206)
    (154, Token (RBRACE _)) -> Just (Reduce 1 206)
    (154, Token (LPAREN _)) -> Just (Reduce 1 206)
    (154, Token (RPAREN _)) -> Just (Reduce 1 206)
    (154, Token (COMMA _)) -> Just (Reduce 1 206)
    (154, Token (SEMICOLON _)) -> Just (Reduce 1 206)
    (154, Token (EQUAL _)) -> Just (Reduce 1 206)
    (154, Token (DERIVING _)) -> Just (Reduce 1 206)
    (154, Token (DARROW _)) -> Just (Reduce 1 206)
    (154, Token (PIPE _)) -> Just (Reduce 1 206)
    (154, Token (COLON_COLON _)) -> Just (Reduce 1 206)
    (154, Token (MINUS _)) -> Just (Reduce 1 206)
    (154, Token (INFIXL _)) -> Just (Reduce 1 206)
    (154, Token (INFIXR _)) -> Just (Reduce 1 206)
    (154, Token (INFIX _)) -> Just (Reduce 1 206)
    (154, Token (RARROW _)) -> Just (Reduce 1 206)
    (154, Token (LBRACKET _)) -> Just (Reduce 1 206)
    (154, Token (RBRACKET _)) -> Just (Reduce 1 206)
    (154, Token (EXCL _)) -> Just (Reduce 1 206)
    (154, Token (QCONID _)) -> Just (Reduce 1 206)
    (154, Token (EXPORT _)) -> Just (Reduce 1 206)
    (154, Token (AS _)) -> Just (Reduce 1 206)
    (154, Token (QVARID _)) -> Just (Reduce 1 206)
    (154, Token (LARROW _)) -> Just (Reduce 1 206)
    (154, Token (THEN _)) -> Just (Reduce 1 206)
    (154, Token (ELSE _)) -> Just (Reduce 1 206)
    (154, Token (QVARSYM _)) -> Just (Reduce 1 206)
    (154, Token (BACKQUOTE _)) -> Just (Reduce 1 206)
    (154, Token (QCONSYM _)) -> Just (Reduce 1 206)
    (154, Token (OF _)) -> Just (Reduce 1 206)
    (154, Token (INTEGER _)) -> Just (Reduce 1 206)
    (155, Token (RPAREN _)) -> Just (Shift 153)
    (156, Token (RPAREN _)) -> Just (Reduce 3 24)
    (157, Token (RPAREN _)) -> Just (Reduce 1 23)
    (157, Token (COMMA _)) -> Just (Shift 89)
    (158, Token (RPAREN _)) -> Just (Reduce 3 17)
    (159, Token (RPAREN _)) -> Just (Reduce 1 16)
    (159, Token (COMMA _)) -> Just (Shift 86)
    (160, Token (RPAREN _)) -> Just (Reduce 3 20)
    (160, Token (COMMA _)) -> Just (Reduce 3 20)
    (161, Token (RPAREN _)) -> Just (Reduce 4 21)
    (161, Token (COMMA _)) -> Just (Reduce 4 21)
    (162, Token (RPAREN _)) -> Just (Reduce 4 22)
    (162, Token (COMMA _)) -> Just (Reduce 4 22)
    (163, Token (RPAREN _)) -> Just (Shift 161)
    (164, Token (RPAREN _)) -> Just (Reduce 1 18)
    (164, Token (COMMA _)) -> Just (Reduce 1 18)
    (165, Token (LPAREN _)) -> Just (Shift 90)
    (165, Token (RPAREN _)) -> Just (Reduce 1 19)
    (165, Token (COMMA _)) -> Just (Reduce 1 19)
    (166, Token (RPAREN _)) -> Just (Shift 162)
    (167, Token (RPAREN _)) -> Just (Reduce 1 25)
    (167, Token (COMMA _)) -> Just (Reduce 1 25)
    (168, Token (RPAREN _)) -> Just (Reduce 1 26)
    (168, Token (COMMA _)) -> Just (Reduce 1 26)
    (169, Token (RPAREN _)) -> Just (Shift 173)
    (169, Token (QCONID _)) -> Just (Shift 224)
    (170, Token (RPAREN _)) -> Just (Shift 174)
    (170, Token (QCONID _)) -> Just (Shift 224)
    (171, Token (RPAREN _)) -> Just (Shift 175)
    (171, Token (QCONID _)) -> Just (Shift 224)
    (172, Token (RPAREN _)) -> Just (Shift 176)
    (172, Token (QCONID _)) -> Just (Shift 224)
    (173, Token (RBRACE _)) -> Just (Reduce 6 35)
    (173, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (174, Token (RBRACE _)) -> Just (Reduce 8 39)
    (174, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (175, Token (RBRACE _)) -> Just (Reduce 8 47)
    (175, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (176, Token (RBRACE _)) -> Just (Reduce 6 43)
    (176, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (177, Token (RBRACE _)) -> Just (Reduce 3 53)
    (177, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (178, Token (RBRACE _)) -> Just (Reduce 8 31)
    (178, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (179, Token (RBRACE _)) -> Just (Reduce 7 30)
    (179, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (180, Token (RBRACE _)) -> Just (Reduce 7 36)
    (180, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (181, Token (RBRACE _)) -> Just (Reduce 9 40)
    (181, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (182, Token (RBRACE _)) -> Just (Reduce 9 48)
    (182, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (183, Token (RBRACE _)) -> Just (Reduce 7 44)
    (183, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (184, Token (RBRACE _)) -> Just (Reduce 4 54)
    (184, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (185, Token (QCONID _)) -> Just (Reduce 0 218)
    (185, Token (QUALIFIED _)) -> Just (Shift 217)
    (186, Token (LPAREN _)) -> Just (Shift 87)
    (187, Token (LPAREN _)) -> Just (Shift 169)
    (187, Token (QCONID _)) -> Just (Shift 224)
    (188, Token (LPAREN _)) -> Just (Shift 170)
    (188, Token (QCONID _)) -> Just (Shift 224)
    (189, Token (LPAREN _)) -> Just (Shift 171)
    (189, Token (QCONID _)) -> Just (Shift 224)
    (190, Token (LPAREN _)) -> Just (Shift 172)
    (190, Token (QCONID _)) -> Just (Shift 224)
    (191, Token (LPAREN _)) -> Just (Shift 148)
    (192, Token (IMPORT _)) -> Just (Shift 237)
    (192, Token (EXPORT _)) -> Just (Shift 238)
    (193, Token (RBRACE _)) -> Just (Reduce 0 216)
    (193, Token (LPAREN _)) -> Just (Reduce 0 216)
    (193, Token (SEMICOLON _)) -> Just (Reduce 0 216)
    (193, Token (HIDING _)) -> Just (Reduce 0 216)
    (193, Token (AS _)) -> Just (Shift 9)
    (194, Token (RPAREN _)) -> Just (Shift 178)
    (195, Token (RPAREN _)) -> Just (Shift 179)
    (196, Token (RBRACE _)) -> Just (Reduce 4 29)
    (196, Token (LPAREN _)) -> Just (Shift 88)
    (196, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (196, Token (HIDING _)) -> Just (Shift 186)
    (197, Token (RBRACE _)) -> Just (Reduce 4 32)
    (197, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (198, Token (RBRACE _)) -> Just (Reduce 3 33)
    (198, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (198, Token (DERIVING _)) -> Just (Shift 187)
    (199, Token (RBRACE _)) -> Just (Reduce 5 37)
    (199, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (199, Token (DERIVING _)) -> Just (Shift 188)
    (200, Token (RBRACE _)) -> Just (Reduce 5 34)
    (200, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (201, Token (RBRACE _)) -> Just (Reduce 7 38)
    (201, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (202, Token (RBRACE _)) -> Just (Reduce 7 46)
    (202, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (203, Token (RBRACE _)) -> Just (Reduce 5 42)
    (203, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (204, Token (RPAREN _)) -> Just (Shift 180)
    (205, Token (RPAREN _)) -> Just (Shift 181)
    (206, Token (RPAREN _)) -> Just (Shift 182)
    (207, Token (RPAREN _)) -> Just (Shift 183)
    (208, Token (RBRACE _)) -> Just (Reduce 5 45)
    (208, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (208, Token (DERIVING _)) -> Just (Shift 189)
    (209, Token (RBRACE _)) -> Just (Reduce 3 41)
    (209, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (209, Token (DERIVING _)) -> Just (Shift 190)
    (210, Token (RBRACE _)) -> Just (Reduce 5 50)
    (210, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (211, Token (RBRACE _)) -> Just (Reduce 3 49)
    (211, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (212, Token (RBRACE _)) -> Just (Reduce 5 52)
    (212, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (213, Token (RBRACE _)) -> Just (Reduce 3 51)
    (213, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (214, Token (RPAREN _)) -> Just (Shift 184)
    (215, Token (RBRACE _)) -> Just (Reduce 2 55)
    (215, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (216, Token (RBRACE _)) -> Just (Reduce 1 56)
    (216, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (217, Token (QCONID _)) -> Just (Reduce 1 219)
    (218, Token (RBRACE _)) -> Just (Reduce 2 217)
    (218, Token (LPAREN _)) -> Just (Reduce 2 217)
    (218, Token (SEMICOLON _)) -> Just (Reduce 2 217)
    (218, Token (HIDING _)) -> Just (Reduce 2 217)
    (219, Token (WHERE _)) -> Just (Reduce 1 102)
    (219, Token (LBRACE _)) -> Just (Reduce 1 102)
    (219, Token (RBRACE _)) -> Just (Reduce 1 102)
    (219, Token (LPAREN _)) -> Just (Reduce 1 102)
    (219, Token (RPAREN _)) -> Just (Reduce 1 102)
    (219, Token (COMMA _)) -> Just (Reduce 1 102)
    (219, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (219, Token (EQUAL _)) -> Just (Reduce 1 102)
    (219, Token (DERIVING _)) -> Just (Reduce 1 102)
    (219, Token (DARROW _)) -> Just (Reduce 1 102)
    (219, Token (PIPE _)) -> Just (Reduce 1 102)
    (219, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (219, Token (MINUS _)) -> Just (Reduce 1 102)
    (219, Token (INFIXL _)) -> Just (Reduce 1 102)
    (219, Token (INFIXR _)) -> Just (Reduce 1 102)
    (219, Token (INFIX _)) -> Just (Reduce 1 102)
    (219, Token (RARROW _)) -> Just (Reduce 1 102)
    (219, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (219, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (219, Token (EXCL _)) -> Just (Reduce 1 102)
    (219, Token (QCONID _)) -> Just (Reduce 1 102)
    (219, Token (EXPORT _)) -> Just (Reduce 1 102)
    (219, Token (AS _)) -> Just (Reduce 1 102)
    (219, Token (QVARID _)) -> Just (Reduce 1 102)
    (219, Token (LARROW _)) -> Just (Reduce 1 102)
    (219, Token (THEN _)) -> Just (Reduce 1 102)
    (219, Token (ELSE _)) -> Just (Reduce 1 102)
    (219, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (219, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (219, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (219, Token (OF _)) -> Just (Reduce 1 102)
    (219, Token (INTEGER _)) -> Just (Reduce 1 102)
    (220, Token (WHERE _)) -> Just (Reduce 2 103)
    (220, Token (LBRACE _)) -> Just (Reduce 2 103)
    (220, Token (RBRACE _)) -> Just (Reduce 2 103)
    (220, Token (LPAREN _)) -> Just (Reduce 2 103)
    (220, Token (RPAREN _)) -> Just (Reduce 2 103)
    (220, Token (COMMA _)) -> Just (Reduce 2 103)
    (220, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (220, Token (EQUAL _)) -> Just (Reduce 2 103)
    (220, Token (DERIVING _)) -> Just (Reduce 2 103)
    (220, Token (DARROW _)) -> Just (Reduce 2 103)
    (220, Token (PIPE _)) -> Just (Reduce 2 103)
    (220, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (220, Token (MINUS _)) -> Just (Reduce 2 103)
    (220, Token (INFIXL _)) -> Just (Reduce 2 103)
    (220, Token (INFIXR _)) -> Just (Reduce 2 103)
    (220, Token (INFIX _)) -> Just (Reduce 2 103)
    (220, Token (RARROW _)) -> Just (Reduce 2 103)
    (220, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (220, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (220, Token (EXCL _)) -> Just (Reduce 2 103)
    (220, Token (QCONID _)) -> Just (Reduce 2 103)
    (220, Token (EXPORT _)) -> Just (Reduce 2 103)
    (220, Token (AS _)) -> Just (Reduce 2 103)
    (220, Token (QVARID _)) -> Just (Reduce 2 103)
    (220, Token (LARROW _)) -> Just (Reduce 2 103)
    (220, Token (THEN _)) -> Just (Reduce 2 103)
    (220, Token (ELSE _)) -> Just (Reduce 2 103)
    (220, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (220, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (220, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (220, Token (OF _)) -> Just (Reduce 2 103)
    (220, Token (INTEGER _)) -> Just (Reduce 2 103)
    (221, Token (WHERE _)) -> Just (Reduce 3 101)
    (221, Token (RBRACE _)) -> Just (Reduce 3 101)
    (221, Token (RPAREN _)) -> Just (Reduce 3 101)
    (221, Token (COMMA _)) -> Just (Reduce 3 101)
    (221, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (221, Token (EQUAL _)) -> Just (Reduce 3 101)
    (221, Token (PIPE _)) -> Just (Reduce 3 101)
    (221, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (221, Token (LARROW _)) -> Just (Reduce 3 101)
    (221, Token (THEN _)) -> Just (Reduce 3 101)
    (221, Token (ELSE _)) -> Just (Reduce 3 101)
    (221, Token (OF _)) -> Just (Reduce 3 101)
    (222, Token (RBRACE _)) -> Just (Reduce 2 120)
    (222, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (222, Token (DERIVING _)) -> Just (Reduce 2 120)
    (223, Token (QCONID _)) -> Just (Shift 224)
    (224, Token (RBRACE _)) -> Just (Reduce 1 134)
    (224, Token (RPAREN _)) -> Just (Reduce 1 134)
    (224, Token (COMMA _)) -> Just (Reduce 1 134)
    (224, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (225, Token (RPAREN _)) -> Just (Reduce 1 132)
    (225, Token (COMMA _)) -> Just (Shift 223)
    (226, Token (RPAREN _)) -> Just (Reduce 3 133)
    (227, Token (RBRACE _)) -> Just (Reduce 7 128)
    (227, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (227, Token (DERIVING _)) -> Just (Reduce 7 128)
    (228, Token (COLON_COLON _)) -> Just (Shift 139)
    (229, Token (RBRACE _)) -> Just (Shift 227)
    (230, Token (RBRACE _)) -> Just (Reduce 3 127)
    (230, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (230, Token (DERIVING _)) -> Just (Reduce 3 127)
    (231, Token (LBRACE _)) -> Just (Shift 74)
    (232, Token (RBRACE _)) -> Just (Reduce 2 66)
    (232, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (233, Token (LBRACE _)) -> Just (Shift 77)
    (234, Token (RBRACE _)) -> Just (Reduce 2 76)
    (234, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (235, Token (RPAREN _)) -> Just (Reduce 1 98)
    (235, Token (COMMA _)) -> Just (Shift 149)
    (236, Token (RPAREN _)) -> Just (Reduce 3 99)
    (237, Token (EXPORT _)) -> Just (Shift 350)
    (237, Token (AS _)) -> Just (Shift 351)
    (237, Token (QVARID _)) -> Just (Shift 352)
    (238, Token (EXPORT _)) -> Just (Shift 350)
    (238, Token (AS _)) -> Just (Shift 351)
    (238, Token (QVARID _)) -> Just (Shift 352)
    (239, Token (COLON_COLON _)) -> Just (Shift 134)
    (240, Token (COLON_COLON _)) -> Just (Shift 135)
    (241, Token (COLON_COLON _)) -> Just (Shift 136)
    (242, Token (RBRACE _)) -> Just (Reduce 6 135)
    (242, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (243, Token (RBRACE _)) -> Just (Reduce 7 136)
    (243, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (244, Token (RBRACE _)) -> Just (Reduce 6 137)
    (244, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (245, Token (EXPORT _)) -> Just (Shift 354)
    (245, Token (AS _)) -> Just (Shift 355)
    (245, Token (QVARID _)) -> Just (Shift 356)
    (245, Token (STRING _)) -> Just (Shift 353)
    (246, Token (STRING _)) -> Just (Shift 357)
    (247, Token (STRING _)) -> Just (Shift 353)
    (248, Token (LBRACE _)) -> Just (Shift 72)
    (249, Token (LBRACE _)) -> Just (Shift 72)
    (250, Token (RBRACE _)) -> Just (Reduce 5 62)
    (250, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (251, Token (RBRACE _)) -> Just (Reduce 5 64)
    (251, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (252, Token (RBRACE _)) -> Just (Reduce 1 60)
    (252, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (253, Token (WHERE _)) -> Just (Shift 248)
    (253, Token (RBRACE _)) -> Just (Reduce 3 61)
    (253, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (254, Token (WHERE _)) -> Just (Shift 249)
    (254, Token (RBRACE _)) -> Just (Reduce 3 63)
    (254, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (255, Token (LBRACE _)) -> Just (Shift 72)
    (256, Token (LBRACE _)) -> Just (Shift 72)
    (257, Token (LBRACE _)) -> Just (Shift 72)
    (258, Token (LBRACE _)) -> Just (Shift 72)
    (259, Token (LBRACE _)) -> Just (Shift 72)
    (260, Token (LBRACE _)) -> Just (Shift 72)
    (261, Token (LBRACE _)) -> Just (Shift 72)
    (262, Token (RBRACE _)) -> Just (Reduce 3 57)
    (262, Token (COMMA _)) -> Just (Reduce 3 57)
    (262, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (262, Token (EQUAL _)) -> Just (Reduce 3 57)
    (262, Token (IN _)) -> Just (Reduce 3 57)
    (263, Token (RBRACE _)) -> Just (Shift 262)
    (264, Token (RBRACE _)) -> Just (Reduce 1 58)
    (264, Token (SEMICOLON _)) -> Just (Shift 73)
    (265, Token (RBRACE _)) -> Just (Reduce 3 59)
    (266, Token (RBRACE _)) -> Just (Reduce 5 87)
    (266, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (267, Token (RBRACE _)) -> Just (Reduce 3 86)
    (267, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (268, Token (COLON_COLON _)) -> Just (Shift 131)
    (269, Token (COMMA _)) -> Just (Reduce 0 225)
    (269, Token (MINUS _)) -> Just (Reduce 0 225)
    (269, Token (QCONID _)) -> Just (Reduce 0 225)
    (269, Token (EXPORT _)) -> Just (Reduce 0 225)
    (269, Token (AS _)) -> Just (Reduce 0 225)
    (269, Token (QVARID _)) -> Just (Reduce 0 225)
    (269, Token (QVARSYM _)) -> Just (Reduce 0 225)
    (269, Token (BACKQUOTE _)) -> Just (Reduce 0 225)
    (269, Token (QCONSYM _)) -> Just (Reduce 0 225)
    (269, Token (INTEGER _)) -> Just (Shift 303)
    (270, Token (MINUS _)) -> Just (Shift 306)
    (270, Token (QVARSYM _)) -> Just (Shift 429)
    (270, Token (BACKQUOTE _)) -> Just (Shift 342)
    (270, Token (QCONSYM _)) -> Just (Shift 345)
    (271, Token (RBRACE _)) -> Just (Reduce 3 88)
    (271, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (272, Token (LPAREN _)) -> Just (Reduce 1 195)
    (272, Token (RPAREN _)) -> Just (Reduce 1 195)
    (272, Token (EQUAL _)) -> Just (Reduce 1 195)
    (272, Token (PIPE _)) -> Just (Reduce 1 195)
    (272, Token (MINUS _)) -> Just (Reduce 1 195)
    (272, Token (RARROW _)) -> Just (Reduce 1 195)
    (272, Token (QCONID _)) -> Just (Reduce 1 195)
    (272, Token (EXPORT _)) -> Just (Reduce 1 195)
    (272, Token (AS _)) -> Just (Reduce 1 195)
    (272, Token (QVARID _)) -> Just (Reduce 1 195)
    (272, Token (QVARSYM _)) -> Just (Reduce 1 195)
    (272, Token (BACKQUOTE _)) -> Just (Reduce 1 195)
    (272, Token (QCONSYM _)) -> Just (Reduce 1 195)
    (273, Token (LPAREN _)) -> Just (Reduce 3 197)
    (273, Token (RPAREN _)) -> Just (Reduce 3 197)
    (273, Token (EQUAL _)) -> Just (Reduce 3 197)
    (273, Token (PIPE _)) -> Just (Reduce 3 197)
    (273, Token (MINUS _)) -> Just (Reduce 3 197)
    (273, Token (RARROW _)) -> Just (Reduce 3 197)
    (273, Token (QCONID _)) -> Just (Reduce 3 197)
    (273, Token (EXPORT _)) -> Just (Reduce 3 197)
    (273, Token (AS _)) -> Just (Reduce 3 197)
    (273, Token (QVARID _)) -> Just (Reduce 3 197)
    (273, Token (QVARSYM _)) -> Just (Reduce 3 197)
    (273, Token (BACKQUOTE _)) -> Just (Reduce 3 197)
    (273, Token (QCONSYM _)) -> Just (Reduce 3 197)
    (274, Token (LPAREN _)) -> Just (Reduce 2 196)
    (274, Token (RPAREN _)) -> Just (Reduce 2 196)
    (274, Token (EQUAL _)) -> Just (Reduce 2 196)
    (274, Token (PIPE _)) -> Just (Reduce 2 196)
    (274, Token (MINUS _)) -> Just (Reduce 2 196)
    (274, Token (RARROW _)) -> Just (Reduce 2 196)
    (274, Token (QCONID _)) -> Just (Reduce 2 196)
    (274, Token (EXPORT _)) -> Just (Reduce 2 196)
    (274, Token (AS _)) -> Just (Reduce 2 196)
    (274, Token (QVARID _)) -> Just (Reduce 2 196)
    (274, Token (QVARSYM _)) -> Just (Reduce 2 196)
    (274, Token (BACKQUOTE _)) -> Just (Reduce 2 196)
    (274, Token (QCONSYM _)) -> Just (Reduce 2 196)
    (275, Token (LPAREN _)) -> Just (Reduce 3 198)
    (275, Token (RPAREN _)) -> Just (Reduce 3 198)
    (275, Token (EQUAL _)) -> Just (Reduce 3 198)
    (275, Token (PIPE _)) -> Just (Reduce 3 198)
    (275, Token (MINUS _)) -> Just (Reduce 3 198)
    (275, Token (RARROW _)) -> Just (Reduce 3 198)
    (275, Token (QCONID _)) -> Just (Reduce 3 198)
    (275, Token (EXPORT _)) -> Just (Reduce 3 198)
    (275, Token (AS _)) -> Just (Reduce 3 198)
    (275, Token (QVARID _)) -> Just (Reduce 3 198)
    (275, Token (QVARSYM _)) -> Just (Reduce 3 198)
    (275, Token (BACKQUOTE _)) -> Just (Reduce 3 198)
    (275, Token (QCONSYM _)) -> Just (Reduce 3 198)
    (276, Token (WHERE _)) -> Just (Reduce 1 153)
    (276, Token (RBRACE _)) -> Just (Reduce 1 153)
    (276, Token (RPAREN _)) -> Just (Reduce 1 153)
    (276, Token (COMMA _)) -> Just (Reduce 1 153)
    (276, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (276, Token (EQUAL _)) -> Just (Reduce 1 153)
    (276, Token (PIPE _)) -> Just (Reduce 1 153)
    (276, Token (LARROW _)) -> Just (Reduce 1 153)
    (276, Token (THEN _)) -> Just (Reduce 1 153)
    (276, Token (ELSE _)) -> Just (Reduce 1 153)
    (276, Token (OF _)) -> Just (Reduce 1 153)
    (277, Token (WHERE _)) -> Just (Reduce 3 146)
    (277, Token (RBRACE _)) -> Just (Reduce 3 146)
    (277, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (277, Token (PIPE _)) -> Just (Shift 60)
    (278, Token (WHERE _)) -> Just (Reduce 5 147)
    (278, Token (RBRACE _)) -> Just (Reduce 5 147)
    (278, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (279, Token (EQUAL _)) -> Just (Shift 47)
    (280, Token (RBRACE _)) -> Just (Reduce 3 67)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (281, Token (RBRACE _)) -> Just (Shift 280)
    (282, Token (RBRACE _)) -> Just (Reduce 3 69)
    (283, Token (RBRACE _)) -> Just (Reduce 1 68)
    (283, Token (SEMICOLON _)) -> Just (Shift 75)
    (284, Token (RBRACE _)) -> Just (Reduce 5 72)
    (284, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (285, Token (RBRACE _)) -> Just (Reduce 5 74)
    (285, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (286, Token (RBRACE _)) -> Just (Reduce 1 70)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (287, Token (WHERE _)) -> Just (Shift 255)
    (287, Token (RBRACE _)) -> Just (Reduce 3 71)
    (287, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (288, Token (WHERE _)) -> Just (Shift 256)
    (288, Token (RBRACE _)) -> Just (Reduce 3 73)
    (288, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (289, Token (RBRACE _)) -> Just (Reduce 3 77)
    (289, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (290, Token (RBRACE _)) -> Just (Shift 289)
    (291, Token (RBRACE _)) -> Just (Reduce 3 79)
    (292, Token (RBRACE _)) -> Just (Reduce 1 78)
    (292, Token (SEMICOLON _)) -> Just (Shift 78)
    (293, Token (RBRACE _)) -> Just (Reduce 5 82)
    (293, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (294, Token (RBRACE _)) -> Just (Reduce 5 84)
    (294, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (295, Token (WHERE _)) -> Just (Shift 257)
    (295, Token (RBRACE _)) -> Just (Reduce 3 81)
    (295, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (296, Token (WHERE _)) -> Just (Shift 258)
    (296, Token (RBRACE _)) -> Just (Reduce 3 83)
    (296, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (297, Token (COMMA _)) -> Just (Shift 91)
    (297, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (298, Token (LPAREN _)) -> Just (Reduce 1 199)
    (298, Token (COMMA _)) -> Just (Shift 91)
    (298, Token (EQUAL _)) -> Just (Reduce 1 199)
    (298, Token (PIPE _)) -> Just (Reduce 1 199)
    (298, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (298, Token (MINUS _)) -> Just (Reduce 1 199)
    (298, Token (QCONID _)) -> Just (Reduce 1 199)
    (298, Token (EXPORT _)) -> Just (Reduce 1 199)
    (298, Token (AS _)) -> Just (Reduce 1 199)
    (298, Token (QVARID _)) -> Just (Reduce 1 199)
    (298, Token (QVARSYM _)) -> Just (Reduce 1 199)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 1 199)
    (298, Token (QCONSYM _)) -> Just (Reduce 1 199)
    (299, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (300, Token (COMMA _)) -> Just (Reduce 1 95)
    (300, Token (MINUS _)) -> Just (Reduce 1 95)
    (300, Token (QCONID _)) -> Just (Reduce 1 95)
    (300, Token (EXPORT _)) -> Just (Reduce 1 95)
    (300, Token (AS _)) -> Just (Reduce 1 95)
    (300, Token (QVARID _)) -> Just (Reduce 1 95)
    (300, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (300, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (300, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (300, Token (INTEGER _)) -> Just (Reduce 1 95)
    (301, Token (COMMA _)) -> Just (Reduce 1 96)
    (301, Token (MINUS _)) -> Just (Reduce 1 96)
    (301, Token (QCONID _)) -> Just (Reduce 1 96)
    (301, Token (EXPORT _)) -> Just (Reduce 1 96)
    (301, Token (AS _)) -> Just (Reduce 1 96)
    (301, Token (QVARID _)) -> Just (Reduce 1 96)
    (301, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (301, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (301, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (301, Token (INTEGER _)) -> Just (Reduce 1 96)
    (302, Token (COMMA _)) -> Just (Reduce 1 97)
    (302, Token (MINUS _)) -> Just (Reduce 1 97)
    (302, Token (QCONID _)) -> Just (Reduce 1 97)
    (302, Token (EXPORT _)) -> Just (Reduce 1 97)
    (302, Token (AS _)) -> Just (Reduce 1 97)
    (302, Token (QVARID _)) -> Just (Reduce 1 97)
    (302, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (302, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (302, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (302, Token (INTEGER _)) -> Just (Reduce 1 97)
    (303, Token (COMMA _)) -> Just (Reduce 1 226)
    (303, Token (MINUS _)) -> Just (Reduce 1 226)
    (303, Token (QCONID _)) -> Just (Reduce 1 226)
    (303, Token (EXPORT _)) -> Just (Reduce 1 226)
    (303, Token (AS _)) -> Just (Reduce 1 226)
    (303, Token (QVARID _)) -> Just (Reduce 1 226)
    (303, Token (QVARSYM _)) -> Just (Reduce 1 226)
    (303, Token (BACKQUOTE _)) -> Just (Reduce 1 226)
    (303, Token (QCONSYM _)) -> Just (Reduce 1 226)
    (304, Token (MINUS _)) -> Just (Shift 306)
    (304, Token (QVARSYM _)) -> Just (Shift 429)
    (304, Token (BACKQUOTE _)) -> Just (Shift 342)
    (304, Token (QCONSYM _)) -> Just (Shift 345)
    (305, Token (MINUS _)) -> Just (Shift 306)
    (305, Token (QVARSYM _)) -> Just (Shift 429)
    (305, Token (BACKQUOTE _)) -> Just (Shift 342)
    (305, Token (QCONSYM _)) -> Just (Shift 345)
    (306, Token (RBRACE _)) -> Just (Reduce 1 89)
    (306, Token (COMMA _)) -> Just (Shift 304)
    (306, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (307, Token (RBRACE _)) -> Just (Reduce 3 91)
    (307, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (308, Token (RBRACE _)) -> Just (Reduce 3 92)
    (308, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (309, Token (RBRACE _)) -> Just (Reduce 1 90)
    (309, Token (COMMA _)) -> Just (Shift 305)
    (309, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (310, Token (RBRACE _)) -> Just (Reduce 1 215)
    (310, Token (LPAREN _)) -> Just (Reduce 1 215)
    (310, Token (COMMA _)) -> Just (Reduce 1 215)
    (310, Token (SEMICOLON _)) -> Just (Reduce 1 215)
    (310, Token (MINUS _)) -> Just (Reduce 1 215)
    (310, Token (QCONID _)) -> Just (Reduce 1 215)
    (310, Token (EXPORT _)) -> Just (Reduce 1 215)
    (310, Token (AS _)) -> Just (Reduce 1 215)
    (310, Token (QVARID _)) -> Just (Reduce 1 215)
    (310, Token (QVARSYM _)) -> Just (Reduce 1 215)
    (310, Token (BACKQUOTE _)) -> Just (Reduce 1 215)
    (310, Token (QCONSYM _)) -> Just (Reduce 1 215)
    (311, Token (RBRACE _)) -> Just (Reduce 1 214)
    (311, Token (LPAREN _)) -> Just (Reduce 1 214)
    (311, Token (COMMA _)) -> Just (Reduce 1 214)
    (311, Token (SEMICOLON _)) -> Just (Reduce 1 214)
    (311, Token (MINUS _)) -> Just (Reduce 1 214)
    (311, Token (QCONID _)) -> Just (Reduce 1 214)
    (311, Token (EXPORT _)) -> Just (Reduce 1 214)
    (311, Token (AS _)) -> Just (Reduce 1 214)
    (311, Token (QVARID _)) -> Just (Reduce 1 214)
    (311, Token (QVARSYM _)) -> Just (Reduce 1 214)
    (311, Token (BACKQUOTE _)) -> Just (Reduce 1 214)
    (311, Token (QCONSYM _)) -> Just (Reduce 1 214)
    (312, Token (WHERE _)) -> Just (Reduce 3 108)
    (312, Token (LBRACE _)) -> Just (Reduce 3 108)
    (312, Token (RBRACE _)) -> Just (Reduce 3 108)
    (312, Token (LPAREN _)) -> Just (Reduce 3 108)
    (312, Token (RPAREN _)) -> Just (Reduce 3 108)
    (312, Token (COMMA _)) -> Just (Reduce 3 108)
    (312, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (312, Token (EQUAL _)) -> Just (Reduce 3 108)
    (312, Token (DERIVING _)) -> Just (Reduce 3 108)
    (312, Token (DARROW _)) -> Just (Reduce 3 108)
    (312, Token (PIPE _)) -> Just (Reduce 3 108)
    (312, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (312, Token (MINUS _)) -> Just (Reduce 3 108)
    (312, Token (INFIXL _)) -> Just (Reduce 3 108)
    (312, Token (INFIXR _)) -> Just (Reduce 3 108)
    (312, Token (INFIX _)) -> Just (Reduce 3 108)
    (312, Token (RARROW _)) -> Just (Reduce 3 108)
    (312, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (312, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (312, Token (EXCL _)) -> Just (Reduce 3 108)
    (312, Token (QCONID _)) -> Just (Reduce 3 108)
    (312, Token (EXPORT _)) -> Just (Reduce 3 108)
    (312, Token (AS _)) -> Just (Reduce 3 108)
    (312, Token (QVARID _)) -> Just (Reduce 3 108)
    (312, Token (LARROW _)) -> Just (Reduce 3 108)
    (312, Token (THEN _)) -> Just (Reduce 3 108)
    (312, Token (ELSE _)) -> Just (Reduce 3 108)
    (312, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (312, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (312, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (312, Token (OF _)) -> Just (Reduce 3 108)
    (312, Token (INTEGER _)) -> Just (Reduce 3 108)
    (313, Token (WHERE _)) -> Just (Reduce 3 106)
    (313, Token (LBRACE _)) -> Just (Reduce 3 106)
    (313, Token (RBRACE _)) -> Just (Reduce 3 106)
    (313, Token (LPAREN _)) -> Just (Reduce 3 106)
    (313, Token (RPAREN _)) -> Just (Reduce 3 106)
    (313, Token (COMMA _)) -> Just (Reduce 3 106)
    (313, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (313, Token (EQUAL _)) -> Just (Reduce 3 106)
    (313, Token (DERIVING _)) -> Just (Reduce 3 106)
    (313, Token (DARROW _)) -> Just (Reduce 3 106)
    (313, Token (PIPE _)) -> Just (Reduce 3 106)
    (313, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (313, Token (MINUS _)) -> Just (Reduce 3 106)
    (313, Token (INFIXL _)) -> Just (Reduce 3 106)
    (313, Token (INFIXR _)) -> Just (Reduce 3 106)
    (313, Token (INFIX _)) -> Just (Reduce 3 106)
    (313, Token (RARROW _)) -> Just (Reduce 3 106)
    (313, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (313, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (313, Token (EXCL _)) -> Just (Reduce 3 106)
    (313, Token (QCONID _)) -> Just (Reduce 3 106)
    (313, Token (EXPORT _)) -> Just (Reduce 3 106)
    (313, Token (AS _)) -> Just (Reduce 3 106)
    (313, Token (QVARID _)) -> Just (Reduce 3 106)
    (313, Token (LARROW _)) -> Just (Reduce 3 106)
    (313, Token (THEN _)) -> Just (Reduce 3 106)
    (313, Token (ELSE _)) -> Just (Reduce 3 106)
    (313, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (313, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (313, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (313, Token (OF _)) -> Just (Reduce 3 106)
    (313, Token (INTEGER _)) -> Just (Reduce 3 106)
    (314, Token (WHERE _)) -> Just (Reduce 3 107)
    (314, Token (LBRACE _)) -> Just (Reduce 3 107)
    (314, Token (RBRACE _)) -> Just (Reduce 3 107)
    (314, Token (LPAREN _)) -> Just (Reduce 3 107)
    (314, Token (RPAREN _)) -> Just (Reduce 3 107)
    (314, Token (COMMA _)) -> Just (Reduce 3 107)
    (314, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (314, Token (EQUAL _)) -> Just (Reduce 3 107)
    (314, Token (DERIVING _)) -> Just (Reduce 3 107)
    (314, Token (DARROW _)) -> Just (Reduce 3 107)
    (314, Token (PIPE _)) -> Just (Reduce 3 107)
    (314, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (314, Token (MINUS _)) -> Just (Reduce 3 107)
    (314, Token (INFIXL _)) -> Just (Reduce 3 107)
    (314, Token (INFIXR _)) -> Just (Reduce 3 107)
    (314, Token (INFIX _)) -> Just (Reduce 3 107)
    (314, Token (RARROW _)) -> Just (Reduce 3 107)
    (314, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (314, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (314, Token (EXCL _)) -> Just (Reduce 3 107)
    (314, Token (QCONID _)) -> Just (Reduce 3 107)
    (314, Token (EXPORT _)) -> Just (Reduce 3 107)
    (314, Token (AS _)) -> Just (Reduce 3 107)
    (314, Token (QVARID _)) -> Just (Reduce 3 107)
    (314, Token (LARROW _)) -> Just (Reduce 3 107)
    (314, Token (THEN _)) -> Just (Reduce 3 107)
    (314, Token (ELSE _)) -> Just (Reduce 3 107)
    (314, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (314, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (314, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (314, Token (OF _)) -> Just (Reduce 3 107)
    (314, Token (INTEGER _)) -> Just (Reduce 3 107)
    (315, Token (RPAREN _)) -> Just (Shift 312)
    (315, Token (COMMA _)) -> Just (Shift 150)
    (316, Token (RBRACKET _)) -> Just (Shift 314)
    (317, Token (WHERE _)) -> Just (Reduce 2 109)
    (317, Token (LBRACE _)) -> Just (Reduce 2 109)
    (317, Token (RBRACE _)) -> Just (Reduce 2 109)
    (317, Token (LPAREN _)) -> Just (Reduce 2 109)
    (317, Token (RPAREN _)) -> Just (Reduce 2 109)
    (317, Token (COMMA _)) -> Just (Reduce 2 109)
    (317, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (317, Token (EQUAL _)) -> Just (Reduce 2 109)
    (317, Token (DERIVING _)) -> Just (Reduce 2 109)
    (317, Token (DARROW _)) -> Just (Reduce 2 109)
    (317, Token (PIPE _)) -> Just (Reduce 2 109)
    (317, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (317, Token (MINUS _)) -> Just (Reduce 2 109)
    (317, Token (INFIXL _)) -> Just (Reduce 2 109)
    (317, Token (INFIXR _)) -> Just (Reduce 2 109)
    (317, Token (INFIX _)) -> Just (Reduce 2 109)
    (317, Token (RARROW _)) -> Just (Reduce 2 109)
    (317, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (317, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (317, Token (EXCL _)) -> Just (Reduce 2 109)
    (317, Token (QCONID _)) -> Just (Reduce 2 109)
    (317, Token (EXPORT _)) -> Just (Reduce 2 109)
    (317, Token (AS _)) -> Just (Reduce 2 109)
    (317, Token (QVARID _)) -> Just (Reduce 2 109)
    (317, Token (LARROW _)) -> Just (Reduce 2 109)
    (317, Token (THEN _)) -> Just (Reduce 2 109)
    (317, Token (ELSE _)) -> Just (Reduce 2 109)
    (317, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (317, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (317, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (317, Token (OF _)) -> Just (Reduce 2 109)
    (317, Token (INTEGER _)) -> Just (Reduce 2 109)
    (318, Token (WHERE _)) -> Just (Reduce 1 104)
    (318, Token (LBRACE _)) -> Just (Reduce 1 104)
    (318, Token (RBRACE _)) -> Just (Reduce 1 104)
    (318, Token (LPAREN _)) -> Just (Reduce 1 104)
    (318, Token (RPAREN _)) -> Just (Reduce 1 104)
    (318, Token (COMMA _)) -> Just (Reduce 1 104)
    (318, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (318, Token (EQUAL _)) -> Just (Reduce 1 104)
    (318, Token (DERIVING _)) -> Just (Reduce 1 104)
    (318, Token (DARROW _)) -> Just (Reduce 1 104)
    (318, Token (PIPE _)) -> Just (Reduce 1 104)
    (318, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (318, Token (MINUS _)) -> Just (Reduce 1 104)
    (318, Token (INFIXL _)) -> Just (Reduce 1 104)
    (318, Token (INFIXR _)) -> Just (Reduce 1 104)
    (318, Token (INFIX _)) -> Just (Reduce 1 104)
    (318, Token (RARROW _)) -> Just (Reduce 1 104)
    (318, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (318, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (318, Token (EXCL _)) -> Just (Reduce 1 104)
    (318, Token (QCONID _)) -> Just (Reduce 1 104)
    (318, Token (EXPORT _)) -> Just (Reduce 1 104)
    (318, Token (AS _)) -> Just (Reduce 1 104)
    (318, Token (QVARID _)) -> Just (Reduce 1 104)
    (318, Token (LARROW _)) -> Just (Reduce 1 104)
    (318, Token (THEN _)) -> Just (Reduce 1 104)
    (318, Token (ELSE _)) -> Just (Reduce 1 104)
    (318, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (318, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (318, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (318, Token (OF _)) -> Just (Reduce 1 104)
    (318, Token (INTEGER _)) -> Just (Reduce 1 104)
    (319, Token (WHERE _)) -> Just (Reduce 1 105)
    (319, Token (LBRACE _)) -> Just (Reduce 1 105)
    (319, Token (RBRACE _)) -> Just (Reduce 1 105)
    (319, Token (LPAREN _)) -> Just (Reduce 1 105)
    (319, Token (RPAREN _)) -> Just (Reduce 1 105)
    (319, Token (COMMA _)) -> Just (Reduce 1 105)
    (319, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (319, Token (EQUAL _)) -> Just (Reduce 1 105)
    (319, Token (DERIVING _)) -> Just (Reduce 1 105)
    (319, Token (DARROW _)) -> Just (Reduce 1 105)
    (319, Token (PIPE _)) -> Just (Reduce 1 105)
    (319, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (319, Token (MINUS _)) -> Just (Reduce 1 105)
    (319, Token (INFIXL _)) -> Just (Reduce 1 105)
    (319, Token (INFIXR _)) -> Just (Reduce 1 105)
    (319, Token (INFIX _)) -> Just (Reduce 1 105)
    (319, Token (RARROW _)) -> Just (Reduce 1 105)
    (319, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (319, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (319, Token (EXCL _)) -> Just (Reduce 1 105)
    (319, Token (QCONID _)) -> Just (Reduce 1 105)
    (319, Token (EXPORT _)) -> Just (Reduce 1 105)
    (319, Token (AS _)) -> Just (Reduce 1 105)
    (319, Token (QVARID _)) -> Just (Reduce 1 105)
    (319, Token (LARROW _)) -> Just (Reduce 1 105)
    (319, Token (THEN _)) -> Just (Reduce 1 105)
    (319, Token (ELSE _)) -> Just (Reduce 1 105)
    (319, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (319, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (319, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (319, Token (OF _)) -> Just (Reduce 1 105)
    (319, Token (INTEGER _)) -> Just (Reduce 1 105)
    (320, Token (RPAREN _)) -> Just (Shift 313)
    (321, Token (WHERE _)) -> Just (Reduce 2 113)
    (321, Token (LBRACE _)) -> Just (Reduce 2 113)
    (321, Token (RBRACE _)) -> Just (Reduce 2 113)
    (321, Token (LPAREN _)) -> Just (Reduce 2 113)
    (321, Token (RPAREN _)) -> Just (Reduce 2 113)
    (321, Token (COMMA _)) -> Just (Reduce 2 113)
    (321, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (321, Token (EQUAL _)) -> Just (Reduce 2 113)
    (321, Token (DERIVING _)) -> Just (Reduce 2 113)
    (321, Token (DARROW _)) -> Just (Reduce 2 113)
    (321, Token (PIPE _)) -> Just (Reduce 2 113)
    (321, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (321, Token (MINUS _)) -> Just (Reduce 2 113)
    (321, Token (INFIXL _)) -> Just (Reduce 2 113)
    (321, Token (INFIXR _)) -> Just (Reduce 2 113)
    (321, Token (INFIX _)) -> Just (Reduce 2 113)
    (321, Token (RARROW _)) -> Just (Reduce 2 113)
    (321, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (321, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (321, Token (EXCL _)) -> Just (Reduce 2 113)
    (321, Token (QCONID _)) -> Just (Reduce 2 113)
    (321, Token (EXPORT _)) -> Just (Reduce 2 113)
    (321, Token (AS _)) -> Just (Reduce 2 113)
    (321, Token (QVARID _)) -> Just (Reduce 2 113)
    (321, Token (LARROW _)) -> Just (Reduce 2 113)
    (321, Token (THEN _)) -> Just (Reduce 2 113)
    (321, Token (ELSE _)) -> Just (Reduce 2 113)
    (321, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (321, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (321, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (321, Token (OF _)) -> Just (Reduce 2 113)
    (321, Token (INTEGER _)) -> Just (Reduce 2 113)
    (322, Token (WHERE _)) -> Just (Reduce 3 115)
    (322, Token (LBRACE _)) -> Just (Reduce 3 115)
    (322, Token (RBRACE _)) -> Just (Reduce 3 115)
    (322, Token (LPAREN _)) -> Just (Reduce 3 115)
    (322, Token (RPAREN _)) -> Just (Reduce 3 115)
    (322, Token (COMMA _)) -> Just (Reduce 3 115)
    (322, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (322, Token (EQUAL _)) -> Just (Reduce 3 115)
    (322, Token (DERIVING _)) -> Just (Reduce 3 115)
    (322, Token (DARROW _)) -> Just (Reduce 3 115)
    (322, Token (PIPE _)) -> Just (Reduce 3 115)
    (322, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (322, Token (MINUS _)) -> Just (Reduce 3 115)
    (322, Token (INFIXL _)) -> Just (Reduce 3 115)
    (322, Token (INFIXR _)) -> Just (Reduce 3 115)
    (322, Token (INFIX _)) -> Just (Reduce 3 115)
    (322, Token (RARROW _)) -> Just (Reduce 3 115)
    (322, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (322, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (322, Token (EXCL _)) -> Just (Reduce 3 115)
    (322, Token (QCONID _)) -> Just (Reduce 3 115)
    (322, Token (EXPORT _)) -> Just (Reduce 3 115)
    (322, Token (AS _)) -> Just (Reduce 3 115)
    (322, Token (QVARID _)) -> Just (Reduce 3 115)
    (322, Token (LARROW _)) -> Just (Reduce 3 115)
    (322, Token (THEN _)) -> Just (Reduce 3 115)
    (322, Token (ELSE _)) -> Just (Reduce 3 115)
    (322, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (322, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (322, Token (OF _)) -> Just (Reduce 3 115)
    (322, Token (INTEGER _)) -> Just (Reduce 3 115)
    (323, Token (WHERE _)) -> Just (Reduce 3 116)
    (323, Token (LBRACE _)) -> Just (Reduce 3 116)
    (323, Token (RBRACE _)) -> Just (Reduce 3 116)
    (323, Token (LPAREN _)) -> Just (Reduce 3 116)
    (323, Token (RPAREN _)) -> Just (Reduce 3 116)
    (323, Token (COMMA _)) -> Just (Reduce 3 116)
    (323, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (323, Token (EQUAL _)) -> Just (Reduce 3 116)
    (323, Token (DERIVING _)) -> Just (Reduce 3 116)
    (323, Token (DARROW _)) -> Just (Reduce 3 116)
    (323, Token (PIPE _)) -> Just (Reduce 3 116)
    (323, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (323, Token (MINUS _)) -> Just (Reduce 3 116)
    (323, Token (INFIXL _)) -> Just (Reduce 3 116)
    (323, Token (INFIXR _)) -> Just (Reduce 3 116)
    (323, Token (INFIX _)) -> Just (Reduce 3 116)
    (323, Token (RARROW _)) -> Just (Reduce 3 116)
    (323, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (323, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (323, Token (EXCL _)) -> Just (Reduce 3 116)
    (323, Token (QCONID _)) -> Just (Reduce 3 116)
    (323, Token (EXPORT _)) -> Just (Reduce 3 116)
    (323, Token (AS _)) -> Just (Reduce 3 116)
    (323, Token (QVARID _)) -> Just (Reduce 3 116)
    (323, Token (LARROW _)) -> Just (Reduce 3 116)
    (323, Token (THEN _)) -> Just (Reduce 3 116)
    (323, Token (ELSE _)) -> Just (Reduce 3 116)
    (323, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (323, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (323, Token (OF _)) -> Just (Reduce 3 116)
    (323, Token (INTEGER _)) -> Just (Reduce 3 116)
    (324, Token (RPAREN _)) -> Just (Shift 322)
    (325, Token (WHERE _)) -> Just (Reduce 2 114)
    (325, Token (LBRACE _)) -> Just (Reduce 2 114)
    (325, Token (RBRACE _)) -> Just (Reduce 2 114)
    (325, Token (LPAREN _)) -> Just (Reduce 2 114)
    (325, Token (RPAREN _)) -> Just (Reduce 2 114)
    (325, Token (COMMA _)) -> Just (Reduce 2 114)
    (325, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (325, Token (EQUAL _)) -> Just (Reduce 2 114)
    (325, Token (DERIVING _)) -> Just (Reduce 2 114)
    (325, Token (DARROW _)) -> Just (Reduce 2 114)
    (325, Token (PIPE _)) -> Just (Reduce 2 114)
    (325, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (325, Token (MINUS _)) -> Just (Reduce 2 114)
    (325, Token (INFIXL _)) -> Just (Reduce 2 114)
    (325, Token (INFIXR _)) -> Just (Reduce 2 114)
    (325, Token (INFIX _)) -> Just (Reduce 2 114)
    (325, Token (RARROW _)) -> Just (Reduce 2 114)
    (325, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (325, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (325, Token (EXCL _)) -> Just (Reduce 2 114)
    (325, Token (QCONID _)) -> Just (Reduce 2 114)
    (325, Token (EXPORT _)) -> Just (Reduce 2 114)
    (325, Token (AS _)) -> Just (Reduce 2 114)
    (325, Token (QVARID _)) -> Just (Reduce 2 114)
    (325, Token (LARROW _)) -> Just (Reduce 2 114)
    (325, Token (THEN _)) -> Just (Reduce 2 114)
    (325, Token (ELSE _)) -> Just (Reduce 2 114)
    (325, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (325, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (325, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (325, Token (OF _)) -> Just (Reduce 2 114)
    (325, Token (INTEGER _)) -> Just (Reduce 2 114)
    (326, Token (WHERE _)) -> Just (Reduce 1 112)
    (326, Token (LBRACE _)) -> Just (Reduce 1 112)
    (326, Token (RBRACE _)) -> Just (Reduce 1 112)
    (326, Token (LPAREN _)) -> Just (Reduce 1 112)
    (326, Token (RPAREN _)) -> Just (Reduce 1 112)
    (326, Token (COMMA _)) -> Just (Reduce 1 112)
    (326, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (326, Token (EQUAL _)) -> Just (Reduce 1 112)
    (326, Token (DERIVING _)) -> Just (Reduce 1 112)
    (326, Token (DARROW _)) -> Just (Reduce 1 112)
    (326, Token (PIPE _)) -> Just (Reduce 1 112)
    (326, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (326, Token (MINUS _)) -> Just (Reduce 1 112)
    (326, Token (INFIXL _)) -> Just (Reduce 1 112)
    (326, Token (INFIXR _)) -> Just (Reduce 1 112)
    (326, Token (INFIX _)) -> Just (Reduce 1 112)
    (326, Token (RARROW _)) -> Just (Reduce 1 112)
    (326, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (326, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (326, Token (EXCL _)) -> Just (Reduce 1 112)
    (326, Token (QCONID _)) -> Just (Reduce 1 112)
    (326, Token (EXPORT _)) -> Just (Reduce 1 112)
    (326, Token (AS _)) -> Just (Reduce 1 112)
    (326, Token (QVARID _)) -> Just (Reduce 1 112)
    (326, Token (LARROW _)) -> Just (Reduce 1 112)
    (326, Token (THEN _)) -> Just (Reduce 1 112)
    (326, Token (ELSE _)) -> Just (Reduce 1 112)
    (326, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (326, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (326, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (326, Token (OF _)) -> Just (Reduce 1 112)
    (326, Token (INTEGER _)) -> Just (Reduce 1 112)
    (327, Token (LBRACE _)) -> Just (Shift 92)
    (327, Token (RBRACE _)) -> Just (Reduce 1 112)
    (327, Token (LPAREN _)) -> Just (Reduce 1 112)
    (327, Token (RPAREN _)) -> Just (Reduce 1 112)
    (327, Token (COMMA _)) -> Just (Reduce 1 112)
    (327, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (327, Token (DERIVING _)) -> Just (Reduce 1 112)
    (327, Token (PIPE _)) -> Just (Reduce 1 112)
    (327, Token (RARROW _)) -> Just (Reduce 1 112)
    (327, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (327, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (327, Token (EXCL _)) -> Just (Reduce 1 112)
    (327, Token (QCONID _)) -> Just (Reduce 1 112)
    (327, Token (EXPORT _)) -> Just (Reduce 1 112)
    (327, Token (AS _)) -> Just (Reduce 1 112)
    (327, Token (QVARID _)) -> Just (Reduce 1 112)
    (327, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (327, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (328, Token (RPAREN _)) -> Just (Shift 323)
    (329, Token (WHERE _)) -> Just (Reduce 1 221)
    (329, Token (LBRACE _)) -> Just (Reduce 1 221)
    (329, Token (RBRACE _)) -> Just (Reduce 1 221)
    (329, Token (LPAREN _)) -> Just (Reduce 1 221)
    (329, Token (RPAREN _)) -> Just (Reduce 1 221)
    (329, Token (COMMA _)) -> Just (Reduce 1 221)
    (329, Token (SEMICOLON _)) -> Just (Reduce 1 221)
    (329, Token (EQUAL _)) -> Just (Reduce 1 221)
    (329, Token (DERIVING _)) -> Just (Reduce 1 221)
    (329, Token (DARROW _)) -> Just (Reduce 1 221)
    (329, Token (PIPE _)) -> Just (Reduce 1 221)
    (329, Token (COLON_COLON _)) -> Just (Reduce 1 221)
    (329, Token (MINUS _)) -> Just (Reduce 1 221)
    (329, Token (INFIXL _)) -> Just (Reduce 1 221)
    (329, Token (INFIXR _)) -> Just (Reduce 1 221)
    (329, Token (INFIX _)) -> Just (Reduce 1 221)
    (329, Token (RARROW _)) -> Just (Reduce 1 221)
    (329, Token (LBRACKET _)) -> Just (Reduce 1 221)
    (329, Token (RBRACKET _)) -> Just (Reduce 1 221)
    (329, Token (EXCL _)) -> Just (Reduce 1 221)
    (329, Token (QCONID _)) -> Just (Reduce 1 221)
    (329, Token (EXPORT _)) -> Just (Reduce 1 221)
    (329, Token (AS _)) -> Just (Reduce 1 221)
    (329, Token (QVARID _)) -> Just (Reduce 1 221)
    (329, Token (LARROW _)) -> Just (Reduce 1 221)
    (329, Token (THEN _)) -> Just (Reduce 1 221)
    (329, Token (ELSE _)) -> Just (Reduce 1 221)
    (329, Token (QVARSYM _)) -> Just (Reduce 1 221)
    (329, Token (BACKQUOTE _)) -> Just (Reduce 1 221)
    (329, Token (QCONSYM _)) -> Just (Reduce 1 221)
    (329, Token (OF _)) -> Just (Reduce 1 221)
    (329, Token (INTEGER _)) -> Just (Reduce 1 221)
    (330, Token (WHERE _)) -> Just (Reduce 1 220)
    (330, Token (LBRACE _)) -> Just (Reduce 1 220)
    (330, Token (RBRACE _)) -> Just (Reduce 1 220)
    (330, Token (LPAREN _)) -> Just (Reduce 1 220)
    (330, Token (RPAREN _)) -> Just (Reduce 1 220)
    (330, Token (COMMA _)) -> Just (Reduce 1 220)
    (330, Token (SEMICOLON _)) -> Just (Reduce 1 220)
    (330, Token (EQUAL _)) -> Just (Reduce 1 220)
    (330, Token (DERIVING _)) -> Just (Reduce 1 220)
    (330, Token (DARROW _)) -> Just (Reduce 1 220)
    (330, Token (PIPE _)) -> Just (Reduce 1 220)
    (330, Token (COLON_COLON _)) -> Just (Reduce 1 220)
    (330, Token (MINUS _)) -> Just (Reduce 1 220)
    (330, Token (INFIXL _)) -> Just (Reduce 1 220)
    (330, Token (INFIXR _)) -> Just (Reduce 1 220)
    (330, Token (INFIX _)) -> Just (Reduce 1 220)
    (330, Token (RARROW _)) -> Just (Reduce 1 220)
    (330, Token (LBRACKET _)) -> Just (Reduce 1 220)
    (330, Token (RBRACKET _)) -> Just (Reduce 1 220)
    (330, Token (EXCL _)) -> Just (Reduce 1 220)
    (330, Token (QCONID _)) -> Just (Reduce 1 220)
    (330, Token (EXPORT _)) -> Just (Reduce 1 220)
    (330, Token (AS _)) -> Just (Reduce 1 220)
    (330, Token (QVARID _)) -> Just (Reduce 1 220)
    (330, Token (LARROW _)) -> Just (Reduce 1 220)
    (330, Token (THEN _)) -> Just (Reduce 1 220)
    (330, Token (ELSE _)) -> Just (Reduce 1 220)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 220)
    (330, Token (BACKQUOTE _)) -> Just (Reduce 1 220)
    (330, Token (QCONSYM _)) -> Just (Reduce 1 220)
    (330, Token (OF _)) -> Just (Reduce 1 220)
    (330, Token (INTEGER _)) -> Just (Reduce 1 220)
    (331, Token (WHERE _)) -> Just (Reduce 1 222)
    (331, Token (LBRACE _)) -> Just (Reduce 1 222)
    (331, Token (RBRACE _)) -> Just (Reduce 1 222)
    (331, Token (LPAREN _)) -> Just (Reduce 1 222)
    (331, Token (RPAREN _)) -> Just (Reduce 1 222)
    (331, Token (COMMA _)) -> Just (Reduce 1 222)
    (331, Token (SEMICOLON _)) -> Just (Reduce 1 222)
    (331, Token (EQUAL _)) -> Just (Reduce 1 222)
    (331, Token (DERIVING _)) -> Just (Reduce 1 222)
    (331, Token (DARROW _)) -> Just (Reduce 1 222)
    (331, Token (PIPE _)) -> Just (Reduce 1 222)
    (331, Token (COLON_COLON _)) -> Just (Reduce 1 222)
    (331, Token (MINUS _)) -> Just (Reduce 1 222)
    (331, Token (INFIXL _)) -> Just (Reduce 1 222)
    (331, Token (INFIXR _)) -> Just (Reduce 1 222)
    (331, Token (INFIX _)) -> Just (Reduce 1 222)
    (331, Token (RARROW _)) -> Just (Reduce 1 222)
    (331, Token (LBRACKET _)) -> Just (Reduce 1 222)
    (331, Token (RBRACKET _)) -> Just (Reduce 1 222)
    (331, Token (EXCL _)) -> Just (Reduce 1 222)
    (331, Token (QCONID _)) -> Just (Reduce 1 222)
    (331, Token (EXPORT _)) -> Just (Reduce 1 222)
    (331, Token (AS _)) -> Just (Reduce 1 222)
    (331, Token (QVARID _)) -> Just (Reduce 1 222)
    (331, Token (LARROW _)) -> Just (Reduce 1 222)
    (331, Token (THEN _)) -> Just (Reduce 1 222)
    (331, Token (ELSE _)) -> Just (Reduce 1 222)
    (331, Token (QVARSYM _)) -> Just (Reduce 1 222)
    (331, Token (BACKQUOTE _)) -> Just (Reduce 1 222)
    (331, Token (QCONSYM _)) -> Just (Reduce 1 222)
    (331, Token (OF _)) -> Just (Reduce 1 222)
    (331, Token (INTEGER _)) -> Just (Reduce 1 222)
    (332, Token (RPAREN _)) -> Just (Reduce 3 110)
    (332, Token (COMMA _)) -> Just (Shift 150)
    (333, Token (RPAREN _)) -> Just (Reduce 3 111)
    (334, Token (RPAREN _)) -> Just (Reduce 1 117)
    (334, Token (COMMA _)) -> Just (Shift 334)
    (335, Token (RPAREN _)) -> Just (Reduce 2 118)
    (336, Token (RBRACE _)) -> Just (Reduce 3 122)
    (336, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (336, Token (DERIVING _)) -> Just (Reduce 3 122)
    (337, Token (RBRACE _)) -> Just (Reduce 1 121)
    (337, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (337, Token (DERIVING _)) -> Just (Reduce 1 121)
    (337, Token (PIPE _)) -> Just (Shift 126)
    (338, Token (RBRACE _)) -> Just (Reduce 3 125)
    (338, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (338, Token (DERIVING _)) -> Just (Reduce 3 125)
    (338, Token (PIPE _)) -> Just (Reduce 3 125)
    (339, Token (RBRACE _)) -> Just (Reduce 4 126)
    (339, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (339, Token (DERIVING _)) -> Just (Reduce 4 126)
    (339, Token (PIPE _)) -> Just (Reduce 4 126)
    (340, Token (RBRACE _)) -> Just (Shift 339)
    (341, Token (BACKQUOTE _)) -> Just (Shift 344)
    (342, Token (QCONID _)) -> Just (Shift 341)
    (342, Token (EXPORT _)) -> Just (Shift 426)
    (342, Token (AS _)) -> Just (Shift 427)
    (342, Token (QVARID _)) -> Just (Shift 428)
    (343, Token (QCONID _)) -> Just (Shift 341)
    (344, Token (RBRACE _)) -> Just (Reduce 3 213)
    (344, Token (LPAREN _)) -> Just (Reduce 3 213)
    (344, Token (RPAREN _)) -> Just (Reduce 3 213)
    (344, Token (COMMA _)) -> Just (Reduce 3 213)
    (344, Token (SEMICOLON _)) -> Just (Reduce 3 213)
    (344, Token (MINUS _)) -> Just (Reduce 3 213)
    (344, Token (RARROW _)) -> Just (Reduce 3 213)
    (344, Token (LBRACKET _)) -> Just (Reduce 3 213)
    (344, Token (RBRACKET _)) -> Just (Reduce 3 213)
    (344, Token (EXCL _)) -> Just (Reduce 3 213)
    (344, Token (QCONID _)) -> Just (Reduce 3 213)
    (344, Token (EXPORT _)) -> Just (Reduce 3 213)
    (344, Token (AS _)) -> Just (Reduce 3 213)
    (344, Token (QVARID _)) -> Just (Reduce 3 213)
    (344, Token (QVARSYM _)) -> Just (Reduce 3 213)
    (344, Token (BACKQUOTE _)) -> Just (Reduce 3 213)
    (344, Token (QCONSYM _)) -> Just (Reduce 3 213)
    (345, Token (RBRACE _)) -> Just (Reduce 1 212)
    (345, Token (LPAREN _)) -> Just (Reduce 1 212)
    (345, Token (RPAREN _)) -> Just (Reduce 1 212)
    (345, Token (COMMA _)) -> Just (Reduce 1 212)
    (345, Token (SEMICOLON _)) -> Just (Reduce 1 212)
    (345, Token (MINUS _)) -> Just (Reduce 1 212)
    (345, Token (RARROW _)) -> Just (Reduce 1 212)
    (345, Token (LBRACKET _)) -> Just (Reduce 1 212)
    (345, Token (RBRACKET _)) -> Just (Reduce 1 212)
    (345, Token (EXCL _)) -> Just (Reduce 1 212)
    (345, Token (QCONID _)) -> Just (Reduce 1 212)
    (345, Token (EXPORT _)) -> Just (Reduce 1 212)
    (345, Token (AS _)) -> Just (Reduce 1 212)
    (345, Token (QVARID _)) -> Just (Reduce 1 212)
    (345, Token (QVARSYM _)) -> Just (Reduce 1 212)
    (345, Token (BACKQUOTE _)) -> Just (Reduce 1 212)
    (345, Token (QCONSYM _)) -> Just (Reduce 1 212)
    (346, Token (RBRACE _)) -> Just (Reduce 3 130)
    (347, Token (RBRACE _)) -> Just (Reduce 1 129)
    (347, Token (COMMA _)) -> Just (Shift 93)
    (348, Token (RBRACE _)) -> Just (Reduce 3 131)
    (348, Token (COMMA _)) -> Just (Reduce 3 131)
    (349, Token (COLON_COLON _)) -> Just (Shift 138)
    (350, Token (EXPORT _)) -> Just (Reduce 1 139)
    (350, Token (AS _)) -> Just (Reduce 1 139)
    (350, Token (QVARID _)) -> Just (Reduce 1 139)
    (350, Token (STRING _)) -> Just (Reduce 1 139)
    (351, Token (EXPORT _)) -> Just (Reduce 1 138)
    (351, Token (AS _)) -> Just (Reduce 1 138)
    (351, Token (QVARID _)) -> Just (Reduce 1 138)
    (351, Token (STRING _)) -> Just (Reduce 1 138)
    (352, Token (EXPORT _)) -> Just (Reduce 1 140)
    (352, Token (AS _)) -> Just (Reduce 1 140)
    (352, Token (QVARID _)) -> Just (Reduce 1 140)
    (352, Token (STRING _)) -> Just (Reduce 1 140)
    (353, Token (LPAREN _)) -> Just (Reduce 1 141)
    (353, Token (MINUS _)) -> Just (Reduce 1 141)
    (353, Token (EXPORT _)) -> Just (Reduce 1 141)
    (353, Token (AS _)) -> Just (Reduce 1 141)
    (353, Token (QVARID _)) -> Just (Reduce 1 141)
    (353, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (354, Token (STRING _)) -> Just (Reduce 1 144)
    (355, Token (STRING _)) -> Just (Reduce 1 143)
    (356, Token (STRING _)) -> Just (Reduce 1 145)
    (357, Token (LPAREN _)) -> Just (Reduce 1 142)
    (357, Token (MINUS _)) -> Just (Reduce 1 142)
    (357, Token (EXPORT _)) -> Just (Reduce 1 142)
    (357, Token (AS _)) -> Just (Reduce 1 142)
    (357, Token (QVARID _)) -> Just (Reduce 1 142)
    (357, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (358, Token (EQUAL _)) -> Just (Reduce 3 149)
    (359, Token (COMMA _)) -> Just (Shift 63)
    (359, Token (EQUAL _)) -> Just (Reduce 1 148)
    (360, Token (COMMA _)) -> Just (Reduce 2 151)
    (360, Token (EQUAL _)) -> Just (Reduce 2 151)
    (360, Token (IN _)) -> Just (Shift 36)
    (361, Token (COMMA _)) -> Just (Reduce 3 150)
    (361, Token (EQUAL _)) -> Just (Reduce 3 150)
    (362, Token (COMMA _)) -> Just (Reduce 1 152)
    (362, Token (EQUAL _)) -> Just (Reduce 1 152)
    (362, Token (LARROW _)) -> Just (Shift 67)
    (363, Token (BACKQUOTE _)) -> Just (Shift 39)
    (364, Token (BACKQUOTE _)) -> Just (Shift 40)
    (365, Token (BACKQUOTE _)) -> Just (Shift 41)
    (366, Token (BACKQUOTE _)) -> Just (Shift 42)
    (367, Token (QCONID _)) -> Just (Shift 363)
    (367, Token (EXPORT _)) -> Just (Shift 364)
    (367, Token (AS _)) -> Just (Shift 365)
    (367, Token (QVARID _)) -> Just (Shift 366)
    (368, Token (WHERE _)) -> Just (Reduce 5 165)
    (368, Token (RBRACE _)) -> Just (Reduce 5 165)
    (368, Token (RPAREN _)) -> Just (Reduce 5 165)
    (368, Token (COMMA _)) -> Just (Reduce 5 165)
    (368, Token (SEMICOLON _)) -> Just (Reduce 5 165)
    (368, Token (EQUAL _)) -> Just (Reduce 5 165)
    (368, Token (PIPE _)) -> Just (Reduce 5 165)
    (368, Token (LARROW _)) -> Just (Reduce 5 165)
    (368, Token (THEN _)) -> Just (Reduce 5 165)
    (368, Token (ELSE _)) -> Just (Reduce 5 165)
    (368, Token (OF _)) -> Just (Reduce 5 165)
    (369, Token (WHERE _)) -> Just (Reduce 3 164)
    (369, Token (RBRACE _)) -> Just (Reduce 3 164)
    (369, Token (RPAREN _)) -> Just (Reduce 3 164)
    (369, Token (COMMA _)) -> Just (Reduce 3 164)
    (369, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (369, Token (EQUAL _)) -> Just (Reduce 3 164)
    (369, Token (PIPE _)) -> Just (Reduce 3 164)
    (369, Token (LARROW _)) -> Just (Reduce 3 164)
    (369, Token (THEN _)) -> Just (Reduce 3 164)
    (369, Token (ELSE _)) -> Just (Reduce 3 164)
    (369, Token (OF _)) -> Just (Reduce 3 164)
    (370, Token (IN _)) -> Just (Shift 36)
    (371, Token (WHERE _)) -> Just (Reduce 3 157)
    (371, Token (RBRACE _)) -> Just (Reduce 3 157)
    (371, Token (RPAREN _)) -> Just (Reduce 3 157)
    (371, Token (COMMA _)) -> Just (Reduce 3 157)
    (371, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (371, Token (EQUAL _)) -> Just (Reduce 3 157)
    (371, Token (PIPE _)) -> Just (Reduce 3 157)
    (371, Token (LARROW _)) -> Just (Reduce 3 157)
    (371, Token (THEN _)) -> Just (Reduce 3 157)
    (371, Token (ELSE _)) -> Just (Reduce 3 157)
    (371, Token (OF _)) -> Just (Reduce 3 157)
    (372, Token (WHERE _)) -> Just (Reduce 4 154)
    (372, Token (RBRACE _)) -> Just (Reduce 4 154)
    (372, Token (RPAREN _)) -> Just (Reduce 4 154)
    (372, Token (COMMA _)) -> Just (Reduce 4 154)
    (372, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (372, Token (EQUAL _)) -> Just (Reduce 4 154)
    (372, Token (PIPE _)) -> Just (Reduce 4 154)
    (372, Token (LARROW _)) -> Just (Reduce 4 154)
    (372, Token (THEN _)) -> Just (Reduce 4 154)
    (372, Token (ELSE _)) -> Just (Reduce 4 154)
    (372, Token (OF _)) -> Just (Reduce 4 154)
    (373, Token (WHERE _)) -> Just (Reduce 4 155)
    (373, Token (RBRACE _)) -> Just (Reduce 4 155)
    (373, Token (RPAREN _)) -> Just (Reduce 4 155)
    (373, Token (COMMA _)) -> Just (Reduce 4 155)
    (373, Token (SEMICOLON _)) -> Just (Reduce 4 155)
    (373, Token (EQUAL _)) -> Just (Reduce 4 155)
    (373, Token (PIPE _)) -> Just (Reduce 4 155)
    (373, Token (LARROW _)) -> Just (Reduce 4 155)
    (373, Token (THEN _)) -> Just (Reduce 4 155)
    (373, Token (ELSE _)) -> Just (Reduce 4 155)
    (373, Token (OF _)) -> Just (Reduce 4 155)
    (374, Token (SEMICOLON _)) -> Just (Shift 386)
    (374, Token (THEN _)) -> Just (Reduce 0 227)
    (375, Token (SEMICOLON _)) -> Just (Shift 386)
    (375, Token (ELSE _)) -> Just (Reduce 0 227)
    (376, Token (WHERE _)) -> Just (Reduce 8 156)
    (376, Token (RBRACE _)) -> Just (Reduce 8 156)
    (376, Token (RPAREN _)) -> Just (Reduce 8 156)
    (376, Token (COMMA _)) -> Just (Reduce 8 156)
    (376, Token (SEMICOLON _)) -> Just (Reduce 8 156)
    (376, Token (EQUAL _)) -> Just (Reduce 8 156)
    (376, Token (PIPE _)) -> Just (Reduce 8 156)
    (376, Token (LARROW _)) -> Just (Reduce 8 156)
    (376, Token (THEN _)) -> Just (Reduce 8 156)
    (376, Token (ELSE _)) -> Just (Reduce 8 156)
    (376, Token (OF _)) -> Just (Reduce 8 156)
    (377, Token (WHERE _)) -> Just (Reduce 3 158)
    (377, Token (RBRACE _)) -> Just (Reduce 3 158)
    (377, Token (RPAREN _)) -> Just (Reduce 3 158)
    (377, Token (COMMA _)) -> Just (Reduce 3 158)
    (377, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (377, Token (EQUAL _)) -> Just (Reduce 3 158)
    (377, Token (PIPE _)) -> Just (Reduce 3 158)
    (377, Token (LARROW _)) -> Just (Reduce 3 158)
    (377, Token (THEN _)) -> Just (Reduce 3 158)
    (377, Token (ELSE _)) -> Just (Reduce 3 158)
    (377, Token (OF _)) -> Just (Reduce 3 158)
    (378, Token (WHERE _)) -> Just (Reduce 5 163)
    (378, Token (RBRACE _)) -> Just (Reduce 5 163)
    (378, Token (RPAREN _)) -> Just (Reduce 5 163)
    (378, Token (COMMA _)) -> Just (Reduce 5 163)
    (378, Token (SEMICOLON _)) -> Just (Reduce 5 163)
    (378, Token (EQUAL _)) -> Just (Reduce 5 163)
    (378, Token (PIPE _)) -> Just (Reduce 5 163)
    (378, Token (LARROW _)) -> Just (Reduce 5 163)
    (378, Token (THEN _)) -> Just (Reduce 5 163)
    (378, Token (ELSE _)) -> Just (Reduce 5 163)
    (378, Token (OF _)) -> Just (Reduce 5 163)
    (379, Token (WHERE _)) -> Just (Reduce 5 160)
    (379, Token (RBRACE _)) -> Just (Reduce 5 160)
    (379, Token (RPAREN _)) -> Just (Reduce 5 160)
    (379, Token (COMMA _)) -> Just (Reduce 5 160)
    (379, Token (SEMICOLON _)) -> Just (Reduce 5 160)
    (379, Token (EQUAL _)) -> Just (Reduce 5 160)
    (379, Token (PIPE _)) -> Just (Reduce 5 160)
    (379, Token (LARROW _)) -> Just (Reduce 5 160)
    (379, Token (THEN _)) -> Just (Reduce 5 160)
    (379, Token (ELSE _)) -> Just (Reduce 5 160)
    (379, Token (OF _)) -> Just (Reduce 5 160)
    (380, Token (WHERE _)) -> Just (Reduce 5 159)
    (380, Token (RBRACE _)) -> Just (Reduce 5 159)
    (380, Token (RPAREN _)) -> Just (Reduce 5 159)
    (380, Token (COMMA _)) -> Just (Reduce 5 159)
    (380, Token (SEMICOLON _)) -> Just (Reduce 5 159)
    (380, Token (EQUAL _)) -> Just (Reduce 5 159)
    (380, Token (PIPE _)) -> Just (Reduce 5 159)
    (380, Token (LARROW _)) -> Just (Reduce 5 159)
    (380, Token (THEN _)) -> Just (Reduce 5 159)
    (380, Token (ELSE _)) -> Just (Reduce 5 159)
    (380, Token (OF _)) -> Just (Reduce 5 159)
    (381, Token (WHERE _)) -> Just (Reduce 5 161)
    (381, Token (RBRACE _)) -> Just (Reduce 5 161)
    (381, Token (RPAREN _)) -> Just (Reduce 5 161)
    (381, Token (COMMA _)) -> Just (Reduce 5 161)
    (381, Token (SEMICOLON _)) -> Just (Reduce 5 161)
    (381, Token (EQUAL _)) -> Just (Reduce 5 161)
    (381, Token (PIPE _)) -> Just (Reduce 5 161)
    (381, Token (LARROW _)) -> Just (Reduce 5 161)
    (381, Token (THEN _)) -> Just (Reduce 5 161)
    (381, Token (ELSE _)) -> Just (Reduce 5 161)
    (381, Token (OF _)) -> Just (Reduce 5 161)
    (382, Token (WHERE _)) -> Just (Reduce 3 162)
    (382, Token (RBRACE _)) -> Just (Reduce 3 162)
    (382, Token (RPAREN _)) -> Just (Reduce 3 162)
    (382, Token (COMMA _)) -> Just (Reduce 3 162)
    (382, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (382, Token (EQUAL _)) -> Just (Reduce 3 162)
    (382, Token (PIPE _)) -> Just (Reduce 3 162)
    (382, Token (LARROW _)) -> Just (Reduce 3 162)
    (382, Token (THEN _)) -> Just (Reduce 3 162)
    (382, Token (ELSE _)) -> Just (Reduce 3 162)
    (382, Token (OF _)) -> Just (Reduce 3 162)
    (383, Token (THEN _)) -> Just (Shift 65)
    (384, Token (ELSE _)) -> Just (Shift 37)
    (385, Token (WHERE _)) -> Just (Reduce 1 166)
    (385, Token (RBRACE _)) -> Just (Reduce 1 166)
    (385, Token (RPAREN _)) -> Just (Reduce 1 166)
    (385, Token (COMMA _)) -> Just (Reduce 1 166)
    (385, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (385, Token (EQUAL _)) -> Just (Reduce 1 166)
    (385, Token (PIPE _)) -> Just (Reduce 1 166)
    (385, Token (COLON_COLON _)) -> Just (Shift 116)
    (385, Token (MINUS _)) -> Just (Shift 34)
    (385, Token (LARROW _)) -> Just (Reduce 1 166)
    (385, Token (THEN _)) -> Just (Reduce 1 166)
    (385, Token (ELSE _)) -> Just (Reduce 1 166)
    (385, Token (QVARSYM _)) -> Just (Shift 38)
    (385, Token (BACKQUOTE _)) -> Just (Shift 367)
    (385, Token (QCONSYM _)) -> Just (Shift 43)
    (385, Token (OF _)) -> Just (Reduce 1 166)
    (386, Token (THEN _)) -> Just (Reduce 1 228)
    (386, Token (ELSE _)) -> Just (Reduce 1 228)
    (387, Token (RBRACE _)) -> Just (Reduce 0 194)
    (388, Token (WHERE _)) -> Just (Reduce 6 168)
    (388, Token (RBRACE _)) -> Just (Reduce 6 168)
    (388, Token (RPAREN _)) -> Just (Reduce 6 168)
    (388, Token (COMMA _)) -> Just (Reduce 6 168)
    (388, Token (SEMICOLON _)) -> Just (Reduce 6 168)
    (388, Token (EQUAL _)) -> Just (Reduce 6 168)
    (388, Token (PIPE _)) -> Just (Reduce 6 168)
    (388, Token (COLON_COLON _)) -> Just (Reduce 6 168)
    (388, Token (MINUS _)) -> Just (Reduce 6 168)
    (388, Token (LARROW _)) -> Just (Reduce 6 168)
    (388, Token (THEN _)) -> Just (Reduce 6 168)
    (388, Token (ELSE _)) -> Just (Reduce 6 168)
    (388, Token (QVARSYM _)) -> Just (Reduce 6 168)
    (388, Token (BACKQUOTE _)) -> Just (Reduce 6 168)
    (388, Token (QCONSYM _)) -> Just (Reduce 6 168)
    (388, Token (OF _)) -> Just (Reduce 6 168)
    (389, Token (WHERE _)) -> Just (Reduce 4 169)
    (389, Token (RBRACE _)) -> Just (Reduce 4 169)
    (389, Token (RPAREN _)) -> Just (Reduce 4 169)
    (389, Token (COMMA _)) -> Just (Reduce 4 169)
    (389, Token (SEMICOLON _)) -> Just (Reduce 4 169)
    (389, Token (EQUAL _)) -> Just (Reduce 4 169)
    (389, Token (PIPE _)) -> Just (Reduce 4 169)
    (389, Token (COLON_COLON _)) -> Just (Reduce 4 169)
    (389, Token (MINUS _)) -> Just (Reduce 4 169)
    (389, Token (LARROW _)) -> Just (Reduce 4 169)
    (389, Token (THEN _)) -> Just (Reduce 4 169)
    (389, Token (ELSE _)) -> Just (Reduce 4 169)
    (389, Token (QVARSYM _)) -> Just (Reduce 4 169)
    (389, Token (BACKQUOTE _)) -> Just (Reduce 4 169)
    (389, Token (QCONSYM _)) -> Just (Reduce 4 169)
    (389, Token (OF _)) -> Just (Reduce 4 169)
    (390, Token (LBRACE _)) -> Just (Shift 82)
    (391, Token (LBRACE _)) -> Just (Shift 387)
    (392, Token (OF _)) -> Just (Shift 390)
    (393, Token (WHERE _)) -> Just (Reduce 2 167)
    (393, Token (RBRACE _)) -> Just (Reduce 2 167)
    (393, Token (RPAREN _)) -> Just (Reduce 2 167)
    (393, Token (COMMA _)) -> Just (Reduce 2 167)
    (393, Token (SEMICOLON _)) -> Just (Reduce 2 167)
    (393, Token (EQUAL _)) -> Just (Reduce 2 167)
    (393, Token (PIPE _)) -> Just (Reduce 2 167)
    (393, Token (COLON_COLON _)) -> Just (Reduce 2 167)
    (393, Token (MINUS _)) -> Just (Reduce 2 167)
    (393, Token (LARROW _)) -> Just (Reduce 2 167)
    (393, Token (THEN _)) -> Just (Reduce 2 167)
    (393, Token (ELSE _)) -> Just (Reduce 2 167)
    (393, Token (QVARSYM _)) -> Just (Reduce 2 167)
    (393, Token (BACKQUOTE _)) -> Just (Reduce 2 167)
    (393, Token (QCONSYM _)) -> Just (Reduce 2 167)
    (393, Token (OF _)) -> Just (Reduce 2 167)
    (394, Token (RBRACE _)) -> Just (Shift 388)
    (395, Token (RBRACE _)) -> Just (Shift 389)
    (396, Token (RBRACE _)) -> Just (Reduce 3 184)
    (397, Token (RBRACE _)) -> Just (Reduce 1 183)
    (397, Token (SEMICOLON _)) -> Just (Shift 83)
    (398, Token (WHERE _)) -> Just (Reduce 1 171)
    (398, Token (LBRACE _)) -> Just (Reduce 1 171)
    (398, Token (RBRACE _)) -> Just (Reduce 1 171)
    (398, Token (LPAREN _)) -> Just (Reduce 1 171)
    (398, Token (RPAREN _)) -> Just (Reduce 1 171)
    (398, Token (COMMA _)) -> Just (Reduce 1 171)
    (398, Token (SEMICOLON _)) -> Just (Reduce 1 171)
    (398, Token (EQUAL _)) -> Just (Reduce 1 171)
    (398, Token (PIPE _)) -> Just (Reduce 1 171)
    (398, Token (COLON_COLON _)) -> Just (Reduce 1 171)
    (398, Token (MINUS _)) -> Just (Reduce 1 171)
    (398, Token (INFIXL _)) -> Just (Reduce 1 171)
    (398, Token (INFIXR _)) -> Just (Reduce 1 171)
    (398, Token (INFIX _)) -> Just (Reduce 1 171)
    (398, Token (QCONID _)) -> Just (Reduce 1 171)
    (398, Token (EXPORT _)) -> Just (Reduce 1 171)
    (398, Token (AS _)) -> Just (Reduce 1 171)
    (398, Token (QVARID _)) -> Just (Reduce 1 171)
    (398, Token (STRING _)) -> Just (Reduce 1 171)
    (398, Token (LARROW _)) -> Just (Reduce 1 171)
    (398, Token (LET _)) -> Just (Reduce 1 171)
    (398, Token (LAMBDA _)) -> Just (Reduce 1 171)
    (398, Token (IF _)) -> Just (Reduce 1 171)
    (398, Token (THEN _)) -> Just (Reduce 1 171)
    (398, Token (ELSE _)) -> Just (Reduce 1 171)
    (398, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (398, Token (BACKQUOTE _)) -> Just (Reduce 1 171)
    (398, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (398, Token (CASE _)) -> Just (Reduce 1 171)
    (398, Token (OF _)) -> Just (Reduce 1 171)
    (398, Token (DO _)) -> Just (Reduce 1 171)
    (398, Token (INTEGER _)) -> Just (Reduce 1 171)
    (399, Token (WHERE _)) -> Just (Reduce 2 172)
    (399, Token (LBRACE _)) -> Just (Reduce 2 172)
    (399, Token (RBRACE _)) -> Just (Reduce 2 172)
    (399, Token (LPAREN _)) -> Just (Reduce 2 172)
    (399, Token (RPAREN _)) -> Just (Reduce 2 172)
    (399, Token (COMMA _)) -> Just (Reduce 2 172)
    (399, Token (SEMICOLON _)) -> Just (Reduce 2 172)
    (399, Token (EQUAL _)) -> Just (Reduce 2 172)
    (399, Token (PIPE _)) -> Just (Reduce 2 172)
    (399, Token (COLON_COLON _)) -> Just (Reduce 2 172)
    (399, Token (MINUS _)) -> Just (Reduce 2 172)
    (399, Token (INFIXL _)) -> Just (Reduce 2 172)
    (399, Token (INFIXR _)) -> Just (Reduce 2 172)
    (399, Token (INFIX _)) -> Just (Reduce 2 172)
    (399, Token (QCONID _)) -> Just (Reduce 2 172)
    (399, Token (EXPORT _)) -> Just (Reduce 2 172)
    (399, Token (AS _)) -> Just (Reduce 2 172)
    (399, Token (QVARID _)) -> Just (Reduce 2 172)
    (399, Token (STRING _)) -> Just (Reduce 2 172)
    (399, Token (LARROW _)) -> Just (Reduce 2 172)
    (399, Token (LET _)) -> Just (Reduce 2 172)
    (399, Token (LAMBDA _)) -> Just (Reduce 2 172)
    (399, Token (IF _)) -> Just (Reduce 2 172)
    (399, Token (THEN _)) -> Just (Reduce 2 172)
    (399, Token (ELSE _)) -> Just (Reduce 2 172)
    (399, Token (QVARSYM _)) -> Just (Reduce 2 172)
    (399, Token (BACKQUOTE _)) -> Just (Reduce 2 172)
    (399, Token (QCONSYM _)) -> Just (Reduce 2 172)
    (399, Token (CASE _)) -> Just (Reduce 2 172)
    (399, Token (OF _)) -> Just (Reduce 2 172)
    (399, Token (DO _)) -> Just (Reduce 2 172)
    (399, Token (INTEGER _)) -> Just (Reduce 2 172)
    (400, Token (WHERE _)) -> Just (Reduce 3 176)
    (400, Token (LBRACE _)) -> Just (Reduce 3 176)
    (400, Token (RBRACE _)) -> Just (Reduce 3 176)
    (400, Token (LPAREN _)) -> Just (Reduce 3 176)
    (400, Token (RPAREN _)) -> Just (Reduce 3 176)
    (400, Token (COMMA _)) -> Just (Reduce 3 176)
    (400, Token (SEMICOLON _)) -> Just (Reduce 3 176)
    (400, Token (EQUAL _)) -> Just (Reduce 3 176)
    (400, Token (PIPE _)) -> Just (Reduce 3 176)
    (400, Token (COLON_COLON _)) -> Just (Reduce 3 176)
    (400, Token (MINUS _)) -> Just (Reduce 3 176)
    (400, Token (INFIXL _)) -> Just (Reduce 3 176)
    (400, Token (INFIXR _)) -> Just (Reduce 3 176)
    (400, Token (INFIX _)) -> Just (Reduce 3 176)
    (400, Token (QCONID _)) -> Just (Reduce 3 176)
    (400, Token (EXPORT _)) -> Just (Reduce 3 176)
    (400, Token (AS _)) -> Just (Reduce 3 176)
    (400, Token (QVARID _)) -> Just (Reduce 3 176)
    (400, Token (STRING _)) -> Just (Reduce 3 176)
    (400, Token (LARROW _)) -> Just (Reduce 3 176)
    (400, Token (LET _)) -> Just (Reduce 3 176)
    (400, Token (LAMBDA _)) -> Just (Reduce 3 176)
    (400, Token (IF _)) -> Just (Reduce 3 176)
    (400, Token (THEN _)) -> Just (Reduce 3 176)
    (400, Token (ELSE _)) -> Just (Reduce 3 176)
    (400, Token (QVARSYM _)) -> Just (Reduce 3 176)
    (400, Token (BACKQUOTE _)) -> Just (Reduce 3 176)
    (400, Token (QCONSYM _)) -> Just (Reduce 3 176)
    (400, Token (CASE _)) -> Just (Reduce 3 176)
    (400, Token (OF _)) -> Just (Reduce 3 176)
    (400, Token (DO _)) -> Just (Reduce 3 176)
    (400, Token (INTEGER _)) -> Just (Reduce 3 176)
    (401, Token (WHERE _)) -> Just (Reduce 4 177)
    (401, Token (LBRACE _)) -> Just (Reduce 4 177)
    (401, Token (RBRACE _)) -> Just (Reduce 4 177)
    (401, Token (LPAREN _)) -> Just (Reduce 4 177)
    (401, Token (RPAREN _)) -> Just (Reduce 4 177)
    (401, Token (COMMA _)) -> Just (Reduce 4 177)
    (401, Token (SEMICOLON _)) -> Just (Reduce 4 177)
    (401, Token (EQUAL _)) -> Just (Reduce 4 177)
    (401, Token (PIPE _)) -> Just (Reduce 4 177)
    (401, Token (COLON_COLON _)) -> Just (Reduce 4 177)
    (401, Token (MINUS _)) -> Just (Reduce 4 177)
    (401, Token (INFIXL _)) -> Just (Reduce 4 177)
    (401, Token (INFIXR _)) -> Just (Reduce 4 177)
    (401, Token (INFIX _)) -> Just (Reduce 4 177)
    (401, Token (QCONID _)) -> Just (Reduce 4 177)
    (401, Token (EXPORT _)) -> Just (Reduce 4 177)
    (401, Token (AS _)) -> Just (Reduce 4 177)
    (401, Token (QVARID _)) -> Just (Reduce 4 177)
    (401, Token (STRING _)) -> Just (Reduce 4 177)
    (401, Token (LARROW _)) -> Just (Reduce 4 177)
    (401, Token (LET _)) -> Just (Reduce 4 177)
    (401, Token (LAMBDA _)) -> Just (Reduce 4 177)
    (401, Token (IF _)) -> Just (Reduce 4 177)
    (401, Token (THEN _)) -> Just (Reduce 4 177)
    (401, Token (ELSE _)) -> Just (Reduce 4 177)
    (401, Token (QVARSYM _)) -> Just (Reduce 4 177)
    (401, Token (BACKQUOTE _)) -> Just (Reduce 4 177)
    (401, Token (QCONSYM _)) -> Just (Reduce 4 177)
    (401, Token (CASE _)) -> Just (Reduce 4 177)
    (401, Token (OF _)) -> Just (Reduce 4 177)
    (401, Token (DO _)) -> Just (Reduce 4 177)
    (401, Token (INTEGER _)) -> Just (Reduce 4 177)
    (402, Token (WHERE _)) -> Just (Reduce 6 182)
    (402, Token (LBRACE _)) -> Just (Reduce 6 182)
    (402, Token (RBRACE _)) -> Just (Reduce 6 182)
    (402, Token (LPAREN _)) -> Just (Reduce 6 182)
    (402, Token (RPAREN _)) -> Just (Reduce 6 182)
    (402, Token (COMMA _)) -> Just (Reduce 6 182)
    (402, Token (SEMICOLON _)) -> Just (Reduce 6 182)
    (402, Token (EQUAL _)) -> Just (Reduce 6 182)
    (402, Token (PIPE _)) -> Just (Reduce 6 182)
    (402, Token (COLON_COLON _)) -> Just (Reduce 6 182)
    (402, Token (MINUS _)) -> Just (Reduce 6 182)
    (402, Token (INFIXL _)) -> Just (Reduce 6 182)
    (402, Token (INFIXR _)) -> Just (Reduce 6 182)
    (402, Token (INFIX _)) -> Just (Reduce 6 182)
    (402, Token (QCONID _)) -> Just (Reduce 6 182)
    (402, Token (EXPORT _)) -> Just (Reduce 6 182)
    (402, Token (AS _)) -> Just (Reduce 6 182)
    (402, Token (QVARID _)) -> Just (Reduce 6 182)
    (402, Token (STRING _)) -> Just (Reduce 6 182)
    (402, Token (LARROW _)) -> Just (Reduce 6 182)
    (402, Token (LET _)) -> Just (Reduce 6 182)
    (402, Token (LAMBDA _)) -> Just (Reduce 6 182)
    (402, Token (IF _)) -> Just (Reduce 6 182)
    (402, Token (THEN _)) -> Just (Reduce 6 182)
    (402, Token (ELSE _)) -> Just (Reduce 6 182)
    (402, Token (QVARSYM _)) -> Just (Reduce 6 182)
    (402, Token (BACKQUOTE _)) -> Just (Reduce 6 182)
    (402, Token (QCONSYM _)) -> Just (Reduce 6 182)
    (402, Token (CASE _)) -> Just (Reduce 6 182)
    (402, Token (OF _)) -> Just (Reduce 6 182)
    (402, Token (DO _)) -> Just (Reduce 6 182)
    (402, Token (INTEGER _)) -> Just (Reduce 6 182)
    (403, Token (WHERE _)) -> Just (Reduce 6 179)
    (403, Token (LBRACE _)) -> Just (Reduce 6 179)
    (403, Token (RBRACE _)) -> Just (Reduce 6 179)
    (403, Token (LPAREN _)) -> Just (Reduce 6 179)
    (403, Token (RPAREN _)) -> Just (Reduce 6 179)
    (403, Token (COMMA _)) -> Just (Reduce 6 179)
    (403, Token (SEMICOLON _)) -> Just (Reduce 6 179)
    (403, Token (EQUAL _)) -> Just (Reduce 6 179)
    (403, Token (PIPE _)) -> Just (Reduce 6 179)
    (403, Token (COLON_COLON _)) -> Just (Reduce 6 179)
    (403, Token (MINUS _)) -> Just (Reduce 6 179)
    (403, Token (INFIXL _)) -> Just (Reduce 6 179)
    (403, Token (INFIXR _)) -> Just (Reduce 6 179)
    (403, Token (INFIX _)) -> Just (Reduce 6 179)
    (403, Token (QCONID _)) -> Just (Reduce 6 179)
    (403, Token (EXPORT _)) -> Just (Reduce 6 179)
    (403, Token (AS _)) -> Just (Reduce 6 179)
    (403, Token (QVARID _)) -> Just (Reduce 6 179)
    (403, Token (STRING _)) -> Just (Reduce 6 179)
    (403, Token (LARROW _)) -> Just (Reduce 6 179)
    (403, Token (LET _)) -> Just (Reduce 6 179)
    (403, Token (LAMBDA _)) -> Just (Reduce 6 179)
    (403, Token (IF _)) -> Just (Reduce 6 179)
    (403, Token (THEN _)) -> Just (Reduce 6 179)
    (403, Token (ELSE _)) -> Just (Reduce 6 179)
    (403, Token (QVARSYM _)) -> Just (Reduce 6 179)
    (403, Token (BACKQUOTE _)) -> Just (Reduce 6 179)
    (403, Token (QCONSYM _)) -> Just (Reduce 6 179)
    (403, Token (CASE _)) -> Just (Reduce 6 179)
    (403, Token (OF _)) -> Just (Reduce 6 179)
    (403, Token (DO _)) -> Just (Reduce 6 179)
    (403, Token (INTEGER _)) -> Just (Reduce 6 179)
    (404, Token (WHERE _)) -> Just (Reduce 6 178)
    (404, Token (LBRACE _)) -> Just (Reduce 6 178)
    (404, Token (RBRACE _)) -> Just (Reduce 6 178)
    (404, Token (LPAREN _)) -> Just (Reduce 6 178)
    (404, Token (RPAREN _)) -> Just (Reduce 6 178)
    (404, Token (COMMA _)) -> Just (Reduce 6 178)
    (404, Token (SEMICOLON _)) -> Just (Reduce 6 178)
    (404, Token (EQUAL _)) -> Just (Reduce 6 178)
    (404, Token (PIPE _)) -> Just (Reduce 6 178)
    (404, Token (COLON_COLON _)) -> Just (Reduce 6 178)
    (404, Token (MINUS _)) -> Just (Reduce 6 178)
    (404, Token (INFIXL _)) -> Just (Reduce 6 178)
    (404, Token (INFIXR _)) -> Just (Reduce 6 178)
    (404, Token (INFIX _)) -> Just (Reduce 6 178)
    (404, Token (QCONID _)) -> Just (Reduce 6 178)
    (404, Token (EXPORT _)) -> Just (Reduce 6 178)
    (404, Token (AS _)) -> Just (Reduce 6 178)
    (404, Token (QVARID _)) -> Just (Reduce 6 178)
    (404, Token (STRING _)) -> Just (Reduce 6 178)
    (404, Token (LARROW _)) -> Just (Reduce 6 178)
    (404, Token (LET _)) -> Just (Reduce 6 178)
    (404, Token (LAMBDA _)) -> Just (Reduce 6 178)
    (404, Token (IF _)) -> Just (Reduce 6 178)
    (404, Token (THEN _)) -> Just (Reduce 6 178)
    (404, Token (ELSE _)) -> Just (Reduce 6 178)
    (404, Token (QVARSYM _)) -> Just (Reduce 6 178)
    (404, Token (BACKQUOTE _)) -> Just (Reduce 6 178)
    (404, Token (QCONSYM _)) -> Just (Reduce 6 178)
    (404, Token (CASE _)) -> Just (Reduce 6 178)
    (404, Token (OF _)) -> Just (Reduce 6 178)
    (404, Token (DO _)) -> Just (Reduce 6 178)
    (404, Token (INTEGER _)) -> Just (Reduce 6 178)
    (405, Token (WHERE _)) -> Just (Reduce 6 180)
    (405, Token (LBRACE _)) -> Just (Reduce 6 180)
    (405, Token (RBRACE _)) -> Just (Reduce 6 180)
    (405, Token (LPAREN _)) -> Just (Reduce 6 180)
    (405, Token (RPAREN _)) -> Just (Reduce 6 180)
    (405, Token (COMMA _)) -> Just (Reduce 6 180)
    (405, Token (SEMICOLON _)) -> Just (Reduce 6 180)
    (405, Token (EQUAL _)) -> Just (Reduce 6 180)
    (405, Token (PIPE _)) -> Just (Reduce 6 180)
    (405, Token (COLON_COLON _)) -> Just (Reduce 6 180)
    (405, Token (MINUS _)) -> Just (Reduce 6 180)
    (405, Token (INFIXL _)) -> Just (Reduce 6 180)
    (405, Token (INFIXR _)) -> Just (Reduce 6 180)
    (405, Token (INFIX _)) -> Just (Reduce 6 180)
    (405, Token (QCONID _)) -> Just (Reduce 6 180)
    (405, Token (EXPORT _)) -> Just (Reduce 6 180)
    (405, Token (AS _)) -> Just (Reduce 6 180)
    (405, Token (QVARID _)) -> Just (Reduce 6 180)
    (405, Token (STRING _)) -> Just (Reduce 6 180)
    (405, Token (LARROW _)) -> Just (Reduce 6 180)
    (405, Token (LET _)) -> Just (Reduce 6 180)
    (405, Token (LAMBDA _)) -> Just (Reduce 6 180)
    (405, Token (IF _)) -> Just (Reduce 6 180)
    (405, Token (THEN _)) -> Just (Reduce 6 180)
    (405, Token (ELSE _)) -> Just (Reduce 6 180)
    (405, Token (QVARSYM _)) -> Just (Reduce 6 180)
    (405, Token (BACKQUOTE _)) -> Just (Reduce 6 180)
    (405, Token (QCONSYM _)) -> Just (Reduce 6 180)
    (405, Token (CASE _)) -> Just (Reduce 6 180)
    (405, Token (OF _)) -> Just (Reduce 6 180)
    (405, Token (DO _)) -> Just (Reduce 6 180)
    (405, Token (INTEGER _)) -> Just (Reduce 6 180)
    (406, Token (WHERE _)) -> Just (Reduce 4 181)
    (406, Token (LBRACE _)) -> Just (Reduce 4 181)
    (406, Token (RBRACE _)) -> Just (Reduce 4 181)
    (406, Token (LPAREN _)) -> Just (Reduce 4 181)
    (406, Token (RPAREN _)) -> Just (Reduce 4 181)
    (406, Token (COMMA _)) -> Just (Reduce 4 181)
    (406, Token (SEMICOLON _)) -> Just (Reduce 4 181)
    (406, Token (EQUAL _)) -> Just (Reduce 4 181)
    (406, Token (PIPE _)) -> Just (Reduce 4 181)
    (406, Token (COLON_COLON _)) -> Just (Reduce 4 181)
    (406, Token (MINUS _)) -> Just (Reduce 4 181)
    (406, Token (INFIXL _)) -> Just (Reduce 4 181)
    (406, Token (INFIXR _)) -> Just (Reduce 4 181)
    (406, Token (INFIX _)) -> Just (Reduce 4 181)
    (406, Token (QCONID _)) -> Just (Reduce 4 181)
    (406, Token (EXPORT _)) -> Just (Reduce 4 181)
    (406, Token (AS _)) -> Just (Reduce 4 181)
    (406, Token (QVARID _)) -> Just (Reduce 4 181)
    (406, Token (STRING _)) -> Just (Reduce 4 181)
    (406, Token (LARROW _)) -> Just (Reduce 4 181)
    (406, Token (LET _)) -> Just (Reduce 4 181)
    (406, Token (LAMBDA _)) -> Just (Reduce 4 181)
    (406, Token (IF _)) -> Just (Reduce 4 181)
    (406, Token (THEN _)) -> Just (Reduce 4 181)
    (406, Token (ELSE _)) -> Just (Reduce 4 181)
    (406, Token (QVARSYM _)) -> Just (Reduce 4 181)
    (406, Token (BACKQUOTE _)) -> Just (Reduce 4 181)
    (406, Token (QCONSYM _)) -> Just (Reduce 4 181)
    (406, Token (CASE _)) -> Just (Reduce 4 181)
    (406, Token (OF _)) -> Just (Reduce 4 181)
    (406, Token (DO _)) -> Just (Reduce 4 181)
    (406, Token (INTEGER _)) -> Just (Reduce 4 181)
    (407, Token (BACKQUOTE _)) -> Just (Shift 54)
    (408, Token (BACKQUOTE _)) -> Just (Shift 55)
    (409, Token (BACKQUOTE _)) -> Just (Shift 56)
    (410, Token (BACKQUOTE _)) -> Just (Shift 57)
    (411, Token (WHERE _)) -> Just (Reduce 1 175)
    (411, Token (LBRACE _)) -> Just (Reduce 1 175)
    (411, Token (RBRACE _)) -> Just (Reduce 1 175)
    (411, Token (LPAREN _)) -> Just (Reduce 1 175)
    (411, Token (RPAREN _)) -> Just (Reduce 1 175)
    (411, Token (COMMA _)) -> Just (Reduce 1 175)
    (411, Token (SEMICOLON _)) -> Just (Reduce 1 175)
    (411, Token (EQUAL _)) -> Just (Reduce 1 175)
    (411, Token (PIPE _)) -> Just (Reduce 1 175)
    (411, Token (COLON_COLON _)) -> Just (Reduce 1 175)
    (411, Token (MINUS _)) -> Just (Reduce 1 175)
    (411, Token (INFIXL _)) -> Just (Reduce 1 175)
    (411, Token (INFIXR _)) -> Just (Reduce 1 175)
    (411, Token (INFIX _)) -> Just (Reduce 1 175)
    (411, Token (QCONID _)) -> Just (Reduce 1 175)
    (411, Token (EXPORT _)) -> Just (Reduce 1 175)
    (411, Token (AS _)) -> Just (Reduce 1 175)
    (411, Token (QVARID _)) -> Just (Reduce 1 175)
    (411, Token (STRING _)) -> Just (Reduce 1 175)
    (411, Token (LARROW _)) -> Just (Reduce 1 175)
    (411, Token (LET _)) -> Just (Reduce 1 175)
    (411, Token (LAMBDA _)) -> Just (Reduce 1 175)
    (411, Token (IF _)) -> Just (Reduce 1 175)
    (411, Token (THEN _)) -> Just (Reduce 1 175)
    (411, Token (ELSE _)) -> Just (Reduce 1 175)
    (411, Token (QVARSYM _)) -> Just (Reduce 1 175)
    (411, Token (BACKQUOTE _)) -> Just (Reduce 1 175)
    (411, Token (QCONSYM _)) -> Just (Reduce 1 175)
    (411, Token (CASE _)) -> Just (Reduce 1 175)
    (411, Token (OF _)) -> Just (Reduce 1 175)
    (411, Token (DO _)) -> Just (Reduce 1 175)
    (411, Token (INTEGER _)) -> Just (Reduce 1 175)
    (412, Token (QCONID _)) -> Just (Shift 407)
    (412, Token (EXPORT _)) -> Just (Shift 408)
    (412, Token (AS _)) -> Just (Shift 409)
    (412, Token (QVARID _)) -> Just (Shift 410)
    (413, Token (WHERE _)) -> Just (Reduce 1 174)
    (413, Token (LBRACE _)) -> Just (Reduce 1 174)
    (413, Token (RBRACE _)) -> Just (Reduce 1 174)
    (413, Token (LPAREN _)) -> Just (Reduce 1 174)
    (413, Token (RPAREN _)) -> Just (Reduce 1 174)
    (413, Token (COMMA _)) -> Just (Reduce 1 174)
    (413, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (413, Token (EQUAL _)) -> Just (Reduce 1 174)
    (413, Token (PIPE _)) -> Just (Reduce 1 174)
    (413, Token (COLON_COLON _)) -> Just (Reduce 1 174)
    (413, Token (MINUS _)) -> Just (Reduce 1 174)
    (413, Token (INFIXL _)) -> Just (Reduce 1 174)
    (413, Token (INFIXR _)) -> Just (Reduce 1 174)
    (413, Token (INFIX _)) -> Just (Reduce 1 174)
    (413, Token (QCONID _)) -> Just (Reduce 1 174)
    (413, Token (EXPORT _)) -> Just (Reduce 1 174)
    (413, Token (AS _)) -> Just (Reduce 1 174)
    (413, Token (QVARID _)) -> Just (Reduce 1 174)
    (413, Token (STRING _)) -> Just (Reduce 1 174)
    (413, Token (LARROW _)) -> Just (Reduce 1 174)
    (413, Token (LET _)) -> Just (Reduce 1 174)
    (413, Token (LAMBDA _)) -> Just (Reduce 1 174)
    (413, Token (IF _)) -> Just (Reduce 1 174)
    (413, Token (THEN _)) -> Just (Reduce 1 174)
    (413, Token (ELSE _)) -> Just (Reduce 1 174)
    (413, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (413, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (413, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (413, Token (CASE _)) -> Just (Reduce 1 174)
    (413, Token (OF _)) -> Just (Reduce 1 174)
    (413, Token (DO _)) -> Just (Reduce 1 174)
    (413, Token (INTEGER _)) -> Just (Reduce 1 174)
    (414, Token (WHERE _)) -> Just (Reduce 1 173)
    (414, Token (LBRACE _)) -> Just (Reduce 1 173)
    (414, Token (RBRACE _)) -> Just (Reduce 1 173)
    (414, Token (LPAREN _)) -> Just (Reduce 1 173)
    (414, Token (RPAREN _)) -> Just (Reduce 1 173)
    (414, Token (COMMA _)) -> Just (Reduce 1 173)
    (414, Token (SEMICOLON _)) -> Just (Reduce 1 173)
    (414, Token (EQUAL _)) -> Just (Reduce 1 173)
    (414, Token (PIPE _)) -> Just (Reduce 1 173)
    (414, Token (COLON_COLON _)) -> Just (Reduce 1 173)
    (414, Token (MINUS _)) -> Just (Reduce 1 173)
    (414, Token (INFIXL _)) -> Just (Reduce 1 173)
    (414, Token (INFIXR _)) -> Just (Reduce 1 173)
    (414, Token (INFIX _)) -> Just (Reduce 1 173)
    (414, Token (QCONID _)) -> Just (Reduce 1 173)
    (414, Token (EXPORT _)) -> Just (Reduce 1 173)
    (414, Token (AS _)) -> Just (Reduce 1 173)
    (414, Token (QVARID _)) -> Just (Reduce 1 173)
    (414, Token (STRING _)) -> Just (Reduce 1 173)
    (414, Token (LARROW _)) -> Just (Reduce 1 173)
    (414, Token (LET _)) -> Just (Reduce 1 173)
    (414, Token (LAMBDA _)) -> Just (Reduce 1 173)
    (414, Token (IF _)) -> Just (Reduce 1 173)
    (414, Token (THEN _)) -> Just (Reduce 1 173)
    (414, Token (ELSE _)) -> Just (Reduce 1 173)
    (414, Token (QVARSYM _)) -> Just (Reduce 1 173)
    (414, Token (BACKQUOTE _)) -> Just (Reduce 1 173)
    (414, Token (QCONSYM _)) -> Just (Reduce 1 173)
    (414, Token (CASE _)) -> Just (Reduce 1 173)
    (414, Token (OF _)) -> Just (Reduce 1 173)
    (414, Token (DO _)) -> Just (Reduce 1 173)
    (414, Token (INTEGER _)) -> Just (Reduce 1 173)
    (415, Token (RPAREN _)) -> Just (Shift 400)
    (416, Token (RPAREN _)) -> Just (Shift 401)
    (417, Token (RPAREN _)) -> Just (Shift 402)
    (418, Token (RPAREN _)) -> Just (Shift 403)
    (419, Token (RPAREN _)) -> Just (Shift 404)
    (420, Token (RPAREN _)) -> Just (Shift 405)
    (421, Token (RPAREN _)) -> Just (Shift 406)
    (422, Token (RBRACE _)) -> Just (Reduce 5 186)
    (422, Token (SEMICOLON _)) -> Just (Reduce 5 186)
    (423, Token (WHERE _)) -> Just (Shift 259)
    (423, Token (RBRACE _)) -> Just (Reduce 3 185)
    (423, Token (SEMICOLON _)) -> Just (Reduce 3 185)
    (424, Token (LPAREN _)) -> Just (Reduce 3 200)
    (424, Token (RPAREN _)) -> Just (Reduce 3 200)
    (424, Token (EQUAL _)) -> Just (Reduce 3 200)
    (424, Token (PIPE _)) -> Just (Reduce 3 200)
    (424, Token (MINUS _)) -> Just (Reduce 3 200)
    (424, Token (RARROW _)) -> Just (Reduce 3 200)
    (424, Token (QCONID _)) -> Just (Reduce 3 200)
    (424, Token (EXPORT _)) -> Just (Reduce 3 200)
    (424, Token (AS _)) -> Just (Reduce 3 200)
    (424, Token (QVARID _)) -> Just (Reduce 3 200)
    (424, Token (QVARSYM _)) -> Just (Reduce 3 200)
    (424, Token (BACKQUOTE _)) -> Just (Reduce 3 200)
    (424, Token (QCONSYM _)) -> Just (Reduce 3 200)
    (425, Token (LPAREN _)) -> Just (Reduce 1 199)
    (425, Token (RPAREN _)) -> Just (Reduce 1 199)
    (425, Token (EQUAL _)) -> Just (Reduce 1 199)
    (425, Token (PIPE _)) -> Just (Reduce 1 199)
    (425, Token (MINUS _)) -> Just (Reduce 1 199)
    (425, Token (RARROW _)) -> Just (Reduce 1 199)
    (425, Token (QCONID _)) -> Just (Reduce 1 199)
    (425, Token (EXPORT _)) -> Just (Reduce 1 199)
    (425, Token (AS _)) -> Just (Reduce 1 199)
    (425, Token (QVARID _)) -> Just (Reduce 1 199)
    (425, Token (QVARSYM _)) -> Just (Reduce 1 199)
    (425, Token (BACKQUOTE _)) -> Just (Reduce 1 199)
    (425, Token (QCONSYM _)) -> Just (Reduce 1 199)
    (426, Token (BACKQUOTE _)) -> Just (Shift 430)
    (427, Token (BACKQUOTE _)) -> Just (Shift 431)
    (428, Token (BACKQUOTE _)) -> Just (Shift 432)
    (429, Token (RBRACE _)) -> Just (Reduce 1 208)
    (429, Token (LPAREN _)) -> Just (Reduce 1 208)
    (429, Token (COMMA _)) -> Just (Reduce 1 208)
    (429, Token (SEMICOLON _)) -> Just (Reduce 1 208)
    (429, Token (MINUS _)) -> Just (Reduce 1 208)
    (429, Token (QCONID _)) -> Just (Reduce 1 208)
    (429, Token (EXPORT _)) -> Just (Reduce 1 208)
    (429, Token (AS _)) -> Just (Reduce 1 208)
    (429, Token (QVARID _)) -> Just (Reduce 1 208)
    (429, Token (QVARSYM _)) -> Just (Reduce 1 208)
    (429, Token (BACKQUOTE _)) -> Just (Reduce 1 208)
    (429, Token (QCONSYM _)) -> Just (Reduce 1 208)
    (430, Token (RBRACE _)) -> Just (Reduce 3 210)
    (430, Token (LPAREN _)) -> Just (Reduce 3 210)
    (430, Token (COMMA _)) -> Just (Reduce 3 210)
    (430, Token (SEMICOLON _)) -> Just (Reduce 3 210)
    (430, Token (MINUS _)) -> Just (Reduce 3 210)
    (430, Token (QCONID _)) -> Just (Reduce 3 210)
    (430, Token (EXPORT _)) -> Just (Reduce 3 210)
    (430, Token (AS _)) -> Just (Reduce 3 210)
    (430, Token (QVARID _)) -> Just (Reduce 3 210)
    (430, Token (QVARSYM _)) -> Just (Reduce 3 210)
    (430, Token (BACKQUOTE _)) -> Just (Reduce 3 210)
    (430, Token (QCONSYM _)) -> Just (Reduce 3 210)
    (431, Token (RBRACE _)) -> Just (Reduce 3 209)
    (431, Token (LPAREN _)) -> Just (Reduce 3 209)
    (431, Token (COMMA _)) -> Just (Reduce 3 209)
    (431, Token (SEMICOLON _)) -> Just (Reduce 3 209)
    (431, Token (MINUS _)) -> Just (Reduce 3 209)
    (431, Token (QCONID _)) -> Just (Reduce 3 209)
    (431, Token (EXPORT _)) -> Just (Reduce 3 209)
    (431, Token (AS _)) -> Just (Reduce 3 209)
    (431, Token (QVARID _)) -> Just (Reduce 3 209)
    (431, Token (QVARSYM _)) -> Just (Reduce 3 209)
    (431, Token (BACKQUOTE _)) -> Just (Reduce 3 209)
    (431, Token (QCONSYM _)) -> Just (Reduce 3 209)
    (432, Token (RBRACE _)) -> Just (Reduce 3 211)
    (432, Token (LPAREN _)) -> Just (Reduce 3 211)
    (432, Token (COMMA _)) -> Just (Reduce 3 211)
    (432, Token (SEMICOLON _)) -> Just (Reduce 3 211)
    (432, Token (MINUS _)) -> Just (Reduce 3 211)
    (432, Token (QCONID _)) -> Just (Reduce 3 211)
    (432, Token (EXPORT _)) -> Just (Reduce 3 211)
    (432, Token (AS _)) -> Just (Reduce 3 211)
    (432, Token (QVARID _)) -> Just (Reduce 3 211)
    (432, Token (QVARSYM _)) -> Just (Reduce 3 211)
    (432, Token (BACKQUOTE _)) -> Just (Reduce 3 211)
    (432, Token (QCONSYM _)) -> Just (Reduce 3 211)
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
production 187 = 69
production 188 = 69
production 189 = 70
production 190 = 70
production 191 = 71
production 192 = 71
production 193 = 71
production 194 = 65
production 195 = 31
production 196 = 31
production 197 = 31
production 198 = 31
production 199 = 72
production 200 = 72
production 201 = 8
production 202 = 8
production 203 = 8
production 204 = 8
production 205 = 8
production 206 = 9
production 207 = 9
production 208 = 73
production 209 = 73
production 210 = 73
production 211 = 73
production 212 = 52
production 213 = 52
production 214 = 44
production 215 = 44
production 216 = 16
production 217 = 16
production 218 = 15
production 219 = 15
production 220 = 47
production 221 = 47
production 222 = 47
production 223 = 74
production 224 = 1
production 225 = 42
production 226 = 42
production 227 = 62
production 228 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 193
    (9, 1) -> Just 218
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 298
    (13, 14) -> Just 18
    (13, 27) -> Just 216
    (13, 30) -> Just 252
    (13, 31) -> Just 76
    (13, 40) -> Just 268
    (13, 41) -> Just 269
    (13, 72) -> Just 272
    (16, 4) -> Just 17
    (16, 8) -> Just 298
    (16, 14) -> Just 18
    (16, 27) -> Just 216
    (16, 30) -> Just 252
    (16, 31) -> Just 76
    (16, 40) -> Just 268
    (16, 41) -> Just 269
    (16, 72) -> Just 272
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 167
    (25, 9) -> Just 168
    (25, 10) -> Just 33
    (25, 13) -> Just 157
    (34, 8) -> Just 414
    (34, 32) -> Just 371
    (34, 61) -> Just 276
    (34, 63) -> Just 385
    (34, 66) -> Just 45
    (34, 67) -> Just 398
    (35, 8) -> Just 414
    (35, 32) -> Just 372
    (35, 61) -> Just 276
    (35, 63) -> Just 385
    (35, 66) -> Just 45
    (35, 67) -> Just 398
    (36, 8) -> Just 414
    (36, 32) -> Just 373
    (36, 61) -> Just 276
    (36, 63) -> Just 385
    (36, 66) -> Just 45
    (36, 67) -> Just 398
    (37, 8) -> Just 414
    (37, 32) -> Just 376
    (37, 61) -> Just 276
    (37, 63) -> Just 385
    (37, 66) -> Just 45
    (37, 67) -> Just 398
    (38, 8) -> Just 414
    (38, 32) -> Just 377
    (38, 61) -> Just 276
    (38, 63) -> Just 385
    (38, 66) -> Just 45
    (38, 67) -> Just 398
    (39, 8) -> Just 414
    (39, 32) -> Just 378
    (39, 61) -> Just 276
    (39, 63) -> Just 385
    (39, 66) -> Just 45
    (39, 67) -> Just 398
    (40, 8) -> Just 414
    (40, 32) -> Just 379
    (40, 61) -> Just 276
    (40, 63) -> Just 385
    (40, 66) -> Just 45
    (40, 67) -> Just 398
    (41, 8) -> Just 414
    (41, 32) -> Just 380
    (41, 61) -> Just 276
    (41, 63) -> Just 385
    (41, 66) -> Just 45
    (41, 67) -> Just 398
    (42, 8) -> Just 414
    (42, 32) -> Just 381
    (42, 61) -> Just 276
    (42, 63) -> Just 385
    (42, 66) -> Just 45
    (42, 67) -> Just 398
    (43, 8) -> Just 414
    (43, 32) -> Just 382
    (43, 61) -> Just 276
    (43, 63) -> Just 385
    (43, 66) -> Just 45
    (43, 67) -> Just 398
    (44, 8) -> Just 414
    (44, 63) -> Just 393
    (44, 66) -> Just 45
    (44, 67) -> Just 398
    (45, 8) -> Just 414
    (45, 67) -> Just 399
    (46, 8) -> Just 414
    (46, 32) -> Just 253
    (46, 61) -> Just 276
    (46, 63) -> Just 385
    (46, 66) -> Just 45
    (46, 67) -> Just 398
    (47, 8) -> Just 414
    (47, 32) -> Just 277
    (47, 61) -> Just 276
    (47, 63) -> Just 385
    (47, 66) -> Just 45
    (47, 67) -> Just 398
    (48, 8) -> Just 414
    (48, 32) -> Just 287
    (48, 61) -> Just 276
    (48, 63) -> Just 385
    (48, 66) -> Just 45
    (48, 67) -> Just 398
    (49, 8) -> Just 414
    (49, 32) -> Just 295
    (49, 61) -> Just 276
    (49, 63) -> Just 385
    (49, 66) -> Just 45
    (49, 67) -> Just 398
    (50, 8) -> Just 414
    (50, 32) -> Just 423
    (50, 61) -> Just 276
    (50, 63) -> Just 385
    (50, 66) -> Just 45
    (50, 67) -> Just 398
    (51, 8) -> Just 414
    (51, 32) -> Just 415
    (51, 61) -> Just 276
    (51, 63) -> Just 385
    (51, 66) -> Just 45
    (51, 67) -> Just 398
    (52, 8) -> Just 414
    (52, 63) -> Just 393
    (52, 66) -> Just 45
    (52, 67) -> Just 398
    (53, 8) -> Just 414
    (53, 61) -> Just 416
    (53, 63) -> Just 385
    (53, 66) -> Just 45
    (53, 67) -> Just 398
    (54, 8) -> Just 414
    (54, 61) -> Just 417
    (54, 63) -> Just 385
    (54, 66) -> Just 45
    (54, 67) -> Just 398
    (55, 8) -> Just 414
    (55, 61) -> Just 418
    (55, 63) -> Just 385
    (55, 66) -> Just 45
    (55, 67) -> Just 398
    (56, 8) -> Just 414
    (56, 61) -> Just 419
    (56, 63) -> Just 385
    (56, 66) -> Just 45
    (56, 67) -> Just 398
    (57, 8) -> Just 414
    (57, 61) -> Just 420
    (57, 63) -> Just 385
    (57, 66) -> Just 45
    (57, 67) -> Just 398
    (58, 8) -> Just 414
    (58, 61) -> Just 421
    (58, 63) -> Just 385
    (58, 66) -> Just 45
    (58, 67) -> Just 398
    (59, 8) -> Just 414
    (59, 33) -> Just 254
    (59, 59) -> Just 279
    (59, 60) -> Just 359
    (59, 61) -> Just 362
    (59, 63) -> Just 385
    (59, 66) -> Just 45
    (59, 67) -> Just 398
    (60, 8) -> Just 414
    (60, 33) -> Just 278
    (60, 59) -> Just 279
    (60, 60) -> Just 359
    (60, 61) -> Just 362
    (60, 63) -> Just 385
    (60, 66) -> Just 45
    (60, 67) -> Just 398
    (61, 8) -> Just 414
    (61, 33) -> Just 288
    (61, 59) -> Just 279
    (61, 60) -> Just 359
    (61, 61) -> Just 362
    (61, 63) -> Just 385
    (61, 66) -> Just 45
    (61, 67) -> Just 398
    (62, 8) -> Just 414
    (62, 33) -> Just 296
    (62, 59) -> Just 279
    (62, 60) -> Just 359
    (62, 61) -> Just 362
    (62, 63) -> Just 385
    (62, 66) -> Just 45
    (62, 67) -> Just 398
    (63, 8) -> Just 414
    (63, 59) -> Just 358
    (63, 60) -> Just 359
    (63, 61) -> Just 362
    (63, 63) -> Just 385
    (63, 66) -> Just 45
    (63, 67) -> Just 398
    (64, 8) -> Just 414
    (64, 32) -> Just 374
    (64, 61) -> Just 276
    (64, 63) -> Just 385
    (64, 66) -> Just 45
    (64, 67) -> Just 398
    (65, 8) -> Just 414
    (65, 32) -> Just 375
    (65, 61) -> Just 276
    (65, 63) -> Just 385
    (65, 66) -> Just 45
    (65, 67) -> Just 398
    (66, 8) -> Just 414
    (66, 32) -> Just 392
    (66, 61) -> Just 276
    (66, 63) -> Just 385
    (66, 66) -> Just 45
    (66, 67) -> Just 398
    (67, 8) -> Just 414
    (67, 32) -> Just 361
    (67, 61) -> Just 276
    (67, 63) -> Just 385
    (67, 66) -> Just 45
    (67, 67) -> Just 398
    (68, 8) -> Just 425
    (68, 72) -> Just 273
    (69, 8) -> Just 425
    (69, 72) -> Just 275
    (70, 8) -> Just 425
    (70, 31) -> Just 71
    (70, 72) -> Just 272
    (71, 8) -> Just 425
    (71, 44) -> Just 69
    (71, 52) -> Just 310
    (71, 72) -> Just 274
    (71, 73) -> Just 311
    (72, 8) -> Just 298
    (72, 27) -> Just 264
    (72, 29) -> Just 263
    (72, 30) -> Just 252
    (72, 31) -> Just 76
    (72, 40) -> Just 268
    (72, 41) -> Just 269
    (72, 72) -> Just 272
    (73, 8) -> Just 298
    (73, 27) -> Just 264
    (73, 29) -> Just 265
    (73, 30) -> Just 252
    (73, 31) -> Just 76
    (73, 40) -> Just 268
    (73, 41) -> Just 269
    (73, 72) -> Just 272
    (74, 8) -> Just 298
    (74, 30) -> Just 286
    (74, 31) -> Just 79
    (74, 35) -> Just 281
    (74, 36) -> Just 283
    (74, 40) -> Just 268
    (74, 41) -> Just 269
    (74, 72) -> Just 272
    (75, 8) -> Just 298
    (75, 30) -> Just 286
    (75, 31) -> Just 79
    (75, 35) -> Just 282
    (75, 36) -> Just 283
    (75, 40) -> Just 268
    (75, 41) -> Just 269
    (75, 72) -> Just 272
    (76, 8) -> Just 425
    (76, 44) -> Just 69
    (76, 52) -> Just 310
    (76, 72) -> Just 274
    (76, 73) -> Just 311
    (77, 8) -> Just 425
    (77, 31) -> Just 80
    (77, 38) -> Just 290
    (77, 39) -> Just 292
    (77, 72) -> Just 272
    (78, 8) -> Just 425
    (78, 31) -> Just 80
    (78, 38) -> Just 291
    (78, 39) -> Just 292
    (78, 72) -> Just 272
    (79, 8) -> Just 425
    (79, 44) -> Just 69
    (79, 52) -> Just 310
    (79, 72) -> Just 274
    (79, 73) -> Just 311
    (80, 8) -> Just 425
    (80, 44) -> Just 69
    (80, 52) -> Just 310
    (80, 72) -> Just 274
    (80, 73) -> Just 311
    (81, 8) -> Just 425
    (81, 31) -> Just 84
    (81, 72) -> Just 272
    (82, 8) -> Just 425
    (82, 31) -> Just 85
    (82, 64) -> Just 394
    (82, 68) -> Just 397
    (82, 72) -> Just 272
    (83, 8) -> Just 425
    (83, 31) -> Just 85
    (83, 64) -> Just 396
    (83, 68) -> Just 397
    (83, 72) -> Just 272
    (84, 8) -> Just 425
    (84, 44) -> Just 69
    (84, 52) -> Just 310
    (84, 72) -> Just 274
    (84, 73) -> Just 311
    (85, 8) -> Just 425
    (85, 44) -> Just 69
    (85, 52) -> Just 310
    (85, 72) -> Just 274
    (85, 73) -> Just 311
    (86, 8) -> Just 164
    (86, 9) -> Just 165
    (86, 11) -> Just 158
    (86, 12) -> Just 159
    (87, 8) -> Just 164
    (87, 9) -> Just 165
    (87, 11) -> Just 194
    (87, 12) -> Just 159
    (88, 8) -> Just 164
    (88, 9) -> Just 165
    (88, 11) -> Just 195
    (88, 12) -> Just 159
    (89, 8) -> Just 167
    (89, 9) -> Just 168
    (89, 10) -> Just 156
    (89, 13) -> Just 157
    (90, 8) -> Just 167
    (90, 9) -> Just 168
    (90, 10) -> Just 166
    (90, 13) -> Just 157
    (91, 8) -> Just 297
    (91, 40) -> Just 299
    (92, 8) -> Just 297
    (92, 40) -> Just 349
    (92, 53) -> Just 340
    (92, 54) -> Just 347
    (93, 8) -> Just 297
    (93, 40) -> Just 349
    (93, 53) -> Just 346
    (93, 54) -> Just 347
    (94, 8) -> Just 228
    (95, 8) -> Just 239
    (96, 8) -> Just 240
    (97, 8) -> Just 241
    (107, 9) -> Just 326
    (107, 45) -> Just 317
    (107, 46) -> Just 318
    (107, 47) -> Just 319
    (108, 9) -> Just 326
    (108, 17) -> Just 109
    (108, 45) -> Just 219
    (108, 46) -> Just 318
    (108, 47) -> Just 319
    (109, 9) -> Just 326
    (109, 23) -> Just 211
    (109, 45) -> Just 220
    (109, 46) -> Just 318
    (109, 47) -> Just 319
    (110, 9) -> Just 326
    (110, 17) -> Just 111
    (110, 45) -> Just 219
    (110, 46) -> Just 318
    (110, 47) -> Just 319
    (111, 9) -> Just 326
    (111, 24) -> Just 213
    (111, 45) -> Just 220
    (111, 46) -> Just 318
    (111, 47) -> Just 319
    (112, 9) -> Just 326
    (112, 17) -> Just 113
    (112, 45) -> Just 219
    (112, 46) -> Just 318
    (112, 47) -> Just 319
    (113, 9) -> Just 326
    (113, 23) -> Just 210
    (113, 45) -> Just 220
    (113, 46) -> Just 318
    (113, 47) -> Just 319
    (114, 9) -> Just 326
    (114, 17) -> Just 115
    (114, 45) -> Just 219
    (114, 46) -> Just 318
    (114, 47) -> Just 319
    (115, 9) -> Just 326
    (115, 24) -> Just 212
    (115, 45) -> Just 220
    (115, 46) -> Just 318
    (115, 47) -> Just 319
    (116, 9) -> Just 326
    (116, 17) -> Just 117
    (116, 18) -> Just 369
    (116, 45) -> Just 219
    (116, 46) -> Just 318
    (116, 47) -> Just 319
    (117, 9) -> Just 326
    (117, 45) -> Just 220
    (117, 46) -> Just 318
    (117, 47) -> Just 319
    (118, 9) -> Just 326
    (118, 17) -> Just 120
    (118, 18) -> Just 221
    (118, 45) -> Just 219
    (118, 46) -> Just 318
    (118, 47) -> Just 319
    (119, 9) -> Just 326
    (119, 17) -> Just 120
    (119, 18) -> Just 368
    (119, 45) -> Just 219
    (119, 46) -> Just 318
    (119, 47) -> Just 319
    (120, 9) -> Just 326
    (120, 45) -> Just 220
    (120, 46) -> Just 318
    (120, 47) -> Just 319
    (121, 9) -> Just 326
    (121, 17) -> Just 122
    (121, 45) -> Just 219
    (121, 46) -> Just 318
    (121, 47) -> Just 319
    (122, 9) -> Just 326
    (122, 19) -> Just 198
    (122, 45) -> Just 220
    (122, 46) -> Just 318
    (122, 47) -> Just 319
    (123, 9) -> Just 326
    (123, 17) -> Just 124
    (123, 45) -> Just 219
    (123, 46) -> Just 318
    (123, 47) -> Just 319
    (124, 9) -> Just 326
    (124, 19) -> Just 199
    (124, 45) -> Just 220
    (124, 46) -> Just 318
    (124, 47) -> Just 319
    (125, 9) -> Just 327
    (125, 17) -> Just 128
    (125, 45) -> Just 219
    (125, 46) -> Just 318
    (125, 47) -> Just 319
    (125, 50) -> Just 222
    (125, 51) -> Just 337
    (126, 9) -> Just 327
    (126, 17) -> Just 128
    (126, 45) -> Just 219
    (126, 46) -> Just 318
    (126, 47) -> Just 319
    (126, 50) -> Just 336
    (126, 51) -> Just 337
    (127, 9) -> Just 140
    (128, 9) -> Just 326
    (128, 45) -> Just 220
    (128, 46) -> Just 318
    (128, 47) -> Just 319
    (128, 52) -> Just 129
    (129, 9) -> Just 326
    (129, 17) -> Just 130
    (129, 45) -> Just 219
    (129, 46) -> Just 318
    (129, 47) -> Just 319
    (130, 9) -> Just 326
    (130, 45) -> Just 220
    (130, 46) -> Just 318
    (130, 47) -> Just 319
    (131, 9) -> Just 326
    (131, 17) -> Just 132
    (131, 18) -> Just 267
    (131, 45) -> Just 219
    (131, 46) -> Just 318
    (131, 47) -> Just 319
    (132, 9) -> Just 326
    (132, 45) -> Just 220
    (132, 46) -> Just 318
    (132, 47) -> Just 319
    (133, 9) -> Just 326
    (133, 17) -> Just 120
    (133, 18) -> Just 197
    (133, 45) -> Just 219
    (133, 46) -> Just 318
    (133, 47) -> Just 319
    (134, 9) -> Just 326
    (134, 17) -> Just 120
    (134, 18) -> Just 242
    (134, 45) -> Just 219
    (134, 46) -> Just 318
    (134, 47) -> Just 319
    (135, 9) -> Just 326
    (135, 17) -> Just 120
    (135, 18) -> Just 243
    (135, 45) -> Just 219
    (135, 46) -> Just 318
    (135, 47) -> Just 319
    (136, 9) -> Just 326
    (136, 17) -> Just 120
    (136, 18) -> Just 244
    (136, 45) -> Just 219
    (136, 46) -> Just 318
    (136, 47) -> Just 319
    (137, 9) -> Just 326
    (137, 17) -> Just 120
    (137, 18) -> Just 266
    (137, 45) -> Just 219
    (137, 46) -> Just 318
    (137, 47) -> Just 319
    (138, 9) -> Just 326
    (138, 17) -> Just 120
    (138, 18) -> Just 348
    (138, 45) -> Just 219
    (138, 46) -> Just 318
    (138, 47) -> Just 319
    (139, 9) -> Just 326
    (139, 17) -> Just 120
    (139, 18) -> Just 229
    (139, 45) -> Just 219
    (139, 46) -> Just 318
    (139, 47) -> Just 319
    (140, 9) -> Just 326
    (140, 45) -> Just 230
    (140, 46) -> Just 318
    (140, 47) -> Just 319
    (141, 9) -> Just 326
    (141, 17) -> Just 142
    (141, 45) -> Just 219
    (141, 46) -> Just 318
    (141, 47) -> Just 319
    (142, 9) -> Just 326
    (142, 22) -> Just 209
    (142, 45) -> Just 220
    (142, 46) -> Just 318
    (142, 47) -> Just 319
    (143, 9) -> Just 326
    (143, 17) -> Just 145
    (143, 45) -> Just 219
    (143, 46) -> Just 318
    (143, 47) -> Just 319
    (144, 9) -> Just 326
    (144, 17) -> Just 146
    (144, 45) -> Just 219
    (144, 46) -> Just 318
    (144, 47) -> Just 319
    (145, 9) -> Just 326
    (145, 45) -> Just 220
    (145, 46) -> Just 318
    (145, 47) -> Just 319
    (146, 9) -> Just 326
    (146, 22) -> Just 208
    (146, 45) -> Just 220
    (146, 46) -> Just 318
    (146, 47) -> Just 319
    (147, 9) -> Just 326
    (147, 17) -> Just 120
    (147, 18) -> Just 315
    (147, 45) -> Just 219
    (147, 46) -> Just 318
    (147, 47) -> Just 319
    (147, 48) -> Just 320
    (147, 49) -> Just 328
    (148, 9) -> Just 326
    (148, 17) -> Just 120
    (148, 18) -> Just 235
    (148, 25) -> Just 214
    (148, 45) -> Just 219
    (148, 46) -> Just 318
    (148, 47) -> Just 319
    (149, 9) -> Just 326
    (149, 17) -> Just 120
    (149, 18) -> Just 235
    (149, 25) -> Just 236
    (149, 45) -> Just 219
    (149, 46) -> Just 318
    (149, 47) -> Just 319
    (150, 9) -> Just 326
    (150, 17) -> Just 120
    (150, 18) -> Just 332
    (150, 45) -> Just 219
    (150, 46) -> Just 318
    (150, 47) -> Just 319
    (150, 48) -> Just 333
    (151, 9) -> Just 326
    (151, 17) -> Just 120
    (151, 18) -> Just 316
    (151, 45) -> Just 219
    (151, 46) -> Just 318
    (151, 47) -> Just 319
    (169, 20) -> Just 225
    (169, 21) -> Just 204
    (170, 20) -> Just 225
    (170, 21) -> Just 205
    (171, 20) -> Just 225
    (171, 21) -> Just 206
    (172, 20) -> Just 225
    (172, 21) -> Just 207
    (185, 15) -> Just 8
    (187, 20) -> Just 200
    (188, 20) -> Just 201
    (189, 20) -> Just 202
    (190, 20) -> Just 203
    (192, 26) -> Just 215
    (193, 16) -> Just 196
    (223, 20) -> Just 225
    (223, 21) -> Just 226
    (231, 34) -> Just 232
    (233, 37) -> Just 234
    (237, 55) -> Just 245
    (238, 55) -> Just 246
    (245, 56) -> Just 95
    (245, 57) -> Just 247
    (246, 58) -> Just 97
    (247, 56) -> Just 96
    (248, 28) -> Just 250
    (249, 28) -> Just 251
    (255, 28) -> Just 284
    (256, 28) -> Just 285
    (257, 28) -> Just 293
    (258, 28) -> Just 294
    (259, 28) -> Just 422
    (260, 28) -> Just 360
    (261, 28) -> Just 370
    (269, 42) -> Just 270
    (270, 43) -> Just 271
    (270, 44) -> Just 309
    (270, 52) -> Just 310
    (270, 73) -> Just 311
    (304, 43) -> Just 307
    (304, 44) -> Just 309
    (304, 52) -> Just 310
    (304, 73) -> Just 311
    (305, 43) -> Just 308
    (305, 44) -> Just 309
    (305, 52) -> Just 310
    (305, 73) -> Just 311
    (334, 49) -> Just 335
    (374, 62) -> Just 383
    (375, 62) -> Just 384
    (387, 65) -> Just 395
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
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_alt $ alt_implies_pat_RARROW_exp_WHERE_decls actions (case snd (pop !! 4) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp actions (case snd (pop !! 2) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_gdpat $ gdpat_implies_patguards_RARROW_exp_PIPE_gdpat actions (case snd (pop !! 4) of { StackValue_patguards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdpat value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard actions (case snd (pop !! 0) of { StackValue_patguard value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_patguards $ patguards_implies_patguard_COMMA_patguards actions (case snd (pop !! 2) of { StackValue_patguard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_patguards value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp_LARROW_infixexp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_patguard $ patguard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_stmts $ stmts_implies actions
                    195 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    198 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    201 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    203 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    204 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    205 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    206 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    207 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    208 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    209 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    210 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    211 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    212 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    213 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    214 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    215 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    216 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    217 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    218 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    219 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    220 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    221 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    222 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    223 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    224 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    225 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    226 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    227 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    228 ->
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
  , stmts_implies =
      return $ Stmts_implies
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

