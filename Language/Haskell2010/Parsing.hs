module  Language.Haskell2010.Parsing  where
import qualified Control.Monad as Monad


type Pos = (Int, Int)
type AS = Pos
type BACKQUOTE = Pos
type CLASS = Pos
type COLON_COLON = Pos
type COMMA = Pos
type DARROW = Pos
type DATA = Pos
type DEFAULT = Pos
type DERIVING = Pos
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
  | CLASS CLASS
  | COLON_COLON COLON_COLON
  | COMMA COMMA
  | DARROW DARROW
  | DATA DATA
  | DEFAULT DEFAULT
  | DERIVING DERIVING
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
    Lexp_implies_MINUS_fexp MINUS Fexp
  | Lexp_implies_fexp Fexp
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
  | StackValue_fexp Fexp
  | StackValue_aexp Aexp
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
  , lexp_implies_MINUS_fexp :: MINUS -> Fexp -> m Lexp
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
    (11, Token (MODULE _)) -> Just (Reduce 1 210)
    (11, Token (WHERE _)) -> Just (Reduce 1 210)
    (11, Token (RBRACE _)) -> Just (Reduce 1 210)
    (11, Token (LPAREN _)) -> Just (Reduce 1 210)
    (11, Token (RPAREN _)) -> Just (Reduce 1 210)
    (11, Token (COMMA _)) -> Just (Reduce 1 210)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 210)
    (11, Token (HIDING _)) -> Just (Reduce 1 210)
    (11, Token (MINUS _)) -> Just (Reduce 1 210)
    (11, Token (QCONID _)) -> Just (Reduce 1 210)
    (11, Token (EXPORT _)) -> Just (Reduce 1 210)
    (11, Token (AS _)) -> Just (Reduce 1 210)
    (11, Token (QVARID _)) -> Just (Reduce 1 210)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 210)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 210)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 69)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 181)
    (13, Token (TYPE _)) -> Just (Shift 139)
    (13, Token (DATA _)) -> Just (Shift 117)
    (13, Token (NEWTYPE _)) -> Just (Shift 137)
    (13, Token (CLASS _)) -> Just (Shift 104)
    (13, Token (INSTANCE _)) -> Just (Shift 106)
    (13, Token (DEFAULT _)) -> Just (Shift 187)
    (13, Token (FOREIGN _)) -> Just (Shift 188)
    (13, Token (INFIXL _)) -> Just (Shift 295)
    (13, Token (INFIXR _)) -> Just (Shift 296)
    (13, Token (INFIX _)) -> Just (Shift 297)
    (13, Token (EXPORT _)) -> Just (Shift 99)
    (13, Token (AS _)) -> Just (Shift 100)
    (13, Token (QVARID _)) -> Just (Shift 101)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 69)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 181)
    (16, Token (TYPE _)) -> Just (Shift 139)
    (16, Token (DATA _)) -> Just (Shift 117)
    (16, Token (NEWTYPE _)) -> Just (Shift 137)
    (16, Token (CLASS _)) -> Just (Shift 104)
    (16, Token (INSTANCE _)) -> Just (Shift 106)
    (16, Token (DEFAULT _)) -> Just (Shift 187)
    (16, Token (FOREIGN _)) -> Just (Shift 188)
    (16, Token (INFIXL _)) -> Just (Shift 295)
    (16, Token (INFIXR _)) -> Just (Shift 296)
    (16, Token (INFIX _)) -> Just (Shift 297)
    (16, Token (EXPORT _)) -> Just (Shift 99)
    (16, Token (AS _)) -> Just (Shift 100)
    (16, Token (QVARID _)) -> Just (Shift 101)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 94)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 150)
    (19, Token (EXPORT _)) -> Just (Shift 99)
    (19, Token (AS _)) -> Just (Shift 100)
    (19, Token (QVARID _)) -> Just (Shift 101)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 94)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 150)
    (22, Token (EXPORT _)) -> Just (Shift 99)
    (22, Token (AS _)) -> Just (Shift 100)
    (22, Token (QVARID _)) -> Just (Shift 101)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 94)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 150)
    (25, Token (EXPORT _)) -> Just (Shift 99)
    (25, Token (AS _)) -> Just (Shift 100)
    (25, Token (QVARID _)) -> Just (Shift 101)
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
    (34, Token (EXPORT _)) -> Just (Shift 99)
    (34, Token (AS _)) -> Just (Shift 100)
    (34, Token (QVARID _)) -> Just (Shift 101)
    (34, Token (STRING _)) -> Just (Shift 395)
    (34, Token (LET _)) -> Just (Shift 256)
    (34, Token (LAMBDA _)) -> Just (Shift 80)
    (34, Token (IF _)) -> Just (Shift 64)
    (34, Token (INTEGER _)) -> Just (Shift 397)
    (35, Token (LPAREN _)) -> Just (Shift 51)
    (35, Token (MINUS _)) -> Just (Shift 44)
    (35, Token (EXPORT _)) -> Just (Shift 99)
    (35, Token (AS _)) -> Just (Shift 100)
    (35, Token (QVARID _)) -> Just (Shift 101)
    (35, Token (STRING _)) -> Just (Shift 395)
    (35, Token (LET _)) -> Just (Shift 256)
    (35, Token (LAMBDA _)) -> Just (Shift 80)
    (35, Token (IF _)) -> Just (Shift 64)
    (35, Token (INTEGER _)) -> Just (Shift 397)
    (36, Token (LPAREN _)) -> Just (Shift 51)
    (36, Token (MINUS _)) -> Just (Shift 44)
    (36, Token (EXPORT _)) -> Just (Shift 99)
    (36, Token (AS _)) -> Just (Shift 100)
    (36, Token (QVARID _)) -> Just (Shift 101)
    (36, Token (STRING _)) -> Just (Shift 395)
    (36, Token (LET _)) -> Just (Shift 256)
    (36, Token (LAMBDA _)) -> Just (Shift 80)
    (36, Token (IF _)) -> Just (Shift 64)
    (36, Token (INTEGER _)) -> Just (Shift 397)
    (37, Token (LPAREN _)) -> Just (Shift 51)
    (37, Token (MINUS _)) -> Just (Shift 44)
    (37, Token (EXPORT _)) -> Just (Shift 99)
    (37, Token (AS _)) -> Just (Shift 100)
    (37, Token (QVARID _)) -> Just (Shift 101)
    (37, Token (STRING _)) -> Just (Shift 395)
    (37, Token (LET _)) -> Just (Shift 256)
    (37, Token (LAMBDA _)) -> Just (Shift 80)
    (37, Token (IF _)) -> Just (Shift 64)
    (37, Token (INTEGER _)) -> Just (Shift 397)
    (38, Token (LPAREN _)) -> Just (Shift 51)
    (38, Token (MINUS _)) -> Just (Shift 44)
    (38, Token (EXPORT _)) -> Just (Shift 99)
    (38, Token (AS _)) -> Just (Shift 100)
    (38, Token (QVARID _)) -> Just (Shift 101)
    (38, Token (STRING _)) -> Just (Shift 395)
    (38, Token (LET _)) -> Just (Shift 256)
    (38, Token (LAMBDA _)) -> Just (Shift 80)
    (38, Token (IF _)) -> Just (Shift 64)
    (38, Token (INTEGER _)) -> Just (Shift 397)
    (39, Token (LPAREN _)) -> Just (Shift 51)
    (39, Token (MINUS _)) -> Just (Shift 44)
    (39, Token (EXPORT _)) -> Just (Shift 99)
    (39, Token (AS _)) -> Just (Shift 100)
    (39, Token (QVARID _)) -> Just (Shift 101)
    (39, Token (STRING _)) -> Just (Shift 395)
    (39, Token (LET _)) -> Just (Shift 256)
    (39, Token (LAMBDA _)) -> Just (Shift 80)
    (39, Token (IF _)) -> Just (Shift 64)
    (39, Token (INTEGER _)) -> Just (Shift 397)
    (40, Token (LPAREN _)) -> Just (Shift 51)
    (40, Token (MINUS _)) -> Just (Shift 44)
    (40, Token (EXPORT _)) -> Just (Shift 99)
    (40, Token (AS _)) -> Just (Shift 100)
    (40, Token (QVARID _)) -> Just (Shift 101)
    (40, Token (STRING _)) -> Just (Shift 395)
    (40, Token (LET _)) -> Just (Shift 256)
    (40, Token (LAMBDA _)) -> Just (Shift 80)
    (40, Token (IF _)) -> Just (Shift 64)
    (40, Token (INTEGER _)) -> Just (Shift 397)
    (41, Token (LPAREN _)) -> Just (Shift 51)
    (41, Token (MINUS _)) -> Just (Shift 44)
    (41, Token (EXPORT _)) -> Just (Shift 99)
    (41, Token (AS _)) -> Just (Shift 100)
    (41, Token (QVARID _)) -> Just (Shift 101)
    (41, Token (STRING _)) -> Just (Shift 395)
    (41, Token (LET _)) -> Just (Shift 256)
    (41, Token (LAMBDA _)) -> Just (Shift 80)
    (41, Token (IF _)) -> Just (Shift 64)
    (41, Token (INTEGER _)) -> Just (Shift 397)
    (42, Token (LPAREN _)) -> Just (Shift 51)
    (42, Token (MINUS _)) -> Just (Shift 44)
    (42, Token (EXPORT _)) -> Just (Shift 99)
    (42, Token (AS _)) -> Just (Shift 100)
    (42, Token (QVARID _)) -> Just (Shift 101)
    (42, Token (STRING _)) -> Just (Shift 395)
    (42, Token (LET _)) -> Just (Shift 256)
    (42, Token (LAMBDA _)) -> Just (Shift 80)
    (42, Token (IF _)) -> Just (Shift 64)
    (42, Token (INTEGER _)) -> Just (Shift 397)
    (43, Token (LPAREN _)) -> Just (Shift 51)
    (43, Token (MINUS _)) -> Just (Shift 44)
    (43, Token (EXPORT _)) -> Just (Shift 99)
    (43, Token (AS _)) -> Just (Shift 100)
    (43, Token (QVARID _)) -> Just (Shift 101)
    (43, Token (STRING _)) -> Just (Shift 395)
    (43, Token (LET _)) -> Just (Shift 256)
    (43, Token (LAMBDA _)) -> Just (Shift 80)
    (43, Token (IF _)) -> Just (Shift 64)
    (43, Token (INTEGER _)) -> Just (Shift 397)
    (44, Token (LPAREN _)) -> Just (Shift 51)
    (44, Token (EXPORT _)) -> Just (Shift 99)
    (44, Token (AS _)) -> Just (Shift 100)
    (44, Token (QVARID _)) -> Just (Shift 101)
    (44, Token (STRING _)) -> Just (Shift 395)
    (44, Token (INTEGER _)) -> Just (Shift 397)
    (45, Token (WHERE _)) -> Just (Reduce 1 168)
    (45, Token (RBRACE _)) -> Just (Reduce 1 168)
    (45, Token (LPAREN _)) -> Just (Shift 51)
    (45, Token (RPAREN _)) -> Just (Reduce 1 168)
    (45, Token (COMMA _)) -> Just (Reduce 1 168)
    (45, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (45, Token (EQUAL _)) -> Just (Reduce 1 168)
    (45, Token (PIPE _)) -> Just (Reduce 1 168)
    (45, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (45, Token (MINUS _)) -> Just (Reduce 1 168)
    (45, Token (EXPORT _)) -> Just (Shift 99)
    (45, Token (AS _)) -> Just (Shift 100)
    (45, Token (QVARID _)) -> Just (Shift 101)
    (45, Token (STRING _)) -> Just (Shift 395)
    (45, Token (LARROW _)) -> Just (Reduce 1 168)
    (45, Token (THEN _)) -> Just (Reduce 1 168)
    (45, Token (ELSE _)) -> Just (Reduce 1 168)
    (45, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (45, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (45, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (45, Token (INTEGER _)) -> Just (Shift 397)
    (46, Token (WHERE _)) -> Just (Reduce 2 167)
    (46, Token (RBRACE _)) -> Just (Reduce 2 167)
    (46, Token (LPAREN _)) -> Just (Shift 51)
    (46, Token (RPAREN _)) -> Just (Reduce 2 167)
    (46, Token (COMMA _)) -> Just (Reduce 2 167)
    (46, Token (SEMICOLON _)) -> Just (Reduce 2 167)
    (46, Token (EQUAL _)) -> Just (Reduce 2 167)
    (46, Token (PIPE _)) -> Just (Reduce 2 167)
    (46, Token (COLON_COLON _)) -> Just (Reduce 2 167)
    (46, Token (MINUS _)) -> Just (Reduce 2 167)
    (46, Token (EXPORT _)) -> Just (Shift 99)
    (46, Token (AS _)) -> Just (Shift 100)
    (46, Token (QVARID _)) -> Just (Shift 101)
    (46, Token (STRING _)) -> Just (Shift 395)
    (46, Token (LARROW _)) -> Just (Reduce 2 167)
    (46, Token (THEN _)) -> Just (Reduce 2 167)
    (46, Token (ELSE _)) -> Just (Reduce 2 167)
    (46, Token (QVARSYM _)) -> Just (Reduce 2 167)
    (46, Token (BACKQUOTE _)) -> Just (Reduce 2 167)
    (46, Token (QCONSYM _)) -> Just (Reduce 2 167)
    (46, Token (INTEGER _)) -> Just (Shift 397)
    (47, Token (LPAREN _)) -> Just (Shift 51)
    (47, Token (MINUS _)) -> Just (Shift 44)
    (47, Token (EXPORT _)) -> Just (Shift 99)
    (47, Token (AS _)) -> Just (Shift 100)
    (47, Token (QVARID _)) -> Just (Shift 101)
    (47, Token (STRING _)) -> Just (Shift 395)
    (47, Token (LET _)) -> Just (Shift 256)
    (47, Token (LAMBDA _)) -> Just (Shift 80)
    (47, Token (IF _)) -> Just (Shift 64)
    (47, Token (INTEGER _)) -> Just (Shift 397)
    (48, Token (LPAREN _)) -> Just (Shift 51)
    (48, Token (MINUS _)) -> Just (Shift 44)
    (48, Token (EXPORT _)) -> Just (Shift 99)
    (48, Token (AS _)) -> Just (Shift 100)
    (48, Token (QVARID _)) -> Just (Shift 101)
    (48, Token (STRING _)) -> Just (Shift 395)
    (48, Token (LET _)) -> Just (Shift 256)
    (48, Token (LAMBDA _)) -> Just (Shift 80)
    (48, Token (IF _)) -> Just (Shift 64)
    (48, Token (INTEGER _)) -> Just (Shift 397)
    (49, Token (LPAREN _)) -> Just (Shift 51)
    (49, Token (MINUS _)) -> Just (Shift 44)
    (49, Token (EXPORT _)) -> Just (Shift 99)
    (49, Token (AS _)) -> Just (Shift 100)
    (49, Token (QVARID _)) -> Just (Shift 101)
    (49, Token (STRING _)) -> Just (Shift 395)
    (49, Token (LET _)) -> Just (Shift 256)
    (49, Token (LAMBDA _)) -> Just (Shift 80)
    (49, Token (IF _)) -> Just (Shift 64)
    (49, Token (INTEGER _)) -> Just (Shift 397)
    (50, Token (LPAREN _)) -> Just (Shift 51)
    (50, Token (MINUS _)) -> Just (Shift 44)
    (50, Token (EXPORT _)) -> Just (Shift 99)
    (50, Token (AS _)) -> Just (Shift 100)
    (50, Token (QVARID _)) -> Just (Shift 101)
    (50, Token (STRING _)) -> Just (Shift 395)
    (50, Token (LET _)) -> Just (Shift 256)
    (50, Token (LAMBDA _)) -> Just (Shift 80)
    (50, Token (IF _)) -> Just (Shift 64)
    (50, Token (INTEGER _)) -> Just (Shift 397)
    (51, Token (LPAREN _)) -> Just (Shift 51)
    (51, Token (MINUS _)) -> Just (Shift 52)
    (51, Token (EXPORT _)) -> Just (Shift 99)
    (51, Token (AS _)) -> Just (Shift 100)
    (51, Token (QVARID _)) -> Just (Shift 101)
    (51, Token (STRING _)) -> Just (Shift 395)
    (51, Token (LET _)) -> Just (Shift 256)
    (51, Token (LAMBDA _)) -> Just (Shift 80)
    (51, Token (IF _)) -> Just (Shift 64)
    (51, Token (QVARSYM _)) -> Just (Shift 53)
    (51, Token (BACKQUOTE _)) -> Just (Shift 396)
    (51, Token (QCONSYM _)) -> Just (Shift 58)
    (51, Token (INTEGER _)) -> Just (Shift 397)
    (52, Token (LPAREN _)) -> Just (Shift 51)
    (52, Token (RPAREN _)) -> Just (Shift 96)
    (52, Token (EXPORT _)) -> Just (Shift 99)
    (52, Token (AS _)) -> Just (Shift 100)
    (52, Token (QVARID _)) -> Just (Shift 101)
    (52, Token (STRING _)) -> Just (Shift 395)
    (52, Token (INTEGER _)) -> Just (Shift 397)
    (53, Token (LPAREN _)) -> Just (Shift 51)
    (53, Token (RPAREN _)) -> Just (Shift 97)
    (53, Token (MINUS _)) -> Just (Shift 44)
    (53, Token (EXPORT _)) -> Just (Shift 99)
    (53, Token (AS _)) -> Just (Shift 100)
    (53, Token (QVARID _)) -> Just (Shift 101)
    (53, Token (STRING _)) -> Just (Shift 395)
    (53, Token (LET _)) -> Just (Shift 256)
    (53, Token (LAMBDA _)) -> Just (Shift 80)
    (53, Token (IF _)) -> Just (Shift 64)
    (53, Token (INTEGER _)) -> Just (Shift 397)
    (54, Token (LPAREN _)) -> Just (Shift 51)
    (54, Token (MINUS _)) -> Just (Shift 44)
    (54, Token (EXPORT _)) -> Just (Shift 99)
    (54, Token (AS _)) -> Just (Shift 100)
    (54, Token (QVARID _)) -> Just (Shift 101)
    (54, Token (STRING _)) -> Just (Shift 395)
    (54, Token (LET _)) -> Just (Shift 256)
    (54, Token (LAMBDA _)) -> Just (Shift 80)
    (54, Token (IF _)) -> Just (Shift 64)
    (54, Token (INTEGER _)) -> Just (Shift 397)
    (55, Token (LPAREN _)) -> Just (Shift 51)
    (55, Token (MINUS _)) -> Just (Shift 44)
    (55, Token (EXPORT _)) -> Just (Shift 99)
    (55, Token (AS _)) -> Just (Shift 100)
    (55, Token (QVARID _)) -> Just (Shift 101)
    (55, Token (STRING _)) -> Just (Shift 395)
    (55, Token (LET _)) -> Just (Shift 256)
    (55, Token (LAMBDA _)) -> Just (Shift 80)
    (55, Token (IF _)) -> Just (Shift 64)
    (55, Token (INTEGER _)) -> Just (Shift 397)
    (56, Token (LPAREN _)) -> Just (Shift 51)
    (56, Token (MINUS _)) -> Just (Shift 44)
    (56, Token (EXPORT _)) -> Just (Shift 99)
    (56, Token (AS _)) -> Just (Shift 100)
    (56, Token (QVARID _)) -> Just (Shift 101)
    (56, Token (STRING _)) -> Just (Shift 395)
    (56, Token (LET _)) -> Just (Shift 256)
    (56, Token (LAMBDA _)) -> Just (Shift 80)
    (56, Token (IF _)) -> Just (Shift 64)
    (56, Token (INTEGER _)) -> Just (Shift 397)
    (57, Token (LPAREN _)) -> Just (Shift 51)
    (57, Token (MINUS _)) -> Just (Shift 44)
    (57, Token (EXPORT _)) -> Just (Shift 99)
    (57, Token (AS _)) -> Just (Shift 100)
    (57, Token (QVARID _)) -> Just (Shift 101)
    (57, Token (STRING _)) -> Just (Shift 395)
    (57, Token (LET _)) -> Just (Shift 256)
    (57, Token (LAMBDA _)) -> Just (Shift 80)
    (57, Token (IF _)) -> Just (Shift 64)
    (57, Token (INTEGER _)) -> Just (Shift 397)
    (58, Token (LPAREN _)) -> Just (Shift 51)
    (58, Token (MINUS _)) -> Just (Shift 44)
    (58, Token (EXPORT _)) -> Just (Shift 99)
    (58, Token (AS _)) -> Just (Shift 100)
    (58, Token (QVARID _)) -> Just (Shift 101)
    (58, Token (STRING _)) -> Just (Shift 395)
    (58, Token (LET _)) -> Just (Shift 256)
    (58, Token (LAMBDA _)) -> Just (Shift 80)
    (58, Token (IF _)) -> Just (Shift 64)
    (58, Token (INTEGER _)) -> Just (Shift 397)
    (59, Token (LPAREN _)) -> Just (Shift 51)
    (59, Token (MINUS _)) -> Just (Shift 44)
    (59, Token (EXPORT _)) -> Just (Shift 99)
    (59, Token (AS _)) -> Just (Shift 100)
    (59, Token (QVARID _)) -> Just (Shift 101)
    (59, Token (STRING _)) -> Just (Shift 395)
    (59, Token (LET _)) -> Just (Shift 255)
    (59, Token (LAMBDA _)) -> Just (Shift 80)
    (59, Token (IF _)) -> Just (Shift 64)
    (59, Token (INTEGER _)) -> Just (Shift 397)
    (60, Token (LPAREN _)) -> Just (Shift 51)
    (60, Token (MINUS _)) -> Just (Shift 44)
    (60, Token (EXPORT _)) -> Just (Shift 99)
    (60, Token (AS _)) -> Just (Shift 100)
    (60, Token (QVARID _)) -> Just (Shift 101)
    (60, Token (STRING _)) -> Just (Shift 395)
    (60, Token (LET _)) -> Just (Shift 255)
    (60, Token (LAMBDA _)) -> Just (Shift 80)
    (60, Token (IF _)) -> Just (Shift 64)
    (60, Token (INTEGER _)) -> Just (Shift 397)
    (61, Token (LPAREN _)) -> Just (Shift 51)
    (61, Token (MINUS _)) -> Just (Shift 44)
    (61, Token (EXPORT _)) -> Just (Shift 99)
    (61, Token (AS _)) -> Just (Shift 100)
    (61, Token (QVARID _)) -> Just (Shift 101)
    (61, Token (STRING _)) -> Just (Shift 395)
    (61, Token (LET _)) -> Just (Shift 255)
    (61, Token (LAMBDA _)) -> Just (Shift 80)
    (61, Token (IF _)) -> Just (Shift 64)
    (61, Token (INTEGER _)) -> Just (Shift 397)
    (62, Token (LPAREN _)) -> Just (Shift 51)
    (62, Token (MINUS _)) -> Just (Shift 44)
    (62, Token (EXPORT _)) -> Just (Shift 99)
    (62, Token (AS _)) -> Just (Shift 100)
    (62, Token (QVARID _)) -> Just (Shift 101)
    (62, Token (STRING _)) -> Just (Shift 395)
    (62, Token (LET _)) -> Just (Shift 255)
    (62, Token (LAMBDA _)) -> Just (Shift 80)
    (62, Token (IF _)) -> Just (Shift 64)
    (62, Token (INTEGER _)) -> Just (Shift 397)
    (63, Token (LPAREN _)) -> Just (Shift 51)
    (63, Token (MINUS _)) -> Just (Shift 44)
    (63, Token (EXPORT _)) -> Just (Shift 99)
    (63, Token (AS _)) -> Just (Shift 100)
    (63, Token (QVARID _)) -> Just (Shift 101)
    (63, Token (STRING _)) -> Just (Shift 395)
    (63, Token (LET _)) -> Just (Shift 255)
    (63, Token (LAMBDA _)) -> Just (Shift 80)
    (63, Token (IF _)) -> Just (Shift 64)
    (63, Token (INTEGER _)) -> Just (Shift 397)
    (64, Token (LPAREN _)) -> Just (Shift 51)
    (64, Token (MINUS _)) -> Just (Shift 44)
    (64, Token (EXPORT _)) -> Just (Shift 99)
    (64, Token (AS _)) -> Just (Shift 100)
    (64, Token (QVARID _)) -> Just (Shift 101)
    (64, Token (STRING _)) -> Just (Shift 395)
    (64, Token (LET _)) -> Just (Shift 256)
    (64, Token (LAMBDA _)) -> Just (Shift 80)
    (64, Token (IF _)) -> Just (Shift 64)
    (64, Token (INTEGER _)) -> Just (Shift 397)
    (65, Token (LPAREN _)) -> Just (Shift 51)
    (65, Token (MINUS _)) -> Just (Shift 44)
    (65, Token (EXPORT _)) -> Just (Shift 99)
    (65, Token (AS _)) -> Just (Shift 100)
    (65, Token (QVARID _)) -> Just (Shift 101)
    (65, Token (STRING _)) -> Just (Shift 395)
    (65, Token (LET _)) -> Just (Shift 256)
    (65, Token (LAMBDA _)) -> Just (Shift 80)
    (65, Token (IF _)) -> Just (Shift 64)
    (65, Token (INTEGER _)) -> Just (Shift 397)
    (66, Token (LPAREN _)) -> Just (Shift 51)
    (66, Token (MINUS _)) -> Just (Shift 44)
    (66, Token (EXPORT _)) -> Just (Shift 99)
    (66, Token (AS _)) -> Just (Shift 100)
    (66, Token (QVARID _)) -> Just (Shift 101)
    (66, Token (STRING _)) -> Just (Shift 395)
    (66, Token (LET _)) -> Just (Shift 256)
    (66, Token (LAMBDA _)) -> Just (Shift 80)
    (66, Token (IF _)) -> Just (Shift 64)
    (66, Token (INTEGER _)) -> Just (Shift 397)
    (67, Token (LPAREN _)) -> Just (Shift 69)
    (67, Token (EXPORT _)) -> Just (Shift 99)
    (67, Token (AS _)) -> Just (Shift 100)
    (67, Token (QVARID _)) -> Just (Shift 101)
    (68, Token (LPAREN _)) -> Just (Shift 69)
    (68, Token (EXPORT _)) -> Just (Shift 99)
    (68, Token (AS _)) -> Just (Shift 100)
    (68, Token (QVARID _)) -> Just (Shift 101)
    (69, Token (LPAREN _)) -> Just (Shift 69)
    (69, Token (MINUS _)) -> Just (Shift 98)
    (69, Token (EXPORT _)) -> Just (Shift 99)
    (69, Token (AS _)) -> Just (Shift 100)
    (69, Token (QVARID _)) -> Just (Shift 101)
    (69, Token (QVARSYM _)) -> Just (Shift 102)
    (70, Token (LPAREN _)) -> Just (Shift 69)
    (70, Token (RPAREN _)) -> Just (Shift 406)
    (70, Token (MINUS _)) -> Just (Shift 67)
    (70, Token (EXPORT _)) -> Just (Shift 99)
    (70, Token (AS _)) -> Just (Shift 100)
    (70, Token (QVARID _)) -> Just (Shift 101)
    (70, Token (QVARSYM _)) -> Just (Shift 411)
    (70, Token (BACKQUOTE _)) -> Just (Shift 337)
    (70, Token (QCONSYM _)) -> Just (Shift 340)
    (71, Token (RBRACE _)) -> Just (Reduce 0 85)
    (71, Token (LPAREN _)) -> Just (Shift 69)
    (71, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (71, Token (INFIXL _)) -> Just (Shift 295)
    (71, Token (INFIXR _)) -> Just (Shift 296)
    (71, Token (INFIX _)) -> Just (Shift 297)
    (71, Token (EXPORT _)) -> Just (Shift 99)
    (71, Token (AS _)) -> Just (Shift 100)
    (71, Token (QVARID _)) -> Just (Shift 101)
    (72, Token (RBRACE _)) -> Just (Reduce 0 85)
    (72, Token (LPAREN _)) -> Just (Shift 69)
    (72, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (72, Token (INFIXL _)) -> Just (Shift 295)
    (72, Token (INFIXR _)) -> Just (Shift 296)
    (72, Token (INFIX _)) -> Just (Shift 297)
    (72, Token (EXPORT _)) -> Just (Shift 99)
    (72, Token (AS _)) -> Just (Shift 100)
    (72, Token (QVARID _)) -> Just (Shift 101)
    (73, Token (RBRACE _)) -> Just (Reduce 0 85)
    (73, Token (LPAREN _)) -> Just (Shift 69)
    (73, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (73, Token (INFIXL _)) -> Just (Shift 295)
    (73, Token (INFIXR _)) -> Just (Shift 296)
    (73, Token (INFIX _)) -> Just (Shift 297)
    (73, Token (EXPORT _)) -> Just (Shift 99)
    (73, Token (AS _)) -> Just (Shift 100)
    (73, Token (QVARID _)) -> Just (Shift 101)
    (74, Token (RBRACE _)) -> Just (Reduce 0 85)
    (74, Token (LPAREN _)) -> Just (Shift 69)
    (74, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (74, Token (INFIXL _)) -> Just (Shift 295)
    (74, Token (INFIXR _)) -> Just (Shift 296)
    (74, Token (INFIX _)) -> Just (Shift 297)
    (74, Token (EXPORT _)) -> Just (Shift 99)
    (74, Token (AS _)) -> Just (Shift 100)
    (74, Token (QVARID _)) -> Just (Shift 101)
    (75, Token (LPAREN _)) -> Just (Shift 69)
    (75, Token (EQUAL _)) -> Just (Shift 47)
    (75, Token (PIPE _)) -> Just (Shift 59)
    (75, Token (MINUS _)) -> Just (Shift 67)
    (75, Token (EXPORT _)) -> Just (Shift 99)
    (75, Token (AS _)) -> Just (Shift 100)
    (75, Token (QVARID _)) -> Just (Shift 101)
    (75, Token (QVARSYM _)) -> Just (Shift 411)
    (75, Token (BACKQUOTE _)) -> Just (Shift 337)
    (75, Token (QCONSYM _)) -> Just (Shift 340)
    (76, Token (RBRACE _)) -> Just (Reduce 0 80)
    (76, Token (LPAREN _)) -> Just (Shift 69)
    (76, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (76, Token (EXPORT _)) -> Just (Shift 99)
    (76, Token (AS _)) -> Just (Shift 100)
    (76, Token (QVARID _)) -> Just (Shift 101)
    (77, Token (RBRACE _)) -> Just (Reduce 0 80)
    (77, Token (LPAREN _)) -> Just (Shift 69)
    (77, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (77, Token (EXPORT _)) -> Just (Shift 99)
    (77, Token (AS _)) -> Just (Shift 100)
    (77, Token (QVARID _)) -> Just (Shift 101)
    (78, Token (LPAREN _)) -> Just (Shift 69)
    (78, Token (EQUAL _)) -> Just (Shift 49)
    (78, Token (PIPE _)) -> Just (Shift 61)
    (78, Token (MINUS _)) -> Just (Shift 67)
    (78, Token (EXPORT _)) -> Just (Shift 99)
    (78, Token (AS _)) -> Just (Shift 100)
    (78, Token (QVARID _)) -> Just (Shift 101)
    (78, Token (QVARSYM _)) -> Just (Shift 411)
    (78, Token (BACKQUOTE _)) -> Just (Shift 337)
    (78, Token (QCONSYM _)) -> Just (Shift 340)
    (79, Token (LPAREN _)) -> Just (Shift 69)
    (79, Token (EQUAL _)) -> Just (Shift 50)
    (79, Token (PIPE _)) -> Just (Shift 62)
    (79, Token (MINUS _)) -> Just (Shift 67)
    (79, Token (EXPORT _)) -> Just (Shift 99)
    (79, Token (AS _)) -> Just (Shift 100)
    (79, Token (QVARID _)) -> Just (Shift 101)
    (79, Token (QVARSYM _)) -> Just (Shift 411)
    (79, Token (BACKQUOTE _)) -> Just (Shift 337)
    (79, Token (QCONSYM _)) -> Just (Shift 340)
    (80, Token (LPAREN _)) -> Just (Shift 69)
    (80, Token (EXPORT _)) -> Just (Shift 99)
    (80, Token (AS _)) -> Just (Shift 100)
    (80, Token (QVARID _)) -> Just (Shift 101)
    (81, Token (LPAREN _)) -> Just (Shift 69)
    (81, Token (MINUS _)) -> Just (Shift 67)
    (81, Token (RARROW _)) -> Just (Shift 35)
    (81, Token (EXPORT _)) -> Just (Shift 99)
    (81, Token (AS _)) -> Just (Shift 100)
    (81, Token (QVARID _)) -> Just (Shift 101)
    (81, Token (QVARSYM _)) -> Just (Shift 411)
    (81, Token (BACKQUOTE _)) -> Just (Shift 337)
    (81, Token (QCONSYM _)) -> Just (Shift 340)
    (82, Token (LPAREN _)) -> Just (Shift 94)
    (82, Token (RPAREN _)) -> Just (Reduce 0 15)
    (82, Token (QCONID _)) -> Just (Shift 150)
    (82, Token (EXPORT _)) -> Just (Shift 99)
    (82, Token (AS _)) -> Just (Shift 100)
    (82, Token (QVARID _)) -> Just (Shift 101)
    (83, Token (LPAREN _)) -> Just (Shift 94)
    (83, Token (RPAREN _)) -> Just (Reduce 0 15)
    (83, Token (QCONID _)) -> Just (Shift 150)
    (83, Token (EXPORT _)) -> Just (Shift 99)
    (83, Token (AS _)) -> Just (Shift 100)
    (83, Token (QVARID _)) -> Just (Shift 101)
    (84, Token (LPAREN _)) -> Just (Shift 94)
    (84, Token (RPAREN _)) -> Just (Reduce 0 15)
    (84, Token (QCONID _)) -> Just (Shift 150)
    (84, Token (EXPORT _)) -> Just (Shift 99)
    (84, Token (AS _)) -> Just (Shift 100)
    (84, Token (QVARID _)) -> Just (Shift 101)
    (85, Token (LPAREN _)) -> Just (Shift 94)
    (85, Token (QCONID _)) -> Just (Shift 150)
    (85, Token (EXPORT _)) -> Just (Shift 99)
    (85, Token (AS _)) -> Just (Shift 100)
    (85, Token (QVARID _)) -> Just (Shift 101)
    (86, Token (LPAREN _)) -> Just (Shift 94)
    (86, Token (RPAREN _)) -> Just (Shift 156)
    (86, Token (DOT_DOT _)) -> Just (Shift 159)
    (86, Token (QCONID _)) -> Just (Shift 150)
    (86, Token (EXPORT _)) -> Just (Shift 99)
    (86, Token (AS _)) -> Just (Shift 100)
    (86, Token (QVARID _)) -> Just (Shift 101)
    (87, Token (LPAREN _)) -> Just (Shift 95)
    (87, Token (EXPORT _)) -> Just (Shift 99)
    (87, Token (AS _)) -> Just (Shift 100)
    (87, Token (QVARID _)) -> Just (Shift 101)
    (88, Token (RBRACE _)) -> Just (Shift 333)
    (88, Token (LPAREN _)) -> Just (Shift 95)
    (88, Token (EXPORT _)) -> Just (Shift 99)
    (88, Token (AS _)) -> Just (Shift 100)
    (88, Token (QVARID _)) -> Just (Shift 101)
    (89, Token (LPAREN _)) -> Just (Shift 95)
    (89, Token (EXPORT _)) -> Just (Shift 99)
    (89, Token (AS _)) -> Just (Shift 100)
    (89, Token (QVARID _)) -> Just (Shift 101)
    (90, Token (LPAREN _)) -> Just (Shift 95)
    (90, Token (EXPORT _)) -> Just (Shift 99)
    (90, Token (AS _)) -> Just (Shift 100)
    (90, Token (QVARID _)) -> Just (Shift 101)
    (91, Token (LPAREN _)) -> Just (Shift 95)
    (91, Token (EXPORT _)) -> Just (Shift 99)
    (91, Token (AS _)) -> Just (Shift 100)
    (91, Token (QVARID _)) -> Just (Shift 101)
    (92, Token (LPAREN _)) -> Just (Shift 95)
    (92, Token (EXPORT _)) -> Just (Shift 99)
    (92, Token (AS _)) -> Just (Shift 100)
    (92, Token (QVARID _)) -> Just (Shift 101)
    (93, Token (LPAREN _)) -> Just (Shift 95)
    (93, Token (EXPORT _)) -> Just (Shift 99)
    (93, Token (AS _)) -> Just (Shift 100)
    (93, Token (QVARID _)) -> Just (Shift 101)
    (94, Token (MINUS _)) -> Just (Shift 98)
    (94, Token (QVARSYM _)) -> Just (Shift 102)
    (94, Token (QCONSYM _)) -> Just (Shift 151)
    (95, Token (MINUS _)) -> Just (Shift 98)
    (95, Token (QVARSYM _)) -> Just (Shift 102)
    (96, Token (WHERE _)) -> Just (Reduce 3 190)
    (96, Token (LBRACE _)) -> Just (Reduce 3 190)
    (96, Token (RBRACE _)) -> Just (Reduce 3 190)
    (96, Token (LPAREN _)) -> Just (Reduce 3 190)
    (96, Token (RPAREN _)) -> Just (Reduce 3 190)
    (96, Token (COMMA _)) -> Just (Reduce 3 190)
    (96, Token (SEMICOLON _)) -> Just (Reduce 3 190)
    (96, Token (EQUAL _)) -> Just (Reduce 3 190)
    (96, Token (PIPE _)) -> Just (Reduce 3 190)
    (96, Token (COLON_COLON _)) -> Just (Reduce 3 190)
    (96, Token (MINUS _)) -> Just (Reduce 3 190)
    (96, Token (INFIXL _)) -> Just (Reduce 3 190)
    (96, Token (INFIXR _)) -> Just (Reduce 3 190)
    (96, Token (INFIX _)) -> Just (Reduce 3 190)
    (96, Token (RARROW _)) -> Just (Reduce 3 190)
    (96, Token (QCONID _)) -> Just (Reduce 3 190)
    (96, Token (EXPORT _)) -> Just (Reduce 3 190)
    (96, Token (AS _)) -> Just (Reduce 3 190)
    (96, Token (QVARID _)) -> Just (Reduce 3 190)
    (96, Token (STRING _)) -> Just (Reduce 3 190)
    (96, Token (LARROW _)) -> Just (Reduce 3 190)
    (96, Token (LET _)) -> Just (Reduce 3 190)
    (96, Token (LAMBDA _)) -> Just (Reduce 3 190)
    (96, Token (IF _)) -> Just (Reduce 3 190)
    (96, Token (THEN _)) -> Just (Reduce 3 190)
    (96, Token (ELSE _)) -> Just (Reduce 3 190)
    (96, Token (QVARSYM _)) -> Just (Reduce 3 190)
    (96, Token (BACKQUOTE _)) -> Just (Reduce 3 190)
    (96, Token (QCONSYM _)) -> Just (Reduce 3 190)
    (96, Token (INTEGER _)) -> Just (Reduce 3 190)
    (97, Token (WHERE _)) -> Just (Reduce 3 191)
    (97, Token (LBRACE _)) -> Just (Reduce 3 191)
    (97, Token (RBRACE _)) -> Just (Reduce 3 191)
    (97, Token (LPAREN _)) -> Just (Reduce 3 191)
    (97, Token (RPAREN _)) -> Just (Reduce 3 191)
    (97, Token (COMMA _)) -> Just (Reduce 3 191)
    (97, Token (SEMICOLON _)) -> Just (Reduce 3 191)
    (97, Token (EQUAL _)) -> Just (Reduce 3 191)
    (97, Token (PIPE _)) -> Just (Reduce 3 191)
    (97, Token (COLON_COLON _)) -> Just (Reduce 3 191)
    (97, Token (MINUS _)) -> Just (Reduce 3 191)
    (97, Token (INFIXL _)) -> Just (Reduce 3 191)
    (97, Token (INFIXR _)) -> Just (Reduce 3 191)
    (97, Token (INFIX _)) -> Just (Reduce 3 191)
    (97, Token (RARROW _)) -> Just (Reduce 3 191)
    (97, Token (QCONID _)) -> Just (Reduce 3 191)
    (97, Token (EXPORT _)) -> Just (Reduce 3 191)
    (97, Token (AS _)) -> Just (Reduce 3 191)
    (97, Token (QVARID _)) -> Just (Reduce 3 191)
    (97, Token (STRING _)) -> Just (Reduce 3 191)
    (97, Token (LARROW _)) -> Just (Reduce 3 191)
    (97, Token (LET _)) -> Just (Reduce 3 191)
    (97, Token (LAMBDA _)) -> Just (Reduce 3 191)
    (97, Token (IF _)) -> Just (Reduce 3 191)
    (97, Token (THEN _)) -> Just (Reduce 3 191)
    (97, Token (ELSE _)) -> Just (Reduce 3 191)
    (97, Token (QVARSYM _)) -> Just (Reduce 3 191)
    (97, Token (BACKQUOTE _)) -> Just (Reduce 3 191)
    (97, Token (QCONSYM _)) -> Just (Reduce 3 191)
    (97, Token (INTEGER _)) -> Just (Reduce 3 191)
    (98, Token (RPAREN _)) -> Just (Shift 96)
    (99, Token (WHERE _)) -> Just (Reduce 1 188)
    (99, Token (LBRACE _)) -> Just (Reduce 1 188)
    (99, Token (RBRACE _)) -> Just (Reduce 1 188)
    (99, Token (LPAREN _)) -> Just (Reduce 1 188)
    (99, Token (RPAREN _)) -> Just (Reduce 1 188)
    (99, Token (COMMA _)) -> Just (Reduce 1 188)
    (99, Token (SEMICOLON _)) -> Just (Reduce 1 188)
    (99, Token (EQUAL _)) -> Just (Reduce 1 188)
    (99, Token (PIPE _)) -> Just (Reduce 1 188)
    (99, Token (COLON_COLON _)) -> Just (Reduce 1 188)
    (99, Token (MINUS _)) -> Just (Reduce 1 188)
    (99, Token (INFIXL _)) -> Just (Reduce 1 188)
    (99, Token (INFIXR _)) -> Just (Reduce 1 188)
    (99, Token (INFIX _)) -> Just (Reduce 1 188)
    (99, Token (RARROW _)) -> Just (Reduce 1 188)
    (99, Token (QCONID _)) -> Just (Reduce 1 188)
    (99, Token (EXPORT _)) -> Just (Reduce 1 188)
    (99, Token (AS _)) -> Just (Reduce 1 188)
    (99, Token (QVARID _)) -> Just (Reduce 1 188)
    (99, Token (STRING _)) -> Just (Reduce 1 188)
    (99, Token (LARROW _)) -> Just (Reduce 1 188)
    (99, Token (LET _)) -> Just (Reduce 1 188)
    (99, Token (LAMBDA _)) -> Just (Reduce 1 188)
    (99, Token (IF _)) -> Just (Reduce 1 188)
    (99, Token (THEN _)) -> Just (Reduce 1 188)
    (99, Token (ELSE _)) -> Just (Reduce 1 188)
    (99, Token (QVARSYM _)) -> Just (Reduce 1 188)
    (99, Token (BACKQUOTE _)) -> Just (Reduce 1 188)
    (99, Token (QCONSYM _)) -> Just (Reduce 1 188)
    (99, Token (INTEGER _)) -> Just (Reduce 1 188)
    (100, Token (WHERE _)) -> Just (Reduce 1 187)
    (100, Token (LBRACE _)) -> Just (Reduce 1 187)
    (100, Token (RBRACE _)) -> Just (Reduce 1 187)
    (100, Token (LPAREN _)) -> Just (Reduce 1 187)
    (100, Token (RPAREN _)) -> Just (Reduce 1 187)
    (100, Token (COMMA _)) -> Just (Reduce 1 187)
    (100, Token (SEMICOLON _)) -> Just (Reduce 1 187)
    (100, Token (EQUAL _)) -> Just (Reduce 1 187)
    (100, Token (PIPE _)) -> Just (Reduce 1 187)
    (100, Token (COLON_COLON _)) -> Just (Reduce 1 187)
    (100, Token (MINUS _)) -> Just (Reduce 1 187)
    (100, Token (INFIXL _)) -> Just (Reduce 1 187)
    (100, Token (INFIXR _)) -> Just (Reduce 1 187)
    (100, Token (INFIX _)) -> Just (Reduce 1 187)
    (100, Token (RARROW _)) -> Just (Reduce 1 187)
    (100, Token (QCONID _)) -> Just (Reduce 1 187)
    (100, Token (EXPORT _)) -> Just (Reduce 1 187)
    (100, Token (AS _)) -> Just (Reduce 1 187)
    (100, Token (QVARID _)) -> Just (Reduce 1 187)
    (100, Token (STRING _)) -> Just (Reduce 1 187)
    (100, Token (LARROW _)) -> Just (Reduce 1 187)
    (100, Token (LET _)) -> Just (Reduce 1 187)
    (100, Token (LAMBDA _)) -> Just (Reduce 1 187)
    (100, Token (IF _)) -> Just (Reduce 1 187)
    (100, Token (THEN _)) -> Just (Reduce 1 187)
    (100, Token (ELSE _)) -> Just (Reduce 1 187)
    (100, Token (QVARSYM _)) -> Just (Reduce 1 187)
    (100, Token (BACKQUOTE _)) -> Just (Reduce 1 187)
    (100, Token (QCONSYM _)) -> Just (Reduce 1 187)
    (100, Token (INTEGER _)) -> Just (Reduce 1 187)
    (101, Token (WHERE _)) -> Just (Reduce 1 189)
    (101, Token (LBRACE _)) -> Just (Reduce 1 189)
    (101, Token (RBRACE _)) -> Just (Reduce 1 189)
    (101, Token (LPAREN _)) -> Just (Reduce 1 189)
    (101, Token (RPAREN _)) -> Just (Reduce 1 189)
    (101, Token (COMMA _)) -> Just (Reduce 1 189)
    (101, Token (SEMICOLON _)) -> Just (Reduce 1 189)
    (101, Token (EQUAL _)) -> Just (Reduce 1 189)
    (101, Token (PIPE _)) -> Just (Reduce 1 189)
    (101, Token (COLON_COLON _)) -> Just (Reduce 1 189)
    (101, Token (MINUS _)) -> Just (Reduce 1 189)
    (101, Token (INFIXL _)) -> Just (Reduce 1 189)
    (101, Token (INFIXR _)) -> Just (Reduce 1 189)
    (101, Token (INFIX _)) -> Just (Reduce 1 189)
    (101, Token (RARROW _)) -> Just (Reduce 1 189)
    (101, Token (QCONID _)) -> Just (Reduce 1 189)
    (101, Token (EXPORT _)) -> Just (Reduce 1 189)
    (101, Token (AS _)) -> Just (Reduce 1 189)
    (101, Token (QVARID _)) -> Just (Reduce 1 189)
    (101, Token (STRING _)) -> Just (Reduce 1 189)
    (101, Token (LARROW _)) -> Just (Reduce 1 189)
    (101, Token (LET _)) -> Just (Reduce 1 189)
    (101, Token (LAMBDA _)) -> Just (Reduce 1 189)
    (101, Token (IF _)) -> Just (Reduce 1 189)
    (101, Token (THEN _)) -> Just (Reduce 1 189)
    (101, Token (ELSE _)) -> Just (Reduce 1 189)
    (101, Token (QVARSYM _)) -> Just (Reduce 1 189)
    (101, Token (BACKQUOTE _)) -> Just (Reduce 1 189)
    (101, Token (QCONSYM _)) -> Just (Reduce 1 189)
    (101, Token (INTEGER _)) -> Just (Reduce 1 189)
    (102, Token (RPAREN _)) -> Just (Shift 97)
    (103, Token (LPAREN _)) -> Just (Shift 143)
    (103, Token (LBRACKET _)) -> Just (Shift 147)
    (103, Token (EXCL _)) -> Just (Shift 103)
    (103, Token (QCONID _)) -> Just (Shift 150)
    (103, Token (EXPORT _)) -> Just (Shift 324)
    (103, Token (AS _)) -> Just (Shift 325)
    (103, Token (QVARID _)) -> Just (Shift 326)
    (104, Token (LPAREN _)) -> Just (Shift 143)
    (104, Token (LBRACKET _)) -> Just (Shift 147)
    (104, Token (EXCL _)) -> Just (Shift 103)
    (104, Token (QCONID _)) -> Just (Shift 150)
    (104, Token (EXPORT _)) -> Just (Shift 324)
    (104, Token (AS _)) -> Just (Shift 325)
    (104, Token (QVARID _)) -> Just (Shift 326)
    (105, Token (WHERE _)) -> Just (Shift 227)
    (105, Token (RBRACE _)) -> Just (Reduce 0 65)
    (105, Token (LPAREN _)) -> Just (Shift 143)
    (105, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (105, Token (DARROW _)) -> Just (Shift 108)
    (105, Token (LBRACKET _)) -> Just (Shift 147)
    (105, Token (EXCL _)) -> Just (Shift 103)
    (105, Token (QCONID _)) -> Just (Shift 150)
    (105, Token (EXPORT _)) -> Just (Shift 324)
    (105, Token (AS _)) -> Just (Shift 325)
    (105, Token (QVARID _)) -> Just (Shift 326)
    (106, Token (LPAREN _)) -> Just (Shift 143)
    (106, Token (LBRACKET _)) -> Just (Shift 147)
    (106, Token (EXCL _)) -> Just (Shift 103)
    (106, Token (QCONID _)) -> Just (Shift 150)
    (106, Token (EXPORT _)) -> Just (Shift 324)
    (106, Token (AS _)) -> Just (Shift 325)
    (106, Token (QVARID _)) -> Just (Shift 326)
    (107, Token (WHERE _)) -> Just (Shift 229)
    (107, Token (RBRACE _)) -> Just (Reduce 0 75)
    (107, Token (LPAREN _)) -> Just (Shift 143)
    (107, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (107, Token (DARROW _)) -> Just (Shift 110)
    (107, Token (LBRACKET _)) -> Just (Shift 147)
    (107, Token (EXCL _)) -> Just (Shift 103)
    (107, Token (QCONID _)) -> Just (Shift 150)
    (107, Token (EXPORT _)) -> Just (Shift 324)
    (107, Token (AS _)) -> Just (Shift 325)
    (107, Token (QVARID _)) -> Just (Shift 326)
    (108, Token (LPAREN _)) -> Just (Shift 143)
    (108, Token (LBRACKET _)) -> Just (Shift 147)
    (108, Token (EXCL _)) -> Just (Shift 103)
    (108, Token (QCONID _)) -> Just (Shift 150)
    (108, Token (EXPORT _)) -> Just (Shift 324)
    (108, Token (AS _)) -> Just (Shift 325)
    (108, Token (QVARID _)) -> Just (Shift 326)
    (109, Token (WHERE _)) -> Just (Shift 227)
    (109, Token (RBRACE _)) -> Just (Reduce 0 65)
    (109, Token (LPAREN _)) -> Just (Shift 143)
    (109, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (109, Token (LBRACKET _)) -> Just (Shift 147)
    (109, Token (EXCL _)) -> Just (Shift 103)
    (109, Token (QCONID _)) -> Just (Shift 150)
    (109, Token (EXPORT _)) -> Just (Shift 324)
    (109, Token (AS _)) -> Just (Shift 325)
    (109, Token (QVARID _)) -> Just (Shift 326)
    (110, Token (LPAREN _)) -> Just (Shift 143)
    (110, Token (LBRACKET _)) -> Just (Shift 147)
    (110, Token (EXCL _)) -> Just (Shift 103)
    (110, Token (QCONID _)) -> Just (Shift 150)
    (110, Token (EXPORT _)) -> Just (Shift 324)
    (110, Token (AS _)) -> Just (Shift 325)
    (110, Token (QVARID _)) -> Just (Shift 326)
    (111, Token (WHERE _)) -> Just (Shift 229)
    (111, Token (RBRACE _)) -> Just (Reduce 0 75)
    (111, Token (LPAREN _)) -> Just (Shift 143)
    (111, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (111, Token (LBRACKET _)) -> Just (Shift 147)
    (111, Token (EXCL _)) -> Just (Shift 103)
    (111, Token (QCONID _)) -> Just (Shift 150)
    (111, Token (EXPORT _)) -> Just (Shift 324)
    (111, Token (AS _)) -> Just (Shift 325)
    (111, Token (QVARID _)) -> Just (Shift 326)
    (112, Token (LPAREN _)) -> Just (Shift 143)
    (112, Token (LBRACKET _)) -> Just (Shift 147)
    (112, Token (EXCL _)) -> Just (Shift 103)
    (112, Token (QCONID _)) -> Just (Shift 150)
    (112, Token (EXPORT _)) -> Just (Shift 324)
    (112, Token (AS _)) -> Just (Shift 325)
    (112, Token (QVARID _)) -> Just (Shift 326)
    (113, Token (WHERE _)) -> Just (Reduce 1 100)
    (113, Token (RBRACE _)) -> Just (Reduce 1 100)
    (113, Token (LPAREN _)) -> Just (Shift 143)
    (113, Token (RPAREN _)) -> Just (Reduce 1 100)
    (113, Token (COMMA _)) -> Just (Reduce 1 100)
    (113, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (113, Token (EQUAL _)) -> Just (Reduce 1 100)
    (113, Token (DARROW _)) -> Just (Shift 115)
    (113, Token (PIPE _)) -> Just (Reduce 1 100)
    (113, Token (RARROW _)) -> Just (Shift 114)
    (113, Token (LBRACKET _)) -> Just (Shift 147)
    (113, Token (EXCL _)) -> Just (Shift 103)
    (113, Token (QCONID _)) -> Just (Shift 150)
    (113, Token (EXPORT _)) -> Just (Shift 324)
    (113, Token (AS _)) -> Just (Shift 325)
    (113, Token (QVARID _)) -> Just (Shift 326)
    (113, Token (LARROW _)) -> Just (Reduce 1 100)
    (113, Token (THEN _)) -> Just (Reduce 1 100)
    (113, Token (ELSE _)) -> Just (Reduce 1 100)
    (114, Token (LPAREN _)) -> Just (Shift 143)
    (114, Token (LBRACKET _)) -> Just (Shift 147)
    (114, Token (EXCL _)) -> Just (Shift 103)
    (114, Token (QCONID _)) -> Just (Shift 150)
    (114, Token (EXPORT _)) -> Just (Shift 324)
    (114, Token (AS _)) -> Just (Shift 325)
    (114, Token (QVARID _)) -> Just (Shift 326)
    (115, Token (LPAREN _)) -> Just (Shift 143)
    (115, Token (LBRACKET _)) -> Just (Shift 147)
    (115, Token (EXCL _)) -> Just (Shift 103)
    (115, Token (QCONID _)) -> Just (Shift 150)
    (115, Token (EXPORT _)) -> Just (Shift 324)
    (115, Token (AS _)) -> Just (Shift 325)
    (115, Token (QVARID _)) -> Just (Shift 326)
    (116, Token (WHERE _)) -> Just (Reduce 1 100)
    (116, Token (RBRACE _)) -> Just (Reduce 1 100)
    (116, Token (LPAREN _)) -> Just (Shift 143)
    (116, Token (RPAREN _)) -> Just (Reduce 1 100)
    (116, Token (COMMA _)) -> Just (Reduce 1 100)
    (116, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (116, Token (EQUAL _)) -> Just (Reduce 1 100)
    (116, Token (PIPE _)) -> Just (Reduce 1 100)
    (116, Token (RARROW _)) -> Just (Shift 114)
    (116, Token (LBRACKET _)) -> Just (Shift 147)
    (116, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (116, Token (EXCL _)) -> Just (Shift 103)
    (116, Token (QCONID _)) -> Just (Shift 150)
    (116, Token (EXPORT _)) -> Just (Shift 324)
    (116, Token (AS _)) -> Just (Shift 325)
    (116, Token (QVARID _)) -> Just (Shift 326)
    (116, Token (LARROW _)) -> Just (Reduce 1 100)
    (116, Token (THEN _)) -> Just (Reduce 1 100)
    (116, Token (ELSE _)) -> Just (Reduce 1 100)
    (117, Token (LPAREN _)) -> Just (Shift 143)
    (117, Token (LBRACKET _)) -> Just (Shift 147)
    (117, Token (EXCL _)) -> Just (Shift 103)
    (117, Token (QCONID _)) -> Just (Shift 150)
    (117, Token (EXPORT _)) -> Just (Shift 324)
    (117, Token (AS _)) -> Just (Shift 325)
    (117, Token (QVARID _)) -> Just (Shift 326)
    (118, Token (RBRACE _)) -> Just (Reduce 0 119)
    (118, Token (LPAREN _)) -> Just (Shift 143)
    (118, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (118, Token (EQUAL _)) -> Just (Shift 121)
    (118, Token (DERIVING _)) -> Just (Reduce 0 119)
    (118, Token (DARROW _)) -> Just (Shift 119)
    (118, Token (LBRACKET _)) -> Just (Shift 147)
    (118, Token (EXCL _)) -> Just (Shift 103)
    (118, Token (QCONID _)) -> Just (Shift 150)
    (118, Token (EXPORT _)) -> Just (Shift 324)
    (118, Token (AS _)) -> Just (Shift 325)
    (118, Token (QVARID _)) -> Just (Shift 326)
    (119, Token (LPAREN _)) -> Just (Shift 143)
    (119, Token (LBRACKET _)) -> Just (Shift 147)
    (119, Token (EXCL _)) -> Just (Shift 103)
    (119, Token (QCONID _)) -> Just (Shift 150)
    (119, Token (EXPORT _)) -> Just (Shift 324)
    (119, Token (AS _)) -> Just (Shift 325)
    (119, Token (QVARID _)) -> Just (Shift 326)
    (120, Token (RBRACE _)) -> Just (Reduce 0 119)
    (120, Token (LPAREN _)) -> Just (Shift 143)
    (120, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (120, Token (EQUAL _)) -> Just (Shift 121)
    (120, Token (DERIVING _)) -> Just (Reduce 0 119)
    (120, Token (LBRACKET _)) -> Just (Shift 147)
    (120, Token (EXCL _)) -> Just (Shift 103)
    (120, Token (QCONID _)) -> Just (Shift 150)
    (120, Token (EXPORT _)) -> Just (Shift 324)
    (120, Token (AS _)) -> Just (Shift 325)
    (120, Token (QVARID _)) -> Just (Shift 326)
    (121, Token (LPAREN _)) -> Just (Shift 143)
    (121, Token (LBRACKET _)) -> Just (Shift 147)
    (121, Token (EXCL _)) -> Just (Shift 103)
    (121, Token (QCONID _)) -> Just (Shift 150)
    (121, Token (EXPORT _)) -> Just (Shift 324)
    (121, Token (AS _)) -> Just (Shift 325)
    (121, Token (QVARID _)) -> Just (Shift 326)
    (122, Token (LPAREN _)) -> Just (Shift 143)
    (122, Token (LBRACKET _)) -> Just (Shift 147)
    (122, Token (EXCL _)) -> Just (Shift 103)
    (122, Token (QCONID _)) -> Just (Shift 150)
    (122, Token (EXPORT _)) -> Just (Shift 324)
    (122, Token (AS _)) -> Just (Shift 325)
    (122, Token (QVARID _)) -> Just (Shift 326)
    (123, Token (LPAREN _)) -> Just (Shift 148)
    (123, Token (QCONID _)) -> Just (Shift 150)
    (124, Token (RBRACE _)) -> Just (Reduce 1 123)
    (124, Token (LPAREN _)) -> Just (Shift 143)
    (124, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (124, Token (DERIVING _)) -> Just (Reduce 1 123)
    (124, Token (PIPE _)) -> Just (Reduce 1 123)
    (124, Token (LBRACKET _)) -> Just (Shift 147)
    (124, Token (EXCL _)) -> Just (Shift 103)
    (124, Token (QCONID _)) -> Just (Shift 150)
    (124, Token (EXPORT _)) -> Just (Shift 324)
    (124, Token (AS _)) -> Just (Shift 325)
    (124, Token (QVARID _)) -> Just (Shift 326)
    (124, Token (BACKQUOTE _)) -> Just (Shift 338)
    (124, Token (QCONSYM _)) -> Just (Shift 340)
    (125, Token (LPAREN _)) -> Just (Shift 143)
    (125, Token (LBRACKET _)) -> Just (Shift 147)
    (125, Token (EXCL _)) -> Just (Shift 103)
    (125, Token (QCONID _)) -> Just (Shift 150)
    (125, Token (EXPORT _)) -> Just (Shift 324)
    (125, Token (AS _)) -> Just (Shift 325)
    (125, Token (QVARID _)) -> Just (Shift 326)
    (126, Token (RBRACE _)) -> Just (Reduce 3 124)
    (126, Token (LPAREN _)) -> Just (Shift 143)
    (126, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (126, Token (DERIVING _)) -> Just (Reduce 3 124)
    (126, Token (PIPE _)) -> Just (Reduce 3 124)
    (126, Token (LBRACKET _)) -> Just (Shift 147)
    (126, Token (EXCL _)) -> Just (Shift 103)
    (126, Token (QCONID _)) -> Just (Shift 150)
    (126, Token (EXPORT _)) -> Just (Shift 324)
    (126, Token (AS _)) -> Just (Shift 325)
    (126, Token (QVARID _)) -> Just (Shift 326)
    (127, Token (LPAREN _)) -> Just (Shift 143)
    (127, Token (LBRACKET _)) -> Just (Shift 147)
    (127, Token (EXCL _)) -> Just (Shift 103)
    (127, Token (QCONID _)) -> Just (Shift 150)
    (127, Token (EXPORT _)) -> Just (Shift 324)
    (127, Token (AS _)) -> Just (Shift 325)
    (127, Token (QVARID _)) -> Just (Shift 326)
    (128, Token (RBRACE _)) -> Just (Reduce 1 100)
    (128, Token (LPAREN _)) -> Just (Shift 143)
    (128, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (128, Token (DARROW _)) -> Just (Shift 133)
    (128, Token (RARROW _)) -> Just (Shift 114)
    (128, Token (LBRACKET _)) -> Just (Shift 147)
    (128, Token (EXCL _)) -> Just (Shift 103)
    (128, Token (QCONID _)) -> Just (Shift 150)
    (128, Token (EXPORT _)) -> Just (Shift 324)
    (128, Token (AS _)) -> Just (Shift 325)
    (128, Token (QVARID _)) -> Just (Shift 326)
    (129, Token (LPAREN _)) -> Just (Shift 143)
    (129, Token (LBRACKET _)) -> Just (Shift 147)
    (129, Token (EXCL _)) -> Just (Shift 103)
    (129, Token (QCONID _)) -> Just (Shift 150)
    (129, Token (EXPORT _)) -> Just (Shift 324)
    (129, Token (AS _)) -> Just (Shift 325)
    (129, Token (QVARID _)) -> Just (Shift 326)
    (130, Token (LPAREN _)) -> Just (Shift 143)
    (130, Token (LBRACKET _)) -> Just (Shift 147)
    (130, Token (EXCL _)) -> Just (Shift 103)
    (130, Token (QCONID _)) -> Just (Shift 150)
    (130, Token (EXPORT _)) -> Just (Shift 324)
    (130, Token (AS _)) -> Just (Shift 325)
    (130, Token (QVARID _)) -> Just (Shift 326)
    (131, Token (LPAREN _)) -> Just (Shift 143)
    (131, Token (LBRACKET _)) -> Just (Shift 147)
    (131, Token (EXCL _)) -> Just (Shift 103)
    (131, Token (QCONID _)) -> Just (Shift 150)
    (131, Token (EXPORT _)) -> Just (Shift 324)
    (131, Token (AS _)) -> Just (Shift 325)
    (131, Token (QVARID _)) -> Just (Shift 326)
    (132, Token (LPAREN _)) -> Just (Shift 143)
    (132, Token (LBRACKET _)) -> Just (Shift 147)
    (132, Token (EXCL _)) -> Just (Shift 103)
    (132, Token (QCONID _)) -> Just (Shift 150)
    (132, Token (EXPORT _)) -> Just (Shift 324)
    (132, Token (AS _)) -> Just (Shift 325)
    (132, Token (QVARID _)) -> Just (Shift 326)
    (133, Token (LPAREN _)) -> Just (Shift 143)
    (133, Token (LBRACKET _)) -> Just (Shift 147)
    (133, Token (EXCL _)) -> Just (Shift 103)
    (133, Token (QCONID _)) -> Just (Shift 150)
    (133, Token (EXPORT _)) -> Just (Shift 324)
    (133, Token (AS _)) -> Just (Shift 325)
    (133, Token (QVARID _)) -> Just (Shift 326)
    (134, Token (LPAREN _)) -> Just (Shift 143)
    (134, Token (LBRACKET _)) -> Just (Shift 147)
    (134, Token (EXCL _)) -> Just (Shift 103)
    (134, Token (QCONID _)) -> Just (Shift 150)
    (134, Token (EXPORT _)) -> Just (Shift 324)
    (134, Token (AS _)) -> Just (Shift 325)
    (134, Token (QVARID _)) -> Just (Shift 326)
    (135, Token (LPAREN _)) -> Just (Shift 143)
    (135, Token (LBRACKET _)) -> Just (Shift 147)
    (135, Token (EXCL _)) -> Just (Shift 103)
    (135, Token (QCONID _)) -> Just (Shift 150)
    (135, Token (EXPORT _)) -> Just (Shift 324)
    (135, Token (AS _)) -> Just (Shift 325)
    (135, Token (QVARID _)) -> Just (Shift 326)
    (136, Token (LBRACE _)) -> Just (Shift 90)
    (136, Token (LPAREN _)) -> Just (Shift 143)
    (136, Token (LBRACKET _)) -> Just (Shift 147)
    (136, Token (EXCL _)) -> Just (Shift 103)
    (136, Token (QCONID _)) -> Just (Shift 150)
    (136, Token (EXPORT _)) -> Just (Shift 324)
    (136, Token (AS _)) -> Just (Shift 325)
    (136, Token (QVARID _)) -> Just (Shift 326)
    (137, Token (LPAREN _)) -> Just (Shift 143)
    (137, Token (LBRACKET _)) -> Just (Shift 147)
    (137, Token (EXCL _)) -> Just (Shift 103)
    (137, Token (QCONID _)) -> Just (Shift 150)
    (137, Token (EXPORT _)) -> Just (Shift 324)
    (137, Token (AS _)) -> Just (Shift 325)
    (137, Token (QVARID _)) -> Just (Shift 326)
    (138, Token (LPAREN _)) -> Just (Shift 143)
    (138, Token (EQUAL _)) -> Just (Shift 123)
    (138, Token (DARROW _)) -> Just (Shift 140)
    (138, Token (LBRACKET _)) -> Just (Shift 147)
    (138, Token (EXCL _)) -> Just (Shift 103)
    (138, Token (QCONID _)) -> Just (Shift 150)
    (138, Token (EXPORT _)) -> Just (Shift 324)
    (138, Token (AS _)) -> Just (Shift 325)
    (138, Token (QVARID _)) -> Just (Shift 326)
    (139, Token (LPAREN _)) -> Just (Shift 143)
    (139, Token (LBRACKET _)) -> Just (Shift 147)
    (139, Token (EXCL _)) -> Just (Shift 103)
    (139, Token (QCONID _)) -> Just (Shift 150)
    (139, Token (EXPORT _)) -> Just (Shift 324)
    (139, Token (AS _)) -> Just (Shift 325)
    (139, Token (QVARID _)) -> Just (Shift 326)
    (140, Token (LPAREN _)) -> Just (Shift 143)
    (140, Token (LBRACKET _)) -> Just (Shift 147)
    (140, Token (EXCL _)) -> Just (Shift 103)
    (140, Token (QCONID _)) -> Just (Shift 150)
    (140, Token (EXPORT _)) -> Just (Shift 324)
    (140, Token (AS _)) -> Just (Shift 325)
    (140, Token (QVARID _)) -> Just (Shift 326)
    (141, Token (LPAREN _)) -> Just (Shift 143)
    (141, Token (EQUAL _)) -> Just (Shift 129)
    (141, Token (LBRACKET _)) -> Just (Shift 147)
    (141, Token (EXCL _)) -> Just (Shift 103)
    (141, Token (QCONID _)) -> Just (Shift 150)
    (141, Token (EXPORT _)) -> Just (Shift 324)
    (141, Token (AS _)) -> Just (Shift 325)
    (141, Token (QVARID _)) -> Just (Shift 326)
    (142, Token (LPAREN _)) -> Just (Shift 143)
    (142, Token (EQUAL _)) -> Just (Shift 123)
    (142, Token (LBRACKET _)) -> Just (Shift 147)
    (142, Token (EXCL _)) -> Just (Shift 103)
    (142, Token (QCONID _)) -> Just (Shift 150)
    (142, Token (EXPORT _)) -> Just (Shift 324)
    (142, Token (AS _)) -> Just (Shift 325)
    (142, Token (QVARID _)) -> Just (Shift 326)
    (143, Token (LPAREN _)) -> Just (Shift 143)
    (143, Token (RPAREN _)) -> Just (Shift 316)
    (143, Token (COMMA _)) -> Just (Shift 329)
    (143, Token (RARROW _)) -> Just (Shift 319)
    (143, Token (LBRACKET _)) -> Just (Shift 147)
    (143, Token (EXCL _)) -> Just (Shift 103)
    (143, Token (QCONID _)) -> Just (Shift 150)
    (143, Token (EXPORT _)) -> Just (Shift 324)
    (143, Token (AS _)) -> Just (Shift 325)
    (143, Token (QVARID _)) -> Just (Shift 326)
    (143, Token (QCONSYM _)) -> Just (Shift 151)
    (144, Token (LPAREN _)) -> Just (Shift 143)
    (144, Token (RPAREN _)) -> Just (Shift 173)
    (144, Token (LBRACKET _)) -> Just (Shift 147)
    (144, Token (EXCL _)) -> Just (Shift 103)
    (144, Token (QCONID _)) -> Just (Shift 150)
    (144, Token (EXPORT _)) -> Just (Shift 324)
    (144, Token (AS _)) -> Just (Shift 325)
    (144, Token (QVARID _)) -> Just (Shift 326)
    (145, Token (LPAREN _)) -> Just (Shift 143)
    (145, Token (LBRACKET _)) -> Just (Shift 147)
    (145, Token (EXCL _)) -> Just (Shift 103)
    (145, Token (QCONID _)) -> Just (Shift 150)
    (145, Token (EXPORT _)) -> Just (Shift 324)
    (145, Token (AS _)) -> Just (Shift 325)
    (145, Token (QVARID _)) -> Just (Shift 326)
    (146, Token (LPAREN _)) -> Just (Shift 143)
    (146, Token (LBRACKET _)) -> Just (Shift 147)
    (146, Token (EXCL _)) -> Just (Shift 103)
    (146, Token (QCONID _)) -> Just (Shift 150)
    (146, Token (EXPORT _)) -> Just (Shift 324)
    (146, Token (AS _)) -> Just (Shift 325)
    (146, Token (QVARID _)) -> Just (Shift 326)
    (147, Token (LPAREN _)) -> Just (Shift 143)
    (147, Token (LBRACKET _)) -> Just (Shift 147)
    (147, Token (RBRACKET _)) -> Just (Shift 320)
    (147, Token (EXCL _)) -> Just (Shift 103)
    (147, Token (QCONID _)) -> Just (Shift 150)
    (147, Token (EXPORT _)) -> Just (Shift 324)
    (147, Token (AS _)) -> Just (Shift 325)
    (147, Token (QVARID _)) -> Just (Shift 326)
    (148, Token (QCONSYM _)) -> Just (Shift 151)
    (149, Token (WHERE _)) -> Just (Reduce 3 193)
    (149, Token (LBRACE _)) -> Just (Reduce 3 193)
    (149, Token (RBRACE _)) -> Just (Reduce 3 193)
    (149, Token (LPAREN _)) -> Just (Reduce 3 193)
    (149, Token (RPAREN _)) -> Just (Reduce 3 193)
    (149, Token (COMMA _)) -> Just (Reduce 3 193)
    (149, Token (SEMICOLON _)) -> Just (Reduce 3 193)
    (149, Token (EQUAL _)) -> Just (Reduce 3 193)
    (149, Token (DERIVING _)) -> Just (Reduce 3 193)
    (149, Token (DARROW _)) -> Just (Reduce 3 193)
    (149, Token (PIPE _)) -> Just (Reduce 3 193)
    (149, Token (COLON_COLON _)) -> Just (Reduce 3 193)
    (149, Token (MINUS _)) -> Just (Reduce 3 193)
    (149, Token (INFIXL _)) -> Just (Reduce 3 193)
    (149, Token (INFIXR _)) -> Just (Reduce 3 193)
    (149, Token (INFIX _)) -> Just (Reduce 3 193)
    (149, Token (RARROW _)) -> Just (Reduce 3 193)
    (149, Token (LBRACKET _)) -> Just (Reduce 3 193)
    (149, Token (RBRACKET _)) -> Just (Reduce 3 193)
    (149, Token (EXCL _)) -> Just (Reduce 3 193)
    (149, Token (QCONID _)) -> Just (Reduce 3 193)
    (149, Token (EXPORT _)) -> Just (Reduce 3 193)
    (149, Token (AS _)) -> Just (Reduce 3 193)
    (149, Token (QVARID _)) -> Just (Reduce 3 193)
    (149, Token (LARROW _)) -> Just (Reduce 3 193)
    (149, Token (THEN _)) -> Just (Reduce 3 193)
    (149, Token (ELSE _)) -> Just (Reduce 3 193)
    (149, Token (QVARSYM _)) -> Just (Reduce 3 193)
    (149, Token (BACKQUOTE _)) -> Just (Reduce 3 193)
    (149, Token (QCONSYM _)) -> Just (Reduce 3 193)
    (149, Token (INTEGER _)) -> Just (Reduce 3 193)
    (150, Token (WHERE _)) -> Just (Reduce 1 192)
    (150, Token (LBRACE _)) -> Just (Reduce 1 192)
    (150, Token (RBRACE _)) -> Just (Reduce 1 192)
    (150, Token (LPAREN _)) -> Just (Reduce 1 192)
    (150, Token (RPAREN _)) -> Just (Reduce 1 192)
    (150, Token (COMMA _)) -> Just (Reduce 1 192)
    (150, Token (SEMICOLON _)) -> Just (Reduce 1 192)
    (150, Token (EQUAL _)) -> Just (Reduce 1 192)
    (150, Token (DERIVING _)) -> Just (Reduce 1 192)
    (150, Token (DARROW _)) -> Just (Reduce 1 192)
    (150, Token (PIPE _)) -> Just (Reduce 1 192)
    (150, Token (COLON_COLON _)) -> Just (Reduce 1 192)
    (150, Token (MINUS _)) -> Just (Reduce 1 192)
    (150, Token (INFIXL _)) -> Just (Reduce 1 192)
    (150, Token (INFIXR _)) -> Just (Reduce 1 192)
    (150, Token (INFIX _)) -> Just (Reduce 1 192)
    (150, Token (RARROW _)) -> Just (Reduce 1 192)
    (150, Token (LBRACKET _)) -> Just (Reduce 1 192)
    (150, Token (RBRACKET _)) -> Just (Reduce 1 192)
    (150, Token (EXCL _)) -> Just (Reduce 1 192)
    (150, Token (QCONID _)) -> Just (Reduce 1 192)
    (150, Token (EXPORT _)) -> Just (Reduce 1 192)
    (150, Token (AS _)) -> Just (Reduce 1 192)
    (150, Token (QVARID _)) -> Just (Reduce 1 192)
    (150, Token (LARROW _)) -> Just (Reduce 1 192)
    (150, Token (THEN _)) -> Just (Reduce 1 192)
    (150, Token (ELSE _)) -> Just (Reduce 1 192)
    (150, Token (QVARSYM _)) -> Just (Reduce 1 192)
    (150, Token (BACKQUOTE _)) -> Just (Reduce 1 192)
    (150, Token (QCONSYM _)) -> Just (Reduce 1 192)
    (150, Token (INTEGER _)) -> Just (Reduce 1 192)
    (151, Token (RPAREN _)) -> Just (Shift 149)
    (152, Token (RPAREN _)) -> Just (Reduce 3 24)
    (153, Token (RPAREN _)) -> Just (Reduce 1 23)
    (153, Token (COMMA _)) -> Just (Shift 85)
    (154, Token (RPAREN _)) -> Just (Reduce 3 17)
    (155, Token (RPAREN _)) -> Just (Reduce 1 16)
    (155, Token (COMMA _)) -> Just (Shift 82)
    (156, Token (RPAREN _)) -> Just (Reduce 3 20)
    (156, Token (COMMA _)) -> Just (Reduce 3 20)
    (157, Token (RPAREN _)) -> Just (Reduce 4 21)
    (157, Token (COMMA _)) -> Just (Reduce 4 21)
    (158, Token (RPAREN _)) -> Just (Reduce 4 22)
    (158, Token (COMMA _)) -> Just (Reduce 4 22)
    (159, Token (RPAREN _)) -> Just (Shift 157)
    (160, Token (RPAREN _)) -> Just (Reduce 1 18)
    (160, Token (COMMA _)) -> Just (Reduce 1 18)
    (161, Token (LPAREN _)) -> Just (Shift 86)
    (161, Token (RPAREN _)) -> Just (Reduce 1 19)
    (161, Token (COMMA _)) -> Just (Reduce 1 19)
    (162, Token (RPAREN _)) -> Just (Shift 158)
    (163, Token (RPAREN _)) -> Just (Reduce 1 25)
    (163, Token (COMMA _)) -> Just (Reduce 1 25)
    (164, Token (RPAREN _)) -> Just (Reduce 1 26)
    (164, Token (COMMA _)) -> Just (Reduce 1 26)
    (165, Token (RPAREN _)) -> Just (Shift 169)
    (165, Token (QCONID _)) -> Just (Shift 220)
    (166, Token (RPAREN _)) -> Just (Shift 170)
    (166, Token (QCONID _)) -> Just (Shift 220)
    (167, Token (RPAREN _)) -> Just (Shift 171)
    (167, Token (QCONID _)) -> Just (Shift 220)
    (168, Token (RPAREN _)) -> Just (Shift 172)
    (168, Token (QCONID _)) -> Just (Shift 220)
    (169, Token (RBRACE _)) -> Just (Reduce 6 35)
    (169, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (170, Token (RBRACE _)) -> Just (Reduce 8 39)
    (170, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (171, Token (RBRACE _)) -> Just (Reduce 8 47)
    (171, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (172, Token (RBRACE _)) -> Just (Reduce 6 43)
    (172, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (173, Token (RBRACE _)) -> Just (Reduce 3 53)
    (173, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (174, Token (RBRACE _)) -> Just (Reduce 8 31)
    (174, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (175, Token (RBRACE _)) -> Just (Reduce 7 30)
    (175, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (176, Token (RBRACE _)) -> Just (Reduce 7 36)
    (176, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (177, Token (RBRACE _)) -> Just (Reduce 9 40)
    (177, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (178, Token (RBRACE _)) -> Just (Reduce 9 48)
    (178, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (179, Token (RBRACE _)) -> Just (Reduce 7 44)
    (179, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (180, Token (RBRACE _)) -> Just (Reduce 4 54)
    (180, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (181, Token (QCONID _)) -> Just (Reduce 0 204)
    (181, Token (QUALIFIED _)) -> Just (Shift 213)
    (182, Token (LPAREN _)) -> Just (Shift 83)
    (183, Token (LPAREN _)) -> Just (Shift 165)
    (183, Token (QCONID _)) -> Just (Shift 220)
    (184, Token (LPAREN _)) -> Just (Shift 166)
    (184, Token (QCONID _)) -> Just (Shift 220)
    (185, Token (LPAREN _)) -> Just (Shift 167)
    (185, Token (QCONID _)) -> Just (Shift 220)
    (186, Token (LPAREN _)) -> Just (Shift 168)
    (186, Token (QCONID _)) -> Just (Shift 220)
    (187, Token (LPAREN _)) -> Just (Shift 144)
    (188, Token (IMPORT _)) -> Just (Shift 233)
    (188, Token (EXPORT _)) -> Just (Shift 234)
    (189, Token (RBRACE _)) -> Just (Reduce 0 202)
    (189, Token (LPAREN _)) -> Just (Reduce 0 202)
    (189, Token (SEMICOLON _)) -> Just (Reduce 0 202)
    (189, Token (HIDING _)) -> Just (Reduce 0 202)
    (189, Token (AS _)) -> Just (Shift 9)
    (190, Token (RPAREN _)) -> Just (Shift 174)
    (191, Token (RPAREN _)) -> Just (Shift 175)
    (192, Token (RBRACE _)) -> Just (Reduce 4 29)
    (192, Token (LPAREN _)) -> Just (Shift 84)
    (192, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (192, Token (HIDING _)) -> Just (Shift 182)
    (193, Token (RBRACE _)) -> Just (Reduce 4 32)
    (193, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (194, Token (RBRACE _)) -> Just (Reduce 3 33)
    (194, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (194, Token (DERIVING _)) -> Just (Shift 183)
    (195, Token (RBRACE _)) -> Just (Reduce 5 37)
    (195, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (195, Token (DERIVING _)) -> Just (Shift 184)
    (196, Token (RBRACE _)) -> Just (Reduce 5 34)
    (196, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (197, Token (RBRACE _)) -> Just (Reduce 7 38)
    (197, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (198, Token (RBRACE _)) -> Just (Reduce 7 46)
    (198, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (199, Token (RBRACE _)) -> Just (Reduce 5 42)
    (199, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (200, Token (RPAREN _)) -> Just (Shift 176)
    (201, Token (RPAREN _)) -> Just (Shift 177)
    (202, Token (RPAREN _)) -> Just (Shift 178)
    (203, Token (RPAREN _)) -> Just (Shift 179)
    (204, Token (RBRACE _)) -> Just (Reduce 5 45)
    (204, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (204, Token (DERIVING _)) -> Just (Shift 185)
    (205, Token (RBRACE _)) -> Just (Reduce 3 41)
    (205, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (205, Token (DERIVING _)) -> Just (Shift 186)
    (206, Token (RBRACE _)) -> Just (Reduce 5 50)
    (206, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (207, Token (RBRACE _)) -> Just (Reduce 3 49)
    (207, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (208, Token (RBRACE _)) -> Just (Reduce 5 52)
    (208, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (209, Token (RBRACE _)) -> Just (Reduce 3 51)
    (209, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (210, Token (RPAREN _)) -> Just (Shift 180)
    (211, Token (RBRACE _)) -> Just (Reduce 2 55)
    (211, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (212, Token (RBRACE _)) -> Just (Reduce 1 56)
    (212, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (213, Token (QCONID _)) -> Just (Reduce 1 205)
    (214, Token (RBRACE _)) -> Just (Reduce 2 203)
    (214, Token (LPAREN _)) -> Just (Reduce 2 203)
    (214, Token (SEMICOLON _)) -> Just (Reduce 2 203)
    (214, Token (HIDING _)) -> Just (Reduce 2 203)
    (215, Token (WHERE _)) -> Just (Reduce 1 102)
    (215, Token (LBRACE _)) -> Just (Reduce 1 102)
    (215, Token (RBRACE _)) -> Just (Reduce 1 102)
    (215, Token (LPAREN _)) -> Just (Reduce 1 102)
    (215, Token (RPAREN _)) -> Just (Reduce 1 102)
    (215, Token (COMMA _)) -> Just (Reduce 1 102)
    (215, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (215, Token (EQUAL _)) -> Just (Reduce 1 102)
    (215, Token (DERIVING _)) -> Just (Reduce 1 102)
    (215, Token (DARROW _)) -> Just (Reduce 1 102)
    (215, Token (PIPE _)) -> Just (Reduce 1 102)
    (215, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (215, Token (MINUS _)) -> Just (Reduce 1 102)
    (215, Token (INFIXL _)) -> Just (Reduce 1 102)
    (215, Token (INFIXR _)) -> Just (Reduce 1 102)
    (215, Token (INFIX _)) -> Just (Reduce 1 102)
    (215, Token (RARROW _)) -> Just (Reduce 1 102)
    (215, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (215, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (215, Token (EXCL _)) -> Just (Reduce 1 102)
    (215, Token (QCONID _)) -> Just (Reduce 1 102)
    (215, Token (EXPORT _)) -> Just (Reduce 1 102)
    (215, Token (AS _)) -> Just (Reduce 1 102)
    (215, Token (QVARID _)) -> Just (Reduce 1 102)
    (215, Token (LARROW _)) -> Just (Reduce 1 102)
    (215, Token (THEN _)) -> Just (Reduce 1 102)
    (215, Token (ELSE _)) -> Just (Reduce 1 102)
    (215, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (215, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (215, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (215, Token (INTEGER _)) -> Just (Reduce 1 102)
    (216, Token (WHERE _)) -> Just (Reduce 2 103)
    (216, Token (LBRACE _)) -> Just (Reduce 2 103)
    (216, Token (RBRACE _)) -> Just (Reduce 2 103)
    (216, Token (LPAREN _)) -> Just (Reduce 2 103)
    (216, Token (RPAREN _)) -> Just (Reduce 2 103)
    (216, Token (COMMA _)) -> Just (Reduce 2 103)
    (216, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (216, Token (EQUAL _)) -> Just (Reduce 2 103)
    (216, Token (DERIVING _)) -> Just (Reduce 2 103)
    (216, Token (DARROW _)) -> Just (Reduce 2 103)
    (216, Token (PIPE _)) -> Just (Reduce 2 103)
    (216, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (216, Token (MINUS _)) -> Just (Reduce 2 103)
    (216, Token (INFIXL _)) -> Just (Reduce 2 103)
    (216, Token (INFIXR _)) -> Just (Reduce 2 103)
    (216, Token (INFIX _)) -> Just (Reduce 2 103)
    (216, Token (RARROW _)) -> Just (Reduce 2 103)
    (216, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (216, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (216, Token (EXCL _)) -> Just (Reduce 2 103)
    (216, Token (QCONID _)) -> Just (Reduce 2 103)
    (216, Token (EXPORT _)) -> Just (Reduce 2 103)
    (216, Token (AS _)) -> Just (Reduce 2 103)
    (216, Token (QVARID _)) -> Just (Reduce 2 103)
    (216, Token (LARROW _)) -> Just (Reduce 2 103)
    (216, Token (THEN _)) -> Just (Reduce 2 103)
    (216, Token (ELSE _)) -> Just (Reduce 2 103)
    (216, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (216, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (216, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (216, Token (INTEGER _)) -> Just (Reduce 2 103)
    (217, Token (WHERE _)) -> Just (Reduce 3 101)
    (217, Token (RBRACE _)) -> Just (Reduce 3 101)
    (217, Token (RPAREN _)) -> Just (Reduce 3 101)
    (217, Token (COMMA _)) -> Just (Reduce 3 101)
    (217, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (217, Token (EQUAL _)) -> Just (Reduce 3 101)
    (217, Token (PIPE _)) -> Just (Reduce 3 101)
    (217, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (217, Token (LARROW _)) -> Just (Reduce 3 101)
    (217, Token (THEN _)) -> Just (Reduce 3 101)
    (217, Token (ELSE _)) -> Just (Reduce 3 101)
    (218, Token (RBRACE _)) -> Just (Reduce 2 120)
    (218, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (218, Token (DERIVING _)) -> Just (Reduce 2 120)
    (219, Token (QCONID _)) -> Just (Shift 220)
    (220, Token (RBRACE _)) -> Just (Reduce 1 134)
    (220, Token (RPAREN _)) -> Just (Reduce 1 134)
    (220, Token (COMMA _)) -> Just (Reduce 1 134)
    (220, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (221, Token (RPAREN _)) -> Just (Reduce 1 132)
    (221, Token (COMMA _)) -> Just (Shift 219)
    (222, Token (RPAREN _)) -> Just (Reduce 3 133)
    (223, Token (RBRACE _)) -> Just (Reduce 7 128)
    (223, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (223, Token (DERIVING _)) -> Just (Reduce 7 128)
    (224, Token (COLON_COLON _)) -> Just (Shift 135)
    (225, Token (RBRACE _)) -> Just (Shift 223)
    (226, Token (RBRACE _)) -> Just (Reduce 3 127)
    (226, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (226, Token (DERIVING _)) -> Just (Reduce 3 127)
    (227, Token (LBRACE _)) -> Just (Shift 73)
    (228, Token (RBRACE _)) -> Just (Reduce 2 66)
    (228, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (229, Token (LBRACE _)) -> Just (Shift 76)
    (230, Token (RBRACE _)) -> Just (Reduce 2 76)
    (230, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (231, Token (RPAREN _)) -> Just (Reduce 1 98)
    (231, Token (COMMA _)) -> Just (Shift 145)
    (232, Token (RPAREN _)) -> Just (Reduce 3 99)
    (233, Token (EXPORT _)) -> Just (Shift 345)
    (233, Token (AS _)) -> Just (Shift 346)
    (233, Token (QVARID _)) -> Just (Shift 347)
    (234, Token (EXPORT _)) -> Just (Shift 345)
    (234, Token (AS _)) -> Just (Shift 346)
    (234, Token (QVARID _)) -> Just (Shift 347)
    (235, Token (COLON_COLON _)) -> Just (Shift 130)
    (236, Token (COLON_COLON _)) -> Just (Shift 131)
    (237, Token (COLON_COLON _)) -> Just (Shift 132)
    (238, Token (RBRACE _)) -> Just (Reduce 6 135)
    (238, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (239, Token (RBRACE _)) -> Just (Reduce 7 136)
    (239, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (240, Token (RBRACE _)) -> Just (Reduce 6 137)
    (240, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (241, Token (EXPORT _)) -> Just (Shift 349)
    (241, Token (AS _)) -> Just (Shift 350)
    (241, Token (QVARID _)) -> Just (Shift 351)
    (241, Token (STRING _)) -> Just (Shift 348)
    (242, Token (STRING _)) -> Just (Shift 352)
    (243, Token (STRING _)) -> Just (Shift 348)
    (244, Token (LBRACE _)) -> Just (Shift 71)
    (245, Token (LBRACE _)) -> Just (Shift 71)
    (246, Token (RBRACE _)) -> Just (Reduce 5 62)
    (246, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (247, Token (RBRACE _)) -> Just (Reduce 5 64)
    (247, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (248, Token (RBRACE _)) -> Just (Reduce 1 60)
    (248, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (249, Token (WHERE _)) -> Just (Shift 244)
    (249, Token (RBRACE _)) -> Just (Reduce 3 61)
    (249, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (250, Token (WHERE _)) -> Just (Shift 245)
    (250, Token (RBRACE _)) -> Just (Reduce 3 63)
    (250, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (251, Token (LBRACE _)) -> Just (Shift 71)
    (252, Token (LBRACE _)) -> Just (Shift 71)
    (253, Token (LBRACE _)) -> Just (Shift 71)
    (254, Token (LBRACE _)) -> Just (Shift 71)
    (255, Token (LBRACE _)) -> Just (Shift 71)
    (256, Token (LBRACE _)) -> Just (Shift 71)
    (257, Token (RBRACE _)) -> Just (Reduce 3 57)
    (257, Token (COMMA _)) -> Just (Reduce 3 57)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (257, Token (EQUAL _)) -> Just (Reduce 3 57)
    (257, Token (IN _)) -> Just (Reduce 3 57)
    (258, Token (RBRACE _)) -> Just (Shift 257)
    (259, Token (RBRACE _)) -> Just (Reduce 1 58)
    (259, Token (SEMICOLON _)) -> Just (Shift 72)
    (260, Token (RBRACE _)) -> Just (Reduce 3 59)
    (261, Token (RBRACE _)) -> Just (Reduce 5 87)
    (261, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (262, Token (RBRACE _)) -> Just (Reduce 3 86)
    (262, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (263, Token (COLON_COLON _)) -> Just (Shift 127)
    (264, Token (COMMA _)) -> Just (Reduce 0 211)
    (264, Token (MINUS _)) -> Just (Reduce 0 211)
    (264, Token (QCONID _)) -> Just (Reduce 0 211)
    (264, Token (EXPORT _)) -> Just (Reduce 0 211)
    (264, Token (AS _)) -> Just (Reduce 0 211)
    (264, Token (QVARID _)) -> Just (Reduce 0 211)
    (264, Token (QVARSYM _)) -> Just (Reduce 0 211)
    (264, Token (BACKQUOTE _)) -> Just (Reduce 0 211)
    (264, Token (QCONSYM _)) -> Just (Reduce 0 211)
    (264, Token (INTEGER _)) -> Just (Shift 298)
    (265, Token (MINUS _)) -> Just (Shift 301)
    (265, Token (QVARSYM _)) -> Just (Shift 411)
    (265, Token (BACKQUOTE _)) -> Just (Shift 337)
    (265, Token (QCONSYM _)) -> Just (Shift 340)
    (266, Token (RBRACE _)) -> Just (Reduce 3 88)
    (266, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (267, Token (LPAREN _)) -> Just (Reduce 1 181)
    (267, Token (RPAREN _)) -> Just (Reduce 1 181)
    (267, Token (EQUAL _)) -> Just (Reduce 1 181)
    (267, Token (PIPE _)) -> Just (Reduce 1 181)
    (267, Token (MINUS _)) -> Just (Reduce 1 181)
    (267, Token (RARROW _)) -> Just (Reduce 1 181)
    (267, Token (QCONID _)) -> Just (Reduce 1 181)
    (267, Token (EXPORT _)) -> Just (Reduce 1 181)
    (267, Token (AS _)) -> Just (Reduce 1 181)
    (267, Token (QVARID _)) -> Just (Reduce 1 181)
    (267, Token (QVARSYM _)) -> Just (Reduce 1 181)
    (267, Token (BACKQUOTE _)) -> Just (Reduce 1 181)
    (267, Token (QCONSYM _)) -> Just (Reduce 1 181)
    (268, Token (LPAREN _)) -> Just (Reduce 3 183)
    (268, Token (RPAREN _)) -> Just (Reduce 3 183)
    (268, Token (EQUAL _)) -> Just (Reduce 3 183)
    (268, Token (PIPE _)) -> Just (Reduce 3 183)
    (268, Token (MINUS _)) -> Just (Reduce 3 183)
    (268, Token (RARROW _)) -> Just (Reduce 3 183)
    (268, Token (QCONID _)) -> Just (Reduce 3 183)
    (268, Token (EXPORT _)) -> Just (Reduce 3 183)
    (268, Token (AS _)) -> Just (Reduce 3 183)
    (268, Token (QVARID _)) -> Just (Reduce 3 183)
    (268, Token (QVARSYM _)) -> Just (Reduce 3 183)
    (268, Token (BACKQUOTE _)) -> Just (Reduce 3 183)
    (268, Token (QCONSYM _)) -> Just (Reduce 3 183)
    (269, Token (LPAREN _)) -> Just (Reduce 2 182)
    (269, Token (RPAREN _)) -> Just (Reduce 2 182)
    (269, Token (EQUAL _)) -> Just (Reduce 2 182)
    (269, Token (PIPE _)) -> Just (Reduce 2 182)
    (269, Token (MINUS _)) -> Just (Reduce 2 182)
    (269, Token (RARROW _)) -> Just (Reduce 2 182)
    (269, Token (QCONID _)) -> Just (Reduce 2 182)
    (269, Token (EXPORT _)) -> Just (Reduce 2 182)
    (269, Token (AS _)) -> Just (Reduce 2 182)
    (269, Token (QVARID _)) -> Just (Reduce 2 182)
    (269, Token (QVARSYM _)) -> Just (Reduce 2 182)
    (269, Token (BACKQUOTE _)) -> Just (Reduce 2 182)
    (269, Token (QCONSYM _)) -> Just (Reduce 2 182)
    (270, Token (LPAREN _)) -> Just (Reduce 3 184)
    (270, Token (RPAREN _)) -> Just (Reduce 3 184)
    (270, Token (EQUAL _)) -> Just (Reduce 3 184)
    (270, Token (PIPE _)) -> Just (Reduce 3 184)
    (270, Token (MINUS _)) -> Just (Reduce 3 184)
    (270, Token (RARROW _)) -> Just (Reduce 3 184)
    (270, Token (QCONID _)) -> Just (Reduce 3 184)
    (270, Token (EXPORT _)) -> Just (Reduce 3 184)
    (270, Token (AS _)) -> Just (Reduce 3 184)
    (270, Token (QVARID _)) -> Just (Reduce 3 184)
    (270, Token (QVARSYM _)) -> Just (Reduce 3 184)
    (270, Token (BACKQUOTE _)) -> Just (Reduce 3 184)
    (270, Token (QCONSYM _)) -> Just (Reduce 3 184)
    (271, Token (WHERE _)) -> Just (Reduce 1 153)
    (271, Token (RBRACE _)) -> Just (Reduce 1 153)
    (271, Token (RPAREN _)) -> Just (Reduce 1 153)
    (271, Token (COMMA _)) -> Just (Reduce 1 153)
    (271, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (271, Token (EQUAL _)) -> Just (Reduce 1 153)
    (271, Token (PIPE _)) -> Just (Reduce 1 153)
    (271, Token (LARROW _)) -> Just (Reduce 1 153)
    (271, Token (THEN _)) -> Just (Reduce 1 153)
    (271, Token (ELSE _)) -> Just (Reduce 1 153)
    (272, Token (WHERE _)) -> Just (Reduce 3 146)
    (272, Token (RBRACE _)) -> Just (Reduce 3 146)
    (272, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (272, Token (PIPE _)) -> Just (Shift 60)
    (273, Token (WHERE _)) -> Just (Reduce 5 147)
    (273, Token (RBRACE _)) -> Just (Reduce 5 147)
    (273, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (274, Token (EQUAL _)) -> Just (Shift 48)
    (275, Token (RBRACE _)) -> Just (Reduce 3 67)
    (275, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (276, Token (RBRACE _)) -> Just (Shift 275)
    (277, Token (RBRACE _)) -> Just (Reduce 3 69)
    (278, Token (RBRACE _)) -> Just (Reduce 1 68)
    (278, Token (SEMICOLON _)) -> Just (Shift 74)
    (279, Token (RBRACE _)) -> Just (Reduce 5 72)
    (279, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (280, Token (RBRACE _)) -> Just (Reduce 5 74)
    (280, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (281, Token (RBRACE _)) -> Just (Reduce 1 70)
    (281, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (282, Token (WHERE _)) -> Just (Shift 251)
    (282, Token (RBRACE _)) -> Just (Reduce 3 71)
    (282, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (283, Token (WHERE _)) -> Just (Shift 252)
    (283, Token (RBRACE _)) -> Just (Reduce 3 73)
    (283, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (284, Token (RBRACE _)) -> Just (Reduce 3 77)
    (284, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (285, Token (RBRACE _)) -> Just (Shift 284)
    (286, Token (RBRACE _)) -> Just (Reduce 3 79)
    (287, Token (RBRACE _)) -> Just (Reduce 1 78)
    (287, Token (SEMICOLON _)) -> Just (Shift 77)
    (288, Token (RBRACE _)) -> Just (Reduce 5 82)
    (288, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (289, Token (RBRACE _)) -> Just (Reduce 5 84)
    (289, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (290, Token (WHERE _)) -> Just (Shift 253)
    (290, Token (RBRACE _)) -> Just (Reduce 3 81)
    (290, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (291, Token (WHERE _)) -> Just (Shift 254)
    (291, Token (RBRACE _)) -> Just (Reduce 3 83)
    (291, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (292, Token (COMMA _)) -> Just (Shift 87)
    (292, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (293, Token (LPAREN _)) -> Just (Reduce 1 185)
    (293, Token (COMMA _)) -> Just (Shift 87)
    (293, Token (EQUAL _)) -> Just (Reduce 1 185)
    (293, Token (PIPE _)) -> Just (Reduce 1 185)
    (293, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (293, Token (MINUS _)) -> Just (Reduce 1 185)
    (293, Token (QCONID _)) -> Just (Reduce 1 185)
    (293, Token (EXPORT _)) -> Just (Reduce 1 185)
    (293, Token (AS _)) -> Just (Reduce 1 185)
    (293, Token (QVARID _)) -> Just (Reduce 1 185)
    (293, Token (QVARSYM _)) -> Just (Reduce 1 185)
    (293, Token (BACKQUOTE _)) -> Just (Reduce 1 185)
    (293, Token (QCONSYM _)) -> Just (Reduce 1 185)
    (294, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (295, Token (COMMA _)) -> Just (Reduce 1 95)
    (295, Token (MINUS _)) -> Just (Reduce 1 95)
    (295, Token (QCONID _)) -> Just (Reduce 1 95)
    (295, Token (EXPORT _)) -> Just (Reduce 1 95)
    (295, Token (AS _)) -> Just (Reduce 1 95)
    (295, Token (QVARID _)) -> Just (Reduce 1 95)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (295, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (295, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (295, Token (INTEGER _)) -> Just (Reduce 1 95)
    (296, Token (COMMA _)) -> Just (Reduce 1 96)
    (296, Token (MINUS _)) -> Just (Reduce 1 96)
    (296, Token (QCONID _)) -> Just (Reduce 1 96)
    (296, Token (EXPORT _)) -> Just (Reduce 1 96)
    (296, Token (AS _)) -> Just (Reduce 1 96)
    (296, Token (QVARID _)) -> Just (Reduce 1 96)
    (296, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (296, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (296, Token (INTEGER _)) -> Just (Reduce 1 96)
    (297, Token (COMMA _)) -> Just (Reduce 1 97)
    (297, Token (MINUS _)) -> Just (Reduce 1 97)
    (297, Token (QCONID _)) -> Just (Reduce 1 97)
    (297, Token (EXPORT _)) -> Just (Reduce 1 97)
    (297, Token (AS _)) -> Just (Reduce 1 97)
    (297, Token (QVARID _)) -> Just (Reduce 1 97)
    (297, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (297, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (297, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (297, Token (INTEGER _)) -> Just (Reduce 1 97)
    (298, Token (COMMA _)) -> Just (Reduce 1 212)
    (298, Token (MINUS _)) -> Just (Reduce 1 212)
    (298, Token (QCONID _)) -> Just (Reduce 1 212)
    (298, Token (EXPORT _)) -> Just (Reduce 1 212)
    (298, Token (AS _)) -> Just (Reduce 1 212)
    (298, Token (QVARID _)) -> Just (Reduce 1 212)
    (298, Token (QVARSYM _)) -> Just (Reduce 1 212)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 1 212)
    (298, Token (QCONSYM _)) -> Just (Reduce 1 212)
    (299, Token (MINUS _)) -> Just (Shift 301)
    (299, Token (QVARSYM _)) -> Just (Shift 411)
    (299, Token (BACKQUOTE _)) -> Just (Shift 337)
    (299, Token (QCONSYM _)) -> Just (Shift 340)
    (300, Token (MINUS _)) -> Just (Shift 301)
    (300, Token (QVARSYM _)) -> Just (Shift 411)
    (300, Token (BACKQUOTE _)) -> Just (Shift 337)
    (300, Token (QCONSYM _)) -> Just (Shift 340)
    (301, Token (RBRACE _)) -> Just (Reduce 1 89)
    (301, Token (COMMA _)) -> Just (Shift 299)
    (301, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (302, Token (RBRACE _)) -> Just (Reduce 3 91)
    (302, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (303, Token (RBRACE _)) -> Just (Reduce 3 92)
    (303, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (304, Token (RBRACE _)) -> Just (Reduce 1 90)
    (304, Token (COMMA _)) -> Just (Shift 300)
    (304, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (305, Token (RBRACE _)) -> Just (Reduce 1 201)
    (305, Token (LPAREN _)) -> Just (Reduce 1 201)
    (305, Token (COMMA _)) -> Just (Reduce 1 201)
    (305, Token (SEMICOLON _)) -> Just (Reduce 1 201)
    (305, Token (MINUS _)) -> Just (Reduce 1 201)
    (305, Token (QCONID _)) -> Just (Reduce 1 201)
    (305, Token (EXPORT _)) -> Just (Reduce 1 201)
    (305, Token (AS _)) -> Just (Reduce 1 201)
    (305, Token (QVARID _)) -> Just (Reduce 1 201)
    (305, Token (QVARSYM _)) -> Just (Reduce 1 201)
    (305, Token (BACKQUOTE _)) -> Just (Reduce 1 201)
    (305, Token (QCONSYM _)) -> Just (Reduce 1 201)
    (306, Token (RBRACE _)) -> Just (Reduce 1 200)
    (306, Token (LPAREN _)) -> Just (Reduce 1 200)
    (306, Token (COMMA _)) -> Just (Reduce 1 200)
    (306, Token (SEMICOLON _)) -> Just (Reduce 1 200)
    (306, Token (MINUS _)) -> Just (Reduce 1 200)
    (306, Token (QCONID _)) -> Just (Reduce 1 200)
    (306, Token (EXPORT _)) -> Just (Reduce 1 200)
    (306, Token (AS _)) -> Just (Reduce 1 200)
    (306, Token (QVARID _)) -> Just (Reduce 1 200)
    (306, Token (QVARSYM _)) -> Just (Reduce 1 200)
    (306, Token (BACKQUOTE _)) -> Just (Reduce 1 200)
    (306, Token (QCONSYM _)) -> Just (Reduce 1 200)
    (307, Token (WHERE _)) -> Just (Reduce 3 108)
    (307, Token (LBRACE _)) -> Just (Reduce 3 108)
    (307, Token (RBRACE _)) -> Just (Reduce 3 108)
    (307, Token (LPAREN _)) -> Just (Reduce 3 108)
    (307, Token (RPAREN _)) -> Just (Reduce 3 108)
    (307, Token (COMMA _)) -> Just (Reduce 3 108)
    (307, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (307, Token (EQUAL _)) -> Just (Reduce 3 108)
    (307, Token (DERIVING _)) -> Just (Reduce 3 108)
    (307, Token (DARROW _)) -> Just (Reduce 3 108)
    (307, Token (PIPE _)) -> Just (Reduce 3 108)
    (307, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (307, Token (MINUS _)) -> Just (Reduce 3 108)
    (307, Token (INFIXL _)) -> Just (Reduce 3 108)
    (307, Token (INFIXR _)) -> Just (Reduce 3 108)
    (307, Token (INFIX _)) -> Just (Reduce 3 108)
    (307, Token (RARROW _)) -> Just (Reduce 3 108)
    (307, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (307, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (307, Token (EXCL _)) -> Just (Reduce 3 108)
    (307, Token (QCONID _)) -> Just (Reduce 3 108)
    (307, Token (EXPORT _)) -> Just (Reduce 3 108)
    (307, Token (AS _)) -> Just (Reduce 3 108)
    (307, Token (QVARID _)) -> Just (Reduce 3 108)
    (307, Token (LARROW _)) -> Just (Reduce 3 108)
    (307, Token (THEN _)) -> Just (Reduce 3 108)
    (307, Token (ELSE _)) -> Just (Reduce 3 108)
    (307, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (307, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (307, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (307, Token (INTEGER _)) -> Just (Reduce 3 108)
    (308, Token (WHERE _)) -> Just (Reduce 3 106)
    (308, Token (LBRACE _)) -> Just (Reduce 3 106)
    (308, Token (RBRACE _)) -> Just (Reduce 3 106)
    (308, Token (LPAREN _)) -> Just (Reduce 3 106)
    (308, Token (RPAREN _)) -> Just (Reduce 3 106)
    (308, Token (COMMA _)) -> Just (Reduce 3 106)
    (308, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (308, Token (EQUAL _)) -> Just (Reduce 3 106)
    (308, Token (DERIVING _)) -> Just (Reduce 3 106)
    (308, Token (DARROW _)) -> Just (Reduce 3 106)
    (308, Token (PIPE _)) -> Just (Reduce 3 106)
    (308, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (308, Token (MINUS _)) -> Just (Reduce 3 106)
    (308, Token (INFIXL _)) -> Just (Reduce 3 106)
    (308, Token (INFIXR _)) -> Just (Reduce 3 106)
    (308, Token (INFIX _)) -> Just (Reduce 3 106)
    (308, Token (RARROW _)) -> Just (Reduce 3 106)
    (308, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (308, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (308, Token (EXCL _)) -> Just (Reduce 3 106)
    (308, Token (QCONID _)) -> Just (Reduce 3 106)
    (308, Token (EXPORT _)) -> Just (Reduce 3 106)
    (308, Token (AS _)) -> Just (Reduce 3 106)
    (308, Token (QVARID _)) -> Just (Reduce 3 106)
    (308, Token (LARROW _)) -> Just (Reduce 3 106)
    (308, Token (THEN _)) -> Just (Reduce 3 106)
    (308, Token (ELSE _)) -> Just (Reduce 3 106)
    (308, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (308, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (308, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (308, Token (INTEGER _)) -> Just (Reduce 3 106)
    (309, Token (WHERE _)) -> Just (Reduce 3 107)
    (309, Token (LBRACE _)) -> Just (Reduce 3 107)
    (309, Token (RBRACE _)) -> Just (Reduce 3 107)
    (309, Token (LPAREN _)) -> Just (Reduce 3 107)
    (309, Token (RPAREN _)) -> Just (Reduce 3 107)
    (309, Token (COMMA _)) -> Just (Reduce 3 107)
    (309, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (309, Token (EQUAL _)) -> Just (Reduce 3 107)
    (309, Token (DERIVING _)) -> Just (Reduce 3 107)
    (309, Token (DARROW _)) -> Just (Reduce 3 107)
    (309, Token (PIPE _)) -> Just (Reduce 3 107)
    (309, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (309, Token (MINUS _)) -> Just (Reduce 3 107)
    (309, Token (INFIXL _)) -> Just (Reduce 3 107)
    (309, Token (INFIXR _)) -> Just (Reduce 3 107)
    (309, Token (INFIX _)) -> Just (Reduce 3 107)
    (309, Token (RARROW _)) -> Just (Reduce 3 107)
    (309, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (309, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (309, Token (EXCL _)) -> Just (Reduce 3 107)
    (309, Token (QCONID _)) -> Just (Reduce 3 107)
    (309, Token (EXPORT _)) -> Just (Reduce 3 107)
    (309, Token (AS _)) -> Just (Reduce 3 107)
    (309, Token (QVARID _)) -> Just (Reduce 3 107)
    (309, Token (LARROW _)) -> Just (Reduce 3 107)
    (309, Token (THEN _)) -> Just (Reduce 3 107)
    (309, Token (ELSE _)) -> Just (Reduce 3 107)
    (309, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (309, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (309, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (309, Token (INTEGER _)) -> Just (Reduce 3 107)
    (310, Token (RPAREN _)) -> Just (Shift 307)
    (310, Token (COMMA _)) -> Just (Shift 146)
    (311, Token (RBRACKET _)) -> Just (Shift 309)
    (312, Token (WHERE _)) -> Just (Reduce 2 109)
    (312, Token (LBRACE _)) -> Just (Reduce 2 109)
    (312, Token (RBRACE _)) -> Just (Reduce 2 109)
    (312, Token (LPAREN _)) -> Just (Reduce 2 109)
    (312, Token (RPAREN _)) -> Just (Reduce 2 109)
    (312, Token (COMMA _)) -> Just (Reduce 2 109)
    (312, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (312, Token (EQUAL _)) -> Just (Reduce 2 109)
    (312, Token (DERIVING _)) -> Just (Reduce 2 109)
    (312, Token (DARROW _)) -> Just (Reduce 2 109)
    (312, Token (PIPE _)) -> Just (Reduce 2 109)
    (312, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (312, Token (MINUS _)) -> Just (Reduce 2 109)
    (312, Token (INFIXL _)) -> Just (Reduce 2 109)
    (312, Token (INFIXR _)) -> Just (Reduce 2 109)
    (312, Token (INFIX _)) -> Just (Reduce 2 109)
    (312, Token (RARROW _)) -> Just (Reduce 2 109)
    (312, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (312, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (312, Token (EXCL _)) -> Just (Reduce 2 109)
    (312, Token (QCONID _)) -> Just (Reduce 2 109)
    (312, Token (EXPORT _)) -> Just (Reduce 2 109)
    (312, Token (AS _)) -> Just (Reduce 2 109)
    (312, Token (QVARID _)) -> Just (Reduce 2 109)
    (312, Token (LARROW _)) -> Just (Reduce 2 109)
    (312, Token (THEN _)) -> Just (Reduce 2 109)
    (312, Token (ELSE _)) -> Just (Reduce 2 109)
    (312, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (312, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (312, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (312, Token (INTEGER _)) -> Just (Reduce 2 109)
    (313, Token (WHERE _)) -> Just (Reduce 1 104)
    (313, Token (LBRACE _)) -> Just (Reduce 1 104)
    (313, Token (RBRACE _)) -> Just (Reduce 1 104)
    (313, Token (LPAREN _)) -> Just (Reduce 1 104)
    (313, Token (RPAREN _)) -> Just (Reduce 1 104)
    (313, Token (COMMA _)) -> Just (Reduce 1 104)
    (313, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (313, Token (EQUAL _)) -> Just (Reduce 1 104)
    (313, Token (DERIVING _)) -> Just (Reduce 1 104)
    (313, Token (DARROW _)) -> Just (Reduce 1 104)
    (313, Token (PIPE _)) -> Just (Reduce 1 104)
    (313, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (313, Token (MINUS _)) -> Just (Reduce 1 104)
    (313, Token (INFIXL _)) -> Just (Reduce 1 104)
    (313, Token (INFIXR _)) -> Just (Reduce 1 104)
    (313, Token (INFIX _)) -> Just (Reduce 1 104)
    (313, Token (RARROW _)) -> Just (Reduce 1 104)
    (313, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (313, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (313, Token (EXCL _)) -> Just (Reduce 1 104)
    (313, Token (QCONID _)) -> Just (Reduce 1 104)
    (313, Token (EXPORT _)) -> Just (Reduce 1 104)
    (313, Token (AS _)) -> Just (Reduce 1 104)
    (313, Token (QVARID _)) -> Just (Reduce 1 104)
    (313, Token (LARROW _)) -> Just (Reduce 1 104)
    (313, Token (THEN _)) -> Just (Reduce 1 104)
    (313, Token (ELSE _)) -> Just (Reduce 1 104)
    (313, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (313, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (313, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (313, Token (INTEGER _)) -> Just (Reduce 1 104)
    (314, Token (WHERE _)) -> Just (Reduce 1 105)
    (314, Token (LBRACE _)) -> Just (Reduce 1 105)
    (314, Token (RBRACE _)) -> Just (Reduce 1 105)
    (314, Token (LPAREN _)) -> Just (Reduce 1 105)
    (314, Token (RPAREN _)) -> Just (Reduce 1 105)
    (314, Token (COMMA _)) -> Just (Reduce 1 105)
    (314, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (314, Token (EQUAL _)) -> Just (Reduce 1 105)
    (314, Token (DERIVING _)) -> Just (Reduce 1 105)
    (314, Token (DARROW _)) -> Just (Reduce 1 105)
    (314, Token (PIPE _)) -> Just (Reduce 1 105)
    (314, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (314, Token (MINUS _)) -> Just (Reduce 1 105)
    (314, Token (INFIXL _)) -> Just (Reduce 1 105)
    (314, Token (INFIXR _)) -> Just (Reduce 1 105)
    (314, Token (INFIX _)) -> Just (Reduce 1 105)
    (314, Token (RARROW _)) -> Just (Reduce 1 105)
    (314, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (314, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (314, Token (EXCL _)) -> Just (Reduce 1 105)
    (314, Token (QCONID _)) -> Just (Reduce 1 105)
    (314, Token (EXPORT _)) -> Just (Reduce 1 105)
    (314, Token (AS _)) -> Just (Reduce 1 105)
    (314, Token (QVARID _)) -> Just (Reduce 1 105)
    (314, Token (LARROW _)) -> Just (Reduce 1 105)
    (314, Token (THEN _)) -> Just (Reduce 1 105)
    (314, Token (ELSE _)) -> Just (Reduce 1 105)
    (314, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (314, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (314, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (314, Token (INTEGER _)) -> Just (Reduce 1 105)
    (315, Token (RPAREN _)) -> Just (Shift 308)
    (316, Token (WHERE _)) -> Just (Reduce 2 113)
    (316, Token (LBRACE _)) -> Just (Reduce 2 113)
    (316, Token (RBRACE _)) -> Just (Reduce 2 113)
    (316, Token (LPAREN _)) -> Just (Reduce 2 113)
    (316, Token (RPAREN _)) -> Just (Reduce 2 113)
    (316, Token (COMMA _)) -> Just (Reduce 2 113)
    (316, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (316, Token (EQUAL _)) -> Just (Reduce 2 113)
    (316, Token (DERIVING _)) -> Just (Reduce 2 113)
    (316, Token (DARROW _)) -> Just (Reduce 2 113)
    (316, Token (PIPE _)) -> Just (Reduce 2 113)
    (316, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (316, Token (MINUS _)) -> Just (Reduce 2 113)
    (316, Token (INFIXL _)) -> Just (Reduce 2 113)
    (316, Token (INFIXR _)) -> Just (Reduce 2 113)
    (316, Token (INFIX _)) -> Just (Reduce 2 113)
    (316, Token (RARROW _)) -> Just (Reduce 2 113)
    (316, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (316, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (316, Token (EXCL _)) -> Just (Reduce 2 113)
    (316, Token (QCONID _)) -> Just (Reduce 2 113)
    (316, Token (EXPORT _)) -> Just (Reduce 2 113)
    (316, Token (AS _)) -> Just (Reduce 2 113)
    (316, Token (QVARID _)) -> Just (Reduce 2 113)
    (316, Token (LARROW _)) -> Just (Reduce 2 113)
    (316, Token (THEN _)) -> Just (Reduce 2 113)
    (316, Token (ELSE _)) -> Just (Reduce 2 113)
    (316, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (316, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (316, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (316, Token (INTEGER _)) -> Just (Reduce 2 113)
    (317, Token (WHERE _)) -> Just (Reduce 3 115)
    (317, Token (LBRACE _)) -> Just (Reduce 3 115)
    (317, Token (RBRACE _)) -> Just (Reduce 3 115)
    (317, Token (LPAREN _)) -> Just (Reduce 3 115)
    (317, Token (RPAREN _)) -> Just (Reduce 3 115)
    (317, Token (COMMA _)) -> Just (Reduce 3 115)
    (317, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (317, Token (EQUAL _)) -> Just (Reduce 3 115)
    (317, Token (DERIVING _)) -> Just (Reduce 3 115)
    (317, Token (DARROW _)) -> Just (Reduce 3 115)
    (317, Token (PIPE _)) -> Just (Reduce 3 115)
    (317, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (317, Token (MINUS _)) -> Just (Reduce 3 115)
    (317, Token (INFIXL _)) -> Just (Reduce 3 115)
    (317, Token (INFIXR _)) -> Just (Reduce 3 115)
    (317, Token (INFIX _)) -> Just (Reduce 3 115)
    (317, Token (RARROW _)) -> Just (Reduce 3 115)
    (317, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (317, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (317, Token (EXCL _)) -> Just (Reduce 3 115)
    (317, Token (QCONID _)) -> Just (Reduce 3 115)
    (317, Token (EXPORT _)) -> Just (Reduce 3 115)
    (317, Token (AS _)) -> Just (Reduce 3 115)
    (317, Token (QVARID _)) -> Just (Reduce 3 115)
    (317, Token (LARROW _)) -> Just (Reduce 3 115)
    (317, Token (THEN _)) -> Just (Reduce 3 115)
    (317, Token (ELSE _)) -> Just (Reduce 3 115)
    (317, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (317, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (317, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (317, Token (INTEGER _)) -> Just (Reduce 3 115)
    (318, Token (WHERE _)) -> Just (Reduce 3 116)
    (318, Token (LBRACE _)) -> Just (Reduce 3 116)
    (318, Token (RBRACE _)) -> Just (Reduce 3 116)
    (318, Token (LPAREN _)) -> Just (Reduce 3 116)
    (318, Token (RPAREN _)) -> Just (Reduce 3 116)
    (318, Token (COMMA _)) -> Just (Reduce 3 116)
    (318, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (318, Token (EQUAL _)) -> Just (Reduce 3 116)
    (318, Token (DERIVING _)) -> Just (Reduce 3 116)
    (318, Token (DARROW _)) -> Just (Reduce 3 116)
    (318, Token (PIPE _)) -> Just (Reduce 3 116)
    (318, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (318, Token (MINUS _)) -> Just (Reduce 3 116)
    (318, Token (INFIXL _)) -> Just (Reduce 3 116)
    (318, Token (INFIXR _)) -> Just (Reduce 3 116)
    (318, Token (INFIX _)) -> Just (Reduce 3 116)
    (318, Token (RARROW _)) -> Just (Reduce 3 116)
    (318, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (318, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (318, Token (EXCL _)) -> Just (Reduce 3 116)
    (318, Token (QCONID _)) -> Just (Reduce 3 116)
    (318, Token (EXPORT _)) -> Just (Reduce 3 116)
    (318, Token (AS _)) -> Just (Reduce 3 116)
    (318, Token (QVARID _)) -> Just (Reduce 3 116)
    (318, Token (LARROW _)) -> Just (Reduce 3 116)
    (318, Token (THEN _)) -> Just (Reduce 3 116)
    (318, Token (ELSE _)) -> Just (Reduce 3 116)
    (318, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (318, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (318, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (318, Token (INTEGER _)) -> Just (Reduce 3 116)
    (319, Token (RPAREN _)) -> Just (Shift 317)
    (320, Token (WHERE _)) -> Just (Reduce 2 114)
    (320, Token (LBRACE _)) -> Just (Reduce 2 114)
    (320, Token (RBRACE _)) -> Just (Reduce 2 114)
    (320, Token (LPAREN _)) -> Just (Reduce 2 114)
    (320, Token (RPAREN _)) -> Just (Reduce 2 114)
    (320, Token (COMMA _)) -> Just (Reduce 2 114)
    (320, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (320, Token (EQUAL _)) -> Just (Reduce 2 114)
    (320, Token (DERIVING _)) -> Just (Reduce 2 114)
    (320, Token (DARROW _)) -> Just (Reduce 2 114)
    (320, Token (PIPE _)) -> Just (Reduce 2 114)
    (320, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (320, Token (MINUS _)) -> Just (Reduce 2 114)
    (320, Token (INFIXL _)) -> Just (Reduce 2 114)
    (320, Token (INFIXR _)) -> Just (Reduce 2 114)
    (320, Token (INFIX _)) -> Just (Reduce 2 114)
    (320, Token (RARROW _)) -> Just (Reduce 2 114)
    (320, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (320, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (320, Token (EXCL _)) -> Just (Reduce 2 114)
    (320, Token (QCONID _)) -> Just (Reduce 2 114)
    (320, Token (EXPORT _)) -> Just (Reduce 2 114)
    (320, Token (AS _)) -> Just (Reduce 2 114)
    (320, Token (QVARID _)) -> Just (Reduce 2 114)
    (320, Token (LARROW _)) -> Just (Reduce 2 114)
    (320, Token (THEN _)) -> Just (Reduce 2 114)
    (320, Token (ELSE _)) -> Just (Reduce 2 114)
    (320, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (320, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (320, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (320, Token (INTEGER _)) -> Just (Reduce 2 114)
    (321, Token (WHERE _)) -> Just (Reduce 1 112)
    (321, Token (LBRACE _)) -> Just (Reduce 1 112)
    (321, Token (RBRACE _)) -> Just (Reduce 1 112)
    (321, Token (LPAREN _)) -> Just (Reduce 1 112)
    (321, Token (RPAREN _)) -> Just (Reduce 1 112)
    (321, Token (COMMA _)) -> Just (Reduce 1 112)
    (321, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (321, Token (EQUAL _)) -> Just (Reduce 1 112)
    (321, Token (DERIVING _)) -> Just (Reduce 1 112)
    (321, Token (DARROW _)) -> Just (Reduce 1 112)
    (321, Token (PIPE _)) -> Just (Reduce 1 112)
    (321, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (321, Token (MINUS _)) -> Just (Reduce 1 112)
    (321, Token (INFIXL _)) -> Just (Reduce 1 112)
    (321, Token (INFIXR _)) -> Just (Reduce 1 112)
    (321, Token (INFIX _)) -> Just (Reduce 1 112)
    (321, Token (RARROW _)) -> Just (Reduce 1 112)
    (321, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (321, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (321, Token (EXCL _)) -> Just (Reduce 1 112)
    (321, Token (QCONID _)) -> Just (Reduce 1 112)
    (321, Token (EXPORT _)) -> Just (Reduce 1 112)
    (321, Token (AS _)) -> Just (Reduce 1 112)
    (321, Token (QVARID _)) -> Just (Reduce 1 112)
    (321, Token (LARROW _)) -> Just (Reduce 1 112)
    (321, Token (THEN _)) -> Just (Reduce 1 112)
    (321, Token (ELSE _)) -> Just (Reduce 1 112)
    (321, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (321, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (321, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (321, Token (INTEGER _)) -> Just (Reduce 1 112)
    (322, Token (LBRACE _)) -> Just (Shift 88)
    (322, Token (RBRACE _)) -> Just (Reduce 1 112)
    (322, Token (LPAREN _)) -> Just (Reduce 1 112)
    (322, Token (RPAREN _)) -> Just (Reduce 1 112)
    (322, Token (COMMA _)) -> Just (Reduce 1 112)
    (322, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (322, Token (DERIVING _)) -> Just (Reduce 1 112)
    (322, Token (PIPE _)) -> Just (Reduce 1 112)
    (322, Token (RARROW _)) -> Just (Reduce 1 112)
    (322, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (322, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (322, Token (EXCL _)) -> Just (Reduce 1 112)
    (322, Token (QCONID _)) -> Just (Reduce 1 112)
    (322, Token (EXPORT _)) -> Just (Reduce 1 112)
    (322, Token (AS _)) -> Just (Reduce 1 112)
    (322, Token (QVARID _)) -> Just (Reduce 1 112)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (322, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (323, Token (RPAREN _)) -> Just (Shift 318)
    (324, Token (WHERE _)) -> Just (Reduce 1 207)
    (324, Token (LBRACE _)) -> Just (Reduce 1 207)
    (324, Token (RBRACE _)) -> Just (Reduce 1 207)
    (324, Token (LPAREN _)) -> Just (Reduce 1 207)
    (324, Token (RPAREN _)) -> Just (Reduce 1 207)
    (324, Token (COMMA _)) -> Just (Reduce 1 207)
    (324, Token (SEMICOLON _)) -> Just (Reduce 1 207)
    (324, Token (EQUAL _)) -> Just (Reduce 1 207)
    (324, Token (DERIVING _)) -> Just (Reduce 1 207)
    (324, Token (DARROW _)) -> Just (Reduce 1 207)
    (324, Token (PIPE _)) -> Just (Reduce 1 207)
    (324, Token (COLON_COLON _)) -> Just (Reduce 1 207)
    (324, Token (MINUS _)) -> Just (Reduce 1 207)
    (324, Token (INFIXL _)) -> Just (Reduce 1 207)
    (324, Token (INFIXR _)) -> Just (Reduce 1 207)
    (324, Token (INFIX _)) -> Just (Reduce 1 207)
    (324, Token (RARROW _)) -> Just (Reduce 1 207)
    (324, Token (LBRACKET _)) -> Just (Reduce 1 207)
    (324, Token (RBRACKET _)) -> Just (Reduce 1 207)
    (324, Token (EXCL _)) -> Just (Reduce 1 207)
    (324, Token (QCONID _)) -> Just (Reduce 1 207)
    (324, Token (EXPORT _)) -> Just (Reduce 1 207)
    (324, Token (AS _)) -> Just (Reduce 1 207)
    (324, Token (QVARID _)) -> Just (Reduce 1 207)
    (324, Token (LARROW _)) -> Just (Reduce 1 207)
    (324, Token (THEN _)) -> Just (Reduce 1 207)
    (324, Token (ELSE _)) -> Just (Reduce 1 207)
    (324, Token (QVARSYM _)) -> Just (Reduce 1 207)
    (324, Token (BACKQUOTE _)) -> Just (Reduce 1 207)
    (324, Token (QCONSYM _)) -> Just (Reduce 1 207)
    (324, Token (INTEGER _)) -> Just (Reduce 1 207)
    (325, Token (WHERE _)) -> Just (Reduce 1 206)
    (325, Token (LBRACE _)) -> Just (Reduce 1 206)
    (325, Token (RBRACE _)) -> Just (Reduce 1 206)
    (325, Token (LPAREN _)) -> Just (Reduce 1 206)
    (325, Token (RPAREN _)) -> Just (Reduce 1 206)
    (325, Token (COMMA _)) -> Just (Reduce 1 206)
    (325, Token (SEMICOLON _)) -> Just (Reduce 1 206)
    (325, Token (EQUAL _)) -> Just (Reduce 1 206)
    (325, Token (DERIVING _)) -> Just (Reduce 1 206)
    (325, Token (DARROW _)) -> Just (Reduce 1 206)
    (325, Token (PIPE _)) -> Just (Reduce 1 206)
    (325, Token (COLON_COLON _)) -> Just (Reduce 1 206)
    (325, Token (MINUS _)) -> Just (Reduce 1 206)
    (325, Token (INFIXL _)) -> Just (Reduce 1 206)
    (325, Token (INFIXR _)) -> Just (Reduce 1 206)
    (325, Token (INFIX _)) -> Just (Reduce 1 206)
    (325, Token (RARROW _)) -> Just (Reduce 1 206)
    (325, Token (LBRACKET _)) -> Just (Reduce 1 206)
    (325, Token (RBRACKET _)) -> Just (Reduce 1 206)
    (325, Token (EXCL _)) -> Just (Reduce 1 206)
    (325, Token (QCONID _)) -> Just (Reduce 1 206)
    (325, Token (EXPORT _)) -> Just (Reduce 1 206)
    (325, Token (AS _)) -> Just (Reduce 1 206)
    (325, Token (QVARID _)) -> Just (Reduce 1 206)
    (325, Token (LARROW _)) -> Just (Reduce 1 206)
    (325, Token (THEN _)) -> Just (Reduce 1 206)
    (325, Token (ELSE _)) -> Just (Reduce 1 206)
    (325, Token (QVARSYM _)) -> Just (Reduce 1 206)
    (325, Token (BACKQUOTE _)) -> Just (Reduce 1 206)
    (325, Token (QCONSYM _)) -> Just (Reduce 1 206)
    (325, Token (INTEGER _)) -> Just (Reduce 1 206)
    (326, Token (WHERE _)) -> Just (Reduce 1 208)
    (326, Token (LBRACE _)) -> Just (Reduce 1 208)
    (326, Token (RBRACE _)) -> Just (Reduce 1 208)
    (326, Token (LPAREN _)) -> Just (Reduce 1 208)
    (326, Token (RPAREN _)) -> Just (Reduce 1 208)
    (326, Token (COMMA _)) -> Just (Reduce 1 208)
    (326, Token (SEMICOLON _)) -> Just (Reduce 1 208)
    (326, Token (EQUAL _)) -> Just (Reduce 1 208)
    (326, Token (DERIVING _)) -> Just (Reduce 1 208)
    (326, Token (DARROW _)) -> Just (Reduce 1 208)
    (326, Token (PIPE _)) -> Just (Reduce 1 208)
    (326, Token (COLON_COLON _)) -> Just (Reduce 1 208)
    (326, Token (MINUS _)) -> Just (Reduce 1 208)
    (326, Token (INFIXL _)) -> Just (Reduce 1 208)
    (326, Token (INFIXR _)) -> Just (Reduce 1 208)
    (326, Token (INFIX _)) -> Just (Reduce 1 208)
    (326, Token (RARROW _)) -> Just (Reduce 1 208)
    (326, Token (LBRACKET _)) -> Just (Reduce 1 208)
    (326, Token (RBRACKET _)) -> Just (Reduce 1 208)
    (326, Token (EXCL _)) -> Just (Reduce 1 208)
    (326, Token (QCONID _)) -> Just (Reduce 1 208)
    (326, Token (EXPORT _)) -> Just (Reduce 1 208)
    (326, Token (AS _)) -> Just (Reduce 1 208)
    (326, Token (QVARID _)) -> Just (Reduce 1 208)
    (326, Token (LARROW _)) -> Just (Reduce 1 208)
    (326, Token (THEN _)) -> Just (Reduce 1 208)
    (326, Token (ELSE _)) -> Just (Reduce 1 208)
    (326, Token (QVARSYM _)) -> Just (Reduce 1 208)
    (326, Token (BACKQUOTE _)) -> Just (Reduce 1 208)
    (326, Token (QCONSYM _)) -> Just (Reduce 1 208)
    (326, Token (INTEGER _)) -> Just (Reduce 1 208)
    (327, Token (RPAREN _)) -> Just (Reduce 3 110)
    (327, Token (COMMA _)) -> Just (Shift 146)
    (328, Token (RPAREN _)) -> Just (Reduce 3 111)
    (329, Token (RPAREN _)) -> Just (Reduce 1 117)
    (329, Token (COMMA _)) -> Just (Shift 329)
    (330, Token (RPAREN _)) -> Just (Reduce 2 118)
    (331, Token (RBRACE _)) -> Just (Reduce 3 122)
    (331, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (331, Token (DERIVING _)) -> Just (Reduce 3 122)
    (332, Token (RBRACE _)) -> Just (Reduce 1 121)
    (332, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (332, Token (DERIVING _)) -> Just (Reduce 1 121)
    (332, Token (PIPE _)) -> Just (Shift 122)
    (333, Token (RBRACE _)) -> Just (Reduce 3 125)
    (333, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (333, Token (DERIVING _)) -> Just (Reduce 3 125)
    (333, Token (PIPE _)) -> Just (Reduce 3 125)
    (334, Token (RBRACE _)) -> Just (Reduce 4 126)
    (334, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (334, Token (DERIVING _)) -> Just (Reduce 4 126)
    (334, Token (PIPE _)) -> Just (Reduce 4 126)
    (335, Token (RBRACE _)) -> Just (Shift 334)
    (336, Token (BACKQUOTE _)) -> Just (Shift 339)
    (337, Token (QCONID _)) -> Just (Shift 336)
    (337, Token (EXPORT _)) -> Just (Shift 408)
    (337, Token (AS _)) -> Just (Shift 409)
    (337, Token (QVARID _)) -> Just (Shift 410)
    (338, Token (QCONID _)) -> Just (Shift 336)
    (339, Token (RBRACE _)) -> Just (Reduce 3 199)
    (339, Token (LPAREN _)) -> Just (Reduce 3 199)
    (339, Token (RPAREN _)) -> Just (Reduce 3 199)
    (339, Token (COMMA _)) -> Just (Reduce 3 199)
    (339, Token (SEMICOLON _)) -> Just (Reduce 3 199)
    (339, Token (MINUS _)) -> Just (Reduce 3 199)
    (339, Token (RARROW _)) -> Just (Reduce 3 199)
    (339, Token (LBRACKET _)) -> Just (Reduce 3 199)
    (339, Token (RBRACKET _)) -> Just (Reduce 3 199)
    (339, Token (EXCL _)) -> Just (Reduce 3 199)
    (339, Token (QCONID _)) -> Just (Reduce 3 199)
    (339, Token (EXPORT _)) -> Just (Reduce 3 199)
    (339, Token (AS _)) -> Just (Reduce 3 199)
    (339, Token (QVARID _)) -> Just (Reduce 3 199)
    (339, Token (QVARSYM _)) -> Just (Reduce 3 199)
    (339, Token (BACKQUOTE _)) -> Just (Reduce 3 199)
    (339, Token (QCONSYM _)) -> Just (Reduce 3 199)
    (340, Token (RBRACE _)) -> Just (Reduce 1 198)
    (340, Token (LPAREN _)) -> Just (Reduce 1 198)
    (340, Token (RPAREN _)) -> Just (Reduce 1 198)
    (340, Token (COMMA _)) -> Just (Reduce 1 198)
    (340, Token (SEMICOLON _)) -> Just (Reduce 1 198)
    (340, Token (MINUS _)) -> Just (Reduce 1 198)
    (340, Token (RARROW _)) -> Just (Reduce 1 198)
    (340, Token (LBRACKET _)) -> Just (Reduce 1 198)
    (340, Token (RBRACKET _)) -> Just (Reduce 1 198)
    (340, Token (EXCL _)) -> Just (Reduce 1 198)
    (340, Token (QCONID _)) -> Just (Reduce 1 198)
    (340, Token (EXPORT _)) -> Just (Reduce 1 198)
    (340, Token (AS _)) -> Just (Reduce 1 198)
    (340, Token (QVARID _)) -> Just (Reduce 1 198)
    (340, Token (QVARSYM _)) -> Just (Reduce 1 198)
    (340, Token (BACKQUOTE _)) -> Just (Reduce 1 198)
    (340, Token (QCONSYM _)) -> Just (Reduce 1 198)
    (341, Token (RBRACE _)) -> Just (Reduce 3 130)
    (342, Token (RBRACE _)) -> Just (Reduce 1 129)
    (342, Token (COMMA _)) -> Just (Shift 89)
    (343, Token (RBRACE _)) -> Just (Reduce 3 131)
    (343, Token (COMMA _)) -> Just (Reduce 3 131)
    (344, Token (COLON_COLON _)) -> Just (Shift 134)
    (345, Token (EXPORT _)) -> Just (Reduce 1 139)
    (345, Token (AS _)) -> Just (Reduce 1 139)
    (345, Token (QVARID _)) -> Just (Reduce 1 139)
    (345, Token (STRING _)) -> Just (Reduce 1 139)
    (346, Token (EXPORT _)) -> Just (Reduce 1 138)
    (346, Token (AS _)) -> Just (Reduce 1 138)
    (346, Token (QVARID _)) -> Just (Reduce 1 138)
    (346, Token (STRING _)) -> Just (Reduce 1 138)
    (347, Token (EXPORT _)) -> Just (Reduce 1 140)
    (347, Token (AS _)) -> Just (Reduce 1 140)
    (347, Token (QVARID _)) -> Just (Reduce 1 140)
    (347, Token (STRING _)) -> Just (Reduce 1 140)
    (348, Token (LPAREN _)) -> Just (Reduce 1 141)
    (348, Token (MINUS _)) -> Just (Reduce 1 141)
    (348, Token (EXPORT _)) -> Just (Reduce 1 141)
    (348, Token (AS _)) -> Just (Reduce 1 141)
    (348, Token (QVARID _)) -> Just (Reduce 1 141)
    (348, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (349, Token (STRING _)) -> Just (Reduce 1 144)
    (350, Token (STRING _)) -> Just (Reduce 1 143)
    (351, Token (STRING _)) -> Just (Reduce 1 145)
    (352, Token (LPAREN _)) -> Just (Reduce 1 142)
    (352, Token (MINUS _)) -> Just (Reduce 1 142)
    (352, Token (EXPORT _)) -> Just (Reduce 1 142)
    (352, Token (AS _)) -> Just (Reduce 1 142)
    (352, Token (QVARID _)) -> Just (Reduce 1 142)
    (352, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (353, Token (EQUAL _)) -> Just (Reduce 3 149)
    (354, Token (COMMA _)) -> Just (Shift 63)
    (354, Token (EQUAL _)) -> Just (Reduce 1 148)
    (355, Token (COMMA _)) -> Just (Reduce 2 151)
    (355, Token (EQUAL _)) -> Just (Reduce 2 151)
    (355, Token (IN _)) -> Just (Shift 36)
    (356, Token (COMMA _)) -> Just (Reduce 3 150)
    (356, Token (EQUAL _)) -> Just (Reduce 3 150)
    (357, Token (COMMA _)) -> Just (Reduce 1 152)
    (357, Token (EQUAL _)) -> Just (Reduce 1 152)
    (357, Token (LARROW _)) -> Just (Shift 66)
    (358, Token (BACKQUOTE _)) -> Just (Shift 39)
    (359, Token (BACKQUOTE _)) -> Just (Shift 40)
    (360, Token (BACKQUOTE _)) -> Just (Shift 41)
    (361, Token (BACKQUOTE _)) -> Just (Shift 42)
    (362, Token (QCONID _)) -> Just (Shift 358)
    (362, Token (EXPORT _)) -> Just (Shift 359)
    (362, Token (AS _)) -> Just (Shift 360)
    (362, Token (QVARID _)) -> Just (Shift 361)
    (363, Token (WHERE _)) -> Just (Reduce 5 165)
    (363, Token (RBRACE _)) -> Just (Reduce 5 165)
    (363, Token (RPAREN _)) -> Just (Reduce 5 165)
    (363, Token (COMMA _)) -> Just (Reduce 5 165)
    (363, Token (SEMICOLON _)) -> Just (Reduce 5 165)
    (363, Token (EQUAL _)) -> Just (Reduce 5 165)
    (363, Token (PIPE _)) -> Just (Reduce 5 165)
    (363, Token (LARROW _)) -> Just (Reduce 5 165)
    (363, Token (THEN _)) -> Just (Reduce 5 165)
    (363, Token (ELSE _)) -> Just (Reduce 5 165)
    (364, Token (WHERE _)) -> Just (Reduce 3 164)
    (364, Token (RBRACE _)) -> Just (Reduce 3 164)
    (364, Token (RPAREN _)) -> Just (Reduce 3 164)
    (364, Token (COMMA _)) -> Just (Reduce 3 164)
    (364, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (364, Token (EQUAL _)) -> Just (Reduce 3 164)
    (364, Token (PIPE _)) -> Just (Reduce 3 164)
    (364, Token (LARROW _)) -> Just (Reduce 3 164)
    (364, Token (THEN _)) -> Just (Reduce 3 164)
    (364, Token (ELSE _)) -> Just (Reduce 3 164)
    (365, Token (IN _)) -> Just (Shift 36)
    (366, Token (WHERE _)) -> Just (Reduce 3 157)
    (366, Token (RBRACE _)) -> Just (Reduce 3 157)
    (366, Token (RPAREN _)) -> Just (Reduce 3 157)
    (366, Token (COMMA _)) -> Just (Reduce 3 157)
    (366, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (366, Token (EQUAL _)) -> Just (Reduce 3 157)
    (366, Token (PIPE _)) -> Just (Reduce 3 157)
    (366, Token (LARROW _)) -> Just (Reduce 3 157)
    (366, Token (THEN _)) -> Just (Reduce 3 157)
    (366, Token (ELSE _)) -> Just (Reduce 3 157)
    (367, Token (WHERE _)) -> Just (Reduce 4 154)
    (367, Token (RBRACE _)) -> Just (Reduce 4 154)
    (367, Token (RPAREN _)) -> Just (Reduce 4 154)
    (367, Token (COMMA _)) -> Just (Reduce 4 154)
    (367, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (367, Token (EQUAL _)) -> Just (Reduce 4 154)
    (367, Token (PIPE _)) -> Just (Reduce 4 154)
    (367, Token (LARROW _)) -> Just (Reduce 4 154)
    (367, Token (THEN _)) -> Just (Reduce 4 154)
    (367, Token (ELSE _)) -> Just (Reduce 4 154)
    (368, Token (WHERE _)) -> Just (Reduce 4 155)
    (368, Token (RBRACE _)) -> Just (Reduce 4 155)
    (368, Token (RPAREN _)) -> Just (Reduce 4 155)
    (368, Token (COMMA _)) -> Just (Reduce 4 155)
    (368, Token (SEMICOLON _)) -> Just (Reduce 4 155)
    (368, Token (EQUAL _)) -> Just (Reduce 4 155)
    (368, Token (PIPE _)) -> Just (Reduce 4 155)
    (368, Token (LARROW _)) -> Just (Reduce 4 155)
    (368, Token (THEN _)) -> Just (Reduce 4 155)
    (368, Token (ELSE _)) -> Just (Reduce 4 155)
    (369, Token (SEMICOLON _)) -> Just (Shift 381)
    (369, Token (THEN _)) -> Just (Reduce 0 213)
    (370, Token (SEMICOLON _)) -> Just (Shift 381)
    (370, Token (ELSE _)) -> Just (Reduce 0 213)
    (371, Token (WHERE _)) -> Just (Reduce 8 156)
    (371, Token (RBRACE _)) -> Just (Reduce 8 156)
    (371, Token (RPAREN _)) -> Just (Reduce 8 156)
    (371, Token (COMMA _)) -> Just (Reduce 8 156)
    (371, Token (SEMICOLON _)) -> Just (Reduce 8 156)
    (371, Token (EQUAL _)) -> Just (Reduce 8 156)
    (371, Token (PIPE _)) -> Just (Reduce 8 156)
    (371, Token (LARROW _)) -> Just (Reduce 8 156)
    (371, Token (THEN _)) -> Just (Reduce 8 156)
    (371, Token (ELSE _)) -> Just (Reduce 8 156)
    (372, Token (WHERE _)) -> Just (Reduce 3 158)
    (372, Token (RBRACE _)) -> Just (Reduce 3 158)
    (372, Token (RPAREN _)) -> Just (Reduce 3 158)
    (372, Token (COMMA _)) -> Just (Reduce 3 158)
    (372, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (372, Token (EQUAL _)) -> Just (Reduce 3 158)
    (372, Token (PIPE _)) -> Just (Reduce 3 158)
    (372, Token (LARROW _)) -> Just (Reduce 3 158)
    (372, Token (THEN _)) -> Just (Reduce 3 158)
    (372, Token (ELSE _)) -> Just (Reduce 3 158)
    (373, Token (WHERE _)) -> Just (Reduce 5 163)
    (373, Token (RBRACE _)) -> Just (Reduce 5 163)
    (373, Token (RPAREN _)) -> Just (Reduce 5 163)
    (373, Token (COMMA _)) -> Just (Reduce 5 163)
    (373, Token (SEMICOLON _)) -> Just (Reduce 5 163)
    (373, Token (EQUAL _)) -> Just (Reduce 5 163)
    (373, Token (PIPE _)) -> Just (Reduce 5 163)
    (373, Token (LARROW _)) -> Just (Reduce 5 163)
    (373, Token (THEN _)) -> Just (Reduce 5 163)
    (373, Token (ELSE _)) -> Just (Reduce 5 163)
    (374, Token (WHERE _)) -> Just (Reduce 5 160)
    (374, Token (RBRACE _)) -> Just (Reduce 5 160)
    (374, Token (RPAREN _)) -> Just (Reduce 5 160)
    (374, Token (COMMA _)) -> Just (Reduce 5 160)
    (374, Token (SEMICOLON _)) -> Just (Reduce 5 160)
    (374, Token (EQUAL _)) -> Just (Reduce 5 160)
    (374, Token (PIPE _)) -> Just (Reduce 5 160)
    (374, Token (LARROW _)) -> Just (Reduce 5 160)
    (374, Token (THEN _)) -> Just (Reduce 5 160)
    (374, Token (ELSE _)) -> Just (Reduce 5 160)
    (375, Token (WHERE _)) -> Just (Reduce 5 159)
    (375, Token (RBRACE _)) -> Just (Reduce 5 159)
    (375, Token (RPAREN _)) -> Just (Reduce 5 159)
    (375, Token (COMMA _)) -> Just (Reduce 5 159)
    (375, Token (SEMICOLON _)) -> Just (Reduce 5 159)
    (375, Token (EQUAL _)) -> Just (Reduce 5 159)
    (375, Token (PIPE _)) -> Just (Reduce 5 159)
    (375, Token (LARROW _)) -> Just (Reduce 5 159)
    (375, Token (THEN _)) -> Just (Reduce 5 159)
    (375, Token (ELSE _)) -> Just (Reduce 5 159)
    (376, Token (WHERE _)) -> Just (Reduce 5 161)
    (376, Token (RBRACE _)) -> Just (Reduce 5 161)
    (376, Token (RPAREN _)) -> Just (Reduce 5 161)
    (376, Token (COMMA _)) -> Just (Reduce 5 161)
    (376, Token (SEMICOLON _)) -> Just (Reduce 5 161)
    (376, Token (EQUAL _)) -> Just (Reduce 5 161)
    (376, Token (PIPE _)) -> Just (Reduce 5 161)
    (376, Token (LARROW _)) -> Just (Reduce 5 161)
    (376, Token (THEN _)) -> Just (Reduce 5 161)
    (376, Token (ELSE _)) -> Just (Reduce 5 161)
    (377, Token (WHERE _)) -> Just (Reduce 3 162)
    (377, Token (RBRACE _)) -> Just (Reduce 3 162)
    (377, Token (RPAREN _)) -> Just (Reduce 3 162)
    (377, Token (COMMA _)) -> Just (Reduce 3 162)
    (377, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (377, Token (EQUAL _)) -> Just (Reduce 3 162)
    (377, Token (PIPE _)) -> Just (Reduce 3 162)
    (377, Token (LARROW _)) -> Just (Reduce 3 162)
    (377, Token (THEN _)) -> Just (Reduce 3 162)
    (377, Token (ELSE _)) -> Just (Reduce 3 162)
    (378, Token (THEN _)) -> Just (Shift 65)
    (379, Token (ELSE _)) -> Just (Shift 37)
    (380, Token (WHERE _)) -> Just (Reduce 1 166)
    (380, Token (RBRACE _)) -> Just (Reduce 1 166)
    (380, Token (RPAREN _)) -> Just (Reduce 1 166)
    (380, Token (COMMA _)) -> Just (Reduce 1 166)
    (380, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (380, Token (EQUAL _)) -> Just (Reduce 1 166)
    (380, Token (PIPE _)) -> Just (Reduce 1 166)
    (380, Token (COLON_COLON _)) -> Just (Shift 112)
    (380, Token (MINUS _)) -> Just (Shift 34)
    (380, Token (LARROW _)) -> Just (Reduce 1 166)
    (380, Token (THEN _)) -> Just (Reduce 1 166)
    (380, Token (ELSE _)) -> Just (Reduce 1 166)
    (380, Token (QVARSYM _)) -> Just (Shift 38)
    (380, Token (BACKQUOTE _)) -> Just (Shift 362)
    (380, Token (QCONSYM _)) -> Just (Shift 43)
    (381, Token (THEN _)) -> Just (Reduce 1 214)
    (381, Token (ELSE _)) -> Just (Reduce 1 214)
    (382, Token (WHERE _)) -> Just (Reduce 1 169)
    (382, Token (LBRACE _)) -> Just (Reduce 1 169)
    (382, Token (RBRACE _)) -> Just (Reduce 1 169)
    (382, Token (LPAREN _)) -> Just (Reduce 1 169)
    (382, Token (RPAREN _)) -> Just (Reduce 1 169)
    (382, Token (COMMA _)) -> Just (Reduce 1 169)
    (382, Token (SEMICOLON _)) -> Just (Reduce 1 169)
    (382, Token (EQUAL _)) -> Just (Reduce 1 169)
    (382, Token (PIPE _)) -> Just (Reduce 1 169)
    (382, Token (COLON_COLON _)) -> Just (Reduce 1 169)
    (382, Token (MINUS _)) -> Just (Reduce 1 169)
    (382, Token (INFIXL _)) -> Just (Reduce 1 169)
    (382, Token (INFIXR _)) -> Just (Reduce 1 169)
    (382, Token (INFIX _)) -> Just (Reduce 1 169)
    (382, Token (QCONID _)) -> Just (Reduce 1 169)
    (382, Token (EXPORT _)) -> Just (Reduce 1 169)
    (382, Token (AS _)) -> Just (Reduce 1 169)
    (382, Token (QVARID _)) -> Just (Reduce 1 169)
    (382, Token (STRING _)) -> Just (Reduce 1 169)
    (382, Token (LARROW _)) -> Just (Reduce 1 169)
    (382, Token (LET _)) -> Just (Reduce 1 169)
    (382, Token (LAMBDA _)) -> Just (Reduce 1 169)
    (382, Token (IF _)) -> Just (Reduce 1 169)
    (382, Token (THEN _)) -> Just (Reduce 1 169)
    (382, Token (ELSE _)) -> Just (Reduce 1 169)
    (382, Token (QVARSYM _)) -> Just (Reduce 1 169)
    (382, Token (BACKQUOTE _)) -> Just (Reduce 1 169)
    (382, Token (QCONSYM _)) -> Just (Reduce 1 169)
    (382, Token (INTEGER _)) -> Just (Reduce 1 169)
    (383, Token (WHERE _)) -> Just (Reduce 2 170)
    (383, Token (LBRACE _)) -> Just (Reduce 2 170)
    (383, Token (RBRACE _)) -> Just (Reduce 2 170)
    (383, Token (LPAREN _)) -> Just (Reduce 2 170)
    (383, Token (RPAREN _)) -> Just (Reduce 2 170)
    (383, Token (COMMA _)) -> Just (Reduce 2 170)
    (383, Token (SEMICOLON _)) -> Just (Reduce 2 170)
    (383, Token (EQUAL _)) -> Just (Reduce 2 170)
    (383, Token (PIPE _)) -> Just (Reduce 2 170)
    (383, Token (COLON_COLON _)) -> Just (Reduce 2 170)
    (383, Token (MINUS _)) -> Just (Reduce 2 170)
    (383, Token (INFIXL _)) -> Just (Reduce 2 170)
    (383, Token (INFIXR _)) -> Just (Reduce 2 170)
    (383, Token (INFIX _)) -> Just (Reduce 2 170)
    (383, Token (QCONID _)) -> Just (Reduce 2 170)
    (383, Token (EXPORT _)) -> Just (Reduce 2 170)
    (383, Token (AS _)) -> Just (Reduce 2 170)
    (383, Token (QVARID _)) -> Just (Reduce 2 170)
    (383, Token (STRING _)) -> Just (Reduce 2 170)
    (383, Token (LARROW _)) -> Just (Reduce 2 170)
    (383, Token (LET _)) -> Just (Reduce 2 170)
    (383, Token (LAMBDA _)) -> Just (Reduce 2 170)
    (383, Token (IF _)) -> Just (Reduce 2 170)
    (383, Token (THEN _)) -> Just (Reduce 2 170)
    (383, Token (ELSE _)) -> Just (Reduce 2 170)
    (383, Token (QVARSYM _)) -> Just (Reduce 2 170)
    (383, Token (BACKQUOTE _)) -> Just (Reduce 2 170)
    (383, Token (QCONSYM _)) -> Just (Reduce 2 170)
    (383, Token (INTEGER _)) -> Just (Reduce 2 170)
    (384, Token (WHERE _)) -> Just (Reduce 3 174)
    (384, Token (LBRACE _)) -> Just (Reduce 3 174)
    (384, Token (RBRACE _)) -> Just (Reduce 3 174)
    (384, Token (LPAREN _)) -> Just (Reduce 3 174)
    (384, Token (RPAREN _)) -> Just (Reduce 3 174)
    (384, Token (COMMA _)) -> Just (Reduce 3 174)
    (384, Token (SEMICOLON _)) -> Just (Reduce 3 174)
    (384, Token (EQUAL _)) -> Just (Reduce 3 174)
    (384, Token (PIPE _)) -> Just (Reduce 3 174)
    (384, Token (COLON_COLON _)) -> Just (Reduce 3 174)
    (384, Token (MINUS _)) -> Just (Reduce 3 174)
    (384, Token (INFIXL _)) -> Just (Reduce 3 174)
    (384, Token (INFIXR _)) -> Just (Reduce 3 174)
    (384, Token (INFIX _)) -> Just (Reduce 3 174)
    (384, Token (QCONID _)) -> Just (Reduce 3 174)
    (384, Token (EXPORT _)) -> Just (Reduce 3 174)
    (384, Token (AS _)) -> Just (Reduce 3 174)
    (384, Token (QVARID _)) -> Just (Reduce 3 174)
    (384, Token (STRING _)) -> Just (Reduce 3 174)
    (384, Token (LARROW _)) -> Just (Reduce 3 174)
    (384, Token (LET _)) -> Just (Reduce 3 174)
    (384, Token (LAMBDA _)) -> Just (Reduce 3 174)
    (384, Token (IF _)) -> Just (Reduce 3 174)
    (384, Token (THEN _)) -> Just (Reduce 3 174)
    (384, Token (ELSE _)) -> Just (Reduce 3 174)
    (384, Token (QVARSYM _)) -> Just (Reduce 3 174)
    (384, Token (BACKQUOTE _)) -> Just (Reduce 3 174)
    (384, Token (QCONSYM _)) -> Just (Reduce 3 174)
    (384, Token (INTEGER _)) -> Just (Reduce 3 174)
    (385, Token (WHERE _)) -> Just (Reduce 4 175)
    (385, Token (LBRACE _)) -> Just (Reduce 4 175)
    (385, Token (RBRACE _)) -> Just (Reduce 4 175)
    (385, Token (LPAREN _)) -> Just (Reduce 4 175)
    (385, Token (RPAREN _)) -> Just (Reduce 4 175)
    (385, Token (COMMA _)) -> Just (Reduce 4 175)
    (385, Token (SEMICOLON _)) -> Just (Reduce 4 175)
    (385, Token (EQUAL _)) -> Just (Reduce 4 175)
    (385, Token (PIPE _)) -> Just (Reduce 4 175)
    (385, Token (COLON_COLON _)) -> Just (Reduce 4 175)
    (385, Token (MINUS _)) -> Just (Reduce 4 175)
    (385, Token (INFIXL _)) -> Just (Reduce 4 175)
    (385, Token (INFIXR _)) -> Just (Reduce 4 175)
    (385, Token (INFIX _)) -> Just (Reduce 4 175)
    (385, Token (QCONID _)) -> Just (Reduce 4 175)
    (385, Token (EXPORT _)) -> Just (Reduce 4 175)
    (385, Token (AS _)) -> Just (Reduce 4 175)
    (385, Token (QVARID _)) -> Just (Reduce 4 175)
    (385, Token (STRING _)) -> Just (Reduce 4 175)
    (385, Token (LARROW _)) -> Just (Reduce 4 175)
    (385, Token (LET _)) -> Just (Reduce 4 175)
    (385, Token (LAMBDA _)) -> Just (Reduce 4 175)
    (385, Token (IF _)) -> Just (Reduce 4 175)
    (385, Token (THEN _)) -> Just (Reduce 4 175)
    (385, Token (ELSE _)) -> Just (Reduce 4 175)
    (385, Token (QVARSYM _)) -> Just (Reduce 4 175)
    (385, Token (BACKQUOTE _)) -> Just (Reduce 4 175)
    (385, Token (QCONSYM _)) -> Just (Reduce 4 175)
    (385, Token (INTEGER _)) -> Just (Reduce 4 175)
    (386, Token (WHERE _)) -> Just (Reduce 6 180)
    (386, Token (LBRACE _)) -> Just (Reduce 6 180)
    (386, Token (RBRACE _)) -> Just (Reduce 6 180)
    (386, Token (LPAREN _)) -> Just (Reduce 6 180)
    (386, Token (RPAREN _)) -> Just (Reduce 6 180)
    (386, Token (COMMA _)) -> Just (Reduce 6 180)
    (386, Token (SEMICOLON _)) -> Just (Reduce 6 180)
    (386, Token (EQUAL _)) -> Just (Reduce 6 180)
    (386, Token (PIPE _)) -> Just (Reduce 6 180)
    (386, Token (COLON_COLON _)) -> Just (Reduce 6 180)
    (386, Token (MINUS _)) -> Just (Reduce 6 180)
    (386, Token (INFIXL _)) -> Just (Reduce 6 180)
    (386, Token (INFIXR _)) -> Just (Reduce 6 180)
    (386, Token (INFIX _)) -> Just (Reduce 6 180)
    (386, Token (QCONID _)) -> Just (Reduce 6 180)
    (386, Token (EXPORT _)) -> Just (Reduce 6 180)
    (386, Token (AS _)) -> Just (Reduce 6 180)
    (386, Token (QVARID _)) -> Just (Reduce 6 180)
    (386, Token (STRING _)) -> Just (Reduce 6 180)
    (386, Token (LARROW _)) -> Just (Reduce 6 180)
    (386, Token (LET _)) -> Just (Reduce 6 180)
    (386, Token (LAMBDA _)) -> Just (Reduce 6 180)
    (386, Token (IF _)) -> Just (Reduce 6 180)
    (386, Token (THEN _)) -> Just (Reduce 6 180)
    (386, Token (ELSE _)) -> Just (Reduce 6 180)
    (386, Token (QVARSYM _)) -> Just (Reduce 6 180)
    (386, Token (BACKQUOTE _)) -> Just (Reduce 6 180)
    (386, Token (QCONSYM _)) -> Just (Reduce 6 180)
    (386, Token (INTEGER _)) -> Just (Reduce 6 180)
    (387, Token (WHERE _)) -> Just (Reduce 6 177)
    (387, Token (LBRACE _)) -> Just (Reduce 6 177)
    (387, Token (RBRACE _)) -> Just (Reduce 6 177)
    (387, Token (LPAREN _)) -> Just (Reduce 6 177)
    (387, Token (RPAREN _)) -> Just (Reduce 6 177)
    (387, Token (COMMA _)) -> Just (Reduce 6 177)
    (387, Token (SEMICOLON _)) -> Just (Reduce 6 177)
    (387, Token (EQUAL _)) -> Just (Reduce 6 177)
    (387, Token (PIPE _)) -> Just (Reduce 6 177)
    (387, Token (COLON_COLON _)) -> Just (Reduce 6 177)
    (387, Token (MINUS _)) -> Just (Reduce 6 177)
    (387, Token (INFIXL _)) -> Just (Reduce 6 177)
    (387, Token (INFIXR _)) -> Just (Reduce 6 177)
    (387, Token (INFIX _)) -> Just (Reduce 6 177)
    (387, Token (QCONID _)) -> Just (Reduce 6 177)
    (387, Token (EXPORT _)) -> Just (Reduce 6 177)
    (387, Token (AS _)) -> Just (Reduce 6 177)
    (387, Token (QVARID _)) -> Just (Reduce 6 177)
    (387, Token (STRING _)) -> Just (Reduce 6 177)
    (387, Token (LARROW _)) -> Just (Reduce 6 177)
    (387, Token (LET _)) -> Just (Reduce 6 177)
    (387, Token (LAMBDA _)) -> Just (Reduce 6 177)
    (387, Token (IF _)) -> Just (Reduce 6 177)
    (387, Token (THEN _)) -> Just (Reduce 6 177)
    (387, Token (ELSE _)) -> Just (Reduce 6 177)
    (387, Token (QVARSYM _)) -> Just (Reduce 6 177)
    (387, Token (BACKQUOTE _)) -> Just (Reduce 6 177)
    (387, Token (QCONSYM _)) -> Just (Reduce 6 177)
    (387, Token (INTEGER _)) -> Just (Reduce 6 177)
    (388, Token (WHERE _)) -> Just (Reduce 6 176)
    (388, Token (LBRACE _)) -> Just (Reduce 6 176)
    (388, Token (RBRACE _)) -> Just (Reduce 6 176)
    (388, Token (LPAREN _)) -> Just (Reduce 6 176)
    (388, Token (RPAREN _)) -> Just (Reduce 6 176)
    (388, Token (COMMA _)) -> Just (Reduce 6 176)
    (388, Token (SEMICOLON _)) -> Just (Reduce 6 176)
    (388, Token (EQUAL _)) -> Just (Reduce 6 176)
    (388, Token (PIPE _)) -> Just (Reduce 6 176)
    (388, Token (COLON_COLON _)) -> Just (Reduce 6 176)
    (388, Token (MINUS _)) -> Just (Reduce 6 176)
    (388, Token (INFIXL _)) -> Just (Reduce 6 176)
    (388, Token (INFIXR _)) -> Just (Reduce 6 176)
    (388, Token (INFIX _)) -> Just (Reduce 6 176)
    (388, Token (QCONID _)) -> Just (Reduce 6 176)
    (388, Token (EXPORT _)) -> Just (Reduce 6 176)
    (388, Token (AS _)) -> Just (Reduce 6 176)
    (388, Token (QVARID _)) -> Just (Reduce 6 176)
    (388, Token (STRING _)) -> Just (Reduce 6 176)
    (388, Token (LARROW _)) -> Just (Reduce 6 176)
    (388, Token (LET _)) -> Just (Reduce 6 176)
    (388, Token (LAMBDA _)) -> Just (Reduce 6 176)
    (388, Token (IF _)) -> Just (Reduce 6 176)
    (388, Token (THEN _)) -> Just (Reduce 6 176)
    (388, Token (ELSE _)) -> Just (Reduce 6 176)
    (388, Token (QVARSYM _)) -> Just (Reduce 6 176)
    (388, Token (BACKQUOTE _)) -> Just (Reduce 6 176)
    (388, Token (QCONSYM _)) -> Just (Reduce 6 176)
    (388, Token (INTEGER _)) -> Just (Reduce 6 176)
    (389, Token (WHERE _)) -> Just (Reduce 6 178)
    (389, Token (LBRACE _)) -> Just (Reduce 6 178)
    (389, Token (RBRACE _)) -> Just (Reduce 6 178)
    (389, Token (LPAREN _)) -> Just (Reduce 6 178)
    (389, Token (RPAREN _)) -> Just (Reduce 6 178)
    (389, Token (COMMA _)) -> Just (Reduce 6 178)
    (389, Token (SEMICOLON _)) -> Just (Reduce 6 178)
    (389, Token (EQUAL _)) -> Just (Reduce 6 178)
    (389, Token (PIPE _)) -> Just (Reduce 6 178)
    (389, Token (COLON_COLON _)) -> Just (Reduce 6 178)
    (389, Token (MINUS _)) -> Just (Reduce 6 178)
    (389, Token (INFIXL _)) -> Just (Reduce 6 178)
    (389, Token (INFIXR _)) -> Just (Reduce 6 178)
    (389, Token (INFIX _)) -> Just (Reduce 6 178)
    (389, Token (QCONID _)) -> Just (Reduce 6 178)
    (389, Token (EXPORT _)) -> Just (Reduce 6 178)
    (389, Token (AS _)) -> Just (Reduce 6 178)
    (389, Token (QVARID _)) -> Just (Reduce 6 178)
    (389, Token (STRING _)) -> Just (Reduce 6 178)
    (389, Token (LARROW _)) -> Just (Reduce 6 178)
    (389, Token (LET _)) -> Just (Reduce 6 178)
    (389, Token (LAMBDA _)) -> Just (Reduce 6 178)
    (389, Token (IF _)) -> Just (Reduce 6 178)
    (389, Token (THEN _)) -> Just (Reduce 6 178)
    (389, Token (ELSE _)) -> Just (Reduce 6 178)
    (389, Token (QVARSYM _)) -> Just (Reduce 6 178)
    (389, Token (BACKQUOTE _)) -> Just (Reduce 6 178)
    (389, Token (QCONSYM _)) -> Just (Reduce 6 178)
    (389, Token (INTEGER _)) -> Just (Reduce 6 178)
    (390, Token (WHERE _)) -> Just (Reduce 4 179)
    (390, Token (LBRACE _)) -> Just (Reduce 4 179)
    (390, Token (RBRACE _)) -> Just (Reduce 4 179)
    (390, Token (LPAREN _)) -> Just (Reduce 4 179)
    (390, Token (RPAREN _)) -> Just (Reduce 4 179)
    (390, Token (COMMA _)) -> Just (Reduce 4 179)
    (390, Token (SEMICOLON _)) -> Just (Reduce 4 179)
    (390, Token (EQUAL _)) -> Just (Reduce 4 179)
    (390, Token (PIPE _)) -> Just (Reduce 4 179)
    (390, Token (COLON_COLON _)) -> Just (Reduce 4 179)
    (390, Token (MINUS _)) -> Just (Reduce 4 179)
    (390, Token (INFIXL _)) -> Just (Reduce 4 179)
    (390, Token (INFIXR _)) -> Just (Reduce 4 179)
    (390, Token (INFIX _)) -> Just (Reduce 4 179)
    (390, Token (QCONID _)) -> Just (Reduce 4 179)
    (390, Token (EXPORT _)) -> Just (Reduce 4 179)
    (390, Token (AS _)) -> Just (Reduce 4 179)
    (390, Token (QVARID _)) -> Just (Reduce 4 179)
    (390, Token (STRING _)) -> Just (Reduce 4 179)
    (390, Token (LARROW _)) -> Just (Reduce 4 179)
    (390, Token (LET _)) -> Just (Reduce 4 179)
    (390, Token (LAMBDA _)) -> Just (Reduce 4 179)
    (390, Token (IF _)) -> Just (Reduce 4 179)
    (390, Token (THEN _)) -> Just (Reduce 4 179)
    (390, Token (ELSE _)) -> Just (Reduce 4 179)
    (390, Token (QVARSYM _)) -> Just (Reduce 4 179)
    (390, Token (BACKQUOTE _)) -> Just (Reduce 4 179)
    (390, Token (QCONSYM _)) -> Just (Reduce 4 179)
    (390, Token (INTEGER _)) -> Just (Reduce 4 179)
    (391, Token (BACKQUOTE _)) -> Just (Shift 54)
    (392, Token (BACKQUOTE _)) -> Just (Shift 55)
    (393, Token (BACKQUOTE _)) -> Just (Shift 56)
    (394, Token (BACKQUOTE _)) -> Just (Shift 57)
    (395, Token (WHERE _)) -> Just (Reduce 1 173)
    (395, Token (LBRACE _)) -> Just (Reduce 1 173)
    (395, Token (RBRACE _)) -> Just (Reduce 1 173)
    (395, Token (LPAREN _)) -> Just (Reduce 1 173)
    (395, Token (RPAREN _)) -> Just (Reduce 1 173)
    (395, Token (COMMA _)) -> Just (Reduce 1 173)
    (395, Token (SEMICOLON _)) -> Just (Reduce 1 173)
    (395, Token (EQUAL _)) -> Just (Reduce 1 173)
    (395, Token (PIPE _)) -> Just (Reduce 1 173)
    (395, Token (COLON_COLON _)) -> Just (Reduce 1 173)
    (395, Token (MINUS _)) -> Just (Reduce 1 173)
    (395, Token (INFIXL _)) -> Just (Reduce 1 173)
    (395, Token (INFIXR _)) -> Just (Reduce 1 173)
    (395, Token (INFIX _)) -> Just (Reduce 1 173)
    (395, Token (QCONID _)) -> Just (Reduce 1 173)
    (395, Token (EXPORT _)) -> Just (Reduce 1 173)
    (395, Token (AS _)) -> Just (Reduce 1 173)
    (395, Token (QVARID _)) -> Just (Reduce 1 173)
    (395, Token (STRING _)) -> Just (Reduce 1 173)
    (395, Token (LARROW _)) -> Just (Reduce 1 173)
    (395, Token (LET _)) -> Just (Reduce 1 173)
    (395, Token (LAMBDA _)) -> Just (Reduce 1 173)
    (395, Token (IF _)) -> Just (Reduce 1 173)
    (395, Token (THEN _)) -> Just (Reduce 1 173)
    (395, Token (ELSE _)) -> Just (Reduce 1 173)
    (395, Token (QVARSYM _)) -> Just (Reduce 1 173)
    (395, Token (BACKQUOTE _)) -> Just (Reduce 1 173)
    (395, Token (QCONSYM _)) -> Just (Reduce 1 173)
    (395, Token (INTEGER _)) -> Just (Reduce 1 173)
    (396, Token (QCONID _)) -> Just (Shift 391)
    (396, Token (EXPORT _)) -> Just (Shift 392)
    (396, Token (AS _)) -> Just (Shift 393)
    (396, Token (QVARID _)) -> Just (Shift 394)
    (397, Token (WHERE _)) -> Just (Reduce 1 172)
    (397, Token (LBRACE _)) -> Just (Reduce 1 172)
    (397, Token (RBRACE _)) -> Just (Reduce 1 172)
    (397, Token (LPAREN _)) -> Just (Reduce 1 172)
    (397, Token (RPAREN _)) -> Just (Reduce 1 172)
    (397, Token (COMMA _)) -> Just (Reduce 1 172)
    (397, Token (SEMICOLON _)) -> Just (Reduce 1 172)
    (397, Token (EQUAL _)) -> Just (Reduce 1 172)
    (397, Token (PIPE _)) -> Just (Reduce 1 172)
    (397, Token (COLON_COLON _)) -> Just (Reduce 1 172)
    (397, Token (MINUS _)) -> Just (Reduce 1 172)
    (397, Token (INFIXL _)) -> Just (Reduce 1 172)
    (397, Token (INFIXR _)) -> Just (Reduce 1 172)
    (397, Token (INFIX _)) -> Just (Reduce 1 172)
    (397, Token (QCONID _)) -> Just (Reduce 1 172)
    (397, Token (EXPORT _)) -> Just (Reduce 1 172)
    (397, Token (AS _)) -> Just (Reduce 1 172)
    (397, Token (QVARID _)) -> Just (Reduce 1 172)
    (397, Token (STRING _)) -> Just (Reduce 1 172)
    (397, Token (LARROW _)) -> Just (Reduce 1 172)
    (397, Token (LET _)) -> Just (Reduce 1 172)
    (397, Token (LAMBDA _)) -> Just (Reduce 1 172)
    (397, Token (IF _)) -> Just (Reduce 1 172)
    (397, Token (THEN _)) -> Just (Reduce 1 172)
    (397, Token (ELSE _)) -> Just (Reduce 1 172)
    (397, Token (QVARSYM _)) -> Just (Reduce 1 172)
    (397, Token (BACKQUOTE _)) -> Just (Reduce 1 172)
    (397, Token (QCONSYM _)) -> Just (Reduce 1 172)
    (397, Token (INTEGER _)) -> Just (Reduce 1 172)
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
    (398, Token (INTEGER _)) -> Just (Reduce 1 171)
    (399, Token (RPAREN _)) -> Just (Shift 384)
    (400, Token (RPAREN _)) -> Just (Shift 385)
    (401, Token (RPAREN _)) -> Just (Shift 386)
    (402, Token (RPAREN _)) -> Just (Shift 387)
    (403, Token (RPAREN _)) -> Just (Shift 388)
    (404, Token (RPAREN _)) -> Just (Shift 389)
    (405, Token (RPAREN _)) -> Just (Shift 390)
    (406, Token (LPAREN _)) -> Just (Reduce 3 186)
    (406, Token (RPAREN _)) -> Just (Reduce 3 186)
    (406, Token (EQUAL _)) -> Just (Reduce 3 186)
    (406, Token (PIPE _)) -> Just (Reduce 3 186)
    (406, Token (MINUS _)) -> Just (Reduce 3 186)
    (406, Token (RARROW _)) -> Just (Reduce 3 186)
    (406, Token (QCONID _)) -> Just (Reduce 3 186)
    (406, Token (EXPORT _)) -> Just (Reduce 3 186)
    (406, Token (AS _)) -> Just (Reduce 3 186)
    (406, Token (QVARID _)) -> Just (Reduce 3 186)
    (406, Token (QVARSYM _)) -> Just (Reduce 3 186)
    (406, Token (BACKQUOTE _)) -> Just (Reduce 3 186)
    (406, Token (QCONSYM _)) -> Just (Reduce 3 186)
    (407, Token (LPAREN _)) -> Just (Reduce 1 185)
    (407, Token (RPAREN _)) -> Just (Reduce 1 185)
    (407, Token (EQUAL _)) -> Just (Reduce 1 185)
    (407, Token (PIPE _)) -> Just (Reduce 1 185)
    (407, Token (MINUS _)) -> Just (Reduce 1 185)
    (407, Token (RARROW _)) -> Just (Reduce 1 185)
    (407, Token (QCONID _)) -> Just (Reduce 1 185)
    (407, Token (EXPORT _)) -> Just (Reduce 1 185)
    (407, Token (AS _)) -> Just (Reduce 1 185)
    (407, Token (QVARID _)) -> Just (Reduce 1 185)
    (407, Token (QVARSYM _)) -> Just (Reduce 1 185)
    (407, Token (BACKQUOTE _)) -> Just (Reduce 1 185)
    (407, Token (QCONSYM _)) -> Just (Reduce 1 185)
    (408, Token (BACKQUOTE _)) -> Just (Shift 412)
    (409, Token (BACKQUOTE _)) -> Just (Shift 413)
    (410, Token (BACKQUOTE _)) -> Just (Shift 414)
    (411, Token (RBRACE _)) -> Just (Reduce 1 194)
    (411, Token (LPAREN _)) -> Just (Reduce 1 194)
    (411, Token (COMMA _)) -> Just (Reduce 1 194)
    (411, Token (SEMICOLON _)) -> Just (Reduce 1 194)
    (411, Token (MINUS _)) -> Just (Reduce 1 194)
    (411, Token (QCONID _)) -> Just (Reduce 1 194)
    (411, Token (EXPORT _)) -> Just (Reduce 1 194)
    (411, Token (AS _)) -> Just (Reduce 1 194)
    (411, Token (QVARID _)) -> Just (Reduce 1 194)
    (411, Token (QVARSYM _)) -> Just (Reduce 1 194)
    (411, Token (BACKQUOTE _)) -> Just (Reduce 1 194)
    (411, Token (QCONSYM _)) -> Just (Reduce 1 194)
    (412, Token (RBRACE _)) -> Just (Reduce 3 196)
    (412, Token (LPAREN _)) -> Just (Reduce 3 196)
    (412, Token (COMMA _)) -> Just (Reduce 3 196)
    (412, Token (SEMICOLON _)) -> Just (Reduce 3 196)
    (412, Token (MINUS _)) -> Just (Reduce 3 196)
    (412, Token (QCONID _)) -> Just (Reduce 3 196)
    (412, Token (EXPORT _)) -> Just (Reduce 3 196)
    (412, Token (AS _)) -> Just (Reduce 3 196)
    (412, Token (QVARID _)) -> Just (Reduce 3 196)
    (412, Token (QVARSYM _)) -> Just (Reduce 3 196)
    (412, Token (BACKQUOTE _)) -> Just (Reduce 3 196)
    (412, Token (QCONSYM _)) -> Just (Reduce 3 196)
    (413, Token (RBRACE _)) -> Just (Reduce 3 195)
    (413, Token (LPAREN _)) -> Just (Reduce 3 195)
    (413, Token (COMMA _)) -> Just (Reduce 3 195)
    (413, Token (SEMICOLON _)) -> Just (Reduce 3 195)
    (413, Token (MINUS _)) -> Just (Reduce 3 195)
    (413, Token (QCONID _)) -> Just (Reduce 3 195)
    (413, Token (EXPORT _)) -> Just (Reduce 3 195)
    (413, Token (AS _)) -> Just (Reduce 3 195)
    (413, Token (QVARID _)) -> Just (Reduce 3 195)
    (413, Token (QVARSYM _)) -> Just (Reduce 3 195)
    (413, Token (BACKQUOTE _)) -> Just (Reduce 3 195)
    (413, Token (QCONSYM _)) -> Just (Reduce 3 195)
    (414, Token (RBRACE _)) -> Just (Reduce 3 197)
    (414, Token (LPAREN _)) -> Just (Reduce 3 197)
    (414, Token (COMMA _)) -> Just (Reduce 3 197)
    (414, Token (SEMICOLON _)) -> Just (Reduce 3 197)
    (414, Token (MINUS _)) -> Just (Reduce 3 197)
    (414, Token (QCONID _)) -> Just (Reduce 3 197)
    (414, Token (EXPORT _)) -> Just (Reduce 3 197)
    (414, Token (AS _)) -> Just (Reduce 3 197)
    (414, Token (QVARID _)) -> Just (Reduce 3 197)
    (414, Token (QVARSYM _)) -> Just (Reduce 3 197)
    (414, Token (BACKQUOTE _)) -> Just (Reduce 3 197)
    (414, Token (QCONSYM _)) -> Just (Reduce 3 197)
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
production 169 = 64
production 170 = 64
production 171 = 65
production 172 = 65
production 173 = 65
production 174 = 65
production 175 = 65
production 176 = 65
production 177 = 65
production 178 = 65
production 179 = 65
production 180 = 65
production 181 = 31
production 182 = 31
production 183 = 31
production 184 = 31
production 185 = 66
production 186 = 66
production 187 = 8
production 188 = 8
production 189 = 8
production 190 = 8
production 191 = 8
production 192 = 9
production 193 = 9
production 194 = 67
production 195 = 67
production 196 = 67
production 197 = 67
production 198 = 52
production 199 = 52
production 200 = 44
production 201 = 44
production 202 = 16
production 203 = 16
production 204 = 15
production 205 = 15
production 206 = 47
production 207 = 47
production 208 = 47
production 209 = 68
production 210 = 1
production 211 = 42
production 212 = 42
production 213 = 62
production 214 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 189
    (9, 1) -> Just 214
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 293
    (13, 14) -> Just 18
    (13, 27) -> Just 212
    (13, 30) -> Just 248
    (13, 31) -> Just 75
    (13, 40) -> Just 263
    (13, 41) -> Just 264
    (13, 66) -> Just 267
    (16, 4) -> Just 17
    (16, 8) -> Just 293
    (16, 14) -> Just 18
    (16, 27) -> Just 212
    (16, 30) -> Just 248
    (16, 31) -> Just 75
    (16, 40) -> Just 263
    (16, 41) -> Just 264
    (16, 66) -> Just 267
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 163
    (25, 9) -> Just 164
    (25, 10) -> Just 33
    (25, 13) -> Just 153
    (34, 8) -> Just 398
    (34, 32) -> Just 366
    (34, 61) -> Just 271
    (34, 63) -> Just 380
    (34, 64) -> Just 45
    (34, 65) -> Just 382
    (35, 8) -> Just 398
    (35, 32) -> Just 367
    (35, 61) -> Just 271
    (35, 63) -> Just 380
    (35, 64) -> Just 45
    (35, 65) -> Just 382
    (36, 8) -> Just 398
    (36, 32) -> Just 368
    (36, 61) -> Just 271
    (36, 63) -> Just 380
    (36, 64) -> Just 45
    (36, 65) -> Just 382
    (37, 8) -> Just 398
    (37, 32) -> Just 371
    (37, 61) -> Just 271
    (37, 63) -> Just 380
    (37, 64) -> Just 45
    (37, 65) -> Just 382
    (38, 8) -> Just 398
    (38, 32) -> Just 372
    (38, 61) -> Just 271
    (38, 63) -> Just 380
    (38, 64) -> Just 45
    (38, 65) -> Just 382
    (39, 8) -> Just 398
    (39, 32) -> Just 373
    (39, 61) -> Just 271
    (39, 63) -> Just 380
    (39, 64) -> Just 45
    (39, 65) -> Just 382
    (40, 8) -> Just 398
    (40, 32) -> Just 374
    (40, 61) -> Just 271
    (40, 63) -> Just 380
    (40, 64) -> Just 45
    (40, 65) -> Just 382
    (41, 8) -> Just 398
    (41, 32) -> Just 375
    (41, 61) -> Just 271
    (41, 63) -> Just 380
    (41, 64) -> Just 45
    (41, 65) -> Just 382
    (42, 8) -> Just 398
    (42, 32) -> Just 376
    (42, 61) -> Just 271
    (42, 63) -> Just 380
    (42, 64) -> Just 45
    (42, 65) -> Just 382
    (43, 8) -> Just 398
    (43, 32) -> Just 377
    (43, 61) -> Just 271
    (43, 63) -> Just 380
    (43, 64) -> Just 45
    (43, 65) -> Just 382
    (44, 8) -> Just 398
    (44, 64) -> Just 46
    (44, 65) -> Just 382
    (45, 8) -> Just 398
    (45, 65) -> Just 383
    (46, 8) -> Just 398
    (46, 65) -> Just 383
    (47, 8) -> Just 398
    (47, 32) -> Just 249
    (47, 61) -> Just 271
    (47, 63) -> Just 380
    (47, 64) -> Just 45
    (47, 65) -> Just 382
    (48, 8) -> Just 398
    (48, 32) -> Just 272
    (48, 61) -> Just 271
    (48, 63) -> Just 380
    (48, 64) -> Just 45
    (48, 65) -> Just 382
    (49, 8) -> Just 398
    (49, 32) -> Just 282
    (49, 61) -> Just 271
    (49, 63) -> Just 380
    (49, 64) -> Just 45
    (49, 65) -> Just 382
    (50, 8) -> Just 398
    (50, 32) -> Just 290
    (50, 61) -> Just 271
    (50, 63) -> Just 380
    (50, 64) -> Just 45
    (50, 65) -> Just 382
    (51, 8) -> Just 398
    (51, 32) -> Just 399
    (51, 61) -> Just 271
    (51, 63) -> Just 380
    (51, 64) -> Just 45
    (51, 65) -> Just 382
    (52, 8) -> Just 398
    (52, 64) -> Just 46
    (52, 65) -> Just 382
    (53, 8) -> Just 398
    (53, 61) -> Just 400
    (53, 63) -> Just 380
    (53, 64) -> Just 45
    (53, 65) -> Just 382
    (54, 8) -> Just 398
    (54, 61) -> Just 401
    (54, 63) -> Just 380
    (54, 64) -> Just 45
    (54, 65) -> Just 382
    (55, 8) -> Just 398
    (55, 61) -> Just 402
    (55, 63) -> Just 380
    (55, 64) -> Just 45
    (55, 65) -> Just 382
    (56, 8) -> Just 398
    (56, 61) -> Just 403
    (56, 63) -> Just 380
    (56, 64) -> Just 45
    (56, 65) -> Just 382
    (57, 8) -> Just 398
    (57, 61) -> Just 404
    (57, 63) -> Just 380
    (57, 64) -> Just 45
    (57, 65) -> Just 382
    (58, 8) -> Just 398
    (58, 61) -> Just 405
    (58, 63) -> Just 380
    (58, 64) -> Just 45
    (58, 65) -> Just 382
    (59, 8) -> Just 398
    (59, 33) -> Just 250
    (59, 59) -> Just 274
    (59, 60) -> Just 354
    (59, 61) -> Just 357
    (59, 63) -> Just 380
    (59, 64) -> Just 45
    (59, 65) -> Just 382
    (60, 8) -> Just 398
    (60, 33) -> Just 273
    (60, 59) -> Just 274
    (60, 60) -> Just 354
    (60, 61) -> Just 357
    (60, 63) -> Just 380
    (60, 64) -> Just 45
    (60, 65) -> Just 382
    (61, 8) -> Just 398
    (61, 33) -> Just 283
    (61, 59) -> Just 274
    (61, 60) -> Just 354
    (61, 61) -> Just 357
    (61, 63) -> Just 380
    (61, 64) -> Just 45
    (61, 65) -> Just 382
    (62, 8) -> Just 398
    (62, 33) -> Just 291
    (62, 59) -> Just 274
    (62, 60) -> Just 354
    (62, 61) -> Just 357
    (62, 63) -> Just 380
    (62, 64) -> Just 45
    (62, 65) -> Just 382
    (63, 8) -> Just 398
    (63, 59) -> Just 353
    (63, 60) -> Just 354
    (63, 61) -> Just 357
    (63, 63) -> Just 380
    (63, 64) -> Just 45
    (63, 65) -> Just 382
    (64, 8) -> Just 398
    (64, 32) -> Just 369
    (64, 61) -> Just 271
    (64, 63) -> Just 380
    (64, 64) -> Just 45
    (64, 65) -> Just 382
    (65, 8) -> Just 398
    (65, 32) -> Just 370
    (65, 61) -> Just 271
    (65, 63) -> Just 380
    (65, 64) -> Just 45
    (65, 65) -> Just 382
    (66, 8) -> Just 398
    (66, 32) -> Just 356
    (66, 61) -> Just 271
    (66, 63) -> Just 380
    (66, 64) -> Just 45
    (66, 65) -> Just 382
    (67, 8) -> Just 407
    (67, 66) -> Just 268
    (68, 8) -> Just 407
    (68, 66) -> Just 270
    (69, 8) -> Just 407
    (69, 31) -> Just 70
    (69, 66) -> Just 267
    (70, 8) -> Just 407
    (70, 44) -> Just 68
    (70, 52) -> Just 305
    (70, 66) -> Just 269
    (70, 67) -> Just 306
    (71, 8) -> Just 293
    (71, 27) -> Just 259
    (71, 29) -> Just 258
    (71, 30) -> Just 248
    (71, 31) -> Just 75
    (71, 40) -> Just 263
    (71, 41) -> Just 264
    (71, 66) -> Just 267
    (72, 8) -> Just 293
    (72, 27) -> Just 259
    (72, 29) -> Just 260
    (72, 30) -> Just 248
    (72, 31) -> Just 75
    (72, 40) -> Just 263
    (72, 41) -> Just 264
    (72, 66) -> Just 267
    (73, 8) -> Just 293
    (73, 30) -> Just 281
    (73, 31) -> Just 78
    (73, 35) -> Just 276
    (73, 36) -> Just 278
    (73, 40) -> Just 263
    (73, 41) -> Just 264
    (73, 66) -> Just 267
    (74, 8) -> Just 293
    (74, 30) -> Just 281
    (74, 31) -> Just 78
    (74, 35) -> Just 277
    (74, 36) -> Just 278
    (74, 40) -> Just 263
    (74, 41) -> Just 264
    (74, 66) -> Just 267
    (75, 8) -> Just 407
    (75, 44) -> Just 68
    (75, 52) -> Just 305
    (75, 66) -> Just 269
    (75, 67) -> Just 306
    (76, 8) -> Just 407
    (76, 31) -> Just 79
    (76, 38) -> Just 285
    (76, 39) -> Just 287
    (76, 66) -> Just 267
    (77, 8) -> Just 407
    (77, 31) -> Just 79
    (77, 38) -> Just 286
    (77, 39) -> Just 287
    (77, 66) -> Just 267
    (78, 8) -> Just 407
    (78, 44) -> Just 68
    (78, 52) -> Just 305
    (78, 66) -> Just 269
    (78, 67) -> Just 306
    (79, 8) -> Just 407
    (79, 44) -> Just 68
    (79, 52) -> Just 305
    (79, 66) -> Just 269
    (79, 67) -> Just 306
    (80, 8) -> Just 407
    (80, 31) -> Just 81
    (80, 66) -> Just 267
    (81, 8) -> Just 407
    (81, 44) -> Just 68
    (81, 52) -> Just 305
    (81, 66) -> Just 269
    (81, 67) -> Just 306
    (82, 8) -> Just 160
    (82, 9) -> Just 161
    (82, 11) -> Just 154
    (82, 12) -> Just 155
    (83, 8) -> Just 160
    (83, 9) -> Just 161
    (83, 11) -> Just 190
    (83, 12) -> Just 155
    (84, 8) -> Just 160
    (84, 9) -> Just 161
    (84, 11) -> Just 191
    (84, 12) -> Just 155
    (85, 8) -> Just 163
    (85, 9) -> Just 164
    (85, 10) -> Just 152
    (85, 13) -> Just 153
    (86, 8) -> Just 163
    (86, 9) -> Just 164
    (86, 10) -> Just 162
    (86, 13) -> Just 153
    (87, 8) -> Just 292
    (87, 40) -> Just 294
    (88, 8) -> Just 292
    (88, 40) -> Just 344
    (88, 53) -> Just 335
    (88, 54) -> Just 342
    (89, 8) -> Just 292
    (89, 40) -> Just 344
    (89, 53) -> Just 341
    (89, 54) -> Just 342
    (90, 8) -> Just 224
    (91, 8) -> Just 235
    (92, 8) -> Just 236
    (93, 8) -> Just 237
    (103, 9) -> Just 321
    (103, 45) -> Just 312
    (103, 46) -> Just 313
    (103, 47) -> Just 314
    (104, 9) -> Just 321
    (104, 17) -> Just 105
    (104, 45) -> Just 215
    (104, 46) -> Just 313
    (104, 47) -> Just 314
    (105, 9) -> Just 321
    (105, 23) -> Just 207
    (105, 45) -> Just 216
    (105, 46) -> Just 313
    (105, 47) -> Just 314
    (106, 9) -> Just 321
    (106, 17) -> Just 107
    (106, 45) -> Just 215
    (106, 46) -> Just 313
    (106, 47) -> Just 314
    (107, 9) -> Just 321
    (107, 24) -> Just 209
    (107, 45) -> Just 216
    (107, 46) -> Just 313
    (107, 47) -> Just 314
    (108, 9) -> Just 321
    (108, 17) -> Just 109
    (108, 45) -> Just 215
    (108, 46) -> Just 313
    (108, 47) -> Just 314
    (109, 9) -> Just 321
    (109, 23) -> Just 206
    (109, 45) -> Just 216
    (109, 46) -> Just 313
    (109, 47) -> Just 314
    (110, 9) -> Just 321
    (110, 17) -> Just 111
    (110, 45) -> Just 215
    (110, 46) -> Just 313
    (110, 47) -> Just 314
    (111, 9) -> Just 321
    (111, 24) -> Just 208
    (111, 45) -> Just 216
    (111, 46) -> Just 313
    (111, 47) -> Just 314
    (112, 9) -> Just 321
    (112, 17) -> Just 113
    (112, 18) -> Just 364
    (112, 45) -> Just 215
    (112, 46) -> Just 313
    (112, 47) -> Just 314
    (113, 9) -> Just 321
    (113, 45) -> Just 216
    (113, 46) -> Just 313
    (113, 47) -> Just 314
    (114, 9) -> Just 321
    (114, 17) -> Just 116
    (114, 18) -> Just 217
    (114, 45) -> Just 215
    (114, 46) -> Just 313
    (114, 47) -> Just 314
    (115, 9) -> Just 321
    (115, 17) -> Just 116
    (115, 18) -> Just 363
    (115, 45) -> Just 215
    (115, 46) -> Just 313
    (115, 47) -> Just 314
    (116, 9) -> Just 321
    (116, 45) -> Just 216
    (116, 46) -> Just 313
    (116, 47) -> Just 314
    (117, 9) -> Just 321
    (117, 17) -> Just 118
    (117, 45) -> Just 215
    (117, 46) -> Just 313
    (117, 47) -> Just 314
    (118, 9) -> Just 321
    (118, 19) -> Just 194
    (118, 45) -> Just 216
    (118, 46) -> Just 313
    (118, 47) -> Just 314
    (119, 9) -> Just 321
    (119, 17) -> Just 120
    (119, 45) -> Just 215
    (119, 46) -> Just 313
    (119, 47) -> Just 314
    (120, 9) -> Just 321
    (120, 19) -> Just 195
    (120, 45) -> Just 216
    (120, 46) -> Just 313
    (120, 47) -> Just 314
    (121, 9) -> Just 322
    (121, 17) -> Just 124
    (121, 45) -> Just 215
    (121, 46) -> Just 313
    (121, 47) -> Just 314
    (121, 50) -> Just 218
    (121, 51) -> Just 332
    (122, 9) -> Just 322
    (122, 17) -> Just 124
    (122, 45) -> Just 215
    (122, 46) -> Just 313
    (122, 47) -> Just 314
    (122, 50) -> Just 331
    (122, 51) -> Just 332
    (123, 9) -> Just 136
    (124, 9) -> Just 321
    (124, 45) -> Just 216
    (124, 46) -> Just 313
    (124, 47) -> Just 314
    (124, 52) -> Just 125
    (125, 9) -> Just 321
    (125, 17) -> Just 126
    (125, 45) -> Just 215
    (125, 46) -> Just 313
    (125, 47) -> Just 314
    (126, 9) -> Just 321
    (126, 45) -> Just 216
    (126, 46) -> Just 313
    (126, 47) -> Just 314
    (127, 9) -> Just 321
    (127, 17) -> Just 128
    (127, 18) -> Just 262
    (127, 45) -> Just 215
    (127, 46) -> Just 313
    (127, 47) -> Just 314
    (128, 9) -> Just 321
    (128, 45) -> Just 216
    (128, 46) -> Just 313
    (128, 47) -> Just 314
    (129, 9) -> Just 321
    (129, 17) -> Just 116
    (129, 18) -> Just 193
    (129, 45) -> Just 215
    (129, 46) -> Just 313
    (129, 47) -> Just 314
    (130, 9) -> Just 321
    (130, 17) -> Just 116
    (130, 18) -> Just 238
    (130, 45) -> Just 215
    (130, 46) -> Just 313
    (130, 47) -> Just 314
    (131, 9) -> Just 321
    (131, 17) -> Just 116
    (131, 18) -> Just 239
    (131, 45) -> Just 215
    (131, 46) -> Just 313
    (131, 47) -> Just 314
    (132, 9) -> Just 321
    (132, 17) -> Just 116
    (132, 18) -> Just 240
    (132, 45) -> Just 215
    (132, 46) -> Just 313
    (132, 47) -> Just 314
    (133, 9) -> Just 321
    (133, 17) -> Just 116
    (133, 18) -> Just 261
    (133, 45) -> Just 215
    (133, 46) -> Just 313
    (133, 47) -> Just 314
    (134, 9) -> Just 321
    (134, 17) -> Just 116
    (134, 18) -> Just 343
    (134, 45) -> Just 215
    (134, 46) -> Just 313
    (134, 47) -> Just 314
    (135, 9) -> Just 321
    (135, 17) -> Just 116
    (135, 18) -> Just 225
    (135, 45) -> Just 215
    (135, 46) -> Just 313
    (135, 47) -> Just 314
    (136, 9) -> Just 321
    (136, 45) -> Just 226
    (136, 46) -> Just 313
    (136, 47) -> Just 314
    (137, 9) -> Just 321
    (137, 17) -> Just 138
    (137, 45) -> Just 215
    (137, 46) -> Just 313
    (137, 47) -> Just 314
    (138, 9) -> Just 321
    (138, 22) -> Just 205
    (138, 45) -> Just 216
    (138, 46) -> Just 313
    (138, 47) -> Just 314
    (139, 9) -> Just 321
    (139, 17) -> Just 141
    (139, 45) -> Just 215
    (139, 46) -> Just 313
    (139, 47) -> Just 314
    (140, 9) -> Just 321
    (140, 17) -> Just 142
    (140, 45) -> Just 215
    (140, 46) -> Just 313
    (140, 47) -> Just 314
    (141, 9) -> Just 321
    (141, 45) -> Just 216
    (141, 46) -> Just 313
    (141, 47) -> Just 314
    (142, 9) -> Just 321
    (142, 22) -> Just 204
    (142, 45) -> Just 216
    (142, 46) -> Just 313
    (142, 47) -> Just 314
    (143, 9) -> Just 321
    (143, 17) -> Just 116
    (143, 18) -> Just 310
    (143, 45) -> Just 215
    (143, 46) -> Just 313
    (143, 47) -> Just 314
    (143, 48) -> Just 315
    (143, 49) -> Just 323
    (144, 9) -> Just 321
    (144, 17) -> Just 116
    (144, 18) -> Just 231
    (144, 25) -> Just 210
    (144, 45) -> Just 215
    (144, 46) -> Just 313
    (144, 47) -> Just 314
    (145, 9) -> Just 321
    (145, 17) -> Just 116
    (145, 18) -> Just 231
    (145, 25) -> Just 232
    (145, 45) -> Just 215
    (145, 46) -> Just 313
    (145, 47) -> Just 314
    (146, 9) -> Just 321
    (146, 17) -> Just 116
    (146, 18) -> Just 327
    (146, 45) -> Just 215
    (146, 46) -> Just 313
    (146, 47) -> Just 314
    (146, 48) -> Just 328
    (147, 9) -> Just 321
    (147, 17) -> Just 116
    (147, 18) -> Just 311
    (147, 45) -> Just 215
    (147, 46) -> Just 313
    (147, 47) -> Just 314
    (165, 20) -> Just 221
    (165, 21) -> Just 200
    (166, 20) -> Just 221
    (166, 21) -> Just 201
    (167, 20) -> Just 221
    (167, 21) -> Just 202
    (168, 20) -> Just 221
    (168, 21) -> Just 203
    (181, 15) -> Just 8
    (183, 20) -> Just 196
    (184, 20) -> Just 197
    (185, 20) -> Just 198
    (186, 20) -> Just 199
    (188, 26) -> Just 211
    (189, 16) -> Just 192
    (219, 20) -> Just 221
    (219, 21) -> Just 222
    (227, 34) -> Just 228
    (229, 37) -> Just 230
    (233, 55) -> Just 241
    (234, 55) -> Just 242
    (241, 56) -> Just 91
    (241, 57) -> Just 243
    (242, 58) -> Just 93
    (243, 56) -> Just 92
    (244, 28) -> Just 246
    (245, 28) -> Just 247
    (251, 28) -> Just 279
    (252, 28) -> Just 280
    (253, 28) -> Just 288
    (254, 28) -> Just 289
    (255, 28) -> Just 355
    (256, 28) -> Just 365
    (264, 42) -> Just 265
    (265, 43) -> Just 266
    (265, 44) -> Just 304
    (265, 52) -> Just 305
    (265, 67) -> Just 306
    (299, 43) -> Just 302
    (299, 44) -> Just 304
    (299, 52) -> Just 305
    (299, 67) -> Just 306
    (300, 43) -> Just 303
    (300, 44) -> Just 304
    (300, 52) -> Just 305
    (300, 67) -> Just 306
    (329, 49) -> Just 330
    (369, 62) -> Just 378
    (370, 62) -> Just 379
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
                      Monad.liftM StackValue_lexp $ lexp_implies_MINUS_fexp actions (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QVARSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_AS_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_EXPORT_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QVARID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_QCONSYM_infixexp_RPAREN actions (case snd (pop !! 3) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_BACKQUOTE_QCONID_BACKQUOTE_infixexp_RPAREN actions (case snd (pop !! 5) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    198 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    201 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    203 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    204 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    205 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    206 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    207 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    208 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    209 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    210 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    211 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    212 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    213 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    214 ->
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
  , lexp_implies_MINUS_fexp = \mINUS0 fexp1 ->
      return $ Lexp_implies_MINUS_fexp mINUS0 fexp1
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

