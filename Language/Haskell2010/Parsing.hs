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
    Exp_implies_LAMBDA_pat_RARROW_exp LAMBDA Pat RARROW Exp
  | Exp_implies_LET_decls_IN_exp LET Decls IN Exp
  | Exp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp IF Exp Semicolon_opt THEN Exp Semicolon_opt ELSE Exp
  | Exp_implies_infixexp_COLON_COLON_type' Infixexp COLON_COLON Type'
  | Exp_implies_infixexp_COLON_COLON_btype_DARROW_type' Infixexp COLON_COLON Btype DARROW Type'
  | Exp_implies_infixexp Infixexp
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
    Infixexp_implies_lexp Lexp
  deriving (Eq, Ord, Read, Show)

data Semicolon_opt =
    Semicolon_opt_implies
  | Semicolon_opt_implies_SEMICOLON SEMICOLON
  deriving (Eq, Ord, Read, Show)

data Lexp =
    Lexp_implies_MINUS_lexp MINUS Lexp
  | Lexp_implies_fexp Fexp
  deriving (Eq, Ord, Read, Show)

data Fexp =
    Fexp_implies_aexp Aexp
  | Fexp_implies_fexp_aexp Fexp Aexp
  | Fexp_implies_fexp_MINUS_aexp Fexp MINUS Aexp
  | Fexp_implies_fexp_op_aexp Fexp Op Aexp
  deriving (Eq, Ord, Read, Show)

data Aexp =
    Aexp_implies_var Var
  | Aexp_implies_INTEGER INTEGER
  | Aexp_implies_STRING STRING
  | Aexp_implies_LPAREN_exp_RPAREN LPAREN Exp RPAREN
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
  | StackValue_INTEGER INTEGER
  | StackValue_QVARSYM QVARSYM
  | StackValue_QCONSYM QCONSYM
  | StackValue_BACKQUOTE BACKQUOTE
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
  , exp_implies_LAMBDA_pat_RARROW_exp :: LAMBDA -> Pat -> RARROW -> Exp -> m Exp
  , exp_implies_LET_decls_IN_exp :: LET -> Decls -> IN -> Exp -> m Exp
  , exp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp :: IF -> Exp -> Semicolon_opt -> THEN -> Exp -> Semicolon_opt -> ELSE -> Exp -> m Exp
  , exp_implies_infixexp_COLON_COLON_type' :: Infixexp -> COLON_COLON -> Type' -> m Exp
  , exp_implies_infixexp_COLON_COLON_btype_DARROW_type' :: Infixexp -> COLON_COLON -> Btype -> DARROW -> Type' -> m Exp
  , exp_implies_infixexp :: Infixexp -> m Exp
  , infixexp_implies_lexp :: Lexp -> m Infixexp
  , lexp_implies_MINUS_lexp :: MINUS -> Lexp -> m Lexp
  , lexp_implies_fexp :: Fexp -> m Lexp
  , fexp_implies_aexp :: Aexp -> m Fexp
  , fexp_implies_fexp_aexp :: Fexp -> Aexp -> m Fexp
  , fexp_implies_fexp_MINUS_aexp :: Fexp -> MINUS -> Aexp -> m Fexp
  , fexp_implies_fexp_op_aexp :: Fexp -> Op -> Aexp -> m Fexp
  , aexp_implies_var :: Var -> m Aexp
  , aexp_implies_INTEGER :: INTEGER -> m Aexp
  , aexp_implies_STRING :: STRING -> m Aexp
  , aexp_implies_LPAREN_exp_RPAREN :: LPAREN -> Exp -> RPAREN -> m Aexp
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
    (11, Token (MODULE _)) -> Just (Reduce 1 199)
    (11, Token (WHERE _)) -> Just (Reduce 1 199)
    (11, Token (RBRACE _)) -> Just (Reduce 1 199)
    (11, Token (LPAREN _)) -> Just (Reduce 1 199)
    (11, Token (RPAREN _)) -> Just (Reduce 1 199)
    (11, Token (COMMA _)) -> Just (Reduce 1 199)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 199)
    (11, Token (HIDING _)) -> Just (Reduce 1 199)
    (11, Token (MINUS _)) -> Just (Reduce 1 199)
    (11, Token (QCONID _)) -> Just (Reduce 1 199)
    (11, Token (EXPORT _)) -> Just (Reduce 1 199)
    (11, Token (AS _)) -> Just (Reduce 1 199)
    (11, Token (QVARID _)) -> Just (Reduce 1 199)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 199)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 199)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 57)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 169)
    (13, Token (TYPE _)) -> Just (Shift 127)
    (13, Token (DATA _)) -> Just (Shift 105)
    (13, Token (NEWTYPE _)) -> Just (Shift 125)
    (13, Token (CLASS _)) -> Just (Shift 92)
    (13, Token (INSTANCE _)) -> Just (Shift 94)
    (13, Token (DEFAULT _)) -> Just (Shift 175)
    (13, Token (FOREIGN _)) -> Just (Shift 176)
    (13, Token (INFIXL _)) -> Just (Shift 293)
    (13, Token (INFIXR _)) -> Just (Shift 294)
    (13, Token (INFIX _)) -> Just (Shift 295)
    (13, Token (EXPORT _)) -> Just (Shift 87)
    (13, Token (AS _)) -> Just (Shift 88)
    (13, Token (QVARID _)) -> Just (Shift 89)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 57)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 169)
    (16, Token (TYPE _)) -> Just (Shift 127)
    (16, Token (DATA _)) -> Just (Shift 105)
    (16, Token (NEWTYPE _)) -> Just (Shift 125)
    (16, Token (CLASS _)) -> Just (Shift 92)
    (16, Token (INSTANCE _)) -> Just (Shift 94)
    (16, Token (DEFAULT _)) -> Just (Shift 175)
    (16, Token (FOREIGN _)) -> Just (Shift 176)
    (16, Token (INFIXL _)) -> Just (Shift 293)
    (16, Token (INFIXR _)) -> Just (Shift 294)
    (16, Token (INFIX _)) -> Just (Shift 295)
    (16, Token (EXPORT _)) -> Just (Shift 87)
    (16, Token (AS _)) -> Just (Shift 88)
    (16, Token (QVARID _)) -> Just (Shift 89)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 82)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 138)
    (19, Token (EXPORT _)) -> Just (Shift 87)
    (19, Token (AS _)) -> Just (Shift 88)
    (19, Token (QVARID _)) -> Just (Shift 89)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 82)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 138)
    (22, Token (EXPORT _)) -> Just (Shift 87)
    (22, Token (AS _)) -> Just (Shift 88)
    (22, Token (QVARID _)) -> Just (Shift 89)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 82)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 138)
    (25, Token (EXPORT _)) -> Just (Shift 87)
    (25, Token (AS _)) -> Just (Shift 88)
    (25, Token (QVARID _)) -> Just (Shift 89)
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
    (34, Token (WHERE _)) -> Just (Reduce 1 161)
    (34, Token (RBRACE _)) -> Just (Reduce 1 161)
    (34, Token (LPAREN _)) -> Just (Shift 45)
    (34, Token (RPAREN _)) -> Just (Reduce 1 161)
    (34, Token (COMMA _)) -> Just (Reduce 1 161)
    (34, Token (SEMICOLON _)) -> Just (Reduce 1 161)
    (34, Token (EQUAL _)) -> Just (Reduce 1 161)
    (34, Token (PIPE _)) -> Just (Reduce 1 161)
    (34, Token (COLON_COLON _)) -> Just (Reduce 1 161)
    (34, Token (MINUS _)) -> Just (Shift 36)
    (34, Token (EXPORT _)) -> Just (Shift 87)
    (34, Token (AS _)) -> Just (Shift 88)
    (34, Token (QVARID _)) -> Just (Shift 89)
    (34, Token (STRING _)) -> Just (Shift 364)
    (34, Token (LARROW _)) -> Just (Reduce 1 161)
    (34, Token (THEN _)) -> Just (Reduce 1 161)
    (34, Token (ELSE _)) -> Just (Reduce 1 161)
    (34, Token (INTEGER _)) -> Just (Shift 365)
    (34, Token (QVARSYM _)) -> Just (Shift 373)
    (34, Token (QCONSYM _)) -> Just (Shift 335)
    (34, Token (BACKQUOTE _)) -> Just (Shift 336)
    (35, Token (LPAREN _)) -> Just (Shift 45)
    (35, Token (MINUS _)) -> Just (Shift 35)
    (35, Token (EXPORT _)) -> Just (Shift 87)
    (35, Token (AS _)) -> Just (Shift 88)
    (35, Token (QVARID _)) -> Just (Shift 89)
    (35, Token (STRING _)) -> Just (Shift 364)
    (35, Token (INTEGER _)) -> Just (Shift 365)
    (36, Token (LPAREN _)) -> Just (Shift 45)
    (36, Token (EXPORT _)) -> Just (Shift 87)
    (36, Token (AS _)) -> Just (Shift 88)
    (36, Token (QVARID _)) -> Just (Shift 89)
    (36, Token (STRING _)) -> Just (Shift 364)
    (36, Token (INTEGER _)) -> Just (Shift 365)
    (37, Token (LPAREN _)) -> Just (Shift 45)
    (37, Token (EXPORT _)) -> Just (Shift 87)
    (37, Token (AS _)) -> Just (Shift 88)
    (37, Token (QVARID _)) -> Just (Shift 89)
    (37, Token (STRING _)) -> Just (Shift 364)
    (37, Token (INTEGER _)) -> Just (Shift 365)
    (38, Token (LPAREN _)) -> Just (Shift 45)
    (38, Token (MINUS _)) -> Just (Shift 35)
    (38, Token (EXPORT _)) -> Just (Shift 87)
    (38, Token (AS _)) -> Just (Shift 88)
    (38, Token (QVARID _)) -> Just (Shift 89)
    (38, Token (STRING _)) -> Just (Shift 364)
    (38, Token (LET _)) -> Just (Shift 244)
    (38, Token (LAMBDA _)) -> Just (Shift 68)
    (38, Token (IF _)) -> Just (Shift 52)
    (38, Token (INTEGER _)) -> Just (Shift 365)
    (39, Token (LPAREN _)) -> Just (Shift 45)
    (39, Token (MINUS _)) -> Just (Shift 35)
    (39, Token (EXPORT _)) -> Just (Shift 87)
    (39, Token (AS _)) -> Just (Shift 88)
    (39, Token (QVARID _)) -> Just (Shift 89)
    (39, Token (STRING _)) -> Just (Shift 364)
    (39, Token (LET _)) -> Just (Shift 244)
    (39, Token (LAMBDA _)) -> Just (Shift 68)
    (39, Token (IF _)) -> Just (Shift 52)
    (39, Token (INTEGER _)) -> Just (Shift 365)
    (40, Token (LPAREN _)) -> Just (Shift 45)
    (40, Token (MINUS _)) -> Just (Shift 35)
    (40, Token (EXPORT _)) -> Just (Shift 87)
    (40, Token (AS _)) -> Just (Shift 88)
    (40, Token (QVARID _)) -> Just (Shift 89)
    (40, Token (STRING _)) -> Just (Shift 364)
    (40, Token (LET _)) -> Just (Shift 244)
    (40, Token (LAMBDA _)) -> Just (Shift 68)
    (40, Token (IF _)) -> Just (Shift 52)
    (40, Token (INTEGER _)) -> Just (Shift 365)
    (41, Token (LPAREN _)) -> Just (Shift 45)
    (41, Token (MINUS _)) -> Just (Shift 35)
    (41, Token (EXPORT _)) -> Just (Shift 87)
    (41, Token (AS _)) -> Just (Shift 88)
    (41, Token (QVARID _)) -> Just (Shift 89)
    (41, Token (STRING _)) -> Just (Shift 364)
    (41, Token (LET _)) -> Just (Shift 244)
    (41, Token (LAMBDA _)) -> Just (Shift 68)
    (41, Token (IF _)) -> Just (Shift 52)
    (41, Token (INTEGER _)) -> Just (Shift 365)
    (42, Token (LPAREN _)) -> Just (Shift 45)
    (42, Token (MINUS _)) -> Just (Shift 35)
    (42, Token (EXPORT _)) -> Just (Shift 87)
    (42, Token (AS _)) -> Just (Shift 88)
    (42, Token (QVARID _)) -> Just (Shift 89)
    (42, Token (STRING _)) -> Just (Shift 364)
    (42, Token (LET _)) -> Just (Shift 244)
    (42, Token (LAMBDA _)) -> Just (Shift 68)
    (42, Token (IF _)) -> Just (Shift 52)
    (42, Token (INTEGER _)) -> Just (Shift 365)
    (43, Token (LPAREN _)) -> Just (Shift 45)
    (43, Token (MINUS _)) -> Just (Shift 35)
    (43, Token (EXPORT _)) -> Just (Shift 87)
    (43, Token (AS _)) -> Just (Shift 88)
    (43, Token (QVARID _)) -> Just (Shift 89)
    (43, Token (STRING _)) -> Just (Shift 364)
    (43, Token (LET _)) -> Just (Shift 244)
    (43, Token (LAMBDA _)) -> Just (Shift 68)
    (43, Token (IF _)) -> Just (Shift 52)
    (43, Token (INTEGER _)) -> Just (Shift 365)
    (44, Token (LPAREN _)) -> Just (Shift 45)
    (44, Token (MINUS _)) -> Just (Shift 35)
    (44, Token (EXPORT _)) -> Just (Shift 87)
    (44, Token (AS _)) -> Just (Shift 88)
    (44, Token (QVARID _)) -> Just (Shift 89)
    (44, Token (STRING _)) -> Just (Shift 364)
    (44, Token (LET _)) -> Just (Shift 244)
    (44, Token (LAMBDA _)) -> Just (Shift 68)
    (44, Token (IF _)) -> Just (Shift 52)
    (44, Token (INTEGER _)) -> Just (Shift 365)
    (45, Token (LPAREN _)) -> Just (Shift 45)
    (45, Token (MINUS _)) -> Just (Shift 46)
    (45, Token (EXPORT _)) -> Just (Shift 87)
    (45, Token (AS _)) -> Just (Shift 88)
    (45, Token (QVARID _)) -> Just (Shift 89)
    (45, Token (STRING _)) -> Just (Shift 364)
    (45, Token (LET _)) -> Just (Shift 244)
    (45, Token (LAMBDA _)) -> Just (Shift 68)
    (45, Token (IF _)) -> Just (Shift 52)
    (45, Token (INTEGER _)) -> Just (Shift 365)
    (45, Token (QVARSYM _)) -> Just (Shift 90)
    (46, Token (LPAREN _)) -> Just (Shift 45)
    (46, Token (RPAREN _)) -> Just (Shift 84)
    (46, Token (MINUS _)) -> Just (Shift 35)
    (46, Token (EXPORT _)) -> Just (Shift 87)
    (46, Token (AS _)) -> Just (Shift 88)
    (46, Token (QVARID _)) -> Just (Shift 89)
    (46, Token (STRING _)) -> Just (Shift 364)
    (46, Token (INTEGER _)) -> Just (Shift 365)
    (47, Token (LPAREN _)) -> Just (Shift 45)
    (47, Token (MINUS _)) -> Just (Shift 35)
    (47, Token (EXPORT _)) -> Just (Shift 87)
    (47, Token (AS _)) -> Just (Shift 88)
    (47, Token (QVARID _)) -> Just (Shift 89)
    (47, Token (STRING _)) -> Just (Shift 364)
    (47, Token (LET _)) -> Just (Shift 243)
    (47, Token (INTEGER _)) -> Just (Shift 365)
    (48, Token (LPAREN _)) -> Just (Shift 45)
    (48, Token (MINUS _)) -> Just (Shift 35)
    (48, Token (EXPORT _)) -> Just (Shift 87)
    (48, Token (AS _)) -> Just (Shift 88)
    (48, Token (QVARID _)) -> Just (Shift 89)
    (48, Token (STRING _)) -> Just (Shift 364)
    (48, Token (LET _)) -> Just (Shift 243)
    (48, Token (INTEGER _)) -> Just (Shift 365)
    (49, Token (LPAREN _)) -> Just (Shift 45)
    (49, Token (MINUS _)) -> Just (Shift 35)
    (49, Token (EXPORT _)) -> Just (Shift 87)
    (49, Token (AS _)) -> Just (Shift 88)
    (49, Token (QVARID _)) -> Just (Shift 89)
    (49, Token (STRING _)) -> Just (Shift 364)
    (49, Token (LET _)) -> Just (Shift 243)
    (49, Token (INTEGER _)) -> Just (Shift 365)
    (50, Token (LPAREN _)) -> Just (Shift 45)
    (50, Token (MINUS _)) -> Just (Shift 35)
    (50, Token (EXPORT _)) -> Just (Shift 87)
    (50, Token (AS _)) -> Just (Shift 88)
    (50, Token (QVARID _)) -> Just (Shift 89)
    (50, Token (STRING _)) -> Just (Shift 364)
    (50, Token (LET _)) -> Just (Shift 243)
    (50, Token (INTEGER _)) -> Just (Shift 365)
    (51, Token (LPAREN _)) -> Just (Shift 45)
    (51, Token (MINUS _)) -> Just (Shift 35)
    (51, Token (EXPORT _)) -> Just (Shift 87)
    (51, Token (AS _)) -> Just (Shift 88)
    (51, Token (QVARID _)) -> Just (Shift 89)
    (51, Token (STRING _)) -> Just (Shift 364)
    (51, Token (LET _)) -> Just (Shift 243)
    (51, Token (INTEGER _)) -> Just (Shift 365)
    (52, Token (LPAREN _)) -> Just (Shift 45)
    (52, Token (MINUS _)) -> Just (Shift 35)
    (52, Token (EXPORT _)) -> Just (Shift 87)
    (52, Token (AS _)) -> Just (Shift 88)
    (52, Token (QVARID _)) -> Just (Shift 89)
    (52, Token (STRING _)) -> Just (Shift 364)
    (52, Token (LET _)) -> Just (Shift 244)
    (52, Token (LAMBDA _)) -> Just (Shift 68)
    (52, Token (IF _)) -> Just (Shift 52)
    (52, Token (INTEGER _)) -> Just (Shift 365)
    (53, Token (LPAREN _)) -> Just (Shift 45)
    (53, Token (MINUS _)) -> Just (Shift 35)
    (53, Token (EXPORT _)) -> Just (Shift 87)
    (53, Token (AS _)) -> Just (Shift 88)
    (53, Token (QVARID _)) -> Just (Shift 89)
    (53, Token (STRING _)) -> Just (Shift 364)
    (53, Token (LET _)) -> Just (Shift 244)
    (53, Token (LAMBDA _)) -> Just (Shift 68)
    (53, Token (IF _)) -> Just (Shift 52)
    (53, Token (INTEGER _)) -> Just (Shift 365)
    (54, Token (LPAREN _)) -> Just (Shift 45)
    (54, Token (MINUS _)) -> Just (Shift 35)
    (54, Token (EXPORT _)) -> Just (Shift 87)
    (54, Token (AS _)) -> Just (Shift 88)
    (54, Token (QVARID _)) -> Just (Shift 89)
    (54, Token (STRING _)) -> Just (Shift 364)
    (54, Token (LET _)) -> Just (Shift 244)
    (54, Token (LAMBDA _)) -> Just (Shift 68)
    (54, Token (IF _)) -> Just (Shift 52)
    (54, Token (INTEGER _)) -> Just (Shift 365)
    (55, Token (LPAREN _)) -> Just (Shift 57)
    (55, Token (EXPORT _)) -> Just (Shift 87)
    (55, Token (AS _)) -> Just (Shift 88)
    (55, Token (QVARID _)) -> Just (Shift 89)
    (56, Token (LPAREN _)) -> Just (Shift 57)
    (56, Token (EXPORT _)) -> Just (Shift 87)
    (56, Token (AS _)) -> Just (Shift 88)
    (56, Token (QVARID _)) -> Just (Shift 89)
    (57, Token (LPAREN _)) -> Just (Shift 57)
    (57, Token (MINUS _)) -> Just (Shift 86)
    (57, Token (EXPORT _)) -> Just (Shift 87)
    (57, Token (AS _)) -> Just (Shift 88)
    (57, Token (QVARID _)) -> Just (Shift 89)
    (57, Token (QVARSYM _)) -> Just (Shift 90)
    (58, Token (LPAREN _)) -> Just (Shift 57)
    (58, Token (RPAREN _)) -> Just (Shift 368)
    (58, Token (MINUS _)) -> Just (Shift 55)
    (58, Token (EXPORT _)) -> Just (Shift 87)
    (58, Token (AS _)) -> Just (Shift 88)
    (58, Token (QVARID _)) -> Just (Shift 89)
    (58, Token (QVARSYM _)) -> Just (Shift 373)
    (58, Token (QCONSYM _)) -> Just (Shift 335)
    (58, Token (BACKQUOTE _)) -> Just (Shift 336)
    (59, Token (RBRACE _)) -> Just (Reduce 0 85)
    (59, Token (LPAREN _)) -> Just (Shift 57)
    (59, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (59, Token (INFIXL _)) -> Just (Shift 293)
    (59, Token (INFIXR _)) -> Just (Shift 294)
    (59, Token (INFIX _)) -> Just (Shift 295)
    (59, Token (EXPORT _)) -> Just (Shift 87)
    (59, Token (AS _)) -> Just (Shift 88)
    (59, Token (QVARID _)) -> Just (Shift 89)
    (60, Token (RBRACE _)) -> Just (Reduce 0 85)
    (60, Token (LPAREN _)) -> Just (Shift 57)
    (60, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (60, Token (INFIXL _)) -> Just (Shift 293)
    (60, Token (INFIXR _)) -> Just (Shift 294)
    (60, Token (INFIX _)) -> Just (Shift 295)
    (60, Token (EXPORT _)) -> Just (Shift 87)
    (60, Token (AS _)) -> Just (Shift 88)
    (60, Token (QVARID _)) -> Just (Shift 89)
    (61, Token (RBRACE _)) -> Just (Reduce 0 85)
    (61, Token (LPAREN _)) -> Just (Shift 57)
    (61, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (61, Token (INFIXL _)) -> Just (Shift 293)
    (61, Token (INFIXR _)) -> Just (Shift 294)
    (61, Token (INFIX _)) -> Just (Shift 295)
    (61, Token (EXPORT _)) -> Just (Shift 87)
    (61, Token (AS _)) -> Just (Shift 88)
    (61, Token (QVARID _)) -> Just (Shift 89)
    (62, Token (RBRACE _)) -> Just (Reduce 0 85)
    (62, Token (LPAREN _)) -> Just (Shift 57)
    (62, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (62, Token (INFIXL _)) -> Just (Shift 293)
    (62, Token (INFIXR _)) -> Just (Shift 294)
    (62, Token (INFIX _)) -> Just (Shift 295)
    (62, Token (EXPORT _)) -> Just (Shift 87)
    (62, Token (AS _)) -> Just (Shift 88)
    (62, Token (QVARID _)) -> Just (Shift 89)
    (63, Token (LPAREN _)) -> Just (Shift 57)
    (63, Token (EQUAL _)) -> Just (Shift 41)
    (63, Token (PIPE _)) -> Just (Shift 47)
    (63, Token (MINUS _)) -> Just (Shift 55)
    (63, Token (EXPORT _)) -> Just (Shift 87)
    (63, Token (AS _)) -> Just (Shift 88)
    (63, Token (QVARID _)) -> Just (Shift 89)
    (63, Token (QVARSYM _)) -> Just (Shift 373)
    (63, Token (QCONSYM _)) -> Just (Shift 335)
    (63, Token (BACKQUOTE _)) -> Just (Shift 336)
    (64, Token (RBRACE _)) -> Just (Reduce 0 80)
    (64, Token (LPAREN _)) -> Just (Shift 57)
    (64, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (64, Token (EXPORT _)) -> Just (Shift 87)
    (64, Token (AS _)) -> Just (Shift 88)
    (64, Token (QVARID _)) -> Just (Shift 89)
    (65, Token (RBRACE _)) -> Just (Reduce 0 80)
    (65, Token (LPAREN _)) -> Just (Shift 57)
    (65, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (65, Token (EXPORT _)) -> Just (Shift 87)
    (65, Token (AS _)) -> Just (Shift 88)
    (65, Token (QVARID _)) -> Just (Shift 89)
    (66, Token (LPAREN _)) -> Just (Shift 57)
    (66, Token (EQUAL _)) -> Just (Shift 43)
    (66, Token (PIPE _)) -> Just (Shift 49)
    (66, Token (MINUS _)) -> Just (Shift 55)
    (66, Token (EXPORT _)) -> Just (Shift 87)
    (66, Token (AS _)) -> Just (Shift 88)
    (66, Token (QVARID _)) -> Just (Shift 89)
    (66, Token (QVARSYM _)) -> Just (Shift 373)
    (66, Token (QCONSYM _)) -> Just (Shift 335)
    (66, Token (BACKQUOTE _)) -> Just (Shift 336)
    (67, Token (LPAREN _)) -> Just (Shift 57)
    (67, Token (EQUAL _)) -> Just (Shift 44)
    (67, Token (PIPE _)) -> Just (Shift 50)
    (67, Token (MINUS _)) -> Just (Shift 55)
    (67, Token (EXPORT _)) -> Just (Shift 87)
    (67, Token (AS _)) -> Just (Shift 88)
    (67, Token (QVARID _)) -> Just (Shift 89)
    (67, Token (QVARSYM _)) -> Just (Shift 373)
    (67, Token (QCONSYM _)) -> Just (Shift 335)
    (67, Token (BACKQUOTE _)) -> Just (Shift 336)
    (68, Token (LPAREN _)) -> Just (Shift 57)
    (68, Token (EXPORT _)) -> Just (Shift 87)
    (68, Token (AS _)) -> Just (Shift 88)
    (68, Token (QVARID _)) -> Just (Shift 89)
    (69, Token (LPAREN _)) -> Just (Shift 57)
    (69, Token (MINUS _)) -> Just (Shift 55)
    (69, Token (RARROW _)) -> Just (Shift 38)
    (69, Token (EXPORT _)) -> Just (Shift 87)
    (69, Token (AS _)) -> Just (Shift 88)
    (69, Token (QVARID _)) -> Just (Shift 89)
    (69, Token (QVARSYM _)) -> Just (Shift 373)
    (69, Token (QCONSYM _)) -> Just (Shift 335)
    (69, Token (BACKQUOTE _)) -> Just (Shift 336)
    (70, Token (LPAREN _)) -> Just (Shift 82)
    (70, Token (RPAREN _)) -> Just (Reduce 0 15)
    (70, Token (QCONID _)) -> Just (Shift 138)
    (70, Token (EXPORT _)) -> Just (Shift 87)
    (70, Token (AS _)) -> Just (Shift 88)
    (70, Token (QVARID _)) -> Just (Shift 89)
    (71, Token (LPAREN _)) -> Just (Shift 82)
    (71, Token (RPAREN _)) -> Just (Reduce 0 15)
    (71, Token (QCONID _)) -> Just (Shift 138)
    (71, Token (EXPORT _)) -> Just (Shift 87)
    (71, Token (AS _)) -> Just (Shift 88)
    (71, Token (QVARID _)) -> Just (Shift 89)
    (72, Token (LPAREN _)) -> Just (Shift 82)
    (72, Token (RPAREN _)) -> Just (Reduce 0 15)
    (72, Token (QCONID _)) -> Just (Shift 138)
    (72, Token (EXPORT _)) -> Just (Shift 87)
    (72, Token (AS _)) -> Just (Shift 88)
    (72, Token (QVARID _)) -> Just (Shift 89)
    (73, Token (LPAREN _)) -> Just (Shift 82)
    (73, Token (QCONID _)) -> Just (Shift 138)
    (73, Token (EXPORT _)) -> Just (Shift 87)
    (73, Token (AS _)) -> Just (Shift 88)
    (73, Token (QVARID _)) -> Just (Shift 89)
    (74, Token (LPAREN _)) -> Just (Shift 82)
    (74, Token (RPAREN _)) -> Just (Shift 144)
    (74, Token (DOT_DOT _)) -> Just (Shift 147)
    (74, Token (QCONID _)) -> Just (Shift 138)
    (74, Token (EXPORT _)) -> Just (Shift 87)
    (74, Token (AS _)) -> Just (Shift 88)
    (74, Token (QVARID _)) -> Just (Shift 89)
    (75, Token (LPAREN _)) -> Just (Shift 83)
    (75, Token (EXPORT _)) -> Just (Shift 87)
    (75, Token (AS _)) -> Just (Shift 88)
    (75, Token (QVARID _)) -> Just (Shift 89)
    (76, Token (RBRACE _)) -> Just (Shift 331)
    (76, Token (LPAREN _)) -> Just (Shift 83)
    (76, Token (EXPORT _)) -> Just (Shift 87)
    (76, Token (AS _)) -> Just (Shift 88)
    (76, Token (QVARID _)) -> Just (Shift 89)
    (77, Token (LPAREN _)) -> Just (Shift 83)
    (77, Token (EXPORT _)) -> Just (Shift 87)
    (77, Token (AS _)) -> Just (Shift 88)
    (77, Token (QVARID _)) -> Just (Shift 89)
    (78, Token (LPAREN _)) -> Just (Shift 83)
    (78, Token (EXPORT _)) -> Just (Shift 87)
    (78, Token (AS _)) -> Just (Shift 88)
    (78, Token (QVARID _)) -> Just (Shift 89)
    (79, Token (LPAREN _)) -> Just (Shift 83)
    (79, Token (EXPORT _)) -> Just (Shift 87)
    (79, Token (AS _)) -> Just (Shift 88)
    (79, Token (QVARID _)) -> Just (Shift 89)
    (80, Token (LPAREN _)) -> Just (Shift 83)
    (80, Token (EXPORT _)) -> Just (Shift 87)
    (80, Token (AS _)) -> Just (Shift 88)
    (80, Token (QVARID _)) -> Just (Shift 89)
    (81, Token (LPAREN _)) -> Just (Shift 83)
    (81, Token (EXPORT _)) -> Just (Shift 87)
    (81, Token (AS _)) -> Just (Shift 88)
    (81, Token (QVARID _)) -> Just (Shift 89)
    (82, Token (MINUS _)) -> Just (Shift 86)
    (82, Token (QVARSYM _)) -> Just (Shift 90)
    (82, Token (QCONSYM _)) -> Just (Shift 139)
    (83, Token (MINUS _)) -> Just (Shift 86)
    (83, Token (QVARSYM _)) -> Just (Shift 90)
    (84, Token (WHERE _)) -> Just (Reduce 3 179)
    (84, Token (LBRACE _)) -> Just (Reduce 3 179)
    (84, Token (RBRACE _)) -> Just (Reduce 3 179)
    (84, Token (LPAREN _)) -> Just (Reduce 3 179)
    (84, Token (RPAREN _)) -> Just (Reduce 3 179)
    (84, Token (COMMA _)) -> Just (Reduce 3 179)
    (84, Token (SEMICOLON _)) -> Just (Reduce 3 179)
    (84, Token (EQUAL _)) -> Just (Reduce 3 179)
    (84, Token (PIPE _)) -> Just (Reduce 3 179)
    (84, Token (COLON_COLON _)) -> Just (Reduce 3 179)
    (84, Token (MINUS _)) -> Just (Reduce 3 179)
    (84, Token (INFIXL _)) -> Just (Reduce 3 179)
    (84, Token (INFIXR _)) -> Just (Reduce 3 179)
    (84, Token (INFIX _)) -> Just (Reduce 3 179)
    (84, Token (RARROW _)) -> Just (Reduce 3 179)
    (84, Token (QCONID _)) -> Just (Reduce 3 179)
    (84, Token (EXPORT _)) -> Just (Reduce 3 179)
    (84, Token (AS _)) -> Just (Reduce 3 179)
    (84, Token (QVARID _)) -> Just (Reduce 3 179)
    (84, Token (STRING _)) -> Just (Reduce 3 179)
    (84, Token (LARROW _)) -> Just (Reduce 3 179)
    (84, Token (LET _)) -> Just (Reduce 3 179)
    (84, Token (LAMBDA _)) -> Just (Reduce 3 179)
    (84, Token (IF _)) -> Just (Reduce 3 179)
    (84, Token (THEN _)) -> Just (Reduce 3 179)
    (84, Token (ELSE _)) -> Just (Reduce 3 179)
    (84, Token (INTEGER _)) -> Just (Reduce 3 179)
    (84, Token (QVARSYM _)) -> Just (Reduce 3 179)
    (84, Token (QCONSYM _)) -> Just (Reduce 3 179)
    (84, Token (BACKQUOTE _)) -> Just (Reduce 3 179)
    (85, Token (WHERE _)) -> Just (Reduce 3 180)
    (85, Token (LBRACE _)) -> Just (Reduce 3 180)
    (85, Token (RBRACE _)) -> Just (Reduce 3 180)
    (85, Token (LPAREN _)) -> Just (Reduce 3 180)
    (85, Token (RPAREN _)) -> Just (Reduce 3 180)
    (85, Token (COMMA _)) -> Just (Reduce 3 180)
    (85, Token (SEMICOLON _)) -> Just (Reduce 3 180)
    (85, Token (EQUAL _)) -> Just (Reduce 3 180)
    (85, Token (PIPE _)) -> Just (Reduce 3 180)
    (85, Token (COLON_COLON _)) -> Just (Reduce 3 180)
    (85, Token (MINUS _)) -> Just (Reduce 3 180)
    (85, Token (INFIXL _)) -> Just (Reduce 3 180)
    (85, Token (INFIXR _)) -> Just (Reduce 3 180)
    (85, Token (INFIX _)) -> Just (Reduce 3 180)
    (85, Token (RARROW _)) -> Just (Reduce 3 180)
    (85, Token (QCONID _)) -> Just (Reduce 3 180)
    (85, Token (EXPORT _)) -> Just (Reduce 3 180)
    (85, Token (AS _)) -> Just (Reduce 3 180)
    (85, Token (QVARID _)) -> Just (Reduce 3 180)
    (85, Token (STRING _)) -> Just (Reduce 3 180)
    (85, Token (LARROW _)) -> Just (Reduce 3 180)
    (85, Token (LET _)) -> Just (Reduce 3 180)
    (85, Token (LAMBDA _)) -> Just (Reduce 3 180)
    (85, Token (IF _)) -> Just (Reduce 3 180)
    (85, Token (THEN _)) -> Just (Reduce 3 180)
    (85, Token (ELSE _)) -> Just (Reduce 3 180)
    (85, Token (INTEGER _)) -> Just (Reduce 3 180)
    (85, Token (QVARSYM _)) -> Just (Reduce 3 180)
    (85, Token (QCONSYM _)) -> Just (Reduce 3 180)
    (85, Token (BACKQUOTE _)) -> Just (Reduce 3 180)
    (86, Token (RPAREN _)) -> Just (Shift 84)
    (87, Token (WHERE _)) -> Just (Reduce 1 177)
    (87, Token (LBRACE _)) -> Just (Reduce 1 177)
    (87, Token (RBRACE _)) -> Just (Reduce 1 177)
    (87, Token (LPAREN _)) -> Just (Reduce 1 177)
    (87, Token (RPAREN _)) -> Just (Reduce 1 177)
    (87, Token (COMMA _)) -> Just (Reduce 1 177)
    (87, Token (SEMICOLON _)) -> Just (Reduce 1 177)
    (87, Token (EQUAL _)) -> Just (Reduce 1 177)
    (87, Token (PIPE _)) -> Just (Reduce 1 177)
    (87, Token (COLON_COLON _)) -> Just (Reduce 1 177)
    (87, Token (MINUS _)) -> Just (Reduce 1 177)
    (87, Token (INFIXL _)) -> Just (Reduce 1 177)
    (87, Token (INFIXR _)) -> Just (Reduce 1 177)
    (87, Token (INFIX _)) -> Just (Reduce 1 177)
    (87, Token (RARROW _)) -> Just (Reduce 1 177)
    (87, Token (QCONID _)) -> Just (Reduce 1 177)
    (87, Token (EXPORT _)) -> Just (Reduce 1 177)
    (87, Token (AS _)) -> Just (Reduce 1 177)
    (87, Token (QVARID _)) -> Just (Reduce 1 177)
    (87, Token (STRING _)) -> Just (Reduce 1 177)
    (87, Token (LARROW _)) -> Just (Reduce 1 177)
    (87, Token (LET _)) -> Just (Reduce 1 177)
    (87, Token (LAMBDA _)) -> Just (Reduce 1 177)
    (87, Token (IF _)) -> Just (Reduce 1 177)
    (87, Token (THEN _)) -> Just (Reduce 1 177)
    (87, Token (ELSE _)) -> Just (Reduce 1 177)
    (87, Token (INTEGER _)) -> Just (Reduce 1 177)
    (87, Token (QVARSYM _)) -> Just (Reduce 1 177)
    (87, Token (QCONSYM _)) -> Just (Reduce 1 177)
    (87, Token (BACKQUOTE _)) -> Just (Reduce 1 177)
    (88, Token (WHERE _)) -> Just (Reduce 1 176)
    (88, Token (LBRACE _)) -> Just (Reduce 1 176)
    (88, Token (RBRACE _)) -> Just (Reduce 1 176)
    (88, Token (LPAREN _)) -> Just (Reduce 1 176)
    (88, Token (RPAREN _)) -> Just (Reduce 1 176)
    (88, Token (COMMA _)) -> Just (Reduce 1 176)
    (88, Token (SEMICOLON _)) -> Just (Reduce 1 176)
    (88, Token (EQUAL _)) -> Just (Reduce 1 176)
    (88, Token (PIPE _)) -> Just (Reduce 1 176)
    (88, Token (COLON_COLON _)) -> Just (Reduce 1 176)
    (88, Token (MINUS _)) -> Just (Reduce 1 176)
    (88, Token (INFIXL _)) -> Just (Reduce 1 176)
    (88, Token (INFIXR _)) -> Just (Reduce 1 176)
    (88, Token (INFIX _)) -> Just (Reduce 1 176)
    (88, Token (RARROW _)) -> Just (Reduce 1 176)
    (88, Token (QCONID _)) -> Just (Reduce 1 176)
    (88, Token (EXPORT _)) -> Just (Reduce 1 176)
    (88, Token (AS _)) -> Just (Reduce 1 176)
    (88, Token (QVARID _)) -> Just (Reduce 1 176)
    (88, Token (STRING _)) -> Just (Reduce 1 176)
    (88, Token (LARROW _)) -> Just (Reduce 1 176)
    (88, Token (LET _)) -> Just (Reduce 1 176)
    (88, Token (LAMBDA _)) -> Just (Reduce 1 176)
    (88, Token (IF _)) -> Just (Reduce 1 176)
    (88, Token (THEN _)) -> Just (Reduce 1 176)
    (88, Token (ELSE _)) -> Just (Reduce 1 176)
    (88, Token (INTEGER _)) -> Just (Reduce 1 176)
    (88, Token (QVARSYM _)) -> Just (Reduce 1 176)
    (88, Token (QCONSYM _)) -> Just (Reduce 1 176)
    (88, Token (BACKQUOTE _)) -> Just (Reduce 1 176)
    (89, Token (WHERE _)) -> Just (Reduce 1 178)
    (89, Token (LBRACE _)) -> Just (Reduce 1 178)
    (89, Token (RBRACE _)) -> Just (Reduce 1 178)
    (89, Token (LPAREN _)) -> Just (Reduce 1 178)
    (89, Token (RPAREN _)) -> Just (Reduce 1 178)
    (89, Token (COMMA _)) -> Just (Reduce 1 178)
    (89, Token (SEMICOLON _)) -> Just (Reduce 1 178)
    (89, Token (EQUAL _)) -> Just (Reduce 1 178)
    (89, Token (PIPE _)) -> Just (Reduce 1 178)
    (89, Token (COLON_COLON _)) -> Just (Reduce 1 178)
    (89, Token (MINUS _)) -> Just (Reduce 1 178)
    (89, Token (INFIXL _)) -> Just (Reduce 1 178)
    (89, Token (INFIXR _)) -> Just (Reduce 1 178)
    (89, Token (INFIX _)) -> Just (Reduce 1 178)
    (89, Token (RARROW _)) -> Just (Reduce 1 178)
    (89, Token (QCONID _)) -> Just (Reduce 1 178)
    (89, Token (EXPORT _)) -> Just (Reduce 1 178)
    (89, Token (AS _)) -> Just (Reduce 1 178)
    (89, Token (QVARID _)) -> Just (Reduce 1 178)
    (89, Token (STRING _)) -> Just (Reduce 1 178)
    (89, Token (LARROW _)) -> Just (Reduce 1 178)
    (89, Token (LET _)) -> Just (Reduce 1 178)
    (89, Token (LAMBDA _)) -> Just (Reduce 1 178)
    (89, Token (IF _)) -> Just (Reduce 1 178)
    (89, Token (THEN _)) -> Just (Reduce 1 178)
    (89, Token (ELSE _)) -> Just (Reduce 1 178)
    (89, Token (INTEGER _)) -> Just (Reduce 1 178)
    (89, Token (QVARSYM _)) -> Just (Reduce 1 178)
    (89, Token (QCONSYM _)) -> Just (Reduce 1 178)
    (89, Token (BACKQUOTE _)) -> Just (Reduce 1 178)
    (90, Token (RPAREN _)) -> Just (Shift 85)
    (91, Token (LPAREN _)) -> Just (Shift 131)
    (91, Token (LBRACKET _)) -> Just (Shift 135)
    (91, Token (EXCL _)) -> Just (Shift 91)
    (91, Token (QCONID _)) -> Just (Shift 138)
    (91, Token (EXPORT _)) -> Just (Shift 322)
    (91, Token (AS _)) -> Just (Shift 323)
    (91, Token (QVARID _)) -> Just (Shift 324)
    (92, Token (LPAREN _)) -> Just (Shift 131)
    (92, Token (LBRACKET _)) -> Just (Shift 135)
    (92, Token (EXCL _)) -> Just (Shift 91)
    (92, Token (QCONID _)) -> Just (Shift 138)
    (92, Token (EXPORT _)) -> Just (Shift 322)
    (92, Token (AS _)) -> Just (Shift 323)
    (92, Token (QVARID _)) -> Just (Shift 324)
    (93, Token (WHERE _)) -> Just (Shift 215)
    (93, Token (RBRACE _)) -> Just (Reduce 0 65)
    (93, Token (LPAREN _)) -> Just (Shift 131)
    (93, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (93, Token (DARROW _)) -> Just (Shift 96)
    (93, Token (LBRACKET _)) -> Just (Shift 135)
    (93, Token (EXCL _)) -> Just (Shift 91)
    (93, Token (QCONID _)) -> Just (Shift 138)
    (93, Token (EXPORT _)) -> Just (Shift 322)
    (93, Token (AS _)) -> Just (Shift 323)
    (93, Token (QVARID _)) -> Just (Shift 324)
    (94, Token (LPAREN _)) -> Just (Shift 131)
    (94, Token (LBRACKET _)) -> Just (Shift 135)
    (94, Token (EXCL _)) -> Just (Shift 91)
    (94, Token (QCONID _)) -> Just (Shift 138)
    (94, Token (EXPORT _)) -> Just (Shift 322)
    (94, Token (AS _)) -> Just (Shift 323)
    (94, Token (QVARID _)) -> Just (Shift 324)
    (95, Token (WHERE _)) -> Just (Shift 217)
    (95, Token (RBRACE _)) -> Just (Reduce 0 75)
    (95, Token (LPAREN _)) -> Just (Shift 131)
    (95, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (95, Token (DARROW _)) -> Just (Shift 98)
    (95, Token (LBRACKET _)) -> Just (Shift 135)
    (95, Token (EXCL _)) -> Just (Shift 91)
    (95, Token (QCONID _)) -> Just (Shift 138)
    (95, Token (EXPORT _)) -> Just (Shift 322)
    (95, Token (AS _)) -> Just (Shift 323)
    (95, Token (QVARID _)) -> Just (Shift 324)
    (96, Token (LPAREN _)) -> Just (Shift 131)
    (96, Token (LBRACKET _)) -> Just (Shift 135)
    (96, Token (EXCL _)) -> Just (Shift 91)
    (96, Token (QCONID _)) -> Just (Shift 138)
    (96, Token (EXPORT _)) -> Just (Shift 322)
    (96, Token (AS _)) -> Just (Shift 323)
    (96, Token (QVARID _)) -> Just (Shift 324)
    (97, Token (WHERE _)) -> Just (Shift 215)
    (97, Token (RBRACE _)) -> Just (Reduce 0 65)
    (97, Token (LPAREN _)) -> Just (Shift 131)
    (97, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (97, Token (LBRACKET _)) -> Just (Shift 135)
    (97, Token (EXCL _)) -> Just (Shift 91)
    (97, Token (QCONID _)) -> Just (Shift 138)
    (97, Token (EXPORT _)) -> Just (Shift 322)
    (97, Token (AS _)) -> Just (Shift 323)
    (97, Token (QVARID _)) -> Just (Shift 324)
    (98, Token (LPAREN _)) -> Just (Shift 131)
    (98, Token (LBRACKET _)) -> Just (Shift 135)
    (98, Token (EXCL _)) -> Just (Shift 91)
    (98, Token (QCONID _)) -> Just (Shift 138)
    (98, Token (EXPORT _)) -> Just (Shift 322)
    (98, Token (AS _)) -> Just (Shift 323)
    (98, Token (QVARID _)) -> Just (Shift 324)
    (99, Token (WHERE _)) -> Just (Shift 217)
    (99, Token (RBRACE _)) -> Just (Reduce 0 75)
    (99, Token (LPAREN _)) -> Just (Shift 131)
    (99, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (99, Token (LBRACKET _)) -> Just (Shift 135)
    (99, Token (EXCL _)) -> Just (Shift 91)
    (99, Token (QCONID _)) -> Just (Shift 138)
    (99, Token (EXPORT _)) -> Just (Shift 322)
    (99, Token (AS _)) -> Just (Shift 323)
    (99, Token (QVARID _)) -> Just (Shift 324)
    (100, Token (LPAREN _)) -> Just (Shift 131)
    (100, Token (LBRACKET _)) -> Just (Shift 135)
    (100, Token (EXCL _)) -> Just (Shift 91)
    (100, Token (QCONID _)) -> Just (Shift 138)
    (100, Token (EXPORT _)) -> Just (Shift 322)
    (100, Token (AS _)) -> Just (Shift 323)
    (100, Token (QVARID _)) -> Just (Shift 324)
    (101, Token (WHERE _)) -> Just (Reduce 1 100)
    (101, Token (RBRACE _)) -> Just (Reduce 1 100)
    (101, Token (LPAREN _)) -> Just (Shift 131)
    (101, Token (RPAREN _)) -> Just (Reduce 1 100)
    (101, Token (COMMA _)) -> Just (Reduce 1 100)
    (101, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (101, Token (EQUAL _)) -> Just (Reduce 1 100)
    (101, Token (DARROW _)) -> Just (Shift 103)
    (101, Token (PIPE _)) -> Just (Reduce 1 100)
    (101, Token (RARROW _)) -> Just (Shift 102)
    (101, Token (LBRACKET _)) -> Just (Shift 135)
    (101, Token (EXCL _)) -> Just (Shift 91)
    (101, Token (QCONID _)) -> Just (Shift 138)
    (101, Token (EXPORT _)) -> Just (Shift 322)
    (101, Token (AS _)) -> Just (Shift 323)
    (101, Token (QVARID _)) -> Just (Shift 324)
    (101, Token (THEN _)) -> Just (Reduce 1 100)
    (101, Token (ELSE _)) -> Just (Reduce 1 100)
    (102, Token (LPAREN _)) -> Just (Shift 131)
    (102, Token (LBRACKET _)) -> Just (Shift 135)
    (102, Token (EXCL _)) -> Just (Shift 91)
    (102, Token (QCONID _)) -> Just (Shift 138)
    (102, Token (EXPORT _)) -> Just (Shift 322)
    (102, Token (AS _)) -> Just (Shift 323)
    (102, Token (QVARID _)) -> Just (Shift 324)
    (103, Token (LPAREN _)) -> Just (Shift 131)
    (103, Token (LBRACKET _)) -> Just (Shift 135)
    (103, Token (EXCL _)) -> Just (Shift 91)
    (103, Token (QCONID _)) -> Just (Shift 138)
    (103, Token (EXPORT _)) -> Just (Shift 322)
    (103, Token (AS _)) -> Just (Shift 323)
    (103, Token (QVARID _)) -> Just (Shift 324)
    (104, Token (WHERE _)) -> Just (Reduce 1 100)
    (104, Token (RBRACE _)) -> Just (Reduce 1 100)
    (104, Token (LPAREN _)) -> Just (Shift 131)
    (104, Token (RPAREN _)) -> Just (Reduce 1 100)
    (104, Token (COMMA _)) -> Just (Reduce 1 100)
    (104, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (104, Token (EQUAL _)) -> Just (Reduce 1 100)
    (104, Token (PIPE _)) -> Just (Reduce 1 100)
    (104, Token (RARROW _)) -> Just (Shift 102)
    (104, Token (LBRACKET _)) -> Just (Shift 135)
    (104, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (104, Token (EXCL _)) -> Just (Shift 91)
    (104, Token (QCONID _)) -> Just (Shift 138)
    (104, Token (EXPORT _)) -> Just (Shift 322)
    (104, Token (AS _)) -> Just (Shift 323)
    (104, Token (QVARID _)) -> Just (Shift 324)
    (104, Token (THEN _)) -> Just (Reduce 1 100)
    (104, Token (ELSE _)) -> Just (Reduce 1 100)
    (105, Token (LPAREN _)) -> Just (Shift 131)
    (105, Token (LBRACKET _)) -> Just (Shift 135)
    (105, Token (EXCL _)) -> Just (Shift 91)
    (105, Token (QCONID _)) -> Just (Shift 138)
    (105, Token (EXPORT _)) -> Just (Shift 322)
    (105, Token (AS _)) -> Just (Shift 323)
    (105, Token (QVARID _)) -> Just (Shift 324)
    (106, Token (RBRACE _)) -> Just (Reduce 0 119)
    (106, Token (LPAREN _)) -> Just (Shift 131)
    (106, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (106, Token (EQUAL _)) -> Just (Shift 109)
    (106, Token (DERIVING _)) -> Just (Reduce 0 119)
    (106, Token (DARROW _)) -> Just (Shift 107)
    (106, Token (LBRACKET _)) -> Just (Shift 135)
    (106, Token (EXCL _)) -> Just (Shift 91)
    (106, Token (QCONID _)) -> Just (Shift 138)
    (106, Token (EXPORT _)) -> Just (Shift 322)
    (106, Token (AS _)) -> Just (Shift 323)
    (106, Token (QVARID _)) -> Just (Shift 324)
    (107, Token (LPAREN _)) -> Just (Shift 131)
    (107, Token (LBRACKET _)) -> Just (Shift 135)
    (107, Token (EXCL _)) -> Just (Shift 91)
    (107, Token (QCONID _)) -> Just (Shift 138)
    (107, Token (EXPORT _)) -> Just (Shift 322)
    (107, Token (AS _)) -> Just (Shift 323)
    (107, Token (QVARID _)) -> Just (Shift 324)
    (108, Token (RBRACE _)) -> Just (Reduce 0 119)
    (108, Token (LPAREN _)) -> Just (Shift 131)
    (108, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (108, Token (EQUAL _)) -> Just (Shift 109)
    (108, Token (DERIVING _)) -> Just (Reduce 0 119)
    (108, Token (LBRACKET _)) -> Just (Shift 135)
    (108, Token (EXCL _)) -> Just (Shift 91)
    (108, Token (QCONID _)) -> Just (Shift 138)
    (108, Token (EXPORT _)) -> Just (Shift 322)
    (108, Token (AS _)) -> Just (Shift 323)
    (108, Token (QVARID _)) -> Just (Shift 324)
    (109, Token (LPAREN _)) -> Just (Shift 131)
    (109, Token (LBRACKET _)) -> Just (Shift 135)
    (109, Token (EXCL _)) -> Just (Shift 91)
    (109, Token (QCONID _)) -> Just (Shift 138)
    (109, Token (EXPORT _)) -> Just (Shift 322)
    (109, Token (AS _)) -> Just (Shift 323)
    (109, Token (QVARID _)) -> Just (Shift 324)
    (110, Token (LPAREN _)) -> Just (Shift 131)
    (110, Token (LBRACKET _)) -> Just (Shift 135)
    (110, Token (EXCL _)) -> Just (Shift 91)
    (110, Token (QCONID _)) -> Just (Shift 138)
    (110, Token (EXPORT _)) -> Just (Shift 322)
    (110, Token (AS _)) -> Just (Shift 323)
    (110, Token (QVARID _)) -> Just (Shift 324)
    (111, Token (LPAREN _)) -> Just (Shift 136)
    (111, Token (QCONID _)) -> Just (Shift 138)
    (112, Token (RBRACE _)) -> Just (Reduce 1 123)
    (112, Token (LPAREN _)) -> Just (Shift 131)
    (112, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (112, Token (DERIVING _)) -> Just (Reduce 1 123)
    (112, Token (PIPE _)) -> Just (Reduce 1 123)
    (112, Token (LBRACKET _)) -> Just (Shift 135)
    (112, Token (EXCL _)) -> Just (Shift 91)
    (112, Token (QCONID _)) -> Just (Shift 138)
    (112, Token (EXPORT _)) -> Just (Shift 322)
    (112, Token (AS _)) -> Just (Shift 323)
    (112, Token (QVARID _)) -> Just (Shift 324)
    (112, Token (QCONSYM _)) -> Just (Shift 335)
    (112, Token (BACKQUOTE _)) -> Just (Shift 337)
    (113, Token (LPAREN _)) -> Just (Shift 131)
    (113, Token (LBRACKET _)) -> Just (Shift 135)
    (113, Token (EXCL _)) -> Just (Shift 91)
    (113, Token (QCONID _)) -> Just (Shift 138)
    (113, Token (EXPORT _)) -> Just (Shift 322)
    (113, Token (AS _)) -> Just (Shift 323)
    (113, Token (QVARID _)) -> Just (Shift 324)
    (114, Token (RBRACE _)) -> Just (Reduce 3 124)
    (114, Token (LPAREN _)) -> Just (Shift 131)
    (114, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (114, Token (DERIVING _)) -> Just (Reduce 3 124)
    (114, Token (PIPE _)) -> Just (Reduce 3 124)
    (114, Token (LBRACKET _)) -> Just (Shift 135)
    (114, Token (EXCL _)) -> Just (Shift 91)
    (114, Token (QCONID _)) -> Just (Shift 138)
    (114, Token (EXPORT _)) -> Just (Shift 322)
    (114, Token (AS _)) -> Just (Shift 323)
    (114, Token (QVARID _)) -> Just (Shift 324)
    (115, Token (LPAREN _)) -> Just (Shift 131)
    (115, Token (LBRACKET _)) -> Just (Shift 135)
    (115, Token (EXCL _)) -> Just (Shift 91)
    (115, Token (QCONID _)) -> Just (Shift 138)
    (115, Token (EXPORT _)) -> Just (Shift 322)
    (115, Token (AS _)) -> Just (Shift 323)
    (115, Token (QVARID _)) -> Just (Shift 324)
    (116, Token (RBRACE _)) -> Just (Reduce 1 100)
    (116, Token (LPAREN _)) -> Just (Shift 131)
    (116, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (116, Token (DARROW _)) -> Just (Shift 121)
    (116, Token (RARROW _)) -> Just (Shift 102)
    (116, Token (LBRACKET _)) -> Just (Shift 135)
    (116, Token (EXCL _)) -> Just (Shift 91)
    (116, Token (QCONID _)) -> Just (Shift 138)
    (116, Token (EXPORT _)) -> Just (Shift 322)
    (116, Token (AS _)) -> Just (Shift 323)
    (116, Token (QVARID _)) -> Just (Shift 324)
    (117, Token (LPAREN _)) -> Just (Shift 131)
    (117, Token (LBRACKET _)) -> Just (Shift 135)
    (117, Token (EXCL _)) -> Just (Shift 91)
    (117, Token (QCONID _)) -> Just (Shift 138)
    (117, Token (EXPORT _)) -> Just (Shift 322)
    (117, Token (AS _)) -> Just (Shift 323)
    (117, Token (QVARID _)) -> Just (Shift 324)
    (118, Token (LPAREN _)) -> Just (Shift 131)
    (118, Token (LBRACKET _)) -> Just (Shift 135)
    (118, Token (EXCL _)) -> Just (Shift 91)
    (118, Token (QCONID _)) -> Just (Shift 138)
    (118, Token (EXPORT _)) -> Just (Shift 322)
    (118, Token (AS _)) -> Just (Shift 323)
    (118, Token (QVARID _)) -> Just (Shift 324)
    (119, Token (LPAREN _)) -> Just (Shift 131)
    (119, Token (LBRACKET _)) -> Just (Shift 135)
    (119, Token (EXCL _)) -> Just (Shift 91)
    (119, Token (QCONID _)) -> Just (Shift 138)
    (119, Token (EXPORT _)) -> Just (Shift 322)
    (119, Token (AS _)) -> Just (Shift 323)
    (119, Token (QVARID _)) -> Just (Shift 324)
    (120, Token (LPAREN _)) -> Just (Shift 131)
    (120, Token (LBRACKET _)) -> Just (Shift 135)
    (120, Token (EXCL _)) -> Just (Shift 91)
    (120, Token (QCONID _)) -> Just (Shift 138)
    (120, Token (EXPORT _)) -> Just (Shift 322)
    (120, Token (AS _)) -> Just (Shift 323)
    (120, Token (QVARID _)) -> Just (Shift 324)
    (121, Token (LPAREN _)) -> Just (Shift 131)
    (121, Token (LBRACKET _)) -> Just (Shift 135)
    (121, Token (EXCL _)) -> Just (Shift 91)
    (121, Token (QCONID _)) -> Just (Shift 138)
    (121, Token (EXPORT _)) -> Just (Shift 322)
    (121, Token (AS _)) -> Just (Shift 323)
    (121, Token (QVARID _)) -> Just (Shift 324)
    (122, Token (LPAREN _)) -> Just (Shift 131)
    (122, Token (LBRACKET _)) -> Just (Shift 135)
    (122, Token (EXCL _)) -> Just (Shift 91)
    (122, Token (QCONID _)) -> Just (Shift 138)
    (122, Token (EXPORT _)) -> Just (Shift 322)
    (122, Token (AS _)) -> Just (Shift 323)
    (122, Token (QVARID _)) -> Just (Shift 324)
    (123, Token (LPAREN _)) -> Just (Shift 131)
    (123, Token (LBRACKET _)) -> Just (Shift 135)
    (123, Token (EXCL _)) -> Just (Shift 91)
    (123, Token (QCONID _)) -> Just (Shift 138)
    (123, Token (EXPORT _)) -> Just (Shift 322)
    (123, Token (AS _)) -> Just (Shift 323)
    (123, Token (QVARID _)) -> Just (Shift 324)
    (124, Token (LBRACE _)) -> Just (Shift 78)
    (124, Token (LPAREN _)) -> Just (Shift 131)
    (124, Token (LBRACKET _)) -> Just (Shift 135)
    (124, Token (EXCL _)) -> Just (Shift 91)
    (124, Token (QCONID _)) -> Just (Shift 138)
    (124, Token (EXPORT _)) -> Just (Shift 322)
    (124, Token (AS _)) -> Just (Shift 323)
    (124, Token (QVARID _)) -> Just (Shift 324)
    (125, Token (LPAREN _)) -> Just (Shift 131)
    (125, Token (LBRACKET _)) -> Just (Shift 135)
    (125, Token (EXCL _)) -> Just (Shift 91)
    (125, Token (QCONID _)) -> Just (Shift 138)
    (125, Token (EXPORT _)) -> Just (Shift 322)
    (125, Token (AS _)) -> Just (Shift 323)
    (125, Token (QVARID _)) -> Just (Shift 324)
    (126, Token (LPAREN _)) -> Just (Shift 131)
    (126, Token (EQUAL _)) -> Just (Shift 111)
    (126, Token (DARROW _)) -> Just (Shift 128)
    (126, Token (LBRACKET _)) -> Just (Shift 135)
    (126, Token (EXCL _)) -> Just (Shift 91)
    (126, Token (QCONID _)) -> Just (Shift 138)
    (126, Token (EXPORT _)) -> Just (Shift 322)
    (126, Token (AS _)) -> Just (Shift 323)
    (126, Token (QVARID _)) -> Just (Shift 324)
    (127, Token (LPAREN _)) -> Just (Shift 131)
    (127, Token (LBRACKET _)) -> Just (Shift 135)
    (127, Token (EXCL _)) -> Just (Shift 91)
    (127, Token (QCONID _)) -> Just (Shift 138)
    (127, Token (EXPORT _)) -> Just (Shift 322)
    (127, Token (AS _)) -> Just (Shift 323)
    (127, Token (QVARID _)) -> Just (Shift 324)
    (128, Token (LPAREN _)) -> Just (Shift 131)
    (128, Token (LBRACKET _)) -> Just (Shift 135)
    (128, Token (EXCL _)) -> Just (Shift 91)
    (128, Token (QCONID _)) -> Just (Shift 138)
    (128, Token (EXPORT _)) -> Just (Shift 322)
    (128, Token (AS _)) -> Just (Shift 323)
    (128, Token (QVARID _)) -> Just (Shift 324)
    (129, Token (LPAREN _)) -> Just (Shift 131)
    (129, Token (EQUAL _)) -> Just (Shift 117)
    (129, Token (LBRACKET _)) -> Just (Shift 135)
    (129, Token (EXCL _)) -> Just (Shift 91)
    (129, Token (QCONID _)) -> Just (Shift 138)
    (129, Token (EXPORT _)) -> Just (Shift 322)
    (129, Token (AS _)) -> Just (Shift 323)
    (129, Token (QVARID _)) -> Just (Shift 324)
    (130, Token (LPAREN _)) -> Just (Shift 131)
    (130, Token (EQUAL _)) -> Just (Shift 111)
    (130, Token (LBRACKET _)) -> Just (Shift 135)
    (130, Token (EXCL _)) -> Just (Shift 91)
    (130, Token (QCONID _)) -> Just (Shift 138)
    (130, Token (EXPORT _)) -> Just (Shift 322)
    (130, Token (AS _)) -> Just (Shift 323)
    (130, Token (QVARID _)) -> Just (Shift 324)
    (131, Token (LPAREN _)) -> Just (Shift 131)
    (131, Token (RPAREN _)) -> Just (Shift 314)
    (131, Token (COMMA _)) -> Just (Shift 327)
    (131, Token (RARROW _)) -> Just (Shift 317)
    (131, Token (LBRACKET _)) -> Just (Shift 135)
    (131, Token (EXCL _)) -> Just (Shift 91)
    (131, Token (QCONID _)) -> Just (Shift 138)
    (131, Token (EXPORT _)) -> Just (Shift 322)
    (131, Token (AS _)) -> Just (Shift 323)
    (131, Token (QVARID _)) -> Just (Shift 324)
    (131, Token (QCONSYM _)) -> Just (Shift 139)
    (132, Token (LPAREN _)) -> Just (Shift 131)
    (132, Token (RPAREN _)) -> Just (Shift 161)
    (132, Token (LBRACKET _)) -> Just (Shift 135)
    (132, Token (EXCL _)) -> Just (Shift 91)
    (132, Token (QCONID _)) -> Just (Shift 138)
    (132, Token (EXPORT _)) -> Just (Shift 322)
    (132, Token (AS _)) -> Just (Shift 323)
    (132, Token (QVARID _)) -> Just (Shift 324)
    (133, Token (LPAREN _)) -> Just (Shift 131)
    (133, Token (LBRACKET _)) -> Just (Shift 135)
    (133, Token (EXCL _)) -> Just (Shift 91)
    (133, Token (QCONID _)) -> Just (Shift 138)
    (133, Token (EXPORT _)) -> Just (Shift 322)
    (133, Token (AS _)) -> Just (Shift 323)
    (133, Token (QVARID _)) -> Just (Shift 324)
    (134, Token (LPAREN _)) -> Just (Shift 131)
    (134, Token (LBRACKET _)) -> Just (Shift 135)
    (134, Token (EXCL _)) -> Just (Shift 91)
    (134, Token (QCONID _)) -> Just (Shift 138)
    (134, Token (EXPORT _)) -> Just (Shift 322)
    (134, Token (AS _)) -> Just (Shift 323)
    (134, Token (QVARID _)) -> Just (Shift 324)
    (135, Token (LPAREN _)) -> Just (Shift 131)
    (135, Token (LBRACKET _)) -> Just (Shift 135)
    (135, Token (RBRACKET _)) -> Just (Shift 318)
    (135, Token (EXCL _)) -> Just (Shift 91)
    (135, Token (QCONID _)) -> Just (Shift 138)
    (135, Token (EXPORT _)) -> Just (Shift 322)
    (135, Token (AS _)) -> Just (Shift 323)
    (135, Token (QVARID _)) -> Just (Shift 324)
    (136, Token (QCONSYM _)) -> Just (Shift 139)
    (137, Token (WHERE _)) -> Just (Reduce 3 182)
    (137, Token (LBRACE _)) -> Just (Reduce 3 182)
    (137, Token (RBRACE _)) -> Just (Reduce 3 182)
    (137, Token (LPAREN _)) -> Just (Reduce 3 182)
    (137, Token (RPAREN _)) -> Just (Reduce 3 182)
    (137, Token (COMMA _)) -> Just (Reduce 3 182)
    (137, Token (SEMICOLON _)) -> Just (Reduce 3 182)
    (137, Token (EQUAL _)) -> Just (Reduce 3 182)
    (137, Token (DERIVING _)) -> Just (Reduce 3 182)
    (137, Token (DARROW _)) -> Just (Reduce 3 182)
    (137, Token (PIPE _)) -> Just (Reduce 3 182)
    (137, Token (COLON_COLON _)) -> Just (Reduce 3 182)
    (137, Token (MINUS _)) -> Just (Reduce 3 182)
    (137, Token (INFIXL _)) -> Just (Reduce 3 182)
    (137, Token (INFIXR _)) -> Just (Reduce 3 182)
    (137, Token (INFIX _)) -> Just (Reduce 3 182)
    (137, Token (RARROW _)) -> Just (Reduce 3 182)
    (137, Token (LBRACKET _)) -> Just (Reduce 3 182)
    (137, Token (RBRACKET _)) -> Just (Reduce 3 182)
    (137, Token (EXCL _)) -> Just (Reduce 3 182)
    (137, Token (QCONID _)) -> Just (Reduce 3 182)
    (137, Token (EXPORT _)) -> Just (Reduce 3 182)
    (137, Token (AS _)) -> Just (Reduce 3 182)
    (137, Token (QVARID _)) -> Just (Reduce 3 182)
    (137, Token (THEN _)) -> Just (Reduce 3 182)
    (137, Token (ELSE _)) -> Just (Reduce 3 182)
    (137, Token (INTEGER _)) -> Just (Reduce 3 182)
    (137, Token (QVARSYM _)) -> Just (Reduce 3 182)
    (137, Token (QCONSYM _)) -> Just (Reduce 3 182)
    (137, Token (BACKQUOTE _)) -> Just (Reduce 3 182)
    (138, Token (WHERE _)) -> Just (Reduce 1 181)
    (138, Token (LBRACE _)) -> Just (Reduce 1 181)
    (138, Token (RBRACE _)) -> Just (Reduce 1 181)
    (138, Token (LPAREN _)) -> Just (Reduce 1 181)
    (138, Token (RPAREN _)) -> Just (Reduce 1 181)
    (138, Token (COMMA _)) -> Just (Reduce 1 181)
    (138, Token (SEMICOLON _)) -> Just (Reduce 1 181)
    (138, Token (EQUAL _)) -> Just (Reduce 1 181)
    (138, Token (DERIVING _)) -> Just (Reduce 1 181)
    (138, Token (DARROW _)) -> Just (Reduce 1 181)
    (138, Token (PIPE _)) -> Just (Reduce 1 181)
    (138, Token (COLON_COLON _)) -> Just (Reduce 1 181)
    (138, Token (MINUS _)) -> Just (Reduce 1 181)
    (138, Token (INFIXL _)) -> Just (Reduce 1 181)
    (138, Token (INFIXR _)) -> Just (Reduce 1 181)
    (138, Token (INFIX _)) -> Just (Reduce 1 181)
    (138, Token (RARROW _)) -> Just (Reduce 1 181)
    (138, Token (LBRACKET _)) -> Just (Reduce 1 181)
    (138, Token (RBRACKET _)) -> Just (Reduce 1 181)
    (138, Token (EXCL _)) -> Just (Reduce 1 181)
    (138, Token (QCONID _)) -> Just (Reduce 1 181)
    (138, Token (EXPORT _)) -> Just (Reduce 1 181)
    (138, Token (AS _)) -> Just (Reduce 1 181)
    (138, Token (QVARID _)) -> Just (Reduce 1 181)
    (138, Token (THEN _)) -> Just (Reduce 1 181)
    (138, Token (ELSE _)) -> Just (Reduce 1 181)
    (138, Token (INTEGER _)) -> Just (Reduce 1 181)
    (138, Token (QVARSYM _)) -> Just (Reduce 1 181)
    (138, Token (QCONSYM _)) -> Just (Reduce 1 181)
    (138, Token (BACKQUOTE _)) -> Just (Reduce 1 181)
    (139, Token (RPAREN _)) -> Just (Shift 137)
    (140, Token (RPAREN _)) -> Just (Reduce 3 24)
    (141, Token (RPAREN _)) -> Just (Reduce 1 23)
    (141, Token (COMMA _)) -> Just (Shift 73)
    (142, Token (RPAREN _)) -> Just (Reduce 3 17)
    (143, Token (RPAREN _)) -> Just (Reduce 1 16)
    (143, Token (COMMA _)) -> Just (Shift 70)
    (144, Token (RPAREN _)) -> Just (Reduce 3 20)
    (144, Token (COMMA _)) -> Just (Reduce 3 20)
    (145, Token (RPAREN _)) -> Just (Reduce 4 21)
    (145, Token (COMMA _)) -> Just (Reduce 4 21)
    (146, Token (RPAREN _)) -> Just (Reduce 4 22)
    (146, Token (COMMA _)) -> Just (Reduce 4 22)
    (147, Token (RPAREN _)) -> Just (Shift 145)
    (148, Token (RPAREN _)) -> Just (Reduce 1 18)
    (148, Token (COMMA _)) -> Just (Reduce 1 18)
    (149, Token (LPAREN _)) -> Just (Shift 74)
    (149, Token (RPAREN _)) -> Just (Reduce 1 19)
    (149, Token (COMMA _)) -> Just (Reduce 1 19)
    (150, Token (RPAREN _)) -> Just (Shift 146)
    (151, Token (RPAREN _)) -> Just (Reduce 1 25)
    (151, Token (COMMA _)) -> Just (Reduce 1 25)
    (152, Token (RPAREN _)) -> Just (Reduce 1 26)
    (152, Token (COMMA _)) -> Just (Reduce 1 26)
    (153, Token (RPAREN _)) -> Just (Shift 157)
    (153, Token (QCONID _)) -> Just (Shift 208)
    (154, Token (RPAREN _)) -> Just (Shift 158)
    (154, Token (QCONID _)) -> Just (Shift 208)
    (155, Token (RPAREN _)) -> Just (Shift 159)
    (155, Token (QCONID _)) -> Just (Shift 208)
    (156, Token (RPAREN _)) -> Just (Shift 160)
    (156, Token (QCONID _)) -> Just (Shift 208)
    (157, Token (RBRACE _)) -> Just (Reduce 6 35)
    (157, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (158, Token (RBRACE _)) -> Just (Reduce 8 39)
    (158, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (159, Token (RBRACE _)) -> Just (Reduce 8 47)
    (159, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (160, Token (RBRACE _)) -> Just (Reduce 6 43)
    (160, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (161, Token (RBRACE _)) -> Just (Reduce 3 53)
    (161, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (162, Token (RBRACE _)) -> Just (Reduce 8 31)
    (162, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (163, Token (RBRACE _)) -> Just (Reduce 7 30)
    (163, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (164, Token (RBRACE _)) -> Just (Reduce 7 36)
    (164, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (165, Token (RBRACE _)) -> Just (Reduce 9 40)
    (165, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (166, Token (RBRACE _)) -> Just (Reduce 9 48)
    (166, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (167, Token (RBRACE _)) -> Just (Reduce 7 44)
    (167, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (168, Token (RBRACE _)) -> Just (Reduce 4 54)
    (168, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (169, Token (QCONID _)) -> Just (Reduce 0 193)
    (169, Token (QUALIFIED _)) -> Just (Shift 201)
    (170, Token (LPAREN _)) -> Just (Shift 71)
    (171, Token (LPAREN _)) -> Just (Shift 153)
    (171, Token (QCONID _)) -> Just (Shift 208)
    (172, Token (LPAREN _)) -> Just (Shift 154)
    (172, Token (QCONID _)) -> Just (Shift 208)
    (173, Token (LPAREN _)) -> Just (Shift 155)
    (173, Token (QCONID _)) -> Just (Shift 208)
    (174, Token (LPAREN _)) -> Just (Shift 156)
    (174, Token (QCONID _)) -> Just (Shift 208)
    (175, Token (LPAREN _)) -> Just (Shift 132)
    (176, Token (IMPORT _)) -> Just (Shift 221)
    (176, Token (EXPORT _)) -> Just (Shift 222)
    (177, Token (RBRACE _)) -> Just (Reduce 0 191)
    (177, Token (LPAREN _)) -> Just (Reduce 0 191)
    (177, Token (SEMICOLON _)) -> Just (Reduce 0 191)
    (177, Token (HIDING _)) -> Just (Reduce 0 191)
    (177, Token (AS _)) -> Just (Shift 9)
    (178, Token (RPAREN _)) -> Just (Shift 162)
    (179, Token (RPAREN _)) -> Just (Shift 163)
    (180, Token (RBRACE _)) -> Just (Reduce 4 29)
    (180, Token (LPAREN _)) -> Just (Shift 72)
    (180, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (180, Token (HIDING _)) -> Just (Shift 170)
    (181, Token (RBRACE _)) -> Just (Reduce 4 32)
    (181, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (182, Token (RBRACE _)) -> Just (Reduce 3 33)
    (182, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (182, Token (DERIVING _)) -> Just (Shift 171)
    (183, Token (RBRACE _)) -> Just (Reduce 5 37)
    (183, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (183, Token (DERIVING _)) -> Just (Shift 172)
    (184, Token (RBRACE _)) -> Just (Reduce 5 34)
    (184, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (185, Token (RBRACE _)) -> Just (Reduce 7 38)
    (185, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (186, Token (RBRACE _)) -> Just (Reduce 7 46)
    (186, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (187, Token (RBRACE _)) -> Just (Reduce 5 42)
    (187, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (188, Token (RPAREN _)) -> Just (Shift 164)
    (189, Token (RPAREN _)) -> Just (Shift 165)
    (190, Token (RPAREN _)) -> Just (Shift 166)
    (191, Token (RPAREN _)) -> Just (Shift 167)
    (192, Token (RBRACE _)) -> Just (Reduce 5 45)
    (192, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (192, Token (DERIVING _)) -> Just (Shift 173)
    (193, Token (RBRACE _)) -> Just (Reduce 3 41)
    (193, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (193, Token (DERIVING _)) -> Just (Shift 174)
    (194, Token (RBRACE _)) -> Just (Reduce 5 50)
    (194, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (195, Token (RBRACE _)) -> Just (Reduce 3 49)
    (195, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (196, Token (RBRACE _)) -> Just (Reduce 5 52)
    (196, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (197, Token (RBRACE _)) -> Just (Reduce 3 51)
    (197, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (198, Token (RPAREN _)) -> Just (Shift 168)
    (199, Token (RBRACE _)) -> Just (Reduce 2 55)
    (199, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (200, Token (RBRACE _)) -> Just (Reduce 1 56)
    (200, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (201, Token (QCONID _)) -> Just (Reduce 1 194)
    (202, Token (RBRACE _)) -> Just (Reduce 2 192)
    (202, Token (LPAREN _)) -> Just (Reduce 2 192)
    (202, Token (SEMICOLON _)) -> Just (Reduce 2 192)
    (202, Token (HIDING _)) -> Just (Reduce 2 192)
    (203, Token (WHERE _)) -> Just (Reduce 1 102)
    (203, Token (LBRACE _)) -> Just (Reduce 1 102)
    (203, Token (RBRACE _)) -> Just (Reduce 1 102)
    (203, Token (LPAREN _)) -> Just (Reduce 1 102)
    (203, Token (RPAREN _)) -> Just (Reduce 1 102)
    (203, Token (COMMA _)) -> Just (Reduce 1 102)
    (203, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (203, Token (EQUAL _)) -> Just (Reduce 1 102)
    (203, Token (DERIVING _)) -> Just (Reduce 1 102)
    (203, Token (DARROW _)) -> Just (Reduce 1 102)
    (203, Token (PIPE _)) -> Just (Reduce 1 102)
    (203, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (203, Token (MINUS _)) -> Just (Reduce 1 102)
    (203, Token (INFIXL _)) -> Just (Reduce 1 102)
    (203, Token (INFIXR _)) -> Just (Reduce 1 102)
    (203, Token (INFIX _)) -> Just (Reduce 1 102)
    (203, Token (RARROW _)) -> Just (Reduce 1 102)
    (203, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (203, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (203, Token (EXCL _)) -> Just (Reduce 1 102)
    (203, Token (QCONID _)) -> Just (Reduce 1 102)
    (203, Token (EXPORT _)) -> Just (Reduce 1 102)
    (203, Token (AS _)) -> Just (Reduce 1 102)
    (203, Token (QVARID _)) -> Just (Reduce 1 102)
    (203, Token (THEN _)) -> Just (Reduce 1 102)
    (203, Token (ELSE _)) -> Just (Reduce 1 102)
    (203, Token (INTEGER _)) -> Just (Reduce 1 102)
    (203, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (203, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (203, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (204, Token (WHERE _)) -> Just (Reduce 2 103)
    (204, Token (LBRACE _)) -> Just (Reduce 2 103)
    (204, Token (RBRACE _)) -> Just (Reduce 2 103)
    (204, Token (LPAREN _)) -> Just (Reduce 2 103)
    (204, Token (RPAREN _)) -> Just (Reduce 2 103)
    (204, Token (COMMA _)) -> Just (Reduce 2 103)
    (204, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (204, Token (EQUAL _)) -> Just (Reduce 2 103)
    (204, Token (DERIVING _)) -> Just (Reduce 2 103)
    (204, Token (DARROW _)) -> Just (Reduce 2 103)
    (204, Token (PIPE _)) -> Just (Reduce 2 103)
    (204, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (204, Token (MINUS _)) -> Just (Reduce 2 103)
    (204, Token (INFIXL _)) -> Just (Reduce 2 103)
    (204, Token (INFIXR _)) -> Just (Reduce 2 103)
    (204, Token (INFIX _)) -> Just (Reduce 2 103)
    (204, Token (RARROW _)) -> Just (Reduce 2 103)
    (204, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (204, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (204, Token (EXCL _)) -> Just (Reduce 2 103)
    (204, Token (QCONID _)) -> Just (Reduce 2 103)
    (204, Token (EXPORT _)) -> Just (Reduce 2 103)
    (204, Token (AS _)) -> Just (Reduce 2 103)
    (204, Token (QVARID _)) -> Just (Reduce 2 103)
    (204, Token (THEN _)) -> Just (Reduce 2 103)
    (204, Token (ELSE _)) -> Just (Reduce 2 103)
    (204, Token (INTEGER _)) -> Just (Reduce 2 103)
    (204, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (204, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (204, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (205, Token (WHERE _)) -> Just (Reduce 3 101)
    (205, Token (RBRACE _)) -> Just (Reduce 3 101)
    (205, Token (RPAREN _)) -> Just (Reduce 3 101)
    (205, Token (COMMA _)) -> Just (Reduce 3 101)
    (205, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (205, Token (EQUAL _)) -> Just (Reduce 3 101)
    (205, Token (PIPE _)) -> Just (Reduce 3 101)
    (205, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (205, Token (THEN _)) -> Just (Reduce 3 101)
    (205, Token (ELSE _)) -> Just (Reduce 3 101)
    (206, Token (RBRACE _)) -> Just (Reduce 2 120)
    (206, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (206, Token (DERIVING _)) -> Just (Reduce 2 120)
    (207, Token (QCONID _)) -> Just (Shift 208)
    (208, Token (RBRACE _)) -> Just (Reduce 1 134)
    (208, Token (RPAREN _)) -> Just (Reduce 1 134)
    (208, Token (COMMA _)) -> Just (Reduce 1 134)
    (208, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (209, Token (RPAREN _)) -> Just (Reduce 1 132)
    (209, Token (COMMA _)) -> Just (Shift 207)
    (210, Token (RPAREN _)) -> Just (Reduce 3 133)
    (211, Token (RBRACE _)) -> Just (Reduce 7 128)
    (211, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (211, Token (DERIVING _)) -> Just (Reduce 7 128)
    (212, Token (COLON_COLON _)) -> Just (Shift 123)
    (213, Token (RBRACE _)) -> Just (Shift 211)
    (214, Token (RBRACE _)) -> Just (Reduce 3 127)
    (214, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (214, Token (DERIVING _)) -> Just (Reduce 3 127)
    (215, Token (LBRACE _)) -> Just (Shift 61)
    (216, Token (RBRACE _)) -> Just (Reduce 2 66)
    (216, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (217, Token (LBRACE _)) -> Just (Shift 64)
    (218, Token (RBRACE _)) -> Just (Reduce 2 76)
    (218, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (219, Token (RPAREN _)) -> Just (Reduce 1 98)
    (219, Token (COMMA _)) -> Just (Shift 133)
    (220, Token (RPAREN _)) -> Just (Reduce 3 99)
    (221, Token (EXPORT _)) -> Just (Shift 343)
    (221, Token (AS _)) -> Just (Shift 344)
    (221, Token (QVARID _)) -> Just (Shift 345)
    (222, Token (EXPORT _)) -> Just (Shift 343)
    (222, Token (AS _)) -> Just (Shift 344)
    (222, Token (QVARID _)) -> Just (Shift 345)
    (223, Token (COLON_COLON _)) -> Just (Shift 118)
    (224, Token (COLON_COLON _)) -> Just (Shift 119)
    (225, Token (COLON_COLON _)) -> Just (Shift 120)
    (226, Token (RBRACE _)) -> Just (Reduce 6 135)
    (226, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (227, Token (RBRACE _)) -> Just (Reduce 7 136)
    (227, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (228, Token (RBRACE _)) -> Just (Reduce 6 137)
    (228, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (229, Token (EXPORT _)) -> Just (Shift 347)
    (229, Token (AS _)) -> Just (Shift 348)
    (229, Token (QVARID _)) -> Just (Shift 349)
    (229, Token (STRING _)) -> Just (Shift 346)
    (230, Token (STRING _)) -> Just (Shift 350)
    (231, Token (STRING _)) -> Just (Shift 346)
    (232, Token (LBRACE _)) -> Just (Shift 59)
    (233, Token (LBRACE _)) -> Just (Shift 59)
    (234, Token (RBRACE _)) -> Just (Reduce 5 62)
    (234, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (235, Token (RBRACE _)) -> Just (Reduce 5 64)
    (235, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (236, Token (RBRACE _)) -> Just (Reduce 1 60)
    (236, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (237, Token (WHERE _)) -> Just (Shift 232)
    (237, Token (RBRACE _)) -> Just (Reduce 3 61)
    (237, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (238, Token (WHERE _)) -> Just (Shift 233)
    (238, Token (RBRACE _)) -> Just (Reduce 3 63)
    (238, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (239, Token (LBRACE _)) -> Just (Shift 59)
    (240, Token (LBRACE _)) -> Just (Shift 59)
    (241, Token (LBRACE _)) -> Just (Shift 59)
    (242, Token (LBRACE _)) -> Just (Shift 59)
    (243, Token (LBRACE _)) -> Just (Shift 59)
    (244, Token (LBRACE _)) -> Just (Shift 59)
    (245, Token (RBRACE _)) -> Just (Reduce 3 57)
    (245, Token (COMMA _)) -> Just (Reduce 3 57)
    (245, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (245, Token (EQUAL _)) -> Just (Reduce 3 57)
    (245, Token (IN _)) -> Just (Reduce 3 57)
    (246, Token (RBRACE _)) -> Just (Shift 245)
    (247, Token (RBRACE _)) -> Just (Reduce 1 58)
    (247, Token (SEMICOLON _)) -> Just (Shift 60)
    (248, Token (RBRACE _)) -> Just (Reduce 3 59)
    (249, Token (RBRACE _)) -> Just (Reduce 5 87)
    (249, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (250, Token (RBRACE _)) -> Just (Reduce 3 86)
    (250, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (251, Token (COLON_COLON _)) -> Just (Shift 115)
    (252, Token (COMMA _)) -> Just (Reduce 0 200)
    (252, Token (MINUS _)) -> Just (Reduce 0 200)
    (252, Token (QCONID _)) -> Just (Reduce 0 200)
    (252, Token (EXPORT _)) -> Just (Reduce 0 200)
    (252, Token (AS _)) -> Just (Reduce 0 200)
    (252, Token (QVARID _)) -> Just (Reduce 0 200)
    (252, Token (INTEGER _)) -> Just (Shift 296)
    (252, Token (QVARSYM _)) -> Just (Reduce 0 200)
    (252, Token (QCONSYM _)) -> Just (Reduce 0 200)
    (252, Token (BACKQUOTE _)) -> Just (Reduce 0 200)
    (253, Token (MINUS _)) -> Just (Shift 299)
    (253, Token (QVARSYM _)) -> Just (Shift 373)
    (253, Token (QCONSYM _)) -> Just (Shift 335)
    (253, Token (BACKQUOTE _)) -> Just (Shift 336)
    (254, Token (RBRACE _)) -> Just (Reduce 3 88)
    (254, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (255, Token (LPAREN _)) -> Just (Reduce 1 170)
    (255, Token (RPAREN _)) -> Just (Reduce 1 170)
    (255, Token (EQUAL _)) -> Just (Reduce 1 170)
    (255, Token (PIPE _)) -> Just (Reduce 1 170)
    (255, Token (MINUS _)) -> Just (Reduce 1 170)
    (255, Token (RARROW _)) -> Just (Reduce 1 170)
    (255, Token (QCONID _)) -> Just (Reduce 1 170)
    (255, Token (EXPORT _)) -> Just (Reduce 1 170)
    (255, Token (AS _)) -> Just (Reduce 1 170)
    (255, Token (QVARID _)) -> Just (Reduce 1 170)
    (255, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (255, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (255, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (256, Token (LPAREN _)) -> Just (Reduce 3 172)
    (256, Token (RPAREN _)) -> Just (Reduce 3 172)
    (256, Token (EQUAL _)) -> Just (Reduce 3 172)
    (256, Token (PIPE _)) -> Just (Reduce 3 172)
    (256, Token (MINUS _)) -> Just (Reduce 3 172)
    (256, Token (RARROW _)) -> Just (Reduce 3 172)
    (256, Token (QCONID _)) -> Just (Reduce 3 172)
    (256, Token (EXPORT _)) -> Just (Reduce 3 172)
    (256, Token (AS _)) -> Just (Reduce 3 172)
    (256, Token (QVARID _)) -> Just (Reduce 3 172)
    (256, Token (QVARSYM _)) -> Just (Reduce 3 172)
    (256, Token (QCONSYM _)) -> Just (Reduce 3 172)
    (256, Token (BACKQUOTE _)) -> Just (Reduce 3 172)
    (257, Token (LPAREN _)) -> Just (Reduce 2 171)
    (257, Token (RPAREN _)) -> Just (Reduce 2 171)
    (257, Token (EQUAL _)) -> Just (Reduce 2 171)
    (257, Token (PIPE _)) -> Just (Reduce 2 171)
    (257, Token (MINUS _)) -> Just (Reduce 2 171)
    (257, Token (RARROW _)) -> Just (Reduce 2 171)
    (257, Token (QCONID _)) -> Just (Reduce 2 171)
    (257, Token (EXPORT _)) -> Just (Reduce 2 171)
    (257, Token (AS _)) -> Just (Reduce 2 171)
    (257, Token (QVARID _)) -> Just (Reduce 2 171)
    (257, Token (QVARSYM _)) -> Just (Reduce 2 171)
    (257, Token (QCONSYM _)) -> Just (Reduce 2 171)
    (257, Token (BACKQUOTE _)) -> Just (Reduce 2 171)
    (258, Token (LPAREN _)) -> Just (Reduce 3 173)
    (258, Token (RPAREN _)) -> Just (Reduce 3 173)
    (258, Token (EQUAL _)) -> Just (Reduce 3 173)
    (258, Token (PIPE _)) -> Just (Reduce 3 173)
    (258, Token (MINUS _)) -> Just (Reduce 3 173)
    (258, Token (RARROW _)) -> Just (Reduce 3 173)
    (258, Token (QCONID _)) -> Just (Reduce 3 173)
    (258, Token (EXPORT _)) -> Just (Reduce 3 173)
    (258, Token (AS _)) -> Just (Reduce 3 173)
    (258, Token (QVARID _)) -> Just (Reduce 3 173)
    (258, Token (QVARSYM _)) -> Just (Reduce 3 173)
    (258, Token (QCONSYM _)) -> Just (Reduce 3 173)
    (258, Token (BACKQUOTE _)) -> Just (Reduce 3 173)
    (259, Token (WHERE _)) -> Just (Reduce 5 157)
    (259, Token (RBRACE _)) -> Just (Reduce 5 157)
    (259, Token (RPAREN _)) -> Just (Reduce 5 157)
    (259, Token (COMMA _)) -> Just (Reduce 5 157)
    (259, Token (SEMICOLON _)) -> Just (Reduce 5 157)
    (259, Token (EQUAL _)) -> Just (Reduce 5 157)
    (259, Token (PIPE _)) -> Just (Reduce 5 157)
    (259, Token (THEN _)) -> Just (Reduce 5 157)
    (259, Token (ELSE _)) -> Just (Reduce 5 157)
    (260, Token (WHERE _)) -> Just (Reduce 3 156)
    (260, Token (RBRACE _)) -> Just (Reduce 3 156)
    (260, Token (RPAREN _)) -> Just (Reduce 3 156)
    (260, Token (COMMA _)) -> Just (Reduce 3 156)
    (260, Token (SEMICOLON _)) -> Just (Reduce 3 156)
    (260, Token (EQUAL _)) -> Just (Reduce 3 156)
    (260, Token (PIPE _)) -> Just (Reduce 3 156)
    (260, Token (THEN _)) -> Just (Reduce 3 156)
    (260, Token (ELSE _)) -> Just (Reduce 3 156)
    (261, Token (IN _)) -> Just (Shift 39)
    (262, Token (WHERE _)) -> Just (Reduce 4 153)
    (262, Token (RBRACE _)) -> Just (Reduce 4 153)
    (262, Token (RPAREN _)) -> Just (Reduce 4 153)
    (262, Token (COMMA _)) -> Just (Reduce 4 153)
    (262, Token (SEMICOLON _)) -> Just (Reduce 4 153)
    (262, Token (EQUAL _)) -> Just (Reduce 4 153)
    (262, Token (PIPE _)) -> Just (Reduce 4 153)
    (262, Token (THEN _)) -> Just (Reduce 4 153)
    (262, Token (ELSE _)) -> Just (Reduce 4 153)
    (263, Token (WHERE _)) -> Just (Reduce 4 154)
    (263, Token (RBRACE _)) -> Just (Reduce 4 154)
    (263, Token (RPAREN _)) -> Just (Reduce 4 154)
    (263, Token (COMMA _)) -> Just (Reduce 4 154)
    (263, Token (SEMICOLON _)) -> Just (Reduce 4 154)
    (263, Token (EQUAL _)) -> Just (Reduce 4 154)
    (263, Token (PIPE _)) -> Just (Reduce 4 154)
    (263, Token (THEN _)) -> Just (Reduce 4 154)
    (263, Token (ELSE _)) -> Just (Reduce 4 154)
    (264, Token (SEMICOLON _)) -> Just (Shift 357)
    (264, Token (THEN _)) -> Just (Reduce 0 202)
    (265, Token (SEMICOLON _)) -> Just (Shift 357)
    (265, Token (ELSE _)) -> Just (Reduce 0 202)
    (266, Token (WHERE _)) -> Just (Reduce 8 155)
    (266, Token (RBRACE _)) -> Just (Reduce 8 155)
    (266, Token (RPAREN _)) -> Just (Reduce 8 155)
    (266, Token (COMMA _)) -> Just (Reduce 8 155)
    (266, Token (SEMICOLON _)) -> Just (Reduce 8 155)
    (266, Token (EQUAL _)) -> Just (Reduce 8 155)
    (266, Token (PIPE _)) -> Just (Reduce 8 155)
    (266, Token (THEN _)) -> Just (Reduce 8 155)
    (266, Token (ELSE _)) -> Just (Reduce 8 155)
    (267, Token (WHERE _)) -> Just (Reduce 1 158)
    (267, Token (RBRACE _)) -> Just (Reduce 1 158)
    (267, Token (RPAREN _)) -> Just (Reduce 1 158)
    (267, Token (COMMA _)) -> Just (Reduce 1 158)
    (267, Token (SEMICOLON _)) -> Just (Reduce 1 158)
    (267, Token (EQUAL _)) -> Just (Reduce 1 158)
    (267, Token (PIPE _)) -> Just (Reduce 1 158)
    (267, Token (COLON_COLON _)) -> Just (Shift 100)
    (267, Token (THEN _)) -> Just (Reduce 1 158)
    (267, Token (ELSE _)) -> Just (Reduce 1 158)
    (268, Token (THEN _)) -> Just (Shift 53)
    (269, Token (ELSE _)) -> Just (Shift 40)
    (270, Token (WHERE _)) -> Just (Reduce 3 146)
    (270, Token (RBRACE _)) -> Just (Reduce 3 146)
    (270, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (270, Token (PIPE _)) -> Just (Shift 48)
    (271, Token (WHERE _)) -> Just (Reduce 5 147)
    (271, Token (RBRACE _)) -> Just (Reduce 5 147)
    (271, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (272, Token (EQUAL _)) -> Just (Shift 42)
    (273, Token (RBRACE _)) -> Just (Reduce 3 67)
    (273, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (274, Token (RBRACE _)) -> Just (Shift 273)
    (275, Token (RBRACE _)) -> Just (Reduce 3 69)
    (276, Token (RBRACE _)) -> Just (Reduce 1 68)
    (276, Token (SEMICOLON _)) -> Just (Shift 62)
    (277, Token (RBRACE _)) -> Just (Reduce 5 72)
    (277, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (278, Token (RBRACE _)) -> Just (Reduce 5 74)
    (278, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (279, Token (RBRACE _)) -> Just (Reduce 1 70)
    (279, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (280, Token (WHERE _)) -> Just (Shift 239)
    (280, Token (RBRACE _)) -> Just (Reduce 3 71)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (281, Token (WHERE _)) -> Just (Shift 240)
    (281, Token (RBRACE _)) -> Just (Reduce 3 73)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (282, Token (RBRACE _)) -> Just (Reduce 3 77)
    (282, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (283, Token (RBRACE _)) -> Just (Shift 282)
    (284, Token (RBRACE _)) -> Just (Reduce 3 79)
    (285, Token (RBRACE _)) -> Just (Reduce 1 78)
    (285, Token (SEMICOLON _)) -> Just (Shift 65)
    (286, Token (RBRACE _)) -> Just (Reduce 5 82)
    (286, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (287, Token (RBRACE _)) -> Just (Reduce 5 84)
    (287, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (288, Token (WHERE _)) -> Just (Shift 241)
    (288, Token (RBRACE _)) -> Just (Reduce 3 81)
    (288, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (289, Token (WHERE _)) -> Just (Shift 242)
    (289, Token (RBRACE _)) -> Just (Reduce 3 83)
    (289, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (290, Token (COMMA _)) -> Just (Shift 75)
    (290, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (291, Token (LPAREN _)) -> Just (Reduce 1 174)
    (291, Token (COMMA _)) -> Just (Shift 75)
    (291, Token (EQUAL _)) -> Just (Reduce 1 174)
    (291, Token (PIPE _)) -> Just (Reduce 1 174)
    (291, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (291, Token (MINUS _)) -> Just (Reduce 1 174)
    (291, Token (QCONID _)) -> Just (Reduce 1 174)
    (291, Token (EXPORT _)) -> Just (Reduce 1 174)
    (291, Token (AS _)) -> Just (Reduce 1 174)
    (291, Token (QVARID _)) -> Just (Reduce 1 174)
    (291, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (291, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (291, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (292, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (293, Token (COMMA _)) -> Just (Reduce 1 95)
    (293, Token (MINUS _)) -> Just (Reduce 1 95)
    (293, Token (QCONID _)) -> Just (Reduce 1 95)
    (293, Token (EXPORT _)) -> Just (Reduce 1 95)
    (293, Token (AS _)) -> Just (Reduce 1 95)
    (293, Token (QVARID _)) -> Just (Reduce 1 95)
    (293, Token (INTEGER _)) -> Just (Reduce 1 95)
    (293, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (293, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (293, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (294, Token (COMMA _)) -> Just (Reduce 1 96)
    (294, Token (MINUS _)) -> Just (Reduce 1 96)
    (294, Token (QCONID _)) -> Just (Reduce 1 96)
    (294, Token (EXPORT _)) -> Just (Reduce 1 96)
    (294, Token (AS _)) -> Just (Reduce 1 96)
    (294, Token (QVARID _)) -> Just (Reduce 1 96)
    (294, Token (INTEGER _)) -> Just (Reduce 1 96)
    (294, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (294, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (294, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (295, Token (COMMA _)) -> Just (Reduce 1 97)
    (295, Token (MINUS _)) -> Just (Reduce 1 97)
    (295, Token (QCONID _)) -> Just (Reduce 1 97)
    (295, Token (EXPORT _)) -> Just (Reduce 1 97)
    (295, Token (AS _)) -> Just (Reduce 1 97)
    (295, Token (QVARID _)) -> Just (Reduce 1 97)
    (295, Token (INTEGER _)) -> Just (Reduce 1 97)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (295, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (295, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (296, Token (COMMA _)) -> Just (Reduce 1 201)
    (296, Token (MINUS _)) -> Just (Reduce 1 201)
    (296, Token (QCONID _)) -> Just (Reduce 1 201)
    (296, Token (EXPORT _)) -> Just (Reduce 1 201)
    (296, Token (AS _)) -> Just (Reduce 1 201)
    (296, Token (QVARID _)) -> Just (Reduce 1 201)
    (296, Token (QVARSYM _)) -> Just (Reduce 1 201)
    (296, Token (QCONSYM _)) -> Just (Reduce 1 201)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 1 201)
    (297, Token (MINUS _)) -> Just (Shift 299)
    (297, Token (QVARSYM _)) -> Just (Shift 373)
    (297, Token (QCONSYM _)) -> Just (Shift 335)
    (297, Token (BACKQUOTE _)) -> Just (Shift 336)
    (298, Token (MINUS _)) -> Just (Shift 299)
    (298, Token (QVARSYM _)) -> Just (Shift 373)
    (298, Token (QCONSYM _)) -> Just (Shift 335)
    (298, Token (BACKQUOTE _)) -> Just (Shift 336)
    (299, Token (RBRACE _)) -> Just (Reduce 1 89)
    (299, Token (COMMA _)) -> Just (Shift 297)
    (299, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (300, Token (RBRACE _)) -> Just (Reduce 3 91)
    (300, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (301, Token (RBRACE _)) -> Just (Reduce 3 92)
    (301, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (302, Token (RBRACE _)) -> Just (Reduce 1 90)
    (302, Token (COMMA _)) -> Just (Shift 298)
    (302, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (303, Token (LBRACE _)) -> Just (Reduce 1 190)
    (303, Token (RBRACE _)) -> Just (Reduce 1 190)
    (303, Token (LPAREN _)) -> Just (Reduce 1 190)
    (303, Token (COMMA _)) -> Just (Reduce 1 190)
    (303, Token (SEMICOLON _)) -> Just (Reduce 1 190)
    (303, Token (EQUAL _)) -> Just (Reduce 1 190)
    (303, Token (PIPE _)) -> Just (Reduce 1 190)
    (303, Token (COLON_COLON _)) -> Just (Reduce 1 190)
    (303, Token (MINUS _)) -> Just (Reduce 1 190)
    (303, Token (INFIXL _)) -> Just (Reduce 1 190)
    (303, Token (INFIXR _)) -> Just (Reduce 1 190)
    (303, Token (INFIX _)) -> Just (Reduce 1 190)
    (303, Token (QCONID _)) -> Just (Reduce 1 190)
    (303, Token (EXPORT _)) -> Just (Reduce 1 190)
    (303, Token (AS _)) -> Just (Reduce 1 190)
    (303, Token (QVARID _)) -> Just (Reduce 1 190)
    (303, Token (STRING _)) -> Just (Reduce 1 190)
    (303, Token (LET _)) -> Just (Reduce 1 190)
    (303, Token (LAMBDA _)) -> Just (Reduce 1 190)
    (303, Token (IF _)) -> Just (Reduce 1 190)
    (303, Token (INTEGER _)) -> Just (Reduce 1 190)
    (303, Token (QVARSYM _)) -> Just (Reduce 1 190)
    (303, Token (QCONSYM _)) -> Just (Reduce 1 190)
    (303, Token (BACKQUOTE _)) -> Just (Reduce 1 190)
    (304, Token (LBRACE _)) -> Just (Reduce 1 189)
    (304, Token (RBRACE _)) -> Just (Reduce 1 189)
    (304, Token (LPAREN _)) -> Just (Reduce 1 189)
    (304, Token (COMMA _)) -> Just (Reduce 1 189)
    (304, Token (SEMICOLON _)) -> Just (Reduce 1 189)
    (304, Token (EQUAL _)) -> Just (Reduce 1 189)
    (304, Token (PIPE _)) -> Just (Reduce 1 189)
    (304, Token (COLON_COLON _)) -> Just (Reduce 1 189)
    (304, Token (MINUS _)) -> Just (Reduce 1 189)
    (304, Token (INFIXL _)) -> Just (Reduce 1 189)
    (304, Token (INFIXR _)) -> Just (Reduce 1 189)
    (304, Token (INFIX _)) -> Just (Reduce 1 189)
    (304, Token (QCONID _)) -> Just (Reduce 1 189)
    (304, Token (EXPORT _)) -> Just (Reduce 1 189)
    (304, Token (AS _)) -> Just (Reduce 1 189)
    (304, Token (QVARID _)) -> Just (Reduce 1 189)
    (304, Token (STRING _)) -> Just (Reduce 1 189)
    (304, Token (LET _)) -> Just (Reduce 1 189)
    (304, Token (LAMBDA _)) -> Just (Reduce 1 189)
    (304, Token (IF _)) -> Just (Reduce 1 189)
    (304, Token (INTEGER _)) -> Just (Reduce 1 189)
    (304, Token (QVARSYM _)) -> Just (Reduce 1 189)
    (304, Token (QCONSYM _)) -> Just (Reduce 1 189)
    (304, Token (BACKQUOTE _)) -> Just (Reduce 1 189)
    (305, Token (WHERE _)) -> Just (Reduce 3 108)
    (305, Token (LBRACE _)) -> Just (Reduce 3 108)
    (305, Token (RBRACE _)) -> Just (Reduce 3 108)
    (305, Token (LPAREN _)) -> Just (Reduce 3 108)
    (305, Token (RPAREN _)) -> Just (Reduce 3 108)
    (305, Token (COMMA _)) -> Just (Reduce 3 108)
    (305, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (305, Token (EQUAL _)) -> Just (Reduce 3 108)
    (305, Token (DERIVING _)) -> Just (Reduce 3 108)
    (305, Token (DARROW _)) -> Just (Reduce 3 108)
    (305, Token (PIPE _)) -> Just (Reduce 3 108)
    (305, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (305, Token (MINUS _)) -> Just (Reduce 3 108)
    (305, Token (INFIXL _)) -> Just (Reduce 3 108)
    (305, Token (INFIXR _)) -> Just (Reduce 3 108)
    (305, Token (INFIX _)) -> Just (Reduce 3 108)
    (305, Token (RARROW _)) -> Just (Reduce 3 108)
    (305, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (305, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (305, Token (EXCL _)) -> Just (Reduce 3 108)
    (305, Token (QCONID _)) -> Just (Reduce 3 108)
    (305, Token (EXPORT _)) -> Just (Reduce 3 108)
    (305, Token (AS _)) -> Just (Reduce 3 108)
    (305, Token (QVARID _)) -> Just (Reduce 3 108)
    (305, Token (THEN _)) -> Just (Reduce 3 108)
    (305, Token (ELSE _)) -> Just (Reduce 3 108)
    (305, Token (INTEGER _)) -> Just (Reduce 3 108)
    (305, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (305, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (305, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (306, Token (WHERE _)) -> Just (Reduce 3 106)
    (306, Token (LBRACE _)) -> Just (Reduce 3 106)
    (306, Token (RBRACE _)) -> Just (Reduce 3 106)
    (306, Token (LPAREN _)) -> Just (Reduce 3 106)
    (306, Token (RPAREN _)) -> Just (Reduce 3 106)
    (306, Token (COMMA _)) -> Just (Reduce 3 106)
    (306, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (306, Token (EQUAL _)) -> Just (Reduce 3 106)
    (306, Token (DERIVING _)) -> Just (Reduce 3 106)
    (306, Token (DARROW _)) -> Just (Reduce 3 106)
    (306, Token (PIPE _)) -> Just (Reduce 3 106)
    (306, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (306, Token (MINUS _)) -> Just (Reduce 3 106)
    (306, Token (INFIXL _)) -> Just (Reduce 3 106)
    (306, Token (INFIXR _)) -> Just (Reduce 3 106)
    (306, Token (INFIX _)) -> Just (Reduce 3 106)
    (306, Token (RARROW _)) -> Just (Reduce 3 106)
    (306, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (306, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (306, Token (EXCL _)) -> Just (Reduce 3 106)
    (306, Token (QCONID _)) -> Just (Reduce 3 106)
    (306, Token (EXPORT _)) -> Just (Reduce 3 106)
    (306, Token (AS _)) -> Just (Reduce 3 106)
    (306, Token (QVARID _)) -> Just (Reduce 3 106)
    (306, Token (THEN _)) -> Just (Reduce 3 106)
    (306, Token (ELSE _)) -> Just (Reduce 3 106)
    (306, Token (INTEGER _)) -> Just (Reduce 3 106)
    (306, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (306, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (306, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (307, Token (WHERE _)) -> Just (Reduce 3 107)
    (307, Token (LBRACE _)) -> Just (Reduce 3 107)
    (307, Token (RBRACE _)) -> Just (Reduce 3 107)
    (307, Token (LPAREN _)) -> Just (Reduce 3 107)
    (307, Token (RPAREN _)) -> Just (Reduce 3 107)
    (307, Token (COMMA _)) -> Just (Reduce 3 107)
    (307, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (307, Token (EQUAL _)) -> Just (Reduce 3 107)
    (307, Token (DERIVING _)) -> Just (Reduce 3 107)
    (307, Token (DARROW _)) -> Just (Reduce 3 107)
    (307, Token (PIPE _)) -> Just (Reduce 3 107)
    (307, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (307, Token (MINUS _)) -> Just (Reduce 3 107)
    (307, Token (INFIXL _)) -> Just (Reduce 3 107)
    (307, Token (INFIXR _)) -> Just (Reduce 3 107)
    (307, Token (INFIX _)) -> Just (Reduce 3 107)
    (307, Token (RARROW _)) -> Just (Reduce 3 107)
    (307, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (307, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (307, Token (EXCL _)) -> Just (Reduce 3 107)
    (307, Token (QCONID _)) -> Just (Reduce 3 107)
    (307, Token (EXPORT _)) -> Just (Reduce 3 107)
    (307, Token (AS _)) -> Just (Reduce 3 107)
    (307, Token (QVARID _)) -> Just (Reduce 3 107)
    (307, Token (THEN _)) -> Just (Reduce 3 107)
    (307, Token (ELSE _)) -> Just (Reduce 3 107)
    (307, Token (INTEGER _)) -> Just (Reduce 3 107)
    (307, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (307, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (307, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (308, Token (RPAREN _)) -> Just (Shift 305)
    (308, Token (COMMA _)) -> Just (Shift 134)
    (309, Token (RBRACKET _)) -> Just (Shift 307)
    (310, Token (WHERE _)) -> Just (Reduce 2 109)
    (310, Token (LBRACE _)) -> Just (Reduce 2 109)
    (310, Token (RBRACE _)) -> Just (Reduce 2 109)
    (310, Token (LPAREN _)) -> Just (Reduce 2 109)
    (310, Token (RPAREN _)) -> Just (Reduce 2 109)
    (310, Token (COMMA _)) -> Just (Reduce 2 109)
    (310, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (310, Token (EQUAL _)) -> Just (Reduce 2 109)
    (310, Token (DERIVING _)) -> Just (Reduce 2 109)
    (310, Token (DARROW _)) -> Just (Reduce 2 109)
    (310, Token (PIPE _)) -> Just (Reduce 2 109)
    (310, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (310, Token (MINUS _)) -> Just (Reduce 2 109)
    (310, Token (INFIXL _)) -> Just (Reduce 2 109)
    (310, Token (INFIXR _)) -> Just (Reduce 2 109)
    (310, Token (INFIX _)) -> Just (Reduce 2 109)
    (310, Token (RARROW _)) -> Just (Reduce 2 109)
    (310, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (310, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (310, Token (EXCL _)) -> Just (Reduce 2 109)
    (310, Token (QCONID _)) -> Just (Reduce 2 109)
    (310, Token (EXPORT _)) -> Just (Reduce 2 109)
    (310, Token (AS _)) -> Just (Reduce 2 109)
    (310, Token (QVARID _)) -> Just (Reduce 2 109)
    (310, Token (THEN _)) -> Just (Reduce 2 109)
    (310, Token (ELSE _)) -> Just (Reduce 2 109)
    (310, Token (INTEGER _)) -> Just (Reduce 2 109)
    (310, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (310, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (310, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (311, Token (WHERE _)) -> Just (Reduce 1 104)
    (311, Token (LBRACE _)) -> Just (Reduce 1 104)
    (311, Token (RBRACE _)) -> Just (Reduce 1 104)
    (311, Token (LPAREN _)) -> Just (Reduce 1 104)
    (311, Token (RPAREN _)) -> Just (Reduce 1 104)
    (311, Token (COMMA _)) -> Just (Reduce 1 104)
    (311, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (311, Token (EQUAL _)) -> Just (Reduce 1 104)
    (311, Token (DERIVING _)) -> Just (Reduce 1 104)
    (311, Token (DARROW _)) -> Just (Reduce 1 104)
    (311, Token (PIPE _)) -> Just (Reduce 1 104)
    (311, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (311, Token (MINUS _)) -> Just (Reduce 1 104)
    (311, Token (INFIXL _)) -> Just (Reduce 1 104)
    (311, Token (INFIXR _)) -> Just (Reduce 1 104)
    (311, Token (INFIX _)) -> Just (Reduce 1 104)
    (311, Token (RARROW _)) -> Just (Reduce 1 104)
    (311, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (311, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (311, Token (EXCL _)) -> Just (Reduce 1 104)
    (311, Token (QCONID _)) -> Just (Reduce 1 104)
    (311, Token (EXPORT _)) -> Just (Reduce 1 104)
    (311, Token (AS _)) -> Just (Reduce 1 104)
    (311, Token (QVARID _)) -> Just (Reduce 1 104)
    (311, Token (THEN _)) -> Just (Reduce 1 104)
    (311, Token (ELSE _)) -> Just (Reduce 1 104)
    (311, Token (INTEGER _)) -> Just (Reduce 1 104)
    (311, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (311, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (311, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (312, Token (WHERE _)) -> Just (Reduce 1 105)
    (312, Token (LBRACE _)) -> Just (Reduce 1 105)
    (312, Token (RBRACE _)) -> Just (Reduce 1 105)
    (312, Token (LPAREN _)) -> Just (Reduce 1 105)
    (312, Token (RPAREN _)) -> Just (Reduce 1 105)
    (312, Token (COMMA _)) -> Just (Reduce 1 105)
    (312, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (312, Token (EQUAL _)) -> Just (Reduce 1 105)
    (312, Token (DERIVING _)) -> Just (Reduce 1 105)
    (312, Token (DARROW _)) -> Just (Reduce 1 105)
    (312, Token (PIPE _)) -> Just (Reduce 1 105)
    (312, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (312, Token (MINUS _)) -> Just (Reduce 1 105)
    (312, Token (INFIXL _)) -> Just (Reduce 1 105)
    (312, Token (INFIXR _)) -> Just (Reduce 1 105)
    (312, Token (INFIX _)) -> Just (Reduce 1 105)
    (312, Token (RARROW _)) -> Just (Reduce 1 105)
    (312, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (312, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (312, Token (EXCL _)) -> Just (Reduce 1 105)
    (312, Token (QCONID _)) -> Just (Reduce 1 105)
    (312, Token (EXPORT _)) -> Just (Reduce 1 105)
    (312, Token (AS _)) -> Just (Reduce 1 105)
    (312, Token (QVARID _)) -> Just (Reduce 1 105)
    (312, Token (THEN _)) -> Just (Reduce 1 105)
    (312, Token (ELSE _)) -> Just (Reduce 1 105)
    (312, Token (INTEGER _)) -> Just (Reduce 1 105)
    (312, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (312, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (312, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (313, Token (RPAREN _)) -> Just (Shift 306)
    (314, Token (WHERE _)) -> Just (Reduce 2 113)
    (314, Token (LBRACE _)) -> Just (Reduce 2 113)
    (314, Token (RBRACE _)) -> Just (Reduce 2 113)
    (314, Token (LPAREN _)) -> Just (Reduce 2 113)
    (314, Token (RPAREN _)) -> Just (Reduce 2 113)
    (314, Token (COMMA _)) -> Just (Reduce 2 113)
    (314, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (314, Token (EQUAL _)) -> Just (Reduce 2 113)
    (314, Token (DERIVING _)) -> Just (Reduce 2 113)
    (314, Token (DARROW _)) -> Just (Reduce 2 113)
    (314, Token (PIPE _)) -> Just (Reduce 2 113)
    (314, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (314, Token (MINUS _)) -> Just (Reduce 2 113)
    (314, Token (INFIXL _)) -> Just (Reduce 2 113)
    (314, Token (INFIXR _)) -> Just (Reduce 2 113)
    (314, Token (INFIX _)) -> Just (Reduce 2 113)
    (314, Token (RARROW _)) -> Just (Reduce 2 113)
    (314, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (314, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (314, Token (EXCL _)) -> Just (Reduce 2 113)
    (314, Token (QCONID _)) -> Just (Reduce 2 113)
    (314, Token (EXPORT _)) -> Just (Reduce 2 113)
    (314, Token (AS _)) -> Just (Reduce 2 113)
    (314, Token (QVARID _)) -> Just (Reduce 2 113)
    (314, Token (THEN _)) -> Just (Reduce 2 113)
    (314, Token (ELSE _)) -> Just (Reduce 2 113)
    (314, Token (INTEGER _)) -> Just (Reduce 2 113)
    (314, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (314, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (314, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (315, Token (WHERE _)) -> Just (Reduce 3 115)
    (315, Token (LBRACE _)) -> Just (Reduce 3 115)
    (315, Token (RBRACE _)) -> Just (Reduce 3 115)
    (315, Token (LPAREN _)) -> Just (Reduce 3 115)
    (315, Token (RPAREN _)) -> Just (Reduce 3 115)
    (315, Token (COMMA _)) -> Just (Reduce 3 115)
    (315, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (315, Token (EQUAL _)) -> Just (Reduce 3 115)
    (315, Token (DERIVING _)) -> Just (Reduce 3 115)
    (315, Token (DARROW _)) -> Just (Reduce 3 115)
    (315, Token (PIPE _)) -> Just (Reduce 3 115)
    (315, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (315, Token (MINUS _)) -> Just (Reduce 3 115)
    (315, Token (INFIXL _)) -> Just (Reduce 3 115)
    (315, Token (INFIXR _)) -> Just (Reduce 3 115)
    (315, Token (INFIX _)) -> Just (Reduce 3 115)
    (315, Token (RARROW _)) -> Just (Reduce 3 115)
    (315, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (315, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (315, Token (EXCL _)) -> Just (Reduce 3 115)
    (315, Token (QCONID _)) -> Just (Reduce 3 115)
    (315, Token (EXPORT _)) -> Just (Reduce 3 115)
    (315, Token (AS _)) -> Just (Reduce 3 115)
    (315, Token (QVARID _)) -> Just (Reduce 3 115)
    (315, Token (THEN _)) -> Just (Reduce 3 115)
    (315, Token (ELSE _)) -> Just (Reduce 3 115)
    (315, Token (INTEGER _)) -> Just (Reduce 3 115)
    (315, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (315, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (315, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (316, Token (WHERE _)) -> Just (Reduce 3 116)
    (316, Token (LBRACE _)) -> Just (Reduce 3 116)
    (316, Token (RBRACE _)) -> Just (Reduce 3 116)
    (316, Token (LPAREN _)) -> Just (Reduce 3 116)
    (316, Token (RPAREN _)) -> Just (Reduce 3 116)
    (316, Token (COMMA _)) -> Just (Reduce 3 116)
    (316, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (316, Token (EQUAL _)) -> Just (Reduce 3 116)
    (316, Token (DERIVING _)) -> Just (Reduce 3 116)
    (316, Token (DARROW _)) -> Just (Reduce 3 116)
    (316, Token (PIPE _)) -> Just (Reduce 3 116)
    (316, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (316, Token (MINUS _)) -> Just (Reduce 3 116)
    (316, Token (INFIXL _)) -> Just (Reduce 3 116)
    (316, Token (INFIXR _)) -> Just (Reduce 3 116)
    (316, Token (INFIX _)) -> Just (Reduce 3 116)
    (316, Token (RARROW _)) -> Just (Reduce 3 116)
    (316, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (316, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (316, Token (EXCL _)) -> Just (Reduce 3 116)
    (316, Token (QCONID _)) -> Just (Reduce 3 116)
    (316, Token (EXPORT _)) -> Just (Reduce 3 116)
    (316, Token (AS _)) -> Just (Reduce 3 116)
    (316, Token (QVARID _)) -> Just (Reduce 3 116)
    (316, Token (THEN _)) -> Just (Reduce 3 116)
    (316, Token (ELSE _)) -> Just (Reduce 3 116)
    (316, Token (INTEGER _)) -> Just (Reduce 3 116)
    (316, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (316, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (316, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (317, Token (RPAREN _)) -> Just (Shift 315)
    (318, Token (WHERE _)) -> Just (Reduce 2 114)
    (318, Token (LBRACE _)) -> Just (Reduce 2 114)
    (318, Token (RBRACE _)) -> Just (Reduce 2 114)
    (318, Token (LPAREN _)) -> Just (Reduce 2 114)
    (318, Token (RPAREN _)) -> Just (Reduce 2 114)
    (318, Token (COMMA _)) -> Just (Reduce 2 114)
    (318, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (318, Token (EQUAL _)) -> Just (Reduce 2 114)
    (318, Token (DERIVING _)) -> Just (Reduce 2 114)
    (318, Token (DARROW _)) -> Just (Reduce 2 114)
    (318, Token (PIPE _)) -> Just (Reduce 2 114)
    (318, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (318, Token (MINUS _)) -> Just (Reduce 2 114)
    (318, Token (INFIXL _)) -> Just (Reduce 2 114)
    (318, Token (INFIXR _)) -> Just (Reduce 2 114)
    (318, Token (INFIX _)) -> Just (Reduce 2 114)
    (318, Token (RARROW _)) -> Just (Reduce 2 114)
    (318, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (318, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (318, Token (EXCL _)) -> Just (Reduce 2 114)
    (318, Token (QCONID _)) -> Just (Reduce 2 114)
    (318, Token (EXPORT _)) -> Just (Reduce 2 114)
    (318, Token (AS _)) -> Just (Reduce 2 114)
    (318, Token (QVARID _)) -> Just (Reduce 2 114)
    (318, Token (THEN _)) -> Just (Reduce 2 114)
    (318, Token (ELSE _)) -> Just (Reduce 2 114)
    (318, Token (INTEGER _)) -> Just (Reduce 2 114)
    (318, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (318, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (318, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (319, Token (WHERE _)) -> Just (Reduce 1 112)
    (319, Token (LBRACE _)) -> Just (Reduce 1 112)
    (319, Token (RBRACE _)) -> Just (Reduce 1 112)
    (319, Token (LPAREN _)) -> Just (Reduce 1 112)
    (319, Token (RPAREN _)) -> Just (Reduce 1 112)
    (319, Token (COMMA _)) -> Just (Reduce 1 112)
    (319, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (319, Token (EQUAL _)) -> Just (Reduce 1 112)
    (319, Token (DERIVING _)) -> Just (Reduce 1 112)
    (319, Token (DARROW _)) -> Just (Reduce 1 112)
    (319, Token (PIPE _)) -> Just (Reduce 1 112)
    (319, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (319, Token (MINUS _)) -> Just (Reduce 1 112)
    (319, Token (INFIXL _)) -> Just (Reduce 1 112)
    (319, Token (INFIXR _)) -> Just (Reduce 1 112)
    (319, Token (INFIX _)) -> Just (Reduce 1 112)
    (319, Token (RARROW _)) -> Just (Reduce 1 112)
    (319, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (319, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (319, Token (EXCL _)) -> Just (Reduce 1 112)
    (319, Token (QCONID _)) -> Just (Reduce 1 112)
    (319, Token (EXPORT _)) -> Just (Reduce 1 112)
    (319, Token (AS _)) -> Just (Reduce 1 112)
    (319, Token (QVARID _)) -> Just (Reduce 1 112)
    (319, Token (THEN _)) -> Just (Reduce 1 112)
    (319, Token (ELSE _)) -> Just (Reduce 1 112)
    (319, Token (INTEGER _)) -> Just (Reduce 1 112)
    (319, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (319, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (319, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (320, Token (LBRACE _)) -> Just (Shift 76)
    (320, Token (RBRACE _)) -> Just (Reduce 1 112)
    (320, Token (LPAREN _)) -> Just (Reduce 1 112)
    (320, Token (RPAREN _)) -> Just (Reduce 1 112)
    (320, Token (COMMA _)) -> Just (Reduce 1 112)
    (320, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (320, Token (DERIVING _)) -> Just (Reduce 1 112)
    (320, Token (PIPE _)) -> Just (Reduce 1 112)
    (320, Token (RARROW _)) -> Just (Reduce 1 112)
    (320, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (320, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (320, Token (EXCL _)) -> Just (Reduce 1 112)
    (320, Token (QCONID _)) -> Just (Reduce 1 112)
    (320, Token (EXPORT _)) -> Just (Reduce 1 112)
    (320, Token (AS _)) -> Just (Reduce 1 112)
    (320, Token (QVARID _)) -> Just (Reduce 1 112)
    (320, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (320, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (321, Token (RPAREN _)) -> Just (Shift 316)
    (322, Token (WHERE _)) -> Just (Reduce 1 196)
    (322, Token (LBRACE _)) -> Just (Reduce 1 196)
    (322, Token (RBRACE _)) -> Just (Reduce 1 196)
    (322, Token (LPAREN _)) -> Just (Reduce 1 196)
    (322, Token (RPAREN _)) -> Just (Reduce 1 196)
    (322, Token (COMMA _)) -> Just (Reduce 1 196)
    (322, Token (SEMICOLON _)) -> Just (Reduce 1 196)
    (322, Token (EQUAL _)) -> Just (Reduce 1 196)
    (322, Token (DERIVING _)) -> Just (Reduce 1 196)
    (322, Token (DARROW _)) -> Just (Reduce 1 196)
    (322, Token (PIPE _)) -> Just (Reduce 1 196)
    (322, Token (COLON_COLON _)) -> Just (Reduce 1 196)
    (322, Token (MINUS _)) -> Just (Reduce 1 196)
    (322, Token (INFIXL _)) -> Just (Reduce 1 196)
    (322, Token (INFIXR _)) -> Just (Reduce 1 196)
    (322, Token (INFIX _)) -> Just (Reduce 1 196)
    (322, Token (RARROW _)) -> Just (Reduce 1 196)
    (322, Token (LBRACKET _)) -> Just (Reduce 1 196)
    (322, Token (RBRACKET _)) -> Just (Reduce 1 196)
    (322, Token (EXCL _)) -> Just (Reduce 1 196)
    (322, Token (QCONID _)) -> Just (Reduce 1 196)
    (322, Token (EXPORT _)) -> Just (Reduce 1 196)
    (322, Token (AS _)) -> Just (Reduce 1 196)
    (322, Token (QVARID _)) -> Just (Reduce 1 196)
    (322, Token (THEN _)) -> Just (Reduce 1 196)
    (322, Token (ELSE _)) -> Just (Reduce 1 196)
    (322, Token (INTEGER _)) -> Just (Reduce 1 196)
    (322, Token (QVARSYM _)) -> Just (Reduce 1 196)
    (322, Token (QCONSYM _)) -> Just (Reduce 1 196)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 1 196)
    (323, Token (WHERE _)) -> Just (Reduce 1 195)
    (323, Token (LBRACE _)) -> Just (Reduce 1 195)
    (323, Token (RBRACE _)) -> Just (Reduce 1 195)
    (323, Token (LPAREN _)) -> Just (Reduce 1 195)
    (323, Token (RPAREN _)) -> Just (Reduce 1 195)
    (323, Token (COMMA _)) -> Just (Reduce 1 195)
    (323, Token (SEMICOLON _)) -> Just (Reduce 1 195)
    (323, Token (EQUAL _)) -> Just (Reduce 1 195)
    (323, Token (DERIVING _)) -> Just (Reduce 1 195)
    (323, Token (DARROW _)) -> Just (Reduce 1 195)
    (323, Token (PIPE _)) -> Just (Reduce 1 195)
    (323, Token (COLON_COLON _)) -> Just (Reduce 1 195)
    (323, Token (MINUS _)) -> Just (Reduce 1 195)
    (323, Token (INFIXL _)) -> Just (Reduce 1 195)
    (323, Token (INFIXR _)) -> Just (Reduce 1 195)
    (323, Token (INFIX _)) -> Just (Reduce 1 195)
    (323, Token (RARROW _)) -> Just (Reduce 1 195)
    (323, Token (LBRACKET _)) -> Just (Reduce 1 195)
    (323, Token (RBRACKET _)) -> Just (Reduce 1 195)
    (323, Token (EXCL _)) -> Just (Reduce 1 195)
    (323, Token (QCONID _)) -> Just (Reduce 1 195)
    (323, Token (EXPORT _)) -> Just (Reduce 1 195)
    (323, Token (AS _)) -> Just (Reduce 1 195)
    (323, Token (QVARID _)) -> Just (Reduce 1 195)
    (323, Token (THEN _)) -> Just (Reduce 1 195)
    (323, Token (ELSE _)) -> Just (Reduce 1 195)
    (323, Token (INTEGER _)) -> Just (Reduce 1 195)
    (323, Token (QVARSYM _)) -> Just (Reduce 1 195)
    (323, Token (QCONSYM _)) -> Just (Reduce 1 195)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 1 195)
    (324, Token (WHERE _)) -> Just (Reduce 1 197)
    (324, Token (LBRACE _)) -> Just (Reduce 1 197)
    (324, Token (RBRACE _)) -> Just (Reduce 1 197)
    (324, Token (LPAREN _)) -> Just (Reduce 1 197)
    (324, Token (RPAREN _)) -> Just (Reduce 1 197)
    (324, Token (COMMA _)) -> Just (Reduce 1 197)
    (324, Token (SEMICOLON _)) -> Just (Reduce 1 197)
    (324, Token (EQUAL _)) -> Just (Reduce 1 197)
    (324, Token (DERIVING _)) -> Just (Reduce 1 197)
    (324, Token (DARROW _)) -> Just (Reduce 1 197)
    (324, Token (PIPE _)) -> Just (Reduce 1 197)
    (324, Token (COLON_COLON _)) -> Just (Reduce 1 197)
    (324, Token (MINUS _)) -> Just (Reduce 1 197)
    (324, Token (INFIXL _)) -> Just (Reduce 1 197)
    (324, Token (INFIXR _)) -> Just (Reduce 1 197)
    (324, Token (INFIX _)) -> Just (Reduce 1 197)
    (324, Token (RARROW _)) -> Just (Reduce 1 197)
    (324, Token (LBRACKET _)) -> Just (Reduce 1 197)
    (324, Token (RBRACKET _)) -> Just (Reduce 1 197)
    (324, Token (EXCL _)) -> Just (Reduce 1 197)
    (324, Token (QCONID _)) -> Just (Reduce 1 197)
    (324, Token (EXPORT _)) -> Just (Reduce 1 197)
    (324, Token (AS _)) -> Just (Reduce 1 197)
    (324, Token (QVARID _)) -> Just (Reduce 1 197)
    (324, Token (THEN _)) -> Just (Reduce 1 197)
    (324, Token (ELSE _)) -> Just (Reduce 1 197)
    (324, Token (INTEGER _)) -> Just (Reduce 1 197)
    (324, Token (QVARSYM _)) -> Just (Reduce 1 197)
    (324, Token (QCONSYM _)) -> Just (Reduce 1 197)
    (324, Token (BACKQUOTE _)) -> Just (Reduce 1 197)
    (325, Token (RPAREN _)) -> Just (Reduce 3 110)
    (325, Token (COMMA _)) -> Just (Shift 134)
    (326, Token (RPAREN _)) -> Just (Reduce 3 111)
    (327, Token (RPAREN _)) -> Just (Reduce 1 117)
    (327, Token (COMMA _)) -> Just (Shift 327)
    (328, Token (RPAREN _)) -> Just (Reduce 2 118)
    (329, Token (RBRACE _)) -> Just (Reduce 3 122)
    (329, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (329, Token (DERIVING _)) -> Just (Reduce 3 122)
    (330, Token (RBRACE _)) -> Just (Reduce 1 121)
    (330, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (330, Token (DERIVING _)) -> Just (Reduce 1 121)
    (330, Token (PIPE _)) -> Just (Shift 110)
    (331, Token (RBRACE _)) -> Just (Reduce 3 125)
    (331, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (331, Token (DERIVING _)) -> Just (Reduce 3 125)
    (331, Token (PIPE _)) -> Just (Reduce 3 125)
    (332, Token (RBRACE _)) -> Just (Reduce 4 126)
    (332, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (332, Token (DERIVING _)) -> Just (Reduce 4 126)
    (332, Token (PIPE _)) -> Just (Reduce 4 126)
    (333, Token (RBRACE _)) -> Just (Shift 332)
    (334, Token (BACKQUOTE _)) -> Just (Shift 338)
    (335, Token (LBRACE _)) -> Just (Reduce 1 187)
    (335, Token (RBRACE _)) -> Just (Reduce 1 187)
    (335, Token (LPAREN _)) -> Just (Reduce 1 187)
    (335, Token (RPAREN _)) -> Just (Reduce 1 187)
    (335, Token (COMMA _)) -> Just (Reduce 1 187)
    (335, Token (SEMICOLON _)) -> Just (Reduce 1 187)
    (335, Token (EQUAL _)) -> Just (Reduce 1 187)
    (335, Token (PIPE _)) -> Just (Reduce 1 187)
    (335, Token (COLON_COLON _)) -> Just (Reduce 1 187)
    (335, Token (MINUS _)) -> Just (Reduce 1 187)
    (335, Token (INFIXL _)) -> Just (Reduce 1 187)
    (335, Token (INFIXR _)) -> Just (Reduce 1 187)
    (335, Token (INFIX _)) -> Just (Reduce 1 187)
    (335, Token (RARROW _)) -> Just (Reduce 1 187)
    (335, Token (LBRACKET _)) -> Just (Reduce 1 187)
    (335, Token (RBRACKET _)) -> Just (Reduce 1 187)
    (335, Token (EXCL _)) -> Just (Reduce 1 187)
    (335, Token (QCONID _)) -> Just (Reduce 1 187)
    (335, Token (EXPORT _)) -> Just (Reduce 1 187)
    (335, Token (AS _)) -> Just (Reduce 1 187)
    (335, Token (QVARID _)) -> Just (Reduce 1 187)
    (335, Token (STRING _)) -> Just (Reduce 1 187)
    (335, Token (LET _)) -> Just (Reduce 1 187)
    (335, Token (LAMBDA _)) -> Just (Reduce 1 187)
    (335, Token (IF _)) -> Just (Reduce 1 187)
    (335, Token (INTEGER _)) -> Just (Reduce 1 187)
    (335, Token (QVARSYM _)) -> Just (Reduce 1 187)
    (335, Token (QCONSYM _)) -> Just (Reduce 1 187)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 1 187)
    (336, Token (QCONID _)) -> Just (Shift 334)
    (336, Token (EXPORT _)) -> Just (Shift 370)
    (336, Token (AS _)) -> Just (Shift 371)
    (336, Token (QVARID _)) -> Just (Shift 372)
    (337, Token (QCONID _)) -> Just (Shift 334)
    (338, Token (LBRACE _)) -> Just (Reduce 3 188)
    (338, Token (RBRACE _)) -> Just (Reduce 3 188)
    (338, Token (LPAREN _)) -> Just (Reduce 3 188)
    (338, Token (RPAREN _)) -> Just (Reduce 3 188)
    (338, Token (COMMA _)) -> Just (Reduce 3 188)
    (338, Token (SEMICOLON _)) -> Just (Reduce 3 188)
    (338, Token (EQUAL _)) -> Just (Reduce 3 188)
    (338, Token (PIPE _)) -> Just (Reduce 3 188)
    (338, Token (COLON_COLON _)) -> Just (Reduce 3 188)
    (338, Token (MINUS _)) -> Just (Reduce 3 188)
    (338, Token (INFIXL _)) -> Just (Reduce 3 188)
    (338, Token (INFIXR _)) -> Just (Reduce 3 188)
    (338, Token (INFIX _)) -> Just (Reduce 3 188)
    (338, Token (RARROW _)) -> Just (Reduce 3 188)
    (338, Token (LBRACKET _)) -> Just (Reduce 3 188)
    (338, Token (RBRACKET _)) -> Just (Reduce 3 188)
    (338, Token (EXCL _)) -> Just (Reduce 3 188)
    (338, Token (QCONID _)) -> Just (Reduce 3 188)
    (338, Token (EXPORT _)) -> Just (Reduce 3 188)
    (338, Token (AS _)) -> Just (Reduce 3 188)
    (338, Token (QVARID _)) -> Just (Reduce 3 188)
    (338, Token (STRING _)) -> Just (Reduce 3 188)
    (338, Token (LET _)) -> Just (Reduce 3 188)
    (338, Token (LAMBDA _)) -> Just (Reduce 3 188)
    (338, Token (IF _)) -> Just (Reduce 3 188)
    (338, Token (INTEGER _)) -> Just (Reduce 3 188)
    (338, Token (QVARSYM _)) -> Just (Reduce 3 188)
    (338, Token (QCONSYM _)) -> Just (Reduce 3 188)
    (338, Token (BACKQUOTE _)) -> Just (Reduce 3 188)
    (339, Token (RBRACE _)) -> Just (Reduce 3 130)
    (340, Token (RBRACE _)) -> Just (Reduce 1 129)
    (340, Token (COMMA _)) -> Just (Shift 77)
    (341, Token (RBRACE _)) -> Just (Reduce 3 131)
    (341, Token (COMMA _)) -> Just (Reduce 3 131)
    (342, Token (COLON_COLON _)) -> Just (Shift 122)
    (343, Token (EXPORT _)) -> Just (Reduce 1 139)
    (343, Token (AS _)) -> Just (Reduce 1 139)
    (343, Token (QVARID _)) -> Just (Reduce 1 139)
    (343, Token (STRING _)) -> Just (Reduce 1 139)
    (344, Token (EXPORT _)) -> Just (Reduce 1 138)
    (344, Token (AS _)) -> Just (Reduce 1 138)
    (344, Token (QVARID _)) -> Just (Reduce 1 138)
    (344, Token (STRING _)) -> Just (Reduce 1 138)
    (345, Token (EXPORT _)) -> Just (Reduce 1 140)
    (345, Token (AS _)) -> Just (Reduce 1 140)
    (345, Token (QVARID _)) -> Just (Reduce 1 140)
    (345, Token (STRING _)) -> Just (Reduce 1 140)
    (346, Token (LPAREN _)) -> Just (Reduce 1 141)
    (346, Token (MINUS _)) -> Just (Reduce 1 141)
    (346, Token (EXPORT _)) -> Just (Reduce 1 141)
    (346, Token (AS _)) -> Just (Reduce 1 141)
    (346, Token (QVARID _)) -> Just (Reduce 1 141)
    (346, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (347, Token (STRING _)) -> Just (Reduce 1 144)
    (348, Token (STRING _)) -> Just (Reduce 1 143)
    (349, Token (STRING _)) -> Just (Reduce 1 145)
    (350, Token (LPAREN _)) -> Just (Reduce 1 142)
    (350, Token (MINUS _)) -> Just (Reduce 1 142)
    (350, Token (EXPORT _)) -> Just (Reduce 1 142)
    (350, Token (AS _)) -> Just (Reduce 1 142)
    (350, Token (QVARID _)) -> Just (Reduce 1 142)
    (350, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (351, Token (EQUAL _)) -> Just (Reduce 3 149)
    (352, Token (COMMA _)) -> Just (Shift 51)
    (352, Token (EQUAL _)) -> Just (Reduce 1 148)
    (353, Token (COMMA _)) -> Just (Reduce 2 151)
    (353, Token (EQUAL _)) -> Just (Reduce 2 151)
    (354, Token (COMMA _)) -> Just (Reduce 3 150)
    (354, Token (EQUAL _)) -> Just (Reduce 3 150)
    (355, Token (COMMA _)) -> Just (Reduce 1 152)
    (355, Token (EQUAL _)) -> Just (Reduce 1 152)
    (355, Token (LARROW _)) -> Just (Shift 54)
    (356, Token (WHERE _)) -> Just (Reduce 1 159)
    (356, Token (RBRACE _)) -> Just (Reduce 1 159)
    (356, Token (RPAREN _)) -> Just (Reduce 1 159)
    (356, Token (COMMA _)) -> Just (Reduce 1 159)
    (356, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (356, Token (EQUAL _)) -> Just (Reduce 1 159)
    (356, Token (PIPE _)) -> Just (Reduce 1 159)
    (356, Token (COLON_COLON _)) -> Just (Reduce 1 159)
    (356, Token (LARROW _)) -> Just (Reduce 1 159)
    (356, Token (THEN _)) -> Just (Reduce 1 159)
    (356, Token (ELSE _)) -> Just (Reduce 1 159)
    (357, Token (THEN _)) -> Just (Reduce 1 203)
    (357, Token (ELSE _)) -> Just (Reduce 1 203)
    (358, Token (WHERE _)) -> Just (Reduce 2 160)
    (358, Token (RBRACE _)) -> Just (Reduce 2 160)
    (358, Token (RPAREN _)) -> Just (Reduce 2 160)
    (358, Token (COMMA _)) -> Just (Reduce 2 160)
    (358, Token (SEMICOLON _)) -> Just (Reduce 2 160)
    (358, Token (EQUAL _)) -> Just (Reduce 2 160)
    (358, Token (PIPE _)) -> Just (Reduce 2 160)
    (358, Token (COLON_COLON _)) -> Just (Reduce 2 160)
    (358, Token (LARROW _)) -> Just (Reduce 2 160)
    (358, Token (THEN _)) -> Just (Reduce 2 160)
    (358, Token (ELSE _)) -> Just (Reduce 2 160)
    (359, Token (WHERE _)) -> Just (Reduce 1 162)
    (359, Token (LBRACE _)) -> Just (Reduce 1 162)
    (359, Token (RBRACE _)) -> Just (Reduce 1 162)
    (359, Token (LPAREN _)) -> Just (Reduce 1 162)
    (359, Token (RPAREN _)) -> Just (Reduce 1 162)
    (359, Token (COMMA _)) -> Just (Reduce 1 162)
    (359, Token (SEMICOLON _)) -> Just (Reduce 1 162)
    (359, Token (EQUAL _)) -> Just (Reduce 1 162)
    (359, Token (PIPE _)) -> Just (Reduce 1 162)
    (359, Token (COLON_COLON _)) -> Just (Reduce 1 162)
    (359, Token (MINUS _)) -> Just (Reduce 1 162)
    (359, Token (INFIXL _)) -> Just (Reduce 1 162)
    (359, Token (INFIXR _)) -> Just (Reduce 1 162)
    (359, Token (INFIX _)) -> Just (Reduce 1 162)
    (359, Token (QCONID _)) -> Just (Reduce 1 162)
    (359, Token (EXPORT _)) -> Just (Reduce 1 162)
    (359, Token (AS _)) -> Just (Reduce 1 162)
    (359, Token (QVARID _)) -> Just (Reduce 1 162)
    (359, Token (STRING _)) -> Just (Reduce 1 162)
    (359, Token (LARROW _)) -> Just (Reduce 1 162)
    (359, Token (LET _)) -> Just (Reduce 1 162)
    (359, Token (LAMBDA _)) -> Just (Reduce 1 162)
    (359, Token (IF _)) -> Just (Reduce 1 162)
    (359, Token (THEN _)) -> Just (Reduce 1 162)
    (359, Token (ELSE _)) -> Just (Reduce 1 162)
    (359, Token (INTEGER _)) -> Just (Reduce 1 162)
    (359, Token (QVARSYM _)) -> Just (Reduce 1 162)
    (359, Token (QCONSYM _)) -> Just (Reduce 1 162)
    (359, Token (BACKQUOTE _)) -> Just (Reduce 1 162)
    (360, Token (WHERE _)) -> Just (Reduce 3 164)
    (360, Token (LBRACE _)) -> Just (Reduce 3 164)
    (360, Token (RBRACE _)) -> Just (Reduce 3 164)
    (360, Token (LPAREN _)) -> Just (Reduce 3 164)
    (360, Token (RPAREN _)) -> Just (Reduce 3 164)
    (360, Token (COMMA _)) -> Just (Reduce 3 164)
    (360, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (360, Token (EQUAL _)) -> Just (Reduce 3 164)
    (360, Token (PIPE _)) -> Just (Reduce 3 164)
    (360, Token (COLON_COLON _)) -> Just (Reduce 3 164)
    (360, Token (MINUS _)) -> Just (Reduce 3 164)
    (360, Token (INFIXL _)) -> Just (Reduce 3 164)
    (360, Token (INFIXR _)) -> Just (Reduce 3 164)
    (360, Token (INFIX _)) -> Just (Reduce 3 164)
    (360, Token (QCONID _)) -> Just (Reduce 3 164)
    (360, Token (EXPORT _)) -> Just (Reduce 3 164)
    (360, Token (AS _)) -> Just (Reduce 3 164)
    (360, Token (QVARID _)) -> Just (Reduce 3 164)
    (360, Token (STRING _)) -> Just (Reduce 3 164)
    (360, Token (LARROW _)) -> Just (Reduce 3 164)
    (360, Token (LET _)) -> Just (Reduce 3 164)
    (360, Token (LAMBDA _)) -> Just (Reduce 3 164)
    (360, Token (IF _)) -> Just (Reduce 3 164)
    (360, Token (THEN _)) -> Just (Reduce 3 164)
    (360, Token (ELSE _)) -> Just (Reduce 3 164)
    (360, Token (INTEGER _)) -> Just (Reduce 3 164)
    (360, Token (QVARSYM _)) -> Just (Reduce 3 164)
    (360, Token (QCONSYM _)) -> Just (Reduce 3 164)
    (360, Token (BACKQUOTE _)) -> Just (Reduce 3 164)
    (361, Token (WHERE _)) -> Just (Reduce 3 165)
    (361, Token (LBRACE _)) -> Just (Reduce 3 165)
    (361, Token (RBRACE _)) -> Just (Reduce 3 165)
    (361, Token (LPAREN _)) -> Just (Reduce 3 165)
    (361, Token (RPAREN _)) -> Just (Reduce 3 165)
    (361, Token (COMMA _)) -> Just (Reduce 3 165)
    (361, Token (SEMICOLON _)) -> Just (Reduce 3 165)
    (361, Token (EQUAL _)) -> Just (Reduce 3 165)
    (361, Token (PIPE _)) -> Just (Reduce 3 165)
    (361, Token (COLON_COLON _)) -> Just (Reduce 3 165)
    (361, Token (MINUS _)) -> Just (Reduce 3 165)
    (361, Token (INFIXL _)) -> Just (Reduce 3 165)
    (361, Token (INFIXR _)) -> Just (Reduce 3 165)
    (361, Token (INFIX _)) -> Just (Reduce 3 165)
    (361, Token (QCONID _)) -> Just (Reduce 3 165)
    (361, Token (EXPORT _)) -> Just (Reduce 3 165)
    (361, Token (AS _)) -> Just (Reduce 3 165)
    (361, Token (QVARID _)) -> Just (Reduce 3 165)
    (361, Token (STRING _)) -> Just (Reduce 3 165)
    (361, Token (LARROW _)) -> Just (Reduce 3 165)
    (361, Token (LET _)) -> Just (Reduce 3 165)
    (361, Token (LAMBDA _)) -> Just (Reduce 3 165)
    (361, Token (IF _)) -> Just (Reduce 3 165)
    (361, Token (THEN _)) -> Just (Reduce 3 165)
    (361, Token (ELSE _)) -> Just (Reduce 3 165)
    (361, Token (INTEGER _)) -> Just (Reduce 3 165)
    (361, Token (QVARSYM _)) -> Just (Reduce 3 165)
    (361, Token (QCONSYM _)) -> Just (Reduce 3 165)
    (361, Token (BACKQUOTE _)) -> Just (Reduce 3 165)
    (362, Token (WHERE _)) -> Just (Reduce 2 163)
    (362, Token (LBRACE _)) -> Just (Reduce 2 163)
    (362, Token (RBRACE _)) -> Just (Reduce 2 163)
    (362, Token (LPAREN _)) -> Just (Reduce 2 163)
    (362, Token (RPAREN _)) -> Just (Reduce 2 163)
    (362, Token (COMMA _)) -> Just (Reduce 2 163)
    (362, Token (SEMICOLON _)) -> Just (Reduce 2 163)
    (362, Token (EQUAL _)) -> Just (Reduce 2 163)
    (362, Token (PIPE _)) -> Just (Reduce 2 163)
    (362, Token (COLON_COLON _)) -> Just (Reduce 2 163)
    (362, Token (MINUS _)) -> Just (Reduce 2 163)
    (362, Token (INFIXL _)) -> Just (Reduce 2 163)
    (362, Token (INFIXR _)) -> Just (Reduce 2 163)
    (362, Token (INFIX _)) -> Just (Reduce 2 163)
    (362, Token (QCONID _)) -> Just (Reduce 2 163)
    (362, Token (EXPORT _)) -> Just (Reduce 2 163)
    (362, Token (AS _)) -> Just (Reduce 2 163)
    (362, Token (QVARID _)) -> Just (Reduce 2 163)
    (362, Token (STRING _)) -> Just (Reduce 2 163)
    (362, Token (LARROW _)) -> Just (Reduce 2 163)
    (362, Token (LET _)) -> Just (Reduce 2 163)
    (362, Token (LAMBDA _)) -> Just (Reduce 2 163)
    (362, Token (IF _)) -> Just (Reduce 2 163)
    (362, Token (THEN _)) -> Just (Reduce 2 163)
    (362, Token (ELSE _)) -> Just (Reduce 2 163)
    (362, Token (INTEGER _)) -> Just (Reduce 2 163)
    (362, Token (QVARSYM _)) -> Just (Reduce 2 163)
    (362, Token (QCONSYM _)) -> Just (Reduce 2 163)
    (362, Token (BACKQUOTE _)) -> Just (Reduce 2 163)
    (363, Token (WHERE _)) -> Just (Reduce 3 169)
    (363, Token (LBRACE _)) -> Just (Reduce 3 169)
    (363, Token (RBRACE _)) -> Just (Reduce 3 169)
    (363, Token (LPAREN _)) -> Just (Reduce 3 169)
    (363, Token (RPAREN _)) -> Just (Reduce 3 169)
    (363, Token (COMMA _)) -> Just (Reduce 3 169)
    (363, Token (SEMICOLON _)) -> Just (Reduce 3 169)
    (363, Token (EQUAL _)) -> Just (Reduce 3 169)
    (363, Token (PIPE _)) -> Just (Reduce 3 169)
    (363, Token (COLON_COLON _)) -> Just (Reduce 3 169)
    (363, Token (MINUS _)) -> Just (Reduce 3 169)
    (363, Token (INFIXL _)) -> Just (Reduce 3 169)
    (363, Token (INFIXR _)) -> Just (Reduce 3 169)
    (363, Token (INFIX _)) -> Just (Reduce 3 169)
    (363, Token (QCONID _)) -> Just (Reduce 3 169)
    (363, Token (EXPORT _)) -> Just (Reduce 3 169)
    (363, Token (AS _)) -> Just (Reduce 3 169)
    (363, Token (QVARID _)) -> Just (Reduce 3 169)
    (363, Token (STRING _)) -> Just (Reduce 3 169)
    (363, Token (LARROW _)) -> Just (Reduce 3 169)
    (363, Token (LET _)) -> Just (Reduce 3 169)
    (363, Token (LAMBDA _)) -> Just (Reduce 3 169)
    (363, Token (IF _)) -> Just (Reduce 3 169)
    (363, Token (THEN _)) -> Just (Reduce 3 169)
    (363, Token (ELSE _)) -> Just (Reduce 3 169)
    (363, Token (INTEGER _)) -> Just (Reduce 3 169)
    (363, Token (QVARSYM _)) -> Just (Reduce 3 169)
    (363, Token (QCONSYM _)) -> Just (Reduce 3 169)
    (363, Token (BACKQUOTE _)) -> Just (Reduce 3 169)
    (364, Token (WHERE _)) -> Just (Reduce 1 168)
    (364, Token (LBRACE _)) -> Just (Reduce 1 168)
    (364, Token (RBRACE _)) -> Just (Reduce 1 168)
    (364, Token (LPAREN _)) -> Just (Reduce 1 168)
    (364, Token (RPAREN _)) -> Just (Reduce 1 168)
    (364, Token (COMMA _)) -> Just (Reduce 1 168)
    (364, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (364, Token (EQUAL _)) -> Just (Reduce 1 168)
    (364, Token (PIPE _)) -> Just (Reduce 1 168)
    (364, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (364, Token (MINUS _)) -> Just (Reduce 1 168)
    (364, Token (INFIXL _)) -> Just (Reduce 1 168)
    (364, Token (INFIXR _)) -> Just (Reduce 1 168)
    (364, Token (INFIX _)) -> Just (Reduce 1 168)
    (364, Token (QCONID _)) -> Just (Reduce 1 168)
    (364, Token (EXPORT _)) -> Just (Reduce 1 168)
    (364, Token (AS _)) -> Just (Reduce 1 168)
    (364, Token (QVARID _)) -> Just (Reduce 1 168)
    (364, Token (STRING _)) -> Just (Reduce 1 168)
    (364, Token (LARROW _)) -> Just (Reduce 1 168)
    (364, Token (LET _)) -> Just (Reduce 1 168)
    (364, Token (LAMBDA _)) -> Just (Reduce 1 168)
    (364, Token (IF _)) -> Just (Reduce 1 168)
    (364, Token (THEN _)) -> Just (Reduce 1 168)
    (364, Token (ELSE _)) -> Just (Reduce 1 168)
    (364, Token (INTEGER _)) -> Just (Reduce 1 168)
    (364, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (364, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (364, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (365, Token (WHERE _)) -> Just (Reduce 1 167)
    (365, Token (LBRACE _)) -> Just (Reduce 1 167)
    (365, Token (RBRACE _)) -> Just (Reduce 1 167)
    (365, Token (LPAREN _)) -> Just (Reduce 1 167)
    (365, Token (RPAREN _)) -> Just (Reduce 1 167)
    (365, Token (COMMA _)) -> Just (Reduce 1 167)
    (365, Token (SEMICOLON _)) -> Just (Reduce 1 167)
    (365, Token (EQUAL _)) -> Just (Reduce 1 167)
    (365, Token (PIPE _)) -> Just (Reduce 1 167)
    (365, Token (COLON_COLON _)) -> Just (Reduce 1 167)
    (365, Token (MINUS _)) -> Just (Reduce 1 167)
    (365, Token (INFIXL _)) -> Just (Reduce 1 167)
    (365, Token (INFIXR _)) -> Just (Reduce 1 167)
    (365, Token (INFIX _)) -> Just (Reduce 1 167)
    (365, Token (QCONID _)) -> Just (Reduce 1 167)
    (365, Token (EXPORT _)) -> Just (Reduce 1 167)
    (365, Token (AS _)) -> Just (Reduce 1 167)
    (365, Token (QVARID _)) -> Just (Reduce 1 167)
    (365, Token (STRING _)) -> Just (Reduce 1 167)
    (365, Token (LARROW _)) -> Just (Reduce 1 167)
    (365, Token (LET _)) -> Just (Reduce 1 167)
    (365, Token (LAMBDA _)) -> Just (Reduce 1 167)
    (365, Token (IF _)) -> Just (Reduce 1 167)
    (365, Token (THEN _)) -> Just (Reduce 1 167)
    (365, Token (ELSE _)) -> Just (Reduce 1 167)
    (365, Token (INTEGER _)) -> Just (Reduce 1 167)
    (365, Token (QVARSYM _)) -> Just (Reduce 1 167)
    (365, Token (QCONSYM _)) -> Just (Reduce 1 167)
    (365, Token (BACKQUOTE _)) -> Just (Reduce 1 167)
    (366, Token (WHERE _)) -> Just (Reduce 1 166)
    (366, Token (LBRACE _)) -> Just (Reduce 1 166)
    (366, Token (RBRACE _)) -> Just (Reduce 1 166)
    (366, Token (LPAREN _)) -> Just (Reduce 1 166)
    (366, Token (RPAREN _)) -> Just (Reduce 1 166)
    (366, Token (COMMA _)) -> Just (Reduce 1 166)
    (366, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (366, Token (EQUAL _)) -> Just (Reduce 1 166)
    (366, Token (PIPE _)) -> Just (Reduce 1 166)
    (366, Token (COLON_COLON _)) -> Just (Reduce 1 166)
    (366, Token (MINUS _)) -> Just (Reduce 1 166)
    (366, Token (INFIXL _)) -> Just (Reduce 1 166)
    (366, Token (INFIXR _)) -> Just (Reduce 1 166)
    (366, Token (INFIX _)) -> Just (Reduce 1 166)
    (366, Token (QCONID _)) -> Just (Reduce 1 166)
    (366, Token (EXPORT _)) -> Just (Reduce 1 166)
    (366, Token (AS _)) -> Just (Reduce 1 166)
    (366, Token (QVARID _)) -> Just (Reduce 1 166)
    (366, Token (STRING _)) -> Just (Reduce 1 166)
    (366, Token (LARROW _)) -> Just (Reduce 1 166)
    (366, Token (LET _)) -> Just (Reduce 1 166)
    (366, Token (LAMBDA _)) -> Just (Reduce 1 166)
    (366, Token (IF _)) -> Just (Reduce 1 166)
    (366, Token (THEN _)) -> Just (Reduce 1 166)
    (366, Token (ELSE _)) -> Just (Reduce 1 166)
    (366, Token (INTEGER _)) -> Just (Reduce 1 166)
    (366, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (366, Token (QCONSYM _)) -> Just (Reduce 1 166)
    (366, Token (BACKQUOTE _)) -> Just (Reduce 1 166)
    (367, Token (RPAREN _)) -> Just (Shift 363)
    (368, Token (LPAREN _)) -> Just (Reduce 3 175)
    (368, Token (RPAREN _)) -> Just (Reduce 3 175)
    (368, Token (EQUAL _)) -> Just (Reduce 3 175)
    (368, Token (PIPE _)) -> Just (Reduce 3 175)
    (368, Token (MINUS _)) -> Just (Reduce 3 175)
    (368, Token (RARROW _)) -> Just (Reduce 3 175)
    (368, Token (QCONID _)) -> Just (Reduce 3 175)
    (368, Token (EXPORT _)) -> Just (Reduce 3 175)
    (368, Token (AS _)) -> Just (Reduce 3 175)
    (368, Token (QVARID _)) -> Just (Reduce 3 175)
    (368, Token (QVARSYM _)) -> Just (Reduce 3 175)
    (368, Token (QCONSYM _)) -> Just (Reduce 3 175)
    (368, Token (BACKQUOTE _)) -> Just (Reduce 3 175)
    (369, Token (LPAREN _)) -> Just (Reduce 1 174)
    (369, Token (RPAREN _)) -> Just (Reduce 1 174)
    (369, Token (EQUAL _)) -> Just (Reduce 1 174)
    (369, Token (PIPE _)) -> Just (Reduce 1 174)
    (369, Token (MINUS _)) -> Just (Reduce 1 174)
    (369, Token (RARROW _)) -> Just (Reduce 1 174)
    (369, Token (QCONID _)) -> Just (Reduce 1 174)
    (369, Token (EXPORT _)) -> Just (Reduce 1 174)
    (369, Token (AS _)) -> Just (Reduce 1 174)
    (369, Token (QVARID _)) -> Just (Reduce 1 174)
    (369, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (369, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (369, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (370, Token (BACKQUOTE _)) -> Just (Shift 374)
    (371, Token (BACKQUOTE _)) -> Just (Shift 375)
    (372, Token (BACKQUOTE _)) -> Just (Shift 376)
    (373, Token (LBRACE _)) -> Just (Reduce 1 183)
    (373, Token (RBRACE _)) -> Just (Reduce 1 183)
    (373, Token (LPAREN _)) -> Just (Reduce 1 183)
    (373, Token (COMMA _)) -> Just (Reduce 1 183)
    (373, Token (SEMICOLON _)) -> Just (Reduce 1 183)
    (373, Token (EQUAL _)) -> Just (Reduce 1 183)
    (373, Token (PIPE _)) -> Just (Reduce 1 183)
    (373, Token (COLON_COLON _)) -> Just (Reduce 1 183)
    (373, Token (MINUS _)) -> Just (Reduce 1 183)
    (373, Token (INFIXL _)) -> Just (Reduce 1 183)
    (373, Token (INFIXR _)) -> Just (Reduce 1 183)
    (373, Token (INFIX _)) -> Just (Reduce 1 183)
    (373, Token (QCONID _)) -> Just (Reduce 1 183)
    (373, Token (EXPORT _)) -> Just (Reduce 1 183)
    (373, Token (AS _)) -> Just (Reduce 1 183)
    (373, Token (QVARID _)) -> Just (Reduce 1 183)
    (373, Token (STRING _)) -> Just (Reduce 1 183)
    (373, Token (LET _)) -> Just (Reduce 1 183)
    (373, Token (LAMBDA _)) -> Just (Reduce 1 183)
    (373, Token (IF _)) -> Just (Reduce 1 183)
    (373, Token (INTEGER _)) -> Just (Reduce 1 183)
    (373, Token (QVARSYM _)) -> Just (Reduce 1 183)
    (373, Token (QCONSYM _)) -> Just (Reduce 1 183)
    (373, Token (BACKQUOTE _)) -> Just (Reduce 1 183)
    (374, Token (LBRACE _)) -> Just (Reduce 3 185)
    (374, Token (RBRACE _)) -> Just (Reduce 3 185)
    (374, Token (LPAREN _)) -> Just (Reduce 3 185)
    (374, Token (COMMA _)) -> Just (Reduce 3 185)
    (374, Token (SEMICOLON _)) -> Just (Reduce 3 185)
    (374, Token (EQUAL _)) -> Just (Reduce 3 185)
    (374, Token (PIPE _)) -> Just (Reduce 3 185)
    (374, Token (COLON_COLON _)) -> Just (Reduce 3 185)
    (374, Token (MINUS _)) -> Just (Reduce 3 185)
    (374, Token (INFIXL _)) -> Just (Reduce 3 185)
    (374, Token (INFIXR _)) -> Just (Reduce 3 185)
    (374, Token (INFIX _)) -> Just (Reduce 3 185)
    (374, Token (QCONID _)) -> Just (Reduce 3 185)
    (374, Token (EXPORT _)) -> Just (Reduce 3 185)
    (374, Token (AS _)) -> Just (Reduce 3 185)
    (374, Token (QVARID _)) -> Just (Reduce 3 185)
    (374, Token (STRING _)) -> Just (Reduce 3 185)
    (374, Token (LET _)) -> Just (Reduce 3 185)
    (374, Token (LAMBDA _)) -> Just (Reduce 3 185)
    (374, Token (IF _)) -> Just (Reduce 3 185)
    (374, Token (INTEGER _)) -> Just (Reduce 3 185)
    (374, Token (QVARSYM _)) -> Just (Reduce 3 185)
    (374, Token (QCONSYM _)) -> Just (Reduce 3 185)
    (374, Token (BACKQUOTE _)) -> Just (Reduce 3 185)
    (375, Token (LBRACE _)) -> Just (Reduce 3 184)
    (375, Token (RBRACE _)) -> Just (Reduce 3 184)
    (375, Token (LPAREN _)) -> Just (Reduce 3 184)
    (375, Token (COMMA _)) -> Just (Reduce 3 184)
    (375, Token (SEMICOLON _)) -> Just (Reduce 3 184)
    (375, Token (EQUAL _)) -> Just (Reduce 3 184)
    (375, Token (PIPE _)) -> Just (Reduce 3 184)
    (375, Token (COLON_COLON _)) -> Just (Reduce 3 184)
    (375, Token (MINUS _)) -> Just (Reduce 3 184)
    (375, Token (INFIXL _)) -> Just (Reduce 3 184)
    (375, Token (INFIXR _)) -> Just (Reduce 3 184)
    (375, Token (INFIX _)) -> Just (Reduce 3 184)
    (375, Token (QCONID _)) -> Just (Reduce 3 184)
    (375, Token (EXPORT _)) -> Just (Reduce 3 184)
    (375, Token (AS _)) -> Just (Reduce 3 184)
    (375, Token (QVARID _)) -> Just (Reduce 3 184)
    (375, Token (STRING _)) -> Just (Reduce 3 184)
    (375, Token (LET _)) -> Just (Reduce 3 184)
    (375, Token (LAMBDA _)) -> Just (Reduce 3 184)
    (375, Token (IF _)) -> Just (Reduce 3 184)
    (375, Token (INTEGER _)) -> Just (Reduce 3 184)
    (375, Token (QVARSYM _)) -> Just (Reduce 3 184)
    (375, Token (QCONSYM _)) -> Just (Reduce 3 184)
    (375, Token (BACKQUOTE _)) -> Just (Reduce 3 184)
    (376, Token (LBRACE _)) -> Just (Reduce 3 186)
    (376, Token (RBRACE _)) -> Just (Reduce 3 186)
    (376, Token (LPAREN _)) -> Just (Reduce 3 186)
    (376, Token (COMMA _)) -> Just (Reduce 3 186)
    (376, Token (SEMICOLON _)) -> Just (Reduce 3 186)
    (376, Token (EQUAL _)) -> Just (Reduce 3 186)
    (376, Token (PIPE _)) -> Just (Reduce 3 186)
    (376, Token (COLON_COLON _)) -> Just (Reduce 3 186)
    (376, Token (MINUS _)) -> Just (Reduce 3 186)
    (376, Token (INFIXL _)) -> Just (Reduce 3 186)
    (376, Token (INFIXR _)) -> Just (Reduce 3 186)
    (376, Token (INFIX _)) -> Just (Reduce 3 186)
    (376, Token (QCONID _)) -> Just (Reduce 3 186)
    (376, Token (EXPORT _)) -> Just (Reduce 3 186)
    (376, Token (AS _)) -> Just (Reduce 3 186)
    (376, Token (QVARID _)) -> Just (Reduce 3 186)
    (376, Token (STRING _)) -> Just (Reduce 3 186)
    (376, Token (LET _)) -> Just (Reduce 3 186)
    (376, Token (LAMBDA _)) -> Just (Reduce 3 186)
    (376, Token (IF _)) -> Just (Reduce 3 186)
    (376, Token (INTEGER _)) -> Just (Reduce 3 186)
    (376, Token (QVARSYM _)) -> Just (Reduce 3 186)
    (376, Token (QCONSYM _)) -> Just (Reduce 3 186)
    (376, Token (BACKQUOTE _)) -> Just (Reduce 3 186)
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
production 154 = 32
production 155 = 32
production 156 = 32
production 157 = 32
production 158 = 32
production 159 = 61
production 160 = 63
production 161 = 63
production 162 = 64
production 163 = 64
production 164 = 64
production 165 = 64
production 166 = 65
production 167 = 65
production 168 = 65
production 169 = 65
production 170 = 31
production 171 = 31
production 172 = 31
production 173 = 31
production 174 = 66
production 175 = 66
production 176 = 8
production 177 = 8
production 178 = 8
production 179 = 8
production 180 = 8
production 181 = 9
production 182 = 9
production 183 = 67
production 184 = 67
production 185 = 67
production 186 = 67
production 187 = 52
production 188 = 52
production 189 = 44
production 190 = 44
production 191 = 16
production 192 = 16
production 193 = 15
production 194 = 15
production 195 = 47
production 196 = 47
production 197 = 47
production 198 = 68
production 199 = 1
production 200 = 42
production 201 = 42
production 202 = 62
production 203 = 62

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 177
    (9, 1) -> Just 202
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 291
    (13, 14) -> Just 18
    (13, 27) -> Just 200
    (13, 30) -> Just 236
    (13, 31) -> Just 63
    (13, 40) -> Just 251
    (13, 41) -> Just 252
    (13, 66) -> Just 255
    (16, 4) -> Just 17
    (16, 8) -> Just 291
    (16, 14) -> Just 18
    (16, 27) -> Just 200
    (16, 30) -> Just 236
    (16, 31) -> Just 63
    (16, 40) -> Just 251
    (16, 41) -> Just 252
    (16, 66) -> Just 255
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 151
    (25, 9) -> Just 152
    (25, 10) -> Just 33
    (25, 13) -> Just 141
    (34, 8) -> Just 366
    (34, 44) -> Just 37
    (34, 52) -> Just 303
    (34, 65) -> Just 362
    (34, 67) -> Just 304
    (35, 8) -> Just 366
    (35, 63) -> Just 358
    (35, 64) -> Just 34
    (35, 65) -> Just 359
    (36, 8) -> Just 366
    (36, 65) -> Just 360
    (37, 8) -> Just 366
    (37, 65) -> Just 361
    (38, 8) -> Just 366
    (38, 32) -> Just 262
    (38, 61) -> Just 267
    (38, 63) -> Just 356
    (38, 64) -> Just 34
    (38, 65) -> Just 359
    (39, 8) -> Just 366
    (39, 32) -> Just 263
    (39, 61) -> Just 267
    (39, 63) -> Just 356
    (39, 64) -> Just 34
    (39, 65) -> Just 359
    (40, 8) -> Just 366
    (40, 32) -> Just 266
    (40, 61) -> Just 267
    (40, 63) -> Just 356
    (40, 64) -> Just 34
    (40, 65) -> Just 359
    (41, 8) -> Just 366
    (41, 32) -> Just 237
    (41, 61) -> Just 267
    (41, 63) -> Just 356
    (41, 64) -> Just 34
    (41, 65) -> Just 359
    (42, 8) -> Just 366
    (42, 32) -> Just 270
    (42, 61) -> Just 267
    (42, 63) -> Just 356
    (42, 64) -> Just 34
    (42, 65) -> Just 359
    (43, 8) -> Just 366
    (43, 32) -> Just 280
    (43, 61) -> Just 267
    (43, 63) -> Just 356
    (43, 64) -> Just 34
    (43, 65) -> Just 359
    (44, 8) -> Just 366
    (44, 32) -> Just 288
    (44, 61) -> Just 267
    (44, 63) -> Just 356
    (44, 64) -> Just 34
    (44, 65) -> Just 359
    (45, 8) -> Just 366
    (45, 32) -> Just 367
    (45, 61) -> Just 267
    (45, 63) -> Just 356
    (45, 64) -> Just 34
    (45, 65) -> Just 359
    (46, 8) -> Just 366
    (46, 63) -> Just 358
    (46, 64) -> Just 34
    (46, 65) -> Just 359
    (47, 8) -> Just 366
    (47, 33) -> Just 238
    (47, 59) -> Just 272
    (47, 60) -> Just 352
    (47, 61) -> Just 355
    (47, 63) -> Just 356
    (47, 64) -> Just 34
    (47, 65) -> Just 359
    (48, 8) -> Just 366
    (48, 33) -> Just 271
    (48, 59) -> Just 272
    (48, 60) -> Just 352
    (48, 61) -> Just 355
    (48, 63) -> Just 356
    (48, 64) -> Just 34
    (48, 65) -> Just 359
    (49, 8) -> Just 366
    (49, 33) -> Just 281
    (49, 59) -> Just 272
    (49, 60) -> Just 352
    (49, 61) -> Just 355
    (49, 63) -> Just 356
    (49, 64) -> Just 34
    (49, 65) -> Just 359
    (50, 8) -> Just 366
    (50, 33) -> Just 289
    (50, 59) -> Just 272
    (50, 60) -> Just 352
    (50, 61) -> Just 355
    (50, 63) -> Just 356
    (50, 64) -> Just 34
    (50, 65) -> Just 359
    (51, 8) -> Just 366
    (51, 59) -> Just 351
    (51, 60) -> Just 352
    (51, 61) -> Just 355
    (51, 63) -> Just 356
    (51, 64) -> Just 34
    (51, 65) -> Just 359
    (52, 8) -> Just 366
    (52, 32) -> Just 264
    (52, 61) -> Just 267
    (52, 63) -> Just 356
    (52, 64) -> Just 34
    (52, 65) -> Just 359
    (53, 8) -> Just 366
    (53, 32) -> Just 265
    (53, 61) -> Just 267
    (53, 63) -> Just 356
    (53, 64) -> Just 34
    (53, 65) -> Just 359
    (54, 8) -> Just 366
    (54, 32) -> Just 354
    (54, 61) -> Just 267
    (54, 63) -> Just 356
    (54, 64) -> Just 34
    (54, 65) -> Just 359
    (55, 8) -> Just 369
    (55, 66) -> Just 256
    (56, 8) -> Just 369
    (56, 66) -> Just 258
    (57, 8) -> Just 369
    (57, 31) -> Just 58
    (57, 66) -> Just 255
    (58, 8) -> Just 369
    (58, 44) -> Just 56
    (58, 52) -> Just 303
    (58, 66) -> Just 257
    (58, 67) -> Just 304
    (59, 8) -> Just 291
    (59, 27) -> Just 247
    (59, 29) -> Just 246
    (59, 30) -> Just 236
    (59, 31) -> Just 63
    (59, 40) -> Just 251
    (59, 41) -> Just 252
    (59, 66) -> Just 255
    (60, 8) -> Just 291
    (60, 27) -> Just 247
    (60, 29) -> Just 248
    (60, 30) -> Just 236
    (60, 31) -> Just 63
    (60, 40) -> Just 251
    (60, 41) -> Just 252
    (60, 66) -> Just 255
    (61, 8) -> Just 291
    (61, 30) -> Just 279
    (61, 31) -> Just 66
    (61, 35) -> Just 274
    (61, 36) -> Just 276
    (61, 40) -> Just 251
    (61, 41) -> Just 252
    (61, 66) -> Just 255
    (62, 8) -> Just 291
    (62, 30) -> Just 279
    (62, 31) -> Just 66
    (62, 35) -> Just 275
    (62, 36) -> Just 276
    (62, 40) -> Just 251
    (62, 41) -> Just 252
    (62, 66) -> Just 255
    (63, 8) -> Just 369
    (63, 44) -> Just 56
    (63, 52) -> Just 303
    (63, 66) -> Just 257
    (63, 67) -> Just 304
    (64, 8) -> Just 369
    (64, 31) -> Just 67
    (64, 38) -> Just 283
    (64, 39) -> Just 285
    (64, 66) -> Just 255
    (65, 8) -> Just 369
    (65, 31) -> Just 67
    (65, 38) -> Just 284
    (65, 39) -> Just 285
    (65, 66) -> Just 255
    (66, 8) -> Just 369
    (66, 44) -> Just 56
    (66, 52) -> Just 303
    (66, 66) -> Just 257
    (66, 67) -> Just 304
    (67, 8) -> Just 369
    (67, 44) -> Just 56
    (67, 52) -> Just 303
    (67, 66) -> Just 257
    (67, 67) -> Just 304
    (68, 8) -> Just 369
    (68, 31) -> Just 69
    (68, 66) -> Just 255
    (69, 8) -> Just 369
    (69, 44) -> Just 56
    (69, 52) -> Just 303
    (69, 66) -> Just 257
    (69, 67) -> Just 304
    (70, 8) -> Just 148
    (70, 9) -> Just 149
    (70, 11) -> Just 142
    (70, 12) -> Just 143
    (71, 8) -> Just 148
    (71, 9) -> Just 149
    (71, 11) -> Just 178
    (71, 12) -> Just 143
    (72, 8) -> Just 148
    (72, 9) -> Just 149
    (72, 11) -> Just 179
    (72, 12) -> Just 143
    (73, 8) -> Just 151
    (73, 9) -> Just 152
    (73, 10) -> Just 140
    (73, 13) -> Just 141
    (74, 8) -> Just 151
    (74, 9) -> Just 152
    (74, 10) -> Just 150
    (74, 13) -> Just 141
    (75, 8) -> Just 290
    (75, 40) -> Just 292
    (76, 8) -> Just 290
    (76, 40) -> Just 342
    (76, 53) -> Just 333
    (76, 54) -> Just 340
    (77, 8) -> Just 290
    (77, 40) -> Just 342
    (77, 53) -> Just 339
    (77, 54) -> Just 340
    (78, 8) -> Just 212
    (79, 8) -> Just 223
    (80, 8) -> Just 224
    (81, 8) -> Just 225
    (91, 9) -> Just 319
    (91, 45) -> Just 310
    (91, 46) -> Just 311
    (91, 47) -> Just 312
    (92, 9) -> Just 319
    (92, 17) -> Just 93
    (92, 45) -> Just 203
    (92, 46) -> Just 311
    (92, 47) -> Just 312
    (93, 9) -> Just 319
    (93, 23) -> Just 195
    (93, 45) -> Just 204
    (93, 46) -> Just 311
    (93, 47) -> Just 312
    (94, 9) -> Just 319
    (94, 17) -> Just 95
    (94, 45) -> Just 203
    (94, 46) -> Just 311
    (94, 47) -> Just 312
    (95, 9) -> Just 319
    (95, 24) -> Just 197
    (95, 45) -> Just 204
    (95, 46) -> Just 311
    (95, 47) -> Just 312
    (96, 9) -> Just 319
    (96, 17) -> Just 97
    (96, 45) -> Just 203
    (96, 46) -> Just 311
    (96, 47) -> Just 312
    (97, 9) -> Just 319
    (97, 23) -> Just 194
    (97, 45) -> Just 204
    (97, 46) -> Just 311
    (97, 47) -> Just 312
    (98, 9) -> Just 319
    (98, 17) -> Just 99
    (98, 45) -> Just 203
    (98, 46) -> Just 311
    (98, 47) -> Just 312
    (99, 9) -> Just 319
    (99, 24) -> Just 196
    (99, 45) -> Just 204
    (99, 46) -> Just 311
    (99, 47) -> Just 312
    (100, 9) -> Just 319
    (100, 17) -> Just 101
    (100, 18) -> Just 260
    (100, 45) -> Just 203
    (100, 46) -> Just 311
    (100, 47) -> Just 312
    (101, 9) -> Just 319
    (101, 45) -> Just 204
    (101, 46) -> Just 311
    (101, 47) -> Just 312
    (102, 9) -> Just 319
    (102, 17) -> Just 104
    (102, 18) -> Just 205
    (102, 45) -> Just 203
    (102, 46) -> Just 311
    (102, 47) -> Just 312
    (103, 9) -> Just 319
    (103, 17) -> Just 104
    (103, 18) -> Just 259
    (103, 45) -> Just 203
    (103, 46) -> Just 311
    (103, 47) -> Just 312
    (104, 9) -> Just 319
    (104, 45) -> Just 204
    (104, 46) -> Just 311
    (104, 47) -> Just 312
    (105, 9) -> Just 319
    (105, 17) -> Just 106
    (105, 45) -> Just 203
    (105, 46) -> Just 311
    (105, 47) -> Just 312
    (106, 9) -> Just 319
    (106, 19) -> Just 182
    (106, 45) -> Just 204
    (106, 46) -> Just 311
    (106, 47) -> Just 312
    (107, 9) -> Just 319
    (107, 17) -> Just 108
    (107, 45) -> Just 203
    (107, 46) -> Just 311
    (107, 47) -> Just 312
    (108, 9) -> Just 319
    (108, 19) -> Just 183
    (108, 45) -> Just 204
    (108, 46) -> Just 311
    (108, 47) -> Just 312
    (109, 9) -> Just 320
    (109, 17) -> Just 112
    (109, 45) -> Just 203
    (109, 46) -> Just 311
    (109, 47) -> Just 312
    (109, 50) -> Just 206
    (109, 51) -> Just 330
    (110, 9) -> Just 320
    (110, 17) -> Just 112
    (110, 45) -> Just 203
    (110, 46) -> Just 311
    (110, 47) -> Just 312
    (110, 50) -> Just 329
    (110, 51) -> Just 330
    (111, 9) -> Just 124
    (112, 9) -> Just 319
    (112, 45) -> Just 204
    (112, 46) -> Just 311
    (112, 47) -> Just 312
    (112, 52) -> Just 113
    (113, 9) -> Just 319
    (113, 17) -> Just 114
    (113, 45) -> Just 203
    (113, 46) -> Just 311
    (113, 47) -> Just 312
    (114, 9) -> Just 319
    (114, 45) -> Just 204
    (114, 46) -> Just 311
    (114, 47) -> Just 312
    (115, 9) -> Just 319
    (115, 17) -> Just 116
    (115, 18) -> Just 250
    (115, 45) -> Just 203
    (115, 46) -> Just 311
    (115, 47) -> Just 312
    (116, 9) -> Just 319
    (116, 45) -> Just 204
    (116, 46) -> Just 311
    (116, 47) -> Just 312
    (117, 9) -> Just 319
    (117, 17) -> Just 104
    (117, 18) -> Just 181
    (117, 45) -> Just 203
    (117, 46) -> Just 311
    (117, 47) -> Just 312
    (118, 9) -> Just 319
    (118, 17) -> Just 104
    (118, 18) -> Just 226
    (118, 45) -> Just 203
    (118, 46) -> Just 311
    (118, 47) -> Just 312
    (119, 9) -> Just 319
    (119, 17) -> Just 104
    (119, 18) -> Just 227
    (119, 45) -> Just 203
    (119, 46) -> Just 311
    (119, 47) -> Just 312
    (120, 9) -> Just 319
    (120, 17) -> Just 104
    (120, 18) -> Just 228
    (120, 45) -> Just 203
    (120, 46) -> Just 311
    (120, 47) -> Just 312
    (121, 9) -> Just 319
    (121, 17) -> Just 104
    (121, 18) -> Just 249
    (121, 45) -> Just 203
    (121, 46) -> Just 311
    (121, 47) -> Just 312
    (122, 9) -> Just 319
    (122, 17) -> Just 104
    (122, 18) -> Just 341
    (122, 45) -> Just 203
    (122, 46) -> Just 311
    (122, 47) -> Just 312
    (123, 9) -> Just 319
    (123, 17) -> Just 104
    (123, 18) -> Just 213
    (123, 45) -> Just 203
    (123, 46) -> Just 311
    (123, 47) -> Just 312
    (124, 9) -> Just 319
    (124, 45) -> Just 214
    (124, 46) -> Just 311
    (124, 47) -> Just 312
    (125, 9) -> Just 319
    (125, 17) -> Just 126
    (125, 45) -> Just 203
    (125, 46) -> Just 311
    (125, 47) -> Just 312
    (126, 9) -> Just 319
    (126, 22) -> Just 193
    (126, 45) -> Just 204
    (126, 46) -> Just 311
    (126, 47) -> Just 312
    (127, 9) -> Just 319
    (127, 17) -> Just 129
    (127, 45) -> Just 203
    (127, 46) -> Just 311
    (127, 47) -> Just 312
    (128, 9) -> Just 319
    (128, 17) -> Just 130
    (128, 45) -> Just 203
    (128, 46) -> Just 311
    (128, 47) -> Just 312
    (129, 9) -> Just 319
    (129, 45) -> Just 204
    (129, 46) -> Just 311
    (129, 47) -> Just 312
    (130, 9) -> Just 319
    (130, 22) -> Just 192
    (130, 45) -> Just 204
    (130, 46) -> Just 311
    (130, 47) -> Just 312
    (131, 9) -> Just 319
    (131, 17) -> Just 104
    (131, 18) -> Just 308
    (131, 45) -> Just 203
    (131, 46) -> Just 311
    (131, 47) -> Just 312
    (131, 48) -> Just 313
    (131, 49) -> Just 321
    (132, 9) -> Just 319
    (132, 17) -> Just 104
    (132, 18) -> Just 219
    (132, 25) -> Just 198
    (132, 45) -> Just 203
    (132, 46) -> Just 311
    (132, 47) -> Just 312
    (133, 9) -> Just 319
    (133, 17) -> Just 104
    (133, 18) -> Just 219
    (133, 25) -> Just 220
    (133, 45) -> Just 203
    (133, 46) -> Just 311
    (133, 47) -> Just 312
    (134, 9) -> Just 319
    (134, 17) -> Just 104
    (134, 18) -> Just 325
    (134, 45) -> Just 203
    (134, 46) -> Just 311
    (134, 47) -> Just 312
    (134, 48) -> Just 326
    (135, 9) -> Just 319
    (135, 17) -> Just 104
    (135, 18) -> Just 309
    (135, 45) -> Just 203
    (135, 46) -> Just 311
    (135, 47) -> Just 312
    (153, 20) -> Just 209
    (153, 21) -> Just 188
    (154, 20) -> Just 209
    (154, 21) -> Just 189
    (155, 20) -> Just 209
    (155, 21) -> Just 190
    (156, 20) -> Just 209
    (156, 21) -> Just 191
    (169, 15) -> Just 8
    (171, 20) -> Just 184
    (172, 20) -> Just 185
    (173, 20) -> Just 186
    (174, 20) -> Just 187
    (176, 26) -> Just 199
    (177, 16) -> Just 180
    (207, 20) -> Just 209
    (207, 21) -> Just 210
    (215, 34) -> Just 216
    (217, 37) -> Just 218
    (221, 55) -> Just 229
    (222, 55) -> Just 230
    (229, 56) -> Just 79
    (229, 57) -> Just 231
    (230, 58) -> Just 81
    (231, 56) -> Just 80
    (232, 28) -> Just 234
    (233, 28) -> Just 235
    (239, 28) -> Just 277
    (240, 28) -> Just 278
    (241, 28) -> Just 286
    (242, 28) -> Just 287
    (243, 28) -> Just 353
    (244, 28) -> Just 261
    (252, 42) -> Just 253
    (253, 43) -> Just 254
    (253, 44) -> Just 302
    (253, 52) -> Just 303
    (253, 67) -> Just 304
    (264, 62) -> Just 268
    (265, 62) -> Just 269
    (297, 43) -> Just 300
    (297, 44) -> Just 302
    (297, 52) -> Just 303
    (297, 67) -> Just 304
    (298, 43) -> Just 301
    (298, 44) -> Just 302
    (298, 52) -> Just 303
    (298, 67) -> Just 304
    (327, 49) -> Just 328
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
                  Token (INTEGER semanticValue) ->
                    StackValue_INTEGER semanticValue
                  Token (QVARSYM semanticValue) ->
                    StackValue_QVARSYM semanticValue
                  Token (QCONSYM semanticValue) ->
                    StackValue_QCONSYM semanticValue
                  Token (BACKQUOTE semanticValue) ->
                    StackValue_BACKQUOTE semanticValue
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
                      Monad.liftM StackValue_exp $ exp_implies_LAMBDA_pat_RARROW_exp actions (case snd (pop !! 3) of { StackValue_LAMBDA value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_exp $ exp_implies_LET_decls_IN_exp actions (case snd (pop !! 3) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_decls value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_IN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_exp $ exp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp actions (case snd (pop !! 7) of { StackValue_IF value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_THEN value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_semicolon_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ELSE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_MINUS_lexp actions (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_MINUS_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    192 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    194 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    198 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    199 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    200 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    201 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    202 ->
                      Monad.liftM StackValue_semicolon_opt $ semicolon_opt_implies actions
                    203 ->
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
  , exp_implies_LAMBDA_pat_RARROW_exp = \lAMBDA0 pat1 rARROW2 exp3 ->
      return $ Exp_implies_LAMBDA_pat_RARROW_exp lAMBDA0 pat1 rARROW2 exp3
  , exp_implies_LET_decls_IN_exp = \lET0 decls1 iN2 exp3 ->
      return $ Exp_implies_LET_decls_IN_exp lET0 decls1 iN2 exp3
  , exp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp = \iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 exp7 ->
      return $ Exp_implies_IF_exp_semicolon_opt_THEN_exp_semicolon_opt_ELSE_exp iF0 exp1 semicolon_opt2 tHEN3 exp4 semicolon_opt5 eLSE6 exp7
  , exp_implies_infixexp_COLON_COLON_type' = \infixexp0 cOLON_COLON1 type'2 ->
      return $ Exp_implies_infixexp_COLON_COLON_type' infixexp0 cOLON_COLON1 type'2
  , exp_implies_infixexp_COLON_COLON_btype_DARROW_type' = \infixexp0 cOLON_COLON1 btype2 dARROW3 type'4 ->
      return $ Exp_implies_infixexp_COLON_COLON_btype_DARROW_type' infixexp0 cOLON_COLON1 btype2 dARROW3 type'4
  , exp_implies_infixexp = \infixexp0 ->
      return $ Exp_implies_infixexp infixexp0
  , infixexp_implies_lexp = \lexp0 ->
      return $ Infixexp_implies_lexp lexp0
  , lexp_implies_MINUS_lexp = \mINUS0 lexp1 ->
      return $ Lexp_implies_MINUS_lexp mINUS0 lexp1
  , lexp_implies_fexp = \fexp0 ->
      return $ Lexp_implies_fexp fexp0
  , fexp_implies_aexp = \aexp0 ->
      return $ Fexp_implies_aexp aexp0
  , fexp_implies_fexp_aexp = \fexp0 aexp1 ->
      return $ Fexp_implies_fexp_aexp fexp0 aexp1
  , fexp_implies_fexp_MINUS_aexp = \fexp0 mINUS1 aexp2 ->
      return $ Fexp_implies_fexp_MINUS_aexp fexp0 mINUS1 aexp2
  , fexp_implies_fexp_op_aexp = \fexp0 op1 aexp2 ->
      return $ Fexp_implies_fexp_op_aexp fexp0 op1 aexp2
  , aexp_implies_var = \var0 ->
      return $ Aexp_implies_var var0
  , aexp_implies_INTEGER = \iNTEGER0 ->
      return $ Aexp_implies_INTEGER iNTEGER0
  , aexp_implies_STRING = \sTRING0 ->
      return $ Aexp_implies_STRING sTRING0
  , aexp_implies_LPAREN_exp_RPAREN = \lPAREN0 exp1 rPAREN2 ->
      return $ Aexp_implies_LPAREN_exp_RPAREN lPAREN0 exp1 rPAREN2
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

