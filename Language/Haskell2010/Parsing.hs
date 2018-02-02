module  Language.Haskell2010.Parsing  where
import qualified Control.Monad as Monad


type Pos = (Int, Int)
type RARROW = Pos
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
type EQUAL = Pos
type EXCL = Pos
type EXPORT = Pos
type FOREIGN = Pos
type HIDING = Pos
type IMPORT = Pos
type INFIX = Pos
type INFIXL = Pos
type INFIXR = Pos
type INSTANCE = Pos
type INTEGER = (Pos, Integer)
type LBRACE = Pos
type LBRACKET = Pos
type LPAREN = Pos
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
type RPAREN = Pos
type SEMICOLON = Pos
type STRING = (Pos, String)
type TODO_FDECL = Pos
type TODO_FUNLHS = Pos
type TODO_INST = Pos
type TODO_NEWCONSTR = Pos
type TOOD_RHS = Pos
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
  | EQUAL EQUAL
  | EXCL EXCL
  | EXPORT EXPORT
  | FOREIGN FOREIGN
  | HIDING HIDING
  | IMPORT IMPORT
  | INFIX INFIX
  | INFIXL INFIXL
  | INFIXR INFIXR
  | INSTANCE INSTANCE
  | INTEGER INTEGER
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | LPAREN LPAREN
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
  | Decl_implies_pat_rhs Pat Rhs
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
  deriving (Eq, Ord, Read, Show)

data Rhs =
    Rhs_implies_EQUAL_exp EQUAL Exp
  | Rhs_implies_EQUAL_exp_WHERE_decls EQUAL Exp WHERE Decls
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
  | Cdecl_implies_pat_rhs Pat Rhs
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
  | Idecl_implies_pat_rhs Pat Rhs
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
    Ops_implies_op Op
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

data Lhs =
    Lhs_implies_pat Pat
  deriving (Eq, Ord, Read, Show)

data Exp =
    Exp_implies_infixexp Infixexp
  deriving (Eq, Ord, Read, Show)

data Infixexp =
    Infixexp_implies_lexp Lexp
  deriving (Eq, Ord, Read, Show)

data Lexp =
    Lexp_implies_fexp Fexp
  deriving (Eq, Ord, Read, Show)

data Fexp =
    Fexp_implies_aexp Aexp
  | Fexp_implies_fexp_aexp Fexp Aexp
  | Fexp_implies_fexp_op_aexp Fexp Op Aexp
  deriving (Eq, Ord, Read, Show)

data Aexp =
    Aexp_implies_var Var
  | Aexp_implies_INTEGER INTEGER
  deriving (Eq, Ord, Read, Show)

data Apat =
    Apat_implies_var Var
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
  | StackValue_COLON_COLON COLON_COLON
  | StackValue_INFIXL INFIXL
  | StackValue_INFIXR INFIXR
  | StackValue_INFIX INFIX
  | StackValue_RARROW RARROW
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_EXCL EXCL
  | StackValue_PIPE PIPE
  | StackValue_QCONID QCONID
  | StackValue_EXPORT EXPORT
  | StackValue_AS AS
  | StackValue_QVARID QVARID
  | StackValue_STRING STRING
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
  | StackValue_rhs Rhs
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
  | StackValue_lhs Lhs
  | StackValue_exp Exp
  | StackValue_infixexp Infixexp
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
  , decl_implies_pat_rhs :: Pat -> Rhs -> m Decl
  , cdecls_opt_implies :: m Cdecls_opt
  , cdecls_opt_implies_WHERE_cdecls :: WHERE -> Cdecls -> m Cdecls_opt
  , cdecls_implies_LBRACE_cdecl_seq_RBRACE :: LBRACE -> Cdecl_seq -> RBRACE -> m Cdecls
  , cdecl_seq_implies_cdecl :: Cdecl -> m Cdecl_seq
  , cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq :: Cdecl -> SEMICOLON -> Cdecl_seq -> m Cdecl_seq
  , cdecl_implies_gendecl :: Gendecl -> m Cdecl
  , cdecl_implies_pat_rhs :: Pat -> Rhs -> m Cdecl
  , idecls_opt_implies :: m Idecls_opt
  , idecls_opt_implies_WHERE_idecls :: WHERE -> Idecls -> m Idecls_opt
  , idecls_implies_LBRACE_idecl_seq_RBRACE :: LBRACE -> Idecl_seq -> RBRACE -> m Idecls
  , idecl_seq_implies_idecl :: Idecl -> m Idecl_seq
  , idecl_seq_implies_idecl_SEMICOLON_idecl_seq :: Idecl -> SEMICOLON -> Idecl_seq -> m Idecl_seq
  , idecl_implies :: m Idecl
  , idecl_implies_pat_rhs :: Pat -> Rhs -> m Idecl
  , gendecl_implies :: m Gendecl
  , gendecl_implies_vars_COLON_COLON_type' :: Vars -> COLON_COLON -> Type' -> m Gendecl
  , gendecl_implies_vars_COLON_COLON_btype_DARROW_type' :: Vars -> COLON_COLON -> Btype -> DARROW -> Type' -> m Gendecl
  , gendecl_implies_fixity_integer_opt_ops :: Fixity -> Integer_opt -> Ops -> m Gendecl
  , ops_implies_op :: Op -> m Ops
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
  , lhs_implies_pat :: Pat -> m Lhs
  , rhs_implies_EQUAL_exp :: EQUAL -> Exp -> m Rhs
  , rhs_implies_EQUAL_exp_WHERE_decls :: EQUAL -> Exp -> WHERE -> Decls -> m Rhs
  , exp_implies_infixexp :: Infixexp -> m Exp
  , infixexp_implies_lexp :: Lexp -> m Infixexp
  , lexp_implies_fexp :: Fexp -> m Lexp
  , fexp_implies_aexp :: Aexp -> m Fexp
  , fexp_implies_fexp_aexp :: Fexp -> Aexp -> m Fexp
  , fexp_implies_fexp_op_aexp :: Fexp -> Op -> Aexp -> m Fexp
  , aexp_implies_var :: Var -> m Aexp
  , aexp_implies_INTEGER :: INTEGER -> m Aexp
  , pat_implies_apat :: Apat -> m Pat
  , pat_implies_pat_apat :: Pat -> Apat -> m Pat
  , apat_implies_var :: Var -> m Apat
  , var_implies_AS :: AS -> m Var
  , var_implies_EXPORT :: EXPORT -> m Var
  , var_implies_QVARID :: QVARID -> m Var
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
  , integer_opt_implies_INTEGER :: INTEGER -> m Integer_opt }

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
    (11, Token (MODULE _)) -> Just (Reduce 1 171)
    (11, Token (WHERE _)) -> Just (Reduce 1 171)
    (11, Token (RBRACE _)) -> Just (Reduce 1 171)
    (11, Token (LPAREN _)) -> Just (Reduce 1 171)
    (11, Token (RPAREN _)) -> Just (Reduce 1 171)
    (11, Token (COMMA _)) -> Just (Reduce 1 171)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 171)
    (11, Token (HIDING _)) -> Just (Reduce 1 171)
    (11, Token (QCONID _)) -> Just (Reduce 1 171)
    (11, Token (EXPORT _)) -> Just (Reduce 1 171)
    (11, Token (AS _)) -> Just (Reduce 1 171)
    (11, Token (QVARID _)) -> Just (Reduce 1 171)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 76)
    (13, Token (LPAREN _)) -> Just (Shift 58)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (13, Token (IMPORT _)) -> Just (Shift 140)
    (13, Token (TYPE _)) -> Just (Shift 98)
    (13, Token (DATA _)) -> Just (Shift 74)
    (13, Token (NEWTYPE _)) -> Just (Shift 96)
    (13, Token (CLASS _)) -> Just (Shift 66)
    (13, Token (INSTANCE _)) -> Just (Shift 68)
    (13, Token (DEFAULT _)) -> Just (Shift 146)
    (13, Token (FOREIGN _)) -> Just (Shift 147)
    (13, Token (INFIXL _)) -> Just (Shift 234)
    (13, Token (INFIXR _)) -> Just (Shift 235)
    (13, Token (INFIX _)) -> Just (Shift 236)
    (13, Token (EXPORT _)) -> Just (Shift 61)
    (13, Token (AS _)) -> Just (Shift 62)
    (13, Token (QVARID _)) -> Just (Shift 63)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 76)
    (16, Token (LPAREN _)) -> Just (Shift 58)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (16, Token (IMPORT _)) -> Just (Shift 140)
    (16, Token (TYPE _)) -> Just (Shift 98)
    (16, Token (DATA _)) -> Just (Shift 74)
    (16, Token (NEWTYPE _)) -> Just (Shift 96)
    (16, Token (CLASS _)) -> Just (Shift 66)
    (16, Token (INSTANCE _)) -> Just (Shift 68)
    (16, Token (DEFAULT _)) -> Just (Shift 146)
    (16, Token (FOREIGN _)) -> Just (Shift 147)
    (16, Token (INFIXL _)) -> Just (Shift 234)
    (16, Token (INFIXR _)) -> Just (Shift 235)
    (16, Token (INFIX _)) -> Just (Shift 236)
    (16, Token (EXPORT _)) -> Just (Shift 61)
    (16, Token (AS _)) -> Just (Shift 62)
    (16, Token (QVARID _)) -> Just (Shift 63)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 59)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 109)
    (19, Token (EXPORT _)) -> Just (Shift 61)
    (19, Token (AS _)) -> Just (Shift 62)
    (19, Token (QVARID _)) -> Just (Shift 63)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 59)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 109)
    (22, Token (EXPORT _)) -> Just (Shift 61)
    (22, Token (AS _)) -> Just (Shift 62)
    (22, Token (QVARID _)) -> Just (Shift 63)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 59)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 109)
    (25, Token (EXPORT _)) -> Just (Shift 61)
    (25, Token (AS _)) -> Just (Shift 62)
    (25, Token (QVARID _)) -> Just (Shift 63)
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
    (34, Token (LPAREN _)) -> Just (Shift 58)
    (34, Token (EXPORT _)) -> Just (Shift 61)
    (34, Token (AS _)) -> Just (Shift 62)
    (34, Token (QVARID _)) -> Just (Shift 63)
    (34, Token (INTEGER _)) -> Just (Shift 294)
    (35, Token (WHERE _)) -> Just (Reduce 1 140)
    (35, Token (RBRACE _)) -> Just (Reduce 1 140)
    (35, Token (LPAREN _)) -> Just (Shift 58)
    (35, Token (SEMICOLON _)) -> Just (Reduce 1 140)
    (35, Token (EXPORT _)) -> Just (Shift 61)
    (35, Token (AS _)) -> Just (Shift 62)
    (35, Token (QVARID _)) -> Just (Shift 63)
    (35, Token (INTEGER _)) -> Just (Shift 294)
    (35, Token (QVARSYM _)) -> Just (Shift 300)
    (35, Token (QCONSYM _)) -> Just (Shift 273)
    (35, Token (BACKQUOTE _)) -> Just (Shift 274)
    (36, Token (LPAREN _)) -> Just (Shift 58)
    (36, Token (EXPORT _)) -> Just (Shift 61)
    (36, Token (AS _)) -> Just (Shift 62)
    (36, Token (QVARID _)) -> Just (Shift 63)
    (36, Token (INTEGER _)) -> Just (Shift 294)
    (37, Token (RBRACE _)) -> Just (Reduce 0 76)
    (37, Token (LPAREN _)) -> Just (Shift 58)
    (37, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (37, Token (INFIXL _)) -> Just (Shift 234)
    (37, Token (INFIXR _)) -> Just (Shift 235)
    (37, Token (INFIX _)) -> Just (Shift 236)
    (37, Token (EXPORT _)) -> Just (Shift 61)
    (37, Token (AS _)) -> Just (Shift 62)
    (37, Token (QVARID _)) -> Just (Shift 63)
    (38, Token (RBRACE _)) -> Just (Reduce 0 76)
    (38, Token (LPAREN _)) -> Just (Shift 58)
    (38, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (38, Token (INFIXL _)) -> Just (Shift 234)
    (38, Token (INFIXR _)) -> Just (Shift 235)
    (38, Token (INFIX _)) -> Just (Shift 236)
    (38, Token (EXPORT _)) -> Just (Shift 61)
    (38, Token (AS _)) -> Just (Shift 62)
    (38, Token (QVARID _)) -> Just (Shift 63)
    (39, Token (RBRACE _)) -> Just (Reduce 0 76)
    (39, Token (LPAREN _)) -> Just (Shift 58)
    (39, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (39, Token (INFIXL _)) -> Just (Shift 234)
    (39, Token (INFIXR _)) -> Just (Shift 235)
    (39, Token (INFIX _)) -> Just (Shift 236)
    (39, Token (EXPORT _)) -> Just (Shift 61)
    (39, Token (AS _)) -> Just (Shift 62)
    (39, Token (QVARID _)) -> Just (Shift 63)
    (40, Token (RBRACE _)) -> Just (Reduce 0 76)
    (40, Token (LPAREN _)) -> Just (Shift 58)
    (40, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (40, Token (INFIXL _)) -> Just (Shift 234)
    (40, Token (INFIXR _)) -> Just (Shift 235)
    (40, Token (INFIX _)) -> Just (Shift 236)
    (40, Token (EXPORT _)) -> Just (Shift 61)
    (40, Token (AS _)) -> Just (Shift 62)
    (40, Token (QVARID _)) -> Just (Shift 63)
    (41, Token (LPAREN _)) -> Just (Shift 58)
    (41, Token (EQUAL _)) -> Just (Shift 34)
    (41, Token (EXPORT _)) -> Just (Shift 61)
    (41, Token (AS _)) -> Just (Shift 62)
    (41, Token (QVARID _)) -> Just (Shift 63)
    (42, Token (RBRACE _)) -> Just (Reduce 0 74)
    (42, Token (LPAREN _)) -> Just (Shift 58)
    (42, Token (SEMICOLON _)) -> Just (Reduce 0 74)
    (42, Token (EXPORT _)) -> Just (Shift 61)
    (42, Token (AS _)) -> Just (Shift 62)
    (42, Token (QVARID _)) -> Just (Shift 63)
    (43, Token (RBRACE _)) -> Just (Reduce 0 74)
    (43, Token (LPAREN _)) -> Just (Shift 58)
    (43, Token (SEMICOLON _)) -> Just (Reduce 0 74)
    (43, Token (EXPORT _)) -> Just (Shift 61)
    (43, Token (AS _)) -> Just (Shift 62)
    (43, Token (QVARID _)) -> Just (Shift 63)
    (44, Token (LPAREN _)) -> Just (Shift 58)
    (44, Token (EQUAL _)) -> Just (Shift 34)
    (44, Token (EXPORT _)) -> Just (Shift 61)
    (44, Token (AS _)) -> Just (Shift 62)
    (44, Token (QVARID _)) -> Just (Shift 63)
    (45, Token (LPAREN _)) -> Just (Shift 58)
    (45, Token (EQUAL _)) -> Just (Shift 34)
    (45, Token (EXPORT _)) -> Just (Shift 61)
    (45, Token (AS _)) -> Just (Shift 62)
    (45, Token (QVARID _)) -> Just (Shift 63)
    (46, Token (LPAREN _)) -> Just (Shift 59)
    (46, Token (RPAREN _)) -> Just (Reduce 0 15)
    (46, Token (QCONID _)) -> Just (Shift 109)
    (46, Token (EXPORT _)) -> Just (Shift 61)
    (46, Token (AS _)) -> Just (Shift 62)
    (46, Token (QVARID _)) -> Just (Shift 63)
    (47, Token (LPAREN _)) -> Just (Shift 59)
    (47, Token (RPAREN _)) -> Just (Reduce 0 15)
    (47, Token (QCONID _)) -> Just (Shift 109)
    (47, Token (EXPORT _)) -> Just (Shift 61)
    (47, Token (AS _)) -> Just (Shift 62)
    (47, Token (QVARID _)) -> Just (Shift 63)
    (48, Token (LPAREN _)) -> Just (Shift 59)
    (48, Token (RPAREN _)) -> Just (Reduce 0 15)
    (48, Token (QCONID _)) -> Just (Shift 109)
    (48, Token (EXPORT _)) -> Just (Shift 61)
    (48, Token (AS _)) -> Just (Shift 62)
    (48, Token (QVARID _)) -> Just (Shift 63)
    (49, Token (LPAREN _)) -> Just (Shift 59)
    (49, Token (QCONID _)) -> Just (Shift 109)
    (49, Token (EXPORT _)) -> Just (Shift 61)
    (49, Token (AS _)) -> Just (Shift 62)
    (49, Token (QVARID _)) -> Just (Shift 63)
    (50, Token (LPAREN _)) -> Just (Shift 59)
    (50, Token (RPAREN _)) -> Just (Shift 115)
    (50, Token (DOT_DOT _)) -> Just (Shift 118)
    (50, Token (QCONID _)) -> Just (Shift 109)
    (50, Token (EXPORT _)) -> Just (Shift 61)
    (50, Token (AS _)) -> Just (Shift 62)
    (50, Token (QVARID _)) -> Just (Shift 63)
    (51, Token (LPAREN _)) -> Just (Shift 58)
    (51, Token (EXPORT _)) -> Just (Shift 61)
    (51, Token (AS _)) -> Just (Shift 62)
    (51, Token (QVARID _)) -> Just (Shift 63)
    (52, Token (RBRACE _)) -> Just (Shift 269)
    (52, Token (LPAREN _)) -> Just (Shift 58)
    (52, Token (EXPORT _)) -> Just (Shift 61)
    (52, Token (AS _)) -> Just (Shift 62)
    (52, Token (QVARID _)) -> Just (Shift 63)
    (53, Token (LPAREN _)) -> Just (Shift 58)
    (53, Token (EXPORT _)) -> Just (Shift 61)
    (53, Token (AS _)) -> Just (Shift 62)
    (53, Token (QVARID _)) -> Just (Shift 63)
    (54, Token (LPAREN _)) -> Just (Shift 58)
    (54, Token (EXPORT _)) -> Just (Shift 61)
    (54, Token (AS _)) -> Just (Shift 62)
    (54, Token (QVARID _)) -> Just (Shift 63)
    (55, Token (LPAREN _)) -> Just (Shift 58)
    (55, Token (EXPORT _)) -> Just (Shift 61)
    (55, Token (AS _)) -> Just (Shift 62)
    (55, Token (QVARID _)) -> Just (Shift 63)
    (56, Token (LPAREN _)) -> Just (Shift 58)
    (56, Token (EXPORT _)) -> Just (Shift 61)
    (56, Token (AS _)) -> Just (Shift 62)
    (56, Token (QVARID _)) -> Just (Shift 63)
    (57, Token (LPAREN _)) -> Just (Shift 58)
    (57, Token (EXPORT _)) -> Just (Shift 61)
    (57, Token (AS _)) -> Just (Shift 62)
    (57, Token (QVARID _)) -> Just (Shift 63)
    (58, Token (QVARSYM _)) -> Just (Shift 64)
    (59, Token (QVARSYM _)) -> Just (Shift 64)
    (59, Token (QCONSYM _)) -> Just (Shift 110)
    (60, Token (WHERE _)) -> Just (Reduce 3 152)
    (60, Token (RBRACE _)) -> Just (Reduce 3 152)
    (60, Token (LPAREN _)) -> Just (Reduce 3 152)
    (60, Token (RPAREN _)) -> Just (Reduce 3 152)
    (60, Token (COMMA _)) -> Just (Reduce 3 152)
    (60, Token (SEMICOLON _)) -> Just (Reduce 3 152)
    (60, Token (EQUAL _)) -> Just (Reduce 3 152)
    (60, Token (COLON_COLON _)) -> Just (Reduce 3 152)
    (60, Token (QCONID _)) -> Just (Reduce 3 152)
    (60, Token (EXPORT _)) -> Just (Reduce 3 152)
    (60, Token (AS _)) -> Just (Reduce 3 152)
    (60, Token (QVARID _)) -> Just (Reduce 3 152)
    (60, Token (INTEGER _)) -> Just (Reduce 3 152)
    (60, Token (QVARSYM _)) -> Just (Reduce 3 152)
    (60, Token (QCONSYM _)) -> Just (Reduce 3 152)
    (60, Token (BACKQUOTE _)) -> Just (Reduce 3 152)
    (61, Token (WHERE _)) -> Just (Reduce 1 150)
    (61, Token (RBRACE _)) -> Just (Reduce 1 150)
    (61, Token (LPAREN _)) -> Just (Reduce 1 150)
    (61, Token (RPAREN _)) -> Just (Reduce 1 150)
    (61, Token (COMMA _)) -> Just (Reduce 1 150)
    (61, Token (SEMICOLON _)) -> Just (Reduce 1 150)
    (61, Token (EQUAL _)) -> Just (Reduce 1 150)
    (61, Token (COLON_COLON _)) -> Just (Reduce 1 150)
    (61, Token (QCONID _)) -> Just (Reduce 1 150)
    (61, Token (EXPORT _)) -> Just (Reduce 1 150)
    (61, Token (AS _)) -> Just (Reduce 1 150)
    (61, Token (QVARID _)) -> Just (Reduce 1 150)
    (61, Token (INTEGER _)) -> Just (Reduce 1 150)
    (61, Token (QVARSYM _)) -> Just (Reduce 1 150)
    (61, Token (QCONSYM _)) -> Just (Reduce 1 150)
    (61, Token (BACKQUOTE _)) -> Just (Reduce 1 150)
    (62, Token (WHERE _)) -> Just (Reduce 1 149)
    (62, Token (RBRACE _)) -> Just (Reduce 1 149)
    (62, Token (LPAREN _)) -> Just (Reduce 1 149)
    (62, Token (RPAREN _)) -> Just (Reduce 1 149)
    (62, Token (COMMA _)) -> Just (Reduce 1 149)
    (62, Token (SEMICOLON _)) -> Just (Reduce 1 149)
    (62, Token (EQUAL _)) -> Just (Reduce 1 149)
    (62, Token (COLON_COLON _)) -> Just (Reduce 1 149)
    (62, Token (QCONID _)) -> Just (Reduce 1 149)
    (62, Token (EXPORT _)) -> Just (Reduce 1 149)
    (62, Token (AS _)) -> Just (Reduce 1 149)
    (62, Token (QVARID _)) -> Just (Reduce 1 149)
    (62, Token (INTEGER _)) -> Just (Reduce 1 149)
    (62, Token (QVARSYM _)) -> Just (Reduce 1 149)
    (62, Token (QCONSYM _)) -> Just (Reduce 1 149)
    (62, Token (BACKQUOTE _)) -> Just (Reduce 1 149)
    (63, Token (WHERE _)) -> Just (Reduce 1 151)
    (63, Token (RBRACE _)) -> Just (Reduce 1 151)
    (63, Token (LPAREN _)) -> Just (Reduce 1 151)
    (63, Token (RPAREN _)) -> Just (Reduce 1 151)
    (63, Token (COMMA _)) -> Just (Reduce 1 151)
    (63, Token (SEMICOLON _)) -> Just (Reduce 1 151)
    (63, Token (EQUAL _)) -> Just (Reduce 1 151)
    (63, Token (COLON_COLON _)) -> Just (Reduce 1 151)
    (63, Token (QCONID _)) -> Just (Reduce 1 151)
    (63, Token (EXPORT _)) -> Just (Reduce 1 151)
    (63, Token (AS _)) -> Just (Reduce 1 151)
    (63, Token (QVARID _)) -> Just (Reduce 1 151)
    (63, Token (INTEGER _)) -> Just (Reduce 1 151)
    (63, Token (QVARSYM _)) -> Just (Reduce 1 151)
    (63, Token (QCONSYM _)) -> Just (Reduce 1 151)
    (63, Token (BACKQUOTE _)) -> Just (Reduce 1 151)
    (64, Token (RPAREN _)) -> Just (Shift 60)
    (65, Token (LPAREN _)) -> Just (Shift 102)
    (65, Token (LBRACKET _)) -> Just (Shift 106)
    (65, Token (EXCL _)) -> Just (Shift 65)
    (65, Token (QCONID _)) -> Just (Shift 109)
    (65, Token (EXPORT _)) -> Just (Shift 260)
    (65, Token (AS _)) -> Just (Shift 261)
    (65, Token (QVARID _)) -> Just (Shift 262)
    (66, Token (LPAREN _)) -> Just (Shift 102)
    (66, Token (LBRACKET _)) -> Just (Shift 106)
    (66, Token (EXCL _)) -> Just (Shift 65)
    (66, Token (QCONID _)) -> Just (Shift 109)
    (66, Token (EXPORT _)) -> Just (Shift 260)
    (66, Token (AS _)) -> Just (Shift 261)
    (66, Token (QVARID _)) -> Just (Shift 262)
    (67, Token (WHERE _)) -> Just (Shift 186)
    (67, Token (RBRACE _)) -> Just (Reduce 0 62)
    (67, Token (LPAREN _)) -> Just (Shift 102)
    (67, Token (SEMICOLON _)) -> Just (Reduce 0 62)
    (67, Token (DARROW _)) -> Just (Shift 70)
    (67, Token (LBRACKET _)) -> Just (Shift 106)
    (67, Token (EXCL _)) -> Just (Shift 65)
    (67, Token (QCONID _)) -> Just (Shift 109)
    (67, Token (EXPORT _)) -> Just (Shift 260)
    (67, Token (AS _)) -> Just (Shift 261)
    (67, Token (QVARID _)) -> Just (Shift 262)
    (68, Token (LPAREN _)) -> Just (Shift 102)
    (68, Token (LBRACKET _)) -> Just (Shift 106)
    (68, Token (EXCL _)) -> Just (Shift 65)
    (68, Token (QCONID _)) -> Just (Shift 109)
    (68, Token (EXPORT _)) -> Just (Shift 260)
    (68, Token (AS _)) -> Just (Shift 261)
    (68, Token (QVARID _)) -> Just (Shift 262)
    (69, Token (WHERE _)) -> Just (Shift 188)
    (69, Token (RBRACE _)) -> Just (Reduce 0 69)
    (69, Token (LPAREN _)) -> Just (Shift 102)
    (69, Token (SEMICOLON _)) -> Just (Reduce 0 69)
    (69, Token (DARROW _)) -> Just (Shift 72)
    (69, Token (LBRACKET _)) -> Just (Shift 106)
    (69, Token (EXCL _)) -> Just (Shift 65)
    (69, Token (QCONID _)) -> Just (Shift 109)
    (69, Token (EXPORT _)) -> Just (Shift 260)
    (69, Token (AS _)) -> Just (Shift 261)
    (69, Token (QVARID _)) -> Just (Shift 262)
    (70, Token (LPAREN _)) -> Just (Shift 102)
    (70, Token (LBRACKET _)) -> Just (Shift 106)
    (70, Token (EXCL _)) -> Just (Shift 65)
    (70, Token (QCONID _)) -> Just (Shift 109)
    (70, Token (EXPORT _)) -> Just (Shift 260)
    (70, Token (AS _)) -> Just (Shift 261)
    (70, Token (QVARID _)) -> Just (Shift 262)
    (71, Token (WHERE _)) -> Just (Shift 186)
    (71, Token (RBRACE _)) -> Just (Reduce 0 62)
    (71, Token (LPAREN _)) -> Just (Shift 102)
    (71, Token (SEMICOLON _)) -> Just (Reduce 0 62)
    (71, Token (LBRACKET _)) -> Just (Shift 106)
    (71, Token (EXCL _)) -> Just (Shift 65)
    (71, Token (QCONID _)) -> Just (Shift 109)
    (71, Token (EXPORT _)) -> Just (Shift 260)
    (71, Token (AS _)) -> Just (Shift 261)
    (71, Token (QVARID _)) -> Just (Shift 262)
    (72, Token (LPAREN _)) -> Just (Shift 102)
    (72, Token (LBRACKET _)) -> Just (Shift 106)
    (72, Token (EXCL _)) -> Just (Shift 65)
    (72, Token (QCONID _)) -> Just (Shift 109)
    (72, Token (EXPORT _)) -> Just (Shift 260)
    (72, Token (AS _)) -> Just (Shift 261)
    (72, Token (QVARID _)) -> Just (Shift 262)
    (73, Token (WHERE _)) -> Just (Shift 188)
    (73, Token (RBRACE _)) -> Just (Reduce 0 69)
    (73, Token (LPAREN _)) -> Just (Shift 102)
    (73, Token (SEMICOLON _)) -> Just (Reduce 0 69)
    (73, Token (LBRACKET _)) -> Just (Shift 106)
    (73, Token (EXCL _)) -> Just (Shift 65)
    (73, Token (QCONID _)) -> Just (Shift 109)
    (73, Token (EXPORT _)) -> Just (Shift 260)
    (73, Token (AS _)) -> Just (Shift 261)
    (73, Token (QVARID _)) -> Just (Shift 262)
    (74, Token (LPAREN _)) -> Just (Shift 102)
    (74, Token (LBRACKET _)) -> Just (Shift 106)
    (74, Token (EXCL _)) -> Just (Shift 65)
    (74, Token (QCONID _)) -> Just (Shift 109)
    (74, Token (EXPORT _)) -> Just (Shift 260)
    (74, Token (AS _)) -> Just (Shift 261)
    (74, Token (QVARID _)) -> Just (Shift 262)
    (75, Token (RBRACE _)) -> Just (Reduce 0 108)
    (75, Token (LPAREN _)) -> Just (Shift 102)
    (75, Token (SEMICOLON _)) -> Just (Reduce 0 108)
    (75, Token (EQUAL _)) -> Just (Shift 78)
    (75, Token (DERIVING _)) -> Just (Reduce 0 108)
    (75, Token (DARROW _)) -> Just (Shift 76)
    (75, Token (LBRACKET _)) -> Just (Shift 106)
    (75, Token (EXCL _)) -> Just (Shift 65)
    (75, Token (QCONID _)) -> Just (Shift 109)
    (75, Token (EXPORT _)) -> Just (Shift 260)
    (75, Token (AS _)) -> Just (Shift 261)
    (75, Token (QVARID _)) -> Just (Shift 262)
    (76, Token (LPAREN _)) -> Just (Shift 102)
    (76, Token (LBRACKET _)) -> Just (Shift 106)
    (76, Token (EXCL _)) -> Just (Shift 65)
    (76, Token (QCONID _)) -> Just (Shift 109)
    (76, Token (EXPORT _)) -> Just (Shift 260)
    (76, Token (AS _)) -> Just (Shift 261)
    (76, Token (QVARID _)) -> Just (Shift 262)
    (77, Token (RBRACE _)) -> Just (Reduce 0 108)
    (77, Token (LPAREN _)) -> Just (Shift 102)
    (77, Token (SEMICOLON _)) -> Just (Reduce 0 108)
    (77, Token (EQUAL _)) -> Just (Shift 78)
    (77, Token (DERIVING _)) -> Just (Reduce 0 108)
    (77, Token (LBRACKET _)) -> Just (Shift 106)
    (77, Token (EXCL _)) -> Just (Shift 65)
    (77, Token (QCONID _)) -> Just (Shift 109)
    (77, Token (EXPORT _)) -> Just (Shift 260)
    (77, Token (AS _)) -> Just (Shift 261)
    (77, Token (QVARID _)) -> Just (Shift 262)
    (78, Token (LPAREN _)) -> Just (Shift 102)
    (78, Token (LBRACKET _)) -> Just (Shift 106)
    (78, Token (EXCL _)) -> Just (Shift 65)
    (78, Token (QCONID _)) -> Just (Shift 109)
    (78, Token (EXPORT _)) -> Just (Shift 260)
    (78, Token (AS _)) -> Just (Shift 261)
    (78, Token (QVARID _)) -> Just (Shift 262)
    (79, Token (LPAREN _)) -> Just (Shift 102)
    (79, Token (LBRACKET _)) -> Just (Shift 106)
    (79, Token (EXCL _)) -> Just (Shift 65)
    (79, Token (QCONID _)) -> Just (Shift 109)
    (79, Token (EXPORT _)) -> Just (Shift 260)
    (79, Token (AS _)) -> Just (Shift 261)
    (79, Token (QVARID _)) -> Just (Shift 262)
    (80, Token (LPAREN _)) -> Just (Shift 107)
    (80, Token (QCONID _)) -> Just (Shift 109)
    (81, Token (RBRACE _)) -> Just (Reduce 1 112)
    (81, Token (LPAREN _)) -> Just (Shift 102)
    (81, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (81, Token (DERIVING _)) -> Just (Reduce 1 112)
    (81, Token (LBRACKET _)) -> Just (Shift 106)
    (81, Token (EXCL _)) -> Just (Shift 65)
    (81, Token (PIPE _)) -> Just (Reduce 1 112)
    (81, Token (QCONID _)) -> Just (Shift 109)
    (81, Token (EXPORT _)) -> Just (Shift 260)
    (81, Token (AS _)) -> Just (Shift 261)
    (81, Token (QVARID _)) -> Just (Shift 262)
    (81, Token (QCONSYM _)) -> Just (Shift 273)
    (81, Token (BACKQUOTE _)) -> Just (Shift 275)
    (82, Token (LPAREN _)) -> Just (Shift 102)
    (82, Token (LBRACKET _)) -> Just (Shift 106)
    (82, Token (EXCL _)) -> Just (Shift 65)
    (82, Token (QCONID _)) -> Just (Shift 109)
    (82, Token (EXPORT _)) -> Just (Shift 260)
    (82, Token (AS _)) -> Just (Shift 261)
    (82, Token (QVARID _)) -> Just (Shift 262)
    (83, Token (RBRACE _)) -> Just (Reduce 3 113)
    (83, Token (LPAREN _)) -> Just (Shift 102)
    (83, Token (SEMICOLON _)) -> Just (Reduce 3 113)
    (83, Token (DERIVING _)) -> Just (Reduce 3 113)
    (83, Token (LBRACKET _)) -> Just (Shift 106)
    (83, Token (EXCL _)) -> Just (Shift 65)
    (83, Token (PIPE _)) -> Just (Reduce 3 113)
    (83, Token (QCONID _)) -> Just (Shift 109)
    (83, Token (EXPORT _)) -> Just (Shift 260)
    (83, Token (AS _)) -> Just (Shift 261)
    (83, Token (QVARID _)) -> Just (Shift 262)
    (84, Token (LPAREN _)) -> Just (Shift 102)
    (84, Token (LBRACKET _)) -> Just (Shift 106)
    (84, Token (EXCL _)) -> Just (Shift 65)
    (84, Token (QCONID _)) -> Just (Shift 109)
    (84, Token (EXPORT _)) -> Just (Shift 260)
    (84, Token (AS _)) -> Just (Shift 261)
    (84, Token (QVARID _)) -> Just (Shift 262)
    (85, Token (RBRACE _)) -> Just (Reduce 1 89)
    (85, Token (LPAREN _)) -> Just (Shift 102)
    (85, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (85, Token (DARROW _)) -> Just (Shift 91)
    (85, Token (RARROW _)) -> Just (Shift 87)
    (85, Token (LBRACKET _)) -> Just (Shift 106)
    (85, Token (EXCL _)) -> Just (Shift 65)
    (85, Token (QCONID _)) -> Just (Shift 109)
    (85, Token (EXPORT _)) -> Just (Shift 260)
    (85, Token (AS _)) -> Just (Shift 261)
    (85, Token (QVARID _)) -> Just (Shift 262)
    (86, Token (LPAREN _)) -> Just (Shift 102)
    (86, Token (LBRACKET _)) -> Just (Shift 106)
    (86, Token (EXCL _)) -> Just (Shift 65)
    (86, Token (QCONID _)) -> Just (Shift 109)
    (86, Token (EXPORT _)) -> Just (Shift 260)
    (86, Token (AS _)) -> Just (Shift 261)
    (86, Token (QVARID _)) -> Just (Shift 262)
    (87, Token (LPAREN _)) -> Just (Shift 102)
    (87, Token (LBRACKET _)) -> Just (Shift 106)
    (87, Token (EXCL _)) -> Just (Shift 65)
    (87, Token (QCONID _)) -> Just (Shift 109)
    (87, Token (EXPORT _)) -> Just (Shift 260)
    (87, Token (AS _)) -> Just (Shift 261)
    (87, Token (QVARID _)) -> Just (Shift 262)
    (88, Token (LPAREN _)) -> Just (Shift 102)
    (88, Token (LBRACKET _)) -> Just (Shift 106)
    (88, Token (EXCL _)) -> Just (Shift 65)
    (88, Token (QCONID _)) -> Just (Shift 109)
    (88, Token (EXPORT _)) -> Just (Shift 260)
    (88, Token (AS _)) -> Just (Shift 261)
    (88, Token (QVARID _)) -> Just (Shift 262)
    (89, Token (LPAREN _)) -> Just (Shift 102)
    (89, Token (LBRACKET _)) -> Just (Shift 106)
    (89, Token (EXCL _)) -> Just (Shift 65)
    (89, Token (QCONID _)) -> Just (Shift 109)
    (89, Token (EXPORT _)) -> Just (Shift 260)
    (89, Token (AS _)) -> Just (Shift 261)
    (89, Token (QVARID _)) -> Just (Shift 262)
    (90, Token (LPAREN _)) -> Just (Shift 102)
    (90, Token (LBRACKET _)) -> Just (Shift 106)
    (90, Token (EXCL _)) -> Just (Shift 65)
    (90, Token (QCONID _)) -> Just (Shift 109)
    (90, Token (EXPORT _)) -> Just (Shift 260)
    (90, Token (AS _)) -> Just (Shift 261)
    (90, Token (QVARID _)) -> Just (Shift 262)
    (91, Token (LPAREN _)) -> Just (Shift 102)
    (91, Token (LBRACKET _)) -> Just (Shift 106)
    (91, Token (EXCL _)) -> Just (Shift 65)
    (91, Token (QCONID _)) -> Just (Shift 109)
    (91, Token (EXPORT _)) -> Just (Shift 260)
    (91, Token (AS _)) -> Just (Shift 261)
    (91, Token (QVARID _)) -> Just (Shift 262)
    (92, Token (RBRACE _)) -> Just (Reduce 1 89)
    (92, Token (LPAREN _)) -> Just (Shift 102)
    (92, Token (RPAREN _)) -> Just (Reduce 1 89)
    (92, Token (COMMA _)) -> Just (Reduce 1 89)
    (92, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (92, Token (RARROW _)) -> Just (Shift 87)
    (92, Token (LBRACKET _)) -> Just (Shift 106)
    (92, Token (RBRACKET _)) -> Just (Reduce 1 89)
    (92, Token (EXCL _)) -> Just (Shift 65)
    (92, Token (QCONID _)) -> Just (Shift 109)
    (92, Token (EXPORT _)) -> Just (Shift 260)
    (92, Token (AS _)) -> Just (Shift 261)
    (92, Token (QVARID _)) -> Just (Shift 262)
    (93, Token (LPAREN _)) -> Just (Shift 102)
    (93, Token (LBRACKET _)) -> Just (Shift 106)
    (93, Token (EXCL _)) -> Just (Shift 65)
    (93, Token (QCONID _)) -> Just (Shift 109)
    (93, Token (EXPORT _)) -> Just (Shift 260)
    (93, Token (AS _)) -> Just (Shift 261)
    (93, Token (QVARID _)) -> Just (Shift 262)
    (94, Token (LPAREN _)) -> Just (Shift 102)
    (94, Token (LBRACKET _)) -> Just (Shift 106)
    (94, Token (EXCL _)) -> Just (Shift 65)
    (94, Token (QCONID _)) -> Just (Shift 109)
    (94, Token (EXPORT _)) -> Just (Shift 260)
    (94, Token (AS _)) -> Just (Shift 261)
    (94, Token (QVARID _)) -> Just (Shift 262)
    (95, Token (LBRACE _)) -> Just (Shift 54)
    (95, Token (LPAREN _)) -> Just (Shift 102)
    (95, Token (LBRACKET _)) -> Just (Shift 106)
    (95, Token (EXCL _)) -> Just (Shift 65)
    (95, Token (QCONID _)) -> Just (Shift 109)
    (95, Token (EXPORT _)) -> Just (Shift 260)
    (95, Token (AS _)) -> Just (Shift 261)
    (95, Token (QVARID _)) -> Just (Shift 262)
    (96, Token (LPAREN _)) -> Just (Shift 102)
    (96, Token (LBRACKET _)) -> Just (Shift 106)
    (96, Token (EXCL _)) -> Just (Shift 65)
    (96, Token (QCONID _)) -> Just (Shift 109)
    (96, Token (EXPORT _)) -> Just (Shift 260)
    (96, Token (AS _)) -> Just (Shift 261)
    (96, Token (QVARID _)) -> Just (Shift 262)
    (97, Token (LPAREN _)) -> Just (Shift 102)
    (97, Token (EQUAL _)) -> Just (Shift 80)
    (97, Token (DARROW _)) -> Just (Shift 99)
    (97, Token (LBRACKET _)) -> Just (Shift 106)
    (97, Token (EXCL _)) -> Just (Shift 65)
    (97, Token (QCONID _)) -> Just (Shift 109)
    (97, Token (EXPORT _)) -> Just (Shift 260)
    (97, Token (AS _)) -> Just (Shift 261)
    (97, Token (QVARID _)) -> Just (Shift 262)
    (98, Token (LPAREN _)) -> Just (Shift 102)
    (98, Token (LBRACKET _)) -> Just (Shift 106)
    (98, Token (EXCL _)) -> Just (Shift 65)
    (98, Token (QCONID _)) -> Just (Shift 109)
    (98, Token (EXPORT _)) -> Just (Shift 260)
    (98, Token (AS _)) -> Just (Shift 261)
    (98, Token (QVARID _)) -> Just (Shift 262)
    (99, Token (LPAREN _)) -> Just (Shift 102)
    (99, Token (LBRACKET _)) -> Just (Shift 106)
    (99, Token (EXCL _)) -> Just (Shift 65)
    (99, Token (QCONID _)) -> Just (Shift 109)
    (99, Token (EXPORT _)) -> Just (Shift 260)
    (99, Token (AS _)) -> Just (Shift 261)
    (99, Token (QVARID _)) -> Just (Shift 262)
    (100, Token (LPAREN _)) -> Just (Shift 102)
    (100, Token (EQUAL _)) -> Just (Shift 86)
    (100, Token (LBRACKET _)) -> Just (Shift 106)
    (100, Token (EXCL _)) -> Just (Shift 65)
    (100, Token (QCONID _)) -> Just (Shift 109)
    (100, Token (EXPORT _)) -> Just (Shift 260)
    (100, Token (AS _)) -> Just (Shift 261)
    (100, Token (QVARID _)) -> Just (Shift 262)
    (101, Token (LPAREN _)) -> Just (Shift 102)
    (101, Token (EQUAL _)) -> Just (Shift 80)
    (101, Token (LBRACKET _)) -> Just (Shift 106)
    (101, Token (EXCL _)) -> Just (Shift 65)
    (101, Token (QCONID _)) -> Just (Shift 109)
    (101, Token (EXPORT _)) -> Just (Shift 260)
    (101, Token (AS _)) -> Just (Shift 261)
    (101, Token (QVARID _)) -> Just (Shift 262)
    (102, Token (LPAREN _)) -> Just (Shift 102)
    (102, Token (RPAREN _)) -> Just (Shift 252)
    (102, Token (COMMA _)) -> Just (Shift 265)
    (102, Token (RARROW _)) -> Just (Shift 255)
    (102, Token (LBRACKET _)) -> Just (Shift 106)
    (102, Token (EXCL _)) -> Just (Shift 65)
    (102, Token (QCONID _)) -> Just (Shift 109)
    (102, Token (EXPORT _)) -> Just (Shift 260)
    (102, Token (AS _)) -> Just (Shift 261)
    (102, Token (QVARID _)) -> Just (Shift 262)
    (102, Token (QCONSYM _)) -> Just (Shift 110)
    (103, Token (LPAREN _)) -> Just (Shift 102)
    (103, Token (RPAREN _)) -> Just (Shift 132)
    (103, Token (LBRACKET _)) -> Just (Shift 106)
    (103, Token (EXCL _)) -> Just (Shift 65)
    (103, Token (QCONID _)) -> Just (Shift 109)
    (103, Token (EXPORT _)) -> Just (Shift 260)
    (103, Token (AS _)) -> Just (Shift 261)
    (103, Token (QVARID _)) -> Just (Shift 262)
    (104, Token (LPAREN _)) -> Just (Shift 102)
    (104, Token (LBRACKET _)) -> Just (Shift 106)
    (104, Token (EXCL _)) -> Just (Shift 65)
    (104, Token (QCONID _)) -> Just (Shift 109)
    (104, Token (EXPORT _)) -> Just (Shift 260)
    (104, Token (AS _)) -> Just (Shift 261)
    (104, Token (QVARID _)) -> Just (Shift 262)
    (105, Token (LPAREN _)) -> Just (Shift 102)
    (105, Token (LBRACKET _)) -> Just (Shift 106)
    (105, Token (EXCL _)) -> Just (Shift 65)
    (105, Token (QCONID _)) -> Just (Shift 109)
    (105, Token (EXPORT _)) -> Just (Shift 260)
    (105, Token (AS _)) -> Just (Shift 261)
    (105, Token (QVARID _)) -> Just (Shift 262)
    (106, Token (LPAREN _)) -> Just (Shift 102)
    (106, Token (LBRACKET _)) -> Just (Shift 106)
    (106, Token (RBRACKET _)) -> Just (Shift 256)
    (106, Token (EXCL _)) -> Just (Shift 65)
    (106, Token (QCONID _)) -> Just (Shift 109)
    (106, Token (EXPORT _)) -> Just (Shift 260)
    (106, Token (AS _)) -> Just (Shift 261)
    (106, Token (QVARID _)) -> Just (Shift 262)
    (107, Token (QCONSYM _)) -> Just (Shift 110)
    (108, Token (WHERE _)) -> Just (Reduce 3 154)
    (108, Token (LBRACE _)) -> Just (Reduce 3 154)
    (108, Token (RBRACE _)) -> Just (Reduce 3 154)
    (108, Token (LPAREN _)) -> Just (Reduce 3 154)
    (108, Token (RPAREN _)) -> Just (Reduce 3 154)
    (108, Token (COMMA _)) -> Just (Reduce 3 154)
    (108, Token (SEMICOLON _)) -> Just (Reduce 3 154)
    (108, Token (EQUAL _)) -> Just (Reduce 3 154)
    (108, Token (DERIVING _)) -> Just (Reduce 3 154)
    (108, Token (DARROW _)) -> Just (Reduce 3 154)
    (108, Token (COLON_COLON _)) -> Just (Reduce 3 154)
    (108, Token (INFIXL _)) -> Just (Reduce 3 154)
    (108, Token (INFIXR _)) -> Just (Reduce 3 154)
    (108, Token (INFIX _)) -> Just (Reduce 3 154)
    (108, Token (RARROW _)) -> Just (Reduce 3 154)
    (108, Token (LBRACKET _)) -> Just (Reduce 3 154)
    (108, Token (RBRACKET _)) -> Just (Reduce 3 154)
    (108, Token (EXCL _)) -> Just (Reduce 3 154)
    (108, Token (PIPE _)) -> Just (Reduce 3 154)
    (108, Token (QCONID _)) -> Just (Reduce 3 154)
    (108, Token (EXPORT _)) -> Just (Reduce 3 154)
    (108, Token (AS _)) -> Just (Reduce 3 154)
    (108, Token (QVARID _)) -> Just (Reduce 3 154)
    (108, Token (INTEGER _)) -> Just (Reduce 3 154)
    (108, Token (QVARSYM _)) -> Just (Reduce 3 154)
    (108, Token (QCONSYM _)) -> Just (Reduce 3 154)
    (108, Token (BACKQUOTE _)) -> Just (Reduce 3 154)
    (109, Token (WHERE _)) -> Just (Reduce 1 153)
    (109, Token (LBRACE _)) -> Just (Reduce 1 153)
    (109, Token (RBRACE _)) -> Just (Reduce 1 153)
    (109, Token (LPAREN _)) -> Just (Reduce 1 153)
    (109, Token (RPAREN _)) -> Just (Reduce 1 153)
    (109, Token (COMMA _)) -> Just (Reduce 1 153)
    (109, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (109, Token (EQUAL _)) -> Just (Reduce 1 153)
    (109, Token (DERIVING _)) -> Just (Reduce 1 153)
    (109, Token (DARROW _)) -> Just (Reduce 1 153)
    (109, Token (COLON_COLON _)) -> Just (Reduce 1 153)
    (109, Token (INFIXL _)) -> Just (Reduce 1 153)
    (109, Token (INFIXR _)) -> Just (Reduce 1 153)
    (109, Token (INFIX _)) -> Just (Reduce 1 153)
    (109, Token (RARROW _)) -> Just (Reduce 1 153)
    (109, Token (LBRACKET _)) -> Just (Reduce 1 153)
    (109, Token (RBRACKET _)) -> Just (Reduce 1 153)
    (109, Token (EXCL _)) -> Just (Reduce 1 153)
    (109, Token (PIPE _)) -> Just (Reduce 1 153)
    (109, Token (QCONID _)) -> Just (Reduce 1 153)
    (109, Token (EXPORT _)) -> Just (Reduce 1 153)
    (109, Token (AS _)) -> Just (Reduce 1 153)
    (109, Token (QVARID _)) -> Just (Reduce 1 153)
    (109, Token (INTEGER _)) -> Just (Reduce 1 153)
    (109, Token (QVARSYM _)) -> Just (Reduce 1 153)
    (109, Token (QCONSYM _)) -> Just (Reduce 1 153)
    (109, Token (BACKQUOTE _)) -> Just (Reduce 1 153)
    (110, Token (RPAREN _)) -> Just (Shift 108)
    (111, Token (RPAREN _)) -> Just (Reduce 3 24)
    (112, Token (RPAREN _)) -> Just (Reduce 1 23)
    (112, Token (COMMA _)) -> Just (Shift 49)
    (113, Token (RPAREN _)) -> Just (Reduce 3 17)
    (114, Token (RPAREN _)) -> Just (Reduce 1 16)
    (114, Token (COMMA _)) -> Just (Shift 46)
    (115, Token (RPAREN _)) -> Just (Reduce 3 20)
    (115, Token (COMMA _)) -> Just (Reduce 3 20)
    (116, Token (RPAREN _)) -> Just (Reduce 4 21)
    (116, Token (COMMA _)) -> Just (Reduce 4 21)
    (117, Token (RPAREN _)) -> Just (Reduce 4 22)
    (117, Token (COMMA _)) -> Just (Reduce 4 22)
    (118, Token (RPAREN _)) -> Just (Shift 116)
    (119, Token (RPAREN _)) -> Just (Reduce 1 18)
    (119, Token (COMMA _)) -> Just (Reduce 1 18)
    (120, Token (LPAREN _)) -> Just (Shift 50)
    (120, Token (RPAREN _)) -> Just (Reduce 1 19)
    (120, Token (COMMA _)) -> Just (Reduce 1 19)
    (121, Token (RPAREN _)) -> Just (Shift 117)
    (122, Token (RPAREN _)) -> Just (Reduce 1 25)
    (122, Token (COMMA _)) -> Just (Reduce 1 25)
    (123, Token (RPAREN _)) -> Just (Reduce 1 26)
    (123, Token (COMMA _)) -> Just (Reduce 1 26)
    (124, Token (RPAREN _)) -> Just (Shift 128)
    (124, Token (QCONID _)) -> Just (Shift 179)
    (125, Token (RPAREN _)) -> Just (Shift 129)
    (125, Token (QCONID _)) -> Just (Shift 179)
    (126, Token (RPAREN _)) -> Just (Shift 130)
    (126, Token (QCONID _)) -> Just (Shift 179)
    (127, Token (RPAREN _)) -> Just (Shift 131)
    (127, Token (QCONID _)) -> Just (Shift 179)
    (128, Token (RBRACE _)) -> Just (Reduce 6 35)
    (128, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (129, Token (RBRACE _)) -> Just (Reduce 8 39)
    (129, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (130, Token (RBRACE _)) -> Just (Reduce 8 47)
    (130, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (131, Token (RBRACE _)) -> Just (Reduce 6 43)
    (131, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (132, Token (RBRACE _)) -> Just (Reduce 3 53)
    (132, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (133, Token (RBRACE _)) -> Just (Reduce 8 31)
    (133, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (134, Token (RBRACE _)) -> Just (Reduce 7 30)
    (134, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (135, Token (RBRACE _)) -> Just (Reduce 7 36)
    (135, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (136, Token (RBRACE _)) -> Just (Reduce 9 40)
    (136, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (137, Token (RBRACE _)) -> Just (Reduce 9 48)
    (137, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (138, Token (RBRACE _)) -> Just (Reduce 7 44)
    (138, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (139, Token (RBRACE _)) -> Just (Reduce 4 54)
    (139, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (140, Token (QCONID _)) -> Just (Reduce 0 165)
    (140, Token (QUALIFIED _)) -> Just (Shift 172)
    (141, Token (LPAREN _)) -> Just (Shift 47)
    (142, Token (LPAREN _)) -> Just (Shift 124)
    (142, Token (QCONID _)) -> Just (Shift 179)
    (143, Token (LPAREN _)) -> Just (Shift 125)
    (143, Token (QCONID _)) -> Just (Shift 179)
    (144, Token (LPAREN _)) -> Just (Shift 126)
    (144, Token (QCONID _)) -> Just (Shift 179)
    (145, Token (LPAREN _)) -> Just (Shift 127)
    (145, Token (QCONID _)) -> Just (Shift 179)
    (146, Token (LPAREN _)) -> Just (Shift 103)
    (147, Token (IMPORT _)) -> Just (Shift 192)
    (147, Token (EXPORT _)) -> Just (Shift 193)
    (148, Token (RBRACE _)) -> Just (Reduce 0 163)
    (148, Token (LPAREN _)) -> Just (Reduce 0 163)
    (148, Token (SEMICOLON _)) -> Just (Reduce 0 163)
    (148, Token (HIDING _)) -> Just (Reduce 0 163)
    (148, Token (AS _)) -> Just (Shift 9)
    (149, Token (RPAREN _)) -> Just (Shift 133)
    (150, Token (RPAREN _)) -> Just (Shift 134)
    (151, Token (RBRACE _)) -> Just (Reduce 4 29)
    (151, Token (LPAREN _)) -> Just (Shift 48)
    (151, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (151, Token (HIDING _)) -> Just (Shift 141)
    (152, Token (RBRACE _)) -> Just (Reduce 4 32)
    (152, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (153, Token (RBRACE _)) -> Just (Reduce 3 33)
    (153, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (153, Token (DERIVING _)) -> Just (Shift 142)
    (154, Token (RBRACE _)) -> Just (Reduce 5 37)
    (154, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (154, Token (DERIVING _)) -> Just (Shift 143)
    (155, Token (RBRACE _)) -> Just (Reduce 5 34)
    (155, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (156, Token (RBRACE _)) -> Just (Reduce 7 38)
    (156, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (157, Token (RBRACE _)) -> Just (Reduce 7 46)
    (157, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (158, Token (RBRACE _)) -> Just (Reduce 5 42)
    (158, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (159, Token (RPAREN _)) -> Just (Shift 135)
    (160, Token (RPAREN _)) -> Just (Shift 136)
    (161, Token (RPAREN _)) -> Just (Shift 137)
    (162, Token (RPAREN _)) -> Just (Shift 138)
    (163, Token (RBRACE _)) -> Just (Reduce 5 45)
    (163, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (163, Token (DERIVING _)) -> Just (Shift 144)
    (164, Token (RBRACE _)) -> Just (Reduce 3 41)
    (164, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (164, Token (DERIVING _)) -> Just (Shift 145)
    (165, Token (RBRACE _)) -> Just (Reduce 5 50)
    (165, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (166, Token (RBRACE _)) -> Just (Reduce 3 49)
    (166, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (167, Token (RBRACE _)) -> Just (Reduce 5 52)
    (167, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (168, Token (RBRACE _)) -> Just (Reduce 3 51)
    (168, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (169, Token (RPAREN _)) -> Just (Shift 139)
    (170, Token (RBRACE _)) -> Just (Reduce 2 55)
    (170, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (171, Token (RBRACE _)) -> Just (Reduce 1 56)
    (171, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (172, Token (QCONID _)) -> Just (Reduce 1 166)
    (173, Token (RBRACE _)) -> Just (Reduce 2 164)
    (173, Token (LPAREN _)) -> Just (Reduce 2 164)
    (173, Token (SEMICOLON _)) -> Just (Reduce 2 164)
    (173, Token (HIDING _)) -> Just (Reduce 2 164)
    (174, Token (WHERE _)) -> Just (Reduce 1 91)
    (174, Token (LBRACE _)) -> Just (Reduce 1 91)
    (174, Token (RBRACE _)) -> Just (Reduce 1 91)
    (174, Token (LPAREN _)) -> Just (Reduce 1 91)
    (174, Token (RPAREN _)) -> Just (Reduce 1 91)
    (174, Token (COMMA _)) -> Just (Reduce 1 91)
    (174, Token (SEMICOLON _)) -> Just (Reduce 1 91)
    (174, Token (EQUAL _)) -> Just (Reduce 1 91)
    (174, Token (DERIVING _)) -> Just (Reduce 1 91)
    (174, Token (DARROW _)) -> Just (Reduce 1 91)
    (174, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (174, Token (INFIXL _)) -> Just (Reduce 1 91)
    (174, Token (INFIXR _)) -> Just (Reduce 1 91)
    (174, Token (INFIX _)) -> Just (Reduce 1 91)
    (174, Token (RARROW _)) -> Just (Reduce 1 91)
    (174, Token (LBRACKET _)) -> Just (Reduce 1 91)
    (174, Token (RBRACKET _)) -> Just (Reduce 1 91)
    (174, Token (EXCL _)) -> Just (Reduce 1 91)
    (174, Token (PIPE _)) -> Just (Reduce 1 91)
    (174, Token (QCONID _)) -> Just (Reduce 1 91)
    (174, Token (EXPORT _)) -> Just (Reduce 1 91)
    (174, Token (AS _)) -> Just (Reduce 1 91)
    (174, Token (QVARID _)) -> Just (Reduce 1 91)
    (174, Token (INTEGER _)) -> Just (Reduce 1 91)
    (174, Token (QVARSYM _)) -> Just (Reduce 1 91)
    (174, Token (QCONSYM _)) -> Just (Reduce 1 91)
    (174, Token (BACKQUOTE _)) -> Just (Reduce 1 91)
    (175, Token (WHERE _)) -> Just (Reduce 2 92)
    (175, Token (LBRACE _)) -> Just (Reduce 2 92)
    (175, Token (RBRACE _)) -> Just (Reduce 2 92)
    (175, Token (LPAREN _)) -> Just (Reduce 2 92)
    (175, Token (RPAREN _)) -> Just (Reduce 2 92)
    (175, Token (COMMA _)) -> Just (Reduce 2 92)
    (175, Token (SEMICOLON _)) -> Just (Reduce 2 92)
    (175, Token (EQUAL _)) -> Just (Reduce 2 92)
    (175, Token (DERIVING _)) -> Just (Reduce 2 92)
    (175, Token (DARROW _)) -> Just (Reduce 2 92)
    (175, Token (COLON_COLON _)) -> Just (Reduce 2 92)
    (175, Token (INFIXL _)) -> Just (Reduce 2 92)
    (175, Token (INFIXR _)) -> Just (Reduce 2 92)
    (175, Token (INFIX _)) -> Just (Reduce 2 92)
    (175, Token (RARROW _)) -> Just (Reduce 2 92)
    (175, Token (LBRACKET _)) -> Just (Reduce 2 92)
    (175, Token (RBRACKET _)) -> Just (Reduce 2 92)
    (175, Token (EXCL _)) -> Just (Reduce 2 92)
    (175, Token (PIPE _)) -> Just (Reduce 2 92)
    (175, Token (QCONID _)) -> Just (Reduce 2 92)
    (175, Token (EXPORT _)) -> Just (Reduce 2 92)
    (175, Token (AS _)) -> Just (Reduce 2 92)
    (175, Token (QVARID _)) -> Just (Reduce 2 92)
    (175, Token (INTEGER _)) -> Just (Reduce 2 92)
    (175, Token (QVARSYM _)) -> Just (Reduce 2 92)
    (175, Token (QCONSYM _)) -> Just (Reduce 2 92)
    (175, Token (BACKQUOTE _)) -> Just (Reduce 2 92)
    (176, Token (RBRACE _)) -> Just (Reduce 3 90)
    (176, Token (RPAREN _)) -> Just (Reduce 3 90)
    (176, Token (COMMA _)) -> Just (Reduce 3 90)
    (176, Token (SEMICOLON _)) -> Just (Reduce 3 90)
    (176, Token (RBRACKET _)) -> Just (Reduce 3 90)
    (177, Token (RBRACE _)) -> Just (Reduce 2 109)
    (177, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (177, Token (DERIVING _)) -> Just (Reduce 2 109)
    (178, Token (QCONID _)) -> Just (Shift 179)
    (179, Token (RBRACE _)) -> Just (Reduce 1 123)
    (179, Token (RPAREN _)) -> Just (Reduce 1 123)
    (179, Token (COMMA _)) -> Just (Reduce 1 123)
    (179, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (180, Token (RPAREN _)) -> Just (Reduce 1 121)
    (180, Token (COMMA _)) -> Just (Shift 178)
    (181, Token (RPAREN _)) -> Just (Reduce 3 122)
    (182, Token (RBRACE _)) -> Just (Reduce 7 117)
    (182, Token (SEMICOLON _)) -> Just (Reduce 7 117)
    (182, Token (DERIVING _)) -> Just (Reduce 7 117)
    (183, Token (COLON_COLON _)) -> Just (Shift 94)
    (184, Token (RBRACE _)) -> Just (Shift 182)
    (185, Token (RBRACE _)) -> Just (Reduce 3 116)
    (185, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (185, Token (DERIVING _)) -> Just (Reduce 3 116)
    (186, Token (LBRACE _)) -> Just (Shift 39)
    (187, Token (RBRACE _)) -> Just (Reduce 2 63)
    (187, Token (SEMICOLON _)) -> Just (Reduce 2 63)
    (188, Token (LBRACE _)) -> Just (Shift 42)
    (189, Token (RBRACE _)) -> Just (Reduce 2 70)
    (189, Token (SEMICOLON _)) -> Just (Reduce 2 70)
    (190, Token (RPAREN _)) -> Just (Reduce 1 87)
    (190, Token (COMMA _)) -> Just (Shift 104)
    (191, Token (RPAREN _)) -> Just (Reduce 3 88)
    (192, Token (EXPORT _)) -> Just (Shift 281)
    (192, Token (AS _)) -> Just (Shift 282)
    (192, Token (QVARID _)) -> Just (Shift 283)
    (193, Token (EXPORT _)) -> Just (Shift 281)
    (193, Token (AS _)) -> Just (Shift 282)
    (193, Token (QVARID _)) -> Just (Shift 283)
    (194, Token (COLON_COLON _)) -> Just (Shift 88)
    (195, Token (COLON_COLON _)) -> Just (Shift 89)
    (196, Token (COLON_COLON _)) -> Just (Shift 90)
    (197, Token (RBRACE _)) -> Just (Reduce 6 124)
    (197, Token (SEMICOLON _)) -> Just (Reduce 6 124)
    (198, Token (RBRACE _)) -> Just (Reduce 7 125)
    (198, Token (SEMICOLON _)) -> Just (Reduce 7 125)
    (199, Token (RBRACE _)) -> Just (Reduce 6 126)
    (199, Token (SEMICOLON _)) -> Just (Reduce 6 126)
    (200, Token (EXPORT _)) -> Just (Shift 285)
    (200, Token (AS _)) -> Just (Shift 286)
    (200, Token (QVARID _)) -> Just (Shift 287)
    (200, Token (STRING _)) -> Just (Shift 284)
    (201, Token (STRING _)) -> Just (Shift 288)
    (202, Token (STRING _)) -> Just (Shift 284)
    (203, Token (RBRACE _)) -> Just (Reduce 1 60)
    (203, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (204, Token (RBRACE _)) -> Just (Reduce 2 61)
    (204, Token (SEMICOLON _)) -> Just (Reduce 2 61)
    (205, Token (LBRACE _)) -> Just (Shift 37)
    (206, Token (RBRACE _)) -> Just (Reduce 3 57)
    (206, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (207, Token (RBRACE _)) -> Just (Shift 206)
    (208, Token (RBRACE _)) -> Just (Reduce 1 58)
    (208, Token (SEMICOLON _)) -> Just (Shift 38)
    (209, Token (RBRACE _)) -> Just (Reduce 3 59)
    (210, Token (RBRACE _)) -> Just (Reduce 5 78)
    (210, Token (SEMICOLON _)) -> Just (Reduce 5 78)
    (211, Token (RBRACE _)) -> Just (Reduce 3 77)
    (211, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (212, Token (COLON_COLON _)) -> Just (Shift 84)
    (213, Token (COMMA _)) -> Just (Reduce 0 172)
    (213, Token (QCONID _)) -> Just (Reduce 0 172)
    (213, Token (EXPORT _)) -> Just (Reduce 0 172)
    (213, Token (AS _)) -> Just (Reduce 0 172)
    (213, Token (QVARID _)) -> Just (Reduce 0 172)
    (213, Token (INTEGER _)) -> Just (Shift 237)
    (213, Token (QVARSYM _)) -> Just (Reduce 0 172)
    (213, Token (QCONSYM _)) -> Just (Reduce 0 172)
    (213, Token (BACKQUOTE _)) -> Just (Reduce 0 172)
    (214, Token (QVARSYM _)) -> Just (Shift 300)
    (214, Token (QCONSYM _)) -> Just (Shift 273)
    (214, Token (BACKQUOTE _)) -> Just (Shift 274)
    (215, Token (RBRACE _)) -> Just (Reduce 3 79)
    (215, Token (SEMICOLON _)) -> Just (Reduce 3 79)
    (216, Token (LPAREN _)) -> Just (Reduce 1 146)
    (216, Token (EQUAL _)) -> Just (Reduce 1 146)
    (216, Token (QCONID _)) -> Just (Reduce 1 146)
    (216, Token (EXPORT _)) -> Just (Reduce 1 146)
    (216, Token (AS _)) -> Just (Reduce 1 146)
    (216, Token (QVARID _)) -> Just (Reduce 1 146)
    (216, Token (INTEGER _)) -> Just (Reduce 1 146)
    (216, Token (QVARSYM _)) -> Just (Reduce 1 146)
    (216, Token (QCONSYM _)) -> Just (Reduce 1 146)
    (216, Token (BACKQUOTE _)) -> Just (Reduce 1 146)
    (217, Token (LPAREN _)) -> Just (Reduce 2 147)
    (217, Token (EQUAL _)) -> Just (Reduce 2 147)
    (217, Token (QCONID _)) -> Just (Reduce 2 147)
    (217, Token (EXPORT _)) -> Just (Reduce 2 147)
    (217, Token (AS _)) -> Just (Reduce 2 147)
    (217, Token (QVARID _)) -> Just (Reduce 2 147)
    (217, Token (INTEGER _)) -> Just (Reduce 2 147)
    (217, Token (QVARSYM _)) -> Just (Reduce 2 147)
    (217, Token (QCONSYM _)) -> Just (Reduce 2 147)
    (217, Token (BACKQUOTE _)) -> Just (Reduce 2 147)
    (218, Token (RBRACE _)) -> Just (Reduce 4 137)
    (218, Token (SEMICOLON _)) -> Just (Reduce 4 137)
    (219, Token (WHERE _)) -> Just (Shift 205)
    (219, Token (RBRACE _)) -> Just (Reduce 2 136)
    (219, Token (SEMICOLON _)) -> Just (Reduce 2 136)
    (220, Token (RBRACE _)) -> Just (Reduce 3 64)
    (220, Token (SEMICOLON _)) -> Just (Reduce 3 64)
    (221, Token (RBRACE _)) -> Just (Shift 220)
    (222, Token (RBRACE _)) -> Just (Reduce 3 66)
    (223, Token (RBRACE _)) -> Just (Reduce 1 65)
    (223, Token (SEMICOLON _)) -> Just (Shift 40)
    (224, Token (RBRACE _)) -> Just (Reduce 1 67)
    (224, Token (SEMICOLON _)) -> Just (Reduce 1 67)
    (225, Token (RBRACE _)) -> Just (Reduce 2 68)
    (225, Token (SEMICOLON _)) -> Just (Reduce 2 68)
    (226, Token (RBRACE _)) -> Just (Reduce 3 71)
    (226, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (227, Token (RBRACE _)) -> Just (Shift 226)
    (228, Token (RBRACE _)) -> Just (Reduce 3 73)
    (229, Token (RBRACE _)) -> Just (Reduce 1 72)
    (229, Token (SEMICOLON _)) -> Just (Shift 43)
    (230, Token (RBRACE _)) -> Just (Reduce 2 75)
    (230, Token (SEMICOLON _)) -> Just (Reduce 2 75)
    (231, Token (COMMA _)) -> Just (Shift 51)
    (231, Token (COLON_COLON _)) -> Just (Reduce 1 82)
    (232, Token (LPAREN _)) -> Just (Reduce 1 148)
    (232, Token (COMMA _)) -> Just (Shift 51)
    (232, Token (EQUAL _)) -> Just (Reduce 1 148)
    (232, Token (COLON_COLON _)) -> Just (Reduce 1 82)
    (232, Token (QCONID _)) -> Just (Reduce 1 148)
    (232, Token (EXPORT _)) -> Just (Reduce 1 148)
    (232, Token (AS _)) -> Just (Reduce 1 148)
    (232, Token (QVARID _)) -> Just (Reduce 1 148)
    (232, Token (INTEGER _)) -> Just (Reduce 1 148)
    (232, Token (QVARSYM _)) -> Just (Reduce 1 148)
    (232, Token (QCONSYM _)) -> Just (Reduce 1 148)
    (232, Token (BACKQUOTE _)) -> Just (Reduce 1 148)
    (233, Token (COLON_COLON _)) -> Just (Reduce 3 83)
    (234, Token (COMMA _)) -> Just (Reduce 1 84)
    (234, Token (QCONID _)) -> Just (Reduce 1 84)
    (234, Token (EXPORT _)) -> Just (Reduce 1 84)
    (234, Token (AS _)) -> Just (Reduce 1 84)
    (234, Token (QVARID _)) -> Just (Reduce 1 84)
    (234, Token (INTEGER _)) -> Just (Reduce 1 84)
    (234, Token (QVARSYM _)) -> Just (Reduce 1 84)
    (234, Token (QCONSYM _)) -> Just (Reduce 1 84)
    (234, Token (BACKQUOTE _)) -> Just (Reduce 1 84)
    (235, Token (COMMA _)) -> Just (Reduce 1 85)
    (235, Token (QCONID _)) -> Just (Reduce 1 85)
    (235, Token (EXPORT _)) -> Just (Reduce 1 85)
    (235, Token (AS _)) -> Just (Reduce 1 85)
    (235, Token (QVARID _)) -> Just (Reduce 1 85)
    (235, Token (INTEGER _)) -> Just (Reduce 1 85)
    (235, Token (QVARSYM _)) -> Just (Reduce 1 85)
    (235, Token (QCONSYM _)) -> Just (Reduce 1 85)
    (235, Token (BACKQUOTE _)) -> Just (Reduce 1 85)
    (236, Token (COMMA _)) -> Just (Reduce 1 86)
    (236, Token (QCONID _)) -> Just (Reduce 1 86)
    (236, Token (EXPORT _)) -> Just (Reduce 1 86)
    (236, Token (AS _)) -> Just (Reduce 1 86)
    (236, Token (QVARID _)) -> Just (Reduce 1 86)
    (236, Token (INTEGER _)) -> Just (Reduce 1 86)
    (236, Token (QVARSYM _)) -> Just (Reduce 1 86)
    (236, Token (QCONSYM _)) -> Just (Reduce 1 86)
    (236, Token (BACKQUOTE _)) -> Just (Reduce 1 86)
    (237, Token (COMMA _)) -> Just (Reduce 1 173)
    (237, Token (QCONID _)) -> Just (Reduce 1 173)
    (237, Token (EXPORT _)) -> Just (Reduce 1 173)
    (237, Token (AS _)) -> Just (Reduce 1 173)
    (237, Token (QVARID _)) -> Just (Reduce 1 173)
    (237, Token (QVARSYM _)) -> Just (Reduce 1 173)
    (237, Token (QCONSYM _)) -> Just (Reduce 1 173)
    (237, Token (BACKQUOTE _)) -> Just (Reduce 1 173)
    (238, Token (QVARSYM _)) -> Just (Shift 300)
    (238, Token (QCONSYM _)) -> Just (Shift 273)
    (238, Token (BACKQUOTE _)) -> Just (Shift 274)
    (239, Token (RBRACE _)) -> Just (Reduce 3 81)
    (239, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (240, Token (RBRACE _)) -> Just (Reduce 1 80)
    (240, Token (COMMA _)) -> Just (Shift 238)
    (240, Token (SEMICOLON _)) -> Just (Reduce 1 80)
    (241, Token (RBRACE _)) -> Just (Reduce 1 162)
    (241, Token (LPAREN _)) -> Just (Reduce 1 162)
    (241, Token (COMMA _)) -> Just (Reduce 1 162)
    (241, Token (SEMICOLON _)) -> Just (Reduce 1 162)
    (241, Token (EXPORT _)) -> Just (Reduce 1 162)
    (241, Token (AS _)) -> Just (Reduce 1 162)
    (241, Token (QVARID _)) -> Just (Reduce 1 162)
    (241, Token (INTEGER _)) -> Just (Reduce 1 162)
    (241, Token (QVARSYM _)) -> Just (Reduce 1 162)
    (242, Token (RBRACE _)) -> Just (Reduce 1 161)
    (242, Token (LPAREN _)) -> Just (Reduce 1 161)
    (242, Token (COMMA _)) -> Just (Reduce 1 161)
    (242, Token (SEMICOLON _)) -> Just (Reduce 1 161)
    (242, Token (EXPORT _)) -> Just (Reduce 1 161)
    (242, Token (AS _)) -> Just (Reduce 1 161)
    (242, Token (QVARID _)) -> Just (Reduce 1 161)
    (242, Token (INTEGER _)) -> Just (Reduce 1 161)
    (242, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (243, Token (WHERE _)) -> Just (Reduce 3 97)
    (243, Token (LBRACE _)) -> Just (Reduce 3 97)
    (243, Token (RBRACE _)) -> Just (Reduce 3 97)
    (243, Token (LPAREN _)) -> Just (Reduce 3 97)
    (243, Token (RPAREN _)) -> Just (Reduce 3 97)
    (243, Token (COMMA _)) -> Just (Reduce 3 97)
    (243, Token (SEMICOLON _)) -> Just (Reduce 3 97)
    (243, Token (EQUAL _)) -> Just (Reduce 3 97)
    (243, Token (DERIVING _)) -> Just (Reduce 3 97)
    (243, Token (DARROW _)) -> Just (Reduce 3 97)
    (243, Token (COLON_COLON _)) -> Just (Reduce 3 97)
    (243, Token (INFIXL _)) -> Just (Reduce 3 97)
    (243, Token (INFIXR _)) -> Just (Reduce 3 97)
    (243, Token (INFIX _)) -> Just (Reduce 3 97)
    (243, Token (RARROW _)) -> Just (Reduce 3 97)
    (243, Token (LBRACKET _)) -> Just (Reduce 3 97)
    (243, Token (RBRACKET _)) -> Just (Reduce 3 97)
    (243, Token (EXCL _)) -> Just (Reduce 3 97)
    (243, Token (PIPE _)) -> Just (Reduce 3 97)
    (243, Token (QCONID _)) -> Just (Reduce 3 97)
    (243, Token (EXPORT _)) -> Just (Reduce 3 97)
    (243, Token (AS _)) -> Just (Reduce 3 97)
    (243, Token (QVARID _)) -> Just (Reduce 3 97)
    (243, Token (INTEGER _)) -> Just (Reduce 3 97)
    (243, Token (QVARSYM _)) -> Just (Reduce 3 97)
    (243, Token (QCONSYM _)) -> Just (Reduce 3 97)
    (243, Token (BACKQUOTE _)) -> Just (Reduce 3 97)
    (244, Token (WHERE _)) -> Just (Reduce 3 95)
    (244, Token (LBRACE _)) -> Just (Reduce 3 95)
    (244, Token (RBRACE _)) -> Just (Reduce 3 95)
    (244, Token (LPAREN _)) -> Just (Reduce 3 95)
    (244, Token (RPAREN _)) -> Just (Reduce 3 95)
    (244, Token (COMMA _)) -> Just (Reduce 3 95)
    (244, Token (SEMICOLON _)) -> Just (Reduce 3 95)
    (244, Token (EQUAL _)) -> Just (Reduce 3 95)
    (244, Token (DERIVING _)) -> Just (Reduce 3 95)
    (244, Token (DARROW _)) -> Just (Reduce 3 95)
    (244, Token (COLON_COLON _)) -> Just (Reduce 3 95)
    (244, Token (INFIXL _)) -> Just (Reduce 3 95)
    (244, Token (INFIXR _)) -> Just (Reduce 3 95)
    (244, Token (INFIX _)) -> Just (Reduce 3 95)
    (244, Token (RARROW _)) -> Just (Reduce 3 95)
    (244, Token (LBRACKET _)) -> Just (Reduce 3 95)
    (244, Token (RBRACKET _)) -> Just (Reduce 3 95)
    (244, Token (EXCL _)) -> Just (Reduce 3 95)
    (244, Token (PIPE _)) -> Just (Reduce 3 95)
    (244, Token (QCONID _)) -> Just (Reduce 3 95)
    (244, Token (EXPORT _)) -> Just (Reduce 3 95)
    (244, Token (AS _)) -> Just (Reduce 3 95)
    (244, Token (QVARID _)) -> Just (Reduce 3 95)
    (244, Token (INTEGER _)) -> Just (Reduce 3 95)
    (244, Token (QVARSYM _)) -> Just (Reduce 3 95)
    (244, Token (QCONSYM _)) -> Just (Reduce 3 95)
    (244, Token (BACKQUOTE _)) -> Just (Reduce 3 95)
    (245, Token (WHERE _)) -> Just (Reduce 3 96)
    (245, Token (LBRACE _)) -> Just (Reduce 3 96)
    (245, Token (RBRACE _)) -> Just (Reduce 3 96)
    (245, Token (LPAREN _)) -> Just (Reduce 3 96)
    (245, Token (RPAREN _)) -> Just (Reduce 3 96)
    (245, Token (COMMA _)) -> Just (Reduce 3 96)
    (245, Token (SEMICOLON _)) -> Just (Reduce 3 96)
    (245, Token (EQUAL _)) -> Just (Reduce 3 96)
    (245, Token (DERIVING _)) -> Just (Reduce 3 96)
    (245, Token (DARROW _)) -> Just (Reduce 3 96)
    (245, Token (COLON_COLON _)) -> Just (Reduce 3 96)
    (245, Token (INFIXL _)) -> Just (Reduce 3 96)
    (245, Token (INFIXR _)) -> Just (Reduce 3 96)
    (245, Token (INFIX _)) -> Just (Reduce 3 96)
    (245, Token (RARROW _)) -> Just (Reduce 3 96)
    (245, Token (LBRACKET _)) -> Just (Reduce 3 96)
    (245, Token (RBRACKET _)) -> Just (Reduce 3 96)
    (245, Token (EXCL _)) -> Just (Reduce 3 96)
    (245, Token (PIPE _)) -> Just (Reduce 3 96)
    (245, Token (QCONID _)) -> Just (Reduce 3 96)
    (245, Token (EXPORT _)) -> Just (Reduce 3 96)
    (245, Token (AS _)) -> Just (Reduce 3 96)
    (245, Token (QVARID _)) -> Just (Reduce 3 96)
    (245, Token (INTEGER _)) -> Just (Reduce 3 96)
    (245, Token (QVARSYM _)) -> Just (Reduce 3 96)
    (245, Token (QCONSYM _)) -> Just (Reduce 3 96)
    (245, Token (BACKQUOTE _)) -> Just (Reduce 3 96)
    (246, Token (RPAREN _)) -> Just (Shift 243)
    (246, Token (COMMA _)) -> Just (Shift 105)
    (247, Token (RBRACKET _)) -> Just (Shift 245)
    (248, Token (WHERE _)) -> Just (Reduce 2 98)
    (248, Token (LBRACE _)) -> Just (Reduce 2 98)
    (248, Token (RBRACE _)) -> Just (Reduce 2 98)
    (248, Token (LPAREN _)) -> Just (Reduce 2 98)
    (248, Token (RPAREN _)) -> Just (Reduce 2 98)
    (248, Token (COMMA _)) -> Just (Reduce 2 98)
    (248, Token (SEMICOLON _)) -> Just (Reduce 2 98)
    (248, Token (EQUAL _)) -> Just (Reduce 2 98)
    (248, Token (DERIVING _)) -> Just (Reduce 2 98)
    (248, Token (DARROW _)) -> Just (Reduce 2 98)
    (248, Token (COLON_COLON _)) -> Just (Reduce 2 98)
    (248, Token (INFIXL _)) -> Just (Reduce 2 98)
    (248, Token (INFIXR _)) -> Just (Reduce 2 98)
    (248, Token (INFIX _)) -> Just (Reduce 2 98)
    (248, Token (RARROW _)) -> Just (Reduce 2 98)
    (248, Token (LBRACKET _)) -> Just (Reduce 2 98)
    (248, Token (RBRACKET _)) -> Just (Reduce 2 98)
    (248, Token (EXCL _)) -> Just (Reduce 2 98)
    (248, Token (PIPE _)) -> Just (Reduce 2 98)
    (248, Token (QCONID _)) -> Just (Reduce 2 98)
    (248, Token (EXPORT _)) -> Just (Reduce 2 98)
    (248, Token (AS _)) -> Just (Reduce 2 98)
    (248, Token (QVARID _)) -> Just (Reduce 2 98)
    (248, Token (INTEGER _)) -> Just (Reduce 2 98)
    (248, Token (QVARSYM _)) -> Just (Reduce 2 98)
    (248, Token (QCONSYM _)) -> Just (Reduce 2 98)
    (248, Token (BACKQUOTE _)) -> Just (Reduce 2 98)
    (249, Token (WHERE _)) -> Just (Reduce 1 93)
    (249, Token (LBRACE _)) -> Just (Reduce 1 93)
    (249, Token (RBRACE _)) -> Just (Reduce 1 93)
    (249, Token (LPAREN _)) -> Just (Reduce 1 93)
    (249, Token (RPAREN _)) -> Just (Reduce 1 93)
    (249, Token (COMMA _)) -> Just (Reduce 1 93)
    (249, Token (SEMICOLON _)) -> Just (Reduce 1 93)
    (249, Token (EQUAL _)) -> Just (Reduce 1 93)
    (249, Token (DERIVING _)) -> Just (Reduce 1 93)
    (249, Token (DARROW _)) -> Just (Reduce 1 93)
    (249, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (249, Token (INFIXL _)) -> Just (Reduce 1 93)
    (249, Token (INFIXR _)) -> Just (Reduce 1 93)
    (249, Token (INFIX _)) -> Just (Reduce 1 93)
    (249, Token (RARROW _)) -> Just (Reduce 1 93)
    (249, Token (LBRACKET _)) -> Just (Reduce 1 93)
    (249, Token (RBRACKET _)) -> Just (Reduce 1 93)
    (249, Token (EXCL _)) -> Just (Reduce 1 93)
    (249, Token (PIPE _)) -> Just (Reduce 1 93)
    (249, Token (QCONID _)) -> Just (Reduce 1 93)
    (249, Token (EXPORT _)) -> Just (Reduce 1 93)
    (249, Token (AS _)) -> Just (Reduce 1 93)
    (249, Token (QVARID _)) -> Just (Reduce 1 93)
    (249, Token (INTEGER _)) -> Just (Reduce 1 93)
    (249, Token (QVARSYM _)) -> Just (Reduce 1 93)
    (249, Token (QCONSYM _)) -> Just (Reduce 1 93)
    (249, Token (BACKQUOTE _)) -> Just (Reduce 1 93)
    (250, Token (WHERE _)) -> Just (Reduce 1 94)
    (250, Token (LBRACE _)) -> Just (Reduce 1 94)
    (250, Token (RBRACE _)) -> Just (Reduce 1 94)
    (250, Token (LPAREN _)) -> Just (Reduce 1 94)
    (250, Token (RPAREN _)) -> Just (Reduce 1 94)
    (250, Token (COMMA _)) -> Just (Reduce 1 94)
    (250, Token (SEMICOLON _)) -> Just (Reduce 1 94)
    (250, Token (EQUAL _)) -> Just (Reduce 1 94)
    (250, Token (DERIVING _)) -> Just (Reduce 1 94)
    (250, Token (DARROW _)) -> Just (Reduce 1 94)
    (250, Token (COLON_COLON _)) -> Just (Reduce 1 94)
    (250, Token (INFIXL _)) -> Just (Reduce 1 94)
    (250, Token (INFIXR _)) -> Just (Reduce 1 94)
    (250, Token (INFIX _)) -> Just (Reduce 1 94)
    (250, Token (RARROW _)) -> Just (Reduce 1 94)
    (250, Token (LBRACKET _)) -> Just (Reduce 1 94)
    (250, Token (RBRACKET _)) -> Just (Reduce 1 94)
    (250, Token (EXCL _)) -> Just (Reduce 1 94)
    (250, Token (PIPE _)) -> Just (Reduce 1 94)
    (250, Token (QCONID _)) -> Just (Reduce 1 94)
    (250, Token (EXPORT _)) -> Just (Reduce 1 94)
    (250, Token (AS _)) -> Just (Reduce 1 94)
    (250, Token (QVARID _)) -> Just (Reduce 1 94)
    (250, Token (INTEGER _)) -> Just (Reduce 1 94)
    (250, Token (QVARSYM _)) -> Just (Reduce 1 94)
    (250, Token (QCONSYM _)) -> Just (Reduce 1 94)
    (250, Token (BACKQUOTE _)) -> Just (Reduce 1 94)
    (251, Token (RPAREN _)) -> Just (Shift 244)
    (252, Token (WHERE _)) -> Just (Reduce 2 102)
    (252, Token (LBRACE _)) -> Just (Reduce 2 102)
    (252, Token (RBRACE _)) -> Just (Reduce 2 102)
    (252, Token (LPAREN _)) -> Just (Reduce 2 102)
    (252, Token (RPAREN _)) -> Just (Reduce 2 102)
    (252, Token (COMMA _)) -> Just (Reduce 2 102)
    (252, Token (SEMICOLON _)) -> Just (Reduce 2 102)
    (252, Token (EQUAL _)) -> Just (Reduce 2 102)
    (252, Token (DERIVING _)) -> Just (Reduce 2 102)
    (252, Token (DARROW _)) -> Just (Reduce 2 102)
    (252, Token (COLON_COLON _)) -> Just (Reduce 2 102)
    (252, Token (INFIXL _)) -> Just (Reduce 2 102)
    (252, Token (INFIXR _)) -> Just (Reduce 2 102)
    (252, Token (INFIX _)) -> Just (Reduce 2 102)
    (252, Token (RARROW _)) -> Just (Reduce 2 102)
    (252, Token (LBRACKET _)) -> Just (Reduce 2 102)
    (252, Token (RBRACKET _)) -> Just (Reduce 2 102)
    (252, Token (EXCL _)) -> Just (Reduce 2 102)
    (252, Token (PIPE _)) -> Just (Reduce 2 102)
    (252, Token (QCONID _)) -> Just (Reduce 2 102)
    (252, Token (EXPORT _)) -> Just (Reduce 2 102)
    (252, Token (AS _)) -> Just (Reduce 2 102)
    (252, Token (QVARID _)) -> Just (Reduce 2 102)
    (252, Token (INTEGER _)) -> Just (Reduce 2 102)
    (252, Token (QVARSYM _)) -> Just (Reduce 2 102)
    (252, Token (QCONSYM _)) -> Just (Reduce 2 102)
    (252, Token (BACKQUOTE _)) -> Just (Reduce 2 102)
    (253, Token (WHERE _)) -> Just (Reduce 3 104)
    (253, Token (LBRACE _)) -> Just (Reduce 3 104)
    (253, Token (RBRACE _)) -> Just (Reduce 3 104)
    (253, Token (LPAREN _)) -> Just (Reduce 3 104)
    (253, Token (RPAREN _)) -> Just (Reduce 3 104)
    (253, Token (COMMA _)) -> Just (Reduce 3 104)
    (253, Token (SEMICOLON _)) -> Just (Reduce 3 104)
    (253, Token (EQUAL _)) -> Just (Reduce 3 104)
    (253, Token (DERIVING _)) -> Just (Reduce 3 104)
    (253, Token (DARROW _)) -> Just (Reduce 3 104)
    (253, Token (COLON_COLON _)) -> Just (Reduce 3 104)
    (253, Token (INFIXL _)) -> Just (Reduce 3 104)
    (253, Token (INFIXR _)) -> Just (Reduce 3 104)
    (253, Token (INFIX _)) -> Just (Reduce 3 104)
    (253, Token (RARROW _)) -> Just (Reduce 3 104)
    (253, Token (LBRACKET _)) -> Just (Reduce 3 104)
    (253, Token (RBRACKET _)) -> Just (Reduce 3 104)
    (253, Token (EXCL _)) -> Just (Reduce 3 104)
    (253, Token (PIPE _)) -> Just (Reduce 3 104)
    (253, Token (QCONID _)) -> Just (Reduce 3 104)
    (253, Token (EXPORT _)) -> Just (Reduce 3 104)
    (253, Token (AS _)) -> Just (Reduce 3 104)
    (253, Token (QVARID _)) -> Just (Reduce 3 104)
    (253, Token (INTEGER _)) -> Just (Reduce 3 104)
    (253, Token (QVARSYM _)) -> Just (Reduce 3 104)
    (253, Token (QCONSYM _)) -> Just (Reduce 3 104)
    (253, Token (BACKQUOTE _)) -> Just (Reduce 3 104)
    (254, Token (WHERE _)) -> Just (Reduce 3 105)
    (254, Token (LBRACE _)) -> Just (Reduce 3 105)
    (254, Token (RBRACE _)) -> Just (Reduce 3 105)
    (254, Token (LPAREN _)) -> Just (Reduce 3 105)
    (254, Token (RPAREN _)) -> Just (Reduce 3 105)
    (254, Token (COMMA _)) -> Just (Reduce 3 105)
    (254, Token (SEMICOLON _)) -> Just (Reduce 3 105)
    (254, Token (EQUAL _)) -> Just (Reduce 3 105)
    (254, Token (DERIVING _)) -> Just (Reduce 3 105)
    (254, Token (DARROW _)) -> Just (Reduce 3 105)
    (254, Token (COLON_COLON _)) -> Just (Reduce 3 105)
    (254, Token (INFIXL _)) -> Just (Reduce 3 105)
    (254, Token (INFIXR _)) -> Just (Reduce 3 105)
    (254, Token (INFIX _)) -> Just (Reduce 3 105)
    (254, Token (RARROW _)) -> Just (Reduce 3 105)
    (254, Token (LBRACKET _)) -> Just (Reduce 3 105)
    (254, Token (RBRACKET _)) -> Just (Reduce 3 105)
    (254, Token (EXCL _)) -> Just (Reduce 3 105)
    (254, Token (PIPE _)) -> Just (Reduce 3 105)
    (254, Token (QCONID _)) -> Just (Reduce 3 105)
    (254, Token (EXPORT _)) -> Just (Reduce 3 105)
    (254, Token (AS _)) -> Just (Reduce 3 105)
    (254, Token (QVARID _)) -> Just (Reduce 3 105)
    (254, Token (INTEGER _)) -> Just (Reduce 3 105)
    (254, Token (QVARSYM _)) -> Just (Reduce 3 105)
    (254, Token (QCONSYM _)) -> Just (Reduce 3 105)
    (254, Token (BACKQUOTE _)) -> Just (Reduce 3 105)
    (255, Token (RPAREN _)) -> Just (Shift 253)
    (256, Token (WHERE _)) -> Just (Reduce 2 103)
    (256, Token (LBRACE _)) -> Just (Reduce 2 103)
    (256, Token (RBRACE _)) -> Just (Reduce 2 103)
    (256, Token (LPAREN _)) -> Just (Reduce 2 103)
    (256, Token (RPAREN _)) -> Just (Reduce 2 103)
    (256, Token (COMMA _)) -> Just (Reduce 2 103)
    (256, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (256, Token (EQUAL _)) -> Just (Reduce 2 103)
    (256, Token (DERIVING _)) -> Just (Reduce 2 103)
    (256, Token (DARROW _)) -> Just (Reduce 2 103)
    (256, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (256, Token (INFIXL _)) -> Just (Reduce 2 103)
    (256, Token (INFIXR _)) -> Just (Reduce 2 103)
    (256, Token (INFIX _)) -> Just (Reduce 2 103)
    (256, Token (RARROW _)) -> Just (Reduce 2 103)
    (256, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (256, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (256, Token (EXCL _)) -> Just (Reduce 2 103)
    (256, Token (PIPE _)) -> Just (Reduce 2 103)
    (256, Token (QCONID _)) -> Just (Reduce 2 103)
    (256, Token (EXPORT _)) -> Just (Reduce 2 103)
    (256, Token (AS _)) -> Just (Reduce 2 103)
    (256, Token (QVARID _)) -> Just (Reduce 2 103)
    (256, Token (INTEGER _)) -> Just (Reduce 2 103)
    (256, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (256, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (256, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (257, Token (WHERE _)) -> Just (Reduce 1 101)
    (257, Token (LBRACE _)) -> Just (Reduce 1 101)
    (257, Token (RBRACE _)) -> Just (Reduce 1 101)
    (257, Token (LPAREN _)) -> Just (Reduce 1 101)
    (257, Token (RPAREN _)) -> Just (Reduce 1 101)
    (257, Token (COMMA _)) -> Just (Reduce 1 101)
    (257, Token (SEMICOLON _)) -> Just (Reduce 1 101)
    (257, Token (EQUAL _)) -> Just (Reduce 1 101)
    (257, Token (DERIVING _)) -> Just (Reduce 1 101)
    (257, Token (DARROW _)) -> Just (Reduce 1 101)
    (257, Token (COLON_COLON _)) -> Just (Reduce 1 101)
    (257, Token (INFIXL _)) -> Just (Reduce 1 101)
    (257, Token (INFIXR _)) -> Just (Reduce 1 101)
    (257, Token (INFIX _)) -> Just (Reduce 1 101)
    (257, Token (RARROW _)) -> Just (Reduce 1 101)
    (257, Token (LBRACKET _)) -> Just (Reduce 1 101)
    (257, Token (RBRACKET _)) -> Just (Reduce 1 101)
    (257, Token (EXCL _)) -> Just (Reduce 1 101)
    (257, Token (PIPE _)) -> Just (Reduce 1 101)
    (257, Token (QCONID _)) -> Just (Reduce 1 101)
    (257, Token (EXPORT _)) -> Just (Reduce 1 101)
    (257, Token (AS _)) -> Just (Reduce 1 101)
    (257, Token (QVARID _)) -> Just (Reduce 1 101)
    (257, Token (INTEGER _)) -> Just (Reduce 1 101)
    (257, Token (QVARSYM _)) -> Just (Reduce 1 101)
    (257, Token (QCONSYM _)) -> Just (Reduce 1 101)
    (257, Token (BACKQUOTE _)) -> Just (Reduce 1 101)
    (258, Token (LBRACE _)) -> Just (Shift 52)
    (258, Token (RBRACE _)) -> Just (Reduce 1 101)
    (258, Token (LPAREN _)) -> Just (Reduce 1 101)
    (258, Token (RPAREN _)) -> Just (Reduce 1 101)
    (258, Token (COMMA _)) -> Just (Reduce 1 101)
    (258, Token (SEMICOLON _)) -> Just (Reduce 1 101)
    (258, Token (DERIVING _)) -> Just (Reduce 1 101)
    (258, Token (RARROW _)) -> Just (Reduce 1 101)
    (258, Token (LBRACKET _)) -> Just (Reduce 1 101)
    (258, Token (RBRACKET _)) -> Just (Reduce 1 101)
    (258, Token (EXCL _)) -> Just (Reduce 1 101)
    (258, Token (PIPE _)) -> Just (Reduce 1 101)
    (258, Token (QCONID _)) -> Just (Reduce 1 101)
    (258, Token (EXPORT _)) -> Just (Reduce 1 101)
    (258, Token (AS _)) -> Just (Reduce 1 101)
    (258, Token (QVARID _)) -> Just (Reduce 1 101)
    (258, Token (QCONSYM _)) -> Just (Reduce 1 101)
    (258, Token (BACKQUOTE _)) -> Just (Reduce 1 101)
    (259, Token (RPAREN _)) -> Just (Shift 254)
    (260, Token (WHERE _)) -> Just (Reduce 1 168)
    (260, Token (LBRACE _)) -> Just (Reduce 1 168)
    (260, Token (RBRACE _)) -> Just (Reduce 1 168)
    (260, Token (LPAREN _)) -> Just (Reduce 1 168)
    (260, Token (RPAREN _)) -> Just (Reduce 1 168)
    (260, Token (COMMA _)) -> Just (Reduce 1 168)
    (260, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (260, Token (EQUAL _)) -> Just (Reduce 1 168)
    (260, Token (DERIVING _)) -> Just (Reduce 1 168)
    (260, Token (DARROW _)) -> Just (Reduce 1 168)
    (260, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (260, Token (INFIXL _)) -> Just (Reduce 1 168)
    (260, Token (INFIXR _)) -> Just (Reduce 1 168)
    (260, Token (INFIX _)) -> Just (Reduce 1 168)
    (260, Token (RARROW _)) -> Just (Reduce 1 168)
    (260, Token (LBRACKET _)) -> Just (Reduce 1 168)
    (260, Token (RBRACKET _)) -> Just (Reduce 1 168)
    (260, Token (EXCL _)) -> Just (Reduce 1 168)
    (260, Token (PIPE _)) -> Just (Reduce 1 168)
    (260, Token (QCONID _)) -> Just (Reduce 1 168)
    (260, Token (EXPORT _)) -> Just (Reduce 1 168)
    (260, Token (AS _)) -> Just (Reduce 1 168)
    (260, Token (QVARID _)) -> Just (Reduce 1 168)
    (260, Token (INTEGER _)) -> Just (Reduce 1 168)
    (260, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (260, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (260, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (261, Token (WHERE _)) -> Just (Reduce 1 167)
    (261, Token (LBRACE _)) -> Just (Reduce 1 167)
    (261, Token (RBRACE _)) -> Just (Reduce 1 167)
    (261, Token (LPAREN _)) -> Just (Reduce 1 167)
    (261, Token (RPAREN _)) -> Just (Reduce 1 167)
    (261, Token (COMMA _)) -> Just (Reduce 1 167)
    (261, Token (SEMICOLON _)) -> Just (Reduce 1 167)
    (261, Token (EQUAL _)) -> Just (Reduce 1 167)
    (261, Token (DERIVING _)) -> Just (Reduce 1 167)
    (261, Token (DARROW _)) -> Just (Reduce 1 167)
    (261, Token (COLON_COLON _)) -> Just (Reduce 1 167)
    (261, Token (INFIXL _)) -> Just (Reduce 1 167)
    (261, Token (INFIXR _)) -> Just (Reduce 1 167)
    (261, Token (INFIX _)) -> Just (Reduce 1 167)
    (261, Token (RARROW _)) -> Just (Reduce 1 167)
    (261, Token (LBRACKET _)) -> Just (Reduce 1 167)
    (261, Token (RBRACKET _)) -> Just (Reduce 1 167)
    (261, Token (EXCL _)) -> Just (Reduce 1 167)
    (261, Token (PIPE _)) -> Just (Reduce 1 167)
    (261, Token (QCONID _)) -> Just (Reduce 1 167)
    (261, Token (EXPORT _)) -> Just (Reduce 1 167)
    (261, Token (AS _)) -> Just (Reduce 1 167)
    (261, Token (QVARID _)) -> Just (Reduce 1 167)
    (261, Token (INTEGER _)) -> Just (Reduce 1 167)
    (261, Token (QVARSYM _)) -> Just (Reduce 1 167)
    (261, Token (QCONSYM _)) -> Just (Reduce 1 167)
    (261, Token (BACKQUOTE _)) -> Just (Reduce 1 167)
    (262, Token (WHERE _)) -> Just (Reduce 1 169)
    (262, Token (LBRACE _)) -> Just (Reduce 1 169)
    (262, Token (RBRACE _)) -> Just (Reduce 1 169)
    (262, Token (LPAREN _)) -> Just (Reduce 1 169)
    (262, Token (RPAREN _)) -> Just (Reduce 1 169)
    (262, Token (COMMA _)) -> Just (Reduce 1 169)
    (262, Token (SEMICOLON _)) -> Just (Reduce 1 169)
    (262, Token (EQUAL _)) -> Just (Reduce 1 169)
    (262, Token (DERIVING _)) -> Just (Reduce 1 169)
    (262, Token (DARROW _)) -> Just (Reduce 1 169)
    (262, Token (COLON_COLON _)) -> Just (Reduce 1 169)
    (262, Token (INFIXL _)) -> Just (Reduce 1 169)
    (262, Token (INFIXR _)) -> Just (Reduce 1 169)
    (262, Token (INFIX _)) -> Just (Reduce 1 169)
    (262, Token (RARROW _)) -> Just (Reduce 1 169)
    (262, Token (LBRACKET _)) -> Just (Reduce 1 169)
    (262, Token (RBRACKET _)) -> Just (Reduce 1 169)
    (262, Token (EXCL _)) -> Just (Reduce 1 169)
    (262, Token (PIPE _)) -> Just (Reduce 1 169)
    (262, Token (QCONID _)) -> Just (Reduce 1 169)
    (262, Token (EXPORT _)) -> Just (Reduce 1 169)
    (262, Token (AS _)) -> Just (Reduce 1 169)
    (262, Token (QVARID _)) -> Just (Reduce 1 169)
    (262, Token (INTEGER _)) -> Just (Reduce 1 169)
    (262, Token (QVARSYM _)) -> Just (Reduce 1 169)
    (262, Token (QCONSYM _)) -> Just (Reduce 1 169)
    (262, Token (BACKQUOTE _)) -> Just (Reduce 1 169)
    (263, Token (RPAREN _)) -> Just (Reduce 3 99)
    (263, Token (COMMA _)) -> Just (Shift 105)
    (264, Token (RPAREN _)) -> Just (Reduce 3 100)
    (265, Token (RPAREN _)) -> Just (Reduce 1 106)
    (265, Token (COMMA _)) -> Just (Shift 265)
    (266, Token (RPAREN _)) -> Just (Reduce 2 107)
    (267, Token (RBRACE _)) -> Just (Reduce 3 111)
    (267, Token (SEMICOLON _)) -> Just (Reduce 3 111)
    (267, Token (DERIVING _)) -> Just (Reduce 3 111)
    (268, Token (RBRACE _)) -> Just (Reduce 1 110)
    (268, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (268, Token (DERIVING _)) -> Just (Reduce 1 110)
    (268, Token (PIPE _)) -> Just (Shift 79)
    (269, Token (RBRACE _)) -> Just (Reduce 3 114)
    (269, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (269, Token (DERIVING _)) -> Just (Reduce 3 114)
    (269, Token (PIPE _)) -> Just (Reduce 3 114)
    (270, Token (RBRACE _)) -> Just (Reduce 4 115)
    (270, Token (SEMICOLON _)) -> Just (Reduce 4 115)
    (270, Token (DERIVING _)) -> Just (Reduce 4 115)
    (270, Token (PIPE _)) -> Just (Reduce 4 115)
    (271, Token (RBRACE _)) -> Just (Shift 270)
    (272, Token (BACKQUOTE _)) -> Just (Shift 276)
    (273, Token (RBRACE _)) -> Just (Reduce 1 159)
    (273, Token (LPAREN _)) -> Just (Reduce 1 159)
    (273, Token (RPAREN _)) -> Just (Reduce 1 159)
    (273, Token (COMMA _)) -> Just (Reduce 1 159)
    (273, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (273, Token (RARROW _)) -> Just (Reduce 1 159)
    (273, Token (LBRACKET _)) -> Just (Reduce 1 159)
    (273, Token (RBRACKET _)) -> Just (Reduce 1 159)
    (273, Token (EXCL _)) -> Just (Reduce 1 159)
    (273, Token (QCONID _)) -> Just (Reduce 1 159)
    (273, Token (EXPORT _)) -> Just (Reduce 1 159)
    (273, Token (AS _)) -> Just (Reduce 1 159)
    (273, Token (QVARID _)) -> Just (Reduce 1 159)
    (273, Token (INTEGER _)) -> Just (Reduce 1 159)
    (273, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (273, Token (QCONSYM _)) -> Just (Reduce 1 159)
    (274, Token (QCONID _)) -> Just (Shift 272)
    (274, Token (EXPORT _)) -> Just (Shift 297)
    (274, Token (AS _)) -> Just (Shift 298)
    (274, Token (QVARID _)) -> Just (Shift 299)
    (275, Token (QCONID _)) -> Just (Shift 272)
    (276, Token (RBRACE _)) -> Just (Reduce 3 160)
    (276, Token (LPAREN _)) -> Just (Reduce 3 160)
    (276, Token (RPAREN _)) -> Just (Reduce 3 160)
    (276, Token (COMMA _)) -> Just (Reduce 3 160)
    (276, Token (SEMICOLON _)) -> Just (Reduce 3 160)
    (276, Token (RARROW _)) -> Just (Reduce 3 160)
    (276, Token (LBRACKET _)) -> Just (Reduce 3 160)
    (276, Token (RBRACKET _)) -> Just (Reduce 3 160)
    (276, Token (EXCL _)) -> Just (Reduce 3 160)
    (276, Token (QCONID _)) -> Just (Reduce 3 160)
    (276, Token (EXPORT _)) -> Just (Reduce 3 160)
    (276, Token (AS _)) -> Just (Reduce 3 160)
    (276, Token (QVARID _)) -> Just (Reduce 3 160)
    (276, Token (INTEGER _)) -> Just (Reduce 3 160)
    (276, Token (QVARSYM _)) -> Just (Reduce 3 160)
    (276, Token (QCONSYM _)) -> Just (Reduce 3 160)
    (277, Token (RBRACE _)) -> Just (Reduce 3 119)
    (278, Token (RBRACE _)) -> Just (Reduce 1 118)
    (278, Token (COMMA _)) -> Just (Shift 53)
    (279, Token (RBRACE _)) -> Just (Reduce 3 120)
    (279, Token (COMMA _)) -> Just (Reduce 3 120)
    (280, Token (COLON_COLON _)) -> Just (Shift 93)
    (281, Token (EXPORT _)) -> Just (Reduce 1 128)
    (281, Token (AS _)) -> Just (Reduce 1 128)
    (281, Token (QVARID _)) -> Just (Reduce 1 128)
    (281, Token (STRING _)) -> Just (Reduce 1 128)
    (282, Token (EXPORT _)) -> Just (Reduce 1 127)
    (282, Token (AS _)) -> Just (Reduce 1 127)
    (282, Token (QVARID _)) -> Just (Reduce 1 127)
    (282, Token (STRING _)) -> Just (Reduce 1 127)
    (283, Token (EXPORT _)) -> Just (Reduce 1 129)
    (283, Token (AS _)) -> Just (Reduce 1 129)
    (283, Token (QVARID _)) -> Just (Reduce 1 129)
    (283, Token (STRING _)) -> Just (Reduce 1 129)
    (284, Token (LPAREN _)) -> Just (Reduce 1 130)
    (284, Token (EXPORT _)) -> Just (Reduce 1 130)
    (284, Token (AS _)) -> Just (Reduce 1 130)
    (284, Token (QVARID _)) -> Just (Reduce 1 130)
    (284, Token (QVARSYM _)) -> Just (Reduce 1 130)
    (285, Token (STRING _)) -> Just (Reduce 1 133)
    (286, Token (STRING _)) -> Just (Reduce 1 132)
    (287, Token (STRING _)) -> Just (Reduce 1 134)
    (288, Token (LPAREN _)) -> Just (Reduce 1 131)
    (288, Token (EXPORT _)) -> Just (Reduce 1 131)
    (288, Token (AS _)) -> Just (Reduce 1 131)
    (288, Token (QVARID _)) -> Just (Reduce 1 131)
    (288, Token (QVARSYM _)) -> Just (Reduce 1 131)
    (289, Token (WHERE _)) -> Just (Reduce 1 138)
    (289, Token (RBRACE _)) -> Just (Reduce 1 138)
    (289, Token (SEMICOLON _)) -> Just (Reduce 1 138)
    (290, Token (WHERE _)) -> Just (Reduce 1 139)
    (290, Token (RBRACE _)) -> Just (Reduce 1 139)
    (290, Token (SEMICOLON _)) -> Just (Reduce 1 139)
    (291, Token (WHERE _)) -> Just (Reduce 1 141)
    (291, Token (RBRACE _)) -> Just (Reduce 1 141)
    (291, Token (LPAREN _)) -> Just (Reduce 1 141)
    (291, Token (SEMICOLON _)) -> Just (Reduce 1 141)
    (291, Token (QCONID _)) -> Just (Reduce 1 141)
    (291, Token (EXPORT _)) -> Just (Reduce 1 141)
    (291, Token (AS _)) -> Just (Reduce 1 141)
    (291, Token (QVARID _)) -> Just (Reduce 1 141)
    (291, Token (INTEGER _)) -> Just (Reduce 1 141)
    (291, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (291, Token (QCONSYM _)) -> Just (Reduce 1 141)
    (291, Token (BACKQUOTE _)) -> Just (Reduce 1 141)
    (292, Token (WHERE _)) -> Just (Reduce 3 143)
    (292, Token (RBRACE _)) -> Just (Reduce 3 143)
    (292, Token (LPAREN _)) -> Just (Reduce 3 143)
    (292, Token (SEMICOLON _)) -> Just (Reduce 3 143)
    (292, Token (QCONID _)) -> Just (Reduce 3 143)
    (292, Token (EXPORT _)) -> Just (Reduce 3 143)
    (292, Token (AS _)) -> Just (Reduce 3 143)
    (292, Token (QVARID _)) -> Just (Reduce 3 143)
    (292, Token (INTEGER _)) -> Just (Reduce 3 143)
    (292, Token (QVARSYM _)) -> Just (Reduce 3 143)
    (292, Token (QCONSYM _)) -> Just (Reduce 3 143)
    (292, Token (BACKQUOTE _)) -> Just (Reduce 3 143)
    (293, Token (WHERE _)) -> Just (Reduce 2 142)
    (293, Token (RBRACE _)) -> Just (Reduce 2 142)
    (293, Token (LPAREN _)) -> Just (Reduce 2 142)
    (293, Token (SEMICOLON _)) -> Just (Reduce 2 142)
    (293, Token (QCONID _)) -> Just (Reduce 2 142)
    (293, Token (EXPORT _)) -> Just (Reduce 2 142)
    (293, Token (AS _)) -> Just (Reduce 2 142)
    (293, Token (QVARID _)) -> Just (Reduce 2 142)
    (293, Token (INTEGER _)) -> Just (Reduce 2 142)
    (293, Token (QVARSYM _)) -> Just (Reduce 2 142)
    (293, Token (QCONSYM _)) -> Just (Reduce 2 142)
    (293, Token (BACKQUOTE _)) -> Just (Reduce 2 142)
    (294, Token (WHERE _)) -> Just (Reduce 1 145)
    (294, Token (RBRACE _)) -> Just (Reduce 1 145)
    (294, Token (LPAREN _)) -> Just (Reduce 1 145)
    (294, Token (SEMICOLON _)) -> Just (Reduce 1 145)
    (294, Token (QCONID _)) -> Just (Reduce 1 145)
    (294, Token (EXPORT _)) -> Just (Reduce 1 145)
    (294, Token (AS _)) -> Just (Reduce 1 145)
    (294, Token (QVARID _)) -> Just (Reduce 1 145)
    (294, Token (INTEGER _)) -> Just (Reduce 1 145)
    (294, Token (QVARSYM _)) -> Just (Reduce 1 145)
    (294, Token (QCONSYM _)) -> Just (Reduce 1 145)
    (294, Token (BACKQUOTE _)) -> Just (Reduce 1 145)
    (295, Token (WHERE _)) -> Just (Reduce 1 144)
    (295, Token (RBRACE _)) -> Just (Reduce 1 144)
    (295, Token (LPAREN _)) -> Just (Reduce 1 144)
    (295, Token (SEMICOLON _)) -> Just (Reduce 1 144)
    (295, Token (QCONID _)) -> Just (Reduce 1 144)
    (295, Token (EXPORT _)) -> Just (Reduce 1 144)
    (295, Token (AS _)) -> Just (Reduce 1 144)
    (295, Token (QVARID _)) -> Just (Reduce 1 144)
    (295, Token (INTEGER _)) -> Just (Reduce 1 144)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 144)
    (295, Token (QCONSYM _)) -> Just (Reduce 1 144)
    (295, Token (BACKQUOTE _)) -> Just (Reduce 1 144)
    (296, Token (LPAREN _)) -> Just (Reduce 1 148)
    (296, Token (EQUAL _)) -> Just (Reduce 1 148)
    (296, Token (QCONID _)) -> Just (Reduce 1 148)
    (296, Token (EXPORT _)) -> Just (Reduce 1 148)
    (296, Token (AS _)) -> Just (Reduce 1 148)
    (296, Token (QVARID _)) -> Just (Reduce 1 148)
    (296, Token (INTEGER _)) -> Just (Reduce 1 148)
    (296, Token (QVARSYM _)) -> Just (Reduce 1 148)
    (296, Token (QCONSYM _)) -> Just (Reduce 1 148)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 1 148)
    (297, Token (BACKQUOTE _)) -> Just (Shift 301)
    (298, Token (BACKQUOTE _)) -> Just (Shift 302)
    (299, Token (BACKQUOTE _)) -> Just (Shift 303)
    (300, Token (RBRACE _)) -> Just (Reduce 1 155)
    (300, Token (LPAREN _)) -> Just (Reduce 1 155)
    (300, Token (COMMA _)) -> Just (Reduce 1 155)
    (300, Token (SEMICOLON _)) -> Just (Reduce 1 155)
    (300, Token (EXPORT _)) -> Just (Reduce 1 155)
    (300, Token (AS _)) -> Just (Reduce 1 155)
    (300, Token (QVARID _)) -> Just (Reduce 1 155)
    (300, Token (INTEGER _)) -> Just (Reduce 1 155)
    (300, Token (QVARSYM _)) -> Just (Reduce 1 155)
    (301, Token (RBRACE _)) -> Just (Reduce 3 157)
    (301, Token (LPAREN _)) -> Just (Reduce 3 157)
    (301, Token (COMMA _)) -> Just (Reduce 3 157)
    (301, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (301, Token (EXPORT _)) -> Just (Reduce 3 157)
    (301, Token (AS _)) -> Just (Reduce 3 157)
    (301, Token (QVARID _)) -> Just (Reduce 3 157)
    (301, Token (INTEGER _)) -> Just (Reduce 3 157)
    (301, Token (QVARSYM _)) -> Just (Reduce 3 157)
    (302, Token (RBRACE _)) -> Just (Reduce 3 156)
    (302, Token (LPAREN _)) -> Just (Reduce 3 156)
    (302, Token (COMMA _)) -> Just (Reduce 3 156)
    (302, Token (SEMICOLON _)) -> Just (Reduce 3 156)
    (302, Token (EXPORT _)) -> Just (Reduce 3 156)
    (302, Token (AS _)) -> Just (Reduce 3 156)
    (302, Token (QVARID _)) -> Just (Reduce 3 156)
    (302, Token (INTEGER _)) -> Just (Reduce 3 156)
    (302, Token (QVARSYM _)) -> Just (Reduce 3 156)
    (303, Token (RBRACE _)) -> Just (Reduce 3 158)
    (303, Token (LPAREN _)) -> Just (Reduce 3 158)
    (303, Token (COMMA _)) -> Just (Reduce 3 158)
    (303, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (303, Token (EXPORT _)) -> Just (Reduce 3 158)
    (303, Token (AS _)) -> Just (Reduce 3 158)
    (303, Token (QVARID _)) -> Just (Reduce 3 158)
    (303, Token (INTEGER _)) -> Just (Reduce 3 158)
    (303, Token (QVARSYM _)) -> Just (Reduce 3 158)
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
production 62 = 23
production 63 = 23
production 64 = 33
production 65 = 34
production 66 = 34
production 67 = 35
production 68 = 35
production 69 = 24
production 70 = 24
production 71 = 36
production 72 = 37
production 73 = 37
production 74 = 38
production 75 = 38
production 76 = 30
production 77 = 30
production 78 = 30
production 79 = 30
production 80 = 42
production 81 = 42
production 82 = 39
production 83 = 39
production 84 = 40
production 85 = 40
production 86 = 40
production 87 = 25
production 88 = 25
production 89 = 18
production 90 = 18
production 91 = 17
production 92 = 17
production 93 = 44
production 94 = 44
production 95 = 44
production 96 = 44
production 97 = 44
production 98 = 44
production 99 = 47
production 100 = 47
production 101 = 45
production 102 = 45
production 103 = 45
production 104 = 45
production 105 = 45
production 106 = 48
production 107 = 48
production 108 = 19
production 109 = 19
production 110 = 49
production 111 = 49
production 112 = 50
production 113 = 50
production 114 = 50
production 115 = 50
production 116 = 22
production 117 = 22
production 118 = 52
production 119 = 52
production 120 = 53
production 121 = 21
production 122 = 21
production 123 = 20
production 124 = 26
production 125 = 26
production 126 = 26
production 127 = 54
production 128 = 54
production 129 = 54
production 130 = 55
production 131 = 57
production 132 = 56
production 133 = 56
production 134 = 56
production 135 = 58
production 136 = 32
production 137 = 32
production 138 = 59
production 139 = 60
production 140 = 61
production 141 = 62
production 142 = 62
production 143 = 62
production 144 = 63
production 145 = 63
production 146 = 31
production 147 = 31
production 148 = 64
production 149 = 8
production 150 = 8
production 151 = 8
production 152 = 8
production 153 = 9
production 154 = 9
production 155 = 65
production 156 = 65
production 157 = 65
production 158 = 65
production 159 = 51
production 160 = 51
production 161 = 43
production 162 = 43
production 163 = 16
production 164 = 16
production 165 = 15
production 166 = 15
production 167 = 46
production 168 = 46
production 169 = 46
production 170 = 66
production 171 = 1
production 172 = 41
production 173 = 41

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 148
    (9, 1) -> Just 173
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 232
    (13, 14) -> Just 18
    (13, 27) -> Just 171
    (13, 30) -> Just 203
    (13, 31) -> Just 41
    (13, 39) -> Just 212
    (13, 40) -> Just 213
    (13, 64) -> Just 216
    (16, 4) -> Just 17
    (16, 8) -> Just 232
    (16, 14) -> Just 18
    (16, 27) -> Just 171
    (16, 30) -> Just 203
    (16, 31) -> Just 41
    (16, 39) -> Just 212
    (16, 40) -> Just 213
    (16, 64) -> Just 216
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 122
    (25, 9) -> Just 123
    (25, 10) -> Just 33
    (25, 13) -> Just 112
    (34, 8) -> Just 295
    (34, 59) -> Just 219
    (34, 60) -> Just 289
    (34, 61) -> Just 290
    (34, 62) -> Just 35
    (34, 63) -> Just 291
    (35, 8) -> Just 295
    (35, 43) -> Just 36
    (35, 51) -> Just 241
    (35, 63) -> Just 293
    (35, 65) -> Just 242
    (36, 8) -> Just 295
    (36, 63) -> Just 292
    (37, 8) -> Just 232
    (37, 27) -> Just 208
    (37, 29) -> Just 207
    (37, 30) -> Just 203
    (37, 31) -> Just 41
    (37, 39) -> Just 212
    (37, 40) -> Just 213
    (37, 64) -> Just 216
    (38, 8) -> Just 232
    (38, 27) -> Just 208
    (38, 29) -> Just 209
    (38, 30) -> Just 203
    (38, 31) -> Just 41
    (38, 39) -> Just 212
    (38, 40) -> Just 213
    (38, 64) -> Just 216
    (39, 8) -> Just 232
    (39, 30) -> Just 224
    (39, 31) -> Just 44
    (39, 34) -> Just 221
    (39, 35) -> Just 223
    (39, 39) -> Just 212
    (39, 40) -> Just 213
    (39, 64) -> Just 216
    (40, 8) -> Just 232
    (40, 30) -> Just 224
    (40, 31) -> Just 44
    (40, 34) -> Just 222
    (40, 35) -> Just 223
    (40, 39) -> Just 212
    (40, 40) -> Just 213
    (40, 64) -> Just 216
    (41, 8) -> Just 296
    (41, 32) -> Just 204
    (41, 64) -> Just 217
    (42, 8) -> Just 296
    (42, 31) -> Just 45
    (42, 37) -> Just 227
    (42, 38) -> Just 229
    (42, 64) -> Just 216
    (43, 8) -> Just 296
    (43, 31) -> Just 45
    (43, 37) -> Just 228
    (43, 38) -> Just 229
    (43, 64) -> Just 216
    (44, 8) -> Just 296
    (44, 32) -> Just 225
    (44, 64) -> Just 217
    (45, 8) -> Just 296
    (45, 32) -> Just 230
    (45, 64) -> Just 217
    (46, 8) -> Just 119
    (46, 9) -> Just 120
    (46, 11) -> Just 113
    (46, 12) -> Just 114
    (47, 8) -> Just 119
    (47, 9) -> Just 120
    (47, 11) -> Just 149
    (47, 12) -> Just 114
    (48, 8) -> Just 119
    (48, 9) -> Just 120
    (48, 11) -> Just 150
    (48, 12) -> Just 114
    (49, 8) -> Just 122
    (49, 9) -> Just 123
    (49, 10) -> Just 111
    (49, 13) -> Just 112
    (50, 8) -> Just 122
    (50, 9) -> Just 123
    (50, 10) -> Just 121
    (50, 13) -> Just 112
    (51, 8) -> Just 231
    (51, 39) -> Just 233
    (52, 8) -> Just 231
    (52, 39) -> Just 280
    (52, 52) -> Just 271
    (52, 53) -> Just 278
    (53, 8) -> Just 231
    (53, 39) -> Just 280
    (53, 52) -> Just 277
    (53, 53) -> Just 278
    (54, 8) -> Just 183
    (55, 8) -> Just 194
    (56, 8) -> Just 195
    (57, 8) -> Just 196
    (65, 9) -> Just 257
    (65, 44) -> Just 248
    (65, 45) -> Just 249
    (65, 46) -> Just 250
    (66, 9) -> Just 257
    (66, 17) -> Just 67
    (66, 44) -> Just 174
    (66, 45) -> Just 249
    (66, 46) -> Just 250
    (67, 9) -> Just 257
    (67, 23) -> Just 166
    (67, 44) -> Just 175
    (67, 45) -> Just 249
    (67, 46) -> Just 250
    (68, 9) -> Just 257
    (68, 17) -> Just 69
    (68, 44) -> Just 174
    (68, 45) -> Just 249
    (68, 46) -> Just 250
    (69, 9) -> Just 257
    (69, 24) -> Just 168
    (69, 44) -> Just 175
    (69, 45) -> Just 249
    (69, 46) -> Just 250
    (70, 9) -> Just 257
    (70, 17) -> Just 71
    (70, 44) -> Just 174
    (70, 45) -> Just 249
    (70, 46) -> Just 250
    (71, 9) -> Just 257
    (71, 23) -> Just 165
    (71, 44) -> Just 175
    (71, 45) -> Just 249
    (71, 46) -> Just 250
    (72, 9) -> Just 257
    (72, 17) -> Just 73
    (72, 44) -> Just 174
    (72, 45) -> Just 249
    (72, 46) -> Just 250
    (73, 9) -> Just 257
    (73, 24) -> Just 167
    (73, 44) -> Just 175
    (73, 45) -> Just 249
    (73, 46) -> Just 250
    (74, 9) -> Just 257
    (74, 17) -> Just 75
    (74, 44) -> Just 174
    (74, 45) -> Just 249
    (74, 46) -> Just 250
    (75, 9) -> Just 257
    (75, 19) -> Just 153
    (75, 44) -> Just 175
    (75, 45) -> Just 249
    (75, 46) -> Just 250
    (76, 9) -> Just 257
    (76, 17) -> Just 77
    (76, 44) -> Just 174
    (76, 45) -> Just 249
    (76, 46) -> Just 250
    (77, 9) -> Just 257
    (77, 19) -> Just 154
    (77, 44) -> Just 175
    (77, 45) -> Just 249
    (77, 46) -> Just 250
    (78, 9) -> Just 258
    (78, 17) -> Just 81
    (78, 44) -> Just 174
    (78, 45) -> Just 249
    (78, 46) -> Just 250
    (78, 49) -> Just 177
    (78, 50) -> Just 268
    (79, 9) -> Just 258
    (79, 17) -> Just 81
    (79, 44) -> Just 174
    (79, 45) -> Just 249
    (79, 46) -> Just 250
    (79, 49) -> Just 267
    (79, 50) -> Just 268
    (80, 9) -> Just 95
    (81, 9) -> Just 257
    (81, 44) -> Just 175
    (81, 45) -> Just 249
    (81, 46) -> Just 250
    (81, 51) -> Just 82
    (82, 9) -> Just 257
    (82, 17) -> Just 83
    (82, 44) -> Just 174
    (82, 45) -> Just 249
    (82, 46) -> Just 250
    (83, 9) -> Just 257
    (83, 44) -> Just 175
    (83, 45) -> Just 249
    (83, 46) -> Just 250
    (84, 9) -> Just 257
    (84, 17) -> Just 85
    (84, 18) -> Just 211
    (84, 44) -> Just 174
    (84, 45) -> Just 249
    (84, 46) -> Just 250
    (85, 9) -> Just 257
    (85, 44) -> Just 175
    (85, 45) -> Just 249
    (85, 46) -> Just 250
    (86, 9) -> Just 257
    (86, 17) -> Just 92
    (86, 18) -> Just 152
    (86, 44) -> Just 174
    (86, 45) -> Just 249
    (86, 46) -> Just 250
    (87, 9) -> Just 257
    (87, 17) -> Just 92
    (87, 18) -> Just 176
    (87, 44) -> Just 174
    (87, 45) -> Just 249
    (87, 46) -> Just 250
    (88, 9) -> Just 257
    (88, 17) -> Just 92
    (88, 18) -> Just 197
    (88, 44) -> Just 174
    (88, 45) -> Just 249
    (88, 46) -> Just 250
    (89, 9) -> Just 257
    (89, 17) -> Just 92
    (89, 18) -> Just 198
    (89, 44) -> Just 174
    (89, 45) -> Just 249
    (89, 46) -> Just 250
    (90, 9) -> Just 257
    (90, 17) -> Just 92
    (90, 18) -> Just 199
    (90, 44) -> Just 174
    (90, 45) -> Just 249
    (90, 46) -> Just 250
    (91, 9) -> Just 257
    (91, 17) -> Just 92
    (91, 18) -> Just 210
    (91, 44) -> Just 174
    (91, 45) -> Just 249
    (91, 46) -> Just 250
    (92, 9) -> Just 257
    (92, 44) -> Just 175
    (92, 45) -> Just 249
    (92, 46) -> Just 250
    (93, 9) -> Just 257
    (93, 17) -> Just 92
    (93, 18) -> Just 279
    (93, 44) -> Just 174
    (93, 45) -> Just 249
    (93, 46) -> Just 250
    (94, 9) -> Just 257
    (94, 17) -> Just 92
    (94, 18) -> Just 184
    (94, 44) -> Just 174
    (94, 45) -> Just 249
    (94, 46) -> Just 250
    (95, 9) -> Just 257
    (95, 44) -> Just 185
    (95, 45) -> Just 249
    (95, 46) -> Just 250
    (96, 9) -> Just 257
    (96, 17) -> Just 97
    (96, 44) -> Just 174
    (96, 45) -> Just 249
    (96, 46) -> Just 250
    (97, 9) -> Just 257
    (97, 22) -> Just 164
    (97, 44) -> Just 175
    (97, 45) -> Just 249
    (97, 46) -> Just 250
    (98, 9) -> Just 257
    (98, 17) -> Just 100
    (98, 44) -> Just 174
    (98, 45) -> Just 249
    (98, 46) -> Just 250
    (99, 9) -> Just 257
    (99, 17) -> Just 101
    (99, 44) -> Just 174
    (99, 45) -> Just 249
    (99, 46) -> Just 250
    (100, 9) -> Just 257
    (100, 44) -> Just 175
    (100, 45) -> Just 249
    (100, 46) -> Just 250
    (101, 9) -> Just 257
    (101, 22) -> Just 163
    (101, 44) -> Just 175
    (101, 45) -> Just 249
    (101, 46) -> Just 250
    (102, 9) -> Just 257
    (102, 17) -> Just 92
    (102, 18) -> Just 246
    (102, 44) -> Just 174
    (102, 45) -> Just 249
    (102, 46) -> Just 250
    (102, 47) -> Just 251
    (102, 48) -> Just 259
    (103, 9) -> Just 257
    (103, 17) -> Just 92
    (103, 18) -> Just 190
    (103, 25) -> Just 169
    (103, 44) -> Just 174
    (103, 45) -> Just 249
    (103, 46) -> Just 250
    (104, 9) -> Just 257
    (104, 17) -> Just 92
    (104, 18) -> Just 190
    (104, 25) -> Just 191
    (104, 44) -> Just 174
    (104, 45) -> Just 249
    (104, 46) -> Just 250
    (105, 9) -> Just 257
    (105, 17) -> Just 92
    (105, 18) -> Just 263
    (105, 44) -> Just 174
    (105, 45) -> Just 249
    (105, 46) -> Just 250
    (105, 47) -> Just 264
    (106, 9) -> Just 257
    (106, 17) -> Just 92
    (106, 18) -> Just 247
    (106, 44) -> Just 174
    (106, 45) -> Just 249
    (106, 46) -> Just 250
    (124, 20) -> Just 180
    (124, 21) -> Just 159
    (125, 20) -> Just 180
    (125, 21) -> Just 160
    (126, 20) -> Just 180
    (126, 21) -> Just 161
    (127, 20) -> Just 180
    (127, 21) -> Just 162
    (140, 15) -> Just 8
    (142, 20) -> Just 155
    (143, 20) -> Just 156
    (144, 20) -> Just 157
    (145, 20) -> Just 158
    (147, 26) -> Just 170
    (148, 16) -> Just 151
    (178, 20) -> Just 180
    (178, 21) -> Just 181
    (186, 33) -> Just 187
    (188, 36) -> Just 189
    (192, 54) -> Just 200
    (193, 54) -> Just 201
    (200, 55) -> Just 55
    (200, 56) -> Just 202
    (201, 57) -> Just 57
    (202, 55) -> Just 56
    (205, 28) -> Just 218
    (213, 41) -> Just 214
    (214, 42) -> Just 215
    (214, 43) -> Just 240
    (214, 51) -> Just 241
    (214, 65) -> Just 242
    (238, 42) -> Just 239
    (238, 43) -> Just 240
    (238, 51) -> Just 241
    (238, 65) -> Just 242
    (265, 48) -> Just 266
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
                  Token (COLON_COLON semanticValue) ->
                    StackValue_COLON_COLON semanticValue
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
                  Token (PIPE semanticValue) ->
                    StackValue_PIPE semanticValue
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
                      Monad.liftM StackValue_decl $ decl_implies_pat_rhs actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    62 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies actions
                    63 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies_WHERE_cdecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls value -> value; _ -> undefined })
                    64 ->
                      Monad.liftM StackValue_cdecls $ cdecls_implies_LBRACE_cdecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cdecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    65 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl actions (case snd (pop !! 0) of { StackValue_cdecl value -> value; _ -> undefined })
                    66 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq actions (case snd (pop !! 2) of { StackValue_cdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecl_seq value -> value; _ -> undefined })
                    67 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    68 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_pat_rhs actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    69 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies actions
                    70 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies_WHERE_idecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls value -> value; _ -> undefined })
                    71 ->
                      Monad.liftM StackValue_idecls $ idecls_implies_LBRACE_idecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_idecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    72 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl actions (case snd (pop !! 0) of { StackValue_idecl value -> value; _ -> undefined })
                    73 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl_SEMICOLON_idecl_seq actions (case snd (pop !! 2) of { StackValue_idecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecl_seq value -> value; _ -> undefined })
                    74 ->
                      Monad.liftM StackValue_idecl $ idecl_implies actions
                    75 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_pat_rhs actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    76 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies actions
                    77 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    78 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    79 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_fixity_integer_opt_ops actions (case snd (pop !! 2) of { StackValue_fixity value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_integer_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    80 ->
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    81 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    82 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    83 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    84 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    85 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    86 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    87 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    88 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    89 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    90 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_RARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    91 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    94 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    98 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    101 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    109 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    112 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    118 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    120 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass_COMMA_dclass_seq actions (case snd (pop !! 2) of { StackValue_dclass value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass_seq value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_safety value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_expent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    128 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_impent $ impent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_expent $ expent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_safety $ safety_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_safety $ safety_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_safety $ safety_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    135 ->
                      Monad.liftM StackValue_lhs $ lhs_implies_pat actions (case snd (pop !! 0) of { StackValue_pat value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp_WHERE_decls actions (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    139 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    146 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    164 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    166 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    173 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
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
  , decl_implies_pat_rhs = \pat0 rhs1 ->
      return $ Decl_implies_pat_rhs pat0 rhs1
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
  , cdecl_implies_pat_rhs = \pat0 rhs1 ->
      return $ Cdecl_implies_pat_rhs pat0 rhs1
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
  , idecl_implies_pat_rhs = \pat0 rhs1 ->
      return $ Idecl_implies_pat_rhs pat0 rhs1
  , gendecl_implies =
      return $ Gendecl_implies
  , gendecl_implies_vars_COLON_COLON_type' = \vars0 cOLON_COLON1 type'2 ->
      return $ Gendecl_implies_vars_COLON_COLON_type' vars0 cOLON_COLON1 type'2
  , gendecl_implies_vars_COLON_COLON_btype_DARROW_type' = \vars0 cOLON_COLON1 btype2 dARROW3 type'4 ->
      return $ Gendecl_implies_vars_COLON_COLON_btype_DARROW_type' vars0 cOLON_COLON1 btype2 dARROW3 type'4
  , gendecl_implies_fixity_integer_opt_ops = \fixity0 integer_opt1 ops2 ->
      return $ Gendecl_implies_fixity_integer_opt_ops fixity0 integer_opt1 ops2
  , ops_implies_op = \op0 ->
      return $ Ops_implies_op op0
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
  , lhs_implies_pat = \pat0 ->
      return $ Lhs_implies_pat pat0
  , rhs_implies_EQUAL_exp = \eQUAL0 exp1 ->
      return $ Rhs_implies_EQUAL_exp eQUAL0 exp1
  , rhs_implies_EQUAL_exp_WHERE_decls = \eQUAL0 exp1 wHERE2 decls3 ->
      return $ Rhs_implies_EQUAL_exp_WHERE_decls eQUAL0 exp1 wHERE2 decls3
  , exp_implies_infixexp = \infixexp0 ->
      return $ Exp_implies_infixexp infixexp0
  , infixexp_implies_lexp = \lexp0 ->
      return $ Infixexp_implies_lexp lexp0
  , lexp_implies_fexp = \fexp0 ->
      return $ Lexp_implies_fexp fexp0
  , fexp_implies_aexp = \aexp0 ->
      return $ Fexp_implies_aexp aexp0
  , fexp_implies_fexp_aexp = \fexp0 aexp1 ->
      return $ Fexp_implies_fexp_aexp fexp0 aexp1
  , fexp_implies_fexp_op_aexp = \fexp0 op1 aexp2 ->
      return $ Fexp_implies_fexp_op_aexp fexp0 op1 aexp2
  , aexp_implies_var = \var0 ->
      return $ Aexp_implies_var var0
  , aexp_implies_INTEGER = \iNTEGER0 ->
      return $ Aexp_implies_INTEGER iNTEGER0
  , pat_implies_apat = \apat0 ->
      return $ Pat_implies_apat apat0
  , pat_implies_pat_apat = \pat0 apat1 ->
      return $ Pat_implies_pat_apat pat0 apat1
  , apat_implies_var = \var0 ->
      return $ Apat_implies_var var0
  , var_implies_AS = \aS0 ->
      return $ Var_implies_AS aS0
  , var_implies_EXPORT = \eXPORT0 ->
      return $ Var_implies_EXPORT eXPORT0
  , var_implies_QVARID = \qVARID0 ->
      return $ Var_implies_QVARID qVARID0
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
      return $ Integer_opt_implies_INTEGER iNTEGER0 }

