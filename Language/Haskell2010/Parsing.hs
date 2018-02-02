module  Language.Haskell2010.Parsing  where
import qualified Control.Monad as Monad


type Pos = (Int, Int)
type ARROW = Pos
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
    ARROW ARROW
  | AS AS
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
  | RBRACE RBRACE
  | RBRACKET RBRACKET
  | RPAREN RPAREN
  | SEMICOLON SEMICOLON
  | STRING STRING
  | TODO_FUNLHS TODO_FUNLHS
  | TODO_INST TODO_INST
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
  | Type'_implies_btype_ARROW_type' Btype ARROW Type'
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
  | Decl_implies_funlhs_rhs Funlhs Rhs
  | Decl_implies_var_rhs Var Rhs
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

data Funlhs =
    Funlhs_implies_TODO_FUNLHS TODO_FUNLHS
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
  | Cdecl_implies_funlhs_rhs Funlhs Rhs
  | Cdecl_implies_var_rhs Var Rhs
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
  | Idecl_implies_funlhs_rhs Funlhs Rhs
  | Idecl_implies_var_rhs Var Rhs
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
  | Gtycon_implies_LPAREN_ARROW_RPAREN LPAREN ARROW RPAREN
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

data Inst =
    Inst_implies_TODO_INST TODO_INST
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
  | StackValue_ARROW ARROW
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_EXCL EXCL
  | StackValue_PIPE PIPE
  | StackValue_QCONID QCONID
  | StackValue_TODO_INST TODO_INST
  | StackValue_EXPORT EXPORT
  | StackValue_AS AS
  | StackValue_QVARID QVARID
  | StackValue_STRING STRING
  | StackValue_TODO_FUNLHS TODO_FUNLHS
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
  | StackValue_funlhs Funlhs
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
  | StackValue_inst Inst
  | StackValue_callconv Callconv
  | StackValue_impent Impent
  | StackValue_safety Safety
  | StackValue_expent Expent
  | StackValue_exp Exp
  | StackValue_infixexp Infixexp
  | StackValue_lexp Lexp
  | StackValue_fexp Fexp
  | StackValue_aexp Aexp
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
  , decl_implies_funlhs_rhs :: Funlhs -> Rhs -> m Decl
  , decl_implies_var_rhs :: Var -> Rhs -> m Decl
  , cdecls_opt_implies :: m Cdecls_opt
  , cdecls_opt_implies_WHERE_cdecls :: WHERE -> Cdecls -> m Cdecls_opt
  , cdecls_implies_LBRACE_cdecl_seq_RBRACE :: LBRACE -> Cdecl_seq -> RBRACE -> m Cdecls
  , cdecl_seq_implies_cdecl :: Cdecl -> m Cdecl_seq
  , cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq :: Cdecl -> SEMICOLON -> Cdecl_seq -> m Cdecl_seq
  , cdecl_implies_gendecl :: Gendecl -> m Cdecl
  , cdecl_implies_funlhs_rhs :: Funlhs -> Rhs -> m Cdecl
  , cdecl_implies_var_rhs :: Var -> Rhs -> m Cdecl
  , idecls_opt_implies :: m Idecls_opt
  , idecls_opt_implies_WHERE_idecls :: WHERE -> Idecls -> m Idecls_opt
  , idecls_implies_LBRACE_idecl_seq_RBRACE :: LBRACE -> Idecl_seq -> RBRACE -> m Idecls
  , idecl_seq_implies_idecl :: Idecl -> m Idecl_seq
  , idecl_seq_implies_idecl_SEMICOLON_idecl_seq :: Idecl -> SEMICOLON -> Idecl_seq -> m Idecl_seq
  , idecl_implies :: m Idecl
  , idecl_implies_funlhs_rhs :: Funlhs -> Rhs -> m Idecl
  , idecl_implies_var_rhs :: Var -> Rhs -> m Idecl
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
  , type'_implies_btype_ARROW_type' :: Btype -> ARROW -> Type' -> m Type'
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
  , gtycon_implies_LPAREN_ARROW_RPAREN :: LPAREN -> ARROW -> RPAREN -> m Gtycon
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
  , inst_implies_TODO_INST :: TODO_INST -> m Inst
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
  , funlhs_implies_TODO_FUNLHS :: TODO_FUNLHS -> m Funlhs
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
    (11, Token (MODULE _)) -> Just (Reduce 1 172)
    (11, Token (WHERE _)) -> Just (Reduce 1 172)
    (11, Token (RBRACE _)) -> Just (Reduce 1 172)
    (11, Token (LPAREN _)) -> Just (Reduce 1 172)
    (11, Token (RPAREN _)) -> Just (Reduce 1 172)
    (11, Token (COMMA _)) -> Just (Reduce 1 172)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 172)
    (11, Token (HIDING _)) -> Just (Reduce 1 172)
    (11, Token (QCONID _)) -> Just (Reduce 1 172)
    (11, Token (EXPORT _)) -> Just (Reduce 1 172)
    (11, Token (AS _)) -> Just (Reduce 1 172)
    (11, Token (QVARID _)) -> Just (Reduce 1 172)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 172)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 172)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 79)
    (13, Token (LPAREN _)) -> Just (Shift 55)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (13, Token (IMPORT _)) -> Just (Shift 137)
    (13, Token (TYPE _)) -> Just (Shift 95)
    (13, Token (DATA _)) -> Just (Shift 71)
    (13, Token (NEWTYPE _)) -> Just (Shift 93)
    (13, Token (CLASS _)) -> Just (Shift 63)
    (13, Token (INSTANCE _)) -> Just (Shift 65)
    (13, Token (DEFAULT _)) -> Just (Shift 143)
    (13, Token (FOREIGN _)) -> Just (Shift 144)
    (13, Token (INFIXL _)) -> Just (Shift 238)
    (13, Token (INFIXR _)) -> Just (Shift 239)
    (13, Token (INFIX _)) -> Just (Shift 240)
    (13, Token (EXPORT _)) -> Just (Shift 58)
    (13, Token (AS _)) -> Just (Shift 59)
    (13, Token (QVARID _)) -> Just (Shift 60)
    (13, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 79)
    (16, Token (LPAREN _)) -> Just (Shift 55)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (16, Token (IMPORT _)) -> Just (Shift 137)
    (16, Token (TYPE _)) -> Just (Shift 95)
    (16, Token (DATA _)) -> Just (Shift 71)
    (16, Token (NEWTYPE _)) -> Just (Shift 93)
    (16, Token (CLASS _)) -> Just (Shift 63)
    (16, Token (INSTANCE _)) -> Just (Shift 65)
    (16, Token (DEFAULT _)) -> Just (Shift 143)
    (16, Token (FOREIGN _)) -> Just (Shift 144)
    (16, Token (INFIXL _)) -> Just (Shift 238)
    (16, Token (INFIXR _)) -> Just (Shift 239)
    (16, Token (INFIX _)) -> Just (Shift 240)
    (16, Token (EXPORT _)) -> Just (Shift 58)
    (16, Token (AS _)) -> Just (Shift 59)
    (16, Token (QVARID _)) -> Just (Shift 60)
    (16, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 56)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 106)
    (19, Token (EXPORT _)) -> Just (Shift 58)
    (19, Token (AS _)) -> Just (Shift 59)
    (19, Token (QVARID _)) -> Just (Shift 60)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 56)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 106)
    (22, Token (EXPORT _)) -> Just (Shift 58)
    (22, Token (AS _)) -> Just (Shift 59)
    (22, Token (QVARID _)) -> Just (Shift 60)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 56)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 106)
    (25, Token (EXPORT _)) -> Just (Shift 58)
    (25, Token (AS _)) -> Just (Shift 59)
    (25, Token (QVARID _)) -> Just (Shift 60)
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
    (34, Token (EXPORT _)) -> Just (Shift 58)
    (34, Token (AS _)) -> Just (Shift 59)
    (34, Token (QVARID _)) -> Just (Shift 60)
    (34, Token (INTEGER _)) -> Just (Shift 298)
    (35, Token (WHERE _)) -> Just (Reduce 1 144)
    (35, Token (RBRACE _)) -> Just (Reduce 1 144)
    (35, Token (LPAREN _)) -> Just (Shift 55)
    (35, Token (SEMICOLON _)) -> Just (Reduce 1 144)
    (35, Token (EXPORT _)) -> Just (Shift 58)
    (35, Token (AS _)) -> Just (Shift 59)
    (35, Token (QVARID _)) -> Just (Shift 60)
    (35, Token (INTEGER _)) -> Just (Shift 298)
    (35, Token (QVARSYM _)) -> Just (Shift 303)
    (35, Token (QCONSYM _)) -> Just (Shift 277)
    (35, Token (BACKQUOTE _)) -> Just (Shift 278)
    (36, Token (LPAREN _)) -> Just (Shift 55)
    (36, Token (EXPORT _)) -> Just (Shift 58)
    (36, Token (AS _)) -> Just (Shift 59)
    (36, Token (QVARID _)) -> Just (Shift 60)
    (36, Token (INTEGER _)) -> Just (Shift 298)
    (37, Token (RBRACE _)) -> Just (Reduce 0 79)
    (37, Token (LPAREN _)) -> Just (Shift 55)
    (37, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (37, Token (INFIXL _)) -> Just (Shift 238)
    (37, Token (INFIXR _)) -> Just (Shift 239)
    (37, Token (INFIX _)) -> Just (Shift 240)
    (37, Token (EXPORT _)) -> Just (Shift 58)
    (37, Token (AS _)) -> Just (Shift 59)
    (37, Token (QVARID _)) -> Just (Shift 60)
    (37, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (38, Token (RBRACE _)) -> Just (Reduce 0 79)
    (38, Token (LPAREN _)) -> Just (Shift 55)
    (38, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (38, Token (INFIXL _)) -> Just (Shift 238)
    (38, Token (INFIXR _)) -> Just (Shift 239)
    (38, Token (INFIX _)) -> Just (Shift 240)
    (38, Token (EXPORT _)) -> Just (Shift 58)
    (38, Token (AS _)) -> Just (Shift 59)
    (38, Token (QVARID _)) -> Just (Shift 60)
    (38, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (39, Token (RBRACE _)) -> Just (Reduce 0 79)
    (39, Token (LPAREN _)) -> Just (Shift 55)
    (39, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (39, Token (INFIXL _)) -> Just (Shift 238)
    (39, Token (INFIXR _)) -> Just (Shift 239)
    (39, Token (INFIX _)) -> Just (Shift 240)
    (39, Token (EXPORT _)) -> Just (Shift 58)
    (39, Token (AS _)) -> Just (Shift 59)
    (39, Token (QVARID _)) -> Just (Shift 60)
    (39, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (40, Token (RBRACE _)) -> Just (Reduce 0 79)
    (40, Token (LPAREN _)) -> Just (Shift 55)
    (40, Token (SEMICOLON _)) -> Just (Reduce 0 79)
    (40, Token (INFIXL _)) -> Just (Shift 238)
    (40, Token (INFIXR _)) -> Just (Shift 239)
    (40, Token (INFIX _)) -> Just (Shift 240)
    (40, Token (EXPORT _)) -> Just (Shift 58)
    (40, Token (AS _)) -> Just (Shift 59)
    (40, Token (QVARID _)) -> Just (Shift 60)
    (40, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (41, Token (RBRACE _)) -> Just (Reduce 0 76)
    (41, Token (LPAREN _)) -> Just (Shift 55)
    (41, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (41, Token (EXPORT _)) -> Just (Shift 58)
    (41, Token (AS _)) -> Just (Shift 59)
    (41, Token (QVARID _)) -> Just (Shift 60)
    (41, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (42, Token (RBRACE _)) -> Just (Reduce 0 76)
    (42, Token (LPAREN _)) -> Just (Shift 55)
    (42, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (42, Token (EXPORT _)) -> Just (Shift 58)
    (42, Token (AS _)) -> Just (Shift 59)
    (42, Token (QVARID _)) -> Just (Shift 60)
    (42, Token (TODO_FUNLHS _)) -> Just (Shift 216)
    (43, Token (LPAREN _)) -> Just (Shift 56)
    (43, Token (RPAREN _)) -> Just (Reduce 0 15)
    (43, Token (QCONID _)) -> Just (Shift 106)
    (43, Token (EXPORT _)) -> Just (Shift 58)
    (43, Token (AS _)) -> Just (Shift 59)
    (43, Token (QVARID _)) -> Just (Shift 60)
    (44, Token (LPAREN _)) -> Just (Shift 56)
    (44, Token (RPAREN _)) -> Just (Reduce 0 15)
    (44, Token (QCONID _)) -> Just (Shift 106)
    (44, Token (EXPORT _)) -> Just (Shift 58)
    (44, Token (AS _)) -> Just (Shift 59)
    (44, Token (QVARID _)) -> Just (Shift 60)
    (45, Token (LPAREN _)) -> Just (Shift 56)
    (45, Token (RPAREN _)) -> Just (Reduce 0 15)
    (45, Token (QCONID _)) -> Just (Shift 106)
    (45, Token (EXPORT _)) -> Just (Shift 58)
    (45, Token (AS _)) -> Just (Shift 59)
    (45, Token (QVARID _)) -> Just (Shift 60)
    (46, Token (LPAREN _)) -> Just (Shift 56)
    (46, Token (QCONID _)) -> Just (Shift 106)
    (46, Token (EXPORT _)) -> Just (Shift 58)
    (46, Token (AS _)) -> Just (Shift 59)
    (46, Token (QVARID _)) -> Just (Shift 60)
    (47, Token (LPAREN _)) -> Just (Shift 56)
    (47, Token (RPAREN _)) -> Just (Shift 112)
    (47, Token (DOT_DOT _)) -> Just (Shift 115)
    (47, Token (QCONID _)) -> Just (Shift 106)
    (47, Token (EXPORT _)) -> Just (Shift 58)
    (47, Token (AS _)) -> Just (Shift 59)
    (47, Token (QVARID _)) -> Just (Shift 60)
    (48, Token (LPAREN _)) -> Just (Shift 55)
    (48, Token (EXPORT _)) -> Just (Shift 58)
    (48, Token (AS _)) -> Just (Shift 59)
    (48, Token (QVARID _)) -> Just (Shift 60)
    (49, Token (RBRACE _)) -> Just (Shift 273)
    (49, Token (LPAREN _)) -> Just (Shift 55)
    (49, Token (EXPORT _)) -> Just (Shift 58)
    (49, Token (AS _)) -> Just (Shift 59)
    (49, Token (QVARID _)) -> Just (Shift 60)
    (50, Token (LPAREN _)) -> Just (Shift 55)
    (50, Token (EXPORT _)) -> Just (Shift 58)
    (50, Token (AS _)) -> Just (Shift 59)
    (50, Token (QVARID _)) -> Just (Shift 60)
    (51, Token (LPAREN _)) -> Just (Shift 55)
    (51, Token (EXPORT _)) -> Just (Shift 58)
    (51, Token (AS _)) -> Just (Shift 59)
    (51, Token (QVARID _)) -> Just (Shift 60)
    (52, Token (LPAREN _)) -> Just (Shift 55)
    (52, Token (EXPORT _)) -> Just (Shift 58)
    (52, Token (AS _)) -> Just (Shift 59)
    (52, Token (QVARID _)) -> Just (Shift 60)
    (53, Token (LPAREN _)) -> Just (Shift 55)
    (53, Token (EXPORT _)) -> Just (Shift 58)
    (53, Token (AS _)) -> Just (Shift 59)
    (53, Token (QVARID _)) -> Just (Shift 60)
    (54, Token (LPAREN _)) -> Just (Shift 55)
    (54, Token (EXPORT _)) -> Just (Shift 58)
    (54, Token (AS _)) -> Just (Shift 59)
    (54, Token (QVARID _)) -> Just (Shift 60)
    (55, Token (QVARSYM _)) -> Just (Shift 61)
    (56, Token (QVARSYM _)) -> Just (Shift 61)
    (56, Token (QCONSYM _)) -> Just (Shift 107)
    (57, Token (WHERE _)) -> Just (Reduce 3 153)
    (57, Token (RBRACE _)) -> Just (Reduce 3 153)
    (57, Token (LPAREN _)) -> Just (Reduce 3 153)
    (57, Token (RPAREN _)) -> Just (Reduce 3 153)
    (57, Token (COMMA _)) -> Just (Reduce 3 153)
    (57, Token (SEMICOLON _)) -> Just (Reduce 3 153)
    (57, Token (EQUAL _)) -> Just (Reduce 3 153)
    (57, Token (COLON_COLON _)) -> Just (Reduce 3 153)
    (57, Token (QCONID _)) -> Just (Reduce 3 153)
    (57, Token (EXPORT _)) -> Just (Reduce 3 153)
    (57, Token (AS _)) -> Just (Reduce 3 153)
    (57, Token (QVARID _)) -> Just (Reduce 3 153)
    (57, Token (INTEGER _)) -> Just (Reduce 3 153)
    (57, Token (QVARSYM _)) -> Just (Reduce 3 153)
    (57, Token (QCONSYM _)) -> Just (Reduce 3 153)
    (57, Token (BACKQUOTE _)) -> Just (Reduce 3 153)
    (58, Token (WHERE _)) -> Just (Reduce 1 151)
    (58, Token (RBRACE _)) -> Just (Reduce 1 151)
    (58, Token (LPAREN _)) -> Just (Reduce 1 151)
    (58, Token (RPAREN _)) -> Just (Reduce 1 151)
    (58, Token (COMMA _)) -> Just (Reduce 1 151)
    (58, Token (SEMICOLON _)) -> Just (Reduce 1 151)
    (58, Token (EQUAL _)) -> Just (Reduce 1 151)
    (58, Token (COLON_COLON _)) -> Just (Reduce 1 151)
    (58, Token (QCONID _)) -> Just (Reduce 1 151)
    (58, Token (EXPORT _)) -> Just (Reduce 1 151)
    (58, Token (AS _)) -> Just (Reduce 1 151)
    (58, Token (QVARID _)) -> Just (Reduce 1 151)
    (58, Token (INTEGER _)) -> Just (Reduce 1 151)
    (58, Token (QVARSYM _)) -> Just (Reduce 1 151)
    (58, Token (QCONSYM _)) -> Just (Reduce 1 151)
    (58, Token (BACKQUOTE _)) -> Just (Reduce 1 151)
    (59, Token (WHERE _)) -> Just (Reduce 1 150)
    (59, Token (RBRACE _)) -> Just (Reduce 1 150)
    (59, Token (LPAREN _)) -> Just (Reduce 1 150)
    (59, Token (RPAREN _)) -> Just (Reduce 1 150)
    (59, Token (COMMA _)) -> Just (Reduce 1 150)
    (59, Token (SEMICOLON _)) -> Just (Reduce 1 150)
    (59, Token (EQUAL _)) -> Just (Reduce 1 150)
    (59, Token (COLON_COLON _)) -> Just (Reduce 1 150)
    (59, Token (QCONID _)) -> Just (Reduce 1 150)
    (59, Token (EXPORT _)) -> Just (Reduce 1 150)
    (59, Token (AS _)) -> Just (Reduce 1 150)
    (59, Token (QVARID _)) -> Just (Reduce 1 150)
    (59, Token (INTEGER _)) -> Just (Reduce 1 150)
    (59, Token (QVARSYM _)) -> Just (Reduce 1 150)
    (59, Token (QCONSYM _)) -> Just (Reduce 1 150)
    (59, Token (BACKQUOTE _)) -> Just (Reduce 1 150)
    (60, Token (WHERE _)) -> Just (Reduce 1 152)
    (60, Token (RBRACE _)) -> Just (Reduce 1 152)
    (60, Token (LPAREN _)) -> Just (Reduce 1 152)
    (60, Token (RPAREN _)) -> Just (Reduce 1 152)
    (60, Token (COMMA _)) -> Just (Reduce 1 152)
    (60, Token (SEMICOLON _)) -> Just (Reduce 1 152)
    (60, Token (EQUAL _)) -> Just (Reduce 1 152)
    (60, Token (COLON_COLON _)) -> Just (Reduce 1 152)
    (60, Token (QCONID _)) -> Just (Reduce 1 152)
    (60, Token (EXPORT _)) -> Just (Reduce 1 152)
    (60, Token (AS _)) -> Just (Reduce 1 152)
    (60, Token (QVARID _)) -> Just (Reduce 1 152)
    (60, Token (INTEGER _)) -> Just (Reduce 1 152)
    (60, Token (QVARSYM _)) -> Just (Reduce 1 152)
    (60, Token (QCONSYM _)) -> Just (Reduce 1 152)
    (60, Token (BACKQUOTE _)) -> Just (Reduce 1 152)
    (61, Token (RPAREN _)) -> Just (Shift 57)
    (62, Token (LPAREN _)) -> Just (Shift 99)
    (62, Token (LBRACKET _)) -> Just (Shift 103)
    (62, Token (EXCL _)) -> Just (Shift 62)
    (62, Token (QCONID _)) -> Just (Shift 106)
    (62, Token (EXPORT _)) -> Just (Shift 264)
    (62, Token (AS _)) -> Just (Shift 265)
    (62, Token (QVARID _)) -> Just (Shift 266)
    (63, Token (LPAREN _)) -> Just (Shift 99)
    (63, Token (LBRACKET _)) -> Just (Shift 103)
    (63, Token (EXCL _)) -> Just (Shift 62)
    (63, Token (QCONID _)) -> Just (Shift 106)
    (63, Token (EXPORT _)) -> Just (Shift 264)
    (63, Token (AS _)) -> Just (Shift 265)
    (63, Token (QVARID _)) -> Just (Shift 266)
    (64, Token (WHERE _)) -> Just (Shift 183)
    (64, Token (RBRACE _)) -> Just (Reduce 0 63)
    (64, Token (LPAREN _)) -> Just (Shift 99)
    (64, Token (SEMICOLON _)) -> Just (Reduce 0 63)
    (64, Token (DARROW _)) -> Just (Shift 67)
    (64, Token (LBRACKET _)) -> Just (Shift 103)
    (64, Token (EXCL _)) -> Just (Shift 62)
    (64, Token (QCONID _)) -> Just (Shift 106)
    (64, Token (EXPORT _)) -> Just (Shift 264)
    (64, Token (AS _)) -> Just (Shift 265)
    (64, Token (QVARID _)) -> Just (Shift 266)
    (65, Token (LPAREN _)) -> Just (Shift 99)
    (65, Token (LBRACKET _)) -> Just (Shift 103)
    (65, Token (EXCL _)) -> Just (Shift 62)
    (65, Token (QCONID _)) -> Just (Shift 106)
    (65, Token (EXPORT _)) -> Just (Shift 264)
    (65, Token (AS _)) -> Just (Shift 265)
    (65, Token (QVARID _)) -> Just (Shift 266)
    (66, Token (WHERE _)) -> Just (Shift 185)
    (66, Token (RBRACE _)) -> Just (Reduce 0 71)
    (66, Token (LPAREN _)) -> Just (Shift 99)
    (66, Token (SEMICOLON _)) -> Just (Reduce 0 71)
    (66, Token (DARROW _)) -> Just (Shift 69)
    (66, Token (LBRACKET _)) -> Just (Shift 103)
    (66, Token (EXCL _)) -> Just (Shift 62)
    (66, Token (QCONID _)) -> Just (Shift 106)
    (66, Token (EXPORT _)) -> Just (Shift 264)
    (66, Token (AS _)) -> Just (Shift 265)
    (66, Token (QVARID _)) -> Just (Shift 266)
    (67, Token (LPAREN _)) -> Just (Shift 99)
    (67, Token (LBRACKET _)) -> Just (Shift 103)
    (67, Token (EXCL _)) -> Just (Shift 62)
    (67, Token (QCONID _)) -> Just (Shift 106)
    (67, Token (EXPORT _)) -> Just (Shift 264)
    (67, Token (AS _)) -> Just (Shift 265)
    (67, Token (QVARID _)) -> Just (Shift 266)
    (68, Token (WHERE _)) -> Just (Shift 183)
    (68, Token (RBRACE _)) -> Just (Reduce 0 63)
    (68, Token (LPAREN _)) -> Just (Shift 99)
    (68, Token (SEMICOLON _)) -> Just (Reduce 0 63)
    (68, Token (LBRACKET _)) -> Just (Shift 103)
    (68, Token (EXCL _)) -> Just (Shift 62)
    (68, Token (QCONID _)) -> Just (Shift 106)
    (68, Token (EXPORT _)) -> Just (Shift 264)
    (68, Token (AS _)) -> Just (Shift 265)
    (68, Token (QVARID _)) -> Just (Shift 266)
    (69, Token (LPAREN _)) -> Just (Shift 99)
    (69, Token (LBRACKET _)) -> Just (Shift 103)
    (69, Token (EXCL _)) -> Just (Shift 62)
    (69, Token (QCONID _)) -> Just (Shift 106)
    (69, Token (EXPORT _)) -> Just (Shift 264)
    (69, Token (AS _)) -> Just (Shift 265)
    (69, Token (QVARID _)) -> Just (Shift 266)
    (70, Token (WHERE _)) -> Just (Shift 185)
    (70, Token (RBRACE _)) -> Just (Reduce 0 71)
    (70, Token (LPAREN _)) -> Just (Shift 99)
    (70, Token (SEMICOLON _)) -> Just (Reduce 0 71)
    (70, Token (LBRACKET _)) -> Just (Shift 103)
    (70, Token (EXCL _)) -> Just (Shift 62)
    (70, Token (QCONID _)) -> Just (Shift 106)
    (70, Token (EXPORT _)) -> Just (Shift 264)
    (70, Token (AS _)) -> Just (Shift 265)
    (70, Token (QVARID _)) -> Just (Shift 266)
    (71, Token (LPAREN _)) -> Just (Shift 99)
    (71, Token (LBRACKET _)) -> Just (Shift 103)
    (71, Token (EXCL _)) -> Just (Shift 62)
    (71, Token (QCONID _)) -> Just (Shift 106)
    (71, Token (EXPORT _)) -> Just (Shift 264)
    (71, Token (AS _)) -> Just (Shift 265)
    (71, Token (QVARID _)) -> Just (Shift 266)
    (72, Token (RBRACE _)) -> Just (Reduce 0 111)
    (72, Token (LPAREN _)) -> Just (Shift 99)
    (72, Token (SEMICOLON _)) -> Just (Reduce 0 111)
    (72, Token (EQUAL _)) -> Just (Shift 75)
    (72, Token (DERIVING _)) -> Just (Reduce 0 111)
    (72, Token (DARROW _)) -> Just (Shift 73)
    (72, Token (LBRACKET _)) -> Just (Shift 103)
    (72, Token (EXCL _)) -> Just (Shift 62)
    (72, Token (QCONID _)) -> Just (Shift 106)
    (72, Token (EXPORT _)) -> Just (Shift 264)
    (72, Token (AS _)) -> Just (Shift 265)
    (72, Token (QVARID _)) -> Just (Shift 266)
    (73, Token (LPAREN _)) -> Just (Shift 99)
    (73, Token (LBRACKET _)) -> Just (Shift 103)
    (73, Token (EXCL _)) -> Just (Shift 62)
    (73, Token (QCONID _)) -> Just (Shift 106)
    (73, Token (EXPORT _)) -> Just (Shift 264)
    (73, Token (AS _)) -> Just (Shift 265)
    (73, Token (QVARID _)) -> Just (Shift 266)
    (74, Token (RBRACE _)) -> Just (Reduce 0 111)
    (74, Token (LPAREN _)) -> Just (Shift 99)
    (74, Token (SEMICOLON _)) -> Just (Reduce 0 111)
    (74, Token (EQUAL _)) -> Just (Shift 75)
    (74, Token (DERIVING _)) -> Just (Reduce 0 111)
    (74, Token (LBRACKET _)) -> Just (Shift 103)
    (74, Token (EXCL _)) -> Just (Shift 62)
    (74, Token (QCONID _)) -> Just (Shift 106)
    (74, Token (EXPORT _)) -> Just (Shift 264)
    (74, Token (AS _)) -> Just (Shift 265)
    (74, Token (QVARID _)) -> Just (Shift 266)
    (75, Token (LPAREN _)) -> Just (Shift 99)
    (75, Token (LBRACKET _)) -> Just (Shift 103)
    (75, Token (EXCL _)) -> Just (Shift 62)
    (75, Token (QCONID _)) -> Just (Shift 106)
    (75, Token (EXPORT _)) -> Just (Shift 264)
    (75, Token (AS _)) -> Just (Shift 265)
    (75, Token (QVARID _)) -> Just (Shift 266)
    (76, Token (LPAREN _)) -> Just (Shift 99)
    (76, Token (LBRACKET _)) -> Just (Shift 103)
    (76, Token (EXCL _)) -> Just (Shift 62)
    (76, Token (QCONID _)) -> Just (Shift 106)
    (76, Token (EXPORT _)) -> Just (Shift 264)
    (76, Token (AS _)) -> Just (Shift 265)
    (76, Token (QVARID _)) -> Just (Shift 266)
    (77, Token (LPAREN _)) -> Just (Shift 104)
    (77, Token (QCONID _)) -> Just (Shift 106)
    (78, Token (RBRACE _)) -> Just (Reduce 1 115)
    (78, Token (LPAREN _)) -> Just (Shift 99)
    (78, Token (SEMICOLON _)) -> Just (Reduce 1 115)
    (78, Token (DERIVING _)) -> Just (Reduce 1 115)
    (78, Token (LBRACKET _)) -> Just (Shift 103)
    (78, Token (EXCL _)) -> Just (Shift 62)
    (78, Token (PIPE _)) -> Just (Reduce 1 115)
    (78, Token (QCONID _)) -> Just (Shift 106)
    (78, Token (EXPORT _)) -> Just (Shift 264)
    (78, Token (AS _)) -> Just (Shift 265)
    (78, Token (QVARID _)) -> Just (Shift 266)
    (78, Token (QCONSYM _)) -> Just (Shift 277)
    (78, Token (BACKQUOTE _)) -> Just (Shift 279)
    (79, Token (LPAREN _)) -> Just (Shift 99)
    (79, Token (LBRACKET _)) -> Just (Shift 103)
    (79, Token (EXCL _)) -> Just (Shift 62)
    (79, Token (QCONID _)) -> Just (Shift 106)
    (79, Token (EXPORT _)) -> Just (Shift 264)
    (79, Token (AS _)) -> Just (Shift 265)
    (79, Token (QVARID _)) -> Just (Shift 266)
    (80, Token (RBRACE _)) -> Just (Reduce 3 116)
    (80, Token (LPAREN _)) -> Just (Shift 99)
    (80, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (80, Token (DERIVING _)) -> Just (Reduce 3 116)
    (80, Token (LBRACKET _)) -> Just (Shift 103)
    (80, Token (EXCL _)) -> Just (Shift 62)
    (80, Token (PIPE _)) -> Just (Reduce 3 116)
    (80, Token (QCONID _)) -> Just (Shift 106)
    (80, Token (EXPORT _)) -> Just (Shift 264)
    (80, Token (AS _)) -> Just (Shift 265)
    (80, Token (QVARID _)) -> Just (Shift 266)
    (81, Token (LPAREN _)) -> Just (Shift 99)
    (81, Token (LBRACKET _)) -> Just (Shift 103)
    (81, Token (EXCL _)) -> Just (Shift 62)
    (81, Token (QCONID _)) -> Just (Shift 106)
    (81, Token (EXPORT _)) -> Just (Shift 264)
    (81, Token (AS _)) -> Just (Shift 265)
    (81, Token (QVARID _)) -> Just (Shift 266)
    (82, Token (RBRACE _)) -> Just (Reduce 1 92)
    (82, Token (LPAREN _)) -> Just (Shift 99)
    (82, Token (SEMICOLON _)) -> Just (Reduce 1 92)
    (82, Token (DARROW _)) -> Just (Shift 88)
    (82, Token (ARROW _)) -> Just (Shift 84)
    (82, Token (LBRACKET _)) -> Just (Shift 103)
    (82, Token (EXCL _)) -> Just (Shift 62)
    (82, Token (QCONID _)) -> Just (Shift 106)
    (82, Token (EXPORT _)) -> Just (Shift 264)
    (82, Token (AS _)) -> Just (Shift 265)
    (82, Token (QVARID _)) -> Just (Shift 266)
    (83, Token (LPAREN _)) -> Just (Shift 99)
    (83, Token (LBRACKET _)) -> Just (Shift 103)
    (83, Token (EXCL _)) -> Just (Shift 62)
    (83, Token (QCONID _)) -> Just (Shift 106)
    (83, Token (EXPORT _)) -> Just (Shift 264)
    (83, Token (AS _)) -> Just (Shift 265)
    (83, Token (QVARID _)) -> Just (Shift 266)
    (84, Token (LPAREN _)) -> Just (Shift 99)
    (84, Token (LBRACKET _)) -> Just (Shift 103)
    (84, Token (EXCL _)) -> Just (Shift 62)
    (84, Token (QCONID _)) -> Just (Shift 106)
    (84, Token (EXPORT _)) -> Just (Shift 264)
    (84, Token (AS _)) -> Just (Shift 265)
    (84, Token (QVARID _)) -> Just (Shift 266)
    (85, Token (LPAREN _)) -> Just (Shift 99)
    (85, Token (LBRACKET _)) -> Just (Shift 103)
    (85, Token (EXCL _)) -> Just (Shift 62)
    (85, Token (QCONID _)) -> Just (Shift 106)
    (85, Token (EXPORT _)) -> Just (Shift 264)
    (85, Token (AS _)) -> Just (Shift 265)
    (85, Token (QVARID _)) -> Just (Shift 266)
    (86, Token (LPAREN _)) -> Just (Shift 99)
    (86, Token (LBRACKET _)) -> Just (Shift 103)
    (86, Token (EXCL _)) -> Just (Shift 62)
    (86, Token (QCONID _)) -> Just (Shift 106)
    (86, Token (EXPORT _)) -> Just (Shift 264)
    (86, Token (AS _)) -> Just (Shift 265)
    (86, Token (QVARID _)) -> Just (Shift 266)
    (87, Token (LPAREN _)) -> Just (Shift 99)
    (87, Token (LBRACKET _)) -> Just (Shift 103)
    (87, Token (EXCL _)) -> Just (Shift 62)
    (87, Token (QCONID _)) -> Just (Shift 106)
    (87, Token (EXPORT _)) -> Just (Shift 264)
    (87, Token (AS _)) -> Just (Shift 265)
    (87, Token (QVARID _)) -> Just (Shift 266)
    (88, Token (LPAREN _)) -> Just (Shift 99)
    (88, Token (LBRACKET _)) -> Just (Shift 103)
    (88, Token (EXCL _)) -> Just (Shift 62)
    (88, Token (QCONID _)) -> Just (Shift 106)
    (88, Token (EXPORT _)) -> Just (Shift 264)
    (88, Token (AS _)) -> Just (Shift 265)
    (88, Token (QVARID _)) -> Just (Shift 266)
    (89, Token (RBRACE _)) -> Just (Reduce 1 92)
    (89, Token (LPAREN _)) -> Just (Shift 99)
    (89, Token (RPAREN _)) -> Just (Reduce 1 92)
    (89, Token (COMMA _)) -> Just (Reduce 1 92)
    (89, Token (SEMICOLON _)) -> Just (Reduce 1 92)
    (89, Token (ARROW _)) -> Just (Shift 84)
    (89, Token (LBRACKET _)) -> Just (Shift 103)
    (89, Token (RBRACKET _)) -> Just (Reduce 1 92)
    (89, Token (EXCL _)) -> Just (Shift 62)
    (89, Token (QCONID _)) -> Just (Shift 106)
    (89, Token (EXPORT _)) -> Just (Shift 264)
    (89, Token (AS _)) -> Just (Shift 265)
    (89, Token (QVARID _)) -> Just (Shift 266)
    (90, Token (LPAREN _)) -> Just (Shift 99)
    (90, Token (LBRACKET _)) -> Just (Shift 103)
    (90, Token (EXCL _)) -> Just (Shift 62)
    (90, Token (QCONID _)) -> Just (Shift 106)
    (90, Token (EXPORT _)) -> Just (Shift 264)
    (90, Token (AS _)) -> Just (Shift 265)
    (90, Token (QVARID _)) -> Just (Shift 266)
    (91, Token (LPAREN _)) -> Just (Shift 99)
    (91, Token (LBRACKET _)) -> Just (Shift 103)
    (91, Token (EXCL _)) -> Just (Shift 62)
    (91, Token (QCONID _)) -> Just (Shift 106)
    (91, Token (EXPORT _)) -> Just (Shift 264)
    (91, Token (AS _)) -> Just (Shift 265)
    (91, Token (QVARID _)) -> Just (Shift 266)
    (92, Token (LBRACE _)) -> Just (Shift 51)
    (92, Token (LPAREN _)) -> Just (Shift 99)
    (92, Token (LBRACKET _)) -> Just (Shift 103)
    (92, Token (EXCL _)) -> Just (Shift 62)
    (92, Token (QCONID _)) -> Just (Shift 106)
    (92, Token (EXPORT _)) -> Just (Shift 264)
    (92, Token (AS _)) -> Just (Shift 265)
    (92, Token (QVARID _)) -> Just (Shift 266)
    (93, Token (LPAREN _)) -> Just (Shift 99)
    (93, Token (LBRACKET _)) -> Just (Shift 103)
    (93, Token (EXCL _)) -> Just (Shift 62)
    (93, Token (QCONID _)) -> Just (Shift 106)
    (93, Token (EXPORT _)) -> Just (Shift 264)
    (93, Token (AS _)) -> Just (Shift 265)
    (93, Token (QVARID _)) -> Just (Shift 266)
    (94, Token (LPAREN _)) -> Just (Shift 99)
    (94, Token (EQUAL _)) -> Just (Shift 77)
    (94, Token (DARROW _)) -> Just (Shift 96)
    (94, Token (LBRACKET _)) -> Just (Shift 103)
    (94, Token (EXCL _)) -> Just (Shift 62)
    (94, Token (QCONID _)) -> Just (Shift 106)
    (94, Token (EXPORT _)) -> Just (Shift 264)
    (94, Token (AS _)) -> Just (Shift 265)
    (94, Token (QVARID _)) -> Just (Shift 266)
    (95, Token (LPAREN _)) -> Just (Shift 99)
    (95, Token (LBRACKET _)) -> Just (Shift 103)
    (95, Token (EXCL _)) -> Just (Shift 62)
    (95, Token (QCONID _)) -> Just (Shift 106)
    (95, Token (EXPORT _)) -> Just (Shift 264)
    (95, Token (AS _)) -> Just (Shift 265)
    (95, Token (QVARID _)) -> Just (Shift 266)
    (96, Token (LPAREN _)) -> Just (Shift 99)
    (96, Token (LBRACKET _)) -> Just (Shift 103)
    (96, Token (EXCL _)) -> Just (Shift 62)
    (96, Token (QCONID _)) -> Just (Shift 106)
    (96, Token (EXPORT _)) -> Just (Shift 264)
    (96, Token (AS _)) -> Just (Shift 265)
    (96, Token (QVARID _)) -> Just (Shift 266)
    (97, Token (LPAREN _)) -> Just (Shift 99)
    (97, Token (EQUAL _)) -> Just (Shift 83)
    (97, Token (LBRACKET _)) -> Just (Shift 103)
    (97, Token (EXCL _)) -> Just (Shift 62)
    (97, Token (QCONID _)) -> Just (Shift 106)
    (97, Token (EXPORT _)) -> Just (Shift 264)
    (97, Token (AS _)) -> Just (Shift 265)
    (97, Token (QVARID _)) -> Just (Shift 266)
    (98, Token (LPAREN _)) -> Just (Shift 99)
    (98, Token (EQUAL _)) -> Just (Shift 77)
    (98, Token (LBRACKET _)) -> Just (Shift 103)
    (98, Token (EXCL _)) -> Just (Shift 62)
    (98, Token (QCONID _)) -> Just (Shift 106)
    (98, Token (EXPORT _)) -> Just (Shift 264)
    (98, Token (AS _)) -> Just (Shift 265)
    (98, Token (QVARID _)) -> Just (Shift 266)
    (99, Token (LPAREN _)) -> Just (Shift 99)
    (99, Token (RPAREN _)) -> Just (Shift 256)
    (99, Token (COMMA _)) -> Just (Shift 269)
    (99, Token (ARROW _)) -> Just (Shift 259)
    (99, Token (LBRACKET _)) -> Just (Shift 103)
    (99, Token (EXCL _)) -> Just (Shift 62)
    (99, Token (QCONID _)) -> Just (Shift 106)
    (99, Token (EXPORT _)) -> Just (Shift 264)
    (99, Token (AS _)) -> Just (Shift 265)
    (99, Token (QVARID _)) -> Just (Shift 266)
    (99, Token (QCONSYM _)) -> Just (Shift 107)
    (100, Token (LPAREN _)) -> Just (Shift 99)
    (100, Token (RPAREN _)) -> Just (Shift 129)
    (100, Token (LBRACKET _)) -> Just (Shift 103)
    (100, Token (EXCL _)) -> Just (Shift 62)
    (100, Token (QCONID _)) -> Just (Shift 106)
    (100, Token (EXPORT _)) -> Just (Shift 264)
    (100, Token (AS _)) -> Just (Shift 265)
    (100, Token (QVARID _)) -> Just (Shift 266)
    (101, Token (LPAREN _)) -> Just (Shift 99)
    (101, Token (LBRACKET _)) -> Just (Shift 103)
    (101, Token (EXCL _)) -> Just (Shift 62)
    (101, Token (QCONID _)) -> Just (Shift 106)
    (101, Token (EXPORT _)) -> Just (Shift 264)
    (101, Token (AS _)) -> Just (Shift 265)
    (101, Token (QVARID _)) -> Just (Shift 266)
    (102, Token (LPAREN _)) -> Just (Shift 99)
    (102, Token (LBRACKET _)) -> Just (Shift 103)
    (102, Token (EXCL _)) -> Just (Shift 62)
    (102, Token (QCONID _)) -> Just (Shift 106)
    (102, Token (EXPORT _)) -> Just (Shift 264)
    (102, Token (AS _)) -> Just (Shift 265)
    (102, Token (QVARID _)) -> Just (Shift 266)
    (103, Token (LPAREN _)) -> Just (Shift 99)
    (103, Token (LBRACKET _)) -> Just (Shift 103)
    (103, Token (RBRACKET _)) -> Just (Shift 260)
    (103, Token (EXCL _)) -> Just (Shift 62)
    (103, Token (QCONID _)) -> Just (Shift 106)
    (103, Token (EXPORT _)) -> Just (Shift 264)
    (103, Token (AS _)) -> Just (Shift 265)
    (103, Token (QVARID _)) -> Just (Shift 266)
    (104, Token (QCONSYM _)) -> Just (Shift 107)
    (105, Token (WHERE _)) -> Just (Reduce 3 155)
    (105, Token (LBRACE _)) -> Just (Reduce 3 155)
    (105, Token (RBRACE _)) -> Just (Reduce 3 155)
    (105, Token (LPAREN _)) -> Just (Reduce 3 155)
    (105, Token (RPAREN _)) -> Just (Reduce 3 155)
    (105, Token (COMMA _)) -> Just (Reduce 3 155)
    (105, Token (SEMICOLON _)) -> Just (Reduce 3 155)
    (105, Token (EQUAL _)) -> Just (Reduce 3 155)
    (105, Token (DERIVING _)) -> Just (Reduce 3 155)
    (105, Token (DARROW _)) -> Just (Reduce 3 155)
    (105, Token (COLON_COLON _)) -> Just (Reduce 3 155)
    (105, Token (INFIXL _)) -> Just (Reduce 3 155)
    (105, Token (INFIXR _)) -> Just (Reduce 3 155)
    (105, Token (INFIX _)) -> Just (Reduce 3 155)
    (105, Token (ARROW _)) -> Just (Reduce 3 155)
    (105, Token (LBRACKET _)) -> Just (Reduce 3 155)
    (105, Token (RBRACKET _)) -> Just (Reduce 3 155)
    (105, Token (EXCL _)) -> Just (Reduce 3 155)
    (105, Token (PIPE _)) -> Just (Reduce 3 155)
    (105, Token (QCONID _)) -> Just (Reduce 3 155)
    (105, Token (EXPORT _)) -> Just (Reduce 3 155)
    (105, Token (AS _)) -> Just (Reduce 3 155)
    (105, Token (QVARID _)) -> Just (Reduce 3 155)
    (105, Token (TODO_FUNLHS _)) -> Just (Reduce 3 155)
    (105, Token (INTEGER _)) -> Just (Reduce 3 155)
    (105, Token (QVARSYM _)) -> Just (Reduce 3 155)
    (105, Token (QCONSYM _)) -> Just (Reduce 3 155)
    (105, Token (BACKQUOTE _)) -> Just (Reduce 3 155)
    (106, Token (WHERE _)) -> Just (Reduce 1 154)
    (106, Token (LBRACE _)) -> Just (Reduce 1 154)
    (106, Token (RBRACE _)) -> Just (Reduce 1 154)
    (106, Token (LPAREN _)) -> Just (Reduce 1 154)
    (106, Token (RPAREN _)) -> Just (Reduce 1 154)
    (106, Token (COMMA _)) -> Just (Reduce 1 154)
    (106, Token (SEMICOLON _)) -> Just (Reduce 1 154)
    (106, Token (EQUAL _)) -> Just (Reduce 1 154)
    (106, Token (DERIVING _)) -> Just (Reduce 1 154)
    (106, Token (DARROW _)) -> Just (Reduce 1 154)
    (106, Token (COLON_COLON _)) -> Just (Reduce 1 154)
    (106, Token (INFIXL _)) -> Just (Reduce 1 154)
    (106, Token (INFIXR _)) -> Just (Reduce 1 154)
    (106, Token (INFIX _)) -> Just (Reduce 1 154)
    (106, Token (ARROW _)) -> Just (Reduce 1 154)
    (106, Token (LBRACKET _)) -> Just (Reduce 1 154)
    (106, Token (RBRACKET _)) -> Just (Reduce 1 154)
    (106, Token (EXCL _)) -> Just (Reduce 1 154)
    (106, Token (PIPE _)) -> Just (Reduce 1 154)
    (106, Token (QCONID _)) -> Just (Reduce 1 154)
    (106, Token (EXPORT _)) -> Just (Reduce 1 154)
    (106, Token (AS _)) -> Just (Reduce 1 154)
    (106, Token (QVARID _)) -> Just (Reduce 1 154)
    (106, Token (TODO_FUNLHS _)) -> Just (Reduce 1 154)
    (106, Token (INTEGER _)) -> Just (Reduce 1 154)
    (106, Token (QVARSYM _)) -> Just (Reduce 1 154)
    (106, Token (QCONSYM _)) -> Just (Reduce 1 154)
    (106, Token (BACKQUOTE _)) -> Just (Reduce 1 154)
    (107, Token (RPAREN _)) -> Just (Shift 105)
    (108, Token (RPAREN _)) -> Just (Reduce 3 24)
    (109, Token (RPAREN _)) -> Just (Reduce 1 23)
    (109, Token (COMMA _)) -> Just (Shift 46)
    (110, Token (RPAREN _)) -> Just (Reduce 3 17)
    (111, Token (RPAREN _)) -> Just (Reduce 1 16)
    (111, Token (COMMA _)) -> Just (Shift 43)
    (112, Token (RPAREN _)) -> Just (Reduce 3 20)
    (112, Token (COMMA _)) -> Just (Reduce 3 20)
    (113, Token (RPAREN _)) -> Just (Reduce 4 21)
    (113, Token (COMMA _)) -> Just (Reduce 4 21)
    (114, Token (RPAREN _)) -> Just (Reduce 4 22)
    (114, Token (COMMA _)) -> Just (Reduce 4 22)
    (115, Token (RPAREN _)) -> Just (Shift 113)
    (116, Token (RPAREN _)) -> Just (Reduce 1 18)
    (116, Token (COMMA _)) -> Just (Reduce 1 18)
    (117, Token (LPAREN _)) -> Just (Shift 47)
    (117, Token (RPAREN _)) -> Just (Reduce 1 19)
    (117, Token (COMMA _)) -> Just (Reduce 1 19)
    (118, Token (RPAREN _)) -> Just (Shift 114)
    (119, Token (RPAREN _)) -> Just (Reduce 1 25)
    (119, Token (COMMA _)) -> Just (Reduce 1 25)
    (120, Token (RPAREN _)) -> Just (Reduce 1 26)
    (120, Token (COMMA _)) -> Just (Reduce 1 26)
    (121, Token (RPAREN _)) -> Just (Shift 125)
    (121, Token (QCONID _)) -> Just (Shift 176)
    (122, Token (RPAREN _)) -> Just (Shift 126)
    (122, Token (QCONID _)) -> Just (Shift 176)
    (123, Token (RPAREN _)) -> Just (Shift 127)
    (123, Token (QCONID _)) -> Just (Shift 176)
    (124, Token (RPAREN _)) -> Just (Shift 128)
    (124, Token (QCONID _)) -> Just (Shift 176)
    (125, Token (RBRACE _)) -> Just (Reduce 6 35)
    (125, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (126, Token (RBRACE _)) -> Just (Reduce 8 39)
    (126, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (127, Token (RBRACE _)) -> Just (Reduce 8 47)
    (127, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (128, Token (RBRACE _)) -> Just (Reduce 6 43)
    (128, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (129, Token (RBRACE _)) -> Just (Reduce 3 53)
    (129, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (130, Token (RBRACE _)) -> Just (Reduce 8 31)
    (130, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (131, Token (RBRACE _)) -> Just (Reduce 7 30)
    (131, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (132, Token (RBRACE _)) -> Just (Reduce 7 36)
    (132, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (133, Token (RBRACE _)) -> Just (Reduce 9 40)
    (133, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (134, Token (RBRACE _)) -> Just (Reduce 9 48)
    (134, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (135, Token (RBRACE _)) -> Just (Reduce 7 44)
    (135, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (136, Token (RBRACE _)) -> Just (Reduce 4 54)
    (136, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (137, Token (QCONID _)) -> Just (Reduce 0 166)
    (137, Token (QUALIFIED _)) -> Just (Shift 169)
    (138, Token (LPAREN _)) -> Just (Shift 44)
    (139, Token (LPAREN _)) -> Just (Shift 121)
    (139, Token (QCONID _)) -> Just (Shift 176)
    (140, Token (LPAREN _)) -> Just (Shift 122)
    (140, Token (QCONID _)) -> Just (Shift 176)
    (141, Token (LPAREN _)) -> Just (Shift 123)
    (141, Token (QCONID _)) -> Just (Shift 176)
    (142, Token (LPAREN _)) -> Just (Shift 124)
    (142, Token (QCONID _)) -> Just (Shift 176)
    (143, Token (LPAREN _)) -> Just (Shift 100)
    (144, Token (IMPORT _)) -> Just (Shift 189)
    (144, Token (EXPORT _)) -> Just (Shift 190)
    (145, Token (RBRACE _)) -> Just (Reduce 0 164)
    (145, Token (LPAREN _)) -> Just (Reduce 0 164)
    (145, Token (SEMICOLON _)) -> Just (Reduce 0 164)
    (145, Token (HIDING _)) -> Just (Reduce 0 164)
    (145, Token (AS _)) -> Just (Shift 9)
    (146, Token (RPAREN _)) -> Just (Shift 130)
    (147, Token (RPAREN _)) -> Just (Shift 131)
    (148, Token (RBRACE _)) -> Just (Reduce 4 29)
    (148, Token (LPAREN _)) -> Just (Shift 45)
    (148, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (148, Token (HIDING _)) -> Just (Shift 138)
    (149, Token (RBRACE _)) -> Just (Reduce 4 32)
    (149, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (150, Token (RBRACE _)) -> Just (Reduce 3 33)
    (150, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (150, Token (DERIVING _)) -> Just (Shift 139)
    (151, Token (RBRACE _)) -> Just (Reduce 5 37)
    (151, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (151, Token (DERIVING _)) -> Just (Shift 140)
    (152, Token (RBRACE _)) -> Just (Reduce 5 34)
    (152, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (153, Token (RBRACE _)) -> Just (Reduce 7 38)
    (153, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (154, Token (RBRACE _)) -> Just (Reduce 7 46)
    (154, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (155, Token (RBRACE _)) -> Just (Reduce 5 42)
    (155, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (156, Token (RPAREN _)) -> Just (Shift 132)
    (157, Token (RPAREN _)) -> Just (Shift 133)
    (158, Token (RPAREN _)) -> Just (Shift 134)
    (159, Token (RPAREN _)) -> Just (Shift 135)
    (160, Token (RBRACE _)) -> Just (Reduce 5 45)
    (160, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (160, Token (DERIVING _)) -> Just (Shift 141)
    (161, Token (RBRACE _)) -> Just (Reduce 3 41)
    (161, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (161, Token (DERIVING _)) -> Just (Shift 142)
    (162, Token (RBRACE _)) -> Just (Reduce 5 50)
    (162, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (163, Token (RBRACE _)) -> Just (Reduce 3 49)
    (163, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (164, Token (RBRACE _)) -> Just (Reduce 5 52)
    (164, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (165, Token (RBRACE _)) -> Just (Reduce 3 51)
    (165, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (166, Token (RPAREN _)) -> Just (Shift 136)
    (167, Token (RBRACE _)) -> Just (Reduce 2 55)
    (167, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (168, Token (RBRACE _)) -> Just (Reduce 1 56)
    (168, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (169, Token (QCONID _)) -> Just (Reduce 1 167)
    (170, Token (RBRACE _)) -> Just (Reduce 2 165)
    (170, Token (LPAREN _)) -> Just (Reduce 2 165)
    (170, Token (SEMICOLON _)) -> Just (Reduce 2 165)
    (170, Token (HIDING _)) -> Just (Reduce 2 165)
    (171, Token (WHERE _)) -> Just (Reduce 1 94)
    (171, Token (LBRACE _)) -> Just (Reduce 1 94)
    (171, Token (RBRACE _)) -> Just (Reduce 1 94)
    (171, Token (LPAREN _)) -> Just (Reduce 1 94)
    (171, Token (RPAREN _)) -> Just (Reduce 1 94)
    (171, Token (COMMA _)) -> Just (Reduce 1 94)
    (171, Token (SEMICOLON _)) -> Just (Reduce 1 94)
    (171, Token (EQUAL _)) -> Just (Reduce 1 94)
    (171, Token (DERIVING _)) -> Just (Reduce 1 94)
    (171, Token (DARROW _)) -> Just (Reduce 1 94)
    (171, Token (COLON_COLON _)) -> Just (Reduce 1 94)
    (171, Token (INFIXL _)) -> Just (Reduce 1 94)
    (171, Token (INFIXR _)) -> Just (Reduce 1 94)
    (171, Token (INFIX _)) -> Just (Reduce 1 94)
    (171, Token (ARROW _)) -> Just (Reduce 1 94)
    (171, Token (LBRACKET _)) -> Just (Reduce 1 94)
    (171, Token (RBRACKET _)) -> Just (Reduce 1 94)
    (171, Token (EXCL _)) -> Just (Reduce 1 94)
    (171, Token (PIPE _)) -> Just (Reduce 1 94)
    (171, Token (QCONID _)) -> Just (Reduce 1 94)
    (171, Token (EXPORT _)) -> Just (Reduce 1 94)
    (171, Token (AS _)) -> Just (Reduce 1 94)
    (171, Token (QVARID _)) -> Just (Reduce 1 94)
    (171, Token (TODO_FUNLHS _)) -> Just (Reduce 1 94)
    (171, Token (INTEGER _)) -> Just (Reduce 1 94)
    (171, Token (QVARSYM _)) -> Just (Reduce 1 94)
    (171, Token (QCONSYM _)) -> Just (Reduce 1 94)
    (171, Token (BACKQUOTE _)) -> Just (Reduce 1 94)
    (172, Token (WHERE _)) -> Just (Reduce 2 95)
    (172, Token (LBRACE _)) -> Just (Reduce 2 95)
    (172, Token (RBRACE _)) -> Just (Reduce 2 95)
    (172, Token (LPAREN _)) -> Just (Reduce 2 95)
    (172, Token (RPAREN _)) -> Just (Reduce 2 95)
    (172, Token (COMMA _)) -> Just (Reduce 2 95)
    (172, Token (SEMICOLON _)) -> Just (Reduce 2 95)
    (172, Token (EQUAL _)) -> Just (Reduce 2 95)
    (172, Token (DERIVING _)) -> Just (Reduce 2 95)
    (172, Token (DARROW _)) -> Just (Reduce 2 95)
    (172, Token (COLON_COLON _)) -> Just (Reduce 2 95)
    (172, Token (INFIXL _)) -> Just (Reduce 2 95)
    (172, Token (INFIXR _)) -> Just (Reduce 2 95)
    (172, Token (INFIX _)) -> Just (Reduce 2 95)
    (172, Token (ARROW _)) -> Just (Reduce 2 95)
    (172, Token (LBRACKET _)) -> Just (Reduce 2 95)
    (172, Token (RBRACKET _)) -> Just (Reduce 2 95)
    (172, Token (EXCL _)) -> Just (Reduce 2 95)
    (172, Token (PIPE _)) -> Just (Reduce 2 95)
    (172, Token (QCONID _)) -> Just (Reduce 2 95)
    (172, Token (EXPORT _)) -> Just (Reduce 2 95)
    (172, Token (AS _)) -> Just (Reduce 2 95)
    (172, Token (QVARID _)) -> Just (Reduce 2 95)
    (172, Token (TODO_FUNLHS _)) -> Just (Reduce 2 95)
    (172, Token (INTEGER _)) -> Just (Reduce 2 95)
    (172, Token (QVARSYM _)) -> Just (Reduce 2 95)
    (172, Token (QCONSYM _)) -> Just (Reduce 2 95)
    (172, Token (BACKQUOTE _)) -> Just (Reduce 2 95)
    (173, Token (RBRACE _)) -> Just (Reduce 3 93)
    (173, Token (RPAREN _)) -> Just (Reduce 3 93)
    (173, Token (COMMA _)) -> Just (Reduce 3 93)
    (173, Token (SEMICOLON _)) -> Just (Reduce 3 93)
    (173, Token (RBRACKET _)) -> Just (Reduce 3 93)
    (174, Token (RBRACE _)) -> Just (Reduce 2 112)
    (174, Token (SEMICOLON _)) -> Just (Reduce 2 112)
    (174, Token (DERIVING _)) -> Just (Reduce 2 112)
    (175, Token (QCONID _)) -> Just (Shift 176)
    (176, Token (RBRACE _)) -> Just (Reduce 1 126)
    (176, Token (RPAREN _)) -> Just (Reduce 1 126)
    (176, Token (COMMA _)) -> Just (Reduce 1 126)
    (176, Token (SEMICOLON _)) -> Just (Reduce 1 126)
    (177, Token (RPAREN _)) -> Just (Reduce 1 124)
    (177, Token (COMMA _)) -> Just (Shift 175)
    (178, Token (RPAREN _)) -> Just (Reduce 3 125)
    (179, Token (RBRACE _)) -> Just (Reduce 7 120)
    (179, Token (SEMICOLON _)) -> Just (Reduce 7 120)
    (179, Token (DERIVING _)) -> Just (Reduce 7 120)
    (180, Token (COLON_COLON _)) -> Just (Shift 91)
    (181, Token (RBRACE _)) -> Just (Shift 179)
    (182, Token (RBRACE _)) -> Just (Reduce 3 119)
    (182, Token (SEMICOLON _)) -> Just (Reduce 3 119)
    (182, Token (DERIVING _)) -> Just (Reduce 3 119)
    (183, Token (LBRACE _)) -> Just (Shift 39)
    (184, Token (RBRACE _)) -> Just (Reduce 2 64)
    (184, Token (SEMICOLON _)) -> Just (Reduce 2 64)
    (185, Token (LBRACE _)) -> Just (Shift 41)
    (186, Token (RBRACE _)) -> Just (Reduce 2 72)
    (186, Token (SEMICOLON _)) -> Just (Reduce 2 72)
    (187, Token (RPAREN _)) -> Just (Reduce 1 90)
    (187, Token (COMMA _)) -> Just (Shift 101)
    (188, Token (RPAREN _)) -> Just (Reduce 3 91)
    (189, Token (EXPORT _)) -> Just (Shift 285)
    (189, Token (AS _)) -> Just (Shift 286)
    (189, Token (QVARID _)) -> Just (Shift 287)
    (190, Token (EXPORT _)) -> Just (Shift 285)
    (190, Token (AS _)) -> Just (Shift 286)
    (190, Token (QVARID _)) -> Just (Shift 287)
    (191, Token (COLON_COLON _)) -> Just (Shift 85)
    (192, Token (COLON_COLON _)) -> Just (Shift 86)
    (193, Token (COLON_COLON _)) -> Just (Shift 87)
    (194, Token (RBRACE _)) -> Just (Reduce 6 128)
    (194, Token (SEMICOLON _)) -> Just (Reduce 6 128)
    (195, Token (RBRACE _)) -> Just (Reduce 7 129)
    (195, Token (SEMICOLON _)) -> Just (Reduce 7 129)
    (196, Token (RBRACE _)) -> Just (Reduce 6 130)
    (196, Token (SEMICOLON _)) -> Just (Reduce 6 130)
    (197, Token (EXPORT _)) -> Just (Shift 289)
    (197, Token (AS _)) -> Just (Shift 290)
    (197, Token (QVARID _)) -> Just (Shift 291)
    (197, Token (STRING _)) -> Just (Shift 288)
    (198, Token (STRING _)) -> Just (Shift 292)
    (199, Token (STRING _)) -> Just (Shift 288)
    (200, Token (COMMA _)) -> Just (Shift 48)
    (200, Token (EQUAL _)) -> Just (Shift 34)
    (200, Token (COLON_COLON _)) -> Just (Reduce 1 85)
    (201, Token (RBRACE _)) -> Just (Reduce 1 60)
    (201, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (202, Token (EQUAL _)) -> Just (Shift 34)
    (203, Token (RBRACE _)) -> Just (Reduce 2 62)
    (203, Token (SEMICOLON _)) -> Just (Reduce 2 62)
    (204, Token (RBRACE _)) -> Just (Reduce 2 61)
    (204, Token (SEMICOLON _)) -> Just (Reduce 2 61)
    (205, Token (LBRACE _)) -> Just (Shift 37)
    (206, Token (RBRACE _)) -> Just (Reduce 3 57)
    (206, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (207, Token (RBRACE _)) -> Just (Shift 206)
    (208, Token (RBRACE _)) -> Just (Reduce 1 58)
    (208, Token (SEMICOLON _)) -> Just (Shift 38)
    (209, Token (RBRACE _)) -> Just (Reduce 3 59)
    (210, Token (RBRACE _)) -> Just (Reduce 5 81)
    (210, Token (SEMICOLON _)) -> Just (Reduce 5 81)
    (211, Token (RBRACE _)) -> Just (Reduce 3 80)
    (211, Token (SEMICOLON _)) -> Just (Reduce 3 80)
    (212, Token (COLON_COLON _)) -> Just (Shift 81)
    (213, Token (COMMA _)) -> Just (Reduce 0 173)
    (213, Token (QCONID _)) -> Just (Reduce 0 173)
    (213, Token (EXPORT _)) -> Just (Reduce 0 173)
    (213, Token (AS _)) -> Just (Reduce 0 173)
    (213, Token (QVARID _)) -> Just (Reduce 0 173)
    (213, Token (INTEGER _)) -> Just (Shift 241)
    (213, Token (QVARSYM _)) -> Just (Reduce 0 173)
    (213, Token (QCONSYM _)) -> Just (Reduce 0 173)
    (213, Token (BACKQUOTE _)) -> Just (Reduce 0 173)
    (214, Token (QVARSYM _)) -> Just (Shift 303)
    (214, Token (QCONSYM _)) -> Just (Shift 277)
    (214, Token (BACKQUOTE _)) -> Just (Shift 278)
    (215, Token (RBRACE _)) -> Just (Reduce 3 82)
    (215, Token (SEMICOLON _)) -> Just (Reduce 3 82)
    (216, Token (LPAREN _)) -> Just (Reduce 1 139)
    (216, Token (EQUAL _)) -> Just (Reduce 1 139)
    (216, Token (QCONID _)) -> Just (Reduce 1 139)
    (216, Token (EXPORT _)) -> Just (Reduce 1 139)
    (216, Token (AS _)) -> Just (Reduce 1 139)
    (216, Token (QVARID _)) -> Just (Reduce 1 139)
    (216, Token (INTEGER _)) -> Just (Reduce 1 139)
    (216, Token (QVARSYM _)) -> Just (Reduce 1 139)
    (216, Token (QCONSYM _)) -> Just (Reduce 1 139)
    (216, Token (BACKQUOTE _)) -> Just (Reduce 1 139)
    (217, Token (COMMA _)) -> Just (Shift 48)
    (217, Token (EQUAL _)) -> Just (Shift 34)
    (217, Token (COLON_COLON _)) -> Just (Reduce 1 85)
    (218, Token (EQUAL _)) -> Just (Shift 34)
    (219, Token (EQUAL _)) -> Just (Shift 34)
    (220, Token (EQUAL _)) -> Just (Shift 34)
    (221, Token (RBRACE _)) -> Just (Reduce 4 141)
    (221, Token (SEMICOLON _)) -> Just (Reduce 4 141)
    (222, Token (WHERE _)) -> Just (Shift 205)
    (222, Token (RBRACE _)) -> Just (Reduce 2 140)
    (222, Token (SEMICOLON _)) -> Just (Reduce 2 140)
    (223, Token (RBRACE _)) -> Just (Reduce 3 65)
    (223, Token (SEMICOLON _)) -> Just (Reduce 3 65)
    (224, Token (RBRACE _)) -> Just (Shift 223)
    (225, Token (RBRACE _)) -> Just (Reduce 3 67)
    (226, Token (RBRACE _)) -> Just (Reduce 1 66)
    (226, Token (SEMICOLON _)) -> Just (Shift 40)
    (227, Token (RBRACE _)) -> Just (Reduce 1 68)
    (227, Token (SEMICOLON _)) -> Just (Reduce 1 68)
    (228, Token (RBRACE _)) -> Just (Reduce 2 70)
    (228, Token (SEMICOLON _)) -> Just (Reduce 2 70)
    (229, Token (RBRACE _)) -> Just (Reduce 2 69)
    (229, Token (SEMICOLON _)) -> Just (Reduce 2 69)
    (230, Token (RBRACE _)) -> Just (Reduce 3 73)
    (230, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (231, Token (RBRACE _)) -> Just (Shift 230)
    (232, Token (RBRACE _)) -> Just (Reduce 3 75)
    (233, Token (RBRACE _)) -> Just (Reduce 1 74)
    (233, Token (SEMICOLON _)) -> Just (Shift 42)
    (234, Token (RBRACE _)) -> Just (Reduce 2 78)
    (234, Token (SEMICOLON _)) -> Just (Reduce 2 78)
    (235, Token (RBRACE _)) -> Just (Reduce 2 77)
    (235, Token (SEMICOLON _)) -> Just (Reduce 2 77)
    (236, Token (COMMA _)) -> Just (Shift 48)
    (236, Token (COLON_COLON _)) -> Just (Reduce 1 85)
    (237, Token (COLON_COLON _)) -> Just (Reduce 3 86)
    (238, Token (COMMA _)) -> Just (Reduce 1 87)
    (238, Token (QCONID _)) -> Just (Reduce 1 87)
    (238, Token (EXPORT _)) -> Just (Reduce 1 87)
    (238, Token (AS _)) -> Just (Reduce 1 87)
    (238, Token (QVARID _)) -> Just (Reduce 1 87)
    (238, Token (INTEGER _)) -> Just (Reduce 1 87)
    (238, Token (QVARSYM _)) -> Just (Reduce 1 87)
    (238, Token (QCONSYM _)) -> Just (Reduce 1 87)
    (238, Token (BACKQUOTE _)) -> Just (Reduce 1 87)
    (239, Token (COMMA _)) -> Just (Reduce 1 88)
    (239, Token (QCONID _)) -> Just (Reduce 1 88)
    (239, Token (EXPORT _)) -> Just (Reduce 1 88)
    (239, Token (AS _)) -> Just (Reduce 1 88)
    (239, Token (QVARID _)) -> Just (Reduce 1 88)
    (239, Token (INTEGER _)) -> Just (Reduce 1 88)
    (239, Token (QVARSYM _)) -> Just (Reduce 1 88)
    (239, Token (QCONSYM _)) -> Just (Reduce 1 88)
    (239, Token (BACKQUOTE _)) -> Just (Reduce 1 88)
    (240, Token (COMMA _)) -> Just (Reduce 1 89)
    (240, Token (QCONID _)) -> Just (Reduce 1 89)
    (240, Token (EXPORT _)) -> Just (Reduce 1 89)
    (240, Token (AS _)) -> Just (Reduce 1 89)
    (240, Token (QVARID _)) -> Just (Reduce 1 89)
    (240, Token (INTEGER _)) -> Just (Reduce 1 89)
    (240, Token (QVARSYM _)) -> Just (Reduce 1 89)
    (240, Token (QCONSYM _)) -> Just (Reduce 1 89)
    (240, Token (BACKQUOTE _)) -> Just (Reduce 1 89)
    (241, Token (COMMA _)) -> Just (Reduce 1 174)
    (241, Token (QCONID _)) -> Just (Reduce 1 174)
    (241, Token (EXPORT _)) -> Just (Reduce 1 174)
    (241, Token (AS _)) -> Just (Reduce 1 174)
    (241, Token (QVARID _)) -> Just (Reduce 1 174)
    (241, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (241, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (241, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (242, Token (QVARSYM _)) -> Just (Shift 303)
    (242, Token (QCONSYM _)) -> Just (Shift 277)
    (242, Token (BACKQUOTE _)) -> Just (Shift 278)
    (243, Token (RBRACE _)) -> Just (Reduce 3 84)
    (243, Token (SEMICOLON _)) -> Just (Reduce 3 84)
    (244, Token (RBRACE _)) -> Just (Reduce 1 83)
    (244, Token (COMMA _)) -> Just (Shift 242)
    (244, Token (SEMICOLON _)) -> Just (Reduce 1 83)
    (245, Token (RBRACE _)) -> Just (Reduce 1 163)
    (245, Token (LPAREN _)) -> Just (Reduce 1 163)
    (245, Token (COMMA _)) -> Just (Reduce 1 163)
    (245, Token (SEMICOLON _)) -> Just (Reduce 1 163)
    (245, Token (EXPORT _)) -> Just (Reduce 1 163)
    (245, Token (AS _)) -> Just (Reduce 1 163)
    (245, Token (QVARID _)) -> Just (Reduce 1 163)
    (245, Token (INTEGER _)) -> Just (Reduce 1 163)
    (245, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (246, Token (RBRACE _)) -> Just (Reduce 1 162)
    (246, Token (LPAREN _)) -> Just (Reduce 1 162)
    (246, Token (COMMA _)) -> Just (Reduce 1 162)
    (246, Token (SEMICOLON _)) -> Just (Reduce 1 162)
    (246, Token (EXPORT _)) -> Just (Reduce 1 162)
    (246, Token (AS _)) -> Just (Reduce 1 162)
    (246, Token (QVARID _)) -> Just (Reduce 1 162)
    (246, Token (INTEGER _)) -> Just (Reduce 1 162)
    (246, Token (QVARSYM _)) -> Just (Reduce 1 162)
    (247, Token (WHERE _)) -> Just (Reduce 3 100)
    (247, Token (LBRACE _)) -> Just (Reduce 3 100)
    (247, Token (RBRACE _)) -> Just (Reduce 3 100)
    (247, Token (LPAREN _)) -> Just (Reduce 3 100)
    (247, Token (RPAREN _)) -> Just (Reduce 3 100)
    (247, Token (COMMA _)) -> Just (Reduce 3 100)
    (247, Token (SEMICOLON _)) -> Just (Reduce 3 100)
    (247, Token (EQUAL _)) -> Just (Reduce 3 100)
    (247, Token (DERIVING _)) -> Just (Reduce 3 100)
    (247, Token (DARROW _)) -> Just (Reduce 3 100)
    (247, Token (COLON_COLON _)) -> Just (Reduce 3 100)
    (247, Token (INFIXL _)) -> Just (Reduce 3 100)
    (247, Token (INFIXR _)) -> Just (Reduce 3 100)
    (247, Token (INFIX _)) -> Just (Reduce 3 100)
    (247, Token (ARROW _)) -> Just (Reduce 3 100)
    (247, Token (LBRACKET _)) -> Just (Reduce 3 100)
    (247, Token (RBRACKET _)) -> Just (Reduce 3 100)
    (247, Token (EXCL _)) -> Just (Reduce 3 100)
    (247, Token (PIPE _)) -> Just (Reduce 3 100)
    (247, Token (QCONID _)) -> Just (Reduce 3 100)
    (247, Token (EXPORT _)) -> Just (Reduce 3 100)
    (247, Token (AS _)) -> Just (Reduce 3 100)
    (247, Token (QVARID _)) -> Just (Reduce 3 100)
    (247, Token (TODO_FUNLHS _)) -> Just (Reduce 3 100)
    (247, Token (INTEGER _)) -> Just (Reduce 3 100)
    (247, Token (QVARSYM _)) -> Just (Reduce 3 100)
    (247, Token (QCONSYM _)) -> Just (Reduce 3 100)
    (247, Token (BACKQUOTE _)) -> Just (Reduce 3 100)
    (248, Token (WHERE _)) -> Just (Reduce 3 98)
    (248, Token (LBRACE _)) -> Just (Reduce 3 98)
    (248, Token (RBRACE _)) -> Just (Reduce 3 98)
    (248, Token (LPAREN _)) -> Just (Reduce 3 98)
    (248, Token (RPAREN _)) -> Just (Reduce 3 98)
    (248, Token (COMMA _)) -> Just (Reduce 3 98)
    (248, Token (SEMICOLON _)) -> Just (Reduce 3 98)
    (248, Token (EQUAL _)) -> Just (Reduce 3 98)
    (248, Token (DERIVING _)) -> Just (Reduce 3 98)
    (248, Token (DARROW _)) -> Just (Reduce 3 98)
    (248, Token (COLON_COLON _)) -> Just (Reduce 3 98)
    (248, Token (INFIXL _)) -> Just (Reduce 3 98)
    (248, Token (INFIXR _)) -> Just (Reduce 3 98)
    (248, Token (INFIX _)) -> Just (Reduce 3 98)
    (248, Token (ARROW _)) -> Just (Reduce 3 98)
    (248, Token (LBRACKET _)) -> Just (Reduce 3 98)
    (248, Token (RBRACKET _)) -> Just (Reduce 3 98)
    (248, Token (EXCL _)) -> Just (Reduce 3 98)
    (248, Token (PIPE _)) -> Just (Reduce 3 98)
    (248, Token (QCONID _)) -> Just (Reduce 3 98)
    (248, Token (EXPORT _)) -> Just (Reduce 3 98)
    (248, Token (AS _)) -> Just (Reduce 3 98)
    (248, Token (QVARID _)) -> Just (Reduce 3 98)
    (248, Token (TODO_FUNLHS _)) -> Just (Reduce 3 98)
    (248, Token (INTEGER _)) -> Just (Reduce 3 98)
    (248, Token (QVARSYM _)) -> Just (Reduce 3 98)
    (248, Token (QCONSYM _)) -> Just (Reduce 3 98)
    (248, Token (BACKQUOTE _)) -> Just (Reduce 3 98)
    (249, Token (WHERE _)) -> Just (Reduce 3 99)
    (249, Token (LBRACE _)) -> Just (Reduce 3 99)
    (249, Token (RBRACE _)) -> Just (Reduce 3 99)
    (249, Token (LPAREN _)) -> Just (Reduce 3 99)
    (249, Token (RPAREN _)) -> Just (Reduce 3 99)
    (249, Token (COMMA _)) -> Just (Reduce 3 99)
    (249, Token (SEMICOLON _)) -> Just (Reduce 3 99)
    (249, Token (EQUAL _)) -> Just (Reduce 3 99)
    (249, Token (DERIVING _)) -> Just (Reduce 3 99)
    (249, Token (DARROW _)) -> Just (Reduce 3 99)
    (249, Token (COLON_COLON _)) -> Just (Reduce 3 99)
    (249, Token (INFIXL _)) -> Just (Reduce 3 99)
    (249, Token (INFIXR _)) -> Just (Reduce 3 99)
    (249, Token (INFIX _)) -> Just (Reduce 3 99)
    (249, Token (ARROW _)) -> Just (Reduce 3 99)
    (249, Token (LBRACKET _)) -> Just (Reduce 3 99)
    (249, Token (RBRACKET _)) -> Just (Reduce 3 99)
    (249, Token (EXCL _)) -> Just (Reduce 3 99)
    (249, Token (PIPE _)) -> Just (Reduce 3 99)
    (249, Token (QCONID _)) -> Just (Reduce 3 99)
    (249, Token (EXPORT _)) -> Just (Reduce 3 99)
    (249, Token (AS _)) -> Just (Reduce 3 99)
    (249, Token (QVARID _)) -> Just (Reduce 3 99)
    (249, Token (TODO_FUNLHS _)) -> Just (Reduce 3 99)
    (249, Token (INTEGER _)) -> Just (Reduce 3 99)
    (249, Token (QVARSYM _)) -> Just (Reduce 3 99)
    (249, Token (QCONSYM _)) -> Just (Reduce 3 99)
    (249, Token (BACKQUOTE _)) -> Just (Reduce 3 99)
    (250, Token (RPAREN _)) -> Just (Shift 247)
    (250, Token (COMMA _)) -> Just (Shift 102)
    (251, Token (RBRACKET _)) -> Just (Shift 249)
    (252, Token (WHERE _)) -> Just (Reduce 2 101)
    (252, Token (LBRACE _)) -> Just (Reduce 2 101)
    (252, Token (RBRACE _)) -> Just (Reduce 2 101)
    (252, Token (LPAREN _)) -> Just (Reduce 2 101)
    (252, Token (RPAREN _)) -> Just (Reduce 2 101)
    (252, Token (COMMA _)) -> Just (Reduce 2 101)
    (252, Token (SEMICOLON _)) -> Just (Reduce 2 101)
    (252, Token (EQUAL _)) -> Just (Reduce 2 101)
    (252, Token (DERIVING _)) -> Just (Reduce 2 101)
    (252, Token (DARROW _)) -> Just (Reduce 2 101)
    (252, Token (COLON_COLON _)) -> Just (Reduce 2 101)
    (252, Token (INFIXL _)) -> Just (Reduce 2 101)
    (252, Token (INFIXR _)) -> Just (Reduce 2 101)
    (252, Token (INFIX _)) -> Just (Reduce 2 101)
    (252, Token (ARROW _)) -> Just (Reduce 2 101)
    (252, Token (LBRACKET _)) -> Just (Reduce 2 101)
    (252, Token (RBRACKET _)) -> Just (Reduce 2 101)
    (252, Token (EXCL _)) -> Just (Reduce 2 101)
    (252, Token (PIPE _)) -> Just (Reduce 2 101)
    (252, Token (QCONID _)) -> Just (Reduce 2 101)
    (252, Token (EXPORT _)) -> Just (Reduce 2 101)
    (252, Token (AS _)) -> Just (Reduce 2 101)
    (252, Token (QVARID _)) -> Just (Reduce 2 101)
    (252, Token (TODO_FUNLHS _)) -> Just (Reduce 2 101)
    (252, Token (INTEGER _)) -> Just (Reduce 2 101)
    (252, Token (QVARSYM _)) -> Just (Reduce 2 101)
    (252, Token (QCONSYM _)) -> Just (Reduce 2 101)
    (252, Token (BACKQUOTE _)) -> Just (Reduce 2 101)
    (253, Token (WHERE _)) -> Just (Reduce 1 96)
    (253, Token (LBRACE _)) -> Just (Reduce 1 96)
    (253, Token (RBRACE _)) -> Just (Reduce 1 96)
    (253, Token (LPAREN _)) -> Just (Reduce 1 96)
    (253, Token (RPAREN _)) -> Just (Reduce 1 96)
    (253, Token (COMMA _)) -> Just (Reduce 1 96)
    (253, Token (SEMICOLON _)) -> Just (Reduce 1 96)
    (253, Token (EQUAL _)) -> Just (Reduce 1 96)
    (253, Token (DERIVING _)) -> Just (Reduce 1 96)
    (253, Token (DARROW _)) -> Just (Reduce 1 96)
    (253, Token (COLON_COLON _)) -> Just (Reduce 1 96)
    (253, Token (INFIXL _)) -> Just (Reduce 1 96)
    (253, Token (INFIXR _)) -> Just (Reduce 1 96)
    (253, Token (INFIX _)) -> Just (Reduce 1 96)
    (253, Token (ARROW _)) -> Just (Reduce 1 96)
    (253, Token (LBRACKET _)) -> Just (Reduce 1 96)
    (253, Token (RBRACKET _)) -> Just (Reduce 1 96)
    (253, Token (EXCL _)) -> Just (Reduce 1 96)
    (253, Token (PIPE _)) -> Just (Reduce 1 96)
    (253, Token (QCONID _)) -> Just (Reduce 1 96)
    (253, Token (EXPORT _)) -> Just (Reduce 1 96)
    (253, Token (AS _)) -> Just (Reduce 1 96)
    (253, Token (QVARID _)) -> Just (Reduce 1 96)
    (253, Token (TODO_FUNLHS _)) -> Just (Reduce 1 96)
    (253, Token (INTEGER _)) -> Just (Reduce 1 96)
    (253, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (253, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (253, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (254, Token (WHERE _)) -> Just (Reduce 1 97)
    (254, Token (LBRACE _)) -> Just (Reduce 1 97)
    (254, Token (RBRACE _)) -> Just (Reduce 1 97)
    (254, Token (LPAREN _)) -> Just (Reduce 1 97)
    (254, Token (RPAREN _)) -> Just (Reduce 1 97)
    (254, Token (COMMA _)) -> Just (Reduce 1 97)
    (254, Token (SEMICOLON _)) -> Just (Reduce 1 97)
    (254, Token (EQUAL _)) -> Just (Reduce 1 97)
    (254, Token (DERIVING _)) -> Just (Reduce 1 97)
    (254, Token (DARROW _)) -> Just (Reduce 1 97)
    (254, Token (COLON_COLON _)) -> Just (Reduce 1 97)
    (254, Token (INFIXL _)) -> Just (Reduce 1 97)
    (254, Token (INFIXR _)) -> Just (Reduce 1 97)
    (254, Token (INFIX _)) -> Just (Reduce 1 97)
    (254, Token (ARROW _)) -> Just (Reduce 1 97)
    (254, Token (LBRACKET _)) -> Just (Reduce 1 97)
    (254, Token (RBRACKET _)) -> Just (Reduce 1 97)
    (254, Token (EXCL _)) -> Just (Reduce 1 97)
    (254, Token (PIPE _)) -> Just (Reduce 1 97)
    (254, Token (QCONID _)) -> Just (Reduce 1 97)
    (254, Token (EXPORT _)) -> Just (Reduce 1 97)
    (254, Token (AS _)) -> Just (Reduce 1 97)
    (254, Token (QVARID _)) -> Just (Reduce 1 97)
    (254, Token (TODO_FUNLHS _)) -> Just (Reduce 1 97)
    (254, Token (INTEGER _)) -> Just (Reduce 1 97)
    (254, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (254, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (254, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (255, Token (RPAREN _)) -> Just (Shift 248)
    (256, Token (WHERE _)) -> Just (Reduce 2 105)
    (256, Token (LBRACE _)) -> Just (Reduce 2 105)
    (256, Token (RBRACE _)) -> Just (Reduce 2 105)
    (256, Token (LPAREN _)) -> Just (Reduce 2 105)
    (256, Token (RPAREN _)) -> Just (Reduce 2 105)
    (256, Token (COMMA _)) -> Just (Reduce 2 105)
    (256, Token (SEMICOLON _)) -> Just (Reduce 2 105)
    (256, Token (EQUAL _)) -> Just (Reduce 2 105)
    (256, Token (DERIVING _)) -> Just (Reduce 2 105)
    (256, Token (DARROW _)) -> Just (Reduce 2 105)
    (256, Token (COLON_COLON _)) -> Just (Reduce 2 105)
    (256, Token (INFIXL _)) -> Just (Reduce 2 105)
    (256, Token (INFIXR _)) -> Just (Reduce 2 105)
    (256, Token (INFIX _)) -> Just (Reduce 2 105)
    (256, Token (ARROW _)) -> Just (Reduce 2 105)
    (256, Token (LBRACKET _)) -> Just (Reduce 2 105)
    (256, Token (RBRACKET _)) -> Just (Reduce 2 105)
    (256, Token (EXCL _)) -> Just (Reduce 2 105)
    (256, Token (PIPE _)) -> Just (Reduce 2 105)
    (256, Token (QCONID _)) -> Just (Reduce 2 105)
    (256, Token (EXPORT _)) -> Just (Reduce 2 105)
    (256, Token (AS _)) -> Just (Reduce 2 105)
    (256, Token (QVARID _)) -> Just (Reduce 2 105)
    (256, Token (TODO_FUNLHS _)) -> Just (Reduce 2 105)
    (256, Token (INTEGER _)) -> Just (Reduce 2 105)
    (256, Token (QVARSYM _)) -> Just (Reduce 2 105)
    (256, Token (QCONSYM _)) -> Just (Reduce 2 105)
    (256, Token (BACKQUOTE _)) -> Just (Reduce 2 105)
    (257, Token (WHERE _)) -> Just (Reduce 3 107)
    (257, Token (LBRACE _)) -> Just (Reduce 3 107)
    (257, Token (RBRACE _)) -> Just (Reduce 3 107)
    (257, Token (LPAREN _)) -> Just (Reduce 3 107)
    (257, Token (RPAREN _)) -> Just (Reduce 3 107)
    (257, Token (COMMA _)) -> Just (Reduce 3 107)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (257, Token (EQUAL _)) -> Just (Reduce 3 107)
    (257, Token (DERIVING _)) -> Just (Reduce 3 107)
    (257, Token (DARROW _)) -> Just (Reduce 3 107)
    (257, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (257, Token (INFIXL _)) -> Just (Reduce 3 107)
    (257, Token (INFIXR _)) -> Just (Reduce 3 107)
    (257, Token (INFIX _)) -> Just (Reduce 3 107)
    (257, Token (ARROW _)) -> Just (Reduce 3 107)
    (257, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (257, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (257, Token (EXCL _)) -> Just (Reduce 3 107)
    (257, Token (PIPE _)) -> Just (Reduce 3 107)
    (257, Token (QCONID _)) -> Just (Reduce 3 107)
    (257, Token (EXPORT _)) -> Just (Reduce 3 107)
    (257, Token (AS _)) -> Just (Reduce 3 107)
    (257, Token (QVARID _)) -> Just (Reduce 3 107)
    (257, Token (TODO_FUNLHS _)) -> Just (Reduce 3 107)
    (257, Token (INTEGER _)) -> Just (Reduce 3 107)
    (257, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (257, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (257, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (258, Token (WHERE _)) -> Just (Reduce 3 108)
    (258, Token (LBRACE _)) -> Just (Reduce 3 108)
    (258, Token (RBRACE _)) -> Just (Reduce 3 108)
    (258, Token (LPAREN _)) -> Just (Reduce 3 108)
    (258, Token (RPAREN _)) -> Just (Reduce 3 108)
    (258, Token (COMMA _)) -> Just (Reduce 3 108)
    (258, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (258, Token (EQUAL _)) -> Just (Reduce 3 108)
    (258, Token (DERIVING _)) -> Just (Reduce 3 108)
    (258, Token (DARROW _)) -> Just (Reduce 3 108)
    (258, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (258, Token (INFIXL _)) -> Just (Reduce 3 108)
    (258, Token (INFIXR _)) -> Just (Reduce 3 108)
    (258, Token (INFIX _)) -> Just (Reduce 3 108)
    (258, Token (ARROW _)) -> Just (Reduce 3 108)
    (258, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (258, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (258, Token (EXCL _)) -> Just (Reduce 3 108)
    (258, Token (PIPE _)) -> Just (Reduce 3 108)
    (258, Token (QCONID _)) -> Just (Reduce 3 108)
    (258, Token (EXPORT _)) -> Just (Reduce 3 108)
    (258, Token (AS _)) -> Just (Reduce 3 108)
    (258, Token (QVARID _)) -> Just (Reduce 3 108)
    (258, Token (TODO_FUNLHS _)) -> Just (Reduce 3 108)
    (258, Token (INTEGER _)) -> Just (Reduce 3 108)
    (258, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (258, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (258, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (259, Token (RPAREN _)) -> Just (Shift 257)
    (260, Token (WHERE _)) -> Just (Reduce 2 106)
    (260, Token (LBRACE _)) -> Just (Reduce 2 106)
    (260, Token (RBRACE _)) -> Just (Reduce 2 106)
    (260, Token (LPAREN _)) -> Just (Reduce 2 106)
    (260, Token (RPAREN _)) -> Just (Reduce 2 106)
    (260, Token (COMMA _)) -> Just (Reduce 2 106)
    (260, Token (SEMICOLON _)) -> Just (Reduce 2 106)
    (260, Token (EQUAL _)) -> Just (Reduce 2 106)
    (260, Token (DERIVING _)) -> Just (Reduce 2 106)
    (260, Token (DARROW _)) -> Just (Reduce 2 106)
    (260, Token (COLON_COLON _)) -> Just (Reduce 2 106)
    (260, Token (INFIXL _)) -> Just (Reduce 2 106)
    (260, Token (INFIXR _)) -> Just (Reduce 2 106)
    (260, Token (INFIX _)) -> Just (Reduce 2 106)
    (260, Token (ARROW _)) -> Just (Reduce 2 106)
    (260, Token (LBRACKET _)) -> Just (Reduce 2 106)
    (260, Token (RBRACKET _)) -> Just (Reduce 2 106)
    (260, Token (EXCL _)) -> Just (Reduce 2 106)
    (260, Token (PIPE _)) -> Just (Reduce 2 106)
    (260, Token (QCONID _)) -> Just (Reduce 2 106)
    (260, Token (EXPORT _)) -> Just (Reduce 2 106)
    (260, Token (AS _)) -> Just (Reduce 2 106)
    (260, Token (QVARID _)) -> Just (Reduce 2 106)
    (260, Token (TODO_FUNLHS _)) -> Just (Reduce 2 106)
    (260, Token (INTEGER _)) -> Just (Reduce 2 106)
    (260, Token (QVARSYM _)) -> Just (Reduce 2 106)
    (260, Token (QCONSYM _)) -> Just (Reduce 2 106)
    (260, Token (BACKQUOTE _)) -> Just (Reduce 2 106)
    (261, Token (WHERE _)) -> Just (Reduce 1 104)
    (261, Token (LBRACE _)) -> Just (Reduce 1 104)
    (261, Token (RBRACE _)) -> Just (Reduce 1 104)
    (261, Token (LPAREN _)) -> Just (Reduce 1 104)
    (261, Token (RPAREN _)) -> Just (Reduce 1 104)
    (261, Token (COMMA _)) -> Just (Reduce 1 104)
    (261, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (261, Token (EQUAL _)) -> Just (Reduce 1 104)
    (261, Token (DERIVING _)) -> Just (Reduce 1 104)
    (261, Token (DARROW _)) -> Just (Reduce 1 104)
    (261, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (261, Token (INFIXL _)) -> Just (Reduce 1 104)
    (261, Token (INFIXR _)) -> Just (Reduce 1 104)
    (261, Token (INFIX _)) -> Just (Reduce 1 104)
    (261, Token (ARROW _)) -> Just (Reduce 1 104)
    (261, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (261, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (261, Token (EXCL _)) -> Just (Reduce 1 104)
    (261, Token (PIPE _)) -> Just (Reduce 1 104)
    (261, Token (QCONID _)) -> Just (Reduce 1 104)
    (261, Token (EXPORT _)) -> Just (Reduce 1 104)
    (261, Token (AS _)) -> Just (Reduce 1 104)
    (261, Token (QVARID _)) -> Just (Reduce 1 104)
    (261, Token (TODO_FUNLHS _)) -> Just (Reduce 1 104)
    (261, Token (INTEGER _)) -> Just (Reduce 1 104)
    (261, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (261, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (261, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (262, Token (LBRACE _)) -> Just (Shift 49)
    (262, Token (RBRACE _)) -> Just (Reduce 1 104)
    (262, Token (LPAREN _)) -> Just (Reduce 1 104)
    (262, Token (RPAREN _)) -> Just (Reduce 1 104)
    (262, Token (COMMA _)) -> Just (Reduce 1 104)
    (262, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (262, Token (DERIVING _)) -> Just (Reduce 1 104)
    (262, Token (ARROW _)) -> Just (Reduce 1 104)
    (262, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (262, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (262, Token (EXCL _)) -> Just (Reduce 1 104)
    (262, Token (PIPE _)) -> Just (Reduce 1 104)
    (262, Token (QCONID _)) -> Just (Reduce 1 104)
    (262, Token (EXPORT _)) -> Just (Reduce 1 104)
    (262, Token (AS _)) -> Just (Reduce 1 104)
    (262, Token (QVARID _)) -> Just (Reduce 1 104)
    (262, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (262, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (263, Token (RPAREN _)) -> Just (Shift 258)
    (264, Token (WHERE _)) -> Just (Reduce 1 169)
    (264, Token (LBRACE _)) -> Just (Reduce 1 169)
    (264, Token (RBRACE _)) -> Just (Reduce 1 169)
    (264, Token (LPAREN _)) -> Just (Reduce 1 169)
    (264, Token (RPAREN _)) -> Just (Reduce 1 169)
    (264, Token (COMMA _)) -> Just (Reduce 1 169)
    (264, Token (SEMICOLON _)) -> Just (Reduce 1 169)
    (264, Token (EQUAL _)) -> Just (Reduce 1 169)
    (264, Token (DERIVING _)) -> Just (Reduce 1 169)
    (264, Token (DARROW _)) -> Just (Reduce 1 169)
    (264, Token (COLON_COLON _)) -> Just (Reduce 1 169)
    (264, Token (INFIXL _)) -> Just (Reduce 1 169)
    (264, Token (INFIXR _)) -> Just (Reduce 1 169)
    (264, Token (INFIX _)) -> Just (Reduce 1 169)
    (264, Token (ARROW _)) -> Just (Reduce 1 169)
    (264, Token (LBRACKET _)) -> Just (Reduce 1 169)
    (264, Token (RBRACKET _)) -> Just (Reduce 1 169)
    (264, Token (EXCL _)) -> Just (Reduce 1 169)
    (264, Token (PIPE _)) -> Just (Reduce 1 169)
    (264, Token (QCONID _)) -> Just (Reduce 1 169)
    (264, Token (EXPORT _)) -> Just (Reduce 1 169)
    (264, Token (AS _)) -> Just (Reduce 1 169)
    (264, Token (QVARID _)) -> Just (Reduce 1 169)
    (264, Token (TODO_FUNLHS _)) -> Just (Reduce 1 169)
    (264, Token (INTEGER _)) -> Just (Reduce 1 169)
    (264, Token (QVARSYM _)) -> Just (Reduce 1 169)
    (264, Token (QCONSYM _)) -> Just (Reduce 1 169)
    (264, Token (BACKQUOTE _)) -> Just (Reduce 1 169)
    (265, Token (WHERE _)) -> Just (Reduce 1 168)
    (265, Token (LBRACE _)) -> Just (Reduce 1 168)
    (265, Token (RBRACE _)) -> Just (Reduce 1 168)
    (265, Token (LPAREN _)) -> Just (Reduce 1 168)
    (265, Token (RPAREN _)) -> Just (Reduce 1 168)
    (265, Token (COMMA _)) -> Just (Reduce 1 168)
    (265, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (265, Token (EQUAL _)) -> Just (Reduce 1 168)
    (265, Token (DERIVING _)) -> Just (Reduce 1 168)
    (265, Token (DARROW _)) -> Just (Reduce 1 168)
    (265, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (265, Token (INFIXL _)) -> Just (Reduce 1 168)
    (265, Token (INFIXR _)) -> Just (Reduce 1 168)
    (265, Token (INFIX _)) -> Just (Reduce 1 168)
    (265, Token (ARROW _)) -> Just (Reduce 1 168)
    (265, Token (LBRACKET _)) -> Just (Reduce 1 168)
    (265, Token (RBRACKET _)) -> Just (Reduce 1 168)
    (265, Token (EXCL _)) -> Just (Reduce 1 168)
    (265, Token (PIPE _)) -> Just (Reduce 1 168)
    (265, Token (QCONID _)) -> Just (Reduce 1 168)
    (265, Token (EXPORT _)) -> Just (Reduce 1 168)
    (265, Token (AS _)) -> Just (Reduce 1 168)
    (265, Token (QVARID _)) -> Just (Reduce 1 168)
    (265, Token (TODO_FUNLHS _)) -> Just (Reduce 1 168)
    (265, Token (INTEGER _)) -> Just (Reduce 1 168)
    (265, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (265, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (265, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (266, Token (WHERE _)) -> Just (Reduce 1 170)
    (266, Token (LBRACE _)) -> Just (Reduce 1 170)
    (266, Token (RBRACE _)) -> Just (Reduce 1 170)
    (266, Token (LPAREN _)) -> Just (Reduce 1 170)
    (266, Token (RPAREN _)) -> Just (Reduce 1 170)
    (266, Token (COMMA _)) -> Just (Reduce 1 170)
    (266, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (266, Token (EQUAL _)) -> Just (Reduce 1 170)
    (266, Token (DERIVING _)) -> Just (Reduce 1 170)
    (266, Token (DARROW _)) -> Just (Reduce 1 170)
    (266, Token (COLON_COLON _)) -> Just (Reduce 1 170)
    (266, Token (INFIXL _)) -> Just (Reduce 1 170)
    (266, Token (INFIXR _)) -> Just (Reduce 1 170)
    (266, Token (INFIX _)) -> Just (Reduce 1 170)
    (266, Token (ARROW _)) -> Just (Reduce 1 170)
    (266, Token (LBRACKET _)) -> Just (Reduce 1 170)
    (266, Token (RBRACKET _)) -> Just (Reduce 1 170)
    (266, Token (EXCL _)) -> Just (Reduce 1 170)
    (266, Token (PIPE _)) -> Just (Reduce 1 170)
    (266, Token (QCONID _)) -> Just (Reduce 1 170)
    (266, Token (EXPORT _)) -> Just (Reduce 1 170)
    (266, Token (AS _)) -> Just (Reduce 1 170)
    (266, Token (QVARID _)) -> Just (Reduce 1 170)
    (266, Token (TODO_FUNLHS _)) -> Just (Reduce 1 170)
    (266, Token (INTEGER _)) -> Just (Reduce 1 170)
    (266, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (266, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (266, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (267, Token (RPAREN _)) -> Just (Reduce 3 102)
    (267, Token (COMMA _)) -> Just (Shift 102)
    (268, Token (RPAREN _)) -> Just (Reduce 3 103)
    (269, Token (RPAREN _)) -> Just (Reduce 1 109)
    (269, Token (COMMA _)) -> Just (Shift 269)
    (270, Token (RPAREN _)) -> Just (Reduce 2 110)
    (271, Token (RBRACE _)) -> Just (Reduce 3 114)
    (271, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (271, Token (DERIVING _)) -> Just (Reduce 3 114)
    (272, Token (RBRACE _)) -> Just (Reduce 1 113)
    (272, Token (SEMICOLON _)) -> Just (Reduce 1 113)
    (272, Token (DERIVING _)) -> Just (Reduce 1 113)
    (272, Token (PIPE _)) -> Just (Shift 76)
    (273, Token (RBRACE _)) -> Just (Reduce 3 117)
    (273, Token (SEMICOLON _)) -> Just (Reduce 3 117)
    (273, Token (DERIVING _)) -> Just (Reduce 3 117)
    (273, Token (PIPE _)) -> Just (Reduce 3 117)
    (274, Token (RBRACE _)) -> Just (Reduce 4 118)
    (274, Token (SEMICOLON _)) -> Just (Reduce 4 118)
    (274, Token (DERIVING _)) -> Just (Reduce 4 118)
    (274, Token (PIPE _)) -> Just (Reduce 4 118)
    (275, Token (RBRACE _)) -> Just (Shift 274)
    (276, Token (BACKQUOTE _)) -> Just (Shift 280)
    (277, Token (RBRACE _)) -> Just (Reduce 1 160)
    (277, Token (LPAREN _)) -> Just (Reduce 1 160)
    (277, Token (RPAREN _)) -> Just (Reduce 1 160)
    (277, Token (COMMA _)) -> Just (Reduce 1 160)
    (277, Token (SEMICOLON _)) -> Just (Reduce 1 160)
    (277, Token (ARROW _)) -> Just (Reduce 1 160)
    (277, Token (LBRACKET _)) -> Just (Reduce 1 160)
    (277, Token (RBRACKET _)) -> Just (Reduce 1 160)
    (277, Token (EXCL _)) -> Just (Reduce 1 160)
    (277, Token (QCONID _)) -> Just (Reduce 1 160)
    (277, Token (EXPORT _)) -> Just (Reduce 1 160)
    (277, Token (AS _)) -> Just (Reduce 1 160)
    (277, Token (QVARID _)) -> Just (Reduce 1 160)
    (277, Token (INTEGER _)) -> Just (Reduce 1 160)
    (277, Token (QVARSYM _)) -> Just (Reduce 1 160)
    (277, Token (QCONSYM _)) -> Just (Reduce 1 160)
    (278, Token (QCONID _)) -> Just (Shift 276)
    (278, Token (EXPORT _)) -> Just (Shift 300)
    (278, Token (AS _)) -> Just (Shift 301)
    (278, Token (QVARID _)) -> Just (Shift 302)
    (279, Token (QCONID _)) -> Just (Shift 276)
    (280, Token (RBRACE _)) -> Just (Reduce 3 161)
    (280, Token (LPAREN _)) -> Just (Reduce 3 161)
    (280, Token (RPAREN _)) -> Just (Reduce 3 161)
    (280, Token (COMMA _)) -> Just (Reduce 3 161)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 161)
    (280, Token (ARROW _)) -> Just (Reduce 3 161)
    (280, Token (LBRACKET _)) -> Just (Reduce 3 161)
    (280, Token (RBRACKET _)) -> Just (Reduce 3 161)
    (280, Token (EXCL _)) -> Just (Reduce 3 161)
    (280, Token (QCONID _)) -> Just (Reduce 3 161)
    (280, Token (EXPORT _)) -> Just (Reduce 3 161)
    (280, Token (AS _)) -> Just (Reduce 3 161)
    (280, Token (QVARID _)) -> Just (Reduce 3 161)
    (280, Token (INTEGER _)) -> Just (Reduce 3 161)
    (280, Token (QVARSYM _)) -> Just (Reduce 3 161)
    (280, Token (QCONSYM _)) -> Just (Reduce 3 161)
    (281, Token (RBRACE _)) -> Just (Reduce 3 122)
    (282, Token (RBRACE _)) -> Just (Reduce 1 121)
    (282, Token (COMMA _)) -> Just (Shift 50)
    (283, Token (RBRACE _)) -> Just (Reduce 3 123)
    (283, Token (COMMA _)) -> Just (Reduce 3 123)
    (284, Token (COLON_COLON _)) -> Just (Shift 90)
    (285, Token (EXPORT _)) -> Just (Reduce 1 132)
    (285, Token (AS _)) -> Just (Reduce 1 132)
    (285, Token (QVARID _)) -> Just (Reduce 1 132)
    (285, Token (STRING _)) -> Just (Reduce 1 132)
    (286, Token (EXPORT _)) -> Just (Reduce 1 131)
    (286, Token (AS _)) -> Just (Reduce 1 131)
    (286, Token (QVARID _)) -> Just (Reduce 1 131)
    (286, Token (STRING _)) -> Just (Reduce 1 131)
    (287, Token (EXPORT _)) -> Just (Reduce 1 133)
    (287, Token (AS _)) -> Just (Reduce 1 133)
    (287, Token (QVARID _)) -> Just (Reduce 1 133)
    (287, Token (STRING _)) -> Just (Reduce 1 133)
    (288, Token (LPAREN _)) -> Just (Reduce 1 134)
    (288, Token (EXPORT _)) -> Just (Reduce 1 134)
    (288, Token (AS _)) -> Just (Reduce 1 134)
    (288, Token (QVARID _)) -> Just (Reduce 1 134)
    (288, Token (QVARSYM _)) -> Just (Reduce 1 134)
    (289, Token (STRING _)) -> Just (Reduce 1 137)
    (290, Token (STRING _)) -> Just (Reduce 1 136)
    (291, Token (STRING _)) -> Just (Reduce 1 138)
    (292, Token (LPAREN _)) -> Just (Reduce 1 135)
    (292, Token (EXPORT _)) -> Just (Reduce 1 135)
    (292, Token (AS _)) -> Just (Reduce 1 135)
    (292, Token (QVARID _)) -> Just (Reduce 1 135)
    (292, Token (QVARSYM _)) -> Just (Reduce 1 135)
    (293, Token (WHERE _)) -> Just (Reduce 1 142)
    (293, Token (RBRACE _)) -> Just (Reduce 1 142)
    (293, Token (SEMICOLON _)) -> Just (Reduce 1 142)
    (294, Token (WHERE _)) -> Just (Reduce 1 143)
    (294, Token (RBRACE _)) -> Just (Reduce 1 143)
    (294, Token (SEMICOLON _)) -> Just (Reduce 1 143)
    (295, Token (WHERE _)) -> Just (Reduce 1 145)
    (295, Token (RBRACE _)) -> Just (Reduce 1 145)
    (295, Token (LPAREN _)) -> Just (Reduce 1 145)
    (295, Token (SEMICOLON _)) -> Just (Reduce 1 145)
    (295, Token (QCONID _)) -> Just (Reduce 1 145)
    (295, Token (EXPORT _)) -> Just (Reduce 1 145)
    (295, Token (AS _)) -> Just (Reduce 1 145)
    (295, Token (QVARID _)) -> Just (Reduce 1 145)
    (295, Token (INTEGER _)) -> Just (Reduce 1 145)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 145)
    (295, Token (QCONSYM _)) -> Just (Reduce 1 145)
    (295, Token (BACKQUOTE _)) -> Just (Reduce 1 145)
    (296, Token (WHERE _)) -> Just (Reduce 3 147)
    (296, Token (RBRACE _)) -> Just (Reduce 3 147)
    (296, Token (LPAREN _)) -> Just (Reduce 3 147)
    (296, Token (SEMICOLON _)) -> Just (Reduce 3 147)
    (296, Token (QCONID _)) -> Just (Reduce 3 147)
    (296, Token (EXPORT _)) -> Just (Reduce 3 147)
    (296, Token (AS _)) -> Just (Reduce 3 147)
    (296, Token (QVARID _)) -> Just (Reduce 3 147)
    (296, Token (INTEGER _)) -> Just (Reduce 3 147)
    (296, Token (QVARSYM _)) -> Just (Reduce 3 147)
    (296, Token (QCONSYM _)) -> Just (Reduce 3 147)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 3 147)
    (297, Token (WHERE _)) -> Just (Reduce 2 146)
    (297, Token (RBRACE _)) -> Just (Reduce 2 146)
    (297, Token (LPAREN _)) -> Just (Reduce 2 146)
    (297, Token (SEMICOLON _)) -> Just (Reduce 2 146)
    (297, Token (QCONID _)) -> Just (Reduce 2 146)
    (297, Token (EXPORT _)) -> Just (Reduce 2 146)
    (297, Token (AS _)) -> Just (Reduce 2 146)
    (297, Token (QVARID _)) -> Just (Reduce 2 146)
    (297, Token (INTEGER _)) -> Just (Reduce 2 146)
    (297, Token (QVARSYM _)) -> Just (Reduce 2 146)
    (297, Token (QCONSYM _)) -> Just (Reduce 2 146)
    (297, Token (BACKQUOTE _)) -> Just (Reduce 2 146)
    (298, Token (WHERE _)) -> Just (Reduce 1 149)
    (298, Token (RBRACE _)) -> Just (Reduce 1 149)
    (298, Token (LPAREN _)) -> Just (Reduce 1 149)
    (298, Token (SEMICOLON _)) -> Just (Reduce 1 149)
    (298, Token (QCONID _)) -> Just (Reduce 1 149)
    (298, Token (EXPORT _)) -> Just (Reduce 1 149)
    (298, Token (AS _)) -> Just (Reduce 1 149)
    (298, Token (QVARID _)) -> Just (Reduce 1 149)
    (298, Token (INTEGER _)) -> Just (Reduce 1 149)
    (298, Token (QVARSYM _)) -> Just (Reduce 1 149)
    (298, Token (QCONSYM _)) -> Just (Reduce 1 149)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 1 149)
    (299, Token (WHERE _)) -> Just (Reduce 1 148)
    (299, Token (RBRACE _)) -> Just (Reduce 1 148)
    (299, Token (LPAREN _)) -> Just (Reduce 1 148)
    (299, Token (SEMICOLON _)) -> Just (Reduce 1 148)
    (299, Token (QCONID _)) -> Just (Reduce 1 148)
    (299, Token (EXPORT _)) -> Just (Reduce 1 148)
    (299, Token (AS _)) -> Just (Reduce 1 148)
    (299, Token (QVARID _)) -> Just (Reduce 1 148)
    (299, Token (INTEGER _)) -> Just (Reduce 1 148)
    (299, Token (QVARSYM _)) -> Just (Reduce 1 148)
    (299, Token (QCONSYM _)) -> Just (Reduce 1 148)
    (299, Token (BACKQUOTE _)) -> Just (Reduce 1 148)
    (300, Token (BACKQUOTE _)) -> Just (Shift 304)
    (301, Token (BACKQUOTE _)) -> Just (Shift 305)
    (302, Token (BACKQUOTE _)) -> Just (Shift 306)
    (303, Token (RBRACE _)) -> Just (Reduce 1 156)
    (303, Token (LPAREN _)) -> Just (Reduce 1 156)
    (303, Token (COMMA _)) -> Just (Reduce 1 156)
    (303, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (303, Token (EXPORT _)) -> Just (Reduce 1 156)
    (303, Token (AS _)) -> Just (Reduce 1 156)
    (303, Token (QVARID _)) -> Just (Reduce 1 156)
    (303, Token (INTEGER _)) -> Just (Reduce 1 156)
    (303, Token (QVARSYM _)) -> Just (Reduce 1 156)
    (304, Token (RBRACE _)) -> Just (Reduce 3 158)
    (304, Token (LPAREN _)) -> Just (Reduce 3 158)
    (304, Token (COMMA _)) -> Just (Reduce 3 158)
    (304, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (304, Token (EXPORT _)) -> Just (Reduce 3 158)
    (304, Token (AS _)) -> Just (Reduce 3 158)
    (304, Token (QVARID _)) -> Just (Reduce 3 158)
    (304, Token (INTEGER _)) -> Just (Reduce 3 158)
    (304, Token (QVARSYM _)) -> Just (Reduce 3 158)
    (305, Token (RBRACE _)) -> Just (Reduce 3 157)
    (305, Token (LPAREN _)) -> Just (Reduce 3 157)
    (305, Token (COMMA _)) -> Just (Reduce 3 157)
    (305, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (305, Token (EXPORT _)) -> Just (Reduce 3 157)
    (305, Token (AS _)) -> Just (Reduce 3 157)
    (305, Token (QVARID _)) -> Just (Reduce 3 157)
    (305, Token (INTEGER _)) -> Just (Reduce 3 157)
    (305, Token (QVARSYM _)) -> Just (Reduce 3 157)
    (306, Token (RBRACE _)) -> Just (Reduce 3 159)
    (306, Token (LPAREN _)) -> Just (Reduce 3 159)
    (306, Token (COMMA _)) -> Just (Reduce 3 159)
    (306, Token (SEMICOLON _)) -> Just (Reduce 3 159)
    (306, Token (EXPORT _)) -> Just (Reduce 3 159)
    (306, Token (AS _)) -> Just (Reduce 3 159)
    (306, Token (QVARID _)) -> Just (Reduce 3 159)
    (306, Token (INTEGER _)) -> Just (Reduce 3 159)
    (306, Token (QVARSYM _)) -> Just (Reduce 3 159)
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
production 63 = 23
production 64 = 23
production 65 = 33
production 66 = 34
production 67 = 34
production 68 = 35
production 69 = 35
production 70 = 35
production 71 = 24
production 72 = 24
production 73 = 36
production 74 = 37
production 75 = 37
production 76 = 38
production 77 = 38
production 78 = 38
production 79 = 30
production 80 = 30
production 81 = 30
production 82 = 30
production 83 = 42
production 84 = 42
production 85 = 39
production 86 = 39
production 87 = 40
production 88 = 40
production 89 = 40
production 90 = 25
production 91 = 25
production 92 = 18
production 93 = 18
production 94 = 17
production 95 = 17
production 96 = 44
production 97 = 44
production 98 = 44
production 99 = 44
production 100 = 44
production 101 = 44
production 102 = 47
production 103 = 47
production 104 = 45
production 105 = 45
production 106 = 45
production 107 = 45
production 108 = 45
production 109 = 48
production 110 = 48
production 111 = 19
production 112 = 19
production 113 = 49
production 114 = 49
production 115 = 50
production 116 = 50
production 117 = 50
production 118 = 50
production 119 = 22
production 120 = 22
production 121 = 52
production 122 = 52
production 123 = 53
production 124 = 21
production 125 = 21
production 126 = 20
production 127 = 54
production 128 = 26
production 129 = 26
production 130 = 26
production 131 = 55
production 132 = 55
production 133 = 55
production 134 = 56
production 135 = 58
production 136 = 57
production 137 = 57
production 138 = 57
production 139 = 31
production 140 = 32
production 141 = 32
production 142 = 59
production 143 = 60
production 144 = 61
production 145 = 62
production 146 = 62
production 147 = 62
production 148 = 63
production 149 = 63
production 150 = 8
production 151 = 8
production 152 = 8
production 153 = 8
production 154 = 9
production 155 = 9
production 156 = 64
production 157 = 64
production 158 = 64
production 159 = 64
production 160 = 51
production 161 = 51
production 162 = 43
production 163 = 43
production 164 = 16
production 165 = 16
production 166 = 15
production 167 = 15
production 168 = 46
production 169 = 46
production 170 = 46
production 171 = 65
production 172 = 1
production 173 = 41
production 174 = 41

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 145
    (9, 1) -> Just 170
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 200
    (13, 14) -> Just 18
    (13, 27) -> Just 168
    (13, 30) -> Just 201
    (13, 31) -> Just 202
    (13, 39) -> Just 212
    (13, 40) -> Just 213
    (16, 4) -> Just 17
    (16, 8) -> Just 200
    (16, 14) -> Just 18
    (16, 27) -> Just 168
    (16, 30) -> Just 201
    (16, 31) -> Just 202
    (16, 39) -> Just 212
    (16, 40) -> Just 213
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 119
    (25, 9) -> Just 120
    (25, 10) -> Just 33
    (25, 13) -> Just 109
    (34, 8) -> Just 299
    (34, 59) -> Just 222
    (34, 60) -> Just 293
    (34, 61) -> Just 294
    (34, 62) -> Just 35
    (34, 63) -> Just 295
    (35, 8) -> Just 299
    (35, 43) -> Just 36
    (35, 51) -> Just 245
    (35, 63) -> Just 297
    (35, 64) -> Just 246
    (36, 8) -> Just 299
    (36, 63) -> Just 296
    (37, 8) -> Just 200
    (37, 27) -> Just 208
    (37, 29) -> Just 207
    (37, 30) -> Just 201
    (37, 31) -> Just 202
    (37, 39) -> Just 212
    (37, 40) -> Just 213
    (38, 8) -> Just 200
    (38, 27) -> Just 208
    (38, 29) -> Just 209
    (38, 30) -> Just 201
    (38, 31) -> Just 202
    (38, 39) -> Just 212
    (38, 40) -> Just 213
    (39, 8) -> Just 217
    (39, 30) -> Just 227
    (39, 31) -> Just 218
    (39, 34) -> Just 224
    (39, 35) -> Just 226
    (39, 39) -> Just 212
    (39, 40) -> Just 213
    (40, 8) -> Just 217
    (40, 30) -> Just 227
    (40, 31) -> Just 218
    (40, 34) -> Just 225
    (40, 35) -> Just 226
    (40, 39) -> Just 212
    (40, 40) -> Just 213
    (41, 8) -> Just 219
    (41, 31) -> Just 220
    (41, 37) -> Just 231
    (41, 38) -> Just 233
    (42, 8) -> Just 219
    (42, 31) -> Just 220
    (42, 37) -> Just 232
    (42, 38) -> Just 233
    (43, 8) -> Just 116
    (43, 9) -> Just 117
    (43, 11) -> Just 110
    (43, 12) -> Just 111
    (44, 8) -> Just 116
    (44, 9) -> Just 117
    (44, 11) -> Just 146
    (44, 12) -> Just 111
    (45, 8) -> Just 116
    (45, 9) -> Just 117
    (45, 11) -> Just 147
    (45, 12) -> Just 111
    (46, 8) -> Just 119
    (46, 9) -> Just 120
    (46, 10) -> Just 108
    (46, 13) -> Just 109
    (47, 8) -> Just 119
    (47, 9) -> Just 120
    (47, 10) -> Just 118
    (47, 13) -> Just 109
    (48, 8) -> Just 236
    (48, 39) -> Just 237
    (49, 8) -> Just 236
    (49, 39) -> Just 284
    (49, 52) -> Just 275
    (49, 53) -> Just 282
    (50, 8) -> Just 236
    (50, 39) -> Just 284
    (50, 52) -> Just 281
    (50, 53) -> Just 282
    (51, 8) -> Just 180
    (52, 8) -> Just 191
    (53, 8) -> Just 192
    (54, 8) -> Just 193
    (62, 9) -> Just 261
    (62, 44) -> Just 252
    (62, 45) -> Just 253
    (62, 46) -> Just 254
    (63, 9) -> Just 261
    (63, 17) -> Just 64
    (63, 44) -> Just 171
    (63, 45) -> Just 253
    (63, 46) -> Just 254
    (64, 9) -> Just 261
    (64, 23) -> Just 163
    (64, 44) -> Just 172
    (64, 45) -> Just 253
    (64, 46) -> Just 254
    (65, 9) -> Just 261
    (65, 17) -> Just 66
    (65, 44) -> Just 171
    (65, 45) -> Just 253
    (65, 46) -> Just 254
    (66, 9) -> Just 261
    (66, 24) -> Just 165
    (66, 44) -> Just 172
    (66, 45) -> Just 253
    (66, 46) -> Just 254
    (67, 9) -> Just 261
    (67, 17) -> Just 68
    (67, 44) -> Just 171
    (67, 45) -> Just 253
    (67, 46) -> Just 254
    (68, 9) -> Just 261
    (68, 23) -> Just 162
    (68, 44) -> Just 172
    (68, 45) -> Just 253
    (68, 46) -> Just 254
    (69, 9) -> Just 261
    (69, 17) -> Just 70
    (69, 44) -> Just 171
    (69, 45) -> Just 253
    (69, 46) -> Just 254
    (70, 9) -> Just 261
    (70, 24) -> Just 164
    (70, 44) -> Just 172
    (70, 45) -> Just 253
    (70, 46) -> Just 254
    (71, 9) -> Just 261
    (71, 17) -> Just 72
    (71, 44) -> Just 171
    (71, 45) -> Just 253
    (71, 46) -> Just 254
    (72, 9) -> Just 261
    (72, 19) -> Just 150
    (72, 44) -> Just 172
    (72, 45) -> Just 253
    (72, 46) -> Just 254
    (73, 9) -> Just 261
    (73, 17) -> Just 74
    (73, 44) -> Just 171
    (73, 45) -> Just 253
    (73, 46) -> Just 254
    (74, 9) -> Just 261
    (74, 19) -> Just 151
    (74, 44) -> Just 172
    (74, 45) -> Just 253
    (74, 46) -> Just 254
    (75, 9) -> Just 262
    (75, 17) -> Just 78
    (75, 44) -> Just 171
    (75, 45) -> Just 253
    (75, 46) -> Just 254
    (75, 49) -> Just 174
    (75, 50) -> Just 272
    (76, 9) -> Just 262
    (76, 17) -> Just 78
    (76, 44) -> Just 171
    (76, 45) -> Just 253
    (76, 46) -> Just 254
    (76, 49) -> Just 271
    (76, 50) -> Just 272
    (77, 9) -> Just 92
    (78, 9) -> Just 261
    (78, 44) -> Just 172
    (78, 45) -> Just 253
    (78, 46) -> Just 254
    (78, 51) -> Just 79
    (79, 9) -> Just 261
    (79, 17) -> Just 80
    (79, 44) -> Just 171
    (79, 45) -> Just 253
    (79, 46) -> Just 254
    (80, 9) -> Just 261
    (80, 44) -> Just 172
    (80, 45) -> Just 253
    (80, 46) -> Just 254
    (81, 9) -> Just 261
    (81, 17) -> Just 82
    (81, 18) -> Just 211
    (81, 44) -> Just 171
    (81, 45) -> Just 253
    (81, 46) -> Just 254
    (82, 9) -> Just 261
    (82, 44) -> Just 172
    (82, 45) -> Just 253
    (82, 46) -> Just 254
    (83, 9) -> Just 261
    (83, 17) -> Just 89
    (83, 18) -> Just 149
    (83, 44) -> Just 171
    (83, 45) -> Just 253
    (83, 46) -> Just 254
    (84, 9) -> Just 261
    (84, 17) -> Just 89
    (84, 18) -> Just 173
    (84, 44) -> Just 171
    (84, 45) -> Just 253
    (84, 46) -> Just 254
    (85, 9) -> Just 261
    (85, 17) -> Just 89
    (85, 18) -> Just 194
    (85, 44) -> Just 171
    (85, 45) -> Just 253
    (85, 46) -> Just 254
    (86, 9) -> Just 261
    (86, 17) -> Just 89
    (86, 18) -> Just 195
    (86, 44) -> Just 171
    (86, 45) -> Just 253
    (86, 46) -> Just 254
    (87, 9) -> Just 261
    (87, 17) -> Just 89
    (87, 18) -> Just 196
    (87, 44) -> Just 171
    (87, 45) -> Just 253
    (87, 46) -> Just 254
    (88, 9) -> Just 261
    (88, 17) -> Just 89
    (88, 18) -> Just 210
    (88, 44) -> Just 171
    (88, 45) -> Just 253
    (88, 46) -> Just 254
    (89, 9) -> Just 261
    (89, 44) -> Just 172
    (89, 45) -> Just 253
    (89, 46) -> Just 254
    (90, 9) -> Just 261
    (90, 17) -> Just 89
    (90, 18) -> Just 283
    (90, 44) -> Just 171
    (90, 45) -> Just 253
    (90, 46) -> Just 254
    (91, 9) -> Just 261
    (91, 17) -> Just 89
    (91, 18) -> Just 181
    (91, 44) -> Just 171
    (91, 45) -> Just 253
    (91, 46) -> Just 254
    (92, 9) -> Just 261
    (92, 44) -> Just 182
    (92, 45) -> Just 253
    (92, 46) -> Just 254
    (93, 9) -> Just 261
    (93, 17) -> Just 94
    (93, 44) -> Just 171
    (93, 45) -> Just 253
    (93, 46) -> Just 254
    (94, 9) -> Just 261
    (94, 22) -> Just 161
    (94, 44) -> Just 172
    (94, 45) -> Just 253
    (94, 46) -> Just 254
    (95, 9) -> Just 261
    (95, 17) -> Just 97
    (95, 44) -> Just 171
    (95, 45) -> Just 253
    (95, 46) -> Just 254
    (96, 9) -> Just 261
    (96, 17) -> Just 98
    (96, 44) -> Just 171
    (96, 45) -> Just 253
    (96, 46) -> Just 254
    (97, 9) -> Just 261
    (97, 44) -> Just 172
    (97, 45) -> Just 253
    (97, 46) -> Just 254
    (98, 9) -> Just 261
    (98, 22) -> Just 160
    (98, 44) -> Just 172
    (98, 45) -> Just 253
    (98, 46) -> Just 254
    (99, 9) -> Just 261
    (99, 17) -> Just 89
    (99, 18) -> Just 250
    (99, 44) -> Just 171
    (99, 45) -> Just 253
    (99, 46) -> Just 254
    (99, 47) -> Just 255
    (99, 48) -> Just 263
    (100, 9) -> Just 261
    (100, 17) -> Just 89
    (100, 18) -> Just 187
    (100, 25) -> Just 166
    (100, 44) -> Just 171
    (100, 45) -> Just 253
    (100, 46) -> Just 254
    (101, 9) -> Just 261
    (101, 17) -> Just 89
    (101, 18) -> Just 187
    (101, 25) -> Just 188
    (101, 44) -> Just 171
    (101, 45) -> Just 253
    (101, 46) -> Just 254
    (102, 9) -> Just 261
    (102, 17) -> Just 89
    (102, 18) -> Just 267
    (102, 44) -> Just 171
    (102, 45) -> Just 253
    (102, 46) -> Just 254
    (102, 47) -> Just 268
    (103, 9) -> Just 261
    (103, 17) -> Just 89
    (103, 18) -> Just 251
    (103, 44) -> Just 171
    (103, 45) -> Just 253
    (103, 46) -> Just 254
    (121, 20) -> Just 177
    (121, 21) -> Just 156
    (122, 20) -> Just 177
    (122, 21) -> Just 157
    (123, 20) -> Just 177
    (123, 21) -> Just 158
    (124, 20) -> Just 177
    (124, 21) -> Just 159
    (137, 15) -> Just 8
    (139, 20) -> Just 152
    (140, 20) -> Just 153
    (141, 20) -> Just 154
    (142, 20) -> Just 155
    (144, 26) -> Just 167
    (145, 16) -> Just 148
    (175, 20) -> Just 177
    (175, 21) -> Just 178
    (183, 33) -> Just 184
    (185, 36) -> Just 186
    (189, 55) -> Just 197
    (190, 55) -> Just 198
    (197, 56) -> Just 52
    (197, 57) -> Just 199
    (198, 58) -> Just 54
    (199, 56) -> Just 53
    (200, 32) -> Just 203
    (202, 32) -> Just 204
    (205, 28) -> Just 221
    (213, 41) -> Just 214
    (214, 42) -> Just 215
    (214, 43) -> Just 244
    (214, 51) -> Just 245
    (214, 64) -> Just 246
    (217, 32) -> Just 228
    (218, 32) -> Just 229
    (219, 32) -> Just 234
    (220, 32) -> Just 235
    (242, 42) -> Just 243
    (242, 43) -> Just 244
    (242, 51) -> Just 245
    (242, 64) -> Just 246
    (269, 48) -> Just 270
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
                  Token (ARROW semanticValue) ->
                    StackValue_ARROW semanticValue
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
                  Token (TODO_INST semanticValue) ->
                    StackValue_TODO_INST semanticValue
                  Token (EXPORT semanticValue) ->
                    StackValue_EXPORT semanticValue
                  Token (AS semanticValue) ->
                    StackValue_AS semanticValue
                  Token (QVARID semanticValue) ->
                    StackValue_QVARID semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (TODO_FUNLHS semanticValue) ->
                    StackValue_TODO_FUNLHS semanticValue
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
                      Monad.liftM StackValue_decl $ decl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    62 ->
                      Monad.liftM StackValue_decl $ decl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    63 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies actions
                    64 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies_WHERE_cdecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls value -> value; _ -> undefined })
                    65 ->
                      Monad.liftM StackValue_cdecls $ cdecls_implies_LBRACE_cdecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cdecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    66 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl actions (case snd (pop !! 0) of { StackValue_cdecl value -> value; _ -> undefined })
                    67 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq actions (case snd (pop !! 2) of { StackValue_cdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecl_seq value -> value; _ -> undefined })
                    68 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    69 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    70 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    71 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies actions
                    72 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies_WHERE_idecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls value -> value; _ -> undefined })
                    73 ->
                      Monad.liftM StackValue_idecls $ idecls_implies_LBRACE_idecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_idecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    74 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl actions (case snd (pop !! 0) of { StackValue_idecl value -> value; _ -> undefined })
                    75 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl_SEMICOLON_idecl_seq actions (case snd (pop !! 2) of { StackValue_idecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecl_seq value -> value; _ -> undefined })
                    76 ->
                      Monad.liftM StackValue_idecl $ idecl_implies actions
                    77 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    78 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    79 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies actions
                    80 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    81 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    82 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_fixity_integer_opt_ops actions (case snd (pop !! 2) of { StackValue_fixity value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_integer_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    83 ->
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    84 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    85 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    86 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    87 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    88 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    89 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    90 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    91 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_ARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    94 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    98 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    101 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_ARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    109 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    112 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    118 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    120 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass_COMMA_dclass_seq actions (case snd (pop !! 2) of { StackValue_dclass value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass_seq value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_inst $ inst_implies_TODO_INST actions (case snd (pop !! 0) of { StackValue_TODO_INST value -> value; _ -> undefined })
                    128 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_safety value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_expent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_impent $ impent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    135 ->
                      Monad.liftM StackValue_expent $ expent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_safety $ safety_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_safety $ safety_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_safety $ safety_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    139 ->
                      Monad.liftM StackValue_funlhs $ funlhs_implies_TODO_FUNLHS actions (case snd (pop !! 0) of { StackValue_TODO_FUNLHS value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp_WHERE_decls actions (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    146 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    165 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    167 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    174 ->
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
  , decl_implies_funlhs_rhs = \funlhs0 rhs1 ->
      return $ Decl_implies_funlhs_rhs funlhs0 rhs1
  , decl_implies_var_rhs = \var0 rhs1 ->
      return $ Decl_implies_var_rhs var0 rhs1
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
  , cdecl_implies_funlhs_rhs = \funlhs0 rhs1 ->
      return $ Cdecl_implies_funlhs_rhs funlhs0 rhs1
  , cdecl_implies_var_rhs = \var0 rhs1 ->
      return $ Cdecl_implies_var_rhs var0 rhs1
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
  , idecl_implies_funlhs_rhs = \funlhs0 rhs1 ->
      return $ Idecl_implies_funlhs_rhs funlhs0 rhs1
  , idecl_implies_var_rhs = \var0 rhs1 ->
      return $ Idecl_implies_var_rhs var0 rhs1
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
  , type'_implies_btype_ARROW_type' = \btype0 aRROW1 type'2 ->
      return $ Type'_implies_btype_ARROW_type' btype0 aRROW1 type'2
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
  , gtycon_implies_LPAREN_ARROW_RPAREN = \lPAREN0 aRROW1 rPAREN2 ->
      return $ Gtycon_implies_LPAREN_ARROW_RPAREN lPAREN0 aRROW1 rPAREN2
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
  , inst_implies_TODO_INST = \tODO_INST0 ->
      return $ Inst_implies_TODO_INST tODO_INST0
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
  , funlhs_implies_TODO_FUNLHS = \tODO_FUNLHS0 ->
      return $ Funlhs_implies_TODO_FUNLHS tODO_FUNLHS0
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

