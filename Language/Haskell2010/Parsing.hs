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

data Topdecl =
    Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_impspec_opt IMPORT Qualified_opt Modid As_opt Impspec_opt
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

data Impspec_opt =
    Impspec_opt_implies
  | Impspec_opt_implies_impspec Impspec
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
  | Fdecl_implies_QVARID_callconv_expent_var_COLON_COLON_type' QVARID Callconv Expent Var COLON_COLON Type'
  deriving (Eq, Ord, Read, Show)

data Decl =
    Decl_implies_gendecl Gendecl
  | Decl_implies_funlhs_rhs Funlhs Rhs
  | Decl_implies_var_rhs Var Rhs
  deriving (Eq, Ord, Read, Show)

data Callconv =
    Callconv_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Impent =
    Impent_implies_STRING STRING
  deriving (Eq, Ord, Read, Show)

data Var =
    Var_implies_QVARID QVARID
  | Var_implies_LPAREN_QVARSYM_RPAREN LPAREN QVARSYM RPAREN
  deriving (Eq, Ord, Read, Show)

data Safety =
    Safety_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Expent =
    Expent_implies_STRING STRING
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

data Op =
    Op_implies_varop Varop
  | Op_implies_conop Conop
  deriving (Eq, Ord, Read, Show)

data Inst =
    Inst_implies_TODO_INST TODO_INST
  deriving (Eq, Ord, Read, Show)

data Tycls =
    Tycls_implies_QCONID QCONID
  deriving (Eq, Ord, Read, Show)

data Tyvar =
    Tyvar_implies_QVARID QVARID
  deriving (Eq, Ord, Read, Show)

data Impspec =
    Impspec_implies_LPAREN_import_seq_RPAREN LPAREN Import_seq RPAREN
  | Impspec_implies_HIDING_LPAREN_import_seq_RPAREN HIDING LPAREN Import_seq RPAREN
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

data Con =
    Con_implies_QCONID QCONID
  | Con_implies_LPAREN_QCONSYM_RPAREN LPAREN QCONSYM RPAREN
  deriving (Eq, Ord, Read, Show)

data Cname_seq =
    Cname_seq_implies_cname Cname
  | Cname_seq_implies_cname_COMMA_cname_seq Cname COMMA Cname_seq
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

data Cname =
    Cname_implies_var Var
  | Cname_implies_con Con
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

data Atypes =
    Atypes_implies
  | Atypes_implies_atype_atypes Atype Atypes
  | Atypes_implies_EXCL_atype_atypes EXCL Atype Atypes
  deriving (Eq, Ord, Read, Show)

data Varop =
    Varop_implies_QVARSYM QVARSYM
  | Varop_implies_BACKQUOTE_QVARID_BACKQUOTE BACKQUOTE QVARID BACKQUOTE
  deriving (Eq, Ord, Read, Show)

data StackValue =
    StackValue_EOF
  | StackValue_MODULE MODULE
  | StackValue_WHERE WHERE
  | StackValue_QCONID QCONID
  | StackValue_LBRACE LBRACE
  | StackValue_RBRACE RBRACE
  | StackValue_SEMICOLON SEMICOLON
  | StackValue_IMPORT IMPORT
  | StackValue_TYPE TYPE
  | StackValue_EQUAL EQUAL
  | StackValue_DATA DATA
  | StackValue_DERIVING DERIVING
  | StackValue_LPAREN LPAREN
  | StackValue_RPAREN RPAREN
  | StackValue_DARROW DARROW
  | StackValue_NEWTYPE NEWTYPE
  | StackValue_CLASS CLASS
  | StackValue_INSTANCE INSTANCE
  | StackValue_DEFAULT DEFAULT
  | StackValue_FOREIGN FOREIGN
  | StackValue_COLON_COLON COLON_COLON
  | StackValue_QVARID QVARID
  | StackValue_STRING STRING
  | StackValue_INTEGER INTEGER
  | StackValue_TODO_FUNLHS TODO_FUNLHS
  | StackValue_TODO_INST TODO_INST
  | StackValue_QUALIFIED QUALIFIED
  | StackValue_AS AS
  | StackValue_HIDING HIDING
  | StackValue_COMMA COMMA
  | StackValue_DOT_DOT DOT_DOT
  | StackValue_ARROW ARROW
  | StackValue_LBRACKET LBRACKET
  | StackValue_RBRACKET RBRACKET
  | StackValue_EXCL EXCL
  | StackValue_PIPE PIPE
  | StackValue_QVARSYM QVARSYM
  | StackValue_QCONSYM QCONSYM
  | StackValue_INFIXL INFIXL
  | StackValue_INFIXR INFIXR
  | StackValue_INFIX INFIX
  | StackValue_BACKQUOTE BACKQUOTE
  | StackValue_module' Module'
  | StackValue_modid Modid
  | StackValue_exports_opt Exports_opt
  | StackValue_body Body
  | StackValue_topdecls Topdecls
  | StackValue_topdecl Topdecl
  | StackValue_qualified_opt Qualified_opt
  | StackValue_as_opt As_opt
  | StackValue_impspec_opt Impspec_opt
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
  | StackValue_callconv Callconv
  | StackValue_impent Impent
  | StackValue_var Var
  | StackValue_safety Safety
  | StackValue_expent Expent
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
  | StackValue_exp Exp
  | StackValue_infixexp Infixexp
  | StackValue_lexp Lexp
  | StackValue_fexp Fexp
  | StackValue_aexp Aexp
  | StackValue_op Op
  | StackValue_inst Inst
  | StackValue_tycls Tycls
  | StackValue_tyvar Tyvar
  | StackValue_impspec Impspec
  | StackValue_import_seq Import_seq
  | StackValue_import' Import'
  | StackValue_con Con
  | StackValue_cname_seq Cname_seq
  | StackValue_exports Exports
  | StackValue_export_seq Export_seq
  | StackValue_export Export
  | StackValue_cname Cname
  | StackValue_atype Atype
  | StackValue_gtycon Gtycon
  | StackValue_type_seq2 Type_seq2
  | StackValue_comma_list Comma_list
  | StackValue_constrs Constrs
  | StackValue_constr Constr
  | StackValue_conop Conop
  | StackValue_fielddecl_seq Fielddecl_seq
  | StackValue_fielddecl Fielddecl
  | StackValue_atypes Atypes
  | StackValue_varop Varop

data SemanticActions m = SemanticActions
  { module'_implies_MODULE_modid_exports_opt_WHERE_body :: MODULE -> Modid -> Exports_opt -> WHERE -> Body -> m Module'
  , module'_implies_body :: Body -> m Module'
  , modid_implies_QCONID :: QCONID -> m Modid
  , body_implies_LBRACE_topdecls_RBRACE :: LBRACE -> Topdecls -> RBRACE -> m Body
  , topdecls_implies_topdecl :: Topdecl -> m Topdecls
  , topdecls_implies_topdecl_SEMICOLON_topdecls :: Topdecl -> SEMICOLON -> Topdecls -> m Topdecls
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_impspec_opt :: IMPORT -> Qualified_opt -> Modid -> As_opt -> Impspec_opt -> m Topdecl
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
  , fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' :: IMPORT -> Callconv -> Impent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' :: IMPORT -> Callconv -> Safety -> Impent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , fdecl_implies_QVARID_callconv_expent_var_COLON_COLON_type' :: QVARID -> Callconv -> Expent -> Var -> COLON_COLON -> Type' -> m Fdecl
  , callconv_implies_QVARID :: QVARID -> m Callconv
  , safety_implies_QVARID :: QVARID -> m Safety
  , impent_implies_STRING :: STRING -> m Impent
  , expent_implies_STRING :: STRING -> m Expent
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
  , integer_opt_implies :: m Integer_opt
  , integer_opt_implies_INTEGER :: INTEGER -> m Integer_opt
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
  , inst_implies_TODO_INST :: TODO_INST -> m Inst
  , tycls_implies_QCONID :: QCONID -> m Tycls
  , tyvar_implies_QVARID :: QVARID -> m Tyvar
  , qualified_opt_implies :: m Qualified_opt
  , qualified_opt_implies_QUALIFIED :: QUALIFIED -> m Qualified_opt
  , as_opt_implies :: m As_opt
  , as_opt_implies_AS_modid :: AS -> Modid -> m As_opt
  , impspec_opt_implies :: m Impspec_opt
  , impspec_opt_implies_impspec :: Impspec -> m Impspec_opt
  , impspec_implies_LPAREN_import_seq_RPAREN :: LPAREN -> Import_seq -> RPAREN -> m Impspec
  , impspec_implies_HIDING_LPAREN_import_seq_RPAREN :: HIDING -> LPAREN -> Import_seq -> RPAREN -> m Impspec
  , import_seq_implies :: m Import_seq
  , import_seq_implies_import' :: Import' -> m Import_seq
  , import_seq_implies_import'_COMMA_import_seq :: Import' -> COMMA -> Import_seq -> m Import_seq
  , import'_implies_var :: Var -> m Import'
  , import'_implies_con :: Con -> m Import'
  , import'_implies_con_LPAREN_RPAREN :: Con -> LPAREN -> RPAREN -> m Import'
  , import'_implies_con_LPAREN_DOT_DOT_RPAREN :: Con -> LPAREN -> DOT_DOT -> RPAREN -> m Import'
  , import'_implies_con_LPAREN_cname_seq_RPAREN :: Con -> LPAREN -> Cname_seq -> RPAREN -> m Import'
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
  , cname_seq_implies_cname :: Cname -> m Cname_seq
  , cname_seq_implies_cname_COMMA_cname_seq :: Cname -> COMMA -> Cname_seq -> m Cname_seq
  , cname_implies_var :: Var -> m Cname
  , cname_implies_con :: Con -> m Cname
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
  , fielddecl_seq_implies_fielddecl :: Fielddecl -> m Fielddecl_seq
  , fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq :: Fielddecl -> COMMA -> Fielddecl_seq -> m Fielddecl_seq
  , fielddecl_implies_vars_COLON_COLON_type' :: Vars -> COLON_COLON -> Type' -> m Fielddecl
  , atypes_implies :: m Atypes
  , atypes_implies_atype_atypes :: Atype -> Atypes -> m Atypes
  , atypes_implies_EXCL_atype_atypes :: EXCL -> Atype -> Atypes -> m Atypes
  , newconstr_implies_EQUAL_con_atype :: EQUAL -> Con -> Atype -> m Newconstr
  , newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE :: EQUAL -> Con -> LBRACE -> Var -> COLON_COLON -> Type' -> RBRACE -> m Newconstr
  , ops_implies_op :: Op -> m Ops
  , ops_implies_op_COMMA_ops :: Op -> COMMA -> Ops -> m Ops
  , vars_implies_var :: Var -> m Vars
  , vars_implies_var_COMMA_vars :: Var -> COMMA -> Vars -> m Vars
  , var_implies_QVARID :: QVARID -> m Var
  , var_implies_LPAREN_QVARSYM_RPAREN :: LPAREN -> QVARSYM -> RPAREN -> m Var
  , con_implies_QCONID :: QCONID -> m Con
  , con_implies_LPAREN_QCONSYM_RPAREN :: LPAREN -> QCONSYM -> RPAREN -> m Con
  , fixity_implies_INFIXL :: INFIXL -> m Fixity
  , fixity_implies_INFIXR :: INFIXR -> m Fixity
  , fixity_implies_INFIX :: INFIX -> m Fixity
  , varop_implies_QVARSYM :: QVARSYM -> m Varop
  , varop_implies_BACKQUOTE_QVARID_BACKQUOTE :: BACKQUOTE -> QVARID -> BACKQUOTE -> m Varop
  , conop_implies_QCONSYM :: QCONSYM -> m Conop
  , conop_implies_BACKQUOTE_QCONID_BACKQUOTE :: BACKQUOTE -> QCONID -> BACKQUOTE -> m Conop
  , op_implies_varop :: Varop -> m Op
  , op_implies_conop :: Conop -> m Op
  , dclass_implies_QCONID :: QCONID -> m Dclass
  , dclass_seq_implies_dclass :: Dclass -> m Dclass_seq
  , dclass_seq_implies_dclass_COMMA_dclass_seq :: Dclass -> COMMA -> Dclass_seq -> m Dclass_seq }

dfaActionTransition :: ActionState -> ActionSymbol -> Maybe Action
dfaActionTransition q s =
  case (q, s) of
    (0, Token (MODULE _)) -> Just (Shift 2)
    (0, Token (LBRACE _)) -> Just (Shift 13)
    (1, EOF) -> Just (Accept)
    (2, Token (QCONID _)) -> Just (Shift 11)
    (3, Token (LBRACE _)) -> Just (Shift 13)
    (4, Token (WHERE _)) -> Just (Reduce 0 97)
    (4, Token (LPAREN _)) -> Just (Shift 160)
    (5, Token (WHERE _)) -> Just (Shift 3)
    (6, EOF) -> Just (Reduce 1 1)
    (7, EOF) -> Just (Reduce 5 0)
    (8, Token (QCONID _)) -> Just (Shift 11)
    (9, Token (QCONID _)) -> Just (Shift 11)
    (10, Token (QCONID _)) -> Just (Shift 11)
    (11, Token (MODULE _)) -> Just (Reduce 1 2)
    (11, Token (WHERE _)) -> Just (Reduce 1 2)
    (11, Token (QCONID _)) -> Just (Reduce 1 2)
    (11, Token (RBRACE _)) -> Just (Reduce 1 2)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 2)
    (11, Token (LPAREN _)) -> Just (Reduce 1 2)
    (11, Token (RPAREN _)) -> Just (Reduce 1 2)
    (11, Token (QVARID _)) -> Just (Reduce 1 2)
    (11, Token (AS _)) -> Just (Reduce 1 2)
    (11, Token (HIDING _)) -> Just (Reduce 1 2)
    (11, Token (COMMA _)) -> Just (Reduce 1 2)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 2)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 2)
    (12, Token (WHERE _)) -> Just (Reduce 1 98)
    (13, Token (RBRACE _)) -> Just (Reduce 0 61)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (13, Token (IMPORT _)) -> Just (Shift 19)
    (13, Token (TYPE _)) -> Just (Shift 20)
    (13, Token (DATA _)) -> Just (Shift 22)
    (13, Token (LPAREN _)) -> Just (Shift 167)
    (13, Token (NEWTYPE _)) -> Just (Shift 46)
    (13, Token (CLASS _)) -> Just (Shift 47)
    (13, Token (INSTANCE _)) -> Just (Shift 48)
    (13, Token (DEFAULT _)) -> Just (Shift 49)
    (13, Token (FOREIGN _)) -> Just (Shift 50)
    (13, Token (QVARID _)) -> Just (Shift 170)
    (13, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (13, Token (INFIXL _)) -> Just (Shift 207)
    (13, Token (INFIXR _)) -> Just (Shift 208)
    (13, Token (INFIX _)) -> Just (Shift 209)
    (14, EOF) -> Just (Reduce 3 3)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 61)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (16, Token (IMPORT _)) -> Just (Shift 19)
    (16, Token (TYPE _)) -> Just (Shift 20)
    (16, Token (DATA _)) -> Just (Shift 22)
    (16, Token (LPAREN _)) -> Just (Shift 167)
    (16, Token (NEWTYPE _)) -> Just (Shift 46)
    (16, Token (CLASS _)) -> Just (Shift 47)
    (16, Token (INSTANCE _)) -> Just (Shift 48)
    (16, Token (DEFAULT _)) -> Just (Shift 49)
    (16, Token (FOREIGN _)) -> Just (Shift 50)
    (16, Token (QVARID _)) -> Just (Shift 170)
    (16, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (16, Token (INFIXL _)) -> Just (Shift 207)
    (16, Token (INFIXR _)) -> Just (Shift 208)
    (16, Token (INFIX _)) -> Just (Shift 209)
    (17, Token (RBRACE _)) -> Just (Reduce 3 5)
    (18, Token (RBRACE _)) -> Just (Reduce 1 4)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (QCONID _)) -> Just (Reduce 0 81)
    (19, Token (QUALIFIED _)) -> Just (Shift 83)
    (20, Token (QCONID _)) -> Just (Shift 239)
    (20, Token (LPAREN _)) -> Just (Shift 98)
    (20, Token (QVARID _)) -> Just (Shift 224)
    (20, Token (LBRACKET _)) -> Just (Shift 100)
    (20, Token (EXCL _)) -> Just (Shift 223)
    (21, Token (QCONID _)) -> Just (Shift 239)
    (21, Token (LPAREN _)) -> Just (Shift 98)
    (21, Token (QVARID _)) -> Just (Shift 224)
    (21, Token (LBRACKET _)) -> Just (Shift 100)
    (21, Token (EXCL _)) -> Just (Shift 223)
    (22, Token (QCONID _)) -> Just (Shift 239)
    (22, Token (LPAREN _)) -> Just (Shift 98)
    (22, Token (QVARID _)) -> Just (Shift 224)
    (22, Token (LBRACKET _)) -> Just (Shift 100)
    (22, Token (EXCL _)) -> Just (Shift 223)
    (23, Token (QCONID _)) -> Just (Shift 110)
    (23, Token (LPAREN _)) -> Just (Shift 27)
    (24, Token (QCONID _)) -> Just (Shift 110)
    (24, Token (LPAREN _)) -> Just (Shift 28)
    (25, Token (QCONID _)) -> Just (Shift 110)
    (25, Token (LPAREN _)) -> Just (Shift 29)
    (26, Token (QCONID _)) -> Just (Shift 110)
    (26, Token (LPAREN _)) -> Just (Shift 30)
    (27, Token (QCONID _)) -> Just (Shift 110)
    (27, Token (RPAREN _)) -> Just (Shift 32)
    (28, Token (QCONID _)) -> Just (Shift 110)
    (28, Token (RPAREN _)) -> Just (Shift 33)
    (29, Token (QCONID _)) -> Just (Shift 110)
    (29, Token (RPAREN _)) -> Just (Shift 34)
    (30, Token (QCONID _)) -> Just (Shift 110)
    (30, Token (RPAREN _)) -> Just (Shift 35)
    (31, Token (QCONID _)) -> Just (Shift 239)
    (31, Token (LPAREN _)) -> Just (Shift 98)
    (31, Token (RPAREN _)) -> Just (Shift 36)
    (31, Token (QVARID _)) -> Just (Shift 224)
    (31, Token (LBRACKET _)) -> Just (Shift 100)
    (31, Token (EXCL _)) -> Just (Shift 223)
    (32, Token (RBRACE _)) -> Just (Reduce 6 10)
    (32, Token (SEMICOLON _)) -> Just (Reduce 6 10)
    (33, Token (RBRACE _)) -> Just (Reduce 8 14)
    (33, Token (SEMICOLON _)) -> Just (Reduce 8 14)
    (34, Token (RBRACE _)) -> Just (Reduce 8 22)
    (34, Token (SEMICOLON _)) -> Just (Reduce 8 22)
    (35, Token (RBRACE _)) -> Just (Reduce 6 18)
    (35, Token (SEMICOLON _)) -> Just (Reduce 6 18)
    (36, Token (RBRACE _)) -> Just (Reduce 3 28)
    (36, Token (SEMICOLON _)) -> Just (Reduce 3 28)
    (37, Token (RBRACE _)) -> Just (Reduce 7 11)
    (37, Token (SEMICOLON _)) -> Just (Reduce 7 11)
    (38, Token (RBRACE _)) -> Just (Reduce 9 15)
    (38, Token (SEMICOLON _)) -> Just (Reduce 9 15)
    (39, Token (RBRACE _)) -> Just (Reduce 9 23)
    (39, Token (SEMICOLON _)) -> Just (Reduce 9 23)
    (40, Token (RBRACE _)) -> Just (Reduce 7 19)
    (40, Token (SEMICOLON _)) -> Just (Reduce 7 19)
    (41, Token (RBRACE _)) -> Just (Reduce 4 29)
    (41, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (42, Token (QCONID _)) -> Just (Shift 239)
    (42, Token (LPAREN _)) -> Just (Shift 98)
    (42, Token (QVARID _)) -> Just (Shift 224)
    (42, Token (LBRACKET _)) -> Just (Shift 100)
    (42, Token (EXCL _)) -> Just (Shift 223)
    (43, Token (QCONID _)) -> Just (Shift 239)
    (43, Token (LPAREN _)) -> Just (Shift 98)
    (43, Token (QVARID _)) -> Just (Shift 224)
    (43, Token (LBRACKET _)) -> Just (Shift 100)
    (43, Token (EXCL _)) -> Just (Shift 223)
    (44, Token (QCONID _)) -> Just (Shift 239)
    (44, Token (LPAREN _)) -> Just (Shift 98)
    (44, Token (QVARID _)) -> Just (Shift 224)
    (44, Token (LBRACKET _)) -> Just (Shift 100)
    (44, Token (EXCL _)) -> Just (Shift 223)
    (45, Token (QCONID _)) -> Just (Shift 239)
    (45, Token (LPAREN _)) -> Just (Shift 98)
    (45, Token (QVARID _)) -> Just (Shift 224)
    (45, Token (LBRACKET _)) -> Just (Shift 100)
    (45, Token (EXCL _)) -> Just (Shift 223)
    (46, Token (QCONID _)) -> Just (Shift 239)
    (46, Token (LPAREN _)) -> Just (Shift 98)
    (46, Token (QVARID _)) -> Just (Shift 224)
    (46, Token (LBRACKET _)) -> Just (Shift 100)
    (46, Token (EXCL _)) -> Just (Shift 223)
    (47, Token (QCONID _)) -> Just (Shift 239)
    (47, Token (LPAREN _)) -> Just (Shift 98)
    (47, Token (QVARID _)) -> Just (Shift 224)
    (47, Token (LBRACKET _)) -> Just (Shift 100)
    (47, Token (EXCL _)) -> Just (Shift 223)
    (48, Token (QCONID _)) -> Just (Shift 239)
    (48, Token (LPAREN _)) -> Just (Shift 98)
    (48, Token (QVARID _)) -> Just (Shift 224)
    (48, Token (LBRACKET _)) -> Just (Shift 100)
    (48, Token (EXCL _)) -> Just (Shift 223)
    (49, Token (LPAREN _)) -> Just (Shift 31)
    (50, Token (IMPORT _)) -> Just (Shift 126)
    (50, Token (QVARID _)) -> Just (Shift 127)
    (51, Token (QCONID _)) -> Just (Reduce 0 83)
    (51, Token (RBRACE _)) -> Just (Reduce 0 83)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 83)
    (51, Token (LPAREN _)) -> Just (Reduce 0 83)
    (51, Token (QVARID _)) -> Just (Reduce 0 83)
    (51, Token (AS _)) -> Just (Shift 9)
    (51, Token (HIDING _)) -> Just (Reduce 0 83)
    (51, Token (COMMA _)) -> Just (Reduce 0 83)
    (51, Token (QVARSYM _)) -> Just (Reduce 0 83)
    (51, Token (QCONSYM _)) -> Just (Reduce 0 83)
    (52, Token (RBRACE _)) -> Just (Reduce 0 85)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (52, Token (LPAREN _)) -> Just (Shift 156)
    (52, Token (HIDING _)) -> Just (Shift 227)
    (53, Token (RBRACE _)) -> Just (Reduce 5 6)
    (53, Token (SEMICOLON _)) -> Just (Reduce 5 6)
    (54, Token (QCONID _)) -> Just (Shift 239)
    (54, Token (EQUAL _)) -> Just (Shift 21)
    (54, Token (LPAREN _)) -> Just (Shift 98)
    (54, Token (QVARID _)) -> Just (Shift 224)
    (54, Token (LBRACKET _)) -> Just (Shift 100)
    (54, Token (EXCL _)) -> Just (Shift 223)
    (55, Token (QCONID _)) -> Just (Shift 239)
    (55, Token (RBRACE _)) -> Just (Reduce 0 134)
    (55, Token (SEMICOLON _)) -> Just (Reduce 0 134)
    (55, Token (EQUAL _)) -> Just (Shift 86)
    (55, Token (DERIVING _)) -> Just (Reduce 0 134)
    (55, Token (LPAREN _)) -> Just (Shift 98)
    (55, Token (DARROW _)) -> Just (Shift 42)
    (55, Token (QVARID _)) -> Just (Shift 224)
    (55, Token (LBRACKET _)) -> Just (Shift 100)
    (55, Token (EXCL _)) -> Just (Shift 223)
    (56, Token (QCONID _)) -> Just (Shift 239)
    (56, Token (RBRACE _)) -> Just (Reduce 0 134)
    (56, Token (SEMICOLON _)) -> Just (Reduce 0 134)
    (56, Token (EQUAL _)) -> Just (Shift 86)
    (56, Token (DERIVING _)) -> Just (Reduce 0 134)
    (56, Token (LPAREN _)) -> Just (Shift 98)
    (56, Token (QVARID _)) -> Just (Shift 224)
    (56, Token (LBRACKET _)) -> Just (Shift 100)
    (56, Token (EXCL _)) -> Just (Shift 223)
    (57, Token (QCONID _)) -> Just (Shift 239)
    (57, Token (EQUAL _)) -> Just (Shift 115)
    (57, Token (LPAREN _)) -> Just (Shift 98)
    (57, Token (QVARID _)) -> Just (Shift 224)
    (57, Token (LBRACKET _)) -> Just (Shift 100)
    (57, Token (EXCL _)) -> Just (Shift 223)
    (58, Token (WHERE _)) -> Just (Shift 120)
    (58, Token (QCONID _)) -> Just (Shift 239)
    (58, Token (RBRACE _)) -> Just (Reduce 0 45)
    (58, Token (SEMICOLON _)) -> Just (Reduce 0 45)
    (58, Token (LPAREN _)) -> Just (Shift 98)
    (58, Token (QVARID _)) -> Just (Shift 224)
    (58, Token (LBRACKET _)) -> Just (Shift 100)
    (58, Token (EXCL _)) -> Just (Shift 223)
    (59, Token (WHERE _)) -> Just (Shift 122)
    (59, Token (QCONID _)) -> Just (Shift 239)
    (59, Token (RBRACE _)) -> Just (Reduce 0 53)
    (59, Token (SEMICOLON _)) -> Just (Reduce 0 53)
    (59, Token (LPAREN _)) -> Just (Shift 98)
    (59, Token (QVARID _)) -> Just (Shift 224)
    (59, Token (LBRACKET _)) -> Just (Shift 100)
    (59, Token (EXCL _)) -> Just (Shift 223)
    (60, Token (QCONID _)) -> Just (Shift 239)
    (60, Token (EQUAL _)) -> Just (Shift 115)
    (60, Token (LPAREN _)) -> Just (Shift 98)
    (60, Token (DARROW _)) -> Just (Shift 43)
    (60, Token (QVARID _)) -> Just (Shift 224)
    (60, Token (LBRACKET _)) -> Just (Shift 100)
    (60, Token (EXCL _)) -> Just (Shift 223)
    (61, Token (WHERE _)) -> Just (Shift 120)
    (61, Token (QCONID _)) -> Just (Shift 239)
    (61, Token (RBRACE _)) -> Just (Reduce 0 45)
    (61, Token (SEMICOLON _)) -> Just (Reduce 0 45)
    (61, Token (LPAREN _)) -> Just (Shift 98)
    (61, Token (DARROW _)) -> Just (Shift 44)
    (61, Token (QVARID _)) -> Just (Shift 224)
    (61, Token (LBRACKET _)) -> Just (Shift 100)
    (61, Token (EXCL _)) -> Just (Shift 223)
    (62, Token (WHERE _)) -> Just (Shift 122)
    (62, Token (QCONID _)) -> Just (Shift 239)
    (62, Token (RBRACE _)) -> Just (Reduce 0 53)
    (62, Token (SEMICOLON _)) -> Just (Reduce 0 53)
    (62, Token (LPAREN _)) -> Just (Shift 98)
    (62, Token (DARROW _)) -> Just (Shift 45)
    (62, Token (QVARID _)) -> Just (Shift 224)
    (62, Token (LBRACKET _)) -> Just (Shift 100)
    (62, Token (EXCL _)) -> Just (Shift 223)
    (63, Token (RBRACE _)) -> Just (Reduce 4 7)
    (63, Token (SEMICOLON _)) -> Just (Reduce 4 7)
    (64, Token (RBRACE _)) -> Just (Reduce 3 8)
    (64, Token (SEMICOLON _)) -> Just (Reduce 3 8)
    (64, Token (DERIVING _)) -> Just (Shift 23)
    (65, Token (RBRACE _)) -> Just (Reduce 5 12)
    (65, Token (SEMICOLON _)) -> Just (Reduce 5 12)
    (65, Token (DERIVING _)) -> Just (Shift 24)
    (66, Token (RBRACE _)) -> Just (Reduce 5 9)
    (66, Token (SEMICOLON _)) -> Just (Reduce 5 9)
    (67, Token (RBRACE _)) -> Just (Reduce 7 13)
    (67, Token (SEMICOLON _)) -> Just (Reduce 7 13)
    (68, Token (RBRACE _)) -> Just (Reduce 7 21)
    (68, Token (SEMICOLON _)) -> Just (Reduce 7 21)
    (69, Token (RBRACE _)) -> Just (Reduce 5 17)
    (69, Token (SEMICOLON _)) -> Just (Reduce 5 17)
    (70, Token (RPAREN _)) -> Just (Shift 37)
    (71, Token (RPAREN _)) -> Just (Shift 38)
    (72, Token (RPAREN _)) -> Just (Shift 39)
    (73, Token (RPAREN _)) -> Just (Shift 40)
    (74, Token (RBRACE _)) -> Just (Reduce 5 20)
    (74, Token (SEMICOLON _)) -> Just (Reduce 5 20)
    (74, Token (DERIVING _)) -> Just (Shift 25)
    (75, Token (RBRACE _)) -> Just (Reduce 3 16)
    (75, Token (SEMICOLON _)) -> Just (Reduce 3 16)
    (75, Token (DERIVING _)) -> Just (Shift 26)
    (76, Token (RBRACE _)) -> Just (Reduce 5 25)
    (76, Token (SEMICOLON _)) -> Just (Reduce 5 25)
    (77, Token (RBRACE _)) -> Just (Reduce 3 24)
    (77, Token (SEMICOLON _)) -> Just (Reduce 3 24)
    (78, Token (RBRACE _)) -> Just (Reduce 5 27)
    (78, Token (SEMICOLON _)) -> Just (Reduce 5 27)
    (79, Token (RBRACE _)) -> Just (Reduce 3 26)
    (79, Token (SEMICOLON _)) -> Just (Reduce 3 26)
    (80, Token (RPAREN _)) -> Just (Shift 41)
    (81, Token (RBRACE _)) -> Just (Reduce 2 30)
    (81, Token (SEMICOLON _)) -> Just (Reduce 2 30)
    (82, Token (RBRACE _)) -> Just (Reduce 1 31)
    (82, Token (SEMICOLON _)) -> Just (Reduce 1 31)
    (83, Token (QCONID _)) -> Just (Reduce 1 82)
    (84, Token (QCONID _)) -> Just (Reduce 2 84)
    (84, Token (RBRACE _)) -> Just (Reduce 2 84)
    (84, Token (SEMICOLON _)) -> Just (Reduce 2 84)
    (84, Token (LPAREN _)) -> Just (Reduce 2 84)
    (84, Token (QVARID _)) -> Just (Reduce 2 84)
    (84, Token (HIDING _)) -> Just (Reduce 2 84)
    (84, Token (COMMA _)) -> Just (Reduce 2 84)
    (84, Token (QVARSYM _)) -> Just (Reduce 2 84)
    (84, Token (QCONSYM _)) -> Just (Reduce 2 84)
    (85, Token (RBRACE _)) -> Just (Reduce 1 86)
    (85, Token (SEMICOLON _)) -> Just (Reduce 1 86)
    (86, Token (QCONID _)) -> Just (Shift 239)
    (86, Token (LPAREN _)) -> Just (Shift 98)
    (86, Token (QVARID _)) -> Just (Shift 224)
    (86, Token (LBRACKET _)) -> Just (Shift 100)
    (86, Token (EXCL _)) -> Just (Shift 223)
    (87, Token (QCONID _)) -> Just (Shift 239)
    (87, Token (LPAREN _)) -> Just (Shift 98)
    (87, Token (QVARID _)) -> Just (Shift 224)
    (87, Token (LBRACKET _)) -> Just (Shift 100)
    (87, Token (EXCL _)) -> Just (Shift 223)
    (88, Token (QCONID _)) -> Just (Shift 239)
    (88, Token (LPAREN _)) -> Just (Shift 98)
    (88, Token (QVARID _)) -> Just (Shift 224)
    (88, Token (LBRACKET _)) -> Just (Shift 100)
    (88, Token (EXCL _)) -> Just (Shift 223)
    (89, Token (QCONID _)) -> Just (Shift 239)
    (89, Token (LPAREN _)) -> Just (Shift 98)
    (89, Token (QVARID _)) -> Just (Shift 224)
    (89, Token (LBRACKET _)) -> Just (Shift 100)
    (89, Token (EXCL _)) -> Just (Shift 223)
    (90, Token (QCONID _)) -> Just (Shift 239)
    (90, Token (LPAREN _)) -> Just (Shift 98)
    (90, Token (QVARID _)) -> Just (Shift 224)
    (90, Token (LBRACKET _)) -> Just (Shift 100)
    (90, Token (EXCL _)) -> Just (Shift 223)
    (91, Token (QCONID _)) -> Just (Shift 239)
    (91, Token (LPAREN _)) -> Just (Shift 98)
    (91, Token (QVARID _)) -> Just (Shift 224)
    (91, Token (LBRACKET _)) -> Just (Shift 100)
    (91, Token (EXCL _)) -> Just (Shift 223)
    (92, Token (QCONID _)) -> Just (Shift 239)
    (92, Token (LPAREN _)) -> Just (Shift 98)
    (92, Token (QVARID _)) -> Just (Shift 224)
    (92, Token (LBRACKET _)) -> Just (Shift 100)
    (92, Token (EXCL _)) -> Just (Shift 223)
    (93, Token (QCONID _)) -> Just (Shift 239)
    (93, Token (LPAREN _)) -> Just (Shift 98)
    (93, Token (QVARID _)) -> Just (Shift 224)
    (93, Token (LBRACKET _)) -> Just (Shift 100)
    (93, Token (EXCL _)) -> Just (Shift 223)
    (94, Token (QCONID _)) -> Just (Shift 239)
    (94, Token (LPAREN _)) -> Just (Shift 98)
    (94, Token (QVARID _)) -> Just (Shift 224)
    (94, Token (LBRACKET _)) -> Just (Shift 100)
    (94, Token (EXCL _)) -> Just (Shift 223)
    (95, Token (QCONID _)) -> Just (Shift 239)
    (95, Token (LPAREN _)) -> Just (Shift 98)
    (95, Token (QVARID _)) -> Just (Shift 224)
    (95, Token (LBRACKET _)) -> Just (Shift 100)
    (95, Token (EXCL _)) -> Just (Shift 223)
    (96, Token (QCONID _)) -> Just (Shift 239)
    (96, Token (LPAREN _)) -> Just (Shift 98)
    (96, Token (QVARID _)) -> Just (Shift 224)
    (96, Token (LBRACKET _)) -> Just (Shift 100)
    (96, Token (EXCL _)) -> Just (Shift 223)
    (97, Token (QCONID _)) -> Just (Shift 239)
    (97, Token (LPAREN _)) -> Just (Shift 98)
    (97, Token (QVARID _)) -> Just (Shift 224)
    (97, Token (LBRACKET _)) -> Just (Shift 100)
    (97, Token (EXCL _)) -> Just (Shift 223)
    (98, Token (QCONID _)) -> Just (Shift 239)
    (98, Token (LPAREN _)) -> Just (Shift 98)
    (98, Token (RPAREN _)) -> Just (Shift 268)
    (98, Token (QVARID _)) -> Just (Shift 224)
    (98, Token (COMMA _)) -> Just (Shift 278)
    (98, Token (ARROW _)) -> Just (Shift 271)
    (98, Token (LBRACKET _)) -> Just (Shift 100)
    (98, Token (EXCL _)) -> Just (Shift 223)
    (98, Token (QCONSYM _)) -> Just (Shift 242)
    (99, Token (QCONID _)) -> Just (Shift 239)
    (99, Token (LPAREN _)) -> Just (Shift 98)
    (99, Token (QVARID _)) -> Just (Shift 224)
    (99, Token (LBRACKET _)) -> Just (Shift 100)
    (99, Token (EXCL _)) -> Just (Shift 223)
    (100, Token (QCONID _)) -> Just (Shift 239)
    (100, Token (LPAREN _)) -> Just (Shift 98)
    (100, Token (QVARID _)) -> Just (Shift 224)
    (100, Token (LBRACKET _)) -> Just (Shift 100)
    (100, Token (RBRACKET _)) -> Just (Shift 272)
    (100, Token (EXCL _)) -> Just (Shift 223)
    (101, Token (QCONID _)) -> Just (Shift 239)
    (101, Token (RBRACE _)) -> Just (Reduce 1 138)
    (101, Token (SEMICOLON _)) -> Just (Reduce 1 138)
    (101, Token (DERIVING _)) -> Just (Reduce 1 138)
    (101, Token (LPAREN _)) -> Just (Shift 98)
    (101, Token (QVARID _)) -> Just (Shift 224)
    (101, Token (LBRACKET _)) -> Just (Shift 100)
    (101, Token (EXCL _)) -> Just (Shift 223)
    (101, Token (PIPE _)) -> Just (Reduce 1 138)
    (101, Token (QCONSYM _)) -> Just (Shift 286)
    (101, Token (BACKQUOTE _)) -> Just (Shift 287)
    (102, Token (QCONID _)) -> Just (Shift 239)
    (102, Token (RBRACE _)) -> Just (Reduce 3 139)
    (102, Token (SEMICOLON _)) -> Just (Reduce 3 139)
    (102, Token (DERIVING _)) -> Just (Reduce 3 139)
    (102, Token (LPAREN _)) -> Just (Shift 98)
    (102, Token (QVARID _)) -> Just (Shift 224)
    (102, Token (LBRACKET _)) -> Just (Shift 100)
    (102, Token (EXCL _)) -> Just (Shift 223)
    (102, Token (PIPE _)) -> Just (Reduce 3 139)
    (103, Token (QCONID _)) -> Just (Shift 239)
    (103, Token (RBRACE _)) -> Just (Reduce 1 115)
    (103, Token (SEMICOLON _)) -> Just (Reduce 1 115)
    (103, Token (LPAREN _)) -> Just (Shift 98)
    (103, Token (DARROW _)) -> Just (Shift 94)
    (103, Token (QVARID _)) -> Just (Shift 224)
    (103, Token (ARROW _)) -> Just (Shift 90)
    (103, Token (LBRACKET _)) -> Just (Shift 100)
    (103, Token (EXCL _)) -> Just (Shift 223)
    (104, Token (QCONID _)) -> Just (Shift 239)
    (104, Token (RBRACE _)) -> Just (Reduce 1 115)
    (104, Token (SEMICOLON _)) -> Just (Reduce 1 115)
    (104, Token (LPAREN _)) -> Just (Shift 98)
    (104, Token (RPAREN _)) -> Just (Reduce 1 115)
    (104, Token (QVARID _)) -> Just (Shift 224)
    (104, Token (COMMA _)) -> Just (Reduce 1 115)
    (104, Token (ARROW _)) -> Just (Shift 90)
    (104, Token (LBRACKET _)) -> Just (Shift 100)
    (104, Token (RBRACKET _)) -> Just (Reduce 1 115)
    (104, Token (EXCL _)) -> Just (Shift 223)
    (105, Token (WHERE _)) -> Just (Reduce 1 117)
    (105, Token (QCONID _)) -> Just (Reduce 1 117)
    (105, Token (LBRACE _)) -> Just (Reduce 1 117)
    (105, Token (RBRACE _)) -> Just (Reduce 1 117)
    (105, Token (SEMICOLON _)) -> Just (Reduce 1 117)
    (105, Token (EQUAL _)) -> Just (Reduce 1 117)
    (105, Token (DERIVING _)) -> Just (Reduce 1 117)
    (105, Token (LPAREN _)) -> Just (Reduce 1 117)
    (105, Token (RPAREN _)) -> Just (Reduce 1 117)
    (105, Token (DARROW _)) -> Just (Reduce 1 117)
    (105, Token (COLON_COLON _)) -> Just (Reduce 1 117)
    (105, Token (QVARID _)) -> Just (Reduce 1 117)
    (105, Token (INTEGER _)) -> Just (Reduce 1 117)
    (105, Token (TODO_FUNLHS _)) -> Just (Reduce 1 117)
    (105, Token (COMMA _)) -> Just (Reduce 1 117)
    (105, Token (ARROW _)) -> Just (Reduce 1 117)
    (105, Token (LBRACKET _)) -> Just (Reduce 1 117)
    (105, Token (RBRACKET _)) -> Just (Reduce 1 117)
    (105, Token (EXCL _)) -> Just (Reduce 1 117)
    (105, Token (PIPE _)) -> Just (Reduce 1 117)
    (105, Token (QVARSYM _)) -> Just (Reduce 1 117)
    (105, Token (QCONSYM _)) -> Just (Reduce 1 117)
    (105, Token (INFIXL _)) -> Just (Reduce 1 117)
    (105, Token (INFIXR _)) -> Just (Reduce 1 117)
    (105, Token (INFIX _)) -> Just (Reduce 1 117)
    (105, Token (BACKQUOTE _)) -> Just (Reduce 1 117)
    (106, Token (WHERE _)) -> Just (Reduce 2 118)
    (106, Token (QCONID _)) -> Just (Reduce 2 118)
    (106, Token (LBRACE _)) -> Just (Reduce 2 118)
    (106, Token (RBRACE _)) -> Just (Reduce 2 118)
    (106, Token (SEMICOLON _)) -> Just (Reduce 2 118)
    (106, Token (EQUAL _)) -> Just (Reduce 2 118)
    (106, Token (DERIVING _)) -> Just (Reduce 2 118)
    (106, Token (LPAREN _)) -> Just (Reduce 2 118)
    (106, Token (RPAREN _)) -> Just (Reduce 2 118)
    (106, Token (DARROW _)) -> Just (Reduce 2 118)
    (106, Token (COLON_COLON _)) -> Just (Reduce 2 118)
    (106, Token (QVARID _)) -> Just (Reduce 2 118)
    (106, Token (INTEGER _)) -> Just (Reduce 2 118)
    (106, Token (TODO_FUNLHS _)) -> Just (Reduce 2 118)
    (106, Token (COMMA _)) -> Just (Reduce 2 118)
    (106, Token (ARROW _)) -> Just (Reduce 2 118)
    (106, Token (LBRACKET _)) -> Just (Reduce 2 118)
    (106, Token (RBRACKET _)) -> Just (Reduce 2 118)
    (106, Token (EXCL _)) -> Just (Reduce 2 118)
    (106, Token (PIPE _)) -> Just (Reduce 2 118)
    (106, Token (QVARSYM _)) -> Just (Reduce 2 118)
    (106, Token (QCONSYM _)) -> Just (Reduce 2 118)
    (106, Token (INFIXL _)) -> Just (Reduce 2 118)
    (106, Token (INFIXR _)) -> Just (Reduce 2 118)
    (106, Token (INFIX _)) -> Just (Reduce 2 118)
    (106, Token (BACKQUOTE _)) -> Just (Reduce 2 118)
    (107, Token (RBRACE _)) -> Just (Reduce 3 116)
    (107, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (107, Token (RPAREN _)) -> Just (Reduce 3 116)
    (107, Token (COMMA _)) -> Just (Reduce 3 116)
    (107, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (108, Token (RBRACE _)) -> Just (Reduce 2 135)
    (108, Token (SEMICOLON _)) -> Just (Reduce 2 135)
    (108, Token (DERIVING _)) -> Just (Reduce 2 135)
    (109, Token (QCONID _)) -> Just (Shift 110)
    (110, Token (RBRACE _)) -> Just (Reduce 1 167)
    (110, Token (SEMICOLON _)) -> Just (Reduce 1 167)
    (110, Token (RPAREN _)) -> Just (Reduce 1 167)
    (110, Token (COMMA _)) -> Just (Reduce 1 167)
    (111, Token (RPAREN _)) -> Just (Reduce 1 168)
    (111, Token (COMMA _)) -> Just (Shift 109)
    (112, Token (RPAREN _)) -> Just (Reduce 3 169)
    (113, Token (LPAREN _)) -> Just (Shift 167)
    (113, Token (QVARID _)) -> Just (Shift 170)
    (114, Token (RBRACE _)) -> Just (Reduce 7 149)
    (114, Token (SEMICOLON _)) -> Just (Reduce 7 149)
    (114, Token (DERIVING _)) -> Just (Reduce 7 149)
    (115, Token (QCONID _)) -> Just (Shift 239)
    (115, Token (LPAREN _)) -> Just (Shift 240)
    (116, Token (RBRACE _)) -> Just (Shift 114)
    (117, Token (COLON_COLON _)) -> Just (Shift 96)
    (118, Token (QCONID _)) -> Just (Shift 239)
    (118, Token (LBRACE _)) -> Just (Shift 113)
    (118, Token (LPAREN _)) -> Just (Shift 98)
    (118, Token (QVARID _)) -> Just (Shift 224)
    (118, Token (LBRACKET _)) -> Just (Shift 100)
    (118, Token (EXCL _)) -> Just (Shift 223)
    (119, Token (RBRACE _)) -> Just (Reduce 3 148)
    (119, Token (SEMICOLON _)) -> Just (Reduce 3 148)
    (119, Token (DERIVING _)) -> Just (Reduce 3 148)
    (120, Token (LBRACE _)) -> Just (Shift 152)
    (121, Token (RBRACE _)) -> Just (Reduce 2 46)
    (121, Token (SEMICOLON _)) -> Just (Reduce 2 46)
    (122, Token (LBRACE _)) -> Just (Shift 154)
    (123, Token (RBRACE _)) -> Just (Reduce 2 54)
    (123, Token (SEMICOLON _)) -> Just (Reduce 2 54)
    (124, Token (RPAREN _)) -> Just (Reduce 1 113)
    (124, Token (COMMA _)) -> Just (Shift 97)
    (125, Token (RPAREN _)) -> Just (Reduce 3 114)
    (126, Token (QVARID _)) -> Just (Shift 147)
    (127, Token (QVARID _)) -> Just (Shift 147)
    (128, Token (RBRACE _)) -> Just (Reduce 6 32)
    (128, Token (SEMICOLON _)) -> Just (Reduce 6 32)
    (129, Token (RBRACE _)) -> Just (Reduce 7 33)
    (129, Token (SEMICOLON _)) -> Just (Reduce 7 33)
    (130, Token (RBRACE _)) -> Just (Reduce 6 34)
    (130, Token (SEMICOLON _)) -> Just (Reduce 6 34)
    (131, Token (QVARID _)) -> Just (Shift 172)
    (131, Token (STRING _)) -> Just (Shift 148)
    (132, Token (STRING _)) -> Just (Shift 173)
    (133, Token (LPAREN _)) -> Just (Shift 167)
    (133, Token (QVARID _)) -> Just (Shift 170)
    (134, Token (LPAREN _)) -> Just (Shift 167)
    (134, Token (QVARID _)) -> Just (Shift 170)
    (135, Token (COLON_COLON _)) -> Just (Shift 91)
    (136, Token (COLON_COLON _)) -> Just (Shift 92)
    (137, Token (COLON_COLON _)) -> Just (Shift 93)
    (138, Token (STRING _)) -> Just (Shift 148)
    (139, Token (LPAREN _)) -> Just (Shift 167)
    (139, Token (QVARID _)) -> Just (Shift 170)
    (140, Token (RBRACE _)) -> Just (Reduce 0 61)
    (140, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (140, Token (LPAREN _)) -> Just (Shift 167)
    (140, Token (QVARID _)) -> Just (Shift 170)
    (140, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (140, Token (INFIXL _)) -> Just (Shift 207)
    (140, Token (INFIXR _)) -> Just (Shift 208)
    (140, Token (INFIX _)) -> Just (Shift 209)
    (141, Token (RBRACE _)) -> Just (Reduce 0 61)
    (141, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (141, Token (LPAREN _)) -> Just (Shift 167)
    (141, Token (QVARID _)) -> Just (Shift 170)
    (141, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (141, Token (INFIXL _)) -> Just (Shift 207)
    (141, Token (INFIXR _)) -> Just (Shift 208)
    (141, Token (INFIX _)) -> Just (Shift 209)
    (142, Token (EQUAL _)) -> Just (Shift 149)
    (142, Token (COLON_COLON _)) -> Just (Reduce 1 152)
    (142, Token (COMMA _)) -> Just (Shift 164)
    (143, Token (RBRACE _)) -> Just (Reduce 1 42)
    (143, Token (SEMICOLON _)) -> Just (Reduce 1 42)
    (144, Token (EQUAL _)) -> Just (Shift 149)
    (145, Token (RBRACE _)) -> Just (Reduce 2 44)
    (145, Token (SEMICOLON _)) -> Just (Reduce 2 44)
    (146, Token (RBRACE _)) -> Just (Reduce 2 43)
    (146, Token (SEMICOLON _)) -> Just (Reduce 2 43)
    (147, Token (QVARID _)) -> Just (Reduce 1 35)
    (147, Token (STRING _)) -> Just (Reduce 1 35)
    (148, Token (LPAREN _)) -> Just (Reduce 1 37)
    (148, Token (QVARID _)) -> Just (Reduce 1 37)
    (148, Token (QVARSYM _)) -> Just (Reduce 1 37)
    (149, Token (LPAREN _)) -> Just (Shift 167)
    (149, Token (QVARID _)) -> Just (Shift 170)
    (149, Token (INTEGER _)) -> Just (Shift 219)
    (150, Token (WHERE _)) -> Just (Reduce 1 72)
    (150, Token (RBRACE _)) -> Just (Reduce 1 72)
    (150, Token (SEMICOLON _)) -> Just (Reduce 1 72)
    (150, Token (LPAREN _)) -> Just (Shift 167)
    (150, Token (QVARID _)) -> Just (Shift 170)
    (150, Token (INTEGER _)) -> Just (Shift 219)
    (150, Token (QVARSYM _)) -> Just (Shift 295)
    (150, Token (QCONSYM _)) -> Just (Shift 286)
    (150, Token (BACKQUOTE _)) -> Just (Shift 288)
    (151, Token (LPAREN _)) -> Just (Shift 167)
    (151, Token (QVARID _)) -> Just (Shift 170)
    (151, Token (INTEGER _)) -> Just (Shift 219)
    (152, Token (RBRACE _)) -> Just (Reduce 0 61)
    (152, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (152, Token (LPAREN _)) -> Just (Shift 167)
    (152, Token (QVARID _)) -> Just (Shift 170)
    (152, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (152, Token (INFIXL _)) -> Just (Shift 207)
    (152, Token (INFIXR _)) -> Just (Shift 208)
    (152, Token (INFIX _)) -> Just (Shift 209)
    (153, Token (RBRACE _)) -> Just (Reduce 0 61)
    (153, Token (SEMICOLON _)) -> Just (Reduce 0 61)
    (153, Token (LPAREN _)) -> Just (Shift 167)
    (153, Token (QVARID _)) -> Just (Shift 170)
    (153, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (153, Token (INFIXL _)) -> Just (Shift 207)
    (153, Token (INFIXR _)) -> Just (Shift 208)
    (153, Token (INFIX _)) -> Just (Shift 209)
    (154, Token (RBRACE _)) -> Just (Reduce 0 58)
    (154, Token (SEMICOLON _)) -> Just (Reduce 0 58)
    (154, Token (LPAREN _)) -> Just (Shift 167)
    (154, Token (QVARID _)) -> Just (Shift 170)
    (154, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (155, Token (RBRACE _)) -> Just (Reduce 0 58)
    (155, Token (SEMICOLON _)) -> Just (Reduce 0 58)
    (155, Token (LPAREN _)) -> Just (Shift 167)
    (155, Token (QVARID _)) -> Just (Shift 170)
    (155, Token (TODO_FUNLHS _)) -> Just (Shift 185)
    (156, Token (QCONID _)) -> Just (Shift 239)
    (156, Token (LPAREN _)) -> Just (Shift 168)
    (156, Token (RPAREN _)) -> Just (Reduce 0 89)
    (156, Token (QVARID _)) -> Just (Shift 170)
    (157, Token (QCONID _)) -> Just (Shift 239)
    (157, Token (LPAREN _)) -> Just (Shift 168)
    (157, Token (RPAREN _)) -> Just (Reduce 0 89)
    (157, Token (QVARID _)) -> Just (Shift 170)
    (158, Token (QCONID _)) -> Just (Shift 239)
    (158, Token (LPAREN _)) -> Just (Shift 168)
    (158, Token (RPAREN _)) -> Just (Reduce 0 89)
    (158, Token (QVARID _)) -> Just (Shift 170)
    (159, Token (QCONID _)) -> Just (Shift 239)
    (159, Token (LPAREN _)) -> Just (Shift 168)
    (159, Token (RPAREN _)) -> Just (Shift 232)
    (159, Token (QVARID _)) -> Just (Shift 170)
    (159, Token (DOT_DOT _)) -> Just (Shift 235)
    (160, Token (MODULE _)) -> Just (Shift 10)
    (160, Token (QCONID _)) -> Just (Shift 239)
    (160, Token (LPAREN _)) -> Just (Shift 168)
    (160, Token (RPAREN _)) -> Just (Reduce 0 100)
    (160, Token (QVARID _)) -> Just (Shift 170)
    (161, Token (MODULE _)) -> Just (Shift 10)
    (161, Token (QCONID _)) -> Just (Shift 239)
    (161, Token (LPAREN _)) -> Just (Shift 168)
    (161, Token (RPAREN _)) -> Just (Reduce 0 100)
    (161, Token (QVARID _)) -> Just (Shift 170)
    (162, Token (QCONID _)) -> Just (Shift 239)
    (162, Token (LPAREN _)) -> Just (Shift 168)
    (162, Token (QVARID _)) -> Just (Shift 170)
    (163, Token (QCONID _)) -> Just (Shift 239)
    (163, Token (LPAREN _)) -> Just (Shift 168)
    (163, Token (RPAREN _)) -> Just (Shift 249)
    (163, Token (QVARID _)) -> Just (Shift 170)
    (163, Token (DOT_DOT _)) -> Just (Shift 252)
    (164, Token (LPAREN _)) -> Just (Shift 167)
    (164, Token (QVARID _)) -> Just (Shift 170)
    (165, Token (RBRACE _)) -> Just (Shift 282)
    (165, Token (LPAREN _)) -> Just (Shift 167)
    (165, Token (QVARID _)) -> Just (Shift 170)
    (166, Token (LPAREN _)) -> Just (Shift 167)
    (166, Token (QVARID _)) -> Just (Shift 170)
    (167, Token (QVARSYM _)) -> Just (Shift 171)
    (168, Token (QVARSYM _)) -> Just (Shift 171)
    (168, Token (QCONSYM _)) -> Just (Shift 242)
    (169, Token (WHERE _)) -> Just (Reduce 3 155)
    (169, Token (QCONID _)) -> Just (Reduce 3 155)
    (169, Token (RBRACE _)) -> Just (Reduce 3 155)
    (169, Token (SEMICOLON _)) -> Just (Reduce 3 155)
    (169, Token (EQUAL _)) -> Just (Reduce 3 155)
    (169, Token (LPAREN _)) -> Just (Reduce 3 155)
    (169, Token (RPAREN _)) -> Just (Reduce 3 155)
    (169, Token (COLON_COLON _)) -> Just (Reduce 3 155)
    (169, Token (QVARID _)) -> Just (Reduce 3 155)
    (169, Token (INTEGER _)) -> Just (Reduce 3 155)
    (169, Token (COMMA _)) -> Just (Reduce 3 155)
    (169, Token (QVARSYM _)) -> Just (Reduce 3 155)
    (169, Token (QCONSYM _)) -> Just (Reduce 3 155)
    (169, Token (BACKQUOTE _)) -> Just (Reduce 3 155)
    (170, Token (WHERE _)) -> Just (Reduce 1 154)
    (170, Token (QCONID _)) -> Just (Reduce 1 154)
    (170, Token (RBRACE _)) -> Just (Reduce 1 154)
    (170, Token (SEMICOLON _)) -> Just (Reduce 1 154)
    (170, Token (EQUAL _)) -> Just (Reduce 1 154)
    (170, Token (LPAREN _)) -> Just (Reduce 1 154)
    (170, Token (RPAREN _)) -> Just (Reduce 1 154)
    (170, Token (COLON_COLON _)) -> Just (Reduce 1 154)
    (170, Token (QVARID _)) -> Just (Reduce 1 154)
    (170, Token (INTEGER _)) -> Just (Reduce 1 154)
    (170, Token (COMMA _)) -> Just (Reduce 1 154)
    (170, Token (QVARSYM _)) -> Just (Reduce 1 154)
    (170, Token (QCONSYM _)) -> Just (Reduce 1 154)
    (170, Token (BACKQUOTE _)) -> Just (Reduce 1 154)
    (171, Token (RPAREN _)) -> Just (Shift 169)
    (172, Token (STRING _)) -> Just (Reduce 1 36)
    (173, Token (LPAREN _)) -> Just (Reduce 1 38)
    (173, Token (QVARID _)) -> Just (Reduce 1 38)
    (173, Token (QVARSYM _)) -> Just (Reduce 1 38)
    (174, Token (LBRACE _)) -> Just (Shift 140)
    (175, Token (RBRACE _)) -> Just (Reduce 3 39)
    (175, Token (SEMICOLON _)) -> Just (Reduce 3 39)
    (176, Token (RBRACE _)) -> Just (Shift 175)
    (177, Token (RBRACE _)) -> Just (Reduce 1 40)
    (177, Token (SEMICOLON _)) -> Just (Shift 141)
    (178, Token (RBRACE _)) -> Just (Reduce 3 41)
    (179, Token (RBRACE _)) -> Just (Reduce 5 63)
    (179, Token (SEMICOLON _)) -> Just (Reduce 5 63)
    (180, Token (RBRACE _)) -> Just (Reduce 3 62)
    (180, Token (SEMICOLON _)) -> Just (Reduce 3 62)
    (181, Token (COLON_COLON _)) -> Just (Shift 89)
    (182, Token (QCONID _)) -> Just (Reduce 0 65)
    (182, Token (QVARID _)) -> Just (Reduce 0 65)
    (182, Token (INTEGER _)) -> Just (Shift 210)
    (182, Token (COMMA _)) -> Just (Reduce 0 65)
    (182, Token (QVARSYM _)) -> Just (Reduce 0 65)
    (182, Token (QCONSYM _)) -> Just (Reduce 0 65)
    (182, Token (BACKQUOTE _)) -> Just (Reduce 0 65)
    (183, Token (QVARSYM _)) -> Just (Shift 295)
    (183, Token (QCONSYM _)) -> Just (Shift 286)
    (183, Token (BACKQUOTE _)) -> Just (Shift 288)
    (184, Token (RBRACE _)) -> Just (Reduce 3 64)
    (184, Token (SEMICOLON _)) -> Just (Reduce 3 64)
    (185, Token (QCONID _)) -> Just (Reduce 1 67)
    (185, Token (EQUAL _)) -> Just (Reduce 1 67)
    (185, Token (LPAREN _)) -> Just (Reduce 1 67)
    (185, Token (QVARID _)) -> Just (Reduce 1 67)
    (185, Token (INTEGER _)) -> Just (Reduce 1 67)
    (185, Token (QVARSYM _)) -> Just (Reduce 1 67)
    (185, Token (QCONSYM _)) -> Just (Reduce 1 67)
    (185, Token (BACKQUOTE _)) -> Just (Reduce 1 67)
    (186, Token (EQUAL _)) -> Just (Shift 149)
    (186, Token (COLON_COLON _)) -> Just (Reduce 1 152)
    (186, Token (COMMA _)) -> Just (Shift 164)
    (187, Token (EQUAL _)) -> Just (Shift 149)
    (188, Token (EQUAL _)) -> Just (Shift 149)
    (189, Token (EQUAL _)) -> Just (Shift 149)
    (190, Token (RBRACE _)) -> Just (Reduce 4 69)
    (190, Token (SEMICOLON _)) -> Just (Reduce 4 69)
    (191, Token (WHERE _)) -> Just (Shift 174)
    (191, Token (RBRACE _)) -> Just (Reduce 2 68)
    (191, Token (SEMICOLON _)) -> Just (Reduce 2 68)
    (192, Token (RBRACE _)) -> Just (Reduce 3 47)
    (192, Token (SEMICOLON _)) -> Just (Reduce 3 47)
    (193, Token (RBRACE _)) -> Just (Shift 192)
    (194, Token (RBRACE _)) -> Just (Reduce 3 49)
    (195, Token (RBRACE _)) -> Just (Reduce 1 48)
    (195, Token (SEMICOLON _)) -> Just (Shift 153)
    (196, Token (RBRACE _)) -> Just (Reduce 1 50)
    (196, Token (SEMICOLON _)) -> Just (Reduce 1 50)
    (197, Token (RBRACE _)) -> Just (Reduce 2 52)
    (197, Token (SEMICOLON _)) -> Just (Reduce 2 52)
    (198, Token (RBRACE _)) -> Just (Reduce 2 51)
    (198, Token (SEMICOLON _)) -> Just (Reduce 2 51)
    (199, Token (RBRACE _)) -> Just (Reduce 3 55)
    (199, Token (SEMICOLON _)) -> Just (Reduce 3 55)
    (200, Token (RBRACE _)) -> Just (Shift 199)
    (201, Token (RBRACE _)) -> Just (Reduce 3 57)
    (202, Token (RBRACE _)) -> Just (Reduce 1 56)
    (202, Token (SEMICOLON _)) -> Just (Shift 155)
    (203, Token (RBRACE _)) -> Just (Reduce 2 60)
    (203, Token (SEMICOLON _)) -> Just (Reduce 2 60)
    (204, Token (RBRACE _)) -> Just (Reduce 2 59)
    (204, Token (SEMICOLON _)) -> Just (Reduce 2 59)
    (205, Token (COLON_COLON _)) -> Just (Reduce 1 152)
    (205, Token (COMMA _)) -> Just (Shift 164)
    (206, Token (COLON_COLON _)) -> Just (Reduce 3 153)
    (207, Token (QCONID _)) -> Just (Reduce 1 158)
    (207, Token (QVARID _)) -> Just (Reduce 1 158)
    (207, Token (INTEGER _)) -> Just (Reduce 1 158)
    (207, Token (COMMA _)) -> Just (Reduce 1 158)
    (207, Token (QVARSYM _)) -> Just (Reduce 1 158)
    (207, Token (QCONSYM _)) -> Just (Reduce 1 158)
    (207, Token (BACKQUOTE _)) -> Just (Reduce 1 158)
    (208, Token (QCONID _)) -> Just (Reduce 1 159)
    (208, Token (QVARID _)) -> Just (Reduce 1 159)
    (208, Token (INTEGER _)) -> Just (Reduce 1 159)
    (208, Token (COMMA _)) -> Just (Reduce 1 159)
    (208, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (208, Token (QCONSYM _)) -> Just (Reduce 1 159)
    (208, Token (BACKQUOTE _)) -> Just (Reduce 1 159)
    (209, Token (QCONID _)) -> Just (Reduce 1 160)
    (209, Token (QVARID _)) -> Just (Reduce 1 160)
    (209, Token (INTEGER _)) -> Just (Reduce 1 160)
    (209, Token (COMMA _)) -> Just (Reduce 1 160)
    (209, Token (QVARSYM _)) -> Just (Reduce 1 160)
    (209, Token (QCONSYM _)) -> Just (Reduce 1 160)
    (209, Token (BACKQUOTE _)) -> Just (Reduce 1 160)
    (210, Token (QCONID _)) -> Just (Reduce 1 66)
    (210, Token (QVARID _)) -> Just (Reduce 1 66)
    (210, Token (COMMA _)) -> Just (Reduce 1 66)
    (210, Token (QVARSYM _)) -> Just (Reduce 1 66)
    (210, Token (QCONSYM _)) -> Just (Reduce 1 66)
    (210, Token (BACKQUOTE _)) -> Just (Reduce 1 66)
    (211, Token (QVARSYM _)) -> Just (Shift 295)
    (211, Token (QCONSYM _)) -> Just (Shift 286)
    (211, Token (BACKQUOTE _)) -> Just (Shift 288)
    (212, Token (RBRACE _)) -> Just (Reduce 3 151)
    (212, Token (SEMICOLON _)) -> Just (Reduce 3 151)
    (213, Token (RBRACE _)) -> Just (Reduce 1 150)
    (213, Token (SEMICOLON _)) -> Just (Reduce 1 150)
    (213, Token (COMMA _)) -> Just (Shift 211)
    (214, Token (WHERE _)) -> Just (Reduce 1 70)
    (214, Token (RBRACE _)) -> Just (Reduce 1 70)
    (214, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (215, Token (WHERE _)) -> Just (Reduce 1 71)
    (215, Token (RBRACE _)) -> Just (Reduce 1 71)
    (215, Token (SEMICOLON _)) -> Just (Reduce 1 71)
    (216, Token (WHERE _)) -> Just (Reduce 1 73)
    (216, Token (QCONID _)) -> Just (Reduce 1 73)
    (216, Token (RBRACE _)) -> Just (Reduce 1 73)
    (216, Token (SEMICOLON _)) -> Just (Reduce 1 73)
    (216, Token (LPAREN _)) -> Just (Reduce 1 73)
    (216, Token (QVARID _)) -> Just (Reduce 1 73)
    (216, Token (INTEGER _)) -> Just (Reduce 1 73)
    (216, Token (QVARSYM _)) -> Just (Reduce 1 73)
    (216, Token (QCONSYM _)) -> Just (Reduce 1 73)
    (216, Token (BACKQUOTE _)) -> Just (Reduce 1 73)
    (217, Token (WHERE _)) -> Just (Reduce 2 74)
    (217, Token (QCONID _)) -> Just (Reduce 2 74)
    (217, Token (RBRACE _)) -> Just (Reduce 2 74)
    (217, Token (SEMICOLON _)) -> Just (Reduce 2 74)
    (217, Token (LPAREN _)) -> Just (Reduce 2 74)
    (217, Token (QVARID _)) -> Just (Reduce 2 74)
    (217, Token (INTEGER _)) -> Just (Reduce 2 74)
    (217, Token (QVARSYM _)) -> Just (Reduce 2 74)
    (217, Token (QCONSYM _)) -> Just (Reduce 2 74)
    (217, Token (BACKQUOTE _)) -> Just (Reduce 2 74)
    (218, Token (WHERE _)) -> Just (Reduce 3 75)
    (218, Token (QCONID _)) -> Just (Reduce 3 75)
    (218, Token (RBRACE _)) -> Just (Reduce 3 75)
    (218, Token (SEMICOLON _)) -> Just (Reduce 3 75)
    (218, Token (LPAREN _)) -> Just (Reduce 3 75)
    (218, Token (QVARID _)) -> Just (Reduce 3 75)
    (218, Token (INTEGER _)) -> Just (Reduce 3 75)
    (218, Token (QVARSYM _)) -> Just (Reduce 3 75)
    (218, Token (QCONSYM _)) -> Just (Reduce 3 75)
    (218, Token (BACKQUOTE _)) -> Just (Reduce 3 75)
    (219, Token (WHERE _)) -> Just (Reduce 1 77)
    (219, Token (QCONID _)) -> Just (Reduce 1 77)
    (219, Token (RBRACE _)) -> Just (Reduce 1 77)
    (219, Token (SEMICOLON _)) -> Just (Reduce 1 77)
    (219, Token (LPAREN _)) -> Just (Reduce 1 77)
    (219, Token (QVARID _)) -> Just (Reduce 1 77)
    (219, Token (INTEGER _)) -> Just (Reduce 1 77)
    (219, Token (QVARSYM _)) -> Just (Reduce 1 77)
    (219, Token (QCONSYM _)) -> Just (Reduce 1 77)
    (219, Token (BACKQUOTE _)) -> Just (Reduce 1 77)
    (220, Token (WHERE _)) -> Just (Reduce 1 76)
    (220, Token (QCONID _)) -> Just (Reduce 1 76)
    (220, Token (RBRACE _)) -> Just (Reduce 1 76)
    (220, Token (SEMICOLON _)) -> Just (Reduce 1 76)
    (220, Token (LPAREN _)) -> Just (Reduce 1 76)
    (220, Token (QVARID _)) -> Just (Reduce 1 76)
    (220, Token (INTEGER _)) -> Just (Reduce 1 76)
    (220, Token (QVARSYM _)) -> Just (Reduce 1 76)
    (220, Token (QCONSYM _)) -> Just (Reduce 1 76)
    (220, Token (BACKQUOTE _)) -> Just (Reduce 1 76)
    (221, Token (RBRACE _)) -> Just (Reduce 1 166)
    (221, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (221, Token (LPAREN _)) -> Just (Reduce 1 166)
    (221, Token (QVARID _)) -> Just (Reduce 1 166)
    (221, Token (INTEGER _)) -> Just (Reduce 1 166)
    (221, Token (COMMA _)) -> Just (Reduce 1 166)
    (221, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (222, Token (RBRACE _)) -> Just (Reduce 1 165)
    (222, Token (SEMICOLON _)) -> Just (Reduce 1 165)
    (222, Token (LPAREN _)) -> Just (Reduce 1 165)
    (222, Token (QVARID _)) -> Just (Reduce 1 165)
    (222, Token (INTEGER _)) -> Just (Reduce 1 165)
    (222, Token (COMMA _)) -> Just (Reduce 1 165)
    (222, Token (QVARSYM _)) -> Just (Reduce 1 165)
    (223, Token (QCONID _)) -> Just (Shift 239)
    (223, Token (LPAREN _)) -> Just (Shift 98)
    (223, Token (QVARID _)) -> Just (Shift 224)
    (223, Token (LBRACKET _)) -> Just (Shift 100)
    (223, Token (EXCL _)) -> Just (Shift 223)
    (224, Token (WHERE _)) -> Just (Reduce 1 80)
    (224, Token (QCONID _)) -> Just (Reduce 1 80)
    (224, Token (LBRACE _)) -> Just (Reduce 1 80)
    (224, Token (RBRACE _)) -> Just (Reduce 1 80)
    (224, Token (SEMICOLON _)) -> Just (Reduce 1 80)
    (224, Token (EQUAL _)) -> Just (Reduce 1 80)
    (224, Token (DERIVING _)) -> Just (Reduce 1 80)
    (224, Token (LPAREN _)) -> Just (Reduce 1 80)
    (224, Token (RPAREN _)) -> Just (Reduce 1 80)
    (224, Token (DARROW _)) -> Just (Reduce 1 80)
    (224, Token (COLON_COLON _)) -> Just (Reduce 1 80)
    (224, Token (QVARID _)) -> Just (Reduce 1 80)
    (224, Token (INTEGER _)) -> Just (Reduce 1 80)
    (224, Token (TODO_FUNLHS _)) -> Just (Reduce 1 80)
    (224, Token (COMMA _)) -> Just (Reduce 1 80)
    (224, Token (ARROW _)) -> Just (Reduce 1 80)
    (224, Token (LBRACKET _)) -> Just (Reduce 1 80)
    (224, Token (RBRACKET _)) -> Just (Reduce 1 80)
    (224, Token (EXCL _)) -> Just (Reduce 1 80)
    (224, Token (PIPE _)) -> Just (Reduce 1 80)
    (224, Token (QVARSYM _)) -> Just (Reduce 1 80)
    (224, Token (QCONSYM _)) -> Just (Reduce 1 80)
    (224, Token (INFIXL _)) -> Just (Reduce 1 80)
    (224, Token (INFIXR _)) -> Just (Reduce 1 80)
    (224, Token (INFIX _)) -> Just (Reduce 1 80)
    (224, Token (BACKQUOTE _)) -> Just (Reduce 1 80)
    (225, Token (RBRACE _)) -> Just (Reduce 3 87)
    (225, Token (SEMICOLON _)) -> Just (Reduce 3 87)
    (226, Token (RBRACE _)) -> Just (Reduce 4 88)
    (226, Token (SEMICOLON _)) -> Just (Reduce 4 88)
    (227, Token (LPAREN _)) -> Just (Shift 157)
    (228, Token (RPAREN _)) -> Just (Shift 225)
    (229, Token (RPAREN _)) -> Just (Shift 226)
    (230, Token (RPAREN _)) -> Just (Reduce 3 91)
    (231, Token (RPAREN _)) -> Just (Reduce 1 90)
    (231, Token (COMMA _)) -> Just (Shift 158)
    (232, Token (RPAREN _)) -> Just (Reduce 3 94)
    (232, Token (COMMA _)) -> Just (Reduce 3 94)
    (233, Token (RPAREN _)) -> Just (Reduce 4 95)
    (233, Token (COMMA _)) -> Just (Reduce 4 95)
    (234, Token (RPAREN _)) -> Just (Reduce 4 96)
    (234, Token (COMMA _)) -> Just (Reduce 4 96)
    (235, Token (RPAREN _)) -> Just (Shift 233)
    (236, Token (RPAREN _)) -> Just (Reduce 1 92)
    (236, Token (COMMA _)) -> Just (Reduce 1 92)
    (237, Token (LPAREN _)) -> Just (Shift 159)
    (237, Token (RPAREN _)) -> Just (Reduce 1 93)
    (237, Token (COMMA _)) -> Just (Reduce 1 93)
    (238, Token (RPAREN _)) -> Just (Shift 234)
    (239, Token (WHERE _)) -> Just (Reduce 1 156)
    (239, Token (QCONID _)) -> Just (Reduce 1 156)
    (239, Token (LBRACE _)) -> Just (Reduce 1 156)
    (239, Token (RBRACE _)) -> Just (Reduce 1 156)
    (239, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (239, Token (EQUAL _)) -> Just (Reduce 1 156)
    (239, Token (DERIVING _)) -> Just (Reduce 1 156)
    (239, Token (LPAREN _)) -> Just (Reduce 1 156)
    (239, Token (RPAREN _)) -> Just (Reduce 1 156)
    (239, Token (DARROW _)) -> Just (Reduce 1 156)
    (239, Token (COLON_COLON _)) -> Just (Reduce 1 156)
    (239, Token (QVARID _)) -> Just (Reduce 1 156)
    (239, Token (INTEGER _)) -> Just (Reduce 1 156)
    (239, Token (TODO_FUNLHS _)) -> Just (Reduce 1 156)
    (239, Token (COMMA _)) -> Just (Reduce 1 156)
    (239, Token (ARROW _)) -> Just (Reduce 1 156)
    (239, Token (LBRACKET _)) -> Just (Reduce 1 156)
    (239, Token (RBRACKET _)) -> Just (Reduce 1 156)
    (239, Token (EXCL _)) -> Just (Reduce 1 156)
    (239, Token (PIPE _)) -> Just (Reduce 1 156)
    (239, Token (QVARSYM _)) -> Just (Reduce 1 156)
    (239, Token (QCONSYM _)) -> Just (Reduce 1 156)
    (239, Token (INFIXL _)) -> Just (Reduce 1 156)
    (239, Token (INFIXR _)) -> Just (Reduce 1 156)
    (239, Token (INFIX _)) -> Just (Reduce 1 156)
    (239, Token (BACKQUOTE _)) -> Just (Reduce 1 156)
    (240, Token (QCONSYM _)) -> Just (Shift 242)
    (241, Token (WHERE _)) -> Just (Reduce 3 157)
    (241, Token (QCONID _)) -> Just (Reduce 3 157)
    (241, Token (LBRACE _)) -> Just (Reduce 3 157)
    (241, Token (RBRACE _)) -> Just (Reduce 3 157)
    (241, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (241, Token (EQUAL _)) -> Just (Reduce 3 157)
    (241, Token (DERIVING _)) -> Just (Reduce 3 157)
    (241, Token (LPAREN _)) -> Just (Reduce 3 157)
    (241, Token (RPAREN _)) -> Just (Reduce 3 157)
    (241, Token (DARROW _)) -> Just (Reduce 3 157)
    (241, Token (COLON_COLON _)) -> Just (Reduce 3 157)
    (241, Token (QVARID _)) -> Just (Reduce 3 157)
    (241, Token (INTEGER _)) -> Just (Reduce 3 157)
    (241, Token (TODO_FUNLHS _)) -> Just (Reduce 3 157)
    (241, Token (COMMA _)) -> Just (Reduce 3 157)
    (241, Token (ARROW _)) -> Just (Reduce 3 157)
    (241, Token (LBRACKET _)) -> Just (Reduce 3 157)
    (241, Token (RBRACKET _)) -> Just (Reduce 3 157)
    (241, Token (EXCL _)) -> Just (Reduce 3 157)
    (241, Token (PIPE _)) -> Just (Reduce 3 157)
    (241, Token (QVARSYM _)) -> Just (Reduce 3 157)
    (241, Token (QCONSYM _)) -> Just (Reduce 3 157)
    (241, Token (INFIXL _)) -> Just (Reduce 3 157)
    (241, Token (INFIXR _)) -> Just (Reduce 3 157)
    (241, Token (INFIX _)) -> Just (Reduce 3 157)
    (241, Token (BACKQUOTE _)) -> Just (Reduce 3 157)
    (242, Token (RPAREN _)) -> Just (Shift 241)
    (243, Token (RPAREN _)) -> Just (Reduce 3 110)
    (244, Token (RPAREN _)) -> Just (Reduce 1 109)
    (244, Token (COMMA _)) -> Just (Shift 162)
    (245, Token (WHERE _)) -> Just (Reduce 3 99)
    (246, Token (RPAREN _)) -> Just (Shift 245)
    (247, Token (RPAREN _)) -> Just (Reduce 3 102)
    (248, Token (RPAREN _)) -> Just (Reduce 1 101)
    (248, Token (COMMA _)) -> Just (Shift 161)
    (249, Token (RPAREN _)) -> Just (Reduce 3 105)
    (249, Token (COMMA _)) -> Just (Reduce 3 105)
    (250, Token (RPAREN _)) -> Just (Reduce 4 106)
    (250, Token (COMMA _)) -> Just (Reduce 4 106)
    (251, Token (RPAREN _)) -> Just (Reduce 4 107)
    (251, Token (COMMA _)) -> Just (Reduce 4 107)
    (252, Token (RPAREN _)) -> Just (Shift 250)
    (253, Token (RPAREN _)) -> Just (Reduce 2 108)
    (253, Token (COMMA _)) -> Just (Reduce 2 108)
    (254, Token (RPAREN _)) -> Just (Reduce 1 103)
    (254, Token (COMMA _)) -> Just (Reduce 1 103)
    (255, Token (LPAREN _)) -> Just (Shift 163)
    (255, Token (RPAREN _)) -> Just (Reduce 1 104)
    (255, Token (COMMA _)) -> Just (Reduce 1 104)
    (256, Token (RPAREN _)) -> Just (Shift 251)
    (257, Token (RPAREN _)) -> Just (Reduce 1 111)
    (257, Token (COMMA _)) -> Just (Reduce 1 111)
    (258, Token (RPAREN _)) -> Just (Reduce 1 112)
    (258, Token (COMMA _)) -> Just (Reduce 1 112)
    (259, Token (WHERE _)) -> Just (Reduce 3 123)
    (259, Token (QCONID _)) -> Just (Reduce 3 123)
    (259, Token (LBRACE _)) -> Just (Reduce 3 123)
    (259, Token (RBRACE _)) -> Just (Reduce 3 123)
    (259, Token (SEMICOLON _)) -> Just (Reduce 3 123)
    (259, Token (EQUAL _)) -> Just (Reduce 3 123)
    (259, Token (DERIVING _)) -> Just (Reduce 3 123)
    (259, Token (LPAREN _)) -> Just (Reduce 3 123)
    (259, Token (RPAREN _)) -> Just (Reduce 3 123)
    (259, Token (DARROW _)) -> Just (Reduce 3 123)
    (259, Token (COLON_COLON _)) -> Just (Reduce 3 123)
    (259, Token (QVARID _)) -> Just (Reduce 3 123)
    (259, Token (INTEGER _)) -> Just (Reduce 3 123)
    (259, Token (TODO_FUNLHS _)) -> Just (Reduce 3 123)
    (259, Token (COMMA _)) -> Just (Reduce 3 123)
    (259, Token (ARROW _)) -> Just (Reduce 3 123)
    (259, Token (LBRACKET _)) -> Just (Reduce 3 123)
    (259, Token (RBRACKET _)) -> Just (Reduce 3 123)
    (259, Token (EXCL _)) -> Just (Reduce 3 123)
    (259, Token (PIPE _)) -> Just (Reduce 3 123)
    (259, Token (QVARSYM _)) -> Just (Reduce 3 123)
    (259, Token (QCONSYM _)) -> Just (Reduce 3 123)
    (259, Token (INFIXL _)) -> Just (Reduce 3 123)
    (259, Token (INFIXR _)) -> Just (Reduce 3 123)
    (259, Token (INFIX _)) -> Just (Reduce 3 123)
    (259, Token (BACKQUOTE _)) -> Just (Reduce 3 123)
    (260, Token (WHERE _)) -> Just (Reduce 3 121)
    (260, Token (QCONID _)) -> Just (Reduce 3 121)
    (260, Token (LBRACE _)) -> Just (Reduce 3 121)
    (260, Token (RBRACE _)) -> Just (Reduce 3 121)
    (260, Token (SEMICOLON _)) -> Just (Reduce 3 121)
    (260, Token (EQUAL _)) -> Just (Reduce 3 121)
    (260, Token (DERIVING _)) -> Just (Reduce 3 121)
    (260, Token (LPAREN _)) -> Just (Reduce 3 121)
    (260, Token (RPAREN _)) -> Just (Reduce 3 121)
    (260, Token (DARROW _)) -> Just (Reduce 3 121)
    (260, Token (COLON_COLON _)) -> Just (Reduce 3 121)
    (260, Token (QVARID _)) -> Just (Reduce 3 121)
    (260, Token (INTEGER _)) -> Just (Reduce 3 121)
    (260, Token (TODO_FUNLHS _)) -> Just (Reduce 3 121)
    (260, Token (COMMA _)) -> Just (Reduce 3 121)
    (260, Token (ARROW _)) -> Just (Reduce 3 121)
    (260, Token (LBRACKET _)) -> Just (Reduce 3 121)
    (260, Token (RBRACKET _)) -> Just (Reduce 3 121)
    (260, Token (EXCL _)) -> Just (Reduce 3 121)
    (260, Token (PIPE _)) -> Just (Reduce 3 121)
    (260, Token (QVARSYM _)) -> Just (Reduce 3 121)
    (260, Token (QCONSYM _)) -> Just (Reduce 3 121)
    (260, Token (INFIXL _)) -> Just (Reduce 3 121)
    (260, Token (INFIXR _)) -> Just (Reduce 3 121)
    (260, Token (INFIX _)) -> Just (Reduce 3 121)
    (260, Token (BACKQUOTE _)) -> Just (Reduce 3 121)
    (261, Token (WHERE _)) -> Just (Reduce 3 122)
    (261, Token (QCONID _)) -> Just (Reduce 3 122)
    (261, Token (LBRACE _)) -> Just (Reduce 3 122)
    (261, Token (RBRACE _)) -> Just (Reduce 3 122)
    (261, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (261, Token (EQUAL _)) -> Just (Reduce 3 122)
    (261, Token (DERIVING _)) -> Just (Reduce 3 122)
    (261, Token (LPAREN _)) -> Just (Reduce 3 122)
    (261, Token (RPAREN _)) -> Just (Reduce 3 122)
    (261, Token (DARROW _)) -> Just (Reduce 3 122)
    (261, Token (COLON_COLON _)) -> Just (Reduce 3 122)
    (261, Token (QVARID _)) -> Just (Reduce 3 122)
    (261, Token (INTEGER _)) -> Just (Reduce 3 122)
    (261, Token (TODO_FUNLHS _)) -> Just (Reduce 3 122)
    (261, Token (COMMA _)) -> Just (Reduce 3 122)
    (261, Token (ARROW _)) -> Just (Reduce 3 122)
    (261, Token (LBRACKET _)) -> Just (Reduce 3 122)
    (261, Token (RBRACKET _)) -> Just (Reduce 3 122)
    (261, Token (EXCL _)) -> Just (Reduce 3 122)
    (261, Token (PIPE _)) -> Just (Reduce 3 122)
    (261, Token (QVARSYM _)) -> Just (Reduce 3 122)
    (261, Token (QCONSYM _)) -> Just (Reduce 3 122)
    (261, Token (INFIXL _)) -> Just (Reduce 3 122)
    (261, Token (INFIXR _)) -> Just (Reduce 3 122)
    (261, Token (INFIX _)) -> Just (Reduce 3 122)
    (261, Token (BACKQUOTE _)) -> Just (Reduce 3 122)
    (262, Token (RPAREN _)) -> Just (Shift 259)
    (262, Token (COMMA _)) -> Just (Shift 99)
    (263, Token (RBRACKET _)) -> Just (Shift 261)
    (264, Token (WHERE _)) -> Just (Reduce 1 120)
    (264, Token (QCONID _)) -> Just (Reduce 1 120)
    (264, Token (LBRACE _)) -> Just (Reduce 1 120)
    (264, Token (RBRACE _)) -> Just (Reduce 1 120)
    (264, Token (SEMICOLON _)) -> Just (Reduce 1 120)
    (264, Token (EQUAL _)) -> Just (Reduce 1 120)
    (264, Token (DERIVING _)) -> Just (Reduce 1 120)
    (264, Token (LPAREN _)) -> Just (Reduce 1 120)
    (264, Token (RPAREN _)) -> Just (Reduce 1 120)
    (264, Token (DARROW _)) -> Just (Reduce 1 120)
    (264, Token (COLON_COLON _)) -> Just (Reduce 1 120)
    (264, Token (QVARID _)) -> Just (Reduce 1 120)
    (264, Token (INTEGER _)) -> Just (Reduce 1 120)
    (264, Token (TODO_FUNLHS _)) -> Just (Reduce 1 120)
    (264, Token (COMMA _)) -> Just (Reduce 1 120)
    (264, Token (ARROW _)) -> Just (Reduce 1 120)
    (264, Token (LBRACKET _)) -> Just (Reduce 1 120)
    (264, Token (RBRACKET _)) -> Just (Reduce 1 120)
    (264, Token (EXCL _)) -> Just (Reduce 1 120)
    (264, Token (PIPE _)) -> Just (Reduce 1 120)
    (264, Token (QVARSYM _)) -> Just (Reduce 1 120)
    (264, Token (QCONSYM _)) -> Just (Reduce 1 120)
    (264, Token (INFIXL _)) -> Just (Reduce 1 120)
    (264, Token (INFIXR _)) -> Just (Reduce 1 120)
    (264, Token (INFIX _)) -> Just (Reduce 1 120)
    (264, Token (BACKQUOTE _)) -> Just (Reduce 1 120)
    (265, Token (WHERE _)) -> Just (Reduce 2 124)
    (265, Token (QCONID _)) -> Just (Reduce 2 124)
    (265, Token (LBRACE _)) -> Just (Reduce 2 124)
    (265, Token (RBRACE _)) -> Just (Reduce 2 124)
    (265, Token (SEMICOLON _)) -> Just (Reduce 2 124)
    (265, Token (EQUAL _)) -> Just (Reduce 2 124)
    (265, Token (DERIVING _)) -> Just (Reduce 2 124)
    (265, Token (LPAREN _)) -> Just (Reduce 2 124)
    (265, Token (RPAREN _)) -> Just (Reduce 2 124)
    (265, Token (DARROW _)) -> Just (Reduce 2 124)
    (265, Token (COLON_COLON _)) -> Just (Reduce 2 124)
    (265, Token (QVARID _)) -> Just (Reduce 2 124)
    (265, Token (INTEGER _)) -> Just (Reduce 2 124)
    (265, Token (TODO_FUNLHS _)) -> Just (Reduce 2 124)
    (265, Token (COMMA _)) -> Just (Reduce 2 124)
    (265, Token (ARROW _)) -> Just (Reduce 2 124)
    (265, Token (LBRACKET _)) -> Just (Reduce 2 124)
    (265, Token (RBRACKET _)) -> Just (Reduce 2 124)
    (265, Token (EXCL _)) -> Just (Reduce 2 124)
    (265, Token (PIPE _)) -> Just (Reduce 2 124)
    (265, Token (QVARSYM _)) -> Just (Reduce 2 124)
    (265, Token (QCONSYM _)) -> Just (Reduce 2 124)
    (265, Token (INFIXL _)) -> Just (Reduce 2 124)
    (265, Token (INFIXR _)) -> Just (Reduce 2 124)
    (265, Token (INFIX _)) -> Just (Reduce 2 124)
    (265, Token (BACKQUOTE _)) -> Just (Reduce 2 124)
    (266, Token (WHERE _)) -> Just (Reduce 1 119)
    (266, Token (QCONID _)) -> Just (Reduce 1 119)
    (266, Token (LBRACE _)) -> Just (Reduce 1 119)
    (266, Token (RBRACE _)) -> Just (Reduce 1 119)
    (266, Token (SEMICOLON _)) -> Just (Reduce 1 119)
    (266, Token (EQUAL _)) -> Just (Reduce 1 119)
    (266, Token (DERIVING _)) -> Just (Reduce 1 119)
    (266, Token (LPAREN _)) -> Just (Reduce 1 119)
    (266, Token (RPAREN _)) -> Just (Reduce 1 119)
    (266, Token (DARROW _)) -> Just (Reduce 1 119)
    (266, Token (COLON_COLON _)) -> Just (Reduce 1 119)
    (266, Token (QVARID _)) -> Just (Reduce 1 119)
    (266, Token (INTEGER _)) -> Just (Reduce 1 119)
    (266, Token (TODO_FUNLHS _)) -> Just (Reduce 1 119)
    (266, Token (COMMA _)) -> Just (Reduce 1 119)
    (266, Token (ARROW _)) -> Just (Reduce 1 119)
    (266, Token (LBRACKET _)) -> Just (Reduce 1 119)
    (266, Token (RBRACKET _)) -> Just (Reduce 1 119)
    (266, Token (EXCL _)) -> Just (Reduce 1 119)
    (266, Token (PIPE _)) -> Just (Reduce 1 119)
    (266, Token (QVARSYM _)) -> Just (Reduce 1 119)
    (266, Token (QCONSYM _)) -> Just (Reduce 1 119)
    (266, Token (INFIXL _)) -> Just (Reduce 1 119)
    (266, Token (INFIXR _)) -> Just (Reduce 1 119)
    (266, Token (INFIX _)) -> Just (Reduce 1 119)
    (266, Token (BACKQUOTE _)) -> Just (Reduce 1 119)
    (267, Token (RPAREN _)) -> Just (Shift 260)
    (268, Token (WHERE _)) -> Just (Reduce 2 128)
    (268, Token (QCONID _)) -> Just (Reduce 2 128)
    (268, Token (LBRACE _)) -> Just (Reduce 2 128)
    (268, Token (RBRACE _)) -> Just (Reduce 2 128)
    (268, Token (SEMICOLON _)) -> Just (Reduce 2 128)
    (268, Token (EQUAL _)) -> Just (Reduce 2 128)
    (268, Token (DERIVING _)) -> Just (Reduce 2 128)
    (268, Token (LPAREN _)) -> Just (Reduce 2 128)
    (268, Token (RPAREN _)) -> Just (Reduce 2 128)
    (268, Token (DARROW _)) -> Just (Reduce 2 128)
    (268, Token (COLON_COLON _)) -> Just (Reduce 2 128)
    (268, Token (QVARID _)) -> Just (Reduce 2 128)
    (268, Token (INTEGER _)) -> Just (Reduce 2 128)
    (268, Token (TODO_FUNLHS _)) -> Just (Reduce 2 128)
    (268, Token (COMMA _)) -> Just (Reduce 2 128)
    (268, Token (ARROW _)) -> Just (Reduce 2 128)
    (268, Token (LBRACKET _)) -> Just (Reduce 2 128)
    (268, Token (RBRACKET _)) -> Just (Reduce 2 128)
    (268, Token (EXCL _)) -> Just (Reduce 2 128)
    (268, Token (PIPE _)) -> Just (Reduce 2 128)
    (268, Token (QVARSYM _)) -> Just (Reduce 2 128)
    (268, Token (QCONSYM _)) -> Just (Reduce 2 128)
    (268, Token (INFIXL _)) -> Just (Reduce 2 128)
    (268, Token (INFIXR _)) -> Just (Reduce 2 128)
    (268, Token (INFIX _)) -> Just (Reduce 2 128)
    (268, Token (BACKQUOTE _)) -> Just (Reduce 2 128)
    (269, Token (WHERE _)) -> Just (Reduce 3 130)
    (269, Token (QCONID _)) -> Just (Reduce 3 130)
    (269, Token (LBRACE _)) -> Just (Reduce 3 130)
    (269, Token (RBRACE _)) -> Just (Reduce 3 130)
    (269, Token (SEMICOLON _)) -> Just (Reduce 3 130)
    (269, Token (EQUAL _)) -> Just (Reduce 3 130)
    (269, Token (DERIVING _)) -> Just (Reduce 3 130)
    (269, Token (LPAREN _)) -> Just (Reduce 3 130)
    (269, Token (RPAREN _)) -> Just (Reduce 3 130)
    (269, Token (DARROW _)) -> Just (Reduce 3 130)
    (269, Token (COLON_COLON _)) -> Just (Reduce 3 130)
    (269, Token (QVARID _)) -> Just (Reduce 3 130)
    (269, Token (INTEGER _)) -> Just (Reduce 3 130)
    (269, Token (TODO_FUNLHS _)) -> Just (Reduce 3 130)
    (269, Token (COMMA _)) -> Just (Reduce 3 130)
    (269, Token (ARROW _)) -> Just (Reduce 3 130)
    (269, Token (LBRACKET _)) -> Just (Reduce 3 130)
    (269, Token (RBRACKET _)) -> Just (Reduce 3 130)
    (269, Token (EXCL _)) -> Just (Reduce 3 130)
    (269, Token (PIPE _)) -> Just (Reduce 3 130)
    (269, Token (QVARSYM _)) -> Just (Reduce 3 130)
    (269, Token (QCONSYM _)) -> Just (Reduce 3 130)
    (269, Token (INFIXL _)) -> Just (Reduce 3 130)
    (269, Token (INFIXR _)) -> Just (Reduce 3 130)
    (269, Token (INFIX _)) -> Just (Reduce 3 130)
    (269, Token (BACKQUOTE _)) -> Just (Reduce 3 130)
    (270, Token (WHERE _)) -> Just (Reduce 3 131)
    (270, Token (QCONID _)) -> Just (Reduce 3 131)
    (270, Token (LBRACE _)) -> Just (Reduce 3 131)
    (270, Token (RBRACE _)) -> Just (Reduce 3 131)
    (270, Token (SEMICOLON _)) -> Just (Reduce 3 131)
    (270, Token (EQUAL _)) -> Just (Reduce 3 131)
    (270, Token (DERIVING _)) -> Just (Reduce 3 131)
    (270, Token (LPAREN _)) -> Just (Reduce 3 131)
    (270, Token (RPAREN _)) -> Just (Reduce 3 131)
    (270, Token (DARROW _)) -> Just (Reduce 3 131)
    (270, Token (COLON_COLON _)) -> Just (Reduce 3 131)
    (270, Token (QVARID _)) -> Just (Reduce 3 131)
    (270, Token (INTEGER _)) -> Just (Reduce 3 131)
    (270, Token (TODO_FUNLHS _)) -> Just (Reduce 3 131)
    (270, Token (COMMA _)) -> Just (Reduce 3 131)
    (270, Token (ARROW _)) -> Just (Reduce 3 131)
    (270, Token (LBRACKET _)) -> Just (Reduce 3 131)
    (270, Token (RBRACKET _)) -> Just (Reduce 3 131)
    (270, Token (EXCL _)) -> Just (Reduce 3 131)
    (270, Token (PIPE _)) -> Just (Reduce 3 131)
    (270, Token (QVARSYM _)) -> Just (Reduce 3 131)
    (270, Token (QCONSYM _)) -> Just (Reduce 3 131)
    (270, Token (INFIXL _)) -> Just (Reduce 3 131)
    (270, Token (INFIXR _)) -> Just (Reduce 3 131)
    (270, Token (INFIX _)) -> Just (Reduce 3 131)
    (270, Token (BACKQUOTE _)) -> Just (Reduce 3 131)
    (271, Token (RPAREN _)) -> Just (Shift 269)
    (272, Token (WHERE _)) -> Just (Reduce 2 129)
    (272, Token (QCONID _)) -> Just (Reduce 2 129)
    (272, Token (LBRACE _)) -> Just (Reduce 2 129)
    (272, Token (RBRACE _)) -> Just (Reduce 2 129)
    (272, Token (SEMICOLON _)) -> Just (Reduce 2 129)
    (272, Token (EQUAL _)) -> Just (Reduce 2 129)
    (272, Token (DERIVING _)) -> Just (Reduce 2 129)
    (272, Token (LPAREN _)) -> Just (Reduce 2 129)
    (272, Token (RPAREN _)) -> Just (Reduce 2 129)
    (272, Token (DARROW _)) -> Just (Reduce 2 129)
    (272, Token (COLON_COLON _)) -> Just (Reduce 2 129)
    (272, Token (QVARID _)) -> Just (Reduce 2 129)
    (272, Token (INTEGER _)) -> Just (Reduce 2 129)
    (272, Token (TODO_FUNLHS _)) -> Just (Reduce 2 129)
    (272, Token (COMMA _)) -> Just (Reduce 2 129)
    (272, Token (ARROW _)) -> Just (Reduce 2 129)
    (272, Token (LBRACKET _)) -> Just (Reduce 2 129)
    (272, Token (RBRACKET _)) -> Just (Reduce 2 129)
    (272, Token (EXCL _)) -> Just (Reduce 2 129)
    (272, Token (PIPE _)) -> Just (Reduce 2 129)
    (272, Token (QVARSYM _)) -> Just (Reduce 2 129)
    (272, Token (QCONSYM _)) -> Just (Reduce 2 129)
    (272, Token (INFIXL _)) -> Just (Reduce 2 129)
    (272, Token (INFIXR _)) -> Just (Reduce 2 129)
    (272, Token (INFIX _)) -> Just (Reduce 2 129)
    (272, Token (BACKQUOTE _)) -> Just (Reduce 2 129)
    (273, Token (WHERE _)) -> Just (Reduce 1 127)
    (273, Token (QCONID _)) -> Just (Reduce 1 127)
    (273, Token (LBRACE _)) -> Just (Reduce 1 127)
    (273, Token (RBRACE _)) -> Just (Reduce 1 127)
    (273, Token (SEMICOLON _)) -> Just (Reduce 1 127)
    (273, Token (EQUAL _)) -> Just (Reduce 1 127)
    (273, Token (DERIVING _)) -> Just (Reduce 1 127)
    (273, Token (LPAREN _)) -> Just (Reduce 1 127)
    (273, Token (RPAREN _)) -> Just (Reduce 1 127)
    (273, Token (DARROW _)) -> Just (Reduce 1 127)
    (273, Token (COLON_COLON _)) -> Just (Reduce 1 127)
    (273, Token (QVARID _)) -> Just (Reduce 1 127)
    (273, Token (INTEGER _)) -> Just (Reduce 1 127)
    (273, Token (TODO_FUNLHS _)) -> Just (Reduce 1 127)
    (273, Token (COMMA _)) -> Just (Reduce 1 127)
    (273, Token (ARROW _)) -> Just (Reduce 1 127)
    (273, Token (LBRACKET _)) -> Just (Reduce 1 127)
    (273, Token (RBRACKET _)) -> Just (Reduce 1 127)
    (273, Token (EXCL _)) -> Just (Reduce 1 127)
    (273, Token (PIPE _)) -> Just (Reduce 1 127)
    (273, Token (QVARSYM _)) -> Just (Reduce 1 127)
    (273, Token (QCONSYM _)) -> Just (Reduce 1 127)
    (273, Token (INFIXL _)) -> Just (Reduce 1 127)
    (273, Token (INFIXR _)) -> Just (Reduce 1 127)
    (273, Token (INFIX _)) -> Just (Reduce 1 127)
    (273, Token (BACKQUOTE _)) -> Just (Reduce 1 127)
    (274, Token (QCONID _)) -> Just (Reduce 1 127)
    (274, Token (LBRACE _)) -> Just (Shift 165)
    (274, Token (RBRACE _)) -> Just (Reduce 1 127)
    (274, Token (SEMICOLON _)) -> Just (Reduce 1 127)
    (274, Token (DERIVING _)) -> Just (Reduce 1 127)
    (274, Token (LPAREN _)) -> Just (Reduce 1 127)
    (274, Token (RPAREN _)) -> Just (Reduce 1 127)
    (274, Token (QVARID _)) -> Just (Reduce 1 127)
    (274, Token (COMMA _)) -> Just (Reduce 1 127)
    (274, Token (ARROW _)) -> Just (Reduce 1 127)
    (274, Token (LBRACKET _)) -> Just (Reduce 1 127)
    (274, Token (RBRACKET _)) -> Just (Reduce 1 127)
    (274, Token (EXCL _)) -> Just (Reduce 1 127)
    (274, Token (PIPE _)) -> Just (Reduce 1 127)
    (274, Token (QCONSYM _)) -> Just (Reduce 1 127)
    (274, Token (BACKQUOTE _)) -> Just (Reduce 1 127)
    (275, Token (RPAREN _)) -> Just (Shift 270)
    (276, Token (RPAREN _)) -> Just (Reduce 3 125)
    (276, Token (COMMA _)) -> Just (Shift 99)
    (277, Token (RPAREN _)) -> Just (Reduce 3 126)
    (278, Token (RPAREN _)) -> Just (Reduce 1 132)
    (278, Token (COMMA _)) -> Just (Shift 278)
    (279, Token (RPAREN _)) -> Just (Reduce 2 133)
    (280, Token (RBRACE _)) -> Just (Reduce 3 137)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 137)
    (280, Token (DERIVING _)) -> Just (Reduce 3 137)
    (281, Token (RBRACE _)) -> Just (Reduce 1 136)
    (281, Token (SEMICOLON _)) -> Just (Reduce 1 136)
    (281, Token (DERIVING _)) -> Just (Reduce 1 136)
    (281, Token (PIPE _)) -> Just (Shift 87)
    (282, Token (RBRACE _)) -> Just (Reduce 3 140)
    (282, Token (SEMICOLON _)) -> Just (Reduce 3 140)
    (282, Token (DERIVING _)) -> Just (Reduce 3 140)
    (282, Token (PIPE _)) -> Just (Reduce 3 140)
    (283, Token (RBRACE _)) -> Just (Reduce 4 141)
    (283, Token (SEMICOLON _)) -> Just (Reduce 4 141)
    (283, Token (DERIVING _)) -> Just (Reduce 4 141)
    (283, Token (PIPE _)) -> Just (Reduce 4 141)
    (284, Token (RBRACE _)) -> Just (Shift 283)
    (285, Token (BACKQUOTE _)) -> Just (Shift 289)
    (286, Token (QCONID _)) -> Just (Reduce 1 163)
    (286, Token (RBRACE _)) -> Just (Reduce 1 163)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 163)
    (286, Token (LPAREN _)) -> Just (Reduce 1 163)
    (286, Token (RPAREN _)) -> Just (Reduce 1 163)
    (286, Token (QVARID _)) -> Just (Reduce 1 163)
    (286, Token (INTEGER _)) -> Just (Reduce 1 163)
    (286, Token (COMMA _)) -> Just (Reduce 1 163)
    (286, Token (ARROW _)) -> Just (Reduce 1 163)
    (286, Token (LBRACKET _)) -> Just (Reduce 1 163)
    (286, Token (RBRACKET _)) -> Just (Reduce 1 163)
    (286, Token (EXCL _)) -> Just (Reduce 1 163)
    (286, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (286, Token (QCONSYM _)) -> Just (Reduce 1 163)
    (287, Token (QCONID _)) -> Just (Shift 285)
    (288, Token (QCONID _)) -> Just (Shift 285)
    (288, Token (QVARID _)) -> Just (Shift 294)
    (289, Token (QCONID _)) -> Just (Reduce 3 164)
    (289, Token (RBRACE _)) -> Just (Reduce 3 164)
    (289, Token (SEMICOLON _)) -> Just (Reduce 3 164)
    (289, Token (LPAREN _)) -> Just (Reduce 3 164)
    (289, Token (RPAREN _)) -> Just (Reduce 3 164)
    (289, Token (QVARID _)) -> Just (Reduce 3 164)
    (289, Token (INTEGER _)) -> Just (Reduce 3 164)
    (289, Token (COMMA _)) -> Just (Reduce 3 164)
    (289, Token (ARROW _)) -> Just (Reduce 3 164)
    (289, Token (LBRACKET _)) -> Just (Reduce 3 164)
    (289, Token (RBRACKET _)) -> Just (Reduce 3 164)
    (289, Token (EXCL _)) -> Just (Reduce 3 164)
    (289, Token (QVARSYM _)) -> Just (Reduce 3 164)
    (289, Token (QCONSYM _)) -> Just (Reduce 3 164)
    (290, Token (RBRACE _)) -> Just (Reduce 3 143)
    (291, Token (RBRACE _)) -> Just (Reduce 1 142)
    (291, Token (COMMA _)) -> Just (Shift 166)
    (292, Token (RBRACE _)) -> Just (Reduce 3 144)
    (292, Token (COMMA _)) -> Just (Reduce 3 144)
    (293, Token (COLON_COLON _)) -> Just (Shift 95)
    (294, Token (BACKQUOTE _)) -> Just (Shift 296)
    (295, Token (RBRACE _)) -> Just (Reduce 1 161)
    (295, Token (SEMICOLON _)) -> Just (Reduce 1 161)
    (295, Token (LPAREN _)) -> Just (Reduce 1 161)
    (295, Token (QVARID _)) -> Just (Reduce 1 161)
    (295, Token (INTEGER _)) -> Just (Reduce 1 161)
    (295, Token (COMMA _)) -> Just (Reduce 1 161)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (296, Token (RBRACE _)) -> Just (Reduce 3 162)
    (296, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (296, Token (LPAREN _)) -> Just (Reduce 3 162)
    (296, Token (QVARID _)) -> Just (Reduce 3 162)
    (296, Token (INTEGER _)) -> Just (Reduce 3 162)
    (296, Token (COMMA _)) -> Just (Reduce 3 162)
    (296, Token (QVARSYM _)) -> Just (Reduce 3 162)
    (_, _) -> Nothing

production :: Int -> Int
production 0 = 0
production 1 = 0
production 2 = 1
production 3 = 3
production 4 = 4
production 5 = 4
production 6 = 5
production 7 = 5
production 8 = 5
production 9 = 5
production 10 = 5
production 11 = 5
production 12 = 5
production 13 = 5
production 14 = 5
production 15 = 5
production 16 = 5
production 17 = 5
production 18 = 5
production 19 = 5
production 20 = 5
production 21 = 5
production 22 = 5
production 23 = 5
production 24 = 5
production 25 = 5
production 26 = 5
production 27 = 5
production 28 = 5
production 29 = 5
production 30 = 5
production 31 = 5
production 32 = 18
production 33 = 18
production 34 = 18
production 35 = 20
production 36 = 23
production 37 = 21
production 38 = 24
production 39 = 25
production 40 = 26
production 41 = 26
production 42 = 19
production 43 = 19
production 44 = 19
production 45 = 15
production 46 = 15
production 47 = 30
production 48 = 31
production 49 = 31
production 50 = 32
production 51 = 32
production 52 = 32
production 53 = 16
production 54 = 16
production 55 = 33
production 56 = 34
production 57 = 34
production 58 = 35
production 59 = 35
production 60 = 35
production 61 = 27
production 62 = 27
production 63 = 27
production 64 = 27
production 65 = 38
production 66 = 38
production 67 = 28
production 68 = 29
production 69 = 29
production 70 = 40
production 71 = 41
production 72 = 42
production 73 = 43
production 74 = 43
production 75 = 43
production 76 = 44
production 77 = 44
production 78 = 46
production 79 = 47
production 80 = 48
production 81 = 6
production 82 = 6
production 83 = 7
production 84 = 7
production 85 = 8
production 86 = 8
production 87 = 49
production 88 = 49
production 89 = 50
production 90 = 50
production 91 = 50
production 92 = 51
production 93 = 51
production 94 = 51
production 95 = 51
production 96 = 51
production 97 = 2
production 98 = 2
production 99 = 54
production 100 = 55
production 101 = 55
production 102 = 55
production 103 = 56
production 104 = 56
production 105 = 56
production 106 = 56
production 107 = 56
production 108 = 56
production 109 = 53
production 110 = 53
production 111 = 57
production 112 = 57
production 113 = 17
production 114 = 17
production 115 = 10
production 116 = 10
production 117 = 9
production 118 = 9
production 119 = 58
production 120 = 58
production 121 = 58
production 122 = 58
production 123 = 58
production 124 = 58
production 125 = 60
production 126 = 60
production 127 = 59
production 128 = 59
production 129 = 59
production 130 = 59
production 131 = 59
production 132 = 61
production 133 = 61
production 134 = 11
production 135 = 11
production 136 = 62
production 137 = 62
production 138 = 63
production 139 = 63
production 140 = 63
production 141 = 63
production 142 = 65
production 143 = 65
production 144 = 66
production 145 = 67
production 146 = 67
production 147 = 67
production 148 = 14
production 149 = 14
production 150 = 39
production 151 = 39
production 152 = 36
production 153 = 36
production 154 = 22
production 155 = 22
production 156 = 52
production 157 = 52
production 158 = 37
production 159 = 37
production 160 = 37
production 161 = 68
production 162 = 68
production 163 = 64
production 164 = 64
production 165 = 45
production 166 = 45
production 167 = 12
production 168 = 13
production 169 = 13

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 54) -> Just 12
    (8, 1) -> Just 51
    (9, 1) -> Just 84
    (10, 1) -> Just 253
    (13, 4) -> Just 15
    (13, 5) -> Just 18
    (13, 19) -> Just 82
    (13, 22) -> Just 142
    (13, 27) -> Just 143
    (13, 28) -> Just 144
    (13, 36) -> Just 181
    (13, 37) -> Just 182
    (16, 4) -> Just 17
    (16, 5) -> Just 18
    (16, 19) -> Just 82
    (16, 22) -> Just 142
    (16, 27) -> Just 143
    (16, 28) -> Just 144
    (16, 36) -> Just 181
    (16, 37) -> Just 182
    (19, 6) -> Just 8
    (20, 9) -> Just 54
    (20, 48) -> Just 264
    (20, 52) -> Just 273
    (20, 58) -> Just 105
    (20, 59) -> Just 266
    (21, 9) -> Just 104
    (21, 10) -> Just 63
    (21, 48) -> Just 264
    (21, 52) -> Just 273
    (21, 58) -> Just 105
    (21, 59) -> Just 266
    (22, 9) -> Just 55
    (22, 48) -> Just 264
    (22, 52) -> Just 273
    (22, 58) -> Just 105
    (22, 59) -> Just 266
    (23, 12) -> Just 66
    (24, 12) -> Just 67
    (25, 12) -> Just 68
    (26, 12) -> Just 69
    (27, 12) -> Just 111
    (27, 13) -> Just 70
    (28, 12) -> Just 111
    (28, 13) -> Just 71
    (29, 12) -> Just 111
    (29, 13) -> Just 72
    (30, 12) -> Just 111
    (30, 13) -> Just 73
    (31, 9) -> Just 104
    (31, 10) -> Just 124
    (31, 17) -> Just 80
    (31, 48) -> Just 264
    (31, 52) -> Just 273
    (31, 58) -> Just 105
    (31, 59) -> Just 266
    (42, 9) -> Just 56
    (42, 48) -> Just 264
    (42, 52) -> Just 273
    (42, 58) -> Just 105
    (42, 59) -> Just 266
    (43, 9) -> Just 57
    (43, 48) -> Just 264
    (43, 52) -> Just 273
    (43, 58) -> Just 105
    (43, 59) -> Just 266
    (44, 9) -> Just 58
    (44, 48) -> Just 264
    (44, 52) -> Just 273
    (44, 58) -> Just 105
    (44, 59) -> Just 266
    (45, 9) -> Just 59
    (45, 48) -> Just 264
    (45, 52) -> Just 273
    (45, 58) -> Just 105
    (45, 59) -> Just 266
    (46, 9) -> Just 60
    (46, 48) -> Just 264
    (46, 52) -> Just 273
    (46, 58) -> Just 105
    (46, 59) -> Just 266
    (47, 9) -> Just 61
    (47, 48) -> Just 264
    (47, 52) -> Just 273
    (47, 58) -> Just 105
    (47, 59) -> Just 266
    (48, 9) -> Just 62
    (48, 48) -> Just 264
    (48, 52) -> Just 273
    (48, 58) -> Just 105
    (48, 59) -> Just 266
    (50, 18) -> Just 81
    (51, 7) -> Just 52
    (52, 8) -> Just 53
    (52, 49) -> Just 85
    (54, 48) -> Just 264
    (54, 52) -> Just 273
    (54, 58) -> Just 106
    (54, 59) -> Just 266
    (55, 11) -> Just 64
    (55, 48) -> Just 264
    (55, 52) -> Just 273
    (55, 58) -> Just 106
    (55, 59) -> Just 266
    (56, 11) -> Just 65
    (56, 48) -> Just 264
    (56, 52) -> Just 273
    (56, 58) -> Just 106
    (56, 59) -> Just 266
    (57, 14) -> Just 74
    (57, 48) -> Just 264
    (57, 52) -> Just 273
    (57, 58) -> Just 106
    (57, 59) -> Just 266
    (58, 15) -> Just 76
    (58, 48) -> Just 264
    (58, 52) -> Just 273
    (58, 58) -> Just 106
    (58, 59) -> Just 266
    (59, 16) -> Just 78
    (59, 48) -> Just 264
    (59, 52) -> Just 273
    (59, 58) -> Just 106
    (59, 59) -> Just 266
    (60, 14) -> Just 75
    (60, 48) -> Just 264
    (60, 52) -> Just 273
    (60, 58) -> Just 106
    (60, 59) -> Just 266
    (61, 15) -> Just 77
    (61, 48) -> Just 264
    (61, 52) -> Just 273
    (61, 58) -> Just 106
    (61, 59) -> Just 266
    (62, 16) -> Just 79
    (62, 48) -> Just 264
    (62, 52) -> Just 273
    (62, 58) -> Just 106
    (62, 59) -> Just 266
    (86, 9) -> Just 101
    (86, 48) -> Just 264
    (86, 52) -> Just 274
    (86, 58) -> Just 105
    (86, 59) -> Just 266
    (86, 62) -> Just 108
    (86, 63) -> Just 281
    (87, 9) -> Just 101
    (87, 48) -> Just 264
    (87, 52) -> Just 274
    (87, 58) -> Just 105
    (87, 59) -> Just 266
    (87, 62) -> Just 280
    (87, 63) -> Just 281
    (88, 9) -> Just 102
    (88, 48) -> Just 264
    (88, 52) -> Just 273
    (88, 58) -> Just 105
    (88, 59) -> Just 266
    (89, 9) -> Just 103
    (89, 10) -> Just 180
    (89, 48) -> Just 264
    (89, 52) -> Just 273
    (89, 58) -> Just 105
    (89, 59) -> Just 266
    (90, 9) -> Just 104
    (90, 10) -> Just 107
    (90, 48) -> Just 264
    (90, 52) -> Just 273
    (90, 58) -> Just 105
    (90, 59) -> Just 266
    (91, 9) -> Just 104
    (91, 10) -> Just 128
    (91, 48) -> Just 264
    (91, 52) -> Just 273
    (91, 58) -> Just 105
    (91, 59) -> Just 266
    (92, 9) -> Just 104
    (92, 10) -> Just 129
    (92, 48) -> Just 264
    (92, 52) -> Just 273
    (92, 58) -> Just 105
    (92, 59) -> Just 266
    (93, 9) -> Just 104
    (93, 10) -> Just 130
    (93, 48) -> Just 264
    (93, 52) -> Just 273
    (93, 58) -> Just 105
    (93, 59) -> Just 266
    (94, 9) -> Just 104
    (94, 10) -> Just 179
    (94, 48) -> Just 264
    (94, 52) -> Just 273
    (94, 58) -> Just 105
    (94, 59) -> Just 266
    (95, 9) -> Just 104
    (95, 10) -> Just 292
    (95, 48) -> Just 264
    (95, 52) -> Just 273
    (95, 58) -> Just 105
    (95, 59) -> Just 266
    (96, 9) -> Just 104
    (96, 10) -> Just 116
    (96, 48) -> Just 264
    (96, 52) -> Just 273
    (96, 58) -> Just 105
    (96, 59) -> Just 266
    (97, 9) -> Just 104
    (97, 10) -> Just 124
    (97, 17) -> Just 125
    (97, 48) -> Just 264
    (97, 52) -> Just 273
    (97, 58) -> Just 105
    (97, 59) -> Just 266
    (98, 9) -> Just 104
    (98, 10) -> Just 262
    (98, 48) -> Just 264
    (98, 52) -> Just 273
    (98, 58) -> Just 105
    (98, 59) -> Just 266
    (98, 60) -> Just 267
    (98, 61) -> Just 275
    (99, 9) -> Just 104
    (99, 10) -> Just 276
    (99, 48) -> Just 264
    (99, 52) -> Just 273
    (99, 58) -> Just 105
    (99, 59) -> Just 266
    (99, 60) -> Just 277
    (100, 9) -> Just 104
    (100, 10) -> Just 263
    (100, 48) -> Just 264
    (100, 52) -> Just 273
    (100, 58) -> Just 105
    (100, 59) -> Just 266
    (101, 48) -> Just 264
    (101, 52) -> Just 273
    (101, 58) -> Just 106
    (101, 59) -> Just 266
    (101, 64) -> Just 88
    (102, 48) -> Just 264
    (102, 52) -> Just 273
    (102, 58) -> Just 106
    (102, 59) -> Just 266
    (103, 48) -> Just 264
    (103, 52) -> Just 273
    (103, 58) -> Just 106
    (103, 59) -> Just 266
    (104, 48) -> Just 264
    (104, 52) -> Just 273
    (104, 58) -> Just 106
    (104, 59) -> Just 266
    (109, 12) -> Just 111
    (109, 13) -> Just 112
    (113, 22) -> Just 117
    (115, 52) -> Just 118
    (118, 48) -> Just 264
    (118, 52) -> Just 273
    (118, 58) -> Just 119
    (118, 59) -> Just 266
    (120, 30) -> Just 121
    (122, 33) -> Just 123
    (126, 20) -> Just 131
    (127, 20) -> Just 132
    (131, 21) -> Just 133
    (131, 23) -> Just 138
    (132, 24) -> Just 139
    (133, 22) -> Just 135
    (134, 22) -> Just 136
    (138, 21) -> Just 134
    (139, 22) -> Just 137
    (140, 19) -> Just 177
    (140, 22) -> Just 142
    (140, 26) -> Just 176
    (140, 27) -> Just 143
    (140, 28) -> Just 144
    (140, 36) -> Just 181
    (140, 37) -> Just 182
    (141, 19) -> Just 177
    (141, 22) -> Just 142
    (141, 26) -> Just 178
    (141, 27) -> Just 143
    (141, 28) -> Just 144
    (141, 36) -> Just 181
    (141, 37) -> Just 182
    (142, 29) -> Just 145
    (144, 29) -> Just 146
    (149, 22) -> Just 220
    (149, 40) -> Just 191
    (149, 41) -> Just 214
    (149, 42) -> Just 215
    (149, 43) -> Just 150
    (149, 44) -> Just 216
    (150, 22) -> Just 220
    (150, 44) -> Just 217
    (150, 45) -> Just 151
    (150, 64) -> Just 221
    (150, 68) -> Just 222
    (151, 22) -> Just 220
    (151, 44) -> Just 218
    (152, 22) -> Just 186
    (152, 27) -> Just 196
    (152, 28) -> Just 187
    (152, 31) -> Just 193
    (152, 32) -> Just 195
    (152, 36) -> Just 181
    (152, 37) -> Just 182
    (153, 22) -> Just 186
    (153, 27) -> Just 196
    (153, 28) -> Just 187
    (153, 31) -> Just 194
    (153, 32) -> Just 195
    (153, 36) -> Just 181
    (153, 37) -> Just 182
    (154, 22) -> Just 188
    (154, 28) -> Just 189
    (154, 34) -> Just 200
    (154, 35) -> Just 202
    (155, 22) -> Just 188
    (155, 28) -> Just 189
    (155, 34) -> Just 201
    (155, 35) -> Just 202
    (156, 22) -> Just 236
    (156, 50) -> Just 228
    (156, 51) -> Just 231
    (156, 52) -> Just 237
    (157, 22) -> Just 236
    (157, 50) -> Just 229
    (157, 51) -> Just 231
    (157, 52) -> Just 237
    (158, 22) -> Just 236
    (158, 50) -> Just 230
    (158, 51) -> Just 231
    (158, 52) -> Just 237
    (159, 22) -> Just 257
    (159, 52) -> Just 258
    (159, 53) -> Just 238
    (159, 57) -> Just 244
    (160, 22) -> Just 254
    (160, 52) -> Just 255
    (160, 55) -> Just 246
    (160, 56) -> Just 248
    (161, 22) -> Just 254
    (161, 52) -> Just 255
    (161, 55) -> Just 247
    (161, 56) -> Just 248
    (162, 22) -> Just 257
    (162, 52) -> Just 258
    (162, 53) -> Just 243
    (162, 57) -> Just 244
    (163, 22) -> Just 257
    (163, 52) -> Just 258
    (163, 53) -> Just 256
    (163, 57) -> Just 244
    (164, 22) -> Just 205
    (164, 36) -> Just 206
    (165, 22) -> Just 205
    (165, 36) -> Just 293
    (165, 65) -> Just 284
    (165, 66) -> Just 291
    (166, 22) -> Just 205
    (166, 36) -> Just 293
    (166, 65) -> Just 290
    (166, 66) -> Just 291
    (174, 25) -> Just 190
    (182, 38) -> Just 183
    (183, 39) -> Just 184
    (183, 45) -> Just 213
    (183, 64) -> Just 221
    (183, 68) -> Just 222
    (186, 29) -> Just 197
    (187, 29) -> Just 198
    (188, 29) -> Just 203
    (189, 29) -> Just 204
    (211, 39) -> Just 212
    (211, 45) -> Just 213
    (211, 64) -> Just 221
    (211, 68) -> Just 222
    (223, 48) -> Just 264
    (223, 52) -> Just 273
    (223, 58) -> Just 265
    (223, 59) -> Just 266
    (278, 61) -> Just 279
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
                  Token (QCONID semanticValue) ->
                    StackValue_QCONID semanticValue
                  Token (LBRACE semanticValue) ->
                    StackValue_LBRACE semanticValue
                  Token (RBRACE semanticValue) ->
                    StackValue_RBRACE semanticValue
                  Token (SEMICOLON semanticValue) ->
                    StackValue_SEMICOLON semanticValue
                  Token (IMPORT semanticValue) ->
                    StackValue_IMPORT semanticValue
                  Token (TYPE semanticValue) ->
                    StackValue_TYPE semanticValue
                  Token (EQUAL semanticValue) ->
                    StackValue_EQUAL semanticValue
                  Token (DATA semanticValue) ->
                    StackValue_DATA semanticValue
                  Token (DERIVING semanticValue) ->
                    StackValue_DERIVING semanticValue
                  Token (LPAREN semanticValue) ->
                    StackValue_LPAREN semanticValue
                  Token (RPAREN semanticValue) ->
                    StackValue_RPAREN semanticValue
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
                  Token (QVARID semanticValue) ->
                    StackValue_QVARID semanticValue
                  Token (STRING semanticValue) ->
                    StackValue_STRING semanticValue
                  Token (INTEGER semanticValue) ->
                    StackValue_INTEGER semanticValue
                  Token (TODO_FUNLHS semanticValue) ->
                    StackValue_TODO_FUNLHS semanticValue
                  Token (TODO_INST semanticValue) ->
                    StackValue_TODO_INST semanticValue
                  Token (QUALIFIED semanticValue) ->
                    StackValue_QUALIFIED semanticValue
                  Token (AS semanticValue) ->
                    StackValue_AS semanticValue
                  Token (HIDING semanticValue) ->
                    StackValue_HIDING semanticValue
                  Token (COMMA semanticValue) ->
                    StackValue_COMMA semanticValue
                  Token (DOT_DOT semanticValue) ->
                    StackValue_DOT_DOT semanticValue
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
                  Token (QVARSYM semanticValue) ->
                    StackValue_QVARSYM semanticValue
                  Token (QCONSYM semanticValue) ->
                    StackValue_QCONSYM semanticValue
                  Token (INFIXL semanticValue) ->
                    StackValue_INFIXL semanticValue
                  Token (INFIXR semanticValue) ->
                    StackValue_INFIXR semanticValue
                  Token (INFIX semanticValue) ->
                    StackValue_INFIX semanticValue
                  Token (BACKQUOTE semanticValue) ->
                    StackValue_BACKQUOTE semanticValue
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
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    3 ->
                      Monad.liftM StackValue_body $ body_implies_LBRACE_topdecls_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_topdecls value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    4 ->
                      Monad.liftM StackValue_topdecls $ topdecls_implies_topdecl actions (case snd (pop !! 0) of { StackValue_topdecl value -> value; _ -> undefined })
                    5 ->
                      Monad.liftM StackValue_topdecls $ topdecls_implies_topdecl_SEMICOLON_topdecls actions (case snd (pop !! 2) of { StackValue_topdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_topdecls value -> value; _ -> undefined })
                    6 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_IMPORT_qualified_opt_modid_as_opt_impspec_opt actions (case snd (pop !! 4) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_qualified_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_modid value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_as_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_impspec_opt value -> value; _ -> undefined })
                    7 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_TYPE_btype_EQUAL_type' actions (case snd (pop !! 3) of { StackValue_TYPE value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    8 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt actions (case snd (pop !! 2) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs_opt value -> value; _ -> undefined })
                    9 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_dclass actions (case snd (pop !! 4) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    10 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 5) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    11 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 6) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    12 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt actions (case snd (pop !! 4) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs_opt value -> value; _ -> undefined })
                    13 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_dclass actions (case snd (pop !! 6) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    14 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 7) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    15 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DATA_btype_DARROW_btype_constrs_opt_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 8) of { StackValue_DATA value -> value; _ -> undefined }) (case snd (pop !! 7) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_constrs_opt value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    16 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr actions (case snd (pop !! 2) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_newconstr value -> value; _ -> undefined })
                    17 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_dclass actions (case snd (pop !! 4) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    18 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 5) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    19 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 6) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    20 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr actions (case snd (pop !! 4) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_newconstr value -> value; _ -> undefined })
                    21 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_dclass actions (case snd (pop !! 6) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    22 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_RPAREN actions (case snd (pop !! 7) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    23 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_NEWTYPE_btype_DARROW_btype_newconstr_DERIVING_LPAREN_dclass_seq_RPAREN actions (case snd (pop !! 8) of { StackValue_NEWTYPE value -> value; _ -> undefined }) (case snd (pop !! 7) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 6) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_newconstr value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_DERIVING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_dclass_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    24 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_CLASS_btype_cdecls_opt actions (case snd (pop !! 2) of { StackValue_CLASS value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls_opt value -> value; _ -> undefined })
                    25 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_CLASS_btype_DARROW_btype_cdecls_opt actions (case snd (pop !! 4) of { StackValue_CLASS value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls_opt value -> value; _ -> undefined })
                    26 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_INSTANCE_btype_idecls_opt actions (case snd (pop !! 2) of { StackValue_INSTANCE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls_opt value -> value; _ -> undefined })
                    27 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_INSTANCE_btype_DARROW_btype_idecls_opt actions (case snd (pop !! 4) of { StackValue_INSTANCE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls_opt value -> value; _ -> undefined })
                    28 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DEFAULT_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_DEFAULT value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    29 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_DEFAULT_LPAREN_type_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_DEFAULT value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    30 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_FOREIGN_fdecl actions (case snd (pop !! 1) of { StackValue_FOREIGN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fdecl value -> value; _ -> undefined })
                    31 ->
                      Monad.liftM StackValue_topdecl $ topdecl_implies_decl actions (case snd (pop !! 0) of { StackValue_decl value -> value; _ -> undefined })
                    32 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    33 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_safety value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    34 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_QVARID_callconv_expent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_expent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    35 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    36 ->
                      Monad.liftM StackValue_safety $ safety_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    37 ->
                      Monad.liftM StackValue_impent $ impent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    38 ->
                      Monad.liftM StackValue_expent $ expent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    39 ->
                      Monad.liftM StackValue_decls $ decls_implies_LBRACE_decl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_decl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    40 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl actions (case snd (pop !! 0) of { StackValue_decl value -> value; _ -> undefined })
                    41 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl_SEMICOLON_decl_seq actions (case snd (pop !! 2) of { StackValue_decl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decl_seq value -> value; _ -> undefined })
                    42 ->
                      Monad.liftM StackValue_decl $ decl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    43 ->
                      Monad.liftM StackValue_decl $ decl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    44 ->
                      Monad.liftM StackValue_decl $ decl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    45 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies actions
                    46 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies_WHERE_cdecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls value -> value; _ -> undefined })
                    47 ->
                      Monad.liftM StackValue_cdecls $ cdecls_implies_LBRACE_cdecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cdecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    48 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl actions (case snd (pop !! 0) of { StackValue_cdecl value -> value; _ -> undefined })
                    49 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq actions (case snd (pop !! 2) of { StackValue_cdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecl_seq value -> value; _ -> undefined })
                    50 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    51 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    52 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    53 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies actions
                    54 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies_WHERE_idecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls value -> value; _ -> undefined })
                    55 ->
                      Monad.liftM StackValue_idecls $ idecls_implies_LBRACE_idecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_idecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    56 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl actions (case snd (pop !! 0) of { StackValue_idecl value -> value; _ -> undefined })
                    57 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl_SEMICOLON_idecl_seq actions (case snd (pop !! 2) of { StackValue_idecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecl_seq value -> value; _ -> undefined })
                    58 ->
                      Monad.liftM StackValue_idecl $ idecl_implies actions
                    59 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    60 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    61 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies actions
                    62 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    63 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    64 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_fixity_integer_opt_ops actions (case snd (pop !! 2) of { StackValue_fixity value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_integer_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    65 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    66 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    67 ->
                      Monad.liftM StackValue_funlhs $ funlhs_implies_TODO_FUNLHS actions (case snd (pop !! 0) of { StackValue_TODO_FUNLHS value -> value; _ -> undefined })
                    68 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    69 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp_WHERE_decls actions (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    70 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    71 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    72 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    73 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    74 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    75 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    76 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    77 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    78 ->
                      Monad.liftM StackValue_inst $ inst_implies_TODO_INST actions (case snd (pop !! 0) of { StackValue_TODO_INST value -> value; _ -> undefined })
                    79 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    80 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    81 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    82 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    83 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    84 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    85 ->
                      Monad.liftM StackValue_impspec_opt $ impspec_opt_implies actions
                    86 ->
                      Monad.liftM StackValue_impspec_opt $ impspec_opt_implies_impspec actions (case snd (pop !! 0) of { StackValue_impspec value -> value; _ -> undefined })
                    87 ->
                      Monad.liftM StackValue_impspec $ impspec_implies_LPAREN_import_seq_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    88 ->
                      Monad.liftM StackValue_impspec $ impspec_implies_HIDING_LPAREN_import_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_HIDING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    89 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies actions
                    90 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import' actions (case snd (pop !! 0) of { StackValue_import' value -> value; _ -> undefined })
                    91 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import'_COMMA_import_seq actions (case snd (pop !! 2) of { StackValue_import' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_import_seq value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_import' $ import'_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_import' $ import'_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    94 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies actions
                    98 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies_exports actions (case snd (pop !! 0) of { StackValue_exports value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_exports $ exports_implies_LPAREN_export_seq_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_export_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies actions
                    101 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export actions (case snd (pop !! 0) of { StackValue_export value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export_COMMA_export_seq actions (case snd (pop !! 2) of { StackValue_export value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_export_seq value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_export $ export_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_export $ export_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_export $ export_implies_MODULE_modid actions (case snd (pop !! 1) of { StackValue_MODULE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    109 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname actions (case snd (pop !! 0) of { StackValue_cname value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname_COMMA_cname_seq actions (case snd (pop !! 2) of { StackValue_cname value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cname_seq value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_cname $ cname_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    112 ->
                      Monad.liftM StackValue_cname $ cname_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_ARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    118 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    120 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    128 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_ARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    135 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    139 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_atypes $ atypes_implies actions
                    146 ->
                      Monad.liftM StackValue_atypes $ atypes_implies_atype_atypes actions (case snd (pop !! 1) of { StackValue_atype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atypes value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_atypes $ atypes_implies_EXCL_atype_atypes actions (case snd (pop !! 2) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_atype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atypes value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass_COMMA_dclass_seq actions (case snd (pop !! 2) of { StackValue_dclass value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass_seq value -> value; _ -> undefined })
                parse' ((q, value) : stack') tokens
        Just Accept ->
          case stack of { [(_, StackValue_module' value)] -> return $ Right (value, tokens); _ -> case tokens of { [] -> return $ Left $ Nothing; (token : _) -> return $ Left $ Just token }}


semanticActions :: Monad m => SemanticActions m
semanticActions = SemanticActions
  { module'_implies_MODULE_modid_exports_opt_WHERE_body = \mODULE0 modid1 exports_opt2 wHERE3 body4 ->
      return $ Module'_implies_MODULE_modid_exports_opt_WHERE_body mODULE0 modid1 exports_opt2 wHERE3 body4
  , module'_implies_body = \body0 ->
      return $ Module'_implies_body body0
  , modid_implies_QCONID = \qCONID0 ->
      return $ Modid_implies_QCONID qCONID0
  , body_implies_LBRACE_topdecls_RBRACE = \lBRACE0 topdecls1 rBRACE2 ->
      return $ Body_implies_LBRACE_topdecls_RBRACE lBRACE0 topdecls1 rBRACE2
  , topdecls_implies_topdecl = \topdecl0 ->
      return $ Topdecls_implies_topdecl topdecl0
  , topdecls_implies_topdecl_SEMICOLON_topdecls = \topdecl0 sEMICOLON1 topdecls2 ->
      return $ Topdecls_implies_topdecl_SEMICOLON_topdecls topdecl0 sEMICOLON1 topdecls2
  , topdecl_implies_IMPORT_qualified_opt_modid_as_opt_impspec_opt = \iMPORT0 qualified_opt1 modid2 as_opt3 impspec_opt4 ->
      return $ Topdecl_implies_IMPORT_qualified_opt_modid_as_opt_impspec_opt iMPORT0 qualified_opt1 modid2 as_opt3 impspec_opt4
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
  , fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' = \iMPORT0 callconv1 impent2 var3 cOLON_COLON4 type'5 ->
      return $ Fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' iMPORT0 callconv1 impent2 var3 cOLON_COLON4 type'5
  , fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' = \iMPORT0 callconv1 safety2 impent3 var4 cOLON_COLON5 type'6 ->
      return $ Fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' iMPORT0 callconv1 safety2 impent3 var4 cOLON_COLON5 type'6
  , fdecl_implies_QVARID_callconv_expent_var_COLON_COLON_type' = \qVARID0 callconv1 expent2 var3 cOLON_COLON4 type'5 ->
      return $ Fdecl_implies_QVARID_callconv_expent_var_COLON_COLON_type' qVARID0 callconv1 expent2 var3 cOLON_COLON4 type'5
  , callconv_implies_QVARID = \qVARID0 ->
      return $ Callconv_implies_QVARID qVARID0
  , safety_implies_QVARID = \qVARID0 ->
      return $ Safety_implies_QVARID qVARID0
  , impent_implies_STRING = \sTRING0 ->
      return $ Impent_implies_STRING sTRING0
  , expent_implies_STRING = \sTRING0 ->
      return $ Expent_implies_STRING sTRING0
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
  , integer_opt_implies =
      return $ Integer_opt_implies
  , integer_opt_implies_INTEGER = \iNTEGER0 ->
      return $ Integer_opt_implies_INTEGER iNTEGER0
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
  , inst_implies_TODO_INST = \tODO_INST0 ->
      return $ Inst_implies_TODO_INST tODO_INST0
  , tycls_implies_QCONID = \qCONID0 ->
      return $ Tycls_implies_QCONID qCONID0
  , tyvar_implies_QVARID = \qVARID0 ->
      return $ Tyvar_implies_QVARID qVARID0
  , qualified_opt_implies =
      return $ Qualified_opt_implies
  , qualified_opt_implies_QUALIFIED = \qUALIFIED0 ->
      return $ Qualified_opt_implies_QUALIFIED qUALIFIED0
  , as_opt_implies =
      return $ As_opt_implies
  , as_opt_implies_AS_modid = \aS0 modid1 ->
      return $ As_opt_implies_AS_modid aS0 modid1
  , impspec_opt_implies =
      return $ Impspec_opt_implies
  , impspec_opt_implies_impspec = \impspec0 ->
      return $ Impspec_opt_implies_impspec impspec0
  , impspec_implies_LPAREN_import_seq_RPAREN = \lPAREN0 import_seq1 rPAREN2 ->
      return $ Impspec_implies_LPAREN_import_seq_RPAREN lPAREN0 import_seq1 rPAREN2
  , impspec_implies_HIDING_LPAREN_import_seq_RPAREN = \hIDING0 lPAREN1 import_seq2 rPAREN3 ->
      return $ Impspec_implies_HIDING_LPAREN_import_seq_RPAREN hIDING0 lPAREN1 import_seq2 rPAREN3
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
  , cname_seq_implies_cname = \cname0 ->
      return $ Cname_seq_implies_cname cname0
  , cname_seq_implies_cname_COMMA_cname_seq = \cname0 cOMMA1 cname_seq2 ->
      return $ Cname_seq_implies_cname_COMMA_cname_seq cname0 cOMMA1 cname_seq2
  , cname_implies_var = \var0 ->
      return $ Cname_implies_var var0
  , cname_implies_con = \con0 ->
      return $ Cname_implies_con con0
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
  , fielddecl_seq_implies_fielddecl = \fielddecl0 ->
      return $ Fielddecl_seq_implies_fielddecl fielddecl0
  , fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq = \fielddecl0 cOMMA1 fielddecl_seq2 ->
      return $ Fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq fielddecl0 cOMMA1 fielddecl_seq2
  , fielddecl_implies_vars_COLON_COLON_type' = \vars0 cOLON_COLON1 type'2 ->
      return $ Fielddecl_implies_vars_COLON_COLON_type' vars0 cOLON_COLON1 type'2
  , atypes_implies =
      return $ Atypes_implies
  , atypes_implies_atype_atypes = \atype0 atypes1 ->
      return $ Atypes_implies_atype_atypes atype0 atypes1
  , atypes_implies_EXCL_atype_atypes = \eXCL0 atype1 atypes2 ->
      return $ Atypes_implies_EXCL_atype_atypes eXCL0 atype1 atypes2
  , newconstr_implies_EQUAL_con_atype = \eQUAL0 con1 atype2 ->
      return $ Newconstr_implies_EQUAL_con_atype eQUAL0 con1 atype2
  , newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE = \eQUAL0 con1 lBRACE2 var3 cOLON_COLON4 type'5 rBRACE6 ->
      return $ Newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE eQUAL0 con1 lBRACE2 var3 cOLON_COLON4 type'5 rBRACE6
  , ops_implies_op = \op0 ->
      return $ Ops_implies_op op0
  , ops_implies_op_COMMA_ops = \op0 cOMMA1 ops2 ->
      return $ Ops_implies_op_COMMA_ops op0 cOMMA1 ops2
  , vars_implies_var = \var0 ->
      return $ Vars_implies_var var0
  , vars_implies_var_COMMA_vars = \var0 cOMMA1 vars2 ->
      return $ Vars_implies_var_COMMA_vars var0 cOMMA1 vars2
  , var_implies_QVARID = \qVARID0 ->
      return $ Var_implies_QVARID qVARID0
  , var_implies_LPAREN_QVARSYM_RPAREN = \lPAREN0 qVARSYM1 rPAREN2 ->
      return $ Var_implies_LPAREN_QVARSYM_RPAREN lPAREN0 qVARSYM1 rPAREN2
  , con_implies_QCONID = \qCONID0 ->
      return $ Con_implies_QCONID qCONID0
  , con_implies_LPAREN_QCONSYM_RPAREN = \lPAREN0 qCONSYM1 rPAREN2 ->
      return $ Con_implies_LPAREN_QCONSYM_RPAREN lPAREN0 qCONSYM1 rPAREN2
  , fixity_implies_INFIXL = \iNFIXL0 ->
      return $ Fixity_implies_INFIXL iNFIXL0
  , fixity_implies_INFIXR = \iNFIXR0 ->
      return $ Fixity_implies_INFIXR iNFIXR0
  , fixity_implies_INFIX = \iNFIX0 ->
      return $ Fixity_implies_INFIX iNFIX0
  , varop_implies_QVARSYM = \qVARSYM0 ->
      return $ Varop_implies_QVARSYM qVARSYM0
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
  , dclass_implies_QCONID = \qCONID0 ->
      return $ Dclass_implies_QCONID qCONID0
  , dclass_seq_implies_dclass = \dclass0 ->
      return $ Dclass_seq_implies_dclass dclass0
  , dclass_seq_implies_dclass_COMMA_dclass_seq = \dclass0 cOMMA1 dclass_seq2 ->
      return $ Dclass_seq_implies_dclass_COMMA_dclass_seq dclass0 cOMMA1 dclass_seq2 }

