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
  | TODO_FDECL TODO_FDECL
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
    Fdecl_implies_TODO_FDECL TODO_FDECL
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

data Var =
    Var_implies_QVARID QVARID
  | Var_implies_LPAREN_QVARSYM_RPAREN LPAREN QVARSYM RPAREN
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
  deriving (Eq, Ord, Read, Show)

data Aexp =
    Aexp_implies_var Var
  | Aexp_implies_INTEGER INTEGER
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

data Op =
    Op_implies_varop Varop
  | Op_implies_conop Conop
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
  | StackValue_TODO_FDECL TODO_FDECL
  | StackValue_COLON_COLON COLON_COLON
  | StackValue_INTEGER INTEGER
  | StackValue_TODO_FUNLHS TODO_FUNLHS
  | StackValue_TODO_INST TODO_INST
  | StackValue_QVARID QVARID
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
  | StackValue_decls Decls
  | StackValue_decl_seq Decl_seq
  | StackValue_gendecl Gendecl
  | StackValue_funlhs Funlhs
  | StackValue_rhs Rhs
  | StackValue_var Var
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
  | StackValue_op Op
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
  , fdecl_implies_TODO_FDECL :: TODO_FDECL -> m Fdecl
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
    (4, Token (WHERE _)) -> Just (Reduce 0 90)
    (4, Token (LPAREN _)) -> Just (Shift 159)
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
    (12, Token (WHERE _)) -> Just (Reduce 1 91)
    (13, Token (RBRACE _)) -> Just (Reduce 0 55)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (13, Token (IMPORT _)) -> Just (Shift 19)
    (13, Token (TYPE _)) -> Just (Shift 20)
    (13, Token (DATA _)) -> Just (Shift 22)
    (13, Token (LPAREN _)) -> Just (Shift 166)
    (13, Token (NEWTYPE _)) -> Just (Shift 46)
    (13, Token (CLASS _)) -> Just (Shift 47)
    (13, Token (INSTANCE _)) -> Just (Shift 48)
    (13, Token (DEFAULT _)) -> Just (Shift 49)
    (13, Token (FOREIGN _)) -> Just (Shift 50)
    (13, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (13, Token (QVARID _)) -> Just (Shift 169)
    (13, Token (INFIXL _)) -> Just (Shift 186)
    (13, Token (INFIXR _)) -> Just (Shift 187)
    (13, Token (INFIX _)) -> Just (Shift 188)
    (14, EOF) -> Just (Reduce 3 3)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 55)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (16, Token (IMPORT _)) -> Just (Shift 19)
    (16, Token (TYPE _)) -> Just (Shift 20)
    (16, Token (DATA _)) -> Just (Shift 22)
    (16, Token (LPAREN _)) -> Just (Shift 166)
    (16, Token (NEWTYPE _)) -> Just (Shift 46)
    (16, Token (CLASS _)) -> Just (Shift 47)
    (16, Token (INSTANCE _)) -> Just (Shift 48)
    (16, Token (DEFAULT _)) -> Just (Shift 49)
    (16, Token (FOREIGN _)) -> Just (Shift 50)
    (16, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (16, Token (QVARID _)) -> Just (Shift 169)
    (16, Token (INFIXL _)) -> Just (Shift 186)
    (16, Token (INFIXR _)) -> Just (Shift 187)
    (16, Token (INFIX _)) -> Just (Shift 188)
    (17, Token (RBRACE _)) -> Just (Reduce 3 5)
    (18, Token (RBRACE _)) -> Just (Reduce 1 4)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (QCONID _)) -> Just (Reduce 0 74)
    (19, Token (QUALIFIED _)) -> Just (Shift 83)
    (20, Token (QCONID _)) -> Just (Shift 215)
    (20, Token (LPAREN _)) -> Just (Shift 95)
    (20, Token (QVARID _)) -> Just (Shift 200)
    (20, Token (LBRACKET _)) -> Just (Shift 97)
    (20, Token (EXCL _)) -> Just (Shift 199)
    (21, Token (QCONID _)) -> Just (Shift 215)
    (21, Token (LPAREN _)) -> Just (Shift 95)
    (21, Token (QVARID _)) -> Just (Shift 200)
    (21, Token (LBRACKET _)) -> Just (Shift 97)
    (21, Token (EXCL _)) -> Just (Shift 199)
    (22, Token (QCONID _)) -> Just (Shift 215)
    (22, Token (LPAREN _)) -> Just (Shift 95)
    (22, Token (QVARID _)) -> Just (Shift 200)
    (22, Token (LBRACKET _)) -> Just (Shift 97)
    (22, Token (EXCL _)) -> Just (Shift 199)
    (23, Token (QCONID _)) -> Just (Shift 107)
    (23, Token (LPAREN _)) -> Just (Shift 27)
    (24, Token (QCONID _)) -> Just (Shift 107)
    (24, Token (LPAREN _)) -> Just (Shift 28)
    (25, Token (QCONID _)) -> Just (Shift 107)
    (25, Token (LPAREN _)) -> Just (Shift 29)
    (26, Token (QCONID _)) -> Just (Shift 107)
    (26, Token (LPAREN _)) -> Just (Shift 30)
    (27, Token (QCONID _)) -> Just (Shift 107)
    (27, Token (RPAREN _)) -> Just (Shift 32)
    (28, Token (QCONID _)) -> Just (Shift 107)
    (28, Token (RPAREN _)) -> Just (Shift 33)
    (29, Token (QCONID _)) -> Just (Shift 107)
    (29, Token (RPAREN _)) -> Just (Shift 34)
    (30, Token (QCONID _)) -> Just (Shift 107)
    (30, Token (RPAREN _)) -> Just (Shift 35)
    (31, Token (QCONID _)) -> Just (Shift 215)
    (31, Token (LPAREN _)) -> Just (Shift 95)
    (31, Token (RPAREN _)) -> Just (Shift 36)
    (31, Token (QVARID _)) -> Just (Shift 200)
    (31, Token (LBRACKET _)) -> Just (Shift 97)
    (31, Token (EXCL _)) -> Just (Shift 199)
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
    (42, Token (QCONID _)) -> Just (Shift 215)
    (42, Token (LPAREN _)) -> Just (Shift 95)
    (42, Token (QVARID _)) -> Just (Shift 200)
    (42, Token (LBRACKET _)) -> Just (Shift 97)
    (42, Token (EXCL _)) -> Just (Shift 199)
    (43, Token (QCONID _)) -> Just (Shift 215)
    (43, Token (LPAREN _)) -> Just (Shift 95)
    (43, Token (QVARID _)) -> Just (Shift 200)
    (43, Token (LBRACKET _)) -> Just (Shift 97)
    (43, Token (EXCL _)) -> Just (Shift 199)
    (44, Token (QCONID _)) -> Just (Shift 215)
    (44, Token (LPAREN _)) -> Just (Shift 95)
    (44, Token (QVARID _)) -> Just (Shift 200)
    (44, Token (LBRACKET _)) -> Just (Shift 97)
    (44, Token (EXCL _)) -> Just (Shift 199)
    (45, Token (QCONID _)) -> Just (Shift 215)
    (45, Token (LPAREN _)) -> Just (Shift 95)
    (45, Token (QVARID _)) -> Just (Shift 200)
    (45, Token (LBRACKET _)) -> Just (Shift 97)
    (45, Token (EXCL _)) -> Just (Shift 199)
    (46, Token (QCONID _)) -> Just (Shift 215)
    (46, Token (LPAREN _)) -> Just (Shift 95)
    (46, Token (QVARID _)) -> Just (Shift 200)
    (46, Token (LBRACKET _)) -> Just (Shift 97)
    (46, Token (EXCL _)) -> Just (Shift 199)
    (47, Token (QCONID _)) -> Just (Shift 215)
    (47, Token (LPAREN _)) -> Just (Shift 95)
    (47, Token (QVARID _)) -> Just (Shift 200)
    (47, Token (LBRACKET _)) -> Just (Shift 97)
    (47, Token (EXCL _)) -> Just (Shift 199)
    (48, Token (QCONID _)) -> Just (Shift 215)
    (48, Token (LPAREN _)) -> Just (Shift 95)
    (48, Token (QVARID _)) -> Just (Shift 200)
    (48, Token (LBRACKET _)) -> Just (Shift 97)
    (48, Token (EXCL _)) -> Just (Shift 199)
    (49, Token (LPAREN _)) -> Just (Shift 31)
    (50, Token (TODO_FDECL _)) -> Just (Shift 123)
    (51, Token (QCONID _)) -> Just (Reduce 0 76)
    (51, Token (RBRACE _)) -> Just (Reduce 0 76)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 76)
    (51, Token (LPAREN _)) -> Just (Reduce 0 76)
    (51, Token (QVARID _)) -> Just (Reduce 0 76)
    (51, Token (AS _)) -> Just (Shift 9)
    (51, Token (HIDING _)) -> Just (Reduce 0 76)
    (51, Token (COMMA _)) -> Just (Reduce 0 76)
    (51, Token (QVARSYM _)) -> Just (Reduce 0 76)
    (51, Token (QCONSYM _)) -> Just (Reduce 0 76)
    (52, Token (RBRACE _)) -> Just (Reduce 0 78)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 78)
    (52, Token (LPAREN _)) -> Just (Shift 155)
    (52, Token (HIDING _)) -> Just (Shift 203)
    (53, Token (RBRACE _)) -> Just (Reduce 5 6)
    (53, Token (SEMICOLON _)) -> Just (Reduce 5 6)
    (54, Token (QCONID _)) -> Just (Shift 215)
    (54, Token (EQUAL _)) -> Just (Shift 21)
    (54, Token (LPAREN _)) -> Just (Shift 95)
    (54, Token (QVARID _)) -> Just (Shift 200)
    (54, Token (LBRACKET _)) -> Just (Shift 97)
    (54, Token (EXCL _)) -> Just (Shift 199)
    (55, Token (QCONID _)) -> Just (Shift 215)
    (55, Token (RBRACE _)) -> Just (Reduce 0 127)
    (55, Token (SEMICOLON _)) -> Just (Reduce 0 127)
    (55, Token (EQUAL _)) -> Just (Shift 86)
    (55, Token (DERIVING _)) -> Just (Reduce 0 127)
    (55, Token (LPAREN _)) -> Just (Shift 95)
    (55, Token (DARROW _)) -> Just (Shift 42)
    (55, Token (QVARID _)) -> Just (Shift 200)
    (55, Token (LBRACKET _)) -> Just (Shift 97)
    (55, Token (EXCL _)) -> Just (Shift 199)
    (56, Token (QCONID _)) -> Just (Shift 215)
    (56, Token (RBRACE _)) -> Just (Reduce 0 127)
    (56, Token (SEMICOLON _)) -> Just (Reduce 0 127)
    (56, Token (EQUAL _)) -> Just (Shift 86)
    (56, Token (DERIVING _)) -> Just (Reduce 0 127)
    (56, Token (LPAREN _)) -> Just (Shift 95)
    (56, Token (QVARID _)) -> Just (Shift 200)
    (56, Token (LBRACKET _)) -> Just (Shift 97)
    (56, Token (EXCL _)) -> Just (Shift 199)
    (57, Token (QCONID _)) -> Just (Shift 215)
    (57, Token (EQUAL _)) -> Just (Shift 112)
    (57, Token (LPAREN _)) -> Just (Shift 95)
    (57, Token (QVARID _)) -> Just (Shift 200)
    (57, Token (LBRACKET _)) -> Just (Shift 97)
    (57, Token (EXCL _)) -> Just (Shift 199)
    (58, Token (WHERE _)) -> Just (Shift 117)
    (58, Token (QCONID _)) -> Just (Shift 215)
    (58, Token (RBRACE _)) -> Just (Reduce 0 39)
    (58, Token (SEMICOLON _)) -> Just (Reduce 0 39)
    (58, Token (LPAREN _)) -> Just (Shift 95)
    (58, Token (QVARID _)) -> Just (Shift 200)
    (58, Token (LBRACKET _)) -> Just (Shift 97)
    (58, Token (EXCL _)) -> Just (Shift 199)
    (59, Token (WHERE _)) -> Just (Shift 119)
    (59, Token (QCONID _)) -> Just (Shift 215)
    (59, Token (RBRACE _)) -> Just (Reduce 0 47)
    (59, Token (SEMICOLON _)) -> Just (Reduce 0 47)
    (59, Token (LPAREN _)) -> Just (Shift 95)
    (59, Token (QVARID _)) -> Just (Shift 200)
    (59, Token (LBRACKET _)) -> Just (Shift 97)
    (59, Token (EXCL _)) -> Just (Shift 199)
    (60, Token (QCONID _)) -> Just (Shift 215)
    (60, Token (EQUAL _)) -> Just (Shift 112)
    (60, Token (LPAREN _)) -> Just (Shift 95)
    (60, Token (DARROW _)) -> Just (Shift 43)
    (60, Token (QVARID _)) -> Just (Shift 200)
    (60, Token (LBRACKET _)) -> Just (Shift 97)
    (60, Token (EXCL _)) -> Just (Shift 199)
    (61, Token (WHERE _)) -> Just (Shift 117)
    (61, Token (QCONID _)) -> Just (Shift 215)
    (61, Token (RBRACE _)) -> Just (Reduce 0 39)
    (61, Token (SEMICOLON _)) -> Just (Reduce 0 39)
    (61, Token (LPAREN _)) -> Just (Shift 95)
    (61, Token (DARROW _)) -> Just (Shift 44)
    (61, Token (QVARID _)) -> Just (Shift 200)
    (61, Token (LBRACKET _)) -> Just (Shift 97)
    (61, Token (EXCL _)) -> Just (Shift 199)
    (62, Token (WHERE _)) -> Just (Shift 119)
    (62, Token (QCONID _)) -> Just (Shift 215)
    (62, Token (RBRACE _)) -> Just (Reduce 0 47)
    (62, Token (SEMICOLON _)) -> Just (Reduce 0 47)
    (62, Token (LPAREN _)) -> Just (Shift 95)
    (62, Token (DARROW _)) -> Just (Shift 45)
    (62, Token (QVARID _)) -> Just (Shift 200)
    (62, Token (LBRACKET _)) -> Just (Shift 97)
    (62, Token (EXCL _)) -> Just (Shift 199)
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
    (83, Token (QCONID _)) -> Just (Reduce 1 75)
    (84, Token (QCONID _)) -> Just (Reduce 2 77)
    (84, Token (RBRACE _)) -> Just (Reduce 2 77)
    (84, Token (SEMICOLON _)) -> Just (Reduce 2 77)
    (84, Token (LPAREN _)) -> Just (Reduce 2 77)
    (84, Token (QVARID _)) -> Just (Reduce 2 77)
    (84, Token (HIDING _)) -> Just (Reduce 2 77)
    (84, Token (COMMA _)) -> Just (Reduce 2 77)
    (84, Token (QVARSYM _)) -> Just (Reduce 2 77)
    (84, Token (QCONSYM _)) -> Just (Reduce 2 77)
    (85, Token (RBRACE _)) -> Just (Reduce 1 79)
    (85, Token (SEMICOLON _)) -> Just (Reduce 1 79)
    (86, Token (QCONID _)) -> Just (Shift 215)
    (86, Token (LPAREN _)) -> Just (Shift 95)
    (86, Token (QVARID _)) -> Just (Shift 200)
    (86, Token (LBRACKET _)) -> Just (Shift 97)
    (86, Token (EXCL _)) -> Just (Shift 199)
    (87, Token (QCONID _)) -> Just (Shift 215)
    (87, Token (LPAREN _)) -> Just (Shift 95)
    (87, Token (QVARID _)) -> Just (Shift 200)
    (87, Token (LBRACKET _)) -> Just (Shift 97)
    (87, Token (EXCL _)) -> Just (Shift 199)
    (88, Token (QCONID _)) -> Just (Shift 215)
    (88, Token (LPAREN _)) -> Just (Shift 95)
    (88, Token (QVARID _)) -> Just (Shift 200)
    (88, Token (LBRACKET _)) -> Just (Shift 97)
    (88, Token (EXCL _)) -> Just (Shift 199)
    (89, Token (QCONID _)) -> Just (Shift 215)
    (89, Token (LPAREN _)) -> Just (Shift 95)
    (89, Token (QVARID _)) -> Just (Shift 200)
    (89, Token (LBRACKET _)) -> Just (Shift 97)
    (89, Token (EXCL _)) -> Just (Shift 199)
    (90, Token (QCONID _)) -> Just (Shift 215)
    (90, Token (LPAREN _)) -> Just (Shift 95)
    (90, Token (QVARID _)) -> Just (Shift 200)
    (90, Token (LBRACKET _)) -> Just (Shift 97)
    (90, Token (EXCL _)) -> Just (Shift 199)
    (91, Token (QCONID _)) -> Just (Shift 215)
    (91, Token (LPAREN _)) -> Just (Shift 95)
    (91, Token (QVARID _)) -> Just (Shift 200)
    (91, Token (LBRACKET _)) -> Just (Shift 97)
    (91, Token (EXCL _)) -> Just (Shift 199)
    (92, Token (QCONID _)) -> Just (Shift 215)
    (92, Token (LPAREN _)) -> Just (Shift 95)
    (92, Token (QVARID _)) -> Just (Shift 200)
    (92, Token (LBRACKET _)) -> Just (Shift 97)
    (92, Token (EXCL _)) -> Just (Shift 199)
    (93, Token (QCONID _)) -> Just (Shift 215)
    (93, Token (LPAREN _)) -> Just (Shift 95)
    (93, Token (QVARID _)) -> Just (Shift 200)
    (93, Token (LBRACKET _)) -> Just (Shift 97)
    (93, Token (EXCL _)) -> Just (Shift 199)
    (94, Token (QCONID _)) -> Just (Shift 215)
    (94, Token (LPAREN _)) -> Just (Shift 95)
    (94, Token (QVARID _)) -> Just (Shift 200)
    (94, Token (LBRACKET _)) -> Just (Shift 97)
    (94, Token (EXCL _)) -> Just (Shift 199)
    (95, Token (QCONID _)) -> Just (Shift 215)
    (95, Token (LPAREN _)) -> Just (Shift 95)
    (95, Token (RPAREN _)) -> Just (Shift 244)
    (95, Token (QVARID _)) -> Just (Shift 200)
    (95, Token (COMMA _)) -> Just (Shift 254)
    (95, Token (ARROW _)) -> Just (Shift 247)
    (95, Token (LBRACKET _)) -> Just (Shift 97)
    (95, Token (EXCL _)) -> Just (Shift 199)
    (95, Token (QCONSYM _)) -> Just (Shift 218)
    (96, Token (QCONID _)) -> Just (Shift 215)
    (96, Token (LPAREN _)) -> Just (Shift 95)
    (96, Token (QVARID _)) -> Just (Shift 200)
    (96, Token (LBRACKET _)) -> Just (Shift 97)
    (96, Token (EXCL _)) -> Just (Shift 199)
    (97, Token (QCONID _)) -> Just (Shift 215)
    (97, Token (LPAREN _)) -> Just (Shift 95)
    (97, Token (QVARID _)) -> Just (Shift 200)
    (97, Token (LBRACKET _)) -> Just (Shift 97)
    (97, Token (RBRACKET _)) -> Just (Shift 248)
    (97, Token (EXCL _)) -> Just (Shift 199)
    (98, Token (QCONID _)) -> Just (Shift 215)
    (98, Token (RBRACE _)) -> Just (Reduce 1 131)
    (98, Token (SEMICOLON _)) -> Just (Reduce 1 131)
    (98, Token (DERIVING _)) -> Just (Reduce 1 131)
    (98, Token (LPAREN _)) -> Just (Shift 95)
    (98, Token (QVARID _)) -> Just (Shift 200)
    (98, Token (LBRACKET _)) -> Just (Shift 97)
    (98, Token (EXCL _)) -> Just (Shift 199)
    (98, Token (PIPE _)) -> Just (Reduce 1 131)
    (98, Token (QCONSYM _)) -> Just (Shift 262)
    (98, Token (BACKQUOTE _)) -> Just (Shift 263)
    (99, Token (QCONID _)) -> Just (Shift 215)
    (99, Token (RBRACE _)) -> Just (Reduce 3 132)
    (99, Token (SEMICOLON _)) -> Just (Reduce 3 132)
    (99, Token (DERIVING _)) -> Just (Reduce 3 132)
    (99, Token (LPAREN _)) -> Just (Shift 95)
    (99, Token (QVARID _)) -> Just (Shift 200)
    (99, Token (LBRACKET _)) -> Just (Shift 97)
    (99, Token (EXCL _)) -> Just (Shift 199)
    (99, Token (PIPE _)) -> Just (Reduce 3 132)
    (100, Token (QCONID _)) -> Just (Shift 215)
    (100, Token (RBRACE _)) -> Just (Reduce 1 108)
    (100, Token (SEMICOLON _)) -> Just (Reduce 1 108)
    (100, Token (LPAREN _)) -> Just (Shift 95)
    (100, Token (DARROW _)) -> Just (Shift 91)
    (100, Token (QVARID _)) -> Just (Shift 200)
    (100, Token (ARROW _)) -> Just (Shift 90)
    (100, Token (LBRACKET _)) -> Just (Shift 97)
    (100, Token (EXCL _)) -> Just (Shift 199)
    (101, Token (QCONID _)) -> Just (Shift 215)
    (101, Token (RBRACE _)) -> Just (Reduce 1 108)
    (101, Token (SEMICOLON _)) -> Just (Reduce 1 108)
    (101, Token (LPAREN _)) -> Just (Shift 95)
    (101, Token (RPAREN _)) -> Just (Reduce 1 108)
    (101, Token (QVARID _)) -> Just (Shift 200)
    (101, Token (COMMA _)) -> Just (Reduce 1 108)
    (101, Token (ARROW _)) -> Just (Shift 90)
    (101, Token (LBRACKET _)) -> Just (Shift 97)
    (101, Token (RBRACKET _)) -> Just (Reduce 1 108)
    (101, Token (EXCL _)) -> Just (Shift 199)
    (102, Token (WHERE _)) -> Just (Reduce 1 110)
    (102, Token (QCONID _)) -> Just (Reduce 1 110)
    (102, Token (LBRACE _)) -> Just (Reduce 1 110)
    (102, Token (RBRACE _)) -> Just (Reduce 1 110)
    (102, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (102, Token (EQUAL _)) -> Just (Reduce 1 110)
    (102, Token (DERIVING _)) -> Just (Reduce 1 110)
    (102, Token (LPAREN _)) -> Just (Reduce 1 110)
    (102, Token (RPAREN _)) -> Just (Reduce 1 110)
    (102, Token (DARROW _)) -> Just (Reduce 1 110)
    (102, Token (COLON_COLON _)) -> Just (Reduce 1 110)
    (102, Token (INTEGER _)) -> Just (Reduce 1 110)
    (102, Token (TODO_FUNLHS _)) -> Just (Reduce 1 110)
    (102, Token (QVARID _)) -> Just (Reduce 1 110)
    (102, Token (COMMA _)) -> Just (Reduce 1 110)
    (102, Token (ARROW _)) -> Just (Reduce 1 110)
    (102, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (102, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (102, Token (EXCL _)) -> Just (Reduce 1 110)
    (102, Token (PIPE _)) -> Just (Reduce 1 110)
    (102, Token (QVARSYM _)) -> Just (Reduce 1 110)
    (102, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (102, Token (INFIXL _)) -> Just (Reduce 1 110)
    (102, Token (INFIXR _)) -> Just (Reduce 1 110)
    (102, Token (INFIX _)) -> Just (Reduce 1 110)
    (102, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (103, Token (WHERE _)) -> Just (Reduce 2 111)
    (103, Token (QCONID _)) -> Just (Reduce 2 111)
    (103, Token (LBRACE _)) -> Just (Reduce 2 111)
    (103, Token (RBRACE _)) -> Just (Reduce 2 111)
    (103, Token (SEMICOLON _)) -> Just (Reduce 2 111)
    (103, Token (EQUAL _)) -> Just (Reduce 2 111)
    (103, Token (DERIVING _)) -> Just (Reduce 2 111)
    (103, Token (LPAREN _)) -> Just (Reduce 2 111)
    (103, Token (RPAREN _)) -> Just (Reduce 2 111)
    (103, Token (DARROW _)) -> Just (Reduce 2 111)
    (103, Token (COLON_COLON _)) -> Just (Reduce 2 111)
    (103, Token (INTEGER _)) -> Just (Reduce 2 111)
    (103, Token (TODO_FUNLHS _)) -> Just (Reduce 2 111)
    (103, Token (QVARID _)) -> Just (Reduce 2 111)
    (103, Token (COMMA _)) -> Just (Reduce 2 111)
    (103, Token (ARROW _)) -> Just (Reduce 2 111)
    (103, Token (LBRACKET _)) -> Just (Reduce 2 111)
    (103, Token (RBRACKET _)) -> Just (Reduce 2 111)
    (103, Token (EXCL _)) -> Just (Reduce 2 111)
    (103, Token (PIPE _)) -> Just (Reduce 2 111)
    (103, Token (QVARSYM _)) -> Just (Reduce 2 111)
    (103, Token (QCONSYM _)) -> Just (Reduce 2 111)
    (103, Token (INFIXL _)) -> Just (Reduce 2 111)
    (103, Token (INFIXR _)) -> Just (Reduce 2 111)
    (103, Token (INFIX _)) -> Just (Reduce 2 111)
    (103, Token (BACKQUOTE _)) -> Just (Reduce 2 111)
    (104, Token (RBRACE _)) -> Just (Reduce 3 109)
    (104, Token (SEMICOLON _)) -> Just (Reduce 3 109)
    (104, Token (RPAREN _)) -> Just (Reduce 3 109)
    (104, Token (COMMA _)) -> Just (Reduce 3 109)
    (104, Token (RBRACKET _)) -> Just (Reduce 3 109)
    (105, Token (RBRACE _)) -> Just (Reduce 2 128)
    (105, Token (SEMICOLON _)) -> Just (Reduce 2 128)
    (105, Token (DERIVING _)) -> Just (Reduce 2 128)
    (106, Token (QCONID _)) -> Just (Shift 107)
    (107, Token (RBRACE _)) -> Just (Reduce 1 160)
    (107, Token (SEMICOLON _)) -> Just (Reduce 1 160)
    (107, Token (RPAREN _)) -> Just (Reduce 1 160)
    (107, Token (COMMA _)) -> Just (Reduce 1 160)
    (108, Token (RPAREN _)) -> Just (Reduce 1 161)
    (108, Token (COMMA _)) -> Just (Shift 106)
    (109, Token (RPAREN _)) -> Just (Reduce 3 162)
    (110, Token (LPAREN _)) -> Just (Shift 166)
    (110, Token (QVARID _)) -> Just (Shift 169)
    (111, Token (RBRACE _)) -> Just (Reduce 7 142)
    (111, Token (SEMICOLON _)) -> Just (Reduce 7 142)
    (111, Token (DERIVING _)) -> Just (Reduce 7 142)
    (112, Token (QCONID _)) -> Just (Shift 215)
    (112, Token (LPAREN _)) -> Just (Shift 216)
    (113, Token (RBRACE _)) -> Just (Shift 111)
    (114, Token (COLON_COLON _)) -> Just (Shift 93)
    (115, Token (QCONID _)) -> Just (Shift 215)
    (115, Token (LBRACE _)) -> Just (Shift 110)
    (115, Token (LPAREN _)) -> Just (Shift 95)
    (115, Token (QVARID _)) -> Just (Shift 200)
    (115, Token (LBRACKET _)) -> Just (Shift 97)
    (115, Token (EXCL _)) -> Just (Shift 199)
    (116, Token (RBRACE _)) -> Just (Reduce 3 141)
    (116, Token (SEMICOLON _)) -> Just (Reduce 3 141)
    (116, Token (DERIVING _)) -> Just (Reduce 3 141)
    (117, Token (LBRACE _)) -> Just (Shift 136)
    (118, Token (RBRACE _)) -> Just (Reduce 2 40)
    (118, Token (SEMICOLON _)) -> Just (Reduce 2 40)
    (119, Token (LBRACE _)) -> Just (Shift 144)
    (120, Token (RBRACE _)) -> Just (Reduce 2 48)
    (120, Token (SEMICOLON _)) -> Just (Reduce 2 48)
    (121, Token (RPAREN _)) -> Just (Reduce 1 106)
    (121, Token (COMMA _)) -> Just (Shift 94)
    (122, Token (RPAREN _)) -> Just (Reduce 3 107)
    (123, Token (RBRACE _)) -> Just (Reduce 1 32)
    (123, Token (SEMICOLON _)) -> Just (Reduce 1 32)
    (124, Token (RBRACE _)) -> Just (Reduce 0 55)
    (124, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (124, Token (LPAREN _)) -> Just (Shift 166)
    (124, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (124, Token (QVARID _)) -> Just (Shift 169)
    (124, Token (INFIXL _)) -> Just (Shift 186)
    (124, Token (INFIXR _)) -> Just (Shift 187)
    (124, Token (INFIX _)) -> Just (Shift 188)
    (125, Token (RBRACE _)) -> Just (Reduce 0 55)
    (125, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (125, Token (LPAREN _)) -> Just (Shift 166)
    (125, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (125, Token (QVARID _)) -> Just (Shift 169)
    (125, Token (INFIXL _)) -> Just (Shift 186)
    (125, Token (INFIXR _)) -> Just (Shift 187)
    (125, Token (INFIX _)) -> Just (Shift 188)
    (126, Token (RBRACE _)) -> Just (Reduce 1 36)
    (126, Token (SEMICOLON _)) -> Just (Reduce 1 36)
    (127, Token (EQUAL _)) -> Just (Shift 151)
    (128, Token (RBRACE _)) -> Just (Reduce 2 37)
    (128, Token (SEMICOLON _)) -> Just (Reduce 2 37)
    (129, Token (RBRACE _)) -> Just (Reduce 2 38)
    (129, Token (SEMICOLON _)) -> Just (Reduce 2 38)
    (130, Token (EQUAL _)) -> Just (Shift 151)
    (130, Token (COLON_COLON _)) -> Just (Reduce 1 145)
    (130, Token (COMMA _)) -> Just (Shift 163)
    (131, Token (LBRACE _)) -> Just (Shift 124)
    (132, Token (RBRACE _)) -> Just (Reduce 3 33)
    (132, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (133, Token (RBRACE _)) -> Just (Shift 132)
    (134, Token (RBRACE _)) -> Just (Reduce 1 34)
    (134, Token (SEMICOLON _)) -> Just (Shift 125)
    (135, Token (RBRACE _)) -> Just (Reduce 3 35)
    (136, Token (RBRACE _)) -> Just (Reduce 0 55)
    (136, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (136, Token (LPAREN _)) -> Just (Shift 166)
    (136, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (136, Token (QVARID _)) -> Just (Shift 169)
    (136, Token (INFIXL _)) -> Just (Shift 186)
    (136, Token (INFIXR _)) -> Just (Shift 187)
    (136, Token (INFIX _)) -> Just (Shift 188)
    (137, Token (RBRACE _)) -> Just (Reduce 0 55)
    (137, Token (SEMICOLON _)) -> Just (Reduce 0 55)
    (137, Token (LPAREN _)) -> Just (Shift 166)
    (137, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (137, Token (QVARID _)) -> Just (Shift 169)
    (137, Token (INFIXL _)) -> Just (Shift 186)
    (137, Token (INFIXR _)) -> Just (Shift 187)
    (137, Token (INFIX _)) -> Just (Shift 188)
    (138, Token (RBRACE _)) -> Just (Reduce 5 57)
    (138, Token (SEMICOLON _)) -> Just (Reduce 5 57)
    (139, Token (RBRACE _)) -> Just (Reduce 3 56)
    (139, Token (SEMICOLON _)) -> Just (Reduce 3 56)
    (140, Token (COLON_COLON _)) -> Just (Shift 89)
    (141, Token (QCONID _)) -> Just (Reduce 0 59)
    (141, Token (INTEGER _)) -> Just (Shift 189)
    (141, Token (QVARID _)) -> Just (Reduce 0 59)
    (141, Token (COMMA _)) -> Just (Reduce 0 59)
    (141, Token (QVARSYM _)) -> Just (Reduce 0 59)
    (141, Token (QCONSYM _)) -> Just (Reduce 0 59)
    (141, Token (BACKQUOTE _)) -> Just (Reduce 0 59)
    (142, Token (QVARSYM _)) -> Just (Shift 273)
    (142, Token (QCONSYM _)) -> Just (Shift 262)
    (142, Token (BACKQUOTE _)) -> Just (Shift 264)
    (143, Token (RBRACE _)) -> Just (Reduce 3 58)
    (143, Token (SEMICOLON _)) -> Just (Reduce 3 58)
    (144, Token (RBRACE _)) -> Just (Reduce 0 52)
    (144, Token (SEMICOLON _)) -> Just (Reduce 0 52)
    (144, Token (LPAREN _)) -> Just (Shift 166)
    (144, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (144, Token (QVARID _)) -> Just (Shift 169)
    (145, Token (RBRACE _)) -> Just (Reduce 0 52)
    (145, Token (SEMICOLON _)) -> Just (Reduce 0 52)
    (145, Token (LPAREN _)) -> Just (Shift 166)
    (145, Token (TODO_FUNLHS _)) -> Just (Shift 146)
    (145, Token (QVARID _)) -> Just (Shift 169)
    (146, Token (EQUAL _)) -> Just (Reduce 1 61)
    (146, Token (LPAREN _)) -> Just (Reduce 1 61)
    (146, Token (INTEGER _)) -> Just (Reduce 1 61)
    (146, Token (QVARID _)) -> Just (Reduce 1 61)
    (146, Token (QVARSYM _)) -> Just (Reduce 1 61)
    (147, Token (EQUAL _)) -> Just (Shift 151)
    (148, Token (EQUAL _)) -> Just (Shift 151)
    (148, Token (COLON_COLON _)) -> Just (Reduce 1 145)
    (148, Token (COMMA _)) -> Just (Shift 163)
    (149, Token (EQUAL _)) -> Just (Shift 151)
    (150, Token (EQUAL _)) -> Just (Shift 151)
    (151, Token (LPAREN _)) -> Just (Shift 166)
    (151, Token (INTEGER _)) -> Just (Shift 197)
    (151, Token (QVARID _)) -> Just (Shift 169)
    (152, Token (RBRACE _)) -> Just (Reduce 4 63)
    (152, Token (SEMICOLON _)) -> Just (Reduce 4 63)
    (153, Token (WHERE _)) -> Just (Shift 131)
    (153, Token (RBRACE _)) -> Just (Reduce 2 62)
    (153, Token (SEMICOLON _)) -> Just (Reduce 2 62)
    (154, Token (WHERE _)) -> Just (Reduce 1 66)
    (154, Token (RBRACE _)) -> Just (Reduce 1 66)
    (154, Token (SEMICOLON _)) -> Just (Reduce 1 66)
    (154, Token (LPAREN _)) -> Just (Shift 166)
    (154, Token (INTEGER _)) -> Just (Shift 197)
    (154, Token (QVARID _)) -> Just (Shift 169)
    (155, Token (QCONID _)) -> Just (Shift 215)
    (155, Token (LPAREN _)) -> Just (Shift 167)
    (155, Token (RPAREN _)) -> Just (Reduce 0 82)
    (155, Token (QVARID _)) -> Just (Shift 169)
    (156, Token (QCONID _)) -> Just (Shift 215)
    (156, Token (LPAREN _)) -> Just (Shift 167)
    (156, Token (RPAREN _)) -> Just (Reduce 0 82)
    (156, Token (QVARID _)) -> Just (Shift 169)
    (157, Token (QCONID _)) -> Just (Shift 215)
    (157, Token (LPAREN _)) -> Just (Shift 167)
    (157, Token (RPAREN _)) -> Just (Reduce 0 82)
    (157, Token (QVARID _)) -> Just (Shift 169)
    (158, Token (QCONID _)) -> Just (Shift 215)
    (158, Token (LPAREN _)) -> Just (Shift 167)
    (158, Token (RPAREN _)) -> Just (Shift 208)
    (158, Token (QVARID _)) -> Just (Shift 169)
    (158, Token (DOT_DOT _)) -> Just (Shift 211)
    (159, Token (MODULE _)) -> Just (Shift 10)
    (159, Token (QCONID _)) -> Just (Shift 215)
    (159, Token (LPAREN _)) -> Just (Shift 167)
    (159, Token (RPAREN _)) -> Just (Reduce 0 93)
    (159, Token (QVARID _)) -> Just (Shift 169)
    (160, Token (MODULE _)) -> Just (Shift 10)
    (160, Token (QCONID _)) -> Just (Shift 215)
    (160, Token (LPAREN _)) -> Just (Shift 167)
    (160, Token (RPAREN _)) -> Just (Reduce 0 93)
    (160, Token (QVARID _)) -> Just (Shift 169)
    (161, Token (QCONID _)) -> Just (Shift 215)
    (161, Token (LPAREN _)) -> Just (Shift 167)
    (161, Token (QVARID _)) -> Just (Shift 169)
    (162, Token (QCONID _)) -> Just (Shift 215)
    (162, Token (LPAREN _)) -> Just (Shift 167)
    (162, Token (RPAREN _)) -> Just (Shift 225)
    (162, Token (QVARID _)) -> Just (Shift 169)
    (162, Token (DOT_DOT _)) -> Just (Shift 228)
    (163, Token (LPAREN _)) -> Just (Shift 166)
    (163, Token (QVARID _)) -> Just (Shift 169)
    (164, Token (RBRACE _)) -> Just (Shift 258)
    (164, Token (LPAREN _)) -> Just (Shift 166)
    (164, Token (QVARID _)) -> Just (Shift 169)
    (165, Token (LPAREN _)) -> Just (Shift 166)
    (165, Token (QVARID _)) -> Just (Shift 169)
    (166, Token (QVARSYM _)) -> Just (Shift 170)
    (167, Token (QVARSYM _)) -> Just (Shift 170)
    (167, Token (QCONSYM _)) -> Just (Shift 218)
    (168, Token (WHERE _)) -> Just (Reduce 3 148)
    (168, Token (RBRACE _)) -> Just (Reduce 3 148)
    (168, Token (SEMICOLON _)) -> Just (Reduce 3 148)
    (168, Token (EQUAL _)) -> Just (Reduce 3 148)
    (168, Token (LPAREN _)) -> Just (Reduce 3 148)
    (168, Token (RPAREN _)) -> Just (Reduce 3 148)
    (168, Token (COLON_COLON _)) -> Just (Reduce 3 148)
    (168, Token (INTEGER _)) -> Just (Reduce 3 148)
    (168, Token (QVARID _)) -> Just (Reduce 3 148)
    (168, Token (COMMA _)) -> Just (Reduce 3 148)
    (168, Token (QVARSYM _)) -> Just (Reduce 3 148)
    (169, Token (WHERE _)) -> Just (Reduce 1 147)
    (169, Token (RBRACE _)) -> Just (Reduce 1 147)
    (169, Token (SEMICOLON _)) -> Just (Reduce 1 147)
    (169, Token (EQUAL _)) -> Just (Reduce 1 147)
    (169, Token (LPAREN _)) -> Just (Reduce 1 147)
    (169, Token (RPAREN _)) -> Just (Reduce 1 147)
    (169, Token (COLON_COLON _)) -> Just (Reduce 1 147)
    (169, Token (INTEGER _)) -> Just (Reduce 1 147)
    (169, Token (QVARID _)) -> Just (Reduce 1 147)
    (169, Token (COMMA _)) -> Just (Reduce 1 147)
    (169, Token (QVARSYM _)) -> Just (Reduce 1 147)
    (170, Token (RPAREN _)) -> Just (Shift 168)
    (171, Token (RBRACE _)) -> Just (Reduce 3 41)
    (171, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (172, Token (RBRACE _)) -> Just (Shift 171)
    (173, Token (RBRACE _)) -> Just (Reduce 3 43)
    (174, Token (RBRACE _)) -> Just (Reduce 1 42)
    (174, Token (SEMICOLON _)) -> Just (Shift 137)
    (175, Token (RBRACE _)) -> Just (Reduce 1 44)
    (175, Token (SEMICOLON _)) -> Just (Reduce 1 44)
    (176, Token (RBRACE _)) -> Just (Reduce 2 45)
    (176, Token (SEMICOLON _)) -> Just (Reduce 2 45)
    (177, Token (RBRACE _)) -> Just (Reduce 2 46)
    (177, Token (SEMICOLON _)) -> Just (Reduce 2 46)
    (178, Token (RBRACE _)) -> Just (Reduce 3 49)
    (178, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (179, Token (RBRACE _)) -> Just (Shift 178)
    (180, Token (RBRACE _)) -> Just (Reduce 3 51)
    (181, Token (RBRACE _)) -> Just (Reduce 1 50)
    (181, Token (SEMICOLON _)) -> Just (Shift 145)
    (182, Token (RBRACE _)) -> Just (Reduce 2 53)
    (182, Token (SEMICOLON _)) -> Just (Reduce 2 53)
    (183, Token (RBRACE _)) -> Just (Reduce 2 54)
    (183, Token (SEMICOLON _)) -> Just (Reduce 2 54)
    (184, Token (COLON_COLON _)) -> Just (Reduce 1 145)
    (184, Token (COMMA _)) -> Just (Shift 163)
    (185, Token (COLON_COLON _)) -> Just (Reduce 3 146)
    (186, Token (QCONID _)) -> Just (Reduce 1 151)
    (186, Token (INTEGER _)) -> Just (Reduce 1 151)
    (186, Token (QVARID _)) -> Just (Reduce 1 151)
    (186, Token (COMMA _)) -> Just (Reduce 1 151)
    (186, Token (QVARSYM _)) -> Just (Reduce 1 151)
    (186, Token (QCONSYM _)) -> Just (Reduce 1 151)
    (186, Token (BACKQUOTE _)) -> Just (Reduce 1 151)
    (187, Token (QCONID _)) -> Just (Reduce 1 152)
    (187, Token (INTEGER _)) -> Just (Reduce 1 152)
    (187, Token (QVARID _)) -> Just (Reduce 1 152)
    (187, Token (COMMA _)) -> Just (Reduce 1 152)
    (187, Token (QVARSYM _)) -> Just (Reduce 1 152)
    (187, Token (QCONSYM _)) -> Just (Reduce 1 152)
    (187, Token (BACKQUOTE _)) -> Just (Reduce 1 152)
    (188, Token (QCONID _)) -> Just (Reduce 1 153)
    (188, Token (INTEGER _)) -> Just (Reduce 1 153)
    (188, Token (QVARID _)) -> Just (Reduce 1 153)
    (188, Token (COMMA _)) -> Just (Reduce 1 153)
    (188, Token (QVARSYM _)) -> Just (Reduce 1 153)
    (188, Token (QCONSYM _)) -> Just (Reduce 1 153)
    (188, Token (BACKQUOTE _)) -> Just (Reduce 1 153)
    (189, Token (QCONID _)) -> Just (Reduce 1 60)
    (189, Token (QVARID _)) -> Just (Reduce 1 60)
    (189, Token (COMMA _)) -> Just (Reduce 1 60)
    (189, Token (QVARSYM _)) -> Just (Reduce 1 60)
    (189, Token (QCONSYM _)) -> Just (Reduce 1 60)
    (189, Token (BACKQUOTE _)) -> Just (Reduce 1 60)
    (190, Token (QVARSYM _)) -> Just (Shift 273)
    (190, Token (QCONSYM _)) -> Just (Shift 262)
    (190, Token (BACKQUOTE _)) -> Just (Shift 264)
    (191, Token (RBRACE _)) -> Just (Reduce 3 144)
    (191, Token (SEMICOLON _)) -> Just (Reduce 3 144)
    (192, Token (RBRACE _)) -> Just (Reduce 1 143)
    (192, Token (SEMICOLON _)) -> Just (Reduce 1 143)
    (192, Token (COMMA _)) -> Just (Shift 190)
    (193, Token (WHERE _)) -> Just (Reduce 1 64)
    (193, Token (RBRACE _)) -> Just (Reduce 1 64)
    (193, Token (SEMICOLON _)) -> Just (Reduce 1 64)
    (194, Token (WHERE _)) -> Just (Reduce 1 65)
    (194, Token (RBRACE _)) -> Just (Reduce 1 65)
    (194, Token (SEMICOLON _)) -> Just (Reduce 1 65)
    (195, Token (WHERE _)) -> Just (Reduce 1 67)
    (195, Token (RBRACE _)) -> Just (Reduce 1 67)
    (195, Token (SEMICOLON _)) -> Just (Reduce 1 67)
    (195, Token (LPAREN _)) -> Just (Reduce 1 67)
    (195, Token (INTEGER _)) -> Just (Reduce 1 67)
    (195, Token (QVARID _)) -> Just (Reduce 1 67)
    (195, Token (QVARSYM _)) -> Just (Reduce 1 67)
    (196, Token (WHERE _)) -> Just (Reduce 2 68)
    (196, Token (RBRACE _)) -> Just (Reduce 2 68)
    (196, Token (SEMICOLON _)) -> Just (Reduce 2 68)
    (196, Token (LPAREN _)) -> Just (Reduce 2 68)
    (196, Token (INTEGER _)) -> Just (Reduce 2 68)
    (196, Token (QVARID _)) -> Just (Reduce 2 68)
    (196, Token (QVARSYM _)) -> Just (Reduce 2 68)
    (197, Token (WHERE _)) -> Just (Reduce 1 70)
    (197, Token (RBRACE _)) -> Just (Reduce 1 70)
    (197, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (197, Token (LPAREN _)) -> Just (Reduce 1 70)
    (197, Token (INTEGER _)) -> Just (Reduce 1 70)
    (197, Token (QVARID _)) -> Just (Reduce 1 70)
    (197, Token (QVARSYM _)) -> Just (Reduce 1 70)
    (198, Token (WHERE _)) -> Just (Reduce 1 69)
    (198, Token (RBRACE _)) -> Just (Reduce 1 69)
    (198, Token (SEMICOLON _)) -> Just (Reduce 1 69)
    (198, Token (LPAREN _)) -> Just (Reduce 1 69)
    (198, Token (INTEGER _)) -> Just (Reduce 1 69)
    (198, Token (QVARID _)) -> Just (Reduce 1 69)
    (198, Token (QVARSYM _)) -> Just (Reduce 1 69)
    (199, Token (QCONID _)) -> Just (Shift 215)
    (199, Token (LPAREN _)) -> Just (Shift 95)
    (199, Token (QVARID _)) -> Just (Shift 200)
    (199, Token (LBRACKET _)) -> Just (Shift 97)
    (199, Token (EXCL _)) -> Just (Shift 199)
    (200, Token (WHERE _)) -> Just (Reduce 1 73)
    (200, Token (QCONID _)) -> Just (Reduce 1 73)
    (200, Token (LBRACE _)) -> Just (Reduce 1 73)
    (200, Token (RBRACE _)) -> Just (Reduce 1 73)
    (200, Token (SEMICOLON _)) -> Just (Reduce 1 73)
    (200, Token (EQUAL _)) -> Just (Reduce 1 73)
    (200, Token (DERIVING _)) -> Just (Reduce 1 73)
    (200, Token (LPAREN _)) -> Just (Reduce 1 73)
    (200, Token (RPAREN _)) -> Just (Reduce 1 73)
    (200, Token (DARROW _)) -> Just (Reduce 1 73)
    (200, Token (COLON_COLON _)) -> Just (Reduce 1 73)
    (200, Token (INTEGER _)) -> Just (Reduce 1 73)
    (200, Token (TODO_FUNLHS _)) -> Just (Reduce 1 73)
    (200, Token (QVARID _)) -> Just (Reduce 1 73)
    (200, Token (COMMA _)) -> Just (Reduce 1 73)
    (200, Token (ARROW _)) -> Just (Reduce 1 73)
    (200, Token (LBRACKET _)) -> Just (Reduce 1 73)
    (200, Token (RBRACKET _)) -> Just (Reduce 1 73)
    (200, Token (EXCL _)) -> Just (Reduce 1 73)
    (200, Token (PIPE _)) -> Just (Reduce 1 73)
    (200, Token (QVARSYM _)) -> Just (Reduce 1 73)
    (200, Token (QCONSYM _)) -> Just (Reduce 1 73)
    (200, Token (INFIXL _)) -> Just (Reduce 1 73)
    (200, Token (INFIXR _)) -> Just (Reduce 1 73)
    (200, Token (INFIX _)) -> Just (Reduce 1 73)
    (200, Token (BACKQUOTE _)) -> Just (Reduce 1 73)
    (201, Token (RBRACE _)) -> Just (Reduce 3 80)
    (201, Token (SEMICOLON _)) -> Just (Reduce 3 80)
    (202, Token (RBRACE _)) -> Just (Reduce 4 81)
    (202, Token (SEMICOLON _)) -> Just (Reduce 4 81)
    (203, Token (LPAREN _)) -> Just (Shift 156)
    (204, Token (RPAREN _)) -> Just (Shift 201)
    (205, Token (RPAREN _)) -> Just (Shift 202)
    (206, Token (RPAREN _)) -> Just (Reduce 3 84)
    (207, Token (RPAREN _)) -> Just (Reduce 1 83)
    (207, Token (COMMA _)) -> Just (Shift 157)
    (208, Token (RPAREN _)) -> Just (Reduce 3 87)
    (208, Token (COMMA _)) -> Just (Reduce 3 87)
    (209, Token (RPAREN _)) -> Just (Reduce 4 88)
    (209, Token (COMMA _)) -> Just (Reduce 4 88)
    (210, Token (RPAREN _)) -> Just (Reduce 4 89)
    (210, Token (COMMA _)) -> Just (Reduce 4 89)
    (211, Token (RPAREN _)) -> Just (Shift 209)
    (212, Token (RPAREN _)) -> Just (Reduce 1 85)
    (212, Token (COMMA _)) -> Just (Reduce 1 85)
    (213, Token (LPAREN _)) -> Just (Shift 158)
    (213, Token (RPAREN _)) -> Just (Reduce 1 86)
    (213, Token (COMMA _)) -> Just (Reduce 1 86)
    (214, Token (RPAREN _)) -> Just (Shift 210)
    (215, Token (WHERE _)) -> Just (Reduce 1 149)
    (215, Token (QCONID _)) -> Just (Reduce 1 149)
    (215, Token (LBRACE _)) -> Just (Reduce 1 149)
    (215, Token (RBRACE _)) -> Just (Reduce 1 149)
    (215, Token (SEMICOLON _)) -> Just (Reduce 1 149)
    (215, Token (EQUAL _)) -> Just (Reduce 1 149)
    (215, Token (DERIVING _)) -> Just (Reduce 1 149)
    (215, Token (LPAREN _)) -> Just (Reduce 1 149)
    (215, Token (RPAREN _)) -> Just (Reduce 1 149)
    (215, Token (DARROW _)) -> Just (Reduce 1 149)
    (215, Token (COLON_COLON _)) -> Just (Reduce 1 149)
    (215, Token (INTEGER _)) -> Just (Reduce 1 149)
    (215, Token (TODO_FUNLHS _)) -> Just (Reduce 1 149)
    (215, Token (QVARID _)) -> Just (Reduce 1 149)
    (215, Token (COMMA _)) -> Just (Reduce 1 149)
    (215, Token (ARROW _)) -> Just (Reduce 1 149)
    (215, Token (LBRACKET _)) -> Just (Reduce 1 149)
    (215, Token (RBRACKET _)) -> Just (Reduce 1 149)
    (215, Token (EXCL _)) -> Just (Reduce 1 149)
    (215, Token (PIPE _)) -> Just (Reduce 1 149)
    (215, Token (QVARSYM _)) -> Just (Reduce 1 149)
    (215, Token (QCONSYM _)) -> Just (Reduce 1 149)
    (215, Token (INFIXL _)) -> Just (Reduce 1 149)
    (215, Token (INFIXR _)) -> Just (Reduce 1 149)
    (215, Token (INFIX _)) -> Just (Reduce 1 149)
    (215, Token (BACKQUOTE _)) -> Just (Reduce 1 149)
    (216, Token (QCONSYM _)) -> Just (Shift 218)
    (217, Token (WHERE _)) -> Just (Reduce 3 150)
    (217, Token (QCONID _)) -> Just (Reduce 3 150)
    (217, Token (LBRACE _)) -> Just (Reduce 3 150)
    (217, Token (RBRACE _)) -> Just (Reduce 3 150)
    (217, Token (SEMICOLON _)) -> Just (Reduce 3 150)
    (217, Token (EQUAL _)) -> Just (Reduce 3 150)
    (217, Token (DERIVING _)) -> Just (Reduce 3 150)
    (217, Token (LPAREN _)) -> Just (Reduce 3 150)
    (217, Token (RPAREN _)) -> Just (Reduce 3 150)
    (217, Token (DARROW _)) -> Just (Reduce 3 150)
    (217, Token (COLON_COLON _)) -> Just (Reduce 3 150)
    (217, Token (INTEGER _)) -> Just (Reduce 3 150)
    (217, Token (TODO_FUNLHS _)) -> Just (Reduce 3 150)
    (217, Token (QVARID _)) -> Just (Reduce 3 150)
    (217, Token (COMMA _)) -> Just (Reduce 3 150)
    (217, Token (ARROW _)) -> Just (Reduce 3 150)
    (217, Token (LBRACKET _)) -> Just (Reduce 3 150)
    (217, Token (RBRACKET _)) -> Just (Reduce 3 150)
    (217, Token (EXCL _)) -> Just (Reduce 3 150)
    (217, Token (PIPE _)) -> Just (Reduce 3 150)
    (217, Token (QVARSYM _)) -> Just (Reduce 3 150)
    (217, Token (QCONSYM _)) -> Just (Reduce 3 150)
    (217, Token (INFIXL _)) -> Just (Reduce 3 150)
    (217, Token (INFIXR _)) -> Just (Reduce 3 150)
    (217, Token (INFIX _)) -> Just (Reduce 3 150)
    (217, Token (BACKQUOTE _)) -> Just (Reduce 3 150)
    (218, Token (RPAREN _)) -> Just (Shift 217)
    (219, Token (RPAREN _)) -> Just (Reduce 3 103)
    (220, Token (RPAREN _)) -> Just (Reduce 1 102)
    (220, Token (COMMA _)) -> Just (Shift 161)
    (221, Token (WHERE _)) -> Just (Reduce 3 92)
    (222, Token (RPAREN _)) -> Just (Shift 221)
    (223, Token (RPAREN _)) -> Just (Reduce 3 95)
    (224, Token (RPAREN _)) -> Just (Reduce 1 94)
    (224, Token (COMMA _)) -> Just (Shift 160)
    (225, Token (RPAREN _)) -> Just (Reduce 3 98)
    (225, Token (COMMA _)) -> Just (Reduce 3 98)
    (226, Token (RPAREN _)) -> Just (Reduce 4 99)
    (226, Token (COMMA _)) -> Just (Reduce 4 99)
    (227, Token (RPAREN _)) -> Just (Reduce 4 100)
    (227, Token (COMMA _)) -> Just (Reduce 4 100)
    (228, Token (RPAREN _)) -> Just (Shift 226)
    (229, Token (RPAREN _)) -> Just (Reduce 2 101)
    (229, Token (COMMA _)) -> Just (Reduce 2 101)
    (230, Token (RPAREN _)) -> Just (Reduce 1 96)
    (230, Token (COMMA _)) -> Just (Reduce 1 96)
    (231, Token (LPAREN _)) -> Just (Shift 162)
    (231, Token (RPAREN _)) -> Just (Reduce 1 97)
    (231, Token (COMMA _)) -> Just (Reduce 1 97)
    (232, Token (RPAREN _)) -> Just (Shift 227)
    (233, Token (RPAREN _)) -> Just (Reduce 1 104)
    (233, Token (COMMA _)) -> Just (Reduce 1 104)
    (234, Token (RPAREN _)) -> Just (Reduce 1 105)
    (234, Token (COMMA _)) -> Just (Reduce 1 105)
    (235, Token (WHERE _)) -> Just (Reduce 3 116)
    (235, Token (QCONID _)) -> Just (Reduce 3 116)
    (235, Token (LBRACE _)) -> Just (Reduce 3 116)
    (235, Token (RBRACE _)) -> Just (Reduce 3 116)
    (235, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (235, Token (EQUAL _)) -> Just (Reduce 3 116)
    (235, Token (DERIVING _)) -> Just (Reduce 3 116)
    (235, Token (LPAREN _)) -> Just (Reduce 3 116)
    (235, Token (RPAREN _)) -> Just (Reduce 3 116)
    (235, Token (DARROW _)) -> Just (Reduce 3 116)
    (235, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (235, Token (INTEGER _)) -> Just (Reduce 3 116)
    (235, Token (TODO_FUNLHS _)) -> Just (Reduce 3 116)
    (235, Token (QVARID _)) -> Just (Reduce 3 116)
    (235, Token (COMMA _)) -> Just (Reduce 3 116)
    (235, Token (ARROW _)) -> Just (Reduce 3 116)
    (235, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (235, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (235, Token (EXCL _)) -> Just (Reduce 3 116)
    (235, Token (PIPE _)) -> Just (Reduce 3 116)
    (235, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (235, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (235, Token (INFIXL _)) -> Just (Reduce 3 116)
    (235, Token (INFIXR _)) -> Just (Reduce 3 116)
    (235, Token (INFIX _)) -> Just (Reduce 3 116)
    (235, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (236, Token (WHERE _)) -> Just (Reduce 3 114)
    (236, Token (QCONID _)) -> Just (Reduce 3 114)
    (236, Token (LBRACE _)) -> Just (Reduce 3 114)
    (236, Token (RBRACE _)) -> Just (Reduce 3 114)
    (236, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (236, Token (EQUAL _)) -> Just (Reduce 3 114)
    (236, Token (DERIVING _)) -> Just (Reduce 3 114)
    (236, Token (LPAREN _)) -> Just (Reduce 3 114)
    (236, Token (RPAREN _)) -> Just (Reduce 3 114)
    (236, Token (DARROW _)) -> Just (Reduce 3 114)
    (236, Token (COLON_COLON _)) -> Just (Reduce 3 114)
    (236, Token (INTEGER _)) -> Just (Reduce 3 114)
    (236, Token (TODO_FUNLHS _)) -> Just (Reduce 3 114)
    (236, Token (QVARID _)) -> Just (Reduce 3 114)
    (236, Token (COMMA _)) -> Just (Reduce 3 114)
    (236, Token (ARROW _)) -> Just (Reduce 3 114)
    (236, Token (LBRACKET _)) -> Just (Reduce 3 114)
    (236, Token (RBRACKET _)) -> Just (Reduce 3 114)
    (236, Token (EXCL _)) -> Just (Reduce 3 114)
    (236, Token (PIPE _)) -> Just (Reduce 3 114)
    (236, Token (QVARSYM _)) -> Just (Reduce 3 114)
    (236, Token (QCONSYM _)) -> Just (Reduce 3 114)
    (236, Token (INFIXL _)) -> Just (Reduce 3 114)
    (236, Token (INFIXR _)) -> Just (Reduce 3 114)
    (236, Token (INFIX _)) -> Just (Reduce 3 114)
    (236, Token (BACKQUOTE _)) -> Just (Reduce 3 114)
    (237, Token (WHERE _)) -> Just (Reduce 3 115)
    (237, Token (QCONID _)) -> Just (Reduce 3 115)
    (237, Token (LBRACE _)) -> Just (Reduce 3 115)
    (237, Token (RBRACE _)) -> Just (Reduce 3 115)
    (237, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (237, Token (EQUAL _)) -> Just (Reduce 3 115)
    (237, Token (DERIVING _)) -> Just (Reduce 3 115)
    (237, Token (LPAREN _)) -> Just (Reduce 3 115)
    (237, Token (RPAREN _)) -> Just (Reduce 3 115)
    (237, Token (DARROW _)) -> Just (Reduce 3 115)
    (237, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (237, Token (INTEGER _)) -> Just (Reduce 3 115)
    (237, Token (TODO_FUNLHS _)) -> Just (Reduce 3 115)
    (237, Token (QVARID _)) -> Just (Reduce 3 115)
    (237, Token (COMMA _)) -> Just (Reduce 3 115)
    (237, Token (ARROW _)) -> Just (Reduce 3 115)
    (237, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (237, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (237, Token (EXCL _)) -> Just (Reduce 3 115)
    (237, Token (PIPE _)) -> Just (Reduce 3 115)
    (237, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (237, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (237, Token (INFIXL _)) -> Just (Reduce 3 115)
    (237, Token (INFIXR _)) -> Just (Reduce 3 115)
    (237, Token (INFIX _)) -> Just (Reduce 3 115)
    (237, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (238, Token (RPAREN _)) -> Just (Shift 235)
    (238, Token (COMMA _)) -> Just (Shift 96)
    (239, Token (RBRACKET _)) -> Just (Shift 237)
    (240, Token (WHERE _)) -> Just (Reduce 1 113)
    (240, Token (QCONID _)) -> Just (Reduce 1 113)
    (240, Token (LBRACE _)) -> Just (Reduce 1 113)
    (240, Token (RBRACE _)) -> Just (Reduce 1 113)
    (240, Token (SEMICOLON _)) -> Just (Reduce 1 113)
    (240, Token (EQUAL _)) -> Just (Reduce 1 113)
    (240, Token (DERIVING _)) -> Just (Reduce 1 113)
    (240, Token (LPAREN _)) -> Just (Reduce 1 113)
    (240, Token (RPAREN _)) -> Just (Reduce 1 113)
    (240, Token (DARROW _)) -> Just (Reduce 1 113)
    (240, Token (COLON_COLON _)) -> Just (Reduce 1 113)
    (240, Token (INTEGER _)) -> Just (Reduce 1 113)
    (240, Token (TODO_FUNLHS _)) -> Just (Reduce 1 113)
    (240, Token (QVARID _)) -> Just (Reduce 1 113)
    (240, Token (COMMA _)) -> Just (Reduce 1 113)
    (240, Token (ARROW _)) -> Just (Reduce 1 113)
    (240, Token (LBRACKET _)) -> Just (Reduce 1 113)
    (240, Token (RBRACKET _)) -> Just (Reduce 1 113)
    (240, Token (EXCL _)) -> Just (Reduce 1 113)
    (240, Token (PIPE _)) -> Just (Reduce 1 113)
    (240, Token (QVARSYM _)) -> Just (Reduce 1 113)
    (240, Token (QCONSYM _)) -> Just (Reduce 1 113)
    (240, Token (INFIXL _)) -> Just (Reduce 1 113)
    (240, Token (INFIXR _)) -> Just (Reduce 1 113)
    (240, Token (INFIX _)) -> Just (Reduce 1 113)
    (240, Token (BACKQUOTE _)) -> Just (Reduce 1 113)
    (241, Token (WHERE _)) -> Just (Reduce 2 117)
    (241, Token (QCONID _)) -> Just (Reduce 2 117)
    (241, Token (LBRACE _)) -> Just (Reduce 2 117)
    (241, Token (RBRACE _)) -> Just (Reduce 2 117)
    (241, Token (SEMICOLON _)) -> Just (Reduce 2 117)
    (241, Token (EQUAL _)) -> Just (Reduce 2 117)
    (241, Token (DERIVING _)) -> Just (Reduce 2 117)
    (241, Token (LPAREN _)) -> Just (Reduce 2 117)
    (241, Token (RPAREN _)) -> Just (Reduce 2 117)
    (241, Token (DARROW _)) -> Just (Reduce 2 117)
    (241, Token (COLON_COLON _)) -> Just (Reduce 2 117)
    (241, Token (INTEGER _)) -> Just (Reduce 2 117)
    (241, Token (TODO_FUNLHS _)) -> Just (Reduce 2 117)
    (241, Token (QVARID _)) -> Just (Reduce 2 117)
    (241, Token (COMMA _)) -> Just (Reduce 2 117)
    (241, Token (ARROW _)) -> Just (Reduce 2 117)
    (241, Token (LBRACKET _)) -> Just (Reduce 2 117)
    (241, Token (RBRACKET _)) -> Just (Reduce 2 117)
    (241, Token (EXCL _)) -> Just (Reduce 2 117)
    (241, Token (PIPE _)) -> Just (Reduce 2 117)
    (241, Token (QVARSYM _)) -> Just (Reduce 2 117)
    (241, Token (QCONSYM _)) -> Just (Reduce 2 117)
    (241, Token (INFIXL _)) -> Just (Reduce 2 117)
    (241, Token (INFIXR _)) -> Just (Reduce 2 117)
    (241, Token (INFIX _)) -> Just (Reduce 2 117)
    (241, Token (BACKQUOTE _)) -> Just (Reduce 2 117)
    (242, Token (WHERE _)) -> Just (Reduce 1 112)
    (242, Token (QCONID _)) -> Just (Reduce 1 112)
    (242, Token (LBRACE _)) -> Just (Reduce 1 112)
    (242, Token (RBRACE _)) -> Just (Reduce 1 112)
    (242, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (242, Token (EQUAL _)) -> Just (Reduce 1 112)
    (242, Token (DERIVING _)) -> Just (Reduce 1 112)
    (242, Token (LPAREN _)) -> Just (Reduce 1 112)
    (242, Token (RPAREN _)) -> Just (Reduce 1 112)
    (242, Token (DARROW _)) -> Just (Reduce 1 112)
    (242, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (242, Token (INTEGER _)) -> Just (Reduce 1 112)
    (242, Token (TODO_FUNLHS _)) -> Just (Reduce 1 112)
    (242, Token (QVARID _)) -> Just (Reduce 1 112)
    (242, Token (COMMA _)) -> Just (Reduce 1 112)
    (242, Token (ARROW _)) -> Just (Reduce 1 112)
    (242, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (242, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (242, Token (EXCL _)) -> Just (Reduce 1 112)
    (242, Token (PIPE _)) -> Just (Reduce 1 112)
    (242, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (242, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (242, Token (INFIXL _)) -> Just (Reduce 1 112)
    (242, Token (INFIXR _)) -> Just (Reduce 1 112)
    (242, Token (INFIX _)) -> Just (Reduce 1 112)
    (242, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (243, Token (RPAREN _)) -> Just (Shift 236)
    (244, Token (WHERE _)) -> Just (Reduce 2 121)
    (244, Token (QCONID _)) -> Just (Reduce 2 121)
    (244, Token (LBRACE _)) -> Just (Reduce 2 121)
    (244, Token (RBRACE _)) -> Just (Reduce 2 121)
    (244, Token (SEMICOLON _)) -> Just (Reduce 2 121)
    (244, Token (EQUAL _)) -> Just (Reduce 2 121)
    (244, Token (DERIVING _)) -> Just (Reduce 2 121)
    (244, Token (LPAREN _)) -> Just (Reduce 2 121)
    (244, Token (RPAREN _)) -> Just (Reduce 2 121)
    (244, Token (DARROW _)) -> Just (Reduce 2 121)
    (244, Token (COLON_COLON _)) -> Just (Reduce 2 121)
    (244, Token (INTEGER _)) -> Just (Reduce 2 121)
    (244, Token (TODO_FUNLHS _)) -> Just (Reduce 2 121)
    (244, Token (QVARID _)) -> Just (Reduce 2 121)
    (244, Token (COMMA _)) -> Just (Reduce 2 121)
    (244, Token (ARROW _)) -> Just (Reduce 2 121)
    (244, Token (LBRACKET _)) -> Just (Reduce 2 121)
    (244, Token (RBRACKET _)) -> Just (Reduce 2 121)
    (244, Token (EXCL _)) -> Just (Reduce 2 121)
    (244, Token (PIPE _)) -> Just (Reduce 2 121)
    (244, Token (QVARSYM _)) -> Just (Reduce 2 121)
    (244, Token (QCONSYM _)) -> Just (Reduce 2 121)
    (244, Token (INFIXL _)) -> Just (Reduce 2 121)
    (244, Token (INFIXR _)) -> Just (Reduce 2 121)
    (244, Token (INFIX _)) -> Just (Reduce 2 121)
    (244, Token (BACKQUOTE _)) -> Just (Reduce 2 121)
    (245, Token (WHERE _)) -> Just (Reduce 3 123)
    (245, Token (QCONID _)) -> Just (Reduce 3 123)
    (245, Token (LBRACE _)) -> Just (Reduce 3 123)
    (245, Token (RBRACE _)) -> Just (Reduce 3 123)
    (245, Token (SEMICOLON _)) -> Just (Reduce 3 123)
    (245, Token (EQUAL _)) -> Just (Reduce 3 123)
    (245, Token (DERIVING _)) -> Just (Reduce 3 123)
    (245, Token (LPAREN _)) -> Just (Reduce 3 123)
    (245, Token (RPAREN _)) -> Just (Reduce 3 123)
    (245, Token (DARROW _)) -> Just (Reduce 3 123)
    (245, Token (COLON_COLON _)) -> Just (Reduce 3 123)
    (245, Token (INTEGER _)) -> Just (Reduce 3 123)
    (245, Token (TODO_FUNLHS _)) -> Just (Reduce 3 123)
    (245, Token (QVARID _)) -> Just (Reduce 3 123)
    (245, Token (COMMA _)) -> Just (Reduce 3 123)
    (245, Token (ARROW _)) -> Just (Reduce 3 123)
    (245, Token (LBRACKET _)) -> Just (Reduce 3 123)
    (245, Token (RBRACKET _)) -> Just (Reduce 3 123)
    (245, Token (EXCL _)) -> Just (Reduce 3 123)
    (245, Token (PIPE _)) -> Just (Reduce 3 123)
    (245, Token (QVARSYM _)) -> Just (Reduce 3 123)
    (245, Token (QCONSYM _)) -> Just (Reduce 3 123)
    (245, Token (INFIXL _)) -> Just (Reduce 3 123)
    (245, Token (INFIXR _)) -> Just (Reduce 3 123)
    (245, Token (INFIX _)) -> Just (Reduce 3 123)
    (245, Token (BACKQUOTE _)) -> Just (Reduce 3 123)
    (246, Token (WHERE _)) -> Just (Reduce 3 124)
    (246, Token (QCONID _)) -> Just (Reduce 3 124)
    (246, Token (LBRACE _)) -> Just (Reduce 3 124)
    (246, Token (RBRACE _)) -> Just (Reduce 3 124)
    (246, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (246, Token (EQUAL _)) -> Just (Reduce 3 124)
    (246, Token (DERIVING _)) -> Just (Reduce 3 124)
    (246, Token (LPAREN _)) -> Just (Reduce 3 124)
    (246, Token (RPAREN _)) -> Just (Reduce 3 124)
    (246, Token (DARROW _)) -> Just (Reduce 3 124)
    (246, Token (COLON_COLON _)) -> Just (Reduce 3 124)
    (246, Token (INTEGER _)) -> Just (Reduce 3 124)
    (246, Token (TODO_FUNLHS _)) -> Just (Reduce 3 124)
    (246, Token (QVARID _)) -> Just (Reduce 3 124)
    (246, Token (COMMA _)) -> Just (Reduce 3 124)
    (246, Token (ARROW _)) -> Just (Reduce 3 124)
    (246, Token (LBRACKET _)) -> Just (Reduce 3 124)
    (246, Token (RBRACKET _)) -> Just (Reduce 3 124)
    (246, Token (EXCL _)) -> Just (Reduce 3 124)
    (246, Token (PIPE _)) -> Just (Reduce 3 124)
    (246, Token (QVARSYM _)) -> Just (Reduce 3 124)
    (246, Token (QCONSYM _)) -> Just (Reduce 3 124)
    (246, Token (INFIXL _)) -> Just (Reduce 3 124)
    (246, Token (INFIXR _)) -> Just (Reduce 3 124)
    (246, Token (INFIX _)) -> Just (Reduce 3 124)
    (246, Token (BACKQUOTE _)) -> Just (Reduce 3 124)
    (247, Token (RPAREN _)) -> Just (Shift 245)
    (248, Token (WHERE _)) -> Just (Reduce 2 122)
    (248, Token (QCONID _)) -> Just (Reduce 2 122)
    (248, Token (LBRACE _)) -> Just (Reduce 2 122)
    (248, Token (RBRACE _)) -> Just (Reduce 2 122)
    (248, Token (SEMICOLON _)) -> Just (Reduce 2 122)
    (248, Token (EQUAL _)) -> Just (Reduce 2 122)
    (248, Token (DERIVING _)) -> Just (Reduce 2 122)
    (248, Token (LPAREN _)) -> Just (Reduce 2 122)
    (248, Token (RPAREN _)) -> Just (Reduce 2 122)
    (248, Token (DARROW _)) -> Just (Reduce 2 122)
    (248, Token (COLON_COLON _)) -> Just (Reduce 2 122)
    (248, Token (INTEGER _)) -> Just (Reduce 2 122)
    (248, Token (TODO_FUNLHS _)) -> Just (Reduce 2 122)
    (248, Token (QVARID _)) -> Just (Reduce 2 122)
    (248, Token (COMMA _)) -> Just (Reduce 2 122)
    (248, Token (ARROW _)) -> Just (Reduce 2 122)
    (248, Token (LBRACKET _)) -> Just (Reduce 2 122)
    (248, Token (RBRACKET _)) -> Just (Reduce 2 122)
    (248, Token (EXCL _)) -> Just (Reduce 2 122)
    (248, Token (PIPE _)) -> Just (Reduce 2 122)
    (248, Token (QVARSYM _)) -> Just (Reduce 2 122)
    (248, Token (QCONSYM _)) -> Just (Reduce 2 122)
    (248, Token (INFIXL _)) -> Just (Reduce 2 122)
    (248, Token (INFIXR _)) -> Just (Reduce 2 122)
    (248, Token (INFIX _)) -> Just (Reduce 2 122)
    (248, Token (BACKQUOTE _)) -> Just (Reduce 2 122)
    (249, Token (WHERE _)) -> Just (Reduce 1 120)
    (249, Token (QCONID _)) -> Just (Reduce 1 120)
    (249, Token (LBRACE _)) -> Just (Reduce 1 120)
    (249, Token (RBRACE _)) -> Just (Reduce 1 120)
    (249, Token (SEMICOLON _)) -> Just (Reduce 1 120)
    (249, Token (EQUAL _)) -> Just (Reduce 1 120)
    (249, Token (DERIVING _)) -> Just (Reduce 1 120)
    (249, Token (LPAREN _)) -> Just (Reduce 1 120)
    (249, Token (RPAREN _)) -> Just (Reduce 1 120)
    (249, Token (DARROW _)) -> Just (Reduce 1 120)
    (249, Token (COLON_COLON _)) -> Just (Reduce 1 120)
    (249, Token (INTEGER _)) -> Just (Reduce 1 120)
    (249, Token (TODO_FUNLHS _)) -> Just (Reduce 1 120)
    (249, Token (QVARID _)) -> Just (Reduce 1 120)
    (249, Token (COMMA _)) -> Just (Reduce 1 120)
    (249, Token (ARROW _)) -> Just (Reduce 1 120)
    (249, Token (LBRACKET _)) -> Just (Reduce 1 120)
    (249, Token (RBRACKET _)) -> Just (Reduce 1 120)
    (249, Token (EXCL _)) -> Just (Reduce 1 120)
    (249, Token (PIPE _)) -> Just (Reduce 1 120)
    (249, Token (QVARSYM _)) -> Just (Reduce 1 120)
    (249, Token (QCONSYM _)) -> Just (Reduce 1 120)
    (249, Token (INFIXL _)) -> Just (Reduce 1 120)
    (249, Token (INFIXR _)) -> Just (Reduce 1 120)
    (249, Token (INFIX _)) -> Just (Reduce 1 120)
    (249, Token (BACKQUOTE _)) -> Just (Reduce 1 120)
    (250, Token (QCONID _)) -> Just (Reduce 1 120)
    (250, Token (LBRACE _)) -> Just (Shift 164)
    (250, Token (RBRACE _)) -> Just (Reduce 1 120)
    (250, Token (SEMICOLON _)) -> Just (Reduce 1 120)
    (250, Token (DERIVING _)) -> Just (Reduce 1 120)
    (250, Token (LPAREN _)) -> Just (Reduce 1 120)
    (250, Token (RPAREN _)) -> Just (Reduce 1 120)
    (250, Token (QVARID _)) -> Just (Reduce 1 120)
    (250, Token (COMMA _)) -> Just (Reduce 1 120)
    (250, Token (ARROW _)) -> Just (Reduce 1 120)
    (250, Token (LBRACKET _)) -> Just (Reduce 1 120)
    (250, Token (RBRACKET _)) -> Just (Reduce 1 120)
    (250, Token (EXCL _)) -> Just (Reduce 1 120)
    (250, Token (PIPE _)) -> Just (Reduce 1 120)
    (250, Token (QCONSYM _)) -> Just (Reduce 1 120)
    (250, Token (BACKQUOTE _)) -> Just (Reduce 1 120)
    (251, Token (RPAREN _)) -> Just (Shift 246)
    (252, Token (RPAREN _)) -> Just (Reduce 3 118)
    (252, Token (COMMA _)) -> Just (Shift 96)
    (253, Token (RPAREN _)) -> Just (Reduce 3 119)
    (254, Token (RPAREN _)) -> Just (Reduce 1 125)
    (254, Token (COMMA _)) -> Just (Shift 254)
    (255, Token (RPAREN _)) -> Just (Reduce 2 126)
    (256, Token (RBRACE _)) -> Just (Reduce 3 130)
    (256, Token (SEMICOLON _)) -> Just (Reduce 3 130)
    (256, Token (DERIVING _)) -> Just (Reduce 3 130)
    (257, Token (RBRACE _)) -> Just (Reduce 1 129)
    (257, Token (SEMICOLON _)) -> Just (Reduce 1 129)
    (257, Token (DERIVING _)) -> Just (Reduce 1 129)
    (257, Token (PIPE _)) -> Just (Shift 87)
    (258, Token (RBRACE _)) -> Just (Reduce 3 133)
    (258, Token (SEMICOLON _)) -> Just (Reduce 3 133)
    (258, Token (DERIVING _)) -> Just (Reduce 3 133)
    (258, Token (PIPE _)) -> Just (Reduce 3 133)
    (259, Token (RBRACE _)) -> Just (Reduce 4 134)
    (259, Token (SEMICOLON _)) -> Just (Reduce 4 134)
    (259, Token (DERIVING _)) -> Just (Reduce 4 134)
    (259, Token (PIPE _)) -> Just (Reduce 4 134)
    (260, Token (RBRACE _)) -> Just (Shift 259)
    (261, Token (BACKQUOTE _)) -> Just (Shift 265)
    (262, Token (QCONID _)) -> Just (Reduce 1 156)
    (262, Token (RBRACE _)) -> Just (Reduce 1 156)
    (262, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (262, Token (LPAREN _)) -> Just (Reduce 1 156)
    (262, Token (RPAREN _)) -> Just (Reduce 1 156)
    (262, Token (QVARID _)) -> Just (Reduce 1 156)
    (262, Token (COMMA _)) -> Just (Reduce 1 156)
    (262, Token (ARROW _)) -> Just (Reduce 1 156)
    (262, Token (LBRACKET _)) -> Just (Reduce 1 156)
    (262, Token (RBRACKET _)) -> Just (Reduce 1 156)
    (262, Token (EXCL _)) -> Just (Reduce 1 156)
    (262, Token (QCONSYM _)) -> Just (Reduce 1 156)
    (263, Token (QCONID _)) -> Just (Shift 261)
    (264, Token (QCONID _)) -> Just (Shift 261)
    (264, Token (QVARID _)) -> Just (Shift 272)
    (265, Token (QCONID _)) -> Just (Reduce 3 157)
    (265, Token (RBRACE _)) -> Just (Reduce 3 157)
    (265, Token (SEMICOLON _)) -> Just (Reduce 3 157)
    (265, Token (LPAREN _)) -> Just (Reduce 3 157)
    (265, Token (RPAREN _)) -> Just (Reduce 3 157)
    (265, Token (QVARID _)) -> Just (Reduce 3 157)
    (265, Token (COMMA _)) -> Just (Reduce 3 157)
    (265, Token (ARROW _)) -> Just (Reduce 3 157)
    (265, Token (LBRACKET _)) -> Just (Reduce 3 157)
    (265, Token (RBRACKET _)) -> Just (Reduce 3 157)
    (265, Token (EXCL _)) -> Just (Reduce 3 157)
    (265, Token (QCONSYM _)) -> Just (Reduce 3 157)
    (266, Token (RBRACE _)) -> Just (Reduce 3 136)
    (267, Token (RBRACE _)) -> Just (Reduce 1 135)
    (267, Token (COMMA _)) -> Just (Shift 165)
    (268, Token (RBRACE _)) -> Just (Reduce 3 137)
    (268, Token (COMMA _)) -> Just (Reduce 3 137)
    (269, Token (COLON_COLON _)) -> Just (Shift 92)
    (270, Token (RBRACE _)) -> Just (Reduce 1 159)
    (270, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (270, Token (COMMA _)) -> Just (Reduce 1 159)
    (271, Token (RBRACE _)) -> Just (Reduce 1 158)
    (271, Token (SEMICOLON _)) -> Just (Reduce 1 158)
    (271, Token (COMMA _)) -> Just (Reduce 1 158)
    (272, Token (BACKQUOTE _)) -> Just (Shift 274)
    (273, Token (RBRACE _)) -> Just (Reduce 1 154)
    (273, Token (SEMICOLON _)) -> Just (Reduce 1 154)
    (273, Token (COMMA _)) -> Just (Reduce 1 154)
    (274, Token (RBRACE _)) -> Just (Reduce 3 155)
    (274, Token (SEMICOLON _)) -> Just (Reduce 3 155)
    (274, Token (COMMA _)) -> Just (Reduce 3 155)
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
production 33 = 20
production 34 = 21
production 35 = 21
production 36 = 19
production 37 = 19
production 38 = 19
production 39 = 15
production 40 = 15
production 41 = 26
production 42 = 27
production 43 = 27
production 44 = 28
production 45 = 28
production 46 = 28
production 47 = 16
production 48 = 16
production 49 = 29
production 50 = 30
production 51 = 30
production 52 = 31
production 53 = 31
production 54 = 31
production 55 = 22
production 56 = 22
production 57 = 22
production 58 = 22
production 59 = 34
production 60 = 34
production 61 = 23
production 62 = 24
production 63 = 24
production 64 = 36
production 65 = 37
production 66 = 38
production 67 = 39
production 68 = 39
production 69 = 40
production 70 = 40
production 71 = 41
production 72 = 42
production 73 = 43
production 74 = 6
production 75 = 6
production 76 = 7
production 77 = 7
production 78 = 8
production 79 = 8
production 80 = 44
production 81 = 44
production 82 = 45
production 83 = 45
production 84 = 45
production 85 = 46
production 86 = 46
production 87 = 46
production 88 = 46
production 89 = 46
production 90 = 2
production 91 = 2
production 92 = 49
production 93 = 50
production 94 = 50
production 95 = 50
production 96 = 51
production 97 = 51
production 98 = 51
production 99 = 51
production 100 = 51
production 101 = 51
production 102 = 48
production 103 = 48
production 104 = 52
production 105 = 52
production 106 = 17
production 107 = 17
production 108 = 10
production 109 = 10
production 110 = 9
production 111 = 9
production 112 = 53
production 113 = 53
production 114 = 53
production 115 = 53
production 116 = 53
production 117 = 53
production 118 = 55
production 119 = 55
production 120 = 54
production 121 = 54
production 122 = 54
production 123 = 54
production 124 = 54
production 125 = 56
production 126 = 56
production 127 = 11
production 128 = 11
production 129 = 57
production 130 = 57
production 131 = 58
production 132 = 58
production 133 = 58
production 134 = 58
production 135 = 60
production 136 = 60
production 137 = 61
production 138 = 62
production 139 = 62
production 140 = 62
production 141 = 14
production 142 = 14
production 143 = 35
production 144 = 35
production 145 = 32
production 146 = 32
production 147 = 25
production 148 = 25
production 149 = 47
production 150 = 47
production 151 = 33
production 152 = 33
production 153 = 33
production 154 = 64
production 155 = 64
production 156 = 59
production 157 = 59
production 158 = 63
production 159 = 63
production 160 = 12
production 161 = 13
production 162 = 13

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 49) -> Just 12
    (8, 1) -> Just 51
    (9, 1) -> Just 84
    (10, 1) -> Just 229
    (13, 4) -> Just 15
    (13, 5) -> Just 18
    (13, 19) -> Just 82
    (13, 22) -> Just 126
    (13, 23) -> Just 127
    (13, 25) -> Just 130
    (13, 32) -> Just 140
    (13, 33) -> Just 141
    (16, 4) -> Just 17
    (16, 5) -> Just 18
    (16, 19) -> Just 82
    (16, 22) -> Just 126
    (16, 23) -> Just 127
    (16, 25) -> Just 130
    (16, 32) -> Just 140
    (16, 33) -> Just 141
    (19, 6) -> Just 8
    (20, 9) -> Just 54
    (20, 43) -> Just 240
    (20, 47) -> Just 249
    (20, 53) -> Just 102
    (20, 54) -> Just 242
    (21, 9) -> Just 101
    (21, 10) -> Just 63
    (21, 43) -> Just 240
    (21, 47) -> Just 249
    (21, 53) -> Just 102
    (21, 54) -> Just 242
    (22, 9) -> Just 55
    (22, 43) -> Just 240
    (22, 47) -> Just 249
    (22, 53) -> Just 102
    (22, 54) -> Just 242
    (23, 12) -> Just 66
    (24, 12) -> Just 67
    (25, 12) -> Just 68
    (26, 12) -> Just 69
    (27, 12) -> Just 108
    (27, 13) -> Just 70
    (28, 12) -> Just 108
    (28, 13) -> Just 71
    (29, 12) -> Just 108
    (29, 13) -> Just 72
    (30, 12) -> Just 108
    (30, 13) -> Just 73
    (31, 9) -> Just 101
    (31, 10) -> Just 121
    (31, 17) -> Just 80
    (31, 43) -> Just 240
    (31, 47) -> Just 249
    (31, 53) -> Just 102
    (31, 54) -> Just 242
    (42, 9) -> Just 56
    (42, 43) -> Just 240
    (42, 47) -> Just 249
    (42, 53) -> Just 102
    (42, 54) -> Just 242
    (43, 9) -> Just 57
    (43, 43) -> Just 240
    (43, 47) -> Just 249
    (43, 53) -> Just 102
    (43, 54) -> Just 242
    (44, 9) -> Just 58
    (44, 43) -> Just 240
    (44, 47) -> Just 249
    (44, 53) -> Just 102
    (44, 54) -> Just 242
    (45, 9) -> Just 59
    (45, 43) -> Just 240
    (45, 47) -> Just 249
    (45, 53) -> Just 102
    (45, 54) -> Just 242
    (46, 9) -> Just 60
    (46, 43) -> Just 240
    (46, 47) -> Just 249
    (46, 53) -> Just 102
    (46, 54) -> Just 242
    (47, 9) -> Just 61
    (47, 43) -> Just 240
    (47, 47) -> Just 249
    (47, 53) -> Just 102
    (47, 54) -> Just 242
    (48, 9) -> Just 62
    (48, 43) -> Just 240
    (48, 47) -> Just 249
    (48, 53) -> Just 102
    (48, 54) -> Just 242
    (50, 18) -> Just 81
    (51, 7) -> Just 52
    (52, 8) -> Just 53
    (52, 44) -> Just 85
    (54, 43) -> Just 240
    (54, 47) -> Just 249
    (54, 53) -> Just 103
    (54, 54) -> Just 242
    (55, 11) -> Just 64
    (55, 43) -> Just 240
    (55, 47) -> Just 249
    (55, 53) -> Just 103
    (55, 54) -> Just 242
    (56, 11) -> Just 65
    (56, 43) -> Just 240
    (56, 47) -> Just 249
    (56, 53) -> Just 103
    (56, 54) -> Just 242
    (57, 14) -> Just 74
    (57, 43) -> Just 240
    (57, 47) -> Just 249
    (57, 53) -> Just 103
    (57, 54) -> Just 242
    (58, 15) -> Just 76
    (58, 43) -> Just 240
    (58, 47) -> Just 249
    (58, 53) -> Just 103
    (58, 54) -> Just 242
    (59, 16) -> Just 78
    (59, 43) -> Just 240
    (59, 47) -> Just 249
    (59, 53) -> Just 103
    (59, 54) -> Just 242
    (60, 14) -> Just 75
    (60, 43) -> Just 240
    (60, 47) -> Just 249
    (60, 53) -> Just 103
    (60, 54) -> Just 242
    (61, 15) -> Just 77
    (61, 43) -> Just 240
    (61, 47) -> Just 249
    (61, 53) -> Just 103
    (61, 54) -> Just 242
    (62, 16) -> Just 79
    (62, 43) -> Just 240
    (62, 47) -> Just 249
    (62, 53) -> Just 103
    (62, 54) -> Just 242
    (86, 9) -> Just 98
    (86, 43) -> Just 240
    (86, 47) -> Just 250
    (86, 53) -> Just 102
    (86, 54) -> Just 242
    (86, 57) -> Just 105
    (86, 58) -> Just 257
    (87, 9) -> Just 98
    (87, 43) -> Just 240
    (87, 47) -> Just 250
    (87, 53) -> Just 102
    (87, 54) -> Just 242
    (87, 57) -> Just 256
    (87, 58) -> Just 257
    (88, 9) -> Just 99
    (88, 43) -> Just 240
    (88, 47) -> Just 249
    (88, 53) -> Just 102
    (88, 54) -> Just 242
    (89, 9) -> Just 100
    (89, 10) -> Just 139
    (89, 43) -> Just 240
    (89, 47) -> Just 249
    (89, 53) -> Just 102
    (89, 54) -> Just 242
    (90, 9) -> Just 101
    (90, 10) -> Just 104
    (90, 43) -> Just 240
    (90, 47) -> Just 249
    (90, 53) -> Just 102
    (90, 54) -> Just 242
    (91, 9) -> Just 101
    (91, 10) -> Just 138
    (91, 43) -> Just 240
    (91, 47) -> Just 249
    (91, 53) -> Just 102
    (91, 54) -> Just 242
    (92, 9) -> Just 101
    (92, 10) -> Just 268
    (92, 43) -> Just 240
    (92, 47) -> Just 249
    (92, 53) -> Just 102
    (92, 54) -> Just 242
    (93, 9) -> Just 101
    (93, 10) -> Just 113
    (93, 43) -> Just 240
    (93, 47) -> Just 249
    (93, 53) -> Just 102
    (93, 54) -> Just 242
    (94, 9) -> Just 101
    (94, 10) -> Just 121
    (94, 17) -> Just 122
    (94, 43) -> Just 240
    (94, 47) -> Just 249
    (94, 53) -> Just 102
    (94, 54) -> Just 242
    (95, 9) -> Just 101
    (95, 10) -> Just 238
    (95, 43) -> Just 240
    (95, 47) -> Just 249
    (95, 53) -> Just 102
    (95, 54) -> Just 242
    (95, 55) -> Just 243
    (95, 56) -> Just 251
    (96, 9) -> Just 101
    (96, 10) -> Just 252
    (96, 43) -> Just 240
    (96, 47) -> Just 249
    (96, 53) -> Just 102
    (96, 54) -> Just 242
    (96, 55) -> Just 253
    (97, 9) -> Just 101
    (97, 10) -> Just 239
    (97, 43) -> Just 240
    (97, 47) -> Just 249
    (97, 53) -> Just 102
    (97, 54) -> Just 242
    (98, 43) -> Just 240
    (98, 47) -> Just 249
    (98, 53) -> Just 103
    (98, 54) -> Just 242
    (98, 59) -> Just 88
    (99, 43) -> Just 240
    (99, 47) -> Just 249
    (99, 53) -> Just 103
    (99, 54) -> Just 242
    (100, 43) -> Just 240
    (100, 47) -> Just 249
    (100, 53) -> Just 103
    (100, 54) -> Just 242
    (101, 43) -> Just 240
    (101, 47) -> Just 249
    (101, 53) -> Just 103
    (101, 54) -> Just 242
    (106, 12) -> Just 108
    (106, 13) -> Just 109
    (110, 25) -> Just 114
    (112, 47) -> Just 115
    (115, 43) -> Just 240
    (115, 47) -> Just 249
    (115, 53) -> Just 116
    (115, 54) -> Just 242
    (117, 26) -> Just 118
    (119, 29) -> Just 120
    (124, 19) -> Just 134
    (124, 21) -> Just 133
    (124, 22) -> Just 126
    (124, 23) -> Just 127
    (124, 25) -> Just 130
    (124, 32) -> Just 140
    (124, 33) -> Just 141
    (125, 19) -> Just 134
    (125, 21) -> Just 135
    (125, 22) -> Just 126
    (125, 23) -> Just 127
    (125, 25) -> Just 130
    (125, 32) -> Just 140
    (125, 33) -> Just 141
    (127, 24) -> Just 128
    (130, 24) -> Just 129
    (131, 20) -> Just 152
    (136, 22) -> Just 175
    (136, 23) -> Just 147
    (136, 25) -> Just 148
    (136, 27) -> Just 172
    (136, 28) -> Just 174
    (136, 32) -> Just 140
    (136, 33) -> Just 141
    (137, 22) -> Just 175
    (137, 23) -> Just 147
    (137, 25) -> Just 148
    (137, 27) -> Just 173
    (137, 28) -> Just 174
    (137, 32) -> Just 140
    (137, 33) -> Just 141
    (141, 34) -> Just 142
    (142, 35) -> Just 143
    (142, 59) -> Just 270
    (142, 63) -> Just 192
    (142, 64) -> Just 271
    (144, 23) -> Just 149
    (144, 25) -> Just 150
    (144, 30) -> Just 179
    (144, 31) -> Just 181
    (145, 23) -> Just 149
    (145, 25) -> Just 150
    (145, 30) -> Just 180
    (145, 31) -> Just 181
    (147, 24) -> Just 176
    (148, 24) -> Just 177
    (149, 24) -> Just 182
    (150, 24) -> Just 183
    (151, 25) -> Just 198
    (151, 36) -> Just 153
    (151, 37) -> Just 193
    (151, 38) -> Just 194
    (151, 39) -> Just 154
    (151, 40) -> Just 195
    (154, 25) -> Just 198
    (154, 40) -> Just 196
    (155, 25) -> Just 212
    (155, 45) -> Just 204
    (155, 46) -> Just 207
    (155, 47) -> Just 213
    (156, 25) -> Just 212
    (156, 45) -> Just 205
    (156, 46) -> Just 207
    (156, 47) -> Just 213
    (157, 25) -> Just 212
    (157, 45) -> Just 206
    (157, 46) -> Just 207
    (157, 47) -> Just 213
    (158, 25) -> Just 233
    (158, 47) -> Just 234
    (158, 48) -> Just 214
    (158, 52) -> Just 220
    (159, 25) -> Just 230
    (159, 47) -> Just 231
    (159, 50) -> Just 222
    (159, 51) -> Just 224
    (160, 25) -> Just 230
    (160, 47) -> Just 231
    (160, 50) -> Just 223
    (160, 51) -> Just 224
    (161, 25) -> Just 233
    (161, 47) -> Just 234
    (161, 48) -> Just 219
    (161, 52) -> Just 220
    (162, 25) -> Just 233
    (162, 47) -> Just 234
    (162, 48) -> Just 232
    (162, 52) -> Just 220
    (163, 25) -> Just 184
    (163, 32) -> Just 185
    (164, 25) -> Just 184
    (164, 32) -> Just 269
    (164, 60) -> Just 260
    (164, 61) -> Just 267
    (165, 25) -> Just 184
    (165, 32) -> Just 269
    (165, 60) -> Just 266
    (165, 61) -> Just 267
    (190, 35) -> Just 191
    (190, 59) -> Just 270
    (190, 63) -> Just 192
    (190, 64) -> Just 271
    (199, 43) -> Just 240
    (199, 47) -> Just 249
    (199, 53) -> Just 241
    (199, 54) -> Just 242
    (254, 56) -> Just 255
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
                  Token (TODO_FDECL semanticValue) ->
                    StackValue_TODO_FDECL semanticValue
                  Token (COLON_COLON semanticValue) ->
                    StackValue_COLON_COLON semanticValue
                  Token (INTEGER semanticValue) ->
                    StackValue_INTEGER semanticValue
                  Token (TODO_FUNLHS semanticValue) ->
                    StackValue_TODO_FUNLHS semanticValue
                  Token (TODO_INST semanticValue) ->
                    StackValue_TODO_INST semanticValue
                  Token (QVARID semanticValue) ->
                    StackValue_QVARID semanticValue
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
                      Monad.liftM StackValue_fdecl $ fdecl_implies_TODO_FDECL actions (case snd (pop !! 0) of { StackValue_TODO_FDECL value -> value; _ -> undefined })
                    33 ->
                      Monad.liftM StackValue_decls $ decls_implies_LBRACE_decl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_decl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    34 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl actions (case snd (pop !! 0) of { StackValue_decl value -> value; _ -> undefined })
                    35 ->
                      Monad.liftM StackValue_decl_seq $ decl_seq_implies_decl_SEMICOLON_decl_seq actions (case snd (pop !! 2) of { StackValue_decl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decl_seq value -> value; _ -> undefined })
                    36 ->
                      Monad.liftM StackValue_decl $ decl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    37 ->
                      Monad.liftM StackValue_decl $ decl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    38 ->
                      Monad.liftM StackValue_decl $ decl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    39 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies actions
                    40 ->
                      Monad.liftM StackValue_cdecls_opt $ cdecls_opt_implies_WHERE_cdecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecls value -> value; _ -> undefined })
                    41 ->
                      Monad.liftM StackValue_cdecls $ cdecls_implies_LBRACE_cdecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cdecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    42 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl actions (case snd (pop !! 0) of { StackValue_cdecl value -> value; _ -> undefined })
                    43 ->
                      Monad.liftM StackValue_cdecl_seq $ cdecl_seq_implies_cdecl_SEMICOLON_cdecl_seq actions (case snd (pop !! 2) of { StackValue_cdecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cdecl_seq value -> value; _ -> undefined })
                    44 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_gendecl actions (case snd (pop !! 0) of { StackValue_gendecl value -> value; _ -> undefined })
                    45 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    46 ->
                      Monad.liftM StackValue_cdecl $ cdecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    47 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies actions
                    48 ->
                      Monad.liftM StackValue_idecls_opt $ idecls_opt_implies_WHERE_idecls actions (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecls value -> value; _ -> undefined })
                    49 ->
                      Monad.liftM StackValue_idecls $ idecls_implies_LBRACE_idecl_seq_RBRACE actions (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_idecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    50 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl actions (case snd (pop !! 0) of { StackValue_idecl value -> value; _ -> undefined })
                    51 ->
                      Monad.liftM StackValue_idecl_seq $ idecl_seq_implies_idecl_SEMICOLON_idecl_seq actions (case snd (pop !! 2) of { StackValue_idecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_SEMICOLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_idecl_seq value -> value; _ -> undefined })
                    52 ->
                      Monad.liftM StackValue_idecl $ idecl_implies actions
                    53 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_funlhs_rhs actions (case snd (pop !! 1) of { StackValue_funlhs value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    54 ->
                      Monad.liftM StackValue_idecl $ idecl_implies_var_rhs actions (case snd (pop !! 1) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_rhs value -> value; _ -> undefined })
                    55 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies actions
                    56 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    57 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_vars_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    58 ->
                      Monad.liftM StackValue_gendecl $ gendecl_implies_fixity_integer_opt_ops actions (case snd (pop !! 2) of { StackValue_fixity value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_integer_opt value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    59 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    60 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    61 ->
                      Monad.liftM StackValue_funlhs $ funlhs_implies_TODO_FUNLHS actions (case snd (pop !! 0) of { StackValue_TODO_FUNLHS value -> value; _ -> undefined })
                    62 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    63 ->
                      Monad.liftM StackValue_rhs $ rhs_implies_EQUAL_exp_WHERE_decls actions (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_WHERE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    64 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    65 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    66 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    67 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    68 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    69 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    70 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    71 ->
                      Monad.liftM StackValue_inst $ inst_implies_TODO_INST actions (case snd (pop !! 0) of { StackValue_TODO_INST value -> value; _ -> undefined })
                    72 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    73 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    74 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    75 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    76 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    77 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    78 ->
                      Monad.liftM StackValue_impspec_opt $ impspec_opt_implies actions
                    79 ->
                      Monad.liftM StackValue_impspec_opt $ impspec_opt_implies_impspec actions (case snd (pop !! 0) of { StackValue_impspec value -> value; _ -> undefined })
                    80 ->
                      Monad.liftM StackValue_impspec $ impspec_implies_LPAREN_import_seq_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    81 ->
                      Monad.liftM StackValue_impspec $ impspec_implies_HIDING_LPAREN_import_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_HIDING value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_import_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    82 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies actions
                    83 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import' actions (case snd (pop !! 0) of { StackValue_import' value -> value; _ -> undefined })
                    84 ->
                      Monad.liftM StackValue_import_seq $ import_seq_implies_import'_COMMA_import_seq actions (case snd (pop !! 2) of { StackValue_import' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_import_seq value -> value; _ -> undefined })
                    85 ->
                      Monad.liftM StackValue_import' $ import'_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    86 ->
                      Monad.liftM StackValue_import' $ import'_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    87 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    88 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    89 ->
                      Monad.liftM StackValue_import' $ import'_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    90 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies actions
                    91 ->
                      Monad.liftM StackValue_exports_opt $ exports_opt_implies_exports actions (case snd (pop !! 0) of { StackValue_exports value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_exports $ exports_implies_LPAREN_export_seq_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_export_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies actions
                    94 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export actions (case snd (pop !! 0) of { StackValue_export value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_export_seq $ export_seq_implies_export_COMMA_export_seq actions (case snd (pop !! 2) of { StackValue_export value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_export_seq value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_export $ export_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_export $ export_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    98 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_RPAREN actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_DOT_DOT_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DOT_DOT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_export $ export_implies_con_LPAREN_cname_seq_RPAREN actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_cname_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    101 ->
                      Monad.liftM StackValue_export $ export_implies_MODULE_modid actions (case snd (pop !! 1) of { StackValue_MODULE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname actions (case snd (pop !! 0) of { StackValue_cname value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_cname_seq $ cname_seq_implies_cname_COMMA_cname_seq actions (case snd (pop !! 2) of { StackValue_cname value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_cname_seq value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_cname $ cname_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_cname $ cname_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    109 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_ARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    112 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    118 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    120 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_ARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_ARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    128 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    135 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_atypes $ atypes_implies actions
                    139 ->
                      Monad.liftM StackValue_atypes $ atypes_implies_atype_atypes actions (case snd (pop !! 1) of { StackValue_atype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atypes value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_atypes $ atypes_implies_EXCL_atype_atypes actions (case snd (pop !! 2) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_atype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atypes value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    146 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    162 ->
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
  , fdecl_implies_TODO_FDECL = \tODO_FDECL0 ->
      return $ Fdecl_implies_TODO_FDECL tODO_FDECL0
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

