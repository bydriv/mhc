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
type LARROW = Pos
type LBRACE = Pos
type LBRACKET = Pos
type LET = Pos
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
type RARROW = Pos
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

data Guards =
    Guards_implies_guard Guard
  | Guards_implies_guard_COMMA_guards Guard COMMA Guards
  deriving (Eq, Ord, Read, Show)

data Guard =
    Guard_implies_infixexp Infixexp
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
  | Aexp_implies_STRING STRING
  | Aexp_implies_LPAREN_exp_RPAREN LPAREN Exp RPAREN
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
  | StackValue_PIPE PIPE
  | StackValue_COLON_COLON COLON_COLON
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
  , gdrhs_implies_guards_EQUAL_exp :: Guards -> EQUAL -> Exp -> m Gdrhs
  , gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs :: Guards -> EQUAL -> Exp -> PIPE -> Gdrhs -> m Gdrhs
  , guards_implies_guard :: Guard -> m Guards
  , guards_implies_guard_COMMA_guards :: Guard -> COMMA -> Guards -> m Guards
  , guard_implies_infixexp :: Infixexp -> m Guard
  , exp_implies_infixexp :: Infixexp -> m Exp
  , infixexp_implies_lexp :: Lexp -> m Infixexp
  , lexp_implies_fexp :: Fexp -> m Lexp
  , fexp_implies_aexp :: Aexp -> m Fexp
  , fexp_implies_fexp_aexp :: Fexp -> Aexp -> m Fexp
  , fexp_implies_fexp_op_aexp :: Fexp -> Op -> Aexp -> m Fexp
  , aexp_implies_var :: Var -> m Aexp
  , aexp_implies_INTEGER :: INTEGER -> m Aexp
  , aexp_implies_STRING :: STRING -> m Aexp
  , aexp_implies_LPAREN_exp_RPAREN :: LPAREN -> Exp -> RPAREN -> m Aexp
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
    (11, Token (MODULE _)) -> Just (Reduce 1 184)
    (11, Token (WHERE _)) -> Just (Reduce 1 184)
    (11, Token (RBRACE _)) -> Just (Reduce 1 184)
    (11, Token (LPAREN _)) -> Just (Reduce 1 184)
    (11, Token (RPAREN _)) -> Just (Reduce 1 184)
    (11, Token (COMMA _)) -> Just (Reduce 1 184)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 184)
    (11, Token (HIDING _)) -> Just (Reduce 1 184)
    (11, Token (QCONID _)) -> Just (Reduce 1 184)
    (11, Token (EXPORT _)) -> Just (Reduce 1 184)
    (11, Token (AS _)) -> Just (Reduce 1 184)
    (11, Token (QVARID _)) -> Just (Reduce 1 184)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 184)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 184)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 67)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 149)
    (13, Token (TYPE _)) -> Just (Shift 107)
    (13, Token (DATA _)) -> Just (Shift 83)
    (13, Token (NEWTYPE _)) -> Just (Shift 105)
    (13, Token (CLASS _)) -> Just (Shift 75)
    (13, Token (INSTANCE _)) -> Just (Shift 77)
    (13, Token (DEFAULT _)) -> Just (Shift 155)
    (13, Token (FOREIGN _)) -> Just (Shift 156)
    (13, Token (INFIXL _)) -> Just (Shift 259)
    (13, Token (INFIXR _)) -> Just (Shift 260)
    (13, Token (INFIX _)) -> Just (Shift 261)
    (13, Token (EXPORT _)) -> Just (Shift 70)
    (13, Token (AS _)) -> Just (Shift 71)
    (13, Token (QVARID _)) -> Just (Shift 72)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 67)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 149)
    (16, Token (TYPE _)) -> Just (Shift 107)
    (16, Token (DATA _)) -> Just (Shift 83)
    (16, Token (NEWTYPE _)) -> Just (Shift 105)
    (16, Token (CLASS _)) -> Just (Shift 75)
    (16, Token (INSTANCE _)) -> Just (Shift 77)
    (16, Token (DEFAULT _)) -> Just (Shift 155)
    (16, Token (FOREIGN _)) -> Just (Shift 156)
    (16, Token (INFIXL _)) -> Just (Shift 259)
    (16, Token (INFIXR _)) -> Just (Shift 260)
    (16, Token (INFIX _)) -> Just (Shift 261)
    (16, Token (EXPORT _)) -> Just (Shift 70)
    (16, Token (AS _)) -> Just (Shift 71)
    (16, Token (QVARID _)) -> Just (Shift 72)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 68)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 118)
    (19, Token (EXPORT _)) -> Just (Shift 70)
    (19, Token (AS _)) -> Just (Shift 71)
    (19, Token (QVARID _)) -> Just (Shift 72)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 68)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 118)
    (22, Token (EXPORT _)) -> Just (Shift 70)
    (22, Token (AS _)) -> Just (Shift 71)
    (22, Token (QVARID _)) -> Just (Shift 72)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 68)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 118)
    (25, Token (EXPORT _)) -> Just (Shift 70)
    (25, Token (AS _)) -> Just (Shift 71)
    (25, Token (QVARID _)) -> Just (Shift 72)
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
    (34, Token (WHERE _)) -> Just (Reduce 1 151)
    (34, Token (RBRACE _)) -> Just (Reduce 1 151)
    (34, Token (LPAREN _)) -> Just (Shift 40)
    (34, Token (RPAREN _)) -> Just (Reduce 1 151)
    (34, Token (COMMA _)) -> Just (Reduce 1 151)
    (34, Token (SEMICOLON _)) -> Just (Reduce 1 151)
    (34, Token (EQUAL _)) -> Just (Reduce 1 151)
    (34, Token (PIPE _)) -> Just (Reduce 1 151)
    (34, Token (EXPORT _)) -> Just (Shift 70)
    (34, Token (AS _)) -> Just (Shift 71)
    (34, Token (QVARID _)) -> Just (Shift 72)
    (34, Token (STRING _)) -> Just (Shift 322)
    (34, Token (INTEGER _)) -> Just (Shift 323)
    (34, Token (QVARSYM _)) -> Just (Shift 330)
    (34, Token (QCONSYM _)) -> Just (Shift 298)
    (34, Token (BACKQUOTE _)) -> Just (Shift 299)
    (35, Token (LPAREN _)) -> Just (Shift 40)
    (35, Token (EXPORT _)) -> Just (Shift 70)
    (35, Token (AS _)) -> Just (Shift 71)
    (35, Token (QVARID _)) -> Just (Shift 72)
    (35, Token (STRING _)) -> Just (Shift 322)
    (35, Token (INTEGER _)) -> Just (Shift 323)
    (36, Token (LPAREN _)) -> Just (Shift 40)
    (36, Token (EXPORT _)) -> Just (Shift 70)
    (36, Token (AS _)) -> Just (Shift 71)
    (36, Token (QVARID _)) -> Just (Shift 72)
    (36, Token (STRING _)) -> Just (Shift 322)
    (36, Token (INTEGER _)) -> Just (Shift 323)
    (37, Token (LPAREN _)) -> Just (Shift 40)
    (37, Token (EXPORT _)) -> Just (Shift 70)
    (37, Token (AS _)) -> Just (Shift 71)
    (37, Token (QVARID _)) -> Just (Shift 72)
    (37, Token (STRING _)) -> Just (Shift 322)
    (37, Token (INTEGER _)) -> Just (Shift 323)
    (38, Token (LPAREN _)) -> Just (Shift 40)
    (38, Token (EXPORT _)) -> Just (Shift 70)
    (38, Token (AS _)) -> Just (Shift 71)
    (38, Token (QVARID _)) -> Just (Shift 72)
    (38, Token (STRING _)) -> Just (Shift 322)
    (38, Token (INTEGER _)) -> Just (Shift 323)
    (39, Token (LPAREN _)) -> Just (Shift 40)
    (39, Token (EXPORT _)) -> Just (Shift 70)
    (39, Token (AS _)) -> Just (Shift 71)
    (39, Token (QVARID _)) -> Just (Shift 72)
    (39, Token (STRING _)) -> Just (Shift 322)
    (39, Token (INTEGER _)) -> Just (Shift 323)
    (40, Token (LPAREN _)) -> Just (Shift 40)
    (40, Token (EXPORT _)) -> Just (Shift 70)
    (40, Token (AS _)) -> Just (Shift 71)
    (40, Token (QVARID _)) -> Just (Shift 72)
    (40, Token (STRING _)) -> Just (Shift 322)
    (40, Token (INTEGER _)) -> Just (Shift 323)
    (40, Token (QVARSYM _)) -> Just (Shift 73)
    (41, Token (RBRACE _)) -> Just (Reduce 0 85)
    (41, Token (LPAREN _)) -> Just (Shift 67)
    (41, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (41, Token (INFIXL _)) -> Just (Shift 259)
    (41, Token (INFIXR _)) -> Just (Shift 260)
    (41, Token (INFIX _)) -> Just (Shift 261)
    (41, Token (EXPORT _)) -> Just (Shift 70)
    (41, Token (AS _)) -> Just (Shift 71)
    (41, Token (QVARID _)) -> Just (Shift 72)
    (42, Token (RBRACE _)) -> Just (Reduce 0 85)
    (42, Token (LPAREN _)) -> Just (Shift 67)
    (42, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (42, Token (INFIXL _)) -> Just (Shift 259)
    (42, Token (INFIXR _)) -> Just (Shift 260)
    (42, Token (INFIX _)) -> Just (Shift 261)
    (42, Token (EXPORT _)) -> Just (Shift 70)
    (42, Token (AS _)) -> Just (Shift 71)
    (42, Token (QVARID _)) -> Just (Shift 72)
    (43, Token (RBRACE _)) -> Just (Reduce 0 85)
    (43, Token (LPAREN _)) -> Just (Shift 67)
    (43, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (43, Token (INFIXL _)) -> Just (Shift 259)
    (43, Token (INFIXR _)) -> Just (Shift 260)
    (43, Token (INFIX _)) -> Just (Shift 261)
    (43, Token (EXPORT _)) -> Just (Shift 70)
    (43, Token (AS _)) -> Just (Shift 71)
    (43, Token (QVARID _)) -> Just (Shift 72)
    (44, Token (RBRACE _)) -> Just (Reduce 0 85)
    (44, Token (LPAREN _)) -> Just (Shift 67)
    (44, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (44, Token (INFIXL _)) -> Just (Shift 259)
    (44, Token (INFIXR _)) -> Just (Shift 260)
    (44, Token (INFIX _)) -> Just (Shift 261)
    (44, Token (EXPORT _)) -> Just (Shift 70)
    (44, Token (AS _)) -> Just (Shift 71)
    (44, Token (QVARID _)) -> Just (Shift 72)
    (45, Token (LPAREN _)) -> Just (Shift 40)
    (45, Token (EXPORT _)) -> Just (Shift 70)
    (45, Token (AS _)) -> Just (Shift 71)
    (45, Token (QVARID _)) -> Just (Shift 72)
    (45, Token (STRING _)) -> Just (Shift 322)
    (45, Token (INTEGER _)) -> Just (Shift 323)
    (46, Token (LPAREN _)) -> Just (Shift 40)
    (46, Token (EXPORT _)) -> Just (Shift 70)
    (46, Token (AS _)) -> Just (Shift 71)
    (46, Token (QVARID _)) -> Just (Shift 72)
    (46, Token (STRING _)) -> Just (Shift 322)
    (46, Token (INTEGER _)) -> Just (Shift 323)
    (47, Token (LPAREN _)) -> Just (Shift 40)
    (47, Token (EXPORT _)) -> Just (Shift 70)
    (47, Token (AS _)) -> Just (Shift 71)
    (47, Token (QVARID _)) -> Just (Shift 72)
    (47, Token (STRING _)) -> Just (Shift 322)
    (47, Token (INTEGER _)) -> Just (Shift 323)
    (48, Token (LPAREN _)) -> Just (Shift 40)
    (48, Token (EXPORT _)) -> Just (Shift 70)
    (48, Token (AS _)) -> Just (Shift 71)
    (48, Token (QVARID _)) -> Just (Shift 72)
    (48, Token (STRING _)) -> Just (Shift 322)
    (48, Token (INTEGER _)) -> Just (Shift 323)
    (49, Token (LPAREN _)) -> Just (Shift 40)
    (49, Token (EXPORT _)) -> Just (Shift 70)
    (49, Token (AS _)) -> Just (Shift 71)
    (49, Token (QVARID _)) -> Just (Shift 72)
    (49, Token (STRING _)) -> Just (Shift 322)
    (49, Token (INTEGER _)) -> Just (Shift 323)
    (50, Token (LPAREN _)) -> Just (Shift 67)
    (50, Token (EQUAL _)) -> Just (Shift 37)
    (50, Token (PIPE _)) -> Just (Shift 45)
    (50, Token (EXPORT _)) -> Just (Shift 70)
    (50, Token (AS _)) -> Just (Shift 71)
    (50, Token (QVARID _)) -> Just (Shift 72)
    (51, Token (RBRACE _)) -> Just (Reduce 0 80)
    (51, Token (LPAREN _)) -> Just (Shift 67)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (51, Token (EXPORT _)) -> Just (Shift 70)
    (51, Token (AS _)) -> Just (Shift 71)
    (51, Token (QVARID _)) -> Just (Shift 72)
    (52, Token (RBRACE _)) -> Just (Reduce 0 80)
    (52, Token (LPAREN _)) -> Just (Shift 67)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (52, Token (EXPORT _)) -> Just (Shift 70)
    (52, Token (AS _)) -> Just (Shift 71)
    (52, Token (QVARID _)) -> Just (Shift 72)
    (53, Token (LPAREN _)) -> Just (Shift 67)
    (53, Token (EQUAL _)) -> Just (Shift 38)
    (53, Token (PIPE _)) -> Just (Shift 47)
    (53, Token (EXPORT _)) -> Just (Shift 70)
    (53, Token (AS _)) -> Just (Shift 71)
    (53, Token (QVARID _)) -> Just (Shift 72)
    (54, Token (LPAREN _)) -> Just (Shift 67)
    (54, Token (EQUAL _)) -> Just (Shift 39)
    (54, Token (PIPE _)) -> Just (Shift 48)
    (54, Token (EXPORT _)) -> Just (Shift 70)
    (54, Token (AS _)) -> Just (Shift 71)
    (54, Token (QVARID _)) -> Just (Shift 72)
    (55, Token (LPAREN _)) -> Just (Shift 68)
    (55, Token (RPAREN _)) -> Just (Reduce 0 15)
    (55, Token (QCONID _)) -> Just (Shift 118)
    (55, Token (EXPORT _)) -> Just (Shift 70)
    (55, Token (AS _)) -> Just (Shift 71)
    (55, Token (QVARID _)) -> Just (Shift 72)
    (56, Token (LPAREN _)) -> Just (Shift 68)
    (56, Token (RPAREN _)) -> Just (Reduce 0 15)
    (56, Token (QCONID _)) -> Just (Shift 118)
    (56, Token (EXPORT _)) -> Just (Shift 70)
    (56, Token (AS _)) -> Just (Shift 71)
    (56, Token (QVARID _)) -> Just (Shift 72)
    (57, Token (LPAREN _)) -> Just (Shift 68)
    (57, Token (RPAREN _)) -> Just (Reduce 0 15)
    (57, Token (QCONID _)) -> Just (Shift 118)
    (57, Token (EXPORT _)) -> Just (Shift 70)
    (57, Token (AS _)) -> Just (Shift 71)
    (57, Token (QVARID _)) -> Just (Shift 72)
    (58, Token (LPAREN _)) -> Just (Shift 68)
    (58, Token (QCONID _)) -> Just (Shift 118)
    (58, Token (EXPORT _)) -> Just (Shift 70)
    (58, Token (AS _)) -> Just (Shift 71)
    (58, Token (QVARID _)) -> Just (Shift 72)
    (59, Token (LPAREN _)) -> Just (Shift 68)
    (59, Token (RPAREN _)) -> Just (Shift 124)
    (59, Token (DOT_DOT _)) -> Just (Shift 127)
    (59, Token (QCONID _)) -> Just (Shift 118)
    (59, Token (EXPORT _)) -> Just (Shift 70)
    (59, Token (AS _)) -> Just (Shift 71)
    (59, Token (QVARID _)) -> Just (Shift 72)
    (60, Token (LPAREN _)) -> Just (Shift 67)
    (60, Token (EXPORT _)) -> Just (Shift 70)
    (60, Token (AS _)) -> Just (Shift 71)
    (60, Token (QVARID _)) -> Just (Shift 72)
    (61, Token (RBRACE _)) -> Just (Shift 294)
    (61, Token (LPAREN _)) -> Just (Shift 67)
    (61, Token (EXPORT _)) -> Just (Shift 70)
    (61, Token (AS _)) -> Just (Shift 71)
    (61, Token (QVARID _)) -> Just (Shift 72)
    (62, Token (LPAREN _)) -> Just (Shift 67)
    (62, Token (EXPORT _)) -> Just (Shift 70)
    (62, Token (AS _)) -> Just (Shift 71)
    (62, Token (QVARID _)) -> Just (Shift 72)
    (63, Token (LPAREN _)) -> Just (Shift 67)
    (63, Token (EXPORT _)) -> Just (Shift 70)
    (63, Token (AS _)) -> Just (Shift 71)
    (63, Token (QVARID _)) -> Just (Shift 72)
    (64, Token (LPAREN _)) -> Just (Shift 67)
    (64, Token (EXPORT _)) -> Just (Shift 70)
    (64, Token (AS _)) -> Just (Shift 71)
    (64, Token (QVARID _)) -> Just (Shift 72)
    (65, Token (LPAREN _)) -> Just (Shift 67)
    (65, Token (EXPORT _)) -> Just (Shift 70)
    (65, Token (AS _)) -> Just (Shift 71)
    (65, Token (QVARID _)) -> Just (Shift 72)
    (66, Token (LPAREN _)) -> Just (Shift 67)
    (66, Token (EXPORT _)) -> Just (Shift 70)
    (66, Token (AS _)) -> Just (Shift 71)
    (66, Token (QVARID _)) -> Just (Shift 72)
    (67, Token (QVARSYM _)) -> Just (Shift 73)
    (68, Token (QVARSYM _)) -> Just (Shift 73)
    (68, Token (QCONSYM _)) -> Just (Shift 119)
    (69, Token (WHERE _)) -> Just (Reduce 3 165)
    (69, Token (RBRACE _)) -> Just (Reduce 3 165)
    (69, Token (LPAREN _)) -> Just (Reduce 3 165)
    (69, Token (RPAREN _)) -> Just (Reduce 3 165)
    (69, Token (COMMA _)) -> Just (Reduce 3 165)
    (69, Token (SEMICOLON _)) -> Just (Reduce 3 165)
    (69, Token (EQUAL _)) -> Just (Reduce 3 165)
    (69, Token (PIPE _)) -> Just (Reduce 3 165)
    (69, Token (COLON_COLON _)) -> Just (Reduce 3 165)
    (69, Token (QCONID _)) -> Just (Reduce 3 165)
    (69, Token (EXPORT _)) -> Just (Reduce 3 165)
    (69, Token (AS _)) -> Just (Reduce 3 165)
    (69, Token (QVARID _)) -> Just (Reduce 3 165)
    (69, Token (STRING _)) -> Just (Reduce 3 165)
    (69, Token (INTEGER _)) -> Just (Reduce 3 165)
    (69, Token (QVARSYM _)) -> Just (Reduce 3 165)
    (69, Token (QCONSYM _)) -> Just (Reduce 3 165)
    (69, Token (BACKQUOTE _)) -> Just (Reduce 3 165)
    (70, Token (WHERE _)) -> Just (Reduce 1 163)
    (70, Token (RBRACE _)) -> Just (Reduce 1 163)
    (70, Token (LPAREN _)) -> Just (Reduce 1 163)
    (70, Token (RPAREN _)) -> Just (Reduce 1 163)
    (70, Token (COMMA _)) -> Just (Reduce 1 163)
    (70, Token (SEMICOLON _)) -> Just (Reduce 1 163)
    (70, Token (EQUAL _)) -> Just (Reduce 1 163)
    (70, Token (PIPE _)) -> Just (Reduce 1 163)
    (70, Token (COLON_COLON _)) -> Just (Reduce 1 163)
    (70, Token (QCONID _)) -> Just (Reduce 1 163)
    (70, Token (EXPORT _)) -> Just (Reduce 1 163)
    (70, Token (AS _)) -> Just (Reduce 1 163)
    (70, Token (QVARID _)) -> Just (Reduce 1 163)
    (70, Token (STRING _)) -> Just (Reduce 1 163)
    (70, Token (INTEGER _)) -> Just (Reduce 1 163)
    (70, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (70, Token (QCONSYM _)) -> Just (Reduce 1 163)
    (70, Token (BACKQUOTE _)) -> Just (Reduce 1 163)
    (71, Token (WHERE _)) -> Just (Reduce 1 162)
    (71, Token (RBRACE _)) -> Just (Reduce 1 162)
    (71, Token (LPAREN _)) -> Just (Reduce 1 162)
    (71, Token (RPAREN _)) -> Just (Reduce 1 162)
    (71, Token (COMMA _)) -> Just (Reduce 1 162)
    (71, Token (SEMICOLON _)) -> Just (Reduce 1 162)
    (71, Token (EQUAL _)) -> Just (Reduce 1 162)
    (71, Token (PIPE _)) -> Just (Reduce 1 162)
    (71, Token (COLON_COLON _)) -> Just (Reduce 1 162)
    (71, Token (QCONID _)) -> Just (Reduce 1 162)
    (71, Token (EXPORT _)) -> Just (Reduce 1 162)
    (71, Token (AS _)) -> Just (Reduce 1 162)
    (71, Token (QVARID _)) -> Just (Reduce 1 162)
    (71, Token (STRING _)) -> Just (Reduce 1 162)
    (71, Token (INTEGER _)) -> Just (Reduce 1 162)
    (71, Token (QVARSYM _)) -> Just (Reduce 1 162)
    (71, Token (QCONSYM _)) -> Just (Reduce 1 162)
    (71, Token (BACKQUOTE _)) -> Just (Reduce 1 162)
    (72, Token (WHERE _)) -> Just (Reduce 1 164)
    (72, Token (RBRACE _)) -> Just (Reduce 1 164)
    (72, Token (LPAREN _)) -> Just (Reduce 1 164)
    (72, Token (RPAREN _)) -> Just (Reduce 1 164)
    (72, Token (COMMA _)) -> Just (Reduce 1 164)
    (72, Token (SEMICOLON _)) -> Just (Reduce 1 164)
    (72, Token (EQUAL _)) -> Just (Reduce 1 164)
    (72, Token (PIPE _)) -> Just (Reduce 1 164)
    (72, Token (COLON_COLON _)) -> Just (Reduce 1 164)
    (72, Token (QCONID _)) -> Just (Reduce 1 164)
    (72, Token (EXPORT _)) -> Just (Reduce 1 164)
    (72, Token (AS _)) -> Just (Reduce 1 164)
    (72, Token (QVARID _)) -> Just (Reduce 1 164)
    (72, Token (STRING _)) -> Just (Reduce 1 164)
    (72, Token (INTEGER _)) -> Just (Reduce 1 164)
    (72, Token (QVARSYM _)) -> Just (Reduce 1 164)
    (72, Token (QCONSYM _)) -> Just (Reduce 1 164)
    (72, Token (BACKQUOTE _)) -> Just (Reduce 1 164)
    (73, Token (RPAREN _)) -> Just (Shift 69)
    (74, Token (LPAREN _)) -> Just (Shift 111)
    (74, Token (LBRACKET _)) -> Just (Shift 115)
    (74, Token (EXCL _)) -> Just (Shift 74)
    (74, Token (QCONID _)) -> Just (Shift 118)
    (74, Token (EXPORT _)) -> Just (Shift 285)
    (74, Token (AS _)) -> Just (Shift 286)
    (74, Token (QVARID _)) -> Just (Shift 287)
    (75, Token (LPAREN _)) -> Just (Shift 111)
    (75, Token (LBRACKET _)) -> Just (Shift 115)
    (75, Token (EXCL _)) -> Just (Shift 74)
    (75, Token (QCONID _)) -> Just (Shift 118)
    (75, Token (EXPORT _)) -> Just (Shift 285)
    (75, Token (AS _)) -> Just (Shift 286)
    (75, Token (QVARID _)) -> Just (Shift 287)
    (76, Token (WHERE _)) -> Just (Shift 195)
    (76, Token (RBRACE _)) -> Just (Reduce 0 65)
    (76, Token (LPAREN _)) -> Just (Shift 111)
    (76, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (76, Token (DARROW _)) -> Just (Shift 79)
    (76, Token (LBRACKET _)) -> Just (Shift 115)
    (76, Token (EXCL _)) -> Just (Shift 74)
    (76, Token (QCONID _)) -> Just (Shift 118)
    (76, Token (EXPORT _)) -> Just (Shift 285)
    (76, Token (AS _)) -> Just (Shift 286)
    (76, Token (QVARID _)) -> Just (Shift 287)
    (77, Token (LPAREN _)) -> Just (Shift 111)
    (77, Token (LBRACKET _)) -> Just (Shift 115)
    (77, Token (EXCL _)) -> Just (Shift 74)
    (77, Token (QCONID _)) -> Just (Shift 118)
    (77, Token (EXPORT _)) -> Just (Shift 285)
    (77, Token (AS _)) -> Just (Shift 286)
    (77, Token (QVARID _)) -> Just (Shift 287)
    (78, Token (WHERE _)) -> Just (Shift 197)
    (78, Token (RBRACE _)) -> Just (Reduce 0 75)
    (78, Token (LPAREN _)) -> Just (Shift 111)
    (78, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (78, Token (DARROW _)) -> Just (Shift 81)
    (78, Token (LBRACKET _)) -> Just (Shift 115)
    (78, Token (EXCL _)) -> Just (Shift 74)
    (78, Token (QCONID _)) -> Just (Shift 118)
    (78, Token (EXPORT _)) -> Just (Shift 285)
    (78, Token (AS _)) -> Just (Shift 286)
    (78, Token (QVARID _)) -> Just (Shift 287)
    (79, Token (LPAREN _)) -> Just (Shift 111)
    (79, Token (LBRACKET _)) -> Just (Shift 115)
    (79, Token (EXCL _)) -> Just (Shift 74)
    (79, Token (QCONID _)) -> Just (Shift 118)
    (79, Token (EXPORT _)) -> Just (Shift 285)
    (79, Token (AS _)) -> Just (Shift 286)
    (79, Token (QVARID _)) -> Just (Shift 287)
    (80, Token (WHERE _)) -> Just (Shift 195)
    (80, Token (RBRACE _)) -> Just (Reduce 0 65)
    (80, Token (LPAREN _)) -> Just (Shift 111)
    (80, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (80, Token (LBRACKET _)) -> Just (Shift 115)
    (80, Token (EXCL _)) -> Just (Shift 74)
    (80, Token (QCONID _)) -> Just (Shift 118)
    (80, Token (EXPORT _)) -> Just (Shift 285)
    (80, Token (AS _)) -> Just (Shift 286)
    (80, Token (QVARID _)) -> Just (Shift 287)
    (81, Token (LPAREN _)) -> Just (Shift 111)
    (81, Token (LBRACKET _)) -> Just (Shift 115)
    (81, Token (EXCL _)) -> Just (Shift 74)
    (81, Token (QCONID _)) -> Just (Shift 118)
    (81, Token (EXPORT _)) -> Just (Shift 285)
    (81, Token (AS _)) -> Just (Shift 286)
    (81, Token (QVARID _)) -> Just (Shift 287)
    (82, Token (WHERE _)) -> Just (Shift 197)
    (82, Token (RBRACE _)) -> Just (Reduce 0 75)
    (82, Token (LPAREN _)) -> Just (Shift 111)
    (82, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (82, Token (LBRACKET _)) -> Just (Shift 115)
    (82, Token (EXCL _)) -> Just (Shift 74)
    (82, Token (QCONID _)) -> Just (Shift 118)
    (82, Token (EXPORT _)) -> Just (Shift 285)
    (82, Token (AS _)) -> Just (Shift 286)
    (82, Token (QVARID _)) -> Just (Shift 287)
    (83, Token (LPAREN _)) -> Just (Shift 111)
    (83, Token (LBRACKET _)) -> Just (Shift 115)
    (83, Token (EXCL _)) -> Just (Shift 74)
    (83, Token (QCONID _)) -> Just (Shift 118)
    (83, Token (EXPORT _)) -> Just (Shift 285)
    (83, Token (AS _)) -> Just (Shift 286)
    (83, Token (QVARID _)) -> Just (Shift 287)
    (84, Token (RBRACE _)) -> Just (Reduce 0 117)
    (84, Token (LPAREN _)) -> Just (Shift 111)
    (84, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (84, Token (EQUAL _)) -> Just (Shift 87)
    (84, Token (DERIVING _)) -> Just (Reduce 0 117)
    (84, Token (DARROW _)) -> Just (Shift 85)
    (84, Token (LBRACKET _)) -> Just (Shift 115)
    (84, Token (EXCL _)) -> Just (Shift 74)
    (84, Token (QCONID _)) -> Just (Shift 118)
    (84, Token (EXPORT _)) -> Just (Shift 285)
    (84, Token (AS _)) -> Just (Shift 286)
    (84, Token (QVARID _)) -> Just (Shift 287)
    (85, Token (LPAREN _)) -> Just (Shift 111)
    (85, Token (LBRACKET _)) -> Just (Shift 115)
    (85, Token (EXCL _)) -> Just (Shift 74)
    (85, Token (QCONID _)) -> Just (Shift 118)
    (85, Token (EXPORT _)) -> Just (Shift 285)
    (85, Token (AS _)) -> Just (Shift 286)
    (85, Token (QVARID _)) -> Just (Shift 287)
    (86, Token (RBRACE _)) -> Just (Reduce 0 117)
    (86, Token (LPAREN _)) -> Just (Shift 111)
    (86, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (86, Token (EQUAL _)) -> Just (Shift 87)
    (86, Token (DERIVING _)) -> Just (Reduce 0 117)
    (86, Token (LBRACKET _)) -> Just (Shift 115)
    (86, Token (EXCL _)) -> Just (Shift 74)
    (86, Token (QCONID _)) -> Just (Shift 118)
    (86, Token (EXPORT _)) -> Just (Shift 285)
    (86, Token (AS _)) -> Just (Shift 286)
    (86, Token (QVARID _)) -> Just (Shift 287)
    (87, Token (LPAREN _)) -> Just (Shift 111)
    (87, Token (LBRACKET _)) -> Just (Shift 115)
    (87, Token (EXCL _)) -> Just (Shift 74)
    (87, Token (QCONID _)) -> Just (Shift 118)
    (87, Token (EXPORT _)) -> Just (Shift 285)
    (87, Token (AS _)) -> Just (Shift 286)
    (87, Token (QVARID _)) -> Just (Shift 287)
    (88, Token (LPAREN _)) -> Just (Shift 111)
    (88, Token (LBRACKET _)) -> Just (Shift 115)
    (88, Token (EXCL _)) -> Just (Shift 74)
    (88, Token (QCONID _)) -> Just (Shift 118)
    (88, Token (EXPORT _)) -> Just (Shift 285)
    (88, Token (AS _)) -> Just (Shift 286)
    (88, Token (QVARID _)) -> Just (Shift 287)
    (89, Token (LPAREN _)) -> Just (Shift 116)
    (89, Token (QCONID _)) -> Just (Shift 118)
    (90, Token (RBRACE _)) -> Just (Reduce 1 121)
    (90, Token (LPAREN _)) -> Just (Shift 111)
    (90, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (90, Token (DERIVING _)) -> Just (Reduce 1 121)
    (90, Token (PIPE _)) -> Just (Reduce 1 121)
    (90, Token (LBRACKET _)) -> Just (Shift 115)
    (90, Token (EXCL _)) -> Just (Shift 74)
    (90, Token (QCONID _)) -> Just (Shift 118)
    (90, Token (EXPORT _)) -> Just (Shift 285)
    (90, Token (AS _)) -> Just (Shift 286)
    (90, Token (QVARID _)) -> Just (Shift 287)
    (90, Token (QCONSYM _)) -> Just (Shift 298)
    (90, Token (BACKQUOTE _)) -> Just (Shift 300)
    (91, Token (LPAREN _)) -> Just (Shift 111)
    (91, Token (LBRACKET _)) -> Just (Shift 115)
    (91, Token (EXCL _)) -> Just (Shift 74)
    (91, Token (QCONID _)) -> Just (Shift 118)
    (91, Token (EXPORT _)) -> Just (Shift 285)
    (91, Token (AS _)) -> Just (Shift 286)
    (91, Token (QVARID _)) -> Just (Shift 287)
    (92, Token (RBRACE _)) -> Just (Reduce 3 122)
    (92, Token (LPAREN _)) -> Just (Shift 111)
    (92, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (92, Token (DERIVING _)) -> Just (Reduce 3 122)
    (92, Token (PIPE _)) -> Just (Reduce 3 122)
    (92, Token (LBRACKET _)) -> Just (Shift 115)
    (92, Token (EXCL _)) -> Just (Shift 74)
    (92, Token (QCONID _)) -> Just (Shift 118)
    (92, Token (EXPORT _)) -> Just (Shift 285)
    (92, Token (AS _)) -> Just (Shift 286)
    (92, Token (QVARID _)) -> Just (Shift 287)
    (93, Token (LPAREN _)) -> Just (Shift 111)
    (93, Token (LBRACKET _)) -> Just (Shift 115)
    (93, Token (EXCL _)) -> Just (Shift 74)
    (93, Token (QCONID _)) -> Just (Shift 118)
    (93, Token (EXPORT _)) -> Just (Shift 285)
    (93, Token (AS _)) -> Just (Shift 286)
    (93, Token (QVARID _)) -> Just (Shift 287)
    (94, Token (RBRACE _)) -> Just (Reduce 1 98)
    (94, Token (LPAREN _)) -> Just (Shift 111)
    (94, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (94, Token (DARROW _)) -> Just (Shift 100)
    (94, Token (RARROW _)) -> Just (Shift 96)
    (94, Token (LBRACKET _)) -> Just (Shift 115)
    (94, Token (EXCL _)) -> Just (Shift 74)
    (94, Token (QCONID _)) -> Just (Shift 118)
    (94, Token (EXPORT _)) -> Just (Shift 285)
    (94, Token (AS _)) -> Just (Shift 286)
    (94, Token (QVARID _)) -> Just (Shift 287)
    (95, Token (LPAREN _)) -> Just (Shift 111)
    (95, Token (LBRACKET _)) -> Just (Shift 115)
    (95, Token (EXCL _)) -> Just (Shift 74)
    (95, Token (QCONID _)) -> Just (Shift 118)
    (95, Token (EXPORT _)) -> Just (Shift 285)
    (95, Token (AS _)) -> Just (Shift 286)
    (95, Token (QVARID _)) -> Just (Shift 287)
    (96, Token (LPAREN _)) -> Just (Shift 111)
    (96, Token (LBRACKET _)) -> Just (Shift 115)
    (96, Token (EXCL _)) -> Just (Shift 74)
    (96, Token (QCONID _)) -> Just (Shift 118)
    (96, Token (EXPORT _)) -> Just (Shift 285)
    (96, Token (AS _)) -> Just (Shift 286)
    (96, Token (QVARID _)) -> Just (Shift 287)
    (97, Token (LPAREN _)) -> Just (Shift 111)
    (97, Token (LBRACKET _)) -> Just (Shift 115)
    (97, Token (EXCL _)) -> Just (Shift 74)
    (97, Token (QCONID _)) -> Just (Shift 118)
    (97, Token (EXPORT _)) -> Just (Shift 285)
    (97, Token (AS _)) -> Just (Shift 286)
    (97, Token (QVARID _)) -> Just (Shift 287)
    (98, Token (LPAREN _)) -> Just (Shift 111)
    (98, Token (LBRACKET _)) -> Just (Shift 115)
    (98, Token (EXCL _)) -> Just (Shift 74)
    (98, Token (QCONID _)) -> Just (Shift 118)
    (98, Token (EXPORT _)) -> Just (Shift 285)
    (98, Token (AS _)) -> Just (Shift 286)
    (98, Token (QVARID _)) -> Just (Shift 287)
    (99, Token (LPAREN _)) -> Just (Shift 111)
    (99, Token (LBRACKET _)) -> Just (Shift 115)
    (99, Token (EXCL _)) -> Just (Shift 74)
    (99, Token (QCONID _)) -> Just (Shift 118)
    (99, Token (EXPORT _)) -> Just (Shift 285)
    (99, Token (AS _)) -> Just (Shift 286)
    (99, Token (QVARID _)) -> Just (Shift 287)
    (100, Token (LPAREN _)) -> Just (Shift 111)
    (100, Token (LBRACKET _)) -> Just (Shift 115)
    (100, Token (EXCL _)) -> Just (Shift 74)
    (100, Token (QCONID _)) -> Just (Shift 118)
    (100, Token (EXPORT _)) -> Just (Shift 285)
    (100, Token (AS _)) -> Just (Shift 286)
    (100, Token (QVARID _)) -> Just (Shift 287)
    (101, Token (RBRACE _)) -> Just (Reduce 1 98)
    (101, Token (LPAREN _)) -> Just (Shift 111)
    (101, Token (RPAREN _)) -> Just (Reduce 1 98)
    (101, Token (COMMA _)) -> Just (Reduce 1 98)
    (101, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (101, Token (RARROW _)) -> Just (Shift 96)
    (101, Token (LBRACKET _)) -> Just (Shift 115)
    (101, Token (RBRACKET _)) -> Just (Reduce 1 98)
    (101, Token (EXCL _)) -> Just (Shift 74)
    (101, Token (QCONID _)) -> Just (Shift 118)
    (101, Token (EXPORT _)) -> Just (Shift 285)
    (101, Token (AS _)) -> Just (Shift 286)
    (101, Token (QVARID _)) -> Just (Shift 287)
    (102, Token (LPAREN _)) -> Just (Shift 111)
    (102, Token (LBRACKET _)) -> Just (Shift 115)
    (102, Token (EXCL _)) -> Just (Shift 74)
    (102, Token (QCONID _)) -> Just (Shift 118)
    (102, Token (EXPORT _)) -> Just (Shift 285)
    (102, Token (AS _)) -> Just (Shift 286)
    (102, Token (QVARID _)) -> Just (Shift 287)
    (103, Token (LPAREN _)) -> Just (Shift 111)
    (103, Token (LBRACKET _)) -> Just (Shift 115)
    (103, Token (EXCL _)) -> Just (Shift 74)
    (103, Token (QCONID _)) -> Just (Shift 118)
    (103, Token (EXPORT _)) -> Just (Shift 285)
    (103, Token (AS _)) -> Just (Shift 286)
    (103, Token (QVARID _)) -> Just (Shift 287)
    (104, Token (LBRACE _)) -> Just (Shift 63)
    (104, Token (LPAREN _)) -> Just (Shift 111)
    (104, Token (LBRACKET _)) -> Just (Shift 115)
    (104, Token (EXCL _)) -> Just (Shift 74)
    (104, Token (QCONID _)) -> Just (Shift 118)
    (104, Token (EXPORT _)) -> Just (Shift 285)
    (104, Token (AS _)) -> Just (Shift 286)
    (104, Token (QVARID _)) -> Just (Shift 287)
    (105, Token (LPAREN _)) -> Just (Shift 111)
    (105, Token (LBRACKET _)) -> Just (Shift 115)
    (105, Token (EXCL _)) -> Just (Shift 74)
    (105, Token (QCONID _)) -> Just (Shift 118)
    (105, Token (EXPORT _)) -> Just (Shift 285)
    (105, Token (AS _)) -> Just (Shift 286)
    (105, Token (QVARID _)) -> Just (Shift 287)
    (106, Token (LPAREN _)) -> Just (Shift 111)
    (106, Token (EQUAL _)) -> Just (Shift 89)
    (106, Token (DARROW _)) -> Just (Shift 108)
    (106, Token (LBRACKET _)) -> Just (Shift 115)
    (106, Token (EXCL _)) -> Just (Shift 74)
    (106, Token (QCONID _)) -> Just (Shift 118)
    (106, Token (EXPORT _)) -> Just (Shift 285)
    (106, Token (AS _)) -> Just (Shift 286)
    (106, Token (QVARID _)) -> Just (Shift 287)
    (107, Token (LPAREN _)) -> Just (Shift 111)
    (107, Token (LBRACKET _)) -> Just (Shift 115)
    (107, Token (EXCL _)) -> Just (Shift 74)
    (107, Token (QCONID _)) -> Just (Shift 118)
    (107, Token (EXPORT _)) -> Just (Shift 285)
    (107, Token (AS _)) -> Just (Shift 286)
    (107, Token (QVARID _)) -> Just (Shift 287)
    (108, Token (LPAREN _)) -> Just (Shift 111)
    (108, Token (LBRACKET _)) -> Just (Shift 115)
    (108, Token (EXCL _)) -> Just (Shift 74)
    (108, Token (QCONID _)) -> Just (Shift 118)
    (108, Token (EXPORT _)) -> Just (Shift 285)
    (108, Token (AS _)) -> Just (Shift 286)
    (108, Token (QVARID _)) -> Just (Shift 287)
    (109, Token (LPAREN _)) -> Just (Shift 111)
    (109, Token (EQUAL _)) -> Just (Shift 95)
    (109, Token (LBRACKET _)) -> Just (Shift 115)
    (109, Token (EXCL _)) -> Just (Shift 74)
    (109, Token (QCONID _)) -> Just (Shift 118)
    (109, Token (EXPORT _)) -> Just (Shift 285)
    (109, Token (AS _)) -> Just (Shift 286)
    (109, Token (QVARID _)) -> Just (Shift 287)
    (110, Token (LPAREN _)) -> Just (Shift 111)
    (110, Token (EQUAL _)) -> Just (Shift 89)
    (110, Token (LBRACKET _)) -> Just (Shift 115)
    (110, Token (EXCL _)) -> Just (Shift 74)
    (110, Token (QCONID _)) -> Just (Shift 118)
    (110, Token (EXPORT _)) -> Just (Shift 285)
    (110, Token (AS _)) -> Just (Shift 286)
    (110, Token (QVARID _)) -> Just (Shift 287)
    (111, Token (LPAREN _)) -> Just (Shift 111)
    (111, Token (RPAREN _)) -> Just (Shift 277)
    (111, Token (COMMA _)) -> Just (Shift 290)
    (111, Token (RARROW _)) -> Just (Shift 280)
    (111, Token (LBRACKET _)) -> Just (Shift 115)
    (111, Token (EXCL _)) -> Just (Shift 74)
    (111, Token (QCONID _)) -> Just (Shift 118)
    (111, Token (EXPORT _)) -> Just (Shift 285)
    (111, Token (AS _)) -> Just (Shift 286)
    (111, Token (QVARID _)) -> Just (Shift 287)
    (111, Token (QCONSYM _)) -> Just (Shift 119)
    (112, Token (LPAREN _)) -> Just (Shift 111)
    (112, Token (RPAREN _)) -> Just (Shift 141)
    (112, Token (LBRACKET _)) -> Just (Shift 115)
    (112, Token (EXCL _)) -> Just (Shift 74)
    (112, Token (QCONID _)) -> Just (Shift 118)
    (112, Token (EXPORT _)) -> Just (Shift 285)
    (112, Token (AS _)) -> Just (Shift 286)
    (112, Token (QVARID _)) -> Just (Shift 287)
    (113, Token (LPAREN _)) -> Just (Shift 111)
    (113, Token (LBRACKET _)) -> Just (Shift 115)
    (113, Token (EXCL _)) -> Just (Shift 74)
    (113, Token (QCONID _)) -> Just (Shift 118)
    (113, Token (EXPORT _)) -> Just (Shift 285)
    (113, Token (AS _)) -> Just (Shift 286)
    (113, Token (QVARID _)) -> Just (Shift 287)
    (114, Token (LPAREN _)) -> Just (Shift 111)
    (114, Token (LBRACKET _)) -> Just (Shift 115)
    (114, Token (EXCL _)) -> Just (Shift 74)
    (114, Token (QCONID _)) -> Just (Shift 118)
    (114, Token (EXPORT _)) -> Just (Shift 285)
    (114, Token (AS _)) -> Just (Shift 286)
    (114, Token (QVARID _)) -> Just (Shift 287)
    (115, Token (LPAREN _)) -> Just (Shift 111)
    (115, Token (LBRACKET _)) -> Just (Shift 115)
    (115, Token (RBRACKET _)) -> Just (Shift 281)
    (115, Token (EXCL _)) -> Just (Shift 74)
    (115, Token (QCONID _)) -> Just (Shift 118)
    (115, Token (EXPORT _)) -> Just (Shift 285)
    (115, Token (AS _)) -> Just (Shift 286)
    (115, Token (QVARID _)) -> Just (Shift 287)
    (116, Token (QCONSYM _)) -> Just (Shift 119)
    (117, Token (WHERE _)) -> Just (Reduce 3 167)
    (117, Token (LBRACE _)) -> Just (Reduce 3 167)
    (117, Token (RBRACE _)) -> Just (Reduce 3 167)
    (117, Token (LPAREN _)) -> Just (Reduce 3 167)
    (117, Token (RPAREN _)) -> Just (Reduce 3 167)
    (117, Token (COMMA _)) -> Just (Reduce 3 167)
    (117, Token (SEMICOLON _)) -> Just (Reduce 3 167)
    (117, Token (EQUAL _)) -> Just (Reduce 3 167)
    (117, Token (DERIVING _)) -> Just (Reduce 3 167)
    (117, Token (DARROW _)) -> Just (Reduce 3 167)
    (117, Token (PIPE _)) -> Just (Reduce 3 167)
    (117, Token (COLON_COLON _)) -> Just (Reduce 3 167)
    (117, Token (INFIXL _)) -> Just (Reduce 3 167)
    (117, Token (INFIXR _)) -> Just (Reduce 3 167)
    (117, Token (INFIX _)) -> Just (Reduce 3 167)
    (117, Token (RARROW _)) -> Just (Reduce 3 167)
    (117, Token (LBRACKET _)) -> Just (Reduce 3 167)
    (117, Token (RBRACKET _)) -> Just (Reduce 3 167)
    (117, Token (EXCL _)) -> Just (Reduce 3 167)
    (117, Token (QCONID _)) -> Just (Reduce 3 167)
    (117, Token (EXPORT _)) -> Just (Reduce 3 167)
    (117, Token (AS _)) -> Just (Reduce 3 167)
    (117, Token (QVARID _)) -> Just (Reduce 3 167)
    (117, Token (INTEGER _)) -> Just (Reduce 3 167)
    (117, Token (QVARSYM _)) -> Just (Reduce 3 167)
    (117, Token (QCONSYM _)) -> Just (Reduce 3 167)
    (117, Token (BACKQUOTE _)) -> Just (Reduce 3 167)
    (118, Token (WHERE _)) -> Just (Reduce 1 166)
    (118, Token (LBRACE _)) -> Just (Reduce 1 166)
    (118, Token (RBRACE _)) -> Just (Reduce 1 166)
    (118, Token (LPAREN _)) -> Just (Reduce 1 166)
    (118, Token (RPAREN _)) -> Just (Reduce 1 166)
    (118, Token (COMMA _)) -> Just (Reduce 1 166)
    (118, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (118, Token (EQUAL _)) -> Just (Reduce 1 166)
    (118, Token (DERIVING _)) -> Just (Reduce 1 166)
    (118, Token (DARROW _)) -> Just (Reduce 1 166)
    (118, Token (PIPE _)) -> Just (Reduce 1 166)
    (118, Token (COLON_COLON _)) -> Just (Reduce 1 166)
    (118, Token (INFIXL _)) -> Just (Reduce 1 166)
    (118, Token (INFIXR _)) -> Just (Reduce 1 166)
    (118, Token (INFIX _)) -> Just (Reduce 1 166)
    (118, Token (RARROW _)) -> Just (Reduce 1 166)
    (118, Token (LBRACKET _)) -> Just (Reduce 1 166)
    (118, Token (RBRACKET _)) -> Just (Reduce 1 166)
    (118, Token (EXCL _)) -> Just (Reduce 1 166)
    (118, Token (QCONID _)) -> Just (Reduce 1 166)
    (118, Token (EXPORT _)) -> Just (Reduce 1 166)
    (118, Token (AS _)) -> Just (Reduce 1 166)
    (118, Token (QVARID _)) -> Just (Reduce 1 166)
    (118, Token (INTEGER _)) -> Just (Reduce 1 166)
    (118, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (118, Token (QCONSYM _)) -> Just (Reduce 1 166)
    (118, Token (BACKQUOTE _)) -> Just (Reduce 1 166)
    (119, Token (RPAREN _)) -> Just (Shift 117)
    (120, Token (RPAREN _)) -> Just (Reduce 3 24)
    (121, Token (RPAREN _)) -> Just (Reduce 1 23)
    (121, Token (COMMA _)) -> Just (Shift 58)
    (122, Token (RPAREN _)) -> Just (Reduce 3 17)
    (123, Token (RPAREN _)) -> Just (Reduce 1 16)
    (123, Token (COMMA _)) -> Just (Shift 55)
    (124, Token (RPAREN _)) -> Just (Reduce 3 20)
    (124, Token (COMMA _)) -> Just (Reduce 3 20)
    (125, Token (RPAREN _)) -> Just (Reduce 4 21)
    (125, Token (COMMA _)) -> Just (Reduce 4 21)
    (126, Token (RPAREN _)) -> Just (Reduce 4 22)
    (126, Token (COMMA _)) -> Just (Reduce 4 22)
    (127, Token (RPAREN _)) -> Just (Shift 125)
    (128, Token (RPAREN _)) -> Just (Reduce 1 18)
    (128, Token (COMMA _)) -> Just (Reduce 1 18)
    (129, Token (LPAREN _)) -> Just (Shift 59)
    (129, Token (RPAREN _)) -> Just (Reduce 1 19)
    (129, Token (COMMA _)) -> Just (Reduce 1 19)
    (130, Token (RPAREN _)) -> Just (Shift 126)
    (131, Token (RPAREN _)) -> Just (Reduce 1 25)
    (131, Token (COMMA _)) -> Just (Reduce 1 25)
    (132, Token (RPAREN _)) -> Just (Reduce 1 26)
    (132, Token (COMMA _)) -> Just (Reduce 1 26)
    (133, Token (RPAREN _)) -> Just (Shift 137)
    (133, Token (QCONID _)) -> Just (Shift 188)
    (134, Token (RPAREN _)) -> Just (Shift 138)
    (134, Token (QCONID _)) -> Just (Shift 188)
    (135, Token (RPAREN _)) -> Just (Shift 139)
    (135, Token (QCONID _)) -> Just (Shift 188)
    (136, Token (RPAREN _)) -> Just (Shift 140)
    (136, Token (QCONID _)) -> Just (Shift 188)
    (137, Token (RBRACE _)) -> Just (Reduce 6 35)
    (137, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (138, Token (RBRACE _)) -> Just (Reduce 8 39)
    (138, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (139, Token (RBRACE _)) -> Just (Reduce 8 47)
    (139, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (140, Token (RBRACE _)) -> Just (Reduce 6 43)
    (140, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (141, Token (RBRACE _)) -> Just (Reduce 3 53)
    (141, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (142, Token (RBRACE _)) -> Just (Reduce 8 31)
    (142, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (143, Token (RBRACE _)) -> Just (Reduce 7 30)
    (143, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (144, Token (RBRACE _)) -> Just (Reduce 7 36)
    (144, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (145, Token (RBRACE _)) -> Just (Reduce 9 40)
    (145, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (146, Token (RBRACE _)) -> Just (Reduce 9 48)
    (146, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (147, Token (RBRACE _)) -> Just (Reduce 7 44)
    (147, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (148, Token (RBRACE _)) -> Just (Reduce 4 54)
    (148, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (149, Token (QCONID _)) -> Just (Reduce 0 178)
    (149, Token (QUALIFIED _)) -> Just (Shift 181)
    (150, Token (LPAREN _)) -> Just (Shift 56)
    (151, Token (LPAREN _)) -> Just (Shift 133)
    (151, Token (QCONID _)) -> Just (Shift 188)
    (152, Token (LPAREN _)) -> Just (Shift 134)
    (152, Token (QCONID _)) -> Just (Shift 188)
    (153, Token (LPAREN _)) -> Just (Shift 135)
    (153, Token (QCONID _)) -> Just (Shift 188)
    (154, Token (LPAREN _)) -> Just (Shift 136)
    (154, Token (QCONID _)) -> Just (Shift 188)
    (155, Token (LPAREN _)) -> Just (Shift 112)
    (156, Token (IMPORT _)) -> Just (Shift 201)
    (156, Token (EXPORT _)) -> Just (Shift 202)
    (157, Token (RBRACE _)) -> Just (Reduce 0 176)
    (157, Token (LPAREN _)) -> Just (Reduce 0 176)
    (157, Token (SEMICOLON _)) -> Just (Reduce 0 176)
    (157, Token (HIDING _)) -> Just (Reduce 0 176)
    (157, Token (AS _)) -> Just (Shift 9)
    (158, Token (RPAREN _)) -> Just (Shift 142)
    (159, Token (RPAREN _)) -> Just (Shift 143)
    (160, Token (RBRACE _)) -> Just (Reduce 4 29)
    (160, Token (LPAREN _)) -> Just (Shift 57)
    (160, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (160, Token (HIDING _)) -> Just (Shift 150)
    (161, Token (RBRACE _)) -> Just (Reduce 4 32)
    (161, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (162, Token (RBRACE _)) -> Just (Reduce 3 33)
    (162, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (162, Token (DERIVING _)) -> Just (Shift 151)
    (163, Token (RBRACE _)) -> Just (Reduce 5 37)
    (163, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (163, Token (DERIVING _)) -> Just (Shift 152)
    (164, Token (RBRACE _)) -> Just (Reduce 5 34)
    (164, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (165, Token (RBRACE _)) -> Just (Reduce 7 38)
    (165, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (166, Token (RBRACE _)) -> Just (Reduce 7 46)
    (166, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (167, Token (RBRACE _)) -> Just (Reduce 5 42)
    (167, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (168, Token (RPAREN _)) -> Just (Shift 144)
    (169, Token (RPAREN _)) -> Just (Shift 145)
    (170, Token (RPAREN _)) -> Just (Shift 146)
    (171, Token (RPAREN _)) -> Just (Shift 147)
    (172, Token (RBRACE _)) -> Just (Reduce 5 45)
    (172, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (172, Token (DERIVING _)) -> Just (Shift 153)
    (173, Token (RBRACE _)) -> Just (Reduce 3 41)
    (173, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (173, Token (DERIVING _)) -> Just (Shift 154)
    (174, Token (RBRACE _)) -> Just (Reduce 5 50)
    (174, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (175, Token (RBRACE _)) -> Just (Reduce 3 49)
    (175, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (176, Token (RBRACE _)) -> Just (Reduce 5 52)
    (176, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (177, Token (RBRACE _)) -> Just (Reduce 3 51)
    (177, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (178, Token (RPAREN _)) -> Just (Shift 148)
    (179, Token (RBRACE _)) -> Just (Reduce 2 55)
    (179, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (180, Token (RBRACE _)) -> Just (Reduce 1 56)
    (180, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (181, Token (QCONID _)) -> Just (Reduce 1 179)
    (182, Token (RBRACE _)) -> Just (Reduce 2 177)
    (182, Token (LPAREN _)) -> Just (Reduce 2 177)
    (182, Token (SEMICOLON _)) -> Just (Reduce 2 177)
    (182, Token (HIDING _)) -> Just (Reduce 2 177)
    (183, Token (WHERE _)) -> Just (Reduce 1 100)
    (183, Token (LBRACE _)) -> Just (Reduce 1 100)
    (183, Token (RBRACE _)) -> Just (Reduce 1 100)
    (183, Token (LPAREN _)) -> Just (Reduce 1 100)
    (183, Token (RPAREN _)) -> Just (Reduce 1 100)
    (183, Token (COMMA _)) -> Just (Reduce 1 100)
    (183, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (183, Token (EQUAL _)) -> Just (Reduce 1 100)
    (183, Token (DERIVING _)) -> Just (Reduce 1 100)
    (183, Token (DARROW _)) -> Just (Reduce 1 100)
    (183, Token (PIPE _)) -> Just (Reduce 1 100)
    (183, Token (COLON_COLON _)) -> Just (Reduce 1 100)
    (183, Token (INFIXL _)) -> Just (Reduce 1 100)
    (183, Token (INFIXR _)) -> Just (Reduce 1 100)
    (183, Token (INFIX _)) -> Just (Reduce 1 100)
    (183, Token (RARROW _)) -> Just (Reduce 1 100)
    (183, Token (LBRACKET _)) -> Just (Reduce 1 100)
    (183, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (183, Token (EXCL _)) -> Just (Reduce 1 100)
    (183, Token (QCONID _)) -> Just (Reduce 1 100)
    (183, Token (EXPORT _)) -> Just (Reduce 1 100)
    (183, Token (AS _)) -> Just (Reduce 1 100)
    (183, Token (QVARID _)) -> Just (Reduce 1 100)
    (183, Token (INTEGER _)) -> Just (Reduce 1 100)
    (183, Token (QVARSYM _)) -> Just (Reduce 1 100)
    (183, Token (QCONSYM _)) -> Just (Reduce 1 100)
    (183, Token (BACKQUOTE _)) -> Just (Reduce 1 100)
    (184, Token (WHERE _)) -> Just (Reduce 2 101)
    (184, Token (LBRACE _)) -> Just (Reduce 2 101)
    (184, Token (RBRACE _)) -> Just (Reduce 2 101)
    (184, Token (LPAREN _)) -> Just (Reduce 2 101)
    (184, Token (RPAREN _)) -> Just (Reduce 2 101)
    (184, Token (COMMA _)) -> Just (Reduce 2 101)
    (184, Token (SEMICOLON _)) -> Just (Reduce 2 101)
    (184, Token (EQUAL _)) -> Just (Reduce 2 101)
    (184, Token (DERIVING _)) -> Just (Reduce 2 101)
    (184, Token (DARROW _)) -> Just (Reduce 2 101)
    (184, Token (PIPE _)) -> Just (Reduce 2 101)
    (184, Token (COLON_COLON _)) -> Just (Reduce 2 101)
    (184, Token (INFIXL _)) -> Just (Reduce 2 101)
    (184, Token (INFIXR _)) -> Just (Reduce 2 101)
    (184, Token (INFIX _)) -> Just (Reduce 2 101)
    (184, Token (RARROW _)) -> Just (Reduce 2 101)
    (184, Token (LBRACKET _)) -> Just (Reduce 2 101)
    (184, Token (RBRACKET _)) -> Just (Reduce 2 101)
    (184, Token (EXCL _)) -> Just (Reduce 2 101)
    (184, Token (QCONID _)) -> Just (Reduce 2 101)
    (184, Token (EXPORT _)) -> Just (Reduce 2 101)
    (184, Token (AS _)) -> Just (Reduce 2 101)
    (184, Token (QVARID _)) -> Just (Reduce 2 101)
    (184, Token (INTEGER _)) -> Just (Reduce 2 101)
    (184, Token (QVARSYM _)) -> Just (Reduce 2 101)
    (184, Token (QCONSYM _)) -> Just (Reduce 2 101)
    (184, Token (BACKQUOTE _)) -> Just (Reduce 2 101)
    (185, Token (RBRACE _)) -> Just (Reduce 3 99)
    (185, Token (RPAREN _)) -> Just (Reduce 3 99)
    (185, Token (COMMA _)) -> Just (Reduce 3 99)
    (185, Token (SEMICOLON _)) -> Just (Reduce 3 99)
    (185, Token (RBRACKET _)) -> Just (Reduce 3 99)
    (186, Token (RBRACE _)) -> Just (Reduce 2 118)
    (186, Token (SEMICOLON _)) -> Just (Reduce 2 118)
    (186, Token (DERIVING _)) -> Just (Reduce 2 118)
    (187, Token (QCONID _)) -> Just (Shift 188)
    (188, Token (RBRACE _)) -> Just (Reduce 1 132)
    (188, Token (RPAREN _)) -> Just (Reduce 1 132)
    (188, Token (COMMA _)) -> Just (Reduce 1 132)
    (188, Token (SEMICOLON _)) -> Just (Reduce 1 132)
    (189, Token (RPAREN _)) -> Just (Reduce 1 130)
    (189, Token (COMMA _)) -> Just (Shift 187)
    (190, Token (RPAREN _)) -> Just (Reduce 3 131)
    (191, Token (RBRACE _)) -> Just (Reduce 7 126)
    (191, Token (SEMICOLON _)) -> Just (Reduce 7 126)
    (191, Token (DERIVING _)) -> Just (Reduce 7 126)
    (192, Token (COLON_COLON _)) -> Just (Shift 103)
    (193, Token (RBRACE _)) -> Just (Shift 191)
    (194, Token (RBRACE _)) -> Just (Reduce 3 125)
    (194, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (194, Token (DERIVING _)) -> Just (Reduce 3 125)
    (195, Token (LBRACE _)) -> Just (Shift 43)
    (196, Token (RBRACE _)) -> Just (Reduce 2 66)
    (196, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (197, Token (LBRACE _)) -> Just (Shift 51)
    (198, Token (RBRACE _)) -> Just (Reduce 2 76)
    (198, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (199, Token (RPAREN _)) -> Just (Reduce 1 96)
    (199, Token (COMMA _)) -> Just (Shift 113)
    (200, Token (RPAREN _)) -> Just (Reduce 3 97)
    (201, Token (EXPORT _)) -> Just (Shift 306)
    (201, Token (AS _)) -> Just (Shift 307)
    (201, Token (QVARID _)) -> Just (Shift 308)
    (202, Token (EXPORT _)) -> Just (Shift 306)
    (202, Token (AS _)) -> Just (Shift 307)
    (202, Token (QVARID _)) -> Just (Shift 308)
    (203, Token (COLON_COLON _)) -> Just (Shift 97)
    (204, Token (COLON_COLON _)) -> Just (Shift 98)
    (205, Token (COLON_COLON _)) -> Just (Shift 99)
    (206, Token (RBRACE _)) -> Just (Reduce 6 133)
    (206, Token (SEMICOLON _)) -> Just (Reduce 6 133)
    (207, Token (RBRACE _)) -> Just (Reduce 7 134)
    (207, Token (SEMICOLON _)) -> Just (Reduce 7 134)
    (208, Token (RBRACE _)) -> Just (Reduce 6 135)
    (208, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (209, Token (EXPORT _)) -> Just (Shift 310)
    (209, Token (AS _)) -> Just (Shift 311)
    (209, Token (QVARID _)) -> Just (Shift 312)
    (209, Token (STRING _)) -> Just (Shift 309)
    (210, Token (STRING _)) -> Just (Shift 313)
    (211, Token (STRING _)) -> Just (Shift 309)
    (212, Token (LBRACE _)) -> Just (Shift 41)
    (213, Token (LBRACE _)) -> Just (Shift 41)
    (214, Token (RBRACE _)) -> Just (Reduce 5 62)
    (214, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (215, Token (RBRACE _)) -> Just (Reduce 5 64)
    (215, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (216, Token (RBRACE _)) -> Just (Reduce 1 60)
    (216, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (217, Token (WHERE _)) -> Just (Shift 212)
    (217, Token (RBRACE _)) -> Just (Reduce 3 61)
    (217, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (218, Token (WHERE _)) -> Just (Shift 213)
    (218, Token (RBRACE _)) -> Just (Reduce 3 63)
    (218, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (219, Token (LBRACE _)) -> Just (Shift 41)
    (220, Token (LBRACE _)) -> Just (Shift 41)
    (221, Token (LBRACE _)) -> Just (Shift 41)
    (222, Token (LBRACE _)) -> Just (Shift 41)
    (223, Token (RBRACE _)) -> Just (Reduce 3 57)
    (223, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (224, Token (RBRACE _)) -> Just (Shift 223)
    (225, Token (RBRACE _)) -> Just (Reduce 1 58)
    (225, Token (SEMICOLON _)) -> Just (Shift 42)
    (226, Token (RBRACE _)) -> Just (Reduce 3 59)
    (227, Token (RBRACE _)) -> Just (Reduce 5 87)
    (227, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (228, Token (RBRACE _)) -> Just (Reduce 3 86)
    (228, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (229, Token (COLON_COLON _)) -> Just (Shift 93)
    (230, Token (COMMA _)) -> Just (Reduce 0 185)
    (230, Token (QCONID _)) -> Just (Reduce 0 185)
    (230, Token (EXPORT _)) -> Just (Reduce 0 185)
    (230, Token (AS _)) -> Just (Reduce 0 185)
    (230, Token (QVARID _)) -> Just (Reduce 0 185)
    (230, Token (INTEGER _)) -> Just (Shift 262)
    (230, Token (QVARSYM _)) -> Just (Reduce 0 185)
    (230, Token (QCONSYM _)) -> Just (Reduce 0 185)
    (230, Token (BACKQUOTE _)) -> Just (Reduce 0 185)
    (231, Token (QVARSYM _)) -> Just (Shift 330)
    (231, Token (QCONSYM _)) -> Just (Shift 298)
    (231, Token (BACKQUOTE _)) -> Just (Shift 299)
    (232, Token (RBRACE _)) -> Just (Reduce 3 88)
    (232, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (233, Token (LPAREN _)) -> Just (Reduce 1 159)
    (233, Token (EQUAL _)) -> Just (Reduce 1 159)
    (233, Token (PIPE _)) -> Just (Reduce 1 159)
    (233, Token (EXPORT _)) -> Just (Reduce 1 159)
    (233, Token (AS _)) -> Just (Reduce 1 159)
    (233, Token (QVARID _)) -> Just (Reduce 1 159)
    (233, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (234, Token (LPAREN _)) -> Just (Reduce 2 160)
    (234, Token (EQUAL _)) -> Just (Reduce 2 160)
    (234, Token (PIPE _)) -> Just (Reduce 2 160)
    (234, Token (EXPORT _)) -> Just (Reduce 2 160)
    (234, Token (AS _)) -> Just (Reduce 2 160)
    (234, Token (QVARID _)) -> Just (Reduce 2 160)
    (234, Token (QVARSYM _)) -> Just (Reduce 2 160)
    (235, Token (WHERE _)) -> Just (Reduce 1 149)
    (235, Token (RBRACE _)) -> Just (Reduce 1 149)
    (235, Token (RPAREN _)) -> Just (Reduce 1 149)
    (235, Token (SEMICOLON _)) -> Just (Reduce 1 149)
    (235, Token (PIPE _)) -> Just (Reduce 1 149)
    (236, Token (WHERE _)) -> Just (Reduce 3 144)
    (236, Token (RBRACE _)) -> Just (Reduce 3 144)
    (236, Token (SEMICOLON _)) -> Just (Reduce 3 144)
    (236, Token (PIPE _)) -> Just (Shift 46)
    (237, Token (WHERE _)) -> Just (Reduce 5 145)
    (237, Token (RBRACE _)) -> Just (Reduce 5 145)
    (237, Token (SEMICOLON _)) -> Just (Reduce 5 145)
    (238, Token (EQUAL _)) -> Just (Shift 36)
    (239, Token (RBRACE _)) -> Just (Reduce 3 67)
    (239, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (240, Token (RBRACE _)) -> Just (Shift 239)
    (241, Token (RBRACE _)) -> Just (Reduce 3 69)
    (242, Token (RBRACE _)) -> Just (Reduce 1 68)
    (242, Token (SEMICOLON _)) -> Just (Shift 44)
    (243, Token (RBRACE _)) -> Just (Reduce 5 72)
    (243, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (244, Token (RBRACE _)) -> Just (Reduce 5 74)
    (244, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (245, Token (RBRACE _)) -> Just (Reduce 1 70)
    (245, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (246, Token (WHERE _)) -> Just (Shift 219)
    (246, Token (RBRACE _)) -> Just (Reduce 3 71)
    (246, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (247, Token (WHERE _)) -> Just (Shift 220)
    (247, Token (RBRACE _)) -> Just (Reduce 3 73)
    (247, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (248, Token (RBRACE _)) -> Just (Reduce 3 77)
    (248, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (249, Token (RBRACE _)) -> Just (Shift 248)
    (250, Token (RBRACE _)) -> Just (Reduce 3 79)
    (251, Token (RBRACE _)) -> Just (Reduce 1 78)
    (251, Token (SEMICOLON _)) -> Just (Shift 52)
    (252, Token (RBRACE _)) -> Just (Reduce 5 82)
    (252, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (253, Token (RBRACE _)) -> Just (Reduce 5 84)
    (253, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (254, Token (WHERE _)) -> Just (Shift 221)
    (254, Token (RBRACE _)) -> Just (Reduce 3 81)
    (254, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (255, Token (WHERE _)) -> Just (Shift 222)
    (255, Token (RBRACE _)) -> Just (Reduce 3 83)
    (255, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (256, Token (COMMA _)) -> Just (Shift 60)
    (256, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (257, Token (LPAREN _)) -> Just (Reduce 1 161)
    (257, Token (COMMA _)) -> Just (Shift 60)
    (257, Token (EQUAL _)) -> Just (Reduce 1 161)
    (257, Token (PIPE _)) -> Just (Reduce 1 161)
    (257, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (257, Token (EXPORT _)) -> Just (Reduce 1 161)
    (257, Token (AS _)) -> Just (Reduce 1 161)
    (257, Token (QVARID _)) -> Just (Reduce 1 161)
    (257, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (258, Token (COLON_COLON _)) -> Just (Reduce 3 92)
    (259, Token (COMMA _)) -> Just (Reduce 1 93)
    (259, Token (QCONID _)) -> Just (Reduce 1 93)
    (259, Token (EXPORT _)) -> Just (Reduce 1 93)
    (259, Token (AS _)) -> Just (Reduce 1 93)
    (259, Token (QVARID _)) -> Just (Reduce 1 93)
    (259, Token (INTEGER _)) -> Just (Reduce 1 93)
    (259, Token (QVARSYM _)) -> Just (Reduce 1 93)
    (259, Token (QCONSYM _)) -> Just (Reduce 1 93)
    (259, Token (BACKQUOTE _)) -> Just (Reduce 1 93)
    (260, Token (COMMA _)) -> Just (Reduce 1 94)
    (260, Token (QCONID _)) -> Just (Reduce 1 94)
    (260, Token (EXPORT _)) -> Just (Reduce 1 94)
    (260, Token (AS _)) -> Just (Reduce 1 94)
    (260, Token (QVARID _)) -> Just (Reduce 1 94)
    (260, Token (INTEGER _)) -> Just (Reduce 1 94)
    (260, Token (QVARSYM _)) -> Just (Reduce 1 94)
    (260, Token (QCONSYM _)) -> Just (Reduce 1 94)
    (260, Token (BACKQUOTE _)) -> Just (Reduce 1 94)
    (261, Token (COMMA _)) -> Just (Reduce 1 95)
    (261, Token (QCONID _)) -> Just (Reduce 1 95)
    (261, Token (EXPORT _)) -> Just (Reduce 1 95)
    (261, Token (AS _)) -> Just (Reduce 1 95)
    (261, Token (QVARID _)) -> Just (Reduce 1 95)
    (261, Token (INTEGER _)) -> Just (Reduce 1 95)
    (261, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (261, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (261, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (262, Token (COMMA _)) -> Just (Reduce 1 186)
    (262, Token (QCONID _)) -> Just (Reduce 1 186)
    (262, Token (EXPORT _)) -> Just (Reduce 1 186)
    (262, Token (AS _)) -> Just (Reduce 1 186)
    (262, Token (QVARID _)) -> Just (Reduce 1 186)
    (262, Token (QVARSYM _)) -> Just (Reduce 1 186)
    (262, Token (QCONSYM _)) -> Just (Reduce 1 186)
    (262, Token (BACKQUOTE _)) -> Just (Reduce 1 186)
    (263, Token (QVARSYM _)) -> Just (Shift 330)
    (263, Token (QCONSYM _)) -> Just (Shift 298)
    (263, Token (BACKQUOTE _)) -> Just (Shift 299)
    (264, Token (RBRACE _)) -> Just (Reduce 3 90)
    (264, Token (SEMICOLON _)) -> Just (Reduce 3 90)
    (265, Token (RBRACE _)) -> Just (Reduce 1 89)
    (265, Token (COMMA _)) -> Just (Shift 263)
    (265, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (266, Token (RBRACE _)) -> Just (Reduce 1 175)
    (266, Token (LPAREN _)) -> Just (Reduce 1 175)
    (266, Token (COMMA _)) -> Just (Reduce 1 175)
    (266, Token (SEMICOLON _)) -> Just (Reduce 1 175)
    (266, Token (QCONID _)) -> Just (Reduce 1 175)
    (266, Token (EXPORT _)) -> Just (Reduce 1 175)
    (266, Token (AS _)) -> Just (Reduce 1 175)
    (266, Token (QVARID _)) -> Just (Reduce 1 175)
    (266, Token (STRING _)) -> Just (Reduce 1 175)
    (266, Token (INTEGER _)) -> Just (Reduce 1 175)
    (266, Token (QVARSYM _)) -> Just (Reduce 1 175)
    (266, Token (QCONSYM _)) -> Just (Reduce 1 175)
    (266, Token (BACKQUOTE _)) -> Just (Reduce 1 175)
    (267, Token (RBRACE _)) -> Just (Reduce 1 174)
    (267, Token (LPAREN _)) -> Just (Reduce 1 174)
    (267, Token (COMMA _)) -> Just (Reduce 1 174)
    (267, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (267, Token (QCONID _)) -> Just (Reduce 1 174)
    (267, Token (EXPORT _)) -> Just (Reduce 1 174)
    (267, Token (AS _)) -> Just (Reduce 1 174)
    (267, Token (QVARID _)) -> Just (Reduce 1 174)
    (267, Token (STRING _)) -> Just (Reduce 1 174)
    (267, Token (INTEGER _)) -> Just (Reduce 1 174)
    (267, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (267, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (267, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (268, Token (WHERE _)) -> Just (Reduce 3 106)
    (268, Token (LBRACE _)) -> Just (Reduce 3 106)
    (268, Token (RBRACE _)) -> Just (Reduce 3 106)
    (268, Token (LPAREN _)) -> Just (Reduce 3 106)
    (268, Token (RPAREN _)) -> Just (Reduce 3 106)
    (268, Token (COMMA _)) -> Just (Reduce 3 106)
    (268, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (268, Token (EQUAL _)) -> Just (Reduce 3 106)
    (268, Token (DERIVING _)) -> Just (Reduce 3 106)
    (268, Token (DARROW _)) -> Just (Reduce 3 106)
    (268, Token (PIPE _)) -> Just (Reduce 3 106)
    (268, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (268, Token (INFIXL _)) -> Just (Reduce 3 106)
    (268, Token (INFIXR _)) -> Just (Reduce 3 106)
    (268, Token (INFIX _)) -> Just (Reduce 3 106)
    (268, Token (RARROW _)) -> Just (Reduce 3 106)
    (268, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (268, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (268, Token (EXCL _)) -> Just (Reduce 3 106)
    (268, Token (QCONID _)) -> Just (Reduce 3 106)
    (268, Token (EXPORT _)) -> Just (Reduce 3 106)
    (268, Token (AS _)) -> Just (Reduce 3 106)
    (268, Token (QVARID _)) -> Just (Reduce 3 106)
    (268, Token (INTEGER _)) -> Just (Reduce 3 106)
    (268, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (268, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (268, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (269, Token (WHERE _)) -> Just (Reduce 3 104)
    (269, Token (LBRACE _)) -> Just (Reduce 3 104)
    (269, Token (RBRACE _)) -> Just (Reduce 3 104)
    (269, Token (LPAREN _)) -> Just (Reduce 3 104)
    (269, Token (RPAREN _)) -> Just (Reduce 3 104)
    (269, Token (COMMA _)) -> Just (Reduce 3 104)
    (269, Token (SEMICOLON _)) -> Just (Reduce 3 104)
    (269, Token (EQUAL _)) -> Just (Reduce 3 104)
    (269, Token (DERIVING _)) -> Just (Reduce 3 104)
    (269, Token (DARROW _)) -> Just (Reduce 3 104)
    (269, Token (PIPE _)) -> Just (Reduce 3 104)
    (269, Token (COLON_COLON _)) -> Just (Reduce 3 104)
    (269, Token (INFIXL _)) -> Just (Reduce 3 104)
    (269, Token (INFIXR _)) -> Just (Reduce 3 104)
    (269, Token (INFIX _)) -> Just (Reduce 3 104)
    (269, Token (RARROW _)) -> Just (Reduce 3 104)
    (269, Token (LBRACKET _)) -> Just (Reduce 3 104)
    (269, Token (RBRACKET _)) -> Just (Reduce 3 104)
    (269, Token (EXCL _)) -> Just (Reduce 3 104)
    (269, Token (QCONID _)) -> Just (Reduce 3 104)
    (269, Token (EXPORT _)) -> Just (Reduce 3 104)
    (269, Token (AS _)) -> Just (Reduce 3 104)
    (269, Token (QVARID _)) -> Just (Reduce 3 104)
    (269, Token (INTEGER _)) -> Just (Reduce 3 104)
    (269, Token (QVARSYM _)) -> Just (Reduce 3 104)
    (269, Token (QCONSYM _)) -> Just (Reduce 3 104)
    (269, Token (BACKQUOTE _)) -> Just (Reduce 3 104)
    (270, Token (WHERE _)) -> Just (Reduce 3 105)
    (270, Token (LBRACE _)) -> Just (Reduce 3 105)
    (270, Token (RBRACE _)) -> Just (Reduce 3 105)
    (270, Token (LPAREN _)) -> Just (Reduce 3 105)
    (270, Token (RPAREN _)) -> Just (Reduce 3 105)
    (270, Token (COMMA _)) -> Just (Reduce 3 105)
    (270, Token (SEMICOLON _)) -> Just (Reduce 3 105)
    (270, Token (EQUAL _)) -> Just (Reduce 3 105)
    (270, Token (DERIVING _)) -> Just (Reduce 3 105)
    (270, Token (DARROW _)) -> Just (Reduce 3 105)
    (270, Token (PIPE _)) -> Just (Reduce 3 105)
    (270, Token (COLON_COLON _)) -> Just (Reduce 3 105)
    (270, Token (INFIXL _)) -> Just (Reduce 3 105)
    (270, Token (INFIXR _)) -> Just (Reduce 3 105)
    (270, Token (INFIX _)) -> Just (Reduce 3 105)
    (270, Token (RARROW _)) -> Just (Reduce 3 105)
    (270, Token (LBRACKET _)) -> Just (Reduce 3 105)
    (270, Token (RBRACKET _)) -> Just (Reduce 3 105)
    (270, Token (EXCL _)) -> Just (Reduce 3 105)
    (270, Token (QCONID _)) -> Just (Reduce 3 105)
    (270, Token (EXPORT _)) -> Just (Reduce 3 105)
    (270, Token (AS _)) -> Just (Reduce 3 105)
    (270, Token (QVARID _)) -> Just (Reduce 3 105)
    (270, Token (INTEGER _)) -> Just (Reduce 3 105)
    (270, Token (QVARSYM _)) -> Just (Reduce 3 105)
    (270, Token (QCONSYM _)) -> Just (Reduce 3 105)
    (270, Token (BACKQUOTE _)) -> Just (Reduce 3 105)
    (271, Token (RPAREN _)) -> Just (Shift 268)
    (271, Token (COMMA _)) -> Just (Shift 114)
    (272, Token (RBRACKET _)) -> Just (Shift 270)
    (273, Token (WHERE _)) -> Just (Reduce 2 107)
    (273, Token (LBRACE _)) -> Just (Reduce 2 107)
    (273, Token (RBRACE _)) -> Just (Reduce 2 107)
    (273, Token (LPAREN _)) -> Just (Reduce 2 107)
    (273, Token (RPAREN _)) -> Just (Reduce 2 107)
    (273, Token (COMMA _)) -> Just (Reduce 2 107)
    (273, Token (SEMICOLON _)) -> Just (Reduce 2 107)
    (273, Token (EQUAL _)) -> Just (Reduce 2 107)
    (273, Token (DERIVING _)) -> Just (Reduce 2 107)
    (273, Token (DARROW _)) -> Just (Reduce 2 107)
    (273, Token (PIPE _)) -> Just (Reduce 2 107)
    (273, Token (COLON_COLON _)) -> Just (Reduce 2 107)
    (273, Token (INFIXL _)) -> Just (Reduce 2 107)
    (273, Token (INFIXR _)) -> Just (Reduce 2 107)
    (273, Token (INFIX _)) -> Just (Reduce 2 107)
    (273, Token (RARROW _)) -> Just (Reduce 2 107)
    (273, Token (LBRACKET _)) -> Just (Reduce 2 107)
    (273, Token (RBRACKET _)) -> Just (Reduce 2 107)
    (273, Token (EXCL _)) -> Just (Reduce 2 107)
    (273, Token (QCONID _)) -> Just (Reduce 2 107)
    (273, Token (EXPORT _)) -> Just (Reduce 2 107)
    (273, Token (AS _)) -> Just (Reduce 2 107)
    (273, Token (QVARID _)) -> Just (Reduce 2 107)
    (273, Token (INTEGER _)) -> Just (Reduce 2 107)
    (273, Token (QVARSYM _)) -> Just (Reduce 2 107)
    (273, Token (QCONSYM _)) -> Just (Reduce 2 107)
    (273, Token (BACKQUOTE _)) -> Just (Reduce 2 107)
    (274, Token (WHERE _)) -> Just (Reduce 1 102)
    (274, Token (LBRACE _)) -> Just (Reduce 1 102)
    (274, Token (RBRACE _)) -> Just (Reduce 1 102)
    (274, Token (LPAREN _)) -> Just (Reduce 1 102)
    (274, Token (RPAREN _)) -> Just (Reduce 1 102)
    (274, Token (COMMA _)) -> Just (Reduce 1 102)
    (274, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (274, Token (EQUAL _)) -> Just (Reduce 1 102)
    (274, Token (DERIVING _)) -> Just (Reduce 1 102)
    (274, Token (DARROW _)) -> Just (Reduce 1 102)
    (274, Token (PIPE _)) -> Just (Reduce 1 102)
    (274, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (274, Token (INFIXL _)) -> Just (Reduce 1 102)
    (274, Token (INFIXR _)) -> Just (Reduce 1 102)
    (274, Token (INFIX _)) -> Just (Reduce 1 102)
    (274, Token (RARROW _)) -> Just (Reduce 1 102)
    (274, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (274, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (274, Token (EXCL _)) -> Just (Reduce 1 102)
    (274, Token (QCONID _)) -> Just (Reduce 1 102)
    (274, Token (EXPORT _)) -> Just (Reduce 1 102)
    (274, Token (AS _)) -> Just (Reduce 1 102)
    (274, Token (QVARID _)) -> Just (Reduce 1 102)
    (274, Token (INTEGER _)) -> Just (Reduce 1 102)
    (274, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (274, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (274, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (275, Token (WHERE _)) -> Just (Reduce 1 103)
    (275, Token (LBRACE _)) -> Just (Reduce 1 103)
    (275, Token (RBRACE _)) -> Just (Reduce 1 103)
    (275, Token (LPAREN _)) -> Just (Reduce 1 103)
    (275, Token (RPAREN _)) -> Just (Reduce 1 103)
    (275, Token (COMMA _)) -> Just (Reduce 1 103)
    (275, Token (SEMICOLON _)) -> Just (Reduce 1 103)
    (275, Token (EQUAL _)) -> Just (Reduce 1 103)
    (275, Token (DERIVING _)) -> Just (Reduce 1 103)
    (275, Token (DARROW _)) -> Just (Reduce 1 103)
    (275, Token (PIPE _)) -> Just (Reduce 1 103)
    (275, Token (COLON_COLON _)) -> Just (Reduce 1 103)
    (275, Token (INFIXL _)) -> Just (Reduce 1 103)
    (275, Token (INFIXR _)) -> Just (Reduce 1 103)
    (275, Token (INFIX _)) -> Just (Reduce 1 103)
    (275, Token (RARROW _)) -> Just (Reduce 1 103)
    (275, Token (LBRACKET _)) -> Just (Reduce 1 103)
    (275, Token (RBRACKET _)) -> Just (Reduce 1 103)
    (275, Token (EXCL _)) -> Just (Reduce 1 103)
    (275, Token (QCONID _)) -> Just (Reduce 1 103)
    (275, Token (EXPORT _)) -> Just (Reduce 1 103)
    (275, Token (AS _)) -> Just (Reduce 1 103)
    (275, Token (QVARID _)) -> Just (Reduce 1 103)
    (275, Token (INTEGER _)) -> Just (Reduce 1 103)
    (275, Token (QVARSYM _)) -> Just (Reduce 1 103)
    (275, Token (QCONSYM _)) -> Just (Reduce 1 103)
    (275, Token (BACKQUOTE _)) -> Just (Reduce 1 103)
    (276, Token (RPAREN _)) -> Just (Shift 269)
    (277, Token (WHERE _)) -> Just (Reduce 2 111)
    (277, Token (LBRACE _)) -> Just (Reduce 2 111)
    (277, Token (RBRACE _)) -> Just (Reduce 2 111)
    (277, Token (LPAREN _)) -> Just (Reduce 2 111)
    (277, Token (RPAREN _)) -> Just (Reduce 2 111)
    (277, Token (COMMA _)) -> Just (Reduce 2 111)
    (277, Token (SEMICOLON _)) -> Just (Reduce 2 111)
    (277, Token (EQUAL _)) -> Just (Reduce 2 111)
    (277, Token (DERIVING _)) -> Just (Reduce 2 111)
    (277, Token (DARROW _)) -> Just (Reduce 2 111)
    (277, Token (PIPE _)) -> Just (Reduce 2 111)
    (277, Token (COLON_COLON _)) -> Just (Reduce 2 111)
    (277, Token (INFIXL _)) -> Just (Reduce 2 111)
    (277, Token (INFIXR _)) -> Just (Reduce 2 111)
    (277, Token (INFIX _)) -> Just (Reduce 2 111)
    (277, Token (RARROW _)) -> Just (Reduce 2 111)
    (277, Token (LBRACKET _)) -> Just (Reduce 2 111)
    (277, Token (RBRACKET _)) -> Just (Reduce 2 111)
    (277, Token (EXCL _)) -> Just (Reduce 2 111)
    (277, Token (QCONID _)) -> Just (Reduce 2 111)
    (277, Token (EXPORT _)) -> Just (Reduce 2 111)
    (277, Token (AS _)) -> Just (Reduce 2 111)
    (277, Token (QVARID _)) -> Just (Reduce 2 111)
    (277, Token (INTEGER _)) -> Just (Reduce 2 111)
    (277, Token (QVARSYM _)) -> Just (Reduce 2 111)
    (277, Token (QCONSYM _)) -> Just (Reduce 2 111)
    (277, Token (BACKQUOTE _)) -> Just (Reduce 2 111)
    (278, Token (WHERE _)) -> Just (Reduce 3 113)
    (278, Token (LBRACE _)) -> Just (Reduce 3 113)
    (278, Token (RBRACE _)) -> Just (Reduce 3 113)
    (278, Token (LPAREN _)) -> Just (Reduce 3 113)
    (278, Token (RPAREN _)) -> Just (Reduce 3 113)
    (278, Token (COMMA _)) -> Just (Reduce 3 113)
    (278, Token (SEMICOLON _)) -> Just (Reduce 3 113)
    (278, Token (EQUAL _)) -> Just (Reduce 3 113)
    (278, Token (DERIVING _)) -> Just (Reduce 3 113)
    (278, Token (DARROW _)) -> Just (Reduce 3 113)
    (278, Token (PIPE _)) -> Just (Reduce 3 113)
    (278, Token (COLON_COLON _)) -> Just (Reduce 3 113)
    (278, Token (INFIXL _)) -> Just (Reduce 3 113)
    (278, Token (INFIXR _)) -> Just (Reduce 3 113)
    (278, Token (INFIX _)) -> Just (Reduce 3 113)
    (278, Token (RARROW _)) -> Just (Reduce 3 113)
    (278, Token (LBRACKET _)) -> Just (Reduce 3 113)
    (278, Token (RBRACKET _)) -> Just (Reduce 3 113)
    (278, Token (EXCL _)) -> Just (Reduce 3 113)
    (278, Token (QCONID _)) -> Just (Reduce 3 113)
    (278, Token (EXPORT _)) -> Just (Reduce 3 113)
    (278, Token (AS _)) -> Just (Reduce 3 113)
    (278, Token (QVARID _)) -> Just (Reduce 3 113)
    (278, Token (INTEGER _)) -> Just (Reduce 3 113)
    (278, Token (QVARSYM _)) -> Just (Reduce 3 113)
    (278, Token (QCONSYM _)) -> Just (Reduce 3 113)
    (278, Token (BACKQUOTE _)) -> Just (Reduce 3 113)
    (279, Token (WHERE _)) -> Just (Reduce 3 114)
    (279, Token (LBRACE _)) -> Just (Reduce 3 114)
    (279, Token (RBRACE _)) -> Just (Reduce 3 114)
    (279, Token (LPAREN _)) -> Just (Reduce 3 114)
    (279, Token (RPAREN _)) -> Just (Reduce 3 114)
    (279, Token (COMMA _)) -> Just (Reduce 3 114)
    (279, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (279, Token (EQUAL _)) -> Just (Reduce 3 114)
    (279, Token (DERIVING _)) -> Just (Reduce 3 114)
    (279, Token (DARROW _)) -> Just (Reduce 3 114)
    (279, Token (PIPE _)) -> Just (Reduce 3 114)
    (279, Token (COLON_COLON _)) -> Just (Reduce 3 114)
    (279, Token (INFIXL _)) -> Just (Reduce 3 114)
    (279, Token (INFIXR _)) -> Just (Reduce 3 114)
    (279, Token (INFIX _)) -> Just (Reduce 3 114)
    (279, Token (RARROW _)) -> Just (Reduce 3 114)
    (279, Token (LBRACKET _)) -> Just (Reduce 3 114)
    (279, Token (RBRACKET _)) -> Just (Reduce 3 114)
    (279, Token (EXCL _)) -> Just (Reduce 3 114)
    (279, Token (QCONID _)) -> Just (Reduce 3 114)
    (279, Token (EXPORT _)) -> Just (Reduce 3 114)
    (279, Token (AS _)) -> Just (Reduce 3 114)
    (279, Token (QVARID _)) -> Just (Reduce 3 114)
    (279, Token (INTEGER _)) -> Just (Reduce 3 114)
    (279, Token (QVARSYM _)) -> Just (Reduce 3 114)
    (279, Token (QCONSYM _)) -> Just (Reduce 3 114)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 3 114)
    (280, Token (RPAREN _)) -> Just (Shift 278)
    (281, Token (WHERE _)) -> Just (Reduce 2 112)
    (281, Token (LBRACE _)) -> Just (Reduce 2 112)
    (281, Token (RBRACE _)) -> Just (Reduce 2 112)
    (281, Token (LPAREN _)) -> Just (Reduce 2 112)
    (281, Token (RPAREN _)) -> Just (Reduce 2 112)
    (281, Token (COMMA _)) -> Just (Reduce 2 112)
    (281, Token (SEMICOLON _)) -> Just (Reduce 2 112)
    (281, Token (EQUAL _)) -> Just (Reduce 2 112)
    (281, Token (DERIVING _)) -> Just (Reduce 2 112)
    (281, Token (DARROW _)) -> Just (Reduce 2 112)
    (281, Token (PIPE _)) -> Just (Reduce 2 112)
    (281, Token (COLON_COLON _)) -> Just (Reduce 2 112)
    (281, Token (INFIXL _)) -> Just (Reduce 2 112)
    (281, Token (INFIXR _)) -> Just (Reduce 2 112)
    (281, Token (INFIX _)) -> Just (Reduce 2 112)
    (281, Token (RARROW _)) -> Just (Reduce 2 112)
    (281, Token (LBRACKET _)) -> Just (Reduce 2 112)
    (281, Token (RBRACKET _)) -> Just (Reduce 2 112)
    (281, Token (EXCL _)) -> Just (Reduce 2 112)
    (281, Token (QCONID _)) -> Just (Reduce 2 112)
    (281, Token (EXPORT _)) -> Just (Reduce 2 112)
    (281, Token (AS _)) -> Just (Reduce 2 112)
    (281, Token (QVARID _)) -> Just (Reduce 2 112)
    (281, Token (INTEGER _)) -> Just (Reduce 2 112)
    (281, Token (QVARSYM _)) -> Just (Reduce 2 112)
    (281, Token (QCONSYM _)) -> Just (Reduce 2 112)
    (281, Token (BACKQUOTE _)) -> Just (Reduce 2 112)
    (282, Token (WHERE _)) -> Just (Reduce 1 110)
    (282, Token (LBRACE _)) -> Just (Reduce 1 110)
    (282, Token (RBRACE _)) -> Just (Reduce 1 110)
    (282, Token (LPAREN _)) -> Just (Reduce 1 110)
    (282, Token (RPAREN _)) -> Just (Reduce 1 110)
    (282, Token (COMMA _)) -> Just (Reduce 1 110)
    (282, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (282, Token (EQUAL _)) -> Just (Reduce 1 110)
    (282, Token (DERIVING _)) -> Just (Reduce 1 110)
    (282, Token (DARROW _)) -> Just (Reduce 1 110)
    (282, Token (PIPE _)) -> Just (Reduce 1 110)
    (282, Token (COLON_COLON _)) -> Just (Reduce 1 110)
    (282, Token (INFIXL _)) -> Just (Reduce 1 110)
    (282, Token (INFIXR _)) -> Just (Reduce 1 110)
    (282, Token (INFIX _)) -> Just (Reduce 1 110)
    (282, Token (RARROW _)) -> Just (Reduce 1 110)
    (282, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (282, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (282, Token (EXCL _)) -> Just (Reduce 1 110)
    (282, Token (QCONID _)) -> Just (Reduce 1 110)
    (282, Token (EXPORT _)) -> Just (Reduce 1 110)
    (282, Token (AS _)) -> Just (Reduce 1 110)
    (282, Token (QVARID _)) -> Just (Reduce 1 110)
    (282, Token (INTEGER _)) -> Just (Reduce 1 110)
    (282, Token (QVARSYM _)) -> Just (Reduce 1 110)
    (282, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (282, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (283, Token (LBRACE _)) -> Just (Shift 61)
    (283, Token (RBRACE _)) -> Just (Reduce 1 110)
    (283, Token (LPAREN _)) -> Just (Reduce 1 110)
    (283, Token (RPAREN _)) -> Just (Reduce 1 110)
    (283, Token (COMMA _)) -> Just (Reduce 1 110)
    (283, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (283, Token (DERIVING _)) -> Just (Reduce 1 110)
    (283, Token (PIPE _)) -> Just (Reduce 1 110)
    (283, Token (RARROW _)) -> Just (Reduce 1 110)
    (283, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (283, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (283, Token (EXCL _)) -> Just (Reduce 1 110)
    (283, Token (QCONID _)) -> Just (Reduce 1 110)
    (283, Token (EXPORT _)) -> Just (Reduce 1 110)
    (283, Token (AS _)) -> Just (Reduce 1 110)
    (283, Token (QVARID _)) -> Just (Reduce 1 110)
    (283, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (283, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (284, Token (RPAREN _)) -> Just (Shift 279)
    (285, Token (WHERE _)) -> Just (Reduce 1 181)
    (285, Token (LBRACE _)) -> Just (Reduce 1 181)
    (285, Token (RBRACE _)) -> Just (Reduce 1 181)
    (285, Token (LPAREN _)) -> Just (Reduce 1 181)
    (285, Token (RPAREN _)) -> Just (Reduce 1 181)
    (285, Token (COMMA _)) -> Just (Reduce 1 181)
    (285, Token (SEMICOLON _)) -> Just (Reduce 1 181)
    (285, Token (EQUAL _)) -> Just (Reduce 1 181)
    (285, Token (DERIVING _)) -> Just (Reduce 1 181)
    (285, Token (DARROW _)) -> Just (Reduce 1 181)
    (285, Token (PIPE _)) -> Just (Reduce 1 181)
    (285, Token (COLON_COLON _)) -> Just (Reduce 1 181)
    (285, Token (INFIXL _)) -> Just (Reduce 1 181)
    (285, Token (INFIXR _)) -> Just (Reduce 1 181)
    (285, Token (INFIX _)) -> Just (Reduce 1 181)
    (285, Token (RARROW _)) -> Just (Reduce 1 181)
    (285, Token (LBRACKET _)) -> Just (Reduce 1 181)
    (285, Token (RBRACKET _)) -> Just (Reduce 1 181)
    (285, Token (EXCL _)) -> Just (Reduce 1 181)
    (285, Token (QCONID _)) -> Just (Reduce 1 181)
    (285, Token (EXPORT _)) -> Just (Reduce 1 181)
    (285, Token (AS _)) -> Just (Reduce 1 181)
    (285, Token (QVARID _)) -> Just (Reduce 1 181)
    (285, Token (INTEGER _)) -> Just (Reduce 1 181)
    (285, Token (QVARSYM _)) -> Just (Reduce 1 181)
    (285, Token (QCONSYM _)) -> Just (Reduce 1 181)
    (285, Token (BACKQUOTE _)) -> Just (Reduce 1 181)
    (286, Token (WHERE _)) -> Just (Reduce 1 180)
    (286, Token (LBRACE _)) -> Just (Reduce 1 180)
    (286, Token (RBRACE _)) -> Just (Reduce 1 180)
    (286, Token (LPAREN _)) -> Just (Reduce 1 180)
    (286, Token (RPAREN _)) -> Just (Reduce 1 180)
    (286, Token (COMMA _)) -> Just (Reduce 1 180)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 180)
    (286, Token (EQUAL _)) -> Just (Reduce 1 180)
    (286, Token (DERIVING _)) -> Just (Reduce 1 180)
    (286, Token (DARROW _)) -> Just (Reduce 1 180)
    (286, Token (PIPE _)) -> Just (Reduce 1 180)
    (286, Token (COLON_COLON _)) -> Just (Reduce 1 180)
    (286, Token (INFIXL _)) -> Just (Reduce 1 180)
    (286, Token (INFIXR _)) -> Just (Reduce 1 180)
    (286, Token (INFIX _)) -> Just (Reduce 1 180)
    (286, Token (RARROW _)) -> Just (Reduce 1 180)
    (286, Token (LBRACKET _)) -> Just (Reduce 1 180)
    (286, Token (RBRACKET _)) -> Just (Reduce 1 180)
    (286, Token (EXCL _)) -> Just (Reduce 1 180)
    (286, Token (QCONID _)) -> Just (Reduce 1 180)
    (286, Token (EXPORT _)) -> Just (Reduce 1 180)
    (286, Token (AS _)) -> Just (Reduce 1 180)
    (286, Token (QVARID _)) -> Just (Reduce 1 180)
    (286, Token (INTEGER _)) -> Just (Reduce 1 180)
    (286, Token (QVARSYM _)) -> Just (Reduce 1 180)
    (286, Token (QCONSYM _)) -> Just (Reduce 1 180)
    (286, Token (BACKQUOTE _)) -> Just (Reduce 1 180)
    (287, Token (WHERE _)) -> Just (Reduce 1 182)
    (287, Token (LBRACE _)) -> Just (Reduce 1 182)
    (287, Token (RBRACE _)) -> Just (Reduce 1 182)
    (287, Token (LPAREN _)) -> Just (Reduce 1 182)
    (287, Token (RPAREN _)) -> Just (Reduce 1 182)
    (287, Token (COMMA _)) -> Just (Reduce 1 182)
    (287, Token (SEMICOLON _)) -> Just (Reduce 1 182)
    (287, Token (EQUAL _)) -> Just (Reduce 1 182)
    (287, Token (DERIVING _)) -> Just (Reduce 1 182)
    (287, Token (DARROW _)) -> Just (Reduce 1 182)
    (287, Token (PIPE _)) -> Just (Reduce 1 182)
    (287, Token (COLON_COLON _)) -> Just (Reduce 1 182)
    (287, Token (INFIXL _)) -> Just (Reduce 1 182)
    (287, Token (INFIXR _)) -> Just (Reduce 1 182)
    (287, Token (INFIX _)) -> Just (Reduce 1 182)
    (287, Token (RARROW _)) -> Just (Reduce 1 182)
    (287, Token (LBRACKET _)) -> Just (Reduce 1 182)
    (287, Token (RBRACKET _)) -> Just (Reduce 1 182)
    (287, Token (EXCL _)) -> Just (Reduce 1 182)
    (287, Token (QCONID _)) -> Just (Reduce 1 182)
    (287, Token (EXPORT _)) -> Just (Reduce 1 182)
    (287, Token (AS _)) -> Just (Reduce 1 182)
    (287, Token (QVARID _)) -> Just (Reduce 1 182)
    (287, Token (INTEGER _)) -> Just (Reduce 1 182)
    (287, Token (QVARSYM _)) -> Just (Reduce 1 182)
    (287, Token (QCONSYM _)) -> Just (Reduce 1 182)
    (287, Token (BACKQUOTE _)) -> Just (Reduce 1 182)
    (288, Token (RPAREN _)) -> Just (Reduce 3 108)
    (288, Token (COMMA _)) -> Just (Shift 114)
    (289, Token (RPAREN _)) -> Just (Reduce 3 109)
    (290, Token (RPAREN _)) -> Just (Reduce 1 115)
    (290, Token (COMMA _)) -> Just (Shift 290)
    (291, Token (RPAREN _)) -> Just (Reduce 2 116)
    (292, Token (RBRACE _)) -> Just (Reduce 3 120)
    (292, Token (SEMICOLON _)) -> Just (Reduce 3 120)
    (292, Token (DERIVING _)) -> Just (Reduce 3 120)
    (293, Token (RBRACE _)) -> Just (Reduce 1 119)
    (293, Token (SEMICOLON _)) -> Just (Reduce 1 119)
    (293, Token (DERIVING _)) -> Just (Reduce 1 119)
    (293, Token (PIPE _)) -> Just (Shift 88)
    (294, Token (RBRACE _)) -> Just (Reduce 3 123)
    (294, Token (SEMICOLON _)) -> Just (Reduce 3 123)
    (294, Token (DERIVING _)) -> Just (Reduce 3 123)
    (294, Token (PIPE _)) -> Just (Reduce 3 123)
    (295, Token (RBRACE _)) -> Just (Reduce 4 124)
    (295, Token (SEMICOLON _)) -> Just (Reduce 4 124)
    (295, Token (DERIVING _)) -> Just (Reduce 4 124)
    (295, Token (PIPE _)) -> Just (Reduce 4 124)
    (296, Token (RBRACE _)) -> Just (Shift 295)
    (297, Token (BACKQUOTE _)) -> Just (Shift 301)
    (298, Token (RBRACE _)) -> Just (Reduce 1 172)
    (298, Token (LPAREN _)) -> Just (Reduce 1 172)
    (298, Token (RPAREN _)) -> Just (Reduce 1 172)
    (298, Token (COMMA _)) -> Just (Reduce 1 172)
    (298, Token (SEMICOLON _)) -> Just (Reduce 1 172)
    (298, Token (RARROW _)) -> Just (Reduce 1 172)
    (298, Token (LBRACKET _)) -> Just (Reduce 1 172)
    (298, Token (RBRACKET _)) -> Just (Reduce 1 172)
    (298, Token (EXCL _)) -> Just (Reduce 1 172)
    (298, Token (QCONID _)) -> Just (Reduce 1 172)
    (298, Token (EXPORT _)) -> Just (Reduce 1 172)
    (298, Token (AS _)) -> Just (Reduce 1 172)
    (298, Token (QVARID _)) -> Just (Reduce 1 172)
    (298, Token (STRING _)) -> Just (Reduce 1 172)
    (298, Token (INTEGER _)) -> Just (Reduce 1 172)
    (298, Token (QVARSYM _)) -> Just (Reduce 1 172)
    (298, Token (QCONSYM _)) -> Just (Reduce 1 172)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 1 172)
    (299, Token (QCONID _)) -> Just (Shift 297)
    (299, Token (EXPORT _)) -> Just (Shift 327)
    (299, Token (AS _)) -> Just (Shift 328)
    (299, Token (QVARID _)) -> Just (Shift 329)
    (300, Token (QCONID _)) -> Just (Shift 297)
    (301, Token (RBRACE _)) -> Just (Reduce 3 173)
    (301, Token (LPAREN _)) -> Just (Reduce 3 173)
    (301, Token (RPAREN _)) -> Just (Reduce 3 173)
    (301, Token (COMMA _)) -> Just (Reduce 3 173)
    (301, Token (SEMICOLON _)) -> Just (Reduce 3 173)
    (301, Token (RARROW _)) -> Just (Reduce 3 173)
    (301, Token (LBRACKET _)) -> Just (Reduce 3 173)
    (301, Token (RBRACKET _)) -> Just (Reduce 3 173)
    (301, Token (EXCL _)) -> Just (Reduce 3 173)
    (301, Token (QCONID _)) -> Just (Reduce 3 173)
    (301, Token (EXPORT _)) -> Just (Reduce 3 173)
    (301, Token (AS _)) -> Just (Reduce 3 173)
    (301, Token (QVARID _)) -> Just (Reduce 3 173)
    (301, Token (STRING _)) -> Just (Reduce 3 173)
    (301, Token (INTEGER _)) -> Just (Reduce 3 173)
    (301, Token (QVARSYM _)) -> Just (Reduce 3 173)
    (301, Token (QCONSYM _)) -> Just (Reduce 3 173)
    (301, Token (BACKQUOTE _)) -> Just (Reduce 3 173)
    (302, Token (RBRACE _)) -> Just (Reduce 3 128)
    (303, Token (RBRACE _)) -> Just (Reduce 1 127)
    (303, Token (COMMA _)) -> Just (Shift 62)
    (304, Token (RBRACE _)) -> Just (Reduce 3 129)
    (304, Token (COMMA _)) -> Just (Reduce 3 129)
    (305, Token (COLON_COLON _)) -> Just (Shift 102)
    (306, Token (EXPORT _)) -> Just (Reduce 1 137)
    (306, Token (AS _)) -> Just (Reduce 1 137)
    (306, Token (QVARID _)) -> Just (Reduce 1 137)
    (306, Token (STRING _)) -> Just (Reduce 1 137)
    (307, Token (EXPORT _)) -> Just (Reduce 1 136)
    (307, Token (AS _)) -> Just (Reduce 1 136)
    (307, Token (QVARID _)) -> Just (Reduce 1 136)
    (307, Token (STRING _)) -> Just (Reduce 1 136)
    (308, Token (EXPORT _)) -> Just (Reduce 1 138)
    (308, Token (AS _)) -> Just (Reduce 1 138)
    (308, Token (QVARID _)) -> Just (Reduce 1 138)
    (308, Token (STRING _)) -> Just (Reduce 1 138)
    (309, Token (LPAREN _)) -> Just (Reduce 1 139)
    (309, Token (EXPORT _)) -> Just (Reduce 1 139)
    (309, Token (AS _)) -> Just (Reduce 1 139)
    (309, Token (QVARID _)) -> Just (Reduce 1 139)
    (309, Token (QVARSYM _)) -> Just (Reduce 1 139)
    (310, Token (STRING _)) -> Just (Reduce 1 142)
    (311, Token (STRING _)) -> Just (Reduce 1 141)
    (312, Token (STRING _)) -> Just (Reduce 1 143)
    (313, Token (LPAREN _)) -> Just (Reduce 1 140)
    (313, Token (EXPORT _)) -> Just (Reduce 1 140)
    (313, Token (AS _)) -> Just (Reduce 1 140)
    (313, Token (QVARID _)) -> Just (Reduce 1 140)
    (313, Token (QVARSYM _)) -> Just (Reduce 1 140)
    (314, Token (EQUAL _)) -> Just (Reduce 3 147)
    (315, Token (COMMA _)) -> Just (Shift 49)
    (315, Token (EQUAL _)) -> Just (Reduce 1 146)
    (316, Token (COMMA _)) -> Just (Reduce 1 148)
    (316, Token (EQUAL _)) -> Just (Reduce 1 148)
    (317, Token (WHERE _)) -> Just (Reduce 1 150)
    (317, Token (RBRACE _)) -> Just (Reduce 1 150)
    (317, Token (RPAREN _)) -> Just (Reduce 1 150)
    (317, Token (COMMA _)) -> Just (Reduce 1 150)
    (317, Token (SEMICOLON _)) -> Just (Reduce 1 150)
    (317, Token (EQUAL _)) -> Just (Reduce 1 150)
    (317, Token (PIPE _)) -> Just (Reduce 1 150)
    (318, Token (WHERE _)) -> Just (Reduce 1 152)
    (318, Token (RBRACE _)) -> Just (Reduce 1 152)
    (318, Token (LPAREN _)) -> Just (Reduce 1 152)
    (318, Token (RPAREN _)) -> Just (Reduce 1 152)
    (318, Token (COMMA _)) -> Just (Reduce 1 152)
    (318, Token (SEMICOLON _)) -> Just (Reduce 1 152)
    (318, Token (EQUAL _)) -> Just (Reduce 1 152)
    (318, Token (PIPE _)) -> Just (Reduce 1 152)
    (318, Token (QCONID _)) -> Just (Reduce 1 152)
    (318, Token (EXPORT _)) -> Just (Reduce 1 152)
    (318, Token (AS _)) -> Just (Reduce 1 152)
    (318, Token (QVARID _)) -> Just (Reduce 1 152)
    (318, Token (STRING _)) -> Just (Reduce 1 152)
    (318, Token (INTEGER _)) -> Just (Reduce 1 152)
    (318, Token (QVARSYM _)) -> Just (Reduce 1 152)
    (318, Token (QCONSYM _)) -> Just (Reduce 1 152)
    (318, Token (BACKQUOTE _)) -> Just (Reduce 1 152)
    (319, Token (WHERE _)) -> Just (Reduce 3 154)
    (319, Token (RBRACE _)) -> Just (Reduce 3 154)
    (319, Token (LPAREN _)) -> Just (Reduce 3 154)
    (319, Token (RPAREN _)) -> Just (Reduce 3 154)
    (319, Token (COMMA _)) -> Just (Reduce 3 154)
    (319, Token (SEMICOLON _)) -> Just (Reduce 3 154)
    (319, Token (EQUAL _)) -> Just (Reduce 3 154)
    (319, Token (PIPE _)) -> Just (Reduce 3 154)
    (319, Token (QCONID _)) -> Just (Reduce 3 154)
    (319, Token (EXPORT _)) -> Just (Reduce 3 154)
    (319, Token (AS _)) -> Just (Reduce 3 154)
    (319, Token (QVARID _)) -> Just (Reduce 3 154)
    (319, Token (STRING _)) -> Just (Reduce 3 154)
    (319, Token (INTEGER _)) -> Just (Reduce 3 154)
    (319, Token (QVARSYM _)) -> Just (Reduce 3 154)
    (319, Token (QCONSYM _)) -> Just (Reduce 3 154)
    (319, Token (BACKQUOTE _)) -> Just (Reduce 3 154)
    (320, Token (WHERE _)) -> Just (Reduce 2 153)
    (320, Token (RBRACE _)) -> Just (Reduce 2 153)
    (320, Token (LPAREN _)) -> Just (Reduce 2 153)
    (320, Token (RPAREN _)) -> Just (Reduce 2 153)
    (320, Token (COMMA _)) -> Just (Reduce 2 153)
    (320, Token (SEMICOLON _)) -> Just (Reduce 2 153)
    (320, Token (EQUAL _)) -> Just (Reduce 2 153)
    (320, Token (PIPE _)) -> Just (Reduce 2 153)
    (320, Token (QCONID _)) -> Just (Reduce 2 153)
    (320, Token (EXPORT _)) -> Just (Reduce 2 153)
    (320, Token (AS _)) -> Just (Reduce 2 153)
    (320, Token (QVARID _)) -> Just (Reduce 2 153)
    (320, Token (STRING _)) -> Just (Reduce 2 153)
    (320, Token (INTEGER _)) -> Just (Reduce 2 153)
    (320, Token (QVARSYM _)) -> Just (Reduce 2 153)
    (320, Token (QCONSYM _)) -> Just (Reduce 2 153)
    (320, Token (BACKQUOTE _)) -> Just (Reduce 2 153)
    (321, Token (WHERE _)) -> Just (Reduce 3 158)
    (321, Token (RBRACE _)) -> Just (Reduce 3 158)
    (321, Token (LPAREN _)) -> Just (Reduce 3 158)
    (321, Token (RPAREN _)) -> Just (Reduce 3 158)
    (321, Token (COMMA _)) -> Just (Reduce 3 158)
    (321, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (321, Token (EQUAL _)) -> Just (Reduce 3 158)
    (321, Token (PIPE _)) -> Just (Reduce 3 158)
    (321, Token (QCONID _)) -> Just (Reduce 3 158)
    (321, Token (EXPORT _)) -> Just (Reduce 3 158)
    (321, Token (AS _)) -> Just (Reduce 3 158)
    (321, Token (QVARID _)) -> Just (Reduce 3 158)
    (321, Token (STRING _)) -> Just (Reduce 3 158)
    (321, Token (INTEGER _)) -> Just (Reduce 3 158)
    (321, Token (QVARSYM _)) -> Just (Reduce 3 158)
    (321, Token (QCONSYM _)) -> Just (Reduce 3 158)
    (321, Token (BACKQUOTE _)) -> Just (Reduce 3 158)
    (322, Token (WHERE _)) -> Just (Reduce 1 157)
    (322, Token (RBRACE _)) -> Just (Reduce 1 157)
    (322, Token (LPAREN _)) -> Just (Reduce 1 157)
    (322, Token (RPAREN _)) -> Just (Reduce 1 157)
    (322, Token (COMMA _)) -> Just (Reduce 1 157)
    (322, Token (SEMICOLON _)) -> Just (Reduce 1 157)
    (322, Token (EQUAL _)) -> Just (Reduce 1 157)
    (322, Token (PIPE _)) -> Just (Reduce 1 157)
    (322, Token (QCONID _)) -> Just (Reduce 1 157)
    (322, Token (EXPORT _)) -> Just (Reduce 1 157)
    (322, Token (AS _)) -> Just (Reduce 1 157)
    (322, Token (QVARID _)) -> Just (Reduce 1 157)
    (322, Token (STRING _)) -> Just (Reduce 1 157)
    (322, Token (INTEGER _)) -> Just (Reduce 1 157)
    (322, Token (QVARSYM _)) -> Just (Reduce 1 157)
    (322, Token (QCONSYM _)) -> Just (Reduce 1 157)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 1 157)
    (323, Token (WHERE _)) -> Just (Reduce 1 156)
    (323, Token (RBRACE _)) -> Just (Reduce 1 156)
    (323, Token (LPAREN _)) -> Just (Reduce 1 156)
    (323, Token (RPAREN _)) -> Just (Reduce 1 156)
    (323, Token (COMMA _)) -> Just (Reduce 1 156)
    (323, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (323, Token (EQUAL _)) -> Just (Reduce 1 156)
    (323, Token (PIPE _)) -> Just (Reduce 1 156)
    (323, Token (QCONID _)) -> Just (Reduce 1 156)
    (323, Token (EXPORT _)) -> Just (Reduce 1 156)
    (323, Token (AS _)) -> Just (Reduce 1 156)
    (323, Token (QVARID _)) -> Just (Reduce 1 156)
    (323, Token (STRING _)) -> Just (Reduce 1 156)
    (323, Token (INTEGER _)) -> Just (Reduce 1 156)
    (323, Token (QVARSYM _)) -> Just (Reduce 1 156)
    (323, Token (QCONSYM _)) -> Just (Reduce 1 156)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 1 156)
    (324, Token (WHERE _)) -> Just (Reduce 1 155)
    (324, Token (RBRACE _)) -> Just (Reduce 1 155)
    (324, Token (LPAREN _)) -> Just (Reduce 1 155)
    (324, Token (RPAREN _)) -> Just (Reduce 1 155)
    (324, Token (COMMA _)) -> Just (Reduce 1 155)
    (324, Token (SEMICOLON _)) -> Just (Reduce 1 155)
    (324, Token (EQUAL _)) -> Just (Reduce 1 155)
    (324, Token (PIPE _)) -> Just (Reduce 1 155)
    (324, Token (QCONID _)) -> Just (Reduce 1 155)
    (324, Token (EXPORT _)) -> Just (Reduce 1 155)
    (324, Token (AS _)) -> Just (Reduce 1 155)
    (324, Token (QVARID _)) -> Just (Reduce 1 155)
    (324, Token (STRING _)) -> Just (Reduce 1 155)
    (324, Token (INTEGER _)) -> Just (Reduce 1 155)
    (324, Token (QVARSYM _)) -> Just (Reduce 1 155)
    (324, Token (QCONSYM _)) -> Just (Reduce 1 155)
    (324, Token (BACKQUOTE _)) -> Just (Reduce 1 155)
    (325, Token (RPAREN _)) -> Just (Shift 321)
    (326, Token (LPAREN _)) -> Just (Reduce 1 161)
    (326, Token (EQUAL _)) -> Just (Reduce 1 161)
    (326, Token (PIPE _)) -> Just (Reduce 1 161)
    (326, Token (EXPORT _)) -> Just (Reduce 1 161)
    (326, Token (AS _)) -> Just (Reduce 1 161)
    (326, Token (QVARID _)) -> Just (Reduce 1 161)
    (326, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (327, Token (BACKQUOTE _)) -> Just (Shift 331)
    (328, Token (BACKQUOTE _)) -> Just (Shift 332)
    (329, Token (BACKQUOTE _)) -> Just (Shift 333)
    (330, Token (RBRACE _)) -> Just (Reduce 1 168)
    (330, Token (LPAREN _)) -> Just (Reduce 1 168)
    (330, Token (COMMA _)) -> Just (Reduce 1 168)
    (330, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (330, Token (QCONID _)) -> Just (Reduce 1 168)
    (330, Token (EXPORT _)) -> Just (Reduce 1 168)
    (330, Token (AS _)) -> Just (Reduce 1 168)
    (330, Token (QVARID _)) -> Just (Reduce 1 168)
    (330, Token (STRING _)) -> Just (Reduce 1 168)
    (330, Token (INTEGER _)) -> Just (Reduce 1 168)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (330, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (330, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (331, Token (RBRACE _)) -> Just (Reduce 3 170)
    (331, Token (LPAREN _)) -> Just (Reduce 3 170)
    (331, Token (COMMA _)) -> Just (Reduce 3 170)
    (331, Token (SEMICOLON _)) -> Just (Reduce 3 170)
    (331, Token (QCONID _)) -> Just (Reduce 3 170)
    (331, Token (EXPORT _)) -> Just (Reduce 3 170)
    (331, Token (AS _)) -> Just (Reduce 3 170)
    (331, Token (QVARID _)) -> Just (Reduce 3 170)
    (331, Token (STRING _)) -> Just (Reduce 3 170)
    (331, Token (INTEGER _)) -> Just (Reduce 3 170)
    (331, Token (QVARSYM _)) -> Just (Reduce 3 170)
    (331, Token (QCONSYM _)) -> Just (Reduce 3 170)
    (331, Token (BACKQUOTE _)) -> Just (Reduce 3 170)
    (332, Token (RBRACE _)) -> Just (Reduce 3 169)
    (332, Token (LPAREN _)) -> Just (Reduce 3 169)
    (332, Token (COMMA _)) -> Just (Reduce 3 169)
    (332, Token (SEMICOLON _)) -> Just (Reduce 3 169)
    (332, Token (QCONID _)) -> Just (Reduce 3 169)
    (332, Token (EXPORT _)) -> Just (Reduce 3 169)
    (332, Token (AS _)) -> Just (Reduce 3 169)
    (332, Token (QVARID _)) -> Just (Reduce 3 169)
    (332, Token (STRING _)) -> Just (Reduce 3 169)
    (332, Token (INTEGER _)) -> Just (Reduce 3 169)
    (332, Token (QVARSYM _)) -> Just (Reduce 3 169)
    (332, Token (QCONSYM _)) -> Just (Reduce 3 169)
    (332, Token (BACKQUOTE _)) -> Just (Reduce 3 169)
    (333, Token (RBRACE _)) -> Just (Reduce 3 171)
    (333, Token (LPAREN _)) -> Just (Reduce 3 171)
    (333, Token (COMMA _)) -> Just (Reduce 3 171)
    (333, Token (SEMICOLON _)) -> Just (Reduce 3 171)
    (333, Token (QCONID _)) -> Just (Reduce 3 171)
    (333, Token (EXPORT _)) -> Just (Reduce 3 171)
    (333, Token (AS _)) -> Just (Reduce 3 171)
    (333, Token (QVARID _)) -> Just (Reduce 3 171)
    (333, Token (STRING _)) -> Just (Reduce 3 171)
    (333, Token (INTEGER _)) -> Just (Reduce 3 171)
    (333, Token (QVARSYM _)) -> Just (Reduce 3 171)
    (333, Token (QCONSYM _)) -> Just (Reduce 3 171)
    (333, Token (BACKQUOTE _)) -> Just (Reduce 3 171)
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
production 91 = 40
production 92 = 40
production 93 = 41
production 94 = 41
production 95 = 41
production 96 = 25
production 97 = 25
production 98 = 18
production 99 = 18
production 100 = 17
production 101 = 17
production 102 = 45
production 103 = 45
production 104 = 45
production 105 = 45
production 106 = 45
production 107 = 45
production 108 = 48
production 109 = 48
production 110 = 46
production 111 = 46
production 112 = 46
production 113 = 46
production 114 = 46
production 115 = 49
production 116 = 49
production 117 = 19
production 118 = 19
production 119 = 50
production 120 = 50
production 121 = 51
production 122 = 51
production 123 = 51
production 124 = 51
production 125 = 22
production 126 = 22
production 127 = 53
production 128 = 53
production 129 = 54
production 130 = 21
production 131 = 21
production 132 = 20
production 133 = 26
production 134 = 26
production 135 = 26
production 136 = 55
production 137 = 55
production 138 = 55
production 139 = 56
production 140 = 58
production 141 = 57
production 142 = 57
production 143 = 57
production 144 = 33
production 145 = 33
production 146 = 59
production 147 = 59
production 148 = 60
production 149 = 32
production 150 = 61
production 151 = 62
production 152 = 63
production 153 = 63
production 154 = 63
production 155 = 64
production 156 = 64
production 157 = 64
production 158 = 64
production 159 = 31
production 160 = 31
production 161 = 65
production 162 = 8
production 163 = 8
production 164 = 8
production 165 = 8
production 166 = 9
production 167 = 9
production 168 = 66
production 169 = 66
production 170 = 66
production 171 = 66
production 172 = 52
production 173 = 52
production 174 = 44
production 175 = 44
production 176 = 16
production 177 = 16
production 178 = 15
production 179 = 15
production 180 = 47
production 181 = 47
production 182 = 47
production 183 = 67
production 184 = 1
production 185 = 42
production 186 = 42

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 157
    (9, 1) -> Just 182
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 257
    (13, 14) -> Just 18
    (13, 27) -> Just 180
    (13, 30) -> Just 216
    (13, 31) -> Just 50
    (13, 40) -> Just 229
    (13, 41) -> Just 230
    (13, 65) -> Just 233
    (16, 4) -> Just 17
    (16, 8) -> Just 257
    (16, 14) -> Just 18
    (16, 27) -> Just 180
    (16, 30) -> Just 216
    (16, 31) -> Just 50
    (16, 40) -> Just 229
    (16, 41) -> Just 230
    (16, 65) -> Just 233
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 131
    (25, 9) -> Just 132
    (25, 10) -> Just 33
    (25, 13) -> Just 121
    (34, 8) -> Just 324
    (34, 44) -> Just 35
    (34, 52) -> Just 266
    (34, 64) -> Just 320
    (34, 66) -> Just 267
    (35, 8) -> Just 324
    (35, 64) -> Just 319
    (36, 8) -> Just 324
    (36, 32) -> Just 236
    (36, 61) -> Just 235
    (36, 62) -> Just 317
    (36, 63) -> Just 34
    (36, 64) -> Just 318
    (37, 8) -> Just 324
    (37, 32) -> Just 217
    (37, 61) -> Just 235
    (37, 62) -> Just 317
    (37, 63) -> Just 34
    (37, 64) -> Just 318
    (38, 8) -> Just 324
    (38, 32) -> Just 246
    (38, 61) -> Just 235
    (38, 62) -> Just 317
    (38, 63) -> Just 34
    (38, 64) -> Just 318
    (39, 8) -> Just 324
    (39, 32) -> Just 254
    (39, 61) -> Just 235
    (39, 62) -> Just 317
    (39, 63) -> Just 34
    (39, 64) -> Just 318
    (40, 8) -> Just 324
    (40, 32) -> Just 325
    (40, 61) -> Just 235
    (40, 62) -> Just 317
    (40, 63) -> Just 34
    (40, 64) -> Just 318
    (41, 8) -> Just 257
    (41, 27) -> Just 225
    (41, 29) -> Just 224
    (41, 30) -> Just 216
    (41, 31) -> Just 50
    (41, 40) -> Just 229
    (41, 41) -> Just 230
    (41, 65) -> Just 233
    (42, 8) -> Just 257
    (42, 27) -> Just 225
    (42, 29) -> Just 226
    (42, 30) -> Just 216
    (42, 31) -> Just 50
    (42, 40) -> Just 229
    (42, 41) -> Just 230
    (42, 65) -> Just 233
    (43, 8) -> Just 257
    (43, 30) -> Just 245
    (43, 31) -> Just 53
    (43, 35) -> Just 240
    (43, 36) -> Just 242
    (43, 40) -> Just 229
    (43, 41) -> Just 230
    (43, 65) -> Just 233
    (44, 8) -> Just 257
    (44, 30) -> Just 245
    (44, 31) -> Just 53
    (44, 35) -> Just 241
    (44, 36) -> Just 242
    (44, 40) -> Just 229
    (44, 41) -> Just 230
    (44, 65) -> Just 233
    (45, 8) -> Just 324
    (45, 33) -> Just 218
    (45, 59) -> Just 238
    (45, 60) -> Just 315
    (45, 61) -> Just 316
    (45, 62) -> Just 317
    (45, 63) -> Just 34
    (45, 64) -> Just 318
    (46, 8) -> Just 324
    (46, 33) -> Just 237
    (46, 59) -> Just 238
    (46, 60) -> Just 315
    (46, 61) -> Just 316
    (46, 62) -> Just 317
    (46, 63) -> Just 34
    (46, 64) -> Just 318
    (47, 8) -> Just 324
    (47, 33) -> Just 247
    (47, 59) -> Just 238
    (47, 60) -> Just 315
    (47, 61) -> Just 316
    (47, 62) -> Just 317
    (47, 63) -> Just 34
    (47, 64) -> Just 318
    (48, 8) -> Just 324
    (48, 33) -> Just 255
    (48, 59) -> Just 238
    (48, 60) -> Just 315
    (48, 61) -> Just 316
    (48, 62) -> Just 317
    (48, 63) -> Just 34
    (48, 64) -> Just 318
    (49, 8) -> Just 324
    (49, 59) -> Just 314
    (49, 60) -> Just 315
    (49, 61) -> Just 316
    (49, 62) -> Just 317
    (49, 63) -> Just 34
    (49, 64) -> Just 318
    (50, 8) -> Just 326
    (50, 65) -> Just 234
    (51, 8) -> Just 326
    (51, 31) -> Just 54
    (51, 38) -> Just 249
    (51, 39) -> Just 251
    (51, 65) -> Just 233
    (52, 8) -> Just 326
    (52, 31) -> Just 54
    (52, 38) -> Just 250
    (52, 39) -> Just 251
    (52, 65) -> Just 233
    (53, 8) -> Just 326
    (53, 65) -> Just 234
    (54, 8) -> Just 326
    (54, 65) -> Just 234
    (55, 8) -> Just 128
    (55, 9) -> Just 129
    (55, 11) -> Just 122
    (55, 12) -> Just 123
    (56, 8) -> Just 128
    (56, 9) -> Just 129
    (56, 11) -> Just 158
    (56, 12) -> Just 123
    (57, 8) -> Just 128
    (57, 9) -> Just 129
    (57, 11) -> Just 159
    (57, 12) -> Just 123
    (58, 8) -> Just 131
    (58, 9) -> Just 132
    (58, 10) -> Just 120
    (58, 13) -> Just 121
    (59, 8) -> Just 131
    (59, 9) -> Just 132
    (59, 10) -> Just 130
    (59, 13) -> Just 121
    (60, 8) -> Just 256
    (60, 40) -> Just 258
    (61, 8) -> Just 256
    (61, 40) -> Just 305
    (61, 53) -> Just 296
    (61, 54) -> Just 303
    (62, 8) -> Just 256
    (62, 40) -> Just 305
    (62, 53) -> Just 302
    (62, 54) -> Just 303
    (63, 8) -> Just 192
    (64, 8) -> Just 203
    (65, 8) -> Just 204
    (66, 8) -> Just 205
    (74, 9) -> Just 282
    (74, 45) -> Just 273
    (74, 46) -> Just 274
    (74, 47) -> Just 275
    (75, 9) -> Just 282
    (75, 17) -> Just 76
    (75, 45) -> Just 183
    (75, 46) -> Just 274
    (75, 47) -> Just 275
    (76, 9) -> Just 282
    (76, 23) -> Just 175
    (76, 45) -> Just 184
    (76, 46) -> Just 274
    (76, 47) -> Just 275
    (77, 9) -> Just 282
    (77, 17) -> Just 78
    (77, 45) -> Just 183
    (77, 46) -> Just 274
    (77, 47) -> Just 275
    (78, 9) -> Just 282
    (78, 24) -> Just 177
    (78, 45) -> Just 184
    (78, 46) -> Just 274
    (78, 47) -> Just 275
    (79, 9) -> Just 282
    (79, 17) -> Just 80
    (79, 45) -> Just 183
    (79, 46) -> Just 274
    (79, 47) -> Just 275
    (80, 9) -> Just 282
    (80, 23) -> Just 174
    (80, 45) -> Just 184
    (80, 46) -> Just 274
    (80, 47) -> Just 275
    (81, 9) -> Just 282
    (81, 17) -> Just 82
    (81, 45) -> Just 183
    (81, 46) -> Just 274
    (81, 47) -> Just 275
    (82, 9) -> Just 282
    (82, 24) -> Just 176
    (82, 45) -> Just 184
    (82, 46) -> Just 274
    (82, 47) -> Just 275
    (83, 9) -> Just 282
    (83, 17) -> Just 84
    (83, 45) -> Just 183
    (83, 46) -> Just 274
    (83, 47) -> Just 275
    (84, 9) -> Just 282
    (84, 19) -> Just 162
    (84, 45) -> Just 184
    (84, 46) -> Just 274
    (84, 47) -> Just 275
    (85, 9) -> Just 282
    (85, 17) -> Just 86
    (85, 45) -> Just 183
    (85, 46) -> Just 274
    (85, 47) -> Just 275
    (86, 9) -> Just 282
    (86, 19) -> Just 163
    (86, 45) -> Just 184
    (86, 46) -> Just 274
    (86, 47) -> Just 275
    (87, 9) -> Just 283
    (87, 17) -> Just 90
    (87, 45) -> Just 183
    (87, 46) -> Just 274
    (87, 47) -> Just 275
    (87, 50) -> Just 186
    (87, 51) -> Just 293
    (88, 9) -> Just 283
    (88, 17) -> Just 90
    (88, 45) -> Just 183
    (88, 46) -> Just 274
    (88, 47) -> Just 275
    (88, 50) -> Just 292
    (88, 51) -> Just 293
    (89, 9) -> Just 104
    (90, 9) -> Just 282
    (90, 45) -> Just 184
    (90, 46) -> Just 274
    (90, 47) -> Just 275
    (90, 52) -> Just 91
    (91, 9) -> Just 282
    (91, 17) -> Just 92
    (91, 45) -> Just 183
    (91, 46) -> Just 274
    (91, 47) -> Just 275
    (92, 9) -> Just 282
    (92, 45) -> Just 184
    (92, 46) -> Just 274
    (92, 47) -> Just 275
    (93, 9) -> Just 282
    (93, 17) -> Just 94
    (93, 18) -> Just 228
    (93, 45) -> Just 183
    (93, 46) -> Just 274
    (93, 47) -> Just 275
    (94, 9) -> Just 282
    (94, 45) -> Just 184
    (94, 46) -> Just 274
    (94, 47) -> Just 275
    (95, 9) -> Just 282
    (95, 17) -> Just 101
    (95, 18) -> Just 161
    (95, 45) -> Just 183
    (95, 46) -> Just 274
    (95, 47) -> Just 275
    (96, 9) -> Just 282
    (96, 17) -> Just 101
    (96, 18) -> Just 185
    (96, 45) -> Just 183
    (96, 46) -> Just 274
    (96, 47) -> Just 275
    (97, 9) -> Just 282
    (97, 17) -> Just 101
    (97, 18) -> Just 206
    (97, 45) -> Just 183
    (97, 46) -> Just 274
    (97, 47) -> Just 275
    (98, 9) -> Just 282
    (98, 17) -> Just 101
    (98, 18) -> Just 207
    (98, 45) -> Just 183
    (98, 46) -> Just 274
    (98, 47) -> Just 275
    (99, 9) -> Just 282
    (99, 17) -> Just 101
    (99, 18) -> Just 208
    (99, 45) -> Just 183
    (99, 46) -> Just 274
    (99, 47) -> Just 275
    (100, 9) -> Just 282
    (100, 17) -> Just 101
    (100, 18) -> Just 227
    (100, 45) -> Just 183
    (100, 46) -> Just 274
    (100, 47) -> Just 275
    (101, 9) -> Just 282
    (101, 45) -> Just 184
    (101, 46) -> Just 274
    (101, 47) -> Just 275
    (102, 9) -> Just 282
    (102, 17) -> Just 101
    (102, 18) -> Just 304
    (102, 45) -> Just 183
    (102, 46) -> Just 274
    (102, 47) -> Just 275
    (103, 9) -> Just 282
    (103, 17) -> Just 101
    (103, 18) -> Just 193
    (103, 45) -> Just 183
    (103, 46) -> Just 274
    (103, 47) -> Just 275
    (104, 9) -> Just 282
    (104, 45) -> Just 194
    (104, 46) -> Just 274
    (104, 47) -> Just 275
    (105, 9) -> Just 282
    (105, 17) -> Just 106
    (105, 45) -> Just 183
    (105, 46) -> Just 274
    (105, 47) -> Just 275
    (106, 9) -> Just 282
    (106, 22) -> Just 173
    (106, 45) -> Just 184
    (106, 46) -> Just 274
    (106, 47) -> Just 275
    (107, 9) -> Just 282
    (107, 17) -> Just 109
    (107, 45) -> Just 183
    (107, 46) -> Just 274
    (107, 47) -> Just 275
    (108, 9) -> Just 282
    (108, 17) -> Just 110
    (108, 45) -> Just 183
    (108, 46) -> Just 274
    (108, 47) -> Just 275
    (109, 9) -> Just 282
    (109, 45) -> Just 184
    (109, 46) -> Just 274
    (109, 47) -> Just 275
    (110, 9) -> Just 282
    (110, 22) -> Just 172
    (110, 45) -> Just 184
    (110, 46) -> Just 274
    (110, 47) -> Just 275
    (111, 9) -> Just 282
    (111, 17) -> Just 101
    (111, 18) -> Just 271
    (111, 45) -> Just 183
    (111, 46) -> Just 274
    (111, 47) -> Just 275
    (111, 48) -> Just 276
    (111, 49) -> Just 284
    (112, 9) -> Just 282
    (112, 17) -> Just 101
    (112, 18) -> Just 199
    (112, 25) -> Just 178
    (112, 45) -> Just 183
    (112, 46) -> Just 274
    (112, 47) -> Just 275
    (113, 9) -> Just 282
    (113, 17) -> Just 101
    (113, 18) -> Just 199
    (113, 25) -> Just 200
    (113, 45) -> Just 183
    (113, 46) -> Just 274
    (113, 47) -> Just 275
    (114, 9) -> Just 282
    (114, 17) -> Just 101
    (114, 18) -> Just 288
    (114, 45) -> Just 183
    (114, 46) -> Just 274
    (114, 47) -> Just 275
    (114, 48) -> Just 289
    (115, 9) -> Just 282
    (115, 17) -> Just 101
    (115, 18) -> Just 272
    (115, 45) -> Just 183
    (115, 46) -> Just 274
    (115, 47) -> Just 275
    (133, 20) -> Just 189
    (133, 21) -> Just 168
    (134, 20) -> Just 189
    (134, 21) -> Just 169
    (135, 20) -> Just 189
    (135, 21) -> Just 170
    (136, 20) -> Just 189
    (136, 21) -> Just 171
    (149, 15) -> Just 8
    (151, 20) -> Just 164
    (152, 20) -> Just 165
    (153, 20) -> Just 166
    (154, 20) -> Just 167
    (156, 26) -> Just 179
    (157, 16) -> Just 160
    (187, 20) -> Just 189
    (187, 21) -> Just 190
    (195, 34) -> Just 196
    (197, 37) -> Just 198
    (201, 55) -> Just 209
    (202, 55) -> Just 210
    (209, 56) -> Just 64
    (209, 57) -> Just 211
    (210, 58) -> Just 66
    (211, 56) -> Just 65
    (212, 28) -> Just 214
    (213, 28) -> Just 215
    (219, 28) -> Just 243
    (220, 28) -> Just 244
    (221, 28) -> Just 252
    (222, 28) -> Just 253
    (230, 42) -> Just 231
    (231, 43) -> Just 232
    (231, 44) -> Just 265
    (231, 52) -> Just 266
    (231, 66) -> Just 267
    (263, 43) -> Just 264
    (263, 44) -> Just 265
    (263, 52) -> Just 266
    (263, 66) -> Just 267
    (290, 49) -> Just 291
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
                      Monad.liftM StackValue_ops $ ops_implies_op actions (case snd (pop !! 0) of { StackValue_op value -> value; _ -> undefined })
                    90 ->
                      Monad.liftM StackValue_ops $ ops_implies_op_COMMA_ops actions (case snd (pop !! 2) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_ops value -> value; _ -> undefined })
                    91 ->
                      Monad.liftM StackValue_vars $ vars_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    92 ->
                      Monad.liftM StackValue_vars $ vars_implies_var_COMMA_vars actions (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_vars value -> value; _ -> undefined })
                    93 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXL actions (case snd (pop !! 0) of { StackValue_INFIXL value -> value; _ -> undefined })
                    94 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIXR actions (case snd (pop !! 0) of { StackValue_INFIXR value -> value; _ -> undefined })
                    95 ->
                      Monad.liftM StackValue_fixity $ fixity_implies_INFIX actions (case snd (pop !! 0) of { StackValue_INFIX value -> value; _ -> undefined })
                    96 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type' actions (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    97 ->
                      Monad.liftM StackValue_type_seq $ type_seq_implies_type'_COMMA_type_seq actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq value -> value; _ -> undefined })
                    98 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    99 ->
                      Monad.liftM StackValue_type' $ type'_implies_btype_RARROW_type' actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    100 ->
                      Monad.liftM StackValue_btype $ btype_implies_atype actions (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    101 ->
                      Monad.liftM StackValue_btype $ btype_implies_btype_atype actions (case snd (pop !! 1) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    102 ->
                      Monad.liftM StackValue_atype $ atype_implies_gtycon actions (case snd (pop !! 0) of { StackValue_gtycon value -> value; _ -> undefined })
                    103 ->
                      Monad.liftM StackValue_atype $ atype_implies_tyvar actions (case snd (pop !! 0) of { StackValue_tyvar value -> value; _ -> undefined })
                    104 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type_seq2_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type_seq2 value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    105 ->
                      Monad.liftM StackValue_atype $ atype_implies_LBRACKET_type'_RBRACKET actions (case snd (pop !! 2) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    106 ->
                      Monad.liftM StackValue_atype $ atype_implies_LPAREN_type'_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    107 ->
                      Monad.liftM StackValue_atype $ atype_implies_EXCL_atype actions (case snd (pop !! 1) of { StackValue_EXCL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    108 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type' actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    109 ->
                      Monad.liftM StackValue_type_seq2 $ type_seq2_implies_type'_COMMA_type_seq2 actions (case snd (pop !! 2) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type_seq2 value -> value; _ -> undefined })
                    110 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_con actions (case snd (pop !! 0) of { StackValue_con value -> value; _ -> undefined })
                    111 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RPAREN actions (case snd (pop !! 1) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    112 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LBRACKET_RBRACKET actions (case snd (pop !! 1) of { StackValue_LBRACKET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACKET value -> value; _ -> undefined })
                    113 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_RARROW_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_RARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    114 ->
                      Monad.liftM StackValue_gtycon $ gtycon_implies_LPAREN_comma_list_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_comma_list value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    115 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA actions (case snd (pop !! 0) of { StackValue_COMMA value -> value; _ -> undefined })
                    116 ->
                      Monad.liftM StackValue_comma_list $ comma_list_implies_COMMA_comma_list actions (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_comma_list value -> value; _ -> undefined })
                    117 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies actions
                    118 ->
                      Monad.liftM StackValue_constrs_opt $ constrs_opt_implies_EQUAL_constrs actions (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    119 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr actions (case snd (pop !! 0) of { StackValue_constr value -> value; _ -> undefined })
                    120 ->
                      Monad.liftM StackValue_constrs $ constrs_implies_constr_PIPE_constrs actions (case snd (pop !! 2) of { StackValue_constr value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_constrs value -> value; _ -> undefined })
                    121 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype actions (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    122 ->
                      Monad.liftM StackValue_constr $ constr_implies_btype_conop_btype actions (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_conop value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_btype value -> value; _ -> undefined })
                    123 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_RBRACE actions (case snd (pop !! 2) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    124 ->
                      Monad.liftM StackValue_constr $ constr_implies_con_LBRACE_fielddecl_seq_RBRACE actions (case snd (pop !! 3) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_fielddecl_seq value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    125 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_atype actions (case snd (pop !! 2) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_atype value -> value; _ -> undefined })
                    126 ->
                      Monad.liftM StackValue_newconstr $ newconstr_implies_EQUAL_con_LBRACE_var_COLON_COLON_type'_RBRACE actions (case snd (pop !! 6) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_con value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_LBRACE value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_type' value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RBRACE value -> value; _ -> undefined })
                    127 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl actions (case snd (pop !! 0) of { StackValue_fielddecl value -> value; _ -> undefined })
                    128 ->
                      Monad.liftM StackValue_fielddecl_seq $ fielddecl_seq_implies_fielddecl_COMMA_fielddecl_seq actions (case snd (pop !! 2) of { StackValue_fielddecl value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_fielddecl_seq value -> value; _ -> undefined })
                    129 ->
                      Monad.liftM StackValue_fielddecl $ fielddecl_implies_vars_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_vars value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    130 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass actions (case snd (pop !! 0) of { StackValue_dclass value -> value; _ -> undefined })
                    131 ->
                      Monad.liftM StackValue_dclass_seq $ dclass_seq_implies_dclass_COMMA_dclass_seq actions (case snd (pop !! 2) of { StackValue_dclass value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_dclass_seq value -> value; _ -> undefined })
                    132 ->
                      Monad.liftM StackValue_dclass $ dclass_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    133 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_impent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    134 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_IMPORT_callconv_safety_impent_var_COLON_COLON_type' actions (case snd (pop !! 6) of { StackValue_IMPORT value -> value; _ -> undefined }) (case snd (pop !! 5) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_safety value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_impent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    135 ->
                      Monad.liftM StackValue_fdecl $ fdecl_implies_EXPORT_callconv_expent_var_COLON_COLON_type' actions (case snd (pop !! 5) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 4) of { StackValue_callconv value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_expent value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_var value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    136 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    137 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    138 ->
                      Monad.liftM StackValue_callconv $ callconv_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    139 ->
                      Monad.liftM StackValue_impent $ impent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    140 ->
                      Monad.liftM StackValue_expent $ expent_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    141 ->
                      Monad.liftM StackValue_safety $ safety_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    142 ->
                      Monad.liftM StackValue_safety $ safety_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    143 ->
                      Monad.liftM StackValue_safety $ safety_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    144 ->
                      Monad.liftM StackValue_gdrhs $ gdrhs_implies_guards_EQUAL_exp actions (case snd (pop !! 2) of { StackValue_guards value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    145 ->
                      Monad.liftM StackValue_gdrhs $ gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs actions (case snd (pop !! 4) of { StackValue_guards value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_EQUAL value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_PIPE value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_gdrhs value -> value; _ -> undefined })
                    146 ->
                      Monad.liftM StackValue_guards $ guards_implies_guard actions (case snd (pop !! 0) of { StackValue_guard value -> value; _ -> undefined })
                    147 ->
                      Monad.liftM StackValue_guards $ guards_implies_guard_COMMA_guards actions (case snd (pop !! 2) of { StackValue_guard value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COMMA value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_guards value -> value; _ -> undefined })
                    148 ->
                      Monad.liftM StackValue_guard $ guard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    177 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    179 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    186 ->
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
  , gdrhs_implies_guards_EQUAL_exp = \guards0 eQUAL1 exp2 ->
      return $ Gdrhs_implies_guards_EQUAL_exp guards0 eQUAL1 exp2
  , gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs = \guards0 eQUAL1 exp2 pIPE3 gdrhs4 ->
      return $ Gdrhs_implies_guards_EQUAL_exp_PIPE_gdrhs guards0 eQUAL1 exp2 pIPE3 gdrhs4
  , guards_implies_guard = \guard0 ->
      return $ Guards_implies_guard guard0
  , guards_implies_guard_COMMA_guards = \guard0 cOMMA1 guards2 ->
      return $ Guards_implies_guard_COMMA_guards guard0 cOMMA1 guards2
  , guard_implies_infixexp = \infixexp0 ->
      return $ Guard_implies_infixexp infixexp0
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
  , aexp_implies_STRING = \sTRING0 ->
      return $ Aexp_implies_STRING sTRING0
  , aexp_implies_LPAREN_exp_RPAREN = \lPAREN0 exp1 rPAREN2 ->
      return $ Aexp_implies_LPAREN_exp_RPAREN lPAREN0 exp1 rPAREN2
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

