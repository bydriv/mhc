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
  | LARROW LARROW
  | LBRACE LBRACE
  | LBRACKET LBRACKET
  | LET LET
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
    Guard_implies_infixexp_LARROW_exp Infixexp LARROW Exp
  | Guard_implies_LET_decls LET Decls
  | Guard_implies_infixexp Infixexp
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
  | StackValue_LARROW LARROW
  | StackValue_LET LET
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
  , guard_implies_infixexp_LARROW_exp :: Infixexp -> LARROW -> Exp -> m Guard
  , guard_implies_LET_decls :: LET -> Decls -> m Guard
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
    (11, Token (MODULE _)) -> Just (Reduce 1 186)
    (11, Token (WHERE _)) -> Just (Reduce 1 186)
    (11, Token (RBRACE _)) -> Just (Reduce 1 186)
    (11, Token (LPAREN _)) -> Just (Reduce 1 186)
    (11, Token (RPAREN _)) -> Just (Reduce 1 186)
    (11, Token (COMMA _)) -> Just (Reduce 1 186)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 186)
    (11, Token (HIDING _)) -> Just (Reduce 1 186)
    (11, Token (QCONID _)) -> Just (Reduce 1 186)
    (11, Token (EXPORT _)) -> Just (Reduce 1 186)
    (11, Token (AS _)) -> Just (Reduce 1 186)
    (11, Token (QVARID _)) -> Just (Reduce 1 186)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 186)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 186)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 68)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 150)
    (13, Token (TYPE _)) -> Just (Shift 108)
    (13, Token (DATA _)) -> Just (Shift 84)
    (13, Token (NEWTYPE _)) -> Just (Shift 106)
    (13, Token (CLASS _)) -> Just (Shift 76)
    (13, Token (INSTANCE _)) -> Just (Shift 78)
    (13, Token (DEFAULT _)) -> Just (Shift 156)
    (13, Token (FOREIGN _)) -> Just (Shift 157)
    (13, Token (INFIXL _)) -> Just (Shift 261)
    (13, Token (INFIXR _)) -> Just (Shift 262)
    (13, Token (INFIX _)) -> Just (Shift 263)
    (13, Token (EXPORT _)) -> Just (Shift 71)
    (13, Token (AS _)) -> Just (Shift 72)
    (13, Token (QVARID _)) -> Just (Shift 73)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 68)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 150)
    (16, Token (TYPE _)) -> Just (Shift 108)
    (16, Token (DATA _)) -> Just (Shift 84)
    (16, Token (NEWTYPE _)) -> Just (Shift 106)
    (16, Token (CLASS _)) -> Just (Shift 76)
    (16, Token (INSTANCE _)) -> Just (Shift 78)
    (16, Token (DEFAULT _)) -> Just (Shift 156)
    (16, Token (FOREIGN _)) -> Just (Shift 157)
    (16, Token (INFIXL _)) -> Just (Shift 261)
    (16, Token (INFIXR _)) -> Just (Shift 262)
    (16, Token (INFIX _)) -> Just (Shift 263)
    (16, Token (EXPORT _)) -> Just (Shift 71)
    (16, Token (AS _)) -> Just (Shift 72)
    (16, Token (QVARID _)) -> Just (Shift 73)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 69)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 119)
    (19, Token (EXPORT _)) -> Just (Shift 71)
    (19, Token (AS _)) -> Just (Shift 72)
    (19, Token (QVARID _)) -> Just (Shift 73)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 69)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 119)
    (22, Token (EXPORT _)) -> Just (Shift 71)
    (22, Token (AS _)) -> Just (Shift 72)
    (22, Token (QVARID _)) -> Just (Shift 73)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 69)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 119)
    (25, Token (EXPORT _)) -> Just (Shift 71)
    (25, Token (AS _)) -> Just (Shift 72)
    (25, Token (QVARID _)) -> Just (Shift 73)
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
    (34, Token (WHERE _)) -> Just (Reduce 1 153)
    (34, Token (RBRACE _)) -> Just (Reduce 1 153)
    (34, Token (LPAREN _)) -> Just (Shift 40)
    (34, Token (RPAREN _)) -> Just (Reduce 1 153)
    (34, Token (COMMA _)) -> Just (Reduce 1 153)
    (34, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (34, Token (EQUAL _)) -> Just (Reduce 1 153)
    (34, Token (PIPE _)) -> Just (Reduce 1 153)
    (34, Token (EXPORT _)) -> Just (Shift 71)
    (34, Token (AS _)) -> Just (Shift 72)
    (34, Token (QVARID _)) -> Just (Shift 73)
    (34, Token (STRING _)) -> Just (Shift 326)
    (34, Token (LARROW _)) -> Just (Reduce 1 153)
    (34, Token (INTEGER _)) -> Just (Shift 327)
    (34, Token (QVARSYM _)) -> Just (Shift 334)
    (34, Token (QCONSYM _)) -> Just (Shift 300)
    (34, Token (BACKQUOTE _)) -> Just (Shift 301)
    (35, Token (LPAREN _)) -> Just (Shift 40)
    (35, Token (EXPORT _)) -> Just (Shift 71)
    (35, Token (AS _)) -> Just (Shift 72)
    (35, Token (QVARID _)) -> Just (Shift 73)
    (35, Token (STRING _)) -> Just (Shift 326)
    (35, Token (INTEGER _)) -> Just (Shift 327)
    (36, Token (LPAREN _)) -> Just (Shift 40)
    (36, Token (EXPORT _)) -> Just (Shift 71)
    (36, Token (AS _)) -> Just (Shift 72)
    (36, Token (QVARID _)) -> Just (Shift 73)
    (36, Token (STRING _)) -> Just (Shift 326)
    (36, Token (INTEGER _)) -> Just (Shift 327)
    (37, Token (LPAREN _)) -> Just (Shift 40)
    (37, Token (EXPORT _)) -> Just (Shift 71)
    (37, Token (AS _)) -> Just (Shift 72)
    (37, Token (QVARID _)) -> Just (Shift 73)
    (37, Token (STRING _)) -> Just (Shift 326)
    (37, Token (INTEGER _)) -> Just (Shift 327)
    (38, Token (LPAREN _)) -> Just (Shift 40)
    (38, Token (EXPORT _)) -> Just (Shift 71)
    (38, Token (AS _)) -> Just (Shift 72)
    (38, Token (QVARID _)) -> Just (Shift 73)
    (38, Token (STRING _)) -> Just (Shift 326)
    (38, Token (INTEGER _)) -> Just (Shift 327)
    (39, Token (LPAREN _)) -> Just (Shift 40)
    (39, Token (EXPORT _)) -> Just (Shift 71)
    (39, Token (AS _)) -> Just (Shift 72)
    (39, Token (QVARID _)) -> Just (Shift 73)
    (39, Token (STRING _)) -> Just (Shift 326)
    (39, Token (INTEGER _)) -> Just (Shift 327)
    (40, Token (LPAREN _)) -> Just (Shift 40)
    (40, Token (EXPORT _)) -> Just (Shift 71)
    (40, Token (AS _)) -> Just (Shift 72)
    (40, Token (QVARID _)) -> Just (Shift 73)
    (40, Token (STRING _)) -> Just (Shift 326)
    (40, Token (INTEGER _)) -> Just (Shift 327)
    (40, Token (QVARSYM _)) -> Just (Shift 74)
    (41, Token (RBRACE _)) -> Just (Reduce 0 85)
    (41, Token (LPAREN _)) -> Just (Shift 68)
    (41, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (41, Token (INFIXL _)) -> Just (Shift 261)
    (41, Token (INFIXR _)) -> Just (Shift 262)
    (41, Token (INFIX _)) -> Just (Shift 263)
    (41, Token (EXPORT _)) -> Just (Shift 71)
    (41, Token (AS _)) -> Just (Shift 72)
    (41, Token (QVARID _)) -> Just (Shift 73)
    (42, Token (RBRACE _)) -> Just (Reduce 0 85)
    (42, Token (LPAREN _)) -> Just (Shift 68)
    (42, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (42, Token (INFIXL _)) -> Just (Shift 261)
    (42, Token (INFIXR _)) -> Just (Shift 262)
    (42, Token (INFIX _)) -> Just (Shift 263)
    (42, Token (EXPORT _)) -> Just (Shift 71)
    (42, Token (AS _)) -> Just (Shift 72)
    (42, Token (QVARID _)) -> Just (Shift 73)
    (43, Token (RBRACE _)) -> Just (Reduce 0 85)
    (43, Token (LPAREN _)) -> Just (Shift 68)
    (43, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (43, Token (INFIXL _)) -> Just (Shift 261)
    (43, Token (INFIXR _)) -> Just (Shift 262)
    (43, Token (INFIX _)) -> Just (Shift 263)
    (43, Token (EXPORT _)) -> Just (Shift 71)
    (43, Token (AS _)) -> Just (Shift 72)
    (43, Token (QVARID _)) -> Just (Shift 73)
    (44, Token (RBRACE _)) -> Just (Reduce 0 85)
    (44, Token (LPAREN _)) -> Just (Shift 68)
    (44, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (44, Token (INFIXL _)) -> Just (Shift 261)
    (44, Token (INFIXR _)) -> Just (Shift 262)
    (44, Token (INFIX _)) -> Just (Shift 263)
    (44, Token (EXPORT _)) -> Just (Shift 71)
    (44, Token (AS _)) -> Just (Shift 72)
    (44, Token (QVARID _)) -> Just (Shift 73)
    (45, Token (LPAREN _)) -> Just (Shift 40)
    (45, Token (EXPORT _)) -> Just (Shift 71)
    (45, Token (AS _)) -> Just (Shift 72)
    (45, Token (QVARID _)) -> Just (Shift 73)
    (45, Token (STRING _)) -> Just (Shift 326)
    (45, Token (LET _)) -> Just (Shift 224)
    (45, Token (INTEGER _)) -> Just (Shift 327)
    (46, Token (LPAREN _)) -> Just (Shift 40)
    (46, Token (EXPORT _)) -> Just (Shift 71)
    (46, Token (AS _)) -> Just (Shift 72)
    (46, Token (QVARID _)) -> Just (Shift 73)
    (46, Token (STRING _)) -> Just (Shift 326)
    (46, Token (LET _)) -> Just (Shift 224)
    (46, Token (INTEGER _)) -> Just (Shift 327)
    (47, Token (LPAREN _)) -> Just (Shift 40)
    (47, Token (EXPORT _)) -> Just (Shift 71)
    (47, Token (AS _)) -> Just (Shift 72)
    (47, Token (QVARID _)) -> Just (Shift 73)
    (47, Token (STRING _)) -> Just (Shift 326)
    (47, Token (LET _)) -> Just (Shift 224)
    (47, Token (INTEGER _)) -> Just (Shift 327)
    (48, Token (LPAREN _)) -> Just (Shift 40)
    (48, Token (EXPORT _)) -> Just (Shift 71)
    (48, Token (AS _)) -> Just (Shift 72)
    (48, Token (QVARID _)) -> Just (Shift 73)
    (48, Token (STRING _)) -> Just (Shift 326)
    (48, Token (LET _)) -> Just (Shift 224)
    (48, Token (INTEGER _)) -> Just (Shift 327)
    (49, Token (LPAREN _)) -> Just (Shift 40)
    (49, Token (EXPORT _)) -> Just (Shift 71)
    (49, Token (AS _)) -> Just (Shift 72)
    (49, Token (QVARID _)) -> Just (Shift 73)
    (49, Token (STRING _)) -> Just (Shift 326)
    (49, Token (LET _)) -> Just (Shift 224)
    (49, Token (INTEGER _)) -> Just (Shift 327)
    (50, Token (LPAREN _)) -> Just (Shift 40)
    (50, Token (EXPORT _)) -> Just (Shift 71)
    (50, Token (AS _)) -> Just (Shift 72)
    (50, Token (QVARID _)) -> Just (Shift 73)
    (50, Token (STRING _)) -> Just (Shift 326)
    (50, Token (INTEGER _)) -> Just (Shift 327)
    (51, Token (LPAREN _)) -> Just (Shift 68)
    (51, Token (EQUAL _)) -> Just (Shift 37)
    (51, Token (PIPE _)) -> Just (Shift 45)
    (51, Token (EXPORT _)) -> Just (Shift 71)
    (51, Token (AS _)) -> Just (Shift 72)
    (51, Token (QVARID _)) -> Just (Shift 73)
    (52, Token (RBRACE _)) -> Just (Reduce 0 80)
    (52, Token (LPAREN _)) -> Just (Shift 68)
    (52, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (52, Token (EXPORT _)) -> Just (Shift 71)
    (52, Token (AS _)) -> Just (Shift 72)
    (52, Token (QVARID _)) -> Just (Shift 73)
    (53, Token (RBRACE _)) -> Just (Reduce 0 80)
    (53, Token (LPAREN _)) -> Just (Shift 68)
    (53, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (53, Token (EXPORT _)) -> Just (Shift 71)
    (53, Token (AS _)) -> Just (Shift 72)
    (53, Token (QVARID _)) -> Just (Shift 73)
    (54, Token (LPAREN _)) -> Just (Shift 68)
    (54, Token (EQUAL _)) -> Just (Shift 38)
    (54, Token (PIPE _)) -> Just (Shift 47)
    (54, Token (EXPORT _)) -> Just (Shift 71)
    (54, Token (AS _)) -> Just (Shift 72)
    (54, Token (QVARID _)) -> Just (Shift 73)
    (55, Token (LPAREN _)) -> Just (Shift 68)
    (55, Token (EQUAL _)) -> Just (Shift 39)
    (55, Token (PIPE _)) -> Just (Shift 48)
    (55, Token (EXPORT _)) -> Just (Shift 71)
    (55, Token (AS _)) -> Just (Shift 72)
    (55, Token (QVARID _)) -> Just (Shift 73)
    (56, Token (LPAREN _)) -> Just (Shift 69)
    (56, Token (RPAREN _)) -> Just (Reduce 0 15)
    (56, Token (QCONID _)) -> Just (Shift 119)
    (56, Token (EXPORT _)) -> Just (Shift 71)
    (56, Token (AS _)) -> Just (Shift 72)
    (56, Token (QVARID _)) -> Just (Shift 73)
    (57, Token (LPAREN _)) -> Just (Shift 69)
    (57, Token (RPAREN _)) -> Just (Reduce 0 15)
    (57, Token (QCONID _)) -> Just (Shift 119)
    (57, Token (EXPORT _)) -> Just (Shift 71)
    (57, Token (AS _)) -> Just (Shift 72)
    (57, Token (QVARID _)) -> Just (Shift 73)
    (58, Token (LPAREN _)) -> Just (Shift 69)
    (58, Token (RPAREN _)) -> Just (Reduce 0 15)
    (58, Token (QCONID _)) -> Just (Shift 119)
    (58, Token (EXPORT _)) -> Just (Shift 71)
    (58, Token (AS _)) -> Just (Shift 72)
    (58, Token (QVARID _)) -> Just (Shift 73)
    (59, Token (LPAREN _)) -> Just (Shift 69)
    (59, Token (QCONID _)) -> Just (Shift 119)
    (59, Token (EXPORT _)) -> Just (Shift 71)
    (59, Token (AS _)) -> Just (Shift 72)
    (59, Token (QVARID _)) -> Just (Shift 73)
    (60, Token (LPAREN _)) -> Just (Shift 69)
    (60, Token (RPAREN _)) -> Just (Shift 125)
    (60, Token (DOT_DOT _)) -> Just (Shift 128)
    (60, Token (QCONID _)) -> Just (Shift 119)
    (60, Token (EXPORT _)) -> Just (Shift 71)
    (60, Token (AS _)) -> Just (Shift 72)
    (60, Token (QVARID _)) -> Just (Shift 73)
    (61, Token (LPAREN _)) -> Just (Shift 68)
    (61, Token (EXPORT _)) -> Just (Shift 71)
    (61, Token (AS _)) -> Just (Shift 72)
    (61, Token (QVARID _)) -> Just (Shift 73)
    (62, Token (RBRACE _)) -> Just (Shift 296)
    (62, Token (LPAREN _)) -> Just (Shift 68)
    (62, Token (EXPORT _)) -> Just (Shift 71)
    (62, Token (AS _)) -> Just (Shift 72)
    (62, Token (QVARID _)) -> Just (Shift 73)
    (63, Token (LPAREN _)) -> Just (Shift 68)
    (63, Token (EXPORT _)) -> Just (Shift 71)
    (63, Token (AS _)) -> Just (Shift 72)
    (63, Token (QVARID _)) -> Just (Shift 73)
    (64, Token (LPAREN _)) -> Just (Shift 68)
    (64, Token (EXPORT _)) -> Just (Shift 71)
    (64, Token (AS _)) -> Just (Shift 72)
    (64, Token (QVARID _)) -> Just (Shift 73)
    (65, Token (LPAREN _)) -> Just (Shift 68)
    (65, Token (EXPORT _)) -> Just (Shift 71)
    (65, Token (AS _)) -> Just (Shift 72)
    (65, Token (QVARID _)) -> Just (Shift 73)
    (66, Token (LPAREN _)) -> Just (Shift 68)
    (66, Token (EXPORT _)) -> Just (Shift 71)
    (66, Token (AS _)) -> Just (Shift 72)
    (66, Token (QVARID _)) -> Just (Shift 73)
    (67, Token (LPAREN _)) -> Just (Shift 68)
    (67, Token (EXPORT _)) -> Just (Shift 71)
    (67, Token (AS _)) -> Just (Shift 72)
    (67, Token (QVARID _)) -> Just (Shift 73)
    (68, Token (QVARSYM _)) -> Just (Shift 74)
    (69, Token (QVARSYM _)) -> Just (Shift 74)
    (69, Token (QCONSYM _)) -> Just (Shift 120)
    (70, Token (WHERE _)) -> Just (Reduce 3 167)
    (70, Token (RBRACE _)) -> Just (Reduce 3 167)
    (70, Token (LPAREN _)) -> Just (Reduce 3 167)
    (70, Token (RPAREN _)) -> Just (Reduce 3 167)
    (70, Token (COMMA _)) -> Just (Reduce 3 167)
    (70, Token (SEMICOLON _)) -> Just (Reduce 3 167)
    (70, Token (EQUAL _)) -> Just (Reduce 3 167)
    (70, Token (PIPE _)) -> Just (Reduce 3 167)
    (70, Token (COLON_COLON _)) -> Just (Reduce 3 167)
    (70, Token (QCONID _)) -> Just (Reduce 3 167)
    (70, Token (EXPORT _)) -> Just (Reduce 3 167)
    (70, Token (AS _)) -> Just (Reduce 3 167)
    (70, Token (QVARID _)) -> Just (Reduce 3 167)
    (70, Token (STRING _)) -> Just (Reduce 3 167)
    (70, Token (LARROW _)) -> Just (Reduce 3 167)
    (70, Token (INTEGER _)) -> Just (Reduce 3 167)
    (70, Token (QVARSYM _)) -> Just (Reduce 3 167)
    (70, Token (QCONSYM _)) -> Just (Reduce 3 167)
    (70, Token (BACKQUOTE _)) -> Just (Reduce 3 167)
    (71, Token (WHERE _)) -> Just (Reduce 1 165)
    (71, Token (RBRACE _)) -> Just (Reduce 1 165)
    (71, Token (LPAREN _)) -> Just (Reduce 1 165)
    (71, Token (RPAREN _)) -> Just (Reduce 1 165)
    (71, Token (COMMA _)) -> Just (Reduce 1 165)
    (71, Token (SEMICOLON _)) -> Just (Reduce 1 165)
    (71, Token (EQUAL _)) -> Just (Reduce 1 165)
    (71, Token (PIPE _)) -> Just (Reduce 1 165)
    (71, Token (COLON_COLON _)) -> Just (Reduce 1 165)
    (71, Token (QCONID _)) -> Just (Reduce 1 165)
    (71, Token (EXPORT _)) -> Just (Reduce 1 165)
    (71, Token (AS _)) -> Just (Reduce 1 165)
    (71, Token (QVARID _)) -> Just (Reduce 1 165)
    (71, Token (STRING _)) -> Just (Reduce 1 165)
    (71, Token (LARROW _)) -> Just (Reduce 1 165)
    (71, Token (INTEGER _)) -> Just (Reduce 1 165)
    (71, Token (QVARSYM _)) -> Just (Reduce 1 165)
    (71, Token (QCONSYM _)) -> Just (Reduce 1 165)
    (71, Token (BACKQUOTE _)) -> Just (Reduce 1 165)
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
    (72, Token (LARROW _)) -> Just (Reduce 1 164)
    (72, Token (INTEGER _)) -> Just (Reduce 1 164)
    (72, Token (QVARSYM _)) -> Just (Reduce 1 164)
    (72, Token (QCONSYM _)) -> Just (Reduce 1 164)
    (72, Token (BACKQUOTE _)) -> Just (Reduce 1 164)
    (73, Token (WHERE _)) -> Just (Reduce 1 166)
    (73, Token (RBRACE _)) -> Just (Reduce 1 166)
    (73, Token (LPAREN _)) -> Just (Reduce 1 166)
    (73, Token (RPAREN _)) -> Just (Reduce 1 166)
    (73, Token (COMMA _)) -> Just (Reduce 1 166)
    (73, Token (SEMICOLON _)) -> Just (Reduce 1 166)
    (73, Token (EQUAL _)) -> Just (Reduce 1 166)
    (73, Token (PIPE _)) -> Just (Reduce 1 166)
    (73, Token (COLON_COLON _)) -> Just (Reduce 1 166)
    (73, Token (QCONID _)) -> Just (Reduce 1 166)
    (73, Token (EXPORT _)) -> Just (Reduce 1 166)
    (73, Token (AS _)) -> Just (Reduce 1 166)
    (73, Token (QVARID _)) -> Just (Reduce 1 166)
    (73, Token (STRING _)) -> Just (Reduce 1 166)
    (73, Token (LARROW _)) -> Just (Reduce 1 166)
    (73, Token (INTEGER _)) -> Just (Reduce 1 166)
    (73, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (73, Token (QCONSYM _)) -> Just (Reduce 1 166)
    (73, Token (BACKQUOTE _)) -> Just (Reduce 1 166)
    (74, Token (RPAREN _)) -> Just (Shift 70)
    (75, Token (LPAREN _)) -> Just (Shift 112)
    (75, Token (LBRACKET _)) -> Just (Shift 116)
    (75, Token (EXCL _)) -> Just (Shift 75)
    (75, Token (QCONID _)) -> Just (Shift 119)
    (75, Token (EXPORT _)) -> Just (Shift 287)
    (75, Token (AS _)) -> Just (Shift 288)
    (75, Token (QVARID _)) -> Just (Shift 289)
    (76, Token (LPAREN _)) -> Just (Shift 112)
    (76, Token (LBRACKET _)) -> Just (Shift 116)
    (76, Token (EXCL _)) -> Just (Shift 75)
    (76, Token (QCONID _)) -> Just (Shift 119)
    (76, Token (EXPORT _)) -> Just (Shift 287)
    (76, Token (AS _)) -> Just (Shift 288)
    (76, Token (QVARID _)) -> Just (Shift 289)
    (77, Token (WHERE _)) -> Just (Shift 196)
    (77, Token (RBRACE _)) -> Just (Reduce 0 65)
    (77, Token (LPAREN _)) -> Just (Shift 112)
    (77, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (77, Token (DARROW _)) -> Just (Shift 80)
    (77, Token (LBRACKET _)) -> Just (Shift 116)
    (77, Token (EXCL _)) -> Just (Shift 75)
    (77, Token (QCONID _)) -> Just (Shift 119)
    (77, Token (EXPORT _)) -> Just (Shift 287)
    (77, Token (AS _)) -> Just (Shift 288)
    (77, Token (QVARID _)) -> Just (Shift 289)
    (78, Token (LPAREN _)) -> Just (Shift 112)
    (78, Token (LBRACKET _)) -> Just (Shift 116)
    (78, Token (EXCL _)) -> Just (Shift 75)
    (78, Token (QCONID _)) -> Just (Shift 119)
    (78, Token (EXPORT _)) -> Just (Shift 287)
    (78, Token (AS _)) -> Just (Shift 288)
    (78, Token (QVARID _)) -> Just (Shift 289)
    (79, Token (WHERE _)) -> Just (Shift 198)
    (79, Token (RBRACE _)) -> Just (Reduce 0 75)
    (79, Token (LPAREN _)) -> Just (Shift 112)
    (79, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (79, Token (DARROW _)) -> Just (Shift 82)
    (79, Token (LBRACKET _)) -> Just (Shift 116)
    (79, Token (EXCL _)) -> Just (Shift 75)
    (79, Token (QCONID _)) -> Just (Shift 119)
    (79, Token (EXPORT _)) -> Just (Shift 287)
    (79, Token (AS _)) -> Just (Shift 288)
    (79, Token (QVARID _)) -> Just (Shift 289)
    (80, Token (LPAREN _)) -> Just (Shift 112)
    (80, Token (LBRACKET _)) -> Just (Shift 116)
    (80, Token (EXCL _)) -> Just (Shift 75)
    (80, Token (QCONID _)) -> Just (Shift 119)
    (80, Token (EXPORT _)) -> Just (Shift 287)
    (80, Token (AS _)) -> Just (Shift 288)
    (80, Token (QVARID _)) -> Just (Shift 289)
    (81, Token (WHERE _)) -> Just (Shift 196)
    (81, Token (RBRACE _)) -> Just (Reduce 0 65)
    (81, Token (LPAREN _)) -> Just (Shift 112)
    (81, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (81, Token (LBRACKET _)) -> Just (Shift 116)
    (81, Token (EXCL _)) -> Just (Shift 75)
    (81, Token (QCONID _)) -> Just (Shift 119)
    (81, Token (EXPORT _)) -> Just (Shift 287)
    (81, Token (AS _)) -> Just (Shift 288)
    (81, Token (QVARID _)) -> Just (Shift 289)
    (82, Token (LPAREN _)) -> Just (Shift 112)
    (82, Token (LBRACKET _)) -> Just (Shift 116)
    (82, Token (EXCL _)) -> Just (Shift 75)
    (82, Token (QCONID _)) -> Just (Shift 119)
    (82, Token (EXPORT _)) -> Just (Shift 287)
    (82, Token (AS _)) -> Just (Shift 288)
    (82, Token (QVARID _)) -> Just (Shift 289)
    (83, Token (WHERE _)) -> Just (Shift 198)
    (83, Token (RBRACE _)) -> Just (Reduce 0 75)
    (83, Token (LPAREN _)) -> Just (Shift 112)
    (83, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (83, Token (LBRACKET _)) -> Just (Shift 116)
    (83, Token (EXCL _)) -> Just (Shift 75)
    (83, Token (QCONID _)) -> Just (Shift 119)
    (83, Token (EXPORT _)) -> Just (Shift 287)
    (83, Token (AS _)) -> Just (Shift 288)
    (83, Token (QVARID _)) -> Just (Shift 289)
    (84, Token (LPAREN _)) -> Just (Shift 112)
    (84, Token (LBRACKET _)) -> Just (Shift 116)
    (84, Token (EXCL _)) -> Just (Shift 75)
    (84, Token (QCONID _)) -> Just (Shift 119)
    (84, Token (EXPORT _)) -> Just (Shift 287)
    (84, Token (AS _)) -> Just (Shift 288)
    (84, Token (QVARID _)) -> Just (Shift 289)
    (85, Token (RBRACE _)) -> Just (Reduce 0 117)
    (85, Token (LPAREN _)) -> Just (Shift 112)
    (85, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (85, Token (EQUAL _)) -> Just (Shift 88)
    (85, Token (DERIVING _)) -> Just (Reduce 0 117)
    (85, Token (DARROW _)) -> Just (Shift 86)
    (85, Token (LBRACKET _)) -> Just (Shift 116)
    (85, Token (EXCL _)) -> Just (Shift 75)
    (85, Token (QCONID _)) -> Just (Shift 119)
    (85, Token (EXPORT _)) -> Just (Shift 287)
    (85, Token (AS _)) -> Just (Shift 288)
    (85, Token (QVARID _)) -> Just (Shift 289)
    (86, Token (LPAREN _)) -> Just (Shift 112)
    (86, Token (LBRACKET _)) -> Just (Shift 116)
    (86, Token (EXCL _)) -> Just (Shift 75)
    (86, Token (QCONID _)) -> Just (Shift 119)
    (86, Token (EXPORT _)) -> Just (Shift 287)
    (86, Token (AS _)) -> Just (Shift 288)
    (86, Token (QVARID _)) -> Just (Shift 289)
    (87, Token (RBRACE _)) -> Just (Reduce 0 117)
    (87, Token (LPAREN _)) -> Just (Shift 112)
    (87, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (87, Token (EQUAL _)) -> Just (Shift 88)
    (87, Token (DERIVING _)) -> Just (Reduce 0 117)
    (87, Token (LBRACKET _)) -> Just (Shift 116)
    (87, Token (EXCL _)) -> Just (Shift 75)
    (87, Token (QCONID _)) -> Just (Shift 119)
    (87, Token (EXPORT _)) -> Just (Shift 287)
    (87, Token (AS _)) -> Just (Shift 288)
    (87, Token (QVARID _)) -> Just (Shift 289)
    (88, Token (LPAREN _)) -> Just (Shift 112)
    (88, Token (LBRACKET _)) -> Just (Shift 116)
    (88, Token (EXCL _)) -> Just (Shift 75)
    (88, Token (QCONID _)) -> Just (Shift 119)
    (88, Token (EXPORT _)) -> Just (Shift 287)
    (88, Token (AS _)) -> Just (Shift 288)
    (88, Token (QVARID _)) -> Just (Shift 289)
    (89, Token (LPAREN _)) -> Just (Shift 112)
    (89, Token (LBRACKET _)) -> Just (Shift 116)
    (89, Token (EXCL _)) -> Just (Shift 75)
    (89, Token (QCONID _)) -> Just (Shift 119)
    (89, Token (EXPORT _)) -> Just (Shift 287)
    (89, Token (AS _)) -> Just (Shift 288)
    (89, Token (QVARID _)) -> Just (Shift 289)
    (90, Token (LPAREN _)) -> Just (Shift 117)
    (90, Token (QCONID _)) -> Just (Shift 119)
    (91, Token (RBRACE _)) -> Just (Reduce 1 121)
    (91, Token (LPAREN _)) -> Just (Shift 112)
    (91, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (91, Token (DERIVING _)) -> Just (Reduce 1 121)
    (91, Token (PIPE _)) -> Just (Reduce 1 121)
    (91, Token (LBRACKET _)) -> Just (Shift 116)
    (91, Token (EXCL _)) -> Just (Shift 75)
    (91, Token (QCONID _)) -> Just (Shift 119)
    (91, Token (EXPORT _)) -> Just (Shift 287)
    (91, Token (AS _)) -> Just (Shift 288)
    (91, Token (QVARID _)) -> Just (Shift 289)
    (91, Token (QCONSYM _)) -> Just (Shift 300)
    (91, Token (BACKQUOTE _)) -> Just (Shift 302)
    (92, Token (LPAREN _)) -> Just (Shift 112)
    (92, Token (LBRACKET _)) -> Just (Shift 116)
    (92, Token (EXCL _)) -> Just (Shift 75)
    (92, Token (QCONID _)) -> Just (Shift 119)
    (92, Token (EXPORT _)) -> Just (Shift 287)
    (92, Token (AS _)) -> Just (Shift 288)
    (92, Token (QVARID _)) -> Just (Shift 289)
    (93, Token (RBRACE _)) -> Just (Reduce 3 122)
    (93, Token (LPAREN _)) -> Just (Shift 112)
    (93, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (93, Token (DERIVING _)) -> Just (Reduce 3 122)
    (93, Token (PIPE _)) -> Just (Reduce 3 122)
    (93, Token (LBRACKET _)) -> Just (Shift 116)
    (93, Token (EXCL _)) -> Just (Shift 75)
    (93, Token (QCONID _)) -> Just (Shift 119)
    (93, Token (EXPORT _)) -> Just (Shift 287)
    (93, Token (AS _)) -> Just (Shift 288)
    (93, Token (QVARID _)) -> Just (Shift 289)
    (94, Token (LPAREN _)) -> Just (Shift 112)
    (94, Token (LBRACKET _)) -> Just (Shift 116)
    (94, Token (EXCL _)) -> Just (Shift 75)
    (94, Token (QCONID _)) -> Just (Shift 119)
    (94, Token (EXPORT _)) -> Just (Shift 287)
    (94, Token (AS _)) -> Just (Shift 288)
    (94, Token (QVARID _)) -> Just (Shift 289)
    (95, Token (RBRACE _)) -> Just (Reduce 1 98)
    (95, Token (LPAREN _)) -> Just (Shift 112)
    (95, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (95, Token (DARROW _)) -> Just (Shift 101)
    (95, Token (RARROW _)) -> Just (Shift 97)
    (95, Token (LBRACKET _)) -> Just (Shift 116)
    (95, Token (EXCL _)) -> Just (Shift 75)
    (95, Token (QCONID _)) -> Just (Shift 119)
    (95, Token (EXPORT _)) -> Just (Shift 287)
    (95, Token (AS _)) -> Just (Shift 288)
    (95, Token (QVARID _)) -> Just (Shift 289)
    (96, Token (LPAREN _)) -> Just (Shift 112)
    (96, Token (LBRACKET _)) -> Just (Shift 116)
    (96, Token (EXCL _)) -> Just (Shift 75)
    (96, Token (QCONID _)) -> Just (Shift 119)
    (96, Token (EXPORT _)) -> Just (Shift 287)
    (96, Token (AS _)) -> Just (Shift 288)
    (96, Token (QVARID _)) -> Just (Shift 289)
    (97, Token (LPAREN _)) -> Just (Shift 112)
    (97, Token (LBRACKET _)) -> Just (Shift 116)
    (97, Token (EXCL _)) -> Just (Shift 75)
    (97, Token (QCONID _)) -> Just (Shift 119)
    (97, Token (EXPORT _)) -> Just (Shift 287)
    (97, Token (AS _)) -> Just (Shift 288)
    (97, Token (QVARID _)) -> Just (Shift 289)
    (98, Token (LPAREN _)) -> Just (Shift 112)
    (98, Token (LBRACKET _)) -> Just (Shift 116)
    (98, Token (EXCL _)) -> Just (Shift 75)
    (98, Token (QCONID _)) -> Just (Shift 119)
    (98, Token (EXPORT _)) -> Just (Shift 287)
    (98, Token (AS _)) -> Just (Shift 288)
    (98, Token (QVARID _)) -> Just (Shift 289)
    (99, Token (LPAREN _)) -> Just (Shift 112)
    (99, Token (LBRACKET _)) -> Just (Shift 116)
    (99, Token (EXCL _)) -> Just (Shift 75)
    (99, Token (QCONID _)) -> Just (Shift 119)
    (99, Token (EXPORT _)) -> Just (Shift 287)
    (99, Token (AS _)) -> Just (Shift 288)
    (99, Token (QVARID _)) -> Just (Shift 289)
    (100, Token (LPAREN _)) -> Just (Shift 112)
    (100, Token (LBRACKET _)) -> Just (Shift 116)
    (100, Token (EXCL _)) -> Just (Shift 75)
    (100, Token (QCONID _)) -> Just (Shift 119)
    (100, Token (EXPORT _)) -> Just (Shift 287)
    (100, Token (AS _)) -> Just (Shift 288)
    (100, Token (QVARID _)) -> Just (Shift 289)
    (101, Token (LPAREN _)) -> Just (Shift 112)
    (101, Token (LBRACKET _)) -> Just (Shift 116)
    (101, Token (EXCL _)) -> Just (Shift 75)
    (101, Token (QCONID _)) -> Just (Shift 119)
    (101, Token (EXPORT _)) -> Just (Shift 287)
    (101, Token (AS _)) -> Just (Shift 288)
    (101, Token (QVARID _)) -> Just (Shift 289)
    (102, Token (RBRACE _)) -> Just (Reduce 1 98)
    (102, Token (LPAREN _)) -> Just (Shift 112)
    (102, Token (RPAREN _)) -> Just (Reduce 1 98)
    (102, Token (COMMA _)) -> Just (Reduce 1 98)
    (102, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (102, Token (RARROW _)) -> Just (Shift 97)
    (102, Token (LBRACKET _)) -> Just (Shift 116)
    (102, Token (RBRACKET _)) -> Just (Reduce 1 98)
    (102, Token (EXCL _)) -> Just (Shift 75)
    (102, Token (QCONID _)) -> Just (Shift 119)
    (102, Token (EXPORT _)) -> Just (Shift 287)
    (102, Token (AS _)) -> Just (Shift 288)
    (102, Token (QVARID _)) -> Just (Shift 289)
    (103, Token (LPAREN _)) -> Just (Shift 112)
    (103, Token (LBRACKET _)) -> Just (Shift 116)
    (103, Token (EXCL _)) -> Just (Shift 75)
    (103, Token (QCONID _)) -> Just (Shift 119)
    (103, Token (EXPORT _)) -> Just (Shift 287)
    (103, Token (AS _)) -> Just (Shift 288)
    (103, Token (QVARID _)) -> Just (Shift 289)
    (104, Token (LPAREN _)) -> Just (Shift 112)
    (104, Token (LBRACKET _)) -> Just (Shift 116)
    (104, Token (EXCL _)) -> Just (Shift 75)
    (104, Token (QCONID _)) -> Just (Shift 119)
    (104, Token (EXPORT _)) -> Just (Shift 287)
    (104, Token (AS _)) -> Just (Shift 288)
    (104, Token (QVARID _)) -> Just (Shift 289)
    (105, Token (LBRACE _)) -> Just (Shift 64)
    (105, Token (LPAREN _)) -> Just (Shift 112)
    (105, Token (LBRACKET _)) -> Just (Shift 116)
    (105, Token (EXCL _)) -> Just (Shift 75)
    (105, Token (QCONID _)) -> Just (Shift 119)
    (105, Token (EXPORT _)) -> Just (Shift 287)
    (105, Token (AS _)) -> Just (Shift 288)
    (105, Token (QVARID _)) -> Just (Shift 289)
    (106, Token (LPAREN _)) -> Just (Shift 112)
    (106, Token (LBRACKET _)) -> Just (Shift 116)
    (106, Token (EXCL _)) -> Just (Shift 75)
    (106, Token (QCONID _)) -> Just (Shift 119)
    (106, Token (EXPORT _)) -> Just (Shift 287)
    (106, Token (AS _)) -> Just (Shift 288)
    (106, Token (QVARID _)) -> Just (Shift 289)
    (107, Token (LPAREN _)) -> Just (Shift 112)
    (107, Token (EQUAL _)) -> Just (Shift 90)
    (107, Token (DARROW _)) -> Just (Shift 109)
    (107, Token (LBRACKET _)) -> Just (Shift 116)
    (107, Token (EXCL _)) -> Just (Shift 75)
    (107, Token (QCONID _)) -> Just (Shift 119)
    (107, Token (EXPORT _)) -> Just (Shift 287)
    (107, Token (AS _)) -> Just (Shift 288)
    (107, Token (QVARID _)) -> Just (Shift 289)
    (108, Token (LPAREN _)) -> Just (Shift 112)
    (108, Token (LBRACKET _)) -> Just (Shift 116)
    (108, Token (EXCL _)) -> Just (Shift 75)
    (108, Token (QCONID _)) -> Just (Shift 119)
    (108, Token (EXPORT _)) -> Just (Shift 287)
    (108, Token (AS _)) -> Just (Shift 288)
    (108, Token (QVARID _)) -> Just (Shift 289)
    (109, Token (LPAREN _)) -> Just (Shift 112)
    (109, Token (LBRACKET _)) -> Just (Shift 116)
    (109, Token (EXCL _)) -> Just (Shift 75)
    (109, Token (QCONID _)) -> Just (Shift 119)
    (109, Token (EXPORT _)) -> Just (Shift 287)
    (109, Token (AS _)) -> Just (Shift 288)
    (109, Token (QVARID _)) -> Just (Shift 289)
    (110, Token (LPAREN _)) -> Just (Shift 112)
    (110, Token (EQUAL _)) -> Just (Shift 96)
    (110, Token (LBRACKET _)) -> Just (Shift 116)
    (110, Token (EXCL _)) -> Just (Shift 75)
    (110, Token (QCONID _)) -> Just (Shift 119)
    (110, Token (EXPORT _)) -> Just (Shift 287)
    (110, Token (AS _)) -> Just (Shift 288)
    (110, Token (QVARID _)) -> Just (Shift 289)
    (111, Token (LPAREN _)) -> Just (Shift 112)
    (111, Token (EQUAL _)) -> Just (Shift 90)
    (111, Token (LBRACKET _)) -> Just (Shift 116)
    (111, Token (EXCL _)) -> Just (Shift 75)
    (111, Token (QCONID _)) -> Just (Shift 119)
    (111, Token (EXPORT _)) -> Just (Shift 287)
    (111, Token (AS _)) -> Just (Shift 288)
    (111, Token (QVARID _)) -> Just (Shift 289)
    (112, Token (LPAREN _)) -> Just (Shift 112)
    (112, Token (RPAREN _)) -> Just (Shift 279)
    (112, Token (COMMA _)) -> Just (Shift 292)
    (112, Token (RARROW _)) -> Just (Shift 282)
    (112, Token (LBRACKET _)) -> Just (Shift 116)
    (112, Token (EXCL _)) -> Just (Shift 75)
    (112, Token (QCONID _)) -> Just (Shift 119)
    (112, Token (EXPORT _)) -> Just (Shift 287)
    (112, Token (AS _)) -> Just (Shift 288)
    (112, Token (QVARID _)) -> Just (Shift 289)
    (112, Token (QCONSYM _)) -> Just (Shift 120)
    (113, Token (LPAREN _)) -> Just (Shift 112)
    (113, Token (RPAREN _)) -> Just (Shift 142)
    (113, Token (LBRACKET _)) -> Just (Shift 116)
    (113, Token (EXCL _)) -> Just (Shift 75)
    (113, Token (QCONID _)) -> Just (Shift 119)
    (113, Token (EXPORT _)) -> Just (Shift 287)
    (113, Token (AS _)) -> Just (Shift 288)
    (113, Token (QVARID _)) -> Just (Shift 289)
    (114, Token (LPAREN _)) -> Just (Shift 112)
    (114, Token (LBRACKET _)) -> Just (Shift 116)
    (114, Token (EXCL _)) -> Just (Shift 75)
    (114, Token (QCONID _)) -> Just (Shift 119)
    (114, Token (EXPORT _)) -> Just (Shift 287)
    (114, Token (AS _)) -> Just (Shift 288)
    (114, Token (QVARID _)) -> Just (Shift 289)
    (115, Token (LPAREN _)) -> Just (Shift 112)
    (115, Token (LBRACKET _)) -> Just (Shift 116)
    (115, Token (EXCL _)) -> Just (Shift 75)
    (115, Token (QCONID _)) -> Just (Shift 119)
    (115, Token (EXPORT _)) -> Just (Shift 287)
    (115, Token (AS _)) -> Just (Shift 288)
    (115, Token (QVARID _)) -> Just (Shift 289)
    (116, Token (LPAREN _)) -> Just (Shift 112)
    (116, Token (LBRACKET _)) -> Just (Shift 116)
    (116, Token (RBRACKET _)) -> Just (Shift 283)
    (116, Token (EXCL _)) -> Just (Shift 75)
    (116, Token (QCONID _)) -> Just (Shift 119)
    (116, Token (EXPORT _)) -> Just (Shift 287)
    (116, Token (AS _)) -> Just (Shift 288)
    (116, Token (QVARID _)) -> Just (Shift 289)
    (117, Token (QCONSYM _)) -> Just (Shift 120)
    (118, Token (WHERE _)) -> Just (Reduce 3 169)
    (118, Token (LBRACE _)) -> Just (Reduce 3 169)
    (118, Token (RBRACE _)) -> Just (Reduce 3 169)
    (118, Token (LPAREN _)) -> Just (Reduce 3 169)
    (118, Token (RPAREN _)) -> Just (Reduce 3 169)
    (118, Token (COMMA _)) -> Just (Reduce 3 169)
    (118, Token (SEMICOLON _)) -> Just (Reduce 3 169)
    (118, Token (EQUAL _)) -> Just (Reduce 3 169)
    (118, Token (DERIVING _)) -> Just (Reduce 3 169)
    (118, Token (DARROW _)) -> Just (Reduce 3 169)
    (118, Token (PIPE _)) -> Just (Reduce 3 169)
    (118, Token (COLON_COLON _)) -> Just (Reduce 3 169)
    (118, Token (INFIXL _)) -> Just (Reduce 3 169)
    (118, Token (INFIXR _)) -> Just (Reduce 3 169)
    (118, Token (INFIX _)) -> Just (Reduce 3 169)
    (118, Token (RARROW _)) -> Just (Reduce 3 169)
    (118, Token (LBRACKET _)) -> Just (Reduce 3 169)
    (118, Token (RBRACKET _)) -> Just (Reduce 3 169)
    (118, Token (EXCL _)) -> Just (Reduce 3 169)
    (118, Token (QCONID _)) -> Just (Reduce 3 169)
    (118, Token (EXPORT _)) -> Just (Reduce 3 169)
    (118, Token (AS _)) -> Just (Reduce 3 169)
    (118, Token (QVARID _)) -> Just (Reduce 3 169)
    (118, Token (INTEGER _)) -> Just (Reduce 3 169)
    (118, Token (QVARSYM _)) -> Just (Reduce 3 169)
    (118, Token (QCONSYM _)) -> Just (Reduce 3 169)
    (118, Token (BACKQUOTE _)) -> Just (Reduce 3 169)
    (119, Token (WHERE _)) -> Just (Reduce 1 168)
    (119, Token (LBRACE _)) -> Just (Reduce 1 168)
    (119, Token (RBRACE _)) -> Just (Reduce 1 168)
    (119, Token (LPAREN _)) -> Just (Reduce 1 168)
    (119, Token (RPAREN _)) -> Just (Reduce 1 168)
    (119, Token (COMMA _)) -> Just (Reduce 1 168)
    (119, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (119, Token (EQUAL _)) -> Just (Reduce 1 168)
    (119, Token (DERIVING _)) -> Just (Reduce 1 168)
    (119, Token (DARROW _)) -> Just (Reduce 1 168)
    (119, Token (PIPE _)) -> Just (Reduce 1 168)
    (119, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (119, Token (INFIXL _)) -> Just (Reduce 1 168)
    (119, Token (INFIXR _)) -> Just (Reduce 1 168)
    (119, Token (INFIX _)) -> Just (Reduce 1 168)
    (119, Token (RARROW _)) -> Just (Reduce 1 168)
    (119, Token (LBRACKET _)) -> Just (Reduce 1 168)
    (119, Token (RBRACKET _)) -> Just (Reduce 1 168)
    (119, Token (EXCL _)) -> Just (Reduce 1 168)
    (119, Token (QCONID _)) -> Just (Reduce 1 168)
    (119, Token (EXPORT _)) -> Just (Reduce 1 168)
    (119, Token (AS _)) -> Just (Reduce 1 168)
    (119, Token (QVARID _)) -> Just (Reduce 1 168)
    (119, Token (INTEGER _)) -> Just (Reduce 1 168)
    (119, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (119, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (119, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (120, Token (RPAREN _)) -> Just (Shift 118)
    (121, Token (RPAREN _)) -> Just (Reduce 3 24)
    (122, Token (RPAREN _)) -> Just (Reduce 1 23)
    (122, Token (COMMA _)) -> Just (Shift 59)
    (123, Token (RPAREN _)) -> Just (Reduce 3 17)
    (124, Token (RPAREN _)) -> Just (Reduce 1 16)
    (124, Token (COMMA _)) -> Just (Shift 56)
    (125, Token (RPAREN _)) -> Just (Reduce 3 20)
    (125, Token (COMMA _)) -> Just (Reduce 3 20)
    (126, Token (RPAREN _)) -> Just (Reduce 4 21)
    (126, Token (COMMA _)) -> Just (Reduce 4 21)
    (127, Token (RPAREN _)) -> Just (Reduce 4 22)
    (127, Token (COMMA _)) -> Just (Reduce 4 22)
    (128, Token (RPAREN _)) -> Just (Shift 126)
    (129, Token (RPAREN _)) -> Just (Reduce 1 18)
    (129, Token (COMMA _)) -> Just (Reduce 1 18)
    (130, Token (LPAREN _)) -> Just (Shift 60)
    (130, Token (RPAREN _)) -> Just (Reduce 1 19)
    (130, Token (COMMA _)) -> Just (Reduce 1 19)
    (131, Token (RPAREN _)) -> Just (Shift 127)
    (132, Token (RPAREN _)) -> Just (Reduce 1 25)
    (132, Token (COMMA _)) -> Just (Reduce 1 25)
    (133, Token (RPAREN _)) -> Just (Reduce 1 26)
    (133, Token (COMMA _)) -> Just (Reduce 1 26)
    (134, Token (RPAREN _)) -> Just (Shift 138)
    (134, Token (QCONID _)) -> Just (Shift 189)
    (135, Token (RPAREN _)) -> Just (Shift 139)
    (135, Token (QCONID _)) -> Just (Shift 189)
    (136, Token (RPAREN _)) -> Just (Shift 140)
    (136, Token (QCONID _)) -> Just (Shift 189)
    (137, Token (RPAREN _)) -> Just (Shift 141)
    (137, Token (QCONID _)) -> Just (Shift 189)
    (138, Token (RBRACE _)) -> Just (Reduce 6 35)
    (138, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (139, Token (RBRACE _)) -> Just (Reduce 8 39)
    (139, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (140, Token (RBRACE _)) -> Just (Reduce 8 47)
    (140, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (141, Token (RBRACE _)) -> Just (Reduce 6 43)
    (141, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (142, Token (RBRACE _)) -> Just (Reduce 3 53)
    (142, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (143, Token (RBRACE _)) -> Just (Reduce 8 31)
    (143, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (144, Token (RBRACE _)) -> Just (Reduce 7 30)
    (144, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (145, Token (RBRACE _)) -> Just (Reduce 7 36)
    (145, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (146, Token (RBRACE _)) -> Just (Reduce 9 40)
    (146, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (147, Token (RBRACE _)) -> Just (Reduce 9 48)
    (147, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (148, Token (RBRACE _)) -> Just (Reduce 7 44)
    (148, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (149, Token (RBRACE _)) -> Just (Reduce 4 54)
    (149, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (150, Token (QCONID _)) -> Just (Reduce 0 180)
    (150, Token (QUALIFIED _)) -> Just (Shift 182)
    (151, Token (LPAREN _)) -> Just (Shift 57)
    (152, Token (LPAREN _)) -> Just (Shift 134)
    (152, Token (QCONID _)) -> Just (Shift 189)
    (153, Token (LPAREN _)) -> Just (Shift 135)
    (153, Token (QCONID _)) -> Just (Shift 189)
    (154, Token (LPAREN _)) -> Just (Shift 136)
    (154, Token (QCONID _)) -> Just (Shift 189)
    (155, Token (LPAREN _)) -> Just (Shift 137)
    (155, Token (QCONID _)) -> Just (Shift 189)
    (156, Token (LPAREN _)) -> Just (Shift 113)
    (157, Token (IMPORT _)) -> Just (Shift 202)
    (157, Token (EXPORT _)) -> Just (Shift 203)
    (158, Token (RBRACE _)) -> Just (Reduce 0 178)
    (158, Token (LPAREN _)) -> Just (Reduce 0 178)
    (158, Token (SEMICOLON _)) -> Just (Reduce 0 178)
    (158, Token (HIDING _)) -> Just (Reduce 0 178)
    (158, Token (AS _)) -> Just (Shift 9)
    (159, Token (RPAREN _)) -> Just (Shift 143)
    (160, Token (RPAREN _)) -> Just (Shift 144)
    (161, Token (RBRACE _)) -> Just (Reduce 4 29)
    (161, Token (LPAREN _)) -> Just (Shift 58)
    (161, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (161, Token (HIDING _)) -> Just (Shift 151)
    (162, Token (RBRACE _)) -> Just (Reduce 4 32)
    (162, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (163, Token (RBRACE _)) -> Just (Reduce 3 33)
    (163, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (163, Token (DERIVING _)) -> Just (Shift 152)
    (164, Token (RBRACE _)) -> Just (Reduce 5 37)
    (164, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (164, Token (DERIVING _)) -> Just (Shift 153)
    (165, Token (RBRACE _)) -> Just (Reduce 5 34)
    (165, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (166, Token (RBRACE _)) -> Just (Reduce 7 38)
    (166, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (167, Token (RBRACE _)) -> Just (Reduce 7 46)
    (167, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (168, Token (RBRACE _)) -> Just (Reduce 5 42)
    (168, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (169, Token (RPAREN _)) -> Just (Shift 145)
    (170, Token (RPAREN _)) -> Just (Shift 146)
    (171, Token (RPAREN _)) -> Just (Shift 147)
    (172, Token (RPAREN _)) -> Just (Shift 148)
    (173, Token (RBRACE _)) -> Just (Reduce 5 45)
    (173, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (173, Token (DERIVING _)) -> Just (Shift 154)
    (174, Token (RBRACE _)) -> Just (Reduce 3 41)
    (174, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (174, Token (DERIVING _)) -> Just (Shift 155)
    (175, Token (RBRACE _)) -> Just (Reduce 5 50)
    (175, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (176, Token (RBRACE _)) -> Just (Reduce 3 49)
    (176, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (177, Token (RBRACE _)) -> Just (Reduce 5 52)
    (177, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (178, Token (RBRACE _)) -> Just (Reduce 3 51)
    (178, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (179, Token (RPAREN _)) -> Just (Shift 149)
    (180, Token (RBRACE _)) -> Just (Reduce 2 55)
    (180, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (181, Token (RBRACE _)) -> Just (Reduce 1 56)
    (181, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (182, Token (QCONID _)) -> Just (Reduce 1 181)
    (183, Token (RBRACE _)) -> Just (Reduce 2 179)
    (183, Token (LPAREN _)) -> Just (Reduce 2 179)
    (183, Token (SEMICOLON _)) -> Just (Reduce 2 179)
    (183, Token (HIDING _)) -> Just (Reduce 2 179)
    (184, Token (WHERE _)) -> Just (Reduce 1 100)
    (184, Token (LBRACE _)) -> Just (Reduce 1 100)
    (184, Token (RBRACE _)) -> Just (Reduce 1 100)
    (184, Token (LPAREN _)) -> Just (Reduce 1 100)
    (184, Token (RPAREN _)) -> Just (Reduce 1 100)
    (184, Token (COMMA _)) -> Just (Reduce 1 100)
    (184, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (184, Token (EQUAL _)) -> Just (Reduce 1 100)
    (184, Token (DERIVING _)) -> Just (Reduce 1 100)
    (184, Token (DARROW _)) -> Just (Reduce 1 100)
    (184, Token (PIPE _)) -> Just (Reduce 1 100)
    (184, Token (COLON_COLON _)) -> Just (Reduce 1 100)
    (184, Token (INFIXL _)) -> Just (Reduce 1 100)
    (184, Token (INFIXR _)) -> Just (Reduce 1 100)
    (184, Token (INFIX _)) -> Just (Reduce 1 100)
    (184, Token (RARROW _)) -> Just (Reduce 1 100)
    (184, Token (LBRACKET _)) -> Just (Reduce 1 100)
    (184, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (184, Token (EXCL _)) -> Just (Reduce 1 100)
    (184, Token (QCONID _)) -> Just (Reduce 1 100)
    (184, Token (EXPORT _)) -> Just (Reduce 1 100)
    (184, Token (AS _)) -> Just (Reduce 1 100)
    (184, Token (QVARID _)) -> Just (Reduce 1 100)
    (184, Token (INTEGER _)) -> Just (Reduce 1 100)
    (184, Token (QVARSYM _)) -> Just (Reduce 1 100)
    (184, Token (QCONSYM _)) -> Just (Reduce 1 100)
    (184, Token (BACKQUOTE _)) -> Just (Reduce 1 100)
    (185, Token (WHERE _)) -> Just (Reduce 2 101)
    (185, Token (LBRACE _)) -> Just (Reduce 2 101)
    (185, Token (RBRACE _)) -> Just (Reduce 2 101)
    (185, Token (LPAREN _)) -> Just (Reduce 2 101)
    (185, Token (RPAREN _)) -> Just (Reduce 2 101)
    (185, Token (COMMA _)) -> Just (Reduce 2 101)
    (185, Token (SEMICOLON _)) -> Just (Reduce 2 101)
    (185, Token (EQUAL _)) -> Just (Reduce 2 101)
    (185, Token (DERIVING _)) -> Just (Reduce 2 101)
    (185, Token (DARROW _)) -> Just (Reduce 2 101)
    (185, Token (PIPE _)) -> Just (Reduce 2 101)
    (185, Token (COLON_COLON _)) -> Just (Reduce 2 101)
    (185, Token (INFIXL _)) -> Just (Reduce 2 101)
    (185, Token (INFIXR _)) -> Just (Reduce 2 101)
    (185, Token (INFIX _)) -> Just (Reduce 2 101)
    (185, Token (RARROW _)) -> Just (Reduce 2 101)
    (185, Token (LBRACKET _)) -> Just (Reduce 2 101)
    (185, Token (RBRACKET _)) -> Just (Reduce 2 101)
    (185, Token (EXCL _)) -> Just (Reduce 2 101)
    (185, Token (QCONID _)) -> Just (Reduce 2 101)
    (185, Token (EXPORT _)) -> Just (Reduce 2 101)
    (185, Token (AS _)) -> Just (Reduce 2 101)
    (185, Token (QVARID _)) -> Just (Reduce 2 101)
    (185, Token (INTEGER _)) -> Just (Reduce 2 101)
    (185, Token (QVARSYM _)) -> Just (Reduce 2 101)
    (185, Token (QCONSYM _)) -> Just (Reduce 2 101)
    (185, Token (BACKQUOTE _)) -> Just (Reduce 2 101)
    (186, Token (RBRACE _)) -> Just (Reduce 3 99)
    (186, Token (RPAREN _)) -> Just (Reduce 3 99)
    (186, Token (COMMA _)) -> Just (Reduce 3 99)
    (186, Token (SEMICOLON _)) -> Just (Reduce 3 99)
    (186, Token (RBRACKET _)) -> Just (Reduce 3 99)
    (187, Token (RBRACE _)) -> Just (Reduce 2 118)
    (187, Token (SEMICOLON _)) -> Just (Reduce 2 118)
    (187, Token (DERIVING _)) -> Just (Reduce 2 118)
    (188, Token (QCONID _)) -> Just (Shift 189)
    (189, Token (RBRACE _)) -> Just (Reduce 1 132)
    (189, Token (RPAREN _)) -> Just (Reduce 1 132)
    (189, Token (COMMA _)) -> Just (Reduce 1 132)
    (189, Token (SEMICOLON _)) -> Just (Reduce 1 132)
    (190, Token (RPAREN _)) -> Just (Reduce 1 130)
    (190, Token (COMMA _)) -> Just (Shift 188)
    (191, Token (RPAREN _)) -> Just (Reduce 3 131)
    (192, Token (RBRACE _)) -> Just (Reduce 7 126)
    (192, Token (SEMICOLON _)) -> Just (Reduce 7 126)
    (192, Token (DERIVING _)) -> Just (Reduce 7 126)
    (193, Token (COLON_COLON _)) -> Just (Shift 104)
    (194, Token (RBRACE _)) -> Just (Shift 192)
    (195, Token (RBRACE _)) -> Just (Reduce 3 125)
    (195, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (195, Token (DERIVING _)) -> Just (Reduce 3 125)
    (196, Token (LBRACE _)) -> Just (Shift 43)
    (197, Token (RBRACE _)) -> Just (Reduce 2 66)
    (197, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (198, Token (LBRACE _)) -> Just (Shift 52)
    (199, Token (RBRACE _)) -> Just (Reduce 2 76)
    (199, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (200, Token (RPAREN _)) -> Just (Reduce 1 96)
    (200, Token (COMMA _)) -> Just (Shift 114)
    (201, Token (RPAREN _)) -> Just (Reduce 3 97)
    (202, Token (EXPORT _)) -> Just (Shift 308)
    (202, Token (AS _)) -> Just (Shift 309)
    (202, Token (QVARID _)) -> Just (Shift 310)
    (203, Token (EXPORT _)) -> Just (Shift 308)
    (203, Token (AS _)) -> Just (Shift 309)
    (203, Token (QVARID _)) -> Just (Shift 310)
    (204, Token (COLON_COLON _)) -> Just (Shift 98)
    (205, Token (COLON_COLON _)) -> Just (Shift 99)
    (206, Token (COLON_COLON _)) -> Just (Shift 100)
    (207, Token (RBRACE _)) -> Just (Reduce 6 133)
    (207, Token (SEMICOLON _)) -> Just (Reduce 6 133)
    (208, Token (RBRACE _)) -> Just (Reduce 7 134)
    (208, Token (SEMICOLON _)) -> Just (Reduce 7 134)
    (209, Token (RBRACE _)) -> Just (Reduce 6 135)
    (209, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (210, Token (EXPORT _)) -> Just (Shift 312)
    (210, Token (AS _)) -> Just (Shift 313)
    (210, Token (QVARID _)) -> Just (Shift 314)
    (210, Token (STRING _)) -> Just (Shift 311)
    (211, Token (STRING _)) -> Just (Shift 315)
    (212, Token (STRING _)) -> Just (Shift 311)
    (213, Token (LBRACE _)) -> Just (Shift 41)
    (214, Token (LBRACE _)) -> Just (Shift 41)
    (215, Token (RBRACE _)) -> Just (Reduce 5 62)
    (215, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (216, Token (RBRACE _)) -> Just (Reduce 5 64)
    (216, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (217, Token (RBRACE _)) -> Just (Reduce 1 60)
    (217, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (218, Token (WHERE _)) -> Just (Shift 213)
    (218, Token (RBRACE _)) -> Just (Reduce 3 61)
    (218, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (219, Token (WHERE _)) -> Just (Shift 214)
    (219, Token (RBRACE _)) -> Just (Reduce 3 63)
    (219, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (220, Token (LBRACE _)) -> Just (Shift 41)
    (221, Token (LBRACE _)) -> Just (Shift 41)
    (222, Token (LBRACE _)) -> Just (Shift 41)
    (223, Token (LBRACE _)) -> Just (Shift 41)
    (224, Token (LBRACE _)) -> Just (Shift 41)
    (225, Token (RBRACE _)) -> Just (Reduce 3 57)
    (225, Token (COMMA _)) -> Just (Reduce 3 57)
    (225, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (225, Token (EQUAL _)) -> Just (Reduce 3 57)
    (226, Token (RBRACE _)) -> Just (Shift 225)
    (227, Token (RBRACE _)) -> Just (Reduce 1 58)
    (227, Token (SEMICOLON _)) -> Just (Shift 42)
    (228, Token (RBRACE _)) -> Just (Reduce 3 59)
    (229, Token (RBRACE _)) -> Just (Reduce 5 87)
    (229, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (230, Token (RBRACE _)) -> Just (Reduce 3 86)
    (230, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (231, Token (COLON_COLON _)) -> Just (Shift 94)
    (232, Token (COMMA _)) -> Just (Reduce 0 187)
    (232, Token (QCONID _)) -> Just (Reduce 0 187)
    (232, Token (EXPORT _)) -> Just (Reduce 0 187)
    (232, Token (AS _)) -> Just (Reduce 0 187)
    (232, Token (QVARID _)) -> Just (Reduce 0 187)
    (232, Token (INTEGER _)) -> Just (Shift 264)
    (232, Token (QVARSYM _)) -> Just (Reduce 0 187)
    (232, Token (QCONSYM _)) -> Just (Reduce 0 187)
    (232, Token (BACKQUOTE _)) -> Just (Reduce 0 187)
    (233, Token (QVARSYM _)) -> Just (Shift 334)
    (233, Token (QCONSYM _)) -> Just (Shift 300)
    (233, Token (BACKQUOTE _)) -> Just (Shift 301)
    (234, Token (RBRACE _)) -> Just (Reduce 3 88)
    (234, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (235, Token (LPAREN _)) -> Just (Reduce 1 161)
    (235, Token (EQUAL _)) -> Just (Reduce 1 161)
    (235, Token (PIPE _)) -> Just (Reduce 1 161)
    (235, Token (EXPORT _)) -> Just (Reduce 1 161)
    (235, Token (AS _)) -> Just (Reduce 1 161)
    (235, Token (QVARID _)) -> Just (Reduce 1 161)
    (235, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (236, Token (LPAREN _)) -> Just (Reduce 2 162)
    (236, Token (EQUAL _)) -> Just (Reduce 2 162)
    (236, Token (PIPE _)) -> Just (Reduce 2 162)
    (236, Token (EXPORT _)) -> Just (Reduce 2 162)
    (236, Token (AS _)) -> Just (Reduce 2 162)
    (236, Token (QVARID _)) -> Just (Reduce 2 162)
    (236, Token (QVARSYM _)) -> Just (Reduce 2 162)
    (237, Token (WHERE _)) -> Just (Reduce 1 151)
    (237, Token (RBRACE _)) -> Just (Reduce 1 151)
    (237, Token (RPAREN _)) -> Just (Reduce 1 151)
    (237, Token (COMMA _)) -> Just (Reduce 1 151)
    (237, Token (SEMICOLON _)) -> Just (Reduce 1 151)
    (237, Token (EQUAL _)) -> Just (Reduce 1 151)
    (237, Token (PIPE _)) -> Just (Reduce 1 151)
    (238, Token (WHERE _)) -> Just (Reduce 3 144)
    (238, Token (RBRACE _)) -> Just (Reduce 3 144)
    (238, Token (SEMICOLON _)) -> Just (Reduce 3 144)
    (238, Token (PIPE _)) -> Just (Shift 46)
    (239, Token (WHERE _)) -> Just (Reduce 5 145)
    (239, Token (RBRACE _)) -> Just (Reduce 5 145)
    (239, Token (SEMICOLON _)) -> Just (Reduce 5 145)
    (240, Token (EQUAL _)) -> Just (Shift 36)
    (241, Token (RBRACE _)) -> Just (Reduce 3 67)
    (241, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (242, Token (RBRACE _)) -> Just (Shift 241)
    (243, Token (RBRACE _)) -> Just (Reduce 3 69)
    (244, Token (RBRACE _)) -> Just (Reduce 1 68)
    (244, Token (SEMICOLON _)) -> Just (Shift 44)
    (245, Token (RBRACE _)) -> Just (Reduce 5 72)
    (245, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (246, Token (RBRACE _)) -> Just (Reduce 5 74)
    (246, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (247, Token (RBRACE _)) -> Just (Reduce 1 70)
    (247, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (248, Token (WHERE _)) -> Just (Shift 220)
    (248, Token (RBRACE _)) -> Just (Reduce 3 71)
    (248, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (249, Token (WHERE _)) -> Just (Shift 221)
    (249, Token (RBRACE _)) -> Just (Reduce 3 73)
    (249, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (250, Token (RBRACE _)) -> Just (Reduce 3 77)
    (250, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (251, Token (RBRACE _)) -> Just (Shift 250)
    (252, Token (RBRACE _)) -> Just (Reduce 3 79)
    (253, Token (RBRACE _)) -> Just (Reduce 1 78)
    (253, Token (SEMICOLON _)) -> Just (Shift 53)
    (254, Token (RBRACE _)) -> Just (Reduce 5 82)
    (254, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (255, Token (RBRACE _)) -> Just (Reduce 5 84)
    (255, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (256, Token (WHERE _)) -> Just (Shift 222)
    (256, Token (RBRACE _)) -> Just (Reduce 3 81)
    (256, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (257, Token (WHERE _)) -> Just (Shift 223)
    (257, Token (RBRACE _)) -> Just (Reduce 3 83)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (258, Token (COMMA _)) -> Just (Shift 61)
    (258, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (259, Token (LPAREN _)) -> Just (Reduce 1 163)
    (259, Token (COMMA _)) -> Just (Shift 61)
    (259, Token (EQUAL _)) -> Just (Reduce 1 163)
    (259, Token (PIPE _)) -> Just (Reduce 1 163)
    (259, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (259, Token (EXPORT _)) -> Just (Reduce 1 163)
    (259, Token (AS _)) -> Just (Reduce 1 163)
    (259, Token (QVARID _)) -> Just (Reduce 1 163)
    (259, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (260, Token (COLON_COLON _)) -> Just (Reduce 3 92)
    (261, Token (COMMA _)) -> Just (Reduce 1 93)
    (261, Token (QCONID _)) -> Just (Reduce 1 93)
    (261, Token (EXPORT _)) -> Just (Reduce 1 93)
    (261, Token (AS _)) -> Just (Reduce 1 93)
    (261, Token (QVARID _)) -> Just (Reduce 1 93)
    (261, Token (INTEGER _)) -> Just (Reduce 1 93)
    (261, Token (QVARSYM _)) -> Just (Reduce 1 93)
    (261, Token (QCONSYM _)) -> Just (Reduce 1 93)
    (261, Token (BACKQUOTE _)) -> Just (Reduce 1 93)
    (262, Token (COMMA _)) -> Just (Reduce 1 94)
    (262, Token (QCONID _)) -> Just (Reduce 1 94)
    (262, Token (EXPORT _)) -> Just (Reduce 1 94)
    (262, Token (AS _)) -> Just (Reduce 1 94)
    (262, Token (QVARID _)) -> Just (Reduce 1 94)
    (262, Token (INTEGER _)) -> Just (Reduce 1 94)
    (262, Token (QVARSYM _)) -> Just (Reduce 1 94)
    (262, Token (QCONSYM _)) -> Just (Reduce 1 94)
    (262, Token (BACKQUOTE _)) -> Just (Reduce 1 94)
    (263, Token (COMMA _)) -> Just (Reduce 1 95)
    (263, Token (QCONID _)) -> Just (Reduce 1 95)
    (263, Token (EXPORT _)) -> Just (Reduce 1 95)
    (263, Token (AS _)) -> Just (Reduce 1 95)
    (263, Token (QVARID _)) -> Just (Reduce 1 95)
    (263, Token (INTEGER _)) -> Just (Reduce 1 95)
    (263, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (263, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (263, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (264, Token (COMMA _)) -> Just (Reduce 1 188)
    (264, Token (QCONID _)) -> Just (Reduce 1 188)
    (264, Token (EXPORT _)) -> Just (Reduce 1 188)
    (264, Token (AS _)) -> Just (Reduce 1 188)
    (264, Token (QVARID _)) -> Just (Reduce 1 188)
    (264, Token (QVARSYM _)) -> Just (Reduce 1 188)
    (264, Token (QCONSYM _)) -> Just (Reduce 1 188)
    (264, Token (BACKQUOTE _)) -> Just (Reduce 1 188)
    (265, Token (QVARSYM _)) -> Just (Shift 334)
    (265, Token (QCONSYM _)) -> Just (Shift 300)
    (265, Token (BACKQUOTE _)) -> Just (Shift 301)
    (266, Token (RBRACE _)) -> Just (Reduce 3 90)
    (266, Token (SEMICOLON _)) -> Just (Reduce 3 90)
    (267, Token (RBRACE _)) -> Just (Reduce 1 89)
    (267, Token (COMMA _)) -> Just (Shift 265)
    (267, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (268, Token (RBRACE _)) -> Just (Reduce 1 177)
    (268, Token (LPAREN _)) -> Just (Reduce 1 177)
    (268, Token (COMMA _)) -> Just (Reduce 1 177)
    (268, Token (SEMICOLON _)) -> Just (Reduce 1 177)
    (268, Token (QCONID _)) -> Just (Reduce 1 177)
    (268, Token (EXPORT _)) -> Just (Reduce 1 177)
    (268, Token (AS _)) -> Just (Reduce 1 177)
    (268, Token (QVARID _)) -> Just (Reduce 1 177)
    (268, Token (STRING _)) -> Just (Reduce 1 177)
    (268, Token (INTEGER _)) -> Just (Reduce 1 177)
    (268, Token (QVARSYM _)) -> Just (Reduce 1 177)
    (268, Token (QCONSYM _)) -> Just (Reduce 1 177)
    (268, Token (BACKQUOTE _)) -> Just (Reduce 1 177)
    (269, Token (RBRACE _)) -> Just (Reduce 1 176)
    (269, Token (LPAREN _)) -> Just (Reduce 1 176)
    (269, Token (COMMA _)) -> Just (Reduce 1 176)
    (269, Token (SEMICOLON _)) -> Just (Reduce 1 176)
    (269, Token (QCONID _)) -> Just (Reduce 1 176)
    (269, Token (EXPORT _)) -> Just (Reduce 1 176)
    (269, Token (AS _)) -> Just (Reduce 1 176)
    (269, Token (QVARID _)) -> Just (Reduce 1 176)
    (269, Token (STRING _)) -> Just (Reduce 1 176)
    (269, Token (INTEGER _)) -> Just (Reduce 1 176)
    (269, Token (QVARSYM _)) -> Just (Reduce 1 176)
    (269, Token (QCONSYM _)) -> Just (Reduce 1 176)
    (269, Token (BACKQUOTE _)) -> Just (Reduce 1 176)
    (270, Token (WHERE _)) -> Just (Reduce 3 106)
    (270, Token (LBRACE _)) -> Just (Reduce 3 106)
    (270, Token (RBRACE _)) -> Just (Reduce 3 106)
    (270, Token (LPAREN _)) -> Just (Reduce 3 106)
    (270, Token (RPAREN _)) -> Just (Reduce 3 106)
    (270, Token (COMMA _)) -> Just (Reduce 3 106)
    (270, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (270, Token (EQUAL _)) -> Just (Reduce 3 106)
    (270, Token (DERIVING _)) -> Just (Reduce 3 106)
    (270, Token (DARROW _)) -> Just (Reduce 3 106)
    (270, Token (PIPE _)) -> Just (Reduce 3 106)
    (270, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (270, Token (INFIXL _)) -> Just (Reduce 3 106)
    (270, Token (INFIXR _)) -> Just (Reduce 3 106)
    (270, Token (INFIX _)) -> Just (Reduce 3 106)
    (270, Token (RARROW _)) -> Just (Reduce 3 106)
    (270, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (270, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (270, Token (EXCL _)) -> Just (Reduce 3 106)
    (270, Token (QCONID _)) -> Just (Reduce 3 106)
    (270, Token (EXPORT _)) -> Just (Reduce 3 106)
    (270, Token (AS _)) -> Just (Reduce 3 106)
    (270, Token (QVARID _)) -> Just (Reduce 3 106)
    (270, Token (INTEGER _)) -> Just (Reduce 3 106)
    (270, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (270, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (270, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (271, Token (WHERE _)) -> Just (Reduce 3 104)
    (271, Token (LBRACE _)) -> Just (Reduce 3 104)
    (271, Token (RBRACE _)) -> Just (Reduce 3 104)
    (271, Token (LPAREN _)) -> Just (Reduce 3 104)
    (271, Token (RPAREN _)) -> Just (Reduce 3 104)
    (271, Token (COMMA _)) -> Just (Reduce 3 104)
    (271, Token (SEMICOLON _)) -> Just (Reduce 3 104)
    (271, Token (EQUAL _)) -> Just (Reduce 3 104)
    (271, Token (DERIVING _)) -> Just (Reduce 3 104)
    (271, Token (DARROW _)) -> Just (Reduce 3 104)
    (271, Token (PIPE _)) -> Just (Reduce 3 104)
    (271, Token (COLON_COLON _)) -> Just (Reduce 3 104)
    (271, Token (INFIXL _)) -> Just (Reduce 3 104)
    (271, Token (INFIXR _)) -> Just (Reduce 3 104)
    (271, Token (INFIX _)) -> Just (Reduce 3 104)
    (271, Token (RARROW _)) -> Just (Reduce 3 104)
    (271, Token (LBRACKET _)) -> Just (Reduce 3 104)
    (271, Token (RBRACKET _)) -> Just (Reduce 3 104)
    (271, Token (EXCL _)) -> Just (Reduce 3 104)
    (271, Token (QCONID _)) -> Just (Reduce 3 104)
    (271, Token (EXPORT _)) -> Just (Reduce 3 104)
    (271, Token (AS _)) -> Just (Reduce 3 104)
    (271, Token (QVARID _)) -> Just (Reduce 3 104)
    (271, Token (INTEGER _)) -> Just (Reduce 3 104)
    (271, Token (QVARSYM _)) -> Just (Reduce 3 104)
    (271, Token (QCONSYM _)) -> Just (Reduce 3 104)
    (271, Token (BACKQUOTE _)) -> Just (Reduce 3 104)
    (272, Token (WHERE _)) -> Just (Reduce 3 105)
    (272, Token (LBRACE _)) -> Just (Reduce 3 105)
    (272, Token (RBRACE _)) -> Just (Reduce 3 105)
    (272, Token (LPAREN _)) -> Just (Reduce 3 105)
    (272, Token (RPAREN _)) -> Just (Reduce 3 105)
    (272, Token (COMMA _)) -> Just (Reduce 3 105)
    (272, Token (SEMICOLON _)) -> Just (Reduce 3 105)
    (272, Token (EQUAL _)) -> Just (Reduce 3 105)
    (272, Token (DERIVING _)) -> Just (Reduce 3 105)
    (272, Token (DARROW _)) -> Just (Reduce 3 105)
    (272, Token (PIPE _)) -> Just (Reduce 3 105)
    (272, Token (COLON_COLON _)) -> Just (Reduce 3 105)
    (272, Token (INFIXL _)) -> Just (Reduce 3 105)
    (272, Token (INFIXR _)) -> Just (Reduce 3 105)
    (272, Token (INFIX _)) -> Just (Reduce 3 105)
    (272, Token (RARROW _)) -> Just (Reduce 3 105)
    (272, Token (LBRACKET _)) -> Just (Reduce 3 105)
    (272, Token (RBRACKET _)) -> Just (Reduce 3 105)
    (272, Token (EXCL _)) -> Just (Reduce 3 105)
    (272, Token (QCONID _)) -> Just (Reduce 3 105)
    (272, Token (EXPORT _)) -> Just (Reduce 3 105)
    (272, Token (AS _)) -> Just (Reduce 3 105)
    (272, Token (QVARID _)) -> Just (Reduce 3 105)
    (272, Token (INTEGER _)) -> Just (Reduce 3 105)
    (272, Token (QVARSYM _)) -> Just (Reduce 3 105)
    (272, Token (QCONSYM _)) -> Just (Reduce 3 105)
    (272, Token (BACKQUOTE _)) -> Just (Reduce 3 105)
    (273, Token (RPAREN _)) -> Just (Shift 270)
    (273, Token (COMMA _)) -> Just (Shift 115)
    (274, Token (RBRACKET _)) -> Just (Shift 272)
    (275, Token (WHERE _)) -> Just (Reduce 2 107)
    (275, Token (LBRACE _)) -> Just (Reduce 2 107)
    (275, Token (RBRACE _)) -> Just (Reduce 2 107)
    (275, Token (LPAREN _)) -> Just (Reduce 2 107)
    (275, Token (RPAREN _)) -> Just (Reduce 2 107)
    (275, Token (COMMA _)) -> Just (Reduce 2 107)
    (275, Token (SEMICOLON _)) -> Just (Reduce 2 107)
    (275, Token (EQUAL _)) -> Just (Reduce 2 107)
    (275, Token (DERIVING _)) -> Just (Reduce 2 107)
    (275, Token (DARROW _)) -> Just (Reduce 2 107)
    (275, Token (PIPE _)) -> Just (Reduce 2 107)
    (275, Token (COLON_COLON _)) -> Just (Reduce 2 107)
    (275, Token (INFIXL _)) -> Just (Reduce 2 107)
    (275, Token (INFIXR _)) -> Just (Reduce 2 107)
    (275, Token (INFIX _)) -> Just (Reduce 2 107)
    (275, Token (RARROW _)) -> Just (Reduce 2 107)
    (275, Token (LBRACKET _)) -> Just (Reduce 2 107)
    (275, Token (RBRACKET _)) -> Just (Reduce 2 107)
    (275, Token (EXCL _)) -> Just (Reduce 2 107)
    (275, Token (QCONID _)) -> Just (Reduce 2 107)
    (275, Token (EXPORT _)) -> Just (Reduce 2 107)
    (275, Token (AS _)) -> Just (Reduce 2 107)
    (275, Token (QVARID _)) -> Just (Reduce 2 107)
    (275, Token (INTEGER _)) -> Just (Reduce 2 107)
    (275, Token (QVARSYM _)) -> Just (Reduce 2 107)
    (275, Token (QCONSYM _)) -> Just (Reduce 2 107)
    (275, Token (BACKQUOTE _)) -> Just (Reduce 2 107)
    (276, Token (WHERE _)) -> Just (Reduce 1 102)
    (276, Token (LBRACE _)) -> Just (Reduce 1 102)
    (276, Token (RBRACE _)) -> Just (Reduce 1 102)
    (276, Token (LPAREN _)) -> Just (Reduce 1 102)
    (276, Token (RPAREN _)) -> Just (Reduce 1 102)
    (276, Token (COMMA _)) -> Just (Reduce 1 102)
    (276, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (276, Token (EQUAL _)) -> Just (Reduce 1 102)
    (276, Token (DERIVING _)) -> Just (Reduce 1 102)
    (276, Token (DARROW _)) -> Just (Reduce 1 102)
    (276, Token (PIPE _)) -> Just (Reduce 1 102)
    (276, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (276, Token (INFIXL _)) -> Just (Reduce 1 102)
    (276, Token (INFIXR _)) -> Just (Reduce 1 102)
    (276, Token (INFIX _)) -> Just (Reduce 1 102)
    (276, Token (RARROW _)) -> Just (Reduce 1 102)
    (276, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (276, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (276, Token (EXCL _)) -> Just (Reduce 1 102)
    (276, Token (QCONID _)) -> Just (Reduce 1 102)
    (276, Token (EXPORT _)) -> Just (Reduce 1 102)
    (276, Token (AS _)) -> Just (Reduce 1 102)
    (276, Token (QVARID _)) -> Just (Reduce 1 102)
    (276, Token (INTEGER _)) -> Just (Reduce 1 102)
    (276, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (276, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (276, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (277, Token (WHERE _)) -> Just (Reduce 1 103)
    (277, Token (LBRACE _)) -> Just (Reduce 1 103)
    (277, Token (RBRACE _)) -> Just (Reduce 1 103)
    (277, Token (LPAREN _)) -> Just (Reduce 1 103)
    (277, Token (RPAREN _)) -> Just (Reduce 1 103)
    (277, Token (COMMA _)) -> Just (Reduce 1 103)
    (277, Token (SEMICOLON _)) -> Just (Reduce 1 103)
    (277, Token (EQUAL _)) -> Just (Reduce 1 103)
    (277, Token (DERIVING _)) -> Just (Reduce 1 103)
    (277, Token (DARROW _)) -> Just (Reduce 1 103)
    (277, Token (PIPE _)) -> Just (Reduce 1 103)
    (277, Token (COLON_COLON _)) -> Just (Reduce 1 103)
    (277, Token (INFIXL _)) -> Just (Reduce 1 103)
    (277, Token (INFIXR _)) -> Just (Reduce 1 103)
    (277, Token (INFIX _)) -> Just (Reduce 1 103)
    (277, Token (RARROW _)) -> Just (Reduce 1 103)
    (277, Token (LBRACKET _)) -> Just (Reduce 1 103)
    (277, Token (RBRACKET _)) -> Just (Reduce 1 103)
    (277, Token (EXCL _)) -> Just (Reduce 1 103)
    (277, Token (QCONID _)) -> Just (Reduce 1 103)
    (277, Token (EXPORT _)) -> Just (Reduce 1 103)
    (277, Token (AS _)) -> Just (Reduce 1 103)
    (277, Token (QVARID _)) -> Just (Reduce 1 103)
    (277, Token (INTEGER _)) -> Just (Reduce 1 103)
    (277, Token (QVARSYM _)) -> Just (Reduce 1 103)
    (277, Token (QCONSYM _)) -> Just (Reduce 1 103)
    (277, Token (BACKQUOTE _)) -> Just (Reduce 1 103)
    (278, Token (RPAREN _)) -> Just (Shift 271)
    (279, Token (WHERE _)) -> Just (Reduce 2 111)
    (279, Token (LBRACE _)) -> Just (Reduce 2 111)
    (279, Token (RBRACE _)) -> Just (Reduce 2 111)
    (279, Token (LPAREN _)) -> Just (Reduce 2 111)
    (279, Token (RPAREN _)) -> Just (Reduce 2 111)
    (279, Token (COMMA _)) -> Just (Reduce 2 111)
    (279, Token (SEMICOLON _)) -> Just (Reduce 2 111)
    (279, Token (EQUAL _)) -> Just (Reduce 2 111)
    (279, Token (DERIVING _)) -> Just (Reduce 2 111)
    (279, Token (DARROW _)) -> Just (Reduce 2 111)
    (279, Token (PIPE _)) -> Just (Reduce 2 111)
    (279, Token (COLON_COLON _)) -> Just (Reduce 2 111)
    (279, Token (INFIXL _)) -> Just (Reduce 2 111)
    (279, Token (INFIXR _)) -> Just (Reduce 2 111)
    (279, Token (INFIX _)) -> Just (Reduce 2 111)
    (279, Token (RARROW _)) -> Just (Reduce 2 111)
    (279, Token (LBRACKET _)) -> Just (Reduce 2 111)
    (279, Token (RBRACKET _)) -> Just (Reduce 2 111)
    (279, Token (EXCL _)) -> Just (Reduce 2 111)
    (279, Token (QCONID _)) -> Just (Reduce 2 111)
    (279, Token (EXPORT _)) -> Just (Reduce 2 111)
    (279, Token (AS _)) -> Just (Reduce 2 111)
    (279, Token (QVARID _)) -> Just (Reduce 2 111)
    (279, Token (INTEGER _)) -> Just (Reduce 2 111)
    (279, Token (QVARSYM _)) -> Just (Reduce 2 111)
    (279, Token (QCONSYM _)) -> Just (Reduce 2 111)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 2 111)
    (280, Token (WHERE _)) -> Just (Reduce 3 113)
    (280, Token (LBRACE _)) -> Just (Reduce 3 113)
    (280, Token (RBRACE _)) -> Just (Reduce 3 113)
    (280, Token (LPAREN _)) -> Just (Reduce 3 113)
    (280, Token (RPAREN _)) -> Just (Reduce 3 113)
    (280, Token (COMMA _)) -> Just (Reduce 3 113)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 113)
    (280, Token (EQUAL _)) -> Just (Reduce 3 113)
    (280, Token (DERIVING _)) -> Just (Reduce 3 113)
    (280, Token (DARROW _)) -> Just (Reduce 3 113)
    (280, Token (PIPE _)) -> Just (Reduce 3 113)
    (280, Token (COLON_COLON _)) -> Just (Reduce 3 113)
    (280, Token (INFIXL _)) -> Just (Reduce 3 113)
    (280, Token (INFIXR _)) -> Just (Reduce 3 113)
    (280, Token (INFIX _)) -> Just (Reduce 3 113)
    (280, Token (RARROW _)) -> Just (Reduce 3 113)
    (280, Token (LBRACKET _)) -> Just (Reduce 3 113)
    (280, Token (RBRACKET _)) -> Just (Reduce 3 113)
    (280, Token (EXCL _)) -> Just (Reduce 3 113)
    (280, Token (QCONID _)) -> Just (Reduce 3 113)
    (280, Token (EXPORT _)) -> Just (Reduce 3 113)
    (280, Token (AS _)) -> Just (Reduce 3 113)
    (280, Token (QVARID _)) -> Just (Reduce 3 113)
    (280, Token (INTEGER _)) -> Just (Reduce 3 113)
    (280, Token (QVARSYM _)) -> Just (Reduce 3 113)
    (280, Token (QCONSYM _)) -> Just (Reduce 3 113)
    (280, Token (BACKQUOTE _)) -> Just (Reduce 3 113)
    (281, Token (WHERE _)) -> Just (Reduce 3 114)
    (281, Token (LBRACE _)) -> Just (Reduce 3 114)
    (281, Token (RBRACE _)) -> Just (Reduce 3 114)
    (281, Token (LPAREN _)) -> Just (Reduce 3 114)
    (281, Token (RPAREN _)) -> Just (Reduce 3 114)
    (281, Token (COMMA _)) -> Just (Reduce 3 114)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (281, Token (EQUAL _)) -> Just (Reduce 3 114)
    (281, Token (DERIVING _)) -> Just (Reduce 3 114)
    (281, Token (DARROW _)) -> Just (Reduce 3 114)
    (281, Token (PIPE _)) -> Just (Reduce 3 114)
    (281, Token (COLON_COLON _)) -> Just (Reduce 3 114)
    (281, Token (INFIXL _)) -> Just (Reduce 3 114)
    (281, Token (INFIXR _)) -> Just (Reduce 3 114)
    (281, Token (INFIX _)) -> Just (Reduce 3 114)
    (281, Token (RARROW _)) -> Just (Reduce 3 114)
    (281, Token (LBRACKET _)) -> Just (Reduce 3 114)
    (281, Token (RBRACKET _)) -> Just (Reduce 3 114)
    (281, Token (EXCL _)) -> Just (Reduce 3 114)
    (281, Token (QCONID _)) -> Just (Reduce 3 114)
    (281, Token (EXPORT _)) -> Just (Reduce 3 114)
    (281, Token (AS _)) -> Just (Reduce 3 114)
    (281, Token (QVARID _)) -> Just (Reduce 3 114)
    (281, Token (INTEGER _)) -> Just (Reduce 3 114)
    (281, Token (QVARSYM _)) -> Just (Reduce 3 114)
    (281, Token (QCONSYM _)) -> Just (Reduce 3 114)
    (281, Token (BACKQUOTE _)) -> Just (Reduce 3 114)
    (282, Token (RPAREN _)) -> Just (Shift 280)
    (283, Token (WHERE _)) -> Just (Reduce 2 112)
    (283, Token (LBRACE _)) -> Just (Reduce 2 112)
    (283, Token (RBRACE _)) -> Just (Reduce 2 112)
    (283, Token (LPAREN _)) -> Just (Reduce 2 112)
    (283, Token (RPAREN _)) -> Just (Reduce 2 112)
    (283, Token (COMMA _)) -> Just (Reduce 2 112)
    (283, Token (SEMICOLON _)) -> Just (Reduce 2 112)
    (283, Token (EQUAL _)) -> Just (Reduce 2 112)
    (283, Token (DERIVING _)) -> Just (Reduce 2 112)
    (283, Token (DARROW _)) -> Just (Reduce 2 112)
    (283, Token (PIPE _)) -> Just (Reduce 2 112)
    (283, Token (COLON_COLON _)) -> Just (Reduce 2 112)
    (283, Token (INFIXL _)) -> Just (Reduce 2 112)
    (283, Token (INFIXR _)) -> Just (Reduce 2 112)
    (283, Token (INFIX _)) -> Just (Reduce 2 112)
    (283, Token (RARROW _)) -> Just (Reduce 2 112)
    (283, Token (LBRACKET _)) -> Just (Reduce 2 112)
    (283, Token (RBRACKET _)) -> Just (Reduce 2 112)
    (283, Token (EXCL _)) -> Just (Reduce 2 112)
    (283, Token (QCONID _)) -> Just (Reduce 2 112)
    (283, Token (EXPORT _)) -> Just (Reduce 2 112)
    (283, Token (AS _)) -> Just (Reduce 2 112)
    (283, Token (QVARID _)) -> Just (Reduce 2 112)
    (283, Token (INTEGER _)) -> Just (Reduce 2 112)
    (283, Token (QVARSYM _)) -> Just (Reduce 2 112)
    (283, Token (QCONSYM _)) -> Just (Reduce 2 112)
    (283, Token (BACKQUOTE _)) -> Just (Reduce 2 112)
    (284, Token (WHERE _)) -> Just (Reduce 1 110)
    (284, Token (LBRACE _)) -> Just (Reduce 1 110)
    (284, Token (RBRACE _)) -> Just (Reduce 1 110)
    (284, Token (LPAREN _)) -> Just (Reduce 1 110)
    (284, Token (RPAREN _)) -> Just (Reduce 1 110)
    (284, Token (COMMA _)) -> Just (Reduce 1 110)
    (284, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (284, Token (EQUAL _)) -> Just (Reduce 1 110)
    (284, Token (DERIVING _)) -> Just (Reduce 1 110)
    (284, Token (DARROW _)) -> Just (Reduce 1 110)
    (284, Token (PIPE _)) -> Just (Reduce 1 110)
    (284, Token (COLON_COLON _)) -> Just (Reduce 1 110)
    (284, Token (INFIXL _)) -> Just (Reduce 1 110)
    (284, Token (INFIXR _)) -> Just (Reduce 1 110)
    (284, Token (INFIX _)) -> Just (Reduce 1 110)
    (284, Token (RARROW _)) -> Just (Reduce 1 110)
    (284, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (284, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (284, Token (EXCL _)) -> Just (Reduce 1 110)
    (284, Token (QCONID _)) -> Just (Reduce 1 110)
    (284, Token (EXPORT _)) -> Just (Reduce 1 110)
    (284, Token (AS _)) -> Just (Reduce 1 110)
    (284, Token (QVARID _)) -> Just (Reduce 1 110)
    (284, Token (INTEGER _)) -> Just (Reduce 1 110)
    (284, Token (QVARSYM _)) -> Just (Reduce 1 110)
    (284, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (284, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (285, Token (LBRACE _)) -> Just (Shift 62)
    (285, Token (RBRACE _)) -> Just (Reduce 1 110)
    (285, Token (LPAREN _)) -> Just (Reduce 1 110)
    (285, Token (RPAREN _)) -> Just (Reduce 1 110)
    (285, Token (COMMA _)) -> Just (Reduce 1 110)
    (285, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (285, Token (DERIVING _)) -> Just (Reduce 1 110)
    (285, Token (PIPE _)) -> Just (Reduce 1 110)
    (285, Token (RARROW _)) -> Just (Reduce 1 110)
    (285, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (285, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (285, Token (EXCL _)) -> Just (Reduce 1 110)
    (285, Token (QCONID _)) -> Just (Reduce 1 110)
    (285, Token (EXPORT _)) -> Just (Reduce 1 110)
    (285, Token (AS _)) -> Just (Reduce 1 110)
    (285, Token (QVARID _)) -> Just (Reduce 1 110)
    (285, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (285, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (286, Token (RPAREN _)) -> Just (Shift 281)
    (287, Token (WHERE _)) -> Just (Reduce 1 183)
    (287, Token (LBRACE _)) -> Just (Reduce 1 183)
    (287, Token (RBRACE _)) -> Just (Reduce 1 183)
    (287, Token (LPAREN _)) -> Just (Reduce 1 183)
    (287, Token (RPAREN _)) -> Just (Reduce 1 183)
    (287, Token (COMMA _)) -> Just (Reduce 1 183)
    (287, Token (SEMICOLON _)) -> Just (Reduce 1 183)
    (287, Token (EQUAL _)) -> Just (Reduce 1 183)
    (287, Token (DERIVING _)) -> Just (Reduce 1 183)
    (287, Token (DARROW _)) -> Just (Reduce 1 183)
    (287, Token (PIPE _)) -> Just (Reduce 1 183)
    (287, Token (COLON_COLON _)) -> Just (Reduce 1 183)
    (287, Token (INFIXL _)) -> Just (Reduce 1 183)
    (287, Token (INFIXR _)) -> Just (Reduce 1 183)
    (287, Token (INFIX _)) -> Just (Reduce 1 183)
    (287, Token (RARROW _)) -> Just (Reduce 1 183)
    (287, Token (LBRACKET _)) -> Just (Reduce 1 183)
    (287, Token (RBRACKET _)) -> Just (Reduce 1 183)
    (287, Token (EXCL _)) -> Just (Reduce 1 183)
    (287, Token (QCONID _)) -> Just (Reduce 1 183)
    (287, Token (EXPORT _)) -> Just (Reduce 1 183)
    (287, Token (AS _)) -> Just (Reduce 1 183)
    (287, Token (QVARID _)) -> Just (Reduce 1 183)
    (287, Token (INTEGER _)) -> Just (Reduce 1 183)
    (287, Token (QVARSYM _)) -> Just (Reduce 1 183)
    (287, Token (QCONSYM _)) -> Just (Reduce 1 183)
    (287, Token (BACKQUOTE _)) -> Just (Reduce 1 183)
    (288, Token (WHERE _)) -> Just (Reduce 1 182)
    (288, Token (LBRACE _)) -> Just (Reduce 1 182)
    (288, Token (RBRACE _)) -> Just (Reduce 1 182)
    (288, Token (LPAREN _)) -> Just (Reduce 1 182)
    (288, Token (RPAREN _)) -> Just (Reduce 1 182)
    (288, Token (COMMA _)) -> Just (Reduce 1 182)
    (288, Token (SEMICOLON _)) -> Just (Reduce 1 182)
    (288, Token (EQUAL _)) -> Just (Reduce 1 182)
    (288, Token (DERIVING _)) -> Just (Reduce 1 182)
    (288, Token (DARROW _)) -> Just (Reduce 1 182)
    (288, Token (PIPE _)) -> Just (Reduce 1 182)
    (288, Token (COLON_COLON _)) -> Just (Reduce 1 182)
    (288, Token (INFIXL _)) -> Just (Reduce 1 182)
    (288, Token (INFIXR _)) -> Just (Reduce 1 182)
    (288, Token (INFIX _)) -> Just (Reduce 1 182)
    (288, Token (RARROW _)) -> Just (Reduce 1 182)
    (288, Token (LBRACKET _)) -> Just (Reduce 1 182)
    (288, Token (RBRACKET _)) -> Just (Reduce 1 182)
    (288, Token (EXCL _)) -> Just (Reduce 1 182)
    (288, Token (QCONID _)) -> Just (Reduce 1 182)
    (288, Token (EXPORT _)) -> Just (Reduce 1 182)
    (288, Token (AS _)) -> Just (Reduce 1 182)
    (288, Token (QVARID _)) -> Just (Reduce 1 182)
    (288, Token (INTEGER _)) -> Just (Reduce 1 182)
    (288, Token (QVARSYM _)) -> Just (Reduce 1 182)
    (288, Token (QCONSYM _)) -> Just (Reduce 1 182)
    (288, Token (BACKQUOTE _)) -> Just (Reduce 1 182)
    (289, Token (WHERE _)) -> Just (Reduce 1 184)
    (289, Token (LBRACE _)) -> Just (Reduce 1 184)
    (289, Token (RBRACE _)) -> Just (Reduce 1 184)
    (289, Token (LPAREN _)) -> Just (Reduce 1 184)
    (289, Token (RPAREN _)) -> Just (Reduce 1 184)
    (289, Token (COMMA _)) -> Just (Reduce 1 184)
    (289, Token (SEMICOLON _)) -> Just (Reduce 1 184)
    (289, Token (EQUAL _)) -> Just (Reduce 1 184)
    (289, Token (DERIVING _)) -> Just (Reduce 1 184)
    (289, Token (DARROW _)) -> Just (Reduce 1 184)
    (289, Token (PIPE _)) -> Just (Reduce 1 184)
    (289, Token (COLON_COLON _)) -> Just (Reduce 1 184)
    (289, Token (INFIXL _)) -> Just (Reduce 1 184)
    (289, Token (INFIXR _)) -> Just (Reduce 1 184)
    (289, Token (INFIX _)) -> Just (Reduce 1 184)
    (289, Token (RARROW _)) -> Just (Reduce 1 184)
    (289, Token (LBRACKET _)) -> Just (Reduce 1 184)
    (289, Token (RBRACKET _)) -> Just (Reduce 1 184)
    (289, Token (EXCL _)) -> Just (Reduce 1 184)
    (289, Token (QCONID _)) -> Just (Reduce 1 184)
    (289, Token (EXPORT _)) -> Just (Reduce 1 184)
    (289, Token (AS _)) -> Just (Reduce 1 184)
    (289, Token (QVARID _)) -> Just (Reduce 1 184)
    (289, Token (INTEGER _)) -> Just (Reduce 1 184)
    (289, Token (QVARSYM _)) -> Just (Reduce 1 184)
    (289, Token (QCONSYM _)) -> Just (Reduce 1 184)
    (289, Token (BACKQUOTE _)) -> Just (Reduce 1 184)
    (290, Token (RPAREN _)) -> Just (Reduce 3 108)
    (290, Token (COMMA _)) -> Just (Shift 115)
    (291, Token (RPAREN _)) -> Just (Reduce 3 109)
    (292, Token (RPAREN _)) -> Just (Reduce 1 115)
    (292, Token (COMMA _)) -> Just (Shift 292)
    (293, Token (RPAREN _)) -> Just (Reduce 2 116)
    (294, Token (RBRACE _)) -> Just (Reduce 3 120)
    (294, Token (SEMICOLON _)) -> Just (Reduce 3 120)
    (294, Token (DERIVING _)) -> Just (Reduce 3 120)
    (295, Token (RBRACE _)) -> Just (Reduce 1 119)
    (295, Token (SEMICOLON _)) -> Just (Reduce 1 119)
    (295, Token (DERIVING _)) -> Just (Reduce 1 119)
    (295, Token (PIPE _)) -> Just (Shift 89)
    (296, Token (RBRACE _)) -> Just (Reduce 3 123)
    (296, Token (SEMICOLON _)) -> Just (Reduce 3 123)
    (296, Token (DERIVING _)) -> Just (Reduce 3 123)
    (296, Token (PIPE _)) -> Just (Reduce 3 123)
    (297, Token (RBRACE _)) -> Just (Reduce 4 124)
    (297, Token (SEMICOLON _)) -> Just (Reduce 4 124)
    (297, Token (DERIVING _)) -> Just (Reduce 4 124)
    (297, Token (PIPE _)) -> Just (Reduce 4 124)
    (298, Token (RBRACE _)) -> Just (Shift 297)
    (299, Token (BACKQUOTE _)) -> Just (Shift 303)
    (300, Token (RBRACE _)) -> Just (Reduce 1 174)
    (300, Token (LPAREN _)) -> Just (Reduce 1 174)
    (300, Token (RPAREN _)) -> Just (Reduce 1 174)
    (300, Token (COMMA _)) -> Just (Reduce 1 174)
    (300, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (300, Token (RARROW _)) -> Just (Reduce 1 174)
    (300, Token (LBRACKET _)) -> Just (Reduce 1 174)
    (300, Token (RBRACKET _)) -> Just (Reduce 1 174)
    (300, Token (EXCL _)) -> Just (Reduce 1 174)
    (300, Token (QCONID _)) -> Just (Reduce 1 174)
    (300, Token (EXPORT _)) -> Just (Reduce 1 174)
    (300, Token (AS _)) -> Just (Reduce 1 174)
    (300, Token (QVARID _)) -> Just (Reduce 1 174)
    (300, Token (STRING _)) -> Just (Reduce 1 174)
    (300, Token (INTEGER _)) -> Just (Reduce 1 174)
    (300, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (300, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (300, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (301, Token (QCONID _)) -> Just (Shift 299)
    (301, Token (EXPORT _)) -> Just (Shift 331)
    (301, Token (AS _)) -> Just (Shift 332)
    (301, Token (QVARID _)) -> Just (Shift 333)
    (302, Token (QCONID _)) -> Just (Shift 299)
    (303, Token (RBRACE _)) -> Just (Reduce 3 175)
    (303, Token (LPAREN _)) -> Just (Reduce 3 175)
    (303, Token (RPAREN _)) -> Just (Reduce 3 175)
    (303, Token (COMMA _)) -> Just (Reduce 3 175)
    (303, Token (SEMICOLON _)) -> Just (Reduce 3 175)
    (303, Token (RARROW _)) -> Just (Reduce 3 175)
    (303, Token (LBRACKET _)) -> Just (Reduce 3 175)
    (303, Token (RBRACKET _)) -> Just (Reduce 3 175)
    (303, Token (EXCL _)) -> Just (Reduce 3 175)
    (303, Token (QCONID _)) -> Just (Reduce 3 175)
    (303, Token (EXPORT _)) -> Just (Reduce 3 175)
    (303, Token (AS _)) -> Just (Reduce 3 175)
    (303, Token (QVARID _)) -> Just (Reduce 3 175)
    (303, Token (STRING _)) -> Just (Reduce 3 175)
    (303, Token (INTEGER _)) -> Just (Reduce 3 175)
    (303, Token (QVARSYM _)) -> Just (Reduce 3 175)
    (303, Token (QCONSYM _)) -> Just (Reduce 3 175)
    (303, Token (BACKQUOTE _)) -> Just (Reduce 3 175)
    (304, Token (RBRACE _)) -> Just (Reduce 3 128)
    (305, Token (RBRACE _)) -> Just (Reduce 1 127)
    (305, Token (COMMA _)) -> Just (Shift 63)
    (306, Token (RBRACE _)) -> Just (Reduce 3 129)
    (306, Token (COMMA _)) -> Just (Reduce 3 129)
    (307, Token (COLON_COLON _)) -> Just (Shift 103)
    (308, Token (EXPORT _)) -> Just (Reduce 1 137)
    (308, Token (AS _)) -> Just (Reduce 1 137)
    (308, Token (QVARID _)) -> Just (Reduce 1 137)
    (308, Token (STRING _)) -> Just (Reduce 1 137)
    (309, Token (EXPORT _)) -> Just (Reduce 1 136)
    (309, Token (AS _)) -> Just (Reduce 1 136)
    (309, Token (QVARID _)) -> Just (Reduce 1 136)
    (309, Token (STRING _)) -> Just (Reduce 1 136)
    (310, Token (EXPORT _)) -> Just (Reduce 1 138)
    (310, Token (AS _)) -> Just (Reduce 1 138)
    (310, Token (QVARID _)) -> Just (Reduce 1 138)
    (310, Token (STRING _)) -> Just (Reduce 1 138)
    (311, Token (LPAREN _)) -> Just (Reduce 1 139)
    (311, Token (EXPORT _)) -> Just (Reduce 1 139)
    (311, Token (AS _)) -> Just (Reduce 1 139)
    (311, Token (QVARID _)) -> Just (Reduce 1 139)
    (311, Token (QVARSYM _)) -> Just (Reduce 1 139)
    (312, Token (STRING _)) -> Just (Reduce 1 142)
    (313, Token (STRING _)) -> Just (Reduce 1 141)
    (314, Token (STRING _)) -> Just (Reduce 1 143)
    (315, Token (LPAREN _)) -> Just (Reduce 1 140)
    (315, Token (EXPORT _)) -> Just (Reduce 1 140)
    (315, Token (AS _)) -> Just (Reduce 1 140)
    (315, Token (QVARID _)) -> Just (Reduce 1 140)
    (315, Token (QVARSYM _)) -> Just (Reduce 1 140)
    (316, Token (EQUAL _)) -> Just (Reduce 3 147)
    (317, Token (COMMA _)) -> Just (Shift 49)
    (317, Token (EQUAL _)) -> Just (Reduce 1 146)
    (318, Token (COMMA _)) -> Just (Reduce 2 149)
    (318, Token (EQUAL _)) -> Just (Reduce 2 149)
    (319, Token (COMMA _)) -> Just (Reduce 3 148)
    (319, Token (EQUAL _)) -> Just (Reduce 3 148)
    (320, Token (COMMA _)) -> Just (Reduce 1 150)
    (320, Token (EQUAL _)) -> Just (Reduce 1 150)
    (320, Token (LARROW _)) -> Just (Shift 50)
    (321, Token (WHERE _)) -> Just (Reduce 1 152)
    (321, Token (RBRACE _)) -> Just (Reduce 1 152)
    (321, Token (RPAREN _)) -> Just (Reduce 1 152)
    (321, Token (COMMA _)) -> Just (Reduce 1 152)
    (321, Token (SEMICOLON _)) -> Just (Reduce 1 152)
    (321, Token (EQUAL _)) -> Just (Reduce 1 152)
    (321, Token (PIPE _)) -> Just (Reduce 1 152)
    (321, Token (LARROW _)) -> Just (Reduce 1 152)
    (322, Token (WHERE _)) -> Just (Reduce 1 154)
    (322, Token (RBRACE _)) -> Just (Reduce 1 154)
    (322, Token (LPAREN _)) -> Just (Reduce 1 154)
    (322, Token (RPAREN _)) -> Just (Reduce 1 154)
    (322, Token (COMMA _)) -> Just (Reduce 1 154)
    (322, Token (SEMICOLON _)) -> Just (Reduce 1 154)
    (322, Token (EQUAL _)) -> Just (Reduce 1 154)
    (322, Token (PIPE _)) -> Just (Reduce 1 154)
    (322, Token (QCONID _)) -> Just (Reduce 1 154)
    (322, Token (EXPORT _)) -> Just (Reduce 1 154)
    (322, Token (AS _)) -> Just (Reduce 1 154)
    (322, Token (QVARID _)) -> Just (Reduce 1 154)
    (322, Token (STRING _)) -> Just (Reduce 1 154)
    (322, Token (LARROW _)) -> Just (Reduce 1 154)
    (322, Token (INTEGER _)) -> Just (Reduce 1 154)
    (322, Token (QVARSYM _)) -> Just (Reduce 1 154)
    (322, Token (QCONSYM _)) -> Just (Reduce 1 154)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 1 154)
    (323, Token (WHERE _)) -> Just (Reduce 3 156)
    (323, Token (RBRACE _)) -> Just (Reduce 3 156)
    (323, Token (LPAREN _)) -> Just (Reduce 3 156)
    (323, Token (RPAREN _)) -> Just (Reduce 3 156)
    (323, Token (COMMA _)) -> Just (Reduce 3 156)
    (323, Token (SEMICOLON _)) -> Just (Reduce 3 156)
    (323, Token (EQUAL _)) -> Just (Reduce 3 156)
    (323, Token (PIPE _)) -> Just (Reduce 3 156)
    (323, Token (QCONID _)) -> Just (Reduce 3 156)
    (323, Token (EXPORT _)) -> Just (Reduce 3 156)
    (323, Token (AS _)) -> Just (Reduce 3 156)
    (323, Token (QVARID _)) -> Just (Reduce 3 156)
    (323, Token (STRING _)) -> Just (Reduce 3 156)
    (323, Token (LARROW _)) -> Just (Reduce 3 156)
    (323, Token (INTEGER _)) -> Just (Reduce 3 156)
    (323, Token (QVARSYM _)) -> Just (Reduce 3 156)
    (323, Token (QCONSYM _)) -> Just (Reduce 3 156)
    (323, Token (BACKQUOTE _)) -> Just (Reduce 3 156)
    (324, Token (WHERE _)) -> Just (Reduce 2 155)
    (324, Token (RBRACE _)) -> Just (Reduce 2 155)
    (324, Token (LPAREN _)) -> Just (Reduce 2 155)
    (324, Token (RPAREN _)) -> Just (Reduce 2 155)
    (324, Token (COMMA _)) -> Just (Reduce 2 155)
    (324, Token (SEMICOLON _)) -> Just (Reduce 2 155)
    (324, Token (EQUAL _)) -> Just (Reduce 2 155)
    (324, Token (PIPE _)) -> Just (Reduce 2 155)
    (324, Token (QCONID _)) -> Just (Reduce 2 155)
    (324, Token (EXPORT _)) -> Just (Reduce 2 155)
    (324, Token (AS _)) -> Just (Reduce 2 155)
    (324, Token (QVARID _)) -> Just (Reduce 2 155)
    (324, Token (STRING _)) -> Just (Reduce 2 155)
    (324, Token (LARROW _)) -> Just (Reduce 2 155)
    (324, Token (INTEGER _)) -> Just (Reduce 2 155)
    (324, Token (QVARSYM _)) -> Just (Reduce 2 155)
    (324, Token (QCONSYM _)) -> Just (Reduce 2 155)
    (324, Token (BACKQUOTE _)) -> Just (Reduce 2 155)
    (325, Token (WHERE _)) -> Just (Reduce 3 160)
    (325, Token (RBRACE _)) -> Just (Reduce 3 160)
    (325, Token (LPAREN _)) -> Just (Reduce 3 160)
    (325, Token (RPAREN _)) -> Just (Reduce 3 160)
    (325, Token (COMMA _)) -> Just (Reduce 3 160)
    (325, Token (SEMICOLON _)) -> Just (Reduce 3 160)
    (325, Token (EQUAL _)) -> Just (Reduce 3 160)
    (325, Token (PIPE _)) -> Just (Reduce 3 160)
    (325, Token (QCONID _)) -> Just (Reduce 3 160)
    (325, Token (EXPORT _)) -> Just (Reduce 3 160)
    (325, Token (AS _)) -> Just (Reduce 3 160)
    (325, Token (QVARID _)) -> Just (Reduce 3 160)
    (325, Token (STRING _)) -> Just (Reduce 3 160)
    (325, Token (LARROW _)) -> Just (Reduce 3 160)
    (325, Token (INTEGER _)) -> Just (Reduce 3 160)
    (325, Token (QVARSYM _)) -> Just (Reduce 3 160)
    (325, Token (QCONSYM _)) -> Just (Reduce 3 160)
    (325, Token (BACKQUOTE _)) -> Just (Reduce 3 160)
    (326, Token (WHERE _)) -> Just (Reduce 1 159)
    (326, Token (RBRACE _)) -> Just (Reduce 1 159)
    (326, Token (LPAREN _)) -> Just (Reduce 1 159)
    (326, Token (RPAREN _)) -> Just (Reduce 1 159)
    (326, Token (COMMA _)) -> Just (Reduce 1 159)
    (326, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (326, Token (EQUAL _)) -> Just (Reduce 1 159)
    (326, Token (PIPE _)) -> Just (Reduce 1 159)
    (326, Token (QCONID _)) -> Just (Reduce 1 159)
    (326, Token (EXPORT _)) -> Just (Reduce 1 159)
    (326, Token (AS _)) -> Just (Reduce 1 159)
    (326, Token (QVARID _)) -> Just (Reduce 1 159)
    (326, Token (STRING _)) -> Just (Reduce 1 159)
    (326, Token (LARROW _)) -> Just (Reduce 1 159)
    (326, Token (INTEGER _)) -> Just (Reduce 1 159)
    (326, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (326, Token (QCONSYM _)) -> Just (Reduce 1 159)
    (326, Token (BACKQUOTE _)) -> Just (Reduce 1 159)
    (327, Token (WHERE _)) -> Just (Reduce 1 158)
    (327, Token (RBRACE _)) -> Just (Reduce 1 158)
    (327, Token (LPAREN _)) -> Just (Reduce 1 158)
    (327, Token (RPAREN _)) -> Just (Reduce 1 158)
    (327, Token (COMMA _)) -> Just (Reduce 1 158)
    (327, Token (SEMICOLON _)) -> Just (Reduce 1 158)
    (327, Token (EQUAL _)) -> Just (Reduce 1 158)
    (327, Token (PIPE _)) -> Just (Reduce 1 158)
    (327, Token (QCONID _)) -> Just (Reduce 1 158)
    (327, Token (EXPORT _)) -> Just (Reduce 1 158)
    (327, Token (AS _)) -> Just (Reduce 1 158)
    (327, Token (QVARID _)) -> Just (Reduce 1 158)
    (327, Token (STRING _)) -> Just (Reduce 1 158)
    (327, Token (LARROW _)) -> Just (Reduce 1 158)
    (327, Token (INTEGER _)) -> Just (Reduce 1 158)
    (327, Token (QVARSYM _)) -> Just (Reduce 1 158)
    (327, Token (QCONSYM _)) -> Just (Reduce 1 158)
    (327, Token (BACKQUOTE _)) -> Just (Reduce 1 158)
    (328, Token (WHERE _)) -> Just (Reduce 1 157)
    (328, Token (RBRACE _)) -> Just (Reduce 1 157)
    (328, Token (LPAREN _)) -> Just (Reduce 1 157)
    (328, Token (RPAREN _)) -> Just (Reduce 1 157)
    (328, Token (COMMA _)) -> Just (Reduce 1 157)
    (328, Token (SEMICOLON _)) -> Just (Reduce 1 157)
    (328, Token (EQUAL _)) -> Just (Reduce 1 157)
    (328, Token (PIPE _)) -> Just (Reduce 1 157)
    (328, Token (QCONID _)) -> Just (Reduce 1 157)
    (328, Token (EXPORT _)) -> Just (Reduce 1 157)
    (328, Token (AS _)) -> Just (Reduce 1 157)
    (328, Token (QVARID _)) -> Just (Reduce 1 157)
    (328, Token (STRING _)) -> Just (Reduce 1 157)
    (328, Token (LARROW _)) -> Just (Reduce 1 157)
    (328, Token (INTEGER _)) -> Just (Reduce 1 157)
    (328, Token (QVARSYM _)) -> Just (Reduce 1 157)
    (328, Token (QCONSYM _)) -> Just (Reduce 1 157)
    (328, Token (BACKQUOTE _)) -> Just (Reduce 1 157)
    (329, Token (RPAREN _)) -> Just (Shift 325)
    (330, Token (LPAREN _)) -> Just (Reduce 1 163)
    (330, Token (EQUAL _)) -> Just (Reduce 1 163)
    (330, Token (PIPE _)) -> Just (Reduce 1 163)
    (330, Token (EXPORT _)) -> Just (Reduce 1 163)
    (330, Token (AS _)) -> Just (Reduce 1 163)
    (330, Token (QVARID _)) -> Just (Reduce 1 163)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (331, Token (BACKQUOTE _)) -> Just (Shift 335)
    (332, Token (BACKQUOTE _)) -> Just (Shift 336)
    (333, Token (BACKQUOTE _)) -> Just (Shift 337)
    (334, Token (RBRACE _)) -> Just (Reduce 1 170)
    (334, Token (LPAREN _)) -> Just (Reduce 1 170)
    (334, Token (COMMA _)) -> Just (Reduce 1 170)
    (334, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (334, Token (QCONID _)) -> Just (Reduce 1 170)
    (334, Token (EXPORT _)) -> Just (Reduce 1 170)
    (334, Token (AS _)) -> Just (Reduce 1 170)
    (334, Token (QVARID _)) -> Just (Reduce 1 170)
    (334, Token (STRING _)) -> Just (Reduce 1 170)
    (334, Token (INTEGER _)) -> Just (Reduce 1 170)
    (334, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (334, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (334, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (335, Token (RBRACE _)) -> Just (Reduce 3 172)
    (335, Token (LPAREN _)) -> Just (Reduce 3 172)
    (335, Token (COMMA _)) -> Just (Reduce 3 172)
    (335, Token (SEMICOLON _)) -> Just (Reduce 3 172)
    (335, Token (QCONID _)) -> Just (Reduce 3 172)
    (335, Token (EXPORT _)) -> Just (Reduce 3 172)
    (335, Token (AS _)) -> Just (Reduce 3 172)
    (335, Token (QVARID _)) -> Just (Reduce 3 172)
    (335, Token (STRING _)) -> Just (Reduce 3 172)
    (335, Token (INTEGER _)) -> Just (Reduce 3 172)
    (335, Token (QVARSYM _)) -> Just (Reduce 3 172)
    (335, Token (QCONSYM _)) -> Just (Reduce 3 172)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 3 172)
    (336, Token (RBRACE _)) -> Just (Reduce 3 171)
    (336, Token (LPAREN _)) -> Just (Reduce 3 171)
    (336, Token (COMMA _)) -> Just (Reduce 3 171)
    (336, Token (SEMICOLON _)) -> Just (Reduce 3 171)
    (336, Token (QCONID _)) -> Just (Reduce 3 171)
    (336, Token (EXPORT _)) -> Just (Reduce 3 171)
    (336, Token (AS _)) -> Just (Reduce 3 171)
    (336, Token (QVARID _)) -> Just (Reduce 3 171)
    (336, Token (STRING _)) -> Just (Reduce 3 171)
    (336, Token (INTEGER _)) -> Just (Reduce 3 171)
    (336, Token (QVARSYM _)) -> Just (Reduce 3 171)
    (336, Token (QCONSYM _)) -> Just (Reduce 3 171)
    (336, Token (BACKQUOTE _)) -> Just (Reduce 3 171)
    (337, Token (RBRACE _)) -> Just (Reduce 3 173)
    (337, Token (LPAREN _)) -> Just (Reduce 3 173)
    (337, Token (COMMA _)) -> Just (Reduce 3 173)
    (337, Token (SEMICOLON _)) -> Just (Reduce 3 173)
    (337, Token (QCONID _)) -> Just (Reduce 3 173)
    (337, Token (EXPORT _)) -> Just (Reduce 3 173)
    (337, Token (AS _)) -> Just (Reduce 3 173)
    (337, Token (QVARID _)) -> Just (Reduce 3 173)
    (337, Token (STRING _)) -> Just (Reduce 3 173)
    (337, Token (INTEGER _)) -> Just (Reduce 3 173)
    (337, Token (QVARSYM _)) -> Just (Reduce 3 173)
    (337, Token (QCONSYM _)) -> Just (Reduce 3 173)
    (337, Token (BACKQUOTE _)) -> Just (Reduce 3 173)
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
production 149 = 60
production 150 = 60
production 151 = 32
production 152 = 61
production 153 = 62
production 154 = 63
production 155 = 63
production 156 = 63
production 157 = 64
production 158 = 64
production 159 = 64
production 160 = 64
production 161 = 31
production 162 = 31
production 163 = 65
production 164 = 8
production 165 = 8
production 166 = 8
production 167 = 8
production 168 = 9
production 169 = 9
production 170 = 66
production 171 = 66
production 172 = 66
production 173 = 66
production 174 = 52
production 175 = 52
production 176 = 44
production 177 = 44
production 178 = 16
production 179 = 16
production 180 = 15
production 181 = 15
production 182 = 47
production 183 = 47
production 184 = 47
production 185 = 67
production 186 = 1
production 187 = 42
production 188 = 42

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 158
    (9, 1) -> Just 183
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 259
    (13, 14) -> Just 18
    (13, 27) -> Just 181
    (13, 30) -> Just 217
    (13, 31) -> Just 51
    (13, 40) -> Just 231
    (13, 41) -> Just 232
    (13, 65) -> Just 235
    (16, 4) -> Just 17
    (16, 8) -> Just 259
    (16, 14) -> Just 18
    (16, 27) -> Just 181
    (16, 30) -> Just 217
    (16, 31) -> Just 51
    (16, 40) -> Just 231
    (16, 41) -> Just 232
    (16, 65) -> Just 235
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 132
    (25, 9) -> Just 133
    (25, 10) -> Just 33
    (25, 13) -> Just 122
    (34, 8) -> Just 328
    (34, 44) -> Just 35
    (34, 52) -> Just 268
    (34, 64) -> Just 324
    (34, 66) -> Just 269
    (35, 8) -> Just 328
    (35, 64) -> Just 323
    (36, 8) -> Just 328
    (36, 32) -> Just 238
    (36, 61) -> Just 237
    (36, 62) -> Just 321
    (36, 63) -> Just 34
    (36, 64) -> Just 322
    (37, 8) -> Just 328
    (37, 32) -> Just 218
    (37, 61) -> Just 237
    (37, 62) -> Just 321
    (37, 63) -> Just 34
    (37, 64) -> Just 322
    (38, 8) -> Just 328
    (38, 32) -> Just 248
    (38, 61) -> Just 237
    (38, 62) -> Just 321
    (38, 63) -> Just 34
    (38, 64) -> Just 322
    (39, 8) -> Just 328
    (39, 32) -> Just 256
    (39, 61) -> Just 237
    (39, 62) -> Just 321
    (39, 63) -> Just 34
    (39, 64) -> Just 322
    (40, 8) -> Just 328
    (40, 32) -> Just 329
    (40, 61) -> Just 237
    (40, 62) -> Just 321
    (40, 63) -> Just 34
    (40, 64) -> Just 322
    (41, 8) -> Just 259
    (41, 27) -> Just 227
    (41, 29) -> Just 226
    (41, 30) -> Just 217
    (41, 31) -> Just 51
    (41, 40) -> Just 231
    (41, 41) -> Just 232
    (41, 65) -> Just 235
    (42, 8) -> Just 259
    (42, 27) -> Just 227
    (42, 29) -> Just 228
    (42, 30) -> Just 217
    (42, 31) -> Just 51
    (42, 40) -> Just 231
    (42, 41) -> Just 232
    (42, 65) -> Just 235
    (43, 8) -> Just 259
    (43, 30) -> Just 247
    (43, 31) -> Just 54
    (43, 35) -> Just 242
    (43, 36) -> Just 244
    (43, 40) -> Just 231
    (43, 41) -> Just 232
    (43, 65) -> Just 235
    (44, 8) -> Just 259
    (44, 30) -> Just 247
    (44, 31) -> Just 54
    (44, 35) -> Just 243
    (44, 36) -> Just 244
    (44, 40) -> Just 231
    (44, 41) -> Just 232
    (44, 65) -> Just 235
    (45, 8) -> Just 328
    (45, 33) -> Just 219
    (45, 59) -> Just 240
    (45, 60) -> Just 317
    (45, 61) -> Just 320
    (45, 62) -> Just 321
    (45, 63) -> Just 34
    (45, 64) -> Just 322
    (46, 8) -> Just 328
    (46, 33) -> Just 239
    (46, 59) -> Just 240
    (46, 60) -> Just 317
    (46, 61) -> Just 320
    (46, 62) -> Just 321
    (46, 63) -> Just 34
    (46, 64) -> Just 322
    (47, 8) -> Just 328
    (47, 33) -> Just 249
    (47, 59) -> Just 240
    (47, 60) -> Just 317
    (47, 61) -> Just 320
    (47, 62) -> Just 321
    (47, 63) -> Just 34
    (47, 64) -> Just 322
    (48, 8) -> Just 328
    (48, 33) -> Just 257
    (48, 59) -> Just 240
    (48, 60) -> Just 317
    (48, 61) -> Just 320
    (48, 62) -> Just 321
    (48, 63) -> Just 34
    (48, 64) -> Just 322
    (49, 8) -> Just 328
    (49, 59) -> Just 316
    (49, 60) -> Just 317
    (49, 61) -> Just 320
    (49, 62) -> Just 321
    (49, 63) -> Just 34
    (49, 64) -> Just 322
    (50, 8) -> Just 328
    (50, 32) -> Just 319
    (50, 61) -> Just 237
    (50, 62) -> Just 321
    (50, 63) -> Just 34
    (50, 64) -> Just 322
    (51, 8) -> Just 330
    (51, 65) -> Just 236
    (52, 8) -> Just 330
    (52, 31) -> Just 55
    (52, 38) -> Just 251
    (52, 39) -> Just 253
    (52, 65) -> Just 235
    (53, 8) -> Just 330
    (53, 31) -> Just 55
    (53, 38) -> Just 252
    (53, 39) -> Just 253
    (53, 65) -> Just 235
    (54, 8) -> Just 330
    (54, 65) -> Just 236
    (55, 8) -> Just 330
    (55, 65) -> Just 236
    (56, 8) -> Just 129
    (56, 9) -> Just 130
    (56, 11) -> Just 123
    (56, 12) -> Just 124
    (57, 8) -> Just 129
    (57, 9) -> Just 130
    (57, 11) -> Just 159
    (57, 12) -> Just 124
    (58, 8) -> Just 129
    (58, 9) -> Just 130
    (58, 11) -> Just 160
    (58, 12) -> Just 124
    (59, 8) -> Just 132
    (59, 9) -> Just 133
    (59, 10) -> Just 121
    (59, 13) -> Just 122
    (60, 8) -> Just 132
    (60, 9) -> Just 133
    (60, 10) -> Just 131
    (60, 13) -> Just 122
    (61, 8) -> Just 258
    (61, 40) -> Just 260
    (62, 8) -> Just 258
    (62, 40) -> Just 307
    (62, 53) -> Just 298
    (62, 54) -> Just 305
    (63, 8) -> Just 258
    (63, 40) -> Just 307
    (63, 53) -> Just 304
    (63, 54) -> Just 305
    (64, 8) -> Just 193
    (65, 8) -> Just 204
    (66, 8) -> Just 205
    (67, 8) -> Just 206
    (75, 9) -> Just 284
    (75, 45) -> Just 275
    (75, 46) -> Just 276
    (75, 47) -> Just 277
    (76, 9) -> Just 284
    (76, 17) -> Just 77
    (76, 45) -> Just 184
    (76, 46) -> Just 276
    (76, 47) -> Just 277
    (77, 9) -> Just 284
    (77, 23) -> Just 176
    (77, 45) -> Just 185
    (77, 46) -> Just 276
    (77, 47) -> Just 277
    (78, 9) -> Just 284
    (78, 17) -> Just 79
    (78, 45) -> Just 184
    (78, 46) -> Just 276
    (78, 47) -> Just 277
    (79, 9) -> Just 284
    (79, 24) -> Just 178
    (79, 45) -> Just 185
    (79, 46) -> Just 276
    (79, 47) -> Just 277
    (80, 9) -> Just 284
    (80, 17) -> Just 81
    (80, 45) -> Just 184
    (80, 46) -> Just 276
    (80, 47) -> Just 277
    (81, 9) -> Just 284
    (81, 23) -> Just 175
    (81, 45) -> Just 185
    (81, 46) -> Just 276
    (81, 47) -> Just 277
    (82, 9) -> Just 284
    (82, 17) -> Just 83
    (82, 45) -> Just 184
    (82, 46) -> Just 276
    (82, 47) -> Just 277
    (83, 9) -> Just 284
    (83, 24) -> Just 177
    (83, 45) -> Just 185
    (83, 46) -> Just 276
    (83, 47) -> Just 277
    (84, 9) -> Just 284
    (84, 17) -> Just 85
    (84, 45) -> Just 184
    (84, 46) -> Just 276
    (84, 47) -> Just 277
    (85, 9) -> Just 284
    (85, 19) -> Just 163
    (85, 45) -> Just 185
    (85, 46) -> Just 276
    (85, 47) -> Just 277
    (86, 9) -> Just 284
    (86, 17) -> Just 87
    (86, 45) -> Just 184
    (86, 46) -> Just 276
    (86, 47) -> Just 277
    (87, 9) -> Just 284
    (87, 19) -> Just 164
    (87, 45) -> Just 185
    (87, 46) -> Just 276
    (87, 47) -> Just 277
    (88, 9) -> Just 285
    (88, 17) -> Just 91
    (88, 45) -> Just 184
    (88, 46) -> Just 276
    (88, 47) -> Just 277
    (88, 50) -> Just 187
    (88, 51) -> Just 295
    (89, 9) -> Just 285
    (89, 17) -> Just 91
    (89, 45) -> Just 184
    (89, 46) -> Just 276
    (89, 47) -> Just 277
    (89, 50) -> Just 294
    (89, 51) -> Just 295
    (90, 9) -> Just 105
    (91, 9) -> Just 284
    (91, 45) -> Just 185
    (91, 46) -> Just 276
    (91, 47) -> Just 277
    (91, 52) -> Just 92
    (92, 9) -> Just 284
    (92, 17) -> Just 93
    (92, 45) -> Just 184
    (92, 46) -> Just 276
    (92, 47) -> Just 277
    (93, 9) -> Just 284
    (93, 45) -> Just 185
    (93, 46) -> Just 276
    (93, 47) -> Just 277
    (94, 9) -> Just 284
    (94, 17) -> Just 95
    (94, 18) -> Just 230
    (94, 45) -> Just 184
    (94, 46) -> Just 276
    (94, 47) -> Just 277
    (95, 9) -> Just 284
    (95, 45) -> Just 185
    (95, 46) -> Just 276
    (95, 47) -> Just 277
    (96, 9) -> Just 284
    (96, 17) -> Just 102
    (96, 18) -> Just 162
    (96, 45) -> Just 184
    (96, 46) -> Just 276
    (96, 47) -> Just 277
    (97, 9) -> Just 284
    (97, 17) -> Just 102
    (97, 18) -> Just 186
    (97, 45) -> Just 184
    (97, 46) -> Just 276
    (97, 47) -> Just 277
    (98, 9) -> Just 284
    (98, 17) -> Just 102
    (98, 18) -> Just 207
    (98, 45) -> Just 184
    (98, 46) -> Just 276
    (98, 47) -> Just 277
    (99, 9) -> Just 284
    (99, 17) -> Just 102
    (99, 18) -> Just 208
    (99, 45) -> Just 184
    (99, 46) -> Just 276
    (99, 47) -> Just 277
    (100, 9) -> Just 284
    (100, 17) -> Just 102
    (100, 18) -> Just 209
    (100, 45) -> Just 184
    (100, 46) -> Just 276
    (100, 47) -> Just 277
    (101, 9) -> Just 284
    (101, 17) -> Just 102
    (101, 18) -> Just 229
    (101, 45) -> Just 184
    (101, 46) -> Just 276
    (101, 47) -> Just 277
    (102, 9) -> Just 284
    (102, 45) -> Just 185
    (102, 46) -> Just 276
    (102, 47) -> Just 277
    (103, 9) -> Just 284
    (103, 17) -> Just 102
    (103, 18) -> Just 306
    (103, 45) -> Just 184
    (103, 46) -> Just 276
    (103, 47) -> Just 277
    (104, 9) -> Just 284
    (104, 17) -> Just 102
    (104, 18) -> Just 194
    (104, 45) -> Just 184
    (104, 46) -> Just 276
    (104, 47) -> Just 277
    (105, 9) -> Just 284
    (105, 45) -> Just 195
    (105, 46) -> Just 276
    (105, 47) -> Just 277
    (106, 9) -> Just 284
    (106, 17) -> Just 107
    (106, 45) -> Just 184
    (106, 46) -> Just 276
    (106, 47) -> Just 277
    (107, 9) -> Just 284
    (107, 22) -> Just 174
    (107, 45) -> Just 185
    (107, 46) -> Just 276
    (107, 47) -> Just 277
    (108, 9) -> Just 284
    (108, 17) -> Just 110
    (108, 45) -> Just 184
    (108, 46) -> Just 276
    (108, 47) -> Just 277
    (109, 9) -> Just 284
    (109, 17) -> Just 111
    (109, 45) -> Just 184
    (109, 46) -> Just 276
    (109, 47) -> Just 277
    (110, 9) -> Just 284
    (110, 45) -> Just 185
    (110, 46) -> Just 276
    (110, 47) -> Just 277
    (111, 9) -> Just 284
    (111, 22) -> Just 173
    (111, 45) -> Just 185
    (111, 46) -> Just 276
    (111, 47) -> Just 277
    (112, 9) -> Just 284
    (112, 17) -> Just 102
    (112, 18) -> Just 273
    (112, 45) -> Just 184
    (112, 46) -> Just 276
    (112, 47) -> Just 277
    (112, 48) -> Just 278
    (112, 49) -> Just 286
    (113, 9) -> Just 284
    (113, 17) -> Just 102
    (113, 18) -> Just 200
    (113, 25) -> Just 179
    (113, 45) -> Just 184
    (113, 46) -> Just 276
    (113, 47) -> Just 277
    (114, 9) -> Just 284
    (114, 17) -> Just 102
    (114, 18) -> Just 200
    (114, 25) -> Just 201
    (114, 45) -> Just 184
    (114, 46) -> Just 276
    (114, 47) -> Just 277
    (115, 9) -> Just 284
    (115, 17) -> Just 102
    (115, 18) -> Just 290
    (115, 45) -> Just 184
    (115, 46) -> Just 276
    (115, 47) -> Just 277
    (115, 48) -> Just 291
    (116, 9) -> Just 284
    (116, 17) -> Just 102
    (116, 18) -> Just 274
    (116, 45) -> Just 184
    (116, 46) -> Just 276
    (116, 47) -> Just 277
    (134, 20) -> Just 190
    (134, 21) -> Just 169
    (135, 20) -> Just 190
    (135, 21) -> Just 170
    (136, 20) -> Just 190
    (136, 21) -> Just 171
    (137, 20) -> Just 190
    (137, 21) -> Just 172
    (150, 15) -> Just 8
    (152, 20) -> Just 165
    (153, 20) -> Just 166
    (154, 20) -> Just 167
    (155, 20) -> Just 168
    (157, 26) -> Just 180
    (158, 16) -> Just 161
    (188, 20) -> Just 190
    (188, 21) -> Just 191
    (196, 34) -> Just 197
    (198, 37) -> Just 199
    (202, 55) -> Just 210
    (203, 55) -> Just 211
    (210, 56) -> Just 65
    (210, 57) -> Just 212
    (211, 58) -> Just 67
    (212, 56) -> Just 66
    (213, 28) -> Just 215
    (214, 28) -> Just 216
    (220, 28) -> Just 245
    (221, 28) -> Just 246
    (222, 28) -> Just 254
    (223, 28) -> Just 255
    (224, 28) -> Just 318
    (232, 42) -> Just 233
    (233, 43) -> Just 234
    (233, 44) -> Just 267
    (233, 52) -> Just 268
    (233, 66) -> Just 269
    (265, 43) -> Just 266
    (265, 44) -> Just 267
    (265, 52) -> Just 268
    (265, 66) -> Just 269
    (292, 49) -> Just 293
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
                  Token (LARROW semanticValue) ->
                    StackValue_LARROW semanticValue
                  Token (LET semanticValue) ->
                    StackValue_LET semanticValue
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
                      Monad.liftM StackValue_guard $ guard_implies_infixexp_LARROW_exp actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_LARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_exp value -> value; _ -> undefined })
                    149 ->
                      Monad.liftM StackValue_guard $ guard_implies_LET_decls actions (case snd (pop !! 1) of { StackValue_LET value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_decls value -> value; _ -> undefined })
                    150 ->
                      Monad.liftM StackValue_guard $ guard_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    151 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    179 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    181 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    188 ->
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
  , guard_implies_infixexp_LARROW_exp = \infixexp0 lARROW1 exp2 ->
      return $ Guard_implies_infixexp_LARROW_exp infixexp0 lARROW1 exp2
  , guard_implies_LET_decls = \lET0 decls1 ->
      return $ Guard_implies_LET_decls lET0 decls1
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

