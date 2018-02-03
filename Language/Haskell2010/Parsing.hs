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
  | Pat_implies_pat_op_apat Pat Op Apat
  deriving (Eq, Ord, Read, Show)

data Exp =
    Exp_implies_infixexp_COLON_COLON_type' Infixexp COLON_COLON Type'
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
  , exp_implies_infixexp_COLON_COLON_type' :: Infixexp -> COLON_COLON -> Type' -> m Exp
  , exp_implies_infixexp_COLON_COLON_btype_DARROW_type' :: Infixexp -> COLON_COLON -> Btype -> DARROW -> Type' -> m Exp
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
  , pat_implies_pat_op_apat :: Pat -> Op -> Apat -> m Pat
  , apat_implies_var :: Var -> m Apat
  , apat_implies_LPAREN_pat_RPAREN :: LPAREN -> Pat -> RPAREN -> m Apat
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
    (11, Token (MODULE _)) -> Just (Reduce 1 190)
    (11, Token (WHERE _)) -> Just (Reduce 1 190)
    (11, Token (RBRACE _)) -> Just (Reduce 1 190)
    (11, Token (LPAREN _)) -> Just (Reduce 1 190)
    (11, Token (RPAREN _)) -> Just (Reduce 1 190)
    (11, Token (COMMA _)) -> Just (Reduce 1 190)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 190)
    (11, Token (HIDING _)) -> Just (Reduce 1 190)
    (11, Token (QCONID _)) -> Just (Reduce 1 190)
    (11, Token (EXPORT _)) -> Just (Reduce 1 190)
    (11, Token (AS _)) -> Just (Reduce 1 190)
    (11, Token (QVARID _)) -> Just (Reduce 1 190)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 190)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 190)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 42)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 156)
    (13, Token (TYPE _)) -> Just (Shift 114)
    (13, Token (DATA _)) -> Just (Shift 92)
    (13, Token (NEWTYPE _)) -> Just (Shift 112)
    (13, Token (CLASS _)) -> Just (Shift 79)
    (13, Token (INSTANCE _)) -> Just (Shift 81)
    (13, Token (DEFAULT _)) -> Just (Shift 162)
    (13, Token (FOREIGN _)) -> Just (Shift 163)
    (13, Token (INFIXL _)) -> Just (Shift 270)
    (13, Token (INFIXR _)) -> Just (Shift 271)
    (13, Token (INFIX _)) -> Just (Shift 272)
    (13, Token (EXPORT _)) -> Just (Shift 74)
    (13, Token (AS _)) -> Just (Shift 75)
    (13, Token (QVARID _)) -> Just (Shift 76)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 42)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 156)
    (16, Token (TYPE _)) -> Just (Shift 114)
    (16, Token (DATA _)) -> Just (Shift 92)
    (16, Token (NEWTYPE _)) -> Just (Shift 112)
    (16, Token (CLASS _)) -> Just (Shift 79)
    (16, Token (INSTANCE _)) -> Just (Shift 81)
    (16, Token (DEFAULT _)) -> Just (Shift 162)
    (16, Token (FOREIGN _)) -> Just (Shift 163)
    (16, Token (INFIXL _)) -> Just (Shift 270)
    (16, Token (INFIXR _)) -> Just (Shift 271)
    (16, Token (INFIX _)) -> Just (Shift 272)
    (16, Token (EXPORT _)) -> Just (Shift 74)
    (16, Token (AS _)) -> Just (Shift 75)
    (16, Token (QVARID _)) -> Just (Shift 76)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 71)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 125)
    (19, Token (EXPORT _)) -> Just (Shift 74)
    (19, Token (AS _)) -> Just (Shift 75)
    (19, Token (QVARID _)) -> Just (Shift 76)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 71)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 125)
    (22, Token (EXPORT _)) -> Just (Shift 74)
    (22, Token (AS _)) -> Just (Shift 75)
    (22, Token (QVARID _)) -> Just (Shift 76)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 71)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 125)
    (25, Token (EXPORT _)) -> Just (Shift 74)
    (25, Token (AS _)) -> Just (Shift 75)
    (25, Token (QVARID _)) -> Just (Shift 76)
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
    (34, Token (WHERE _)) -> Just (Reduce 1 155)
    (34, Token (RBRACE _)) -> Just (Reduce 1 155)
    (34, Token (LPAREN _)) -> Just (Shift 41)
    (34, Token (RPAREN _)) -> Just (Reduce 1 155)
    (34, Token (COMMA _)) -> Just (Reduce 1 155)
    (34, Token (SEMICOLON _)) -> Just (Reduce 1 155)
    (34, Token (EQUAL _)) -> Just (Reduce 1 155)
    (34, Token (PIPE _)) -> Just (Reduce 1 155)
    (34, Token (COLON_COLON _)) -> Just (Reduce 1 155)
    (34, Token (EXPORT _)) -> Just (Shift 74)
    (34, Token (AS _)) -> Just (Shift 75)
    (34, Token (QVARID _)) -> Just (Shift 76)
    (34, Token (STRING _)) -> Just (Shift 335)
    (34, Token (LARROW _)) -> Just (Reduce 1 155)
    (34, Token (INTEGER _)) -> Just (Shift 336)
    (34, Token (QVARSYM _)) -> Just (Shift 344)
    (34, Token (QCONSYM _)) -> Just (Shift 309)
    (34, Token (BACKQUOTE _)) -> Just (Shift 310)
    (35, Token (LPAREN _)) -> Just (Shift 41)
    (35, Token (EXPORT _)) -> Just (Shift 74)
    (35, Token (AS _)) -> Just (Shift 75)
    (35, Token (QVARID _)) -> Just (Shift 76)
    (35, Token (STRING _)) -> Just (Shift 335)
    (35, Token (INTEGER _)) -> Just (Shift 336)
    (36, Token (LPAREN _)) -> Just (Shift 41)
    (36, Token (EXPORT _)) -> Just (Shift 74)
    (36, Token (AS _)) -> Just (Shift 75)
    (36, Token (QVARID _)) -> Just (Shift 76)
    (36, Token (STRING _)) -> Just (Shift 335)
    (36, Token (INTEGER _)) -> Just (Shift 336)
    (37, Token (LPAREN _)) -> Just (Shift 41)
    (37, Token (EXPORT _)) -> Just (Shift 74)
    (37, Token (AS _)) -> Just (Shift 75)
    (37, Token (QVARID _)) -> Just (Shift 76)
    (37, Token (STRING _)) -> Just (Shift 335)
    (37, Token (INTEGER _)) -> Just (Shift 336)
    (38, Token (LPAREN _)) -> Just (Shift 41)
    (38, Token (EXPORT _)) -> Just (Shift 74)
    (38, Token (AS _)) -> Just (Shift 75)
    (38, Token (QVARID _)) -> Just (Shift 76)
    (38, Token (STRING _)) -> Just (Shift 335)
    (38, Token (INTEGER _)) -> Just (Shift 336)
    (39, Token (LPAREN _)) -> Just (Shift 41)
    (39, Token (EXPORT _)) -> Just (Shift 74)
    (39, Token (AS _)) -> Just (Shift 75)
    (39, Token (QVARID _)) -> Just (Shift 76)
    (39, Token (STRING _)) -> Just (Shift 335)
    (39, Token (INTEGER _)) -> Just (Shift 336)
    (40, Token (LPAREN _)) -> Just (Shift 42)
    (40, Token (EXPORT _)) -> Just (Shift 74)
    (40, Token (AS _)) -> Just (Shift 75)
    (40, Token (QVARID _)) -> Just (Shift 76)
    (41, Token (LPAREN _)) -> Just (Shift 41)
    (41, Token (EXPORT _)) -> Just (Shift 74)
    (41, Token (AS _)) -> Just (Shift 75)
    (41, Token (QVARID _)) -> Just (Shift 76)
    (41, Token (STRING _)) -> Just (Shift 335)
    (41, Token (INTEGER _)) -> Just (Shift 336)
    (41, Token (QVARSYM _)) -> Just (Shift 77)
    (42, Token (LPAREN _)) -> Just (Shift 42)
    (42, Token (EXPORT _)) -> Just (Shift 74)
    (42, Token (AS _)) -> Just (Shift 75)
    (42, Token (QVARID _)) -> Just (Shift 76)
    (42, Token (QVARSYM _)) -> Just (Shift 77)
    (43, Token (LPAREN _)) -> Just (Shift 42)
    (43, Token (RPAREN _)) -> Just (Shift 339)
    (43, Token (EXPORT _)) -> Just (Shift 74)
    (43, Token (AS _)) -> Just (Shift 75)
    (43, Token (QVARID _)) -> Just (Shift 76)
    (43, Token (QVARSYM _)) -> Just (Shift 344)
    (43, Token (QCONSYM _)) -> Just (Shift 309)
    (43, Token (BACKQUOTE _)) -> Just (Shift 310)
    (44, Token (RBRACE _)) -> Just (Reduce 0 85)
    (44, Token (LPAREN _)) -> Just (Shift 42)
    (44, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (44, Token (INFIXL _)) -> Just (Shift 270)
    (44, Token (INFIXR _)) -> Just (Shift 271)
    (44, Token (INFIX _)) -> Just (Shift 272)
    (44, Token (EXPORT _)) -> Just (Shift 74)
    (44, Token (AS _)) -> Just (Shift 75)
    (44, Token (QVARID _)) -> Just (Shift 76)
    (45, Token (RBRACE _)) -> Just (Reduce 0 85)
    (45, Token (LPAREN _)) -> Just (Shift 42)
    (45, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (45, Token (INFIXL _)) -> Just (Shift 270)
    (45, Token (INFIXR _)) -> Just (Shift 271)
    (45, Token (INFIX _)) -> Just (Shift 272)
    (45, Token (EXPORT _)) -> Just (Shift 74)
    (45, Token (AS _)) -> Just (Shift 75)
    (45, Token (QVARID _)) -> Just (Shift 76)
    (46, Token (RBRACE _)) -> Just (Reduce 0 85)
    (46, Token (LPAREN _)) -> Just (Shift 42)
    (46, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (46, Token (INFIXL _)) -> Just (Shift 270)
    (46, Token (INFIXR _)) -> Just (Shift 271)
    (46, Token (INFIX _)) -> Just (Shift 272)
    (46, Token (EXPORT _)) -> Just (Shift 74)
    (46, Token (AS _)) -> Just (Shift 75)
    (46, Token (QVARID _)) -> Just (Shift 76)
    (47, Token (RBRACE _)) -> Just (Reduce 0 85)
    (47, Token (LPAREN _)) -> Just (Shift 42)
    (47, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (47, Token (INFIXL _)) -> Just (Shift 270)
    (47, Token (INFIXR _)) -> Just (Shift 271)
    (47, Token (INFIX _)) -> Just (Shift 272)
    (47, Token (EXPORT _)) -> Just (Shift 74)
    (47, Token (AS _)) -> Just (Shift 75)
    (47, Token (QVARID _)) -> Just (Shift 76)
    (48, Token (LPAREN _)) -> Just (Shift 41)
    (48, Token (EXPORT _)) -> Just (Shift 74)
    (48, Token (AS _)) -> Just (Shift 75)
    (48, Token (QVARID _)) -> Just (Shift 76)
    (48, Token (STRING _)) -> Just (Shift 335)
    (48, Token (LET _)) -> Just (Shift 230)
    (48, Token (INTEGER _)) -> Just (Shift 336)
    (49, Token (LPAREN _)) -> Just (Shift 41)
    (49, Token (EXPORT _)) -> Just (Shift 74)
    (49, Token (AS _)) -> Just (Shift 75)
    (49, Token (QVARID _)) -> Just (Shift 76)
    (49, Token (STRING _)) -> Just (Shift 335)
    (49, Token (LET _)) -> Just (Shift 230)
    (49, Token (INTEGER _)) -> Just (Shift 336)
    (50, Token (LPAREN _)) -> Just (Shift 41)
    (50, Token (EXPORT _)) -> Just (Shift 74)
    (50, Token (AS _)) -> Just (Shift 75)
    (50, Token (QVARID _)) -> Just (Shift 76)
    (50, Token (STRING _)) -> Just (Shift 335)
    (50, Token (LET _)) -> Just (Shift 230)
    (50, Token (INTEGER _)) -> Just (Shift 336)
    (51, Token (LPAREN _)) -> Just (Shift 41)
    (51, Token (EXPORT _)) -> Just (Shift 74)
    (51, Token (AS _)) -> Just (Shift 75)
    (51, Token (QVARID _)) -> Just (Shift 76)
    (51, Token (STRING _)) -> Just (Shift 335)
    (51, Token (LET _)) -> Just (Shift 230)
    (51, Token (INTEGER _)) -> Just (Shift 336)
    (52, Token (LPAREN _)) -> Just (Shift 41)
    (52, Token (EXPORT _)) -> Just (Shift 74)
    (52, Token (AS _)) -> Just (Shift 75)
    (52, Token (QVARID _)) -> Just (Shift 76)
    (52, Token (STRING _)) -> Just (Shift 335)
    (52, Token (LET _)) -> Just (Shift 230)
    (52, Token (INTEGER _)) -> Just (Shift 336)
    (53, Token (LPAREN _)) -> Just (Shift 41)
    (53, Token (EXPORT _)) -> Just (Shift 74)
    (53, Token (AS _)) -> Just (Shift 75)
    (53, Token (QVARID _)) -> Just (Shift 76)
    (53, Token (STRING _)) -> Just (Shift 335)
    (53, Token (INTEGER _)) -> Just (Shift 336)
    (54, Token (LPAREN _)) -> Just (Shift 42)
    (54, Token (EQUAL _)) -> Just (Shift 37)
    (54, Token (PIPE _)) -> Just (Shift 48)
    (54, Token (EXPORT _)) -> Just (Shift 74)
    (54, Token (AS _)) -> Just (Shift 75)
    (54, Token (QVARID _)) -> Just (Shift 76)
    (54, Token (QVARSYM _)) -> Just (Shift 344)
    (54, Token (QCONSYM _)) -> Just (Shift 309)
    (54, Token (BACKQUOTE _)) -> Just (Shift 310)
    (55, Token (RBRACE _)) -> Just (Reduce 0 80)
    (55, Token (LPAREN _)) -> Just (Shift 42)
    (55, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (55, Token (EXPORT _)) -> Just (Shift 74)
    (55, Token (AS _)) -> Just (Shift 75)
    (55, Token (QVARID _)) -> Just (Shift 76)
    (56, Token (RBRACE _)) -> Just (Reduce 0 80)
    (56, Token (LPAREN _)) -> Just (Shift 42)
    (56, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (56, Token (EXPORT _)) -> Just (Shift 74)
    (56, Token (AS _)) -> Just (Shift 75)
    (56, Token (QVARID _)) -> Just (Shift 76)
    (57, Token (LPAREN _)) -> Just (Shift 42)
    (57, Token (EQUAL _)) -> Just (Shift 38)
    (57, Token (PIPE _)) -> Just (Shift 50)
    (57, Token (EXPORT _)) -> Just (Shift 74)
    (57, Token (AS _)) -> Just (Shift 75)
    (57, Token (QVARID _)) -> Just (Shift 76)
    (57, Token (QVARSYM _)) -> Just (Shift 344)
    (57, Token (QCONSYM _)) -> Just (Shift 309)
    (57, Token (BACKQUOTE _)) -> Just (Shift 310)
    (58, Token (LPAREN _)) -> Just (Shift 42)
    (58, Token (EQUAL _)) -> Just (Shift 39)
    (58, Token (PIPE _)) -> Just (Shift 51)
    (58, Token (EXPORT _)) -> Just (Shift 74)
    (58, Token (AS _)) -> Just (Shift 75)
    (58, Token (QVARID _)) -> Just (Shift 76)
    (58, Token (QVARSYM _)) -> Just (Shift 344)
    (58, Token (QCONSYM _)) -> Just (Shift 309)
    (58, Token (BACKQUOTE _)) -> Just (Shift 310)
    (59, Token (LPAREN _)) -> Just (Shift 71)
    (59, Token (RPAREN _)) -> Just (Reduce 0 15)
    (59, Token (QCONID _)) -> Just (Shift 125)
    (59, Token (EXPORT _)) -> Just (Shift 74)
    (59, Token (AS _)) -> Just (Shift 75)
    (59, Token (QVARID _)) -> Just (Shift 76)
    (60, Token (LPAREN _)) -> Just (Shift 71)
    (60, Token (RPAREN _)) -> Just (Reduce 0 15)
    (60, Token (QCONID _)) -> Just (Shift 125)
    (60, Token (EXPORT _)) -> Just (Shift 74)
    (60, Token (AS _)) -> Just (Shift 75)
    (60, Token (QVARID _)) -> Just (Shift 76)
    (61, Token (LPAREN _)) -> Just (Shift 71)
    (61, Token (RPAREN _)) -> Just (Reduce 0 15)
    (61, Token (QCONID _)) -> Just (Shift 125)
    (61, Token (EXPORT _)) -> Just (Shift 74)
    (61, Token (AS _)) -> Just (Shift 75)
    (61, Token (QVARID _)) -> Just (Shift 76)
    (62, Token (LPAREN _)) -> Just (Shift 71)
    (62, Token (QCONID _)) -> Just (Shift 125)
    (62, Token (EXPORT _)) -> Just (Shift 74)
    (62, Token (AS _)) -> Just (Shift 75)
    (62, Token (QVARID _)) -> Just (Shift 76)
    (63, Token (LPAREN _)) -> Just (Shift 71)
    (63, Token (RPAREN _)) -> Just (Shift 131)
    (63, Token (DOT_DOT _)) -> Just (Shift 134)
    (63, Token (QCONID _)) -> Just (Shift 125)
    (63, Token (EXPORT _)) -> Just (Shift 74)
    (63, Token (AS _)) -> Just (Shift 75)
    (63, Token (QVARID _)) -> Just (Shift 76)
    (64, Token (LPAREN _)) -> Just (Shift 72)
    (64, Token (EXPORT _)) -> Just (Shift 74)
    (64, Token (AS _)) -> Just (Shift 75)
    (64, Token (QVARID _)) -> Just (Shift 76)
    (65, Token (RBRACE _)) -> Just (Shift 305)
    (65, Token (LPAREN _)) -> Just (Shift 72)
    (65, Token (EXPORT _)) -> Just (Shift 74)
    (65, Token (AS _)) -> Just (Shift 75)
    (65, Token (QVARID _)) -> Just (Shift 76)
    (66, Token (LPAREN _)) -> Just (Shift 72)
    (66, Token (EXPORT _)) -> Just (Shift 74)
    (66, Token (AS _)) -> Just (Shift 75)
    (66, Token (QVARID _)) -> Just (Shift 76)
    (67, Token (LPAREN _)) -> Just (Shift 72)
    (67, Token (EXPORT _)) -> Just (Shift 74)
    (67, Token (AS _)) -> Just (Shift 75)
    (67, Token (QVARID _)) -> Just (Shift 76)
    (68, Token (LPAREN _)) -> Just (Shift 72)
    (68, Token (EXPORT _)) -> Just (Shift 74)
    (68, Token (AS _)) -> Just (Shift 75)
    (68, Token (QVARID _)) -> Just (Shift 76)
    (69, Token (LPAREN _)) -> Just (Shift 72)
    (69, Token (EXPORT _)) -> Just (Shift 74)
    (69, Token (AS _)) -> Just (Shift 75)
    (69, Token (QVARID _)) -> Just (Shift 76)
    (70, Token (LPAREN _)) -> Just (Shift 72)
    (70, Token (EXPORT _)) -> Just (Shift 74)
    (70, Token (AS _)) -> Just (Shift 75)
    (70, Token (QVARID _)) -> Just (Shift 76)
    (71, Token (QVARSYM _)) -> Just (Shift 77)
    (71, Token (QCONSYM _)) -> Just (Shift 126)
    (72, Token (QVARSYM _)) -> Just (Shift 77)
    (73, Token (WHERE _)) -> Just (Reduce 3 171)
    (73, Token (RBRACE _)) -> Just (Reduce 3 171)
    (73, Token (LPAREN _)) -> Just (Reduce 3 171)
    (73, Token (RPAREN _)) -> Just (Reduce 3 171)
    (73, Token (COMMA _)) -> Just (Reduce 3 171)
    (73, Token (SEMICOLON _)) -> Just (Reduce 3 171)
    (73, Token (EQUAL _)) -> Just (Reduce 3 171)
    (73, Token (PIPE _)) -> Just (Reduce 3 171)
    (73, Token (COLON_COLON _)) -> Just (Reduce 3 171)
    (73, Token (QCONID _)) -> Just (Reduce 3 171)
    (73, Token (EXPORT _)) -> Just (Reduce 3 171)
    (73, Token (AS _)) -> Just (Reduce 3 171)
    (73, Token (QVARID _)) -> Just (Reduce 3 171)
    (73, Token (STRING _)) -> Just (Reduce 3 171)
    (73, Token (LARROW _)) -> Just (Reduce 3 171)
    (73, Token (INTEGER _)) -> Just (Reduce 3 171)
    (73, Token (QVARSYM _)) -> Just (Reduce 3 171)
    (73, Token (QCONSYM _)) -> Just (Reduce 3 171)
    (73, Token (BACKQUOTE _)) -> Just (Reduce 3 171)
    (74, Token (WHERE _)) -> Just (Reduce 1 169)
    (74, Token (RBRACE _)) -> Just (Reduce 1 169)
    (74, Token (LPAREN _)) -> Just (Reduce 1 169)
    (74, Token (RPAREN _)) -> Just (Reduce 1 169)
    (74, Token (COMMA _)) -> Just (Reduce 1 169)
    (74, Token (SEMICOLON _)) -> Just (Reduce 1 169)
    (74, Token (EQUAL _)) -> Just (Reduce 1 169)
    (74, Token (PIPE _)) -> Just (Reduce 1 169)
    (74, Token (COLON_COLON _)) -> Just (Reduce 1 169)
    (74, Token (QCONID _)) -> Just (Reduce 1 169)
    (74, Token (EXPORT _)) -> Just (Reduce 1 169)
    (74, Token (AS _)) -> Just (Reduce 1 169)
    (74, Token (QVARID _)) -> Just (Reduce 1 169)
    (74, Token (STRING _)) -> Just (Reduce 1 169)
    (74, Token (LARROW _)) -> Just (Reduce 1 169)
    (74, Token (INTEGER _)) -> Just (Reduce 1 169)
    (74, Token (QVARSYM _)) -> Just (Reduce 1 169)
    (74, Token (QCONSYM _)) -> Just (Reduce 1 169)
    (74, Token (BACKQUOTE _)) -> Just (Reduce 1 169)
    (75, Token (WHERE _)) -> Just (Reduce 1 168)
    (75, Token (RBRACE _)) -> Just (Reduce 1 168)
    (75, Token (LPAREN _)) -> Just (Reduce 1 168)
    (75, Token (RPAREN _)) -> Just (Reduce 1 168)
    (75, Token (COMMA _)) -> Just (Reduce 1 168)
    (75, Token (SEMICOLON _)) -> Just (Reduce 1 168)
    (75, Token (EQUAL _)) -> Just (Reduce 1 168)
    (75, Token (PIPE _)) -> Just (Reduce 1 168)
    (75, Token (COLON_COLON _)) -> Just (Reduce 1 168)
    (75, Token (QCONID _)) -> Just (Reduce 1 168)
    (75, Token (EXPORT _)) -> Just (Reduce 1 168)
    (75, Token (AS _)) -> Just (Reduce 1 168)
    (75, Token (QVARID _)) -> Just (Reduce 1 168)
    (75, Token (STRING _)) -> Just (Reduce 1 168)
    (75, Token (LARROW _)) -> Just (Reduce 1 168)
    (75, Token (INTEGER _)) -> Just (Reduce 1 168)
    (75, Token (QVARSYM _)) -> Just (Reduce 1 168)
    (75, Token (QCONSYM _)) -> Just (Reduce 1 168)
    (75, Token (BACKQUOTE _)) -> Just (Reduce 1 168)
    (76, Token (WHERE _)) -> Just (Reduce 1 170)
    (76, Token (RBRACE _)) -> Just (Reduce 1 170)
    (76, Token (LPAREN _)) -> Just (Reduce 1 170)
    (76, Token (RPAREN _)) -> Just (Reduce 1 170)
    (76, Token (COMMA _)) -> Just (Reduce 1 170)
    (76, Token (SEMICOLON _)) -> Just (Reduce 1 170)
    (76, Token (EQUAL _)) -> Just (Reduce 1 170)
    (76, Token (PIPE _)) -> Just (Reduce 1 170)
    (76, Token (COLON_COLON _)) -> Just (Reduce 1 170)
    (76, Token (QCONID _)) -> Just (Reduce 1 170)
    (76, Token (EXPORT _)) -> Just (Reduce 1 170)
    (76, Token (AS _)) -> Just (Reduce 1 170)
    (76, Token (QVARID _)) -> Just (Reduce 1 170)
    (76, Token (STRING _)) -> Just (Reduce 1 170)
    (76, Token (LARROW _)) -> Just (Reduce 1 170)
    (76, Token (INTEGER _)) -> Just (Reduce 1 170)
    (76, Token (QVARSYM _)) -> Just (Reduce 1 170)
    (76, Token (QCONSYM _)) -> Just (Reduce 1 170)
    (76, Token (BACKQUOTE _)) -> Just (Reduce 1 170)
    (77, Token (RPAREN _)) -> Just (Shift 73)
    (78, Token (LPAREN _)) -> Just (Shift 118)
    (78, Token (LBRACKET _)) -> Just (Shift 122)
    (78, Token (EXCL _)) -> Just (Shift 78)
    (78, Token (QCONID _)) -> Just (Shift 125)
    (78, Token (EXPORT _)) -> Just (Shift 296)
    (78, Token (AS _)) -> Just (Shift 297)
    (78, Token (QVARID _)) -> Just (Shift 298)
    (79, Token (LPAREN _)) -> Just (Shift 118)
    (79, Token (LBRACKET _)) -> Just (Shift 122)
    (79, Token (EXCL _)) -> Just (Shift 78)
    (79, Token (QCONID _)) -> Just (Shift 125)
    (79, Token (EXPORT _)) -> Just (Shift 296)
    (79, Token (AS _)) -> Just (Shift 297)
    (79, Token (QVARID _)) -> Just (Shift 298)
    (80, Token (WHERE _)) -> Just (Shift 202)
    (80, Token (RBRACE _)) -> Just (Reduce 0 65)
    (80, Token (LPAREN _)) -> Just (Shift 118)
    (80, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (80, Token (DARROW _)) -> Just (Shift 83)
    (80, Token (LBRACKET _)) -> Just (Shift 122)
    (80, Token (EXCL _)) -> Just (Shift 78)
    (80, Token (QCONID _)) -> Just (Shift 125)
    (80, Token (EXPORT _)) -> Just (Shift 296)
    (80, Token (AS _)) -> Just (Shift 297)
    (80, Token (QVARID _)) -> Just (Shift 298)
    (81, Token (LPAREN _)) -> Just (Shift 118)
    (81, Token (LBRACKET _)) -> Just (Shift 122)
    (81, Token (EXCL _)) -> Just (Shift 78)
    (81, Token (QCONID _)) -> Just (Shift 125)
    (81, Token (EXPORT _)) -> Just (Shift 296)
    (81, Token (AS _)) -> Just (Shift 297)
    (81, Token (QVARID _)) -> Just (Shift 298)
    (82, Token (WHERE _)) -> Just (Shift 204)
    (82, Token (RBRACE _)) -> Just (Reduce 0 75)
    (82, Token (LPAREN _)) -> Just (Shift 118)
    (82, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (82, Token (DARROW _)) -> Just (Shift 85)
    (82, Token (LBRACKET _)) -> Just (Shift 122)
    (82, Token (EXCL _)) -> Just (Shift 78)
    (82, Token (QCONID _)) -> Just (Shift 125)
    (82, Token (EXPORT _)) -> Just (Shift 296)
    (82, Token (AS _)) -> Just (Shift 297)
    (82, Token (QVARID _)) -> Just (Shift 298)
    (83, Token (LPAREN _)) -> Just (Shift 118)
    (83, Token (LBRACKET _)) -> Just (Shift 122)
    (83, Token (EXCL _)) -> Just (Shift 78)
    (83, Token (QCONID _)) -> Just (Shift 125)
    (83, Token (EXPORT _)) -> Just (Shift 296)
    (83, Token (AS _)) -> Just (Shift 297)
    (83, Token (QVARID _)) -> Just (Shift 298)
    (84, Token (WHERE _)) -> Just (Shift 202)
    (84, Token (RBRACE _)) -> Just (Reduce 0 65)
    (84, Token (LPAREN _)) -> Just (Shift 118)
    (84, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (84, Token (LBRACKET _)) -> Just (Shift 122)
    (84, Token (EXCL _)) -> Just (Shift 78)
    (84, Token (QCONID _)) -> Just (Shift 125)
    (84, Token (EXPORT _)) -> Just (Shift 296)
    (84, Token (AS _)) -> Just (Shift 297)
    (84, Token (QVARID _)) -> Just (Shift 298)
    (85, Token (LPAREN _)) -> Just (Shift 118)
    (85, Token (LBRACKET _)) -> Just (Shift 122)
    (85, Token (EXCL _)) -> Just (Shift 78)
    (85, Token (QCONID _)) -> Just (Shift 125)
    (85, Token (EXPORT _)) -> Just (Shift 296)
    (85, Token (AS _)) -> Just (Shift 297)
    (85, Token (QVARID _)) -> Just (Shift 298)
    (86, Token (WHERE _)) -> Just (Shift 204)
    (86, Token (RBRACE _)) -> Just (Reduce 0 75)
    (86, Token (LPAREN _)) -> Just (Shift 118)
    (86, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (86, Token (LBRACKET _)) -> Just (Shift 122)
    (86, Token (EXCL _)) -> Just (Shift 78)
    (86, Token (QCONID _)) -> Just (Shift 125)
    (86, Token (EXPORT _)) -> Just (Shift 296)
    (86, Token (AS _)) -> Just (Shift 297)
    (86, Token (QVARID _)) -> Just (Shift 298)
    (87, Token (LPAREN _)) -> Just (Shift 118)
    (87, Token (LBRACKET _)) -> Just (Shift 122)
    (87, Token (EXCL _)) -> Just (Shift 78)
    (87, Token (QCONID _)) -> Just (Shift 125)
    (87, Token (EXPORT _)) -> Just (Shift 296)
    (87, Token (AS _)) -> Just (Shift 297)
    (87, Token (QVARID _)) -> Just (Shift 298)
    (88, Token (WHERE _)) -> Just (Reduce 1 98)
    (88, Token (RBRACE _)) -> Just (Reduce 1 98)
    (88, Token (LPAREN _)) -> Just (Shift 118)
    (88, Token (RPAREN _)) -> Just (Reduce 1 98)
    (88, Token (COMMA _)) -> Just (Reduce 1 98)
    (88, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (88, Token (EQUAL _)) -> Just (Reduce 1 98)
    (88, Token (DARROW _)) -> Just (Shift 90)
    (88, Token (PIPE _)) -> Just (Reduce 1 98)
    (88, Token (RARROW _)) -> Just (Shift 89)
    (88, Token (LBRACKET _)) -> Just (Shift 122)
    (88, Token (EXCL _)) -> Just (Shift 78)
    (88, Token (QCONID _)) -> Just (Shift 125)
    (88, Token (EXPORT _)) -> Just (Shift 296)
    (88, Token (AS _)) -> Just (Shift 297)
    (88, Token (QVARID _)) -> Just (Shift 298)
    (89, Token (LPAREN _)) -> Just (Shift 118)
    (89, Token (LBRACKET _)) -> Just (Shift 122)
    (89, Token (EXCL _)) -> Just (Shift 78)
    (89, Token (QCONID _)) -> Just (Shift 125)
    (89, Token (EXPORT _)) -> Just (Shift 296)
    (89, Token (AS _)) -> Just (Shift 297)
    (89, Token (QVARID _)) -> Just (Shift 298)
    (90, Token (LPAREN _)) -> Just (Shift 118)
    (90, Token (LBRACKET _)) -> Just (Shift 122)
    (90, Token (EXCL _)) -> Just (Shift 78)
    (90, Token (QCONID _)) -> Just (Shift 125)
    (90, Token (EXPORT _)) -> Just (Shift 296)
    (90, Token (AS _)) -> Just (Shift 297)
    (90, Token (QVARID _)) -> Just (Shift 298)
    (91, Token (WHERE _)) -> Just (Reduce 1 98)
    (91, Token (RBRACE _)) -> Just (Reduce 1 98)
    (91, Token (LPAREN _)) -> Just (Shift 118)
    (91, Token (RPAREN _)) -> Just (Reduce 1 98)
    (91, Token (COMMA _)) -> Just (Reduce 1 98)
    (91, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (91, Token (EQUAL _)) -> Just (Reduce 1 98)
    (91, Token (PIPE _)) -> Just (Reduce 1 98)
    (91, Token (RARROW _)) -> Just (Shift 89)
    (91, Token (LBRACKET _)) -> Just (Shift 122)
    (91, Token (RBRACKET _)) -> Just (Reduce 1 98)
    (91, Token (EXCL _)) -> Just (Shift 78)
    (91, Token (QCONID _)) -> Just (Shift 125)
    (91, Token (EXPORT _)) -> Just (Shift 296)
    (91, Token (AS _)) -> Just (Shift 297)
    (91, Token (QVARID _)) -> Just (Shift 298)
    (92, Token (LPAREN _)) -> Just (Shift 118)
    (92, Token (LBRACKET _)) -> Just (Shift 122)
    (92, Token (EXCL _)) -> Just (Shift 78)
    (92, Token (QCONID _)) -> Just (Shift 125)
    (92, Token (EXPORT _)) -> Just (Shift 296)
    (92, Token (AS _)) -> Just (Shift 297)
    (92, Token (QVARID _)) -> Just (Shift 298)
    (93, Token (RBRACE _)) -> Just (Reduce 0 117)
    (93, Token (LPAREN _)) -> Just (Shift 118)
    (93, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (93, Token (EQUAL _)) -> Just (Shift 96)
    (93, Token (DERIVING _)) -> Just (Reduce 0 117)
    (93, Token (DARROW _)) -> Just (Shift 94)
    (93, Token (LBRACKET _)) -> Just (Shift 122)
    (93, Token (EXCL _)) -> Just (Shift 78)
    (93, Token (QCONID _)) -> Just (Shift 125)
    (93, Token (EXPORT _)) -> Just (Shift 296)
    (93, Token (AS _)) -> Just (Shift 297)
    (93, Token (QVARID _)) -> Just (Shift 298)
    (94, Token (LPAREN _)) -> Just (Shift 118)
    (94, Token (LBRACKET _)) -> Just (Shift 122)
    (94, Token (EXCL _)) -> Just (Shift 78)
    (94, Token (QCONID _)) -> Just (Shift 125)
    (94, Token (EXPORT _)) -> Just (Shift 296)
    (94, Token (AS _)) -> Just (Shift 297)
    (94, Token (QVARID _)) -> Just (Shift 298)
    (95, Token (RBRACE _)) -> Just (Reduce 0 117)
    (95, Token (LPAREN _)) -> Just (Shift 118)
    (95, Token (SEMICOLON _)) -> Just (Reduce 0 117)
    (95, Token (EQUAL _)) -> Just (Shift 96)
    (95, Token (DERIVING _)) -> Just (Reduce 0 117)
    (95, Token (LBRACKET _)) -> Just (Shift 122)
    (95, Token (EXCL _)) -> Just (Shift 78)
    (95, Token (QCONID _)) -> Just (Shift 125)
    (95, Token (EXPORT _)) -> Just (Shift 296)
    (95, Token (AS _)) -> Just (Shift 297)
    (95, Token (QVARID _)) -> Just (Shift 298)
    (96, Token (LPAREN _)) -> Just (Shift 118)
    (96, Token (LBRACKET _)) -> Just (Shift 122)
    (96, Token (EXCL _)) -> Just (Shift 78)
    (96, Token (QCONID _)) -> Just (Shift 125)
    (96, Token (EXPORT _)) -> Just (Shift 296)
    (96, Token (AS _)) -> Just (Shift 297)
    (96, Token (QVARID _)) -> Just (Shift 298)
    (97, Token (LPAREN _)) -> Just (Shift 118)
    (97, Token (LBRACKET _)) -> Just (Shift 122)
    (97, Token (EXCL _)) -> Just (Shift 78)
    (97, Token (QCONID _)) -> Just (Shift 125)
    (97, Token (EXPORT _)) -> Just (Shift 296)
    (97, Token (AS _)) -> Just (Shift 297)
    (97, Token (QVARID _)) -> Just (Shift 298)
    (98, Token (LPAREN _)) -> Just (Shift 123)
    (98, Token (QCONID _)) -> Just (Shift 125)
    (99, Token (RBRACE _)) -> Just (Reduce 1 121)
    (99, Token (LPAREN _)) -> Just (Shift 118)
    (99, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (99, Token (DERIVING _)) -> Just (Reduce 1 121)
    (99, Token (PIPE _)) -> Just (Reduce 1 121)
    (99, Token (LBRACKET _)) -> Just (Shift 122)
    (99, Token (EXCL _)) -> Just (Shift 78)
    (99, Token (QCONID _)) -> Just (Shift 125)
    (99, Token (EXPORT _)) -> Just (Shift 296)
    (99, Token (AS _)) -> Just (Shift 297)
    (99, Token (QVARID _)) -> Just (Shift 298)
    (99, Token (QCONSYM _)) -> Just (Shift 309)
    (99, Token (BACKQUOTE _)) -> Just (Shift 311)
    (100, Token (LPAREN _)) -> Just (Shift 118)
    (100, Token (LBRACKET _)) -> Just (Shift 122)
    (100, Token (EXCL _)) -> Just (Shift 78)
    (100, Token (QCONID _)) -> Just (Shift 125)
    (100, Token (EXPORT _)) -> Just (Shift 296)
    (100, Token (AS _)) -> Just (Shift 297)
    (100, Token (QVARID _)) -> Just (Shift 298)
    (101, Token (RBRACE _)) -> Just (Reduce 3 122)
    (101, Token (LPAREN _)) -> Just (Shift 118)
    (101, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (101, Token (DERIVING _)) -> Just (Reduce 3 122)
    (101, Token (PIPE _)) -> Just (Reduce 3 122)
    (101, Token (LBRACKET _)) -> Just (Shift 122)
    (101, Token (EXCL _)) -> Just (Shift 78)
    (101, Token (QCONID _)) -> Just (Shift 125)
    (101, Token (EXPORT _)) -> Just (Shift 296)
    (101, Token (AS _)) -> Just (Shift 297)
    (101, Token (QVARID _)) -> Just (Shift 298)
    (102, Token (LPAREN _)) -> Just (Shift 118)
    (102, Token (LBRACKET _)) -> Just (Shift 122)
    (102, Token (EXCL _)) -> Just (Shift 78)
    (102, Token (QCONID _)) -> Just (Shift 125)
    (102, Token (EXPORT _)) -> Just (Shift 296)
    (102, Token (AS _)) -> Just (Shift 297)
    (102, Token (QVARID _)) -> Just (Shift 298)
    (103, Token (RBRACE _)) -> Just (Reduce 1 98)
    (103, Token (LPAREN _)) -> Just (Shift 118)
    (103, Token (SEMICOLON _)) -> Just (Reduce 1 98)
    (103, Token (DARROW _)) -> Just (Shift 108)
    (103, Token (RARROW _)) -> Just (Shift 89)
    (103, Token (LBRACKET _)) -> Just (Shift 122)
    (103, Token (EXCL _)) -> Just (Shift 78)
    (103, Token (QCONID _)) -> Just (Shift 125)
    (103, Token (EXPORT _)) -> Just (Shift 296)
    (103, Token (AS _)) -> Just (Shift 297)
    (103, Token (QVARID _)) -> Just (Shift 298)
    (104, Token (LPAREN _)) -> Just (Shift 118)
    (104, Token (LBRACKET _)) -> Just (Shift 122)
    (104, Token (EXCL _)) -> Just (Shift 78)
    (104, Token (QCONID _)) -> Just (Shift 125)
    (104, Token (EXPORT _)) -> Just (Shift 296)
    (104, Token (AS _)) -> Just (Shift 297)
    (104, Token (QVARID _)) -> Just (Shift 298)
    (105, Token (LPAREN _)) -> Just (Shift 118)
    (105, Token (LBRACKET _)) -> Just (Shift 122)
    (105, Token (EXCL _)) -> Just (Shift 78)
    (105, Token (QCONID _)) -> Just (Shift 125)
    (105, Token (EXPORT _)) -> Just (Shift 296)
    (105, Token (AS _)) -> Just (Shift 297)
    (105, Token (QVARID _)) -> Just (Shift 298)
    (106, Token (LPAREN _)) -> Just (Shift 118)
    (106, Token (LBRACKET _)) -> Just (Shift 122)
    (106, Token (EXCL _)) -> Just (Shift 78)
    (106, Token (QCONID _)) -> Just (Shift 125)
    (106, Token (EXPORT _)) -> Just (Shift 296)
    (106, Token (AS _)) -> Just (Shift 297)
    (106, Token (QVARID _)) -> Just (Shift 298)
    (107, Token (LPAREN _)) -> Just (Shift 118)
    (107, Token (LBRACKET _)) -> Just (Shift 122)
    (107, Token (EXCL _)) -> Just (Shift 78)
    (107, Token (QCONID _)) -> Just (Shift 125)
    (107, Token (EXPORT _)) -> Just (Shift 296)
    (107, Token (AS _)) -> Just (Shift 297)
    (107, Token (QVARID _)) -> Just (Shift 298)
    (108, Token (LPAREN _)) -> Just (Shift 118)
    (108, Token (LBRACKET _)) -> Just (Shift 122)
    (108, Token (EXCL _)) -> Just (Shift 78)
    (108, Token (QCONID _)) -> Just (Shift 125)
    (108, Token (EXPORT _)) -> Just (Shift 296)
    (108, Token (AS _)) -> Just (Shift 297)
    (108, Token (QVARID _)) -> Just (Shift 298)
    (109, Token (LPAREN _)) -> Just (Shift 118)
    (109, Token (LBRACKET _)) -> Just (Shift 122)
    (109, Token (EXCL _)) -> Just (Shift 78)
    (109, Token (QCONID _)) -> Just (Shift 125)
    (109, Token (EXPORT _)) -> Just (Shift 296)
    (109, Token (AS _)) -> Just (Shift 297)
    (109, Token (QVARID _)) -> Just (Shift 298)
    (110, Token (LPAREN _)) -> Just (Shift 118)
    (110, Token (LBRACKET _)) -> Just (Shift 122)
    (110, Token (EXCL _)) -> Just (Shift 78)
    (110, Token (QCONID _)) -> Just (Shift 125)
    (110, Token (EXPORT _)) -> Just (Shift 296)
    (110, Token (AS _)) -> Just (Shift 297)
    (110, Token (QVARID _)) -> Just (Shift 298)
    (111, Token (LBRACE _)) -> Just (Shift 67)
    (111, Token (LPAREN _)) -> Just (Shift 118)
    (111, Token (LBRACKET _)) -> Just (Shift 122)
    (111, Token (EXCL _)) -> Just (Shift 78)
    (111, Token (QCONID _)) -> Just (Shift 125)
    (111, Token (EXPORT _)) -> Just (Shift 296)
    (111, Token (AS _)) -> Just (Shift 297)
    (111, Token (QVARID _)) -> Just (Shift 298)
    (112, Token (LPAREN _)) -> Just (Shift 118)
    (112, Token (LBRACKET _)) -> Just (Shift 122)
    (112, Token (EXCL _)) -> Just (Shift 78)
    (112, Token (QCONID _)) -> Just (Shift 125)
    (112, Token (EXPORT _)) -> Just (Shift 296)
    (112, Token (AS _)) -> Just (Shift 297)
    (112, Token (QVARID _)) -> Just (Shift 298)
    (113, Token (LPAREN _)) -> Just (Shift 118)
    (113, Token (EQUAL _)) -> Just (Shift 98)
    (113, Token (DARROW _)) -> Just (Shift 115)
    (113, Token (LBRACKET _)) -> Just (Shift 122)
    (113, Token (EXCL _)) -> Just (Shift 78)
    (113, Token (QCONID _)) -> Just (Shift 125)
    (113, Token (EXPORT _)) -> Just (Shift 296)
    (113, Token (AS _)) -> Just (Shift 297)
    (113, Token (QVARID _)) -> Just (Shift 298)
    (114, Token (LPAREN _)) -> Just (Shift 118)
    (114, Token (LBRACKET _)) -> Just (Shift 122)
    (114, Token (EXCL _)) -> Just (Shift 78)
    (114, Token (QCONID _)) -> Just (Shift 125)
    (114, Token (EXPORT _)) -> Just (Shift 296)
    (114, Token (AS _)) -> Just (Shift 297)
    (114, Token (QVARID _)) -> Just (Shift 298)
    (115, Token (LPAREN _)) -> Just (Shift 118)
    (115, Token (LBRACKET _)) -> Just (Shift 122)
    (115, Token (EXCL _)) -> Just (Shift 78)
    (115, Token (QCONID _)) -> Just (Shift 125)
    (115, Token (EXPORT _)) -> Just (Shift 296)
    (115, Token (AS _)) -> Just (Shift 297)
    (115, Token (QVARID _)) -> Just (Shift 298)
    (116, Token (LPAREN _)) -> Just (Shift 118)
    (116, Token (EQUAL _)) -> Just (Shift 104)
    (116, Token (LBRACKET _)) -> Just (Shift 122)
    (116, Token (EXCL _)) -> Just (Shift 78)
    (116, Token (QCONID _)) -> Just (Shift 125)
    (116, Token (EXPORT _)) -> Just (Shift 296)
    (116, Token (AS _)) -> Just (Shift 297)
    (116, Token (QVARID _)) -> Just (Shift 298)
    (117, Token (LPAREN _)) -> Just (Shift 118)
    (117, Token (EQUAL _)) -> Just (Shift 98)
    (117, Token (LBRACKET _)) -> Just (Shift 122)
    (117, Token (EXCL _)) -> Just (Shift 78)
    (117, Token (QCONID _)) -> Just (Shift 125)
    (117, Token (EXPORT _)) -> Just (Shift 296)
    (117, Token (AS _)) -> Just (Shift 297)
    (117, Token (QVARID _)) -> Just (Shift 298)
    (118, Token (LPAREN _)) -> Just (Shift 118)
    (118, Token (RPAREN _)) -> Just (Shift 288)
    (118, Token (COMMA _)) -> Just (Shift 301)
    (118, Token (RARROW _)) -> Just (Shift 291)
    (118, Token (LBRACKET _)) -> Just (Shift 122)
    (118, Token (EXCL _)) -> Just (Shift 78)
    (118, Token (QCONID _)) -> Just (Shift 125)
    (118, Token (EXPORT _)) -> Just (Shift 296)
    (118, Token (AS _)) -> Just (Shift 297)
    (118, Token (QVARID _)) -> Just (Shift 298)
    (118, Token (QCONSYM _)) -> Just (Shift 126)
    (119, Token (LPAREN _)) -> Just (Shift 118)
    (119, Token (RPAREN _)) -> Just (Shift 148)
    (119, Token (LBRACKET _)) -> Just (Shift 122)
    (119, Token (EXCL _)) -> Just (Shift 78)
    (119, Token (QCONID _)) -> Just (Shift 125)
    (119, Token (EXPORT _)) -> Just (Shift 296)
    (119, Token (AS _)) -> Just (Shift 297)
    (119, Token (QVARID _)) -> Just (Shift 298)
    (120, Token (LPAREN _)) -> Just (Shift 118)
    (120, Token (LBRACKET _)) -> Just (Shift 122)
    (120, Token (EXCL _)) -> Just (Shift 78)
    (120, Token (QCONID _)) -> Just (Shift 125)
    (120, Token (EXPORT _)) -> Just (Shift 296)
    (120, Token (AS _)) -> Just (Shift 297)
    (120, Token (QVARID _)) -> Just (Shift 298)
    (121, Token (LPAREN _)) -> Just (Shift 118)
    (121, Token (LBRACKET _)) -> Just (Shift 122)
    (121, Token (EXCL _)) -> Just (Shift 78)
    (121, Token (QCONID _)) -> Just (Shift 125)
    (121, Token (EXPORT _)) -> Just (Shift 296)
    (121, Token (AS _)) -> Just (Shift 297)
    (121, Token (QVARID _)) -> Just (Shift 298)
    (122, Token (LPAREN _)) -> Just (Shift 118)
    (122, Token (LBRACKET _)) -> Just (Shift 122)
    (122, Token (RBRACKET _)) -> Just (Shift 292)
    (122, Token (EXCL _)) -> Just (Shift 78)
    (122, Token (QCONID _)) -> Just (Shift 125)
    (122, Token (EXPORT _)) -> Just (Shift 296)
    (122, Token (AS _)) -> Just (Shift 297)
    (122, Token (QVARID _)) -> Just (Shift 298)
    (123, Token (QCONSYM _)) -> Just (Shift 126)
    (124, Token (WHERE _)) -> Just (Reduce 3 173)
    (124, Token (LBRACE _)) -> Just (Reduce 3 173)
    (124, Token (RBRACE _)) -> Just (Reduce 3 173)
    (124, Token (LPAREN _)) -> Just (Reduce 3 173)
    (124, Token (RPAREN _)) -> Just (Reduce 3 173)
    (124, Token (COMMA _)) -> Just (Reduce 3 173)
    (124, Token (SEMICOLON _)) -> Just (Reduce 3 173)
    (124, Token (EQUAL _)) -> Just (Reduce 3 173)
    (124, Token (DERIVING _)) -> Just (Reduce 3 173)
    (124, Token (DARROW _)) -> Just (Reduce 3 173)
    (124, Token (PIPE _)) -> Just (Reduce 3 173)
    (124, Token (COLON_COLON _)) -> Just (Reduce 3 173)
    (124, Token (INFIXL _)) -> Just (Reduce 3 173)
    (124, Token (INFIXR _)) -> Just (Reduce 3 173)
    (124, Token (INFIX _)) -> Just (Reduce 3 173)
    (124, Token (RARROW _)) -> Just (Reduce 3 173)
    (124, Token (LBRACKET _)) -> Just (Reduce 3 173)
    (124, Token (RBRACKET _)) -> Just (Reduce 3 173)
    (124, Token (EXCL _)) -> Just (Reduce 3 173)
    (124, Token (QCONID _)) -> Just (Reduce 3 173)
    (124, Token (EXPORT _)) -> Just (Reduce 3 173)
    (124, Token (AS _)) -> Just (Reduce 3 173)
    (124, Token (QVARID _)) -> Just (Reduce 3 173)
    (124, Token (INTEGER _)) -> Just (Reduce 3 173)
    (124, Token (QVARSYM _)) -> Just (Reduce 3 173)
    (124, Token (QCONSYM _)) -> Just (Reduce 3 173)
    (124, Token (BACKQUOTE _)) -> Just (Reduce 3 173)
    (125, Token (WHERE _)) -> Just (Reduce 1 172)
    (125, Token (LBRACE _)) -> Just (Reduce 1 172)
    (125, Token (RBRACE _)) -> Just (Reduce 1 172)
    (125, Token (LPAREN _)) -> Just (Reduce 1 172)
    (125, Token (RPAREN _)) -> Just (Reduce 1 172)
    (125, Token (COMMA _)) -> Just (Reduce 1 172)
    (125, Token (SEMICOLON _)) -> Just (Reduce 1 172)
    (125, Token (EQUAL _)) -> Just (Reduce 1 172)
    (125, Token (DERIVING _)) -> Just (Reduce 1 172)
    (125, Token (DARROW _)) -> Just (Reduce 1 172)
    (125, Token (PIPE _)) -> Just (Reduce 1 172)
    (125, Token (COLON_COLON _)) -> Just (Reduce 1 172)
    (125, Token (INFIXL _)) -> Just (Reduce 1 172)
    (125, Token (INFIXR _)) -> Just (Reduce 1 172)
    (125, Token (INFIX _)) -> Just (Reduce 1 172)
    (125, Token (RARROW _)) -> Just (Reduce 1 172)
    (125, Token (LBRACKET _)) -> Just (Reduce 1 172)
    (125, Token (RBRACKET _)) -> Just (Reduce 1 172)
    (125, Token (EXCL _)) -> Just (Reduce 1 172)
    (125, Token (QCONID _)) -> Just (Reduce 1 172)
    (125, Token (EXPORT _)) -> Just (Reduce 1 172)
    (125, Token (AS _)) -> Just (Reduce 1 172)
    (125, Token (QVARID _)) -> Just (Reduce 1 172)
    (125, Token (INTEGER _)) -> Just (Reduce 1 172)
    (125, Token (QVARSYM _)) -> Just (Reduce 1 172)
    (125, Token (QCONSYM _)) -> Just (Reduce 1 172)
    (125, Token (BACKQUOTE _)) -> Just (Reduce 1 172)
    (126, Token (RPAREN _)) -> Just (Shift 124)
    (127, Token (RPAREN _)) -> Just (Reduce 3 24)
    (128, Token (RPAREN _)) -> Just (Reduce 1 23)
    (128, Token (COMMA _)) -> Just (Shift 62)
    (129, Token (RPAREN _)) -> Just (Reduce 3 17)
    (130, Token (RPAREN _)) -> Just (Reduce 1 16)
    (130, Token (COMMA _)) -> Just (Shift 59)
    (131, Token (RPAREN _)) -> Just (Reduce 3 20)
    (131, Token (COMMA _)) -> Just (Reduce 3 20)
    (132, Token (RPAREN _)) -> Just (Reduce 4 21)
    (132, Token (COMMA _)) -> Just (Reduce 4 21)
    (133, Token (RPAREN _)) -> Just (Reduce 4 22)
    (133, Token (COMMA _)) -> Just (Reduce 4 22)
    (134, Token (RPAREN _)) -> Just (Shift 132)
    (135, Token (RPAREN _)) -> Just (Reduce 1 18)
    (135, Token (COMMA _)) -> Just (Reduce 1 18)
    (136, Token (LPAREN _)) -> Just (Shift 63)
    (136, Token (RPAREN _)) -> Just (Reduce 1 19)
    (136, Token (COMMA _)) -> Just (Reduce 1 19)
    (137, Token (RPAREN _)) -> Just (Shift 133)
    (138, Token (RPAREN _)) -> Just (Reduce 1 25)
    (138, Token (COMMA _)) -> Just (Reduce 1 25)
    (139, Token (RPAREN _)) -> Just (Reduce 1 26)
    (139, Token (COMMA _)) -> Just (Reduce 1 26)
    (140, Token (RPAREN _)) -> Just (Shift 144)
    (140, Token (QCONID _)) -> Just (Shift 195)
    (141, Token (RPAREN _)) -> Just (Shift 145)
    (141, Token (QCONID _)) -> Just (Shift 195)
    (142, Token (RPAREN _)) -> Just (Shift 146)
    (142, Token (QCONID _)) -> Just (Shift 195)
    (143, Token (RPAREN _)) -> Just (Shift 147)
    (143, Token (QCONID _)) -> Just (Shift 195)
    (144, Token (RBRACE _)) -> Just (Reduce 6 35)
    (144, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (145, Token (RBRACE _)) -> Just (Reduce 8 39)
    (145, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (146, Token (RBRACE _)) -> Just (Reduce 8 47)
    (146, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (147, Token (RBRACE _)) -> Just (Reduce 6 43)
    (147, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (148, Token (RBRACE _)) -> Just (Reduce 3 53)
    (148, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (149, Token (RBRACE _)) -> Just (Reduce 8 31)
    (149, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (150, Token (RBRACE _)) -> Just (Reduce 7 30)
    (150, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (151, Token (RBRACE _)) -> Just (Reduce 7 36)
    (151, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (152, Token (RBRACE _)) -> Just (Reduce 9 40)
    (152, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (153, Token (RBRACE _)) -> Just (Reduce 9 48)
    (153, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (154, Token (RBRACE _)) -> Just (Reduce 7 44)
    (154, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (155, Token (RBRACE _)) -> Just (Reduce 4 54)
    (155, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (156, Token (QCONID _)) -> Just (Reduce 0 184)
    (156, Token (QUALIFIED _)) -> Just (Shift 188)
    (157, Token (LPAREN _)) -> Just (Shift 60)
    (158, Token (LPAREN _)) -> Just (Shift 140)
    (158, Token (QCONID _)) -> Just (Shift 195)
    (159, Token (LPAREN _)) -> Just (Shift 141)
    (159, Token (QCONID _)) -> Just (Shift 195)
    (160, Token (LPAREN _)) -> Just (Shift 142)
    (160, Token (QCONID _)) -> Just (Shift 195)
    (161, Token (LPAREN _)) -> Just (Shift 143)
    (161, Token (QCONID _)) -> Just (Shift 195)
    (162, Token (LPAREN _)) -> Just (Shift 119)
    (163, Token (IMPORT _)) -> Just (Shift 208)
    (163, Token (EXPORT _)) -> Just (Shift 209)
    (164, Token (RBRACE _)) -> Just (Reduce 0 182)
    (164, Token (LPAREN _)) -> Just (Reduce 0 182)
    (164, Token (SEMICOLON _)) -> Just (Reduce 0 182)
    (164, Token (HIDING _)) -> Just (Reduce 0 182)
    (164, Token (AS _)) -> Just (Shift 9)
    (165, Token (RPAREN _)) -> Just (Shift 149)
    (166, Token (RPAREN _)) -> Just (Shift 150)
    (167, Token (RBRACE _)) -> Just (Reduce 4 29)
    (167, Token (LPAREN _)) -> Just (Shift 61)
    (167, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (167, Token (HIDING _)) -> Just (Shift 157)
    (168, Token (RBRACE _)) -> Just (Reduce 4 32)
    (168, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (169, Token (RBRACE _)) -> Just (Reduce 3 33)
    (169, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (169, Token (DERIVING _)) -> Just (Shift 158)
    (170, Token (RBRACE _)) -> Just (Reduce 5 37)
    (170, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (170, Token (DERIVING _)) -> Just (Shift 159)
    (171, Token (RBRACE _)) -> Just (Reduce 5 34)
    (171, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (172, Token (RBRACE _)) -> Just (Reduce 7 38)
    (172, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (173, Token (RBRACE _)) -> Just (Reduce 7 46)
    (173, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (174, Token (RBRACE _)) -> Just (Reduce 5 42)
    (174, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (175, Token (RPAREN _)) -> Just (Shift 151)
    (176, Token (RPAREN _)) -> Just (Shift 152)
    (177, Token (RPAREN _)) -> Just (Shift 153)
    (178, Token (RPAREN _)) -> Just (Shift 154)
    (179, Token (RBRACE _)) -> Just (Reduce 5 45)
    (179, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (179, Token (DERIVING _)) -> Just (Shift 160)
    (180, Token (RBRACE _)) -> Just (Reduce 3 41)
    (180, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (180, Token (DERIVING _)) -> Just (Shift 161)
    (181, Token (RBRACE _)) -> Just (Reduce 5 50)
    (181, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (182, Token (RBRACE _)) -> Just (Reduce 3 49)
    (182, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (183, Token (RBRACE _)) -> Just (Reduce 5 52)
    (183, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (184, Token (RBRACE _)) -> Just (Reduce 3 51)
    (184, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (185, Token (RPAREN _)) -> Just (Shift 155)
    (186, Token (RBRACE _)) -> Just (Reduce 2 55)
    (186, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (187, Token (RBRACE _)) -> Just (Reduce 1 56)
    (187, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (188, Token (QCONID _)) -> Just (Reduce 1 185)
    (189, Token (RBRACE _)) -> Just (Reduce 2 183)
    (189, Token (LPAREN _)) -> Just (Reduce 2 183)
    (189, Token (SEMICOLON _)) -> Just (Reduce 2 183)
    (189, Token (HIDING _)) -> Just (Reduce 2 183)
    (190, Token (WHERE _)) -> Just (Reduce 1 100)
    (190, Token (LBRACE _)) -> Just (Reduce 1 100)
    (190, Token (RBRACE _)) -> Just (Reduce 1 100)
    (190, Token (LPAREN _)) -> Just (Reduce 1 100)
    (190, Token (RPAREN _)) -> Just (Reduce 1 100)
    (190, Token (COMMA _)) -> Just (Reduce 1 100)
    (190, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (190, Token (EQUAL _)) -> Just (Reduce 1 100)
    (190, Token (DERIVING _)) -> Just (Reduce 1 100)
    (190, Token (DARROW _)) -> Just (Reduce 1 100)
    (190, Token (PIPE _)) -> Just (Reduce 1 100)
    (190, Token (COLON_COLON _)) -> Just (Reduce 1 100)
    (190, Token (INFIXL _)) -> Just (Reduce 1 100)
    (190, Token (INFIXR _)) -> Just (Reduce 1 100)
    (190, Token (INFIX _)) -> Just (Reduce 1 100)
    (190, Token (RARROW _)) -> Just (Reduce 1 100)
    (190, Token (LBRACKET _)) -> Just (Reduce 1 100)
    (190, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (190, Token (EXCL _)) -> Just (Reduce 1 100)
    (190, Token (QCONID _)) -> Just (Reduce 1 100)
    (190, Token (EXPORT _)) -> Just (Reduce 1 100)
    (190, Token (AS _)) -> Just (Reduce 1 100)
    (190, Token (QVARID _)) -> Just (Reduce 1 100)
    (190, Token (INTEGER _)) -> Just (Reduce 1 100)
    (190, Token (QVARSYM _)) -> Just (Reduce 1 100)
    (190, Token (QCONSYM _)) -> Just (Reduce 1 100)
    (190, Token (BACKQUOTE _)) -> Just (Reduce 1 100)
    (191, Token (WHERE _)) -> Just (Reduce 2 101)
    (191, Token (LBRACE _)) -> Just (Reduce 2 101)
    (191, Token (RBRACE _)) -> Just (Reduce 2 101)
    (191, Token (LPAREN _)) -> Just (Reduce 2 101)
    (191, Token (RPAREN _)) -> Just (Reduce 2 101)
    (191, Token (COMMA _)) -> Just (Reduce 2 101)
    (191, Token (SEMICOLON _)) -> Just (Reduce 2 101)
    (191, Token (EQUAL _)) -> Just (Reduce 2 101)
    (191, Token (DERIVING _)) -> Just (Reduce 2 101)
    (191, Token (DARROW _)) -> Just (Reduce 2 101)
    (191, Token (PIPE _)) -> Just (Reduce 2 101)
    (191, Token (COLON_COLON _)) -> Just (Reduce 2 101)
    (191, Token (INFIXL _)) -> Just (Reduce 2 101)
    (191, Token (INFIXR _)) -> Just (Reduce 2 101)
    (191, Token (INFIX _)) -> Just (Reduce 2 101)
    (191, Token (RARROW _)) -> Just (Reduce 2 101)
    (191, Token (LBRACKET _)) -> Just (Reduce 2 101)
    (191, Token (RBRACKET _)) -> Just (Reduce 2 101)
    (191, Token (EXCL _)) -> Just (Reduce 2 101)
    (191, Token (QCONID _)) -> Just (Reduce 2 101)
    (191, Token (EXPORT _)) -> Just (Reduce 2 101)
    (191, Token (AS _)) -> Just (Reduce 2 101)
    (191, Token (QVARID _)) -> Just (Reduce 2 101)
    (191, Token (INTEGER _)) -> Just (Reduce 2 101)
    (191, Token (QVARSYM _)) -> Just (Reduce 2 101)
    (191, Token (QCONSYM _)) -> Just (Reduce 2 101)
    (191, Token (BACKQUOTE _)) -> Just (Reduce 2 101)
    (192, Token (WHERE _)) -> Just (Reduce 3 99)
    (192, Token (RBRACE _)) -> Just (Reduce 3 99)
    (192, Token (RPAREN _)) -> Just (Reduce 3 99)
    (192, Token (COMMA _)) -> Just (Reduce 3 99)
    (192, Token (SEMICOLON _)) -> Just (Reduce 3 99)
    (192, Token (EQUAL _)) -> Just (Reduce 3 99)
    (192, Token (PIPE _)) -> Just (Reduce 3 99)
    (192, Token (RBRACKET _)) -> Just (Reduce 3 99)
    (193, Token (RBRACE _)) -> Just (Reduce 2 118)
    (193, Token (SEMICOLON _)) -> Just (Reduce 2 118)
    (193, Token (DERIVING _)) -> Just (Reduce 2 118)
    (194, Token (QCONID _)) -> Just (Shift 195)
    (195, Token (RBRACE _)) -> Just (Reduce 1 132)
    (195, Token (RPAREN _)) -> Just (Reduce 1 132)
    (195, Token (COMMA _)) -> Just (Reduce 1 132)
    (195, Token (SEMICOLON _)) -> Just (Reduce 1 132)
    (196, Token (RPAREN _)) -> Just (Reduce 1 130)
    (196, Token (COMMA _)) -> Just (Shift 194)
    (197, Token (RPAREN _)) -> Just (Reduce 3 131)
    (198, Token (RBRACE _)) -> Just (Reduce 7 126)
    (198, Token (SEMICOLON _)) -> Just (Reduce 7 126)
    (198, Token (DERIVING _)) -> Just (Reduce 7 126)
    (199, Token (COLON_COLON _)) -> Just (Shift 110)
    (200, Token (RBRACE _)) -> Just (Shift 198)
    (201, Token (RBRACE _)) -> Just (Reduce 3 125)
    (201, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (201, Token (DERIVING _)) -> Just (Reduce 3 125)
    (202, Token (LBRACE _)) -> Just (Shift 46)
    (203, Token (RBRACE _)) -> Just (Reduce 2 66)
    (203, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (204, Token (LBRACE _)) -> Just (Shift 55)
    (205, Token (RBRACE _)) -> Just (Reduce 2 76)
    (205, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (206, Token (RPAREN _)) -> Just (Reduce 1 96)
    (206, Token (COMMA _)) -> Just (Shift 120)
    (207, Token (RPAREN _)) -> Just (Reduce 3 97)
    (208, Token (EXPORT _)) -> Just (Shift 317)
    (208, Token (AS _)) -> Just (Shift 318)
    (208, Token (QVARID _)) -> Just (Shift 319)
    (209, Token (EXPORT _)) -> Just (Shift 317)
    (209, Token (AS _)) -> Just (Shift 318)
    (209, Token (QVARID _)) -> Just (Shift 319)
    (210, Token (COLON_COLON _)) -> Just (Shift 105)
    (211, Token (COLON_COLON _)) -> Just (Shift 106)
    (212, Token (COLON_COLON _)) -> Just (Shift 107)
    (213, Token (RBRACE _)) -> Just (Reduce 6 133)
    (213, Token (SEMICOLON _)) -> Just (Reduce 6 133)
    (214, Token (RBRACE _)) -> Just (Reduce 7 134)
    (214, Token (SEMICOLON _)) -> Just (Reduce 7 134)
    (215, Token (RBRACE _)) -> Just (Reduce 6 135)
    (215, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (216, Token (EXPORT _)) -> Just (Shift 321)
    (216, Token (AS _)) -> Just (Shift 322)
    (216, Token (QVARID _)) -> Just (Shift 323)
    (216, Token (STRING _)) -> Just (Shift 320)
    (217, Token (STRING _)) -> Just (Shift 324)
    (218, Token (STRING _)) -> Just (Shift 320)
    (219, Token (LBRACE _)) -> Just (Shift 44)
    (220, Token (LBRACE _)) -> Just (Shift 44)
    (221, Token (RBRACE _)) -> Just (Reduce 5 62)
    (221, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (222, Token (RBRACE _)) -> Just (Reduce 5 64)
    (222, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (223, Token (RBRACE _)) -> Just (Reduce 1 60)
    (223, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (224, Token (WHERE _)) -> Just (Shift 219)
    (224, Token (RBRACE _)) -> Just (Reduce 3 61)
    (224, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (225, Token (WHERE _)) -> Just (Shift 220)
    (225, Token (RBRACE _)) -> Just (Reduce 3 63)
    (225, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (226, Token (LBRACE _)) -> Just (Shift 44)
    (227, Token (LBRACE _)) -> Just (Shift 44)
    (228, Token (LBRACE _)) -> Just (Shift 44)
    (229, Token (LBRACE _)) -> Just (Shift 44)
    (230, Token (LBRACE _)) -> Just (Shift 44)
    (231, Token (RBRACE _)) -> Just (Reduce 3 57)
    (231, Token (COMMA _)) -> Just (Reduce 3 57)
    (231, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (231, Token (EQUAL _)) -> Just (Reduce 3 57)
    (232, Token (RBRACE _)) -> Just (Shift 231)
    (233, Token (RBRACE _)) -> Just (Reduce 1 58)
    (233, Token (SEMICOLON _)) -> Just (Shift 45)
    (234, Token (RBRACE _)) -> Just (Reduce 3 59)
    (235, Token (RBRACE _)) -> Just (Reduce 5 87)
    (235, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (236, Token (RBRACE _)) -> Just (Reduce 3 86)
    (236, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (237, Token (COLON_COLON _)) -> Just (Shift 102)
    (238, Token (COMMA _)) -> Just (Reduce 0 191)
    (238, Token (QCONID _)) -> Just (Reduce 0 191)
    (238, Token (EXPORT _)) -> Just (Reduce 0 191)
    (238, Token (AS _)) -> Just (Reduce 0 191)
    (238, Token (QVARID _)) -> Just (Reduce 0 191)
    (238, Token (INTEGER _)) -> Just (Shift 273)
    (238, Token (QVARSYM _)) -> Just (Reduce 0 191)
    (238, Token (QCONSYM _)) -> Just (Reduce 0 191)
    (238, Token (BACKQUOTE _)) -> Just (Reduce 0 191)
    (239, Token (QVARSYM _)) -> Just (Shift 344)
    (239, Token (QCONSYM _)) -> Just (Shift 309)
    (239, Token (BACKQUOTE _)) -> Just (Shift 310)
    (240, Token (RBRACE _)) -> Just (Reduce 3 88)
    (240, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (241, Token (LPAREN _)) -> Just (Reduce 1 163)
    (241, Token (RPAREN _)) -> Just (Reduce 1 163)
    (241, Token (EQUAL _)) -> Just (Reduce 1 163)
    (241, Token (PIPE _)) -> Just (Reduce 1 163)
    (241, Token (QCONID _)) -> Just (Reduce 1 163)
    (241, Token (EXPORT _)) -> Just (Reduce 1 163)
    (241, Token (AS _)) -> Just (Reduce 1 163)
    (241, Token (QVARID _)) -> Just (Reduce 1 163)
    (241, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (241, Token (QCONSYM _)) -> Just (Reduce 1 163)
    (241, Token (BACKQUOTE _)) -> Just (Reduce 1 163)
    (242, Token (LPAREN _)) -> Just (Reduce 2 164)
    (242, Token (RPAREN _)) -> Just (Reduce 2 164)
    (242, Token (EQUAL _)) -> Just (Reduce 2 164)
    (242, Token (PIPE _)) -> Just (Reduce 2 164)
    (242, Token (QCONID _)) -> Just (Reduce 2 164)
    (242, Token (EXPORT _)) -> Just (Reduce 2 164)
    (242, Token (AS _)) -> Just (Reduce 2 164)
    (242, Token (QVARID _)) -> Just (Reduce 2 164)
    (242, Token (QVARSYM _)) -> Just (Reduce 2 164)
    (242, Token (QCONSYM _)) -> Just (Reduce 2 164)
    (242, Token (BACKQUOTE _)) -> Just (Reduce 2 164)
    (243, Token (LPAREN _)) -> Just (Reduce 3 165)
    (243, Token (RPAREN _)) -> Just (Reduce 3 165)
    (243, Token (EQUAL _)) -> Just (Reduce 3 165)
    (243, Token (PIPE _)) -> Just (Reduce 3 165)
    (243, Token (QCONID _)) -> Just (Reduce 3 165)
    (243, Token (EXPORT _)) -> Just (Reduce 3 165)
    (243, Token (AS _)) -> Just (Reduce 3 165)
    (243, Token (QVARID _)) -> Just (Reduce 3 165)
    (243, Token (QVARSYM _)) -> Just (Reduce 3 165)
    (243, Token (QCONSYM _)) -> Just (Reduce 3 165)
    (243, Token (BACKQUOTE _)) -> Just (Reduce 3 165)
    (244, Token (WHERE _)) -> Just (Reduce 5 152)
    (244, Token (RBRACE _)) -> Just (Reduce 5 152)
    (244, Token (RPAREN _)) -> Just (Reduce 5 152)
    (244, Token (COMMA _)) -> Just (Reduce 5 152)
    (244, Token (SEMICOLON _)) -> Just (Reduce 5 152)
    (244, Token (EQUAL _)) -> Just (Reduce 5 152)
    (244, Token (PIPE _)) -> Just (Reduce 5 152)
    (245, Token (WHERE _)) -> Just (Reduce 3 151)
    (245, Token (RBRACE _)) -> Just (Reduce 3 151)
    (245, Token (RPAREN _)) -> Just (Reduce 3 151)
    (245, Token (COMMA _)) -> Just (Reduce 3 151)
    (245, Token (SEMICOLON _)) -> Just (Reduce 3 151)
    (245, Token (EQUAL _)) -> Just (Reduce 3 151)
    (245, Token (PIPE _)) -> Just (Reduce 3 151)
    (246, Token (WHERE _)) -> Just (Reduce 1 153)
    (246, Token (RBRACE _)) -> Just (Reduce 1 153)
    (246, Token (RPAREN _)) -> Just (Reduce 1 153)
    (246, Token (COMMA _)) -> Just (Reduce 1 153)
    (246, Token (SEMICOLON _)) -> Just (Reduce 1 153)
    (246, Token (EQUAL _)) -> Just (Reduce 1 153)
    (246, Token (PIPE _)) -> Just (Reduce 1 153)
    (246, Token (COLON_COLON _)) -> Just (Shift 87)
    (247, Token (WHERE _)) -> Just (Reduce 3 144)
    (247, Token (RBRACE _)) -> Just (Reduce 3 144)
    (247, Token (SEMICOLON _)) -> Just (Reduce 3 144)
    (247, Token (PIPE _)) -> Just (Shift 49)
    (248, Token (WHERE _)) -> Just (Reduce 5 145)
    (248, Token (RBRACE _)) -> Just (Reduce 5 145)
    (248, Token (SEMICOLON _)) -> Just (Reduce 5 145)
    (249, Token (EQUAL _)) -> Just (Shift 36)
    (250, Token (RBRACE _)) -> Just (Reduce 3 67)
    (250, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (251, Token (RBRACE _)) -> Just (Shift 250)
    (252, Token (RBRACE _)) -> Just (Reduce 3 69)
    (253, Token (RBRACE _)) -> Just (Reduce 1 68)
    (253, Token (SEMICOLON _)) -> Just (Shift 47)
    (254, Token (RBRACE _)) -> Just (Reduce 5 72)
    (254, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (255, Token (RBRACE _)) -> Just (Reduce 5 74)
    (255, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (256, Token (RBRACE _)) -> Just (Reduce 1 70)
    (256, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (257, Token (WHERE _)) -> Just (Shift 226)
    (257, Token (RBRACE _)) -> Just (Reduce 3 71)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (258, Token (WHERE _)) -> Just (Shift 227)
    (258, Token (RBRACE _)) -> Just (Reduce 3 73)
    (258, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (259, Token (RBRACE _)) -> Just (Reduce 3 77)
    (259, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (260, Token (RBRACE _)) -> Just (Shift 259)
    (261, Token (RBRACE _)) -> Just (Reduce 3 79)
    (262, Token (RBRACE _)) -> Just (Reduce 1 78)
    (262, Token (SEMICOLON _)) -> Just (Shift 56)
    (263, Token (RBRACE _)) -> Just (Reduce 5 82)
    (263, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (264, Token (RBRACE _)) -> Just (Reduce 5 84)
    (264, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (265, Token (WHERE _)) -> Just (Shift 228)
    (265, Token (RBRACE _)) -> Just (Reduce 3 81)
    (265, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (266, Token (WHERE _)) -> Just (Shift 229)
    (266, Token (RBRACE _)) -> Just (Reduce 3 83)
    (266, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (267, Token (COMMA _)) -> Just (Shift 64)
    (267, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (268, Token (LPAREN _)) -> Just (Reduce 1 166)
    (268, Token (COMMA _)) -> Just (Shift 64)
    (268, Token (EQUAL _)) -> Just (Reduce 1 166)
    (268, Token (PIPE _)) -> Just (Reduce 1 166)
    (268, Token (COLON_COLON _)) -> Just (Reduce 1 91)
    (268, Token (QCONID _)) -> Just (Reduce 1 166)
    (268, Token (EXPORT _)) -> Just (Reduce 1 166)
    (268, Token (AS _)) -> Just (Reduce 1 166)
    (268, Token (QVARID _)) -> Just (Reduce 1 166)
    (268, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (268, Token (QCONSYM _)) -> Just (Reduce 1 166)
    (268, Token (BACKQUOTE _)) -> Just (Reduce 1 166)
    (269, Token (COLON_COLON _)) -> Just (Reduce 3 92)
    (270, Token (COMMA _)) -> Just (Reduce 1 93)
    (270, Token (QCONID _)) -> Just (Reduce 1 93)
    (270, Token (EXPORT _)) -> Just (Reduce 1 93)
    (270, Token (AS _)) -> Just (Reduce 1 93)
    (270, Token (QVARID _)) -> Just (Reduce 1 93)
    (270, Token (INTEGER _)) -> Just (Reduce 1 93)
    (270, Token (QVARSYM _)) -> Just (Reduce 1 93)
    (270, Token (QCONSYM _)) -> Just (Reduce 1 93)
    (270, Token (BACKQUOTE _)) -> Just (Reduce 1 93)
    (271, Token (COMMA _)) -> Just (Reduce 1 94)
    (271, Token (QCONID _)) -> Just (Reduce 1 94)
    (271, Token (EXPORT _)) -> Just (Reduce 1 94)
    (271, Token (AS _)) -> Just (Reduce 1 94)
    (271, Token (QVARID _)) -> Just (Reduce 1 94)
    (271, Token (INTEGER _)) -> Just (Reduce 1 94)
    (271, Token (QVARSYM _)) -> Just (Reduce 1 94)
    (271, Token (QCONSYM _)) -> Just (Reduce 1 94)
    (271, Token (BACKQUOTE _)) -> Just (Reduce 1 94)
    (272, Token (COMMA _)) -> Just (Reduce 1 95)
    (272, Token (QCONID _)) -> Just (Reduce 1 95)
    (272, Token (EXPORT _)) -> Just (Reduce 1 95)
    (272, Token (AS _)) -> Just (Reduce 1 95)
    (272, Token (QVARID _)) -> Just (Reduce 1 95)
    (272, Token (INTEGER _)) -> Just (Reduce 1 95)
    (272, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (272, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (272, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (273, Token (COMMA _)) -> Just (Reduce 1 192)
    (273, Token (QCONID _)) -> Just (Reduce 1 192)
    (273, Token (EXPORT _)) -> Just (Reduce 1 192)
    (273, Token (AS _)) -> Just (Reduce 1 192)
    (273, Token (QVARID _)) -> Just (Reduce 1 192)
    (273, Token (QVARSYM _)) -> Just (Reduce 1 192)
    (273, Token (QCONSYM _)) -> Just (Reduce 1 192)
    (273, Token (BACKQUOTE _)) -> Just (Reduce 1 192)
    (274, Token (QVARSYM _)) -> Just (Shift 344)
    (274, Token (QCONSYM _)) -> Just (Shift 309)
    (274, Token (BACKQUOTE _)) -> Just (Shift 310)
    (275, Token (RBRACE _)) -> Just (Reduce 3 90)
    (275, Token (SEMICOLON _)) -> Just (Reduce 3 90)
    (276, Token (RBRACE _)) -> Just (Reduce 1 89)
    (276, Token (COMMA _)) -> Just (Shift 274)
    (276, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (277, Token (RBRACE _)) -> Just (Reduce 1 181)
    (277, Token (LPAREN _)) -> Just (Reduce 1 181)
    (277, Token (COMMA _)) -> Just (Reduce 1 181)
    (277, Token (SEMICOLON _)) -> Just (Reduce 1 181)
    (277, Token (COLON_COLON _)) -> Just (Reduce 1 181)
    (277, Token (QCONID _)) -> Just (Reduce 1 181)
    (277, Token (EXPORT _)) -> Just (Reduce 1 181)
    (277, Token (AS _)) -> Just (Reduce 1 181)
    (277, Token (QVARID _)) -> Just (Reduce 1 181)
    (277, Token (STRING _)) -> Just (Reduce 1 181)
    (277, Token (INTEGER _)) -> Just (Reduce 1 181)
    (277, Token (QVARSYM _)) -> Just (Reduce 1 181)
    (277, Token (QCONSYM _)) -> Just (Reduce 1 181)
    (277, Token (BACKQUOTE _)) -> Just (Reduce 1 181)
    (278, Token (RBRACE _)) -> Just (Reduce 1 180)
    (278, Token (LPAREN _)) -> Just (Reduce 1 180)
    (278, Token (COMMA _)) -> Just (Reduce 1 180)
    (278, Token (SEMICOLON _)) -> Just (Reduce 1 180)
    (278, Token (COLON_COLON _)) -> Just (Reduce 1 180)
    (278, Token (QCONID _)) -> Just (Reduce 1 180)
    (278, Token (EXPORT _)) -> Just (Reduce 1 180)
    (278, Token (AS _)) -> Just (Reduce 1 180)
    (278, Token (QVARID _)) -> Just (Reduce 1 180)
    (278, Token (STRING _)) -> Just (Reduce 1 180)
    (278, Token (INTEGER _)) -> Just (Reduce 1 180)
    (278, Token (QVARSYM _)) -> Just (Reduce 1 180)
    (278, Token (QCONSYM _)) -> Just (Reduce 1 180)
    (278, Token (BACKQUOTE _)) -> Just (Reduce 1 180)
    (279, Token (WHERE _)) -> Just (Reduce 3 106)
    (279, Token (LBRACE _)) -> Just (Reduce 3 106)
    (279, Token (RBRACE _)) -> Just (Reduce 3 106)
    (279, Token (LPAREN _)) -> Just (Reduce 3 106)
    (279, Token (RPAREN _)) -> Just (Reduce 3 106)
    (279, Token (COMMA _)) -> Just (Reduce 3 106)
    (279, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (279, Token (EQUAL _)) -> Just (Reduce 3 106)
    (279, Token (DERIVING _)) -> Just (Reduce 3 106)
    (279, Token (DARROW _)) -> Just (Reduce 3 106)
    (279, Token (PIPE _)) -> Just (Reduce 3 106)
    (279, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (279, Token (INFIXL _)) -> Just (Reduce 3 106)
    (279, Token (INFIXR _)) -> Just (Reduce 3 106)
    (279, Token (INFIX _)) -> Just (Reduce 3 106)
    (279, Token (RARROW _)) -> Just (Reduce 3 106)
    (279, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (279, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (279, Token (EXCL _)) -> Just (Reduce 3 106)
    (279, Token (QCONID _)) -> Just (Reduce 3 106)
    (279, Token (EXPORT _)) -> Just (Reduce 3 106)
    (279, Token (AS _)) -> Just (Reduce 3 106)
    (279, Token (QVARID _)) -> Just (Reduce 3 106)
    (279, Token (INTEGER _)) -> Just (Reduce 3 106)
    (279, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (279, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (280, Token (WHERE _)) -> Just (Reduce 3 104)
    (280, Token (LBRACE _)) -> Just (Reduce 3 104)
    (280, Token (RBRACE _)) -> Just (Reduce 3 104)
    (280, Token (LPAREN _)) -> Just (Reduce 3 104)
    (280, Token (RPAREN _)) -> Just (Reduce 3 104)
    (280, Token (COMMA _)) -> Just (Reduce 3 104)
    (280, Token (SEMICOLON _)) -> Just (Reduce 3 104)
    (280, Token (EQUAL _)) -> Just (Reduce 3 104)
    (280, Token (DERIVING _)) -> Just (Reduce 3 104)
    (280, Token (DARROW _)) -> Just (Reduce 3 104)
    (280, Token (PIPE _)) -> Just (Reduce 3 104)
    (280, Token (COLON_COLON _)) -> Just (Reduce 3 104)
    (280, Token (INFIXL _)) -> Just (Reduce 3 104)
    (280, Token (INFIXR _)) -> Just (Reduce 3 104)
    (280, Token (INFIX _)) -> Just (Reduce 3 104)
    (280, Token (RARROW _)) -> Just (Reduce 3 104)
    (280, Token (LBRACKET _)) -> Just (Reduce 3 104)
    (280, Token (RBRACKET _)) -> Just (Reduce 3 104)
    (280, Token (EXCL _)) -> Just (Reduce 3 104)
    (280, Token (QCONID _)) -> Just (Reduce 3 104)
    (280, Token (EXPORT _)) -> Just (Reduce 3 104)
    (280, Token (AS _)) -> Just (Reduce 3 104)
    (280, Token (QVARID _)) -> Just (Reduce 3 104)
    (280, Token (INTEGER _)) -> Just (Reduce 3 104)
    (280, Token (QVARSYM _)) -> Just (Reduce 3 104)
    (280, Token (QCONSYM _)) -> Just (Reduce 3 104)
    (280, Token (BACKQUOTE _)) -> Just (Reduce 3 104)
    (281, Token (WHERE _)) -> Just (Reduce 3 105)
    (281, Token (LBRACE _)) -> Just (Reduce 3 105)
    (281, Token (RBRACE _)) -> Just (Reduce 3 105)
    (281, Token (LPAREN _)) -> Just (Reduce 3 105)
    (281, Token (RPAREN _)) -> Just (Reduce 3 105)
    (281, Token (COMMA _)) -> Just (Reduce 3 105)
    (281, Token (SEMICOLON _)) -> Just (Reduce 3 105)
    (281, Token (EQUAL _)) -> Just (Reduce 3 105)
    (281, Token (DERIVING _)) -> Just (Reduce 3 105)
    (281, Token (DARROW _)) -> Just (Reduce 3 105)
    (281, Token (PIPE _)) -> Just (Reduce 3 105)
    (281, Token (COLON_COLON _)) -> Just (Reduce 3 105)
    (281, Token (INFIXL _)) -> Just (Reduce 3 105)
    (281, Token (INFIXR _)) -> Just (Reduce 3 105)
    (281, Token (INFIX _)) -> Just (Reduce 3 105)
    (281, Token (RARROW _)) -> Just (Reduce 3 105)
    (281, Token (LBRACKET _)) -> Just (Reduce 3 105)
    (281, Token (RBRACKET _)) -> Just (Reduce 3 105)
    (281, Token (EXCL _)) -> Just (Reduce 3 105)
    (281, Token (QCONID _)) -> Just (Reduce 3 105)
    (281, Token (EXPORT _)) -> Just (Reduce 3 105)
    (281, Token (AS _)) -> Just (Reduce 3 105)
    (281, Token (QVARID _)) -> Just (Reduce 3 105)
    (281, Token (INTEGER _)) -> Just (Reduce 3 105)
    (281, Token (QVARSYM _)) -> Just (Reduce 3 105)
    (281, Token (QCONSYM _)) -> Just (Reduce 3 105)
    (281, Token (BACKQUOTE _)) -> Just (Reduce 3 105)
    (282, Token (RPAREN _)) -> Just (Shift 279)
    (282, Token (COMMA _)) -> Just (Shift 121)
    (283, Token (RBRACKET _)) -> Just (Shift 281)
    (284, Token (WHERE _)) -> Just (Reduce 2 107)
    (284, Token (LBRACE _)) -> Just (Reduce 2 107)
    (284, Token (RBRACE _)) -> Just (Reduce 2 107)
    (284, Token (LPAREN _)) -> Just (Reduce 2 107)
    (284, Token (RPAREN _)) -> Just (Reduce 2 107)
    (284, Token (COMMA _)) -> Just (Reduce 2 107)
    (284, Token (SEMICOLON _)) -> Just (Reduce 2 107)
    (284, Token (EQUAL _)) -> Just (Reduce 2 107)
    (284, Token (DERIVING _)) -> Just (Reduce 2 107)
    (284, Token (DARROW _)) -> Just (Reduce 2 107)
    (284, Token (PIPE _)) -> Just (Reduce 2 107)
    (284, Token (COLON_COLON _)) -> Just (Reduce 2 107)
    (284, Token (INFIXL _)) -> Just (Reduce 2 107)
    (284, Token (INFIXR _)) -> Just (Reduce 2 107)
    (284, Token (INFIX _)) -> Just (Reduce 2 107)
    (284, Token (RARROW _)) -> Just (Reduce 2 107)
    (284, Token (LBRACKET _)) -> Just (Reduce 2 107)
    (284, Token (RBRACKET _)) -> Just (Reduce 2 107)
    (284, Token (EXCL _)) -> Just (Reduce 2 107)
    (284, Token (QCONID _)) -> Just (Reduce 2 107)
    (284, Token (EXPORT _)) -> Just (Reduce 2 107)
    (284, Token (AS _)) -> Just (Reduce 2 107)
    (284, Token (QVARID _)) -> Just (Reduce 2 107)
    (284, Token (INTEGER _)) -> Just (Reduce 2 107)
    (284, Token (QVARSYM _)) -> Just (Reduce 2 107)
    (284, Token (QCONSYM _)) -> Just (Reduce 2 107)
    (284, Token (BACKQUOTE _)) -> Just (Reduce 2 107)
    (285, Token (WHERE _)) -> Just (Reduce 1 102)
    (285, Token (LBRACE _)) -> Just (Reduce 1 102)
    (285, Token (RBRACE _)) -> Just (Reduce 1 102)
    (285, Token (LPAREN _)) -> Just (Reduce 1 102)
    (285, Token (RPAREN _)) -> Just (Reduce 1 102)
    (285, Token (COMMA _)) -> Just (Reduce 1 102)
    (285, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (285, Token (EQUAL _)) -> Just (Reduce 1 102)
    (285, Token (DERIVING _)) -> Just (Reduce 1 102)
    (285, Token (DARROW _)) -> Just (Reduce 1 102)
    (285, Token (PIPE _)) -> Just (Reduce 1 102)
    (285, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (285, Token (INFIXL _)) -> Just (Reduce 1 102)
    (285, Token (INFIXR _)) -> Just (Reduce 1 102)
    (285, Token (INFIX _)) -> Just (Reduce 1 102)
    (285, Token (RARROW _)) -> Just (Reduce 1 102)
    (285, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (285, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (285, Token (EXCL _)) -> Just (Reduce 1 102)
    (285, Token (QCONID _)) -> Just (Reduce 1 102)
    (285, Token (EXPORT _)) -> Just (Reduce 1 102)
    (285, Token (AS _)) -> Just (Reduce 1 102)
    (285, Token (QVARID _)) -> Just (Reduce 1 102)
    (285, Token (INTEGER _)) -> Just (Reduce 1 102)
    (285, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (285, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (285, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (286, Token (WHERE _)) -> Just (Reduce 1 103)
    (286, Token (LBRACE _)) -> Just (Reduce 1 103)
    (286, Token (RBRACE _)) -> Just (Reduce 1 103)
    (286, Token (LPAREN _)) -> Just (Reduce 1 103)
    (286, Token (RPAREN _)) -> Just (Reduce 1 103)
    (286, Token (COMMA _)) -> Just (Reduce 1 103)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 103)
    (286, Token (EQUAL _)) -> Just (Reduce 1 103)
    (286, Token (DERIVING _)) -> Just (Reduce 1 103)
    (286, Token (DARROW _)) -> Just (Reduce 1 103)
    (286, Token (PIPE _)) -> Just (Reduce 1 103)
    (286, Token (COLON_COLON _)) -> Just (Reduce 1 103)
    (286, Token (INFIXL _)) -> Just (Reduce 1 103)
    (286, Token (INFIXR _)) -> Just (Reduce 1 103)
    (286, Token (INFIX _)) -> Just (Reduce 1 103)
    (286, Token (RARROW _)) -> Just (Reduce 1 103)
    (286, Token (LBRACKET _)) -> Just (Reduce 1 103)
    (286, Token (RBRACKET _)) -> Just (Reduce 1 103)
    (286, Token (EXCL _)) -> Just (Reduce 1 103)
    (286, Token (QCONID _)) -> Just (Reduce 1 103)
    (286, Token (EXPORT _)) -> Just (Reduce 1 103)
    (286, Token (AS _)) -> Just (Reduce 1 103)
    (286, Token (QVARID _)) -> Just (Reduce 1 103)
    (286, Token (INTEGER _)) -> Just (Reduce 1 103)
    (286, Token (QVARSYM _)) -> Just (Reduce 1 103)
    (286, Token (QCONSYM _)) -> Just (Reduce 1 103)
    (286, Token (BACKQUOTE _)) -> Just (Reduce 1 103)
    (287, Token (RPAREN _)) -> Just (Shift 280)
    (288, Token (WHERE _)) -> Just (Reduce 2 111)
    (288, Token (LBRACE _)) -> Just (Reduce 2 111)
    (288, Token (RBRACE _)) -> Just (Reduce 2 111)
    (288, Token (LPAREN _)) -> Just (Reduce 2 111)
    (288, Token (RPAREN _)) -> Just (Reduce 2 111)
    (288, Token (COMMA _)) -> Just (Reduce 2 111)
    (288, Token (SEMICOLON _)) -> Just (Reduce 2 111)
    (288, Token (EQUAL _)) -> Just (Reduce 2 111)
    (288, Token (DERIVING _)) -> Just (Reduce 2 111)
    (288, Token (DARROW _)) -> Just (Reduce 2 111)
    (288, Token (PIPE _)) -> Just (Reduce 2 111)
    (288, Token (COLON_COLON _)) -> Just (Reduce 2 111)
    (288, Token (INFIXL _)) -> Just (Reduce 2 111)
    (288, Token (INFIXR _)) -> Just (Reduce 2 111)
    (288, Token (INFIX _)) -> Just (Reduce 2 111)
    (288, Token (RARROW _)) -> Just (Reduce 2 111)
    (288, Token (LBRACKET _)) -> Just (Reduce 2 111)
    (288, Token (RBRACKET _)) -> Just (Reduce 2 111)
    (288, Token (EXCL _)) -> Just (Reduce 2 111)
    (288, Token (QCONID _)) -> Just (Reduce 2 111)
    (288, Token (EXPORT _)) -> Just (Reduce 2 111)
    (288, Token (AS _)) -> Just (Reduce 2 111)
    (288, Token (QVARID _)) -> Just (Reduce 2 111)
    (288, Token (INTEGER _)) -> Just (Reduce 2 111)
    (288, Token (QVARSYM _)) -> Just (Reduce 2 111)
    (288, Token (QCONSYM _)) -> Just (Reduce 2 111)
    (288, Token (BACKQUOTE _)) -> Just (Reduce 2 111)
    (289, Token (WHERE _)) -> Just (Reduce 3 113)
    (289, Token (LBRACE _)) -> Just (Reduce 3 113)
    (289, Token (RBRACE _)) -> Just (Reduce 3 113)
    (289, Token (LPAREN _)) -> Just (Reduce 3 113)
    (289, Token (RPAREN _)) -> Just (Reduce 3 113)
    (289, Token (COMMA _)) -> Just (Reduce 3 113)
    (289, Token (SEMICOLON _)) -> Just (Reduce 3 113)
    (289, Token (EQUAL _)) -> Just (Reduce 3 113)
    (289, Token (DERIVING _)) -> Just (Reduce 3 113)
    (289, Token (DARROW _)) -> Just (Reduce 3 113)
    (289, Token (PIPE _)) -> Just (Reduce 3 113)
    (289, Token (COLON_COLON _)) -> Just (Reduce 3 113)
    (289, Token (INFIXL _)) -> Just (Reduce 3 113)
    (289, Token (INFIXR _)) -> Just (Reduce 3 113)
    (289, Token (INFIX _)) -> Just (Reduce 3 113)
    (289, Token (RARROW _)) -> Just (Reduce 3 113)
    (289, Token (LBRACKET _)) -> Just (Reduce 3 113)
    (289, Token (RBRACKET _)) -> Just (Reduce 3 113)
    (289, Token (EXCL _)) -> Just (Reduce 3 113)
    (289, Token (QCONID _)) -> Just (Reduce 3 113)
    (289, Token (EXPORT _)) -> Just (Reduce 3 113)
    (289, Token (AS _)) -> Just (Reduce 3 113)
    (289, Token (QVARID _)) -> Just (Reduce 3 113)
    (289, Token (INTEGER _)) -> Just (Reduce 3 113)
    (289, Token (QVARSYM _)) -> Just (Reduce 3 113)
    (289, Token (QCONSYM _)) -> Just (Reduce 3 113)
    (289, Token (BACKQUOTE _)) -> Just (Reduce 3 113)
    (290, Token (WHERE _)) -> Just (Reduce 3 114)
    (290, Token (LBRACE _)) -> Just (Reduce 3 114)
    (290, Token (RBRACE _)) -> Just (Reduce 3 114)
    (290, Token (LPAREN _)) -> Just (Reduce 3 114)
    (290, Token (RPAREN _)) -> Just (Reduce 3 114)
    (290, Token (COMMA _)) -> Just (Reduce 3 114)
    (290, Token (SEMICOLON _)) -> Just (Reduce 3 114)
    (290, Token (EQUAL _)) -> Just (Reduce 3 114)
    (290, Token (DERIVING _)) -> Just (Reduce 3 114)
    (290, Token (DARROW _)) -> Just (Reduce 3 114)
    (290, Token (PIPE _)) -> Just (Reduce 3 114)
    (290, Token (COLON_COLON _)) -> Just (Reduce 3 114)
    (290, Token (INFIXL _)) -> Just (Reduce 3 114)
    (290, Token (INFIXR _)) -> Just (Reduce 3 114)
    (290, Token (INFIX _)) -> Just (Reduce 3 114)
    (290, Token (RARROW _)) -> Just (Reduce 3 114)
    (290, Token (LBRACKET _)) -> Just (Reduce 3 114)
    (290, Token (RBRACKET _)) -> Just (Reduce 3 114)
    (290, Token (EXCL _)) -> Just (Reduce 3 114)
    (290, Token (QCONID _)) -> Just (Reduce 3 114)
    (290, Token (EXPORT _)) -> Just (Reduce 3 114)
    (290, Token (AS _)) -> Just (Reduce 3 114)
    (290, Token (QVARID _)) -> Just (Reduce 3 114)
    (290, Token (INTEGER _)) -> Just (Reduce 3 114)
    (290, Token (QVARSYM _)) -> Just (Reduce 3 114)
    (290, Token (QCONSYM _)) -> Just (Reduce 3 114)
    (290, Token (BACKQUOTE _)) -> Just (Reduce 3 114)
    (291, Token (RPAREN _)) -> Just (Shift 289)
    (292, Token (WHERE _)) -> Just (Reduce 2 112)
    (292, Token (LBRACE _)) -> Just (Reduce 2 112)
    (292, Token (RBRACE _)) -> Just (Reduce 2 112)
    (292, Token (LPAREN _)) -> Just (Reduce 2 112)
    (292, Token (RPAREN _)) -> Just (Reduce 2 112)
    (292, Token (COMMA _)) -> Just (Reduce 2 112)
    (292, Token (SEMICOLON _)) -> Just (Reduce 2 112)
    (292, Token (EQUAL _)) -> Just (Reduce 2 112)
    (292, Token (DERIVING _)) -> Just (Reduce 2 112)
    (292, Token (DARROW _)) -> Just (Reduce 2 112)
    (292, Token (PIPE _)) -> Just (Reduce 2 112)
    (292, Token (COLON_COLON _)) -> Just (Reduce 2 112)
    (292, Token (INFIXL _)) -> Just (Reduce 2 112)
    (292, Token (INFIXR _)) -> Just (Reduce 2 112)
    (292, Token (INFIX _)) -> Just (Reduce 2 112)
    (292, Token (RARROW _)) -> Just (Reduce 2 112)
    (292, Token (LBRACKET _)) -> Just (Reduce 2 112)
    (292, Token (RBRACKET _)) -> Just (Reduce 2 112)
    (292, Token (EXCL _)) -> Just (Reduce 2 112)
    (292, Token (QCONID _)) -> Just (Reduce 2 112)
    (292, Token (EXPORT _)) -> Just (Reduce 2 112)
    (292, Token (AS _)) -> Just (Reduce 2 112)
    (292, Token (QVARID _)) -> Just (Reduce 2 112)
    (292, Token (INTEGER _)) -> Just (Reduce 2 112)
    (292, Token (QVARSYM _)) -> Just (Reduce 2 112)
    (292, Token (QCONSYM _)) -> Just (Reduce 2 112)
    (292, Token (BACKQUOTE _)) -> Just (Reduce 2 112)
    (293, Token (WHERE _)) -> Just (Reduce 1 110)
    (293, Token (LBRACE _)) -> Just (Reduce 1 110)
    (293, Token (RBRACE _)) -> Just (Reduce 1 110)
    (293, Token (LPAREN _)) -> Just (Reduce 1 110)
    (293, Token (RPAREN _)) -> Just (Reduce 1 110)
    (293, Token (COMMA _)) -> Just (Reduce 1 110)
    (293, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (293, Token (EQUAL _)) -> Just (Reduce 1 110)
    (293, Token (DERIVING _)) -> Just (Reduce 1 110)
    (293, Token (DARROW _)) -> Just (Reduce 1 110)
    (293, Token (PIPE _)) -> Just (Reduce 1 110)
    (293, Token (COLON_COLON _)) -> Just (Reduce 1 110)
    (293, Token (INFIXL _)) -> Just (Reduce 1 110)
    (293, Token (INFIXR _)) -> Just (Reduce 1 110)
    (293, Token (INFIX _)) -> Just (Reduce 1 110)
    (293, Token (RARROW _)) -> Just (Reduce 1 110)
    (293, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (293, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (293, Token (EXCL _)) -> Just (Reduce 1 110)
    (293, Token (QCONID _)) -> Just (Reduce 1 110)
    (293, Token (EXPORT _)) -> Just (Reduce 1 110)
    (293, Token (AS _)) -> Just (Reduce 1 110)
    (293, Token (QVARID _)) -> Just (Reduce 1 110)
    (293, Token (INTEGER _)) -> Just (Reduce 1 110)
    (293, Token (QVARSYM _)) -> Just (Reduce 1 110)
    (293, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (293, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (294, Token (LBRACE _)) -> Just (Shift 65)
    (294, Token (RBRACE _)) -> Just (Reduce 1 110)
    (294, Token (LPAREN _)) -> Just (Reduce 1 110)
    (294, Token (RPAREN _)) -> Just (Reduce 1 110)
    (294, Token (COMMA _)) -> Just (Reduce 1 110)
    (294, Token (SEMICOLON _)) -> Just (Reduce 1 110)
    (294, Token (DERIVING _)) -> Just (Reduce 1 110)
    (294, Token (PIPE _)) -> Just (Reduce 1 110)
    (294, Token (RARROW _)) -> Just (Reduce 1 110)
    (294, Token (LBRACKET _)) -> Just (Reduce 1 110)
    (294, Token (RBRACKET _)) -> Just (Reduce 1 110)
    (294, Token (EXCL _)) -> Just (Reduce 1 110)
    (294, Token (QCONID _)) -> Just (Reduce 1 110)
    (294, Token (EXPORT _)) -> Just (Reduce 1 110)
    (294, Token (AS _)) -> Just (Reduce 1 110)
    (294, Token (QVARID _)) -> Just (Reduce 1 110)
    (294, Token (QCONSYM _)) -> Just (Reduce 1 110)
    (294, Token (BACKQUOTE _)) -> Just (Reduce 1 110)
    (295, Token (RPAREN _)) -> Just (Shift 290)
    (296, Token (WHERE _)) -> Just (Reduce 1 187)
    (296, Token (LBRACE _)) -> Just (Reduce 1 187)
    (296, Token (RBRACE _)) -> Just (Reduce 1 187)
    (296, Token (LPAREN _)) -> Just (Reduce 1 187)
    (296, Token (RPAREN _)) -> Just (Reduce 1 187)
    (296, Token (COMMA _)) -> Just (Reduce 1 187)
    (296, Token (SEMICOLON _)) -> Just (Reduce 1 187)
    (296, Token (EQUAL _)) -> Just (Reduce 1 187)
    (296, Token (DERIVING _)) -> Just (Reduce 1 187)
    (296, Token (DARROW _)) -> Just (Reduce 1 187)
    (296, Token (PIPE _)) -> Just (Reduce 1 187)
    (296, Token (COLON_COLON _)) -> Just (Reduce 1 187)
    (296, Token (INFIXL _)) -> Just (Reduce 1 187)
    (296, Token (INFIXR _)) -> Just (Reduce 1 187)
    (296, Token (INFIX _)) -> Just (Reduce 1 187)
    (296, Token (RARROW _)) -> Just (Reduce 1 187)
    (296, Token (LBRACKET _)) -> Just (Reduce 1 187)
    (296, Token (RBRACKET _)) -> Just (Reduce 1 187)
    (296, Token (EXCL _)) -> Just (Reduce 1 187)
    (296, Token (QCONID _)) -> Just (Reduce 1 187)
    (296, Token (EXPORT _)) -> Just (Reduce 1 187)
    (296, Token (AS _)) -> Just (Reduce 1 187)
    (296, Token (QVARID _)) -> Just (Reduce 1 187)
    (296, Token (INTEGER _)) -> Just (Reduce 1 187)
    (296, Token (QVARSYM _)) -> Just (Reduce 1 187)
    (296, Token (QCONSYM _)) -> Just (Reduce 1 187)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 1 187)
    (297, Token (WHERE _)) -> Just (Reduce 1 186)
    (297, Token (LBRACE _)) -> Just (Reduce 1 186)
    (297, Token (RBRACE _)) -> Just (Reduce 1 186)
    (297, Token (LPAREN _)) -> Just (Reduce 1 186)
    (297, Token (RPAREN _)) -> Just (Reduce 1 186)
    (297, Token (COMMA _)) -> Just (Reduce 1 186)
    (297, Token (SEMICOLON _)) -> Just (Reduce 1 186)
    (297, Token (EQUAL _)) -> Just (Reduce 1 186)
    (297, Token (DERIVING _)) -> Just (Reduce 1 186)
    (297, Token (DARROW _)) -> Just (Reduce 1 186)
    (297, Token (PIPE _)) -> Just (Reduce 1 186)
    (297, Token (COLON_COLON _)) -> Just (Reduce 1 186)
    (297, Token (INFIXL _)) -> Just (Reduce 1 186)
    (297, Token (INFIXR _)) -> Just (Reduce 1 186)
    (297, Token (INFIX _)) -> Just (Reduce 1 186)
    (297, Token (RARROW _)) -> Just (Reduce 1 186)
    (297, Token (LBRACKET _)) -> Just (Reduce 1 186)
    (297, Token (RBRACKET _)) -> Just (Reduce 1 186)
    (297, Token (EXCL _)) -> Just (Reduce 1 186)
    (297, Token (QCONID _)) -> Just (Reduce 1 186)
    (297, Token (EXPORT _)) -> Just (Reduce 1 186)
    (297, Token (AS _)) -> Just (Reduce 1 186)
    (297, Token (QVARID _)) -> Just (Reduce 1 186)
    (297, Token (INTEGER _)) -> Just (Reduce 1 186)
    (297, Token (QVARSYM _)) -> Just (Reduce 1 186)
    (297, Token (QCONSYM _)) -> Just (Reduce 1 186)
    (297, Token (BACKQUOTE _)) -> Just (Reduce 1 186)
    (298, Token (WHERE _)) -> Just (Reduce 1 188)
    (298, Token (LBRACE _)) -> Just (Reduce 1 188)
    (298, Token (RBRACE _)) -> Just (Reduce 1 188)
    (298, Token (LPAREN _)) -> Just (Reduce 1 188)
    (298, Token (RPAREN _)) -> Just (Reduce 1 188)
    (298, Token (COMMA _)) -> Just (Reduce 1 188)
    (298, Token (SEMICOLON _)) -> Just (Reduce 1 188)
    (298, Token (EQUAL _)) -> Just (Reduce 1 188)
    (298, Token (DERIVING _)) -> Just (Reduce 1 188)
    (298, Token (DARROW _)) -> Just (Reduce 1 188)
    (298, Token (PIPE _)) -> Just (Reduce 1 188)
    (298, Token (COLON_COLON _)) -> Just (Reduce 1 188)
    (298, Token (INFIXL _)) -> Just (Reduce 1 188)
    (298, Token (INFIXR _)) -> Just (Reduce 1 188)
    (298, Token (INFIX _)) -> Just (Reduce 1 188)
    (298, Token (RARROW _)) -> Just (Reduce 1 188)
    (298, Token (LBRACKET _)) -> Just (Reduce 1 188)
    (298, Token (RBRACKET _)) -> Just (Reduce 1 188)
    (298, Token (EXCL _)) -> Just (Reduce 1 188)
    (298, Token (QCONID _)) -> Just (Reduce 1 188)
    (298, Token (EXPORT _)) -> Just (Reduce 1 188)
    (298, Token (AS _)) -> Just (Reduce 1 188)
    (298, Token (QVARID _)) -> Just (Reduce 1 188)
    (298, Token (INTEGER _)) -> Just (Reduce 1 188)
    (298, Token (QVARSYM _)) -> Just (Reduce 1 188)
    (298, Token (QCONSYM _)) -> Just (Reduce 1 188)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 1 188)
    (299, Token (RPAREN _)) -> Just (Reduce 3 108)
    (299, Token (COMMA _)) -> Just (Shift 121)
    (300, Token (RPAREN _)) -> Just (Reduce 3 109)
    (301, Token (RPAREN _)) -> Just (Reduce 1 115)
    (301, Token (COMMA _)) -> Just (Shift 301)
    (302, Token (RPAREN _)) -> Just (Reduce 2 116)
    (303, Token (RBRACE _)) -> Just (Reduce 3 120)
    (303, Token (SEMICOLON _)) -> Just (Reduce 3 120)
    (303, Token (DERIVING _)) -> Just (Reduce 3 120)
    (304, Token (RBRACE _)) -> Just (Reduce 1 119)
    (304, Token (SEMICOLON _)) -> Just (Reduce 1 119)
    (304, Token (DERIVING _)) -> Just (Reduce 1 119)
    (304, Token (PIPE _)) -> Just (Shift 97)
    (305, Token (RBRACE _)) -> Just (Reduce 3 123)
    (305, Token (SEMICOLON _)) -> Just (Reduce 3 123)
    (305, Token (DERIVING _)) -> Just (Reduce 3 123)
    (305, Token (PIPE _)) -> Just (Reduce 3 123)
    (306, Token (RBRACE _)) -> Just (Reduce 4 124)
    (306, Token (SEMICOLON _)) -> Just (Reduce 4 124)
    (306, Token (DERIVING _)) -> Just (Reduce 4 124)
    (306, Token (PIPE _)) -> Just (Reduce 4 124)
    (307, Token (RBRACE _)) -> Just (Shift 306)
    (308, Token (BACKQUOTE _)) -> Just (Shift 312)
    (309, Token (RBRACE _)) -> Just (Reduce 1 178)
    (309, Token (LPAREN _)) -> Just (Reduce 1 178)
    (309, Token (RPAREN _)) -> Just (Reduce 1 178)
    (309, Token (COMMA _)) -> Just (Reduce 1 178)
    (309, Token (SEMICOLON _)) -> Just (Reduce 1 178)
    (309, Token (COLON_COLON _)) -> Just (Reduce 1 178)
    (309, Token (RARROW _)) -> Just (Reduce 1 178)
    (309, Token (LBRACKET _)) -> Just (Reduce 1 178)
    (309, Token (RBRACKET _)) -> Just (Reduce 1 178)
    (309, Token (EXCL _)) -> Just (Reduce 1 178)
    (309, Token (QCONID _)) -> Just (Reduce 1 178)
    (309, Token (EXPORT _)) -> Just (Reduce 1 178)
    (309, Token (AS _)) -> Just (Reduce 1 178)
    (309, Token (QVARID _)) -> Just (Reduce 1 178)
    (309, Token (STRING _)) -> Just (Reduce 1 178)
    (309, Token (INTEGER _)) -> Just (Reduce 1 178)
    (309, Token (QVARSYM _)) -> Just (Reduce 1 178)
    (309, Token (QCONSYM _)) -> Just (Reduce 1 178)
    (309, Token (BACKQUOTE _)) -> Just (Reduce 1 178)
    (310, Token (QCONID _)) -> Just (Shift 308)
    (310, Token (EXPORT _)) -> Just (Shift 341)
    (310, Token (AS _)) -> Just (Shift 342)
    (310, Token (QVARID _)) -> Just (Shift 343)
    (311, Token (QCONID _)) -> Just (Shift 308)
    (312, Token (RBRACE _)) -> Just (Reduce 3 179)
    (312, Token (LPAREN _)) -> Just (Reduce 3 179)
    (312, Token (RPAREN _)) -> Just (Reduce 3 179)
    (312, Token (COMMA _)) -> Just (Reduce 3 179)
    (312, Token (SEMICOLON _)) -> Just (Reduce 3 179)
    (312, Token (COLON_COLON _)) -> Just (Reduce 3 179)
    (312, Token (RARROW _)) -> Just (Reduce 3 179)
    (312, Token (LBRACKET _)) -> Just (Reduce 3 179)
    (312, Token (RBRACKET _)) -> Just (Reduce 3 179)
    (312, Token (EXCL _)) -> Just (Reduce 3 179)
    (312, Token (QCONID _)) -> Just (Reduce 3 179)
    (312, Token (EXPORT _)) -> Just (Reduce 3 179)
    (312, Token (AS _)) -> Just (Reduce 3 179)
    (312, Token (QVARID _)) -> Just (Reduce 3 179)
    (312, Token (STRING _)) -> Just (Reduce 3 179)
    (312, Token (INTEGER _)) -> Just (Reduce 3 179)
    (312, Token (QVARSYM _)) -> Just (Reduce 3 179)
    (312, Token (QCONSYM _)) -> Just (Reduce 3 179)
    (312, Token (BACKQUOTE _)) -> Just (Reduce 3 179)
    (313, Token (RBRACE _)) -> Just (Reduce 3 128)
    (314, Token (RBRACE _)) -> Just (Reduce 1 127)
    (314, Token (COMMA _)) -> Just (Shift 66)
    (315, Token (RBRACE _)) -> Just (Reduce 3 129)
    (315, Token (COMMA _)) -> Just (Reduce 3 129)
    (316, Token (COLON_COLON _)) -> Just (Shift 109)
    (317, Token (EXPORT _)) -> Just (Reduce 1 137)
    (317, Token (AS _)) -> Just (Reduce 1 137)
    (317, Token (QVARID _)) -> Just (Reduce 1 137)
    (317, Token (STRING _)) -> Just (Reduce 1 137)
    (318, Token (EXPORT _)) -> Just (Reduce 1 136)
    (318, Token (AS _)) -> Just (Reduce 1 136)
    (318, Token (QVARID _)) -> Just (Reduce 1 136)
    (318, Token (STRING _)) -> Just (Reduce 1 136)
    (319, Token (EXPORT _)) -> Just (Reduce 1 138)
    (319, Token (AS _)) -> Just (Reduce 1 138)
    (319, Token (QVARID _)) -> Just (Reduce 1 138)
    (319, Token (STRING _)) -> Just (Reduce 1 138)
    (320, Token (LPAREN _)) -> Just (Reduce 1 139)
    (320, Token (EXPORT _)) -> Just (Reduce 1 139)
    (320, Token (AS _)) -> Just (Reduce 1 139)
    (320, Token (QVARID _)) -> Just (Reduce 1 139)
    (320, Token (QVARSYM _)) -> Just (Reduce 1 139)
    (321, Token (STRING _)) -> Just (Reduce 1 142)
    (322, Token (STRING _)) -> Just (Reduce 1 141)
    (323, Token (STRING _)) -> Just (Reduce 1 143)
    (324, Token (LPAREN _)) -> Just (Reduce 1 140)
    (324, Token (EXPORT _)) -> Just (Reduce 1 140)
    (324, Token (AS _)) -> Just (Reduce 1 140)
    (324, Token (QVARID _)) -> Just (Reduce 1 140)
    (324, Token (QVARSYM _)) -> Just (Reduce 1 140)
    (325, Token (EQUAL _)) -> Just (Reduce 3 147)
    (326, Token (COMMA _)) -> Just (Shift 52)
    (326, Token (EQUAL _)) -> Just (Reduce 1 146)
    (327, Token (COMMA _)) -> Just (Reduce 2 149)
    (327, Token (EQUAL _)) -> Just (Reduce 2 149)
    (328, Token (COMMA _)) -> Just (Reduce 3 148)
    (328, Token (EQUAL _)) -> Just (Reduce 3 148)
    (329, Token (COMMA _)) -> Just (Reduce 1 150)
    (329, Token (EQUAL _)) -> Just (Reduce 1 150)
    (329, Token (LARROW _)) -> Just (Shift 53)
    (330, Token (WHERE _)) -> Just (Reduce 1 154)
    (330, Token (RBRACE _)) -> Just (Reduce 1 154)
    (330, Token (RPAREN _)) -> Just (Reduce 1 154)
    (330, Token (COMMA _)) -> Just (Reduce 1 154)
    (330, Token (SEMICOLON _)) -> Just (Reduce 1 154)
    (330, Token (EQUAL _)) -> Just (Reduce 1 154)
    (330, Token (PIPE _)) -> Just (Reduce 1 154)
    (330, Token (COLON_COLON _)) -> Just (Reduce 1 154)
    (330, Token (LARROW _)) -> Just (Reduce 1 154)
    (331, Token (WHERE _)) -> Just (Reduce 1 156)
    (331, Token (RBRACE _)) -> Just (Reduce 1 156)
    (331, Token (LPAREN _)) -> Just (Reduce 1 156)
    (331, Token (RPAREN _)) -> Just (Reduce 1 156)
    (331, Token (COMMA _)) -> Just (Reduce 1 156)
    (331, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (331, Token (EQUAL _)) -> Just (Reduce 1 156)
    (331, Token (PIPE _)) -> Just (Reduce 1 156)
    (331, Token (COLON_COLON _)) -> Just (Reduce 1 156)
    (331, Token (QCONID _)) -> Just (Reduce 1 156)
    (331, Token (EXPORT _)) -> Just (Reduce 1 156)
    (331, Token (AS _)) -> Just (Reduce 1 156)
    (331, Token (QVARID _)) -> Just (Reduce 1 156)
    (331, Token (STRING _)) -> Just (Reduce 1 156)
    (331, Token (LARROW _)) -> Just (Reduce 1 156)
    (331, Token (INTEGER _)) -> Just (Reduce 1 156)
    (331, Token (QVARSYM _)) -> Just (Reduce 1 156)
    (331, Token (QCONSYM _)) -> Just (Reduce 1 156)
    (331, Token (BACKQUOTE _)) -> Just (Reduce 1 156)
    (332, Token (WHERE _)) -> Just (Reduce 3 158)
    (332, Token (RBRACE _)) -> Just (Reduce 3 158)
    (332, Token (LPAREN _)) -> Just (Reduce 3 158)
    (332, Token (RPAREN _)) -> Just (Reduce 3 158)
    (332, Token (COMMA _)) -> Just (Reduce 3 158)
    (332, Token (SEMICOLON _)) -> Just (Reduce 3 158)
    (332, Token (EQUAL _)) -> Just (Reduce 3 158)
    (332, Token (PIPE _)) -> Just (Reduce 3 158)
    (332, Token (COLON_COLON _)) -> Just (Reduce 3 158)
    (332, Token (QCONID _)) -> Just (Reduce 3 158)
    (332, Token (EXPORT _)) -> Just (Reduce 3 158)
    (332, Token (AS _)) -> Just (Reduce 3 158)
    (332, Token (QVARID _)) -> Just (Reduce 3 158)
    (332, Token (STRING _)) -> Just (Reduce 3 158)
    (332, Token (LARROW _)) -> Just (Reduce 3 158)
    (332, Token (INTEGER _)) -> Just (Reduce 3 158)
    (332, Token (QVARSYM _)) -> Just (Reduce 3 158)
    (332, Token (QCONSYM _)) -> Just (Reduce 3 158)
    (332, Token (BACKQUOTE _)) -> Just (Reduce 3 158)
    (333, Token (WHERE _)) -> Just (Reduce 2 157)
    (333, Token (RBRACE _)) -> Just (Reduce 2 157)
    (333, Token (LPAREN _)) -> Just (Reduce 2 157)
    (333, Token (RPAREN _)) -> Just (Reduce 2 157)
    (333, Token (COMMA _)) -> Just (Reduce 2 157)
    (333, Token (SEMICOLON _)) -> Just (Reduce 2 157)
    (333, Token (EQUAL _)) -> Just (Reduce 2 157)
    (333, Token (PIPE _)) -> Just (Reduce 2 157)
    (333, Token (COLON_COLON _)) -> Just (Reduce 2 157)
    (333, Token (QCONID _)) -> Just (Reduce 2 157)
    (333, Token (EXPORT _)) -> Just (Reduce 2 157)
    (333, Token (AS _)) -> Just (Reduce 2 157)
    (333, Token (QVARID _)) -> Just (Reduce 2 157)
    (333, Token (STRING _)) -> Just (Reduce 2 157)
    (333, Token (LARROW _)) -> Just (Reduce 2 157)
    (333, Token (INTEGER _)) -> Just (Reduce 2 157)
    (333, Token (QVARSYM _)) -> Just (Reduce 2 157)
    (333, Token (QCONSYM _)) -> Just (Reduce 2 157)
    (333, Token (BACKQUOTE _)) -> Just (Reduce 2 157)
    (334, Token (WHERE _)) -> Just (Reduce 3 162)
    (334, Token (RBRACE _)) -> Just (Reduce 3 162)
    (334, Token (LPAREN _)) -> Just (Reduce 3 162)
    (334, Token (RPAREN _)) -> Just (Reduce 3 162)
    (334, Token (COMMA _)) -> Just (Reduce 3 162)
    (334, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (334, Token (EQUAL _)) -> Just (Reduce 3 162)
    (334, Token (PIPE _)) -> Just (Reduce 3 162)
    (334, Token (COLON_COLON _)) -> Just (Reduce 3 162)
    (334, Token (QCONID _)) -> Just (Reduce 3 162)
    (334, Token (EXPORT _)) -> Just (Reduce 3 162)
    (334, Token (AS _)) -> Just (Reduce 3 162)
    (334, Token (QVARID _)) -> Just (Reduce 3 162)
    (334, Token (STRING _)) -> Just (Reduce 3 162)
    (334, Token (LARROW _)) -> Just (Reduce 3 162)
    (334, Token (INTEGER _)) -> Just (Reduce 3 162)
    (334, Token (QVARSYM _)) -> Just (Reduce 3 162)
    (334, Token (QCONSYM _)) -> Just (Reduce 3 162)
    (334, Token (BACKQUOTE _)) -> Just (Reduce 3 162)
    (335, Token (WHERE _)) -> Just (Reduce 1 161)
    (335, Token (RBRACE _)) -> Just (Reduce 1 161)
    (335, Token (LPAREN _)) -> Just (Reduce 1 161)
    (335, Token (RPAREN _)) -> Just (Reduce 1 161)
    (335, Token (COMMA _)) -> Just (Reduce 1 161)
    (335, Token (SEMICOLON _)) -> Just (Reduce 1 161)
    (335, Token (EQUAL _)) -> Just (Reduce 1 161)
    (335, Token (PIPE _)) -> Just (Reduce 1 161)
    (335, Token (COLON_COLON _)) -> Just (Reduce 1 161)
    (335, Token (QCONID _)) -> Just (Reduce 1 161)
    (335, Token (EXPORT _)) -> Just (Reduce 1 161)
    (335, Token (AS _)) -> Just (Reduce 1 161)
    (335, Token (QVARID _)) -> Just (Reduce 1 161)
    (335, Token (STRING _)) -> Just (Reduce 1 161)
    (335, Token (LARROW _)) -> Just (Reduce 1 161)
    (335, Token (INTEGER _)) -> Just (Reduce 1 161)
    (335, Token (QVARSYM _)) -> Just (Reduce 1 161)
    (335, Token (QCONSYM _)) -> Just (Reduce 1 161)
    (335, Token (BACKQUOTE _)) -> Just (Reduce 1 161)
    (336, Token (WHERE _)) -> Just (Reduce 1 160)
    (336, Token (RBRACE _)) -> Just (Reduce 1 160)
    (336, Token (LPAREN _)) -> Just (Reduce 1 160)
    (336, Token (RPAREN _)) -> Just (Reduce 1 160)
    (336, Token (COMMA _)) -> Just (Reduce 1 160)
    (336, Token (SEMICOLON _)) -> Just (Reduce 1 160)
    (336, Token (EQUAL _)) -> Just (Reduce 1 160)
    (336, Token (PIPE _)) -> Just (Reduce 1 160)
    (336, Token (COLON_COLON _)) -> Just (Reduce 1 160)
    (336, Token (QCONID _)) -> Just (Reduce 1 160)
    (336, Token (EXPORT _)) -> Just (Reduce 1 160)
    (336, Token (AS _)) -> Just (Reduce 1 160)
    (336, Token (QVARID _)) -> Just (Reduce 1 160)
    (336, Token (STRING _)) -> Just (Reduce 1 160)
    (336, Token (LARROW _)) -> Just (Reduce 1 160)
    (336, Token (INTEGER _)) -> Just (Reduce 1 160)
    (336, Token (QVARSYM _)) -> Just (Reduce 1 160)
    (336, Token (QCONSYM _)) -> Just (Reduce 1 160)
    (336, Token (BACKQUOTE _)) -> Just (Reduce 1 160)
    (337, Token (WHERE _)) -> Just (Reduce 1 159)
    (337, Token (RBRACE _)) -> Just (Reduce 1 159)
    (337, Token (LPAREN _)) -> Just (Reduce 1 159)
    (337, Token (RPAREN _)) -> Just (Reduce 1 159)
    (337, Token (COMMA _)) -> Just (Reduce 1 159)
    (337, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (337, Token (EQUAL _)) -> Just (Reduce 1 159)
    (337, Token (PIPE _)) -> Just (Reduce 1 159)
    (337, Token (COLON_COLON _)) -> Just (Reduce 1 159)
    (337, Token (QCONID _)) -> Just (Reduce 1 159)
    (337, Token (EXPORT _)) -> Just (Reduce 1 159)
    (337, Token (AS _)) -> Just (Reduce 1 159)
    (337, Token (QVARID _)) -> Just (Reduce 1 159)
    (337, Token (STRING _)) -> Just (Reduce 1 159)
    (337, Token (LARROW _)) -> Just (Reduce 1 159)
    (337, Token (INTEGER _)) -> Just (Reduce 1 159)
    (337, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (337, Token (QCONSYM _)) -> Just (Reduce 1 159)
    (337, Token (BACKQUOTE _)) -> Just (Reduce 1 159)
    (338, Token (RPAREN _)) -> Just (Shift 334)
    (339, Token (LPAREN _)) -> Just (Reduce 3 167)
    (339, Token (RPAREN _)) -> Just (Reduce 3 167)
    (339, Token (EQUAL _)) -> Just (Reduce 3 167)
    (339, Token (PIPE _)) -> Just (Reduce 3 167)
    (339, Token (QCONID _)) -> Just (Reduce 3 167)
    (339, Token (EXPORT _)) -> Just (Reduce 3 167)
    (339, Token (AS _)) -> Just (Reduce 3 167)
    (339, Token (QVARID _)) -> Just (Reduce 3 167)
    (339, Token (QVARSYM _)) -> Just (Reduce 3 167)
    (339, Token (QCONSYM _)) -> Just (Reduce 3 167)
    (339, Token (BACKQUOTE _)) -> Just (Reduce 3 167)
    (340, Token (LPAREN _)) -> Just (Reduce 1 166)
    (340, Token (RPAREN _)) -> Just (Reduce 1 166)
    (340, Token (EQUAL _)) -> Just (Reduce 1 166)
    (340, Token (PIPE _)) -> Just (Reduce 1 166)
    (340, Token (QCONID _)) -> Just (Reduce 1 166)
    (340, Token (EXPORT _)) -> Just (Reduce 1 166)
    (340, Token (AS _)) -> Just (Reduce 1 166)
    (340, Token (QVARID _)) -> Just (Reduce 1 166)
    (340, Token (QVARSYM _)) -> Just (Reduce 1 166)
    (340, Token (QCONSYM _)) -> Just (Reduce 1 166)
    (340, Token (BACKQUOTE _)) -> Just (Reduce 1 166)
    (341, Token (BACKQUOTE _)) -> Just (Shift 345)
    (342, Token (BACKQUOTE _)) -> Just (Shift 346)
    (343, Token (BACKQUOTE _)) -> Just (Shift 347)
    (344, Token (RBRACE _)) -> Just (Reduce 1 174)
    (344, Token (LPAREN _)) -> Just (Reduce 1 174)
    (344, Token (COMMA _)) -> Just (Reduce 1 174)
    (344, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (344, Token (COLON_COLON _)) -> Just (Reduce 1 174)
    (344, Token (QCONID _)) -> Just (Reduce 1 174)
    (344, Token (EXPORT _)) -> Just (Reduce 1 174)
    (344, Token (AS _)) -> Just (Reduce 1 174)
    (344, Token (QVARID _)) -> Just (Reduce 1 174)
    (344, Token (STRING _)) -> Just (Reduce 1 174)
    (344, Token (INTEGER _)) -> Just (Reduce 1 174)
    (344, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (344, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (344, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (345, Token (RBRACE _)) -> Just (Reduce 3 176)
    (345, Token (LPAREN _)) -> Just (Reduce 3 176)
    (345, Token (COMMA _)) -> Just (Reduce 3 176)
    (345, Token (SEMICOLON _)) -> Just (Reduce 3 176)
    (345, Token (COLON_COLON _)) -> Just (Reduce 3 176)
    (345, Token (QCONID _)) -> Just (Reduce 3 176)
    (345, Token (EXPORT _)) -> Just (Reduce 3 176)
    (345, Token (AS _)) -> Just (Reduce 3 176)
    (345, Token (QVARID _)) -> Just (Reduce 3 176)
    (345, Token (STRING _)) -> Just (Reduce 3 176)
    (345, Token (INTEGER _)) -> Just (Reduce 3 176)
    (345, Token (QVARSYM _)) -> Just (Reduce 3 176)
    (345, Token (QCONSYM _)) -> Just (Reduce 3 176)
    (345, Token (BACKQUOTE _)) -> Just (Reduce 3 176)
    (346, Token (RBRACE _)) -> Just (Reduce 3 175)
    (346, Token (LPAREN _)) -> Just (Reduce 3 175)
    (346, Token (COMMA _)) -> Just (Reduce 3 175)
    (346, Token (SEMICOLON _)) -> Just (Reduce 3 175)
    (346, Token (COLON_COLON _)) -> Just (Reduce 3 175)
    (346, Token (QCONID _)) -> Just (Reduce 3 175)
    (346, Token (EXPORT _)) -> Just (Reduce 3 175)
    (346, Token (AS _)) -> Just (Reduce 3 175)
    (346, Token (QVARID _)) -> Just (Reduce 3 175)
    (346, Token (STRING _)) -> Just (Reduce 3 175)
    (346, Token (INTEGER _)) -> Just (Reduce 3 175)
    (346, Token (QVARSYM _)) -> Just (Reduce 3 175)
    (346, Token (QCONSYM _)) -> Just (Reduce 3 175)
    (346, Token (BACKQUOTE _)) -> Just (Reduce 3 175)
    (347, Token (RBRACE _)) -> Just (Reduce 3 177)
    (347, Token (LPAREN _)) -> Just (Reduce 3 177)
    (347, Token (COMMA _)) -> Just (Reduce 3 177)
    (347, Token (SEMICOLON _)) -> Just (Reduce 3 177)
    (347, Token (COLON_COLON _)) -> Just (Reduce 3 177)
    (347, Token (QCONID _)) -> Just (Reduce 3 177)
    (347, Token (EXPORT _)) -> Just (Reduce 3 177)
    (347, Token (AS _)) -> Just (Reduce 3 177)
    (347, Token (QVARID _)) -> Just (Reduce 3 177)
    (347, Token (STRING _)) -> Just (Reduce 3 177)
    (347, Token (INTEGER _)) -> Just (Reduce 3 177)
    (347, Token (QVARSYM _)) -> Just (Reduce 3 177)
    (347, Token (QCONSYM _)) -> Just (Reduce 3 177)
    (347, Token (BACKQUOTE _)) -> Just (Reduce 3 177)
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
production 152 = 32
production 153 = 32
production 154 = 61
production 155 = 62
production 156 = 63
production 157 = 63
production 158 = 63
production 159 = 64
production 160 = 64
production 161 = 64
production 162 = 64
production 163 = 31
production 164 = 31
production 165 = 31
production 166 = 65
production 167 = 65
production 168 = 8
production 169 = 8
production 170 = 8
production 171 = 8
production 172 = 9
production 173 = 9
production 174 = 66
production 175 = 66
production 176 = 66
production 177 = 66
production 178 = 52
production 179 = 52
production 180 = 44
production 181 = 44
production 182 = 16
production 183 = 16
production 184 = 15
production 185 = 15
production 186 = 47
production 187 = 47
production 188 = 47
production 189 = 67
production 190 = 1
production 191 = 42
production 192 = 42

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 164
    (9, 1) -> Just 189
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 268
    (13, 14) -> Just 18
    (13, 27) -> Just 187
    (13, 30) -> Just 223
    (13, 31) -> Just 54
    (13, 40) -> Just 237
    (13, 41) -> Just 238
    (13, 65) -> Just 241
    (16, 4) -> Just 17
    (16, 8) -> Just 268
    (16, 14) -> Just 18
    (16, 27) -> Just 187
    (16, 30) -> Just 223
    (16, 31) -> Just 54
    (16, 40) -> Just 237
    (16, 41) -> Just 238
    (16, 65) -> Just 241
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 138
    (25, 9) -> Just 139
    (25, 10) -> Just 33
    (25, 13) -> Just 128
    (34, 8) -> Just 337
    (34, 44) -> Just 35
    (34, 52) -> Just 277
    (34, 64) -> Just 333
    (34, 66) -> Just 278
    (35, 8) -> Just 337
    (35, 64) -> Just 332
    (36, 8) -> Just 337
    (36, 32) -> Just 247
    (36, 61) -> Just 246
    (36, 62) -> Just 330
    (36, 63) -> Just 34
    (36, 64) -> Just 331
    (37, 8) -> Just 337
    (37, 32) -> Just 224
    (37, 61) -> Just 246
    (37, 62) -> Just 330
    (37, 63) -> Just 34
    (37, 64) -> Just 331
    (38, 8) -> Just 337
    (38, 32) -> Just 257
    (38, 61) -> Just 246
    (38, 62) -> Just 330
    (38, 63) -> Just 34
    (38, 64) -> Just 331
    (39, 8) -> Just 337
    (39, 32) -> Just 265
    (39, 61) -> Just 246
    (39, 62) -> Just 330
    (39, 63) -> Just 34
    (39, 64) -> Just 331
    (40, 8) -> Just 340
    (40, 65) -> Just 243
    (41, 8) -> Just 337
    (41, 32) -> Just 338
    (41, 61) -> Just 246
    (41, 62) -> Just 330
    (41, 63) -> Just 34
    (41, 64) -> Just 331
    (42, 8) -> Just 340
    (42, 31) -> Just 43
    (42, 65) -> Just 241
    (43, 8) -> Just 340
    (43, 44) -> Just 40
    (43, 52) -> Just 277
    (43, 65) -> Just 242
    (43, 66) -> Just 278
    (44, 8) -> Just 268
    (44, 27) -> Just 233
    (44, 29) -> Just 232
    (44, 30) -> Just 223
    (44, 31) -> Just 54
    (44, 40) -> Just 237
    (44, 41) -> Just 238
    (44, 65) -> Just 241
    (45, 8) -> Just 268
    (45, 27) -> Just 233
    (45, 29) -> Just 234
    (45, 30) -> Just 223
    (45, 31) -> Just 54
    (45, 40) -> Just 237
    (45, 41) -> Just 238
    (45, 65) -> Just 241
    (46, 8) -> Just 268
    (46, 30) -> Just 256
    (46, 31) -> Just 57
    (46, 35) -> Just 251
    (46, 36) -> Just 253
    (46, 40) -> Just 237
    (46, 41) -> Just 238
    (46, 65) -> Just 241
    (47, 8) -> Just 268
    (47, 30) -> Just 256
    (47, 31) -> Just 57
    (47, 35) -> Just 252
    (47, 36) -> Just 253
    (47, 40) -> Just 237
    (47, 41) -> Just 238
    (47, 65) -> Just 241
    (48, 8) -> Just 337
    (48, 33) -> Just 225
    (48, 59) -> Just 249
    (48, 60) -> Just 326
    (48, 61) -> Just 329
    (48, 62) -> Just 330
    (48, 63) -> Just 34
    (48, 64) -> Just 331
    (49, 8) -> Just 337
    (49, 33) -> Just 248
    (49, 59) -> Just 249
    (49, 60) -> Just 326
    (49, 61) -> Just 329
    (49, 62) -> Just 330
    (49, 63) -> Just 34
    (49, 64) -> Just 331
    (50, 8) -> Just 337
    (50, 33) -> Just 258
    (50, 59) -> Just 249
    (50, 60) -> Just 326
    (50, 61) -> Just 329
    (50, 62) -> Just 330
    (50, 63) -> Just 34
    (50, 64) -> Just 331
    (51, 8) -> Just 337
    (51, 33) -> Just 266
    (51, 59) -> Just 249
    (51, 60) -> Just 326
    (51, 61) -> Just 329
    (51, 62) -> Just 330
    (51, 63) -> Just 34
    (51, 64) -> Just 331
    (52, 8) -> Just 337
    (52, 59) -> Just 325
    (52, 60) -> Just 326
    (52, 61) -> Just 329
    (52, 62) -> Just 330
    (52, 63) -> Just 34
    (52, 64) -> Just 331
    (53, 8) -> Just 337
    (53, 32) -> Just 328
    (53, 61) -> Just 246
    (53, 62) -> Just 330
    (53, 63) -> Just 34
    (53, 64) -> Just 331
    (54, 8) -> Just 340
    (54, 44) -> Just 40
    (54, 52) -> Just 277
    (54, 65) -> Just 242
    (54, 66) -> Just 278
    (55, 8) -> Just 340
    (55, 31) -> Just 58
    (55, 38) -> Just 260
    (55, 39) -> Just 262
    (55, 65) -> Just 241
    (56, 8) -> Just 340
    (56, 31) -> Just 58
    (56, 38) -> Just 261
    (56, 39) -> Just 262
    (56, 65) -> Just 241
    (57, 8) -> Just 340
    (57, 44) -> Just 40
    (57, 52) -> Just 277
    (57, 65) -> Just 242
    (57, 66) -> Just 278
    (58, 8) -> Just 340
    (58, 44) -> Just 40
    (58, 52) -> Just 277
    (58, 65) -> Just 242
    (58, 66) -> Just 278
    (59, 8) -> Just 135
    (59, 9) -> Just 136
    (59, 11) -> Just 129
    (59, 12) -> Just 130
    (60, 8) -> Just 135
    (60, 9) -> Just 136
    (60, 11) -> Just 165
    (60, 12) -> Just 130
    (61, 8) -> Just 135
    (61, 9) -> Just 136
    (61, 11) -> Just 166
    (61, 12) -> Just 130
    (62, 8) -> Just 138
    (62, 9) -> Just 139
    (62, 10) -> Just 127
    (62, 13) -> Just 128
    (63, 8) -> Just 138
    (63, 9) -> Just 139
    (63, 10) -> Just 137
    (63, 13) -> Just 128
    (64, 8) -> Just 267
    (64, 40) -> Just 269
    (65, 8) -> Just 267
    (65, 40) -> Just 316
    (65, 53) -> Just 307
    (65, 54) -> Just 314
    (66, 8) -> Just 267
    (66, 40) -> Just 316
    (66, 53) -> Just 313
    (66, 54) -> Just 314
    (67, 8) -> Just 199
    (68, 8) -> Just 210
    (69, 8) -> Just 211
    (70, 8) -> Just 212
    (78, 9) -> Just 293
    (78, 45) -> Just 284
    (78, 46) -> Just 285
    (78, 47) -> Just 286
    (79, 9) -> Just 293
    (79, 17) -> Just 80
    (79, 45) -> Just 190
    (79, 46) -> Just 285
    (79, 47) -> Just 286
    (80, 9) -> Just 293
    (80, 23) -> Just 182
    (80, 45) -> Just 191
    (80, 46) -> Just 285
    (80, 47) -> Just 286
    (81, 9) -> Just 293
    (81, 17) -> Just 82
    (81, 45) -> Just 190
    (81, 46) -> Just 285
    (81, 47) -> Just 286
    (82, 9) -> Just 293
    (82, 24) -> Just 184
    (82, 45) -> Just 191
    (82, 46) -> Just 285
    (82, 47) -> Just 286
    (83, 9) -> Just 293
    (83, 17) -> Just 84
    (83, 45) -> Just 190
    (83, 46) -> Just 285
    (83, 47) -> Just 286
    (84, 9) -> Just 293
    (84, 23) -> Just 181
    (84, 45) -> Just 191
    (84, 46) -> Just 285
    (84, 47) -> Just 286
    (85, 9) -> Just 293
    (85, 17) -> Just 86
    (85, 45) -> Just 190
    (85, 46) -> Just 285
    (85, 47) -> Just 286
    (86, 9) -> Just 293
    (86, 24) -> Just 183
    (86, 45) -> Just 191
    (86, 46) -> Just 285
    (86, 47) -> Just 286
    (87, 9) -> Just 293
    (87, 17) -> Just 88
    (87, 18) -> Just 245
    (87, 45) -> Just 190
    (87, 46) -> Just 285
    (87, 47) -> Just 286
    (88, 9) -> Just 293
    (88, 45) -> Just 191
    (88, 46) -> Just 285
    (88, 47) -> Just 286
    (89, 9) -> Just 293
    (89, 17) -> Just 91
    (89, 18) -> Just 192
    (89, 45) -> Just 190
    (89, 46) -> Just 285
    (89, 47) -> Just 286
    (90, 9) -> Just 293
    (90, 17) -> Just 91
    (90, 18) -> Just 244
    (90, 45) -> Just 190
    (90, 46) -> Just 285
    (90, 47) -> Just 286
    (91, 9) -> Just 293
    (91, 45) -> Just 191
    (91, 46) -> Just 285
    (91, 47) -> Just 286
    (92, 9) -> Just 293
    (92, 17) -> Just 93
    (92, 45) -> Just 190
    (92, 46) -> Just 285
    (92, 47) -> Just 286
    (93, 9) -> Just 293
    (93, 19) -> Just 169
    (93, 45) -> Just 191
    (93, 46) -> Just 285
    (93, 47) -> Just 286
    (94, 9) -> Just 293
    (94, 17) -> Just 95
    (94, 45) -> Just 190
    (94, 46) -> Just 285
    (94, 47) -> Just 286
    (95, 9) -> Just 293
    (95, 19) -> Just 170
    (95, 45) -> Just 191
    (95, 46) -> Just 285
    (95, 47) -> Just 286
    (96, 9) -> Just 294
    (96, 17) -> Just 99
    (96, 45) -> Just 190
    (96, 46) -> Just 285
    (96, 47) -> Just 286
    (96, 50) -> Just 193
    (96, 51) -> Just 304
    (97, 9) -> Just 294
    (97, 17) -> Just 99
    (97, 45) -> Just 190
    (97, 46) -> Just 285
    (97, 47) -> Just 286
    (97, 50) -> Just 303
    (97, 51) -> Just 304
    (98, 9) -> Just 111
    (99, 9) -> Just 293
    (99, 45) -> Just 191
    (99, 46) -> Just 285
    (99, 47) -> Just 286
    (99, 52) -> Just 100
    (100, 9) -> Just 293
    (100, 17) -> Just 101
    (100, 45) -> Just 190
    (100, 46) -> Just 285
    (100, 47) -> Just 286
    (101, 9) -> Just 293
    (101, 45) -> Just 191
    (101, 46) -> Just 285
    (101, 47) -> Just 286
    (102, 9) -> Just 293
    (102, 17) -> Just 103
    (102, 18) -> Just 236
    (102, 45) -> Just 190
    (102, 46) -> Just 285
    (102, 47) -> Just 286
    (103, 9) -> Just 293
    (103, 45) -> Just 191
    (103, 46) -> Just 285
    (103, 47) -> Just 286
    (104, 9) -> Just 293
    (104, 17) -> Just 91
    (104, 18) -> Just 168
    (104, 45) -> Just 190
    (104, 46) -> Just 285
    (104, 47) -> Just 286
    (105, 9) -> Just 293
    (105, 17) -> Just 91
    (105, 18) -> Just 213
    (105, 45) -> Just 190
    (105, 46) -> Just 285
    (105, 47) -> Just 286
    (106, 9) -> Just 293
    (106, 17) -> Just 91
    (106, 18) -> Just 214
    (106, 45) -> Just 190
    (106, 46) -> Just 285
    (106, 47) -> Just 286
    (107, 9) -> Just 293
    (107, 17) -> Just 91
    (107, 18) -> Just 215
    (107, 45) -> Just 190
    (107, 46) -> Just 285
    (107, 47) -> Just 286
    (108, 9) -> Just 293
    (108, 17) -> Just 91
    (108, 18) -> Just 235
    (108, 45) -> Just 190
    (108, 46) -> Just 285
    (108, 47) -> Just 286
    (109, 9) -> Just 293
    (109, 17) -> Just 91
    (109, 18) -> Just 315
    (109, 45) -> Just 190
    (109, 46) -> Just 285
    (109, 47) -> Just 286
    (110, 9) -> Just 293
    (110, 17) -> Just 91
    (110, 18) -> Just 200
    (110, 45) -> Just 190
    (110, 46) -> Just 285
    (110, 47) -> Just 286
    (111, 9) -> Just 293
    (111, 45) -> Just 201
    (111, 46) -> Just 285
    (111, 47) -> Just 286
    (112, 9) -> Just 293
    (112, 17) -> Just 113
    (112, 45) -> Just 190
    (112, 46) -> Just 285
    (112, 47) -> Just 286
    (113, 9) -> Just 293
    (113, 22) -> Just 180
    (113, 45) -> Just 191
    (113, 46) -> Just 285
    (113, 47) -> Just 286
    (114, 9) -> Just 293
    (114, 17) -> Just 116
    (114, 45) -> Just 190
    (114, 46) -> Just 285
    (114, 47) -> Just 286
    (115, 9) -> Just 293
    (115, 17) -> Just 117
    (115, 45) -> Just 190
    (115, 46) -> Just 285
    (115, 47) -> Just 286
    (116, 9) -> Just 293
    (116, 45) -> Just 191
    (116, 46) -> Just 285
    (116, 47) -> Just 286
    (117, 9) -> Just 293
    (117, 22) -> Just 179
    (117, 45) -> Just 191
    (117, 46) -> Just 285
    (117, 47) -> Just 286
    (118, 9) -> Just 293
    (118, 17) -> Just 91
    (118, 18) -> Just 282
    (118, 45) -> Just 190
    (118, 46) -> Just 285
    (118, 47) -> Just 286
    (118, 48) -> Just 287
    (118, 49) -> Just 295
    (119, 9) -> Just 293
    (119, 17) -> Just 91
    (119, 18) -> Just 206
    (119, 25) -> Just 185
    (119, 45) -> Just 190
    (119, 46) -> Just 285
    (119, 47) -> Just 286
    (120, 9) -> Just 293
    (120, 17) -> Just 91
    (120, 18) -> Just 206
    (120, 25) -> Just 207
    (120, 45) -> Just 190
    (120, 46) -> Just 285
    (120, 47) -> Just 286
    (121, 9) -> Just 293
    (121, 17) -> Just 91
    (121, 18) -> Just 299
    (121, 45) -> Just 190
    (121, 46) -> Just 285
    (121, 47) -> Just 286
    (121, 48) -> Just 300
    (122, 9) -> Just 293
    (122, 17) -> Just 91
    (122, 18) -> Just 283
    (122, 45) -> Just 190
    (122, 46) -> Just 285
    (122, 47) -> Just 286
    (140, 20) -> Just 196
    (140, 21) -> Just 175
    (141, 20) -> Just 196
    (141, 21) -> Just 176
    (142, 20) -> Just 196
    (142, 21) -> Just 177
    (143, 20) -> Just 196
    (143, 21) -> Just 178
    (156, 15) -> Just 8
    (158, 20) -> Just 171
    (159, 20) -> Just 172
    (160, 20) -> Just 173
    (161, 20) -> Just 174
    (163, 26) -> Just 186
    (164, 16) -> Just 167
    (194, 20) -> Just 196
    (194, 21) -> Just 197
    (202, 34) -> Just 203
    (204, 37) -> Just 205
    (208, 55) -> Just 216
    (209, 55) -> Just 217
    (216, 56) -> Just 68
    (216, 57) -> Just 218
    (217, 58) -> Just 70
    (218, 56) -> Just 69
    (219, 28) -> Just 221
    (220, 28) -> Just 222
    (226, 28) -> Just 254
    (227, 28) -> Just 255
    (228, 28) -> Just 263
    (229, 28) -> Just 264
    (230, 28) -> Just 327
    (238, 42) -> Just 239
    (239, 43) -> Just 240
    (239, 44) -> Just 276
    (239, 52) -> Just 277
    (239, 66) -> Just 278
    (274, 43) -> Just 275
    (274, 44) -> Just 276
    (274, 52) -> Just 277
    (274, 66) -> Just 278
    (301, 49) -> Just 302
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
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    152 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    153 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    183 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    185 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    189 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    191 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    192 ->
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
  , exp_implies_infixexp_COLON_COLON_type' = \infixexp0 cOLON_COLON1 type'2 ->
      return $ Exp_implies_infixexp_COLON_COLON_type' infixexp0 cOLON_COLON1 type'2
  , exp_implies_infixexp_COLON_COLON_btype_DARROW_type' = \infixexp0 cOLON_COLON1 btype2 dARROW3 type'4 ->
      return $ Exp_implies_infixexp_COLON_COLON_btype_DARROW_type' infixexp0 cOLON_COLON1 btype2 dARROW3 type'4
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

