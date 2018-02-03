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
    (11, Token (MODULE _)) -> Just (Reduce 1 196)
    (11, Token (WHERE _)) -> Just (Reduce 1 196)
    (11, Token (RBRACE _)) -> Just (Reduce 1 196)
    (11, Token (LPAREN _)) -> Just (Reduce 1 196)
    (11, Token (RPAREN _)) -> Just (Reduce 1 196)
    (11, Token (COMMA _)) -> Just (Reduce 1 196)
    (11, Token (SEMICOLON _)) -> Just (Reduce 1 196)
    (11, Token (HIDING _)) -> Just (Reduce 1 196)
    (11, Token (MINUS _)) -> Just (Reduce 1 196)
    (11, Token (QCONID _)) -> Just (Reduce 1 196)
    (11, Token (EXPORT _)) -> Just (Reduce 1 196)
    (11, Token (AS _)) -> Just (Reduce 1 196)
    (11, Token (QVARID _)) -> Just (Reduce 1 196)
    (11, Token (QVARSYM _)) -> Just (Reduce 1 196)
    (11, Token (QCONSYM _)) -> Just (Reduce 1 196)
    (12, Token (WHERE _)) -> Just (Reduce 1 4)
    (13, Token (RBRACE _)) -> Just (Reduce 0 85)
    (13, Token (LPAREN _)) -> Just (Shift 46)
    (13, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (13, Token (IMPORT _)) -> Just (Shift 162)
    (13, Token (TYPE _)) -> Just (Shift 120)
    (13, Token (DATA _)) -> Just (Shift 98)
    (13, Token (NEWTYPE _)) -> Just (Shift 118)
    (13, Token (CLASS _)) -> Just (Shift 85)
    (13, Token (INSTANCE _)) -> Just (Shift 87)
    (13, Token (DEFAULT _)) -> Just (Shift 168)
    (13, Token (FOREIGN _)) -> Just (Shift 169)
    (13, Token (INFIXL _)) -> Just (Shift 277)
    (13, Token (INFIXR _)) -> Just (Shift 278)
    (13, Token (INFIX _)) -> Just (Shift 279)
    (13, Token (EXPORT _)) -> Just (Shift 80)
    (13, Token (AS _)) -> Just (Shift 81)
    (13, Token (QVARID _)) -> Just (Shift 82)
    (14, EOF) -> Just (Reduce 3 2)
    (15, Token (RBRACE _)) -> Just (Shift 14)
    (16, Token (RBRACE _)) -> Just (Reduce 0 85)
    (16, Token (LPAREN _)) -> Just (Shift 46)
    (16, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (16, Token (IMPORT _)) -> Just (Shift 162)
    (16, Token (TYPE _)) -> Just (Shift 120)
    (16, Token (DATA _)) -> Just (Shift 98)
    (16, Token (NEWTYPE _)) -> Just (Shift 118)
    (16, Token (CLASS _)) -> Just (Shift 85)
    (16, Token (INSTANCE _)) -> Just (Shift 87)
    (16, Token (DEFAULT _)) -> Just (Shift 168)
    (16, Token (FOREIGN _)) -> Just (Shift 169)
    (16, Token (INFIXL _)) -> Just (Shift 277)
    (16, Token (INFIXR _)) -> Just (Shift 278)
    (16, Token (INFIX _)) -> Just (Shift 279)
    (16, Token (EXPORT _)) -> Just (Shift 80)
    (16, Token (AS _)) -> Just (Shift 81)
    (16, Token (QVARID _)) -> Just (Shift 82)
    (17, Token (RBRACE _)) -> Just (Reduce 3 28)
    (18, Token (RBRACE _)) -> Just (Reduce 1 27)
    (18, Token (SEMICOLON _)) -> Just (Shift 16)
    (19, Token (MODULE _)) -> Just (Shift 10)
    (19, Token (LPAREN _)) -> Just (Shift 75)
    (19, Token (RPAREN _)) -> Just (Reduce 0 6)
    (19, Token (QCONID _)) -> Just (Shift 131)
    (19, Token (EXPORT _)) -> Just (Shift 80)
    (19, Token (AS _)) -> Just (Shift 81)
    (19, Token (QVARID _)) -> Just (Shift 82)
    (20, Token (WHERE _)) -> Just (Reduce 3 5)
    (21, Token (RPAREN _)) -> Just (Shift 20)
    (22, Token (MODULE _)) -> Just (Shift 10)
    (22, Token (LPAREN _)) -> Just (Shift 75)
    (22, Token (RPAREN _)) -> Just (Reduce 0 6)
    (22, Token (QCONID _)) -> Just (Shift 131)
    (22, Token (EXPORT _)) -> Just (Shift 80)
    (22, Token (AS _)) -> Just (Shift 81)
    (22, Token (QVARID _)) -> Just (Shift 82)
    (23, Token (RPAREN _)) -> Just (Reduce 3 8)
    (24, Token (RPAREN _)) -> Just (Reduce 1 7)
    (24, Token (COMMA _)) -> Just (Shift 22)
    (25, Token (LPAREN _)) -> Just (Shift 75)
    (25, Token (RPAREN _)) -> Just (Shift 26)
    (25, Token (DOT_DOT _)) -> Just (Shift 29)
    (25, Token (QCONID _)) -> Just (Shift 131)
    (25, Token (EXPORT _)) -> Just (Shift 80)
    (25, Token (AS _)) -> Just (Shift 81)
    (25, Token (QVARID _)) -> Just (Shift 82)
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
    (34, Token (WHERE _)) -> Just (Reduce 1 158)
    (34, Token (RBRACE _)) -> Just (Reduce 1 158)
    (34, Token (LPAREN _)) -> Just (Shift 44)
    (34, Token (RPAREN _)) -> Just (Reduce 1 158)
    (34, Token (COMMA _)) -> Just (Reduce 1 158)
    (34, Token (SEMICOLON _)) -> Just (Reduce 1 158)
    (34, Token (EQUAL _)) -> Just (Reduce 1 158)
    (34, Token (PIPE _)) -> Just (Reduce 1 158)
    (34, Token (COLON_COLON _)) -> Just (Reduce 1 158)
    (34, Token (MINUS _)) -> Just (Shift 36)
    (34, Token (EXPORT _)) -> Just (Shift 80)
    (34, Token (AS _)) -> Just (Shift 81)
    (34, Token (QVARID _)) -> Just (Shift 82)
    (34, Token (STRING _)) -> Just (Shift 347)
    (34, Token (LARROW _)) -> Just (Reduce 1 158)
    (34, Token (INTEGER _)) -> Just (Shift 348)
    (34, Token (QVARSYM _)) -> Just (Shift 356)
    (34, Token (QCONSYM _)) -> Just (Shift 319)
    (34, Token (BACKQUOTE _)) -> Just (Shift 320)
    (35, Token (LPAREN _)) -> Just (Shift 44)
    (35, Token (MINUS _)) -> Just (Shift 35)
    (35, Token (EXPORT _)) -> Just (Shift 80)
    (35, Token (AS _)) -> Just (Shift 81)
    (35, Token (QVARID _)) -> Just (Shift 82)
    (35, Token (STRING _)) -> Just (Shift 347)
    (35, Token (INTEGER _)) -> Just (Shift 348)
    (36, Token (LPAREN _)) -> Just (Shift 44)
    (36, Token (EXPORT _)) -> Just (Shift 80)
    (36, Token (AS _)) -> Just (Shift 81)
    (36, Token (QVARID _)) -> Just (Shift 82)
    (36, Token (STRING _)) -> Just (Shift 347)
    (36, Token (INTEGER _)) -> Just (Shift 348)
    (37, Token (LPAREN _)) -> Just (Shift 44)
    (37, Token (EXPORT _)) -> Just (Shift 80)
    (37, Token (AS _)) -> Just (Shift 81)
    (37, Token (QVARID _)) -> Just (Shift 82)
    (37, Token (STRING _)) -> Just (Shift 347)
    (37, Token (INTEGER _)) -> Just (Shift 348)
    (38, Token (LPAREN _)) -> Just (Shift 44)
    (38, Token (MINUS _)) -> Just (Shift 35)
    (38, Token (EXPORT _)) -> Just (Shift 80)
    (38, Token (AS _)) -> Just (Shift 81)
    (38, Token (QVARID _)) -> Just (Shift 82)
    (38, Token (STRING _)) -> Just (Shift 347)
    (38, Token (INTEGER _)) -> Just (Shift 348)
    (39, Token (LPAREN _)) -> Just (Shift 44)
    (39, Token (MINUS _)) -> Just (Shift 35)
    (39, Token (EXPORT _)) -> Just (Shift 80)
    (39, Token (AS _)) -> Just (Shift 81)
    (39, Token (QVARID _)) -> Just (Shift 82)
    (39, Token (STRING _)) -> Just (Shift 347)
    (39, Token (INTEGER _)) -> Just (Shift 348)
    (40, Token (LPAREN _)) -> Just (Shift 44)
    (40, Token (MINUS _)) -> Just (Shift 35)
    (40, Token (EXPORT _)) -> Just (Shift 80)
    (40, Token (AS _)) -> Just (Shift 81)
    (40, Token (QVARID _)) -> Just (Shift 82)
    (40, Token (STRING _)) -> Just (Shift 347)
    (40, Token (INTEGER _)) -> Just (Shift 348)
    (41, Token (LPAREN _)) -> Just (Shift 44)
    (41, Token (MINUS _)) -> Just (Shift 35)
    (41, Token (EXPORT _)) -> Just (Shift 80)
    (41, Token (AS _)) -> Just (Shift 81)
    (41, Token (QVARID _)) -> Just (Shift 82)
    (41, Token (STRING _)) -> Just (Shift 347)
    (41, Token (INTEGER _)) -> Just (Shift 348)
    (42, Token (LPAREN _)) -> Just (Shift 46)
    (42, Token (EXPORT _)) -> Just (Shift 80)
    (42, Token (AS _)) -> Just (Shift 81)
    (42, Token (QVARID _)) -> Just (Shift 82)
    (43, Token (LPAREN _)) -> Just (Shift 46)
    (43, Token (EXPORT _)) -> Just (Shift 80)
    (43, Token (AS _)) -> Just (Shift 81)
    (43, Token (QVARID _)) -> Just (Shift 82)
    (44, Token (LPAREN _)) -> Just (Shift 44)
    (44, Token (MINUS _)) -> Just (Shift 45)
    (44, Token (EXPORT _)) -> Just (Shift 80)
    (44, Token (AS _)) -> Just (Shift 81)
    (44, Token (QVARID _)) -> Just (Shift 82)
    (44, Token (STRING _)) -> Just (Shift 347)
    (44, Token (INTEGER _)) -> Just (Shift 348)
    (44, Token (QVARSYM _)) -> Just (Shift 83)
    (45, Token (LPAREN _)) -> Just (Shift 44)
    (45, Token (RPAREN _)) -> Just (Shift 77)
    (45, Token (MINUS _)) -> Just (Shift 35)
    (45, Token (EXPORT _)) -> Just (Shift 80)
    (45, Token (AS _)) -> Just (Shift 81)
    (45, Token (QVARID _)) -> Just (Shift 82)
    (45, Token (STRING _)) -> Just (Shift 347)
    (45, Token (INTEGER _)) -> Just (Shift 348)
    (46, Token (LPAREN _)) -> Just (Shift 46)
    (46, Token (MINUS _)) -> Just (Shift 79)
    (46, Token (EXPORT _)) -> Just (Shift 80)
    (46, Token (AS _)) -> Just (Shift 81)
    (46, Token (QVARID _)) -> Just (Shift 82)
    (46, Token (QVARSYM _)) -> Just (Shift 83)
    (47, Token (LPAREN _)) -> Just (Shift 46)
    (47, Token (RPAREN _)) -> Just (Shift 351)
    (47, Token (MINUS _)) -> Just (Shift 42)
    (47, Token (EXPORT _)) -> Just (Shift 80)
    (47, Token (AS _)) -> Just (Shift 81)
    (47, Token (QVARID _)) -> Just (Shift 82)
    (47, Token (QVARSYM _)) -> Just (Shift 356)
    (47, Token (QCONSYM _)) -> Just (Shift 319)
    (47, Token (BACKQUOTE _)) -> Just (Shift 320)
    (48, Token (RBRACE _)) -> Just (Reduce 0 85)
    (48, Token (LPAREN _)) -> Just (Shift 46)
    (48, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (48, Token (INFIXL _)) -> Just (Shift 277)
    (48, Token (INFIXR _)) -> Just (Shift 278)
    (48, Token (INFIX _)) -> Just (Shift 279)
    (48, Token (EXPORT _)) -> Just (Shift 80)
    (48, Token (AS _)) -> Just (Shift 81)
    (48, Token (QVARID _)) -> Just (Shift 82)
    (49, Token (RBRACE _)) -> Just (Reduce 0 85)
    (49, Token (LPAREN _)) -> Just (Shift 46)
    (49, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (49, Token (INFIXL _)) -> Just (Shift 277)
    (49, Token (INFIXR _)) -> Just (Shift 278)
    (49, Token (INFIX _)) -> Just (Shift 279)
    (49, Token (EXPORT _)) -> Just (Shift 80)
    (49, Token (AS _)) -> Just (Shift 81)
    (49, Token (QVARID _)) -> Just (Shift 82)
    (50, Token (RBRACE _)) -> Just (Reduce 0 85)
    (50, Token (LPAREN _)) -> Just (Shift 46)
    (50, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (50, Token (INFIXL _)) -> Just (Shift 277)
    (50, Token (INFIXR _)) -> Just (Shift 278)
    (50, Token (INFIX _)) -> Just (Shift 279)
    (50, Token (EXPORT _)) -> Just (Shift 80)
    (50, Token (AS _)) -> Just (Shift 81)
    (50, Token (QVARID _)) -> Just (Shift 82)
    (51, Token (RBRACE _)) -> Just (Reduce 0 85)
    (51, Token (LPAREN _)) -> Just (Shift 46)
    (51, Token (SEMICOLON _)) -> Just (Reduce 0 85)
    (51, Token (INFIXL _)) -> Just (Shift 277)
    (51, Token (INFIXR _)) -> Just (Shift 278)
    (51, Token (INFIX _)) -> Just (Shift 279)
    (51, Token (EXPORT _)) -> Just (Shift 80)
    (51, Token (AS _)) -> Just (Shift 81)
    (51, Token (QVARID _)) -> Just (Shift 82)
    (52, Token (LPAREN _)) -> Just (Shift 44)
    (52, Token (MINUS _)) -> Just (Shift 35)
    (52, Token (EXPORT _)) -> Just (Shift 80)
    (52, Token (AS _)) -> Just (Shift 81)
    (52, Token (QVARID _)) -> Just (Shift 82)
    (52, Token (STRING _)) -> Just (Shift 347)
    (52, Token (LET _)) -> Just (Shift 236)
    (52, Token (INTEGER _)) -> Just (Shift 348)
    (53, Token (LPAREN _)) -> Just (Shift 44)
    (53, Token (MINUS _)) -> Just (Shift 35)
    (53, Token (EXPORT _)) -> Just (Shift 80)
    (53, Token (AS _)) -> Just (Shift 81)
    (53, Token (QVARID _)) -> Just (Shift 82)
    (53, Token (STRING _)) -> Just (Shift 347)
    (53, Token (LET _)) -> Just (Shift 236)
    (53, Token (INTEGER _)) -> Just (Shift 348)
    (54, Token (LPAREN _)) -> Just (Shift 44)
    (54, Token (MINUS _)) -> Just (Shift 35)
    (54, Token (EXPORT _)) -> Just (Shift 80)
    (54, Token (AS _)) -> Just (Shift 81)
    (54, Token (QVARID _)) -> Just (Shift 82)
    (54, Token (STRING _)) -> Just (Shift 347)
    (54, Token (LET _)) -> Just (Shift 236)
    (54, Token (INTEGER _)) -> Just (Shift 348)
    (55, Token (LPAREN _)) -> Just (Shift 44)
    (55, Token (MINUS _)) -> Just (Shift 35)
    (55, Token (EXPORT _)) -> Just (Shift 80)
    (55, Token (AS _)) -> Just (Shift 81)
    (55, Token (QVARID _)) -> Just (Shift 82)
    (55, Token (STRING _)) -> Just (Shift 347)
    (55, Token (LET _)) -> Just (Shift 236)
    (55, Token (INTEGER _)) -> Just (Shift 348)
    (56, Token (LPAREN _)) -> Just (Shift 44)
    (56, Token (MINUS _)) -> Just (Shift 35)
    (56, Token (EXPORT _)) -> Just (Shift 80)
    (56, Token (AS _)) -> Just (Shift 81)
    (56, Token (QVARID _)) -> Just (Shift 82)
    (56, Token (STRING _)) -> Just (Shift 347)
    (56, Token (LET _)) -> Just (Shift 236)
    (56, Token (INTEGER _)) -> Just (Shift 348)
    (57, Token (LPAREN _)) -> Just (Shift 44)
    (57, Token (MINUS _)) -> Just (Shift 35)
    (57, Token (EXPORT _)) -> Just (Shift 80)
    (57, Token (AS _)) -> Just (Shift 81)
    (57, Token (QVARID _)) -> Just (Shift 82)
    (57, Token (STRING _)) -> Just (Shift 347)
    (57, Token (INTEGER _)) -> Just (Shift 348)
    (58, Token (LPAREN _)) -> Just (Shift 46)
    (58, Token (EQUAL _)) -> Just (Shift 39)
    (58, Token (PIPE _)) -> Just (Shift 52)
    (58, Token (MINUS _)) -> Just (Shift 42)
    (58, Token (EXPORT _)) -> Just (Shift 80)
    (58, Token (AS _)) -> Just (Shift 81)
    (58, Token (QVARID _)) -> Just (Shift 82)
    (58, Token (QVARSYM _)) -> Just (Shift 356)
    (58, Token (QCONSYM _)) -> Just (Shift 319)
    (58, Token (BACKQUOTE _)) -> Just (Shift 320)
    (59, Token (RBRACE _)) -> Just (Reduce 0 80)
    (59, Token (LPAREN _)) -> Just (Shift 46)
    (59, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (59, Token (EXPORT _)) -> Just (Shift 80)
    (59, Token (AS _)) -> Just (Shift 81)
    (59, Token (QVARID _)) -> Just (Shift 82)
    (60, Token (RBRACE _)) -> Just (Reduce 0 80)
    (60, Token (LPAREN _)) -> Just (Shift 46)
    (60, Token (SEMICOLON _)) -> Just (Reduce 0 80)
    (60, Token (EXPORT _)) -> Just (Shift 80)
    (60, Token (AS _)) -> Just (Shift 81)
    (60, Token (QVARID _)) -> Just (Shift 82)
    (61, Token (LPAREN _)) -> Just (Shift 46)
    (61, Token (EQUAL _)) -> Just (Shift 40)
    (61, Token (PIPE _)) -> Just (Shift 54)
    (61, Token (MINUS _)) -> Just (Shift 42)
    (61, Token (EXPORT _)) -> Just (Shift 80)
    (61, Token (AS _)) -> Just (Shift 81)
    (61, Token (QVARID _)) -> Just (Shift 82)
    (61, Token (QVARSYM _)) -> Just (Shift 356)
    (61, Token (QCONSYM _)) -> Just (Shift 319)
    (61, Token (BACKQUOTE _)) -> Just (Shift 320)
    (62, Token (LPAREN _)) -> Just (Shift 46)
    (62, Token (EQUAL _)) -> Just (Shift 41)
    (62, Token (PIPE _)) -> Just (Shift 55)
    (62, Token (MINUS _)) -> Just (Shift 42)
    (62, Token (EXPORT _)) -> Just (Shift 80)
    (62, Token (AS _)) -> Just (Shift 81)
    (62, Token (QVARID _)) -> Just (Shift 82)
    (62, Token (QVARSYM _)) -> Just (Shift 356)
    (62, Token (QCONSYM _)) -> Just (Shift 319)
    (62, Token (BACKQUOTE _)) -> Just (Shift 320)
    (63, Token (LPAREN _)) -> Just (Shift 75)
    (63, Token (RPAREN _)) -> Just (Reduce 0 15)
    (63, Token (QCONID _)) -> Just (Shift 131)
    (63, Token (EXPORT _)) -> Just (Shift 80)
    (63, Token (AS _)) -> Just (Shift 81)
    (63, Token (QVARID _)) -> Just (Shift 82)
    (64, Token (LPAREN _)) -> Just (Shift 75)
    (64, Token (RPAREN _)) -> Just (Reduce 0 15)
    (64, Token (QCONID _)) -> Just (Shift 131)
    (64, Token (EXPORT _)) -> Just (Shift 80)
    (64, Token (AS _)) -> Just (Shift 81)
    (64, Token (QVARID _)) -> Just (Shift 82)
    (65, Token (LPAREN _)) -> Just (Shift 75)
    (65, Token (RPAREN _)) -> Just (Reduce 0 15)
    (65, Token (QCONID _)) -> Just (Shift 131)
    (65, Token (EXPORT _)) -> Just (Shift 80)
    (65, Token (AS _)) -> Just (Shift 81)
    (65, Token (QVARID _)) -> Just (Shift 82)
    (66, Token (LPAREN _)) -> Just (Shift 75)
    (66, Token (QCONID _)) -> Just (Shift 131)
    (66, Token (EXPORT _)) -> Just (Shift 80)
    (66, Token (AS _)) -> Just (Shift 81)
    (66, Token (QVARID _)) -> Just (Shift 82)
    (67, Token (LPAREN _)) -> Just (Shift 75)
    (67, Token (RPAREN _)) -> Just (Shift 137)
    (67, Token (DOT_DOT _)) -> Just (Shift 140)
    (67, Token (QCONID _)) -> Just (Shift 131)
    (67, Token (EXPORT _)) -> Just (Shift 80)
    (67, Token (AS _)) -> Just (Shift 81)
    (67, Token (QVARID _)) -> Just (Shift 82)
    (68, Token (LPAREN _)) -> Just (Shift 76)
    (68, Token (EXPORT _)) -> Just (Shift 80)
    (68, Token (AS _)) -> Just (Shift 81)
    (68, Token (QVARID _)) -> Just (Shift 82)
    (69, Token (RBRACE _)) -> Just (Shift 315)
    (69, Token (LPAREN _)) -> Just (Shift 76)
    (69, Token (EXPORT _)) -> Just (Shift 80)
    (69, Token (AS _)) -> Just (Shift 81)
    (69, Token (QVARID _)) -> Just (Shift 82)
    (70, Token (LPAREN _)) -> Just (Shift 76)
    (70, Token (EXPORT _)) -> Just (Shift 80)
    (70, Token (AS _)) -> Just (Shift 81)
    (70, Token (QVARID _)) -> Just (Shift 82)
    (71, Token (LPAREN _)) -> Just (Shift 76)
    (71, Token (EXPORT _)) -> Just (Shift 80)
    (71, Token (AS _)) -> Just (Shift 81)
    (71, Token (QVARID _)) -> Just (Shift 82)
    (72, Token (LPAREN _)) -> Just (Shift 76)
    (72, Token (EXPORT _)) -> Just (Shift 80)
    (72, Token (AS _)) -> Just (Shift 81)
    (72, Token (QVARID _)) -> Just (Shift 82)
    (73, Token (LPAREN _)) -> Just (Shift 76)
    (73, Token (EXPORT _)) -> Just (Shift 80)
    (73, Token (AS _)) -> Just (Shift 81)
    (73, Token (QVARID _)) -> Just (Shift 82)
    (74, Token (LPAREN _)) -> Just (Shift 76)
    (74, Token (EXPORT _)) -> Just (Shift 80)
    (74, Token (AS _)) -> Just (Shift 81)
    (74, Token (QVARID _)) -> Just (Shift 82)
    (75, Token (MINUS _)) -> Just (Shift 79)
    (75, Token (QVARSYM _)) -> Just (Shift 83)
    (75, Token (QCONSYM _)) -> Just (Shift 132)
    (76, Token (MINUS _)) -> Just (Shift 79)
    (76, Token (QVARSYM _)) -> Just (Shift 83)
    (77, Token (WHERE _)) -> Just (Reduce 3 176)
    (77, Token (RBRACE _)) -> Just (Reduce 3 176)
    (77, Token (LPAREN _)) -> Just (Reduce 3 176)
    (77, Token (RPAREN _)) -> Just (Reduce 3 176)
    (77, Token (COMMA _)) -> Just (Reduce 3 176)
    (77, Token (SEMICOLON _)) -> Just (Reduce 3 176)
    (77, Token (EQUAL _)) -> Just (Reduce 3 176)
    (77, Token (PIPE _)) -> Just (Reduce 3 176)
    (77, Token (COLON_COLON _)) -> Just (Reduce 3 176)
    (77, Token (MINUS _)) -> Just (Reduce 3 176)
    (77, Token (QCONID _)) -> Just (Reduce 3 176)
    (77, Token (EXPORT _)) -> Just (Reduce 3 176)
    (77, Token (AS _)) -> Just (Reduce 3 176)
    (77, Token (QVARID _)) -> Just (Reduce 3 176)
    (77, Token (STRING _)) -> Just (Reduce 3 176)
    (77, Token (LARROW _)) -> Just (Reduce 3 176)
    (77, Token (INTEGER _)) -> Just (Reduce 3 176)
    (77, Token (QVARSYM _)) -> Just (Reduce 3 176)
    (77, Token (QCONSYM _)) -> Just (Reduce 3 176)
    (77, Token (BACKQUOTE _)) -> Just (Reduce 3 176)
    (78, Token (WHERE _)) -> Just (Reduce 3 177)
    (78, Token (RBRACE _)) -> Just (Reduce 3 177)
    (78, Token (LPAREN _)) -> Just (Reduce 3 177)
    (78, Token (RPAREN _)) -> Just (Reduce 3 177)
    (78, Token (COMMA _)) -> Just (Reduce 3 177)
    (78, Token (SEMICOLON _)) -> Just (Reduce 3 177)
    (78, Token (EQUAL _)) -> Just (Reduce 3 177)
    (78, Token (PIPE _)) -> Just (Reduce 3 177)
    (78, Token (COLON_COLON _)) -> Just (Reduce 3 177)
    (78, Token (MINUS _)) -> Just (Reduce 3 177)
    (78, Token (QCONID _)) -> Just (Reduce 3 177)
    (78, Token (EXPORT _)) -> Just (Reduce 3 177)
    (78, Token (AS _)) -> Just (Reduce 3 177)
    (78, Token (QVARID _)) -> Just (Reduce 3 177)
    (78, Token (STRING _)) -> Just (Reduce 3 177)
    (78, Token (LARROW _)) -> Just (Reduce 3 177)
    (78, Token (INTEGER _)) -> Just (Reduce 3 177)
    (78, Token (QVARSYM _)) -> Just (Reduce 3 177)
    (78, Token (QCONSYM _)) -> Just (Reduce 3 177)
    (78, Token (BACKQUOTE _)) -> Just (Reduce 3 177)
    (79, Token (RPAREN _)) -> Just (Shift 77)
    (80, Token (WHERE _)) -> Just (Reduce 1 174)
    (80, Token (RBRACE _)) -> Just (Reduce 1 174)
    (80, Token (LPAREN _)) -> Just (Reduce 1 174)
    (80, Token (RPAREN _)) -> Just (Reduce 1 174)
    (80, Token (COMMA _)) -> Just (Reduce 1 174)
    (80, Token (SEMICOLON _)) -> Just (Reduce 1 174)
    (80, Token (EQUAL _)) -> Just (Reduce 1 174)
    (80, Token (PIPE _)) -> Just (Reduce 1 174)
    (80, Token (COLON_COLON _)) -> Just (Reduce 1 174)
    (80, Token (MINUS _)) -> Just (Reduce 1 174)
    (80, Token (QCONID _)) -> Just (Reduce 1 174)
    (80, Token (EXPORT _)) -> Just (Reduce 1 174)
    (80, Token (AS _)) -> Just (Reduce 1 174)
    (80, Token (QVARID _)) -> Just (Reduce 1 174)
    (80, Token (STRING _)) -> Just (Reduce 1 174)
    (80, Token (LARROW _)) -> Just (Reduce 1 174)
    (80, Token (INTEGER _)) -> Just (Reduce 1 174)
    (80, Token (QVARSYM _)) -> Just (Reduce 1 174)
    (80, Token (QCONSYM _)) -> Just (Reduce 1 174)
    (80, Token (BACKQUOTE _)) -> Just (Reduce 1 174)
    (81, Token (WHERE _)) -> Just (Reduce 1 173)
    (81, Token (RBRACE _)) -> Just (Reduce 1 173)
    (81, Token (LPAREN _)) -> Just (Reduce 1 173)
    (81, Token (RPAREN _)) -> Just (Reduce 1 173)
    (81, Token (COMMA _)) -> Just (Reduce 1 173)
    (81, Token (SEMICOLON _)) -> Just (Reduce 1 173)
    (81, Token (EQUAL _)) -> Just (Reduce 1 173)
    (81, Token (PIPE _)) -> Just (Reduce 1 173)
    (81, Token (COLON_COLON _)) -> Just (Reduce 1 173)
    (81, Token (MINUS _)) -> Just (Reduce 1 173)
    (81, Token (QCONID _)) -> Just (Reduce 1 173)
    (81, Token (EXPORT _)) -> Just (Reduce 1 173)
    (81, Token (AS _)) -> Just (Reduce 1 173)
    (81, Token (QVARID _)) -> Just (Reduce 1 173)
    (81, Token (STRING _)) -> Just (Reduce 1 173)
    (81, Token (LARROW _)) -> Just (Reduce 1 173)
    (81, Token (INTEGER _)) -> Just (Reduce 1 173)
    (81, Token (QVARSYM _)) -> Just (Reduce 1 173)
    (81, Token (QCONSYM _)) -> Just (Reduce 1 173)
    (81, Token (BACKQUOTE _)) -> Just (Reduce 1 173)
    (82, Token (WHERE _)) -> Just (Reduce 1 175)
    (82, Token (RBRACE _)) -> Just (Reduce 1 175)
    (82, Token (LPAREN _)) -> Just (Reduce 1 175)
    (82, Token (RPAREN _)) -> Just (Reduce 1 175)
    (82, Token (COMMA _)) -> Just (Reduce 1 175)
    (82, Token (SEMICOLON _)) -> Just (Reduce 1 175)
    (82, Token (EQUAL _)) -> Just (Reduce 1 175)
    (82, Token (PIPE _)) -> Just (Reduce 1 175)
    (82, Token (COLON_COLON _)) -> Just (Reduce 1 175)
    (82, Token (MINUS _)) -> Just (Reduce 1 175)
    (82, Token (QCONID _)) -> Just (Reduce 1 175)
    (82, Token (EXPORT _)) -> Just (Reduce 1 175)
    (82, Token (AS _)) -> Just (Reduce 1 175)
    (82, Token (QVARID _)) -> Just (Reduce 1 175)
    (82, Token (STRING _)) -> Just (Reduce 1 175)
    (82, Token (LARROW _)) -> Just (Reduce 1 175)
    (82, Token (INTEGER _)) -> Just (Reduce 1 175)
    (82, Token (QVARSYM _)) -> Just (Reduce 1 175)
    (82, Token (QCONSYM _)) -> Just (Reduce 1 175)
    (82, Token (BACKQUOTE _)) -> Just (Reduce 1 175)
    (83, Token (RPAREN _)) -> Just (Shift 78)
    (84, Token (LPAREN _)) -> Just (Shift 124)
    (84, Token (LBRACKET _)) -> Just (Shift 128)
    (84, Token (EXCL _)) -> Just (Shift 84)
    (84, Token (QCONID _)) -> Just (Shift 131)
    (84, Token (EXPORT _)) -> Just (Shift 306)
    (84, Token (AS _)) -> Just (Shift 307)
    (84, Token (QVARID _)) -> Just (Shift 308)
    (85, Token (LPAREN _)) -> Just (Shift 124)
    (85, Token (LBRACKET _)) -> Just (Shift 128)
    (85, Token (EXCL _)) -> Just (Shift 84)
    (85, Token (QCONID _)) -> Just (Shift 131)
    (85, Token (EXPORT _)) -> Just (Shift 306)
    (85, Token (AS _)) -> Just (Shift 307)
    (85, Token (QVARID _)) -> Just (Shift 308)
    (86, Token (WHERE _)) -> Just (Shift 208)
    (86, Token (RBRACE _)) -> Just (Reduce 0 65)
    (86, Token (LPAREN _)) -> Just (Shift 124)
    (86, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (86, Token (DARROW _)) -> Just (Shift 89)
    (86, Token (LBRACKET _)) -> Just (Shift 128)
    (86, Token (EXCL _)) -> Just (Shift 84)
    (86, Token (QCONID _)) -> Just (Shift 131)
    (86, Token (EXPORT _)) -> Just (Shift 306)
    (86, Token (AS _)) -> Just (Shift 307)
    (86, Token (QVARID _)) -> Just (Shift 308)
    (87, Token (LPAREN _)) -> Just (Shift 124)
    (87, Token (LBRACKET _)) -> Just (Shift 128)
    (87, Token (EXCL _)) -> Just (Shift 84)
    (87, Token (QCONID _)) -> Just (Shift 131)
    (87, Token (EXPORT _)) -> Just (Shift 306)
    (87, Token (AS _)) -> Just (Shift 307)
    (87, Token (QVARID _)) -> Just (Shift 308)
    (88, Token (WHERE _)) -> Just (Shift 210)
    (88, Token (RBRACE _)) -> Just (Reduce 0 75)
    (88, Token (LPAREN _)) -> Just (Shift 124)
    (88, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (88, Token (DARROW _)) -> Just (Shift 91)
    (88, Token (LBRACKET _)) -> Just (Shift 128)
    (88, Token (EXCL _)) -> Just (Shift 84)
    (88, Token (QCONID _)) -> Just (Shift 131)
    (88, Token (EXPORT _)) -> Just (Shift 306)
    (88, Token (AS _)) -> Just (Shift 307)
    (88, Token (QVARID _)) -> Just (Shift 308)
    (89, Token (LPAREN _)) -> Just (Shift 124)
    (89, Token (LBRACKET _)) -> Just (Shift 128)
    (89, Token (EXCL _)) -> Just (Shift 84)
    (89, Token (QCONID _)) -> Just (Shift 131)
    (89, Token (EXPORT _)) -> Just (Shift 306)
    (89, Token (AS _)) -> Just (Shift 307)
    (89, Token (QVARID _)) -> Just (Shift 308)
    (90, Token (WHERE _)) -> Just (Shift 208)
    (90, Token (RBRACE _)) -> Just (Reduce 0 65)
    (90, Token (LPAREN _)) -> Just (Shift 124)
    (90, Token (SEMICOLON _)) -> Just (Reduce 0 65)
    (90, Token (LBRACKET _)) -> Just (Shift 128)
    (90, Token (EXCL _)) -> Just (Shift 84)
    (90, Token (QCONID _)) -> Just (Shift 131)
    (90, Token (EXPORT _)) -> Just (Shift 306)
    (90, Token (AS _)) -> Just (Shift 307)
    (90, Token (QVARID _)) -> Just (Shift 308)
    (91, Token (LPAREN _)) -> Just (Shift 124)
    (91, Token (LBRACKET _)) -> Just (Shift 128)
    (91, Token (EXCL _)) -> Just (Shift 84)
    (91, Token (QCONID _)) -> Just (Shift 131)
    (91, Token (EXPORT _)) -> Just (Shift 306)
    (91, Token (AS _)) -> Just (Shift 307)
    (91, Token (QVARID _)) -> Just (Shift 308)
    (92, Token (WHERE _)) -> Just (Shift 210)
    (92, Token (RBRACE _)) -> Just (Reduce 0 75)
    (92, Token (LPAREN _)) -> Just (Shift 124)
    (92, Token (SEMICOLON _)) -> Just (Reduce 0 75)
    (92, Token (LBRACKET _)) -> Just (Shift 128)
    (92, Token (EXCL _)) -> Just (Shift 84)
    (92, Token (QCONID _)) -> Just (Shift 131)
    (92, Token (EXPORT _)) -> Just (Shift 306)
    (92, Token (AS _)) -> Just (Shift 307)
    (92, Token (QVARID _)) -> Just (Shift 308)
    (93, Token (LPAREN _)) -> Just (Shift 124)
    (93, Token (LBRACKET _)) -> Just (Shift 128)
    (93, Token (EXCL _)) -> Just (Shift 84)
    (93, Token (QCONID _)) -> Just (Shift 131)
    (93, Token (EXPORT _)) -> Just (Shift 306)
    (93, Token (AS _)) -> Just (Shift 307)
    (93, Token (QVARID _)) -> Just (Shift 308)
    (94, Token (WHERE _)) -> Just (Reduce 1 100)
    (94, Token (RBRACE _)) -> Just (Reduce 1 100)
    (94, Token (LPAREN _)) -> Just (Shift 124)
    (94, Token (RPAREN _)) -> Just (Reduce 1 100)
    (94, Token (COMMA _)) -> Just (Reduce 1 100)
    (94, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (94, Token (EQUAL _)) -> Just (Reduce 1 100)
    (94, Token (DARROW _)) -> Just (Shift 96)
    (94, Token (PIPE _)) -> Just (Reduce 1 100)
    (94, Token (RARROW _)) -> Just (Shift 95)
    (94, Token (LBRACKET _)) -> Just (Shift 128)
    (94, Token (EXCL _)) -> Just (Shift 84)
    (94, Token (QCONID _)) -> Just (Shift 131)
    (94, Token (EXPORT _)) -> Just (Shift 306)
    (94, Token (AS _)) -> Just (Shift 307)
    (94, Token (QVARID _)) -> Just (Shift 308)
    (95, Token (LPAREN _)) -> Just (Shift 124)
    (95, Token (LBRACKET _)) -> Just (Shift 128)
    (95, Token (EXCL _)) -> Just (Shift 84)
    (95, Token (QCONID _)) -> Just (Shift 131)
    (95, Token (EXPORT _)) -> Just (Shift 306)
    (95, Token (AS _)) -> Just (Shift 307)
    (95, Token (QVARID _)) -> Just (Shift 308)
    (96, Token (LPAREN _)) -> Just (Shift 124)
    (96, Token (LBRACKET _)) -> Just (Shift 128)
    (96, Token (EXCL _)) -> Just (Shift 84)
    (96, Token (QCONID _)) -> Just (Shift 131)
    (96, Token (EXPORT _)) -> Just (Shift 306)
    (96, Token (AS _)) -> Just (Shift 307)
    (96, Token (QVARID _)) -> Just (Shift 308)
    (97, Token (WHERE _)) -> Just (Reduce 1 100)
    (97, Token (RBRACE _)) -> Just (Reduce 1 100)
    (97, Token (LPAREN _)) -> Just (Shift 124)
    (97, Token (RPAREN _)) -> Just (Reduce 1 100)
    (97, Token (COMMA _)) -> Just (Reduce 1 100)
    (97, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (97, Token (EQUAL _)) -> Just (Reduce 1 100)
    (97, Token (PIPE _)) -> Just (Reduce 1 100)
    (97, Token (RARROW _)) -> Just (Shift 95)
    (97, Token (LBRACKET _)) -> Just (Shift 128)
    (97, Token (RBRACKET _)) -> Just (Reduce 1 100)
    (97, Token (EXCL _)) -> Just (Shift 84)
    (97, Token (QCONID _)) -> Just (Shift 131)
    (97, Token (EXPORT _)) -> Just (Shift 306)
    (97, Token (AS _)) -> Just (Shift 307)
    (97, Token (QVARID _)) -> Just (Shift 308)
    (98, Token (LPAREN _)) -> Just (Shift 124)
    (98, Token (LBRACKET _)) -> Just (Shift 128)
    (98, Token (EXCL _)) -> Just (Shift 84)
    (98, Token (QCONID _)) -> Just (Shift 131)
    (98, Token (EXPORT _)) -> Just (Shift 306)
    (98, Token (AS _)) -> Just (Shift 307)
    (98, Token (QVARID _)) -> Just (Shift 308)
    (99, Token (RBRACE _)) -> Just (Reduce 0 119)
    (99, Token (LPAREN _)) -> Just (Shift 124)
    (99, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (99, Token (EQUAL _)) -> Just (Shift 102)
    (99, Token (DERIVING _)) -> Just (Reduce 0 119)
    (99, Token (DARROW _)) -> Just (Shift 100)
    (99, Token (LBRACKET _)) -> Just (Shift 128)
    (99, Token (EXCL _)) -> Just (Shift 84)
    (99, Token (QCONID _)) -> Just (Shift 131)
    (99, Token (EXPORT _)) -> Just (Shift 306)
    (99, Token (AS _)) -> Just (Shift 307)
    (99, Token (QVARID _)) -> Just (Shift 308)
    (100, Token (LPAREN _)) -> Just (Shift 124)
    (100, Token (LBRACKET _)) -> Just (Shift 128)
    (100, Token (EXCL _)) -> Just (Shift 84)
    (100, Token (QCONID _)) -> Just (Shift 131)
    (100, Token (EXPORT _)) -> Just (Shift 306)
    (100, Token (AS _)) -> Just (Shift 307)
    (100, Token (QVARID _)) -> Just (Shift 308)
    (101, Token (RBRACE _)) -> Just (Reduce 0 119)
    (101, Token (LPAREN _)) -> Just (Shift 124)
    (101, Token (SEMICOLON _)) -> Just (Reduce 0 119)
    (101, Token (EQUAL _)) -> Just (Shift 102)
    (101, Token (DERIVING _)) -> Just (Reduce 0 119)
    (101, Token (LBRACKET _)) -> Just (Shift 128)
    (101, Token (EXCL _)) -> Just (Shift 84)
    (101, Token (QCONID _)) -> Just (Shift 131)
    (101, Token (EXPORT _)) -> Just (Shift 306)
    (101, Token (AS _)) -> Just (Shift 307)
    (101, Token (QVARID _)) -> Just (Shift 308)
    (102, Token (LPAREN _)) -> Just (Shift 124)
    (102, Token (LBRACKET _)) -> Just (Shift 128)
    (102, Token (EXCL _)) -> Just (Shift 84)
    (102, Token (QCONID _)) -> Just (Shift 131)
    (102, Token (EXPORT _)) -> Just (Shift 306)
    (102, Token (AS _)) -> Just (Shift 307)
    (102, Token (QVARID _)) -> Just (Shift 308)
    (103, Token (LPAREN _)) -> Just (Shift 124)
    (103, Token (LBRACKET _)) -> Just (Shift 128)
    (103, Token (EXCL _)) -> Just (Shift 84)
    (103, Token (QCONID _)) -> Just (Shift 131)
    (103, Token (EXPORT _)) -> Just (Shift 306)
    (103, Token (AS _)) -> Just (Shift 307)
    (103, Token (QVARID _)) -> Just (Shift 308)
    (104, Token (LPAREN _)) -> Just (Shift 129)
    (104, Token (QCONID _)) -> Just (Shift 131)
    (105, Token (RBRACE _)) -> Just (Reduce 1 123)
    (105, Token (LPAREN _)) -> Just (Shift 124)
    (105, Token (SEMICOLON _)) -> Just (Reduce 1 123)
    (105, Token (DERIVING _)) -> Just (Reduce 1 123)
    (105, Token (PIPE _)) -> Just (Reduce 1 123)
    (105, Token (LBRACKET _)) -> Just (Shift 128)
    (105, Token (EXCL _)) -> Just (Shift 84)
    (105, Token (QCONID _)) -> Just (Shift 131)
    (105, Token (EXPORT _)) -> Just (Shift 306)
    (105, Token (AS _)) -> Just (Shift 307)
    (105, Token (QVARID _)) -> Just (Shift 308)
    (105, Token (QCONSYM _)) -> Just (Shift 319)
    (105, Token (BACKQUOTE _)) -> Just (Shift 321)
    (106, Token (LPAREN _)) -> Just (Shift 124)
    (106, Token (LBRACKET _)) -> Just (Shift 128)
    (106, Token (EXCL _)) -> Just (Shift 84)
    (106, Token (QCONID _)) -> Just (Shift 131)
    (106, Token (EXPORT _)) -> Just (Shift 306)
    (106, Token (AS _)) -> Just (Shift 307)
    (106, Token (QVARID _)) -> Just (Shift 308)
    (107, Token (RBRACE _)) -> Just (Reduce 3 124)
    (107, Token (LPAREN _)) -> Just (Shift 124)
    (107, Token (SEMICOLON _)) -> Just (Reduce 3 124)
    (107, Token (DERIVING _)) -> Just (Reduce 3 124)
    (107, Token (PIPE _)) -> Just (Reduce 3 124)
    (107, Token (LBRACKET _)) -> Just (Shift 128)
    (107, Token (EXCL _)) -> Just (Shift 84)
    (107, Token (QCONID _)) -> Just (Shift 131)
    (107, Token (EXPORT _)) -> Just (Shift 306)
    (107, Token (AS _)) -> Just (Shift 307)
    (107, Token (QVARID _)) -> Just (Shift 308)
    (108, Token (LPAREN _)) -> Just (Shift 124)
    (108, Token (LBRACKET _)) -> Just (Shift 128)
    (108, Token (EXCL _)) -> Just (Shift 84)
    (108, Token (QCONID _)) -> Just (Shift 131)
    (108, Token (EXPORT _)) -> Just (Shift 306)
    (108, Token (AS _)) -> Just (Shift 307)
    (108, Token (QVARID _)) -> Just (Shift 308)
    (109, Token (RBRACE _)) -> Just (Reduce 1 100)
    (109, Token (LPAREN _)) -> Just (Shift 124)
    (109, Token (SEMICOLON _)) -> Just (Reduce 1 100)
    (109, Token (DARROW _)) -> Just (Shift 114)
    (109, Token (RARROW _)) -> Just (Shift 95)
    (109, Token (LBRACKET _)) -> Just (Shift 128)
    (109, Token (EXCL _)) -> Just (Shift 84)
    (109, Token (QCONID _)) -> Just (Shift 131)
    (109, Token (EXPORT _)) -> Just (Shift 306)
    (109, Token (AS _)) -> Just (Shift 307)
    (109, Token (QVARID _)) -> Just (Shift 308)
    (110, Token (LPAREN _)) -> Just (Shift 124)
    (110, Token (LBRACKET _)) -> Just (Shift 128)
    (110, Token (EXCL _)) -> Just (Shift 84)
    (110, Token (QCONID _)) -> Just (Shift 131)
    (110, Token (EXPORT _)) -> Just (Shift 306)
    (110, Token (AS _)) -> Just (Shift 307)
    (110, Token (QVARID _)) -> Just (Shift 308)
    (111, Token (LPAREN _)) -> Just (Shift 124)
    (111, Token (LBRACKET _)) -> Just (Shift 128)
    (111, Token (EXCL _)) -> Just (Shift 84)
    (111, Token (QCONID _)) -> Just (Shift 131)
    (111, Token (EXPORT _)) -> Just (Shift 306)
    (111, Token (AS _)) -> Just (Shift 307)
    (111, Token (QVARID _)) -> Just (Shift 308)
    (112, Token (LPAREN _)) -> Just (Shift 124)
    (112, Token (LBRACKET _)) -> Just (Shift 128)
    (112, Token (EXCL _)) -> Just (Shift 84)
    (112, Token (QCONID _)) -> Just (Shift 131)
    (112, Token (EXPORT _)) -> Just (Shift 306)
    (112, Token (AS _)) -> Just (Shift 307)
    (112, Token (QVARID _)) -> Just (Shift 308)
    (113, Token (LPAREN _)) -> Just (Shift 124)
    (113, Token (LBRACKET _)) -> Just (Shift 128)
    (113, Token (EXCL _)) -> Just (Shift 84)
    (113, Token (QCONID _)) -> Just (Shift 131)
    (113, Token (EXPORT _)) -> Just (Shift 306)
    (113, Token (AS _)) -> Just (Shift 307)
    (113, Token (QVARID _)) -> Just (Shift 308)
    (114, Token (LPAREN _)) -> Just (Shift 124)
    (114, Token (LBRACKET _)) -> Just (Shift 128)
    (114, Token (EXCL _)) -> Just (Shift 84)
    (114, Token (QCONID _)) -> Just (Shift 131)
    (114, Token (EXPORT _)) -> Just (Shift 306)
    (114, Token (AS _)) -> Just (Shift 307)
    (114, Token (QVARID _)) -> Just (Shift 308)
    (115, Token (LPAREN _)) -> Just (Shift 124)
    (115, Token (LBRACKET _)) -> Just (Shift 128)
    (115, Token (EXCL _)) -> Just (Shift 84)
    (115, Token (QCONID _)) -> Just (Shift 131)
    (115, Token (EXPORT _)) -> Just (Shift 306)
    (115, Token (AS _)) -> Just (Shift 307)
    (115, Token (QVARID _)) -> Just (Shift 308)
    (116, Token (LPAREN _)) -> Just (Shift 124)
    (116, Token (LBRACKET _)) -> Just (Shift 128)
    (116, Token (EXCL _)) -> Just (Shift 84)
    (116, Token (QCONID _)) -> Just (Shift 131)
    (116, Token (EXPORT _)) -> Just (Shift 306)
    (116, Token (AS _)) -> Just (Shift 307)
    (116, Token (QVARID _)) -> Just (Shift 308)
    (117, Token (LBRACE _)) -> Just (Shift 71)
    (117, Token (LPAREN _)) -> Just (Shift 124)
    (117, Token (LBRACKET _)) -> Just (Shift 128)
    (117, Token (EXCL _)) -> Just (Shift 84)
    (117, Token (QCONID _)) -> Just (Shift 131)
    (117, Token (EXPORT _)) -> Just (Shift 306)
    (117, Token (AS _)) -> Just (Shift 307)
    (117, Token (QVARID _)) -> Just (Shift 308)
    (118, Token (LPAREN _)) -> Just (Shift 124)
    (118, Token (LBRACKET _)) -> Just (Shift 128)
    (118, Token (EXCL _)) -> Just (Shift 84)
    (118, Token (QCONID _)) -> Just (Shift 131)
    (118, Token (EXPORT _)) -> Just (Shift 306)
    (118, Token (AS _)) -> Just (Shift 307)
    (118, Token (QVARID _)) -> Just (Shift 308)
    (119, Token (LPAREN _)) -> Just (Shift 124)
    (119, Token (EQUAL _)) -> Just (Shift 104)
    (119, Token (DARROW _)) -> Just (Shift 121)
    (119, Token (LBRACKET _)) -> Just (Shift 128)
    (119, Token (EXCL _)) -> Just (Shift 84)
    (119, Token (QCONID _)) -> Just (Shift 131)
    (119, Token (EXPORT _)) -> Just (Shift 306)
    (119, Token (AS _)) -> Just (Shift 307)
    (119, Token (QVARID _)) -> Just (Shift 308)
    (120, Token (LPAREN _)) -> Just (Shift 124)
    (120, Token (LBRACKET _)) -> Just (Shift 128)
    (120, Token (EXCL _)) -> Just (Shift 84)
    (120, Token (QCONID _)) -> Just (Shift 131)
    (120, Token (EXPORT _)) -> Just (Shift 306)
    (120, Token (AS _)) -> Just (Shift 307)
    (120, Token (QVARID _)) -> Just (Shift 308)
    (121, Token (LPAREN _)) -> Just (Shift 124)
    (121, Token (LBRACKET _)) -> Just (Shift 128)
    (121, Token (EXCL _)) -> Just (Shift 84)
    (121, Token (QCONID _)) -> Just (Shift 131)
    (121, Token (EXPORT _)) -> Just (Shift 306)
    (121, Token (AS _)) -> Just (Shift 307)
    (121, Token (QVARID _)) -> Just (Shift 308)
    (122, Token (LPAREN _)) -> Just (Shift 124)
    (122, Token (EQUAL _)) -> Just (Shift 110)
    (122, Token (LBRACKET _)) -> Just (Shift 128)
    (122, Token (EXCL _)) -> Just (Shift 84)
    (122, Token (QCONID _)) -> Just (Shift 131)
    (122, Token (EXPORT _)) -> Just (Shift 306)
    (122, Token (AS _)) -> Just (Shift 307)
    (122, Token (QVARID _)) -> Just (Shift 308)
    (123, Token (LPAREN _)) -> Just (Shift 124)
    (123, Token (EQUAL _)) -> Just (Shift 104)
    (123, Token (LBRACKET _)) -> Just (Shift 128)
    (123, Token (EXCL _)) -> Just (Shift 84)
    (123, Token (QCONID _)) -> Just (Shift 131)
    (123, Token (EXPORT _)) -> Just (Shift 306)
    (123, Token (AS _)) -> Just (Shift 307)
    (123, Token (QVARID _)) -> Just (Shift 308)
    (124, Token (LPAREN _)) -> Just (Shift 124)
    (124, Token (RPAREN _)) -> Just (Shift 298)
    (124, Token (COMMA _)) -> Just (Shift 311)
    (124, Token (RARROW _)) -> Just (Shift 301)
    (124, Token (LBRACKET _)) -> Just (Shift 128)
    (124, Token (EXCL _)) -> Just (Shift 84)
    (124, Token (QCONID _)) -> Just (Shift 131)
    (124, Token (EXPORT _)) -> Just (Shift 306)
    (124, Token (AS _)) -> Just (Shift 307)
    (124, Token (QVARID _)) -> Just (Shift 308)
    (124, Token (QCONSYM _)) -> Just (Shift 132)
    (125, Token (LPAREN _)) -> Just (Shift 124)
    (125, Token (RPAREN _)) -> Just (Shift 154)
    (125, Token (LBRACKET _)) -> Just (Shift 128)
    (125, Token (EXCL _)) -> Just (Shift 84)
    (125, Token (QCONID _)) -> Just (Shift 131)
    (125, Token (EXPORT _)) -> Just (Shift 306)
    (125, Token (AS _)) -> Just (Shift 307)
    (125, Token (QVARID _)) -> Just (Shift 308)
    (126, Token (LPAREN _)) -> Just (Shift 124)
    (126, Token (LBRACKET _)) -> Just (Shift 128)
    (126, Token (EXCL _)) -> Just (Shift 84)
    (126, Token (QCONID _)) -> Just (Shift 131)
    (126, Token (EXPORT _)) -> Just (Shift 306)
    (126, Token (AS _)) -> Just (Shift 307)
    (126, Token (QVARID _)) -> Just (Shift 308)
    (127, Token (LPAREN _)) -> Just (Shift 124)
    (127, Token (LBRACKET _)) -> Just (Shift 128)
    (127, Token (EXCL _)) -> Just (Shift 84)
    (127, Token (QCONID _)) -> Just (Shift 131)
    (127, Token (EXPORT _)) -> Just (Shift 306)
    (127, Token (AS _)) -> Just (Shift 307)
    (127, Token (QVARID _)) -> Just (Shift 308)
    (128, Token (LPAREN _)) -> Just (Shift 124)
    (128, Token (LBRACKET _)) -> Just (Shift 128)
    (128, Token (RBRACKET _)) -> Just (Shift 302)
    (128, Token (EXCL _)) -> Just (Shift 84)
    (128, Token (QCONID _)) -> Just (Shift 131)
    (128, Token (EXPORT _)) -> Just (Shift 306)
    (128, Token (AS _)) -> Just (Shift 307)
    (128, Token (QVARID _)) -> Just (Shift 308)
    (129, Token (QCONSYM _)) -> Just (Shift 132)
    (130, Token (WHERE _)) -> Just (Reduce 3 179)
    (130, Token (LBRACE _)) -> Just (Reduce 3 179)
    (130, Token (RBRACE _)) -> Just (Reduce 3 179)
    (130, Token (LPAREN _)) -> Just (Reduce 3 179)
    (130, Token (RPAREN _)) -> Just (Reduce 3 179)
    (130, Token (COMMA _)) -> Just (Reduce 3 179)
    (130, Token (SEMICOLON _)) -> Just (Reduce 3 179)
    (130, Token (EQUAL _)) -> Just (Reduce 3 179)
    (130, Token (DERIVING _)) -> Just (Reduce 3 179)
    (130, Token (DARROW _)) -> Just (Reduce 3 179)
    (130, Token (PIPE _)) -> Just (Reduce 3 179)
    (130, Token (COLON_COLON _)) -> Just (Reduce 3 179)
    (130, Token (MINUS _)) -> Just (Reduce 3 179)
    (130, Token (INFIXL _)) -> Just (Reduce 3 179)
    (130, Token (INFIXR _)) -> Just (Reduce 3 179)
    (130, Token (INFIX _)) -> Just (Reduce 3 179)
    (130, Token (RARROW _)) -> Just (Reduce 3 179)
    (130, Token (LBRACKET _)) -> Just (Reduce 3 179)
    (130, Token (RBRACKET _)) -> Just (Reduce 3 179)
    (130, Token (EXCL _)) -> Just (Reduce 3 179)
    (130, Token (QCONID _)) -> Just (Reduce 3 179)
    (130, Token (EXPORT _)) -> Just (Reduce 3 179)
    (130, Token (AS _)) -> Just (Reduce 3 179)
    (130, Token (QVARID _)) -> Just (Reduce 3 179)
    (130, Token (INTEGER _)) -> Just (Reduce 3 179)
    (130, Token (QVARSYM _)) -> Just (Reduce 3 179)
    (130, Token (QCONSYM _)) -> Just (Reduce 3 179)
    (130, Token (BACKQUOTE _)) -> Just (Reduce 3 179)
    (131, Token (WHERE _)) -> Just (Reduce 1 178)
    (131, Token (LBRACE _)) -> Just (Reduce 1 178)
    (131, Token (RBRACE _)) -> Just (Reduce 1 178)
    (131, Token (LPAREN _)) -> Just (Reduce 1 178)
    (131, Token (RPAREN _)) -> Just (Reduce 1 178)
    (131, Token (COMMA _)) -> Just (Reduce 1 178)
    (131, Token (SEMICOLON _)) -> Just (Reduce 1 178)
    (131, Token (EQUAL _)) -> Just (Reduce 1 178)
    (131, Token (DERIVING _)) -> Just (Reduce 1 178)
    (131, Token (DARROW _)) -> Just (Reduce 1 178)
    (131, Token (PIPE _)) -> Just (Reduce 1 178)
    (131, Token (COLON_COLON _)) -> Just (Reduce 1 178)
    (131, Token (MINUS _)) -> Just (Reduce 1 178)
    (131, Token (INFIXL _)) -> Just (Reduce 1 178)
    (131, Token (INFIXR _)) -> Just (Reduce 1 178)
    (131, Token (INFIX _)) -> Just (Reduce 1 178)
    (131, Token (RARROW _)) -> Just (Reduce 1 178)
    (131, Token (LBRACKET _)) -> Just (Reduce 1 178)
    (131, Token (RBRACKET _)) -> Just (Reduce 1 178)
    (131, Token (EXCL _)) -> Just (Reduce 1 178)
    (131, Token (QCONID _)) -> Just (Reduce 1 178)
    (131, Token (EXPORT _)) -> Just (Reduce 1 178)
    (131, Token (AS _)) -> Just (Reduce 1 178)
    (131, Token (QVARID _)) -> Just (Reduce 1 178)
    (131, Token (INTEGER _)) -> Just (Reduce 1 178)
    (131, Token (QVARSYM _)) -> Just (Reduce 1 178)
    (131, Token (QCONSYM _)) -> Just (Reduce 1 178)
    (131, Token (BACKQUOTE _)) -> Just (Reduce 1 178)
    (132, Token (RPAREN _)) -> Just (Shift 130)
    (133, Token (RPAREN _)) -> Just (Reduce 3 24)
    (134, Token (RPAREN _)) -> Just (Reduce 1 23)
    (134, Token (COMMA _)) -> Just (Shift 66)
    (135, Token (RPAREN _)) -> Just (Reduce 3 17)
    (136, Token (RPAREN _)) -> Just (Reduce 1 16)
    (136, Token (COMMA _)) -> Just (Shift 63)
    (137, Token (RPAREN _)) -> Just (Reduce 3 20)
    (137, Token (COMMA _)) -> Just (Reduce 3 20)
    (138, Token (RPAREN _)) -> Just (Reduce 4 21)
    (138, Token (COMMA _)) -> Just (Reduce 4 21)
    (139, Token (RPAREN _)) -> Just (Reduce 4 22)
    (139, Token (COMMA _)) -> Just (Reduce 4 22)
    (140, Token (RPAREN _)) -> Just (Shift 138)
    (141, Token (RPAREN _)) -> Just (Reduce 1 18)
    (141, Token (COMMA _)) -> Just (Reduce 1 18)
    (142, Token (LPAREN _)) -> Just (Shift 67)
    (142, Token (RPAREN _)) -> Just (Reduce 1 19)
    (142, Token (COMMA _)) -> Just (Reduce 1 19)
    (143, Token (RPAREN _)) -> Just (Shift 139)
    (144, Token (RPAREN _)) -> Just (Reduce 1 25)
    (144, Token (COMMA _)) -> Just (Reduce 1 25)
    (145, Token (RPAREN _)) -> Just (Reduce 1 26)
    (145, Token (COMMA _)) -> Just (Reduce 1 26)
    (146, Token (RPAREN _)) -> Just (Shift 150)
    (146, Token (QCONID _)) -> Just (Shift 201)
    (147, Token (RPAREN _)) -> Just (Shift 151)
    (147, Token (QCONID _)) -> Just (Shift 201)
    (148, Token (RPAREN _)) -> Just (Shift 152)
    (148, Token (QCONID _)) -> Just (Shift 201)
    (149, Token (RPAREN _)) -> Just (Shift 153)
    (149, Token (QCONID _)) -> Just (Shift 201)
    (150, Token (RBRACE _)) -> Just (Reduce 6 35)
    (150, Token (SEMICOLON _)) -> Just (Reduce 6 35)
    (151, Token (RBRACE _)) -> Just (Reduce 8 39)
    (151, Token (SEMICOLON _)) -> Just (Reduce 8 39)
    (152, Token (RBRACE _)) -> Just (Reduce 8 47)
    (152, Token (SEMICOLON _)) -> Just (Reduce 8 47)
    (153, Token (RBRACE _)) -> Just (Reduce 6 43)
    (153, Token (SEMICOLON _)) -> Just (Reduce 6 43)
    (154, Token (RBRACE _)) -> Just (Reduce 3 53)
    (154, Token (SEMICOLON _)) -> Just (Reduce 3 53)
    (155, Token (RBRACE _)) -> Just (Reduce 8 31)
    (155, Token (SEMICOLON _)) -> Just (Reduce 8 31)
    (156, Token (RBRACE _)) -> Just (Reduce 7 30)
    (156, Token (SEMICOLON _)) -> Just (Reduce 7 30)
    (157, Token (RBRACE _)) -> Just (Reduce 7 36)
    (157, Token (SEMICOLON _)) -> Just (Reduce 7 36)
    (158, Token (RBRACE _)) -> Just (Reduce 9 40)
    (158, Token (SEMICOLON _)) -> Just (Reduce 9 40)
    (159, Token (RBRACE _)) -> Just (Reduce 9 48)
    (159, Token (SEMICOLON _)) -> Just (Reduce 9 48)
    (160, Token (RBRACE _)) -> Just (Reduce 7 44)
    (160, Token (SEMICOLON _)) -> Just (Reduce 7 44)
    (161, Token (RBRACE _)) -> Just (Reduce 4 54)
    (161, Token (SEMICOLON _)) -> Just (Reduce 4 54)
    (162, Token (QCONID _)) -> Just (Reduce 0 190)
    (162, Token (QUALIFIED _)) -> Just (Shift 194)
    (163, Token (LPAREN _)) -> Just (Shift 64)
    (164, Token (LPAREN _)) -> Just (Shift 146)
    (164, Token (QCONID _)) -> Just (Shift 201)
    (165, Token (LPAREN _)) -> Just (Shift 147)
    (165, Token (QCONID _)) -> Just (Shift 201)
    (166, Token (LPAREN _)) -> Just (Shift 148)
    (166, Token (QCONID _)) -> Just (Shift 201)
    (167, Token (LPAREN _)) -> Just (Shift 149)
    (167, Token (QCONID _)) -> Just (Shift 201)
    (168, Token (LPAREN _)) -> Just (Shift 125)
    (169, Token (IMPORT _)) -> Just (Shift 214)
    (169, Token (EXPORT _)) -> Just (Shift 215)
    (170, Token (RBRACE _)) -> Just (Reduce 0 188)
    (170, Token (LPAREN _)) -> Just (Reduce 0 188)
    (170, Token (SEMICOLON _)) -> Just (Reduce 0 188)
    (170, Token (HIDING _)) -> Just (Reduce 0 188)
    (170, Token (AS _)) -> Just (Shift 9)
    (171, Token (RPAREN _)) -> Just (Shift 155)
    (172, Token (RPAREN _)) -> Just (Shift 156)
    (173, Token (RBRACE _)) -> Just (Reduce 4 29)
    (173, Token (LPAREN _)) -> Just (Shift 65)
    (173, Token (SEMICOLON _)) -> Just (Reduce 4 29)
    (173, Token (HIDING _)) -> Just (Shift 163)
    (174, Token (RBRACE _)) -> Just (Reduce 4 32)
    (174, Token (SEMICOLON _)) -> Just (Reduce 4 32)
    (175, Token (RBRACE _)) -> Just (Reduce 3 33)
    (175, Token (SEMICOLON _)) -> Just (Reduce 3 33)
    (175, Token (DERIVING _)) -> Just (Shift 164)
    (176, Token (RBRACE _)) -> Just (Reduce 5 37)
    (176, Token (SEMICOLON _)) -> Just (Reduce 5 37)
    (176, Token (DERIVING _)) -> Just (Shift 165)
    (177, Token (RBRACE _)) -> Just (Reduce 5 34)
    (177, Token (SEMICOLON _)) -> Just (Reduce 5 34)
    (178, Token (RBRACE _)) -> Just (Reduce 7 38)
    (178, Token (SEMICOLON _)) -> Just (Reduce 7 38)
    (179, Token (RBRACE _)) -> Just (Reduce 7 46)
    (179, Token (SEMICOLON _)) -> Just (Reduce 7 46)
    (180, Token (RBRACE _)) -> Just (Reduce 5 42)
    (180, Token (SEMICOLON _)) -> Just (Reduce 5 42)
    (181, Token (RPAREN _)) -> Just (Shift 157)
    (182, Token (RPAREN _)) -> Just (Shift 158)
    (183, Token (RPAREN _)) -> Just (Shift 159)
    (184, Token (RPAREN _)) -> Just (Shift 160)
    (185, Token (RBRACE _)) -> Just (Reduce 5 45)
    (185, Token (SEMICOLON _)) -> Just (Reduce 5 45)
    (185, Token (DERIVING _)) -> Just (Shift 166)
    (186, Token (RBRACE _)) -> Just (Reduce 3 41)
    (186, Token (SEMICOLON _)) -> Just (Reduce 3 41)
    (186, Token (DERIVING _)) -> Just (Shift 167)
    (187, Token (RBRACE _)) -> Just (Reduce 5 50)
    (187, Token (SEMICOLON _)) -> Just (Reduce 5 50)
    (188, Token (RBRACE _)) -> Just (Reduce 3 49)
    (188, Token (SEMICOLON _)) -> Just (Reduce 3 49)
    (189, Token (RBRACE _)) -> Just (Reduce 5 52)
    (189, Token (SEMICOLON _)) -> Just (Reduce 5 52)
    (190, Token (RBRACE _)) -> Just (Reduce 3 51)
    (190, Token (SEMICOLON _)) -> Just (Reduce 3 51)
    (191, Token (RPAREN _)) -> Just (Shift 161)
    (192, Token (RBRACE _)) -> Just (Reduce 2 55)
    (192, Token (SEMICOLON _)) -> Just (Reduce 2 55)
    (193, Token (RBRACE _)) -> Just (Reduce 1 56)
    (193, Token (SEMICOLON _)) -> Just (Reduce 1 56)
    (194, Token (QCONID _)) -> Just (Reduce 1 191)
    (195, Token (RBRACE _)) -> Just (Reduce 2 189)
    (195, Token (LPAREN _)) -> Just (Reduce 2 189)
    (195, Token (SEMICOLON _)) -> Just (Reduce 2 189)
    (195, Token (HIDING _)) -> Just (Reduce 2 189)
    (196, Token (WHERE _)) -> Just (Reduce 1 102)
    (196, Token (LBRACE _)) -> Just (Reduce 1 102)
    (196, Token (RBRACE _)) -> Just (Reduce 1 102)
    (196, Token (LPAREN _)) -> Just (Reduce 1 102)
    (196, Token (RPAREN _)) -> Just (Reduce 1 102)
    (196, Token (COMMA _)) -> Just (Reduce 1 102)
    (196, Token (SEMICOLON _)) -> Just (Reduce 1 102)
    (196, Token (EQUAL _)) -> Just (Reduce 1 102)
    (196, Token (DERIVING _)) -> Just (Reduce 1 102)
    (196, Token (DARROW _)) -> Just (Reduce 1 102)
    (196, Token (PIPE _)) -> Just (Reduce 1 102)
    (196, Token (COLON_COLON _)) -> Just (Reduce 1 102)
    (196, Token (MINUS _)) -> Just (Reduce 1 102)
    (196, Token (INFIXL _)) -> Just (Reduce 1 102)
    (196, Token (INFIXR _)) -> Just (Reduce 1 102)
    (196, Token (INFIX _)) -> Just (Reduce 1 102)
    (196, Token (RARROW _)) -> Just (Reduce 1 102)
    (196, Token (LBRACKET _)) -> Just (Reduce 1 102)
    (196, Token (RBRACKET _)) -> Just (Reduce 1 102)
    (196, Token (EXCL _)) -> Just (Reduce 1 102)
    (196, Token (QCONID _)) -> Just (Reduce 1 102)
    (196, Token (EXPORT _)) -> Just (Reduce 1 102)
    (196, Token (AS _)) -> Just (Reduce 1 102)
    (196, Token (QVARID _)) -> Just (Reduce 1 102)
    (196, Token (INTEGER _)) -> Just (Reduce 1 102)
    (196, Token (QVARSYM _)) -> Just (Reduce 1 102)
    (196, Token (QCONSYM _)) -> Just (Reduce 1 102)
    (196, Token (BACKQUOTE _)) -> Just (Reduce 1 102)
    (197, Token (WHERE _)) -> Just (Reduce 2 103)
    (197, Token (LBRACE _)) -> Just (Reduce 2 103)
    (197, Token (RBRACE _)) -> Just (Reduce 2 103)
    (197, Token (LPAREN _)) -> Just (Reduce 2 103)
    (197, Token (RPAREN _)) -> Just (Reduce 2 103)
    (197, Token (COMMA _)) -> Just (Reduce 2 103)
    (197, Token (SEMICOLON _)) -> Just (Reduce 2 103)
    (197, Token (EQUAL _)) -> Just (Reduce 2 103)
    (197, Token (DERIVING _)) -> Just (Reduce 2 103)
    (197, Token (DARROW _)) -> Just (Reduce 2 103)
    (197, Token (PIPE _)) -> Just (Reduce 2 103)
    (197, Token (COLON_COLON _)) -> Just (Reduce 2 103)
    (197, Token (MINUS _)) -> Just (Reduce 2 103)
    (197, Token (INFIXL _)) -> Just (Reduce 2 103)
    (197, Token (INFIXR _)) -> Just (Reduce 2 103)
    (197, Token (INFIX _)) -> Just (Reduce 2 103)
    (197, Token (RARROW _)) -> Just (Reduce 2 103)
    (197, Token (LBRACKET _)) -> Just (Reduce 2 103)
    (197, Token (RBRACKET _)) -> Just (Reduce 2 103)
    (197, Token (EXCL _)) -> Just (Reduce 2 103)
    (197, Token (QCONID _)) -> Just (Reduce 2 103)
    (197, Token (EXPORT _)) -> Just (Reduce 2 103)
    (197, Token (AS _)) -> Just (Reduce 2 103)
    (197, Token (QVARID _)) -> Just (Reduce 2 103)
    (197, Token (INTEGER _)) -> Just (Reduce 2 103)
    (197, Token (QVARSYM _)) -> Just (Reduce 2 103)
    (197, Token (QCONSYM _)) -> Just (Reduce 2 103)
    (197, Token (BACKQUOTE _)) -> Just (Reduce 2 103)
    (198, Token (WHERE _)) -> Just (Reduce 3 101)
    (198, Token (RBRACE _)) -> Just (Reduce 3 101)
    (198, Token (RPAREN _)) -> Just (Reduce 3 101)
    (198, Token (COMMA _)) -> Just (Reduce 3 101)
    (198, Token (SEMICOLON _)) -> Just (Reduce 3 101)
    (198, Token (EQUAL _)) -> Just (Reduce 3 101)
    (198, Token (PIPE _)) -> Just (Reduce 3 101)
    (198, Token (RBRACKET _)) -> Just (Reduce 3 101)
    (199, Token (RBRACE _)) -> Just (Reduce 2 120)
    (199, Token (SEMICOLON _)) -> Just (Reduce 2 120)
    (199, Token (DERIVING _)) -> Just (Reduce 2 120)
    (200, Token (QCONID _)) -> Just (Shift 201)
    (201, Token (RBRACE _)) -> Just (Reduce 1 134)
    (201, Token (RPAREN _)) -> Just (Reduce 1 134)
    (201, Token (COMMA _)) -> Just (Reduce 1 134)
    (201, Token (SEMICOLON _)) -> Just (Reduce 1 134)
    (202, Token (RPAREN _)) -> Just (Reduce 1 132)
    (202, Token (COMMA _)) -> Just (Shift 200)
    (203, Token (RPAREN _)) -> Just (Reduce 3 133)
    (204, Token (RBRACE _)) -> Just (Reduce 7 128)
    (204, Token (SEMICOLON _)) -> Just (Reduce 7 128)
    (204, Token (DERIVING _)) -> Just (Reduce 7 128)
    (205, Token (COLON_COLON _)) -> Just (Shift 116)
    (206, Token (RBRACE _)) -> Just (Shift 204)
    (207, Token (RBRACE _)) -> Just (Reduce 3 127)
    (207, Token (SEMICOLON _)) -> Just (Reduce 3 127)
    (207, Token (DERIVING _)) -> Just (Reduce 3 127)
    (208, Token (LBRACE _)) -> Just (Shift 50)
    (209, Token (RBRACE _)) -> Just (Reduce 2 66)
    (209, Token (SEMICOLON _)) -> Just (Reduce 2 66)
    (210, Token (LBRACE _)) -> Just (Shift 59)
    (211, Token (RBRACE _)) -> Just (Reduce 2 76)
    (211, Token (SEMICOLON _)) -> Just (Reduce 2 76)
    (212, Token (RPAREN _)) -> Just (Reduce 1 98)
    (212, Token (COMMA _)) -> Just (Shift 126)
    (213, Token (RPAREN _)) -> Just (Reduce 3 99)
    (214, Token (EXPORT _)) -> Just (Shift 327)
    (214, Token (AS _)) -> Just (Shift 328)
    (214, Token (QVARID _)) -> Just (Shift 329)
    (215, Token (EXPORT _)) -> Just (Shift 327)
    (215, Token (AS _)) -> Just (Shift 328)
    (215, Token (QVARID _)) -> Just (Shift 329)
    (216, Token (COLON_COLON _)) -> Just (Shift 111)
    (217, Token (COLON_COLON _)) -> Just (Shift 112)
    (218, Token (COLON_COLON _)) -> Just (Shift 113)
    (219, Token (RBRACE _)) -> Just (Reduce 6 135)
    (219, Token (SEMICOLON _)) -> Just (Reduce 6 135)
    (220, Token (RBRACE _)) -> Just (Reduce 7 136)
    (220, Token (SEMICOLON _)) -> Just (Reduce 7 136)
    (221, Token (RBRACE _)) -> Just (Reduce 6 137)
    (221, Token (SEMICOLON _)) -> Just (Reduce 6 137)
    (222, Token (EXPORT _)) -> Just (Shift 331)
    (222, Token (AS _)) -> Just (Shift 332)
    (222, Token (QVARID _)) -> Just (Shift 333)
    (222, Token (STRING _)) -> Just (Shift 330)
    (223, Token (STRING _)) -> Just (Shift 334)
    (224, Token (STRING _)) -> Just (Shift 330)
    (225, Token (LBRACE _)) -> Just (Shift 48)
    (226, Token (LBRACE _)) -> Just (Shift 48)
    (227, Token (RBRACE _)) -> Just (Reduce 5 62)
    (227, Token (SEMICOLON _)) -> Just (Reduce 5 62)
    (228, Token (RBRACE _)) -> Just (Reduce 5 64)
    (228, Token (SEMICOLON _)) -> Just (Reduce 5 64)
    (229, Token (RBRACE _)) -> Just (Reduce 1 60)
    (229, Token (SEMICOLON _)) -> Just (Reduce 1 60)
    (230, Token (WHERE _)) -> Just (Shift 225)
    (230, Token (RBRACE _)) -> Just (Reduce 3 61)
    (230, Token (SEMICOLON _)) -> Just (Reduce 3 61)
    (231, Token (WHERE _)) -> Just (Shift 226)
    (231, Token (RBRACE _)) -> Just (Reduce 3 63)
    (231, Token (SEMICOLON _)) -> Just (Reduce 3 63)
    (232, Token (LBRACE _)) -> Just (Shift 48)
    (233, Token (LBRACE _)) -> Just (Shift 48)
    (234, Token (LBRACE _)) -> Just (Shift 48)
    (235, Token (LBRACE _)) -> Just (Shift 48)
    (236, Token (LBRACE _)) -> Just (Shift 48)
    (237, Token (RBRACE _)) -> Just (Reduce 3 57)
    (237, Token (COMMA _)) -> Just (Reduce 3 57)
    (237, Token (SEMICOLON _)) -> Just (Reduce 3 57)
    (237, Token (EQUAL _)) -> Just (Reduce 3 57)
    (238, Token (RBRACE _)) -> Just (Shift 237)
    (239, Token (RBRACE _)) -> Just (Reduce 1 58)
    (239, Token (SEMICOLON _)) -> Just (Shift 49)
    (240, Token (RBRACE _)) -> Just (Reduce 3 59)
    (241, Token (RBRACE _)) -> Just (Reduce 5 87)
    (241, Token (SEMICOLON _)) -> Just (Reduce 5 87)
    (242, Token (RBRACE _)) -> Just (Reduce 3 86)
    (242, Token (SEMICOLON _)) -> Just (Reduce 3 86)
    (243, Token (COLON_COLON _)) -> Just (Shift 108)
    (244, Token (COMMA _)) -> Just (Reduce 0 197)
    (244, Token (MINUS _)) -> Just (Reduce 0 197)
    (244, Token (QCONID _)) -> Just (Reduce 0 197)
    (244, Token (EXPORT _)) -> Just (Reduce 0 197)
    (244, Token (AS _)) -> Just (Reduce 0 197)
    (244, Token (QVARID _)) -> Just (Reduce 0 197)
    (244, Token (INTEGER _)) -> Just (Shift 280)
    (244, Token (QVARSYM _)) -> Just (Reduce 0 197)
    (244, Token (QCONSYM _)) -> Just (Reduce 0 197)
    (244, Token (BACKQUOTE _)) -> Just (Reduce 0 197)
    (245, Token (MINUS _)) -> Just (Shift 283)
    (245, Token (QVARSYM _)) -> Just (Shift 356)
    (245, Token (QCONSYM _)) -> Just (Shift 319)
    (245, Token (BACKQUOTE _)) -> Just (Shift 320)
    (246, Token (RBRACE _)) -> Just (Reduce 3 88)
    (246, Token (SEMICOLON _)) -> Just (Reduce 3 88)
    (247, Token (LPAREN _)) -> Just (Reduce 1 167)
    (247, Token (RPAREN _)) -> Just (Reduce 1 167)
    (247, Token (EQUAL _)) -> Just (Reduce 1 167)
    (247, Token (PIPE _)) -> Just (Reduce 1 167)
    (247, Token (MINUS _)) -> Just (Reduce 1 167)
    (247, Token (QCONID _)) -> Just (Reduce 1 167)
    (247, Token (EXPORT _)) -> Just (Reduce 1 167)
    (247, Token (AS _)) -> Just (Reduce 1 167)
    (247, Token (QVARID _)) -> Just (Reduce 1 167)
    (247, Token (QVARSYM _)) -> Just (Reduce 1 167)
    (247, Token (QCONSYM _)) -> Just (Reduce 1 167)
    (247, Token (BACKQUOTE _)) -> Just (Reduce 1 167)
    (248, Token (LPAREN _)) -> Just (Reduce 3 169)
    (248, Token (RPAREN _)) -> Just (Reduce 3 169)
    (248, Token (EQUAL _)) -> Just (Reduce 3 169)
    (248, Token (PIPE _)) -> Just (Reduce 3 169)
    (248, Token (MINUS _)) -> Just (Reduce 3 169)
    (248, Token (QCONID _)) -> Just (Reduce 3 169)
    (248, Token (EXPORT _)) -> Just (Reduce 3 169)
    (248, Token (AS _)) -> Just (Reduce 3 169)
    (248, Token (QVARID _)) -> Just (Reduce 3 169)
    (248, Token (QVARSYM _)) -> Just (Reduce 3 169)
    (248, Token (QCONSYM _)) -> Just (Reduce 3 169)
    (248, Token (BACKQUOTE _)) -> Just (Reduce 3 169)
    (249, Token (LPAREN _)) -> Just (Reduce 2 168)
    (249, Token (RPAREN _)) -> Just (Reduce 2 168)
    (249, Token (EQUAL _)) -> Just (Reduce 2 168)
    (249, Token (PIPE _)) -> Just (Reduce 2 168)
    (249, Token (MINUS _)) -> Just (Reduce 2 168)
    (249, Token (QCONID _)) -> Just (Reduce 2 168)
    (249, Token (EXPORT _)) -> Just (Reduce 2 168)
    (249, Token (AS _)) -> Just (Reduce 2 168)
    (249, Token (QVARID _)) -> Just (Reduce 2 168)
    (249, Token (QVARSYM _)) -> Just (Reduce 2 168)
    (249, Token (QCONSYM _)) -> Just (Reduce 2 168)
    (249, Token (BACKQUOTE _)) -> Just (Reduce 2 168)
    (250, Token (LPAREN _)) -> Just (Reduce 3 170)
    (250, Token (RPAREN _)) -> Just (Reduce 3 170)
    (250, Token (EQUAL _)) -> Just (Reduce 3 170)
    (250, Token (PIPE _)) -> Just (Reduce 3 170)
    (250, Token (MINUS _)) -> Just (Reduce 3 170)
    (250, Token (QCONID _)) -> Just (Reduce 3 170)
    (250, Token (EXPORT _)) -> Just (Reduce 3 170)
    (250, Token (AS _)) -> Just (Reduce 3 170)
    (250, Token (QVARID _)) -> Just (Reduce 3 170)
    (250, Token (QVARSYM _)) -> Just (Reduce 3 170)
    (250, Token (QCONSYM _)) -> Just (Reduce 3 170)
    (250, Token (BACKQUOTE _)) -> Just (Reduce 3 170)
    (251, Token (WHERE _)) -> Just (Reduce 5 154)
    (251, Token (RBRACE _)) -> Just (Reduce 5 154)
    (251, Token (RPAREN _)) -> Just (Reduce 5 154)
    (251, Token (COMMA _)) -> Just (Reduce 5 154)
    (251, Token (SEMICOLON _)) -> Just (Reduce 5 154)
    (251, Token (EQUAL _)) -> Just (Reduce 5 154)
    (251, Token (PIPE _)) -> Just (Reduce 5 154)
    (252, Token (WHERE _)) -> Just (Reduce 3 153)
    (252, Token (RBRACE _)) -> Just (Reduce 3 153)
    (252, Token (RPAREN _)) -> Just (Reduce 3 153)
    (252, Token (COMMA _)) -> Just (Reduce 3 153)
    (252, Token (SEMICOLON _)) -> Just (Reduce 3 153)
    (252, Token (EQUAL _)) -> Just (Reduce 3 153)
    (252, Token (PIPE _)) -> Just (Reduce 3 153)
    (253, Token (WHERE _)) -> Just (Reduce 1 155)
    (253, Token (RBRACE _)) -> Just (Reduce 1 155)
    (253, Token (RPAREN _)) -> Just (Reduce 1 155)
    (253, Token (COMMA _)) -> Just (Reduce 1 155)
    (253, Token (SEMICOLON _)) -> Just (Reduce 1 155)
    (253, Token (EQUAL _)) -> Just (Reduce 1 155)
    (253, Token (PIPE _)) -> Just (Reduce 1 155)
    (253, Token (COLON_COLON _)) -> Just (Shift 93)
    (254, Token (WHERE _)) -> Just (Reduce 3 146)
    (254, Token (RBRACE _)) -> Just (Reduce 3 146)
    (254, Token (SEMICOLON _)) -> Just (Reduce 3 146)
    (254, Token (PIPE _)) -> Just (Shift 53)
    (255, Token (WHERE _)) -> Just (Reduce 5 147)
    (255, Token (RBRACE _)) -> Just (Reduce 5 147)
    (255, Token (SEMICOLON _)) -> Just (Reduce 5 147)
    (256, Token (EQUAL _)) -> Just (Shift 38)
    (257, Token (RBRACE _)) -> Just (Reduce 3 67)
    (257, Token (SEMICOLON _)) -> Just (Reduce 3 67)
    (258, Token (RBRACE _)) -> Just (Shift 257)
    (259, Token (RBRACE _)) -> Just (Reduce 3 69)
    (260, Token (RBRACE _)) -> Just (Reduce 1 68)
    (260, Token (SEMICOLON _)) -> Just (Shift 51)
    (261, Token (RBRACE _)) -> Just (Reduce 5 72)
    (261, Token (SEMICOLON _)) -> Just (Reduce 5 72)
    (262, Token (RBRACE _)) -> Just (Reduce 5 74)
    (262, Token (SEMICOLON _)) -> Just (Reduce 5 74)
    (263, Token (RBRACE _)) -> Just (Reduce 1 70)
    (263, Token (SEMICOLON _)) -> Just (Reduce 1 70)
    (264, Token (WHERE _)) -> Just (Shift 232)
    (264, Token (RBRACE _)) -> Just (Reduce 3 71)
    (264, Token (SEMICOLON _)) -> Just (Reduce 3 71)
    (265, Token (WHERE _)) -> Just (Shift 233)
    (265, Token (RBRACE _)) -> Just (Reduce 3 73)
    (265, Token (SEMICOLON _)) -> Just (Reduce 3 73)
    (266, Token (RBRACE _)) -> Just (Reduce 3 77)
    (266, Token (SEMICOLON _)) -> Just (Reduce 3 77)
    (267, Token (RBRACE _)) -> Just (Shift 266)
    (268, Token (RBRACE _)) -> Just (Reduce 3 79)
    (269, Token (RBRACE _)) -> Just (Reduce 1 78)
    (269, Token (SEMICOLON _)) -> Just (Shift 60)
    (270, Token (RBRACE _)) -> Just (Reduce 5 82)
    (270, Token (SEMICOLON _)) -> Just (Reduce 5 82)
    (271, Token (RBRACE _)) -> Just (Reduce 5 84)
    (271, Token (SEMICOLON _)) -> Just (Reduce 5 84)
    (272, Token (WHERE _)) -> Just (Shift 234)
    (272, Token (RBRACE _)) -> Just (Reduce 3 81)
    (272, Token (SEMICOLON _)) -> Just (Reduce 3 81)
    (273, Token (WHERE _)) -> Just (Shift 235)
    (273, Token (RBRACE _)) -> Just (Reduce 3 83)
    (273, Token (SEMICOLON _)) -> Just (Reduce 3 83)
    (274, Token (COMMA _)) -> Just (Shift 68)
    (274, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (275, Token (LPAREN _)) -> Just (Reduce 1 171)
    (275, Token (COMMA _)) -> Just (Shift 68)
    (275, Token (EQUAL _)) -> Just (Reduce 1 171)
    (275, Token (PIPE _)) -> Just (Reduce 1 171)
    (275, Token (COLON_COLON _)) -> Just (Reduce 1 93)
    (275, Token (MINUS _)) -> Just (Reduce 1 171)
    (275, Token (QCONID _)) -> Just (Reduce 1 171)
    (275, Token (EXPORT _)) -> Just (Reduce 1 171)
    (275, Token (AS _)) -> Just (Reduce 1 171)
    (275, Token (QVARID _)) -> Just (Reduce 1 171)
    (275, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (275, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (275, Token (BACKQUOTE _)) -> Just (Reduce 1 171)
    (276, Token (COLON_COLON _)) -> Just (Reduce 3 94)
    (277, Token (COMMA _)) -> Just (Reduce 1 95)
    (277, Token (MINUS _)) -> Just (Reduce 1 95)
    (277, Token (QCONID _)) -> Just (Reduce 1 95)
    (277, Token (EXPORT _)) -> Just (Reduce 1 95)
    (277, Token (AS _)) -> Just (Reduce 1 95)
    (277, Token (QVARID _)) -> Just (Reduce 1 95)
    (277, Token (INTEGER _)) -> Just (Reduce 1 95)
    (277, Token (QVARSYM _)) -> Just (Reduce 1 95)
    (277, Token (QCONSYM _)) -> Just (Reduce 1 95)
    (277, Token (BACKQUOTE _)) -> Just (Reduce 1 95)
    (278, Token (COMMA _)) -> Just (Reduce 1 96)
    (278, Token (MINUS _)) -> Just (Reduce 1 96)
    (278, Token (QCONID _)) -> Just (Reduce 1 96)
    (278, Token (EXPORT _)) -> Just (Reduce 1 96)
    (278, Token (AS _)) -> Just (Reduce 1 96)
    (278, Token (QVARID _)) -> Just (Reduce 1 96)
    (278, Token (INTEGER _)) -> Just (Reduce 1 96)
    (278, Token (QVARSYM _)) -> Just (Reduce 1 96)
    (278, Token (QCONSYM _)) -> Just (Reduce 1 96)
    (278, Token (BACKQUOTE _)) -> Just (Reduce 1 96)
    (279, Token (COMMA _)) -> Just (Reduce 1 97)
    (279, Token (MINUS _)) -> Just (Reduce 1 97)
    (279, Token (QCONID _)) -> Just (Reduce 1 97)
    (279, Token (EXPORT _)) -> Just (Reduce 1 97)
    (279, Token (AS _)) -> Just (Reduce 1 97)
    (279, Token (QVARID _)) -> Just (Reduce 1 97)
    (279, Token (INTEGER _)) -> Just (Reduce 1 97)
    (279, Token (QVARSYM _)) -> Just (Reduce 1 97)
    (279, Token (QCONSYM _)) -> Just (Reduce 1 97)
    (279, Token (BACKQUOTE _)) -> Just (Reduce 1 97)
    (280, Token (COMMA _)) -> Just (Reduce 1 198)
    (280, Token (MINUS _)) -> Just (Reduce 1 198)
    (280, Token (QCONID _)) -> Just (Reduce 1 198)
    (280, Token (EXPORT _)) -> Just (Reduce 1 198)
    (280, Token (AS _)) -> Just (Reduce 1 198)
    (280, Token (QVARID _)) -> Just (Reduce 1 198)
    (280, Token (QVARSYM _)) -> Just (Reduce 1 198)
    (280, Token (QCONSYM _)) -> Just (Reduce 1 198)
    (280, Token (BACKQUOTE _)) -> Just (Reduce 1 198)
    (281, Token (MINUS _)) -> Just (Shift 283)
    (281, Token (QVARSYM _)) -> Just (Shift 356)
    (281, Token (QCONSYM _)) -> Just (Shift 319)
    (281, Token (BACKQUOTE _)) -> Just (Shift 320)
    (282, Token (MINUS _)) -> Just (Shift 283)
    (282, Token (QVARSYM _)) -> Just (Shift 356)
    (282, Token (QCONSYM _)) -> Just (Shift 319)
    (282, Token (BACKQUOTE _)) -> Just (Shift 320)
    (283, Token (RBRACE _)) -> Just (Reduce 1 89)
    (283, Token (COMMA _)) -> Just (Shift 281)
    (283, Token (SEMICOLON _)) -> Just (Reduce 1 89)
    (284, Token (RBRACE _)) -> Just (Reduce 3 91)
    (284, Token (SEMICOLON _)) -> Just (Reduce 3 91)
    (285, Token (RBRACE _)) -> Just (Reduce 3 92)
    (285, Token (SEMICOLON _)) -> Just (Reduce 3 92)
    (286, Token (RBRACE _)) -> Just (Reduce 1 90)
    (286, Token (COMMA _)) -> Just (Shift 282)
    (286, Token (SEMICOLON _)) -> Just (Reduce 1 90)
    (287, Token (RBRACE _)) -> Just (Reduce 1 187)
    (287, Token (LPAREN _)) -> Just (Reduce 1 187)
    (287, Token (COMMA _)) -> Just (Reduce 1 187)
    (287, Token (SEMICOLON _)) -> Just (Reduce 1 187)
    (287, Token (COLON_COLON _)) -> Just (Reduce 1 187)
    (287, Token (MINUS _)) -> Just (Reduce 1 187)
    (287, Token (QCONID _)) -> Just (Reduce 1 187)
    (287, Token (EXPORT _)) -> Just (Reduce 1 187)
    (287, Token (AS _)) -> Just (Reduce 1 187)
    (287, Token (QVARID _)) -> Just (Reduce 1 187)
    (287, Token (STRING _)) -> Just (Reduce 1 187)
    (287, Token (INTEGER _)) -> Just (Reduce 1 187)
    (287, Token (QVARSYM _)) -> Just (Reduce 1 187)
    (287, Token (QCONSYM _)) -> Just (Reduce 1 187)
    (287, Token (BACKQUOTE _)) -> Just (Reduce 1 187)
    (288, Token (RBRACE _)) -> Just (Reduce 1 186)
    (288, Token (LPAREN _)) -> Just (Reduce 1 186)
    (288, Token (COMMA _)) -> Just (Reduce 1 186)
    (288, Token (SEMICOLON _)) -> Just (Reduce 1 186)
    (288, Token (COLON_COLON _)) -> Just (Reduce 1 186)
    (288, Token (MINUS _)) -> Just (Reduce 1 186)
    (288, Token (QCONID _)) -> Just (Reduce 1 186)
    (288, Token (EXPORT _)) -> Just (Reduce 1 186)
    (288, Token (AS _)) -> Just (Reduce 1 186)
    (288, Token (QVARID _)) -> Just (Reduce 1 186)
    (288, Token (STRING _)) -> Just (Reduce 1 186)
    (288, Token (INTEGER _)) -> Just (Reduce 1 186)
    (288, Token (QVARSYM _)) -> Just (Reduce 1 186)
    (288, Token (QCONSYM _)) -> Just (Reduce 1 186)
    (288, Token (BACKQUOTE _)) -> Just (Reduce 1 186)
    (289, Token (WHERE _)) -> Just (Reduce 3 108)
    (289, Token (LBRACE _)) -> Just (Reduce 3 108)
    (289, Token (RBRACE _)) -> Just (Reduce 3 108)
    (289, Token (LPAREN _)) -> Just (Reduce 3 108)
    (289, Token (RPAREN _)) -> Just (Reduce 3 108)
    (289, Token (COMMA _)) -> Just (Reduce 3 108)
    (289, Token (SEMICOLON _)) -> Just (Reduce 3 108)
    (289, Token (EQUAL _)) -> Just (Reduce 3 108)
    (289, Token (DERIVING _)) -> Just (Reduce 3 108)
    (289, Token (DARROW _)) -> Just (Reduce 3 108)
    (289, Token (PIPE _)) -> Just (Reduce 3 108)
    (289, Token (COLON_COLON _)) -> Just (Reduce 3 108)
    (289, Token (MINUS _)) -> Just (Reduce 3 108)
    (289, Token (INFIXL _)) -> Just (Reduce 3 108)
    (289, Token (INFIXR _)) -> Just (Reduce 3 108)
    (289, Token (INFIX _)) -> Just (Reduce 3 108)
    (289, Token (RARROW _)) -> Just (Reduce 3 108)
    (289, Token (LBRACKET _)) -> Just (Reduce 3 108)
    (289, Token (RBRACKET _)) -> Just (Reduce 3 108)
    (289, Token (EXCL _)) -> Just (Reduce 3 108)
    (289, Token (QCONID _)) -> Just (Reduce 3 108)
    (289, Token (EXPORT _)) -> Just (Reduce 3 108)
    (289, Token (AS _)) -> Just (Reduce 3 108)
    (289, Token (QVARID _)) -> Just (Reduce 3 108)
    (289, Token (INTEGER _)) -> Just (Reduce 3 108)
    (289, Token (QVARSYM _)) -> Just (Reduce 3 108)
    (289, Token (QCONSYM _)) -> Just (Reduce 3 108)
    (289, Token (BACKQUOTE _)) -> Just (Reduce 3 108)
    (290, Token (WHERE _)) -> Just (Reduce 3 106)
    (290, Token (LBRACE _)) -> Just (Reduce 3 106)
    (290, Token (RBRACE _)) -> Just (Reduce 3 106)
    (290, Token (LPAREN _)) -> Just (Reduce 3 106)
    (290, Token (RPAREN _)) -> Just (Reduce 3 106)
    (290, Token (COMMA _)) -> Just (Reduce 3 106)
    (290, Token (SEMICOLON _)) -> Just (Reduce 3 106)
    (290, Token (EQUAL _)) -> Just (Reduce 3 106)
    (290, Token (DERIVING _)) -> Just (Reduce 3 106)
    (290, Token (DARROW _)) -> Just (Reduce 3 106)
    (290, Token (PIPE _)) -> Just (Reduce 3 106)
    (290, Token (COLON_COLON _)) -> Just (Reduce 3 106)
    (290, Token (MINUS _)) -> Just (Reduce 3 106)
    (290, Token (INFIXL _)) -> Just (Reduce 3 106)
    (290, Token (INFIXR _)) -> Just (Reduce 3 106)
    (290, Token (INFIX _)) -> Just (Reduce 3 106)
    (290, Token (RARROW _)) -> Just (Reduce 3 106)
    (290, Token (LBRACKET _)) -> Just (Reduce 3 106)
    (290, Token (RBRACKET _)) -> Just (Reduce 3 106)
    (290, Token (EXCL _)) -> Just (Reduce 3 106)
    (290, Token (QCONID _)) -> Just (Reduce 3 106)
    (290, Token (EXPORT _)) -> Just (Reduce 3 106)
    (290, Token (AS _)) -> Just (Reduce 3 106)
    (290, Token (QVARID _)) -> Just (Reduce 3 106)
    (290, Token (INTEGER _)) -> Just (Reduce 3 106)
    (290, Token (QVARSYM _)) -> Just (Reduce 3 106)
    (290, Token (QCONSYM _)) -> Just (Reduce 3 106)
    (290, Token (BACKQUOTE _)) -> Just (Reduce 3 106)
    (291, Token (WHERE _)) -> Just (Reduce 3 107)
    (291, Token (LBRACE _)) -> Just (Reduce 3 107)
    (291, Token (RBRACE _)) -> Just (Reduce 3 107)
    (291, Token (LPAREN _)) -> Just (Reduce 3 107)
    (291, Token (RPAREN _)) -> Just (Reduce 3 107)
    (291, Token (COMMA _)) -> Just (Reduce 3 107)
    (291, Token (SEMICOLON _)) -> Just (Reduce 3 107)
    (291, Token (EQUAL _)) -> Just (Reduce 3 107)
    (291, Token (DERIVING _)) -> Just (Reduce 3 107)
    (291, Token (DARROW _)) -> Just (Reduce 3 107)
    (291, Token (PIPE _)) -> Just (Reduce 3 107)
    (291, Token (COLON_COLON _)) -> Just (Reduce 3 107)
    (291, Token (MINUS _)) -> Just (Reduce 3 107)
    (291, Token (INFIXL _)) -> Just (Reduce 3 107)
    (291, Token (INFIXR _)) -> Just (Reduce 3 107)
    (291, Token (INFIX _)) -> Just (Reduce 3 107)
    (291, Token (RARROW _)) -> Just (Reduce 3 107)
    (291, Token (LBRACKET _)) -> Just (Reduce 3 107)
    (291, Token (RBRACKET _)) -> Just (Reduce 3 107)
    (291, Token (EXCL _)) -> Just (Reduce 3 107)
    (291, Token (QCONID _)) -> Just (Reduce 3 107)
    (291, Token (EXPORT _)) -> Just (Reduce 3 107)
    (291, Token (AS _)) -> Just (Reduce 3 107)
    (291, Token (QVARID _)) -> Just (Reduce 3 107)
    (291, Token (INTEGER _)) -> Just (Reduce 3 107)
    (291, Token (QVARSYM _)) -> Just (Reduce 3 107)
    (291, Token (QCONSYM _)) -> Just (Reduce 3 107)
    (291, Token (BACKQUOTE _)) -> Just (Reduce 3 107)
    (292, Token (RPAREN _)) -> Just (Shift 289)
    (292, Token (COMMA _)) -> Just (Shift 127)
    (293, Token (RBRACKET _)) -> Just (Shift 291)
    (294, Token (WHERE _)) -> Just (Reduce 2 109)
    (294, Token (LBRACE _)) -> Just (Reduce 2 109)
    (294, Token (RBRACE _)) -> Just (Reduce 2 109)
    (294, Token (LPAREN _)) -> Just (Reduce 2 109)
    (294, Token (RPAREN _)) -> Just (Reduce 2 109)
    (294, Token (COMMA _)) -> Just (Reduce 2 109)
    (294, Token (SEMICOLON _)) -> Just (Reduce 2 109)
    (294, Token (EQUAL _)) -> Just (Reduce 2 109)
    (294, Token (DERIVING _)) -> Just (Reduce 2 109)
    (294, Token (DARROW _)) -> Just (Reduce 2 109)
    (294, Token (PIPE _)) -> Just (Reduce 2 109)
    (294, Token (COLON_COLON _)) -> Just (Reduce 2 109)
    (294, Token (MINUS _)) -> Just (Reduce 2 109)
    (294, Token (INFIXL _)) -> Just (Reduce 2 109)
    (294, Token (INFIXR _)) -> Just (Reduce 2 109)
    (294, Token (INFIX _)) -> Just (Reduce 2 109)
    (294, Token (RARROW _)) -> Just (Reduce 2 109)
    (294, Token (LBRACKET _)) -> Just (Reduce 2 109)
    (294, Token (RBRACKET _)) -> Just (Reduce 2 109)
    (294, Token (EXCL _)) -> Just (Reduce 2 109)
    (294, Token (QCONID _)) -> Just (Reduce 2 109)
    (294, Token (EXPORT _)) -> Just (Reduce 2 109)
    (294, Token (AS _)) -> Just (Reduce 2 109)
    (294, Token (QVARID _)) -> Just (Reduce 2 109)
    (294, Token (INTEGER _)) -> Just (Reduce 2 109)
    (294, Token (QVARSYM _)) -> Just (Reduce 2 109)
    (294, Token (QCONSYM _)) -> Just (Reduce 2 109)
    (294, Token (BACKQUOTE _)) -> Just (Reduce 2 109)
    (295, Token (WHERE _)) -> Just (Reduce 1 104)
    (295, Token (LBRACE _)) -> Just (Reduce 1 104)
    (295, Token (RBRACE _)) -> Just (Reduce 1 104)
    (295, Token (LPAREN _)) -> Just (Reduce 1 104)
    (295, Token (RPAREN _)) -> Just (Reduce 1 104)
    (295, Token (COMMA _)) -> Just (Reduce 1 104)
    (295, Token (SEMICOLON _)) -> Just (Reduce 1 104)
    (295, Token (EQUAL _)) -> Just (Reduce 1 104)
    (295, Token (DERIVING _)) -> Just (Reduce 1 104)
    (295, Token (DARROW _)) -> Just (Reduce 1 104)
    (295, Token (PIPE _)) -> Just (Reduce 1 104)
    (295, Token (COLON_COLON _)) -> Just (Reduce 1 104)
    (295, Token (MINUS _)) -> Just (Reduce 1 104)
    (295, Token (INFIXL _)) -> Just (Reduce 1 104)
    (295, Token (INFIXR _)) -> Just (Reduce 1 104)
    (295, Token (INFIX _)) -> Just (Reduce 1 104)
    (295, Token (RARROW _)) -> Just (Reduce 1 104)
    (295, Token (LBRACKET _)) -> Just (Reduce 1 104)
    (295, Token (RBRACKET _)) -> Just (Reduce 1 104)
    (295, Token (EXCL _)) -> Just (Reduce 1 104)
    (295, Token (QCONID _)) -> Just (Reduce 1 104)
    (295, Token (EXPORT _)) -> Just (Reduce 1 104)
    (295, Token (AS _)) -> Just (Reduce 1 104)
    (295, Token (QVARID _)) -> Just (Reduce 1 104)
    (295, Token (INTEGER _)) -> Just (Reduce 1 104)
    (295, Token (QVARSYM _)) -> Just (Reduce 1 104)
    (295, Token (QCONSYM _)) -> Just (Reduce 1 104)
    (295, Token (BACKQUOTE _)) -> Just (Reduce 1 104)
    (296, Token (WHERE _)) -> Just (Reduce 1 105)
    (296, Token (LBRACE _)) -> Just (Reduce 1 105)
    (296, Token (RBRACE _)) -> Just (Reduce 1 105)
    (296, Token (LPAREN _)) -> Just (Reduce 1 105)
    (296, Token (RPAREN _)) -> Just (Reduce 1 105)
    (296, Token (COMMA _)) -> Just (Reduce 1 105)
    (296, Token (SEMICOLON _)) -> Just (Reduce 1 105)
    (296, Token (EQUAL _)) -> Just (Reduce 1 105)
    (296, Token (DERIVING _)) -> Just (Reduce 1 105)
    (296, Token (DARROW _)) -> Just (Reduce 1 105)
    (296, Token (PIPE _)) -> Just (Reduce 1 105)
    (296, Token (COLON_COLON _)) -> Just (Reduce 1 105)
    (296, Token (MINUS _)) -> Just (Reduce 1 105)
    (296, Token (INFIXL _)) -> Just (Reduce 1 105)
    (296, Token (INFIXR _)) -> Just (Reduce 1 105)
    (296, Token (INFIX _)) -> Just (Reduce 1 105)
    (296, Token (RARROW _)) -> Just (Reduce 1 105)
    (296, Token (LBRACKET _)) -> Just (Reduce 1 105)
    (296, Token (RBRACKET _)) -> Just (Reduce 1 105)
    (296, Token (EXCL _)) -> Just (Reduce 1 105)
    (296, Token (QCONID _)) -> Just (Reduce 1 105)
    (296, Token (EXPORT _)) -> Just (Reduce 1 105)
    (296, Token (AS _)) -> Just (Reduce 1 105)
    (296, Token (QVARID _)) -> Just (Reduce 1 105)
    (296, Token (INTEGER _)) -> Just (Reduce 1 105)
    (296, Token (QVARSYM _)) -> Just (Reduce 1 105)
    (296, Token (QCONSYM _)) -> Just (Reduce 1 105)
    (296, Token (BACKQUOTE _)) -> Just (Reduce 1 105)
    (297, Token (RPAREN _)) -> Just (Shift 290)
    (298, Token (WHERE _)) -> Just (Reduce 2 113)
    (298, Token (LBRACE _)) -> Just (Reduce 2 113)
    (298, Token (RBRACE _)) -> Just (Reduce 2 113)
    (298, Token (LPAREN _)) -> Just (Reduce 2 113)
    (298, Token (RPAREN _)) -> Just (Reduce 2 113)
    (298, Token (COMMA _)) -> Just (Reduce 2 113)
    (298, Token (SEMICOLON _)) -> Just (Reduce 2 113)
    (298, Token (EQUAL _)) -> Just (Reduce 2 113)
    (298, Token (DERIVING _)) -> Just (Reduce 2 113)
    (298, Token (DARROW _)) -> Just (Reduce 2 113)
    (298, Token (PIPE _)) -> Just (Reduce 2 113)
    (298, Token (COLON_COLON _)) -> Just (Reduce 2 113)
    (298, Token (MINUS _)) -> Just (Reduce 2 113)
    (298, Token (INFIXL _)) -> Just (Reduce 2 113)
    (298, Token (INFIXR _)) -> Just (Reduce 2 113)
    (298, Token (INFIX _)) -> Just (Reduce 2 113)
    (298, Token (RARROW _)) -> Just (Reduce 2 113)
    (298, Token (LBRACKET _)) -> Just (Reduce 2 113)
    (298, Token (RBRACKET _)) -> Just (Reduce 2 113)
    (298, Token (EXCL _)) -> Just (Reduce 2 113)
    (298, Token (QCONID _)) -> Just (Reduce 2 113)
    (298, Token (EXPORT _)) -> Just (Reduce 2 113)
    (298, Token (AS _)) -> Just (Reduce 2 113)
    (298, Token (QVARID _)) -> Just (Reduce 2 113)
    (298, Token (INTEGER _)) -> Just (Reduce 2 113)
    (298, Token (QVARSYM _)) -> Just (Reduce 2 113)
    (298, Token (QCONSYM _)) -> Just (Reduce 2 113)
    (298, Token (BACKQUOTE _)) -> Just (Reduce 2 113)
    (299, Token (WHERE _)) -> Just (Reduce 3 115)
    (299, Token (LBRACE _)) -> Just (Reduce 3 115)
    (299, Token (RBRACE _)) -> Just (Reduce 3 115)
    (299, Token (LPAREN _)) -> Just (Reduce 3 115)
    (299, Token (RPAREN _)) -> Just (Reduce 3 115)
    (299, Token (COMMA _)) -> Just (Reduce 3 115)
    (299, Token (SEMICOLON _)) -> Just (Reduce 3 115)
    (299, Token (EQUAL _)) -> Just (Reduce 3 115)
    (299, Token (DERIVING _)) -> Just (Reduce 3 115)
    (299, Token (DARROW _)) -> Just (Reduce 3 115)
    (299, Token (PIPE _)) -> Just (Reduce 3 115)
    (299, Token (COLON_COLON _)) -> Just (Reduce 3 115)
    (299, Token (MINUS _)) -> Just (Reduce 3 115)
    (299, Token (INFIXL _)) -> Just (Reduce 3 115)
    (299, Token (INFIXR _)) -> Just (Reduce 3 115)
    (299, Token (INFIX _)) -> Just (Reduce 3 115)
    (299, Token (RARROW _)) -> Just (Reduce 3 115)
    (299, Token (LBRACKET _)) -> Just (Reduce 3 115)
    (299, Token (RBRACKET _)) -> Just (Reduce 3 115)
    (299, Token (EXCL _)) -> Just (Reduce 3 115)
    (299, Token (QCONID _)) -> Just (Reduce 3 115)
    (299, Token (EXPORT _)) -> Just (Reduce 3 115)
    (299, Token (AS _)) -> Just (Reduce 3 115)
    (299, Token (QVARID _)) -> Just (Reduce 3 115)
    (299, Token (INTEGER _)) -> Just (Reduce 3 115)
    (299, Token (QVARSYM _)) -> Just (Reduce 3 115)
    (299, Token (QCONSYM _)) -> Just (Reduce 3 115)
    (299, Token (BACKQUOTE _)) -> Just (Reduce 3 115)
    (300, Token (WHERE _)) -> Just (Reduce 3 116)
    (300, Token (LBRACE _)) -> Just (Reduce 3 116)
    (300, Token (RBRACE _)) -> Just (Reduce 3 116)
    (300, Token (LPAREN _)) -> Just (Reduce 3 116)
    (300, Token (RPAREN _)) -> Just (Reduce 3 116)
    (300, Token (COMMA _)) -> Just (Reduce 3 116)
    (300, Token (SEMICOLON _)) -> Just (Reduce 3 116)
    (300, Token (EQUAL _)) -> Just (Reduce 3 116)
    (300, Token (DERIVING _)) -> Just (Reduce 3 116)
    (300, Token (DARROW _)) -> Just (Reduce 3 116)
    (300, Token (PIPE _)) -> Just (Reduce 3 116)
    (300, Token (COLON_COLON _)) -> Just (Reduce 3 116)
    (300, Token (MINUS _)) -> Just (Reduce 3 116)
    (300, Token (INFIXL _)) -> Just (Reduce 3 116)
    (300, Token (INFIXR _)) -> Just (Reduce 3 116)
    (300, Token (INFIX _)) -> Just (Reduce 3 116)
    (300, Token (RARROW _)) -> Just (Reduce 3 116)
    (300, Token (LBRACKET _)) -> Just (Reduce 3 116)
    (300, Token (RBRACKET _)) -> Just (Reduce 3 116)
    (300, Token (EXCL _)) -> Just (Reduce 3 116)
    (300, Token (QCONID _)) -> Just (Reduce 3 116)
    (300, Token (EXPORT _)) -> Just (Reduce 3 116)
    (300, Token (AS _)) -> Just (Reduce 3 116)
    (300, Token (QVARID _)) -> Just (Reduce 3 116)
    (300, Token (INTEGER _)) -> Just (Reduce 3 116)
    (300, Token (QVARSYM _)) -> Just (Reduce 3 116)
    (300, Token (QCONSYM _)) -> Just (Reduce 3 116)
    (300, Token (BACKQUOTE _)) -> Just (Reduce 3 116)
    (301, Token (RPAREN _)) -> Just (Shift 299)
    (302, Token (WHERE _)) -> Just (Reduce 2 114)
    (302, Token (LBRACE _)) -> Just (Reduce 2 114)
    (302, Token (RBRACE _)) -> Just (Reduce 2 114)
    (302, Token (LPAREN _)) -> Just (Reduce 2 114)
    (302, Token (RPAREN _)) -> Just (Reduce 2 114)
    (302, Token (COMMA _)) -> Just (Reduce 2 114)
    (302, Token (SEMICOLON _)) -> Just (Reduce 2 114)
    (302, Token (EQUAL _)) -> Just (Reduce 2 114)
    (302, Token (DERIVING _)) -> Just (Reduce 2 114)
    (302, Token (DARROW _)) -> Just (Reduce 2 114)
    (302, Token (PIPE _)) -> Just (Reduce 2 114)
    (302, Token (COLON_COLON _)) -> Just (Reduce 2 114)
    (302, Token (MINUS _)) -> Just (Reduce 2 114)
    (302, Token (INFIXL _)) -> Just (Reduce 2 114)
    (302, Token (INFIXR _)) -> Just (Reduce 2 114)
    (302, Token (INFIX _)) -> Just (Reduce 2 114)
    (302, Token (RARROW _)) -> Just (Reduce 2 114)
    (302, Token (LBRACKET _)) -> Just (Reduce 2 114)
    (302, Token (RBRACKET _)) -> Just (Reduce 2 114)
    (302, Token (EXCL _)) -> Just (Reduce 2 114)
    (302, Token (QCONID _)) -> Just (Reduce 2 114)
    (302, Token (EXPORT _)) -> Just (Reduce 2 114)
    (302, Token (AS _)) -> Just (Reduce 2 114)
    (302, Token (QVARID _)) -> Just (Reduce 2 114)
    (302, Token (INTEGER _)) -> Just (Reduce 2 114)
    (302, Token (QVARSYM _)) -> Just (Reduce 2 114)
    (302, Token (QCONSYM _)) -> Just (Reduce 2 114)
    (302, Token (BACKQUOTE _)) -> Just (Reduce 2 114)
    (303, Token (WHERE _)) -> Just (Reduce 1 112)
    (303, Token (LBRACE _)) -> Just (Reduce 1 112)
    (303, Token (RBRACE _)) -> Just (Reduce 1 112)
    (303, Token (LPAREN _)) -> Just (Reduce 1 112)
    (303, Token (RPAREN _)) -> Just (Reduce 1 112)
    (303, Token (COMMA _)) -> Just (Reduce 1 112)
    (303, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (303, Token (EQUAL _)) -> Just (Reduce 1 112)
    (303, Token (DERIVING _)) -> Just (Reduce 1 112)
    (303, Token (DARROW _)) -> Just (Reduce 1 112)
    (303, Token (PIPE _)) -> Just (Reduce 1 112)
    (303, Token (COLON_COLON _)) -> Just (Reduce 1 112)
    (303, Token (MINUS _)) -> Just (Reduce 1 112)
    (303, Token (INFIXL _)) -> Just (Reduce 1 112)
    (303, Token (INFIXR _)) -> Just (Reduce 1 112)
    (303, Token (INFIX _)) -> Just (Reduce 1 112)
    (303, Token (RARROW _)) -> Just (Reduce 1 112)
    (303, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (303, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (303, Token (EXCL _)) -> Just (Reduce 1 112)
    (303, Token (QCONID _)) -> Just (Reduce 1 112)
    (303, Token (EXPORT _)) -> Just (Reduce 1 112)
    (303, Token (AS _)) -> Just (Reduce 1 112)
    (303, Token (QVARID _)) -> Just (Reduce 1 112)
    (303, Token (INTEGER _)) -> Just (Reduce 1 112)
    (303, Token (QVARSYM _)) -> Just (Reduce 1 112)
    (303, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (303, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (304, Token (LBRACE _)) -> Just (Shift 69)
    (304, Token (RBRACE _)) -> Just (Reduce 1 112)
    (304, Token (LPAREN _)) -> Just (Reduce 1 112)
    (304, Token (RPAREN _)) -> Just (Reduce 1 112)
    (304, Token (COMMA _)) -> Just (Reduce 1 112)
    (304, Token (SEMICOLON _)) -> Just (Reduce 1 112)
    (304, Token (DERIVING _)) -> Just (Reduce 1 112)
    (304, Token (PIPE _)) -> Just (Reduce 1 112)
    (304, Token (RARROW _)) -> Just (Reduce 1 112)
    (304, Token (LBRACKET _)) -> Just (Reduce 1 112)
    (304, Token (RBRACKET _)) -> Just (Reduce 1 112)
    (304, Token (EXCL _)) -> Just (Reduce 1 112)
    (304, Token (QCONID _)) -> Just (Reduce 1 112)
    (304, Token (EXPORT _)) -> Just (Reduce 1 112)
    (304, Token (AS _)) -> Just (Reduce 1 112)
    (304, Token (QVARID _)) -> Just (Reduce 1 112)
    (304, Token (QCONSYM _)) -> Just (Reduce 1 112)
    (304, Token (BACKQUOTE _)) -> Just (Reduce 1 112)
    (305, Token (RPAREN _)) -> Just (Shift 300)
    (306, Token (WHERE _)) -> Just (Reduce 1 193)
    (306, Token (LBRACE _)) -> Just (Reduce 1 193)
    (306, Token (RBRACE _)) -> Just (Reduce 1 193)
    (306, Token (LPAREN _)) -> Just (Reduce 1 193)
    (306, Token (RPAREN _)) -> Just (Reduce 1 193)
    (306, Token (COMMA _)) -> Just (Reduce 1 193)
    (306, Token (SEMICOLON _)) -> Just (Reduce 1 193)
    (306, Token (EQUAL _)) -> Just (Reduce 1 193)
    (306, Token (DERIVING _)) -> Just (Reduce 1 193)
    (306, Token (DARROW _)) -> Just (Reduce 1 193)
    (306, Token (PIPE _)) -> Just (Reduce 1 193)
    (306, Token (COLON_COLON _)) -> Just (Reduce 1 193)
    (306, Token (MINUS _)) -> Just (Reduce 1 193)
    (306, Token (INFIXL _)) -> Just (Reduce 1 193)
    (306, Token (INFIXR _)) -> Just (Reduce 1 193)
    (306, Token (INFIX _)) -> Just (Reduce 1 193)
    (306, Token (RARROW _)) -> Just (Reduce 1 193)
    (306, Token (LBRACKET _)) -> Just (Reduce 1 193)
    (306, Token (RBRACKET _)) -> Just (Reduce 1 193)
    (306, Token (EXCL _)) -> Just (Reduce 1 193)
    (306, Token (QCONID _)) -> Just (Reduce 1 193)
    (306, Token (EXPORT _)) -> Just (Reduce 1 193)
    (306, Token (AS _)) -> Just (Reduce 1 193)
    (306, Token (QVARID _)) -> Just (Reduce 1 193)
    (306, Token (INTEGER _)) -> Just (Reduce 1 193)
    (306, Token (QVARSYM _)) -> Just (Reduce 1 193)
    (306, Token (QCONSYM _)) -> Just (Reduce 1 193)
    (306, Token (BACKQUOTE _)) -> Just (Reduce 1 193)
    (307, Token (WHERE _)) -> Just (Reduce 1 192)
    (307, Token (LBRACE _)) -> Just (Reduce 1 192)
    (307, Token (RBRACE _)) -> Just (Reduce 1 192)
    (307, Token (LPAREN _)) -> Just (Reduce 1 192)
    (307, Token (RPAREN _)) -> Just (Reduce 1 192)
    (307, Token (COMMA _)) -> Just (Reduce 1 192)
    (307, Token (SEMICOLON _)) -> Just (Reduce 1 192)
    (307, Token (EQUAL _)) -> Just (Reduce 1 192)
    (307, Token (DERIVING _)) -> Just (Reduce 1 192)
    (307, Token (DARROW _)) -> Just (Reduce 1 192)
    (307, Token (PIPE _)) -> Just (Reduce 1 192)
    (307, Token (COLON_COLON _)) -> Just (Reduce 1 192)
    (307, Token (MINUS _)) -> Just (Reduce 1 192)
    (307, Token (INFIXL _)) -> Just (Reduce 1 192)
    (307, Token (INFIXR _)) -> Just (Reduce 1 192)
    (307, Token (INFIX _)) -> Just (Reduce 1 192)
    (307, Token (RARROW _)) -> Just (Reduce 1 192)
    (307, Token (LBRACKET _)) -> Just (Reduce 1 192)
    (307, Token (RBRACKET _)) -> Just (Reduce 1 192)
    (307, Token (EXCL _)) -> Just (Reduce 1 192)
    (307, Token (QCONID _)) -> Just (Reduce 1 192)
    (307, Token (EXPORT _)) -> Just (Reduce 1 192)
    (307, Token (AS _)) -> Just (Reduce 1 192)
    (307, Token (QVARID _)) -> Just (Reduce 1 192)
    (307, Token (INTEGER _)) -> Just (Reduce 1 192)
    (307, Token (QVARSYM _)) -> Just (Reduce 1 192)
    (307, Token (QCONSYM _)) -> Just (Reduce 1 192)
    (307, Token (BACKQUOTE _)) -> Just (Reduce 1 192)
    (308, Token (WHERE _)) -> Just (Reduce 1 194)
    (308, Token (LBRACE _)) -> Just (Reduce 1 194)
    (308, Token (RBRACE _)) -> Just (Reduce 1 194)
    (308, Token (LPAREN _)) -> Just (Reduce 1 194)
    (308, Token (RPAREN _)) -> Just (Reduce 1 194)
    (308, Token (COMMA _)) -> Just (Reduce 1 194)
    (308, Token (SEMICOLON _)) -> Just (Reduce 1 194)
    (308, Token (EQUAL _)) -> Just (Reduce 1 194)
    (308, Token (DERIVING _)) -> Just (Reduce 1 194)
    (308, Token (DARROW _)) -> Just (Reduce 1 194)
    (308, Token (PIPE _)) -> Just (Reduce 1 194)
    (308, Token (COLON_COLON _)) -> Just (Reduce 1 194)
    (308, Token (MINUS _)) -> Just (Reduce 1 194)
    (308, Token (INFIXL _)) -> Just (Reduce 1 194)
    (308, Token (INFIXR _)) -> Just (Reduce 1 194)
    (308, Token (INFIX _)) -> Just (Reduce 1 194)
    (308, Token (RARROW _)) -> Just (Reduce 1 194)
    (308, Token (LBRACKET _)) -> Just (Reduce 1 194)
    (308, Token (RBRACKET _)) -> Just (Reduce 1 194)
    (308, Token (EXCL _)) -> Just (Reduce 1 194)
    (308, Token (QCONID _)) -> Just (Reduce 1 194)
    (308, Token (EXPORT _)) -> Just (Reduce 1 194)
    (308, Token (AS _)) -> Just (Reduce 1 194)
    (308, Token (QVARID _)) -> Just (Reduce 1 194)
    (308, Token (INTEGER _)) -> Just (Reduce 1 194)
    (308, Token (QVARSYM _)) -> Just (Reduce 1 194)
    (308, Token (QCONSYM _)) -> Just (Reduce 1 194)
    (308, Token (BACKQUOTE _)) -> Just (Reduce 1 194)
    (309, Token (RPAREN _)) -> Just (Reduce 3 110)
    (309, Token (COMMA _)) -> Just (Shift 127)
    (310, Token (RPAREN _)) -> Just (Reduce 3 111)
    (311, Token (RPAREN _)) -> Just (Reduce 1 117)
    (311, Token (COMMA _)) -> Just (Shift 311)
    (312, Token (RPAREN _)) -> Just (Reduce 2 118)
    (313, Token (RBRACE _)) -> Just (Reduce 3 122)
    (313, Token (SEMICOLON _)) -> Just (Reduce 3 122)
    (313, Token (DERIVING _)) -> Just (Reduce 3 122)
    (314, Token (RBRACE _)) -> Just (Reduce 1 121)
    (314, Token (SEMICOLON _)) -> Just (Reduce 1 121)
    (314, Token (DERIVING _)) -> Just (Reduce 1 121)
    (314, Token (PIPE _)) -> Just (Shift 103)
    (315, Token (RBRACE _)) -> Just (Reduce 3 125)
    (315, Token (SEMICOLON _)) -> Just (Reduce 3 125)
    (315, Token (DERIVING _)) -> Just (Reduce 3 125)
    (315, Token (PIPE _)) -> Just (Reduce 3 125)
    (316, Token (RBRACE _)) -> Just (Reduce 4 126)
    (316, Token (SEMICOLON _)) -> Just (Reduce 4 126)
    (316, Token (DERIVING _)) -> Just (Reduce 4 126)
    (316, Token (PIPE _)) -> Just (Reduce 4 126)
    (317, Token (RBRACE _)) -> Just (Shift 316)
    (318, Token (BACKQUOTE _)) -> Just (Shift 322)
    (319, Token (RBRACE _)) -> Just (Reduce 1 184)
    (319, Token (LPAREN _)) -> Just (Reduce 1 184)
    (319, Token (RPAREN _)) -> Just (Reduce 1 184)
    (319, Token (COMMA _)) -> Just (Reduce 1 184)
    (319, Token (SEMICOLON _)) -> Just (Reduce 1 184)
    (319, Token (COLON_COLON _)) -> Just (Reduce 1 184)
    (319, Token (MINUS _)) -> Just (Reduce 1 184)
    (319, Token (RARROW _)) -> Just (Reduce 1 184)
    (319, Token (LBRACKET _)) -> Just (Reduce 1 184)
    (319, Token (RBRACKET _)) -> Just (Reduce 1 184)
    (319, Token (EXCL _)) -> Just (Reduce 1 184)
    (319, Token (QCONID _)) -> Just (Reduce 1 184)
    (319, Token (EXPORT _)) -> Just (Reduce 1 184)
    (319, Token (AS _)) -> Just (Reduce 1 184)
    (319, Token (QVARID _)) -> Just (Reduce 1 184)
    (319, Token (STRING _)) -> Just (Reduce 1 184)
    (319, Token (INTEGER _)) -> Just (Reduce 1 184)
    (319, Token (QVARSYM _)) -> Just (Reduce 1 184)
    (319, Token (QCONSYM _)) -> Just (Reduce 1 184)
    (319, Token (BACKQUOTE _)) -> Just (Reduce 1 184)
    (320, Token (QCONID _)) -> Just (Shift 318)
    (320, Token (EXPORT _)) -> Just (Shift 353)
    (320, Token (AS _)) -> Just (Shift 354)
    (320, Token (QVARID _)) -> Just (Shift 355)
    (321, Token (QCONID _)) -> Just (Shift 318)
    (322, Token (RBRACE _)) -> Just (Reduce 3 185)
    (322, Token (LPAREN _)) -> Just (Reduce 3 185)
    (322, Token (RPAREN _)) -> Just (Reduce 3 185)
    (322, Token (COMMA _)) -> Just (Reduce 3 185)
    (322, Token (SEMICOLON _)) -> Just (Reduce 3 185)
    (322, Token (COLON_COLON _)) -> Just (Reduce 3 185)
    (322, Token (MINUS _)) -> Just (Reduce 3 185)
    (322, Token (RARROW _)) -> Just (Reduce 3 185)
    (322, Token (LBRACKET _)) -> Just (Reduce 3 185)
    (322, Token (RBRACKET _)) -> Just (Reduce 3 185)
    (322, Token (EXCL _)) -> Just (Reduce 3 185)
    (322, Token (QCONID _)) -> Just (Reduce 3 185)
    (322, Token (EXPORT _)) -> Just (Reduce 3 185)
    (322, Token (AS _)) -> Just (Reduce 3 185)
    (322, Token (QVARID _)) -> Just (Reduce 3 185)
    (322, Token (STRING _)) -> Just (Reduce 3 185)
    (322, Token (INTEGER _)) -> Just (Reduce 3 185)
    (322, Token (QVARSYM _)) -> Just (Reduce 3 185)
    (322, Token (QCONSYM _)) -> Just (Reduce 3 185)
    (322, Token (BACKQUOTE _)) -> Just (Reduce 3 185)
    (323, Token (RBRACE _)) -> Just (Reduce 3 130)
    (324, Token (RBRACE _)) -> Just (Reduce 1 129)
    (324, Token (COMMA _)) -> Just (Shift 70)
    (325, Token (RBRACE _)) -> Just (Reduce 3 131)
    (325, Token (COMMA _)) -> Just (Reduce 3 131)
    (326, Token (COLON_COLON _)) -> Just (Shift 115)
    (327, Token (EXPORT _)) -> Just (Reduce 1 139)
    (327, Token (AS _)) -> Just (Reduce 1 139)
    (327, Token (QVARID _)) -> Just (Reduce 1 139)
    (327, Token (STRING _)) -> Just (Reduce 1 139)
    (328, Token (EXPORT _)) -> Just (Reduce 1 138)
    (328, Token (AS _)) -> Just (Reduce 1 138)
    (328, Token (QVARID _)) -> Just (Reduce 1 138)
    (328, Token (STRING _)) -> Just (Reduce 1 138)
    (329, Token (EXPORT _)) -> Just (Reduce 1 140)
    (329, Token (AS _)) -> Just (Reduce 1 140)
    (329, Token (QVARID _)) -> Just (Reduce 1 140)
    (329, Token (STRING _)) -> Just (Reduce 1 140)
    (330, Token (LPAREN _)) -> Just (Reduce 1 141)
    (330, Token (MINUS _)) -> Just (Reduce 1 141)
    (330, Token (EXPORT _)) -> Just (Reduce 1 141)
    (330, Token (AS _)) -> Just (Reduce 1 141)
    (330, Token (QVARID _)) -> Just (Reduce 1 141)
    (330, Token (QVARSYM _)) -> Just (Reduce 1 141)
    (331, Token (STRING _)) -> Just (Reduce 1 144)
    (332, Token (STRING _)) -> Just (Reduce 1 143)
    (333, Token (STRING _)) -> Just (Reduce 1 145)
    (334, Token (LPAREN _)) -> Just (Reduce 1 142)
    (334, Token (MINUS _)) -> Just (Reduce 1 142)
    (334, Token (EXPORT _)) -> Just (Reduce 1 142)
    (334, Token (AS _)) -> Just (Reduce 1 142)
    (334, Token (QVARID _)) -> Just (Reduce 1 142)
    (334, Token (QVARSYM _)) -> Just (Reduce 1 142)
    (335, Token (EQUAL _)) -> Just (Reduce 3 149)
    (336, Token (COMMA _)) -> Just (Shift 56)
    (336, Token (EQUAL _)) -> Just (Reduce 1 148)
    (337, Token (COMMA _)) -> Just (Reduce 2 151)
    (337, Token (EQUAL _)) -> Just (Reduce 2 151)
    (338, Token (COMMA _)) -> Just (Reduce 3 150)
    (338, Token (EQUAL _)) -> Just (Reduce 3 150)
    (339, Token (COMMA _)) -> Just (Reduce 1 152)
    (339, Token (EQUAL _)) -> Just (Reduce 1 152)
    (339, Token (LARROW _)) -> Just (Shift 57)
    (340, Token (WHERE _)) -> Just (Reduce 1 156)
    (340, Token (RBRACE _)) -> Just (Reduce 1 156)
    (340, Token (RPAREN _)) -> Just (Reduce 1 156)
    (340, Token (COMMA _)) -> Just (Reduce 1 156)
    (340, Token (SEMICOLON _)) -> Just (Reduce 1 156)
    (340, Token (EQUAL _)) -> Just (Reduce 1 156)
    (340, Token (PIPE _)) -> Just (Reduce 1 156)
    (340, Token (COLON_COLON _)) -> Just (Reduce 1 156)
    (340, Token (LARROW _)) -> Just (Reduce 1 156)
    (341, Token (WHERE _)) -> Just (Reduce 2 157)
    (341, Token (RBRACE _)) -> Just (Reduce 2 157)
    (341, Token (RPAREN _)) -> Just (Reduce 2 157)
    (341, Token (COMMA _)) -> Just (Reduce 2 157)
    (341, Token (SEMICOLON _)) -> Just (Reduce 2 157)
    (341, Token (EQUAL _)) -> Just (Reduce 2 157)
    (341, Token (PIPE _)) -> Just (Reduce 2 157)
    (341, Token (COLON_COLON _)) -> Just (Reduce 2 157)
    (341, Token (LARROW _)) -> Just (Reduce 2 157)
    (342, Token (WHERE _)) -> Just (Reduce 1 159)
    (342, Token (RBRACE _)) -> Just (Reduce 1 159)
    (342, Token (LPAREN _)) -> Just (Reduce 1 159)
    (342, Token (RPAREN _)) -> Just (Reduce 1 159)
    (342, Token (COMMA _)) -> Just (Reduce 1 159)
    (342, Token (SEMICOLON _)) -> Just (Reduce 1 159)
    (342, Token (EQUAL _)) -> Just (Reduce 1 159)
    (342, Token (PIPE _)) -> Just (Reduce 1 159)
    (342, Token (COLON_COLON _)) -> Just (Reduce 1 159)
    (342, Token (MINUS _)) -> Just (Reduce 1 159)
    (342, Token (QCONID _)) -> Just (Reduce 1 159)
    (342, Token (EXPORT _)) -> Just (Reduce 1 159)
    (342, Token (AS _)) -> Just (Reduce 1 159)
    (342, Token (QVARID _)) -> Just (Reduce 1 159)
    (342, Token (STRING _)) -> Just (Reduce 1 159)
    (342, Token (LARROW _)) -> Just (Reduce 1 159)
    (342, Token (INTEGER _)) -> Just (Reduce 1 159)
    (342, Token (QVARSYM _)) -> Just (Reduce 1 159)
    (342, Token (QCONSYM _)) -> Just (Reduce 1 159)
    (342, Token (BACKQUOTE _)) -> Just (Reduce 1 159)
    (343, Token (WHERE _)) -> Just (Reduce 3 161)
    (343, Token (RBRACE _)) -> Just (Reduce 3 161)
    (343, Token (LPAREN _)) -> Just (Reduce 3 161)
    (343, Token (RPAREN _)) -> Just (Reduce 3 161)
    (343, Token (COMMA _)) -> Just (Reduce 3 161)
    (343, Token (SEMICOLON _)) -> Just (Reduce 3 161)
    (343, Token (EQUAL _)) -> Just (Reduce 3 161)
    (343, Token (PIPE _)) -> Just (Reduce 3 161)
    (343, Token (COLON_COLON _)) -> Just (Reduce 3 161)
    (343, Token (MINUS _)) -> Just (Reduce 3 161)
    (343, Token (QCONID _)) -> Just (Reduce 3 161)
    (343, Token (EXPORT _)) -> Just (Reduce 3 161)
    (343, Token (AS _)) -> Just (Reduce 3 161)
    (343, Token (QVARID _)) -> Just (Reduce 3 161)
    (343, Token (STRING _)) -> Just (Reduce 3 161)
    (343, Token (LARROW _)) -> Just (Reduce 3 161)
    (343, Token (INTEGER _)) -> Just (Reduce 3 161)
    (343, Token (QVARSYM _)) -> Just (Reduce 3 161)
    (343, Token (QCONSYM _)) -> Just (Reduce 3 161)
    (343, Token (BACKQUOTE _)) -> Just (Reduce 3 161)
    (344, Token (WHERE _)) -> Just (Reduce 3 162)
    (344, Token (RBRACE _)) -> Just (Reduce 3 162)
    (344, Token (LPAREN _)) -> Just (Reduce 3 162)
    (344, Token (RPAREN _)) -> Just (Reduce 3 162)
    (344, Token (COMMA _)) -> Just (Reduce 3 162)
    (344, Token (SEMICOLON _)) -> Just (Reduce 3 162)
    (344, Token (EQUAL _)) -> Just (Reduce 3 162)
    (344, Token (PIPE _)) -> Just (Reduce 3 162)
    (344, Token (COLON_COLON _)) -> Just (Reduce 3 162)
    (344, Token (MINUS _)) -> Just (Reduce 3 162)
    (344, Token (QCONID _)) -> Just (Reduce 3 162)
    (344, Token (EXPORT _)) -> Just (Reduce 3 162)
    (344, Token (AS _)) -> Just (Reduce 3 162)
    (344, Token (QVARID _)) -> Just (Reduce 3 162)
    (344, Token (STRING _)) -> Just (Reduce 3 162)
    (344, Token (LARROW _)) -> Just (Reduce 3 162)
    (344, Token (INTEGER _)) -> Just (Reduce 3 162)
    (344, Token (QVARSYM _)) -> Just (Reduce 3 162)
    (344, Token (QCONSYM _)) -> Just (Reduce 3 162)
    (344, Token (BACKQUOTE _)) -> Just (Reduce 3 162)
    (345, Token (WHERE _)) -> Just (Reduce 2 160)
    (345, Token (RBRACE _)) -> Just (Reduce 2 160)
    (345, Token (LPAREN _)) -> Just (Reduce 2 160)
    (345, Token (RPAREN _)) -> Just (Reduce 2 160)
    (345, Token (COMMA _)) -> Just (Reduce 2 160)
    (345, Token (SEMICOLON _)) -> Just (Reduce 2 160)
    (345, Token (EQUAL _)) -> Just (Reduce 2 160)
    (345, Token (PIPE _)) -> Just (Reduce 2 160)
    (345, Token (COLON_COLON _)) -> Just (Reduce 2 160)
    (345, Token (MINUS _)) -> Just (Reduce 2 160)
    (345, Token (QCONID _)) -> Just (Reduce 2 160)
    (345, Token (EXPORT _)) -> Just (Reduce 2 160)
    (345, Token (AS _)) -> Just (Reduce 2 160)
    (345, Token (QVARID _)) -> Just (Reduce 2 160)
    (345, Token (STRING _)) -> Just (Reduce 2 160)
    (345, Token (LARROW _)) -> Just (Reduce 2 160)
    (345, Token (INTEGER _)) -> Just (Reduce 2 160)
    (345, Token (QVARSYM _)) -> Just (Reduce 2 160)
    (345, Token (QCONSYM _)) -> Just (Reduce 2 160)
    (345, Token (BACKQUOTE _)) -> Just (Reduce 2 160)
    (346, Token (WHERE _)) -> Just (Reduce 3 166)
    (346, Token (RBRACE _)) -> Just (Reduce 3 166)
    (346, Token (LPAREN _)) -> Just (Reduce 3 166)
    (346, Token (RPAREN _)) -> Just (Reduce 3 166)
    (346, Token (COMMA _)) -> Just (Reduce 3 166)
    (346, Token (SEMICOLON _)) -> Just (Reduce 3 166)
    (346, Token (EQUAL _)) -> Just (Reduce 3 166)
    (346, Token (PIPE _)) -> Just (Reduce 3 166)
    (346, Token (COLON_COLON _)) -> Just (Reduce 3 166)
    (346, Token (MINUS _)) -> Just (Reduce 3 166)
    (346, Token (QCONID _)) -> Just (Reduce 3 166)
    (346, Token (EXPORT _)) -> Just (Reduce 3 166)
    (346, Token (AS _)) -> Just (Reduce 3 166)
    (346, Token (QVARID _)) -> Just (Reduce 3 166)
    (346, Token (STRING _)) -> Just (Reduce 3 166)
    (346, Token (LARROW _)) -> Just (Reduce 3 166)
    (346, Token (INTEGER _)) -> Just (Reduce 3 166)
    (346, Token (QVARSYM _)) -> Just (Reduce 3 166)
    (346, Token (QCONSYM _)) -> Just (Reduce 3 166)
    (346, Token (BACKQUOTE _)) -> Just (Reduce 3 166)
    (347, Token (WHERE _)) -> Just (Reduce 1 165)
    (347, Token (RBRACE _)) -> Just (Reduce 1 165)
    (347, Token (LPAREN _)) -> Just (Reduce 1 165)
    (347, Token (RPAREN _)) -> Just (Reduce 1 165)
    (347, Token (COMMA _)) -> Just (Reduce 1 165)
    (347, Token (SEMICOLON _)) -> Just (Reduce 1 165)
    (347, Token (EQUAL _)) -> Just (Reduce 1 165)
    (347, Token (PIPE _)) -> Just (Reduce 1 165)
    (347, Token (COLON_COLON _)) -> Just (Reduce 1 165)
    (347, Token (MINUS _)) -> Just (Reduce 1 165)
    (347, Token (QCONID _)) -> Just (Reduce 1 165)
    (347, Token (EXPORT _)) -> Just (Reduce 1 165)
    (347, Token (AS _)) -> Just (Reduce 1 165)
    (347, Token (QVARID _)) -> Just (Reduce 1 165)
    (347, Token (STRING _)) -> Just (Reduce 1 165)
    (347, Token (LARROW _)) -> Just (Reduce 1 165)
    (347, Token (INTEGER _)) -> Just (Reduce 1 165)
    (347, Token (QVARSYM _)) -> Just (Reduce 1 165)
    (347, Token (QCONSYM _)) -> Just (Reduce 1 165)
    (347, Token (BACKQUOTE _)) -> Just (Reduce 1 165)
    (348, Token (WHERE _)) -> Just (Reduce 1 164)
    (348, Token (RBRACE _)) -> Just (Reduce 1 164)
    (348, Token (LPAREN _)) -> Just (Reduce 1 164)
    (348, Token (RPAREN _)) -> Just (Reduce 1 164)
    (348, Token (COMMA _)) -> Just (Reduce 1 164)
    (348, Token (SEMICOLON _)) -> Just (Reduce 1 164)
    (348, Token (EQUAL _)) -> Just (Reduce 1 164)
    (348, Token (PIPE _)) -> Just (Reduce 1 164)
    (348, Token (COLON_COLON _)) -> Just (Reduce 1 164)
    (348, Token (MINUS _)) -> Just (Reduce 1 164)
    (348, Token (QCONID _)) -> Just (Reduce 1 164)
    (348, Token (EXPORT _)) -> Just (Reduce 1 164)
    (348, Token (AS _)) -> Just (Reduce 1 164)
    (348, Token (QVARID _)) -> Just (Reduce 1 164)
    (348, Token (STRING _)) -> Just (Reduce 1 164)
    (348, Token (LARROW _)) -> Just (Reduce 1 164)
    (348, Token (INTEGER _)) -> Just (Reduce 1 164)
    (348, Token (QVARSYM _)) -> Just (Reduce 1 164)
    (348, Token (QCONSYM _)) -> Just (Reduce 1 164)
    (348, Token (BACKQUOTE _)) -> Just (Reduce 1 164)
    (349, Token (WHERE _)) -> Just (Reduce 1 163)
    (349, Token (RBRACE _)) -> Just (Reduce 1 163)
    (349, Token (LPAREN _)) -> Just (Reduce 1 163)
    (349, Token (RPAREN _)) -> Just (Reduce 1 163)
    (349, Token (COMMA _)) -> Just (Reduce 1 163)
    (349, Token (SEMICOLON _)) -> Just (Reduce 1 163)
    (349, Token (EQUAL _)) -> Just (Reduce 1 163)
    (349, Token (PIPE _)) -> Just (Reduce 1 163)
    (349, Token (COLON_COLON _)) -> Just (Reduce 1 163)
    (349, Token (MINUS _)) -> Just (Reduce 1 163)
    (349, Token (QCONID _)) -> Just (Reduce 1 163)
    (349, Token (EXPORT _)) -> Just (Reduce 1 163)
    (349, Token (AS _)) -> Just (Reduce 1 163)
    (349, Token (QVARID _)) -> Just (Reduce 1 163)
    (349, Token (STRING _)) -> Just (Reduce 1 163)
    (349, Token (LARROW _)) -> Just (Reduce 1 163)
    (349, Token (INTEGER _)) -> Just (Reduce 1 163)
    (349, Token (QVARSYM _)) -> Just (Reduce 1 163)
    (349, Token (QCONSYM _)) -> Just (Reduce 1 163)
    (349, Token (BACKQUOTE _)) -> Just (Reduce 1 163)
    (350, Token (RPAREN _)) -> Just (Shift 346)
    (351, Token (LPAREN _)) -> Just (Reduce 3 172)
    (351, Token (RPAREN _)) -> Just (Reduce 3 172)
    (351, Token (EQUAL _)) -> Just (Reduce 3 172)
    (351, Token (PIPE _)) -> Just (Reduce 3 172)
    (351, Token (MINUS _)) -> Just (Reduce 3 172)
    (351, Token (QCONID _)) -> Just (Reduce 3 172)
    (351, Token (EXPORT _)) -> Just (Reduce 3 172)
    (351, Token (AS _)) -> Just (Reduce 3 172)
    (351, Token (QVARID _)) -> Just (Reduce 3 172)
    (351, Token (QVARSYM _)) -> Just (Reduce 3 172)
    (351, Token (QCONSYM _)) -> Just (Reduce 3 172)
    (351, Token (BACKQUOTE _)) -> Just (Reduce 3 172)
    (352, Token (LPAREN _)) -> Just (Reduce 1 171)
    (352, Token (RPAREN _)) -> Just (Reduce 1 171)
    (352, Token (EQUAL _)) -> Just (Reduce 1 171)
    (352, Token (PIPE _)) -> Just (Reduce 1 171)
    (352, Token (MINUS _)) -> Just (Reduce 1 171)
    (352, Token (QCONID _)) -> Just (Reduce 1 171)
    (352, Token (EXPORT _)) -> Just (Reduce 1 171)
    (352, Token (AS _)) -> Just (Reduce 1 171)
    (352, Token (QVARID _)) -> Just (Reduce 1 171)
    (352, Token (QVARSYM _)) -> Just (Reduce 1 171)
    (352, Token (QCONSYM _)) -> Just (Reduce 1 171)
    (352, Token (BACKQUOTE _)) -> Just (Reduce 1 171)
    (353, Token (BACKQUOTE _)) -> Just (Shift 357)
    (354, Token (BACKQUOTE _)) -> Just (Shift 358)
    (355, Token (BACKQUOTE _)) -> Just (Shift 359)
    (356, Token (RBRACE _)) -> Just (Reduce 1 180)
    (356, Token (LPAREN _)) -> Just (Reduce 1 180)
    (356, Token (COMMA _)) -> Just (Reduce 1 180)
    (356, Token (SEMICOLON _)) -> Just (Reduce 1 180)
    (356, Token (COLON_COLON _)) -> Just (Reduce 1 180)
    (356, Token (MINUS _)) -> Just (Reduce 1 180)
    (356, Token (QCONID _)) -> Just (Reduce 1 180)
    (356, Token (EXPORT _)) -> Just (Reduce 1 180)
    (356, Token (AS _)) -> Just (Reduce 1 180)
    (356, Token (QVARID _)) -> Just (Reduce 1 180)
    (356, Token (STRING _)) -> Just (Reduce 1 180)
    (356, Token (INTEGER _)) -> Just (Reduce 1 180)
    (356, Token (QVARSYM _)) -> Just (Reduce 1 180)
    (356, Token (QCONSYM _)) -> Just (Reduce 1 180)
    (356, Token (BACKQUOTE _)) -> Just (Reduce 1 180)
    (357, Token (RBRACE _)) -> Just (Reduce 3 182)
    (357, Token (LPAREN _)) -> Just (Reduce 3 182)
    (357, Token (COMMA _)) -> Just (Reduce 3 182)
    (357, Token (SEMICOLON _)) -> Just (Reduce 3 182)
    (357, Token (COLON_COLON _)) -> Just (Reduce 3 182)
    (357, Token (MINUS _)) -> Just (Reduce 3 182)
    (357, Token (QCONID _)) -> Just (Reduce 3 182)
    (357, Token (EXPORT _)) -> Just (Reduce 3 182)
    (357, Token (AS _)) -> Just (Reduce 3 182)
    (357, Token (QVARID _)) -> Just (Reduce 3 182)
    (357, Token (STRING _)) -> Just (Reduce 3 182)
    (357, Token (INTEGER _)) -> Just (Reduce 3 182)
    (357, Token (QVARSYM _)) -> Just (Reduce 3 182)
    (357, Token (QCONSYM _)) -> Just (Reduce 3 182)
    (357, Token (BACKQUOTE _)) -> Just (Reduce 3 182)
    (358, Token (RBRACE _)) -> Just (Reduce 3 181)
    (358, Token (LPAREN _)) -> Just (Reduce 3 181)
    (358, Token (COMMA _)) -> Just (Reduce 3 181)
    (358, Token (SEMICOLON _)) -> Just (Reduce 3 181)
    (358, Token (COLON_COLON _)) -> Just (Reduce 3 181)
    (358, Token (MINUS _)) -> Just (Reduce 3 181)
    (358, Token (QCONID _)) -> Just (Reduce 3 181)
    (358, Token (EXPORT _)) -> Just (Reduce 3 181)
    (358, Token (AS _)) -> Just (Reduce 3 181)
    (358, Token (QVARID _)) -> Just (Reduce 3 181)
    (358, Token (STRING _)) -> Just (Reduce 3 181)
    (358, Token (INTEGER _)) -> Just (Reduce 3 181)
    (358, Token (QVARSYM _)) -> Just (Reduce 3 181)
    (358, Token (QCONSYM _)) -> Just (Reduce 3 181)
    (358, Token (BACKQUOTE _)) -> Just (Reduce 3 181)
    (359, Token (RBRACE _)) -> Just (Reduce 3 183)
    (359, Token (LPAREN _)) -> Just (Reduce 3 183)
    (359, Token (COMMA _)) -> Just (Reduce 3 183)
    (359, Token (SEMICOLON _)) -> Just (Reduce 3 183)
    (359, Token (COLON_COLON _)) -> Just (Reduce 3 183)
    (359, Token (MINUS _)) -> Just (Reduce 3 183)
    (359, Token (QCONID _)) -> Just (Reduce 3 183)
    (359, Token (EXPORT _)) -> Just (Reduce 3 183)
    (359, Token (AS _)) -> Just (Reduce 3 183)
    (359, Token (QVARID _)) -> Just (Reduce 3 183)
    (359, Token (STRING _)) -> Just (Reduce 3 183)
    (359, Token (INTEGER _)) -> Just (Reduce 3 183)
    (359, Token (QVARSYM _)) -> Just (Reduce 3 183)
    (359, Token (QCONSYM _)) -> Just (Reduce 3 183)
    (359, Token (BACKQUOTE _)) -> Just (Reduce 3 183)
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
production 156 = 61
production 157 = 62
production 158 = 62
production 159 = 63
production 160 = 63
production 161 = 63
production 162 = 63
production 163 = 64
production 164 = 64
production 165 = 64
production 166 = 64
production 167 = 31
production 168 = 31
production 169 = 31
production 170 = 31
production 171 = 65
production 172 = 65
production 173 = 8
production 174 = 8
production 175 = 8
production 176 = 8
production 177 = 8
production 178 = 9
production 179 = 9
production 180 = 66
production 181 = 66
production 182 = 66
production 183 = 66
production 184 = 52
production 185 = 52
production 186 = 44
production 187 = 44
production 188 = 16
production 189 = 16
production 190 = 15
production 191 = 15
production 192 = 47
production 193 = 47
production 194 = 47
production 195 = 67
production 196 = 1
production 197 = 42
production 198 = 42

dfaGotoTransition :: GotoState -> GotoSymbol -> Maybe GotoState
dfaGotoTransition q s =
  case (q, production s) of
    (0, 0) -> Just 1
    (0, 3) -> Just 6
    (2, 1) -> Just 4
    (3, 3) -> Just 7
    (4, 2) -> Just 5
    (4, 5) -> Just 12
    (8, 1) -> Just 170
    (9, 1) -> Just 195
    (10, 1) -> Just 30
    (13, 4) -> Just 15
    (13, 8) -> Just 275
    (13, 14) -> Just 18
    (13, 27) -> Just 193
    (13, 30) -> Just 229
    (13, 31) -> Just 58
    (13, 40) -> Just 243
    (13, 41) -> Just 244
    (13, 65) -> Just 247
    (16, 4) -> Just 17
    (16, 8) -> Just 275
    (16, 14) -> Just 18
    (16, 27) -> Just 193
    (16, 30) -> Just 229
    (16, 31) -> Just 58
    (16, 40) -> Just 243
    (16, 41) -> Just 244
    (16, 65) -> Just 247
    (19, 6) -> Just 21
    (19, 7) -> Just 24
    (19, 8) -> Just 31
    (19, 9) -> Just 32
    (22, 6) -> Just 23
    (22, 7) -> Just 24
    (22, 8) -> Just 31
    (22, 9) -> Just 32
    (25, 8) -> Just 144
    (25, 9) -> Just 145
    (25, 10) -> Just 33
    (25, 13) -> Just 134
    (34, 8) -> Just 349
    (34, 44) -> Just 37
    (34, 52) -> Just 287
    (34, 64) -> Just 345
    (34, 66) -> Just 288
    (35, 8) -> Just 349
    (35, 62) -> Just 341
    (35, 63) -> Just 34
    (35, 64) -> Just 342
    (36, 8) -> Just 349
    (36, 64) -> Just 343
    (37, 8) -> Just 349
    (37, 64) -> Just 344
    (38, 8) -> Just 349
    (38, 32) -> Just 254
    (38, 61) -> Just 253
    (38, 62) -> Just 340
    (38, 63) -> Just 34
    (38, 64) -> Just 342
    (39, 8) -> Just 349
    (39, 32) -> Just 230
    (39, 61) -> Just 253
    (39, 62) -> Just 340
    (39, 63) -> Just 34
    (39, 64) -> Just 342
    (40, 8) -> Just 349
    (40, 32) -> Just 264
    (40, 61) -> Just 253
    (40, 62) -> Just 340
    (40, 63) -> Just 34
    (40, 64) -> Just 342
    (41, 8) -> Just 349
    (41, 32) -> Just 272
    (41, 61) -> Just 253
    (41, 62) -> Just 340
    (41, 63) -> Just 34
    (41, 64) -> Just 342
    (42, 8) -> Just 352
    (42, 65) -> Just 248
    (43, 8) -> Just 352
    (43, 65) -> Just 250
    (44, 8) -> Just 349
    (44, 32) -> Just 350
    (44, 61) -> Just 253
    (44, 62) -> Just 340
    (44, 63) -> Just 34
    (44, 64) -> Just 342
    (45, 8) -> Just 349
    (45, 62) -> Just 341
    (45, 63) -> Just 34
    (45, 64) -> Just 342
    (46, 8) -> Just 352
    (46, 31) -> Just 47
    (46, 65) -> Just 247
    (47, 8) -> Just 352
    (47, 44) -> Just 43
    (47, 52) -> Just 287
    (47, 65) -> Just 249
    (47, 66) -> Just 288
    (48, 8) -> Just 275
    (48, 27) -> Just 239
    (48, 29) -> Just 238
    (48, 30) -> Just 229
    (48, 31) -> Just 58
    (48, 40) -> Just 243
    (48, 41) -> Just 244
    (48, 65) -> Just 247
    (49, 8) -> Just 275
    (49, 27) -> Just 239
    (49, 29) -> Just 240
    (49, 30) -> Just 229
    (49, 31) -> Just 58
    (49, 40) -> Just 243
    (49, 41) -> Just 244
    (49, 65) -> Just 247
    (50, 8) -> Just 275
    (50, 30) -> Just 263
    (50, 31) -> Just 61
    (50, 35) -> Just 258
    (50, 36) -> Just 260
    (50, 40) -> Just 243
    (50, 41) -> Just 244
    (50, 65) -> Just 247
    (51, 8) -> Just 275
    (51, 30) -> Just 263
    (51, 31) -> Just 61
    (51, 35) -> Just 259
    (51, 36) -> Just 260
    (51, 40) -> Just 243
    (51, 41) -> Just 244
    (51, 65) -> Just 247
    (52, 8) -> Just 349
    (52, 33) -> Just 231
    (52, 59) -> Just 256
    (52, 60) -> Just 336
    (52, 61) -> Just 339
    (52, 62) -> Just 340
    (52, 63) -> Just 34
    (52, 64) -> Just 342
    (53, 8) -> Just 349
    (53, 33) -> Just 255
    (53, 59) -> Just 256
    (53, 60) -> Just 336
    (53, 61) -> Just 339
    (53, 62) -> Just 340
    (53, 63) -> Just 34
    (53, 64) -> Just 342
    (54, 8) -> Just 349
    (54, 33) -> Just 265
    (54, 59) -> Just 256
    (54, 60) -> Just 336
    (54, 61) -> Just 339
    (54, 62) -> Just 340
    (54, 63) -> Just 34
    (54, 64) -> Just 342
    (55, 8) -> Just 349
    (55, 33) -> Just 273
    (55, 59) -> Just 256
    (55, 60) -> Just 336
    (55, 61) -> Just 339
    (55, 62) -> Just 340
    (55, 63) -> Just 34
    (55, 64) -> Just 342
    (56, 8) -> Just 349
    (56, 59) -> Just 335
    (56, 60) -> Just 336
    (56, 61) -> Just 339
    (56, 62) -> Just 340
    (56, 63) -> Just 34
    (56, 64) -> Just 342
    (57, 8) -> Just 349
    (57, 32) -> Just 338
    (57, 61) -> Just 253
    (57, 62) -> Just 340
    (57, 63) -> Just 34
    (57, 64) -> Just 342
    (58, 8) -> Just 352
    (58, 44) -> Just 43
    (58, 52) -> Just 287
    (58, 65) -> Just 249
    (58, 66) -> Just 288
    (59, 8) -> Just 352
    (59, 31) -> Just 62
    (59, 38) -> Just 267
    (59, 39) -> Just 269
    (59, 65) -> Just 247
    (60, 8) -> Just 352
    (60, 31) -> Just 62
    (60, 38) -> Just 268
    (60, 39) -> Just 269
    (60, 65) -> Just 247
    (61, 8) -> Just 352
    (61, 44) -> Just 43
    (61, 52) -> Just 287
    (61, 65) -> Just 249
    (61, 66) -> Just 288
    (62, 8) -> Just 352
    (62, 44) -> Just 43
    (62, 52) -> Just 287
    (62, 65) -> Just 249
    (62, 66) -> Just 288
    (63, 8) -> Just 141
    (63, 9) -> Just 142
    (63, 11) -> Just 135
    (63, 12) -> Just 136
    (64, 8) -> Just 141
    (64, 9) -> Just 142
    (64, 11) -> Just 171
    (64, 12) -> Just 136
    (65, 8) -> Just 141
    (65, 9) -> Just 142
    (65, 11) -> Just 172
    (65, 12) -> Just 136
    (66, 8) -> Just 144
    (66, 9) -> Just 145
    (66, 10) -> Just 133
    (66, 13) -> Just 134
    (67, 8) -> Just 144
    (67, 9) -> Just 145
    (67, 10) -> Just 143
    (67, 13) -> Just 134
    (68, 8) -> Just 274
    (68, 40) -> Just 276
    (69, 8) -> Just 274
    (69, 40) -> Just 326
    (69, 53) -> Just 317
    (69, 54) -> Just 324
    (70, 8) -> Just 274
    (70, 40) -> Just 326
    (70, 53) -> Just 323
    (70, 54) -> Just 324
    (71, 8) -> Just 205
    (72, 8) -> Just 216
    (73, 8) -> Just 217
    (74, 8) -> Just 218
    (84, 9) -> Just 303
    (84, 45) -> Just 294
    (84, 46) -> Just 295
    (84, 47) -> Just 296
    (85, 9) -> Just 303
    (85, 17) -> Just 86
    (85, 45) -> Just 196
    (85, 46) -> Just 295
    (85, 47) -> Just 296
    (86, 9) -> Just 303
    (86, 23) -> Just 188
    (86, 45) -> Just 197
    (86, 46) -> Just 295
    (86, 47) -> Just 296
    (87, 9) -> Just 303
    (87, 17) -> Just 88
    (87, 45) -> Just 196
    (87, 46) -> Just 295
    (87, 47) -> Just 296
    (88, 9) -> Just 303
    (88, 24) -> Just 190
    (88, 45) -> Just 197
    (88, 46) -> Just 295
    (88, 47) -> Just 296
    (89, 9) -> Just 303
    (89, 17) -> Just 90
    (89, 45) -> Just 196
    (89, 46) -> Just 295
    (89, 47) -> Just 296
    (90, 9) -> Just 303
    (90, 23) -> Just 187
    (90, 45) -> Just 197
    (90, 46) -> Just 295
    (90, 47) -> Just 296
    (91, 9) -> Just 303
    (91, 17) -> Just 92
    (91, 45) -> Just 196
    (91, 46) -> Just 295
    (91, 47) -> Just 296
    (92, 9) -> Just 303
    (92, 24) -> Just 189
    (92, 45) -> Just 197
    (92, 46) -> Just 295
    (92, 47) -> Just 296
    (93, 9) -> Just 303
    (93, 17) -> Just 94
    (93, 18) -> Just 252
    (93, 45) -> Just 196
    (93, 46) -> Just 295
    (93, 47) -> Just 296
    (94, 9) -> Just 303
    (94, 45) -> Just 197
    (94, 46) -> Just 295
    (94, 47) -> Just 296
    (95, 9) -> Just 303
    (95, 17) -> Just 97
    (95, 18) -> Just 198
    (95, 45) -> Just 196
    (95, 46) -> Just 295
    (95, 47) -> Just 296
    (96, 9) -> Just 303
    (96, 17) -> Just 97
    (96, 18) -> Just 251
    (96, 45) -> Just 196
    (96, 46) -> Just 295
    (96, 47) -> Just 296
    (97, 9) -> Just 303
    (97, 45) -> Just 197
    (97, 46) -> Just 295
    (97, 47) -> Just 296
    (98, 9) -> Just 303
    (98, 17) -> Just 99
    (98, 45) -> Just 196
    (98, 46) -> Just 295
    (98, 47) -> Just 296
    (99, 9) -> Just 303
    (99, 19) -> Just 175
    (99, 45) -> Just 197
    (99, 46) -> Just 295
    (99, 47) -> Just 296
    (100, 9) -> Just 303
    (100, 17) -> Just 101
    (100, 45) -> Just 196
    (100, 46) -> Just 295
    (100, 47) -> Just 296
    (101, 9) -> Just 303
    (101, 19) -> Just 176
    (101, 45) -> Just 197
    (101, 46) -> Just 295
    (101, 47) -> Just 296
    (102, 9) -> Just 304
    (102, 17) -> Just 105
    (102, 45) -> Just 196
    (102, 46) -> Just 295
    (102, 47) -> Just 296
    (102, 50) -> Just 199
    (102, 51) -> Just 314
    (103, 9) -> Just 304
    (103, 17) -> Just 105
    (103, 45) -> Just 196
    (103, 46) -> Just 295
    (103, 47) -> Just 296
    (103, 50) -> Just 313
    (103, 51) -> Just 314
    (104, 9) -> Just 117
    (105, 9) -> Just 303
    (105, 45) -> Just 197
    (105, 46) -> Just 295
    (105, 47) -> Just 296
    (105, 52) -> Just 106
    (106, 9) -> Just 303
    (106, 17) -> Just 107
    (106, 45) -> Just 196
    (106, 46) -> Just 295
    (106, 47) -> Just 296
    (107, 9) -> Just 303
    (107, 45) -> Just 197
    (107, 46) -> Just 295
    (107, 47) -> Just 296
    (108, 9) -> Just 303
    (108, 17) -> Just 109
    (108, 18) -> Just 242
    (108, 45) -> Just 196
    (108, 46) -> Just 295
    (108, 47) -> Just 296
    (109, 9) -> Just 303
    (109, 45) -> Just 197
    (109, 46) -> Just 295
    (109, 47) -> Just 296
    (110, 9) -> Just 303
    (110, 17) -> Just 97
    (110, 18) -> Just 174
    (110, 45) -> Just 196
    (110, 46) -> Just 295
    (110, 47) -> Just 296
    (111, 9) -> Just 303
    (111, 17) -> Just 97
    (111, 18) -> Just 219
    (111, 45) -> Just 196
    (111, 46) -> Just 295
    (111, 47) -> Just 296
    (112, 9) -> Just 303
    (112, 17) -> Just 97
    (112, 18) -> Just 220
    (112, 45) -> Just 196
    (112, 46) -> Just 295
    (112, 47) -> Just 296
    (113, 9) -> Just 303
    (113, 17) -> Just 97
    (113, 18) -> Just 221
    (113, 45) -> Just 196
    (113, 46) -> Just 295
    (113, 47) -> Just 296
    (114, 9) -> Just 303
    (114, 17) -> Just 97
    (114, 18) -> Just 241
    (114, 45) -> Just 196
    (114, 46) -> Just 295
    (114, 47) -> Just 296
    (115, 9) -> Just 303
    (115, 17) -> Just 97
    (115, 18) -> Just 325
    (115, 45) -> Just 196
    (115, 46) -> Just 295
    (115, 47) -> Just 296
    (116, 9) -> Just 303
    (116, 17) -> Just 97
    (116, 18) -> Just 206
    (116, 45) -> Just 196
    (116, 46) -> Just 295
    (116, 47) -> Just 296
    (117, 9) -> Just 303
    (117, 45) -> Just 207
    (117, 46) -> Just 295
    (117, 47) -> Just 296
    (118, 9) -> Just 303
    (118, 17) -> Just 119
    (118, 45) -> Just 196
    (118, 46) -> Just 295
    (118, 47) -> Just 296
    (119, 9) -> Just 303
    (119, 22) -> Just 186
    (119, 45) -> Just 197
    (119, 46) -> Just 295
    (119, 47) -> Just 296
    (120, 9) -> Just 303
    (120, 17) -> Just 122
    (120, 45) -> Just 196
    (120, 46) -> Just 295
    (120, 47) -> Just 296
    (121, 9) -> Just 303
    (121, 17) -> Just 123
    (121, 45) -> Just 196
    (121, 46) -> Just 295
    (121, 47) -> Just 296
    (122, 9) -> Just 303
    (122, 45) -> Just 197
    (122, 46) -> Just 295
    (122, 47) -> Just 296
    (123, 9) -> Just 303
    (123, 22) -> Just 185
    (123, 45) -> Just 197
    (123, 46) -> Just 295
    (123, 47) -> Just 296
    (124, 9) -> Just 303
    (124, 17) -> Just 97
    (124, 18) -> Just 292
    (124, 45) -> Just 196
    (124, 46) -> Just 295
    (124, 47) -> Just 296
    (124, 48) -> Just 297
    (124, 49) -> Just 305
    (125, 9) -> Just 303
    (125, 17) -> Just 97
    (125, 18) -> Just 212
    (125, 25) -> Just 191
    (125, 45) -> Just 196
    (125, 46) -> Just 295
    (125, 47) -> Just 296
    (126, 9) -> Just 303
    (126, 17) -> Just 97
    (126, 18) -> Just 212
    (126, 25) -> Just 213
    (126, 45) -> Just 196
    (126, 46) -> Just 295
    (126, 47) -> Just 296
    (127, 9) -> Just 303
    (127, 17) -> Just 97
    (127, 18) -> Just 309
    (127, 45) -> Just 196
    (127, 46) -> Just 295
    (127, 47) -> Just 296
    (127, 48) -> Just 310
    (128, 9) -> Just 303
    (128, 17) -> Just 97
    (128, 18) -> Just 293
    (128, 45) -> Just 196
    (128, 46) -> Just 295
    (128, 47) -> Just 296
    (146, 20) -> Just 202
    (146, 21) -> Just 181
    (147, 20) -> Just 202
    (147, 21) -> Just 182
    (148, 20) -> Just 202
    (148, 21) -> Just 183
    (149, 20) -> Just 202
    (149, 21) -> Just 184
    (162, 15) -> Just 8
    (164, 20) -> Just 177
    (165, 20) -> Just 178
    (166, 20) -> Just 179
    (167, 20) -> Just 180
    (169, 26) -> Just 192
    (170, 16) -> Just 173
    (200, 20) -> Just 202
    (200, 21) -> Just 203
    (208, 34) -> Just 209
    (210, 37) -> Just 211
    (214, 55) -> Just 222
    (215, 55) -> Just 223
    (222, 56) -> Just 72
    (222, 57) -> Just 224
    (223, 58) -> Just 74
    (224, 56) -> Just 73
    (225, 28) -> Just 227
    (226, 28) -> Just 228
    (232, 28) -> Just 261
    (233, 28) -> Just 262
    (234, 28) -> Just 270
    (235, 28) -> Just 271
    (236, 28) -> Just 337
    (244, 42) -> Just 245
    (245, 43) -> Just 246
    (245, 44) -> Just 286
    (245, 52) -> Just 287
    (245, 66) -> Just 288
    (281, 43) -> Just 284
    (281, 44) -> Just 286
    (281, 52) -> Just 287
    (281, 66) -> Just 288
    (282, 43) -> Just 285
    (282, 44) -> Just 286
    (282, 52) -> Just 287
    (282, 66) -> Just 288
    (311, 49) -> Just 312
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
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_type' actions (case snd (pop !! 2) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    154 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp_COLON_COLON_btype_DARROW_type' actions (case snd (pop !! 4) of { StackValue_infixexp value -> value; _ -> undefined }) (case snd (pop !! 3) of { StackValue_COLON_COLON value -> value; _ -> undefined }) (case snd (pop !! 2) of { StackValue_btype value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_DARROW value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_type' value -> value; _ -> undefined })
                    155 ->
                      Monad.liftM StackValue_exp $ exp_implies_infixexp actions (case snd (pop !! 0) of { StackValue_infixexp value -> value; _ -> undefined })
                    156 ->
                      Monad.liftM StackValue_infixexp $ infixexp_implies_lexp actions (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    157 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_MINUS_lexp actions (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_lexp value -> value; _ -> undefined })
                    158 ->
                      Monad.liftM StackValue_lexp $ lexp_implies_fexp actions (case snd (pop !! 0) of { StackValue_fexp value -> value; _ -> undefined })
                    159 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_aexp actions (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    160 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_aexp actions (case snd (pop !! 1) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    161 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_MINUS_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    162 ->
                      Monad.liftM StackValue_fexp $ fexp_implies_fexp_op_aexp actions (case snd (pop !! 2) of { StackValue_fexp value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_aexp value -> value; _ -> undefined })
                    163 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    164 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_INTEGER actions (case snd (pop !! 0) of { StackValue_INTEGER value -> value; _ -> undefined })
                    165 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_STRING actions (case snd (pop !! 0) of { StackValue_STRING value -> value; _ -> undefined })
                    166 ->
                      Monad.liftM StackValue_aexp $ aexp_implies_LPAREN_exp_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_exp value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    167 ->
                      Monad.liftM StackValue_pat $ pat_implies_apat actions (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    168 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_apat actions (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    169 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_MINUS_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    170 ->
                      Monad.liftM StackValue_pat $ pat_implies_pat_op_apat actions (case snd (pop !! 2) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_op value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_apat value -> value; _ -> undefined })
                    171 ->
                      Monad.liftM StackValue_apat $ apat_implies_var actions (case snd (pop !! 0) of { StackValue_var value -> value; _ -> undefined })
                    172 ->
                      Monad.liftM StackValue_apat $ apat_implies_LPAREN_pat_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_pat value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    173 ->
                      Monad.liftM StackValue_var $ var_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    174 ->
                      Monad.liftM StackValue_var $ var_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    175 ->
                      Monad.liftM StackValue_var $ var_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    176 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_MINUS_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_MINUS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    177 ->
                      Monad.liftM StackValue_var $ var_implies_LPAREN_QVARSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    178 ->
                      Monad.liftM StackValue_con $ con_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    179 ->
                      Monad.liftM StackValue_con $ con_implies_LPAREN_QCONSYM_RPAREN actions (case snd (pop !! 2) of { StackValue_LPAREN value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONSYM value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_RPAREN value -> value; _ -> undefined })
                    180 ->
                      Monad.liftM StackValue_varop $ varop_implies_QVARSYM actions (case snd (pop !! 0) of { StackValue_QVARSYM value -> value; _ -> undefined })
                    181 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_AS_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    182 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_EXPORT_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_EXPORT value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    183 ->
                      Monad.liftM StackValue_varop $ varop_implies_BACKQUOTE_QVARID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QVARID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    184 ->
                      Monad.liftM StackValue_conop $ conop_implies_QCONSYM actions (case snd (pop !! 0) of { StackValue_QCONSYM value -> value; _ -> undefined })
                    185 ->
                      Monad.liftM StackValue_conop $ conop_implies_BACKQUOTE_QCONID_BACKQUOTE actions (case snd (pop !! 2) of { StackValue_BACKQUOTE value -> value; _ -> undefined }) (case snd (pop !! 1) of { StackValue_QCONID value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_BACKQUOTE value -> value; _ -> undefined })
                    186 ->
                      Monad.liftM StackValue_op $ op_implies_varop actions (case snd (pop !! 0) of { StackValue_varop value -> value; _ -> undefined })
                    187 ->
                      Monad.liftM StackValue_op $ op_implies_conop actions (case snd (pop !! 0) of { StackValue_conop value -> value; _ -> undefined })
                    188 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies actions
                    189 ->
                      Monad.liftM StackValue_as_opt $ as_opt_implies_AS_modid actions (case snd (pop !! 1) of { StackValue_AS value -> value; _ -> undefined }) (case snd (pop !! 0) of { StackValue_modid value -> value; _ -> undefined })
                    190 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies actions
                    191 ->
                      Monad.liftM StackValue_qualified_opt $ qualified_opt_implies_QUALIFIED actions (case snd (pop !! 0) of { StackValue_QUALIFIED value -> value; _ -> undefined })
                    192 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_AS actions (case snd (pop !! 0) of { StackValue_AS value -> value; _ -> undefined })
                    193 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_EXPORT actions (case snd (pop !! 0) of { StackValue_EXPORT value -> value; _ -> undefined })
                    194 ->
                      Monad.liftM StackValue_tyvar $ tyvar_implies_QVARID actions (case snd (pop !! 0) of { StackValue_QVARID value -> value; _ -> undefined })
                    195 ->
                      Monad.liftM StackValue_tycls $ tycls_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    196 ->
                      Monad.liftM StackValue_modid $ modid_implies_QCONID actions (case snd (pop !! 0) of { StackValue_QCONID value -> value; _ -> undefined })
                    197 ->
                      Monad.liftM StackValue_integer_opt $ integer_opt_implies actions
                    198 ->
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
      return $ Integer_opt_implies_INTEGER iNTEGER0 }

