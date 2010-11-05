import haxe.macro.Expr;
import haxe.macro.Context;

class Mac {
    public static function main() {
//#if macro
        TemplateLoader.template("Hey there");
        //TemplateLoader.template(switch(name.expr){case EConst(c):switch(c){case CString(n):n;default:}default:});
//#end
    }
}


//#if macro
@:macro class TemplateLoader {
    public static function template(name :Expr) :Expr{
        trace(name.expr);
        //var tname :String;
        var tname :String = switch(name.expr){case EConst(c):switch(c){case CString(n):n;default:}default:};
        trace(tname);
        return null;
    }

    static function load_template(name:String) : Expr {
        trace("W00t!  - "+name);
        return null;
    }


    public static function extract_string(inexpr :Expr) :Expr{
        var pos = inexpr.pos;

        ESwitch(
            {expr: EParenthesis(
                {expr: EField(
                    {expr: EConst(CIdent('name')), pos: pos}, 'expr'), pos: pos}), pos: pos},
            [{expr: {expr: EBlock([{expr: ESwitch( {expr:
                    EParenthesis( {expr: EConst(CIdent('c')), pos: pos}),
                    pos: pos},[{expr: {expr: EBlock([{expr:
                        EBinop(OpAssign(),{expr: EConst(CIdent(tname)),
                            pos: pos},{expr: EConst(CIdent('n')), pos: pos
                            }), pos: pos}]), pos: pos}, values: [{expr:
                    ECall( {expr: EConst(CType(CString)), pos: pos},[{
expr: EConst(CIdent('n')), pos: pos}]), pos: pos}] }],{expr: EBlock([]), pos:
                    pos}), pos: pos}]), pos: pos}, values: [{expr: ECall(
                    {expr: EConst(CType(EConst)), pos: pos},[{expr:
                    EConst(CIdent('c')), pos: pos}]), pos: pos}] }],{expr:
            EBlock([]), pos: pos})
    }
}
//#end

