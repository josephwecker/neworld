import haxe.macro.Expr;
import haxe.macro.Context;

class Mac {
    public static function main() {
        TemplateLoader.template("Hey there");
        //TemplateLoader.template(switch(name.expr){case EConst(c):switch(c){case CString(n):n;default:}default:});
    }
}


@:macro class TemplateLoader {
    public static function template(name :Expr) :Expr{
        var uname:String = extract_string(name.expr);
        trace(uname);
        return null;
      /*  var tname:String = switch(name.expr){case EConst(c):switch(c){case CString(n):n;default:}default:};
        trace(tname);
        return null;*/
    }

    static function load_template(name:String) : Expr {
        trace("W00t!  - "+name);
        return null;
    }


    // Simplifies extracting strings
    public static function extract_string(inexpr :Expr) :Expr{
        var p = inexpr.pos;
        var n = {expr: EConst(CIdent('n')), pos: p};
        var c = {expr: EConst(CIdent('c')), pos: p};

        return {expr: ESwitch({ expr: EParenthesis(inexpr), pos:p},[{ expr: {
            expr: EBlock([{ expr: ESwitch({ expr: EParenthesis(c), pos:p},[{
                expr: { expr: EBlock([n]), pos:p}, values: [{ expr: ECall({
                    expr: EConst(CType('CString')), pos:p},[n]), pos:p}] }],{
                        expr: EBlock([]), pos:p}), pos:p}]), pos:p}, values: [{
                            expr: ECall({ expr: EConst(CType('EConst')),
                            pos:p},[c]), pos:p}] }],{ expr: EBlock([]),
                            pos:p}), pos: p};
    }
}
