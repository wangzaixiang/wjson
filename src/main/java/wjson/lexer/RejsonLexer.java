package wjson.lexer;

import java.io.IOException;
import java.util.function.Function;

import wjson.parser.Program;

public class RejsonLexer implements Program.Lexer {

    private final CharSequence source;
    private final int length;
    private int cursor = -1;
    private char cursorChar;

    private static final char EOI = (char)-1;

    private Object last_val;


    public RejsonLexer(CharSequence source){
        this.source = source;
        this.length = source.length();
        this.cursorChar = nextChar();
    }

    @Override
    public Object getLVal() {
        return last_val;
    }

    @Override
    public int yylex() throws IOException {

        int  result = yylex_expr();
        // System.out.println("[" + result + "]");
        return result;
    }



    public int yylex_expr() throws IOException {
        last_val = null;
        ws();

        if(cursor == source.length()){
            cursor += 1;
            return YYEOF;
        }
        else if(cursor > source.length()){
            return YYEOF;
        }

        switch(cursorChar){
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                return number();
            case '-':   // ->
                advance();
                if(ch(ch -> ch >='0' && ch <='9')) {
                    cursor -= 2; cursorChar = source.charAt(cursor);
                    return number();
                }
                else return YYerror;
            case '_':
                advance();
                if(ch('*'))
                    return Anys;
                else return Any1;
            case '/':
                advance();
                return '/';
            case '[':
            case ']':
            case '{':
            case '}':
            case '(':
            case ')':
            case ',':
            case ':':
            case '@':
                char it = cursorChar;
                advance();
                return it;
            case '\'':
                return yylex_string('\'');
            case '"':
                return yylex_string('\"');

            default:
                if(cursorChar == '$' || cursorChar == '_' || (cursorChar >= 'a' && cursorChar <= 'z')
                        || (cursorChar >='A' && cursorChar <='Z') ) {
                    return yylex_identity();
                }
                else return YYerror;

        }
    }

    @Override
    public void yyerror(String msg) {
        System.err.println("yyerror:" + msg);
    }

    private void ws() {
//        while (((1L << cursorChar) & ((cursorChar - 64) >> 31) & 0x100002600L) != 0L) {
//            cursorChar = nextChar();
//        }
        while(ch(this::isWhiteSpace)) ;
    }

    private boolean isWhiteSpace(char ch){
        return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
    }

    private char nextChar() {
        cursor += 1;
        if (cursor < length)
            return source.charAt(cursor);
        else return EOI;
    }

    private char[] sliceCharArray(int start, int end) {
        char[] array = new char[end - start];
        for(int i = start; i < end; i++)
            array[i - start] = source.charAt(i);
        return array;
    }

    // TODO parse float number
    private int number(){
        int start = cursor;

        ch('-');
        boolean p1 = _int();
        boolean p2 = _frac();
        boolean p3 = _exp();

        String content = new String( sliceCharArray(start, cursor) );
        if(p2 == false && p3 == false) {
            // last_val = new LiteralInteger( new BigInteger(content));
            last_val = Double.parseDouble(content);
            return LiteralNumber;
        }
        else {
            // last_val = new LiteralDecimal( new BigDecimal(content));
            last_val = Double.parseDouble(content);
            return LiteralNumber;
        }

    }
    private void oneOrMoreDigits() {
        if (digit()) zeroOrMoreDigits();
        else throw fail("DIGIT");
    }
    private boolean digit() {
        return cursorChar >= '0' && cursorChar <= '9' && advance();
    }
    private void zeroOrMoreDigits() {
        while (digit()) {
        }
    }
    private boolean _int() {
        if (!ch('0')) oneOrMoreDigits();
        return true;
    }
    private boolean _frac(){
        if(ch('.')) {
            oneOrMoreDigits();
            return true;
        }
        else return false;
    }
    private boolean _exp() {
        if(ch('e')||ch('E')){
            ch( ch -> ch == '-' || ch == '+');
            oneOrMoreDigits();
            return true;
        }
        else return false;
    }

    private int yylex_identity(){
        int start = cursor;

        if(identityHead()) {
            while(identityTail()) ;
        }
        if(cursorChar == '"'){
            String tag = new String( sliceCharArray(start, cursor) );
            int start2 = cursor;
            yylex_string('\"');
            String c = new String(sliceCharArray(start2, cursor));
            last_val = new String[]{tag, c};
            return TagedString;
        }

        String content = new String(sliceCharArray(start, cursor));
        if("null".equals(content)) {
            return NULL;
        }
        else if("true".equals(content)) {
            return TRUE;
        }
        else if("false".equals(content)){
            return FALSE;
        }
        else if("string".equals(content)){
            return STRING;
        }
        else if("number".equals(content)){
            return NUMBER;
        }
        else if("integer".equals(content)){
            return INTEGER;
        }
        else if("boolean".equals(content)){
            return BOOLEAN;
        }
        else {
            last_val =  content;
            return ID;
        }
    }

    private boolean identityHead(){
        if( cursorChar == '$' || cursorChar == '_' ||
                (cursorChar >= 'a' && cursorChar <= 'z') ||
                (cursorChar >='A' && cursorChar <='Z') ) {
            advance();
            return true;
        }
        else return false;
    }
    private boolean identityTail() {
        if( cursorChar == '$' || cursorChar == '_' ||
                (cursorChar >= 'a' && cursorChar <= 'z') ||
                (cursorChar >='A' && cursorChar <='Z')  ||
                (cursorChar >='0' && cursorChar <='9') ) {
            advance();
            return true;
        }
        else return false;
    }



    private boolean ch(char c) {
        if (cursorChar == c) {
            advance();
            return true;
        } else return false;
    }

    private boolean ch(Function<Character,Boolean> predict) {
        if(predict.apply(cursorChar)) {
            advance();
            return true;
        }
        else return false;
    }

    private boolean advance() {
        cursorChar = nextChar();
        return true;
    }



    final int yylex_string(char startChar){

        final int begin = this.cursor;
        int end = -1;
        boolean existsEscape = false;
        final int length = this.length;

        StringBuilder buffer = null;

        int cursor = this.cursor + 1;
        for(; cursor < length; cursor++) {
            char cursorChar = source.charAt(cursor);
            if(cursorChar == startChar) {
                end = cursor;
                break;
            }
            else if(cursorChar == '\\') {
                if(!existsEscape){
                    buffer = new StringBuilder();
                    for(int i = begin + 1; i < cursor; i++)
                        buffer.append(source.charAt(i));
                }
                existsEscape = true;

                cursor = cursor + 1;
                if(cursor >= length) throw fail("escape");

                char esc0 = source.charAt(cursor);
                switch(esc0){
                    case '"':
                    case '/':
                    case '\\':
                    case '\'':
                        buffer.append(esc0);  break;
                    case 'b':
                        buffer.append('\b');  break;
                    case 'f':
                        buffer.append('\f');  break;
                    case 'n':
                        buffer.append( '\n');  break;
                    case 'r':
                        buffer.append( '\r');  break;
                    case 't':
                        buffer.append( '\t');  break;
                    case 'u':
                        if(cursor + 4 >= this.length) throw fail("escape");
                        int a = hexValue(source.charAt(cursor+1));
                        int b = hexValue(source.charAt(cursor+2));
                        int c = hexValue(source.charAt(cursor+3));
                        int d = hexValue(source.charAt(cursor+4));
                        cursor += 4;
                        int value = (a << 12)  | (b << 8) | (c << 4) | d;
                        buffer.append ((char)value); break;
                    default:
                        throw fail("escape");

                }

            }
            else {
                if(existsEscape)
                    buffer.append(cursorChar);
            }
        }

        this.cursor = cursor;

        if(end > begin) {
            String stringValue;
            if(!existsEscape) {
                stringValue = source.subSequence(begin +1, end).toString(); // new String(chars, begin + 1, end);
            }
            else {
                stringValue = buffer.toString();
            }
            advance();
            last_val = stringValue;
            return LiteralString;
        }
        else throw fail("expect end '\"'");
    }

    private int hexValue(char c) {
        if ('0' <= c && c <= '9') return c - '0';
        else if ('a' <= c && c <= 'f') return c - 87;
        else if ('A' <= c && c <= 'F') return c - 55;
        else throw fail("hex digit");
    }


    ParsingException fail(String target) {
        return fail(target, cursor, cursorChar);
    }

    ParsingException fail(String target, int cursor) {
        return fail(target, cursor, cursorChar);
    }

    ParsingException fail(String target, int cursor, char errorChar) {
        Line line = getLine(cursor);

        String unexpected;
        if (errorChar == EOI) unexpected = "end-of-input";
        else if (Character.isISOControl(errorChar)) unexpected = String.format("\\u%04x", (int) errorChar);
        else unexpected = "" + errorChar;

        String expected = ("'\uFFFF'".equals(target)) ? "end-of-input" : target;

        String summary = "Unexpected " + unexpected + " at input index:" +
                cursor + "(line:" + line.lineNr + ",position:" + line.column +
                "), expected: " + expected;

        String detail = line.text; // TODO

        return new ParsingException(summary, detail);
    }

    public static class ParsingException extends RuntimeException {

        ParsingException(String summary, String detail) {
            super(summary + ":" + detail);
        }
    }

    public static class Line {
        final int lineNr;
        final int column;
        public final String text;


        Line(int lineNr, int column, String text) {
            this.lineNr = lineNr;
            this.column = column;
            this.text = text;
        }

        @Override
        public String toString() {
            return "\"" + text + "\"," + "line:" + lineNr + ",column:" + column;
        }
    }

    private Line getLine(int index) {
        int savedCursor = cursor;
        cursor = -1;
        Line line = loop(index);
        cursor = savedCursor;
        return line;
    }

    private Line loop(int index) {
        StringBuilder sb = new StringBuilder();
        int lineNo = 1;
        int ix = 0;
        int lineStartIx = 0;
        while (true) {
            char nc = nextChar();
            switch (nc) {
                case '\n':
                    if (index > ix) {
                        sb.setLength(0);
                        lineNo++;
                        ix++;
                        lineStartIx = ix + 1;
                        break;
                    }
                case EOI:
                    return new Line(lineNo, index - lineStartIx + 1, sb.toString());
                default:
                    sb.append(nc);
                    ix++;
            }
        }
    }

}