/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Skeleton implementation for Bison LALR(1) parsers in Java

   Copyright (C) 2007-2015, 2018-2021 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

package wjson.parser;



import java.text.MessageFormat;
import java.util.ArrayList;
/* "%code imports" blocks.  */
/* "Program.y":8  */

 import static wjson.patterns.ast.Tree.*;
 import java.util.*;

/* "../src/main/java/wjson/parser/Program.java":50  */

/**
 * A Bison parser, automatically generated from <tt>Program.y</tt>.
 *
 * @author LALR (1) parser skeleton written by Paolo Bonzini.
 */
public class Program
{
  /** Version number for the Bison executable that generated this parser.  */
  public static final String bisonVersion = "3.8.2";

  /** Name of the skeleton that generated this parser.  */
  public static final String bisonSkeleton = "lalr1.java";



  /**
   * True if verbose error messages are enabled.
   */
  private boolean yyErrorVerbose = true;

  /**
   * Whether verbose error messages are enabled.
   */
  public final boolean getErrorVerbose() { return yyErrorVerbose; }

  /**
   * Set the verbosity of error messages.
   * @param verbose True to request verbose error messages.
   */
  public final void setErrorVerbose(boolean verbose)
  { yyErrorVerbose = verbose; }




  public enum SymbolKind
  {
    S_YYEOF(0),                    /* "end of file"  */
    S_YYerror(1),                  /* error  */
    S_YYUNDEF(2),                  /* "invalid token"  */
    S_ID(3),                       /* ID  */
    S_NULL(4),                     /* NULL  */
    S_TRUE(5),                     /* TRUE  */
    S_FALSE(6),                    /* FALSE  */
    S_LiteralString(7),            /* LiteralString  */
    S_LiteralNumber(8),            /* LiteralNumber  */
    S_STRING(9),                   /* STRING  */
    S_NUMBER(10),                  /* NUMBER  */
    S_BOOLEAN(11),                 /* BOOLEAN  */
    S_INTEGER(12),                 /* INTEGER  */
    S_Any1(13),                    /* Any1  */
    S_Anys(14),                    /* Anys  */
    S_TagedString(15),             /* TagedString  */
    S_16_(16),                     /* '@'  */
    S_17_(17),                     /* '['  */
    S_18_(18),                     /* ']'  */
    S_19_(19),                     /* '('  */
    S_20_(20),                     /* ')'  */
    S_21_(21),                     /* '{'  */
    S_22_(22),                     /* '}'  */
    S_23_(23),                     /* ','  */
    S_24_(24),                     /* ':'  */
    S_25_(25),                     /* '/'  */
    S_YYACCEPT(26),                /* $accept  */
    S_program(27),                 /* program  */
    S_bind_jsval(28),              /* bind_jsval  */
    S_opt_bind(29),                /* opt_bind  */
    S_jsval(30),                   /* jsval  */
    S_opt_arr_items(31),           /* opt_arr_items  */
    S_arr_items(32),               /* arr_items  */
    S_opt_fields(33),              /* opt_fields  */
    S_fields(34),                  /* fields  */
    S_field(35),                   /* field  */
    S_path(36);                    /* path  */


    private final int yycode_;

    SymbolKind (int n) {
      this.yycode_ = n;
    }

    private static final SymbolKind[] values_ = {
      SymbolKind.S_YYEOF,
      SymbolKind.S_YYerror,
      SymbolKind.S_YYUNDEF,
      SymbolKind.S_ID,
      SymbolKind.S_NULL,
      SymbolKind.S_TRUE,
      SymbolKind.S_FALSE,
      SymbolKind.S_LiteralString,
      SymbolKind.S_LiteralNumber,
      SymbolKind.S_STRING,
      SymbolKind.S_NUMBER,
      SymbolKind.S_BOOLEAN,
      SymbolKind.S_INTEGER,
      SymbolKind.S_Any1,
      SymbolKind.S_Anys,
      SymbolKind.S_TagedString,
      SymbolKind.S_16_,
      SymbolKind.S_17_,
      SymbolKind.S_18_,
      SymbolKind.S_19_,
      SymbolKind.S_20_,
      SymbolKind.S_21_,
      SymbolKind.S_22_,
      SymbolKind.S_23_,
      SymbolKind.S_24_,
      SymbolKind.S_25_,
      SymbolKind.S_YYACCEPT,
      SymbolKind.S_program,
      SymbolKind.S_bind_jsval,
      SymbolKind.S_opt_bind,
      SymbolKind.S_jsval,
      SymbolKind.S_opt_arr_items,
      SymbolKind.S_arr_items,
      SymbolKind.S_opt_fields,
      SymbolKind.S_fields,
      SymbolKind.S_field,
      SymbolKind.S_path
    };

    static final SymbolKind get(int code) {
      return values_[code];
    }

    public final int getCode() {
      return this.yycode_;
    }

    /* Return YYSTR after stripping away unnecessary quotes and
       backslashes, so that it's suitable for yyerror.  The heuristic is
       that double-quoting is unnecessary unless the string contains an
       apostrophe, a comma, or backslash (other than backslash-backslash).
       YYSTR is taken from yytname.  */
    private static String yytnamerr_(String yystr)
    {
      if (yystr.charAt (0) == '"')
        {
          StringBuffer yyr = new StringBuffer();
          strip_quotes: for (int i = 1; i < yystr.length(); i++)
            switch (yystr.charAt(i))
              {
              case '\'':
              case ',':
                break strip_quotes;

              case '\\':
                if (yystr.charAt(++i) != '\\')
                  break strip_quotes;
                /* Fall through.  */
              default:
                yyr.append(yystr.charAt(i));
                break;

              case '"':
                return yyr.toString();
              }
        }
      return yystr;
    }

    /* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
       First, the terminals, then, starting at \a YYNTOKENS_, nonterminals.  */
    private static final String[] yytname_ = yytname_init();
  private static final String[] yytname_init()
  {
    return new String[]
    {
  "\"end of file\"", "error", "\"invalid token\"", "ID", "NULL", "TRUE",
  "FALSE", "LiteralString", "LiteralNumber", "STRING", "NUMBER", "BOOLEAN",
  "INTEGER", "Any1", "Anys", "TagedString", "'@'", "'['", "']'", "'('",
  "')'", "'{'", "'}'", "','", "':'", "'/'", "$accept", "program",
  "bind_jsval", "opt_bind", "jsval", "opt_arr_items", "arr_items",
  "opt_fields", "fields", "field", "path", null
    };
  }

    /* The user-facing name of this symbol.  */
    public final String getName() {
      return yytnamerr_(yytname_[yycode_]);
    }

  };


  /**
   * Communication interface between the scanner and the Bison-generated
   * parser <tt>Program</tt>.
   */
  public interface Lexer {
    /* Token kinds.  */
    /** Token "end of file", to be returned by the scanner.  */
    static final int YYEOF = 0;
    /** Token error, to be returned by the scanner.  */
    static final int YYerror = 256;
    /** Token "invalid token", to be returned by the scanner.  */
    static final int YYUNDEF = 257;
    /** Token ID, to be returned by the scanner.  */
    static final int ID = 258;
    /** Token NULL, to be returned by the scanner.  */
    static final int NULL = 259;
    /** Token TRUE, to be returned by the scanner.  */
    static final int TRUE = 260;
    /** Token FALSE, to be returned by the scanner.  */
    static final int FALSE = 261;
    /** Token LiteralString, to be returned by the scanner.  */
    static final int LiteralString = 262;
    /** Token LiteralNumber, to be returned by the scanner.  */
    static final int LiteralNumber = 263;
    /** Token STRING, to be returned by the scanner.  */
    static final int STRING = 264;
    /** Token NUMBER, to be returned by the scanner.  */
    static final int NUMBER = 265;
    /** Token BOOLEAN, to be returned by the scanner.  */
    static final int BOOLEAN = 266;
    /** Token INTEGER, to be returned by the scanner.  */
    static final int INTEGER = 267;
    /** Token Any1, to be returned by the scanner.  */
    static final int Any1 = 268;
    /** Token Anys, to be returned by the scanner.  */
    static final int Anys = 269;
    /** Token TagedString, to be returned by the scanner.  */
    static final int TagedString = 270;

    /** Deprecated, use YYEOF instead.  */
    public static final int EOF = YYEOF;


    /**
     * Method to retrieve the semantic value of the last scanned token.
     * @return the semantic value of the last scanned token.
     */
    Object getLVal();

    /**
     * Entry point for the scanner.  Returns the token identifier corresponding
     * to the next token and prepares to return the semantic value
     * of the token.
     * @return the token identifier corresponding to the next token.
     */
    int yylex() throws java.io.IOException;

    /**
     * Emit an errorin a user-defined way.
     *
     *
     * @param msg The string for the error message.
     */
     void yyerror(String msg);


  }


  /**
   * The object doing lexical analysis for us.
   */
  private Lexer yylexer;





  /**
   * Instantiates the Bison-generated parser.
   * @param yylexer The scanner that will supply tokens to the parser.
   */
  public Program(Lexer yylexer)
  {

    this.yylexer = yylexer;

  }



  private int yynerrs = 0;

  /**
   * The number of syntax errors so far.
   */
  public final int getNumberOfErrors() { return yynerrs; }

  /**
   * Print an error message via the lexer.
   *
   * @param msg The error message.
   */
  public final void yyerror(String msg) {
      yylexer.yyerror(msg);
  }



  private final class YYStack {
    private int[] stateStack = new int[16];
    private Object[] valueStack = new Object[16];

    public int size = 16;
    public int height = -1;

    public final void push(int state, Object value) {
      height++;
      if (size == height) {
        int[] newStateStack = new int[size * 2];
        System.arraycopy(stateStack, 0, newStateStack, 0, height);
        stateStack = newStateStack;

        Object[] newValueStack = new Object[size * 2];
        System.arraycopy(valueStack, 0, newValueStack, 0, height);
        valueStack = newValueStack;

        size *= 2;
      }

      stateStack[height] = state;
      valueStack[height] = value;
    }

    public final void pop() {
      pop(1);
    }

    public final void pop(int num) {
      // Avoid memory leaks... garbage collection is a white lie!
      if (0 < num) {
        java.util.Arrays.fill(valueStack, height - num + 1, height + 1, null);
      }
      height -= num;
    }

    public final int stateAt(int i) {
      return stateStack[height - i];
    }

    public final Object valueAt(int i) {
      return valueStack[height - i];
    }

    // Print the state stack on the debug stream.
    public void print(java.io.PrintStream out) {
      out.print ("Stack now");

      for (int i = 0; i <= height; i++) {
        out.print(' ');
        out.print(stateStack[i]);
      }
      out.println();
    }
  }

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return success (<tt>true</tt>).
   */
  public static final int YYACCEPT = 0;

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return failure (<tt>false</tt>).
   */
  public static final int YYABORT = 1;



  /**
   * Returned by a Bison action in order to start error recovery without
   * printing an error message.
   */
  public static final int YYERROR = 2;

  /**
   * Internal return codes that are not supported for user semantic
   * actions.
   */
  private static final int YYERRLAB = 3;
  private static final int YYNEWSTATE = 4;
  private static final int YYDEFAULT = 5;
  private static final int YYREDUCE = 6;
  private static final int YYERRLAB1 = 7;
  private static final int YYRETURN = 8;


  private int yyerrstatus_ = 0;


  /**
   * Whether error recovery is being done.  In this state, the parser
   * reads token until it reaches a known state, and then restarts normal
   * operation.
   */
  public final boolean recovering ()
  {
    return yyerrstatus_ == 0;
  }

  /** Compute post-reduction state.
   * @param yystate   the current state
   * @param yysym     the nonterminal to push on the stack
   */
  private int yyLRGotoState(int yystate, int yysym) {
    int yyr = yypgoto_[yysym - YYNTOKENS_] + yystate;
    if (0 <= yyr && yyr <= YYLAST_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS_];
  }

  private int yyaction(int yyn, YYStack yystack, int yylen)
  {
    /* If YYLEN is nonzero, implement the default value of the action:
       '$$ = $1'.  Otherwise, use the top of the stack.

       Otherwise, the following line sets YYVAL to garbage.
       This behavior is undocumented and Bison
       users should not rely upon it.  */
    Object yyval = (0 < yylen) ? yystack.valueAt(yylen - 1) : yystack.valueAt(0);

    switch (yyn)
      {
          case 2: /* program: bind_jsval  */
  if (yyn == 2)
    /* "Program.y":29  */
                             { yyval = ((PatternWithName)(yystack.valueAt (0))); root = ((PatternWithName)(yystack.valueAt (0))); };
  break;


  case 3: /* bind_jsval: opt_bind jsval  */
  if (yyn == 3)
    /* "Program.y":32  */
                                 { yyval = ((Pattern)(yystack.valueAt (0))).bind(((String)(yystack.valueAt (1))));};
  break;


  case 4: /* opt_bind: %empty  */
  if (yyn == 4)
    /* "Program.y":35  */
                   { yyval = null; };
  break;


  case 5: /* opt_bind: ID '@'  */
  if (yyn == 5)
    /* "Program.y":36  */
                         { yyval = ((String)(yystack.valueAt (1)));};
  break;


  case 6: /* jsval: NULL  */
  if (yyn == 6)
    /* "Program.y":39  */
               { yyval = new NullPattern(); };
  break;


  case 7: /* jsval: TRUE  */
  if (yyn == 7)
    /* "Program.y":40  */
                       { yyval = new BooleanPattern(true); };
  break;


  case 8: /* jsval: FALSE  */
  if (yyn == 8)
    /* "Program.y":41  */
                        { yyval = new BooleanPattern(false); };
  break;


  case 9: /* jsval: LiteralString  */
  if (yyn == 9)
    /* "Program.y":42  */
                                { yyval = new StringPattern(((String)(yystack.valueAt (0)))); };
  break;


  case 10: /* jsval: LiteralNumber  */
  if (yyn == 10)
    /* "Program.y":43  */
                                { yyval = new NumberPattern(((double)(yystack.valueAt (0)))); };
  break;


  case 11: /* jsval: STRING  */
  if (yyn == 11)
    /* "Program.y":44  */
                         { yyval = new AnyVal("string"); };
  break;


  case 12: /* jsval: NUMBER  */
  if (yyn == 12)
    /* "Program.y":45  */
                         { yyval = new AnyVal("number"); };
  break;


  case 13: /* jsval: BOOLEAN  */
  if (yyn == 13)
    /* "Program.y":46  */
                          { yyval = new AnyVal("boolean"); };
  break;


  case 14: /* jsval: INTEGER  */
  if (yyn == 14)
    /* "Program.y":47  */
                          { yyval = new AnyVal("integer"); };
  break;


  case 15: /* jsval: Any1  */
  if (yyn == 15)
    /* "Program.y":48  */
                       { yyval = new AnyVal("any"); };
  break;


  case 16: /* jsval: Anys  */
  if (yyn == 16)
    /* "Program.y":49  */
                       { yyval = new AnyVals(); };
  break;


  case 17: /* jsval: TagedString  */
  if (yyn == 17)
    /* "Program.y":50  */
                              { yyval = new TagedString(((String[])(yystack.valueAt (0)))[0], ((String[])(yystack.valueAt (0)))[1]); };
  break;


  case 18: /* jsval: '[' opt_arr_items ']'  */
  if (yyn == 18)
    /* "Program.y":51  */
                                        { yyval = new ArrayPattern(((List<PatternWithName>)(yystack.valueAt (1)))); };
  break;


  case 19: /* jsval: '(' opt_arr_items ')'  */
  if (yyn == 19)
    /* "Program.y":52  */
                                        { yyval = new ArrayPattern(((List<PatternWithName>)(yystack.valueAt (1)))); };
  break;


  case 20: /* jsval: '{' opt_fields '}'  */
  if (yyn == 20)
    /* "Program.y":53  */
                                     { yyval = new ObjPattern(((Map<String, PatternWithName>)(yystack.valueAt (1)))); };
  break;


  case 21: /* opt_arr_items: %empty  */
  if (yyn == 21)
    /* "Program.y":56  */
                   { yyval = new ArrayList<PatternWithName>(); };
  break;


  case 22: /* opt_arr_items: arr_items  */
  if (yyn == 22)
    /* "Program.y":57  */
                                  { yyval = ((List<PatternWithName>)(yystack.valueAt (0))); };
  break;


  case 23: /* arr_items: bind_jsval  */
  if (yyn == 23)
    /* "Program.y":60  */
                                   { yyval = Arrays.asList(((PatternWithName)(yystack.valueAt (0)))); };
  break;


  case 24: /* arr_items: arr_items ',' bind_jsval  */
  if (yyn == 24)
    /* "Program.y":61  */
                                                 { yyval = append(((List<PatternWithName>)(yystack.valueAt (2))), ((PatternWithName)(yystack.valueAt (0)))); };
  break;


  case 25: /* opt_fields: %empty  */
  if (yyn == 25)
    /* "Program.y":64  */
                        { yyval = new HashMap<String, PatternWithName>(); };
  break;


  case 26: /* opt_fields: fields  */
  if (yyn == 26)
    /* "Program.y":65  */
                               { yyval = ((Map<String, PatternWithName>)(yystack.valueAt (0))); };
  break;


  case 27: /* opt_fields: fields ','  */
  if (yyn == 27)
    /* "Program.y":66  */
                               { yyval = ((Map<String, PatternWithName>)(yystack.valueAt (1))); };
  break;


  case 28: /* fields: field  */
  if (yyn == 28)
    /* "Program.y":69  */
                              { yyval = append( new HashMap<String, PatternWithName>(), ((Field)(yystack.valueAt (0))).name, ((Field)(yystack.valueAt (0))).pattern);  };
  break;


  case 29: /* fields: fields ',' field  */
  if (yyn == 29)
    /* "Program.y":70  */
                                         { yyval = append(((Map<String, PatternWithName>)(yystack.valueAt (2))), ((Field)(yystack.valueAt (0))).name, ((Field)(yystack.valueAt (0))).pattern); };
  break;


  case 30: /* field: path ':' opt_bind jsval  */
  if (yyn == 30)
    /* "Program.y":75  */
                                            { yyval = new Field(((String)(yystack.valueAt (3))), new PatternWithName(((String)(yystack.valueAt (1))), ((Pattern)(yystack.valueAt (0))))); };
  break;


  case 31: /* field: opt_bind Anys  */
  if (yyn == 31)
    /* "Program.y":76  */
                                  { yyval = new Field("_*", new PatternWithName(((String)(yystack.valueAt (1))), new AnyVals())); };
  break;


  case 32: /* path: ID  */
  if (yyn == 32)
    /* "Program.y":79  */
                           { yyval = ((String)(yystack.valueAt (0))); };
  break;


  case 33: /* path: LiteralString  */
  if (yyn == 33)
    /* "Program.y":80  */
                                      { yyval = ((String)(yystack.valueAt (0))); };
  break;


  case 34: /* path: path '/' ID  */
  if (yyn == 34)
    /* "Program.y":81  */
                                    { yyval = ((String)(yystack.valueAt (2))) + "/" + ((String)(yystack.valueAt (0))); };
  break;


  case 35: /* path: path '/' LiteralString  */
  if (yyn == 35)
    /* "Program.y":82  */
                                               { yyval = ((String)(yystack.valueAt (2))) + "/" + ((String)(yystack.valueAt (0))); };
  break;



/* "../src/main/java/wjson/parser/Program.java":712  */

        default: break;
      }

    yystack.pop(yylen);
    yylen = 0;
    /* Shift the result of the reduction.  */
    int yystate = yyLRGotoState(yystack.stateAt(0), yyr1_[yyn]);
    yystack.push(yystate, yyval);
    return YYNEWSTATE;
  }




  /**
   * Parse input from the scanner that was specified at object construction
   * time.  Return whether the end of the input was reached successfully.
   *
   * @return <tt>true</tt> if the parsing succeeds.  Note that this does not
   *          imply that there were no syntax errors.
   */
  public boolean parse() throws java.io.IOException

  {


    /* Lookahead token kind.  */
    int yychar = YYEMPTY_;
    /* Lookahead symbol kind.  */
    SymbolKind yytoken = null;

    /* State.  */
    int yyn = 0;
    int yylen = 0;
    int yystate = 0;
    YYStack yystack = new YYStack ();
    int label = YYNEWSTATE;



    /* Semantic value of the lookahead.  */
    Object yylval = null;



    yyerrstatus_ = 0;
    yynerrs = 0;

    /* Initialize the stack.  */
    yystack.push (yystate, yylval);



    for (;;)
      switch (label)
      {
        /* New state.  Unlike in the C/C++ skeletons, the state is already
           pushed when we come here.  */
      case YYNEWSTATE:

        /* Accept?  */
        if (yystate == YYFINAL_)
          return true;

        /* Take a decision.  First try without lookahead.  */
        yyn = yypact_[yystate];
        if (yyPactValueIsDefault (yyn))
          {
            label = YYDEFAULT;
            break;
          }

        /* Read a lookahead token.  */
        if (yychar == YYEMPTY_)
          {

            yychar = yylexer.yylex ();
            yylval = yylexer.getLVal();

          }

        /* Convert token to internal form.  */
        yytoken = yytranslate_ (yychar);

        if (yytoken == SymbolKind.S_YYerror)
          {
            // The scanner already issued an error message, process directly
            // to error recovery.  But do not keep the error token as
            // lookahead, it is too special and may lead us to an endless
            // loop in error recovery. */
            yychar = Lexer.YYUNDEF;
            yytoken = SymbolKind.S_YYUNDEF;
            label = YYERRLAB1;
          }
        else
          {
            /* If the proper action on seeing token YYTOKEN is to reduce or to
               detect an error, take that action.  */
            yyn += yytoken.getCode();
            if (yyn < 0 || YYLAST_ < yyn || yycheck_[yyn] != yytoken.getCode()) {
              label = YYDEFAULT;
            }

            /* <= 0 means reduce or error.  */
            else if ((yyn = yytable_[yyn]) <= 0)
              {
                if (yyTableValueIsError(yyn)) {
                  label = YYERRLAB;
                } else {
                  yyn = -yyn;
                  label = YYREDUCE;
                }
              }

            else
              {
                /* Shift the lookahead token.  */
                /* Discard the token being shifted.  */
                yychar = YYEMPTY_;

                /* Count tokens shifted since error; after three, turn off error
                   status.  */
                if (yyerrstatus_ > 0)
                  --yyerrstatus_;

                yystate = yyn;
                yystack.push(yystate, yylval);
                label = YYNEWSTATE;
              }
          }
        break;

      /*-----------------------------------------------------------.
      | yydefault -- do the default action for the current state.  |
      `-----------------------------------------------------------*/
      case YYDEFAULT:
        yyn = yydefact_[yystate];
        if (yyn == 0)
          label = YYERRLAB;
        else
          label = YYREDUCE;
        break;

      /*-----------------------------.
      | yyreduce -- Do a reduction.  |
      `-----------------------------*/
      case YYREDUCE:
        yylen = yyr2_[yyn];
        label = yyaction(yyn, yystack, yylen);
        yystate = yystack.stateAt(0);
        break;

      /*------------------------------------.
      | yyerrlab -- here on detecting error |
      `------------------------------------*/
      case YYERRLAB:
        /* If not already recovering from an error, report this error.  */
        if (yyerrstatus_ == 0)
          {
            ++yynerrs;
            if (yychar == YYEMPTY_)
              yytoken = null;
            yyreportSyntaxError(new Context(this, yystack, yytoken));
          }

        if (yyerrstatus_ == 3)
          {
            /* If just tried and failed to reuse lookahead token after an
               error, discard it.  */

            if (yychar <= Lexer.YYEOF)
              {
                /* Return failure if at end of input.  */
                if (yychar == Lexer.YYEOF)
                  return false;
              }
            else
              yychar = YYEMPTY_;
          }

        /* Else will try to reuse lookahead token after shifting the error
           token.  */
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------.
      | errorlab -- error raised explicitly by YYERROR.  |
      `-------------------------------------------------*/
      case YYERROR:
        /* Do not reclaim the symbols of the rule which action triggered
           this YYERROR.  */
        yystack.pop (yylen);
        yylen = 0;
        yystate = yystack.stateAt(0);
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------------------.
      | yyerrlab1 -- common code for both syntax error and YYERROR.  |
      `-------------------------------------------------------------*/
      case YYERRLAB1:
        yyerrstatus_ = 3;       /* Each real token shifted decrements this.  */

        // Pop stack until we find a state that shifts the error token.
        for (;;)
          {
            yyn = yypact_[yystate];
            if (!yyPactValueIsDefault (yyn))
              {
                yyn += SymbolKind.S_YYerror.getCode();
                if (0 <= yyn && yyn <= YYLAST_
                    && yycheck_[yyn] == SymbolKind.S_YYerror.getCode())
                  {
                    yyn = yytable_[yyn];
                    if (0 < yyn)
                      break;
                  }
              }

            /* Pop the current state because it cannot handle the
             * error token.  */
            if (yystack.height == 0)
              return false;


            yystack.pop ();
            yystate = yystack.stateAt(0);
          }

        if (label == YYABORT)
          /* Leave the switch.  */
          break;



        /* Shift the error token.  */

        yystate = yyn;
        yystack.push (yyn, yylval);
        label = YYNEWSTATE;
        break;

        /* Accept.  */
      case YYACCEPT:
        return true;

        /* Abort.  */
      case YYABORT:
        return false;
      }
}




  /**
   * Information needed to get the list of expected tokens and to forge
   * a syntax error diagnostic.
   */
  public static final class Context {
    Context(Program parser, YYStack stack, SymbolKind token) {
      yyparser = parser;
      yystack = stack;
      yytoken = token;
    }

    private Program yyparser;
    private YYStack yystack;


    /**
     * The symbol kind of the lookahead token.
     */
    public final SymbolKind getToken() {
      return yytoken;
    }

    private SymbolKind yytoken;
    static final int NTOKENS = Program.YYNTOKENS_;

    /**
     * Put in YYARG at most YYARGN of the expected tokens given the
     * current YYCTX, and return the number of tokens stored in YYARG.  If
     * YYARG is null, return the number of expected tokens (guaranteed to
     * be less than YYNTOKENS).
     */
    int getExpectedTokens(SymbolKind yyarg[], int yyargn) {
      return getExpectedTokens (yyarg, 0, yyargn);
    }

    int getExpectedTokens(SymbolKind yyarg[], int yyoffset, int yyargn) {
      int yycount = yyoffset;
      int yyn = yypact_[this.yystack.stateAt(0)];
      if (!yyPactValueIsDefault(yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative
             indexes in YYCHECK.  In other words, skip the first
             -YYN actions for this state because they are default
             actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST_ - yyn + 1;
          int yyxend = yychecklim < NTOKENS ? yychecklim : NTOKENS;
          for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck_[yyx + yyn] == yyx && yyx != SymbolKind.S_YYerror.getCode()
                && !yyTableValueIsError(yytable_[yyx + yyn]))
              {
                if (yyarg == null)
                  yycount += 1;
                else if (yycount == yyargn)
                  return 0; // FIXME: this is incorrect.
                else
                  yyarg[yycount++] = SymbolKind.get(yyx);
              }
        }
      if (yyarg != null && yycount == yyoffset && yyoffset < yyargn)
        yyarg[yycount] = null;
      return yycount - yyoffset;
    }
  }




  private int yysyntaxErrorArguments(Context yyctx, SymbolKind[] yyarg, int yyargn) {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action,
         then the only way this function was invoked is if the
         default action is an error action.  In that case, don't
         check for expected tokens because there are none.
       - The only way there can be no lookahead present (in tok) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this
         state is a consistent state with a default action.  There
         might have been a previous inconsistent state, consistent
         state with a non-default action, or user semantic action
         that manipulated yychar.  (However, yychar is currently out
         of scope during semantic actions.)
       - Of course, the expected token list depends on states to
         have correct lookahead information, and it depends on the
         parser not to perform extra reductions after fetching a
         lookahead from the scanner and before detecting a syntax
         error.  Thus, state merging (from LALR or IELR) and default
         reductions corrupt the expected token list.  However, the
         list is correct for canonical LR with one exception: it
         will still contain any token that will not be accepted due
         to an error action in a later state.
    */
    int yycount = 0;
    if (yyctx.getToken() != null)
      {
        if (yyarg != null)
          yyarg[yycount] = yyctx.getToken();
        yycount += 1;
        yycount += yyctx.getExpectedTokens(yyarg, 1, yyargn);
      }
    return yycount;
  }


  /**
   * Build and emit a "syntax error" message in a user-defined way.
   *
   * @param ctx  The context of the error.
   */
  private void yyreportSyntaxError(Context yyctx) {
      if (yyErrorVerbose) {
          final int argmax = 5;
          SymbolKind[] yyarg = new SymbolKind[argmax];
          int yycount = yysyntaxErrorArguments(yyctx, yyarg, argmax);
          String[] yystr = new String[yycount];
          for (int yyi = 0; yyi < yycount; ++yyi) {
              yystr[yyi] = yyarg[yyi].getName();
          }
          String yyformat;
          switch (yycount) {
              default:
              case 0: yyformat = "syntax error"; break;
              case 1: yyformat = "syntax error, unexpected {0}"; break;
              case 2: yyformat = "syntax error, unexpected {0}, expecting {1}"; break;
              case 3: yyformat = "syntax error, unexpected {0}, expecting {1} or {2}"; break;
              case 4: yyformat = "syntax error, unexpected {0}, expecting {1} or {2} or {3}"; break;
              case 5: yyformat = "syntax error, unexpected {0}, expecting {1} or {2} or {3} or {4}"; break;
          }
          yyerror(new MessageFormat(yyformat).format(yystr));
      } else {
          yyerror("syntax error");
      }
  }

  /**
   * Whether the given <code>yypact_</code> value indicates a defaulted state.
   * @param yyvalue   the value to check
   */
  private static boolean yyPactValueIsDefault(int yyvalue) {
    return yyvalue == yypact_ninf_;
  }

  /**
   * Whether the given <code>yytable_</code>
   * value indicates a syntax error.
   * @param yyvalue the value to check
   */
  private static boolean yyTableValueIsError(int yyvalue) {
    return yyvalue == yytable_ninf_;
  }

  private static final byte yypact_ninf_ = -26;
  private static final byte yytable_ninf_ = -28;

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
  private static final byte[] yypact_ = yypact_init();
  private static final byte[] yypact_init()
  {
    return new byte[]
    {
       6,   -13,    11,   -26,    32,   -26,   -26,   -26,   -26,   -26,
     -26,   -26,   -26,   -26,   -26,   -26,   -26,   -26,   -26,     7,
       4,    -2,   -26,   -26,    -3,    -7,     8,   -13,   -26,     3,
      -4,     9,   -26,   -11,   -26,     6,   -26,   -26,   -26,    -1,
       6,     5,   -26,   -26,    32,   -26,   -26,   -26
    };
  }

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
  private static final byte[] yydefact_ = yydefact_init();
  private static final byte[] yydefact_init()
  {
    return new byte[]
    {
       4,     0,     0,     2,     0,     5,     1,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     4,
       4,     4,     3,    23,     0,    22,     0,    32,    33,     0,
       0,    26,    28,     0,    18,     4,    19,    31,    20,     4,
       4,     0,    24,    29,     0,    34,    35,    30
    };
  }

/* YYPGOTO[NTERM-NUM].  */
  private static final byte[] yypgoto_ = yypgoto_init();
  private static final byte[] yypgoto_init()
  {
    return new byte[]
    {
     -26,   -26,     0,   -17,   -25,    10,   -26,   -26,   -26,   -12,
     -26
    };
  }

/* YYDEFGOTO[NTERM-NUM].  */
  private static final byte[] yydefgoto_ = yydefgoto_init();
  private static final byte[] yydefgoto_init()
  {
    return new byte[]
    {
       0,     2,    23,     4,    22,    24,    25,    30,    31,    32,
      33
    };
  }

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
  private static final byte[] yytable_ = yytable_init();
  private static final byte[] yytable_init()
  {
    return new byte[]
    {
       3,    27,    27,     5,    29,    28,    28,     1,    45,     1,
       1,     6,    46,    40,    41,    34,    35,    37,    38,    47,
     -25,   -27,    29,    44,   -21,   -21,     0,    43,    36,     0,
      26,     0,    39,     0,     0,    42,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,     0,    21
    };
  }

private static final byte[] yycheck_ = yycheck_init();
  private static final byte[] yycheck_init()
  {
    return new byte[]
    {
       0,     3,     3,    16,    21,     7,     7,     3,     3,     3,
       3,     0,     7,    24,    25,    18,    23,    14,    22,    44,
      22,    22,    39,    40,    20,    18,    -1,    39,    20,    -1,
      20,    -1,    23,    -1,    -1,    35,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    -1,    17,
      -1,    19,    -1,    21
    };
  }

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
  private static final byte[] yystos_ = yystos_init();
  private static final byte[] yystos_init()
  {
    return new byte[]
    {
       0,     3,    27,    28,    29,    16,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    17,
      19,    21,    30,    28,    31,    32,    31,     3,     7,    29,
      33,    34,    35,    36,    18,    23,    20,    14,    22,    23,
      24,    25,    28,    35,    29,     3,     7,    30
    };
  }

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
  private static final byte[] yyr1_ = yyr1_init();
  private static final byte[] yyr1_init()
  {
    return new byte[]
    {
       0,    26,    27,    28,    29,    29,    30,    30,    30,    30,
      30,    30,    30,    30,    30,    30,    30,    30,    30,    30,
      30,    31,    31,    32,    32,    33,    33,    33,    34,    34,
      35,    35,    36,    36,    36,    36
    };
  }

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
  private static final byte[] yyr2_ = yyr2_init();
  private static final byte[] yyr2_init()
  {
    return new byte[]
    {
       0,     2,     1,     2,     0,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       3,     0,     1,     1,     3,     0,     1,     2,     1,     3,
       4,     2,     1,     1,     3,     3
    };
  }




  /* YYTRANSLATE_(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
     as returned by yylex, with out-of-bounds checking.  */
  private static final SymbolKind yytranslate_(int t)
  {
    // Last valid token kind.
    int code_max = 270;
    if (t <= 0)
      return SymbolKind.S_YYEOF;
    else if (t <= code_max)
      return SymbolKind.get(yytranslate_table_[t]);
    else
      return SymbolKind.S_YYUNDEF;
  }
  private static final byte[] yytranslate_table_ = yytranslate_table_init();
  private static final byte[] yytranslate_table_init()
  {
    return new byte[]
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      19,    20,     2,     2,    23,     2,     2,    25,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    24,     2,
       2,     2,     2,     2,    16,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    17,     2,    18,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    21,     2,    22,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15
    };
  }


  private static final int YYLAST_ = 53;
  private static final int YYEMPTY_ = -2;
  private static final int YYFINAL_ = 6;
  private static final int YYNTOKENS_ = 26;

/* Unqualified %code blocks.  */
/* "Program.y":12  */

  public PatternWithName root;

/* "../src/main/java/wjson/parser/Program.java":1315  */

}
