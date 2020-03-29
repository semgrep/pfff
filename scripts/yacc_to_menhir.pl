#!/usr/bin/perl

my $after_percentpercent = 0;

while(<>) {
    if(/^%%/) { $after_percentpercent = 1; }

  # remove C-style comments
  s#/\*\(\*#(*#g;
  s#\*\)\*/#*)#g;
  s#/\*#(*#g;
  s#\*/#*)#g;

  # remove pad sectioning number
  s#\(\*[0-9] #(* #g;

    if($after_percentpercent) {
  
  # use chars instead of long tokens for parens-like tokens
  s#\bTOParen\b#"("#g;
  s#\bTCParen\b#")"#g;

  s#\bTOBracket\b#"["#g;
  s#\bTCBracket\b#"]"#g;

  s#\bTOBracketLess\b#"[<"#g;
  s#\bTGreaterCBracket\b#">]"#g;
  s#\bTOBracketPipe\b#"[|"#g;
  s#\bTPipeCBracket\b#"|]"#g;
  s#\bTOBraceLess\b#"{<"#g;
  s#\bTGreaterCBrace\b#">}"#g;

  s#\bTOBrace\b#"{"#g;
  s#\bTCBrace\b#"}"#g;

  # use chars instead of long tokens for common punctuators
  s#\bTSemiColon\b#";"#g;
  s#\bTComma\b#","#g;
  s#\bTDot\b#"."#g;
  s#\bTColon\b#":"#g;

  s#\bTSemiColonSemiColon\b#";;"#g;
  s#\bTDotDot\b#".."#g;
  s#\bTColonColon\b#"::"#g;
  s#\bTQuestionQuestion\b#"??"#g;


  s#\bTQuestion\b#"?"#g;
  s#\bTTilde\b#"~"#g;
  s/\bTSharp\b/"#"/g;
  s#\bTColonGreater\b#":>"#g;

  s#\bTPipe\b#"|"#g;
  s#\bTAssignMutable\b#"<-"#g;
  s#\bTAssign\b#":="#g;
  s#\bTBang\b#"!"#g;
  s#\bTArrow\b#"->"#g;
  s#\bTUnderscore\b#"_"#g;

#  s#\bT_DOTS\b#"..."#g;

  # use chars instead of long tokens for important operators
  s#\bTStar\b#"*"#g;
  s#\bTEq\b#"="#g;

  s#\bTQuote\b#"'"#g;
  s#\bTBackQuote\b#"`"#g;

  }  

  print;
}
