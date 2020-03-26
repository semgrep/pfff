#!/usr/bin/perl

while(<>) {
  # remove C-style comments
  s#/\*##g;
  s#\*/##g;

  # remove pad sectioning number
  s#\(\*[0-9] #(* #g;
  
  # use chars instead of long tokens for parens-like tokens
  s#\bT_LPAREN\b#"("#g;
  s#\bT_RPAREN\b#")"#g;

  s#\bT_LBRACKET\b#"["#g;
  s#\bT_RBRACKET\b#"]"#g;

  s#\bT_LCURLY\b#"{"#g;
  s#\bT_RCURLY\b#"}"#g;

  # use chars instead of long tokens for common punctuators
  s#\bT_SEMICOLON\b#";"#g;
  s#\bT_COMMA\b#","#g;
  s#\bT_PERIOD\b#"."#g;
  s#\bT_COLON\b#":"#g;

  s#\bT_DOTS\b#"..."#g;

  # use chars instead of long tokens for important operators
  s#\bT_MULT\b#"*"#g;
  s#\bT_ASSIGN\b#"="#g;

  

  print;
}
