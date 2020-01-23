#!/usr/bin/env node

const Parser = require('tree-sitter');
const Java = require('../tree_sitter/tree-sitter-java'); 

var args = process.argv.slice(2);

var fs = require("fs");
const sourceCode = fs.readFileSync(args[0]).toString();

const parser = new Parser();
parser.setLanguage(Java); 
const tree = parser.parse(sourceCode);

console.log(JSON.stringify(tree.rootNode, ["type", "startPosition", "endPosition", "row", "column", "children"]))