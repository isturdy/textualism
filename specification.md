# textualism: lightweight publication markup with formal semantics

## Design goals:
+ Design for writing and editing, not end-user readability.
+ Be extensible: syntax is more readable and portable than raw html.
+ Invisible whitespace is insignificant (and tabs are illegal).
+ Have explicit syntax with minimal special cases and exceptions.

## Indentation notes:
+ Indentation levels:
  + Root: the primary indentation level of the current document.
  + Current: the primary indentation level of the current block.
  + Dedent: less indented than the current indent; must be the current indentation of an ancestor block.
+ A dedent closes all blocks within the ancestor of the same indentation.
+ A block takes its indentation from the least-indented element.
+ Certain blocks concatenate adjacent entries on the same indent.

## General formatting:
+ Paragraphs are separated by blank lines; new lines are converted to spaces (but unnecessary between paragraphs and indent blocks).
+ Headings begin with a sequence of equal signs equal to the level of the header.
  + Headings within an indent block are local to that block.

## Header

A header is a set of key-value pairs begun and ended with three tildes.
+ Key: `/^((?:[^:]|~:)+): */` (applying `~`-escaping).
+ Value: The value continues until the first line at root indentation.
  + Simple/list: Each element consists of one line; inline formatting allowed.
  + Block: At least three quotation marks, closing with the next instance of three quotation marks or the next dedent. Block formatting allowed.
  + Explicit: At least three backticks, closing with the next instance of the same number of backticks at the current indentation or the next dedent. No formatting processed.

## Indent blocks
+ \`\`\`: code block
  + \`\`\`language other classes
  + code-language header field sets default
  + \`\`\`{} for explicit plain text (overrides default)
+ $$$: display mathematics
  + First line can contain id and caption; numbered if id given.
+ >: blockquote
  + Further paragraphs are indented
+ ~: citation
  + Appends to blockquote; disregarded elsewhere
+ |, <|>, |>, <|: aligned blocks
  + Newlines produce line breaks.
  + ragged on edges with angle brackets.
+ +: unordered list (concatenates)
+ #: ordered list (concatenates)
  + simplified internal references.
