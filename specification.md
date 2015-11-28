# textualism: lightweight publication markup with formal semantics

## Design goals:
+ Design for writing and editing, not end-user readability.
+ Be extensible: syntax is more readable and portable than raw html.
+ Trailing whitespace is ignored.
+ Have explicit syntax with minimal special cases and exceptions.

## Indentation notes:
+ Indentation levels:
  + Root: the primary indentation level of the current document.
  + Current: the primary indentation level of the current block.
  + Dedent: less indented than the current indent; must be the current indentation of an ancestor block.
+ A dedent closes all blocks within the ancestor of the same indentation.
+ The indentation of an indent block is taken from the first element
  + Explicit blocks cannot nest, so they take it from the least-indented element (they are also the only block in which indentation is significant, and we want to allow people to indent the first line).
+ Certain blocks concatenate adjacent entries on the same indent.

## General formatting:
+ Paragraphs are separated by blank lines; new lines are ignored (\newline if you need to force them--but in paragraph contexts you usually should not)
+ Headings begin with a sequence of equal signs equal to the level of the header.
  + Headings within an indent block are local to that block.

## Line formatting
+ Quotation marks as \`'/\`\`'' (renders with smart quotes)
  + \`\`{type}'' for specific class.
  + \`' for explicit, unclassed, single quotes.
+ Explicit block as `%{type}content%` (type optional). Tilde escapes allowed, necessary for leading '{' and any '%'.
+ `*text*` for emphasis. Nesting is prohibited; use `\\em{\em{text}}` if necessary.

## Header
A header is an optional set of key-value pairs begun and ended with three tildes.
+ Key: `/^((?:[^:]|~:)+): */` (applying `~`-escaping).
+ Value: The value continues until the first line at root indentation.
  + Simple/list: Each element consists of one line; inline formatting allowed.
  + Block: Nothing but whitespace on the key's line; ends with the next dedent.
  + Explicit: `%%%` on the key's line (any whitespace allowed); ends with the next dedent (using the explicit dedent rules). No formatting processed.

## Indent blocks
All internal blocks must start at the beginning of a line (after indents)
+ %%%: explicit block
  + %%% language other attributes
    + e.g. %%% haskell literate
  + [future] code-language header field sets default
    + %%%{} for explicit plain text (overrides default)
+ $$$: display mathematics
  + First line can contain id and caption; numbered if id given.
+ >: blockquote
  + Further paragraphs are indented
  + Citation after optional label on introductory line.
+ |, <|>, |>, <|: aligned blocks
  + Newlines produce line breaks.
  + ragged on edges with angle brackets.
<<<<<<< HEAD
+ Lists:
  + +: unordered list (concatenates).
  + #: ordered list (concatenates).
  + List (rather than item) labels should be attached to the indicator, e.g.
	`#foo [itemlabel]: content`
  + For a list of blocks, only the item label should be on the first line.
=======
+ +: unordered list (concatenates)
+ #: ordered list (concatenates)
>>>>>>> master
+ ----- (or more): horizontal line. Ignores contents.
+ `/#+/`: header (with level).

## Labels
+ `[id]:` creates a label.
  + `[id]:` labels the parent block.
  + `[^fnname]: fntext` for footnotes. The name is only significant internally.
  + `[@id]: url` for links. Useful for keeping long links out of the text, or for reuse.
+ `[id]{displayname}` references a label. The displayname is optional; many label types have sensible default displays.
  + `[id]` will display section number, equation number, or figure number as appropriate, but the display name is honored if present.
  + `[^fnname]` will, unsurprisingly, display as a footnote. Any display name given is disregarded.

+ `<value>` creates an anonymous label--consider `<content>{display}` the equivalent of `[id]{display}` and a separate `[id]: content`.
  + `<^fntext>` for quick footnotes.
  + `<@address>{display}` for urls (if content is a valid url format).
