Why another lightweight markup language?
----------------------------------------

Does the world need another lightweight markup language? Almost certainly not. But I need one, and am happy to ~~inflict it on~~ make it available to others.

Markdown was designed for readability in raw text form, and did a good job of that. However, I see markdown primarily used like Latex, as a writing format processed into another format for publication. And here I think markdown, along with the other lightweight formats I have seen, falls short. In an effort to preserve readability, their syntaxes are often complex, full of special cases or complicated interpretations in order to allow near-arbitrary formatting of text on the page. And they are rarely extensible, so that new constructions need to inserted as raw HTML, which is ugly and not portable to alternative output formats, or new syntax, which means that markdown written for one engine is often horribly mangled by others.

Thus, my objectives in designing textualism are formalism and extensibility. The language definition should be unambiguous, and easy to understand without memorizing lists of special cases. And, in order to encourage implementations to adhere to the standard, there should be a standard means of extending the language, allowing somewhat graceful degradation where an extension is not recognized. That said, suggestions on improving the syntax are welcome.
