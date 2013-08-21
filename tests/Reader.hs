module Reader where

import           Data.Text

import           Text.Textualism

file :: Text
file = pack
  "~~~~~\n\
  \title: Test Document\n\
  \date: 2013-08-20\n\
  \tags: foo\n\
  \~~~~~\n\
  \\n\
  \# [1]: First *header*\n\
  \\n\
  \Lorem ipsum dolor sit amet,<^``{foo} partial''>\n\
  \\n\
  \> citation\n\
  \  blockquote (first paragraph).\n\
  \\n\
  \  second paragraph.\n\
  \\n\
  \  <|>\n\
  \     embedded\n\
  \     verse\n\
  \\n\
  \[^1]:\n\
  \  Lorem ipsum.\n"
