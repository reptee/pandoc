{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}


module Text.Pandoc.Writers.Vimdoc (writeVimdoc) where

import Control.Monad (forM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default (Default (..))
import Data.Functor ((<&>), void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..), WrapOption (..))
import Text.PrettyPrint (Style (..), renderStyle, style)
import Text.Show.Pretty (ppDoc)

-- NOTE: for comparison against markdown
-- NOTE: Test idea: each line in the render of random document not containing code block should not exceed textWidth characters

import Text.Pandoc.Class (runPure)
import Text.Pandoc.Writers.Markdown (writeCommonMark, writeMarkdown)
import Text.Pandoc.Shared (orderedListMarkers, capitalize, trimr)
import Text.Pandoc.URI (isURI)
import Text.Pandoc.Parsing.General (readWithM, readWith, many1Till, many1TillChar)
import Text.Pandoc.Error (PandocError, renderError)
import Text.Parsec (string, try, manyTill, char, choice, eof, anyChar)
import Control.Applicative (asum, (<|>), optional)
import Text.Pandoc.Logging (LogMessage(ReferenceNotFound))
import Text.Pandoc.Writers.Shared (metaToContext, defField)
import Text.DocLayout (literal, render)
import Text.Pandoc.Templates (renderTemplate)
import Data.Maybe (fromJust)

data WriterState = WriterState
  { indentLevel :: Int -- How much to indent the block. Inlines shouldn't
                       -- be concerned with indent level (I guess?)
  , textWidth :: Int -- maximum length of a line
  , shiftWidth :: Int -- spaces per indentation level
  , wrapText :: WrapOption
  }

instance Default WriterState where
  def =
    WriterState
      { indentLevel = 0
      , textWidth = 78
      , shiftWidth = 4
      , wrapText = WrapAuto
      }

wrapAuto, wrapNone, wrapPreserve :: (Monad m) => RR m a -> RR m a
wrapAuto = local (\st -> st{wrapText = WrapAuto})
wrapNone = local (\st -> st{wrapText = WrapNone})
wrapPreserve = local (\st -> st{wrapText = WrapPreserve})

-- NOTE: used XWiki writer as a reference

-- TODO: rename to VimdocWriterS
type WW = StateT WriterState

-- TODO: rename to VimdocWriterR
type RR m = ReaderT WriterState m

-- TODO: Maybe rename to vimhelp?
writeVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> m Text
writeVimdoc opts (Pandoc meta body) =
  evalStateT (pandocToVimdoc opts (Pandoc meta body)) def

pandocToVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> WW m Text
pandocToVimdoc opts (Pandoc meta body) = do
  st <- get
  metadata <-
    flip runReaderT st $
      metaToContext
        opts
        (fmap (literal . trimr) . blockListToVimdoc)
        (fmap (literal . trimr) . inlineListToVimdoc)
        meta
  main <- runReaderT (blockListToVimdoc body) st

  let context =
        defField "body" main $
          defField "toc" (writerTableOfContents opts) metadata

  -- TODO: generate modeline (vim:tw=xx:sw=x:...)
  pure $
    case writerTemplate opts of
      Just tpl -> render (Just $ textWidth st) $ renderTemplate tpl context
      Nothing -> main

vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- TODO: use text builder instead of concatenating text?

blockListToVimdoc :: (PandocMonad m) => [Block] -> RR m Text
blockListToVimdoc blocks = vcat <$> mapM blockToVimdoc blocks

-- TODO: respect textWidth
blockToVimdoc :: (PandocMonad m) => Block -> RR m Text

blockToVimdoc (Plain inlines) = inlineListToVimdoc inlines

blockToVimdoc (Para inlines) = do
  contents <- inlineListToVimdoc inlines
  pure $ contents <> "\n"

blockToVimdoc (LineBlock inliness) = vcat <$> mapM inlineListToVimdoc inliness

blockToVimdoc (CodeBlock (_, cls, _) code) =
  case cls of
    -- TODO: indent code indentLevel + shiftWidth
    (lang : _) -> pure $ ">" <> lang <> "\n" <> code <> "\n<"
    _ -> pure $ ">" <> "\n" <> code <> "\n<"

-- TODO: vimdoc -> vimhelp?
blockToVimdoc (RawBlock format raw) = case format of
  "vimdoc" -> pure raw
  _ -> pure ""

blockToVimdoc (BlockQuote blocks) = do
  content <- blockListToVimdoc blocks
  pure $ "| " <> content

blockToVimdoc (OrderedList listAttr items) = do
  -- TODO: renamer
  let merged = (,) <$> items <*> orderedListMarkers listAttr
  items' <- forM merged $ \(blocks, marker) -> do
    il <- asks indentLevel
    let markerLen = T.length marker
    -- TODO: it is definitely wrong.
    -- It will produce <indent> <marker> <space> <indent> <content>
    item <- local (\r -> r{indentLevel = indentLevel r + markerLen + 1}) $ blockListToVimdoc blocks
    pure $ T.replicate il " " <> marker <> " " <> item
  pure $ vcat items'

blockToVimdoc (BulletList items) = do
  items' <- forM items $ \blocks -> do
    il <- asks indentLevel
    let marker = "-"
    item <- local (\r -> r{indentLevel=indentLevel r + 2}) $ blockListToVimdoc blocks
    pure $ T.replicate il " " <> marker <> " " <> item
  pure $ vcat items'


blockToVimdoc (DefinitionList items) = do
  items' <- forM items $ \(term, definitions) -> do
    -- TODO: indent term 1*shiftWidth
    -- TODO: indent definition 2*shiftWidth

    labeledTerm <- case term of
      -- TODO: also handle `Code attr code` the same way? It is easier to type
      -- `code`{attrs} then [`code`]{attrs}.
      -- TODO: Maybe it should also check for attributes like `mapping`, so it
      -- will automatically generate labels that are literally the terms
      -- themselves
      code@[Code (_, _, attrs) inlines] -> mkVimdocDefinition code attrs
      [Span (_, _, attrs) inlines] -> mkVimdocDefinition inlines attrs
      _ -> mkVimdocDefinition term []

    definition' <-
      local (\r -> r{indentLevel = indentLevel r + 2 * shiftWidth r})
        . fmap (T.intercalate "\n\n")
        $ traverse blockListToVimdoc definitions

    pure $ labeledTerm <> "\n" <> definition'
  pure $ vcat items'

-- TODO: reject SoftBreak and LineBreak?
blockToVimdoc (Header level (_, _, attr) text) = do
  tw <- asks textWidth
  let rule = case level of
        1 -> T.replicate tw "=" <> "\n"
        2 -> T.replicate tw "-" <> "\n"
        _ -> ""
  let labels =
        [ "*" <> label <> "*"
        | (attrName, label) <- attr
        , attrName == "label"
        ]
  let labeled = T.intercalate " " labels
  text' <- inlineListToVimdoc $ case level of
    3 -> capitalize text
    _ -> text

  let spaceLeft = tw - T.length text'

  pure $
    T.concat
      [ rule
      , text'
      , T.justifyRight spaceLeft ' ' labeled
      , "\n"
      ]

blockToVimdoc HorizontalRule = do
  tw <- asks textWidth
  pure $ T.replicate tw "-"

-- TODO: noop
blockToVimdoc (Table _ blkCapt specs thead tbody tfoot) = pure ""

-- TODO: how to handle figures in a format that can't display them?
-- see how panvimdoc accomplishes it
blockToVimdoc (Figure attr caption blocks) = blockListToVimdoc blocks

blockToVimdoc (Div attr blocks) = blockListToVimdoc blocks

mkVimdocDefinition :: (PandocMonad m) => [Inline] -> [(Text, Text)] -> RR m Text
mkVimdocDefinition term attrs = do
  sw <- asks shiftWidth
  tw <- asks textWidth
  let labels =
        [ "*" <> label <> "*"
        | (attrName, label) <- attrs
        , attrName == "label"
        ]
  let catLabels = T.intercalate " " labels
  term' <- inlineListToVimdoc term
  let termLen = sw + T.length term'
  let labelsLen = T.length catLabels

  pure $
    if termLen + labelsLen > tw
      then
        T.concat
          [ T.justifyRight tw ' ' catLabels
          , "\n"
          , T.replicate sw " "
          , term'
          ]
      else
        T.concat
          [ T.replicate sw " "
          , term'
          , T.replicate (2 * sw) " "
          , catLabels
          ]

test =
  let content =
        [ LineBlock [[Str "qq"], [Str "hello"]]
        -- , CodeBlock ("rust", ["rustt", "qq"], []) "hello"
        , RawBlock "markdown" "hello\nqq"
        ]
      meta = mempty
   in runPure $ writeMarkdown def (Pandoc (Meta meta) content)

-- >>> test
-- Right "qq  \nhello\n\nhello\nqq\n"

inlineListToVimdoc :: (PandocMonad m) => [Inline] -> RR m Text
inlineListToVimdoc inlines = mconcat <$> mapM inlineToVimdoc inlines

inlineToVimdoc :: (PandocMonad m) => Inline -> RR m Text

inlineToVimdoc (Str str) = pure str

-- Can't find bold/italic/emph/underline in the syntax file
-- ($VIMRUNTIME/syntax/help.vim)
-- Can't find it in TS grammar definition either
-- https://github.com/neovim/tree-sitter-vimdoc/blob/master/grammar.js
-- TODO: check what panvimdoc does

inlineToVimdoc (Emph inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Underline inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Strong inlines) = inlineListToVimdoc inlines

-- Neither does vimdoc has any support for these
inlineToVimdoc (Strikeout inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Superscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Subscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (SmallCaps inlines) = inlineListToVimdoc inlines

inlineToVimdoc (Quoted typ inlines) =
  let quote = case typ of SingleQuote -> "'"; DoubleQuote -> "\""
   in inlineListToVimdoc inlines >>= \text -> pure (quote <> text <> quote)

-- TODO: is there reasonable syntax? What does markdown do?
inlineToVimdoc (Cite _citations inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Code _ inlines) = pure $ "`" <> inlines <> "`"
inlineToVimdoc Space = pure " "
inlineToVimdoc SoftBreak = pure ""

-- Are line breaks always allowed?
inlineToVimdoc LineBreak = pure "\n"

-- TODO: is it the best way to handle this?
inlineToVimdoc (Math _mathType math) = pure $ "`$" <> math <> "$`"

inlineToVimdoc (RawInline (Format format) text) = case format of
  "vimdoc" -> pure text
  _ -> pure ""

-- TODO: TEST: empty link generation
-- TODO: TEST:
-- >>> refdocLinkToLink "https://neovim.io/doc/user/vim_diff.html#vim-differences"
-- Right "vim-differences"
--
-- TODO: TEST:
-- >>> refdocLinkToLink "https://neovim.io/doc/user/vim_diff.html"
-- Right "vim_diff.txt"
--
-- TODO: TEST:
-- >>> refdocLinkToLink "https://vimhelp.org/motion.txt.html"
-- Right "motion.txt"
--
-- TODO: TEST:
-- >>> refdocLinkToLink "https://vimhelp.org/motion.txt.html#motion.txt"
-- Right "motion.txt"
inlineToVimdoc (Link _ txt (src, _)) = do
  txt' <- inlineListToVimdoc txt

  let space =
        if " " `T.isSuffixOf` txt' && not (T.null txt')
          then ""
          else " "

  pure $ case refdocLinkToLink src of
    Right link -> txt' <> space <> "|" <> link <> "|"
    Left _ | isURI src -> txt' <> " " <> src
    Left _ | "#" `T.isPrefixOf` src ->
      -- TODO: something more elegant?
      let src' = fromJust (T.stripPrefix "#" src)
       in txt' <> space <> "|" <> src' <> "|"
    -- TODO: vimdoc-TS does not seem to expect any extra characters around URL:
    -- https://github.com/neovim/tree-sitter-vimdoc/blob/ffa29e863738adfc1496717c4acb7aae92a80ed4/grammar.js#L225
    Left _ -> txt' <> space <> "<" <> src <> ">"

inlineToVimdoc (Image {}) = pure ""

-- TODO: mimic ANSI writer,
--       see how panvimdoc handles it
inlineToVimdoc (Note blocks) = blockListToVimdoc blocks

inlineToVimdoc (Span _ inlines) = inlineListToVimdoc inlines


refdocLinkToLink :: Text -> Either PandocError Text
refdocLinkToLink x = (\parser -> readWith parser Nothing x) $ do
  string "http" >> optional (char 's') >> string "://"

  let vimhelpP = do
        try (string "vimhelp.org/") <|> string "neo.vimhelp.org/"

        try (many1Till anyChar (char '#') >> many1TillChar anyChar eof)
          <|> many1TillChar anyChar (try $ string ".html" >> eof)

  let neovimP = do
        string "neovim.io/doc/user/"
        try (many1Till anyChar (char '#') >> many1TillChar anyChar eof)
          <|> do base <- many1TillChar anyChar (try $ string ".html" >> eof)
                 pure $ base <> ".txt"

  try vimhelpP <|> neovimP
