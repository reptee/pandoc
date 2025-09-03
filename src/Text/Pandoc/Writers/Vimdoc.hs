{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}


module Text.Pandoc.Writers.Vimdoc (writeVimdoc) where

import Control.Applicative (optional, (<|>))
import Control.Monad (forM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Default (Default (..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.DocLayout hiding (char)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..))
import Text.Pandoc.Parsing.General (many1Till, many1TillChar, readWith)
import Text.Pandoc.Shared (capitalize, orderedListMarkers)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.URI (isURI)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toTableOfContents)
import Text.Parsec (anyChar, char, eof, string, try)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (traceShowId)
import Text.Read (readMaybe)
import Data.List (intercalate, intersperse)

-- NOTE: used xwiki, typst, zimwiki writers as a reference

-- NOTE: Test idea: each line in the render of random document not containing code block should not exceed textWidth characters

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

-- TODO: rename to VimdocWriterR
type RR m = ReaderT WriterState m

docTextWidth :: Meta -> Maybe Int
docTextWidth meta = case lookupMeta "textwidth" meta of
  Just (MetaInlines [Str tw]) -> readMaybe (T.unpack tw)
  Just (MetaString tw) -> readMaybe (T.unpack tw)
  _ -> Nothing

docShiftWidth :: Meta -> Maybe Int
docShiftWidth meta = case lookupMeta "shiftwidth" meta of
  Just (MetaInlines [Str sw]) -> readMaybe (T.unpack sw)
  Just (MetaString sw) -> readMaybe (T.unpack sw)
  _ -> Nothing

makeModeLine :: WriterState -> Text
makeModeLine ws =
  T.pack . intercalate ":" $
    [ " vim" -- deliberate extra space in the style of bundled help pages
    , "tw=" <> show tw
    , "sw=" <> show sw
    , "ft=help"
    , "norl" -- left-to-right text
    , "et:" -- expandtab and finishing ":"
    ]
 where
  tw = textWidth ws
  sw = shiftWidth ws

writeVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> m Text
writeVimdoc opts document@(Pandoc meta _) =
  -- TODO: remove
  -- traceShowId (writerVariables opts) `seq`
  --   traceShowId (meta) `seq`
  runReaderT (pandocToVimdoc opts document) $
    def{textWidth = tw, shiftWidth = sw}
 where
  tw = fromMaybe (textWidth def) $ docTextWidth meta
  sw = fromMaybe (shiftWidth def) $ docShiftWidth meta

pandocToVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> RR m Text
pandocToVimdoc opts (Pandoc meta body) = do
  st <- ask

  metadata <- metaToContext opts blockListToVimdoc inlineListToVimdoc meta
  main <- traceShowId metadata `seq` blockListToVimdoc body
  let modeline = makeModeLine st
  title <- inlineListToVimdoc $ docTitle meta
  authors <- traverse inlineListToVimdoc $ docAuthors meta
  let authors' = mconcat $ intersperse ("," <> space) (fmap nowrap authors)

  let combinedTitle =
        render (Just $ textWidth st) $
          cblock (textWidth st) $
            (title <> space)
              <> (if null authors' then "" else "by" <> space <> authors')

  -- This is placed here because I couldn't find a way to right-align text
  -- inside template to the width specified by a variable
  let toc_reminder =
        render Nothing . rblock (textWidth st) $
          ("Type |gO| to see the table of contents." :: Doc Text)

  -- TODO: nicer TOC (like `:h undotree-contents`)
  toc <-
    fmap (render (Just $ textWidth st)) . blockToVimdoc $
      toTableOfContents opts body

  let context =
        defField "body" main
          . defField "toc" (if writerTableOfContents opts then toc else "")
          . defField "modeline" modeline
          . defField "combined-title" combinedTitle
          . defField "toc-reminder" toc_reminder
          $ metadata

  pure $
    case writerTemplate opts of
      Just tpl -> render (Just $ textWidth st) $ renderTemplate tpl context
      Nothing -> render (Just $ textWidth st) main

blockListToVimdoc :: (PandocMonad m) => [Block] -> RR m (Doc Text)
blockListToVimdoc blocks = vcat <$> mapM blockToVimdoc blocks

blockToVimdoc :: (PandocMonad m) => Block -> RR m (Doc Text)

blockToVimdoc (Plain inlines) = inlineListToVimdoc inlines

blockToVimdoc (Para inlines) = do
  contents <- inlineListToVimdoc inlines
  pure $ contents <> "\n"

blockToVimdoc (LineBlock inliness) = vcat <$> mapM inlineListToVimdoc inliness

blockToVimdoc (CodeBlock (_, cls, _) code) = do
  sw <- asks shiftWidth
  let lang = case cls of
        (lang : _) -> lang
        _ -> ""
  pure . vcat $
    [ ">" <> literal lang
    , nest sw (literal code)
    , flush "<"
    ]

blockToVimdoc (RawBlock format raw) = case format of
  "vimdoc" -> pure $ literal raw
  _ -> pure ""

blockToVimdoc (BlockQuote blocks) = do
  content <- blockListToVimdoc blocks
  pure $ prefixed "| " content

blockToVimdoc (OrderedList listAttr items) = do
  -- TODO: both ordered and bullet- lists have no spacing between items
  -- regargless of content. It may make sense to add blankline if there is a
  -- paragraph or a nested list. (see isParaOrList from Writers/Jats)
  let itemsWithMarkers = zip (orderedListMarkers listAttr) items
  items' <- forM itemsWithMarkers $ \(marker, blocks) -> do
    il <- asks indentLevel
    let markerLen = T.length marker

    item' <- blockListToVimdoc blocks
    pure . nest il $ literal marker <> space <> nest (markerLen + 1) item'
  pure $ vcat items'

blockToVimdoc (BulletList items) = do
  items' <- forM items $ \blocks -> do
    let marker = "-"
    item <- blockListToVimdoc blocks
    pure $ marker <> " " <> nest 2 item
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

    r <- ask
    definition' <-
      (nest (indentLevel r + 2 * shiftWidth r) . vsep)
        <$> traverse blockListToVimdoc definitions
      -- local (\r -> r{indentLevel = indentLevel r + 2 * shiftWidth r})
      --   . fmap (T.intercalate "\n\n")
      --   $ traverse blockListToVimdoc definitions

    pure $ labeledTerm <> cr <> definition'
  pure $ vsep items' <> blankline

-- TODO: reject SoftBreak and LineBreak?
blockToVimdoc (Header level (ref, _, _) inlines) = do
  tw <- asks textWidth
  let rule = case level of
        1 -> T.replicate tw "="
        2 -> T.replicate tw "-"
        _ -> ""
  title <- fmap (render Nothing) . inlineListToVimdoc $ case level of
    3 -> capitalize inlines
    _ -> inlines

  let label = "*" <> ref <> "*"
  let spaceLeft = tw - T.length title

  pure $ vcat
      [ blankline
      , literal rule
      , literal $ title <> T.justifyRight spaceLeft ' ' label
      , blankline
      ]

blockToVimdoc HorizontalRule = do
  tw <- asks textWidth
  pure . literal $ T.replicate tw "-"

-- TODO: noop
blockToVimdoc (Table _ blkCapt specs thead tbody tfoot) = pure ""

-- TODO: how to handle figures in a format that can't display them?
-- see how panvimdoc accomplishes it
blockToVimdoc (Figure attr caption blocks) = blockListToVimdoc blocks

blockToVimdoc (Div attr blocks) = blockListToVimdoc blocks

mkVimdocDefinition ::
  (PandocMonad m) =>
  [Inline] ->
  [(Text, Text)] ->
  RR m (Doc Text)
mkVimdocDefinition term attrs = do
  sw <- asks shiftWidth
  tw <- asks textWidth
  let labels =
        [ "*" <> label <> "*"
        | (attrName, label) <- attrs
        , attrName == "label"
        ]
  let catLabels = T.intercalate " " labels
  term' <- render Nothing <$> inlineListToVimdoc term
  let termLen = sw + T.length term'
  let labelsLen = T.length catLabels

  pure $
    if termLen + labelsLen > tw
      then
        vcat
          [ rblock tw (literal catLabels)
          , rblock sw (literal term')
          ]
      else
        mconcat
          [ rblock sw (literal term')
          , rblock (tw - termLen) (literal catLabels)
          ]

inlineListToVimdoc :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
inlineListToVimdoc inlines = hcat <$> mapM inlineToVimdoc inlines

inlineToVimdoc :: (PandocMonad m) => Inline -> RR m (Doc Text)

inlineToVimdoc (Str str) = pure $ literal str

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

-- TODO: handle `:help <something>`, like panvimdoc
inlineToVimdoc (Code _ inlines) = pure . literal $ "`" <> inlines <> "`"
inlineToVimdoc Space = pure space
inlineToVimdoc SoftBreak = pure space

-- Are line breaks always allowed?
inlineToVimdoc LineBreak = pure "\n"

-- TODO: is it the best way to handle this?
inlineToVimdoc (Math _mathType math) = pure . literal $ "`$" <> math <> "$`"

inlineToVimdoc (RawInline (Format format) text) = case format of
  "vimdoc" -> pure $ literal text
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
  txt' <- render Nothing <$> inlineListToVimdoc txt

  let space =
        if " " `T.isSuffixOf` txt' && not (T.null txt')
          then ""
          else " "

  pure . literal $ case refdocLinkToLink src of
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
