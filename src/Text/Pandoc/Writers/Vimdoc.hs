{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Text.Pandoc.Writers.Vimdoc (writeVimdoc) where

import Control.Applicative (optional, (<|>))
import Control.Monad (forM)
import Control.Monad.Reader
import Data.Default (Default (..))
import Data.List (intercalate, intersperse, transpose)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.DocLayout hiding (char, link, text)
import Text.Pandoc.Class.PandocMonad ( report, PandocMonad )
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Logging (LogMessage (..))
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..))
import Text.Pandoc.Parsing.General (many1Till, many1TillChar, readWith)
import Text.Pandoc.Shared (capitalize, onlySimpleTableCells, orderedListMarkers, isTightList)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.URI (escapeURI, isURI)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable, toTableOfContents)
import Text.Parsec (anyChar, char, eof, string, try)
import Text.Read (readMaybe)

-- NOTE: used xwiki, typst, zimwiki writers as a reference

data WriterState = WriterState
  { indentLevel :: Int -- How much to indent the block. Inlines shouldn't
                       -- be concerned with indent level (I guess?)
  , shiftWidth :: Int -- spaces per indentation level
  , writerOptions :: WriterOptions
  , vimdocPrefix :: Maybe Text
  }

instance Default WriterState where
  def =
    WriterState
      { indentLevel = 0
      , shiftWidth = 4
      , writerOptions = def
      , vimdocPrefix = Nothing
      }

indent :: (Monad m) => Int -> (RR m a) -> (RR m a)
indent n = local (\s -> s{indentLevel = indentLevel s + n})

-- TODO: rename to VimdocWriterR
type RR m = ReaderT WriterState m

docShiftWidth :: Meta -> Maybe Int
docShiftWidth meta = case lookupMeta "shiftwidth" meta of
  Just (MetaInlines [Str sw]) -> readMaybe (T.unpack sw)
  Just (MetaString sw) -> readMaybe (T.unpack sw)
  _ -> Nothing

docVimdocPrefix :: Meta -> Maybe Text
docVimdocPrefix meta = case lookupMeta "vimdoc-prefix" meta of
  Just (MetaInlines [Str pref]) -> Just pref
  Just (MetaString pref) -> Just pref
  _ -> Nothing

-- TODO: add tabstop
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
  tw = writerColumns . writerOptions $ ws
  sw = shiftWidth ws

writeVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> m Text
writeVimdoc opts document@(Pandoc meta _) =
  let
    sw = fromMaybe (shiftWidth def) $ docShiftWidth meta
    vp = docVimdocPrefix meta
   in
    runReaderT (pandocToVimdoc opts document) $
      def{shiftWidth = sw, writerOptions = opts, vimdocPrefix = vp}

pandocToVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> RR m Text
pandocToVimdoc opts (Pandoc meta body) = do
  st <- ask

  metadata <- metaToContext opts blockListToVimdoc inlineListToVimdoc meta
  main <- blockListToVimdoc body
  title <- inlineListToVimdoc $ docTitle meta
  authors <- traverse inlineListToVimdoc $ docAuthors meta
  let authors' = mconcat $ intersperse ("," <> space) (fmap nowrap authors)
  let tw = writerColumns . writerOptions $ st

  let combinedTitle =
        render (Just tw) . cblock tw $
            (title <> space)
              <> (if null authors' then "" else "by" <> space <> authors')

  -- This is placed here because I couldn't find a way to right-align text
  -- inside template to the width specified by a variable
  let toc_reminder =
        render Nothing . rblock tw $
          ("Type |gO| to see the table of contents." :: Doc Text)

  -- TODO: nicer TOC (like `:h undotree-contents`)
  toc <-
    fmap (render (Just tw)) . blockToVimdoc $
      toTableOfContents opts body

  let modeline = makeModeLine st
  let context =
        defField "body" main
          . defField "toc" (if writerTableOfContents opts then toc else "")
          . defField "modeline" modeline
          . defField "combined-title" combinedTitle
          . defField "toc-reminder" toc_reminder
          $ metadata

  pure $
    case writerTemplate opts of
      Just tpl -> render (Just tw) $ renderTemplate tpl context
      Nothing -> render (Just tw) main

blockListToVimdoc :: (PandocMonad m) => [Block] -> RR m (Doc Text)
blockListToVimdoc blocks = vcat <$> mapM blockToVimdoc blocks

blockToVimdoc :: (PandocMonad m) => Block -> RR m (Doc Text)

blockToVimdoc (Plain inlines) = inlineListToVimdoc inlines

blockToVimdoc (Para inlines) = do
  contents <- inlineListToVimdoc inlines
  pure $ contents <> blankline

blockToVimdoc (LineBlock inliness) = vcat <$> mapM inlineListToVimdoc inliness

blockToVimdoc (CodeBlock (_, cls, _) code) = do
  sw <- asks shiftWidth
  let lang = case cls of
        (lang' : _) -> lang'
        _ -> ""
  -- NOTE: No blankline after the codeblock because closing `<` is concealed
  pure . vcat $
    [ ">" <> literal lang
    , nest sw (literal code)
    , flush "<"
    ]

blockToVimdoc block@(RawBlock format raw) = case format of
  "vimdoc" -> pure $ literal raw
  _ -> "" <$ report (BlockNotRendered block)

-- Should it be formatted as plain text? Vimdoc does not support any form of
-- quotes
blockToVimdoc (BlockQuote blocks) = do
  content <- blockListToVimdoc blocks
  pure $ prefixed "| " content <> blankline

blockToVimdoc (OrderedList listAttr items) = do
  -- TODO: both ordered and bullet- lists have no spacing between items
  -- regargless of content. It may make sense to add blankline if there is a
  -- paragraph or a nested list. (see isParaOrList from Writers/Jats)
  let itemSpacer = if isTightList items then empty else blankline
  let itemsWithMarkers = zip (orderedListMarkers listAttr) items
  items' <- forM itemsWithMarkers $ \(marker, blocks) -> do
    let markerLen = T.length marker

    item' <- indent (markerLen + 1) $ blockListToVimdoc blocks
    pure $ literal marker <> space <> nest (markerLen + 1) item' <> itemSpacer
  pure $ vcat items' <> blankline

blockToVimdoc (BulletList items) = do
  let itemSpacer = if isTightList items then empty else blankline
  items' <- forM items $ \blocks -> do
    let marker = "-"
    item <- indent 2 $ blockListToVimdoc blocks
    pure $ marker <> " " <> nest 2 item <> itemSpacer
  pure $ vcat items' <> blankline

blockToVimdoc (DefinitionList items) = do
  sw <- asks shiftWidth
  let sepAll = if all (isTightList . snd) items then vcat else vsep
  items' <- forM items $ \(term, definitions) -> do
    let sepCur = if isTightList definitions then vcat else vsep
    labeledTerm <- indent sw $ mkVimdocDefinitionTerm term
    definitions' <- indent (2 * sw) $ traverse blockListToVimdoc definitions
    pure $ nest sw labeledTerm <> cr <> nest (2 * sw) (sepCur definitions')
  pure $ sepAll items' <> blankline

-- TODO: reject SoftBreak and LineBreak?
blockToVimdoc (Header level (ref, _, _) inlines) = do
  tw <- asks (writerColumns . writerOptions)
  let rule = case level of
        1 -> T.replicate tw "="
        2 -> T.replicate tw "-"
        _ -> ""
  title <- fmap (render Nothing) . inlineListToVimdoc $ case level of
    3 -> capitalize inlines
    _ -> inlines

  label <- mkVimdocRef ref
  -- One manual space that ensures that even if spaceLeft is 0, title and ref
  -- don't touch each other
  let label' = " " <> label
  let spaceLeft = tw - T.length title

  pure $ vcat
      [ blankline
      , literal rule
      , literal $ title <> T.justifyRight spaceLeft ' ' label'
      , blankline
      ]

blockToVimdoc HorizontalRule = do
  tw <- asks (writerColumns . writerOptions)
  pure $ literal (T.replicate tw "-") <> blankline

-- Based on blockToMarkdown' from Text.Pandoc.Writers.Markdown
blockToVimdoc t@(Table (_, _, _) blkCapt specs thead tbody tfoot) = do
  let isColRowSpans (Cell _ _ rs cs _) = rs > 1 || cs > 1
  let rowHasColRowSpans (Row _ cs) = any isColRowSpans cs
  let tbodyHasColRowSpans (TableBody _ _ rhs rs) =
        any rowHasColRowSpans rhs || any rowHasColRowSpans rs
  let theadHasColRowSpans (TableHead _ rs) = any rowHasColRowSpans rs
  let tfootHasColRowSpans (TableFoot _ rs) = any rowHasColRowSpans rs
  let hasColRowSpans =
        theadHasColRowSpans thead
          || any tbodyHasColRowSpans tbody
          || tfootHasColRowSpans tfoot
  let (caption, aligns, widths, headers, rows) =
        toLegacyTable blkCapt specs thead tbody tfoot
  let numcols =
        maximum $
          length aligns :| length widths : map length (headers : rows)
  caption' <- inlineListToVimdoc caption
  let caption''
        | null caption = blankline
        | otherwise = blankline $$ caption' $$ blankline
  let hasSimpleCells = onlySimpleTableCells $ headers : rows
  let isSimple = hasSimpleCells && all (== 0) widths && not hasColRowSpans
  let isPlainBlock (Plain _) = True
      isPlainBlock _ = False
  let hasBlocks = not (all (all (all isPlainBlock)) $ headers : rows)
  let padRow r = r ++ replicate x empty
       where
        x = numcols - length r
  let aligns' = aligns ++ replicate x AlignDefault
       where
        x = numcols - length aligns
  let widths' = widths ++ replicate x 0.0
       where
        x = numcols - length widths
  sw <- asks shiftWidth
  rawHeaders <- padRow <$> mapM blockListToVimdoc headers
  rawRows <- mapM (fmap padRow . mapM blockListToVimdoc) rows
  let hasHeader = all null headers
  if
    | isSimple -> do
        -- Simple table
        tbl <-
          indent sw $
            pandocTable False hasHeader aligns' widths' rawHeaders rawRows
        pure $ nest sw (tbl $$ caption'') $$ blankline
    | not (hasBlocks || hasColRowSpans) -> do
        -- Multiline table
        tbl <-
          indent sw $
            pandocTable True hasHeader aligns' widths' rawHeaders rawRows
        pure $ nest sw (tbl $$ caption'') $$ blankline
    | otherwise -> ("[TABLE]" $$ caption'') <$ report (BlockNotRendered t)

-- TODO: how to handle figures in a format that can't display them?
-- see how panvimdoc accomplishes it
blockToVimdoc (Figure _ _ blocks) = blockListToVimdoc blocks

blockToVimdoc (Div _ blocks) = blockListToVimdoc blocks

mkVimdocRef :: (Monad m) => Text -> RR m Text
mkVimdocRef ref = do
  pref <- asks vimdocPrefix
  case pref of
    Nothing -> pure $ "*" <> ref <> "*"
    Just pref' -> pure $ "*" <> pref' <> "-" <> ref <> "*"

mkVimdocDefinitionTerm ::
  (PandocMonad m) =>
  [Inline] ->
  RR m (Doc Text)
mkVimdocDefinitionTerm inlines = do
  il <- asks indentLevel
  tw <- asks (writerColumns . writerOptions)
  -- TODO: Maybe it should also check for attributes like `mapping`, so it
  -- will automatically generate labels that are literally the terms
  -- themselves
  -- NOTE: commands in vim are unique, so they get no prefix
  label <- case inlines of
        [Code (ref, _, _) code] | T.isPrefixOf ":" code ->
          pure . Just $ "*" <> ref <> "*"
        [Code (ref, _, _) _] | not (T.null ref) -> Just <$> mkVimdocRef ref
        [Span (ref, _, _) _] | not (T.null ref) -> Just <$> mkVimdocRef ref
        _ -> pure Nothing

  let termLen = offset term
  let labelLen = maybe 0 T.length label

  if il + termLen + labelLen > tw
    then
      pure . mconcat $
        [ case label of
            Nothing -> empty
            Just l -> flush (rblock tw $ literal l) <> cr
        , term
        ]
    else
      pure . mconcat $
        [ term
        , case label of
            Nothing -> empty
            Just l -> rblock (tw - termLen - il) (literal l)
        ]

-- | Write a pandoc-style Markdown table.
pandocTable ::
  (Monad m) =>
  -- | whether this is a multiline table
  Bool ->
  -- | whether the table has a header
  Bool ->
  -- | column alignments
  [Alignment] ->
  -- | column widths
  [Double] ->
  -- | table header cells
  [Doc Text] ->
  -- | table body rows
  [[Doc Text]] ->
  RR m (Doc Text)
pandocTable multiline headless aligns widths rawHeaders rawRows = do
  let isSimple = all (== 0) widths
  let alignHeader alignment = case alignment of
        AlignLeft -> lblock
        AlignCenter -> cblock
        AlignRight -> rblock
        AlignDefault -> lblock
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break.
  -- The @+2@ is needed for specifying the alignment.
  let numChars = (+ 2) . maybe 0 maximum . nonEmpty . map offset
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break *inside a word*.
  -- The @+2@ is needed for specifying the alignment.
  let minNumChars = (+ 2) . maybe 0 maximum . nonEmpty . map minOffset
  let columns = transpose (rawHeaders : rawRows)

  il <- asks indentLevel

  -- x = (2 * length columns)         -- spaces for specifying the alignment
  -- y = (length columns - 1)         -- spaces between the columns
  -- x + y = (3 * length columns - 1) -- total needed correction
  tw <- asks (writerColumns . writerOptions)
  let tw' = tw - il - 3 * length columns + 1
  wrap <- asks (writerWrapText . writerOptions)

  -- minimal column width without wrapping a single word
  let relWidth w col =
        max
          (floor $ fromIntegral (tw' - 1) * w)
          ( if wrap == WrapAuto
              then minNumChars col
              else numChars col
          )
  let widthsInChars
        | isSimple = map numChars columns
        | otherwise = zipWith relWidth widths columns
  let makeRow =
        hcat
          . intersperse (lblock 1 (literal " "))
          . zipWith3 alignHeader aligns widthsInChars
  let rows' = map makeRow rawRows
  -- TODO: reduce tw in case head is not empty
  let head' = makeRow rawHeaders <> " ~"
  let head'' =
        if headless
          then empty
          else head'
  let body =
        if multiline
          then
            vsep rows'
              $$ if length rows' < 2
                then blankline -- #4578
                else empty
          else vcat rows'
  return $
    blankline
      $$ head''
      $$ (if multiline then blankline else empty)
      $$ body

inlineListToVimdoc :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
inlineListToVimdoc inlines = hcat <$> mapM inlineToVimdoc inlines

inlineToVimdoc :: (PandocMonad m) => Inline -> RR m (Doc Text)

inlineToVimdoc (Str str) = pure $ literal str

-- Neither `:h help-writing`, nor neovim's grammar.js for vimdoc and
-- highlights.scm say anything about styling text, so we strip all the
-- formatting
inlineToVimdoc (Emph inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Underline inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Strong inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Strikeout inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Superscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Subscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (SmallCaps inlines) = inlineListToVimdoc inlines

inlineToVimdoc (Quoted typ inlines) =
  let quote = case typ of SingleQuote -> "'"; DoubleQuote -> "\""
   in inlineListToVimdoc inlines >>= \text -> pure (quote <> text <> quote)

-- TODO: is there reasonable syntax? What does markdown do?
inlineToVimdoc (Cite _citations inlines) = inlineListToVimdoc inlines

{- FOURMOLU_DISABLE -}
inlineToVimdoc (Code (_, cls, _) code) = do
  let hasNoLang = null cls
  pure . literal $ case T.words code of
    [":help", ref] | hasNoLang -> "|" <> ref <> "|"
    [":h", ref]    | hasNoLang -> "|" <> ref <> "|"
    _                          -> "`" <> code <> "`"
{- FOURMOLU_ENABLE -}

inlineToVimdoc Space = pure space
inlineToVimdoc SoftBreak =
  asks (writerWrapText . writerOptions) >>= \case
    WrapAuto -> pure space
    WrapNone -> pure space
    WrapPreserve -> pure "\n"

-- Are line breaks always allowed?
inlineToVimdoc LineBreak = pure "\n"

-- TODO: is it the best way to handle this?
inlineToVimdoc (Math _mathType math) = pure . literal $ "`$" <> math <> "$`"

inlineToVimdoc inline@(RawInline (Format format) text) = case format of
  "vimdoc" -> pure $ literal text
  _ -> "" <$ report (InlineNotRendered inline)

inlineToVimdoc (Link _ txt (src, _title)) = do
  txt' <- render Nothing <$> inlineListToVimdoc txt

  let isShortlink = case txt of
        [Str x] | escapeURI x == src -> True
        _ -> False

  let delim =
        if " " `T.isSuffixOf` txt' && not (T.null txt')
          then ""
          else " "

  pure . literal $ case refdocLinkToLink src of
    Right link | isShortlink -> "|" <> link <> "|"
    Right link -> txt' <> delim <> "|" <> link <> "|"
    Left _ | isURI src, isShortlink -> src
    Left _
      | "#" `T.isPrefixOf` src ->
          -- TODO: something more elegant?
          let src' = fromJust (T.stripPrefix "#" src)
           in txt' <> delim <> "|" <> src' <> "|"
    -- TODO: vimdoc-TS does not seem to expect any extra characters around URL:
    -- https://github.com/neovim/tree-sitter-vimdoc/blob/ffa29e863738adfc1496717c4acb7aae92a80ed4/grammar.js#L225
    Left _ -> txt' <> delim <> src

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
