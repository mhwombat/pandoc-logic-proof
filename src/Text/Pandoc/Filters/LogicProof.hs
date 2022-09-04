{-|
Module      : LogicProof
Description : Provides a way to write locig proofs in Markdown.
Copyright   : (c) Amy de Buitléir, 2020-2022
License     : BSD--3
Maintainer  : amy@nualeargais.ie
Stability   : experimental
Portability : POSIX

See <https://github.com/mhwombat/pandoc-logic-proof> for information
on how to use this filter.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filters.LogicProof
  (
    transform,
    formatProofs
  ) where

import Data.Foldable    (foldl')
import Data.Text        qualified as T
import Text.Pandoc      qualified as P
import Text.Pandoc.Walk (walk)


-- | A transformation that can be used with Hakyll.
transform :: P.Pandoc -> P.Pandoc
transform = walk formatProofs

-- | Exported for use by the executable.
formatProofs :: P.Block -> P.Block
formatProofs x@(P.CodeBlock (_,cs,_) s)
  | null cs                = x
  | head cs == "logicproof" = proofToTable . renumber $ parseProof s
  | otherwise              = x
formatProofs x = x

type Proof = [ProofRow]

type ProofRow = [T.Text]

proofToTable :: Proof -> P.Block
proofToTable p = P.Table attr defaultTableCaption colSpecs
                    defaultTableHeader [toTableBody p]
                    defaultTableFooter
  where attr = ("",["logicproof"],[])
        colSpecs = replicate 3 defaultColSpec

toTableBody :: Proof -> P.TableBody
toTableBody p = P.TableBody P.nullAttr (P.RowHeadColumns 0) []
                  $  map toTableRow p

toTableRow :: ProofRow -> P.Row
toTableRow row
  | length row < 3 = error "short row in logic proof"
  | otherwise      = P.Row P.nullAttr cells
  where label = head row
        justification = last row
        statement = penultimate row
        depth = length row - 3
        statement' = indent depth statement
        cells = [
                  textToCell label,
                  blocksToCell . map removePara $ parseBlocks statement',
                  blocksToCell . map removePara $ parseBlocks justification
                ]

removePara :: P.Block -> P.Block
removePara (P.Para xs) = P.Plain xs
removePara x           = x

penultimate :: [a] -> a
penultimate = last . init

indent :: Int -> T.Text -> T.Text
indent 0 s = s
indent n s = filler `T.append` s
  where filler = T.pack . concat $ replicate n
                     "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"

blocksToCell :: [P.Block] -> P.Cell
blocksToCell
  = P.Cell P.nullAttr P.AlignDefault (P.RowSpan 1) (P.ColSpan 1)

textToCell :: T.Text -> P.Cell
textToCell t
  = P.Cell P.nullAttr P.AlignDefault (P.RowSpan 1) (P.ColSpan 1)
      [ P.Plain [P.Str t] ]

parseProof :: T.Text -> Proof
parseProof = map (map trim  . T.splitOn "|") . T.lines

renumber :: Proof -> Proof
renumber proofRows = map (fillInReferences refs) proofRows'
  where proofRows' = renumber' proofRows
        refs = makeLookupTable proofRows'

renumber' :: Proof -> Proof
renumber' = zipWith f [(1 :: Int)..]
  where f n row = T.pack (show n) : row

makeLookupTable :: Proof -> [(T.Text, T.Text)]
makeLookupTable = map f
  where f (new : old : _) = (T.pack ("(@" ++ T.unpack old ++ ")"), new)
        f _               = error "short row in logic proof"

fillInReferences :: [(T.Text, T.Text)] -> ProofRow -> ProofRow
fillInReferences refs (label : _ : fields)
  = (label `T.append` ".") : map (multiReplace refs) fields
fillInReferences _ _ = error "short row in logic proof"

multiReplace :: [(T.Text, T.Text)] -> T.Text -> T.Text
multiReplace refs haystack = foldl' replace haystack refs

replace :: T.Text -> (T.Text, T.Text) -> T.Text
replace haystack (needle, label) = T.replace needle label haystack

trim :: T.Text -> T.Text
trim s
  | T.null s       = s
  | T.head s == ' ' = trim (T.tail s)
  | T.last s == ' ' = trim (T.init s)
  | otherwise      = s




readDefaults :: P.ReaderOptions
readDefaults = P.def { P.readerStandalone = True,
                       P.readerExtensions = P.pandocExtensions }

parseBlocks :: T.Text -> [P.Block]
parseBlocks s = bs
  where (Right (P.Pandoc _ bs)) = P.runPure $ P.readMarkdown readDefaults s

defaultColSpec :: P.ColSpec
defaultColSpec = (P.AlignDefault, P.ColWidthDefault)

defaultTableCaption :: P.Caption
defaultTableCaption = P.Caption Nothing []

defaultTableHeader :: P.TableHead
defaultTableHeader = P.TableHead P.nullAttr []

defaultTableFooter :: P.TableFoot
defaultTableFooter = P.TableFoot P.nullAttr []
