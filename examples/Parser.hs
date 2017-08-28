{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecursiveDo #-}
module Parser(main) where

import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Editors

import           Control.Monad
import           Data.Maybe
import           Language.Haskell.Exts


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

example :: String
example = unlines
    ["module Main(main) where"
    ,""
    ,"main :: IO ()"
    ,"main = putStrLn \"Hello World!\""
    ]


parser :: ParseMode -> String -> String
parser mode x = case parseFileContentsWithMode mode x of
    ParseOk x       -> show $ Control.Monad.void x
    x@ParseFailed{} -> show x


pMode :: UI (Element, Behavior ParseMode)
pMode = do
    filename <- UI.input
    language <- UI.select
        #+ [UI.option # set text (show x) | x <- knownLanguages]
        # set UI.selection (Just 0)
    ignore <- UI.input # set (attr "type") "checkbox"

    layout <- grid
        [[string "Filename", element filename]
        ,[string "Language", element language]
        ,[string "Ignore pragmas", element ignore]]

    filename <- stepper "" $ UI.valueChange filename
    language <- fmap (fmap ((!!) knownLanguages . fromMaybe 0)) $ stepper Nothing $ UI.selectionChange language
    ignore <- stepper False $ UI.checkedChange ignore

    let val = (\f l i -> defaultParseMode{parseFilename=f, baseLanguage=l, ignoreLanguagePragmas=i}) <$>
            filename <*> language <*> ignore
    return (layout, val)

pModeEditor :: Editor ParseMode Layout ParseMode
pModeEditor =
  (\f l i  -> defaultParseMode{parseFilename=f, baseLanguage=l, ignoreLanguagePragmas=i})
    <$> field "Filename" parseFilename editor
    -*- field "Language" baseLanguage (editorJust $ editorSelection (pure knownLanguages) (pure (string . show)))
    -*- field "Ignore pragmas" ignoreLanguagePragmas editor

pMode' :: UI (UI Element, Behavior ParseMode)
pMode' = mdo
  parseModeE <- create pModeEditor parseModeB
  parseModeB <- stepper defaultParseMode (edited parseModeE)
  return (render parseModeE, parseModeB)

setup :: Window -> UI ()
setup window = void $ do
    _ <- return window # set title "HSE parser"
    mode <- pMode
    mode2 <- pMode'

    source <- UI.textarea
        # set UI.style [("width","600px"),("height","250px")]
        # set value example
    sourceVal <- stepper example $ UI.valueChange source

    output <- UI.textarea
        # set UI.style [("width","600px"),("height","250px")]
        # set (attr "readonly") "readonly"
        # sink value (parser <$> snd mode <*> sourceVal)

    getBody window #+ [row [column [element source, element output], element $ fst mode, fst mode2]]
