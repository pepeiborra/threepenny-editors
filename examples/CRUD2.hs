{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeApplications      #-}
module CRUD2 (main) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Person2                              hiding (main)

import qualified Graphics.UI.Threepenny               as UI
import           Graphics.UI.Threepenny.Core          hiding (delete)
import           Graphics.UI.Threepenny.Editors       hiding (create)
import qualified Graphics.UI.Threepenny.Editors       as Editors
import           Graphics.UI.Threepenny.Editors.Types

type Database = Map Int Person

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ mdo
    filterEditor <- Editors.create (editor @String) bFilter
    dbEditor     <- Editors.create (editorCollection dbEditorConfig editor) db

    -- debug <- UI.new # sink text (show <$> db)

    db :: Behavior (Maybe Int, Database) <-
      stepper (Nothing, mempty) $ head <$> unions
        [ edited dbEditor
        , (\kk -> first( (`mplus` (Set.lookupMin kk))
                       . (>>= \k -> guard (k `Set.member` kk) >> return k)))
          <$> options <*> db <@ edited filterEditor
        ]

    bFilter <- stepper "" (edited filterEditor)

    let dbEditorConfig ba = (defaultEditorCollectionConfig ba)
          { eccDisplay  = displayPerson . snd <$> ba
          , eccOptions  = options
          }
        options = applyFilter <$> bFilter <*> db

    void $ return window # set title "CRUD Example (editorCollection)"
    getBody window
      #+ [column [ render filterEditor
                 , string ""
                 , render dbEditor
                 , string ""
                 --, render debug
                 ]]
  where
   displayPerson db i =
     UI.option # set text (maybe "" showPerson (Map.lookup i db))
   showPerson :: Person -> String
   showPerson Person{..} = lastName ++ ", " ++ firstName

   applyFilter :: String -> (_,Database) -> Set Int
   applyFilter pat (_,db) =
     Set.fromList [ k | (k,Person{..}) <- Map.toList db
                      , pat `isPrefixOf` firstName || pat `isPrefixOf` lastName]

