{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-----------------------------------------------------------------------------
    Example from threepenny-gui extended with Editors:

    Small database with CRUD operations and filtering.
    To keep things simple, the list box is rebuild every time
    that the database is updated. This is perfectly fine for rapid prototyping.
    A more sophisticated approach would use incremental updates.
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo          #-}
module CRUD
  ( main
  , Database
  , DatabaseKey
  , create
  , update
  , delete
  , lookup
  ) where
import           Control.Monad                  (void)
import           Data.List                      (isPrefixOf)
import qualified Data.Map                       as Map
import           Data.Maybe
import           Generics.SOP.TH
import           Prelude                        hiding (lookup)

import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core    hiding (delete)
import           Graphics.UI.Threepenny.Editors hiding (create)
import qualified Graphics.UI.Threepenny.Editors as Editors

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ mdo
    return window # set title "CRUD Example (Simple)"

    -- GUI elements
    createBtn   <- UI.button #+ [string "Create"]
    deleteBtn   <- UI.button #+ [string "Delete"]
    listBox     <- UI.listBox  bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry    bFilterString
    dataItem    <- Editors.create editor (fromMaybe (DataItem "" "") <$> bSelectionDataItem)

    -- GUI layout
    element listBox # set (attr "size") "10" # set style [("width","200px")]

    let glue = string " "
    getBody window #+ [grid
        [[row [string "Filter prefix:", element filterEntry], glue]
        ,[element listBox, render dataItem]
        ,[row [element createBtn, element deleteBtn], glue]
        ]]

    -- events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry
    let tFilter = isPrefixOf <$> UI.userText filterEntry
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = edited dataItem
        eCreate     = UI.click createBtn
        eDelete     = UI.click deleteBtn

    -- database
    -- bDatabase :: Behavior (Database DataItem)
    let update' mkey x = flip update x <$> mkey
    bDatabase <- accumB emptydb $ concatenate <$> unions
        [ create (DataItem "Emil" "Example") <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]

    -- selection
    -- bSelection :: Behavior (Maybe DatabaseKey)
    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    -- automatically enable / disable editing
    let
        bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelection

    element deleteBtn # sink UI.enabled bDisplayItem
    element (firstName $ widgetControl dataItem) # sink UI.enabled bDisplayItem
    element (lastName  $ widgetControl dataItem) # sink UI.enabled bDisplayItem


{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb = Database 0 Map.empty
keys    = Map.keys . db

create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db
delete key   (Database newkey db) = Database newkey     $ Map.delete key db
lookup key   (Database _      db) = Map.lookup key db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
data DataItemDual (purpose :: Purpose) = DataItem
  { firstName, lastName :: Field purpose String }

type DataItem = DataItemDual Data
type DataItemEditor = DataItemDual Edit

showDataItem :: DataItem -> String
showDataItem DataItem{..} = lastName ++ ", " ++ firstName

instance HasEmpty DataItem

instance Editable DataItem where
  type EditorWidget DataItem = DataItemEditor
  editor = editorGenericBi

instance Renderable DataItemEditor where
  render = renderGeneric

deriveGeneric ''DataItemDual
