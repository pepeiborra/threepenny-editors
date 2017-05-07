
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , EditorFactory
  , createEditor
  , Editable(..)
    -- ** Editor compoosition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorUnit
  , editorReadShow
  , editorEnumBounded
  , editorSum
  , withDefault
  -- * Reexports
  , Compose(..)
  )where

import Graphics.UI.Threepenny.Editors.Profunctor
