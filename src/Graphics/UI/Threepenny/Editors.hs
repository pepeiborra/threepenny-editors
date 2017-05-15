
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
  , field
    -- ** Editor constructors
  , editorUnit
  , editorReadShow
  , editorEnumBounded
  , editorSum
  , editorJust
    -- ** Generic editors
  , editorGeneric
  , editorGenericSimple
  )where

import Graphics.UI.Threepenny.Editors.Profunctor
