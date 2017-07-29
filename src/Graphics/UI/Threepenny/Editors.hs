
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , EditorFactory
  , createEditor
  , Editable(..)
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , Vertically(..)
  , Horizontally(..)
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
    -- ** Generic editors
  , editorGeneric
  , editorGenericSimple
    -- ** Layouts
  , Layout(Grid, Single)
  , horizontal
  , vertical
  , runLayout
  )where

import Graphics.UI.Threepenny.Editors.Profunctor
