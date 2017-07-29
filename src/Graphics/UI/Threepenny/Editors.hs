
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , Base.editorElement
  , Base.editorTidings
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

import qualified Graphics.UI.Threepenny.Editors.Base as Base
import Graphics.UI.Threepenny.Editors.Layout
import Graphics.UI.Threepenny.Editors.Profunctor
