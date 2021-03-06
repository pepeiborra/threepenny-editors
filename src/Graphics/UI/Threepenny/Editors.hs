{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-duplicate-exports  #-}
{- | Types and combinators to create widgets for editing algebraic datatypes.

   This module builds around the idea that editors usually have the same shape
   as the data they are editing. We can immediately take advantage of this to
   automatically build editors from datatype definitions.

> data Person = Person { first, last, email :: String, age :: Int }
>
> deriveGeneric ''Person
>
> instance Editable Person

   This produces a generic editor with a fixed vertical layout. To customize
   the layout, we can use a explicit instance and monoidal layout builders:

> instance Editable Person where
>   editor = Person <$> fieldLayout Next  "First:" first editor
>                   <*> fieldLayout Break "Last:"  last  editor
>                   <*> fieldLayout Next  "Email:" email editor
>                   <*> fieldLayout Next  "Age:"   age   editor

   We can take this a step further by repurposing datatype definitions to
   represent not only data, but also the collections of editors that are composed
   to build the datatype editor. This is done via the 'Purpose' type
   and the 'Field' type family.

> data Person purpose =
>   Person { first, last, email :: Field purpose String
>          , age                :: Field purpose Int}
>
> deriveGeneric ''Person
>
> instance Editable (Person Data) where
>   type EditorWidget (Person Data) = Person Edit
>   editor = editorGenericBi
>
> instance Renderable (Person Edit) where
>   render = renderGeneric

'renderGeneric' will produce a vertical layout. A direct implementation would use standard threepenny layout combinators since the fields of @Person Edit@ are instances of 'Widget':

> instance Renderable (Person Edit) where
>   render Person{..} =
>     grid [[string "First:", element first, string "Email:", element email]
>          ,[string "Last:",  element last, string "Age:", element age]
>          ]

-}
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , Editable(..)
  , dimapE
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , fieldLayout
  , withSomeWidget
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
  , editorString
  , editorText
  , editorCheckBox
  , editorList
  , editorCollection
  , EditorCollection
  , someEditor
  , withSomeWidget
    -- ** Dual purpose datatypes
  , Field
  , ListField
  , Purpose(..)
    -- ** Generic editors
  , editorGeneric
  , editorGenericBi
    -- * Widgets
  , GenericWidget(..)
  , edited
  , contents
    -- * Layouts
  , Layout
  -- ** Monoidal layouts
  , Vertical(..)
  , Horizontal(..)
  , Columns(..)
  -- ** Custom layout definition
  , Renderable(..)
  , renderGeneric
  , getLayoutGeneric
    -- ** Representing empty values
  , HasEmpty(..)
  ) where

import           Data.Biapplicative
import           Data.Char
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.HasEmpty
import           Data.Kind
import           Data.Maybe
import qualified Data.Sequence                         as Seq
import           Data.Text                             (Text)
import           Generics.SOP                          hiding (Compose)
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Widgets
import           Text.Casing

import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Types

-- | The class of 'Editable' datatypes.
--
--   There are several ways to create an instance, from easiest to most advanced:
--
--   * Automatically (via 'SOP'), producing an editor with a vertical layout:
--
-- > instance Editable MyDatatype
--
--   * Using the applicative layout combinators:
--
-- >  instance Editable MyDatatype where
-- >    editor = MyDatatype <$> field "Name:" name editor
-- >                        -*- field "Age:"  age  editor
--
--   * Using a monoidal layout builder:
--
-- >  instance Editable MyDatatype where
-- >    editor = MyDatatype <$> fieldLayout Break "Name:" name editor
-- >                        <*> fieldLayout Next  "Age:"  age  editor
--
--   * Using a dual purpose datatype, leaving the layout details for the 'Renderable' instance.
--
-- > instance Editable (MyDatatype Data) where
-- >   type EditorWidget (MyDatatype Data) = MyDatatype Edit
-- >   editor = editorGenericBi
--
class (HasEmpty a, Renderable (EditorWidget a), Renderable (ListEditorWidget a)) => Editable a where
  -- | The widget type that realizes the editor. Defaults to 'Layout' and only needs to be manually defined when using custom renderables.
  type family EditorWidget a
  -- | The widget type that realizes the editor for lists. Defaults to ''EditorCollection'.
  type family ListEditorWidget a
  type EditorWidget a = Layout
  type ListEditorWidget a = EditorCollection Int (EditorWidget a)

  editor :: Editor a (EditorWidget a) a
  listEditor :: Editor [a] (ListEditorWidget a) [a]

  default editor :: (Generic a, HasDatatypeInfo a, (All (All Editable `And` All HasEmpty) (Code a)), EditorWidget a ~ Layout) => Editor a (EditorWidget a) a
  editor = editorGeneric

  default listEditor :: (HasEmpty a, ListEditorWidget a ~ EditorCollection Int (EditorWidget a)) => Editor [a] (ListEditorWidget a) [a]
  listEditor = fmap snd $ Editor $ \ba -> mdo
    e <- create (editorList editor) (liftA2 (,) bIndex ba)
    bIndex <- stepper Nothing (fst <$> edited e)
    return e

-- | Conceal the widget type of some 'Editor'
withSomeWidget :: Renderable w => Editor a w b -> Editor a Layout b
withSomeWidget = first getLayout

-- | A version of 'editor' with a concealed widget type.
someEditor :: Editable a => Editor a Layout a
someEditor = withSomeWidget editor

-- | A container for 'EditorWidget'.
data EditorWidgetFor a where
  EditorWidgetFor :: Editable a => EditorWidget a -> EditorWidgetFor a

-- | 'Purpose' is a kind for type level 'Field's
data Purpose = Data | Edit

-- | Type level fields. Used to define dual purpose datatype constructors, which can be instantiated to either store data or widgets.
--   Example:
--
-- > data PersonF (purpose :: Purpose) = Person
-- >   { education           :: Field purpose Education
-- >   , firstName, lastName :: Field purpose Text
-- >   , age                 :: Field purpose (Maybe Int)
-- >   }
--
-- > type Person = PersonF Data
-- > type PersonEditor = PersonF Edit
type family Field (purpose :: Purpose) a where
  Field 'Data  a = a
  Field 'Edit  a  = EditorWidget a

-- | List version of 'Field'. Example:
--
-- > data PersonF (purpose :: Purpose) = Person
-- >   { education           :: Field purpose Education
-- >   , firstName, lastName :: Field purpose Text
-- >   , age                 :: Field purpose (Maybe Int)
-- >   , addresses           :: ListField purpose String
-- >   }
--
-- > type Person = PersonF Data
-- > type PersonEditor = PersonF Edit
type family ListField (purpose :: Purpose) a where
  ListField 'Data  a = [a]
  ListField 'Edit  a  = ListEditorWidget a

instance Editable () where
  type EditorWidget () = Element
  editor = editorUnit

instance Editable Char where
  type EditorWidget Char = TextEntry
  type ListEditorWidget Char = TextEntry
  editor = editorJust editorReadShow
  listEditor = editorString

instance Editable a => Editable [a] where
  type EditorWidget [a] = ListEditorWidget a
  editor = listEditor

instance Editable Text where
  type EditorWidget Text = TextEntry
  editor = editorText

instance Editable Bool where
  type EditorWidget Bool = Element
  editor = editorCheckBox

instance Editable (Maybe Int) where
  type EditorWidget (Maybe Int) = TextEntry
  editor = editorReadShow
instance Editable (Maybe Double) where
  type EditorWidget (Maybe Double) = TextEntry
  editor = editorReadShow
instance Editable Int where
  type EditorWidget Int = TextEntry
  editor = editorJust editor
instance Editable Double where
  type EditorWidget Double = TextEntry
  editor = editorJust editor

instance (Editable a, Editable b) => Editable (a |*| b) where
  type EditorWidget (a |*| b) = EditorWidget a |*| EditorWidget b
  editor = bipure (:|*|) (:|*|) <<*>> dimapE (\(x :|*| _) -> x) id editor
                                <<*>> dimapE (\(_ :|*| y) -> y) id editor

instance (Editable a, Editable b) => Editable (a -*- b) where
  type EditorWidget (a -*- b) = EditorWidget a -*- EditorWidget b
  editor = bipure (:-*-) (:-*-) <<*>> dimapE (\(x :-*- _) -> x) id editor
                                <<*>> dimapE (\(_ :-*- y) -> y) id editor

instance Editable a => Editable (Identity a) where
  type EditorWidget (Identity a) = EditorWidget a
  editor = editorIdentity editor

{----------------------------------------------
  Generic derivations for Renderable datatypes
-----------------------------------------------}
-- | A generic 'render' derivation for data types with a single constructor
--   which renders the (labelled) fields in a vertical layout.
--   For custom layouts use `getLayoutGeneric`.
--
-- /e.g./ given the declarations
--
-- @
-- data PersonEditor = PersonEditor { firstName, lastName :: EditorWidget String }
-- deriveGeneric ''PersonEditor
-- @
--
-- using `renderGeneric` to instantiate `Renderable`
--
-- @
-- instance Renderable PersonEditor where
--   getLayout = renderGeneric
-- @
--
-- will be equivalent to writing the below by hand
--
-- @
-- instance Renderable PersonEditor where
--   getLayout PersonEditor{..} =
--       grid [ [string "First name:", element firstName]
--            , [string "Last name:",  element lastName ]
--            ]
-- @

renderGeneric
  :: forall a xs.
     (Generic a, HasDatatypeInfo a, All Renderable xs, Code a ~ '[xs])
  => a -> UI Element
renderGeneric = render . (Grid . Seq.fromList . fmap Seq.fromList) . getLayoutGeneric

-- | A helper to implement `getLayout` for data types with a single constructor.
--   Given a value, `getLayoutGeneric` returns a grid of `Layout`s with one row per field.
--   Rows can carry one element, for unnamed fields; two elements, for named fields; or three elements, for operators.
getLayoutGeneric
  :: forall a xs.
     (Generic a, HasDatatypeInfo a, All Renderable xs, Code a ~ '[xs])
  => a -> [[Layout]]
getLayoutGeneric = getLayoutGeneric' (datatypeInfo (Proxy @ a)) . from

getLayoutGeneric' :: (All Renderable xs) => DatatypeInfo '[xs] -> SOP I '[xs] -> [[Layout]]
getLayoutGeneric' (ADT _ _ (c :* Nil)) (SOP (Z x)) = getLayoutConstructor c x
getLayoutGeneric' (Newtype _ _ c) (SOP (Z x))      = getLayoutConstructor c x
getLayoutGeneric' _ _                              = error "unreachable"

getLayoutConstructor :: All Renderable xs => ConstructorInfo xs -> NP I xs -> [[Layout]]
getLayoutConstructor (Record _ fields) renders = hcollapse $ hcliftA2 (Proxy @ Renderable) (\f (I x) -> K $ getLayoutField f x) fields renders
getLayoutConstructor Constructor{} renders = hcollapse $ hcliftA (Proxy @ Renderable) (\(I x) -> K [getLayout x]) renders
getLayoutConstructor (Infix name _ _) (I r1 :* I r2 :* Nil) = [[ getLayout r1, getLayout(string name), getLayout r2]]

getLayoutField :: Renderable x => FieldInfo x -> x -> [Layout]
getLayoutField (FieldInfo name) x =  [getLayout(toFieldLabel name), getLayout x]

{----------------------------------------------
  Generic derivations for Biapplicative editors
-----------------------------------------------}
-- | A generic 'editor' derivation for dual purpose datatypes with a single constructor.
--
--   /e.g./ for the datatype
--
-- @
--  data Person purpose = Person { firstName, lastName :: Field purpose String }
-- @
--
-- @ instance Editable (Person Data) where
--     type EditorWidget (Person Data) = Person Edit
--     editor = editorGenericBi
-- @
--
--   will be equivalent to
--
-- > instance Editable (Person Data) where
-- >  type EditorWidget (Person Data) = Person Edit
-- >  editor = bipure DataItem DataItem
-- >              <<*>> edit firstName editor
-- >              <<*>> edit lastName editor
--
editorGenericBi
  :: forall xs typ .
     ( Generic (typ 'Data)
     , Generic (typ 'Edit)
     , All Editable xs
     , Code (typ 'Data) ~ '[xs]
     , Code (typ 'Edit) ~ '[EditorWidgetsFor xs]
     )
  => Editor (typ 'Data) (typ 'Edit) (typ 'Data)
editorGenericBi = dimapE from to $ bimap to id constructorEditorBi

constructorEditorBi
  :: forall xs . (All Editable xs)
  => Editor (SOP I '[xs]) (SOP I '[EditorWidgetsFor xs]) (SOP I '[xs])
constructorEditorBi = dimapE (unZ . unSOP) (SOP . Z) . bimap (SOP . Z . unpackWidgets) id $ constructorEditorBi'

constructorEditorBi' :: (SListI xs, All Editable xs) => Editor (NP I xs) (NP EditorWidgetFor xs) (NP I xs)
constructorEditorBi' = sequence_NP2 fieldsEditorBi

unpackWidgets :: NP EditorWidgetFor xs -> NP I (EditorWidgetsFor xs)
unpackWidgets Nil                       = Nil
unpackWidgets (EditorWidgetFor e :* xs) = I e :* unpackWidgets xs

type family EditorWidgetsFor (xs :: [*]) where
  EditorWidgetsFor '[] = '[]
  EditorWidgetsFor (x ': xs) = EditorWidget x ': EditorWidgetsFor xs

fieldsEditorBi :: forall xs . All Editable xs => NP2 EditorWidgetFor (Editor (NP I xs)) xs
fieldsEditorBi = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP2 EditorWidgetFor (Editor (NP I xs)) ys
  go _ SNil = Nil2
  go f SCons = bimap EditorWidgetFor id (dimapE (unI . hd . f) id editor) :** go (tl . f) sList

{----------------------------------------------
  Generic derivations for Applicative Editables
-----------------------------------------------}

-- A bifunctorial version of NP, used to sequence the applicative effects
data NP2 :: (k -> *) -> (* -> k -> *) -> [k] -> * where
  Nil2 :: NP2 ann f '[]
  (:**) :: f (ann x) x -> NP2 ann f xs -> NP2 ann f (x ': xs)

sequence_NP2 :: Biapplicative f => NP2 w f xs -> f (NP w xs) (NP I xs)
sequence_NP2 Nil2 = bipure Nil Nil
sequence_NP2 (x :** xs) = bipure (:*) (\x xx -> I x :* xx) <<*>> x <<*>> sequence_NP2 xs

constructorEditorFor
  :: (All Editable xs, All HasEmpty xs)
  => ConstructorInfo xs
  -> Editor (SOP I '[xs]) Layout (SOP I '[xs])
constructorEditorFor (Record _ fields) = dimapE (unZ . unSOP) (SOP . Z) $ constructorEditorFor' fields
constructorEditorFor (Constructor _) = dimapE (unZ . unSOP) (SOP . Z) someEditor
constructorEditorFor Infix{} = dimapE (unZ . unSOP) (SOP . Z) someEditor

-- | A generic 'editor' derivation for SOP types.
--
--   The datatype arguments are layered in vertical fashion and labelled with
--   field names if available.
editorGeneric
  :: forall a .
     (Generic a, HasDatatypeInfo a, (All (All Editable `And` All HasEmpty) (Code a)))
  => Editor a Layout a
editorGeneric = dimapE from to $ editorGeneric' (datatypeInfo(Proxy @ a))

editorGeneric'
  :: forall xx.
     (All (All Editable `And` All HasEmpty) xx)
  => DatatypeInfo xx -> Editor (SOP I xx) Layout (SOP I xx)
editorGeneric' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGeneric' (ADT _ _ cc) = editorSum above editors constructor where
  editors :: [(Tag, Editor (SOP I xx) Layout (SOP I xx))]
  editors = first Tag <$> constructorEditorsFor cc
  constructors = hmap (K . constructorName) cc
  constructor a = Tag $ hcollapse $ hliftA2 const constructors (unSOP a)
editorGeneric' (Newtype _ _ c) = constructorEditorFor c

newtype Tag = Tag String deriving (Eq, Ord)
instance Show Tag where show (Tag t) = init $ toFieldLabel t

constructorEditorsFor
  :: forall xx . (All (All Editable `And` All HasEmpty) xx)
  => NP ConstructorInfo xx -> [(String, Editor (SOP I xx) Layout (SOP I xx))]
constructorEditorsFor cc =
  hcollapse $ hcliftA3 p (\c i p -> (constructorName c,) `mapKK` constructorEditorForUnion c i p) cc
    (injections  :: NP (Injection  (NP I) xx) xx)
    (projections :: NP (Projection (Compose Maybe (NP I)) xx) xx)
  where
    p = Proxy @ (All Editable `And` All HasEmpty)

constructorEditorForUnion
  :: (SListI xx, All Editable xs, All HasEmpty xs)
  => ConstructorInfo xs
  -> Injection (NP I) xx xs
  -> Projection (Compose Maybe (NP I)) xx xs
  -> K (Editor (SOP I xx) Layout (SOP I xx)) xs
constructorEditorForUnion (Constructor _) inj prj = K $ composeEditor inj prj editor
constructorEditorForUnion Infix{} inj prj = K $ composeEditor inj prj editor
constructorEditorForUnion (Record _ fields) inj prj = K $ composeEditor inj prj $ constructorEditorFor' fields

composeEditor
  :: forall xss xs.
    (SListI xss, All HasEmpty xs) =>
     Injection (NP I) xss xs
  -> Projection (Compose Maybe (NP I)) xss xs
  -> Editor (NP I xs) Layout (NP I xs)
  -> Editor (SOP I xss) Layout (SOP I xss)
composeEditor (Fn inj) (Fn prj) = dimapE f (SOP . unK . inj)
  where
    f = fromMaybe emptyValue . getCompose . prj . K . hexpand (Compose Nothing) . hmap (Compose . Just) . unSOP

instance HasEmpty a => HasEmpty (I a) where emptyValue = I emptyValue
instance All HasEmpty xs => HasEmpty (NP I xs) where
  emptyValue = hcpure (Proxy @ HasEmpty) emptyValue

constructorEditorFor' :: (SListI xs, All Editable xs) => NP FieldInfo xs -> Editor (NP I xs) Layout (NP I xs)
constructorEditorFor' fields = vertically $ hsequence $ hliftA Vertically $ fieldsEditor (hliftA (K . fieldName) fields)

-- Tuple editor without fields
instance (All Editable xs, All HasEmpty xs) => Editable (NP I xs) where
  type EditorWidget (NP I xs) = Layout
  editor = horizontally $ hsequence $ hliftA Horizontally tupleEditor

tupleEditor :: forall xs . All Editable xs => NP (Editor (NP I xs) Layout) xs
tupleEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (Editor (NP I xs) Layout) ys
  go _ SNil  = Nil
  go f SCons = dimapE (unI . hd . f) id someEditor :* go (tl . f) sList

fieldsEditor :: forall xs . All Editable xs => NP (K String) xs -> NP (Editor (NP I xs) Layout) xs
fieldsEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (K String) ys -> NP (Editor (NP I xs) Layout) ys
  go _ SNil Nil = Nil
  go f SCons (K fn :* xs) = field (toFieldLabel fn) (unI . hd . f) someEditor :* go (tl . f) sList xs

toFieldLabel :: String -> String
toFieldLabel (fromAny -> Identifier (x:xx)) =
  unwords (onHead toUpper x : fmap (onHead toLower) xx) ++ ":"
    where
      onHead f (x:xx) = f x : xx
      onHead _ []     = []
toFieldLabel _ = ""
