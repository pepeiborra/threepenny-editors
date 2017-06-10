[![Travis Build Status](https://travis-ci.org/pepeiborra/threepenny-editors.svg)](https://travis-ci.org/pepeiborra/threepenny-editors)
[![Hackage](https://img.shields.io/hackage/v/threepenny-editors.svg)](https://hackage.haskell.org/package/threepenny-editors)
[![Stackage Nightly](http://stackage.org/package/threepenny-editors/badge/nightly)](http://stackage.org/nightly/package/threepenny-editors)

# threepenny-editors 

## Introduction
A library allowing to easily create threepenny-gui widgets for editing algebraic datatypes. 
The library provides a set of editors for primitive and base types, a set of editor
constructors for building editors for type constructors, and a set of combinators for
composing editors - the `EditorFactory` type has an `Applicative`-like structure, with two
combinators for horizontal and vertical composition, as well as
a `Profunctor` instance. Don't worry if you are not familiar with these concepts as they are
not required to perform simple tasks with this library.
```
newtype EditorFactory a b
instance Profunctor EditorFactory

(|*|) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
(-*-) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
```

The library also provides an `Editable` type class to associate a default `EditorFactoy` with
a type:
```
class Editable a where
  editor :: EditorFactory a a
```

## Example

Let's start with something simple, obtaining an `EditorFactory` for a newtype:
```
newtype Brexiteer = Brexiteer {unBrexiteer::Bool} deriving (Bounded, Enum, Eq, Read, Show, Ord, Generic)
```

Since we already have an `Editable` instance for `Bool` that displays a checkbox, 
we can obtain an `Editable` instance for `Brexiteer` for free:
```
deriving instance Editable Brexiteer
```

We can also wrap the existing `Bool` editor manually if we want to using `dimap`:
```
editorBrexiteer = dimap unBrexiteer Brexiteer (editor :: Editor Bool Bool)
```
The type annotation above is only for illustrative purposes.

Perhaps we are not happy with the default checkbox editor and want to have a different UI?
The code below shows how to use a textbox instead:
```
editorBrexiteerText :: EditorFactory Brexiteer Brexiteer
editorBrexiteerText = editorReadShow
```
Or a combo box:
```
editorBrexiteerChoice :: EditorFactoy Brexiteer Brexiteer
editorBrexiteerChoice = editorEnumBounded
```
Let's move on to a union type now:
```
data Education
  = Basic
  | Intermediate
  | Other_ String
  deriving (Eq, Read, Show)
```
We could define an editor for `Education` with `editorReadShow`, but maybe we want a more user
friendly UI that displays a choice of education type, and only in the `Other` case a free form
text input. The `editorSum` combinator takes a list of choices and an editor for each choice:
```
editorEducation :: EditorFactory Education Education
editorEducation = do
    let selector x = case x of
            Other _ -> "Other"
            _       -> show x
    editorSum
      [ ("Basic", const Basic <$> editorUnit)
      , ("Intermediate", const Intermediate <$> editorUnit)
      , ("Other", dimap (fromMaybe "" . getOther) Other editor)
      ]
      selector

getOther :: Education -> Maybe String
getOther (Other s) = Just s
getOther _         = Nothing
```

Or more simply, we could just use `editorGeneric` to achieve the same effect, provided that
`Education` has got SOP.Generic and SOP.HasDatatypeInfo instances
```
import           GHC.Generics
import qualified Generics.SOP as SOP

deriving instance Generic Education
instance SOP.HasDatatypeInfo Education
instance SOP.Generic Education

-- Derive an Editable instance that uses editorGeneric
instance Editable Education

-- Explicitly call editorGeneric
editorEducation :: EditorFactory Education Education
editorEducation = editorGeneric
```
Moving on to a record type, let's look at how to compose multiple editors together:
```
data Person = Person
  { education           :: Education
  , firstName, lastName :: String
  , age                 :: Maybe Int
  , brexiteer           :: Brexiteer
  , status              :: LegalStatus
  }
  deriving (Generic, Show)
```
The `field` combinator encapsulates the common pattern of pairing a label and a base editor
to build the editor for a record field:
```
field :: String -> (out -> inn) -> EditorFactory inn a -> EditorFactory out a
field name f e = string name *| lmap f e
```
Where `*|` prepends a UI Element to an Editor horizontally: 
```
(*|) :: UI Element -> EditorFactory s a -> EditorFactory s a
```
Armed with `field` and applicative composition (vertical '-*-' and horizontal '|*|'),
we define the editor for `Person` almost mechanically:
```
editorPerson :: EditorFactory Person Person
editorPerson =
    (\fn ln a e ls b -> Person e fn ln a b ls)
      <$> field "First:"     firstName editor
      -*- field "Last:"      lastName editor
      -*- field "Age:"       age editor
      -*- field "Education:" education editorEducation
      -*- field "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      -*- field "Brexiter"   brexiteer editor
```
The only bit of ingenuity in the code above is the deliberate reordering of the fields.

It is also possible to generically derive the editor for person in the same way as before, in which
case the labels are taken from the field names, and the order from the declaration order.
