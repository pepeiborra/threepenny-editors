[![Travis Build Status](https://travis-ci.org/pepeiborra/threepenny-editors.svg)](https://travis-ci.org/pepeiborra/threepenny-editors)
[![Hackage](https://img.shields.io/hackage/v/threepenny-editors.svg)](https://hackage.haskell.org/package/threepenny-editors)
[![Stackage Nightly](http://stackage.org/package/threepenny-editors/badge/nightly)](http://stackage.org/nightly/package/threepenny-editors)

# threepenny-editors 

## Description
Threepenny-editors is a library for constructing of widgets for editing algebraic datatypes in [threepenny-gui](http://hackage.haskell.org/package/threepenny-gui). An editor factory is a function `Behaviour outer -> (widget, Tidings inner)`. The library introduces an abstraction `Editor` around these functions which allows treat them as first class entities, improving compositionality and applicability. Moreover, the generic functions provided help automating much of the boilerplate required for the definitions.

More concretely:

1. `Editor outer widget inner` is an factory to create editors for a type `inner` inside a datatype `outer` implemented by a `widget`. 
2. `widget` must be a `Renderable` type. `Renderable` generalizes the `Widget` class from threepenny-gui with the ability to perform layout from the Haskell side.
3. Once `create`d, an `Editor` yields a tuple of `Widget` and `Tidings`, to be composed in a threepenny-gui app.
4. The `Editable` type class associates a type with its `Editor`. Instances are provided for most primitive types.

`Editor`s have `Biapplicative` and `Profunctor` instances. `Biapplicative` allows to compose editors on both their `widget` and their `inner` structure, whereas `Profunctor` allows to apply an `inner` editor to an `outer` datatype by focusing the editor into a substructure.

The library provides generic functions (using [generics-sop](http://hackage.haskell.org/package/generics-sop)) to generate editors and renderables:

- `editorGeneric` - A generic editor for standard datatypes where all the fields are `Editable` that produces a vertical layout.
- `editorGenericBi` - A generic editor for dual purpose datatypes where all the fields are `Editable`.
- `renderGeneric` - A generic render for single constructor dual purpose datatypes that produces a vertical layout.

A dual purpose datatype is a type constructor where the type argument has kind `Purpose`, with two possible instantiations: `Data` or `Edit`. Instantiating with `Data` yields a container of data, i.e. a normal datatype, whereas instantiating with `Edit` yields a container of widgets. This allows to generate data and editor from the same definition when the shape of the editor matches the shape of the data.

## Example

Let's start with something simple, obtaining an `Editor` for a newtype:

```
newtype Brexiteer = Brexiteer {unBrexiteer::Bool} deriving (Bounded, Enum, Eq, Read, Show, Ord, Generic)
```

Since we already have an `Editable` instance for `Bool` that displays a checkbox, 
we can obtain an `Editable` instance for `Brexiteer` for free:

```
deriving instance Editable Brexiteer
```

We can also wrap the existing `Bool` editor manually using `dimapE`:

```
editorBrexiteer = dimapE unBrexiteer Brexiteer (editor :: Editor Bool Element Bool)
```

The type annotation above is only for illustrative purposes.

Perhaps we are not happy with the default checkbox editor and want to have a different UI?
We can use a text box:

```
editorBrexiteerText :: Editor (Maybe Brexiteer) TextEntry (Maybe Brexiteer)
editorBrexiteerText = editorReadShow
```

Or a combo box:

```
editorBrexiteerChoice :: Editor (Maybe Brexiteer) (ListBox Brexiteer) (Maybe Brexiteer)
editorBrexiteerChoice = editorEnumBounded (pure string)
```

Let's move on to a union type now:

```
data Education
  = Basic
  | Intermediate
  | Other_ String
  deriving (Eq, Read, Show)
```

We could define an editor with `editorReadShow`, but maybe we want a more user
friendly UI that displays a choice, and only when `Other` is selected displays a free form
text input. This can be achieved with the `editorSum` combinator, which takes a list of choices
and an editor for each choice. While `editorSum` is easy enough to use,
`editorGeneric` removes away all the boilerplate for us:

```
import Generics.SOP.TH

derivingGeneric ''Education

-- Derive an Editable instance that uses editorGeneric
instance Editable Education
```

Moving on, let's look at how to compose multiple editors together:

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
field :: String -> (out -> inn) -> Editor inn a -> Editor out a
field name f e = string name *| dimap f id e
```

Where `*|` prepends a UI Element to an Editor horizontally: 
```
(*|) :: UI Element -> Editor s w a -> Editor s w a
```

Armed with `field` and applicative composition (vertical '-*-' and horizontal '|*|'),
we define the editor for `Person` almost mechanically:

```
editorPerson :: Editor Person Person
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

In addition to the simple layout combinators, `threepenny-editors` supports the more flexible monoidal layout builders.
`Columns` is such builder which allows to lay out editors in multiple columns:

```
{-# LANGUAGE ApplicativeDo #-}
editorPersonColumns :: Editor Person Columns Person
editorPersonColumns = do
      firstName <- fieldLayout Next "First:"     firstName editor
      lastName  <- fieldLayout Next "Last:"      lastName editor
      age       <- fieldLayout Next "Age:"       age editor
      education <- fieldLayout Break "Education:" education editorEducation
      status    <- fieldLayout Next "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Next "Brexiter"   brexiteer editor
      return Person{..}
```

In all the editors created so far, the widget type was `Layout` (`Columns` is just a newtype of `Layout`). Because `Layout` is opaque,
there is no way to access the field widgets. This can be a problem if we want to set attributes on the fields, etc. To do that,
we would need to define a data type to capture all the field editors, and use in place of `Layout`. This is what the `Biapplicative`
interface allows us to do.

But moreover, all the editors defined so far have one thing in common: they have the same shape as the datatype they are editing. 
We can take advantage of this to define a datatype with a dual purpose, which can be used to either hold data, or hold widgets:

```
-- | A dual purpose data type that doubles as a value and as a widget depending on the type argument.
data PersonDual (purpose :: Purpose) = Person
  { education           :: Field purpose Education
  , firstName, lastName :: Field purpose String
  , age                 :: Field purpose (Maybe Int)
  , brexiteer           :: Field purpose Brexiteer
  , status              :: Field purpose LegalStatus
  }

type Person = PersonDual Data
type PersonEditor = PersonDual Edit
```
This binds the field widgets, allowing to e.g. set attributes on them.
To define a `PersonEditor`, we make use of the `Editor` `Biapplicative` instance:

```
personEditor =
    bipure Person Person
      <<*>> dimap education id editorEducation
      <<*>> dimap firstName id editor
      <<*>> dimap lastName  id editor
      <<*>> dimap age       id editor
      <<*>> dimap brexiteer id editor
      <<*>> dimap status    id (editorJust $ editorSelection ...)
```

When all the fields are `Editable` this definition can be derived via `editorGenericBi`.

Now the actual layout is define separately from the editor composition, 
using standard threepenny-gui primitives:

```
instance Renderable PersonEditor where
  render Person{..} = grid
   [ [string "First:", element firstName, string "Age:", element age]
   , [string "Last:", element lastName, string "Brexiteer:", element brexiteer]
   , [string "Status:", element status, string "Education:", element education]
   ]
```

Once again `render` can be derived via `renderGeneric`, which produces a single column vertical layout.

The complete code for the dual purpose `Person` example can be found below:
```
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad
import           Generics.SOP.TH
import           Graphics.UI.Threepenny
import           Graphics.UI.Threepenny.Editors

data Education
  = Basic
  | Intermediate
  | Other String

instance Editable Education

newtype Brexiteer = Brexiteer Bool

instance Editable Brexiteer where editor = editorGeneric

data LegalStatus
  = Single
  | Married
  | Divorced
  | Widowed

instance Editable LegalStatus

data PersonDual (purpose :: Purpose) = Person
  { education           :: Field purpose Education
  , firstName, lastName :: Field purpose String
  , age                 :: Field purpose (Maybe Int)
  , brexiteer           :: Field purpose Brexiteer
  , status              :: Field purpose LegalStatus
  }

type Person = PersonDual 'Data
type PersonEditor = PersonDual 'Edit

instance Editable Person where
  type EditorWidget Person = PersonEditor
  editor = editorGenericBi

instance Renderable PersonEditor where
  render = renderGeneric

deriveGeneric ''Education
deriveGeneric ''Brexiteer
deriveGeneric ''LegalStatus
deriveGeneric ''PersonDual

main = startGUI defaultConfig $ \w -> mdo
  personE :: GenericWidget PersonEditor Person <- create editor personB
  personB :: Behavior Person <- stepper (Person Basic "" "" Nothing (Brexiteer False) Single) (edited personE)

  void $ getBody w #+ [render personE]
  void $ element (firstName (widgetControl personE)) # set style [("background-color", "Blue")]
```
