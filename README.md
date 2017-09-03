[![Travis Build Status](https://travis-ci.org/pepeiborra/threepenny-editors.svg)](https://travis-ci.org/pepeiborra/threepenny-editors)
[![Hackage](https://img.shields.io/hackage/v/threepenny-editors.svg)](https://hackage.haskell.org/package/threepenny-editors)
[![Stackage Nightly](http://stackage.org/package/threepenny-editors/badge/nightly)](http://stackage.org/nightly/package/threepenny-editors)

# threepenny-editors 

## What
Threepenny editors is a package for [threepenny-gui](http://hackage.haskell.org/package/threepenny-gui) which simplifies the construction of widgets for editing algebraic datatypes. An editor is a function `Behaviour outer -> (widget, Tidings inner)`. This library introduces an abstraction `Editor` around these functions which allows to compose them and treat them as first class entities, automating much of the boilerplate required for the definitions.

## How 
1. `Editor outer widget inner` is an editor for a field `inner` inside a datatype `outer` implemented by a `widget`. 
2. `widget` must be a `Renderable` type. The simplest `Renderable` type is an HTML `Element`. The next simplest `Renderable` is `Layout`.
3. Once `create`d, an `Editor` yields a `GenericWidget` ready to be composed in a threepenny-gui app.

Moreover, the `Editable` type class allows to define the default `Editor` for a type. Instances are provided for most primitive types.

`Editor`s have `Biapplicative` and `Profunctor` instances. `Biapplicative` allows to compose editors on both their `widget` and their `inner` structure, whereas `Profunctor` allows to apply an `inner` editor to an `outer` datatype by focusing the editor into a substructure.


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

We can also wrap the existing `Bool` editor manually if we want to using `dimapE`:

```
editorBrexiteer = dimapE unBrexiteer Brexiteer (editor :: Editor Bool Element Bool)
```

The type annotation above is only for illustrative purposes.

Perhaps we are not happy with the default checkbox editor and want to have a different UI?
The code below shows how to use a textbox instead:

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

We could define an editor for `Education` with `editorReadShow`, but maybe we want a more user
friendly UI that displays a choice of education type, and only in the `Other` case a free form
text input. The `editorSum` combinator takes a list of choices and an editor for each choice:

```
editorEducation :: Editor Education Education
editorEducation = do
    let selector x = case x of
            Other _ -> "Other"
            _       -> show x
    editorSum
      [ ("Basic", const Basic <$> editorUnit)
      , ("Intermediate", const Intermediate <$> editorUnit)
      , ("Other", dimapE (fromMaybe "" . getOther) Other someEditor)
      ]
      selector

getOther :: Education -> Maybe String
getOther (Other s) = Just s
getOther _         = Nothing
```

Or more simply, we could just use `editorGeneric` to achieve the same effect, provided that
`Education` has a `Generic` instance:

```
import Generics.SOP.TH

derivingGeneric ''Education

-- Derive an Editable instance that uses editorGeneric
instance Editable Education

-- Explicitly call editorGeneric
editorEducation :: Editor Education Layout Education
editorEducation = editorGeneric
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
An example such builder allows to lay out editors in multiple 'Columns':

```
{-# LANGUAGE ApplicativeDo              #-}
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

All the editors defined so far have one thing in common: they have the same shape as the datatype they are editing. 
We can take advantage of this to define datatypes with a dual purpose, which can be used to either hold data, or hold widgets:

```
-- | A dual purpose data type that doubles as a value and as a widget depending on the type argument.
data PersonF (purpose :: Purpose) = Person
  { education           :: Field purpose Education
  , firstName, lastName :: Field purpose String
  , age                 :: Field purpose (Maybe Int)
  , brexiteer           :: Field purpose Brexiteer
  , status              :: Field purpose LegalStatus
  }

type Person = PersonF Data
type PersonEditor = PersonF Edit
```
The advantage of this is that the field widgets are bound to names now, and can be manipulated directly to e.g. set attributes on them.
To define a `PersonEditor`, we make use of the `Editor` `Biapplicative` instance:

```
personEditor =
    bipure Person Person
      <<*>> edit education editorEducation
      <<*>> edit firstName editor
      <<*>> edit lastName  editor
      <<*>> edit age       editor
      <<*>> edit brexiteer editor
      <<*>> edit status    (editorJust $ editorSelection ...)
```
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
