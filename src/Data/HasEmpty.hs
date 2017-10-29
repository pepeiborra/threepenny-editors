{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HasEmpty where

import           Data.Functor.Identity
import           Data.Map                (Map)
import           Data.Sequence
import           Data.Set                (Set)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Generics.SOP
import           Generics.SOP.Constraint hiding (Compose)

-- | This class defines how to represent empty values in a UI.
--   A generic derivation is available for every SOP type.
class HasEmpty a where
    emptyValue :: a
    default emptyValue :: (Generic a , HasEmptyCode (Code a) , All HasEmpty (Head (Code a))) => a
    emptyValue = gEmptyValue

instance HasEmpty Bool   where emptyValue = False
instance HasEmpty Char   where emptyValue = '?'
instance HasEmpty Int    where emptyValue = 0
instance HasEmpty Double where emptyValue = 0
instance HasEmpty Text   where emptyValue = Text.pack emptyValue
instance HasEmpty [a]    where emptyValue = []
instance HasEmpty (Maybe a) where emptyValue = Nothing
instance HasEmpty ()     where emptyValue = ()
instance (HasEmpty a, HasEmpty b) => HasEmpty (a,b) where emptyValue = (emptyValue, emptyValue)
instance HasEmpty a => HasEmpty (Identity a) where emptyValue = Identity emptyValue
instance Ord k => HasEmpty (Map k v) where emptyValue = mempty
instance Ord k => HasEmpty (Set k) where emptyValue = mempty
instance HasEmpty (Seq k) where emptyValue = mempty

-- Generic HasEmpty values
-- -----------------------

class HasEmptyCode (xs :: [k])
instance HasEmptyCode (x ': xs)

gEmptyValue :: forall a. ( Generic a , HasEmptyCode (Code a) , All HasEmpty (Head (Code a))) => a
gEmptyValue = case sList :: SList (Code a) of
               SCons -> to $ SOP $ Z $ hcpure (Proxy @ HasEmpty) (I emptyValue)
               SNil  -> error "unreachable"
