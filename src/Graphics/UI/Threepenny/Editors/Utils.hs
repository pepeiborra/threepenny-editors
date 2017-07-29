module Graphics.UI.Threepenny.Editors.Utils (calmB, calmE) where

import           Data.Maybe
import           Graphics.UI.Threepenny.Core as UI

-- | Returns a new behavior that only notifies for new values.
calmB :: Eq a => Behavior a -> UI (Behavior a)
calmB b = do
  w <- askWindow
  (e, trigger) <- liftIO newEvent
  liftIOLater $ do
    current <- currentValue b
    trigger current
    runUI w $ onChanges b (liftIO . trigger)
  eCalm <- calmE e
  fmap (fromMaybe (error "calmB")) <$> stepper Nothing (Just <$> eCalm)

data Memory a = Empty | New a | Same a
updateMemory :: Eq a => a -> Memory a -> Memory a
updateMemory x Empty  = New x
updateMemory x (New  a) | a /= x = New x
updateMemory x (Same a) | a /= x = New x
updateMemory x _ = Same x
isNew :: Memory a -> Maybe a
isNew (New x) = Just x
isNew _ = Nothing

-- | Returns a new 'Event' that skips consecutive triggers with the same value.
calmE :: Eq a => Event a -> UI (Event a)
calmE e =
  filterJust . fmap isNew <$> accumE Empty (updateMemory <$> e)
