{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries
import Arkham.Card
import Arkham.Classes
import Arkham.Id

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

instance RunMessage Treachery where
  runMessage msg (Treachery a) = Treachery <$> runMessage msg a

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Nothing -> error $ "Unknown treachery: " <> show cardCode
  Just (SomeTreacheryCard a) -> \i t -> Treachery $ cbCardBuilder a (i, t)

instance FromJSON Treachery where
  parseJSON v = flip (withObject "Treachery") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withTreacheryCardCode cCode $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a v

withTreacheryCardCode
  :: CardCode
  -> (forall a. IsTreachery a => TreacheryCard a -> r)
  -> r
withTreacheryCardCode cCode f =
  case lookup cCode allTreacheries of
    Nothing -> error $ "Unknown treachery: " <> show cCode
    Just (SomeTreacheryCard a) -> f a

allTreacheries :: HashMap CardCode SomeTreacheryCard
allTreacheries = mapFromList $ map
  (toFst someTreacheryCardCode)
  [ -- Night of the Zealot
  -- signature
    SomeTreacheryCard coverUp
  -- Ghouls
  , SomeTreacheryCard graspingHands
  -- Striking Fear
  , SomeTreacheryCard rottingRemains
  , SomeTreacheryCard frozenInFear
  , SomeTreacheryCard dissonantVoices
  -- Ancient Evils
  , SomeTreacheryCard ancientEvils
  -- Chilling Colds
  , SomeTreacheryCard cryptChill
  , SomeTreacheryCard obscuringFog
  -- Locked Doors
  , SomeTreacheryCard lockedDoor
  ]
