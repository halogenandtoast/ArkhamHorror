{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery where

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Id
import Arkham.Prelude
import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries

createTreachery :: IsCard a => a -> InvestigatorId -> TreacheryId -> Treachery
createTreachery a iid tid =
  let this = lookupTreachery (toCardCode a) iid tid (toCardId a)
   in overAttrs (\attrs -> attrs {treacheryTaboo = tabooList, treacheryMutated = mutated}) this
 where
  tabooList = case toCard a of
    PlayerCard pc -> pcTabooList pc
    _ -> Nothing
  mutated = case toCard a of
    PlayerCard pc -> tabooMutated tabooList pc
    _ -> Nothing

instance RunMessage Treachery where
  runMessage msg t@(Treachery a) = do
    if t.placement.outOfGame
      then case msg of
        ReturnLocationToGame _ -> Treachery <$> runMessage msg a
        _ -> pure t
      else case msg of
        Revelation iid (isSource t -> True) -> Treachery <$> runMessage msg (overAttrs ((resolvedL %~ insertSet iid) . (waitingL .~ True)) a)
        _ -> Treachery <$> runMessage msg a

lookupTreachery :: CardCode -> InvestigatorId -> TreacheryId -> CardId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Nothing -> error $ "Unknown treachery: " <> show cardCode
  Just (SomeTreacheryCard a) -> \i t c -> Treachery $ cbCardBuilder a c (i, t)

instance FromJSON Treachery where
  parseJSON = withObject "Treachery" $ \o -> do
    cCode <- o .: "cardCode"
    withTreacheryCardCode cCode
      $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a (Object o)

withTreacheryCardCode
  :: CardCode -> (forall a. IsTreachery a => TreacheryCard a -> r) -> r
withTreacheryCardCode cCode f = case lookup cCode allTreacheries of
  Nothing -> error $ "Unknown treachery: " <> show cCode
  Just (SomeTreacheryCard a) -> f a

allTreacheries :: Map CardCode SomeTreacheryCard
allTreacheries =
  (mapFromList (concatMap someTreacheryCardCodes Registry.treacheries) <>)
    $ mapFromList
    $ concatMap someTreacheryCardCodes allTreacheryCardBuilders
