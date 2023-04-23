module Arkham.Treachery.Cards.SlitheringBehindYou
  ( SlitheringBehindYou(..)
  , slitheringBehindYou
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Zone

newtype SlitheringBehindYou = SlitheringBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slitheringBehindYou :: TreacheryCard SlitheringBehindYou
slitheringBehindYou = treachery SlitheringBehindYou Cards.slitheringBehindYou

instance RunMessage SlitheringBehindYou where
  runMessage msg t@(SlitheringBehindYou attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- selectOne $ enemyIs Enemies.huntingHorror
      case mHuntingHorrorId of
        Just eid -> pushAll
          [PlaceDoom (EnemyTarget eid) 1, ShuffleDeck Deck.EncounterDeck]
        Nothing -> do
          huntingHorror <- findUniqueCard Enemies.huntingHorror
          spawnHuntingHorror <- createEnemy huntingHorror iid
          pushAll [spawnHuntingHorror, ShuffleDeck EncounterDeck]
      pure t
    _ -> SlitheringBehindYou <$> runMessage msg attrs
