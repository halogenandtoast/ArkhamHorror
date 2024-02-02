module Arkham.Treachery.Cards.StalkedInTheDark (
  stalkedInTheDark,
  StalkedInTheDark (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

stalkedInTheDark :: TreacheryCard StalkedInTheDark
stalkedInTheDark = treachery StalkedInTheDark Cards.stalkedInTheDark

instance RunMessage StalkedInTheDark where
  runMessage msg t@(StalkedInTheDark attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- getHuntingHorror
      case mHuntingHorrorId of
        Just eid -> do
          iids <- selectList $ colocatedWith iid
          pushAll
            $ [Ready (EnemyTarget eid), EnemyEngageInvestigator eid iid]
            <> map (EnemyAttack . enemyAttack eid attrs) iids
        Nothing -> push $ gainSurge attrs
      pure t
    _ -> StalkedInTheDark <$> runMessage msg attrs
