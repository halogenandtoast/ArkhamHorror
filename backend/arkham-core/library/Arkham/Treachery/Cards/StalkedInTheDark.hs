module Arkham.Treachery.Cards.StalkedInTheDark
  ( stalkedInTheDark
  , StalkedInTheDark(..)
  ) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype StalkedInTheDark = StalkedInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
            <> [ EnemyAttack iid' eid DamageAny RegularAttack | iid' <- iids ]
        Nothing -> push (Surge iid source)
      pure t
    _ -> StalkedInTheDark <$> runMessage msg attrs
