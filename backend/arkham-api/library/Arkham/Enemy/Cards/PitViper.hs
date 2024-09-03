module Arkham.Enemy.Cards.PitViper (
  pitViper,
  PitViper (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype PitViper = PitViper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitViper :: EnemyCard PitViper
pitViper = enemy PitViper Cards.pitViper (3, Static 1, 3) (1, 0)

instance HasAbilities PitViper where
  getAbilities (PitViper a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          ( InvestigatorExists
              $ You
              <> NotInvestigator
                (HasMatchingTreachery $ treacheryIs Treacheries.poisoned)
          )
          $ ForcedAbility
          $ DealtDamage
            Timing.After
            (SourceIsEnemyAttack $ EnemyWithId $ toId a)
            You
      ]

instance RunMessage PitViper where
  runMessage msg e@(PitViper attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      poisoned <- getSetAsidePoisoned
      push $ CreateWeaknessInThreatArea poisoned iid
      pure e
    _ -> PitViper <$> runMessage msg attrs
