module Arkham.Enemy.Cards.SerpentOfTenochtitlan
  ( serpentOfTenochtitlan
  , SerpentOfTenochtitlan(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Ancient) )
import Arkham.Treachery.Cards qualified as Treacheries

newtype SerpentOfTenochtitlan = SerpentOfTenochtitlan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentOfTenochtitlan :: EnemyCard SerpentOfTenochtitlan
serpentOfTenochtitlan = enemy
  SerpentOfTenochtitlan
  Cards.serpentOfTenochtitlan
  (3, Static 5, 3)
  (1, 1)

instance HasModifiersFor SerpentOfTenochtitlan where
  getModifiersFor target (SerpentOfTenochtitlan a) | isTarget a target = do
    atAncientLocation <- selectAny $ EnemyWithId (toId a) <> EnemyAt
      (LocationWithTrait Ancient)
    pure $ toModifiers a $ if atAncientLocation
      then [AddKeyword Retaliate, AddKeyword Alert]
      else [AddKeyword Hunter]
  getModifiersFor _ _ = pure []

instance HasAbilities SerpentOfTenochtitlan where
  getAbilities (SerpentOfTenochtitlan a) = withBaseAbilities
    a
    [ restrictedAbility
        a
        1
        (InvestigatorExists $ You <> NotInvestigator
          (HasMatchingTreachery $ treacheryIs Treacheries.poisoned)
        )
      $ ForcedAbility
      $ DealtDamage
          Timing.After
          (SourceIsEnemyAttack $ EnemyWithId $ toId a)
          You
    ]

instance RunMessage SerpentOfTenochtitlan where
  runMessage msg e@(SerpentOfTenochtitlan attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      poisoned <- getSetAsidePoisoned
      push $ CreateWeaknessInThreatArea poisoned iid
      pure e
    _ -> SerpentOfTenochtitlan <$> runMessage msg attrs
