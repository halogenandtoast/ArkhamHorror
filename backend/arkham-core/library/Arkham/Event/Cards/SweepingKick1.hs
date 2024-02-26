module Arkham.Event.Cards.SweepingKick1 (
  sweepingKick1,
  SweepingKick1 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.SkillType

newtype SweepingKick1 = SweepingKick1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sweepingKick1 :: EventCard SweepingKick1
sweepingKick1 = event SweepingKick1 Cards.sweepingKick1

instance RunMessage SweepingKick1 where
  runMessage msg e@(SweepingKick1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifiers attrs iid [AddSkillValue #agility, DamageDealt 1]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) ->
          do
            mSkillTestTarget <- getSkillTestTarget
            case mSkillTestTarget of
              Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
              _ -> pure ()
            pure e
    _ -> SweepingKick1 <$> runMessage msg attrs
