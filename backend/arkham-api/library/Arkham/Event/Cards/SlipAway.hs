module Arkham.Event.Cards.SlipAway (slipAway, SlipAway (..)) where

import Arkham.Classes
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType

newtype SlipAway = SlipAway EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway :: EventCard SlipAway
slipAway = event SlipAway Cards.slipAway

instance RunMessage SlipAway where
  runMessage msg e@(SlipAway attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      sid <- getRandom
      chooseEvade <- toMessage <$> mkChooseEvade sid iid attrs
      pushAll
        [ skillTestModifier sid attrs iid (AddSkillValue SkillAgility)
        , chooseEvade
        ]
      pure e
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (EnemyTarget enemyId) -> do
          nonElite <- enemyId <=~> NonEliteEnemy
          pushWhen nonElite $ nextPhaseModifier #upkeep attrs enemyId DoesNotReadyDuringUpkeep
        _ -> error "Invalid call, expected enemy skill test target"
      pure e
    _ -> SlipAway <$> runMessage msg attrs
