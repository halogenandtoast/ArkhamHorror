module Arkham.Event.Events.OnTheBeat1 (onTheBeat1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype OnTheBeat1 = OnTheBeat1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheBeat1 :: EventCard OnTheBeat1
onTheBeat1 = event OnTheBeat1 Cards.onTheBeat1

instance RunMessage OnTheBeat1 where
  runMessage msg e@(OnTheBeat1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- Card says +3 skill value while investigating or parleying until end of turn.
      -- ActionSkillModifier applies per action+skill pair. We cover the primary skill
      -- for each action. A full implementation would require an effect with HasModifiersFor
      -- that checks the current skill test action.
      turnModifiers iid attrs iid
        [ ActionSkillModifier #investigate #intellect 3
        , ActionSkillModifier #parley #willpower 3
        , ActionSkillModifier #parley #combat 3
        , ActionSkillModifier #parley #agility 3
        , ActionSkillModifier #parley #intellect 3
        ]
      pure e
    _ -> OnTheBeat1 <$> liftRunMessage msg attrs
