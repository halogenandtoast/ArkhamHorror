module Arkham.Event.Events.CausticReaction (causticReaction) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype CausticReaction = CausticReaction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causticReaction :: EventCard CausticReaction
causticReaction = event CausticReaction Cards.causticReaction

instance RunMessage CausticReaction where
  runMessage msg e@(CausticReaction attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers
        sid
        attrs
        iid
        [ SkillModifier #intellect 1
        , DamageDealtCalculation
            $ IfInvestigatorExistsCalculation iid (InvestigatorWithClues $ atLeast 2) (Fixed 1) (Fixed 0)
        ]
      chooseFightEnemyWith #intellect sid iid attrs
      pure e
    _ -> CausticReaction <$> liftRunMessage msg attrs
