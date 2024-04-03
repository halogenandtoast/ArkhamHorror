module Arkham.Event.Cards.Banish1 (banish1, Banish1 (..)) where

import Arkham.Aspect
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype Banish1 = Banish1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banish1 :: EventCard Banish1
banish1 = event Banish1 Cards.banish1

instance RunMessage Banish1 where
  runMessage msg e@(Banish1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      chooseEvade <-
        leftOr
          <$> aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvadeMatch iid attrs NonEliteEnemy)
      pushAll chooseEvade
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          tokens <- map (.face) <$> getSkillTestRevealedChaosTokens
          locations <- select $ LocationCanBeEnteredBy eid
          player <- getPlayer iid

          pushAll
            $ [chooseOrRunOne player [targetLabel lid [EnemyMove eid lid] | lid <- locations] | notNull locations]
            <> [ nextPhaseModifier #upkeep attrs eid DoesNotReadyDuringUpkeep
               | any (`elem` tokens) [Skull, Cultist, Tablet, ElderThing]
               ]
        _ -> error "Wrong target"
      pure e
    _ -> Banish1 <$> runMessage msg attrs
