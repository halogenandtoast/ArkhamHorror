module Arkham.Investigator.Cards.AminaZidane (aminaZidane, AminaZidane (..)) where

import Arkham.Ability
import Arkham.Helpers.Doom (getDoomOnTarget)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Ref (targetToSource)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (PlayCard)
import Arkham.Matcher

newtype AminaZidane = AminaZidane InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

aminaZidane :: InvestigatorCard AminaZidane
aminaZidane =
  investigator AminaZidane Cards.aminaZidane
    $ Stats {health = 5, sanity = 9, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor AminaZidane where
  getModifiersFor target (AminaZidane a) | isTarget a target = do
    modified a [CanReduceCostOf #asset 3]
  getModifiersFor _ _ = pure []

instance HasAbilities AminaZidane where
  getAbilities (AminaZidane x) =
    [ playerLimit PerRound $ restrictedAbility x 1 Self $ freeReaction $ PlayCard #when You $ basic #asset
    ]

instance HasChaosTokenValue AminaZidane where
  getChaosTokenValue iid ElderSign (AminaZidane attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AminaZidane where
  runMessage msg i@(AminaZidane attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      reduceCostOf (attrs.ability 1) card 3
      cardResolutionModifier card (attrs.ability 1) card (EntersPlayWithDoom 1)
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      hasDoom <- select $ TargetWithDoom <> TargetAtLocation (locationWithInvestigator iid)
      validTargets <- select $ TargetAtLocation (locationWithInvestigator iid)

      when (length hasDoom > 1 || length validTargets > 1) do
        chooseOne iid $ Label "Do not move doom" []
          : targetLabels hasDoom (only . handleTargetChoice iid attrs)
      pure i
    HandleTargetChoice iid (isSource attrs -> True) target -> do
      targets <- filter (/= target) <$> select (TargetAtLocation $ locationWithInvestigator iid)
      when (notNull targets) do
        n <- getDoomOnTarget target
        chooseOne
          iid
          [ targetLabel target' [MoveTokens #elderSign (targetToSource target) target' #doom n]
          | target' <- targets
          ]
      pure i
    _ -> AminaZidane <$> liftRunMessage msg attrs
