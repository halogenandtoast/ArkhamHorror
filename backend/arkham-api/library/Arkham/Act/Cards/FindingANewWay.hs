module Arkham.Act.Cards.FindingANewWay (findingANewWay) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FindingANewWay = FindingANewWay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingANewWay :: ActCard FindingANewWay
findingANewWay = act (4, A) FindingANewWay Cards.findingANewWay Nothing

instance HasAbilities FindingANewWay where
  getAbilities (FindingANewWay x)
    | onSide A x =
        [ mkAbility x 1 actionAbility
        , restricted x 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage FindingANewWay where
  runMessage msg a@(FindingANewWay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ DiscardTopOfEncounterDeck
          iid
          3
          (toSource attrs)
          (Just $ toTarget attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let locationCards = filterLocations cards
      focusCards locationCards do
        chooseTargetM iid locationCards \card -> do
          unfocusCards
          drawCard iid card
      pure a
    _ -> FindingANewWay <$> liftRunMessage msg attrs
