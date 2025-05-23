module Arkham.Act.Cards.DisruptingTheRitual (disruptingTheRitual) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: ActCard DisruptingTheRitual
disruptingTheRitual = act (3, A) DisruptingTheRitual Cards.disruptingTheRitual Nothing

instance HasAbilities DisruptingTheRitual where
  getAbilities (DisruptingTheRitual a)
    | onSide A a =
        [ skillTestAbility $ mkAbility a 1 $ actionAbilityWithCost (ClueCost $ Static 1)
        , restricted a 2 (CluesOnThis $ AtLeast $ PerPlayer 2) $ Objective $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #agility] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) attrs kind (Fixed 3)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure a
    _ -> DisruptingTheRitual <$> liftRunMessage msg attrs
