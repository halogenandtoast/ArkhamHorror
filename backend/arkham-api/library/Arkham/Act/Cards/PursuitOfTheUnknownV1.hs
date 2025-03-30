module Arkham.Act.Cards.PursuitOfTheUnknownV1 (pursuitOfTheUnknownV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype PursuitOfTheUnknownV1 = PursuitOfTheUnknownV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuitOfTheUnknownV1 :: ActCard PursuitOfTheUnknownV1
pursuitOfTheUnknownV1 = act (2, A) PursuitOfTheUnknownV1 Cards.pursuitOfTheUnknownV1 Nothing

instance HasAbilities PursuitOfTheUnknownV1 where
  getAbilities (PursuitOfTheUnknownV1 x) =
    [ restricted x 1 (exists $ investigator_ $ oneOf [can.heal.any (x.ability 1), can.heal.trauma])
        $ actionAbilityWithCost (SpendTokenKeyCost 2 #cultist)
    , restricted
        x
        2
        (EachUndefeatedInvestigator $ at_ $ "Hidden Tunnel" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage PursuitOfTheUnknownV1 where
  runMessage msg a@(PursuitOfTheUnknownV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        chooseOneM iid do
          labeled "Do not take healing" nothing
          labeled "Heal 1 damage, 1 horror, and 1 trauma" do
            whenM (canHaveDamageHealed (attrs.ability 1) iid) do
              healDamage iid (attrs.ability 1) 1

            whenM (canHaveDamageHealed (attrs.ability 1) iid) do
              healHorror iid (attrs.ability 1) 1

            hasPhysicalTrauma <- fieldP InvestigatorPhysicalTrauma (> 0) iid
            hasMentalTrauma <- fieldP InvestigatorMentalTrauma (> 0) iid

            if
              | hasPhysicalTrauma && hasMentalTrauma -> do
                  chooseOneM iid do
                    labeled "Heal 1 physical trauma" $ push $ HealTrauma iid 1 0
                    labeled "Heal 1 mental trauma" $ push $ HealTrauma iid 0 1
              | hasPhysicalTrauma -> push $ HealTrauma iid 1 0
              | hasMentalTrauma -> push $ HealTrauma iid 0 1
              | otherwise -> pure ()

      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> PursuitOfTheUnknownV1 <$> liftRunMessage msg attrs
