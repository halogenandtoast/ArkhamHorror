module Arkham.Treachery.Cards.WildRide (wildRide) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WrittenInRock.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Trait (Trait (Ally, Resident))

newtype WildRide = WildRide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildRide :: TreacheryCard WildRide
wildRide = treachery WildRide Cards.wildRide

instance HasAbilities WildRide where
  getAbilities (WildRide a) =
    [ restricted a 1 InYourThreatArea $ forced $ ScenarioEvent #when Nothing "mineCartMoved"
    ]

instance RunMessage WildRide where
  runMessage msg t@(WildRide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getId
      revelationSkillTest sid iid attrs #agility (Fixed 5)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n do
        labeled' "wildRide.moveAgain" do
          do_ (ScenarioSpecific "moveMineCart" Null)
          do_ (ScenarioSpecific "moveMineCart" Null)
        labeled' "wildRide.damage" do
          eachInvestigator \iid' -> do
            directDamageAndHorror iid' (attrs.ability 1) 1 1
            selectEach (assetControlledBy iid <> hasAnyTrait [Resident, Ally]) \a ->
              dealAssetDirectDamageAndHorror a (attrs.ability 1) 1 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WildRide <$> liftRunMessage msg attrs
