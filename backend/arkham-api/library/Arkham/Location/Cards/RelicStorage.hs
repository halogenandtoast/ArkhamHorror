module Arkham.Location.Cards.RelicStorage (relicStorage) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype RelicStorage = RelicStorage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicStorage :: LocationCard RelicStorage
relicStorage = location RelicStorage Cards.relicStorage 5 (PerPlayer 1)

instance HasModifiersFor RelicStorage where
  getModifiersFor (RelicStorage a) = whenAny (InvestigatorWithTokenKey #skull) do
    modifySelect
      a
      (location_ "Lodge Catacombs")
      [ConnectedToWhen "Lodge Catacombs" (LocationWithId a.id)]
    modifySelf a [ConnectedToWhen (be a) "Lodge Catacombs"]

instance HasAbilities RelicStorage where
  getAbilities (RelicStorage a) =
    withBaseAbilities a
      $ (guard a.unrevealed *> [skillTestAbility $ mkAbility a 1 $ forced $ Enters #when You (be a)])
      <> (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #after You (be a)])

instance RunMessage RelicStorage where
  runMessage msg l@(RelicStorage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cultists <- select $ NearestEnemyTo iid #cultist
      chooseTargetM iid cultists $ placeDoomOn (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      getRandomKey >>= traverse_ (placeKey attrs)
      pure l
    _ -> RelicStorage <$> liftRunMessage msg attrs
