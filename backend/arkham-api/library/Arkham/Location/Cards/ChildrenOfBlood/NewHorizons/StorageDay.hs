module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.StorageDay (storageDay) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype StorageDay = StorageDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

storageDay :: LocationCard StorageDay
storageDay = symbolLabel $ location StorageDay Cards.storageDay 4 (PerPlayer 1)

instance HasAbilities StorageDay where
  getAbilities (StorageDay a) =
    extendRevealed1 a
      $ playerLimit PerPhase
      $ restricted a 1 Here
      $ freeReaction (SuccessfulInvestigation #when You (be a))

instance RunMessage StorageDay where
  runMessage msg l@(StorageDay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier
          sid
          (attrs.ability 1)
          (toTarget attrs)
          (AlternateSuccessfullInvestigation $ ProxyTarget (toTarget attrs) (toTarget attrs))
      pure l
    Successful (Action.Investigate, _) iid _ (ProxyTarget (isTarget attrs -> True) _) _ -> do
      drawCards iid (attrs.ability 1) 2
      pure l
    _ -> StorageDay <$> liftRunMessage msg attrs
