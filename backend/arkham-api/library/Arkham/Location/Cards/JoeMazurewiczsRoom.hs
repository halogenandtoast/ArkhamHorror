module Arkham.Location.Cards.JoeMazurewiczsRoom (joeMazurewiczsRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheSecretName.Helpers
import Arkham.Strategy
import Arkham.Trait (Trait (Blessed, Item))

newtype JoeMazurewiczsRoom = JoeMazurewiczsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeMazurewiczsRoom :: LocationCard JoeMazurewiczsRoom
joeMazurewiczsRoom = location JoeMazurewiczsRoom Cards.joeMazurewiczsRoom 3 (PerPlayer 1)

instance HasAbilities JoeMazurewiczsRoom where
  getAbilities (JoeMazurewiczsRoom a) =
    extendRevealed
      a
      [ groupLimit PerGame $ restricted a 1 Here actionAbility
      , scenarioI18n $ hauntedI "joeMazurewiczsRoom.haunted" a 2
      ]

instance RunMessage JoeMazurewiczsRoom where
  runMessage msg l@(JoeMazurewiczsRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      search iid source iid [fromDeck] (basic $ hasAnyTrait [Blessed, Item]) (AddFoundToHand iid 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 2) 1
        labeledValidate' hasAssets "discardAssets" $ chooseAndDiscardAsset iid (attrs.ability 2)
      pure l
    _ -> JoeMazurewiczsRoom <$> liftRunMessage msg attrs
