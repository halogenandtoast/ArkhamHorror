module Arkham.Treachery.Cards.DeathAndDecay (deathAndDecay) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Item))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeathAndDecay = DeathAndDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathAndDecay :: TreacheryCard DeathAndDecay
deathAndDecay = treachery DeathAndDecay Cards.deathAndDecay

instance RunMessage DeathAndDecay where
  runMessage msg t@(DeathAndDecay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> AssetCanBeDamagedBySource (toSource attrs)
      items <- select $ assetControlledBy iid <> AssetWithTrait Item
      scenarioI18n $ blueDecide iid do
        labeled' "placeDoomOnTheBlueAgenda" $ placeDoomOnFactionAgenda attrs BlueFaction 1
        labeled' "youAndYourAssetsTakeDamage" do
          directDamage iid attrs 1
          for_ assets \asset -> dealAssetDamage asset attrs 1
        when (notNull items) do
          labeled' "removeItemFromGame" do
            chooseTargetM iid items \item -> push $ RemoveFromGame (toTarget item)
      pure t
    _ -> DeathAndDecay <$> liftRunMessage msg attrs
