module Arkham.Asset.Assets.ObsidianJaguar (obsidianJaguar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.RelicsOfThePast.Helpers

newtype ObsidianJaguar = ObsidianJaguar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianJaguar :: AssetCard ObsidianJaguar
obsidianJaguar = asset ObsidianJaguar Cards.obsidianJaguar

instance HasAbilities ObsidianJaguar where
  getAbilities (ObsidianJaguar a) =
    [restricted a 1 ControlsThis $ forced $ InvestigatorResigned #when You]

instance RunMessage ObsidianJaguar where
  runMessage msg a@(ObsidianJaguar attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      enemies <- select $ NearestEnemyTo iid AnyEnemy
      chooseOneM iid $ scenarioI18n do
        labeled' "placeDoomOnYourLocation" $ withLocationOf iid \lid -> placeDoom attrs lid 1
        when (notNull enemies) do
          labeled' "nearestEnemyAttacksYou" do
            chooseOrRunOneM iid $ targets enemies \enemy -> initiateEnemyAttack enemy attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure a
    _ -> ObsidianJaguar <$> liftRunMessage msg attrs
