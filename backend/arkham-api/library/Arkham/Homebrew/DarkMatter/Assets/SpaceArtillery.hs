module Arkham.Homebrew.DarkMatter.Assets.SpaceArtillery (spaceArtillery) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SpaceArtillery = SpaceArtillery AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spaceArtillery :: AssetCard SpaceArtillery
spaceArtillery = asset SpaceArtillery Cards.spaceArtillery

{- | "Uses (2 supplies).
[action][action] Choose a location and spend 1 supply: Deal 3 damage to each
enemy and investigator at that location and at each connecting location."
-}
instance HasAbilities SpaceArtillery where
  getAbilities (SpaceArtillery a) =
    [controlled_ a 1 $ doubleActionAbilityWithCost (assetUseCost a Supply 1)]

instance RunMessage SpaceArtillery where
  runMessage msg a@(SpaceArtillery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select Anywhere
      chooseOneM iid $ targets locations \lid -> do
        -- "that location and each connecting location"
        blast <- select $ oneOf [LocationWithId lid, connectedFrom (LocationWithId lid)]
        for_ blast \target -> do
          investigators <- select $ investigatorAt target
          for_ investigators \iid' -> assignDamage iid' (attrs.ability 1) 3
          enemies <- select $ enemyAt target
          for_ enemies \enemy -> nonAttackEnemyDamage Nothing (attrs.ability 1) 3 enemy
      pure a
    _ -> SpaceArtillery <$> liftRunMessage msg attrs
