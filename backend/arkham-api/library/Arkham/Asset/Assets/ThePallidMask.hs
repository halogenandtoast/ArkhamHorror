module Arkham.Asset.Assets.ThePallidMask (thePallidMask) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (PlayCard)

newtype ThePallidMask = ThePallidMask AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePallidMask :: AssetCard ThePallidMask
thePallidMask = assetWith ThePallidMask Cards.thePallidMask (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor ThePallidMask where
  getModifiersFor (ThePallidMask a) = for_ a.controller \iid -> modified_ a iid [SanityModifier (-2)]

instance RunMessage ThePallidMask where
  runMessage msg a@(ThePallidMask attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      selectOne (enemyIs Enemies.theManInThePallidMask) >>= traverse_ (push . RemoveEnemy)
      createSetAsideEnemy_ Enemies.hasturTheTatteredKing =<< selectJust (location_ "Palace of the King")
      pure a
    _ -> ThePallidMask <$> liftRunMessage msg attrs
