module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.CrateOfGoods (
  crateOfGoodsCrimsonLedger,
  crateOfGoodsHandOfTheStrangler,
  crateOfGoodsAbyssalSword,
  crateOfGoodsRingOfTheDeep,
  crateOfGoodsTabletOfDagon,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

{- | The five crates share a front, so they share an entity. Only the card def --
and therefore the back they flip to -- differs.
-}
newtype CrateOfGoods = CrateOfGoods AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "[action] Test [intellect] (3) or [agility] (3) to sift through the contents
of the crate. If you succeed, an investigator at your location may place 1 of his
or her clues on Crate of Goods. Then, if there are clues on Crate of Goods equal
to the number of investigators, flip it over and draw it."

Agenda 2a also places doom here for each ready Cultist at its location, and
clears it when the crate is flipped.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
crateOfGoodsCrimsonLedger :: AssetCard CrateOfGoods
crateOfGoodsCrimsonLedger = asset CrateOfGoods Cards.crateOfGoodsCrimsonLedger

crateOfGoodsHandOfTheStrangler :: AssetCard CrateOfGoods
crateOfGoodsHandOfTheStrangler = asset CrateOfGoods Cards.crateOfGoodsHandOfTheStrangler

crateOfGoodsAbyssalSword :: AssetCard CrateOfGoods
crateOfGoodsAbyssalSword = asset CrateOfGoods Cards.crateOfGoodsAbyssalSword

crateOfGoodsRingOfTheDeep :: AssetCard CrateOfGoods
crateOfGoodsRingOfTheDeep = asset CrateOfGoods Cards.crateOfGoodsRingOfTheDeep

crateOfGoodsTabletOfDagon :: AssetCard CrateOfGoods
crateOfGoodsTabletOfDagon = asset CrateOfGoods Cards.crateOfGoodsTabletOfDagon

instance RunMessage CrateOfGoods where
  runMessage msg (CrateOfGoods attrs) = runQueueT $ CrateOfGoods <$> liftRunMessage msg attrs
