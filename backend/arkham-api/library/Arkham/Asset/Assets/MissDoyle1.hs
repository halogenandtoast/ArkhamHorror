module Arkham.Asset.Assets.MissDoyle1 (missDoyle1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Investigator hiding (findCard)
import Arkham.Matcher

newtype MissDoyle1 = MissDoyle1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

missDoyle1 :: AssetCard MissDoyle1
missDoyle1 = ally MissDoyle1 Cards.missDoyle1 (2, 2)

instance HasAbilities MissDoyle1 where
  getAbilities (MissDoyle1 a) =
    [ restricted a 1 ControlsThis $ forced $ AssetEntersPlay #when (be a)
    , mkAbility a 2 $ SilentForcedAbility $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage MissDoyle1 where
  runMessage msg a@(MissDoyle1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hope <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.hope
      augur <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.augur
      zeal <- fromJustNote "must be" . listToMaybe <$> searchBonded iid Cards.zeal
      (playCat, deckCats) <- splitAt 1 <$> shuffle [hope, augur, zeal]
      shuffleCardsIntoDeck iid deckCats
      for_ playCat $ putCardIntoPlay iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hope <- findAllCards (`cardMatch` (cardIs Cards.hope <> CardOwnedBy iid))
      zeal <- findAllCards (`cardMatch` (cardIs Cards.zeal <> CardOwnedBy iid))
      augur <- findAllCards (`cardMatch` (cardIs Cards.augur <> CardOwnedBy iid))
      for_ (concat [hope, zeal, augur]) (placeInBonded iid)
      pure a
    _ -> MissDoyle1 <$> liftRunMessage msg attrs
