module Arkham.Asset.Cards.ResearchLibrarian where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetCard ResearchLibrarian
researchLibrarian = ally ResearchLibrarian Cards.researchLibrarian (1, 1)

instance HasAbilities ResearchLibrarian where
  getAbilities (ResearchLibrarian x) =
    [ controlledAbility x 1 (CanSearchDeck <> CanShuffleDeck)
        $ freeReaction
        $ AssetEntersPlay #when (be x)
    ]

instance RunMessage ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDeck] #tome (DrawFound iid 1)
      pure a
    _ -> ResearchLibrarian <$> liftRunMessage msg attrs
