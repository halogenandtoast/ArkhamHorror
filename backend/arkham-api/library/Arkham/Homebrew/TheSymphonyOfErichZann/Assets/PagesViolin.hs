module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.PagesViolin (pagesViolin) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype PagesViolin = PagesViolin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pagesViolin :: AssetCard PagesViolin
pagesViolin = asset PagesViolin Cards.pagesViolin

instance HasAbilities PagesViolin where
  -- Discarding a card from hand lets it be exhausted to draw one back.
  getAbilities (PagesViolin a) =
    [ controlled a 1 NoRestriction
        $ triggered (DiscardedFromHand #after You AnySource #any) (exhaust a)
    ]

instance RunMessage PagesViolin where
  runMessage msg a@(PagesViolin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> PagesViolin <$> liftRunMessage msg attrs
