module Arkham.Asset.Assets.KatjaEastbankKeeperOfEsotericLore2 (katjaEastbankKeeperOfEsotericLore2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card (onlyPlayerCards)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose

newtype KatjaEastbankKeeperOfEsotericLore2 = KatjaEastbankKeeperOfEsotericLore2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

katjaEastbankKeeperOfEsotericLore2 :: AssetCard KatjaEastbankKeeperOfEsotericLore2
katjaEastbankKeeperOfEsotericLore2 = ally KatjaEastbankKeeperOfEsotericLore2 Cards.katjaEastbankKeeperOfEsotericLore2 (2, 2)

instance HasAbilities KatjaEastbankKeeperOfEsotericLore2 where
  getAbilities (KatjaEastbankKeeperOfEsotericLore2 x) =
    [ controlled x 1 criteria1
        $ ReactionAbility (DrawCard #when You (basic NonWeakness) (DeckOf You)) (exhaust x)
    , controlled x 2 criteria2 actionAbility
    ]
   where
    criteria1 = if length x.cardsUnderneath < 5 then NoRestriction else Never
    criteria2 = if notNull x.cardsUnderneath then NoRestriction else Never

instance RunMessage KatjaEastbankKeeperOfEsotericLore2 where
  runMessage msg a@(KatjaEastbankKeeperOfEsotericLore2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw card
      push $ PlaceUnderneath (toTarget attrs) [card]
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      focusCards attrs.cardsUnderneath do
        chooseTargetM iid (onlyPlayerCards attrs.cardsUnderneath) $ drawCard iid
      pure a
    _ -> KatjaEastbankKeeperOfEsotericLore2 <$> liftRunMessage msg attrs
