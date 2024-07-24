module Arkham.Asset.Cards.KatjaEastbankKeeperOfEsotericLore2 (
  katjaEastbankKeeperOfEsotericLore2,
  KatjaEastbankKeeperOfEsotericLore2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card (onlyPlayerCards)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher hiding (PlaceUnderneath)

newtype KatjaEastbankKeeperOfEsotericLore2 = KatjaEastbankKeeperOfEsotericLore2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

katjaEastbankKeeperOfEsotericLore2 :: AssetCard KatjaEastbankKeeperOfEsotericLore2
katjaEastbankKeeperOfEsotericLore2 = ally KatjaEastbankKeeperOfEsotericLore2 Cards.katjaEastbankKeeperOfEsotericLore2 (2, 2)

instance HasAbilities KatjaEastbankKeeperOfEsotericLore2 where
  getAbilities (KatjaEastbankKeeperOfEsotericLore2 x) =
    [ controlledAbility x 1 criteria1
        $ ReactionAbility
          (DrawCard #when You (basic NonWeakness) (DeckOf You))
          (exhaust x)
    , controlledAbility x 2 criteria2 actionAbility
    ]
   where
    criteria1 = if length x.cardsUnderneath < 5 then NoRestriction else Never
    criteria2 = if notNull x.cardsUnderneath then NoRestriction else Never

instance RunMessage KatjaEastbankKeeperOfEsotericLore2 where
  runMessage msg a@(KatjaEastbankKeeperOfEsotericLore2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw attrs card
      push $ PlaceUnderneath (toTarget attrs) [card]
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      focusCards attrs.cardsUnderneath \unfocus -> do
        chooseOne
          iid
          [ targetLabel card [unfocus, InvestigatorDrewPlayerCard iid card]
          | card <- onlyPlayerCards attrs.cardsUnderneath
          ]
      pure a
    _ -> KatjaEastbankKeeperOfEsotericLore2 <$> liftRunMessage msg attrs
