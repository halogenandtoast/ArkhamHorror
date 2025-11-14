module Arkham.Asset.Assets.EceSahinTheVermillionVeiledLady (eceSahinTheVermillionVeiledLady) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getInvestigators)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype EceSahinTheVermillionVeiledLady = EceSahinTheVermillionVeiledLady AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eceSahinTheVermillionVeiledLady :: AssetCard EceSahinTheVermillionVeiledLady
eceSahinTheVermillionVeiledLady = allyWith EceSahinTheVermillionVeiledLady Cards.eceSahinTheVermillionVeiledLady (2, 3) noSlots

instance HasAbilities EceSahinTheVermillionVeiledLady where
  getAbilities (EceSahinTheVermillionVeiledLady a) =
    [ controlled_ a 1 $ FastAbility (exhaust a)
    , controlled a 2 restriction $ forced $ CampaignEvent #after Nothing "shiftKey"
    ]
   where
    restriction = if null a.cardsUnderneath then Never else NoRestriction

instance RunMessage EceSahinTheVermillionVeiledLady where
  runMessage msg a@(EceSahinTheVermillionVeiledLady attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      chooseTargetM iid investigators \iid' ->
        lookAt
          iid
          (attrs.ability 1)
          iid'
          [(FromTopOfDeck 1, PutBack)]
          (basic AnyCard)
          (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      for_ cards \card -> do
        focusCards [card] do
          chooseOneM iid $ withI18n do
            cardNameVar attrs
              $ labeledValidate'
                (cardMatch card (card_ NonWeakness) && (length attrs.cardsUnderneath < 5))
                "placeUnderneath"
              $ placeUnderneath attrs (only card)
            skip_
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ attrs.cardsUnderneath \card -> do
        for_ card.owner \iid -> drawCardFrom iid Deck.NoDeck card
      pure a
    _ -> EceSahinTheVermillionVeiledLady <$> liftRunMessage msg attrs
