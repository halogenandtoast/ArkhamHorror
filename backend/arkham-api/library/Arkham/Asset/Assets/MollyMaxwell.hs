module Arkham.Asset.Assets.MollyMaxwell (mollyMaxwell) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Deck
import Arkham.Helpers.Scenario (getKnownRemainingOriginalDeckCards)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (toTraits)

newtype MollyMaxwell = MollyMaxwell AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mollyMaxwell :: AssetCard MollyMaxwell
mollyMaxwell = ally MollyMaxwell Cards.mollyMaxwell (2, 4)

instance HasAbilities MollyMaxwell where
  getAbilities (MollyMaxwell a) =
    [restricted a 1 ControlsThis $ FastAbility $ exhaust a <> horrorCost a 1]

instance RunMessage MollyMaxwell where
  runMessage msg a@(MollyMaxwell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- filterCards (card_ #asset) . map toCard <$> getKnownRemainingOriginalDeckCards iid
      chooseOneDropDown iid
        $ ( "Trait that won't match"
          , RevealUntilFirst iid (toSource attrs) (toDeck iid) (basic $ NotCard AnyCard)
          )
        : [ ( tshow trait
            , RevealUntilFirst iid (toSource attrs) (toDeck iid) (basic $ CardWithTrait trait <> #asset)
            )
          | trait <- toList (unions $ map toTraits cards)
          ]
      pure a
    RevealedCards iid (isSource attrs -> True) _ mcard rest -> do
      focusCards (rest <> maybeToList mcard) do
        chooseOneM iid do
          case mcard of
            Nothing -> labeled "No cards found" $ shuffleCardsIntoDeck iid rest
            Just c -> targeting c do
              addToHand iid [c]
              shuffleCardsIntoDeck iid rest
      pure a
    _ -> MollyMaxwell <$> liftRunMessage msg attrs
