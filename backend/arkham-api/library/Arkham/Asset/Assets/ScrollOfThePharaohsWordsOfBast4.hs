module Arkham.Asset.Assets.ScrollOfThePharaohsWordsOfBast4 (scrollOfThePharaohsWordsOfBast4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait

newtype ScrollOfThePharaohsWordsOfBast4 = ScrollOfThePharaohsWordsOfBast4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfThePharaohsWordsOfBast4 :: AssetCard ScrollOfThePharaohsWordsOfBast4
scrollOfThePharaohsWordsOfBast4 = asset ScrollOfThePharaohsWordsOfBast4 Cards.scrollOfThePharaohsWordsOfBast4

instance HasAbilities ScrollOfThePharaohsWordsOfBast4 where
  getAbilities (ScrollOfThePharaohsWordsOfBast4 a) =
    [ controlled a 1 (can.target.encounterDeck You)
        $ actionAbilityWithCost
        $ HorrorCost (toSource a) YouTarget 1
        <> UseCostUpTo (be a) Secret 1 2
    ]

instance RunMessage ScrollOfThePharaohsWordsOfBast4 where
  runMessage msg a@(ScrollOfThePharaohsWordsOfBast4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      lookAt
        iid
        attrs
        EncounterDeckTarget
        [(FromTopOfDeck (n * 3), PutBackInAnyOrder)]
        #any
        (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      let discardable = onlyEncounterCards $ filterCards (NonPeril <> NotCard (CardWithTrait Elite)) cards
      focusCards cards $ chooseOrRunOneM iid $ targets discardable $ addToEncounterDiscard . only
      pure a
    _ -> ScrollOfThePharaohsWordsOfBast4 <$> liftRunMessage msg attrs
