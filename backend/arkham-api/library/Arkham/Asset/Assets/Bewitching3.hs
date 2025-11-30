module Arkham.Asset.Assets.Bewitching3 (bewitching3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers (getAdditionalSearchTargets)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Strategy
import Arkham.Trait qualified as Trait

newtype Bewitching3 = Bewitching3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bewitching3 :: AssetCard Bewitching3
bewitching3 = asset Bewitching3 Cards.bewitching3

instance HasAbilities Bewitching3 where
  getAbilities (Bewitching3 a) =
    [ restricted a 1 ControlsThis $ freeReaction $ DrawingStartingHand #when You
    , controlled a 2 criteria $ ReactionAbility (EnemyEngaged #when You AnyEnemy) (exhaust a)
    ]
   where
    criteria = if null a.cardsUnderneath then Never else NoRestriction

instance RunMessage Bewitching3 where
  runMessage msg a@(Bewitching3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid attrs iid [fromDeck] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let tricks = filterCards (CardWithTrait Trait.Trick) cards
      additionalTargets <- getAdditionalSearchTargets iid
      if null tricks
        then focusCards cards $ chooseOneM iid $ labeled "No cards found" nothing
        else doStep (3 + additionalTargets) msg
      pure a
    DoStep n msg'@(SearchFound iid (isTarget attrs -> True) _ cards) | n > 0 -> do
      let underTitles = map toTitle attrs.cardsUnderneath
      let tricks = cards & filterCards (CardWithTrait Trait.Trick) & filter ((`notElem` underTitles) . toTitle)
      unless (null tricks) do
        chooseUpToNM iid 1 "Choose no more Trick cards" do
          targets tricks \card -> do
            push $ RemoveCardFromSearch iid card.id
            push $ PlaceUnderneath (toTarget attrs) [card]
            doStep (n - 1) msg'
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canSearch <- can.search.deck iid
      chooseOrRunOneM iid do
        labeled "Draw 1 attached card" do
          focusCards attrs.cardsUnderneath $ chooseTargetM iid attrs.cardsUnderneath $ drawCard iid
        when canSearch do
          labeled
            "Search the top 9 cards of your deck for a copy of an attached card, draw it, and shuffle your deck"
            do
              let cardMatcher = mapOneOf (CardWithTitle . toTitle) attrs.cardsUnderneath
              search iid attrs iid [fromTopOfDeck 9] (basic cardMatcher) (DrawFound iid 1)
      pure a
    _ -> Bewitching3 <$> liftRunMessage msg attrs
