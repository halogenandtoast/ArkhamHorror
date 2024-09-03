module Arkham.Asset.Cards.Bewitching3 (bewitching3, Bewitching3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers (getAdditionalSearchTargets)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Strategy
import Arkham.Trait qualified as Trait
import Data.Function (on)
import Data.List (nubBy)

newtype Bewitching3 = Bewitching3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bewitching3 :: AssetCard Bewitching3
bewitching3 = asset Bewitching3 Cards.bewitching3

instance HasAbilities Bewitching3 where
  getAbilities (Bewitching3 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ freeReaction
        $ DrawingStartingHand #when You
    , controlledAbility attrs 2 criteria
        $ ReactionAbility (EnemyEngaged #when You AnyEnemy) (exhaust attrs)
    ]
   where
    criteria = if length attrs.cardsUnderneath == 0 then Never else NoRestriction

instance RunMessage Bewitching3 where
  runMessage msg a@(Bewitching3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid attrs iid [fromDeck] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let tricks = nubBy ((==) `on` toCardCode) $ filter (`cardMatch` CardWithTrait Trait.Trick) cards
      additionalTargets <- getAdditionalSearchTargets iid
      if null tricks
        then do
          focusCards cards \unfocus -> do
            chooseOne iid [Label "No cards found" [unfocus]]
        else do
          chooseUpToN
            iid
            (3 + additionalTargets)
            "Choose no more Trick cards"
            [ targetLabel
              card
              [RemoveCardFromSearch iid (toCardId card), PlaceUnderneath (toTarget attrs) [card]]
            | card <- tricks
            ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canSearch <- can.search.deck iid
      chooseOrRunOne
        iid
        $ [Label "Draw 1 attached card" [DoStep 1 msg]]
        <> [ Label
            "Search the top 9 cards of your deck for a copy of an attached card, draw it, and shuffle your deck"
            [DoStep 2 msg]
           | canSearch
           ]
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      focusCards attrs.cardsUnderneath \unfocus -> do
        chooseOne iid [targetLabel card [unfocus, AddToHand iid [card]] | card <- attrs.cardsUnderneath]
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      let cardMatcher = mapOneOf CardWithCardCode $ map toCardCode attrs.cardsUnderneath
      search iid attrs iid [fromTopOfDeck 9] (basic cardMatcher) (DrawFound iid 1)
      pure a
    _ -> Bewitching3 <$> liftRunMessage msg attrs
