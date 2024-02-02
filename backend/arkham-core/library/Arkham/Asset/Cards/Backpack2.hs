module Arkham.Asset.Cards.Backpack2 (
  backpack2,
  Backpack2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (Supply)
import Arkham.Card
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Trait

newtype Backpack2 = Backpack2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

backpack2 :: AssetCard Backpack2
backpack2 = asset Backpack2 Cards.backpack2

instance HasModifiersFor Backpack2 where
  getModifiersFor (InvestigatorTarget iid) (Backpack2 attrs) | controlledBy attrs iid = do
    pure $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor _ _ = pure []

instance HasAbilities Backpack2 where
  getAbilities (Backpack2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction (AssetEntersPlay #after $ AssetWithId $ toId a)
    ]

instance RunMessage Backpack2 where
  runMessage msg a@(Backpack2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let cardMatcher = NonWeakness <> CardWithOneOf [CardWithTrait Item, CardWithTrait Supply]
      push $ search iid source iid [fromTopOfDeck 12] cardMatcher (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      player <- getPlayer iid
      push
        $ chooseUpToN player (3 + additionalTargets) "Done choosing cards"
        $ [targetLabel (toCardId c) [PlaceUnderneath (toTarget attrs) [c]] | c <- cards]
      pure a
    SearchNoneFound iid target | isTarget attrs target -> do
      player <- getPlayer iid
      push $ chooseOne player [Label "No Cards Found" []]
      pure a
    InitiatePlayCard iid card _ _ _ | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs -> do
      let remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
      pushAll $ [toDiscardBy iid attrs attrs | null remaining] <> [addToHand iid card, msg]
      pure $ Backpack2 $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack2 <$> runMessage msg attrs
