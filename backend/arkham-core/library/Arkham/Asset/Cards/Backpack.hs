module Arkham.Asset.Cards.Backpack
  ( backpack
  , Backpack(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( Supply )
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype Backpack = Backpack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack :: AssetCard Backpack
backpack = asset Backpack Cards.backpack

instance HasModifiersFor Backpack where
  getModifiersFor (InvestigatorTarget iid) (Backpack attrs)
    | controlledBy attrs iid = pure
    $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor _ _ = pure []

instance HasAbilities Backpack where
  getAbilities (Backpack a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
            Free
    ]

instance RunMessage Backpack where
  runMessage msg a@(Backpack attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 6]
        (NonWeakness <> CardWithOneOf [CardWithTrait Item, CardWithTrait Supply]
        )
        (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      pushAll
        [ chooseUpToN
            iid
            3
            "Done choosing cards"
            [ TargetLabel
                (CardIdTarget $ toCardId c)
                [PlaceUnderneath (toTarget attrs) [c]]
            | c <- cards
            ]
        ]
      pure a
    SearchNoneFound iid target | isTarget attrs target -> do
      push $ chooseOne iid [Label "No Cards Found" []]
      pure a
    InitiatePlayCard iid cardId _ _
      | controlledBy attrs iid && cardId `elem` map
        toCardId
        (assetCardsUnderneath attrs)
      -> do
        let
          card =
            fromJustNote "card missing" $ find matcher $ assetCardsUnderneath
              attrs
          remaining = deleteFirstMatch matcher $ assetCardsUnderneath attrs
          matcher = (== cardId) . toCardId
        pushAll
          $ [ Discard (toTarget attrs) | null remaining ]
          <> [AddToHand iid card, msg]
        pure $ Backpack $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack <$> runMessage msg attrs
