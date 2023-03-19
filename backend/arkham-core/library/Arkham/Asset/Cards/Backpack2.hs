module Arkham.Asset.Cards.Backpack2
  ( backpack2
  , Backpack2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( Supply )
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( PlaceUnderneath )
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype Backpack2 = Backpack2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack2 :: AssetCard Backpack2
backpack2 = asset Backpack2 Cards.backpack2

instance HasModifiersFor Backpack2 where
  getModifiersFor (InvestigatorTarget iid) (Backpack2 attrs)
    | controlledBy attrs iid = pure
    $ toModifiers attrs (map AsIfInHand $ assetCardsUnderneath attrs)
  getModifiersFor _ _ = pure []

instance HasAbilities Backpack2 where
  getAbilities (Backpack2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
            Free
    ]

instance RunMessage Backpack2 where
  runMessage msg a@(Backpack2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 12]
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
    InitiatePlayCard iid card _ _ _ | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs
      -> do
        let
          remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
        pushAll
          $ [ Discard (toSource attrs) (toTarget attrs) | null remaining ]
          <> [AddToHand iid card, msg]
        pure $ Backpack2 $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack2 <$> runMessage msg attrs
