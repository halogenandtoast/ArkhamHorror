module Arkham.Asset.Cards.Backpack (
  backpack,
  Backpack (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (Supply)
import Arkham.Card
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype Backpack = Backpack AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

backpack :: AssetCard Backpack
backpack = asset Backpack Cards.backpack

instance HasModifiersFor Backpack where
  getModifiersFor (InvestigatorTarget iid) (Backpack attrs)
    | controlledBy attrs iid =
        pure
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
      push
        $ search
          iid
          source
          iid
          [fromTopOfDeck 6]
          ( NonWeakness <> CardWithOneOf [CardWithTrait Item, CardWithTrait Supply]
          )
          (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      player <- getPlayer iid
      pushAll
        [ chooseUpToN
            player
            (3 + additionalTargets)
            "Done choosing cards"
            [ TargetLabel
              (CardIdTarget $ toCardId c)
              [PlaceUnderneath (toTarget attrs) [c]]
            | c <- cards
            ]
        ]
      pure a
    SearchNoneFound iid target | isTarget attrs target -> do
      player <- getPlayer iid
      push $ chooseOne player [Label "No Cards Found" []]
      pure a
    InitiatePlayCard iid card _ _ _ | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs ->
      do
        let
          remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
        pushAll
          $ [toDiscardBy iid attrs attrs | null remaining]
          <> [addToHand iid card, msg]
        pure $ Backpack $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack <$> runMessage msg attrs
