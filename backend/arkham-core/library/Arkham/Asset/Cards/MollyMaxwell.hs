module Arkham.Asset.Cards.MollyMaxwell
  ( mollyMaxwell
  , MollyMaxwell(..)
  ) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Deck
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Trait ( toTraits )

newtype MollyMaxwell = MollyMaxwell AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mollyMaxwell :: AssetCard MollyMaxwell
mollyMaxwell = ally MollyMaxwell Cards.mollyMaxwell (2, 4)

instance HasAbilities MollyMaxwell where
  getAbilities (MollyMaxwell a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ ExhaustCost (toTarget a)
        <> HorrorCost (toSource a) (toTarget a) 1
    ]

instance RunMessage MollyMaxwell where
  runMessage msg a@(MollyMaxwell attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      cards <- filter (`cardMatch` Matcher.AssetCard)
        <$> getKnownRemainingOriginalDeckCards iid
      let deckTraits = toList . unions $ map toTraits cards
      push
        $ chooseOneDropDown iid
        $ ( "Trait that won't match"
          , RevealUntilFirst
            iid
            (toSource attrs)
            (InvestigatorDeck iid)
            (NotCard AnyCard)
          )
        : [ ( tshow trait
            , RevealUntilFirst
              iid
              (toSource attrs)
              (InvestigatorDeck iid)
              (CardWithTrait trait)
            )
          | trait <- deckTraits
          ]
      pure a
    RevealedCards iid (isSource attrs -> True) _ mcard rest -> do
      pushAll $ case mcard of
        Nothing ->
          [ FocusCards rest
          , chooseOne
            iid
            [ Label
                "No cards found"
                [UnfocusCards, ShuffleCardsIntoDeck (InvestigatorDeck iid) rest]
            ]
          ]
        Just c ->
          [ FocusCards (rest <> [c])
          , chooseOne
            iid
            [ targetLabel
                (toCardId c)
                [ UnfocusCards
                , AddToHand iid c
                , ShuffleCardsIntoDeck (InvestigatorDeck iid) rest
                ]
            ]
          ]
      pure a
    _ -> MollyMaxwell <$> runMessage msg attrs
