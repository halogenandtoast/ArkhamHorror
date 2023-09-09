module Arkham.Act.Cards.TheCosmosBeckons (
  TheCosmosBeckons (..),
  theCosmosBeckons,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck

newtype TheCosmosBeckons = TheCosmosBeckons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCosmosBeckons :: ActCard TheCosmosBeckons
theCosmosBeckons =
  act
    (1, A)
    TheCosmosBeckons
    Cards.theCosmosBeckons
    (Just $ GroupClueCost (PerPlayer 1) "Hideous Palace")

instance HasAbilities TheCosmosBeckons where
  getAbilities (TheCosmosBeckons attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
            $ ActionAbility Nothing
            $ ActionCost 1 <> ClueCostX
        ]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage TheCosmosBeckons where
  runMessage msg a@(TheCosmosBeckons attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawFromScenarioDeck iid CosmosDeck (toTarget attrs) x
      pure a
    DrewFromScenarioDeck iid _ (isTarget attrs -> True) cards -> do
      cardsWithMsgs <- traverse (traverseToSnd placeLocation) cards
      pushAll
        [ FocusCards $ map flipCard cards
        , chooseOrRunOne
            iid
            [ targetLabel (toCardId card) [UnfocusCards, placement, RunCosmos iid lid]
            | (card, (lid, placement)) <- cardsWithMsgs
            ]
        ]
      pure a
    _ -> TheCosmosBeckons <$> runMessage msg attrs
