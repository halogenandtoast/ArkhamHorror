module Arkham.Act.Cards.HarlanIsInDanger
  ( HarlanIsInDanger(..)
  , harlanIsInDanger
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement

newtype HarlanIsInDanger = HarlanIsInDanger ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlanIsInDanger :: ActCard HarlanIsInDanger
harlanIsInDanger =
  act (1, A) HarlanIsInDanger Cards.harlanIsInDanger
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Miskatonic University"

instance RunMessage HarlanIsInDanger where
  runMessage msg a@(HarlanIsInDanger attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      harlanEarnstone <- genCard Assets.harlanEarnstone
      easttown <- selectJust $ LocationWithTitle "Easttown"
      pushAll
        [ CreateAssetAt harlanEarnstone (AtLocation easttown)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> HarlanIsInDanger <$> runMessage msg attrs
