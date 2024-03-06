module Arkham.Location.Cards.FrankElwoodsRoom (
  frankElwoodsRoom,
  FrankElwoodsRoom (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Projection

newtype FrankElwoodsRoom = FrankElwoodsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frankElwoodsRoom :: LocationCard FrankElwoodsRoom
frankElwoodsRoom =
  location FrankElwoodsRoom Cards.frankElwoodsRoom 3 (PerPlayer 1)

instance HasAbilities FrankElwoodsRoom where
  getAbilities (FrankElwoodsRoom attrs) = withRevealedAbilities attrs [haunted hauntedText attrs 1]
   where
    hauntedText =
      "You must either place 1 of your clues on Frank Elwood's Room, or place 1 doom on the current agenda."

instance RunMessage FrankElwoodsRoom where
  runMessage msg l@(FrankElwoodsRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Place 1 of your clues on Frank Elwood's Room"
            [InvestigatorSpendClues iid 1, PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1]
          | hasClues
          ]
        <> [Label "Place 1 doom on the current agenda" [placeDoomOnAgenda]]
      pure l
    _ -> FrankElwoodsRoom <$> runMessage msg attrs
