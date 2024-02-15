module Arkham.Location.Cards.AncientHall (ancientHall, AncientHall (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype AncientHall = AncientHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHall :: LocationCard AncientHall
ancientHall =
  locationWith AncientHall Cards.ancientHall 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities AncientHall where
  getAbilities (AncientHall attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 (cluesOnThis 1) (forced $ RoundEnds #when)]

instance RunMessage AncientHall where
  runMessage msg l@(AncientHall attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt (toId attrs) <> InvestigatorCanSpendResources (Static 3)
      lead <- getLeadPlayer
      let flipClue = FlipClues (toTarget attrs) 1
      if null iids
        then push flipClue
        else
          push
            $ chooseOne lead
            $ Label "Do not spend 3 resources to cancel this effect" [flipClue]
            : [targetLabel iid [SpendResources iid 3] | iid <- iids]
      pure l
    _ -> AncientHall <$> runMessage msg attrs
