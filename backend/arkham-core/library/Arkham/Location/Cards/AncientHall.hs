module Arkham.Location.Cards.AncientHall
  ( ancientHall
  , AncientHall(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype AncientHall = AncientHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHall :: LocationCard AncientHall
ancientHall = locationWith
  AncientHall
  Cards.ancientHall
  3
  (PerPlayer 2)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities AncientHall where
  getAbilities (AncientHall attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (CluesOnThis $ AtLeast $ Static 1)
        (ForcedAbility $ RoundEnds $ Timing.When)
    ]

instance RunMessage AncientHall where
  runMessage msg l@(AncientHall attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      iids <-
        selectList
        $ InvestigatorAt (LocationWithId $ toId attrs)
        <> InvestigatorCanSpendResources (Static 3)
      leadInvestigatorId <- getLeadInvestigatorId
      let flipClue = FlipClues (toTarget attrs) 1
      if null iids
        then push flipClue
        else
          push
          $ chooseOne leadInvestigatorId
          $ Label "Do not spend 3 resources to cancel this effect" [flipClue]
          : [ targetLabel iid [SpendResources iid 3] | iid <- iids ]
      pure l
    _ -> AncientHall <$> runMessage msg attrs
