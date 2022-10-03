module Arkham.Location.Cards.CryptOfTheSepulchralLamp
  ( cryptOfTheSepulchralLamp
  , CryptOfTheSepulchralLamp(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype CryptOfTheSepulchralLamp = CryptOfTheSepulchralLamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptOfTheSepulchralLamp :: LocationCard CryptOfTheSepulchralLamp
cryptOfTheSepulchralLamp = locationWith
  CryptOfTheSepulchralLamp
  Cards.cryptOfTheSepulchralLamp
  2
  (PerPlayer 2)
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasAbilities CryptOfTheSepulchralLamp where
  getAbilities (CryptOfTheSepulchralLamp attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (AnyCriterion
          [ Negate
              (LocationExists
              $ LocationInDirection dir (LocationWithId $ toId attrs)
              )
          | dir <- [Above, RightOf]
          ]
        )
      $ ForcedAbility
      $ RevealLocation Timing.When Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage CryptOfTheSepulchralLamp where
  runMessage msg l@(CryptOfTheSepulchralLamp attrs) = case msg of
    Investigate iid lid s mt _ False | lid == toId attrs -> do
      let investigate = Investigate iid lid s mt SkillWillpower False
      CryptOfTheSepulchralLamp <$> runMessage investigate attrs
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      n <- countM (directionEmpty attrs) [Above, RightOf]
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) n)
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      placements <- mapMaybeM (toMaybePlacement attrs) [Above, RightOf]
      pushAll $ concat $ zipWith ($) placements cards
      pure l
    _ -> CryptOfTheSepulchralLamp <$> runMessage msg attrs
