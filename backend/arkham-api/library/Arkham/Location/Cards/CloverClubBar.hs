module Arkham.Location.Cards.CloverClubBar (cloverClubBar, CloverClubBar (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (cloverClubBar)
import Arkham.Location.Runner
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: LocationCard CloverClubBar
cloverClubBar = location CloverClubBar Cards.cloverClubBar 3 (Static 0)

instance HasAbilities CloverClubBar where
  getAbilities (CloverClubBar attrs) =
    extendRevealed
      attrs
      [ playerLimit PerGame
          $ restrictedAbility attrs 1 (OnAct 1 <> Here)
          $ actionAbilityWithCost (ResourceCost 2)
      ]

instance RunMessage CloverClubBar where
  runMessage msg l@(CloverClubBar attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let drawing = drawCards iid (attrs.ability 1) 2
      name <- field InvestigatorName iid
      pushAll [GainClues iid (attrs.ability 1) 2, drawing, Remember $ HadADrink $ labeled name iid]
      pure l
    _ -> CloverClubBar <$> runMessage msg attrs
