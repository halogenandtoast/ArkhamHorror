module Arkham.Location.Cards.RecordsOffice (
  recordsOffice,
  RecordsOffice (..),
)
where

import Arkham.Prelude

import Arkham.Game.Helpers (getActions)
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype RecordsOffice = RecordsOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recordsOffice :: LocationCard RecordsOffice
recordsOffice = location RecordsOffice Cards.recordsOffice 3 (PerPlayer 2)

instance HasAbilities RecordsOffice where
  getAbilities (RecordsOffice attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          (Here <> exists (You <> NotInvestigator (InvestigatorEngagedWith AnyEnemy)))
          $ ActionAbility []
          $ ActionCost 3
      ]

instance RunMessage RecordsOffice where
  runMessage msg l@(RecordsOffice attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ DoStep 1 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      player <- getPlayer iid
      actions <- withModifiers iid (toModifiers attrs [ActionCostModifier (-1)]) $ do
        concatMapM (getActions iid) (defaultWindows iid)

      let available = filter (elem #investigate . abilityActions) (nub actions)

      pushAll
        $ [ chooseOne player
              $ map (\ab -> AbilityLabel iid ab (defaultWindows iid) []) available
          ]
        <> [DoStep (n + 1) msg' | n < 4]

      pure l
    _ -> RecordsOffice <$> runMessage msg attrs
