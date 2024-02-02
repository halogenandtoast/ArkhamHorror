module Arkham.Location.Cards.WitchHauntedWoodsTaintedWell (
  witchHauntedWoodsTaintedWell,
  WitchHauntedWoodsTaintedWell (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype WitchHauntedWoodsTaintedWell = WitchHauntedWoodsTaintedWell LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

witchHauntedWoodsTaintedWell :: LocationCard WitchHauntedWoodsTaintedWell
witchHauntedWoodsTaintedWell =
  location
    WitchHauntedWoodsTaintedWell
    Cards.witchHauntedWoodsTaintedWell
    3
    (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsTaintedWell where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsTaintedWell attrs) =
    do
      mlid <- field InvestigatorLocation iid
      case mlid of
        Nothing -> pure []
        Just lid ->
          if lid == toId attrs
            then
              pure
                $ toModifiers
                  attrs
                  [ CanCommitToSkillTestPerformedByAnInvestigatorAt
                      $ LocationWithTitle "Witch-Haunted Woods"
                  ]
            else do
              isWitchHauntedWoods <-
                member lid
                  <$> select (LocationWithTitle "Witch-Haunted Woods")
              pure
                $ toModifiers
                  attrs
                  [ CanCommitToSkillTestPerformedByAnInvestigatorAt
                    (LocationWithId $ toId attrs)
                  | isWitchHauntedWoods
                  ]
  getModifiersFor _ _ = pure []

instance RunMessage WitchHauntedWoodsTaintedWell where
  runMessage msg (WitchHauntedWoodsTaintedWell attrs) =
    WitchHauntedWoodsTaintedWell <$> runMessage msg attrs
