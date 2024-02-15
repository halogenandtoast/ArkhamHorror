module Arkham.Location.Cards.SacredWoods_184 (
  sacredWoods_184,
  SacredWoods_184 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype SacredWoods_184 = SacredWoods_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredWoods_184 :: LocationCard SacredWoods_184
sacredWoods_184 =
  locationWith
    SacredWoods_184
    Cards.sacredWoods_184
    4
    (PerPlayer 1)
    (labelL .~ "star")

instance HasAbilities SacredWoods_184 where
  getAbilities (SacredWoods_184 attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ restrictedAbility
              attrs
              1
              (InvestigatorExists $ investigatorAt $ toId attrs)
              $ ForcedAbility
              $ PutLocationIntoPlay Timing.After Anyone
              $ LocationWithId
              $ toId attrs
          , restrictedAbility
              attrs
              2
              ( Here
                  <> InvestigatorExists (You <> DeckIsEmpty)
                  <> CluesOnThis (AtLeast $ Static 1)
                  <> CanDiscoverCluesAt (LocationWithId $ toId attrs)
              )
              $ ActionAbility []
              $ ActionCost 1
          ]
        else []

instance RunMessage SacredWoods_184 where
  runMessage msg l@(SacredWoods_184 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      iids <- select $ investigatorAt (toId attrs)
      pushAll
        [ DiscardTopOfDeck iid 10 (toAbilitySource attrs 1) Nothing
        | iid <- iids
        ]
      pure l
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      n <- field LocationClues (toId attrs)
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 1) n Nothing
      pure l
    _ -> SacredWoods_184 <$> runMessage msg attrs
