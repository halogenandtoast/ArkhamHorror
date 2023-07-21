module Arkham.Location.Cards.TimeWrackedWoods (
  timeWrackedWoods,
  TimeWrackedWoods (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype TimeWrackedWoods = TimeWrackedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWrackedWoods :: LocationCard TimeWrackedWoods
timeWrackedWoods =
  location TimeWrackedWoods Cards.timeWrackedWoods 4 (PerPlayer 2)

instance HasAbilities TimeWrackedWoods where
  getAbilities (TimeWrackedWoods attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            attrs
            1
            ( Here
                <> InVictoryDisplay
                  (CardWithVengeance <> NotCard (CardWithTrait Elite))
                  (AtLeast $ Static 1)
            )
          $ ActionAbility Nothing
          $ ActionCost 2
      ]

instance RunMessage TimeWrackedWoods where
  runMessage msg l@(TimeWrackedWoods attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <-
        mapMaybe (preview _EncounterCard)
          <$> selectList
            ( VictoryDisplayCardMatch $
                CardWithVengeance
                  <> NotCard
                    (CardWithTrait Elite)
            )
      pushAll
        [ FocusCards $ map EncounterCard targets
        , chooseOrRunOne
            iid
            [ TargetLabel (CardIdTarget $ toCardId c) [AddToEncounterDiscard c]
            | c <- targets
            ]
        , UnfocusCards
        ]
      pure l
    _ -> TimeWrackedWoods <$> runMessage msg attrs
