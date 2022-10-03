module Arkham.Location.Cards.BurialPit
  ( burialPit
  , BurialPit(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype BurialPit = BurialPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialPit :: LocationCard BurialPit
burialPit = locationWith
  BurialPit
  Cards.burialPit
  3
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities BurialPit where
  getAbilities (BurialPit attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ Explored
        Timing.After
        You
        (SuccessfulExplore $ LocationWithId $ toId attrs)
    ]

instance RunMessage BurialPit where
  runMessage msg l@(BurialPit attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        choose = chooseOne
          iid
          [ Label
            "Draw a card from the top of the encounter deck"
            [InvestigatorDrawEncounterCard iid]
          , Label "Place 1 doom on burial pit" [PlaceDoom (toTarget attrs) 1]
          ]
      pushAll [choose, choose]
      pure l
    _ -> BurialPit <$> runMessage msg attrs
