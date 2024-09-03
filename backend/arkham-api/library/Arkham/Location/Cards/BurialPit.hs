module Arkham.Location.Cards.BurialPit (burialPit, BurialPit (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BurialPit = BurialPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialPit :: LocationCard BurialPit
burialPit =
  locationWith
    BurialPit
    Cards.burialPit
    3
    (PerPlayer 1)
    (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities BurialPit where
  getAbilities (BurialPit attrs) =
    withBaseAbilities
      attrs
      [mkAbility attrs 1 $ forced $ Explored #after You (SuccessfulExplore $ be attrs)]

instance RunMessage BurialPit where
  runMessage msg l@(BurialPit attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      let
        choose =
          chooseOne
            player
            [ Label "Draw a card from the top of the encounter deck" [drawEncounterCard iid attrs]
            , Label "Place 1 doom on burial pit" [PlaceDoom (attrs.ability 1) (toTarget attrs) 1]
            ]
      pushAll [choose, choose]
      pure l
    _ -> BurialPit <$> runMessage msg attrs
