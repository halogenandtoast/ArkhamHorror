module Arkham.Location.Cards.Xochimilco
  ( xochimilco
  , Xochimilco(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype Xochimilco = Xochimilco LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

xochimilco :: LocationCard Xochimilco
xochimilco =
  locationWith Xochimilco Cards.xochimilco 4 (Static 0) (labelL .~ "heart")

instance HasModifiersFor Xochimilco where
  getModifiersFor (InvestigatorTarget iid) (Xochimilco a) = do
    atXochimilco <- iid <=~> InvestigatorAt (LocationWithId $ toId a)
    pure $ toModifiers a [ CannotGainResources | atXochimilco ]
  getModifiersFor _ _ = pure []

instance HasAbilities Xochimilco where
  getAbilities (Xochimilco attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
      <> ResourceCost 3
    | locationRevealed attrs
    ]

instance RunMessage Xochimilco where
  runMessage msg l@(Xochimilco attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure l
    _ -> Xochimilco <$> runMessage msg attrs
