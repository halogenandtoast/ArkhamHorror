module Arkham.Location.Cards.MetropolitanCathedral
  ( metropolitanCathedral
  , MetropolitanCathedral(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype MetropolitanCathedral = MetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

metropolitanCathedral :: LocationCard MetropolitanCathedral
metropolitanCathedral = locationWith
  MetropolitanCathedral
  Cards.metropolitanCathedral
  3
  (Static 0)
  (labelL .~ "square")

instance HasAbilities MetropolitanCathedral where
  getAbilities (MetropolitanCathedral attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 Here
        $ ActionAbility Nothing
        $ ActionCost 1
        <> HorrorCost (toSource attrs) YouTarget 1
        , restrictedAbility
          attrs
          2
          (Here <> InvestigatorExists
            (You <> HandWith (LengthIs $ AtLeast $ Static 6))
          )
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
        ]
      else []

instance RunMessage MetropolitanCathedral where
  runMessage msg l@(MetropolitanCathedral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid attrs 2
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push
        $ Explore iid (toSource attrs)
        $ CardWithPrintedLocationSymbol
        $ locationSymbol attrs
      pure l
    _ -> MetropolitanCathedral <$> runMessage msg attrs
