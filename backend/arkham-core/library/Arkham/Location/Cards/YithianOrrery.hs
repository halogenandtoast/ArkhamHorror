module Arkham.Location.Cards.YithianOrrery (
  yithianOrrery,
  YithianOrrery (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype YithianOrrery = YithianOrrery LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

yithianOrrery :: LocationCard YithianOrrery
yithianOrrery = location YithianOrrery Cards.yithianOrrery 4 (PerPlayer 1)

instance HasModifiersFor YithianOrrery where
  getModifiersFor (InvestigatorTarget iid) (YithianOrrery a) = do
    here <- iid <=~> investigatorAt (toId a)
    pure $ toModifiers a [HandSize 2 | here]
  getModifiersFor _ _ = pure []

instance HasAbilities YithianOrrery where
  getAbilities (YithianOrrery attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
          $ ActionAbility []
          $ ActionCost 2
      ]

instance RunMessage YithianOrrery where
  runMessage msg l@(YithianOrrery attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Remember RealizedWhatYearItIs
      pure l
    _ -> YithianOrrery <$> runMessage msg attrs
