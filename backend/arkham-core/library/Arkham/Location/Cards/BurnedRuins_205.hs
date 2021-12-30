module Arkham.Location.Cards.BurnedRuins_205
  ( burnedRuins_205
  , BurnedRuins_205(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (burnedRuins_205)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype BurnedRuins_205 = BurnedRuins_205 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_205 :: LocationCard BurnedRuins_205
burnedRuins_205 = location
  BurnedRuins_205
  Cards.burnedRuins_205
  2
  (Static 3)
  Triangle
  [Square, Diamond]

instance HasAbilities BurnedRuins_205 where
  getAbilities (BurnedRuins_205 x) = do
    let rest = withDrawCardUnderneathAction x
    [ mkAbility x 1
          $ ForcedAbility
          $ SkillTestResult
              Timing.After
              You
              (WhileInvestigating $ LocationWithId $ toId x)
          $ FailureResult AnyValue
        | locationRevealed x
        ]
      <> rest

instance LocationRunner env => RunMessage env BurnedRuins_205 where
  runMessage msg l@(BurnedRuins_205 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> l <$ when
      (locationClues attrs > 0)
      (pushAll [RemoveClues (toTarget attrs) 1, PlaceDoom (toTarget attrs) 1])
    _ -> BurnedRuins_205 <$> runMessage msg attrs
