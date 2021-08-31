module Arkham.Types.Location.Cards.BurnedRuins_205
  ( burnedRuins_205
  , BurnedRuins_205(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (burnedRuins_205)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env BurnedRuins_205 where
  getAbilities i w (BurnedRuins_205 x) = do
    rest <- withDrawCardUnderneathAction i w x
    pure
      $ [ mkAbility x 1
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
