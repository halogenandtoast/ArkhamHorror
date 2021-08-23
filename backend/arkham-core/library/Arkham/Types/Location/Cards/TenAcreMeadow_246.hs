module Arkham.Types.Location.Cards.TenAcreMeadow_246
  ( tenAcreMeadow_246
  , TenAcreMeadow_246(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (tenAcreMeadow_246)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window

newtype TenAcreMeadow_246 = TenAcreMeadow_246 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246 :: LocationCard TenAcreMeadow_246
tenAcreMeadow_246 = location
  TenAcreMeadow_246
  Cards.tenAcreMeadow_246
  3
  (Static 1)
  Diamond
  [Circle, Triangle, Plus]

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasAbilities env TenAcreMeadow_246 where
  getAbilities iid window@(Window Timing.When FastPlayerWindow) (TenAcreMeadow_246 attrs)
    = withBaseAbilities iid window attrs $ do
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure [ locationAbility (ability attrs) | anyAbominations ]
  getAbilities iid window (TenAcreMeadow_246 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env TenAcreMeadow_246 where
  runMessage msg l@(TenAcreMeadow_246 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ pushAll
        [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid)
                [ PlaceClues (EnemyTarget eid) 1
                , CreateEffect
                  "02246"
                  Nothing
                  (toSource attrs)
                  (EnemyTarget eid)
                ]
            | eid <- abominations
            ]
        ]
    _ -> TenAcreMeadow_246 <$> runMessage msg attrs
