module Arkham.Types.Location.Cards.TenAcreMeadow_246
  ( tenAcreMeadow_246
  , TenAcreMeadow_246(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (tenAcreMeadow_246)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype TenAcreMeadow_246 = TenAcreMeadow_246 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_246 :: LocationId -> TenAcreMeadow_246
tenAcreMeadow_246 = TenAcreMeadow_246 . baseAttrs
  Cards.tenAcreMeadow_246
  3
  (Static 1)
  Diamond
  [Circle, Triangle, Plus]

instance HasModifiersFor env TenAcreMeadow_246 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env TenAcreMeadow_246 where
  getActions iid FastPlayerWindow (TenAcreMeadow_246 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure [ ActivateCardAbilityAction iid (ability attrs) | anyAbominations ]
  getActions iid window (TenAcreMeadow_246 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TenAcreMeadow_246 where
  runMessage msg l@(TenAcreMeadow_246 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ unshiftMessages
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
