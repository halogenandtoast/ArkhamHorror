module Arkham.Location.Cards.TenAcreMeadow_246 (
  tenAcreMeadow_246,
  TenAcreMeadow_246 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (tenAcreMeadow_246)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype TenAcreMeadow_246 = TenAcreMeadow_246 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

tenAcreMeadow_246 :: LocationCard TenAcreMeadow_246
tenAcreMeadow_246 =
  location TenAcreMeadow_246 Cards.tenAcreMeadow_246 3 (Static 1)

instance HasAbilities TenAcreMeadow_246 where
  getAbilities (TenAcreMeadow_246 attrs) =
    withBaseAbilities
      attrs
      [ groupLimit PerGame
        $ restrictedAbility
          attrs
          1
          (Here <> exists (EnemyAt YourLocation <> EnemyWithTrait Abomination))
          (FastAbility Free)
      | locationRevealed attrs
      ]

instance RunMessage TenAcreMeadow_246 where
  runMessage msg l@(TenAcreMeadow_246 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            [ targetLabel
              eid
              [ PlaceClues (toAbilitySource attrs 1) (toTarget eid) 1
              , CreateEffect
                  "02246"
                  Nothing
                  (toSource attrs)
                  (EnemyTarget eid)
              ]
            | eid <- abominations
            ]
        ]
      pure l
    _ -> TenAcreMeadow_246 <$> runMessage msg attrs
