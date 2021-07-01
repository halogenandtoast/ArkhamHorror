module Arkham.Types.Location.Cards.DunwichVillage_242
  ( dunwichVillage_242
  , DunwichVillage_242(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (dunwichVillage_242)
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

newtype DunwichVillage_242 = DunwichVillage_242 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_242 :: LocationId -> DunwichVillage_242
dunwichVillage_242 = DunwichVillage_242 . baseAttrs
  Cards.dunwichVillage_242
  3
  (Static 1)
  Circle
  [Triangle, Square, Diamond]

instance HasModifiersFor env DunwichVillage_242 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env DunwichVillage_242 where
  getActions iid NonFast (DunwichVillage_242 attrs) =
    withResignAction iid NonFast attrs
  getActions iid FastPlayerWindow (DunwichVillage_242 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      investigatorsWithClues <- notNull <$> locationInvestigatorsWithClues attrs
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | investigatorsWithClues && anyAbominations
        ]
  getActions iid window (DunwichVillage_242 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env DunwichVillage_242 where
  runMessage msg l@(DunwichVillage_242 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorsWithClues <- locationInvestigatorsWithClues attrs
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorsWithClues || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ unshiftMessages
        [ chooseOne
            iid
            [ Label
              "Place clue on Abomination"
              [ chooseOne
                  iid
                  [ TargetLabel
                      (EnemyTarget eid)
                      [ PlaceClues (EnemyTarget eid) 1
                      , InvestigatorSpendClues iid 1
                      ]
                  | eid <- abominations
                  ]
              ]
            , Label "Do not place clue on Abomination" []
            ]
        | iid <- investigatorsWithClues
        ]
    _ -> DunwichVillage_242 <$> runMessage msg attrs
