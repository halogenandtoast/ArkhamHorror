module Arkham.Types.Location.Cards.DunwichVillage_242
  ( dunwichVillage_242
  , DunwichVillage_242(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (dunwichVillage_242)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype DunwichVillage_242 = DunwichVillage_242 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_242 :: LocationCard DunwichVillage_242
dunwichVillage_242 = location
  DunwichVillage_242
  Cards.dunwichVillage_242
  3
  (Static 1)
  Circle
  [Triangle, Square, Diamond]

instance HasAbilities env DunwichVillage_242 where
  getAbilities iid window (DunwichVillage_242 attrs) =
    withResignAction iid window attrs $ pure
      [ restrictedAbility
            attrs
            1
            (Here
            <> InvestigatorExists (You <> InvestigatorWithAnyClues)
            <> EnemyCriteria (EnemyExists $ EnemyWithTrait Abomination)
            )
            (FastAbility Free)
          & (abilityLimitL .~ GroupLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env DunwichVillage_242 where
  runMessage msg l@(DunwichVillage_242 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorsWithClues <- locationInvestigatorsWithClues attrs
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorsWithClues || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ pushAll
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
