module Arkham.Location.Cards.ColdSpringGlen_244
  ( coldSpringGlen_244
  , ColdSpringGlen_244(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (coldSpringGlen_244)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field(..))
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait

newtype ColdSpringGlen_244 = ColdSpringGlen_244 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_244 :: LocationCard ColdSpringGlen_244
coldSpringGlen_244 = location
  ColdSpringGlen_244
  Cards.coldSpringGlen_244
  3
  (Static 2)
  Triangle
  [Circle, Diamond, Plus]

instance HasModifiersFor ColdSpringGlen_244 where
  getModifiersFor _ (EnemyTarget eid) (ColdSpringGlen_244 attrs) =
    pure $ toModifiers
      attrs
      [ EnemyEvade (-1) | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ColdSpringGlen_244 where
  getAbilities (ColdSpringGlen_244 attrs) = withResignAction
    attrs
    [ restrictedAbility
          attrs
          1
          (Here
          <> InvestigatorExists (You <> InvestigatorWithAnyClues)
          <> EnemyCriteria
               (EnemyExists
               $ EnemyAt YourLocation
               <> EnemyWithTrait Abomination
               )
          )
          (FastAbility Free)
        & (abilityLimitL .~ GroupLimit PerGame 1)
    | locationRevealed attrs
    ]

instance RunMessage ColdSpringGlen_244 where
  runMessage msg l@(ColdSpringGlen_244 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorWithCluePairs <- filter ((> 0) . snd) <$> traverse
        (traverseToSnd (field InvestigatorClues))
        (setToList $ locationInvestigators attrs)
      abominations <-
        map EnemyTarget <$> locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorWithCluePairs || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      let
        totalClues = sum $ map snd investigatorWithCluePairs
        investigators = map fst investigatorWithCluePairs
        placeClueOnAbomination = chooseOne
          iid
          [ TargetLabel target [SpendClues 1 investigators, PlaceClues target 1]
          | target <- abominations
          ]

      l <$ pushAll
        ([placeClueOnAbomination]
        <> [ chooseOne
               iid
               [ Label "Spend a second clue" [placeClueOnAbomination]
               , Label "Do not spend a second clue" []
               ]
           | totalClues > 1
           ]
        )
    _ -> ColdSpringGlen_244 <$> runMessage msg attrs
