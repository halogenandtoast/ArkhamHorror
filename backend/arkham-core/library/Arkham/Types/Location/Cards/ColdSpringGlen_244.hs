module Arkham.Types.Location.Cards.ColdSpringGlen_244
  ( coldSpringGlen_244
  , ColdSpringGlen_244(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (coldSpringGlen_244)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

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

instance HasModifiersFor env ColdSpringGlen_244 where
  getModifiersFor _ (EnemyTarget eid) (ColdSpringGlen_244 attrs) =
    pure $ toModifiers
      attrs
      [ EnemyEvade (-1) | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env ColdSpringGlen_244 where
  getAbilities iid window (ColdSpringGlen_244 attrs) =
    withResignAction iid window attrs $ pure
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

instance LocationRunner env => RunMessage env ColdSpringGlen_244 where
  runMessage msg l@(ColdSpringGlen_244 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorWithCluePairs <- filter ((> 0) . snd) <$> traverse
        (traverseToSnd (fmap unClueCount . getCount))
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
