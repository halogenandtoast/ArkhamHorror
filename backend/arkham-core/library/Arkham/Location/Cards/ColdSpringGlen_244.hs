module Arkham.Location.Cards.ColdSpringGlen_244
  ( coldSpringGlen_244
  , ColdSpringGlen_244(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards ( coldSpringGlen_244 )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype ColdSpringGlen_244 = ColdSpringGlen_244 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_244 :: LocationCard ColdSpringGlen_244
coldSpringGlen_244 =
  location ColdSpringGlen_244 Cards.coldSpringGlen_244 3 (Static 2)

instance HasModifiersFor ColdSpringGlen_244 where
  getModifiersFor (EnemyTarget eid) (ColdSpringGlen_244 attrs) =
    pure $ toModifiers
      attrs
      [ EnemyEvade (-1) | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities ColdSpringGlen_244 where
  getAbilities (ColdSpringGlen_244 attrs) = withResignAction
    attrs
    [ limitedAbility (GroupLimit PerGame 1) $ restrictedAbility
        attrs
        1
        (Here
        <> InvestigatorExists (You <> InvestigatorWithAnyClues)
        <> EnemyCriteria
             (EnemyExists $ EnemyAt YourLocation <> EnemyWithTrait Abomination
             )
        )
        (FastAbility Free)
    | locationRevealed attrs
    ]

instance RunMessage ColdSpringGlen_244 where
  runMessage msg l@(ColdSpringGlen_244 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      investigatorWithCluePairs <-
        selectWithField InvestigatorClues
        $ investigatorAt (toId attrs)
        <> InvestigatorWithAnyClues
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

      pushAll
        $ placeClueOnAbomination
        : [ chooseOne
              iid
              [ Label "Spend a second clue" [placeClueOnAbomination]
              , Label "Do not spend a second clue" []
              ]
          | totalClues > 1
          ]
      pure l
    _ -> ColdSpringGlen_244 <$> runMessage msg attrs
