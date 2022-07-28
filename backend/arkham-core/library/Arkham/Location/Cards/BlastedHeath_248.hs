module Arkham.Location.Cards.BlastedHeath_248
  ( blastedHeath_248
  , BlastedHeath_248(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (blastedHeath_248)
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

newtype BlastedHeath_248 = BlastedHeath_248 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_248 :: LocationCard BlastedHeath_248
blastedHeath_248 = location
  BlastedHeath_248
  Cards.blastedHeath_248
  4
  (Static 3)
  Square
  [Circle, Hourglass]

instance HasAbilities BlastedHeath_248 where
  getAbilities (BlastedHeath_248 attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
            attrs
            1
            (Here
            <> InvestigatorExists
                 (InvestigatorAt YourLocation <> InvestigatorWithAnyClues)
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

instance RunMessage BlastedHeath_248 where
  runMessage msg l@(BlastedHeath_248 attrs) = case msg of
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
    _ -> BlastedHeath_248 <$> runMessage msg attrs
