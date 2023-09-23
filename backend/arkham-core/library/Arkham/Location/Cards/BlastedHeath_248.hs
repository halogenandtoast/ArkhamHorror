module Arkham.Location.Cards.BlastedHeath_248 (
  blastedHeath_248,
  BlastedHeath_248 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (blastedHeath_248)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype BlastedHeath_248 = BlastedHeath_248 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_248 :: LocationCard BlastedHeath_248
blastedHeath_248 =
  location BlastedHeath_248 Cards.blastedHeath_248 4 (Static 3)

instance HasAbilities BlastedHeath_248 where
  getAbilities (BlastedHeath_248 attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            attrs
            1
            ( Here
                <> InvestigatorExists
                  (InvestigatorAt YourLocation <> InvestigatorWithAnyClues)
                <> EnemyCriteria
                  ( EnemyExists
                      $ EnemyAt YourLocation
                      <> EnemyWithTrait Abomination
                  )
            )
            (FastAbility Free)
        | locationRevealed attrs
        ]

instance RunMessage BlastedHeath_248 where
  runMessage msg l@(BlastedHeath_248 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      investigatorWithCluePairs <-
        selectWithField InvestigatorClues
          $ investigatorAt (toId attrs)
          <> InvestigatorWithAnyClues
      abominations <- selectTargets $ EnemyWithTrait Abomination <> enemyAt (toId attrs)
      when
        (null investigatorWithCluePairs || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      let
        totalClues = sum $ map snd investigatorWithCluePairs
        investigators = map fst investigatorWithCluePairs
        placeClueOnAbomination =
          chooseOne
            iid
            [ targetLabel target [SpendClues 1 investigators, PlaceClues (toAbilitySource attrs 1) target 1]
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
    _ -> BlastedHeath_248 <$> runMessage msg attrs
