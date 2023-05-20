module Arkham.Location.Cards.DevilsHopYard_252 (
  devilsHopYard_252,
  DevilsHopYard_252 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (devilsHopYard_252)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype DevilsHopYard_252 = DevilsHopYard_252 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_252 :: LocationCard DevilsHopYard_252
devilsHopYard_252 =
  location DevilsHopYard_252 Cards.devilsHopYard_252 1 (Static 2)

instance HasAbilities DevilsHopYard_252 where
  getAbilities (DevilsHopYard_252 attrs) =
    withBaseAbilities attrs $
      [ limitedAbility (GroupLimit PerGame 1) $
        restrictedAbility
          attrs
          1
          ( Here
              <> InvestigatorExists (You <> InvestigatorWithAnyClues)
              <> EnemyCriteria
                ( EnemyExists $
                    EnemyAt YourLocation
                      <> EnemyWithTrait Abomination
                )
          )
          (FastAbility Free)
      | locationRevealed attrs
      ]

instance RunMessage DevilsHopYard_252 where
  runMessage msg l@(DevilsHopYard_252 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      investigatorWithCluePairs <-
        selectWithField InvestigatorClues $
          investigatorAt (toId attrs)
            <> InvestigatorWithAnyClues
      abominations <-
        map EnemyTarget <$> locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorWithCluePairs || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      let
        placeClueOnAbomination iid' =
          chooseOne
            iid'
            [ targetLabel
              target
              [PlaceClues (toAbilitySource attrs 1) target 1, InvestigatorSpendClues iid' 1]
            | target <- abominations
            ]

      push $
        chooseOne
          iid
          [ targetLabel iid' $
            placeClueOnAbomination iid'
              : [ chooseOne
                  iid'
                  [ Label "Spend a second clue" [placeClueOnAbomination iid']
                  , Label "Do not spend a second clue" []
                  ]
                | clueCount > 1
                ]
          | (iid', clueCount) <- investigatorWithCluePairs
          ]
      pure l
    _ -> DevilsHopYard_252 <$> runMessage msg attrs
