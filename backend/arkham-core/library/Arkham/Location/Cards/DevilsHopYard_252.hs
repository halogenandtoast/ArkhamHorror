module Arkham.Location.Cards.DevilsHopYard_252
  ( devilsHopYard_252
  , DevilsHopYard_252(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (devilsHopYard_252)
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

newtype DevilsHopYard_252 = DevilsHopYard_252 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_252 :: LocationCard DevilsHopYard_252
devilsHopYard_252 = location
  DevilsHopYard_252
  Cards.devilsHopYard_252
  1
  (Static 2)
  Hourglass
  [Square, Plus]

instance HasAbilities DevilsHopYard_252 where
  getAbilities (DevilsHopYard_252 attrs) =
    withBaseAbilities attrs $
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

instance RunMessage DevilsHopYard_252 where
  runMessage msg l@(DevilsHopYard_252 attrs) = case msg of
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
        placeClueOnAbomination iid' = chooseOne
          iid'
          [ TargetLabel
              target
              [PlaceClues target 1, InvestigatorSpendClues iid' 1]
          | target <- abominations
          ]

      l <$ push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              ([placeClueOnAbomination iid']
              <> [ chooseOne
                     iid'
                     [ Label
                       "Spend a second clue"
                       [placeClueOnAbomination iid']
                     , Label "Do not spend a second clue" []
                     ]
                 | clueCount > 1
                 ]
              )
          | (iid', clueCount) <- investigatorWithCluePairs
          ]
        )
    _ -> DevilsHopYard_252 <$> runMessage msg attrs
