module Arkham.Types.Location.Cards.DevilsHopYard_253
  ( devilsHopYard_253
  , DevilsHopYard_253(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (devilsHopYard_253)
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
import Arkham.Types.Target
import Arkham.Types.Trait

newtype DevilsHopYard_253 = DevilsHopYard_253 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_253 :: LocationCard DevilsHopYard_253
devilsHopYard_253 = location
  DevilsHopYard_253
  Cards.devilsHopYard_253
  2
  (PerPlayer 1)
  Hourglass
  [Square, Plus]

instance HasAbilities DevilsHopYard_253 where
  getAbilities (DevilsHopYard_253 attrs) = withBaseAbilities
    attrs
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

instance LocationRunner env => RunMessage env DevilsHopYard_253 where
  runMessage msg l@(DevilsHopYard_253 attrs) = case msg of
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
    _ -> DevilsHopYard_253 <$> runMessage msg attrs
