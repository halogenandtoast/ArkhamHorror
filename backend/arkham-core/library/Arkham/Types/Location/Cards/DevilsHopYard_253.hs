module Arkham.Types.Location.Cards.DevilsHopYard_253
  ( devilsHopYard_253
  , DevilsHopYard_253(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (devilsHopYard_253)
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

newtype DevilsHopYard_253 = DevilsHopYard_253 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_253 :: LocationId -> DevilsHopYard_253
devilsHopYard_253 = DevilsHopYard_253 . baseAttrs
  Cards.devilsHopYard_253
  2
  (PerPlayer 1)
  Hourglass
  [Square, Plus]

instance HasModifiersFor env DevilsHopYard_253 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env DevilsHopYard_253 where
  getActions iid FastPlayerWindow (DevilsHopYard_253 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      investigatorsWithClues <- notNull <$> locationInvestigatorsWithClues attrs
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | investigatorsWithClues && anyAbominations
        ]
  getActions iid window (DevilsHopYard_253 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DevilsHopYard_253 where
  runMessage msg l@(DevilsHopYard_253 attrs) = case msg of
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
    _ -> DevilsHopYard_253 <$> runMessage msg attrs
