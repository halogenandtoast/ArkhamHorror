module Arkham.Types.Location.Cards.DevilsHopYard_252
  ( devilsHopYard_252
  , DevilsHopYard_252(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (devilsHopYard_252)
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
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DevilsHopYard_252 = DevilsHopYard_252 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_252 :: LocationId -> DevilsHopYard_252
devilsHopYard_252 = DevilsHopYard_252 . baseAttrs
  Cards.devilsHopYard_252
  1
  (Static 2)
  Hourglass
  [Square, Plus]

instance HasModifiersFor env DevilsHopYard_252 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env DevilsHopYard_252 where
  getActions iid FastPlayerWindow (DevilsHopYard_252 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      investigatorsWithClues <- notNull <$> locationInvestigatorsWithClues attrs
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | investigatorsWithClues && anyAbominations
        ]
  getActions iid window (DevilsHopYard_252 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DevilsHopYard_252 where
  runMessage msg l@(DevilsHopYard_252 attrs) = case msg of
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
        placeClueOnAbomination iid' = chooseOne
          iid'
          [ TargetLabel
              target
              [PlaceClues target 1, InvestigatorSpendClues iid' 1]
          | target <- abominations
          ]

      l <$ unshiftMessage
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
