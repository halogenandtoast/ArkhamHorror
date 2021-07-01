module Arkham.Types.Location.Cards.WhateleyRuins_250
  ( whateleyRuins_250
  , WhateleyRuins_250(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (whateleyRuins_250)
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
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype WhateleyRuins_250 = WhateleyRuins_250 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whateleyRuins_250 :: LocationId -> WhateleyRuins_250
whateleyRuins_250 = WhateleyRuins_250 . baseAttrs
  Cards.whateleyRuins_250
  3
  (PerPlayer 2)
  Plus
  [Triangle, Diamond, Hourglass]

instance HasModifiersFor env WhateleyRuins_250 where
  getModifiersFor _ (InvestigatorTarget iid) (WhateleyRuins_250 attrs) =
    pure $ toModifiers
      attrs
      [ SkillModifier SkillWillpower (-1) | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (FastAbility Free)
    & (abilityLimitL .~ GroupLimit PerGame 1)

instance ActionRunner env => HasActions env WhateleyRuins_250 where
  getActions iid FastPlayerWindow (WhateleyRuins_250 attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      investigatorsWithClues <- notNull <$> locationInvestigatorsWithClues attrs
      anyAbominations <- notNull <$> locationEnemiesWithTrait attrs Abomination
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | investigatorsWithClues && anyAbominations
        ]
  getActions iid window (WhateleyRuins_250 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env WhateleyRuins_250 where
  runMessage msg l@(WhateleyRuins_250 attrs) = case msg of
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
              <> [ chooseOne
                     iid'
                     [ Label
                       "Spend a third clue"
                       [placeClueOnAbomination iid']
                     , Label "Do not spend a third clue" []
                     ]
                 | clueCount > 2
                 ]
              )
          | (iid', clueCount) <- investigatorWithCluePairs
          ]
        )
    _ -> WhateleyRuins_250 <$> runMessage msg attrs
