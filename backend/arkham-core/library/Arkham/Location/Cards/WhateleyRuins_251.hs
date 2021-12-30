module Arkham.Location.Cards.WhateleyRuins_251
  ( whateleyRuins_251
  , WhateleyRuins_251(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (whateleyRuins_251)
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype WhateleyRuins_251 = WhateleyRuins_251 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whateleyRuins_251 :: LocationCard WhateleyRuins_251
whateleyRuins_251 = location
  WhateleyRuins_251
  Cards.whateleyRuins_251
  2
  (PerPlayer 2)
  Plus
  [Triangle, Diamond, Hourglass]

instance HasModifiersFor env WhateleyRuins_251 where
  getModifiersFor _ (InvestigatorTarget iid) (WhateleyRuins_251 attrs) =
    pure $ toModifiers
      attrs
      [ SkillModifier SkillWillpower (-1) | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WhateleyRuins_251 where
  getAbilities (WhateleyRuins_251 attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env WhateleyRuins_251 where
  runMessage msg l@(WhateleyRuins_251 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 4)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        abominations <- getSetList @EnemyId (CardCode "02255")
        abominationsWithLocation <- traverse (traverseToSnd getId) abominations
        abominationsWithLocationAndAccessibleLocations <-
          for abominationsWithLocation $ \(abomination, locationId) ->
            (abomination, locationId, )
              . map unEnemyAccessibleLocationId
              <$> getSetList (abomination, locationId)

        l <$ push
          (chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid)
                [ chooseOne
                    iid
                    [ TargetLabel
                        (LocationTarget destination)
                        [EnemyMove eid from destination]
                    ]
                | destination <- destinations
                ]
            | (eid, from, destinations) <-
              abominationsWithLocationAndAccessibleLocations
            ]
          )
    _ -> WhateleyRuins_251 <$> runMessage msg attrs
