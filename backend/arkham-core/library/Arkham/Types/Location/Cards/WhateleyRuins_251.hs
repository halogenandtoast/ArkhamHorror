module Arkham.Types.Location.Cards.WhateleyRuins_251
  ( whateleyRuins_251
  , WhateleyRuins_251(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (whateleyRuins_251)
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

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

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1)

instance ActionRunner env => HasActions env WhateleyRuins_251 where
  getActions iid NonFast (WhateleyRuins_251 attrs) | locationRevealed attrs =
    withBaseActions iid NonFast attrs $ pure [locationAbility (ability attrs)]
  getActions i window (WhateleyRuins_251 attrs) = getActions i window attrs

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
