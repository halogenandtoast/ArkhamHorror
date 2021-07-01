module Arkham.Types.Enemy.Cards.JeremiahPierce
  ( JeremiahPierce(..)
  , jeremiahPierce
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyCard JeremiahPierce
jeremiahPierce = enemy JeremiahPierce Cards.jeremiahPierce
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 4)
  . (healthL .~ Static 3)
  . (evadeL .~ 4)

instance HasModifiersFor env JeremiahPierce where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env JeremiahPierce where
  getActions iid NonFast (JeremiahPierce attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (EnemySource enemyId)
              1
              (ActionAbility (Just Parley) (ActionCost 1))
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId -> do
      mYourHouse <- getLocationIdWithTitle "Your House"
      let
        spawnLocation =
          LocationWithTitle $ maybe "Rivertown" (const "Your House") mYourHouse
      e <$ spawnAt (Just iid) eid spawnLocation
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ unshiftMessages
        [ AddToVictory (EnemyTarget enemyId)
        , CreateEffect
          (toCardCode attrs)
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        ]
    _ -> JeremiahPierce <$> runMessage msg attrs
