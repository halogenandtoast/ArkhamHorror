module Arkham.Types.Enemy.Cards.JeremiahPierce
  ( JeremiahPierce(..)
  , jeremiahPierce
  ) where


import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyId -> JeremiahPierce
jeremiahPierce uuid =
  JeremiahPierce
    $ baseAttrs uuid "50044"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 3)
    . (evadeL .~ 4)
    . (uniqueL .~ True)

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
          enemyCardCode
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        ]
    _ -> JeremiahPierce <$> runMessage msg attrs
