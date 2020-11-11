{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.HuntingNightgaunt where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype HuntingNightgaunt = HuntingNightgaunt Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingNightgaunt :: EnemyId -> HuntingNightgaunt
huntingNightgaunt uuid =
  HuntingNightgaunt
    $ baseAttrs uuid "01172"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 4)
    . (evade .~ 1)

instance HasModifiersFor env HuntingNightgaunt where
  getModifiersFor = noModifiersFor

instance HasModifiers env HuntingNightgaunt where
  getModifiers _ (HuntingNightgaunt Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env HuntingNightgaunt where
  getActions i window (HuntingNightgaunt attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env HuntingNightgaunt where
  runMessage msg (HuntingNightgaunt attrs@Attrs {..}) = case msg of
    TryEvadeEnemy iid eid source skillType onSuccess onFailure skillTestModifiers tokenResponses
      | eid == enemyId
      -> HuntingNightgaunt
        <$> runMessage
              (TryEvadeEnemy
                iid
                eid
                source
                skillType
                onSuccess
                onFailure
                (DoubleNegativeModifiersOnTokens : skillTestModifiers)
                tokenResponses
              )
              attrs
    _ -> HuntingNightgaunt <$> runMessage msg attrs
