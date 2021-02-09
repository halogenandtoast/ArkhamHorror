module Arkham.Types.Enemy.Cards.OBannionsThug where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype OBannionsThug = OBannionsThug EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oBannionsThug :: EnemyId -> OBannionsThug
oBannionsThug uuid =
  OBannionsThug
    $ baseAttrs uuid "02097"
    $ (healthDamageL .~ 2)
    . (fightL .~ 4)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)

instance HasModifiersFor env OBannionsThug where
  getModifiersFor _ (InvestigatorTarget iid) (OBannionsThug a@EnemyAttrs {..})
    | iid `elem` enemyEngagedInvestigators = pure
    $ toModifiers a [CannotGainResources]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OBannionsThug where
  getActions i window (OBannionsThug attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env OBannionsThug where
  runMessage msg (OBannionsThug attrs) = OBannionsThug <$> runMessage msg attrs
