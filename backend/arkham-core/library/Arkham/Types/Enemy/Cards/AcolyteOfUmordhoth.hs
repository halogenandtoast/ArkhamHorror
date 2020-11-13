{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.AcolyteOfUmordhoth where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype AcolyteOfUmordhoth = AcolyteOfUmordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

acolyteOfUmordhoth :: EnemyId -> AcolyteOfUmordhoth
acolyteOfUmordhoth uuid =
  AcolyteOfUmordhoth
    $ baseAttrs uuid "50039"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 3)
    . (health .~ Static 3)
    . (evade .~ 2)
    . (prey .~ FewestCards)

instance (HasCount CardCount InvestigatorId env) => HasModifiersFor env AcolyteOfUmordhoth where
  getModifiersFor _ (EnemyTarget eid) (AcolyteOfUmordhoth Attrs {..})
    | eid == enemyId = do
      anyWithoutCards <- or <$> for
        (setToList enemyEngagedInvestigators)
        (\iid -> asks $ (== 0) . unCardCount . getCount iid)
      pure [ CannotBeEvaded | anyWithoutCards ]
  getModifiersFor _ _ _ = pure []

instance HasModifiers env AcolyteOfUmordhoth where
  getModifiers _ (AcolyteOfUmordhoth Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env AcolyteOfUmordhoth where
  getActions i window (AcolyteOfUmordhoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env AcolyteOfUmordhoth where
  runMessage msg (AcolyteOfUmordhoth attrs) =
    AcolyteOfUmordhoth <$> runMessage msg attrs
