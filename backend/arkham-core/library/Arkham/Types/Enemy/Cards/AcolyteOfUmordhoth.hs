module Arkham.Types.Enemy.Cards.AcolyteOfUmordhoth where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype AcolyteOfUmordhoth = AcolyteOfUmordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

acolyteOfUmordhoth :: EnemyId -> AcolyteOfUmordhoth
acolyteOfUmordhoth uuid =
  AcolyteOfUmordhoth
    $ baseAttrs uuid "50039"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 2)
    . (preyL .~ FewestCards)

instance HasCount CardCount env InvestigatorId => HasModifiersFor env AcolyteOfUmordhoth where
  getModifiersFor _ (EnemyTarget eid) (AcolyteOfUmordhoth a@Attrs {..})
    | eid == enemyId = do
      anyWithoutCards <- or <$> for
        (setToList enemyEngagedInvestigators)
        (\iid -> (== 0) . unCardCount <$> getCount iid)
      pure $ toModifiers a [ CannotBeEvaded | anyWithoutCards ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AcolyteOfUmordhoth where
  getActions i window (AcolyteOfUmordhoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env AcolyteOfUmordhoth where
  runMessage msg (AcolyteOfUmordhoth attrs) =
    AcolyteOfUmordhoth <$> runMessage msg attrs
