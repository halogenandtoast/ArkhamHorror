module Arkham.Treachery.Cards.RuinAndDestruction
  ( ruinAndDestruction
  , RuinAndDestruction(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance TreacheryRunner env => RunMessage RuinAndDestruction where
  runMessage msg t@(RuinAndDestruction attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      targetInvestigators <-
        selectList $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTitle
          "Brood of Yog-Sothoth"
      t <$ pushAll
        ([ BeginSkillTest
             iid'
             source
             (InvestigatorTarget iid')
             Nothing
             SkillAgility
             3
         | iid' <- targetInvestigators
         ]
        <> [ Surge iid source | null targetInvestigators ]
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t
      <$ push (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> RuinAndDestruction <$> runMessage msg attrs
