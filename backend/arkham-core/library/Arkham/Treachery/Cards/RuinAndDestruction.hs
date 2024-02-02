module Arkham.Treachery.Cards.RuinAndDestruction (
  ruinAndDestruction,
  RuinAndDestruction (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance RunMessage RuinAndDestruction where
  runMessage msg t@(RuinAndDestruction attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      targetInvestigators <-
        selectList
          $ InvestigatorAt
          $ LocationWithEnemy
          $ EnemyWithTitle
            broodTitle
      pushAll
        $ [ beginSkillTest iid' source (InvestigatorTarget iid') SkillAgility 3
          | iid' <- targetInvestigators
          ]
        <> [gainSurge attrs | null targetInvestigators]
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source ->
          t
            <$ push (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> RuinAndDestruction <$> runMessage msg attrs
