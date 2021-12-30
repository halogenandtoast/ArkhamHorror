module Arkham.Treachery.Cards.RuinAndDestruction
  ( ruinAndDestruction
  , RuinAndDestruction(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Id
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance TreacheryRunner env => RunMessage env RuinAndDestruction where
  runMessage msg t@(RuinAndDestruction attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothoth <- getSetList @EnemyId (CardCode "02255")
      broodOfYogSothothLocations <- traverse
        (getId @LocationId)
        broodOfYogSothoth
      targetInvestigators <-
        concat
          <$> traverse (getSetList @InvestigatorId) broodOfYogSothothLocations
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
