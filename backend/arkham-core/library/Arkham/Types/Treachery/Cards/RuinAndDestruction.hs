module Arkham.Types.Treachery.Cards.RuinAndDestruction
  ( ruinAndDestruction
  , RuinAndDestruction(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance HasModifiersFor env RuinAndDestruction where
  getModifiersFor = noModifiersFor

instance HasActions env RuinAndDestruction where
  getActions i window (RuinAndDestruction attrs) = getActions i window attrs

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
      t <$ unshiftMessages
        ([ BeginSkillTest
             iid'
             source
             (InvestigatorTarget iid')
             Nothing
             SkillAgility
             3
         | iid' <- targetInvestigators
         ]
        <> [Discard (toTarget attrs)]
        <> [ Surge iid source | null targetInvestigators ]
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> t <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> RuinAndDestruction <$> runMessage msg attrs
