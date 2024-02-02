module Arkham.Agenda.Cards.EndlessCaverns (
  EndlessCaverns (..),
  endlessCaverns,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.GameValue
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.SkillType

newtype EndlessCaverns = EndlessCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

endlessCaverns :: AgendaCard EndlessCaverns
endlessCaverns = agenda (3, A) EndlessCaverns Cards.endlessCaverns (Static 4)

instance RunMessage EndlessCaverns where
  runMessage msg a@(EndlessCaverns attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      lead <- getLeadPlayer
      leadId <- getLeadInvestigatorId
      iids <- getInvestigatorIds
      pushAll
        $ enemyMsgs
        <> [ questionLabel "Choose a scout" lead
              $ ChooseOne
                [ targetLabel iid [HandleTargetChoice leadId (toSource attrs) (InvestigatorTarget iid)]
                | iid <- iids
                ]
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      player <- getPlayer iid
      hasRope <- getHasSupply iid Rope
      unless hasRope
        $ push
        $ chooseOne
          player
          [ SkillLabel
              SkillCombat
              [ beginSkillTest
                  iid
                  (toSource attrs)
                  (toTarget attrs)
                  SkillCombat
                  5
              ]
          , SkillLabel
              SkillAgility
              [ beginSkillTest
                  iid
                  (toSource attrs)
                  (toTarget attrs)
                  SkillAgility
                  5
              ]
          ]
      pure a
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push $ SufferTrauma iid 1 0
          pure a
    _ -> EndlessCaverns <$> runMessage msg attrs
