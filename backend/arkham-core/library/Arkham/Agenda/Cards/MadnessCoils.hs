module Arkham.Agenda.Cards.MadnessCoils
  ( MadnessCoils(..)
  , madnessCoils
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype Metadata = Metadata { chosenSkills :: HashSet SkillType }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype MadnessCoils = MadnessCoils (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessCoils :: AgendaCard MadnessCoils
madnessCoils = agenda
  (1, A)
  (MadnessCoils . (`with` Metadata mempty))
  Cards.madnessCoils
  (Static 7)

instance HasAbilities MadnessCoils where
  getAbilities (MadnessCoils (a `With` _)) | onSide A a=
    [ restrictedAbility
          a
          1
          (EnemyCriteria
          $ EnemyExists
              (EnemyWithTitle "Hastur"
              <> EnemyWithDamage (AtLeast $ PerPlayer 3)
              )
          )
        $ Objective
        $ ForcedAbility AnyWindow
    ]
  getAbilities _ = []

instance RunMessage MadnessCoils where
  runMessage msg a@(MadnessCoils (attrs `With` metadata)) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      let
        skills = setFromList [SkillWillpower, SkillIntellect]
          `difference` chosenSkills metadata
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      push
        (chooseOne leadInvestigatorId
        $ map
            (\sk -> Label
              ("Any investigator tests " <> tshow sk)
              [ chooseOrRunOne
                  leadInvestigatorId
                  [ targetLabel
                      iid
                      [ BeginSkillTest
                          iid
                          (toSource attrs)
                          (toTarget attrs)
                          Nothing
                          sk
                          4
                      ]
                  | iid <- investigatorIds
                  ]
              ]
            )
            (setToList skills)
        <> [ Label
             "This can't be real. This can't be real. This can't be real. Each investigator takes 2 horror. Advance to agenda 2a."
             ([ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              | iid <- investigatorIds
              ]
             <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
             )
           , Label
             "The investigators faint and awaken some time later. Advance to agenda 2a and place 1 doom on it."
             [ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
             , PlaceDoomOnAgenda
             ]
           ]
        )
      pure a
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} st _
      | isSource attrs source -> do
        pushAfter (== SkillTestApplyResultsAfter) $ AdvanceAgenda (toId attrs)
        pure $ MadnessCoils $ attrs `with` Metadata
          (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        pushAfter (== SkillTestApplyResultsAfter)
          $ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        pure a
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> MadnessCoils . (`with` metadata) <$> runMessage msg attrs
