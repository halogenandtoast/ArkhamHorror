module Arkham.Act.Cards.ArkhamAsylum
  ( ArkhamAsylum(..)
  , arkhamAsylum
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Metadata = Metadata { chosenSkills :: HashSet SkillType }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype ArkhamAsylum = ArkhamAsylum (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum = act
  (1, A)
  (ArkhamAsylum . (`with` Metadata mempty))
  Cards.arkhamAsylum
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasAbilities ArkhamAsylum where
  getAbilities (ArkhamAsylum (attrs `With` _)) = getAbilities attrs

instance RunMessage ArkhamAsylum where
  runMessage msg a@(ArkhamAsylum (attrs `With` metadata)) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      let
        skills = setFromList [SkillCombat, SkillAgility, SkillIntellect]
          `difference` chosenSkills metadata
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      push
        $ chooseOne leadInvestigatorId
        $ map
            (\sk -> Label
              ("Any investigator tests " <> tshow sk)
              [ chooseOrRunOne
                  leadInvestigatorId
                  [ targetLabel
                      iid
                      [ beginSkillTest
                          iid
                          (toSource attrs)
                          (toTarget attrs)
                          sk
                          4
                      ]
                  | iid <- investigatorIds
                  ]
              ]
            )
            (setToList skills)
        <> [ Label
               "You knock her over and grab the keys"
               [ Remember YouTookTheKeysByForce
               , AdvanceActDeck (actDeckId attrs) (toSource attrs)
               ]
           ]

      pure a
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} (SkillSkillTest st) _
      | isSource attrs source -> do
        insertAfterMatching
          [AdvanceAct (toId attrs) source AdvancedWithClues]
          (== SkillTestApplyResultsAfter)
        pure $ ArkhamAsylum $ attrs `with` Metadata
          (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        insertAfterMatching
          [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
          (== SkillTestApplyResultsAfter)
        pure a
    _ -> ArkhamAsylum . (`with` metadata) <$> runMessage msg attrs
