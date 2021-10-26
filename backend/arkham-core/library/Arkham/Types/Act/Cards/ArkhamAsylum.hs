module Arkham.Types.Act.Cards.ArkhamAsylum
  ( ArkhamAsylum(..)
  , arkhamAsylum
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
-- import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Metadata = Metadata { chosenSkills :: HashSet SkillType }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype ArkhamAsylum = ArkhamAsylum (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum = act
  (1, A)
  (ArkhamAsylum . (`with` Metadata mempty))
  Cards.arkhamAsylum
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasAbilities ArkhamAsylum where
  getAbilities (ArkhamAsylum (attrs `With` _)) = getAbilities attrs

instance ActRunner env => RunMessage env ArkhamAsylum where
  runMessage msg a@(ArkhamAsylum (attrs `With` metadata)) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      let
        skills = setFromList [SkillCombat, SkillAgility, SkillIntellect]
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
               "You knock her over and grab the keys"
               [ Remember YouTookTheKeysByForce
               , AdvanceActDeck (actDeckId attrs) (toSource attrs)
               ]
           ]
        )
      pure a
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} st _
      | isSource attrs source -> do
        replaceMessageMatching
          (== SkillTestApplyResultsAfter)
          (\m -> [m, AdvanceAct (toId attrs) source])
        pure $ ArkhamAsylum $ attrs `with` Metadata
          (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        replaceMessageMatching
          (== SkillTestApplyResultsAfter)
          (\m -> [m, AdvanceActDeck (actDeckId attrs) (toSource attrs)])
        pure a
    _ -> ArkhamAsylum . (`with` metadata) <$> runMessage msg attrs
