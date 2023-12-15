module Arkham.Act.Cards.ArkhamAsylum (ArkhamAsylum (..), arkhamAsylum) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: Set SkillType}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype ArkhamAsylum = ArkhamAsylum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum =
  actWith (1, A) ArkhamAsylum Cards.arkhamAsylum (groupClueCost $ PerPlayer 3)
    $ (metaL .~ (toJSON $ Metadata mempty))

instance HasAbilities ArkhamAsylum where
  getAbilities (ArkhamAsylum attrs) = getAbilities attrs

instance RunMessage ArkhamAsylum where
  runMessage msg a@(ArkhamAsylum attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      let metadata = toResult (actMeta attrs)
      let skills = setFromList [#combat, #agility, #intellect] `difference` chosenSkills metadata
      lead <- getLeadPlayer
      investigators <- getInvestigatorIds
      push
        $ chooseOne lead
        $ [ Label
            ("Any investigator tests " <> tshow sk)
            [ chooseOrRunOne lead [targetLabel iid [beginSkillTest iid attrs attrs sk 4] | iid <- investigators]
            ]
          | sk <- setToList skills
          ]
        <> [ Label
              "You knock her over and grab the keys"
              [Remember YouTookTheKeysByForce, advanceActDeck attrs]
           ]

      pure a
    FailedSkillTest _ _ source Initiator {} (SkillSkillTest st) _ | isSource attrs source -> do
      insertAfterMatching
        [AdvanceAct (toId attrs) source AdvancedWithClues]
        (== SkillTestApplyResultsAfter)
      let metadata = toResult (actMeta attrs)
      pure $ ArkhamAsylum $ attrs & metaL .~ toJSON (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source Initiator {} _ _ | isSource attrs source -> do
      insertAfterMatching [advanceActDeck attrs] (== SkillTestApplyResultsAfter)
      pure a
    _ -> ArkhamAsylum <$> runMessage msg attrs
