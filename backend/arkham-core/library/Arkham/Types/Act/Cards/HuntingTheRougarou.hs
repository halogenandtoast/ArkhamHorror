{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.HuntingTheRougarou where

import Arkham.Import

import Arkham.Types.Act.Attrs
import qualified Arkham.Types.Act.Attrs as Act
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype HuntingTheRougarou = HuntingTheRougarou Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingTheRougarou :: HuntingTheRougarou
huntingTheRougarou =
  HuntingTheRougarou $ baseAttrs "81006" "Huntin the Rougarou" "Act 2a"

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (FastAbility FastPlayerWindow))
  { abilityLimit = PerPhase
  }

instance ActionRunner env investigator => HasActions env investigator HuntingTheRougarou where
  getActions i FastPlayerWindow (HuntingTheRougarou a) = do
    baseActions <- getActions i FastPlayerWindow a
    unused <- getIsUnused i (ability a)
    mrougarou <- asks (fmap unStoryEnemyId <$> getId (CardCode "81028"))
    engagedWithTheRougarou <- maybe
      (pure False)
      (asks . (member (getId @InvestigatorId () i) .) . getSet)
      mrougarou
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) (ability a)
         | unused && engagedWithTheRougarou
         ]
  getActions i window (HuntingTheRougarou x) = getActions i window x

instance ActRunner env => RunMessage env HuntingTheRougarou where
  runMessage msg a@(HuntingTheRougarou attrs@Attrs {..}) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source ->
      runMessage (AdvanceAct actId) a
    AdvanceAct aid | aid == actId && actSequence == "Act 2a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      rougarou <- asks $ unStoryEnemyId . fromJustNote "must be" . getId
        (CardCode "81028")

      requiredClueCount <- getPlayerCountValue (PerPlayer 4)
      learnedMoreAboutTheCurse <- (>= requiredClueCount)
        <$> getSpendableClueCount investigatorIds

      requiredDamage <- getPlayerCountValue (PerPlayer 1)
      protectedOurselves <-
        asks $ (>= requiredDamage) . unDamageCount . getCount rougarou

      assetIds <- asks $ setToList . getSet @AssetId rougarou
      keptItContained <- or
        <$> for assetIds (asks . ((Trap `member`) .) . getSet)

      scenarioLogs <- asks $ getSet ()
      let
        calmedItDown = any
          (`member` scenarioLogs)
          [FoundAStrangeDoll, FoundAnAncientBindingStone]

      if and
          [ learnedMoreAboutTheCurse
          , keptItContained
          , protectedOurselves
          , calmedItDown
          ]
        then unshiftMessage
          (chooseOne leadInvestigatorId [Label "Resolution 3" [Resolution 3]])
        else unshiftMessage
          (chooseOne
            leadInvestigatorId
            [Label "Flip back to a side" [RevertAct actId]]
          )
      pure
        $ HuntingTheRougarou
        $ attrs
        & (Act.sequence .~ "Act 2b")
        & (flipped .~ True)
    RevertAct aid | aid == actId && actSequence == "Act 2b" ->
      pure
        $ HuntingTheRougarou
        $ attrs
        & (Act.sequence .~ "Act 2a")
        & (flipped .~ False)
    EnemyMove eid lid _ -> do
      isRougarou <- asks $ (== CardCode "81028") . getId eid
      a <$ when isRougarou (unshiftMessage (PlaceClues (LocationTarget lid) 1))
    EnemyDefeated _ _ "81028" _ -> a <$ unshiftMessage (Resolution 2)
    _ -> HuntingTheRougarou <$> runMessage msg attrs
