module Arkham.Investigator.Cards.LilyChen (lilyChen, LilyChen (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Broken))
import Data.Aeson.KeyMap qualified as KM

newtype Metadata = Metadata {flipDiscipline :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype LilyChen = LilyChen (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

lilyChen :: InvestigatorCard LilyChen
lilyChen =
  startsWith
    [ Assets.disciplineAlignmentOfSpirit
    , Assets.disciplineQuiescenceOfThought
    , Assets.disciplinePrescienceOfFate
    , Assets.disciplineBalanceOfBody
    ]
    $ investigator (LilyChen . (`with` Metadata False)) Cards.lilyChen
    $ Stats {health = 7, sanity = 7, willpower = 3, intellect = 2, combat = 4, agility = 3}

instance HasChaosTokenValue LilyChen where
  getChaosTokenValue iid ElderSign (LilyChen (With attrs _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LilyChen where
  runMessage msg (LilyChen (With attrs meta)) = runQueueT $ case msg of
    Do BeginRound -> do
      attrs' <- liftRunMessage msg attrs
      quiescent <- fieldMap InvestigatorHand ((<= 2) . length) (toId attrs)
      balanced <- selectNone $ enemyAtLocationWith attrs.id
      pure
        . LilyChen
        . (`with` meta)
        $ attrs'
        & setMeta
          (object ["aligned" .= True, "balanced" .= balanced, "prescient" .= True, "quiescent" .= quiescent])
    ElderSignEffect iid | iid == toId attrs -> do
      pure $ LilyChen $ attrs `with` Metadata True
    SkillTestEnds iid _ | attrs.id == iid && flipDiscipline meta -> do
      brokenDisciplines <- select $ AssetWithTrait Broken <> AssetWithTitle "Discipline"
      unless (null brokenDisciplines) do
        chooseOne attrs.id
          $ targetLabels brokenDisciplines (only . Flip attrs.id #elderSign . toTarget)
      pure $ LilyChen $ attrs `with` Metadata False
    StartSkillTest iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      let
        meta' = case attrs.meta of
          Object o -> Object $ KM.insert "prescient" (Bool False) o
          _ -> object ["balanced" .= True, "quiescent" .= True, "prescient" .= False, "aligned" .= True]
      pure . LilyChen . (`with` meta) $ attrs' & setMeta meta'
    AssignDamage target | isTarget attrs target -> do
      let tookDamage = investigatorAssignedHealthDamage attrs > 0
      if tookDamage
        then do
          let
            meta' =
              case attrs.meta of
                Object o -> Object $ KM.insert "aligned" (Bool False) o
                _ -> object ["balanced" .= True, "quiescent" .= True, "prescient" .= True, "aligned" .= False]
          attrs' <- lift $ runMessage msg attrs
          pure . LilyChen . (`with` meta) $ attrs' & setMeta meta'
        else LilyChen . (`with` meta) <$> liftRunMessage msg attrs
    _ ->
      do
        quiescent <- fieldMap InvestigatorHand ((< 2) . length) attrs.id
        balanced <- selectNone (enemyAtLocationWith attrs.id)

        let
          setQuiescent = if quiescent then id else KM.insert "quiescent" (Bool False)
          setBalanced = if balanced then id else KM.insert "balanced" (Bool False)
          meta' = case attrs.meta of
            Object o -> Object $ setQuiescent $ setBalanced o
            _ ->
              object ["balanced" .= balanced, "quiescent" .= quiescent, "prescient" .= True, "aligned" .= True]

        LilyChen . (`with` meta) . setMeta meta' <$> liftRunMessage msg attrs
