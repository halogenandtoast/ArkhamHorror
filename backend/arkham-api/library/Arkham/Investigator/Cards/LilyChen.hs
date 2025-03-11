module Arkham.Investigator.Cards.LilyChen (lilyChen, lilyChenEffect) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Broken))

newtype Metadata = Metadata {flipDiscipline :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype LilyChen = LilyChen (InvestigatorAttrs `With` Metadata)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator LilyChen where
  investigatorFromAttrs = LilyChen . (`with` Metadata False)

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
  runMessage msg i@(LilyChen (With attrs meta)) = runQueueT $ case msg of
    BeginGame -> do
      createCardEffect Cards.lilyChen Nothing attrs attrs
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      pure $ LilyChen $ attrs `with` Metadata True
    SkillTestEnds _ iid _ | attrs.id == iid && flipDiscipline meta -> do
      brokenDisciplines <- select $ AssetWithTrait Broken <> AssetWithTitle "Discipline"
      unless (null brokenDisciplines) do
        chooseOne attrs.id
          $ targetLabels brokenDisciplines (only . Flip attrs.id #elderSign . toTarget)
      pure $ LilyChen $ attrs `with` Metadata False
    _ -> LilyChen . (`with` meta) <$> liftRunMessage msg attrs

newtype LilyChenEffect = LilyChenEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lilyChenEffect :: EffectArgs -> LilyChenEffect
lilyChenEffect = cardEffect LilyChenEffect Cards.lilyChen

instance HasModifiersFor LilyChenEffect where
  getModifiersFor (LilyChenEffect a) = for_ a.target.investigator \iid -> do
    modifiedWhen_ a (hasEffectKey "balanced" a) iid [MetaModifier "balanced"]
    modifiedWhen_ a (hasEffectKey "quiescent" a) iid [MetaModifier "quiescent"]
    modifiedWhen_ a (hasEffectKey "aligned" a) iid [MetaModifier "aligned"]
    modifiedWhen_ a (hasEffectKey "prescient" a) iid [MetaModifier "prescient"]

instance RunMessage LilyChenEffect where
  runMessage msg e@(LilyChenEffect attrs) = runQueueT $ case msg of
    Do BeginRound -> case attrs.target.investigator of
      Nothing -> pure e
      Just iid -> do
        quiescent <- fieldMap InvestigatorHand ((<= 2) . length) iid
        balanced <- selectNone $ enemyAtLocationWith iid
        pure
          . LilyChenEffect
          $ attrs
          & setEffectKey "aligned"
          & setEffectKey "prescient"
          & (if quiescent then setEffectKey else unsetEffectKey) "quiescent"
          & (if balanced then setEffectKey else unsetEffectKey) "balanced"
    StartSkillTest iid | isTarget iid attrs.target -> do
      pure $ LilyChenEffect $ attrs & unsetEffectKey "prescient"
    InvestigatorDamage iid _source damage horror | isTarget iid attrs.target -> do
      pure
        $ if damage > 0 || horror > 0
          then LilyChenEffect $ attrs & unsetEffectKey "aligned"
          else e
    _ -> case attrs.target.investigator of
      Nothing -> pure e
      Just iid -> do
        quiescent <- fieldMap InvestigatorHand ((< 2) . length) iid
        balanced <- selectNone $ enemyAtLocationWith iid
        let setQuiescent = (if quiescent then setEffectKey else unsetEffectKey) "quiescent"
        let setBalanced = (if balanced then setEffectKey else unsetEffectKey) "balanced"
        LilyChenEffect <$> liftRunMessage msg (attrs & setQuiescent & setBalanced)
