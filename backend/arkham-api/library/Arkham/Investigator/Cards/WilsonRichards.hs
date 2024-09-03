module Arkham.Investigator.Cards.WilsonRichards (wilsonRichards, WilsonRichards (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Tool))

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype WilsonRichards = WilsonRichards (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

wilsonRichards :: InvestigatorCard WilsonRichards
wilsonRichards =
  investigator (WilsonRichards . (`with` Meta False)) Cards.wilsonRichards
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor WilsonRichards where
  getModifiersFor target (WilsonRichards (With a _)) | isTarget a target = do
    maybeModified a do
      source <- MaybeT getSkillTestSource
      aid <- hoistMaybe source.asset
      liftGuardM $ aid <=~> AssetWithTrait Tool
      pure [AnySkillValue 1]
  getModifiersFor (CardIdTarget cid) (WilsonRichards (With a meta)) = do
    maybeModified a do
      guard $ active meta
      c <- lift (getCard cid)
      guard $ a.id `elem` c.owner
      guard $ c `cardMatch` card_ (#asset <> #tool)
      pure [ReduceCostOf (CardWithId cid) 1]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue WilsonRichards where
  getChaosTokenValue iid ElderSign (WilsonRichards (With attrs _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage WilsonRichards where
  runMessage msg i@(WilsonRichards (With attrs meta)) = runQueueT $ case msg of
    BeginRound -> pure . WilsonRichards $ attrs `with` Meta True
    PaidForCardCost iid pc _ | attrs.id == iid -> do
      if active meta && toCard pc `cardMatch` card_ (#asset <> #tool)
        then pure . WilsonRichards $ attrs `with` Meta False
        else pure i
    ElderSignEffect iid | attrs `is` iid -> do
      playAreaTools <- select $ assetInPlayAreaOf iid <> #tool
      handTools <- select $ inHandOf iid <> #asset <> #tool
      validPlayAreaTools <- flip filterM playAreaTools \aid -> do
        cost <- field AssetCost aid
        pure $ any (\c -> maybe False ((<= cost) . toPrintedCost) c.cost) handTools
      when (notNull validPlayAreaTools) do
        chooseOne iid $ Label "Do not swap" []
          : targetLabels validPlayAreaTools (only . Msg.handleTargetChoice iid attrs)
      pure i
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      cost <- field AssetCost aid
      handTools <- select $ inHandOf iid <> #asset <> #tool
      let validHandTools = filter (\c -> maybe False ((<= cost) . toPrintedCost) c.cost) handTools
      returnToHand iid aid
      chooseOne iid [targetLabel tool [Msg.putCardIntoPlay iid tool] | tool <- validHandTools]
      pure i
    _ -> WilsonRichards . (`with` meta) <$> liftRunMessage msg attrs
