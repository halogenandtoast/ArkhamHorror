module Arkham.Asset.Assets.AndyVanNortwickAmbitiousJournalist (andyVanNortwick) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, controllerGetsMaybe)
import Arkham.History
import Arkham.Matcher

newtype AndyVanNortwickAmbitiousJournalist = AndyVanNortwickAmbitiousJournalist AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

andyVanNortwick :: AssetCard AndyVanNortwickAmbitiousJournalist
andyVanNortwick = ally AndyVanNortwickAmbitiousJournalist Cards.andyVanNortwick (1, 3)

instance HasModifiersFor AndyVanNortwickAmbitiousJournalist where
  getModifiersFor (AndyVanNortwickAmbitiousJournalist a) = do
    controllerGets a [SkillModifier #intellect 1]
    controllerGetsMaybe a \iid -> do
      noClues <- selectNone $ locationWithInvestigator iid <> LocationWithAnyClues
      guard noClues
      n <- lift $ getHistoryField RoundHistory iid HistoryAttacksOfOpportunity
      guard $ n == 0 && getAssetMetaDefault True a
      pure [IgnoreAttacksOfOpportunity]

instance RunMessage AndyVanNortwickAmbitiousJournalist where
  runMessage msg a@(AndyVanNortwickAmbitiousJournalist attrs) = runQueueT $ case msg of
    BeginRound -> pure $ AndyVanNortwickAmbitiousJournalist $ attrs & setMeta True
    EnemyAttack details -> do
      case details.investigator of
        Just iid | Just iid == attrs.controller -> pure $ AndyVanNortwickAmbitiousJournalist $ attrs & setMeta False
        _ -> pure a
    _ -> AndyVanNortwickAmbitiousJournalist <$> liftRunMessage msg attrs
