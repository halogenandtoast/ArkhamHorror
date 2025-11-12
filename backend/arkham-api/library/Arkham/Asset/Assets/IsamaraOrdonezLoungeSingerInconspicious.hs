module Arkham.Asset.Assets.IsamaraOrdonezLoungeSingerInconspicious (isamaraOrdonezLoungeSingerInconspicious) where

import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype IsamaraOrdonezLoungeSingerInconspicious = IsamaraOrdonezLoungeSingerInconspicious AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezLoungeSingerInconspicious :: AssetCard IsamaraOrdonezLoungeSingerInconspicious
isamaraOrdonezLoungeSingerInconspicious = asset IsamaraOrdonezLoungeSingerInconspicious Cards.isamaraOrdonezLoungeSingerInconspicious

instance HasAbilities IsamaraOrdonezLoungeSingerInconspicious where
  getAbilities (IsamaraOrdonezLoungeSingerInconspicious a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage IsamaraOrdonezLoungeSingerInconspicious where
  runMessage msg a@(IsamaraOrdonezLoungeSingerInconspicious attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower MaxAlarmLevelCalculation
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      do_ msg
      pure a
    Do (PassedThisSkillTest _ (isAbilitySource attrs 1 -> True)) -> do
      n <- perPlayer 1
      when (attrs.token #clue >= n) do
        remember ConvincedIsamaraToParticipateInTheHeist
      pure a
    _ -> IsamaraOrdonezLoungeSingerInconspicious <$> liftRunMessage msg attrs
