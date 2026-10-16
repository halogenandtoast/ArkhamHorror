{- | Antediluvian Sigils, four copies of it.

Act 1b hangs each surviving copy on a /location/, so no one controls it and
both printed abilities belong to whoever is standing there -- hence
'OnSameLocation' rather than @ControlsThis@. The offerings on the copies are the
dial every culprit's text reads.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.AntediluvianSigils (antediluvianSigils) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers hiding (antediluvianSigils)
import Arkham.I18n
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token

newtype AntediluvianSigils = AntediluvianSigils AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

antediluvianSigils :: AssetCard AntediluvianSigils
antediluvianSigils = asset AntediluvianSigils Cards.antediluvianSigils

instance HasAbilities AntediluvianSigils where
  getAbilities (AntediluvianSigils a) =
    scenarioI18n
      [ -- "{action}: Test {willpower} (3). If you succeed, add 1 or remove 1
        -- offering from Antediluvian Sigils. This action does not provoke
        -- attacks of opportunity."
        withI18nTooltip "antediluvianSigils.test"
          $ skillTestAbility
          $ noAOO
          $ restricted a 1 OnSameLocation actionAbility
      , -- "{fast} During your turn, spend 1 clue: Add or remove 1 offering from
        -- Antediluvian Sigils."
        withI18nTooltip "antediluvianSigils.spend"
          $ restricted a 2 (OnSameLocation <> DuringTurn You)
          $ FastAbility
          $ clueCost 1
      ]

-- | "add 1 or remove 1 offering from Antediluvian Sigils"
adjustOfferings
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> AssetAttrs -> m ()
adjustOfferings source iid attrs = do
  offerings <- offeringsOn attrs.id
  chooseOneM iid $ scenarioI18n $ scope "antediluvianSigils" do
    labeled "addOffering" $ placeTokens source attrs Token.Offering 1
    when (offerings > 0) do
      labeled "removeOffering" $ removeTokens source attrs Token.Offering 1

instance RunMessage AntediluvianSigils where
  runMessage msg a@(AntediluvianSigils attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      adjustOfferings (attrs.ability 1) iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      adjustOfferings (attrs.ability 2) iid attrs
      pure a
    _ -> AntediluvianSigils <$> liftRunMessage msg attrs
