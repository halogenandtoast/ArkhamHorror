{- | The Butler, unmasked. He blanks exhausted assets, then opens each
investigation phase by asking the table to exhaust everything or pay a sigil.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.ThomasRogersColdAndDistant (
  thomasRogersColdAndDistant,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ThomasRogersColdAndDistant = ThomasRogersColdAndDistant EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasRogersColdAndDistant :: EnemyCard ThomasRogersColdAndDistant
thomasRogersColdAndDistant = enemy ThomasRogersColdAndDistant Cards.thomasRogersColdAndDistant

instance HasModifiersFor ThomasRogersColdAndDistant where
  -- "Treat the printed text box of each exhausted asset as if it were blank
  -- (except for Traits)."
  getModifiersFor (ThomasRogersColdAndDistant a) = modifySelect a AssetExhausted [Blank]

instance HasAbilities ThomasRogersColdAndDistant where
  getAbilities (ThomasRogersColdAndDistant a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "thomasRogersColdAndDistant.cold"
      $ mkAbility a 1
      $ forced
      $ PhaseBegins #when #investigation

instance RunMessage ThomasRogersColdAndDistant where
  runMessage msg e@(ThomasRogersColdAndDistant attrs) = runQueueT $ case msg of
    -- "In player order, each investigator must either..." Each seat is handed
    -- its own message so its choices are built after the previous seat paid.
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator (`forInvestigator` msg)
      pure e
    ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 1) -> do
      assets <- select $ assetControlledBy iid <> AssetReady
      candidates <- unchosenSigils
      chooseOneM iid $ scenarioI18n $ scope "thomasRogersColdAndDistant" do
        labeled "exhaustAssets" $ for_ assets exhaustThis
        when (notNull candidates)
          $ labeled "removeOffering"
          $ chooseOrRunOneM iid
          $ targets candidates (removeOfferingFromSigil (attrs.ability 1))
      pure e
    _ -> ThomasRogersColdAndDistant <$> liftRunMessage msg attrs
