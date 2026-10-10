module Arkham.Homebrew.AgesUnwound.Locations.SecretsLongForgotten_193 (
  secretsLongForgotten_193,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SecretsLongForgotten_193 = SecretsLongForgotten_193 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsLongForgotten_193 :: LocationCard SecretsLongForgotten_193
secretsLongForgotten_193 =
  symbolLabel
    $ locationWith SecretsLongForgotten_193 Cards.secretsLongForgotten_193 6 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "__Forced__ - After you successfully investigate Secrets Long Forgotten:
Either draw a card, or take 1 horror and search your collection for a level 0
skill card, adding that card to your hand."
-}
instance HasAbilities SecretsLongForgotten_193 where
  getAbilities (SecretsLongForgotten_193 a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ SuccessfulInvestigation #after You (be a)

instance RunMessage SecretsLongForgotten_193 where
  runMessage msg l@(SecretsLongForgotten_193 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ timeRunsOutI18n $ scope "secretsLongForgotten" do
        labeled "drawACard" $ drawCards iid (attrs.ability 1) 1
        labeled "searchForASkill" do
          assignHorror iid (attrs.ability 1) 1
          chooseCollectionCard iid (attrs.ability 1) levelZeroSkill
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardCodeTarget code) -> do
      for_ (lookupCardDef code) \def -> do
        card <- genCard def
        addToHand iid (only card)
      pure l
    _ -> SecretsLongForgotten_193 <$> liftRunMessage msg attrs
