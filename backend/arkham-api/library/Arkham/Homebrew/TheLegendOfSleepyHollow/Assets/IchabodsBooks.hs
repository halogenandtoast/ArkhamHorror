module Arkham.Homebrew.TheLegendOfSleepyHollow.Assets.IchabodsBooks (ichabodsBooks) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Message.Lifted.Log (remember)
import Arkham.Projection

newtype IchabodsBooks = IchabodsBooks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichabodsBooks :: AssetCard IchabodsBooks
ichabodsBooks = asset IchabodsBooks Cards.ichabodsBooks

{- | "Any investigator at Derelict Schoolhouse may activate this ability." The
books never leave the schoolhouse, so being there is the whole restriction.
-}
instance HasAbilities IchabodsBooks where
  getAbilities (IchabodsBooks a) =
    [ scenarioI18n
        $ withI18nTooltip "ichabodsBooks.divination"
        $ skillTestAbility
        $ restricted a 1 OnSameLocation actionAbility
    ]

instance RunMessage IchabodsBooks where
  runMessage msg a@(IchabodsBooks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inHand <- fieldMap InvestigatorHand length iid
      scenarioI18n
        $ scope "ichabodsBooks"
        $ countVar inHand
        $ chooseAmount iid "cardsToDiscard" "$cards" 0 inHand attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$cards" -> discarded) (isTarget attrs -> True) -> do
      repeated discarded $ chooseAndDiscardCard iid (attrs.ability 1)
      sid <- getRandom
      when (discarded > 0)
        $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower (2 * discarded))
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 6)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember performedADivinationSpell
      pure a
    _ -> IchabodsBooks <$> liftRunMessage msg attrs
