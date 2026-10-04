module Arkham.Homebrew.AgainstTheWendigo.Assets.CharlieFoxtail (charlieFoxtail) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (handOverGuideAbility)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wendigo, pattern Wild)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CharlieFoxtail = CharlieFoxtail AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlieFoxtail :: AssetCard CharlieFoxtail
charlieFoxtail = allyWith CharlieFoxtail Cards.charlieFoxtail (4, 1) noSlots

instance HasAbilities CharlieFoxtail where
  getAbilities (CharlieFoxtail a) =
    [ -- "{reaction} When you resolve a skill test in a Wild location: You get +1 [intellect]."
      controlled a 1 (exists $ YourLocation <> LocationWithTrait Wild)
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue AnySkillTest) mempty
    , -- "Forced - If an investigator in your location reveals or puts into play
      -- a Wendigo card: Test [willpower] (2) before resolving the Revelation."
      controlled a 2 (exists $ InvestigatorAt YourLocation)
        $ forced
        $ DrawCard #when (InvestigatorAt YourLocation) (basic $ CardWithTrait Wendigo) AnyDeck
    , handOverGuideAbility a 3
    ]

instance RunMessage CharlieFoxtail where
  runMessage msg a@(CharlieFoxtail attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure a
    -- "If you fail, put Sarcee Guide out of play."
    FailedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      others <- select $ InvestigatorAt (locationWithInvestigator iid) <> NotInvestigator (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others \other -> takeControlOfAsset other attrs.id
      pure a
    _ -> CharlieFoxtail <$> liftRunMessage msg attrs
