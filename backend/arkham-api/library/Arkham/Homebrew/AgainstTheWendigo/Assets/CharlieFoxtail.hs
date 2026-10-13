module Arkham.Homebrew.AgainstTheWendigo.Assets.CharlieFoxtail (charlieFoxtail) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (
  duringUnresolvedTest,
  handOverGuideAbility,
  navigateOrRiverTest,
 )
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wendigo, pattern Wild)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CharlieFoxtail = CharlieFoxtail AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlieFoxtail :: AssetCard CharlieFoxtail
charlieFoxtail = allyWith CharlieFoxtail Cards.charlieFoxtail (4, 1) noSlots

instance HasModifiersFor CharlieFoxtail where
  -- "Forced - If Charlie Foxtail is defeated or discarded, put Charlie Foxtail out of play."
  getModifiersFor (CharlieFoxtail a) = modifySelf a [RemoveFromGameInsteadOfDiscard]

instance HasAbilities CharlieFoxtail where
  getAbilities (CharlieFoxtail a) =
    [ -- "{reaction} When you resolve a skill test in a Wild location: You get +2 [intellect]."
      controlled a 1 (exists (YourLocation <> LocationWithTrait Wild) <> duringUnresolvedTest)
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue AnySkillTest) mempty
    , -- "{reaction} When you resolve a skill test at stage 4 of a Navigate action, or
      -- triggered by a River treachery card: You get +2 [combat] or +2 [agility]."
      controlled a 2 duringUnresolvedTest
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue navigateOrRiverTest) mempty
    , {- "Forced - If an investigator in your location reveals or puts into play a
      Wendigo card: Test [willpower] (3) (before resolving the Revelation effect)."
      A treachery appears by being revealed; an enemy by entering play. Splitting
      them that way tests a drawn Wendigo enemy once rather than twice. -}
      controlled a 3 (exists $ InvestigatorAt YourLocation)
        $ forced
        $ oneOf
          [ DrawCard #when (InvestigatorAt YourLocation) (basic $ CardWithTrait Wendigo <> #treachery) AnyDeck
          , EnemyEntersPlay #when (EnemyWithTrait Wendigo <> EnemyAt YourLocation)
          ]
    , handOverGuideAbility a 4
    ]

instance RunMessage CharlieFoxtail where
  runMessage msg a@(CharlieFoxtail attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> chooseOneM iid $ withI18n $ countVar 2 do
        skillVar #combat
          $ labeled "getPlus"
          $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
        skillVar #agility
          $ labeled "getPlus"
          $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 3) iid #willpower (Fixed 3)
      pure a
    -- "If you fail, put Charlie Foxtail out of play."
    FailedThisSkillTest _ (isAbilitySource attrs 3 -> True) -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 4 -> do
      others <-
        select $ InvestigatorAt (locationWithInvestigator iid) <> NotInvestigator (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others \other -> takeControlOfAsset other attrs.id
      pure a
    _ -> CharlieFoxtail <$> liftRunMessage msg attrs
