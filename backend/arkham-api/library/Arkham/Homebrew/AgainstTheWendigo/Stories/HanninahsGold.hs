module Arkham.Homebrew.AgainstTheWendigo.Stories.HanninahsGold (hanninahsGold) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (scenarioI18n)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype HanninahsGold = HanninahsGold StoryAttrs
  deriving anyclass (IsStory)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Choose if you want to listen to the prospector and attempt to leave the
mountains (Choice 1), or if you are going to see the gold vein (Choice 2).
Attach Hanninah's Gold to Mad Prospector. While Hanninah's Gold is in play, Mad
Prospector gains the abilities that match your choice."

Both choices read the same way -- the investigators are stuck at the Mad
Prospector until someone passes a test -- and differ only in the skill tested
and in what failing costs, so the choice is remembered in the story's meta and
both abilities hang off this card.
-}
hanninahsGold :: StoryCard HanninahsGold
hanninahsGold = persistStory $ story HanninahsGold Cards.hanninahsGold

listenedToTheProspector :: StoryAttrs -> Bool
listenedToTheProspector a = toResultDefault False a.meta

instance HasModifiersFor HanninahsGold where
  -- "The investigators cannot leave Mad Prospector."
  getModifiersFor (HanninahsGold a) =
    modifySelect a (InvestigatorAt $ locationIs Locations.madProspector) [CannotMove]

instance HasAbilities HanninahsGold where
  getAbilities (HanninahsGold a) =
    [restricted a 1 (exists $ You <> InvestigatorAt (locationIs Locations.madProspector)) actionAbility]

instance RunMessage HanninahsGold where
  runMessage msg s@(HanninahsGold attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.madProspector) \lid ->
        push $ StoryMessage $ PlaceStory (toCard attrs) (AttachedToLocation lid)
      chooseOneM iid $ scenarioI18n $ scope "hanninahsGold" do
        labeled "listenToTheProspector"
          $ push
          $ HandleTargetChoice iid (toSource attrs) (toTarget attrs)
        labeled "seeTheGoldVein" nothing
      pure s
    -- Only choice 1 announces itself; the meta starts out False for choice 2.
    HandleTargetChoice _ (isSource attrs -> True) (isTarget attrs -> True) ->
      pure $ HanninahsGold $ setMeta True attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let sType = if listenedToTheProspector attrs then #intellect else #combat
      beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)
      pure s
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      if listenedToTheProspector attrs
        then record YouSavedTheGoldProspector
        else record YouHaveFoundHanninahsGold
      removeStory attrs
      pure s
    -- "For each point you fail by, take 1 horror" (choice 1) or "1 damage" (choice 2).
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n -> do
      if listenedToTheProspector attrs
        then assignHorror iid (attrs.ability 1) n
        else assignDamage iid (attrs.ability 1) n
      pure s
    _ -> HanninahsGold <$> liftRunMessage msg attrs
