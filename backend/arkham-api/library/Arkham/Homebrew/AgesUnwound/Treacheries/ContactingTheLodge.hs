module Arkham.Homebrew.AgesUnwound.Treacheries.ContactingTheLodge (contactingTheLodge) where

import Arkham.Ability
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n, getStandardActions)
import Arkham.Modifier
import Arkham.Treachery.Import.Lifted

newtype ContactingTheLodge = ContactingTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contactingTheLodge :: TreacheryCard ContactingTheLodge
contactingTheLodge = treachery ContactingTheLodge Cards.contactingTheLodge

{- | "[action]: Test [agility] (9). You may spend any number of additional
actions when you perform this action. You get +2 [agility] for this skill test
for each action being spent (including this ability's [action] cost). If you
succeed, flip this card over and resolve its text."

Attached to Arkham, Massachusetts; the ability carries no location wording of its
own, but /Exposition/ in the same set spells out "Investigators at any location
may trigger this ability" for the one ability that is meant to reach past its
host, so the unqualified ones read as on-location. 'OnSameLocation', as Locked
Door.
-}
instance HasAbilities ContactingTheLodge where
  getAbilities (ContactingTheLodge a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage ContactingTheLodge where
  runMessage msg t@(ContactingTheLodge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- The ability's own action is already paid by here, so what is left is what
      can still be added. Only /standard/ actions can be spent on an arbitrary
      ability, which is exactly what 'getStandardActions' counts and
      'loseStandardActions' takes. -}
      spendable <- getStandardActions iid
      if spendable > 0
        then campaignI18n $ chooseAmount iid "contactingTheLodge.extraActions" "$actions" 0 spendable attrs
        else beginTheTest iid attrs 0
      pure t
    ResolveAmounts iid (getChoiceAmount "$actions" -> n) (isTarget attrs -> True) -> do
      when (n > 0) $ loseStandardActions iid (attrs.ability 1) n
      beginTheTest iid attrs n
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      readStory iid attrs Stories.favorsForFavors
      pure t
    _ -> ContactingTheLodge <$> liftRunMessage msg attrs

-- | +2 [agility] per action spent, this ability's own action included.
beginTheTest :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> Int -> m ()
beginTheTest iid attrs n = do
  sid <- getRandom
  skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility (2 * (n + 1)))
  beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 9)
