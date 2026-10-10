module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.BlueChamber (blueChamber) where

import Arkham.Card
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Helpers.SkillTest (getSkillTestMatchingSkillIcons, isSkillTestAt, withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Investigator.Types (Field (InvestigatorCommittedCards))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype BlueChamber = BlueChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blueChamber :: LocationCard BlueChamber
blueChamber =
  locationWith BlueChamber Cards.blueChamber 3 (PerPlayer 2) $ costToEnterUnrevealedL .~ chamberToll

{- | "During each skill test at Blue Chamber, treat the first printed icon on each
non-weakness player card as a matching icon."

The engine has no per-card matching-icon set -- @skillTestIconValues@ is one map
for the whole test -- so the rule is expressed the way the engine already says
"this card gains a matching icon": one @[wild]@ icon on the card, granted only
when the card's first printed icon is not matching already. A card whose first
printed icon does match needs nothing, and a card with no printed icons has no
first icon to reinterpret.

Cards in hand are covered as well as committed ones, because
'Arkham.Helpers.SkillTest.getIsCommittable' reads the same icons: the rule's
real bite is letting you commit a card you otherwise could not.
-}
instance HasModifiersFor BlueChamber where
  getModifiersFor (BlueChamber a) = when a.revealed do
    whenM (isSkillTestAt a.id) do
      matching <- getSkillTestMatchingSkillIcons
      inHand <- select $ basic NonWeakness <> InHandOf NotForPlay Anyone
      committed <- filterCards NonWeakness <$> selectAll InvestigatorCommittedCards Anyone
      let
        reinterpreted c = case listToMaybe (cdSkills $ toCardDef c) of
          Just icon -> icon `notMember` matching
          Nothing -> False
      modifyEach a (filter reinterpreted (inHand <> committed)) [AddSkillIcons [#wild]]

instance HasAbilities BlueChamber where
  -- "[skull]: Each card committed to this test loses 1 matching icon."
  getAbilities (BlueChamber a) =
    extendRevealed1 a
      $ describedSkullEffect 0 "Each card committed to this test loses 1 matching icon." a 1

instance RunMessage BlueChamber where
  runMessage msg l@(BlueChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        {- The loss is counted per card inside 'skillIconCount', but it is read off
        the investigator who committed, so every seat needs it -- a card may still
        be committed after the token resolves. -}
        withSkillTest \sid -> eachInvestigator \iid ->
          skillTestModifier sid (attrs.ability 1) iid (FewerMatchingIconsPerCard 1)
        pure l
      _ -> BlueChamber <$> liftRunMessage msg attrs
