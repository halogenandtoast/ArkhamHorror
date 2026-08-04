module Arkham.Story.Cards.RuthlessCharge (ruthlessCharge) where

import Arkham.Draw.Types (CardDraw (..), CardDrawRules (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RuthlessCharge = RuthlessCharge StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthlessCharge :: StoryCard RuthlessCharge
ruthlessCharge = story RuthlessCharge Cards.ruthlessCharge

instance RunMessage RuthlessCharge where
  runMessage msg s@(RuthlessCharge attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      rage <- getCthulhuRage
      getCthulhuLocation >>= traverse_ \lid -> do
        -- "If Cthulhu (Hoary Wings) is in play: Each investigator at Cthulhu's
        -- location discards one card from hand at random."
        whenM (selectAny $ cthulhuFacet Enemies.cthulhuHoaryWings) do
          selectEach (investigatorAt lid) (`randomDiscard` attrs)

        {- "If Cthulhu (Fierce Visage) is in play: Cthulhu (Fierce Visage) attacks
        the investigator with the highest [willpower] at Cthulhu's location. If no
        attack was made, the investigator with the highest [willpower] draws the top
        card of the encounter deck. That card's effects cannot be canceled and it
        loses surge."

        The facet is in play by the branch's own condition, so the only way no
        attack is made is that nobody is standing with Cthulhu — hence the fallback
        keys off an empty set of targets, and then reaches for the highest
        [willpower] investigator anywhere. -}
        whenM (selectAny $ cthulhuFacet Enemies.cthulhuFierceVisage) do
          targets <- select $ InvestigatorWithHighestSkill #willpower (investigatorAt lid)
          attacked <- or <$> traverse (cthulhuFacetAttacks attrs Enemies.cthulhuFierceVisage) targets
          unless attacked do
            selectEach (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator) \iid ->
              drawEncounterCardEdit iid attrs \d ->
                d
                  { cardDrawRules =
                      singleton
                        $ WithDrawnCardModifiers (toSource attrs) [EffectsCannotBeCanceled, NoSurge]
                  }

        {- "If Cthulhu (Wicked Claw) is in play: The investigator with the highest
        [combat] at Cthulhu's location must test [combat] (X), where X is Cthulhu's
        Rage. If they fail, Cthulhu (Wicked Claw) attacks them." -}
        whenM (selectAny $ cthulhuFacet Enemies.cthulhuWickedClaw) do
          selectEach (InvestigatorWithHighestSkill #combat (investigatorAt lid)) \iid -> do
            sid <- getRandom
            onFailedByEffect sid AnyValue attrs iid $ void $ cthulhuFacetAttacks attrs Enemies.cthulhuWickedClaw iid
            beginSkillTest sid iid attrs iid #combat (Fixed rage)
      pure s
    _ -> RuthlessCharge <$> liftRunMessage msg attrs
