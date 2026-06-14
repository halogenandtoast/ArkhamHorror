module Arkham.Treachery.Cards.WingsOfTerror (wingsOfTerror) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WingsOfTerror = WingsOfTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingsOfTerror :: TreacheryCard WingsOfTerror
wingsOfTerror = treachery WingsOfTerror Cards.wingsOfTerror

instance HasAbilities WingsOfTerror where
  getAbilities (WingsOfTerror a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ SkillTestResult #after You #any #failure]

instance RunMessage WingsOfTerror where
  runMessage msg t@(WingsOfTerror attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid attrs Deck.EncounterDeck $ basic #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      case mcard of
        Nothing -> pure ()
        Just c -> do
          -- create an enemy that has been discarded, have it attack you, then
          -- remove it. This technically means we have an enemy at no location.
          focusCard c do
            chooseOneM iid $ scenarioI18n do
              labeled' "wingsOfTerror.drawEnemy" $ drawCard iid c
              labeled' "wingsOfTerror.attack" do
                addToEncounterDiscard (only c)
                push $ EnemyAttackFromDiscard iid (toSource attrs) (EncounterCard c)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WingsOfTerror <$> liftRunMessage msg attrs
