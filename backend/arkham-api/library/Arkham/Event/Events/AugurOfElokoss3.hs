module Arkham.Event.Events.AugurOfElokoss3 (augurOfElokoss3) where

import Arkham.ChaosToken.Types
import Arkham.Criteria
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message (setOptionCriteria)
import Arkham.Modifier
import Arkham.Trait (Trait (Hex, Terror))

newtype AugurOfElokoss3 = AugurOfElokoss3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augurOfElokoss3 :: EventCard AugurOfElokoss3
augurOfElokoss3 = event AugurOfElokoss3 Cards.augurOfElokoss3

instance RunMessage AugurOfElokoss3 where
  runMessage msg e@(AugurOfElokoss3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      addSkillValue sid attrs iid #willpower
      investigate sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTest \sid -> priority $ skillTestModifier sid attrs iid $ DiscoveredClues 1
      faces <- map (.face) <$> getSkillTestRevealedChaosTokens
      when (any isSymbolChaosToken faces) do
        additionalSkillTestOptionEdit
          ( setOptionCriteria
              $ exists
              $ TreacheryInThreatAreaOf (affectsOthersKnown iid Anyone)
              <> hasAnyTrait [Terror, Hex]
          )
          "Augur of Elokoss"
          (do_ msg)
      pure e
    Do (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      treacheries <-
        select $ TreacheryInThreatAreaOf (affectsOthersKnown iid Anyone) <> hasAnyTrait [Terror, Hex]
      chooseOrRunOneM iid do
        targets treacheries $ toDiscardBy iid attrs
        withI18n skip_
      pure e
    _ -> AugurOfElokoss3 <$> liftRunMessage msg attrs
