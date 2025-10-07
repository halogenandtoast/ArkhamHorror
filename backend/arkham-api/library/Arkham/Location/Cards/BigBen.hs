module Arkham.Location.Cards.BigBen (bigBen) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest.Target
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BigBen = BigBen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bigBen :: LocationCard BigBen
bigBen = symbolLabel $ location BigBen Cards.bigBen 4 (PerPlayer 1)

instance HasAbilities BigBen where
  getAbilities (BigBen a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ skillTestAbility
      $ restricted a 1 (Here <> exists (orConnected_ (location_ $ be a) <> LocationWithConcealedCard))
      $ FastAbility Free

instance RunMessage BigBen where
  runMessage msg l@(BigBen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select (orConnected_ (LocationWithId attrs.id) <> LocationWithConcealedCard)
      sid <- getRandom
      chooseTargetM iid locations \lid -> do
        concealed <- map toId <$> getConcealedAt lid
        chooseOrRunOneM iid do
          targets concealed \card -> beginSkillTest sid iid (attrs.ability 1) card #agility (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenJustM getSkillTestTarget \case
        ConcealedCardTarget card -> revealConcealed iid (attrs.ability 1) card
        _ -> pure ()
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid attrs 1
      pure l
    _ -> BigBen <$> liftRunMessage msg attrs
