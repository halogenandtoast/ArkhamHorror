module Arkham.Homebrew.DarkMatter.Treacheries.SolarFlare (solarFlare) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype SolarFlare = SolarFlare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

solarFlare :: TreacheryCard SolarFlare
solarFlare = treachery SolarFlare Cards.solarFlare

{- | "Revelation - Test [agility] (4). For each point you fail by, you must either
choose and discard a card from your hand, or take 1 damage and 1 horror."

Each point is its own step so that the hand is re-checked between choices.
-}
instance RunMessage SolarFlare where
  runMessage msg t@(SolarFlare attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasCards <- fieldMap InvestigatorHand notNull iid
      if hasCards
        then chooseOneM iid $ withI18n do
          countVar 1 $ labeled "discardCardsFromHand" $ chooseAndDiscardCard iid attrs
          chooseTakeHorrorAndDamage iid attrs 1 1
        else assignDamageAndHorror iid attrs 1 1
      doStep (n - 1) msg'
      pure t
    _ -> SolarFlare <$> liftRunMessage msg attrs
