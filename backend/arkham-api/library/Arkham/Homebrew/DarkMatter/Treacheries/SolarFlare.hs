module Arkham.Homebrew.DarkMatter.Treacheries.SolarFlare (solarFlare) where

import Arkham.Helpers.Message.Discard (chooseAndDiscardCard)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SolarFlare = SolarFlare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

solarFlare :: TreacheryCard SolarFlare
solarFlare = treachery SolarFlare Cards.solarFlare

instance RunMessage SolarFlare where
  runMessage msg t@(SolarFlare attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      replicateM_ n do
        hasCards <- fieldP InvestigatorHand (not . null) iid
        chooseOneM iid $ withI18n do
          when hasCards
            $ unscoped
            $ countVar 1
            $ labeled' "discardCardsFromHand"
            $ push
            $ toMessage
            $ chooseAndDiscardCard iid attrs
          chooseTakeHorrorAndDamage iid attrs 1 1
      pure t
    _ -> SolarFlare <$> liftRunMessage msg attrs
