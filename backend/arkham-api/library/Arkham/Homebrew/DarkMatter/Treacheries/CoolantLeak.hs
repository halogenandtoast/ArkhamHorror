module Arkham.Homebrew.DarkMatter.Treacheries.CoolantLeak (coolantLeak) where

import Arkham.Helpers.Message.Discard (chooseAndDiscardCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype CoolantLeak = CoolantLeak TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coolantLeak :: TreacheryCard CoolantLeak
coolantLeak = treachery CoolantLeak Cards.coolantLeak

{- | "Revelation - Test [agility] (4). For each point you fail by, either choose
and discard a card from your hand, or take 1 damage. If you control the EVA
Suit story asset, you automatically succeed."
-}
instance RunMessage CoolantLeak where
  runMessage msg t@(CoolantLeak attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEvaSuit <- selectAny $ assetIs Assets.evaSuit <> assetControlledBy iid
      if hasEvaSuit
        then pure t
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (Fixed 4)
          pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      replicateM_ n do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "discardCardsFromHand" $ push $ toMessage $ chooseAndDiscardCard iid attrs
          countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
      pure t
    _ -> CoolantLeak <$> liftRunMessage msg attrs
