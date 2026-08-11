module Arkham.Homebrew.DarkMatter.Locations.InfirmaryInTheShadowOfEarth (
  infirmaryInTheShadowOfEarth,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest (withSkillTestTarget)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype InfirmaryInTheShadowOfEarth = InfirmaryInTheShadowOfEarth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmaryInTheShadowOfEarth :: LocationCard InfirmaryInTheShadowOfEarth
infirmaryInTheShadowOfEarth =
  location InfirmaryInTheShadowOfEarth Cards.infirmaryInTheShadowOfEarth 4 (PerPlayer 1)

{- | "[action] Choose an [[Ally]] asset at this location: Test [intellect] (2). If
you succeed, discard a copy of Contamination attached to them."
-}
instance HasAbilities InfirmaryInTheShadowOfEarth where
  getAbilities (InfirmaryInTheShadowOfEarth a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (Here <> exists (#ally <> AssetAt (be a))) actionAbility

instance RunMessage InfirmaryInTheShadowOfEarth where
  runMessage msg l@(InfirmaryInTheShadowOfEarth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      allies <- select $ #ally <> AssetAt (be attrs)
      chooseOrRunOneM iid $ targets allies \aid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) aid #intellect (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTestTarget \case
        AssetTarget aid -> do
          contaminations <-
            select $ treacheryIs Treacheries.contamination <> TreacheryIsAttachedTo (AssetTarget aid)
          for_ (take 1 contaminations) $ toDiscardBy iid (attrs.ability 1)
        _ -> pure ()
      pure l
    _ -> InfirmaryInTheShadowOfEarth <$> liftRunMessage msg attrs
