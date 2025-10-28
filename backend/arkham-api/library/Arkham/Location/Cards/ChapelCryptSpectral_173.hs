module Arkham.Location.Cards.ChapelCryptSpectral_173 (chapelCryptSpectral_173) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheWagesOfSin.Helpers

newtype ChapelCryptSpectral_173 = ChapelCryptSpectral_173 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCryptSpectral_173 :: LocationCard ChapelCryptSpectral_173
chapelCryptSpectral_173 = location ChapelCryptSpectral_173 Cards.chapelCryptSpectral_173 6 (Static 0)

instance HasModifiersFor ChapelCryptSpectral_173 where
  getModifiersFor (ChapelCryptSpectral_173 a) = maybeModifySelf a do
    liftGuardM $ selectNone $ enemyAt a <> ReadyEnemy
    pure [ShroudModifier (-3)]

instance HasAbilities ChapelCryptSpectral_173 where
  getAbilities (ChapelCryptSpectral_173 a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "chapelCryptSpectral_173.haunted" a 1

instance RunMessage ChapelCryptSpectral_173 where
  runMessage msg l@(ChapelCryptSpectral_173 attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.chapelCryptSpectral_173
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mTopCard <- fieldMap InvestigatorDeck (listToMaybe . take 1 . unDeck) iid
      for_ mTopCard \topCard -> do
        let reanimatedDead = PlayerCard $ topCard {pcCardCode = "xreanimated"}
        push $ RemovePlayerCardFromGame False (PlayerCard topCard)
        createEnemy reanimatedDead iid
      pure l
    _ -> ChapelCryptSpectral_173 <$> liftRunMessage msg attrs
