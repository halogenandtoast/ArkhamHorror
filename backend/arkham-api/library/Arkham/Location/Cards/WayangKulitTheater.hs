module Arkham.Location.Cards.WayangKulitTheater (wayangKulitTheater) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Token (countTokens)

newtype WayangKulitTheater = WayangKulitTheater LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wayangKulitTheater :: LocationCard WayangKulitTheater
wayangKulitTheater = symbolLabel $ location WayangKulitTheater Cards.wayangKulitTheater 0 (PerPlayer 1)

instance HasModifiersFor WayangKulitTheater where
  getModifiersFor (WayangKulitTheater a) = do
    mTheShadeReaper <- selectOne $ scarletKeyIs Keys.theShadeReaper
    for_ mTheShadeReaper \theShadeReaper -> do
      x <- min 8 <$> fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper
      modifySelf a [ShroudModifier x]

instance HasAbilities WayangKulitTheater where
  getAbilities (WayangKulitTheater a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> oneOf [youExist InvestigatorWithAnyDamage, not_ $ Remembered SharedADeepPain])
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage WayangKulitTheater where
  runMessage msg l@(WayangKulitTheater attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 2
      remember SharedADeepPain
      pure l
    _ -> WayangKulitTheater <$> liftRunMessage msg attrs
