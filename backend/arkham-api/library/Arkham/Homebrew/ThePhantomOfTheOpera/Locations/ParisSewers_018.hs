module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.ParisSewers_018 (parisSewers_018) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Direction
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ParisSewers_018 = ParisSewers_018 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parisSewers_018 :: LocationCard ParisSewers_018
parisSewers_018 =
  locationWith ParisSewers_018 Cards.parisSewers_018 1 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities ParisSewers_018 where
  getAbilities (ParisSewers_018 a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "parisSewers_018.toll"
          $ restricted a 1 Here
          $ forced
          $ TurnBegins #when You
      , scenarioI18n
          $ withI18nTooltip "parisSewers_018.glimpse"
          $ restricted a 2 (Here <> exists (ConcealedCardAt (be a)))
          $ freeReaction
          $ TurnBegins #after You
      ]

instance RunMessage ParisSewers_018 where
  runMessage msg l@(ParisSewers_018 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        countVar 1 $ labeled "loseActions" $ loseActions iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseRevealConcealedAt iid (attrs.ability 2) (be attrs)
      pure l
    _ -> ParisSewers_018 <$> liftRunMessage msg attrs
