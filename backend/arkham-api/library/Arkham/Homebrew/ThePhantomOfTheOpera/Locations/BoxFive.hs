module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.BoxFive (boxFive) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BoxFive = BoxFive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxFive :: LocationCard BoxFive
boxFive = symbolLabel $ location BoxFive Cards.boxFive 4 (PerPlayer 1)

instance HasAbilities BoxFive where
  getAbilities (BoxFive a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "boxFive.vantage"
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists ConcealedCardAny)
      $ freeReaction
      $ DiscoveringLastClue #when Anyone (be a)

instance RunMessage BoxFive where
  runMessage msg l@(BoxFive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      concealed <- select ConcealedCardAny
      chooseNM iid (min n (length concealed))
        $ targets concealed (revealConcealed iid (attrs.ability 1) . (.id))
      pure l
    _ -> BoxFive <$> liftRunMessage msg attrs
