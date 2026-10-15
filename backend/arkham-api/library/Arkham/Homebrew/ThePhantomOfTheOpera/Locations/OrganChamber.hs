module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.OrganChamber (organChamber) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers (afterExposed, placeConcealedCard)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, modifySelfWhen)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype OrganChamber = OrganChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

organChamber :: LocationCard OrganChamber
organChamber = symbolLabel $ location OrganChamber Cards.organChamber 5 (PerPlayer 1)

instance HasModifiersFor OrganChamber where
  getModifiersFor (OrganChamber a) = do
    whenUnrevealed a $ modifySelf a [Blocked]
    whenRevealed a do
      required <- perPlayer 1
      n <- selectCount (ConcealedCardAt (be a))
      modifySelfWhen a (n >= required) [ShroudModifier (negate (n `div` required))]

instance HasAbilities OrganChamber where
  getAbilities (OrganChamber a) =
    if a.revealed
      then
        extendRevealed1 a
          $ scenarioI18n
          $ withI18nTooltip "organChamber.draw"
          $ restricted a 1 (Here <> exists (ConcealedCardAt (connectedFrom (be a)))) actionAbility
      else
        extendUnrevealed1 a
          $ scenarioI18n
          $ withI18nTooltip "organChamber.unmasked"
          $ mkAbility a 2
          $ forced
          $ afterExposed Enemies.thePhantom

instance RunMessage OrganChamber where
  runMessage msg l@(OrganChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- getConcealedChoicesAt NotForExpose (connectedFrom (be attrs))
      chooseTargetM iid concealed \c -> do
        placeConcealedCard iid c.id (AtLocation attrs.id)
        doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      required <- perPlayer 2
      n <- selectCount (ConcealedCardAt (be attrs))
      when (n >= required && attrs.token #horror == 0)
        $ placeTokens (attrs.ability 1) attrs #horror 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      reveal attrs
      pure l
    _ -> OrganChamber <$> liftRunMessage msg attrs
